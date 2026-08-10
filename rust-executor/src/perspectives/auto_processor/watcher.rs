//! P-B2 executor watcher — pure-logic debounce + batch cap and a one-pass
//! runner that ties [`AutoProcessorConfig`] + the P-A claim + the
//! interpretation engine together.
//!
//! Split into two layers so each is trivially testable in isolation:
//! * [`WatcherState`] — pure observation state: record item arrivals, drain a
//!   ready batch when the debounce window has elapsed. No I/O; unit-tested
//!   deterministically.
//! * [`run_one_pass`] — the async pass itself: `try_claim`, load shapes,
//!   gather the SPARQL transcript, run `run_interpretation_with_strategy`
//!   with the deserialized dedup strategy. Standalone and re-triggerable so
//!   it can be driven from a test, a REPL, or the future event-driven
//!   watcher loop (P-B2b) without changing its guarantees.
//!
//! The eventual watcher loop (P-B2b) wires this to real perspective link
//! events + telepresence presence — it will still delegate the actual pass
//! to [`run_one_pass`], so the coordination contract stays in one place.

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::interpretation::{
    gather_transcript_sparql, run_interpretation_with_strategy_and_model, DedupStrategy,
};
use crate::perspectives::model_query::load_shape_from_store;
use crate::perspectives::perspective_instance::PerspectiveInstance;

use super::claim::{try_claim, ClaimOutcome};
use super::config::AutoProcessorConfig;

use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

/// Stable id for a `(speaker, text)` transcript turn — the atom the polling
/// watch loop feeds to [`WatcherState::record_item`] and (indirectly) to
/// [`crate::perspectives::auto_processor::claim::batch_key`]. SHA-256 over
/// `speaker || \0 || text` keeps it injective across the field boundary; a
/// hex prefix keeps claim-link URIs short.
///
/// The value is only meaningful within a single processor's pending window;
/// consumers should not persist it or compare across processors.
pub fn turn_id(speaker: &str, text: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(speaker.as_bytes());
    hasher.update([0u8]);
    hasher.update(text.as_bytes());
    let digest = hasher.finalize();
    format!("{:x}", digest)[..16].to_string()
}

/// Per-processor observation state. Kept in a `BTreeMap` inside
/// [`WatcherState`] so iteration is deterministic (mattering for tests, not
/// correctness).
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ProcessorPending {
    /// Source item ids seen since the last drained batch, insertion order,
    /// deduplicated.
    pub items: Vec<String>,
    /// Wall-clock (unix millis) of the most recent [`WatcherState::record_item`]
    /// for this processor. Used to compare against `debounce_ms`.
    pub last_touched_ms: i64,
    /// Wall-clock (unix millis) of the *oldest* still-pending item — i.e. the
    /// first `record_item` after the queue was last emptied. Used to compare
    /// against `max_wait_ms` so a sub-`batch_min` batch is not orphaned.
    pub first_touched_ms: i64,
}

/// Container that observes source item arrivals across every declared
/// processor and hands back a ready batch when the debounce window elapses.
/// Pure state — no I/O, no clock — so the debounce + cap rules are testable
/// with hand-picked `now_ms` values.
#[derive(Debug, Default, Clone)]
pub struct WatcherState {
    per_processor: BTreeMap<String, ProcessorPending>,
}

impl WatcherState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Note that `item_id` was just observed under `processor_id`'s scope at
    /// `now_ms`. Duplicate ids inside the same pending window are ignored;
    /// this keeps the pending queue small when the source graph emits several
    /// `link-added` events for the same node (e.g. type + predicate + data
    /// links landing back-to-back).
    pub fn record_item(&mut self, processor_id: &str, item_id: String, now_ms: i64) {
        let entry = self
            .per_processor
            .entry(processor_id.to_string())
            .or_default();
        if entry.items.is_empty() {
            // First item of a fresh window — start the max-wait clock.
            entry.first_touched_ms = now_ms;
        }
        if !entry.items.contains(&item_id) {
            entry.items.push(item_id);
        }
        entry.last_touched_ms = now_ms;
    }

    /// Read-only view of a processor's pending state (tests + observability).
    pub fn pending_for(&self, processor_id: &str) -> Option<&ProcessorPending> {
        self.per_processor.get(processor_id)
    }

    /// Drain a ready batch, or `None` if the processor should keep waiting.
    ///
    /// A batch is ready when **all** of:
    /// 1. pending is non-empty;
    /// 2. the debounce has settled (`now - last_touched >= debounce_ms`) — no
    ///    fresh arrival in the quiet window;
    /// 3. the size threshold is met — either `items.len() >= batch_min` (the
    ///    Flux "wait for N inputs" rule) **or** the oldest pending item has
    ///    waited past `max_wait_ms` (the safety valve, when configured).
    ///
    /// On drain it takes up to `batch_max` ids (FIFO); any overflow stays for
    /// the next window. Draining the queue empty resets the `max_wait_ms`
    /// clock (the next `record_item` re-arms `first_touched_ms`).
    pub fn drain_ready_batch(
        &mut self,
        cfg: &AutoProcessorConfig,
        now_ms: i64,
    ) -> Option<Vec<String>> {
        let entry = self.per_processor.get_mut(&cfg.processor_id)?;
        if entry.items.is_empty() {
            return None;
        }
        if now_ms.saturating_sub(entry.last_touched_ms) < cfg.debounce_ms {
            return None;
        }
        let threshold_met = entry.items.len() >= cfg.batch_min.max(1);
        let wait_expired = cfg
            .max_wait_ms
            .is_some_and(|w| now_ms.saturating_sub(entry.first_touched_ms) >= w);
        if !threshold_met && !wait_expired {
            return None;
        }
        let take = entry.items.len().min(cfg.batch_max);
        Some(entry.items.drain(..take).collect())
    }
}

/// Result of a single [`run_one_pass`] call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PassOutcome {
    /// We won the claim and ran interpretation. `bases` are the URIs of
    /// instances created / updated / re-linked (same shape
    /// `run_interpretation` returns).
    Won { bases: Vec<String> },
    /// Another peer holds an active, unexpired claim on this batch — we did
    /// not touch the LLM and did not write anything. The winner's result will
    /// reach us via link sync.
    BackedOff { holder: String },
    /// Authorship fast-path stood us down before writing a claim: an *online*
    /// author of the batch precedes us in message order, so they are the
    /// elected processor for this pass. No claim was written; no contention was
    /// added to the shared graph. `winner` is that author's DID. `try_claim`
    /// (P-A) is still the real correctness guard — this only reduces contention
    /// when telepresence data is available.
    NotCandidate { winner: String },
    /// Authorship policy stood us down with *no* winner: none of the batch's
    /// authors are currently online (all participants dropped, or the item
    /// synced in late). Per the "only participants process" rule we neither
    /// claim nor process — we wait for an author to come back online. No claim
    /// was written. This is a benign, expected steady-state on a quiet
    /// neighbourhood; the next pass re-evaluates presence.
    AwaitingAuthor,
    /// The claim was ours, but at least one of `cfg.interpretation_classes`
    /// did not resolve via `load_shape` (partial-sync SDNA / config typo).
    /// We logged, skipped the LLM call and let the claim TTL-expire so a
    /// later pass — once the shape lands — can re-take it.
    ShapesMissing { missing: Vec<String> },
    /// Won the claim but `source_scope_query` returned zero `(speaker, text)`
    /// rows. Nothing to interpret; no LLM round-trip.
    EmptyTranscript,
}

/// Outcome of authorship-ordered processor election (pure; see
/// [`elect_author`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthorElection {
    /// This agent is the elected processor — the first reachable author in
    /// message order is us. Proceed to `try_claim`.
    Me,
    /// Another online author precedes us in message order; stand down. They
    /// hold the local data and are present, so they should process.
    Other(String),
    /// No author of the batch is reachable (none online, self not among the
    /// authors). Under the "only participants process" policy nobody runs the
    /// pass this round; wait for an author to return.
    NoneOnline,
}

/// Pure authorship-ordered processor election.
///
/// Replaces the old lexical min-DID rule: the agent that runs the (expensive,
/// LLM) pass should be a *participant* — an author of the data in scope — not
/// merely whoever happens to have the smallest DID online. `authors` is the
/// batch's authors in **message order** (deduplicated, first-occurrence order
/// preserved by the caller). An author is *reachable* when it is either this
/// agent (`self_did`, always present since we are the one running) or a member
/// of `online_dids`.
///
/// The winner is the **first reachable author in message order** — mirroring
/// Flux, which walks from the first message's author onward until it finds a
/// present participant. Message order is a total order every peer computes
/// identically from the synced graph, so peers converge on the same winner
/// without a global sync clock; `try_claim` remains the guard for the residual
/// "who is online differs across peers" race.
///
/// Returns [`AuthorElection::NoneOnline`] when no author is reachable — the
/// strict "wait for a participant" branch (policy (b)).
pub fn elect_author(authors: &[String], online_dids: &[String], self_did: &str) -> AuthorElection {
    for author in authors {
        if author == self_did {
            return AuthorElection::Me;
        }
        if online_dids.iter().any(|d| d == author) {
            return AuthorElection::Other(author.clone());
        }
    }
    AuthorElection::NoneOnline
}

/// Resolve the author DID of each batch item, in `item_ids` order, for
/// authorship-ordered election ([`elect_author`]). Each item's author is the
/// lexicographically smallest author DID among its outgoing links — a message
/// node's links normally share a single author, so the `min` is just "the
/// author", and picking `min` keeps the result deterministic across peers when
/// they don't. Items with no links contribute no author. The returned list is
/// deduplicated with first-occurrence (message) order preserved, so the caller
/// walks it exactly as Flux walks messages: first author first.
async fn batch_authors(
    perspective: &PerspectiveInstance,
    item_ids: &[String],
) -> anyhow::Result<Vec<String>> {
    use crate::types::LinkQuery;
    let mut authors: Vec<String> = Vec::new();
    for item in item_ids {
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(item.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("batch_authors: get_links({item}) failed: {e:#}"))?;
        if let Some(author) = links.into_iter().map(|l| l.author).min() {
            if !authors.contains(&author) {
                authors.push(author);
            }
        }
    }
    Ok(authors)
}

/// Turn an [`AutoProcessorConfig::dedup_strategy_json`] blob into a live
/// [`DedupStrategy`]. Missing / unparseable / unknown-kind blobs fall back to
/// [`DedupStrategy::NormalizedString`] with a warn log, preserving the
/// runner-default behaviour of every existing caller.
///
/// Wire shape (matches what `write_processor` stores):
/// * `None` → default (`NormalizedString`).
/// * `{"kind":"normalized"}` → `NormalizedString`.
/// * `{"kind":"semantic","base_url":..., "model":..., "threshold":<f32>}`
///    → `Semantic { .. }`.
///
/// Hand-rolled rather than `serde` derive on the enum so this PR does not
/// add a serde dependency to the interpretation module — the dedup enum
/// itself keeps its current shape.
pub fn parse_dedup_strategy_json(blob: Option<&str>) -> DedupStrategy {
    let Some(json) = blob else {
        return DedupStrategy::default();
    };
    let value: serde_json::Value = match serde_json::from_str(json) {
        Ok(v) => v,
        Err(err) => {
            log::warn!(
                "parse_dedup_strategy_json: bad JSON `{json}`, falling back to default: {err}"
            );
            return DedupStrategy::default();
        }
    };
    match value.get("kind").and_then(|v| v.as_str()) {
        Some("normalized") => DedupStrategy::NormalizedString,
        Some("semantic") => {
            // Post-#883, semantic dedup embeds through AIService's own local
            // model — no external `base_url`. Only `model` (the registered
            // embedding-model name) + `threshold` are needed.
            let model = value
                .get("model")
                .and_then(|v| v.as_str())
                .map(str::to_string);
            let threshold = value
                .get("threshold")
                .and_then(|v| v.as_f64())
                .map(|f| f as f32);
            match (model, threshold) {
                (Some(model), Some(threshold)) => DedupStrategy::Semantic { model, threshold },
                _ => {
                    log::warn!(
                        "parse_dedup_strategy_json: semantic strategy missing required fields \
                         (model/threshold) in `{json}`; falling back to default"
                    );
                    DedupStrategy::default()
                }
            }
        }
        other => {
            log::warn!(
                "parse_dedup_strategy_json: unknown `kind` {:?} in `{json}`, falling back to \
                 default",
                other
            );
            DedupStrategy::default()
        }
    }
}

/// Run one processing pass for `cfg` against the id-set `item_ids`, following
/// the spec §4.3 coordination protocol. Standalone so callers can trigger it
/// from a test, a REPL, or the future event-driven watcher loop without
/// changing its guarantees.
///
/// Ordering:
/// 0. Authorship-ordered candidacy (fast-path, P-B2b3) — resolve the batch's
///    authors (in message order) and read
///    [`PerspectiveInstance::online_agents`], then [`elect_author`]: the first
///    *online* author in message order is the processor. If that is not us we
///    short-circuit with [`PassOutcome::NotCandidate`]; if no author is online
///    we short-circuit with [`PassOutcome::AwaitingAuthor`] and wait (the
///    "only participants process" rule). This never writes to the shared
///    graph, so it keeps contention (and claim links) off busy perspectives.
///    A missing/erroring telepresence adapter, or a batch with no resolvable
///    authors, is treated as "no candidacy signal" and falls through —
///    correctness still rests on step 1's claim.
/// 1. `try_claim` — reserve the batch in the shared graph. Loss = back off
///    silently; the winner's result reaches us via link sync.
/// 2. Resolve each `cfg.interpretation_classes` entry via [`load_shape`]. A
///    single miss returns [`PassOutcome::ShapesMissing`] and lets the claim
///    TTL-expire so a later pass — once the SDNA lands — can re-take it.
/// 3. Gather the transcript via [`gather_transcript_sparql`] using
///    `cfg.source_scope_query`. Zero rows = [`PassOutcome::EmptyTranscript`].
/// 4. [`run_interpretation_with_strategy`] with the deserialized dedup.
///
/// `cfg.llm_model` (if set) is honored: the pass routes through an AI-task
/// row bound to that model id. `cfg.llm_base_url` is NOT yet applied — it
/// implies a distinct model *provider* registration (different LLM channel),
/// which is a runtime concern handled at `AIService::spawn_model` time; a
/// dedicated dynamic-registration follow-up will wire it in. When it is
/// `Some` we emit a `warn` log so a misconfigured processor is visible in
/// the executor log instead of silently falling back to the default provider.
pub async fn run_one_pass(
    perspective: &mut PerspectiveInstance,
    cfg: &AutoProcessorConfig,
    item_ids: &[String],
    now_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<PassOutcome> {
    if cfg.llm_base_url.is_some() {
        log::warn!(
            "auto_processor `{}`: llm_base_url is set but not yet honored — dynamic model \
             provider registration is a follow-up. Falling back to the AIService default \
             provider for `{}`.",
            cfg.processor_id,
            cfg.llm_model.as_deref().unwrap_or("<default model>"),
        );
    }

    // 0. Authorship-ordered candidacy (fast-path, P-B2b3). Best-effort: a
    //    missing/erroring telepresence adapter or a batch with no resolvable
    //    authors falls through to `try_claim`, which is the real correctness
    //    guard. Otherwise it elects the first *online* author in message order
    //    and either proceeds (that is us), stands down for the winner, or waits
    //    when no author is online ("only participants process").
    let me = did_for_context(context)
        .map_err(|e| anyhow::anyhow!("run_one_pass: did_for_context: {e:#}"))?;
    match perspective.online_agents().await {
        Ok(agents) => {
            let online: Vec<String> = agents.into_iter().map(|a| a.did).collect();
            let authors = match batch_authors(perspective, item_ids).await {
                Ok(a) => a,
                Err(e) => {
                    log::debug!(
                        "auto_processor `{}`: batch_authors failed ({e:#}); proceeding to try_claim",
                        cfg.processor_id
                    );
                    Vec::new()
                }
            };
            if authors.is_empty() {
                // No authorship signal — don't stall the pass; let the claim
                // layer (min-DID) elect a processor instead.
                log::debug!(
                    "auto_processor `{}`: no resolvable batch authors; proceeding to try_claim",
                    cfg.processor_id
                );
            } else {
                match elect_author(&authors, &online, &me) {
                    AuthorElection::Me => { /* elected — fall through to claim */ }
                    AuthorElection::Other(winner) => {
                        log::info!(
                            "auto_processor `{}`: standing down — online author `{winner}` \
                             precedes us in message order",
                            cfg.processor_id
                        );
                        return Ok(PassOutcome::NotCandidate { winner });
                    }
                    AuthorElection::NoneOnline => {
                        log::info!(
                            "auto_processor `{}`: no batch author online — waiting for a \
                             participant to return before processing",
                            cfg.processor_id
                        );
                        return Ok(PassOutcome::AwaitingAuthor);
                    }
                }
            }
        }
        Err(e) => {
            // Expected on perspectives without a telepresence-capable
            // link-language. Correctness is unaffected — `try_claim` below is
            // the real guard.
            log::debug!(
                "auto_processor `{}`: online_agents unavailable ({e:#}); proceeding to try_claim",
                cfg.processor_id
            );
        }
    }

    // 1. Reserve the batch.
    let claim = try_claim(
        perspective,
        &cfg.processor_id,
        item_ids,
        cfg.claim_ttl_ms,
        now_ms,
        context,
    )
    .await?;
    if let ClaimOutcome::BackedOff { holder } = claim {
        log::info!(
            "auto_processor `{}`: backed off — holder `{holder}` has the claim",
            cfg.processor_id
        );
        return Ok(PassOutcome::BackedOff { holder });
    }

    // 2. Resolve shapes.
    let store = &*perspective.sparql_store;
    let mut shapes = Vec::with_capacity(cfg.interpretation_classes.len());
    let mut missing = Vec::new();
    for class in &cfg.interpretation_classes {
        match load_shape_from_store(store, class) {
            Ok(shape) => shapes.push(shape),
            Err(err) => {
                log::warn!(
                    "auto_processor `{}`: load_shape(`{class}`) failed: {err:#}",
                    cfg.processor_id
                );
                missing.push(class.clone());
            }
        }
    }
    if !missing.is_empty() {
        return Ok(PassOutcome::ShapesMissing { missing });
    }

    // 3. Gather transcript.
    let transcript = gather_transcript_sparql(perspective, &cfg.source_scope_query).await?;
    if transcript.is_empty() {
        log::info!(
            "auto_processor `{}`: source_scope_query returned 0 rows; nothing to interpret",
            cfg.processor_id
        );
        return Ok(PassOutcome::EmptyTranscript);
    }

    // 4. Interpret.
    let dedup = parse_dedup_strategy_json(cfg.dedup_strategy_json.as_deref());
    let base_prefix = format!("ad4m://autoprocessor/{}/instance/", cfg.processor_id);
    let bases = run_interpretation_with_strategy_and_model(
        perspective,
        &shapes,
        &transcript,
        &base_prefix,
        context,
        &dedup,
        cfg.llm_model.as_deref(),
        // Existing-instance scope: not yet wired from the processor config —
        // #883 added the plumbing (`existing_instance_context(scope)`), but the
        // per-channel scope belongs to a follow-up config field. `None` keeps
        // today's whole-perspective existing-set behaviour.
        None,
    )
    .await?;
    Ok(PassOutcome::Won { bases })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::auto_processor::claim::{batch_key, write_claim};
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

    fn cfg(id: &str, debounce_ms: i64, batch_max: usize) -> AutoProcessorConfig {
        AutoProcessorConfig {
            processor_id: id.into(),
            source_scope_query: format!(
                "SELECT ?speaker ?text WHERE {{ ?s <ns://{id}/turn> ?t . }}"
            ),
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms,
            batch_min: 1,
            batch_max,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            llm_base_url: None,
            llm_model: None,
            dedup_strategy_json: None,
        }
    }

    // ---- turn_id -----------------------------------------------------------

    /// `turn_id` is deterministic (same inputs → same output every call) and
    /// injective across the (speaker, text) field boundary — swapping bytes
    /// between the fields must yield a different id. Also short (16 hex
    /// chars) so a batch of turn ids doesn't blow up the claim-link URI.
    #[test]
    fn turn_id_is_deterministic_and_field_injective() {
        assert_eq!(turn_id("alice", "hi"), turn_id("alice", "hi"));
        assert_ne!(turn_id("alice", "hi"), turn_id("alic", "ehi"));
        assert_ne!(turn_id("alice", "hi"), turn_id("bob", "hi"));
        assert_eq!(turn_id("alice", "hi").len(), 16);
        // Field content is hex.
        assert!(turn_id("alice", "hi")
            .chars()
            .all(|c| c.is_ascii_hexdigit()));
    }

    // ---- elect_author ------------------------------------------------------

    /// The first author in message order is us and we are (definitionally)
    /// present → we are elected regardless of who else is online. This is the
    /// common single-participant / I-spoke-first case.
    #[test]
    fn elect_author_elects_self_when_first_author() {
        let authors = vec!["did:key:me".into(), "did:key:bob".into()];
        let online = vec!["did:key:bob".into()];
        assert_eq!(
            elect_author(&authors, &online, "did:key:me"),
            AuthorElection::Me
        );
    }

    /// The first author precedes us in message order AND is online → stand
    /// down for them; they hold the data and are present. Message order, not
    /// DID order, decides — `zzz-alice` wins over us even though her DID sorts
    /// last, because she authored the first message.
    #[test]
    fn elect_author_stands_down_for_earlier_online_author() {
        let authors = vec!["did:key:zzz-alice".into(), "did:key:me".into()];
        let online = vec!["did:key:zzz-alice".into()];
        assert_eq!(
            elect_author(&authors, &online, "did:key:me"),
            AuthorElection::Other("did:key:zzz-alice".into())
        );
    }

    /// The first author is offline; the second author is us → we skip past the
    /// absent participant and are elected. This is Nico's "everyone else
    /// offline, eventually I find my own message and process" case.
    #[test]
    fn elect_author_skips_offline_author_to_reach_self() {
        let authors = vec!["did:key:alice".into(), "did:key:me".into()];
        let online: Vec<String> = vec![]; // alice dropped
        assert_eq!(
            elect_author(&authors, &online, "did:key:me"),
            AuthorElection::Me
        );
    }

    /// No author of the batch is online and we are not among the authors (a
    /// non-participant watching a synced transcript) → nobody processes. The
    /// strict "only participants process" branch — policy (b).
    #[test]
    fn elect_author_waits_when_no_author_online() {
        let authors = vec!["did:key:alice".into(), "did:key:bob".into()];
        let online: Vec<String> = vec![];
        assert_eq!(
            elect_author(&authors, &online, "did:key:non-participant"),
            AuthorElection::NoneOnline
        );
    }

    /// Earliest-in-order online author wins even when a later author is also
    /// online — order is the tiebreak, so election is deterministic across
    /// peers without a global clock.
    #[test]
    fn elect_author_picks_earliest_in_order_among_multiple_online() {
        let authors = vec!["did:key:alice".into(), "did:key:bob".into()];
        let online = vec!["did:key:bob".into(), "did:key:alice".into()];
        assert_eq!(
            elect_author(&authors, &online, "did:key:me"),
            AuthorElection::Other("did:key:alice".into())
        );
    }

    // ---- batch_authors -----------------------------------------------------

    /// `batch_authors` reads each item's link author from the graph, in
    /// `item_ids` order, deduplicates repeated authors, and skips items with no
    /// links. Here both authored items share the local test agent, so the
    /// result is a single-entry list; the linkless id contributes nothing.
    #[tokio::test]
    async fn batch_authors_resolves_dedupes_and_skips_linkless() {
        use crate::types::{Link, LinkStatus};
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let me = did_for_context(&ctx).expect("did");
        for uri in ["msg://a", "msg://b"] {
            p.add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://body".into()),
                    target: "literal:string:hi".into(),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed body");
        }
        let authors = batch_authors(
            &p,
            &["msg://a".into(), "msg://b".into(), "msg://no-links".into()],
        )
        .await
        .expect("batch_authors");
        assert_eq!(
            authors,
            vec![me],
            "same author collapses to one entry; the linkless id is skipped"
        );
    }

    // ---- WatcherState -------------------------------------------------------

    /// Nothing recorded → `drain_ready_batch` returns None regardless of the
    /// elapsed time. The watcher-loop caller should just keep waiting.
    #[test]
    fn drain_returns_none_when_nothing_pending() {
        let mut w = WatcherState::new();
        let c = cfg("p", 1_000, 32);
        assert_eq!(w.drain_ready_batch(&c, 10_000), None);
    }

    /// Items recorded but the debounce window has not elapsed yet → None.
    /// (`now - last_touched = 500ms < 1_000ms`.)
    #[test]
    fn drain_returns_none_while_debounce_not_elapsed() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 10_000);
        w.record_item("p", "i2".into(), 10_400);
        let c = cfg("p", 1_000, 32);
        assert_eq!(w.drain_ready_batch(&c, 10_900), None);
    }

    /// Duplicate ids in the pending window are collapsed to one entry. Guards
    /// against noisy `link-added` streams (type + predicate + data links per
    /// node) fanning the batch out with the same id.
    #[test]
    fn record_dedupes_repeated_ids() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 100);
        w.record_item("p", "i1".into(), 200);
        w.record_item("p", "i2".into(), 300);
        w.record_item("p", "i1".into(), 400);
        let pending = w.pending_for("p").expect("pending exists");
        assert_eq!(pending.items, vec!["i1".to_string(), "i2".to_string()]);
        // last_touched follows the most recent record regardless of duplicate
        // status — otherwise a noisy re-emit resets the debounce clock.
        assert_eq!(pending.last_touched_ms, 400);
    }

    /// Debounce elapsed + pending non-empty → drain returns everything (below
    /// the cap) and empties the queue.
    #[test]
    fn drain_returns_all_pending_after_debounce() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 1_000);
        w.record_item("p", "i2".into(), 1_500);
        let c = cfg("p", 1_000, 32);
        let batch = w
            .drain_ready_batch(&c, 3_000)
            .expect("debounce elapsed → batch");
        assert_eq!(batch, vec!["i1".to_string(), "i2".to_string()]);
        assert!(
            w.pending_for("p").unwrap().items.is_empty(),
            "drain must empty the queue when everything fits"
        );
    }

    /// batch_max caps a single drain; the overflow stays for the next
    /// window in FIFO order.
    #[test]
    fn drain_caps_at_batch_max_leaving_overflow() {
        let mut w = WatcherState::new();
        for i in 0..5 {
            w.record_item("p", format!("i{i}"), 1_000 + i as i64);
        }
        let c = cfg("p", 100, 2);
        let batch = w.drain_ready_batch(&c, 5_000).expect("batch");
        assert_eq!(batch, vec!["i0".to_string(), "i1".to_string()]);
        let remaining = &w.pending_for("p").unwrap().items;
        assert_eq!(
            remaining,
            &vec!["i2".to_string(), "i3".to_string(), "i4".to_string()],
            "overflow must be preserved in insertion order for the next window"
        );
    }

    /// `batch_min` holds a below-threshold batch even after the debounce has
    /// settled — the Flux "wait for N inputs" rule. Two items pending, min is
    /// 3, no `max_wait_ms` → keep waiting (None) and leave the queue intact.
    #[test]
    fn drain_holds_below_batch_min() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 1_000);
        w.record_item("p", "i2".into(), 1_100);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 3;
        assert_eq!(
            w.drain_ready_batch(&c, 5_000),
            None,
            "below batch_min with no max_wait must keep waiting"
        );
        assert_eq!(
            w.pending_for("p").unwrap().items.len(),
            2,
            "held batch stays queued"
        );
    }

    /// Once `batch_min` items have accumulated (debounce settled), the batch
    /// drains normally.
    #[test]
    fn drain_fires_at_batch_min() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 1_000);
        w.record_item("p", "i2".into(), 1_100);
        w.record_item("p", "i3".into(), 1_200);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 3;
        assert_eq!(
            w.drain_ready_batch(&c, 5_000),
            Some(vec!["i1".into(), "i2".into(), "i3".into()])
        );
    }

    /// `max_wait_ms` is the safety valve: a sub-`batch_min` batch drains once
    /// the oldest item has waited long enough, so it is never orphaned. Item
    /// first seen at t=1_000, max_wait=2_000 → still held at t=2_500 relative
    /// to... no: elapsed since first_touched (1_000) at now=3_500 is 2_500 ≥
    /// 2_000 → drains despite being below min.
    #[test]
    fn drain_flushes_below_min_after_max_wait() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 1_000);
        w.record_item("p", "i2".into(), 1_100);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 5;
        c.max_wait_ms = Some(2_000);
        // Debounce settled (no arrival since 1_100) and 3_500-1_000=2_500 ≥
        // 2_000 max_wait → flush the partial batch.
        assert_eq!(
            w.drain_ready_batch(&c, 3_500),
            Some(vec!["i1".into(), "i2".into()]),
            "max_wait must flush a sub-min batch rather than orphan it"
        );
    }

    /// `max_wait_ms` does not fire early: before the oldest item ages past the
    /// window, a sub-min batch is still held.
    #[test]
    fn drain_respects_max_wait_before_expiry() {
        let mut w = WatcherState::new();
        w.record_item("p", "i1".into(), 1_000);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 5;
        c.max_wait_ms = Some(2_000);
        // 1_500-1_000 = 500 < 2_000 → still waiting (debounce already settled).
        assert_eq!(w.drain_ready_batch(&c, 1_500), None);
    }

    /// Per-processor state is isolated: draining one processor's queue does
    /// not touch another's, and their debounce windows tick independently.
    #[test]
    fn per_processor_state_is_isolated() {
        let mut w = WatcherState::new();
        w.record_item("a", "a-1".into(), 1_000);
        w.record_item("b", "b-1".into(), 2_000);
        let ca = cfg("a", 100, 32);
        let cb = cfg("b", 100, 32);
        assert_eq!(
            w.drain_ready_batch(&ca, 1_500),
            Some(vec!["a-1".to_string()])
        );
        assert!(
            w.pending_for("a").unwrap().items.is_empty(),
            "'a' drained, 'b' untouched"
        );
        assert_eq!(
            w.pending_for("b").unwrap().items,
            vec!["b-1".to_string()],
            "'b' state untouched by 'a' drain"
        );
        assert_eq!(
            w.drain_ready_batch(&cb, 2_500),
            Some(vec!["b-1".to_string()])
        );
    }

    // ---- parse_dedup_strategy_json -----------------------------------------

    #[test]
    fn parse_dedup_none_returns_default() {
        assert!(matches!(
            parse_dedup_strategy_json(None),
            DedupStrategy::NormalizedString
        ));
    }

    #[test]
    fn parse_dedup_normalized_kind() {
        assert!(matches!(
            parse_dedup_strategy_json(Some(r#"{"kind":"normalized"}"#)),
            DedupStrategy::NormalizedString
        ));
    }

    #[test]
    fn parse_dedup_semantic_full_shape() {
        let strat = parse_dedup_strategy_json(Some(
            r#"{"kind":"semantic","model":"nomic","threshold":0.8}"#,
        ));
        match strat {
            DedupStrategy::Semantic { model, threshold } => {
                assert_eq!(model, "nomic");
                assert!((threshold - 0.8).abs() < 1e-4);
            }
            other => panic!("expected Semantic, got {other:?}"),
        }
    }

    /// `semantic` missing a required field → default. Silent behaviour would
    /// hide a config bug; the runner instead falls back and warn-logs.
    #[test]
    fn parse_dedup_semantic_incomplete_falls_back() {
        assert!(matches!(
            parse_dedup_strategy_json(Some(r#"{"kind":"semantic","base_url":"x"}"#)),
            DedupStrategy::NormalizedString
        ));
    }

    /// Unparseable JSON and unknown `kind` both fall back to the default.
    #[test]
    fn parse_dedup_bad_json_or_unknown_kind_falls_back() {
        assert!(matches!(
            parse_dedup_strategy_json(Some("not json at all")),
            DedupStrategy::NormalizedString
        ));
        assert!(matches!(
            parse_dedup_strategy_json(Some(r#"{"kind":"martian-vector-dedup"}"#)),
            DedupStrategy::NormalizedString
        ));
    }

    // ---- run_one_pass -------------------------------------------------------

    /// If another peer already holds an unexpired claim on the same batch,
    /// `run_one_pass` short-circuits with `BackedOff` before touching the
    /// LLM or attempting shape loads. Verified with a smaller-DID seed
    /// claim (`aaa:incumbent` sorts before any `did:key:...`).
    #[tokio::test]
    async fn run_one_pass_backs_off_when_batch_already_claimed() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = cfg("proc", 100, 32);
        let items = vec!["i1".to_string(), "i2".to_string()];
        let key = batch_key(&items);
        write_claim(&mut p, "proc", &key, "aaa:incumbent", 60_000, &ctx)
            .await
            .expect("seed incumbent");

        let outcome = run_one_pass(&mut p, &cfg, &items, 1_000, &ctx)
            .await
            .expect("run_one_pass");
        assert!(
            matches!(&outcome, PassOutcome::BackedOff { holder } if holder == "aaa:incumbent"),
            "expected BackedOff{{holder: aaa:incumbent}}, got {outcome:?}"
        );
    }

    /// Won the claim but the configured class has no matching shape (none
    /// registered on the perspective) → the pass short-circuits with
    /// `ShapesMissing` before calling the interpretation engine. Proves
    /// the runner never invokes the LLM on a misconfigured pass.
    #[tokio::test]
    async fn run_one_pass_reports_missing_shapes_without_touching_llm() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = cfg("proc", 100, 32);
        cfg.interpretation_classes = vec!["ns://Unregistered".into()];
        let items = vec!["i1".to_string()];
        let outcome = run_one_pass(&mut p, &cfg, &items, 1_000, &ctx)
            .await
            .expect("run_one_pass");
        match outcome {
            PassOutcome::ShapesMissing { missing } => {
                assert_eq!(missing, vec!["ns://Unregistered".to_string()]);
            }
            other => panic!("expected ShapesMissing, got {other:?}"),
        }
    }

    /// `cfg.llm_model` must reach the interpretation engine — the pass has to
    /// register (or reuse) an AI-task row bound to that model id, not the
    /// shared default row. Verified end-to-end: seed a real shape + transcript,
    /// run the pass, then confirm `Ad4mDb` now holds a per-model task named
    /// `adam://interpretation?model=<id>` with `model_id = <id>`. The LLM
    /// invocation itself is expected to fail in this unit-test environment
    /// (no `AIService` provider registered for the fabricated model) — but the
    /// DB insert happens *before* the LLM step, so the failure does not gate
    /// this proof. This is the plumbing check the P-B2b watcher-loop relies on.
    #[tokio::test]
    async fn run_one_pass_uses_cfg_llm_model_for_interpretation_task() {
        use crate::db::Ad4mDb;
        use crate::perspectives::interpretation::interpretation_task_name_for_model;
        use crate::perspectives::interpretation_test_support::INTENTION_SDNA;
        use crate::types::{AITask, Link, LinkStatus};

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Intention", INTENTION_SDNA)]).await;

        // Fabricated model id: guaranteed no matching AIService LLM channel,
        // so the LLM call fails — but only after `ensure_interpretation_task`
        // has inserted the per-model row we're asserting on.
        let model = "wiring-probe-model-v1";
        let expected_name = interpretation_task_name_for_model(Some(model));

        // Pre-clean: this test asserts on a fresh insert. `ensure_db_init` is
        // shared across the single-threaded test run, so scrub the target name.
        let leftover: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == expected_name)
            .collect();
        for t in leftover {
            Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
        }

        // Two turns so `gather_transcript_sparql` returns non-empty and the
        // pass does not short-circuit on `EmptyTranscript`.
        for (uri, author, body) in [
            (
                "msg://a",
                "did:key:alice",
                "I'll ship the interpretation refactor.",
            ),
            (
                "msg://b",
                "did:key:bob",
                "Roger, I'll review this afternoon.",
            ),
        ] {
            p.add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://body".into()),
                    target: format!("literal:string:{body}"),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed body");
            p.add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://author".into()),
                    target: author.into(),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed author");
        }

        let mut c = cfg("wiring-probe-proc", 100, 32);
        c.interpretation_classes = vec!["ns://Intention".into()];
        c.source_scope_query = "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . \
                                ?m <ns://author> ?speaker . } ORDER BY ?m"
            .to_string();
        c.llm_model = Some(model.to_string());

        // The pass is *expected* to fail once it reaches the LLM step (no
        // provider registered for `wiring-probe-model-v1`) — we swallow the
        // Result. The plumbing proof lives in the DB check below.
        let _ = run_one_pass(
            &mut p,
            &c,
            &["msg://a".into(), "msg://b".into()],
            1_000,
            &ctx,
        )
        .await;

        let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == expected_name)
            .collect();
        assert_eq!(
            rows.len(),
            1,
            "expected exactly one per-model interpretation task row for `{model}`; \
             the watcher must plumb cfg.llm_model into ensure_interpretation_task_for_model"
        );
        assert_eq!(
            rows[0].model_id, model,
            "per-model task row must carry cfg.llm_model as its model_id"
        );
    }
}
