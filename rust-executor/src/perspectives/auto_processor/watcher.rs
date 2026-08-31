//! P-B2 executor watcher — pure-logic debounce + batch cap and a one-pass
//! runner that ties [`AutoProcessorConfig`] + the P-A claim + the
//! interpretation engine together.
//!
//! Split into two layers so each is trivially testable in isolation:
//! * [`WatcherState`] — pure observation state: record item arrivals, drain a
//!   ready batch when the debounce window has elapsed. No I/O; unit-tested
//!   deterministically.
//! * [`run_one_pass`] — the async pass itself: `try_claim`, load shapes,
//!   interpret the drained [`PendingTurn`] payload (no second SPARQL), run
//!   `run_interpretation_with_strategy` with the deserialized dedup strategy.
//!   Standalone and re-triggerable so it can be driven from a test, a REPL, or
//!   the future event-driven watcher loop without changing its guarantees.
//!
//! The eventual watcher loop (P-B2b) wires this to real perspective link
//! events + telepresence presence — it will still delegate the actual pass
//! to [`run_one_pass`], so the coordination contract stays in one place.

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::interpretation::{
    run_interpretation_with_harness_and_model, run_interpretation_with_strategy_and_model,
    DedupStrategy, InterpretationRunCursor, TranscriptTurn,
};
use crate::perspectives::model_query::load_shape_from_store;
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkStatus};

use super::claim::{batch_key, try_claim, ClaimOutcome};
use super::config::AutoProcessorConfig;
use super::events::{
    emit, emit_neighbourhood_state, AutoProcessorEvent, AutoProcessorNeighbourhoodState,
    AutoProcessorStep, NeighbourhoodPhase,
};

use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashSet};

/// Stable id for a `(speaker, text, timestamp)` transcript turn — the atom the
/// polling watch loop feeds to [`WatcherState::record_item`] and (indirectly)
/// to [`crate::perspectives::auto_processor::claim::batch_key`]. SHA-256 over
/// `speaker || \0 || text || \0 || timestamp` keeps it injective across field
/// boundaries; a hex prefix keeps claim-link URIs short.
///
/// Identical content at different times is a **new** turn. The same link
/// (same speaker, body, and link timestamp) collapses. The value is only
/// meaningful within a single processor's pending window; consumers should
/// not persist it or compare across processors (until the shared
/// `InterpretationRun.sources` cursor lands).
pub fn turn_id(speaker: &str, text: &str, timestamp: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(speaker.as_bytes());
    hasher.update([0u8]);
    hasher.update(text.as_bytes());
    hasher.update([0u8]);
    hasher.update(timestamp.as_bytes());
    let digest = hasher.finalize();
    format!("{:x}", digest)[..16].to_string()
}

/// One pending source turn: the content-hash id plus the payload
/// `run_one_pass` interprets. Kept together so a drained batch is exactly
/// what the LLM sees — no second SPARQL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PendingTurn {
    pub id: String,
    pub speaker: String,
    pub text: String,
    pub timestamp: String,
}

impl PendingTurn {
    pub fn from_transcript(turn: &TranscriptTurn) -> Self {
        Self {
            id: turn_id(&turn.speaker, &turn.text, &turn.timestamp),
            speaker: turn.speaker.clone(),
            text: turn.text.clone(),
            timestamp: turn.timestamp.clone(),
        }
    }

    pub fn as_transcript(&self) -> TranscriptTurn {
        TranscriptTurn {
            speaker: self.speaker.clone(),
            text: self.text.clone(),
            timestamp: self.timestamp.clone(),
        }
    }
}

/// Per-processor observation state. Kept in a `BTreeMap` inside
/// [`WatcherState`] so iteration is deterministic (mattering for tests, not
/// correctness).
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ProcessorPending {
    /// Source turns seen since the last drained batch, insertion order,
    /// deduplicated by [`PendingTurn::id`].
    pub items: Vec<PendingTurn>,
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
    /// Per-batch (keyed by [`crate::perspectives::auto_processor::claim::batch_key`])
    /// wall-clock (unix millis) of the first tick we stood down for an *online*
    /// elected author. Drives the stall-fallback: once a deferred batch has
    /// waited past `claim_ttl_ms` without being processed, we escalate straight
    /// to `try_claim`. This covers an elected author that is online but never
    /// actually claims — the claim TTL only recovers a *crashed claimant*, not
    /// an *inactive elected author*.
    standdown_since: BTreeMap<String, i64>,
    /// Per-processor turn id → wall-clock (unix millis) before which the id
    /// must not be re-enqueued, set when a pass backed off to a peer's claim.
    ///
    /// The durable cursor is the winner's `InterpretationRun.sources`, which
    /// only reaches us once their links sync. Without this, the losing peer
    /// re-gathers the same turns every debounce window and calls `try_claim`
    /// again — and `try_claim` *writes* its claim before reading the holders,
    /// so a slow-syncing neighbourhood would see steady claim-link churn. The
    /// deadline is the claim TTL: past it the winner's claim has expired
    /// anyway, so retrying is the intended crashed-claimant recovery rather
    /// than a redundant race.
    deferred: BTreeMap<String, BTreeMap<String, i64>>,
}

impl WatcherState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Note that `turn` was just observed under `processor_id`'s scope at
    /// `now_ms`. Duplicate ids inside the same pending window are ignored;
    /// this keeps the pending queue small when the source graph emits several
    /// `link-added` events for the same node (e.g. type + predicate + data
    /// links landing back-to-back).
    ///
    /// `last_touched_ms` (the debounce clock) advances **only for a genuinely
    /// new id**. This is what lets the polling watch loop actually settle: the
    /// loop re-gathers the *whole* transcript every tick and re-`record_item`s
    /// each turn, so if a duplicate re-emit bumped the clock, the debounce
    /// window would reset every tick and a batch would never drain. Debounce
    /// therefore means "quiet for `debounce_ms` since the last *new* item".
    pub fn record_item(&mut self, processor_id: &str, turn: PendingTurn, now_ms: i64) {
        let entry = self
            .per_processor
            .entry(processor_id.to_string())
            .or_default();
        if !entry.items.iter().any(|t| t.id == turn.id) {
            if entry.items.is_empty() {
                // First item of a fresh window — start the max-wait clock.
                entry.first_touched_ms = now_ms;
            }
            entry.items.push(turn);
            entry.last_touched_ms = now_ms;
        }
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
    ) -> Option<Vec<PendingTurn>> {
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
        // `batch_max.max(1)`: defence-in-depth against a 0 cap (config loading
        // already rejects it) — a 0 would take nothing, drain nothing, and the
        // caller would run an empty pass every tick without the queue emptying.
        let take = entry.items.len().min(cfg.batch_max.max(1));
        Some(entry.items.drain(..take).collect())
    }

    /// Record that this tick stood down for a batch's online elected author.
    /// Idempotent: only the *first* stand-down time is kept, so the stall clock
    /// measures continuous deferral, not the most recent tick.
    pub fn note_standdown(&mut self, batch_key: String, now_ms: i64) {
        self.standdown_since.entry(batch_key).or_insert(now_ms);
    }

    /// Whether a stood-down batch has now been deferred past `claim_ttl_ms` and
    /// should escalate past election straight to `try_claim`. `false` for a
    /// batch we have never stood down for.
    pub fn should_escalate(&self, batch_key: &str, now_ms: i64, claim_ttl_ms: i64) -> bool {
        self.standdown_since
            .get(batch_key)
            .is_some_and(|since| now_ms.saturating_sub(*since) >= claim_ttl_ms)
    }

    /// Clear a batch's stand-down record once it is processed (or a peer took
    /// it), so a later batch that happens to reuse the key starts a fresh clock.
    pub fn clear_standdown(&mut self, batch_key: &str) {
        self.standdown_since.remove(batch_key);
    }

    /// Hold `ids` back from re-enqueue for `ttl_ms`, because a peer holds the
    /// claim on them. Expired entries for this processor are pruned on the way
    /// in, so the map stays bounded by what is currently deferred rather than
    /// growing with every id ever backed off.
    pub fn defer_turns(&mut self, processor_id: &str, ids: &[String], now_ms: i64, ttl_ms: i64) {
        let entry = self.deferred.entry(processor_id.to_string()).or_default();
        entry.retain(|_, until| *until > now_ms);
        let until = now_ms.saturating_add(ttl_ms);
        for id in ids {
            entry.insert(id.clone(), until);
        }
    }

    /// Whether `id` is still inside a [`Self::defer_turns`] window.
    pub fn is_deferred(&self, processor_id: &str, id: &str, now_ms: i64) -> bool {
        self.deferred
            .get(processor_id)
            .and_then(|m| m.get(id))
            .is_some_and(|until| *until > now_ms)
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
    /// Won the claim but the drained batch was empty. Nothing to interpret;
    /// no LLM round-trip.
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

/// Threshold (seconds) after which a managed user is treated as offline for
/// auto-processor loop supervision purposes. Mirrors the freshness window used
/// by `capabilities::track_last_seen_from_token` for last-seen tracking, so a
/// user active by that surface is also considered active here.
pub const MANAGED_USER_ONLINE_WINDOW_S: i64 = 300;

/// Pure filter: from a list of `(user_email, last_seen_seconds)` tuples, return
/// the emails of users whose `last_seen` is within `threshold_s` of `now_s`.
///
/// Split from the live supervisor loop so the freshness policy is trivially
/// testable — no DB, no wall clock, no spawning. `last_seen == None` means
/// "never seen since server boot" and is treated as offline (a fresh managed
/// user who has not yet authenticated should not gate an LLM loop against
/// their DID). `last_seen > now_s` is capped to "just now" — a client with
/// a slightly-fast clock is still online, not future-perfect.
pub fn select_online_managed_users<I>(users: I, now_s: i64, threshold_s: i64) -> Vec<String>
where
    I: IntoIterator<Item = (String, Option<i64>)>,
{
    let cutoff = now_s.saturating_sub(threshold_s);
    users
        .into_iter()
        .filter_map(|(email, last_seen)| {
            let ls = last_seen?;
            // Clamp future timestamps forward-into-online; still filter by cutoff.
            let effective = ls.min(now_s);
            (effective >= cutoff).then_some(email)
        })
        .collect()
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
/// 3. Interpret the drained [`PendingTurn`] payload (no second SPARQL).
///    Empty `turns` = [`PassOutcome::EmptyTranscript`].
/// 4. [`run_interpretation_with_strategy`] with the deserialized dedup.
///
/// The pass always uses the executor's default LLM (via `AIService`).
/// `AutoProcessorConfig` is a neighbourhood-shared record and must not carry
/// per-peer LLM overrides — each peer picks a model that matches its local
/// hardware in `AIService` config.
pub async fn run_one_pass(
    perspective: &mut PerspectiveInstance,
    cfg: &AutoProcessorConfig,
    turns: &[PendingTurn],
    now_ms: i64,
    context: &AgentContext,
    escalate_past_election: bool,
) -> anyhow::Result<PassOutcome> {
    // Step signals (P-B2c): observable pass lifecycle for tests + the WS layer.
    // `me` (the acting agent's DID) is resolved up front so every signal is
    // tagged with which peer emitted it — the multi-user/executor observer uses
    // this to see who claimed vs backed off.
    let uuid = perspective.uuid.clone();
    let me = did_for_context(context)
        .map_err(|e| anyhow::anyhow!("run_one_pass: did_for_context: {e:#}"))?;
    let item_ids: Vec<String> = turns.iter().map(|t| t.id.clone()).collect();
    let batch_authors: Vec<String> = turns.iter().map(|t| t.speaker.clone()).collect();
    let pass_started = std::time::Instant::now();
    /*
       Derived once, up here, rather than at the claim below where it used to be.

       Every signal this pass emits carries it, including the ones emitted *before* a claim is
       attempted (`BatchReady`, `NotCandidate`, `AwaitingAuthor`, `BackedOff`). Computing it at
       the claim would have left exactly the stand-down signals — the ones a consumer most wants
       to attribute to a batch — as the only ones it could not join to a row.
    */
    let batch_key_hex = batch_key(&item_ids);
    macro_rules! signal {
        ($step:expr) => {
            emit(
                AutoProcessorEvent::new(&uuid, &cfg.processor_id, $step)
                    .with_agent_did(&me)
                    .with_items(&item_ids)
                    .with_batch_key(&batch_key_hex),
            )
            .await
        };
        ($step:expr, detail = $d:expr) => {
            emit(
                AutoProcessorEvent::new(&uuid, &cfg.processor_id, $step)
                    .with_agent_did(&me)
                    .with_items(&item_ids)
                    .with_batch_key(&batch_key_hex)
                    .with_detail($d),
            )
            .await
        };
        ($step:expr, bases = $b:expr) => {
            emit(
                AutoProcessorEvent::new(&uuid, &cfg.processor_id, $step)
                    .with_agent_did(&me)
                    .with_items(&item_ids)
                    .with_batch_key(&batch_key_hex)
                    .with_bases($b),
            )
            .await
        };
    }

    // 0. Authorship-ordered candidacy (fast-path, P-B2b3). Best-effort: a
    //    missing/erroring telepresence adapter or a batch with no resolvable
    //    authors falls through to `try_claim`, which is the real correctness
    //    guard. Otherwise it elects the first *online* author in message order
    //    and either proceeds (that is us), stands down for the winner, or waits
    //    when no author is online ("only participants process").
    //
    //    Stall-fallback: when `escalate_past_election` is set (the caller has
    //    seen this batch stand down past `claim_ttl_ms`), skip candidacy and go
    //    straight to the claim — the min-DID claim still prevents doubles among
    //    peers that escalate together. This is the liveness guard for an elected
    //    author that is online but never actually claims.
    if escalate_past_election {
        log::info!(
            "⚙️ auto_processor `{}`: escalating past election (elected author stalled > claim_ttl_ms); claiming anyway",
            cfg.processor_id
        );
    } else {
        match perspective.online_agents().await {
            Ok(agents) => {
                let online: Vec<String> = agents.into_iter().map(|a| a.did).collect();
                // Batch authors in message order, deduplicated (first occurrence
                // kept) — the participants `elect_author` walks.
                let mut authors: Vec<String> = Vec::new();
                for a in &batch_authors {
                    if !authors.contains(a) {
                        authors.push(a.clone());
                    }
                }
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
                            // Steady-state passive branch — debug per Nico's
                            // level policy (fires every tick while another
                            // peer is elected).
                            log::debug!(
                                "⚙️ auto_processor `{}`: standing down — online author `{winner}` precedes us in message order",
                                cfg.processor_id
                            );
                            signal!(AutoProcessorStep::NotCandidate, detail = winner.clone());
                            return Ok(PassOutcome::NotCandidate { winner });
                        }
                        AuthorElection::NoneOnline => {
                            log::debug!(
                                "⚙️ auto_processor `{}`: no batch author online — waiting for a participant to return before processing",
                                cfg.processor_id
                            );
                            signal!(AutoProcessorStep::AwaitingAuthor);
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
    }

    // 1. Reserve the batch.
    let claim = try_claim(
        perspective,
        &cfg.processor_id,
        &item_ids,
        cfg.claim_ttl_ms,
        now_ms,
        context,
    )
    .await?;
    if let ClaimOutcome::BackedOff { holder } = claim {
        // Steady-state passive branch — debug per Nico's level policy
        // (fires every tick while another peer holds the claim).
        log::debug!(
            "⚙️ auto_processor `{}`: backed off — holder `{holder}` has the claim",
            cfg.processor_id
        );
        signal!(AutoProcessorStep::BackedOff, detail = holder.clone());
        return Ok(PassOutcome::BackedOff { holder });
    }
    // Signal-based Claimed (and the neighbourhood-state Claimed row below)
    // are DID- / perspective-scoped events with their own Abandoned /
    // Finished companions on every exit path, so they can fire here right
    // after `try_claim` succeeds. The lifecycle **info log** (`picked task`
    // ↔ `completed`) is deliberately deferred until after the no-op guards
    // (ShapesMissing / EmptyTranscript) so every emitted `picked task` has
    // a matching `✅ completed` or `❌ abandoned` companion — see the
    // one-to-one pairing note in rust-executor/LOGGING.md and CodeRabbit
    // review on PR #942 (round 2, watcher.rs claimed-pass lifecycle).
    signal!(AutoProcessorStep::Claimed);
    // Neighbourhood-state (Nico 2026-08-19): a small perspective-scoped
    // event so a UI can render "someone is auto-processing here". Distinct
    // from `AutoProcessorStep::Claimed` above (which is DID-scoped and
    // carries the batch payload) — this one is delivered to every reader
    // of the perspective, and carries only the claimant + batch key.
    emit_neighbourhood_state(AutoProcessorNeighbourhoodState::new(
        &uuid,
        &cfg.processor_id,
        &me,
        &batch_key_hex,
        NeighbourhoodPhase::Claimed,
    ))
    .await;

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
        signal!(
            AutoProcessorStep::ShapesMissing,
            detail = missing.join(", ")
        );
        // Close the neighbourhood-state Claimed row: this pass claimed the
        // batch but is walking away without processing it. Without this,
        // an observer's UI would keep showing "in progress" for a batch
        // that will only clear when the claim TTL-expires.
        emit_neighbourhood_state(AutoProcessorNeighbourhoodState::new(
            &uuid,
            &cfg.processor_id,
            &me,
            &batch_key_hex,
            NeighbourhoodPhase::Abandoned,
        ))
        .await;
        return Ok(PassOutcome::ShapesMissing { missing });
    }

    // 3. Interpret the drained batch — no second SPARQL.
    signal!(AutoProcessorStep::GatheringTranscript);
    if turns.is_empty() {
        // Steady-state passive branch — debug per Nico's level policy.
        log::debug!(
            "⚙️ auto_processor `{}`: drained batch is empty; nothing to interpret",
            cfg.processor_id
        );
        signal!(AutoProcessorStep::EmptyTranscript);
        emit_neighbourhood_state(AutoProcessorNeighbourhoodState::new(
            &uuid,
            &cfg.processor_id,
            &me,
            &batch_key_hex,
            NeighbourhoodPhase::Abandoned,
        ))
        .await;
        return Ok(PassOutcome::EmptyTranscript);
    }

    // Past both no-op guards: from here on the pass is committed to a
    // real interpretation attempt, so emit the lifecycle `picked task`
    // info line. Every path out from here MUST emit a terminal companion:
    // `✅ … completed` on the happy path (below) or `❌ … abandoned` on
    // any fallible-op failure between here and the completion line
    // (wrapped via `abandon_on_err!` so `?` still bubbles the error up).
    log::info!(
        "⚙️ auto-processor picked task processor={} items={} perspective={}",
        cfg.processor_id,
        item_ids.len(),
        uuid
    );

    // Companion log for post-claim failures — pairs every `picked task`
    // with an `❌ abandoned` when a fallible operation returns Err.
    // Matches the AI/interpretation failure-companion pattern established
    // in the round-2 refactor.
    macro_rules! abandon_on_err {
        ($expr:expr) => {
            match $expr {
                Ok(v) => v,
                Err(e) => {
                    log::error!(
                        "❌ ⚙️ auto-processor task abandoned processor={} items={} perspective={} reason={:#}",
                        cfg.processor_id,
                        item_ids.len(),
                        uuid,
                        e
                    );
                    return Err(e.into());
                }
            }
        };
    }

    let transcript: Vec<TranscriptTurn> = turns.iter().map(|t| t.as_transcript()).collect();

    // 4. Interpret.
    signal!(AutoProcessorStep::RunningInterpretation);
    let dedup = parse_dedup_strategy_json(cfg.dedup_strategy_json.as_deref());
    // Spawn scope: the processor's configured `base_prefix`, or a per-processor
    // default when the config omits it (original behaviour).
    let base_prefix = cfg
        .base_prefix
        .clone()
        .unwrap_or_else(|| format!("ad4m://autoprocessor/{}/instance/", cfg.processor_id));

    // Pre-pass snapshot of every instance URI the interpretation classes
    // already know about — the whole perspective, NOT filtered by
    // `existing_scope`, because we need to know whether a returned base
    // pre-existed anywhere (not just within our dedup scope). Used below to
    // distinguish freshly-created bases from upserts of already-existing
    // instances for mint-scope linking (CodeRabbit #902 review): linking an
    // upserted pre-existing instance would multi-parent unrelated graph state
    // into our scope. Skipped entirely when mint_scope is absent — nothing
    // consumes the snapshot in that path.
    let pre_existing_uris: HashSet<String> = if cfg.mint_scope.is_some() {
        abandon_on_err!(
            crate::perspectives::interpretation::existing_instance_context(
                perspective,
                &shapes,
                None,
            )
            .await
        )
        .into_values()
        .flat_map(|instances| instances.into_iter().map(|i| i.id))
        .collect()
    } else {
        HashSet::new()
    };

    let cursor = InterpretationRunCursor {
        processor: super::config::processor_node(&cfg.processor_id),
        sources: item_ids.clone(),
    };
    // Build the emit context once so both fork branches can share it.
    // `cfg.emit_debug_events == true` enables the mid-pass `LlmRequestSent`
    // / `LlmResponseReceived` events (dev PR #903) — for the classic path
    // via `run_interpretation_with_strategy_and_model`, for the harness
    // path we pass this into `run_with_tools` for per-tool-call events
    // in a follow-up commit on this branch (Nico 2026-08-25).
    let emit_ctx = cfg
        .emit_debug_events
        .then(|| super::events::InterpretationEmitContext {
            perspective_uuid: uuid.clone(),
            processor_id: cfg.processor_id.clone(),
            agent_did: me.clone(),
            item_ids: item_ids.clone(),
            batch_key: batch_key_hex.clone(),
        });

    // Fork: harness-dispatched pass when the operator opted in via
    // `AutoProcessorConfig.max_tool_calls > 0`; otherwise the classic
    // single-shot LLM+parse+plan pipeline. The two paths converge on the
    // same overlay-writing gate (`apply_with_overlay`), so downstream
    // provenance + processed signalling is identical.
    //
    // Debug carriage differs by path today:
    // * classic → `InterpretationOutcome { bases, debug }` — `debug`
    //   carries the raw prompt/response for persistence on the
    //   InterpretationRun node.
    // * harness → `Vec<String>` bases only. The harness path emits
    //   per-tool-call events via its own logging surface (see harness/mod.rs
    //   `harness: round=` prints); persisting the full transcript on
    //   InterpretationRun is a follow-up (there's no single prompt/response
    //   to snapshot — it's a multi-turn loop).
    let (bases, debug) = match cfg.max_tool_calls {
        Some(n) if n > 0 => {
            let bases = abandon_on_err!(
                run_interpretation_with_harness_and_model(
                    perspective,
                    &shapes,
                    &transcript,
                    &base_prefix,
                    context,
                    None,
                    cfg.existing_scope.as_ref(),
                    Some(&cursor),
                    n,
                    // Auto-processor is an internal caller — no per-pass user
                    // token to bill. MCP admin credential (if configured) is
                    // read from env inside the harness path.
                    None,
                    // `emit_debug_events` gates the same event stream the
                    // classic path uses; `None` when disabled means the
                    // per-tool-call events also stay silent.
                    emit_ctx.as_ref(),
                    // Dedup on drain: the auto-processor runs indefinitely, so
                    // re-proposed instances must collapse to updates (not
                    // unbounded duplicate creates). James Weir PR #911 review.
                    true,
                    // No per-pass credit gate on the auto-processor path:
                    // the watcher runs as an internal service (no user
                    // session to bill against) and each completion still
                    // fire-and-forgets bill_prompt_if_authed via AIService.
                    None,
                )
                .await
            );
            (bases, None)
        }
        _ => {
            let outcome = abandon_on_err!(
                run_interpretation_with_strategy_and_model(
                    perspective,
                    &shapes,
                    &transcript,
                    &base_prefix,
                    context,
                    &dedup,
                    None,
                    // Existing-instance scope: constrains dedup to a subtree
                    // when the processor config specifies one, e.g. "existing
                    // Task instances that live under project X." `None` keeps
                    // the whole-perspective existing set — the pre-scope-config
                    // behaviour.
                    cfg.existing_scope.as_ref(),
                    Some(&cursor),
                    cfg.emit_debug_events,
                    emit_ctx.as_ref(),
                )
                .await
            );
            (outcome.bases, outcome.debug)
        }
    };

    // Mint-scope child links: if the processor declares a `mint_scope`, wire
    // every **freshly created** base as a child under the target node via the
    // configured predicate — turning the SoA-tree "children live under this
    // node" from a URI-prefix convention into an actual graph edge. We filter
    // to freshly-created bases (not present in `pre_existing_uris`) so that
    // upserts of pre-existing instances don't get re-parented into our scope
    // (CodeRabbit #902 review). Written outside the interpretation batch: if
    // the executor crashes between the mint and the link write, the base is
    // orphaned but the next pass will upsert by identity + re-attempt the
    // link write (idempotent). The `Processed` signal fires only after the
    // mint-scope write succeeds, so observers never see a false "done" state.
    if let Some(mint_scope) = &cfg.mint_scope {
        let created = partition_created(&bases, &pre_existing_uris);
        abandon_on_err!(
            write_mint_scope_links(
                perspective,
                mint_scope,
                &created,
                context,
                &cfg.processor_id,
            )
            .await
        );
    }
    // Emit the `Processed` event. Carries the final `bases` list — the
    // LLM prompt + response now travel via the mid-pass `LlmRequestSent`
    // and `LlmResponseReceived` events (Nico 2026-08-20), so a subscribing
    // UI can render "waiting on LLM" between them instead of only seeing
    // one lump payload here at the end. `_debug` is intentionally not
    // attached to `Processed` any more — the persistent `InterpretationRun`
    // (`debug_prompt` / `debug_response`) is the post-hoc lookup channel.
    let _ = debug; // consumed by the interpretation engine → InterpretationRun
    let ev = AutoProcessorEvent::new(&uuid, &cfg.processor_id, AutoProcessorStep::Processed)
        .with_agent_did(&me)
        .with_items(&item_ids)
        .with_batch_key(&batch_key_hex)
        .with_bases(&bases);
    emit(ev).await;
    // Neighbourhood-state: pass complete on this executor. Consumers use
    // this to close out the `Claimed` row they showed for the same
    // `batch_key`.
    emit_neighbourhood_state(AutoProcessorNeighbourhoodState::new(
        &uuid,
        &cfg.processor_id,
        &me,
        &batch_key_hex,
        NeighbourhoodPhase::Finished,
    ))
    .await;

    log::info!(
        "✅ ⚙️ auto-processor task completed processor={} items={} bases={} latency={}ms",
        cfg.processor_id,
        item_ids.len(),
        bases.len(),
        pass_started.elapsed().as_millis()
    );
    Ok(PassOutcome::Won { bases })
}

/// Add a `mint_scope.id --predicate--> new_base_uri` link for every URI in
/// `bases`. Only [`Scope::Raw`] carries an explicit predicate, so
/// [`Scope::Model`] is treated as configuration error rather than a silent
/// no-op — the caller should have constructed a `Raw` variant when it needs
/// mint-time child linking.
///
/// Return every URI in `bases` that is NOT in `pre_existing`, preserving
/// original order. Used by mint-scope linking (CodeRabbit #902 review) to
/// exclude upserts of pre-existing instances — writing a mint-scope link
/// for those would multi-parent unrelated graph state into our scope.
pub(crate) fn partition_created<'a>(
    bases: &'a [String],
    pre_existing: &HashSet<String>,
) -> Vec<String> {
    bases
        .iter()
        .filter(|b| !pre_existing.contains(*b))
        .cloned()
        .collect()
}

pub(crate) async fn write_mint_scope_links(
    perspective: &mut PerspectiveInstance,
    mint_scope: &Scope,
    bases: &[String],
    context: &AgentContext,
    processor_id: &str,
) -> anyhow::Result<()> {
    let (parent_id, predicate) = match mint_scope {
        Scope::Raw { id, predicate } => (id.clone(), predicate.clone()),
        Scope::Model { .. } => {
            anyhow::bail!(
                "auto_processor `{processor_id}`: mint_scope must be a `Raw` scope \
                 (id + predicate); `Model` scopes carry no linking predicate"
            );
        }
    };
    for base in bases {
        perspective
            .add_link(
                Link {
                    source: parent_id.clone(),
                    predicate: Some(predicate.clone()),
                    target: base.clone(),
                },
                LinkStatus::Shared,
                None,
                context,
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "auto_processor `{processor_id}`: mint_scope add_link({parent_id} -> {base}) \
                     failed: {e:#}"
                )
            })?;
    }
    Ok(())
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
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms,
            batch_min: 1,
            batch_max,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
        }
    }

    fn pt(id: &str) -> PendingTurn {
        PendingTurn {
            id: id.to_string(),
            speaker: "s".into(),
            text: id.to_string(),
            timestamp: "ts".into(),
        }
    }

    fn rec(w: &mut WatcherState, proc: &str, id: &str, now: i64) {
        w.record_item(proc, pt(id), now);
    }

    fn ids(turns: &[PendingTurn]) -> Vec<String> {
        turns.iter().map(|t| t.id.clone()).collect()
    }

    // ---- turn_id -----------------------------------------------------------

    /// `turn_id` is deterministic, injective across field boundaries (including
    /// timestamp), and short (16 hex chars). Identical bodies at different
    /// timestamps are distinct; the same (speaker, text, timestamp) collapses.
    #[test]
    fn turn_id_is_deterministic_and_field_injective() {
        assert_eq!(turn_id("alice", "hi", "t1"), turn_id("alice", "hi", "t1"));
        assert_ne!(turn_id("alice", "hi", "t1"), turn_id("alic", "ehi", "t1"));
        assert_ne!(turn_id("alice", "hi", "t1"), turn_id("bob", "hi", "t1"));
        assert_ne!(
            turn_id("alice", "yes", "t1"),
            turn_id("alice", "yes", "t2"),
            "identical content at different times is a new turn"
        );
        assert_eq!(turn_id("alice", "hi", "t1").len(), 16);
        assert!(turn_id("alice", "hi", "t1")
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

    // ---- select_online_managed_users ---------------------------------------

    /// The supervisor filters by wall-clock freshness — users seen within the
    /// window get a loop; users beyond it do not. Empty-last-seen is the
    /// never-yet-authenticated case and is excluded (we do not want to spawn
    /// an LLM loop against a DID that has never touched the server).
    #[test]
    fn selects_users_within_freshness_window() {
        let now = 1_000_000_i64;
        let window = 300_i64;
        let online = select_online_managed_users(
            vec![
                ("alice@x".into(), Some(now - 10)),   // seen 10s ago
                ("bob@x".into(), Some(now - 299)),    // right on the edge, still in
                ("carol@x".into(), Some(now - 301)),  // just past — out
                ("dave@x".into(), Some(now - 5_000)), // long stale — out
                ("eve@x".into(), None),               // never seen — out
            ],
            now,
            window,
        );
        assert_eq!(online, vec!["alice@x", "bob@x"]);
    }

    /// A last-seen slightly in the future (client clock skew) still counts
    /// as online — the effective timestamp is clamped forward-into-now. Only
    /// a stale absolute time-since-window pushes a user out.
    #[test]
    fn future_last_seen_is_treated_as_online() {
        let now = 1_000_000_i64;
        let window = 300_i64;
        let online = select_online_managed_users(
            vec![
                ("skewed@x".into(), Some(now + 60)), // 1 min in the future
                ("stale@x".into(), Some(now - 400)), // 400s ago — out
            ],
            now,
            window,
        );
        assert_eq!(online, vec!["skewed@x"]);
    }

    /// Empty input → empty output; the supervisor treats an empty user list
    /// as "no managed users online right now" and spawns no loops.
    #[test]
    fn empty_input_selects_no_users() {
        let selected =
            select_online_managed_users(Vec::<(String, Option<i64>)>::new(), 1_000_000, 300);
        assert!(selected.is_empty());
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

    /// A backed-off batch is held back until the claim TTL elapses, then
    /// becomes eligible again (the crashed-claimant recovery path). Deferrals
    /// are per-processor and expired entries are pruned rather than retained.
    #[test]
    fn deferred_turns_expire_and_are_processor_scoped() {
        let mut w = WatcherState::new();
        let ids = vec!["i1".to_string(), "i2".to_string()];
        w.defer_turns("p", &ids, 1_000, 60_000);

        assert!(w.is_deferred("p", "i1", 1_000));
        assert!(w.is_deferred("p", "i2", 60_000));
        assert!(!w.is_deferred("p", "i3", 1_000), "only the given ids");
        assert!(!w.is_deferred("other", "i1", 1_000), "per-processor");
        assert!(
            !w.is_deferred("p", "i1", 61_000),
            "past now+ttl the batch is eligible again"
        );

        // A later deferral prunes the expired entries instead of accumulating.
        w.defer_turns("p", &["i9".to_string()], 61_000, 60_000);
        assert!(!w.is_deferred("p", "i1", 61_000));
        assert!(w.is_deferred("p", "i9", 61_000));
    }

    /// A deferred id is not re-enqueued while its window is open, but the
    /// debounce clock is untouched, so unrelated turns still batch normally.
    #[test]
    fn deferred_ids_are_skipped_by_the_caller_not_the_queue() {
        let mut w = WatcherState::new();
        w.defer_turns("p", &["i1".to_string()], 1_000, 60_000);
        // `record_item` itself is unconditional — the tick consults
        // `is_deferred` before recording, so a caller that ignores it still
        // works exactly as before.
        rec(&mut w, "p", "i2", 1_000);
        assert_eq!(w.pending_for("p").expect("pending").items.len(), 1);
    }

    /// Stall-fallback clock: a batch we never stood down for never escalates;
    /// once stood down, it escalates only after `claim_ttl_ms` has elapsed; and
    /// clearing it resets the clock.
    #[test]
    fn standdown_escalates_only_after_claim_ttl() {
        let mut w = WatcherState::new();
        let ttl = 60_000;
        // Never stood down → never escalate.
        assert!(!w.should_escalate("k", 1_000_000, ttl));

        // First stand-down at t=1_000_000. `note_standdown` is first-write-wins.
        w.note_standdown("k".into(), 1_000_000);
        w.note_standdown("k".into(), 1_030_000); // ignored — keeps the earliest
        assert!(!w.should_escalate("k", 1_030_000, ttl)); // 30s < 60s
        assert!(w.should_escalate("k", 1_060_000, ttl)); // exactly ttl → escalate
        assert!(w.should_escalate("k", 1_120_000, ttl)); // well past

        // A different batch is independent.
        assert!(!w.should_escalate("other", 2_000_000, ttl));

        // Clearing resets: a later reuse of the key starts a fresh clock.
        w.clear_standdown("k");
        assert!(!w.should_escalate("k", 1_120_000, ttl));
    }

    /// Items recorded but the debounce window has not elapsed yet → None.
    /// (`now - last_touched = 500ms < 1_000ms`.)
    #[test]
    fn drain_returns_none_while_debounce_not_elapsed() {
        let mut w = WatcherState::new();
        rec(&mut w, "p", "i1", 10_000);
        rec(&mut w, "p", "i2", 10_400);
        let c = cfg("p", 1_000, 32);
        assert_eq!(w.drain_ready_batch(&c, 10_900), None);
    }

    /// Duplicate ids in the pending window are collapsed to one entry. Guards
    /// against noisy `link-added` streams (type + predicate + data links per
    /// node) fanning the batch out with the same id.
    #[test]
    fn record_dedupes_repeated_ids() {
        let mut w = WatcherState::new();
        rec(&mut w, "p", "i1", 100);
        rec(&mut w, "p", "i1", 200);
        rec(&mut w, "p", "i2", 300);
        rec(&mut w, "p", "i1", 400);
        let pending = w.pending_for("p").expect("pending exists");
        assert_eq!(
            ids(&pending.items),
            vec!["i1".to_string(), "i2".to_string()]
        );
        // last_touched advances only on a genuinely NEW id: i1@100 then i2@300.
        // The duplicate i1 re-emits at 200 and 400 must NOT bump it — otherwise
        // the polling loop's every-tick re-gather would reset the debounce
        // window forever and no batch would ever drain. So it's 300, not 400.
        assert_eq!(pending.last_touched_ms, 300);
    }

    /// Debounce elapsed + pending non-empty → drain returns everything (below
    /// the cap) and empties the queue.
    #[test]
    fn drain_returns_all_pending_after_debounce() {
        let mut w = WatcherState::new();
        rec(&mut w, "p", "i1", 1_000);
        rec(&mut w, "p", "i2", 1_500);
        let c = cfg("p", 1_000, 32);
        let batch = w
            .drain_ready_batch(&c, 3_000)
            .expect("debounce elapsed → batch");
        assert_eq!(ids(&batch), vec!["i1".to_string(), "i2".to_string()]);
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
            rec(&mut w, "p", &format!("i{i}"), 1_000 + i as i64);
        }
        let c = cfg("p", 100, 2);
        let batch = w.drain_ready_batch(&c, 5_000).expect("batch");
        assert_eq!(ids(&batch), vec!["i0".to_string(), "i1".to_string()]);
        assert_eq!(
            ids(&w.pending_for("p").unwrap().items),
            vec!["i2".to_string(), "i3".to_string(), "i4".to_string()],
            "overflow must be preserved in insertion order for the next window"
        );
    }

    /// `batch_min` holds a below-threshold batch even after the debounce has
    /// settled — the Flux "wait for N inputs" rule. Two items pending, min is
    /// 3, no `max_wait_ms` → keep waiting (None) and leave the queue intact.
    #[test]
    fn drain_holds_below_batch_min() {
        let mut w = WatcherState::new();
        rec(&mut w, "p", "i1", 1_000);
        rec(&mut w, "p", "i2", 1_100);
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
        rec(&mut w, "p", "i1", 1_000);
        rec(&mut w, "p", "i2", 1_100);
        rec(&mut w, "p", "i3", 1_200);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 3;
        let batch = w.drain_ready_batch(&c, 5_000).expect("batch");
        assert_eq!(
            ids(&batch),
            vec!["i1".to_string(), "i2".to_string(), "i3".to_string()]
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
        rec(&mut w, "p", "i1", 1_000);
        rec(&mut w, "p", "i2", 1_100);
        let mut c = cfg("p", 100, 32);
        c.batch_min = 5;
        c.max_wait_ms = Some(2_000);
        // Debounce settled (no arrival since 1_100) and 3_500-1_000=2_500 ≥
        // 2_000 max_wait → flush the partial batch.
        let batch = w.drain_ready_batch(&c, 3_500).expect("flush");
        assert_eq!(
            ids(&batch),
            vec!["i1".to_string(), "i2".to_string()],
            "max_wait must flush a sub-min batch rather than orphan it"
        );
    }

    /// `max_wait_ms` does not fire early: before the oldest item ages past the
    /// window, a sub-min batch is still held.
    #[test]
    fn drain_respects_max_wait_before_expiry() {
        let mut w = WatcherState::new();
        rec(&mut w, "p", "i1", 1_000);
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
        rec(&mut w, "a", "a-1", 1_000);
        rec(&mut w, "b", "b-1", 2_000);
        let ca = cfg("a", 100, 32);
        let cb = cfg("b", 100, 32);
        let batch_a = w.drain_ready_batch(&ca, 1_500).expect("a");
        assert_eq!(ids(&batch_a), vec!["a-1".to_string()]);
        assert!(
            w.pending_for("a").unwrap().items.is_empty(),
            "'a' drained, 'b' untouched"
        );
        assert_eq!(
            ids(&w.pending_for("b").unwrap().items),
            vec!["b-1".to_string()],
            "'b' state untouched by 'a' drain"
        );
        let batch_b = w.drain_ready_batch(&cb, 2_500).expect("b");
        assert_eq!(ids(&batch_b), vec!["b-1".to_string()]);
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
        let items = vec![pt("i1"), pt("i2")];
        let key = batch_key(&ids(&items));
        write_claim(&mut p, "proc", &key, "aaa:incumbent", 60_000, &ctx)
            .await
            .expect("seed incumbent");

        let outcome = run_one_pass(&mut p, &cfg, &items, 1_000, &ctx, false)
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
        let items = vec![pt("i1")];
        let outcome = run_one_pass(&mut p, &cfg, &items, 1_000, &ctx, false)
            .await
            .expect("run_one_pass");
        match outcome {
            PassOutcome::ShapesMissing { missing } => {
                assert_eq!(missing, vec!["ns://Unregistered".to_string()]);
            }
            other => panic!("expected ShapesMissing, got {other:?}"),
        }
    }

    // ---- write_mint_scope_links -------------------------------------------

    /// `Raw` mint_scope + N minted bases → N shared `parent --predicate--> base`
    /// links land on the perspective. Verifies the SoA-tree child-link write is
    /// atomic in intent (one call, N links) and uses the configured predicate.
    #[tokio::test]
    async fn write_mint_scope_links_writes_one_shared_link_per_base() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let scope = Scope::Raw {
            id: "soa://project/42".into(),
            predicate: "soa://contains".into(),
        };
        let bases = vec![
            "soa://project/42/task/a".to_string(),
            "soa://project/42/task/b".to_string(),
        ];
        write_mint_scope_links(&mut p, &scope, &bases, &ctx, "test-proc")
            .await
            .expect("write_mint_scope_links");
        let links = p
            .get_links(&crate::types::LinkQuery {
                source: Some("soa://project/42".into()),
                predicate: Some("soa://contains".into()),
                ..Default::default()
            })
            .await
            .expect("get_links");
        let targets: std::collections::BTreeSet<String> =
            links.iter().map(|l| l.data.target.clone()).collect();
        assert_eq!(
            targets,
            bases
                .iter()
                .cloned()
                .collect::<std::collections::BTreeSet<_>>(),
            "every minted base linked as child under the mint_scope parent"
        );
        for l in &links {
            assert_eq!(l.status, Some(LinkStatus::Shared), "child link must sync");
        }
    }

    /// Empty base list → no writes; a no-op is still success.
    #[tokio::test]
    async fn write_mint_scope_links_handles_empty_bases() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let scope = Scope::Raw {
            id: "soa://p".into(),
            predicate: "soa://c".into(),
        };
        write_mint_scope_links(&mut p, &scope, &[], &ctx, "empty")
            .await
            .expect("write_mint_scope_links");
        let links = p
            .get_links(&crate::types::LinkQuery {
                source: Some("soa://p".into()),
                ..Default::default()
            })
            .await
            .expect("get_links");
        assert!(links.is_empty(), "no bases → no links");
    }

    // ---- partition_created (CodeRabbit #902 fix) ---------------------------

    /// Every base that was NOT in the pre-pass snapshot is created; every base
    /// that WAS in the snapshot is an upsert and must not be linked into the
    /// mint scope (would multi-parent unrelated graph state).
    #[test]
    fn partition_created_filters_upserts_out() {
        let bases = vec![
            "soa://new/task-a".to_string(),
            "soa://existing/task-b".to_string(),
            "soa://new/task-c".to_string(),
        ];
        let mut pre: HashSet<String> = HashSet::new();
        pre.insert("soa://existing/task-b".into());
        pre.insert("soa://existing/task-z-untouched".into()); // extra pre-existing not in bases
        let created = partition_created(&bases, &pre);
        assert_eq!(
            created,
            vec!["soa://new/task-a", "soa://new/task-c"],
            "pre-existing bases are excluded, order preserved for the rest"
        );
    }

    /// Empty pre-existing set → every returned base is "created."
    #[test]
    fn partition_created_all_new_when_snapshot_empty() {
        let bases = vec!["a".to_string(), "b".to_string()];
        let created = partition_created(&bases, &HashSet::new());
        assert_eq!(created, bases);
    }

    /// Every base pre-existed → the created set is empty (no mint-scope
    /// linking should happen this pass).
    #[test]
    fn partition_created_all_upserts_returns_empty() {
        let bases = vec!["a".to_string(), "b".to_string()];
        let mut pre: HashSet<String> = HashSet::new();
        pre.insert("a".into());
        pre.insert("b".into());
        let created = partition_created(&bases, &pre);
        assert!(
            created.is_empty(),
            "when every base pre-existed, no new links are written"
        );
    }

    /// `Model` scope has no predicate, so mint-time child linking is a config
    /// error rather than a silent no-op: the config declared a mint target,
    /// dropping it would create unlinked bases under a UI expecting children.
    #[tokio::test]
    async fn write_mint_scope_links_errors_on_model_scope() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let scope = Scope::Model {
            model: "Project".into(),
            id: "soa://project/42".into(),
            field: None,
        };
        let err = write_mint_scope_links(&mut p, &scope, &["x".into()], &ctx, "modelled")
            .await
            .expect_err("model scope must fail");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("must be a `Raw` scope"),
            "error mentions the shape mismatch: {msg}"
        );
    }
}
