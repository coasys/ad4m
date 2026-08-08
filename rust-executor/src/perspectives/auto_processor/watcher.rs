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

use crate::agent::AgentContext;
use crate::perspectives::interpretation::{
    gather_transcript_sparql, run_interpretation_with_strategy_and_model, DedupStrategy,
};
use crate::perspectives::model_query::load_shape_from_store;
use crate::perspectives::perspective_instance::PerspectiveInstance;

use super::claim::{try_claim, ClaimOutcome};
use super::config::AutoProcessorConfig;

use std::collections::BTreeMap;

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
        if !entry.items.contains(&item_id) {
            entry.items.push(item_id);
        }
        entry.last_touched_ms = now_ms;
    }

    /// Read-only view of a processor's pending state (tests + observability).
    pub fn pending_for(&self, processor_id: &str) -> Option<&ProcessorPending> {
        self.per_processor.get(processor_id)
    }

    /// If `now_ms - last_touched >= cfg.debounce_ms` and pending is non-empty,
    /// drain up to `cfg.batch_max` ids (FIFO) and return them; anything over
    /// the cap stays in the queue for the next window.
    ///
    /// Returns `None` when nothing is pending or the debounce hasn't elapsed
    /// — the watcher-loop caller should just wait.
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
    /// The claim was ours, but at least one of `cfg.interpretation_classes`
    /// did not resolve via `load_shape` (partial-sync SDNA / config typo).
    /// We logged, skipped the LLM call and let the claim TTL-expire so a
    /// later pass — once the shape lands — can re-take it.
    ShapesMissing { missing: Vec<String> },
    /// Won the claim but `source_scope_query` returned zero `(speaker, text)`
    /// rows. Nothing to interpret; no LLM round-trip.
    EmptyTranscript,
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
            let base_url = value
                .get("base_url")
                .and_then(|v| v.as_str())
                .map(str::to_string);
            let model = value
                .get("model")
                .and_then(|v| v.as_str())
                .map(str::to_string);
            let threshold = value
                .get("threshold")
                .and_then(|v| v.as_f64())
                .map(|f| f as f32);
            match (base_url, model, threshold) {
                (Some(base_url), Some(model), Some(threshold)) => DedupStrategy::Semantic {
                    base_url,
                    model,
                    threshold,
                },
                _ => {
                    log::warn!(
                        "parse_dedup_strategy_json: semantic strategy missing required fields \
                         (base_url/model/threshold) in `{json}`; falling back to default"
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
            batch_max,
            claim_ttl_ms: 60_000,
            llm_base_url: None,
            llm_model: None,
            dedup_strategy_json: None,
        }
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
            r#"{"kind":"semantic","base_url":"http://x/v1","model":"nomic","threshold":0.8}"#,
        ));
        match strat {
            DedupStrategy::Semantic {
                base_url,
                model,
                threshold,
            } => {
                assert_eq!(base_url, "http://x/v1");
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
