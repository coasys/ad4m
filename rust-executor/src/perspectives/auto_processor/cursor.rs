//! Processed-turn cursor: gather-side optional time filter + a model query
//! over [`InterpretationRun`](crate::perspectives::interpretation) `sources`.
//!
//! The watch tick subtracts in Rust: scope query, drop turns older than
//! `source_window_ms` when set, load this processor's source IDs (optionally
//! bounded by `ran_at`), skip those.
//!
//! The cursor is what retires a turn, so it is read through the model query
//! rather than raw SPARQL: only links whose signature verifies hydrate
//! (#1113, default provenance — `includeUnverified` is never set), so a run
//! whose links do not verify cannot mark a turn as processed.

use super::config::processor_node;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::HashSet;

/// True when `timestamp` (RFC3339 on the body-link reifier) is inside
/// `[now_ms - window_ms, now_ms]`. Unparseable timestamps are kept — dropping
/// them would silently lose turns the gatherer already accepted.
pub fn turn_in_source_window(timestamp: &str, now_ms: i64, window_ms: i64) -> bool {
    let Ok(dt) = chrono::DateTime::parse_from_rfc3339(timestamp) else {
        return true;
    };
    dt.timestamp_millis() >= now_ms.saturating_sub(window_ms)
}

/// One row of the cursor query: a turn id a run of this processor consumed,
/// and that run's `ran_at` (unix millis; `None` when absent or unparseable).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessedSource {
    pub id: String,
    pub ran_at_ms: Option<i64>,
}

/// Read every turn id this processor's runs consumed, with each run's
/// `ran_at`. Not windowed: the window depends on `now_ms`, so the watch loop
/// keeps these rows while the store is unchanged and applies
/// [`processed_ids_in_window`] at each tick.
///
/// The run class is hard-wired but registered lazily (first interpretation
/// write), so a perspective that never ran one has no shape to query and no
/// cursor.
pub async fn load_processed_sources(
    perspective: &PerspectiveInstance,
    processor_id: &str,
) -> anyhow::Result<Vec<ProcessedSource>> {
    use crate::perspectives::hardwired_class::subject_class_registered;
    use crate::perspectives::interpretation::{INTERP_RUN_CLASS, INTERP_RUN_TARGET_CLASS};

    if !subject_class_registered(perspective, INTERP_RUN_TARGET_CLASS).await? {
        return Ok(Vec::new());
    }
    // `ranAt` only matters once a window applies it in Rust — it stays a
    // `literal:string:` IRI, so an `xsd:integer` FILTER could never apply to
    // it — but the cursor needs it here, not in the query.
    let query = serde_json::json!({
        "properties": ["ranAt", "sources"],
        "where": { "processor": processor_node(processor_id) },
    })
    .to_string();
    // Zero shape-wait, like the transcript read on the same tick: the marker
    // check above means a missing shape here is a partial sync, and the tick
    // must not stall 20 s on it — the run-class SHACL write wakes the loop
    // and the error keeps the processor's turns pending for the retry.
    let result_json = perspective
        .model_query_within(INTERP_RUN_CLASS, &query, std::time::Duration::ZERO)
        .await
        .map_err(|e| anyhow::anyhow!("load_processed_sources: model_query failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&result_json)
        .map_err(|e| anyhow::anyhow!("load_processed_sources: bad model_query result: {e:#}"))?;
    let mut out = Vec::new();
    for instance in result["instances"].as_array().into_iter().flatten() {
        let ran_at_ms =
            super::scalar_string(instance.get("ranAt")).and_then(|s| s.parse::<i64>().ok());
        for id in instance
            .get("sources")
            .and_then(|v| v.as_array())
            .into_iter()
            .flatten()
            .filter_map(|v| v.as_str())
        {
            out.push(ProcessedSource {
                id: id.to_string(),
                ran_at_ms,
            });
        }
    }
    Ok(out)
}

/// The turn ids `rows` retire at `now_ms`. When `window_ms` is `Some`, only
/// runs whose `ran_at` is inside that window count; `None` is unbounded and
/// ignores `ran_at` entirely.
///
/// In the windowed case a row whose `ran_at` is absent or unparseable is
/// **dropped**, i.e. treated as not-yet-processed. The cursor is a suppression
/// list, so the conservative direction is to re-process (output identity-dedup
/// catches the duplicate) rather than to suppress a turn that was never
/// actually interpreted.
pub fn processed_ids_in_window(
    rows: &[ProcessedSource],
    now_ms: i64,
    window_ms: Option<i64>,
) -> HashSet<String> {
    let cutoff = window_ms.map(|w| now_ms.saturating_sub(w));
    rows.iter()
        .filter(|row| match cutoff {
            None => true,
            Some(cutoff) => row.ran_at_ms.is_some_and(|ran_at| ran_at >= cutoff),
        })
        .map(|row| row.id.clone())
        .collect()
}

/// Load turn IDs this processor has consumed: [`load_processed_sources`]
/// filtered by [`processed_ids_in_window`]. Empty when the class has never
/// been written.
pub async fn load_processed_source_ids(
    perspective: &PerspectiveInstance,
    processor_id: &str,
    now_ms: i64,
    window_ms: Option<i64>,
) -> anyhow::Result<HashSet<String>> {
    let rows = load_processed_sources(perspective, processor_id).await?;
    Ok(processed_ids_in_window(&rows, now_ms, window_ms))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::{
        ensure_interpretation_overlay_classes, mint_interpretation_run, InterpretationRunCursor,
        InterpretationRunMeta,
    };
    use crate::perspectives::interpretation_test_support::{
        setup_perspective_no_llm, MESSAGE_SDNA,
    };

    #[test]
    fn turn_in_source_window_keeps_recent_drops_old() {
        let now = 1_700_000_000_000i64;
        let window_ms = 7 * 24 * 60 * 60 * 1000;
        let recent = chrono::DateTime::from_timestamp_millis(now - 1_000)
            .unwrap()
            .to_rfc3339();
        let old = chrono::DateTime::from_timestamp_millis(now - window_ms - 1)
            .unwrap()
            .to_rfc3339();
        assert!(turn_in_source_window(&recent, now, window_ms));
        assert!(!turn_in_source_window(&old, now, window_ms));
        assert!(
            turn_in_source_window("not-rfc3339", now, 1),
            "unparseable timestamps must not be dropped"
        );
    }

    #[tokio::test]
    async fn load_processed_source_ids_scopes_to_processor_and_ran_at_window() {
        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");

        let in_window = InterpretationRunMeta {
            run_id: "run-in".into(),
            model: "m".into(),
            prompt_version: "p".into(),
            ran_at: "1000".into(),
            debug_prompt: None,
            debug_response: None,
        };
        mint_interpretation_run(
            &mut p,
            &in_window,
            Some(&InterpretationRunCursor {
                processor: processor_node("proc-a"),
                sources: vec!["aaa111".into(), "bbb222".into()],
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint in-window");

        let other_proc = InterpretationRunMeta {
            run_id: "run-other".into(),
            model: "m".into(),
            prompt_version: "p".into(),
            ran_at: "1000".into(),
            debug_prompt: None,
            debug_response: None,
        };
        mint_interpretation_run(
            &mut p,
            &other_proc,
            Some(&InterpretationRunCursor {
                processor: processor_node("proc-b"),
                sources: vec!["ccc333".into()],
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint other processor");

        let expired = InterpretationRunMeta {
            run_id: "run-old".into(),
            model: "m".into(),
            prompt_version: "p".into(),
            ran_at: "1".into(),
            debug_prompt: None,
            debug_response: None,
        };
        mint_interpretation_run(
            &mut p,
            &expired,
            Some(&InterpretationRunCursor {
                processor: processor_node("proc-a"),
                sources: vec!["old999".into()],
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint expired");

        // now=1500, window=600 → cutoff 900. ran_at=1000 in; ran_at=1 out.
        let ids = load_processed_source_ids(&p, "proc-a", 1_500, Some(600))
            .await
            .expect("load");
        assert!(ids.contains("aaa111"), "got {ids:?}");
        assert!(ids.contains("bbb222"), "got {ids:?}");
        assert!(
            !ids.contains("ccc333"),
            "other processor's sources must not leak; got {ids:?}"
        );
        assert!(
            !ids.contains("old999"),
            "out-of-window ran_at must be dropped; got {ids:?}"
        );

        let unbounded = load_processed_source_ids(&p, "proc-a", 1_500, None)
            .await
            .expect("load unbounded");
        assert!(
            unbounded.contains("old999"),
            "omitted window must keep every ran_at; got {unbounded:?}"
        );
        assert!(
            !unbounded.contains("ccc333"),
            "other processor's sources must not leak even unbounded; got {unbounded:?}"
        );
    }

    /// `ran_at` is `min_count: 0`, so a run can legitimately lack it. Such a
    /// run still suppresses its sources on an unbounded cursor; a windowed
    /// cursor cannot place it in time and drops it, preferring a re-process
    /// (output dedup catches it) over silently retiring an uninterpreted turn.
    #[tokio::test]
    async fn run_without_ran_at_counts_only_when_unbounded() {
        use crate::types::{Link, LinkStatus};
        use ad4m_client::literal::Literal;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");
        let run_uri = "ad4m://interp/run/no-ran-at";
        let source_target = Literal::from_string("ddd444".to_string())
            .to_url()
            .expect("literal url");
        // The run conforms by `run_id` (identity) + `processor` + `sources` —
        // `InterpretationRun` carries no `ad4m://type` discriminator.
        let run_id_literal = Literal::from_string("no-ran-at".to_string())
            .to_url()
            .expect("literal url");
        for (predicate, target) in [
            ("ad4m://interp/run_id", run_id_literal),
            ("ad4m://interp/processor", processor_node("proc-a")),
            ("ad4m://interp/sources", source_target),
        ] {
            p.add_link(
                Link {
                    source: run_uri.into(),
                    predicate: Some(predicate.into()),
                    target,
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .expect("seed run link");
        }

        let unbounded = load_processed_source_ids(&p, "proc-a", 1_500, None)
            .await
            .expect("load unbounded");
        assert!(
            unbounded.contains("ddd444"),
            "a run without ran_at must still suppress when unbounded; got {unbounded:?}"
        );

        let windowed = load_processed_source_ids(&p, "proc-a", 1_500, Some(600))
            .await
            .expect("load windowed");
        assert!(
            !windowed.contains("ddd444"),
            "an unplaceable run must not suppress inside a window; got {windowed:?}"
        );
    }

    #[tokio::test]
    async fn one_shot_run_without_processor_is_invisible_to_cursor() {
        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");
        let meta = InterpretationRunMeta {
            run_id: "run-oneshot".into(),
            model: "m".into(),
            prompt_version: "p".into(),
            ran_at: "1000".into(),
            debug_prompt: None,
            debug_response: None,
        };
        mint_interpretation_run(&mut p, &meta, None, None, &ctx)
            .await
            .expect("mint one-shot");
        let ids = load_processed_source_ids(&p, "proc-a", 1_500, Some(600))
            .await
            .expect("load");
        assert!(
            ids.is_empty(),
            "a run with no processor link must not suppress auto; got {ids:?}"
        );
    }

    #[tokio::test]
    async fn tick_skips_turns_already_in_windowed_cursor() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::{PendingTurn, WatcherState};
        use crate::perspectives::interpretation::gather_transcript_model;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(
            &mut p,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let cfg = AutoProcessorConfig {
            processor_id: "cursor-skip".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let turns = gather_transcript_model(&p, "ns://Message", "body", "{}")
            .await
            .expect("gather")
            .turns;
        assert_eq!(turns.len(), 1);
        let pending = PendingTurn::from_transcript(&turns[0]);
        let now_ms = chrono::Utc::now().timestamp_millis();
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");
        mint_interpretation_run(
            &mut p,
            &InterpretationRunMeta {
                run_id: "prior-win".into(),
                model: "m".into(),
                prompt_version: "p".into(),
                ran_at: now_ms.to_string(),
                debug_prompt: None,
                debug_response: None,
            },
            Some(&InterpretationRunCursor {
                processor: processor_node("cursor-skip"),
                sources: vec![pending.id.clone()],
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint prior sources");

        let mut watcher = WatcherState::new();
        let had_pending = p
            .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
            .await
            .polls_at_base_rate();
        assert_eq!(
            watcher
                .pending_for("cursor-skip")
                .map(|e| e.items.len())
                .unwrap_or(0),
            0,
            "in-window sources must suppress re-enqueue after a RAM-empty restart"
        );
        assert!(
            !had_pending,
            "a tick whose only turn is already processed is idle, so the loop may back off (#1072)"
        );
    }

    /// The tick's return value drives the watch loop's idle back-off (#1072):
    /// no processor declared → idle; a new unprocessed turn still inside its
    /// debounce window → pending, so the loop stays at the base rate.
    #[tokio::test]
    async fn tick_reports_pending_only_while_a_new_turn_waits() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(
            &mut p,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let now_ms = chrono::Utc::now().timestamp_millis();
        let mut watcher = WatcherState::new();
        assert!(
            !p.run_auto_processor_tick(&mut watcher, now_ms, &ctx)
                .await
                .polls_at_base_rate(),
            "no processor declared: nothing can be pending"
        );

        let cfg = AutoProcessorConfig {
            processor_id: "idle-backoff".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            // Longer than the test, so the batch never drains and no pass runs.
            debounce_ms: 600_000,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms, &ctx)
                .await
                .polls_at_base_rate(),
            "an unprocessed turn waiting out its debounce keeps the loop at the base rate"
        );
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
                .await
                .polls_at_base_rate(),
            "still pending on the next tick"
        );
    }

    /// A drained batch whose pass does not retire it (here `ShapesMissing`: no
    /// class shape is registered, so no LLM is reached) is retried, so the
    /// loop must stay at the base rate on the draining tick and on the tick
    /// that re-records it (#1072).
    #[tokio::test]
    async fn tick_stays_pending_across_a_drained_retry_batch() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(
            &mut p,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let cfg = AutoProcessorConfig {
            processor_id: "retry-pending".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let now_ms = chrono::Utc::now().timestamp_millis();
        let mut watcher = WatcherState::new();
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms, &ctx)
                .await
                .polls_at_base_rate(),
            "recording tick"
        );
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms + 51, &ctx)
                .await
                .polls_at_base_rate(),
            "the draining tick of a batch that will be retried must not report idle"
        );
        assert!(
            watcher
                .pending_for("retry-pending")
                .is_none_or(|e| e.items.is_empty()),
            "precondition: the batch was drained"
        );
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms + 102, &ctx)
                .await
                .polls_at_base_rate(),
            "the unretired turn is re-recorded on the next tick"
        );
    }

    /// A queue left behind by a processor that is no longer declared must not
    /// pin the loop at the base rate (#1072): a declared processor with
    /// nothing to do reports idle even while a stale queue exists.
    #[tokio::test]
    async fn tick_ignores_the_queue_of_an_undeclared_processor() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::{PendingTurn, WatcherState};

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let cfg = AutoProcessorConfig {
            processor_id: "declared".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let now_ms = chrono::Utc::now().timestamp_millis();
        let mut watcher = WatcherState::new();
        watcher.record_item(
            "removed",
            PendingTurn {
                id: "stale".into(),
                speaker: "did:key:alice".into(),
                text: "left behind".into(),
                timestamp: "1".into(),
            },
            now_ms,
        );
        assert!(
            !p.run_auto_processor_tick(&mut watcher, now_ms, &ctx)
                .await
                .polls_at_base_rate(),
            "a removed processor's queue must not keep the loop at the base rate"
        );
    }

    /// The cursor's headline promise, end to end: a pass over a *grown*
    /// transcript enqueues only what is new. `m1`/`m2` are already on a prior
    /// run's `sources`, so a tick that re-gathers all three turns queues `m3`
    /// alone rather than re-interpreting the conversation so far.
    #[tokio::test]
    async fn tick_enqueues_only_turns_added_since_the_last_run() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::{PendingTurn, WatcherState};
        use crate::perspectives::interpretation::gather_transcript_model;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        for (uri, body) in [("msg://1", "first"), ("msg://2", "second")] {
            seed_message(&mut p, &ctx, uri, "did:key:alice", body, "ns://body").await;
        }
        let cfg = AutoProcessorConfig {
            processor_id: "incremental".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        // The first pass consumed exactly the two turns present at the time.
        let first_two = gather_transcript_model(&p, "ns://Message", "body", "{}")
            .await
            .expect("gather")
            .turns;
        assert_eq!(first_two.len(), 2);
        let now_ms = chrono::Utc::now().timestamp_millis();
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");
        mint_interpretation_run(
            &mut p,
            &InterpretationRunMeta {
                run_id: "run-first".into(),
                model: "m".into(),
                prompt_version: "p".into(),
                ran_at: now_ms.to_string(),
                debug_prompt: None,
                debug_response: None,
            },
            Some(&InterpretationRunCursor {
                processor: processor_node("incremental"),
                sources: first_two
                    .iter()
                    .map(|t| PendingTurn::from_transcript(t).id)
                    .collect(),
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint first run");

        seed_message(
            &mut p,
            &ctx,
            "msg://3",
            "did:key:alice",
            "third",
            "ns://body",
        )
        .await;

        let mut watcher = WatcherState::new();
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        let pending = watcher.pending_for("incremental").expect("pending exists");
        assert_eq!(
            pending.items.len(),
            1,
            "only the turn added since the last run may enqueue; got {:?}",
            pending.items
        );
        assert_eq!(pending.items[0].text, "third");
    }

    /// `turn_id` hashes the timestamp, so the same words said twice are two
    /// turns and both get interpreted. The seeds are deliberately spaced:
    /// link timestamps are millisecond-precision, so two identical bodies
    /// authored inside one millisecond genuinely do hash to a single turn.
    #[tokio::test]
    async fn tick_enqueues_repeated_text_said_at_different_times() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(&mut p, &ctx, "msg://1", "did:key:alice", "yes", "ns://body").await;
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        seed_message(&mut p, &ctx, "msg://2", "did:key:alice", "yes", "ns://body").await;

        let cfg = AutoProcessorConfig {
            processor_id: "repeated-text".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let mut watcher = WatcherState::new();
        let now_ms = chrono::Utc::now().timestamp_millis();
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        let pending = watcher
            .pending_for("repeated-text")
            .expect("pending exists");
        assert_eq!(
            pending.items.len(),
            2,
            "the same text at two times is two turns; got {:?}",
            pending.items
        );
    }

    /// `batch_max` bounds what a *pass* sees, not merely what the queue
    /// releases: the ids on `BatchReady` are the transcript `run_one_pass`
    /// interprets (the batch payload is carried, never re-gathered), and the
    /// overflow stays queued for the following pass.
    #[tokio::test]
    async fn tick_caps_the_batch_handed_to_a_pass_at_batch_max() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::events::{
            next_event_matching, subscribe, AutoProcessorStep,
        };
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        for (uri, body) in [
            ("msg://1", "one"),
            ("msg://2", "two"),
            ("msg://3", "three"),
            ("msg://4", "four"),
            ("msg://5", "five"),
        ] {
            seed_message(&mut p, &ctx, uri, "did:key:alice", body, "ns://body").await;
        }
        let cfg = AutoProcessorConfig {
            processor_id: "capped".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 2,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let uuid = p.uuid.clone();
        let mut rx = subscribe().await;
        let mut watcher = WatcherState::new();
        // Tick 1 records all five; tick 2 is past the debounce so a batch
        // drains. No class shape is registered, so the pass stops at
        // `ShapesMissing` without reaching an LLM.
        let now_ms = chrono::Utc::now().timestamp_millis();
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        p.run_auto_processor_tick(&mut watcher, now_ms + 51, &ctx)
            .await;

        let ready = next_event_matching(&mut rx, std::time::Duration::from_secs(5), |e| {
            e.perspective_uuid == uuid
                && e.processor_id == "capped"
                && e.step == AutoProcessorStep::BatchReady
        })
        .await
        .expect("BatchReady signal");
        assert_eq!(
            ready.agent_did.as_deref(),
            Some(crate::agent::did_for_context(&ctx).unwrap().as_str()),
            "BatchReady names the acting agent, as every later step does"
        );
        assert_eq!(
            ready.item_ids.len(),
            2,
            "the pass transcript must be capped at batch_max; got {:?}",
            ready.item_ids
        );
        assert_eq!(
            watcher
                .pending_for("capped")
                .map(|e| e.items.len())
                .unwrap_or(0),
            3,
            "the overflow stays queued for the next pass"
        );
    }

    /// `BatchReady` names the agent whose tick it is, not the main agent: a
    /// managed user's pass has to reach that user's own (non-admin) session,
    /// which drops an event with no `agentDid` or someone else's.
    #[tokio::test]
    async fn batch_ready_names_a_managed_user_running_the_tick() {
        use crate::agent::{did_for_context, AgentContext};
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::events::{
            next_event_matching, subscribe, AutoProcessorStep,
        };
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, main_ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let email = "batch-ready-user@test";
        let user_ctx = AgentContext::managed(crate::test_utils::create_test_user(email));
        let user_did = did_for_context(&user_ctx).expect("user did");
        assert_ne!(
            Some(user_did.as_str()),
            did_for_context(&main_ctx).ok().as_deref(),
            "the test needs a DID distinct from the main agent's"
        );

        seed_message(
            &mut p,
            &main_ctx,
            "msg://u1",
            "did:key:alice",
            "one",
            "ns://body",
        )
        .await;
        let cfg = AutoProcessorConfig {
            processor_id: "as-user".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 2,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &main_ctx)
            .await
            .expect("write_processor");

        let uuid = p.uuid.clone();
        let mut rx = subscribe().await;
        let mut watcher = WatcherState::new();
        let now_ms = chrono::Utc::now().timestamp_millis();
        p.run_auto_processor_tick(&mut watcher, now_ms, &user_ctx)
            .await;
        p.run_auto_processor_tick(&mut watcher, now_ms + 51, &user_ctx)
            .await;

        let ready = next_event_matching(&mut rx, std::time::Duration::from_secs(5), |e| {
            e.perspective_uuid == uuid
                && e.processor_id == "as-user"
                && e.step == AutoProcessorStep::BatchReady
        })
        .await
        .expect("BatchReady signal");
        assert_eq!(ready.agent_did.as_deref(), Some(user_did.as_str()));
    }

    #[tokio::test]
    async fn tick_drops_turns_older_than_source_window() {
        use crate::perspectives::auto_processor::config::{
            load_processors, write_processor, AutoProcessorConfig,
        };
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(
            &mut p,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let cfg = AutoProcessorConfig {
            processor_id: "window-drop".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            source_window_ms: Some(1), // 1ms — anything not stamped this millisecond is old
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(
            loaded[0].source_window_ms,
            Some(1),
            "explicit window must hydrate (model_query properties list)"
        );

        let mut watcher = WatcherState::new();
        // now far after the seed timestamp → window drops the turn.
        let now_ms = chrono::Utc::now().timestamp_millis() + 10_000;
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        assert_eq!(
            watcher
                .pending_for("window-drop")
                .map(|e| e.items.len())
                .unwrap_or(0),
            0,
            "turns older than source_window_ms must not be enqueued"
        );
    }

    #[tokio::test]
    async fn tick_omitted_window_does_not_drop_old_turns() {
        use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
        use crate::perspectives::auto_processor::watcher::WatcherState;

        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        seed_message(
            &mut p,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let cfg = AutoProcessorConfig {
            processor_id: "no-window".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let mut watcher = WatcherState::new();
        let now_ms = chrono::Utc::now().timestamp_millis() + 10_000;
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        assert_eq!(
            watcher
                .pending_for("no-window")
                .map(|e| e.items.len())
                .unwrap_or(0),
            1,
            "omitting source_window_ms must enqueue even old turns"
        );
    }

    /// The cursor is what retires a turn: an unverifiable `InterpretationRun`
    /// could mark every source id as processed and starve the processor. A
    /// raw SPARQL read cannot tell a forged run from a signed one, so the
    /// cursor reads the `InterpretationRun` class through the model query,
    /// under the same proof guard the transcript gets.
    #[tokio::test]
    async fn an_unverifiable_run_marks_nothing_processed() {
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");

        // A signed run retires its sources (the verified half).
        mint_interpretation_run(
            &mut p,
            &InterpretationRunMeta {
                run_id: "run-real".into(),
                model: "m".into(),
                prompt_version: "p".into(),
                ran_at: "1000".into(),
                debug_prompt: None,
                debug_response: None,
            },
            Some(&InterpretationRunCursor {
                processor: processor_node("proc-a"),
                sources: vec!["real111".into()],
            }),
            None,
            &ctx,
        )
        .await
        .expect("mint verified run");

        // The forgery carries the right triples but proofs that do not cover
        // them — the shape a gossiped run arrives in (#1113). The last link
        // sits on the *verified* run itself: its identity and processor
        // links are real, so only the per-source proof check keeps the forged
        // `sources` out of the cursor.
        let attacker = TestSigner::generate();
        for (signed_source, stored_source, predicate, target) in [
            (
                "ad4m://interp/run/forged",
                "ad4m://interp/run/tampered",
                "ad4m://interp/run_id",
                "literal:string:forged",
            ),
            (
                "ad4m://interp/run/forged",
                "ad4m://interp/run/tampered",
                "ad4m://interp/processor",
                "ad4m://autoprocessor/proc-a",
            ),
            (
                "ad4m://interp/run/forged",
                "ad4m://interp/run/tampered",
                "ad4m://interp/sources",
                "literal:string:fake222",
            ),
            (
                "ad4m://interp/run/forged",
                "ad4m://interp/run/run-real",
                "ad4m://interp/sources",
                "literal:string:fake333",
            ),
        ] {
            let mut l = LinkExpression::from(attacker.sign(Link {
                source: signed_source.into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            }));
            l.data.source = stored_source.into();
            l.status = Some(LinkStatus::Shared);
            assert!(
                !l.compute_proof_valid(),
                "fixture check: the proof must not cover the tampered link"
            );
            p.sparql_store.add_link(&l).expect("seed forged link");
        }

        let ids = load_processed_source_ids(&p, "proc-a", 1_500, None)
            .await
            .expect("load_processed_source_ids");
        assert!(
            ids.contains("real111"),
            "the verified run still retires its sources; got {ids:?}"
        );
        assert!(
            !ids.contains("fake333"),
            "an unverifiable `sources` link on a valid run retires nothing; got {ids:?}"
        );
        assert!(
            !ids.contains("fake222"),
            "an unverifiable run must not mark its sources processed; got {ids:?}"
        );
    }
}
