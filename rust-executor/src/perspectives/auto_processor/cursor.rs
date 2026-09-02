//! Processed-turn cursor: gather-side optional time filter + SPARQL over
//! [`InterpretationRun`](crate::perspectives::interpretation) `sources`.
//!
//! The watch tick subtracts in Rust: scope query, drop turns older than
//! `source_window_ms` when set, load this processor's source IDs (optionally
//! bounded by `ran_at`), skip those. Do not wrap the user's SPARQL with
//! `FILTER NOT EXISTS`.

use super::config::processor_node;
use crate::perspectives::interpretation::decode_literal_string;
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

/// Engine-owned cursor query: source turn IDs recorded by one AutoProcessor.
///
/// `ran_at` is `OPTIONAL` because it only matters when a window is set — an
/// unbounded cursor must not lose a run's sources just because that run carries
/// no timestamp (`ran_at` is `min_count: 0` on the class). It is filtered in
/// Rust rather than SPARQL: the value is a `literal:string:` IRI, so an
/// `xsd:integer` FILTER does not apply to it.
fn processed_ids_query(processor_id: &str) -> String {
    let node = processor_node(processor_id);
    // No `ad4m://type` filter — `InterpretationRun` dropped its type
    // discriminator (Nico 2026-08-19: type flags are an anti-pattern).
    // We require the identity property `interp/run_id` so a bare node with
    // only `interp/processor` + `interp/sources` cannot masquerade as a run
    // and silently suppress source IDs (CodeRabbit #881, 2026-08-19).
    format!(
        r#"SELECT ?id ?ran_at WHERE {{
  ?run <ad4m://interp/run_id> ?run_id .
  ?run <ad4m://interp/processor> <{node}> .
  ?run <ad4m://interp/sources> ?id .
  OPTIONAL {{ ?run <ad4m://interp/ran_at> ?ran_at }}
}}"#
    )
}

fn decode_binding(raw: &str) -> String {
    if raw.starts_with("literal:") {
        decode_literal_string(raw).unwrap_or_else(|| raw.to_string())
    } else {
        raw.to_string()
    }
}

/// Load turn IDs this processor has consumed. When `window_ms` is `Some`, only
/// runs whose `ran_at` is inside that window count; `None` is unbounded and
/// ignores `ran_at` entirely. Empty when the class has never been written.
///
/// In the windowed case a row whose `ran_at` is absent or unparseable is
/// **dropped**, i.e. treated as not-yet-processed. The cursor is a suppression
/// list, so the conservative direction is to re-process (output identity-dedup
/// catches the duplicate) rather than to suppress a turn that was never
/// actually interpreted.
pub async fn load_processed_source_ids(
    perspective: &PerspectiveInstance,
    processor_id: &str,
    now_ms: i64,
    window_ms: Option<i64>,
) -> anyhow::Result<HashSet<String>> {
    let cutoff = window_ms.map(|w| now_ms.saturating_sub(w));
    let rows_json = perspective
        .sparql_query(processed_ids_query(processor_id))
        .map_err(|e| anyhow::anyhow!("load_processed_source_ids: SPARQL failed: {e:#}"))?;
    let rows: Vec<serde_json::Value> = serde_json::from_str(&rows_json)
        .map_err(|e| anyhow::anyhow!("load_processed_source_ids: bad SPARQL JSON: {e:#}"))?;
    let mut out = HashSet::new();
    for row in rows {
        let Some(raw_id) = row.get("id").and_then(|v| v.as_str()) else {
            continue;
        };
        if let Some(cutoff) = cutoff {
            let ran_at = row
                .get("ran_at")
                .and_then(|v| v.as_str())
                .map(decode_binding)
                .and_then(|s| s.parse::<i64>().ok());
            match ran_at {
                Some(ran_at_ms) if ran_at_ms >= cutoff => {}
                _ => continue,
            }
        }
        out.insert(decode_binding(raw_id));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::{
        ensure_interpretation_overlay_classes, mint_interpretation_run, InterpretationRunCursor,
        InterpretationRunMeta,
    };
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

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
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let run_uri = "ad4m://interp/run/no-ran-at";
        let source_target = Literal::from_string("ddd444".to_string())
            .to_url()
            .expect("literal url");
        // No `ad4m://type` seed — `InterpretationRun` dropped its type
        // discriminator; the cursor SPARQL matches on `interp/run_id`
        // (identity) + `interp/processor` + `interp/sources`.
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
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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
        use crate::perspectives::interpretation::{
            gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
        };
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let turns = gather_transcript_sparql(&p, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
            .await
            .expect("gather");
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
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        assert_eq!(
            watcher
                .pending_for("cursor-skip")
                .map(|e| e.items.len())
                .unwrap_or(0),
            0,
            "in-window sources must suppress re-enqueue after a RAM-empty restart"
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
        use crate::perspectives::interpretation::{
            gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
        };
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        for (uri, body) in [("msg://1", "first"), ("msg://2", "second")] {
            seed_message(&mut p, &ctx, uri, "did:key:alice", body, "ns://body").await;
        }
        let cfg = AutoProcessorConfig {
            processor_id: "incremental".into(),
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        // The first pass consumed exactly the two turns present at the time.
        let first_two = gather_transcript_sparql(&p, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
            .await
            .expect("gather");
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
        use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        seed_message(&mut p, &ctx, "msg://1", "did:key:alice", "yes", "ns://body").await;
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        seed_message(&mut p, &ctx, "msg://2", "did:key:alice", "yes", "ns://body").await;

        let cfg = AutoProcessorConfig {
            processor_id: "repeated-text".into(),
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
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
        use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 2,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
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

    #[tokio::test]
    async fn tick_drops_turns_older_than_source_window() {
        use crate::perspectives::auto_processor::config::{
            load_processors, write_processor, AutoProcessorConfig,
        };
        use crate::perspectives::auto_processor::watcher::WatcherState;
        use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: Some(1), // 1ms — anything not stamped this millisecond is old
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
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
        use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
        use crate::perspectives::interpretation_test_support::seed_message;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
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
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 50,
            batch_min: 1,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
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
}
