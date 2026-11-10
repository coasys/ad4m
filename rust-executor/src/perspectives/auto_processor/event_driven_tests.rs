//! The watch loop is woken by link writes, not a poll timer: with no store
//! change a tick runs no store query (configs, transcript and cursor come from
//! the previous read), and a new turn still wakes it and is queued once.

use super::config::{processor_node, write_processor, AutoProcessorConfig};
use super::events::{next_event_matching, subscribe, AutoProcessorStep};
use super::watcher::{PendingTurn, WatcherState};
use crate::perspectives::interpretation::{
    ensure_interpretation_overlay_classes, gather_transcript_sparql, mint_interpretation_run,
    InterpretationRunCursor, InterpretationRunMeta, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
};
use crate::perspectives::interpretation_test_support::{seed_message, setup_perspective_no_llm};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::time::Duration;

/// How many times this perspective's store ran the transcript scope query.
fn transcript_queries(p: &PerspectiveInstance) -> usize {
    p.sparql_store
        .queries_run()
        .iter()
        .filter(|q| q.as_str() == BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
        .count()
}

fn all_queries(p: &PerspectiveInstance) -> usize {
    p.sparql_store.queries_run().len()
}

fn config(
    processor_id: &str,
    debounce_ms: i64,
    source_window_ms: Option<i64>,
) -> AutoProcessorConfig {
    AutoProcessorConfig {
        processor_id: processor_id.into(),
        source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms,
        batch_min: 1,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        source_window_ms,
        ..Default::default()
    }
}

/// Retire `ids` for `processor_id` the way a won pass does: a run whose
/// `sources` name them, stamped `ran_at`.
async fn mint_run(
    p: &mut PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    run_id: &str,
    processor_id: &str,
    ids: Vec<String>,
    ran_at: i64,
) {
    ensure_interpretation_overlay_classes(p, ctx)
        .await
        .expect("overlay classes");
    mint_interpretation_run(
        p,
        &InterpretationRunMeta {
            run_id: run_id.into(),
            model: "m".into(),
            prompt_version: "p".into(),
            ran_at: ran_at.to_string(),
            debug_prompt: None,
            debug_response: None,
        },
        Some(&InterpretationRunCursor {
            processor: processor_node(processor_id),
            sources: ids,
        }),
        None,
        ctx,
    )
    .await
    .expect("mint run");
}

async fn turn_ids(p: &PerspectiveInstance) -> Vec<PendingTurn> {
    gather_transcript_sparql(p, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
        .await
        .expect("gather")
        .iter()
        .map(PendingTurn::from_transcript)
        .collect()
}

fn pending_texts(watcher: &WatcherState, processor_id: &str) -> Vec<String> {
    watcher
        .pending_for(processor_id)
        .map(|e| e.items.iter().map(|t| t.text.clone()).collect())
        .unwrap_or_default()
}

/// The staging symptom: most perspectives declare no processor, yet every tick
/// asked the store whether the class is registered. After the first tick, a
/// perspective whose links did not change is not queried again.
#[tokio::test]
async fn a_tick_without_link_changes_queries_nothing_when_no_processor_is_declared() {
    let (p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();

    assert!(!p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await);
    let after_first = all_queries(&p);
    for i in 1..=5 {
        assert!(
            !p.run_auto_processor_tick(&mut watcher, now_ms + i * 60_000, &ctx)
                .await
        );
    }
    assert_eq!(
        all_queries(&p),
        after_first,
        "no link changed, so no tick after the first may query the store"
    );
}

/// With a processor declared and a turn waiting out its debounce (the loop's
/// base-rate case), unchanged links mean no transcript, cursor or config
/// query. A new turn is read on the next tick, and queued once.
#[tokio::test]
async fn a_tick_rereads_the_transcript_only_after_a_link_change() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "first",
        "ns://body",
    )
    .await;
    // Longer than the test: the batch never drains, so no pass runs.
    write_processor(&mut p, &config("gated", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");

    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    assert!(p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await);
    assert_eq!(
        transcript_queries(&p),
        1,
        "the first tick reads the transcript"
    );
    let after_first = all_queries(&p);

    for i in 1..=10 {
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms + i * 500, &ctx)
                .await,
            "the turn is still waiting"
        );
    }
    assert_eq!(
        all_queries(&p),
        after_first,
        "ten ticks without a link change ran store queries"
    );
    assert_eq!(pending_texts(&watcher, "gated"), vec!["first"]);

    seed_message(
        &mut p,
        &ctx,
        "msg://2",
        "did:key:alice",
        "second",
        "ns://body",
    )
    .await;
    p.run_auto_processor_tick(&mut watcher, now_ms + 6_000, &ctx)
        .await;
    assert_eq!(transcript_queries(&p), 2, "a link change reads it again");
    p.run_auto_processor_tick(&mut watcher, now_ms + 6_500, &ctx)
        .await;
    assert_eq!(transcript_queries(&p), 2, "once");
    assert_eq!(
        pending_texts(&watcher, "gated"),
        vec!["first", "second"],
        "each turn is queued once"
    );
}

/// Reusing the last read must keep what a fresh read gives: only turns no
/// run has consumed are queued, and both windows (turn timestamp and the
/// run's `ran_at`) are applied at the tick's own `now_ms`, so a run that ages
/// out of the window frees its turns without any link changing.
#[tokio::test]
async fn reused_reads_queue_only_new_turns_and_follow_the_window() {
    const WINDOW: i64 = 60_000;
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    seed_message(&mut p, &ctx, "msg://1", "did:key:alice", "old", "ns://body").await;
    write_processor(
        &mut p,
        &config("windowed", 600_000, Some(WINDOW)),
        Some(false),
        &ctx,
    )
    .await
    .expect("write_processor");

    let now_ms = chrono::Utc::now().timestamp_millis();
    let old = turn_ids(&p).await;
    assert_eq!(old.len(), 1);
    // A run that consumed "old", stamped half a window ago.
    mint_run(
        &mut p,
        &ctx,
        "run-old",
        "windowed",
        vec![old[0].id.clone()],
        now_ms - WINDOW / 2,
    )
    .await;

    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert!(
        pending_texts(&watcher, "windowed").is_empty(),
        "a consumed turn is not queued"
    );

    seed_message(&mut p, &ctx, "msg://2", "did:key:alice", "new", "ns://body").await;
    p.run_auto_processor_tick(&mut watcher, now_ms + 1, &ctx)
        .await;
    assert_eq!(
        pending_texts(&watcher, "windowed"),
        vec!["new"],
        "only the turn added since the run is queued"
    );
    let reads = all_queries(&p);

    // No link changes from here on. Past `ran_at + WINDOW` the run no longer
    // counts, while "old" (stamped now) is still inside the window: it is
    // queued again, as a fresh read would queue it.
    p.run_auto_processor_tick(&mut watcher, now_ms + WINDOW / 2 + 1_000, &ctx)
        .await;
    assert_eq!(pending_texts(&watcher, "windowed"), vec!["new", "old"]);
    assert_eq!(all_queries(&p), reads, "and without reading the store");

    // A run that consumed both turns, and a watcher that starts after it:
    // nothing to queue. Past `ran_at + WINDOW` the run no longer counts, but
    // both turns are older than the window by then, so the reused read
    // queues neither.
    let both: Vec<String> = turn_ids(&p).await.into_iter().map(|t| t.id).collect();
    assert_eq!(both.len(), 2);
    // `turn_ids` runs the scope query too; count the ticks' reads from here.
    let transcript_reads = transcript_queries(&p);
    mint_run(&mut p, &ctx, "run-both", "windowed", both, now_ms + 5_000).await;
    let mut late = WatcherState::new();
    p.run_auto_processor_tick(&mut late, now_ms + 5_000, &ctx)
        .await;
    assert!(pending_texts(&late, "windowed").is_empty());
    let late_reads = all_queries(&p);
    p.run_auto_processor_tick(&mut late, now_ms + 5_000 + WINDOW + 1, &ctx)
        .await;
    assert!(
        pending_texts(&late, "windowed").is_empty(),
        "turns older than the window are not queued from a reused read"
    );
    assert_eq!(
        all_queries(&p),
        late_reads,
        "the late watcher reads once, then reuses it"
    );

    assert_eq!(
        transcript_queries(&p),
        transcript_reads + 1,
        "only the late watcher's first tick (after the new run) read the transcript again"
    );
}

/// The loop itself: after its first pass it sleeps while no link changes
/// (the poll it replaced re-read the transcript at 0.5 s, 1.5 s, 3.5 s, …),
/// wakes for a new turn, queues it once, and ends promptly on teardown.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn the_watch_loop_sleeps_until_a_link_changes() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "first",
        "ns://body",
    )
    .await;
    write_processor(&mut p, &config("looped", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let first = turn_ids(&p).await;
    let now_ms = chrono::Utc::now().timestamp_millis();
    mint_run(
        &mut p,
        &ctx,
        "run-1",
        "looped",
        vec![first[0].id.clone()],
        now_ms,
    )
    .await;
    let before = transcript_queries(&p);

    let uuid = p.uuid.clone();
    let mut rx = subscribe().await;
    let looping = p.clone();
    let loop_ctx = ctx.clone();
    let handle = tokio::spawn(async move { looping.auto_processor_watch_loop(loop_ctx).await });

    // The first pass reads the transcript once.
    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while transcript_queries(&p) == before {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the loop's first pass never read the transcript"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    tokio::time::sleep(Duration::from_secs(3)).await;
    assert_eq!(
        transcript_queries(&p),
        before + 1,
        "no link changed, yet the idle loop read the transcript again"
    );

    seed_message(
        &mut p,
        &ctx,
        "msg://2",
        "did:key:alice",
        "second",
        "ns://body",
    )
    .await;
    let second = turn_ids(&p)
        .await
        .into_iter()
        .find(|t| t.text == "second")
        .expect("second turn");
    let ready = next_event_matching(&mut rx, Duration::from_secs(3), |e| {
        e.perspective_uuid == uuid
            && e.processor_id == "looped"
            && e.step == AutoProcessorStep::BatchReady
    })
    .await
    .expect("the new turn woke the loop within 3 s");
    assert_eq!(
        ready.item_ids,
        vec![second.id.clone()],
        "the batch is the new turn alone"
    );

    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}
