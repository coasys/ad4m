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

/// How many times this perspective's store read a transcript: the scope
/// query, or a read of what it gained from added links (`delta_read`).
fn transcript_queries(p: &PerspectiveInstance) -> usize {
    p.sparql_store
        .queries_run()
        .iter()
        .filter(|q| is_transcript_read(q))
        .count()
}

/// Every transcript read selects `?speaker`; no config or cursor read does.
fn is_transcript_read(query: &str) -> bool {
    query.contains("?speaker")
}

/// The rows every transcript read of this store returned so far.
fn transcript_rows(p: &PerspectiveInstance) -> usize {
    p.sparql_store
        .query_rows_run()
        .iter()
        .filter(|(q, _)| is_transcript_read(q))
        .map(|(_, rows)| rows)
        .sum()
}

/// Every store read: SPARQL queries and link index scans.
fn all_queries(p: &PerspectiveInstance) -> usize {
    p.sparql_store.queries_run().len() + p.sparql_store.link_scans_run()
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

    assert!(!p
        .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
        .await
        .polls_at_base_rate());
    let after_first = all_queries(&p);
    for i in 1..=5 {
        assert!(!p
            .run_auto_processor_tick(&mut watcher, now_ms + i * 60_000, &ctx)
            .await
            .polls_at_base_rate());
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
    assert!(p
        .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
        .await
        .polls_at_base_rate());
    assert_eq!(
        transcript_queries(&p),
        1,
        "the first tick reads the transcript"
    );
    let after_first = all_queries(&p);

    for i in 1..=10 {
        assert!(
            p.run_auto_processor_tick(&mut watcher, now_ms + i * 500, &ctx)
                .await
                .polls_at_base_rate(),
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
/// out of the window frees its turns without any link changing. Consumed
/// turns are kept without their text, so freeing one reads the transcript
/// once more.
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
    // queued again, as a fresh read would queue it. The kept read holds no
    // text for a consumed turn, so this costs one transcript read.
    let transcript_reads = transcript_queries(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + WINDOW / 2 + 1_000, &ctx)
        .await;
    assert_eq!(pending_texts(&watcher, "windowed"), vec!["new", "old"]);
    assert_eq!(
        transcript_queries(&p),
        transcript_reads + 1,
        "a consumed turn coming back is read again, once"
    );
    assert_eq!(all_queries(&p), reads + 1, "and nothing else is read");
    let reads = all_queries(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + WINDOW / 2 + 1_500, &ctx)
        .await;
    assert_eq!(all_queries(&p), reads, "the next tick reuses that read");

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

/// Writes that add no turn (reactions, other agents' outputs, claims, a peer
/// syncing) wake an idle loop, but they must not make it read more often than
/// the idle back-off it replaced: a write-woken tick waits out the back-off
/// (doubling from the base tick up to 4 s) since the last read that found the
/// store changed. Here an unrelated link lands every 100 ms for 3 s; the
/// reads must be at least the first back-off step (1 s) apart.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn unrelated_writes_do_not_make_an_idle_loop_read_more_often_than_the_back_off() {
    use crate::types::{Link, LinkStatus};

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
    write_processor(&mut p, &config("busy", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let first = turn_ids(&p).await;
    let now_ms = chrono::Utc::now().timestamp_millis();
    mint_run(
        &mut p,
        &ctx,
        "run-1",
        "busy",
        vec![first[0].id.clone()],
        now_ms,
    )
    .await;
    let before = transcript_queries(&p);

    let looping = p.clone();
    let loop_ctx = ctx.clone();
    let handle = tokio::spawn(async move { looping.auto_processor_watch_loop(loop_ctx).await });

    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while transcript_queries(&p) == before {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the loop's first pass never read the transcript"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    let mut reads = vec![tokio::time::Instant::now()];
    let mut seen = transcript_queries(&p);

    let start = tokio::time::Instant::now();
    let mut i = 0;
    while start.elapsed() < Duration::from_secs(3) {
        p.add_link(
            Link {
                source: "note://reactions".into(),
                predicate: Some("ns://reaction".into()),
                target: format!("literal:string:r{i}"),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("unrelated link");
        i += 1;
        tokio::time::sleep(Duration::from_millis(100)).await;
        let now = transcript_queries(&p);
        if now > seen {
            reads.extend(std::iter::repeat_n(tokio::time::Instant::now(), now - seen));
            seen = now;
        }
    }

    let gaps: Vec<u128> = reads
        .windows(2)
        .map(|w| (w[1] - w[0]).as_millis())
        .collect();
    println!(
        "{} transcript reads in ~3 s of unrelated writes, gaps {gaps:?} ms",
        reads.len()
    );
    assert!(
        reads.len() <= 3,
        "{} transcript reads in ~3 s of unrelated writes (gaps {gaps:?} ms); \
         the back-off allows at most 3",
        reads.len()
    );
    assert!(
        gaps.iter().all(|g| *g >= 800),
        "reads closer than the first back-off step: gaps {gaps:?} ms"
    );

    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}

/// The kept transcript holds text only for turns that may still be queued: a
/// consumed turn is kept as its id and timestamp.
#[tokio::test]
async fn a_kept_transcript_holds_no_text_for_consumed_turns() {
    use super::watcher::CachedTurn;

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "done",
        "ns://body",
    )
    .await;
    seed_message(
        &mut p,
        &ctx,
        "msg://2",
        "did:key:alice",
        "open",
        "ns://body",
    )
    .await;
    let cfg = config("lean", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let turns = turn_ids(&p).await;
    let done = turns.iter().find(|t| t.text == "done").expect("done turn");
    let now_ms = chrono::Utc::now().timestamp_millis();
    mint_run(
        &mut p,
        &ctx,
        "run-done",
        "lean",
        vec![done.id.clone()],
        now_ms,
    )
    .await;

    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let (kept_at, kept) = watcher
        .reads
        .transcript(&cfg)
        .expect("the tick kept its read");
    assert_eq!(kept_at, p.sparql_store.write_generation());
    assert_eq!(kept.len(), 2);
    for turn in kept.iter() {
        match turn {
            CachedTurn::Consumed { id, .. } => assert_eq!(id, &done.id),
            CachedTurn::Full(t) => assert_eq!(t.text, "open"),
        }
    }
    assert!(kept
        .iter()
        .any(|t| matches!(t, CachedTurn::Consumed { .. })));
    assert_eq!(pending_texts(&watcher, "lean"), vec!["open"]);
}

/// Hand `watcher` a kept read of `n` pending turns, as if the store had
/// answered them at the current generation, and tick twice over it: the
/// first tick queues them, the second re-records them as the loop does at
/// every tick. Returns the id checks `record_item` made.
async fn id_checks_for_two_ticks_over(n: usize) -> u64 {
    let (p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let cfg = config("many", 600_000, None);
    let generation = p.sparql_store.write_generation();
    let mut watcher = WatcherState::new();
    watcher.reads.set_configs(generation, vec![cfg.clone()]);
    watcher
        .reads
        .set_processed(&cfg.processor_id, generation, Vec::new());
    let turns = (0..n)
        .map(|i| PendingTurn {
            id: format!("{i:016x}"),
            speaker: "did:key:alice".into(),
            text: format!("turn {i}"),
            timestamp: format!("2026-11-17T10:00:00.{i:06}Z"),
        })
        .collect();
    watcher
        .reads
        .set_transcript(&cfg, generation, turns, |_, _| true);

    let now_ms = chrono::Utc::now().timestamp_millis();
    assert!(p
        .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
        .await
        .polls_at_base_rate());
    assert!(p
        .run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await
        .polls_at_base_rate());
    assert_eq!(
        watcher.pending_for("many").map(|e| e.items.len()),
        Some(n),
        "every turn is queued once"
    );
    watcher.id_checks
}

/// A tick records every queueable turn of the kept transcript again, so the
/// cost of recording one must not grow with the queue: over 50k pending
/// turns (a large space with no model) the duplicate check scanned the
/// queue, and a tick made N²/2 id comparisons. Counted, not timed: at most
/// one id check per turn per tick, at 5k and at 50k alike.
#[tokio::test]
async fn a_tick_over_50k_kept_turns_records_them_in_linear_time() {
    for n in [5_000usize, 50_000] {
        let checks = id_checks_for_two_ticks_over(n).await;
        println!("{n} pending turns, two ticks: {checks} id checks");
        assert!(
            checks <= 2 * n as u64,
            "{checks} id checks for two ticks over {n} pending turns; \
             recording must cost one check per turn"
        );
    }
}

/// Josh's profile, part 3: on an executor with no model a processor's turns
/// stay pending forever, and a pending tick polled at the base rate (0.5 s)
/// forever, in every loop of the space. With no model a tick drains nothing,
/// keeps the turns queued, and reports `NoModel`, which backs the loop off to
/// the idle delay. Once a model is there, the next tick drains the batch.
#[tokio::test]
async fn with_no_model_pending_turns_back_off_to_the_idle_delay() {
    use super::watcher::test_hooks::{PassHooks, HOOKS};
    use super::watcher::{
        next_tick_delay_ms, TickOutcome, AUTO_PROCESSOR_IDLE_TICK_MAX_MS, AUTO_PROCESSOR_TICK_MS,
    };

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
    write_processor(&mut p, &config("modelless", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");

    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    let no_model = PassHooks {
        no_model: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true)),
        ..Default::default()
    };
    HOOKS
        .scope(no_model, async {
            let mut floor = AUTO_PROCESSOR_TICK_MS;
            // Past the debounce from the second tick on: a batch is ready.
            for i in 0..6 {
                let outcome = p
                    .run_auto_processor_tick(&mut watcher, now_ms + i * 500, &ctx)
                    .await;
                assert_eq!(outcome, TickOutcome::NoModel, "tick {i}");
                floor = next_tick_delay_ms(floor, outcome.polls_at_base_rate());
            }
            assert_eq!(
                floor, AUTO_PROCESSOR_IDLE_TICK_MAX_MS,
                "with no model the loop backs off to the idle delay"
            );
        })
        .await;
    assert_eq!(
        pending_texts(&watcher, "modelless"),
        vec!["first"],
        "nothing is drained without a model; the turn stays queued"
    );

    // A model is there (the hooks' default): the batch drains at once.
    let outcome = p
        .run_auto_processor_tick(&mut watcher, now_ms + 3_000, &ctx)
        .await;
    assert_eq!(outcome, TickOutcome::Pending);
    assert!(
        pending_texts(&watcher, "modelless").is_empty(),
        "the first tick with a model drains the waiting batch"
    );
}

/// The loop itself, with no model and a turn pending: after the tick that
/// finds no model it waits for a write or the idle fallback, instead of
/// ticking every 0.5 s. Counted by how often a tick asked for a model.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn with_no_model_the_watch_loop_stops_polling() {
    use super::watcher::test_hooks::{PassHooks, HOOKS};
    use std::sync::atomic::Ordering;

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
    write_processor(&mut p, &config("modelless", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");

    let hooks = PassHooks {
        no_model: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true)),
        ..Default::default()
    };
    let checks = hooks.model_checks.clone();
    let looping = p.clone();
    let loop_ctx = ctx.clone();
    let handle = tokio::spawn(HOOKS.scope(hooks, async move {
        looping.auto_processor_watch_loop(loop_ctx).await
    }));

    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while checks.load(Ordering::Relaxed) == 0 {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the loop's first tick never asked for a model"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    tokio::time::sleep(Duration::from_secs(3)).await;
    let ticks = checks.load(Ordering::Relaxed);
    println!("{ticks} ticks in ~3 s with a turn pending and no model");
    assert_eq!(
        ticks, 1,
        "with no model and no write the loop ticked {ticks} times in ~3 s; \
         polling at 0.5 s would be about 6"
    );

    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}

/// A body link signed by `signer` at `at`, as a peer's link arrives.
fn body_link(
    signer: &crate::agent::signatures::TestSigner,
    msg: &str,
    text: &str,
    at: chrono::DateTime<chrono::Utc>,
) -> crate::types::LinkExpression {
    let data = crate::types::Link {
        source: msg.into(),
        predicate: Some("ns://body".into()),
        target: format!("literal:string:{text}"),
    }
    .normalize();
    let signed = signer.sign_at(data, at);
    crate::types::LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(crate::types::LinkStatus::Local),
    }
}

/// The ids of the kept transcript, and of a fresh read of the same query:
/// a read brought up to date from added links must hold the same turns.
async fn kept_and_fresh_ids(
    p: &PerspectiveInstance,
    watcher: &WatcherState,
    cfg: &AutoProcessorConfig,
) -> (Vec<String>, Vec<String>) {
    let (_, kept) = watcher.reads.transcript(cfg).expect("a kept read");
    let mut kept: Vec<String> = kept.iter().map(|t| t.id().to_string()).collect();
    let mut fresh: Vec<String> = gather_transcript_sparql(p, &cfg.source_scope_query)
        .await
        .expect("fresh read")
        .iter()
        .map(|t| PendingTurn::from_transcript(t).id)
        .collect();
    kept.sort();
    kept.dedup();
    fresh.sort();
    fresh.dedup();
    (kept, fresh)
}

/// Josh's profile, part 2: in a busy conversation every write moves the
/// store's generation, so with turns pending each tick read all N turns
/// again. After the first read, a tick reads only what the links added since
/// can add: a new message is one row, an unrelated link none.
#[tokio::test]
async fn a_write_after_the_first_read_reads_only_the_new_turns() {
    const N: usize = 200;
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    for i in 0..N {
        seed_message(
            &mut p,
            &ctx,
            &format!("msg://{i}"),
            "did:key:alice",
            &format!("turn {i}"),
            "ns://body",
        )
        .await;
    }
    let cfg = config("delta", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(
        transcript_rows(&p) - rows,
        N,
        "the first tick reads every turn"
    );

    seed_message(
        &mut p,
        &ctx,
        "msg://new",
        "did:key:alice",
        "new",
        "ns://body",
    )
    .await;
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_rows(&p) - rows,
        1,
        "a tick after a new message reads that message, not all {} turns",
        N + 1
    );
    let pending = pending_texts(&watcher, "delta");
    assert_eq!(pending.len(), N + 1);
    assert_eq!(pending.last().map(String::as_str), Some("new"));

    p.add_link(
        crate::types::Link {
            source: "note://reactions".into(),
            predicate: Some("ns://reaction".into()),
            target: "literal:string:r".into(),
        },
        crate::types::LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("unrelated link");
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 1_000, &ctx)
        .await;
    assert_eq!(
        transcript_rows(&p) - rows,
        0,
        "an unrelated link adds no row"
    );

    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh, "the merged read holds what a full read holds");
}

/// A turn that arrives late, stamped before every turn already read (a peer's
/// link syncing in), is read from what was added, not missed for being old.
#[tokio::test]
async fn a_late_turn_with_an_older_timestamp_is_read_from_the_links_added() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    for i in 0..20 {
        seed_message(
            &mut p,
            &ctx,
            &format!("msg://{i}"),
            "did:key:alice",
            &format!("turn {i}"),
            "ns://body",
        )
        .await;
    }
    let cfg = config("late", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    let signer = crate::agent::signatures::TestSigner::generate();
    let yesterday = chrono::Utc::now() - chrono::Duration::days(1);
    p.sparql_store
        .add_link(&body_link(&signer, "msg://late", "late", yesterday))
        .expect("a peer's link");
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(transcript_rows(&p) - rows, 1, "only the late turn is read");
    assert_eq!(
        pending_texts(&watcher, "late").last().map(String::as_str),
        Some("late"),
        "the late turn is queued"
    );
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
}

/// WE's scope query (a channel's messages, directly or in a thread, flagged
/// as input): an old message enters the scope when its thread link lands
/// after its body. The row is read from the added link, though no new turn
/// link was written.
#[tokio::test]
async fn a_link_that_brings_an_old_message_into_scope_is_read_from_the_links_added() {
    const SCOPE: &str = r#"SELECT ?speaker ?text ?timestamp WHERE {
  { <ns://chan> <ns://child> ?m . }
  UNION
  { <ns://chan> <ns://child> ?line . ?line <ns://thread> ?m . }
  ?m <ns://flag> <ns://text-block> .
  ?m <ns://source> ?source .
  FILTER(?source IN ("input", "typed"))
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp"#;
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let link = |s: &str, pr: &str, t: &str| crate::types::Link {
        source: s.into(),
        predicate: Some(pr.into()),
        target: t.into(),
    };
    let mut links = vec![
        link("ns://chan", "ns://child", "msg://1"),
        link("ns://chan", "ns://child", "line://1"),
    ];
    for m in ["msg://1", "msg://2"] {
        links.push(link(m, "ns://flag", "ns://text-block"));
        links.push(link(m, "ns://source", "literal:string:input"));
        links.push(link(m, "ns://body", &format!("literal:string:{m}")));
    }
    for l in links {
        p.add_link(l, crate::types::LinkStatus::Local, None, &ctx)
            .await
            .expect("link");
    }
    let cfg = AutoProcessorConfig {
        source_scope_query: SCOPE.into(),
        ..config("we", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(pending_texts(&watcher, "we"), vec!["msg://1"]);

    p.add_link(
        link("line://1", "ns://thread", "msg://2"),
        crate::types::LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("thread link");
    let reads = transcript_queries(&p);
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(pending_texts(&watcher, "we"), vec!["msg://1", "msg://2"]);
    assert_eq!(transcript_queries(&p) - reads, 1);
    assert_eq!(
        transcript_rows(&p) - rows,
        1,
        "only the message it brought in"
    );
    assert!(
        p.sparql_store
            .queries_run()
            .last()
            .is_some_and(|q| q.contains("VALUES")),
        "a read of what the thread link added, not the scope query"
    );
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
}

/// A removal (a deleted message, or the old half of an edit) may take rows
/// away, which a read of what was added cannot see: the tick reads the whole
/// transcript again, and the kept read no longer holds the removed turn.
#[tokio::test]
async fn a_removal_reads_the_whole_transcript_again() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    let links: Vec<_> = (0..10)
        .map(|i| {
            body_link(
                &signer,
                &format!("msg://{i}"),
                &format!("turn {i}"),
                start + chrono::Duration::seconds(i),
            )
        })
        .collect();
    for l in &links {
        p.sparql_store.add_link(l).expect("link");
    }
    let cfg = config("removal", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    p.sparql_store.remove_link(&links[3]).expect("remove");
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(transcript_rows(&p) - rows, 9, "a full read");
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
    assert_eq!(kept.len(), 9);

    // Added links after that are read from the log again.
    p.sparql_store
        .add_link(&body_link(
            &signer,
            "msg://10",
            "turn 10",
            chrono::Utc::now(),
        ))
        .expect("link");
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 1_000, &ctx)
        .await;
    assert_eq!(transcript_rows(&p) - rows, 1);
}

/// A long run of writes that are not additions (removals, here of a link the
/// store does not hold) keeps the store's write log bounded, and the tick
/// after them reads the whole transcript.
#[tokio::test]
async fn a_long_run_of_removals_keeps_the_write_log_bounded_and_the_next_tick_reads_in_full() {
    use crate::perspectives::write_log::WRITE_LOG_MAX_ENTRIES;

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    for i in 0..10 {
        p.sparql_store
            .add_link(&body_link(
                &signer,
                &format!("msg://{i}"),
                &format!("turn {i}"),
                start + chrono::Duration::seconds(i),
            ))
            .expect("link");
    }
    let cfg = config("bounded", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    let ghost = body_link(&signer, "msg://ghost", "never stored", start);
    for _ in 0..3 * WRITE_LOG_MAX_ENTRIES {
        p.sparql_store.remove_link(&ghost).expect("remove");
    }
    assert!(
        p.sparql_store.write_log_entries() <= WRITE_LOG_MAX_ENTRIES,
        "{} log entries after {} removals",
        p.sparql_store.write_log_entries(),
        3 * WRITE_LOG_MAX_ENTRIES
    );
    let rows = transcript_rows(&p);
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(transcript_rows(&p) - rows, 10, "the tick reads in full");
}

/// Josh's open question 1 and Lal's P2-2: a loop waiting after a `NoModel`
/// tick also waits on the AI service's readiness watch, so a model
/// configured while turns wait is used at once, not after the 60 s
/// fallback or the next write.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_model_becoming_ready_wakes_a_loop_waiting_for_one() {
    use super::watcher::test_hooks::{PassHooks, HOOKS};
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

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
    write_processor(&mut p, &config("awaiting", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");

    let no_model = Arc::new(AtomicBool::new(true));
    let readiness = Arc::new(tokio::sync::watch::Sender::new(0u64));
    let hooks = PassHooks {
        no_model: no_model.clone(),
        model_readiness: Some(readiness.clone()),
        ..Default::default()
    };
    let checks = hooks.model_checks.clone();
    let uuid = p.uuid.clone();
    let mut rx = subscribe().await;
    let looping = p.clone();
    let loop_ctx = ctx.clone();
    let handle = tokio::spawn(HOOKS.scope(hooks, async move {
        looping.auto_processor_watch_loop(loop_ctx).await
    }));

    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while checks.load(Ordering::Relaxed) == 0 {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the loop's first tick never asked for a model"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    // Let the loop settle into its idle wait.
    tokio::time::sleep(Duration::from_millis(700)).await;
    assert_eq!(checks.load(Ordering::Relaxed), 1);

    // A model is configured. No link is written.
    no_model.store(false, Ordering::Relaxed);
    let configured = tokio::time::Instant::now();
    readiness.send_modify(|g| *g += 1);
    next_event_matching(&mut rx, Duration::from_millis(1_500), |e| {
        e.perspective_uuid == uuid
            && e.processor_id == "awaiting"
            && e.step == AutoProcessorStep::BatchReady
    })
    .await
    .expect("the waiting batch drains within 1.5 s of the model, not 60 s");
    println!(
        "batch ready {:?} after the model was configured",
        configured.elapsed()
    );

    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}

/// Lal's P2-3: a turn that arrives late, stamped before the turns already
/// read, must sit where a fresh read of the scope query (`ORDER BY
/// ?timestamp`) puts it, so turns are queued, and batched, in the same order.
/// The turns are held back (deferred) until all four are read, so the queue
/// is built from the transcript's order alone: watcher A reads three turns,
/// then merges the late one; watcher B reads all four at once.
#[tokio::test]
async fn a_late_older_turn_is_merged_where_a_fresh_read_puts_it() {
    use super::watcher::turn_id;

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    let links: Vec<_> = (0..4)
        .map(|i| {
            body_link(
                &signer,
                &format!("msg://{i}"),
                &format!("turn {i}"),
                start + chrono::Duration::seconds(i),
            )
        })
        .collect();
    let ids: Vec<String> = links
        .iter()
        .enumerate()
        .map(|(i, l)| turn_id(&l.author, &format!("turn {i}"), &l.timestamp))
        .collect();
    // Turns 1 to 3 first; turn 0, the oldest, arrives late.
    for l in &links[1..] {
        p.sparql_store.add_link(l).expect("link");
    }
    let cfg = config("ordered", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    let now_ms = chrono::Utc::now().timestamp_millis();
    let mut merged = WatcherState::new();
    merged.defer_turns("ordered", &ids, now_ms, 10_000);
    p.run_auto_processor_tick(&mut merged, now_ms, &ctx).await;

    p.sparql_store.add_link(&links[0]).expect("the late turn");
    p.run_auto_processor_tick(&mut merged, now_ms + 500, &ctx)
        .await;
    assert!(
        p.sparql_store
            .queries_run()
            .last()
            .is_some_and(|q| q.contains("VALUES")),
        "the late turn was merged from a read of what was added"
    );

    let mut fresh = WatcherState::new();
    fresh.defer_turns("ordered", &ids, now_ms, 10_000);
    p.run_auto_processor_tick(&mut fresh, now_ms + 500, &ctx)
        .await;

    let order = |w: &WatcherState| -> Vec<String> {
        let (_, turns) = w.reads.transcript(&cfg).expect("a kept read");
        turns.iter().map(|t| t.id().to_string()).collect()
    };
    assert_eq!(order(&merged), ids, "the merged read is in timestamp order");
    assert_eq!(order(&merged), order(&fresh));

    // The deferral ends: both queue the four turns in transcript order.
    for w in [&mut merged, &mut fresh] {
        p.run_auto_processor_tick(w, now_ms + 20_000, &ctx).await;
    }
    let queued = |w: &WatcherState| -> Vec<String> {
        w.pending_for("ordered")
            .map(|e| e.items.iter().map(|t| t.id.clone()).collect())
            .unwrap_or_default()
    };
    assert_eq!(queued(&merged), ids);
    assert_eq!(queued(&merged), queued(&fresh));
    let drain = AutoProcessorConfig {
        debounce_ms: 0,
        batch_max: 2,
        ..cfg.clone()
    };
    let batch_ids = |b: Option<Vec<PendingTurn>>| -> Vec<String> {
        b.expect("a batch").into_iter().map(|t| t.id).collect()
    };
    assert_eq!(
        batch_ids(merged.drain_ready_batch(&drain, now_ms + 20_500)),
        batch_ids(fresh.drain_ready_batch(&drain, now_ms + 20_500)),
        "the first FIFO batch is the fresh read's"
    );
}
