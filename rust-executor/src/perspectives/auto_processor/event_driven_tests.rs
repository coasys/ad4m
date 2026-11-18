//! The watch loop is woken by link writes, not a poll timer: with no store
//! change a tick runs no store query (configs, transcript and cursor come from
//! the previous read), and a new turn still wakes it and is queued once.

use super::config::{processor_node, write_processor, AutoProcessorConfig};
use super::events::{next_event_matching, subscribe, AutoProcessorStep};
use super::watcher::{PendingTurn, WatcherState};
use crate::perspectives::interpretation::{
    ensure_interpretation_overlay_classes, gather_transcript_model, mint_interpretation_run,
    InterpretationRunCursor, InterpretationRunMeta,
};
use crate::perspectives::interpretation_test_support::{
    seed_message, setup_perspective_no_llm, MESSAGE_SDNA,
};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::time::Duration;

/// How many times this perspective's store read a transcript.
fn transcript_queries(p: &PerspectiveInstance) -> usize {
    p.sparql_store
        .queries_run()
        .iter()
        .filter(|q| is_transcript_read(q))
        .count()
}

/// The transcript's model query selects the instances conforming to the
/// source class: it names the class flag's target. No config or cursor read
/// does.
fn is_transcript_read(query: &str) -> bool {
    query.contains("ns://message")
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
        source_class: "ns://Message".into(),
        source_text: "body".into(),
        source_query: None,
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
    gather_transcript_model(p, "ns://Message", "body", "{}")
        .await
        .expect("gather")
        .turns
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
    let (p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    // A transcript read is the scope query plus its `links` provenance read.
    assert_eq!(all_queries(&p), reads + 2, "and nothing else is read");
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
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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

/// The wait itself, without the loop: `wait_for_tick` runs every committed
/// batch through the trigger before it decides — an out-of-scope write is
/// evaluated (`matched` is invoked) and the wait ends `Elapsed`, not `Write`;
/// an in-scope one ends it `Write`. `Elapsed` alone cannot distinguish "the
/// write was ruled out" from "the write never arrived", so the matcher is
/// counted.
#[tokio::test]
async fn the_wait_evaluates_a_write_and_only_a_match_ends_it() {
    use super::watcher::{wait_for_tick, WaitEnd};
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    write_processor(&mut p, &config("waited", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    // Builds the wake trigger the loop would test against.
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let mut rx = p.commit_writes.subscribe();
    let teardown = p.is_teardown.clone();

    p.add_link(
        Link {
            source: "note://reactions".into(),
            predicate: Some("ns://reaction".into()),
            target: "literal:string:r1".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("unrelated link");
    let evaluated = std::cell::Cell::new(0usize);
    let end = wait_for_tick(&teardown, 400, Some(&mut rx), None, |w| {
        evaluated.set(evaluated.get() + 1);
        p.auto_processor_writes_match(&watcher, w)
    })
    .await;
    assert!(
        evaluated.get() >= 1,
        "the out-of-scope write was never evaluated, so `Elapsed` proves nothing"
    );
    assert_eq!(
        end,
        WaitEnd::Elapsed,
        "an out-of-scope write ended the wait"
    );

    // Positive control: an in-scope write ends the same wait.
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "first",
        "ns://body",
    )
    .await;
    let end = wait_for_tick(&teardown, 2_000, Some(&mut rx), None, |w| {
        p.auto_processor_writes_match(&watcher, w)
    })
    .await;
    assert_eq!(
        end,
        WaitEnd::Write,
        "an in-scope write did not end the wait"
    );
}

/// Writes that cannot touch the scope (a predicate the model trigger has no
/// rule for: reactions, other agents' outputs, claims, a peer syncing) do not
/// wake an idle loop at all (#1794): `check_subscribed_queries` reports them
/// to the commit feed, the processor's `ModelTrigger` rules them out, and the
/// loop sleeps on. The control write after them — received last, so every
/// earlier write was evaluated by then — wakes the loop for exactly one more
/// transcript read; had any unrelated write woken it, the count would be
/// higher.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn unrelated_writes_do_not_wake_an_idle_loop() {
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    let seen = transcript_queries(&p);

    for i in 0..20 {
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
    }

    // Positive control, received after every unrelated write: it wakes the
    // loop, which reads the transcript once. Waiting for that read also
    // waits out every earlier evaluation — and a wake on any of the 20
    // would have produced one more read than this.
    seed_message(
        &mut p,
        &ctx,
        "msg://2",
        "did:key:alice",
        "second",
        "ns://body",
    )
    .await;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while transcript_queries(&p) == seen {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the in-scope write never woke the loop"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    tokio::time::sleep(Duration::from_millis(200)).await;
    assert_eq!(
        transcript_queries(&p),
        seen + 1,
        "exactly the control write woke the loop"
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

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    let (kept_at, kept, _) = watcher
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
/// first tick queues them (every id is new), the second re-records them as
/// the loop does at every tick (every id is a duplicate). Returns the id
/// comparisons the queue's duplicate check made in each tick.
async fn id_comparisons_in_two_ticks_over(n: usize) -> (u64, u64) {
    use super::watcher::id_comparisons;

    let (p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
        .set_transcript(&cfg, generation, turns, |_, _| true, None);

    let now_ms = chrono::Utc::now().timestamp_millis();
    let before = id_comparisons();
    assert!(p
        .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
        .await
        .polls_at_base_rate());
    let first = id_comparisons() - before;
    let before = id_comparisons();
    assert!(p
        .run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await
        .polls_at_base_rate());
    let second = id_comparisons() - before;
    assert_eq!(
        watcher.pending_for("many").map(|e| e.items.len()),
        Some(n),
        "every turn is queued once"
    );
    (first, second)
}

/// A tick records every queueable turn of the kept transcript again, so the
/// cost of recording one must not grow with the queue: over 50k pending
/// turns (a large space with no model) the duplicate check scanned the
/// queue, and a tick made N²/2 id comparisons. Counted, not timed, and
/// counted as comparisons of queued ids, not as calls: the second tick finds
/// every id already queued, so it must compare at least one id per turn (the
/// check goes through the queue's id set), and at most two (a lookup, not a
/// scan). At 5k and at 50k alike.
#[tokio::test]
async fn a_tick_over_50k_kept_turns_records_them_in_linear_time() {
    for n in [5_000usize, 50_000] {
        let (first, second) = id_comparisons_in_two_ticks_over(n).await;
        let n = n as u64;
        println!(
            "{n} pending turns: {first} id comparisons queueing them, {second} re-recording them"
        );
        assert!(
            first <= n,
            "{first} id comparisons to queue {n} new turns; a lookup of a new id compares next to none"
        );
        assert!(
            (n..=2 * n).contains(&second),
            "{second} id comparisons to re-record {n} queued turns; \
             the duplicate check must look each up in the queue's id set, \
             about one comparison per turn"
        );
    }
}

/// A turn whose `resolveLanguage` text the language cannot serve is not a
/// turn yet: it is not queued, and — the subtle half — it is not *consumed*
/// either. Kept reads are trusted while the store's write generation holds,
/// but an expression arriving is not a store write, so a read that dropped an
/// unresolved text carries a retry clock of its own: past it, the tick
/// re-reads the transcript even though no link changed.
#[tokio::test]
async fn an_unresolvable_text_is_neither_queued_nor_consumed() {
    use super::watcher::{TickOutcome, UNRESOLVED_TEXT_RETRY_MS};
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::interpretation_test_support::{
        serve_test_language, stop_test_language,
    };
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    write_processor(&mut p, &config("lang", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");

    // The message's body lives on a language that is not loaded: the
    // resolveLanguage fetch fails. `body` carries `resolveLanguage: literal`
    // in the SDNA, which is what makes it an expression-resolved property.
    for (predicate, target) in [
        ("ns://type", "ns://message"),
        ("ns://body", "QmUnavailableLang://expr1"),
    ] {
        p.add_link(
            Link {
                source: "msg://lang".into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            },
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("seed message link");
    }

    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    let outcome = p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(
        outcome,
        TickOutcome::Idle,
        "nothing to queue: an unresolvable text is not a turn"
    );
    assert!(
        pending_texts(&watcher, "lang").is_empty(),
        "the expression URL must not be queued as the body"
    );

    // A tick just inside the retry clock still trusts the kept read — the
    // retry is at the loop's idle cadence, not every poll.
    let outcome = p
        .run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(outcome, TickOutcome::Idle);
    let queries_at_idle = transcript_queries(&p);
    assert_eq!(queries_at_idle, 1);

    // The language comes up and serves the expression — without a store
    // write. Past the retry clock, the next tick re-reads and queues it.
    let expression = serde_json::to_string(&TestSigner::generate().sign("resolved words"))
        .expect("expression json");
    serve_test_language("QmUnavailableLang", expression).await;
    let outcome = p
        .run_auto_processor_tick(&mut watcher, now_ms + UNRESOLVED_TEXT_RETRY_MS, &ctx)
        .await;
    stop_test_language("QmUnavailableLang").await;
    assert_eq!(
        outcome,
        TickOutcome::Pending,
        "now the turn waits out its debounce"
    );
    assert_eq!(
        transcript_queries(&p),
        2,
        "past the retry clock the kept read is not trusted"
    );
    assert_eq!(pending_texts(&watcher, "lang"), vec!["resolved words"]);
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

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
/// ticking every 0.5 s. Counted by how often a tick asked for a model. The
/// observation is deterministic, not an elapsed window: out-of-scope writes
/// the wait evaluates must not tick it, and the in-scope control write after
/// them — received last, so every earlier write was evaluated by then —
/// ticks it exactly once more.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn with_no_model_the_watch_loop_stops_polling() {
    use super::watcher::test_hooks::{PassHooks, HOOKS};
    use crate::types::{Link, LinkStatus};
    use std::sync::atomic::Ordering;

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    let parked = checks.load(Ordering::Relaxed);

    for i in 0..10 {
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
    }

    // Positive control, received after every unrelated write: an in-scope
    // turn ends the wait, whose tick asks for a model once more.
    seed_message(
        &mut p,
        &ctx,
        "msg://2",
        "did:key:alice",
        "second",
        "ns://body",
    )
    .await;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while checks.load(Ordering::Relaxed) == parked {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the in-scope write never woke the loop"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    assert_eq!(
        checks.load(Ordering::Relaxed),
        parked + 1,
        "only the in-scope write ticked the parked loop; \
         a poll at 0.5 s would have ticked many times by now"
    );

    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}

/// The two links of a conforming `ns://Message` — the class flag, then the
/// body — signed by `signer` at `at`, as a peer's links arrive.
fn message_links(
    signer: &crate::agent::signatures::TestSigner,
    msg: &str,
    text: &str,
    at: chrono::DateTime<chrono::Utc>,
) -> Vec<crate::types::LinkExpression> {
    let sign = |predicate: &str, target: &str| {
        let data = crate::types::Link {
            source: msg.into(),
            predicate: Some(predicate.into()),
            target: target.into(),
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
    };
    vec![
        sign("ns://type", "ns://message"),
        sign("ns://body", &format!("literal:string:{text}")),
    ]
}

/// The ids of the kept transcript, and of a fresh model-query read of the
/// same scope: the kept read must hold the same turns.
async fn kept_and_fresh_ids(
    p: &PerspectiveInstance,
    watcher: &WatcherState,
    cfg: &AutoProcessorConfig,
) -> (Vec<String>, Vec<String>) {
    let (_, kept, _) = watcher.reads.transcript(cfg).expect("a kept read");
    let mut kept: Vec<String> = kept.iter().map(|t| t.id().to_string()).collect();
    let mut fresh: Vec<String> = gather_transcript_model(
        p,
        &cfg.source_class,
        &cfg.source_text,
        &cfg.transcript_query_json(),
    )
    .await
    .expect("fresh read")
    .turns
    .iter()
    .map(|t| PendingTurn::from_transcript(t).id)
    .collect();
    kept.sort();
    kept.dedup();
    fresh.sort();
    fresh.dedup();
    (kept, fresh)
}

/// A turn that arrives late, stamped before every turn already read (a peer's
/// link syncing in), is not missed for being old: the next tick reads the
/// scope in full and queues it.
#[tokio::test]
async fn a_late_turn_with_an_older_timestamp_is_queued() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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
    for l in message_links(&signer, "msg://late", "late", yesterday) {
        p.sparql_store.add_link(&l).expect("a peer's link");
    }
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert!(
        pending_texts(&watcher, "late").iter().any(|t| t == "late"),
        "the late turn is queued; got {:?}",
        pending_texts(&watcher, "late")
    );
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
}

/// `source_query.parent` limits the transcript to children of that parent:
/// an old message enters the scope when its parent link lands after its
/// body, and the next tick picks it up.
#[tokio::test]
async fn a_link_that_brings_an_old_message_into_a_parent_scope_is_queued() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let link = |s: &str, pr: &str, t: &str| crate::types::Link {
        source: s.into(),
        predicate: Some(pr.into()),
        target: t.into(),
    };
    p.add_link(
        link("ns://chan", "ns://child", "msg://1"),
        crate::types::LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("child link");
    for m in ["msg://1", "msg://2"] {
        for l in [
            link(m, "ns://type", "ns://message"),
            link(m, "ns://body", &format!("literal:string:{m}")),
        ] {
            p.add_link(l, crate::types::LinkStatus::Local, None, &ctx)
                .await
                .expect("link");
        }
    }
    let cfg = AutoProcessorConfig {
        source_query: Some(r#"{"parent":{"id":"ns://chan","predicate":"ns://child"}}"#.into()),
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
        link("ns://chan", "ns://child", "msg://2"),
        crate::types::LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("child link");
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(pending_texts(&watcher, "we"), vec!["msg://1", "msg://2"]);
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
}

/// A removal (a deleted message, or the old half of an edit) takes rows away:
/// the tick reads the whole transcript again, and the kept read no longer
/// holds the removed turn.
#[tokio::test]
async fn a_removal_reads_the_whole_transcript_again() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    let links: Vec<Vec<_>> = (0..10)
        .map(|i| {
            message_links(
                &signer,
                &format!("msg://{i}"),
                &format!("turn {i}"),
                start + chrono::Duration::seconds(i),
            )
        })
        .collect();
    for l in links.iter().flatten() {
        p.sparql_store.add_link(l).expect("link");
    }
    let cfg = config("removal", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    // Removing the flag link takes msg://3 out of the class.
    p.sparql_store.remove_link(&links[3][0]).expect("remove");
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    let (kept, fresh) = kept_and_fresh_ids(&p, &watcher, &cfg).await;
    assert_eq!(kept, fresh);
    assert_eq!(kept.len(), 9);
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

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
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

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    let links: Vec<Vec<_>> = (0..4)
        .map(|i| {
            message_links(
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
        .map(|(i, l)| turn_id(&l[1].author, &format!("turn {i}"), &l[1].timestamp))
        .collect();
    // Turns 1 to 3 first; turn 0, the oldest, arrives late.
    for l in links[1..].iter().flatten() {
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

    for l in &links[0] {
        p.sparql_store.add_link(l).expect("the late turn");
    }
    p.run_auto_processor_tick(&mut merged, now_ms + 500, &ctx)
        .await;

    let mut fresh = WatcherState::new();
    fresh.defer_turns("ordered", &ids, now_ms, 10_000);
    p.run_auto_processor_tick(&mut fresh, now_ms + 500, &ctx)
        .await;

    let order = |w: &WatcherState| -> Vec<String> {
        let (_, turns, _) = w.reads.transcript(&cfg).expect("a kept read");
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

/// A forged message — a signature over other data, carried on this link —
/// does not verify, and default model-query provenance withholds it (#1113):
/// the instance loses its `body` and never becomes a turn. A peer without
/// the key cannot put words into the transcript, and a speaker name in the
/// LLM input can only come from a link that verifies.
#[tokio::test]
async fn a_forged_message_is_not_in_the_transcript() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let at = chrono::Utc::now() - chrono::Duration::minutes(5);
    let real = message_links(&signer, "msg://real", "real text", at);
    for l in &real {
        p.sparql_store.add_link(l).expect("link");
    }
    // What a peer without the key can gossip: the signer's signature over a
    // different body, asserted on this one.
    let mut forged = message_links(&signer, "msg://forged", "decoy", at);
    forged[1].data.target = "literal:string:forged text".into();
    assert!(
        !forged[1].compute_proof_valid(),
        "the fixture must not verify"
    );
    for l in forged {
        p.sparql_store.add_link(&l).expect("forged link");
    }

    let cfg = config("proof", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    let items = &watcher.pending_for("proof").expect("pending").items;
    assert_eq!(
        items.iter().map(|t| t.text.as_str()).collect::<Vec<_>>(),
        vec!["real text"]
    );
    // A turn's speaker and timestamp are the instance's own link metadata:
    // the DID that signed it and the earliest of its link timestamps.
    assert_eq!(items[0].speaker, signer.did);
    assert_eq!(items[0].timestamp, real[1].timestamp);
}

/// The scope is the class, not the predicate: a node carrying the text
/// property without the class flag does not conform, so the model query
/// never returns it and its "body" never becomes a turn.
#[tokio::test]
async fn a_text_predicate_without_the_class_flag_is_not_in_the_transcript() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let at = chrono::Utc::now() - chrono::Duration::minutes(5);
    for l in message_links(&signer, "msg://real", "real text", at) {
        p.sparql_store.add_link(&l).expect("link");
    }
    // A signed, verified body link — but no flag: the node does not conform.
    let bare = message_links(&signer, "msg://bare", "bare body", at)[1].clone();
    p.sparql_store.add_link(&bare).expect("body-only link");

    let cfg = config("shape", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    assert_eq!(pending_texts(&watcher, "shape"), vec!["real text"]);
}

/// `source_query.where` narrows the scope the way a model subscription's
/// would: only conforming instances whose `body` matches become turns.
#[tokio::test]
async fn a_where_clause_filters_the_transcript() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = crate::agent::signatures::TestSigner::generate();
    let at = chrono::Utc::now() - chrono::Duration::minutes(5);
    for (msg, text) in [("msg://keep", "keep me"), ("msg://drop", "another")] {
        for l in message_links(&signer, msg, text, at) {
            p.sparql_store.add_link(&l).expect("link");
        }
    }
    let cfg = AutoProcessorConfig {
        source_query: Some(r#"{"where":{"body":{"contains":"keep"}}}"#.into()),
        ..config("w", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    assert_eq!(pending_texts(&watcher, "w"), vec!["keep me"]);
}
