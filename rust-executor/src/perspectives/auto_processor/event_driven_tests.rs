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

/// The next commit-feed batch carrying `predicate`, on a deadline: a bare
/// `recv()` hangs a regressed test at the suite timeout, and settling for
/// whatever batch arrived first would let the assertion below it pass on a
/// write it was not about.
async fn recv_edge(
    commits: &mut tokio::sync::broadcast::Receiver<
        crate::perspectives::perspective_instance::CommitWrite,
    >,
    predicate: &str,
) -> std::sync::Arc<crate::perspectives::perspective_instance::subscription_triggers::Writes> {
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            match commits.recv().await {
                Ok(Some(writes)) if writes.wrote(predicate) => return writes,
                Ok(_) => {}
                Err(e) => panic!("commit feed: {e}"),
            }
        }
    })
    .await
    .unwrap_or_else(|_| panic!("no commit-feed batch carrying `{predicate}` within 5 s"))
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

/// `MESSAGE_SDNA` with `body` on a real `resolveLanguage` — the shape of a
/// text property whose stored target is an expression IRI the store cannot
/// decode, so a `where` on it is decided after hydration.
fn message_sdna_on_language(language: &str) -> String {
    MESSAGE_SDNA.replace(
        "\"resolve_language\":\"literal\"",
        &format!("\"resolve_language\":\"{language}\""),
    )
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
    // A transcript read is one scope query: its `links` provenance comes
    // back on the instance query's own rows.
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
        // Only the reaction batch counts: a setup write broadcast by a
        // spawned task can land in the feed first, and "any batch was
        // evaluated" would then prove nothing about this one.
        if w.wrote("ns://reaction") {
            evaluated.set(evaluated.get() + 1);
        }
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

/// `MESSAGE_SDNA` plus a required `ns://priority` the seeded message
/// lacks.
const MESSAGE_SDNA_V2: &str = r#"{
  "target_class":"ns://Message",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","resolve_language":"literal","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]},
    {"path":"ns://priority","name":"priority","min_count":1,"max_count":1}
  ]
}"#;

/// `MESSAGE_SDNA` with `body` moved to `ns://body2`.
const MESSAGE_SDNA_BODY2: &str = r#"{
  "target_class":"ns://Message",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body2","name":"body","resolve_language":"literal","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body2","target":"value"}]}
  ]
}"#;

/// A shape-only write — nothing the transcript's trigger matches, no
/// `ns://type` or `ns://body` link moves — still invalidates the kept read:
/// the shape it was read under is gone. The edit below adds a required
/// property the seeded message lacks, so the re-read returns no turns and
/// the pending queue is drained of it — before the fix the kept read was
/// trusted for as long as only schema writes landed.
///
/// The write goes through `add_sdna` — the production commit path — because
/// a raw `sparql_store` write takes the unclassifiable fallback and would
/// mark the read stale by accident, hiding the bug it tests for.
#[tokio::test]
async fn a_schema_write_stales_the_kept_transcript() {
    use crate::perspectives::perspective_instance::SdnaType;
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "hello",
        "ns://body",
    )
    .await;
    let cfg = config("schema", 600_000, None);
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(pending_texts(&watcher, "schema"), vec!["hello"]);
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    // The unrelated-write optimisation the schema case must not give up: a
    // predicate the trigger has no rule for keeps the kept read.
    p.add_link(
        Link {
            source: "note://reactions".into(),
            predicate: Some("ns://reaction".into()),
            target: "literal:string:+1".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("unrelated link");
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads,
        "an unrelated write must not re-read the kept transcript"
    );

    p.add_sdna(
        "ns://Message".into(),
        String::new(),
        SdnaType::SubjectClass,
        Some(MESSAGE_SDNA_V2.into()),
        &ctx,
    )
    .await
    .expect("add_sdna");
    p.run_auto_processor_tick(&mut watcher, now_ms + 1000, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a schema write must re-read the kept transcript"
    );
    assert!(
        pending_texts(&watcher, "schema").is_empty(),
        "the message no longer conforms, so its turn is dropped"
    );

    // The trigger's predicate rules were compiled from the same shape the
    // kept read ran under, so the schema write must rebuild it, not only
    // stale the read: with `body` moved to `ns://body2` the old rules still
    // watch `ns://body`, and a data write on the new predicate would match
    // nothing — neither wake nor stale (#1795).
    p.add_sdna(
        "ns://Message".into(),
        String::new(),
        SdnaType::SubjectClass,
        Some(MESSAGE_SDNA_BODY2.into()),
        &ctx,
    )
    .await
    .expect("add_sdna body2");
    p.run_auto_processor_tick(&mut watcher, now_ms + 1500, &ctx)
        .await;
    let reads = transcript_queries(&p);

    p.add_link(
        Link {
            source: "msg://1".into(),
            predicate: Some("ns://body2".into()),
            target: "literal:string:hi".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("body2 link");
    p.run_auto_processor_tick(&mut watcher, now_ms + 2000, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a data write on the new predicate must wake and stale the kept transcript"
    );
    assert_eq!(
        pending_texts(&watcher, "schema"),
        vec!["hi"],
        "the re-read sees the message conform to the new shape"
    );
}

/// An edit to only the `resolveLanguage`/`transform`/`getter` link of a
/// stored property shape carries no SHACL-vocabulary predicate, yet it IS
/// the shape — the classifier must see it through the source: the node an
/// existing `sh://property` link names. The same predicate on instance
/// data, which a `@Property` decorator emits onto every instance's
/// property URI, must not stale (#1795).
#[tokio::test]
async fn a_shape_detail_edit_stales_the_kept_transcript() {
    use crate::types::{Link, LinkStatus, PerspectiveDiff};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "hello",
        "ns://body",
    )
    .await;
    write_processor(&mut p, &config("shape", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(pending_texts(&watcher, "shape"), vec!["hello"]);
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    // The decorator's per-instance spelling of the same predicate is data:
    // `msg://1.body` is named by no `sh://property` link.
    p.add_link(
        Link {
            source: "msg://1.body".into(),
            predicate: Some("ad4m://resolveLanguage".into()),
            target: "literal:string:literal".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("instance resolveLanguage");
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads,
        "resolveLanguage on an instance property URI is data, not schema"
    );

    // A local write on the shape's property node — `ns://ns://Message.type`
    // here (the fixture names the class by URI) — is the stored shape. The
    // flag property declares no `resolveLanguage`, so the shape the tick
    // reads shows whether it took the edit.
    let type_language = |p: &PerspectiveInstance| {
        p.get_shape("ns://Message")
            .expect("shape")
            .properties
            .iter()
            .find(|prop| prop.name == "type")
            .and_then(|prop| prop.resolve_language.clone())
    };
    assert_eq!(type_language(&p), None);
    p.add_link(
        Link {
            source: "ns://ns://Message.type".into(),
            predicate: Some("ad4m://resolveLanguage".into()),
            target: "literal:string:literal".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("shape resolveLanguage");
    p.run_auto_processor_tick(&mut watcher, now_ms + 1000, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a resolveLanguage edit on a property shape must stale the kept transcript"
    );
    assert_eq!(
        type_language(&p).as_deref(),
        Some("literal"),
        "the re-read parses the edited shape, not the cached one"
    );
    let reads = transcript_queries(&p);

    // A peer's diff lands the same classification through
    // `persist_remote_diff`'s feed entry: signed links are applied before
    // the tick drains, so the property node already answers as a shape.
    let signer = crate::agent::signatures::TestSigner::generate();
    let data = crate::types::Link {
        source: "ns://ns://Message.body".into(),
        predicate: Some("ad4m://transform".into()),
        target: "literal:string:{}".into(),
    }
    .normalize();
    let signed = signer.sign(data);
    p.diff_from_link_language(PerspectiveDiff {
        additions: vec![crate::types::LinkExpression::from(signed)],
        removals: vec![],
    })
    .await
    .expect("peer diff");
    p.run_auto_processor_tick(&mut watcher, now_ms + 1500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a peer diff editing only a shape detail must stale the kept transcript"
    );
}

/// A peer's schema diff moves the store before its feed entry is broadcast
/// (`persist_remote_diff` writes, then broadcasts). A tick in between sees a
/// write no entry classified: it must re-read under the stored shape, not
/// the cached one, though nothing has dropped the cache yet.
#[tokio::test]
async fn a_schema_write_seen_before_its_feed_entry_is_read_under_the_new_shape() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "hello",
        "ns://body",
    )
    .await;
    write_processor(
        &mut p,
        &config("gap-shape", 600_000, None),
        Some(false),
        &ctx,
    )
    .await
    .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let type_language = |p: &PerspectiveInstance| {
        p.get_shape("ns://Message")
            .expect("shape")
            .properties
            .iter()
            .find(|prop| prop.name == "type")
            .and_then(|prop| prop.resolve_language.clone())
    };
    assert_eq!(type_language(&p), None, "the tick cached the shape");

    // Straight into the store: no feed entry, no cache drop.
    let mut schema = crate::types::LinkExpression::from(
        crate::agent::signatures::TestSigner::generate().sign(crate::types::Link {
            source: "ns://ns://Message.type".into(),
            predicate: Some("ad4m://resolveLanguage".into()),
            target: "literal:string:literal".into(),
        }),
    );
    schema.status = Some(crate::types::LinkStatus::Shared);
    p.sparql_store
        .add_link(&schema)
        .expect("a peer's schema link");
    let reads = transcript_queries(&p);

    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(transcript_queries(&p), reads + 1, "the gap stales the read");
    assert_eq!(
        type_language(&p),
        Some("literal".to_string()),
        "the re-read parsed the stored shape, not the cached one"
    );
}

/// The links of `sdna_json`'s shape, signed as a peer would send them.
fn peer_shape_diff(sdna_json: &str) -> crate::types::PerspectiveDiff {
    let signer = crate::agent::signatures::TestSigner::generate();
    crate::types::PerspectiveDiff {
        additions: crate::perspectives::shacl_parser::parse_shacl_to_links(
            sdna_json,
            "ns://Message",
        )
        .expect("shape links")
        .into_iter()
        .map(|l| crate::types::LinkExpression::from(signer.sign(l.normalize())))
        .collect(),
        removals: vec![],
    }
}

/// The tick that drains a peer's schema diff re-reads under the new shape:
/// the entry stales every kept transcript, and a re-read under the shape
/// the write replaced, from a cache still warm, would be marked fresh
/// (#1795). The tick drops the shape cache where it marks the reads stale,
/// so no write path's ordering decides which shape it parses.
#[tokio::test]
async fn a_peer_schema_diff_is_re_read_under_the_new_shape() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    seed_message(
        &mut p,
        &ctx,
        "msg://1",
        "did:key:alice",
        "hello",
        "ns://body",
    )
    .await;
    write_processor(&mut p, &config("order", 50, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(pending_texts(&watcher, "order"), vec!["hello"]);
    assert_eq!(transcript_queries(&p), 1);

    p.diff_from_link_language(peer_shape_diff(MESSAGE_SDNA_V2))
        .await
        .expect("peer schema diff");

    // The driven tick stands in for the one the entry wakes: the message
    // lacks `ns://priority`, so under the new shape it no longer conforms.
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(transcript_queries(&p), 2);
    assert!(
        pending_texts(&watcher, "order").is_empty(),
        "the re-read under the new shape drops the message"
    );

    // The same ordering on the loop's own timer: the schema write's feed
    // entry wakes its wait, and the tick re-reads once.
    let looping = p.clone();
    let loop_ctx = ctx.clone();
    let handle = tokio::spawn(async move { looping.auto_processor_watch_loop(loop_ctx).await });
    let reads = transcript_queries(&p);

    p.diff_from_link_language(peer_shape_diff(MESSAGE_SDNA_BODY2))
        .await
        .expect("second peer schema diff");

    let deadline = tokio::time::Instant::now() + Duration::from_secs(5);
    while transcript_queries(&p) == reads {
        assert!(
            tokio::time::Instant::now() < deadline,
            "the loop's timer-driven tick never re-read the transcript"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    p.teardown_background_tasks().await;
    tokio::time::timeout(Duration::from_secs(2), handle)
        .await
        .expect("teardown ends the loop promptly")
        .expect("loop task");
}

/// A fed write is only half of coverage: an entry's arrival says nothing
/// about the writes before it. Here an in-scope write bypasses the feed (a
/// raw `sparql_store` write, the way a peer's links land in these tests) and
/// an unrelated fed write lands after it — its entry classified, matching
/// nothing. The generation the raw write took has no entry, so the tick must
/// treat the gap as unclassifiable and re-read the kept transcript — which
/// picks the bypassed turn up — rather than trusting the unrelated entry's
/// say-so.
#[tokio::test]
async fn an_unfed_write_before_a_fed_one_stales_the_kept_transcript() {
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
    write_processor(&mut p, &config("gap", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert_eq!(pending_texts(&watcher, "gap"), vec!["first"]);
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    // The write the feed never sees.
    let signer = crate::agent::signatures::TestSigner::generate();
    for l in message_links(&signer, "msg://raw", "raw", chrono::Utc::now()) {
        p.sparql_store.add_link(&l).expect("unfed write");
    }
    // An unrelated fed write after it: classified, matching nothing.
    p.add_link(
        Link {
            source: "note://reactions".into(),
            predicate: Some("ns://reaction".into()),
            target: "literal:string:+1".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("unrelated link");

    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a generation no entry accounts for must re-read the kept transcript"
    );
    assert_eq!(
        pending_texts(&watcher, "gap"),
        vec!["first", "raw"],
        "the bypassed in-scope write is in the transcript it staled"
    );
}

/// The gap does not need a matching write to matter: a feed-bypassing write
/// and an unrelated fed one, both outside the scope, still re-read the kept
/// transcript. The fed entry alone proves nothing about the generation the
/// raw write took — coverage is every generation since the last drain
/// having an entry, not the newest entry reaching the store's count.
#[tokio::test]
async fn a_feed_bypassing_write_stales_the_kept_transcript() {
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
    write_processor(&mut p, &config("raw", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    // An unrelated write the feed never sees, then an unrelated fed write.
    let signer = crate::agent::signatures::TestSigner::generate();
    let raw = crate::types::Link {
        source: "note://raw".into(),
        predicate: Some("ns://reaction".into()),
        target: "literal:string:raw".into(),
    }
    .normalize();
    let signed = signer.sign(raw);
    let mut unfed = crate::types::LinkExpression::from(signed);
    unfed.status = Some(LinkStatus::Local);
    p.sparql_store.add_link(&unfed).expect("unfed write");
    p.add_link(
        Link {
            source: "note://reactions".into(),
            predicate: Some("ns://reaction".into()),
            target: "literal:string:+1".into(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("unrelated link");

    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads + 1,
        "a generation no entry accounts for must re-read the kept transcript"
    );
    assert_eq!(
        pending_texts(&watcher, "raw"),
        vec!["first"],
        "nothing in scope changed; the re-read returns the same turns"
    );
}

/// A fed write that lands mid-tick — after the tick read the store's
/// generation — leaves an entry carrying a generation above the one the
/// tick saw. The drain proves coverage past the generation it read, so the
/// next tick owes nothing: no gap, no stale-all, the kept transcript is
/// reused. Sending on `commit_writes` is what the commit path does; the
/// store staying at `generation` is what "mid-tick" means — the write had
/// not committed when the tick looked.
#[tokio::test]
async fn a_write_landing_mid_tick_is_covered_without_a_stale_all() {
    use crate::perspectives::perspective_instance::subscription_triggers;

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
    write_processor(&mut p, &config("mid", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    // An unrelated entry carrying a generation above the store's — what a
    // fed write committing after the tick's generation read leaves behind.
    let reaction = crate::types::Link {
        source: "note://reactions".into(),
        predicate: Some("ns://reaction".into()),
        target: "literal:string:+1".into(),
    };
    let generation = p.sparql_store.write_generation();
    let mut writes =
        subscription_triggers::writes_of_links([reaction.clone()].iter()).expect("predicated link");
    writes.generations = vec![generation + 1];
    p.commit_writes
        .send(Some(std::sync::Arc::new(writes)))
        .expect("the feed is watched");

    // This tick drains the entry while the store still reads `generation`.
    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;

    // The write commits. Straight into the store, so the store moves to
    // `generation + 1` without a second entry: only the coverage the drain
    // above proved can account for it. A drain that dropped what it proved
    // past the generation it read would see a gap here and re-read.
    let mut committed = crate::types::LinkExpression::from(
        crate::agent::signatures::TestSigner::generate().sign(reaction),
    );
    committed.status = Some(crate::types::LinkStatus::Shared);
    p.sparql_store.add_link(&committed).expect("commit");
    assert_eq!(p.sparql_store.write_generation(), generation + 1);

    p.run_auto_processor_tick(&mut watcher, now_ms + 1_000, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads,
        "coverage proven past the generation read: the next tick owes nothing"
    );
    assert_eq!(pending_texts(&watcher, "mid"), vec!["first"]);
}

/// The link-by-link fallback of `write_link_diff` still broadcasts one feed
/// entry — carrying one generation per link the fallback wrote — so a diff
/// `apply_diff` refuses whole stays covered: one entry, N generations, no
/// stale-all. The refusal below is a link in the store's metadata
/// vocabulary, which `apply_diff` fails the diff on and the fallback then
/// skips per link.
#[tokio::test]
async fn a_fallback_diff_feeds_one_entry_with_every_generation_it_took() {
    use crate::agent::signatures::TestSigner;
    use crate::types::PerspectiveDiff;

    const N: usize = 3;
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
    write_processor(&mut p, &config("fb", 600_000, None), Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let reads = transcript_queries(&p);
    assert_eq!(reads, 1);

    let signer = TestSigner::generate();
    let link = |predicate: &str, target: &str| {
        let signed = signer.sign(
            crate::types::Link {
                source: "note://reactions".into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            }
            .normalize(),
        );
        let mut l = crate::types::LinkExpression::from(signed);
        l.status = Some(crate::types::LinkStatus::Local);
        l
    };
    let mut additions: Vec<_> = (0..N)
        .map(|i| link("ns://reaction", &format!("literal:string:r{i}")))
        .collect();
    additions.push(link("ad4m://ontology/note", "literal:string:x"));

    let mut rx = p.commit_writes.subscribe();
    let before = p.sparql_store.write_generation();
    p.persist_link_diff(&PerspectiveDiff::from(additions, Vec::new()))
        .await
        .expect("persist_link_diff");

    let entry = tokio::time::timeout(Duration::from_secs(5), rx.recv())
        .await
        .expect("a feed entry within 5 s")
        .expect("the feed is open")
        .expect("the batch carried only predicated links");
    let expected: Vec<u64> = (before + 1..=before + N as u64).collect();
    assert_eq!(
        entry.generations, expected,
        "one generation per link the fallback wrote — the refused one took none"
    );
    assert_eq!(p.sparql_store.write_generation(), before + N as u64);

    p.run_auto_processor_tick(&mut watcher, now_ms + 500, &ctx)
        .await;
    assert_eq!(
        transcript_queries(&p),
        reads,
        "every generation accounted for: the kept transcript is reused"
    );
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

/// The same `where` must not reject a row whose `resolveLanguage` fetch
/// failed: a value comparison on an expression-backed property is never
/// pushed, and the hydrated raw URL is `__unresolved` — undecidable, not a
/// non-match. The row is not a turn yet, but the retry clock re-reads it
/// once the language serves the expression, and the turn is then admitted
/// without a store write (#1795).
#[tokio::test]
async fn a_where_on_the_text_keeps_an_unresolvable_row_for_retry() {
    use super::watcher::{TickOutcome, UNRESOLVED_TEXT_RETRY_MS};
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::interpretation_test_support::{
        serve_test_language, stop_test_language,
    };
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[(
        "ns://Message",
        &message_sdna_on_language("QmUnavailableWhereLang"),
    )])
    .await;
    let cfg = AutoProcessorConfig {
        source_query: Some(r#"{"where":{"body":{"contains":"resolved"}}}"#.into()),
        ..config("wu", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    // The message's body lives on a language that is not loaded; the stored
    // target is the raw expression URL, which `contains "resolved"` cannot
    // prove false of the resolved value.
    for (predicate, target) in [
        ("ns://type", "ns://message"),
        ("ns://body", "QmUnavailableWhereLang://expr1"),
    ] {
        p.add_link(
            Link {
                source: "msg://wu".into(),
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
        pending_texts(&watcher, "wu").is_empty(),
        "the expression URL must not be queued as the body"
    );

    // The language comes up and serves the expression — without a store
    // write. The `where` cannot reject the row on the raw URL, so the read
    // carried `__unresolved`, the retry clock re-reads, and the turn —
    // whose resolved text matches — is queued.
    let expression = serde_json::to_string(&TestSigner::generate().sign("resolved words"))
        .expect("expression json");
    serve_test_language("QmUnavailableWhereLang", expression).await;
    let outcome = p
        .run_auto_processor_tick(&mut watcher, now_ms + UNRESOLVED_TEXT_RETRY_MS, &ctx)
        .await;
    stop_test_language("QmUnavailableWhereLang").await;
    assert_eq!(
        outcome,
        TickOutcome::Pending,
        "the resolved text satisfies the where and the turn waits out its debounce"
    );
    assert_eq!(
        transcript_queries(&p),
        2,
        "past the retry clock the kept read is not trusted"
    );
    assert_eq!(pending_texts(&watcher, "wu"), vec!["resolved words"]);
}

/// The mirror of the retry test: a value comparison on an expression-backed
/// property is decided on the resolved value, after hydration — so a body
/// that does not satisfy the `where` is dropped, even though the raw
/// expression URL the store holds is all a pushed comparison could see
/// (#1795).
#[tokio::test]
async fn a_where_on_the_text_drops_the_row_whose_resolved_value_fails() {
    use super::watcher::TickOutcome;
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::interpretation_test_support::{
        serve_test_language, stop_test_language,
    };
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[(
        "ns://Message",
        &message_sdna_on_language("QmResolvedWhereLang"),
    )])
    .await;
    let cfg = AutoProcessorConfig {
        source_query: Some(r#"{"where":{"body":{"contains":"resolved"}}}"#.into()),
        ..config("wr", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    // The language is already up and the expression decodes to a body the
    // `where` excludes. The comparison is not pushed — only the
    // post-hydration filter can reject the row.
    let expression = serde_json::to_string(&TestSigner::generate().sign("other words"))
        .expect("expression json");
    serve_test_language("QmResolvedWhereLang", expression).await;
    for (predicate, target) in [
        ("ns://type", "ns://message"),
        ("ns://body", "QmResolvedWhereLang://expr1"),
    ] {
        p.add_link(
            Link {
                source: "msg://wr".into(),
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
    // The body resolves: an unfiltered read returns it decoded, so the
    // `where`, not a failed fetch, is what drops it below.
    let unfiltered = crate::perspectives::interpretation::gather_transcript_model(
        &p,
        "ns://Message",
        "body",
        "{}",
    )
    .await
    .expect("unfiltered read");
    stop_test_language("QmResolvedWhereLang").await;
    let texts: Vec<&str> = unfiltered.turns.iter().map(|t| t.text.as_str()).collect();
    assert_eq!(texts, vec!["other words"]);
    assert_eq!(
        outcome,
        TickOutcome::Idle,
        "a resolved body that fails the `where` is not a turn"
    );
    assert!(
        pending_texts(&watcher, "wr").is_empty(),
        "the resolved `other words` must not queue under `contains: resolved`"
    );
}

/// The `NOT` case: `NOT {body: {contains: "resolved"}}` cannot be pushed
/// either — the inner comparison would test the raw expression IRI — so the
/// whole negation is decided on the resolved value. A body matching the
/// negated condition must be rejected post-hydration, not kept as a turn.
#[tokio::test]
async fn a_not_where_on_the_text_drops_the_row_whose_resolved_value_matches() {
    use super::watcher::TickOutcome;
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::interpretation_test_support::{
        serve_test_language, stop_test_language,
    };
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[(
        "ns://Message",
        &message_sdna_on_language("QmResolvedNotLang"),
    )])
    .await;
    let cfg = AutoProcessorConfig {
        source_query: Some(r#"{"where":{"NOT":{"body":{"contains":"resolved"}}}}"#.into()),
        ..config("wn", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");

    let expression = serde_json::to_string(&TestSigner::generate().sign("resolved words"))
        .expect("expression json");
    serve_test_language("QmResolvedNotLang", expression).await;
    for (predicate, target) in [
        ("ns://type", "ns://message"),
        ("ns://body", "QmResolvedNotLang://expr1"),
    ] {
        p.add_link(
            Link {
                source: "msg://wn".into(),
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
    // As above: the body resolves, so the negation is what drops it.
    let unfiltered = crate::perspectives::interpretation::gather_transcript_model(
        &p,
        "ns://Message",
        "body",
        "{}",
    )
    .await
    .expect("unfiltered read");
    stop_test_language("QmResolvedNotLang").await;
    let texts: Vec<&str> = unfiltered.turns.iter().map(|t| t.text.as_str()).collect();
    assert_eq!(texts, vec!["resolved words"]);
    assert_eq!(
        outcome,
        TickOutcome::Idle,
        "a body matching the negated `contains` is not a turn"
    );
    assert!(
        pending_texts(&watcher, "wn").is_empty(),
        "`resolved words` must not queue under `NOT contains: resolved`"
    );
}

/// A `path` scope — `{"parent":{"ids":C,"path":["ns://child","we://thread"]}}`
/// — reads a conversation's children and, one hop further, their replies:
/// both become pending turns while a message under another conversation
/// never does (#1795). The wake half rides the same scope: the `ns://child`
/// edge under a different anchor cannot change the read, so the trigger
/// rules it out, while the same edge under the scoped anchor and a
/// `we://thread` edge off a scoped message both wake it.
#[tokio::test]
async fn a_path_scope_reads_children_and_their_replies() {
    use crate::types::{Link, LinkStatus};

    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let link = |source: &str, predicate: &str, target: &str| Link {
        source: source.into(),
        predicate: Some(predicate.into()),
        target: target.into(),
    };

    // Conversation C holds a direct child; that child threads a reply one
    // `we://thread` hop deeper. Another conversation holds a message the
    // path cannot reach.
    for (s, pr, t) in [
        ("we://conv", "ns://child", "msg://direct"),
        ("msg://direct", "we://thread", "msg://reply"),
        ("we://other-conv", "ns://child", "msg://other"),
    ] {
        p.add_link(link(s, pr, t), LinkStatus::Local, None, &ctx)
            .await
            .expect("scope edge");
    }
    for (msg, text) in [
        ("msg://direct", "direct"),
        ("msg://reply", "reply"),
        ("msg://other", "other conv"),
    ] {
        for l in [
            link(msg, "ns://type", "ns://message"),
            link(msg, "ns://body", &format!("literal:string:{text}")),
        ] {
            p.add_link(l, LinkStatus::Local, None, &ctx)
                .await
                .expect("message link");
        }
    }

    let cfg = AutoProcessorConfig {
        source_query: Some(
            r#"{"parent":{"ids":"we://conv","path":["ns://child","we://thread"]}}"#.into(),
        ),
        ..config("path", 600_000, None)
    };
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    let mut texts = pending_texts(&watcher, "path");
    texts.sort();
    assert_eq!(
        texts,
        vec!["direct", "reply"],
        "hop one and hop two queue; the foreign conversation's message does not"
    );

    // The wake check is over the same `Writes` the commit feed reports.
    // The membership edge under another conversation is ruled out…
    let mut commits = p.commit_writes.subscribe();
    p.add_link(
        link("we://other-conv", "ns://child", "msg://late"),
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .expect("foreign edge");
    let writes = recv_edge(&mut commits, "ns://child").await;
    assert!(
        !p.auto_processor_writes_match(&watcher, &writes),
        "a child edge under another conversation must not wake the loop"
    );
    // …while the same edge under the anchor — and the thread edge that
    // would carry a new reply — both do.
    for (s, pr, t) in [
        ("we://conv", "ns://child", "msg://new"),
        ("msg://reply", "we://thread", "msg://deeper"),
    ] {
        p.add_link(link(s, pr, t), LinkStatus::Local, None, &ctx)
            .await
            .expect("in-scope edge");
        let writes = recv_edge(&mut commits, pr).await;
        assert!(
            p.auto_processor_writes_match(&watcher, &writes),
            "{s} -{pr}-> {t} must wake the loop"
        );
    }
}

/// A pass writes to the store, so the queues the tick recorded before it
/// may be out of date: a later processor drains only on the next tick, after
/// it re-read the store, and that tick drains it first.
#[tokio::test]
async fn a_pass_that_writes_leaves_the_next_drain_to_the_next_tick() {
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
    for id in ["a", "b"] {
        write_processor(&mut p, &config(id, 0, None), Some(false), &ctx)
            .await
            .expect("write_processor");
    }
    let mut watcher = WatcherState::new();
    let now_ms = chrono::Utc::now().timestamp_millis();

    let before = p.sparql_store.write_generation();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
    assert!(
        p.sparql_store.write_generation() > before,
        "the pass wrote to the store"
    );
    assert!(pending_texts(&watcher, "a").is_empty(), "a drained");
    assert_eq!(
        pending_texts(&watcher, "b"),
        vec!["first".to_string()],
        "b waits for a tick that read the store after a's pass"
    );

    p.run_auto_processor_tick(&mut watcher, now_ms + 1, &ctx)
        .await;
    assert!(pending_texts(&watcher, "b").is_empty(), "b drained next");
    assert_eq!(watcher.last_pass_ms.get("b"), Some(&(now_ms + 1)));
}
