//! Subscription re-checks after a write (PERF-16).
//!
//! A perspective's background loops run under one `join!` on one task
//! (`start_background_tasks`), and `check_subscribed_queries` re-runs every
//! affected subscription. These tests pin that a pass leaves that task free
//! while the store works, that independent re-checks run side by side, and
//! that each result lands on the subscription that asked for it.
//!
//! The fixture's query is slow on purpose: a filtered self-join over `n`
//! links, so one evaluation takes a measurable time.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::types::{Link, LinkExpression, LinkStatus};
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};

const SLOW_PREDICATE: &str = "ns://slow";
const SLOW_QUERY: &str = "SELECT (COUNT(*) AS ?c) WHERE { \
     ?a <ns://slow> ?x . ?b <ns://slow> ?y . FILTER(STR(?x) < STR(?y)) }";
const FAST_QUERY: &str = "SELECT ?s ?o WHERE { ?s <ns://title> ?o . }";

fn add_links(p: &PerspectiveInstance, predicate: &str, range: std::ops::Range<usize>) {
    let signer = TestSigner::generate();
    for i in range {
        let data = Link {
            source: format!("ns://node/{i}"),
            predicate: Some(predicate.to_string()),
            target: format!("literal:string:v{i:06}"),
        };
        let mut link = LinkExpression::from(signer.sign(data));
        link.status = Some(LinkStatus::Shared);
        p.sparql_store.add_link(&link).expect("add link");
    }
}

/// The median time of one evaluation of `SLOW_QUERY`.
fn slow_query_time(p: &PerspectiveInstance) -> Duration {
    let mut runs: Vec<Duration> = (0..3)
        .map(|_| {
            let started = Instant::now();
            p.sparql_query(SLOW_QUERY.into()).expect("slow query");
            started.elapsed()
        })
        .collect();
    runs.sort();
    runs[1]
}

/// Add slow-query links until one evaluation takes at least `at_least`
/// (bounded, so a very fast machine still finishes). Returns the link count
/// and the time of one evaluation.
fn grow_until_slow(p: &PerspectiveInstance, at_least: Duration) -> (usize, Duration) {
    let mut n = 0;
    let mut next = 400;
    loop {
        add_links(p, SLOW_PREDICATE, n..next);
        n = next;
        let one = slow_query_time(p);
        if one >= at_least || n >= 12_800 {
            return (n, one);
        }
        next = n * 2;
    }
}

async fn subscribe(p: &PerspectiveInstance, query: &str, count: usize) -> Vec<String> {
    let mut ids = Vec::with_capacity(count);
    for _ in 0..count {
        let (id, _, _) = p
            .subscribe_and_query(query.into(), "c".into(), None)
            .await
            .expect("subscribe");
        ids.push(id);
    }
    ids
}

/// Run one pass while a sibling future on the same task ticks every 2 ms,
/// as the perspective's other loops do beside `subscribed_queries_loop`.
/// Returns the pass time and the longest the sibling went without running.
async fn pass_with_sibling(p: &PerspectiveInstance) -> (Duration, Duration) {
    let done = AtomicBool::new(false);
    // The sibling is polled first, so it is waiting on its timer when the
    // pass starts.
    let sibling = async {
        let mut last = Instant::now();
        let mut longest = Duration::ZERO;
        loop {
            sleep(Duration::from_millis(2)).await;
            longest = longest.max(last.elapsed());
            last = Instant::now();
            if done.load(Ordering::Acquire) {
                return longest;
            }
        }
    };
    let pass = async {
        let started = Instant::now();
        p.check_subscribed_queries(ChangedPredicates::CheckAll)
            .await;
        done.store(true, Ordering::Release);
        started.elapsed()
    };
    let (longest, elapsed) = tokio::join!(sibling, pass);
    (elapsed, longest)
}

async fn assert_updated(p: &PerspectiveInstance, ids: &[String], query: &str) {
    let expected = p.sparql_query(query.into()).expect("query");
    let queries = p.subscribed_queries.lock().await;
    for id in ids {
        assert_eq!(queries[id].last_result, expected, "subscription {id}");
        assert_eq!(queries[id].revision, 1, "subscription {id}");
    }
}

/// On dev the pass ran the store queries on the task itself: the sibling
/// stood still for the whole pass (every query, one after another).
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_recheck_pass_leaves_its_task_free_while_the_store_works() {
    let (p, _, _) = setup_perspective_no_llm(&[]).await;
    let (n, one) = grow_until_slow(&p, Duration::from_millis(80));
    let ids = subscribe(&p, SLOW_QUERY, 2).await;
    add_links(&p, SLOW_PREDICATE, n..n + 1);

    let (elapsed, longest) = pass_with_sibling(&p).await;

    assert!(
        longest < one / 2,
        "the sibling went {longest:?} without running during a {elapsed:?} pass \
         (one query: {one:?}, {n} links)"
    );
    assert_updated(&p, &ids, SLOW_QUERY).await;
}

/// On dev a pass took the sum of its queries' times.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn independent_rechecks_run_side_by_side() {
    let cores = std::thread::available_parallelism().map_or(1, |n| n.get());
    if cores < 4 {
        eprintln!("skipped: {cores} cores, the test needs 4");
        return;
    }
    let (p, _, _) = setup_perspective_no_llm(&[]).await;
    let (n, one) = grow_until_slow(&p, Duration::from_millis(80));
    const SUBSCRIPTIONS: u32 = 4;
    let ids = subscribe(&p, SLOW_QUERY, SUBSCRIPTIONS as usize).await;
    add_links(&p, SLOW_PREDICATE, n..n + 1);

    let started = Instant::now();
    p.check_subscribed_queries(ChangedPredicates::CheckAll)
        .await;
    let elapsed = started.elapsed();

    let sequential = one * SUBSCRIPTIONS;
    assert!(
        elapsed < sequential * 6 / 10,
        "{SUBSCRIPTIONS} re-checks took {elapsed:?}; one after another is {sequential:?} \
         (one query: {one:?}, {n} links)"
    );
    assert_updated(&p, &ids, SLOW_QUERY).await;
}

/// Re-checks finish in any order; each result still lands on its own
/// subscription, and a later pass carries the newer result.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn each_result_lands_on_its_own_subscription_and_passes_stay_in_order() {
    let (p, _, _) = setup_perspective_no_llm(&[]).await;
    let (n, _) = grow_until_slow(&p, Duration::from_millis(30));
    // The slow subscriptions are registered first, the fast ones finish first.
    let slow = subscribe(&p, SLOW_QUERY, 3).await;
    let fast = subscribe(&p, FAST_QUERY, 3).await;
    add_links(&p, SLOW_PREDICATE, n..n + 1);
    add_links(&p, "ns://title", 0..1);

    p.check_subscribed_queries(ChangedPredicates::CheckAll)
        .await;
    assert_updated(&p, &slow, SLOW_QUERY).await;
    assert_updated(&p, &fast, FAST_QUERY).await;

    add_links(&p, SLOW_PREDICATE, n + 1..n + 2);
    add_links(&p, "ns://title", 1..2);
    p.check_subscribed_queries(ChangedPredicates::CheckAll)
        .await;
    let queries = p.subscribed_queries.lock().await;
    for (ids, query) in [(&slow, SLOW_QUERY), (&fast, FAST_QUERY)] {
        let newest = p.sparql_query(query.into()).expect("query");
        for id in ids {
            assert_eq!(queries[id].last_result, newest, "subscription {id}");
            assert_eq!(queries[id].revision, 2, "subscription {id}");
        }
    }
}

/// Pass time and the longest stall of a sibling on the same task, for 1, 10
/// and 50 subscriptions. Prints a table; run with `--ignored --nocapture`.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "benchmark: prints timings"]
async fn bench_recheck_pass() {
    let (p, _, _) = setup_perspective_no_llm(&[]).await;
    let (mut n, one) = grow_until_slow(&p, Duration::from_millis(40));
    println!("one query: {one:?} over {n} links");
    println!("| subscriptions | pass | longest sibling stall |");
    println!("|---|---|---|");
    let mut ids: Vec<String> = Vec::new();
    for count in [1usize, 10, 50] {
        let more = subscribe(&p, SLOW_QUERY, count - ids.len()).await;
        ids.extend(more);
        add_links(&p, SLOW_PREDICATE, n..n + 1);
        n += 1;
        let (elapsed, longest) = pass_with_sibling(&p).await;
        println!("| {count} | {elapsed:?} | {longest:?} |");
    }
}
