//! The pending-diffs commit queue (`pending_diffs_loop`) against a scripted
//! link language, on tokio's paused clock.
//!
//! The fake language rejects every commit while it is "down" (a dead link
//! server), and while up refuses any diff that adds a link to `REFUSED`.
//! `fail_next` makes the next commits fail as if the server were briefly down.
//! Accepted links are recorded in order as `+target` / `-target`.

use super::*;
use crate::db::Ad4mDb;
use crate::types::{DecoratedNeighbourhoodExpression, ExpressionProof, Link, PerspectiveState};
use std::sync::atomic::AtomicUsize;

/// The fake language refuses, whenever it is up, any diff that adds a link
/// to this target.
const REFUSED: &str = "refused";

#[derive(Default)]
struct FakeLanguage {
    attempts: AtomicUsize,
    down: AtomicBool,
    /// Fail this many of the next commits, whatever they carry.
    fail_next: AtomicUsize,
    committed: std::sync::Mutex<Vec<String>>,
    /// Links per accepted commit, in order.
    batches: std::sync::Mutex<Vec<usize>>,
}

impl FakeLanguage {
    fn commit(&self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        self.attempts.fetch_add(1, Ordering::SeqCst);
        if self.down.load(Ordering::SeqCst)
            || self
                .fail_next
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |n| n.checked_sub(1))
                .is_ok()
        {
            return Err(anyhow!("link server unreachable"));
        }
        if diff.additions.iter().any(|l| l.data.target == REFUSED) {
            return Err(anyhow!("link server refused the diff"));
        }
        self.batches
            .lock()
            .unwrap()
            .push(diff.additions.len() + diff.removals.len());
        let mut committed = self.committed.lock().unwrap();
        committed.extend(diff.additions.iter().map(|l| format!("+{}", l.data.target)));
        committed.extend(diff.removals.iter().map(|l| format!("-{}", l.data.target)));
        Ok(None)
    }

    fn committed(&self) -> Vec<String> {
        self.committed.lock().unwrap().clone()
    }

    fn attempts(&self) -> usize {
        self.attempts.load(Ordering::SeqCst)
    }
}

fn link(author: &str, target: &str) -> LinkExpression {
    LinkExpression {
        author: author.to_string(),
        timestamp: "2026-10-16T00:00:00.000Z".to_string(),
        data: Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://has".to_string()),
            target: target.to_string(),
        },
        proof: ExpressionProof::default(),
        status: None,
    }
}

fn add(target: &str) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: vec![link("did:key:alice", target)],
        removals: vec![],
    }
}

/// A neighbourhood perspective whose link language is `fake`.
async fn neighbourhood_perspective(fake: &Arc<FakeLanguage>) -> PerspectiveInstance {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("pending diffs".to_string()),
            shared_url: Some("neighbourhood://test".to_string()),
            neighbourhood: Some(DecoratedNeighbourhoodExpression::default()),
            state: PerspectiveState::Synced,
            owners: None,
        },
        None,
    );
    let fake = fake.clone();
    *perspective.link_language.write().await = Some(Language::with_test_commit(
        "fake-link-language".to_string(),
        Arc::new(move |diff| fake.commit(diff)),
    ));
    perspective
}

fn enqueue(perspective: &PerspectiveInstance, diff: &PerspectiveDiff) {
    Ad4mDb::with_global_instance(|db| db.add_pending_diff(&perspective.uuid, diff)).unwrap();
}

/// Store `target`'s link as Shared, as a write through `commit` leaves it.
fn store_shared(perspective: &PerspectiveInstance, target: &str) {
    perspective
        .sparql_store
        .add_link(&LinkExpression {
            status: Some(LinkStatus::Shared),
            ..link("did:key:alice", target)
        })
        .unwrap();
}

/// `fake`'s commit, with a render that answers `remote`.
fn rendering_language(fake: &Arc<FakeLanguage>, remote: Vec<LinkExpression>) -> Language {
    let fake = fake.clone();
    Language::with_test_commit(
        "fake-link-language".to_string(),
        Arc::new(move |diff| fake.commit(diff)),
    )
    .with_test_render(Arc::new(move || {
        Ok(Some(Perspective {
            links: remote.clone(),
        }))
    }))
}

/// Diffs still waiting to be committed.
fn pending(perspective: &PerspectiveInstance) -> usize {
    Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&perspective.uuid, None))
        .unwrap()
        .1
        .len()
}

/// Targets of the diffs taken out of the queue and kept (parked), read
/// through the export, which lists every row with its `is_pending` flag.
fn parked_targets(perspective: &PerspectiveInstance) -> Vec<String> {
    let export = Ad4mDb::with_global_instance(|db| db.export_all_to_json()).unwrap();
    export["perspective_diffs"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["perspective"] == perspective.uuid.as_str() && d["is_pending"] == false)
        .flat_map(|d| d["additions"].as_array().unwrap().clone())
        .map(|l| l["data"]["target"].as_str().unwrap().to_string())
        .collect()
}

fn start_loop(perspective: &PerspectiveInstance) -> tokio::task::JoinHandle<()> {
    let perspective = perspective.clone();
    tokio::spawn(async move { perspective.pending_diffs_loop().await })
}

/// Advance the paused clock in 100 ms steps until `done`, at most `limit`.
/// Returns the simulated time it took.
async fn run_until(limit: Duration, mut done: impl FnMut() -> bool) -> Option<Duration> {
    let start = tokio::time::Instant::now();
    while start.elapsed() < limit {
        if done() {
            return Some(start.elapsed());
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    done().then(|| start.elapsed())
}

fn targets(prefix: &str, n: usize) -> Vec<String> {
    (0..n).map(|i| format!("{}{}", prefix, i)).collect()
}

#[tokio::test(start_paused = true)]
async fn a_large_queue_drains_once_the_language_recovers() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    let good = targets("link", 600);
    for t in &good {
        enqueue(&perspective, &add(t));
    }

    let task = start_loop(&perspective);
    // A minute of outage: nothing gets through, nothing is lost.
    tokio::time::sleep(Duration::from_secs(60)).await;
    assert!(fake.committed().is_empty());
    assert_eq!(pending(&perspective), 600);

    fake.down.store(false, Ordering::SeqCst);
    let drained = run_until(Duration::from_secs(120), || pending(&perspective) == 0).await;
    task.abort();

    assert!(
        drained.is_some(),
        "600 queued diffs must drain within 120 s of the language recovering; \
         {} still pending after {} attempts",
        pending(&perspective),
        fake.attempts()
    );
    let mut committed = fake.committed();
    committed.sort();
    let mut expected: Vec<String> = good.iter().map(|t| format!("+{}", t)).collect();
    expected.sort();
    assert_eq!(
        committed, expected,
        "every accepted diff reaches the language once"
    );
}

/// A burst of single-link commits through `commit()`, spawned as
/// `spawn_commit_and_handle_error` does, while the loop drains: every link
/// reaches the language exactly once and nothing stays queued.
#[tokio::test(start_paused = true)]
async fn a_burst_through_commit_reaches_the_language_exactly_once() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let task = start_loop(&perspective);

    let burst = targets("burst", 1500);
    let mut commits = Vec::new();
    for (i, t) in burst.iter().enumerate() {
        let p = perspective.clone();
        let diff = add(t);
        commits.push(tokio::spawn(async move { p.commit(&diff).await }));
        // About one link per millisecond, as a client adding links in a loop.
        if i % 2 == 1 {
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
    }
    for c in commits {
        c.await.unwrap().unwrap();
    }
    let drained = run_until(Duration::from_secs(60), || pending(&perspective) == 0).await;
    task.abort();

    assert!(drained.is_some(), "{} still pending", pending(&perspective));
    let mut committed = fake.committed();
    committed.sort();
    let mut expected: Vec<String> = burst.iter().map(|t| format!("+{}", t)).collect();
    expected.sort();
    assert_eq!(
        committed.len(),
        expected.len(),
        "links reaching the language (batches: {:?})",
        fake.batches.lock().unwrap()
    );
    assert_eq!(committed, expected, "every link reaches the language once");
}

#[tokio::test(start_paused = true)]
async fn a_failing_language_is_retried_with_backoff() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    for t in targets("link", 10) {
        enqueue(&perspective, &add(&t));
    }

    let task = start_loop(&perspective);
    tokio::time::sleep(Duration::from_secs(600)).await;
    task.abort();

    let attempts = fake.attempts();
    // 100 ms doubling to a 30 s cap: about 9 attempts in the first minute,
    // then 2 a minute (28 in all). Each attempt makes 3 calls: the batch,
    // its oldest diff alone, the rest. Retrying every 100 ms would be
    // ~18,000 calls; without the cap the wait grows to 51.2 s and gives
    // about 60.
    assert!(
        (75..=180).contains(&attempts),
        "10 simulated minutes against a dead language made {} commit attempts",
        attempts
    );
    assert_eq!(pending(&perspective), 10, "an outage loses nothing");
}

/// The queue of commits a link language did not accept lives in the
/// executor's database, not in the language: a restart (a new instance over
/// the same database, with a new language instance) sends them, oldest
/// first, and does not send again a diff the language accepted before it.
#[tokio::test(start_paused = true)]
async fn a_restart_sends_the_diffs_the_language_did_not_accept() {
    let before = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&before).await;
    perspective.set_immediate_commits(10).await;
    perspective.commit(&add("accepted")).await.unwrap();
    before.down.store(true, Ordering::SeqCst);
    for t in ["first", "second", "third"] {
        perspective.commit(&add(t)).await.unwrap();
    }
    assert_eq!(before.committed(), vec!["+accepted"]);
    assert_eq!(
        queued_targets(&perspective),
        vec!["first", "second", "third"]
    );

    // The restart: nothing of the old instance or its language survives
    // but the database.
    let handle = perspective.persisted.lock().await.clone();
    drop(perspective);
    let after = Arc::new(FakeLanguage::default());
    let restarted = PerspectiveInstance::new(handle, None);
    let language = after.clone();
    *restarted.link_language.write().await = Some(Language::with_test_commit(
        "fake-link-language".to_string(),
        Arc::new(move |diff| language.commit(diff)),
    ));
    let task = start_loop(&restarted);
    let drained = run_until(Duration::from_secs(30), || pending(&restarted) == 0).await;
    task.abort();

    assert!(drained.is_some(), "{} still pending", pending(&restarted));
    assert_eq!(
        after.committed(),
        vec!["+first", "+second", "+third"],
        "the restarted executor sends what was not accepted, in order, and only that"
    );
}

/// A success resets the backoff: a second outage starts again from 100 ms
/// instead of the 30 s the first one reached.
#[tokio::test(start_paused = true)]
async fn the_backoff_starts_again_after_a_success() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("first"));
    let task = start_loop(&perspective);

    tokio::time::sleep(Duration::from_secs(600)).await;
    fake.down.store(false, Ordering::SeqCst);
    run_until(Duration::from_secs(60), || pending(&perspective) == 0)
        .await
        .expect("the queue drains once the language is back");

    fake.down.store(true, Ordering::SeqCst);
    enqueue(&perspective, &add("second"));
    let before = fake.attempts();
    tokio::time::sleep(Duration::from_secs(10)).await;
    task.abort();

    let attempts = fake.attempts() - before;
    assert!(
        attempts >= 4,
        "a second outage got {} attempts in its first 10 s: the backoff did not reset",
        attempts
    );
}

/// A diff the language refuses while it accepts the others is parked (kept,
/// out of the queue), and the diffs queued before and after it still reach
/// the language, in order.
#[tokio::test(start_paused = true)]
async fn a_refused_diff_does_not_block_the_diffs_behind_it() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("before"));
    enqueue(&perspective, &add(REFUSED));
    enqueue(&perspective, &add("after"));
    let task = start_loop(&perspective);

    let drained = run_until(Duration::from_secs(60), || pending(&perspective) == 0).await;
    // A later write is no longer queued behind the refused diff.
    perspective.commit(&add("later")).await.unwrap();
    task.abort();

    assert!(
        drained.is_some(),
        "{} diffs still pending after {} commit attempts",
        pending(&perspective),
        fake.attempts()
    );
    assert_eq!(fake.committed(), vec!["+before", "+after", "+later"]);
    assert_eq!(pending(&perspective), 0);
    assert_eq!(parked_targets(&perspective), vec![REFUSED]);
}

/// A queued diff's links are Shared in the store before the commit loop sends
/// them. The fallback sync leaves them to the queue: sending them too would
/// commit the same links twice.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_leaves_a_queued_diff_to_the_queue() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("queued"));
    store_shared(&perspective, "queued");
    store_shared(&perspective, "missing");
    *perspective.link_language.write().await = Some(rendering_language(&fake, vec![]));

    assert!(perspective.ensure_public_links_are_shared().await);

    assert_eq!(fake.committed(), vec!["+missing"]);
    assert_eq!(pending(&perspective), 1);
}

/// The store reads a link without a predicate back as `None`; a language may
/// render the same link with `""`. They are one link, so the fallback sync
/// does not send it again.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_matches_an_empty_predicate_to_none() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let without = |predicate: Option<&str>| LinkExpression {
        data: Link {
            predicate: predicate.map(str::to_string),
            ..link("did:key:alice", "plain").data
        },
        ..link("did:key:alice", "plain")
    };
    perspective
        .sparql_store
        .add_link(&LinkExpression {
            status: Some(LinkStatus::Shared),
            ..without(None)
        })
        .unwrap();
    *perspective.link_language.write().await =
        Some(rendering_language(&fake, vec![without(Some(""))]));

    assert!(perspective.ensure_public_links_are_shared().await);

    assert!(fake.committed().is_empty(), "{:?}", fake.committed());
}

/// Two links that differ only in a nonempty predicate are two links: the
/// fallback sync sends the one the remote lacks.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_tells_two_predicates_apart() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let with = |predicate: &str| LinkExpression {
        data: Link {
            predicate: Some(predicate.to_string()),
            ..link("did:key:alice", "plain").data
        },
        ..link("did:key:alice", "plain")
    };
    perspective
        .sparql_store
        .add_link(&LinkExpression {
            status: Some(LinkStatus::Shared),
            ..with("ad4m://other")
        })
        .unwrap();
    *perspective.link_language.write().await =
        Some(rendering_language(&fake, vec![with("ad4m://has")]));

    assert!(perspective.ensure_public_links_are_shared().await);

    assert_eq!(fake.committed(), vec!["+plain"]);
}

/// The fallback sync sends the Shared links the remote lacks, but never the
/// links of a parked diff: they stay Shared in the store, and sending them
/// would put them behind the diffs that went through after them. The parked
/// diff failed only transiently, so the language would take it again.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_does_not_send_a_parked_diff_again() {
    let fake = Arc::new(FakeLanguage::default());
    fake.fail_next.store(2, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("a"));
    enqueue(&perspective, &add("b"));
    let task = start_loop(&perspective);
    run_until(Duration::from_secs(60), || pending(&perspective) == 0)
        .await
        .expect("the queue drains");
    task.abort();
    assert_eq!(parked_targets(&perspective), vec!["a"]);

    for target in ["a", "b", "missing"] {
        store_shared(&perspective, target);
    }
    *perspective.link_language.write().await =
        Some(rendering_language(&fake, vec![link("did:key:alice", "b")]));

    assert!(perspective.ensure_public_links_are_shared().await);
    assert_eq!(fake.committed(), vec!["+b", "+missing"]);
}

/// A fallback sync that starts while the commit loop is parking a diff waits
/// for the park: it must not read the parked rows before the park lands and
/// then send the parked link after the diffs that went through behind it.
///
/// The test seam's commit is synchronous, so the rest of the batch blocks a
/// worker thread on a std channel once the language has taken it; the
/// multi-thread runtime keeps the others free.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn the_fallback_sync_waits_for_a_park_in_flight() {
    let fake = Arc::new(FakeLanguage::default());
    // The batch and the oldest diff alone fail; the rest goes through.
    fake.fail_next.store(2, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("a"));
    enqueue(&perspective, &add("b"));
    for target in ["a", "b"] {
        store_shared(&perspective, target);
    }

    let (entered_tx, entered_rx) = std::sync::mpsc::channel::<()>();
    let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
    let entered_tx = std::sync::Mutex::new(entered_tx);
    let release_rx = std::sync::Mutex::new(release_rx);
    let rest_sent = AtomicBool::new(false);
    let in_language = fake.clone();
    *perspective.link_language.write().await = Some(
        Language::with_test_commit(
            "fake-link-language".to_string(),
            Arc::new(move |diff| {
                let result = in_language.commit(diff);
                // Hold the commit loop between the language taking the rest
                // of the batch and the park.
                if result.is_ok() && !rest_sent.swap(true, Ordering::SeqCst) {
                    entered_tx.lock().unwrap().send(()).unwrap();
                    release_rx
                        .lock()
                        .unwrap()
                        .recv_timeout(Duration::from_secs(30))
                        .map_err(|_| anyhow!("never released"))?;
                }
                result
            }),
        )
        .with_test_render(Arc::new(|| {
            Ok(Some(Perspective {
                links: vec![link("did:key:alice", "b")],
            }))
        })),
    );

    let commit_loop = {
        let p = perspective.clone();
        tokio::spawn(async move { p.commit_pending_diffs().await })
    };
    tokio::task::spawn_blocking(move || entered_rx.recv_timeout(Duration::from_secs(10)))
        .await
        .unwrap()
        .expect("the rest of the batch reaches the language");

    let fallback = {
        let p = perspective.clone();
        tokio::spawn(async move { p.ensure_public_links_are_shared().await })
    };
    // Time for a fallback that does not wait for the park to run to its end.
    tokio::time::sleep(Duration::from_millis(500)).await;
    release_tx.send(()).unwrap();
    commit_loop.await.unwrap().unwrap();
    assert!(fallback.await.unwrap());

    assert_eq!(parked_targets(&perspective), vec!["a"]);
    assert_eq!(fake.committed(), vec!["+b"]);
}

/// A fallback sync that starts while a pending batch is on its way to the
/// language waits for it, and then sees its links in the render: the same
/// links are not sent twice.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn the_fallback_sync_waits_for_a_pending_batch_in_flight() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("a"));
    store_shared(&perspective, "a");

    let (entered_tx, entered_rx) = std::sync::mpsc::channel::<()>();
    let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
    let entered_tx = std::sync::Mutex::new(entered_tx);
    let release_rx = std::sync::Mutex::new(release_rx);
    let first = AtomicBool::new(true);
    let in_language = fake.clone();
    let rendered = fake.clone();
    *perspective.link_language.write().await = Some(
        Language::with_test_commit(
            "fake-link-language".to_string(),
            Arc::new(move |diff| {
                // Hold the commit loop's first send before the language takes it.
                if first.swap(false, Ordering::SeqCst) {
                    entered_tx.lock().unwrap().send(()).unwrap();
                    release_rx
                        .lock()
                        .unwrap()
                        .recv_timeout(Duration::from_secs(30))
                        .map_err(|_| anyhow!("never released"))?;
                }
                in_language.commit(diff)
            }),
        )
        // The remote holds what the language accepted so far.
        .with_test_render(Arc::new(move || {
            Ok(Some(Perspective {
                links: rendered
                    .committed()
                    .iter()
                    .filter_map(|c| c.strip_prefix('+'))
                    .map(|target| link("did:key:alice", target))
                    .collect(),
            }))
        })),
    );

    let commit_loop = {
        let p = perspective.clone();
        tokio::spawn(async move { p.commit_pending_diffs().await })
    };
    tokio::task::spawn_blocking(move || entered_rx.recv_timeout(Duration::from_secs(10)))
        .await
        .unwrap()
        .expect("the pending batch is on its way to the language");

    let fallback = {
        let p = perspective.clone();
        tokio::spawn(async move { p.ensure_public_links_are_shared().await })
    };
    // Time for a fallback that does not wait for the batch to send its links.
    tokio::time::sleep(Duration::from_millis(500)).await;
    release_tx.send(()).unwrap();
    commit_loop.await.unwrap().unwrap();
    assert!(fallback.await.unwrap());

    assert_eq!(fake.committed(), vec!["+a"]);
}

/// Two healthy diffs; the batch and the oldest alone fail on a transient
/// outage, and the newer one goes through once it is over. The oldest was
/// never refused for good, so it must be kept (parked), not deleted.
#[tokio::test(start_paused = true)]
async fn a_transient_failure_of_the_oldest_diff_parks_it_instead_of_deleting_it() {
    let fake = Arc::new(FakeLanguage::default());
    fake.fail_next.store(2, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake).await;
    enqueue(&perspective, &add("a"));
    enqueue(&perspective, &add("b"));
    let task = start_loop(&perspective);

    let drained = run_until(Duration::from_secs(60), || pending(&perspective) == 0).await;
    task.abort();

    assert!(
        drained.is_some(),
        "{} diffs still pending",
        pending(&perspective)
    );
    assert_eq!(fake.committed(), vec!["+b"]);
    assert_eq!(parked_targets(&perspective), vec!["a"]);
}

/// A slow link-language commit must not block `set_immediate_commits`, and
/// the burst limit still holds: with one immediate commit allowed and that one
/// in flight, a second commit goes to the pending queue, not to the language.
///
/// The test seam's commit is synchronous, so the slow commit blocks a worker
/// thread on a std channel; the multi-thread runtime keeps the others free.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_slow_commit_does_not_block_set_immediate_commits() {
    use std::sync::mpsc;

    Ad4mDb::init_global_instance(":memory:").unwrap();
    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("slow commit".to_string()),
            shared_url: Some("neighbourhood://test".to_string()),
            neighbourhood: Some(DecoratedNeighbourhoodExpression::default()),
            state: PerspectiveState::Synced,
            owners: None,
        },
        None,
    );
    let (entered_tx, entered_rx) = mpsc::channel::<()>();
    let (release_tx, release_rx) = mpsc::channel::<()>();
    let entered_tx = std::sync::Mutex::new(entered_tx);
    let release_rx = std::sync::Mutex::new(release_rx);
    let calls = Arc::new(AtomicUsize::new(0));
    let calls_in_language = calls.clone();
    *perspective.link_language.write().await = Some(Language::with_test_commit(
        "slow-link-language".to_string(),
        Arc::new(move |_diff| {
            calls_in_language.fetch_add(1, Ordering::SeqCst);
            entered_tx.lock().unwrap().send(()).unwrap();
            release_rx
                .lock()
                .unwrap()
                .recv_timeout(Duration::from_secs(30))
                .map_err(|_| anyhow!("never released"))?;
            Ok(None)
        }),
    ));
    perspective.set_immediate_commits(1).await;

    let slow = {
        let p = perspective.clone();
        tokio::spawn(async move { p.commit(&add("slow")).await })
    };
    tokio::task::spawn_blocking(move || entered_rx.recv_timeout(Duration::from_secs(10)))
        .await
        .unwrap()
        .expect("the first commit reaches the language");

    // A second commit waits for the first to be sent or queued (see
    // a_slow_failed_commit_stays_ahead_of_later_diffs); callers spawn commits.
    let second = {
        let p = perspective.clone();
        tokio::spawn(async move { p.commit(&add("second")).await })
    };
    let reset = tokio::time::timeout(
        Duration::from_secs(2),
        perspective.set_immediate_commits(IMMEDIATE_COMMITS_COUNT),
    )
    .await;
    // Undo the reset so the second commit still finds the burst limit at 0.
    perspective.set_immediate_commits(0).await;

    release_tx.send(()).unwrap();
    slow.await.unwrap().unwrap();
    second.await.unwrap().unwrap();

    reset.expect("set_immediate_commits waited for the slow language call");
    assert_eq!(
        calls.load(Ordering::SeqCst),
        1,
        "the second commit found the burst limit at 0 and skipped the language"
    );
    assert_eq!(pending(&perspective), 1, "the second commit was queued");
}

/// A link language whose first commit blocks until `release` and then fails
/// (a link server that times out), and which records every later commit as
/// `+target` / `-target`.
fn slow_then_failing_language(
    release: std::sync::mpsc::Receiver<()>,
) -> (
    Language,
    std::sync::mpsc::Receiver<()>,
    Arc<std::sync::Mutex<Vec<String>>>,
) {
    let (entered_tx, entered_rx) = std::sync::mpsc::channel::<()>();
    let entered_tx = std::sync::Mutex::new(entered_tx);
    let release = std::sync::Mutex::new(release);
    let first_call = AtomicBool::new(true);
    let seen = Arc::new(std::sync::Mutex::new(Vec::new()));
    let seen_in_language = seen.clone();
    let language = Language::with_test_commit(
        "slow-then-failing-link-language".to_string(),
        Arc::new(move |diff| {
            if first_call.swap(false, Ordering::SeqCst) {
                entered_tx.lock().unwrap().send(()).unwrap();
                let _ = release
                    .lock()
                    .unwrap()
                    .recv_timeout(Duration::from_secs(30));
                return Err(anyhow!("link server timed out"));
            }
            let mut seen = seen_in_language.lock().unwrap();
            seen.extend(diff.additions.iter().map(|l| format!("+{}", l.data.target)));
            seen.extend(diff.removals.iter().map(|l| format!("-{}", l.data.target)));
            Ok(None)
        }),
    );
    (language, entered_rx, seen)
}

fn slow_commit_perspective() -> PerspectiveInstance {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("slow failing commit".to_string()),
            shared_url: Some("neighbourhood://test".to_string()),
            neighbourhood: Some(DecoratedNeighbourhoodExpression::default()),
            state: PerspectiveState::Synced,
            owners: None,
        },
        None,
    )
}

fn queued_targets(perspective: &PerspectiveInstance) -> Vec<String> {
    let (queued, _) =
        Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&perspective.uuid, None)).unwrap();
    queued
        .additions
        .iter()
        .map(|l| l.data.target.clone())
        .collect()
}

/// An immediate commit that is slow and then fails is queued ahead of a diff
/// committed while it was in flight, whether that diff found the burst limit
/// at 0 (budget 1) or had a slot of its own (budget 2): the later diff never
/// reaches the language first.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_slow_failed_commit_stays_ahead_of_later_diffs() {
    for budget in [1, 2] {
        let perspective = slow_commit_perspective();
        let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
        let (language, entered_rx, seen) = slow_then_failing_language(release_rx);
        *perspective.link_language.write().await = Some(language);
        perspective.set_immediate_commits(budget).await;

        let first = {
            let p = perspective.clone();
            tokio::spawn(async move { p.commit(&add("first")).await })
        };
        tokio::task::spawn_blocking(move || entered_rx.recv_timeout(Duration::from_secs(10)))
            .await
            .unwrap()
            .expect("the first commit reaches the language");
        let second = {
            let p = perspective.clone();
            tokio::spawn(async move { p.commit(&add("second")).await })
        };
        tokio::time::sleep(Duration::from_millis(500)).await;
        release_tx.send(()).unwrap();
        first.await.unwrap().unwrap();
        second.await.unwrap().unwrap();

        assert!(
            seen.lock().unwrap().is_empty(),
            "budget {}: the later diff reached the language before the failed one",
            budget
        );
        assert_eq!(
            queued_targets(&perspective),
            vec!["first", "second"],
            "budget {}: pending queue order",
            budget
        );
    }
}

/// End to end through the pending loop: a link added by a slow immediate
/// commit that fails, then removed while that call is in flight, reaches the
/// language as +x before -x. In the other order peers keep x.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_removal_does_not_overtake_a_slow_failed_addition() {
    let perspective = slow_commit_perspective();
    let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
    let (language, entered_rx, seen) = slow_then_failing_language(release_rx);
    *perspective.link_language.write().await = Some(language);
    perspective.set_immediate_commits(1).await;
    let task = start_loop(&perspective);

    let add_x = {
        let p = perspective.clone();
        tokio::spawn(async move { p.commit(&add("x")).await })
    };
    tokio::task::spawn_blocking(move || entered_rx.recv_timeout(Duration::from_secs(10)))
        .await
        .unwrap()
        .expect("the addition reaches the language");
    let remove_x = {
        let p = perspective.clone();
        let diff = PerspectiveDiff {
            additions: vec![],
            removals: vec![link("did:key:alice", "x")],
        };
        tokio::spawn(async move { p.commit(&diff).await })
    };
    // Longer than the loop's 1 s of inactivity: with the removal queued
    // first, the loop sends it alone before the addition fails.
    tokio::time::sleep(Duration::from_millis(1500)).await;
    release_tx.send(()).unwrap();
    add_x.await.unwrap().unwrap();
    remove_x.await.unwrap().unwrap();
    run_until(Duration::from_secs(10), || pending(&perspective) == 0)
        .await
        .expect("the queue drains");
    task.abort();

    assert_eq!(*seen.lock().unwrap(), vec!["+x", "-x"]);
}

/// Store `count` Shared links, targets `{prefix}0..`, and return them.
fn store_many_shared(
    perspective: &PerspectiveInstance,
    prefix: &str,
    count: usize,
) -> Vec<LinkExpression> {
    (0..count)
        .map(|i| {
            let target = format!("{prefix}{i}");
            store_shared(perspective, &target);
            link("did:key:alice", &target)
        })
        .collect()
}

/// The staging case (2026-11-10): the link language's mirror held about 500
/// of the perspective's 12,803 shared links, while the server held them all.
/// Pushing the other 12,303 would only send duplicates (or, from an empty
/// mirror, the whole room again), so the fallback sync commits nothing and
/// reports failure: its backoff tries again later.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_does_not_push_most_of_the_shared_set_again() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let shared = store_many_shared(&perspective, "l", 12_803);
    *perspective.link_language.write().await =
        Some(rendering_language(&fake, shared[..500].to_vec()));

    assert!(!perspective.ensure_public_links_are_shared().await);

    assert_eq!(fake.attempts(), 0, "nothing is committed");
}

/// A catch-up of more than 20 links that is still a minority of the shared
/// set goes through as before.
#[tokio::test(start_paused = true)]
async fn the_fallback_sync_still_sends_a_minority_of_the_shared_set() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let shared = store_many_shared(&perspective, "l", 100);
    *perspective.link_language.write().await =
        Some(rendering_language(&fake, shared[..70].to_vec()));

    assert!(perspective.ensure_public_links_are_shared().await);

    assert_eq!(fake.committed().len(), 30);
}

/// After a publish the creator sends every shared link the new room lacks:
/// the links made before the neighbourhood existed were never sent.
#[tokio::test(start_paused = true)]
async fn the_push_after_publish_sends_the_whole_shared_set() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    store_many_shared(&perspective, "l", 100);
    *perspective.link_language.write().await = Some(rendering_language(&fake, vec![]));

    assert!(perspective.share_links_after_publish().await);

    assert_eq!(fake.committed().len(), 100);
}

/// The push after publish can fail: the language refuses the commit, or does
/// not know the room yet (`render()` null while its first render is stored).
/// The background fallback refuses a push of the whole set, so the creator's
/// links go to the pending-diffs queue, which retries them (across restarts)
/// until the language takes them.
#[tokio::test(start_paused = true)]
async fn a_failed_push_after_publish_leaves_the_shared_set_to_the_queue() {
    for render_known in [true, false] {
        let fake = Arc::new(FakeLanguage::default());
        let perspective = neighbourhood_perspective(&fake).await;
        store_many_shared(&perspective, "l", 100);
        let language = if render_known {
            fake.fail_next.store(1, Ordering::SeqCst);
            rendering_language(&fake, vec![])
        } else {
            let f = fake.clone();
            Language::with_test_commit(
                "fake-link-language".to_string(),
                Arc::new(move |diff| f.commit(diff)),
            )
            .with_test_render(Arc::new(|| Ok(None)))
        };
        *perspective.link_language.write().await = Some(language);

        perspective.share_links_after_publish().await;
        assert!(fake.committed().is_empty(), "render known: {render_known}");
        assert_eq!(pending(&perspective), 1, "render known: {render_known}");

        let task = start_loop(&perspective);
        run_until(Duration::from_secs(60), || pending(&perspective) == 0)
            .await
            .expect("the queue sends the creator's links");
        task.abort();
        assert_eq!(fake.committed().len(), 100, "render known: {render_known}");
    }
}

/// Staging.8 on a neighbourhood published before it: the link language is
/// the old clone, whose `render()` answers its short mirror and is never null.
/// The fallback loop refuses on every pass, backs off to 5 minutes, and
/// neither commits nor queues anything, however long it runs.
#[tokio::test(start_paused = true)]
async fn the_fallback_loop_never_pushes_from_an_old_languages_short_mirror() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let shared = store_many_shared(&perspective, "l", 2_000);
    let renders = Arc::new(AtomicUsize::new(0));
    let (f, r, mirror) = (fake.clone(), renders.clone(), shared[..100].to_vec());
    *perspective.link_language.write().await = Some(
        Language::with_test_commit(
            "fake-link-language".to_string(),
            Arc::new(move |diff| f.commit(diff)),
        )
        .with_test_render(Arc::new(move || {
            r.fetch_add(1, Ordering::SeqCst);
            Ok(Some(Perspective {
                links: mirror.clone(),
            }))
        })),
    );

    let looping = perspective.clone();
    let task = tokio::spawn(async move { looping.fallback_sync_loop().await });
    tokio::time::sleep(Duration::from_secs(20 * 60)).await;
    perspective.is_teardown.store(true, Ordering::Release);
    task.await.unwrap();

    assert_eq!(fake.attempts(), 0, "nothing is committed");
    assert_eq!(pending(&perspective), 0, "nothing is queued");
    // Passes at 0, 60, 180, 420, 720 and 1020 s: the backoff doubles to 5 minutes.
    assert_eq!(renders.load(Ordering::SeqCst), 6);
    assert_eq!(
        *perspective.fallback_sync_interval.lock().await,
        Duration::from_secs(300)
    );
}

/// A failed push after publish queues the shared links no queued or parked
/// diff holds: a link already waiting in the queue is not queued twice, so
/// the queue sends it once.
#[tokio::test(start_paused = true)]
async fn a_failed_push_after_publish_does_not_queue_a_queued_link_again() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    store_many_shared(&perspective, "l", 30);
    store_shared(&perspective, "queued");
    enqueue(&perspective, &add("queued"));
    fake.fail_next.store(1, Ordering::SeqCst);
    *perspective.link_language.write().await = Some(rendering_language(&fake, vec![]));

    assert!(perspective.share_links_after_publish().await);

    let mut queued = queued_targets(&perspective);
    queued.sort();
    assert_eq!(queued.len(), 31, "{queued:?}");
    assert_eq!(queued.iter().filter(|t| *t == "queued").count(), 1);
}

/// An executor that was offline wrote most of its shared set while away:
/// those commits failed and wait in the queue. The fallback leaves queued
/// links to the queue, so it neither refuses nor sends them, and the queue
/// delivers all of them.
#[tokio::test(start_paused = true)]
async fn an_offline_backlog_larger_than_half_the_shared_set_goes_through_the_queue() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake).await;
    let on_server = store_many_shared(&perspective, "s", 20);
    for target in targets("offline", 80) {
        store_shared(&perspective, &target);
        enqueue(&perspective, &add(&target));
    }
    *perspective.link_language.write().await = Some(rendering_language(&fake, on_server));

    assert!(
        perspective.ensure_public_links_are_shared().await,
        "not refused"
    );
    assert_eq!(
        fake.attempts(),
        0,
        "the fallback leaves the backlog to the queue"
    );

    let task = start_loop(&perspective);
    run_until(Duration::from_secs(600), || pending(&perspective) == 0)
        .await
        .expect("the queue drains the backlog");
    task.abort();
    assert_eq!(fake.committed().len(), 80);
}
