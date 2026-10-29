//! The pending-diffs commit queue (`pending_diffs_loop`) against a scripted
//! link language, on tokio's paused clock.
//!
//! The fake language rejects every commit while it is "down" (a dead link
//! server), and while up refuses any diff that adds a link to `REFUSED`.
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
    committed: std::sync::Mutex<Vec<String>>,
    /// Links per accepted commit, in order.
    batches: std::sync::Mutex<Vec<usize>>,
}

impl FakeLanguage {
    fn commit(&self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        self.attempts.fetch_add(1, Ordering::SeqCst);
        if self.down.load(Ordering::SeqCst) {
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

/// Diffs still waiting to be committed.
fn pending(perspective: &PerspectiveInstance) -> usize {
    Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&perspective.uuid, None))
        .unwrap()
        .1
        .len()
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

/// A diff the language refuses while it accepts the others is dropped, and
/// the diffs queued before and after it still reach the language, in order.
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
