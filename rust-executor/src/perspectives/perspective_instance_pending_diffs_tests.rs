//! The pending-diffs commit queue (`pending_diffs_loop`) against a scripted
//! link language, on tokio's paused clock.
//!
//! The fake language rejects every commit while it is "down" (a dead link
//! server). Accepted links are recorded in order as `+target` / `-target`.

use super::*;
use crate::db::Ad4mDb;
use crate::types::{DecoratedNeighbourhoodExpression, ExpressionProof, Link, PerspectiveState};
use std::sync::atomic::AtomicUsize;

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
    // then 2 a minute (28 in all). Retrying every 100 ms would be ~6,000;
    // without the cap the wait grows to 51.2 s and gives about 20.
    assert!(
        (25..=60).contains(&attempts),
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
