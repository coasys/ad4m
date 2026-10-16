//! The pending-diffs commit queue (`pending_diffs_loop`) against a scripted
//! link language, on tokio's paused clock.
//!
//! The fake language rejects every commit while it is "down" (a dead link
//! server) and, while up, rejects any commit holding a link authored by
//! `mallory` (a link server's permanent 4xx: removing another agent's link,
//! a plaintext link in an E2E room). Accepted links are recorded in order as
//! `+target` / `-target`.

use super::*;
use crate::db::Ad4mDb;
use crate::types::{DecoratedNeighbourhoodExpression, ExpressionProof, Link, PerspectiveState};
use std::sync::atomic::AtomicUsize;

const REJECTED_AUTHOR: &str = "did:key:mallory";

#[derive(Default)]
struct FakeLanguage {
    attempts: AtomicUsize,
    down: AtomicBool,
    /// Answer every commit holding `throttled_target` with the link server's 429.
    throttled_target: std::sync::Mutex<Option<String>>,
    committed: std::sync::Mutex<Vec<String>>,
}

impl FakeLanguage {
    fn commit(&self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        self.attempts.fetch_add(1, Ordering::SeqCst);
        if self.down.load(Ordering::SeqCst) {
            return Err(anyhow!("link server unreachable"));
        }
        if let Some(target) = self.throttled_target.lock().unwrap().as_ref() {
            if diff.additions.iter().any(|l| &l.data.target == target) {
                return Err(anyhow!(
                    r#"ApiError: {{"error":"rate limit exceeded","retryAfterSec":12}}"#
                ));
            }
        }
        if diff
            .additions
            .iter()
            .chain(diff.removals.iter())
            .any(|l| l.author == REJECTED_AUTHOR)
        {
            return Err(anyhow!(
                "400: cannot remove a link authored by another agent"
            ));
        }
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

fn remove(target: &str) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: vec![],
        removals: vec![link("did:key:alice", target)],
    }
}

fn rejected(target: &str) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: vec![],
        removals: vec![link(REJECTED_AUTHOR, target)],
    }
}

/// A neighbourhood perspective whose link language is `fake`.
fn neighbourhood_perspective(fake: &Arc<FakeLanguage>) -> PerspectiveInstance {
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
    perspective.set_test_link_commit(Arc::new(move |diff| fake.commit(diff)));
    perspective
}

fn enqueue(perspective: &PerspectiveInstance, diff: &PerspectiveDiff) {
    Ad4mDb::with_global_instance(|db| db.add_pending_diff(&perspective.uuid, diff)).unwrap();
}

/// Diffs still waiting to be committed (not counting ones set aside).
fn pending(perspective: &PerspectiveInstance) -> usize {
    Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&perspective.uuid, None))
        .unwrap()
        .1
        .len()
}

/// Diffs set aside after the language kept rejecting them, with the error.
fn set_aside(perspective: &PerspectiveInstance) -> Vec<(PerspectiveDiff, String)> {
    Ad4mDb::with_global_instance(|db| db.get_failed_pending_diffs(&perspective.uuid))
        .unwrap()
        .into_iter()
        .map(|(_, diff, error)| (diff, error))
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
async fn a_rejected_diff_at_the_head_does_not_block_the_diffs_behind_it() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake);
    enqueue(&perspective, &rejected("someone-elses-link"));
    let later = targets("ok", 20);
    for t in &later {
        enqueue(&perspective, &add(t));
    }

    let task = start_loop(&perspective);
    let committed_all = run_until(Duration::from_secs(120), || {
        fake.committed().len() == later.len()
    })
    .await;
    task.abort();

    let expected: Vec<String> = later.iter().map(|t| format!("+{}", t)).collect();
    assert_eq!(
        fake.committed(),
        expected,
        "the diffs behind a rejected one must reach the language, in order \
         (attempts: {})",
        fake.attempts()
    );
    assert!(committed_all.is_some());

    // Once the language has accepted the rest, the rejected diff is set
    // aside, kept with the language's error, and the queue is empty.
    let task = start_loop(&perspective);
    let emptied = run_until(Duration::from_secs(60), || pending(&perspective) == 0).await;
    task.abort();
    assert!(emptied.is_some(), "the rejected diff leaves the queue");
    let aside = set_aside(&perspective);
    assert_eq!(aside.len(), 1);
    assert_eq!(aside[0].0, rejected("someone-elses-link"));
    assert!(
        aside[0].1.contains("authored by another agent"),
        "{}",
        aside[0].1
    );
}

#[tokio::test(start_paused = true)]
async fn a_large_queue_drains_once_the_language_recovers() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake);
    let good = targets("link", 600);
    for (i, t) in good.iter().enumerate() {
        if i == 300 {
            enqueue(&perspective, &rejected("someone-elses-link"));
        }
        enqueue(&perspective, &add(t));
    }

    let task = start_loop(&perspective);
    // A minute of outage: nothing gets through, nothing is lost.
    tokio::time::sleep(Duration::from_secs(60)).await;
    assert!(fake.committed().is_empty());
    assert_eq!(pending(&perspective), 601);

    fake.down.store(false, Ordering::SeqCst);
    let drained = run_until(Duration::from_secs(120), || pending(&perspective) == 0).await;
    task.abort();

    assert!(
        drained.is_some(),
        "601 queued diffs must drain within 120 s of the language recovering; \
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
    let aside = set_aside(&perspective);
    assert_eq!(
        aside.len(),
        1,
        "only the rejected diff is set aside, none of the outage's"
    );
    assert_eq!(aside[0].0, rejected("someone-elses-link"));
}

#[tokio::test(start_paused = true)]
async fn a_failing_language_is_retried_with_backoff() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake);
    for t in targets("link", 10) {
        enqueue(&perspective, &add(&t));
    }

    let task = start_loop(&perspective);
    tokio::time::sleep(Duration::from_secs(600)).await;
    task.abort();

    let attempts = fake.attempts();
    // 100 ms doubling to a 30 s cap: about 9 attempts in the first minute,
    // then 2 a minute. Retrying every 100 ms would be ~6,000.
    assert!(
        (10..=60).contains(&attempts),
        "10 simulated minutes against a dead language made {} commit attempts",
        attempts
    );
    assert_eq!(pending(&perspective), 10, "an outage sets nothing aside");
    assert!(set_aside(&perspective).is_empty());
}

/// A rate-limited commit (the link server's 429) is the server pacing us, not a
/// bad diff: it is retried with backoff and never set aside, even while other
/// diffs go through.
#[tokio::test(start_paused = true)]
async fn a_rate_limited_diff_is_never_set_aside() {
    let fake = Arc::new(FakeLanguage::default());
    *fake.throttled_target.lock().unwrap() = Some("throttled".to_string());
    let perspective = neighbourhood_perspective(&fake);
    enqueue(&perspective, &add("throttled"));
    for target in targets("later", 20) {
        enqueue(&perspective, &add(&target));
    }
    let handle = start_loop(&perspective);

    run_until(Duration::from_secs(300), || false).await;
    assert!(
        set_aside(&perspective).is_empty(),
        "a 429 set a diff aside: {:?}",
        set_aside(&perspective)
    );

    // Once the server stops throttling, it goes through.
    *fake.throttled_target.lock().unwrap() = None;
    let drained = run_until(Duration::from_secs(120), || pending(&perspective) == 0).await;
    assert!(drained.is_some(), "{} still pending", pending(&perspective));
    assert!(fake.committed().contains(&"+throttled".to_string()));
    handle.abort();
}

#[tokio::test(start_paused = true)]
async fn a_diff_touching_a_rejected_diffs_link_waits_behind_it() {
    let fake = Arc::new(FakeLanguage::default());
    let perspective = neighbourhood_perspective(&fake);
    // One diff the language rejects that also adds y; then a removal of y
    // (must not overtake the add) and an unrelated z (free to go ahead).
    let mut bad = rejected("someone-elses-link");
    bad.additions.push(link("did:key:alice", "y"));
    enqueue(&perspective, &bad);
    enqueue(&perspective, &remove("y"));
    enqueue(&perspective, &add("z"));

    let task = start_loop(&perspective);
    let drained = run_until(Duration::from_secs(120), || pending(&perspective) == 0).await;
    task.abort();

    assert!(drained.is_some(), "{} still pending", pending(&perspective));
    assert_eq!(
        fake.committed(),
        vec!["+z", "-y"],
        "-y waits until the diff adding y is set aside; z goes ahead"
    );
    assert_eq!(set_aside(&perspective).len(), 1);
}

#[tokio::test(start_paused = true)]
async fn queued_commits_keep_their_order() {
    let fake = Arc::new(FakeLanguage::default());
    fake.down.store(true, Ordering::SeqCst);
    let perspective = neighbourhood_perspective(&fake);

    // Through `commit`: the first fails and is queued; every later one queues
    // behind it even after the language is back.
    perspective.commit(&add("x")).await.unwrap();
    fake.down.store(false, Ordering::SeqCst);
    perspective.commit(&remove("x")).await.unwrap();
    perspective.commit(&add("x")).await.unwrap();
    assert!(
        fake.committed().is_empty(),
        "a new commit must not overtake the queue"
    );
    assert_eq!(pending(&perspective), 3);

    let task = start_loop(&perspective);
    let drained = run_until(Duration::from_secs(30), || pending(&perspective) == 0).await;
    task.abort();
    assert!(drained.is_some());
    assert_eq!(fake.committed(), vec!["+x", "-x", "+x"]);
}
