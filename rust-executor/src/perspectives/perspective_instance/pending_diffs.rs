//! The pending-diffs commit queue of a neighbourhood perspective.
//!
//! `commit()` sends a diff straight to the link language only while nothing
//! is queued; otherwise (and when that commit fails) the diff is appended to
//! `perspective_diff`. `pending_diffs_loop` drains the queue in id order:
//!
//! - The first batch of a burst waits for a quiet second, 3 s since the burst
//!   began, or a full batch. After a success, and while retrying, the next
//!   batch goes on the next tick, so new arrivals never starve the drain.
//! - A batch never holds two diffs that touch the same link: the language
//!   gets one merged diff (all additions, then all removals), which would
//!   reorder an add and a remove of that link.
//! - A failed batch backs off (100 ms doubling to 30 s, reset on success) and
//!   is logged once per failure. After [`ATTEMPTS`] failures it is bisected
//!   to find the diff the language rejects.
//! - A single diff rejected [`ATTEMPTS`] times becomes a *suspect*: diffs
//!   behind it that share no link with it (or with a diff held behind it) go
//!   ahead, so one bad diff cannot block the queue and an add/remove of the
//!   same link never reorder. A suspect that fails again right after the
//!   language accepted another commit is set aside: its row keeps the data,
//!   with `failed_at`/`last_error` set, and a warning is logged. While the
//!   language accepts nothing (an outage) nothing is set aside.

use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::time::Duration;

use deno_core::error::AnyError;
use tokio::time::{self, Instant};

use super::{
    PerspectiveInstance, IMMEDIATE_COMMITS_COUNT, MAX_COMMIT_BYTES, MAX_PENDING_DIFFS_COUNT,
    MAX_PENDING_SECONDS,
};
use crate::db::Ad4mDb;
use crate::types::PerspectiveDiff;

/// Test stand-in for `Language::commit`.
#[cfg(test)]
pub(crate) type TestLinkCommit =
    std::sync::Arc<dyn Fn(PerspectiveDiff) -> Result<Option<String>, AnyError> + Send + Sync>;

const TICK: Duration = Duration::from_millis(100);
const BACKOFF_BASE: Duration = Duration::from_millis(100);
const BACKOFF_CAP: Duration = Duration::from_secs(30);
/// Failures of a full batch before it is bisected, and of a single diff
/// before it becomes a suspect.
const ATTEMPTS: u32 = 3;
/// Suspects held aside at once. Past this the queue waits at the backoff cap
/// rather than probing ever deeper during an outage.
const MAX_SUSPECTS: usize = 32;
/// Queue lengths logged when crossed upwards.
const QUEUE_THRESHOLDS: [usize; 3] = [100, 1_000, 10_000];

/// Identity of a link inside a diff: a removal names the LinkExpression it
/// removes, so two diffs touching the same key must commit in order.
type LinkKey = (String, String, String, Option<String>, String);

/// Error texts that mean "not now", not "not this diff": the link server
/// pacing us (its 429), or not reachable. They only back off; they never count
/// toward setting a diff aside, so a good diff is never set aside because the
/// server was busy or away.
const TRANSIENT_ERRORS: &[&str] = &[
    "rate limit",
    "too many requests",
    "429",
    "unreachable",
    "timed out",
    "timeout",
    "connection refused",
    "connection reset",
    "error sending request",
    "fetch failed",
    "network",
];

fn is_transient(error: &AnyError) -> bool {
    let text = format!("{:#}", error).to_lowercase();
    TRANSIENT_ERRORS.iter().any(|needle| text.contains(needle))
}

fn link_keys(diff: &PerspectiveDiff) -> HashSet<LinkKey> {
    diff.additions
        .iter()
        .chain(diff.removals.iter())
        .map(|l| {
            (
                l.author.clone(),
                l.timestamp.clone(),
                l.data.source.clone(),
                l.data.predicate.clone(),
                l.data.target.clone(),
            )
        })
        .collect()
}

fn merge(diffs: impl IntoIterator<Item = PerspectiveDiff>) -> PerspectiveDiff {
    let mut merged = PerspectiveDiff::empty();
    for diff in diffs {
        merged.additions.extend(diff.additions);
        merged.removals.extend(diff.removals);
    }
    merged
}

/// Delay before retry number `step` (0-based) of a failed commit.
fn backoff_delay(step: u32) -> Duration {
    BACKOFF_BASE
        .checked_mul(1u32.checked_shl(step).unwrap_or(u32::MAX))
        .map_or(BACKOFF_CAP, |d| d.min(BACKOFF_CAP))
}

struct Suspect {
    id: u64,
    diff: PerspectiveDiff,
    keys: HashSet<LinkKey>,
    failures: u32,
    /// The language accepted another commit since this one last failed.
    success_since_failure: bool,
}

/// The loop's per-perspective state. In memory only: after a restart the
/// queue is still in the table and isolation starts over.
struct PendingQueue {
    suspects: Vec<Suspect>,
    batch_limit: usize,
    batch_failures: u32,
    backoff_step: u32,
    retry_at: Option<Instant>,
    /// The last attempt succeeded and diffs remain: commit on the next tick.
    draining: bool,
    burst_started: Option<Instant>,
    reported_threshold: usize,
    peak: usize,
}

impl PendingQueue {
    fn new() -> Self {
        Self {
            suspects: Vec::new(),
            batch_limit: MAX_PENDING_DIFFS_COUNT,
            batch_failures: 0,
            backoff_step: 0,
            retry_at: None,
            draining: false,
            burst_started: None,
            reported_threshold: 0,
            peak: 0,
        }
    }

    fn retrying(&self) -> bool {
        self.retry_at.is_some() || self.batch_failures > 0 || !self.suspects.is_empty()
    }

    fn report_length(&mut self, uuid: &str, count: usize) {
        self.peak = self.peak.max(count);
        for threshold in QUEUE_THRESHOLDS {
            if count >= threshold && self.reported_threshold < threshold {
                self.reported_threshold = threshold;
                log::warn!(
                    "Perspective {}: {} diffs waiting to be committed to the link language",
                    uuid,
                    count
                );
            }
        }
        if count == 0 && self.peak > 0 {
            if self.peak >= QUEUE_THRESHOLDS[0] {
                log::info!(
                    "Perspective {}: pending diffs drained to 0 (peak {})",
                    uuid,
                    self.peak
                );
            } else {
                log::debug!(
                    "Perspective {}: pending diffs drained to 0 (peak {})",
                    uuid,
                    self.peak
                );
            }
            self.peak = 0;
            self.reported_threshold = 0;
        }
    }

    fn on_empty(&mut self) {
        self.suspects.clear();
        self.batch_limit = MAX_PENDING_DIFFS_COUNT;
        self.batch_failures = 0;
        self.draining = false;
        self.burst_started = None;
    }

    fn on_success(&mut self) {
        self.batch_limit = MAX_PENDING_DIFFS_COUNT;
        self.batch_failures = 0;
        self.backoff_step = 0;
        self.retry_at = None;
        self.draining = true;
        self.burst_started = None;
        for suspect in &mut self.suspects {
            suspect.success_since_failure = true;
        }
    }

    fn schedule_retry(&mut self) -> Duration {
        let delay = backoff_delay(self.backoff_step);
        self.backoff_step = self.backoff_step.saturating_add(1);
        self.retry_at = Some(Instant::now() + delay);
        self.draining = false;
        delay
    }

    /// The next batch: pending rows in id order, minus every row that shares
    /// a link with a suspect or with a row held before it, ending before a
    /// row that shares a link with the batch itself (see the module doc).
    fn select_batch(&self, rows: Vec<(u64, PerspectiveDiff)>) -> Vec<(u64, PerspectiveDiff)> {
        let mut held: HashSet<LinkKey> = self
            .suspects
            .iter()
            .flat_map(|s| s.keys.iter().cloned())
            .collect();
        let mut in_batch: HashSet<LinkKey> = HashSet::new();
        let mut batch = Vec::new();
        let mut bytes = 0usize;
        for (id, diff) in rows {
            if batch.len() >= self.batch_limit {
                break;
            }
            let keys = link_keys(&diff);
            if !held.is_disjoint(&keys) {
                held.extend(keys);
                continue;
            }
            if !in_batch.is_disjoint(&keys) {
                break;
            }
            let size = serde_json::to_string(&diff).map(|s| s.len()).unwrap_or(0);
            if !batch.is_empty() && bytes + size > MAX_COMMIT_BYTES {
                break;
            }
            bytes += size;
            in_batch.extend(keys);
            batch.push((id, diff));
        }
        batch
    }
}

impl PerspectiveInstance {
    /// Test seam: route every link-language commit to `commit` instead of a
    /// Deno runtime, and report a link language as present.
    #[cfg(test)]
    pub(crate) fn set_test_link_commit(&self, commit: TestLinkCommit) {
        *self.test_link_commit.lock().unwrap() = Some(commit);
    }

    pub(super) async fn has_link_language(&self) -> bool {
        #[cfg(test)]
        if self.test_link_commit.lock().unwrap().is_some() {
            return true;
        }
        self.link_language.read().await.is_some()
    }

    /// Commit `diff` to the link language. `None` when there is none.
    pub(super) async fn link_language_commit(
        &self,
        diff: PerspectiveDiff,
    ) -> Option<Result<Option<String>, AnyError>> {
        #[cfg(test)]
        {
            let hook = self.test_link_commit.lock().unwrap().clone();
            if let Some(hook) = hook {
                return Some(hook(diff));
            }
        }
        let link_language = self.link_language.read().await.clone();
        match link_language {
            Some(mut link_language) => Some(link_language.commit(diff).await),
            None => None,
        }
    }

    async fn has_new_diffs_in_last_second(&self) -> bool {
        let timer = self.commit_debounce_timer.lock().await;
        timer
            .map(|instant| instant.elapsed() < Duration::from_secs(1))
            .unwrap_or(false)
    }

    pub(super) async fn pending_diffs_loop(&self) {
        let mut queue = PendingQueue::new();
        let mut interval = time::interval(TICK);

        while !self.is_teardown.load(Ordering::Acquire) {
            interval.tick().await;
            if !self.has_link_language().await {
                continue;
            }

            let count = match Ad4mDb::with_global_instance(|db| db.count_pending_diffs(&self.uuid))
            {
                Ok(count) => count,
                Err(e) => {
                    log::error!("Perspective {}: counting pending diffs: {}", self.uuid, e);
                    continue;
                }
            };
            queue.report_length(&self.uuid, count);
            if count == 0 {
                queue.on_empty();
                continue;
            }

            let now = Instant::now();
            if queue.retry_at.is_some_and(|at| now < at) {
                continue;
            }
            if !(queue.draining || queue.retrying()) {
                // First batch of a burst: let it collect.
                let started = *queue.burst_started.get_or_insert(now);
                let ready = now.duration_since(started) >= Duration::from_secs(MAX_PENDING_SECONDS)
                    || !self.has_new_diffs_in_last_second().await
                    || count >= MAX_PENDING_DIFFS_COUNT;
                if !ready {
                    continue;
                }
            }

            self.commit_next(&mut queue).await;
        }
    }

    /// One commit attempt: a suspect due for a retry, else the next batch,
    /// else (everything left is held) the oldest suspect.
    async fn commit_next(&self, queue: &mut PendingQueue) {
        if let Some(index) = queue.suspects.iter().position(|s| s.success_since_failure) {
            self.retry_suspect(queue, index).await;
            return;
        }

        let suspect_ids: Vec<u64> = queue.suspects.iter().map(|s| s.id).collect();
        let window = MAX_PENDING_DIFFS_COUNT * 2;
        let rows = match Ad4mDb::with_global_instance(|db| {
            db.get_pending_diff_rows(&self.uuid, &suspect_ids, Some(window))
        }) {
            Ok(rows) => rows,
            Err(e) => {
                log::error!("Perspective {}: reading pending diffs: {}", self.uuid, e);
                return;
            }
        };
        let batch = queue.select_batch(rows);
        if batch.is_empty() {
            if !queue.suspects.is_empty() {
                self.retry_suspect(queue, 0).await;
            }
            return;
        }

        let ids: Vec<u64> = batch.iter().map(|(id, _)| *id).collect();
        let head = batch[0].clone();
        let diff = merge(batch.into_iter().map(|(_, d)| d));
        log::debug!(
            "Perspective {}: committing {} pending diffs",
            self.uuid,
            ids.len()
        );
        // Spec §5.2: a commit-only language returns Ok(None). Success is
        // "commit didn't throw", with or without a revision.
        match self.link_language_commit(diff).await {
            None => {}
            Some(Ok(_)) => self.clear_committed(queue, ids).await,
            Some(Err(e)) => self.on_batch_failure(queue, ids.len(), head, e),
        }
    }

    async fn clear_committed(&self, queue: &mut PendingQueue, ids: Vec<u64>) {
        let n = ids.len();
        if let Err(e) = Ad4mDb::with_global_instance(|db| db.clear_pending_diffs(&self.uuid, ids)) {
            log::error!(
                "Perspective {}: committed {} pending diffs but could not clear them: {}",
                self.uuid,
                n,
                e
            );
        }
        queue.on_success();
        self.set_immediate_commits(IMMEDIATE_COMMITS_COUNT).await;
        log::debug!("Perspective {}: committed {} pending diffs", self.uuid, n);
    }

    fn on_batch_failure(
        &self,
        queue: &mut PendingQueue,
        len: usize,
        head: (u64, PerspectiveDiff),
        error: AnyError,
    ) {
        queue.draining = false;
        if is_transient(&error) {
            let delay = queue.schedule_retry();
            log::warn!(
                "Perspective {}: committing {} pending diffs failed for now: {:#}; retrying in {:?}",
                self.uuid,
                len,
                error,
                delay
            );
            return;
        }
        queue.batch_failures += 1;
        let bisecting = queue.batch_limit < MAX_PENDING_DIFFS_COUNT;
        if len > 1 && (bisecting || queue.batch_failures >= ATTEMPTS) {
            // Halve and retry on the next tick: isolating the rejected diff
            // takes log2(batch) attempts, not a backoff each.
            queue.batch_limit = (len / 2).max(1);
            queue.batch_failures = 0;
            log::warn!(
                "Perspective {}: link language rejected a batch of {} pending diffs: {:#}; \
                 retrying the first {} alone",
                self.uuid,
                len,
                error,
                queue.batch_limit
            );
            return;
        }
        if len == 1 && queue.batch_failures >= ATTEMPTS && queue.suspects.len() < MAX_SUSPECTS {
            let (id, diff) = head;
            queue.suspects.push(Suspect {
                id,
                keys: link_keys(&diff),
                diff,
                failures: queue.batch_failures,
                success_since_failure: false,
            });
            queue.batch_failures = 0;
            let delay = queue.schedule_retry();
            log::warn!(
                "Perspective {}: link language rejected pending diff {} {} times: {:#}; \
                 committing the diffs behind it meanwhile (next attempt in {:?})",
                self.uuid,
                id,
                ATTEMPTS,
                error,
                delay
            );
            return;
        }
        let delay = queue.schedule_retry();
        log::warn!(
            "Perspective {}: committing {} pending diffs failed: {:#}; retrying in {:?}",
            self.uuid,
            len,
            error,
            delay
        );
    }

    async fn retry_suspect(&self, queue: &mut PendingQueue, index: usize) {
        let id = queue.suspects[index].id;
        let still_pending =
            Ad4mDb::with_global_instance(|db| db.is_pending_diff(&self.uuid, id)).unwrap_or(false);
        if !still_pending {
            queue.suspects.remove(index);
            return;
        }

        let diff = queue.suspects[index].diff.clone();
        let error = match self.link_language_commit(diff).await {
            None => return,
            Some(Ok(_)) => {
                queue.suspects.remove(index);
                self.clear_committed(queue, vec![id]).await;
                return;
            }
            Some(Err(error)) => error,
        };

        if is_transient(&error) {
            queue.suspects[index].success_since_failure = false;
            let delay = queue.schedule_retry();
            log::warn!(
                "Perspective {}: pending diff {} failed for now: {:#}; retrying in {:?}",
                self.uuid,
                id,
                error,
                delay
            );
            return;
        }
        let suspect = &mut queue.suspects[index];
        suspect.failures += 1;
        let failures = suspect.failures;
        let confirmed = suspect.success_since_failure;
        suspect.success_since_failure = false;
        if !confirmed {
            let delay = queue.schedule_retry();
            log::warn!(
                "Perspective {}: pending diff {} rejected again: {:#}; retrying in {:?}",
                self.uuid,
                id,
                error,
                delay
            );
            return;
        }

        // Rejected right after the language accepted another commit: it is
        // this diff, not the language. Set it aside; the next tick goes on.
        queue.suspects.remove(index);
        let message = format!("{:#}", error);
        match Ad4mDb::with_global_instance(|db| {
            db.set_pending_diff_failed(&self.uuid, id, &message)
        }) {
            Ok(()) => log::warn!(
                "Perspective {}: set aside pending diff {} after {} rejections while the link \
                 language accepted other diffs: {}. Its links stay local; the row stays in \
                 perspective_diff with failed_at set",
                self.uuid,
                id,
                failures,
                message
            ),
            Err(e) => log::error!(
                "Perspective {}: could not set aside pending diff {}: {}",
                self.uuid,
                id,
                e
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backoff_doubles_from_100ms_to_a_30s_cap() {
        assert_eq!(backoff_delay(0), Duration::from_millis(100));
        assert_eq!(backoff_delay(1), Duration::from_millis(200));
        assert_eq!(backoff_delay(8), Duration::from_millis(25_600));
        assert_eq!(backoff_delay(9), BACKOFF_CAP);
        assert_eq!(backoff_delay(40), BACKOFF_CAP);
    }
}
