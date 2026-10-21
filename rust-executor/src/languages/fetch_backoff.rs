//! The bounded retry `install_language` uses when the language language does
//! not have a language's source or meta yet: a language another node has just
//! published takes a while to propagate through the language language's store.

use log::warn;
use std::fmt::Display;
use std::future::Future;
use std::time::Duration;

/// How long `install_language` keeps asking the language language for a
/// language's source: long enough for a language another node has just
/// published to propagate through the language language's store.
pub(super) const SOURCE_FETCH_DEADLINE: Duration = Duration::from_secs(30);
/// How long `install_language` keeps asking for a language's meta. The meta is
/// optional (it is only stored beside the bundle), so it gets less.
pub(super) const META_FETCH_DEADLINE: Duration = Duration::from_secs(15);
/// The waits between attempts; the last repeats.
const FETCH_BACKOFF: [Duration; 5] = [
    Duration::from_millis(500),
    Duration::from_secs(1),
    Duration::from_secs(2),
    Duration::from_secs(4),
    Duration::from_secs(8),
];

/// Call `attempt` until it yields a value, backing off between attempts, and
/// give up (`None`) once `deadline` has passed. Never waits when the next
/// attempt would start after the deadline, and cuts an attempt off at it.
pub(super) async fn fetch_with_backoff<T, E, F, Fut>(
    what: &str,
    deadline: Duration,
    mut attempt: F,
) -> Option<T>
where
    E: Display,
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<Option<T>, E>>,
{
    let give_up_at = tokio::time::Instant::now() + deadline;
    for n in 0.. {
        match tokio::time::timeout_at(give_up_at, attempt()).await {
            Ok(Ok(Some(value))) => return Some(value),
            Ok(Ok(None)) => {}
            Ok(Err(e)) => warn!("Fetching {} failed (attempt {}): {}", what, n + 1, e),
            Err(_) => {
                warn!("Fetching {}: gave up after {:?}", what, deadline);
                return None;
            }
        }
        let wait = FETCH_BACKOFF[n.min(FETCH_BACKOFF.len() - 1)];
        if tokio::time::Instant::now() + wait >= give_up_at {
            warn!("Fetching {}: not available after {} attempts", what, n + 1);
            return None;
        }
        tokio::time::sleep(wait).await;
    }
    unreachable!("the attempt loop only ends by returning")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::languages::language_runtime::{LanguageOperation, LanguageRuntimeRequest};
    use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
    use crate::languages::LanguageController;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use tokio::sync::mpsc::UnboundedReceiver;
    use uuid::Uuid;

    /// Requests a fake language language has received, by kind.
    #[derive(Default)]
    struct Counts {
        meta: AtomicUsize,
        source: AtomicUsize,
    }

    /// Answer `requests` as a language language that has no language at all.
    fn serve_empty_language_language(
        mut requests: UnboundedReceiver<LanguageRuntimeRequest>,
    ) -> Arc<Counts> {
        let counts = Arc::new(Counts::default());
        let counting = counts.clone();
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                if let LanguageOperation::Execute(script, _) = &request.operation {
                    if script.contains("expressionGet(") {
                        counting.meta.fetch_add(1, Ordering::SeqCst);
                    } else if script.contains("languageGetSource(") {
                        counting.source.fetch_add(1, Ordering::SeqCst);
                    }
                }
                let _ = request.response_tx.send(Ok("null".to_string()));
            }
        });
        counts
    }

    /// Serve an empty fake language language from `controller` as its language
    /// language; returns its address and the previous one.
    async fn install_fake_language_language(
        controller: &LanguageController,
    ) -> (String, Option<String>, Arc<Counts>) {
        let address = format!("QmFakeLanguageLanguage{}", Uuid::new_v4().simple());
        let (tx, requests) = tokio::sync::mpsc::unbounded_channel();
        controller
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        let previous = controller
            .system_addresses
            .lock()
            .await
            .language_language
            .replace(address.clone());
        (address, previous, serve_empty_language_language(requests))
    }

    /// Attempts at 0, 0.5, 1.5, 3.5 and 7.5 s; the next would start at 15.5 s,
    /// past the 15 s deadline, so it gives up at 7.5 s without waiting.
    #[tokio::test(start_paused = true)]
    async fn fetching_backs_off_and_does_not_wait_after_its_last_attempt() {
        let start = tokio::time::Instant::now();
        let mut attempts = Vec::new();
        let fetched: Option<()> = fetch_with_backoff("nothing", META_FETCH_DEADLINE, || {
            attempts.push(start.elapsed());
            async { Ok::<_, String>(None) }
        })
        .await;

        assert!(fetched.is_none());
        let ms: Vec<u128> = attempts.iter().map(|d| d.as_millis()).collect();
        assert_eq!(ms, [0, 500, 1500, 3500, 7500]);
        assert_eq!(start.elapsed(), Duration::from_millis(7500));
    }

    #[tokio::test(start_paused = true)]
    async fn fetching_cuts_off_an_attempt_at_the_deadline() {
        let start = tokio::time::Instant::now();
        let fetched: Option<()> = fetch_with_backoff("a hung request", META_FETCH_DEADLINE, || {
            std::future::pending::<Result<Option<()>, String>>()
        })
        .await;
        assert!(fetched.is_none());
        assert_eq!(start.elapsed(), META_FETCH_DEADLINE);
    }

    /// A languages directory under a temp dir for the tests that write into
    /// it; see `test_utils::test_languages_directory`.
    fn bundle_on_disk(address: &str, bundle: &str) -> PathBuf {
        let dir = crate::test_utils::test_languages_directory().join(address);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("bundle.js"), bundle).unwrap();
        dir
    }

    /// A bundle already on disk needs no meta from the language language; the
    /// install must not wait on it (it used to retry ten times, 275 s).
    #[tokio::test(start_paused = true)]
    async fn installing_a_language_on_disk_does_not_wait_for_its_meta() {
        let controller = LanguageController::global_instance();
        let (ll, previous, counts) = install_fake_language_language(&controller).await;
        // Its hash does not match the address, so the install stops at the
        // check after the meta, before it would load a runtime.
        let address = format!("QmOnDisk{}", Uuid::new_v4().simple());
        bundle_on_disk(&address, "export const name = 'on disk';");

        let installed = tokio::time::timeout(
            Duration::from_millis(500),
            LanguageController::install_language(address.clone()),
        )
        .await;

        controller.system_addresses.lock().await.language_language = previous;
        controller.set_runtime_for_test(&ll, None).await;
        let error = installed
            .expect("install_language waited for meta it does not need")
            .expect_err("the bundle's hash does not match its address");
        assert!(error.to_string().contains("hash mismatch"), "{}", error);
        assert_eq!(counts.meta.load(Ordering::SeqCst), 0);
    }

    /// A language the language language does not have: the install gives up
    /// after the source deadline (it used to take 275 s), without asking for
    /// a meta it has no bundle to store beside.
    #[tokio::test(start_paused = true)]
    async fn installing_an_unknown_language_gives_up_at_the_source_deadline() {
        let controller = LanguageController::global_instance();
        let (ll, previous, counts) = install_fake_language_language(&controller).await;
        let address = format!("QmUnknown{}", Uuid::new_v4().simple());
        let start = tokio::time::Instant::now();

        let installed = tokio::time::timeout(
            SOURCE_FETCH_DEADLINE + Duration::from_secs(1),
            LanguageController::install_language(address.clone()),
        )
        .await;

        controller.system_addresses.lock().await.language_language = previous;
        controller.set_runtime_for_test(&ll, None).await;
        let error = installed
            .expect("install_language kept retrying past its deadline")
            .expect_err("there is no source to install");
        assert!(error.to_string().contains("no source"), "{}", error);
        assert!(start.elapsed() <= SOURCE_FETCH_DEADLINE);
        // Attempts at 0, 0.5, 1.5, 3.5, 7.5, 15.5 and 23.5 s.
        assert_eq!(counts.source.load(Ordering::SeqCst), 7);
        assert_eq!(counts.meta.load(Ordering::SeqCst), 0);
    }
}
