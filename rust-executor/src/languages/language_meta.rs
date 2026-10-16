//! Language meta without asking the language language twice.
//!
//! A language's address is the hash of its bundle and its meta is published
//! once, with the bundle, so the meta under an address never changes. Asking
//! the language language for it costs remote requests (Cloudflare for the
//! default one) queued behind every other request to that language's runtime.
//! So the executor answers from what it already knows: the in-memory cache,
//! then the language's `meta.json` (written when this node publishes or
//! installs the language), and only then the language language.
//!
//! Also the bounded retry used when a language language does not have an
//! expression yet (a just-published language propagating through KV).

use super::{LanguageController, LanguageMeta};
use crate::utils::languages_directory;
use log::warn;
use serde_json::Value as JsonValue;
use std::fmt::Display;
use std::future::Future;
use std::path::{Component, Path, PathBuf};
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

/// The directory of the language at `address`, if `address` can be one: a
/// single path component, so a caller-supplied address cannot name a file
/// outside the languages directory.
pub(super) fn language_dir(address: &str) -> Option<PathBuf> {
    let mut components = Path::new(address).components();
    match (components.next(), components.next()) {
        (Some(Component::Normal(_)), None) => Some(languages_directory().join(address)),
        _ => None,
    }
}

/// An expression's `data` the way the language language may hold it: an
/// object, or that object as a JSON string.
pub(super) fn unstring_meta_data(data: JsonValue) -> JsonValue {
    match &data {
        JsonValue::String(s) => serde_json::from_str::<JsonValue>(s).unwrap_or(data),
        _ => data,
    }
}

/// The `LanguageMeta` for `address` from a published meta (`data`) and the
/// agent that signed the publication.
pub(super) fn language_meta_from_data(
    address: &str,
    data: JsonValue,
    author: String,
) -> LanguageMeta {
    let mut meta: LanguageMeta =
        serde_json::from_value(unstring_meta_data(data)).unwrap_or_default();
    meta.author = author;
    meta.address = address.to_string();
    if meta.templated.is_none() {
        meta.templated = Some(false);
    }
    meta
}

/// What `meta.json` holds: the published meta with its author, so the file
/// alone answers `language.meta`.
pub(super) fn meta_file_content(data: JsonValue, author: &str) -> JsonValue {
    let mut data = unstring_meta_data(data);
    if let JsonValue::Object(map) = &mut data {
        if !author.is_empty() {
            map.insert("author".to_string(), JsonValue::String(author.to_string()));
        }
    }
    data
}

/// The meta in the language's `meta.json`, if it is there and names its
/// author (files written before the author was recorded do not).
fn read_meta_file(address: &str) -> Option<LanguageMeta> {
    let path = language_dir(address)?.join("meta.json");
    let content = std::fs::read_to_string(&path).ok()?;
    let data = unstring_meta_data(serde_json::from_str::<JsonValue>(&content).ok()?);
    let author = data.get("author")?.as_str()?.to_string();
    if author.is_empty() {
        return None;
    }
    Some(language_meta_from_data(address, data, author))
}

impl LanguageController {
    /// The meta under `address` this node already knows, without asking the
    /// language language.
    pub(crate) fn known_language_meta(&self, address: &str) -> Option<LanguageMeta> {
        if let Some(meta) = self.language_metas.lock().unwrap().get(address) {
            return Some(meta.clone());
        }
        let meta = read_meta_file(address)?;
        self.cache_language_meta(&meta);
        Some(meta)
    }

    pub(super) fn cache_language_meta(&self, meta: &LanguageMeta) {
        self.language_metas
            .lock()
            .unwrap()
            .insert(meta.address.clone(), meta.clone());
    }

    /// Record the meta this node has just published for `meta.address`, signed
    /// by `author`: write it to the language's `meta.json` (beside the bundle
    /// the publisher saved) and cache it. A failed write is logged; the cache
    /// still answers until restart, and the language language after that.
    pub fn remember_published_meta(&self, meta: &LanguageMeta, author: &str) {
        let meta = LanguageMeta {
            author: author.to_string(),
            ..meta.clone()
        };
        let meta = language_meta_from_data(
            &meta.address,
            serde_json::to_value(&meta).unwrap_or_default(),
            meta.author.clone(),
        );
        self.cache_language_meta(&meta);
        let Some(dir) = language_dir(&meta.address) else {
            return;
        };
        let written = std::fs::create_dir_all(&dir).and_then(|_| {
            let content = serde_json::to_string_pretty(&meta).map_err(std::io::Error::other)?;
            std::fs::write(dir.join("meta.json"), content)
        });
        if let Err(e) = written {
            warn!("Could not write meta.json for {}: {}", meta.address, e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::languages::language_runtime::{LanguageOperation, LanguageRuntimeRequest};
    use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
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

    /// Answer `requests` as a language language that holds `meta` (an
    /// expression, or `null`) and `source` for every address.
    fn serve_language_language(
        mut requests: UnboundedReceiver<LanguageRuntimeRequest>,
        meta: &'static str,
        source: &'static str,
    ) -> Arc<Counts> {
        let counts = Arc::new(Counts::default());
        let counting = counts.clone();
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                let LanguageOperation::Execute(script, _) = request.operation else {
                    let _ = request.response_tx.send(Ok("null".to_string()));
                    continue;
                };
                let answer = if script.contains("expressionGet(") {
                    counting.meta.fetch_add(1, Ordering::SeqCst);
                    meta
                } else if script.contains("languageGetSource(") {
                    counting.source.fetch_add(1, Ordering::SeqCst);
                    source
                } else {
                    "null"
                };
                let _ = request.response_tx.send(Ok(answer.to_string()));
            }
        });
        counts
    }

    /// Serve a fake language language from `controller` as its language
    /// language; returns its address and the previous one.
    async fn install_fake_language_language(
        controller: &LanguageController,
        meta: &'static str,
        source: &'static str,
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
        (
            address,
            previous,
            serve_language_language(requests, meta, source),
        )
    }

    const PUBLISHED: &str = r#"{"author":"did:key:publisher","data":{"name":"published","possibleTemplateParams":["uid"]}}"#;

    #[tokio::test]
    async fn language_meta_is_asked_of_the_language_language_once() {
        let controller = LanguageController::new();
        let (_, _, counts) = install_fake_language_language(&controller, PUBLISHED, "null").await;
        let address = format!("QmMetaOnce{}", Uuid::new_v4().simple());

        let first = controller.get_language_expression(&address).await.unwrap();
        let second = controller.get_language_expression(&address).await.unwrap();

        assert_eq!(first.author, "did:key:publisher");
        assert_eq!(first.name, "published");
        assert_eq!(first.address, address);
        assert_eq!(second.author, first.author);
        assert_eq!(
            second.possible_template_params,
            Some(vec!["uid".to_string()])
        );
        assert_eq!(
            counts.meta.load(Ordering::SeqCst),
            1,
            "the second read of the same address asked the language language again"
        );
    }

    #[tokio::test]
    async fn an_unknown_language_is_not_cached() {
        let controller = LanguageController::new();
        let (_, _, counts) = install_fake_language_language(&controller, "null", "null").await;
        let address = format!("QmNotYetPublished{}", Uuid::new_v4().simple());

        assert!(controller.get_language_expression(&address).await.is_err());
        assert!(controller.get_language_expression(&address).await.is_err());
        assert_eq!(
            counts.meta.load(Ordering::SeqCst),
            2,
            "a language not published yet may be by the next read"
        );
    }

    #[test]
    fn an_address_names_one_directory_under_the_languages_directory() {
        assert!(language_dir("QmSomething").is_some());
        assert!(language_dir("../QmSomething").is_none());
        assert!(language_dir("Qm/../../etc").is_none());
        assert!(language_dir("/etc").is_none());
        assert!(language_dir("").is_none());
        assert!(language_dir(".").is_none());
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
        let (ll, previous, counts) =
            install_fake_language_language(&controller, "null", "null").await;
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
        let (ll, previous, counts) =
            install_fake_language_language(&controller, "null", "null").await;
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

    #[test]
    fn a_published_meta_is_answered_from_disk() {
        let address = format!("QmPublishedHere{}", Uuid::new_v4().simple());
        bundle_on_disk(&address, "export const name = 'published here';");
        let meta = LanguageMeta {
            address: address.clone(),
            name: "published here".to_string(),
            template_source_language_address: Some("QmSource".to_string()),
            ..Default::default()
        };
        LanguageController::new().remember_published_meta(&meta, "did:key:me");

        // A controller that has not seen it (a restart) reads the file.
        let read = LanguageController::new()
            .known_language_meta(&address)
            .expect("meta.json answers language.meta");
        assert_eq!(read.author, "did:key:me");
        assert_eq!(read.address, address);
        assert_eq!(read.name, "published here");
        assert_eq!(
            read.template_source_language_address,
            Some("QmSource".to_string())
        );
        assert_eq!(read.templated, Some(false));
    }

    #[test]
    fn a_meta_file_without_its_author_is_not_an_answer() {
        let address = format!("QmOldMetaFile{}", Uuid::new_v4().simple());
        let dir = bundle_on_disk(&address, "export const name = 'old';");
        std::fs::write(dir.join("meta.json"), r#"{"name":"old"}"#).unwrap();
        assert!(LanguageController::new()
            .known_language_meta(&address)
            .is_none());
    }
}
