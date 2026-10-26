//! `install_language` against a fake language language. Its store answers
//! reads consistently (R2), so a miss means the language was never published:
//! one request decides, and only a request that hangs runs into the timeout.

use super::language_runtime::{LanguageOperation, LanguageRuntimeRequest};
use super::language_runtime_handle::LanguageRuntimeHandle;
use super::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::UnboundedReceiver;
use uuid::Uuid;

/// Requests a fake language language has received, by kind.
#[derive(Default)]
struct Counts {
    meta: AtomicUsize,
    source: AtomicUsize,
}

/// Answer `requests` as a language language that has no language at all, or,
/// with `answer` false, never answer them.
fn serve_empty_language_language(
    mut requests: UnboundedReceiver<LanguageRuntimeRequest>,
    answer: bool,
) -> Arc<Counts> {
    let counts = Arc::new(Counts::default());
    let counting = counts.clone();
    tokio::spawn(async move {
        let mut unanswered = Vec::new();
        while let Some(request) = requests.recv().await {
            if let LanguageOperation::Execute(script, _) = &request.operation {
                if script.contains("expressionGet(") {
                    counting.meta.fetch_add(1, Ordering::SeqCst);
                } else if script.contains("languageGetSource(") {
                    counting.source.fetch_add(1, Ordering::SeqCst);
                }
            }
            if answer {
                let _ = request.response_tx.send(Ok("null".to_string()));
            } else {
                unanswered.push(request);
            }
        }
    });
    counts
}

/// Serve a fake language language from `controller` as its language
/// language; returns its address, the previous one and its request counts.
async fn install_fake_language_language(
    controller: &LanguageController,
    answer: bool,
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
        serve_empty_language_language(requests, answer),
    )
}

/// A bundle already on disk needs no meta from the language language: the
/// install must not ask for it.
#[tokio::test(start_paused = true)]
async fn installing_a_language_on_disk_does_not_ask_for_its_meta() {
    let controller = LanguageController::global_instance();
    let (ll, previous, counts) = install_fake_language_language(&controller, true).await;
    // Its hash does not match the address, so the install stops at the hash
    // check, before it would load a runtime.
    let address = format!("QmOnDisk{}", Uuid::new_v4().simple());
    let dir = crate::test_utils::test_languages_directory().join(&address);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("bundle.js"), "export const name = 'on disk';").unwrap();

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

/// A language the language language does not have was never published: one
/// request says so, and the install fails at once (it used to retry for up
/// to 275 s, then 30 s).
#[tokio::test(start_paused = true)]
async fn installing_an_unknown_language_fails_after_one_request() {
    let controller = LanguageController::global_instance();
    let (ll, previous, counts) = install_fake_language_language(&controller, true).await;
    let address = format!("QmUnknown{}", Uuid::new_v4().simple());
    let start = tokio::time::Instant::now();

    let installed = tokio::time::timeout(
        LANGUAGE_FETCH_TIMEOUT,
        LanguageController::install_language(address.clone()),
    )
    .await;

    controller.system_addresses.lock().await.language_language = previous;
    controller.set_runtime_for_test(&ll, None).await;
    let error = installed
        .expect("install_language retried a definite miss")
        .expect_err("there is no source to install");
    assert!(error.to_string().contains("no source"), "{}", error);
    assert_eq!(start.elapsed(), Duration::ZERO, "no waiting on a miss");
    assert_eq!(counts.source.load(Ordering::SeqCst), 1);
    assert_eq!(counts.meta.load(Ordering::SeqCst), 0);
}

/// A language language that never answers holds an install only until the
/// timeout: the install fails, and `ensure_link_language` tries again later.
#[tokio::test(start_paused = true)]
async fn a_source_request_that_hangs_ends_at_the_timeout() {
    let controller = LanguageController::global_instance();
    let (ll, previous, counts) = install_fake_language_language(&controller, false).await;
    let address = format!("QmHung{}", Uuid::new_v4().simple());
    let start = tokio::time::Instant::now();

    let installed = tokio::time::timeout(
        LANGUAGE_FETCH_TIMEOUT + Duration::from_secs(1),
        LanguageController::install_language(address.clone()),
    )
    .await;

    controller.system_addresses.lock().await.language_language = previous;
    controller.set_runtime_for_test(&ll, None).await;
    let error = installed
        .expect("install_language waited past its timeout")
        .expect_err("the source never came");
    assert!(error.to_string().contains("timed out"), "{}", error);
    assert_eq!(start.elapsed(), LANGUAGE_FETCH_TIMEOUT);
    assert_eq!(counts.source.load(Ordering::SeqCst), 1);
}

/// The agent language sends profiles and identity calls to its `storeUrl` setting. When
/// `identity.storeUrl` cannot be written there, the language is never fetched or loaded, so no
/// identity call reaches the store an older or unparsable settings file names (or the
/// language's default store): the identity backend answers `unavailable`.
#[tokio::test(start_paused = true)]
async fn the_agent_language_is_not_loaded_when_its_store_url_cannot_be_written() {
    use crate::identity::backend::{AgentLanguageBackend, IdentityBackend, IdentityError};
    let controller = LanguageController::global_instance();
    let (ll, previous, counts) = install_fake_language_language(&controller, true).await;
    let languages = crate::test_utils::test_languages_directory();
    let settings = crate::identity::IdentitySettings {
        store_url: Some("http://127.0.0.1:9".into()),
        ..Default::default()
    };

    // `settings.json` is a directory: the setting cannot be written.
    let agent = format!("QmAgentUnwritable{}", Uuid::new_v4().simple());
    std::fs::create_dir_all(languages.join(&agent).join("settings.json")).unwrap();
    let refused = tokio::time::timeout(
        Duration::from_millis(500),
        controller.install_agent_language(&languages, &agent, &settings),
    )
    .await
    .expect("no fetch to wait for");
    let fetched = counts.meta.load(Ordering::SeqCst) + counts.source.load(Ordering::SeqCst);
    let loaded = controller.is_language_loaded(&agent).await;
    let previous_agent = controller
        .system_addresses
        .lock()
        .await
        .agent_language
        .replace(agent.clone());
    let identity_call = AgentLanguageBackend.log("did:scid:coasys:1:Qm", 0).await;
    controller.system_addresses.lock().await.agent_language = previous_agent;

    // Control: with a writable settings file the same call goes on to fetch the language.
    let writable = format!("QmAgentWritable{}", Uuid::new_v4().simple());
    let installed = tokio::time::timeout(
        Duration::from_millis(500),
        controller.install_agent_language(&languages, &writable, &settings),
    )
    .await
    .expect("the fake language language answers at once");
    let fetched_writable = counts.meta.load(Ordering::SeqCst);

    controller.system_addresses.lock().await.language_language = previous;
    controller.set_runtime_for_test(&ll, None).await;

    let error = refused.expect_err("an unwritten storeUrl stops the install");
    assert!(error.to_string().contains("identity.storeUrl"), "{error}");
    assert_eq!(fetched, 0, "the agent language was fetched");
    assert!(!loaded, "the agent language was loaded");
    assert!(
        matches!(identity_call, Err(IdentityError::Unavailable(_))),
        "{identity_call:?}"
    );
    assert!(
        installed.is_err(),
        "the fake language language has no source"
    );
    assert_eq!(fetched_writable, 1);
    let written: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(languages.join(&writable).join("settings.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(written["storeUrl"], "http://127.0.0.1:9");
}
