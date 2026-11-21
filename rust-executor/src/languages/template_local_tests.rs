//! `apply_template_local`: template a bundle on this machine only — read the
//! source from the local languages directory, save it and load it. The
//! template data may hold a secret (an Acorn project passphrase), so the
//! language language is never asked and nothing is published.

use super::error::LanguageError;
use super::language_runtime::{LanguageOperation, LanguageRuntimeRequest};
use super::language_runtime_handle::LanguageRuntimeHandle;
use super::mount_tests::interact;
use super::LanguageController;
use crate::utils::languages_directory;
use serde_json::{json, Map, Value as JsonValue};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedReceiver;
use uuid::Uuid;

/// The fixture source: one marked variable `seed` at its `<to-be-filled>`
/// placeholder (what `unfilled_template_variables` reports), an `interactions`
/// that answers the templated `seed` on the `project` address.
const SEEDED_SOURCE: &str = "//!@ad4m-template-variable\nconst seed = \"<to-be-filled>\";\nexport const name = \"seeded\";\nexport async function init() {}\nexport function interactions(a) { return a === \"project\" ? [{ name: \"seed\", label: \"seed\", parameters: [], execute: async () => seed }] : []; }\n";

fn setup() -> LanguageController {
    crate::test_utils::init_v8_platform();
    crate::test_utils::test_languages_directory();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    LanguageController::global_instance()
}

fn template_data(entries: &[(&str, &str)]) -> Map<String, JsonValue> {
    entries
        .iter()
        .map(|(key, value)| (key.to_string(), json!(value)))
        .collect()
}

/// Save `SEEDED_SOURCE` and return its address (the `base` of a test).
fn save_base(controller: &LanguageController) -> String {
    controller
        .save_language_bundle(SEEDED_SOURCE, None)
        .unwrap()
        .0
}

/// Requests a fake language language has received, by kind — `any` counts every
/// request, so a request of a kind the counters do not name still shows.
#[derive(Default)]
struct Counts {
    meta: AtomicUsize,
    source: AtomicUsize,
    any: AtomicUsize,
}

/// Answer every request with `null` and count them.
fn serve_empty_language_language(
    mut requests: UnboundedReceiver<LanguageRuntimeRequest>,
) -> Arc<Counts> {
    let counts = Arc::new(Counts::default());
    let counting = counts.clone();
    tokio::spawn(async move {
        while let Some(request) = requests.recv().await {
            counting.any.fetch_add(1, Ordering::SeqCst);
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

/// Serve a fake language language from `controller` as its language
/// language; returns its address, the previous one and its request counts.
/// (Copy of `install_fake_language_language` in install_tests.rs.)
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

#[tokio::test]
async fn same_seed_gives_same_address() {
    let controller = setup();
    let base = save_base(&controller);

    let first = controller
        .apply_template_local(&base, template_data(&[("seed", "x")]))
        .await
        .unwrap();
    let second = controller
        .apply_template_local(&base, template_data(&[("seed", "x")]))
        .await
        .unwrap();
    let other = controller
        .apply_template_local(&base, template_data(&[("seed", "y")]))
        .await
        .unwrap();

    assert_eq!(first, second);
    assert_ne!(first, other);
}

#[tokio::test]
async fn the_templated_language_is_loaded_and_sees_its_seed() {
    let controller = setup();
    let base = save_base(&controller);

    let address = controller
        .apply_template_local(&base, template_data(&[("seed", "x")]))
        .await
        .unwrap();

    let result = interact(&format!("{address}://project"), "seed", json!({}))
        .await
        .unwrap();
    assert_eq!(result, json!("x"));
}

/// The source comes from this machine's languages directory; the template data
/// may hold a secret. The language language must not see a single request —
/// fetching the source from it (as `language_apply_template_on_source` does)
/// turns this test red.
#[tokio::test]
async fn the_language_language_is_never_asked() {
    let controller = setup();
    let (ll, previous, counts) = install_fake_language_language(&controller).await;
    let base = save_base(&controller);

    let result = controller
        .apply_template_local(&base, template_data(&[("seed", "x")]))
        .await;

    controller.system_addresses.lock().await.language_language = previous;
    controller.set_runtime_for_test(&ll, None).await;

    result.unwrap();
    assert_eq!(counts.meta.load(Ordering::SeqCst), 0);
    assert_eq!(counts.source.load(Ordering::SeqCst), 0);
    assert_eq!(
        counts.any.load(Ordering::SeqCst),
        0,
        "the language language got a request"
    );
}

/// A `var happ = ` source templates a DNA from the template data — a path
/// `apply_template_local` does not take — so the language must ship its happ
/// under another name and the call refuses it loudly.
#[tokio::test]
async fn a_source_with_var_happ_is_refused() {
    let controller = setup();
    let (base, _) = controller
        .save_language_bundle(&format!("var happ = \"x\";\n{SEEDED_SOURCE}"), None)
        .unwrap();

    let error = controller
        .apply_template_local(&base, template_data(&[("seed", "x")]))
        .await
        .unwrap_err();

    assert!(
        matches!(error, LanguageError::InvalidBundle { .. }),
        "{error}"
    );
    assert!(error.to_string().contains("var happ"), "{error}");
}

#[tokio::test]
async fn an_unfilled_variable_is_refused() {
    let controller = setup();
    let base = save_base(&controller);

    let error = controller
        .apply_template_local(&base, Map::new())
        .await
        .unwrap_err();

    assert!(error.to_string().contains("seed"), "{error}");
}

#[tokio::test]
async fn an_unknown_source_is_not_found() {
    let controller = setup();
    let address = format!("QmMissing{}", Uuid::new_v4().simple());

    let error = controller
        .apply_template_local(&address, template_data(&[("seed", "x")]))
        .await
        .unwrap_err();

    assert!(matches!(error, LanguageError::NotFound { .. }), "{error}");
}

#[tokio::test]
async fn a_source_that_is_not_a_plain_address_is_refused() {
    let controller = setup();
    // A bundle outside the languages directory, which a path in the address could reach.
    let outside = languages_directory()
        .parent()
        .expect("languages directory has a parent")
        .join(format!("outside-{}", Uuid::new_v4().simple()));
    std::fs::create_dir_all(&outside).unwrap();
    std::fs::write(
        outside.join("bundle.js"),
        "//!@ad4m-template-variable\nconst seed = \"\";\n",
    )
    .unwrap();
    let escape = format!("../{}", outside.file_name().unwrap().to_string_lossy());

    for address in [
        escape.as_str(),
        outside.to_str().unwrap(),
        "Qm/sub",
        "Qm\\sub",
        "..",
        "",
        "Qm abc",
    ] {
        let error = controller
            .apply_template_local(address, template_data(&[("seed", "x")]))
            .await
            .unwrap_err();
        assert!(
            matches!(error, LanguageError::InvalidBundle { .. })
                && error.to_string().contains("plain language address"),
            "{address:?}: {error}"
        );
    }
    std::fs::remove_dir_all(&outside).unwrap();
}
