//! The `mount-test` fixture language: records in memory, `create`/`update`/
//! `delete` interactions on static addresses. The Acorn steps (A2–A7) use it
//! as the stand-in for the Acorn language, so executor work can be tested
//! without Holochain. These tests also pin down that "static" addresses like
//! `<lang>://class/MountThing` already work through `expression_interact`
//! with no executor change.

use super::error::LanguageError;
use super::LanguageController;
use crate::types::InteractionCall;
use serde_json::{json, Value};

/// Source of the `mount-test` fixture language.
pub(crate) const MOUNT_TEST_SOURCE: &str = include_str!("fixtures/mount_test_language.js");

/// Save and load a fresh copy of `mount-test`; returns its address.
/// Each call appends a unique comment line so every test gets its own runtime (own in-memory state).
pub(crate) async fn load_mount_test_language() -> String {
    crate::test_utils::init_v8_platform();
    crate::test_utils::test_languages_directory();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();

    let source = format!(
        "{}\n// instance {}\n",
        MOUNT_TEST_SOURCE,
        uuid::Uuid::new_v4()
    );
    let controller = LanguageController::global_instance();
    let (_, path) = controller.save_language_bundle(&source, None).unwrap();
    controller.load_language(path, false).await.unwrap()
}

/// Call `name` on `url` with `params` (a JSON value); returns the parsed JSON result.
pub(crate) async fn interact(url: &str, name: &str, params: Value) -> Result<Value, LanguageError> {
    let call = InteractionCall {
        name: name.to_string(),
        parameters_stringified: params.to_string(),
    };
    let result = LanguageController::global_instance()
        .expression_interact(url, &call, &crate::agent::AgentContext::main_agent())
        .await?;
    Ok(match result {
        Some(s) => serde_json::from_str(&s).unwrap_or(Value::String(s)),
        None => Value::Null,
    })
}

#[tokio::test]
async fn create_on_a_static_class_address_returns_a_delta() {
    let addr = load_mount_test_language().await;
    let result = interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"title": "a"}),
    )
    .await
    .unwrap();
    assert_eq!(
        result,
        json!({
            "address": "t1",
            "changed": [{"class": "MountThing", "address": "t1", "data": {"title": "a", "done": false, "tags": []}}],
            "removed": []
        })
    );
}

#[tokio::test]
async fn update_and_delete_are_offered_on_a_record_address() {
    let addr = load_mount_test_language().await;
    interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"title": "a"}),
    )
    .await
    .unwrap();

    let controller = LanguageController::global_instance();
    let names: Vec<String> = controller
        .expression_interactions(&format!("{addr}://t1"))
        .await
        .unwrap()
        .into_iter()
        .map(|i| i.name)
        .collect();
    assert_eq!(names, ["update", "delete"]);

    let updated = interact(&format!("{addr}://t1"), "update", json!({"done": true}))
        .await
        .unwrap();
    assert_eq!(updated["changed"][0]["data"]["done"], true);
    assert_eq!(updated["changed"][0]["data"]["title"], "a");

    let deleted = interact(&format!("{addr}://t1"), "delete", json!({}))
        .await
        .unwrap();
    assert_eq!(deleted["removed"], json!(["t1"]));

    let remaining = controller
        .expression_interactions(&format!("{addr}://t1"))
        .await
        .unwrap();
    assert!(remaining.is_empty());
}

#[tokio::test]
async fn an_unknown_interaction_name_fails() {
    let addr = load_mount_test_language().await;
    let error = interact(&format!("{addr}://class/MountThing"), "nope", json!({}))
        .await
        .unwrap_err();
    assert!(
        error.to_string().contains("No interaction named nope"),
        "{error}"
    );
}

#[tokio::test]
async fn a_throwing_interaction_reports_its_message() {
    let addr = load_mount_test_language().await;
    let error = interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"fail": "boom"}),
    )
    .await
    .unwrap_err();
    assert!(error.to_string().contains("create: boom"), "{error}");
}

#[tokio::test]
async fn the_call_log_records_every_call() {
    let addr = load_mount_test_language().await;
    interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"title": "a"}),
    )
    .await
    .unwrap();
    interact(&format!("{addr}://t1"), "update", json!({"done": true}))
        .await
        .unwrap();
    interact(&format!("{addr}://t1"), "delete", json!({}))
        .await
        .unwrap();

    let calls = interact(&format!("{addr}://project"), "calls", json!({}))
        .await
        .unwrap();
    let names: Vec<&str> = calls
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert_eq!(names, ["create", "update", "delete"]);
    assert_eq!(calls[1]["params"], json!({"done": true}));
}

#[tokio::test]
async fn the_project_address_reports_the_language_address() {
    let addr = load_mount_test_language().await;
    let reported = interact(&format!("{addr}://project"), "address", json!({}))
        .await
        .unwrap();
    assert_eq!(reported, Value::String(addr));
}

#[tokio::test]
async fn mount_shapes_returns_the_parsed_shapes() {
    let addr = load_mount_test_language().await;
    let shapes = LanguageController::global_instance()
        .mount_shapes(&addr)
        .await
        .unwrap();
    assert_eq!(shapes.version, "1");
    assert_eq!(shapes.shapes.len(), 1);
    assert_eq!(shapes.shapes[0].name, "MountThing");
    assert_eq!(
        shapes.shapes[0].shape["constructor_actions"][0]["source"],
        json!(format!("{addr}://class/MountThing"))
    );
}

#[tokio::test]
async fn mount_snapshot_lists_the_records() {
    let addr = load_mount_test_language().await;
    interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"title": "a"}),
    )
    .await
    .unwrap();
    interact(
        &format!("{addr}://class/MountThing"),
        "create",
        json!({"title": "b", "done": true, "tags": ["x"]}),
    )
    .await
    .unwrap();

    let snapshot = LanguageController::global_instance()
        .mount_snapshot(&addr)
        .await
        .unwrap();
    assert_eq!(snapshot.entries.len(), 2);
    let by_address: std::collections::HashMap<&str, _> = snapshot
        .entries
        .iter()
        .map(|e| (e.address.as_str(), e))
        .collect();
    assert_eq!(by_address["t1"].r#class, "MountThing");
    assert_eq!(
        by_address["t1"].data,
        json!({"title": "a", "done": false, "tags": []})
    );
    assert_eq!(
        by_address["t2"].data,
        json!({"title": "b", "done": true, "tags": ["x"]})
    );
}

#[tokio::test]
async fn a_language_without_mount_exports_cannot_be_mounted() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::test_languages_directory();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();

    let source = format!(
        "export const name = \"plain\"; export async function init() {{}}\n// instance {}\n",
        uuid::Uuid::new_v4()
    );
    let controller = LanguageController::global_instance();
    let (_, path) = controller.save_language_bundle(&source, None).unwrap();
    let addr = controller.load_language(path, false).await.unwrap();

    let error = controller.mount_shapes(&addr).await.unwrap_err();
    assert!(
        error.to_string().contains("language cannot be mounted"),
        "{error}"
    );
    let error = controller.mount_snapshot(&addr).await.unwrap_err();
    assert!(
        error.to_string().contains("language cannot be mounted"),
        "{error}"
    );
}

/// The mount exports are not a `Capability` (those are agent-language only), so
/// the same break test as
/// `capability::tests::the_bootstrap_copies_every_capability_export_onto_the_instance`
/// gets its own copy here: an export missing from `flatExportNames` never
/// reaches the `language` object the mount calls evaluate.
#[test]
fn the_bootstrap_copies_the_mount_exports() {
    let bootstrap = include_str!("../js_core/language_bootstrap.js");
    let start = bootstrap
        .find("const flatExportNames = [")
        .expect("language_bootstrap.js lists its flat exports");
    let list = &bootstrap[start..start + bootstrap[start..].find("];").unwrap()];
    for name in ["mountShapes", "mountSnapshot"] {
        assert!(
            list.contains(&format!("\"{name}\"")),
            "language_bootstrap.js does not copy {name} onto the language instance"
        );
    }
}
