//! `perspective.createSubject` on a class whose constructor is a `call`
//! answers the URL the language minted, not `true` (design §3.3). Until
//! ad4m#1775, only the main agent may make such a call, and only with
//! `expression:update`.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::{perspective_update_capability, EXPRESSION_UPDATE_CAPABILITY};
use crate::api::tests::support::{admin_ctx, Registered};
use crate::api::ws_handler::build_handler_map;
use crate::languages::mount_tests::{interact, load_mount_test_language, mount_thing_shape};
use crate::perspectives::perspective_instance::SdnaType;
use crate::types::RequestContext;

/// A registered perspective with `shape` as the SDNA of `MountThing`, by
/// that name. `registerShapes` would name the class after its target class
/// IRI instead, as a mount does not.
async fn mount_thing_perspective(shape: &str) -> Registered {
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_sdna(
        "MountThing".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shape.to_string()),
        &ctx,
    )
    .await
    .unwrap();
    let uuid = p.persisted.lock().await.uuid.clone();
    crate::perspectives::register_perspective(uuid.clone(), p);
    Registered(uuid)
}

async fn call(method: &str, params: Value) -> Result<Value, (u16, String)> {
    call_as(admin_ctx(), method, params).await
}

async fn call_as(
    ctx: Arc<RequestContext>,
    method: &str,
    params: Value,
) -> Result<Value, (u16, String)> {
    build_handler_map()
        .dispatch(method, params, ctx)
        .await
        .map_err(|e| (e.code, e.message))
}

fn create_params(uuid: &str) -> Value {
    json!({
        "uuid": uuid,
        "subjectClass": "MountThing",
        "expressionAddress": "ignored",
        "initialValues": "{\"title\":\"a\"}",
    })
}

/// How many calls the mount-test language has served.
async fn call_count(lang: &str) -> usize {
    interact(&format!("{lang}://project"), "calls", json!({}))
        .await
        .unwrap()
        .as_array()
        .unwrap()
        .len()
}

#[tokio::test]
async fn create_subject_rpc_returns_the_url_for_a_call_constructor() {
    let lang = load_mount_test_language().await;
    let shape = mount_thing_shape(&lang).await;
    let p = mount_thing_perspective(&shape).await;

    let result = call(
        "perspective.createSubject",
        json!({
            "uuid": p.0,
            "subjectClass": "MountThing",
            "expressionAddress": "ignored",
            "initialValues": "{\"title\":\"a\"}",
        }),
    )
    .await
    .unwrap();

    assert_eq!(result, json!(format!("{lang}://t1")));
}

#[tokio::test]
async fn create_subject_rpc_needs_expression_update_for_a_call() {
    let lang = load_mount_test_language().await;
    let shape = mount_thing_shape(&lang).await;
    let p = mount_thing_perspective(&shape).await;
    let update = perspective_update_capability(vec!["*".to_string()]);

    // A main-agent app with only the perspective capability: refused
    // before the language is called.
    let before = call_count(&lang).await;
    let app = Arc::new(RequestContext::app_for_test(vec![update.clone()]));
    let err = call_as(app, "perspective.createSubject", create_params(&p.0))
        .await
        .unwrap_err();
    assert!(err.1.contains("expression:update"), "{err:?}");
    assert_eq!(call_count(&lang).await, before, "the language was called");

    // The same app with `expression:update` too: the call runs.
    let app = Arc::new(RequestContext::app_for_test(vec![
        update,
        EXPRESSION_UPDATE_CAPABILITY.clone(),
    ]));
    let result = call_as(app, "perspective.createSubject", create_params(&p.0))
        .await
        .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|url| url.starts_with(&format!("{lang}://"))));
}

#[tokio::test]
async fn create_subject_rpc_refuses_a_call_for_a_managed_user() {
    let lang = load_mount_test_language().await;
    let shape = mount_thing_shape(&lang).await;
    let p = mount_thing_perspective(&shape).await;
    let alice = "did:key:z6MkAliceCallActionUser";
    crate::perspectives::get_perspective(&p.0)
        .unwrap()
        .persisted
        .lock()
        .await
        .add_owner(alice);

    // Alice owns the perspective and holds every capability a call needs,
    // but the call would run as the main agent.
    let before = call_count(&lang).await;
    let user = Arc::new(RequestContext::user_for_test(
        alice,
        vec![
            perspective_update_capability(vec!["*".to_string()]),
            EXPRESSION_UPDATE_CAPABILITY.clone(),
        ],
    ));
    let err = call_as(user, "perspective.createSubject", create_params(&p.0))
        .await
        .unwrap_err();
    assert!(err.1.contains("main agent"), "{err:?}");
    assert_eq!(call_count(&lang).await, before, "the language was called");
}
