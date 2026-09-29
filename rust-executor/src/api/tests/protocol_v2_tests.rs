//! Protocol v2 (opt-in executor features), driven through the real
//! `HandlerMap` so each test also proves the method is registered.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::ALL_CAPABILITY;
use crate::api::protocol::{PROTOCOL_FEATURES, PROTOCOL_VERSION};
use crate::api::ws_handler::{build_handler_map, WsRpcError};
use crate::types::RequestContext;

pub(crate) fn admin_ctx() -> Arc<RequestContext> {
    Arc::new(RequestContext {
        capabilities: Ok(vec![ALL_CAPABILITY.clone()]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: true,
        user_email: None,
        user_did: None,
        cancel_token: None,
    })
}

/// A context that holds no capability at all.
pub(crate) fn no_cap_ctx() -> Arc<RequestContext> {
    Arc::new(RequestContext {
        capabilities: Ok(vec![]),
        auto_permit_cap_requests: false,
        auth_token: "not-a-token".into(),
        is_admin_credential: false,
        user_email: None,
        user_did: None,
        cancel_token: None,
    })
}

pub(crate) async fn call(
    method: &str,
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    build_handler_map().dispatch(method, params, ctx).await
}

// ── X1: runtime.protocol ────────────────────────────────────────────────────

#[tokio::test]
async fn runtime_protocol_reports_version_and_features() {
    let reply = call("runtime.protocol", json!({}), admin_ctx())
        .await
        .expect("runtime.protocol must be registered");
    assert_eq!(reply["version"], json!(PROTOCOL_VERSION));
    assert_eq!(reply["version"], json!(2));
    let features: Vec<&str> = reply["features"]
        .as_array()
        .expect("features must be an array")
        .iter()
        .map(|f| f.as_str().expect("feature names are strings"))
        .collect();
    assert_eq!(features, PROTOCOL_FEATURES);
    assert!(features.contains(&"runtime.protocol"));
}

#[tokio::test]
async fn runtime_protocol_needs_no_capability() {
    let reply = call("runtime.protocol", json!({}), no_cap_ctx()).await;
    assert!(reply.is_ok(), "clients probe before they hold a token");
}

#[test]
fn protocol_features_are_unique() {
    let mut seen = std::collections::HashSet::new();
    for f in PROTOCOL_FEATURES {
        assert!(seen.insert(*f), "duplicate feature {f}");
    }
}

// ── Perspective fixture ─────────────────────────────────────────────────────

/// Unregisters the fixture perspective when the test ends (also on panic).
pub(crate) struct Registered(pub String);
impl Drop for Registered {
    fn drop(&mut self) {
        crate::perspectives::unregister_perspective(&self.0);
    }
}

/// A perspective with `classes` registered in the global registry, so the
/// handlers find it by uuid.
pub(crate) async fn registered_perspective(classes: &[(&str, &str)]) -> Registered {
    let (perspective, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(classes).await;
    let uuid = perspective.persisted.lock().await.uuid.clone();
    crate::perspectives::register_perspective(uuid.clone(), perspective);
    Registered(uuid)
}

// ── X9: perspective.discardBatch ────────────────────────────────────────────

#[tokio::test]
async fn discard_batch_drops_an_open_batch_once() {
    let p = registered_perspective(&[]).await;
    let batch_id = call("perspective.createBatch", json!({ "uuid": p.0 }), admin_ctx())
        .await
        .unwrap();
    let params = json!({ "uuid": p.0, "batchId": batch_id });

    assert_eq!(
        call("perspective.discardBatch", params.clone(), admin_ctx())
            .await
            .unwrap(),
        json!(true)
    );
    assert_eq!(
        call("perspective.discardBatch", params.clone(), admin_ctx())
            .await
            .unwrap(),
        json!(false),
        "a second discard is a no-op"
    );
    let err = call("perspective.commitBatch", params, admin_ctx())
        .await
        .expect_err("a discarded batch cannot be committed");
    assert!(err.message.contains("No batch found"), "{}", err.message);
}

#[tokio::test]
async fn discard_batch_leaves_other_batches_committable() {
    let p = registered_perspective(&[]).await;
    let a = call("perspective.createBatch", json!({ "uuid": p.0 }), admin_ctx())
        .await
        .unwrap();
    let b = call("perspective.createBatch", json!({ "uuid": p.0 }), admin_ctx())
        .await
        .unwrap();
    call(
        "perspective.discardBatch",
        json!({ "uuid": p.0, "batchId": a }),
        admin_ctx(),
    )
    .await
    .unwrap();
    call(
        "perspective.commitBatch",
        json!({ "uuid": p.0, "batchId": b }),
        admin_ctx(),
    )
    .await
    .expect("the other batch stays open");
}

#[tokio::test]
async fn discard_batch_checks_the_update_capability() {
    let p = registered_perspective(&[]).await;
    let err = call(
        "perspective.discardBatch",
        json!({ "uuid": p.0, "batchId": "x" }),
        no_cap_ctx(),
    )
    .await
    .expect_err("no capability");
    assert_eq!(err.code, 403);
}
