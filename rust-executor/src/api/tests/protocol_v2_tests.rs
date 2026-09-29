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

