//! Host-rate and membrane-proof runtime handlers. No case here reaches the
//! global DB, which other tests share.

use serde_json::json;
use std::sync::Arc;

use crate::api::runtime_ws::register_ws_handlers;
use crate::api::ws_handler::HandlerMap;
use crate::types::{RequestContext, RuntimeInfo};

/// `runtimeInfo` carries the commit `build.rs` recorded: a full hash, with
/// `-dirty` for an unclean tree, or `unknown` without git. Never empty.
#[test]
fn runtime_info_carries_the_build_commit_next_to_the_version() {
    let info = serde_json::to_value(RuntimeInfo::for_this_build(true, false)).unwrap();
    assert_eq!(
        info["ad4mExecutorVersion"],
        json!(*crate::globals::AD4M_VERSION)
    );
    let commit = info["ad4mExecutorCommit"]
        .as_str()
        .expect("ad4mExecutorCommit is a string");
    assert!(!commit.is_empty());
    let hash = commit.strip_suffix("-dirty").unwrap_or(commit);
    assert!(
        hash == "unknown" || (hash.len() == 40 && hash.chars().all(|c| c.is_ascii_hexdigit())),
        "commit is a full hash or `unknown`: {commit}"
    );
    assert_eq!(info["isInitialized"], json!(true));
    assert_eq!(info["isUnlocked"], json!(false));
}

/// The `runtime.info` handler answers with the same build commit.
#[tokio::test]
async fn runtime_info_handler_reports_the_build_commit() {
    crate::agent::AgentService::init_global_test_instance();
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: false,
        user_email: None,
        user_did: None,
        cancel_token: None,
    });
    let info = map.dispatch("runtime.info", json!({}), ctx).await.unwrap();
    assert_eq!(
        info["ad4mExecutorCommit"],
        json!(crate::globals::AD4M_GIT_COMMIT)
    );
    assert_eq!(
        info["ad4mExecutorVersion"],
        json!(*crate::globals::AD4M_VERSION)
    );
}

async fn error_code(method: &str, params: serde_json::Value, is_admin_credential: bool) -> u16 {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential,
        user_email: None,
        user_did: None,
        cancel_token: None,
    });
    map.dispatch(method, params, ctx).await.unwrap_err().code
}

#[tokio::test]
async fn host_rate_and_membrane_proof_handlers_check_access() {
    assert_eq!(error_code("runtime.hostRates", json!({}), false).await, 403);
    assert_eq!(
        error_code("runtime.setHostRates", json!({ "rates": [] }), false).await,
        403
    );
    let proof = json!({ "proof": "cHJvb2Y=" });
    assert_eq!(
        error_code("runtime.setUnytMembraneProof", proof, false).await,
        403
    );
    assert_eq!(
        error_code("runtime.unytVersionInfo", json!({}), false).await,
        403
    );
}

#[tokio::test]
async fn host_rate_and_membrane_proof_setters_refuse_invalid_params() {
    for (method, params) in [
        ("runtime.setHostRates", json!({})),
        ("runtime.setHostRates", json!({ "rates": "[]" })),
        (
            "runtime.setHostRates",
            json!({ "rates": [{ "description": "a" }] }),
        ),
        (
            "runtime.setHostRates",
            json!({ "rates": [{ "description": "a", "priceInHOT": -1 }] }),
        ),
        (
            "runtime.setHostRates",
            json!({ "rates": [{ "description": "", "priceInHOT": 1 }] }),
        ),
        ("runtime.setUnytMembraneProof", json!({})),
        ("runtime.setUnytMembraneProof", json!({ "proof": "" })),
        (
            "runtime.setUnytMembraneProof",
            json!({ "proof": "not base64!" }),
        ),
        (
            "runtime.setHostRates",
            json!({ "rates": [
                { "description": "a", "priceInHOT": 1 },
                { "description": "a", "priceInHOT": 2 },
            ] }),
        ),
    ] {
        assert_eq!(
            error_code(method, params.clone(), true).await,
            400,
            "{method} {params}"
        );
    }
}

// Nothing delivers a friend message and the outbox has no owner, so storing one would let
// every user read it through `runtime.outbox`.
#[tokio::test]
async fn send_friend_message_is_not_implemented_and_stores_nothing() {
    use crate::agent::capabilities::RUNTIME_MESSAGES_CREATE_CAPABILITY;
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![RUNTIME_MESSAGES_CREATE_CAPABILITY.clone()]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: false,
        user_email: None,
        user_did: None,
        cancel_token: None,
    });
    let message = json!({ "did": "did:key:friend", "message": { "links": [] } });
    let err = map
        .dispatch("runtime.sendFriendMessage", message, ctx)
        .await
        .unwrap_err();
    assert_eq!(err.code, 501, "{}", err.message);
}
