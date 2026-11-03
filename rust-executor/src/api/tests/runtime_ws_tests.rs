//! Host-rate, membrane-proof and Unyt wallet runtime handlers. Only the Unyt key case
//! writes the global DB, which other tests share: it stores a public agent key.

use serde_json::json;
use std::sync::Arc;

use crate::api::runtime_ws::register_ws_handlers;
use crate::api::ws_handler::HandlerMap;
use crate::types::RequestContext;

/// The error code of `method` for the operator, or for an app token without capabilities.
async fn error_code(method: &str, params: serde_json::Value, operator: bool) -> u16 {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(if operator {
        RequestContext::operator_for_test()
    } else {
        RequestContext::app_for_test(vec![])
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

// Only the admin credential: an app that holds every capability is refused too, and a user
// with the default capabilities. No call here gets past the check, so none touches the host
// or the DB. The guard refuses, with its message.
#[tokio::test]
async fn operator_only_methods_refuse_an_app_holding_every_capability_and_users() {
    use crate::agent::capabilities::{get_user_default_capabilities, ALL_CAPABILITY};
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let app = RequestContext::app_for_test(vec![ALL_CAPABILITY.clone()]);
    let user =
        RequestContext::user_for_test("did:key:operator-only", get_user_default_capabilities());
    for (method, params) in [
        ("runtime.openLink", json!({ "url": "https://example.org" })),
        ("runtime.restartHolochain", json!({})),
        ("runtime.setHostRates", json!({ "rates": [] })),
        (
            "runtime.setUnytMembraneProof",
            json!({ "proof": "cHJvb2Y=" }),
        ),
        (
            "runtime.grantNotification",
            json!({ "id": "n1", "granted": true }),
        ),
        (
            "runtime.importData",
            json!({ "type": "db", "filePath": "/nonexistent/dump.json" }),
        ),
        ("runtime.unytAgentKey", json!({})),
        ("runtime.unytHotAgentPubkey", json!({})),
        ("runtime.unytWalletBalance", json!({})),
        ("runtime.unytWalletHistory", json!({ "perPage": 10 })),
        (
            "runtime.unytSendHot",
            json!({ "recipient": "r", "amount": "1" }),
        ),
        ("runtime.unytReinstallDna", json!({})),
    ] {
        for ctx in [&app, &user] {
            let err = map
                .dispatch(method, params.clone(), Arc::new(ctx.clone()))
                .await
                .unwrap_err();
            assert_eq!(
                (err.code, err.message.as_str()),
                (403, crate::api::guards::OPERATOR_ONLY),
                "{method}"
            );
        }
    }
}

/// The answer of `method` to the operator.
async fn operator_call(
    method: &str,
    params: serde_json::Value,
) -> Result<serde_json::Value, (u16, String)> {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    map.dispatch(
        method,
        params,
        Arc::new(RequestContext::operator_for_test()),
    )
    .await
    .map_err(|e| (e.code, e.message))
}

// The operator reads the executor's Unyt agent key: the public key the keystore made, which
// the executor stores. Both methods answer it, and nothing else.
#[tokio::test]
async fn unyt_key_methods_answer_the_operator_the_stored_public_key() {
    crate::test_utils::ensure_test_db();
    let key = holochain::prelude::AgentPubKey::from_raw_32(vec![7; 32]).to_string();
    crate::db::Ad4mDb::with_global_instance(|db| db.set_setting("unyt_agent_key", &key)).unwrap();
    for method in ["runtime.unytAgentKey", "runtime.unytHotAgentPubkey"] {
        let answer = operator_call(method, json!({})).await.unwrap();
        assert_eq!(answer, json!(key), "{method}");
        let text = answer.as_str().unwrap();
        assert!(
            holochain::prelude::AgentPubKey::try_from(text).is_ok(),
            "{method}: {text}"
        );
    }
}

// The guard admits the operator, and the call reaches the Unyt service. Unit tests store no
// membrane proof and start no conductor, so the service refuses with its own error, not the
// guard's 403 or a 501.
#[tokio::test]
async fn unyt_wallet_methods_reach_the_unyt_service_for_the_operator() {
    crate::test_utils::ensure_test_db();
    assert!(crate::unyt_service::get_membrane_proof().is_none());
    for (method, params, error) in [
        ("runtime.unytWalletBalance", json!({}), "No membrane proof"),
        ("runtime.unytWalletHistory", json!({}), "No membrane proof"),
        (
            "runtime.unytWalletHistory",
            json!({ "page": 3, "perPage": 10 }),
            "No membrane proof",
        ),
        (
            "runtime.unytSendHot",
            json!({ "recipient": "uhCAkrecipient", "amount": "1.5", "note": "n" }),
            "No membrane proof",
        ),
        (
            "runtime.unytReinstallDna",
            json!({}),
            "Holochain service not available",
        ),
    ] {
        let (code, message) = operator_call(method, params).await.unwrap_err();
        assert_eq!(code, 500, "{method}: {message}");
        assert!(message.contains(error), "{method}: {message}");
    }
}

#[tokio::test]
async fn unyt_wallet_methods_refuse_invalid_params() {
    for (method, params) in [
        ("runtime.unytSendHot", json!({ "recipient": "r" })),
        (
            "runtime.unytSendHot",
            json!({ "recipient": " ", "amount": "1" }),
        ),
        (
            "runtime.unytSendHot",
            json!({ "recipient": "r", "amount": "-" }),
        ),
        ("runtime.unytWalletHistory", json!({ "perPage": 0 })),
        ("runtime.unytWalletHistory", json!({ "perPage": -1 })),
    ] {
        assert_eq!(
            error_code(method, params.clone(), true).await,
            400,
            "{method} {params}"
        );
    }
}

#[test]
fn unyt_send_hot_result_names_the_proposal() {
    let result = crate::api::types::UnytSendHotResult {
        proposal_hash: "uhCkkproposal".to_string(),
    };
    assert_eq!(
        serde_json::to_value(&result).unwrap(),
        json!({ "proposalHash": "uhCkkproposal" })
    );
}

// Free hosting is a billing decision for the whole executor: an app approved for "quit" may not
// switch it (#1328). The refusal comes before the handler, so the shared DB stays untouched.
#[tokio::test]
async fn set_free_hosting_enabled_refuses_an_app_without_every_capability() {
    use crate::agent::capabilities::RUNTIME_QUIT_CAPABILITY;
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let quit_app = Arc::new(RequestContext::app_for_test(vec![
        RUNTIME_QUIT_CAPABILITY.clone()
    ]));
    let err = map
        .dispatch(
            "runtime.setFreeHostingEnabled",
            json!({ "enabled": true }),
            quit_app,
        )
        .await
        .unwrap_err();
    assert_eq!(err.code, 403, "{}", err.message);
}

// Nothing delivers a friend message and the outbox has no owner, so storing one would let
// every user read it through `runtime.outbox`.
#[tokio::test]
async fn send_friend_message_is_not_implemented_and_stores_nothing() {
    use crate::agent::capabilities::RUNTIME_MESSAGES_CREATE_CAPABILITY;
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext::app_for_test(vec![
        RUNTIME_MESSAGES_CREATE_CAPABILITY.clone(),
    ]));
    let message = json!({ "did": "did:key:friend", "message": { "links": [] } });
    let err = map
        .dispatch("runtime.sendFriendMessage", message, ctx)
        .await
        .unwrap_err();
    assert_eq!(err.code, 501, "{}", err.message);
}
