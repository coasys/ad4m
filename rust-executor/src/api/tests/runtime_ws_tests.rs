//! Host-rate and membrane-proof runtime handlers. No case here reaches the
//! global DB, which other tests share.

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
        (
            "runtime.importData",
            json!({ "type": "perspective", "filePath": "/nonexistent/export.json", "perspectiveUuid": "p1" }),
        ),
        ("runtime.unytAgentKey", json!({})),
        (
            "runtime.unytSendHot",
            json!({ "recipient": "r", "amount": "1" }),
        ),
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

// The unyt stubs have no implementation: the operator, the only caller the guard admits,
// gets the 501.
#[tokio::test]
async fn unyt_stubs_answer_the_operator_not_implemented() {
    for (method, params) in [
        ("runtime.unytAgentKey", json!({})),
        (
            "runtime.unytSendHot",
            json!({ "recipient": "r", "amount": "1" }),
        ),
        ("runtime.unytWalletBalance", json!({})),
        ("runtime.unytWalletHistory", json!({})),
        ("runtime.unytHotAgentPubkey", json!({})),
        ("runtime.unytReinstallDna", json!({})),
    ] {
        assert_eq!(error_code(method, params, true).await, 501, "{method}");
    }
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

// `runtime.importData {type:"perspective"}` bulk-loads a snapshot file into a named
// perspective: without `perspectiveUuid` it is a 400 before any lookup. An unknown uuid is
// `get_perspective_or_404`'s usual 404 (it reads the global config on a miss, which this
// file's cases leave alone), and a caller the operator guard turns away never reaches
// either (`operator_only_methods_refuse_an_app_holding_every_capability_and_users`).
#[tokio::test]
async fn perspective_import_needs_a_perspective_uuid() {
    assert_eq!(
        error_code(
            "runtime.importData",
            json!({ "type": "perspective", "filePath": "/nonexistent/export.json" }),
            true,
        )
        .await,
        400
    );
}

// A file that is not a perspective export is the caller's mistake: 400, not 500.
#[tokio::test]
async fn perspective_import_of_a_malformed_file_is_a_400() {
    use crate::perspectives::perspective_instance::PerspectiveInstance;
    use crate::perspectives::register_perspective;
    use crate::types::{PerspectiveHandle, PerspectiveState};

    let uuid = uuid::Uuid::new_v4().to_string();
    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some("import refusal".to_string()),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
            owners: None,
        },
        None,
    );
    register_perspective(uuid.clone(), perspective);
    let _registered = super::support::Registered(uuid.clone());
    let file = tempfile::NamedTempFile::new().unwrap();
    std::fs::write(file.path(), "{\"nope\": 1}").unwrap();

    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let err = map
        .dispatch(
            "runtime.importData",
            json!({
                "type": "perspective",
                "filePath": file.path().to_str().unwrap(),
                "perspectiveUuid": uuid,
            }),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap_err();
    assert_eq!(err.code, 400, "{}", err.message);
}

// The operator's import lands in the perspective's store: links a query then finds,
// with the counts the load reports.
#[tokio::test]
async fn operator_imports_a_perspective_snapshot_into_the_store() {
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::perspective_instance::PerspectiveInstance;
    use crate::perspectives::register_perspective;
    use crate::types::domain::Perspective as DomainPerspective;
    use crate::types::{
        DecoratedLinkExpression, Link, LinkStatus, PerspectiveHandle, PerspectiveState,
    };

    let uuid = uuid::Uuid::new_v4().to_string();
    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some("import test".to_string()),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
            owners: None,
        },
        None,
    );
    register_perspective(uuid.clone(), perspective.clone());
    let _registered = super::support::Registered(uuid.clone());

    // The snapshot file, as `runtime.exportData {type:"perspective"}` writes it.
    let signer = TestSigner::generate();
    let links: Vec<DecoratedLinkExpression> = (0..3)
        .map(|i| {
            let signed = signer.sign(
                Link {
                    source: format!("ad4m://s/{i}"),
                    predicate: Some("ad4m://p".to_string()),
                    target: format!("ad4m://t/{i}"),
                }
                .normalize(),
            );
            DecoratedLinkExpression {
                author: signed.author,
                timestamp: signed.timestamp,
                data: signed.data,
                proof: crate::types::DecoratedExpressionProof {
                    key: signed.proof.key,
                    signature: signed.proof.signature,
                    valid: None,
                    invalid: None,
                },
                status: Some(LinkStatus::Shared),
            }
        })
        .collect();
    let file = tempfile::NamedTempFile::new().unwrap();
    let snapshot = DomainPerspective { links };
    std::fs::write(
        file.path(),
        serde_json::to_string_pretty(&snapshot).unwrap(),
    )
    .unwrap();

    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let result = map
        .dispatch(
            "runtime.importData",
            json!({
                "type": "perspective",
                "filePath": file.path().to_str().unwrap(),
                "perspectiveUuid": uuid,
            }),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap();
    assert_eq!(result, json!({ "imported": 3, "covered": 0, "skipped": 0 }));

    let stored = perspective
        .get_links(&crate::types::LinkQuery {
            source: None,
            target: None,
            predicate: None,
            from_date: None,
            until_date: None,
            limit: None,
        })
        .await
        .unwrap();
    assert_eq!(stored.len(), 3);
    assert!(stored.iter().all(|l| l.proof.valid == Some(true)));
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

/// `runtime.setLogFilter` turns `ws_rpc=debug` on for its ttl, and `runtime.logFilter`
/// reports it and when it reverts; after the ttl the startup filter is back by itself. Only
/// the operator may do either: an app, a user, even with every capability, gets 403.
#[tokio::test]
async fn the_operator_sets_a_log_filter_for_a_while_and_nobody_else_may() {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let debug_ws_rpc = || {
        crate::log_filter::state().enabled(
            &log::Metadata::builder()
                .target("rust_executor::api::ws_rpc")
                .level(log::Level::Debug)
                .build(),
        )
    };
    let set = json!({ "filter": "ws_rpc=debug", "ttlSeconds": 1 });

    for ctx in [
        RequestContext::app_for_test(vec![]),
        RequestContext::app_for_test(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]),
        RequestContext::user_for_test("did:key:log-filter-user", vec![]),
    ] {
        let ctx = Arc::new(ctx);
        for (method, params) in [
            ("runtime.setLogFilter", set.clone()),
            ("runtime.logFilter", json!({})),
        ] {
            let refused = map.dispatch(method, params, ctx.clone()).await.unwrap_err();
            assert_eq!(refused.code, 403, "{method}: {}", refused.message);
        }
    }
    assert!(!debug_ws_rpc(), "a refused call changed the filter");

    let operator = Arc::new(RequestContext::operator_for_test());
    let reply = map
        .dispatch("runtime.setLogFilter", set, operator.clone())
        .await
        .unwrap();
    // The in-force filter, with the rule that keeps MCP session ids out of the log.
    assert_eq!(reply["overrideFilter"], "ws_rpc=debug,rmcp=warn");
    assert!(reply["revertsAt"].is_string(), "{reply}");
    assert!(debug_ws_rpc());
    let status = map
        .dispatch("runtime.logFilter", json!({}), operator.clone())
        .await
        .unwrap();
    assert_eq!(status["overrideFilter"], "ws_rpc=debug,rmcp=warn");

    tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    assert!(!debug_ws_rpc(), "the filter did not revert after its ttl");
    let status = map
        .dispatch("runtime.logFilter", json!({}), operator.clone())
        .await
        .unwrap();
    assert_eq!(status["overrideFilter"], serde_json::Value::Null);
    assert_eq!(status["revertsAt"], serde_json::Value::Null);

    for bad in [
        json!({ "filter": "ws_rpc=loud" }),
        json!({ "filter": "ws_rpc=debug", "ttlSeconds": 0 }),
        json!({ "filter": "ws_rpc=debug", "ttlSeconds": 14401 }),
    ] {
        let refused = map
            .dispatch("runtime.setLogFilter", bad.clone(), operator.clone())
            .await
            .unwrap_err();
        assert_eq!(refused.code, 400, "{bad}");
    }
}
