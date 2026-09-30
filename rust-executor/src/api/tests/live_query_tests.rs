//! Live query RPCs (`perspective.subscribeQuery`, `modelSubscribe`,
//! `resyncSubscription`, `disposeQuery`), driven through the real
//! `HandlerMap` so each test also proves the method and its contract are
//! registered.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::api::tests::support::{admin_conn_ctx, admin_ctx, registered_perspective};
use crate::api::ws_handler::{build_handler_map, WsRpcError};
use crate::types::RequestContext;

/// A context that holds no capability at all.
fn no_cap_ctx() -> Arc<RequestContext> {
    Arc::new(RequestContext {
        capabilities: Ok(vec![]),
        auto_permit_cap_requests: false,
        auth_token: "not-a-token".into(),
        is_admin_credential: false,
        user_email: None,
        user_did: None,
        cancel_token: None,
        connection_id: None,
    })
}

async fn call(method: &str, params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    build_handler_map().dispatch(method, params, ctx).await
}

const TODO_SDNA: &str = r#"{
  "target_class": "test://Todo",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"test://Todo"}
  ],
  "properties": [
    {"path":"test://title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://title","target":"value"}]}
  ]
}"#;

#[test]
fn keepalive_methods_are_gone() {
    let names = build_handler_map().method_names();
    for gone in ["perspective.keepAliveQuery", "perspective.keepAliveSparql"] {
        assert!(!names.contains(&gone.to_string()), "{gone}");
    }
    assert!(names.contains(&"perspective.resyncSubscription".to_string()));
}
// ── subscribe replies and resync ────────────────────────────────────────

#[tokio::test]
async fn subscribe_replies_json_at_revision_zero() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let reply = call(
        "perspective.subscribeQuery",
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s <test://none> ?o }" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(reply["result"], json!([]));
    assert_eq!(reply["revision"], json!(0));
    assert!(reply["subscriptionId"].is_string());

    let reply = call(
        "perspective.modelSubscribe",
        json!({ "uuid": p.0, "class_name": "Todo", "query_json": "{}" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(reply["result"]["instances"], json!([]));
    assert_eq!(reply["revision"], json!(0));
    assert!(reply["subscriptionId"].is_string());
}

#[tokio::test]
async fn resync_subscription_returns_revision_and_result() {
    let p = registered_perspective(&[]).await;
    let sub = call(
        "perspective.subscribeQuery",
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s <test://none> ?o }" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    let id = sub["subscriptionId"].clone();
    let reply = call(
        "perspective.resyncSubscription",
        json!({ "uuid": p.0, "subscriptionId": id }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(reply, json!({ "revision": 0, "result": [] }));

    for (subscription, connection) in [(json!("unknown"), "c"), (id.clone(), "other")] {
        let err = call(
            "perspective.resyncSubscription",
            json!({ "uuid": p.0, "subscriptionId": subscription }),
            admin_conn_ctx(connection),
        )
        .await
        .expect_err("not this connection's subscription");
        assert_eq!(err.code, 404);
    }
    let err = call(
        "perspective.resyncSubscription",
        json!({ "uuid": p.0, "subscriptionId": id }),
        no_cap_ctx(),
    )
    .await
    .expect_err("no capability");
    assert_eq!(err.code, 403);
}

#[tokio::test]
async fn subscribing_needs_a_socket() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    for (method, params) in [
        (
            "perspective.subscribeQuery",
            json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s ?p ?o }" }),
        ),
        (
            "perspective.modelSubscribe",
            json!({ "uuid": p.0, "class_name": "Todo", "query_json": "{}" }),
        ),
    ] {
        let err = call(method, params, admin_ctx())
            .await
            .expect_err("no connection (REST)");
        assert_eq!(err.code, 400, "{method}");
    }
}

#[tokio::test]
async fn dispose_query_ends_only_this_connections_subscription() {
    let p = registered_perspective(&[]).await;
    let sub = call(
        "perspective.subscribeQuery",
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s ?p ?o }" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    let dispose = |connection: &'static str| {
        call(
            "perspective.disposeQuery",
            json!({ "uuid": p.0, "subscriptionId": sub["subscriptionId"] }),
            admin_conn_ctx(connection),
        )
    };
    assert_eq!(dispose("other").await.unwrap(), json!(false));
    assert_eq!(dispose("c").await.unwrap(), json!(true));
    assert_eq!(dispose("c").await.unwrap(), json!(false));
}
