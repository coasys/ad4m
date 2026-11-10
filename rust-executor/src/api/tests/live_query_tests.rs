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
    Arc::new(RequestContext::app_for_test(vec![]))
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
    let map = build_handler_map();
    let names: Vec<&str> = map.specs().iter().map(|s| s.name.as_str()).collect();
    for gone in ["perspective.keepAliveQuery", "perspective.keepAliveSparql"] {
        assert!(!names.contains(&gone), "{gone}");
    }
    assert!(names.contains(&"perspective.resyncSubscription"));
}
// ── subscribe replies and resync ────────────────────────────────────────

#[tokio::test]
async fn subscribe_replies_json_at_revision_zero() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let reply = call(
        "perspective.subscribeQuery",
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s <test://none> ?o }",
                "subscriptionId": "query-sub" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(reply["result"], json!([]));
    assert_eq!(reply["revision"], json!(0));
    assert_eq!(
        reply["subscriptionId"],
        json!("query-sub"),
        "the client's id"
    );

    let reply = call(
        "perspective.modelSubscribe",
        json!({ "uuid": p.0, "class_name": "Todo", "query_json": "{}",
                "subscriptionId": "model-sub" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(reply["result"]["instances"], json!([]));
    assert_eq!(reply["revision"], json!(0));
    assert_eq!(
        reply["subscriptionId"],
        json!("model-sub"),
        "the client's id"
    );
}

#[tokio::test]
async fn resync_subscription_returns_revision_and_result() {
    let p = registered_perspective(&[]).await;
    let sub = call(
        "perspective.subscribeQuery",
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s <test://none> ?o }",
                "subscriptionId": "s" }),
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
            json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s ?p ?o }", "subscriptionId": "s" }),
        ),
        (
            "perspective.modelSubscribe",
            json!({ "uuid": p.0, "class_name": "Todo", "query_json": "{}", "subscriptionId": "s" }),
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
        json!({ "uuid": p.0, "query": "SELECT ?s WHERE { ?s ?p ?o }", "subscriptionId": "s" }),
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

// ── client-minted ids ───────────────────────────────────────────────────

/// `perspective.subscribeQuery` and `modelSubscribe` with `subscriptionId` set to `id`.
fn opens(uuid: &str, id: &str) -> [(&'static str, Value); 2] {
    [
        (
            "perspective.subscribeQuery",
            json!({ "uuid": uuid, "query": "SELECT ?s WHERE { ?s ?p ?o }", "subscriptionId": id }),
        ),
        (
            "perspective.modelSubscribe",
            json!({ "uuid": uuid, "class_name": "Todo", "query_json": "{}", "subscriptionId": id }),
        ),
    ]
}

#[tokio::test]
async fn a_subscribe_needs_the_clients_id() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    for (method, mut params) in opens(&p.0, "s") {
        params.as_object_mut().unwrap().remove("subscriptionId");
        let err = call(method, params, admin_conn_ctx("c"))
            .await
            .expect_err("no subscriptionId");
        assert_eq!(err.code, 400, "{method}: {}", err.message);
    }
}

#[tokio::test]
async fn a_subscribe_with_a_taken_id_gets_409_and_registers_nothing() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let [(method, params), _] = opens(&p.0, "taken");
    call(method, params, admin_conn_ctx("c")).await.unwrap();
    for connection in ["c", "other"] {
        for (method, params) in opens(&p.0, "taken") {
            let err = call(method, params, admin_conn_ctx(connection))
                .await
                .expect_err("the id is taken");
            assert_eq!(err.code, 409, "{method} on {connection}: {}", err.message);
        }
    }
    // The first subscription is the only one, still the first connection's.
    let dispose = |connection: &'static str| {
        call(
            "perspective.disposeQuery",
            json!({ "uuid": p.0, "subscriptionId": "taken" }),
            admin_conn_ctx(connection),
        )
    };
    assert_eq!(dispose("other").await.unwrap(), json!(false));
    assert_eq!(dispose("c").await.unwrap(), json!(true));
    assert_eq!(dispose("c").await.unwrap(), json!(false));
}

/// A call whose cancel arrived while it opened ends its subscription: the client disposes
/// the id after the cancel, and that dispose may have run before the subscription was
/// registered.
#[tokio::test]
async fn a_subscribe_cancelled_while_it_opens_leaves_nothing() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    for (method, params) in opens(&p.0, "cancelled") {
        let mut ctx = (*admin_conn_ctx("c")).clone();
        let token = tokio_util::sync::CancellationToken::new();
        token.cancel();
        ctx.cancel_token = Some(token);
        let err = call(method, params, Arc::new(ctx))
            .await
            .expect_err("cancelled");
        assert_eq!(err.code, 499, "{method}: {}", err.message);
        let resync = call(
            "perspective.resyncSubscription",
            json!({ "uuid": p.0, "subscriptionId": "cancelled" }),
            admin_conn_ctx("c"),
        )
        .await
        .expect_err("nothing registered");
        assert_eq!(resync.code, 404, "{method}");
    }
}

#[test]
fn the_live_query_opens_are_the_methods_marked_opens() {
    let map = build_handler_map();
    let marked: Vec<&str> = map
        .specs()
        .iter()
        .filter(|s| s.opens)
        .map(|s| s.name.as_str())
        .collect();
    assert_eq!(
        marked,
        ["perspective.modelSubscribe", "perspective.subscribeQuery"]
    );
    for name in marked {
        assert!(map.opens(name), "{name}: the connection reads the marker");
    }
    assert!(!map.opens("perspective.disposeQuery"));
}
