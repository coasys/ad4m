//! Live query RPCs (`perspective.subscribe`,
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

// ── subscribe replies and resync ────────────────────────────────────────

#[tokio::test]
async fn subscribe_replies_json_at_revision_zero() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let reply = call(
        "perspective.subscribe",
        json!({ "uuid": p.0, "sparql": "SELECT ?s WHERE { ?s <test://none> ?o }",
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
        "perspective.subscribe",
        json!({ "uuid": p.0, "model": { "className": "Todo", "queryJson": "{}" },
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
        "perspective.subscribe",
        json!({ "uuid": p.0, "sparql": "SELECT ?s WHERE { ?s <test://none> ?o }",
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
    for (method, params) in opens(&p.0, "s") {
        let err = call(method, params, admin_ctx())
            .await
            .expect_err("no connection (REST)");
        assert_eq!(err.code, 400, "{method}");
    }
}

/// A subscription re-runs one query; a cursor would re-run the same page
/// forever, so `after` (even `""`) is a bad request on a model `subscribe`.
#[tokio::test]
async fn model_subscribe_refuses_a_cursor() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    for (classes, after) in [
        (json!({ "className": "Todo" }), json!("")),
        (json!({ "className": "Todo" }), json!("c1")),
        (json!({ "classNames": ["Todo"] }), json!("")),
    ] {
        let mut model = json!({ "queryJson": json!({ "limit": 5, "after": after }).to_string() });
        model
            .as_object_mut()
            .unwrap()
            .extend(classes.as_object().unwrap().clone());
        let params = json!({ "uuid": p.0, "model": model, "subscriptionId": "cursor-sub" });
        let err = call("perspective.subscribe", params, admin_conn_ctx("c"))
            .await
            .expect_err("a subscription cannot page by cursor");
        assert_eq!(err.code, 400, "{classes} after={after}");
        assert!(err.message.contains("after"), "{}", err.message);
    }
    // Without a cursor the same query subscribes.
    call(
        "perspective.subscribe",
        json!({ "uuid": p.0, "model": { "className": "Todo", "queryJson": r#"{"limit":5}"# },
                "subscriptionId": "cursor-sub" }),
        admin_conn_ctx("c"),
    )
    .await
    .expect("no cursor: subscribes");
}

#[tokio::test]
async fn dispose_query_ends_only_this_connections_subscription() {
    let p = registered_perspective(&[]).await;
    let sub = call(
        "perspective.subscribe",
        json!({ "uuid": p.0, "sparql": "SELECT ?s WHERE { ?s ?p ?o }", "subscriptionId": "s" }),
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

/// `perspective.subscribe` with a SPARQL query and with a model query, `subscriptionId` set to
/// `id`.
fn opens(uuid: &str, id: &str) -> [(&'static str, Value); 2] {
    [
        (
            "perspective.subscribe",
            json!({ "uuid": uuid, "sparql": "SELECT ?s WHERE { ?s ?p ?o }", "subscriptionId": id }),
        ),
        (
            "perspective.subscribe",
            json!({ "uuid": uuid, "model": { "className": "Todo", "queryJson": "{}" }, "subscriptionId": id }),
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
async fn a_subscribe_with_a_malformed_id_gets_400_and_registers_nothing() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let too_long = "a".repeat(65);
    for id in ["", "line\nbreak", "a b", too_long.as_str()] {
        for (method, params) in opens(&p.0, id) {
            let err = call(method, params, admin_conn_ctx("c"))
                .await
                .expect_err("malformed id");
            assert_eq!(err.code, 400, "{method} with {id:?}: {}", err.message);
        }
        let disposed = call(
            "perspective.disposeQuery",
            json!({ "uuid": p.0, "subscriptionId": id }),
            admin_conn_ctx("c"),
        )
        .await
        .unwrap();
        assert_eq!(disposed, json!(false), "{id:?} registered");
    }
    // The longest valid id opens.
    let longest = "a".repeat(64);
    let [(method, params), _] = opens(&p.0, &longest);
    call(method, params, admin_conn_ctx("c")).await.unwrap();
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
    assert_eq!(marked, ["perspective.subscribe"]);
    for name in marked {
        assert!(map.opens(name), "{name}: the connection reads the marker");
    }
    assert!(!map.opens("perspective.disposeQuery"));
}

/// Exactly one of `model` and `sparql`: both, neither and unknown members are refused, and
/// nothing is registered.
#[tokio::test]
async fn subscribe_takes_exactly_one_of_model_and_sparql() {
    let p = registered_perspective(&[("Todo", TODO_SDNA)]).await;
    let sparql = "SELECT ?s WHERE { ?s ?p ?o }";
    let model = json!({ "className": "Todo", "queryJson": "{}" });
    for params in [
        json!({ "uuid": p.0, "subscriptionId": "bad", "sparql": sparql, "model": model }),
        json!({ "uuid": p.0, "subscriptionId": "bad" }),
        json!({ "uuid": p.0, "subscriptionId": "bad", "sparql": sparql, "extra": 1 }),
        json!({ "uuid": p.0, "subscriptionId": "bad", "model": { "queryJson": "{}" } }),
        json!({ "uuid": p.0, "subscriptionId": "bad", "model": { "className": "Todo", "classNames": ["Todo"], "queryJson": "{}" } }),
    ] {
        let err = call("perspective.subscribe", params.clone(), admin_conn_ctx("c"))
            .await
            .expect_err("not exactly one query");
        assert_eq!(err.code, 400, "{params}: {}", err.message);
    }
    let disposed = call(
        "perspective.disposeQuery",
        json!({ "uuid": p.0, "subscriptionId": "bad" }),
        admin_conn_ctx("c"),
    )
    .await
    .unwrap();
    assert_eq!(disposed, json!(false), "a refused call registered");
}
