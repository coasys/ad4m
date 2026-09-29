//! WS-RPC handlers of the executor protocol (batches, shape and agent
//! reads, live queries), driven through the real `HandlerMap` so each test
//! also proves the method is registered.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::ALL_CAPABILITY;
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
        connection_id: None,
    })
}

/// `admin_ctx()` on the WS RPC connection `connection_id`.
pub(crate) fn admin_conn_ctx(connection_id: &str) -> Arc<RequestContext> {
    let mut ctx = (*admin_ctx()).clone();
    ctx.connection_id = Some(connection_id.to_string());
    Arc::new(ctx)
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
        connection_id: None,
    })
}

pub(crate) async fn call(
    method: &str,
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    build_handler_map().dispatch(method, params, ctx).await
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

// ── perspective.discardBatch ────────────────────────────────────────────

#[tokio::test]
async fn discard_batch_drops_an_open_batch_once() {
    let p = registered_perspective(&[]).await;
    let batch_id = call(
        "perspective.createBatch",
        json!({ "uuid": p.0 }),
        admin_ctx(),
    )
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
    let a = call(
        "perspective.createBatch",
        json!({ "uuid": p.0 }),
        admin_ctx(),
    )
    .await
    .unwrap();
    let b = call(
        "perspective.createBatch",
        json!({ "uuid": p.0 }),
        admin_ctx(),
    )
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

// ── perspective.getAllShacl { names } ───────────────────────────────────

pub(crate) const TODO_SDNA: &str = r#"{
  "target_class": "test://Todo",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"test://Todo"}
  ],
  "properties": [
    {"path":"test://title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://title","target":"value"}]}
  ]
}"#;

pub(crate) const NOTE_SDNA: &str = r#"{
  "target_class": "test://Note",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"test://Note"}
  ],
  "properties": [
    {"path":"test://body","name":"body","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://body","target":"value"}]}
  ]
}"#;

fn shape_names(reply: &Value) -> Vec<String> {
    let mut names: Vec<String> = reply
        .as_array()
        .expect("array")
        .iter()
        .map(|e| e["name"].as_str().unwrap().to_string())
        .collect();
    names.sort();
    names
}

#[tokio::test]
async fn get_all_shacl_without_names_returns_every_shape() {
    let p = registered_perspective(&[("Todo", TODO_SDNA), ("Note", NOTE_SDNA)]).await;
    let all = call(
        "perspective.getAllShacl",
        json!({ "uuid": p.0 }),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(shape_names(&all), vec!["Note", "Todo"]);
    let with_null = call(
        "perspective.getAllShacl",
        json!({ "uuid": p.0, "names": null }),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(with_null, all, "names: null is the same as no filter");
}

#[tokio::test]
async fn get_all_shacl_names_filters_the_reply() {
    let p = registered_perspective(&[("Todo", TODO_SDNA), ("Note", NOTE_SDNA)]).await;
    let all = call(
        "perspective.getAllShacl",
        json!({ "uuid": p.0 }),
        admin_ctx(),
    )
    .await
    .unwrap();
    let only_todo = call(
        "perspective.getAllShacl",
        json!({ "uuid": p.0, "names": ["Todo", "Unknown"] }),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(shape_names(&only_todo), vec!["Todo"]);
    let todo_in_all = all
        .as_array()
        .unwrap()
        .iter()
        .find(|e| e["name"] == "Todo")
        .unwrap();
    assert_eq!(
        &only_todo[0], todo_in_all,
        "same entry as the unfiltered read"
    );

    let none = call(
        "perspective.getAllShacl",
        json!({ "uuid": p.0, "names": [] }),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(none, json!([]));
}

#[tokio::test]
async fn get_all_shacl_rejects_malformed_names() {
    let p = registered_perspective(&[]).await;
    for bad in [json!("Todo"), json!([1]), json!({ "a": 1 })] {
        let err = call(
            "perspective.getAllShacl",
            json!({ "uuid": p.0, "names": bad }),
            admin_ctx(),
        )
        .await
        .expect_err("malformed names");
        assert_eq!(err.code, 400);
    }
}

// ── agent.byDIDs ────────────────────────────────────

fn init_agent() -> String {
    crate::test_utils::setup_wallet();
    crate::perspectives::interpretation_test_support::ensure_db_init();
    crate::agent::AgentService::init_global_test_instance();
    crate::agent::did()
}

#[tokio::test]
async fn agents_by_dids_aligns_with_input_and_matches_by_did() {
    let me = init_agent();
    let single = call("agent.byDid", json!({ "did": me }), admin_ctx())
        .await
        .unwrap();
    // The test agent has a DID but no stored profile, so `single` may be
    // `null`; the point is that each entry equals the single-item reply.

    let many = call(
        "agent.byDIDs",
        json!({ "dids": [me, "did:key:unknown", me] }),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(many, json!([single, null, single]));
    assert_eq!(many.as_array().unwrap().len(), 3);

    let empty = call("agent.byDIDs", json!({ "dids": [] }), admin_ctx())
        .await
        .unwrap();
    assert_eq!(empty, json!([]));
}

#[tokio::test]
async fn agents_by_dids_checks_params_and_capability() {
    init_agent();
    let err = call("agent.byDIDs", json!({}), admin_ctx())
        .await
        .expect_err("dids is required");
    assert_eq!(err.code, 400);
    let err = call("agent.byDIDs", json!({ "dids": [] }), no_cap_ctx())
        .await
        .expect_err("no capability");
    assert_eq!(err.code, 403);
}

#[tokio::test]
async fn agents_by_dids_rejects_more_than_the_cap() {
    use crate::api::agent_ws::MAX_AGENTS_BY_DIDS;
    init_agent();
    let at_cap: Vec<String> = (0..MAX_AGENTS_BY_DIDS)
        .map(|i| format!("did:key:unknown-{i}"))
        .collect();
    let reply = call("agent.byDIDs", json!({ "dids": at_cap }), admin_ctx())
        .await
        .expect("the cap itself is allowed");
    assert_eq!(reply.as_array().unwrap().len(), MAX_AGENTS_BY_DIDS);

    // Duplicates count toward the cap: it bounds the reply, not the lookups.
    let over: Vec<&str> = vec!["did:key:same"; MAX_AGENTS_BY_DIDS + 1];
    let err = call("agent.byDIDs", json!({ "dids": over }), admin_ctx())
        .await
        .expect_err("over the cap");
    assert_eq!(err.code, 400);
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
