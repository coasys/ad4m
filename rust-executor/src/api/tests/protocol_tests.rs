//! WS-RPC handlers of the executor protocol (batches, shape and agent
//! reads), driven through the real `HandlerMap` so each test
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
