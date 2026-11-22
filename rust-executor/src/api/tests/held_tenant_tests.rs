//! A `Held` tenant's session answers `not-admitted` to every call except reads, `data.export*`,
//! `host.usage` and `host.leave` (AR-05 4.5.7, AR-05-R64). The one place sessions are admitted
//! is the dispatcher's `admit_entry`, so the test walks `HandlerMap::admit`, the dispatcher's
//! steps before the handler.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::get_user_default_capabilities;
use crate::api::ws_handler::{build_handler_map, WsRpcError};
use crate::db::TenantRow;
use crate::perspectives::interpretation_test_support::ensure_db_init;
use crate::types::RequestContext;

const HELD_DID: &str = "did:key:held-tenant";
const ACTIVE_DID: &str = "did:key:active-tenant";

fn tenant(id: &str, state: &str, device_did: &str) -> TenantRow {
    TenantRow {
        id: id.into(),
        did_scid: format!("did:scid:coasys:1:{id}"),
        device_key_name: Some(format!("user:{id}")),
        device_did: Some(device_did.into()),
        state: state.into(),
        joined_via: "\"anyone\"".into(),
        created_at: 10,
        activated_at: Some(11),
        detached_at: None,
        pending_expires_at: None,
    }
}

fn world() {
    ensure_db_init();
    crate::config::set_global_config(crate::config::Ad4mConfig::default());
    crate::bots::set_loaded(true);
    let db = crate::db_backend::db_backend();
    crate::db::Ad4mDb::with_global_instance(|db| db.clear_hosting_for_test().unwrap());
    db.insert_tenant(&tenant("t-held", "held", HELD_DID))
        .unwrap();
    db.insert_tenant(&tenant("t-active", "active", ACTIVE_DID))
        .unwrap();
}

fn session(did: &str) -> Arc<RequestContext> {
    Arc::new(RequestContext::user_for_test(
        did,
        get_user_default_capabilities(),
    ))
}

async fn admit(method: &str, params: Value, did: &str) -> Result<(), WsRpcError> {
    build_handler_map()
        .admit(method, &params, &session(did))
        .await
}

fn not_admitted(answer: &Result<(), WsRpcError>) -> bool {
    matches!(answer, Err(e) if e.code == 403 && e.message.starts_with("not-admitted"))
}

/// Calls of each kind a tenant makes: writes, AI calls, joining a space.
fn refused_calls() -> Vec<(&'static str, Value)> {
    vec![
        ("perspective.createBatch", json!({ "uuid": "p" })),
        ("ai.embed", json!({ "modelId": "m", "text": "t" })),
        ("neighbourhood.join", json!({ "url": "neighbourhood://Qm" })),
    ]
}

#[tokio::test]
async fn a_held_tenants_writes_ai_calls_and_space_joins_answer_not_admitted() {
    world();
    for (method, params) in refused_calls() {
        let answer = admit(method, params, HELD_DID).await;
        assert!(not_admitted(&answer), "{method}: {answer:?}");
    }
}

#[tokio::test]
async fn a_held_tenant_may_read_see_its_usage_and_leave() {
    world();
    for (method, params) in [
        ("perspective.all", json!(null)),
        ("host.usage", json!(null)),
        ("host.leave", json!({ "keepData": true })),
        ("data.exportPrepare", json!({})),
        ("data.exportChunk", json!({ "exportId": "e", "index": 0 })),
    ] {
        let answer = admit(method, params, HELD_DID).await;
        assert!(!not_admitted(&answer), "{method}: {answer:?}");
    }
}

#[tokio::test]
async fn an_active_tenant_is_not_refused_as_held() {
    world();
    for (method, params) in refused_calls() {
        let answer = admit(method, params, ACTIVE_DID).await;
        assert!(!not_admitted(&answer), "{method}: {answer:?}");
    }
}

#[tokio::test]
async fn a_held_tenant_turned_active_is_admitted_at_once() {
    world();
    let db = crate::db_backend::db_backend();
    let held = db.tenant("t-held").unwrap().unwrap();
    let mut active = held.clone();
    active.state = "active".into();
    assert!(db.transition_tenant("t-held", &["held"], &active).unwrap());
    let answer = admit("ai.embed", json!({ "modelId": "m", "text": "t" }), HELD_DID).await;
    assert!(!not_admitted(&answer), "{answer:?}");
}
