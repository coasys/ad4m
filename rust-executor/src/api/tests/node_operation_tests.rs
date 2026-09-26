//! A multi-user user session acts for its user only. The calls below act on the node
//! itself (its main key, wallet, host files, database or node-wide AI settings), and the
//! default user capabilities cover the capabilities they check, so before these guards a
//! user token reached all of them.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::{get_user_default_capabilities, ALL_CAPABILITY};
use crate::agent::{signing_key_id, signing_key_id_for_context, AgentContext, AgentService};
use crate::api::ws_handler::build_handler_map;
use crate::types::RequestContext;

fn user_session(email: &str) -> Arc<RequestContext> {
    Arc::new(RequestContext {
        capabilities: Ok(get_user_default_capabilities()),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: false,
        user_email: Some(email.to_string()),
        user_did: None,
        cancel_token: None,
    })
}

fn operator_session() -> Arc<RequestContext> {
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

async fn call(op: &str, params: Value, ctx: Arc<RequestContext>) -> Result<Value, (u16, String)> {
    build_handler_map()
        .dispatch(op, params, ctx)
        .await
        .map_err(|e| (e.code, e.message))
}

#[tokio::test]
async fn user_sessions_cannot_call_node_operations() {
    let dir = tempfile::tempdir().unwrap();
    let export_path = dir.path().join("dump.json");
    let calls = [
        ("agent.generate", json!({ "passphrase": "p" })),
        ("agent.lock", json!({ "passphrase": "p" })),
        (
            "agent.unlock",
            json!({ "passphrase": "p", "holochain": false }),
        ),
        (
            "runtime.exportData",
            json!({ "exportType": "db", "filePath": export_path.to_str().unwrap() }),
        ),
        (
            "runtime.importData",
            json!({ "importType": "db", "filePath": export_path.to_str().unwrap() }),
        ),
        ("ai.discoverModels", json!({})),
        ("ai.addModel", json!({})),
        ("ai.updateModel", json!({})),
        ("ai.removeModel", json!({})),
        ("ai.setDefaultModel", json!({})),
        ("agent.addEntanglementProofs", json!({})),
        ("agent.deleteEntanglementProofs", json!({})),
        ("agent.entanglementProofPreflight", json!({})),
    ];
    for (op, params) in calls {
        let result = call(op, params, user_session("guard.user@example.org")).await;
        match result {
            Err((403, message)) => assert!(message.contains(op), "{op}: {message}"),
            other => panic!("{op} must refuse a user session, got {other:?}"),
        }
    }
    assert!(
        !export_path.exists(),
        "a refused export must not write the file"
    );
}

#[tokio::test]
async fn a_user_session_signs_as_the_user_not_the_node() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let email = "signing.user@example.org";
    AgentService::ensure_user_key_exists(email).unwrap();

    let result = call(
        "agent.sign",
        json!({ "message": "hello" }),
        user_session(email),
    )
    .await
    .expect("a user may sign as themselves");

    let user_key =
        signing_key_id_for_context(&AgentContext::for_user_email(email.to_string())).unwrap();
    assert_eq!(result["publicKey"], json!(user_key));
    assert_ne!(result["publicKey"], json!(signing_key_id()));

    let user_did = AgentService::get_user_did_by_email(email).unwrap();
    let signature = result["signature"].as_str().unwrap();
    assert!(
        crate::agent::signatures::verify_string_signed_by_did(&user_did, "hello", signature)
            .unwrap(),
        "the signature must verify against the user's own DID"
    );
}

#[tokio::test]
async fn the_operator_still_signs_as_the_node() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let result = call(
        "agent.sign",
        json!({ "message": "hello" }),
        operator_session(),
    )
    .await
    .expect("the operator signs as the node");
    assert_eq!(result["publicKey"], json!(signing_key_id()));
}

#[tokio::test]
async fn a_user_sets_their_own_dm_language_not_the_nodes() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    // Profiles persist under test_data, so each run uses a fresh user.
    let email = &format!("dm.user.{}@example.org", uuid::Uuid::new_v4());
    AgentService::ensure_user_key_exists(email).unwrap();
    let node_dm_before = AgentService::with_global_instance(|a| {
        a.agent
            .as_ref()
            .and_then(|agent| agent.direct_message_language.clone())
    });

    let returned = call(
        "agent.updateProfile",
        json!({ "dmLanguage": "lang://the-users-own" }),
        user_session(email),
    )
    .await
    .expect("a user may set their own DM language");
    assert_eq!(
        returned["did"],
        json!(AgentService::get_user_did_by_email(email).unwrap()),
        "the call must return the user's own profile, not the node's"
    );

    let node_dm_after = AgentService::with_global_instance(|a| {
        a.agent
            .as_ref()
            .and_then(|agent| agent.direct_message_language.clone())
    });
    assert_eq!(
        node_dm_before, node_dm_after,
        "a user session changed the node's DM language"
    );
    let profile = AgentService::with_global_instance(|a| a.load_user_agent_profile(email))
        .unwrap()
        .expect("the user's profile now exists");
    assert_eq!(
        profile.direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );

    // A later perspective-only update keeps the DM language.
    call(
        "agent.updateProfile",
        json!({ "publicPerspective": { "links": [] } }),
        user_session(email),
    )
    .await
    .expect("a user may update their own perspective");
    let profile = AgentService::with_global_instance(|a| a.load_user_agent_profile(email))
        .unwrap()
        .expect("the user's profile exists");
    assert_eq!(
        profile.direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );
}
