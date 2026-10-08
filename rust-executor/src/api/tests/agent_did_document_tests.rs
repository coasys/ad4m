//! #1229: no DID document the executor returns or emits may contain a private
//! key. One test per way a document leaves: `agent.status` (main agent and
//! managed user), what `agent.generate` and `agent.unlock` return, the
//! `agent.lock` reply and the `agent-status-changed` event, `agent.json` on
//! disk, and an `agent.json` written before the fix.
//!
//! Every check looks for the actual private-key values of the agent's keypair
//! (`private_key_values`), not only for a `privateKey` field name, and also
//! checks that the public document is still there — dropping the document
//! entirely would pass a secrets-only check.
//!
//! These share the global wallet and `AgentService` with other tests; each one
//! ends by re-running `setup_agent()` so the next test finds them consistent.

use std::sync::Arc;
use std::time::Duration;

use serde_json::json;

use super::support::admin_ctx;
use crate::agent::capabilities::ALL_CAPABILITY;
use crate::agent::AgentService;
use crate::api::agent_ws::register_ws_handlers;
use crate::api::ws_handler::HandlerMap;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC};
use crate::test_utils::{
    assert_no_private_keys, private_key_values, setup_agent, setup_wallet,
    wallet_private_key_values,
};
use crate::types::RequestContext;

fn handlers() -> HandlerMap {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    map
}

/// The public half that must survive: the DID and the base58 public key of the
/// signing method.
fn assert_public_document_present(serialized: &str, did: &str, what: &str) {
    assert!(
        serialized.contains(did),
        "{what} lost the DID: {serialized}"
    );
    assert!(
        serialized.contains("publicKeyBase58"),
        "{what} lost the public keys: {serialized}"
    );
}

fn main_did() -> String {
    AgentService::with_global_instance(|s| s.did.clone().expect("main agent DID"))
}

#[tokio::test]
async fn agent_status_for_the_main_agent_carries_no_private_key() {
    setup_wallet();
    setup_agent();
    let secrets = wallet_private_key_values("main");

    let reply = handlers()
        .dispatch("agent.status", json!({}), admin_ctx())
        .await
        .expect("agent.status");
    let reply = reply.to_string();
    assert_no_private_keys(&reply, &secrets, "agent.status");
    assert_public_document_present(&reply, &main_did(), "agent.status");

    // Language code reads the same document through `agent.didDocument()`.
    let for_languages = serde_json::to_string(&crate::agent::did_document()).unwrap();
    assert_no_private_keys(&for_languages, &secrets, "agent.didDocument()");
    assert_public_document_present(&for_languages, &main_did(), "agent.didDocument()");
}

/// `agent.generate` replies with `dump()` right after `create_new_keys()` and
/// `save()` (its language loading follows and does not touch the document).
#[tokio::test]
async fn a_generated_agent_reply_and_agent_json_carry_no_private_key() {
    setup_wallet();
    setup_agent();
    let tmp = tempfile::tempdir().unwrap();
    let app_path = tmp.path().to_str().unwrap().to_string();
    std::fs::create_dir_all(format!("{app_path}/ad4m")).unwrap();

    let mut service = AgentService::new(app_path.clone());
    service.create_new_keys();
    service.save("pw-1229".to_string());
    let reply = serde_json::to_string(&service.dump()).unwrap();
    let did = service.did.clone().unwrap();
    let secrets = wallet_private_key_values("main");
    let on_disk = std::fs::read_to_string(format!("{app_path}/ad4m/agent.json")).unwrap();
    setup_agent();

    assert_no_private_keys(&reply, &secrets, "agent.generate reply");
    assert_public_document_present(&reply, &did, "agent.generate reply");
    assert_no_private_keys(&on_disk, &secrets, "agent.json after generate");
    assert_public_document_present(&on_disk, &did, "agent.json after generate");
}

/// `agent.lock` replies with the status and publishes it as
/// `agent-status-changed`; `agent.unlock` replies with `dump()` after
/// `AgentService::unlock`.
#[tokio::test]
async fn lock_reply_status_changed_event_and_unlock_carry_no_private_key() {
    setup_wallet();
    setup_agent();
    let secrets = wallet_private_key_values("main");
    let did = main_did();
    let mut events = get_global_pubsub()
        .await
        .subscribe(&AGENT_STATUS_CHANGED_TOPIC)
        .await;

    let lock_reply = handlers()
        .dispatch(
            "agent.lock",
            json!({ "passphrase": "pw-1229" }),
            admin_ctx(),
        )
        .await
        .expect("agent.lock")
        .to_string();
    let event = tokio::time::timeout(Duration::from_secs(5), events.recv())
        .await
        .expect("agent.lock publishes agent-status-changed")
        .expect("event");
    // Unlock before asserting, so a failure here leaves the wallet usable.
    let unlocked = AgentService::with_mutable_global_instance(|s| s.unlock("pw-1229".into()));
    let unlock_reply =
        serde_json::to_string(&AgentService::with_global_instance(|s| s.dump())).unwrap();
    setup_agent();

    unlocked.expect("unlock with the lock passphrase");
    assert_no_private_keys(&lock_reply, &secrets, "agent.lock reply");
    assert_public_document_present(&lock_reply, &did, "agent.lock reply");
    assert_no_private_keys(&event, &secrets, "agent-status-changed event");
    assert_public_document_present(&event, &did, "agent-status-changed event");
    assert_no_private_keys(&unlock_reply, &secrets, "agent.unlock reply");
    assert_public_document_present(&unlock_reply, &did, "agent.unlock reply");
}

/// An app dir holding an `agent.json` as written before the fix, for the
/// global main agent. Call after `setup_wallet(); setup_agent();`.
struct PreFixAgentFile {
    _tmp: tempfile::TempDir,
    app_path: String,
    agent_file: String,
    did: String,
    signing_key_id: String,
    keystore: String,
    secrets: Vec<String>,
}

impl PreFixAgentFile {
    fn new() -> Self {
        let backend = crate::wallet::wallet_backend();
        let public = backend.get_public_key("main").unwrap();
        let secret = backend.get_secret_key("main").unwrap();
        let secrets = private_key_values(&public, &secret);
        let did = main_did();
        let signing_key_id =
            AgentService::with_global_instance(|s| s.signing_key_id.clone().unwrap());
        let keystore = backend.export("pw-1229");

        // Exactly what the wallet used to produce: did-key's default config.
        let old_document = {
            use did_key::DIDCore;
            let keypair =
                did_key::from_existing_key::<did_key::Ed25519KeyPair>(&public, Some(&secret));
            serde_json::to_string(&keypair.get_did_document(did_key::CONFIG_LD_PRIVATE)).unwrap()
        };
        assert!(
            secrets.iter().all(|s| old_document.contains(s.as_str())),
            "the fixture must be the leaking document"
        );

        let tmp = tempfile::tempdir().unwrap();
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{app_path}/ad4m")).unwrap();
        let agent_file = format!("{app_path}/ad4m/agent.json");
        std::fs::write(
            &agent_file,
            json!({
                "did": did,
                "didDocument": old_document,
                "signingKeyId": signing_key_id,
                "keystore": keystore,
                "agent": null,
            })
            .to_string(),
        )
        .unwrap();

        Self {
            _tmp: tmp,
            app_path,
            agent_file,
            did,
            signing_key_id,
            keystore,
            secrets,
        }
    }
}

/// An `agent.json` written before the fix stores the document with both
/// private keys. Loading it must neither serve them (status, before and after
/// unlock) nor leave them on disk, and must keep the keystore intact.
#[tokio::test]
async fn an_agent_json_written_before_the_fix_is_served_and_rewritten_without_private_keys() {
    setup_wallet();
    setup_agent();
    let PreFixAgentFile {
        _tmp,
        app_path,
        agent_file,
        did,
        signing_key_id,
        keystore,
        secrets,
    } = PreFixAgentFile::new();

    let mut service = AgentService::new(app_path.clone());
    service.load();
    let locked_status = serde_json::to_string(&service.dump()).unwrap();
    let on_disk = std::fs::read_to_string(&agent_file).unwrap();
    let unlocked = service.unlock("pw-1229".into());
    let unlocked_status = serde_json::to_string(&service.dump()).unwrap();
    // A second load finds nothing to remove and must not rewrite the file.
    service.load();
    let on_disk_after_second_load = std::fs::read_to_string(&agent_file).unwrap();
    setup_agent();

    unlocked.expect("the keystore survives the rewrite and still unlocks");
    assert_no_private_keys(&locked_status, &secrets, "agent.status of a loaded node");
    assert_public_document_present(&locked_status, &did, "agent.status of a loaded node");
    assert_no_private_keys(&unlocked_status, &secrets, "agent.unlock of a loaded node");
    assert_public_document_present(&unlocked_status, &did, "agent.unlock of a loaded node");

    assert_no_private_keys(&on_disk, &secrets, "agent.json after load");
    let stored: serde_json::Value = serde_json::from_str(&on_disk).unwrap();
    assert_eq!(
        stored["keystore"],
        json!(keystore),
        "keystore must be kept verbatim"
    );
    assert_eq!(stored["did"], json!(did));
    assert_eq!(stored["signingKeyId"], json!(signing_key_id));
    assert_public_document_present(
        stored["didDocument"].as_str().unwrap(),
        &did,
        "stored didDocument",
    );
    assert_eq!(on_disk, on_disk_after_second_load);
    let agent_dir = std::path::Path::new(&agent_file).parent().unwrap();
    let mut entries: Vec<_> = std::fs::read_dir(agent_dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    entries.sort();
    assert_eq!(entries, vec!["agent.json"], "the rewrite left a temp file");
}

/// An operator who hardened `agent.json` to 0600 keeps 0600: the rewrite
/// replaces the inode, and a new inode would otherwise get the umask default.
#[cfg(unix)]
#[tokio::test]
async fn rewriting_a_hardened_agent_json_keeps_its_mode() {
    use std::os::unix::fs::PermissionsExt;
    setup_wallet();
    setup_agent();
    let fixture = PreFixAgentFile::new();
    let before = std::fs::read_to_string(&fixture.agent_file).unwrap();
    std::fs::set_permissions(&fixture.agent_file, std::fs::Permissions::from_mode(0o600)).unwrap();

    AgentService::new(fixture.app_path.clone()).load();
    let after = std::fs::read_to_string(&fixture.agent_file).unwrap();
    let mode = std::fs::metadata(&fixture.agent_file)
        .unwrap()
        .permissions()
        .mode()
        & 0o777;
    setup_agent();

    assert_ne!(before, after, "the fixture must take the rewrite path");
    assert_eq!(mode, 0o600, "the rewrite loosened a hardened agent.json");
}

/// If `agent.json` cannot be rewritten (read-only data dir), `load()` still
/// succeeds and serves a public document; the file is left as it was.
#[cfg(unix)]
#[tokio::test]
async fn when_agent_json_cannot_be_rewritten_load_still_serves_no_private_key() {
    use std::os::unix::fs::PermissionsExt;
    setup_wallet();
    setup_agent();
    let fixture = PreFixAgentFile::new();
    let before = std::fs::read_to_string(&fixture.agent_file).unwrap();
    let agent_dir = std::path::Path::new(&fixture.agent_file)
        .parent()
        .unwrap()
        .to_path_buf();
    std::fs::set_permissions(&agent_dir, std::fs::Permissions::from_mode(0o555)).unwrap();
    // Root ignores directory permissions; the branch under test is then
    // unreachable.
    let writable = std::fs::write(agent_dir.join("probe"), "").is_ok();

    let mut service = AgentService::new(fixture.app_path.clone());
    service.load();
    let status = serde_json::to_string(&service.dump()).unwrap();
    let after = std::fs::read_to_string(&fixture.agent_file).unwrap();
    let entries = std::fs::read_dir(&agent_dir).unwrap().count();
    std::fs::set_permissions(&agent_dir, std::fs::Permissions::from_mode(0o755)).unwrap();
    setup_agent();

    if writable {
        eprintln!("skipped: the data dir stays writable for this user");
        return;
    }
    assert_no_private_keys(
        &status,
        &fixture.secrets,
        "agent.status, file not rewritable",
    );
    assert_public_document_present(&status, &fixture.did, "agent.status, file not rewritable");
    assert_eq!(
        before, after,
        "a failed rewrite must leave agent.json as it was"
    );
    assert_eq!(entries, 1, "a failed rewrite must not leave a temp file");
}

/// Multi-user mode: `agent.status` for a managed user and the `AgentData` it
/// is built from.
#[tokio::test]
async fn agent_status_for_a_managed_user_carries_no_private_key() {
    setup_wallet();
    setup_agent();
    let email = "did-doc-1229@example.com";
    AgentService::ensure_user_key_exists(email).unwrap();
    let secrets = wallet_private_key_values(email);
    let user_did = AgentService::get_user_did_by_email(email).unwrap();

    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![ALL_CAPABILITY.clone()]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: false,
        user_email: Some(email.to_string()),
        user_did: Some(user_did.clone()),
        cancel_token: None,
    });
    let reply = handlers()
        .dispatch("agent.status", json!({}), ctx)
        .await
        .expect("agent.status for a user")
        .to_string();
    assert_no_private_keys(&reply, &secrets, "agent.status for a user");
    assert_public_document_present(&reply, &user_did, "agent.status for a user");

    let agent_data = AgentService::get_user_agent_data(email).unwrap();
    assert_no_private_keys(&agent_data.did_document, &secrets, "AgentData.did_document");
    assert_public_document_present(
        &agent_data.did_document,
        &user_did,
        "AgentData.did_document",
    );
}
