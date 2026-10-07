//! A multi-user user session acts for its user only. The calls below act on the node
//! itself (its main key, wallet, host files, database or node-wide settings), and the
//! default user capabilities cover the capabilities they check, so before these guards a
//! user token reached all of them.

use std::path::PathBuf;
use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::{get_user_default_capabilities, ALL_CAPABILITY};
use crate::agent::{signing_key_id, signing_key_id_for_context, AgentContext, AgentService};
use crate::api::ws_handler::build_handler_map;
use crate::db::Ad4mDb;
use crate::languages::LanguageController;
use crate::pubsub::{get_global_pubsub, AGENT_UPDATED_TOPIC};
use crate::types::{Agent, ModelApiInput, ModelInput, ModelType, RequestContext};

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

/// A managed user for one test: a fresh email with its own wallet key. Dropping it deletes
/// the user's profile directory, so test profiles do not pile up under `test_data`.
/// Needs `setup_wallet` and `setup_agent` first.
struct TestUser {
    email: String,
    profile_path: PathBuf,
}

impl TestUser {
    fn new(label: &str) -> Self {
        let email = format!("{label}.{}@example.org", uuid::Uuid::new_v4());
        AgentService::ensure_user_key_exists(&email).unwrap();
        let profile_path =
            AgentService::with_global_instance(|a| a.user_profile_path(&email)).unwrap();
        Self {
            email,
            profile_path,
        }
    }

    fn did(&self) -> String {
        AgentService::get_user_did_by_email(&self.email).unwrap()
    }

    fn stored_profile(&self) -> Agent {
        AgentService::with_global_instance(|a| a.load_user_agent_profile(&self.email))
            .unwrap()
            .expect("the user's profile exists")
    }
}

impl Drop for TestUser {
    fn drop(&mut self) {
        if let Some(user_dir) = self.profile_path.parent() {
            let _ = std::fs::remove_dir_all(user_dir);
        }
    }
}

#[tokio::test]
async fn user_sessions_cannot_call_node_operations() {
    let dir = tempfile::tempdir().unwrap();
    let export_path = dir.path().join("dump.json");
    // Every call carries params its contract accepts, so the dispatcher's params check
    // passes and the guard is what refuses.
    let model = json!({ "name": "m", "type": "LLM" });
    let proof = json!({
        "deviceKey": "k",
        "deviceKeySignedByDid": "s",
        "deviceKeyType": "t",
        "did": "did:key:z6Mk",
        "didSignedByDeviceKey": "s",
        "didSigningKeyId": "i",
    });
    let calls = [
        ("agent.generate", json!({ "passphrase": "p" })),
        ("agent.lock", json!({ "passphrase": "p" })),
        (
            "agent.unlock",
            json!({ "passphrase": "p", "holochain": false }),
        ),
        (
            "runtime.exportData",
            json!({ "type": "db", "filePath": export_path.to_str().unwrap() }),
        ),
        (
            "runtime.importData",
            json!({ "type": "db", "filePath": export_path.to_str().unwrap() }),
        ),
        (
            "ai.discoverModels",
            json!({ "baseUrl": "https://api.example.org/v1" }),
        ),
        ("ai.addModel", json!({ "model": model })),
        ("ai.updateModel", json!({ "id": "m1", "model": model })),
        ("ai.removeModel", json!({ "id": "m1" })),
        (
            "ai.setDefaultModel",
            json!({ "id": "m1", "modelType": "LLM" }),
        ),
        ("agent.addEntanglementProofs", json!({ "proofs": [proof] })),
        (
            "agent.deleteEntanglementProofs",
            json!({ "proofs": [proof] }),
        ),
        (
            "agent.entanglementProofPreflight",
            json!({ "deviceKey": "k", "deviceKeyType": "t" }),
        ),
        ("agent.getEntanglementProofs", json!({})),
        (
            "agent.addTrustedAgents",
            json!({ "agents": ["did:key:z6Mk"] }),
        ),
        (
            "agent.deleteTrustedAgents",
            json!({ "agents": ["did:key:z6Mk"] }),
        ),
        (
            "language.publish",
            json!({
                "languagePath": dir.path().join("bundle.js").to_str().unwrap(),
                "languageMeta": { "name": "n", "description": "d" },
            }),
        ),
        ("language.remove", json!({ "address": "QmLang" })),
        (
            "language.writeSettings",
            json!({ "address": "QmLang", "settings": {} }),
        ),
        ("runtime.addHcAgentInfos", json!({ "agentInfos": [] })),
        (
            "runtime.addLinkLanguageTemplates",
            json!({ "addresses": [] }),
        ),
        (
            "runtime.removeLinkLanguageTemplates",
            json!({ "addresses": [] }),
        ),
        ("runtime.friends", json!({})),
        ("runtime.addFriends", json!({ "dids": [] })),
        ("runtime.removeFriends", json!({ "dids": [] })),
        (
            "runtime.sendFriendMessage",
            json!({ "did": "did:key:z6Mk", "message": { "links": [] } }),
        ),
        ("runtime.outbox", json!({})),
        ("hosting.wallet", json!({})),
        ("hosting.walletHistory", json!({})),
    ];
    let mut not_refused = Vec::new();
    for (op, params) in calls {
        // Spawned, so a handler that panics is reported with the rest instead of ending
        // the loop.
        let result = tokio::spawn(call(op, params, user_session("guard.user@example.org"))).await;
        match result {
            Ok(Err((403, message))) if message.contains(op) => {}
            other => not_refused.push(format!("{op}: {other:?}")),
        }
    }
    assert!(
        not_refused.is_empty(),
        "these calls must refuse a user session:\n{}",
        not_refused.join("\n")
    );
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
    let user = TestUser::new("dm.user");
    let node_dm_before = AgentService::with_global_instance(|a| {
        a.agent
            .as_ref()
            .and_then(|agent| agent.direct_message_language.clone())
    });

    let returned = call(
        "agent.updateProfile",
        json!({ "dmLanguage": "lang://the-users-own" }),
        user_session(&user.email),
    )
    .await
    .expect("a user may set their own DM language");
    assert_eq!(
        returned["did"],
        json!(user.did()),
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
    assert_eq!(
        user.stored_profile().direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );

    // A later perspective-only update keeps the DM language.
    call(
        "agent.updateProfile",
        json!({ "publicPerspective": { "links": [] } }),
        user_session(&user.email),
    )
    .await
    .expect("a user may update their own perspective");
    assert_eq!(
        user.stored_profile().direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );
}

/// Every `agent-updated` event received so far that names `did`.
fn announced_for(events: &mut tokio::sync::broadcast::Receiver<String>, did: &str) -> Vec<Value> {
    let mut announced = Vec::new();
    while let Ok(message) = events.try_recv() {
        let event: Value = serde_json::from_str(&message).unwrap();
        if event["did"] == json!(did) {
            announced.push(event);
        }
    }
    announced
}

#[tokio::test]
async fn one_profile_update_with_both_fields_stores_and_announces_once() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("both.fields");
    let mut events = get_global_pubsub()
        .await
        .subscribe(&AGENT_UPDATED_TOPIC)
        .await;

    let returned = call(
        "agent.updateProfile",
        json!({
            "dmLanguage": "lang://the-users-dm",
            "publicPerspective": { "links": [{
                "author": "did:key:z6MkProfileLinkAuthor",
                "timestamp": "2026-09-26T00:00:00.000Z",
                "data": {
                    "source": "did:key:z6MkProfileLinkAuthor",
                    "predicate": "sioc://has_name",
                    "target": "literal://string:hello",
                },
                "proof": { "key": "#key", "signature": "00" },
            }] },
        }),
        user_session(&user.email),
    )
    .await
    .expect("a user may update both profile fields at once");

    let did = user.did();
    assert_eq!(returned["did"], json!(did));
    assert_eq!(
        returned["directMessageLanguage"],
        json!("lang://the-users-dm")
    );
    assert_eq!(
        returned["perspective"]["links"][0]["data"]["target"],
        json!("literal://string:hello")
    );

    let stored = user.stored_profile();
    assert_eq!(stored.did, did);
    assert_eq!(
        stored.direct_message_language.as_deref(),
        Some("lang://the-users-dm")
    );
    assert_eq!(stored.perspective.map(|p| p.links.len()), Some(1));

    let announced = announced_for(&mut events, &did);
    assert_eq!(
        announced.len(),
        1,
        "one update must store and announce the profile once: {announced:?}"
    );
    assert_eq!(
        announced[0]["directMessageLanguage"],
        json!("lang://the-users-dm")
    );
    assert_eq!(
        announced[0]["perspective"]["links"][0]["data"]["target"],
        json!("literal://string:hello")
    );
}

#[tokio::test]
async fn a_profile_that_fails_to_load_fails_the_update() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("unreadable.profile");
    std::fs::create_dir_all(user.profile_path.parent().unwrap()).unwrap();
    std::fs::write(&user.profile_path, "{ not a profile").unwrap();

    for params in [
        json!({ "publicPerspective": { "links": [] } }),
        json!({ "dmLanguage": "lang://the-users-dm" }),
    ] {
        match call(
            "agent.updateProfile",
            params.clone(),
            user_session(&user.email),
        )
        .await
        {
            Err((500, message)) => assert!(
                message.contains("Failed to load user profile"),
                "{params}: {message}"
            ),
            other => panic!("{params}: an unreadable profile must fail the update, got {other:?}"),
        }
        assert_eq!(
            std::fs::read_to_string(&user.profile_path).unwrap(),
            "{ not a profile",
            "{params}: a failed update must leave the stored profile alone"
        );
    }
}

#[tokio::test]
async fn a_user_reads_only_their_own_compute_log() {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let (alice, bob) = ("alice.log@example.org", "bob.log@example.org");
    Ad4mDb::with_global_instance(|db| {
        db.insert_compute_log(alice, "prompt", Some("alice's prompt"), 1.0, 9.0)
            .unwrap();
        db.insert_compute_log(bob, "prompt", Some("bob's prompt"), 2.0, 8.0)
            .unwrap();
    });
    let owners = |log: Value| -> Vec<String> {
        log.as_array()
            .unwrap()
            .iter()
            .map(|entry| entry["userEmail"].as_str().unwrap().to_string())
            .collect()
    };

    match call(
        "runtime.computeLog",
        json!({ "userEmail": bob }),
        user_session(alice),
    )
    .await
    {
        Err((403, _)) => {}
        other => panic!("a user read another user's compute log: {other:?}"),
    }

    let own = call("runtime.computeLog", json!({}), user_session(alice))
        .await
        .expect("a user may read their own compute log");
    assert_eq!(
        owners(own),
        vec![alice],
        "no userEmail means the caller's own log"
    );
    let own_by_name = call(
        "runtime.computeLog",
        json!({ "userEmail": alice }),
        user_session(alice),
    )
    .await
    .expect("a user may name themselves");
    assert_eq!(owners(own_by_name), vec![alice]);

    let bobs = call(
        "runtime.computeLog",
        json!({ "userEmail": bob }),
        operator_session(),
    )
    .await
    .expect("the operator may read any user's compute log");
    assert_eq!(owners(bobs), vec![bob]);
}

#[tokio::test]
async fn the_default_model_hides_its_key_from_users_only() {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let id = Ad4mDb::with_global_instance(|db| {
        let id = db.add_model(&ModelInput {
            name: "Remote".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.org/v1".to_string(),
                api_key: "sk-provider-secret".to_string(),
                model: "gpt".to_string(),
                api_type: "OpenAi".to_string(),
                max_num_ctx: None,
            }),
            local: None,
            model_type: ModelType::Llm,
        })?;
        db.set_default_model(ModelType::Llm, &id)?;
        crate::db::Ad4mDbResult::Ok(id)
    })
    .unwrap();
    let params = json!({ "modelType": "LLM" });

    let as_user = call(
        "ai.getDefaultModel",
        params.clone(),
        user_session("model.user@example.org"),
    )
    .await
    .expect("a user may read the default model");
    assert_eq!(as_user["id"], json!(id));
    assert_eq!(as_user["api"]["model"], json!("gpt"));
    assert_eq!(
        as_user["api"]["apiKey"],
        json!(""),
        "a user session must not see the operator's provider key"
    );

    let as_operator = call("ai.getDefaultModel", params, operator_session())
        .await
        .expect("the operator reads the default model");
    assert_eq!(as_operator["api"]["apiKey"], json!("sk-provider-secret"));
}

/// The template the test languages are cloned from.
const TEMPLATE_SOURCE: &str = r#"//!@ad4m-template-variable
const uid = "template";
export const name = "node-operation-template";
export async function init() {}
"#;

/// A language language that keeps published languages in memory. Like the real one, it
/// signs each published meta with `agentCreateSignedExpression`, so the author of a
/// published language names the agent the executor published it as.
fn in_memory_language_language(template_address: &str) -> String {
    let address = serde_json::to_string(template_address).unwrap();
    let source = serde_json::to_string(TEMPLATE_SOURCE).unwrap();
    format!(
        r#"import {{ agentCreateSignedExpression }} from "ad4m:host";

const metas = new Map([[{address}, {{ data: {{ address: {address}, name: "node-operation-template" }} }}]]);
const sources = new Map([[{address}, {source}]]);

export const name = "node-operation-languages";
export async function init() {{}}
export async function expressionCreate(language) {{
    metas.set(language.meta.address, agentCreateSignedExpression(language.meta));
    sources.set(language.meta.address, language.bundle);
    return language.meta.address;
}}
export async function expressionGet(address) {{
    return metas.get(address) ?? null;
}}
export async function languageGetSource(address) {{
    return sources.get(address) ?? null;
}}
"#
    )
}

/// The in-memory language language, installed as the node's language language for one
/// test. The languages directory is process-wide and set once, so every test that installs
/// this shares one temp dir, set before anything saves a bundle; no bundle lands under the
/// home directory. Bundles save under their hash, so the tests do not collide.
struct TestLanguageLanguage {
    controller: LanguageController,
    address: String,
    template_address: String,
    previous: Option<String>,
}

impl TestLanguageLanguage {
    async fn install() -> Self {
        crate::test_utils::init_v8_platform();
        static DIR: std::sync::OnceLock<tempfile::TempDir> = std::sync::OnceLock::new();
        let dir = DIR.get_or_init(|| {
            let dir = tempfile::tempdir().unwrap();
            crate::utils::set_languages_directory(dir.path().to_str().unwrap());
            dir
        });
        assert!(
            crate::utils::languages_directory().starts_with(dir.path()),
            "the languages directory was set before this test, to {:?}",
            crate::utils::languages_directory()
        );

        let controller = LanguageController::global_instance();
        let (template_address, _) = controller
            .save_language_bundle(TEMPLATE_SOURCE, None)
            .unwrap();
        let (address, bundle_path) = controller
            .save_language_bundle(&in_memory_language_language(&template_address), None)
            .unwrap();
        controller
            .load_language(bundle_path, false)
            .await
            .expect("the in-memory language language loads");
        let previous = controller
            .system_addresses
            .lock()
            .await
            .language_language
            .replace(address.clone());
        Self {
            controller,
            address,
            template_address,
            previous,
        }
    }

    /// Unloads the languages the test installed, then this language language.
    async fn uninstall(self, installed: &[&str]) {
        for address in installed.iter().copied().chain([self.address.as_str()]) {
            self.controller
                .unload_language(address)
                .await
                .expect("the test language unloads");
        }
    }
}

impl Drop for TestLanguageLanguage {
    fn drop(&mut self) {
        if let Ok(mut system) = self.controller.system_addresses.try_lock() {
            system.language_language = self.previous.take();
        }
    }
}

#[tokio::test]
async fn apply_template_publishes_as_the_calling_session() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("template.user");
    let languages = TestLanguageLanguage::install().await;
    let template = |uid: &str| {
        json!({
            "sourceLanguageHash": languages.template_address,
            "templateData": json!({ "uid": uid }).to_string(),
        })
    };

    let users_copy = call(
        "language.applyTemplate",
        template("the user's copy"),
        user_session(&user.email),
    )
    .await
    .expect("a user may template a link language to create a neighbourhood");
    let operators_copy = call(
        "language.applyTemplate",
        template("the operator's copy"),
        operator_session(),
    )
    .await
    .expect("the operator may template a language");
    let users_meta = call(
        "language.meta",
        json!({ "address": users_copy["address"] }),
        user_session(&user.email),
    )
    .await
    .expect("the user's copy was published");
    let operators_meta = call(
        "language.meta",
        json!({ "address": operators_copy["address"] }),
        operator_session(),
    )
    .await
    .expect("the operator's copy was published");

    assert_eq!(
        users_meta["author"],
        json!(user.did()),
        "a user session must publish the templated language as the user"
    );
    assert_eq!(
        users_meta["templateSourceLanguageAddress"],
        json!(languages.template_address)
    );
    assert_eq!(
        operators_meta["author"],
        json!(crate::agent::did()),
        "the operator publishes as the node's main agent"
    );

    languages
        .uninstall(&[
            users_copy["address"].as_str().unwrap(),
            operators_copy["address"].as_str().unwrap(),
        ])
        .await;
}

/// The MCP neighbourhood tool clones its link language itself; a user's clone must carry
/// the user's DID as author, as `language.applyTemplate` does.
#[tokio::test]
async fn mcp_clones_a_link_language_as_the_calling_agent() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("mcp.template.user");
    let languages = TestLanguageLanguage::install().await;

    let users_copy = crate::mcp::tools::neighbourhoods::clone_link_language(
        &languages.template_address,
        "the user's space",
        &AgentContext::for_user_email(user.email.clone()),
    )
    .await
    .expect("a user clones a link language");
    let operators_copy = crate::mcp::tools::neighbourhoods::clone_link_language(
        &languages.template_address,
        "the operator's space",
        &AgentContext::main_agent(),
    )
    .await
    .expect("the operator clones a link language");

    let author = |address: &str| {
        let address = address.to_string();
        async move {
            call(
                "language.meta",
                json!({ "address": address }),
                operator_session(),
            )
            .await
            .expect("the copy was published")["author"]
                .clone()
        }
    };
    assert_eq!(
        author(&users_copy).await,
        json!(user.did()),
        "a user's MCP neighbourhood must publish its link language as the user"
    );
    assert_eq!(author(&operators_copy).await, json!(crate::agent::did()));

    languages.uninstall(&[&users_copy, &operators_copy]).await;
}
