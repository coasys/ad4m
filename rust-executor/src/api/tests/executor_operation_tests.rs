//! A user session acts for its user's agent only: it signs, publishes and sets its profile as
//! that agent, not the main agent. That user sessions cannot call the calls that act on the
//! executor itself (`Guard::Executor`) is a row of the method matrix
//! (`method_matrix_tests.rs`).

use std::path::PathBuf;
use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::get_user_default_capabilities;
use crate::agent::{signing_key_id, signing_key_id_for_context, AgentContext, AgentService};
use crate::api::ws_handler::build_handler_map;
use crate::db::Ad4mDb;
use crate::events::{Audience, Event, Topic};
use crate::languages::LanguageController;
use crate::pubsub::get_global_pubsub;
use crate::types::{Agent, ModelApiInput, ModelInput, ModelType, RequestContext};

/// A user session on the managed agent `did`, with the default user capabilities.
fn user_session(did: &str) -> Arc<RequestContext> {
    Arc::new(RequestContext::user_for_test(
        did,
        get_user_default_capabilities(),
    ))
}

fn operator_session() -> Arc<RequestContext> {
    Arc::new(RequestContext::operator_for_test())
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
    did: String,
    profile_path: PathBuf,
}

impl TestUser {
    fn new(label: &str) -> Self {
        let email = format!("{label}.{}@example.org", uuid::Uuid::new_v4());
        let did = crate::test_utils::create_test_user(&email);
        let profile_path =
            AgentService::with_global_instance(|a| a.user_profile_path(&email)).unwrap();
        Self { did, profile_path }
    }

    fn did(&self) -> String {
        self.did.clone()
    }

    fn stored_profile(&self) -> Agent {
        AgentService::load_managed_agent_profile(&self.did)
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
async fn a_user_session_signs_as_the_user_not_the_main_agent() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let email = "signing.user@example.org";
    let user_did = crate::test_utils::create_test_user(email);

    let result = call(
        "agent.sign",
        json!({ "message": "hello" }),
        user_session(&user_did),
    )
    .await
    .expect("a user may sign as themselves");

    let user_key = signing_key_id_for_context(&AgentContext::managed(user_did.clone())).unwrap();
    assert_eq!(result["publicKey"], json!(user_key));
    assert_ne!(result["publicKey"], json!(signing_key_id()));

    let signature = result["signature"].as_str().unwrap();
    assert!(
        crate::agent::signatures::verify_string_signed_by_did(&user_did, "hello", signature)
            .unwrap(),
        "the signature must verify against the user's own DID"
    );
}

#[tokio::test]
async fn the_operator_still_signs_as_the_main_agent() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let result = call(
        "agent.sign",
        json!({ "message": "hello" }),
        operator_session(),
    )
    .await
    .expect("the operator signs as the main agent");
    assert_eq!(result["publicKey"], json!(signing_key_id()));
}

#[tokio::test]
async fn a_user_sets_their_own_dm_language_not_the_main_agents() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("dm.user");
    let main_agent_dm_before = AgentService::with_global_instance(|a| {
        a.agent
            .as_ref()
            .and_then(|agent| agent.direct_message_language.clone())
    });

    let returned = call(
        "agent.updateProfile",
        json!({ "dmLanguage": "lang://the-users-own" }),
        user_session(&user.did),
    )
    .await
    .expect("a user may set their own DM language");
    assert_eq!(
        returned["did"],
        json!(user.did()),
        "the call must return the user's own profile, not the main agent's"
    );

    let main_agent_dm_after = AgentService::with_global_instance(|a| {
        a.agent
            .as_ref()
            .and_then(|agent| agent.direct_message_language.clone())
    });
    assert_eq!(
        main_agent_dm_before, main_agent_dm_after,
        "a user session changed the main agent's DM language"
    );
    assert_eq!(
        user.stored_profile().direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );

    // A later perspective-only update keeps the DM language.
    call(
        "agent.updateProfile",
        json!({ "publicPerspective": { "links": [] } }),
        user_session(&user.did),
    )
    .await
    .expect("a user may update their own perspective");
    assert_eq!(
        user.stored_profile().direct_message_language.as_deref(),
        Some("lang://the-users-own")
    );
}

/// Every `agent-updated` event received so far that names `did`.
/// Each goes to that agent only.
fn announced_for(
    events: &mut tokio::sync::broadcast::Receiver<Arc<Event>>,
    did: &str,
) -> Vec<Value> {
    let mut announced = Vec::new();
    while let Ok(event) = events.try_recv() {
        if event.topic == Topic::AgentUpdated && event.payload["did"] == json!(did) {
            assert_eq!(event.audience, Audience::Dids(vec![did.to_string()]));
            announced.push(event.payload.clone());
        }
    }
    announced
}

#[tokio::test]
async fn one_profile_update_with_both_fields_stores_and_announces_once() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = TestUser::new("both.fields");
    let mut events = get_global_pubsub().await.subscribe_events();

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
        user_session(&user.did),
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
            user_session(&user.did),
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
    let (alice_did, bob_did) = ("did:key:z6MkAliceLog", "did:key:z6MkBobLog");
    Ad4mDb::with_global_instance(|db| {
        db.add_user(alice, alice_did, "pw").unwrap();
        db.add_user(bob, bob_did, "pw").unwrap();
        db.insert_compute_log(alice_did, "prompt", Some("alice's prompt"), 1.0, 9.0)
            .unwrap();
        db.insert_compute_log(bob_did, "prompt", Some("bob's prompt"), 2.0, 8.0)
            .unwrap();
    });
    // The log names each entry's account by email, as before agents were keyed by DID.
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
        user_session(alice_did),
    )
    .await
    {
        Err((403, _)) => {}
        other => panic!("a user read another user's compute log: {other:?}"),
    }
    match call(
        "runtime.computeLog",
        json!({ "userEmail": "nobody@example.org" }),
        user_session(alice_did),
    )
    .await
    {
        Err((403, _)) => {}
        other => panic!("a user named an email that is not theirs: {other:?}"),
    }

    let own = call("runtime.computeLog", json!({}), user_session(alice_did))
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
        user_session(alice_did),
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

    let nobodys = call(
        "runtime.computeLog",
        json!({ "userEmail": "nobody@example.org" }),
        operator_session(),
    )
    .await
    .expect("the operator reads an empty log for an unknown email");
    assert!(owners(nobodys).is_empty());

    // An app on the main agent is not the operator: AGENT_READ, the most common app
    // capability, reads no user's log.
    let app = Arc::new(RequestContext::app_for_test(vec![
        crate::agent::capabilities::AGENT_READ_CAPABILITY.clone(),
    ]));
    match call("runtime.computeLog", json!({ "userEmail": bob }), app).await {
        Err((403, _)) => {}
        other => panic!("a main-agent app read a user's compute log: {other:?}"),
    }
}

#[tokio::test]
async fn the_default_model_never_shows_its_key() {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let id = Ad4mDb::with_global_instance(|db| {
        let id = db.add_model(&ModelInput {
            name: "Remote".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.org/v1".to_string(),
                api_key: Some("sk-provider-secret".into()),
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
        user_session("did:key:test:model-user"),
    )
    .await
    .expect("a user may read the default model");
    let as_operator = call("ai.getDefaultModel", params, operator_session())
        .await
        .expect("the operator reads the default model");

    for model in [as_user, as_operator] {
        assert_eq!(model["id"], json!(id));
        assert_eq!(model["api"]["model"], json!("gpt"));
        assert_eq!(model["api"]["hasApiKey"], json!(true));
        assert!(
            !model.to_string().contains("sk-provider-secret"),
            "no session reads the provider key back: {model}"
        );
    }
}

/// The template the test languages are cloned from.
const TEMPLATE_SOURCE: &str = r#"//!@ad4m-template-variable
const uid = "template";
export const name = "executor-operation-template";
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

const metas = new Map([[{address}, {{ data: {{ address: {address}, name: "executor-operation-template" }} }}]]);
const sources = new Map([[{address}, {source}]]);

export const name = "executor-operation-languages";
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

/// The in-memory language language, installed as the executor's language language for one
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
        user_session(&user.did),
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
        user_session(&user.did),
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
        "the operator publishes as the executor's main agent"
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
        &AgentContext::managed(user.did.clone()),
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
