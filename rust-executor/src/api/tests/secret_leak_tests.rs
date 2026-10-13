//! The secret-leak test (Plan 20 §8.3): no seeded secret reaches a response or an event.
//!
//! The test seeds the node with one known value of each kind of secret: the admin
//! credential, an app token, a user's token and password, a webhook credential and an AI
//! provider API key. It then calls every read method of the handler map as each principal
//! (operator, app, user, anonymous), and captures every event on the operator's and the
//! user's event streams from before the seeding to the end. No response, error message or
//! event may contain a seeded value. #1164 (app tokens in `agent.getApps` and
//! `apps-changed`) and #1167 (provider keys in `ai.models`) are the leaks it would have
//! caught.
//!
//! A read method is one flagged `read` in the handler map, or one of
//! [`READ_ONLY_UNFLAGGED`]. Excluded by design: `runtime.exportData`, an operator backup that
//! holds the secrets ([`EXCLUDED`]). [`SKIPPED`] lists the read methods that need a service
//! unit tests do not start, each with the reason. Log lines are not covered.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use futures::StreamExt;
use serde_json::{json, Value};

use crate::agent::capabilities::{
    generate_capability_token, permit_capability, request_capability, AuthInfo, AuthInfoExtended,
    ListenerReach, ALL_CAPABILITY,
};
use crate::agent::{AgentContext, AgentService};
use crate::api::event_interest::watching_all;
use crate::api::events_ws::build_event_stream;
use crate::api::ws_handler::{build_handler_map, HandlerMap};
use crate::config::{get_global_config, set_global_config, Ad4mConfig};
use crate::db::Ad4mDb;
use crate::events::{Audience, Event, Topic};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{register_perspective, unregister_perspective};
use crate::pubsub::get_global_pubsub;
use crate::runtime_service::RuntimeService;
use crate::secret::Secret;
use crate::test_utils::{ensure_test_db, setup_agent, setup_wallet, MultiUserMode};
use crate::types::{
    ModelApiInput, ModelInput, ModelType, PerspectiveHandle, PerspectiveState, RequestContext,
};

/// Methods that only read but carry no `read` flag (the flag also promises that a client may
/// resend the call after a reconnect).
const READ_ONLY_UNFLAGGED: &[&str] = &[
    "expression.getMany",
    "expression.interactions",
    "perspective.evaluateGetters",
    "perspective.flowValidOutputs",
    "perspective.get",
    "perspective.getSubjectData",
    "perspective.interpretationOverlays",
    "perspective.modelQuery",
    "perspective.queryLinks",
    "perspective.snapshot",
    "perspective.subjectClassesOf",
    "runtime.friendStatus",
    "runtime.linkLanguageTemplates",
];

/// Read methods left out by design.
const EXCLUDED: &[(&str, &str)] = &[(
    "runtime.exportData",
    "an operator backup: it writes the node's data, secrets included, to a file the operator \
     names, by design",
)];

/// Read methods this test does not call, and why.
const SKIPPED: &[(&str, &str)] = &[
    (
        "ai.discoverModels",
        "calls the provider at baseUrl over the network",
    ),
    (
        "perspective.queryProlog",
        "starts a Scryer engine, which aborts in debug builds",
    ),
    (
        "runtime.hcAgentInfos",
        "waits for the Holochain conductor, which unit tests do not start",
    ),
    (
        "runtime.networkMetrics",
        "waits for the Holochain conductor, which unit tests do not start",
    ),
];

/// The secrets, each under a name for the failure message.
struct Secrets(Vec<(&'static str, String)>);

impl Secrets {
    fn assert_absent(&self, text: &str, what: &str) {
        for (name, value) in &self.0 {
            assert!(
                !text.contains(value.as_str()),
                "{what} carries the {name}: {text}"
            );
        }
    }
}

/// Restores the global config when the test ends, also on panic.
struct ConfigGuard(Ad4mConfig);

impl Drop for ConfigGuard {
    fn drop(&mut self) {
        set_global_config(self.0.clone());
    }
}

/// Starts the runtime service, which the friend, outbox and template reads need, on an
/// empty bootstrap seed; stops it when the test ends. Without it those handlers panic
/// holding its lock, which poisons it for the rest of the test process.
struct RuntimeServiceGuard;

impl RuntimeServiceGuard {
    fn start() -> Self {
        let dir = tempfile::tempdir().unwrap();
        let seed = dir.path().join("seed.json");
        let empty = json!({
            "trustedAgents": [], "knownLinkLanguages": [], "directMessageLanguage": "",
            "agentLanguage": "", "perspectiveLanguage": "", "neighbourhoodLanguage": "",
            "languageLanguageBundle": "",
        });
        std::fs::write(&seed, empty.to_string()).unwrap();
        RuntimeService::init_global_instance(seed.to_string_lossy().into_owned());
        assert!(RuntimeService::global_instance().lock().unwrap().is_some());
        RuntimeServiceGuard
    }
}

impl Drop for RuntimeServiceGuard {
    fn drop(&mut self) {
        let instance = RuntimeService::global_instance();
        *instance.lock().unwrap_or_else(|p| p.into_inner()) = None;
    }
}

/// Takes the test's perspectives out of the global registry when the test ends.
struct Registered(Vec<String>);

impl Drop for Registered {
    fn drop(&mut self) {
        for uuid in &self.0 {
            unregister_perspective(uuid);
        }
    }
}

fn perspective(owners: Option<Vec<String>>) -> String {
    let uuid = uuid::Uuid::new_v4().to_string();
    let instance = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some("secret leak test".to_string()),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
            owners,
        },
        None,
    );
    register_perspective(uuid.clone(), instance);
    uuid
}

/// What the parameters of a call refer to.
struct Fixture {
    main_did: String,
    user_email: String,
    perspective: String,
    model_id: String,
}

/// Minimal valid parameters for `method`.
fn params(method: &str, fx: &Fixture) -> Value {
    let uuid = fx.perspective.as_str();
    match method {
        "agent.byDid" | "runtime.friendStatus" => json!({ "did": fx.main_did }),
        "agent.entanglementProofPreflight" => {
            json!({ "deviceKey": "leak-test-device", "deviceKeyType": "ethereum" })
        }
        "ai.getDefaultModel" => json!({ "modelType": "LLM" }),
        "ai.modelLoadingStatus" => json!({ "modelId": fx.model_id }),
        "expression.get" | "expression.getRaw" | "expression.interactions" => {
            json!({ "url": "literal://string:leak" })
        }
        "expression.getMany" => json!({ "urls": ["literal://string:leak"] }),
        "language.get" | "language.meta" | "language.source" => {
            json!({ "address": "literal" })
        }
        "neighbourhood.hasTelepresence"
        | "neighbourhood.onlineAgents"
        | "neighbourhood.otherAgents"
        | "perspective.get"
        | "perspective.getAllShacl"
        | "perspective.getShaclNames"
        | "perspective.interpretationOverlays"
        | "perspective.queryLinks"
        | "perspective.snapshot"
        | "perspective.flowStates"
        | "perspective.subjectClassesOf" => json!({ "uuid": uuid }),
        "perspective.flowFired" => json!({ "uuid": uuid, "instance": "test://leak" }),
        "perspective.getShacl" | "perspective.getShaclTargetClass" => {
            json!({ "uuid": uuid, "name": "Leak" })
        }
        "perspective.getSubjectData" => {
            json!({ "uuid": uuid, "subjectClass": "Leak", "expressionAddress": "test://leak" })
        }
        "perspective.modelQuery" => {
            json!({ "uuid": uuid, "class_name": "Leak", "query_json": "{}" })
        }
        "perspective.evaluateGetters" => json!({ "uuid": uuid, "class_name": "Leak" }),
        "perspective.verifyFlowReceipt" => json!({ "uuid": uuid, "receipt": {} }),
        "perspective.flowValidOutputs" => json!({ "uuid": uuid, "flow": "Leak" }),
        "perspective.querySparql" => {
            json!({ "uuid": uuid, "query": "SELECT ?s WHERE { ?s ?p ?o }" })
        }
        "runtime.verifySignature" => {
            json!({ "did": fx.main_did, "data": "leak", "signedData": "00" })
        }
        "user.wallet" => json!({ "email": fx.user_email }),
        _ => json!({}),
    }
}

/// Collects every message of `stream` until the test ends.
fn collect(
    mut stream: std::pin::Pin<Box<dyn futures::Stream<Item = String> + Send>>,
) -> Arc<Mutex<Vec<String>>> {
    let seen = Arc::new(Mutex::new(Vec::new()));
    let sink = seen.clone();
    tokio::spawn(async move {
        while let Some(message) = stream.next().await {
            sink.lock().unwrap().push(message);
        }
    });
    seen
}

fn context(token: &str, admin: &Option<Secret>) -> Arc<RequestContext> {
    let ctx = RequestContext::open(token, admin, ListenerReach::Network)
        .unwrap_or_else(|e| panic!("the token opens a session: {e}"));
    Arc::new(ctx)
}

/// A call that gave the client no answer.
const NO_ANSWER: u16 = 0;

/// What a call answered: its result or its error, as the client would read it. A handler
/// that panics or takes over 10 s answers [`NO_ANSWER`].
async fn answer(
    map: &Arc<HandlerMap>,
    method: &str,
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, (u16, String)> {
    let map = map.clone();
    let name = method.to_string();
    let call = tokio::spawn(async move {
        map.dispatch(&name, params, ctx)
            .await
            .map_err(|e| (e.code, e.message))
    });
    match tokio::time::timeout(Duration::from_secs(10), call).await {
        Ok(Ok(answer)) => answer,
        Ok(Err(e)) => Err((NO_ANSWER, format!("panicked: {e}"))),
        Err(_) => Err((NO_ANSWER, "no answer within 10 s".to_string())),
    }
}

// Multi-threaded, so a handler that blocks its thread still lets the timeout fire.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn no_read_method_or_event_carries_a_seeded_secret() {
    let _multi_user = MultiUserMode::on();
    setup_wallet();
    setup_agent();
    ensure_test_db();
    let _runtime = RuntimeServiceGuard::start();
    let id = || uuid::Uuid::new_v4().simple().to_string();

    let admin_credential = format!("leak-admin-credential-{}", id());
    let _config = ConfigGuard(get_global_config());
    set_global_config(Ad4mConfig {
        admin_credential: Some(Secret::new(admin_credential.clone())),
        ..get_global_config()
    });
    let admin = Some(Secret::new(admin_credential.clone()));
    let main_did = AgentService::with_global_instance(|a| a.did.clone()).expect("main DID");

    let password = format!("leak-password-{}", id());
    let user_email = format!("leak-user-{}@example.org", id());
    let user_did = crate::user_management::create_user(&user_email, &password).unwrap();
    let user_token =
        crate::user_management::login_user(&user_email, &password, "leak test").unwrap();

    let operator = context(&admin_credential, &admin);
    let user = context(&user_token, &admin);
    let operator_events =
        collect(build_event_stream(operator.session.clone(), watching_all(), None).await);
    let user_events = collect(build_event_stream(user.session.clone(), watching_all(), None).await);

    // Seeded while the streams listen: seeding publishes the events that leaked before.
    let provider_key = format!("sk-leak-provider-key-{}", id());
    let model_id = Ad4mDb::with_global_instance(|db| {
        let id = db.add_model(&ModelInput {
            name: "Leak test model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.org/v1".to_string(),
                api_key: Some(provider_key.clone().into()),
                model: "leak-model".to_string(),
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
    // Loads the model as start-up does: its loading-status events reach the streams.
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();

    let main_perspective = perspective(None);
    let user_perspective = perspective(Some(vec![user_did.clone()]));
    let _registered = Registered(vec![main_perspective.clone(), user_perspective.clone()]);
    let map = Arc::new(build_handler_map());
    let webhook_credential = format!("leak-webhook-credential-{}", id());
    let mut notifications = Vec::new();
    for (ctx, perspective) in [
        (operator.clone(), &main_perspective),
        (user.clone(), &user_perspective),
    ] {
        let input = json!({
            "description": "leak test",
            "appName": "Leak test",
            "appUrl": "https://leak.test",
            "appIconPath": "/icon.png",
            "trigger": "SELECT ?s WHERE { ?s <test://leak> ?o }",
            "perspectiveIds": [perspective],
            "webhookUrl": "https://webhook.leak.test",
            "webhookAuth": webhook_credential,
        });
        let created = answer(&map, "runtime.createNotification", input, ctx)
            .await
            .expect("the notification is created");
        notifications.push(created.as_str().unwrap().to_string());
    }

    let app_auth = AuthInfo {
        app_name: "Leak test app".to_string(),
        app_desc: "seeded by the secret-leak test".to_string(),
        capabilities: Some(vec![ALL_CAPABILITY.clone()]),
        ..AuthInfo::default()
    };
    let app_request = request_capability(app_auth.clone(), AgentContext::main_agent())
        .await
        .unwrap();
    let code = permit_capability(AuthInfoExtended {
        request_id: app_request.clone(),
        auth: app_auth.clone(),
    })
    .unwrap();
    let app_token = generate_capability_token(app_request.clone(), code)
        .await
        .unwrap()
        .expose_once();

    // An app on the user's agent (#1399): the user approves it through the API and the app
    // redeems; the approval request and `apps-changed` reach the user's stream.
    let user_app_request = request_capability(
        app_auth.clone(),
        crate::agent::AgentService::managed_agent_for_email(&user_email).unwrap(),
    )
    .await
    .unwrap();
    let user_app_code = answer(
        &map,
        "agent.permitCapability",
        json!({ "auth": serde_json::to_string(&AuthInfoExtended {
            request_id: user_app_request.clone(),
            auth: app_auth,
        })
        .unwrap() }),
        user.clone(),
    )
    .await
    .expect("the user approves a request for their own agent");
    let user_app_token = generate_capability_token(
        user_app_request.clone(),
        user_app_code.as_str().unwrap().to_string(),
    )
    .await
    .unwrap()
    .expose_once();
    let app = context(&app_token, &admin);
    let anonymous = context("", &admin);

    // A JWT's signature alone is enough to forge nothing, but it is unique to the token:
    // finding it means some part of the token got out.
    let signature = |jwt: &str| jwt.rsplit('.').next().unwrap().to_string();
    let secrets = Secrets(vec![
        ("admin credential", admin_credential.clone()),
        ("app token", app_token.clone()),
        ("app token's signature", signature(&app_token)),
        ("user's app token", user_app_token.clone()),
        ("user's app token's signature", signature(&user_app_token)),
        ("user token", user_token.clone()),
        ("user token's signature", signature(&user_token)),
        ("user password", password.clone()),
        ("webhook credential", webhook_credential.clone()),
        ("provider API key", provider_key.clone()),
    ]);

    let specs = map.specs();
    let names: Vec<&str> = specs.iter().map(|s| s.name.as_str()).collect();
    for name in READ_ONLY_UNFLAGGED
        .iter()
        .chain(EXCLUDED.iter().chain(SKIPPED).map(|(name, _)| name))
    {
        assert!(names.contains(name), "{name} is no method; update the list");
    }
    let is_listed = |list: &[(&str, &str)], name: &str| list.iter().any(|(n, _)| *n == name);
    let reads: Vec<&str> = specs
        .iter()
        .filter(|s| s.read || READ_ONLY_UNFLAGGED.contains(&s.name.as_str()))
        .map(|s| s.name.as_str())
        .filter(|name| !is_listed(EXCLUDED, name) && !is_listed(SKIPPED, name))
        .collect();

    let fixture = Fixture {
        main_did,
        user_email: user_email.clone(),
        perspective: main_perspective.clone(),
        model_id: model_id.clone(),
    };
    let principals = [
        ("operator", operator),
        ("app", app),
        ("user", user),
        ("anonymous", anonymous),
    ];
    let mut answers: BTreeMap<(&str, &str), String> = BTreeMap::new();
    let mut unanswered = Vec::new();
    for method in &reads {
        for (principal, ctx) in &principals {
            let reply = answer(&map, method, params(method, &fixture), ctx.clone()).await;
            match &reply {
                Err((400, message)) if message.starts_with("Invalid params") => {
                    panic!("{method}: the test's parameters are wrong: {message}")
                }
                Err((NO_ANSWER, why)) => unanswered.push(format!("{method} as {principal}: {why}")),
                _ => {}
            }
            let text = match &reply {
                Ok(value) => value.to_string(),
                Err((code, message)) => format!("error {code}: {message}"),
            };
            secrets.assert_absent(&text, &format!("{method} as the {principal}"));
            answers.insert((method, principal), text);
        }
    }
    assert!(
        unanswered.is_empty(),
        "calls without an answer (fix them, or list the method in SKIPPED with the reason):\n{}",
        unanswered.join("\n")
    );

    // Every event published so far has reached both collectors once this one has.
    let fence = format!("leak-fence-{}", id());
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AiModelLoadingStatus,
        Audience::Everyone,
        json!({ "fence": fence }),
    ));
    for events in [&operator_events, &user_events] {
        tokio::time::timeout(Duration::from_secs(10), async {
            while !events.lock().unwrap().iter().any(|e| e.contains(&fence)) {
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        })
        .await
        .expect("the fence arrives");
    }
    for (stream, events) in [("operator", &operator_events), ("user", &user_events)] {
        for event in events.lock().unwrap().iter() {
            secrets.assert_absent(event, &format!("an event on the {stream}'s stream"));
        }
    }

    // The test is not vacuous: the records that hold the secrets are in the answers that
    // would have leaked them, and the approval events went out.
    let operator_answer = |method: &str| answers[&(method, "operator")].clone();
    let apps = operator_answer("agent.getApps");
    assert!(apps.contains(&app_request), "{apps}");
    let listed = operator_answer("runtime.notifications");
    assert!(
        listed.contains(&notifications[0]) && listed.contains(r#""hasWebhookAuth":true"#),
        "{listed}"
    );
    let users_list = &answers[&("runtime.notifications", "user")];
    assert!(users_list.contains(&notifications[1]), "{users_list}");
    let models = operator_answer("ai.models");
    assert!(
        models.contains(&model_id) && models.contains(r#""hasApiKey":true"#),
        "{models}"
    );
    let users = operator_answer("user.list");
    assert!(users.contains(&user_email), "{users}");
    let operator_events = operator_events.lock().unwrap();
    assert!(operator_events
        .iter()
        .any(|e| e.contains("apps-changed") && e.contains(&app_request)));
    let user_events = user_events.lock().unwrap();
    assert!(user_events
        .iter()
        .any(|e| e.contains("exception-occurred") && e.contains(&user_app_request)));
    assert!(user_events
        .iter()
        .any(|e| e.contains("apps-changed") && e.contains(&user_app_request)));
    assert!(!operator_events
        .iter()
        .any(|e| e.contains(&user_app_request)));
    assert!(operator_events
        .iter()
        .any(|e| e.contains(&notifications[0]) && e.contains("hasWebhookAuth")));

    println!(
        "secret-leak test: {} read methods called as {} principals",
        reads.len(),
        principals.len(),
    );
    for method in &reads {
        let outcome = |principal: &str| {
            let text = &answers[&(*method, principal)];
            match text.strip_prefix("error ") {
                Some(error) => error.split(':').next().unwrap_or_default().to_string(),
                None => "ok".to_string(),
            }
        };
        let outcomes: Vec<String> = principals
            .iter()
            .map(|(principal, _)| format!("{principal} {}", outcome(principal)))
            .collect();
        println!("called: {method}: {}", outcomes.join(", "));
    }
    for (name, why) in EXCLUDED.iter().chain(SKIPPED) {
        println!("not called: {name}: {why}");
    }
}
