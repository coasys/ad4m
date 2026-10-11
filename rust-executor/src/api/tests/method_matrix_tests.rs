//! The method matrix: every method that has a guard, called by each kind of caller on
//! resources of its own and of others, gets the answer its guard names.
//!
//! The walk reads the guards from the handler map, so a method that gets a guard is covered
//! without a new test. The rows are per guard kind ([`resource_rows`], [`perspective_rows`]).
//! A method whose params need more than the perspective's uuid adds an entry to
//! [`EXTRA_PARAMS`]; without it the walk reports the method's 400. The walk calls
//! [`HandlerMap::admit`], the dispatcher's steps before the handler, so no handler runs.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use serde::Deserialize;
use serde_json::{json, Value};
use ts_rs::TS;

use crate::agent::capabilities::{
    anonymous_capabilities_when, perspective_query_capability, Capability, AGENT_AUTH_CAPABILITY,
    AGENT_READ_CAPABILITY, AGENT_UPDATE_CAPABILITY,
};
use crate::agent::AgentService;
use crate::api::guards::{uuid_param, Admission, Guard};
use crate::api::perspectives_ws::PerspectiveUuidParams;
use crate::api::ws_handler::{build_handler_map, HandlerMap, NoParams, WsRpcError};
use crate::types::{PerspectiveHandle, RequestContext};

const USER_DID: &str = "did:key:matrix-user";
const OTHER_USER_DID: &str = "did:key:matrix-other-user";

/// Who calls.
#[derive(Clone, Copy, Debug)]
enum Caller {
    Operator,
    /// An app token without the guard's capability.
    AppWithout,
    /// An app token with the guard's capability.
    AppWith,
    /// A user session with the guard's capability.
    User,
    /// A second user with the guard's capability.
    OtherUser,
    /// No token: the anonymous capabilities only.
    Anonymous,
}

/// The perspective a call names.
#[derive(Clone, Copy, Debug)]
enum Target {
    /// Owned by [`Caller::User`].
    UsersOwn,
    /// Owned by the node's main agent.
    MainAgents,
    Unowned,
    /// No such perspective.
    Missing,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Expect {
    Granted,
    Forbidden,
    /// 403 from the owner rule: `null` for a method that hides existence.
    NotOwner,
    /// 404: `null` for a method that hides existence.
    NotFound,
}

/// Guards that name no resource: who passes each.
fn resource_rows(guard: &Guard) -> &'static [(Caller, Expect)] {
    use Caller::*;
    use Expect::*;
    match guard {
        Guard::Anonymous(_) => &[
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Granted),
            (User, Granted),
            (Anonymous, Granted),
        ],
        Guard::Node(_) => &[
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Granted),
            (User, Forbidden),
            (Anonymous, Forbidden),
        ],
        Guard::SelfAgent(_) => &[
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Granted),
            (User, Granted),
            (Anonymous, Forbidden),
        ],
        Guard::Perspective { .. } => &[],
    }
}

/// `Guard::Perspective`: who reaches which perspective.
fn perspective_rows() -> &'static [(Caller, Target, Expect)] {
    use Caller::*;
    use Expect::*;
    use Target::*;
    &[
        (Operator, UsersOwn, Granted),
        (Operator, MainAgents, Granted),
        (Operator, Unowned, Granted),
        (Operator, Missing, NotFound),
        (AppWithout, Unowned, Forbidden),
        (AppWith, Unowned, Granted),
        (AppWith, MainAgents, Granted),
        (AppWith, UsersOwn, NotOwner),
        (User, UsersOwn, Granted),
        (User, Unowned, NotOwner),
        (User, MainAgents, NotOwner),
        (OtherUser, UsersOwn, NotOwner),
        (Anonymous, Unowned, Forbidden),
    ]
}

/// A method and the params it needs beyond the perspective's uuid.
type ExtraParams = (&'static str, fn() -> Value);

const EXTRA_PARAMS: &[ExtraParams] = &[
    ("test.perspective.hidden", || json!({ "note": "fixture" })),
    // Guard::Node. `admit` runs no handler, so no path, key or proof here is used.
    ("agent.generate", || json!({ "passphrase": "p" })),
    ("agent.lock", || json!({ "passphrase": "p" })),
    (
        "agent.unlock",
        || json!({ "passphrase": "p", "holochain": false }),
    ),
    ("agent.removeApp", || json!({ "id": "request-id" })),
    ("agent.revokeToken", || json!({ "token": "request-id" })),
    ("agent.addTrustedAgents", || json!({ "agents": [] })),
    ("agent.byDid", || json!({ "did": "did:key:z6Mk" })),
    (
        "agent.import",
        || json!({ "did": "did:key:z6Mk", "didDocument": "{}", "keystore": "k", "passphrase": "p" }),
    ),
    ("agent.sign", || json!({ "message": "m" })),
    (
        "agent.requestCapability",
        || json!({ "authInfo": { "appName": "a", "appDesc": "d", "appDomain": "example.org" } }),
    ),
    ("agent.permitCapability", || json!({ "auth": "{}" })),
    (
        "agent.generateJwt",
        || json!({ "requestId": "request-id", "rand": "123" }),
    ),
    ("agent.deleteTrustedAgents", || json!({ "agents": [] })),
    (
        "agent.addEntanglementProofs",
        || json!({ "proofs": [entanglement_proof()] }),
    ),
    (
        "agent.deleteEntanglementProofs",
        || json!({ "proofs": [entanglement_proof()] }),
    ),
    (
        "agent.entanglementProofPreflight",
        || json!({ "deviceKey": "k", "deviceKeyType": "t" }),
    ),
    (
        "ai.discoverModels",
        || json!({ "baseUrl": "https://api.example.org/v1" }),
    ),
    (
        "ai.addModel",
        || json!({ "model": { "name": "m", "type": "LLM" } }),
    ),
    (
        "ai.updateModel",
        || json!({ "id": "m1", "model": { "name": "m", "type": "LLM" } }),
    ),
    ("ai.removeModel", || json!({ "id": "m1" })),
    ("ai.getDefaultModel", || json!({ "modelType": "LLM" })),
    ("ai.addTask", || json!({ "task": ai_task() })),
    ("ai.updateTask", || {
        let mut task = ai_task();
        task["taskId"] = json!("t1");
        task["createdAt"] = json!("2026-01-01T00:00:00Z");
        task["updatedAt"] = json!("2026-01-01T00:00:00Z");
        json!({ "task": task })
    }),
    ("ai.removeTask", || json!({ "id": "t1" })),
    ("ai.prompt", || json!({ "taskId": "t1", "prompt": "p" })),
    ("ai.embed", || json!({ "modelId": "m1", "text": "t" })),
    ("ai.transcriptionOpen", || json!({ "modelId": "m1" })),
    ("ai.transcriptionClose", || json!({ "streamId": "s1" })),
    (
        "ai.setDefaultModel",
        || json!({ "id": "m1", "modelType": "LLM" }),
    ),
    ("language.publish", || {
        json!({
            "languagePath": "/nonexistent/bundle.js",
            "languageMeta": { "name": "n", "description": "d" },
        })
    }),
    ("language.remove", || json!({ "address": "QmLang" })),
    ("language.get", || json!({ "address": "QmLang" })),
    ("language.meta", || json!({ "address": "QmLang" })),
    ("language.source", || json!({ "address": "QmLang" })),
    (
        "language.applyTemplate",
        || json!({ "sourceLanguageHash": "QmLang", "templateData": "{}" }),
    ),
    (
        "language.writeSettings",
        || json!({ "address": "QmLang", "settings": {} }),
    ),
    (
        "runtime.exportData",
        || json!({ "type": "db", "filePath": "/nonexistent/dump.json" }),
    ),
    (
        "runtime.importData",
        || json!({ "type": "db", "filePath": "/nonexistent/dump.json" }),
    ),
    ("runtime.addFriends", || json!({ "dids": [] })),
    ("runtime.removeFriends", || json!({ "dids": [] })),
    (
        "runtime.sendFriendMessage",
        || json!({ "did": "did:key:z6Mk", "message": { "links": [] } }),
    ),
    (
        "runtime.addLinkLanguageTemplates",
        || json!({ "addresses": [] }),
    ),
    (
        "runtime.removeLinkLanguageTemplates",
        || json!({ "addresses": [] }),
    ),
    ("runtime.addHcAgentInfos", || json!({ "agentInfos": [] })),
    (
        "runtime.setFreeHostingEnabled",
        || json!({ "enabled": true }),
    ),
    ("runtime.setStatus", || json!({ "status": {} })),
    (
        "runtime.verifySignature",
        || json!({ "did": "did:key:z6Mk", "data": "d", "signedData": "s" }),
    ),
    ("runtime.friendStatus", || json!({ "did": "did:key:z6Mk" })),
    ("runtime.createNotification", notification),
    ("runtime.updateNotification", || {
        let mut params = notification();
        params["id"] = json!("n1");
        params
    }),
    (
        "runtime.grantNotification",
        || json!({ "id": "n1", "granted": true }),
    ),
    ("runtime.deleteNotification", || json!({ "id": "n1" })),
    (
        "user.create",
        || json!({ "email": "new@example.org", "password": "p" }),
    ),
    (
        "user.login",
        || json!({ "email": "new@example.org", "password": "p" }),
    ),
    (
        "user.verifyEmail",
        || json!({ "email": "new@example.org", "code": "123456" }),
    ),
    (
        "user.requestVerification",
        || json!({ "email": "new@example.org" }),
    ),
    ("user.wallet", || json!({ "email": "new@example.org" })),
    ("user.emailTest", || json!({ "action": "clear" })),
    (
        "neighbourhood.join",
        || json!({ "url": "neighbourhood://Qm" }),
    ),
    (
        "neighbourhood.publish",
        || json!({ "perspectiveUuid": "p", "linkLanguage": "QmLang", "meta": { "links": [] } }),
    ),
    (
        "neighbourhood.sendBroadcast",
        || json!({ "uuid": "p", "payload": { "links": [] } }),
    ),
    (
        "neighbourhood.sendSignal",
        || json!({ "uuid": "p", "remoteAgentDid": "did:key:z6Mk", "payload": { "links": [] } }),
    ),
    (
        "neighbourhood.setOnlineStatus",
        || json!({ "uuid": "p", "status": { "links": [] } }),
    ),
    ("neighbourhood.hasTelepresence", || json!({ "uuid": "p" })),
    ("neighbourhood.onlineAgents", || json!({ "uuid": "p" })),
    ("neighbourhood.otherAgents", || json!({ "uuid": "p" })),
    ("expression.get", || json!({ "url": "literal://string:x" })),
    (
        "expression.getRaw",
        || json!({ "url": "literal://string:x" }),
    ),
    ("expression.getMany", || json!({ "urls": [] })),
    (
        "expression.create",
        || json!({ "content": "x", "languageAddress": "literal" }),
    ),
    (
        "expression.interactions",
        || json!({ "url": "literal://string:x" }),
    ),
    ("expression.interact", || {
        json!({
            "url": "literal://string:x",
            "interactionCall": { "name": "n", "parametersStringified": "{}" },
        })
    }),
    ("hosting.requestPayment", || json!({ "amountHOT": "1" })),
    ("hosting.setHotWallet", || json!({ "address": "a" })),
    // Guard::Perspective: the params beside `uuid`.
    ("perspective.queryProlog", || json!({ "query": "true." })),
    ("perspective.querySparql", || json!({ "query": "ASK {}" })),
    ("perspective.subscribeQuery", || json!({ "query": "true." })),
    (
        "perspective.subscribeSparql",
        || json!({ "query": "ASK {}" }),
    ),
    (
        "perspective.keepAliveQuery",
        || json!({ "subscriptionId": "s" }),
    ),
    (
        "perspective.disposeQuery",
        || json!({ "subscriptionId": "s" }),
    ),
    (
        "perspective.keepAliveSparql",
        || json!({ "subscriptionId": "s" }),
    ),
    (
        "perspective.disposeSparql",
        || json!({ "subscriptionId": "s" }),
    ),
    (
        "perspective.getSubjectData",
        || json!({ "subjectClass": "C", "expressionAddress": "e" }),
    ),
    (
        "perspective.modelQuery",
        || json!({ "class_name": "C", "query_json": "{}" }),
    ),
    (
        "perspective.modelSubscribe",
        || json!({ "class_name": "C", "query_json": "{}" }),
    ),
    (
        "perspective.evaluateGetters",
        || json!({ "class_name": "C" }),
    ),
    ("perspective.verifyFlowReceipt", || json!({ "receipt": {} })),
    ("perspective.flowValidOutputs", || json!({ "flow": "f" })),
    ("perspective.getShaclTargetClass", || json!({ "name": "S" })),
    ("perspective.getShacl", || json!({ "name": "S" })),
    ("perspective.update", || json!({ "name": "n" })),
    ("perspective.addLink", || json!({ "link": link() })),
    ("perspective.addLinkExpression", || {
        json!({ "link": {
            "author": "did:key:a",
            "timestamp": "2026-01-01T00:00:00Z",
            "data": link(),
            "proof": { "key": "k", "signature": "s" },
        } })
    }),
    ("perspective.addLinks", || json!({ "links": [link()] })),
    (
        "perspective.updateLink",
        || json!({ "oldLink": link_expression(), "newLink": link() }),
    ),
    (
        "perspective.removeLink",
        || json!({ "link": link_expression() }),
    ),
    (
        "perspective.removeLinks",
        || json!({ "links": [link_expression()] }),
    ),
    (
        "perspective.linkMutations",
        || json!({ "mutations": { "additions": [link()], "removals": [] } }),
    ),
    (
        "perspective.addSdna",
        || json!({ "name": "C", "sdnaType": "subject_class" }),
    ),
    (
        "perspective.executeCommands",
        || json!({ "commands": "[]", "expression": "e" }),
    ),
    (
        "perspective.createSubject",
        || json!({ "subjectClass": "C", "expressionAddress": "e" }),
    ),
    ("perspective.commitBatch", || json!({ "batchId": "b" })),
    (
        "perspective.runInterpretation",
        || json!({ "transcript": [], "basePrefix": "test://" }),
    ),
    (
        "perspective.runInterpretationWithHarness",
        || json!({ "transcript": [], "basePrefix": "test://", "maxToolCalls": 1 }),
    ),
    ("perspective.addAutoProcessor", || {
        json!({
            "processorId": "p",
            "sourceScopeQuery": "SELECT * {}",
            "interpretationClasses": ["C"],
            "debounceMs": 0,
            "batchMax": 1,
            "claimTtlMs": 1,
        })
    }),
    (
        "perspective.removeAutoProcessor",
        || json!({ "processorId": "p" }),
    ),
    (
        "perspective.acceptInterpretation",
        || json!({ "base": "b" }),
    ),
    (
        "perspective.rejectInterpretation",
        || json!({ "base": "b" }),
    ),
    (
        "perspective.acceptFlowProposal",
        || json!({ "proposalUri": "p" }),
    ),
    (
        "perspective.rejectFlowProposal",
        || json!({ "proposalUri": "p" }),
    ),
    (
        "perspective.proposeFlowTransition",
        || json!({ "instanceUri": "i", "toState": "s" }),
    ),
    (
        "perspective.mintFlowReceipt",
        || json!({ "instanceUri": "i" }),
    ),
];

fn link() -> Value {
    json!({ "source": "s", "target": "t" })
}

fn link_expression() -> Value {
    json!({
        "author": "did:key:a",
        "data": link(),
        "proof": {},
        "timestamp": "2026-01-01T00:00:00Z",
    })
}

fn ai_task() -> Value {
    json!({ "name": "n", "modelId": "m1", "systemPrompt": "s", "promptExamples": [] })
}

fn notification() -> Value {
    json!({
        "description": "d",
        "appName": "a",
        "appUrl": "https://example.org",
        "trigger": "t",
        "perspectiveIds": [],
        "webhookUrl": "https://example.org/hook",
        "webhookAuth": "w",
    })
}

fn entanglement_proof() -> Value {
    json!({
        "deviceKey": "k",
        "deviceKeySignedByDid": "s",
        "deviceKeyType": "t",
        "did": "did:key:z6Mk",
        "didSignedByDeviceKey": "s",
        "didSigningKeyId": "i",
    })
}

fn params(method: &str, uuid: Option<&str>) -> Value {
    let mut params = EXTRA_PARAMS
        .iter()
        .find(|(name, _)| *name == method)
        .map_or_else(|| json!({}), |(_, extra)| extra());
    if let Some(uuid) = uuid {
        params["uuid"] = json!(uuid);
    }
    params
}

fn context(caller: Caller, capability: Capability) -> RequestContext {
    match caller {
        Caller::Operator => RequestContext::operator_for_test(),
        Caller::AppWithout => RequestContext::app_for_test(vec![]),
        Caller::AppWith => RequestContext::app_for_test(vec![capability]),
        Caller::User => {
            RequestContext::user_for_test("user@example.org", USER_DID, vec![capability])
        }
        Caller::OtherUser => {
            RequestContext::user_for_test("other@example.org", OTHER_USER_DID, vec![capability])
        }
        // The most a caller without a token holds: multi-user mode on adds signing up,
        // logging in and verifying an email.
        Caller::Anonymous => {
            RequestContext::for_test(crate::agent::session::Principal::Anonymous {
                caps: anonymous_capabilities_when(true),
            })
        }
    }
}

fn outcome(result: Result<Admission, WsRpcError>) -> Result<Admission, u16> {
    result.map_err(|e| e.code)
}

/// The perspectives the rows name; removed when dropped.
struct World {
    users_own: String,
    main_agents: String,
    unowned: String,
}

impl World {
    async fn new() -> World {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        if crate::db::Ad4mDb::global_instance()
            .lock()
            .unwrap()
            .is_none()
        {
            crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        }
        let main_did = AgentService::with_global_instance(|a| a.did.clone()).expect("main DID");
        let run = uuid::Uuid::new_v4();
        let world = World {
            users_own: format!("{run}-users-own"),
            main_agents: format!("{run}-main-agents"),
            unowned: format!("{run}-unowned"),
        };
        for (uuid, owners) in [
            (&world.users_own, Some(vec![USER_DID.to_string()])),
            (&world.main_agents, Some(vec![main_did])),
            (&world.unowned, None),
        ] {
            let handle = PerspectiveHandle {
                uuid: uuid.clone(),
                owners,
                ..Default::default()
            };
            crate::perspectives::add_perspective(handle, None)
                .await
                .unwrap();
        }
        world
    }

    fn uuid(&self, target: Target) -> String {
        match target {
            Target::UsersOwn => self.users_own.clone(),
            Target::MainAgents => self.main_agents.clone(),
            Target::Unowned => self.unowned.clone(),
            Target::Missing => format!("{}-missing", uuid::Uuid::new_v4()),
        }
    }

    async fn remove(self) {
        for uuid in [&self.users_own, &self.main_agents, &self.unowned] {
            crate::perspectives::remove_perspective(uuid).await;
        }
    }
}

/// Every row of every guarded method in `map` whose answer differs from the row's, and the
/// number of rows checked.
async fn walk(map: &HandlerMap, world: &World) -> (Vec<String>, usize) {
    let mut failures = Vec::new();
    let mut checked = 0;
    for (method, guard) in map.guards() {
        let Some(guard) = guard else { continue };
        let hides = matches!(
            guard,
            Guard::Perspective {
                hides_existence: true,
                ..
            }
        );
        let mut cases: Vec<(Caller, Option<String>, Expect)> = resource_rows(guard)
            .iter()
            .map(|&(caller, expect)| (caller, None, expect))
            .collect();
        if matches!(guard, Guard::Perspective { .. }) {
            cases.extend(
                perspective_rows()
                    .iter()
                    .map(|&(caller, target, expect)| (caller, Some(world.uuid(target)), expect)),
            );
        }
        for (caller, uuid, expect) in cases {
            let ctx = context(caller, guard.capability("*"));
            let got = outcome(
                map.admit(method, &params(method, uuid.as_deref()), &ctx)
                    .await,
            );
            let want = match expect {
                Expect::Granted => Ok(Admission::Granted),
                Expect::Forbidden => Err(403),
                Expect::NotOwner if hides => Ok(Admission::Hidden),
                Expect::NotOwner => Err(403),
                Expect::NotFound if hides => Ok(Admission::Hidden),
                Expect::NotFound => Err(404),
            };
            checked += 1;
            if got != want {
                failures.push(format!(
                    "{method} as {caller:?} on {uuid:?}: expected {want:?}, got {got:?}"
                ));
            }
        }
    }
    (failures, checked)
}

/// The params of a test method that needs more than the uuid ([`EXTRA_PARAMS`]).
#[allow(dead_code)]
#[derive(Deserialize, TS)]
struct UuidAndNote {
    uuid: String,
    note: String,
}

/// One test-only method per guard kind; each handler counts its calls in `calls`.
fn register_test_methods(map: &mut HandlerMap, calls: &Arc<AtomicUsize>) {
    let handler = || {
        let calls = calls.clone();
        move |_: Value, _: Arc<RequestContext>| {
            let calls = calls.clone();
            async move {
                calls.fetch_add(1, Ordering::SeqCst);
                Ok::<_, WsRpcError>(json!(true))
            }
        }
    };
    map.method::<NoParams, bool>("test.anonymous", handler())
        .guard(Guard::Anonymous(AGENT_AUTH_CAPABILITY.clone()));
    map.method::<NoParams, bool>("test.node", handler())
        .guard(Guard::Node(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<NoParams, bool>("test.self", handler())
        .read()
        .guard(Guard::SelfAgent(AGENT_READ_CAPABILITY.clone()));
    map.method::<PerspectiveUuidParams, bool>("test.perspective", handler())
        .guard(Guard::Perspective {
            capability: perspective_query_capability,
            uuid: uuid_param,
            hides_existence: false,
        });
    map.method::<UuidAndNote, Option<bool>>("test.perspective.hidden", handler())
        .read()
        .guard(Guard::Perspective {
            capability: perspective_query_capability,
            uuid: uuid_param,
            hides_existence: true,
        });
    map.method::<NoParams, bool>("test.unguarded", handler());
}

#[tokio::test]
async fn every_guarded_method_admits_exactly_the_callers_its_guard_names() {
    let world = World::new().await;
    let mut map = build_handler_map();
    register_test_methods(&mut map, &Arc::new(AtomicUsize::new(0)));

    let (failures, checked) = walk(&map, &world).await;
    world.remove().await;
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    // 3 resource guards × 5 rows, 2 perspective guards × 13 rows: the walk reached them all.
    assert!(checked >= 3 * 5 + 2 * 13, "only {checked} rows checked");
}

#[tokio::test]
async fn dispatch_runs_the_guard_after_the_params_and_before_the_handler() {
    let world = World::new().await;
    let calls = Arc::new(AtomicUsize::new(0));
    let mut map = HandlerMap::new();
    register_test_methods(&mut map, &calls);
    let call = |method: &'static str, params: Value, caller: Caller, capability: &Capability| {
        let ctx = Arc::new(context(caller, capability.clone()));
        let map = &map;
        async move { map.dispatch(method, params, ctx).await.map_err(|e| e.code) }
    };
    let query = perspective_query_capability(vec!["*".into()]);
    let note = |uuid: &str| json!({ "uuid": uuid, "note": "n" });

    // Params first: a caller without the capability still gets 400 for bad params.
    assert_eq!(
        call("test.perspective", json!({}), Caller::AppWithout, &query).await,
        Err(400)
    );
    // Then the capability, then the owner rule; the handler never runs.
    assert_eq!(
        call(
            "test.node",
            json!({}),
            Caller::AppWithout,
            &AGENT_UPDATE_CAPABILITY
        )
        .await,
        Err(403)
    );
    assert_eq!(
        call(
            "test.node",
            json!({}),
            Caller::User,
            &AGENT_UPDATE_CAPABILITY
        )
        .await,
        Err(403)
    );
    let others = json!({ "uuid": world.users_own });
    assert_eq!(
        call("test.perspective", others, Caller::OtherUser, &query).await,
        Err(403)
    );
    assert_eq!(
        calls.load(Ordering::SeqCst),
        0,
        "no refused call ran its handler"
    );

    // A method that hides existence answers null without running its handler, for another
    // user's perspective and for a missing one alike. The capability still refuses with 403.
    for uuid in [world.users_own.clone(), world.uuid(Target::Missing)] {
        assert_eq!(
            call(
                "test.perspective.hidden",
                note(&uuid),
                Caller::OtherUser,
                &query
            )
            .await,
            Ok(Value::Null)
        );
    }
    assert_eq!(
        call(
            "test.perspective.hidden",
            note(&world.users_own),
            Caller::AppWithout,
            &query
        )
        .await,
        Err(403)
    );
    assert_eq!(
        calls.load(Ordering::SeqCst),
        0,
        "a hidden answer ran no handler"
    );

    // Admitted: the handler runs and its result goes back.
    assert_eq!(
        call(
            "test.perspective.hidden",
            note(&world.users_own),
            Caller::User,
            &query
        )
        .await,
        Ok(json!(true))
    );
    assert_eq!(
        call(
            "test.node",
            json!({}),
            Caller::Operator,
            &AGENT_UPDATE_CAPABILITY
        )
        .await,
        Ok(json!(true))
    );
    assert_eq!(calls.load(Ordering::SeqCst), 2);
    world.remove().await;
}

#[test]
fn every_method_on_one_perspective_has_a_guard() {
    let map = build_handler_map();
    let unguarded: Vec<&str> = map
        .guards()
        .into_iter()
        .filter(|(name, guard)| name.starts_with("perspective.") && guard.is_none())
        .map(|(name, _)| name)
        .collect();
    // Neither names one perspective: `all` lists the caller's, `create` makes a new one.
    assert_eq!(unguarded, vec!["perspective.all", "perspective.create"]);
}

/// Every guard checks a capability first. A method that checks none keeps no guard: a guard
/// would refuse the callers that reach it today without a capability. Nor does a method that
/// answers every caller but the operator "Admin credential required": a guard would answer
/// with the capability's message. `perspective.all` and `perspective.create` name no
/// perspective.
#[test]
fn only_methods_a_guard_cannot_express_lack_one() {
    let map = build_handler_map();
    let unguarded: Vec<&str> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| guard.is_none().then_some(name))
        .collect();
    assert_eq!(
        unguarded,
        vec![
            "agent.isLocked",
            "perspective.all",
            "perspective.create",
            "runtime.freeHostingEnabled",
            "runtime.info",
            "runtime.openLink",
            "runtime.restartHolochain",
            "runtime.setHostRates",
            "runtime.setUnytMembraneProof",
            "runtime.tlsDomain",
            "runtime.unytAgentKey",
            "runtime.unytHotAgentPubkey",
            "runtime.unytReinstallDna",
            "runtime.unytSendHot",
            "runtime.unytWalletBalance",
            "runtime.unytWalletHistory",
            "user.credits",
            "user.freeAccess",
            "user.logout",
            "user.revokeAllSessions",
            "user.setMultiUserEnabled",
        ]
    );
}

#[tokio::test]
async fn a_method_without_a_guard_leaves_every_check_to_its_handler() {
    let calls = Arc::new(AtomicUsize::new(0));
    let mut map = HandlerMap::new();
    register_test_methods(&mut map, &calls);
    let anonymous = Arc::new(RequestContext::for_test(
        crate::agent::session::Principal::Anonymous { caps: vec![] },
    ));
    assert_eq!(
        map.dispatch("test.unguarded", json!({}), anonymous)
            .await
            .unwrap(),
        json!(true)
    );
    assert_eq!(calls.load(Ordering::SeqCst), 1);

    let guards = map.guards();
    let unguarded: Vec<&str> = guards
        .iter()
        .filter_map(|(name, guard)| guard.is_none().then_some(*name))
        .collect();
    assert_eq!(unguarded, vec!["test.unguarded"]);
}

#[test]
#[should_panic(expected = "has two guards")]
fn a_method_takes_one_guard() {
    let mut map = HandlerMap::new();
    map.method::<NoParams, bool>("test.twice", |_: Value, _: Arc<RequestContext>| async {
        Ok::<_, WsRpcError>(json!(true))
    })
    .guard(Guard::Node(AGENT_UPDATE_CAPABILITY.clone()))
    .guard(Guard::Node(AGENT_UPDATE_CAPABILITY.clone()));
}

#[test]
#[should_panic(expected = "not dispatched through the map")]
fn a_method_the_socket_handles_itself_takes_no_guard() {
    HandlerMap::new()
        .inline::<NoParams, bool>("test.inline")
        .guard(Guard::Anonymous(AGENT_AUTH_CAPABILITY.clone()));
}
