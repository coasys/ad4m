//! The method matrix: every method, called by each kind of caller on resources of its own and
//! of others, gets the answer its guard names. Every method has a guard: the walk fails on
//! one without.
//!
//! The walk reads the guards from the handler map, so a method that gets a guard is covered
//! without a new test. The rows are per guard kind ([`resource_rows`], [`perspective_rows`],
//! [`task_rows`]).
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
    AGENT_READ_CAPABILITY, AGENT_UPDATE_CAPABILITY, AI_PROMPT_CAPABILITY,
};
use crate::agent::AgentService;
use crate::api::guards::{uuid_param, Admission, Guard};
use crate::api::perspectives_ws::PerspectiveUuidParams;
use crate::api::ws_handler::{build_handler_map, HandlerMap, NoParams, WsRpcError};
use crate::types::{PerspectiveHandle, RequestContext};

pub(super) const USER_EMAIL: &str = "user@example.org";
pub(super) const USER_DID: &str = "did:key:matrix-user";
pub(super) const OTHER_USER_DID: &str = "did:key:matrix-other-user";

/// Who calls.
#[derive(Clone, Copy, Debug)]
pub(super) enum Caller {
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
pub(super) enum Target {
    /// Owned by [`Caller::User`].
    UsersOwn,
    /// Owned by the node's main agent.
    MainAgents,
    Unowned,
    /// No such perspective.
    Missing,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Expect {
    Granted,
    Forbidden,
    /// 403 from the owner rule: `null` for a method that hides existence.
    NotOwner,
    /// 404: `null` for a method that hides existence.
    NotFound,
}

/// Guards that name no resource: who passes each. A guard that names one has its own rows
/// ([`perspective_rows`], [`task_rows`]).
pub(super) fn resource_rows(guard: &Guard) -> &'static [(Caller, Expect)] {
    use Caller::*;
    use Expect::*;
    match guard {
        Guard::Open => &[
            (Operator, Granted),
            (AppWithout, Granted),
            (AppWith, Granted),
            (User, Granted),
            (Anonymous, Granted),
        ],
        Guard::Operator => &[
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Forbidden),
            (User, Forbidden),
            (Anonymous, Forbidden),
        ],
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
        Guard::Perspective { .. } | Guard::Task { .. } => &[],
    }
}

/// `Guard::Task`: who reaches which AI task. For tasks, [`Target::UsersOwn`] is a task of
/// [`Caller::User`] and [`Target::MainAgents`] an operator task (no user). A user's refusal
/// is 404, so it cannot learn which task ids exist; the operator and apps reach every task,
/// and the handler answers for a missing one.
fn task_rows() -> &'static [(Caller, Target, Expect)] {
    use Caller::*;
    use Expect::*;
    use Target::*;
    &[
        (Operator, UsersOwn, Granted),
        (Operator, MainAgents, Granted),
        (Operator, Missing, Granted),
        (AppWithout, UsersOwn, Forbidden),
        (AppWith, UsersOwn, Granted),
        (AppWith, MainAgents, Granted),
        (AppWith, Missing, Granted),
        (User, UsersOwn, Granted),
        (User, MainAgents, NotFound),
        (User, Missing, NotFound),
        (OtherUser, UsersOwn, NotFound),
        (Anonymous, UsersOwn, Forbidden),
    ]
}

/// `Guard::Perspective`: who reaches which perspective.
pub(super) fn perspective_rows() -> &'static [(Caller, Target, Expect)] {
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

/// Stands for the row's perspective or task in [`EXTRA_PARAMS`], where a method names it in a
/// param other than `uuid`. [`params`] replaces it with the row's id.
const TARGET: &str = "@target";

const EXTRA_PARAMS: &[ExtraParams] = &[
    ("test.perspective.hidden", || json!({ "note": "fixture" })),
    ("test.task", || json!({ "taskId": TARGET })),
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
        task["taskId"] = json!(TARGET);
        task["createdAt"] = json!("2026-01-01T00:00:00Z");
        task["updatedAt"] = json!("2026-01-01T00:00:00Z");
        json!({ "task": task })
    }),
    ("ai.removeTask", || json!({ "id": TARGET })),
    ("ai.prompt", || json!({ "taskId": TARGET, "prompt": "p" })),
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
        || json!({ "perspectiveUuid": TARGET, "linkLanguage": "QmLang", "meta": { "links": [] } }),
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
    // Guard::Operator and Guard::Open.
    (
        "runtime.openLink",
        || json!({ "url": "https://example.org" }),
    ),
    ("runtime.setHostRates", || json!({ "rates": [] })),
    ("runtime.setUnytMembraneProof", || json!({ "proof": "p" })),
    (
        "runtime.unytSendHot",
        || json!({ "recipient": "r", "amount": "1" }),
    ),
    ("user.setMultiUserEnabled", || json!({ "enabled": true })),
    (
        "user.freeAccess",
        || json!({ "email": "new@example.org", "enabled": true }),
    ),
    (
        "user.credits",
        || json!({ "email": "new@example.org", "amount": 1.0 }),
    ),
    ("perspective.create", || json!({ "name": "n" })),
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

/// The params of `method` naming `target` (a perspective or a task): as the `uuid` param of a
/// perspective method, and wherever its [`EXTRA_PARAMS`] put [`TARGET`].
fn params(method: &str, target: Option<&str>, uuid_param: bool) -> Value {
    fn replace(value: &mut Value, target: &str) {
        match value {
            Value::String(s) if s == TARGET => *s = target.to_string(),
            Value::Array(items) => items.iter_mut().for_each(|v| replace(v, target)),
            Value::Object(fields) => fields.values_mut().for_each(|v| replace(v, target)),
            _ => {}
        }
    }
    let mut params = EXTRA_PARAMS
        .iter()
        .find(|(name, _)| *name == method)
        .map_or_else(|| json!({}), |(_, extra)| extra());
    if let Some(target) = target {
        replace(&mut params, target);
        if uuid_param {
            params["uuid"] = json!(target);
        }
    }
    params
}

/// The context of `caller`. "With the capability" holds `capability` (none for a guard that
/// checks none).
fn context(caller: Caller, capability: Option<Capability>) -> RequestContext {
    let caps = || capability.clone().into_iter().collect::<Vec<_>>();
    match caller {
        Caller::Operator => RequestContext::operator_for_test(),
        Caller::AppWithout => RequestContext::app_for_test(vec![]),
        Caller::AppWith => RequestContext::app_for_test(caps()),
        Caller::User => RequestContext::user_for_test(USER_EMAIL, USER_DID, caps()),
        Caller::OtherUser => {
            RequestContext::user_for_test("other@example.org", OTHER_USER_DID, caps())
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

/// The perspectives and AI tasks the rows name; [`World::remove`] removes them.
pub(super) struct World {
    users_own: String,
    main_agents: String,
    unowned: String,
    users_task: String,
    operators_task: String,
}

impl World {
    pub(super) async fn new() -> World {
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
        let task = |owner: Option<&str>| {
            crate::db::Ad4mDb::with_global_instance(|db| {
                db.add_task(
                    format!("{run}-task"),
                    "m1".to_string(),
                    "s".to_string(),
                    vec![],
                    None,
                    owner,
                )
            })
            .expect("a task")
        };
        let world = World {
            users_own: format!("{run}-users-own"),
            main_agents: format!("{run}-main-agents"),
            unowned: format!("{run}-unowned"),
            users_task: task(Some(USER_EMAIL)),
            operators_task: task(None),
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

    pub(super) fn uuid(&self, target: Target) -> String {
        match target {
            Target::UsersOwn => self.users_own.clone(),
            Target::MainAgents => self.main_agents.clone(),
            Target::Unowned => self.unowned.clone(),
            Target::Missing => format!("{}-missing", uuid::Uuid::new_v4()),
        }
    }

    /// The task a task row names: the user's own, or an operator task for
    /// [`Target::MainAgents`].
    fn task(&self, target: Target) -> String {
        match target {
            Target::UsersOwn => self.users_task.clone(),
            Target::MainAgents => self.operators_task.clone(),
            Target::Unowned | Target::Missing => format!("{}-missing", uuid::Uuid::new_v4()),
        }
    }

    pub(super) async fn remove(self) {
        for uuid in [&self.users_own, &self.main_agents, &self.unowned] {
            crate::perspectives::remove_perspective(uuid).await;
        }
        for task in [self.users_task, self.operators_task] {
            crate::db::Ad4mDb::with_global_instance(|db| db.remove_task(task)).unwrap();
        }
    }
}

/// Every row of every method in `map` whose answer differs from the row's (and every method
/// without a guard), and the number of rows checked.
async fn walk(map: &HandlerMap, world: &World) -> (Vec<String>, usize) {
    let mut failures = Vec::new();
    let mut checked = 0;
    for (method, guard) in map.guards() {
        let Some(guard) = guard else {
            failures.push(format!("{method} has no guard"));
            continue;
        };
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
        let names_perspective = matches!(guard, Guard::Perspective { .. });
        if names_perspective {
            cases.extend(
                perspective_rows()
                    .iter()
                    .map(|&(caller, target, expect)| (caller, Some(world.uuid(target)), expect)),
            );
        }
        if matches!(guard, Guard::Task { .. }) {
            cases.extend(
                task_rows()
                    .iter()
                    .map(|&(caller, target, expect)| (caller, Some(world.task(target)), expect)),
            );
        }
        for (caller, target, expect) in cases {
            let ctx = context(caller, guard.capability("*"));
            let params = params(method, target.as_deref(), names_perspective);
            let got = outcome(map.admit(method, &params, &ctx).await);
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
                    "{method} as {caller:?} on {target:?}: expected {want:?}, got {got:?}"
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

/// The params of the test method on one AI task.
#[allow(dead_code)]
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
struct TaskIdParams {
    task_id: String,
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
    map.method::<TaskIdParams, bool>("test.task", handler())
        .guard(Guard::Task {
            capability: AI_PROMPT_CAPABILITY.clone(),
            task_id: |params| params.get("taskId")?.as_str().map(str::to_string),
        });
    map.method::<NoParams, bool>("test.open", handler())
        .guard(Guard::Open);
    map.method::<NoParams, bool>("test.operator", handler())
        .guard(Guard::Operator);
}

#[tokio::test]
async fn every_method_admits_exactly_the_callers_its_guard_names() {
    let world = World::new().await;
    let mut map = build_handler_map();
    register_test_methods(&mut map, &Arc::new(AtomicUsize::new(0)));

    let (failures, checked) = walk(&map, &world).await;
    world.remove().await;
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    // 5 resource guards × 5 rows, 2 perspective guards × 13 rows, 1 task guard × 12 rows:
    // the walk reached them all.
    assert!(
        checked >= 5 * 5 + 2 * 13 + 12,
        "only {checked} rows checked"
    );
}

/// `Guard::Operator` answers every other caller as the handlers did before it: 403 "Admin
/// credential required", whatever the caller's capabilities.
#[tokio::test]
async fn operator_methods_answer_everyone_else_admin_credential_required() {
    let map = build_handler_map();
    let operator_methods: Vec<&str> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| matches!(guard, Some(Guard::Operator)).then_some(name))
        .collect();
    assert_eq!(
        operator_methods,
        vec![
            "runtime.openLink",
            "runtime.restartHolochain",
            "runtime.setHostRates",
            "runtime.setUnytMembraneProof",
            "user.freeAccess",
            "user.setMultiUserEnabled",
        ]
    );
    let all = Some(crate::agent::capabilities::ALL_CAPABILITY.clone());
    for method in operator_methods {
        for caller in [
            Caller::AppWithout,
            Caller::AppWith,
            Caller::User,
            Caller::Anonymous,
        ] {
            let err = map
                .admit(
                    method,
                    &params(method, None, false),
                    &context(caller, all.clone()),
                )
                .await
                .unwrap_err();
            assert_eq!(
                (err.code, err.message.as_str()),
                (403, "Admin credential required"),
                "{method} as {caller:?}"
            );
        }
    }
}

#[tokio::test]
async fn dispatch_runs_the_guard_after_the_params_and_before_the_handler() {
    let world = World::new().await;
    let calls = Arc::new(AtomicUsize::new(0));
    let mut map = HandlerMap::new();
    register_test_methods(&mut map, &calls);
    let call = |method: &'static str, params: Value, caller: Caller, capability: &Capability| {
        let ctx = Arc::new(context(caller, Some(capability.clone())));
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
fn every_registered_method_has_a_guard() {
    let map = build_handler_map();
    let unguarded: Vec<&str> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| guard.is_none().then_some(name))
        .collect();
    assert_eq!(unguarded, Vec::<&str>::new());
}

/// The methods that check no capability: any caller, anonymous included, reaches the
/// handler.
#[test]
fn only_methods_that_check_no_capability_are_open() {
    let map = build_handler_map();
    let open: Vec<&str> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| matches!(guard, Some(Guard::Open)).then_some(name))
        .collect();
    assert_eq!(
        open,
        vec![
            "agent.isLocked",
            "runtime.freeHostingEnabled",
            "runtime.info",
            "runtime.tlsDomain",
            "runtime.unytAgentKey",
            "runtime.unytHotAgentPubkey",
            "runtime.unytReinstallDna",
            "runtime.unytSendHot",
            "runtime.unytWalletBalance",
            "runtime.unytWalletHistory",
            "user.credits",
            "user.logout",
            "user.revokeAllSessions",
        ]
    );
}

/// The executor builds its map with `require_guards`, so a method without a guard stops it
/// at start, naming the method.
#[test]
#[should_panic(expected = "WS RPC methods without a guard: test.unguarded.")]
fn a_map_with_a_method_without_a_guard_does_not_build() {
    let mut map = HandlerMap::new();
    register_test_methods(&mut map, &Arc::new(AtomicUsize::new(0)));
    map.method::<NoParams, bool>("test.unguarded", |_: Value, _: Arc<RequestContext>| async {
        Ok::<_, WsRpcError>(json!(true))
    });
    map.require_guards();
}

/// A map built without `require_guards` still refuses a method without a guard: its handler
/// never runs.
#[tokio::test]
async fn dispatch_refuses_a_method_without_a_guard() {
    let calls = Arc::new(AtomicUsize::new(0));
    let mut map = HandlerMap::new();
    let counted = calls.clone();
    map.method::<NoParams, bool>("test.unguarded", move |_: Value, _: Arc<RequestContext>| {
        let calls = counted.clone();
        async move {
            calls.fetch_add(1, Ordering::SeqCst);
            Ok::<_, WsRpcError>(json!(true))
        }
    });
    let err = map
        .dispatch(
            "test.unguarded",
            json!({}),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap_err();
    assert_eq!(err.code, 500, "{}", err.message);
    assert!(err.message.contains("no guard"), "{}", err.message);
    assert_eq!(calls.load(Ordering::SeqCst), 0);
}

/// `Guard::Open` runs the handler for every caller, one without any capability too;
/// `Guard::Operator` only for the operator.
#[tokio::test]
async fn open_and_operator_guards_run_the_handler_for_the_callers_they_name() {
    let calls = Arc::new(AtomicUsize::new(0));
    let mut map = HandlerMap::new();
    register_test_methods(&mut map, &calls);
    let anonymous = Arc::new(RequestContext::for_test(
        crate::agent::session::Principal::Anonymous { caps: vec![] },
    ));
    let operator = Arc::new(RequestContext::operator_for_test());
    assert_eq!(
        map.dispatch("test.open", json!({}), anonymous.clone())
            .await
            .unwrap(),
        json!(true)
    );
    assert_eq!(
        map.dispatch("test.operator", json!({}), anonymous)
            .await
            .map_err(|e| (e.code, e.message)),
        Err((403, "Admin credential required".to_string()))
    );
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    assert_eq!(
        map.dispatch("test.operator", json!({}), operator)
            .await
            .unwrap(),
        json!(true)
    );
    assert_eq!(calls.load(Ordering::SeqCst), 2);
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
