//! The method matrix: every method, called by each kind of caller on resources of its own and
//! of others, gets the answer its guard names. Every method has a guard: the walk fails on
//! one without.
//!
//! The walk reads the guards from the handler map, so a method that gets a guard is covered
//! without a new test. The rows are per guard kind ([`resource_rows`]) and, for
//! `Guard::Owns`, per kind of resource ([`perspective_rows`], [`agent_resource_rows`] for AI
//! tasks, grants and capability requests).
//! A method whose params need more than the perspective's uuid adds an entry to
//! [`EXTRA_PARAMS`]; without it the walk reports the method's 400. The walk calls
//! [`HandlerMap::admit`], the dispatcher's steps before the handler, so no handler runs.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use serde::Deserialize;
use serde_json::{json, Value};
use ts_rs::TS;

use crate::agent::capabilities::pending::PendingRequests;
use crate::agent::capabilities::{
    anonymous_capabilities_when, capability_matches, perspective_query_capability, AuthInfo,
    Capability, AGENT_AUTH_CAPABILITY, AGENT_READ_CAPABILITY, AGENT_UPDATE_CAPABILITY,
    AI_PROMPT_CAPABILITY,
};
use crate::agent::grants::{Grants, LiveGrant};
use crate::agent::{AgentContext, AgentService};
use crate::api::guards::{Guard, IdParam, Owner, UUID_PARAM};
use crate::api::perspectives_ws::PerspectiveUuidParams;
use crate::api::ws_handler::{build_handler_map, HandlerMap, NoParams, WsRpcError};
use crate::types::{PerspectiveHandle, RequestContext};

const USER_DID: &str = "did:key:matrix-user";
const OTHER_USER_DID: &str = "did:key:matrix-other-user";

/// Who calls.
#[derive(Clone, Copy, Debug)]
enum Caller {
    Operator,
    /// An app token on the main agent without the guard's capability.
    AppWithout,
    /// An app token on the main agent with the guard's capability.
    AppWith,
    /// A user session (a grant on the user's managed agent) with the guard's capability.
    User,
    /// A second user with the guard's capability.
    OtherUser,
    /// An app the user approved on their managed agent with only an unrelated narrow
    /// capability ([`unrelated_capability`]), not the guard's: it acts as the user's agent,
    /// so it owns the user's resources, but holds nothing the guards check.
    UserAppWithout,
    /// No token: the anonymous capabilities only.
    Anonymous,
}

/// The resource a call names.
#[derive(Clone, Copy, Debug)]
enum Target {
    /// Owned by [`Caller::User`]'s agent.
    UsersOwn,
    /// Owned by the executor's main agent.
    MainAgents,
    /// A perspective without owners (the main agent's).
    Unowned,
    /// No such resource.
    Missing,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Expect {
    Granted,
    /// 403: the capability, or the rule of `Operator` and `Executor`.
    Forbidden,
    /// 404: missing or not the caller's (D6).
    NotFound,
}

/// What [`Caller::UserAppWithout`] holds: querying one perspective that no row names. No
/// guard checks it: a perspective guard builds its capability from the row's uuid, and every
/// other perspective capability names `*`, which a single pointer does not cover.
fn unrelated_capability() -> Capability {
    perspective_query_capability(vec!["matrix-unrelated-perspective".to_string()])
}

/// Whether a caller without a token holds `cap` (multi-user mode on: the widest set).
fn anonymous_holds(cap: &Capability) -> bool {
    capability_matches(&anonymous_capabilities_when(true), cap)
}

/// Guards that name no resource: who passes each. `Guard::Owns` has its own rows
/// ([`owns_rows`]).
fn resource_rows(guard: &Guard) -> Vec<(Caller, Expect)> {
    use Caller::*;
    use Expect::*;
    match guard {
        Guard::Open => vec![
            (Operator, Granted),
            (AppWithout, Granted),
            (AppWith, Granted),
            (User, Granted),
            (UserAppWithout, Granted),
            (Anonymous, Granted),
        ],
        Guard::Operator => vec![
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Forbidden),
            (User, Forbidden),
            (UserAppWithout, Forbidden),
            (Anonymous, Forbidden),
        ],
        // The capability alone: a caller without a token passes exactly when the anonymous
        // set covers it.
        Guard::Cap(cap) => vec![
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Granted),
            (User, Granted),
            (OtherUser, Granted),
            (UserAppWithout, Forbidden),
            (
                Anonymous,
                if anonymous_holds(cap) {
                    Granted
                } else {
                    Forbidden
                },
            ),
        ],
        Guard::Executor(_) => vec![
            (Operator, Granted),
            (AppWithout, Forbidden),
            (AppWith, Granted),
            (User, Forbidden),
            (UserAppWithout, Forbidden),
            (Anonymous, Forbidden),
        ],
        Guard::Owns { .. } => vec![],
    }
}

/// `Guard::Owns` on a perspective: who reaches which.
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
        (AppWith, UsersOwn, NotFound),
        (AppWith, Missing, NotFound),
        (User, UsersOwn, Granted),
        (User, Unowned, NotFound),
        (User, MainAgents, NotFound),
        (User, Missing, NotFound),
        (OtherUser, UsersOwn, NotFound),
        // The capability comes first: 403 even on its own agent's perspective.
        (UserAppWithout, UsersOwn, Forbidden),
        (UserAppWithout, MainAgents, Forbidden),
        (Anonymous, Unowned, Forbidden),
    ]
}

/// `Guard::Owns` on an AI task, a grant or a capability request: [`Target::UsersOwn`] is
/// [`Caller::User`]'s agent's, [`Target::MainAgents`] the main agent's (a task without a
/// user, a grant or request on the main agent). The operator reaches all; every other session
/// its agent's; the rest is 404, missing or not.
fn agent_resource_rows() -> &'static [(Caller, Target, Expect)] {
    use Caller::*;
    use Expect::*;
    use Target::*;
    &[
        (Operator, UsersOwn, Granted),
        (Operator, MainAgents, Granted),
        (Operator, Missing, NotFound),
        (AppWithout, MainAgents, Forbidden),
        (AppWith, MainAgents, Granted),
        (AppWith, UsersOwn, NotFound),
        (AppWith, Missing, NotFound),
        (User, UsersOwn, Granted),
        (User, MainAgents, NotFound),
        (User, Missing, NotFound),
        (OtherUser, UsersOwn, NotFound),
        (OtherUser, MainAgents, NotFound),
        // The capability comes first: 403 even on its own agent's resource.
        (UserAppWithout, UsersOwn, Forbidden),
        (UserAppWithout, MainAgents, Forbidden),
        (Anonymous, UsersOwn, Forbidden),
    ]
}

/// The rows of `Guard::Owns` on `owner`.
fn owns_rows(owner: Owner) -> &'static [(Caller, Target, Expect)] {
    match owner {
        Owner::Perspective => perspective_rows(),
        Owner::Task | Owner::Grant | Owner::Request => agent_resource_rows(),
    }
}

/// A method and the params it needs beyond the perspective's uuid.
type ExtraParams = (&'static str, fn() -> Value);

/// Stands for the row's perspective or task in [`EXTRA_PARAMS`], where a method names it in a
/// param other than `uuid`. [`params`] replaces it with the row's id.
const TARGET: &str = "@target";

const EXTRA_PARAMS: &[ExtraParams] = &[
    ("test.perspective.or_null", || json!({ "note": "fixture" })),
    ("test.task", || json!({ "taskId": TARGET })),
    // Guard::Executor. `admit` runs no handler, so no path, key or proof here is used.
    ("agent.generate", || json!({ "passphrase": "p" })),
    ("agent.lock", || json!({ "passphrase": "p" })),
    (
        "agent.unlock",
        || json!({ "passphrase": "p", "holochain": false }),
    ),
    ("agent.removeApp", || json!({ "id": TARGET })),
    ("agent.revokeToken", || json!({ "token": TARGET })),
    ("agent.addTrustedAgents", || json!({ "agents": [] })),
    ("agent.byDid", || json!({ "did": "did:key:z6Mk" })),
    ("agent.byDIDs", || json!({ "dids": ["did:key:z6Mk"] })),
    (
        "agent.import",
        || json!({ "did": "did:key:z6Mk", "didDocument": "{}", "keystore": "k", "passphrase": "p" }),
    ),
    ("agent.sign", || json!({ "message": "m" })),
    (
        "agent.requestCapability",
        || json!({ "authInfo": { "appName": "a", "appDesc": "d", "appDomain": "example.org" } }),
    ),
    // The request id sits in a JSON string: [`params`] replaces `TARGET` inside strings too.
    ("agent.permitCapability", || {
        let auth = json!({ "requestId": TARGET, "auth": { "appName": "a", "appDesc": "d" } });
        json!({ "auth": auth.to_string() })
    }),
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
    // Guard::Owns on a perspective: the params beside `uuid`.
    ("perspective.queryProlog", || json!({ "query": "true." })),
    ("perspective.querySparql", || json!({ "query": "ASK {}" })),
    ("perspective.subscribeQuery", || json!({ "query": "true." })),
    (
        "perspective.subscribeSparql",
        || json!({ "query": "ASK {}" }),
    ),
    (
        "perspective.resyncSubscription",
        || json!({ "subscriptionId": "s" }),
    ),
    (
        "perspective.disposeQuery",
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
    ("perspective.discardBatch", || json!({ "batchId": "b" })),
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
    ("perspective.flowFired", || json!({ "instance": "i" })),
    // #712's SFU. `sfu.servesAllMembers` names its perspective in `uuid`; the reads without
    // params take `{}`.
    ("sfu.startRoom", sfu_room),
    ("sfu.stopRoom", sfu_room),
    ("sfu.callLeave", sfu_room),
    (
        "sfu.callJoin",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r", "sdpOffer": "{}" }),
    ),
    (
        "sfu.callSetQualityPreference",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r", "preference": "low" }),
    ),
    (
        "sfu.callAnswerServerOffer",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r", "sdpAnswer": "{}" }),
    ),
    (
        "sfu.addIceCandidate",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r", "candidate": "c" }),
    ),
    (
        "sfu.sendData",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r", "channelLabel": "c", "data": "d" }),
    ),
    ("sfu.getConfig", sfu_neighbourhood),
    ("sfu.sfuPeerForNeighbourhood", sfu_neighbourhood),
    ("sfu.sfuPeersForNeighbourhood", sfu_neighbourhood),
    (
        "sfu.setConfig",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "config": {} }),
    ),
    (
        "sfu.ensureMembership",
        || json!({ "neighbourhoodUrl": "neighbourhood://Qm", "did": "did:key:z6Mk" }),
    ),
];

fn sfu_neighbourhood() -> Value {
    json!({ "neighbourhoodUrl": "neighbourhood://Qm" })
}

fn sfu_room() -> Value {
    json!({ "neighbourhoodUrl": "neighbourhood://Qm", "roomName": "r" })
}

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

/// The params of `method` naming `target` (the id of a perspective, task, grant or request):
/// as the `uuid` param of a perspective method, and wherever its [`EXTRA_PARAMS`] put
/// [`TARGET`], inside a string too.
fn params(method: &str, target: Option<&str>, uuid_param: bool) -> Value {
    fn replace(value: &mut Value, target: &str) {
        match value {
            Value::String(s) if s.contains(TARGET) => *s = s.replace(TARGET, target),
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
        Caller::User => RequestContext::user_for_test(USER_DID, caps()),
        Caller::OtherUser => RequestContext::user_for_test(OTHER_USER_DID, caps()),
        Caller::UserAppWithout => {
            RequestContext::user_for_test(USER_DID, vec![unrelated_capability()])
        }
        // The most a caller without a token holds: multi-user mode on adds signing up,
        // logging in and verifying an email.
        Caller::Anonymous => RequestContext::for_session(
            crate::agent::session::Session::for_test(crate::agent::session::Principal::Anonymous)
                .with_caps(anonymous_capabilities_when(true)),
        ),
    }
}

/// The perspectives, AI tasks, grants and capability requests the rows name;
/// [`World::remove`] removes them.
struct World {
    users_own: String,
    main_agents: String,
    unowned: String,
    users_task: String,
    operators_task: String,
    users_grant: String,
    main_agents_grant: String,
    users_request: String,
    main_agents_request: String,
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
        let grant = |name: &str, agent: AgentContext| {
            let id = format!("{run}-{name}-grant");
            Grants::global()
                .insert(LiveGrant::for_test(&id, agent, u64::MAX))
                .expect("a grant");
            id
        };
        let request = |name: &str, agent: AgentContext| {
            let id = format!("{run}-{name}-request");
            PendingRequests::global()
                .request(
                    id.clone(),
                    AuthInfo::default(),
                    agent,
                    crate::agent::grants::now(),
                )
                .expect("a pending request");
            id
        };
        let user = || AgentContext::managed(USER_DID.to_string());
        let world = World {
            users_own: format!("{run}-users-own"),
            main_agents: format!("{run}-main-agents"),
            unowned: format!("{run}-unowned"),
            users_task: task(Some(USER_DID)),
            operators_task: task(None),
            users_grant: grant("users", user()),
            main_agents_grant: grant("main", AgentContext::main_agent()),
            users_request: request("users", user()),
            main_agents_request: request("main", AgentContext::main_agent()),
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

    /// The id of the `owner` resource a row names.
    fn id(&self, owner: Owner, target: Target) -> String {
        let missing = || format!("{}-missing", uuid::Uuid::new_v4());
        let (users, main_agents) = match owner {
            Owner::Perspective => (&self.users_own, &self.main_agents),
            Owner::Task => (&self.users_task, &self.operators_task),
            Owner::Grant => (&self.users_grant, &self.main_agents_grant),
            Owner::Request => (&self.users_request, &self.main_agents_request),
        };
        match target {
            Target::UsersOwn => users.clone(),
            Target::MainAgents => main_agents.clone(),
            Target::Unowned if owner == Owner::Perspective => self.unowned.clone(),
            Target::Unowned | Target::Missing => missing(),
        }
    }

    async fn remove(self) {
        for uuid in [&self.users_own, &self.main_agents, &self.unowned] {
            crate::perspectives::remove_perspective(uuid).await;
        }
        for task in [self.users_task, self.operators_task] {
            crate::db::Ad4mDb::with_global_instance(|db| db.remove_task(task)).unwrap();
        }
        // A test may have removed one already.
        for grant in [&self.users_grant, &self.main_agents_grant] {
            let _ = Grants::global().remove(&Grants::global().find(grant));
        }
    }
}

/// The 403/404 of a refusal.
fn outcome(result: Result<(), WsRpcError>) -> Result<(), u16> {
    result.map_err(|e| e.code)
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
        let mut cases: Vec<(Caller, Option<String>, Expect)> = resource_rows(guard)
            .into_iter()
            .map(|(caller, expect)| (caller, None, expect))
            .collect();
        let owner = match guard {
            Guard::Owns { owner, .. } => Some(*owner),
            _ => None,
        };
        if let Some(owner) = owner {
            cases.extend(
                owns_rows(owner).iter().map(|&(caller, target, expect)| {
                    (caller, Some(world.id(owner, target)), expect)
                }),
            );
        }
        for (caller, target, expect) in cases {
            let ctx = context(caller, guard.capability("*"));
            let params = params(method, target.as_deref(), owner == Some(Owner::Perspective));
            let got = outcome(map.admit(method, &params, &ctx).await);
            let want = match expect {
                Expect::Granted => Ok(()),
                Expect::Forbidden => Err(403),
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
        .guard(Guard::Cap(AGENT_AUTH_CAPABILITY.clone()));
    map.method::<NoParams, bool>("test.executor", handler())
        .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<NoParams, bool>("test.cap", handler())
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<PerspectiveUuidParams, bool>("test.perspective", handler())
        .guard(Guard::perspective(perspective_query_capability));
    map.method::<UuidAndNote, Option<bool>>("test.perspective.or_null", handler())
        .read()
        .or_null()
        .guard(Guard::Owns {
            cap: perspective_query_capability,
            id: UUID_PARAM,
            owner: Owner::Perspective,
        });
    map.method::<TaskIdParams, bool>("test.task", handler())
        .guard(Guard::Owns {
            cap: |_| AI_PROMPT_CAPABILITY.clone(),
            id: IdParam::Field(&["taskId"]),
            owner: Owner::Task,
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
    // The test methods alone: Open, Operator and Executor × 6 rows, 2 Cap guards × 7 rows, 2
    // perspective guards and 1 task guard: the walk reached them all.
    let least = 3 * 6 + 2 * 7 + 2 * perspective_rows().len() + agent_resource_rows().len();
    assert!(checked >= least, "only {checked} rows checked");
}

/// An app the user approved acts as the user's agent, but with only what it was granted: one
/// with an unrelated narrow capability cannot end the user's logins and apps.
#[tokio::test]
async fn a_users_app_without_agent_update_cannot_revoke_the_users_sessions() {
    let map = build_handler_map();
    let app = context(Caller::UserAppWithout, None);
    let refused = map
        .admit("user.revokeAllSessions", &json!({}), &app)
        .await
        .unwrap_err();
    assert_eq!(refused.code, 403, "{}", refused.message);
    let user = context(Caller::User, Some(AGENT_UPDATE_CAPABILITY.clone()));
    assert!(map
        .admit("user.revokeAllSessions", &json!({}), &user)
        .await
        .is_ok());
}

/// `Guard::Cap` checks the capability alone, so a caller without a token reaches exactly the
/// methods whose capability is in the anonymous set (and the `Open` ones): asking for a
/// capability, redeeming it, signing up, logging in, verifying an email.
#[test]
fn anonymous_callers_reach_only_the_anonymous_capabilitys_methods() {
    let map = build_handler_map();
    let anonymous: Vec<&str> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| match guard {
            Some(Guard::Cap(cap) | Guard::Executor(cap)) if anonymous_holds(cap) => Some(name),
            _ => None,
        })
        .collect();
    assert_eq!(
        anonymous,
        vec![
            "agent.generateJwt",
            "agent.requestCapability",
            "user.create",
            "user.login",
            "user.multiUserEnabled",
            "user.requestVerification",
            "user.verifyEmail",
        ]
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
            "runtime.grantNotification",
            "runtime.importData",
            "runtime.openLink",
            "runtime.restartHolochain",
            "runtime.setHostRates",
            "runtime.setUnytMembraneProof",
            "runtime.unytAgentKey",
            "runtime.unytHotAgentPubkey",
            "runtime.unytReinstallDna",
            "runtime.unytSendHot",
            "runtime.unytWalletBalance",
            "runtime.unytWalletHistory",
            "sfu.ensureMembership",
            "user.credits",
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
    // Then the capability, then the rule; the handler never runs.
    assert_eq!(
        call(
            "test.executor",
            json!({}),
            Caller::AppWithout,
            &AGENT_UPDATE_CAPABILITY
        )
        .await,
        Err(403)
    );
    assert_eq!(
        call(
            "test.executor",
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
        Err(404)
    );
    assert_eq!(
        calls.load(Ordering::SeqCst),
        0,
        "no refused call ran its handler"
    );

    // A method flagged or_null answers null without running its handler, for another user's
    // perspective and for a missing one alike. The capability still refuses with 403.
    for uuid in [
        world.users_own.clone(),
        world.id(Owner::Perspective, Target::Missing),
    ] {
        assert_eq!(
            call(
                "test.perspective.or_null",
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
            "test.perspective.or_null",
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
        "a null answer ran no handler"
    );

    // Admitted: the handler runs and its result goes back.
    assert_eq!(
        call(
            "test.perspective.or_null",
            note(&world.users_own),
            Caller::User,
            &query
        )
        .await,
        Ok(json!(true))
    );
    assert_eq!(
        call(
            "test.executor",
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

/// D6: only `perspective.get` and `perspective.snapshot` answer `null` for a perspective that
/// is missing or not the caller's; every other method that names one answers 404.
#[tokio::test]
async fn perspective_get_answers_null_and_query_links_404_for_another_users_perspective() {
    let world = World::new().await;
    let map = build_handler_map();
    assert_eq!(
        map.or_null_methods(),
        vec!["perspective.get", "perspective.snapshot"]
    );
    let query = perspective_query_capability(vec!["*".into()]);
    let other = Arc::new(context(Caller::OtherUser, Some(query)));
    let theirs = world.users_own.clone();
    for method in ["perspective.get", "perspective.snapshot"] {
        assert_eq!(
            map.dispatch(method, json!({ "uuid": theirs }), other.clone())
                .await
                .map_err(|e| e.code),
            Ok(Value::Null),
            "{method}"
        );
    }
    let err = map
        .dispatch(
            "perspective.queryLinks",
            params("perspective.queryLinks", Some(&theirs), true),
            other.clone(),
        )
        .await
        .unwrap_err();
    assert_eq!(
        (err.code, err.message),
        (404, format!("Perspective {theirs} not found")),
        "not the caller's answers as a missing one does"
    );
    world.remove().await;
}

/// D12, D14: a user approves and revokes the requests and grants of their own agent; another
/// user's and the main agent's are 404 to them. The operator approves and revokes any.
#[tokio::test]
async fn a_user_approves_and_revokes_only_their_own_agents_requests_and_grants() {
    let world = World::new().await;
    let map = build_handler_map();
    let user_caps = crate::agent::capabilities::get_user_default_capabilities();
    let alice = Arc::new(RequestContext::user_for_test(USER_DID, user_caps.clone()));
    let bob = Arc::new(RequestContext::user_for_test(OTHER_USER_DID, user_caps));
    let operator = Arc::new(RequestContext::operator_for_test());
    let bobs_grant = format!("{}-bob-grant", uuid::Uuid::new_v4());
    Grants::global()
        .insert(LiveGrant::for_test(
            &bobs_grant,
            AgentContext::managed(OTHER_USER_DID.to_string()),
            u64::MAX,
        ))
        .unwrap();
    let permit = |request: &str| params("agent.permitCapability", Some(request), false);
    let code = |answer: Result<Value, WsRpcError>| answer.map(|_| ()).map_err(|e| e.code);

    // Alice: her own request and grant; Bob's and the main agent's read as missing.
    for (method, params) in [
        ("agent.permitCapability", permit(&world.main_agents_request)),
        (
            "agent.revokeToken",
            json!({ "token": world.main_agents_grant }),
        ),
        ("agent.removeApp", json!({ "id": world.main_agents_grant })),
        ("agent.revokeToken", json!({ "token": bobs_grant })),
        ("agent.removeApp", json!({ "id": bobs_grant })),
    ] {
        assert_eq!(
            code(map.dispatch(method, params.clone(), alice.clone()).await),
            Err(404),
            "alice: {method} {params}"
        );
    }
    assert!(Grants::global()
        .get(&crate::agent::session::TokenId(bobs_grant.clone()))
        .is_some());
    assert_eq!(
        code(
            map.dispatch(
                "agent.permitCapability",
                permit(&world.users_request),
                alice.clone()
            )
            .await
        ),
        Ok(())
    );
    assert_eq!(
        code(
            map.dispatch(
                "agent.revokeToken",
                json!({ "token": world.users_grant }),
                alice.clone()
            )
            .await
        ),
        Ok(())
    );
    // Bob cannot approve Alice's request either.
    assert_eq!(
        code(
            map.dispatch(
                "agent.permitCapability",
                permit(&world.users_request),
                bob.clone()
            )
            .await
        ),
        Err(404)
    );

    // The operator approves and revokes for any agent.
    for (method, params) in [
        ("agent.permitCapability", permit(&world.main_agents_request)),
        ("agent.revokeToken", json!({ "token": bobs_grant })),
        ("agent.removeApp", json!({ "id": world.main_agents_grant })),
    ] {
        assert_eq!(
            code(map.dispatch(method, params.clone(), operator.clone()).await),
            Ok(()),
            "operator: {method} {params}"
        );
    }
    assert!(Grants::global()
        .get(&crate::agent::session::TokenId(bobs_grant.clone()))
        .is_none());
    Grants::global()
        .remove(&Grants::global().find(&bobs_grant))
        .unwrap();
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
            "user.logout",
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
        crate::agent::session::Principal::Anonymous,
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
    .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()))
    .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()));
}

#[test]
#[should_panic(expected = "not dispatched through the map")]
fn a_method_the_socket_handles_itself_takes_no_guard() {
    HandlerMap::new()
        .inline::<NoParams, bool>("test.inline")
        .guard(Guard::Cap(AGENT_AUTH_CAPABILITY.clone()));
}
