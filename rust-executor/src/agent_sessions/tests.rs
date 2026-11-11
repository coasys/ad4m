//! The session service end to end: ownership, capabilities, the running-turn rules and
//! the transcript a turn leaves.
//!
//! The model is a script ([`Scripted`]); the tools a turn sees are a stub provider unless
//! a test names the real MCP surface (`deps.provider = None`). `send` spawns its turn as
//! a detached task, so a test waits on the reply row's status, never on a handle.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use serde_json::{json, Value};

use super::run::{run_turn, TurnDeps};
use super::service::{
    self, lifecycle_entry_for_test, running_for_test, AgentSessionCreate, AgentSessionUpdateParams,
    SessionError,
};
use super::store::{agent_session_store, DbAgentSessionStore};
use super::types::{AgentSession, AgentSessionMessage, AgentSessionMessageRole, AgentTurnStatus};
use crate::agent::capabilities::{
    perspective_query_capability, perspective_update_capability, Capability, AGENT_READ_CAPABILITY,
    ALL_CAPABILITY,
};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
use crate::db::Ad4mDb;
use crate::events::Topic;
use crate::types::RequestContext;

// ── callers ─────────────────────────────────────────────────────────────────

fn ctx_for(session: Session) -> RequestContext {
    RequestContext {
        cancel_token: None,
        session: Arc::new(session),
        connection_id: None,
    }
}

/// A managed user `did` holding `caps`, as a session over the RPC socket sees them.
fn user(did: &str, caps: Vec<Capability>) -> RequestContext {
    ctx_for(
        Session::for_test(Principal::Agent(AgentContext::managed(did.to_string())))
            .with_caps(caps)
            .with_did(did),
    )
}

/// The operator's session: it acts as the main agent.
fn operator() -> RequestContext {
    ctx_for(Session::for_test(Principal::Operator))
}

fn everything() -> Vec<Capability> {
    vec![ALL_CAPABILITY.clone()]
}

// ── a scripted model ─────────────────────────────────────────────────────────

/// A model that answers `script` completions one per call, then "done"; `released` keeps a
/// completion pending until it flips, so a turn stays `running` until the test lets it
/// end — whenever it starts, no notification to miss.
struct Scripted {
    script: Mutex<Vec<HarnessCompletion>>,
    released: Arc<AtomicBool>,
    /// What each completion was asked with: the messages and the tools on offer.
    asked: Mutex<Vec<(Vec<Value>, Vec<ToolSchema>)>>,
}

impl Scripted {
    fn answering(script: Vec<HarnessCompletion>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            released: Arc::new(AtomicBool::new(true)),
            asked: Mutex::new(Vec::new()),
        })
    }

    /// A model whose completions wait on `released`: a turn stays `running` until the
    /// test lets it end.
    fn holding(released: Arc<AtomicBool>) -> Arc<Self> {
        Self::holding_with(released, vec![])
    }

    /// A model whose completions wait on `released`, then answer `script` one a call:
    /// a turn stays inside the model's answer until the test lets it land.
    fn holding_with(released: Arc<AtomicBool>, script: Vec<HarnessCompletion>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            released,
            asked: Mutex::new(Vec::new()),
        })
    }

    /// How many completions the model was asked for.
    fn times_asked(&self) -> usize {
        self.asked.lock().unwrap().len()
    }

    /// The tools the first completion was offered.
    fn offered(&self) -> Vec<ToolSchema> {
        let asked = self.asked.lock().unwrap();
        let (_, tools) = asked.first().expect("the model was asked");
        tools.clone()
    }
}

#[async_trait::async_trait]
impl CompletionSource for Scripted {
    async fn complete(
        &self,
        _model_id: &str,
        messages: &[Value],
        tools: Vec<ToolSchema>,
    ) -> anyhow::Result<HarnessCompletion> {
        self.asked.lock().unwrap().push((messages.to_vec(), tools));
        while !self.released.load(Ordering::SeqCst) {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        let mut script = self.script.lock().unwrap();
        Ok(if script.is_empty() {
            HarnessCompletion {
                content: "done".into(),
                tool_calls: vec![],
            }
        } else {
            script.remove(0)
        })
    }
}

fn calls(name: &str, arguments: Value) -> HarnessCompletion {
    HarnessCompletion {
        content: String::new(),
        tool_calls: vec![HarnessToolCall {
            id: format!("call-{name}"),
            name: name.into(),
            arguments,
        }],
    }
}

fn schema(name: &str) -> ToolSchema {
    ToolSchema {
        name: name.to_string(),
        description: format!("the {name} tool"),
        parameters: json!({ "type": "object", "properties": {} }),
        side_effect: crate::ai_service::harness::provider::SideEffect::Read,
    }
}

/// A provider that answers `ok:<name>` to any call and records them.
struct StubProvider {
    tools: Vec<ToolSchema>,
    calls: Mutex<Vec<String>>,
}

impl StubProvider {
    fn named(names: &[&str]) -> Arc<Self> {
        Arc::new(Self {
            tools: names.iter().map(|name| schema(name)).collect(),
            calls: Mutex::new(Vec::new()),
        })
    }
}

#[async_trait::async_trait]
impl ToolProvider for StubProvider {
    async fn tools(&self) -> Vec<ToolSchema> {
        self.tools.clone()
    }
    async fn call(&self, name: &str, _args: Value) -> anyhow::Result<String> {
        self.calls.lock().unwrap().push(name.to_string());
        if !self.tools.iter().any(|t| t.name == name) {
            anyhow::bail!("tool `{name}` is not in the current provider's surface");
        }
        Ok(format!("ok:{name}"))
    }
}

/// A provider whose one tool, `wait`, never returns: the turn that calls it stays inside the
/// call until something ends it.
struct Stuck;

#[async_trait::async_trait]
impl ToolProvider for Stuck {
    async fn tools(&self) -> Vec<ToolSchema> {
        vec![schema("wait")]
    }
    async fn call(&self, _name: &str, _args: Value) -> anyhow::Result<String> {
        std::future::pending().await
    }
}

fn deps(completion: Arc<dyn CompletionSource>) -> TurnDeps {
    TurnDeps {
        completion: Some(completion),
        provider: Some(StubProvider::named(&[])),
        ..Default::default()
    }
}

// ── fixtures ─────────────────────────────────────────────────────────────────

fn setup() {
    crate::mcp::tools::test_support::setup();
    service::clear_running_for_test();
    super::store::set_agent_session_store(Some(Arc::new(DbAgentSessionStore)));
}

fn create(ctx: &RequestContext) -> AgentSession {
    service::create(
        ctx,
        AgentSessionCreate {
            title: Some("test".into()),
            model: Some("scripted".into()),
            ..Default::default()
        },
    )
    .expect("a session")
}

/// The first tool step of the session, once the turn has written it, within 10 s.
async fn first_step(owner: &str, session_id: &str) -> AgentSessionMessage {
    let store = agent_session_store();
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        let rows = store.messages(Some(owner), session_id, 100, None).unwrap();
        if let Some(step) = rows
            .into_iter()
            .find(|m| m.role == AgentSessionMessageRole::Tool)
        {
            return step;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the turn wrote no tool step within 10 s"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}

/// One turn of `session` sent by `caller`, the model answering from `model`: the tool steps
/// the turn wrote, in order.
async fn steps_of(
    session: &AgentSession,
    caller: &RequestContext,
    model: Arc<Scripted>,
    provider: Option<Arc<dyn ToolProvider>>,
) -> Vec<AgentSessionMessage> {
    let store = agent_session_store();
    let agent = caller.acts_as();
    let before = store
        .messages(agent.managed_did(), &session.id, 500, None)
        .unwrap()
        .len();
    run_turn(
        session.clone(),
        caller.session.clone(),
        agent.clone(),
        store.clone(),
        TurnDeps {
            completion: Some(model),
            provider,
            ..Default::default()
        },
    )
    .await
    .expect("the turn ran");
    store
        .messages(agent.managed_did(), &session.id, 500, None)
        .unwrap()
        .into_iter()
        .skip(before)
        .filter(|m| m.role == AgentSessionMessageRole::Tool)
        .collect()
}

/// The row `id` once it is not `running` any more, within 10 s.
async fn settled(owner: Option<&str>, reply_id: &str) -> AgentSessionMessage {
    let store = agent_session_store();
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        if let Ok(Some(row)) = store.message(owner, reply_id) {
            if row.status != Some(AgentTurnStatus::Running) {
                return row;
            }
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the turn did not settle within 10 s"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}

// ── ownership ────────────────────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_session_is_its_owners_alone() {
    setup();
    let alice = user("did:key:alice-a1", everything());
    let bob = user("did:key:bob-b1", everything());
    let session = create(&alice);

    // Bob can name Alice's session id and still gets "not found" everywhere.
    assert!(matches!(
        service::get(&bob, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(service::list(&bob).unwrap().is_empty());
    assert!(service::list(&alice).unwrap().len() == 1);
    assert!(matches!(
        service::messages(&bob, &session.id, None, None),
        Err(SessionError::NotFound(_))
    ));
    assert!(matches!(
        service::update(&bob, &session.id, AgentSessionUpdateParams::default()),
        Err(SessionError::NotFound(_))
    ));
    assert!(matches!(
        service::cancel(&bob, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(matches!(
        service::send(&bob, &session.id, "hi", deps(Scripted::answering(vec![]))).await,
        Err(SessionError::NotFound(_))
    ));
    assert!(matches!(
        service::delete(&bob, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(service::get(&alice, &session.id).is_ok());
}

#[tokio::test(flavor = "multi_thread")]
async fn the_main_agent_and_a_user_never_reach_each_others_sessions() {
    setup();
    let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
    let operator = operator();
    let user_a = user("did:key:alice-a2", everything());
    let main_session = create(&operator);
    let user_session = create(&user_a);

    // The operator acts as the main agent: the user's session is not theirs.
    assert!(matches!(
        service::get(&operator, &user_session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(matches!(
        service::get(&user_a, &main_session.id),
        Err(SessionError::NotFound(_))
    ));
    let listed = |caller: &RequestContext| -> Vec<String> {
        let sessions = service::list(caller).unwrap();
        sessions.into_iter().map(|session| session.id).collect()
    };
    assert!(listed(&operator).contains(&main_session.id));
    assert!(!listed(&operator).contains(&user_session.id));
    assert_eq!(listed(&user_a), vec![user_session.id.clone()]);

    // Nor does any other call: each names the other's session and finds none.
    for (caller, theirs) in [(&operator, &user_session), (&user_a, &main_session)] {
        assert!(matches!(
            service::messages(caller, &theirs.id, None, None),
            Err(SessionError::NotFound(_))
        ));
        let rename = AgentSessionUpdateParams {
            title: Some("taken".into()),
            ..Default::default()
        };
        assert!(matches!(
            service::update(caller, &theirs.id, rename),
            Err(SessionError::NotFound(_))
        ));
        assert!(matches!(
            service::cancel(caller, &theirs.id),
            Err(SessionError::NotFound(_))
        ));
        assert!(matches!(
            service::send(caller, &theirs.id, "hi", deps(Scripted::answering(vec![]))).await,
            Err(SessionError::NotFound(_))
        ));
        assert!(matches!(
            service::delete(caller, &theirs.id),
            Err(SessionError::NotFound(_))
        ));
    }
    for (owner, own) in [(&operator, &main_session), (&user_a, &user_session)] {
        assert_eq!(service::get(owner, &own.id).unwrap().title, "test");
        assert!(service::messages(owner, &own.id, None, None)
            .unwrap()
            .is_empty());
    }

    // And each one's events go to its own sessions only.
    let mut heard = 0;
    while let Ok(Ok(event)) = tokio::time::timeout(Duration::from_millis(200), events.recv()).await
    {
        let of_main = event.payload["sessionId"] == main_session.id.as_str();
        let of_user = event.payload["sessionId"] == user_session.id.as_str();
        if !(of_main || of_user) {
            continue;
        }
        heard += 1;
        assert_eq!(operator.session.may_receive(&event), of_main);
        assert_eq!(user_a.session.may_receive(&event), of_user);
    }
    assert_eq!(heard, 2, "one `created` event each");
}

// ── capabilities ─────────────────────────────────────────────────────────────

#[test]
fn a_create_narrows_the_capabilities_it_was_given_to_what_the_caller_holds() {
    setup();
    // The caller holds read on perspective A and nothing else.
    let caller = user(
        "did:key:alice-a3",
        vec![perspective_query_capability(vec!["pa".into()])],
    );
    let session = service::create(
        &caller,
        AgentSessionCreate {
            model: Some("m".into()),
            capabilities: Some(vec![
                perspective_query_capability(vec!["pa".into()]),
                perspective_update_capability(vec!["pa".into()]),
                perspective_query_capability(vec!["pb".into()]),
            ]),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(
        session.capabilities,
        vec![perspective_query_capability(vec!["pa".into()])],
        "only the read on A the caller holds stays"
    );

    // Absent: everything the caller holds.
    let session = service::create(
        &caller,
        AgentSessionCreate {
            model: Some("m".into()),
            capabilities: None,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(
        session.capabilities,
        vec![perspective_query_capability(vec!["pa".into()])]
    );
}

#[test]
fn an_update_cannot_widen_a_session_past_the_caller() {
    setup();
    let caller = user(
        "did:key:alice-a4",
        vec![perspective_query_capability(vec!["pa".into()])],
    );
    let session = create(&caller);

    // Asking for an update capability the caller does not hold narrows to nothing.
    let session = service::update(
        &caller,
        &session.id,
        AgentSessionUpdateParams {
            capabilities: Some(vec![perspective_update_capability(vec!["pa".into()])]),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(session.capabilities.is_empty());
}

// ── the running-turn rules ──────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_send_runs_detached_and_the_reply_lands_on_the_row() {
    setup();
    let alice = user("did:key:alice-a5", everything());
    let session = create(&alice);

    let started = service::send(
        &alice,
        &session.id,
        "what can you see?",
        deps(Scripted::answering(vec![])),
    )
    .await
    .unwrap();
    let reply = settled(Some("did:key:alice-a5"), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Done));
    assert_eq!(reply.content["text"], "done");

    let messages = service::messages(&alice, &session.id, None, None).unwrap();
    assert_eq!(messages.len(), 2);
    assert_eq!(messages[0].role, AgentSessionMessageRole::User);
    assert_eq!(messages[0].content["text"], "what can you see?");
    assert!(!running_for_test(&session.id));
}

#[tokio::test(flavor = "multi_thread")]
async fn one_turn_per_session_and_eight_per_caller() {
    setup();
    let alice = user("did:key:alice-a6", everything());
    let hold = Arc::new(AtomicBool::new(false));

    // A turn that waits: the session is busy until it is released.
    let session = create(&alice);
    service::send(
        &alice,
        &session.id,
        "one",
        deps(Scripted::holding(hold.clone())),
    )
    .await
    .unwrap();
    assert!(matches!(
        service::send(
            &alice,
            &session.id,
            "two",
            deps(Scripted::answering(vec![]))
        )
        .await,
        Err(SessionError::Busy)
    ));

    // Eight running turns per caller at most — on any of their sessions.
    let mut replies = Vec::new();
    for _ in 0..service::MAX_RUNNING_PER_CALLER - 1 {
        let extra = create(&alice);
        let started = service::send(
            &alice,
            &extra.id,
            "go",
            deps(Scripted::holding(hold.clone())),
        )
        .await
        .unwrap();
        replies.push((extra.id, started.reply_id));
    }
    let ninth = create(&alice);
    assert!(matches!(
        service::send(&alice, &ninth.id, "go", deps(Scripted::answering(vec![]))).await,
        Err(SessionError::TooManyTurns)
    ));

    // Another caller's limit is their own.
    let bob = user("did:key:bob-a6", everything());
    let his = create(&bob);
    let his_started = service::send(&bob, &his.id, "go", deps(Scripted::holding(hold.clone())))
        .await
        .unwrap();
    let his_session = his.id.clone();
    replies.push((his.id, his_started.reply_id));

    hold.store(true, Ordering::SeqCst);
    // The held tasks end `done`; the registry empties.
    for (session_id, reply_id) in replies {
        let owner = if session_id == his_session {
            Some("did:key:bob-a6")
        } else {
            Some("did:key:alice-a6")
        };
        let row = settled(owner, &reply_id).await;
        assert_eq!(row.status, Some(AgentTurnStatus::Done));
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn cancel_ends_the_running_turn() {
    setup();
    let alice = user("did:key:alice-a7", everything());
    let session = create(&alice);
    let hold = Arc::new(AtomicBool::new(false));

    let started = service::send(
        &alice,
        &session.id,
        "go",
        deps(Scripted::holding(hold.clone())),
    )
    .await
    .unwrap();
    assert!(service::cancel(&alice, &session.id).unwrap());
    let reply = settled(Some("did:key:alice-a7"), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Cancelled));
    // The model answering afterwards changes nothing: the row stays as the cancel left it.
    hold.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(200)).await;
    let reply = agent_session_store()
        .message(Some("did:key:alice-a7"), &started.reply_id)
        .unwrap()
        .unwrap();
    assert_eq!(reply.status, Some(AgentTurnStatus::Cancelled));

    // Nothing running now: cancel says so, and a new send is free to go.
    assert!(!service::cancel(&alice, &session.id).unwrap());
    assert!(service::send(
        &alice,
        &session.id,
        "again",
        deps(Scripted::answering(vec![]))
    )
    .await
    .is_ok());
}

#[tokio::test(flavor = "multi_thread")]
async fn delete_ends_a_running_turn_and_takes_the_transcript() {
    setup();
    let alice = user("did:key:alice-a8", everything());
    let session = create(&alice);
    let hold = Arc::new(AtomicBool::new(false));
    service::send(
        &alice,
        &session.id,
        "go",
        deps(Scripted::holding(hold.clone())),
    )
    .await
    .unwrap();

    assert!(service::delete(&alice, &session.id).unwrap());
    assert!(matches!(
        service::get(&alice, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(service::messages(&alice, &session.id, None, None).is_err());
    assert!(!running_for_test(&session.id));
    hold.store(true, Ordering::SeqCst);
}

#[tokio::test(flavor = "multi_thread")]
async fn only_its_owner_ends_a_sessions_running_turn() {
    setup();
    let did = "did:key:alice-a10";
    let alice = user(did, everything());
    let session = create(&alice);
    let hold = Arc::new(AtomicBool::new(false));
    let started = service::send(
        &alice,
        &session.id,
        "go",
        deps(Scripted::holding(hold.clone())),
    )
    .await
    .unwrap();

    // Another user and the operator name the session: neither finds it, and the turn
    // runs on.
    for other in [user("did:key:bob-a10", everything()), operator()] {
        assert!(matches!(
            service::delete(&other, &session.id),
            Err(SessionError::NotFound(_))
        ));
        assert!(matches!(
            service::cancel(&other, &session.id),
            Err(SessionError::NotFound(_))
        ));
        // A send answers "not found" too, not "busy": the answer does not say a turn is
        // running there.
        assert!(matches!(
            service::send(&other, &session.id, "hi", deps(Scripted::answering(vec![]))).await,
            Err(SessionError::NotFound(_))
        ));
        assert!(running_for_test(&session.id));
    }
    // The operator passes the `owns` guard of any id, so over the socket its call reaches
    // the handler. The answer is the same.
    let methods = crate::api::ws_handler::build_handler_map();
    for (method, params) in [
        ("agent.sessionDelete", json!({ "id": session.id })),
        ("agent.cancel", json!({ "sessionId": session.id })),
    ] {
        let refused = methods
            .dispatch(method, params, Arc::new(operator()))
            .await
            .unwrap_err();
        assert_eq!(refused.code, 404, "{method}: {}", refused.message);
        assert!(running_for_test(&session.id), "{method}");
    }

    hold.store(true, Ordering::SeqCst);
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Done));
    assert_eq!(reply.content["text"], "done");
    assert!(service::get(&alice, &session.id).is_ok());
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_who_may_only_read_changes_no_session_and_starts_no_turn() {
    setup();
    let did = "did:key:alice-a15";
    let alice = user(did, everything());
    let session = create(&alice);
    // One of Alice's apps, holding `agent:READ` and nothing else.
    let reader = || Arc::new(user(did, vec![AGENT_READ_CAPABILITY.clone()]));
    let methods = crate::api::ws_handler::build_handler_map();

    for (method, params) in [
        ("agent.sessionList", json!({})),
        ("agent.session", json!({ "id": session.id })),
        ("agent.messages", json!({ "sessionId": session.id })),
    ] {
        let answer = methods.dispatch(method, params, reader()).await;
        assert!(answer.is_ok(), "{method}: {:?}", answer.err());
    }
    for (method, params) in [
        ("agent.sessionCreate", json!({})),
        (
            "agent.sessionUpdate",
            json!({ "id": session.id, "title": "theirs" }),
        ),
        (
            "agent.send",
            json!({ "sessionId": session.id, "text": "hi" }),
        ),
        ("agent.cancel", json!({ "sessionId": session.id })),
        ("agent.sessionDelete", json!({ "id": session.id })),
    ] {
        let refused = methods
            .dispatch(method, params, reader())
            .await
            .expect_err(method);
        assert_eq!(refused.code, 403, "{method}: {}", refused.message);
    }

    // The session is as Alice made it, with nothing said on it.
    let kept = service::get(&alice, &session.id).unwrap();
    assert_eq!(kept.title, "test");
    assert_eq!(service::list(&alice).unwrap().len(), 1);
    assert!(service::messages(&alice, &session.id, None, None)
        .unwrap()
        .is_empty());
    assert!(!running_for_test(&session.id));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_cancelled_turn_leaves_no_step_running() {
    setup();
    let did = "did:key:alice-a11";
    let alice = user(did, everything());
    let session = create(&alice);

    // The turn is inside a tool call that never returns.
    let started = service::send(
        &alice,
        &session.id,
        "go",
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls("wait", json!({}))])),
            provider: Some(Arc::new(Stuck)),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    let step = first_step(did, &session.id).await;
    assert_eq!(step.status, Some(AgentTurnStatus::Running));

    assert!(service::cancel(&alice, &session.id).unwrap());
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Cancelled));
    // The step ends with its turn: nothing will return for it.
    let step = settled(Some(did), &step.id).await;
    assert_eq!(step.status, Some(AgentTurnStatus::Interrupted));
    assert!(step.error.is_some());
}

#[tokio::test(flavor = "multi_thread")]
async fn a_deleted_sessions_turn_calls_no_tool_afterwards() {
    setup();
    let alice = user("did:key:alice-a13", everything());
    let session = create(&alice);

    // The turn waits on a model that is about to ask for a tool.
    let hold = Arc::new(AtomicBool::new(false));
    let model = Arc::new(Scripted {
        script: Mutex::new(vec![calls("note", json!({}))]),
        released: hold.clone(),
        asked: Mutex::new(Vec::new()),
    });
    let provider = StubProvider::named(&["note"]);
    service::send(
        &alice,
        &session.id,
        "go",
        TurnDeps {
            completion: Some(model.clone()),
            provider: Some(provider.clone()),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    while model.times_asked() == 0 {
        tokio::time::sleep(Duration::from_millis(10)).await;
    }

    assert!(service::delete(&alice, &session.id).unwrap());
    // The model answers after the delete: the turn is gone, so nothing runs its tool call
    // and nothing asks the model again.
    tokio::time::sleep(Duration::from_millis(50)).await;
    hold.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(300)).await;
    assert!(provider.calls.lock().unwrap().is_empty());
    assert_eq!(model.times_asked(), 1);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_send_started_while_a_delete_is_midway_leaves_no_turn_running() {
    setup();
    let did = "did:key:alice-a16";
    let alice = user(did, everything());
    let session = create(&alice);

    // The gap a send once slipped a whole turn start into: the delete has cleared the
    // running-turn slot and not yet taken the rows.
    let (delete_reached, let_delete_run) = service::pause_delete_before_commit();
    let deleting = {
        let caller = user(did, everything());
        let session_id = session.id.clone();
        tokio::task::spawn_blocking(move || service::delete(&caller, &session_id))
    };
    while !delete_reached.load(Ordering::SeqCst) {
        tokio::time::sleep(Duration::from_millis(5)).await;
    }

    // A send on the session while the delete is parked: at the gap the row is still
    // there to find, and its turn would ask the held model for a write call.
    let released = Arc::new(AtomicBool::new(false));
    let model = Scripted::holding_with(
        released.clone(),
        vec![calls("add_link", json!({"perspective_id": "px"}))],
    );
    let provider = StubProvider::named(&["add_link"]);
    let sending = {
        let model = model.clone();
        let provider = provider.clone();
        let session_id = session.id.clone();
        tokio::spawn(async move {
            service::send(
                &alice,
                &session_id,
                "go",
                TurnDeps {
                    completion: Some(model),
                    provider: Some(provider),
                    ..Default::default()
                },
            )
            .await
        })
    };

    // An admitted send's task asks the model — that is what the delete must not miss.
    // A refused send never asks: give it time to say so, then let the delete run.
    let deadline = std::time::Instant::now() + Duration::from_secs(5);
    while model.times_asked() == 0 && std::time::Instant::now() < deadline {
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    let_delete_run.store(true, Ordering::SeqCst);

    assert!(deleting.await.unwrap().unwrap());
    let outcome = sending.await.unwrap();
    assert!(
        matches!(outcome, Err(SessionError::NotFound(_))),
        "a send on a deleted session answers not-found: {outcome:?}"
    );
    assert!(
        !running_for_test(&session.id),
        "no turn may be running on a session that is gone"
    );
    // Whatever the send's model answers afterwards, none of it runs.
    released.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(200)).await;
    assert!(provider.calls.lock().unwrap().is_empty());
}

#[tokio::test(flavor = "multi_thread")]
async fn another_owners_delete_leaves_the_sessions_lifecycle_lock_in_place() {
    setup();
    let did = "did:key:alice-a17";
    let alice = user(did, everything());
    let session = create(&alice);
    // A send leaves the session's lifecycle lock in the table: the session lives on.
    let started = service::send(&alice, &session.id, "hi", deps(Scripted::answering(vec![])))
        .await
        .unwrap();
    settled(Some(did), &started.reply_id).await;
    assert!(lifecycle_entry_for_test(Some(did), &session.id));

    // Bob's delete of an id that is not his answers not-found. It must not retire
    // Alice's lock: a send of hers would take a fresh one while her own delete still
    // holds the old, and the two would no longer exclude each other.
    let bob = user("did:key:bob-a17", everything());
    assert!(matches!(
        service::delete(&bob, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(
        lifecycle_entry_for_test(Some(did), &session.id),
        "another owner's not-found retires no lock of this owner's session"
    );
}

/// A model that panics when asked: the turn's task dies without settling anything.
struct Panicking;

#[async_trait::async_trait]
impl CompletionSource for Panicking {
    async fn complete(
        &self,
        _model_id: &str,
        _messages: &[Value],
        _tools: Vec<ToolSchema>,
    ) -> anyhow::Result<HarnessCompletion> {
        panic!("the model fell over")
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_whose_task_died_frees_its_session_and_the_next_send_ends_its_rows() {
    setup();
    let did = "did:key:alice-a14";
    let alice = user(did, everything());
    let session = create(&alice);

    let died = service::send(&alice, &session.id, "go", deps(Arc::new(Panicking)))
        .await
        .unwrap();
    // The task is gone and so is its hold on the session; its reply row was never settled.
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while running_for_test(&session.id) {
        assert!(
            std::time::Instant::now() < deadline,
            "the dead task still holds the session after 10 s"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    let store = agent_session_store();
    let left = store.message(Some(did), &died.reply_id).unwrap().unwrap();
    assert_eq!(left.status, Some(AgentTurnStatus::Running));

    // The next send ends what the dead task left running, and runs its own turn.
    let next = service::send(
        &alice,
        &session.id,
        "again",
        deps(Scripted::answering(vec![])),
    )
    .await
    .unwrap();
    let left = store.message(Some(did), &died.reply_id).unwrap().unwrap();
    assert_eq!(left.status, Some(AgentTurnStatus::Interrupted));
    let reply = settled(Some(did), &next.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Done));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_that_runs_out_of_time_ends_failed_and_frees_its_session() {
    setup();
    let did = "did:key:alice-a12";
    let alice = user(did, everything());
    let session = create(&alice);

    // The turn is inside a tool call that never returns, with 200 ms to live.
    let started = service::send(
        &alice,
        &session.id,
        "go",
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls("wait", json!({}))])),
            provider: Some(Arc::new(Stuck)),
            timeout: Some(Duration::from_millis(200)),
        },
    )
    .await
    .unwrap();

    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Failed));
    assert_eq!(reply.error.as_deref(), Some("the turn ran out of time"));
    // The call it was inside ended with it, and the session takes the next message.
    let step = first_step(did, &session.id).await;
    assert_eq!(step.status, Some(AgentTurnStatus::Interrupted));
    assert!(!running_for_test(&session.id));
    let next = service::send(
        &alice,
        &session.id,
        "again",
        deps(Scripted::answering(vec![])),
    )
    .await
    .unwrap();
    let reply = settled(Some(did), &next.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Done));
}

#[test]
fn the_startup_sweep_marks_every_running_row_interrupted() {
    setup();
    let alice = "did:key:alice-a9";
    let session = {
        let ctx = user(alice, everything());
        create(&ctx)
    };
    let store = agent_session_store();
    store
        .append_message(
            Some(alice),
            "a-turn",
            &session.id,
            AgentSessionMessageRole::Assistant,
            &json!({"text": ""}),
            Some(AgentTurnStatus::Running),
        )
        .unwrap();
    store
        .append_message(
            Some(alice),
            "a-step",
            &session.id,
            AgentSessionMessageRole::Tool,
            &json!({"name": "t", "arguments": {}}),
            Some(AgentTurnStatus::Running),
        )
        .unwrap();

    service::startup_sweep();

    for id in ["a-turn", "a-step"] {
        let row = store.message(Some(alice), id).unwrap().unwrap();
        assert_eq!(row.status, Some(AgentTurnStatus::Interrupted), "{id}");
        assert!(row.error.is_some());
    }
    // A second sweep finds nothing: `interrupted` is a settled state.
    service::startup_sweep();
    let row = store.message(Some(alice), "a-turn").unwrap().unwrap();
    assert_eq!(row.status, Some(AgentTurnStatus::Interrupted));
}

// ── the transcript a turn leaves ─────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_tool_call_becomes_a_step_row_the_history_reads_back() {
    setup();
    let alice = AgentContext::managed("did:key:alice-b1".to_string());
    let ctx = user("did:key:alice-b1", everything());
    let session = create(&ctx);
    let store = agent_session_store();

    // A turn of this session by hand: the model calls a tool, the observer writes it.
    let caller = ctx.session.clone();
    let provider = StubProvider::named(&["describe_perspective"]);
    let outcome = run_turn(
        session.clone(),
        caller,
        alice,
        store.clone(),
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "describe_perspective",
                json!({"perspective_id": "px"}),
            )])),
            provider: Some(provider.clone()),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    assert_eq!(outcome, "done");

    // The step: name, arguments, target, status and result on one `tool` row.
    let rows = store
        .messages(Some("did:key:alice-b1"), &session.id, 100, None)
        .unwrap();
    let step = rows
        .iter()
        .find(|m| m.role == AgentSessionMessageRole::Tool)
        .expect("a tool step row");
    assert_eq!(step.content["name"], "describe_perspective");
    assert_eq!(step.content["target"], "px");
    assert_eq!(step.content["result"], "ok:describe_perspective");
    assert_eq!(step.status, Some(AgentTurnStatus::Done));

    // And the model sees it next turn: the history rebuilds the call and its result.
    let history = super::run::history(rows);
    let tool_call = history
        .iter()
        .find(|m| m.get("tool_calls").is_some())
        .expect("an assistant tool_calls entry");
    assert_eq!(
        tool_call["tool_calls"][0]["function"]["name"],
        "describe_perspective"
    );
    let result = history
        .iter()
        .find(|m| m["role"] == "tool")
        .expect("a tool result entry");
    assert_eq!(result["content"], "ok:describe_perspective");
}

#[tokio::test(flavor = "multi_thread")]
async fn the_next_turn_reads_the_last_one_in_the_order_it_happened() {
    setup();
    let did = "did:key:alice-b4";
    let alice = user(did, everything());
    let session = create(&alice);

    let first = service::send(
        &alice,
        &session.id,
        "look",
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "describe_perspective",
                json!({"perspective_id": "px"}),
            )])),
            provider: Some(StubProvider::named(&["describe_perspective"])),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    settled(Some(did), &first.reply_id).await;
    // The rows as `send` and the observer wrote them: the answer's row before the step.
    let rows = service::messages(&alice, &session.id, None, None).unwrap();
    let roles: Vec<_> = rows.iter().map(|m| m.role).collect();
    assert_eq!(
        roles,
        [
            AgentSessionMessageRole::User,
            AgentSessionMessageRole::Assistant,
            AgentSessionMessageRole::Tool,
        ]
    );

    let model = Scripted::answering(vec![]);
    let second = service::send(&alice, &session.id, "and now?", deps(model.clone()))
        .await
        .unwrap();
    settled(Some(did), &second.reply_id).await;

    // What the model was asked: the call and its result come before the answer they led
    // to, and the new message is last.
    let asked = model.asked.lock().unwrap();
    let (messages, _) = &asked[0];
    let roles: Vec<_> = messages
        .iter()
        .map(|m| m["role"].as_str().unwrap_or_default())
        .collect();
    assert_eq!(
        roles,
        ["system", "user", "assistant", "tool", "assistant", "user"],
        "{messages:#?}"
    );
    assert_eq!(messages[1]["content"], "look");
    assert_eq!(
        messages[2]["tool_calls"][0]["function"]["name"],
        "describe_perspective"
    );
    assert_eq!(
        messages[3]["tool_call_id"],
        messages[2]["tool_calls"][0]["id"]
    );
    assert_eq!(messages[3]["content"], "ok:describe_perspective");
    assert_eq!(messages[4]["content"], "done");
    assert_eq!(messages[5]["content"], "and now?");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_failing_tool_call_ends_its_step_failed() {
    setup();
    let alice = AgentContext::managed("did:key:alice-b2".to_string());
    let ctx = user("did:key:alice-b2", everything());
    let session = create(&ctx);
    let store = agent_session_store();

    // A provider that knows no tool the model names: the dispatch refuses.
    run_turn(
        session.clone(),
        ctx.session.clone(),
        alice,
        store.clone(),
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "add_link",
                json!({"perspective_id": "px"}),
            )])),
            provider: Some(StubProvider::named(&[])),
            ..Default::default()
        },
    )
    .await
    .unwrap();

    let rows = store
        .messages(Some("did:key:alice-b2"), &session.id, 100, None)
        .unwrap();
    let step = rows
        .iter()
        .find(|m| m.role == AgentSessionMessageRole::Tool)
        .expect("a tool step row");
    assert_eq!(step.status, Some(AgentTurnStatus::Failed));
    assert!(step.content.get("error").is_some(), "{:?}", step.content);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_whose_caller_session_ended_is_refused() {
    setup();
    let alice = AgentContext::managed("did:key:alice-b3".to_string());
    // A session whose grant is already past its end: `check` refuses.
    let dead = Session::for_test(Principal::Agent(alice.clone()))
        .with_caps(everything())
        .expiring_at(1);
    let ctx = RequestContext {
        cancel_token: None,
        session: Arc::new(dead),
        connection_id: None,
    };
    let session = create(&ctx);
    // Give the turn its first running row so the gate is what the turn hits.
    let store = agent_session_store();
    store
        .append_message(
            Some("did:key:alice-b3"),
            "u1",
            &session.id,
            AgentSessionMessageRole::User,
            &json!({"text": "go"}),
            None,
        )
        .unwrap();
    store
        .append_message(
            Some("did:key:alice-b3"),
            "r1",
            &session.id,
            AgentSessionMessageRole::Assistant,
            &json!({"text": ""}),
            Some(AgentTurnStatus::Running),
        )
        .unwrap();

    let outcome = run_turn(
        session,
        ctx.session.clone(),
        alice,
        store,
        deps(Scripted::answering(vec![])),
    )
    .await;
    assert!(outcome.is_err(), "a dead caller refuses the turn");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_completion_landing_after_the_caller_was_revoked_runs_no_tool() {
    setup();
    let did = "did:key:alice-b6";
    let alice = user(did, everything());
    let session = create(&alice);

    // The model has been asked and is holding its answer: the turn is inside a
    // completion when the caller's grant ends (a revoke or a logout is the same
    // `ended` cancellation).
    let released = Arc::new(AtomicBool::new(false));
    let model = Scripted::holding_with(
        released.clone(),
        vec![calls("add_link", json!({"perspective_id": "px"}))],
    );
    let provider = StubProvider::named(&["add_link"]);
    let started = service::send(
        &alice,
        &session.id,
        "go",
        TurnDeps {
            completion: Some(model.clone()),
            provider: Some(provider.clone()),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    while model.times_asked() == 0 {
        tokio::time::sleep(Duration::from_millis(10)).await;
    }

    alice.session.grant().unwrap().ended.cancel();
    released.store(true, Ordering::SeqCst);

    // No call of that completion may run: the turn ends refused, the way a failed
    // gate check ends it, and the write tool it asked for stays uncalled.
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Failed));
    assert!(
        provider.calls.lock().unwrap().is_empty(),
        "a call of a dead caller's completion must not run"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_whose_grant_still_holds_runs_its_tool_calls() {
    setup();
    let did = "did:key:alice-b7";
    let alice = user(did, everything());
    let session = create(&alice);
    let provider = StubProvider::named(&["add_link"]);
    let started = service::send(
        &alice,
        &session.id,
        "go",
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "add_link",
                json!({"perspective_id": "px"}),
            )])),
            provider: Some(provider.clone()),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(AgentTurnStatus::Done));
    assert_eq!(
        *provider.calls.lock().unwrap(),
        vec!["add_link".to_string()]
    );
}

// ── the events ───────────────────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_session_change_reaches_its_owners_event_stream() {
    setup();
    let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
    let alice = user("did:key:alice-c1", everything());
    let session = create(&alice);

    let event = tokio::time::timeout(Duration::from_secs(5), events.recv())
        .await
        .expect("an event")
        .expect("the stream is open");
    assert_eq!(event.topic, Topic::AgentSessionChanged);
    assert_eq!(
        event.audience,
        crate::events::Audience::Dids(vec!["did:key:alice-c1".to_string()]),
        "only the owner's sessions hear it"
    );
    assert_eq!(event.payload["kind"], "created");
    assert_eq!(event.payload["session"]["id"], session.id);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_sessions_events_reach_its_owners_agent_readers_and_no_one_else() {
    setup();
    let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
    let did = "did:key:alice-c2";
    let alice = user(did, everything());

    // A session's whole life: created, a turn with a tool step, renamed, deleted.
    let session = create(&alice);
    let started = service::send(
        &alice,
        &session.id,
        "look",
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "describe_perspective",
                json!({"perspective_id": "px"}),
            )])),
            provider: Some(StubProvider::named(&["describe_perspective"])),
            ..Default::default()
        },
    )
    .await
    .unwrap();
    settled(Some(did), &started.reply_id).await;
    let rename = AgentSessionUpdateParams {
        title: Some("renamed".into()),
        ..Default::default()
    };
    service::update(&alice, &session.id, rename).unwrap();
    assert!(service::delete(&alice, &session.id).unwrap());

    // Who is listening: the owner's apps with and without `agent:READ`, another user,
    // the operator (the main agent) and a session nobody logged in on.
    let reader = user(did, vec![AGENT_READ_CAPABILITY.clone()]);
    let no_agent_read = user(did, vec![perspective_query_capability(vec!["px".into()])]);
    let bob = user("did:key:bob-c2", everything());
    let operator = operator();
    let anonymous = ctx_for(Session::for_test(Principal::Anonymous));
    let listeners = [
        ("the owner", &alice, true),
        ("the owner's app holding agent:READ", &reader, true),
        ("the owner's app without agent:READ", &no_agent_read, false),
        ("another user", &bob, false),
        ("the operator", &operator, false),
        ("an anonymous session", &anonymous, false),
    ];

    let mut kinds: Vec<String> = Vec::new();
    while let Ok(Ok(event)) = tokio::time::timeout(Duration::from_millis(300), events.recv()).await
    {
        if event.payload["sessionId"] != session.id.as_str() {
            continue;
        }
        let kind = event.payload["kind"].as_str().unwrap_or_default();
        assert_eq!(event.topic, Topic::AgentSessionChanged);
        for (who, listener, hears) in &listeners {
            assert_eq!(
                listener.session.may_receive(&event),
                *hears,
                "{who} and a `{kind}` event"
            );
        }
        kinds.push(kind.to_string());
    }
    let count = |kind: &str| kinds.iter().filter(|k| *k == kind).count();
    assert_eq!(count("created"), 1, "{kinds:?}");
    assert_eq!(count("updated"), 1, "{kinds:?}");
    assert_eq!(count("deleted"), 1, "{kinds:?}");
    // The person's message, the reply running and done, the step running and done.
    assert_eq!(count("message"), 5, "{kinds:?}");
}

// ── credits ──────────────────────────────────────────────────────────────────

/// Free hosting off while it lives: a managed user pays for every model call, as on a
/// hosted node.
struct PaidHosting {
    was_free: bool,
}

impl PaidHosting {
    fn on() -> Self {
        let was_free = Ad4mDb::with_global_instance(|db| {
            let was_free = db.get_free_hosting_enabled()?;
            db.set_free_hosting_enabled(false)?;
            Ok::<_, anyhow::Error>(was_free)
        })
        .unwrap();
        Self { was_free }
    }
}

impl Drop for PaidHosting {
    fn drop(&mut self) {
        let _ = crate::db_backend::db_backend().set_free_hosting_enabled(self.was_free);
    }
}

/// A managed user with `credits` to spend. Returns their DID.
fn account(credits: f64) -> String {
    let id = uuid::Uuid::new_v4();
    let email = format!("user-{id}@example.org");
    let did = format!("did:key:user-{id}");
    Ad4mDb::with_global_instance(|db| {
        db.add_user(&email, &did, "pw")?;
        db.set_user_credits(&email, credits)?;
        Ok::<_, anyhow::Error>(())
    })
    .unwrap();
    did
}

#[tokio::test(flavor = "multi_thread")]
async fn a_user_without_credits_starts_no_turn() {
    setup();
    let _paid = PaidHosting::on();
    let broke = user(&account(0.0), everything());
    let funded = user(&account(5.0), everything());

    // Over the socket `agent.send` refuses before a row is written or a slot taken.
    let session = create(&broke);
    let refused = crate::api::ws_handler::build_handler_map()
        .dispatch(
            "agent.send",
            json!({ "sessionId": session.id, "text": "hi" }),
            Arc::new(user(broke.user_did().unwrap(), everything())),
        )
        .await
        .unwrap_err();
    assert_eq!(refused.code, 403, "{}", refused.message);
    assert!(refused.message.contains("credits"), "{}", refused.message);
    assert!(service::messages(&broke, &session.id, None, None)
        .unwrap()
        .is_empty());
    assert!(!running_for_test(&session.id));

    // A turn that is already running when the credits run out asks the model nothing
    // more: the gate stands before every completion.
    let model = Scripted::answering(vec![]);
    let outcome = run_turn(
        session.clone(),
        broke.session.clone(),
        broke.acts_as(),
        agent_session_store(),
        deps(model.clone()),
    )
    .await;
    assert!(outcome.is_err(), "{outcome:?}");
    assert_eq!(model.times_asked(), 0);

    // The same turn of a user who can pay runs.
    let session = create(&funded);
    let model = Scripted::answering(vec![]);
    let outcome = run_turn(
        session.clone(),
        funded.session.clone(),
        funded.acts_as(),
        agent_session_store(),
        deps(model.clone()),
    )
    .await;
    assert_eq!(outcome.unwrap(), "done");
    assert_eq!(model.times_asked(), 1);
}

// ── the capability intersection on the real MCP surface ─────────────────────

/// Two perspectives of `did`'s; the returned ids unregister on `clean_up`.
async fn two_perspectives_of(did: &str) -> (String, String, Vec<String>) {
    let a = crate::mcp::tools::test_support::perspective(Some(vec![did.to_string()])).await;
    let b = crate::mcp::tools::test_support::perspective(Some(vec![did.to_string()])).await;
    (a.clone(), b.clone(), vec![a, b])
}

async fn clean_up(perspectives: Vec<String>) {
    for uuid in perspectives {
        crate::perspectives::unregister_perspective(&uuid);
    }
}

/// Every link of the perspective `uuid` as (source, predicate, target), sorted.
async fn links_of(uuid: &str) -> Vec<(String, String, String)> {
    let mut links: Vec<_> = crate::perspectives::get_perspective(uuid)
        .expect("the perspective")
        .get_links(&crate::types::LinkQuery::default())
        .await
        .expect("its links")
        .into_iter()
        .map(|link| {
            (
                link.data.source,
                link.data.predicate.unwrap_or_default(),
                link.data.target,
            )
        })
        .collect();
    links.sort();
    links
}

/// What a tool answers a session whose capabilities do not cover the call.
const NOT_HELD: &str = "Capability is not matched";

fn refusal(step: &AgentSessionMessage) -> &str {
    step.content["error"].as_str().unwrap_or_default()
}

#[tokio::test(flavor = "multi_thread")]
async fn a_read_only_sessions_write_tool_is_refused_and_a_read_answers() {
    setup();
    let did = "did:key:alice-d1";
    let (a, _b, cleanup) = two_perspectives_of(did).await;
    // The caller holds read on A; the session asks for exactly that.
    let ctx = user(did, vec![perspective_query_capability(vec![a.clone()])]);
    let session = service::create(
        &ctx,
        AgentSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![perspective_query_capability(vec![a.clone()])]),
            ..Default::default()
        },
    )
    .unwrap();
    let before = links_of(&a).await;

    // Read on A answers; a write on A is refused, and no link lands. `None` for the
    // provider: the turn's real MCP surface, acting as the session's narrowed in-process
    // session.
    let steps = steps_of(
        &session,
        &ctx,
        Scripted::answering(vec![
            calls("query_links", json!({"perspective_id": a})),
            calls(
                "add_link",
                json!({
                    "perspective_id": a,
                    "source": "s://a", "predicate": "p://x", "target": "t://1",
                }),
            ),
        ]),
        None,
    )
    .await;

    assert_eq!(steps.len(), 2, "{steps:?}");
    assert_eq!(
        steps[0].status,
        Some(AgentTurnStatus::Done),
        "the read answers: {steps:?}"
    );
    assert_eq!(
        steps[1].status,
        Some(AgentTurnStatus::Failed),
        "the write is refused: {steps:?}"
    );
    assert!(
        refusal(&steps[1]).contains(NOT_HELD),
        "the model reads why: {steps:?}"
    );
    assert!(steps[1].content.get("result").is_none());
    assert_eq!(links_of(&a).await, before, "the refused write left a link");
    clean_up(cleanup).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn a_session_reaches_no_perspective_outside_its_capabilities() {
    setup();
    let did = "did:key:alice-d2";
    let (a, b, cleanup) = two_perspectives_of(did).await;
    // The caller may read both; the session may read only A. What narrows the turn is the
    // session's set, not the caller's.
    let ctx = user(
        did,
        vec![
            perspective_query_capability(vec![a.clone()]),
            perspective_query_capability(vec![b.clone()]),
        ],
    );
    let session = service::create(
        &ctx,
        AgentSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![perspective_query_capability(vec![a.clone()])]),
            ..Default::default()
        },
    )
    .unwrap();

    let steps = steps_of(
        &session,
        &ctx,
        Scripted::answering(vec![calls("query_links", json!({"perspective_id": b}))]),
        None,
    )
    .await;

    assert_eq!(steps.len(), 1, "{steps:?}");
    assert_eq!(
        steps[0].status,
        Some(AgentTurnStatus::Failed),
        "perspective B is outside the session's caps: {steps:?}"
    );
    assert!(refusal(&steps[0]).contains(NOT_HELD), "{steps:?}");
    clean_up(cleanup).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_reaches_no_further_than_the_caller_who_sends_it() {
    setup();
    let did = "did:key:alice-d4";
    let (a, b, cleanup) = two_perspectives_of(did).await;
    let read = |uuid: &String| perspective_query_capability(vec![uuid.clone()]);
    // Made by a caller who reads A and B: the session holds both.
    let wide = user(did, vec![read(&a), read(&b)]);
    let session = create(&wide);
    assert_eq!(session.capabilities, vec![read(&a), read(&b)]);

    // Sent by another session of the same agent, one that reads A alone.
    let narrow = user(did, vec![read(&a)]);
    let steps = steps_of(
        &session,
        &narrow,
        Scripted::answering(vec![
            calls("query_links", json!({"perspective_id": a})),
            calls("query_links", json!({"perspective_id": b})),
        ]),
        None,
    )
    .await;
    assert_eq!(steps.len(), 2, "{steps:?}");
    assert_eq!(steps[0].status, Some(AgentTurnStatus::Done), "{steps:?}");
    assert_eq!(
        steps[1].status,
        Some(AgentTurnStatus::Failed),
        "the sender does not read B, so neither does its turn: {steps:?}"
    );
    assert!(refusal(&steps[1]).contains(NOT_HELD), "{steps:?}");

    // The session itself reaches B: sent by the wide caller, the same call answers.
    let steps = steps_of(
        &session,
        &wide,
        Scripted::answering(vec![calls("query_links", json!({"perspective_id": b}))]),
        None,
    )
    .await;
    assert_eq!(steps[0].status, Some(AgentTurnStatus::Done), "{steps:?}");
    clean_up(cleanup).await;
}

/// The tools that answer before a caller is authenticated, less the two that only read:
/// what is left logs in, signs up, asks for a capability or mints a token.
fn session_opening_tools() -> Vec<&'static str> {
    let opening: Vec<&str> = crate::mcp::tools::AUTH_TOOLS
        .iter()
        .copied()
        .filter(|tool| !["get_documentation", "auth_status"].contains(tool))
        .collect();
    assert_eq!(opening.len(), 6, "{opening:?}");
    opening
}

// The second lock behind the turn's tool filter: the bridge every in-process caller
// dispatches through has no arm for these, whoever holds the handler.
#[tokio::test(flavor = "multi_thread")]
async fn the_in_process_bridge_dispatches_no_tool_that_opens_or_widens_a_session() {
    setup();
    let operator =
        crate::mcp::tools::test_support::handler_as(crate::mcp::tools::test_support::operator());
    for tool in session_opening_tools() {
        let answer = match operator.call_tool_by_name(tool, json!({})).await {
            Ok(answer) => answer,
            Err(e) => e.to_string(),
        };
        assert!(
            answer.contains(&format!("Unknown tool: {tool}")),
            "`{tool}` was dispatched: {answer}"
        );
    }
    // The tools beside them that only read still answer.
    let status = operator
        .call_tool_by_name("auth_status", json!({}))
        .await
        .expect("auth_status answers");
    assert!(!status.contains("Unknown tool"), "{status}");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_is_offered_no_tool_that_opens_or_widens_a_session() {
    setup();
    let ctx = user("did:key:alice-d3", everything());
    let session = create(&ctx);
    let opening = session_opening_tools();

    let model = Scripted::answering(opening.iter().map(|tool| calls(tool, json!({}))).collect());
    let steps = steps_of(&session, &ctx, model.clone(), None).await;

    let offered: Vec<String> = model.offered().into_iter().map(|tool| tool.name).collect();
    assert!(
        offered.iter().any(|tool| tool == "query_links"),
        "the turn has the executor's tools: {offered:?}"
    );
    for tool in &opening {
        assert!(
            !offered.iter().any(|name| name == tool),
            "`{tool}` is on offer"
        );
    }
    // A model that names one anyway gets no further.
    assert_eq!(steps.len(), opening.len(), "{steps:?}");
    for step in &steps {
        assert_eq!(step.status, Some(AgentTurnStatus::Failed), "{step:?}");
        assert!(
            refusal(step).contains("is not in the current provider's filtered surface"),
            "{step:?}"
        );
    }
}

// ── every tool, against a perspective the session does not hold ─────────────

/// A subject class `attack://{class}` with a `body` text and an `items` collection.
fn class_shape(class: &str) -> String {
    json!({
        "target_class": format!("attack://{class}"),
        "constructor_actions": [{
            "action": "addLink", "source": "this",
            "predicate": "rdf://type", "target": format!("attack://{class}"),
        }],
        "properties": [
            {
                "path": "attack://body", "name": "body", "datatype": "xsd:string",
                "min_count": 1, "max_count": 1, "writable": true,
                "setter": [{
                    "action": "setSingleTarget", "source": "this",
                    "predicate": "attack://body", "target": "value",
                }],
            },
            {
                "path": "ad4m://has_child", "name": "items", "collection": true,
                "writable": true,
                "adder": [{
                    "action": "addLink", "source": "this",
                    "predicate": "ad4m://has_child", "target": "value",
                }],
                "remover": [{
                    "action": "removeLink", "source": "this",
                    "predicate": "ad4m://has_child", "target": "value",
                }],
            },
        ],
    })
    .to_string()
}

/// Gives the perspective `uuid` the class `class`, one instance `item://{name}` of it whose
/// body is `secret`, and a plain link to `secret` — written by the operator.
async fn seed(uuid: &str, class: &str, name: &str, secret: &str) {
    let operator =
        crate::mcp::tools::test_support::handler_as(crate::mcp::tools::test_support::operator());
    let base = format!("item://{name}");
    for (tool, arguments) in [
        (
            "add_model",
            json!({"perspective_id": uuid, "class_name": class, "shacl_json": class_shape(class)}),
        ),
        (
            "instance_create",
            json!({
                "perspective_id": uuid, "class_name": class, "base_uri": base,
                "properties": {"body": secret},
            }),
        ),
        (
            "add_link",
            json!({
                "perspective_id": uuid, "source": base,
                "predicate": "attack://holds", "target": format!("literal:string:{secret}"),
            }),
        ),
    ] {
        operator
            .call_tool_by_name(tool, arguments)
            .await
            .unwrap_or_else(|e| panic!("seeding with {tool}: {e}"));
    }
}

/// One call of every tool that names a perspective, aimed at `p`, whose class is `class`
/// and whose instance is `item://{name}`: the reads first, then the writes, then the two
/// kinds of tool a class brings.
fn perspective_calls(p: &str, class: &str, name: &str) -> Vec<(String, Value)> {
    let base = format!("item://{name}");
    let command = json!([{
        "action": "addLink", "source": "this",
        "predicate": "attack://wrote", "target": "attack://by-command",
    }])
    .to_string();
    let mut rows: Vec<(String, Value)> = [
        ("query_links", json!({"perspective_id": p})),
        ("describe_perspective", json!({"perspective_id": p})),
        ("instance_query", json!({"perspective_id": p, "class_name": class})),
        ("instance_get", json!({"perspective_id": p, "class_name": class, "base_uri": base})),
        ("instance_transcript", json!({"perspective_id": p, "parent": base})),
        ("get_children", json!({"perspective_id": p, "parent": base})),
        ("get_flows", json!({"perspective_id": p})),
        ("flow_state", json!({"perspective_id": p, "flow_name": "f", "expression_address": base})),
        ("flow_actions", json!({"perspective_id": p, "flow_name": "f", "expression_address": base})),
        ("flow_valid_outputs", json!({"perspective_id": p, "flow_name": "f"})),
        ("generate_waker_query", json!({"perspective_id": p, "class_name": class})),
        ("get_mention_waker_config", json!({"perspective_id": p})),
        ("template_read", json!({"perspective_id": p, "base_uri": base})),
        (
            "add_link",
            json!({
                "perspective_id": p, "source": base,
                "predicate": "attack://wrote", "target": "attack://landed",
            }),
        ),
        (
            "add_model",
            json!({"perspective_id": p, "class_name": "Planted", "shacl_json": class_shape("Planted")}),
        ),
        (
            "instance_create",
            json!({"perspective_id": p, "class_name": class, "properties": {"body": "planted"}}),
        ),
        (
            "instance_update",
            json!({
                "perspective_id": p, "class_name": class, "base_uri": base,
                "properties": {"body": "changed"},
            }),
        ),
        (
            "instance_add_to_collection",
            json!({
                "perspective_id": p, "class_name": class, "base_uri": base,
                "collection": "items", "item_uri": "attack://item",
            }),
        ),
        (
            "instance_remove_from_collection",
            json!({
                "perspective_id": p, "class_name": class, "base_uri": base,
                "collection": "items", "item_uri": "attack://item",
            }),
        ),
        ("add_child", json!({"perspective_id": p, "parent": base, "child": "attack://child"})),
        (
            "execute_commands",
            json!({"perspective_id": p, "commands": command, "expression_address": base}),
        ),
        ("template_update", json!({"perspective_id": p, "base_uri": base})),
        ("add_flow", json!({"perspective_id": p, "flow_name": "planted", "shacl_json": "{}"})),
        ("flow_proposal_accept", json!({"perspective_id": p, "proposal_uri": base})),
        ("flow_proposal_reject", json!({"perspective_id": p, "proposal_uri": base})),
        (
            "neighbourhood_publish_from_perspective",
            json!({"perspective_uuid": p, "link_language": "lang://none"}),
        ),
        ("instance_remove", json!({"perspective_id": p, "class_name": class, "base_uri": base})),
    ]
    .into_iter()
    .map(|(tool, arguments)| (tool.to_string(), arguments))
    .collect();
    let lower = class.to_lowercase();
    rows.push((format!("{lower}_query"), json!({"perspective_id": p})));
    rows.push((
        format!("{lower}_create"),
        json!({"perspective_id": p, "body": "planted"}),
    ));
    rows
}

/// One call of every tool that names no perspective: what it reaches is the agent's.
fn agent_wide_calls() -> Vec<(String, Value)> {
    [
        ("list_perspectives", json!({})),
        ("add_perspective", json!({"name": "planted"})),
        (
            "neighbourhood_join_from_url",
            json!({"url": "neighbourhood://QmPlanted"}),
        ),
        ("set_agent_profile", json!({"username": "planted"})),
        (
            "set_agent_profile_picture",
            json!({"image_base64": "AAAA", "mime_type": "image/png"}),
        ),
        ("set_agent_public_perspective", json!({"links_json": "[]"})),
        ("get_agent_profile", json!({})),
        ("get_agent_public_perspective", json!({})),
        ("get_my_did", json!({})),
        ("language_meta", json!({"address": "QmNone"})),
        ("list_link_language_templates", json!({})),
    ]
    .into_iter()
    .map(|(tool, arguments)| (tool.to_string(), arguments))
    .collect()
}

/// Calls on `a`, which the session holds, that name `b` or what is in it wherever an
/// argument takes a free value: none of them may read from `b` or write to it.
fn calls_naming_another_perspective(a: &str, b: &str, theirs: &str) -> Vec<(String, Value)> {
    let command = json!([{
        "action": "addLink", "source": "this",
        "predicate": "attack://wrote", "target": "attack://by-command",
    }])
    .to_string();
    [
        (
            "query_links",
            json!({"perspective_id": a, "perspective_uuid": b, "uuid": b, "source": theirs}),
        ),
        (
            "instance_get",
            json!({"perspective_id": a, "class_name": "Note", "base_uri": theirs}),
        ),
        (
            "instance_query",
            json!({"perspective_id": a, "class_name": "Vault"}),
        ),
        (
            "instance_query",
            json!({
                "perspective_id": a, "class_name": "Note", "parent": b,
                "filter": {"perspective_id": b, "id": theirs},
            }),
        ),
        (
            "get_children",
            json!({"perspective_id": a, "parent": theirs}),
        ),
        (
            "instance_transcript",
            json!({"perspective_id": a, "parent": b}),
        ),
        (
            "instance_update",
            json!({
                "perspective_id": a, "class_name": "Note", "base_uri": theirs,
                "properties": {"body": "changed", "perspective_id": b},
            }),
        ),
        (
            "add_child",
            json!({"perspective_id": a, "parent": b, "child": theirs}),
        ),
        (
            "execute_commands",
            json!({"perspective_id": a, "commands": command, "expression_address": theirs}),
        ),
    ]
    .into_iter()
    .map(|(tool, arguments)| (tool.to_string(), arguments))
    .collect()
}

/// Whether a tool's parameters name a perspective anywhere below their top level, where
/// `perspective_calls` would not aim it.
fn names_a_perspective_below_the_top(parameters: &Value) -> bool {
    fn any_key(value: &Value) -> bool {
        match value {
            Value::Object(object) => object
                .iter()
                .any(|(key, value)| key.contains("perspective") || any_key(value)),
            Value::Array(items) => items.iter().any(any_key),
            _ => false,
        }
    }
    parameters.as_object().is_some_and(|top| {
        top.iter().any(|(key, value)| match value.as_object() {
            Some(properties) if key == "properties" => properties.values().any(any_key),
            _ => any_key(value),
        })
    })
}

/// The tool steps of `table` called in order on `session` by `caller`, and the tools the
/// model was offered. Six calls a turn: a turn stops at twenty.
async fn steps_of_calls(
    session: &AgentSession,
    caller: &RequestContext,
    table: &[(String, Value)],
) -> (Vec<AgentSessionMessage>, Vec<ToolSchema>) {
    let mut steps = Vec::new();
    let mut offered = Vec::new();
    for chunk in table.chunks(6) {
        let model = Scripted::answering(
            chunk
                .iter()
                .map(|(tool, arguments)| calls(tool, arguments.clone()))
                .collect(),
        );
        steps.extend(steps_of(session, caller, model.clone(), None).await);
        offered = model.offered();
    }
    assert_eq!(steps.len(), table.len(), "one step a call: {steps:?}");
    (steps, offered)
}

/// A session of `caller`'s that holds read and write on perspective A and nothing else,
/// sent by that caller, who holds everything: whatever tool a turn calls, perspective B —
/// the same agent's — is neither read nor changed, and nothing of the agent's outside A is
/// reached.
async fn a_narrowed_sessions_tools_stay_inside_it(
    caller: RequestContext,
    owners: Option<Vec<String>>,
) {
    use crate::mcp::tools::test_support;
    let _runtime = test_support::RuntimeService::start();
    let a = test_support::perspective(owners.clone()).await;
    let b = test_support::perspective(owners).await;
    seed(&a, "Note", "inside-a", "words-of-a").await;
    seed(&b, "Vault", "inside-b", "words-of-b").await;
    let untouched = links_of(&b).await;
    assert!(
        untouched
            .iter()
            .any(|(_, _, target)| target.contains("words-of-b")),
        "B holds its words: {untouched:?}"
    );

    let session = service::create(
        &caller,
        AgentSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![
                perspective_query_capability(vec![a.clone()]),
                perspective_update_capability(vec![a.clone()]),
            ]),
            ..Default::default()
        },
    )
    .unwrap();

    // Every tool that names a perspective, aimed at B.
    let table = perspective_calls(&b, "Vault", "inside-b");
    let (outside, offered) = steps_of_calls(&session, &caller, &table).await;
    for (step, (tool, _)) in outside.iter().zip(&table) {
        assert_eq!(
            step.status,
            Some(AgentTurnStatus::Failed),
            "`{tool}` on B: {step:?}"
        );
        // A class tool of B's is not there to call; every other tool refuses the session.
        let expected = if tool.starts_with("vault_") {
            "is not in the current provider's filtered surface"
        } else {
            NOT_HELD
        };
        assert!(refusal(step).contains(expected), "`{tool}` on B: {step:?}");
        assert!(
            step.content.get("result").is_none(),
            "`{tool}` on B: {step:?}"
        );
    }

    // The table is the whole surface: every tool on offer that names a perspective has a
    // row (A's class tools aside: B's are the ones aimed at B), none names one where the
    // table would not aim it, and no class tool of B's is on offer.
    for tool in &offered {
        let names_a_perspective = tool.parameters["properties"]
            .as_object()
            .is_some_and(|properties| properties.keys().any(|key| key.contains("perspective")));
        assert!(
            !names_a_perspective
                || tool.name.starts_with("note_")
                || table.iter().any(|(row, _)| *row == tool.name),
            "`{}` names a perspective and `perspective_calls` has no row for it",
            tool.name
        );
        assert!(
            !names_a_perspective_below_the_top(&tool.parameters),
            "`{}` names a perspective inside an argument: {}",
            tool.name,
            tool.parameters
        );
        assert!(
            !tool.name.starts_with("vault_"),
            "`{}` is on offer",
            tool.name
        );
    }
    assert!(
        offered.iter().any(|tool| tool.name == "note_query"),
        "A's class tools are on offer"
    );

    // Every tool that names none: the session holds no capability of the agent's own.
    let table = agent_wide_calls();
    let (agent_wide, _) = steps_of_calls(&session, &caller, &table).await;
    for (step, (tool, _)) in agent_wide.iter().zip(&table) {
        assert_eq!(
            step.status,
            Some(AgentTurnStatus::Failed),
            "`{tool}`: {step:?}"
        );
        assert!(refusal(step).contains(NOT_HELD), "`{tool}`: {step:?}");
    }
    let (status, _) =
        steps_of_calls(&session, &caller, &[("auth_status".to_string(), json!({}))]).await;
    assert!(
        !status[0].content.to_string().contains(&b),
        "what the session reads of itself names B: {status:?}"
    );
    for tool in &offered {
        let listed = table.iter().any(|(row, _)| *row == tool.name);
        let names_a_perspective = tool.parameters["properties"]
            .as_object()
            .is_some_and(|properties| properties.keys().any(|key| key.contains("perspective")));
        assert!(
            names_a_perspective
                || listed
                || ["auth_status", "get_documentation"].contains(&tool.name.as_str()),
            "`{}` names no perspective and `agent_wide_calls` has no row for it",
            tool.name
        );
    }

    // The same session on A, which it holds: the reads answer with A's words and the write
    // lands, so what refused every call above was the session's capabilities.
    let (inside, _) = steps_of_calls(
        &session,
        &caller,
        &[
            ("query_links".to_string(), json!({"perspective_id": a})),
            (
                "instance_query".to_string(),
                json!({"perspective_id": a, "class_name": "Note"}),
            ),
            ("note_query".to_string(), json!({"perspective_id": a})),
            (
                "add_link".to_string(),
                json!({
                    "perspective_id": a, "source": "item://inside-a",
                    "predicate": "attack://wrote", "target": "attack://landed",
                }),
            ),
        ],
    )
    .await;
    for step in &inside {
        assert_eq!(step.status, Some(AgentTurnStatus::Done), "on A: {step:?}");
    }
    for step in &inside[..2] {
        assert!(
            step.content["result"]
                .as_str()
                .unwrap_or_default()
                .contains("words-of-a"),
            "on A: {step:?}"
        );
    }
    assert!(
        links_of(&a).await.contains(&(
            "item://inside-a".to_string(),
            "attack://wrote".to_string(),
            "attack://landed".to_string(),
        )),
        "the write on A landed: {inside:?}"
    );

    // Calls on A that name B wherever an argument takes a free value.
    let table = calls_naming_another_perspective(&a, &b, "item://inside-b");
    let (naming, _) = steps_of_calls(&session, &caller, &table).await;

    // Nothing of B's reached the transcript, and B is as it was.
    for step in outside
        .iter()
        .chain(&agent_wide)
        .chain(&inside)
        .chain(&naming)
    {
        assert!(
            !step.content.to_string().contains("words-of-b"),
            "B's words are in the transcript: {step:?}"
        );
    }
    assert_eq!(links_of(&b).await, untouched, "B changed");
    let perspectives = test_support::handler_as(test_support::operator())
        .call_tool_by_name("list_perspectives", json!({}))
        .await
        .unwrap();
    assert!(!perspectives.contains("planted"), "{perspectives}");
    service::delete(&caller, &session.id).unwrap();
    clean_up(vec![a, b]).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn no_tool_reaches_a_perspective_outside_a_users_sessions_capabilities() {
    setup();
    let did = crate::test_utils::create_test_user("alice-d5@example.org");
    a_narrowed_sessions_tools_stay_inside_it(user(&did, everything()), Some(vec![did])).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn no_tool_reaches_a_perspective_outside_the_main_agents_sessions_capabilities() {
    setup();
    a_narrowed_sessions_tools_stay_inside_it(operator(), None).await;
}
