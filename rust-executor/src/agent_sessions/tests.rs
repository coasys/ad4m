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
    self, running_for_test, AgentSessionCreate, AgentSessionUpdateParams, SessionError,
};
use super::store::{agent_session_store, DbAgentSessionStore};
use super::types::{AgentSession, AgentSessionMessageRole, AgentTurnStatus};
use crate::agent::capabilities::{
    perspective_query_capability, perspective_update_capability, Capability, ALL_CAPABILITY,
};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
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
}

impl Scripted {
    fn answering(script: Vec<HarnessCompletion>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            released: Arc::new(AtomicBool::new(true)),
        })
    }

    /// A model whose completions wait on `released`: a turn stays `running` until the
    /// test lets it end.
    fn holding(released: Arc<AtomicBool>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(Vec::new()),
            released,
        })
    }
}

#[async_trait::async_trait]
impl CompletionSource for Scripted {
    async fn complete(
        &self,
        _model_id: &str,
        _messages: &[Value],
        _tools: Vec<ToolSchema>,
    ) -> anyhow::Result<HarnessCompletion> {
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

/// A provider that answers `ok:<name>` to any call and records them.
struct StubProvider {
    tools: Vec<ToolSchema>,
    calls: Mutex<Vec<String>>,
}

impl StubProvider {
    fn named(names: &[&str]) -> Arc<Self> {
        Arc::new(Self {
            tools: names
                .iter()
                .map(|name| ToolSchema {
                    name: name.to_string(),
                    description: format!("the {name} tool"),
                    parameters: json!({ "type": "object", "properties": {} }),
                    side_effect: crate::ai_service::harness::provider::SideEffect::Read,
                })
                .collect(),
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

fn deps(completion: Arc<dyn CompletionSource>) -> TurnDeps {
    TurnDeps {
        completion: Some(completion),
        provider: Some(StubProvider::named(&[])),
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

/// The reply row once it is not `running` any more, within 10 s.
async fn settled(owner: Option<&str>, reply_id: &str) -> super::types::AgentSessionMessage {
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
    assert!(!service::delete(&bob, &session.id).unwrap_or(false));
    assert!(service::get(&alice, &session.id).is_ok());
}

#[tokio::test(flavor = "multi_thread")]
async fn the_main_agent_and_a_user_never_reach_each_others_sessions() {
    setup();
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
    assert_eq!(service::list(&operator).unwrap().len(), 1);
    assert_eq!(service::list(&user_a).unwrap().len(), 1);
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

    let started = service::send(&alice, &session.id, "go", deps(Scripted::holding(hold)))
        .await
        .unwrap();
    assert!(service::cancel(&alice, &session.id).unwrap());
    let reply = settled(Some("did:key:alice-a7"), &started.reply_id).await;
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

    // Read on A answers; a write on A is refused, and no link lands.
    let caller = ctx.session.clone();
    let agent = AgentContext::managed(did.to_string());
    let store = agent_session_store();
    run_turn(
        session.clone(),
        caller,
        agent,
        store.clone(),
        TurnDeps {
            completion: Some(Scripted::answering(vec![
                calls("query_links", json!({"perspective_id": a, "query": {}})),
                calls(
                    "add_link",
                    json!({
                        "perspective_id": a,
                        "link": {"source": "s://a", "predicate": "p://x", "target": "t://1"},
                    }),
                ),
            ])),
            // `None`: the turn's real MCP surface, acting as the session's narrowed
            // in-process session.
            provider: None,
        },
    )
    .await
    .unwrap();

    let rows = store.messages(Some(did), &session.id, 100, None).unwrap();
    let steps: Vec<_> = rows
        .iter()
        .filter(|m| m.role == AgentSessionMessageRole::Tool)
        .collect();
    assert_eq!(steps.len(), 2, "{rows:?}");
    assert_eq!(
        steps[0].status,
        Some(AgentTurnStatus::Done),
        "the read answers"
    );
    assert_eq!(
        steps[1].status,
        Some(AgentTurnStatus::Failed),
        "the write is refused"
    );
    let error = steps[1].content["error"].as_str().unwrap_or_default();
    assert!(
        error.contains("capab")
            || error.contains("permission")
            || error.contains("denied")
            || error.contains("403")
            || error.contains("missing"),
        "the refusal is readable by the model: {error}"
    );
    assert!(steps[1].content.get("result").is_none());
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

    run_turn(
        session.clone(),
        ctx.session.clone(),
        AgentContext::managed(did.to_string()),
        agent_session_store(),
        TurnDeps {
            completion: Some(Scripted::answering(vec![calls(
                "query_links",
                json!({"perspective_id": b, "query": {}}),
            )])),
            provider: None,
        },
    )
    .await
    .unwrap();

    let rows = agent_session_store()
        .messages(Some(did), &session.id, 100, None)
        .unwrap();
    let step = rows
        .iter()
        .find(|m| m.role == AgentSessionMessageRole::Tool)
        .expect("a tool step");
    assert_eq!(
        step.status,
        Some(AgentTurnStatus::Failed),
        "perspective B is outside the session's caps: {rows:?}"
    );
    clean_up(cleanup).await;
}
