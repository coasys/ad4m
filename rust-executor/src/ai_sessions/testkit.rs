//! What the session tests share: callers, a scripted model, stub tool providers and the
//! fixtures that send a turn and wait on its rows.
//!
//! The model is a script ([`Scripted`]); the tools a turn sees are a stub provider unless
//! a test names the real MCP surface (`provider: None`). `send` spawns its turn as a
//! detached task, so a test waits on the reply row's status, never on a handle.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use serde_json::{json, Value};

use super::run::{run_turn, TurnDeps};
use super::service::{self, AiSessionCreate, AiSessionUpdateParams, SessionError};
use super::store::{ai_session_store, DbAiSessionStore};
use super::types::AiTurnStatus::*;
use super::types::{AiSession, AiSessionMessage, AiSessionMessageRole};
use crate::agent::capabilities::{Capability, ALL_CAPABILITY};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
use crate::types::RequestContext;

// ── callers ─────────────────────────────────────────────────────────────────

pub(super) fn ctx_for(session: Session) -> RequestContext {
    RequestContext {
        cancel_token: None,
        session: Arc::new(session),
        connection_id: None,
        peer: None,
    }
}

/// A managed user `did` holding `caps`, as a session over the RPC socket sees them.
pub(super) fn user(did: &str, caps: Vec<Capability>) -> RequestContext {
    ctx_for(
        Session::for_test(Principal::Agent(AgentContext::managed(did.to_string())))
            .with_caps(caps)
            .with_did(did),
    )
}

/// The operator's session: it acts as the main agent.
pub(super) fn operator() -> RequestContext {
    ctx_for(Session::for_test(Principal::Operator))
}

pub(super) fn everything() -> Vec<Capability> {
    vec![ALL_CAPABILITY.clone()]
}

/// Alice `did:key:alice-{tag}` holding everything, and a session of hers named "test".
pub(super) fn alice(tag: &str) -> (RequestContext, AiSession) {
    let caller = user(&format!("did:key:alice-{tag}"), everything());
    let session = create(&caller);
    (caller, session)
}

// ── a scripted model ─────────────────────────────────────────────────────────

/// A model that answers `script` completions one per call, then "done"; `released` keeps a
/// completion pending until it flips, so a turn stays `running` until the test lets it
/// end — whenever it starts, no notification to miss.
pub(super) struct Scripted {
    script: Mutex<Vec<HarnessCompletion>>,
    released: Arc<AtomicBool>,
    /// What each completion was asked with: the messages and the tools on offer.
    pub(super) asked: Mutex<Vec<(Vec<Value>, Vec<ToolSchema>)>>,
}

impl Scripted {
    pub(super) fn answering(script: Vec<HarnessCompletion>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            released: Arc::new(AtomicBool::new(true)),
            asked: Mutex::new(Vec::new()),
        })
    }

    /// A model whose completions wait on `released`: a turn stays inside the model's
    /// answer until the test lets it land.
    pub(super) fn holding_with(
        released: Arc<AtomicBool>,
        script: Vec<HarnessCompletion>,
    ) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            released,
            asked: Mutex::new(Vec::new()),
        })
    }

    pub(super) fn holding(released: Arc<AtomicBool>) -> Arc<Self> {
        Self::holding_with(released, vec![])
    }

    /// How many completions the model was asked for.
    pub(super) fn times_asked(&self) -> usize {
        self.asked.lock().unwrap().len()
    }

    /// The tools the first completion was offered.
    pub(super) fn offered(&self) -> Vec<ToolSchema> {
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

/// A model that panics when asked: the turn's task dies without settling anything.
pub(super) struct Panicking;

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

pub(super) fn calls(name: &str, arguments: Value) -> HarnessCompletion {
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
pub(super) struct StubProvider {
    tools: Vec<ToolSchema>,
    pub(super) calls: Mutex<Vec<String>>,
}

impl StubProvider {
    pub(super) fn named(names: &[&str]) -> Arc<Self> {
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

/// A provider whose one tool, `wait`, never returns: the turn that calls it stays inside
/// the call until something ends it.
pub(super) struct Stuck;

#[async_trait::async_trait]
impl ToolProvider for Stuck {
    async fn tools(&self) -> Vec<ToolSchema> {
        vec![schema("wait")]
    }
    async fn call(&self, _name: &str, _args: Value) -> anyhow::Result<String> {
        std::future::pending().await
    }
}

pub(super) fn deps(completion: Arc<dyn CompletionSource>) -> TurnDeps {
    TurnDeps {
        completion: Some(completion),
        provider: Some(StubProvider::named(&[])),
        ..Default::default()
    }
}

/// Turn deps with a stub `provider` and `timeout` alongside the model.
pub(super) fn tooling(
    completion: Arc<Scripted>,
    provider: Arc<dyn ToolProvider>,
    timeout: Option<Duration>,
) -> TurnDeps {
    TurnDeps {
        completion: Some(completion),
        provider: Some(provider),
        timeout,
    }
}

// ── fixtures ─────────────────────────────────────────────────────────────────

pub(super) fn setup() {
    crate::mcp::tools::test_support::setup();
    service::clear_running_for_test();
    super::store::set_ai_session_store(Some(Arc::new(DbAiSessionStore)));
}

pub(super) fn create(ctx: &RequestContext) -> AiSession {
    service::create(
        ctx,
        AiSessionCreate {
            title: Some("test".into()),
            model: Some("scripted".into()),
            ..Default::default()
        },
    )
    .expect("a session")
}

/// What `check` finds, within 10 s.
pub(super) async fn poll<T>(what: &str, mut check: impl FnMut() -> Option<T>) -> T {
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(found) = check() {
            return found;
        }
        assert!(std::time::Instant::now() < deadline, "{what} within 10 s");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}

/// The first tool step of the session, once the turn has written it, within 10 s.
pub(super) async fn first_step(owner: &str, session_id: &str) -> AiSessionMessage {
    let store = ai_session_store();
    poll("the turn wrote no tool step", || {
        store
            .messages(Some(owner), session_id, 100, None)
            .unwrap()
            .into_iter()
            .find(|m| m.role == AiSessionMessageRole::Tool)
    })
    .await
}

/// The row `id` once it is not `running` any more, within 10 s.
pub(super) async fn settled(owner: Option<&str>, reply_id: &str) -> AiSessionMessage {
    let store = ai_session_store();
    poll("the turn did not settle", || {
        store
            .message(owner, reply_id)
            .ok()
            .flatten()
            .filter(|row| row.status != Some(Running))
    })
    .await
}

/// One turn of `session` sent by `caller`, the model answering from `model`: the tool steps
/// the turn wrote, in order.
pub(super) async fn steps_of(
    session: &AiSession,
    caller: &RequestContext,
    model: Arc<Scripted>,
    provider: Option<Arc<dyn ToolProvider>>,
) -> Vec<AiSessionMessage> {
    let store = ai_session_store();
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
        .filter(|m| m.role == AiSessionMessageRole::Tool)
        .collect()
}

/// The owner's DID as the store scopes it (`None` for the operator/main agent).
fn owner_did(caller: &RequestContext) -> Option<String> {
    caller.user_did().map(str::to_string)
}

/// `send` "go" on `session`, unwrapped.
pub(super) async fn send_go(
    caller: &RequestContext,
    session: &AiSession,
    deps: TurnDeps,
) -> service::AiSessionSendStarted {
    service::send(caller, &session.id, "go", deps)
        .await
        .unwrap()
}

/// `send` a turn that stays `running` on the held model until `hold` flips.
pub(super) async fn send_held(
    caller: &RequestContext,
    session: &AiSession,
    hold: &Arc<AtomicBool>,
) -> service::AiSessionSendStarted {
    send_go(caller, session, deps(Scripted::holding(hold.clone()))).await
}

/// `send` plus the reply row settled, by the caller's own owner scope.
pub(super) async fn sent_reply(
    caller: &RequestContext,
    session: &AiSession,
    deps: TurnDeps,
) -> (service::AiSessionSendStarted, AiSessionMessage) {
    let started = send_go(caller, session, deps).await;
    let reply = settled(owner_did(caller).as_deref(), &started.reply_id).await;
    (started, reply)
}

/// `send` asking for the scripted tool calls through `provider`, then its reply row settled.
pub(super) async fn sent_tool_call(
    caller: &RequestContext,
    session: &AiSession,
    completions: Vec<HarnessCompletion>,
    provider: Arc<StubProvider>,
) -> AiSessionMessage {
    sent_reply(
        caller,
        session,
        tooling(Scripted::answering(completions), provider, None),
    )
    .await
    .1
}

/// Every session method `caller` can name `session` answers not-found — an id that
/// exists but is not theirs must read exactly like one that does not.
pub(super) async fn assert_unreachable(caller: &RequestContext, session: &AiSession) {
    for (what, outcome) in [
        ("get", service::get(caller, &session.id).map(|_| ())),
        (
            "messages",
            service::messages(caller, &session.id, None, None).map(|_| ()),
        ),
        (
            "update",
            service::update(caller, &session.id, AiSessionUpdateParams::default()).map(|_| ()),
        ),
        ("cancel", service::cancel(caller, &session.id).map(|_| ())),
        (
            "send",
            service::send(caller, &session.id, "hi", deps(Scripted::answering(vec![])))
                .await
                .map(|_| ()),
        ),
        ("delete", service::delete(caller, &session.id).map(|_| ())),
    ] {
        assert!(
            matches!(outcome, Err(SessionError::NotFound(_))),
            "{what}: {outcome:?}"
        );
    }
}

/// Waits until the model has been asked for its first completion.
pub(super) async fn until_asked(model: &Scripted) {
    while model.times_asked() == 0 {
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
}
