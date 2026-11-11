//! The session service: the calls `agent.session*` make, and the registry of the turns
//! they start.
//!
//! A send stores the person's message and a `running` assistant row, hands the turn to a
//! detached tokio task, and returns at once — the answer lands on the row (and on the
//! `agent-session-changed` event) whenever it is ready; the caller's socket may be gone by
//! then. The task is the only thing a run keeps in memory ([`RUNNING`], one slot per
//! session): the status it will write is the row's `status`, so a restart loses only the
//! handle, and the startup sweep marks every row still `running` `interrupted`.
//!
//! [`RUNNING`] also limits the work: one running turn per session, and at most
//! [`MAX_RUNNING_PER_CALLER`] per caller. The slot is taken before the rows are written,
//! so two sends on one session can never race into two turns, and a cancel that finds no
//! slot yet still ends the turn — the start marks it `cancelled` when it notices.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use tokio::task::AbortHandle;
use ts_rs::TS;
use uuid::Uuid;

use super::run::{run_turn, TurnDeps};
use super::store::{agent_session_store, AgentSessionStore};
use super::types::{
    assistant_content, user_content, AgentSession, AgentSessionChanged, AgentSessionMessage,
    AgentSessionMessageRole, AgentSessionUpdate, AgentTurnStatus,
};
use crate::agent::capabilities::{caps_within, Capability};
use crate::agent::AgentContext;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub_sync;
use crate::types::RequestContext;

/// A turn runs at most this long before the task is ended and the row marked `failed`:
/// long enough for an agent reading docs and editing a large record over many calls.
const TURN_TIMEOUT: Duration = Duration::from_secs(15 * 60);

/// The running turns a caller may have across their sessions.
pub(crate) const MAX_RUNNING_PER_CALLER: usize = 8;

/// The failure a session call reports. The API maps each onto the code its kind means:
/// `NotFound` 404 (missing or another agent's — they are one answer), `Busy` and
/// `TooManyTurns` 409, `Invalid` 400, `Failed` 500.
#[derive(Debug, thiserror::Error)]
pub enum SessionError {
    #[error("Agent session {0} not found")]
    NotFound(String),
    #[error("a turn is already running on this session; cancel it or let it end")]
    Busy,
    #[error("you already have {MAX_RUNNING_PER_CALLER} turns running; cancel one or let it end")]
    TooManyTurns,
    #[error("{0}")]
    Invalid(String),
    #[error(transparent)]
    Failed(#[from] anyhow::Error),
}

/// `agent.send`'s answer: the ids of the person's message and of the `running` assistant
/// row the detached task writes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSendStarted {
    pub message_id: String,
    pub reply_id: String,
}

/// What `agent.sessionCreate` takes.
#[derive(Debug, Clone, Default, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionCreate {
    pub title: Option<String>,
    pub model: Option<String>,
    /// The perspective the conversation is about, as a hint for the model. Not a limit.
    #[serde(default)]
    pub default_perspective: Option<String>,
    /// The perspective holding the agent's WE template library.
    #[serde(default)]
    pub library_perspective: Option<String>,
    /// What the session's tools may do. Absent: everything the caller holds.
    #[serde(default)]
    pub capabilities: Option<Vec<Capability>>,
}

/// What `agent.sessionUpdate` takes. A field left out keeps its value; a nullable one is
/// cleared with `null` — the `Option<Option<..>>` serde shape, which plain serde collapses
/// to `None` either way, so each one takes [`triple_option`].
#[derive(Debug, Clone, Default, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionUpdateParams {
    #[serde(default)]
    pub title: Option<String>,
    #[serde(default, deserialize_with = "triple_option")]
    pub model: Option<Option<String>>,
    #[serde(default, deserialize_with = "triple_option")]
    pub default_perspective: Option<Option<String>>,
    #[serde(default, deserialize_with = "triple_option")]
    pub library_perspective: Option<Option<String>>,
    #[serde(default)]
    pub capabilities: Option<Vec<Capability>>,
}

/// Reads `Option<Option<T>>` the way `Option<T>` reads its field: absent stays the
/// `#[serde(default)]` `None`, `null` becomes `Some(None)`, a value `Some(Some(_))`.
fn triple_option<'de, D, T>(deserializer: D) -> Result<Option<Option<T>>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Ok(Some(Option::<T>::deserialize(deserializer)?))
}

/// The turns running now, keyed by session id. A `std::sync::Mutex`: held to read or
/// replace an entry, never across an await.
static RUNNING: Lazy<Mutex<HashMap<String, RunningTurn>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// One running turn's registry slot: everything needed to end it early and to know whose
/// it is. The row the turn writes is in the database; this is only the live task.
struct RunningTurn {
    /// The id of the assistant row the turn writes, set before the task registers, so a
    /// cancel that races the spawn can still name it.
    reply: String,
    /// The agent that sent it: for the per-caller limit and the events.
    agent: AgentContext,
    /// The task's abort handle once the spawn has handed it over, `None` between the slot
    /// reservation and the registration.
    abort: Option<AbortHandle>,
}

/// The slot a running task holds until it ends — `Drop` frees it, so a panicking or
/// aborted task can never hold its session's slot forever.
struct TurnSlot {
    session_id: String,
    reply: String,
}

impl Drop for TurnSlot {
    fn drop(&mut self) {
        let mut running = RUNNING.lock().unwrap();
        // Free the slot only if it is still this task's: a cancel has already taken it,
        // and a newer send's slot for the same session is not ours to free.
        if running
            .get(&self.session_id)
            .is_some_and(|t| t.reply == self.reply)
        {
            running.remove(&self.session_id);
        }
    }
}

/// The row's new state published to its owner, so a watching client draws each message,
/// status change and step as it lands.
fn publish(agent: &AgentContext, changed: AgentSessionChanged) {
    get_global_pubsub_sync().publish_event(Event::new(
        Topic::AgentSessionChanged,
        Audience::of(agent),
        changed,
    ));
}

/// The `agent-session-changed` event of a message row (`kind: "message"`), to the
/// session's owner. The observer calls this for every step it writes.
pub(crate) fn publish_step(agent: &AgentContext, session_id: &str, message: AgentSessionMessage) {
    publish(
        agent,
        AgentSessionChanged {
            session_id: session_id.to_string(),
            kind: "message".to_string(),
            session: None,
            message: Some(message),
        },
    );
}

/// A session row event (`created`, `updated`, `deleted`), to its owner.
fn publish_session(agent: &AgentContext, kind: &str, session: &AgentSession) {
    publish(
        agent,
        AgentSessionChanged {
            session_id: session.id.clone(),
            kind: kind.to_string(),
            session: Some(session.clone()),
            message: None,
        },
    );
}

/// The capabilities `requested` may give the session: everything `caller_caps` covers of
/// them, never more — the same narrowing a minted grant gets. Absent: all of the caller's.
fn narrowed_caps(
    caller_caps: &[Capability],
    requested: Option<Vec<Capability>>,
) -> Vec<Capability> {
    match requested {
        Some(requested) => caps_within(caller_caps, &requested),
        None => caller_caps.to_vec(),
    }
}

/// `agent.sessionCreate`: a session of the caller's agent.
pub fn create(
    ctx: &RequestContext,
    params: AgentSessionCreate,
) -> Result<AgentSession, SessionError> {
    let agent = ctx.acts_as();
    let now = chrono::Utc::now().to_rfc3339();
    let session = AgentSession {
        id: Uuid::new_v4().to_string(),
        owner_did: agent.managed_did().map(str::to_string),
        title: params.title.unwrap_or_default(),
        model: params.model,
        default_perspective: params.default_perspective,
        library_perspective: params.library_perspective,
        capabilities: narrowed_caps(&ctx.session.capabilities(), params.capabilities),
        created_at: now.clone(),
        updated_at: now,
    };
    agent_session_store()
        .create_session(&session)
        .map_err(SessionError::Failed)?;
    publish_session(&agent, "created", &session);
    Ok(session)
}

/// `agent.sessionList`: the caller's sessions.
pub fn list(ctx: &RequestContext) -> Result<Vec<AgentSession>, SessionError> {
    agent_session_store()
        .sessions(ctx.acts_as().managed_did())
        .map_err(SessionError::Failed)
}

/// `agent.session` / the read side: the session `id` of the caller's agent.
pub fn get(ctx: &RequestContext, id: &str) -> Result<AgentSession, SessionError> {
    agent_session_store()
        .session(ctx.acts_as().managed_did(), id)
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(id.to_string()))
}

/// `agent.messages`: the newest `limit` transcript entries of the session, or those older
/// than `before` (`seq`).
pub fn messages(
    ctx: &RequestContext,
    session_id: &str,
    limit: Option<u32>,
    before: Option<i64>,
) -> Result<Vec<AgentSessionMessage>, SessionError> {
    let store = agent_session_store();
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    if store
        .session(owner, session_id)
        .map_err(SessionError::Failed)?
        .is_none()
    {
        return Err(SessionError::NotFound(session_id.to_string()));
    }
    store
        .messages(owner, session_id, limit.unwrap_or(200), before)
        .map_err(SessionError::Failed)
}

/// `agent.sessionUpdate`: the fields `params` names, on the caller's session `id`. The
/// capabilities it sets can only reach what the caller holds.
pub fn update(
    ctx: &RequestContext,
    id: &str,
    params: AgentSessionUpdateParams,
) -> Result<AgentSession, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let update = AgentSessionUpdate {
        title: params.title,
        model: params.model,
        default_perspective: params.default_perspective,
        library_perspective: params.library_perspective,
        capabilities: params
            .capabilities
            .map(|caps| caps_within(&ctx.session.capabilities(), &caps)),
    };
    let store = agent_session_store();
    if !store
        .update_session(owner, id, &update)
        .map_err(SessionError::Failed)?
    {
        return Err(SessionError::NotFound(id.to_string()));
    }
    let session = get(ctx, id)?;
    publish_session(&ctx.acts_as(), "updated", &session);
    Ok(session)
}

/// `agent.sessionDelete`: the session `id` of the caller and its transcript, gone. A turn
/// running on it is cancelled first.
pub fn delete(ctx: &RequestContext, id: &str) -> Result<bool, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = agent_session_store();
    // Whose it is comes first: only its owner's delete may end the turn running on it.
    let session = store
        .session(owner, id)
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(id.to_string()))?;
    // A running turn writes on rows the delete is about to take: end it first.
    let taken = RUNNING.lock().unwrap().remove(id);
    if let Some(turn) = taken {
        if let Some(abort) = turn.abort {
            abort.abort();
        }
    }
    if !store
        .delete_session(owner, id)
        .map_err(SessionError::Failed)?
    {
        return Err(SessionError::NotFound(id.to_string()));
    }
    publish_session(&ctx.acts_as(), "deleted", &session);
    Ok(true)
}

/// `agent.send`: the person's message stored, a `running` assistant row created, the turn
/// started as a detached task — and the answer returned at once. The reply lands on the
/// row and the event stream whenever the model is done; the socket need not stay open.
pub async fn send(
    ctx: &RequestContext,
    session_id: &str,
    text: &str,
    deps: TurnDeps,
) -> Result<AgentSendStarted, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = agent_session_store();
    let session = store
        .session(owner, session_id)
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(session_id.to_string()))?;
    if text.trim().is_empty() {
        return Err(SessionError::Invalid("the message is empty".to_string()));
    }

    // The slot first: no two sends race into two turns, and the per-caller limit counts
    // what is really running.
    let reply_id = Uuid::new_v4().to_string();
    {
        let mut running = RUNNING.lock().unwrap();
        if running.contains_key(session_id) {
            return Err(SessionError::Busy);
        }
        if running.values().filter(|t| t.agent == agent).count() >= MAX_RUNNING_PER_CALLER {
            return Err(SessionError::TooManyTurns);
        }
        running.insert(
            session_id.to_string(),
            RunningTurn {
                reply: reply_id.clone(),
                agent: agent.clone(),
                abort: None,
            },
        );
    }
    // From here the slot is ours; any early return must free it. `SendHold` does on drop.
    let hold = SendHold(session_id.to_string());

    // Rows still `running` here belong to a task that died (a panic lost its task without
    // the sweep having run): they are finished, whatever they say.
    store
        .interrupt_running(Some(session_id))
        .map_err(SessionError::Failed)?;

    let user = store
        .append_message(
            owner,
            &Uuid::new_v4().to_string(),
            session_id,
            AgentSessionMessageRole::User,
            &user_content(text),
            None,
        )
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(session_id.to_string()))?;
    publish_step(&agent, session_id, user.clone());

    let reply = store
        .append_message(
            owner,
            &reply_id,
            session_id,
            AgentSessionMessageRole::Assistant,
            &assistant_content(""),
            Some(AgentTurnStatus::Running),
        )
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(session_id.to_string()))?;
    publish_step(&agent, session_id, reply);

    // The task must not start until its abort handle is in the slot: a cancel that
    // arrives between spawn and registration still ends the turn — it finds the slot
    // gone and the start marks the row `cancelled` itself.
    let (registered, until) = tokio::sync::oneshot::channel::<()>();
    let task_store = store.clone();
    let task_agent = agent.clone();
    let task_session_id = session_id.to_string();
    let task_reply_id = reply_id.clone();
    let caller = ctx.session.clone();
    let run = run_turn(session, caller, agent.clone(), store.clone(), deps);
    let handle = tokio::spawn(async move {
        if until.await.is_err() {
            return;
        }
        // Frees the registry slot on every exit — return, abort, panic alike.
        let _slot = TurnSlot {
            session_id: task_session_id.clone(),
            reply: task_reply_id.clone(),
        };
        let outcome = tokio::time::timeout(TURN_TIMEOUT, run).await;
        if !release(&task_session_id, &task_reply_id) {
            // A cancel or delete took the slot and settled the row already.
            return;
        }
        match outcome {
            Ok(Ok(text)) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AgentTurnStatus::Done,
                None,
                Some(&serde_json::json!({ "text": text })),
            ),
            Ok(Err(e)) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AgentTurnStatus::Failed,
                Some(&format!("{e:#}")),
                None,
            ),
            Err(_elapsed) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AgentTurnStatus::Failed,
                Some("the turn ran out of time"),
                None,
            ),
        }
    });

    let mut running = RUNNING.lock().unwrap();
    match running.get_mut(session_id) {
        Some(turn) if turn.reply == reply_id => {
            turn.abort = Some(handle.abort_handle());
            let _ = registered.send(());
        }
        // The slot is gone: a cancel (or delete) landed between spawn and here. The task
        // waits on `until` — drop it without ever starting, and settle the row.
        _ => {
            drop(registered);
            handle.abort();
            drop(running);
            settle(
                &store,
                &agent,
                &reply_id,
                AgentTurnStatus::Cancelled,
                Some("the turn was cancelled before it started"),
                None,
            );
        }
    }
    std::mem::forget(hold);
    Ok(AgentSendStarted {
        message_id: user.id,
        reply_id,
    })
}

/// `agent.cancel`: the turn running on the caller's session `session_id`, ended. `false`
/// when nothing is running.
pub fn cancel(ctx: &RequestContext, session_id: &str) -> Result<bool, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = agent_session_store();
    if store
        .session(owner, session_id)
        .map_err(SessionError::Failed)?
        .is_none()
    {
        return Err(SessionError::NotFound(session_id.to_string()));
    }
    let taken = RUNNING.lock().unwrap().remove(session_id);
    match taken {
        Some(turn) => match turn.abort {
            Some(abort) => {
                abort.abort();
                settle(
                    &store,
                    &turn.agent,
                    &turn.reply,
                    AgentTurnStatus::Cancelled,
                    Some("the caller cancelled the turn"),
                    None,
                );
                Ok(true)
            }
            // The task was spawned but never registered: it waits on `until` and its
            // start will settle the row when it finds the slot gone.
            None => Ok(true),
        },
        None => Ok(store
            .interrupt_running(Some(session_id))
            .map_err(SessionError::Failed)?
            > 0),
    }
}

/// The startup sweep: every turn a previous run of this executor left `running` was cut
/// off with the process — mark them so a client never waits on a dead row. Called once
/// from `run()` after the database opens.
pub fn startup_sweep() {
    match agent_session_store().interrupt_running(None) {
        Ok(0) => {}
        Ok(n) => log::warn!("agent sessions: marked {n} turns interrupted that a restart cut off"),
        Err(e) => log::error!("agent sessions: the startup sweep failed: {e}"),
    }
}

/// Frees the session's slot if it is still this task's. `true` when the task still owns
/// it and may settle the row; `false` when a cancel or delete took it and settled.
fn release(session_id: &str, reply: &str) -> bool {
    let mut running = RUNNING.lock().unwrap();
    match running.get(session_id) {
        Some(turn) if turn.reply == reply => {
            running.remove(session_id);
            true
        }
        _ => false,
    }
}

/// The turn's end written on its row and told to the owner: `status`, `error`, and
/// `content_patch` merged into the row's content.
fn settle(
    store: &Arc<dyn AgentSessionStore>,
    agent: &AgentContext,
    reply_id: &str,
    status: AgentTurnStatus,
    error: Option<&str>,
    content_patch: Option<&serde_json::Value>,
) {
    let owner = agent.managed_did();
    if let Err(e) = store.finish_message(owner, reply_id, status, error, content_patch) {
        log::error!("agent sessions: could not settle turn {reply_id}: {e}");
        return;
    }
    match store.message(owner, reply_id) {
        Ok(Some(row)) => publish_step(agent, &row.session_id.clone(), row),
        Ok(None) => log::warn!("agent sessions: turn {reply_id} settled but its row is gone"),
        Err(e) => log::error!("agent sessions: could not read the settled turn {reply_id}: {e}"),
    }
}

/// Frees the session's slot on drop: a `send` that fails after reserving leaves no
/// phantom turn behind.
struct SendHold(String);

impl Drop for SendHold {
    fn drop(&mut self) {
        RUNNING.lock().unwrap().remove(&self.0);
    }
}

/// For tests: the running-turn table a test inspects.
#[cfg(test)]
pub(crate) fn running_for_test(session_id: &str) -> bool {
    RUNNING.lock().unwrap().contains_key(session_id)
}

#[cfg(test)]
pub(crate) fn clear_running_for_test() {
    RUNNING.lock().unwrap().clear();
}
