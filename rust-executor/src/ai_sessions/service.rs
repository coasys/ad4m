//! The session service: the calls `ai.session*` make, and the registry of the turns
//! they start.
//!
//! A send stores the person's message and a `running` assistant row, hands the turn to a
//! detached tokio task, and returns at once — the answer lands on the row (and on the
//! `ai-session-changed` event) whenever it is ready; the caller's socket may be gone by
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
use super::store::{ai_session_store, AiSessionStore};
use super::types::{
    assistant_content, user_content, AiSession, AiSessionChanged, AiSessionMessage,
    AiSessionMessageRole, AiSessionUpdate, AiTurnStatus,
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
    #[error("AI session {0} not found")]
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

/// `ai.sessionSend`'s answer: the ids of the person's message and of the `running` assistant
/// row the detached task writes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionSendStarted")]
pub struct AiSessionSendStarted {
    pub message_id: String,
    pub reply_id: String,
}

/// What `ai.sessionCreate` takes.
#[derive(Debug, Clone, Default, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionCreate")]
pub struct AiSessionCreate {
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

/// What `ai.sessionUpdate` takes. A field left out keeps its value; a nullable one is
/// cleared with `null` — the `Option<Option<..>>` serde shape, which plain serde collapses
/// to `None` either way, so each one takes [`triple_option`].
#[derive(Debug, Clone, Default, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionUpdateParams")]
pub struct AiSessionUpdateParams {
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

/// The send-vs-delete serializer: one lock per session id. A send holds its session's
/// across admission — the session read, the slot take, the transcript rows and the task
/// registration — and a delete holds it across the slot clear, the transcript delete
/// and the task abort, so a send can no longer slip a turn start into the gap a delete
/// opens between clearing [`RUNNING`] and committing: the send either finished before
/// the delete (its turn ends with the session) or starts after (and answers not-found
/// like any send on a deleted session). A `std::sync::Mutex`: no work under it awaits.
/// An entry leaves with its session — a delete drops it, and a send that finds the
/// session gone drops the one it took. Keyed by owner and id: a not-found is a verdict
/// on the asker's sessions only, so another owner's miss on the same id retires nothing
/// of this owner's.
static LIFECYCLE: Lazy<Mutex<HashMap<LifecycleKey, Arc<Mutex<()>>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

type LifecycleKey = (Option<String>, String);

fn lifecycle_key(owner: Option<&str>, session_id: &str) -> LifecycleKey {
    (owner.map(str::to_string), session_id.to_string())
}

/// The session's lifecycle lock (see [`LIFECYCLE`]), creating its entry on first use.
fn lifecycle_lock(owner: Option<&str>, session_id: &str) -> Arc<Mutex<()>> {
    LIFECYCLE
        .lock()
        .unwrap()
        .entry(lifecycle_key(owner, session_id))
        .or_default()
        .clone()
}

/// Drops the session's lifecycle entry when it is still `lock`'s — nothing keeps a lock
/// for a session that is gone. The caller holds it, so a send waiting on it wakes to the
/// same not-found.
fn retire_lifecycle_lock(owner: Option<&str>, session_id: &str, lock: &Arc<Mutex<()>>) {
    let key = lifecycle_key(owner, session_id);
    let mut lifecycle = LIFECYCLE.lock().unwrap();
    if lifecycle.get(&key).is_some_and(|l| Arc::ptr_eq(l, lock)) {
        lifecycle.remove(&key);
    }
}

/// The row's new state published to its owner, so a watching client draws each message,
/// status change and step as it lands.
fn publish(agent: &AgentContext, changed: AiSessionChanged) {
    get_global_pubsub_sync().publish_event(Event::new(
        Topic::AiSessionChanged,
        Audience::of(agent),
        changed,
    ));
}

/// The `ai-session-changed` event of a message row (`kind: "message"`), to the
/// session's owner. The observer calls this for every step it writes.
pub(crate) fn publish_step(agent: &AgentContext, session_id: &str, message: AiSessionMessage) {
    publish(
        agent,
        AiSessionChanged {
            session_id: session_id.to_string(),
            kind: "message".to_string(),
            session: None,
            message: Some(message),
        },
    );
}

/// A session row event (`created`, `updated`, `deleted`), to its owner.
fn publish_session(agent: &AgentContext, kind: &str, session: &AiSession) {
    publish(
        agent,
        AiSessionChanged {
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

/// `ai.sessionCreate`: a session of the caller's agent.
pub fn create(ctx: &RequestContext, params: AiSessionCreate) -> Result<AiSession, SessionError> {
    let agent = ctx.acts_as();
    let now = chrono::Utc::now().to_rfc3339();
    let session = AiSession {
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
    ai_session_store()
        .create_session(&session)
        .map_err(SessionError::Failed)?;
    publish_session(&agent, "created", &session);
    Ok(session)
}

/// `ai.sessionList`: the caller's sessions.
pub fn list(ctx: &RequestContext) -> Result<Vec<AiSession>, SessionError> {
    ai_session_store()
        .sessions(ctx.acts_as().managed_did())
        .map_err(SessionError::Failed)
}

/// `ai.session` / the read side: the session `id` of the caller's agent.
pub fn get(ctx: &RequestContext, id: &str) -> Result<AiSession, SessionError> {
    ai_session_store()
        .session(ctx.acts_as().managed_did(), id)
        .map_err(SessionError::Failed)?
        .ok_or_else(|| SessionError::NotFound(id.to_string()))
}

/// `ai.sessionMessages`: the newest `limit` transcript entries of the session, or those older
/// than `before` (`seq`).
pub fn messages(
    ctx: &RequestContext,
    session_id: &str,
    limit: Option<u32>,
    before: Option<i64>,
) -> Result<Vec<AiSessionMessage>, SessionError> {
    let store = ai_session_store();
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

/// `ai.sessionUpdate`: the fields `params` names, on the caller's session `id`. The
/// capabilities it sets can only reach what the caller holds.
pub fn update(
    ctx: &RequestContext,
    id: &str,
    params: AiSessionUpdateParams,
) -> Result<AiSession, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let update = AiSessionUpdate {
        title: params.title,
        model: params.model,
        default_perspective: params.default_perspective,
        library_perspective: params.library_perspective,
        capabilities: params
            .capabilities
            .map(|caps| caps_within(&ctx.session.capabilities(), &caps)),
    };
    let store = ai_session_store();
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

/// `ai.sessionDelete`: the session `id` of the caller and its transcript, gone. A turn
/// running on it is cancelled first.
pub fn delete(ctx: &RequestContext, id: &str) -> Result<bool, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = ai_session_store();
    // Admission and teardown on one session are mutually exclusive: a send may not slip
    // a turn into the gap between the slot clear below and the transcript delete.
    let lifecycle = lifecycle_lock(owner, id);
    let _guard = lifecycle.lock().unwrap();
    // Whose it is comes first: only its owner's delete may end the turn running on it.
    let Some(session) = store.session(owner, id).map_err(SessionError::Failed)? else {
        retire_lifecycle_lock(owner, id, &lifecycle);
        return Err(SessionError::NotFound(id.to_string()));
    };
    // A running turn writes on rows the delete is about to take: end it first.
    let taken = RUNNING.lock().unwrap().remove(id);
    if let Some(turn) = taken {
        if let Some(abort) = turn.abort {
            abort.abort();
        }
    }
    // A test parks the delete here to run a send inside the gap above.
    #[cfg(test)]
    wait_at_delete_pause();
    if !store
        .delete_session(owner, id)
        .map_err(SessionError::Failed)?
    {
        retire_lifecycle_lock(owner, id, &lifecycle);
        return Err(SessionError::NotFound(id.to_string()));
    }
    publish_session(&ctx.acts_as(), "deleted", &session);
    retire_lifecycle_lock(owner, id, &lifecycle);
    Ok(true)
}

/// `ai.sessionSend`: the person's message stored, a `running` assistant row created, the turn
/// started as a detached task — and the answer returned at once. The reply lands on the
/// row and the event stream whenever the model is done; the socket need not stay open.
pub async fn send(
    ctx: &RequestContext,
    session_id: &str,
    text: &str,
    deps: TurnDeps,
) -> Result<AiSessionSendStarted, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = ai_session_store();
    // Admission holds the session's lifecycle lock through the read, the slot take and
    // the task registration below: a delete may not land between them. It is a std
    // mutex across synchronous work only — nothing under it awaits.
    let lifecycle = lifecycle_lock(owner, session_id);
    let _guard = lifecycle.lock().unwrap();
    let Some(session) = store
        .session(owner, session_id)
        .map_err(SessionError::Failed)?
    else {
        retire_lifecycle_lock(owner, session_id, &lifecycle);
        return Err(SessionError::NotFound(session_id.to_string()));
    };
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
    let hold = SendHold {
        session_id: session_id.to_string(),
        reply: reply_id.clone(),
        armed: true,
    };

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
            AiSessionMessageRole::User,
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
            AiSessionMessageRole::Assistant,
            &assistant_content(""),
            Some(AiTurnStatus::Running),
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
    let timeout = deps.timeout.unwrap_or(TURN_TIMEOUT);
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
        let outcome = tokio::time::timeout(timeout, run).await;
        if !release(&task_session_id, &task_reply_id) {
            // A cancel or delete took the slot and settled the row already.
            return;
        }
        match outcome {
            Ok(Ok(text)) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AiTurnStatus::Done,
                None,
                Some(&serde_json::json!({ "text": text })),
            ),
            Ok(Err(e)) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AiTurnStatus::Failed,
                Some(&format!("{e:#}")),
                None,
            ),
            Err(_elapsed) => settle(
                &task_store,
                &task_agent,
                &task_reply_id,
                AiTurnStatus::Failed,
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
                AiTurnStatus::Cancelled,
                Some("the turn was cancelled before it started"),
                None,
            );
        }
    }
    hold.keep();
    Ok(AiSessionSendStarted {
        message_id: user.id,
        reply_id,
    })
}

/// `ai.sessionCancel`: the turn running on the caller's session `session_id`, ended. `false`
/// when nothing is running.
pub fn cancel(ctx: &RequestContext, session_id: &str) -> Result<bool, SessionError> {
    let agent = ctx.acts_as();
    let owner = agent.managed_did();
    let store = ai_session_store();
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
                    AiTurnStatus::Cancelled,
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
    match ai_session_store().interrupt_running(None) {
        Ok(0) => {}
        Ok(n) => log::warn!("ai sessions: marked {n} turns interrupted that a restart cut off"),
        Err(e) => log::error!("ai sessions: the startup sweep failed: {e}"),
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
    store: &Arc<dyn AiSessionStore>,
    agent: &AgentContext,
    reply_id: &str,
    status: AiTurnStatus,
    error: Option<&str>,
    content_patch: Option<&serde_json::Value>,
) {
    let owner = agent.managed_did();
    if let Err(e) = store.finish_message(owner, reply_id, status, error, content_patch) {
        log::error!("ai sessions: could not settle turn {reply_id}: {e}");
        return;
    }
    match store.message(owner, reply_id) {
        Ok(Some(row)) => publish_step(agent, &row.session_id.clone(), row),
        Ok(None) => log::warn!("ai sessions: turn {reply_id} settled but its row is gone"),
        Err(e) => log::error!("ai sessions: could not read the settled turn {reply_id}: {e}"),
    }
}

/// Frees the slot a `send` reserved on drop: a `send` that fails after reserving leaves no
/// phantom turn behind. Only its own slot — a cancel may have taken it since, and a newer
/// send's slot for the same session is not this one's to free.
struct SendHold {
    session_id: String,
    reply: String,
    armed: bool,
}

impl SendHold {
    /// The send went through: the slot stays with the turn it started.
    fn keep(mut self) {
        self.armed = false;
    }
}

impl Drop for SendHold {
    fn drop(&mut self) {
        if self.armed {
            release(&self.session_id, &self.reply);
        }
    }
}

/// Arms a pause inside `delete` for a test, between the session's running-turn slot
/// being cleared and its rows being deleted — the gap a send once slipped a turn
/// start into. One-shot: the next delete consumes the pair, sets `reached` and spins
/// on `open` there.
#[cfg(test)]
static DELETE_PAUSE: Lazy<
    Mutex<
        Option<(
            Arc<std::sync::atomic::AtomicBool>,
            Arc<std::sync::atomic::AtomicBool>,
        )>,
    >,
> = Lazy::new(|| Mutex::new(None));

/// Arms the pause inside `delete`. One-shot, so a test arms it again for another go.
#[cfg(test)]
pub(crate) fn pause_delete_before_commit() -> (
    Arc<std::sync::atomic::AtomicBool>,
    Arc<std::sync::atomic::AtomicBool>,
) {
    let reached = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let open = Arc::new(std::sync::atomic::AtomicBool::new(false));
    *DELETE_PAUSE.lock().unwrap() = Some((reached.clone(), open.clone()));
    (reached, open)
}

/// Where `delete` parks for [`pause_delete_before_commit`].
#[cfg(test)]
fn wait_at_delete_pause() {
    use std::sync::atomic::Ordering;
    if let Some((reached, open)) = DELETE_PAUSE.lock().unwrap().take() {
        reached.store(true, Ordering::SeqCst);
        while !open.load(Ordering::SeqCst) {
            std::thread::sleep(Duration::from_millis(1));
        }
    }
}

/// For tests: the running-turn table a test inspects.
#[cfg(test)]
pub(crate) fn running_for_test(session_id: &str) -> bool {
    RUNNING.lock().unwrap().contains_key(session_id)
}

/// For tests: whether the lifecycle table holds a lock for `owner`'s session.
#[cfg(test)]
pub(crate) fn lifecycle_entry_for_test(owner: Option<&str>, session_id: &str) -> bool {
    LIFECYCLE
        .lock()
        .unwrap()
        .contains_key(&lifecycle_key(owner, session_id))
}

#[cfg(test)]
pub(crate) fn clear_running_for_test() {
    RUNNING.lock().unwrap().clear();
    LIFECYCLE.lock().unwrap().clear();
}

#[cfg(test)]
mod slot_tests {
    use super::*;

    fn reserve(session_id: &str, reply: &str) {
        RUNNING.lock().unwrap().insert(
            session_id.to_string(),
            RunningTurn {
                reply: reply.to_string(),
                agent: AgentContext::main_agent(),
                abort: None,
            },
        );
    }

    fn hold(session_id: &str, reply: &str) -> SendHold {
        SendHold {
            session_id: session_id.to_string(),
            reply: reply.to_string(),
            armed: true,
        }
    }

    // A send that fails after a cancel and a second send took its place must leave the
    // second send's slot: without it that turn could run beside a third and never settle.
    #[test]
    fn a_failed_send_frees_its_own_slot_and_no_newer_one() {
        let session = format!("slot-{}", Uuid::new_v4());
        reserve(&session, "second");
        drop(hold(&session, "first"));
        assert!(running_for_test(&session), "the newer send keeps its slot");

        // A send that went through keeps the slot for its turn.
        hold(&session, "second").keep();
        assert!(running_for_test(&session), "a kept slot stays");

        drop(hold(&session, "second"));
        assert!(!running_for_test(&session), "its own slot is freed");
    }
}
