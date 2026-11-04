//! Agent replies: an agent's turn in a conversation that an app keeps.
//!
//! `perspective.runAgent` runs a session on the request and keeps nothing: the client sends the
//! whole conversation every time and gets the answer back on the same call, so the answer is lost
//! when the client goes away and no other device sees it. An agent reply is the stateful form.
//!
//! **The conversation is the container's.** Any node can hold a conversation (the built-in
//! `Container`, #1446): an app's chat record, a channel. Its messages are its children that carry
//! text (`TextContent`), whatever class the app writes them in. The executor writes only its own
//! turns, as `AgentReply` records: a `TextContent` (its `text`), so any reader of the container
//! sees them as messages, plus a `status`.
//!
//! **A reply runs on the node.** [`start_reply`] creates the reply as `running` under the
//! container, returns its id, and runs the agent in a task of its own. The task sets the text and
//! `done`, or `failed` with the error. Closing the client changes nothing: the task holds the
//! caller's session, which lasts as long as its grant, not its socket. Clients follow a reply with
//! an ordinary query subscription on `AgentReply` in the container.
//!
//! **Which messages are the request.** The caller's own messages are the person's turns, and the
//! caller's replies that finished are the agent's. Anything else in the container (another
//! person's message, another person's reply, a reply that failed) is left out. "The caller's"
//! is read per link: a message counts only when the caller wrote its `text` and nobody else did,
//! and a reply only when the same holds for its `status` and `text`. In a shared container
//! another member can write a newer `text` onto the caller's message, and a record's `author`
//! (its earliest link's) would still name the caller. So nothing another member writes reaches
//! the model as a turn of the conversation. What the agent's tools read in `workIn` is another
//! matter: that is the data it works on.
//!
//! **One at a time, per caller.** A caller has at most one reply running in a container
//! ([`RunningReplies`], held per perspective): the conversation is the caller's, so two people
//! in one shared container each get their own. [`cancel_reply`] stops the caller's own reply and
//! no one else's. A reply still `running` when the executor starts again was cut off: the sweep
//! at load marks it `interrupted` (`perspective_instance::agent_replies`). Nothing else marks a
//! reply it did not start: a `running` reply in the container may be another node's, still
//! working.

use crate::agent::AgentContext;
use crate::ai_service::harness::ToolObserver;
use crate::perspectives::agent_reply_steps::{
    ensure_agent_step_class, StepWriter, AGENT_STEP_PREDICATE,
};
use crate::perspectives::agent_session::{AgentMessage, AgentReply, AgentSessionError};
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::model_query::builtin_classes::TEXT_CONTENT_URI;
use crate::perspectives::model_query::containment::HAS_CHILD;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkStatus};
use futures::future::BoxFuture;
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Duration;

/// The class's local name, as `create_subject` and `model_query` take it.
pub const AGENT_REPLY_CLASS: &str = "AgentReply";
/// The class's URI: what a perspective registers it under.
pub const AGENT_REPLY_TARGET_CLASS: &str = "ad4m://AgentReply";
const AGENT_REPLY_SDNA: &str = include_str!("hardwired_sdna/agent_reply.json");

/// Where reply ids are minted: outside any app's namespace, like the interpretation run nodes.
const REPLY_URI_PREFIX: &str = "ad4m://agent_reply/";

/// How long a reply may run. Long enough for an agent that reads documentation and edits a large
/// record over many calls, as for `perspective.runAgent`.
pub const REPLY_TIMEOUT: Duration = Duration::from_secs(900);

/// How many of the container's newest messages make up the conversation.
const CONVERSATION_LIMIT: usize = 200;

/// Where a reply is. Only `running` changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplyStatus {
    Running,
    Done,
    Failed,
    Cancelled,
    Interrupted,
}

impl ReplyStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            ReplyStatus::Running => "running",
            ReplyStatus::Done => "done",
            ReplyStatus::Failed => "failed",
            ReplyStatus::Cancelled => "cancelled",
            ReplyStatus::Interrupted => "interrupted",
        }
    }
}

/// The reply running in a container: its id, and how to stop it once its task has started.
#[derive(Debug)]
pub struct RunningReply {
    pub reply: String,
    abort: Option<tokio::task::AbortHandle>,
}

impl RunningReply {
    /// Stop its task, if it has started.
    pub(crate) fn abort(&self) {
        if let Some(abort) = &self.abort {
            abort.abort();
        }
    }
}

/// Whose reply a slot holds: the container, and the caller's DID.
pub type ReplySlot = (String, String);

/// The replies running in one perspective, by container and caller. A std mutex: held only to
/// read or swap an entry, never across an await.
pub type RunningReplies = HashMap<ReplySlot, RunningReply>;

/// Runs the agent on the conversation, telling `observer` about each tool call (the reply's
/// steps): in the executor, `run_agent_session` with the caller's session; in tests, a script.
pub type Runner = Box<
    dyn FnOnce(
            Vec<AgentMessage>,
            Arc<dyn ToolObserver>,
        ) -> BoxFuture<'static, Result<AgentReply, AgentSessionError>>
        + Send,
>;

#[derive(Debug, thiserror::Error)]
pub enum ReplyError {
    /// The request can't be answered as it is: nothing to answer, or no container.
    #[error("{0}")]
    Invalid(String),
    /// The caller already has a reply running in this container.
    #[error("a reply is already running in this conversation: wait for it, or cancel it")]
    Busy,
    #[error(transparent)]
    Failed(#[from] anyhow::Error),
}

/// Register the classes in the perspective, once: the reply and its steps. A reply class
/// registered before it had `steps` is refreshed.
pub async fn ensure_agent_reply_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        AGENT_REPLY_CLASS,
        AGENT_REPLY_TARGET_CLASS,
        AGENT_REPLY_SDNA,
        Some(AGENT_STEP_PREDICATE),
        context,
    )
    .await?;
    ensure_agent_step_class(perspective, context).await
}

/// Start a reply in `container`, as `caller_did` acting through `context`: create it as
/// `running`, start `run` on the conversation, and return the reply's id without waiting.
pub async fn start_reply(
    mut perspective: PerspectiveInstance,
    container: &str,
    caller_did: &str,
    context: AgentContext,
    run: Runner,
) -> Result<String, ReplyError> {
    let container = container.trim();
    if container.is_empty() {
        return Err(ReplyError::Invalid(
            "`container` is empty: name the node that holds the conversation".into(),
        ));
    }
    ensure_agent_reply_class(&mut perspective, &context).await?;

    // Hold the caller's slot in the container from here, so two calls can't both start a reply.
    let slot: ReplySlot = (container.to_string(), caller_did.to_string());
    let reply = format!("{REPLY_URI_PREFIX}{}", uuid::Uuid::new_v4());
    reserve(&perspective, &slot, &reply)?;
    // The request can be dropped at any await from here (a client's `request.cancel`, a session
    // that ends): the hold gives the slot back then, and on an error. The task takes it over
    // below, with no await in between.
    let hold = SlotHold {
        perspective: perspective.clone(),
        slot: slot.clone(),
        reply: reply.clone(),
    };
    let messages =
        prepare_and_create(&mut perspective, container, caller_did, &reply, &context).await?;
    std::mem::forget(hold);

    let cancel_context = context.clone();
    let task_perspective = perspective.clone();
    let task_slot = slot.clone();
    let task_reply = reply.clone();
    let steps: Arc<dyn ToolObserver> = Arc::new(StepWriter::new(
        perspective.clone(),
        reply.clone(),
        context.clone(),
    ));
    // The task waits until its abort handle is in the slot. Otherwise a runner that answers at
    // once releases the slot first, and the check below reads the empty slot as a cancel.
    let (registered, until_registered) = tokio::sync::oneshot::channel::<()>();
    let handle = tokio::spawn(async move {
        if until_registered.await.is_err() {
            return;
        }
        let outcome = match tokio::time::timeout(REPLY_TIMEOUT, run(messages, steps)).await {
            Ok(outcome) => outcome,
            Err(_) => Err(AgentSessionError::Failed(anyhow::anyhow!(
                "the agent did not answer within {} s",
                REPLY_TIMEOUT.as_secs()
            ))),
        };
        // Whoever takes the slot writes the end: this task, or a cancel that got there first.
        if !release(&task_perspective, &task_slot, &task_reply) {
            return;
        }
        let values = match outcome {
            Ok(answer) => json!({ "status": ReplyStatus::Done.as_str(), "text": answer.text }),
            Err(e) => json!({ "status": ReplyStatus::Failed.as_str(), "error": e.to_string() }),
        };
        settle(task_perspective, &task_reply, values, &context).await;
    });
    let cancelled_meanwhile = {
        let mut running = perspective
            .agent_replies
            .lock()
            .expect("agent replies poisoned");
        match running.get_mut(&slot) {
            Some(entry) if entry.reply == reply => {
                entry.abort = Some(handle.abort_handle());
                false
            }
            _ => true,
        }
    };
    if !cancelled_meanwhile {
        let _ = registered.send(());
    } else {
        // A cancel took the slot before the task was registered, so it could not stop it or
        // mark the reply: both are done here. Dropping `registered` ends the task unrun.
        drop(registered);
        handle.abort();
        settle(
            perspective,
            &reply,
            json!({ "status": ReplyStatus::Cancelled.as_str() }),
            &cancel_context,
        )
        .await;
    }
    Ok(reply)
}

/// Stop the reply `caller_did` has running in `container` and mark it `cancelled`. `false` when
/// the caller has none running: another caller's reply in the same container is theirs to
/// cancel. What the agent wrote before it was stopped stays written.
pub async fn cancel_reply(
    perspective: PerspectiveInstance,
    container: &str,
    caller_did: &str,
    context: &AgentContext,
) -> anyhow::Result<bool> {
    let slot: ReplySlot = (container.trim().to_string(), caller_did.to_string());
    let taken = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned")
        .remove(&slot);
    let Some(running) = taken else {
        return Ok(false);
    };
    let Some(abort) = running.abort else {
        // Its task hasn't started yet. `start_reply` finds its slot gone once it has, then stops
        // the task and marks the reply.
        return Ok(true);
    };
    abort.abort();
    settle(
        perspective,
        &running.reply,
        json!({ "status": ReplyStatus::Cancelled.as_str() }),
        context,
    )
    .await;
    Ok(true)
}

/// Stop every reply running in `container` on this node, whoever started it, without marking
/// them: for a caller that removes the replies next (deleting a session).
pub(crate) fn stop_replies_in(perspective: &PerspectiveInstance, container: &str) {
    let mut running = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned");
    running.retain(|(c, _), reply| {
        if c == container {
            reply.abort();
            false
        } else {
            true
        }
    });
}

/// How long [`settle`] waits before it tries a failed write again.
const SETTLE_RETRY: Duration = Duration::from_secs(1);

/// Write how a reply ended, trying once more if the write fails. A reply whose end is never
/// written reads as `running` until the sweep at the next load; the error is logged.
async fn settle(
    perspective: PerspectiveInstance,
    reply: &str,
    values: Value,
    context: &AgentContext,
) {
    let Err(first) = set_reply(perspective.clone(), reply, values.clone(), context).await else {
        return;
    };
    log::warn!("agent reply {reply}: could not record how it ended, trying again: {first:#}");
    tokio::time::sleep(SETTLE_RETRY).await;
    if let Err(e) = set_reply(perspective, reply, values, context).await {
        log::error!(
            "agent reply {reply}: could not record how it ended; it reads as running until the \
             sweep at the next load: {e:#}"
        );
    }
}

/// Gives a reserved slot back when dropped: on an error, or when the request is dropped before
/// the reply's task holds the slot. `std::mem::forget` it once the task does.
struct SlotHold {
    perspective: PerspectiveInstance,
    slot: ReplySlot,
    reply: String,
}

impl Drop for SlotHold {
    fn drop(&mut self) {
        release(&self.perspective, &self.slot, &self.reply);
    }
}

/// Read the conversation, refuse one with nothing to answer, and create the new reply under the
/// container.
async fn prepare_and_create(
    perspective: &mut PerspectiveInstance,
    container: &str,
    caller_did: &str,
    reply: &str,
    context: &AgentContext,
) -> Result<Vec<AgentMessage>, ReplyError> {
    let replies = replies_in(perspective, container, caller_did).await?;

    let query = json!({
        "parent": { "id": container },
        "where": written_only_by(caller_did, &["text"]),
        "order": [["timestamp", "desc"]],
        "limit": CONVERSATION_LIMIT,
    });
    let mut rows = query_rows(perspective, TEXT_CONTENT_URI, &query).await?;
    rows.reverse();
    let messages = conversation(&rows, &replies);
    match messages.last() {
        Some(last) if last.role == "user" => {}
        _ => {
            return Err(ReplyError::Invalid(
                "there is nothing to answer: write the person's message into the container \
                 before asking for a reply"
                    .into(),
            ))
        }
    }

    let batch = perspective.create_batch().await;
    if let Err(e) = stage_reply(perspective, container, reply, &batch, context).await {
        let _ = perspective.discard_batch(&batch).await;
        return Err(e.into());
    }
    perspective
        .commit_batch(batch, context)
        .await
        .map_err(|e| anyhow::anyhow!("could not save the reply: {e:#}"))?;
    Ok(messages)
}

/// The reply and its place in the conversation, staged in `batch`.
async fn stage_reply(
    perspective: &mut PerspectiveInstance,
    container: &str,
    reply: &str,
    batch: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(AGENT_REPLY_CLASS.to_string()),
                query: None,
            },
            reply.to_string(),
            Some(json!({ "status": ReplyStatus::Running.as_str() })),
            Some(batch.to_string()),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not create the reply: {e:#}"))?;
    perspective
        .add_link(
            Link {
                source: container.to_string(),
                predicate: Some(HAS_CHILD.to_string()),
                target: reply.to_string(),
            },
            LinkStatus::Shared,
            Some(batch.to_string()),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not put the reply in the conversation: {e:#}"))?;
    Ok(())
}

/// A reply as the conversation reads it. `trusted` when the caller wrote all of it: the reply
/// itself and every `status` and `text` link on it.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ReplyRow {
    pub status: String,
    pub text: String,
    pub trusted: bool,
}

/// A `where` clause for records whose `properties` `did` wrote and nobody else did. A property
/// reads its latest link whoever wrote it, and a record's `author` is only its earliest link's,
/// so checking the author would let another member of a shared perspective change the words of
/// the caller's own message. Each condition is on the property's own links, answered in the
/// store.
fn written_only_by(did: &str, properties: &[&str]) -> Value {
    let conditions: Vec<Value> = properties
        .iter()
        .flat_map(|p| {
            [
                json!({ *p: { "author": did } }),
                json!({ "NOT": { *p: { "author": { "not": did } } } }),
            ]
        })
        .collect();
    json!({ "AND": conditions })
}

/// The container's replies, by id, each marked whether the caller wrote all of it.
async fn replies_in(
    perspective: &PerspectiveInstance,
    container: &str,
    caller_did: &str,
) -> anyhow::Result<HashMap<String, ReplyRow>> {
    let ids = |rows: &[Value]| -> HashSet<String> {
        rows.iter()
            .filter_map(|row| row.get("id").and_then(Value::as_str).map(str::to_string))
            .collect()
    };
    let trusted = ids(&query_rows(
        perspective,
        AGENT_REPLY_CLASS,
        &json!({
            "parent": { "id": container },
            "where": written_only_by(caller_did, &["status", "text"]),
        }),
    )
    .await?);
    let rows = query_rows(
        perspective,
        AGENT_REPLY_CLASS,
        &json!({ "parent": { "id": container } }),
    )
    .await?;
    Ok(rows
        .iter()
        .filter_map(|row| {
            let id = row.get("id").and_then(Value::as_str)?;
            Some((
                id.to_string(),
                ReplyRow {
                    status: string_field(row, "status"),
                    text: string_field(row, "text"),
                    trusted: trusted.contains(id),
                },
            ))
        })
        .collect())
}

/// The conversation the model is given, oldest first, from `rows`: the container's messages
/// the caller wrote alone (see [`written_only_by`]). The caller's messages are the person's
/// turns, the caller's finished replies the agent's, and nothing else: another reply is never a
/// message, so a reply somebody else wrote can't be read as the caller's.
pub(crate) fn conversation(
    rows: &[Value],
    replies: &HashMap<String, ReplyRow>,
) -> Vec<AgentMessage> {
    rows.iter()
        .filter_map(|row| {
            let id = row.get("id").and_then(Value::as_str).unwrap_or_default();
            if let Some(reply) = replies.get(id) {
                let finished = reply.status == ReplyStatus::Done.as_str();
                return (reply.trusted && finished && !reply.text.trim().is_empty()).then(|| {
                    AgentMessage {
                        role: "assistant".into(),
                        content: reply.text.clone(),
                    }
                });
            }
            let text = string_field(row, "text");
            (!text.trim().is_empty()).then(|| AgentMessage {
                role: "user".into(),
                content: text,
            })
        })
        .collect()
}

/// Write `values` onto a reply through its class setters.
pub(crate) async fn set_reply(
    mut perspective: PerspectiveInstance,
    reply: &str,
    values: Value,
    context: &AgentContext,
) -> anyhow::Result<()> {
    perspective
        .update_subject(
            SubjectClassOption {
                class_name: Some(AGENT_REPLY_CLASS.to_string()),
                query: None,
            },
            reply.to_string(),
            values,
            None,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not update reply {reply}: {e:#}"))
}

/// The replies still marked `running` in a perspective, by the agents in `hosted`, that have no
/// task here, each with its author. Used by the sweep at load. Replies by agents of other nodes
/// are theirs to settle.
pub(crate) async fn stale_running_replies(
    perspective: &PerspectiveInstance,
    hosted: &HashSet<String>,
) -> anyhow::Result<Vec<(String, String)>> {
    let rows = query_rows(
        perspective,
        AGENT_REPLY_CLASS,
        &json!({ "where": { "status": ReplyStatus::Running.as_str() } }),
    )
    .await?;
    let running: HashSet<String> = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned")
        .values()
        .map(|r| r.reply.clone())
        .collect();
    Ok(rows
        .iter()
        .filter_map(|row| {
            let id = row.get("id").and_then(Value::as_str)?;
            let author = row.get("author").and_then(Value::as_str)?;
            (hosted.contains(author) && !running.contains(id))
                .then(|| (id.to_string(), author.to_string()))
        })
        .collect())
}

fn reserve(
    perspective: &PerspectiveInstance,
    slot: &ReplySlot,
    reply: &str,
) -> Result<(), ReplyError> {
    let mut running = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned");
    if running.contains_key(slot) {
        return Err(ReplyError::Busy);
    }
    running.insert(
        slot.clone(),
        RunningReply {
            reply: reply.to_string(),
            abort: None,
        },
    );
    Ok(())
}

/// Give up the slot if `reply` still holds it. `true` if it did.
fn release(perspective: &PerspectiveInstance, slot: &ReplySlot, reply: &str) -> bool {
    let mut running = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned");
    match running.get(slot) {
        Some(entry) if entry.reply == reply => {
            running.remove(slot);
            true
        }
        _ => false,
    }
}

async fn query_rows(
    perspective: &PerspectiveInstance,
    class: &str,
    query: &Value,
) -> anyhow::Result<Vec<Value>> {
    let raw = perspective
        .model_query(class, &query.to_string())
        .await
        .map_err(|e| anyhow::anyhow!("could not read {class}: {e:#}"))?;
    let parsed: Value = serde_json::from_str(&raw)?;
    Ok(parsed
        .get("instances")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default())
}

fn string_field(row: &Value, name: &str) -> String {
    match row.get(name) {
        Some(Value::String(s)) => s.clone(),
        None | Some(Value::Null) => String::new(),
        Some(other) => other.to_string(),
    }
}
