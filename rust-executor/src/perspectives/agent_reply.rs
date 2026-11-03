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
//! replies that finished are the agent's. Anything else in the container (another person's
//! message, a reply that failed) is left out, so a message somebody else wrote never reaches the
//! model as an instruction.
//!
//! **One at a time.** A container has at most one reply running ([`RunningReplies`], held per
//! perspective). [`cancel_reply`] stops it. A reply still `running` when the executor starts
//! again was cut off: the sweep at load marks it `interrupted`
//! (`perspective_instance::agent_replies`).

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

/// The replies running in one perspective, by container. A std mutex: held only to read or swap
/// an entry, never across an await.
pub type RunningReplies = HashMap<String, RunningReply>;

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
    /// A reply is already running in this container.
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

    // Hold the container's slot from here, so two calls can't both start a reply.
    let reply = format!("{REPLY_URI_PREFIX}{}", uuid::Uuid::new_v4());
    reserve(&perspective, container, &reply)?;
    let started =
        prepare_and_create(&mut perspective, container, caller_did, &reply, &context).await;
    let messages = match started {
        Ok(messages) => messages,
        Err(e) => {
            release(&perspective, container, &reply);
            return Err(e);
        }
    };

    let cancel_context = context.clone();
    let task_perspective = perspective.clone();
    let task_container = container.to_string();
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
        if !release(&task_perspective, &task_container, &task_reply) {
            return;
        }
        let values = match outcome {
            Ok(answer) => json!({ "status": ReplyStatus::Done.as_str(), "text": answer.text }),
            Err(e) => json!({ "status": ReplyStatus::Failed.as_str(), "error": e.to_string() }),
        };
        if let Err(e) = set_reply(task_perspective, &task_reply, values, &context).await {
            log::error!("agent reply {task_reply}: could not record how it ended: {e:#}");
        }
    });
    let cancelled_meanwhile = {
        let mut running = perspective
            .agent_replies
            .lock()
            .expect("agent replies poisoned");
        match running.get_mut(container) {
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
        if let Err(e) = set_reply(
            perspective,
            &reply,
            json!({ "status": ReplyStatus::Cancelled.as_str() }),
            &cancel_context,
        )
        .await
        {
            log::error!("agent reply {reply}: could not mark it cancelled: {e:#}");
        }
    }
    Ok(reply)
}

/// Stop the reply running in `container` and mark it `cancelled`. `false` when none is running.
/// What the agent wrote before it was stopped stays written.
pub async fn cancel_reply(
    perspective: PerspectiveInstance,
    container: &str,
    context: &AgentContext,
) -> anyhow::Result<bool> {
    let taken = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned")
        .remove(container.trim());
    let Some(running) = taken else {
        return Ok(false);
    };
    let Some(abort) = running.abort else {
        // Its task hasn't started yet. `start_reply` finds its slot gone once it has, then stops
        // the task and marks the reply.
        return Ok(true);
    };
    abort.abort();
    set_reply(
        perspective,
        &running.reply,
        json!({ "status": ReplyStatus::Cancelled.as_str() }),
        context,
    )
    .await?;
    Ok(true)
}

/// Read the conversation, refuse one with nothing to answer, settle replies left `running` by
/// an earlier executor, and create the new reply under the container.
async fn prepare_and_create(
    perspective: &mut PerspectiveInstance,
    container: &str,
    caller_did: &str,
    reply: &str,
    context: &AgentContext,
) -> Result<Vec<AgentMessage>, ReplyError> {
    let replies = replies_in(perspective, container).await?;
    for (id, row) in &replies {
        // The slot is ours, so a `running` reply here has no task: it was cut off.
        if row.status == ReplyStatus::Running.as_str() {
            set_reply(
                perspective.clone(),
                id,
                json!({ "status": ReplyStatus::Interrupted.as_str() }),
                context,
            )
            .await?;
        }
    }

    let query = json!({
        "parent": { "id": container },
        "order": [["timestamp", "desc"]],
        "limit": CONVERSATION_LIMIT,
    });
    let mut rows = query_rows(perspective, TEXT_CONTENT_URI, &query).await?;
    rows.reverse();
    let messages = conversation(&rows, &replies, caller_did);
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
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(AGENT_REPLY_CLASS.to_string()),
                query: None,
            },
            reply.to_string(),
            Some(json!({ "status": ReplyStatus::Running.as_str() })),
            Some(batch.clone()),
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
            Some(batch.clone()),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not put the reply in the conversation: {e:#}"))?;
    perspective
        .commit_batch(batch, context)
        .await
        .map_err(|e| anyhow::anyhow!("could not save the reply: {e:#}"))?;
    Ok(messages)
}

/// A reply as the conversation reads it.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ReplyRow {
    pub status: String,
    pub text: String,
}

/// The container's replies, by id.
async fn replies_in(
    perspective: &PerspectiveInstance,
    container: &str,
) -> anyhow::Result<HashMap<String, ReplyRow>> {
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
                },
            ))
        })
        .collect())
}

/// The conversation the model is given, oldest first: the caller's own messages as the
/// person's turns, finished replies as the agent's, and nothing else.
pub(crate) fn conversation(
    rows: &[Value],
    replies: &HashMap<String, ReplyRow>,
    caller_did: &str,
) -> Vec<AgentMessage> {
    rows.iter()
        .filter_map(|row| {
            let id = row.get("id").and_then(Value::as_str).unwrap_or_default();
            if let Some(reply) = replies.get(id) {
                let finished = reply.status == ReplyStatus::Done.as_str();
                return (finished && !reply.text.trim().is_empty()).then(|| AgentMessage {
                    role: "assistant".into(),
                    content: reply.text.clone(),
                });
            }
            let author = row
                .get("author")
                .and_then(Value::as_str)
                .unwrap_or_default();
            let text = string_field(row, "text");
            (author == caller_did && !text.trim().is_empty()).then(|| AgentMessage {
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
    container: &str,
    reply: &str,
) -> Result<(), ReplyError> {
    let mut running = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned");
    if running.contains_key(container) {
        return Err(ReplyError::Busy);
    }
    running.insert(
        container.to_string(),
        RunningReply {
            reply: reply.to_string(),
            abort: None,
        },
    );
    Ok(())
}

/// Give up the container's slot if `reply` still holds it. `true` if it did.
fn release(perspective: &PerspectiveInstance, container: &str, reply: &str) -> bool {
    let mut running = perspective
        .agent_replies
        .lock()
        .expect("agent replies poisoned");
    match running.get(container) {
        Some(entry) if entry.reply == reply => {
            running.remove(container);
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
