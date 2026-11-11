//! The harness [`ToolObserver`] of an agent-session turn: every tool call the model makes
//! becomes a `role: "tool"` transcript entry, live, so the UI shows the steps as they run.
//!
//! `tool_called` appends a `running` step naming the tool, its arguments and the target it
//! acts on; `tool_returned` ends it `done` or `failed` with the result or the error. The
//! loop runs calls one at a time in order, so a FIFO of open step ids pairs each return
//! with its call; a return without a call (never from the loop; kept safe) appends a
//! finished step. A step still open when the turn ends — cancelled, out of time, or its
//! task gone mid-call — is marked `interrupted` as the observer drops, so no step reads
//! `running` on a turn that has ended; it answers the model `"error: …"` in the rebuilt
//! history.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use serde_json::{json, Value};
use uuid::Uuid;

use super::service;
use super::store::AgentSessionStore;
use super::types::{tool_content, tool_target, AgentSessionMessageRole, AgentTurnStatus};
use crate::agent::AgentContext;
use crate::ai_service::harness::ToolObserver;

/// Writes each tool call of one turn to the session's transcript.
pub struct StepObserver {
    store: Arc<dyn AgentSessionStore>,
    owner_did: Option<String>,
    session_id: String,
    /// The agent the `agent-session-changed` events go to.
    agent: AgentContext,
    /// Ids of the steps still `running`, oldest first.
    open: Mutex<VecDeque<String>>,
}

impl StepObserver {
    pub fn new(store: Arc<dyn AgentSessionStore>, agent: AgentContext, session_id: &str) -> Self {
        Self {
            store,
            owner_did: agent.managed_did().map(str::to_string),
            session_id: session_id.to_string(),
            agent,
            open: Mutex::new(VecDeque::new()),
        }
    }

    /// The step's row, as it now reads, told to the session's owner.
    fn push(&self, step: super::types::AgentSessionMessage) {
        service::publish_step(&self.agent, &self.session_id, step);
    }
}

/// The turn is over and nothing will return for the steps still open: each is marked
/// `interrupted` and told to the owner. Runs on every way a turn ends, an abort included —
/// the observer lives in the turn's future.
impl Drop for StepObserver {
    fn drop(&mut self) {
        let open = std::mem::take(self.open.get_mut().unwrap_or_else(|e| e.into_inner()));
        for id in open {
            match self.store.finish_message(
                self.owner_did.as_deref(),
                &id,
                AgentTurnStatus::Interrupted,
                Some("the turn ended before this call returned"),
                None,
            ) {
                Ok(true) => {
                    if let Ok(Some(step)) = self.store.message(self.owner_did.as_deref(), &id) {
                        self.push(step);
                    }
                }
                // The session was deleted with its rows: nothing is left to mark.
                Ok(false) => {}
                Err(e) => log::error!(
                    "agent session {}: could not mark a cut-off tool step: {e}",
                    self.session_id
                ),
            }
        }
    }
}

#[async_trait]
impl ToolObserver for StepObserver {
    async fn tool_called(&self, name: &str, arguments: &Value) {
        let content = tool_content(name, arguments, tool_target(arguments));
        match self.store.append_message(
            self.owner_did.as_deref(),
            &Uuid::new_v4().to_string(),
            &self.session_id,
            AgentSessionMessageRole::Tool,
            &content,
            Some(AgentTurnStatus::Running),
        ) {
            Ok(Some(step)) => {
                self.open.lock().unwrap().push_back(step.id.clone());
                self.push(step);
            }
            Ok(None) => log::warn!(
                "agent session {}: a tool step found no session to land on",
                self.session_id
            ),
            Err(e) => log::error!(
                "agent session {}: could not write a tool step: {e}",
                self.session_id
            ),
        }
    }

    async fn tool_returned(&self, name: &str, arguments: &Value, result: &str) {
        // A tool's failure reaches the observer two ways: a dispatch that failed reads
        // `error: …`, and a refused call (a capability or ownership check inside the
        // tool) returns the refusal as data — the `{"error": .., "status": <code>}` of
        // `method_error`. Either ends the step `failed` with the words under `error`, so
        // the transcript and the UI say what went wrong.
        let refusal = |result: &str| -> Option<String> {
            // `method_error`'s exact shape: a string `error` and a numeric `status`,
            // nothing else — a payload with other fields is data, not a refusal.
            let value: Value = serde_json::from_str(result).ok()?;
            let object = value.as_object()?;
            if !object.keys().all(|k| k == "error" || k == "status") {
                return None;
            }
            let error = value.get("error")?.as_str()?;
            value.get("status")?.as_u64()?;
            Some(error.to_string())
        };
        let (status, patch, error) = match result.strip_prefix("error: ") {
            Some(error) => (
                AgentTurnStatus::Failed,
                json!({ "error": error.trim() }),
                Some(error.trim().to_string()),
            ),
            None => match refusal(result) {
                Some(error) => (
                    AgentTurnStatus::Failed,
                    json!({ "error": error }),
                    Some(error),
                ),
                None => (AgentTurnStatus::Done, json!({ "result": result }), None),
            },
        };
        let id = self.open.lock().unwrap().pop_front();
        match id {
            Some(id) => {
                if let Err(e) = self.store.finish_message(
                    self.owner_did.as_deref(),
                    &id,
                    status,
                    error.as_deref(),
                    Some(&patch),
                ) {
                    log::error!(
                        "agent session {}: could not finish a tool step: {e}",
                        self.session_id
                    );
                }
                if let Ok(Some(step)) = self.store.message(self.owner_did.as_deref(), &id) {
                    self.push(step);
                }
            }
            None => {
                log::warn!(
                    "agent session {}: a tool returned that was never called",
                    self.session_id
                );
                let mut content = tool_content(name, arguments, tool_target(arguments));
                if let (Value::Object(c), Value::Object(p)) = (&mut content, &patch) {
                    c.extend(p.clone());
                }
                if let Ok(Some(step)) = self.store.append_message(
                    self.owner_did.as_deref(),
                    &Uuid::new_v4().to_string(),
                    &self.session_id,
                    AgentSessionMessageRole::Tool,
                    &content,
                    Some(status),
                ) {
                    self.push(step);
                }
            }
        }
    }
}
