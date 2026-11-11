//! The wire and row types of an AI session: `ai_sessions` and `ai_messages` rows.
//!
//! `AiSession` and `AiSessionMessage` are the API's types too (`ai.sessionList`,
//! `ai.sessionMessages`), so their fields serialize camelCase and carry `TS` derives for the
//! generated contracts. `owner_did` is `None` for a session of the main agent, a managed
//! agent's DID otherwise — the `NULL`-is-the-main-agent convention of `tasks.agent_did`.

use crate::agent::capabilities::Capability;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use ts_rs::TS;

/// A conversation with the executor's AI: the `ai_sessions` row.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISession")]
pub struct AiSession {
    pub id: String,
    /// The managed agent that owns the session; `null` for a session of the main agent.
    pub owner_did: Option<String>,
    pub title: String,
    /// The model the session runs on; `null` = the node's default language model.
    pub model: Option<String>,
    /// The perspective the conversation is about: a hint for the model, not a limit. The
    /// session's tools may act on every perspective the caller and the capabilities allow.
    pub default_perspective: Option<String>,
    /// The perspective holding the agent's WE template library (#1663).
    pub library_perspective: Option<String>,
    /// What the session's tools may do, as a caller holds them: `with.domain`,
    /// `with.pointers` and `can`, e.g. read on perspective A, read+update on perspective B.
    /// The harness checks every tool call against these intersected with the caller's own.
    pub capabilities: Vec<Capability>,
    pub created_at: String,
    pub updated_at: String,
}

/// What `ai.sessionUpdate` may change. `None` keeps a column; for the nullable ones an
/// inner `None` clears it, `Some` sets it — the `Option<Option<..>>` of `set_nullable`.
#[derive(Debug, Clone, Default)]
pub struct AiSessionUpdate {
    pub title: Option<String>,
    pub model: Option<Option<String>>,
    pub default_perspective: Option<Option<String>>,
    pub library_perspective: Option<Option<String>>,
    pub capabilities: Option<Vec<Capability>>,
}

/// Who wrote an `ai_messages` row.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
#[ts(export, rename = "AISessionMessageRole")]
pub enum AiSessionMessageRole {
    /// The person the session belongs to.
    User,
    /// The agent's answer to a turn.
    Assistant,
    /// One tool call the agent made and what it answered (a step).
    Tool,
}

impl AiSessionMessageRole {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::User => "user",
            Self::Assistant => "assistant",
            Self::Tool => "tool",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "user" => Some(Self::User),
            "assistant" => Some(Self::Assistant),
            "tool" => Some(Self::Tool),
            _ => None,
        }
    }
}

/// Where an assistant turn or a tool step is. `running` is only ever set on a row its task
/// owns; the startup sweep marks any it finds `interrupted`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
#[ts(export, rename = "AITurnStatus")]
pub enum AiTurnStatus {
    Running,
    Done,
    Failed,
    Cancelled,
    Interrupted,
}

impl AiTurnStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Running => "running",
            Self::Done => "done",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
            Self::Interrupted => "interrupted",
        }
    }
}

/// One transcript entry: the `ai_messages` row.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionMessage")]
pub struct AiSessionMessage {
    pub id: String,
    pub session_id: String,
    /// The order the entries were written, per session.
    #[ts(type = "number")]
    pub seq: i64,
    pub role: AiSessionMessageRole,
    /// The transcript entry: `{"text": ..}` for user and assistant turns;
    /// `{"name", "arguments", "target"?, "result"? | "error"?}` for a tool step.
    #[ts(type = "unknown")]
    pub content: Value,
    /// Set on the assistant's turns and on tool steps: `running` until it ends.
    pub status: Option<AiTurnStatus>,
    /// Why a `failed`, `cancelled` or `interrupted` row ended.
    pub error: Option<String>,
    pub created_at: String,
}

impl AiSessionMessage {
    /// A failed or otherwise unfinished tool step's answer to the model.
    pub fn tool_result_text(&self) -> String {
        let content = &self.content;
        if let Some(result) = content.get("result").and_then(Value::as_str) {
            result.to_string()
        } else if let Some(error) = content.get("error").and_then(Value::as_str) {
            format!("error: {error}")
        } else if self.status == Some(AiTurnStatus::Running) {
            "error: the tool call did not return (the turn was interrupted)".to_string()
        } else {
            format!(
                "error: {}",
                self.error.as_deref().unwrap_or("the tool call failed")
            )
        }
    }
}

/// The `content` of a message the person wrote.
pub fn user_content(text: &str) -> Value {
    json!({ "text": text })
}

/// The `content` of an assistant turn: `text` is empty while it runs.
pub fn assistant_content(text: &str) -> Value {
    json!({ "text": text })
}

/// The `content` of a tool step when the model asked for the call. `target` is the id the
/// call acts on (a `perspective_id` or other address argument) when there is one.
pub fn tool_content(name: &str, arguments: &Value, target: Option<&str>) -> Value {
    match target {
        Some(target) => json!({
            "name": name,
            "arguments": arguments,
            "target": target,
        }),
        None => json!({
            "name": name,
            "arguments": arguments,
        }),
    }
}

/// What a tool step names as the thing it acts on: its `perspective_id` or, for a tool that
/// names a record or address, that argument. The UI shows it; nothing checks it.
pub fn tool_target(arguments: &Value) -> Option<&str> {
    for key in ["perspective_id", "base_uri", "expression_address", "uri"] {
        if let Some(target) = arguments.get(key).and_then(Value::as_str) {
            return Some(target);
        }
    }
    None
}

/// The `ai-session-changed` event: pushed to the session's owner on a new message, a
/// status change and each tool step.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionChanged")]
pub struct AiSessionChanged {
    pub session_id: String,
    /// `created`, `updated`, `deleted` or `message`.
    pub kind: String,
    /// The session row, for `created` and `updated`.
    pub session: Option<AiSession>,
    /// The message row, for `message`.
    pub message: Option<AiSessionMessage>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_tool_message_answers_its_result_or_its_error() {
        let mut step = AiSessionMessage {
            id: "m1".into(),
            session_id: "s1".into(),
            seq: 1,
            role: AiSessionMessageRole::Tool,
            content: json!({ "name": "t", "arguments": {} }),
            status: Some(AiTurnStatus::Done),
            error: None,
            created_at: "now".into(),
        };
        step.content["result"] = json!("the answer");
        assert_eq!(step.tool_result_text(), "the answer");
        step.content = json!({ "name": "t", "error": "not allowed" });
        assert_eq!(step.tool_result_text(), "error: not allowed");
        step.content = json!({ "name": "t" });
        step.status = Some(AiTurnStatus::Running);
        assert!(step.tool_result_text().starts_with("error: "));
        step.status = Some(AiTurnStatus::Failed);
        step.error = Some("boom".into());
        assert_eq!(step.tool_result_text(), "error: boom");
    }

    #[test]
    fn a_tool_step_names_the_perspective_it_acts_on() {
        assert_eq!(
            tool_target(&json!({ "perspective_id": "p1", "x": 1 })),
            Some("p1")
        );
        assert_eq!(
            tool_target(&json!({ "base_uri": "ad4m://x" })),
            Some("ad4m://x")
        );
        assert_eq!(tool_target(&json!({ "other": 1 })), None);
    }
}
