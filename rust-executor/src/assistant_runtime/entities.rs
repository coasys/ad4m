//! WE entity model — subject classes materialised as `we://` links.
//!
//! Matches the WE contract (coasys/we#95): a subject instance is a base
//! expression URI carrying a flag link (`source --we://flag--> we://<class>`)
//! plus one property link per field (`source --we://<field>--> <literal>`).
//! HasMany relations are collection links (`source --we://<rel>--> target`).
//!
//! Personal config (`Assistant`/`Personality`/`Skill`/`McpServer`) lives in
//! the agent's we-root perspective; conversations (`Thread`/`Message`) live in
//! the neighbourhood perspective (fallback we-root).  All predicates are the
//! literal `we://…` strings below so the links this subsystem reads and writes
//! are byte-for-byte the ones WE's `Ad4mModel` layer produces and consumes.
//!
//! Everything in this module is pure (no executor runtime) so it is unit
//! testable without a model or a live perspective — the perspective I/O that
//! turns these structs into links lives in [`super::store`].

use std::collections::HashMap;

use ad4m_client::literal::{Literal, LiteralValue};

use crate::types::Link;

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Instance flag predicate. `source --we://flag--> we://<class>` marks a base
/// as an instance of a class.
pub const FLAG: &str = "we://flag";

// Class flag targets.
pub const CLASS_ASSISTANT: &str = "we://assistant";
pub const CLASS_PERSONALITY: &str = "we://personality";
pub const CLASS_SKILL: &str = "we://skill";
pub const CLASS_MCP_SERVER: &str = "we://mcp_server";
pub const CLASS_THREAD: &str = "we://thread";
pub const CLASS_MESSAGE: &str = "we://message";
pub const CLASS_RUN_STATE: &str = "we://run_state";

// Property predicates (snake_case of the field, prefixed `we://`).
pub const P_NAME: &str = "we://name";
pub const P_MODEL_ID: &str = "we://model_id";
pub const P_SYSTEM_PROMPT: &str = "we://system_prompt";
pub const P_PERSONALITY_IDS: &str = "we://personality_ids";
pub const P_SKILL_IDS: &str = "we://skill_ids";
pub const P_MCP_SERVER_IDS: &str = "we://mcp_server_ids";
pub const P_BODY: &str = "we://body";
pub const P_DESCRIPTION: &str = "we://description";
pub const P_TRANSPORT: &str = "we://transport";
pub const P_URL: &str = "we://url";
pub const P_COMMAND: &str = "we://command";
pub const P_AUTH: &str = "we://auth";
pub const P_TITLE: &str = "we://title";
pub const P_ASSISTANT_ID: &str = "we://assistant_id";
pub const P_CREATED_AT: &str = "we://created_at";
pub const P_UPDATED_AT: &str = "we://updated_at";
pub const P_THREAD_ID: &str = "we://thread_id";
pub const P_ROLE: &str = "we://role";
pub const P_CONTENT: &str = "we://content";
pub const P_TOOL_CALLS: &str = "we://tool_calls";
pub const P_TS: &str = "we://ts";
pub const P_STATUS: &str = "we://status";
pub const P_CURSOR: &str = "we://cursor";
pub const P_PENDING_TOOL_CALL: &str = "we://pending_tool_call";

/// Thread → Message HasMany collection predicate.
pub const REL_MESSAGE: &str = "we://message";

// ---------------------------------------------------------------------------
// Literal encode / decode
// ---------------------------------------------------------------------------

/// Encode a scalar string field as a `literal:string:…` URL, exactly as the
/// `Ad4mModel` property writer does.
pub fn encode_literal(value: &str) -> String {
    Literal::from_string(value.to_string())
        .to_url()
        .unwrap_or_else(|_| format!("literal:string:{}", value))
}

/// Decode a link target back to a plain string. `literal:…` URLs are decoded
/// to their inner value; any other target (a raw URI reference) is returned
/// verbatim.
pub fn decode_literal(target: &str) -> String {
    match Literal::from_url(target.to_string()) {
        Ok(lit) => match lit.get() {
            Ok(LiteralValue::String(s)) => s,
            Ok(other) => other.to_string(),
            Err(_) => target.to_string(),
        },
        Err(_) => target.to_string(),
    }
}

/// Parse a JSON-encoded `string[]` id array property (used for
/// `personalityIds`/`skillIds`/`mcpServerIds`). Tolerant: a missing or
/// malformed value yields an empty vector.
pub fn parse_id_array(value: Option<&String>) -> Vec<String> {
    value
        .and_then(|s| serde_json::from_str::<Vec<String>>(s).ok())
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Link builders
// ---------------------------------------------------------------------------

/// `source --we://flag--> we://<class>`
pub fn flag_link(base: &str, class: &str) -> Link {
    Link {
        source: base.to_string(),
        predicate: Some(FLAG.to_string()),
        target: class.to_string(),
    }
}

/// `source --predicate--> literal:string:<value>`
pub fn property_link(base: &str, predicate: &str, value: &str) -> Link {
    Link {
        source: base.to_string(),
        predicate: Some(predicate.to_string()),
        target: encode_literal(value),
    }
}

// ---------------------------------------------------------------------------
// Entities
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Assistant {
    pub id: String,
    pub name: String,
    pub model_id: String,
    pub system_prompt: String,
    pub personality_ids: Vec<String>,
    pub skill_ids: Vec<String>,
    pub mcp_server_ids: Vec<String>,
}

impl Assistant {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        Assistant {
            id,
            name: p.get(P_NAME).cloned().unwrap_or_default(),
            model_id: p.get(P_MODEL_ID).cloned().unwrap_or_default(),
            system_prompt: p.get(P_SYSTEM_PROMPT).cloned().unwrap_or_default(),
            personality_ids: parse_id_array(p.get(P_PERSONALITY_IDS)),
            skill_ids: parse_id_array(p.get(P_SKILL_IDS)),
            mcp_server_ids: parse_id_array(p.get(P_MCP_SERVER_IDS)),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Personality {
    pub id: String,
    pub name: String,
    pub body: String,
}

impl Personality {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        Personality {
            id,
            name: p.get(P_NAME).cloned().unwrap_or_default(),
            body: p.get(P_BODY).cloned().unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Skill {
    pub id: String,
    pub name: String,
    pub description: String,
    pub body: String,
}

impl Skill {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        Skill {
            id,
            name: p.get(P_NAME).cloned().unwrap_or_default(),
            description: p.get(P_DESCRIPTION).cloned().unwrap_or_default(),
            body: p.get(P_BODY).cloned().unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct McpServer {
    pub id: String,
    pub name: String,
    pub transport: String,
    pub url: String,
    pub command: String,
    pub auth: String,
}

impl McpServer {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        McpServer {
            id,
            name: p.get(P_NAME).cloned().unwrap_or_default(),
            transport: p.get(P_TRANSPORT).cloned().unwrap_or_default(),
            url: p.get(P_URL).cloned().unwrap_or_default(),
            command: p.get(P_COMMAND).cloned().unwrap_or_default(),
            auth: p.get(P_AUTH).cloned().unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Thread {
    pub id: String,
    pub title: String,
    pub assistant_id: String,
    pub model_id: String,
    pub created_at: String,
    pub updated_at: String,
}

impl Thread {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        Thread {
            id,
            title: p.get(P_TITLE).cloned().unwrap_or_default(),
            assistant_id: p.get(P_ASSISTANT_ID).cloned().unwrap_or_default(),
            model_id: p.get(P_MODEL_ID).cloned().unwrap_or_default(),
            created_at: p.get(P_CREATED_AT).cloned().unwrap_or_default(),
            updated_at: p.get(P_UPDATED_AT).cloned().unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Message {
    pub id: String,
    pub thread_id: String,
    pub role: String,
    pub content: String,
    /// JSON-encoded array of tool calls + results (may be empty).
    pub tool_calls: String,
    /// ISO-8601 timestamp.
    pub ts: String,
    /// `""` | `streaming` | `complete` | `error`.
    pub status: String,
}

impl Message {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        Message {
            id,
            thread_id: p.get(P_THREAD_ID).cloned().unwrap_or_default(),
            role: p.get(P_ROLE).cloned().unwrap_or_default(),
            content: p.get(P_CONTENT).cloned().unwrap_or_default(),
            tool_calls: p.get(P_TOOL_CALLS).cloned().unwrap_or_default(),
            ts: p.get(P_TS).cloned().unwrap_or_default(),
            status: p.get(P_STATUS).cloned().unwrap_or_default(),
        }
    }

    /// The links that materialise this message as a `Message` subject
    /// instance (flag + one property link per set field).
    pub fn to_links(&self) -> Vec<Link> {
        let mut links = vec![
            flag_link(&self.id, CLASS_MESSAGE),
            property_link(&self.id, P_THREAD_ID, &self.thread_id),
            property_link(&self.id, P_ROLE, &self.role),
            property_link(&self.id, P_CONTENT, &self.content),
            property_link(&self.id, P_TS, &self.ts),
            property_link(&self.id, P_STATUS, &self.status),
        ];
        if !self.tool_calls.is_empty() {
            links.push(property_link(&self.id, P_TOOL_CALLS, &self.tool_calls));
        }
        links
    }
}

/// Per-run durability record. One instance per active run, keyed by thread.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RunState {
    pub id: String,
    pub thread_id: String,
    /// `running` | `awaiting_tool` | `awaiting_model` | `done` | `error`.
    pub status: String,
    /// Progress marker (iteration index rendered as a string).
    pub cursor: String,
    /// JSON of an in-flight tool call awaiting execution (may be empty).
    pub pending_tool_call: String,
}

impl RunState {
    pub fn from_props(id: String, p: &HashMap<String, String>) -> Self {
        RunState {
            id,
            thread_id: p.get(P_THREAD_ID).cloned().unwrap_or_default(),
            status: p.get(P_STATUS).cloned().unwrap_or_default(),
            cursor: p.get(P_CURSOR).cloned().unwrap_or_default(),
            pending_tool_call: p.get(P_PENDING_TOOL_CALL).cloned().unwrap_or_default(),
        }
    }

    pub fn to_links(&self) -> Vec<Link> {
        let mut links = vec![
            flag_link(&self.id, CLASS_RUN_STATE),
            property_link(&self.id, P_THREAD_ID, &self.thread_id),
            property_link(&self.id, P_STATUS, &self.status),
            property_link(&self.id, P_CURSOR, &self.cursor),
        ];
        if !self.pending_tool_call.is_empty() {
            links.push(property_link(&self.id, P_PENDING_TOOL_CALL, &self.pending_tool_call));
        }
        links
    }

    /// A run that was interrupted mid-flight and should be re-enqueued on boot.
    pub fn is_active(&self) -> bool {
        matches!(
            self.status.as_str(),
            "running" | "awaiting_tool" | "awaiting_model"
        )
    }
}

// ---------------------------------------------------------------------------
// Trigger logic (pure — the core dedupe/resume decisions)
// ---------------------------------------------------------------------------

/// Given a thread's messages ordered by `ts`, return the message that should
/// trigger a run: the trailing message iff it is a completed user turn. This
/// self-dedupes — once an assistant reply is appended, the trailing message is
/// no longer a completed user turn, so re-scanning is a no-op.
pub fn trigger_message(sorted: &[Message]) -> Option<&Message> {
    match sorted.last() {
        Some(m) if m.role == "user" && m.status == "complete" => Some(m),
        _ => None,
    }
}

/// True when the trailing message is a half-written assistant reply — i.e. a
/// run died mid-stream and must be resumed.
pub fn needs_resume(sorted: &[Message]) -> bool {
    matches!(sorted.last(), Some(m) if m.role == "assistant" && m.status == "streaming")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_round_trip() {
        for v in ["hello", "with spaces & symbols/:", "", "unicode ☃ 漢字"] {
            let enc = encode_literal(v);
            assert!(enc.starts_with("literal:"), "encoded = {enc}");
            assert_eq!(decode_literal(&enc), v);
        }
    }

    #[test]
    fn decode_passes_through_raw_uris() {
        assert_eq!(decode_literal("we://message/abc"), "we://message/abc");
        assert_eq!(decode_literal("did:key:xyz"), "did:key:xyz");
    }

    #[test]
    fn id_array_parsing_is_tolerant() {
        assert_eq!(parse_id_array(Some(&r#"["a","b"]"#.to_string())), vec!["a", "b"]);
        assert!(parse_id_array(Some(&"not json".to_string())).is_empty());
        assert!(parse_id_array(None).is_empty());
    }

    #[test]
    fn message_links_round_trip_through_props() {
        let msg = Message {
            id: "we://message/1".into(),
            thread_id: "we://thread/9".into(),
            role: "assistant".into(),
            content: "hi there".into(),
            tool_calls: r#"[{"name":"f"}]"#.into(),
            ts: "2026-07-28T00:00:00Z".into(),
            status: "complete".into(),
        };
        // Rebuild the property map the way store::load_props would, decoding
        // each link target.
        let mut props = HashMap::new();
        for l in msg.to_links() {
            if let Some(pred) = l.predicate {
                if pred == FLAG {
                    continue;
                }
                props.insert(pred, decode_literal(&l.target));
            }
        }
        assert_eq!(Message::from_props(msg.id.clone(), &props), msg);
    }

    #[test]
    fn assistant_parses_id_arrays_and_scalars() {
        let mut p = HashMap::new();
        p.insert(P_NAME.to_string(), "Ada".to_string());
        p.insert(P_MODEL_ID.to_string(), "llama".to_string());
        p.insert(P_SYSTEM_PROMPT.to_string(), "be helpful".to_string());
        p.insert(P_PERSONALITY_IDS.to_string(), r#"["p1"]"#.to_string());
        p.insert(P_SKILL_IDS.to_string(), r#"["s1","s2"]"#.to_string());
        let a = Assistant::from_props("we://assistant/1".into(), &p);
        assert_eq!(a.name, "Ada");
        assert_eq!(a.model_id, "llama");
        assert_eq!(a.personality_ids, vec!["p1"]);
        assert_eq!(a.skill_ids, vec!["s1", "s2"]);
        assert!(a.mcp_server_ids.is_empty());
    }

    fn msg(role: &str, status: &str, ts: &str) -> Message {
        Message {
            id: format!("m-{ts}"),
            thread_id: "t".into(),
            role: role.into(),
            content: String::new(),
            tool_calls: String::new(),
            ts: ts.into(),
            status: status.into(),
        }
    }

    #[test]
    fn trigger_fires_only_on_trailing_complete_user() {
        let convo = vec![
            msg("user", "complete", "1"),
            msg("assistant", "complete", "2"),
            msg("user", "complete", "3"),
        ];
        assert_eq!(trigger_message(&convo).map(|m| m.ts.as_str()), Some("3"));

        // Trailing assistant reply → no trigger (self-dedupe).
        let answered = vec![msg("user", "complete", "1"), msg("assistant", "complete", "2")];
        assert!(trigger_message(&answered).is_none());

        // Incomplete user turn → no trigger.
        let partial = vec![msg("user", "streaming", "1")];
        assert!(trigger_message(&partial).is_none());

        assert!(trigger_message(&[]).is_none());
    }

    #[test]
    fn resume_detects_trailing_streaming_assistant() {
        let interrupted = vec![msg("user", "complete", "1"), msg("assistant", "streaming", "2")];
        assert!(needs_resume(&interrupted));
        let done = vec![msg("user", "complete", "1"), msg("assistant", "complete", "2")];
        assert!(!needs_resume(&done));
    }

    #[test]
    fn run_state_activity() {
        let mut rs = RunState {
            status: "running".into(),
            ..Default::default()
        };
        assert!(rs.is_active());
        rs.status = "awaiting_tool".into();
        assert!(rs.is_active());
        rs.status = "done".into();
        assert!(!rs.is_active());
        rs.status = "error".into();
        assert!(!rs.is_active());
    }
}
