//! `instance_transcript` — a compact, chronological reading of the children
//! under a node (through `Container`, #1446) that carry text: the most recent
//! N instances with their text, author display name and timestamp, formatted
//! as plain text an LLM can read in one go instead of N+1 tool calls.
//!
//! This is the static-surface replacement for the old
//! `get_children_body_parsed`, and it is one model query on the built-in
//! abstract class `TextContent` (#1446): every class with a `text` property,
//! or a property aliased to `text`, whatever the app calls its class. The
//! tool looks up no class and no property; which children carry text, and
//! under which name, is TextContent's answer. The values come through
//! `model_query`, so signed literal envelopes and custom-language
//! expressions resolve the same way `instance_query` resolves them.

use super::{error_json, link_target, run_model_query, Ad4mMcpHandler, MAX_QUERY_LIMIT};
use crate::languages::LanguageController;
use crate::perspectives::model_query::builtin_classes::TEXT_CONTENT_URI;
use crate::types::Agent;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;

/// Default number of most-recent instances a transcript shows.
pub(super) const DEFAULT_TRANSCRIPT_LIMIT: usize = 50;

/// Parameters for reading a transcript of the children under a node that
/// carry text
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceTranscriptParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent node URI whose children are read (e.g. a Channel's id): its
    /// `ad4m://has_child` children, and those of its class's own `children`
    /// relation. A bare string is wrapped as a literal URI.
    pub parent: String,
    /// How many of the most recent instances to include (default 50, max 500)
    pub limit: Option<usize>,
}

/// Resolve a DID to a display name via the agent's public profile
/// (`sioc://has_username` / `sioc://has_given_name` under `flux://profile`).
/// Falls back to the DID itself whenever the profile cannot be read.
pub(super) async fn display_name_for(did: &str) -> String {
    let controller = LanguageController::global_instance();
    let agent_lang = match controller.get_agent_language().await {
        Ok(lang) => lang,
        Err(_) => return did.to_string(),
    };
    let lang_address = agent_lang.address().to_string();

    let Ok(Some(expr_json)) = controller.get_expression(&lang_address, did).await else {
        return did.to_string();
    };
    let agent: Option<Agent> =
        serde_json::from_value(expr_json.get("data").cloned().unwrap_or(Value::Null)).ok();
    let Some(perspective) = agent.and_then(|a| a.perspective) else {
        return did.to_string();
    };
    for link in &perspective.links {
        if link.data.source != "flux://profile" {
            continue;
        }
        let predicate = link.data.predicate.as_deref().unwrap_or("");
        if predicate == "sioc://has_username" || predicate == "sioc://has_given_name" {
            let name = Ad4mMcpHandler::resolve_literal_value(&link.data.target);
            if !name.trim().is_empty() {
                return name;
            }
        }
    }
    did.to_string()
}

/// TextContent's `text` of one instance, as stored.
fn render_text(instance: &Value) -> String {
    match instance.get("text") {
        None | Some(Value::Null) => "(no text)".to_string(),
        Some(Value::String(s)) => s.clone(),
        Some(other) => other.to_string(),
    }
}

impl Ad4mMcpHandler {
    /// Read the most recent children under a node that carry text, through
    /// TextContent, as a transcript.
    #[tool(
        description = "Read the most recent children of a node that carry text (its ad4m://has_child children, plus those of its class's own children relation, e.g. a block's we://child), as a plain-text transcript in chronological order — one entry per instance with its timestamp, author display name and DID, and its text as stored (HTML or Markdown). It is one query on the built-in abstract class TextContent: it reads every child whose class has a `text` property or a property aliased to `text`, whatever the app calls the class; children of other classes are not shown. Ideal for reading a conversation-shaped container in one call (parent=<channel id>). limit picks how many of the newest to show (default 50); when there are more, the output starts with '(showing last N of M …)'. Use instance_query for one specific class, for the full property maps, or for filters."
    )]
    pub async fn instance_transcript(
        &self,
        params: Parameters<InstanceTranscriptParams>,
    ) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let parent = p.parent.trim();
        if parent.is_empty() {
            return error_json("parent must be a non-empty URI");
        }

        let limit = p
            .limit
            .unwrap_or(DEFAULT_TRANSCRIPT_LIMIT)
            .clamp(1, MAX_QUERY_LIMIT);
        let query = json!({
            "parent": { "id": link_target(parent) },
            "order": [["timestamp", "desc"]],
            "limit": limit,
        });
        let (mut instances, total) =
            match run_model_query(&perspective, TEXT_CONTENT_URI, &query).await {
                Ok(v) => v,
                Err(e) => return error_json(format!("Error reading TextContent instances: {e}")),
            };
        if instances.is_empty() {
            return format!("(no TextContent instances under {parent})");
        }
        // Newest-first from the query so `limit` keeps the most recent;
        // present oldest-first like a conversation reads.
        instances.reverse();

        let mut names: HashMap<String, String> = HashMap::new();
        for inst in &instances {
            if let Some(did) = inst.get("author").and_then(Value::as_str) {
                if !names.contains_key(did) {
                    let name = display_name_for(did).await;
                    names.insert(did.to_string(), name);
                }
            }
        }

        let mut lines: Vec<String> = Vec::with_capacity(instances.len() + 1);
        if total > instances.len() {
            lines.push(format!(
                "(showing last {} of {} TextContent instances under {})",
                instances.len(),
                total,
                parent
            ));
        }
        for inst in &instances {
            let did = inst.get("author").and_then(Value::as_str).unwrap_or("");
            let name = names.get(did).cloned().unwrap_or_else(|| did.to_string());
            let timestamp = inst.get("timestamp").and_then(Value::as_str).unwrap_or("");
            lines.push(format!(
                "[{}] {} ({}):\n{}",
                timestamp,
                name,
                did,
                render_text(inst)
            ));
        }
        lines.join("\n\n")
    }
}
