//! `instance_transcript` — a compact, chronological reading of the children
//! under a node that carry text (`ad4m://has_child`, and the `children` of any
//! class conforming to the built-in `Container`): the most recent N instances
//! with their text, author display name and timestamp, formatted
//! as plain text an LLM can read in one go instead of N+1 tool calls.
//!
//! This is the static-surface replacement for the old
//! `get_children_body_parsed`, and it is one model query on the built-in
//! abstract class `TextContent` (#1446) per containment predicate: every
//! class with a `text` property, or a property aliased to `text`, whatever
//! the app calls its class. The tool looks up no class and no property;
//! which children carry text, and under which name, is TextContent's
//! answer. The values come through `model_query`, so signed literal
//! envelopes and custom-language expressions resolve the same way
//! `instance_query` resolves them.

use super::{
    containment_predicates, error_json, link_target, run_model_query, Ad4mMcpHandler,
    MAX_QUERY_LIMIT,
};
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
    /// Parent node URI whose children are read (e.g. a Channel's id): via
    /// ad4m://has_child, and via the children relation of any class
    /// implementing the built-in Container (e.g. we://child). A bare string
    /// is wrapped as a literal URI.
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
        description = "Read the most recent children of a node that carry text (children via ad4m://has_child, and via the children relation of any class implementing the built-in Container, e.g. we://child), as a plain-text transcript in chronological order — one entry per instance with its timestamp, author display name and DID, and its text as stored (HTML or Markdown). It reads through the built-in abstract class TextContent: every child whose class has a `text` property or a property aliased to `text`, whatever the app calls the class; children of other classes are not shown. Ideal for reading a conversation-shaped container in one call (parent=<channel id>). limit picks how many of the newest to show (default 50); when there are more, the output starts with '(showing last N of M …)', where M is an upper bound when the node holds children under several predicates. Use instance_query for one specific class, for the full property maps, or for filters."
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
        // Children through every containment predicate (`has_child`, and the
        // `children` of each class conforming to Container): newest-first with
        // the same limit under each, merged newest-first and cut to `limit`,
        // so the most recent of all of them stay. A record two predicates
        // both reach is shown once.
        let predicates = containment_predicates(&perspective);
        let mut instances: Vec<Value> = Vec::new();
        let mut total = 0;
        for predicate in &predicates {
            let query = json!({
                "parent": { "id": link_target(parent), "predicate": predicate },
                "order": [["timestamp", "desc"]],
                "limit": limit,
            });
            match run_model_query(&perspective, TEXT_CONTENT_URI, &query).await {
                Ok((found, count)) => {
                    total += count;
                    instances.extend(found);
                }
                Err(e) => return error_json(format!("Error reading TextContent instances: {e}")),
            }
        }
        if predicates.len() > 1 {
            let timestamp = |inst: &Value| {
                inst.get("timestamp")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .to_string()
            };
            instances.sort_by_key(|inst| std::cmp::Reverse(timestamp(inst)));
            let fetched = instances.len();
            let mut seen = std::collections::HashSet::new();
            instances.retain(|inst| {
                inst.get("id")
                    .and_then(Value::as_str)
                    .is_none_or(|id| seen.insert(id.to_string()))
            });
            total = total.saturating_sub(fetched - instances.len());
            instances.truncate(limit);
        }
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
            // A record two predicates both reach is subtracted only when both
            // windows returned it, so across predicates the total is a bound.
            let bound = if predicates.len() > 1 { "up to " } else { "" };
            lines.push(format!(
                "(showing last {} of {}{} TextContent instances under {})",
                instances.len(),
                bound,
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
