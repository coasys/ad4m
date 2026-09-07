//! `instance_transcript` — a compact, chronological reading of one class's
//! `ad4m://has_child` children under a node: the most recent N instances
//! with their text property, author display name and timestamp, formatted
//! as plain text an LLM can read in one go instead of N+1 tool calls.
//!
//! This is the static-surface replacement for the old
//! `get_children_body_parsed`: same transcript shape, but the text property
//! is looked up on the class shape (defaulting to `body`, then the class's
//! identity property) and the values come through `model_query` so signed
//! literal envelopes and custom-language expressions resolve the same way
//! `instance_query` resolves them.

use super::{
    class_properties, error_json, find_property, link_target, resolve_class, run_model_query,
    Ad4mMcpHandler, PropView, HAS_CHILD, MAX_QUERY_LIMIT,
};
use crate::languages::LanguageController;
use crate::types::Agent;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;

/// Default number of most-recent instances a transcript shows.
pub(super) const DEFAULT_TRANSCRIPT_LIMIT: usize = 50;

/// Parameters for reading a transcript of one class's children under a node
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceTranscriptParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class of the children to read (e.g. "Message"), as listed by
    /// describe_perspective
    pub class_name: String,
    /// Parent node URI whose ad4m://has_child children are read (e.g. a
    /// Channel's id). A bare string is wrapped as a literal URI.
    pub parent: String,
    /// How many of the most recent instances to include (default 50, max 500)
    pub limit: Option<usize>,
    /// Which property holds the text shown per entry. Defaults to `body`,
    /// falling back to the class's identity property.
    pub text_property: Option<String>,
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

fn render_text(instance: &Value, text_property: &str) -> String {
    match instance.get(text_property) {
        None | Some(Value::Null) => format!("(no {text_property})"),
        Some(Value::String(s)) => s.clone(),
        Some(other) => other.to_string(),
    }
}

impl Ad4mMcpHandler {
    /// Read the most recent children of one class under a node as a transcript.
    #[tool(
        description = "Read the most recent instances of a class that are ad4m://has_child children of a node, as a plain-text transcript in chronological order — one entry per instance with its timestamp, author display name and DID, and its text property (body by default). Ideal for reading a Flux channel (class_name='Message', parent=<channel id>) in one call. limit picks how many of the newest to show (default 50); when there are more, the output starts with '(showing last N of M …)'. Use instance_query for the full property maps or for filters."
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
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        let parent = p.parent.trim();
        if parent.is_empty() {
            return error_json("parent must be a non-empty URI");
        }

        // Which property to print: the caller's choice, else `body`, else the
        // class's identity property — and a clear error if none applies.
        // Only single-valued, non-flag properties qualify (a collection
        // renders as an array, a flag is a class marker), so the lookup is
        // restricted to exactly the set the error message offers.
        let infos = class_properties(&shape);
        let scalars: Vec<PropView<'_>> = infos
            .iter()
            .filter(|i| !i.flag() && !i.collection())
            .copied()
            .collect();
        let scalar_names: Vec<&str> = scalars.iter().map(|i| i.name()).collect();
        let text_property = match p.text_property.as_deref().map(str::trim) {
            Some(name) if !name.is_empty() => match find_property(&scalars, name) {
                Some(info) => info.name().to_string(),
                None => {
                    let problem = match find_property(&infos, name) {
                        Some(info) if info.collection() => format!(
                            "text_property '{}' on class '{}' is a collection, not a \
                             single-valued property",
                            info.name(),
                            class_name
                        ),
                        Some(info) => format!(
                            "text_property '{}' on class '{}' is a class marker, not a \
                             text property",
                            info.name(),
                            class_name
                        ),
                        None => {
                            format!("Unknown text_property '{}' on class '{}'", name, class_name)
                        }
                    };
                    return error_json(format!(
                        "{}. Single-valued properties: {}",
                        problem,
                        scalar_names.join(", ")
                    ));
                }
            },
            _ => {
                match find_property(&scalars, "body")
                    .or_else(|| scalars.iter().find(|i| i.identity()))
                {
                    Some(info) => info.name().to_string(),
                    None => {
                        return error_json(format!(
                            "Class '{}' has neither a `body` nor an identity property — pass \
                         text_property. Single-valued properties: {}",
                            class_name,
                            scalar_names.join(", ")
                        ))
                    }
                }
            }
        };

        let limit = p
            .limit
            .unwrap_or(DEFAULT_TRANSCRIPT_LIMIT)
            .clamp(1, MAX_QUERY_LIMIT);
        let query = json!({
            "parent": { "id": link_target(parent), "predicate": HAS_CHILD },
            "order": [["timestamp", "desc"]],
            "limit": limit,
        });
        let (mut instances, total) = match run_model_query(&perspective, &class_name, &query).await
        {
            Ok(v) => v,
            Err(e) => return error_json(format!("Error reading {class_name} instances: {e}")),
        };
        if instances.is_empty() {
            return format!("(no {class_name} instances under {parent})");
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
                "(showing last {} of {} {} instances under {})",
                instances.len(),
                total,
                class_name,
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
                render_text(inst, &text_property)
            ));
        }
        lines.join("\n\n")
    }
}
