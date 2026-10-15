//! `instance_transcript` — a compact, chronological reading of the children
//! under a node that carry text (`ad4m://has_child`, and the `children` of any
//! class conforming to the built-in `Container`): the most recent N instances
//! with their text, author display name and timestamp, formatted
//! as plain text an LLM can read in one go instead of N+1 tool calls.
//!
//! This is the static-surface replacement for the old
//! `get_children_body_parsed`: same transcript shape, but the values come
//! through `model_query` so signed literal envelopes and custom-language
//! expressions resolve the same way `instance_query` resolves them.
//!
//! Without a class it is one model query on the built-in abstract class
//! `TextContent` (#1446): every class with a `text` property, or a property
//! aliased to `text`, whatever the app calls its class. With a class it is
//! one model query on that class. Either runs once per containment predicate.

use super::{
    class_properties, containment_predicates, error_json, find_property, link_target,
    resolve_class, run_model_query, Ad4mMcpHandler, PropView, MAX_QUERY_LIMIT,
};
use crate::languages::LanguageController;
use crate::perspectives::model_query::builtin_classes::TEXT_CONTENT_URI;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
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
    /// Subject class of the children to read (e.g. "Message"), as listed by
    /// describe_perspective. Omit it to read through the built-in abstract
    /// class TextContent: every class with a `text` property, or a property
    /// aliased to `text`.
    pub class_name: Option<String>,
    /// Parent node URI whose children are read (e.g. a Channel's id): via
    /// ad4m://has_child, and via the children relation of any class
    /// implementing the built-in Container (e.g. we://child). A bare string
    /// is wrapped as a literal URI.
    pub parent: String,
    /// How many of the most recent instances to include (default 50, max 500)
    pub limit: Option<usize>,
    /// Which property holds the text shown per entry, with class_name.
    /// Defaults to the class's text (its `text`, or the property it aliases
    /// to `text`), then `body`, then the class's identity property.
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

/// Which property of `class_name` to print: `requested`, else the one that
/// answers to TextContent's `text`, else `body`, else the class's identity
/// property — and a clear error if none applies. Only single-valued,
/// non-flag properties qualify (a collection renders as an array, a flag is
/// a class marker), so the lookup is restricted to exactly the set the error
/// message offers.
fn text_property_for(
    perspective: &PerspectiveInstance,
    class_name: &str,
    shape: &ModelShape,
    requested: Option<&str>,
) -> Result<String, String> {
    let infos = class_properties(shape);
    let scalars: Vec<PropView<'_>> = infos
        .iter()
        .filter(|i| !i.flag() && !i.collection())
        .copied()
        .collect();
    let scalar_names: Vec<&str> = scalars.iter().map(|i| i.name()).collect();
    if let Some(name) = requested.map(str::trim).filter(|n| !n.is_empty()) {
        if let Some(info) = find_property(&scalars, name) {
            return Ok(info.name().to_string());
        }
        let problem = match find_property(&infos, name) {
            Some(info) if info.collection() => format!(
                "text_property '{}' on class '{}' is a collection, not a single-valued property",
                info.name(),
                class_name
            ),
            Some(info) => format!(
                "text_property '{}' on class '{}' is a class marker, not a text property",
                info.name(),
                class_name
            ),
            None => format!("Unknown text_property '{}' on class '{}'", name, class_name),
        };
        return Err(error_json(format!(
            "{}. Single-valued properties: {}",
            problem,
            scalar_names.join(", ")
        )));
    }
    let through_text_content = perspective
        .property_through(TEXT_CONTENT_URI, class_name, "text")
        .ok()
        .flatten();
    let found = through_text_content
        .as_deref()
        .and_then(|name| find_property(&scalars, name))
        .or_else(|| find_property(&scalars, "body"))
        .or_else(|| scalars.iter().find(|i| i.identity()));
    match found {
        Some(info) => Ok(info.name().to_string()),
        None => Err(error_json(format!(
            "Class '{}' has no text (neither a `text` property, an alias for it, a `body` nor an \
             identity property) — pass text_property. Single-valued properties: {}",
            class_name,
            scalar_names.join(", ")
        ))),
    }
}

impl Ad4mMcpHandler {
    /// Read the most recent children under a node that carry text, as a
    /// transcript: of one class, or through TextContent.
    #[tool(
        description = "Read the most recent children of a node that carry text (children via ad4m://has_child, and via the children relation of any class implementing the built-in Container, e.g. we://child), as a plain-text transcript in chronological order — one entry per instance with its timestamp, author display name and DID, and its text as stored (HTML or Markdown). Ideal for reading a conversation-shaped container in one call: with only parent=<channel id> it is one query on the built-in abstract class TextContent, which reads every class with a `text` property or a property aliased to `text`, whatever the app calls the class. Pass class_name (and optionally text_property) to read one class instead; use describe_perspective to find them. limit picks how many of the newest to show (default 50); when there are more, the output starts with '(showing last N of M …)', where M is an upper bound when the node holds children under several predicates. Use instance_query for the full property maps or for filters."
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
        let class_name = p
            .class_name
            .as_deref()
            .map(str::trim)
            .filter(|c| !c.is_empty());
        // What to query, and which property holds the text.
        let (query_as, text_property) = match class_name {
            Some(class_name) => {
                let (class_name, shape) = match resolve_class(&perspective, class_name).await {
                    Ok(v) => v,
                    Err(e) => return e,
                };
                match text_property_for(
                    &perspective,
                    &class_name,
                    &shape,
                    p.text_property.as_deref(),
                ) {
                    Ok(text_property) => (class_name, text_property),
                    Err(e) => return e,
                }
            }
            None if p.text_property.is_some() => {
                return error_json(
                    "text_property needs class_name: without it the transcript reads `text` \
                     through TextContent",
                )
            }
            None => (TEXT_CONTENT_URI.to_string(), "text".to_string()),
        };
        let label = if query_as == TEXT_CONTENT_URI {
            "TextContent"
        } else {
            query_as.as_str()
        };

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
            match run_model_query(&perspective, &query_as, &query).await {
                Ok((found, count)) => {
                    total += count;
                    instances.extend(found);
                }
                Err(e) => return error_json(format!("Error reading {label} instances: {e}")),
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
            return format!("(no {label} instances under {parent})");
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
                "(showing last {} of {}{} {} instances under {})",
                instances.len(),
                bound,
                total,
                label,
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
