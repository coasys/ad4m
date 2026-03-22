//! Child relationship tools
//!
//! Generic tools for working with AD4M's implicit tree structure
//! via `ad4m://has_child` links. Used by Flux for messages in channels,
//! tasks in boards, etc.

use super::Ad4mMcpHandler;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::HashMap;

use crate::types::{Agent, LinkQuery, LinkStatus};
use crate::languages::LanguageController;
use crate::types::Link;

const HAS_CHILD_PREDICATE: &str = "ad4m://has_child";

/// Wrap a value as a literal URI only if it doesn't already look like a URI.
/// Values that contain "://" are assumed to be URIs and left as-is.
fn maybe_encode_literal(value: &str) -> String {
    if value.contains("://") {
        value.to_string()
    } else {
        Ad4mMcpHandler::encode_literal(value)
    }
}

/// Resolve a link target to its actual content value.
/// Tries literal decoding first; if the target is a language expression URL,
/// resolves it through the LanguageController.
async fn resolve_expression_value(target: &str) -> String {
    // First try literal decode (fast, no async)
    let literal_result = Ad4mMcpHandler::resolve_literal_value(target);
    if literal_result != target {
        // Literal decoding worked — check if the decoded value is itself a JSON
        // expression envelope with a "data" field (signed literal expressions)
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&literal_result) {
            if let Some(data) = parsed.get("data") {
                return match data {
                    serde_json::Value::String(s) => s.clone(),
                    other => other.to_string(),
                };
            }
        }
        return literal_result;
    }

    // Not a literal — try full expression resolution via LanguageController
    if let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(target) {
        let controller = LanguageController::global_instance();
        if let Ok(Some(expr_json)) = controller
            .get_expression(&lang_address, &expression_address)
            .await
        {
            // Extract the data field from the expression envelope
            if let Some(data) = expr_json.get("data") {
                return match data {
                    serde_json::Value::String(s) => s.clone(),
                    other => other.to_string(),
                };
            }
        }
    }

    // Fallback: return the raw target
    target.to_string()
}

/// Resolve a DID to a human-readable display name by looking up the agent's
/// public profile. Returns the username, given name, or the DID itself as fallback.
async fn resolve_did_to_name(did: &str) -> String {
    let controller = LanguageController::global_instance();
    let agent_lang = match controller.get_agent_language().await {
        Ok(lang) => lang,
        Err(_) => return did.to_string(),
    };
    let lang_address = agent_lang.address().to_string();

    match controller.get_expression(&lang_address, did).await {
        Ok(Some(expr_json)) => {
            let agent: Option<Agent> = serde_json::from_value(
                expr_json
                    .get("data")
                    .cloned()
                    .unwrap_or(serde_json::Value::Null),
            )
            .ok();

            if let Some(agent) = agent {
                if let Some(ref perspective) = agent.perspective {
                    // Look for username or given name in profile links
                    for link in &perspective.links {
                        if link.data.source == "flux://profile" {
                            let predicate = link.data.predicate.as_deref().unwrap_or("");
                            if predicate == "sioc://has_username"
                                || predicate == "sioc://has_given_name"
                            {
                                let name = Ad4mMcpHandler::resolve_literal_value(&link.data.target);
                                if !name.is_empty() {
                                    return name;
                                }
                            }
                        }
                    }
                }
            }
            did.to_string()
        }
        _ => did.to_string(),
    }
}

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for adding a child to a parent subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddChildParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent expression address (e.g., a channel ID).
    /// Will be auto-wrapped as literal://string: if not already a URI.
    pub parent_address: String,
    /// Child expression address (e.g., a message ID).
    /// Will be auto-wrapped as literal://string: if not already a URI.
    pub child_address: String,
}

/// Parameters for getting children of a parent subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetChildrenParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent expression address to get children of.
    /// Will be auto-wrapped as literal://string: if not already a URI.
    pub parent_address: String,
}

/// Parameters for getting children with resolved body content
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetChildrenBodyParsedParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent expression address (e.g., a channel ID).
    /// Will be auto-wrapped as literal://string: if not already a URI.
    pub parent_address: String,
    /// SHACL class name to discover the body property (e.g., "Message")
    pub class_name: String,
    /// Maximum number of messages to return (most recent). Defaults to 50.
    pub limit: Option<usize>,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Add a child to a parent subject instance via ad4m://has_child link.
    /// This is the generic tree structure used by Flux for messages in channels,
    /// subgroups in conversations, tasks in boards, etc.
    #[tool(
        description = "Add a child to a parent subject using ad4m://has_child. This is how Flux organizes messages in channels, tasks in boards, etc. — it's a generic tree structure. The parent is typically a channel/board ID, the child is a message/task ID. Both addresses are auto-wrapped as literal://string: if not already URIs. After adding the child link, you typically also set properties on the child (e.g., message_set_body)."
    )]
    pub async fn add_child(&self, params: Parameters<AddChildParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let source = maybe_encode_literal(&p.parent_address);
                let target = maybe_encode_literal(&p.child_address);

                let link = Link {
                    source,
                    predicate: Some(HAS_CHILD_PREDICATE.to_string()),
                    target,
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(decorated) => {
                        let result = json!({
                            "success": true,
                            "link": {
                                "source": decorated.data.source,
                                "predicate": decorated.data.predicate,
                                "target": decorated.data.target,
                                "timestamp": decorated.timestamp,
                            }
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding child: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get all children of a parent subject instance.
    /// Returns all ad4m://has_child links from the parent address.
    #[tool(
        description = "Get all children of a parent subject. Returns items linked via ad4m://has_child — messages in a channel, tasks in a board, subgroups in a conversation, etc. The parent_address is auto-wrapped as literal://string: if not already a URI. Returns child addresses and timestamps, sorted by timestamp."
    )]
    pub async fn get_children(&self, params: Parameters<GetChildrenParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                let source = maybe_encode_literal(&p.parent_address);

                let query = LinkQuery {
                    source: Some(source),
                    predicate: Some(HAS_CHILD_PREDICATE.to_string()),
                    target: None,
                    ..Default::default()
                };

                match perspective.get_links(&query).await {
                    Ok(mut links) => {
                        // Sort by timestamp
                        links.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

                        let children: Vec<serde_json::Value> = links
                            .iter()
                            .map(|l| {
                                json!({
                                    "address": l.data.target,
                                    "timestamp": l.timestamp,
                                    "author": l.author,
                                })
                            })
                            .collect();

                        let result = json!({
                            "parent": p.parent_address,
                            "count": children.len(),
                            "children": children,
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error getting children: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get all children of a parent with their body property resolved to plain text.
    /// Returns a formatted transcript string with author names, timestamps, and message content.
    #[tool(
        description = "Get children of a parent with their body content resolved to readable text. Uses the SHACL class definition to discover the body property, resolves language expressions to plain text, and resolves author DIDs to display names. Returns a formatted transcript — ideal for reading conversations, task lists, etc. without multiple round-trips. The class_name parameter specifies which SHACL class to use for body property discovery (e.g., 'Message'). The limit parameter controls how many of the most recent messages to return (default 50)."
    )]
    pub async fn get_children_body_parsed(
        &self,
        params: Parameters<GetChildrenBodyParsedParams>,
    ) -> String {
        let p = &params.0;

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Step 1: Get all has_child links, sorted by timestamp
        let source = maybe_encode_literal(&p.parent_address);
        let mut child_links = match perspective
            .get_links(&LinkQuery {
                source: Some(source),
                predicate: Some(HAS_CHILD_PREDICATE.to_string()),
                target: None,
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error getting children: {}", e),
        };

        child_links.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

        if child_links.is_empty() {
            return "(no messages)".to_string();
        }

        // Apply limit: keep only the most recent N messages
        let limit = p.limit.unwrap_or(50);
        let total = child_links.len();
        if child_links.len() > limit {
            child_links = child_links.split_off(child_links.len() - limit);
        }

        // Step 2: Discover the body property predicate from SHACL
        let body_predicate = {
            let props = crate::mcp::shacl::load_class_properties(&perspective, &p.class_name).await;
            let body_prop = props.iter().find(|prop| prop.name.to_lowercase() == "body");
            match body_prop {
                Some(prop) => prop.predicate.clone(),
                None => {
                    // Fallback: try common Flux predicate
                    log::warn!(
                        "No 'body' property found in SHACL for class '{}', trying flux://body",
                        p.class_name
                    );
                    Some("flux://body".to_string())
                }
            }
        };

        let body_predicate = match body_predicate {
            Some(pred) => pred,
            None => return "Error: could not determine body property predicate".to_string(),
        };

        // Step 3: For each child, resolve the body content
        struct ChildEntry {
            body: String,
            author_did: String,
            timestamp: String,
        }

        let mut entries: Vec<ChildEntry> = Vec::new();
        let mut unique_dids: Vec<String> = Vec::new();

        for child_link in child_links.iter() {
            let child_addr = &child_link.data.target;

            // Get the body property link
            let body_value = match perspective
                .get_links(&LinkQuery {
                    source: Some(child_addr.clone()),
                    predicate: Some(body_predicate.clone()),
                    ..Default::default()
                })
                .await
            {
                Ok(links) if !links.is_empty() => {
                    resolve_expression_value(&links[0].data.target).await
                }
                Ok(_) => "(no body)".to_string(),
                Err(e) => {
                    log::warn!("get_children_body_parsed: error querying body links: {}", e);
                    "(no body)".to_string()
                }
            };

            let author_did = child_link.author.clone();
            if !unique_dids.contains(&author_did) {
                unique_dids.push(author_did.clone());
            }

            entries.push(ChildEntry {
                body: body_value,
                author_did,
                timestamp: child_link.timestamp.clone(),
            });
        }

        // Step 4: Batch-resolve author DIDs to display names
        let mut name_cache: HashMap<String, String> = HashMap::new();
        for did in &unique_dids {
            let name = resolve_did_to_name(did).await;
            name_cache.insert(did.clone(), name);
        }

        // Step 5: Format as transcript
        let mut lines: Vec<String> = Vec::new();
        for entry in &entries {
            let display_name = name_cache
                .get(&entry.author_did)
                .cloned()
                .unwrap_or_else(|| entry.author_did.clone());

            lines.push(format!(
                "[{}] {} ({}):\n{}",
                entry.timestamp, display_name, entry.author_did, entry.body
            ));
        }

        if total > limit {
            lines.insert(0, format!("(showing last {} of {} messages)", limit, total));
        }

        lines.join("\n\n")
    }
}
