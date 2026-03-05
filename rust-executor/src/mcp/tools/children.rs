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

use crate::graphql::graphql_types::{LinkQuery, LinkStatus};
use crate::types::Link;

const HAS_CHILD_PREDICATE: &str = "ad4m://has_child";

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
                let source = Self::encode_literal(&p.parent_address);
                let target = Self::encode_literal(&p.child_address);

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
                let source = Self::encode_literal(&p.parent_address);

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
}
