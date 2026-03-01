//! Subscription / waker query tools
//!
//! Tools for generating SurrealQL queries for external waker processes.
//! Queries are derived from SHACL class definitions when available,
//! avoiding hardcoded type-specific predicates.

use super::Ad4mMcpHandler;
use crate::mcp::shacl;
use crate::perspectives::get_perspective;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for generating a waker query config
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SubscribeToModelParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name to watch (e.g., "Message")
    pub class_name: String,
    /// Parent expression address to scope the subscription (e.g., a channel address).
    /// If provided, only watches for new instances that are children of this parent.
    pub parent_address: Option<String>,
    /// Predicate URI to filter by (e.g., "ad4m://has_child").
    /// If neither parent_address nor predicate is provided, the query is derived
    /// from the SHACL definition — watching for links whose predicates match
    /// any property defined on the subject class.
    pub predicate: Option<String>,
    /// Target value to match when filtering by predicate.
    /// Used together with `predicate` to narrow the subscription scope.
    pub target_value: Option<String>,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Generate a waker query config for watching model changes in a perspective
    #[tool(
        description = "Generate a SurrealQL query config for watching changes to a subject class in a perspective. This does NOT create a live subscription — it returns a query and config that you pass to an external waker process. The waker uses perspectiveSubscribeSurrealQuery (same mechanism as Flux UI) for live updates. Flow: 1) Call this tool to get the query config, 2) Store subscription_id + context in memory, 3) Add waker_config to the waker's config file and restart it, 4) When woken, use MCP tools to fetch the latest data. The query is derived from the SHACL class definition — no hardcoded type predicates."
    )]
    pub async fn generate_waker_query(&self, params: Parameters<SubscribeToModelParams>) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let p = &params.0;

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };

        let query = if let Some(ref parent) = p.parent_address {
            // Scope to children of a specific parent
            format!(
                "SELECT * FROM link WHERE source = '{}' AND predicate = 'ad4m://has_child'",
                Self::encode_literal(parent)
            )
        } else if let Some(ref predicate) = p.predicate {
            // Explicit predicate filter
            if let Some(ref target) = p.target_value {
                format!(
                    "SELECT * FROM link WHERE predicate = '{}' AND target = '{}'",
                    predicate, target
                )
            } else {
                format!("SELECT * FROM link WHERE predicate = '{}'", predicate)
            }
        } else {
            // Derive query from SHACL definition: watch for links matching
            // any predicate defined on the subject class's properties.
            let shacl_class = shacl::load_class(&perspective, &p.class_name).await;
            if let Some(class) = shacl_class {
                let predicates: Vec<String> = class
                    .properties
                    .iter()
                    .filter_map(|prop| prop.predicate.clone())
                    .collect();

                if predicates.is_empty() {
                    // No SHACL predicates found — fall back to broad query
                    "SELECT * FROM link ORDER BY timestamp DESC LIMIT 50".to_string()
                } else if predicates.len() == 1 {
                    format!("SELECT * FROM link WHERE predicate = '{}'", predicates[0])
                } else {
                    let predicate_list = predicates
                        .iter()
                        .map(|p| format!("'{}'", p))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("SELECT * FROM link WHERE predicate IN [{}]", predicate_list)
                }
            } else {
                // No SHACL definition found — fall back to broad query
                "SELECT * FROM link ORDER BY timestamp DESC LIMIT 50".to_string()
            }
        };

        let subscription_id = uuid::Uuid::new_v4().to_string();

        json!({
            "subscription_id": subscription_id,
            "perspective_id": p.perspective_id,
            "class_name": p.class_name,
            "parent_address": p.parent_address,
            "predicate": p.predicate,
            "target_value": p.target_value,
            "surreal_query": query,
            "waker_config": {
                "id": subscription_id,
                "perspective": p.perspective_id,
                "query": query,
            },
            "message": format!(
                "Subscription {} created for {} changes{}. Add the waker_config entry to your waker's config file and restart it. The waker uses perspectiveSubscribeSurrealQuery (same as Flux UI) for live change detection. Store this subscription_id in your memory with its context so you know what to do when woken.",
                subscription_id,
                p.class_name,
                p.parent_address.as_ref().map(|a| format!(" under parent {}", a)).unwrap_or_default()
            ),
        }).to_string()
    }
}
