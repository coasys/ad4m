//! Subscription / waker query tools
//!
//! Tools for generating SurrealQL queries for external waker processes.
//! Queries are derived from SHACL class definitions when available,
//! avoiding hardcoded type-specific predicates.

use super::Ad4mMcpHandler;
use crate::agent::AgentService;
use crate::mcp::shacl;
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

/// Parameters for getting a mention-watcher config
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct MentionWakerConfigParams {
    /// Perspective UUID of the neighbourhood to watch for mentions.
    pub perspective_id: String,
    /// Override the agent name used in the query (defaults to profile username/given_name).
    pub name_override: Option<String>,
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
            // Use the same encoding logic as add_child/message_create: leave URIs as-is
            let parent_encoded = if parent.contains("://") {
                parent.clone()
            } else {
                Self::encode_literal(parent)
            };
            format!(
                "SELECT * FROM link WHERE source = '{}' AND predicate = 'ad4m://has_child'",
                parent_encoded
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

    /// Generate a single waker subscription config for mention tracking
    #[tool(
        description = "Generate a single waker subscription config entry that watches for mentions of this agent by name(s) or DID in a neighbourhood. Uses fn::contains(fn::parse_literal(target), term): fn::parse_literal extracts only the .data field from literal://json: Flux message bodies, so the author DID in the surrounding expression shape does not produce false-positive wakes; for literal://string: targets it returns the decoded value; for non-literal strings (raw DIDs etc.) it returns the value unchanged so explicit DID-as-target mention links still match. Both functions are already defined in SurrealDBService::new(). Returns one subscription ORing all profile names and the full DID. Agents should call this once per neighbourhood they join and add the returned waker_config entry to their waker config file, then restart the waker. Profile names (username, given_name, family_name) are all included; name_override adds an extra alias without replacing them."
    )]
    pub async fn get_mention_waker_config(
        &self,
        params: Parameters<MentionWakerConfigParams>,
    ) -> String {
        let token = self.get_auth_token().await.unwrap_or_default();

        // Resolve agent
        let agent = match AgentService::get_agent_for_context(
            &crate::agent::AgentContext::from_auth_token(token.clone()),
        ) {
            Ok(a) => a,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let did = agent.did.clone();

        // Validate perspective
        if let Err(e) = self
            .get_readable_perspective(&params.0.perspective_id)
            .await
        {
            return json!({"error": format!("Perspective not accessible: {}", e)}).to_string();
        }

        // Collect all profile names from the agent's public perspective.
        // Note on profile ontology: sioc:// predicates are currently shared with Flux;
        // a dedicated ad4m-wide profile ontology should replace these in the future.
        let mut names: Vec<String> = Vec::new();

        if let Some(ref perspective) = agent.perspective {
            for link in &perspective.links {
                if link.data.source == "flux://profile" {
                    let pred = link.data.predicate.as_deref().unwrap_or("");
                    if matches!(
                        pred,
                        "sioc://has_username" | "sioc://has_given_name" | "sioc://has_family_name"
                    ) {
                        let val = Self::resolve_literal_value(&link.data.target);
                        if !val.is_empty() && !names.contains(&val) {
                            names.push(val);
                        }
                    }
                }
            }
        }

        // Append name_override as an additional alias (does not replace profile names)
        if let Some(ref override_name) = params.0.name_override {
            if !override_name.is_empty() && !names.contains(override_name) {
                names.push(override_name.clone());
            }
        }

        let perspective_id = &params.0.perspective_id;

        // Build the SurrealQL query using fn::contains + fn::parse_literal.
        //
        // fn::parse_literal(target) handles all target types:
        //   - literal://json: targets (Flux message bodies): parses the expression
        //     shape and returns only the .data field — e.g. "Hey @Marvin!" — so the
        //     author DID embedded in the surrounding JSON is never seen by fn::contains,
        //     preventing false-positive wakes on every message the agent sends.
        //   - literal://string: targets (profile names etc.): returns the decoded string.
        //   - Non-literal strings (raw DIDs, URIs): returned unchanged, so explicit
        //     DID-as-target mention/follow links are still matched correctly.
        //
        // fn::contains(parsed, term) → substring check (str.includes).
        //
        // Both functions are already defined in SurrealDBService::new().
        // Case-insensitive matching via string::lowercase() (SurrealDB built-in):
        //   string::lowercase(fn::parse_literal(target)) lowercases the parsed value;
        //   search terms are also lowercased in Rust before embedding in the query.
        //   DIDs are already lowercase so .to_lowercase() is a no-op for them.
        //   If string::lowercase() receives a non-string (null/object), it returns null,
        //   which fn::contains handles gracefully (returns false).
        let all_terms: Vec<String> = names
            .iter()
            .map(|n| n.to_lowercase())
            .chain(std::iter::once(did.to_lowercase()))
            .collect();

        let mention_conditions: Vec<String> = all_terms
            .iter()
            .map(|t| {
                format!(
                    "fn::contains(string::lowercase(fn::parse_literal(target)), '{}')",
                    t
                )
            })
            .collect();

        let mention_predicate = format!("({})", mention_conditions.join(" OR "));

        // Two-hop query: find has_child links where any property of the child contains a mention.
        // has_child -> child_id -[any predicate]-> target_with_mention
        // The 2nd hop is predicate-agnostic so it works regardless of ontology (flux://has_body, etc.)
        // Note: must use array::len() > 0 instead of .source IS NOT NONE because SurrealDB
        // returns [] (empty array) for non-matching graph traversals, and [] IS NOT NONE == true.
        let query = format!(
            "SELECT * FROM link WHERE predicate = 'ad4m://has_child' AND array::len(out->link[WHERE {}]) > 0",
            mention_predicate
        );

        let sub_id = format!("mention-{}", &did[did.len().saturating_sub(12)..]);

        let subscription = json!({
            "id": sub_id,
            "perspective": perspective_id,
            "query": query,
        });

        json!({
            "did": did,
            "names": names,
            "perspective_id": perspective_id,
            "query": query,
            "subscription": subscription,
            "message": format!(
                "Add the subscription entry to your waker config and restart the waker. \
                 This single query watches for mentions of {} by {} in perspective {}.",
                did,
                if names.is_empty() {
                    "DID only".to_string()
                } else {
                    format!("name(s) [{}] or", names.join(", "))
                },
                perspective_id
            ),
        })
        .to_string()
    }
}
