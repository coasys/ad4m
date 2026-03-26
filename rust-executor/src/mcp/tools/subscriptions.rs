//! Subscription / waker query tools
//!
//! Tools for generating SPARQL queries for external waker processes.
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
        description = "Generate a SPARQL query config for watching changes to a subject class in a perspective. This does NOT create a live subscription — it returns a query and config that you pass to an external waker process. The waker uses perspective query subscriptions for live updates. Flow: 1) Call this tool to get the query config, 2) Store subscription_id + context in memory, 3) Add waker_config to the waker's config file and restart it, 4) When woken, use MCP tools to fetch the latest data. The query is derived from the SHACL class definition — no hardcoded type predicates."
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
            let parent_encoded = if parent.contains("://") {
                parent.clone()
            } else {
                Self::encode_literal(parent)
            };
            format!(
                "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?source) = \"{}\" && STR(?predicate) = \"ad4m://has_child\") }}",
                parent_encoded
            )
        } else if let Some(ref predicate) = p.predicate {
            if let Some(ref target) = p.target_value {
                format!(
                    "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\" && STR(?target) = \"{}\") }}",
                    predicate, target
                )
            } else {
                format!(
                    "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\") }}",
                    predicate
                )
            }
        } else {
            // Derive query from SHACL definition
            let shacl_class = shacl::load_class(&perspective, &p.class_name).await;
            if let Some(class) = shacl_class {
                let predicates: Vec<String> = class
                    .properties
                    .iter()
                    .filter_map(|prop| prop.predicate.clone())
                    .collect();

                if predicates.is_empty() {
                    "SELECT ?source ?predicate ?target WHERE { ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) } LIMIT 50".to_string()
                } else if predicates.len() == 1 {
                    format!(
                        "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\") }}",
                        predicates[0]
                    )
                } else {
                    let filter_conditions = predicates
                        .iter()
                        .map(|p| format!("STR(?predicate) = \"{}\"", p))
                        .collect::<Vec<_>>()
                        .join(" || ");
                    format!(
                        "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER({}) }}",
                        filter_conditions
                    )
                }
            } else {
                "SELECT ?source ?predicate ?target WHERE { ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) } LIMIT 50".to_string()
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
            "query": query,
            "message": format!(
                "Subscription {} created for {} changes{}.",
                subscription_id,
                p.class_name,
                p.parent_address.as_ref().map(|a| format!(" under parent {}", a)).unwrap_or_default()
            ),
        }).to_string()
    }

    /// Generate a single waker subscription config for mention tracking
    #[tool(
        description = "Generate a single waker subscription config entry that watches for mentions of this agent by name(s) or DID in a neighbourhood. Watches for any link whose target contains a mention term (using SPARQL CONTAINS for substring matching). The waker plugin resolves parent channels in a second query when the subscription fires. Returns one subscription ORing all profile names and the full DID. Agents should call this once per neighbourhood they join and add the returned waker_config entry to their waker config file, then restart the waker. Profile names (username, given_name, family_name) are all included; name_override adds an extra alias without replacing them."
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

        // Build the SPARQL query using CONTAINS for mention matching.
        //
        // The SPARQL CONTAINS function performs substring matching on the target IRI/literal.
        // Case-insensitive matching via LCASE().
        // Search terms are lowercased in Rust before embedding in the query.
        let all_terms: Vec<String> = names
            .iter()
            .map(|n| n.to_lowercase())
            .chain(std::iter::once(did.to_lowercase()))
            .collect();

        let mention_conditions: Vec<String> = all_terms
            .iter()
            .map(|t| format!("CONTAINS(LCASE(STR(?target)), \"{}\")", t))
            .collect();

        let mention_predicate = format!("({})", mention_conditions.join(" || "));

        // Discover the body property predicate from SHACL to scope the query.
        // Without this, the query scans ALL links which is very slow on large perspectives.
        let perspective = self
            .get_readable_perspective(&params.0.perspective_id)
            .await
            .ok();
        let body_predicate = if let Some(ref p) = perspective {
            let props = shacl::load_class_properties(p, "Message").await;
            props
                .iter()
                .find(|prop| prop.name.to_lowercase() == "body")
                .and_then(|prop| prop.predicate.clone())
        } else {
            None
        };

        let query = if let Some(ref pred) = body_predicate {
            format!(
                "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\") FILTER({}) }}",
                pred, mention_predicate
            )
        } else {
            log::warn!("get_mention_waker_config: no body predicate found in SHACL for Message class, falling back to unscoped query");
            format!(
                "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER({}) }}",
                mention_predicate
            )
        };

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
