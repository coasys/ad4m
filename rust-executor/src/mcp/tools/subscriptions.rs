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
// Helpers
// ============================================================================

/// Escape a string for safe interpolation inside a double-quoted SPARQL
/// string literal. Mention terms come from profile names / `name_override`
/// — untrusted input, potentially attacker-controlled in a shared
/// neighbourhood — so without this, a `"` or `\` breaks out of the literal
/// and can inject arbitrary SPARQL into the mention-matching FILTER.
fn escape_sparql_literal(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Validate that a value intended to be an IRI is safe for interpolation
/// inside a SPARQL string literal (`STR(?x) = "..."`).  A well-formed IRI
/// has a scheme (e.g. `ad4m:`, `did:`, `literal:`) and must not contain
/// `"` or `\` which would break out of the enclosing double-quoted literal.
fn validate_sparql_iri(s: &str) -> Result<&str, String> {
    if !s.contains(':') {
        return Err(format!("expected IRI (scheme:...), got: {}", s));
    }
    if s.contains('"') || s.contains('\\') {
        return Err(format!(
            "IRI contains characters unsafe for SPARQL interpolation: {}",
            s
        ));
    }
    Ok(s)
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
            if let Err(e) = validate_sparql_iri(&parent_encoded) {
                return json!({"error": format!("Invalid parent_address: {}", e)}).to_string();
            }
            format!(
                "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?source) = \"{}\" && STR(?predicate) = \"ad4m://has_child\") }}",
                parent_encoded
            )
        } else if let Some(ref predicate) = p.predicate {
            if let Err(e) = validate_sparql_iri(predicate) {
                return json!({"error": format!("Invalid predicate: {}", e)}).to_string();
            }
            if let Some(ref target) = p.target_value {
                format!(
                    "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\" && STR(?target) = \"{}\") }}",
                    predicate, escape_sparql_literal(target)
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
                    .filter(|p| validate_sparql_iri(p).is_ok())
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
                .filter(|p| validate_sparql_iri(p).is_ok())
        } else {
            None
        };

        if body_predicate.is_none() {
            log::warn!("get_mention_waker_config: no body predicate found in SHACL for Message class, falling back to unscoped (but ontology-excluded) query");
        }
        let query = build_mention_query(&names, &did, body_predicate.as_deref());

        let sub_id = build_mention_sub_id(perspective_id);

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

/// Build the SPARQL mention-detection query for the waker.
///
/// CONTAINS does case-insensitive substring matching on the parsed literal
/// target, so a message body containing any profile name or the agent DID
/// matches. `body_predicate` scopes the scan to the message-body predicate when
/// SHACL resolves it (much faster on large perspectives).
///
/// Both the scoped and the unscoped-fallback query exclude `ad4m://ontology/*`
/// predicates (author DID, timestamp, proofKey — see `sparql_store`). Without
/// that exclusion the DID search term matches every link the agent itself
/// authored (its own author-DID proof metadata), so the agent would wake on its
/// own writes rather than on real mentions.
///
/// Search terms (profile names, DID) are interpolated into the query, so each is
/// escaped as a SPARQL string literal — a name containing `"` or `\` would
/// otherwise break out of the CONTAINS literal and could inject query syntax.
///
/// Pure over its inputs, so it unit-tests without an agent or perspective.
pub fn build_mention_query(names: &[String], did: &str, body_predicate: Option<&str>) -> String {
    let all_terms: Vec<String> = names
        .iter()
        .map(|n| n.to_lowercase())
        .chain(std::iter::once(did.to_lowercase()))
        .collect();
    let mention_conditions: Vec<String> = all_terms
        .iter()
        .map(|t| {
            format!(
                "CONTAINS(LCASE(STR(<ad4m://fn/parse_literal>(?target))), \"{}\")",
                escape_sparql_literal(t)
            )
        })
        .collect();
    let mention_predicate = format!("({})", mention_conditions.join(" || "));
    // Never match proof metadata, else the DID term self-matches authored links.
    let ontology_guard = "FILTER(!STRSTARTS(STR(?predicate), \"ad4m://ontology/\"))";
    match body_predicate {
        Some(pred) => format!(
            "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\") {} FILTER({}) }}",
            pred, ontology_guard, mention_predicate
        ),
        None => format!(
            "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) {} FILTER({}) }}",
            ontology_guard, mention_predicate
        ),
    }
}

/// Escape a string for embedding inside a SPARQL double-quoted literal.
/// Backslash first, then the double-quote and the control characters that a
/// STRING_LITERAL2 forbids raw (CR/LF/TAB) — a term carrying a newline would
/// otherwise produce an unparseable query.
fn escape_sparql_literal(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Build a mention-subscription id keyed on the perspective.
///
/// Deriving the id from the agent DID alone made it identical across every
/// neighbourhood the agent joined; the waker's dispose-by-id then evicted a
/// prior perspective's subscription when the agent subscribed in a second one,
/// so an agent in N neighbourhoods only woke in the most-recent. Keying on the
/// full perspective id gives distinct ids for distinct perspectives with no
/// truncation collision, and matches the plugin's `mention-${perspectiveId}`
/// dedup guard so a repeat subscribe on the same perspective is skipped.
pub fn build_mention_sub_id(perspective_id: &str) -> String {
    format!("mention-{}", perspective_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoped_query_excludes_ontology_proof_metadata() {
        let q = build_mention_query(
            &["alice".to_string()],
            "did:key:zAgent",
            Some("ad4m://message_body"),
        );
        assert!(
            q.contains("ad4m://message_body"),
            "scoped to the body predicate"
        );
        assert!(
            q.contains("FILTER(!STRSTARTS(STR(?predicate), \"ad4m://ontology/\"))"),
            "excludes ontology proof metadata"
        );
    }

    #[test]
    fn unscoped_fallback_still_excludes_ontology_proof_metadata() {
        // Regression: the fallback (no body predicate) used to be fully unscoped,
        // so the DID term matched the agent's own ad4m://ontology/author links.
        let q = build_mention_query(&["alice".to_string()], "did:key:zAgent", None);
        assert!(
            q.contains("FILTER(!STRSTARTS(STR(?predicate), \"ad4m://ontology/\"))"),
            "fallback must still exclude proof metadata"
        );
    }

    #[test]
    fn query_matches_names_and_did_case_insensitively() {
        let q = build_mention_query(
            &["Alice".to_string(), "Bob".to_string()],
            "did:key:zABC",
            None,
        );
        assert!(q.contains("\"alice\""), "name lowercased");
        assert!(q.contains("\"bob\""), "second name lowercased");
        assert!(q.contains("\"did:key:zabc\""), "DID lowercased + included");
        assert!(q.contains("parse_literal"), "decodes literal targets");
    }

    #[test]
    fn query_escapes_quotes_and_backslashes_in_terms() {
        // A term containing a double-quote or backslash must not break out of the
        // CONTAINS string literal (SPARQL injection guard).
        let q = build_mention_query(&["a\"b\\c".to_string()], "did:key:zX", None);
        assert!(q.contains("a\\\"b\\\\c"), "quote + backslash escaped: {q}");
        assert!(
            !q.contains("\"a\"b"),
            "raw unescaped quote must not appear: {q}"
        );
    }

    #[test]
    fn query_escapes_control_characters_in_terms() {
        // SPARQL STRING_LITERAL2 forbids raw CR/LF/TAB; a term carrying them must
        // be escaped, not embedded raw, or the query fails to parse.
        let q = build_mention_query(&["a\nb\tc\rd".to_string()], "did:key:zX", None);
        assert!(q.contains("a\\nb\\tc\\rd"), "control chars escaped: {q:?}");
        assert!(!q.contains('\n'), "no raw newline in the query: {q:?}");
        assert!(!q.contains('\t'), "no raw tab in the query: {q:?}");
    }

    #[test]
    fn sub_id_is_perspective_scoped_not_did_scoped() {
        // Two perspectives, same agent → distinct ids (no cross-neighbourhood
        // eviction). Uses the full perspective id (no truncation collision) and
        // matches the plugin's mention-${perspectiveId} dedup guard.
        let a = build_mention_sub_id("c41dfd35-769e-474f-a2e6-4e5d580615c8");
        let b = build_mention_sub_id("8432bdcb-410e-48e2-b1e1-2bb3f831d067");
        assert_ne!(a, b, "distinct perspectives must yield distinct sub ids");
        assert_eq!(a, "mention-c41dfd35-769e-474f-a2e6-4e5d580615c8");
        // Repeat subscribe on the same perspective → same id (dedup fires).
        assert_eq!(
            a,
            build_mention_sub_id("c41dfd35-769e-474f-a2e6-4e5d580615c8")
        );
    }

    #[test]
    fn test_escape_sparql_literal_neutralizes_quote_breakout() {
        // A profile name / name_override containing a `"` must not be able
        // to close the SPARQL string literal early and inject a second
        // CONTAINS(...) clause (or worse) into the mention FILTER.
        let malicious = r#"x") || CONTAINS(STR(?target), "leaked"#;
        let escaped = escape_sparql_literal(malicious);

        let condition = format!("CONTAINS(LCASE(STR(?target)), \"{}\")", escaped);

        // The whole payload must resolve to a single string literal — i.e.
        // exactly two unescaped double quotes (the ones we added), none
        // contributed by the input.
        let unescaped_quote_count = condition
            .char_indices()
            .filter(|&(i, c)| c == '"' && (i == 0 || condition.as_bytes()[i - 1] != b'\\'))
            .count();
        assert_eq!(
            unescaped_quote_count, 2,
            "escaped payload must not introduce unescaped quotes: {condition}"
        );
    }

    #[test]
    fn test_escape_sparql_literal_handles_backslash_and_control_chars() {
        assert_eq!(escape_sparql_literal(r"a\b"), r"a\\b");
        assert_eq!(escape_sparql_literal("a\"b"), "a\\\"b");
        assert_eq!(escape_sparql_literal("a\nb"), "a\\nb");
        assert_eq!(escape_sparql_literal("plain"), "plain");
    }

    #[test]
    fn test_validate_sparql_iri_accepts_valid_iris() {
        assert!(validate_sparql_iri("ad4m://has_child").is_ok());
        assert!(validate_sparql_iri("did:key:z6Mk123").is_ok());
        assert!(validate_sparql_iri("literal:string:hello").is_ok());
        assert!(validate_sparql_iri("urn:isbn:0451450523").is_ok());
    }

    #[test]
    fn test_validate_sparql_iri_rejects_no_scheme() {
        assert!(validate_sparql_iri("no-scheme-here").is_err());
    }

    #[test]
    fn test_validate_sparql_iri_rejects_injection_chars() {
        assert!(validate_sparql_iri(r#"ad4m://x" || true || ""#).is_err());
        assert!(validate_sparql_iri(r"ad4m://x\n").is_err());
    }
}
