//! Subscription / waker query tools
//!
//! Tools for generating SPARQL and model queries for external waker processes.
//! Queries are derived from SHACL class definitions when available,
//! avoiding hardcoded type-specific predicates.

use super::Ad4mMcpHandler;
use crate::agent::AgentService;
use crate::mcp::shacl;
use crate::perspectives::model_query::builtin_classes::{
    CONTAINER_URI, MENTIONING_URI, TEXT_CONTENT_URI,
};
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
    /// Accepts the pre-rename `parent_address` spelling from older clients.
    #[serde(alias = "parent_address")]
    pub parent: Option<String>,
    /// Predicate URI to filter by (e.g., "ad4m://has_child").
    /// If neither parent nor predicate is provided, the query is derived
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
}

// ============================================================================
// Helpers
// ============================================================================

/// Escape a string for safe interpolation inside a double-quoted SPARQL
/// string literal. `generate_waker_query`'s `target_value` is caller input,
/// so without this a `"` or `\` breaks out of the literal and can inject
/// SPARQL into the FILTER. Also escapes CR/LF/TAB, which SPARQL
/// STRING_LITERAL2 forbids raw.
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
        description = "Generate a query config for watching changes in a perspective. This does NOT create a live subscription — it returns a query and config that you pass to an external waker process. The waker uses perspective query subscriptions for live updates. Flow: 1) Call this tool to get the query config, 2) Store subscription_id + context in memory, 3) Add waker_config to the waker's config file and restart it, 4) When woken, use MCP tools to fetch the latest data. With parent, the query is a model query over the built-in class in query_class (ad4m://Container): the parent's children, whatever their class and however the parent holds them; subscribe to it with a model subscription. Otherwise it is SPARQL derived from the SHACL class definition — no hardcoded type predicates."
    )]
    pub async fn generate_waker_query(&self, params: Parameters<SubscribeToModelParams>) -> String {
        let p = &params.0;

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };

        // Under a parent, the subscription is a model query over the built-in
        // Container (#1446): that parent's row, whose `children` are
        // everything it holds, through `has_child` or its class's own
        // `children` relation. It changes when a child is added or removed.
        let mut query_class: Option<&str> = None;
        let query = if let Some(ref parent) = p.parent {
            let parent_encoded = Self::wrap_bare_as_literal(parent);
            if let Err(e) = validate_sparql_iri(&parent_encoded) {
                return json!({"error": format!("Invalid parent: {}", e)}).to_string();
            }
            query_class = Some(CONTAINER_URI);
            json!({
                "where": { "id": parent_encoded },
                "properties": ["children"],
            })
            .to_string()
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
            "parent": p.parent,
            "predicate": p.predicate,
            "target_value": p.target_value,
            "query": query,
            "query_class": query_class,
            "message": format!(
                "Subscription {} created for {} changes{}.",
                subscription_id,
                p.class_name,
                p.parent.as_ref().map(|a| format!(" under parent {}", a)).unwrap_or_default()
            ),
        })
        .to_string()
    }

    /// Generate a single waker subscription config for mention tracking
    #[tool(
        description = "Generate a single waker subscription config entry that watches for mentions of this agent in a neighbourhood. It carries two model queries over built-in abstract classes, which the waker opens under one id, so a record both find wakes it once, through the query that reports it first. `mentionQuery` reads Mentioning (class ad4m://Mentioning, `{ mentions }`): every record whose `mentions` relation links the agent's DID, whatever the app calls its class, with text or without: a class takes part when it has a relation named `mentions`, or one aliased to `mentions`, under any predicate (the built-in's own is `ad4m://mentions`). `query` reads TextContent (class ad4m://TextContent, `{ text }`): every record whose text contains one of the agent's profile names (username, given_name, family_name) or its DID, case-insensitively, whatever the app calls its class or text property. Both match only through a link another agent wrote, whose signature verifies, so the agent's own reply naming it does not wake it; both are newest first. A mention link wakes the agent whatever the text says. The text query matches substrings: the waker keeps a record found through its text only when the text holds a returned term as a whole word (\"data\" matches \"hi Data!\", not \"metadata\"); HTML markup is ignored, and a mention chip's DID attribute counts. The rows carry each mention link's or text link's author, timestamp and signature verdict (`__links`), which the waker hands the woken agent. The waker opens both with perspective.modelSubscribe and, when one fires, resolves the parent channels of each mentioned record in a second query. Agents should call this once per neighbourhood they join and add the returned waker_config entry to their waker config file, then restart the waker."
    )]
    pub async fn get_mention_waker_config(
        &self,
        params: Parameters<MentionWakerConfigParams>,
    ) -> String {
        if let Err(e) = self
            .get_readable_perspective(&params.0.perspective_id)
            .await
        {
            return e;
        }

        // Resolve agent
        let context = match self.get_agent_context().await {
            Ok(context) => context,
            Err(e) => return json!({"error": e}).to_string(),
        };
        let agent = match AgentService::get_agent_for_context(&context) {
            Ok(a) => a,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let did = agent.did.clone();

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

        let perspective_id = &params.0.perspective_id;
        let query = build_mention_query(&names, &did).to_string();
        let mention_query = build_mentioning_query(&did).to_string();
        let terms = mention_terms(&names, &did);
        let sub_id = build_mention_sub_id(perspective_id);

        let subscription = json!({
            "id": sub_id,
            "perspective": perspective_id,
            "class": TEXT_CONTENT_URI,
            "query": query,
            "terms": terms,
            "did": did,
            "mentionClass": MENTIONING_URI,
            "mentionQuery": mention_query,
        });

        json!({
            "did": did,
            "names": names,
            "perspective_id": perspective_id,
            "class": TEXT_CONTENT_URI,
            "query": query,
            "mention_class": MENTIONING_URI,
            "mention_query": mention_query,
            "terms": terms,
            "subscription": subscription,
            "message": format!(
                "Add the subscription entry to your waker config and restart the waker. \
                 It watches for Mentioning records whose `mentions` link {} and for {} in \
                 TextContent text, in perspective {}.",
                did,
                if names.is_empty() {
                    "the DID".to_string()
                } else {
                    format!("name(s) [{}] or the DID", names.join(", "))
                },
                perspective_id
            ),
        })
        .to_string()
    }
}

/// How many of the newest mention candidates the subscription holds. The waker keeps
/// the ids it has woken on and wakes only on new ones, so the window only has
/// to cover the mentions that arrive between two of its results; it keeps
/// what the executor re-sends on every change bounded however long the
/// neighbourhood's history grows.
pub const MENTION_WINDOW: usize = 50;

/// The distinct mention terms, profile names first and the DID last.
///
/// The waker gets them with the subscription (`terms`): the query below
/// matches text by substring, and the waker keeps a record found only through
/// its text when the text holds one of these as a whole word.
pub fn mention_terms(names: &[String], did: &str) -> Vec<String> {
    let mut terms: Vec<String> = Vec::new();
    for term in names.iter().map(String::as_str).chain(std::iter::once(did)) {
        if !term.is_empty() && !terms.iter().any(|t| t == term) {
            terms.push(term.to_string());
        }
    }
    terms
}

/// The model query that finds the agent named in text: every record whose
/// `text` contains a profile name or the DID, in a link another agent wrote,
/// newest first, at most [`MENTION_WINDOW`], over the built-in
/// `TextContent { text }` (#1446), so every class with a text property takes
/// part under whatever name it aliases to `text`. Mention links are the other
/// query's ([`build_mentioning_query`]).
///
/// Each branch carries `author: { not: did }`, a per-link author: a record
/// matches only through a link someone else wrote. A reply that names its own
/// author is not a mention of that author, and waking on it loops the bot
/// (#1459).
///
/// `contains` compiles to a case-insensitive `CONTAINS` and matches
/// substrings ("data" is in "metadata"); model query has no word match. So
/// the rows carry `text`, and the waker keeps one only when its text holds a
/// term as a whole word (`WakerSubscriptionManager`). `links: ["text"]`
/// returns each row's text links with their author, timestamp and signature
/// verdict (`__links`): the waker takes the mention's provenance from the
/// newest verified one by another agent whose own text names the agent
/// (#1576).
///
/// Pure over its inputs, so it unit-tests without an agent or perspective.
pub fn build_mention_query(names: &[String], did: &str) -> serde_json::Value {
    let any_term: Vec<serde_json::Value> = mention_terms(names, did)
        .iter()
        .map(|term| json!({ "text": { "contains": term, "author": { "not": did } } }))
        .collect();
    json!({
        "where": { "OR": any_term },
        "order": { "timestamp": "DESC" },
        "limit": MENTION_WINDOW,
        "properties": ["text"],
        "links": ["text"],
    })
}

/// The model query that finds mention links to `did`: every record whose
/// `mentions` relation links `did` in a link another agent wrote, newest
/// first, at most [`MENTION_WINDOW`], over the built-in
/// `Mentioning { mentions }`. Every class with a relation named or aliased
/// `mentions` takes part, with text or without: WE puts a post's mention on
/// the post's root, which has no text (#1557), and the root's
/// `CollectionBlock` has `mentions` over `we://mention` like every WE block.
/// A record no class describes is not read: a mention is a relation of a
/// class.
///
/// `author: { not: did }` is a per-link author, as in the text query: a link
/// the agent wrote itself is not a mention of it (#1459). The model query
/// reads only links whose signature verified (`includeUnverified` is off), so
/// a forged mention link, which may claim any author, matches nothing.
///
/// `links: ["mentions"]` returns each row's mention links with their author,
/// timestamp and signature verdict (`__links`, from the links' reifiers,
/// which no peer link can write, #1564): the waker hands the woken agent the
/// newest verified one to its DID by another agent as the mention's
/// provenance (#1563). A row wakes the agent whatever its text says (WE's
/// chip label may run into the next word, "@Datai have…").
///
/// Pure over its input, so it unit-tests without an agent or perspective.
pub fn build_mentioning_query(did: &str) -> serde_json::Value {
    json!({
        "where": { "mentions": { "eq": did, "author": { "not": did } } },
        "order": { "timestamp": "DESC" },
        "limit": MENTION_WINDOW,
        "properties": ["mentions"],
        "links": ["mentions"],
    })
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

    /// Clients built against the pre-rename schema still send
    /// `parent_address`; both spellings must land in `parent`.
    #[test]
    fn subscribe_params_accept_legacy_parent_address() {
        let legacy: SubscribeToModelParams = serde_json::from_str(
            r#"{"perspective_id":"p","class_name":"Message","parent_address":"chan://1"}"#,
        )
        .expect("legacy spelling deserializes");
        assert_eq!(legacy.parent.as_deref(), Some("chan://1"));

        let current: SubscribeToModelParams = serde_json::from_str(
            r#"{"perspective_id":"p","class_name":"Message","parent":"chan://2"}"#,
        )
        .expect("current spelling deserializes");
        assert_eq!(current.parent.as_deref(), Some("chan://2"));
    }

    /// The text query over TextContent: one `contains` per distinct term
    /// (names first, the DID last), each through a link the agent did not
    /// write; newest first, bounded, with `text` for the waker's word check and
    /// the text links for its provenance. Terms go in as they are: the store
    /// compares case-insensitively, and a JSON value needs no escaping. The
    /// store-backed behaviour (which records match) is in `instances/tests.rs`.
    #[test]
    fn mention_query_ors_a_text_contains_per_term() {
        let q = build_mention_query(
            &["Alice".into(), "a\"b".into(), "Alice".into(), "".into()],
            "did:key:zAgent",
        );
        let not_me = json!({ "not": "did:key:zAgent" });
        assert_eq!(
            q,
            json!({
                "where": { "OR": [
                    { "text": { "contains": "Alice", "author": not_me } },
                    { "text": { "contains": "a\"b", "author": not_me } },
                    { "text": { "contains": "did:key:zAgent", "author": not_me } },
                ] },
                "order": { "timestamp": "DESC" },
                "limit": MENTION_WINDOW,
                "properties": ["text"],
                "links": ["text"],
            })
        );
        // It is a query the model query engine accepts.
        let parsed: crate::perspectives::model_query::ModelQueryInput =
            serde_json::from_value(q).expect("a model query");
        assert_eq!(parsed.limit, Some(MENTION_WINDOW));
        assert_eq!(
            mention_terms(
                &["Alice".into(), "".into(), "Alice".into()],
                "did:key:zAgent"
            ),
            vec!["Alice".to_string(), "did:key:zAgent".to_string()]
        );
    }

    /// The mention query over Mentioning: `mentions` linking the DID through a
    /// link the agent did not write, newest first, bounded, with the mention
    /// links for the waker's provenance. The store-backed behaviour is in
    /// `instances/tests.rs`.
    #[test]
    fn mentioning_query_reads_mention_links_to_the_did_by_others() {
        let q = build_mentioning_query("did:key:zAgent");
        assert_eq!(
            q,
            json!({
                "where": { "mentions": {
                    "eq": "did:key:zAgent",
                    "author": { "not": "did:key:zAgent" },
                } },
                "order": { "timestamp": "DESC" },
                "limit": MENTION_WINDOW,
                "properties": ["mentions"],
                "links": ["mentions"],
            })
        );
        let parsed: crate::perspectives::model_query::ModelQueryInput =
            serde_json::from_value(q).expect("a model query");
        assert_eq!(parsed.limit, Some(MENTION_WINDOW));
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
        // A target value containing a `"` must not be able to close the
        // SPARQL string literal early and inject a second CONTAINS(...)
        // clause (or worse) into the FILTER.
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

    /// A children-subscription parent in the store's canonical single-colon
    /// spelling must be watched verbatim. The old `contains("://")` check
    /// re-wrapped it into `literal:string:literal%3Astring%3A…` — a node no
    /// link ever points at — so the subscription matched nothing and never
    /// fired (found live in the 2026-09-15 wake test).
    #[test]
    fn test_waker_parent_wrapping_is_idempotent_for_stored_ids() {
        use crate::mcp::tools::Ad4mMcpHandler;
        let stored = "literal:string:wake-test-general";
        let once = Ad4mMcpHandler::wrap_bare_as_literal(stored);
        assert_eq!(once, stored, "already-wrapped parent must pass through");
        // And what add_child stores for a bare name is exactly what a later
        // subscribe call produces for the same bare name — one spelling, both
        // directions.
        let wrapped_bare = Ad4mMcpHandler::wrap_bare_as_literal("wake-test-general");
        assert_eq!(wrapped_bare, stored);
        assert_eq!(Ad4mMcpHandler::wrap_bare_as_literal(&wrapped_bare), stored);
        // Other id shapes pass through untouched.
        assert_eq!(
            Ad4mMcpHandler::wrap_bare_as_literal("ad4m://obj/abc"),
            "ad4m://obj/abc"
        );
        assert_eq!(
            Ad4mMcpHandler::wrap_bare_as_literal("did:key:z6Mk123"),
            "did:key:z6Mk123"
        );
    }
}
