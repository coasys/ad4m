//! Subscription / waker query tools
//!
//! Tools for generating SPARQL queries for external waker processes.
//! Queries are derived from SHACL class definitions when available,
//! avoiding hardcoded type-specific predicates.

use super::Ad4mMcpHandler;
use crate::agent::AgentService;
use crate::mcp::shacl;
use crate::perspectives::model_query::builtin_classes::CONTAINER_URI;
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
/// and can inject arbitrary SPARQL into the mention-matching FILTER. Also
/// escapes CR/LF/TAB, which SPARQL STRING_LITERAL2 forbids raw.
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
        description = "Generate a single waker subscription config entry that watches for mentions of this agent by name(s) or DID in a neighbourhood. Watches for any link whose target contains a mention term as a whole word, case-insensitively (\"data\" matches \"hi Data!\", not \"metadata\"), and skips links the agent authored itself. The waker plugin resolves parent channels in a second query when the subscription fires. Returns one subscription ORing all profile names and the full DID. Agents should call this once per neighbourhood they join and add the returned waker_config entry to their waker config file, then restart the waker. Profile names (username, given_name, family_name) are all included; name_override adds an extra alias without replacing them."
    )]
    pub async fn get_mention_waker_config(
        &self,
        params: Parameters<MentionWakerConfigParams>,
    ) -> String {
        let perspective = match self
            .get_readable_perspective(&params.0.perspective_id)
            .await
        {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };

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

        // Append name_override as an additional alias (does not replace profile names)
        if let Some(ref override_name) = params.0.name_override {
            if !override_name.is_empty() && !names.contains(override_name) {
                names.push(override_name.clone());
            }
        }

        let perspective_id = &params.0.perspective_id;

        // Discover the body property predicate from SHACL to scope the query.
        // Without this, the query scans ALL links which is very slow on large perspectives.
        let body_predicate = shacl::load_class_properties(&perspective, "Message")
            .await
            .iter()
            .find(|prop| prop.name.to_lowercase() == "body")
            .and_then(|prop| prop.predicate.clone())
            .filter(|p| validate_sparql_iri(p).is_ok());

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
/// A term matches as a whole word, case-insensitively, in the parsed literal
/// target: "Data" matches "hi Data!" and "@data", not "metadata". Each term is
/// regex-escaped and wrapped in `(?:^|\W)…(?:\W|$)` rather than `\b…\b`, so a
/// name that starts or ends with punctuation still matches. Oxigraph runs
/// `REGEX` on the Rust `regex` crate, which is what `regex::escape` escapes for.
/// `body_predicate` scopes the scan to the message-body predicate when SHACL
/// resolves it (much faster on large perspectives).
///
/// Both the scoped and the unscoped-fallback query exclude `ad4m://ontology/*`
/// predicates (author DID, timestamp, proofKey — see `sparql_store`). Without
/// that exclusion the DID search term matches every link the agent itself
/// authored (its own author-DID proof metadata), so the agent would wake on its
/// own writes rather than on real mentions.
///
/// Links the agent authored itself never match either: a reply that names its
/// own author is not a mention of that author, and waking on it loops the bot.
///
/// Search terms (profile names, DID) are interpolated into the query, so each is
/// escaped as a SPARQL string literal — a name containing `"` or `\` would
/// otherwise break out of the REGEX literal and could inject query syntax.
///
/// Pure over its inputs, so it unit-tests without an agent or perspective.
pub fn build_mention_query(names: &[String], did: &str, body_predicate: Option<&str>) -> String {
    let mention_conditions: Vec<String> = names
        .iter()
        .map(String::as_str)
        .chain(std::iter::once(did))
        .map(|term| {
            let pattern = format!(r"(?:^|\W){}(?:\W|$)", regex::escape(term));
            format!(
                "REGEX(STR(<ad4m://fn/parse_literal>(?target)), \"{}\", \"i\")",
                escape_sparql_literal(&pattern)
            )
        })
        .collect();
    let mention_predicate = format!("({})", mention_conditions.join(" || "));
    // Never match proof metadata, else the DID term self-matches authored links.
    let ontology_guard = "FILTER(!STRSTARTS(STR(?predicate), \"ad4m://ontology/\"))";
    let own_writes_guard = format!(
        "FILTER NOT EXISTS {{ ?reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> ; <ad4m://ontology/author> \"{}\" . }}",
        escape_sparql_literal(did)
    );
    match body_predicate {
        Some(pred) => format!(
            "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(STR(?predicate) = \"{}\") {} FILTER({}) {} }}",
            pred, ontology_guard, mention_predicate, own_writes_guard
        ),
        None => format!(
            "SELECT ?source ?predicate ?target WHERE {{ ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) {} FILTER({}) {} }}",
            ontology_guard, mention_predicate, own_writes_guard
        ),
    }
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
        assert!(q.contains("Alice"), "first name included");
        assert!(q.contains("Bob"), "second name included");
        assert!(q.contains("did:key:zABC"), "DID included");
        assert_eq!(
            q.matches(", \"i\")").count(),
            3,
            "every term case-insensitive"
        );
        assert!(q.contains("parse_literal"), "decodes literal targets");
    }

    #[test]
    fn query_escapes_quotes_and_backslashes_in_terms() {
        // A term containing a double-quote or backslash must not break out of the
        // REGEX string literal (SPARQL injection guard). `regex::escape` doubles
        // the backslash, then the SPARQL literal escape doubles both again.
        let q = build_mention_query(&["a\"b\\c".to_string()], "did:key:zX", None);
        assert!(
            q.contains("a\\\"b\\\\\\\\c"),
            "quote + backslash escaped: {q}"
        );
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

    /// Run the mention query against a real store holding one body link per
    /// `(author, source, text)` and return the matched sources, sorted.
    fn mention_matches(
        did: &str,
        names: &[&str],
        bodies: &[(&crate::agent::signatures::TestSigner, &str, &str)],
    ) -> Vec<String> {
        use crate::perspectives::sparql_store::SparqlStore;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let store = SparqlStore::new(None).unwrap();
        for (signer, source, text) in bodies {
            let data = Link {
                source: source.to_string(),
                predicate: Some("flux://body".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(text)),
            };
            let signed = signer.sign(data.normalize());
            store
                .add_link(&LinkExpression {
                    author: signed.author,
                    timestamp: signed.timestamp,
                    data: signed.data,
                    proof: signed.proof,
                    status: Some(LinkStatus::Shared),
                })
                .unwrap();
        }
        let names: Vec<String> = names.iter().map(|n| n.to_string()).collect();
        let query = build_mention_query(&names, did, Some("flux://body"));
        let rows: serde_json::Value =
            serde_json::from_str(&store.query_arbitrary(&query).expect(&query)).unwrap();
        let mut sources: Vec<String> = rows
            .as_array()
            .unwrap()
            .iter()
            .map(|row| row["source"].as_str().unwrap().to_string())
            .collect();
        sources.sort();
        sources
    }

    #[test]
    fn mention_query_matches_the_name_as_a_whole_word_only() {
        // e2e 2026-10-15: "data" woke the bot named Data on "metadata".
        let me = crate::agent::signatures::TestSigner::generate();
        let other = crate::agent::signatures::TestSigner::generate();
        let matched = mention_matches(
            &me.did,
            &["Data"],
            &[
                (&other, "ad4m://m1", "hi Data!"),
                (&other, "ad4m://m2", "the metadata is stale"),
                (&other, "ad4m://m3", "DATA, look"),
                (&other, "ad4m://m4", "a database row"),
                (&other, "ad4m://m5", "@data"),
                (&other, "ad4m://m6", "data"),
                (&other, "ad4m://m7", "Data's turn"),
            ],
        );
        assert_eq!(
            matched,
            vec![
                "ad4m://m1",
                "ad4m://m3",
                "ad4m://m5",
                "ad4m://m6",
                "ad4m://m7"
            ]
        );
    }

    #[test]
    fn mention_query_treats_regex_characters_in_names_literally() {
        let me = crate::agent::signatures::TestSigner::generate();
        let other = crate::agent::signatures::TestSigner::generate();
        let matched = mention_matches(
            &me.did,
            &["J.R. (bot)"],
            &[
                (&other, "ad4m://m1", "ask J.R. (bot) please"),
                (&other, "ad4m://m2", "ask JxRx (bot) please"),
            ],
        );
        assert_eq!(matched, vec!["ad4m://m1"]);
        // Quote and backslash survive both escapes and still match themselves.
        let matched = mention_matches(
            &me.did,
            &["a\"b\\c"],
            &[
                (&other, "ad4m://m1", "hi a\"b\\c!"),
                (&other, "ad4m://m2", "hi a\"bc!"),
            ],
        );
        assert_eq!(matched, vec!["ad4m://m1"]);
    }

    #[test]
    fn mention_query_skips_the_agents_own_writes() {
        // e2e 2026-10-15: a bot's reply naming itself woke the bot again.
        let me = crate::agent::signatures::TestSigner::generate();
        let other = crate::agent::signatures::TestSigner::generate();
        let did_mention = format!("cc {}", me.did);
        let matched = mention_matches(
            &me.did,
            &["data"],
            &[
                (&me, "ad4m://mine", "Data here, on it"),
                (&me, "ad4m://mine-did", &did_mention),
                (&other, "ad4m://theirs", "data, can you look?"),
                (&other, "ad4m://theirs-did", &did_mention),
            ],
        );
        assert_eq!(matched, vec!["ad4m://theirs", "ad4m://theirs-did"]);
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
