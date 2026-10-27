//! Subscription / waker query tools
//!
//! Tools for generating SPARQL and model queries for external waker processes.
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
        description = "Generate a single waker subscription config entry that watches for mentions of this agent in a neighbourhood. A mention is a `we://mention` link from the record that mentions the agent (a post, a message) to the agent's DID, which is how WE stores one; the name the text shows plays no part. It is a SPARQL query: the source of every such link another agent wrote, with a valid signature, newest first. Links the agent authored itself never match, so its own reply mentioning it does not wake it. Temporarily, until WE's transcriber writes that link, it also matches a spoken transcript line (`we://text_source` \"spoken\") that says one of the agent's profile names as a whole word; such a row carries `via: \"spoken-name\"`. Typed text never matches by name. The waker opens it with perspective.subscribeQuery and, when it fires, resolves the parent channels of each mentioned record in a second query. Agents should call this once per neighbourhood they join and add the returned waker_config entry to their waker config file, then restart the waker."
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
        let spoken_names = profile_names(&agent);

        let perspective_id = &params.0.perspective_id;
        let query = build_mention_query(&did, &spoken_names);
        let sub_id = build_mention_sub_id(perspective_id);

        let subscription = json!({
            "id": sub_id,
            "perspective": perspective_id,
            "query": query,
        });

        json!({
            "did": did,
            "perspective_id": perspective_id,
            "predicate": MENTION_PREDICATE,
            "spoken_names": spoken_names,
            "query": query,
            "subscription": subscription,
            "message": format!(
                "Add the subscription entry to your waker config and restart the waker. \
                 This single query watches for {} links to {} in perspective {}, \
                 and for spoken transcript lines that say {}.",
                MENTION_PREDICATE,
                did,
                perspective_id,
                if spoken_names.is_empty() {
                    "no name (your profile has none)".to_string()
                } else {
                    format!("[{}]", spoken_names.join(", "))
                }
            ),
        })
        .to_string()
    }
}

/// WE's mention relation: a record that mentions someone (a post's root, a
/// feed line) links to their DID with this predicate (`WeNode.mentions` in
/// WE's entities). The link is the mention; the text only shows a label for
/// it, whichever name WE found, so the waker watches the link.
pub const MENTION_PREDICATE: &str = "we://mention";

/// How many of the newest mention candidates the subscription holds. The waker keeps
/// the ids it has woken on and wakes only on new ones, so the window only has
/// to cover the mentions that arrive between two of its results; it keeps
/// what the executor re-sends on every change bounded however long the
/// neighbourhood's history grows.
pub const MENTION_WINDOW: usize = 50;

/// The SPARQL query that finds mentions of `did`: the source of every
/// [`MENTION_PREDICATE`] link to `did` that another agent wrote and whose
/// signature verifies, newest first, at most [`MENTION_WINDOW`] of them.
///
/// The link is joined to its reifier for its author, timestamp and proof
/// verdict, the same `rdf:reifies` + `ad4m://ontology/*` facts the model query
/// reads them from. Only the store writes those facts: it refuses any link in
/// that vocabulary (`sparql_store::is_link_metadata`, #1564), so no peer can
/// add an author or a verdict to someone else's mention. A link the agent wrote itself is not a mention of it: a
/// bot's own reply mentioning it would wake it again (#1459). An unverified
/// link may claim any author, so it does not count either.
///
/// The DID is compared as a string literal, escaped, rather than spliced in
/// as an IRI. `?source` needs no `isIRI` check: the store writes every link
/// source as an IRI (`sparql_store::make_direct_triple`), and only links
/// carry this predicate. Rows carry `source`, which the waker reads as the mentioned
/// record, and the mention link's provenance (#1563): `author`, `timestamp`
/// and `proofValid`, which the waker hands the woken agent with the record.
///
/// With `spoken_names`, the query also finds spoken lines that say one of
/// them, see [`spoken_name_mentions`]; their rows carry `via`.
/// Pure over its inputs, so it unit-tests without an agent.
pub fn build_mention_query(did: &str, spoken_names: &[String]) -> String {
    let did = escape_sparql_literal(did);
    let mention_links = format!(
        "?source <{MENTION_PREDICATE}> ?mentioned . \
         FILTER(STR(?mentioned) = \"{did}\") \
         ?reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <{MENTION_PREDICATE}> ?mentioned )>> ."
    );
    let mentions = match spoken_name_mentions(spoken_names) {
        Some(spoken) => format!("{{ {mention_links} }} UNION {{ {spoken} }}"),
        None => mention_links,
    };
    format!(
        "SELECT ?source ?author ?timestamp ?proofValid ?via WHERE {{ \
         {mentions} \
         ?reifier <ad4m://ontology/author> ?author . \
         ?reifier <ad4m://ontology/timestamp> ?timestamp . \
         ?reifier <ad4m://ontology/proofValid> ?proofValid . \
         FILTER(STR(?proofValid) = \"true\" && STR(?author) != \"{did}\") \
         }} ORDER BY DESC(?timestamp) LIMIT {MENTION_WINDOW}"
    )
}

// ----------------------------------------------------------------------------
// TEMPORARY: spoken-name fallback, until WE links spoken mentions.
//
// WE's transcriber writes a spoken line as a TextBlock with
// `we://text_source "spoken"` and no `we://mention` link, so a bot named in a
// call is not mentioned by link. Until the transcriber writes that link,
// the mention query also finds spoken lines that say the agent's name as a
// whole word (decided on #1557). Typed text never matches by name.
//
// To remove: delete this section and the `spoken_names` parameter of
// `build_mention_query`, its `UNION` and `?via`, and `profile_names` in
// `get_mention_waker_config`; in core, `MentionMessage.spokenName`; in the
// plugin, its wake-message lines; and the tests named `spoken_*`.
// ----------------------------------------------------------------------------

/// WE's `TextBlock.source` predicate (`we://text_source`).
pub const TEXT_SOURCE_PREDICATE: &str = "we://text_source";
/// The `TextBlock.source` value WE's transcriber writes: a recogniser heard
/// the line. `typed` and `corrected` lines are not matched by name.
pub const SPOKEN: &str = "spoken";
/// WE's `TextBlock.text` predicate: the block's words.
pub const TEXT_PREDICATE: &str = "we://text";
/// The `via` a spoken-name row carries; a `we://mention` row has none.
pub const VIA_SPOKEN_NAME: &str = "spoken-name";

/// The agent's names from its profile (`flux://profile` username, given and
/// family name), the names a speaker would say.
pub fn profile_names(agent: &crate::types::Agent) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for link in agent.perspective.iter().flat_map(|p| &p.links) {
        if link.data.source != "flux://profile" {
            continue;
        }
        if !matches!(
            link.data.predicate.as_deref(),
            Some("sioc://has_username" | "sioc://has_given_name" | "sioc://has_family_name")
        ) {
            continue;
        }
        let name = Ad4mMcpHandler::resolve_literal_value(&link.data.target)
            .trim()
            .to_string();
        if !name.is_empty() && !names.contains(&name) {
            names.push(name);
        }
    }
    names
}

/// The `UNION` branch of [`build_mention_query`] for spoken lines: a record
/// with `we://text_source "spoken"` whose `we://text` holds one of `names` as
/// a whole word, case-insensitively, joined to the text link's reifier, so
/// the shared filters keep only lines another agent spoke, verified, and the
/// row's provenance is the speaker's. `None` without names.
///
/// A word is letters, marks, digits and `_`, as the old waker filter had it:
/// "Data," matches, "metadata" does not. Each name is regex-escaped, then the
/// pattern is escaped as a SPARQL string literal.
pub fn spoken_name_mentions(names: &[String]) -> Option<String> {
    if names.is_empty() {
        return None;
    }
    const NOT_WORD: &str = r"[^\p{L}\p{M}\p{N}_]";
    let alternatives = names
        .iter()
        .map(|name| regex::escape(name))
        .collect::<Vec<_>>()
        .join("|");
    let pattern = escape_sparql_literal(&format!(
        "(?:^|{NOT_WORD})(?:{alternatives})(?:{NOT_WORD}|$)"
    ));
    Some(format!(
        "?source <{TEXT_SOURCE_PREDICATE}> ?textSource . \
         FILTER(STR(?textSource) = \"{SPOKEN}\") \
         ?source <{TEXT_PREDICATE}> ?text . \
         FILTER(REGEX(STR(?text), \"{pattern}\", \"i\")) \
         ?reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <{TEXT_PREDICATE}> ?text )>> . \
         BIND(\"{VIA_SPOKEN_NAME}\" AS ?via)"
    ))
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

    /// The mention query is SPARQL the waker subscribes to as is: it reads
    /// [`MENTION_PREDICATE`] links to the DID, through the link's reifier, by
    /// another author with a verified proof, newest first and bounded, and
    /// names the mentioned record `source`. A quote in the DID stays inside
    /// its string literal. The store-backed behaviour (which links match) is
    /// in `instances/tests.rs`.
    #[test]
    fn mention_query_reads_we_mention_links_to_the_did_by_others() {
        assert_eq!(MENTION_PREDICATE, "we://mention");
        let q = build_mention_query("did:key:zAgent", &[]);
        assert!(
            crate::perspectives::perspective_instance::is_sparql_query(&q),
            "{q}"
        );
        assert!(
            q.starts_with("SELECT ?source ?author ?timestamp ?proofValid "),
            "the record and its mention link's provenance: {q}"
        );
        assert!(
            q.contains("?source <we://mention> ?mentioned ."),
            "the mention link: {q}"
        );
        assert!(
            q.contains(r#"STR(?mentioned) = "did:key:zAgent""#),
            "to the DID: {q}"
        );
        assert!(
            q.contains("<<( ?source <we://mention> ?mentioned )>>"),
            "through that link's reifier: {q}"
        );
        assert!(
            q.contains(r#"FILTER(STR(?proofValid) = "true" && STR(?author) != "did:key:zAgent")"#),
            "verified, by someone else: {q}"
        );
        assert!(
            q.ends_with(&format!("ORDER BY DESC(?timestamp) LIMIT {MENTION_WINDOW}")),
            "newest first, bounded: {q}"
        );

        let hostile = build_mention_query(r#"did:x" || true || ""#, &[]);
        assert!(
            hostile.contains(r#"STR(?mentioned) = "did:x\" || true || \"""#),
            "{hostile}"
        );
    }

    /// TEMPORARY spoken-name fallback: the names a speaker would say are the
    /// profile's username, given and family name, each once, trimmed. Not its
    /// other fields, and not links outside `flux://profile`.
    #[test]
    fn spoken_names_are_the_profile_names() {
        use crate::types::domain::Perspective;
        use crate::types::{Agent, DecoratedLinkExpression, Link};
        let link = |source: &str, predicate: &str, value: &str| DecoratedLinkExpression {
            data: Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: Ad4mMcpHandler::encode_literal(value),
            },
            ..Default::default()
        };
        let agent = Agent {
            did: "did:key:zAgent".to_string(),
            perspective: Some(Perspective {
                links: vec![
                    link("flux://profile", "sioc://has_username", "Data"),
                    link("flux://profile", "sioc://has_given_name", " Data "),
                    link("flux://profile", "sioc://has_family_name", "Soong"),
                    link("flux://profile", "sioc://has_bio", "An android"),
                    link("flux://profile", "sioc://has_given_name", ""),
                    link("ad4m://elsewhere", "sioc://has_username", "Lore"),
                ],
            }),
            ..Default::default()
        };
        assert_eq!(profile_names(&agent), vec!["Data", "Soong"]);
        assert!(profile_names(&Agent::default()).is_empty());
    }

    /// TEMPORARY spoken-name fallback: without names the query is the link
    /// query alone; with names it adds a `UNION` branch over spoken lines.
    /// A quote in a name stays inside the REGEX's string literal.
    #[test]
    fn spoken_names_add_a_union_branch_over_spoken_lines() {
        assert!(spoken_name_mentions(&[]).is_none());
        assert!(!build_mention_query("did:key:zAgent", &[]).contains("UNION"));

        let q = build_mention_query("did:key:zAgent", &["Data".to_string()]);
        assert!(
            crate::perspectives::perspective_instance::is_sparql_query(&q),
            "{q}"
        );
        assert!(
            q.contains("} UNION { ?source <we://text_source> ?textSource ."),
            "{q}"
        );
        assert!(q.contains(r#"FILTER(STR(?textSource) = "spoken")"#), "{q}");
        assert!(q.contains(r#"BIND("spoken-name" AS ?via)"#), "{q}");

        let hostile = spoken_name_mentions(&[r#"x", "i")) || true || ("#.to_string()]).unwrap();
        let regex = hostile
            .split("REGEX(STR(?text), \"")
            .nth(1)
            .and_then(|rest| rest.split("\", \"i\")").next())
            .unwrap_or_else(|| panic!("{hostile}"));
        assert!(
            regex
                .char_indices()
                .all(|(i, c)| c != '"' || regex.as_bytes()[i - 1] == b'\\'),
            "an unescaped quote in {regex}"
        );
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
