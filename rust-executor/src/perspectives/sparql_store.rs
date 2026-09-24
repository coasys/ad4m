use crate::languages::literal::RFC3986_COMPONENT_ENCODE;
use crate::types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link, LinkExpression};
use chrono::DateTime as ChronoDateTime;
use deno_core::anyhow::{anyhow, Error};
use oxigraph::model::*;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::sync::Arc;

const ONT_AUTHOR: &str = "ad4m://ontology/author";
const ONT_TIMESTAMP: &str = "ad4m://ontology/timestamp";
const ONT_PROOF_KEY: &str = "ad4m://ontology/proofKey";
const ONT_PROOF_SIG: &str = "ad4m://ontology/proofSignature";
const ONT_PROOF_VALID: &str = "ad4m://ontology/proofValid";
const ONT_STATUS: &str = "ad4m://ontology/status";
/// The target exactly as the link was signed, kept only when it differs from
/// what [`storage_term_to_target_string`] renders for the stored term. See
/// [`signed_target_annotation`].
const ONT_WIRE_TARGET: &str = "ad4m://ontology/wireTarget";
const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";

/// Datatype IRI for AD4M JSON literals — used when a property holds a JSON
/// payload (objects/arrays) that should round-trip through the store without
/// being coerced into `xsd:string`.
const ONT_JSON: &str = "ad4m://json";

const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";

/// Convert a wire-format link target string to the oxigraph [`Term`] that
/// represents it in storage.
///
/// `literal:string:X`, `literal:number:N`, `literal:boolean:B`, and
/// `literal:json:J` are translated to typed RDF literals; anything else
/// (other URI schemes, raw IRIs, DIDs, plain strings used as IRIs) stays as a
/// [`NamedNode`].  Failure paths (malformed percent-encoding, unparseable
/// JSON) fall back to safe string literals rather than rejecting the input —
/// callers expect every wire value to round-trip.
fn target_to_storage_term(target: &str) -> Term {
    use percent_encoding::percent_decode_str;

    if let Some(body) = target.strip_prefix("literal:") {
        if let Some(rest) = body.strip_prefix("string:") {
            let decoded = percent_decode_str(rest)
                .decode_utf8()
                .map(|c| c.into_owned())
                .unwrap_or_else(|_| rest.to_string());
            return Literal::new_simple_literal(decoded).into();
        }
        if let Some(rest) = body.strip_prefix("number:") {
            // `literal_encode` emits integers without a fractional part and
            // decimals with one; preserve that distinction in the datatype.
            if rest.parse::<i64>().is_ok() {
                return Literal::new_typed_literal(rest, NamedNode::new_unchecked(XSD_INTEGER))
                    .into();
            }
            if rest.parse::<f64>().is_ok() {
                return Literal::new_typed_literal(rest, NamedNode::new_unchecked(XSD_DECIMAL))
                    .into();
            }
            // Unparseable — fall back to a plain string literal rather than
            // polluting the integer/decimal index with junk.
            return Literal::new_simple_literal(rest).into();
        }
        if let Some(rest) = body.strip_prefix("boolean:") {
            if rest == "true" || rest == "false" {
                return Literal::new_typed_literal(rest, NamedNode::new_unchecked(XSD_BOOLEAN))
                    .into();
            }
            return Literal::new_simple_literal(rest).into();
        }
        if let Some(rest) = body.strip_prefix("json:") {
            let decoded = percent_decode_str(rest)
                .decode_utf8()
                .map(|c| c.into_owned())
                .unwrap_or_else(|_| rest.to_string());
            // Re-serialise so the stored lexical form is canonical JSON; if
            // parsing fails, keep the decoded text under xsd:string so the
            // value at least round-trips even though it's not JSON.
            if let Ok(json_val) = serde_json::from_str::<Value>(&decoded) {
                let canonical = serde_json::to_string(&json_val).unwrap_or(decoded);
                return Literal::new_typed_literal(canonical, NamedNode::new_unchecked(ONT_JSON))
                    .into();
            }
            return Literal::new_simple_literal(decoded).into();
        }
    }

    Term::NamedNode(NamedNode::new_unchecked(target))
}

/// Inverse of [`target_to_storage_term`]: render a stored [`Term`] back into
/// the wire-format URL string the SDK / hydration layer expect.
///
/// Typed literals are re-encoded to their `literal:<kind>:` form so existing
/// consumers (TypeScript SDK, `parse_literal_value`) see the same shape they
/// did before the typed-literal migration.
fn storage_term_to_target_string(term: &Term) -> String {
    use percent_encoding::utf8_percent_encode;

    match term {
        Term::NamedNode(n) => n.as_str().to_string(),
        Term::Literal(l) => {
            let dt = l.datatype().as_str();
            let val = l.value();
            match dt {
                XSD_INTEGER | XSD_DECIMAL => format!("literal:number:{val}"),
                XSD_BOOLEAN => format!("literal:boolean:{val}"),
                ONT_JSON => {
                    let encoded = utf8_percent_encode(val, &RFC3986_COMPONENT_ENCODE).to_string();
                    format!("literal:json:{encoded}")
                }
                _ => {
                    let encoded = utf8_percent_encode(val, &RFC3986_COMPONENT_ENCODE).to_string();
                    format!("literal:string:{encoded}")
                }
            }
        }
        Term::BlankNode(b) => format!("_:{}", b.as_str()),
        Term::Triple(_) => String::new(),
    }
}

/// The `ad4m://ontology/wireTarget` value to store for a link, if any.
///
/// [`target_to_storage_term`] is lossy for `literal:*` targets: it
/// percent-decodes `string:` and `json:` payloads and re-serialises JSON, and
/// [`storage_term_to_target_string`] renders the result back with the
/// RFC 3986 set. A target signed in any other encoding (raw text, raw JSON,
/// `-_.~` escaped, JSON whitespace) would read back as different bytes, and a
/// read-back copy would no longer verify. Keeping the signed string beside the
/// canonical term makes every link-returning read hand back what was signed,
/// while the typed literal keeps value queries and the index unchanged.
/// Canonical targets and plain IRIs need no annotation, so the common case
/// costs nothing.
fn signed_target_annotation(link: &LinkExpression, target_term: &Term) -> Option<String> {
    let rendered = storage_term_to_target_string(target_term);
    (rendered != link.data.target).then(|| link.data.target.clone())
}

/// Heap entry for bounded top-N selection. Ordered by `(dt, seq)` ascending —
/// `seq` provides a stable tiebreaker so two links with identical timestamps
/// don't collide. Used by `SPARQLStore::query_links_top_n_by_timestamp`.
struct TimestampedLink {
    dt: ChronoDateTime<chrono::FixedOffset>,
    seq: u64,
    link: DecoratedLinkExpression,
}

impl PartialEq for TimestampedLink {
    fn eq(&self, other: &Self) -> bool {
        self.dt == other.dt && self.seq == other.seq
    }
}
impl Eq for TimestampedLink {}
impl PartialOrd for TimestampedLink {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for TimestampedLink {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.dt.cmp(&other.dt).then(self.seq.cmp(&other.seq))
    }
}

fn literal(val: &str) -> Literal {
    Literal::new_simple_literal(val)
}

fn status_str(status: &LinkStatus) -> &'static str {
    match status {
        LinkStatus::Shared => "Shared",
        LinkStatus::Local => "Local",
    }
}

/// Decode a stored `proofValid` annotation into a verdict.
///
/// The store holds a *boolean*, never a third "unevaluated" state:
/// [`SparqlStore::insert_link_triples`] writes the annotation unconditionally,
/// deriving the verdict from the signature when the caller did not supply one.
/// `proof.valid` is a read view over the signature — something the executor
/// computes so a UI or a SPARQL querier does not have to roll its own crypto —
/// and a read view has no reason to record that nobody looked yet.
///
/// The empty string means the annotation is absent, which this maps to `false`.
/// Absence is not reachable from anything this store writes; it can only come
/// from hand-seeded or foreign data, so it is answered fail-closed, the same way
/// every consumer that branches on the field (`signed_by`, the `flow_evaluator`
/// revocation filter, `get_sdna_facts`) reads an unverified link. Both SPARQL
/// read paths bind `?proofValid` through `OPTIONAL` and the solution accessors
/// yield `""` for an unbound variable, so this is the shape absence arrives in.
fn decode_proof_valid(s: &str) -> bool {
    s == "true"
}

/// Back-compat SPARQL function for legacy notification triggers and SDNA
/// queries that still call `<ad4m://fn/parse_literal>(?target)`.
///
/// The typed-literal storage layer means callers can usually just use
/// `STR(?target)` for `xsd:string` / `xsd:integer` / `xsd:decimal` /
/// `xsd:boolean` values directly, but signed-expression envelopes still
/// land in the store as `"{author,timestamp,data,proof}"^^ad4m:json` and
/// `STR` returns the full JSON rather than the inner `data` field. The
/// shim preserves that envelope-unwrapping behaviour for queries that
/// depend on it (e.g. Flux mention detection), and also keeps the old
/// `literal:string:` / `literal:json:` NamedNode forms working in case
/// any pre-migration link store still feeds them through.
fn parse_literal_fn(args: &[Term]) -> Option<Term> {
    if args.len() != 1 {
        return None;
    }
    match &args[0] {
        Term::Literal(l) => {
            let raw = l.value();
            let datatype = l.datatype().as_str();
            if let Some(parsed) = decode_literal_payload(raw, Some(datatype)) {
                return Some(Literal::new_simple_literal(parsed).into());
            }
            // Typed-literal fallback: convert to a plain xsd:string with the
            // same lexical form. Model-query WHERE clauses compare the result
            // of `<ad4m://fn/parse_literal>(?var)` against a string constant
            // (`"true"`, `"42"`, …) with SPARQL `=`, which is strict about
            // datatypes — `"true"^^xsd:boolean = "true"` returns false. The
            // pre-refactor code always did this simple-literal wrap for the
            // xsd:* datatypes; without it, boolean/integer/decimal WHEREs
            // silently return zero rows. Regression covered by
            // model_query::integration_tests::
            //   test_full_model_query_signed_expression_boolean_where
            Some(Literal::new_simple_literal(raw).into())
        }
        Term::NamedNode(n) => {
            let raw = n.as_str();
            if let Some(parsed) = decode_literal_payload(raw, None) {
                return Some(Literal::new_simple_literal(parsed).into());
            }
            // Not a `literal:` URI — hand back the NamedNode unchanged so
            // callers can still work with e.g. `ad4m://` addresses.
            Some(args[0].clone())
        }
        _ => Some(args[0].clone()),
    }
}

/// Extract the "parsed value" from a raw literal payload, mirroring the
/// production language's own `parseLiteral`. Returns `None` when the payload
/// isn't in one of the recognised envelope shapes — callers then hand back
/// the original term unchanged.
///
/// Recognised shapes, tried in order:
///   1. `literal:string:<url-encoded>` — URL-decode the tail
///   2. `literal:number:<n>` / `literal:boolean:<b>` — return the tail
///   3. `literal:json:<url-encoded JSON>` — URL-decode + parse; if it's a
///      signed-expression envelope (`{author,timestamp,data,proof}`), return
///      only `.data`; otherwise return the decoded JSON
///   4. `datatype == ad4m:json` — parse the value as JSON; if it's an
///      envelope with `.data`, return `.data`; otherwise return the value
///   5. Raw JSON that happens to be an envelope with `.data` — return `.data`
///      (regression guard: envelope-unwrapping used to fire regardless of
///      datatype, and the mention-detection path in Flux still depends on
///      that. See PR #880 CI failure on `fix/agent-harness-findings`.)
fn decode_literal_payload(raw: &str, datatype: Option<&str>) -> Option<String> {
    use percent_encoding::percent_decode_str;

    if let Some(body) = raw.strip_prefix("literal:") {
        if let Some(rest) = body.strip_prefix("string:") {
            return Some(
                percent_decode_str(rest)
                    .decode_utf8()
                    .map(|c| c.into_owned())
                    .unwrap_or_else(|_| rest.to_string()),
            );
        }
        if let Some(rest) = body.strip_prefix("number:") {
            return Some(rest.to_string());
        }
        if let Some(rest) = body.strip_prefix("boolean:") {
            return Some(rest.to_string());
        }
        if let Some(rest) = body.strip_prefix("json:") {
            let decoded = percent_decode_str(rest)
                .decode_utf8()
                .map(|c| c.into_owned())
                .unwrap_or_else(|_| rest.to_string());
            return Some(unwrap_envelope_or_original(&decoded));
        }
        // "literal:" prefix but unknown scheme — return the whole raw string
        // (matches the pre-refactor "return args[0].clone()" fallback).
        return None;
    }

    // No `literal:` prefix — this is the typed-literal storage path. Try
    // JSON-envelope unwrapping when either the RDF datatype says so or the
    // raw value happens to be a valid envelope; otherwise the caller keeps
    // the original term.
    let is_json_datatype = datatype.map(|d| d == ONT_JSON).unwrap_or(false);
    if is_json_datatype {
        return Some(unwrap_envelope_or_original(raw));
    }
    if let Some(unwrapped) = try_unwrap_envelope(raw) {
        return Some(unwrapped);
    }
    None
}

/// Parse `raw` as JSON; if it's an object with a `data` field, return the
/// field's string value (or a JSON re-serialisation of a non-string value).
/// Returns `None` when `raw` is not parseable JSON or has no `data` field.
fn try_unwrap_envelope(raw: &str) -> Option<String> {
    let json_val = serde_json::from_str::<Value>(raw).ok()?;
    let data = json_val.get("data")?;
    Some(match data {
        Value::String(s) => s.clone(),
        other => serde_json::to_string(other).unwrap_or_default(),
    })
}

/// As `try_unwrap_envelope`, but returns the original string when unwrapping
/// fails — used on paths that have already committed to returning *some*
/// string (e.g. after decoding `literal:json:`).
fn unwrap_envelope_or_original(raw: &str) -> String {
    try_unwrap_envelope(raw).unwrap_or_else(|| raw.to_string())
}

fn strip_html_fn(args: &[Term]) -> Option<Term> {
    if args.len() != 1 {
        return None;
    }
    let val = match &args[0] {
        Term::Literal(l) => l.value().to_string(),
        _ => return Some(args[0].clone()),
    };
    let mut result = String::with_capacity(val.len());
    let mut in_tag = false;
    for ch in val.chars() {
        match ch {
            '<' => in_tag = true,
            '>' => in_tag = false,
            _ if !in_tag => result.push(ch),
            _ => {}
        }
    }
    Some(Literal::new_simple_literal(&result).into())
}

/// Validates that a SPARQL query is read-only by parsing it with the SPARQL parser.
/// Only SELECT, ASK, CONSTRUCT, and DESCRIBE queries are accepted.
/// UPDATE operations (INSERT, DELETE, DROP, etc.) will fail to parse as a Query.
pub fn validate_readonly_query(query: &str) -> Result<(), Error> {
    let _ = SparqlEvaluator::new().parse_query(query).map_err(|e| {
        anyhow!(
            "Query is not valid read-only SPARQL (only SELECT/ASK/CONSTRUCT/DESCRIBE allowed): {}",
            e
        )
    })?;
    Ok(())
}

/// Generate a deterministic reifier IRI from link data + timestamp.
fn make_reifier_iri(link: &LinkExpression) -> NamedNode {
    // Hash the *normalized* storage-term form of the target, not the raw
    // wire string. `literal:json:` targets are canonicalized before storage
    // (see `target_to_storage_term`), so two callers describing the same
    // JSON value with different key order/whitespace would otherwise hash
    // to different reifier IRIs for what ends up being the identical stored
    // triple. `remove_link` recomputes this same hash from whatever
    // wire-format string the caller passes in (e.g. the hydrated form read
    // back from a query), so a mismatch here orphans the reifier metadata
    // and leaves the direct triple un-removable. Normalizing first makes
    // the hash a pure function of the stored term, independent of how the
    // caller happened to format an equivalent value. This is a no-op for
    // every other target shape (NamedNode / number / boolean round-trip
    // through `target_to_storage_term` unchanged).
    let normalized_target =
        storage_term_to_target_string(&target_to_storage_term(&link.data.target));

    let mut hasher = Sha256::new();
    hasher.update(link.author.as_bytes());
    hasher.update(link.data.source.as_bytes());
    hasher.update(link.data.predicate.as_deref().unwrap_or("").as_bytes());
    hasher.update(normalized_target.as_bytes());
    hasher.update(link.timestamp.as_bytes());
    let hash = hex::encode(hasher.finalize());
    NamedNode::new_unchecked(format!("link:{}", &hash[..32]))
}

/// Build the direct triple `(source, predicate, target)` for a link.
///
/// The target is rendered through [`target_to_storage_term`] so `literal:*`
/// wire-form targets become typed RDF literals in storage while plain IRIs
/// stay as [`NamedNode`]s.
fn make_direct_triple(link: &LinkExpression) -> (NamedNode, NamedNode, Term) {
    let source_iri = NamedNode::new_unchecked(&link.data.source);
    let predicate_val = link.data.predicate.as_deref().unwrap_or("");
    let predicate_iri = NamedNode::new_unchecked(predicate_val);
    let target_term = target_to_storage_term(&link.data.target);
    (source_iri, predicate_iri, target_term)
}

/// Oxigraph-backed SPARQL store for AD4M link data.
/// Uses RDF 1.2 reifiers: direct triples in default graph with metadata
/// attached via `rdf:reifies` triple terms.
///
/// # Storage Model
/// Each link is stored as:
/// 1. Direct triple: `<source> <predicate> <target> .` (default graph)
/// 2. Reifier: `<link:HASH> rdf:reifies <<( source predicate target )>> .`
/// 3. Metadata: `<link:HASH> ad4m://ontology/* "value" .` (default graph)
///
/// A `literal:*` target is stored as a typed literal, so its wire bytes are
/// not kept by the triple. When they differ from the canonical rendering, the
/// reifier also carries `ad4m://ontology/wireTarget`, and every read that
/// returns links (`get_all_links`, `query_links`, `model_query`'s `__links`)
/// hands back those signed bytes.
///
/// # Thread Safety
/// Oxigraph's `Store` is `Send + Sync` and uses internal locking for concurrent access.
#[derive(Clone)]
pub struct SparqlStore {
    store: Arc<Store>,
}

impl SparqlStore {
    /// Create a new SparqlStore.
    ///
    /// If `data_path` is `Some`, opens a persistent RocksDB-backed store at that path.
    /// If `data_path` is `None`, creates an in-memory store (useful for tests).
    pub fn new(data_path: Option<&str>) -> Result<Self, Error> {
        let store = match data_path {
            Some(path) => {
                let store_path = std::path::PathBuf::from(path).join("sparql_store");
                std::fs::create_dir_all(&store_path).map_err(|e| {
                    anyhow!(
                        "Failed to create SPARQL store directory {:?}: {}",
                        store_path,
                        e
                    )
                })?;
                log::info!("Opening persistent SPARQL store at {:?}", store_path);
                Store::open(&store_path).map_err(|e| {
                    anyhow!(
                        "Failed to open persistent SPARQL store at {:?}: {}",
                        store_path,
                        e
                    )
                })?
            }
            None => Store::new()?,
        };
        Ok(SparqlStore {
            store: Arc::new(store),
        })
    }

    /// Returns the number of quads in the store (for diagnostics).
    pub fn quad_count(&self) -> usize {
        self.store.len().unwrap_or(0)
    }

    /// Returns true if the store contains any quads beyond the migration marker.
    pub fn has_data(&self) -> bool {
        let migration_subj = NamedNodeRef::new_unchecked("ad4m://system/migration");
        self.store
            .quads_for_pattern(None, None, None, None)
            .any(|q| {
                q.as_ref()
                    .map(|quad| quad.subject != migration_subj.into())
                    .unwrap_or(false)
            })
    }

    fn insert_link_triples(&self, link: &LinkExpression) -> Result<(), Error> {
        let (source_iri, predicate_iri, target_term) = make_direct_triple(link);
        let reifier_iri = make_reifier_iri(link);

        // 1. Direct triple in default graph. `target_term` may be a typed
        //    literal (for `literal:*` wire values) or a NamedNode.
        let target_ref: TermRef = match &target_term {
            Term::NamedNode(n) => TermRef::NamedNode(n.as_ref()),
            Term::Literal(l) => TermRef::Literal(l.as_ref()),
            Term::BlankNode(b) => TermRef::BlankNode(b.as_ref()),
            Term::Triple(_) => {
                return Err(anyhow!(
                    "Triple-shaped target is not supported in link storage"
                ));
            }
        };
        self.store.insert(QuadRef::new(
            source_iri.as_ref(),
            predicate_iri.as_ref(),
            target_ref,
            GraphNameRef::DefaultGraph,
        ))?;

        // 2. Reifier: <link:HASH> rdf:reifies <<( source predicate target )>>
        //    Triple's object position accepts any Term, so a typed literal
        //    target reifies the same way a NamedNode target does.
        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);
        let triple_term = Triple::new(
            source_iri.clone(),
            predicate_iri.clone(),
            target_term.clone(),
        );
        self.store.insert(QuadRef::new(
            reifier_iri.as_ref(),
            rdf_reifies,
            TermRef::Triple(&triple_term),
            GraphNameRef::DefaultGraph,
        ))?;

        // 3. Metadata on the reifier node (all default graph)
        let proof = &link.proof;

        // `proof.valid` is a read view over the signature: always compute it from
        // the key and signature, never trust the caller's field. This means no
        // test helper, migration, or external caller can slip a fabricated or
        // stale verdict onto disk. The caller's `proof.valid` is intentionally
        // ignored here â tests that want `Some(true)` must carry a real signature.
        let valid_str = link.compute_proof_valid().to_string();

        // Status must be decided by the caller, not defaulted here. A silent
        // `None => Shared` fallback would let any write path that forgot to
        // set it mislabel a local link as shared — the kind of quiet
        // conflation that never surfaces in tests. Every production path
        // (add/update/batch, link-language ingest, migration, boot rebuild)
        // assigns status at its own boundary; a `None` reaching this point is
        // a bug, and refusing the insert makes it fail loudly.
        let status = link.status.as_ref().ok_or_else(|| {
            anyhow!(
                "Refusing to store link without an explicit local/shared status: {} -[{}]-> {}",
                link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                link.data.target
            )
        })?;

        let wire_target = signed_target_annotation(link, &target_term);

        // `None` writes nothing, after clearing a stale value: a canonical
        // re-insert must not keep the wire target of an earlier encoding.
        let annotations: Vec<(&str, Option<&str>)> = vec![
            (ONT_AUTHOR, Some(&link.author)),
            (ONT_TIMESTAMP, Some(&link.timestamp)),
            (ONT_PROOF_KEY, Some(&proof.key)),
            (ONT_PROOF_SIG, Some(&proof.signature)),
            (ONT_STATUS, Some(status_str(status))),
            (ONT_PROOF_VALID, Some(&valid_str)),
            (ONT_WIRE_TARGET, wire_target.as_deref()),
        ];

        for (pred_uri, value) in &annotations {
            let pred = NamedNodeRef::new_unchecked(pred_uri);

            // Delete before insert. `make_reifier_iri` hashes author, source,
            // predicate, target and timestamp — not the proof — so re-inserting
            // a link whose verdict, key or signature changed lands on the same
            // reifier. Without the delete the old annotation stays alongside the
            // new one: a link once stored "true" keeps reading back verified,
            // and the `OPTIONAL { ?reifier <proofValid> ?proofValid }` in
            // `query_links` matches both quads and returns the link twice.
            let stale: Vec<_> = self
                .store
                .quads_for_pattern(
                    Some(reifier_iri.as_ref().into()),
                    Some(pred),
                    None,
                    Some(GraphNameRef::DefaultGraph),
                )
                .collect::<Result<Vec<_>, _>>()?;
            for quad in &stale {
                self.store.remove(quad)?;
            }

            let Some(value) = value else { continue };
            let lit = literal(value);
            self.store.insert(QuadRef::new(
                reifier_iri.as_ref(),
                pred,
                TermRef::Literal(lit.as_ref()),
                GraphNameRef::DefaultGraph,
            ))?;
        }

        Ok(())
    }

    /// Insert triples for a link into the store.
    pub fn add_link(&self, link: &LinkExpression) -> Result<(), Error> {
        self.insert_link_triples(link)
    }

    /// Remove all triples for a link from the store.
    pub fn remove_link(&self, link: &LinkExpression) -> Result<(), Error> {
        let reifier_iri = make_reifier_iri(link);

        // 1. Remove all quads where reifier is subject (metadata + rdf:reifies)
        let quads: Vec<_> = self
            .store
            .quads_for_pattern(
                Some(reifier_iri.as_ref().into()),
                None,
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()?;
        for quad in &quads {
            self.store.remove(quad)?;
        }

        // 2. Remove the direct triple IF no other reifier references it
        let (source, predicate, target_term) = make_direct_triple(link);
        let triple_term = Triple::new(source.clone(), predicate.clone(), target_term.clone());
        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);

        let still_referenced = self
            .store
            .quads_for_pattern(
                None,
                Some(rdf_reifies),
                Some(TermRef::Triple(&triple_term)),
                None,
            )
            .next()
            .is_some();

        if !still_referenced {
            let target_ref: TermRef = match &target_term {
                Term::NamedNode(n) => TermRef::NamedNode(n.as_ref()),
                Term::Literal(l) => TermRef::Literal(l.as_ref()),
                Term::BlankNode(b) => TermRef::BlankNode(b.as_ref()),
                Term::Triple(_) => return Ok(()),
            };
            self.store.remove(QuadRef::new(
                source.as_ref(),
                predicate.as_ref(),
                target_ref,
                GraphNameRef::DefaultGraph,
            ))?;
        }

        Ok(())
    }

    /// Return all links in the store using a SPARQL 1.2 reifier query.
    pub fn get_all_links(&self) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?source ?predicate ?target ?wireTarget ?author ?timestamp ?proofKey ?proofSig ?proofValid ?status WHERE {
                ?source ?predicate ?target .
                ?reifier rdf:reifies <<( ?source ?predicate ?target )>> .
                FILTER(isIRI(?source) && isIRI(?predicate))
                ?reifier <ad4m://ontology/author> ?author .
                ?reifier <ad4m://ontology/timestamp> ?timestamp .
                OPTIONAL { ?reifier <ad4m://ontology/proofKey> ?proofKey . }
                OPTIONAL { ?reifier <ad4m://ontology/proofSignature> ?proofSig . }
                OPTIONAL { ?reifier <ad4m://ontology/proofValid> ?proofValid . }
                OPTIONAL { ?reifier <ad4m://ontology/status> ?status . }
                OPTIONAL { ?reifier <ad4m://ontology/wireTarget> ?wireTarget . }
            }
        "#;

        let results = self
            .sparql_evaluator()
            .parse_query(query)
            .map_err(|e| anyhow!("Failed to parse get_all_links query: {}", e))?
            .on_store(&self.store)
            .execute()
            .map_err(|e| anyhow!("get_all_links query failed: {}", e))?;

        match results {
            QueryResults::Solutions(solutions) => {
                let mut links = Vec::new();
                for solution in solutions {
                    let solution = solution?;
                    if let Some(link) = self.link_from_solution(&solution) {
                        links.push(link);
                    }
                }
                Ok(links)
            }
            _ => Ok(Vec::new()),
        }
    }

    /// Query links matching optional filters using index-based pattern matching.
    /// Scans direct triples in the default graph, then looks up reifiers for metadata.
    /// `limit` truncates in iteration order (RocksDB scan order, not timestamp);
    /// callers that need a top-N page by timestamp should use
    /// [`Self::query_links_top_n_by_timestamp`] instead, which bounds memory.
    pub fn query_links(
        &self,
        source: Option<&str>,
        predicate: Option<&str>,
        target: Option<&str>,
        from_date: Option<&str>,
        until_date: Option<&str>,
        limit: Option<usize>,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        use std::ops::ControlFlow;
        // Bail out early on the zero-page case: the closure below pushes first,
        // then checks `links.len() >= lim`, so without this guard `Some(0)`
        // would return one element instead of zero.
        if matches!(limit, Some(0)) {
            return Ok(Vec::new());
        }
        let mut links = Vec::new();
        self.for_each_matched_link(source, predicate, target, from_date, until_date, |link| {
            links.push(link);
            match limit {
                Some(lim) if links.len() >= lim => ControlFlow::Break(()),
                _ => ControlFlow::Continue(()),
            }
        })?;
        Ok(links)
    }

    /// Returns at most `limit` links matching the filters, sorted by RFC3339
    /// timestamp (ascending if `reverse=false`, descending if `reverse=true`).
    /// Uses a bounded heap of size `limit` so memory stays O(limit) regardless
    /// of how many links match — avoids the O(N) materialise-sort-truncate
    /// path when callers only want a top-N page (e.g. paginated `get_links`).
    pub fn query_links_top_n_by_timestamp(
        &self,
        source: Option<&str>,
        predicate: Option<&str>,
        target: Option<&str>,
        from_date: Option<&str>,
        until_date: Option<&str>,
        limit: usize,
        reverse: bool,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        use std::cmp::Reverse;
        use std::collections::BinaryHeap;
        use std::ops::ControlFlow;

        if limit == 0 {
            return Ok(Vec::new());
        }

        let mut seq: u64 = 0;
        if !reverse {
            // Ascending: keep K smallest timestamps. BinaryHeap is a max-heap,
            // so evicting the top whenever size exceeds `limit` retains
            // exactly the K smallest.
            let mut heap: BinaryHeap<TimestampedLink> = BinaryHeap::with_capacity(limit + 1);
            self.for_each_matched_link(source, predicate, target, from_date, until_date, |link| {
                let dt = ChronoDateTime::parse_from_rfc3339(&link.timestamp).unwrap_or_default();
                heap.push(TimestampedLink { dt, seq, link });
                seq += 1;
                if heap.len() > limit {
                    heap.pop();
                }
                ControlFlow::Continue(())
            })?;
            let mut out: Vec<DecoratedLinkExpression> = Vec::with_capacity(heap.len());
            while let Some(r) = heap.pop() {
                out.push(r.link);
            }
            // heap pops largest-first → out is currently DESC; flip to ASC.
            out.reverse();
            Ok(out)
        } else {
            // Descending: keep K largest. `Reverse` inverts the heap so it
            // evicts the smallest whenever size exceeds `limit`.
            let mut heap: BinaryHeap<Reverse<TimestampedLink>> =
                BinaryHeap::with_capacity(limit + 1);
            self.for_each_matched_link(source, predicate, target, from_date, until_date, |link| {
                let dt = ChronoDateTime::parse_from_rfc3339(&link.timestamp).unwrap_or_default();
                heap.push(Reverse(TimestampedLink { dt, seq, link }));
                seq += 1;
                if heap.len() > limit {
                    heap.pop();
                }
                ControlFlow::Continue(())
            })?;
            let mut out: Vec<DecoratedLinkExpression> = Vec::with_capacity(heap.len());
            while let Some(Reverse(r)) = heap.pop() {
                out.push(r.link);
            }
            // Reverse-heap pops smallest-first → out is currently ASC; flip to DESC.
            out.reverse();
            Ok(out)
        }
    }

    /// Iterate over all links matching the given filters, calling `callback`
    /// for each. The callback returns `ControlFlow::Break(())` to stop early.
    /// Shared by [`Self::query_links`] and
    /// [`Self::query_links_top_n_by_timestamp`] so the (gnarly) RocksDB
    /// reifier walk lives in exactly one place.
    fn for_each_matched_link<F>(
        &self,
        source: Option<&str>,
        predicate: Option<&str>,
        target: Option<&str>,
        from_date: Option<&str>,
        until_date: Option<&str>,
        mut callback: F,
    ) -> Result<(), Error>
    where
        F: FnMut(DecoratedLinkExpression) -> std::ops::ControlFlow<()>,
    {
        use std::ops::ControlFlow;

        let source_node = source.map(|s| NamedNode::new_unchecked(s));
        let predicate_node = predicate.map(|p| NamedNode::new_unchecked(p));
        // Targets pass through the same wire→Term translation used at write
        // time so a wildcard query for a typed-literal target hits the same
        // POS index entry the write created.
        let target_term = target.map(target_to_storage_term);

        let s_ref = source_node.as_ref().map(|n| n.as_ref().into());
        let p_ref = predicate_node.as_ref().map(|n| n.as_ref());
        let t_ref: Option<TermRef> = target_term.as_ref().map(|t| match t {
            Term::NamedNode(n) => TermRef::NamedNode(n.as_ref()),
            Term::Literal(l) => TermRef::Literal(l.as_ref()),
            Term::BlankNode(b) => TermRef::BlankNode(b.as_ref()),
            Term::Triple(_) => {
                TermRef::NamedNode(NamedNodeRef::new_unchecked("ad4m://unreachable"))
            }
        });

        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);

        // Search direct triples in the default graph
        for quad_result in
            self.store
                .quads_for_pattern(s_ref, p_ref, t_ref, Some(GraphNameRef::DefaultGraph))
        {
            let quad = quad_result?;

            // Skip reifier and metadata predicates — only process data triples
            let pred_str = quad.predicate.as_str();
            if pred_str == RDF_REIFIES || pred_str.starts_with("ad4m://ontology/") {
                continue;
            }

            let src = match &quad.subject {
                NamedOrBlankNode::NamedNode(n) => n.as_str().to_string(),
                _ => continue,
            };
            let pred = quad.predicate.as_str().to_string();
            // Re-serialise stored typed literals back into the wire-format
            // `literal:*:` URL so downstream consumers see the shape the SDK
            // expects, regardless of how the value is held in oxigraph.
            let tgt = match &quad.object {
                Term::NamedNode(_) | Term::Literal(_) => {
                    storage_term_to_target_string(&quad.object)
                }
                _ => continue,
            };

            // Build triple term for reifier lookup
            let triple_term = Triple::new(
                quad.subject.clone(),
                quad.predicate.clone(),
                quad.object.clone(),
            );

            // Find all reifiers for this triple
            for reifier_quad in self.store.quads_for_pattern(
                None,
                Some(rdf_reifies),
                Some(TermRef::Triple(&triple_term)),
                Some(GraphNameRef::DefaultGraph),
            ) {
                let rq = reifier_quad?;
                let reifier_node = match &rq.subject {
                    NamedOrBlankNode::NamedNode(n) => n,
                    _ => continue,
                };

                let reifier_subject: NamedOrBlankNodeRef = reifier_node.as_ref().into();

                // Fetch ALL annotations in one pattern scan instead of 6
                // separate quads_for_pattern calls. Each call to oxigraph's
                // quads_for_pattern materialises an iterator (and likely a
                // RocksDB snapshot); for a 10K-row query that's 6 × 10K =
                // 60K iterator allocations on the hot path. One pass cuts
                // that to 1 per link.
                let mut author = String::new();
                let mut timestamp = String::new();
                let mut proof_key = String::new();
                let mut proof_sig = String::new();
                let mut proof_valid_str = String::new();
                let mut status_val = String::new();
                let mut wire_target = None;
                for ann_quad in self.store.quads_for_pattern(
                    Some(reifier_subject),
                    None,
                    None,
                    Some(GraphNameRef::DefaultGraph),
                ) {
                    let aq = ann_quad?;
                    let pred_str = aq.predicate.as_str();
                    let value = match &aq.object {
                        Term::Literal(l) => l.value().to_string(),
                        _ => continue,
                    };
                    match pred_str {
                        ONT_AUTHOR => author = value,
                        ONT_TIMESTAMP => timestamp = value,
                        ONT_PROOF_KEY => proof_key = value,
                        ONT_PROOF_SIG => proof_sig = value,
                        ONT_PROOF_VALID => proof_valid_str = value,
                        ONT_STATUS => status_val = value,
                        ONT_WIRE_TARGET => wire_target = Some(value),
                        _ => {}
                    }
                }

                // Skip links without required metadata
                if author.is_empty() || timestamp.is_empty() {
                    continue;
                }

                // Apply date filters using proper DateTime parsing
                // (string comparison is unreliable when subsecond precision differs)
                if let Some(from) = from_date {
                    match (
                        ChronoDateTime::parse_from_rfc3339(timestamp.as_str()),
                        ChronoDateTime::parse_from_rfc3339(from),
                    ) {
                        (Ok(ts), Ok(fd)) => {
                            if ts < fd {
                                continue;
                            }
                        }
                        _ => {
                            // Fallback to string comparison if parsing fails
                            if timestamp.as_str() < from {
                                continue;
                            }
                        }
                    }
                }
                if let Some(until) = until_date {
                    match (
                        ChronoDateTime::parse_from_rfc3339(timestamp.as_str()),
                        ChronoDateTime::parse_from_rfc3339(until),
                    ) {
                        (Ok(ts), Ok(ud)) => {
                            if ts > ud {
                                continue;
                            }
                        }
                        _ => {
                            // Fallback to string comparison if parsing fails
                            if timestamp.as_str() > until {
                                continue;
                            }
                        }
                    }
                }

                let proof_valid = decode_proof_valid(&proof_valid_str);
                let status = match status_val.as_str() {
                    "Local" => Some(LinkStatus::Local),
                    "Shared" => Some(LinkStatus::Shared),
                    _ => None,
                };

                let link = DecoratedLinkExpression {
                    author,
                    timestamp,
                    data: Link {
                        source: src.clone(),
                        predicate: if pred.is_empty() {
                            None
                        } else {
                            Some(pred.clone())
                        },
                        target: wire_target.unwrap_or_else(|| tgt.clone()),
                    },
                    proof: DecoratedExpressionProof {
                        key: proof_key,
                        signature: proof_sig,
                        valid: Some(proof_valid),
                        invalid: Some(!proof_valid),
                    },
                    status,
                };

                if let ControlFlow::Break(_) = callback(link) {
                    return Ok(());
                }
            }
        }

        Ok(())
    }

    /// Find a specific link by source, predicate, target, author, and timestamp.
    pub fn get_link(
        &self,
        source: &str,
        predicate: Option<&str>,
        target: &str,
        author: &str,
        timestamp: &str,
    ) -> Result<Option<DecoratedLinkExpression>, Error> {
        let links = self.query_links(Some(source), predicate, Some(target), None, None, None)?;
        Ok(links
            .into_iter()
            .find(|l| l.author == author && l.timestamp == timestamp))
    }

    /// Get all links with the given source.
    pub fn get_links_by_source(&self, source: &str) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.query_links(Some(source), None, None, None, None, None)
    }

    /// Get all links with the given target.
    pub fn get_links_by_target(&self, target: &str) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.query_links(None, None, Some(target), None, None, None)
    }

    /// Get all links with the given predicate.
    pub fn get_links_by_predicate(
        &self,
        predicate: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.query_links(None, Some(predicate), None, None, None, None)
    }

    /// Get all links with the given predicate where source ends with the given suffix.
    pub fn get_links_by_predicate_and_source_suffix(
        &self,
        predicate: &str,
        source_suffix: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let links = self.get_links_by_predicate(predicate)?;
        Ok(links
            .into_iter()
            .filter(|l| l.data.source.ends_with(source_suffix))
            .collect())
    }

    fn sparql_evaluator(&self) -> SparqlEvaluator {
        SparqlEvaluator::new()
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/strip_html"),
                strip_html_fn,
            )
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/parse_literal"),
                parse_literal_fn,
            )
    }

    fn link_from_solution(
        &self,
        solution: &oxigraph::sparql::QuerySolution,
    ) -> Option<DecoratedLinkExpression> {
        let source = match solution.get("source")? {
            Term::NamedNode(n) => n.as_str().to_string(),
            _ => return None,
        };
        let predicate = match solution.get("predicate")? {
            Term::NamedNode(n) => {
                let s = n.as_str().to_string();
                if s.is_empty() {
                    None
                } else {
                    Some(s)
                }
            }
            _ => return None,
        };
        // SPARQL solutions may bind ?target to either a NamedNode (raw IRI
        // target) or a typed Literal (post-typed-literal-migration storage).
        // Both are serialised back to the wire-format `literal:*:` form via
        // `storage_term_to_target_string` so callers see a stable shape,
        // unless the link was signed over another encoding (`?wireTarget`).
        let target = match solution.get("target")? {
            t @ Term::NamedNode(_) | t @ Term::Literal(_) => match solution.get("wireTarget") {
                Some(Term::Literal(w)) => w.value().to_string(),
                _ => storage_term_to_target_string(t),
            },
            _ => return None,
        };

        let get_str = |var: &str| -> String {
            solution
                .get(var)
                .and_then(|t| match t {
                    Term::Literal(l) => Some(l.value().to_string()),
                    Term::NamedNode(n) => Some(n.as_str().to_string()),
                    _ => None,
                })
                .unwrap_or_default()
        };

        let author = get_str("author");
        let timestamp = get_str("timestamp");
        let proof_key = get_str("proofKey");
        let proof_sig = get_str("proofSig");
        let proof_valid_str = get_str("proofValid");
        let proof_valid = decode_proof_valid(&proof_valid_str);
        let status_val = get_str("status");
        let status = match status_val.as_str() {
            "Local" => Some(LinkStatus::Local),
            "Shared" => Some(LinkStatus::Shared),
            "" => None,
            _ => None,
        };

        Some(DecoratedLinkExpression {
            author,
            timestamp,
            data: Link {
                source,
                predicate,
                target,
            },
            proof: DecoratedExpressionProof {
                key: proof_key,
                signature: proof_sig,
                valid: Some(proof_valid),
                invalid: Some(!proof_valid),
            },
            status,
        })
    }

    /// Execute a read-only SPARQL SELECT query, returning a JSON string.
    /// All data lives in the default graph — no union graph needed.
    ///
    /// Applies wire-format hydration to `?target`/`?t` bindings — this is
    /// the internal convention AD4M's own hydration/relation/projection
    /// query builders rely on. For query text supplied by an external or
    /// untrusted caller (who has no reason to expect — or want — a
    /// same-named variable of their own silently reformatted), use
    /// [`Self::query_arbitrary`] instead.
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
        self.query_internal(query_string, true)
    }

    /// Execute a caller-supplied read-only SPARQL SELECT query (e.g. the
    /// `perspective.querySparql` RPC). Unlike [`Self::query`], this never
    /// re-encodes `?target`/`?t` bindings as wire-format `literal:*:`
    /// strings — those names are purely an internal convention, and an
    /// external caller choosing the same name for something unrelated
    /// (say, `SELECT ?target WHERE { ?r <ad4m://ontology/author> ?target }`)
    /// should get its plain lexical value back, not a silently mangled one.
    pub fn query_arbitrary(&self, query_string: &str) -> Result<String, Error> {
        self.query_internal(query_string, false)
    }

    fn query_internal(
        &self,
        query_string: &str,
        hydrate_target_vars: bool,
    ) -> Result<String, Error> {
        validate_readonly_query(query_string)?;

        let results = self
            .sparql_evaluator()
            .parse_query(query_string)
            .map_err(|e| anyhow!("Failed to parse SPARQL query: {}", e))?
            .on_store(&self.store)
            .execute()
            .map_err(|e| {
                let truncated = &query_string[..query_string.len().min(500)];
                anyhow!("SPARQL query failed: {}\nQuery: {}", e, truncated)
            })?;

        match results {
            QueryResults::Solutions(solutions) => {
                let vars: Vec<String> = solutions
                    .variables()
                    .iter()
                    .map(|v| v.as_str().to_string())
                    .collect();
                let mut rows = Vec::new();
                for solution in solutions {
                    let solution = solution?;
                    let mut row = serde_json::Map::new();
                    for var in &vars {
                        if let Some(term) = solution.get(var.as_str()) {
                            // Variables named `target` (or `t`) are special:
                            // hydration / relation collection / projection
                            // code consumes them as wire-format `literal:*:`
                            // URLs.  Round-tripping typed literals back
                            // through [`storage_term_to_target_string`]
                            // keeps the JSON shape the SDK expects (an
                            // `xsd:integer 5` becomes `literal:number:5` and
                            // hydrates to a JSON `5`, not the string `"5"`).
                            // All other variables emit the lexical form so
                            // SPARQL `FILTER(STR(?x) = ...)` and `COUNT`
                            // consumers continue to see the raw value. Only
                            // applied for internal callers — see
                            // `query_arbitrary` for external/untrusted queries.
                            let is_target_var =
                                hydrate_target_vars && (var == "target" || var == "t");
                            let val = match term {
                                Term::NamedNode(n) => Value::String(n.as_str().to_string()),
                                Term::Literal(l) => {
                                    if is_target_var {
                                        Value::String(storage_term_to_target_string(term))
                                    } else {
                                        Value::String(l.value().to_string())
                                    }
                                }
                                Term::BlankNode(b) => Value::String(format!("_:{}", b.as_str())),
                                Term::Triple(_) => Value::Null,
                            };
                            row.insert(var.clone(), val);
                        }
                    }
                    rows.push(Value::Object(row));
                }
                Ok(serde_json::to_string(&rows)?)
            }
            QueryResults::Boolean(b) => Ok(serde_json::to_string(&b)?),
            QueryResults::Graph(triples) => {
                let mut rows: Vec<serde_json::Map<String, Value>> = Vec::new();
                for triple_result in triples {
                    let triple = triple_result?;
                    let mut row = serde_json::Map::new();
                    row.insert(
                        "subject".to_string(),
                        Value::String(triple.subject.to_string()),
                    );
                    row.insert(
                        "predicate".to_string(),
                        Value::String(triple.predicate.to_string()),
                    );
                    row.insert(
                        "object".to_string(),
                        Value::String(triple.object.to_string()),
                    );
                    rows.push(row);
                }
                Ok(serde_json::to_string(&rows)?)
            }
        }
    }

    /// Async wrapper around `query()` that runs the blocking SPARQL operation
    /// on a dedicated thread pool to avoid blocking the tokio runtime.
    pub async fn query_async(&self, query_string: &str) -> Result<String, Error> {
        let store = self.clone();
        let query = query_string.to_string();
        tokio::task::spawn_blocking(move || store.query(&query))
            .await
            .map_err(|e| deno_core::anyhow::anyhow!("spawn_blocking join error: {}", e))?
    }

    /// Cancellation-aware async wrapper around `query()`.
    ///
    /// Races the SPARQL evaluation against `cancel.cancelled()`.  If the
    /// caller cancels mid-flight, this returns
    /// [`Error::msg("query cancelled")`] immediately and the result
    /// (whenever it arrives on the blocking thread) is dropped without
    /// being serialised to JSON or sent over the network.
    ///
    /// **Caveat — Oxigraph cannot be interrupted.**  The blocking thread
    /// continues running until Oxigraph returns, so this method does
    /// *not* free CPU that's already in flight.  What it does save:
    ///
    /// - JSON serialisation of the result (often the biggest single cost
    ///   for large result sets)
    /// - the WebSocket write back to the client
    /// - the client's deserialisation tax
    /// - any post-processing the caller would have done with the value
    ///
    /// For a long-running scan-all that would have shipped megabytes of
    /// SPARQL results back, that's still meaningful network + memory
    /// savings.  A future Oxigraph release with an interrupt hook would
    /// let us actually preempt the eval; the API here is forward-compatible
    /// with that.
    pub async fn query_cancellable(
        &self,
        query_string: &str,
        cancel: tokio_util::sync::CancellationToken,
    ) -> Result<String, Error> {
        if cancel.is_cancelled() {
            return Err(anyhow!("query cancelled"));
        }
        let store = self.clone();
        let query = query_string.to_string();
        // query_arbitrary, not query: this is the arbitrary-caller-supplied-
        // SPARQL entry point (matches the non-cancellable sparql_query path
        // one level up in perspective_instance.rs). `query()` re-encodes
        // external ?target/?t bindings into AD4M's internal literal:*: wire
        // format, which callers of an ad-hoc query string have no reason to
        // expect (CodeRabbit review, PR #855).
        let handle = tokio::task::spawn_blocking(move || store.query_arbitrary(&query));

        tokio::select! {
            biased;
            _ = cancel.cancelled() => {
                // We intentionally don't `handle.abort()`: a JoinHandle's
                // abort signal can't preempt a synchronous loop in
                // Oxigraph anyway, and abort tends to leave the blocking
                // pool in a degraded state under repeated cancellation.
                // The handle is dropped by `select!` when this arm wins;
                // the result will be discarded when it eventually arrives.
                Err(anyhow!("query cancelled"))
            }
            result = handle => {
                result.map_err(|e| deno_core::anyhow::anyhow!("spawn_blocking join error: {}", e))?
            }
        }
    }

    /// Remove all triples from the store.
    pub fn clear(&self) -> Result<(), Error> {
        self.store.clear()?;
        Ok(())
    }

    /// Flush pending writes to disk.
    /// This ensures RocksDB memtable data is written to SST files,
    /// allowing the memtable memory to be reclaimed.
    pub fn flush(&self) -> Result<(), Error> {
        self.store
            .flush()
            .map_err(|e| anyhow!("SPARQL store flush failed: {}", e))
    }

    /// Clear the store and bulk-insert all provided links.
    pub fn reload(&self, links: Vec<LinkExpression>) -> Result<(), Error> {
        self.clear()?;
        for link in &links {
            self.insert_link_triples(link)?;
        }
        self.flush()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::signatures::TestSigner;

    fn make_link(
        signer: &TestSigner,
        source: &str,
        predicate: &str,
        target: &str,
    ) -> LinkExpression {
        let data = Link {
            source: source.to_string(),
            predicate: if predicate.is_empty() {
                None
            } else {
                Some(predicate.to_string())
            },
            target: target.to_string(),
        };
        let signed = signer.sign(data.normalize());
        LinkExpression {
            author: signed.author,
            timestamp: signed.timestamp,
            data: signed.data,
            proof: signed.proof,
            status: Some(LinkStatus::Shared),
        }
    }

    fn make_link_with_ts(
        signer: &TestSigner,
        source: &str,
        predicate: &str,
        target: &str,
        ts: &str,
    ) -> LinkExpression {
        let data = Link {
            source: source.to_string(),
            predicate: if predicate.is_empty() {
                None
            } else {
                Some(predicate.to_string())
            },
            target: target.to_string(),
        };
        let signed = signer.sign_at(data.normalize(), ts.parse().expect("fixture timestamp"));
        LinkExpression {
            author: signed.author,
            timestamp: signed.timestamp,
            data: signed.data,
            proof: signed.proof,
            status: Some(LinkStatus::Shared),
        }
    }

    fn new_service() -> SparqlStore {
        SparqlStore::new(None).unwrap()
    }

    // ── Storage Model Tests (Reifier Model) ──

    #[test]
    fn test_add_link_creates_direct_triple() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(
            &signer,
            "ad4m://source1",
            "ad4m://predicate1",
            "ad4m://target1",
        );
        svc.add_link(&link).unwrap();

        // Direct triple should be in default graph
        let result = svc
            .query(
                "SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(isIRI(?s) && isIRI(?o) && ?p != <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>) }",
            )
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        let direct = rows.iter().find(|r| {
            r["s"].as_str() == Some("ad4m://source1")
                && r["p"].as_str() == Some("ad4m://predicate1")
                && r["o"].as_str() == Some("ad4m://target1")
        });
        assert!(direct.is_some(), "Direct triple not found. Got: {}", result);
    }

    #[test]
    fn test_add_link_creates_reifier() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        let reifier = make_reifier_iri(&link);

        // Reifier should reference the triple term
        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);
        let reifier_quads: Vec<_> = svc
            .store
            .quads_for_pattern(
                Some(reifier.as_ref().into()),
                Some(rdf_reifies),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(reifier_quads.len(), 1, "Expected 1 rdf:reifies triple");
    }

    #[test]
    fn test_add_link_creates_metadata_on_reifier() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        let reifier = make_reifier_iri(&link);

        // Query for metadata on the reifier IRI
        let result = svc
            .query(&format!(
                r#"SELECT ?p ?v WHERE {{
                <{}> ?p ?v .
                FILTER(STRSTARTS(STR(?p), "ad4m://ontology/"))
            }}"#,
                reifier.as_str()
            ))
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();

        let preds: Vec<&str> = rows.iter().filter_map(|r| r["p"].as_str()).collect();
        assert!(
            preds.contains(&"ad4m://ontology/author"),
            "Missing author annotation"
        );
        assert!(
            preds.contains(&"ad4m://ontology/timestamp"),
            "Missing timestamp annotation"
        );
        assert!(
            preds.contains(&"ad4m://ontology/proofKey"),
            "Missing proofKey annotation"
        );
        assert!(
            preds.contains(&"ad4m://ontology/proofSignature"),
            "Missing proofSig annotation"
        );
        assert!(
            preds.contains(&"ad4m://ontology/status"),
            "Missing status annotation"
        );

        let author_row = rows
            .iter()
            .find(|r| r["p"].as_str() == Some("ad4m://ontology/author"))
            .unwrap();
        assert_eq!(author_row["v"].as_str().unwrap(), signer.did.as_str());
    }

    #[test]
    fn test_remove_link_removes_direct_triple() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        svc.remove_link(&link).unwrap();

        // Direct triple should be gone
        let result = svc
            .query("SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(?s = <ad4m://src>) }")
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            rows.is_empty(),
            "Triple still exists after removal: {}",
            result
        );
    }

    #[test]
    fn test_remove_link_removes_reifier_and_metadata() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        let reifier = make_reifier_iri(&link);
        svc.remove_link(&link).unwrap();

        // No reifier triples should remain
        let result = svc
            .query(&format!(
                r#"SELECT ?p ?v WHERE {{ <{}> ?p ?v . }}"#,
                reifier.as_str()
            ))
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(rows.is_empty(), "Reifier triples still exist: {}", result);
    }

    #[test]
    fn test_remove_preserves_shared_direct_triple() {
        let signer = TestSigner::generate();
        let svc = new_service();
        // Two different links with same s/p/o but different timestamps
        let link1 = make_link_with_ts(
            &signer,
            "ad4m://src",
            "ad4m://pred",
            "ad4m://tgt",
            "2024-01-01T00:00:00Z",
        );
        let link2 = make_link_with_ts(
            &signer,
            "ad4m://src",
            "ad4m://pred",
            "ad4m://tgt",
            "2024-01-02T00:00:00Z",
        );
        svc.add_link(&link1).unwrap();
        svc.add_link(&link2).unwrap();

        // Remove link1 — direct triple should remain because link2 still references it
        svc.remove_link(&link1).unwrap();

        let all = svc.get_all_links().unwrap();
        assert_eq!(all.len(), 1, "Should have 1 link remaining");
        assert_eq!(all[0].author, signer.did);
    }

    #[test]
    fn test_no_named_graphs_used() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // No named graphs should exist
        let named: Vec<_> = svc
            .store
            .named_graphs()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(
            named.is_empty(),
            "No named graphs should be used in reifier model"
        );
    }

    #[test]
    fn test_all_data_in_default_graph() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // All quads should be in the default graph
        let all_quads: Vec<_> = svc
            .store
            .quads_for_pattern(None, None, None, None)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        for quad in &all_quads {
            assert_eq!(
                quad.graph_name,
                GraphName::DefaultGraph,
                "Found quad not in default graph: {:?}",
                quad
            );
        }

        // Should have: 1 direct triple + 1 reifier + 6 metadata = 8 quads
        assert_eq!(all_quads.len(), 8, "Expected 8 quads total");
    }

    /// The canonical storage model needs no migration: a `literal:*` wire target
    /// is stored as a native typed RDF literal on write and rendered back to the
    /// exact same wire form on read. This is the round-trip every scalar relies
    /// on — including `SHACLFlow`'s bookkeeping links, which encode stateName /
    /// stateValue / actionName as `literal:string:` / `literal:number:` targets
    /// via the same `Literal` helper the model layer uses, then read them back
    /// with `Literal.fromUrl(target)` (which requires the `literal:` wire form).
    /// If this holds, SHACLFlow round-trips on a fresh store with no migration.
    #[test]
    fn test_literal_targets_round_trip_to_wire_form_without_migration() {
        let signer = TestSigner::generate();
        let svc = new_service();

        // SHACLFlow-shaped bookkeeping links (string + number) and an ordinary
        // model-style string property — all added through the normal write path.
        svc.add_link(&make_link(
            &signer,
            "flow://TODO.ready",
            "ad4m://stateName",
            "literal:string:ready",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flow://TODO.ready",
            "ad4m://stateValue",
            "literal:number:0",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "task://1",
            "ns://title",
            "literal:string:Write%20the%20guide",
        ))
        .unwrap();

        let by = |source: &str, pred: &str| -> String {
            svc.get_all_links()
                .unwrap()
                .into_iter()
                .find(|l| l.data.source == source && l.data.predicate.as_deref() == Some(pred))
                .unwrap_or_else(|| panic!("link {source} {pred} not found"))
                .data
                .target
        };

        // Read returns the exact wire form SHACLFlow / Literal.fromUrl expects…
        assert_eq!(
            by("flow://TODO.ready", "ad4m://stateName"),
            "literal:string:ready"
        );
        assert_eq!(
            by("flow://TODO.ready", "ad4m://stateValue"),
            "literal:number:0"
        );
        assert_eq!(
            by("task://1", "ns://title"),
            "literal:string:Write%20the%20guide"
        );

        // …while the stored term is a native typed literal (indexed WHERE), not a
        // `literal:*` NamedNode IRI — so no migration is ever needed to reach the
        // indexed shape.
        let stored = svc
            .store
            .quads_for_pattern(
                Some(NamedNodeRef::new_unchecked("flow://TODO.ready").into()),
                Some(NamedNodeRef::new_unchecked("ad4m://stateName")),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .next()
            .unwrap()
            .unwrap();
        match stored.object {
            Term::Literal(ref l) => {
                assert_eq!(l.value(), "ready");
                assert_eq!(
                    l.datatype().as_str(),
                    "http://www.w3.org/2001/XMLSchema#string"
                );
            }
            other => panic!("expected a typed literal in storage, got {other:?}"),
        }
    }

    // ── Query Tests ──

    #[test]
    fn test_query_links_by_source() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://b", "ad4m://p", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://q", "ad4m://t3"))
            .unwrap();

        let results = svc
            .query_links(Some("ad4m://a"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.source == "ad4m://a"));
    }

    #[test]
    fn test_query_links_by_predicate() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://b", "ad4m://q", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://c", "ad4m://p", "ad4m://t3"))
            .unwrap();

        let results = svc
            .query_links(None, Some("ad4m://p"), None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);
        assert!(results
            .iter()
            .all(|l| l.data.predicate.as_deref() == Some("ad4m://p")));
    }

    #[test]
    fn test_query_links_by_target() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://b", "ad4m://q", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://c", "ad4m://r", "ad4m://t2"))
            .unwrap();

        let results = svc
            .query_links(None, None, Some("ad4m://t1"), None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.target == "ad4m://t1"));
    }

    #[test]
    fn test_query_links_by_source_and_predicate() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://q", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://b", "ad4m://p", "ad4m://t3"))
            .unwrap();

        let results = svc
            .query_links(Some("ad4m://a"), Some("ad4m://p"), None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t1");
    }

    #[test]
    fn test_query_links_by_source_predicate_target() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t2"))
            .unwrap();

        let results = svc
            .query_links(
                Some("ad4m://a"),
                Some("ad4m://p"),
                Some("ad4m://t1"),
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t1");
    }

    #[test]
    fn test_query_links_returns_metadata() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link_with_ts(
            &signer,
            "ad4m://s",
            "ad4m://p",
            "ad4m://t",
            "2024-06-01T12:00:00.000Z",
        );
        svc.add_link(&link).unwrap();

        let results = svc
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].author, signer.did);
        assert_eq!(results[0].timestamp, "2024-06-01T12:00:00.000Z");
        assert_eq!(results[0].proof.key, signer.key_id);
        assert!(!results[0].proof.signature.is_empty());
        assert_eq!(results[0].proof.valid, Some(true));
        assert_eq!(results[0].status, Some(LinkStatus::Shared));
    }

    #[test]
    fn test_query_links_date_filter() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link_with_ts(
            &signer,
            "ad4m://s",
            "ad4m://p",
            "ad4m://t1",
            "2024-01-01T00:00:00Z",
        ))
        .unwrap();
        svc.add_link(&make_link_with_ts(
            &signer,
            "ad4m://s",
            "ad4m://p",
            "ad4m://t2",
            "2024-06-15T00:00:00Z",
        ))
        .unwrap();
        svc.add_link(&make_link_with_ts(
            &signer,
            "ad4m://s",
            "ad4m://p",
            "ad4m://t3",
            "2024-12-31T00:00:00Z",
        ))
        .unwrap();

        // fromDate filter
        let results = svc
            .query_links(None, None, None, Some("2024-06-01T00:00:00Z"), None, None)
            .unwrap();
        assert_eq!(results.len(), 2);

        // untilDate filter
        let results = svc
            .query_links(None, None, None, None, Some("2024-06-30T00:00:00Z"), None)
            .unwrap();
        assert_eq!(results.len(), 2);

        // both
        let results = svc
            .query_links(
                None,
                None,
                None,
                Some("2024-06-01T00:00:00Z"),
                Some("2024-06-30T00:00:00Z"),
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t2");
    }

    #[test]
    fn test_query_links_limit() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..10 {
            svc.add_link(&make_link_with_ts(
                &signer,
                "ad4m://s",
                "ad4m://p",
                &format!("ad4m://t{}", i),
                &format!("2024-01-{:02}T00:00:00Z", i + 1),
            ))
            .unwrap();
        }

        let results = svc
            .query_links(None, None, None, None, None, Some(3))
            .unwrap();
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn test_sparql_query_direct_triple_pattern() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "flux://community1",
            "flux://has_channel",
            "flux://channel1",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://community1",
            "flux://has_channel",
            "flux://channel2",
        ))
        .unwrap();

        let result = svc
            .query(
                r#"SELECT ?channel WHERE {
                <flux://community1> <flux://has_channel> ?channel .
            }"#,
            )
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 2);
        let channels: Vec<&str> = rows.iter().filter_map(|r| r["channel"].as_str()).collect();
        assert!(channels.contains(&"flux://channel1"));
        assert!(channels.contains(&"flux://channel2"));
    }

    #[test]
    fn test_sparql_query_with_join() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "flux://ch1",
            "flux://entry_type",
            "flux://channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch1",
            "flux://name",
            "literal:string:general",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch2",
            "flux://entry_type",
            "flux://channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch2",
            "flux://name",
            "literal:string:random",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://msg1",
            "flux://entry_type",
            "flux://message",
        ))
        .unwrap();

        let result = svc
            .query(
                r#"SELECT ?ch ?name WHERE {
                ?ch <flux://entry_type> <flux://channel> .
                ?ch <flux://name> ?name .
            }"#,
            )
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 2);
    }

    // ── Sync / reload tests ──

    #[test]
    fn test_sync_existing_links_to_sparql() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let links = vec![
            make_link(&signer, "ad4m://a", "ad4m://p1", "ad4m://t1"),
            make_link(&signer, "ad4m://b", "ad4m://p2", "ad4m://t2"),
            make_link(&signer, "ad4m://c", "ad4m://p3", "ad4m://t3"),
        ];
        svc.reload(links).unwrap();

        let all = svc.get_all_links().unwrap();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn test_link_add_then_query_roundtrip() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(
            &signer,
            "literal:string:hello",
            "flux://has_channel",
            "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK",
        );
        svc.add_link(&link).unwrap();

        let results = svc
            .query_links(
                Some("literal:string:hello"),
                Some("flux://has_channel"),
                None,
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.source, "literal:string:hello");
        assert_eq!(
            results[0].data.predicate.as_deref(),
            Some("flux://has_channel")
        );
        assert_eq!(
            results[0].data.target,
            "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK"
        );
        assert_eq!(results[0].author, signer.did);
    }

    // ── Validate readonly ──

    #[test]
    fn test_validate_readonly_rejects_insert() {
        assert!(validate_readonly_query("INSERT DATA { <a> <b> <c> }").is_err());
    }

    #[test]
    fn test_validate_readonly_allows_select() {
        assert!(validate_readonly_query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }").is_ok());
    }

    // ── Clear / reload ──

    #[test]
    fn test_clear_removes_all() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        svc.clear().unwrap();
        let all = svc.get_all_links().unwrap();
        assert!(all.is_empty());
    }

    // ── Concurrent write protection tests ──

    #[test]
    fn test_concurrent_writes_no_panic() {
        let svc = new_service();
        let svc = Arc::new(svc);
        let mut handles = vec![];
        for thread_id in 0..10 {
            let svc = svc.clone();
            handles.push(std::thread::spawn(move || {
                let signer = TestSigner::generate();
                for i in 0..100 {
                    let link = make_link_with_ts(
                        &signer,
                        &format!("ad4m://src_{}", thread_id),
                        "ad4m://pred",
                        &format!("ad4m://tgt_{}_{}", thread_id, i),
                        &format!("2024-01-01T{:02}:{:02}:{:02}Z", thread_id, i / 60, i % 60),
                    );
                    svc.add_link(&link).unwrap();
                }
            }));
        }
        for h in handles {
            h.join().unwrap();
        }
        let all = svc.get_all_links().unwrap();
        assert_eq!(all.len(), 1000, "Expected 1000 links, got {}", all.len());
    }

    #[test]
    fn test_concurrent_read_during_write() {
        let svc = Arc::new(new_service());
        let svc_writer = svc.clone();
        let svc_reader = svc.clone();

        let writer = std::thread::spawn(move || {
            let signer = TestSigner::generate();
            for i in 0..200 {
                let link = make_link_with_ts(
                    &signer,
                    "ad4m://src",
                    "ad4m://pred",
                    &format!("ad4m://tgt_{}", i),
                    &format!("2024-01-01T00:{:02}:{:02}Z", i / 60, i % 60),
                );
                svc_writer.add_link(&link).unwrap();
            }
        });

        let reader = std::thread::spawn(move || {
            for _ in 0..200 {
                // Should never error, even during concurrent writes
                let _ = svc_reader
                    .query("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10")
                    .unwrap();
            }
        });

        writer.join().unwrap();
        reader.join().unwrap();
    }

    #[test]
    fn test_concurrent_removes_no_corruption() {
        let svc = Arc::new(new_service());
        let signer = TestSigner::generate();
        // Pre-generate all links so the same author+timestamp is used for both
        // add and remove (the reifier IRI hashes author + s/p/o + timestamp).
        let mut remove_links: Vec<LinkExpression> = Vec::new();
        for i in 0..100 {
            svc.add_link(&make_link_with_ts(
                &signer,
                "ad4m://keep",
                "ad4m://pred",
                &format!("ad4m://tgt_{}", i),
                &format!("2024-01-01T00:{:02}:{:02}Z", i / 60, i % 60),
            ))
            .unwrap();
            let remove = make_link_with_ts(
                &signer,
                "ad4m://remove",
                "ad4m://pred",
                &format!("ad4m://tgt_{}", i),
                &format!("2024-01-01T01:{:02}:{:02}Z", i / 60, i % 60),
            );
            svc.add_link(&remove).unwrap();
            remove_links.push(remove);
        }

        // Remove the "remove" links in parallel from 5 threads
        let remove_links = Arc::new(remove_links);
        let mut handles = vec![];
        for chunk_start in (0..100).step_by(20) {
            let svc = svc.clone();
            let remove_links = remove_links.clone();
            handles.push(std::thread::spawn(move || {
                for i in chunk_start..chunk_start + 20 {
                    svc.remove_link(&remove_links[i]).unwrap();
                }
            }));
        }
        for h in handles {
            h.join().unwrap();
        }

        let keep_links = svc
            .query_links(Some("ad4m://keep"), None, None, None, None, None)
            .unwrap();
        assert_eq!(keep_links.len(), 100, "Keep links corrupted");
        let remove_links = svc
            .query_links(Some("ad4m://remove"), None, None, None, None, None)
            .unwrap();
        assert_eq!(remove_links.len(), 0, "Remove links not fully removed");
    }

    // ── Persistence tests ──

    #[test]
    fn test_inmemory_store_for_tests() {
        let signer = TestSigner::generate();
        let svc = SparqlStore::new(None).unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        assert!(svc.has_data());
    }

    #[test]
    fn test_has_data_empty_store() {
        let svc = new_service();
        assert!(!svc.has_data());
    }

    #[test]
    fn test_has_data_after_add() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        assert!(svc.has_data());
    }

    // ── Error messages ──

    #[test]
    fn test_validation_error_is_descriptive() {
        let result = validate_readonly_query("NOT VALID SPARQL");
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("not valid read-only SPARQL"),
            "Validation error should be descriptive, got: {}",
            err_msg
        );
    }

    #[test]
    fn test_valid_query_on_empty_store_returns_ok() {
        let svc = new_service();
        let result = svc.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        assert!(result.is_ok());
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result.unwrap()).unwrap();
        assert_eq!(rows.len(), 0);
    }

    // ── Parser-based SPARQL validation ──

    #[test]
    fn test_rejects_insert_query() {
        assert!(validate_readonly_query("INSERT DATA { <a> <b> <c> }").is_err());
    }

    #[test]
    fn test_rejects_delete_query() {
        assert!(validate_readonly_query("DELETE DATA { <a> <b> <c> }").is_err());
    }

    #[test]
    fn test_accepts_valid_select() {
        assert!(validate_readonly_query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }").is_ok());
    }

    #[test]
    fn test_accepts_ask_query() {
        assert!(validate_readonly_query("ASK WHERE { ?s ?p ?o }").is_ok());
    }

    #[test]
    fn test_accepts_construct_query() {
        assert!(validate_readonly_query("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").is_ok());
    }

    #[test]
    fn test_accepts_describe_query() {
        assert!(validate_readonly_query("DESCRIBE <http://example.org>").is_ok());
    }

    // ── Reifier IRI tests ──

    #[test]
    fn test_reifier_iri_is_deterministic() {
        let signer = TestSigner::generate();
        let link = make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t");
        let iri1 = make_reifier_iri(&link);
        let iri2 = make_reifier_iri(&link);
        assert_eq!(iri1, iri2, "Same link data should produce same reifier IRI");
    }

    #[test]
    fn test_reifier_iri_differs_for_different_timestamps() {
        let signer = TestSigner::generate();
        let link1 = make_link_with_ts(
            &signer,
            "ad4m://a",
            "ad4m://p",
            "ad4m://t",
            "2024-01-01T00:00:00Z",
        );
        let link2 = make_link_with_ts(
            &signer,
            "ad4m://a",
            "ad4m://p",
            "ad4m://t",
            "2024-01-02T00:00:00Z",
        );
        let iri1 = make_reifier_iri(&link1);
        let iri2 = make_reifier_iri(&link2);
        assert_ne!(
            iri1, iri2,
            "Different timestamps should produce different reifier IRIs"
        );
    }

    // ── Direct query finds data without GRAPH pattern ──

    #[test]
    fn test_direct_query_finds_triples() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "ad4m://src",
            "ad4m://pred",
            "ad4m://tgt",
        ))
        .unwrap();

        // Direct query should find the triple without GRAPH wrapper
        let result = svc.query(
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(?s = <ad4m://src> && ?p = <ad4m://pred>) }"
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            !rows.is_empty(),
            "Direct query should find data triples in default graph"
        );
    }

    #[test]
    fn test_persistent_store_survives_drop() {
        let signer = TestSigner::generate();
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().to_str().unwrap();

        // Create store, add data, drop
        {
            let store = SparqlStore::new(Some(path)).unwrap();
            store
                .add_link(&make_link(
                    &signer,
                    "ad4m://src",
                    "ad4m://pred",
                    "ad4m://tgt",
                ))
                .unwrap();
            assert!(store.has_data());
        }

        // Reopen — data should still be there
        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(
                store.has_data(),
                "Persistent store should retain data after drop"
            );
            let links = store.get_all_links().unwrap();
            assert_eq!(links.len(), 1);
            assert_eq!(links[0].data.source, "ad4m://src");
            assert_eq!(links[0].data.predicate.as_deref(), Some("ad4m://pred"));
            assert_eq!(links[0].data.target, "ad4m://tgt");
        }
    }

    #[test]
    fn test_inmemory_store_loses_data_on_drop() {
        let signer = TestSigner::generate();
        {
            let store = SparqlStore::new(None).unwrap();
            store
                .add_link(&make_link(
                    &signer,
                    "ad4m://src",
                    "ad4m://pred",
                    "ad4m://tgt",
                ))
                .unwrap();
            assert!(store.has_data());
        }

        let store2 = SparqlStore::new(None).unwrap();
        assert!(
            !store2.has_data(),
            "In-memory store should lose data after drop"
        );
    }

    #[test]
    fn test_has_data_skips_rebuild_for_persistent_store() {
        let signer = TestSigner::generate();
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().to_str().unwrap();

        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(!store.has_data());
            store
                .add_link(&make_link(&signer, "ad4m://a", "ad4m://b", "ad4m://c"))
                .unwrap();
            assert!(store.has_data());
        }

        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(store.has_data());
        }
    }

    // ── query_links optimization tests ──

    #[test]
    fn query_links_by_source() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..50 {
            let src = format!("ad4m://source{}", i);
            let link = make_link(&signer, &src, "ad4m://pred", "ad4m://target");
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(Some("ad4m://source7"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.source, "ad4m://source7");
    }

    #[test]
    fn query_links_by_predicate() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..20 {
            let pred = format!("ad4m://pred{}", i % 4);
            let src = format!("ad4m://src{}", i);
            let link = make_link(&signer, &src, &pred, "ad4m://tgt");
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(None, Some("ad4m://pred2"), None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 5);
        for r in &results {
            assert_eq!(r.data.predicate.as_deref(), Some("ad4m://pred2"));
        }
    }

    #[test]
    fn query_links_by_target() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..10 {
            let tgt = format!("ad4m://tgt{}", i % 3);
            let src = format!("ad4m://src{}", i);
            let link = make_link(&signer, &src, "ad4m://pred", &tgt);
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(None, None, Some("ad4m://tgt1"), None, None, None)
            .unwrap();
        assert!(results.len() >= 3);
        for r in &results {
            assert_eq!(r.data.target, "ad4m://tgt1");
        }
    }

    #[test]
    fn query_links_date_range() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let timestamps = [
            "2024-01-10T00:00:00.000Z",
            "2024-01-15T00:00:00.000Z",
            "2024-01-20T00:00:00.000Z",
            "2024-01-25T00:00:00.000Z",
        ];
        for (i, ts) in timestamps.iter().enumerate() {
            let src = format!("ad4m://src{}", i);
            let link = make_link_with_ts(&signer, &src, "ad4m://pred", "ad4m://tgt", ts);
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(
                None,
                None,
                None,
                Some("2024-01-14T00:00:00.000Z"),
                Some("2024-01-21T00:00:00.000Z"),
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn query_links_with_limit() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..20 {
            let src = format!("ad4m://src{}", i);
            let link = make_link(&signer, &src, "ad4m://pred", "ad4m://tgt");
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(None, None, None, None, None, Some(5))
            .unwrap();
        assert_eq!(results.len(), 5);
    }

    #[test]
    fn query_links_with_limit_zero_returns_empty() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..5 {
            let src = format!("ad4m://src{}", i);
            svc.add_link(&make_link(&signer, &src, "ad4m://pred", "ad4m://tgt"))
                .unwrap();
        }
        // The closure-based collector pushes a link before checking the limit,
        // so without the early-return for `Some(0)` this would return 1 row.
        let results = svc
            .query_links(None, None, None, None, None, Some(0))
            .unwrap();
        assert!(
            results.is_empty(),
            "query_links(.., Some(0)) must return zero rows, got {} — \
             zero-page semantics regressed",
            results.len()
        );
    }

    #[test]
    fn query_links_top_n_ascending_returns_oldest() {
        let signer = TestSigner::generate();
        let svc = new_service();
        // 10 links with timestamps spaced one day apart, ts9 newest.
        for i in 0..10 {
            let ts = format!("2024-01-{:02}T00:00:00.000Z", i + 1);
            let src = format!("ad4m://src{}", i);
            let link = make_link_with_ts(&signer, &src, "ad4m://pred", "ad4m://tgt", &ts);
            svc.add_link(&link).unwrap();
        }
        // Top 3 ascending = the 3 oldest, sorted oldest→newest.
        let results = svc
            .query_links_top_n_by_timestamp(None, None, None, None, None, 3, false)
            .unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].timestamp, "2024-01-01T00:00:00.000Z");
        assert_eq!(results[1].timestamp, "2024-01-02T00:00:00.000Z");
        assert_eq!(results[2].timestamp, "2024-01-03T00:00:00.000Z");
    }

    #[test]
    fn query_links_top_n_descending_returns_newest() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..10 {
            let ts = format!("2024-01-{:02}T00:00:00.000Z", i + 1);
            let src = format!("ad4m://src{}", i);
            let link = make_link_with_ts(&signer, &src, "ad4m://pred", "ad4m://tgt", &ts);
            svc.add_link(&link).unwrap();
        }
        // Top 3 descending = the 3 newest, sorted newest→oldest.
        let results = svc
            .query_links_top_n_by_timestamp(None, None, None, None, None, 3, true)
            .unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].timestamp, "2024-01-10T00:00:00.000Z");
        assert_eq!(results[1].timestamp, "2024-01-09T00:00:00.000Z");
        assert_eq!(results[2].timestamp, "2024-01-08T00:00:00.000Z");
    }

    #[test]
    fn query_links_top_n_limit_exceeds_results() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..3 {
            let ts = format!("2024-01-{:02}T00:00:00.000Z", i + 1);
            let src = format!("ad4m://src{}", i);
            let link = make_link_with_ts(&signer, &src, "ad4m://pred", "ad4m://tgt", &ts);
            svc.add_link(&link).unwrap();
        }
        // Asking for 100 but only 3 exist — should return all 3 sorted.
        let results = svc
            .query_links_top_n_by_timestamp(None, None, None, None, None, 100, false)
            .unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].timestamp, "2024-01-01T00:00:00.000Z");
        assert_eq!(results[2].timestamp, "2024-01-03T00:00:00.000Z");
    }

    #[test]
    fn query_links_top_n_limit_zero_returns_empty() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        let results = svc
            .query_links_top_n_by_timestamp(None, None, None, None, None, 0, false)
            .unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn query_links_combined_filters() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://likes", "ad4m://b"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://knows", "ad4m://c"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://b", "ad4m://likes", "ad4m://c"))
            .unwrap();
        svc.add_link(&make_link(&signer, "ad4m://a", "ad4m://likes", "ad4m://c"))
            .unwrap();

        let results = svc
            .query_links(
                Some("ad4m://a"),
                Some("ad4m://likes"),
                None,
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 2);
        for r in &results {
            assert_eq!(r.data.source, "ad4m://a");
            assert_eq!(r.data.predicate.as_deref(), Some("ad4m://likes"));
        }

        let results = svc
            .query_links(
                Some("ad4m://a"),
                Some("ad4m://likes"),
                Some("ad4m://b"),
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://b");
    }

    #[test]
    fn query_links_empty_store() {
        let svc = new_service();
        let results = svc.query_links(None, None, None, None, None, None).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn query_links_no_filters_returns_all() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..10 {
            let src = format!("ad4m://src{}", i);
            svc.add_link(&make_link(&signer, &src, "ad4m://pred", "ad4m://tgt"))
                .unwrap();
        }
        let results = svc.query_links(None, None, None, None, None, None).unwrap();
        assert_eq!(results.len(), 10);
    }

    #[test]
    fn test_query_links_skips_literal_targets() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "ad4m://src",
            "ad4m://pred",
            "ad4m://normal_target",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "ad4m://src",
            "ad4m://pred",
            "literal:string:hello",
        ))
        .unwrap();

        let results = svc
            .query_links(Some("ad4m://src"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);

        let results = svc
            .query_links(
                Some("ad4m://src"),
                None,
                Some("literal:string:hello"),
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "literal:string:hello");
    }

    #[test]
    fn test_query_links_same_source_many_predicates() {
        let signer = TestSigner::generate();
        let svc = new_service();
        for i in 0..100 {
            svc.add_link(&make_link(
                &signer,
                "ad4m://src",
                &format!("ad4m://pred_{}", i),
                &format!("ad4m://target_{}", i),
            ))
            .unwrap();
        }

        let results = svc
            .query_links(
                Some("ad4m://src"),
                Some("ad4m://pred_42"),
                None,
                None,
                None,
                None,
            )
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://target_42");

        let all = svc
            .query_links(Some("ad4m://src"), None, None, None, None, None)
            .unwrap();
        assert_eq!(all.len(), 100);
    }

    #[test]
    fn test_query_links_unicode_roundtrip() {
        let signer = TestSigner::generate();
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "ad4m://héllo",
            "ad4m://prédicat",
            "ad4m://目标",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "ad4m://emoji🎉",
            "ad4m://pred",
            "ad4m://target",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "ad4m://中文源",
            "ad4m://日本語述語",
            "ad4m://한국어대상",
        ))
        .unwrap();

        let results = svc
            .query_links(Some("ad4m://héllo"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://目标");

        let results = svc
            .query_links(Some("ad4m://emoji🎉"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);

        let results = svc
            .query_links(Some("ad4m://中文源"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(
            results[0].data.predicate.as_deref(),
            Some("ad4m://日本語述語")
        );
        assert_eq!(results[0].data.target, "ad4m://한국어대상");
    }

    #[test]
    fn test_query_links_date_range_exact_boundary() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let exact_ts = "2024-06-15T12:00:00.000Z";
        svc.add_link(&make_link_with_ts(
            &signer,
            "ad4m://s",
            "ad4m://p",
            "ad4m://t1",
            exact_ts,
        ))
        .unwrap();

        let results = svc
            .query_links(None, None, None, Some(exact_ts), None, None)
            .unwrap();
        assert_eq!(
            results.len(),
            1,
            "from_date at exact timestamp should include the link"
        );

        let results = svc
            .query_links(None, None, None, None, Some(exact_ts), None)
            .unwrap();
        assert_eq!(
            results.len(),
            1,
            "until_date at exact timestamp should include the link"
        );

        let results = svc
            .query_links(None, None, None, Some(exact_ts), Some(exact_ts), None)
            .unwrap();
        assert_eq!(results.len(), 1, "exact from+until should include the link");
    }

    // ── proof.valid is a derived boolean, never "unevaluated" ──
    //
    // `proof.valid` is a read view over the signature. The store's contract is
    // that `proofValid` is always present and always "true" or "false", and
    // that the value is always one a signature check produced — either the
    // caller's, or one the store derives when the caller left the field empty.
    //
    // Each test below names the mutation that turns it red. Every one of those
    // mutations was applied and confirmed red before this landed.

    /// Build a link whose signature genuinely verifies, with the verdict left
    /// uncomputed — the shape `migrate_links_from_rusqlite_to_sparql` used to
    /// hand the store.
    fn signed_link_without_verdict(signer: &TestSigner, source: &str) -> LinkExpression {
        let signed = signer.sign(
            Link {
                source: source.to_string(),
                predicate: Some("ad4m://pred".to_string()),
                target: "ad4m://tgt".to_string(),
            }
            .normalize(),
        );
        LinkExpression {
            author: signed.author,
            timestamp: signed.timestamp,
            data: signed.data,
            proof: signed.proof,
            status: Some(LinkStatus::Shared),
        }
    }

    /// Count the `proofValid` quads on a link's reifier. A stale verdict left
    /// behind by a re-insert shows up here as a second quad.
    fn proof_valid_quads(svc: &SparqlStore, link: &LinkExpression) -> Vec<String> {
        let reifier = make_reifier_iri(link);
        svc.store
            .quads_for_pattern(
                Some(reifier.as_ref().into()),
                Some(NamedNodeRef::new_unchecked(ONT_PROOF_VALID)),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .map(|q| match q.unwrap().object {
                Term::Literal(l) => l.value().to_string(),
                other => panic!("proofValid must be a literal, got {other:?}"),
            })
            .collect()
    }

    /// A caller that supplies no verdict gets one computed from the signature,
    /// not the absent annotation the tri-state used to write.
    ///
    /// Turns red on: deleting `.unwrap_or_else(|| link.compute_proof_valid())`
    /// in `insert_link_triples` and writing `proof.valid.unwrap_or(false)`
    /// instead — the pre-#1064 write. The link reads back `Some(false)`.
    #[test]
    fn store_derives_the_verdict_when_the_caller_supplies_none() {
        let svc = new_service();
        let signer = TestSigner::generate();
        let link = signed_link_without_verdict(&signer, "ad4m://derive-src");
        svc.add_link(&link).unwrap();

        let links = svc.get_all_links().unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].proof.valid,
            Some(true),
            "a genuinely valid signature with no caller verdict must be \
             verified by the store, not recorded as unevaluated or false — got \
             {:?}",
            links[0].proof.valid
        );
        assert_eq!(links[0].proof.invalid, Some(false));
    }

    /// The derivation is a real signature check, not a constant `true`.
    ///
    /// Turns red on: making the derivation a constant —
    /// `.unwrap_or_else(|| true)` in `insert_link_triples`. That mutation
    /// leaves `caller_supplied_false_is_stored_as_false` green, so only this
    /// test pins that the store actually checks the signature.
    #[test]
    fn store_derives_false_for_a_broken_signature() {
        let svc = new_service();
        let signer = TestSigner::generate();
        let mut link = signed_link_without_verdict(&signer, "ad4m://derive-bad");
        // Same length, still valid hex, wrong bytes: this reaches the crypto
        // and fails there, rather than erroring out of `hex::decode`.
        link.proof.signature = link.proof.signature.replace('a', "b");
        svc.add_link(&link).unwrap();

        let links = svc.get_all_links().unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].proof.valid,
            Some(false),
            "a tampered signature must derive as false"
        );
        assert_eq!(links[0].proof.invalid, Some(true));
    }

    /// A malformed proof envelope — the case where `verify` returns `Err`
    /// rather than `Ok(false)` — is a failed verification, not a panic and not
    /// a third state. `!!!` is not hex, so `hex::decode` errors.
    ///
    /// Turns red on: changing `verify_or_false` to return `true` on `Err`.
    /// It also catches a `.unwrap()`/`.expect()` there, as a panic.
    #[test]
    fn store_derives_false_when_verification_errors() {
        let svc = new_service();
        let signer = TestSigner::generate();
        let mut link = signed_link_without_verdict(&signer, "ad4m://derive-malformed");
        link.proof.signature = "!!!not-hex!!!".to_string();
        svc.add_link(&link).unwrap();

        let links = svc.get_all_links().unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].proof.valid,
            Some(false),
            "a proof that cannot even be parsed has not verified"
        );
    }

    /// Re-inserting a link whose signature changed must overwrite the stored verdict.
    ///
    /// `make_reifier_iri` hashes author, source, predicate, target and
    /// timestamp — not the proof — so the second insert lands on the same
    /// reifier. Before the delete-before-insert fix the old `proofValid` quad
    /// stayed: a link once stored "true" kept reading back verified, and
    /// `query_links` returned it twice because its `OPTIONAL` matched both
    /// quads.
    ///
    /// Turns red on: removing the stale collect-and-remove loop from
    /// `insert_link_triples`. Both assertions fail — two quads, and
    /// `get_all_links` yields the stale `Some(true)`.
    #[test]
    fn reinserting_a_link_overwrites_a_stale_verdict() {
        let svc = new_service();
        let signer = TestSigner::generate();
        let mut link = make_link(&signer, "ad4m://restale", "ad4m://pred", "ad4m://tgt");

        // First insert: real signature → proofValid "true"
        svc.add_link(&link).unwrap();

        // Corrupt the signature so the second insert computes "false".
        // make_reifier_iri hashes author/source/predicate/target/timestamp, NOT
        // the signature, so both inserts land on the same reifier.
        link.proof.signature = "deadbeef".to_string();
        svc.add_link(&link).unwrap();

        assert_eq!(
            proof_valid_quads(&svc, &link),
            vec!["false".to_string()],
            "a re-insert must replace the verdict quad, not add a second one"
        );

        let links = svc.get_all_links().unwrap();
        assert_eq!(
            links.len(),
            1,
            "a duplicated verdict quad makes the OPTIONAL match twice and the \
             link come back twice"
        );
        assert_eq!(
            links[0].proof.valid,
            Some(false),
            "the re-inserted verdict must win over the stored one"
        );
    }

    /// Every write carries the annotation, so the two read paths never have to
    /// answer "nobody checked". Pins both, because each decodes independently.
    ///
    /// Turns red on: dropping the `(ONT_PROOF_VALID, &valid_str)` entry from
    /// the `annotations` vec in `insert_link_triples` — the state the old
    /// `if let Some(valid_str) = …` guard produced for a caller-`None` link.
    #[test]
    fn stored_verdict_is_always_present_and_boolean() {
        let svc = new_service();
        let signer = TestSigner::generate();
        let link = signed_link_without_verdict(&signer, "ad4m://always-present");
        svc.add_link(&link).unwrap();

        assert_eq!(
            proof_valid_quads(&svc, &link).len(),
            1,
            "exactly one proofValid quad must be stored for every link"
        );

        for from_get_all in svc.get_all_links().unwrap() {
            assert!(
                from_get_all.proof.valid.is_some(),
                "get_all_links must never report an unevaluated verdict"
            );
        }
        for from_query in svc
            .query_links(Some("ad4m://always-present"), None, None, None, None, None)
            .unwrap()
        {
            assert!(
                from_query.proof.valid.is_some(),
                "query_links must never report an unevaluated verdict"
            );
        }
    }

    /// A negative verdict the caller computed is still recorded as one — the
    /// control that stops "always store true" from passing the tests above.
    ///
    /// Turns red on: hard-coding `"true"` as the `ONT_PROOF_VALID` value.
    #[test]
    fn invalid_signature_is_stored_as_false() {
        let signer = TestSigner::generate();
        let svc = new_service();
        // Corrupt the signature so compute_proof_valid() returns false.
        let mut link = make_link(&signer, "ad4m://neg", "ad4m://pred", "ad4m://tgt");
        link.proof.signature = "deadbeef".to_string();
        svc.add_link(&link).unwrap();

        let from_get_all = svc.get_all_links().unwrap();
        assert_eq!(from_get_all.len(), 1);
        assert_eq!(from_get_all[0].proof.valid, Some(false));

        let from_query = svc
            .query_links(Some("ad4m://neg"), None, None, None, None, None)
            .unwrap();
        assert_eq!(from_query.len(), 1);
        assert_eq!(from_query[0].proof.valid, Some(false));
    }

    /// Data this store did not write — hand-seeded rows, a foreign dump — can
    /// still arrive with no `proofValid` quad. That decodes fail-closed, the
    /// way every consumer of the field reads an unverified link, and never as
    /// an "unevaluated" third state.
    ///
    /// Turns red on: `decode_proof_valid(s) -> bool { s != "false" }`, the
    /// fail-open reading, under which an absent annotation reports verified.
    #[test]
    fn absent_annotation_decodes_fail_closed() {
        assert!(!decode_proof_valid(""), "absent must read as not verified");
        assert!(!decode_proof_valid("false"));
        assert!(decode_proof_valid("true"));
        assert!(
            !decode_proof_valid("TRUE"),
            "only the exact literal this store writes counts as verified"
        );
    }

    /// v4 migration converts every `literal:*:` IRI-shaped object into the
    /// matching typed RDF literal and bumps the version marker.
    #[test]
    fn test_multi_user_sync_wildcard_query() {
        let signer = TestSigner::generate();
        let svc = new_service();

        // Setup link (created by node 1 user 1 during neighbourhood creation)
        let setup = make_link_with_ts(
            &signer,
            "test://setup",
            "test://init",
            "test://neighbourhood",
            "2024-01-15T10:00:00.000Z",
        );

        // 4 user links (simulating the integration test)
        let n1u1 = make_link_with_ts(
            &signer,
            "test://node1user1",
            "test://created",
            "test://link1",
            "2024-01-15T10:00:01.000Z",
        );
        let n1u2 = make_link_with_ts(
            &signer,
            "test://node1user2",
            "test://created",
            "test://link2",
            "2024-01-15T10:00:02.000Z",
        );
        let n2u1 = make_link_with_ts(
            &signer,
            "test://node2user1",
            "test://created",
            "test://link3",
            "2024-01-15T10:00:03.000Z",
        );
        let n2u2 = make_link_with_ts(
            &signer,
            "test://node2user2",
            "test://created",
            "test://link4",
            "2024-01-15T10:00:04.000Z",
        );

        // Node 1 adds its local links
        svc.add_link(&setup).unwrap();
        svc.add_link(&n1u1).unwrap();
        svc.add_link(&n1u2).unwrap();

        // Simulate sync: Node 2's links arrive via diff_from_link_language
        svc.add_link(&n2u1).unwrap();
        svc.add_link(&n2u2).unwrap();

        // Wildcard query (same as the integration test: LinkQuery({}))
        let all = svc.query_links(None, None, None, None, None, None).unwrap();

        assert_eq!(
            all.len(),
            5,
            "Expected 5 links (1 setup + 4 user), got {}. Links: {:?}",
            all.len(),
            all.iter()
                .map(|l| format!("{} -> {} (by {})", l.data.source, l.data.target, l.author))
                .collect::<Vec<_>>()
        );

        // Verify each link is present
        let sources: Vec<&str> = all.iter().map(|l| l.data.source.as_str()).collect();
        assert!(sources.contains(&"test://setup"), "Missing setup link");
        assert!(
            sources.contains(&"test://node1user1"),
            "Missing node1user1 link"
        );
        assert!(
            sources.contains(&"test://node1user2"),
            "Missing node1user2 link"
        );
        assert!(
            sources.contains(&"test://node2user1"),
            "Missing node2user1 link"
        );
        assert!(
            sources.contains(&"test://node2user2"),
            "Missing node2user2 link"
        );
    }

    /// Same scenario but also test get_all_links() (SPARQL-based query path)
    #[test]
    fn test_multi_user_sync_get_all_links() {
        let signer = TestSigner::generate();
        let svc = new_service();

        let links: Vec<LinkExpression> = (0..5)
            .map(|i| {
                make_link_with_ts(
                    &signer,
                    &format!("test://source{}", i),
                    "test://pred",
                    &format!("test://target{}", i),
                    &format!("2024-01-15T10:00:0{}.000Z", i),
                )
            })
            .collect();

        for link in &links {
            svc.add_link(link).unwrap();
        }

        let all = svc.get_all_links().unwrap();
        assert_eq!(
            all.len(),
            5,
            "get_all_links returned {} links, expected 5",
            all.len()
        );

        // Also check query_links returns the same count
        let queried = svc.query_links(None, None, None, None, None, None).unwrap();
        assert_eq!(
            queried.len(),
            5,
            "query_links returned {} links, expected 5",
            queried.len()
        );
    }

    /// Test that re-adding the same link (idempotent insert from sync) doesn't
    /// cause duplicates or data corruption.
    #[test]
    fn test_idempotent_sync_insert() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link_with_ts(
            &signer,
            "test://src",
            "test://pred",
            "test://tgt",
            "2024-01-15T10:00:00.000Z",
        );

        // Add twice (simulates: local add + sync receives same link back)
        svc.add_link(&link).unwrap();
        svc.add_link(&link).unwrap();

        let all = svc.query_links(None, None, None, None, None, None).unwrap();
        assert_eq!(
            all.len(),
            1,
            "Idempotent insert should produce exactly 1 link, got {}",
            all.len()
        );
    }

    // ── Summarisation Pipeline SPARQL Tests ──
    //
    // These tests reproduce the exact SPARQL queries used by the Flux
    // summarisation pipeline against a real Oxigraph store with reifier
    // storage to catch edge cases that mocked tests miss.

    /// Helper: set up a conversation channel with messages in the store.
    /// Returns (channel_id, conversation_id, message_ids).
    fn setup_conversation_channel(
        svc: &SparqlStore,
        channel_id: &str,
        is_conversation: bool,
        message_count: usize,
    ) -> (String, String, Vec<String>) {
        let signer = TestSigner::generate();
        // Channel entry_type flag
        svc.add_link(&make_link(
            &signer,
            channel_id,
            "flux://entry_type",
            "flux://has_channel",
        ))
        .unwrap();

        // isConversation property
        if is_conversation {
            svc.add_link(&make_link(
                &signer,
                channel_id,
                "flux://channel_is_conversation",
                "true",
            ))
            .unwrap();
        }

        // Conversation entity as child of channel
        let conv_id = format!("{}-conv", channel_id);
        svc.add_link(&make_link(
            &signer,
            channel_id,
            "ad4m://has_child",
            &conv_id,
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            &conv_id,
            "flux://entry_type",
            "flux://conversation",
        ))
        .unwrap();

        // Channel creation link (parent -> channel)
        svc.add_link(&make_link(
            &signer,
            "ad4m://self",
            "flux://has_channel",
            channel_id,
        ))
        .unwrap();

        // Messages as children of channel
        let mut msg_ids = Vec::new();
        for i in 0..message_count {
            let msg_id = format!("{}-msg-{}", channel_id, i);
            let link = make_link_with_ts(
                &signer,
                channel_id,
                "ad4m://has_child",
                &msg_id,
                &format!("2026-01-15T10:{:02}:00.000Z", i),
            );
            svc.add_link(&link).unwrap();
            svc.add_link(&make_link(
                &signer,
                &msg_id,
                "flux://entry_type",
                "flux://has_message",
            ))
            .unwrap();
            svc.add_link(&make_link(
                &signer,
                &msg_id,
                "flux://body",
                &format!("literal:string:Message%20{}", i),
            ))
            .unwrap();
            msg_ids.push(msg_id);
        }

        (channel_id.to_string(), conv_id, msg_ids)
    }

    #[test]
    fn test_recent_conversations_sparql_basic() {
        // Tests the exact SPARQL query from Channel.recentConversations()
        // against the real store with reifier storage model.
        let svc = new_service();
        setup_conversation_channel(&svc, "flux://ch1", true, 3);

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?channelId (SAMPLE(?cId) AS ?conversationId) (MAX(?ts) AS ?lastActivity) WHERE {
                ?channelId <flux://entry_type> <flux://has_channel> .
                ?channelId <flux://channel_is_conversation> ?_isConv .
                FILTER(STR(?_isConv) = "true")
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?cId .
                    ?cId <flux://entry_type> <flux://conversation> .
                }
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?item .
                    ?_itemReifier rdf:reifies <<( ?channelId <ad4m://has_child> ?item )>> .
                    ?_itemReifier <ad4m://ontology/timestamp> ?itemTs .
                    ?item <flux://entry_type> ?itemType .
                    FILTER(?itemType IN (<flux://has_message>, <flux://has_post>))
                }
                OPTIONAL {
                    ?_chanReifier rdf:reifies <<( ?_parent <flux://has_channel> ?channelId )>> .
                    ?_chanReifier <ad4m://ontology/timestamp> ?chanCreatedTs .
                }
                BIND(COALESCE(?itemTs, ?chanCreatedTs, "1970-01-01T00:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>) AS ?ts)
            }
            GROUP BY ?channelId
            ORDER BY DESC(?lastActivity)
            LIMIT 20
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            !rows.is_empty(),
            "recentConversations query returned no results. Raw: {}",
            result
        );
        assert_eq!(
            rows[0]["channelId"].as_str().unwrap(),
            "flux://ch1",
            "Expected channel ID flux://ch1, got: {:?}",
            rows[0]
        );
        assert_eq!(
            rows[0]["conversationId"].as_str().unwrap(),
            "flux://ch1-conv",
            "Expected conversation ID, got: {:?}",
            rows[0]
        );
        // lastActivity should be set (from message timestamps)
        let last_activity = rows[0]["lastActivity"].as_str().unwrap_or("");
        assert!(
            !last_activity.is_empty() && last_activity != "1970-01-01T00:00:00Z",
            "Expected valid lastActivity timestamp, got: {}",
            last_activity
        );
    }

    #[test]
    fn test_recent_conversations_non_conversation_channel_excluded() {
        // A channel without isConversation=true should NOT appear
        let svc = new_service();
        setup_conversation_channel(&svc, "flux://ch-space", false, 5);
        setup_conversation_channel(&svc, "flux://ch-conv", true, 2);

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?channelId (SAMPLE(?cId) AS ?conversationId) (MAX(?ts) AS ?lastActivity) WHERE {
                ?channelId <flux://entry_type> <flux://has_channel> .
                ?channelId <flux://channel_is_conversation> ?_isConv .
                FILTER(STR(?_isConv) = "true")
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?cId .
                    ?cId <flux://entry_type> <flux://conversation> .
                }
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?item .
                    ?_itemReifier rdf:reifies <<( ?channelId <ad4m://has_child> ?item )>> .
                    ?_itemReifier <ad4m://ontology/timestamp> ?itemTs .
                    ?item <flux://entry_type> ?itemType .
                    FILTER(?itemType IN (<flux://has_message>, <flux://has_post>))
                }
                OPTIONAL {
                    ?_chanReifier rdf:reifies <<( ?_parent <flux://has_channel> ?channelId )>> .
                    ?_chanReifier <ad4m://ontology/timestamp> ?chanCreatedTs .
                }
                BIND(COALESCE(?itemTs, ?chanCreatedTs, "1970-01-01T00:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>) AS ?ts)
            }
            GROUP BY ?channelId
            ORDER BY DESC(?lastActivity)
            LIMIT 20
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        let channel_ids: Vec<&str> = rows
            .iter()
            .filter_map(|r| r["channelId"].as_str())
            .collect();
        assert!(
            !channel_ids.contains(&"flux://ch-space"),
            "Non-conversation channel should be excluded. Got: {:?}",
            channel_ids
        );
        assert!(
            channel_ids.contains(&"flux://ch-conv"),
            "Conversation channel should be included. Got: {:?}",
            channel_ids
        );
    }

    #[test]
    fn test_recent_conversations_boolean_literal_prefix() {
        let signer = TestSigner::generate();
        // Tests that isConversation stored as "literal:boolean:true" also works
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "flux://ch-lit",
            "flux://entry_type",
            "flux://has_channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch-lit",
            "flux://channel_is_conversation",
            "literal:boolean:true",
        ))
        .unwrap();

        let query = r#"
            SELECT ?channelId WHERE {
                ?channelId <flux://entry_type> <flux://has_channel> .
                ?channelId <flux://channel_is_conversation> ?_isConv .
                FILTER(STR(?_isConv) = "true")
            }
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            !rows.is_empty(),
            "literal:boolean:true should pass parse_literal filter. Raw: {}",
            result
        );
    }

    #[test]
    fn test_recent_conversations_empty_channel_no_messages() {
        // Channel with isConversation=true but no messages should still appear
        // (using channel creation timestamp as fallback)
        let svc = new_service();
        setup_conversation_channel(&svc, "flux://ch-empty", true, 0);

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?channelId (SAMPLE(?cId) AS ?conversationId) (MAX(?ts) AS ?lastActivity) WHERE {
                ?channelId <flux://entry_type> <flux://has_channel> .
                ?channelId <flux://channel_is_conversation> ?_isConv .
                FILTER(STR(?_isConv) = "true")
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?cId .
                    ?cId <flux://entry_type> <flux://conversation> .
                }
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?item .
                    ?_itemReifier rdf:reifies <<( ?channelId <ad4m://has_child> ?item )>> .
                    ?_itemReifier <ad4m://ontology/timestamp> ?itemTs .
                    ?item <flux://entry_type> ?itemType .
                    FILTER(?itemType IN (<flux://has_message>, <flux://has_post>))
                }
                OPTIONAL {
                    ?_chanReifier rdf:reifies <<( ?_parent <flux://has_channel> ?channelId )>> .
                    ?_chanReifier <ad4m://ontology/timestamp> ?chanCreatedTs .
                }
                BIND(COALESCE(?itemTs, ?chanCreatedTs, "1970-01-01T00:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>) AS ?ts)
            }
            GROUP BY ?channelId
            ORDER BY DESC(?lastActivity)
            LIMIT 20
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            !rows.is_empty(),
            "Empty conversation channel should still appear in results. Raw: {}",
            result
        );
    }

    #[test]
    fn test_unprocessed_items_sparql_basic() {
        let signer = TestSigner::generate();
        // Tests the exact SPARQL queries from Channel.unprocessedItems()
        let svc = new_service();
        let (ch_id, _, msg_ids) = setup_conversation_channel(&svc, "flux://ch-unproc", true, 5);

        // Query 1: all items
        let all_items_query = format!(
            r#"SELECT ?id WHERE {{
                <{}> <ad4m://has_child> ?id .
                ?id <flux://entry_type> ?type .
                FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
            }}"#,
            ch_id
        );
        let result = svc.query(&all_items_query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(
            rows.len(),
            5,
            "Expected 5 items, got {}. Raw: {}",
            rows.len(),
            result
        );

        // Mark some as processed (add to subgroup)
        let sg_id = "flux://ch-unproc-sg-1";
        svc.add_link(&make_link(
            &signer,
            sg_id,
            "flux://entry_type",
            "flux://conversation_subgroup",
        ))
        .unwrap();
        svc.add_link(&make_link(&signer, sg_id, "flux://has_item", &msg_ids[0]))
            .unwrap();
        svc.add_link(&make_link(&signer, sg_id, "flux://has_item", &msg_ids[1]))
            .unwrap();

        // Query 2: processed items
        let processed_query = r#"SELECT ?id WHERE {
            ?sg <flux://has_item> ?id .
            ?sg <flux://entry_type> <flux://conversation_subgroup> .
        }"#;
        let result = svc.query(processed_query).unwrap();
        let processed: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(
            processed.len(),
            2,
            "Expected 2 processed items, got {}",
            processed.len()
        );
    }

    #[test]
    fn test_unprocessed_items_data_query_with_reifier() {
        // Tests Query 3 from unprocessedItems — fetching full item data
        // including reifier metadata (author, timestamp)
        let svc = new_service();
        let (ch_id, _, msg_ids) = setup_conversation_channel(&svc, "flux://ch-data", true, 2);

        let values_clause = msg_ids
            .iter()
            .map(|id| format!("<{}>", id))
            .collect::<Vec<_>>()
            .join(" ");

        let data_query = format!(
            r#"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?id ?author ?timestamp ?type ?body WHERE {{
                VALUES ?id {{ {} }}
                <{}> <ad4m://has_child> ?id .
                ?_reifier rdf:reifies <<( <{}> <ad4m://has_child> ?id )>> .
                ?_reifier <ad4m://ontology/author> ?author .
                ?_reifier <ad4m://ontology/timestamp> ?timestamp .
                ?id <flux://entry_type> ?type .
                FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
                OPTIONAL {{ ?id <flux://body> ?body . }}
            }}
            ORDER BY ?timestamp"#,
            values_clause, ch_id, ch_id
        );

        let result = svc.query(&data_query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(
            rows.len(),
            2,
            "Expected 2 data rows, got {}. Raw: {}",
            rows.len(),
            result
        );
        // Verify author and timestamp are populated from reifier
        for row in &rows {
            assert!(
                row["author"].as_str().is_some() && !row["author"].as_str().unwrap().is_empty(),
                "Author should be populated from reifier. Row: {:?}",
                row
            );
            assert!(
                row["timestamp"].as_str().is_some()
                    && !row["timestamp"].as_str().unwrap().is_empty(),
                "Timestamp should be populated from reifier. Row: {:?}",
                row
            );
        }
    }

    #[test]
    fn test_recent_conversations_multiple_channels_ordering() {
        let signer = TestSigner::generate();
        // Two conversation channels with different last-activity times.
        // The one with more recent messages should come first.
        let svc = new_service();

        // Channel 1: older messages
        let ch1 = "flux://ch-old";
        svc.add_link(&make_link(
            &signer,
            ch1,
            "flux://entry_type",
            "flux://has_channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            ch1,
            "flux://channel_is_conversation",
            "true",
        ))
        .unwrap();
        let msg1 = make_link_with_ts(
            &signer,
            ch1,
            "ad4m://has_child",
            "flux://ch-old-msg",
            "2026-01-01T01:00:00.000Z",
        );
        svc.add_link(&msg1).unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch-old-msg",
            "flux://entry_type",
            "flux://has_message",
        ))
        .unwrap();

        // Channel 2: newer messages
        let ch2 = "flux://ch-new";
        svc.add_link(&make_link(
            &signer,
            ch2,
            "flux://entry_type",
            "flux://has_channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            ch2,
            "flux://channel_is_conversation",
            "true",
        ))
        .unwrap();
        let msg2 = make_link_with_ts(
            &signer,
            ch2,
            "ad4m://has_child",
            "flux://ch-new-msg",
            "2026-01-15T10:00:00.000Z",
        );
        svc.add_link(&msg2).unwrap();
        svc.add_link(&make_link(
            &signer,
            "flux://ch-new-msg",
            "flux://entry_type",
            "flux://has_message",
        ))
        .unwrap();

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?channelId (SAMPLE(?cId) AS ?conversationId) (MAX(?ts) AS ?lastActivity) WHERE {
                ?channelId <flux://entry_type> <flux://has_channel> .
                ?channelId <flux://channel_is_conversation> ?_isConv .
                FILTER(STR(?_isConv) = "true")
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?cId .
                    ?cId <flux://entry_type> <flux://conversation> .
                }
                OPTIONAL {
                    ?channelId <ad4m://has_child> ?item .
                    ?_itemReifier rdf:reifies <<( ?channelId <ad4m://has_child> ?item )>> .
                    ?_itemReifier <ad4m://ontology/timestamp> ?itemTs .
                    ?item <flux://entry_type> ?itemType .
                    FILTER(?itemType IN (<flux://has_message>, <flux://has_post>))
                }
                OPTIONAL {
                    ?_chanReifier rdf:reifies <<( ?_parent <flux://has_channel> ?channelId )>> .
                    ?_chanReifier <ad4m://ontology/timestamp> ?chanCreatedTs .
                }
                BIND(COALESCE(?itemTs, ?chanCreatedTs, "1970-01-01T00:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>) AS ?ts)
            }
            GROUP BY ?channelId
            ORDER BY DESC(?lastActivity)
            LIMIT 20
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 2, "Expected 2 channels. Raw: {}", result);
        // Newer channel should come first
        assert_eq!(
            rows[0]["channelId"].as_str().unwrap(),
            "flux://ch-new",
            "Newer channel should be first. Got: {:?}",
            rows
        );
        assert_eq!(
            rows[1]["channelId"].as_str().unwrap(),
            "flux://ch-old",
            "Older channel should be second. Got: {:?}",
            rows
        );
    }

    #[test]
    fn test_conversation_has_child_subscription_direct_triple() {
        let signer = TestSigner::generate();
        // Verify that `<channel> <ad4m://has_child> <msg>` exists as a direct
        // triple (required for SPARQL subscription matching)
        let svc = new_service();
        let link = make_link(&signer, "flux://ch-sub", "ad4m://has_child", "flux://msg-1");
        svc.add_link(&link).unwrap();

        let query = r#"SELECT ?id WHERE {
            <flux://ch-sub> <ad4m://has_child> ?id .
        }"#;
        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0]["id"].as_str().unwrap(), "flux://msg-1");
    }

    #[test]
    fn test_literal_string_target_is_typed_literal_in_store() {
        let signer = TestSigner::generate();
        // `literal:string:X` wire targets land as typed xsd:string literals,
        // so `STR(?raw)` yields the decoded value directly — no custom
        // function needed.
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "flux://test",
            "flux://prop",
            "literal:string:hello",
        ))
        .unwrap();

        let query = r#"SELECT ?val WHERE {
            <flux://test> <flux://prop> ?raw .
            BIND(STR(?raw) AS ?val)
        }"#;
        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 1, "Expected 1 result. Raw: {}", result);
        assert_eq!(
            rows[0]["val"].as_str().unwrap(),
            "hello",
            "typed-literal storage should expose decoded value via STR()"
        );

        // The stored object must be a typed literal, not an IRI — proves the
        // POS index entry is shaped for native xsd comparison.
        use oxigraph::model::Term;
        let any_quad = svc
            .store
            .quads_for_pattern(
                Some(NamedNodeRef::new_unchecked("flux://test").into()),
                Some(NamedNodeRef::new_unchecked("flux://prop")),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .next()
            .and_then(|r| r.ok())
            .expect("triple should exist");
        match any_quad.object {
            Term::Literal(ref l) => {
                assert_eq!(l.value(), "hello");
                assert_eq!(
                    l.datatype().as_str(),
                    "http://www.w3.org/2001/XMLSchema#string"
                );
            }
            other => panic!("Expected typed literal, got: {other:?}"),
        }
    }

    #[test]
    fn test_bare_true_target_stays_as_named_node() {
        let signer = TestSigner::generate();
        // A bare `"true"` target isn't a `literal:*:` wire value, so it stays
        // as a NamedNode <true> in the store. `STR(?val)` returns the IRI
        // text, so the same FILTER pattern still works.
        let svc = new_service();
        svc.add_link(&make_link(&signer, "flux://item", "flux://flag", "true"))
            .unwrap();

        let query = r#"SELECT ?s WHERE {
            ?s <flux://flag> ?val .
            FILTER(STR(?val) = "true")
        }"#;
        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(
            rows.len(),
            1,
            "Bare 'true' target should match STR(?val)=\"true\". Raw: {}",
            result
        );
    }

    #[test]
    fn test_group_by_with_reifier_and_optional() {
        let signer = TestSigner::generate();
        // Isolated test: GROUP BY + OPTIONAL with reifier pattern.
        // This is the specific combination used in recentConversations.
        let svc = new_service();

        // Add two items to one group
        let link1 = make_link_with_ts(
            &signer,
            "flux://group1",
            "flux://has_item",
            "flux://item1",
            "2026-01-15T10:00:00.000Z",
        );
        svc.add_link(&link1).unwrap();

        let link2 = make_link_with_ts(
            &signer,
            "flux://group1",
            "flux://has_item",
            "flux://item2",
            "2026-01-15T11:00:00.000Z",
        );
        svc.add_link(&link2).unwrap();

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?groupId (MAX(?ts) AS ?lastTs) WHERE {
                ?groupId <flux://has_item> ?item .
                OPTIONAL {
                    ?_reifier rdf:reifies <<( ?groupId <flux://has_item> ?item )>> .
                    ?_reifier <ad4m://ontology/timestamp> ?ts .
                }
            }
            GROUP BY ?groupId
        "#;

        let result = svc.query(query).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 1, "Expected 1 group. Raw: {}", result);
        assert_eq!(
            rows[0]["groupId"].as_str().unwrap(),
            "flux://group1",
            "Group ID mismatch"
        );
        // MAX timestamp should be the later one
        let last_ts = rows[0]["lastTs"].as_str().unwrap_or("");
        assert!(
            !last_ts.is_empty(),
            "MAX timestamp should be present. Raw: {}",
            result
        );
    }

    // ── Cancellation tests ──────────────────────────────────────────────

    #[tokio::test]
    async fn query_cancellable_returns_normally_when_not_cancelled() {
        let signer = TestSigner::generate();
        let svc = new_service();
        let link = make_link(&signer, "ad4m://s", "ad4m://p", "ad4m://t");
        svc.add_link(&link).unwrap();

        let cancel = tokio_util::sync::CancellationToken::new();
        let result = svc
            .query_cancellable("SELECT ?s ?p ?o WHERE { ?s ?p ?o }", cancel)
            .await
            .expect("non-cancelled query should succeed");
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(!rows.is_empty(), "expected at least one row");
    }

    #[tokio::test]
    async fn query_cancellable_returns_error_when_cancelled_before_call() {
        let svc = new_service();
        let cancel = tokio_util::sync::CancellationToken::new();
        // Cancel up front — the select! biased arm should fire immediately.
        cancel.cancel();
        let err = svc
            .query_cancellable("SELECT ?s ?p ?o WHERE { ?s ?p ?o }", cancel)
            .await
            .expect_err("pre-cancelled query should error");
        assert!(
            err.to_string().contains("query cancelled"),
            "expected cancellation marker in error, got: {}",
            err
        );
    }

    #[tokio::test]
    async fn query_cancellable_biased_select_prefers_cancel_over_ready_handle() {
        let signer = TestSigner::generate();
        // Honest framing (review nit, PR #855): this does NOT test true
        // mid-flight cancellation — we can't deterministically force the
        // SPARQL eval to be slow, so there's no way to land the cancel
        // while `spawn_blocking` is actually still running without
        // introducing timing-dependent flakiness.
        //
        // What this test does verify: cancelling before the handle is
        // awaited, on a query with real work to do (50 links, not the
        // trivial empty-store case the other pre-cancelled test uses),
        // still takes the `cancel.cancelled()` arm of the `select!` and
        // never touches the query result — i.e. the `biased` ordering
        // holds even when the blocking handle would very plausibly have
        // already resolved by the time we poll (a fast query over 50
        // triples on a local RocksDB-backed store almost certainly has).
        let svc = new_service();
        // Insert a handful of links so the query has actual work to do.
        for i in 0..50 {
            let link = make_link(
                &signer,
                &format!("ad4m://s{}", i),
                "ad4m://p",
                &format!("ad4m://t{}", i),
            );
            svc.add_link(&link).unwrap();
        }

        let cancel = tokio_util::sync::CancellationToken::new();
        let cancel_clone = cancel.clone();
        // Cancel before the await — guarantees the biased cancel arm
        // fires regardless of how fast the blocking handle resolves.
        cancel_clone.cancel();
        let err = svc
            .query_cancellable("SELECT ?s ?p ?o WHERE { ?s ?p ?o }", cancel)
            .await
            .expect_err("cancelled query should error");
        assert!(err.to_string().contains("query cancelled"));
    }

    // ── Reifier IRI stability for literal:json: targets ──

    #[test]
    fn test_remove_link_with_hydrated_json_target_round_trip() {
        let signer = TestSigner::generate();
        // The realistic failure mode: a caller inserts with a hand-written
        // (whitespace-containing) JSON target, later reads it back via a
        // query — receiving the re-serialized, whitespace-compacted wire
        // string `storage_term_to_target_string` produces — and then
        // removes the link using that hydrated string. Before normalizing
        // the reifier-IRI hash input, insert and remove hashed two
        // differently-formatted (but semantically identical) strings to
        // two different reifier IRIs, so removal silently no-opped: the
        // recomputed reifier IRI never matched the one stored at insert
        // time. Note: canonicalization here normalizes whitespace, not
        // object key order (this crate builds serde_json with the
        // `preserve_order` feature via a transitive dependency).
        let svc = new_service();
        let insert_target = "literal:json:{\"a\": 1, \"b\": 2}".to_string();
        // Use a fixed timestamp for both insert and remove so both calls produce
        // the same reifier IRI (reifier hashes author + s/p/o + timestamp).
        let ts = "2024-01-15T10:00:00.000Z";

        let link = make_link_with_ts(
            &signer,
            "ad4m://json_source",
            "ad4m://json_pred",
            &insert_target,
            ts,
        );
        svc.add_link(&link).unwrap();

        let before = svc.get_all_links().unwrap();
        assert_eq!(
            before
                .iter()
                .filter(|l| l.data.source == "ad4m://json_source")
                .count(),
            1,
            "link should be present after insert"
        );

        // Simulate "query, then remove what you read back".
        let hydrated_target =
            storage_term_to_target_string(&target_to_storage_term(&insert_target));
        assert_ne!(
            hydrated_target, insert_target,
            "test setup: hydration must actually reformat the target (whitespace-compacted), \
             otherwise this test isn't exercising the mismatch at all"
        );

        let remove_link = make_link_with_ts(
            &signer,
            "ad4m://json_source",
            "ad4m://json_pred",
            &hydrated_target,
            ts,
        );
        svc.remove_link(&remove_link).unwrap();

        let after = svc.get_all_links().unwrap();
        assert_eq!(
            after
                .iter()
                .filter(|l| l.data.source == "ad4m://json_source")
                .count(),
            0,
            "link should be gone after removing with the hydrated (re-serialized) JSON target"
        );
    }

    // ── query() vs query_arbitrary() target-hydration scoping ──

    #[test]
    fn test_query_arbitrary_does_not_hydrate_coincidentally_named_variables() {
        let signer = TestSigner::generate();
        // A caller-supplied query has no reason to expect a variable it
        // happens to name `?target` or `?t` to be silently re-encoded into
        // AD4M's internal literal:*: wire format.
        let svc = new_service();
        svc.add_link(&make_link(
            &signer,
            "ad4m://msg1",
            "ad4m://ontology/author",
            "did:key:zAlice",
        ))
        .unwrap();
        svc.add_link(&make_link(
            &signer,
            "ad4m://msg1",
            "flux://priority",
            "literal:number:5",
        ))
        .unwrap();

        // query_arbitrary(): ?t bound to a typed-integer literal must come
        // back as its plain lexical value, not `literal:number:5`.
        let arbitrary_result = svc
            .query_arbitrary("SELECT ?t WHERE { <ad4m://msg1> <flux://priority> ?t . }")
            .unwrap();
        let arbitrary_rows: Vec<serde_json::Value> =
            serde_json::from_str(&arbitrary_result).unwrap();
        assert_eq!(
            arbitrary_rows[0]["t"].as_str(),
            Some("5"),
            "query_arbitrary must not wire-encode a coincidentally-named ?t: {}",
            arbitrary_result
        );

        // query(): same shape, but through the internal-hydration path —
        // still wire-encodes ?t, unchanged from before this fix.
        let internal_result = svc
            .query("SELECT ?t WHERE { <ad4m://msg1> <flux://priority> ?t . }")
            .unwrap();
        let internal_rows: Vec<serde_json::Value> = serde_json::from_str(&internal_result).unwrap();
        assert_eq!(
            internal_rows[0]["t"].as_str(),
            Some("literal:number:5"),
            "query() must keep wire-encoding ?t for internal callers: {}",
            internal_result
        );
    }
}

#[cfg(test)]
mod parse_literal_tests {
    use super::parse_literal_fn;
    use oxigraph::model::{Literal, Term};

    fn parsed(value: &str) -> String {
        let term: Term = Literal::new_simple_literal(value).into();
        match parse_literal_fn(&[term]) {
            Some(Term::Literal(l)) => l.value().to_string(),
            other => panic!("expected a literal, got {other:?}"),
        }
    }

    #[test]
    fn json_literal_returns_only_the_data_field_not_author() {
        // A signed message expression: parse_literal must return only `.data`, so
        // a mention query matching on the parsed target never wakes on the author
        // DID sitting beside the content. Regression guard for the self-wake path.
        let v = "literal:json:{\"author\":\"did:key:zAgentDID\",\"timestamp\":\"t\",\"data\":\"weekly harvest report\"}";
        let out = parsed(v);
        assert_eq!(out, "weekly harvest report");
        assert!(
            !out.contains("zAgentDID"),
            "author DID must not appear in the matched text: {out}"
        );
    }

    #[test]
    fn string_literal_is_url_decoded() {
        assert_eq!(parsed("literal:string:hello%20world"), "hello world");
    }
}
