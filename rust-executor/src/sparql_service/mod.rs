use crate::graphql::graphql_types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use deno_core::anyhow::{anyhow, Error};
use lazy_static::lazy_static;
use oxigraph::model::*;
use oxigraph::sparql::{QueryOptions, QueryResults};
use oxigraph::store::Store;
use serde_json::Value;
use std::sync::Arc;

const ONT_AUTHOR: &str = "ad4m://ontology/author";
const ONT_TIMESTAMP: &str = "ad4m://ontology/timestamp";
const ONT_PROOF_KEY: &str = "ad4m://ontology/proofKey";
const ONT_PROOF_SIG: &str = "ad4m://ontology/proofSignature";
const ONT_PROOF_VALID: &str = "ad4m://ontology/proofValid";
const ONT_STATUS: &str = "ad4m://ontology/status";

fn literal(val: &str) -> Literal {
    Literal::new_simple_literal(val)
}

fn status_str(status: &Option<LinkStatus>) -> &'static str {
    match status {
        Some(LinkStatus::Shared) => "Shared",
        Some(LinkStatus::Local) => "Local",
        None => "Shared",
    }
}

fn parse_literal_fn(args: &[Term]) -> Option<Term> {
    if args.len() != 1 {
        return None;
    }
    let val = match &args[0] {
        Term::Literal(l) => l.value().to_string(),
        _ => return Some(args[0].clone()),
    };
    if !val.starts_with("literal://") {
        return Some(args[0].clone());
    }
    let body = &val[10..];
    if let Some(rest) = body.strip_prefix("string:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        Some(Literal::new_simple_literal(decoded.as_ref()).into())
    } else if let Some(rest) = body.strip_prefix("number:") {
        Some(Literal::new_simple_literal(rest).into())
    } else if let Some(rest) = body.strip_prefix("boolean:") {
        Some(Literal::new_simple_literal(rest).into())
    } else if let Some(rest) = body.strip_prefix("json:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        Some(Literal::new_simple_literal(decoded.as_ref()).into())
    } else {
        Some(args[0].clone())
    }
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

/// Transform AD4M-style URIs in angle brackets within a SPARQL query string.
/// Converts `<scheme://path>` to `<scheme:path>` for non-standard schemes,
/// making them valid IRIs that the SPARQL parser will accept.
/// Standard schemes (http, https, ftp, ws, wss) are left unchanged.
fn transform_sparql_iris(query: &str) -> String {
    let mut result = String::with_capacity(query.len());
    let bytes = query.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'<' {
            // Find the closing >
            if let Some(end) = query[i + 1..].find('>') {
                let iri_content = &query[i + 1..i + 1 + end];
                // Only transform if it contains :// and is not a standard scheme
                let transformed = to_iri(iri_content);
                result.push('<');
                result.push_str(&transformed);
                result.push('>');
                i = i + 1 + end + 1;
            } else {
                result.push(bytes[i] as char);
                i += 1;
            }
        } else if bytes[i] == b'"' {
            // Skip string literals
            result.push('"');
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' && i + 1 < bytes.len() {
                    result.push(bytes[i] as char);
                    result.push(bytes[i + 1] as char);
                    i += 2;
                } else if bytes[i] == b'"' {
                    result.push('"');
                    i += 1;
                    break;
                } else {
                    result.push(bytes[i] as char);
                    i += 1;
                }
            }
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

/// Validates that a SPARQL query is read-only (no INSERT/DELETE/DROP/CLEAR/CREATE/LOAD)
pub fn validate_readonly_query(query: &str) -> Result<(), Error> {
    let upper = query.to_uppercase();
    let mutating = ["INSERT", "DELETE", "DROP", "CLEAR", "CREATE", "LOAD"];
    for op in &mutating {
        let mut pos = 0;
        while let Some(idx) = upper[pos..].find(op) {
            let abs = pos + idx;
            let before_ok = abs == 0 || {
                let c = upper.as_bytes()[abs - 1];
                matches!(c, b' ' | b'\t' | b'\n' | b'\r' | b';' | b'(' | b'{')
            };
            let after_pos = abs + op.len();
            let after_ok = after_pos >= upper.len() || {
                let c = upper.as_bytes()[after_pos];
                matches!(c, b' ' | b'\t' | b'\n' | b'\r' | b';' | b'(' | b'{' | b'}')
            };
            if before_ok && after_ok {
                return Err(anyhow!(
                    "Query contains mutating operation '{}'. Only read-only SPARQL is permitted.",
                    op
                ));
            }
            pos = abs + 1;
        }
    }
    Ok(())
}

/// Transform an AD4M URI into a valid RDF IRI.
/// AD4M uses `scheme://path` format (e.g., `literal://string:hello`)
/// which can fail strict IRI validation when the authority part contains
/// invalid characters. We transform to opaque URI format: `scheme:path`
/// (e.g., `literal:string:hello`) which is always valid.
///
/// Standard schemes (http, https, ftp, ws, wss) are left unchanged.
fn to_iri(ad4m_uri: &str) -> String {
    // Don't transform standard web schemes
    if ad4m_uri.starts_with("http://") || ad4m_uri.starts_with("https://")
        || ad4m_uri.starts_with("ftp://") || ad4m_uri.starts_with("ws://")
        || ad4m_uri.starts_with("wss://") {
        return ad4m_uri.to_string();
    }
    // Transform scheme://path → scheme:path
    if let Some(pos) = ad4m_uri.find("://") {
        let scheme = &ad4m_uri[..pos];
        let rest = &ad4m_uri[pos + 3..];
        format!("{}:{}", scheme, rest)
    } else {
        ad4m_uri.to_string()
    }
}

/// Reverse of to_iri: transform opaque URI back to AD4M format.
/// `literal:string:hello` → `literal://string:hello`
///
/// Standard schemes are left unchanged.
fn from_iri(iri: &str) -> String {
    // Standard web schemes already have ://
    if iri.starts_with("http://") || iri.starts_with("https://")
        || iri.starts_with("ftp://") || iri.starts_with("ws://")
        || iri.starts_with("wss://") {
        return iri.to_string();
    }
    // Find scheme:path and transform to scheme://path
    if let Some(pos) = iri.find(':') {
        let scheme = &iri[..pos];
        let rest = &iri[pos + 1..];
        // Only transform if it doesn't already have ://
        if !rest.starts_with("//") {
            format!("{}://{}", scheme, rest)
        } else {
            iri.to_string()
        }
    } else {
        iri.to_string()
    }
}

/// Build the direct triple (source, predicate, target as IRIs) for a link.
/// Transforms AD4M URIs to valid RDF IRIs using opaque URI format.
fn make_direct_triple(link: &DecoratedLinkExpression) -> (NamedNode, NamedNode, NamedNode) {
    let source_iri = NamedNode::new_unchecked(to_iri(&link.data.source));
    let predicate_val = link.data.predicate.as_deref().unwrap_or("");
    let predicate_iri = NamedNode::new_unchecked(to_iri(predicate_val));
    let target_iri = NamedNode::new_unchecked(to_iri(&link.data.target));
    (source_iri, predicate_iri, target_iri)
}

/// Oxigraph-backed SPARQL store for AD4M link data.
/// Uses direct triples + RDF-star annotations for metadata.
/// Synchronous API — Oxigraph operations are not async.
#[derive(Clone)]
pub struct SparqlService {
    store: Arc<Store>,
}

impl SparqlService {
    /// Create a new SparqlService with an in-memory store.
    pub fn new(_data_path: Option<&str>) -> Result<Self, Error> {
        let store = Store::new()?;
        Ok(SparqlService {
            store: Arc::new(store),
        })
    }

    fn insert_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link);

        // 1. Insert the direct triple: <source> <predicate> <target>
        self.store.insert(QuadRef::new(
            source_iri.as_ref(),
            predicate_iri.as_ref(),
            TermRef::NamedNode(target_iri.as_ref()),
            GraphNameRef::DefaultGraph,
        ))?;

        // 2. Insert RDF-star annotations on the quoted triple
        let quoted = Triple::new(
            source_iri.clone(),
            predicate_iri.clone(),
            target_iri.clone(),
        );
        let quoted_subject: Subject = quoted.into();

        let proof = &link.proof;
        let valid_str = proof.valid.unwrap_or(false).to_string();

        let annotations: &[(&str, &str)] = &[
            (ONT_AUTHOR, &link.author),
            (ONT_TIMESTAMP, &link.timestamp),
            (ONT_PROOF_KEY, &proof.key),
            (ONT_PROOF_SIG, &proof.signature),
            (ONT_PROOF_VALID, &valid_str),
            (ONT_STATUS, status_str(&link.status)),
        ];

        for (pred_uri, value) in annotations {
            let pred = NamedNodeRef::new_unchecked(pred_uri);
            let lit = literal(value);
            let subj_ref: SubjectRef = quoted_subject.as_ref();
            self.store.insert(QuadRef::new(
                subj_ref,
                pred,
                TermRef::Literal(lit.as_ref()),
                GraphNameRef::DefaultGraph,
            ))?;
        }

        Ok(())
    }

    /// Insert triples for a link into the store.
    pub fn add_link(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        self.insert_link_triples(link)
    }

    /// Remove all triples for a link from the store.
    pub fn remove_link(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link);

        // 1. Remove the direct triple
        self.store.remove(QuadRef::new(
            source_iri.as_ref(),
            predicate_iri.as_ref(),
            TermRef::NamedNode(target_iri.as_ref()),
            GraphNameRef::DefaultGraph,
        ))?;

        // 2. Remove RDF-star annotation triples
        let quoted = Triple::new(
            source_iri.clone(),
            predicate_iri.clone(),
            target_iri.clone(),
        );
        let quoted_subject: Subject = quoted.into();

        let annotation_quads: Vec<_> = self
            .store
            .quads_for_pattern(Some(quoted_subject.as_ref().into()), None, None, None)
            .collect::<Result<Vec<_>, _>>()?;
        for quad in annotation_quads {
            self.store.remove(&quad)?;
        }
        Ok(())
    }

    /// Return all links in the store by querying via SPARQL-star.
    pub fn get_all_links(&self) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let query = r#"
            SELECT ?source ?predicate ?target ?author ?timestamp ?proofKey ?proofSig ?proofValid ?status WHERE {
                ?source ?predicate ?target .
                FILTER(isIRI(?source))
                BIND(<< ?source ?predicate ?target >> AS ?ann)
                ?ann <ad4m://ontology/author> ?author .
                ?ann <ad4m://ontology/timestamp> ?timestamp .
                OPTIONAL { ?ann <ad4m://ontology/proofKey> ?proofKey . }
                OPTIONAL { ?ann <ad4m://ontology/proofSignature> ?proofSig . }
                OPTIONAL { ?ann <ad4m://ontology/proofValid> ?proofValid . }
                OPTIONAL { ?ann <ad4m://ontology/status> ?status . }
            }
        "#;

        let options = self.query_options();
        let results = self.store.query_opt(query, options)?;

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

    /// Query links matching optional filters using direct store pattern matching.
    /// This is faster than SPARQL parsing and avoids IRI validation issues.
    pub fn query_links(
        &self,
        source: Option<&str>,
        predicate: Option<&str>,
        target: Option<&str>,
        from_date: Option<&str>,
        until_date: Option<&str>,
        limit: Option<usize>,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let source_node = source.map(|s| NamedNode::new_unchecked(to_iri(s)));
        let predicate_node = predicate.map(|p| NamedNode::new_unchecked(to_iri(p)));
        let target_node = target.map(|t| NamedNode::new_unchecked(to_iri(t)));

        let s_ref = source_node.as_ref().map(|n| n.as_ref().into());
        let p_ref = predicate_node.as_ref().map(|n| n.as_ref());
        let t_ref: Option<TermRef> = target_node.as_ref().map(|n| n.as_ref().into());

        let mut links = Vec::new();
        let ont_author = NamedNodeRef::new_unchecked(ONT_AUTHOR);
        let ont_timestamp = NamedNodeRef::new_unchecked(ONT_TIMESTAMP);
        let ont_proof_key = NamedNodeRef::new_unchecked(ONT_PROOF_KEY);
        let ont_proof_sig = NamedNodeRef::new_unchecked(ONT_PROOF_SIG);
        let ont_proof_valid = NamedNodeRef::new_unchecked(ONT_PROOF_VALID);
        let ont_status = NamedNodeRef::new_unchecked(ONT_STATUS);

        for quad_result in self.store.quads_for_pattern(s_ref, p_ref, t_ref, None) {
            let quad = quad_result?;

            // Skip non-IRI subjects (RDF-star annotation triples have quoted triple subjects)
            let (src_raw, src) = match &quad.subject {
                Subject::NamedNode(n) => (n.as_str().to_string(), from_iri(n.as_str())),
                _ => continue,
            };
            let (pred_raw, pred) = (quad.predicate.as_str().to_string(), from_iri(quad.predicate.as_str()));
            let (tgt_raw, tgt) = match &quad.object {
                Term::NamedNode(n) => (n.as_str().to_string(), from_iri(n.as_str())),
                _ => continue,
            };

            // Skip annotation predicates
            if pred.starts_with("ad4m://ontology/") {
                continue;
            }

            // Get RDF-star annotations using raw (stored) IRIs
            let quoted = Triple::new(
                NamedNode::new_unchecked(&src_raw),
                NamedNode::new_unchecked(&pred_raw),
                NamedNode::new_unchecked(&tgt_raw),
            );
            let quoted_subject: Subject = quoted.into();

            let get_annotation = |pred_node: NamedNodeRef| -> String {
                self.store
                    .quads_for_pattern(Some(quoted_subject.as_ref().into()), Some(pred_node), None, None)
                    .next()
                    .and_then(|r| r.ok())
                    .and_then(|q| match &q.object {
                        Term::Literal(l) => Some(l.value().to_string()),
                        _ => None,
                    })
                    .unwrap_or_default()
            };

            let author = get_annotation(ont_author);
            let timestamp = get_annotation(ont_timestamp);

            // Skip links without required metadata (annotations not yet synced)
            if author.is_empty() || timestamp.is_empty() {
                continue;
            }

            // Apply date filters
            if let Some(from) = from_date {
                if timestamp.as_str() < from {
                    continue;
                }
            }
            if let Some(until) = until_date {
                if timestamp.as_str() > until {
                    continue;
                }
            }

            let proof_key = get_annotation(ont_proof_key);
            let proof_sig = get_annotation(ont_proof_sig);
            let proof_valid_str = get_annotation(ont_proof_valid);
            let proof_valid = if proof_valid_str.is_empty() { None } else { Some(proof_valid_str == "true") };
            let status_val = get_annotation(ont_status);
            let status = match status_val.as_str() {
                "Local" => Some(LinkStatus::Local),
                "Shared" => Some(LinkStatus::Shared),
                _ => None,
            };

            links.push(DecoratedLinkExpression {
                author,
                timestamp,
                data: Link {
                    source: src,
                    predicate: if pred.is_empty() { None } else { Some(pred) },
                    target: tgt,
                },
                proof: DecoratedExpressionProof {
                    key: proof_key,
                    signature: proof_sig,
                    valid: proof_valid,
                    invalid: proof_valid.map(|v| !v),
                },
                status,
            });

            if let Some(lim) = limit {
                if links.len() >= lim {
                    break;
                }
            }
        }

        Ok(links)
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

    fn query_options(&self) -> QueryOptions {
        QueryOptions::default()
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/parse_literal"),
                parse_literal_fn,
            )
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/strip_html"),
                strip_html_fn,
            )
    }

    fn link_from_solution(&self, solution: &oxigraph::sparql::QuerySolution) -> Option<DecoratedLinkExpression> {
        let source = match solution.get("source")? {
            Term::NamedNode(n) => from_iri(n.as_str()),
            _ => return None,
        };
        let predicate = match solution.get("predicate")? {
            Term::NamedNode(n) => {
                let s = from_iri(n.as_str());
                if s.is_empty() { None } else { Some(s) }
            },
            _ => return None,
        };
        let target = match solution.get("target")? {
            Term::NamedNode(n) => from_iri(n.as_str()),
            _ => return None,
        };

        let get_str = |var: &str| -> String {
            solution.get(var).and_then(|t| match t {
                Term::Literal(l) => Some(l.value().to_string()),
                Term::NamedNode(n) => Some(n.as_str().to_string()),
                _ => None,
            }).unwrap_or_default()
        };

        let author = get_str("author");
        let timestamp = get_str("timestamp");
        let proof_key = get_str("proofKey");
        let proof_sig = get_str("proofSig");
        let proof_valid_str = get_str("proofValid");
        let proof_valid = if proof_valid_str.is_empty() { None } else { Some(proof_valid_str == "true") };
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
                valid: proof_valid,
                invalid: proof_valid.map(|v| !v),
            },
            status,
        })
    }

    /// Execute an arbitrary read-only SPARQL SELECT query, returning a JSON string.
    /// AD4M URIs in angle brackets are automatically transformed to valid RDF IRIs
    /// (e.g., `<literal://string:foo>` → `<literal:string:foo>`).
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
        let transformed = transform_sparql_iris(query_string);
        validate_readonly_query(&transformed)?;

        let options = self.query_options();
        let results = self.store.query_opt(&transformed, options)?;

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
                            let val = match term {
                                Term::NamedNode(n) => Value::String(from_iri(n.as_str())),
                                Term::Literal(l) => Value::String(l.value().to_string()),
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

    /// Remove all triples from the store.
    pub fn clear(&self) -> Result<(), Error> {
        self.store.clear()?;
        Ok(())
    }

    /// Clear the store and bulk-insert all provided links.
    pub fn reload(&self, links: Vec<DecoratedLinkExpression>) -> Result<(), Error> {
        self.clear()?;
        for link in &links {
            self.insert_link_triples(link)?;
        }
        Ok(())
    }
}

lazy_static! {
    static ref SPARQL_SERVICE: Arc<std::sync::RwLock<Option<SparqlService>>> =
        Arc::new(std::sync::RwLock::new(None));
}

pub fn init_sparql_service(data_path: Option<&str>) -> Result<(), Error> {
    let service = SparqlService::new(data_path)?;
    let mut lock = SPARQL_SERVICE.write().unwrap();
    *lock = Some(service);
    Ok(())
}

pub fn get_sparql_service() -> SparqlService {
    let lock = SPARQL_SERVICE.read().unwrap();
    lock.clone()
        .expect("SparqlService not initialized. Call init_sparql_service() first.")
}
