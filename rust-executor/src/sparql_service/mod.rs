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

#[cfg(test)]
mod tests {
    use super::*;

    fn make_link(source: &str, predicate: &str, target: &str) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: "did:key:z6Mktest".to_string(),
            timestamp: "2024-01-15T10:00:00.000Z".to_string(),
            data: Link {
                source: source.to_string(),
                predicate: if predicate.is_empty() { None } else { Some(predicate.to_string()) },
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "testkey".to_string(),
                signature: "testsig".to_string(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: Some(LinkStatus::Shared),
        }
    }

    fn make_link_with_ts(source: &str, predicate: &str, target: &str, ts: &str, author: &str) -> DecoratedLinkExpression {
        let mut link = make_link(source, predicate, target);
        link.timestamp = ts.to_string();
        link.author = author.to_string();
        link
    }

    fn new_service() -> SparqlService {
        SparqlService::new(None).unwrap()
    }

    // ── Storage Model Tests ──

    #[test]
    fn test_add_link_creates_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://source1", "ad4m://predicate1", "ad4m://target1");
        svc.add_link(&link).unwrap();

        let result = svc.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(isIRI(?s) && isIRI(?o)) }").unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        // Should have at least the direct triple
        let direct = rows.iter().find(|r| {
            r["s"].as_str() == Some("ad4m://source1")
                && r["p"].as_str() == Some("ad4m://predicate1")
                && r["o"].as_str() == Some("ad4m://target1")
        });
        assert!(direct.is_some(), "Direct triple not found. Got: {}", result);
    }

    #[test]
    fn test_add_link_creates_rdf_star_annotations() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // Query for annotations on the quoted triple
        let result = svc.query(
            r#"SELECT ?p ?v WHERE {
                << <ad4m://src> <ad4m://pred> <ad4m://tgt> >> ?p ?v .
            }"#
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();

        let preds: Vec<&str> = rows.iter().filter_map(|r| r["p"].as_str()).collect();
        assert!(preds.contains(&"ad4m://ontology/author"), "Missing author annotation");
        assert!(preds.contains(&"ad4m://ontology/timestamp"), "Missing timestamp annotation");
        assert!(preds.contains(&"ad4m://ontology/proofKey"), "Missing proofKey annotation");
        assert!(preds.contains(&"ad4m://ontology/proofSignature"), "Missing proofSig annotation");
        assert!(preds.contains(&"ad4m://ontology/status"), "Missing status annotation");

        let author_row = rows.iter().find(|r| r["p"].as_str() == Some("ad4m://ontology/author")).unwrap();
        assert_eq!(author_row["v"].as_str().unwrap(), "did:key:z6Mktest");
    }

    #[test]
    fn test_remove_link_removes_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        svc.remove_link(&link).unwrap();

        let result = svc.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(isIRI(?s)) }").unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(rows.is_empty(), "Triple still exists after removal: {}", result);
    }

    #[test]
    fn test_remove_link_removes_annotations() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        svc.remove_link(&link).unwrap();

        let result = svc.query(
            r#"SELECT ?p ?v WHERE {
                << <ad4m://src> <ad4m://pred> <ad4m://tgt> >> ?p ?v .
            }"#
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(rows.is_empty(), "Annotation triples still exist: {}", result);
    }

    #[test]
    fn test_iri_roundtrip() {
        let cases = vec![
            "literal://string:foo",
            "flux://has_channel",
            "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK",
            "ad4m://self",
            "http://example.com/resource",
            "https://schema.org/Person",
        ];
        for uri in cases {
            let iri = to_iri(uri);
            let back = from_iri(&iri);
            assert_eq!(back, uri, "Roundtrip failed for '{}': to_iri='{}', from_iri='{}'", uri, iri, back);
        }
    }

    #[test]
    fn test_no_link_node_triples() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // Check no ad4m:Link type triples exist
        let result = svc.query(
            r#"SELECT ?s WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ad4m://Link> . }"#
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(rows.is_empty(), "Found link-node type triples: {}", result);
    }

    // ── Query Tests ──

    #[test]
    fn test_query_links_by_source() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://p", "ad4m://t2")).unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://q", "ad4m://t3")).unwrap();

        let results = svc.query_links(Some("ad4m://a"), None, None, None, None, None).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.source == "ad4m://a"));
    }

    #[test]
    fn test_query_links_by_predicate() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://q", "ad4m://t2")).unwrap();
        svc.add_link(&make_link("ad4m://c", "ad4m://p", "ad4m://t3")).unwrap();

        let results = svc.query_links(None, Some("ad4m://p"), None, None, None, None).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.predicate.as_deref() == Some("ad4m://p")));
    }

    #[test]
    fn test_query_links_by_target() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://q", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://c", "ad4m://r", "ad4m://t2")).unwrap();

        let results = svc.query_links(None, None, Some("ad4m://t1"), None, None, None).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.target == "ad4m://t1"));
    }

    #[test]
    fn test_query_links_by_source_and_predicate() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://q", "ad4m://t2")).unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://p", "ad4m://t3")).unwrap();

        let results = svc.query_links(Some("ad4m://a"), Some("ad4m://p"), None, None, None, None).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t1");
    }

    #[test]
    fn test_query_links_by_source_predicate_target() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1")).unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t2")).unwrap();

        let results = svc.query_links(Some("ad4m://a"), Some("ad4m://p"), Some("ad4m://t1"), None, None, None).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t1");
    }

    #[test]
    fn test_query_links_returns_metadata() {
        let svc = new_service();
        let link = make_link_with_ts("ad4m://s", "ad4m://p", "ad4m://t", "2024-06-01T12:00:00.000Z", "did:key:z6Mkauthor");
        svc.add_link(&link).unwrap();

        let results = svc.query_links(Some("ad4m://s"), None, None, None, None, None).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].author, "did:key:z6Mkauthor");
        assert_eq!(results[0].timestamp, "2024-06-01T12:00:00.000Z");
        assert_eq!(results[0].proof.key, "testkey");
        assert_eq!(results[0].proof.signature, "testsig");
        assert_eq!(results[0].proof.valid, Some(true));
        assert_eq!(results[0].status, Some(LinkStatus::Shared));
    }

    #[test]
    fn test_query_links_date_filter() {
        let svc = new_service();
        svc.add_link(&make_link_with_ts("ad4m://s", "ad4m://p", "ad4m://t1", "2024-01-01T00:00:00Z", "did:key:z6Mk1")).unwrap();
        svc.add_link(&make_link_with_ts("ad4m://s", "ad4m://p", "ad4m://t2", "2024-06-15T00:00:00Z", "did:key:z6Mk2")).unwrap();
        svc.add_link(&make_link_with_ts("ad4m://s", "ad4m://p", "ad4m://t3", "2024-12-31T00:00:00Z", "did:key:z6Mk3")).unwrap();

        // fromDate filter
        let results = svc.query_links(None, None, None, Some("2024-06-01T00:00:00Z"), None, None).unwrap();
        assert_eq!(results.len(), 2);

        // untilDate filter
        let results = svc.query_links(None, None, None, None, Some("2024-06-30T00:00:00Z"), None).unwrap();
        assert_eq!(results.len(), 2);

        // both
        let results = svc.query_links(None, None, None, Some("2024-06-01T00:00:00Z"), Some("2024-06-30T00:00:00Z"), None).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t2");
    }

    #[test]
    fn test_query_links_limit() {
        let svc = new_service();
        for i in 0..10 {
            svc.add_link(&make_link_with_ts(
                "ad4m://s", "ad4m://p", &format!("ad4m://t{}", i),
                &format!("2024-01-{:02}T00:00:00Z", i + 1), "did:key:z6Mk1"
            )).unwrap();
        }

        let results = svc.query_links(None, None, None, None, None, Some(3)).unwrap();
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn test_sparql_query_direct_triple_pattern() {
        let svc = new_service();
        svc.add_link(&make_link("flux://community1", "flux://has_channel", "flux://channel1")).unwrap();
        svc.add_link(&make_link("flux://community1", "flux://has_channel", "flux://channel2")).unwrap();

        let result = svc.query(
            r#"SELECT ?channel WHERE {
                <flux://community1> <flux://has_channel> ?channel .
            }"#
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 2);
        let channels: Vec<&str> = rows.iter().filter_map(|r| r["channel"].as_str()).collect();
        assert!(channels.contains(&"flux://channel1"));
        assert!(channels.contains(&"flux://channel2"));
    }

    #[test]
    fn test_sparql_query_with_join() {
        let svc = new_service();
        svc.add_link(&make_link("flux://ch1", "flux://entry_type", "flux://channel")).unwrap();
        svc.add_link(&make_link("flux://ch1", "flux://name", "literal://string:general")).unwrap();
        svc.add_link(&make_link("flux://ch2", "flux://entry_type", "flux://channel")).unwrap();
        svc.add_link(&make_link("flux://ch2", "flux://name", "literal://string:random")).unwrap();
        svc.add_link(&make_link("flux://msg1", "flux://entry_type", "flux://message")).unwrap();

        let result = svc.query(
            r#"SELECT ?ch ?name WHERE {
                ?ch <flux://entry_type> <flux://channel> .
                ?ch <flux://name> ?name .
            }"#
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(rows.len(), 2);
    }

    // ── Sync / reload tests ──

    #[test]
    fn test_sync_existing_links_to_sparql() {
        let svc = new_service();
        let links = vec![
            make_link("ad4m://a", "ad4m://p1", "ad4m://t1"),
            make_link("ad4m://b", "ad4m://p2", "ad4m://t2"),
            make_link("ad4m://c", "ad4m://p3", "ad4m://t3"),
        ];
        svc.reload(links).unwrap();

        let all = svc.get_all_links().unwrap();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn test_link_add_then_query_roundtrip() {
        let svc = new_service();
        let link = make_link("literal://string:hello", "flux://has_channel", "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK");
        svc.add_link(&link).unwrap();

        let results = svc.query_links(Some("literal://string:hello"), Some("flux://has_channel"), None, None, None, None).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.source, "literal://string:hello");
        assert_eq!(results[0].data.predicate.as_deref(), Some("flux://has_channel"));
        assert_eq!(results[0].data.target, "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK");
        assert_eq!(results[0].author, "did:key:z6Mktest");
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
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t")).unwrap();
        svc.clear().unwrap();
        let all = svc.get_all_links().unwrap();
        assert!(all.is_empty());
    }
}
