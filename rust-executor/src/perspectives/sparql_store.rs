use crate::graphql::graphql_types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use deno_core::anyhow::{anyhow, Error};
use oxigraph::model::*;
use oxigraph::sparql::{Query, QueryOptions, QueryResults};
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
    // Extract the string value from either a Literal or a NamedNode.
    // AD4M stores link targets as NamedNodes (via NamedNode::new_unchecked),
    // so we need to handle both cases for `fn::parse_literal` to be useful
    // in SPARQL queries that operate on link targets.
    let val = match &args[0] {
        Term::Literal(l) => l.value().to_string(),
        Term::NamedNode(n) => n.as_str().to_string(),
        _ => return Some(args[0].clone()),
    };
    let body = if val.starts_with("literal:") {
        &val[8..]
    } else {
        return Some(args[0].clone());
    };
    if let Some(rest) = body.strip_prefix("string:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        Some(Literal::new_simple_literal(decoded.as_ref()).into())
    } else if let Some(rest) = body.strip_prefix("number:") {
        Some(Literal::new_simple_literal(rest).into())
    } else if let Some(rest) = body.strip_prefix("boolean:") {
        Some(Literal::new_simple_literal(rest).into())
    } else if let Some(rest) = body.strip_prefix("json:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        // For JSON literals that are signed expressions (contain "data" field),
        // extract just the data field value for content matching.
        if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(&decoded) {
            if let Some(data) = json_val.get("data") {
                let data_str = match data {
                    serde_json::Value::String(s) => s.clone(),
                    _ => serde_json::to_string(data).unwrap_or(decoded.into_owned()),
                };
                return Some(Literal::new_simple_literal(&data_str).into());
            }
        }
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

/// Validates that a SPARQL query is read-only by parsing it with Oxigraph's SPARQL parser.
/// Only SELECT, ASK, CONSTRUCT, and DESCRIBE queries are accepted.
/// UPDATE operations (INSERT, DELETE, DROP, etc.) will fail to parse as a Query.
pub fn validate_readonly_query(query: &str) -> Result<(), Error> {
    Query::parse(query, None).map_err(|e| {
        anyhow!(
            "Query is not valid read-only SPARQL (only SELECT/ASK/CONSTRUCT/DESCRIBE allowed): {}",
            e
        )
    })?;
    Ok(())
}

/// Generate a deterministic graph IRI from link data + timestamp.
fn make_graph_iri(link: &DecoratedLinkExpression) -> NamedNode {
    let mut hasher = Sha256::new();
    hasher.update(link.data.source.as_bytes());
    hasher.update(link.data.predicate.as_deref().unwrap_or("").as_bytes());
    hasher.update(link.data.target.as_bytes());
    hasher.update(link.timestamp.as_bytes());
    let hash = hex::encode(hasher.finalize());
    NamedNode::new_unchecked(format!("link:{}", &hash[..32]))
}

/// Build the direct triple (source, predicate, target as IRIs) for a link.
fn make_direct_triple(link: &DecoratedLinkExpression) -> (NamedNode, NamedNode, NamedNode) {
    let source_iri = NamedNode::new_unchecked(&link.data.source);
    let predicate_val = link.data.predicate.as_deref().unwrap_or("");
    let predicate_iri = NamedNode::new_unchecked(predicate_val);
    let target_iri = NamedNode::new_unchecked(&link.data.target);
    (source_iri, predicate_iri, target_iri)
}

/// Oxigraph-backed SPARQL store for AD4M link data.
/// Uses named graphs for link triples + default graph metadata keyed by graph IRI.
/// Synchronous API — Oxigraph operations are not async.
///
/// # Thread Safety
/// Oxigraph's `Store` is `Send + Sync` and uses internal locking for concurrent access.
/// Multiple threads can safely read and write simultaneously without external synchronization.
/// See: oxigraph 0.4 source — `Store` derives `Clone` and wraps an internally-locked storage layer.
/// The oxigraph test suite includes a `test_send_sync` test verifying `Store: Send + Sync`.
#[derive(Clone)]
pub struct SparqlStore {
    store: Arc<Store>,
}

impl SparqlStore {
    /// Create a new SparqlStore.
    ///
    /// If `data_path` is `Some`, opens a persistent RocksDB-backed store at that path.
    /// If `data_path` is `None`, creates an in-memory store (useful for tests).
    ///
    /// Create a new SparqlStore. If `data_path` is provided, uses Oxigraph's persistent
    /// RocksDB-backed store at `{data_path}/sparql_store/`. If `None`, creates an in-memory store.
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

    /// Returns true if the store contains any quads (non-empty).
    pub fn has_data(&self) -> bool {
        self.store
            .quads_for_pattern(None, None, None, None)
            .next()
            .is_some()
    }

    fn insert_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link);
        let graph = make_graph_iri(link);

        // 1. Insert the direct triple in the link's named graph
        self.store.insert(QuadRef::new(
            source_iri.as_ref(),
            predicate_iri.as_ref(),
            TermRef::NamedNode(target_iri.as_ref()),
            GraphNameRef::NamedNode(graph.as_ref()),
        ))?;

        // 2. Insert metadata in default graph, keyed by graph IRI
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
            self.store.insert(QuadRef::new(
                graph.as_ref(),
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
        let graph = make_graph_iri(link);

        // 1. Remove all quads in the named graph
        let graph_quads: Vec<_> = self
            .store
            .quads_for_pattern(
                None,
                None,
                None,
                Some(GraphNameRef::NamedNode(graph.as_ref())),
            )
            .collect::<Result<Vec<_>, _>>()?;
        for quad in graph_quads {
            self.store.remove(&quad)?;
        }

        // 2. Remove all metadata triples in default graph with graph IRI as subject
        let meta_quads: Vec<_> = self
            .store
            .quads_for_pattern(
                Some(graph.as_ref().into()),
                None,
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()?;
        for quad in meta_quads {
            self.store.remove(&quad)?;
        }
        Ok(())
    }

    /// Return all links in the store by querying via named graphs.
    pub fn get_all_links(&self) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let query = r#"
            SELECT ?source ?predicate ?target ?author ?timestamp ?proofKey ?proofSig ?proofValid ?status WHERE {
                GRAPH ?g { ?source ?predicate ?target . }
                FILTER(isIRI(?source))
                ?g <ad4m://ontology/author> ?author .
                ?g <ad4m://ontology/timestamp> ?timestamp .
                OPTIONAL { ?g <ad4m://ontology/proofKey> ?proofKey . }
                OPTIONAL { ?g <ad4m://ontology/proofSignature> ?proofSig . }
                OPTIONAL { ?g <ad4m://ontology/proofValid> ?proofValid . }
                OPTIONAL { ?g <ad4m://ontology/status> ?status . }
            }
        "#;

        let options = self.query_options();
        let mut parsed_query = Query::parse(query, None)
            .map_err(|e| anyhow!("Failed to parse get_all_links query: {}", e))?;
        parsed_query.dataset_mut().set_default_graph_as_union();
        let results = self.store.query_opt(parsed_query, options)?;

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
        let source_node = source.map(|s| NamedNode::new_unchecked(s));
        let predicate_node = predicate.map(|p| NamedNode::new_unchecked(p));
        let target_node = target.map(|t| NamedNode::new_unchecked(t));

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

        // Iterate over all named graphs to find matching triples
        for graph_name in self.store.named_graphs() {
            let graph_name = graph_name?;
            let graph_ref = match &graph_name {
                NamedOrBlankNode::NamedNode(n) => GraphNameRef::NamedNode(n.as_ref()),
                NamedOrBlankNode::BlankNode(b) => GraphNameRef::BlankNode(b.as_ref()),
            };

            // Each named graph contains exactly one direct triple
            for quad_result in self
                .store
                .quads_for_pattern(s_ref, p_ref, t_ref, Some(graph_ref))
            {
                let quad = quad_result?;

                let src = match &quad.subject {
                    Subject::NamedNode(n) => n.as_str().to_string(),
                    _ => continue,
                };
                let pred = quad.predicate.as_str().to_string();
                let tgt = match &quad.object {
                    Term::NamedNode(n) => n.as_str().to_string(),
                    _ => continue,
                };

                // Skip annotation predicates (shouldn't be in named graphs, but safety check)
                if pred.starts_with("ad4m://ontology/") {
                    continue;
                }

                // Get metadata from default graph using graph IRI as subject
                let graph_subject: SubjectRef = match &graph_name {
                    NamedOrBlankNode::NamedNode(n) => n.as_ref().into(),
                    NamedOrBlankNode::BlankNode(b) => b.as_ref().into(),
                };

                let get_annotation = |pred_node: NamedNodeRef| -> String {
                    self.store
                        .quads_for_pattern(
                            Some(graph_subject),
                            Some(pred_node),
                            None,
                            Some(GraphNameRef::DefaultGraph),
                        )
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

                // Skip links without required metadata
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
                let proof_valid = if proof_valid_str.is_empty() {
                    None
                } else {
                    Some(proof_valid_str == "true")
                };
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
                        return Ok(links);
                    }
                }
            }
        }

        Ok(links)
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
        let target = match solution.get("target")? {
            Term::NamedNode(n) => n.as_str().to_string(),
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
        let proof_valid = if proof_valid_str.is_empty() {
            None
        } else {
            Some(proof_valid_str == "true")
        };
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
    /// All AD4M URIs are valid IRIs, so queries are passed through as-is.
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
        validate_readonly_query(query_string)?;

        let mut parsed_query = Query::parse(query_string, None)
            .map_err(|e| anyhow!("Failed to parse SPARQL query: {}", e))?;
        // Include all named graphs in the default dataset so unscoped
        // triple patterns find triples stored in named graphs.
        parsed_query.dataset_mut().set_default_graph_as_union();

        let options = self.query_options();
        let results = self.store.query_opt(parsed_query, options).map_err(|e| {
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
                            let val = match term {
                                Term::NamedNode(n) => Value::String(n.as_str().to_string()),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn make_link(source: &str, predicate: &str, target: &str) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: "did:key:z6Mktest".to_string(),
            timestamp: "2024-01-15T10:00:00.000Z".to_string(),
            data: Link {
                source: source.to_string(),
                predicate: if predicate.is_empty() {
                    None
                } else {
                    Some(predicate.to_string())
                },
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

    fn make_link_with_ts(
        source: &str,
        predicate: &str,
        target: &str,
        ts: &str,
        author: &str,
    ) -> DecoratedLinkExpression {
        let mut link = make_link(source, predicate, target);
        link.timestamp = ts.to_string();
        link.author = author.to_string();
        link
    }

    fn new_service() -> SparqlStore {
        SparqlStore::new(None).unwrap()
    }

    // ── Storage Model Tests ──

    #[test]
    fn test_add_link_creates_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://source1", "ad4m://predicate1", "ad4m://target1");
        svc.add_link(&link).unwrap();

        // Direct triple should be findable via GRAPH pattern or unscoped (with union default graph)
        let result = svc
            .query(
                "SELECT ?s ?p ?o WHERE { GRAPH ?g { ?s ?p ?o } . FILTER(isIRI(?s) && isIRI(?o)) }",
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
    fn test_add_link_creates_metadata_in_default_graph() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // Query for metadata on the graph IRI in default graph
        let result = svc
            .query(
                r#"SELECT ?g ?p ?v WHERE {
                GRAPH ?g { <ad4m://src> <ad4m://pred> <ad4m://tgt> . }
                ?g ?p ?v .
                FILTER(STRSTARTS(STR(?p), "ad4m://ontology/"))
            }"#,
            )
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
        assert_eq!(author_row["v"].as_str().unwrap(), "did:key:z6Mktest");
    }

    #[test]
    fn test_remove_link_removes_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        svc.remove_link(&link).unwrap();

        let result = svc
            .query("SELECT ?s ?p ?o WHERE { GRAPH ?g { ?s ?p ?o } . FILTER(isIRI(?s)) }")
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            rows.is_empty(),
            "Triple still exists after removal: {}",
            result
        );
    }

    #[test]
    fn test_remove_link_removes_metadata() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        let graph = make_graph_iri(&link);
        svc.remove_link(&link).unwrap();

        // Check no metadata triples remain for the graph IRI
        let result = svc
            .query(&format!(
                r#"SELECT ?p ?v WHERE {{ <{}> ?p ?v . }}"#,
                graph.as_str()
            ))
            .unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(rows.is_empty(), "Metadata triples still exist: {}", result);
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
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://p", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://q", "ad4m://t3"))
            .unwrap();

        let results = svc
            .query_links(Some("ad4m://a"), None, None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.source == "ad4m://a"));
    }

    #[test]
    fn test_query_links_by_predicate() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://q", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link("ad4m://c", "ad4m://p", "ad4m://t3"))
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
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://q", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://c", "ad4m://r", "ad4m://t2"))
            .unwrap();

        let results = svc
            .query_links(None, None, Some("ad4m://t1"), None, None, None)
            .unwrap();
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|l| l.data.target == "ad4m://t1"));
    }

    #[test]
    fn test_query_links_by_source_and_predicate() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://q", "ad4m://t2"))
            .unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://p", "ad4m://t3"))
            .unwrap();

        let results = svc
            .query_links(Some("ad4m://a"), Some("ad4m://p"), None, None, None, None)
            .unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].data.target, "ad4m://t1");
    }

    #[test]
    fn test_query_links_by_source_predicate_target() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t1"))
            .unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t2"))
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
        let svc = new_service();
        let link = make_link_with_ts(
            "ad4m://s",
            "ad4m://p",
            "ad4m://t",
            "2024-06-01T12:00:00.000Z",
            "did:key:z6Mkauthor",
        );
        svc.add_link(&link).unwrap();

        let results = svc
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap();
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
        svc.add_link(&make_link_with_ts(
            "ad4m://s",
            "ad4m://p",
            "ad4m://t1",
            "2024-01-01T00:00:00Z",
            "did:key:z6Mk1",
        ))
        .unwrap();
        svc.add_link(&make_link_with_ts(
            "ad4m://s",
            "ad4m://p",
            "ad4m://t2",
            "2024-06-15T00:00:00Z",
            "did:key:z6Mk2",
        ))
        .unwrap();
        svc.add_link(&make_link_with_ts(
            "ad4m://s",
            "ad4m://p",
            "ad4m://t3",
            "2024-12-31T00:00:00Z",
            "did:key:z6Mk3",
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
        let svc = new_service();
        for i in 0..10 {
            svc.add_link(&make_link_with_ts(
                "ad4m://s",
                "ad4m://p",
                &format!("ad4m://t{}", i),
                &format!("2024-01-{:02}T00:00:00Z", i + 1),
                "did:key:z6Mk1",
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
        let svc = new_service();
        svc.add_link(&make_link(
            "flux://community1",
            "flux://has_channel",
            "flux://channel1",
        ))
        .unwrap();
        svc.add_link(&make_link(
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
        let svc = new_service();
        svc.add_link(&make_link(
            "flux://ch1",
            "flux://entry_type",
            "flux://channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            "flux://ch1",
            "flux://name",
            "literal:string:general",
        ))
        .unwrap();
        svc.add_link(&make_link(
            "flux://ch2",
            "flux://entry_type",
            "flux://channel",
        ))
        .unwrap();
        svc.add_link(&make_link(
            "flux://ch2",
            "flux://name",
            "literal:string:random",
        ))
        .unwrap();
        svc.add_link(&make_link(
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
        let link = make_link(
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
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        svc.clear().unwrap();
        let all = svc.get_all_links().unwrap();
        assert!(all.is_empty());
    }

    // ── Concurrent write protection tests (Tier 1, 3.1) ──

    #[test]
    fn test_concurrent_writes_no_panic() {
        let svc = new_service();
        let svc = Arc::new(svc);
        let mut handles = vec![];
        for thread_id in 0..10 {
            let svc = svc.clone();
            handles.push(std::thread::spawn(move || {
                for i in 0..100 {
                    let link = make_link_with_ts(
                        &format!("ad4m://src_{}", thread_id),
                        "ad4m://pred",
                        &format!("ad4m://tgt_{}_{}", thread_id, i),
                        &format!("2024-01-01T{:02}:{:02}:00Z", thread_id, i),
                        "did:key:z6Mktest",
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
            for i in 0..200 {
                let link = make_link_with_ts(
                    "ad4m://src",
                    "ad4m://pred",
                    &format!("ad4m://tgt_{}", i),
                    &format!("2024-01-01T00:{:02}:{:02}Z", i / 60, i % 60),
                    "did:key:z6Mktest",
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
        // Add 200 links: 100 "keep" and 100 "remove"
        for i in 0..100 {
            svc.add_link(&make_link_with_ts(
                "ad4m://keep",
                "ad4m://pred",
                &format!("ad4m://tgt_{}", i),
                &format!("2024-01-01T00:{:02}:00Z", i),
                "did:key:z6Mktest",
            ))
            .unwrap();
            svc.add_link(&make_link_with_ts(
                "ad4m://remove",
                "ad4m://pred",
                &format!("ad4m://tgt_{}", i),
                &format!("2024-01-01T01:{:02}:00Z", i),
                "did:key:z6Mktest",
            ))
            .unwrap();
        }

        // Remove the "remove" links in parallel from 5 threads
        let mut handles = vec![];
        for chunk_start in (0..100).step_by(20) {
            let svc = svc.clone();
            handles.push(std::thread::spawn(move || {
                for i in chunk_start..chunk_start + 20 {
                    let link = make_link_with_ts(
                        "ad4m://remove",
                        "ad4m://pred",
                        &format!("ad4m://tgt_{}", i),
                        &format!("2024-01-01T01:{:02}:00Z", i),
                        "did:key:z6Mktest",
                    );
                    svc.remove_link(&link).unwrap();
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

    // ── Persistence tests (Tier 1, 3.2) ──
    // Note: Persistence via Store::open() requires the `rocksdb` feature which is not
    // currently enabled. These tests document the expected behavior for when it is.

    #[test]
    fn test_inmemory_store_for_tests() {
        let svc = SparqlStore::new(None).unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t"))
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
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://p", "ad4m://t"))
            .unwrap();
        assert!(svc.has_data());
    }

    // ── Error messages with query text (Tier 3, 3.10) ──

    #[test]
    fn test_validation_error_is_descriptive() {
        // Parser-based validation produces descriptive errors from Oxigraph
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
        // A valid query on an empty store should return Ok with empty results
        let result = svc.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        assert!(result.is_ok());
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result.unwrap()).unwrap();
        assert_eq!(rows.len(), 0);
    }

    // ── Parser-based SPARQL validation (Tier 3, 3.8) ──

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
    fn test_rejects_insert_in_comment() {
        // "INSERT" in a comment should NOT cause rejection — the parser handles this correctly
        let query = "SELECT * WHERE { ?s ?p ?o }\n# INSERT is just a comment here";
        // This is valid SPARQL (comment doesn't affect parsing)
        assert!(validate_readonly_query(query).is_ok());
    }

    #[test]
    fn test_rejects_syntactically_invalid_sparql() {
        assert!(validate_readonly_query("NOT VALID SPARQL AT ALL").is_err());
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

    // ── Named Graph Tests ──

    #[test]
    fn test_named_graph_iri_is_deterministic() {
        let link = make_link("ad4m://a", "ad4m://p", "ad4m://t");
        let iri1 = make_graph_iri(&link);
        let iri2 = make_graph_iri(&link);
        assert_eq!(iri1, iri2, "Same link data should produce same graph IRI");
    }

    #[test]
    fn test_named_graph_iri_differs_for_different_timestamps() {
        let link1 = make_link_with_ts(
            "ad4m://a",
            "ad4m://p",
            "ad4m://t",
            "2024-01-01T00:00:00Z",
            "did:key:z6Mk1",
        );
        let link2 = make_link_with_ts(
            "ad4m://a",
            "ad4m://p",
            "ad4m://t",
            "2024-01-02T00:00:00Z",
            "did:key:z6Mk1",
        );
        let iri1 = make_graph_iri(&link1);
        let iri2 = make_graph_iri(&link2);
        assert_ne!(
            iri1, iri2,
            "Different timestamps should produce different graph IRIs"
        );
    }

    #[test]
    fn test_link_stored_in_named_graph() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // The direct triple should NOT be in the default graph
        let default_quads: Vec<_> = svc
            .store
            .quads_for_pattern(None, None, None, Some(GraphNameRef::DefaultGraph))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        // Default graph should only have metadata triples (with link: IRI subjects)
        for quad in &default_quads {
            match &quad.subject {
                Subject::NamedNode(n) => {
                    assert!(
                        n.as_str().starts_with("link:"),
                        "Default graph should only have graph IRI subjects, found: {}",
                        n.as_str()
                    );
                }
                _ => panic!("Unexpected non-NamedNode subject in default graph"),
            }
        }

        // The direct triple should be in a named graph
        let graph = make_graph_iri(&link);
        let named_quads: Vec<_> = svc
            .store
            .quads_for_pattern(
                None,
                None,
                None,
                Some(GraphNameRef::NamedNode(graph.as_ref())),
            )
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(
            named_quads.len(),
            1,
            "Expected exactly 1 triple in named graph"
        );
    }

    #[test]
    fn test_metadata_in_default_graph() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        let graph = make_graph_iri(&link);
        let meta_quads: Vec<_> = svc
            .store
            .quads_for_pattern(
                Some(graph.as_ref().into()),
                None,
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(meta_quads.len(), 6, "Expected 6 metadata triples (author, timestamp, proofKey, proofSig, proofValid, status)");
    }

    #[test]
    fn test_remove_cleans_both_graph_and_metadata() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();
        let graph = make_graph_iri(&link);

        svc.remove_link(&link).unwrap();

        // Named graph should be empty
        let named_quads: Vec<_> = svc
            .store
            .quads_for_pattern(
                None,
                None,
                None,
                Some(GraphNameRef::NamedNode(graph.as_ref())),
            )
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(
            named_quads.is_empty(),
            "Named graph still has triples after removal"
        );

        // Default graph metadata should be empty
        let meta_quads: Vec<_> = svc
            .store
            .quads_for_pattern(
                Some(graph.as_ref().into()),
                None,
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(
            meta_quads.is_empty(),
            "Default graph still has metadata after removal"
        );
    }

    #[test]
    fn test_unscoped_query_finds_named_graph_triples() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://src", "ad4m://pred", "ad4m://tgt"))
            .unwrap();

        // Unscoped query (no GRAPH wrapper) should still find the triple
        // because we set default_graph_as_union
        let result = svc.query(
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(?s = <ad4m://src> && ?p = <ad4m://pred>) }"
        ).unwrap();
        let rows: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert!(
            !rows.is_empty(),
            "Unscoped query should find named graph triples via union default graph"
        );
    }

    #[test]
    fn test_persistent_store_survives_drop() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().to_str().unwrap();

        // Create store, add data, drop
        {
            let store = SparqlStore::new(Some(path)).unwrap();
            store
                .add_link(&make_link("ad4m://src", "ad4m://pred", "ad4m://tgt"))
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
        {
            let store = SparqlStore::new(None).unwrap();
            store
                .add_link(&make_link("ad4m://src", "ad4m://pred", "ad4m://tgt"))
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
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().to_str().unwrap();

        // First "startup": no data, rebuild needed
        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(
                !store.has_data(),
                "Fresh persistent store should have no data"
            );
            // Simulate rebuild
            store
                .add_link(&make_link("ad4m://a", "ad4m://b", "ad4m://c"))
                .unwrap();
            assert!(store.has_data());
        }

        // Second "startup": data persisted, skip rebuild
        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(
                store.has_data(),
                "Persistent store should have data on second open — rebuild should be skipped"
            );
        }
    }

    #[test]
    fn test_shacl_constructor_lookup_via_sparql_store() {
        // Simulate the full flow: parse SHACL → add links to store → look up constructor
        use super::super::shacl_parser::parse_shacl_to_links;

        let svc = new_service();

        // SHACL JSON similar to what Community model generates
        let shacl_json = r#"{
            "target_class": "flux://Community",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "flux://entry_type", "target": "community"}
            ],
            "destructor_actions": [],
            "properties": [
                {
                    "path": "flux://entry_type",
                    "name": "type",
                    "has_value": "community",
                    "min_count": 1,
                    "max_count": 1
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Community").unwrap();
        assert!(!links.is_empty(), "parse_shacl_to_links should produce links");

        // Check that a constructor link exists in parsed output
        let constructor_link = links.iter().find(|l|
            l.predicate.as_deref() == Some("ad4m://constructor")
        );
        assert!(constructor_link.is_some(), "Should have a constructor link in parsed SHACL");
        let constructor_link = constructor_link.unwrap();
        assert!(constructor_link.source.ends_with("CommunityShape"),
            "Constructor link source should end with CommunityShape, got: {}", constructor_link.source);

        // Add all SHACL links to the store
        for link in &links {
            let decorated = make_link(
                &link.source,
                link.predicate.as_deref().unwrap_or(""),
                &link.target,
            );
            svc.add_link(&decorated).unwrap();
        }

        // Now try to look up the constructor the same way get_shape_actions_from_shacl does
        let result = svc.get_links_by_predicate_and_source_suffix("ad4m://constructor", "CommunityShape").unwrap();

        assert!(!result.is_empty(),
            "Should find constructor link for CommunityShape. \
             All links in store: {:?}",
            svc.get_all_links().unwrap().iter().map(|l| format!("{} -> {:?} -> {}", l.data.source, l.data.predicate, l.data.target)).collect::<Vec<_>>()
        );

        assert!(result[0].data.source.ends_with("CommunityShape"));
        assert_eq!(result[0].data.predicate.as_deref(), Some("ad4m://constructor"));
    }

    #[test]
    fn test_shacl_constructor_survives_disk_persistence() {
        use super::super::shacl_parser::parse_shacl_to_links;
        use tempfile::TempDir;

        let dir = TempDir::new().unwrap();
        let path = dir.path().to_str().unwrap();

        let shacl_json = r#"{
            "target_class": "flux://Community",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "flux://entry_type", "target": "flux://has_community"}
            ],
            "destructor_actions": [],
            "properties": [
                {
                    "path": "flux://entry_type",
                    "name": "type",
                    "min_count": 1,
                    "max_count": 1
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Community").unwrap();

        // Store links in persistent store
        {
            let svc = SparqlStore::new(Some(path)).unwrap();
            for link in &links {
                let decorated = make_link(
                    &link.source,
                    link.predicate.as_deref().unwrap_or(""),
                    &link.target,
                );
                svc.add_link(&decorated).unwrap();
            }
            assert!(svc.has_data());
            // svc dropped here
        }

        // Reopen and query
        {
            let svc = SparqlStore::new(Some(path)).unwrap();
            assert!(svc.has_data(), "Persistent store should have data after reopen");

            let result = svc.get_links_by_predicate_and_source_suffix("ad4m://constructor", "CommunityShape").unwrap();
            assert!(!result.is_empty(),
                "Constructor link should survive disk persistence. All links: {:?}",
                svc.get_all_links().unwrap().iter().map(|l| format!("{} -> {:?} -> {}", l.data.source, l.data.predicate, l.data.target)).collect::<Vec<_>>()
            );
        }
    }
}
