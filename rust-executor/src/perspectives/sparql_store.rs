use crate::graphql::graphql_types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
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
const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";

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

/// Validates that a SPARQL query is read-only by parsing it with the SPARQL parser.
/// Only SELECT, ASK, CONSTRUCT, and DESCRIBE queries are accepted.
/// UPDATE operations (INSERT, DELETE, DROP, etc.) will fail to parse as a Query.
pub fn validate_readonly_query(query: &str) -> Result<(), Error> {
    let _ = SparqlEvaluator::new()
        .parse_query(query)
        .map_err(|e| {
            anyhow!(
                "Query is not valid read-only SPARQL (only SELECT/ASK/CONSTRUCT/DESCRIBE allowed): {}",
                e
            )
        })?;
    Ok(())
}

/// Generate a deterministic reifier IRI from link data + timestamp.
fn make_reifier_iri(link: &DecoratedLinkExpression) -> NamedNode {
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
/// Uses RDF 1.2 reifiers: direct triples in default graph with metadata
/// attached via `rdf:reifies` triple terms.
///
/// # Storage Model
/// Each link is stored as:
/// 1. Direct triple: `<source> <predicate> <target> .` (default graph)
/// 2. Reifier: `<link:HASH> rdf:reifies <<( source predicate target )>> .`
/// 3. Metadata: `<link:HASH> ad4m://ontology/* "value" .` (default graph)
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

    /// Returns true if the store contains any quads (non-empty).
    pub fn has_data(&self) -> bool {
        self.store
            .quads_for_pattern(None, None, None, None)
            .next()
            .is_some()
    }

    fn insert_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link);
        let reifier_iri = make_reifier_iri(link);

        // 1. Direct triple in default graph
        self.store.insert(QuadRef::new(
            source_iri.as_ref(),
            predicate_iri.as_ref(),
            TermRef::NamedNode(target_iri.as_ref()),
            GraphNameRef::DefaultGraph,
        ))?;

        // 2. Reifier: <link:HASH> rdf:reifies <<( source predicate target )>>
        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);
        let triple_term = Triple::new(
            source_iri.clone(),
            predicate_iri.clone(),
            target_iri.clone(),
        );
        self.store.insert(QuadRef::new(
            reifier_iri.as_ref(),
            rdf_reifies,
            TermRef::Triple(&triple_term),
            GraphNameRef::DefaultGraph,
        ))?;

        // 3. Metadata on the reifier node (all default graph)
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
                reifier_iri.as_ref(),
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
        let (source, predicate, target) = make_direct_triple(link);
        let triple_term = Triple::new(source.clone(), predicate.clone(), target.clone());
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
            self.store.remove(QuadRef::new(
                source.as_ref(),
                predicate.as_ref(),
                TermRef::NamedNode(target.as_ref()),
                GraphNameRef::DefaultGraph,
            ))?;
        }

        Ok(())
    }

    /// Return all links in the store using a SPARQL 1.2 reifier query.
    pub fn get_all_links(&self) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?source ?predicate ?target ?author ?timestamp ?proofKey ?proofSig ?proofValid ?status WHERE {
                ?source ?predicate ?target .
                ?reifier rdf:reifies <<( ?source ?predicate ?target )>> .
                FILTER(isIRI(?source) && isIRI(?predicate))
                ?reifier <ad4m://ontology/author> ?author .
                ?reifier <ad4m://ontology/timestamp> ?timestamp .
                OPTIONAL { ?reifier <ad4m://ontology/proofKey> ?proofKey . }
                OPTIONAL { ?reifier <ad4m://ontology/proofSignature> ?proofSig . }
                OPTIONAL { ?reifier <ad4m://ontology/proofValid> ?proofValid . }
                OPTIONAL { ?reifier <ad4m://ontology/status> ?status . }
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
        let rdf_reifies = NamedNodeRef::new_unchecked(RDF_REIFIES);
        let ont_author = NamedNodeRef::new_unchecked(ONT_AUTHOR);
        let ont_timestamp = NamedNodeRef::new_unchecked(ONT_TIMESTAMP);
        let ont_proof_key = NamedNodeRef::new_unchecked(ONT_PROOF_KEY);
        let ont_proof_sig = NamedNodeRef::new_unchecked(ONT_PROOF_SIG);
        let ont_proof_valid = NamedNodeRef::new_unchecked(ONT_PROOF_VALID);
        let ont_status = NamedNodeRef::new_unchecked(ONT_STATUS);

        // Search direct triples in the default graph
        for quad_result in self
            .store
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
            let tgt = match &quad.object {
                Term::NamedNode(n) => n.as_str().to_string(),
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

                let reifier_subject: NamedOrBlankNodeRef =
                    reifier_node.as_ref().into();

                let get_annotation = |pred_node: NamedNodeRef| -> String {
                    self.store
                        .quads_for_pattern(
                            Some(reifier_subject),
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
                        source: src.clone(),
                        predicate: if pred.is_empty() {
                            None
                        } else {
                            Some(pred.clone())
                        },
                        target: tgt.clone(),
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

    fn sparql_evaluator(&self) -> SparqlEvaluator {
        SparqlEvaluator::new()
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
    /// All data lives in the default graph — no union graph needed.
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
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

    /// Check the migration version stored in the store.
    /// Returns 0 if no migration version is found.
    pub fn migration_version(&self) -> u32 {
        let migration_subj = NamedNodeRef::new_unchecked("ad4m://system/migration");
        let migration_pred = NamedNodeRef::new_unchecked("ad4m://system/version");
        self.store
            .quads_for_pattern(
                Some(migration_subj.into()),
                Some(migration_pred),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .next()
            .and_then(|r| r.ok())
            .and_then(|q| match &q.object {
                Term::Literal(l) => l.value().parse::<u32>().ok(),
                _ => None,
            })
            .unwrap_or(0)
    }

    /// Set the migration version marker.
    pub fn set_migration_version(&self, version: u32) -> Result<(), Error> {
        let migration_subj = NamedNode::new_unchecked("ad4m://system/migration");
        let migration_pred = NamedNodeRef::new_unchecked("ad4m://system/version");

        // Remove old version marker if any
        let old_quads: Vec<_> = self
            .store
            .quads_for_pattern(
                Some(migration_subj.as_ref().into()),
                Some(migration_pred),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .collect::<Result<Vec<_>, _>>()?;
        for q in &old_quads {
            self.store.remove(q)?;
        }

        // Insert new version
        let version_lit = Literal::new_simple_literal(&version.to_string());
        self.store.insert(QuadRef::new(
            migration_subj.as_ref(),
            migration_pred,
            TermRef::Literal(version_lit.as_ref()),
            GraphNameRef::DefaultGraph,
        ))?;

        Ok(())
    }

    /// Migrate data from named-graph storage model to reifier storage model.
    /// Returns the number of links migrated.
    pub fn migrate_named_graphs_to_reifiers(&self) -> Result<usize, Error> {
        // Check if already migrated
        if self.migration_version() >= 2 {
            return Ok(0);
        }

        // Check if there are any named graphs (old storage model)
        let has_named_graphs = self
            .store
            .named_graphs()
            .next()
            .is_some();

        if !has_named_graphs {
            // No old data to migrate, just set version
            self.set_migration_version(2)?;
            return Ok(0);
        }

        log::info!("Migrating link storage from named graphs to RDF 1.2 reifiers...");

        // Collect all old-format links by querying named graphs
        // We use the deprecated query_opt with set_default_graph_as_union to read old data
        let query = r#"
            SELECT ?g ?source ?predicate ?target ?author ?timestamp
                   ?proofKey ?proofSig ?proofValid ?status
            WHERE {
                GRAPH ?g { ?source ?predicate ?target . }
                FILTER(isIRI(?source) && isIRI(?predicate))
                ?g <ad4m://ontology/author> ?author .
                ?g <ad4m://ontology/timestamp> ?timestamp .
                OPTIONAL { ?g <ad4m://ontology/proofKey> ?proofKey . }
                OPTIONAL { ?g <ad4m://ontology/proofSignature> ?proofSig . }
                OPTIONAL { ?g <ad4m://ontology/proofValid> ?proofValid . }
                OPTIONAL { ?g <ad4m://ontology/status> ?status . }
            }
        "#;

        // Use the deprecated API to read old named-graph data
        #[allow(deprecated)]
        let results = {
            let evaluator = self.sparql_evaluator();
            let mut parsed_query = oxigraph::sparql::Query::parse(query, None)
                .map_err(|e| anyhow!("Failed to parse migration query: {}", e))?;
            parsed_query.dataset_mut().set_default_graph_as_union();
            self.store.query_opt(parsed_query, evaluator)?
        };

        let mut links_to_migrate: Vec<DecoratedLinkExpression> = Vec::new();
        let mut graph_iris: Vec<NamedNode> = Vec::new();

        if let QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                let solution = solution?;

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

                if let Some(Term::NamedNode(g)) = solution.get("g") {
                    graph_iris.push(g.clone());
                }

                let source = match solution.get("source") {
                    Some(Term::NamedNode(n)) => n.as_str().to_string(),
                    _ => continue,
                };
                let predicate = match solution.get("predicate") {
                    Some(Term::NamedNode(n)) => {
                        let s = n.as_str().to_string();
                        if s.is_empty() { None } else { Some(s) }
                    }
                    _ => continue,
                };
                let target = match solution.get("target") {
                    Some(Term::NamedNode(n)) => n.as_str().to_string(),
                    _ => continue,
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
                    _ => None,
                };

                links_to_migrate.push(DecoratedLinkExpression {
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
                });
            }
        }

        let count = links_to_migrate.len();
        log::info!("Found {} links in named-graph format to migrate", count);

        // Remove all old named-graph data
        // First remove named graph contents and metadata
        for graph_iri in &graph_iris {
            // Remove quads in the named graph
            let ng_quads: Vec<_> = self
                .store
                .quads_for_pattern(
                    None,
                    None,
                    None,
                    Some(GraphNameRef::NamedNode(graph_iri.as_ref())),
                )
                .collect::<Result<Vec<_>, _>>()?;
            for q in &ng_quads {
                self.store.remove(q)?;
            }

            // Remove metadata in default graph
            let meta_quads: Vec<_> = self
                .store
                .quads_for_pattern(
                    Some(graph_iri.as_ref().into()),
                    None,
                    None,
                    Some(GraphNameRef::DefaultGraph),
                )
                .collect::<Result<Vec<_>, _>>()?;
            for q in &meta_quads {
                self.store.remove(q)?;
            }

            // Remove the named graph itself
            let _ = self.store.remove_named_graph(graph_iri.as_ref());
        }

        // Insert new reifier-format data
        for link in &links_to_migrate {
            self.insert_link_triples(link)?;
        }

        // Set migration version
        self.set_migration_version(2)?;

        log::info!("Migration complete: {} links migrated to reifier format", count);
        Ok(count)
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

    // ── Storage Model Tests (Reifier Model) ──

    #[test]
    fn test_add_link_creates_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://source1", "ad4m://predicate1", "ad4m://target1");
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
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
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
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
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
        assert_eq!(author_row["v"].as_str().unwrap(), "did:key:z6Mktest");
    }

    #[test]
    fn test_remove_link_removes_direct_triple() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
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
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
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
        let svc = new_service();
        // Two different links with same s/p/o but different timestamps
        let link1 = make_link_with_ts(
            "ad4m://src", "ad4m://pred", "ad4m://tgt",
            "2024-01-01T00:00:00Z", "did:key:z6Mk1",
        );
        let link2 = make_link_with_ts(
            "ad4m://src", "ad4m://pred", "ad4m://tgt",
            "2024-01-02T00:00:00Z", "did:key:z6Mk2",
        );
        svc.add_link(&link1).unwrap();
        svc.add_link(&link2).unwrap();

        // Remove link1 — direct triple should remain because link2 still references it
        svc.remove_link(&link1).unwrap();

        let all = svc.get_all_links().unwrap();
        assert_eq!(all.len(), 1, "Should have 1 link remaining");
        assert_eq!(all[0].author, "did:key:z6Mk2");
    }

    #[test]
    fn test_no_named_graphs_used() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
        svc.add_link(&link).unwrap();

        // No named graphs should exist
        let named: Vec<_> = svc.store.named_graphs().collect::<Result<Vec<_>, _>>().unwrap();
        assert!(named.is_empty(), "No named graphs should be used in reifier model");
    }

    #[test]
    fn test_all_data_in_default_graph() {
        let svc = new_service();
        let link = make_link("ad4m://src", "ad4m://pred", "ad4m://tgt");
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

    // ── Concurrent write protection tests ──

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

    // ── Persistence tests ──

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
        let link = make_link("ad4m://a", "ad4m://p", "ad4m://t");
        let iri1 = make_reifier_iri(&link);
        let iri2 = make_reifier_iri(&link);
        assert_eq!(iri1, iri2, "Same link data should produce same reifier IRI");
    }

    #[test]
    fn test_reifier_iri_differs_for_different_timestamps() {
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
        let svc = new_service();
        svc.add_link(&make_link("ad4m://src", "ad4m://pred", "ad4m://tgt"))
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

        {
            let store = SparqlStore::new(Some(path)).unwrap();
            assert!(!store.has_data());
            store
                .add_link(&make_link("ad4m://a", "ad4m://b", "ad4m://c"))
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
        let svc = new_service();
        for i in 0..50 {
            let src = format!("ad4m://source{}", i);
            let link = make_link(&src, "ad4m://pred", "ad4m://target");
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
        let svc = new_service();
        for i in 0..20 {
            let pred = format!("ad4m://pred{}", i % 4);
            let src = format!("ad4m://src{}", i);
            let link = make_link(&src, &pred, "ad4m://tgt");
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
        let svc = new_service();
        for i in 0..10 {
            let tgt = format!("ad4m://tgt{}", i % 3);
            let src = format!("ad4m://src{}", i);
            let link = make_link(&src, "ad4m://pred", &tgt);
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
        let svc = new_service();
        let timestamps = [
            "2024-01-10T00:00:00.000Z",
            "2024-01-15T00:00:00.000Z",
            "2024-01-20T00:00:00.000Z",
            "2024-01-25T00:00:00.000Z",
        ];
        for (i, ts) in timestamps.iter().enumerate() {
            let src = format!("ad4m://src{}", i);
            let link = make_link_with_ts(&src, "ad4m://pred", "ad4m://tgt", ts, "did:key:z6Mktest");
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
        let svc = new_service();
        for i in 0..20 {
            let src = format!("ad4m://src{}", i);
            let link = make_link(&src, "ad4m://pred", "ad4m://tgt");
            svc.add_link(&link).unwrap();
        }
        let results = svc
            .query_links(None, None, None, None, None, Some(5))
            .unwrap();
        assert_eq!(results.len(), 5);
    }

    #[test]
    fn query_links_combined_filters() {
        let svc = new_service();
        svc.add_link(&make_link("ad4m://a", "ad4m://likes", "ad4m://b"))
            .unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://knows", "ad4m://c"))
            .unwrap();
        svc.add_link(&make_link("ad4m://b", "ad4m://likes", "ad4m://c"))
            .unwrap();
        svc.add_link(&make_link("ad4m://a", "ad4m://likes", "ad4m://c"))
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
        let svc = new_service();
        for i in 0..10 {
            let src = format!("ad4m://src{}", i);
            svc.add_link(&make_link(&src, "ad4m://pred", "ad4m://tgt"))
                .unwrap();
        }
        let results = svc.query_links(None, None, None, None, None, None).unwrap();
        assert_eq!(results.len(), 10);
    }

    #[test]
    fn test_query_links_skips_literal_targets() {
        let svc = new_service();
        svc.add_link(&make_link(
            "ad4m://src",
            "ad4m://pred",
            "ad4m://normal_target",
        ))
        .unwrap();
        svc.add_link(&make_link(
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
        let svc = new_service();
        for i in 0..100 {
            svc.add_link(&make_link(
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
        let svc = new_service();
        svc.add_link(&make_link("ad4m://héllo", "ad4m://prédicat", "ad4m://目标"))
            .unwrap();
        svc.add_link(&make_link("ad4m://emoji🎉", "ad4m://pred", "ad4m://target"))
            .unwrap();
        svc.add_link(&make_link(
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
        let svc = new_service();
        let exact_ts = "2024-06-15T12:00:00.000Z";
        svc.add_link(&make_link_with_ts(
            "ad4m://s",
            "ad4m://p",
            "ad4m://t1",
            exact_ts,
            "did:key:z6Mk1",
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

    // ── Migration tests ──

    #[test]
    fn test_migration_version_default_zero() {
        let svc = new_service();
        assert_eq!(svc.migration_version(), 0);
    }

    #[test]
    fn test_migration_version_set_and_get() {
        let svc = new_service();
        svc.set_migration_version(2).unwrap();
        assert_eq!(svc.migration_version(), 2);
    }

    #[test]
    fn test_migration_no_named_graphs_sets_version() {
        let svc = new_service();
        // No old data, migration should just set version
        let count = svc.migrate_named_graphs_to_reifiers().unwrap();
        assert_eq!(count, 0);
        assert_eq!(svc.migration_version(), 2);
    }

    #[test]
    fn test_migration_skips_if_already_done() {
        let svc = new_service();
        svc.set_migration_version(2).unwrap();
        let count = svc.migrate_named_graphs_to_reifiers().unwrap();
        assert_eq!(count, 0);
    }
}
