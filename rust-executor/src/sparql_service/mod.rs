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

/// Build the direct triple (source, predicate, target as IRIs) for a link.
fn make_direct_triple(link: &DecoratedLinkExpression) -> Result<(NamedNode, NamedNode, NamedNode), Error> {
    let source_iri = NamedNode::new(&link.data.source)
        .map_err(|e| anyhow!("Invalid source IRI '{}': {}", link.data.source, e))?;
    let predicate_val = link.data.predicate.as_deref().unwrap_or("");
    let predicate_iri = NamedNode::new(predicate_val)
        .map_err(|e| anyhow!("Invalid predicate IRI '{}': {}", predicate_val, e))?;
    let target_iri = NamedNode::new(&link.data.target)
        .map_err(|e| anyhow!("Invalid target IRI '{}': {}", link.data.target, e))?;
    Ok((source_iri, predicate_iri, target_iri))
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
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link)?;

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
        let (source_iri, predicate_iri, target_iri) = make_direct_triple(link)?;

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

    /// Find links matching optional filters using SPARQL-star.
    pub fn get_link(
        &self,
        source: Option<&str>,
        predicate: Option<&str>,
        target: Option<&str>,
        author: Option<&str>,
        timestamp: Option<&str>,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let all = self.get_all_links()?;
        Ok(all
            .into_iter()
            .filter(|l| {
                source.map_or(true, |s| l.data.source == s)
                    && predicate.map_or(true, |p| l.data.predicate.as_deref() == Some(p))
                    && target.map_or(true, |t| l.data.target == t)
                    && author.map_or(true, |a| l.author == a)
                    && timestamp.map_or(true, |ts| l.timestamp == ts)
            })
            .collect())
    }

    /// Get all links with the given source.
    pub fn get_links_by_source(&self, source: &str) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.get_link(Some(source), None, None, None, None)
    }

    /// Get all links with the given target.
    pub fn get_links_by_target(&self, target: &str) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.get_link(None, None, Some(target), None, None)
    }

    /// Get all links with the given predicate.
    pub fn get_links_by_predicate(
        &self,
        predicate: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        self.get_link(None, Some(predicate), None, None, None)
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
            Term::NamedNode(n) => n.as_str().to_string(),
            _ => return None,
        };
        let predicate = match solution.get("predicate")? {
            Term::NamedNode(n) => {
                let s = n.as_str().to_string();
                if s.is_empty() { None } else { Some(s) }
            },
            _ => return None,
        };
        let target = match solution.get("target")? {
            Term::NamedNode(n) => n.as_str().to_string(),
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
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
        validate_readonly_query(query_string)?;

        let options = self.query_options();
        let results = self.store.query_opt(query_string, options)?;

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
