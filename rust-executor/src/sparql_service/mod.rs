use crate::graphql::graphql_types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use deno_core::anyhow::{anyhow, Error};
use lazy_static::lazy_static;
use oxigraph::model::*;
use oxigraph::sparql::{QueryOptions, QueryResults};
use oxigraph::store::Store;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::sync::Arc;

const LINK_PREFIX: &str = "ad4m://link/";

const ONT_LINK: &str = "ad4m://ontology/Link";
const ONT_SOURCE: &str = "ad4m://ontology/source";
const ONT_TARGET: &str = "ad4m://ontology/target";
const ONT_PREDICATE: &str = "ad4m://ontology/predicate";
const ONT_AUTHOR: &str = "ad4m://ontology/author";
const ONT_TIMESTAMP: &str = "ad4m://ontology/timestamp";
const ONT_PROOF_KEY: &str = "ad4m://ontology/proofKey";
const ONT_PROOF_SIG: &str = "ad4m://ontology/proofSignature";
const ONT_PROOF_VALID: &str = "ad4m://ontology/proofValid";
const ONT_STATUS: &str = "ad4m://ontology/status";

fn link_uri(link: &DecoratedLinkExpression) -> String {
    let mut hasher = Sha256::new();
    hasher.update(link.data.source.as_bytes());
    hasher.update(link.data.predicate.as_deref().unwrap_or("").as_bytes());
    hasher.update(link.data.target.as_bytes());
    hasher.update(link.author.as_bytes());
    hasher.update(link.timestamp.as_bytes());
    format!("{}{:x}", LINK_PREFIX, hasher.finalize())
}

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

fn link_from_store(store: &Store, subject: &NamedNode) -> Option<DecoratedLinkExpression> {
    let subj = subject.as_ref();
    let get = |ont_uri: &str| -> Option<String> {
        store
            .quads_for_pattern(
                Some(subj.into()),
                Some(NamedNodeRef::new_unchecked(ont_uri)),
                None,
                None,
            )
            .next()
            .and_then(|q| q.ok())
            .and_then(|q| match q.object {
                Term::Literal(l) => Some(l.value().to_string()),
                _ => None,
            })
    };

    let source = get(ONT_SOURCE)?;
    let target = get(ONT_TARGET)?;
    let predicate = get(ONT_PREDICATE).and_then(|p| if p.is_empty() { None } else { Some(p) });
    let author = get(ONT_AUTHOR).unwrap_or_default();
    let timestamp = get(ONT_TIMESTAMP).unwrap_or_default();
    let proof_key = get(ONT_PROOF_KEY).unwrap_or_default();
    let proof_sig = get(ONT_PROOF_SIG).unwrap_or_default();
    let proof_valid = get(ONT_PROOF_VALID).map(|v| v == "true");
    let status_val = get(ONT_STATUS);
    let status = match status_val.as_deref() {
        Some("Local") => Some(LinkStatus::Local),
        Some("Shared") => Some(LinkStatus::Shared),
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

/// Oxigraph-backed SPARQL store for AD4M link data.
/// Synchronous API — Oxigraph operations are not async.
#[derive(Clone)]
pub struct SparqlService {
    store: Arc<Store>,
}

impl SparqlService {
    /// Create a new SparqlService with an in-memory store.
    /// Links are synced from the perspective on startup via sync_existing_links_to_sparql().
    pub fn new(_data_path: Option<&str>) -> Result<Self, Error> {
        let store = Store::new()?;
        Ok(SparqlService {
            store: Arc::new(store),
        })
    }

    fn insert_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
        let uri_str = link_uri(link);
        let subj = NamedNode::new(&uri_str)?;
        let subj_ref = subj.as_ref();

        let predicate_val = link.data.predicate.as_deref().unwrap_or("");
        let proof = &link.proof;
        let valid_str = proof.valid.unwrap_or(false).to_string();

        let rdf_type =
            NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

        let lit_source = literal(&link.data.source);
        let lit_target = literal(&link.data.target);
        let lit_predicate = literal(predicate_val);
        let lit_author = literal(&link.author);
        let lit_timestamp = literal(&link.timestamp);
        let lit_proof_key = literal(&proof.key);
        let lit_proof_sig = literal(&proof.signature);
        let lit_proof_valid = literal(&valid_str);
        let lit_status = literal(status_str(&link.status));

        let triples: [(NamedNodeRef, TermRef); 10] = [
            (rdf_type, NamedNodeRef::new_unchecked(ONT_LINK).into()),
            (
                NamedNodeRef::new_unchecked(ONT_SOURCE),
                lit_source.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_TARGET),
                lit_target.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_PREDICATE),
                lit_predicate.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_AUTHOR),
                lit_author.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_TIMESTAMP),
                lit_timestamp.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_PROOF_KEY),
                lit_proof_key.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_PROOF_SIG),
                lit_proof_sig.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_PROOF_VALID),
                lit_proof_valid.as_ref().into(),
            ),
            (
                NamedNodeRef::new_unchecked(ONT_STATUS),
                lit_status.as_ref().into(),
            ),
        ];

        for (pred, obj) in &triples {
            self.store.insert(QuadRef::new(
                subj_ref,
                *pred,
                *obj,
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
        let uri_str = link_uri(link);
        let subj = NamedNode::new(&uri_str)?;
        let subj_ref = subj.as_ref();
        let quads: Vec<_> = self
            .store
            .quads_for_pattern(Some(subj_ref.into()), None, None, None)
            .collect::<Result<Vec<_>, _>>()?;
        for quad in quads {
            self.store.remove(&quad)?;
        }
        Ok(())
    }

    /// Return all links in the store.
    pub fn get_all_links(&self) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let rdf_type =
            NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
        let link_type = NamedNodeRef::new_unchecked(ONT_LINK);
        let mut links = Vec::new();
        for quad in self
            .store
            .quads_for_pattern(None, Some(rdf_type), Some(link_type.into()), None)
        {
            let quad = quad?;
            if let Subject::NamedNode(subj) = quad.subject {
                if let Some(link) = link_from_store(&self.store, &subj) {
                    links.push(link);
                }
            }
        }
        Ok(links)
    }

    /// Find links matching optional filters.
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

    /// Execute an arbitrary read-only SPARQL SELECT query, returning a JSON string.
    pub fn query(&self, query_string: &str) -> Result<String, Error> {
        validate_readonly_query(query_string)?;

        let options = QueryOptions::default()
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/parse_literal"),
                parse_literal_fn,
            )
            .with_custom_function(
                NamedNode::new_unchecked("ad4m://fn/strip_html"),
                strip_html_fn,
            );

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
