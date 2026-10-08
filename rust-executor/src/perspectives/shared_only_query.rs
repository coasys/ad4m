//! Rewrite a caller-supplied SPARQL query so it reads Shared links only.
//!
//! The executor scope of [`link_visibility`](super::link_visibility) reads every
//! row, every user's Local links included. That is right for the engine's own
//! derivations, and wrong for a job whose output is published: the
//! auto-processor writes what it extracts as Shared, so a Local message it
//! gathered would be sent to the LLM and its extraction published to every
//! co-owner (#1058 review, item 2). Its gather runs the processor's
//! `source_scope_query`, which is arbitrary SPARQL, so the restriction cannot be
//! a fixed `WHERE` fragment. It is applied to the parsed algebra instead.
//!
//! Every link is stored as its bare triple plus a reifier that carries the
//! link's author, timestamp and status (`SparqlStore::add_link`). The rewrite
//! adds one condition per triple pattern of every basic graph pattern:
//!
//! - a pattern on a reifier (`rdf:reifies`, or an `ad4m://ontology/*`
//!   predicate) matches only when that reifier's status is `"Shared"`;
//! - any other pattern is a link triple, and matches only when some reifier of
//!   that exact triple has status `"Shared"`.
//!
//! Each condition is a `FILTER EXISTS`, so a triple shared by several authors
//! still yields one row. A Local link of anyone, the runner's own included,
//! matches nothing: the query sees the perspective as if Local links did not
//! exist. A triple with no status at all (data older than the store's status
//! requirement) does not match either, which is the fail-closed side.
//!
//! Not rewritten: `EXISTS` / `NOT EXISTS` inside a `FILTER` expression. They
//! decide which rows match but never bind a value, so no Local content reaches
//! the result through them. Property paths and `SERVICE` are refused, because a
//! path's intermediate hops cannot be checked one by one.

use deno_core::anyhow::{anyhow, Error as AnyError};
use spargebra::algebra::{Expression, GraphPattern};
use spargebra::term::{
    BlankNode, Literal, NamedNode, NamedNodePattern, TermPattern, TriplePattern, Variable,
};
use spargebra::Query;
use std::collections::HashMap;

const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
const ONTOLOGY_PREFIX: &str = "ad4m://ontology/";
const ONT_STATUS: &str = "ad4m://ontology/status";
const STATUS_SHARED: &str = "Shared";

/// `query`, restricted to Shared links. See the module docs for the rule.
pub fn shared_only_query(query: &str) -> Result<String, AnyError> {
    let parsed = Query::parse(query, None)
        .map_err(|e| anyhow!("shared_only_query: not valid SPARQL: {e}"))?;
    let mut rewriter = Rewriter::default();
    let rewritten = match parsed {
        Query::Select {
            dataset,
            pattern,
            base_iri,
        } => Query::Select {
            dataset,
            pattern: rewriter.pattern(pattern)?,
            base_iri,
        },
        Query::Ask {
            dataset,
            pattern,
            base_iri,
        } => Query::Ask {
            dataset,
            pattern: rewriter.pattern(pattern)?,
            base_iri,
        },
        Query::Construct { .. } | Query::Describe { .. } => {
            return Err(anyhow!(
                "shared_only_query: only SELECT and ASK can be restricted to Shared links"
            ))
        }
    };
    Ok(rewritten.to_string())
}

#[derive(Default)]
struct Rewriter {
    /// Fresh variables for the reifiers the added conditions bind.
    next_reifier: usize,
    /// Blank nodes of the query turned into variables. A blank node in a
    /// basic graph pattern is a variable that is not projected; inside a
    /// `FILTER EXISTS` it would be a new, unrelated one, so the added
    /// conditions could not refer to it.
    blank_nodes: HashMap<BlankNode, Variable>,
}

impl Rewriter {
    fn pattern(&mut self, pattern: GraphPattern) -> Result<GraphPattern, AnyError> {
        Ok(match pattern {
            GraphPattern::Bgp { patterns } => self.bgp(patterns)?,
            GraphPattern::Path { .. } => {
                return Err(anyhow!(
                    "shared_only_query: property paths cannot be restricted to Shared links"
                ))
            }
            GraphPattern::Service { .. } => {
                return Err(anyhow!("shared_only_query: SERVICE is not supported"))
            }
            GraphPattern::Join { left, right } => GraphPattern::Join {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            GraphPattern::LeftJoin {
                left,
                right,
                expression,
            } => GraphPattern::LeftJoin {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
                expression,
            },
            GraphPattern::Lateral { left, right } => GraphPattern::Lateral {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            GraphPattern::Filter { expr, inner } => GraphPattern::Filter {
                expr,
                inner: Box::new(self.pattern(*inner)?),
            },
            GraphPattern::Union { left, right } => GraphPattern::Union {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            GraphPattern::Graph { name, inner } => GraphPattern::Graph {
                name,
                inner: Box::new(self.pattern(*inner)?),
            },
            GraphPattern::Extend {
                inner,
                variable,
                expression,
            } => GraphPattern::Extend {
                inner: Box::new(self.pattern(*inner)?),
                variable,
                expression,
            },
            GraphPattern::Minus { left, right } => GraphPattern::Minus {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            values @ GraphPattern::Values { .. } => values,
            GraphPattern::OrderBy { inner, expression } => GraphPattern::OrderBy {
                inner: Box::new(self.pattern(*inner)?),
                expression,
            },
            GraphPattern::Project { inner, variables } => GraphPattern::Project {
                inner: Box::new(self.pattern(*inner)?),
                variables,
            },
            GraphPattern::Distinct { inner } => GraphPattern::Distinct {
                inner: Box::new(self.pattern(*inner)?),
            },
            GraphPattern::Reduced { inner } => GraphPattern::Reduced {
                inner: Box::new(self.pattern(*inner)?),
            },
            GraphPattern::Slice {
                inner,
                start,
                length,
            } => GraphPattern::Slice {
                inner: Box::new(self.pattern(*inner)?),
                start,
                length,
            },
            GraphPattern::Group {
                inner,
                variables,
                aggregates,
            } => GraphPattern::Group {
                inner: Box::new(self.pattern(*inner)?),
                variables,
                aggregates,
            },
        })
    }

    fn bgp(&mut self, patterns: Vec<TriplePattern>) -> Result<GraphPattern, AnyError> {
        let patterns = patterns
            .into_iter()
            .map(|p| self.triple(p))
            .collect::<Result<Vec<_>, _>>()?;
        let mut condition: Option<Expression> = None;
        for p in &patterns {
            let exists = Expression::Exists(Box::new(GraphPattern::Bgp {
                patterns: self.shared_condition(p),
            }));
            condition = Some(match condition {
                None => exists,
                Some(c) => Expression::And(Box::new(c), Box::new(exists)),
            });
        }
        let bgp = GraphPattern::Bgp { patterns };
        Ok(match condition {
            None => bgp,
            Some(expr) => GraphPattern::Filter {
                expr,
                inner: Box::new(bgp),
            },
        })
    }

    /// The patterns a `FILTER EXISTS` needs to admit `p`.
    fn shared_condition(&mut self, p: &TriplePattern) -> Vec<TriplePattern> {
        let shared = |subject: TermPattern| TriplePattern {
            subject,
            predicate: NamedNodePattern::NamedNode(NamedNode::new_unchecked(ONT_STATUS)),
            object: TermPattern::Literal(Literal::new_simple_literal(STATUS_SHARED)),
        };
        if is_reifier_predicate(&p.predicate) {
            return vec![shared(p.subject.clone())];
        }
        let reifier = TermPattern::Variable(Variable::new_unchecked(format!(
            "__ad4m_shared_reifier_{}",
            self.next_reifier
        )));
        self.next_reifier += 1;
        vec![
            TriplePattern {
                subject: reifier.clone(),
                predicate: NamedNodePattern::NamedNode(NamedNode::new_unchecked(RDF_REIFIES)),
                object: TermPattern::Triple(Box::new(p.clone())),
            },
            shared(reifier),
        ]
    }

    fn triple(&mut self, p: TriplePattern) -> Result<TriplePattern, AnyError> {
        Ok(TriplePattern {
            subject: self.term(p.subject)?,
            predicate: p.predicate,
            object: self.term(p.object)?,
        })
    }

    fn term(&mut self, term: TermPattern) -> Result<TermPattern, AnyError> {
        Ok(match term {
            TermPattern::BlankNode(b) => {
                let next = self.blank_nodes.len();
                TermPattern::Variable(
                    self.blank_nodes
                        .entry(b)
                        .or_insert_with(|| {
                            Variable::new_unchecked(format!("__ad4m_shared_blank_{next}"))
                        })
                        .clone(),
                )
            }
            TermPattern::Triple(t) => TermPattern::Triple(Box::new(self.triple(*t)?)),
            other => other,
        })
    }
}

/// A pattern on a link's reifier rather than on the link's own triple.
fn is_reifier_predicate(predicate: &NamedNodePattern) -> bool {
    match predicate {
        NamedNodePattern::NamedNode(n) => {
            n.as_str() == RDF_REIFIES || n.as_str().starts_with(ONTOLOGY_PREFIX)
        }
        NamedNodePattern::Variable(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn refuses_what_it_cannot_restrict() {
        assert!(shared_only_query("SELECT ?o WHERE { ?s <ns://a>+ ?o }").is_err());
        assert!(shared_only_query("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").is_err());
        assert!(shared_only_query("not sparql").is_err());
    }

    #[test]
    fn keeps_order_and_projection() {
        let q = shared_only_query(
            "SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text . \
             ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> . \
             ?r <ad4m://ontology/author> ?speaker . ?r <ad4m://ontology/timestamp> ?timestamp . } \
             ORDER BY ?timestamp",
        )
        .unwrap();
        assert!(q.contains("ORDER BY"), "{q}");
        assert!(q.contains("SELECT ?speaker ?text ?timestamp"), "{q}");
        assert!(q.contains("__ad4m_shared_reifier_0"), "{q}");
        // The rewritten text parses again.
        Query::parse(&q, None).unwrap();
    }
}
