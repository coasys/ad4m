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
//! The rewrite also reaches every graph pattern nested in an expression: an
//! `EXISTS` / `NOT EXISTS` in a `FILTER`, a `BIND`, an `OPTIONAL`'s filter,
//! an `ORDER BY` key, a `HAVING` condition or an aggregate, at any depth and
//! inside sub-selects. Such a pattern decides more than which rows match:
//! wrapped in `IF`, `COALESCE`, an order key or an aggregate, its verdict
//! becomes a bound value or an ordering, so an unrestricted one would leak one
//! bit about Local links per row. Every match on the algebra below is
//! exhaustive, with no `_` arm, so a variant a future `spargebra` adds fails
//! to compile here instead of passing through unrestricted.
//!
//! Property paths and `SERVICE` are refused, because a path's intermediate hops
//! cannot be checked one by one.

use deno_core::anyhow::{anyhow, Error as AnyError};
use spargebra::algebra::{AggregateExpression, Expression, GraphPattern, OrderExpression};
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
                expression: expression.map(|e| self.expression(e)).transpose()?,
            },
            GraphPattern::Lateral { left, right } => GraphPattern::Lateral {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            GraphPattern::Filter { expr, inner } => GraphPattern::Filter {
                expr: self.expression(expr)?,
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
                expression: self.expression(expression)?,
            },
            GraphPattern::Minus { left, right } => GraphPattern::Minus {
                left: Box::new(self.pattern(*left)?),
                right: Box::new(self.pattern(*right)?),
            },
            // Constants only: no pattern and no expression to restrict.
            values @ GraphPattern::Values { .. } => values,
            GraphPattern::OrderBy { inner, expression } => GraphPattern::OrderBy {
                inner: Box::new(self.pattern(*inner)?),
                expression: expression
                    .into_iter()
                    .map(|e| self.order_expression(e))
                    .collect::<Result<_, _>>()?,
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
                aggregates: aggregates
                    .into_iter()
                    .map(|(v, a)| Ok((v, self.aggregate(a)?)))
                    .collect::<Result<_, AnyError>>()?,
            },
        })
    }

    /// `expression` with every graph pattern nested in it (`EXISTS`, which
    /// `NOT EXISTS` wraps) restricted like the query's own.
    fn expression(&mut self, expression: Expression) -> Result<Expression, AnyError> {
        Ok(match expression {
            Expression::Exists(p) => Expression::Exists(Box::new(self.pattern(*p)?)),
            leaf @ (Expression::NamedNode(_)
            | Expression::Literal(_)
            | Expression::Variable(_)
            | Expression::Bound(_)) => leaf,
            Expression::Or(a, b) => Expression::Or(self.boxed(a)?, self.boxed(b)?),
            Expression::And(a, b) => Expression::And(self.boxed(a)?, self.boxed(b)?),
            Expression::Equal(a, b) => Expression::Equal(self.boxed(a)?, self.boxed(b)?),
            Expression::SameTerm(a, b) => Expression::SameTerm(self.boxed(a)?, self.boxed(b)?),
            Expression::Greater(a, b) => Expression::Greater(self.boxed(a)?, self.boxed(b)?),
            Expression::GreaterOrEqual(a, b) => {
                Expression::GreaterOrEqual(self.boxed(a)?, self.boxed(b)?)
            }
            Expression::Less(a, b) => Expression::Less(self.boxed(a)?, self.boxed(b)?),
            Expression::LessOrEqual(a, b) => {
                Expression::LessOrEqual(self.boxed(a)?, self.boxed(b)?)
            }
            Expression::Add(a, b) => Expression::Add(self.boxed(a)?, self.boxed(b)?),
            Expression::Subtract(a, b) => Expression::Subtract(self.boxed(a)?, self.boxed(b)?),
            Expression::Multiply(a, b) => Expression::Multiply(self.boxed(a)?, self.boxed(b)?),
            Expression::Divide(a, b) => Expression::Divide(self.boxed(a)?, self.boxed(b)?),
            Expression::UnaryPlus(a) => Expression::UnaryPlus(self.boxed(a)?),
            Expression::UnaryMinus(a) => Expression::UnaryMinus(self.boxed(a)?),
            Expression::Not(a) => Expression::Not(self.boxed(a)?),
            Expression::If(a, b, c) => {
                Expression::If(self.boxed(a)?, self.boxed(b)?, self.boxed(c)?)
            }
            Expression::In(a, list) => Expression::In(self.boxed(a)?, self.expressions(list)?),
            Expression::Coalesce(list) => Expression::Coalesce(self.expressions(list)?),
            Expression::FunctionCall(f, args) => {
                Expression::FunctionCall(f, self.expressions(args)?)
            }
        })
    }

    fn boxed(&mut self, e: Box<Expression>) -> Result<Box<Expression>, AnyError> {
        Ok(Box::new(self.expression(*e)?))
    }

    fn expressions(&mut self, list: Vec<Expression>) -> Result<Vec<Expression>, AnyError> {
        list.into_iter().map(|e| self.expression(e)).collect()
    }

    fn order_expression(&mut self, e: OrderExpression) -> Result<OrderExpression, AnyError> {
        Ok(match e {
            OrderExpression::Asc(e) => OrderExpression::Asc(self.expression(e)?),
            OrderExpression::Desc(e) => OrderExpression::Desc(self.expression(e)?),
        })
    }

    fn aggregate(&mut self, a: AggregateExpression) -> Result<AggregateExpression, AnyError> {
        Ok(match a {
            count @ AggregateExpression::CountSolutions { .. } => count,
            AggregateExpression::FunctionCall {
                name,
                expr,
                distinct,
            } => AggregateExpression::FunctionCall {
                name,
                expr: self.expression(expr)?,
                distinct,
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

    /// Every `EXISTS` in an expression is restricted too, `NOT EXISTS` and
    /// nested ones included (#1058 review, item 4).
    #[test]
    fn restricts_exists_inside_expressions() {
        let q = shared_only_query(
            "SELECT ?t WHERE { ?m <ns://body> ?b . \
             OPTIONAL { ?m <ns://x> ?y FILTER(EXISTS { ?p <ns://a> ?q }) } \
             BIND(IF(EXISTS { ?x <ns://body> ?s }, 1, 0) AS ?t) \
             FILTER(NOT EXISTS { ?e <ns://b> ?f FILTER(EXISTS { ?g <ns://c> ?h }) }) } \
             GROUP BY ?t HAVING (COUNT(IF(EXISTS { ?i <ns://d> ?j }, 1, 0)) > 0) \
             ORDER BY DESC(IF(EXISTS { ?k <ns://e> ?l }, 1, 0))",
        )
        .unwrap();
        // One reifier condition per link triple: ?m body, ?m x, ?p a, ?x body,
        // ?e b, ?g c, ?i d, ?k e.
        for n in 0..8 {
            assert!(
                q.contains(&format!("__ad4m_shared_reifier_{n}")),
                "{n}: {q}"
            );
        }
        assert!(!q.contains("__ad4m_shared_reifier_8"), "{q}");
        Query::parse(&q, None).unwrap();
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
