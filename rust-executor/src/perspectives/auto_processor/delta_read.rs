//! Read only what a processor's scope query gained from the links added since
//! its kept transcript ([`TickReads`](super::watcher::TickReads)).
//!
//! The scope query is any SPARQL SELECT an app wrote. A row it gains from
//! added links uses at least one added quad, matched by one of the query's
//! triple patterns (semi-naive evaluation). For each triple pattern that an
//! added quad can match, [`delta_query`] copies the query's pattern with that
//! one triple pattern restricted, by a `VALUES` on one of its variables, to the
//! terms the added quads hold in that position, and runs the union of the
//! copies. So a turn arriving late, with a timestamp older than every kept
//! turn (a peer's link syncing in), or an old message that a new link brings
//! into scope (its flag or membership link landing after its body), is read
//! like any new turn, from the index, not from a scan of every turn.
//!
//! This holds only while the query is monotone: adding links can add rows but
//! never remove or change one. [`delta_query`] answers [`DeltaRead::Full`]
//! for anything else (`OPTIONAL`, `MINUS`, `EXISTS`, `GROUP BY`, `LIMIT`,
//! property paths, `GRAPH`, `SERVICE`, `FROM`, blank nodes, a triple pattern
//! with no variable that an added quad matches), and the caller reads
//! everything. A write that removed anything is never answered here: the
//! store's log ([`WriteLog`](crate::perspectives::write_log::WriteLog))
//! reports it, and the caller reads everything.

use crate::perspectives::sparql_store::is_link_metadata;
use crate::perspectives::write_log::AddedLink;
use oxigraph::model::Term;
use spargebra::algebra::{Expression, GraphPattern};
use spargebra::term::{GroundTerm, NamedNodePattern, TermPattern, TriplePattern};
use spargebra::{Query, SparqlParser};

/// How to bring a kept transcript up to date with the links added since.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DeltaRead {
    /// No added link can match the query: the kept rows are the answer.
    Unchanged,
    /// Run this SELECT: its rows are the rows the query gained (and possibly
    /// some it already had).
    Query(String),
    /// The query is not one this can follow: read it whole.
    Full,
}

/// The order a kept transcript holds its turns in, so a merge puts each
/// gained turn where a fresh read puts it ([`transcript_order`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TranscriptOrder {
    /// The query has no `ORDER BY`: SPARQL gives its rows no order, so a
    /// fresh read's order is the engine's, and gained turns are appended.
    Unordered,
    /// `ORDER BY ?timestamp` (or `DESC(?timestamp)`): turns are kept sorted
    /// by timestamp, then by turn id, a total order. A fresh read and a merge
    /// both sort the same turns by it, so they agree turn for turn. It is
    /// the query's own order wherever the query's is defined: timestamps are
    /// stored as plain literals, which SPARQL orders by code point, as Rust
    /// orders `str`; ties, which SPARQL leaves in no order, go by turn id.
    Timestamp { descending: bool },
}

/// The order `scope_query`'s turns are kept in. `None` for an `ORDER BY` the
/// kept transcript cannot reproduce (any key but `?timestamp` alone: a
/// turn kept without its text has no `?text` or `?speaker` to sort by); such
/// a query is always read whole ([`delta_query`] answers [`DeltaRead::Full`]).
pub(crate) fn transcript_order(scope_query: &str) -> Option<TranscriptOrder> {
    let Ok(Query::Select { pattern, .. }) = SparqlParser::new().parse_query(scope_query) else {
        return Some(TranscriptOrder::Unordered);
    };
    order_of(&pattern)
}

fn order_of(pattern: &GraphPattern) -> Option<TranscriptOrder> {
    use spargebra::algebra::OrderExpression;
    match pattern {
        GraphPattern::Project { inner, .. }
        | GraphPattern::Distinct { inner }
        | GraphPattern::Reduced { inner }
        | GraphPattern::Slice { inner, .. } => order_of(inner),
        GraphPattern::OrderBy { expression, .. } => match expression.as_slice() {
            [OrderExpression::Asc(Expression::Variable(v))] if v.as_str() == "timestamp" => {
                Some(TranscriptOrder::Timestamp { descending: false })
            }
            [OrderExpression::Desc(Expression::Variable(v))] if v.as_str() == "timestamp" => {
                Some(TranscriptOrder::Timestamp { descending: true })
            }
            _ => None,
        },
        _ => Some(TranscriptOrder::Unordered),
    }
}

/// The read that brings `scope_query`'s kept result up to date after `added`.
pub(crate) fn delta_query(scope_query: &str, added: &[AddedLink]) -> DeltaRead {
    if added.is_empty() {
        return DeltaRead::Unchanged;
    }
    // A merge must put each gained turn where a fresh read would.
    if transcript_order(scope_query).is_none() {
        return DeltaRead::Full;
    }
    let Ok(Query::Select {
        dataset: None,
        pattern,
        base_iri,
    }) = SparqlParser::new().parse_query(scope_query)
    else {
        return DeltaRead::Full;
    };
    // Keep the solution modifiers (projection, order, DISTINCT) over the
    // union of the restricted copies, so the gained rows come in the
    // query's own order.
    let mut modifiers = Vec::new();
    let mut next = pattern;
    let core = loop {
        next = match next {
            GraphPattern::Project { inner, variables } => {
                modifiers.push(Modifier::Project(variables));
                *inner
            }
            GraphPattern::OrderBy { inner, expression } => {
                modifiers.push(Modifier::OrderBy(expression));
                *inner
            }
            GraphPattern::Distinct { inner } | GraphPattern::Reduced { inner } => *inner,
            other => break other,
        };
    };
    let Some(copies) = restricted_copies(&core, added) else {
        return DeltaRead::Full;
    };
    let Some(union) = copies
        .into_iter()
        .reduce(|left, right| GraphPattern::Union {
            left: Box::new(left),
            right: Box::new(right),
        })
    else {
        return DeltaRead::Unchanged;
    };
    // One added link can match several of the query's triple patterns: the
    // same row then comes from several copies.
    let mut pattern = union;
    for modifier in modifiers.into_iter().rev() {
        pattern = match modifier {
            Modifier::Project(variables) => GraphPattern::Project {
                inner: Box::new(pattern),
                variables,
            },
            Modifier::OrderBy(expression) => GraphPattern::OrderBy {
                inner: Box::new(pattern),
                expression,
            },
        };
    }
    let pattern = GraphPattern::Distinct {
        inner: Box::new(pattern),
    };
    DeltaRead::Query(
        Query::Select {
            dataset: None,
            pattern,
            base_iri,
        }
        .to_string(),
    )
}

enum Modifier {
    Project(Vec<spargebra::term::Variable>),
    OrderBy(Vec<spargebra::algebra::OrderExpression>),
}

/// Copies of `pattern`, each with one triple pattern restricted to what
/// `added` can match there. `None` when `pattern` is not monotone, or a
/// triple pattern an added quad may match cannot be restricted.
fn restricted_copies(pattern: &GraphPattern, added: &[AddedLink]) -> Option<Vec<GraphPattern>> {
    Some(match pattern {
        GraphPattern::Bgp { patterns } => {
            let mut copies = Vec::new();
            for triple in patterns {
                let Some((variable, values)) = restriction(triple, added)? else {
                    continue;
                };
                copies.push(GraphPattern::Join {
                    left: Box::new(GraphPattern::Values {
                        variables: vec![variable],
                        bindings: values.into_iter().map(|v| vec![Some(v)]).collect(),
                    }),
                    right: Box::new(GraphPattern::Bgp {
                        patterns: patterns.clone(),
                    }),
                });
            }
            copies
        }
        // A row of a join uses an added quad on one side or the other.
        GraphPattern::Join { left, right } => {
            let mut copies = Vec::new();
            for l in restricted_copies(left, added)? {
                copies.push(GraphPattern::Join {
                    left: Box::new(l),
                    right: right.clone(),
                });
            }
            for r in restricted_copies(right, added)? {
                copies.push(GraphPattern::Join {
                    left: left.clone(),
                    right: Box::new(r),
                });
            }
            copies
        }
        // A row of a union comes from one branch: a copy keeps only the
        // branch it restricts, so the other branch is not read whole.
        GraphPattern::Union { left, right } => {
            let mut copies = restricted_copies(left, added)?;
            copies.extend(restricted_copies(right, added)?);
            copies
        }
        GraphPattern::Filter { expr, inner } => {
            if has_exists(expr) {
                return None;
            }
            restricted_copies(inner, added)?
                .into_iter()
                .map(|inner| GraphPattern::Filter {
                    expr: expr.clone(),
                    inner: Box::new(inner),
                })
                .collect()
        }
        GraphPattern::Extend {
            inner,
            variable,
            expression,
        } => {
            if has_exists(expression) {
                return None;
            }
            restricted_copies(inner, added)?
                .into_iter()
                .map(|inner| GraphPattern::Extend {
                    inner: Box::new(inner),
                    variable: variable.clone(),
                    expression: expression.clone(),
                })
                .collect()
        }
        // A constant table gains no rows.
        GraphPattern::Values { .. } => Vec::new(),
        // A sub-select's modifiers, kept on each copy.
        GraphPattern::Project { inner, variables } => restricted_copies(inner, added)?
            .into_iter()
            .map(|inner| GraphPattern::Project {
                inner: Box::new(inner),
                variables: variables.clone(),
            })
            .collect(),
        GraphPattern::Distinct { inner } | GraphPattern::Reduced { inner } => {
            restricted_copies(inner, added)?
        }
        GraphPattern::OrderBy { inner, .. } => restricted_copies(inner, added)?,
        // Not monotone (OPTIONAL, MINUS, aggregates, LIMIT), or not a pattern
        // of the default graph's quads this can restrict (paths, GRAPH,
        // SERVICE, LATERAL).
        _ => return None,
    })
}

/// The variable of `triple` to restrict, and the terms the added quads hold
/// there, when an added quad may match `triple`. `Some(None)`: no added quad
/// matches it. `None`: one may, and it cannot be restricted.
///
/// The store writes two kinds of quad per link: its direct triple
/// `source predicate target`, and its reifier's facts `reifier p v`, every `p`
/// in the store's own vocabulary ([`is_link_metadata`]), which no link's
/// predicate is.
fn restriction(
    triple: &TriplePattern,
    added: &[AddedLink],
) -> Option<Option<(spargebra::term::Variable, Vec<GroundTerm>)>> {
    let predicate = match &triple.predicate {
        NamedNodePattern::NamedNode(p) => Some(p),
        NamedNodePattern::Variable(_) => None,
    };
    let reifier_fact = predicate.is_none_or(|p| is_link_metadata(p.as_str()));
    let direct = |link: &&AddedLink| predicate.is_none_or(|p| *p == link.predicate);
    match &triple.subject {
        TermPattern::Variable(subject) => {
            let mut values = Vec::new();
            for link in added.iter().filter(direct) {
                values.push(GroundTerm::NamedNode(link.source.clone()));
            }
            if reifier_fact {
                for link in added {
                    values.push(GroundTerm::NamedNode(link.reifier.clone()));
                }
            }
            Some((!values.is_empty()).then(|| (subject.clone(), dedup(values))))
        }
        TermPattern::NamedNode(subject) => {
            // A reifier named in the query: its facts are not followed here.
            if added.iter().any(|l| l.reifier == *subject) {
                return None;
            }
            let matching: Vec<&AddedLink> = added
                .iter()
                .filter(direct)
                .filter(|l| l.source == *subject)
                .collect();
            if matching.is_empty() {
                return Some(None);
            }
            match (&triple.object, &triple.predicate) {
                (TermPattern::Variable(object), _) => {
                    let values = ground_terms(matching.iter().map(|l| &l.target))?;
                    Some(Some((object.clone(), values)))
                }
                (object, NamedNodePattern::Variable(p)) if is_ground(object) => {
                    let values = matching
                        .iter()
                        .filter(|l| term_matches(object, &l.target))
                        .map(|l| GroundTerm::NamedNode(l.predicate.clone()))
                        .collect::<Vec<_>>();
                    Some((!values.is_empty()).then(|| (p.clone(), dedup(values))))
                }
                (object, NamedNodePattern::NamedNode(_)) if is_ground(object) => {
                    // No variable: matched or not, nothing to restrict.
                    if matching.iter().any(|l| term_matches(object, &l.target)) {
                        None
                    } else {
                        Some(None)
                    }
                }
                _ => None,
            }
        }
        // The store holds no quad with a literal subject.
        TermPattern::Literal(_) => Some(None),
        // Blank nodes act as variables that cannot be bound by VALUES.
        _ => None,
    }
}

fn is_ground(term: &TermPattern) -> bool {
    matches!(term, TermPattern::NamedNode(_) | TermPattern::Literal(_))
}

fn term_matches(pattern: &TermPattern, term: &Term) -> bool {
    match (pattern, term) {
        (TermPattern::NamedNode(p), Term::NamedNode(t)) => p == t,
        (TermPattern::Literal(p), Term::Literal(t)) => p == t,
        _ => false,
    }
}

fn ground_terms<'a>(terms: impl Iterator<Item = &'a Term>) -> Option<Vec<GroundTerm>> {
    let mut out = Vec::new();
    for term in terms {
        out.push(match term {
            Term::NamedNode(n) => GroundTerm::NamedNode(n.clone()),
            Term::Literal(l) => GroundTerm::Literal(l.clone()),
            _ => return None,
        });
    }
    Some(dedup(out))
}

fn dedup(mut values: Vec<GroundTerm>) -> Vec<GroundTerm> {
    values.sort_by_key(|v| v.to_string());
    values.dedup();
    values
}

/// Whether `expr` holds an `EXISTS` (or `NOT EXISTS`): its pattern can gain
/// or lose a match from an added quad that no outer triple pattern matches.
fn has_exists(expr: &Expression) -> bool {
    match expr {
        Expression::Exists(_) => true,
        Expression::Or(a, b)
        | Expression::And(a, b)
        | Expression::Equal(a, b)
        | Expression::SameTerm(a, b)
        | Expression::Greater(a, b)
        | Expression::GreaterOrEqual(a, b)
        | Expression::Less(a, b)
        | Expression::LessOrEqual(a, b)
        | Expression::Add(a, b)
        | Expression::Subtract(a, b)
        | Expression::Multiply(a, b)
        | Expression::Divide(a, b) => has_exists(a) || has_exists(b),
        Expression::UnaryPlus(a) | Expression::UnaryMinus(a) | Expression::Not(a) => has_exists(a),
        Expression::In(a, list) => has_exists(a) || list.iter().any(has_exists),
        Expression::If(a, b, c) => has_exists(a) || has_exists(b) || has_exists(c),
        Expression::Coalesce(list) | Expression::FunctionCall(_, list) => {
            list.iter().any(has_exists)
        }
        Expression::NamedNode(_)
        | Expression::Literal(_)
        | Expression::Variable(_)
        | Expression::Bound(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
    use oxigraph::model::{Literal, NamedNode};

    fn added(source: &str, predicate: &str, target: Term) -> AddedLink {
        AddedLink {
            source: NamedNode::new_unchecked(source),
            predicate: NamedNode::new_unchecked(predicate),
            target,
            reifier: NamedNode::new_unchecked(format!("link://{source}/{predicate}")),
        }
    }

    fn body(source: &str) -> AddedLink {
        added(
            source,
            "ns://body",
            Term::Literal(Literal::new_simple_literal("hi")),
        )
    }

    #[test]
    fn a_body_link_is_read_by_its_terms() {
        let DeltaRead::Query(q) =
            delta_query(BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY, &[body("msg://1")])
        else {
            panic!("a delta read");
        };
        assert!(q.contains("VALUES"), "{q}");
        assert!(q.contains("<msg://1>"), "{q}");
        assert!(q.contains("<link://msg://1/ns://body>"), "{q}");
        assert!(q.contains("ORDER BY"), "the query's order is kept: {q}");
        crate::perspectives::sparql_store::validate_readonly_query(&q).expect("valid SPARQL");
    }

    #[test]
    fn no_added_link_reads_nothing() {
        assert_eq!(
            delta_query(BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY, &[]),
            DeltaRead::Unchanged
        );
        // No triple pattern of this query matches an `ns://other` link or a
        // reifier's facts.
        let q = "SELECT ?s ?o WHERE { ?s <ns://a> ?o . <ns://c> <ns://b> ?s }";
        assert_eq!(
            delta_query(
                q,
                &[added(
                    "msg://1",
                    "ns://other",
                    Term::Literal(Literal::new_simple_literal("x"))
                )]
            ),
            DeltaRead::Unchanged
        );
    }

    #[test]
    fn a_query_a_delta_cannot_follow_is_read_whole() {
        let link = [body("msg://1")];
        for q in [
            "SELECT ?m ?t WHERE { ?m <ns://body> ?t OPTIONAL { ?m <ns://x> ?y } }",
            "SELECT ?m ?t WHERE { ?m <ns://body> ?t } LIMIT 10",
            "SELECT ?m ?t WHERE { ?m <ns://body> ?t FILTER NOT EXISTS { ?m <ns://hidden> true } }",
            "SELECT ?m ?t WHERE { ?m <ns://body> ?t MINUS { ?m <ns://hidden> ?h } }",
            "SELECT ?m (COUNT(?t) AS ?n) WHERE { ?m <ns://body> ?t } GROUP BY ?m",
            "SELECT ?m ?t WHERE { ?m <ns://body>/<ns://x>* ?t }",
            "SELECT ?t WHERE { _:b <ns://body> ?t }",
            "SELECT ?t WHERE { GRAPH ?g { ?m <ns://body> ?t } }",
            "SELECT ?t WHERE { <msg://1> <ns://body> \"hi\" . ?m <ns://x> ?t }",
            "ASK { ?m <ns://body> ?t }",
            "not sparql",
        ] {
            assert_eq!(delta_query(q, &link), DeltaRead::Full, "{q}");
        }
    }

    #[test]
    fn a_union_branch_is_read_alone() {
        let q = "SELECT ?m WHERE { { <ns://chan> <ns://child> ?m } UNION { ?m <ns://other> ?x } }";
        let DeltaRead::Query(read) = delta_query(
            q,
            &[added(
                "ns://chan",
                "ns://child",
                Term::NamedNode(NamedNode::new_unchecked("msg://9")),
            )],
        ) else {
            panic!("a delta read");
        };
        assert!(
            !read.contains("ns://other"),
            "the branch no added link matches is not read: {read}"
        );
        assert!(read.contains("<msg://9>"), "{read}");
    }

    #[test]
    fn only_an_order_by_timestamp_is_kept_and_any_other_order_reads_whole() {
        assert_eq!(
            transcript_order(BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY),
            Some(TranscriptOrder::Timestamp { descending: false })
        );
        assert_eq!(
            transcript_order("SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text } ORDER BY DESC(?timestamp)"),
            Some(TranscriptOrder::Timestamp { descending: true })
        );
        assert_eq!(
            transcript_order("SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text }"),
            Some(TranscriptOrder::Unordered)
        );
        for q in [
            "SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text } ORDER BY ?speaker",
            "SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text } ORDER BY ?timestamp ?text",
            "SELECT ?speaker ?text ?timestamp WHERE { ?m <ns://body> ?text } ORDER BY STR(?timestamp)",
        ] {
            assert_eq!(transcript_order(q), None, "{q}");
            assert_eq!(delta_query(q, &[body("msg://1")]), DeltaRead::Full, "{q}");
        }
    }
}
