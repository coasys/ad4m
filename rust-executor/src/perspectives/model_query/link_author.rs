//! `where: { author }` as a condition on the links a clause matches (#1114).
//!
//! # What `author` means in a where clause
//!
//! Hydration gives every instance one synthetic `author`: the author of its
//! **earliest** link. Before this module, a where-clause `author` was matched
//! against that field after hydration. A role query
//! `{ agent: <candidate>, author: <admin> }` therefore asked "did admin write
//! this instance's first link?", not "did admin write the `agent` link?". Admin
//! creates a role instance, anyone adds `agent -> themselves`, and the query
//! accepts them. That fails open, which is exactly what role gating must not do
//! (#1046 §1).
//!
//! The rule now:
//!
//! - **Beside a property condition**, `author` applies to the links that satisfy
//!   the condition. `{ agent: X, author: A }` matches when there is an
//!   `agent -> X` link written by `A`. It is pushed into SPARQL by joining that
//!   link's reifier ([`link_author_join`]). The author filter runs before any
//!   last-write-wins reading of the value, which is the ordering #997 requires.
//! - **Scope follows the clause tree.** An `author` scopes every link-backed
//!   condition in its own clause and in nested `AND`/`OR`/`NOT` sub-clauses. An
//!   `author` inside a sub-clause also scopes the property conditions of the
//!   clauses enclosing it. So `{ agent: X, OR: [{ author: A }, { author: B }] }`
//!   (the shape the flow translator emits for `or` branches) means
//!   `OR: [{ agent: X, author: A }, { agent: X, author: B }]`.
//! - **A bare `author`**, with no link-backed condition in reach, has no link to
//!   scope. It keeps its instance-level meaning and is still matched after
//!   hydration, against the same `author` the instance JSON shows.
//!
//! A scoped `author` can only be answered in the store, where each link still
//! carries its own author. If any part of the clause has to be evaluated after
//! hydration, that fallback would compare against the earliest link's author
//! again. [`refuse_unanswerable_link_author`] rejects such a query instead of
//! answering it wrongly.
//!
//! The instance JSON is unchanged. Only the meaning of the condition changed.

use super::sparql_builder::compile_where_clause;
use super::types::{ModelQueryInput, ModelShape, ShapeResolver, WhereCondition};
use super::utils::escape_sparql_string;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::collections::BTreeMap;

const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
const ONT_AUTHOR: &str = "ad4m://ontology/author";

/// The author scope a clause is compiled under.
///
/// Threaded down the clause tree by `compile_where_clause_seq`. `'a` borrows
/// from the where clause being compiled.
#[derive(Clone, Default)]
pub(super) struct LinkAuthorScope<'a> {
    /// Author conditions every link-backed condition in this clause must meet.
    /// More than one when clauses nest, and each must hold.
    pub(super) authors: Vec<&'a WhereCondition>,
    /// Link-backed conditions of the enclosing conjunctive clauses. A
    /// sub-clause that brings its own `author` re-emits them under that author.
    pub(super) enclosing_leaves: Vec<(&'a str, &'a WhereCondition)>,
}

/// The clause's `author` condition, if it is link metadata.
///
/// A class that declares a property named `author` gets an ordinary property
/// condition instead. `author` is reserved at SHACL registration, so only
/// hand-built shapes can do that.
pub(super) fn link_author_condition<'a>(
    wc: &'a BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> Option<&'a WhereCondition> {
    if shape.properties.iter().any(|p| p.name == "author") {
        return None;
    }
    wc.get("author")
}

/// Whether `name` is a condition on a link: a property or relation of the
/// shape with a predicate. Getter-backed properties have none, and `id`,
/// `timestamp` and the like are not properties.
pub(super) fn is_link_leaf(name: &str, shape: &ModelShape) -> bool {
    !matches!(name, "AND" | "OR" | "NOT")
        && shape
            .properties
            .iter()
            .any(|p| p.name == name && !p.predicate.is_empty())
}

/// The link-backed conditions written directly in this clause.
pub(super) fn link_leaves<'a>(
    wc: &'a BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> Vec<(&'a str, &'a WhereCondition)> {
    wc.iter()
        .filter(|(name, _)| is_link_leaf(name, shape))
        .map(|(name, cond)| (name.as_str(), cond))
        .collect()
}

/// Whether this clause, or any sub-clause of it, has a link-backed condition.
pub(super) fn has_link_leaf(wc: &BTreeMap<String, WhereCondition>, shape: &ModelShape) -> bool {
    wc.iter().any(|(name, cond)| match (name.as_str(), cond) {
        ("AND" | "OR", WhereCondition::SubClauses(branches)) => {
            branches.iter().any(|b| has_link_leaf(b, shape))
        }
        ("NOT", WhereCondition::SubClause(branch)) => has_link_leaf(branch, shape),
        _ => is_link_leaf(name, shape),
    })
}

/// A SPARQL boolean expression testing `var` against one author condition.
///
/// Covers the shapes an author condition takes in practice: a DID, a list of
/// DIDs (`in`), and `not`/`contains`. Numeric operators on a DID mean nothing,
/// so they return `None` and the clause is declined.
fn author_expr(var: &str, cond: &WhereCondition) -> Option<String> {
    let quoted = |s: &str| format!("\"{}\"", escape_sparql_string(s));
    let list = |items: &[String]| items.iter().map(|s| quoted(s)).collect::<Vec<_>>();
    match cond {
        WhereCondition::String(did) => Some(format!("STR({var}) = {}", quoted(did))),
        WhereCondition::StringArray(dids) if dids.is_empty() => Some("false".to_string()),
        WhereCondition::StringArray(dids) => {
            Some(format!("STR({var}) IN ({})", list(dids).join(", ")))
        }
        WhereCondition::Ops(ops) => {
            let other = ops.between.is_some()
                || ops.lt.is_some()
                || ops.lte.is_some()
                || ops.gt.is_some()
                || ops.gte.is_some()
                || ops.some.is_some()
                || ops.none.is_some();
            if other || (ops.not.is_none() && ops.contains.is_none()) {
                return None;
            }
            let mut parts = Vec::new();
            match &ops.not {
                None => {}
                Some(Value::String(did)) => parts.push(format!("STR({var}) != {}", quoted(did))),
                Some(Value::Array(items)) if !items.is_empty() => {
                    let dids: Option<Vec<String>> = items
                        .iter()
                        .map(|v| v.as_str().map(str::to_string))
                        .collect();
                    parts.push(format!("!(STR({var}) IN ({}))", list(&dids?).join(", ")));
                }
                Some(_) => return None,
            }
            match &ops.contains {
                None => {}
                Some(Value::String(needle)) => parts.push(format!(
                    "CONTAINS(LCASE(STR({var})), LCASE({}))",
                    quoted(needle)
                )),
                Some(_) => return None,
            }
            Some(parts.join(" && "))
        }
        _ => None,
    }
}

/// Require the link `subject <predicate> object` to be written by an author
/// meeting every condition in `authors`.
///
/// Joins the link's reifier, the same `rdf:reifies` + `ad4m://ontology/author`
/// pair the instance query reads authors from. `object` is the term or
/// variable the caller's own triple pattern used, so the join names the very
/// link that satisfied the value condition, not merely some link on the
/// predicate. `tag` must be unique within the query; the variables start with
/// `?_` so that a relation quantifier's rebase namespaces them.
///
/// `Ok(None)` when there is no author to check. `Err(())` when an author
/// condition cannot be rendered, and the leaf must be declined.
pub(super) fn link_author_join(
    authors: &[&WhereCondition],
    subject: &str,
    predicate: &str,
    object: &str,
    tag: &str,
) -> Result<Option<String>, ()> {
    if authors.is_empty() {
        return Ok(None);
    }
    let reifier = format!("?_la{tag}");
    let author = format!("?_la{tag}_a");
    let tests = authors
        .iter()
        .map(|cond| author_expr(&author, cond))
        .collect::<Option<Vec<_>>>()
        .ok_or(())?;
    Ok(Some(format!(
        "    {reifier} <{RDF_REIFIES}> <<( {subject} <{predicate}> {object} )>> .\n    {reifier} <{ONT_AUTHOR}> {author} .\n    FILTER({})",
        tests.join(" && ")
    )))
}

/// Refuse a query whose link-scoped `author` would be evaluated after hydration.
///
/// After hydration an instance has one `author`, its earliest link's, so a
/// per-link condition cannot be answered there. Letting it fall through would
/// bring back the laundering this module removes. A clause is answered in the
/// store only when it compiles in full, so that is the test.
pub(super) fn refuse_unanswerable_link_author(
    query: &ModelQueryInput,
    shape: &ModelShape,
    resolver: &dyn ShapeResolver,
) -> Result<(), Error> {
    let Some(ref wc) = query.where_clause else {
        return Ok(());
    };
    let compiled = compile_where_clause(wc, shape, Some(resolver));
    if compiled.link_author_scoped && !compiled.complete {
        return Err(anyhow!(
            "where: an `author` condition beside property conditions applies to the links \
             those conditions match, and is answered in the store. Part of this clause can \
             only be evaluated after hydration (for example `timestamp`, a getter property, \
             a relation quantifier or an operator the store cannot express), where an \
             instance has a single author (its earliest link's) and the per-link answer is \
             gone. Refusing rather than matching against the wrong author."
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::types::WhereOps;
    use serde_json::json;

    #[test]
    fn author_expr_renders_each_supported_shape() {
        let s = WhereCondition::String("did:a".into());
        assert_eq!(author_expr("?v", &s).unwrap(), r#"STR(?v) = "did:a""#);

        let arr = WhereCondition::StringArray(vec!["did:a".into(), "did:b".into()]);
        assert_eq!(
            author_expr("?v", &arr).unwrap(),
            r#"STR(?v) IN ("did:a", "did:b")"#
        );
        assert_eq!(
            author_expr("?v", &WhereCondition::StringArray(vec![])).unwrap(),
            "false"
        );

        let not = WhereCondition::Ops(WhereOps {
            not: Some(json!(["did:a"])),
            ..Default::default()
        });
        assert_eq!(
            author_expr("?v", &not).unwrap(),
            r#"!(STR(?v) IN ("did:a"))"#
        );
    }

    #[test]
    fn author_expr_declines_what_it_cannot_express() {
        let gt = WhereCondition::Ops(WhereOps {
            gt: Some(1.0),
            ..Default::default()
        });
        assert!(author_expr("?v", &gt).is_none());
        let empty_not = WhereCondition::Ops(WhereOps {
            not: Some(json!([])),
            ..Default::default()
        });
        assert!(author_expr("?v", &empty_not).is_none());
        assert!(author_expr("?v", &WhereCondition::Number(1.0)).is_none());
    }

    #[test]
    fn a_did_is_escaped_inside_the_filter() {
        let join = link_author_join(
            &[&WhereCondition::String("did:\"x".into())],
            "?source",
            "ns://p",
            "?o",
            "0",
        )
        .unwrap()
        .unwrap();
        assert!(join.contains(r#"STR(?_la0_a) = "did:\"x""#), "{join}");
        assert!(join.contains("<<( ?source <ns://p> ?o )>>"), "{join}");
    }
}
