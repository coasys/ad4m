//! `author` in a where clause: per link, or per instance (#1114).
//!
//! # Why
//!
//! Hydration gives every instance one synthetic `author`: the author of its
//! **earliest** link. A role query `{ agent: <candidate>, author: <admin> }`
//! matched against that field asks "did admin write this instance's first
//! link?", not "did admin write the `agent` link?". Admin creates a role
//! instance, anyone adds `agent -> themselves`, and the query accepts them. That
//! fails open, which is exactly what role gating must not do (#1046 §1).
//!
//! # The three forms
//!
//! `author` is scoped by **nesting** it under the property it applies to.
//!
//! | Form | Example | Meaning |
//! |---|---|---|
//! | bare | `{ author: A }` | the instance's author (its earliest link's) is A |
//! | nested | `{ agent: { eq: X, author: A } }` | A wrote an `agent -> X` link |
//! | side-by-side | `{ agent: X, author: A }` | both: the instance's author is A, and A wrote an `agent -> X` link |
//!
//! - **Nested** (per link). A property or relation condition's operator
//!   object takes `author` beside its value operators: `eq`, `not`,
//!   `contains`, `gt`/`lt`…, or a relation quantifier. The author and the value
//!   condition must hold on the **same link**, which is pushed into SPARQL by
//!   joining that triple's reifier ([`link_author_join`]). With no value
//!   operator, `{ agent: { author: A } }` means A wrote some `agent` link,
//!   whatever its value. Beside `some`, A wrote a link to a record satisfying
//!   the nested clause; beside `none`, A wrote no such link.
//! - **Bare** (per instance). A top-level `author` with no link-backed
//!   condition beside it in the same object keeps its old meaning and is
//!   matched after hydration, against the `author` the instance JSON shows.
//!   Getter-backed properties and `timestamp` have no link, so beside only
//!   those the `author` is bare too.
//! - **Side-by-side** (both). A top-level `author` A beside link-backed
//!   conditions in the same object is sugar for
//!   `{ author: A, agent: { eq: X, author: A } }`: the instance author is A,
//!   and every link-backed sibling is satisfied by a link A wrote. Each half is
//!   a conjunct, so the result is narrower than either reading alone and fails
//!   closed. For a single-author instance it is the old answer. The
//!   instance-level half is pushed too ([`instance_author_filter`]), since the
//!   per-link half forbids the post-hydration fallback. `author: { not: A }`
//!   negates both halves: the instance author is not A, and each sibling is
//!   satisfied by a link someone other than A wrote. A `none` quantifier
//!   sibling is not scoped: "A wrote no such link" is wider than "no such
//!   link", and the sugar must only ever narrow.
//!
//! Scope never crosses an object boundary. An `author` inside an
//! `AND`/`OR`/`NOT` sub-clause, or inside a quantifier's nested clause, applies
//! to the siblings in its own object by the same three rules, and an `author`
//! outside a sub-clause does not reach into it.
//!
//! Negation is explicit and each spelling means one thing:
//! `NOT: { agent: { eq: X, author: A } }` is "A wrote no `agent -> X` link",
//! while `agent: { eq: X, author: { not: A } }` is "someone other than A wrote
//! an `agent -> X` link". An instance where both A and B wrote it fails the
//! first and passes the second.
//!
//! # Refusals
//!
//! A per-link author (nested, or the per-link half of side-by-side) can only
//! be answered in the store, where every link still carries its own author.
//! After hydration an instance has one `author`, so if any part of the query
//! has to be evaluated there, [`refuse_unanswerable_link_author`] returns an
//! `Err` instead of answering against the wrong author. The flag is carried
//! up out of relation quantifiers' nested clauses, so the refusal does not
//! depend on the post-hydration filter failing closed on quantifiers.
//!
//! `author` nested under a condition with no link (a getter property,
//! `timestamp`, `id`) is an `Err` too, as are `eq` combined with another value
//! operator and an author value that is not a DID, a DID array, `{ not }` or
//! `{ contains }`.

use super::sparql_builder::{compile_where_clause, instance_link_predicates, local_predicates};
use super::types::{ModelQueryInput, ModelShape, ShapeResolver, WhereCondition, WhereOps};
use super::utils::escape_sparql_string;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::borrow::Cow;
use std::collections::BTreeMap;

const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
const ONT_AUTHOR: &str = "ad4m://ontology/author";
const ONT_TIMESTAMP: &str = "ad4m://ontology/timestamp";
const ONT_STATUS: &str = "ad4m://ontology/status";

/// What the where compiler learnt about `author` while compiling a clause.
///
/// Threaded through the compiler by `&mut`, the same way the variable counter
/// is, and merged up from a relation quantifier's nested clause, so the answer
/// covers every level of the query.
#[derive(Debug, Default, Clone)]
pub(super) struct LinkAuthorState {
    /// A per-link author condition was compiled somewhere in the query.
    pub(super) per_link: bool,
    /// The first malformed condition met: `author` where there is no link,
    /// `eq` beside another value operator, an unrenderable author value.
    pub(super) error: Option<String>,
}

impl LinkAuthorState {
    pub(super) fn fail(&mut self, message: String) {
        self.error.get_or_insert(message);
    }

    /// Fold in what a separately compiled nested clause found.
    pub(super) fn merge(&mut self, per_link: bool, error: Option<String>) {
        self.per_link |= per_link;
        if let Some(e) = error {
            self.fail(e);
        }
    }
}

/// The clause's top-level `author` condition, if it is link metadata.
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

/// The top-level `author` of this clause when it is side-by-side, i.e. has a
/// link-backed sibling in the same object. `None` for a bare one.
pub(super) fn side_by_side_author<'a>(
    wc: &'a BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> Option<&'a WhereCondition> {
    link_author_condition(wc, shape)
        .filter(|_| wc.keys().any(|k| k != "author" && is_link_leaf(k, shape)))
}

/// Whether a side-by-side `author` scopes this sibling. Every link-backed
/// sibling except a `none` quantifier, which scoping would widen.
pub(super) fn side_by_side_scopes(name: &str, cond: &WhereCondition, shape: &ModelShape) -> bool {
    is_link_leaf(name, shape) && !matches!(cond, WhereCondition::Ops(o) if o.none.is_some())
}

/// A leaf condition split into its value condition and its nested `author`.
///
/// `{ eq: X, author: A }` becomes `(X, A)`, `{ not: X, author: A }` becomes
/// `({ not: X }, A)`, `{ author: A }` becomes `(None, A)`: "some link, any
/// value". Anything without `eq`/`author` is returned as it is.
pub(super) fn split_leaf(
    cond: &WhereCondition,
) -> Result<(Option<Cow<'_, WhereCondition>>, Option<&WhereCondition>), String> {
    let WhereCondition::Ops(ops) = cond else {
        return Ok((Some(Cow::Borrowed(cond)), None));
    };
    if ops.eq.is_none() && ops.author.is_none() {
        return Ok((Some(Cow::Borrowed(cond)), None));
    }
    let author = ops.author.as_deref();
    if let Some(author) = author {
        if author_expr("?v", author).is_none() {
            return Err(
                "`author` must be a DID, an array of DIDs, `{ not: did | [dids] }` or \
                 `{ contains: text }`"
                    .to_string(),
            );
        }
    }
    let rest = WhereOps {
        eq: None,
        author: None,
        ..ops.clone()
    };
    let rest_has_ops = rest.not.is_some()
        || rest.between.is_some()
        || rest.lt.is_some()
        || rest.lte.is_some()
        || rest.gt.is_some()
        || rest.gte.is_some()
        || rest.contains.is_some()
        || rest.some.is_some()
        || rest.none.is_some();
    let value = match ops.eq.as_deref() {
        Some(_) if rest_has_ops => {
            return Err(
                "`eq` cannot be combined with another value operator; use one of them".to_string(),
            )
        }
        Some(
            eq @ (WhereCondition::Bool(_)
            | WhereCondition::Number(_)
            | WhereCondition::String(_)
            | WhereCondition::StringArray(_)
            | WhereCondition::NumberArray(_)),
        ) => Some(Cow::Borrowed(eq)),
        Some(_) => return Err("`eq` takes a value or an array of values".to_string()),
        None if rest_has_ops => Some(Cow::Owned(WhereCondition::Ops(rest))),
        None => None,
    };
    Ok((value, author))
}

/// Whether a clause may hold a per-link author, judged from its syntax alone:
/// a nested `author` anywhere, or a top-level `author` with a sibling. Used
/// where the clause cannot be compiled (no shape for a quantifier's target),
/// so the refusal errs towards firing.
pub(super) fn may_hold_link_author(wc: &BTreeMap<String, WhereCondition>) -> bool {
    fn cond_holds(c: &WhereCondition) -> bool {
        match c {
            WhereCondition::Ops(o) => {
                o.author.is_some()
                    || o.some.as_ref().is_some_and(may_hold_link_author)
                    || o.none.as_ref().is_some_and(may_hold_link_author)
            }
            WhereCondition::SubClauses(bs) => bs.iter().any(may_hold_link_author),
            WhereCondition::SubClause(b) => may_hold_link_author(b),
            _ => false,
        }
    }
    (wc.contains_key("author") && wc.len() > 1) || wc.values().any(cond_holds)
}

/// A SPARQL boolean expression testing `var` against one author condition.
///
/// Covers the shapes an author condition takes: a DID, a list of DIDs (`in`),
/// and `not`/`contains`. Anything else returns `None`.
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
                || ops.none.is_some()
                || ops.eq.is_some()
                || ops.author.is_some();
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

/// Require the instance's author, as hydration defines it, to meet `cond`.
///
/// The instance-level half of a side-by-side `author`. Hydration takes the
/// author of the earliest of the links it reads for an instance: those on the
/// shape's predicates ([`instance_link_predicates`]), with `local` predicates
/// restricted to `Local` links, each with an author and a timestamp.
/// Timestamps compare as strings, as hydration compares them.
///
/// When several links share the earliest timestamp hydration shows whichever
/// it met first, which the store does not fix. This requires **every** one of
/// them to meet `cond`, so a tie can only exclude.
///
/// `None` when `cond` cannot be rendered.
pub(super) fn instance_author_filter(
    shape: &ModelShape,
    cond: &WhereCondition,
    tag: &str,
) -> Option<String> {
    let predicates = instance_link_predicates(shape);
    let locals = local_predicates(shape);
    // One of the links hydration reads, as variables suffixed `n`.
    let link = |n: &str, indent: &str| {
        let p = format!("?_ia{tag}p{n}");
        let o = format!("?_ia{tag}o{n}");
        let r = format!("?_ia{tag}r{n}");
        let mut lines = Vec::new();
        if !predicates.is_empty() {
            let values = predicates
                .iter()
                .map(|p| format!("<{p}>"))
                .collect::<Vec<_>>()
                .join(" ");
            lines.push(format!("VALUES {p} {{ {values} }}"));
        }
        lines.push(format!("?source {p} {o} ."));
        lines.push(format!("{r} <{RDF_REIFIES}> <<( ?source {p} {o} )>> ."));
        lines.push(format!("FILTER(isIRI({p}))"));
        lines.push(format!("{r} <{ONT_AUTHOR}> ?_ia{tag}a{n} ."));
        lines.push(format!("{r} <{ONT_TIMESTAMP}> ?_ia{tag}t{n} ."));
        if !locals.is_empty() {
            let s = format!("?_ia{tag}s{n}");
            lines.push(format!("OPTIONAL {{ {r} <{ONT_STATUS}> {s} . }}"));
            lines.push(format!(
                "FILTER(!({p} IN ({})) || {s} = \"Local\")",
                locals.join(", ")
            ));
        }
        lines
            .iter()
            .map(|l| format!("{indent}{l}"))
            .collect::<Vec<_>>()
            .join("\n")
    };
    let failing = author_expr(&format!("?_ia{tag}a2"), cond)?;
    Some(format!(
        "    FILTER EXISTS {{\n{first}\n        FILTER NOT EXISTS {{\n{earlier}\n            FILTER(STR(?_ia{tag}t1) < STR(?_ia{tag}t0))\n        }}\n        FILTER NOT EXISTS {{\n{tied}\n            FILTER(STR(?_ia{tag}t2) = STR(?_ia{tag}t0) && !({failing}))\n        }}\n    }}",
        first = link("0", "        "),
        earlier = link("1", "            "),
        tied = link("2", "            "),
    ))
}

/// Refuse a query whose `author` cannot be answered correctly.
///
/// After hydration an instance has one `author`, its earliest link's, so a
/// per-link condition cannot be answered there. Letting it fall through would
/// bring back the laundering this module removes. A query is answered in the
/// store only when its where clause compiles in full, so that is the test.
/// Malformed `author`/`eq` conditions are refused here too, with the reason.
pub(super) fn refuse_unanswerable_link_author(
    query: &ModelQueryInput,
    shape: &ModelShape,
    resolver: &dyn ShapeResolver,
) -> Result<(), Error> {
    let Some(ref wc) = query.where_clause else {
        return Ok(());
    };
    let compiled = compile_where_clause(wc, shape, Some(resolver));
    if let Some(error) = compiled.link_author_error {
        return Err(anyhow!("where: {error}"));
    }
    if compiled.link_author_scoped && !compiled.complete {
        return Err(anyhow!(
            "where: a per-link `author` (nested under a property, or a top-level `author` \
             beside property conditions) is answered in the store. Part of this query can \
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
    use serde_json::json;

    fn cond(v: Value) -> WhereCondition {
        serde_json::from_value(v).unwrap()
    }

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
        assert!(author_expr("?v", &cond(json!({ "eq": "did:a" }))).is_none());
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

    #[test]
    fn split_leaf_separates_value_and_author() {
        let c = cond(json!({ "eq": "x", "author": "did:a" }));
        let (v, a) = split_leaf(&c).unwrap();
        assert!(matches!(v.as_deref(), Some(WhereCondition::String(s)) if s == "x"));
        assert!(matches!(a, Some(WhereCondition::String(s)) if s == "did:a"));

        let c = cond(json!({ "author": ["did:a", "did:b"] }));
        let (v, a) = split_leaf(&c).unwrap();
        assert!(v.is_none(), "author alone: any value");
        assert!(matches!(a, Some(WhereCondition::StringArray(_))));

        let c = cond(json!({ "not": "x", "author": "did:a" }));
        let (v, a) = split_leaf(&c).unwrap();
        match v.as_deref() {
            Some(WhereCondition::Ops(o)) => {
                assert_eq!(o.not, Some(json!("x")));
                assert!(o.author.is_none() && o.eq.is_none());
            }
            other => panic!("expected the remaining ops, got {other:?}"),
        }
        assert!(a.is_some());

        let c = cond(json!({ "eq": ["x", "y"] }));
        let (v, a) = split_leaf(&c).unwrap();
        assert!(matches!(v.as_deref(), Some(WhereCondition::StringArray(_))));
        assert!(a.is_none(), "`eq` alone is the bare value");

        let plain = cond(json!("x"));
        let (v, a) = split_leaf(&plain).unwrap();
        assert!(matches!(v, Some(Cow::Borrowed(_))) && a.is_none());
    }

    #[test]
    fn split_leaf_refuses_malformed_combinations() {
        for bad in [
            json!({ "eq": "x", "not": "y" }),
            json!({ "eq": "x", "some": {} }),
            json!({ "eq": { "gt": 1 } }),
            json!({ "author": 3 }),
            json!({ "author": { "gt": 1 } }),
            json!({ "eq": "x", "author": { "eq": "did:a" } }),
        ] {
            assert!(split_leaf(&cond(bad.clone())).is_err(), "{bad}");
        }
    }
}
