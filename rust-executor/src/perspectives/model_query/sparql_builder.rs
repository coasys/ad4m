//! SPARQL query construction for the model query pipeline.
//!
//! This module generates the SPARQL strings that find conforming model
//! instances and apply where-clause filters.  It produces two kinds of output:
//!
//! - **Conformance patterns** — triple patterns that identify instances
//!   belonging to a particular model class (based on required/flag properties).
//! - **Where-clause patterns** — `FILTER` and `VALUES` clauses that push
//!   client-specified conditions into the SPARQL query for server-side
//!   evaluation.
//!
//! The main entry points are [`build_instance_sparql`] (full row query) and
//! [`build_count_sparql`] (lightweight `COUNT`).  Both delegate to
//! [`build_query_patterns`] for the shared conformance + where logic.

use serde_json::Value;

use std::collections::BTreeMap;

use super::types::{
    InstanceQueryPlan, ModelQueryInput, ModelShape, OrderDirection, Scope, SortKey,
    SparqlPagination, WhereCondition,
};
use super::utils::{
    escape_sparql_string, format_literal_number, looks_like_absolute_iri, validate_iri,
};

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";
const XSD_DOUBLE: &str = "http://www.w3.org/2001/XMLSchema#double";

/// Render a finite f64 as a typed-literal SPARQL term, mirroring the
/// `xsd:integer` vs `xsd:decimal` split used by the storage layer.
fn typed_number_literal(n: f64) -> Option<String> {
    let s = format_literal_number(n)?;
    let dt = if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
        XSD_INTEGER
    } else {
        XSD_DECIMAL
    };
    Some(format!("\"{s}\"^^<{dt}>"))
}

/// Build a targeted reifier timestamp probe for the pagination sub-query.
///
/// The timestamp probe finds the earliest reifier timestamp for each source
/// instance so that `ORDER BY ?_first_ts` can sort by creation time.  The
/// probe tries to use a specific conformance predicate (flag or required
/// property) for efficiency, falling back to a generic `?source ?_anyP ?_anyT`
/// pattern if no specific predicate is available.
pub(super) fn build_timestamp_probe(shape: &ModelShape) -> String {
    let rdf_reifies = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
    let ont_ts = "ad4m://ontology/timestamp";

    if let Some(prop) = shape.properties.iter().find(|p| {
        p.is_flag
            && p.initial_value
                .as_ref()
                .map(|v| validate_iri(v).is_ok())
                .unwrap_or(false)
            && validate_iri(&p.predicate).is_ok()
    }) {
        let initial = prop.initial_value.as_ref().unwrap();
        return format!(
            "?_r <{rdf_reifies}> <<( ?source <{}> <{initial}> )>> . ?_r <{ont_ts}> ?_first_ts .",
            prop.predicate
        );
    }

    if let Some(prop) = shape
        .properties
        .iter()
        .find(|p| p.is_required && !p.predicate.is_empty() && validate_iri(&p.predicate).is_ok())
    {
        let safe_name = prop.name.replace(|c: char| !c.is_alphanumeric(), "_");
        return format!(
            "?_r <{rdf_reifies}> <<( ?source <{}> ?cf_{safe_name} )>> . ?_r <{ont_ts}> ?_first_ts .",
            prop.predicate
        );
    }

    format!(
        "?source ?_anyP ?_anyT . ?_r <{rdf_reifies}> <<( ?source ?_anyP ?_anyT )>> . ?_r <{ont_ts}> ?_first_ts ."
    )
}

/// Build the SPARQL query (or two-phase plan) that retrieves instance rows.
///
/// Returns an [`InstanceQueryPlan`]:
/// - [`Single`](InstanceQueryPlan::Single) when no pagination is needed.
/// - [`TwoPhase`](InstanceQueryPlan::TwoPhase) when `sparql_pagination` is
///   provided, splitting the work into a lightweight pagination sub-query
///   and a `VALUES`-based property fetch.
pub(super) fn build_instance_sparql(
    shape: &ModelShape,
    query: &ModelQueryInput,
    sparql_pagination: Option<&SparqlPagination>,
) -> InstanceQueryPlan {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    let needed: Vec<&str> = shape
        .properties
        .iter()
        .filter(|p| !p.predicate.is_empty())
        .filter(|p| !(p.is_collection && p.getter.is_some()))
        .map(|p| p.predicate.as_str())
        .collect();

    let predicate_filter = if needed.is_empty() {
        String::new()
    } else {
        let unique: std::collections::BTreeSet<&str> = needed.into_iter().collect();
        let values: String = unique
            .iter()
            .map(|p| format!("<{p}>"))
            .collect::<Vec<_>>()
            .join(" ");
        format!("    VALUES ?predicate {{ {values} }}\n")
    };

    let pagination_suffix = if let Some(pg) = sparql_pagination {
        let mut suffix = String::new();
        match &pg.sort_key {
            SortKey::Timestamp => match pg.direction {
                OrderDirection::DESC => suffix.push_str("\n    ORDER BY DESC(?_first_ts)"),
                OrderDirection::ASC => suffix.push_str("\n    ORDER BY ASC(?_first_ts)"),
            },
            SortKey::Property(_) => {
                let dir = match pg.direction {
                    OrderDirection::DESC => "DESC",
                    OrderDirection::ASC => "ASC",
                };
                suffix.push_str(&format!(
                    "\n    ORDER BY ASC(IF(BOUND(?_sort_str), 0, 1)) {dir}(?_sort_num) {dir}(?_sort_str)"
                ));
            }
            SortKey::Projection(_) => {
                let dir = match pg.direction {
                    OrderDirection::DESC => "DESC",
                    OrderDirection::ASC => "ASC",
                };
                // COUNT returns 0 for sources with no matches (not unbound),
                // so no null-guard is needed here.
                suffix.push_str(&format!("\n    ORDER BY {dir}(?_proj_sort)"));
            }
            SortKey::RelationProperty { .. } => {
                let dir = match pg.direction {
                    OrderDirection::DESC => "DESC",
                    OrderDirection::ASC => "ASC",
                };
                // Same nulls-to-end + numeric-first logic as Property.
                suffix.push_str(&format!(
                    "\n    ORDER BY ASC(IF(BOUND(?_rp_str), 0, 1)) {dir}(?_rp_num) {dir}(?_rp_str)"
                ));
            }
        }
        if let Some(offset) = pg.offset {
            if offset > 0 {
                suffix.push_str(&format!("\n    OFFSET {offset}"));
            }
        }
        if let Some(limit) = pg.limit {
            suffix.push_str(&format!("\n    LIMIT {limit}"));
        }
        suffix
    } else {
        String::new()
    };

    if let Some(pg) = sparql_pagination {
        let subquery_body = match &pg.sort_key {
            SortKey::Timestamp => {
                let ts_probe = build_timestamp_probe(shape);
                format!(
                    r#"SELECT DISTINCT ?source ?_first_ts WHERE {{
{conformance}
{where_extra}
            {ts_probe}
        }}{pagination_suffix}"#
                )
            }
            SortKey::Property(predicate) => {
                // `parse_literal` yields the lexical form for a typed literal
                // and the inner `data` for a signed envelope — sorting a
                // `resolveLanguage:"literal"` property must compare on the
                // value, not on the envelope JSON (which would sort by author
                // and timestamp instead). Plain `STR()` would be enough for
                // deterministic literals alone, but not for envelopes, and the
                // ORDER BY sits inside a GROUP BY subquery over an already
                // filtered set, so there is no index to lose here.
                // The xsd:double cast yields the numeric sort key when the
                // value parses as a number.
                format!(
                    r#"SELECT DISTINCT ?source (SAMPLE(?_nv) AS ?_sort_num) (SAMPLE(?_sv) AS ?_sort_str) WHERE {{
{conformance}
{where_extra}
            OPTIONAL {{ ?source <{predicate}> ?_sort_raw . BIND(STR(<ad4m://fn/parse_literal>(?_sort_raw)) AS ?_sv) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR(<ad4m://fn/parse_literal>(?_sort_raw))) AS ?_nv) }}
        }} GROUP BY ?source{pagination_suffix}"#
                )
            }
            SortKey::Projection(predicate) => {
                format!(
                    r#"SELECT DISTINCT ?source (COUNT(DISTINCT ?_proj_t) AS ?_proj_sort) WHERE {{
{conformance}
{where_extra}
            OPTIONAL {{ ?source <{predicate}> ?_proj_t . }}
        }} GROUP BY ?source{pagination_suffix}"#
                )
            }
            SortKey::RelationProperty {
                rel_pred,
                prop_pred,
            } => {
                format!(
                    r#"SELECT DISTINCT ?source (SAMPLE(?_rp_num_v) AS ?_rp_num) (SAMPLE(?_rp_str_v) AS ?_rp_str) WHERE {{
{conformance}
{where_extra}
            OPTIONAL {{ ?source <{rel_pred}> ?_rp_rel . OPTIONAL {{ ?_rp_rel <{prop_pred}> ?_rp_raw . BIND(STR(<ad4m://fn/parse_literal>(?_rp_raw)) AS ?_rp_str_v) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR(<ad4m://fn/parse_literal>(?_rp_raw))) AS ?_rp_num_v) }} }}
        }} GROUP BY ?source{pagination_suffix}"#
                )
            }
        };
        InstanceQueryPlan::TwoPhase {
            pagination_subquery: subquery_body,
            predicate_filter,
        }
    } else {
        InstanceQueryPlan::Single(format!(
            r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
{conformance}
{where_extra}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
        ))
    }
}

/// Build a COUNT SPARQL query that returns the number of conforming instances.
pub(super) fn build_count_sparql(shape: &ModelShape, query: &ModelQueryInput) -> Option<String> {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    if conformance.trim().is_empty() && where_extra.trim().is_empty() {
        return None;
    }

    Some(format!(
        r#"SELECT (COUNT(DISTINCT ?source) AS ?cnt) WHERE {{
{conformance}
{where_extra}
}}"#
    ))
}

/// Check whether **all** where-clause conditions can be pushed into SPARQL.
///
/// Answered by *performing* the compilation and reporting whether it was
/// complete, so this cannot drift from what [`build_query_patterns`] actually
/// emits.
///
/// It used to be a second, hand-maintained judgement about which conditions the
/// emitter could handle, and the two disagreed. A scalar property was declared
/// pushable for any condition, while the emitter additionally required a
/// non-empty, IRI-valid predicate — so a `@Property({ getter })` field, which
/// carries no predicate, was reported pushable and then emitted nothing. The
/// caller skips post-hydration filtering when this returns `true`, so the
/// condition was applied in neither place and the query silently returned
/// unfiltered rows (and `count()` an unfiltered count).
///
/// When this returns `false`, post-hydration Rust-side filtering is required for
/// the remaining conditions, and SPARQL-level pagination must not be pushed.
pub(super) fn all_where_pushable(query: &ModelQueryInput, shape: &ModelShape) -> bool {
    match query.where_clause {
        None => true,
        Some(ref wc) => compile_where_clause(wc, shape).complete,
    }
}

/// Build the conformance and where-clause SPARQL pattern strings.
///
/// Returns `(conformance, where_extra)` — two SPARQL fragments that are
/// interpolated into both the instance query and the `COUNT` query.
///
/// **Conformance patterns** ensure only instances of the target model class
/// are matched.  They are derived from the shape's required and flag
/// properties, with multiple fallback tiers:
/// 1. Required properties → `?source <pred> ?cf_name .`
/// 2. Flag properties with initial values → `?source <pred> <initial> .`
/// 3. Any property with an initial value (first match)
/// 4. Structural fallback using known predicates via `FILTER(?_structPred IN (...))`
///
/// **Where patterns** translate the query's `where` clause into SPARQL
/// `FILTER`/`VALUES` expressions for server-side evaluation.
pub(super) fn build_query_patterns(
    shape: &ModelShape,
    query: &ModelQueryInput,
) -> (String, String) {
    let mut conformance_patterns = Vec::new();

    // Parent filter
    if let Some(ref parent) = query.parent {
        match parent {
            Scope::Raw { id, predicate } => {
                if let (Ok(safe_id), Ok(safe_pred)) = (validate_iri(id), validate_iri(predicate)) {
                    conformance_patterns.push(format!("    <{safe_id}> <{safe_pred}> ?source ."));
                } else {
                    log::warn!(
                        "Skipping parent scope: invalid IRI in id='{}' or predicate='{}'",
                        id,
                        predicate
                    );
                }
            }
            Scope::Model { id, field, model } => {
                let safe_id = match validate_iri(id) {
                    Ok(s) => s,
                    Err(_) => {
                        log::warn!("Skipping parent scope: invalid IRI in id='{}'", id);
                        return (String::new(), String::new());
                    }
                };
                if let Some(ref f) = field {
                    if let Ok(safe_f) = validate_iri(f) {
                        conformance_patterns.push(format!("    <{safe_id}> <{safe_f}> ?source ."));
                    } else {
                        log::warn!("Skipping parent scope: invalid IRI in field='{}'", f);
                    }
                } else {
                    let safe_model = escape_sparql_string(model);
                    let hash_model = format!("#{safe_model}");
                    conformance_patterns.push(format!("    <{safe_id}> ?_parentPred ?source ."));
                    conformance_patterns.push(format!(
                        "    FILTER(STRENDS(STR(?_parentPred), \"/{safe_model}\") || STRENDS(STR(?_parentPred), \"{hash_model}\"))",
                    ));
                }
            }
        }
    }

    // Conformance patterns from shape properties
    let mut has_conformance = false;
    for prop in &shape.properties {
        if prop.is_required {
            if validate_iri(&prop.predicate).is_err() {
                continue;
            }
            let safe_name = prop.name.replace(|c: char| !c.is_alphanumeric(), "_");
            has_conformance = true;
            if prop.is_flag {
                if let Some(ref initial) = prop.initial_value {
                    if validate_iri(initial).is_ok() {
                        conformance_patterns
                            .push(format!("    ?source <{}> <{initial}> .", prop.predicate));
                    } else {
                        let escaped = escape_sparql_string(initial);
                        conformance_patterns.push(format!(
                            "    ?source <{}> ?cf_{safe_name} . FILTER(STR(?cf_{safe_name}) = \"{escaped}\")",
                            prop.predicate
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cf_{safe_name} .",
                        prop.predicate
                    ));
                }
            } else {
                conformance_patterns.push(format!(
                    "    ?source <{}> ?cf_{safe_name} .",
                    prop.predicate
                ));
            }
        }
    }

    // Fallback: if no required properties, try initial values
    if !has_conformance {
        for prop in &shape.properties {
            if let Some(ref initial) = prop.initial_value {
                if validate_iri(&prop.predicate).is_err() {
                    continue;
                }
                let safe_name = prop.name.replace(|c: char| !c.is_alphanumeric(), "_");
                has_conformance = true;
                if prop.is_flag {
                    if validate_iri(initial).is_ok() {
                        conformance_patterns
                            .push(format!("    ?source <{}> <{initial}> .", prop.predicate));
                    } else {
                        let escaped = escape_sparql_string(initial);
                        conformance_patterns.push(format!(
                            "    ?source <{}> ?cfInit_{safe_name} . FILTER(STR(?cfInit_{safe_name}) = \"{escaped}\")",
                            prop.predicate
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cfInit_{safe_name} .",
                        prop.predicate
                    ));
                }
                break;
            }
        }
    }

    // Fallback: structural matching using known predicates.
    if !has_conformance && conformance_patterns.is_empty() {
        log::debug!(
            "Model class uses structural conformance fallback — no required/flag properties found. \
             This may match instances from other model classes sharing the same predicates."
        );
        let known_predicates: Vec<String> = shape
            .properties
            .iter()
            .filter(|p| !p.predicate.is_empty() && validate_iri(&p.predicate).is_ok())
            .map(|p| format!("<{}>", p.predicate))
            .collect();

        if !known_predicates.is_empty() {
            conformance_patterns.push(format!(
                "    {{ SELECT DISTINCT ?source WHERE {{ ?source ?_structPred ?_structTarget . FILTER(?_structPred IN ({})) }} }}",
                known_predicates.join(", ")
            ));
        }
    }

    // WHERE clause filters that can be pushed to SPARQL.
    let where_patterns = query
        .where_clause
        .as_ref()
        .map(|wc| compile_where_clause(wc, shape).patterns)
        .unwrap_or_default();

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

/// The outcome of compiling a where clause into SPARQL patterns.
pub(super) struct CompiledWhere {
    /// Patterns to splice into the query's WHERE block.
    ///
    /// Always sound to apply even when `complete` is false: every pattern is a
    /// conjunctive constraint, so a partial set narrows the candidates without
    /// ever excluding a row the full clause would have kept.
    pub(super) patterns: Vec<String>,
    /// Whether *every* condition in the clause reached `patterns`.
    ///
    /// When false the caller must still filter post-hydration, and must not push
    /// pagination — a `LIMIT` over a partially-filtered set returns the wrong
    /// rows, silently. This flag is the single source of that answer; deriving it
    /// separately from the emission is what let the two disagree.
    pub(super) complete: bool,
}

/// `None` when a condition produced no patterns at all.
///
/// Every unhandled match arm in [`compile_leaf_condition`] falls through to this,
/// so "the emitter did not understand this condition" is reported rather than
/// silently dropping the constraint.
fn finish(out: Vec<String>) -> Option<Vec<String>> {
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

/// Does this pattern group bind `?source` on its own?
///
/// Only `UNION` branches need to care. SPARQL evaluates each branch
/// independently and joins the result with the surrounding group afterwards, so
/// a branch holding nothing but `FILTER(?source = <x>)` is evaluated with
/// `?source` unbound — the filter errors, the branch yields nothing, and the
/// disjunction quietly loses an arm. Inside the ordinary conjunction that is not
/// a concern, because the instance query's own `?source ?predicate ?target`
/// binds it.
///
/// Deliberately conservative: a false negative only costs pushdown for that
/// query, while a false positive would produce wrong results.
fn binds_source(patterns: &[String]) -> bool {
    patterns.iter().any(|p| {
        let t = p.trim();
        t.starts_with("?source <")
            || t.starts_with("{ ?source <")
            || t.starts_with("VALUES ?source ")
            || t.contains("> ?source .")
    })
}

/// Compile a where clause into SPARQL patterns, reporting whether all of it fit.
///
/// Combinators compile as follows, and each is all-or-nothing for a reason:
///
/// - `AND` — a conjunction, so a partially compiled branch is still sound; the
///   uncompiled parts fall to the post-hydration filter.
/// - `OR` — a disjunction, so partial compilation is **not** sound: emitting only
///   the arms that compiled would exclude rows the missing arm would have
///   matched. Every branch must compile *and* bind `?source`, or the whole `OR`
///   is left to post-hydration.
/// - `NOT` — same reasoning as `OR`; a partial `FILTER NOT EXISTS` excludes rows
///   it should keep. The branch need not bind `?source` itself, because
///   `FILTER NOT EXISTS` is evaluated per solution with outer bindings visible.
pub(super) fn compile_where_clause(
    wc: &BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> CompiledWhere {
    let mut seq = 0usize;
    compile_where_clause_seq(wc, shape, &mut seq)
}

/// The recursive body, threading a counter that makes every generated variable
/// name unique across the whole clause.
///
/// Variable names were derived from the property name alone. That is safe while a
/// where clause is a flat map — its keys are unique — but a combinator lets the
/// same property appear more than once, and two leaves on one property then emit
/// `BIND(… AS ?_pw_title_v)` twice into the same group. SPARQL forbids binding a
/// variable already in scope, so such a clause did not return the wrong rows: it
/// failed to parse.
///
/// The counter advances once per leaf, so variables *within* a leaf still share a
/// suffix and correlate as they must.
fn compile_where_clause_seq(
    wc: &BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
    seq: &mut usize,
) -> CompiledWhere {
    let mut patterns = Vec::new();
    let mut complete = true;

    for (prop_name, condition) in wc {
        match prop_name.as_str() {
            "AND" => match condition {
                WhereCondition::SubClauses(branches) => {
                    for branch in branches {
                        let compiled = compile_where_clause_seq(branch, shape, seq);
                        patterns.extend(compiled.patterns);
                        complete &= compiled.complete;
                    }
                }
                _ => complete = false,
            },
            "OR" => match condition {
                WhereCondition::SubClauses(branches) if !branches.is_empty() => {
                    let mut arms = Vec::with_capacity(branches.len());
                    let mut ok = true;
                    for branch in branches {
                        let compiled = compile_where_clause_seq(branch, shape, seq);
                        if !compiled.complete || !binds_source(&compiled.patterns) {
                            ok = false;
                            break;
                        }
                        arms.push(format!("{{\n{}\n    }}", compiled.patterns.join("\n")));
                    }
                    if ok {
                        patterns.push(format!("    {}", arms.join(" UNION ")));
                    } else {
                        complete = false;
                    }
                }
                _ => complete = false,
            },
            "NOT" => match condition {
                WhereCondition::SubClause(branch) => {
                    let compiled = compile_where_clause_seq(branch, shape, seq);
                    if compiled.complete && !compiled.patterns.is_empty() {
                        patterns.push(format!(
                            "    FILTER NOT EXISTS {{\n{}\n    }}",
                            compiled.patterns.join("\n")
                        ));
                    } else {
                        complete = false;
                    }
                }
                _ => complete = false,
            },
            _ => match compile_leaf_condition(prop_name.as_str(), condition, shape, seq) {
                Some(p) => patterns.extend(p),
                None => complete = false,
            },
        }
    }

    CompiledWhere { patterns, complete }
}

/// Compile one `(property, condition)` pair into SPARQL patterns.
///
/// `None` means "not expressible here" — an unknown property, a getter-backed
/// property with no link predicate, a predicate that is not a valid IRI, or a
/// condition shape no arm handles. The caller turns that into
/// `CompiledWhere::complete = false`, which is what routes the condition to the
/// post-hydration filter instead of dropping it.
fn compile_leaf_condition(
    prop_name: &str,
    condition: &WhereCondition,
    shape: &ModelShape,
    seq: &mut usize,
) -> Option<Vec<String>> {
    let mut out: Vec<String> = Vec::new();
    // One id per leaf: variables inside a leaf share it, because they are meant
    // to correlate. Variables across leaves never do.
    let leaf_id = *seq;
    *seq += 1;

    if prop_name == "base" || prop_name == "id" {
        match condition {
            WhereCondition::String(val) => {
                if validate_iri(val).is_ok() {
                    // `VALUES` rather than `FILTER(?source = <val>)`:
                    // it binds `?source` instead of merely testing it,
                    // which is what lets an `id` condition stand alone
                    // inside a `UNION` branch (see `binds_source`), and
                    // it hands Oxigraph a single subject to seek to
                    // rather than a predicate over every candidate.
                    out.push(format!("    VALUES ?source {{ <{val}> }}"));
                } else {
                    out.push(format!(
                        "    FILTER(STR(?source) = \"{}\")",
                        escape_sparql_string(val)
                    ));
                }
            }
            WhereCondition::StringArray(vals) => {
                let valid: Vec<&str> = vals
                    .iter()
                    .filter(|v| validate_iri(v).is_ok())
                    .map(|v| v.as_str())
                    .collect();
                if valid.len() == vals.len() {
                    let iris = valid
                        .iter()
                        .map(|v| format!("<{v}>"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    out.push(format!("    VALUES ?source {{ {iris} }}"));
                } else {
                    let ids = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    out.push(format!("    FILTER(STR(?source) IN ({ids}))"));
                }
            }
            _ => {}
        }
        return finish(out);
    }

    // Relation-based where
    if let Some(prop) = shape
        .properties
        .iter()
        .find(|p| &p.name == prop_name && p.is_collection)
    {
        let direction = prop.direction.as_deref().unwrap_or("forward");
        match condition {
            WhereCondition::String(val) => {
                if validate_iri(val).is_ok() {
                    if direction == "reverse" {
                        out.push(format!("    <{val}> <{}> ?source .", prop.predicate));
                    } else {
                        out.push(format!("    ?source <{}> <{val}> .", prop.predicate));
                    }
                } else {
                    let safe_name = format!(
                        "{}_{leaf_id}",
                        prop_name.replace(|c: char| !c.is_alphanumeric(), "_")
                    );
                    let escaped = escape_sparql_string(val);
                    if direction == "reverse" {
                        out.push(format!(
                                    "    ?_rv_{safe_name} <{}> ?source . FILTER(STR(?_rv_{safe_name}) = \"{escaped}\")",
                                    prop.predicate
                                ));
                    } else {
                        out.push(format!(
                                    "    ?source <{}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) = \"{escaped}\")",
                                    prop.predicate
                                ));
                    }
                }
            }
            WhereCondition::StringArray(vals) => {
                let safe_name = format!(
                    "{}_{leaf_id}",
                    prop_name.replace(|c: char| !c.is_alphanumeric(), "_")
                );
                let all_valid = vals.iter().all(|v| validate_iri(v).is_ok());
                if all_valid {
                    let iris = vals
                        .iter()
                        .map(|v| format!("<{v}>"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    if direction == "reverse" {
                        out.push(format!(
                                    "    VALUES ?_rv_{safe_name} {{ {iris} }}\n    ?_rv_{safe_name} <{}> ?source .",
                                    prop.predicate
                                ));
                    } else {
                        out.push(format!(
                                    "    VALUES ?_ft_{safe_name} {{ {iris} }}\n    ?source <{}> ?_ft_{safe_name} .",
                                    prop.predicate
                                ));
                    }
                } else {
                    let str_list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if direction == "reverse" {
                        out.push(format!(
                                    "    ?_rv_{safe_name} <{}> ?source . FILTER(STR(?_rv_{safe_name}) IN ({str_list}))",
                                    prop.predicate
                                ));
                    } else {
                        out.push(format!(
                                    "    ?source <{}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) IN ({str_list}))",
                                    prop.predicate
                                ));
                    }
                }
            }
            _ => {}
        }
        return finish(out);
    }

    // Property-based where
    if let Some(prop) = shape
        .properties
        .iter()
        .find(|p| &p.name == prop_name && !p.is_collection && !p.predicate.is_empty())
    {
        if validate_iri(&prop.predicate).is_err() {
            return None;
        }
        let safe_name = format!(
            "{}_{leaf_id}",
            prop_name.replace(|c: char| !c.is_alphanumeric(), "_")
        );
        let is_literal_prop = prop.is_deterministic_literal();
        match condition {
            WhereCondition::String(val) => {
                if is_literal_prop {
                    // Match the typed-literal storage form directly so
                    // Oxigraph's POS index handles the lookup.  When
                    // the value is itself a valid absolute IRI we keep
                    // a UNION fallback so constructor-seeded raw URIs
                    // on literal-resolveLanguage properties still
                    // resolve.
                    let escaped = escape_sparql_string(val);
                    if looks_like_absolute_iri(val) {
                        out.push(format!(
                                    "    {{ ?source <{0}> \"{escaped}\"^^<{XSD_STRING}> . }} UNION {{ ?source <{0}> <{val}> . }}",
                                    prop.predicate
                                ));
                    } else {
                        out.push(format!(
                            "    ?source <{}> \"{escaped}\"^^<{XSD_STRING}> .",
                            prop.predicate
                        ));
                    }
                } else {
                    // Expression-resolved storage (signed literal envelope
                    // or custom resolveLanguage): the stored term is the
                    // envelope/expression, never a NamedNode equal to
                    // `val`. Unwrap with `fn/parse_literal` and compare on
                    // the decoded value. (Emitting `<val>` here would also
                    // be an invalid relative IRI for non-absolute values.)
                    let var = format!("?_pw_{safe_name}");
                    let escaped = escape_sparql_string(val);
                    out.push(format!("    ?source <{}> {var} .", prop.predicate));
                    out.push(format!(
                        "    FILTER(<ad4m://fn/parse_literal>({var}) = \"{escaped}\")"
                    ));
                }
            }
            WhereCondition::Number(n) => {
                if is_literal_prop {
                    if let Some(typed) = typed_number_literal(*n) {
                        out.push(format!("    ?source <{}> {typed} .", prop.predicate));
                    } else {
                        out.push("    FILTER(false)".to_string());
                    }
                } else {
                    let var = format!("?_pw_{safe_name}");
                    out.push(format!("    ?source <{}> {var} .", prop.predicate));
                    out.push(format!(
                        "    FILTER(<ad4m://fn/parse_literal>({var}) = \"{n}\")"
                    ));
                }
            }
            WhereCondition::Bool(b) => {
                if is_literal_prop {
                    out.push(format!(
                        "    ?source <{}> \"{b}\"^^<{XSD_BOOLEAN}> .",
                        prop.predicate
                    ));
                } else {
                    let var = format!("?_pw_{safe_name}");
                    out.push(format!("    ?source <{}> {var} .", prop.predicate));
                    out.push(format!(
                        "    FILTER(<ad4m://fn/parse_literal>({var}) = \"{b}\")"
                    ));
                }
            }
            WhereCondition::StringArray(vals) => {
                if is_literal_prop {
                    let mut items: Vec<String> = Vec::with_capacity(vals.len() * 2);
                    for v in vals {
                        let escaped = escape_sparql_string(v);
                        items.push(format!("\"{escaped}\"^^<{XSD_STRING}>"));
                        if looks_like_absolute_iri(v) {
                            items.push(format!("<{v}>"));
                        }
                    }
                    let iv_var = format!("?_iv_{safe_name}");
                    out.push(format!("    VALUES {iv_var} {{ {} }}", items.join(" ")));
                    out.push(format!("    ?source <{}> {iv_var} .", prop.predicate));
                } else {
                    let values_list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let var = format!("?_pw_{safe_name}");
                    out.push(format!("    ?source <{}> {var} .", prop.predicate));
                    out.push(format!(
                        "    FILTER(<ad4m://fn/parse_literal>({var}) IN ({values_list}))"
                    ));
                }
            }
            WhereCondition::NumberArray(vals) => {
                if is_literal_prop {
                    let items: Vec<String> = vals
                        .iter()
                        .filter_map(|n| typed_number_literal(*n))
                        .collect();
                    if items.is_empty() {
                        out.push("    FILTER(false)".to_string());
                    } else {
                        let iv_var = format!("?_iv_{safe_name}");
                        out.push(format!("    VALUES {iv_var} {{ {} }}", items.join(" ")));
                        out.push(format!("    ?source <{}> {iv_var} .", prop.predicate));
                    }
                } else {
                    let values_list = vals
                        .iter()
                        .map(|n| format!("\"{n}\""))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let var = format!("?_pw_{safe_name}");
                    out.push(format!("    ?source <{}> {var} .", prop.predicate));
                    out.push(format!(
                        "    FILTER(<ad4m://fn/parse_literal>({var}) IN ({values_list}))"
                    ));
                }
            }
            WhereCondition::Ops(ops) => {
                let var = format!("?_pw_{safe_name}");
                let val_var = format!("?_pw_{safe_name}_v");
                out.push(format!("    ?source <{}> {var} .", prop.predicate));
                // Compare on the lexical string rather than on a typed
                // literal term: Oxigraph treats a simple literal and an
                // `xsd:string` literal as distinct terms, so `?v != "x"^^xsd:string`
                // silently fails to match values stored either way.
                //
                // Deterministic literals expose their value via `STR()`
                // directly; envelope / custom-language values need
                // `parse_literal` first to reach the inner `data`.
                let val_src = if is_literal_prop {
                    var.clone()
                } else {
                    format!("<ad4m://fn/parse_literal>({var})")
                };
                out.push(format!("    BIND(STR({val_src}) AS {val_var})"));

                let mut filters = Vec::new();
                // A requested operator that produces no filter must not leave the
                // condition looking compiled. The triple pattern and BIND above are
                // already in `out`, so `finish` would return `Some` and the caller
                // would skip post-hydration filtering — silently dropping the
                // constraint. Today both layers ignore the same shapes, so no rows
                // differ; they agree by coincidence rather than by construction, and
                // this PR's whole point is that pushability follows the emission.
                let mut unhandled = false;

                if let Some(ref not_val) = ops.not {
                    match not_val {
                        Value::String(s) => {
                            filters.push(format!("{val_var} != \"{}\"", escape_sparql_string(s)));
                        }
                        Value::Number(n) => {
                            let n_f64 = n.as_f64().unwrap_or(0.0);
                            let n_str = if n_f64.fract() == 0.0 {
                                format!("{}", n_f64 as i64)
                            } else {
                                format!("{n_f64}")
                            };
                            filters.push(format!("{val_var} != \"{n_str}\""));
                        }
                        Value::Bool(b) => {
                            filters.push(format!("{val_var} != \"{b}\""));
                        }
                        Value::Array(arr) => {
                            let items: Vec<String> = arr
                                .iter()
                                .filter_map(|item| match item {
                                    Value::String(s) => {
                                        Some(format!("\"{}\"", escape_sparql_string(s)))
                                    }
                                    Value::Number(n) => {
                                        let f = n.as_f64().unwrap_or(0.0);
                                        let s = if f.fract() == 0.0 {
                                            format!("{}", f as i64)
                                        } else {
                                            format!("{f}")
                                        };
                                        Some(format!("\"{s}\""))
                                    }
                                    _ => None,
                                })
                                .collect();
                            if items.is_empty() {
                                // Either an empty array or one holding only shapes
                                // this cannot render.
                                unhandled = true;
                            } else {
                                filters.push(format!("{val_var} NOT IN ({})", items.join(", ")));
                            }
                        }
                        _ => unhandled = true,
                    }
                }

                let has_numeric = ops.gt.is_some()
                    || ops.gte.is_some()
                    || ops.lt.is_some()
                    || ops.lte.is_some()
                    || ops.between.is_some();

                if has_numeric {
                    let num_var = format!("?_pw_{safe_name}_num");
                    out.push(format!("    BIND(<{XSD_DOUBLE}>({val_var}) AS {num_var})"));
                    if let Some(gt) = ops.gt {
                        filters.push(format!("{num_var} > {gt}"));
                    }
                    if let Some(gte) = ops.gte {
                        filters.push(format!("{num_var} >= {gte}"));
                    }
                    if let Some(lt) = ops.lt {
                        filters.push(format!("{num_var} < {lt}"));
                    }
                    if let Some(lte) = ops.lte {
                        filters.push(format!("{num_var} <= {lte}"));
                    }
                    if let Some((lo, hi)) = ops.between {
                        filters.push(format!("{num_var} >= {lo} && {num_var} <= {hi}"));
                    }
                }

                if let Some(ref contains_val) = ops.contains {
                    let needle = match contains_val {
                        Value::String(s) => s.clone(),
                        other => other.to_string(),
                    };
                    filters.push(format!(
                        "CONTAINS(LCASE({val_var}), LCASE(\"{}\"))",
                        escape_sparql_string(&needle)
                    ));
                }

                // No filters at all means either an empty `{}` — no operator was
                // requested, so this constrains nothing and should not narrow to
                // "the property exists" — or every requested one was unrenderable.
                if filters.is_empty() || unhandled {
                    return None;
                }
                out.push(format!("    FILTER({})", filters.join(" && ")));
            }
            // SubClauses/SubClause are OR/AND/NOT combinators evaluated
            // Rust-side; the outer loop skips them before reaching here.
            WhereCondition::SubClauses(_) | WhereCondition::SubClause(_) => {}
        }
        return finish(out);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::{flag, prop, relation, shape};

    fn make_pg(sort_key: SortKey, direction: OrderDirection) -> SparqlPagination {
        SparqlPagination {
            sort_key,
            direction,
            offset: None,
            limit: Some(10),
        }
    }

    fn pagination_subquery(shape: &ModelShape, pg: &SparqlPagination) -> String {
        match build_instance_sparql(shape, &ModelQueryInput::default(), Some(pg)) {
            InstanceQueryPlan::TwoPhase {
                pagination_subquery,
                ..
            } => pagination_subquery,
            InstanceQueryPlan::Single(_) => panic!("Expected TwoPhase query plan, got Single"),
        }
    }

    #[test]
    fn test_pagination_subquery_projection_sort_contains_count_distinct() {
        let s = shape(
            "Post",
            vec![
                flag("type", "test://type", "test://post"),
                prop("title", "test://title"),
            ],
        );
        let pg = make_pg(
            SortKey::Projection("test://has-like".to_string()),
            OrderDirection::DESC,
        );
        let sparql = pagination_subquery(&s, &pg);

        assert!(
            sparql.contains("COUNT(DISTINCT ?_proj_t)"),
            "should emit COUNT(DISTINCT) for projection sort: {sparql}"
        );
        assert!(
            sparql.contains("?_proj_sort"),
            "should project ?_proj_sort: {sparql}"
        );
        assert!(
            sparql.contains("OPTIONAL { ?source <test://has-like> ?_proj_t"),
            "should join via predicate OPTIONAL: {sparql}"
        );
        assert!(
            sparql.contains("GROUP BY ?source"),
            "should use GROUP BY for count aggregate: {sparql}"
        );
        assert!(
            sparql.contains("DESC(?_proj_sort)"),
            "ORDER BY should use DESC: {sparql}"
        );
    }

    #[test]
    fn test_pagination_subquery_projection_sort_asc() {
        let s = shape("Post", vec![flag("type", "test://type", "test://post")]);
        let pg = make_pg(
            SortKey::Projection("test://has-comment".to_string()),
            OrderDirection::ASC,
        );
        let sparql = pagination_subquery(&s, &pg);
        assert!(
            sparql.contains("ASC(?_proj_sort)"),
            "ORDER BY should use ASC: {sparql}"
        );
    }

    #[test]
    fn test_pagination_subquery_relation_property_sort_contains_double_optional() {
        let s = shape(
            "Post",
            vec![
                flag("type", "test://type", "test://post"),
                prop("title", "test://title"),
            ],
        );
        let pg = make_pg(
            SortKey::RelationProperty {
                rel_pred: "test://has-location".to_string(),
                prop_pred: "test://location-name".to_string(),
            },
            OrderDirection::ASC,
        );
        let sparql = pagination_subquery(&s, &pg);

        assert!(
            sparql.contains("?source <test://has-location> ?_rp_rel"),
            "outer OPTIONAL should join via relation predicate: {sparql}"
        );
        assert!(
            sparql.contains("?_rp_rel <test://location-name> ?_rp_raw"),
            "inner OPTIONAL should join via property predicate: {sparql}"
        );
        assert!(
            sparql.contains("SAMPLE(?_rp_num_v)") && sparql.contains("SAMPLE(?_rp_str_v)"),
            "should project SAMPLE of numeric and string sort columns: {sparql}"
        );
        assert!(
            sparql.contains("ASC(IF(BOUND(?_rp_str), 0, 1))"),
            "ORDER BY should push nulls to end: {sparql}"
        );
        assert!(
            sparql.contains("GROUP BY ?source"),
            "should use GROUP BY: {sparql}"
        );
    }

    #[test]
    fn test_pagination_subquery_relation_property_sort_desc() {
        let s = shape("Post", vec![flag("type", "test://type", "test://post")]);
        let pg = make_pg(
            SortKey::RelationProperty {
                rel_pred: "test://has-location".to_string(),
                prop_pred: "test://location-name".to_string(),
            },
            OrderDirection::DESC,
        );
        let sparql = pagination_subquery(&s, &pg);
        assert!(
            sparql.contains("DESC(?_rp_num)") && sparql.contains("DESC(?_rp_str)"),
            "ORDER BY should use DESC: {sparql}"
        );
    }
}

#[cfg(test)]
mod where_compiler_tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::{prop, relation, shape};

    fn wc(pairs: Vec<(&str, WhereCondition)>) -> BTreeMap<String, WhereCondition> {
        pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
    }

    fn branch(pairs: Vec<(&str, WhereCondition)>) -> BTreeMap<String, WhereCondition> {
        wc(pairs)
    }

    fn test_shape() -> ModelShape {
        shape(
            "Post",
            vec![
                prop("title", "we://title"),
                prop("body", "we://body"),
                relation("signals", "we://signal"),
            ],
        )
    }

    /// A property defined by a getter carries no link predicate.
    fn getter_shape() -> ModelShape {
        let mut p = prop("computed", "");
        p.getter = Some("SELECT ?target WHERE { <Base> ?p ?target }".to_string());
        shape("Post", vec![prop("title", "we://title"), p])
    }

    // ---- the divergence this refactor closes -----------------------------

    #[test]
    fn test_getter_property_condition_is_not_pushable() {
        // Regression: `all_where_pushable` reported *any* condition on a scalar
        // property pushable, while the emitter additionally required a
        // non-empty, IRI-valid predicate. A getter-backed property has neither,
        // so the condition reached SPARQL from neither side and the caller —
        // seeing "pushable" — skipped post-hydration filtering too. The query
        // silently returned unfiltered rows.
        let s = getter_shape();
        let clause = wc(vec![(
            "computed",
            WhereCondition::String("anything".to_string()),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(compiled.patterns.is_empty(), "nothing can be emitted");
        assert!(
            !compiled.complete,
            "so the clause must be reported incomplete, sending it to the \
             post-hydration filter instead of being dropped",
        );
    }

    #[test]
    fn test_unknown_property_is_not_pushable() {
        let s = test_shape();
        let clause = wc(vec![("nope", WhereCondition::String("x".to_string()))]);
        assert!(!compile_where_clause(&clause, &s).complete);
    }

    #[test]
    fn test_pushability_is_derived_from_emission() {
        // The property that makes the two impossible to desynchronise: a clause
        // is complete exactly when compiling it produced patterns for every key.
        let s = test_shape();
        let clause = wc(vec![
            ("title", WhereCondition::String("hello".to_string())),
            ("nope", WhereCondition::String("x".to_string())),
        ]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(!compiled.patterns.is_empty(), "the known key still pushes");
        assert!(!compiled.complete, "but the clause is not fully covered");
    }

    // ---- OR -> UNION -----------------------------------------------------

    #[test]
    fn test_or_compiles_to_union() {
        let s = test_shape();
        let clause = wc(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                branch(vec![("title", WhereCondition::String("a".to_string()))]),
                branch(vec![("body", WhereCondition::String("b".to_string()))]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("UNION"), "expected a UNION: {sparql}");
        assert!(sparql.contains("we://title"), "{sparql}");
        assert!(sparql.contains("we://body"), "{sparql}");
    }

    #[test]
    fn test_or_is_all_or_nothing() {
        // Emitting only the arms that compiled would exclude rows the missing
        // arm would have matched — unlike a conjunction, a partial disjunction
        // is not a sound narrowing.
        let s = test_shape();
        let clause = wc(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                branch(vec![("title", WhereCondition::String("a".to_string()))]),
                branch(vec![("nope", WhereCondition::String("b".to_string()))]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(
            compiled.patterns.is_empty(),
            "no half-disjunction is emitted"
        );
        assert!(!compiled.complete);
    }

    #[test]
    fn test_or_over_id_binds_source() {
        // `id` emits VALUES rather than a bare FILTER precisely so it can stand
        // alone inside a UNION branch, where `?source` is otherwise unbound.
        let s = test_shape();
        let clause = wc(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                branch(vec![(
                    "id",
                    WhereCondition::String("we://post/1".to_string()),
                )]),
                branch(vec![(
                    "id",
                    WhereCondition::String("we://post/2".to_string()),
                )]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(compiled.complete, "an id disjunction must push down");
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("VALUES ?source"), "{sparql}");
        assert!(!sparql.contains("FILTER(?source ="), "{sparql}");
    }

    #[test]
    fn test_or_refuses_a_branch_that_cannot_bind_source() {
        // A non-IRI id compiles to `FILTER(STR(?source) = …)`, which tests
        // `?source` without binding it. Sound in the main conjunction, useless
        // in a UNION arm — so the disjunction falls back rather than silently
        // losing that arm.
        //
        // Note `literal:string:x` does NOT exercise this: it parses as a valid
        // absolute IRI (scheme `literal`) and so takes the VALUES branch. The
        // value here has a space in it, which no IRI may.
        let s = test_shape();
        let clause = wc(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                branch(vec![(
                    "id",
                    WhereCondition::String("not an iri".to_string()),
                )]),
                branch(vec![("title", WhereCondition::String("a".to_string()))]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }

    // ---- NOT -> FILTER NOT EXISTS ----------------------------------------

    #[test]
    fn test_not_compiles_to_filter_not_exists() {
        let s = test_shape();
        let clause = wc(vec![(
            "NOT",
            WhereCondition::SubClause(branch(vec![(
                "title",
                WhereCondition::String("draft".to_string()),
            )])),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("FILTER NOT EXISTS"), "{sparql}");
        assert!(sparql.contains("we://title"), "{sparql}");
    }

    #[test]
    fn test_not_branch_need_not_bind_source() {
        // FILTER NOT EXISTS is evaluated per solution with the outer bindings
        // visible, so — unlike a UNION arm — its branch may test `?source`
        // without binding it.
        let s = test_shape();
        let clause = wc(vec![(
            "NOT",
            WhereCondition::SubClause(branch(vec![(
                "id",
                WhereCondition::String("not an iri".to_string()),
            )])),
        )]);

        assert!(compile_where_clause(&clause, &s).complete);
    }

    // ---- AND -------------------------------------------------------------

    #[test]
    fn test_and_is_conjunctive_and_may_be_partial() {
        // A conjunction narrows, so pushing the arms that compiled is sound;
        // the rest is caught post-hydration, which `complete: false` requests.
        let s = test_shape();
        let clause = wc(vec![(
            "AND",
            WhereCondition::SubClauses(vec![
                branch(vec![("title", WhereCondition::String("a".to_string()))]),
                branch(vec![("nope", WhereCondition::String("b".to_string()))]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(!compiled.patterns.is_empty(), "the sound half still pushes");
        assert!(!compiled.complete);
    }

    #[test]
    fn test_nested_combinators_compile() {
        let s = test_shape();
        let inner = wc(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                branch(vec![("title", WhereCondition::String("a".to_string()))]),
                branch(vec![("title", WhereCondition::String("b".to_string()))]),
            ]),
        )]);
        let clause = wc(vec![(
            "AND",
            WhereCondition::SubClauses(vec![
                inner,
                branch(vec![("body", WhereCondition::String("c".to_string()))]),
            ]),
        )]);

        let compiled = compile_where_clause(&clause, &s);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("UNION"), "{sparql}");
        assert!(sparql.contains("we://body"), "{sparql}");
    }
}

#[cfg(test)]
mod ops_completeness_tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::{prop, shape};
    use crate::perspectives::model_query::types::WhereOps;

    fn wc(p: Vec<(&str, WhereCondition)>) -> BTreeMap<String, WhereCondition> {
        p.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
    }
    fn s() -> ModelShape {
        shape("Post", vec![prop("title", "ns://title")])
    }
    fn ops(o: WhereOps) -> WhereCondition {
        WhereCondition::Ops(o)
    }

    #[test]
    fn a_renderable_operator_pushes_down() {
        let c = wc(vec![(
            "title",
            ops(WhereOps {
                not: Some(Value::String("draft".into())),
                ..Default::default()
            }),
        )]);
        assert!(compile_where_clause(&c, &s()).complete);
    }

    #[test]
    fn an_unrenderable_not_shape_is_not_complete() {
        // An object is not a value this can compare against. The triple pattern and
        // BIND are already emitted, so without the guard `finish` would report the
        // clause compiled and the caller would skip the Rust filter entirely.
        let c = wc(vec![(
            "title",
            ops(WhereOps {
                not: Some(serde_json::json!({"a": 1})),
                ..Default::default()
            }),
        )]);
        let compiled = compile_where_clause(&c, &s());
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }

    #[test]
    fn an_empty_not_array_is_not_complete() {
        let c = wc(vec![(
            "title",
            ops(WhereOps {
                not: Some(Value::Array(vec![])),
                ..Default::default()
            }),
        )]);
        assert!(!compile_where_clause(&c, &s()).complete);
    }

    #[test]
    fn one_bad_operator_spoils_an_otherwise_good_ops() {
        // The `gt` renders fine, so filters is non-empty — the reason a bare
        // `filters.is_empty()` check would not have caught this.
        let c = wc(vec![(
            "title",
            ops(WhereOps {
                not: Some(serde_json::json!({"a": 1})),
                gt: Some(5.0),
                ..Default::default()
            }),
        )]);
        assert!(
            !compile_where_clause(&c, &s()).complete,
            "a dropped `not` must not be masked by a rendered `gt`",
        );
    }

    #[test]
    fn an_empty_ops_object_constrains_nothing() {
        // `{}` requests no operator. Emitting the triple pattern alone would narrow
        // to "the property exists", which is not what the Rust filter does for the
        // same clause — it matches everything.
        let c = wc(vec![("title", ops(WhereOps::default()))]);
        let compiled = compile_where_clause(&c, &s());
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }
}
