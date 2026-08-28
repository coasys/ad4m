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
    InstanceQueryPlan, ModelQueryInput, ModelShape, OrderDirection, Scope, ShapeResolver, SortKey,
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
            "?_r <{rdf_reifies}> <<( ?source <{}> ?_cf_{safe_name} )>> . ?_r <{ont_ts}> ?_first_ts .",
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
    resolver: Option<&dyn ShapeResolver>,
) -> InstanceQueryPlan {
    let (conformance, where_extra) = build_query_patterns(shape, query, resolver);

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
pub(super) fn build_count_sparql(
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: Option<&dyn ShapeResolver>,
) -> Option<String> {
    let (conformance, where_extra) = build_query_patterns(shape, query, resolver);

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
pub(super) fn all_where_pushable(
    query: &ModelQueryInput,
    shape: &ModelShape,
    resolver: Option<&dyn ShapeResolver>,
) -> bool {
    match query.where_clause {
        None => true,
        Some(ref wc) => compile_where_clause(wc, shape, resolver).complete,
    }
}

/// Build the conformance and where-clause SPARQL pattern strings.
///
/// Returns `(conformance, where_extra)` — two SPARQL fragments that are
/// interpolated into both the instance query and the `COUNT` query.
///
/// **Conformance patterns** ensure only instances of the target model class
/// are matched — see [`shape_conformance_patterns`] for the tiers.
///
/// **Where patterns** translate the query's `where` clause into SPARQL
/// `FILTER`/`VALUES` expressions for server-side evaluation.
pub(super) fn build_query_patterns(
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: Option<&dyn ShapeResolver>,
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

    // The structural fallback is a last resort, and a parent scope has already
    // narrowed `?source` to one node's children — narrow enough that a scan of
    // every node sharing a predicate would cost more than it excludes.
    let scoped_by_parent = !conformance_patterns.is_empty();
    conformance_patterns.extend(shape_conformance_patterns(shape, !scoped_by_parent));

    // WHERE clause filters that can be pushed to SPARQL.
    let where_patterns = query
        .where_clause
        .as_ref()
        .map(|wc| compile_where_clause(wc, shape, resolver).patterns)
        .unwrap_or_default();

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

/// The patterns that say "`?source` is an instance of this class", derived from
/// the shape alone, in fallback tiers:
/// 1. Required properties → `?source <pred> ?_cf_name .`
/// 2. Flag properties with initial values → `?source <pred> <initial> .`
/// 3. Any property with an initial value (first match)
/// 4. Structural fallback using known predicates via `FILTER(?_structPred IN (...))`
///
/// The helper variables begin `?_` so that [`rebase_into_subquery`] namespaces
/// them: emitted into a nested clause under their bare names they would collide
/// with the enclosing class's conformance variable for a property of the same
/// name, silently requiring the two records to share a value for it.
///
/// Split out of [`build_query_patterns`] because a relation quantifier needs the
/// same answer about its *target* class: `FILTER EXISTS` over the relation's
/// predicate alone asks whether the record links to anything with the right
/// properties, while hydration resolves that relation through the target class's
/// own query — which applies these. Without them a record can match a clause and
/// come back with the relation empty.
///
/// `allow_structural_fallback` gates the final tier, which scans every node
/// carrying any of the class's predicates. It is the weakest signal and the most
/// expensive one, so a caller that has already narrowed the subject declines it.
fn shape_conformance_patterns(shape: &ModelShape, allow_structural_fallback: bool) -> Vec<String> {
    let mut conformance_patterns = Vec::new();

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
                            "    ?source <{}> ?_cf_{safe_name} . FILTER(STR(?_cf_{safe_name}) = \"{escaped}\")",
                            prop.predicate
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?_cf_{safe_name} .",
                        prop.predicate
                    ));
                }
            } else {
                conformance_patterns.push(format!(
                    "    ?source <{}> ?_cf_{safe_name} .",
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
                            "    ?source <{}> ?_cfInit_{safe_name} . FILTER(STR(?_cfInit_{safe_name}) = \"{escaped}\")",
                            prop.predicate
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?_cfInit_{safe_name} .",
                        prop.predicate
                    ));
                }
                break;
            }
        }
    }

    // Fallback: structural matching using known predicates.
    if !has_conformance && allow_structural_fallback {
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

    conformance_patterns
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
    resolver: Option<&dyn ShapeResolver>,
) -> CompiledWhere {
    let mut seq = 0usize;
    compile_where_clause_seq(wc, shape, resolver, &mut seq)
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
    resolver: Option<&dyn ShapeResolver>,
    seq: &mut usize,
) -> CompiledWhere {
    let mut patterns = Vec::new();
    let mut complete = true;

    for (prop_name, condition) in wc {
        match prop_name.as_str() {
            "AND" => match condition {
                WhereCondition::SubClauses(branches) => {
                    for branch in branches {
                        let compiled = compile_where_clause_seq(branch, shape, resolver, seq);
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
                        let compiled = compile_where_clause_seq(branch, shape, resolver, seq);
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
                    let compiled = compile_where_clause_seq(branch, shape, resolver, seq);
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
            _ => {
                match compile_leaf_condition(prop_name.as_str(), condition, shape, resolver, seq) {
                    Some(p) => patterns.extend(p),
                    None => complete = false,
                }
            }
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
    resolver: Option<&dyn ShapeResolver>,
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

    // Relation-based where.
    //
    // Every relation is `is_collection` regardless of cardinality — `load_shape`
    // marks them so the pipeline hydrates them all as arrays, and
    // `is_scalar_relation` is what unwraps a `@HasOne` afterwards. So this
    // branch is "the property is a link", not "the property is many-valued", and
    // a quantifier over a to-one relation lands here like any other.
    if let Some(prop) = shape
        .properties
        .iter()
        .find(|p| &p.name == prop_name && p.is_collection)
    {
        // Validated once for the whole branch rather than per arm: every arm
        // emits it into a triple pattern, and two of them used to trust it while
        // the property branch below checked. Declining is the sound failure —
        // the condition falls to the post-hydration filter instead of reaching
        // the store as malformed SPARQL. Empty counts as unemittable for the
        // same reason it does in the property branch: a getter-backed relation
        // has no predicate, and `<>` is a relative IRI that matches nothing
        // while reporting the clause pushed.
        if prop.predicate.is_empty() {
            return None;
        }
        let safe_pred = validate_iri(&prop.predicate).ok()?;
        let direction = prop.direction.as_deref().unwrap_or("forward");
        match condition {
            WhereCondition::String(val) => {
                if validate_iri(val).is_ok() {
                    if direction == "reverse" {
                        out.push(format!("    <{val}> <{safe_pred}> ?source ."));
                    } else {
                        out.push(format!("    ?source <{safe_pred}> <{val}> ."));
                    }
                } else {
                    let safe_name = format!(
                        "{}_{leaf_id}",
                        prop_name.replace(|c: char| !c.is_alphanumeric(), "_")
                    );
                    let escaped = escape_sparql_string(val);
                    if direction == "reverse" {
                        out.push(format!(
                            "    ?_rv_{safe_name} <{safe_pred}> ?source . FILTER(STR(?_rv_{safe_name}) = \"{escaped}\")"
                        ));
                    } else {
                        out.push(format!(
                            "    ?source <{safe_pred}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) = \"{escaped}\")"
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
                            "    VALUES ?_rv_{safe_name} {{ {iris} }}\n    ?_rv_{safe_name} <{safe_pred}> ?source ."
                        ));
                    } else {
                        out.push(format!(
                            "    VALUES ?_ft_{safe_name} {{ {iris} }}\n    ?source <{safe_pred}> ?_ft_{safe_name} ."
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
                            "    ?_rv_{safe_name} <{safe_pred}> ?source . FILTER(STR(?_rv_{safe_name}) IN ({str_list}))"
                        ));
                    } else {
                        out.push(format!(
                            "    ?source <{safe_pred}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) IN ({str_list}))"
                        ));
                    }
                }
            }
            WhereCondition::Ops(ops) if ops.some.is_some() || ops.none.is_some() => {
                // Every decline below ends the same way for the caller: the
                // clause is incomplete, the post-hydration filter rejects every
                // row, and the query returns nothing. That is the sound answer
                // but an opaque one, so each reason says itself here — once per
                // compile, where the reason is known, rather than once per row
                // in `matches_ops`, where it is not.
                //
                // A quantifier compiles to an EXISTS group and nothing else, so
                // any operator sitting beside it would be dropped on the way
                // out — and the post-hydration layer, which fails closed on a
                // quantifier, would not catch it either. Decline instead.
                if ops.not.is_some()
                    || ops.contains.is_some()
                    || ops.between.is_some()
                    || ops.lt.is_some()
                    || ops.lte.is_some()
                    || ops.gt.is_some()
                    || ops.gte.is_some()
                {
                    log::warn!(
                        "where: `{prop_name}` combines a relation quantifier with another \
                         operator. A quantifier compiles to an EXISTS group that carries \
                         nothing else, so the pair cannot be answered together. The query \
                         will return no rows."
                    );
                    return None;
                }
                let (inner, negate) = match (&ops.some, &ops.none) {
                    (Some(inner), None) => (inner, false),
                    (None, Some(inner)) => (inner, true),
                    // Both at once has no single sensible reading, and
                    // guessing one would be worse than declining.
                    _ => {
                        log::warn!(
                            "where: `{prop_name}` sets both `some` and `none`, which has no \
                             single reading. The query will return no rows."
                        );
                        return None;
                    }
                };
                let quantified = compile_relation_quantifier(
                    shape, prop, safe_pred, inner, negate, resolver, leaf_id,
                )?;
                out.push(quantified);
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
        match build_instance_sparql(shape, &ModelQueryInput::default(), Some(pg), None) {
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

        let compiled = compile_where_clause(&clause, &s, None);

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
        assert!(!compile_where_clause(&clause, &s, None).complete);
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

        let compiled = compile_where_clause(&clause, &s, None);

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

        let compiled = compile_where_clause(&clause, &s, None);

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

        let compiled = compile_where_clause(&clause, &s, None);

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

        let compiled = compile_where_clause(&clause, &s, None);

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

        let compiled = compile_where_clause(&clause, &s, None);
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

        let compiled = compile_where_clause(&clause, &s, None);

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

        assert!(compile_where_clause(&clause, &s, None).complete);
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

        let compiled = compile_where_clause(&clause, &s, None);

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

        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("UNION"), "{sparql}");
        assert!(sparql.contains("we://body"), "{sparql}");
    }
}
/// Rename every generated variable in `patterns` so they cannot collide with
/// the scope they are about to be nested into.
///
/// `FILTER EXISTS` shares the enclosing variable scope, so an inner clause
/// constraining the same property as the outer one would otherwise reuse the
/// same helper variable and silently correlate the two — the inner condition
/// would constrain the *outer* row rather than the linked one.
///
/// Only *variable tokens* are rewritten. A plain `str::replace` over the whole
/// pattern would also rewrite the text inside quoted literals and IRIs, so a
/// record whose title is the string `"?source"` would be searched for under the
/// linked record's variable name instead — a wrong answer with nothing on the
/// wire to say so. Scanning skips both, which also lets a single pass do the
/// job: `?source` and the helper variables are distinguished by name rather
/// than by substitution order, so nothing is rewritten twice. Nesting is still
/// safe because each pass prefixes whatever the previous pass produced, so
/// depth keeps the names distinct without threading a counter.
fn rebase_into_subquery(patterns: &[String], namespace: &str, target_var: &str) -> Vec<String> {
    patterns
        .iter()
        .map(|p| rebase_pattern(p, namespace, target_var))
        .collect()
}

/// Rewrite the SPARQL variables in one pattern, leaving lexical values alone.
fn rebase_pattern(pattern: &str, namespace: &str, target_var: &str) -> String {
    let mut out = String::with_capacity(pattern.len() + namespace.len());
    let mut chars = pattern.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            // A quoted literal is a value, not syntax. Copy it through verbatim,
            // honouring the backslash escapes `escape_sparql_string` writes so a
            // `\"` does not read as the end of the literal.
            '"' => {
                out.push(c);
                while let Some(lit) = chars.next() {
                    out.push(lit);
                    if lit == '\\' {
                        if let Some(escaped) = chars.next() {
                            out.push(escaped);
                        }
                    } else if lit == '"' {
                        break;
                    }
                }
            }
            // An IRI is opaque too, and may legitimately carry `?source` in a
            // query component. `validate_iri` guarantees no `>` inside one.
            '<' => {
                out.push(c);
                for iri in chars.by_ref() {
                    out.push(iri);
                    if iri == '>' {
                        break;
                    }
                }
            }
            '?' => {
                let mut name = String::new();
                while let Some(&n) = chars.peek() {
                    if n.is_alphanumeric() || n == '_' {
                        name.push(n);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if name == "source" {
                    out.push_str(target_var);
                } else if let Some(rest) = name.strip_prefix('_') {
                    // Helper variables all begin `?_`; namespacing them is what
                    // keeps an inner clause from correlating with the outer one.
                    out.push_str("?_");
                    out.push_str(namespace);
                    out.push_str(rest);
                } else {
                    out.push('?');
                    out.push_str(&name);
                }
            }
            _ => out.push(c),
        }
    }

    out
}

/// Compile `{ rel: { some | none: { … } } }` into `FILTER [NOT] EXISTS`.
///
/// The nested clause constrains the **linked** record, not this one, so it is
/// compiled against the target class's shape and rebased onto the variable
/// bound by the relation triple.
///
/// Returns `None` — declining to push down — when the nested clause cannot be
/// compiled in full. A partial `EXISTS` would match rows the full clause
/// rejects, and a partial `NOT EXISTS` would reject rows it should keep;
/// neither is a sound narrowing, so this is all-or-nothing like `OR`.
///
/// An **empty** nested clause is the "has any" / "has none" case and needs no
/// target shape, so it compiles even without a resolver.
fn compile_relation_quantifier(
    shape: &ModelShape,
    prop: &super::types::ShapeProperty,
    safe_pred: &str,
    inner: &BTreeMap<String, WhereCondition>,
    negate: bool,
    resolver: Option<&dyn ShapeResolver>,
    leaf_id: usize,
) -> Option<String> {
    // The caller found `prop` *by* this name, so the two cannot disagree.
    let prop_name = prop.name.as_str();
    // The leaf id keeps two quantifiers on the same relation apart. Each
    // `FILTER EXISTS` is already its own group, so this is belt-and-braces — but
    // it costs nothing and removes a case anyone would have to reason about.
    let namespace = format!(
        "q{leaf_id}{}",
        prop_name.replace(|c: char| !c.is_alphanumeric(), "_")
    );
    let target_var = format!("?_{namespace}t");

    // A reverse relation is `target → source`, so the linked record is the
    // subject of the triple rather than its object.
    let link = if prop.direction.as_deref() == Some("reverse") {
        format!("        {target_var} <{safe_pred}> ?source .")
    } else {
        format!("        ?source <{safe_pred}> {target_var} .")
    };

    let mut body = vec![link];

    // `include_relations` is the shape's own record of every link-typed property
    // and the class it points at, written by `load_shape` from the SHACL — it is
    // not the query's `include` map, and nothing here depends on the caller
    // having asked to hydrate this relation. A relation is missing from it only
    // when the SDNA declares no target class at all, which is also the case
    // where there is no class to conform against.
    let target_class = shape
        .include_relations
        .iter()
        .find(|r| r.name == prop_name)
        .map(|r| r.target_class_name.as_str())
        .filter(|n| !n.is_empty());
    let target_shape = match (target_class, resolver) {
        (Some(class), Some(r)) => match r.get_shape(class) {
            Ok(s) => Some(s),
            Err(e) => {
                log::warn!(
                    "where: relation `{prop_name}` names target class `{class}`, which the \
                     shape resolver could not load ({e}). A quantifier over it cannot be \
                     compiled."
                );
                None
            }
        },
        _ => None,
    };

    // Being linked by the relation's predicate is not the same as being an
    // instance of the class it names. Hydration resolves the relation through
    // the target class's own query, which applies these patterns, so without
    // them a record can satisfy `some: { body: … }` and come back with its
    // `comments` empty — the filter and the row disagreeing about the same
    // link. The structural fallback is declined: the link triple has already
    // narrowed the subject to this record's targets, and a scan of every node
    // sharing a predicate would cost more inside an `EXISTS` than it excludes.
    if let Some(ref target_shape) = target_shape {
        body.extend(rebase_into_subquery(
            &shape_conformance_patterns(target_shape.as_ref(), false),
            &namespace,
            &target_var,
        ));
    }

    if !inner.is_empty() {
        // The nested clause names properties of the *target* class, so it can
        // only be compiled with that class's shape in hand.
        let Some(target_shape) = target_shape.as_ref() else {
            log::warn!(
                "where: a nested clause on relation `{prop_name}` needs the target class's \
                 shape, and none is available — the relation declares no target class, or \
                 the query was compiled without a shape resolver. The query will return no \
                 rows."
            );
            return None;
        };

        let compiled = compile_where_clause(inner, target_shape.as_ref(), resolver);
        if !compiled.complete || compiled.patterns.is_empty() {
            log::warn!(
                "where: the nested clause on relation `{prop_name}` could not be compiled to \
                 SPARQL in full. A partial EXISTS would match rows the clause rejects, so it \
                 is declined outright and the query will return no rows."
            );
            return None;
        }
        body.extend(rebase_into_subquery(
            &compiled.patterns,
            &namespace,
            &target_var,
        ));
    }

    let keyword = if negate {
        "FILTER NOT EXISTS"
    } else {
        "FILTER EXISTS"
    };
    Some(format!("    {keyword} {{\n{}\n    }}", body.join("\n")))
}

#[cfg(test)]
mod relation_quantifier_tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::{
        flag, prop, relation, scalar_relation, shape, StaticShapeResolver,
    };
    use crate::perspectives::model_query::types::{ShapeRelation, WhereOps};

    fn wc(pairs: Vec<(&str, WhereCondition)>) -> BTreeMap<String, WhereCondition> {
        pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
    }

    fn ops_with(
        some: Option<Vec<(&str, WhereCondition)>>,
        none: Option<Vec<(&str, WhereCondition)>>,
    ) -> WhereCondition {
        WhereCondition::Ops(WhereOps {
            some: some.map(wc),
            none: none.map(wc),
            ..Default::default()
        })
    }

    /// A Post whose `comments` relation names Comment as its target class.
    fn post_shape() -> ModelShape {
        let mut s = shape(
            "Post",
            vec![
                prop("title", "we://title"),
                relation("comments", "we://comment"),
            ],
        );
        s.include_relations.push(ShapeRelation {
            name: "comments".to_string(),
            predicate: "we://comment".to_string(),
            direction: "forward".to_string(),
            kind: "hasMany".to_string(),
            max_count: None,
            target_class_name: "Comment".to_string(),
            target_class_uri: String::new(),
        });
        s
    }

    fn comment_shape() -> ModelShape {
        shape("Comment", vec![prop("body", "we://body")])
    }

    fn resolver_with_comment() -> StaticShapeResolver {
        let r = StaticShapeResolver::new();
        r.register("Comment", comment_shape());
        r
    }

    /// A Comment that declares its class the way a real model does — a required
    /// flag — so conformance has something to assert.
    fn flagged_comment_shape() -> ModelShape {
        shape(
            "Comment",
            vec![
                flag("type", "we://type", "we://comment"),
                prop("body", "we://body"),
            ],
        )
    }

    fn resolver_with_flagged_comment() -> StaticShapeResolver {
        let r = StaticShapeResolver::new();
        r.register("Comment", flagged_comment_shape());
        r
    }

    #[test]
    fn test_some_with_empty_clause_is_existence() {
        // "has at least one comment" — no target shape needed, so it compiles
        // even with no resolver available.
        let s = post_shape();
        let clause = wc(vec![("comments", ops_with(Some(vec![]), None))]);

        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("FILTER EXISTS"), "{sparql}");
        assert!(sparql.contains("?source <we://comment>"), "{sparql}");
    }

    #[test]
    fn test_none_with_empty_clause_is_absence() {
        let s = post_shape();
        let clause = wc(vec![("comments", ops_with(None, Some(vec![])))]);

        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.complete);
        assert!(compiled.patterns.join("\n").contains("FILTER NOT EXISTS"));
    }

    #[test]
    fn test_some_with_inner_clause_constrains_the_linked_record() {
        let s = post_shape();
        let r = resolver_with_comment();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![("body", WhereCondition::String("spam".to_string()))]),
                None,
            ),
        )]);

        let compiled = compile_where_clause(&clause, &s, Some(&r));

        assert!(compiled.complete, "the target shape resolves, so it pushes");
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("FILTER EXISTS"), "{sparql}");
        // The nested condition must be rebased onto the linked record's
        // variable, never left on ?source — otherwise it would constrain the
        // Post rather than the Comment.
        assert!(sparql.contains("we://body"), "{sparql}");
        let body_line = sparql
            .lines()
            .find(|l| l.contains("we://body"))
            .expect("a pattern for the nested condition");
        assert!(
            !body_line.contains("?source"),
            "nested condition must not be left on the outer subject: {body_line}"
        );
    }

    #[test]
    fn an_operator_beside_a_quantifier_declines() {
        // The EXISTS group carries the quantifier and nothing else, so a `not`
        // riding along would vanish. `matches_ops` fails closed on the
        // quantifier rather than applying the `not`, so nothing downstream
        // would apply it either.
        let s = post_shape();
        let r = resolver_with_comment();
        let clause = wc(vec![(
            "comments",
            WhereCondition::Ops(WhereOps {
                some: Some(BTreeMap::new()),
                not: Some(Value::String("we://c1".to_string())),
                ..Default::default()
            }),
        )]);
        assert!(!compile_where_clause(&clause, &s, Some(&r)).complete);
    }

    #[test]
    fn test_inner_clause_without_a_resolver_declines() {
        // Nothing can name Comment's properties without its shape, and a
        // partial EXISTS would match rows the full clause rejects.
        let s = post_shape();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![("body", WhereCondition::String("spam".to_string()))]),
                None,
            ),
        )]);

        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.patterns.is_empty());
        assert!(!compiled.complete, "must fall back rather than half-push");
    }

    #[test]
    fn test_inner_clause_that_cannot_compile_declines() {
        let s = post_shape();
        let r = resolver_with_comment();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![(
                    "unknownProp",
                    WhereCondition::String("x".to_string()),
                )]),
                None,
            ),
        )]);

        let compiled = compile_where_clause(&clause, &s, Some(&r));
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }

    #[test]
    fn test_reverse_relation_quantifier_reverses_the_triple() {
        // For a @BelongsTo relation the linked record points *at* this one, so
        // the existence pattern must have the target as subject.
        let mut s = post_shape();
        let mut rel = relation("mentions", "we://mention");
        rel.direction = Some("reverse".to_string());
        s.properties.push(rel);

        let clause = wc(vec![("mentions", ops_with(Some(vec![]), None))]);
        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("<we://mention> ?source ."), "{sparql}");
    }

    #[test]
    fn the_linked_record_must_conform_to_the_target_class() {
        // Being linked by `we://comment` is not being a Comment. Hydration
        // resolves the relation through Comment's own query, so a quantifier
        // that asks only about the predicate can match a Post whose `comments`
        // then comes back empty.
        let s = post_shape();
        let r = resolver_with_flagged_comment();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![("body", WhereCondition::String("spam".to_string()))]),
                None,
            ),
        )]);

        let compiled = compile_where_clause(&clause, &s, Some(&r));

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        let conformance = sparql
            .lines()
            .find(|l| l.contains("<we://type> <we://comment>"))
            .expect("the target class's flag must be asserted: {sparql}");
        // On the *linked* record, never the Post.
        assert!(
            !conformance.contains("?source"),
            "conformance must be rebased onto the linked record: {conformance}"
        );
    }

    #[test]
    fn conformance_helpers_are_namespaced_like_every_other_helper() {
        // A required non-flag property binds a helper variable. Left unprefixed
        // it would collide with the outer class's conformance variable of the
        // same name, silently requiring the Post and the Comment to share a
        // value for it.
        let s = post_shape();
        let r = StaticShapeResolver::new();
        let mut c = shape("Comment", vec![prop("body", "we://body")]);
        c.properties.push({
            let mut p = prop("title", "we://title");
            p.is_required = true;
            p
        });
        r.register("Comment", c);
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![("body", WhereCondition::String("spam".to_string()))]),
                None,
            ),
        )]);

        let sparql = compile_where_clause(&clause, &s, Some(&r))
            .patterns
            .join("\n");

        assert!(
            sparql.contains("<we://title> ?_q"),
            "the conformance helper must carry this quantifier's namespace: {sparql}"
        );
    }

    #[test]
    fn a_target_class_with_no_conformance_adds_nothing() {
        // The structural fallback is declined inside an EXISTS — the link triple
        // has already narrowed the subject, and a scan of every node sharing a
        // predicate would cost more than it excludes.
        let s = post_shape();
        let r = resolver_with_comment();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![("body", WhereCondition::String("spam".to_string()))]),
                None,
            ),
        )]);

        let sparql = compile_where_clause(&clause, &s, Some(&r))
            .patterns
            .join("\n");

        assert!(
            !sparql.contains("SELECT DISTINCT"),
            "no structural scan inside the EXISTS: {sparql}"
        );
    }

    #[test]
    fn a_nested_value_that_looks_like_a_variable_is_left_alone() {
        // Rebasing renames variables, and a value is not one. A comment whose
        // body is literally "?source" must still be searched for by that text —
        // rewriting inside the quotes would look for the linked record's
        // variable name instead and quietly return the wrong rows.
        let s = post_shape();
        let r = resolver_with_comment();
        let clause = wc(vec![(
            "comments",
            ops_with(
                Some(vec![(
                    "body",
                    WhereCondition::String("?source".to_string()),
                )]),
                None,
            ),
        )]);

        let compiled = compile_where_clause(&clause, &s, Some(&r));

        assert!(compiled.complete);
        let sparql = compiled.patterns.join("\n");
        assert!(
            sparql.contains("\"?source\""),
            "the literal value must survive rebasing: {sparql}"
        );
        // ...while the variable it shares a spelling with is still rebased: the
        // relation triple is the only place `?source` legitimately remains, and
        // that is the *outer* subject, above the rebased body.
        let body_line = sparql
            .lines()
            .find(|l| l.contains("we://body"))
            .expect("a pattern for the nested condition");
        assert!(
            !body_line.contains("?source <"),
            "the nested condition must not be left on the outer subject: {body_line}"
        );
    }

    #[test]
    fn rebasing_skips_variable_names_inside_iris() {
        // `?source` in a query component is part of the IRI, not a variable.
        let patterns = vec!["    ?source <we://p?source=1> ?_ft_x .".to_string()];

        let rebased = rebase_into_subquery(&patterns, "q0comments", "?_q0commentst");

        assert_eq!(
            rebased[0],
            "    ?_q0commentst <we://p?source=1> ?_q0commentsft_x ."
        );
    }

    #[test]
    fn rebasing_survives_an_escaped_quote_in_a_literal() {
        // An escaped quote does not end the literal; reading it as one would
        // put the rest of the value back into scanning range.
        let patterns =
            vec!["    FILTER(STR(?_ft_x) = \"a\\\"?source\") ?source <we://p> ?_r .".to_string()];

        let rebased = rebase_into_subquery(&patterns, "q0", "?_q0t");

        assert_eq!(
            rebased[0],
            "    FILTER(STR(?_q0ft_x) = \"a\\\"?source\") ?_q0t <we://p> ?_q0r ."
        );
    }

    #[test]
    fn a_to_one_relation_takes_a_quantifier_too() {
        // `{ author: { none: {} } }` on a `@HasOne` — "has nobody assigned" — is
        // the same question as `{ comments: { none: {} } }` on a `@HasMany`, and
        // is answered in the same place. `load_shape` marks every relation
        // `is_collection` whatever its cardinality (the pipeline hydrates them
        // all as arrays and `is_scalar_relation` unwraps the to-one ones
        // afterwards), so nothing routes this to the property arm, which would
        // produce no filter and leave the fail-closed post-hydration path to
        // return zero rows with no diagnostic.
        let mut s = post_shape();
        s.properties.push(scalar_relation("author", "we://author"));
        s.include_relations.push(ShapeRelation {
            name: "author".to_string(),
            predicate: "we://author".to_string(),
            direction: "forward".to_string(),
            kind: "hasOne".to_string(),
            max_count: Some(1),
            target_class_name: "Agent".to_string(),
            target_class_uri: String::new(),
        });

        let clause = wc(vec![("author", ops_with(None, Some(vec![])))]);
        let compiled = compile_where_clause(&clause, &s, None);

        assert!(compiled.complete, "cardinality does not gate a quantifier");
        let sparql = compiled.patterns.join("\n");
        assert!(sparql.contains("FILTER NOT EXISTS"), "{sparql}");
        assert!(sparql.contains("?source <we://author>"), "{sparql}");
    }

    #[test]
    fn a_getter_backed_relation_declines_rather_than_emitting_an_empty_iri() {
        // The same hole `test_getter_property_condition_is_not_pushable` closed
        // on the property branch: a getter-backed relation carries no predicate,
        // and `<>` parses as a relative IRI, so the clause reported itself
        // pushed and matched nothing.
        let mut s = post_shape();
        let mut rel = relation("computed", "");
        rel.getter = Some("SELECT ?target WHERE { <Base> ?p ?target }".to_string());
        s.properties.push(rel);

        let clause = wc(vec![(
            "computed",
            WhereCondition::String("anything".to_string()),
        )]);

        let compiled = compile_where_clause(&clause, &s, None);
        assert!(compiled.patterns.is_empty(), "nothing can be emitted");
        assert!(
            !compiled.complete,
            "so it must reach the post-hydration filter rather than be dropped"
        );
    }

    #[test]
    fn a_relation_with_an_unemittable_predicate_declines() {
        // Every arm of the relation branch drops the predicate into a triple
        // pattern, so it is validated once for the branch. Declining sends the
        // condition to the post-hydration filter; emitting would send malformed
        // SPARQL to the store.
        let mut s = post_shape();
        s.properties.push(relation("tags", "we://tags?<broken>"));

        let clause = wc(vec![(
            "tags",
            WhereCondition::String("not-an-iri".to_string()),
        )]);

        let compiled = compile_where_clause(&clause, &s, None);
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }

    #[test]
    fn test_some_and_none_together_is_refused() {
        // No single sensible reading; guessing one would be worse than
        // declining.
        let s = post_shape();
        let clause = wc(vec![("comments", ops_with(Some(vec![]), Some(vec![])))]);

        assert!(!compile_where_clause(&clause, &s, None).complete);
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
        assert!(compile_where_clause(&c, &s(), None).complete);
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
        let compiled = compile_where_clause(&c, &s(), None);
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
        assert!(!compile_where_clause(&c, &s(), None).complete);
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
            !compile_where_clause(&c, &s(), None).complete,
            "a dropped `not` must not be masked by a rendered `gt`",
        );
    }

    #[test]
    fn an_empty_ops_object_constrains_nothing() {
        // `{}` requests no operator. Emitting the triple pattern alone would narrow
        // to "the property exists", which is not what the Rust filter does for the
        // same clause — it matches everything.
        let c = wc(vec![("title", ops(WhereOps::default()))]);
        let compiled = compile_where_clause(&c, &s(), None);
        assert!(!compiled.complete);
        assert!(compiled.patterns.is_empty());
    }
}
