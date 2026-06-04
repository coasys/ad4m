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

use super::types::{
    InstanceQueryPlan, ModelQueryInput, ModelShape, OrderDirection, ParentScope, SortKey,
    SparqlPagination, WhereCondition,
};
use super::utils::{
    escape_sparql_string, format_literal_number, looks_like_absolute_iri, validate_iri,
};

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";

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

/// Build the pagination sub-query for TwoPhase plans, supporting arbitrary
/// multi-key ORDER BY across reifier-timestamp and scalar-property keys.
///
/// The sub-query always returns `?source` for each matching instance plus
/// one or more sort columns:
///   - reifier-timestamp keys contribute a single `?_first_ts` column,
///   - each property key contributes `?_sort_num_i` + `?_sort_str_i` via
///     SAMPLE aggregation under `GROUP BY ?source`.
///
/// When the sort spec contains any property keys, the sub-query uses
/// `GROUP BY ?source` (required for SAMPLE).  When it's pure-timestamp,
/// it uses `SELECT DISTINCT` for efficiency.
fn build_pagination_subquery(
    shape: &ModelShape,
    pg: &SparqlPagination,
    conformance: &str,
    where_extra: &str,
) -> String {
    let needs_timestamp = pg
        .sort_keys
        .iter()
        .any(|(k, _)| matches!(k, SortKey::Timestamp));
    let property_keys: Vec<(usize, &str, OrderDirection)> = pg
        .sort_keys
        .iter()
        .enumerate()
        .filter_map(|(i, (k, d))| match k {
            SortKey::Property(p) => Some((i, p.as_str(), *d)),
            SortKey::Timestamp => None,
        })
        .collect();

    let mut select_columns = vec!["?source".to_string()];
    let mut probe = String::new();
    let mut optionals = String::new();

    if needs_timestamp {
        let ts_probe = build_timestamp_probe(shape);
        probe.push_str(&format!("            {ts_probe}\n"));
        // ?_first_ts is bare in the timestamp-only case; under GROUP BY we
        // need to wrap it in SAMPLE so SPARQL accepts the projection.
        if property_keys.is_empty() {
            select_columns.push("?_first_ts".to_string());
        } else {
            select_columns.push("(SAMPLE(?_first_ts) AS ?_first_ts)".to_string());
        }
    }

    for (i, predicate, _) in &property_keys {
        let raw = format!("?_sort_raw_{i}");
        let sv = format!("?_sort_str_{i}");
        let nv = format!("?_sort_num_{i}");
        // STR() returns the lexical form for a typed literal and the IRI
        // text for a NamedNode — either way we get a comparable value.
        // xsd:double yields the numeric sort key when the value parses as
        // a number, NULL otherwise.
        optionals.push_str(&format!(
            "            OPTIONAL {{ ?source <{predicate}> {raw} . BIND(STR({raw}) AS {sv}) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR({raw})) AS {nv}) }}\n"
        ));
        select_columns.push(format!("(SAMPLE({nv}) AS ?_sort_num_{i})"));
        select_columns.push(format!("(SAMPLE({sv}) AS ?_sort_str_{i})"));
    }

    let order_by_terms: Vec<String> = pg
        .sort_keys
        .iter()
        .enumerate()
        .map(|(i, (key, dir))| match key {
            SortKey::Timestamp => match dir {
                OrderDirection::ASC => "ASC(?_first_ts)".to_string(),
                OrderDirection::DESC => "DESC(?_first_ts)".to_string(),
            },
            SortKey::Property(_) => {
                let d = match dir {
                    OrderDirection::ASC => "ASC",
                    OrderDirection::DESC => "DESC",
                };
                // Push unbound (NULL) values to the end regardless of dir,
                // sort numeric column first, fall back to string column.
                format!(
                    "ASC(IF(BOUND(?_sort_str_{i}), 0, 1)) {d}(?_sort_num_{i}) {d}(?_sort_str_{i})"
                )
            }
        })
        .collect();

    let mut suffix = String::new();
    if !order_by_terms.is_empty() {
        suffix.push_str("\n    ORDER BY ");
        suffix.push_str(&order_by_terms.join(" "));
    }
    if let Some(offset) = pg.offset {
        if offset > 0 {
            suffix.push_str(&format!("\n    OFFSET {offset}"));
        }
    }
    if let Some(limit) = pg.limit {
        suffix.push_str(&format!("\n    LIMIT {limit}"));
    }

    if property_keys.is_empty() {
        // Pure-timestamp (or no sort keys) → DISTINCT, no GROUP BY
        format!(
            r#"SELECT DISTINCT {cols} WHERE {{
{conformance}
{where_extra}
{probe}        }}{suffix}"#,
            cols = select_columns.join(" "),
            conformance = conformance,
            where_extra = where_extra,
            probe = probe,
            suffix = suffix,
        )
    } else {
        // Property sort → GROUP BY ?source for SAMPLE
        format!(
            r#"SELECT {cols} WHERE {{
{conformance}
{where_extra}
{probe}{optionals}        }} GROUP BY ?source{suffix}"#,
            cols = select_columns.join(" "),
            conformance = conformance,
            where_extra = where_extra,
            probe = probe,
            optionals = optionals,
            suffix = suffix,
        )
    }
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

    if let Some(pg) = sparql_pagination {
        let subquery_body = build_pagination_subquery(shape, pg, &conformance, &where_extra);
        // The two-phase property fetch derives its own predicate filter via
        // `build_predicate_filter_for_property_fetch(shape)` in query.rs.
        let _ = predicate_filter;
        InstanceQueryPlan::TwoPhase {
            pagination_subquery: subquery_body,
        }
    } else {
        // Property rows: hydration's Rust fold handles per-scalar
        // last-write-wins. Pushing the LWW (MAX(?ts) GROUP BY ?source
        // ?predicate) subquery into the main SELECT was tried but regressed
        // `test_perf_*` benchmarks by >10× — Oxigraph 0.5's planner joins
        // the outer pattern across all triples before applying the nested
        // aggregate. The per-row Rust fold over already-fetched rows is
        // cheaper than the planner cliff, and stays correct as long as
        // every reifier's timestamp is returned (which it is).
        let with_metadata = query.with_metadata.unwrap_or(true);
        let sparql = if with_metadata {
            format!(
                r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
{conformance}
{where_extra}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
            )
        } else {
            // No reifier-metadata join: the caller opted out of
            // author/createdAt/updatedAt and accepts last-row-wins-by-
            // insertion-order hydration. Saves three triple-pattern matches
            // per result row.
            format!(
                r#"SELECT ?source ?predicate ?target WHERE {{
{conformance}
{where_extra}
{predicate_filter}    ?source ?predicate ?target .
    FILTER(isIRI(?source) && isIRI(?predicate))
}}"#
            )
        };
        InstanceQueryPlan::Single(sparql)
    }
}

/// Derive the `VALUES ?predicate { ... }` filter used by the two-phase
/// property fetch.  Skips collection predicates that have a getter (those are
/// resolved separately) but otherwise mirrors the single-phase filter.
pub(super) fn build_predicate_filter_for_property_fetch(shape: &ModelShape) -> String {
    let needed: std::collections::BTreeSet<&str> = shape
        .properties
        .iter()
        .filter(|p| !p.predicate.is_empty())
        .filter(|p| !(p.is_collection && p.getter.is_some()))
        .map(|p| p.predicate.as_str())
        .collect();
    if needed.is_empty() {
        String::new()
    } else {
        let values: String = needed
            .iter()
            .map(|p| format!("<{p}>"))
            .collect::<Vec<_>>()
            .join(" ");
        format!("    VALUES ?predicate {{ {values} }}\n")
    }
}

/// Build a per-source `(createdAt, updatedAt)` aggregate query using the
/// conformance + where patterns as the source scope.  Returns `None` when
/// both patterns are empty (the aggregate would scan every reifier — strictly
/// worse than the Rust fold).
///
/// `createdAt` is the minimum reifier timestamp for any link with that source,
/// `updatedAt` the maximum.  Reifier timestamps are stored as `xsd:dateTime`
/// (post-v5 migration) so `MIN` / `MAX` use native datetime semantics.
///
/// **Note:** under Oxigraph 0.5.x the bounded-`VALUES` aggregate still costs
/// 150–300 ms on the medium-tier benchmark because the planner walks every
/// reifier when matching the triple-term pattern, so the production pipeline
/// keeps the per-row Rust fold in `hydrate_one` (which sees every link the
/// main property query returns).  This builder is exercised by integration
/// tests and is kept available so a future planner improvement can flip the
/// aggregate back on without re-deriving the WHERE shape.
#[cfg_attr(not(test), allow(dead_code))]
pub(super) fn build_aggregate_sparql(
    shape: &ModelShape,
    query: &ModelQueryInput,
) -> Option<String> {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    if conformance.trim().is_empty() && where_extra.trim().is_empty() {
        return None;
    }

    Some(format!(
        r#"SELECT ?source (MIN(?_ts) AS ?createdAt) (MAX(?_ts) AS ?updatedAt) WHERE {{
{conformance}
{where_extra}
    ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?_p ?_t )>> .
    ?_r <ad4m://ontology/timestamp> ?_ts .
}} GROUP BY ?source"#
    ))
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
/// Returns `true` when every condition targets either `id`/`base` (with
/// simple string values), a collection relation (with string/array values),
/// or a known scalar property.  When this returns `false`, post-hydration
/// Rust-side filtering is required for the remaining conditions.
/// Returns the upper-bound size of the matching `?source` set implied by
/// the WHERE clause's id/base equality conditions, or `None` if no such
/// bound exists.
///
/// Used by the query planner to detect cases where TwoPhase pagination is
/// pure overhead: when WHERE pins `id` to a single IRI (returns Some(1))
/// or to a finite `StringArray` (returns Some(len)), the source set is
/// already bounded by the WHERE filter alone and the timestamp probe in
/// phase 1 adds work without changing the result.
pub(super) fn where_clause_max_source_count(query: &ModelQueryInput) -> Option<usize> {
    let wc = query.where_clause.as_ref()?;
    let mut min: Option<usize> = None;
    for (prop_name, condition) in wc {
        if prop_name != "id" && prop_name != "base" {
            continue;
        }
        let cap = match condition {
            WhereCondition::String(_)
            | WhereCondition::Number(_)
            | WhereCondition::Bool(_) => Some(1),
            WhereCondition::StringArray(arr) => Some(arr.len()),
            WhereCondition::NumberArray(arr) => Some(arr.len()),
            // `Ops` (gt/lt/between/contains/not) can match an unbounded set.
            WhereCondition::Ops(_) => None,
        };
        if let Some(cap) = cap {
            min = Some(min.map(|m| m.min(cap)).unwrap_or(cap));
        }
    }
    min
}

/// Cheap variant of [`where_clause_max_source_count`] that just answers
/// "is the result set provably bounded by WHERE?" without computing the
/// exact bound.
pub(super) fn where_clause_caps_result_size(query: &ModelQueryInput) -> bool {
    where_clause_max_source_count(query).is_some()
}

pub(super) fn all_where_pushable(query: &ModelQueryInput, shape: &ModelShape) -> bool {
    let Some(ref wc) = query.where_clause else {
        return true;
    };
    for (prop_name, condition) in wc {
        if prop_name == "base" || prop_name == "id" {
            match condition {
                WhereCondition::String(_)
                | WhereCondition::StringArray(_)
                | WhereCondition::Number(_)
                | WhereCondition::Bool(_)
                | WhereCondition::NumberArray(_)
                | WhereCondition::Ops(_) => continue,
            }
        }
        if shape
            .properties
            .iter()
            .any(|p| p.name == *prop_name && p.is_collection)
        {
            if matches!(
                condition,
                WhereCondition::String(_) | WhereCondition::StringArray(_)
            ) {
                continue;
            }
            return false;
        }
        if shape
            .properties
            .iter()
            .any(|p| p.name == *prop_name && !p.is_collection)
        {
            continue;
        }
        return false;
    }
    true
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
            ParentScope::Raw { id, predicate } => {
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
            ParentScope::Model { id, field, model } => {
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
    let mut where_patterns = Vec::new();
    if let Some(ref wc) = query.where_clause {
        for (prop_name, condition) in wc {
            if prop_name == "base" || prop_name == "id" {
                match condition {
                    WhereCondition::String(val) => {
                        if validate_iri(val).is_ok() {
                            // Use VALUES ?source instead of FILTER(?source = <iri>):
                            // the planner can pin ?source to the bound IRI before
                            // joining with the rest of the WHERE, which on Oxigraph's
                            // 0.5 planner is materially cheaper at scale than a
                            // post-join FILTER.
                            where_patterns.push(format!("    VALUES ?source {{ <{val}> }}"));
                        } else {
                            where_patterns.push(format!(
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
                            where_patterns.push(format!("    VALUES ?source {{ {iris} }}"));
                        } else {
                            let ids = vals
                                .iter()
                                .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                                .collect::<Vec<_>>()
                                .join(", ");
                            where_patterns.push(format!("    FILTER(STR(?source) IN ({ids}))"));
                        }
                    }
                    WhereCondition::Number(n) => {
                        // id/base values are stored as IRIs; compare their
                        // lexical form against the number's canonical string.
                        if let Some(s) = format_literal_number(*n) {
                            where_patterns
                                .push(format!("    FILTER(STR(?source) = \"{s}\")"));
                        } else {
                            where_patterns.push("    FILTER(false)".to_string());
                        }
                    }
                    WhereCondition::Bool(b) => {
                        where_patterns.push(format!("    FILTER(STR(?source) = \"{b}\")"));
                    }
                    WhereCondition::NumberArray(vals) => {
                        let ids: Vec<String> = vals
                            .iter()
                            .filter_map(|n| format_literal_number(*n))
                            .map(|s| format!("\"{s}\""))
                            .collect();
                        if ids.is_empty() {
                            where_patterns.push("    FILTER(false)".to_string());
                        } else {
                            where_patterns.push(format!(
                                "    FILTER(STR(?source) IN ({}))",
                                ids.join(", ")
                            ));
                        }
                    }
                    WhereCondition::Ops(ops) => {
                        // Operate on the IRI's string form so range / contains
                        // comparisons work uniformly regardless of whether the
                        // value is a plain string or a stringified number.
                        where_patterns.push("    BIND(STR(?source) AS ?_id_str)".to_string());
                        let var = "?_id_str";
                        let mut filters: Vec<String> = Vec::new();

                        if let Some(ref not_val) = ops.not {
                            match not_val {
                                Value::String(s) => {
                                    if validate_iri(s).is_ok() {
                                        filters.push(format!("?source != <{s}>"));
                                    } else {
                                        let escaped = escape_sparql_string(s);
                                        filters.push(format!("{var} != \"{escaped}\""));
                                    }
                                }
                                Value::Number(n) => {
                                    let f = n.as_f64().unwrap_or(0.0);
                                    if let Some(s) = format_literal_number(f) {
                                        filters.push(format!("{var} != \"{s}\""));
                                    }
                                }
                                Value::Bool(b) => {
                                    filters.push(format!("{var} != \"{b}\""));
                                }
                                Value::Array(arr) => {
                                    let items: Vec<String> = arr
                                        .iter()
                                        .filter_map(|item| match item {
                                            Value::String(s) => Some(format!(
                                                "\"{}\"",
                                                escape_sparql_string(s)
                                            )),
                                            Value::Number(n) => {
                                                let f = n.as_f64().unwrap_or(0.0);
                                                format_literal_number(f)
                                                    .map(|s| format!("\"{s}\""))
                                            }
                                            Value::Bool(b) => Some(format!("\"{b}\"")),
                                            _ => None,
                                        })
                                        .collect();
                                    if !items.is_empty() {
                                        filters.push(format!(
                                            "{var} NOT IN ({})",
                                            items.join(", ")
                                        ));
                                    }
                                }
                                _ => {}
                            }
                        }

                        if let Some(gt) = ops.gt {
                            if let Some(s) = format_literal_number(gt) {
                                filters.push(format!("{var} > \"{s}\""));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(gte) = ops.gte {
                            if let Some(s) = format_literal_number(gte) {
                                filters.push(format!("{var} >= \"{s}\""));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(lt) = ops.lt {
                            if let Some(s) = format_literal_number(lt) {
                                filters.push(format!("{var} < \"{s}\""));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(lte) = ops.lte {
                            if let Some(s) = format_literal_number(lte) {
                                filters.push(format!("{var} <= \"{s}\""));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some((lo, hi)) = ops.between {
                            match (format_literal_number(lo), format_literal_number(hi)) {
                                (Some(lo_s), Some(hi_s)) => {
                                    filters.push(format!(
                                        "{var} >= \"{lo_s}\" && {var} <= \"{hi_s}\""
                                    ));
                                }
                                _ => filters.push("false".to_string()),
                            }
                        }

                        if let Some(ref contains_val) = ops.contains {
                            let needle = match contains_val {
                                Value::String(s) => s.clone(),
                                other => other.to_string(),
                            };
                            filters.push(format!(
                                "CONTAINS(LCASE({var}), LCASE(\"{}\"))",
                                escape_sparql_string(&needle)
                            ));
                        }

                        if !filters.is_empty() {
                            where_patterns.push(format!("    FILTER({})", filters.join(" && ")));
                        }
                    }
                }
                continue;
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
                                where_patterns
                                    .push(format!("    <{val}> <{}> ?source .", prop.predicate));
                            } else {
                                where_patterns
                                    .push(format!("    ?source <{}> <{val}> .", prop.predicate));
                            }
                        } else {
                            let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                            let escaped = escape_sparql_string(val);
                            if direction == "reverse" {
                                where_patterns.push(format!(
                                    "    ?_rv_{safe_name} <{}> ?source . FILTER(STR(?_rv_{safe_name}) = \"{escaped}\")",
                                    prop.predicate
                                ));
                            } else {
                                where_patterns.push(format!(
                                    "    ?source <{}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) = \"{escaped}\")",
                                    prop.predicate
                                ));
                            }
                        }
                    }
                    WhereCondition::StringArray(vals) => {
                        let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                        let all_valid = vals.iter().all(|v| validate_iri(v).is_ok());
                        if all_valid {
                            let iris = vals
                                .iter()
                                .map(|v| format!("<{v}>"))
                                .collect::<Vec<_>>()
                                .join(" ");
                            if direction == "reverse" {
                                where_patterns.push(format!(
                                    "    VALUES ?_rv_{safe_name} {{ {iris} }}\n    ?_rv_{safe_name} <{}> ?source .",
                                    prop.predicate
                                ));
                            } else {
                                where_patterns.push(format!(
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
                                where_patterns.push(format!(
                                    "    ?_rv_{safe_name} <{}> ?source . FILTER(STR(?_rv_{safe_name}) IN ({str_list}))",
                                    prop.predicate
                                ));
                            } else {
                                where_patterns.push(format!(
                                    "    ?source <{}> ?_ft_{safe_name} . FILTER(STR(?_ft_{safe_name}) IN ({str_list}))",
                                    prop.predicate
                                ));
                            }
                        }
                    }
                    _ => {}
                }
                continue;
            }

            // Property-based where
            if let Some(prop) = shape
                .properties
                .iter()
                .find(|p| &p.name == prop_name && !p.is_collection && !p.predicate.is_empty())
            {
                if validate_iri(&prop.predicate).is_err() {
                    continue;
                }
                let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                // Only `resolveLanguage: "literal"` stores deterministic
                // `literal:*` targets we can probe directly. Other resolvers
                // wrap values in author-signed expression IRIs, so they fall
                // through to the FILTER-on-decoded path below.
                let is_literal_prop = prop.resolve_language.as_deref() == Some("literal");
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
                                where_patterns.push(format!(
                                    "    {{ ?source <{0}> \"{escaped}\"^^<{XSD_STRING}> . }} UNION {{ ?source <{0}> <{val}> . }}",
                                    prop.predicate
                                ));
                            } else {
                                where_patterns.push(format!(
                                    "    ?source <{}> \"{escaped}\"^^<{XSD_STRING}> .",
                                    prop.predicate
                                ));
                            }
                        } else if validate_iri(val).is_ok() {
                            where_patterns
                                .push(format!("    ?source <{}> <{val}> .", prop.predicate));
                        } else {
                            // Raw-URI property where the value isn't IRI-shaped
                            // — match against a plain string literal so we
                            // accept either a NamedNode whose IRI text equals
                            // the value or a typed literal carrying it.
                            let var = format!("?_pw_{safe_name}");
                            let escaped = escape_sparql_string(val);
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns.push(format!("    FILTER(STR({var}) = \"{escaped}\")"));
                        }
                    }
                    WhereCondition::Number(n) => {
                        if is_literal_prop {
                            if let Some(typed) = typed_number_literal(*n) {
                                where_patterns
                                    .push(format!("    ?source <{}> {typed} .", prop.predicate));
                            } else {
                                where_patterns.push("    FILTER(false)".to_string());
                            }
                        } else {
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns.push(format!("    FILTER(STR({var}) = \"{n}\")"));
                        }
                    }
                    WhereCondition::Bool(b) => {
                        if is_literal_prop {
                            where_patterns.push(format!(
                                "    ?source <{}> \"{b}\"^^<{XSD_BOOLEAN}> .",
                                prop.predicate
                            ));
                        } else {
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns.push(format!("    FILTER(STR({var}) = \"{b}\")"));
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
                            where_patterns
                                .push(format!("    VALUES {iv_var} {{ {} }}", items.join(" ")));
                            where_patterns
                                .push(format!("    ?source <{}> {iv_var} .", prop.predicate));
                        } else {
                            let values_list = vals
                                .iter()
                                .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                                .collect::<Vec<_>>()
                                .join(", ");
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns
                                .push(format!("    FILTER(STR({var}) IN ({values_list}))"));
                        }
                    }
                    WhereCondition::NumberArray(vals) => {
                        if is_literal_prop {
                            let items: Vec<String> = vals
                                .iter()
                                .filter_map(|n| typed_number_literal(*n))
                                .collect();
                            if items.is_empty() {
                                where_patterns.push("    FILTER(false)".to_string());
                            } else {
                                let iv_var = format!("?_iv_{safe_name}");
                                where_patterns
                                    .push(format!("    VALUES {iv_var} {{ {} }}", items.join(" ")));
                                where_patterns
                                    .push(format!("    ?source <{}> {iv_var} .", prop.predicate));
                            }
                        } else {
                            let values_list = vals
                                .iter()
                                .map(|n| format!("\"{n}\""))
                                .collect::<Vec<_>>()
                                .join(", ");
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns
                                .push(format!("    FILTER(STR({var}) IN ({values_list}))"));
                        }
                    }
                    WhereCondition::Ops(ops) => {
                        // Bind the raw typed-literal target; comparisons run
                        // directly against `?val` using SPARQL's native xsd
                        // datatype semantics (no `fn/parse_literal` hop).
                        let var = format!("?_pw_{safe_name}");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));

                        let mut filters = Vec::new();

                        if let Some(ref not_val) = ops.not {
                            match not_val {
                                Value::String(s) => {
                                    let escaped = escape_sparql_string(s);
                                    filters.push(format!("{var} != \"{escaped}\"^^<{XSD_STRING}>"));
                                }
                                Value::Number(n) => {
                                    let n_f64 = n.as_f64().unwrap_or(0.0);
                                    if let Some(typed) = typed_number_literal(n_f64) {
                                        filters.push(format!("{var} != {typed}"));
                                    }
                                }
                                Value::Bool(b) => {
                                    filters.push(format!("{var} != \"{b}\"^^<{XSD_BOOLEAN}>"));
                                }
                                Value::Array(arr) => {
                                    let items: Vec<String> = arr
                                        .iter()
                                        .filter_map(|item| match item {
                                            Value::String(s) => Some(format!(
                                                "\"{}\"^^<{XSD_STRING}>",
                                                escape_sparql_string(s)
                                            )),
                                            Value::Number(n) => {
                                                let f = n.as_f64().unwrap_or(0.0);
                                                typed_number_literal(f)
                                            }
                                            Value::Bool(b) => {
                                                Some(format!("\"{b}\"^^<{XSD_BOOLEAN}>"))
                                            }
                                            _ => None,
                                        })
                                        .collect();
                                    if !items.is_empty() {
                                        filters
                                            .push(format!("{var} NOT IN ({})", items.join(", ")));
                                    }
                                }
                                _ => {}
                            }
                        }

                        if let Some(gt) = ops.gt {
                            if let Some(typed) = typed_number_literal(gt) {
                                filters.push(format!("{var} > {typed}"));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(gte) = ops.gte {
                            if let Some(typed) = typed_number_literal(gte) {
                                filters.push(format!("{var} >= {typed}"));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(lt) = ops.lt {
                            if let Some(typed) = typed_number_literal(lt) {
                                filters.push(format!("{var} < {typed}"));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some(lte) = ops.lte {
                            if let Some(typed) = typed_number_literal(lte) {
                                filters.push(format!("{var} <= {typed}"));
                            } else {
                                filters.push("false".to_string());
                            }
                        }
                        if let Some((lo, hi)) = ops.between {
                            match (typed_number_literal(lo), typed_number_literal(hi)) {
                                (Some(lo_t), Some(hi_t)) => {
                                    filters.push(format!("{var} >= {lo_t} && {var} <= {hi_t}"));
                                }
                                _ => filters.push("false".to_string()),
                            }
                        }

                        if let Some(ref contains_val) = ops.contains {
                            let needle = match contains_val {
                                Value::String(s) => s.clone(),
                                other => other.to_string(),
                            };
                            filters.push(format!(
                                "CONTAINS(LCASE(STR({var})), LCASE(\"{}\"))",
                                escape_sparql_string(&needle)
                            ));
                        }

                        if !filters.is_empty() {
                            where_patterns.push(format!("    FILTER({})", filters.join(" && ")));
                        }
                    }
                }
                continue;
            }
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}
