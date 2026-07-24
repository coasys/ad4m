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
                // STR() returns the lexical form for a typed literal and the
                // IRI text for a NamedNode — either way we get a comparable
                // value for the sort.  The xsd:double cast yields the numeric
                // sort key when the value parses as a number.
                format!(
                    r#"SELECT DISTINCT ?source (SAMPLE(?_nv) AS ?_sort_num) (SAMPLE(?_sv) AS ?_sort_str) WHERE {{
{conformance}
{where_extra}
            OPTIONAL {{ ?source <{predicate}> ?_sort_raw . BIND(STR(?_sort_raw) AS ?_sv) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR(?_sort_raw)) AS ?_nv) }}
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
            OPTIONAL {{ ?source <{rel_pred}> ?_rp_rel . OPTIONAL {{ ?_rp_rel <{prop_pred}> ?_rp_raw . BIND(STR(?_rp_raw) AS ?_rp_str_v) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR(?_rp_raw)) AS ?_rp_num_v) }} }}
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
/// Returns `true` when every condition targets either `id`/`base` (with
/// simple string values), a collection relation (with string/array values),
/// or a known scalar property.  When this returns `false`, post-hydration
/// Rust-side filtering is required for the remaining conditions.
pub(super) fn all_where_pushable(query: &ModelQueryInput, shape: &ModelShape) -> bool {
    let Some(ref wc) = query.where_clause else {
        return true;
    };
    for (prop_name, condition) in wc {
        // OR/AND/NOT are always evaluated Rust-side; SPARQL-level pagination
        // cannot be applied when they are present.
        if prop_name == "OR" || prop_name == "AND" || prop_name == "NOT" {
            return false;
        }

        if prop_name == "base" || prop_name == "id" {
            match condition {
                WhereCondition::String(_) | WhereCondition::StringArray(_) => continue,
                _ => return false,
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
            // OR/AND/NOT are evaluated Rust-side after hydration; skip SPARQL emission.
            if prop_name == "OR" || prop_name == "AND" || prop_name == "NOT" {
                continue;
            }

            if prop_name == "base" || prop_name == "id" {
                match condition {
                    WhereCondition::String(val) => {
                        if validate_iri(val).is_ok() {
                            where_patterns.push(format!("    FILTER(?source = <{val}>)"));
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
                    _ => {}
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
                let is_literal_prop = prop.resolve_language.is_some();
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
                        let var = format!("?_pw_{safe_name}");
                        let val_var = format!("?_pw_{safe_name}_v");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!("    BIND(STR({var}) AS {val_var})"));

                        let mut filters = Vec::new();

                        if let Some(ref not_val) = ops.not {
                            match not_val {
                                Value::String(s) => {
                                    filters.push(format!(
                                        "{val_var} != \"{}\"",
                                        escape_sparql_string(s)
                                    ));
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
                                    if !items.is_empty() {
                                        filters.push(format!(
                                            "{val_var} NOT IN ({})",
                                            items.join(", ")
                                        ));
                                    }
                                }
                                _ => {}
                            }
                        }

                        let has_numeric = ops.gt.is_some()
                            || ops.gte.is_some()
                            || ops.lt.is_some()
                            || ops.lte.is_some()
                            || ops.between.is_some();

                        if has_numeric {
                            let num_var = format!("?_pw_{safe_name}_num");
                            where_patterns.push(format!(
                                "    BIND(<http://www.w3.org/2001/XMLSchema#double>({val_var}) AS {num_var})"
                            ));
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

                        if !filters.is_empty() {
                            where_patterns.push(format!("    FILTER({})", filters.join(" && ")));
                        }
                    }
                    // SubClauses/SubClause are OR/AND/NOT combinators evaluated
                    // Rust-side; the outer loop skips them before reaching here.
                    WhereCondition::SubClauses(_) | WhereCondition::SubClause(_) => {}
                }
                continue;
            }
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::{flag, prop, shape};

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
