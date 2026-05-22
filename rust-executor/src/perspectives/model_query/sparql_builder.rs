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
use super::utils::{escape_sparql_string, validate_iri};

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
                format!(
                    r#"SELECT DISTINCT ?source (SAMPLE(?_nv) AS ?_sort_num) (SAMPLE(?_sv) AS ?_sort_str) WHERE {{
{conformance}
{where_extra}
            OPTIONAL {{ ?source <{predicate}> ?_sort_raw . BIND(SUBSTR(STR(?_sort_raw), 16) AS ?_sv) BIND(<http://www.w3.org/2001/XMLSchema#double>(SUBSTR(STR(?_sort_raw), 16)) AS ?_nv) }}
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
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns.push(format!(
                                "    FILTER(STR(<ad4m://fn/parse_literal>({var})) = \"{}\")",
                                escape_sparql_string(val)
                            ));
                        } else if validate_iri(val).is_ok() {
                            where_patterns
                                .push(format!("    ?source <{}> <{val}> .", prop.predicate));
                        } else {
                            let var = format!("?_pw_{safe_name}");
                            where_patterns
                                .push(format!("    ?source <{}> {var} .", prop.predicate));
                            where_patterns.push(format!(
                                "    FILTER(STR(<ad4m://fn/parse_literal>({var})) = \"{}\")",
                                escape_sparql_string(val)
                            ));
                        }
                    }
                    WhereCondition::Number(n) => {
                        let var = format!("?_pw_{safe_name}");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!(
                            "    FILTER(STR(<ad4m://fn/parse_literal>({var})) = \"{n}\")"
                        ));
                    }
                    WhereCondition::Bool(b) => {
                        let var = format!("?_pw_{safe_name}");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!(
                            "    FILTER(STR(<ad4m://fn/parse_literal>({var})) = \"{b}\")"
                        ));
                    }
                    WhereCondition::StringArray(vals) => {
                        let values_list = vals
                            .iter()
                            .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let var = format!("?_pw_{safe_name}");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!(
                            "    FILTER(STR(<ad4m://fn/parse_literal>({var})) IN ({values_list}))"
                        ));
                    }
                    WhereCondition::NumberArray(vals) => {
                        let values_list = vals
                            .iter()
                            .map(|n| format!("\"{n}\""))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let var = format!("?_pw_{safe_name}");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!(
                            "    FILTER(STR(<ad4m://fn/parse_literal>({var})) IN ({values_list}))"
                        ));
                    }
                    WhereCondition::Ops(ops) => {
                        let var = format!("?_pw_{safe_name}");
                        let val_var = format!("?_pw_{safe_name}_v");
                        where_patterns.push(format!("    ?source <{}> {var} .", prop.predicate));
                        where_patterns.push(format!(
                            "    BIND(STR(<ad4m://fn/parse_literal>({var})) AS {val_var})"
                        ));

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
                }
                continue;
            }
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}
