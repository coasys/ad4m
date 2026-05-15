//! SPARQL getter evaluation for properties and relations.
//!
//! Some model properties and relations are not stored as simple triples but
//! are instead defined by custom SPARQL expressions (the `getter` field in
//! the shape metadata, corresponding to the `@Property({ getter: "..." })`
//! or `@HasMany({ getter: "..." })` decorators in TypeScript).
//!
//! This module evaluates those getters efficiently using **batched `VALUES`
//! queries** — a single SPARQL query handles all instances at once by
//! injecting `VALUES ?source { <id1> <id2> ... }`.
//!
//! Two getter forms are supported:
//! - **`ASK`** — Converted to a `SELECT ?source` that returns which sources
//!   match, producing a boolean per instance.
//! - **`SELECT`** — Augmented with a `VALUES ?source` clause, producing
//!   string values (scalar or array) per instance.
//!
//! After getter evaluation, optional post-getter where-clause filters
//! ([`apply_where_filter_to_relation`]) can further narrow down relation
//! results based on target instance properties.

use deno_core::anyhow::Error;
use serde_json::{Map, Value};
use std::collections::{BTreeMap, HashMap};

use super::filtering::matches_condition;
use super::shape::parse_shape_from_json;
use super::types::{IncludeValue, ModelShape, ShapeProperty};
use super::utils::{parse_literal_value, validate_iri};
use crate::perspectives::sparql_store::SparqlStore;

/// Evaluate property getters for a batch of instances, returning only the
/// getter-computed properties.
///
/// This is the public entry point used by `perspective_instance.rs` when
/// the client requests a targeted getter evaluation (as opposed to the
/// full query pipeline).  Returns a map of `instance_id → { prop: value }`.
pub fn evaluate_getters_batch(
    store: &SparqlStore,
    class_name: &str,
    instance_ids: &[String],
    property_names: Option<&[String]>,
    shape_json: Option<&str>,
) -> Result<Value, Error> {
    if instance_ids.is_empty() {
        return Ok(Value::Object(Map::new()));
    }

    let shape = if let Some(json) = shape_json {
        parse_shape_from_json(json, class_name)?
    } else {
        return Err(deno_core::anyhow::anyhow!(
            "shape_json is required for evaluate_getters_batch"
        ));
    };

    let getter_props: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| {
            p.getter.is_some()
                && property_names
                    .map(|names| names.iter().any(|n| n == &p.name))
                    .unwrap_or(true)
        })
        .collect();

    if getter_props.is_empty() {
        return Ok(Value::Object(Map::new()));
    }

    let mut instances: Vec<Value> = instance_ids
        .iter()
        .map(|id| {
            let mut obj = Map::new();
            obj.insert("id".to_string(), Value::String(id.clone()));
            Value::Object(obj)
        })
        .collect();

    evaluate_getters(store, &mut instances, &shape, None, true)?;

    let mut result = Map::new();
    for inst in &instances {
        if let Some(id) = inst.get("id").and_then(|v| v.as_str()) {
            let mut props = Map::new();
            for prop in &getter_props {
                if let Some(val) = inst.get(&prop.name) {
                    if val != &Value::Null {
                        props.insert(prop.name.clone(), val.clone());
                    }
                }
            }
            if !props.is_empty() {
                result.insert(id.to_string(), Value::Object(props));
            }
        }
    }

    Ok(Value::Object(result))
}

/// Strip a trailing top-level `LIMIT N` clause from a SPARQL query string.
///
/// Getter expressions may include their own `LIMIT 1` for scalar results.
/// When batching, we need to remove this limit because the batched query
/// returns results for multiple sources.
pub(super) fn strip_trailing_limit(query: &str) -> String {
    let trimmed = query.trim_end();
    let upper = trimmed.to_uppercase();
    if let Some(limit_pos) = upper.rfind("LIMIT") {
        let after_limit = trimmed[limit_pos + 5..].trim();
        if !after_limit.is_empty() && after_limit.chars().all(|c| c.is_ascii_digit()) {
            return trimmed[..limit_pos].trim_end().to_string();
        }
    }
    trimmed.to_string()
}

/// Convert an `ASK` getter to a batched `SELECT` returning matching source IRIs.
///
/// Replaces `<Base>` with `?source`, extracts the body between `{ }`, and
/// wraps it in `SELECT ?source WHERE { VALUES ?source { ... } <body> }`.
pub(super) fn convert_ask_to_batched_select(ask: &str, values_clause: &str) -> String {
    let normalized = ask.replace("<Base>", "?source");
    if let (Some(open), Some(close)) = (normalized.find('{'), normalized.rfind('}')) {
        let body = &normalized[open + 1..close];
        format!(
            "SELECT ?source WHERE {{ VALUES ?source {{ {} }} {} }}",
            values_clause,
            body.trim()
        )
    } else {
        normalized
    }
}

/// Inject a `VALUES ?source` clause into a `SELECT` getter for batching.
///
/// Performs three transformations:
/// 1. Replaces `<Base>` with `?source`.
/// 2. Strips any trailing `LIMIT N` (see [`strip_trailing_limit`]).
/// 3. Ensures `?source` is in the projection and adds `VALUES ?source { ... }`
///    inside the first `{`.
pub(super) fn inject_values_into_select(select: &str, values_clause: &str) -> String {
    let mut query = select.replace("<Base>", "?source");

    query = strip_trailing_limit(&query);

    let upper = query.to_uppercase();
    if let Some(select_end) = upper.find("SELECT").map(|p| p + 6) {
        if let Some(where_rel) = upper[select_end..].find("WHERE") {
            let projection = &query[select_end..select_end + where_rel];
            if !projection.contains("?source") {
                query.insert_str(select_end, " ?source");
            }
        }
    }

    if let Some(brace_pos) = query.find('{') {
        let insert = format!(" VALUES ?source {{ {values_clause} }}");
        query.insert_str(brace_pos + 1, &insert);
    }

    query
}

/// Evaluate SPARQL getters on all instances using batched `VALUES` queries.
///
/// For each property/relation with a `getter` expression:
/// - `ASK` getters are converted to batched `SELECT`s → boolean result.
/// - `SELECT` getters are injected with `VALUES ?source` → string result(s).
///
/// When `deep_query` is `false`, only relation conformance getters (collections
/// and scalar relations) are evaluated, skipping pure-property getters for
/// performance.
pub(super) fn evaluate_getters(
    store: &SparqlStore,
    instances: &mut [Value],
    shape: &ModelShape,
    _include: Option<&HashMap<String, IncludeValue>>,
    deep_query: bool,
) -> Result<(), Error> {
    let getter_props: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.getter.is_some() && (deep_query || p.is_collection || p.is_scalar_relation))
        .collect();

    if getter_props.is_empty() || instances.is_empty() {
        return Ok(());
    }

    let instance_iris: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst.get("id").and_then(|v| v.as_str()))
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if instance_iris.is_empty() {
        return Ok(());
    }

    let values_clause = instance_iris
        .iter()
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(" ");

    log::debug!(
        "evaluate_getters: {} getter props for {} instances (batched VALUES)",
        getter_props.len(),
        instance_iris.len()
    );

    for prop in &getter_props {
        let getter = prop.getter.as_ref().unwrap();
        let upper = getter.trim().to_uppercase();

        if upper.starts_with("ASK") {
            let batched = convert_ask_to_batched_select(getter, &values_clause);
            match store.query(&batched) {
                Ok(result_json) => {
                    let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();
                    let matched: std::collections::HashSet<&str> = rows
                        .iter()
                        .filter_map(|row| row.get("source").and_then(|v| v.as_str()))
                        .collect();
                    for inst in instances.iter_mut() {
                        let id_owned = inst
                            .get("id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        if let Some(id) = id_owned {
                            if let Some(obj) = inst.as_object_mut() {
                                obj.insert(
                                    prop.name.clone(),
                                    Value::Bool(matched.contains(id.as_str())),
                                );
                            }
                        }
                    }
                }
                Err(e) => {
                    log::warn!("Batched ASK getter failed for '{}': {}", prop.name, e);
                }
            }
        } else if upper.starts_with("SELECT") {
            let batched = inject_values_into_select(getter, &values_clause);

            match store.query(&batched) {
                Ok(result_json) => {
                    let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();

                    let mut grouped: HashMap<String, Vec<String>> = HashMap::new();
                    for row in &rows {
                        let source = match row.get("source").and_then(|v| v.as_str()) {
                            Some(s) => s,
                            None => continue,
                        };
                        if let Some(obj) = row.as_object() {
                            if let Some((_, val)) = obj.iter().find(|(k, _)| k.as_str() != "source")
                            {
                                if let Some(s) = val.as_str() {
                                    if !s.is_empty() && s != "None" {
                                        grouped
                                            .entry(source.to_string())
                                            .or_default()
                                            .push(s.to_string());
                                    }
                                }
                            }
                        }
                    }

                    for inst in instances.iter_mut() {
                        let id_owned = match inst
                            .get("id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string())
                        {
                            Some(id) => id,
                            None => continue,
                        };
                        let values = grouped.get(id_owned.as_str());
                        if let Some(obj) = inst.as_object_mut() {
                            if prop.is_collection {
                                if prop.is_scalar_relation {
                                    let val = values
                                        .and_then(|v| v.first())
                                        .map(|s| Value::String(s.clone()))
                                        .unwrap_or(Value::Null);
                                    obj.insert(prop.name.clone(), val);
                                } else {
                                    let arr: Vec<Value> = values
                                        .map(|v| {
                                            v.iter().map(|s| Value::String(s.clone())).collect()
                                        })
                                        .unwrap_or_default();
                                    obj.insert(prop.name.clone(), Value::Array(arr));
                                }
                            } else {
                                if let Some(val) = values.and_then(|v| v.first()) {
                                    obj.insert(prop.name.clone(), Value::String(val.clone()));
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    log::warn!("Batched SELECT getter failed for '{}': {}", prop.name, e);
                }
            }
        }
    }

    // Post-getter where-clause filtering
    for prop in &getter_props {
        let (wf, wp) = match (&prop.where_filter, &prop.where_predicates) {
            (Some(wf), Some(wp)) => (wf, wp),
            _ => continue,
        };
        apply_where_filter_to_relation(store, instances, &prop.name, wf, wp)?;
    }

    Ok(())
}

/// Apply a post-getter where-clause filter to a relation across all instances.
///
/// After a getter populates a relation's target IDs, this function fetches
/// the target instances' property values (via batched `VALUES` queries) and
/// filters out targets that don't match the where conditions.  The relation
/// arrays on each parent instance are updated in-place.
pub(super) fn apply_where_filter_to_relation(
    store: &SparqlStore,
    instances: &mut [Value],
    relation_name: &str,
    where_filter: &BTreeMap<String, super::types::WhereCondition>,
    where_predicates: &HashMap<String, String>,
) -> Result<(), Error> {
    let all_targets: Vec<String> = instances
        .iter()
        .filter_map(|inst| {
            inst.get(relation_name)
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .collect::<Vec<_>>()
                })
        })
        .flatten()
        .collect();

    if all_targets.is_empty() {
        return Ok(());
    }

    let unique_targets: Vec<&str> = {
        let mut seen = std::collections::HashSet::new();
        all_targets
            .iter()
            .filter(|t| seen.insert(t.as_str()))
            .map(|t| t.as_str())
            .collect()
    };

    let values_clause = unique_targets
        .iter()
        .filter_map(|id| validate_iri(id).ok())
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(" ");

    if values_clause.is_empty() {
        return Ok(());
    }

    let mut target_pass: HashMap<String, bool> = unique_targets
        .iter()
        .map(|t| (t.to_string(), true))
        .collect();

    for (prop_name, condition) in where_filter {
        let predicate = match where_predicates.get(prop_name) {
            Some(p) => p,
            None => continue,
        };
        if validate_iri(predicate).is_err() {
            continue;
        }

        let query = format!(
            "SELECT ?source ?val WHERE {{ VALUES ?source {{ {} }} ?source <{}> ?val . }}",
            values_clause, predicate
        );

        let result_json = store.query(&query)?;
        let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();

        let mut target_vals: HashMap<String, Value> = HashMap::new();
        for row in &rows {
            if let (Some(source), Some(val_str)) = (
                row.get("source").and_then(|v| v.as_str()),
                row.get("val").and_then(|v| v.as_str()),
            ) {
                target_vals.insert(source.to_string(), parse_literal_value(val_str));
            }
        }

        for (target_id, pass) in target_pass.iter_mut() {
            if !*pass {
                continue;
            }
            match target_vals.get(target_id) {
                Some(val) => {
                    if !matches_condition(val, condition) {
                        *pass = false;
                    }
                }
                None => {
                    *pass = false;
                }
            }
        }
    }

    for inst in instances.iter_mut() {
        if let Some(arr) = inst.get(relation_name).and_then(|v| v.as_array()).cloned() {
            let filtered: Vec<Value> = arr
                .into_iter()
                .filter(|v| {
                    v.as_str()
                        .map(|id| target_pass.get(id).copied().unwrap_or(false))
                        .unwrap_or(false)
                })
                .collect();
            if let Some(obj) = inst.as_object_mut() {
                obj.insert(relation_name.to_string(), Value::Array(filtered));
            }
        }
    }

    Ok(())
}
