//! Projection resolution for lightweight relation aggregations.
//!
//! Projections (keys prefixed with `$` in the TS query) compute per-instance
//! counts or filtered lists over a relation predicate without fully hydrating
//! the related instances.  Each projection produces a single grouped SPARQL
//! query that runs against the store and attaches results to the parent
//! instances.
//!
//! Two modes are supported:
//! - **Count** (`count: true`) — attaches an integer count per parent.
//! - **List** (`count: false`) — attaches an array of target IRIs per parent,
//!   optionally ordered and limited.
//!
//! Projections can also filter by target properties (via `where`) and by
//! reifier metadata (author/timestamp).

use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

use super::types::{ModelQueryInput, ModelShape, OrderDirection, ProjectionInput, WhereCondition};
use super::utils::{escape_sparql_string, validate_iri};
use crate::perspectives::sparql_store::SparqlStore;

/// Resolve all projections for a set of parent instances.
///
/// For each projection key, builds and executes a single grouped SPARQL
/// query that collects either counts or target IRI lists, then merges the
/// results into the instance objects.
///
/// When `proj.target_shape` is set, raw target IRIs are replaced with fully
/// hydrated model instances via a recursive `execute_model_query_inner` call
/// (one batch per projection key, eliminating TS-side round-trips).
pub(super) fn resolve_projections(
    store: &SparqlStore,
    instances: &mut Vec<Value>,
    projections: &HashMap<String, ProjectionInput>,
    shape: &ModelShape,
    depth: u8,
) -> Result<(), deno_core::anyhow::Error> {
    if instances.is_empty() || projections.is_empty() {
        return Ok(());
    }

    let parent_ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str())
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if parent_ids.is_empty() {
        return Ok(());
    }

    let values_clause = parent_ids
        .iter()
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(" ");

    for (key, proj) in projections {
        let predicate = match shape.properties.iter().find(|p| p.name == proj.from) {
            Some(p) if !p.predicate.is_empty() => p.predicate.clone(),
            Some(_) => {
                log::warn!(
                    "IncludeProjection '{}': relation '{}' has empty predicate — skipping",
                    key,
                    proj.from
                );
                continue;
            }
            None => {
                log::warn!(
                    "IncludeProjection '{}': relation '{}' not found in shape — skipping",
                    key,
                    proj.from
                );
                continue;
            }
        };

        let safe_pred = match validate_iri(&predicate) {
            Ok(p) => p.to_string(),
            Err(_) => {
                log::warn!(
                    "IncludeProjection '{}': predicate '{}' is not a valid IRI — skipping",
                    key,
                    predicate
                );
                continue;
            }
        };

        let where_patterns = build_projection_where_patterns(proj);
        let reifier_patterns = build_projection_reifier_patterns(proj, &safe_pred);

        if proj.count {
            let sparql = format!(
                concat!(
                    "SELECT ?parent (COUNT(DISTINCT ?t) AS ?n) WHERE {{\n",
                    "    VALUES ?parent {{ {values_clause} }}\n",
                    "    ?parent <{safe_pred}> ?t .\n",
                    "{where_patterns}",
                    "{reifier_patterns}",
                    "}} GROUP BY ?parent"
                ),
                values_clause = values_clause,
                safe_pred = safe_pred,
                where_patterns = where_patterns,
                reifier_patterns = reifier_patterns,
            );

            let result_json = store.query(&sparql)?;
            let rows: Vec<Value> = serde_json::from_str(&result_json)?;

            let mut count_map: HashMap<String, u64> = HashMap::new();
            for row in &rows {
                let parent = row["parent"].as_str().unwrap_or("").to_string();
                let n: u64 = row["n"]
                    .as_str()
                    .and_then(|s| s.parse().ok())
                    .or_else(|| row["n"].as_u64())
                    .unwrap_or(0);
                count_map.insert(parent, n);
            }

            for inst in instances.iter_mut() {
                if let Some(obj) = inst.as_object_mut() {
                    let id = obj
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let cnt = count_map.get(&id).copied().unwrap_or(0);
                    obj.insert(key.clone(), Value::Number(cnt.into()));
                }
            }
        } else {
            let order_clause = build_projection_order_clause(proj);

            let sparql = format!(
                concat!(
                    "SELECT ?parent ?t WHERE {{\n",
                    "    VALUES ?parent {{ {values_clause} }}\n",
                    "    ?parent <{safe_pred}> ?t .\n",
                    "{where_patterns}",
                    "{reifier_patterns}",
                    "}}{order_clause}"
                ),
                values_clause = values_clause,
                safe_pred = safe_pred,
                where_patterns = where_patterns,
                reifier_patterns = reifier_patterns,
                order_clause = order_clause,
            );

            let result_json = store.query(&sparql)?;
            let rows: Vec<Value> = serde_json::from_str(&result_json)?;

            let mut list_map: HashMap<String, Vec<Value>> = HashMap::new();
            for row in &rows {
                if let Some(parent) = row["parent"].as_str() {
                    let t = row["t"]
                        .as_str()
                        .map(|s| Value::String(s.to_string()))
                        .unwrap_or(Value::Null);
                    list_map.entry(parent.to_string()).or_default().push(t);
                }
            }

            // ----------------------------------------------------------------
            // 4. If a target shape is available, hydrate raw IRIs → full model
            //    instances in-process — no TS round-trip required.
            // ----------------------------------------------------------------
            if let Some(ref target_shape) = proj.target_shape {
                if !proj.count {
                    if let Some(target_class) = target_shape["className"].as_str() {
                        if !target_class.is_empty() {
                            // Collect all unique raw IRI strings.
                            let mut seen = std::collections::HashSet::new();
                            let mut all_ids: Vec<String> = Vec::new();
                            for vals in list_map.values() {
                                for v in vals {
                                    if let Some(s) = v.as_str() {
                                        if seen.insert(s.to_string()) {
                                            all_ids.push(s.to_string());
                                        }
                                    }
                                }
                            }

                            if !all_ids.is_empty() {
                                let target_shape_json =
                                    serde_json::to_string(target_shape).unwrap_or_default();
                                let mut sub_where = BTreeMap::new();
                                sub_where
                                    .insert("id".to_string(), WhereCondition::StringArray(all_ids));
                                let sub_query = ModelQueryInput {
                                    where_clause: Some(sub_where),
                                    deep_query: Some(true),
                                    ..ModelQueryInput::default()
                                };

                                if let Ok(result) = super::query::execute_model_query_inner(
                                    store,
                                    target_class,
                                    &sub_query,
                                    Some(&target_shape_json),
                                    depth + 1,
                                ) {
                                    let hydrated: HashMap<String, Value> = result
                                        .instances
                                        .into_iter()
                                        .filter_map(|inst| {
                                            let id = inst["id"].as_str()?.to_string();
                                            Some((id, inst))
                                        })
                                        .collect();

                                    // Replace raw IRI strings with hydrated objects.
                                    for vals in list_map.values_mut() {
                                        for v in vals.iter_mut() {
                                            if let Some(id) = v.as_str() {
                                                if let Some(obj) = hydrated.get(id) {
                                                    *v = obj.clone();
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            for inst in instances.iter_mut() {
                if let Some(obj) = inst.as_object_mut() {
                    let id = obj
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let val = match proj.limit {
                        Some(1) => list_map
                            .get(&id)
                            .and_then(|v| v.first().cloned())
                            .unwrap_or(Value::Null),
                        Some(n) => Value::Array(
                            list_map
                                .get(&id)
                                .cloned()
                                .unwrap_or_default()
                                .into_iter()
                                .take(n)
                                .collect(),
                        ),
                        None => Value::Array(list_map.get(&id).cloned().unwrap_or_default()),
                    };
                    obj.insert(key.clone(), val);
                }
            }
        }
    }

    Ok(())
}

/// Build SPARQL where-clause patterns for a projection's `where` filter.
///
/// Translates property-level conditions (string, number, bool, array) into
/// `FILTER` expressions that constrain which targets are counted/listed.
/// Conditions on `id`/`base` filter on `?t` directly; conditions on
/// `author`/`timestamp` are handled by [`build_projection_reifier_patterns`]
/// instead.
pub(super) fn build_projection_where_patterns(proj: &ProjectionInput) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    let pred_lookup: HashMap<String, String> = if let Some(ref ts) = proj.target_shape {
        let mut map = HashMap::new();
        if let Some(props) = ts["properties"].as_object() {
            for (name, pm) in props {
                if let Some(pred) = pm["predicate"].as_str() {
                    if !pred.is_empty() {
                        map.insert(name.clone(), pred.to_string());
                    }
                }
            }
        }
        if let Some(rels) = ts["relations"].as_object() {
            for (name, rm) in rels {
                if let Some(pred) = rm["predicate"].as_str() {
                    if !pred.is_empty() {
                        map.insert(name.clone(), pred.to_string());
                    }
                }
            }
        }
        map
    } else {
        HashMap::new()
    };

    let mut patterns = Vec::new();
    let mut filter_idx = 0usize;

    for (prop_name, condition) in wc {
        if prop_name == "id" || prop_name == "base" {
            match condition {
                WhereCondition::String(val) => {
                    let escaped = escape_sparql_string(val);
                    patterns.push(format!("    FILTER(STR(?t) = \"{escaped}\")\n"));
                }
                WhereCondition::StringArray(vals) => {
                    let list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    patterns.push(format!("    FILTER(STR(?t) IN ({list}))\n"));
                }
                _ => {}
            }
            continue;
        }

        if prop_name == "author" || prop_name == "timestamp" {
            continue;
        }

        let pred = match pred_lookup.get(prop_name) {
            Some(p) => p.clone(),
            None => continue,
        };

        if validate_iri(&pred).is_err() {
            continue;
        }

        let var = format!("_pw{filter_idx}");
        filter_idx += 1;

        match condition {
            WhereCondition::String(val) => {
                let escaped = escape_sparql_string(val);
                patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                patterns.push(format!(
                    "    FILTER(STR(<ad4m://fn/parse_literal>(?{var})) = \"{escaped}\")\n",
                ));
            }
            WhereCondition::Bool(b) => {
                let bval = if *b { "true" } else { "false" };
                patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                patterns.push(format!(
                    "    FILTER(STR(<ad4m://fn/parse_literal>(?{var})) = \"{bval}\")\n",
                ));
            }
            WhereCondition::Number(n) => {
                patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                patterns.push(format!(
                    "    FILTER(STR(<ad4m://fn/parse_literal>(?{var})) = \"{n}\")\n",
                ));
            }
            WhereCondition::StringArray(vals) => {
                let list = vals
                    .iter()
                    .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                patterns.push(format!(
                    "    FILTER(STR(<ad4m://fn/parse_literal>(?{var})) IN ({list}))\n",
                ));
            }
            _ => {}
        }
    }

    patterns.join("")
}

/// Build an `ORDER BY` clause for a projection query.
///
/// Only `id`/`base` ordering is supported (ordering by `?t`).  Other
/// property-level ordering would require joining additional triples and
/// is not implemented for projections.
pub(super) fn build_projection_order_clause(proj: &ProjectionInput) -> String {
    let Some(ref order) = proj.order else {
        return String::new();
    };
    let terms: Vec<String> = order
        .iter()
        .filter_map(|(k, dir)| {
            if k == "id" || k == "base" {
                Some(match dir {
                    OrderDirection::ASC => "ASC(?t)".to_string(),
                    OrderDirection::DESC => "DESC(?t)".to_string(),
                })
            } else {
                None
            }
        })
        .collect();
    if terms.is_empty() {
        String::new()
    } else {
        let joined = terms.join(" ");
        format!("\nORDER BY {joined}")
    }
}

/// Build SPARQL patterns that filter projection targets by reifier metadata.
///
/// When the projection's where clause includes `author` or `timestamp`
/// conditions, this generates triple patterns that join against the RDF 1.2
/// reifier (the statement that records who created the link and when).
pub(super) fn build_projection_reifier_patterns(proj: &ProjectionInput, safe_pred: &str) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    let author_cond = wc.get("author");
    let timestamp_cond = wc.get("timestamp");

    if author_cond.is_none() && timestamp_cond.is_none() {
        return String::new();
    }

    let mut patterns = Vec::new();

    patterns.push(format!(
        "    ?_prj_reif <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<(?parent <{safe_pred}> ?t)>> .\n"
    ));

    if let Some(cond) = author_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/author> ?_prj_author .\n".to_string());
        if let WhereCondition::String(did) = cond {
            let escaped = escape_sparql_string(did);
            patterns.push(format!("    FILTER(STR(?_prj_author) = \"{escaped}\")\n"));
        }
    }

    if let Some(cond) = timestamp_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/timestamp> ?_prj_timestamp .\n".to_string());
        if let WhereCondition::String(ts) = cond {
            let escaped = escape_sparql_string(ts);
            patterns.push(format!(
                "    FILTER(STR(?_prj_timestamp) = \"{escaped}\")\n"
            ));
        }
    }

    patterns.join("")
}
