//! Relation resolution: reverse relations and recursive `include` eager-loading.
//!
//! This module handles two concerns:
//!
//! 1. **Reverse relations** ([`resolve_reverse_relations`]) — For `@BelongsTo`
//!    relations where the triple direction is `target → source` (the *other*
//!    instance points *at* this one), we query for `?source <pred> ?target`
//!    where `?target` is our instance ID, and collect the `?source` values.
//!
//! 2. **Recursive eager-loading** ([`resolve_includes_recursive`]) — When the
//!    query includes `include: { comments: true }`, we collect all target IDs
//!    from the relation, run a recursive sub-query via
//!    [`execute_model_query_inner`], and replace the raw ID arrays with fully
//!    hydrated child instances.  This supports arbitrary nesting depth
//!    (bounded by [`MAX_INCLUDE_DEPTH`](super::utils::MAX_INCLUDE_DEPTH)).

use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::HashMap;

use super::query::execute_model_query_inner;
use super::types::{IncludeValue, ModelQueryInput, ModelShape, ShapeRelation, WhereCondition};
use super::utils::validate_iri;
use crate::perspectives::sparql_store::SparqlStore;

/// Resolve reverse relations (`@BelongsTo`) for all instances in a batch.
///
/// For each `(name, predicate, is_single)` relation, executes a single
/// batched SPARQL query: `?source <pred> ?target` with `VALUES ?target { ... }`
/// containing all instance IDs.  The results are attached to each instance
/// as either a scalar (for `belongsToOne`) or an array (for `belongsToMany`).
pub fn resolve_reverse_relations(
    store: &SparqlStore,
    instances: &mut [Value],
    relations: &[(String, String, bool)], // (name, predicate, is_single)
) -> Result<(), Error> {
    if relations.is_empty() || instances.is_empty() {
        return Ok(());
    }

    let instance_iris: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str())
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

    for (rel_name, predicate, is_single) in relations {
        let safe_pred = match validate_iri(predicate) {
            Ok(p) => p,
            Err(_) => continue,
        };

        let sparql = format!(
            "SELECT ?source ?target WHERE {{ VALUES ?target {{ {} }} ?source <{safe_pred}> ?target . }}",
            values_clause
        );
        let result_json = store.query(&sparql)?;
        let rows: Vec<Value> = serde_json::from_str(&result_json)?;

        let mut target_to_sources: HashMap<String, Vec<String>> = HashMap::new();
        for row in &rows {
            let source = row["source"].as_str().unwrap_or("");
            let target = row["target"].as_str().unwrap_or("");
            if !source.is_empty() && !target.is_empty() {
                target_to_sources
                    .entry(target.to_string())
                    .or_default()
                    .push(source.to_string());
            }
        }

        for inst in instances.iter_mut() {
            let id = match inst["id"].as_str() {
                Some(id) => id,
                None => continue,
            };

            let source_ids: Vec<Value> = target_to_sources
                .get(id)
                .map(|sources| sources.iter().map(|s| Value::String(s.clone())).collect())
                .unwrap_or_default();

            if *is_single {
                let val = source_ids.first().cloned().unwrap_or(Value::Null);
                inst.as_object_mut()
                    .map(|obj| obj.insert(rel_name.clone(), val));
            } else {
                inst.as_object_mut()
                    .map(|obj| obj.insert(rel_name.clone(), Value::Array(source_ids)));
            }
        }
    }

    Ok(())
}

/// Resolve all `include`d relations for a set of instances.
///
/// Iterates over the `include` map and, for each relation with enriched
/// metadata in the shape, delegates to either [`resolve_forward_include`]
/// or [`resolve_reverse_include`].  Sub-queries within `IncludeValue::SubQuery`
/// are passed through to the recursive call.
pub(super) async fn resolve_includes_recursive(
    store: &SparqlStore,
    instances: &mut [Value],
    include: &HashMap<String, IncludeValue>,
    shape: &ModelShape,
    depth: u8,
) -> Result<(), Error> {
    for (rel_name, include_val) in include {
        match include_val {
            IncludeValue::Bool(false) => continue,
            _ => {}
        }

        let rel = match shape.include_relations.iter().find(|r| r.name == *rel_name) {
            Some(r) => r,
            None => continue,
        };

        let sub_query = match include_val {
            IncludeValue::Bool(true) => ModelQueryInput::default(),
            IncludeValue::SubQuery(sq) => *sq.clone(),
            _ => continue,
        };

        if rel.direction == "reverse" {
            resolve_reverse_include(store, instances, rel, &sub_query, depth).await?;
        } else {
            resolve_forward_include(store, instances, rel, &sub_query, depth).await?;
        }
    }
    Ok(())
}

/// Resolve a forward relation (`@HasMany` / `@HasOne`) for all instances.
///
/// Collects all unique target IDs from the relation arrays, runs a sub-query
/// to hydrate them, and replaces the raw ID arrays with hydrated JSON objects.
/// If the sub-query specifies an `order`, the result order is preserved.
async fn resolve_forward_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    sub_query: &ModelQueryInput,
    depth: u8,
) -> Result<(), Error> {
    let mut seen = std::collections::HashSet::new();
    let mut all_ids: Vec<String> = Vec::new();
    for inst in instances.iter() {
        if let Some(arr) = inst[&rel.name].as_array() {
            for item in arr {
                if let Some(id) = item.as_str() {
                    if seen.insert(id.to_string()) {
                        all_ids.push(id.to_string());
                    }
                }
            }
        } else if let Some(id) = inst[&rel.name].as_str() {
            if seen.insert(id.to_string()) {
                all_ids.push(id.to_string());
            }
        }
    }
    if all_ids.is_empty() {
        return Ok(());
    }

    let mut query = sub_query.clone();
    let mut wc = query.where_clause.take().unwrap_or_default();

    if let Some(existing_id) = wc.get("id") {
        let filter_ids: Vec<String> = match existing_id {
            WhereCondition::String(s) => vec![s.clone()],
            WhereCondition::StringArray(arr) => arr.clone(),
            _ => vec![],
        };
        all_ids.retain(|id| filter_ids.contains(id));
    }
    wc.insert("id".to_string(), WhereCondition::StringArray(all_ids));
    query.where_clause = Some(wc);

    let has_sub_order = sub_query.order.is_some();

    let result = Box::pin(execute_model_query_inner(
        store,
        &rel.target_class_name,
        &query,
        Some(&rel.target_shape_json),
        depth + 1,
    ))
    .await?;

    let mut hydrated: HashMap<String, Value> = HashMap::new();
    let ordered_ids: Vec<String> = result
        .instances
        .iter()
        .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
        .collect();
    for inst in result.instances {
        if let Some(id) = inst["id"].as_str() {
            hydrated.insert(id.to_string(), inst);
        }
    }

    for inst in instances.iter_mut() {
        let raw = inst[&rel.name].clone();
        let parent_ids: Vec<String> = if let Some(arr) = raw.as_array() {
            arr.iter()
                .filter_map(|item| item.as_str().map(|s| s.to_string()))
                .collect()
        } else if let Some(id) = raw.as_str() {
            vec![id.to_string()]
        } else {
            continue;
        };

        let iter_ids: Vec<&str> = if has_sub_order {
            ordered_ids
                .iter()
                .filter(|id| parent_ids.iter().any(|pid| pid == *id))
                .map(|s| s.as_str())
                .collect()
        } else {
            parent_ids.iter().map(|s| s.as_str()).collect()
        };

        let items: Vec<Value> = iter_ids
            .iter()
            .filter_map(|id| hydrated.get(*id).cloned())
            .collect();

        let resolved = if rel.max_count == Some(1) {
            items.first().cloned().unwrap_or(Value::Null)
        } else {
            Value::Array(items)
        };
        inst.as_object_mut()
            .map(|obj| obj.insert(rel.name.clone(), resolved));
    }
    Ok(())
}

/// Resolve a reverse include relation (`@BelongsTo`) for all instances.
///
/// Queries for `?source <pred> ?target` where targets are the current
/// instance IDs, collects all source IDs, hydrates them via a sub-query,
/// and attaches the results (scalar for `belongsToOne`, array for
/// `belongsToMany`).
async fn resolve_reverse_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    sub_query: &ModelQueryInput,
    depth: u8,
) -> Result<(), Error> {
    let all_ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
        .collect();
    if all_ids.is_empty() {
        return Ok(());
    }

    let id_list = all_ids
        .iter()
        .filter(|id| validate_iri(id).is_ok())
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(", ");
    if id_list.is_empty() {
        return Ok(());
    }
    let safe_pred = match validate_iri(&rel.predicate) {
        Ok(p) => p,
        Err(_) => return Ok(()),
    };
    let sparql = format!(
        "SELECT ?source ?target WHERE {{ ?source <{safe_pred}> ?target . FILTER(?target IN ({id_list})) }}"
    );
    let result_json = store.query(&sparql)?;
    let rows: Vec<Value> = serde_json::from_str(&result_json)?;

    let mut sources_by_target: HashMap<String, Vec<String>> = HashMap::new();
    for row in &rows {
        let source = row["source"].as_str().unwrap_or("");
        let target = row["target"].as_str().unwrap_or("");
        if !source.is_empty() && !target.is_empty() {
            sources_by_target
                .entry(target.to_string())
                .or_default()
                .push(source.to_string());
        }
    }

    let all_source_ids: Vec<String> = {
        let mut set = std::collections::HashSet::new();
        for ids in sources_by_target.values() {
            for id in ids {
                set.insert(id.clone());
            }
        }
        set.into_iter().collect()
    };

    let has_sub_order = sub_query.order.is_some();
    let hydrated: HashMap<String, Value>;
    let ordered_result_ids: Vec<String>;
    if all_source_ids.is_empty() {
        hydrated = HashMap::new();
        ordered_result_ids = Vec::new();
    } else {
        let mut query = sub_query.clone();
        let mut wc = query.where_clause.take().unwrap_or_default();
        if let Some(existing_id) = wc.get("id") {
            let filter_ids: Vec<String> = match existing_id {
                WhereCondition::String(s) => vec![s.clone()],
                WhereCondition::StringArray(arr) => arr.clone(),
                _ => vec![],
            };
            let filtered: Vec<String> = all_source_ids
                .into_iter()
                .filter(|id| filter_ids.contains(id))
                .collect();
            wc.insert("id".to_string(), WhereCondition::StringArray(filtered));
        } else {
            wc.insert(
                "id".to_string(),
                WhereCondition::StringArray(all_source_ids),
            );
        }
        query.where_clause = Some(wc);

        let result = Box::pin(execute_model_query_inner(
            store,
            &rel.target_class_name,
            &query,
            Some(&rel.target_shape_json),
            depth + 1,
        ))
        .await?;

        ordered_result_ids = result
            .instances
            .iter()
            .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
            .collect();
        let mut map = HashMap::new();
        for inst in result.instances {
            if let Some(id) = inst["id"].as_str() {
                map.insert(id.to_string(), inst);
            }
        }
        hydrated = map;
    };

    for inst in instances.iter_mut() {
        let inst_id = inst["id"].as_str().unwrap_or("").to_string();
        let source_ids = sources_by_target.get(&inst_id).cloned().unwrap_or_default();

        let resolved = if rel.kind == "belongsToOne" || rel.max_count == Some(1) {
            source_ids
                .first()
                .and_then(|id| hydrated.get(id).cloned())
                .unwrap_or(Value::Null)
        } else {
            let iter_ids: Vec<&str> = if has_sub_order {
                ordered_result_ids
                    .iter()
                    .filter(|id| source_ids.contains(id))
                    .map(|s| s.as_str())
                    .collect()
            } else {
                source_ids.iter().map(|s| s.as_str()).collect()
            };
            let items: Vec<Value> = iter_ids
                .iter()
                .filter_map(|id| hydrated.get(*id).cloned())
                .collect();
            Value::Array(items)
        };
        inst.as_object_mut()
            .map(|obj| obj.insert(rel.name.clone(), resolved));
    }
    Ok(())
}
