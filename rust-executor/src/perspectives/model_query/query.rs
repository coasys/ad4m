//! Top-level query orchestrator.
//!
//! [`execute_model_query`] is the single public entry point that external code
//! (e.g. `perspective_instance.rs`) calls.  It wires together shape resolution,
//! SPARQL construction, store execution, hydration, filtering, getter
//! evaluation, relation resolution, and projection attachment — producing a
//! [`ModelQueryResult`] with hydrated JSON instances and a total count.

use super::eval_transform::eval_transform;
use super::filtering::{matches_where, sort_instances};
use super::getters::evaluate_getters;
use super::hydration::{filter_properties, group_results_by_source, hydrate_instances};
use super::projection::resolve_projections;
use super::relations::{resolve_includes_recursive, resolve_reverse_relations};
use super::shape::{load_shape, parse_shape_from_json};
use super::sparql_builder::{all_where_pushable, build_count_sparql, build_instance_sparql};
use super::types::{
    InstanceQueryPlan, ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection, SortKey,
    SparqlPagination,
};
use super::utils::{validate_iri, MAX_INCLUDE_DEPTH};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::Error;
use serde_json::Value;

/// Execute a model query against the Oxigraph store.
///
/// This is the main public entry point.  It delegates to
/// [`execute_model_query_inner`] with an initial recursion depth of 0.
///
/// # Arguments
///
/// * `store` — The Oxigraph SPARQL store to query against.
/// * `class_name` — Name of the model class (e.g. `"Recipe"`, `"Message"`).
/// * `query_input` — The deserialized query object from the TS client.
/// * `shape_json` — Optional JSON metadata describing the model's shape.
///   When `None`, the shape is loaded from SHACL triples in the store.
pub async fn execute_model_query(
    store: &SparqlStore,
    class_name: &str,
    query_input: &ModelQueryInput,
    shape_json: Option<&str>,
) -> Result<ModelQueryResult, Error> {
    execute_model_query_inner(store, class_name, query_input, shape_json, 0).await
}

/// Inner implementation with recursion depth tracking.
///
/// The `depth` parameter prevents infinite cycles when resolving nested
/// `include` relations (e.g. A includes B which includes A).  If depth
/// exceeds [`MAX_INCLUDE_DEPTH`], an empty result is returned.
pub(super) async fn execute_model_query_inner(
    store: &SparqlStore,
    class_name: &str,
    query_input: &ModelQueryInput,
    shape_json: Option<&str>,
    depth: u8,
) -> Result<ModelQueryResult, Error> {
    if depth > MAX_INCLUDE_DEPTH {
        log::warn!(
            "Include resolution depth {} exceeded for class '{}'; returning empty",
            MAX_INCLUDE_DEPTH,
            class_name
        );
        return Ok(ModelQueryResult {
            instances: vec![],
            total_count: 0,
        });
    }
    // Load shape from store or parse from provided JSON
    let shape = if let Some(json) = shape_json {
        parse_shape_from_json(json, class_name)?
    } else {
        load_shape(store, class_name)?
    };

    // Fast path: COUNT-only
    let is_count_only = query_input.limit == Some(0);
    if is_count_only && all_where_pushable(query_input, &shape) {
        if let Some(sparql) = build_count_sparql(&shape, query_input) {
            let result_json = store.query_async(&sparql).await?;
            let results: Vec<Value> = serde_json::from_str(&result_json)?;
            let count = results
                .first()
                .and_then(|r| {
                    r["cnt"]
                        .as_str()
                        .and_then(|s| s.parse::<usize>().ok())
                        .or_else(|| r["cnt"].as_u64().map(|n| n as usize))
                })
                .unwrap_or(0);
            return Ok(ModelQueryResult {
                instances: vec![],
                total_count: count,
            });
        }
    }

    // Full pipeline
    let can_push_pagination = all_where_pushable(query_input, &shape) && {
        match &query_input.order {
            None => true,
            Some(order) => {
                order.len() == 1
                    && (order[0].0 == "timestamp"
                        || order[0].0 == "createdAt"
                        || order[0].0 == "updatedAt"
                        || shape.properties.iter().any(|p| {
                            p.name == order[0].0 && !p.is_collection && !p.predicate.is_empty()
                        }))
            }
        }
    };

    let sparql_pagination =
        if can_push_pagination && (query_input.limit.is_some() || query_input.offset.is_some()) {
            let direction = query_input
                .order
                .as_ref()
                .and_then(|o| o.first())
                .map(|(_, d)| *d)
                .unwrap_or(OrderDirection::ASC);
            let sort_key = match &query_input.order {
                None => SortKey::Timestamp,
                Some(order) => {
                    let key = &order[0].0;
                    if key == "timestamp" || key == "createdAt" || key == "updatedAt" {
                        SortKey::Timestamp
                    } else if let Some(prop) = shape
                        .properties
                        .iter()
                        .find(|p| p.name == *key && !p.is_collection && !p.predicate.is_empty())
                    {
                        SortKey::Property(prop.predicate.clone())
                    } else {
                        SortKey::Timestamp
                    }
                }
            };
            Some(SparqlPagination {
                sort_key,
                direction,
                offset: query_input.offset,
                limit: query_input.limit,
            })
        } else {
            None
        };

    let query_plan = build_instance_sparql(&shape, query_input, sparql_pagination.as_ref());

    let raw_results: Vec<Value> = match query_plan {
        InstanceQueryPlan::Single(sparql) => {
            let result_json = store.query_async(&sparql).await?;
            serde_json::from_str(&result_json)?
        }
        InstanceQueryPlan::TwoPhase {
            pagination_subquery,
            predicate_filter,
        } => {
            let page_json = store.query_async(&pagination_subquery).await?;
            let page_results: Vec<Value> = serde_json::from_str(&page_json)?;

            if page_results.is_empty() {
                vec![]
            } else {
                let source_values: String = page_results
                    .iter()
                    .filter_map(|r| r["source"].as_str())
                    .filter_map(|s| validate_iri(s).ok())
                    .map(|s| format!("<{s}>"))
                    .collect::<Vec<_>>()
                    .join(" ");

                if source_values.is_empty() {
                    vec![]
                } else {
                    let property_sparql = format!(
                        r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
    VALUES ?source {{ {source_values} }}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
                    );
                    let result_json = store.query_async(&property_sparql).await?;
                    serde_json::from_str(&result_json)?
                }
            }
        }
    };

    let grouped = group_results_by_source(&raw_results, &shape);
    let mut instances = hydrate_instances(&shape, &grouped);

    // Apply transform expressions for resolveLanguage properties
    resolve_language_transforms(&shape, &mut instances).await?;

    // Resolve reverse relations
    let reverse_rels: Vec<(String, String, bool)> = shape
        .properties
        .iter()
        .filter(|p| p.direction.as_deref() == Some("reverse"))
        .map(|p| (p.name.clone(), p.predicate.clone(), p.is_scalar_relation))
        .collect();
    if !reverse_rels.is_empty() && !instances.is_empty() {
        resolve_reverse_relations(store, &mut instances, &reverse_rels)?;
    }

    // Apply post-hydration where-clause filters
    if let Some(ref where_clause) = query_input.where_clause {
        if !all_where_pushable(query_input, &shape) {
            instances.retain(|inst| matches_where(inst, where_clause, &shape));
        }
    }

    // Calculate total count
    let total_count = if sparql_pagination.is_some() {
        if let Some(count_sparql) = build_count_sparql(&shape, query_input) {
            let result_json = store.query_async(&count_sparql).await?;
            let results: Vec<Value> = serde_json::from_str(&result_json)?;
            results
                .first()
                .and_then(|r| {
                    r["cnt"]
                        .as_str()
                        .and_then(|s| s.parse::<usize>().ok())
                        .or_else(|| r["cnt"].as_u64().map(|n| n as usize))
                })
                .unwrap_or(instances.len())
        } else {
            instances.len()
        }
    } else {
        instances.len()
    };

    // Apply ordering and pagination
    let mut paginated: Vec<Value> = if sparql_pagination.is_some() {
        if let Some(ref order) = query_input.order {
            sort_instances(&mut instances, order);
        } else {
            sort_instances(
                &mut instances,
                &[("timestamp".to_string(), OrderDirection::ASC)],
            );
        }
        instances
    } else {
        if let Some(ref order) = query_input.order {
            sort_instances(&mut instances, order);
        } else if query_input.limit.is_some() || query_input.offset.is_some() {
            sort_instances(
                &mut instances,
                &[("timestamp".to_string(), OrderDirection::ASC)],
            );
        }

        let offset = query_input.offset.unwrap_or(0);
        if let Some(limit) = query_input.limit {
            instances.into_iter().skip(offset).take(limit).collect()
        } else {
            instances.into_iter().skip(offset).collect()
        }
    };

    // Evaluate property/relation getters (post-pagination)
    if !paginated.is_empty() {
        let deep_query = query_input.deep_query.unwrap_or(true);
        evaluate_getters(
            store,
            &mut paginated,
            &shape,
            query_input.include.as_ref(),
            deep_query,
        )?;
    }

    // Eager-load included relations
    if let Some(ref include) = query_input.include {
        if !paginated.is_empty() && !shape.include_relations.is_empty() {
            resolve_includes_recursive(store, &mut paginated, include, &shape, depth).await?;
        }
    }

    // Strip unrequested properties if specified
    let mut final_instances: Vec<Value> = if let Some(ref requested) = query_input.properties {
        let mut keep = requested.clone();
        if let Some(ref inc) = query_input.include {
            for rel_name in inc.keys() {
                if !keep.contains(rel_name) {
                    keep.push(rel_name.clone());
                }
            }
        }
        paginated
            .into_iter()
            .map(|inst| filter_properties(inst, &keep))
            .collect()
    } else {
        paginated
    };

    // Attach projection results
    if let Some(ref projections) = query_input.projections {
        resolve_projections(store, &mut final_instances, projections, &shape)?;
    }

    Ok(ModelQueryResult {
        instances: final_instances,
        total_count,
    })
}

/// Apply transform expressions to resolveLanguage properties.
///
/// For properties marked with `resolve_language`, if the value is a non-literal
/// expression URL (not starting with "literal:"), this function fetches the
/// expression data from the language controller and applies the property's
/// transform expression (or the default file decode).
async fn resolve_language_transforms(
    shape: &ModelShape,
    instances: &mut [Value],
) -> Result<(), Error> {
    let resolve_props: Vec<&super::types::ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.resolve_language.is_some())
        .collect();

    if resolve_props.is_empty() {
        return Ok(());
    }

    let controller = crate::languages::LanguageController::global_instance();

    for instance in instances.iter_mut() {
        for prop in &resolve_props {
            if let Some(uri) = instance[&prop.name].as_str() {
                // Skip literal: URIs (they've already been hydrated)
                if !uri.starts_with("literal:") {
                    if let Ok((lang, expr_addr)) =
                        crate::languages::LanguageController::parse_expr_url(uri)
                    {
                        if let Ok(Some(expr_json)) =
                            controller.get_expression(&lang, &expr_addr).await
                        {
                            // Extract the resolved data from the expression
                            let resolved: Value = expr_json
                                .get("data")
                                .cloned()
                                .unwrap_or_else(|| Value::Null);

                            // Parse string data if needed
                            let resolved = match &resolved {
                                Value::String(s) => serde_json::from_str(s).unwrap_or(resolved),
                                other => other.clone(),
                            };

                            // Apply the transform expression (or default file decode)
                            let default_transform = super::types::default_file_decode();
                            let transform = prop.transform.as_ref().unwrap_or(&default_transform);
                            instance[&prop.name] = eval_transform(transform, &resolved, &resolved);
                        }
                    }
                }
            } else if instance[&prop.name].is_object() {
                // For literal objects (already-hydrated from literal: URIs), apply the transform
                let obj = instance[&prop.name].clone();
                let default_transform = super::types::default_file_decode();
                let transform = prop.transform.as_ref().unwrap_or(&default_transform);
                instance[&prop.name] = eval_transform(transform, &obj, &obj);
            }
        }
    }
    Ok(())
}
