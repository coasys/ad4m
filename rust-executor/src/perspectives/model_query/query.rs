//! Top-level query orchestrator.
//!
//! [`execute_model_query`] is the single public entry point that external code
//! (e.g. `perspective_instance.rs`) calls.  Shape resolution is performed by
//! the caller; this function takes an already-resolved [`ModelShape`] plus a
//! [`ShapeResolver`] that recursive include resolution uses to look up
//! target-class shapes (themselves cached).

use super::eval_transform::eval_transform;
use super::filtering::{matches_where, sort_instances};
use super::getters::evaluate_getters;
use super::hydration::{filter_properties, group_results_by_source, hydrate_instances};
use super::projection::resolve_projections;
use super::relations::{resolve_includes_recursive, resolve_reverse_relations};
use super::sparql_builder::{
    all_where_pushable, build_count_sparql, build_instance_sparql,
    build_predicate_filter_for_property_fetch, where_clause_caps_result_size,
    where_clause_max_source_count,
};
use super::types::{
    InstanceQueryPlan, ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection,
    ShapeResolver, SortKey, SparqlPagination,
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
/// * `shape` — The resolved model shape for this class (from the cache).
/// * `query_input` — The deserialized query object from the TS client.
/// * `resolver` — Used to resolve target-class shapes for recursive
///   `include` resolution.  Typically a cache-backed resolver living on
///   the `PerspectiveInstance`.
pub async fn execute_model_query(
    store: &SparqlStore,
    shape: &ModelShape,
    query_input: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
) -> Result<ModelQueryResult, Error> {
    execute_model_query_inner(store, shape, query_input, resolver, 0).await
}

/// Inner implementation with recursion depth tracking.
///
/// The `depth` parameter prevents infinite cycles when resolving nested
/// `include` relations (e.g. A includes B which includes A).  If depth
/// exceeds [`MAX_INCLUDE_DEPTH`], an empty result is returned.
pub(super) async fn execute_model_query_inner(
    store: &SparqlStore,
    shape: &ModelShape,
    query_input: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    depth: u8,
) -> Result<ModelQueryResult, Error> {
    if depth > MAX_INCLUDE_DEPTH {
        log::warn!(
            "Include resolution depth {} exceeded for class '{}'; returning empty",
            MAX_INCLUDE_DEPTH,
            shape.target_class
        );
        return Ok(ModelQueryResult {
            instances: vec![],
            total_count: 0,
        });
    }

    // Fast path: COUNT-only.  Engaged when the caller sets `limit: 0`
    // (zero rows) AND either explicitly asked for count (`count: true`) or
    // didn't opt out (`count: None`, the back-compat default).  Skipped
    // when the caller explicitly set `count: false` (they want neither
    // rows nor count — return both empty).
    let is_count_only = query_input.limit == Some(0) && query_input.count != Some(false);
    if is_count_only && all_where_pushable(query_input, shape) {
        if let Some(sparql) = build_count_sparql(shape, query_input) {
            let results = store.query_values(&sparql)?;
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

    // Full pipeline.
    //
    // Multi-key sorts are pushed too: every key has to be either a
    // reifier-timestamp synonym (`timestamp`/`createdAt`/`updatedAt`) or a
    // scalar property in the shape with a non-empty predicate.  If any
    // key fails that test, fall back to the post-hydration Rust sort.
    let order_keys_pushable = match &query_input.order {
        None => true,
        Some(order) => order.iter().all(|(name, _)| {
            name == "timestamp"
                || name == "createdAt"
                || name == "updatedAt"
                || shape
                    .properties
                    .iter()
                    .any(|p| p.name == *name && !p.is_collection && !p.predicate.is_empty())
        }),
    };
    let can_push_pagination = all_where_pushable(query_input, shape) && order_keys_pushable;

    // When WHERE includes a uniquely-selective `id`/`base` equality, the
    // result set is bounded a priori — at most 1 row for `String`, at most
    // |arr| rows for `StringArray`.  In that case the TwoPhase pagination
    // plan is wasted work: phase 1's timestamp probe + ORDER BY scans the
    // reifier index for every candidate source, but there's nothing to
    // sort over and nothing to cut.  Detect it and fall through to the
    // Single plan, which applies LIMIT/OFFSET in the Rust post-step.
    //
    // This handles the common "fetch one row by id" pattern — flux's
    // `SemanticRelationship.findAll({ where: { expression: id }, limit: 1 })`
    // and equivalents — where the WHERE narrows to a single source IRI but
    // the limit hint still forces TwoPhase under the old policy.
    let where_is_uniquely_selective = where_clause_caps_result_size(query_input);
    let pagination_would_be_no_op = where_is_uniquely_selective
        && query_input.offset.unwrap_or(0) == 0
        && match query_input.limit {
            None => true,
            Some(n) => n >= where_clause_max_source_count(query_input).unwrap_or(usize::MAX),
        };

    let sparql_pagination =
        if !pagination_would_be_no_op
            && can_push_pagination
            && (query_input.limit.is_some() || query_input.offset.is_some())
        {
            let sort_keys: Vec<(SortKey, OrderDirection)> = match &query_input.order {
                None => vec![(SortKey::Timestamp, OrderDirection::ASC)],
                Some(order) => order
                    .iter()
                    .map(|(name, dir)| {
                        let key = if name == "timestamp"
                            || name == "createdAt"
                            || name == "updatedAt"
                        {
                            SortKey::Timestamp
                        } else if let Some(prop) =
                            shape.properties.iter().find(|p| {
                                p.name == *name && !p.is_collection && !p.predicate.is_empty()
                            })
                        {
                            SortKey::Property(prop.predicate.clone())
                        } else {
                            SortKey::Timestamp
                        };
                        (key, *dir)
                    })
                    .collect(),
            };
            Some(SparqlPagination {
                sort_keys,
                offset: query_input.offset,
                limit: query_input.limit,
            })
        } else {
            None
        };

    let query_plan = build_instance_sparql(shape, query_input, sparql_pagination.as_ref());

    let raw_results: Vec<Value> = match query_plan {
        InstanceQueryPlan::Single(sparql) => store.query_values_async(&sparql).await?,
        InstanceQueryPlan::TwoPhase {
            pagination_subquery,
        } => {
            let page_results = store.query_values_async(&pagination_subquery).await?;

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
                    // Property fetch keeps the wide-row shape: hydration's
                    // Rust fold handles per-scalar last-write-wins (the LWW
                    // SPARQL subquery hit a planner cliff in benchmarks —
                    // see the comment in `build_instance_sparql`).
                    let predicate_filter = build_predicate_filter_for_property_fetch(shape);
                    let with_metadata = query_input.with_metadata.unwrap_or(true);
                    let property_sparql = if with_metadata {
                        format!(
                            r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
    VALUES ?source {{ {source_values} }}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
                        )
                    } else {
                        format!(
                            r#"SELECT ?source ?predicate ?target WHERE {{
    VALUES ?source {{ {source_values} }}
{predicate_filter}    ?source ?predicate ?target .
    FILTER(isIRI(?predicate))
}}"#
                        )
                    };
                    store.query_values_async(&property_sparql).await?
                }
            }
        }
    };

    let grouped = group_results_by_source(&raw_results, shape);
    let mut instances = hydrate_instances(shape, &grouped);

    // createdAt / updatedAt synthesis stays in hydration's Rust fold: the
    // main property query returns every link row for each source, so the
    // per-row min/max over reifier timestamps observes every reifier the
    // SPARQL aggregate would have seen — without a second round trip.  We
    // tried pushing the aggregate (`build_aggregate_sparql` retains the
    // builder for unit tests) but the bounded `VALUES ?source { ... }` MIN
    // / MAX still cost ~150-300 ms on the Flux medium-tier benchmark
    // because the planner walks every reifier in the store when matching
    // the triple-term pattern.  The Rust fold over already-fetched rows is
    // strictly cheaper.

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
        if !all_where_pushable(query_input, shape) {
            instances.retain(|inst| matches_where(inst, where_clause, shape));
        }
    }

    // Calculate total count.
    //
    // The COUNT round-trip fires only when the caller asks for it:
    //   - `count: Some(true)`  → always fire (even without pagination)
    //   - `count: None`        → fire when pagination is applied (back-compat)
    //   - `count: Some(false)` → never fire; `total_count = instances.len()`
    //
    // The previous behaviour was equivalent to `count: None`, so existing
    // callers that don't pass `count` see no change.
    let want_count = match query_input.count {
        Some(true) => true,
        Some(false) => false,
        None => sparql_pagination.is_some(),
    };
    let total_count = if want_count {
        if let Some(count_sparql) = build_count_sparql(shape, query_input) {
            let results = store.query_values(&count_sparql)?;
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
            shape,
            query_input.include.as_ref(),
            deep_query,
        )?;
    }

    // Eager-load included relations
    if let Some(ref include) = query_input.include {
        if !paginated.is_empty() && !shape.include_relations.is_empty() {
            resolve_includes_recursive(store, &mut paginated, include, shape, resolver, depth)
                .await?;
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
        resolve_projections(
            store,
            &mut final_instances,
            projections,
            shape,
            resolver,
            depth,
        )
        .await?;
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

    // Pass 1: walk every (instance, resolve-language prop) pair and decide
    // whether it needs a `get_expression` lookup.  Collect the unique
    // (lang, expr_addr) pairs so we can fetch them in parallel — the
    // previous implementation awaited each call sequentially.
    let mut fetch_jobs: std::collections::HashMap<(String, String), Option<Value>> =
        std::collections::HashMap::new();
    let mut already_resolved: std::collections::HashMap<usize, Vec<(String, Value)>> =
        std::collections::HashMap::new();

    for (inst_idx, instance) in instances.iter().enumerate() {
        for prop in &resolve_props {
            let current = instance[&prop.name].clone();
            let mut record_resolved = |val: Value| {
                already_resolved
                    .entry(inst_idx)
                    .or_default()
                    .push((prop.name.clone(), val));
            };
            match &current {
                Value::String(uri) if !uri.starts_with("literal:") => {
                    match crate::languages::LanguageController::parse_expr_url(uri) {
                        Ok((lang, expr_addr)) => {
                            fetch_jobs.entry((lang, expr_addr)).or_insert(None);
                        }
                        Err(_) => record_resolved(current),
                    }
                }
                Value::Object(_) | Value::String(_) => record_resolved(current),
                Value::Null => {}
                _ => record_resolved(current),
            }
        }
    }

    // Pass 2: fire every unique `get_expression` call concurrently.  Order
    // of the resulting `Vec` matches the order of jobs we send in.
    if !fetch_jobs.is_empty() {
        let pairs: Vec<(String, String)> = fetch_jobs.keys().cloned().collect();
        let futures: Vec<_> = pairs
            .iter()
            .map(|(lang, addr)| controller.get_expression(lang, addr))
            .collect();
        let outputs = futures::future::join_all(futures).await;
        for (pair, out) in pairs.into_iter().zip(outputs.into_iter()) {
            let resolved = match out {
                Ok(Some(expr_json)) => {
                    let data = expr_json.get("data").cloned().unwrap_or(Value::Null);
                    Some(match &data {
                        Value::String(s) => serde_json::from_str(s).unwrap_or(data),
                        _ => data,
                    })
                }
                _ => None,
            };
            fetch_jobs.insert(pair, resolved);
        }
    }

    // Pass 3: walk again and write transformed values back.  String→fetch
    // misses (controller returned None / Err) fall back to the raw URI, as
    // the previous implementation did.
    for (inst_idx, instance) in instances.iter_mut().enumerate() {
        if let Some(prefilled) = already_resolved.get(&inst_idx) {
            for (name, val) in prefilled {
                let prop = match resolve_props.iter().find(|p| p.name == *name) {
                    Some(p) => p,
                    None => continue,
                };
                let default_transform = super::types::default_file_decode();
                let transform = prop.transform.as_ref().unwrap_or(&default_transform);
                instance[name] = eval_transform(transform, val, val);
            }
        }
        for prop in &resolve_props {
            let current = instance[&prop.name].clone();
            let resolved: Option<Value> = match &current {
                Value::String(uri) if !uri.starts_with("literal:") => {
                    match crate::languages::LanguageController::parse_expr_url(uri) {
                        Ok((lang, expr_addr)) => fetch_jobs
                            .get(&(lang, expr_addr))
                            .cloned()
                            .flatten()
                            .or_else(|| Some(current.clone())),
                        Err(_) => continue, // pre-resolved above
                    }
                }
                _ => continue, // pre-resolved above
            };
            if let Some(resolved) = resolved {
                let default_transform = super::types::default_file_decode();
                let transform = prop.transform.as_ref().unwrap_or(&default_transform);
                instance[&prop.name] = eval_transform(transform, &resolved, &resolved);
            }
        }
    }
    Ok(())
}
