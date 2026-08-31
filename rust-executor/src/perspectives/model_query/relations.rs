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

use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

use super::query::execute_model_query_inner;
use super::types::{
    IncludeValue, ModelQueryInput, ModelShape, ShapeRelation, ShapeResolver, WhereCondition,
};
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
    resolver: &dyn ShapeResolver,
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

        // Checked here, where the include is recognised, rather than down in
        // hydration: both resolvers return early when the relation holds no
        // targets, so validating later would accept the query on an empty
        // collection and reject the same query on a full one. Whether a request
        // is answerable must not depend on the data it is asking about.
        reject_pagination_on_polymorphic(rel_name, &sub_query)?;

        if rel.direction == "reverse" {
            resolve_reverse_include(store, instances, rel, &sub_query, resolver, depth).await?;
        } else {
            resolve_forward_include(store, instances, rel, &sub_query, resolver, depth).await?;
        }
    }
    Ok(())
}

/// Refuse `limit`/`offset` on a polymorphic include.
///
/// A polymorphic read runs one sub-query per class present, so a limit would
/// apply to each group separately: the same `limit: 5` returns five rows or
/// fifteen depending on how many classes the data happens to contain, which the
/// caller cannot see. Nor can it be applied afterwards without deciding which
/// classes lose their members.
///
/// Refused rather than dropped. Silently returning every row to a caller who
/// asked for five is wrong in a way that surfaces much later and only as
/// slowness, and an untyped include without `polymorphic` already fails loudly
/// rather than degrading.
fn reject_pagination_on_polymorphic(
    rel_name: &str,
    sub_query: &ModelQueryInput,
) -> Result<(), Error> {
    if !sub_query.polymorphic.unwrap_or(false) {
        return Ok(());
    }
    if sub_query.limit.is_some() || sub_query.offset.is_some() {
        return Err(anyhow!(
            "include on relation '{rel_name}': `limit`/`offset` are not supported on a \
             polymorphic include, because the read runs one query per class present and a \
             limit would apply to each of them separately. Drop them and the relation's own \
             link order decides what survives, or read one class at a time."
        ));
    }
    Ok(())
}

/// The JSON key carrying each polymorphically-hydrated instance's concrete
/// class, so the TypeScript layer can construct the right model class for it.
///
/// Half of a wire contract: the reader is `SUBJECT_CLASS_KEY` in
/// `core/src/model/Ad4mModel.ts`, and the two are separate literals in separate
/// languages. Renaming one alone does not fail to compile — it degrades to
/// "every instance stays plain JSON", which is the same shape as a caller having
/// declared no classes. `tests/js/tests/model/model-polymorphic.test.ts` asserts
/// both keys end to end, so the drift is caught there rather than in the field.
pub(crate) const SUBJECT_CLASS_KEY: &str = "__subjectClass";

/// The JSON key carrying every class a polymorphically-hydrated instance
/// conforms to, most specific first — the whole set [`SUBJECT_CLASS_KEY`] names
/// only the head of.
///
/// Always present, even where it holds a single name. A key that appeared only
/// when a target was ambiguous would be one every consumer had to guard, to
/// learn something a one-element array says just as well.
///
/// Same wire contract as [`SUBJECT_CLASS_KEY`], and the same counterpart file.
pub(crate) const SUBJECT_CLASSES_KEY: &str = "__subjectClasses";

/// Hydrate a heterogeneous set of target URIs, each as the class it actually is.
///
/// A relation whose targets are of mixed type cannot be hydrated against one
/// shape. `hydrate_one` only reads predicates present in the shape it is given,
/// so hydrating a subclass against its parent's shape does not merely mislabel it
/// — the subclass's own properties never enter the JSON at all. Choosing the right class
/// afterwards, in TypeScript, would therefore construct it over data that had
/// already been discarded.
///
/// So: classify the targets, group them by concrete class, and run one sub-query
/// per group against that group's own shape. The cost is one query per *distinct
/// class present*, not per instance.
///
/// Each instance carries its class name back under [`SUBJECT_CLASS_KEY`], which
/// is what lets the caller construct the matching model class rather than the
/// one the relation declared.
///
/// # One entry per link
///
/// Membership is structural and therefore not exclusive, so a base expression
/// can conform to two unrelated classes at once — a node carrying both classes'
/// flags is idiomatic AD4M, not malformed data. Such a target is still hydrated
/// **once**: links decide the relation's cardinality and classes decide only how
/// a target is read, so one link is one member. Returning a member per class
/// would make a three-link collection arrive with five entries, and every caller
/// would have to know to collapse them.
///
/// Which reading wins is therefore a policy, and this is where it is applied.
/// `subject_classes_of` ranks the set by specificity and declines to choose;
/// hydration needs exactly one shape, so it takes the head. Within an
/// inheritance chain that is genuinely the most derived class — a subclass
/// requires everything its parent does and more. Between two unrelated classes
/// the ranking ties and the alphabetical tie-break decides, which is arbitrary
/// but identical on every peer reading the same data. There is no better answer
/// available: with no `rdf:type` triple to appeal to, "which class is this
/// really" has no fact underneath it, and agreement between peers is worth more
/// than a locally plausible guess.
///
/// So that the discarded readings do not vanish without trace, every instance
/// also carries the full set under [`SUBJECT_CLASSES_KEY`]. A caller that wants
/// one of the others has its id and can query that class for it directly.
async fn hydrate_polymorphic(
    store: &SparqlStore,
    relation_name: &str,
    target_ids: &[String],
    sub_query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    depth: u8,
    hydrated: &mut HashMap<String, Value>,
    ordered_ids: &mut Vec<String>,
) -> Result<(), Error> {
    // `limit`/`offset` were refused before either resolver ran — see
    // `reject_pagination_on_polymorphic`. Nothing to check here.
    let classes =
        crate::perspectives::subject_classes_of::subject_classes_of(store, resolver, target_ids)?;

    // Group by class, keeping the ids of each group in the order they arrived so
    // a group's own results stay stable.
    //
    // `subject_classes_of` returns every class a URI conforms to, most specific
    // first. Hydration needs exactly one shape per target, so the first is taken
    // — hydrating against a parent is what this whole function exists to avoid.
    // The rest are not lost: they ride back on the instance below.
    let mut by_class: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for id in target_ids {
        if let Some(class_name) = classes.get(id).and_then(|names| names.first()) {
            by_class
                .entry(class_name.clone())
                .or_default()
                .push(id.clone());
        } else {
            // A target matching no registered class is skipped rather than
            // hydrated as something it is not. It stays absent from the
            // relation, which is the same outcome a non-conforming target has
            // always had — but from the outside it looks like a child that
            // simply is not there, and in a live neighbourhood it may only look
            // that way until the type link arrives. Leave a trail.
            log::debug!(
                "polymorphic include on '{relation_name}': '{id}' matches no registered class, skipping"
            );
        }
    }

    for (class_name, ids) in by_class {
        let target_shape = match resolver.get_shape(&class_name) {
            Ok(s) => s,
            Err(e) => {
                log::warn!("polymorphic include: no shape for '{class_name}': {e}");
                continue;
            }
        };

        let mut group_query = sub_query.clone();
        let mut wc = group_query.where_clause.take().unwrap_or_default();
        wc.insert("id".to_string(), WhereCondition::StringArray(ids));
        group_query.where_clause = Some(wc);
        // Already rejected above, so there is nothing here to drop — cleared
        // only so a group query cannot inherit one by some later edit.
        group_query.limit = None;
        group_query.offset = None;

        let result = Box::pin(execute_model_query_inner(
            store,
            target_shape.as_ref(),
            &group_query,
            resolver,
            depth + 1,
        ))
        .await?;

        for mut inst in result.instances {
            if let Some(id) = inst["id"].as_str().map(|s| s.to_string()) {
                if let Some(obj) = inst.as_object_mut() {
                    obj.insert(
                        SUBJECT_CLASS_KEY.to_string(),
                        Value::String(class_name.clone()),
                    );
                    // The readings this one was chosen over. Costs nothing to
                    // return — the classification that picked the head had to
                    // produce the whole set to rank it.
                    let all = classes
                        .get(&id)
                        .cloned()
                        .unwrap_or_else(|| vec![class_name.clone()]);
                    obj.insert(
                        SUBJECT_CLASSES_KEY.to_string(),
                        Value::Array(all.into_iter().map(Value::String).collect()),
                    );
                }
                ordered_ids.push(id.clone());
                hydrated.insert(id, inst);
            }
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
    resolver: &dyn ShapeResolver,
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
    let polymorphic = sub_query.polymorphic.unwrap_or(false);
    let target_ids = all_ids.clone();
    wc.insert("id".to_string(), WhereCondition::StringArray(all_ids));
    query.where_clause = Some(wc);

    // Ordering a polymorphic set by a property is only meaningful within a
    // concrete class — the classes need not share the property at all — so the
    // parent's link order is kept across classes.
    let has_sub_order = sub_query.order.is_some() && !polymorphic;

    let mut hydrated: HashMap<String, Value> = HashMap::new();
    let mut ordered_ids: Vec<String> = Vec::new();

    if polymorphic {
        hydrate_polymorphic(
            store,
            &rel.name,
            &target_ids,
            &query,
            resolver,
            depth,
            &mut hydrated,
            &mut ordered_ids,
        )
        .await?;
    } else {
        if rel.target_class_name.is_empty() {
            // An untyped relation names no class, so there is no shape to
            // hydrate its targets against. This used to surface as a shape
            // lookup for the empty string, which failed the whole query with a
            // message naming neither the relation nor the fix.
            return Err(anyhow!(
                "include on relation '{}': the relation declares no target class, so its \
                 targets cannot be hydrated. Either give it a target, or read it with \
                 `polymorphic: true` to hydrate each target as the class it actually is.",
                rel.name
            ));
        }
        let target_shape = resolver.get_shape(&rel.target_class_name)?;
        let result = Box::pin(execute_model_query_inner(
            store,
            target_shape.as_ref(),
            &query,
            resolver,
            depth + 1,
        ))
        .await?;

        ordered_ids = result
            .instances
            .iter()
            .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
            .collect();
        for inst in result.instances {
            if let Some(id) = inst["id"].as_str() {
                hydrated.insert(id.to_string(), inst);
            }
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
///
/// Honours `polymorphic` exactly as the forward path does. The inverse side of
/// a heterogeneous relation is heterogeneous for the same reason: whatever
/// points *at* this instance is whatever somebody linked, so a `parent` read
/// backwards from a block can be any of several container classes.
async fn resolve_reverse_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    sub_query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
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

    let polymorphic = sub_query.polymorphic.unwrap_or(false);
    // Ordering a polymorphic set by a property is only meaningful within a
    // concrete class — the classes need not share the property at all — so the
    // link order is kept across classes, as on the forward path.
    let has_sub_order = sub_query.order.is_some() && !polymorphic;
    let mut hydrated: HashMap<String, Value> = HashMap::new();
    let mut ordered_result_ids: Vec<String> = Vec::new();
    if !all_source_ids.is_empty() {
        let mut query = sub_query.clone();
        let mut wc = query.where_clause.take().unwrap_or_default();
        let target_ids: Vec<String> = if let Some(existing_id) = wc.get("id") {
            let filter_ids: Vec<String> = match existing_id {
                WhereCondition::String(s) => vec![s.clone()],
                WhereCondition::StringArray(arr) => arr.clone(),
                _ => vec![],
            };
            all_source_ids
                .into_iter()
                .filter(|id| filter_ids.contains(id))
                .collect()
        } else {
            all_source_ids
        };
        wc.insert(
            "id".to_string(),
            WhereCondition::StringArray(target_ids.clone()),
        );
        query.where_clause = Some(wc);

        if polymorphic {
            hydrate_polymorphic(
                store,
                &rel.name,
                &target_ids,
                &query,
                resolver,
                depth,
                &mut hydrated,
                &mut ordered_result_ids,
            )
            .await?;
        } else {
            if rel.target_class_name.is_empty() {
                // Same shape of failure as the forward path, and the same fix:
                // an untyped relation names no class, so there is nothing to
                // hydrate its sources against.
                return Err(anyhow!(
                    "include on relation '{}': the relation declares no target class, so its \
                     targets cannot be hydrated. Either give it a target, or read it with \
                     `polymorphic: true` to hydrate each target as the class it actually is.",
                    rel.name
                ));
            }
            let target_shape = resolver.get_shape(&rel.target_class_name)?;
            let result = Box::pin(execute_model_query_inner(
                store,
                target_shape.as_ref(),
                &query,
                resolver,
                depth + 1,
            ))
            .await?;

            ordered_result_ids = result
                .instances
                .iter()
                .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
                .collect();
            for inst in result.instances {
                if let Some(id) = inst["id"].as_str() {
                    hydrated.insert(id.to_string(), inst);
                }
            }
        }
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
