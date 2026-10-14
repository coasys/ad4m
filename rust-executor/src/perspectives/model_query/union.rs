//! One model query over several classes (#1238).
//!
//! A query names one class, so a view of mixed records — a canvas's placements
//! and edge routes, a feed of several kinds — used to run one query per class
//! and merge the results itself. That cannot page: a correct page of the union
//! needs `offset + limit` rows from every class. And it loses classification,
//! because each row is tagged with the class that was asked.
//!
//! [`execute_union_query`] answers the union in one pass. It reuses the
//! per-class pipeline and the polymorphic-include policy rather than a second
//! copy of either:
//!
//! ```text
//!  classes ──▶ 1. per class: execute_model_query_inner (scope + where, no paging)
//!                      │      rows hydrated as that class
//!                      ▼
//!              2. subject_classes_of(union ids) ──▶ choose() one requested class
//!                      │      among those whose step-1 query returned the record;
//!                      │      a record answered by two classes is kept once
//!                      ▼
//!              3. sort once (nulls last, then id), count, limit/offset once
//!                      ▼
//!              4. per chosen class: hydrate the page's ids with the caller's
//!                 include / projections / links / getters / properties
//! ```
//!
//! Semantics, as decided on the issue
//! (<https://github.com/coasys/ad4m/issues/1238#issuecomment-6081266040>):
//!
//! - `where` on a property a class does not declare excludes that class's rows,
//!   the same answer a per-class query gives. A record of several requested
//!   classes is kept when any of its readings passes, and is read as one that
//!   does.
//! - `order` sorts rows without the value last in both directions, then by id,
//!   the rule the store's `ORDER BY` applies to a single class.
//! - `limit` / `offset` apply once over the union; `totalCount` is the union
//!   after `where`.
//! - Every row carries `__subjectClass` (the class it was hydrated as) and
//!   `__subjectClasses` (every class it conforms to), as polymorphic members do.

use super::filtering::{compare_values, extract_sort_value};
use super::projection::resolve_projections;
use super::query::execute_model_query_inner;
use super::relations::{choose, SUBJECT_CLASSES_KEY, SUBJECT_CLASS_KEY};
use super::types::{
    ModelQueryInput, ModelQueryResult, OrderDirection, Scope, ShapeResolver, WhereCondition,
};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};

/// The classes a model query reads: one, or a union.
///
/// A one-element union is still a union: its rows carry `__subjectClass`, which
/// a single-class query's rows do not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryClasses {
    One(String),
    Union(Vec<String>),
}

impl From<&str> for QueryClasses {
    fn from(name: &str) -> Self {
        QueryClasses::One(name.to_string())
    }
}

impl From<String> for QueryClasses {
    fn from(name: String) -> Self {
        QueryClasses::One(name)
    }
}

impl QueryClasses {
    /// A union over `names`, sorted and without duplicates, so that two
    /// subscriptions naming the same classes in another order share one entry
    /// (#1311). Order only ever picked the fallback reading of a record of a
    /// class with no required triples; sorted, that fallback is alphabetical.
    pub fn union(mut names: Vec<String>) -> Self {
        names.sort();
        names.dedup();
        QueryClasses::Union(names)
    }

    pub fn names(&self) -> &[String] {
        match self {
            QueryClasses::One(name) => std::slice::from_ref(name),
            QueryClasses::Union(names) => names,
        }
    }
}

/// The sort the union is paged on: the caller's keys, or creation time when
/// they named none. Either way the id breaks the remaining ties.
fn effective_order(query: &ModelQueryInput) -> Vec<(String, OrderDirection)> {
    match &query.order {
        Some(order) if !order.is_empty() => order.clone(),
        _ => vec![("timestamp".to_string(), OrderDirection::ASC)],
    }
}

/// Order rows of several classes against each other.
///
/// Not [`super::filtering::sort_instances`]: that reverses the whole
/// comparison for `DESC`, nulls included, so a row without the value comes
/// first. The store's `ORDER BY` keeps such rows last in both directions
/// (`ASC(IF(BOUND(?_sort_str), 0, 1))`) and breaks ties by `?source` (#1227).
/// In a union "no value" is the common case — a class that does not declare
/// the key — so the union follows the store, and a union page orders rows the
/// way a single-class page does.
fn sort_union(rows: &mut [Value], order: &[(String, OrderDirection)]) {
    rows.sort_by(|a, b| {
        for (key, dir) in order {
            let av = extract_sort_value(a, key);
            let bv = extract_sort_value(b, key);
            let cmp = match (av.is_null(), bv.is_null()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => {
                    let c = compare_values(&av, &bv);
                    if *dir == OrderDirection::DESC {
                        c.reverse()
                    } else {
                        c
                    }
                }
            };
            if cmp != Ordering::Equal {
                return cmp;
            }
        }
        a["id"]
            .as_str()
            .unwrap_or("")
            .cmp(b["id"].as_str().unwrap_or(""))
    });
}

/// Refuse what has no single meaning over several classes.
fn refuse_unanswerable(classes: &[String], query: &ModelQueryInput) -> Result<(), Error> {
    if classes.is_empty() {
        return Err(anyhow!("classNames: name at least one class"));
    }
    // A per-anchor limit and a walk pick their rows inside the store, once per
    // class, so `limitPerAnchor: 5` would keep five per anchor *per class*.
    if let Some(Scope::Traverse {
        limit_per_anchor,
        levels,
        ..
    }) = &query.parent
    {
        if limit_per_anchor.is_some() || levels.is_some() {
            return Err(anyhow!(
                "classNames: a parent scope with `limitPerAnchor` or `levels` is not supported \
                 over several classes — each class would slice its own rows per anchor. Page the \
                 union with `limit`/`offset`, or query one class at a time."
            ));
        }
    }
    // A dotted key sorts on a related record, which needs every class to
    // declare that relation; the union is hydrated without includes until the
    // page is cut.
    if let Some(order) = &query.order {
        if let Some((key, _)) = order.iter().find(|(k, _)| k.contains('.')) {
            return Err(anyhow!(
                "classNames: order key '{key}' sorts on a relation, which is not supported over \
                 several classes. Order by a property, a timestamp or a `$` projection count."
            ));
        }
    }
    Ok(())
}

/// Execute one model query over several classes.
///
/// `classes` are bare class names, each resolved through `resolver`; a missing
/// shape surfaces as the resolver's error, so the caller's shape-wait loop
/// treats it exactly as it treats a single class.
pub async fn execute_union_query(
    store: &SparqlStore,
    classes: &[String],
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
) -> Result<ModelQueryResult, Error> {
    refuse_unanswerable(classes, query)?;
    let mut requested: Vec<String> = Vec::new();
    for c in classes {
        if !requested.contains(c) {
            requested.push(c.clone());
        }
    }

    let order = effective_order(query);
    // Projections an order key names are needed to sort; everything else the
    // caller asked to attach waits for the page.
    let sort_projections: Option<HashMap<_, _>> = query.projections.as_ref().map(|projs| {
        projs
            .iter()
            .filter(|(k, _)| order.iter().any(|(key, _)| key == *k))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    });

    // 1. Each class's rows in scope that pass `where`, hydrated as that class.
    //    No paging and no order: both apply to the union, not to a member.
    let select = ModelQueryInput {
        include: None,
        projections: None,
        links: None,
        properties: None,
        order: None,
        limit: None,
        offset: None,
        count: None,
        // Getters run after paging in the per-class pipeline too, so neither
        // `where` nor `order` sees them there; skipping them here changes nothing.
        deep_query: Some(false),
        ..query.clone()
    };
    let mut rows_by_class: HashMap<String, HashMap<String, Value>> = HashMap::new();
    // Every id in the order it was first seen, for a stable classification call.
    let mut all_ids: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for class in &requested {
        let shape = resolver.get_shape(class)?;
        let mut result =
            execute_model_query_inner(store, shape.as_ref(), &select, resolver, 0).await?;
        if let Some(projs) = sort_projections.as_ref().filter(|p| !p.is_empty()) {
            resolve_projections(
                store,
                &mut result.instances,
                projs,
                shape.as_ref(),
                resolver,
                0,
                query.link_status.as_ref(),
                query.include_unverified,
            )
            .await?;
        }
        let mut rows = HashMap::new();
        for row in result.instances {
            let Some(id) = row["id"].as_str().map(str::to_string) else {
                continue;
            };
            if seen.insert(id.clone()) {
                all_ids.push(id.clone());
            }
            rows.insert(id, row);
        }
        rows_by_class.insert(class.clone(), rows);
    }

    // 2. One row per record, as the class chosen for it. The choice is the
    //    polymorphic include's (`choose`), restricted to the requested classes
    //    whose step-1 query returned the record: `preferClasses` first, then
    //    the most specific of them. Step 1 already answered "does it pass as
    //    this class, under this read's `where` and link filters", so a record
    //    is kept whenever one of its readings passes — the answer a per-class
    //    query gives — and a reading the read cannot see (a Local or
    //    unverified flag under a shared read) can never be the one chosen.
    let classified =
        crate::perspectives::subject_classes_of::subject_classes_of(store, resolver, &all_ids)?;
    let preferred = query.prefer_classes.as_deref().unwrap_or(&[]);
    let mut union: Vec<Value> = Vec::new();
    let mut chosen_by_id: HashMap<String, String> = HashMap::new();
    for id in &all_ids {
        let conforms: Vec<String> = classified
            .get(id)
            .map(|all| {
                all.iter()
                    .filter(|c| {
                        requested.contains(c)
                            && rows_by_class.get(*c).is_some_and(|r| r.contains_key(id))
                    })
                    .cloned()
                    .collect()
            })
            .unwrap_or_default();
        // Classification is structural and skips a class with no required
        // triples; the store's conformance query still answers for it. Such a
        // record falls back to the first requested class that returned it.
        let chosen = match choose(&conforms, preferred) {
            Some(c) => c.clone(),
            None => match requested
                .iter()
                .find(|c| rows_by_class.get(*c).is_some_and(|r| r.contains_key(id)))
            {
                Some(c) => c.clone(),
                None => continue,
            },
        };
        let Some(mut row) = rows_by_class
            .get_mut(&chosen)
            .and_then(|rows| rows.remove(id))
        else {
            continue;
        };
        let all = classified
            .get(id)
            .cloned()
            .unwrap_or_else(|| vec![chosen.clone()]);
        if let Some(obj) = row.as_object_mut() {
            obj.insert(SUBJECT_CLASS_KEY.to_string(), Value::String(chosen.clone()));
            obj.insert(
                SUBJECT_CLASSES_KEY.to_string(),
                Value::Array(all.into_iter().map(Value::String).collect()),
            );
        }
        chosen_by_id.insert(id.clone(), chosen);
        union.push(row);
    }

    // 3. Order, count and cut the page once, over the union.
    sort_union(&mut union, &order);
    let total_count = union.len();
    if query.limit == Some(0) {
        return Ok(ModelQueryResult {
            instances: vec![],
            total_count,
        });
    }
    let page: Vec<String> = union
        .iter()
        .skip(query.offset.unwrap_or(0))
        .take(query.limit.unwrap_or(usize::MAX))
        .filter_map(|r| r["id"].as_str().map(str::to_string))
        .collect();
    let mut tags: HashMap<String, Value> = HashMap::new();
    for row in &union {
        if let Some(id) = row["id"].as_str() {
            tags.insert(id.to_string(), row[SUBJECT_CLASSES_KEY].clone());
        }
    }

    // 4. Hydrate the page as the caller asked, one query per chosen class.
    //    `where` and the scope were answered in step 1; the id list replaces them.
    let mut page_by_class: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for id in &page {
        if let Some(class) = chosen_by_id.get(id) {
            page_by_class
                .entry(class.clone())
                .or_default()
                .push(id.clone());
        }
    }
    let mut hydrated: HashMap<String, Value> = HashMap::new();
    for (class, ids) in page_by_class {
        let shape = resolver.get_shape(&class)?;
        let fetch = ModelQueryInput {
            parent: None,
            where_clause: Some(BTreeMap::from([(
                "id".to_string(),
                WhereCondition::StringArray(ids),
            )])),
            order: None,
            limit: None,
            offset: None,
            count: None,
            ..query.clone()
        };
        let result = execute_model_query_inner(store, shape.as_ref(), &fetch, resolver, 0).await?;
        for mut row in result.instances {
            let Some(id) = row["id"].as_str().map(str::to_string) else {
                continue;
            };
            if let Some(obj) = row.as_object_mut() {
                obj.insert(SUBJECT_CLASS_KEY.to_string(), Value::String(class.clone()));
                obj.insert(
                    SUBJECT_CLASSES_KEY.to_string(),
                    tags.get(&id).cloned().unwrap_or(Value::Null),
                );
            }
            hydrated.insert(id, row);
        }
    }

    Ok(ModelQueryResult {
        instances: page.iter().filter_map(|id| hydrated.remove(id)).collect(),
        total_count,
    })
}

#[cfg(test)]
#[path = "union_tests.rs"]
mod tests;
