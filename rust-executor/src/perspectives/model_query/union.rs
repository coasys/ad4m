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
//!  classes ──▶ 1. per class: select_ids (scope + where, no paging): each id
//!                      │      and its sort value, nothing hydrated; a class
//!                      │      whose where/order the store cannot answer runs
//!                      │      execute_model_query_inner and reads its rows
//!                      ▼
//!              2. count; a record one class returned is read as it, one several
//!                      │      returned goes to subject_classes_of ──▶ choose()
//!                      │      among those classes; each record is kept once
//!                      ▼
//!              3. sort once (nulls last, then id), limit/offset once
//!                      ▼
//!              4. per chosen class: hydrate the page's ids with the caller's
//!                 include / projections / links / getters / properties
//! ```
//!
//! Only the page is hydrated (PERF-29): a union read costs ids for every
//! record and rows for the page, as a single-class page does.
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
use super::query::{execute_model_query_inner, select_ids, walk_order, with_walk};
use super::relations::{choose, SUBJECT_CLASSES_KEY, SUBJECT_CLASS_KEY};
use super::types::{
    ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection, ProjectionInput, Scope,
    ShapeResolver, SortKey, WhereCondition,
};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

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

/// One record of the union before it is hydrated: its id, the class it is read
/// as, and its value for each order key, in the order's order.
struct Member {
    id: String,
    class: String,
    keys: Vec<Value>,
}

/// Order records of several classes against each other.
///
/// Not [`super::filtering::sort_instances`]: that reverses the whole
/// comparison for `DESC`, nulls included, so a row without the value comes
/// first. The store's `ORDER BY` keeps such rows last in both directions
/// (`ASC(IF(BOUND(?_sort_str), 0, 1))`) and breaks ties by `?source` (#1227).
/// In a union "no value" is the common case — a class that does not declare
/// the key — so the union follows the store, and a union page orders rows the
/// way a single-class page does.
fn sort_union(members: &mut [Member], order: &[(String, OrderDirection)]) {
    members.sort_by(|a, b| {
        for (i, (_, dir)) in order.iter().enumerate() {
            let av = a.keys.get(i).unwrap_or(&Value::Null);
            let bv = b.keys.get(i).unwrap_or(&Value::Null);
            let cmp = match (av.is_null(), bv.is_null()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => {
                    let c = compare_values(av, bv);
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
        a.id.cmp(&b.id)
    });
}

/// Where a class's value for one order key comes from when the class's id
/// phase answers it, so the record is not hydrated to be sorted.
enum KeySource {
    /// Creation time: `?_first_ts`, the time a single-class page sorts on.
    FirstTimestamp,
    /// A scalar property, as `?_sort_str`: the lexical value `parse_literal`
    /// reads, which [`compare_values`] orders as it orders the hydrated one.
    Property(String),
    /// The record's id.
    Id,
    /// A key this class's rows never carry: null on every record.
    Absent,
}

/// The id phase's source for `key` on `shape`, or `None` when only the
/// hydrated row has the value the union sorts on: `updatedAt` and `author`
/// (link metadata the id phase does not bind), a `$` projection count, and a
/// property whose hydrated value is not its stored literal (a collection, a
/// flag, a relation, a getter, a transform, a reverse or local property, or one
/// a language other than `literal` resolves).
fn key_source(shape: &ModelShape, key: &str) -> Option<KeySource> {
    match key {
        "timestamp" | "createdAt" => return Some(KeySource::FirstTimestamp),
        "id" | "baseExpression" => return Some(KeySource::Id),
        "updatedAt" | "author" => return None,
        _ if key.starts_with('$') => return None,
        _ => {}
    }
    match shape.properties.iter().find(|p| p.name == key) {
        None if shape.include_relations.iter().all(|r| r.name != key) => Some(KeySource::Absent),
        Some(p)
            if !p.is_collection
                && !p.is_flag
                && !p.is_scalar_relation
                && !p.local
                && !p.predicate.is_empty()
                && p.getter.is_none()
                && p.transform.is_none()
                && p.direction.as_deref() != Some("reverse")
                && matches!(p.resolve_language.as_deref(), None | Some("literal")) =>
        {
            Some(KeySource::Property(p.predicate.clone()))
        }
        _ => None,
    }
}

/// A class's records in scope that pass `where`, each with its value for every
/// key of `order`, from the class's id phase: nothing is hydrated. `None` when
/// the id phase cannot answer — a `where` matched after hydration, or an order
/// needing a value only the hydrated row has, or two the store sorts on
/// differently (it binds one sort value per query).
async fn members_from_ids(
    store: &SparqlStore,
    shape: &ModelShape,
    select: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    order: &[(String, OrderDirection)],
) -> Result<Option<Vec<(String, Vec<Value>)>>, Error> {
    let Some(sources) = order
        .iter()
        .map(|(key, _)| key_source(shape, key))
        .collect::<Option<Vec<_>>>()
    else {
        return Ok(None);
    };
    let mut predicates = sources.iter().filter_map(|s| match s {
        KeySource::Property(p) => Some(p),
        _ => None,
    });
    let sort_key = match predicates.next() {
        Some(p) if predicates.all(|q| q == p) => {
            if sources
                .iter()
                .any(|s| matches!(s, KeySource::FirstTimestamp))
            {
                return Ok(None);
            }
            SortKey::Property(p.clone())
        }
        Some(_) => return Ok(None),
        None => SortKey::Timestamp,
    };
    let Some(rows) = select_ids(store, shape, select, resolver, sort_key).await? else {
        return Ok(None);
    };
    Ok(Some(
        rows.into_iter()
            .filter_map(|row| {
                let id = row["source"].as_str()?.to_string();
                let keys = sources
                    .iter()
                    .map(|source| match source {
                        KeySource::FirstTimestamp => row["_first_ts"].clone(),
                        KeySource::Property(_) => row["_sort_str"].clone(),
                        KeySource::Id => Value::String(id.clone()),
                        KeySource::Absent => Value::Null,
                    })
                    .collect();
                Some((id, keys))
            })
            .collect(),
    ))
}

/// [`members_from_ids`] for a class whose id phase cannot answer: its rows
/// hydrated as that class (with the projections an order key names), and each
/// key read off the row.
async fn members_from_rows(
    store: &SparqlStore,
    shape: &ModelShape,
    select: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    order: &[(String, OrderDirection)],
    sort_projections: Option<&HashMap<String, ProjectionInput>>,
) -> Result<Vec<(String, Vec<Value>)>, Error> {
    let mut result = execute_model_query_inner(store, shape, select, resolver, 0).await?;
    if let Some(projs) = sort_projections.filter(|p| !p.is_empty()) {
        resolve_projections(
            store,
            &mut result.instances,
            projs,
            shape,
            resolver,
            0,
            select.link_status.as_ref(),
            select.include_unverified,
        )
        .await?;
    }
    Ok(result
        .instances
        .iter()
        .filter_map(|row| {
            let id = row["id"].as_str()?.to_string();
            let keys = order
                .iter()
                .map(|(key, _)| extract_sort_value(row, key))
                .collect();
            Some((id, keys))
        })
        .collect())
}

/// Refuse what has no single meaning over several classes.
fn refuse_unanswerable(classes: &[String], query: &ModelQueryInput) -> Result<(), Error> {
    if classes.is_empty() {
        return Err(anyhow!("classNames: name at least one class"));
    }
    refuse_union_reads(query)
}

/// The reads a union cannot answer, whatever its classes. A query over an
/// abstract class (#1435) checks these before it resolves its classes, so
/// whether it is refused does not depend on what happens to be installed.
pub(crate) fn refuse_union_reads(query: &ModelQueryInput) -> Result<(), Error> {
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
                "classNames / abstract class: a parent scope with `limitPerAnchor` or `levels` \
                 is not supported over several classes — each class would slice its own rows \
                 per anchor. Page the union with `limit`/`offset`, or query one class at a time."
            ));
        }
    }
    // A dotted key sorts on a related record, which needs every class to
    // declare that relation; the union is hydrated without includes until the
    // page is cut.
    if let Some(order) = &query.order {
        if let Some((key, _)) = order.iter().find(|(k, _)| k.contains('.')) {
            return Err(anyhow!(
                "classNames / abstract class: order key '{key}' sorts on a relation, which is \
                 not supported over several classes. Order by a property, a timestamp or a `$` \
                 projection count."
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
    execute_union_query_with_views(store, classes, query, resolver, &HashMap::new()).await
}

/// [`execute_union_query`], reading each class in `views` through its view
/// instead of its shape: a query over an abstract class (#1435), whose rows
/// carry the abstract class's names. Records an `include` reaches are read
/// through `resolver`, as their own class.
pub async fn execute_union_query_with_views(
    store: &SparqlStore,
    classes: &[String],
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    views: &HashMap<String, Arc<ModelShape>>,
) -> Result<ModelQueryResult, Error> {
    refuse_unanswerable(classes, query)?;
    // A walk is made once for the union, not once per class: every class
    // reads the same reached nodes, and their order is the walk's.
    let walked_query = with_walk(store, query)?;
    let query = walked_query.as_ref().unwrap_or(query);
    let walk_order = walk_order(query);
    let shape_of = |class: &str| match views.get(class) {
        Some(view) => Ok(view.clone()),
        None => resolver.get_shape(class),
    };
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

    // 1. Each class's records in scope that pass `where`, with their value for
    //    each order key: from the class's id phase, so nothing is hydrated, or,
    //    for a class whose `where` or order the store cannot answer, from its
    //    rows hydrated as that class. No paging and no order: both apply to
    //    the union, not to a member. A containment walk with no order sorts on
    //    the walk, so no key is read.
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
    let sort_keys: &[(String, OrderDirection)] = if walk_order.is_some() { &[] } else { &order };
    let mut keys_by_class: HashMap<String, HashMap<String, Vec<Value>>> = HashMap::new();
    // Every id in the order it was first seen.
    let mut all_ids: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for class in &requested {
        let shape = shape_of(class)?;
        let members =
            match members_from_ids(store, shape.as_ref(), &select, resolver, sort_keys).await? {
                Some(members) => members,
                None => {
                    members_from_rows(
                        store,
                        shape.as_ref(),
                        &select,
                        resolver,
                        sort_keys,
                        sort_projections.as_ref(),
                    )
                    .await?
                }
            };
        let mut keyed = HashMap::new();
        for (id, keys) in members {
            if seen.insert(id.clone()) {
                all_ids.push(id.clone());
            }
            keyed.insert(id, keys);
        }
        keys_by_class.insert(class.clone(), keyed);
    }
    // The union after `where`: each record once, however many classes return it.
    let total_count = all_ids.len();
    if query.limit == Some(0) {
        return Ok(ModelQueryResult {
            instances: vec![],
            total_count,
        });
    }
    let readers_of = |id: &str| -> Vec<&String> {
        requested
            .iter()
            .filter(|c| keys_by_class.get(*c).is_some_and(|k| k.contains_key(id)))
            .collect()
    };

    // 2. One reading per record. The choice is the polymorphic include's
    //    (`choose`), restricted to the requested classes whose step-1 query
    //    returned the record: `preferClasses` first, then the most specific of
    //    them. Step 1 already answered "does it pass as this class, under this
    //    read's `where` and link filters", so a record is kept whenever one of
    //    its readings passes — the answer a per-class query gives — and a
    //    reading the read cannot see (a Local or unverified flag under a shared
    //    read) can never be the one chosen. A record one class returned is read
    //    as that class without asking; only records several returned are
    //    classified before the page is cut, since their reading decides their
    //    sort value.
    let shared: Vec<String> = all_ids
        .iter()
        .filter(|id| readers_of(id).len() > 1)
        .cloned()
        .collect();
    let mut classified: HashMap<String, Vec<String>> = if shared.is_empty() {
        HashMap::new()
    } else {
        crate::perspectives::subject_classes_of::subject_classes_of(store, resolver, &shared)?
    };
    let preferred = query.prefer_classes.as_deref().unwrap_or(&[]);
    let mut union: Vec<Member> = Vec::with_capacity(all_ids.len());
    for id in &all_ids {
        let readers = readers_of(id);
        let Some(first) = readers.first() else {
            continue;
        };
        let chosen = if readers.len() == 1 {
            (*first).clone()
        } else {
            let conforms: Vec<String> = classified
                .get(id)
                .map(|all| {
                    all.iter()
                        .filter(|c| readers.contains(c))
                        .cloned()
                        .collect()
                })
                .unwrap_or_default();
            // Classification is structural and skips a class with no required
            // triples; the store's conformance query still answers for it. Such
            // a record falls back to the first requested class that returned it.
            choose(&conforms, preferred)
                .cloned()
                .unwrap_or_else(|| (*first).clone())
        };
        let keys = keys_by_class
            .get(&chosen)
            .and_then(|k| k.get(id))
            .cloned()
            .unwrap_or_default();
        union.push(Member {
            id: id.clone(),
            class: chosen,
            keys,
        });
    }

    // 3. Order and cut the page once, over the union. A containment walk with
    //    no order keeps the walk's: nearest first.
    match &walk_order {
        Some(positions) => {
            union.sort_by_key(|m| positions.get(&m.id).copied().unwrap_or(usize::MAX))
        }
        None => sort_union(&mut union, &order),
    }
    let page: Vec<Member> = union
        .into_iter()
        .skip(query.offset.unwrap_or(0))
        .take(query.limit.unwrap_or(usize::MAX))
        .collect();
    // Every class each page record conforms to, for `__subjectClasses`.
    let shared: HashSet<String> = shared.into_iter().collect();
    let unclassified: Vec<String> = page
        .iter()
        .filter(|m| !shared.contains(&m.id))
        .map(|m| m.id.clone())
        .collect();
    if !unclassified.is_empty() {
        classified.extend(crate::perspectives::subject_classes_of::subject_classes_of(
            store,
            resolver,
            &unclassified,
        )?);
    }

    // 4. Hydrate the page as the caller asked, one query per chosen class.
    //    `where` and the scope were answered in step 1; the id list replaces them.
    let mut page_by_class: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for member in &page {
        page_by_class
            .entry(member.class.clone())
            .or_default()
            .push(member.id.clone());
    }
    let mut hydrated: HashMap<String, Value> = HashMap::new();
    for (class, ids) in page_by_class {
        let shape = shape_of(&class)?;
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
                let all = classified
                    .get(&id)
                    .cloned()
                    .unwrap_or_else(|| vec![class.clone()]);
                obj.insert(
                    SUBJECT_CLASSES_KEY.to_string(),
                    Value::Array(all.into_iter().map(Value::String).collect()),
                );
            }
            hydrated.insert(id, row);
        }
    }

    Ok(ModelQueryResult {
        instances: page.iter().filter_map(|m| hydrated.remove(&m.id)).collect(),
        total_count,
    })
}

#[cfg(test)]
#[path = "union_tests.rs"]
mod tests;
