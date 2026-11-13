//! The id phase from the scope index: the fast path of a paged query.
//!
//! The two-phase plan first asks the store for one page of instance ids, in
//! order, then hydrates those ids. For the most common page (the newest N
//! instances of a class, or of a class under one parent) the first query
//! groups every instance by its earliest flag link and sorts them all, so its
//! cost grows with the class. The scope index
//! ([`crate::perspectives::scope_index`]) keeps these lists sorted already, so
//! the page is one seek and a scan of N entries.
//!
//! [`index_page`] gives the ids the SPARQL id phase would give, or `None` when
//! the query is not one the index answers exactly. Then the caller runs the
//! SPARQL id phase as before. Hydration is the same on both paths.
//!
//! # When the index answers
//!
//! All of these must hold. Each one makes the index read equal the SPARQL
//! read:
//!
//! - The id phase sorts by the reifier timestamp (`order` absent, or one
//!   timestamp synonym, ascending or descending), and the store applies the
//!   window (no per-anchor limit, no walk, no filter after hydration).
//! - `limit` is set and not 0, and `offset` is at most [`MAX_INDEX_OFFSET`].
//! - No `where`, no `linkStatus`, and `includeUnverified` is not `true`: the
//!   index holds members by verified links of any status.
//! - The class has a flag, its rule in the index is the rule of this shape,
//!   and the class is ready.
//! - The SPARQL path orders by the same flag triple (its timestamp probe), and
//!   the shape's conformance is that flag triple alone.
//! - No member of the class has a timestamp that is not canonical: then the
//!   index's time order and SPARQL's string order agree.
//! - The parent is absent (the class scope), `{ id, predicate }`,
//!   `{ model, id, field }` with a field (one `Under` scope), or `{ id }` over
//!   containment (one `Under` scope per containment edge whose holder check
//!   passes on `id`). A traversal or a model scope without a field goes to
//!   SPARQL.
//!
//! # Order and ties
//!
//! SPARQL sorts by the earliest flag time, then by IRI ascending. The index
//! sorts by the earliest flag time, then by `seq`. The two differ only inside
//! one millisecond. So the read takes the first `offset + limit` entries and
//! then every entry with the same millisecond as the last one, and sorts what
//! it read by time and IRI before it cuts the window. A millisecond that holds
//! more than [`MAX_TIE_READ`] extra entries (a bulk import) goes to SPARQL.
//!
//! # Offset, not cursors
//!
//! A query names its window with `offset` and `limit`; it cannot carry a scope
//! cursor ([`crate::perspectives::scope_index::Cursor`]) yet, because that is a
//! change to the RPC contract and the SDK. An index read with an offset scans
//! the skipped entries, so its cost grows with the offset. Up to
//! [`MAX_INDEX_OFFSET`] that scan is still far cheaper than the SPARQL id
//! phase; past it the query goes to SPARQL, which costs the same at any
//! offset. Infinite scroll asks for small offsets first and seldom goes far.

use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::HashMap;

use super::containment::{edge_patterns, has_child_only, ContainmentEdge};
use super::sparql_builder::LinkGuard;
use super::types::{
    ContainedScope, ModelQueryInput, ModelShape, OrderDirection, Scope as QueryScope,
    ShapeProperty, SortKey, SparqlPagination,
};
use super::utils::{emittable_iri, validate_iri};
use crate::perspectives::scope_index::writer::ClassRule;
use crate::perspectives::scope_index::{MemberKey, Readiness, Scope};
use crate::perspectives::sparql_store::{EvalStop, SparqlStore};

/// The largest `offset` the index serves. See the module documentation.
pub(super) const MAX_INDEX_OFFSET: usize = 1000;

/// The most entries one millisecond may add past the window. See the module
/// documentation.
pub(super) const MAX_TIE_READ: usize = 10_000;

/// A page of ids from the index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct IndexPage {
    /// The page, in the query's order.
    pub ids: Vec<String>,
    /// The number of instances in the whole scope, when the index knows it
    /// (one scope). `None` for a union of scopes: the caller counts it.
    pub total: Option<usize>,
}

/// The ids of `query`'s page from the scope index, or `None` when the index
/// does not answer this query exactly (see the module documentation).
///
/// `pagination` is the id phase's plan. The caller passes it only when the
/// store applies the window: no per-anchor limit, no walk, no filter after
/// hydration.
pub(super) fn index_page(
    stop: &EvalStop,
    store: &SparqlStore,
    shape: &ModelShape,
    query: &ModelQueryInput,
    pagination: &SparqlPagination,
) -> Result<Option<IndexPage>, Error> {
    let Some(rule) = indexed_rule(store, shape, query, pagination) else {
        return Ok(None);
    };
    let Some(scopes) = scopes_of(stop, store, &rule, query)? else {
        return Ok(None);
    };
    let reader = store.scope_reader();
    let noncanonical = reader.noncanonical(&rule.class)?;
    if noncanonical != 0 {
        if store.scope_index_first_noncanonical(&rule) {
            log::warn!(
                "Scope index: class '{}' has {noncanonical} member(s) whose flag timestamp is \
                 not in the executor's form; its queries use the SPARQL path",
                rule.class
            );
        }
        return Ok(None);
    }
    let offset = pagination.offset.unwrap_or(0);
    let Some(limit) = pagination.limit else {
        return Ok(None);
    };
    let newest_first = pagination.direction == OrderDirection::DESC;
    let need = offset + limit;

    // Every scope's first `need` entries and its boundary millisecond. The
    // same member has the same key in every scope of its class.
    let mut read: HashMap<String, MemberKey> = HashMap::new();
    for scope in &scopes {
        let Some(entries) = read_scope(store, scope, newest_first, need)? else {
            return Ok(None);
        };
        read.extend(entries);
    }
    let mut members: Vec<(String, MemberKey)> = read.into_iter().collect();
    members.sort_by(|(a_id, a), (b_id, b)| {
        let by_time = if newest_first {
            b.t_ms().cmp(&a.t_ms())
        } else {
            a.t_ms().cmp(&b.t_ms())
        };
        by_time.then_with(|| a_id.cmp(b_id))
    });
    let ids = members
        .into_iter()
        .skip(offset)
        .take(limit)
        .map(|(id, _)| id)
        .collect();
    let total = match scopes.as_slice() {
        [] => Some(0),
        [one] => Some(reader.count(one)?.max(0) as usize),
        _ => None,
    };
    #[cfg(test)]
    PAGES_SERVED.with(|n| n.set(n.get() + 1));
    Ok(Some(IndexPage { ids, total }))
}

#[cfg(test)]
thread_local! {
    /// Pages [`index_page`] gave on this thread, so a test can tell which
    /// path answered a query.
    pub(super) static PAGES_SERVED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// The first `need` entries of `scope` in the order asked, plus every later
/// entry in the last one's millisecond. `None` when that millisecond holds
/// more than [`MAX_TIE_READ`] entries past `need`.
fn read_scope(
    store: &SparqlStore,
    scope: &Scope,
    newest_first: bool,
    need: usize,
) -> Result<Option<Vec<(String, MemberKey)>>, Error> {
    let reader = store.scope_reader();
    let mut out: Vec<(String, MemberKey)> = Vec::with_capacity(need);
    for item in reader.entries(scope.id(), None, None, newest_first) {
        let (member, key) = item?;
        if out.len() >= need {
            let boundary = out.last().map(|(_, k)| k.t_ms());
            if boundary != Some(key.t_ms()) {
                break;
            }
            if out.len() >= need + MAX_TIE_READ {
                return Ok(None);
            }
        }
        out.push((member, key));
    }
    Ok(Some(out))
}

/// The class's rule, when every condition on the query and the shape holds
/// (all but the parent and the noncanonical count).
fn indexed_rule(
    store: &SparqlStore,
    shape: &ModelShape,
    query: &ModelQueryInput,
    pagination: &SparqlPagination,
) -> Option<ClassRule> {
    if !matches!(pagination.sort_key, SortKey::Timestamp)
        || pagination.limit.is_none_or(|l| l == 0)
        || pagination.offset.unwrap_or(0) > MAX_INDEX_OFFSET
    {
        return None;
    }
    if query.where_clause.as_ref().is_some_and(|w| !w.is_empty())
        || query.link_status.is_some()
        || query.include_unverified == Some(true)
        || query.walked.is_some()
    {
        return None;
    }
    let order_is_time = match &query.order {
        None => true,
        Some(order) => {
            order.len() == 1
                && matches!(order[0].0.as_str(), "timestamp" | "createdAt" | "updatedAt")
        }
    };
    if !order_is_time {
        return None;
    }
    let rule = ClassRule::from_shape(shape)?;
    if store.scope_index_rule(&rule.class).as_ref() != Some(&rule)
        || store.scope_index_readiness(&rule) != Readiness::Ready
    {
        return None;
    }
    (probe_is_flag(shape, &rule) && conformance_is_flag(shape, &rule)).then_some(rule)
}

/// Whether `prop` is the flag property of `rule`.
fn is_rule_flag(prop: &ShapeProperty, rule: &ClassRule) -> bool {
    prop.is_flag
        && prop.predicate == rule.flag_predicate
        && prop.initial_value.as_deref() == Some(rule.flag_value.as_str())
}

/// Whether the SPARQL id phase's timestamp probe reads the flag triple of
/// `rule` (the same choice as `sparql_builder::build_timestamp_probe`).
fn probe_is_flag(shape: &ModelShape, rule: &ClassRule) -> bool {
    shape
        .properties
        .iter()
        .find(|p| {
            p.is_flag
                && p.initial_value.as_deref().is_some_and(emittable_iri)
                && validate_iri(&p.predicate).is_ok()
        })
        .is_some_and(|p| is_rule_flag(p, rule))
}

/// Whether the shape's conformance patterns are the flag triple of `rule`
/// alone (the same tiers as `sparql_builder::shape_conformance_patterns`):
/// every required property with a valid predicate is the flag, or there is
/// none and the first property with an initial value is the flag.
fn conformance_is_flag(shape: &ModelShape, rule: &ClassRule) -> bool {
    let required: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.is_required && validate_iri(&p.predicate).is_ok())
        .collect();
    if !required.is_empty() {
        return required.iter().all(|p| is_rule_flag(p, rule));
    }
    shape
        .properties
        .iter()
        .find(|p| p.initial_value.is_some() && validate_iri(&p.predicate).is_ok())
        .is_some_and(|p| is_rule_flag(p, rule))
}

/// The scopes whose union is the query's parent scope, or `None` when the
/// parent is not one the index holds.
fn scopes_of(
    stop: &EvalStop,
    store: &SparqlStore,
    rule: &ClassRule,
    query: &ModelQueryInput,
) -> Result<Option<Vec<Scope>>, Error> {
    let Some(parent) = &query.parent else {
        return Ok(Some(vec![rule.class_scope()]));
    };
    let under = |id: &str, predicate: &str| -> Option<Vec<Scope>> {
        (emittable_iri(id) && emittable_iri(predicate)).then(|| vec![rule.under(id, predicate)])
    };
    match parent {
        QueryScope::Raw { id, predicate } => Ok(under(id, predicate)),
        QueryScope::Model {
            id,
            field: Some(field),
            ..
        } => Ok(under(id, field)),
        QueryScope::Contained(ContainedScope { id }) => {
            if !emittable_iri(id) {
                return Ok(None);
            }
            let edges = query.containment.clone().unwrap_or_else(has_child_only);
            let mut scopes = Vec::new();
            for edge in edges.iter() {
                if !emittable_iri(&edge.predicate) {
                    return Ok(None);
                }
                if holds_through(stop, store, edge, id, LinkGuard::of(query))? {
                    scopes.push(rule.under(id, &edge.predicate));
                }
            }
            Ok(Some(scopes))
        }
        QueryScope::Model { field: None, .. } | QueryScope::Traverse { .. } => Ok(None),
    }
}

/// Whether `id` can hold children through `edge`: always for an edge any
/// node holds through (`has_child`), else when `id` holds at least one node
/// through the edge with its holder check. The holder check reads only `id`,
/// so when one child passes it, the edge's children under `id` are exactly the
/// `Under` scope of its predicate.
fn holds_through(
    stop: &EvalStop,
    store: &SparqlStore,
    edge: &ContainmentEdge,
    id: &str,
    guard: LinkGuard,
) -> Result<bool, Error> {
    if edge.holder.is_none() {
        return Ok(true);
    }
    let sparql = format!(
        "SELECT ?_held WHERE {{ {} }} LIMIT 1",
        edge_patterns(edge, 0, &format!("<{id}>"), "?_held", guard)
    );
    let rows: Vec<Value> = serde_json::from_str(&store.query(stop, &sparql)?)?;
    Ok(!rows.is_empty())
}
