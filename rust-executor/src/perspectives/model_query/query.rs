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
    all_where_pushable, build_count_sparql, build_instance_sparql, level_limits,
    local_status_filter, per_anchor_limit, ANCHOR_VAR,
};
use super::types::{
    InstanceQueryPlan, ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection, Scope,
    ScopeDirection, ShapeResolver, SortKey, SparqlPagination,
};
use super::utils::{validate_iri, values_or_str_filter, MAX_INCLUDE_DEPTH};
use crate::perspectives::link_visibility::{viewer_author_filter, viewer_edge_filter};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::{HashMap, HashSet};

/// The anchors, predicate and direction a walk starts from.
///
/// `None` for any scope that is not a traversal — the walk is meaningless without a relation to
/// follow, and the caller falls back to asking once.
fn walk_roots(query: &ModelQueryInput) -> Option<(Vec<String>, String, ScopeDirection)> {
    match &query.parent {
        Some(Scope::Traverse {
            ids,
            predicate,
            direction,
            ..
        }) => Some((ids.clone(), predicate.clone(), *direction)),
        _ => None,
    }
}

/// Resolve a transitive traversal for `viewer_did`: every `(anchor, node)`
/// pair the anchor reaches through links the viewer may see.
///
/// A `+` path cannot filter its hops, so the builder cannot express this. It
/// is walked here instead, one hop per query over the whole frontier, and the
/// builder then keeps only these pairs
/// ([`VisibleReach`](super::sparql_builder::VisibleReach)). Without it, a node
/// reachable only through another user's `Local` link would be returned.
///
/// `None` when there is nothing to resolve: executor scope, or a scope that is
/// not a transitive traversal. The walk follows the predicate through nodes of
/// any class, as the `+` path does, and stops when a hop reaches nothing new.
async fn visible_reach(
    store: &SparqlStore,
    query: &ModelQueryInput,
    viewer_did: Option<&str>,
) -> Result<Option<Vec<(String, String)>>, Error> {
    let (
        Some(did),
        Some(Scope::Traverse {
            ids,
            predicate,
            transitive: true,
            direction,
            ..
        }),
    ) = (viewer_did, &query.parent)
    else {
        return Ok(None);
    };
    // The builder matches nothing for an unwritable predicate, and drops an
    // anchor that is not a valid IRI. Walk from the same anchors.
    let Ok(predicate) = validate_iri(predicate) else {
        return Ok(Some(vec![]));
    };
    let edge = match direction {
        ScopeDirection::Out => format!("?from <{predicate}> ?to"),
        ScopeDirection::In => format!("?to <{predicate}> ?from"),
    };
    let visible = viewer_edge_filter(Some(did), &edge, "reach");

    let anchors: Vec<&String> = ids.iter().filter(|id| validate_iri(id).is_ok()).collect();
    let mut reached: HashMap<&str, HashSet<String>> = anchors
        .iter()
        .map(|a| (a.as_str(), HashSet::new()))
        .collect();
    let mut frontier: Vec<(&str, String)> = anchors
        .iter()
        .map(|a| (a.as_str(), a.to_string()))
        .collect();
    let mut pairs = Vec::new();

    while !frontier.is_empty() {
        let from: Vec<String> = frontier
            .iter()
            .map(|(_, node)| node.clone())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();
        let sparql = format!(
            "SELECT DISTINCT ?from ?to WHERE {{\n    {edge} .\n    {}\n{visible}}}",
            values_or_str_filter("from", &from)
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query_async(&sparql).await?)?;
        let mut next_of: HashMap<&str, Vec<&str>> = HashMap::new();
        for row in &rows {
            if let (Some(from), Some(to)) = (row["from"].as_str(), row["to"].as_str()) {
                next_of.entry(from).or_default().push(to);
            }
        }

        let mut next = Vec::new();
        for (anchor, node) in &frontier {
            for to in next_of.get(node.as_str()).into_iter().flatten() {
                let seen = reached.get_mut(anchor).expect("every anchor is seeded");
                if seen.insert(to.to_string()) {
                    pairs.push((anchor.to_string(), to.to_string()));
                    next.push((*anchor, to.to_string()));
                }
            }
        }
        frontier = next;
    }
    Ok(Some(pairs))
}

/// Keep the first `n` rows of each anchor, discarding the rest.
///
/// SPARQL cannot say this: it has no window functions and its sub-SELECTs are uncorrelated, so
/// "five replies under each of these twenty comments" has no expression in the query language.
/// Applied between the phases, the cost stays proportional to rows of one string rather than to
/// records — the widest branch is over-fetched as ids and never hydrated.
///
/// The rows arrive in the order the `ORDER BY` put them, so keeping the first N of each anchor
/// keeps the *right* N.
fn slice_per_anchor(rows: &mut Vec<Value>, n: usize) {
    let before = rows.len();
    let mut kept: HashMap<String, usize> = HashMap::new();
    rows.retain(|row| {
        // A row with no anchor cannot be attributed to one, so it is kept rather than dropped:
        // losing rows silently is worse than a slice that is occasionally too generous.
        let Some(anchor) = row[ANCHOR_VAR].as_str() else {
            return true;
        };
        let seen = kept.entry(anchor.to_string()).or_insert(0);
        if *seen < n {
            *seen += 1;
            true
        } else {
            false
        }
    });
    log::debug!(
        "Per-anchor limit {}: kept {} of {} ids across {} anchors",
        n,
        rows.len(),
        before,
        kept.len()
    );
}

/// Walk a relation level by level, keeping `levels[depth]` results per anchor at each depth.
///
/// This is the whole reason the walk belongs in the executor. A caller can drive it — ask for one
/// level, use the ids as the next level's anchors — but each step is then a network round trip, and
/// a client that draws as each answer lands shows the tree assembling itself a level at a time.
/// Here the steps are sequential SPARQL against a store in the same process, with no serialisation
/// between them, and the records are hydrated once for the union of every level. Three levels cost
/// one request and one hydration instead of three of each.
///
/// Returns the ids in breadth-first order, which is also the order the caller's rows come back in.
/// A node already seen is not walked again, so a cycle terminates rather than looping.
#[allow(clippy::too_many_arguments)]
async fn walk_levels(
    store: &SparqlStore,
    shape: &ModelShape,
    query_input: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    pagination: &SparqlPagination,
    roots: Vec<String>,
    predicate: &str,
    direction: ScopeDirection,
    levels: &[usize],
    viewer_did: Option<&str>,
) -> Result<Vec<String>, Error> {
    let mut ordered: Vec<String> = Vec::new();
    // Seeded with the roots so that a cycle back to an anchor neither returns
    // the anchor as its own descendant nor walks it a second time.
    let mut seen: HashSet<String> = roots.iter().cloned().collect();
    let mut frontier = roots;

    for (depth, limit) in levels.iter().enumerate() {
        if frontier.is_empty() {
            break;
        }
        // One level, expressed as an ordinary scoped query over every anchor on it. The builder
        // then needs no notion of a walk at all — it keeps emitting one level, and this drives it.
        let mut level_input = query_input.clone();
        level_input.parent = Some(Scope::Traverse {
            ids: frontier.clone(),
            predicate: predicate.to_string(),
            transitive: false,
            direction,
            limit_per_anchor: Some(*limit),
            levels: None,
        });
        // Paging belongs to the caller's whole result, not to a level of the walk.
        level_input.limit = None;
        level_input.offset = None;

        let plan = build_instance_sparql(
            shape,
            &level_input,
            Some(pagination),
            Some(resolver),
            viewer_did,
            None,
        );
        let InstanceQueryPlan::TwoPhase {
            pagination_subquery,
            ..
        } = plan
        else {
            log::warn!(
                "walk_levels: expected a two-phase plan at depth {depth}, stopping the walk"
            );
            break;
        };

        let json = store.query_async(&pagination_subquery).await?;
        let rows: Vec<Value> = serde_json::from_str(&json)?;

        // Slicing and de-duplication are one pass, not two, because a node that is not going to be
        // reported must not spend one of its anchor's places on the way to being dropped. Sliced
        // first, an anchor whose top-ranked child was already reached — through a cycle, through
        // another anchor, or simply because two parents share a reply, which needs no cycle at all
        // — came away with nothing, and the reply it still had to give was never reported by
        // anybody. So the check runs first and the quota counts only what survives it.
        //
        // The node itself is reported once, under whichever anchor the ordering reaches first; the
        // others spend their places on replies of their own. The result is a flat union, and
        // filling it with duplicates would return fewer distinct records than the caller asked for.
        let mut kept: HashMap<String, usize> = HashMap::new();
        let mut next = Vec::new();
        for row in &rows {
            let Some(id) = row["source"].as_str() else {
                continue;
            };
            // A node reachable by two routes is one node. Walking it twice would duplicate its
            // subtree and, in a cycle, never finish.
            if seen.contains(id) {
                continue;
            }
            // A row with no anchor cannot be attributed to one, so it is kept rather than dropped:
            // losing rows silently is worse than a slice that is occasionally too generous. (Same
            // reading as `slice_per_anchor`, which the non-walking path still uses.)
            if let Some(anchor) = row[ANCHOR_VAR].as_str() {
                let used = kept.entry(anchor.to_string()).or_insert(0);
                if *used >= *limit {
                    continue;
                }
                *used += 1;
            }
            seen.insert(id.to_string());
            ordered.push(id.to_string());
            next.push(id.to_string());
        }
        log::debug!(
            "Walk depth {depth}: kept {} of {} rows across {} anchors at a breadth of {limit}",
            next.len(),
            rows.len(),
            kept.len()
        );
        frontier = next;
    }

    Ok(ordered)
}

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
/// * `viewer_did` — Visibility scope of the read. `None` is executor scope
///   (every link); `Some(did)` hides `Local` links authored by anyone else.
///   See [`link_visibility`](crate::perspectives::link_visibility). The scope
///   is carried through include recursion and projections, so a relation
///   cannot be used to walk into another user's private links.
pub async fn execute_model_query(
    store: &SparqlStore,
    shape: &ModelShape,
    query_input: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    viewer_did: Option<&str>,
) -> Result<ModelQueryResult, Error> {
    execute_model_query_inner(store, shape, query_input, resolver, 0, viewer_did).await
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
    viewer_did: Option<&str>,
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

    // The scope's documentation promises these combinations are mutually
    // exclusive; honouring only one of them silently would be a wrong answer
    // that looks like a right one, so they are refused up front — the same
    // treatment a transitive projection gives a reifier filter.
    if let Some(Scope::Traverse {
        transitive,
        limit_per_anchor,
        levels: Some(_),
        ..
    }) = &query_input.parent
    {
        if *transitive {
            return Err(Error::msg(
                "Traverse scope: `levels` and `transitive` are mutually exclusive — a bounded \
                 walk names its depths, a transitive path reaches everything. Use one.",
            ));
        }
        if limit_per_anchor.is_some() {
            return Err(Error::msg(
                "Traverse scope: `levels` and `limitPerAnchor` are mutually exclusive — the walk \
                 applies its own per-anchor limit at every depth, so a separate one has no place \
                 to act. Put the first level's breadth in `levels[0]`.",
            ));
        }
    }

    // A transitive traversal read by a viewer, walked over the links that
    // viewer may see. Every query below that is built from this scope needs it.
    let reach = visible_reach(store, query_input, viewer_did).await?;
    let reach = reach.as_deref();

    // Fast path: COUNT-only
    //
    // A walk cannot take it. The COUNT query is built from the scope as written, and the scope says
    // nothing about depth — `levels` is walked below, not in SPARQL — so this would answer for one
    // step from the anchors however many levels were asked for. That is the number the total in the
    // full pipeline was just taught not to report; leaving it here would mean the same walk answers
    // two different figures depending on whether the caller asked for rows alongside it, and
    // `count()` in TypeScript is exactly `limit: 0`. A walk has to be walked to be counted.
    let is_count_only = query_input.limit == Some(0);
    let count_only_can_skip_the_walk = !matches!(
        query_input.parent,
        Some(Scope::Traverse {
            levels: Some(_),
            ..
        })
    );
    if is_count_only
        && count_only_can_skip_the_walk
        && all_where_pushable(query_input, shape, Some(resolver))
    {
        if let Some(sparql) =
            build_count_sparql(shape, query_input, Some(resolver), viewer_did, reach)
        {
            let result_json = store.query(&sparql)?;
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

    // Full pipeline.
    //
    // A single order key is pushable to SPARQL when it is:
    //   - a reifier-timestamp synonym (`timestamp`/`createdAt`/`updatedAt`),
    //   - a scalar property on the entry shape,
    //   - a `$`-prefixed projection count key whose `from` relation exists, or
    //   - a dotted relation-property path (`relName.propName`) where the
    //     relation exists in the shape's include_relations and the named
    //     property exists as a scalar on the target shape.
    // Anything else (or more than one key) falls back to the post-hydration
    // Rust sort.
    let order_fully_pushable = {
        match &query_input.order {
            None => true,
            Some(order) => {
                order.len() == 1 && {
                    let name = &order[0].0;
                    if name == "timestamp" || name == "createdAt" || name == "updatedAt" {
                        true
                    } else if shape
                        .properties
                        .iter()
                        .any(|p| p.name == *name && !p.is_collection && !p.predicate.is_empty())
                    {
                        true
                    } else if name.starts_with('$') {
                        query_input
                            .projections
                            .as_ref()
                            .and_then(|projs| projs.get(name.as_str()))
                            .map(|proj| {
                                proj.count
                                    && shape
                                        .properties
                                        .iter()
                                        .any(|p| p.name == proj.from && !p.predicate.is_empty())
                            })
                            .unwrap_or(false)
                    } else if let Some(dot_pos) = name.find('.') {
                        let rel_name = &name[..dot_pos];
                        let prop_name = &name[dot_pos + 1..];
                        shape
                            .include_relations
                            .iter()
                            .find(|r| r.name == rel_name && !r.predicate.is_empty())
                            .and_then(|rel| resolver.get_shape(&rel.target_class_name).ok())
                            .map(|target_shape| {
                                target_shape.properties.iter().any(|p| {
                                    p.name == prop_name
                                        && !p.is_collection
                                        && !p.predicate.is_empty()
                                })
                            })
                            .unwrap_or(false)
                    } else {
                        false
                    }
                }
            }
        }
    };
    let can_push_pagination =
        all_where_pushable(query_input, shape, Some(resolver)) && order_fully_pushable;

    // A per-anchor limit forces the two-phase shape even with no global limit.
    // The whole value of slicing per anchor is that it happens on the id phase,
    // before hydration; the single-phase query hydrates as it matches, so
    // slicing its output would mean having already paid for everything thrown
    // away. No global LIMIT is emitted in that case — the ordering is what
    // matters, and the truncation is per anchor.
    let anchor_limit = per_anchor_limit(query_input);
    let walk = level_limits(query_input).cloned();

    // A per-anchor limit picks its N *in the store*, so the store has to be able to express the
    // order that decides which N. Only the first key is emitted, and a key the builder cannot
    // translate falls back to the reifier timestamp — so an order the store cannot express fully
    // means the slice kept rows the caller did not ask for, and no amount of re-sorting afterwards
    // puts them back: the rows it dropped were never hydrated.
    //
    // Sorting the survivors instead is the tempting repair and it is the wrong one, twice over. It
    // cannot recover a discarded row, and where the order *was* pushed faithfully it would undo a
    // correct SPARQL ordering — projection counts and relation properties are not resolved at that
    // point in the pipeline, so sorting on them compares nulls.
    //
    // Refused rather than approximated, for the same reason `levels` + `transitive` is: a result
    // that answers a question the caller did not ask, while looking exactly like one that did.
    // An absent `order` is not refused — asking for no order is a legitimate "any N".
    if (anchor_limit.is_some() || walk.is_some()) && !order_fully_pushable {
        return Err(Error::msg(
            "Traverse scope: a per-anchor limit selects its results in the store, so `order` must \
             be one key the store can sort by — a timestamp synonym, a scalar property, a \
             projection count, or a relation path. Several keys, or a key outside that set, would \
             slice on a different ordering than the one requested.",
        ));
    }

    // A walk and a per-anchor limit are properties of the SCOPE, not of paging, so they need the
    // two-phase shape whether or not the filter can be pushed down — the first phase is where the
    // ids to slice come from. Tying them to `can_push_pagination` meant that any filter evaluated
    // after hydration silently took the single-phase plan and with it the whole walk. `author` is
    // always such a filter, being link metadata rather than a property of the shape, so a thread
    // hiding muted authors — which is any thread with a mute list behind it — came back one level
    // deep.
    //
    // The global LIMIT/OFFSET is pushed into the store only when the scope does NOT slice or walk.
    // A pushed LIMIT truncates the id phase *before* the per-anchor slice, so one prolific anchor's
    // rows fill the window and starve the others — and in a walk the same window would apply at
    // every level, an OFFSET skipping rows at every depth. When the scope needs the phases for its
    // own sake, the window is instead applied executor-side at the end, after the slice (and after
    // any post-hydration filter). It also stays un-pushed when the filter itself cannot be:
    // truncating in the store before a post-hydration filter runs would discard rows that filter
    // would have kept.
    //
    // The per-level slice is applied before such a filter, so a row it later removes has still
    // taken one of its parent's places. That is the same trade `limit_per_anchor` has always made,
    // and the alternative — fetching a level whole to filter it — is what the limit exists to avoid.
    let scope_needs_phases = anchor_limit.is_some() || walk.is_some();
    let sparql_pagination = if scope_needs_phases
        || (can_push_pagination && (query_input.limit.is_some() || query_input.offset.is_some()))
    {
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
                } else if key.starts_with('$') {
                    // Projection count sort — find the from-relation's predicate.
                    query_input
                        .projections
                        .as_ref()
                        .and_then(|projs| projs.get(key.as_str()))
                        .and_then(|proj| {
                            shape
                                .properties
                                .iter()
                                .find(|p| p.name == proj.from && !p.predicate.is_empty())
                                .map(|p| p.predicate.clone())
                        })
                        .map(SortKey::Projection)
                        .unwrap_or(SortKey::Timestamp)
                } else if let Some(dot_pos) = key.find('.') {
                    // Dotted relation-property path: "relName.propName"
                    let rel_name = &key[..dot_pos];
                    let prop_name = &key[dot_pos + 1..];
                    shape
                        .include_relations
                        .iter()
                        .find(|r| r.name == rel_name && !r.predicate.is_empty())
                        .and_then(|rel| {
                            resolver
                                .get_shape(&rel.target_class_name)
                                .ok()
                                .and_then(|ts| {
                                    ts.properties
                                        .iter()
                                        .find(|p| {
                                            p.name == prop_name
                                                && !p.is_collection
                                                && !p.predicate.is_empty()
                                        })
                                        .map(|p| SortKey::RelationProperty {
                                            rel_pred: rel.predicate.clone(),
                                            prop_pred: p.predicate.clone(),
                                        })
                                })
                        })
                        .unwrap_or(SortKey::Timestamp)
                } else {
                    SortKey::Timestamp
                }
            }
        };
        let push_window = can_push_pagination && !scope_needs_phases;
        Some(SparqlPagination {
            sort_key,
            direction,
            offset: if push_window {
                query_input.offset
            } else {
                None
            },
            limit: if push_window { query_input.limit } else { None },
        })
    } else {
        None
    };
    // Whether the caller's window has already been applied by the store. When false but a
    // pagination subquery still ran (a slicing/walking scope, or an un-pushable filter), the
    // window is applied executor-side at the end of the pipeline instead.
    let window_pushed = sparql_pagination
        .as_ref()
        .map(|pg| pg.limit.is_some() || pg.offset.is_some())
        .unwrap_or(false);

    let query_plan = build_instance_sparql(
        shape,
        query_input,
        sparql_pagination.as_ref(),
        Some(resolver),
        viewer_did,
        reach,
    );

    // Captures the source IRI order returned by the phase-1 pagination subquery
    // so we can restore it after hydration (which uses BTreeMap, alphabetical order).
    let mut pagination_source_order: Option<Vec<String>> = None;

    let raw_results: Vec<Value> = match query_plan {
        InstanceQueryPlan::Single(sparql) => {
            let result_json = store.query_async(&sparql).await?;
            serde_json::from_str(&result_json)?
        }
        InstanceQueryPlan::TwoPhase {
            pagination_subquery,
            predicate_filter,
        } => {
            // Phase one either asks once, or walks a level at a time — the difference being where
            // the ids come from. Phase two is the same either way: one hydration for whatever it
            // settled on, which is what keeps a three-level walk to a single pass over records.
            let ordered_ids: Vec<String> = match (&walk, walk_roots(query_input)) {
                (Some(levels), Some((roots, predicate, direction))) => {
                    walk_levels(
                        store,
                        shape,
                        query_input,
                        resolver,
                        sparql_pagination
                            .as_ref()
                            .expect("a walk forces the two-phase plan, which needs pagination"),
                        roots,
                        &predicate,
                        direction,
                        levels,
                        viewer_did,
                    )
                    .await?
                }
                _ => {
                    let page_json = store.query_async(&pagination_subquery).await?;
                    let mut page_results: Vec<Value> = serde_json::from_str(&page_json)?;
                    if let Some(n) = anchor_limit {
                        slice_per_anchor(&mut page_results, n);
                    }
                    page_results
                        .iter()
                        .filter_map(|r| r["source"].as_str().map(|s| s.to_string()))
                        .collect()
                }
            };

            pagination_source_order = Some(ordered_ids.clone());

            if ordered_ids.is_empty() {
                vec![]
            } else {
                let source_ids: Vec<String> = ordered_ids
                    .iter()
                    .filter_map(|s| validate_iri(s).ok().map(|s| s.to_string()))
                    .collect();

                if source_ids.is_empty() {
                    vec![]
                } else {
                    let source_constraint = values_or_str_filter("source", &source_ids);
                    let local_status = local_status_filter(shape);
                    // Phase 1 pages over conformance triples, which carry no
                    // author, so it can hand us sources whose links the viewer
                    // may not see. Filtering here means those sources hydrate
                    // to nothing and drop out, rather than surfacing another
                    // user's private link rows.
                    let viewer = viewer_author_filter(viewer_did, "_reifier", "author");
                    let property_sparql = format!(
                        r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
    {source_constraint}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
{local_status}{viewer}}}"#
                    );
                    let result_json = store.query_async(&property_sparql).await?;
                    serde_json::from_str(&result_json)?
                }
            }
        }
    };

    let grouped = group_results_by_source(&raw_results, shape);
    let mut instances = hydrate_instances(shape, &grouped);

    // group_results_by_source uses BTreeMap (alphabetical by source IRI), so
    // after hydration the instances are in lexicographic IRI order, not the
    // sort order produced by the phase-1 pagination subquery.  Restore the
    // SPARQL-established order here so that subsequent steps (includes,
    // projections) see instances in the correct sequence.
    if let Some(ref source_order) = pagination_source_order {
        let pos: std::collections::HashMap<&str, usize> = source_order
            .iter()
            .enumerate()
            .map(|(i, id)| (id.as_str(), i))
            .collect();
        instances.sort_by_key(|inst| {
            inst["id"]
                .as_str()
                .and_then(|id| pos.get(id))
                .copied()
                .unwrap_or(usize::MAX)
        });
    }

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
        resolve_reverse_relations(store, &mut instances, &reverse_rels, viewer_did)?;
    }

    // Apply post-hydration where-clause filters
    if let Some(ref where_clause) = query_input.where_clause {
        if !all_where_pushable(query_input, shape, Some(resolver)) {
            instances.retain(|inst| matches_where(inst, where_clause, shape));
        }
    }

    // Calculate total count
    //
    // A walk counts itself. The COUNT query is built from the scope as written, and the scope says
    // nothing about depth — `levels` is walked here, not there — so it answers for one step from
    // the anchors however many levels were asked for. For `levels: [1, 1]` over a thread it reports
    // every direct reply to the anchor while the walk returns one of them and one below it: not
    // "how many exist" and not "how many you got", but a third number belonging to neither, which a
    // client paging on it reads as more rows to fetch.
    //
    // The walk is bounded by its own figures, so "how many exist below the anchor" is not a
    // question it asked. What it has is the size of the union it built — that is the total the
    // window below pages through, which is what a total is for. `instances` is that union here:
    // hydrated, filtered, and not yet cut to the caller's page.
    //
    // A per-anchor limit without a walk keeps the store's count. There the scope *is* the whole
    // question — "how many replies exist under these anchors" — and the limit says how many of them
    // to return, exactly the "showing 5 of 320" reading a total carries everywhere else.
    let total_count = if walk.is_some() {
        instances.len()
    } else if sparql_pagination.is_some() {
        if let Some(count_sparql) =
            build_count_sparql(shape, query_input, Some(resolver), viewer_did, reach)
        {
            let result_json = store.query(&count_sparql)?;
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
    let mut paginated: Vec<Value> = if window_pushed {
        // Ordering was pushed into the SPARQL pagination subquery and the
        // correct sequence has already been restored above — do not re-sort.
        // (Re-sorting here would also be a no-op-or-worse for Projection /
        // RelationProperty keys, since their source values — projection
        // counts, hydrated relations — aren't resolved yet at this point in
        // the pipeline.)
        instances
    } else if sparql_pagination.is_some() {
        // The pagination subquery ran — the order above is already right, so
        // no re-sort (see the previous arm) — but the caller's window was NOT
        // pushed into the store: either the scope slices or walks (a pushed
        // LIMIT would truncate the id phase before the slice) or the filter
        // runs after hydration (a pushed LIMIT would discard rows the filter
        // would have kept). Both have happened by this point, so the window
        // lands here.
        let offset = query_input.offset.unwrap_or(0);
        if let Some(limit) = query_input.limit {
            instances.into_iter().skip(offset).take(limit).collect()
        } else {
            instances.into_iter().skip(offset).collect()
        }
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
            resolve_includes_recursive(
                store,
                &mut paginated,
                include,
                shape,
                resolver,
                depth,
                viewer_did,
            )
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
            viewer_did,
        )
        .await?;
    }

    Ok(ModelQueryResult {
        instances: final_instances,
        total_count,
    })
}

/// Apply transform expressions to expression-resolved properties.
///
/// Properties whose values are stored as signed expression URIs (rather than
/// deterministic `literal:` IRIs) need their expression data fetched from the
/// language controller and the property's transform expression applied. Any
/// property with a non-`None` `resolve_language` falls in this bucket:
///   - `Some("literal")` → signed-envelope literal (per-value provenance).
///   - `Some(<addr>)`    → expression on that custom language.
/// Values stored as deterministic `literal:` IRIs (i.e. `resolve_language ==
/// None`) are left untouched by the per-value check below.
async fn resolve_language_transforms(
    shape: &ModelShape,
    instances: &mut [Value],
) -> Result<(), Error> {
    // Two kinds of properties need post-hydration work here:
    //   - expression-resolved properties (`resolve_language` set): fetch the
    //     expression data, then transform.
    //   - deterministic-literal properties that carry a transform: their value
    //     is already decoded by hydration, but the transform still has to be
    //     applied to it (e.g. concat a prefix onto the stored literal).
    let resolve_props: Vec<&super::types::ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| !p.is_deterministic_literal() || p.transform.is_some())
        .collect();

    if resolve_props.is_empty() {
        return Ok(());
    }

    let controller = crate::languages::LanguageController::global_instance();

    for instance in instances.iter_mut() {
        for prop in &resolve_props {
            // Compute the "resolved" focus value for the transform:
            //   - String that parses as a language expression URL → fetch via controller
            //   - Anything else (already-decoded literal string, object, etc.) → use as-is
            let current = instance[&prop.name].clone();
            // Only expression-resolved properties fetch their data from the
            // language controller. Deterministic-literal properties (with a
            // transform) use their already-decoded value as the transform focus
            // directly — never re-interpreted as an expression URL.
            let is_expr = !prop.is_deterministic_literal();
            let resolved: Option<Value> = match &current {
                Value::String(uri) if is_expr && !uri.starts_with("literal:") => {
                    match crate::languages::LanguageController::parse_expr_url(uri) {
                        Ok((lang, expr_addr)) => {
                            // Ensure the language is loaded before attempting to fetch the
                            // expression. The runtimes map only contains languages that have
                            // been explicitly installed/loaded; languages referenced via
                            // resolveLanguage (e.g. FILE_STORAGE_LANGUAGE) may not be
                            // loaded yet at query time.
                            if !controller.is_language_loaded(&lang).await {
                                if let Err(e) = controller.language_by_ref(&lang).await {
                                    log::warn!(
                                        "resolve_language_transforms: failed to load language {} \
                                         for property '{}': {}",
                                        lang,
                                        prop.name,
                                        e
                                    );
                                    instance[&prop.name] = current;
                                    continue;
                                }
                            }
                            match controller.get_expression(&lang, &expr_addr).await {
                                Ok(Some(expr_json)) => {
                                    let data =
                                        expr_json.get("data").cloned().unwrap_or(Value::Null);
                                    Some(match &data {
                                        Value::String(s) => serde_json::from_str(s).unwrap_or(data),
                                        _ => data,
                                    })
                                }
                                // Not a fetchable expression — fall back to the raw value
                                _ => Some(current.clone()),
                            }
                        }
                        Err(_) => Some(current.clone()),
                    }
                }
                Value::Object(_) => Some(current.clone()),
                Value::String(_) => Some(current.clone()),
                Value::Null => None,
                _ => Some(current.clone()),
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
