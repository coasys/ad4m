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
//!    An include's `limit` / `offset` page each parent's rows, not the whole
//!    page's ([`PerParentPage`]): the sub-query is then scoped as a
//!    `Traverse` from the parents, cut per anchor in the store where the store
//!    can cut it exactly ([`per_parent_query`]).

use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

use super::query::{execute_model_query_inner, order_fully_pushable};
use super::sparql_builder::{all_where_pushable, verified_link_exists};
use super::types::{
    IncludeValue, ModelQueryInput, ModelShape, OrderDirection, Scope, ScopeDirection,
    ShapeRelation, ShapeResolver, WhereCondition,
};
use super::utils::{validate_iri, values_or_str_filter};
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkStatus;

/// Resolve reverse relations (`@BelongsTo`) for all instances in a batch.
///
/// For each `(name, predicate, is_single)` relation, executes a single
/// batched SPARQL query: `?source <pred> ?target` with `VALUES ?target { ... }`
/// containing all instance IDs.  The results are attached to each instance
/// as either a scalar (for `belongsToOne`) or an array (for `belongsToMany`).
///
/// With a `link_status` (#1116), only links of that status are read, and a
/// link whose signature did not verify is not read unless the query opted in
/// with `include_unverified` (#1113). Both are checked on one reifier
/// ([`verified_link_exists`]).
pub fn resolve_reverse_relations(
    store: &SparqlStore,
    instances: &mut [Value],
    relations: &[(String, String, bool)], // (name, predicate, is_single)
    link_status: Option<&LinkStatus>,
    include_unverified: Option<bool>,
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

    let target_constraint = values_or_str_filter("target", &instance_iris);

    for (rel_name, predicate, is_single) in relations {
        let safe_pred = match validate_iri(predicate) {
            Ok(p) => p,
            Err(_) => continue,
        };

        let filter = verified_link_exists(
            "?source",
            safe_pred,
            "?target",
            link_status,
            include_unverified,
        );
        let sparql = format!(
            "SELECT ?source ?target WHERE {{ {} ?source <{safe_pred}> ?target .{filter} }}",
            target_constraint
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
///
/// `link_status` and `include_unverified` are the parent query's. A sub-query
/// that does not set its own inherits each of them, so a Shared-only read stays
/// Shared-only on the included instances, and a caller that asked to see
/// unverified rows sees them there too. A sub-query's own setting wins,
/// including an explicit `includeUnverified: false`.
pub(super) async fn resolve_includes_recursive(
    store: &SparqlStore,
    instances: &mut [Value],
    include: &HashMap<String, IncludeValue>,
    shape: &ModelShape,
    resolver: &dyn ShapeResolver,
    depth: u8,
    link_status: Option<&LinkStatus>,
    include_unverified: Option<bool>,
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

        let mut sub_query = match include_val {
            IncludeValue::Bool(true) => ModelQueryInput::default(),
            IncludeValue::SubQuery(sq) => *sq.clone(),
            _ => continue,
        };
        if sub_query.link_status.is_none() {
            sub_query.link_status = link_status.cloned();
        }
        if sub_query.include_unverified.is_none() {
            sub_query.include_unverified = include_unverified;
        }

        // Checked here, where the include is recognised, rather than down in
        // hydration: both resolvers return early when the relation holds no
        // targets, so validating later would accept the query on an empty
        // collection and reject the same query on a full one. Whether a request
        // is answerable must not depend on the data it is asking about.
        reject_pagination_on_polymorphic(rel_name, &sub_query)?;

        if rel.direction == "reverse" {
            resolve_reverse_include(store, instances, rel, &sub_query, resolver, depth).await?;
        } else {
            let links = links_are_the_relation(shape, rel);
            resolve_forward_include(store, instances, rel, links, &sub_query, resolver, depth)
                .await?;
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
/// Membership is structural and therefore not exclusive: a class is a set of
/// required triples, so a base expression carrying two classes' requirements
/// belongs to both. Nothing about that is malformed — two independently authored
/// classes needing an overlapping set of predicates is ordinary, and it is why
/// `subject_classes_of` answers with a set rather than a name. Such a target is
/// still hydrated **once**: links decide the relation's cardinality and classes
/// decide only how a target is read, so one link is one member. Returning a
/// member per class would make a three-link collection arrive with five entries,
/// and every caller would have to know to collapse them.
///
/// Which reading wins is therefore a policy, and this is where it is applied.
/// `subject_classes_of` ranks the set by specificity and declines to choose;
/// hydration needs exactly one shape, so it chooses.
///
/// The caller decides where it can. `prefer_classes` names the classes the read
/// is for, and the first of them the target conforms to is the one it is
/// hydrated as — see [`choose`]. That is the only ordering with a claim on being
/// right, because specificity answers "which class demanded most of this node",
/// which is a question nobody asked.
///
/// Where the caller names nothing, or names nothing this target is, specificity
/// decides as before. Within an inheritance chain that is genuinely the most
/// derived class — a subclass requires everything its parent does and more.
/// Between two unrelated classes the ranking ties and the alphabetical tie-break
/// decides, which is arbitrary but identical on every peer reading the same
/// data. With no `rdf:type` triple to appeal to, "which class is this really"
/// has no fact underneath it, so agreement between peers is the best a default
/// can do — and naming classes is how a caller replaces the default with an
/// answer.
///
/// So that the discarded readings do not vanish without trace, every instance
/// also carries the full set under [`SUBJECT_CLASSES_KEY`]. A caller that wants
/// one of the others has its id and can query that class for it directly.
/// Pick the class to hydrate a target against, from the set it conforms to.
///
/// The caller's `preferClasses` decides, in the order they wrote it: the first
/// one the target actually conforms to wins. That is the only ordering with a
/// claim on being right, because it is the only one that knows what the read is
/// for. `matched` is already ranked by specificity, so a caller who names
/// nothing — or names nothing this target is — gets exactly what it got before.
///
/// Preference is consulted in the caller's order rather than in `matched`'s, so
/// naming `[Post, ImagePost]` reads an `ImagePost` as a `Post`. That is the
/// point: a relation declared to hold `Post`s wants posts, not whatever more
/// specific thing each one happens to be.
pub(super) fn choose<'a>(matched: &'a [String], preferred: &[String]) -> Option<&'a String> {
    preferred
        .iter()
        .find_map(|want| matched.iter().find(|have| *have == want))
        .or_else(|| matched.first())
}

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
    // first. Hydration needs exactly one shape per target, so one is chosen —
    // whichever the caller asked for, falling back to the most specific.
    // The rest are not lost: they ride back on the instance below.
    let preferred = sub_query.prefer_classes.as_deref().unwrap_or(&[]);
    let mut by_class: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for id in target_ids {
        if let Some(class_name) = classes.get(id).and_then(|names| choose(names, preferred)) {
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

/// Whether a forward relation's values are exactly its links over its
/// predicate, so that a [`Scope::Traverse`] over that predicate reaches the same
/// rows the relation lists.
///
/// True for a plain link relation and for one filled by the conformance getter
/// the SDK generates for a typed relation (the target class's conformance is
/// also applied by the include's own query). False for a getter of the model
/// author's own, a relation `where`, and a `local` relation (read from Local
/// links only, which the scope's link guard does not know of): each of those
/// lists something other than the links.
fn links_are_the_relation(shape: &ModelShape, rel: &ShapeRelation) -> bool {
    let Some(prop) = shape.properties.iter().find(|p| p.name == rel.name) else {
        return false;
    };
    let getter_lists_links = match &prop.getter {
        None => true,
        Some(getter) => super::getters::is_generated_relation_getter(getter, &rel.predicate),
    };
    getter_lists_links && prop.where_filter.is_none() && !prop.local
}

/// An include's `limit` / `offset`, read per parent.
///
/// `include: { comments: { limit: 5 } }` on a page of posts is each post with
/// its own five, so the window cannot go to the include's sub-query as written:
/// one query for the whole page, cut once, hands the five to whichever posts
/// own them and `[]` to the rest. The sub-query runs without a window and each
/// parent's rows are cut here; [`per_parent_query`] lets the store do the
/// cutting where it can do it exactly.
///
/// `offset` is per parent too, for the same reason: skipping the first N rows
/// of the whole page would skip one post's comments and none of another's.
struct PerParentPage {
    offset: usize,
    limit: Option<usize>,
}

impl PerParentPage {
    /// `None` when the include asks for no window: every parent keeps every
    /// related row, read exactly as before.
    fn of(sub_query: &ModelQueryInput) -> Option<Self> {
        if sub_query.limit.is_none() && sub_query.offset.is_none() {
            return None;
        }
        Some(Self {
            offset: sub_query.offset.unwrap_or(0),
            limit: sub_query.limit,
        })
    }

    /// How many rows of each parent the store must keep for this page:
    /// everything the offset skips, and the page after it.
    fn per_anchor(&self) -> Option<usize> {
        self.limit.map(|limit| self.offset.saturating_add(limit))
    }

    /// One parent's page of `ordered`, the sub-query's rows in the order that
    /// chooses them. `members` are the parent's own related ids; with
    /// `keep_member_order` the page is shown in their order (the relation's
    /// link order, as an include without `order` always has been).
    fn select<'a>(
        &self,
        ordered: &'a [String],
        members: &'a [String],
        keep_member_order: bool,
    ) -> Vec<&'a str> {
        let page: Vec<&str> = ordered
            .iter()
            .filter(|id| members.contains(id))
            .skip(self.offset)
            .take(self.limit.unwrap_or(usize::MAX))
            .map(String::as_str)
            .collect();
        if !keep_member_order {
            return page;
        }
        let mut seen = std::collections::HashSet::new();
        members
            .iter()
            .map(String::as_str)
            .filter(|id| page.contains(id) && seen.insert(*id))
            .collect()
    }
}

/// The sub-query for a windowed include: the related rows of every parent on
/// the page, scoped as a [`Scope::Traverse`] from the parents over the
/// relation's predicate, without the caller's window (each parent's is applied
/// by [`PerParentPage::select`]).
///
/// The rows are chosen in the include's `order`, or oldest first without one —
/// what a limited read with no order has always meant. When the store can sort
/// by that order and evaluate the whole `where`, the scope carries
/// `limitPerAnchor`, so the store keeps only each parent's first
/// `offset + limit` ids and hydration sees only those. Otherwise a cut in the
/// store would keep rows the caller did not ask for — the order it slices by
/// is not the requested one, or a filter run after hydration removes rows that
/// had already taken a parent's places — so every related row is read, and
/// filtered and sorted in the executor, before each parent is cut. That is the
/// cost an include with such an order or filter has always paid.
///
/// The scope stands for the relation only when `links_are_the_relation`: the
/// relation lists exactly its links (see [`links_are_the_relation`]). Where it
/// does not, where a parent id is not an IRI, or where the sub-query names a
/// `parent` scope of its own (which it keeps), the relation is applied as the
/// `where.id` list of `members` instead, as an include without a window is, and
/// nothing is cut in the store.
#[allow(clippy::too_many_arguments)]
fn per_parent_query(
    sub_query: &ModelQueryInput,
    rel: &ShapeRelation,
    links_are_the_relation: bool,
    parent_ids: &[String],
    direction: ScopeDirection,
    members: &[String],
    page: &PerParentPage,
    target_shape: &ModelShape,
    resolver: &dyn ShapeResolver,
) -> ModelQueryInput {
    let mut query = sub_query.clone();
    query.limit = None;
    query.offset = None;
    if query.order.is_none() {
        query.order = Some(vec![("timestamp".to_string(), OrderDirection::ASC)]);
    }
    // A Traverse drops an anchor that is not an IRI, which would leave that
    // parent with nothing where the id list still reaches its rows.
    if !links_are_the_relation
        || query.parent.is_some()
        || validate_iri(&rel.predicate).is_err()
        || parent_ids.iter().any(|id| validate_iri(id).is_err())
    {
        let mut wc = query.where_clause.take().unwrap_or_default();
        let ids = match wc.get("id") {
            Some(WhereCondition::String(s)) => {
                members.iter().filter(|id| *id == s).cloned().collect()
            }
            Some(WhereCondition::StringArray(arr)) => members
                .iter()
                .filter(|id| arr.contains(id))
                .cloned()
                .collect(),
            Some(_) => vec![],
            None => members.to_vec(),
        };
        wc.insert("id".to_string(), WhereCondition::StringArray(ids));
        query.where_clause = Some(wc);
        return query;
    }
    let store_can_cut = order_fully_pushable(&query, target_shape, resolver)
        && all_where_pushable(&query, target_shape, Some(resolver));
    query.parent = Some(Scope::Traverse {
        ids: parent_ids.to_vec(),
        predicate: Some(rel.predicate.clone()),
        transitive: false,
        direction,
        limit_per_anchor: if store_can_cut {
            page.per_anchor()
        } else {
            None
        },
        levels: None,
        max_depth: None,
    });
    query
}

/// Resolve a forward relation (`@HasMany` / `@HasOne`) for all instances.
///
/// Collects all unique target IDs from the relation arrays, runs a sub-query
/// to hydrate them, and replaces the raw ID arrays with hydrated JSON objects.
/// If the sub-query specifies an `order`, the result order is preserved.
/// A `limit` / `offset` pages each instance's targets ([`PerParentPage`]).
async fn resolve_forward_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    links_are_the_relation: bool,
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
    // Never set on a polymorphic include, which refuses `limit`/`offset`.
    let page = PerParentPage::of(sub_query);
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
        if let Some(page) = &page {
            let parent_ids: Vec<String> = instances
                .iter()
                .filter_map(|inst| inst["id"].as_str().map(str::to_string))
                .collect();
            query = per_parent_query(
                sub_query,
                rel,
                links_are_the_relation,
                &parent_ids,
                ScopeDirection::Out,
                &target_ids,
                page,
                target_shape.as_ref(),
                resolver,
            );
        }
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

        let iter_ids: Vec<&str> = if let Some(page) = &page {
            page.select(&ordered_ids, &parent_ids, !has_sub_order)
        } else if has_sub_order {
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

    let safe_ids: Vec<String> = all_ids
        .iter()
        .filter(|id| validate_iri(id).is_ok())
        .map(|id| id.to_string())
        .collect();
    if safe_ids.is_empty() {
        return Ok(());
    }
    let safe_pred = match validate_iri(&rel.predicate) {
        Ok(p) => p,
        Err(_) => return Ok(()),
    };
    let target_constraint = values_or_str_filter("target", &safe_ids);
    let filter = verified_link_exists(
        "?source",
        safe_pred,
        "?target",
        sub_query.link_status.as_ref(),
        sub_query.include_unverified,
    );
    let sparql = format!(
        "SELECT ?source ?target WHERE {{ ?source <{safe_pred}> ?target . {target_constraint}{filter} }}"
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
    // Never set on a polymorphic include, which refuses `limit`/`offset`.
    let page = PerParentPage::of(sub_query);
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
            if let Some(page) = &page {
                // The sources were read above over the same predicate and
                // under the same link guard as the scope's step, so the scope
                // is the relation.
                query = per_parent_query(
                    sub_query,
                    rel,
                    true,
                    &safe_ids,
                    ScopeDirection::In,
                    &target_ids,
                    page,
                    target_shape.as_ref(),
                    resolver,
                );
            }
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
            let first = match &page {
                Some(page) => page
                    .select(&ordered_result_ids, &source_ids, !has_sub_order)
                    .first()
                    .map(|id| id.to_string()),
                None => source_ids.first().cloned(),
            };
            first
                .and_then(|id| hydrated.get(&id).cloned())
                .unwrap_or(Value::Null)
        } else {
            let iter_ids: Vec<&str> = if let Some(page) = &page {
                page.select(&ordered_result_ids, &source_ids, !has_sub_order)
            } else if has_sub_order {
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

#[cfg(test)]
mod per_parent_tests {
    use super::*;
    use crate::perspectives::model_query::shape::parse_shape_from_json;
    use crate::perspectives::model_query::test_helpers::StaticShapeResolver;

    const COMMENT: &str = r#"{
        "className": "Comment",
        "properties": {
            "cflag": { "predicate": "we://cflag", "required": true, "flag": true, "initial": "we://comment" },
            "body": { "predicate": "we://body", "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    fn post(relation: &str) -> ModelShape {
        parse_shape_from_json(
            &format!(
                r#"{{
                "className": "Post",
                "properties": {{}},
                "relations": {{ "comments": {relation} }}
            }}"#
            ),
            "Post",
        )
        .unwrap()
    }

    fn comments_relation(shape: &ModelShape) -> &ShapeRelation {
        shape
            .include_relations
            .iter()
            .find(|r| r.name == "comments")
            .unwrap()
    }

    #[test]
    fn a_plain_or_generated_relation_is_its_links_and_others_are_not() {
        let plain = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment" }"#,
        );
        assert!(links_are_the_relation(&plain, comments_relation(&plain)));

        let generated = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment",
                 "getter": "SELECT ?target WHERE { <Base> <we://comment> ?target . ?target <we://cflag> <we://comment> . }" }"#,
        );
        assert!(links_are_the_relation(
            &generated,
            comments_relation(&generated)
        ));

        let own = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment",
                 "getter": "SELECT ?target WHERE { ?target <we://reply_to> <Base> . }" }"#,
        );
        assert!(!links_are_the_relation(&own, comments_relation(&own)));

        let local = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment", "local": true }"#,
        );
        assert!(!links_are_the_relation(&local, comments_relation(&local)));
    }

    fn plan(sub_query: ModelQueryInput, links: bool) -> ModelQueryInput {
        let shape = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment" }"#,
        );
        let (resolver, comment) = StaticShapeResolver::from_json("Comment", COMMENT).unwrap();
        let page = PerParentPage::of(&sub_query).expect("a window");
        per_parent_query(
            &sub_query,
            comments_relation(&shape),
            links,
            &["we://post/1".to_string(), "we://post/2".to_string()],
            ScopeDirection::Out,
            &["we://c/1".to_string()],
            &page,
            comment.as_ref(),
            &resolver,
        )
    }

    /// An order and filter the store can evaluate: the store keeps each
    /// parent's `offset + limit`, and nothing else bounds the read.
    #[test]
    fn a_store_expressible_include_is_cut_per_anchor_in_the_store() {
        let q = plan(
            ModelQueryInput {
                limit: Some(2),
                offset: Some(1),
                order: Some(vec![("timestamp".to_string(), OrderDirection::DESC)]),
                ..Default::default()
            },
            true,
        );
        assert_eq!((q.limit, q.offset), (None, None));
        assert!(q
            .where_clause
            .as_ref()
            .is_none_or(|w| !w.contains_key("id")));
        match q.parent {
            Some(Scope::Traverse {
                ids,
                predicate,
                direction,
                limit_per_anchor,
                transitive: false,
                ..
            }) => {
                assert_eq!(ids, vec!["we://post/1", "we://post/2"]);
                assert_eq!(predicate.as_deref(), Some("we://comment"));
                assert_eq!(direction, ScopeDirection::Out);
                assert_eq!(limit_per_anchor, Some(3));
            }
            other => panic!("expected a Traverse scope, got {other:?}"),
        }
    }

    /// Two order keys: the store cannot sort by them, so it must not cut.
    #[test]
    fn an_order_the_store_cannot_express_is_not_cut_in_the_store() {
        let q = plan(
            ModelQueryInput {
                limit: Some(2),
                order: Some(vec![
                    ("body".to_string(), OrderDirection::DESC),
                    ("timestamp".to_string(), OrderDirection::ASC),
                ]),
                ..Default::default()
            },
            true,
        );
        assert!(matches!(
            q.parent,
            Some(Scope::Traverse {
                limit_per_anchor: None,
                ..
            })
        ));
    }

    /// A relation that is not its links is scoped by its listed ids instead.
    #[test]
    fn a_relation_that_is_not_its_links_is_scoped_by_its_ids() {
        let q = plan(
            ModelQueryInput {
                limit: Some(2),
                ..Default::default()
            },
            false,
        );
        assert!(q.parent.is_none());
        assert!(matches!(
            q.where_clause.as_ref().and_then(|w| w.get("id")),
            Some(WhereCondition::StringArray(ids)) if ids == &vec!["we://c/1".to_string()]
        ));
        assert_eq!(
            q.order,
            Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            "a limited include with no order chooses oldest first"
        );
    }

    /// A Traverse would drop an anchor that is not an IRI, leaving that parent
    /// with nothing; such a page is scoped by the listed ids instead.
    #[test]
    fn a_parent_id_that_is_not_an_iri_is_scoped_by_its_ids() {
        let shape = post(
            r#"{ "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment" }"#,
        );
        let (resolver, comment) = StaticShapeResolver::from_json("Comment", COMMENT).unwrap();
        let sub_query = ModelQueryInput {
            limit: Some(2),
            ..Default::default()
        };
        let page = PerParentPage::of(&sub_query).expect("a window");
        let q = per_parent_query(
            &sub_query,
            comments_relation(&shape),
            true,
            &["we://post/1".to_string(), "we://post 2".to_string()],
            ScopeDirection::Out,
            &["we://c/1".to_string()],
            &page,
            comment.as_ref(),
            &resolver,
        );
        assert!(q.parent.is_none());
        assert!(matches!(
            q.where_clause.as_ref().and_then(|w| w.get("id")),
            Some(WhereCondition::StringArray(ids)) if ids == &vec!["we://c/1".to_string()]
        ));
    }
}
