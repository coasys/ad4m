use super::*;
use crate::agent::AgentContext;
use crate::perspectives::interpretation::dedup::Resolution;
use crate::perspectives::interpretation::types::{
    ExistingInstances, ExistingLinks, InterpretationOp, ProposedInstance,
};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// Replace `(base, predicate)` with a single link to `target`: remove every
/// existing link under that predicate, then add the new one. A generic
/// single-valued link upsert — used by the provenance overlay to keep
/// `inferred/<p>` (and `run`) single-valued and current across passes.
///
/// `batch_id` groups the underlying `remove_links` + `add_link` into a caller-
/// owned batch so a mid-write failure rolls back atomically. Pass `None` for
/// unbatched writes (e.g. test seeding).
pub(crate) async fn replace_link(
    perspective: &mut PerspectiveInstance,
    base: &str,
    predicate: &str,
    target: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let existing = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await?;
    if !existing.is_empty() {
        let exprs: Vec<LinkExpression> = existing.into_iter().map(Into::into).collect();
        perspective.remove_links(exprs, batch_id.clone()).await?;
    }
    perspective
        .add_link(
            Link {
                source: base.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            LinkStatus::Shared,
            batch_id,
            context,
        )
        .await?;
    Ok(())
}

/// The scalar (non-relation) field values a proposed instance wants written —
/// the payload handed to `create_subject` / `update_subject` as `initial_values`.
/// Relation fields are stripped (their values are instance *refs*, resolved by
/// [`resolve_relation_links`] instead), and `null`s are dropped so a missing
/// optional field never becomes a literal-encoded `null`.
///
/// Unknown/extra keys survive: the class's SDNA is the authority on what gets
/// written, and `create_subject` only acts on properties that declare an
/// `ad4m://setter`, so a hallucinated field can never become a link.
pub(crate) fn scalar_values(
    shape: &ModelShape,
    inst: &ProposedInstance,
) -> serde_json::Map<String, serde_json::Value> {
    let rel_names = relation_names(shape);
    inst.props
        .iter()
        .filter(|(k, v)| !rel_names.contains(k.as_str()) && !v.is_null())
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

/// Turn proposed instances into create/update ops with no relation context.
/// Thin wrapper over [`plan_interpretation_ops_with_context`] with an empty
/// existing-id set. Any proposed `id` is therefore treated as hallucinated and
/// routed to `Create` — callers that need to exercise the id-becomes-`Update`
/// path (or resolve relations against the graph's existing instances) must
/// call the `_with_context` form directly with a real
/// [`ExistingInstances`] map.
pub fn plan_interpretation_ops(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<InterpretationOp> {
    plan_interpretation_ops_with_context(shapes, proposed, base_prefix, &ExistingInstances::new())
}

/// Turn proposed instances into create/update/add-links ops (Phase 2:
/// relation-aware). A proposal with an `id` present in `known_existing_ids`
/// becomes an `Update` on that existing base (scalar fields); a proposal with
/// no `id` — OR with an `id` the graph doesn't recognise (the LLM
/// hallucinated it) — becomes a `Create` under `base_prefix` with a fresh
/// base. Hallucinated-id Updates against non-existent bases produce a silent
/// no-op write, so routing them to Create yields a visible instance instead of
/// data lost between the model and the graph. On top of that, forward relation
/// fields are resolved into real `Link`s and emitted as an additive `AddLinks`
/// op on the proposal's base. Unknown-class proposals are dropped.
///
/// Two passes, because a relation ref can point *forward* to a sibling minted
/// later in the same response:
///   1. Place every proposal — mint or reuse its base — and index bases per
///      class **in the LLM's output order**, so `new:<Class>:<n>` ordinals
///      resolve deterministically. `known_existing_ids` seeds the set of valid
///      existing-id relation targets (what the model was shown in `existing`).
///   2. Resolve each proposal's relation refs against the full index and emit
///      links. Unresolvable refs (typo'd id, out-of-range ordinal) are dropped
///      with a `log::warn!` — the node still lands, just without that edge.
///
/// `proposed` **must be in the LLM's emission order** for the ordinals to line
/// up; `run_interpretation` guarantees this by dedup-filtering in place rather
/// than re-partitioning.
///
/// Treats every proposal as [`Resolution::Keep`] — i.e. no dedup context. The
/// production path calls [`plan_interpretation_ops_resolved`] with real
/// [`Resolution`] tags so `new:<Class>:<n>` ordinals survive dedup (James #883).
pub fn plan_interpretation_ops_with_context(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
    existing: &ExistingInstances,
) -> Vec<InterpretationOp> {
    let resolved: Vec<(ProposedInstance, Resolution)> = proposed
        .iter()
        .map(|p| (p.clone(), Resolution::Keep))
        .collect();
    // No existing-link context here: this convenience wrapper is used where the
    // caller has no graph to read edges from (unit tests, first-pass planning),
    // so every resolved relation is emitted. The idempotency guard is exercised
    // through [`plan_interpretation_ops_resolved`] with a populated set.
    plan_interpretation_ops_resolved(
        shapes,
        &resolved,
        base_prefix,
        existing,
        &ExistingLinks::new(),
    )
}

/// Ordinal-preserving planner: like [`plan_interpretation_ops_with_context`] but
/// each proposal carries the [`Resolution`] identity-dedup assigned it. Every
/// item — kept OR duplicate — takes a slot in the per-class ordinal index so a
/// relation `new:<Class>:<n>` resolves against the model's *full* emission
/// order (James #883: dedup that dropped items before planning shifted later
/// ordinals). Ops are emitted only for kept items:
///   - [`Resolution::Keep`]: as before — id naming a same-class existing
///     instance → `Update`, else a fresh `Create` under `base_prefix`.
///   - [`Resolution::DupOfExisting`]: indexed at the real existing base (so refs
///     to it link the real node) but written nothing.
///   - [`Resolution::DupOfEarlier`]: indexed at the earlier slot's base, written
///     nothing.
///
/// `existing_links` carries the `(source, predicate, target)` relation edges the
/// graph already holds (see [`ExistingLinks`]); a resolved relation link whose
/// triple is already present is NOT re-emitted, so repeated continuous passes
/// stay idempotent (James #883 #4). Pass an empty set for a first pass / when no
/// graph state is available.
pub fn plan_interpretation_ops_resolved(
    shapes: &[ModelShape],
    resolved: &[(ProposedInstance, Resolution)],
    base_prefix: &str,
    existing: &ExistingInstances,
    existing_links: &ExistingLinks,
) -> Vec<InterpretationOp> {
    struct Placed<'a> {
        shape: &'a ModelShape,
        inst: &'a ProposedInstance,
        base: String,
        is_update: bool,
        /// Whether Pass 2 emits an op for this slot. Duplicates are indexed for
        /// ordinal resolution but never written.
        emit: bool,
    }

    // Pass 1: place proposals + build the per-class ordinal index.
    let mut per_class: HashMap<String, Vec<String>> = HashMap::new();
    // Ids the graph holds, extended with proposals we route to Update, so
    // relation refs can resolve against real *and* just-planned bases.
    let mut existing_ids: HashSet<String> = existing.keys().cloned().collect();
    let mut placed: Vec<Placed> = Vec::with_capacity(resolved.len());
    // Base URI resolved for each `resolved` slot (None = skipped: unknown class
    // or an unresolvable earlier ref). Keeps `DupOfEarlier(idx)` aligned to the
    // emission order even though `placed` is compacted.
    let mut slot_base: Vec<Option<String>> = Vec::with_capacity(resolved.len());
    for (inst, resolution) in resolved {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_label(&s.target_class, shapes) == inst.class)
        else {
            log::debug!(
                "interpretation: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            slot_base.push(None);
            continue;
        };
        let placed_slot: Option<(String, bool, bool)> = match resolution {
            Resolution::DupOfExisting(existing_base) => Some((existing_base.clone(), false, false)),
            Resolution::DupOfEarlier(idx) => match slot_base.get(*idx).cloned().flatten() {
                Some(base) => Some((base, false, false)),
                None => {
                    log::debug!(
                        "interpretation: DupOfEarlier({idx}) for '{}' had no resolved base; dropping",
                        inst.class
                    );
                    None
                }
            },
            Resolution::Keep => {
                let (base, is_update) = match &inst.id {
                    // Only route to Update when the proposed id names an existing
                    // instance OF THE SAME CLASS. An id that belongs to a
                    // different class must Create, never Update — running this
                    // class's setters against a foreign-class node clobbers links
                    // on shared predicates.
                    Some(id)
                        if existing.get(id).is_some_and(|entries| {
                            entries.iter().any(|ctx| ctx.class == inst.class)
                        }) =>
                    {
                        (id.clone(), true)
                    }
                    _ => {
                        if let Some(hallucinated) = &inst.id {
                            match existing.get(hallucinated) {
                                Some(entries) => log::debug!(
                                    "interpretation: proposed id {hallucinated:?} has classes [{}], not '{}'; routing to Create",
                                    entries.iter().map(|e| e.class.as_str()).collect::<Vec<_>>().join(", "),
                                    inst.class
                                ),
                                None => log::debug!(
                                    "interpretation: proposed id {hallucinated:?} not among existing instances for class {}; routing to Create",
                                    inst.class
                                ),
                            }
                        }
                        (
                            format!(
                                "{base_prefix}{}/{}",
                                // Use the shape's bare local name for the minted base URI
                                // even when `inst.class` is a full URI (collision case), so
                                // the id stays clean (`…/task/<uuid>`, not `…/flux://task/…`).
                                class_local_name(&shape.target_class).to_lowercase(),
                                Uuid::new_v4()
                            ),
                            false,
                        )
                    }
                };
                Some((base, is_update, true))
            }
        };
        match placed_slot {
            Some((base, is_update, emit)) => {
                // Index under the class name the LLM uses (matches the
                // relation's `targetClass`, i.e. the bare local name), in
                // output order.
                per_class
                    .entry(inst.class.clone())
                    .or_default()
                    .push(base.clone());
                if is_update {
                    existing_ids.insert(base.clone());
                }
                slot_base.push(Some(base.clone()));
                placed.push(Placed {
                    shape,
                    inst,
                    base,
                    is_update,
                    emit,
                });
            }
            None => slot_base.push(None),
        }
    }

    // Pass 2: build ops for the written slots, resolving relations against the
    // full index.
    //
    // Relation constraints (`sh:minCount`, `sh:disjoint`) are not checked here:
    // they are a property of the whole batch — refusing one record can empty a
    // sibling's required relation — and the harness path builds its ops without
    // this planner. `enforce_relation_constraints` checks both, in the shared
    // tail.
    let mut out = Vec::with_capacity(placed.len());
    for p in &placed {
        // Resolve relations for ALL slots (including deduplicated ones) —
        // AddLinks is additive and the existing-link guard suppresses repeats.
        let rel_links = resolve_relation_links(
            p.shape,
            p.inst,
            &p.base,
            &per_class,
            &existing_ids,
            existing_links,
        );
        if p.emit {
            let values = scalar_values(p.shape, p.inst);
            if p.is_update {
                if !values.is_empty() {
                    out.push(InterpretationOp::Update {
                        base: p.base.clone(),
                        class: p.inst.class.clone(),
                        values,
                    });
                }
            } else {
                out.push(InterpretationOp::Create {
                    base: p.base.clone(),
                    class: p.inst.class.clone(),
                    values,
                });
            }
        }
        if !rel_links.is_empty() {
            out.push(InterpretationOp::AddLinks {
                source: p.base.clone(),
                links: rel_links,
            });
        }
    }
    out
}

/// Resolve a proposed instance's forward relation fields into perspective
/// links. Each relation field's value is a ref (or array of refs), each of the
/// two forms taught in the system prompt: an existing instance's `id`, or
/// `new:<Class>:<n>` (1-based ordinal into that class's output-order bases).
/// Reverse-direction relations are skipped (they need the inverse predicate on
/// the target class — Phase 3). Single-cardinality relations keep only the
/// first resolved ref. Unresolved/malformed refs are dropped with `log::warn!`.
///
/// `existing_links` makes this idempotent across repeated passes (James #883 #4):
/// a resolved `(source, predicate, target)` edge already present in the graph is
/// not re-emitted, so a second continuous pass over the same proposals grows no
/// duplicate link (and mints no duplicate reifier node downstream). For a
/// single-cardinality relation this means an unchanged target is a no-op; a
/// *changed* target still emits additively (removing the stale edge needs a
/// removal op — out of scope, Phase 3 semantic diff), so at minimum no duplicate
/// edge is ever produced for an unchanged relation.
fn resolve_relation_links(
    shape: &ModelShape,
    inst: &ProposedInstance,
    source_base: &str,
    per_class: &HashMap<String, Vec<String>>,
    existing_ids: &HashSet<String>,
    existing_links: &ExistingLinks,
) -> Vec<Link> {
    let mut links = Vec::new();
    for rel in &shape.include_relations {
        if rel.direction != "forward" {
            // Reverse relations (belongsTo*) store the edge on the other class;
            // writing one requires the inverse predicate. Deferred to Phase 3.
            continue;
        }
        let Some(value) = inst.props.get(&rel.name) else {
            continue;
        };
        let Some(raw_refs) = normalize_refs(value) else {
            log::warn!(
                "interpretation: relation '{}' on '{}' had a non-string ref value; skipping",
                rel.name,
                inst.class
            );
            continue;
        };
        let single =
            matches!(rel.kind.as_str(), "hasOne" | "belongsToOne") || rel.max_count == Some(1);
        let mut emitted = 0usize;
        for raw in raw_refs {
            match resolve_ref(&raw, per_class, existing_ids) {
                Some(target) => {
                    if single && emitted >= 1 {
                        log::warn!(
                            "interpretation: single-cardinality relation '{}' on '{}' got extra ref '{}'; ignoring",
                            rel.name,
                            inst.class,
                            raw
                        );
                        continue;
                    }
                    // Idempotency guard (James #883 #4): the edge already exists,
                    // so re-emitting it would grow a duplicate link (and a fresh
                    // reifier node). Count it as satisfied — for a single-
                    // cardinality relation this also stops a later ref replacing
                    // an unchanged target — but write nothing.
                    if existing_links.contains(&(
                        source_base.to_string(),
                        rel.predicate.clone(),
                        target.clone(),
                    )) {
                        log::debug!(
                            "interpretation: relation '{}' on '{}' already links {} -> {}; skipping (idempotent)",
                            rel.name,
                            inst.class,
                            source_base,
                            target
                        );
                        emitted += 1;
                        continue;
                    }
                    links.push(Link {
                        source: source_base.to_string(),
                        predicate: Some(rel.predicate.clone()),
                        target,
                    });
                    emitted += 1;
                }
                None => {
                    log::warn!(
                        "interpretation: dropping unresolved relation ref '{}' on '{}.{}'",
                        raw,
                        inst.class,
                        rel.name
                    );
                }
            }
        }
    }
    links
}

/// Hold an instance's resolved relation links to the shape's `sh:minCount` and
/// `sh:disjoint` declarations, after dedup has had its say.
///
/// The model's references are only half the story: dedup maps a `new:` slot
/// onto an existing record, so two references written differently can resolve
/// to the same node. That is how an edge record came to join a task to itself —
/// the model proposed a "new" task carrying an existing task's title, and a
/// connection from it to that existing task.
///
/// - **Creating**: an instance that would break either constraint is not
///   written at all (`None`). A required relation left empty, or a disjoint
///   pair sharing a target, means the record cannot say what it exists to say,
///   and a half-made one is worse than none: it is drawn, counted and deduped
///   against.
/// - **Updating** a record the graph already holds (an upsert, or a dedup
///   match): only the new links that would make a disjoint pair overlap —
///   with each other or with what is already stored — are dropped. The record
///   and its scalar update stand.
fn apply_relation_constraints(
    shape: &ModelShape,
    class: &str,
    base: &str,
    creating: bool,
    links: Vec<Link>,
    existing_links: &ExistingLinks,
) -> Option<Vec<Link>> {
    let forward = || {
        shape
            .include_relations
            .iter()
            .filter(|r| r.direction == "forward")
    };
    // What each relation will hold once these links land: the new links plus,
    // for a record that already exists, the links it carries now.
    let holds = |predicate: &str, links: &[Link]| -> HashSet<String> {
        let mut targets: HashSet<String> = links
            .iter()
            .filter(|l| l.predicate.as_deref() == Some(predicate))
            .map(|l| l.target.clone())
            .collect();
        if !creating {
            targets.extend(
                existing_links
                    .iter()
                    .filter(|(s, p, _)| s == base && p == predicate)
                    .map(|(_, _, t)| t.clone()),
            );
        }
        targets
    };

    if creating {
        if let Some(missing) =
            forward().find(|r| r.required && holds(&r.predicate, &links).is_empty())
        {
            log::warn!(
                "⚠️ 🧠 interpretation: not creating {class} {base}: required relation {} resolved to nothing",
                missing.predicate
            );
            return None;
        }
    }

    let mut kept = links;
    for rel in forward().filter(|r| !r.disjoint.is_empty()) {
        for other in &rel.disjoint {
            let ours = holds(&rel.predicate, &kept);
            let theirs = holds(other, &kept);
            let shared: Vec<&String> = ours.intersection(&theirs).collect();
            if shared.is_empty() {
                continue;
            }
            if creating {
                log::warn!(
                    "⚠️ 🧠 interpretation: not creating {class} {base}: {} and {other} would both point at {shared:?}",
                    rel.predicate
                );
                return None;
            }
            // The record stands; only the overlapping new links go, which is
            // the expected outcome of a later pass re-pointing a stored edge.
            log::debug!(
                "🧠 interpretation: dropping links from {base} that would make {} and {other} both point at {shared:?}",
                rel.predicate
            );
            let shared: HashSet<String> = shared.into_iter().cloned().collect();
            kept.retain(|l| {
                let on_pair = l.predicate.as_deref() == Some(rel.predicate.as_str())
                    || l.predicate.as_deref() == Some(other.as_str());
                !(on_pair && shared.contains(&l.target))
            });
        }
    }
    Some(kept)
}

/// Hold a whole batch of interpretation ops to the `sh:minCount` and
/// `sh:disjoint` its classes declare, before anything is written.
///
/// Run on the ops both paths produce — the single-shot planner's and the
/// harness's drained buffer — in the shared tail, so neither can write what
/// the other refuses. It works on ops rather than proposals because the harness
/// has no proposals: its `Create` and the `AddLinks` that give it its ends
/// arrive from separate tool calls, so a record's links are gathered by base
/// across the batch.
///
/// A refused record is gone from the batch entirely, and so is every link to
/// or from it: a sibling that referred to it does not get a link to a node that
/// is never written, and a dedup slot that mapped onto it does not write links
/// on that missing base. Losing that link can leave the sibling's own required
/// relation empty, so refusal repeats until nothing more is refused.
///
/// Links on a record that already exists — an `Update`, or a dedup match onto a
/// stored record, which emits `AddLinks` alone — keep the record and lose only
/// the links that would make a disjoint pair overlap, with each other or with
/// what is stored. A base whose class is not known, from this batch or from
/// `existing`, passes through unchecked.
pub(crate) fn enforce_relation_constraints(
    ops: Vec<InterpretationOp>,
    shapes: &[ModelShape],
    existing: &ExistingInstances,
    existing_links: &ExistingLinks,
) -> Vec<InterpretationOp> {
    let shape_of = |class: &str| {
        shapes
            .iter()
            .find(|s| class_label(&s.target_class, shapes) == class)
    };
    let mut created: Vec<(String, String)> = Vec::new();
    let mut class_of: HashMap<String, String> = HashMap::new();
    let mut links_of: HashMap<String, Vec<Link>> = HashMap::new();
    for op in &ops {
        match op {
            InterpretationOp::Create { base, class, .. } => {
                created.push((base.clone(), class.clone()));
                class_of.insert(base.clone(), class.clone());
            }
            InterpretationOp::Update { base, class, .. } => {
                class_of.insert(base.clone(), class.clone());
            }
            InterpretationOp::AddLinks { source, links } => {
                links_of
                    .entry(source.clone())
                    .or_default()
                    .extend(links.iter().cloned());
            }
        }
    }
    let class_for = |base: &str| -> Option<String> {
        class_of.get(base).cloned().or_else(|| {
            existing.get(base).and_then(|entries| {
                entries
                    .iter()
                    .find(|e| shape_of(&e.class).is_some())
                    .map(|e| e.class.clone())
            })
        })
    };
    let surviving = |links: &[Link], refused: &HashSet<String>| -> Vec<Link> {
        links
            .iter()
            .filter(|l| !refused.contains(&l.target))
            .cloned()
            .collect()
    };

    // Refuse creates until the set stops growing: each refusal can take away
    // a link a sibling's required relation was counting on.
    let mut refused: HashSet<String> = HashSet::new();
    loop {
        let mut grew = false;
        for (base, class) in &created {
            if refused.contains(base) {
                continue;
            }
            let Some(shape) = shape_of(class) else {
                continue;
            };
            let links = surviving(links_of.get(base).map_or(&[][..], |l| l), &refused);
            if apply_relation_constraints(shape, class, base, true, links, existing_links).is_none()
            {
                refused.insert(base.clone());
                grew = true;
            }
        }
        if !grew {
            break;
        }
    }

    // What each base may keep: everything that survives, held to the
    // constraints as an existing record for bases this batch does not create.
    let mut allowed: HashMap<String, HashSet<(Option<String>, String)>> = HashMap::new();
    for (base, links) in &links_of {
        if refused.contains(base) {
            continue;
        }
        let links = surviving(links, &refused);
        let creating = created.iter().any(|(b, _)| b == base);
        let kept = match (creating, class_for(base)) {
            (false, Some(class)) => match shape_of(&class) {
                Some(shape) => {
                    apply_relation_constraints(shape, &class, base, false, links, existing_links)
                        .unwrap_or_default()
                }
                None => links,
            },
            _ => links,
        };
        allowed.insert(
            base.clone(),
            kept.into_iter().map(|l| (l.predicate, l.target)).collect(),
        );
    }

    let mut out = Vec::with_capacity(ops.len());
    for op in ops {
        match op {
            InterpretationOp::Create { ref base, .. } if refused.contains(base) => {}
            InterpretationOp::AddLinks { source, links } => {
                if refused.contains(&source) {
                    continue;
                }
                let Some(allowed) = allowed.get(&source) else {
                    continue;
                };
                let before = links.len();
                let links: Vec<Link> = links
                    .into_iter()
                    .filter(|l| allowed.contains(&(l.predicate.clone(), l.target.clone())))
                    .collect();
                if links.len() < before {
                    log::debug!(
                        "🧠 interpretation: dropped {} link(s) from {source} to refused or overlapping targets",
                        before - links.len()
                    );
                }
                if !links.is_empty() {
                    out.push(InterpretationOp::AddLinks { source, links });
                }
            }
            op => out.push(op),
        }
    }
    out
}

/// Normalise a relation field value into a list of raw ref strings. A single
/// string becomes a 1-element vec; an array of strings passes through; anything
/// else (number, object, array with a non-string element) yields `None` so the
/// caller can warn and skip.
fn normalize_refs(value: &serde_json::Value) -> Option<Vec<String>> {
    match value {
        serde_json::Value::String(s) => Some(vec![s.clone()]),
        // Keep every string ref and skip (log) any non-string element, rather
        // than collecting into `Option<Vec>` — a single bad element there
        // dropped the WHOLE relation array (James #883). A malformed ref should
        // cost only itself, not the sibling refs the model got right.
        serde_json::Value::Array(arr) => Some(
            arr.iter()
                .filter_map(|v| match v.as_str() {
                    Some(s) => Some(s.to_string()),
                    None => {
                        log::debug!("interpretation: dropping non-string relation ref {v:?}");
                        None
                    }
                })
                .collect(),
        ),
        _ => None,
    }
}

/// Resolve one raw relation ref to a target base URI. `new:<Class>:<n>` looks up
/// the (n-1)th base of `<Class>` in `per_class`; anything else is treated as an
/// existing-id ref, accepted only if present in `existing_ids` (what the LLM was
/// shown). Returns `None` for out-of-range ordinals, unknown classes, unparsable
/// `<n>`, or invented ids.
fn resolve_ref(
    raw: &str,
    per_class: &HashMap<String, Vec<String>>,
    existing_ids: &HashSet<String>,
) -> Option<String> {
    if let Some(rest) = raw.strip_prefix("new:") {
        // `<Class>:<n>` — split on the LAST colon so class names are free to
        // contain none (they never contain one in practice, but be safe).
        let (class, n_str) = rest.rsplit_once(':')?;
        let n: usize = n_str.trim().parse().ok()?;
        if n == 0 {
            return None;
        }
        per_class.get(class)?.get(n - 1).cloned()
    } else if existing_ids.contains(raw) {
        Some(raw.to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;

    #[test]
    fn plan_ops_creates_without_id_and_updates_with_id() {
        // An `id` field marks an upsert: patch the existing node's scalar fields
        // (no fresh base, no re-run constructor). Absence of `id` = a create.
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        let raw = r#"[
      {"class":"Intention","title":"Write the design doc"},
      {"class":"Intention","id":"soa://existing/intention/42","title":"Write the design doc and circulate it","owner":"Nico"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // `id` is parsed into its own field, kept out of `props`.
        assert_eq!(
            proposed[1].id.as_deref(),
            Some("soa://existing/intention/42")
        );
        assert!(!proposed[1].props.contains_key("id"));

        // The upsert only fires when the graph actually holds that id — seed
        // known_existing_ids to mimic what `existing_instance_context` would
        // return in production. Without this the planner treats the id as
        // hallucinated and routes to Create.
        // Seed the existing node with its real class — Update routing now
        // requires the id to name a *same-class* instance (James #883).
        let known = existing_map(vec![InstanceContext {
            id: "soa://existing/intention/42".to_string(),
            title: "Write the design doc".to_string(),
            class: "Intention".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);
        let ops = plan_interpretation_ops_with_context(&shapes, &proposed, "soa://ext/", &known);
        assert_eq!(ops.len(), 2);

        match &ops[0] {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                assert!(base.starts_with("soa://ext/intention/"));
                assert_eq!(class, "Intention");
                // The scalar payload is handed to `create_subject`; the type flag
                // comes from the class constructor, never from the planner.
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the design doc")
                );
                assert!(
                    !values.contains_key("type"),
                    "planner must not carry the type flag; got {values:?}"
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
        match &ops[1] {
            InterpretationOp::Update {
                base,
                class,
                values,
            } => {
                assert_eq!(base, "soa://existing/intention/42");
                assert_eq!(class, "Intention");
                // update patches scalar fields on the EXISTING base…
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the design doc and circulate it")
                );
                assert_eq!(values.get("owner").and_then(|v| v.as_str()), Some("Nico"));
                // …and never re-writes the type flag.
                assert!(!values.contains_key("type"));
            }
            other => panic!("expected Update, got {other:?}"),
        }
    }

    #[test]
    fn normalize_refs_keeps_valid_refs_and_skips_non_strings() {
        use serde_json::json;
        assert_eq!(normalize_refs(&json!("t1")), Some(vec!["t1".to_string()]));
        assert_eq!(
            normalize_refs(&json!(["a", "b"])),
            Some(vec!["a".to_string(), "b".to_string()])
        );
        // Regression (James #883): one non-string element must NOT drop the whole
        // array — the good refs survive.
        assert_eq!(
            normalize_refs(&json!(["a", 5, "b", null])),
            Some(vec!["a".to_string(), "b".to_string()])
        );
        assert_eq!(normalize_refs(&json!(5)), None);
    }

    #[test]
    fn plan_ops_cross_class_id_routes_to_create_not_foreign_node() {
        // Regression (James #883): an `id` that names an existing instance of a
        // DIFFERENT class must Create, never Update. Updating a foreign-class
        // node runs this class's setters against it and clobbers links on shared
        // predicates. Only a same-class id may upsert.
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        // The graph holds a Task at this id…
        let known = existing_map(vec![InstanceContext {
            id: "soa://existing/task/42".to_string(),
            title: "Ship the MVP".to_string(),
            class: "Task".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);
        // …but the LLM emits an Intention pointing at the Task's id.
        let raw = r#"[
      {"class":"Intention","id":"soa://existing/task/42","title":"Ship the MVP"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops_with_context(&shapes, &proposed, "soa://ext/", &known);
        assert_eq!(ops.len(), 1);
        match &ops[0] {
            InterpretationOp::Create { base, class, .. } => {
                assert!(
                    base.starts_with("soa://ext/intention/"),
                    "cross-class id must mint a fresh base, got {base}"
                );
                assert_ne!(
                    base, "soa://existing/task/42",
                    "planner must not Update the foreign-class (Task) node"
                );
                assert_eq!(class, "Intention");
            }
            other => panic!("expected Create for a cross-class id, got {other:?}"),
        }
    }

    #[test]
    fn plan_ops_hallucinated_id_routes_to_create() {
        // The LLM sometimes invents an `id` that names no real base — copying the
        // format from the prompt but with a made-up path. Trusting it produces an
        // Update against a base that has no type flag, i.e. a scalar write no
        // reader will ever find. The planner must fall back to Create so the
        // instance lands somewhere visible.
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        let raw = r#"[
      {"class":"Intention","id":"soa://existing/intention/999","title":"Ghost"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // existing intentionally does NOT contain the proposed id.
        let ops = plan_interpretation_ops_with_context(
            &shapes,
            &proposed,
            "soa://ext/",
            &ExistingInstances::new(),
        );
        assert_eq!(ops.len(), 1);
        match &ops[0] {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                assert!(
                    base.starts_with("soa://ext/intention/"),
                    "hallucinated id must be replaced by a fresh minted base, got {base}"
                );
                assert_ne!(
                    base, "soa://existing/intention/999",
                    "planner must not reuse the hallucinated id"
                );
                assert_eq!(class, "Intention");
                assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Ghost"));
            }
            other => panic!("expected Create for hallucinated id, got {other:?}"),
        }
    }

    #[test]
    fn plan_ops_drops_unknown_class() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let raw = r#"[
      {"class":"Task","title":"Real"},
      {"class":"Hallucinated","title":"Nope"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&shapes, &proposed, "soa://ext/");
        assert_eq!(ops.len(), 1, "unknown-class proposal must be dropped");
        assert!(matches!(ops[0], InterpretationOp::Create { .. }));
    }

    #[test]
    fn plan_ops_empty_input_yields_no_ops() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        assert!(plan_interpretation_ops(&shapes, &[], "soa://ext/").is_empty());
    }

    #[test]
    fn relation_properties_are_excluded_from_interpretation() {
        // A shape whose interpretation hint also declares a link-typed relation
        // (`blocks`). load_shape lists that relation in `properties` too, so
        // without the guard it would be offered to the LLM as a scalar field and —
        // if the LLM emits it — literal-encoded by a setter into a bogus target.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        // Sanity: the relation really is present in both lists.
        assert!(
            shape.include_relations.iter().any(|r| r.name == "blocks"),
            "fixture must declare a `blocks` relation"
        );
        assert!(
            shape.properties.iter().any(|p| p.name == "blocks"),
            "load_shape is expected to also list the relation in properties"
        );

        // 1. build_interpretation_input must not offer the relation as a field.
        let input = build_interpretation_input(
            &[shape.clone()],
            &[TranscriptTurn::from_speaker_text("Nico", "block it")],
            &no_existing(),
            &[],
            &HashMap::new(),
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let field_names: Vec<&str> = v["classes"][0]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|f| f["name"].as_str())
            .collect();
        assert!(field_names.contains(&"title"), "scalar field must remain");
        assert!(
            !field_names.contains(&"blocks"),
            "relation must not be offered to the LLM as a field; got {field_names:?}"
        );

        // Forward relations surface in a dedicated `relations` block (so the LLM
        // knows what refs it *can* fill), separate from the scalar `fields`. The
        // `blocks` relation on `TASK_WITH_RELATION_SDNA` is `hasMany` forward, so
        // it belongs there with its target class + hint.
        let relations = v["classes"][0]["relations"].as_array().unwrap();
        let blocks_rel = relations
            .iter()
            .find(|r| r["name"].as_str() == Some("blocks"))
            .expect("hasMany forward relation must appear in `relations` block");
        assert_eq!(
            blocks_rel["targetClass"].as_str(),
            Some("Task"),
            "relation targetClass must reflect ShapeRelation.target_class_name"
        );
        assert_eq!(
            blocks_rel["hint"].as_str(),
            Some("Other tasks this one blocks."),
            "relation hint must reflect the sibling property's interpretationHint"
        );

        // 2. The scalar write payload must not carry the relation either, even
        //    when the LLM emits it (here as an array — exactly the case a setter
        //    would literal-encode into a bogus `literal:json:` target).
        let raw = r#"[{"class":"Task","title":"Do X","blocks":["soa://t2","soa://t3"]}]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");
        let InterpretationOp::Create { values, .. } = &ops[0] else {
            panic!("expected a Create, got {:?}", ops[0]);
        };
        assert!(
            !values.contains_key("blocks"),
            "relation must never reach the scalar write path; got {values:?}"
        );
        // The scalar title still lands.
        assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Do X"));
    }

    #[test]
    fn relations_write_link_to_new_sibling() {
        // Two Tasks minted in one pass; the first `blocks` the second via a
        // `new:Task:2` ref. The relation must resolve to the second Task's freshly
        // minted base and land as a real `ns://blocks` link (not a literal).
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let raw = r#"[
      {"class":"Task","title":"Ship the API","blocks":["new:Task:2"]},
      {"class":"Task","title":"Write the client"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let first_base = nth_create_base(&ops, 0);
        let second_base = nth_create_base(&ops, 1);
        let targets = targets_of(addlinks_for(&ops, &first_base), "ns://blocks");
        assert_eq!(
            targets,
            vec![second_base],
            "blocks ref `new:Task:2` must point at the second Task's base"
        );
        // The target is an instance base, never a literal URI.
        assert!(
            !targets[0].starts_with("literal:"),
            "relation target must be an instance URI, not a literal"
        );
    }

    #[test]
    fn relations_write_link_to_existing_id() {
        // A single new Task blocks an existing one, referenced by its id. Only ids
        // the model was shown (in `known_existing_ids`) are accepted as targets.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let existing_id = "soa://ext/task/already-here".to_string();
        let known = existing_ids(&[existing_id.as_str()]);
        let raw = format!(r#"[{{"class":"Task","title":"New work","blocks":["{existing_id}"]}}]"#,);
        let proposed = parse_interpretation_response(&raw).unwrap();
        let ops = plan_interpretation_ops_with_context(&[shape], &proposed, "soa://ext/", &known);

        let base = nth_create_base(&ops, 0);
        assert_eq!(
            targets_of(addlinks_for(&ops, &base), "ns://blocks"),
            vec![existing_id],
            "existing-id blocks ref must resolve to that id"
        );
    }

    #[test]
    fn relations_drop_unresolved_ref() {
        // An out-of-range ordinal and an invented id are both unresolvable — the
        // node still lands, just with no relation link and no panic.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let raw = r#"[
      {"class":"Task","title":"Lonely","blocks":["new:Task:99","soa://ext/task/never-shown"]}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // Empty known-ids: the bare id ref is "invented" from the model's POV.
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let base = nth_create_base(&ops, 0);
        assert!(
            addlinks_for(&ops, &base).is_empty(),
            "unresolved refs must not become links; got {ops:#?}"
        );
        // The node still lands with its scalar — a dropped relation never drops it.
        let InterpretationOp::Create { values, .. } = &ops[0] else {
            panic!("expected a Create, got {:?}", ops[0]);
        };
        assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Lonely"));
    }

    #[test]
    fn relations_hasone_takes_first_of_array() {
        // A single-cardinality (`hasOne`) relation given an array keeps only the
        // first resolved ref. `parent` is hasOne forward -> Task.
        let sdna = r#"{
      "target_class":"ns://Task",
      "interpretation_hint":"A task.",
      "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
        {"path":"ns://parent","name":"parent","relation_kind":"hasOne","target_class_name":"Task","class":"ns://TaskShape","interpretation_hint":"The parent task."}
      ]
    }"#;
        let shape = shape_from_sdna("Task", sdna);
        // Sanity: the fixture really is single-cardinality forward.
        let parent_rel = shape
            .include_relations
            .iter()
            .find(|r| r.name == "parent")
            .expect("parent relation present");
        assert_eq!(parent_rel.direction, "forward");
        assert!(
            parent_rel.kind == "hasOne" || parent_rel.max_count == Some(1),
            "parent must be single-cardinality"
        );

        let raw = r#"[
      {"class":"Task","title":"Child","parent":["new:Task:2","new:Task:3"]},
      {"class":"Task","title":"First parent"},
      {"class":"Task","title":"Second parent"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let child_base = nth_create_base(&ops, 0);
        let first_parent_base = nth_create_base(&ops, 1);
        assert_eq!(
            targets_of(addlinks_for(&ops, &child_base), "ns://parent"),
            vec![first_parent_base],
            "hasOne must keep only the first resolved ref (new:Task:2)"
        );
    }

    #[test]
    fn relations_from_update_target_emit_addlinks() {
        // A relation whose source is an *existing* (upsert) instance must not fold
        // into that instance's scalar Update — it becomes an additive `AddLinks`
        // op (relations to a fresh sibling grow the graph, never replace scalars).
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let existing_id = "soa://ext/task/existing".to_string();
        let raw = format!(
            r#"[
          {{"class":"Task","id":"{existing_id}","title":"Renamed","blocks":["new:Task:2"]}},
          {{"class":"Task","title":"Fresh sibling"}}
        ]"#,
        );
        let proposed = parse_interpretation_response(&raw).unwrap();
        // Same-class seed so the id upserts (routing now checks class — James #883).
        let known = existing_map(vec![InstanceContext {
            id: existing_id.clone(),
            title: "Existing".to_string(),
            class: "Task".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);
        let ops = plan_interpretation_ops_with_context(&[shape], &proposed, "soa://ext/", &known);

        // The Update carries the retitled scalar, no relation value.
        let update = ops
            .iter()
            .find_map(|op| match op {
                InterpretationOp::Update { base, values, .. } if base == &existing_id => {
                    Some(values)
                }
                _ => None,
            })
            .expect("expected an Update on the existing id");
        assert!(
            !update.contains_key("blocks"),
            "scalar Update must not carry the relation; got {update:?}"
        );
        assert_eq!(
            update.get("title").and_then(|v| v.as_str()),
            Some("Renamed")
        );

        // The relation lands as an additive AddLinks on the same base, pointing at
        // the fresh sibling.
        let sibling_base = nth_create_base(&ops, 0);
        assert_eq!(
            targets_of(addlinks_for(&ops, &existing_id), "ns://blocks"),
            vec![sibling_base],
            "AddLinks must point the blocks relation at the fresh sibling's base"
        );
    }

    #[test]
    fn plan_resolved_indexes_dup_slot_so_ordinal_resolves_to_existing_base() {
        // James #883: a proposal deduped as already-existing must still occupy
        // its slot in the emission-order ordinal index, mapped to the REAL
        // existing base — so a sibling's `new:<Class>:<n>` links the existing
        // node, not the wrong sibling or a fresh mint. Emission order:
        // [Alpha(new), Existing(dup-of-existing), Gamma(new)]; Gamma.blocks =
        // `new:Task:2` must target Existing's base, and no Create is emitted for
        // the dup.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let existing_base = "soa://ext/task/existing".to_string();
        let raw = r#"[
          {"class":"Task","title":"Alpha"},
          {"class":"Task","title":"Existing Title"},
          {"class":"Task","title":"Gamma","blocks":["new:Task:2"]}
        ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let resolved = vec![
            (proposed[0].clone(), Resolution::Keep),
            (
                proposed[1].clone(),
                Resolution::DupOfExisting(existing_base.clone()),
            ),
            (proposed[2].clone(), Resolution::Keep),
        ];
        let known = existing_map(vec![InstanceContext {
            id: existing_base.clone(),
            title: "Existing Title".to_string(),
            class: "Task".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);
        let ops = plan_interpretation_ops_resolved(
            &[shape],
            &resolved,
            "soa://ext/",
            &known,
            &ExistingLinks::new(),
        );

        // The dup slot is never re-created.
        assert!(
            !ops.iter().any(|op| matches!(
                op,
                InterpretationOp::Create { base, .. } if base == &existing_base
            )),
            "the dup-of-existing slot must not be re-created; got {ops:#?}"
        );
        // Creates, in order: Alpha (#0), Gamma (#1) — the dup took no create slot.
        let gamma_base = nth_create_base(&ops, 1);
        assert_eq!(
            targets_of(addlinks_for(&ops, &gamma_base), "ns://blocks"),
            vec![existing_base],
            "new:Task:2 (the 2nd Task in emission order = the dup) must resolve \
             to the existing Task's base, not a sibling or fresh mint"
        );
    }

    /// A Task the graph holds, as the planner's existing-instance context.
    fn existing_task(id: &str, title: &str) -> InstanceContext {
        InstanceContext {
            id: id.to_string(),
            title: title.to_string(),
            class: "Task".to_string(),
            properties: std::collections::BTreeMap::new(),
        }
    }

    fn connection_shapes() -> Vec<ModelShape> {
        vec![
            shape_from_sdna("Task", TASK_SDNA),
            shape_from_sdna("Connection", CONNECTION_SDNA),
        ]
    }

    fn creates_of(ops: &[InterpretationOp], class: &str) -> usize {
        ops.iter()
            .filter(|op| matches!(op, InterpretationOp::Create { class: c, .. } if c == class))
            .count()
    }

    #[test]
    fn load_shape_reads_disjoint_and_required_relations() {
        let shape = shape_from_sdna("Connection", CONNECTION_SDNA);
        let from = shape
            .include_relations
            .iter()
            .find(|r| r.name == "from")
            .unwrap();
        assert_eq!(from.disjoint, vec!["ns://to".to_string()]);
        assert!(from.required);
    }

    #[test]
    fn plan_refuses_an_edge_whose_ends_dedup_to_one_record() {
        // What a live extraction pass wrote: the model proposed a "new" task
        // carrying an existing task's exact title, and a connection from it to
        // that existing task. Identity dedup turned the new task into the
        // existing one, so both ends resolved to the same node.
        let task = "soa://ext/task/existing";
        let raw = format!(
            r#"[
              {{"class":"Task","title":"Refresh the stream"}},
              {{"class":"Connection","label":"leads to","from":"new:Task:1","to":"{task}"}}
            ]"#
        );
        let proposed = parse_interpretation_response(&raw).unwrap();
        let resolved = vec![
            (
                proposed[0].clone(),
                Resolution::DupOfExisting(task.to_string()),
            ),
            (proposed[1].clone(), Resolution::Keep),
        ];
        let known = existing_map(vec![existing_task(task, "Refresh the stream")]);
        let ops = plan_interpretation_ops_resolved(
            &connection_shapes(),
            &resolved,
            "soa://ext/",
            &known,
            &ExistingLinks::new(),
        );
        let ops =
            enforce_relation_constraints(ops, &connection_shapes(), &known, &ExistingLinks::new());

        assert_eq!(
            creates_of(&ops, "Connection"),
            0,
            "an edge from a record to itself must not be written; got {ops:#?}"
        );
        assert!(
            !ops.iter()
                .any(|op| matches!(op, InterpretationOp::AddLinks { links, .. }
                if links.iter().any(|l| l.predicate.as_deref() == Some("ns://from")))),
            "and none of its links either; got {ops:#?}"
        );
    }

    #[test]
    fn plan_writes_an_edge_between_two_different_records() {
        let raw = r#"[
          {"class":"Task","title":"Fix the video"},
          {"class":"Task","title":"Refresh the stream"},
          {"class":"Connection","label":"leads to","from":"new:Task:1","to":"new:Task:2"}
        ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops_with_context(
            &connection_shapes(),
            &proposed,
            "soa://ext/",
            &ExistingInstances::new(),
        );
        let ops = enforce_relation_constraints(
            ops,
            &connection_shapes(),
            &ExistingInstances::new(),
            &ExistingLinks::new(),
        );

        let edge = nth_create_base(&ops, 2);
        let links = addlinks_for(&ops, &edge);
        assert_eq!(
            targets_of(links, "ns://from"),
            vec![nth_create_base(&ops, 0)]
        );
        assert_eq!(targets_of(links, "ns://to"), vec![nth_create_base(&ops, 1)]);
    }

    #[test]
    fn plan_refuses_an_edge_missing_a_required_end() {
        // Seen live: a source the model wrote as an object, or as an id it
        // invented, is dropped as unresolved — and the record was written
        // anyway, an edge with one end or none.
        let task = "soa://ext/task/existing";
        let raw = format!(
            r#"[
              {{"class":"Connection","label":"leads to","from":"soa://existing/event/2026-10-16T11:24:34Z","to":"{task}"}}
            ]"#
        );
        let proposed = parse_interpretation_response(&raw).unwrap();
        let known = existing_map(vec![existing_task(task, "Refresh the stream")]);
        let ops = plan_interpretation_ops_with_context(
            &connection_shapes(),
            &proposed,
            "soa://ext/",
            &known,
        );
        let ops =
            enforce_relation_constraints(ops, &connection_shapes(), &known, &ExistingLinks::new());

        assert!(
            ops.is_empty(),
            "nothing to write for a one-ended edge; got {ops:#?}"
        );
    }

    #[test]
    fn plan_drops_an_update_link_that_would_join_an_edge_to_itself() {
        // An edge the graph holds already, from the task. A later pass naming
        // the same edge points its other end at that task too: the new link is
        // dropped and the stored edge left as it was.
        let task = "soa://ext/task/existing";
        let edge = "soa://ext/connection/existing";
        let raw = format!(
            r#"[
              {{"class":"Connection","id":"{edge}","label":"follows from","to":"{task}"}}
            ]"#
        );
        let proposed = parse_interpretation_response(&raw).unwrap();
        let mut known = existing_map(vec![existing_task(task, "Refresh the stream")]);
        known.insert(
            edge.to_string(),
            vec![InstanceContext {
                id: edge.to_string(),
                title: String::new(),
                class: "Connection".to_string(),
                properties: std::collections::BTreeMap::new(),
            }],
        );
        let mut stored = ExistingLinks::new();
        stored.insert((edge.to_string(), "ns://from".to_string(), task.to_string()));
        stored.insert((
            edge.to_string(),
            "ns://to".to_string(),
            "soa://ext/task/other".to_string(),
        ));

        let resolved = vec![(proposed[0].clone(), Resolution::Keep)];
        let ops = plan_interpretation_ops_resolved(
            &connection_shapes(),
            &resolved,
            "soa://ext/",
            &known,
            &stored,
        );
        let ops = enforce_relation_constraints(ops, &connection_shapes(), &known, &stored);

        assert!(
            targets_of(addlinks_for(&ops, edge), "ns://to").is_empty(),
            "the edge's `to` must not gain the task its `from` names; got {ops:#?}"
        );
        assert!(
            ops.iter()
                .any(|op| matches!(op, InterpretationOp::Update { base, .. } if base == edge)),
            "the scalar update still applies; got {ops:#?}"
        );
    }

    #[test]
    fn enforce_refuses_a_record_whose_required_relation_names_a_refused_sibling() {
        // data-bot on #1473: refusing one record left its minted base resolvable,
        // so a sibling's `new:Note:1` linked a node nothing writes — and passed
        // its own required check with it. Note 1 is refused (its `about` was
        // invented); Note 2's only `about` is Note 1, so it goes too.
        let raw = r#"[
          {"class":"Note","text":"a","about":"soa://invented/x"},
          {"class":"Note","text":"b","about":"new:Note:1"}
        ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let shapes = vec![shape_from_sdna("Note", NOTE_SDNA)];
        let ops = plan_interpretation_ops_with_context(
            &shapes,
            &proposed,
            "soa://ext/",
            &ExistingInstances::new(),
        );
        let ops = enforce_relation_constraints(
            ops,
            &shapes,
            &ExistingInstances::new(),
            &ExistingLinks::new(),
        );

        assert!(ops.is_empty(), "both notes must be refused; got {ops:#?}");

        // The other way round: the note that refers comes first, so it has
        // passed before the one it names is refused. Refusal repeats until
        // nothing more is refused, which is what takes it out.
        let raw = r#"[
          {"class":"Note","text":"b","about":"new:Note:2"},
          {"class":"Note","text":"a","about":"soa://invented/x"}
        ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops_with_context(
            &shapes,
            &proposed,
            "soa://ext/",
            &ExistingInstances::new(),
        );
        let ops = enforce_relation_constraints(
            ops,
            &shapes,
            &ExistingInstances::new(),
            &ExistingLinks::new(),
        );
        assert!(
            ops.is_empty(),
            "the forward ref is refused too; got {ops:#?}"
        );
    }

    #[test]
    fn enforce_writes_no_links_on_a_refused_records_dedup_slot() {
        // data-bot on #1473: a `DupOfEarlier` of a refused slot took its base
        // and emitted `AddLinks` on it — links on a node with no `Create`, and
        // no type marker for any class query to find. Checked over the batch, a
        // duplicate's links are the record's own: they count toward its
        // required relation, and go with it when it is refused.
        let thing = "soa://ext/thing/existing";
        let shapes = vec![shape_from_sdna("Note", NOTE_SDNA)];
        let known = existing_map(vec![InstanceContext {
            id: thing.to_string(),
            title: "x".to_string(),
            class: "Thing".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);
        let run = |second_about: &str| {
            // The duplicate also points `seeAlso` at a real record, so it
            // carries a link that survives resolution whatever happens to
            // the note's `about`.
            let raw = format!(
                r#"[
                  {{"class":"Note","text":"a","about":"soa://invented/x"}},
                  {{"class":"Note","text":"a","about":"{second_about}","seeAlso":"{thing}"}}
                ]"#
            );
            let proposed = parse_interpretation_response(&raw).unwrap();
            let resolved = vec![
                (proposed[0].clone(), Resolution::Keep),
                (proposed[1].clone(), Resolution::DupOfEarlier(0)),
            ];
            let ops = plan_interpretation_ops_resolved(
                &shapes,
                &resolved,
                "soa://ext/",
                &known,
                &ExistingLinks::new(),
            );
            enforce_relation_constraints(ops, &shapes, &known, &ExistingLinks::new())
        };

        // Neither proposal gives the note an `about`: nothing is written —
        // including the duplicate's `seeAlso`, which resolved but sits on the
        // refused base.
        let ops = run("soa://invented/y");
        assert!(ops.is_empty(), "nothing on the refused base; got {ops:#?}");

        // The duplicate supplies it: the note is written, and every link sits
        // on a base this batch creates.
        let ops = run(thing);
        let note = nth_create_base(&ops, 0);
        assert_eq!(
            targets_of(addlinks_for(&ops, &note), "ns://about"),
            vec![thing.to_string()]
        );
        assert!(
            ops.iter().all(|op| match op {
                InterpretationOp::AddLinks { source, .. } => source == &note,
                _ => true,
            }),
            "no links on a base without a Create; got {ops:#?}"
        );
    }

    #[test]
    fn enforce_holds_harness_ops_whose_links_arrive_separately() {
        // The harness builds `Create` and the `AddLinks` giving it its ends from
        // separate tool calls (`_propose_create`, `propose_link_child`), and
        // never runs the planner. The batch check gathers a record's links
        // across ops, so a self-edge built that way is refused like any other.
        let x = "soa://ext/task/x";
        let y = "soa://ext/task/y";
        let link = |source: &str, predicate: &str, target: &str| InterpretationOp::AddLinks {
            source: source.to_string(),
            links: vec![Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }],
        };
        let create = |base: &str| InterpretationOp::Create {
            base: base.to_string(),
            class: "Connection".to_string(),
            values: serde_json::Map::new(),
        };
        let ops = vec![
            create("soa://ext/connection/loop"),
            link("soa://ext/connection/loop", "ns://from", x),
            link("soa://ext/connection/loop", "ns://to", x),
            create("soa://ext/connection/edge"),
            link("soa://ext/connection/edge", "ns://from", x),
            link("soa://ext/connection/edge", "ns://to", y),
        ];
        let known = existing_map(vec![existing_task(x, "X"), existing_task(y, "Y")]);
        let ops =
            enforce_relation_constraints(ops, &connection_shapes(), &known, &ExistingLinks::new());

        assert_eq!(creates_of(&ops, "Connection"), 1, "{ops:#?}");
        assert!(addlinks_for(&ops, "soa://ext/connection/loop").is_empty());
        assert_eq!(nth_create_base(&ops, 0), "soa://ext/connection/edge");
    }

    #[test]
    fn plan_ops_hasone_relation_idempotent_across_passes() {
        // James #883 #4: rerun the planner over the SAME proposal with pass-1's
        // edge already present as existing state — a single-cardinality (hasOne)
        // relation whose target is unchanged must NOT re-emit. No duplicate
        // AddLinks on pass 2 ⇒ no duplicate link, and no duplicate reifier node
        // downstream (the reifier IRI hashes in the link timestamp, so a re-add
        // would mint a fresh one — see sparql_store::make_reifier_iri).
        let sdna = r#"{
      "target_class":"ns://Task",
      "interpretation_hint":"A task.",
      "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
        {"path":"ns://parent","name":"parent","relation_kind":"hasOne","target_class_name":"Task","class":"ns://TaskShape","interpretation_hint":"The parent task."}
      ]
    }"#;
        let shape = shape_from_sdna("Task", sdna);
        let child = "soa://ext/task/child".to_string();
        let parent = "soa://ext/task/parent".to_string();
        // Both instances already exist (same class) so the child upserts and
        // references the parent by a stable id — the source base is identical
        // across passes, which is what makes the existence check meaningful.
        let known = existing_map(vec![
            InstanceContext {
                id: child.clone(),
                title: "Child".to_string(),
                class: "Task".to_string(),
                properties: std::collections::BTreeMap::new(),
            },
            InstanceContext {
                id: parent.clone(),
                title: "Parent".to_string(),
                class: "Task".to_string(),
                properties: std::collections::BTreeMap::new(),
            },
        ]);
        let proposed = vec![proposal(
            "Task",
            Some(&child),
            &[("parent", serde_json::json!([parent.clone()]))],
        )];
        let resolved: Vec<_> = proposed
            .iter()
            .map(|p| (p.clone(), Resolution::Keep))
            .collect();

        // Pass 1: no existing links -> the edge is emitted once.
        let ops1 = plan_interpretation_ops_resolved(
            std::slice::from_ref(&shape),
            &resolved,
            "soa://ext/",
            &known,
            &ExistingLinks::new(),
        );
        assert_eq!(
            targets_of(addlinks_for(&ops1, &child), "ns://parent"),
            vec![parent.clone()],
            "pass 1 must emit the hasOne edge"
        );

        // Pass 2: feed pass-1's edge back as existing state -> no re-emission.
        let existing_links = links_from_ops(&ops1);
        let ops2 = plan_interpretation_ops_resolved(
            std::slice::from_ref(&shape),
            &resolved,
            "soa://ext/",
            &known,
            &existing_links,
        );
        assert!(
            addlinks_for(&ops2, &child).is_empty(),
            "pass 2 must not re-emit the unchanged hasOne edge; got {ops2:#?}"
        );
        assert!(
            !ops2.iter().any(|op| matches!(
                op,
                InterpretationOp::AddLinks { source, .. } if source == &child
            )),
            "pass 2 must emit no AddLinks op at all for an unchanged relation; got {ops2:#?}"
        );
    }

    #[test]
    fn plan_ops_hasmany_relation_idempotent_across_passes() {
        // James #883 #4 (multi-cardinality): a `hasMany` (`blocks`) relation from
        // an existing hub to two existing targets must not re-emit either edge on
        // a second pass — while a genuinely NEW target is still added (additive
        // growth is preserved, only the duplicates are suppressed).
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let hub = "soa://ext/task/hub".to_string();
        let a = "soa://ext/task/a".to_string();
        let b = "soa://ext/task/b".to_string();
        let c = "soa://ext/task/c".to_string();
        let known = existing_map(
            [&hub, &a, &b, &c]
                .iter()
                .map(|id| InstanceContext {
                    id: (**id).clone(),
                    title: (**id).clone(),
                    class: "Task".to_string(),
                    properties: std::collections::BTreeMap::new(),
                })
                .collect(),
        );
        let base_proposed = vec![proposal(
            "Task",
            Some(&hub),
            &[("blocks", serde_json::json!([a.clone(), b.clone()]))],
        )];
        let resolved: Vec<_> = base_proposed
            .iter()
            .map(|p| (p.clone(), Resolution::Keep))
            .collect();

        // Pass 1: both edges emitted.
        let ops1 = plan_interpretation_ops_resolved(
            std::slice::from_ref(&shape),
            &resolved,
            "soa://ext/",
            &known,
            &ExistingLinks::new(),
        );
        let mut t1 = targets_of(addlinks_for(&ops1, &hub), "ns://blocks");
        t1.sort();
        assert_eq!(
            t1,
            vec![a.clone(), b.clone()],
            "pass 1 must emit both blocks edges"
        );

        let existing_links = links_from_ops(&ops1);

        // Pass 2: identical proposal -> both edges already exist -> nothing added.
        let ops2 = plan_interpretation_ops_resolved(
            std::slice::from_ref(&shape),
            &resolved,
            "soa://ext/",
            &known,
            &existing_links,
        );
        assert!(
            addlinks_for(&ops2, &hub).is_empty(),
            "pass 2 must not re-emit existing hasMany edges; got {ops2:#?}"
        );

        // Pass 3: same two targets PLUS a new one -> only the new edge is added.
        let grown_proposed = vec![proposal(
            "Task",
            Some(&hub),
            &[(
                "blocks",
                serde_json::json!([a.clone(), b.clone(), c.clone()]),
            )],
        )];
        let resolved_grown: Vec<_> = grown_proposed
            .iter()
            .map(|p| (p.clone(), Resolution::Keep))
            .collect();
        let ops3 = plan_interpretation_ops_resolved(
            std::slice::from_ref(&shape),
            &resolved_grown,
            "soa://ext/",
            &known,
            &existing_links,
        );
        assert_eq!(
            targets_of(addlinks_for(&ops3, &hub), "ns://blocks"),
            vec![c.clone()],
            "only the genuinely new target must be added; the two existing edges stay suppressed"
        );
    }

    #[tokio::test]
    async fn apply_ops_upsert_replaces_scalar_without_touching_type_flag() {
        // End-to-end (no LLM): seed a real perspective with an Intention whose
        // title/owner are already set, then apply an Update op that patches the
        // scalar fields on that same base. The old scalar values must be GONE (the
        // setters are `setSingleTarget` = replace-per-predicate) and the new ones
        // present; the type flag stays untouched.
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://existing/intention/upsert-target";

        // Seed: original title + owner on the target instance.
        seed_instance(&mut perspective, &ctx, &shapes[0], base, "Draft the design").await;
        apply_one(
            &mut perspective,
            &shapes,
            &ctx,
            proposal(
                "Intention",
                Some(base),
                &[("owner", serde_json::json!("Nico"))],
            ),
        )
        .await;
        assert_eq!(
            decoded_targets(&perspective, base, "ns://owner").await,
            vec![serde_json::json!("Nico")],
            "sanity: the seeding update wrote the owner"
        );

        // Now upsert: same base, revised title + new owner.
        let ops = apply_one(
            &mut perspective,
            &shapes,
            &ctx,
            proposal(
                "Intention",
                Some(base),
                &[
                    (
                        "title",
                        serde_json::json!("Draft the design and circulate it"),
                    ),
                    ("owner", serde_json::json!("Josh")),
                ],
            ),
        )
        .await;
        assert!(matches!(ops[0], InterpretationOp::Update { .. }));

        // Type flag survives (the constructor wrote it; updates never touch it).
        let type_links = decoded_targets(&perspective, base, "ns://type").await;
        assert_eq!(
            type_links,
            vec![serde_json::json!("ns://intention")],
            "type flag must remain exactly once"
        );
        // Title: exactly the new value, no residue of the old one.
        assert_eq!(
            decoded_targets(&perspective, base, "ns://title").await,
            vec![serde_json::json!("Draft the design and circulate it")],
            "title must have been replaced (no old-value residue)"
        );
        // Owner: exactly the new value ("Nico" gone).
        assert_eq!(
            decoded_targets(&perspective, base, "ns://owner").await,
            vec![serde_json::json!("Josh")],
            "owner must have been replaced"
        );
    }

    #[tokio::test]
    async fn apply_ops_addlinks_are_additive() {
        // Two `blocks` edges on the same base must coexist: relations are appended,
        // never replaced-per-predicate the way scalars are.
        use crate::types::LinkQuery;
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("Task", TASK_WITH_RELATION_SDNA)]).await;
        let base = "soa://existing/task/hub";
        seed_instance(&mut perspective, &ctx, &shapes[0], base, "Hub task").await;

        for target in ["soa://ext/task/a", "soa://ext/task/b"] {
            apply_interpretation_ops(
                &mut perspective,
                &[InterpretationOp::AddLinks {
                    source: base.to_string(),
                    links: vec![Link {
                        source: base.to_string(),
                        predicate: Some("ns://blocks".to_string()),
                        target: target.to_string(),
                    }],
                }],
                &ctx,
            )
            .await
            .expect("apply AddLinks");
        }

        let links = perspective
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ns://blocks".to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links");
        let mut targets: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
        targets.sort();
        assert_eq!(
            targets,
            vec![
                "soa://ext/task/a".to_string(),
                "soa://ext/task/b".to_string()
            ],
            "AddLinks must accumulate, not replace"
        );
    }
}
