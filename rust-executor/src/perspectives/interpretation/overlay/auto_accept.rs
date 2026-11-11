//! Automatic accept of eligible inferred items at the end of a pass.
//!
//! The class-level `auto_accept` rule (`ad4m://auto_accept` on the SHACL
//! shape, surfaced as `ModelShape::auto_accept`) lets a DNA declare that
//! inferred instances of a class need no human click on
//! `accept_interpretation`:
//!
//! * `always` — every inferred item of the class is accepted,
//! * `when_placed` — only items that are *placed*: at least one of the
//!   class's parent relations points at another item, either as an accepted
//!   link or as a still-staged `inferred/<p>` overlay value,
//! * `never` / absent — nothing is auto-accepted; the overlay stays pending.
//!
//! A **parent relation** is any declared forward `hasOne` relation
//! (`ShapeRelation` with `kind == "hasOne" && direction == "forward"`) — the
//! definition is structural, no class- or property-names enter here.
//!
//! Everything routes through the human path's own
//! [`accept_interpretation`]: auto-accept *is* the accept button, pressed by
//! the shape declaration rather than a user. A per-item failure is logged
//! and does not abort the pass.
//!
//! # What auto-accept never does
//!
//! **It never resolves another agent's overlay.** In a shared perspective a
//! base carries each pending suggestion under the DID that staged it. Every
//! overlay link on the base — `kind`, `run`, every `inferred/<p>` — must be
//! authored by the DID this pass signs with; a single foreign-authored one
//! leaves the item pending for that agent (or a person) to resolve, instead
//! of this pass re-signing its values as its own.
//!
//! **It never changes a real value, and never restores a removed one.** The
//! §4 gate (see [`super`]) stages a proposal instead of writing it when the
//! real value is not the LLM's to overwrite: the item was written or accepted
//! by a person, or a person edited it after the last inference. Accepting
//! such an overlay replaces the real value with the staged one, which is a
//! person's decision. So an item is auto-accepted only while every staged
//! `inferred/<p>` equals the real `<p>` it would replace: the accept then
//! overwrites nothing. A staged value whose real `<p>` is gone means a person
//! deleted the field since the snapshot was taken — accepting would write it
//! back over their deletion, so the item stays pending for them instead.
//! Relation predicates are exempt from the deleted-field check: a staged
//! `inferred/<p>` relation is a suggestion that never had a real counterpart
//! (scalar provenance is all the overlay records today).
//!
//! **It never lets one class overrule another.** A `Create`/`Update` op
//! names the item's class and that class decides. An item the ops do not
//! name a class for (an `AddLinks` source) is classified by its links, and
//! that answer is a set: the item is accepted only if every class in the set
//! allows it.
//!
//! Ordering: items whose class declares no parent relations are accepted
//! first — a parent-class item is real before any children that link to it
//! are materialized. Within each group, the op/touched-base order is kept,
//! so the whole pass is deterministic.

use super::classes::INTERP_OVERLAY_CLASS;
use super::{accept_interpretation, is_overlay_link, INFERRED_PREFIX, OVERLAY_KIND_PRED};
use crate::agent::AgentContext;
use crate::perspectives::interpretation::{replace_link, InterpretationOp};
use crate::perspectives::model_query::types::{AutoAccept, ModelShape};
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use std::collections::HashMap;
use std::sync::Arc;

/// Marks a base as auto-accepted: `<base> ad4m://interp/auto_accepted
/// literal:string:<run_id>`, written once per accepted pass (`replace_link`
/// keeps it single-valued). Shared, signed by the pass's `AgentContext`.
///
/// Deliberately **not** under `inferred/` (`ad4m://interp/inferred/`): that
/// prefix is the overlay's suggestion vocabulary, and every reader that
/// treats `ad4m://interp/*` as "still pending" (`is_overlay_link`,
/// `overlay_links_query`, `prune_overlay_shell_if_empty`) matches on kind /
/// run / `inferred/` — none of which this predicate touches. The overlay on
/// the base is already gone when the marker lands.
const AUTO_ACCEPTED_PRED: &str = "ad4m://interp/auto_accepted";

/// The class each op names, keyed by base. `Create`/`Update` carry `class`;
/// `AddLinks` does not — their sources are earlier-pass or existing items
/// whose class is resolved structurally inside [`auto_accept_pass`].
///
/// Like `flow_spawn::created_bases_of`, this is captured in
/// `apply_ops_and_run_flow_passes` before `ops` moves into the apply.
pub(crate) fn op_classes_of(ops: &[InterpretationOp]) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for op in ops {
        let (base, class) = match op {
            InterpretationOp::Create { base, class, .. }
            | InterpretationOp::Update { base, class, .. } => (base, class),
            InterpretationOp::AddLinks { .. } => continue,
        };
        map.entry(base.clone()).or_insert_with(|| class.clone());
    }
    map
}

/// The class's parent-relation predicates: every declared forward `hasOne`
/// relation — the generic, shape-derived definition of "has a parent slot".
fn parent_predicates(shape: &ModelShape) -> Vec<&str> {
    shape
        .include_relations
        .iter()
        .filter(|r| r.direction == "forward" && r.kind == "hasOne")
        .map(|r| r.predicate.as_str())
        .collect()
}

/// Whether at least one parent relation points at another item — as an
/// accepted real link **or** a still-staged `inferred/<p>` overlay value
/// (from this pass's own writes or an earlier pass that left the item
/// pending). `links` are the base's own outgoing links.
///
/// A parent link from an item to itself does not count: it places the item
/// under nothing.
fn is_placed(links: &[DecoratedLinkExpression], parent_predicates: &[&str]) -> bool {
    parent_predicates.iter().any(|p| {
        let inferred = format!("{INFERRED_PREFIX}{p}");
        links.iter().any(|l| {
            let parent = match l.data.predicate.as_deref() {
                Some(q) if q == *p => serde_json::Value::String(l.data.target.clone()),
                Some(q) if q == inferred => parse_literal_value(&l.data.target),
                _ => return false,
            };
            parent != serde_json::Value::String(l.data.source.clone())
        })
    })
}

/// The class's declared rule. An absent declaration is `never`.
fn rule_of(shape: &ModelShape) -> AutoAccept {
    shape.auto_accept.unwrap_or(AutoAccept::Never)
}

/// Whether this class's rule allows accepting the item that carries `links`.
fn rule_allows(shape: &ModelShape, links: &[DecoratedLinkExpression]) -> bool {
    match rule_of(shape) {
        AutoAccept::Always => true,
        AutoAccept::WhenPlaced => is_placed(links, &parent_predicates(shape)),
        AutoAccept::Never => false,
    }
}

/// Whether accepting the overlay on this base would change real state: some
/// staged `inferred/<p>` differs from a real `<p>` link the base carries —
/// or the real `<p>` is gone entirely, which is the state a person leaves by
/// deleting a field of a pending item (the accept would write the value back
/// over their deletion). The first half is the state the §4 gate leaves when
/// it declines to overwrite a value the LLM does not own, and the state a
/// person creates by editing a pending item. Values are compared decoded,
/// the way the gate compares them.
///
/// Predicates the item's classes declare as relations are exempt: a staged
/// `inferred/<p>` relation is a proposed edge, not a deleted value.
fn accept_would_change_a_real_value(
    links: &[DecoratedLinkExpression],
    shapes: &[Arc<ModelShape>],
) -> bool {
    let relation_preds: std::collections::HashSet<&str> = shapes
        .iter()
        .flat_map(|s| s.include_relations.iter().map(|r| r.predicate.as_str()))
        .collect();
    links.iter().any(|staged| {
        let Some(real_pred) = staged
            .data
            .predicate
            .as_deref()
            .and_then(|p| p.strip_prefix(INFERRED_PREFIX))
        else {
            return false;
        };
        if relation_preds.contains(real_pred) {
            return false;
        }
        let staged_value = parse_literal_value(&staged.data.target);
        let reals: Vec<serde_json::Value> = links
            .iter()
            .filter(|l| l.data.predicate.as_deref() == Some(real_pred))
            .map(|l| parse_literal_value(&l.data.target))
            .collect();
        reals.is_empty() || reals.iter().any(|v| *v != staged_value)
    })
}

/// Write the auto-accepted marker on `base`. The marker is this function and
/// its single call site, and nothing reads it: removing both removes it.
async fn mark_auto_accepted(
    perspective: &mut PerspectiveInstance,
    base: &str,
    run_id: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let target = ad4m_client::literal::Literal::from_string(run_id.to_string())
        .to_url()
        .map_err(|e| anyhow::anyhow!("auto-accept marker encode: {e:#}"))?;
    replace_link(
        perspective,
        base,
        AUTO_ACCEPTED_PRED,
        &target,
        None,
        context,
    )
    .await
}

/// Auto-accept the eligible inferred items among `bases` (the pass's touched
/// bases), right after `apply_with_overlay` and before flow spawning, so a
/// flow's `requires` reads accepted values.
///
/// An item is a candidate only while it still carries an overlay (a `kind`
/// link): bases without one — human-owned or already accepted — are
/// skipped, so `always` never re-accepts. `when_placed` re-evaluates against
/// the base's current links, so an earlier-pass item that gains a parent in
/// this pass (it shows up here as an `AddLinks` source) is accepted now.
///
/// Two guards keep an otherwise eligible candidate pending: the overlay must
/// be *this agent's own* — in a shared perspective an earlier pass of
/// another agent may have staged it, and accepting would re-sign that
/// agent's values as this pass's — and accepting must not change real state
/// (see the module docs).
///
/// Per-item failures are logged at warn and never abort the pass. Returns
/// the bases that were actually accepted, in accept order.
pub(crate) async fn auto_accept_pass(
    perspective: &mut PerspectiveInstance,
    bases: &[String],
    op_classes: &HashMap<String, String>,
    run_id: &str,
    context: &AgentContext,
) -> Vec<String> {
    if bases.is_empty() {
        return Vec::new();
    }

    // The DID this pass signs with — the only author whose overlays this pass
    // may resolve.
    let pass_did = match crate::agent::did_for_context(context) {
        Ok(d) => d,
        Err(e) => {
            log::warn!("interpretation: auto-accept could not resolve the pass's DID: {e:#}");
            return Vec::new();
        }
    };

    // Bases the ops do not name a class for (AddLinks sources only) are
    // classified by their links — the same definition of "what is this node"
    // the rest of the executor uses.
    let uncovered: Vec<String> = bases
        .iter()
        .filter(|b| !op_classes.contains_key(*b))
        .cloned()
        .collect();
    let resolved = match perspective.subject_classes_of(&uncovered) {
        Ok(m) => m,
        Err(e) => {
            log::warn!("interpretation: auto-accept class resolution failed: {e:#}");
            HashMap::new()
        }
    };

    // Root-class items first (their classes declare no parent relation), then
    // nested ones — so a parent is real before its children are materialized.
    let mut order: Vec<(String, Vec<Arc<ModelShape>>)> = Vec::new();
    let mut nested: Vec<(String, Vec<Arc<ModelShape>>)> = Vec::new();
    for base in bases {
        // The classes that decide for this base. An op names exactly one. A
        // structural classification is a set, and every pending base is in
        // it as an overlay: that class is the executor's own bookkeeping and
        // says nothing about the item, so it is dropped. Every class left
        // must allow the accept.
        let classes: Vec<&String> = match op_classes.get(base) {
            Some(class) => vec![class],
            None => resolved
                .get(base)
                .map(|all| all.iter().filter(|c| *c != INTERP_OVERLAY_CLASS).collect())
                .unwrap_or_default(),
        };
        let mut shapes = Vec::with_capacity(classes.len());
        for class in classes {
            match perspective.get_shape(class) {
                Ok(s) => shapes.push(s),
                Err(e) => {
                    // A class whose rule cannot be read cannot allow anything.
                    log::warn!(
                        "interpretation: auto-accept could not load shape for class \
                         '{class}' (base {base}): {e:#}"
                    );
                    shapes.clear();
                    break;
                }
            }
        }
        if shapes.is_empty() {
            continue;
        }
        if shapes.iter().all(|s| parent_predicates(s).is_empty()) {
            order.push((base.clone(), shapes));
        } else {
            nested.push((base.clone(), shapes));
        }
    }
    order.extend(nested);

    let mut accepted = Vec::new();
    for (base, shapes) in order {
        if shapes.iter().any(|s| rule_of(s) == AutoAccept::Never) {
            continue;
        }

        let links = match perspective
            .get_links(&LinkQuery {
                source: Some(base.clone()),
                ..Default::default()
            })
            .await
        {
            Ok(l) => l,
            Err(e) => {
                log::warn!("interpretation: auto-accept could not read {base}: {e:#}");
                continue;
            }
        };
        let has_overlay = links
            .iter()
            .any(|l| l.data.predicate.as_deref() == Some(OVERLAY_KIND_PRED));
        if !has_overlay {
            continue;
        }
        // The overlay must be this pass's agent's own: every overlay link on
        // the base — `kind`, `run`, and every `inferred/<p>` — signed by it.
        // A base carrying another agent's overlay stays pending for that
        // agent (or a person) to resolve.
        if links
            .iter()
            .any(|l| is_overlay_link(l) && l.author != pass_did)
        {
            log::debug!(
                "interpretation: {base} stays pending: its overlay was staged by another agent"
            );
            continue;
        }
        if !shapes.iter().all(|s| rule_allows(s, &links)) {
            continue;
        }
        if accept_would_change_a_real_value(&links, &shapes) {
            log::debug!(
                "interpretation: {base} stays pending: accepting it would change a real value"
            );
            continue;
        }

        match accept_interpretation(perspective, &base, None, context).await {
            Ok(()) => {
                if let Err(e) = mark_auto_accepted(perspective, &base, run_id, context).await {
                    log::warn!("interpretation: auto-accept marker on {base} failed: {e:#}");
                }
                accepted.push(base);
            }
            Err(e) => {
                log::warn!("interpretation: auto-accept of {base} failed: {e:#}");
            }
        }
    }
    accepted
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::overlay::{
        apply_with_overlay, list_overlays, seed_overlay,
    };
    use crate::perspectives::interpretation::types::InterpretationOp;
    use crate::perspectives::interpretation_test_support::*;
    use crate::types::{AITask, Link, LinkStatus};
    use serde_json::json;

    /// The whole two-class fixture set: parent (`always`), child
    /// (`when_placed` + `partOf` hasOne), `never`, and undeclared.
    const AA_CLASSES: &[(&str, &str)] = &[
        ("Chapter", AA_PARENT_SDNA),
        ("Note", AA_CHILD_SDNA),
        ("Draft", AA_NEVER_SDNA),
        ("Plain", AA_PLAIN_SDNA),
    ];

    fn task() -> AITask {
        AITask {
            model_id: "gemma3:12b".to_string(),
            system_prompt: "You extract typed instances.".to_string(),
            prompt_examples: Vec::new(),
            ..Default::default()
        }
    }

    /// Drive one pass tail exactly the way `apply_ops_and_run_flow_passes`
    /// does — overlay-apply the ops, then auto-accept over the touched
    /// bases — without the flow passes (they are covered by run.rs's own
    /// tail tests).
    async fn pass(
        p: &mut PerspectiveInstance,
        shapes: &[ModelShape],
        ops: Vec<InterpretationOp>,
        run_id: &str,
        ctx: &AgentContext,
    ) -> Vec<String> {
        let op_classes = op_classes_of(&ops);
        let bases = apply_with_overlay(
            p,
            shapes,
            ops,
            &task(),
            run_id.to_string(),
            "1700000000000".to_string(),
            ctx,
            None,
            None,
        )
        .await
        .expect("apply_with_overlay");
        auto_accept_pass(p, &bases, &op_classes, run_id, ctx).await
    }

    fn create_op(base: &str, class: &str, title: &str) -> InterpretationOp {
        InterpretationOp::Create {
            base: base.to_string(),
            class: class.to_string(),
            values: serde_json::Map::from_iter([("title".to_string(), json!(title))]),
        }
    }

    fn addlinks_op(source: &str, predicate: &str, target: &str) -> InterpretationOp {
        InterpretationOp::AddLinks {
            source: source.to_string(),
            links: vec![Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }],
        }
    }

    /// The bases still waiting on a human, sorted for comparison.
    async fn pending(p: &PerspectiveInstance) -> Vec<String> {
        let mut v: Vec<String> = list_overlays(p)
            .await
            .expect("list_overlays")
            .into_iter()
            .map(|o| o.base)
            .collect();
        v.sort();
        v
    }

    /// `(author, decoded target)` of the `ad4m://interp/auto_accepted`
    /// marker on `base`, or `None`.
    async fn marker_on(p: &PerspectiveInstance, base: &str) -> Option<(String, String)> {
        let links = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some(AUTO_ACCEPTED_PRED.to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links(marker)");
        links.into_iter().next().map(|l| {
            (
                l.author,
                crate::perspectives::model_query::utils::parse_literal_value(&l.data.target)
                    .as_str()
                    .unwrap_or_default()
                    .to_string(),
            )
        })
    }

    /// The real `title` of `base`, read the way app code reads it.
    async fn title_of(p: &PerspectiveInstance, class: &str, base: &str) -> Option<String> {
        model_instances(p, class, &["title"])
            .await
            .iter()
            .find(|r| r["id"] == json!(base))
            .and_then(|r| r["title"].as_str().map(str::to_string))
    }

    /// An `always` class: the Create lands its real value *and* the overlay
    /// is gone by the end of the same pass — the item never waits on a
    /// human. The marker records the run that accepted it, signed by the
    /// pass's own context (the same signer `apply_with_overlay` used).
    #[tokio::test]
    async fn always_accepts_in_the_same_pass() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let base = "soa://ext/chapter/a";
        let run_id = "run-aa-1";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![create_op(base, "Chapter", "Chapter A")],
            run_id,
            &ctx,
        )
        .await;

        assert_eq!(accepted, vec![base.to_string()]);
        assert!(
            pending(&p).await.is_empty(),
            "nothing left pending for a human"
        );
        // The marker carries this pass's run id and was signed by the
        // pass's context — not by some other agent.
        let (author, value) = marker_on(&p, base).await.expect("marker present");
        assert_eq!(value, run_id);
        assert_eq!(author, crate::agent::did_for_context(&ctx).unwrap());
        // The real instance value is intact and readable.
        let rows = model_instances(&p, "Chapter", &["title"]).await;
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0]["title"], json!("Chapter A"));
    }

    /// `never` and an undeclared class are indistinguishable: both keep
    /// their overlay pending — the pre-feature behaviour.
    #[tokio::test]
    async fn never_and_undeclared_classes_stay_pending() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let draft = "soa://ext/draft/1";
        let plain = "soa://ext/plain/1";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(draft, "Draft", "needs review"),
                create_op(plain, "Plain", "no rule declared"),
            ],
            "run-never",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty(), "nothing may be auto-accepted");
        assert_eq!(
            pending(&p).await,
            vec![draft.to_string(), plain.to_string()]
        );
        assert!(marker_on(&p, draft).await.is_none());
        assert!(marker_on(&p, plain).await.is_none());
    }

    /// `when_placed` without a parent value stays pending; when a *later*
    /// pass writes the parent link, the item is in that pass's touched
    /// bases and gets accepted then — the case the spec calls out.
    #[tokio::test]
    async fn when_placed_accepts_in_a_later_pass_once_a_parent_lands() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/late";
        let parent = "soa://ext/chapter/late";

        // Pass 1: the child arrives unplaced → stays pending.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                create_op(child, "Note", "Orphan note"),
            ],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![parent.to_string()]);
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        // Pass 2: the parent link lands (AddLinks touches the child as its
        // source; the class resolves through subject_classes_of since the
        // op names no class) → placed now → accepted.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![child.to_string()]);
        assert!(pending(&p).await.is_empty());
        assert_eq!(marker_on(&p, child).await.unwrap().1, "run-2");
    }

    /// Everything placed in the same pass: the child is accepted — and the
    /// parent-class item is accepted *first*, so a parent is real before
    /// its children are materialized.
    #[tokio::test]
    async fn when_placed_accepts_within_one_pass_roots_before_children() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/inner";
        let parent = "soa://ext/chapter/inner";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(child, "Note", "Placed note"),
                create_op(parent, "Chapter", "Chapter"),
                addlinks_op(child, "ns://partOf", parent),
            ],
            "run-roots",
            &ctx,
        )
        .await;

        assert_eq!(
            accepted,
            vec![parent.to_string(), child.to_string()],
            "root-class items accept before items that declare parent relations"
        );
        assert!(pending(&p).await.is_empty());
    }

    /// Placement also counts a *staged* `inferred/<p>` value for the parent
    /// relation — an earlier pass's suggestion a human has not accepted yet.
    /// (Today relations never travel through `inferred/`, but the rule reads
    /// both, so a future overlay that snapshots relation values keeps the
    /// same semantics.)
    #[tokio::test]
    async fn when_placed_counts_a_staged_inferred_parent_value() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/inferred-parent";
        // The staged scalar needs its real counterpart, or the base reads as
        // "a person deleted title since the snapshot" and the pass leaves it
        // pending (a staged value with no real link blocks the accept now).
        p.add_link(
            Link {
                source: child.into(),
                predicate: Some("ns://title".into()),
                target: "literal:string:Staged note".into(),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link");
        seed_overlay(
            &mut p,
            child,
            std::collections::BTreeMap::from_iter([
                ("ns://title".to_string(), json!("Staged note")),
                // A staged parent-relation value, as if an earlier pass
                // proposed `partOf` without writing the real link.
                ("ns://partOf".to_string(), json!("soa://ext/chapter/p")),
            ]),
            &ctx,
        )
        .await
        .expect("seed_overlay");

        let accepted = auto_accept_pass(
            &mut p,
            &[child.to_string()],
            &HashMap::from_iter([(child.to_string(), "Note".to_string())]),
            "run-inferred",
            &ctx,
        )
        .await;

        assert_eq!(accepted, vec![child.to_string()]);
        assert!(pending(&p).await.is_empty());
    }

    /// A `when_placed` item whose parent predicate carries nothing stays
    /// pending — the guard is on the *value*, not on the class merely
    /// declaring a parent slot.
    #[tokio::test]
    async fn when_placed_without_a_value_stays_pending() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/orphan";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![create_op(child, "Note", "No parent yet")],
            "run-orphan",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty());
        assert_eq!(pending(&p).await, vec![child.to_string()]);
        assert!(marker_on(&p, child).await.is_none());
    }

    /// Auto-accept is idempotent: a second pass over the same bases finds
    /// no overlay left and accepts nothing — no error, no second marker.
    #[tokio::test]
    async fn second_pass_over_accepted_bases_is_a_no_op() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let base = "soa://ext/chapter/twice";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![create_op(base, "Chapter", "Twice")],
            "run-first",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![base.to_string()]);

        let again = auto_accept_pass(
            &mut p,
            &[base.to_string()],
            &HashMap::from_iter([(base.to_string(), "Chapter".to_string())]),
            "run-second",
            &ctx,
        )
        .await;
        assert!(again.is_empty(), "no overlay left → nothing re-accepted");
        // The marker still names the run that actually accepted it.
        assert_eq!(marker_on(&p, base).await.unwrap().1, "run-first");
    }

    /// A human-seeded instance that carries no overlay is not a candidate:
    /// an `always` class's untouched-by-the-LLM item is never marked —
    /// auto-accept only resolves *inferred* items.
    #[tokio::test]
    async fn bases_without_an_overlay_are_skipped() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let human = "soa://ext/chapter/human";
        seed_instance(&mut p, &ctx, &shapes[0], human, "human-owned").await;

        // The base enters `touched_bases` through an AddLinks edge only —
        // no overlay is minted for it → no candidate → no marker.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(human, "ns://partOf", "soa://ext/chapter/other")],
            "run-human",
            &ctx,
        )
        .await;
        assert!(accepted.is_empty());
        assert!(marker_on(&p, human).await.is_none());
        // And the human's real links are untouched (the AddLinks edge
        // itself landed — that part was the pass's own write).
        let links = p
            .get_links(&LinkQuery {
                source: Some(human.to_string()),
                predicate: Some("ns://partOf".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(links.len(), 1);
    }

    /// The marker is not overlay data: after an auto-accept the base
    /// disappears from `list_overlays` even though `ad4m://interp/*` links
    /// remain on it — nothing treats the marker as "still pending", and
    /// `prune_overlay_shell_if_empty` already ran before it landed.
    #[tokio::test]
    async fn the_marker_does_not_read_as_pending_overlay_state() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let base = "soa://ext/chapter/marker";

        pass(
            &mut p,
            &shapes,
            vec![create_op(base, "Chapter", "Marked")],
            "run-marker",
            &ctx,
        )
        .await;

        assert!(list_overlays(&p).await.unwrap().is_empty());
        assert!(marker_on(&p, base).await.is_some());
        // No overlay vocabulary left on the base: no kind/run/inferred —
        // only real data links plus the one marker.
        let remaining: Vec<String> = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                ..Default::default()
            })
            .await
            .unwrap()
            .into_iter()
            .filter_map(|l| l.data.predicate)
            .filter(|pred| pred.starts_with("ad4m://interp/"))
            .collect();
        assert_eq!(remaining, vec![AUTO_ACCEPTED_PRED.to_string()]);
    }

    /// A value a person wrote is not the LLM's to overwrite: the §4 gate
    /// stages the proposal instead of writing it, and auto-accept must not
    /// then write it anyway. The item stays pending with the person's value
    /// intact, whatever the class declares.
    #[tokio::test]
    async fn a_suggestion_that_would_change_a_real_value_stays_pending() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let base = "soa://ext/chapter/edited";
        seed_instance(&mut p, &ctx, &shapes[0], base, "human title").await;

        let accepted = pass(
            &mut p,
            &shapes,
            vec![InterpretationOp::Update {
                base: base.to_string(),
                class: "Chapter".to_string(),
                values: serde_json::Map::from_iter([("title".to_string(), json!("llm title"))]),
            }],
            "run-edit",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty(), "a person decides on this suggestion");
        assert_eq!(
            title_of(&p, "Chapter", base).await.as_deref(),
            Some("human title")
        );
        assert_eq!(pending(&p).await, vec![base.to_string()]);
        assert!(marker_on(&p, base).await.is_none());
    }

    /// A person edits an item while it is still pending; a later pass then
    /// places it. Accepting would put the LLM's older value back over the
    /// edit, so the item stays pending and the edit survives.
    #[tokio::test]
    async fn an_edit_on_a_pending_item_survives_later_placement() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/edited";
        let parent = "soa://ext/chapter/edited-parent";

        pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                create_op(child, "Note", "llm title"),
            ],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        // The person's edit: the real value changes, the overlay does not.
        let edited = ad4m_client::literal::Literal::from_string("human title".to_string())
            .to_url()
            .unwrap();
        replace_link(&mut p, child, "ns://title", &edited, None, &ctx)
            .await
            .expect("human edit");

        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty(), "placed, but no longer untouched");
        assert_eq!(
            title_of(&p, "Note", child).await.as_deref(),
            Some("human title")
        );
        assert_eq!(pending(&p).await, vec![child.to_string()]);
    }

    /// An item the LLM wrote and nobody touched is still accepted when a
    /// later pass refines its value: the gate lets the LLM overwrite its own
    /// inference, so real and staged values agree again by accept time.
    #[tokio::test]
    async fn an_untouched_item_is_accepted_after_the_llm_refines_it() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/refined";
        let parent = "soa://ext/chapter/refined-parent";

        pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                create_op(child, "Note", "first wording"),
            ],
            "run-1",
            &ctx,
        )
        .await;

        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                InterpretationOp::Update {
                    base: child.to_string(),
                    class: "Note".to_string(),
                    values: serde_json::Map::from_iter([(
                        "title".to_string(),
                        json!("second wording"),
                    )]),
                },
                addlinks_op(child, "ns://partOf", parent),
            ],
            "run-2",
            &ctx,
        )
        .await;

        assert_eq!(accepted, vec![child.to_string()]);
        assert_eq!(
            title_of(&p, "Note", child).await.as_deref(),
            Some("second wording")
        );
        assert!(pending(&p).await.is_empty());
    }

    /// Every link on `base` matching `predicate`, in `get_links` order.
    async fn links_of(
        p: &PerspectiveInstance,
        base: &str,
        predicate: &str,
    ) -> Vec<DecoratedLinkExpression> {
        p.get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
    }

    /// In a shared perspective a *pending* item's overlay records who staged
    /// it: another agent's pass left this item pending, and a later pass of
    /// ours placing it must not accept it — accepting would re-sign that
    /// agent's values as ours. The item stays pending, every overlay link
    /// still authored by its own agent.
    #[tokio::test]
    async fn a_foreign_agents_overlay_is_left_pending() {
        let (mut p, shapes, ctx_ours) = setup_perspective_no_llm(AA_CLASSES).await;
        let did_other = crate::test_utils::create_test_user("bob-foreign-overlay@test");
        let ctx_other = AgentContext::managed(did_other.clone());
        let child = "soa://ext/note/foreign";
        let parent = "soa://ext/chapter/foreign-parent";

        // The other agent's pass stages the item; unplaced, it stays pending.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![create_op(child, "Note", "their note")],
            "run-theirs",
            &ctx_other,
        )
        .await;
        assert!(accepted.is_empty());
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        // Our pass adds the parent link: the item is placed now, but its
        // overlay is not ours to resolve.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-ours",
            &ctx_ours,
        )
        .await;

        assert!(
            accepted.is_empty(),
            "another agent's overlay is not ours to accept"
        );
        assert_eq!(pending(&p).await, vec![child.to_string()]);
        assert!(marker_on(&p, child).await.is_none(), "no accept marker");

        let did_ours = crate::agent::did_for_context(&ctx_ours).unwrap();
        let links = p
            .get_links(&LinkQuery {
                source: Some(child.to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        for l in links.iter().filter(|l| is_overlay_link(l)) {
            assert_eq!(
                l.author, did_other,
                "overlay link {:?} was re-signed",
                l.data.predicate
            );
        }
        // The only link of ours on the base is the parent edge this pass's
        // own op wrote — nothing the accept would have produced.
        assert!(
            links
                .iter()
                .filter(|l| l.author == did_ours)
                .all(|l| l.data.predicate.as_deref() == Some("ns://partOf")),
            "our pass signed only its own AddLinks edge"
        );
    }

    /// The same placed-later sequence on an overlay *this* agent staged is
    /// accepted, and the marker lands signed by the pass's own context.
    /// Companion to `a_foreign_agents_overlay_is_left_pending`.
    #[tokio::test]
    async fn an_overlay_our_agent_staged_is_accepted() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/ours";
        let parent = "soa://ext/chapter/ours-parent";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![create_op(child, "Note", "our note")],
            "run-1",
            &ctx,
        )
        .await;
        assert!(accepted.is_empty());
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![child.to_string()]);
        assert!(pending(&p).await.is_empty());
        let (author, run) = marker_on(&p, child).await.expect("marker");
        assert_eq!(run, "run-2");
        assert_eq!(author, crate::agent::did_for_context(&ctx).unwrap());
    }

    /// A person trims an optional field off a pending `when_placed` item:
    /// the real `note` link goes, its `inferred/<p>` snapshot stays behind.
    /// When a later pass then places the item, accepting would write that
    /// value back over the deletion — so the item must stay pending and the
    /// field must not come back.
    #[tokio::test]
    async fn a_field_a_person_removed_is_not_restored_when_the_item_is_placed() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[
            ("Chapter", AA_PARENT_SDNA),
            ("Memo", AA_CHILD_OPTIONAL_SDNA),
        ])
        .await;
        let child = "soa://ext/memo/trimmed";
        let parent = "soa://ext/chapter/trimmed-parent";

        // Pass 1: the item lands with `note` set, unplaced → pending.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                InterpretationOp::Create {
                    base: child.to_string(),
                    class: "Memo".to_string(),
                    values: serde_json::Map::from_iter([
                        ("title".to_string(), json!("the memo")),
                        ("note".to_string(), json!("a field the person trims")),
                    ]),
                },
            ],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![parent.to_string()]);
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        // The person deletes `note` from the pending item — the generic
        // remove path drops the real link; the suggestion stays staged.
        let note = links_of(&p, child, "ns://note").await;
        assert_eq!(note.len(), 1);
        p.remove_links(note.into_iter().map(Into::into).collect(), None)
            .await
            .expect("person removes the field");

        // Pass 2: only the parent link lands — nothing re-stages `note`.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;

        assert!(
            accepted.is_empty(),
            "a person trimmed it — a person resolves it"
        );
        assert_eq!(pending(&p).await, vec![child.to_string()]);
        assert!(marker_on(&p, child).await.is_none());
        assert!(
            links_of(&p, child, "ns://note").await.is_empty(),
            "the removed value must not come back as a real link"
        );
        let staged = links_of(&p, child, "ad4m://interp/inferred/ns://note").await;
        assert_eq!(
            staged.len(),
            1,
            "the suggestion stays staged for the person to decide"
        );
        assert_eq!(
            title_of(&p, "Memo", child).await.as_deref(),
            Some("the memo")
        );
    }

    /// The same deletion, then a pass that *re-emits* the removed field as an
    /// Update (what the LLM path does when it proposes the value again): the
    /// value must not be written back as a real link either — the snapshot
    /// records it was deleted, so the proposal stays a staged suggestion.
    #[tokio::test]
    async fn a_removed_field_comes_back_only_as_a_suggestion() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[
            ("Chapter", AA_PARENT_SDNA),
            ("Memo", AA_CHILD_OPTIONAL_SDNA),
        ])
        .await;
        let child = "soa://ext/memo/reemitted";
        let parent = "soa://ext/chapter/reemitted-parent";

        pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                InterpretationOp::Create {
                    base: child.to_string(),
                    class: "Memo".to_string(),
                    values: serde_json::Map::from_iter([
                        ("title".to_string(), json!("the memo")),
                        ("note".to_string(), json!("first wording")),
                    ]),
                },
            ],
            "run-1",
            &ctx,
        )
        .await;
        let note = links_of(&p, child, "ns://note").await;
        p.remove_links(note.into_iter().map(Into::into).collect(), None)
            .await
            .expect("person removes the field");

        // Pass 2: the field is proposed again and the parent link lands.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                InterpretationOp::Update {
                    base: child.to_string(),
                    class: "Memo".to_string(),
                    values: serde_json::Map::from_iter([(
                        "note".to_string(),
                        json!("second wording"),
                    )]),
                },
                addlinks_op(child, "ns://partOf", parent),
            ],
            "run-2",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty());
        assert_eq!(pending(&p).await, vec![child.to_string()]);
        assert!(
            links_of(&p, child, "ns://note").await.is_empty(),
            "the re-emitted value stays a suggestion, not a real write"
        );
        // The staged suggestion updates to the new wording for the person.
        let staged = links_of(&p, child, "ad4m://interp/inferred/ns://note").await;
        assert_eq!(staged.len(), 1);
        assert_eq!(
            parse_literal_value(&staged[0].data.target),
            json!("second wording")
        );
    }

    /// A parent link from an item to itself places it under nothing, so a
    /// `when_placed` item carrying only that stays pending.
    #[tokio::test]
    async fn a_parent_link_to_itself_does_not_place_an_item() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(AA_CLASSES).await;
        let child = "soa://ext/note/self";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(child, "Note", "Points at itself"),
                addlinks_op(child, "ns://partOf", child),
            ],
            "run-self",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty());
        assert_eq!(pending(&p).await, vec![child.to_string()]);
    }

    fn remark_op(base: &str, class: &str, remark: &str) -> InterpretationOp {
        InterpretationOp::Create {
            base: base.to_string(),
            class: class.to_string(),
            values: serde_json::Map::from_iter([("remark".to_string(), json!(remark))]),
        }
    }

    /// Every pending item also conforms to the executor's overlay class (it
    /// carries the overlay's `kind` link). When a later pass places an item
    /// through an `AddLinks` op, its class is read from its links, and the
    /// overlay class must not be taken for the item's class: it declares no
    /// rule, so the item would never be accepted. The class here has one
    /// required triple, like the overlay class, and sorts after it by name.
    #[tokio::test]
    async fn a_later_pass_reads_the_item_class_not_the_overlay_class() {
        let (mut p, shapes, ctx) =
            setup_perspective_no_llm(&[("Chapter", AA_PARENT_SDNA), ("Remark", AA_UNFLAGGED_SDNA)])
                .await;
        let child = "soa://ext/remark/late";
        let parent = "soa://ext/chapter/remark-parent";

        let accepted = pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                remark_op(child, "Remark", "Unplaced remark"),
            ],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![parent.to_string()]);

        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;
        assert_eq!(accepted, vec![child.to_string()]);
        assert!(pending(&p).await.is_empty());
    }

    /// An item whose class is read from its links can conform to several
    /// classes. If one of them says `never`, a person decides — a class that
    /// allows the accept does not overrule one that does not.
    #[tokio::test]
    async fn an_item_of_two_classes_needs_both_rules_to_allow_it() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[
            ("Chapter", AA_PARENT_SDNA),
            ("Remark", AA_UNFLAGGED_SDNA),
            ("Sealed", AA_UNFLAGGED_NEVER_SDNA),
        ])
        .await;
        let child = "soa://ext/remark/two-classes";
        let parent = "soa://ext/chapter/two-classes-parent";

        pass(
            &mut p,
            &shapes,
            vec![
                create_op(parent, "Chapter", "Chapter"),
                remark_op(child, "Remark", "Also a Sealed"),
            ],
            "run-1",
            &ctx,
        )
        .await;
        let classes = p.subject_classes_of(&[child.to_string()]).unwrap();
        assert!(
            ["Remark", "Sealed"]
                .iter()
                .all(|c| classes[child].iter().any(|have| have == c)),
            "fixture: the item conforms to both classes, got {:?}",
            classes[child]
        );

        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://partOf", parent)],
            "run-2",
            &ctx,
        )
        .await;

        assert!(accepted.is_empty(), "one of its classes says never");
        assert_eq!(pending(&p).await, vec![child.to_string()]);
    }

    /// The same with the roles turned round: one class says `always`, the
    /// other `when_placed` and the item is not placed. The item waits.
    #[tokio::test]
    async fn an_always_class_does_not_overrule_a_class_that_waits() {
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[
            ("Chapter", AA_PARENT_SDNA),
            ("Remark", AA_UNFLAGGED_SDNA),
            ("Open", AA_UNFLAGGED_ALWAYS_SDNA),
        ])
        .await;
        let child = "soa://ext/remark/open-and-waiting";
        let other = "soa://ext/chapter/not-a-parent";

        pass(
            &mut p,
            &shapes,
            vec![
                create_op(other, "Chapter", "Chapter"),
                remark_op(child, "Remark", "Also an Open"),
            ],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(pending(&p).await, vec![child.to_string()]);

        // A link that is no parent relation of either class: the item is
        // touched, and still unplaced.
        let accepted = pass(
            &mut p,
            &shapes,
            vec![addlinks_op(child, "ns://seeAlso", other)],
            "run-2",
            &ctx,
        )
        .await;

        assert!(
            accepted.is_empty(),
            "its when_placed class is not satisfied"
        );
        assert_eq!(pending(&p).await, vec![child.to_string()]);
    }

    /// Direct unit check on the eligibility helpers: `is_placed` sees both
    /// a real parent link and a staged `inferred/<p>` one — but not a
    /// hasMany link, an unrelated predicate, or a bare overlay.
    #[test]
    fn is_placed_reads_real_and_staged_parent_values() {
        let shape = shape_from_sdna("Note", AA_CHILD_SDNA);
        let preds = parent_predicates(&shape);
        assert_eq!(preds, vec!["ns://partOf"]);

        let mk = |pred: &str| DecoratedLinkExpression {
            author: "did:test".into(),
            timestamp: "t".into(),
            data: Link {
                source: "soa://x".into(),
                predicate: Some(pred.into()),
                target: "soa://y".into(),
            },
            proof: Default::default(),
            status: Some(LinkStatus::Shared),
        };
        assert!(is_placed(&[mk("ns://partOf")], &preds), "real value");
        assert!(
            is_placed(&[mk("ad4m://interp/inferred/ns://partOf")], &preds),
            "staged inferred value"
        );
        assert!(!is_placed(&[mk("ns://other")], &preds), "unrelated");
        assert!(!is_placed(&[], &preds), "no links at all");
        // The overlay marker itself is not a parent value.
        assert!(!is_placed(&[mk(AUTO_ACCEPTED_PRED)], &preds));
        // A parent link back to the item itself is not a placement, real or
        // staged.
        let mut own = mk("ns://partOf");
        own.data.target = own.data.source.clone();
        assert!(!is_placed(&[own], &preds), "real self link");
        let mut own_staged = mk("ad4m://interp/inferred/ns://partOf");
        own_staged.data.target = "literal:string:soa%3A%2F%2Fx".into();
        assert!(!is_placed(&[own_staged], &preds), "staged self link");
    }
}
