//! Automatic accept of eligible inferred items at the end of a pass.
//!
//! The class-level `auto_accept` rule (`ad4m://auto_accept` on the SHACL
//! shape, surfaced as `ModelShape::auto_accept`) lets a DNA declare that
//! inferred instances of a class need no human click on
//! `accept_interpretation`:
//!
//! * `always` — every inferred item of the class is accepted,
//! * `when_placed` — only items that are *placed*: at least one of the
//!   class's parent relations has a value, either an accepted link or a
//!   still-staged `inferred/<p>` overlay value,
//! * `never` / absent — nothing is auto-accepted; the overlay stays pending.
//!
//! A **parent relation** is any declared forward `hasOne` relation
//! (`ShapeRelation` with `kind == "hasOne" && direction == "forward"`) — the
//! definition is structural, no class- or property-names enter here.
//!
//! Everything routes through the human path's own
//! [`accept_interpretation`]: auto-accept *is* the accept button, pressed by
//! the shape declaration rather than a user. A per-item failure is logged
//! and does not abort the pass; the marker link is kept tiny so removing the
//! feature is one commit.
//!
//! Ordering: items whose class declares no parent relations are accepted
//! first — a parent-class item is real before any children that link to it
//! are materialized. Within each group, the op/touched-base order is kept,
//! so the whole pass is deterministic.

use super::{accept_interpretation, INFERRED_PREFIX, OVERLAY_KIND_PRED};
use crate::agent::AgentContext;
use crate::perspectives::interpretation::{replace_link, InterpretationOp};
use crate::perspectives::model_query::types::{AutoAccept, ModelShape};
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

/// Whether at least one parent relation carries a value on this base — an
/// accepted real link **or** a still-staged `inferred/<p>` overlay value
/// (from this pass's own writes or an earlier pass that left the item
/// pending).
fn is_placed(links: &[DecoratedLinkExpression], parent_predicates: &[&str]) -> bool {
    parent_predicates.iter().any(|p| {
        let inferred = format!("{INFERRED_PREFIX}{p}");
        links.iter().any(|l| {
            l.data
                .predicate
                .as_deref()
                .is_some_and(|q| q == *p || q == inferred)
        })
    })
}

/// Write the auto-accepted marker on `base`. One small function so Nico's
/// call on whether to keep the marker is a one-commit change — delete this
/// fn and its single call site.
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

    // Bases the ops do not name a class for (AddLinks sources only): resolve
    // structurally, most specific class first — the same definition of
    // "what is this node" the rest of the executor uses.
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

    // Root-class items first (their class declares no parent relation), then
    // nested ones — so a parent is real before its children are materialized.
    let mut order: Vec<(String, Arc<ModelShape>)> = Vec::new();
    let mut nested: Vec<(String, Arc<ModelShape>)> = Vec::new();
    for base in bases {
        let class = op_classes
            .get(base)
            .or_else(|| resolved.get(base).and_then(|v| v.first()));
        let Some(class) = class else { continue };
        let shape = match perspective.get_shape(class) {
            Ok(s) => s,
            Err(e) => {
                log::warn!(
                    "interpretation: auto-accept could not load shape for class \
                     '{class}' (base {base}): {e:#}"
                );
                continue;
            }
        };
        let entry = (base.clone(), shape);
        if parent_predicates(&entry.1).is_empty() {
            order.push(entry);
        } else {
            nested.push(entry);
        }
    }
    order.extend(nested);

    let mut accepted = Vec::new();
    for (base, shape) in order {
        let rule = shape.auto_accept.unwrap_or(AutoAccept::Never);
        if rule == AutoAccept::Never {
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
        let eligible = rule == AutoAccept::Always
            || (rule == AutoAccept::WhenPlaced && is_placed(&links, &parent_predicates(&shape)));
        if !eligible {
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

    /// Acceptance promotes staged suggestions to real values — the half of
    /// "accept" that changes graph state, not just overlay bookkeeping.
    /// A human-seeded value + a gated Update proposal + `always` ⇒ the
    /// LLM's value is the real one at the end of the pass (the class
    /// opted in to trusting the interpreter over its instances).
    #[tokio::test]
    async fn accept_materializes_a_gated_suggestion_to_the_real_value() {
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

        assert_eq!(accepted, vec![base.to_string()]);
        let rows = model_instances(&p, "Chapter", &["title"]).await;
        let title = rows
            .iter()
            .find(|r| r["id"] == json!(base))
            .and_then(|r| r["title"].as_str());
        assert_eq!(title, Some("llm title"), "staged value promoted to real");
        assert!(pending(&p).await.is_empty());
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
    }
}
