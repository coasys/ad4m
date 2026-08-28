//! Interpretation provenance overlay (#883).
//!
//! The whole mechanism is **one extra subject class instantiated over the same
//! base URI** as the instance the LLM writes — an `InterpretationOverlay` — plus
//! one `InterpretationRun` per pass. The overlay carries, in one instance:
//!   * `kind` (`create` | `update`) — how the engine minted the node,
//!   * `run` → the [`InterpretationRun`] that wrote it (model / prompt_version /
//!     ran_at — the *who* is already the link author's DID, so no `agent` field),
//!   * `inferred/<realPredicate>` → the LLM's value for each affected property.
//!
//! `inferred/<p>` are **parallel links** (`ad4m://interp/inferred/<realPredicate>`)
//! so they never collide with the real property links and stay RDF-clean /
//! `model_query`-able. They double as (a) the LLM's provenance snapshot and (b)
//! the last-inferred baseline used to detect human divergence.
//!
//! The overlay is written *additively* over the same base: readers that don't
//! know about it ignore it and see the plain instance. There is **exactly one
//! overlay per base**, updated in place across passes — no per-run accumulation.
//!
//! ## The one rule that protects humans (Update routing, §4)
//! The engine overwrites a real value **only when it can prove the LLM owns it**;
//! otherwise it stages the proposal in the overlay and leaves the real value
//! untouched:
//!   * real field unset → the LLM is filling in a fresh value → write it.
//!   * base has an overlay and real == last-recorded `inferred/<p>` → the LLM
//!     wrote it and it's untouched → overwrite *and* bump `inferred/<p>` to match.
//!   * base has an overlay and real ≠ `inferred/<p>` → a human diverged → **leave
//!     the real prop untouched**, only set `inferred/<p> = proposed` (suggestion).
//!   * base has **no overlay** → the LLM never wrote this instance (human/seed
//!     data) → **never overwrite**; mint an overlay recording `inferred/<p> =
//!     proposed` as a suggestion only.
//!
//! ## Module layout
//!   * [`classes`] — the two hard-wired SDNA subject classes, their registration,
//!     the per-pass run node and its provenance metadata.
//!   * [`gate`] — the §4 human-divergence gate and the graph reads it needs.
//!   * [`write`] — persisting an overlay (mint-or-bump + `inferred/<p>` links).
//!   * this module — the orchestration ([`apply_with_overlay`]) tying them
//!     together, plus the shared predicate vocabulary. The generic
//!     single-valued link upsert (`replace_link`) lives in the `graph` submodule.

mod accept;
mod classes;
mod gate;
mod write;

pub(crate) use accept::{
    accept_interpretation, list_overlays, overlay_of, reject_interpretation, OverlayView,
};
pub use classes::InterpretationRunCursor;
pub(crate) use classes::{
    ensure_interpretation_overlay_classes, mint_interpretation_run, InterpretationRunMeta,
};
use gate::{gate_update, inferred_snapshot};
use write::write_overlay;

use super::InterpretationOp;
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::AITask;
use std::collections::BTreeMap;

// ---- shared predicate vocabulary -------------------------------------------

/// The overlay's mandatory `kind` predicate — its sole discriminator (there is
/// no separate type flag): the presence of a `kind` link marks a base as
/// overlay-carrying (used by `write::overlay_exists` and `model_query`
/// conformance matching alike).
pub(super) const OVERLAY_KIND_PRED: &str = "ad4m://interp/kind";
/// Parallel-predicate prefix for the LLM's per-property value snapshot: the full
/// predicate is `ad4m://interp/inferred/<realPredicate>`.
pub(super) const INFERRED_PREFIX: &str = "ad4m://interp/inferred/";

// ---- overlay write plan ----------------------------------------------------

/// Whether the engine minted the node (`create`) or patched an existing one
/// (`update`) — recorded on the overlay's `kind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum OverlayKind {
    Create,
    Update,
}

impl OverlayKind {
    pub(super) fn as_str(self) -> &'static str {
        match self {
            OverlayKind::Create => "create",
            OverlayKind::Update => "update",
        }
    }
}

/// One base's overlay write: its `kind` plus the `inferred/<predicate> = value`
/// snapshot to record (keyed by the *real* predicate IRI).
pub(super) struct OverlayWrite {
    pub base: String,
    pub kind: OverlayKind,
    pub inferred: BTreeMap<String, serde_json::Value>,
}

/// Plan + apply the provenance overlay for a fully-planned set of real ops, and
/// apply the (possibly gated) real ops themselves. This is the single tail-step
/// [`crate::perspectives::interpretation::run_interpretation`] runs after
/// planning + no-op stripping. Returns the affected instance bases (same set the
/// pre-overlay path returned: every base a Create/Update/AddLinks touched).
///
/// Order of operations matters: the human-divergence gate and the last-inferred
/// baseline are read from the graph state *before* this pass writes anything, so
/// all reads happen first, then the gated real ops are applied, then the
/// overlays. The run node is minted when there is at least one overlay to
/// anchor, **or** when an AutoProcessor cursor must be recorded (even if the
/// LLM wrote nothing — those turns are still consumed).
pub(crate) async fn apply_with_overlay(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    ops: Vec<InterpretationOp>,
    task: &AITask,
    run_id: String,
    ran_at: String,
    context: &AgentContext,
    cursor: Option<&InterpretationRunCursor>,
    debug: Option<&crate::perspectives::interpretation::InterpretationDebug>,
) -> anyhow::Result<Vec<String>> {
    if ops.is_empty() {
        if cursor.is_some() {
            ensure_interpretation_overlay_classes(perspective, context).await?;
            let mut meta = InterpretationRunMeta::from_task(task, run_id, ran_at);
            if let Some(d) = debug {
                meta.debug_prompt = Some(d.prompt.clone());
                meta.debug_response = Some(d.response.clone());
            }
            // Empty-ops fast-path: no overlays to write, but the cursor mint
            // itself is a fan-out of `create_subject` + N × `update_subject`
            // (one per additional source id). Without a batch, each of those
            // is a separate `Shared` commit and therefore a separate p-diff-sync
            // revision — peers can observe the run at an intermediate state
            // with only some of the sources present, so their cursor filter
            // misses the unpublished ones and re-processes those turns
            // (2026-08-20 test-2 flake: `[22c2, 82e3, 22c2, ...]` — 22c2
            // dup'd because Bob's cursor at process time saw only one of
            // Alice's sources). Batch the whole mint so it lands atomically
            // as a single revision.
            let batch_id = perspective.create_batch().await;
            match mint_interpretation_run(
                perspective,
                &meta,
                cursor,
                Some(batch_id.clone()),
                context,
            )
            .await
            {
                Ok(_) => {
                    perspective
                        .commit_batch(batch_id.clone(), context)
                        .await
                        .map_err(|e| {
                            // commit_batch already tries to drop the batch on
                            // failure; belt-and-suspenders here too.
                            let _ = perspective.discard_batch(&batch_id);
                            anyhow::anyhow!(
                                "plan_interpretation_ops_resolved: empty-ops fast-path \
                                 commit_batch failed: {e:#}"
                            )
                        })?;
                }
                Err(e) => {
                    let _ = perspective.discard_batch(&batch_id).await;
                    return Err(e);
                }
            }
        }
        return Ok(Vec::new());
    }

    // Affected bases are reported for the *planned* set, independent of whether
    // the gate later drops a real write (a base that only gains an overlay
    // suggestion was still affected).
    let affected = touched_bases(&ops);

    ensure_interpretation_overlay_classes(perspective, context).await?;

    // Phase 1 (reads only): compute per-base overlay writes + the gated real ops.
    let mut overlays: Vec<OverlayWrite> = Vec::new();
    let mut real_ops: Vec<InterpretationOp> = Vec::new();
    for op in ops {
        match op {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                // A create is entirely the LLM's own: snapshot every value, real
                // write kept verbatim.
                let inferred = inferred_snapshot(shapes, &class, &values);
                overlays.push(OverlayWrite {
                    base: base.clone(),
                    kind: OverlayKind::Create,
                    inferred,
                });
                real_ops.push(InterpretationOp::Create {
                    base,
                    class,
                    values,
                });
            }
            InterpretationOp::Update {
                base,
                class,
                values,
            } => {
                // Human-divergence gate: keep only the real writes still owned by
                // the LLM; every proposed value is snapshotted as inferred.
                let (kept, inferred) =
                    gate_update(perspective, shapes, &base, &class, &values).await?;
                // A newly-minted overlay for an Update records kind=update; if an
                // overlay already exists (LLM-created earlier), write_overlay keeps
                // its original kind=create.
                overlays.push(OverlayWrite {
                    base: base.clone(),
                    kind: OverlayKind::Update,
                    inferred,
                });
                if !kept.is_empty() {
                    real_ops.push(InterpretationOp::Update {
                        base,
                        class,
                        values: kept,
                    });
                }
            }
            // Relations grow the graph additively; provenance for links is out of
            // scope for chunks 1+2 (scalar snapshot only). Pass through unchanged.
            other @ InterpretationOp::AddLinks { .. } => real_ops.push(other),
        }
    }

    // Phase 2: apply the gated real ops (own batch, atomic).
    super::apply_interpretation_ops(perspective, &real_ops, context).await?;

    // Phase 3: mint the run + write the overlays.
    //
    // Two independent triggers combine here:
    //   * `!overlays.is_empty()` — one-shot interpretation only mints when
    //     there is an overlay to anchor to a run.
    //   * `cursor.is_some()` — AutoProcessor always mints a run so consumed
    //     turn IDs land on it even when this pass wrote no overlay (the
    //     empty-batch bookkeeping the processed-turn cursor needs).
    //
    // Either trigger opens a Phase-3 batch that groups the run mint AND every
    // overlay write together, so a mid-write failure rolls back the run node
    // and every partial `inferred/<p>` link atomically. Without the batch a
    // crash between overlay writes would leave `inferred/<p>` out of sync with
    // the (already-committed) real value; `gate_update` would then read
    // `real != inferred` and permanently classify the property as
    // human-diverged, dropping every subsequent LLM proposal on that field.
    if !overlays.is_empty() || cursor.is_some() {
        let mut meta = InterpretationRunMeta::from_task(task, run_id, ran_at);
        if let Some(d) = debug {
            meta.debug_prompt = Some(d.prompt.clone());
            meta.debug_response = Some(d.response.clone());
        }
        let batch_id = perspective.create_batch().await;
        let mut phase3_err: Option<anyhow::Error> = None;

        'overlay: {
            let run_uri = match mint_interpretation_run(
                perspective,
                &meta,
                cursor,
                Some(batch_id.clone()),
                context,
            )
            .await
            {
                Ok(uri) => uri,
                Err(e) => {
                    phase3_err = Some(e);
                    break 'overlay;
                }
            };
            for ow in &overlays {
                if let Err(e) =
                    write_overlay(perspective, ow, &run_uri, Some(batch_id.clone()), context).await
                {
                    phase3_err = Some(e);
                    break 'overlay;
                }
            }
        }

        if let Some(e) = phase3_err {
            // Drop the half-built batch so it does not sit in `batch_store` for
            // BATCH_TIMEOUT_SECS. Mirrors the discard-on-error pattern used by
            // `apply_interpretation_ops` in Phase 2.
            let _ = perspective.discard_batch(&batch_id).await;
            return Err(e);
        }

        if let Err(e) = perspective.commit_batch(batch_id.clone(), context).await {
            // Defense-in-depth: `commit_batch` already removes the batch on
            // entry, so this discard is a no-op today. Kept explicit so the
            // "no lingering batch on any error path" invariant survives future
            // changes to `commit_batch`'s control flow.
            let _ = perspective.discard_batch(&batch_id).await;
            return Err(anyhow::anyhow!(
                "gate_apply_and_persist: phase 3 commit_batch failed: {e:#}"
            ));
        }
    }

    Ok(affected)
}

/// The instance bases an op set touches, in op order and de-duplicated (same
/// definition [`crate::perspectives::interpretation::run_interpretation`] used
/// before overlays existed).
fn touched_bases(ops: &[InterpretationOp]) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    for op in ops {
        let base = match op {
            InterpretationOp::Create { base, .. } | InterpretationOp::Update { base, .. } => base,
            InterpretationOp::AddLinks { source, .. } => source,
        };
        if seen.insert(base.clone()) {
            out.push(base.clone());
        }
    }
    out
}

/// Test-only: seed an `InterpretationOverlay` over `base` as if a prior LLM pass
/// had authored it — recording `inferred/<predicate> = value` for each entry so
/// the §4 gate reads the base as LLM-owned (real == inferred) and a subsequent
/// pass is allowed to overwrite/grow it in place. Reuses the production write
/// path (`ensure_interpretation_overlay_classes` → `mint_interpretation_run` →
/// `write_overlay`), so a seeded overlay is byte-identical to one the engine
/// itself would have written on the create pass. `kind` is always `create`: a
/// seed stands in for the pass that first minted the instance.
#[cfg(test)]
pub(crate) async fn seed_overlay(
    perspective: &mut PerspectiveInstance,
    base: &str,
    inferred: BTreeMap<String, serde_json::Value>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_interpretation_overlay_classes(perspective, context).await?;
    let meta = InterpretationRunMeta {
        run_id: format!("seed-{base}"),
        model: "seed".to_string(),
        prompt_version: "seed".to_string(),
        ran_at: "0".to_string(),
        debug_prompt: None,
        debug_response: None,
    };
    let run_uri = mint_interpretation_run(perspective, &meta, None, None, context).await?;
    let ow = OverlayWrite {
        base: base.to_string(),
        kind: OverlayKind::Create,
        inferred,
    };
    write_overlay(perspective, &ow, &run_uri, None, context).await
}

#[cfg(test)]
mod tests {
    use super::classes::InterpretationRunMeta;
    use super::write::overlay_exists;
    use super::{apply_with_overlay, INFERRED_PREFIX};
    use crate::agent::AgentContext;
    use crate::perspectives::interpretation::InterpretationOp;
    use crate::perspectives::interpretation_test_support::*;
    use crate::perspectives::model_query::types::ModelShape;
    use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
    use crate::types::{AITask, LinkQuery};
    use serde_json::json;

    /// The predicate a provenance snapshot uses for a real predicate.
    fn inferred_pred(real: &str) -> String {
        format!("{INFERRED_PREFIX}{real}")
    }

    /// A minimal interpretation task — enough to derive run provenance without a
    /// live AIService (`from_task` only reads the prompt + examples + model_id).
    fn dummy_task() -> AITask {
        AITask {
            model_id: "gemma3:12b".to_string(),
            system_prompt: "You extract typed instances.".to_string(),
            prompt_examples: Vec::new(),
            ..Default::default()
        }
    }

    async fn run_ops(
        perspective: &mut PerspectiveInstance,
        shapes: &[ModelShape],
        ops: Vec<InterpretationOp>,
        run_id: &str,
        ctx: &AgentContext,
    ) -> Vec<String> {
        apply_with_overlay(
            perspective,
            shapes,
            ops,
            &dummy_task(),
            run_id.to_string(),
            "1700000000000".to_string(),
            ctx,
            None,
            None,
        )
        .await
        .expect("apply_with_overlay")
    }

    fn create_op(base: &str, values: &[(&str, serde_json::Value)]) -> InterpretationOp {
        InterpretationOp::Create {
            base: base.to_string(),
            class: "Intention".to_string(),
            values: values
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
        }
    }

    fn update_op(base: &str, values: &[(&str, serde_json::Value)]) -> InterpretationOp {
        InterpretationOp::Update {
            base: base.to_string(),
            class: "Intention".to_string(),
            values: values
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
        }
    }

    async fn real_title(perspective: &PerspectiveInstance, base: &str) -> Option<String> {
        model_instances(perspective, "Intention", &["title", "owner"])
            .await
            .into_iter()
            .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base))
            .and_then(|r| r.get("title").and_then(|t| t.as_str()).map(str::to_string))
    }

    // ---- chunk 1: overlay instantiation + inferred snapshot ----------------

    #[tokio::test]
    async fn create_yields_real_instance_and_overlay_snapshot() {
        // A create must land a real Intention AND an overlay over the same base
        // whose `inferred/<p>` equals every written value, with kind=create and a
        // run link into a minted InterpretationRun.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/create-1";

        let affected = run_ops(
            &mut p,
            &shapes,
            vec![create_op(
                base,
                &[
                    ("title", json!("Ship the parser")),
                    ("owner", json!("Nico")),
                ],
            )],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(
            affected,
            vec![base.to_string()],
            "the create base is affected"
        );

        // Real instance is present and readable through the normal model path.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Ship the parser"),
            "the real Intention must be written and readable"
        );

        // Overlay inferred snapshot == the written values (parallel links).
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Ship the parser")],
            "inferred/title must equal the written title"
        );
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://owner")).await,
            vec![json!("Nico")],
            "inferred/owner must equal the written owner"
        );

        // kind=create, and exactly one overlay flag on the base.
        assert_eq!(
            decoded_targets(&p, base, "ad4m://interp/kind").await,
            vec![json!("create")],
            "a create overlay records kind=create"
        );
        let flags = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/kind".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(
            flags.len(),
            1,
            "exactly one overlay kind link identifies the base (no separate type flag)"
        );

        // The run link points at a minted InterpretationRun carrying our run_id.
        let run_links = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/run".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(run_links.len(), 1, "overlay carries exactly one run link");
        let run_uri = run_links[0].data.target.clone();
        assert_eq!(
            decoded_targets(&p, &run_uri, "ad4m://interp/run_id").await,
            vec![json!("run-1")],
            "the run node carries the pass's run_id"
        );
        assert_eq!(
            decoded_targets(&p, &run_uri, "ad4m://interp/model").await,
            vec![json!("gemma3:12b")],
            "the run node records the model"
        );
    }

    #[tokio::test]
    async fn single_overlay_per_base_updated_in_place_across_passes() {
        // Chunk 1 (one overlay per base, no accumulation) + chunk 2a (an
        // unchanged, still-LLM-owned node is overwritten in place on the 2nd pass
        // and the overlay's inferred value is bumped to match).
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/roll-1";

        // Pass 1: create with title v1.
        run_ops(
            &mut p,
            &shapes,
            vec![create_op(base, &[("title", json!("Draft the doc"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Pass 2: the LLM refines the SAME (still-LLM-owned) node to v2.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(
                base,
                &[("title", json!("Draft and circulate the doc"))],
            )],
            "run-2",
            &ctx,
        )
        .await;

        // Real value overwritten in place (LLM refining its own inference).
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Draft and circulate the doc"),
            "an unchanged (LLM-owned) node is overwritten in place on pass 2"
        );

        // Exactly ONE overlay kind link — updated in place, not accumulated per run.
        let flags = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/kind".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(flags.len(), 1, "one overlay per base across passes");

        // inferred/title bumped to v2, single link (old value replaced).
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Draft and circulate the doc")],
            "the overlay's inferred snapshot is bumped in place to the new value"
        );

        // run link bumped to pass 2's run.
        let run_uri = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/run".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(run_uri.len(), 1, "still a single run link after pass 2");
        assert_eq!(
            decoded_targets(&p, &run_uri[0].data.target, "ad4m://interp/run_id").await,
            vec![json!("run-2")],
            "the run pointer is bumped to the latest pass"
        );
    }

    // ---- chunk 2: human-divergence gate ------------------------------------

    #[tokio::test]
    async fn human_edited_value_is_protected_overlay_only_gets_suggestion() {
        // Chunk 2b: once a human has edited the real value away from the overlay's
        // last-inferred baseline, a later LLM pass must NOT overwrite the real
        // value — it only updates the overlay's inferred/<p> as a suggestion.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/human-1";

        // Pass 1: LLM creates (real == inferred == v1).
        run_ops(
            &mut p,
            &shapes,
            vec![create_op(base, &[("title", json!("Original LLM title"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Human edits the real value in place (diverging from the baseline).
        p.update_subject(
            SubjectClassOption {
                class_name: Some("Intention".to_string()),
                query: None,
            },
            base.to_string(),
            json!({ "title": "Human corrected title" }),
            None,
            &ctx,
        )
        .await
        .expect("human edit");
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human corrected title"),
            "sanity: the human edit landed on the real value"
        );

        // Pass 2: LLM proposes a new value for the now-diverged field.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(
                base,
                &[("title", json!("Second LLM suggestion"))],
            )],
            "run-2",
            &ctx,
        )
        .await;

        // Real value is LEFT UNTOUCHED — the human's edit survives.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human corrected title"),
            "a human-diverged real value must not be overwritten by a later pass"
        );
        // The overlay records the LLM's proposal as a suggestion only.
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Second LLM suggestion")],
            "the overlay's inferred/<p> is updated to the new suggestion"
        );
    }

    #[tokio::test]
    async fn non_llm_instance_is_never_overwritten_only_gets_overlay_suggestion() {
        // §4 core protection (review fix): a base the LLM did NOT write (no
        // overlay — e.g. human/seed-created) must never have its real values
        // overwritten by an interpretation pass. The pass may only stage the
        // proposal in a freshly-minted overlay as a suggestion.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/seed-1";

        // A non-LLM actor creates the instance directly (no overlay is minted).
        p.create_subject(
            SubjectClassOption {
                class_name: Some("Intention".to_string()),
                query: None,
            },
            base.to_string(),
            Some(json!({ "title": "Human authored title" })),
            None,
            &ctx,
            None,
        )
        .await
        .expect("seed create");
        assert!(
            !overlay_exists(&p, base).await.unwrap(),
            "sanity: a plain create leaves no overlay on the base"
        );

        // The LLM proposes a different value for the same field on its first pass.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(base, &[("title", json!("LLM proposed title"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Real value is LEFT UNTOUCHED — the LLM never owned this instance.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human authored title"),
            "an instance the LLM never wrote must not be overwritten on first touch"
        );
        // An overlay is now established, recording the proposal as a suggestion.
        assert!(
            overlay_exists(&p, base).await.unwrap(),
            "the pass still establishes an overlay baseline"
        );
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("LLM proposed title")],
            "the overlay records the LLM's value as an inferred suggestion only"
        );
        assert_eq!(
            decoded_targets(&p, base, "ad4m://interp/kind").await,
            vec![json!("update")],
            "a suggestion-only overlay on an existing node records kind=update"
        );
    }

    #[tokio::test]
    async fn from_task_prompt_version_tracks_prompt_changes() {
        // prompt_version must be a stable hash of system prompt + few-shots, so a
        // prompt change yields a new version and an identical prompt reproduces it.
        let a = InterpretationRunMeta::from_task(&dummy_task(), "r".into(), "0".into());
        let b = InterpretationRunMeta::from_task(&dummy_task(), "r".into(), "0".into());
        assert_eq!(
            a.prompt_version, b.prompt_version,
            "same prompt → same version"
        );

        let mut changed = dummy_task();
        changed.system_prompt = "Different system prompt.".into();
        let c = InterpretationRunMeta::from_task(&changed, "r".into(), "0".into());
        assert_ne!(
            a.prompt_version, c.prompt_version,
            "a changed system prompt → a different prompt_version"
        );
        assert_eq!(a.model, "gemma3:12b", "model is carried from the task");
    }
}
