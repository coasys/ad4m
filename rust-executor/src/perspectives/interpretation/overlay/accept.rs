//! Human accept / reject of the interpretation overlay (design §8 accept-loop).
//!
//! An `InterpretationOverlay` marks an instance (or a single property) as
//! LLM-inferred and, when the §4 gate declined to touch a human/seed value,
//! carries the model's *staged* value in an `inferred/<p>` link. A human then
//! resolves each suggestion:
//!
//! * **accept** — the LLM's value becomes the real, human-owned value and the
//!   overlay is **deleted** (Nico's call: no `accepted` flag, since one base can
//!   be touched by many runs and kept overlays would accumulate). Once the
//!   overlay is gone the §4 gate treats the base as human-owned, so a later run
//!   will not silently overwrite it — delete *is* the lock.
//! * **reject** — the suggestion is dropped. Rejecting a whole *create* removes
//!   the suggested instance entirely; rejecting an *update* just drops the
//!   overlay and leaves the real value as it was.
//!
//! Property-scoped accept/reject act on a single `inferred/<p>`; when the last
//! `inferred/<p>` is gone the overlay's `kind`/`run` links are cleaned up too.

use super::{INFERRED_PREFIX, OVERLAY_KIND_PRED};
use crate::agent::AgentContext;
use crate::perspectives::interpretation::replace_link;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkExpression, LinkQuery};

const OVERLAY_RUN_PRED: &str = "ad4m://interp/run";

/// One pending overlay, flattened for the query surface / UIs.
#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct OverlayView {
    /// The base instance the overlay sits on.
    pub base: String,
    /// `"create"` | `"update"` — whether the LLM authored the whole instance or
    /// only proposed changes to an existing one.
    pub kind: String,
    /// The `InterpretationRun` that last wrote it, if present.
    pub run: Option<String>,
    /// `(real predicate, staged value)` pairs — the model's proposed values a
    /// human has not yet accepted.
    pub inferred: Vec<(String, serde_json::Value)>,
}

/// Read the overlay on `base`, or `None` when the base carries none.
pub(crate) async fn overlay_of(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<Option<OverlayView>> {
    let all = links_from(perspective, base).await?;
    let kind = match first_target(&all, OVERLAY_KIND_PRED) {
        Some(k) => k,
        None => return Ok(None),
    };
    let run = first_target(&all, OVERLAY_RUN_PRED);
    let inferred = all
        .iter()
        .filter_map(|l| {
            let pred = l.data.predicate.as_deref()?;
            let real = pred.strip_prefix(INFERRED_PREFIX)?;
            Some((real.to_string(), parse_literal_value(&l.data.target)))
        })
        .collect();
    Ok(Some(OverlayView {
        base: base.to_string(),
        kind,
        run,
        inferred,
    }))
}

/// Every base in the perspective that currently carries an overlay — the
/// pending-suggestions list a UI renders.
pub(crate) async fn list_overlays(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<Vec<OverlayView>> {
    let kind_links = perspective
        .get_links(&LinkQuery {
            predicate: Some(OVERLAY_KIND_PRED.to_string()),
            ..Default::default()
        })
        .await?;
    let mut bases: Vec<String> = kind_links.into_iter().map(|l| l.data.source).collect();
    bases.sort();
    bases.dedup();
    let mut out = Vec::with_capacity(bases.len());
    for base in bases {
        if let Some(v) = overlay_of(perspective, &base).await? {
            out.push(v);
        }
    }
    Ok(out)
}

/// Accept the overlay's suggestion(s) on `base`: materialize the staged value(s)
/// as the real, human-owned value(s) and delete the (targeted) overlay. With
/// `property = None` the whole overlay is accepted; with `Some(p)` only that
/// property's suggestion is.
pub(crate) async fn accept_interpretation(
    perspective: &mut PerspectiveInstance,
    base: &str,
    property: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let all = links_from(perspective, base).await?;
    if first_target(&all, OVERLAY_KIND_PRED).is_none() {
        anyhow::bail!("accept_interpretation: no overlay on `{base}`");
    }

    // Materialize each targeted `inferred/<p>` into the real value, then drop it.
    for l in inferred_links(&all, property) {
        let real_pred = l
            .data
            .predicate
            .as_deref()
            .and_then(|p| p.strip_prefix(INFERRED_PREFIX))
            .expect("filtered to inferred links");
        // Copy the staged literal target verbatim onto the real predicate — same
        // encoding the interpreter writes for a literal-valued property. Accept
        // is a user-triggered single-property operation, so no caller-owned
        // batch to thread — `None` matches every other one-shot call site.
        replace_link(perspective, base, real_pred, &l.data.target, None, context).await?;
        perspective
            .remove_links(vec![LinkExpression::from(l.clone())], None)
            .await?;
    }

    // If no staged suggestion remains, the overlay has done its job — remove its
    // `kind`/`run` so the base reads as plain human-owned data (delete = lock).
    prune_overlay_shell_if_empty(perspective, base).await
}

/// Reject the overlay's suggestion(s) on `base`. `property = Some(p)` drops just
/// that suggestion. `property = None` rejects the whole base: a `create` overlay
/// deletes the suggested instance outright — including any inbound links (e.g.
/// an AutoProcessor `mint_scope` parent link) that would otherwise dangle after
/// the base is gone; an `update` overlay is removed while the real
/// (pre-inference) value is left untouched.
pub(crate) async fn reject_interpretation(
    perspective: &mut PerspectiveInstance,
    base: &str,
    property: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let _ = context;
    let all = links_from(perspective, base).await?;
    let raw_kind = match first_target(&all, OVERLAY_KIND_PRED) {
        Some(k) => k,
        None => anyhow::bail!("reject_interpretation: no overlay on `{base}`"),
    };
    // `write_overlay` uses `create_subject`'s setter path, which encodes a
    // non-URI string target as `literal:string:<value>`. Compare against the
    // decoded value or a whole-base reject of a `create` silently takes the
    // update branch and leaves the LLM-authored instance orphaned in the
    // graph (CodeRabbit #881 review). `parse_literal_value` also handles the
    // constructor's plain-string target (`"create"`) — that branch returns
    // `Value::String("create")` unchanged.
    let kind = match parse_literal_value(&raw_kind) {
        serde_json::Value::String(s) => s,
        other => other.to_string(),
    };

    if let Some(_prop) = property {
        // Drop just this suggestion; the real value stays as it is.
        remove(perspective, inferred_links(&all, property)).await?;
        return prune_overlay_shell_if_empty(perspective, base).await;
    }

    // Whole-base reject.
    if kind == "create" {
        // The LLM authored the whole instance — discard it, its overlay, AND
        // every inbound link that points at it (e.g. the `mint_scope` parent
        // link an AutoProcessor writes). Without the inbound sweep, a
        // parent-scope UI querying "children of X" would keep seeing a link
        // to a base whose scalars have all been deleted (CodeRabbit #881
        // review).
        remove(perspective, all.iter().collect()).await?;
        let inbound = perspective
            .get_links(&LinkQuery {
                target: Some(base.to_string()),
                ..Default::default()
            })
            .await?;
        if !inbound.is_empty() {
            remove(perspective, inbound.iter().collect()).await?;
        }
        Ok(())
    } else {
        // Update: drop only the overlay (kind + run + all inferred), keep the
        // real value the human/prior state already holds.
        let shell = all
            .iter()
            .filter(|l| is_overlay_link(l))
            .collect::<Vec<_>>();
        remove(perspective, shell).await
    }
}

// ── helpers ────────────────────────────────────────────────────────────────

async fn links_from(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<Vec<DecoratedLinkExpression>> {
    Ok(perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await?)
}

fn first_target(links: &[DecoratedLinkExpression], predicate: &str) -> Option<String> {
    links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some(predicate))
        .map(|l| l.data.target.clone())
}

/// The `inferred/<p>` links on the base, optionally narrowed to one real
/// predicate `property`.
fn inferred_links<'a>(
    links: &'a [DecoratedLinkExpression],
    property: Option<&str>,
) -> Vec<&'a DecoratedLinkExpression> {
    links
        .iter()
        .filter(|l| {
            let Some(pred) = l.data.predicate.as_deref() else {
                return false;
            };
            let Some(real) = pred.strip_prefix(INFERRED_PREFIX) else {
                return false;
            };
            property.is_none_or(|p| p == real)
        })
        .collect()
}

/// True for a link that is part of the overlay itself (its `kind`/`run`
/// discriminators or an `inferred/<p>` snapshot) rather than real instance data.
fn is_overlay_link(l: &DecoratedLinkExpression) -> bool {
    match l.data.predicate.as_deref() {
        Some(p) => {
            p == OVERLAY_KIND_PRED || p == OVERLAY_RUN_PRED || p.starts_with(INFERRED_PREFIX)
        }
        None => false,
    }
}

async fn remove(
    perspective: &mut PerspectiveInstance,
    links: Vec<&DecoratedLinkExpression>,
) -> anyhow::Result<()> {
    if links.is_empty() {
        return Ok(());
    }
    let exprs: Vec<LinkExpression> = links.into_iter().cloned().map(Into::into).collect();
    perspective.remove_links(exprs, None).await?;
    Ok(())
}

/// Once no `inferred/<p>` suggestion is left on the base, remove the overlay's
/// `kind`/`run` shell too — no overlay means the §4 gate treats the base as
/// human-owned.
async fn prune_overlay_shell_if_empty(
    perspective: &mut PerspectiveInstance,
    base: &str,
) -> anyhow::Result<()> {
    let all = links_from(perspective, base).await?;
    let any_inferred = all.iter().any(|l| {
        l.data
            .predicate
            .as_deref()
            .is_some_and(|p| p.starts_with(INFERRED_PREFIX))
    });
    if any_inferred {
        return Ok(());
    }
    let shell = all
        .iter()
        .filter(|l| {
            matches!(
                l.data.predicate.as_deref(),
                Some(OVERLAY_KIND_PRED) | Some(OVERLAY_RUN_PRED)
            )
        })
        .collect::<Vec<_>>();
    remove(perspective, shell).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::{Link, LinkStatus};

    async fn add(
        p: &mut PerspectiveInstance,
        base: &str,
        pred: &str,
        target: &str,
        ctx: &AgentContext,
    ) {
        p.add_links(
            vec![Link {
                source: base.into(),
                predicate: Some(pred.into()),
                target: target.into(),
            }],
            LinkStatus::Local,
            None,
            ctx,
            None,
        )
        .await
        .expect("add_links");
    }

    async fn preds_on(p: &PerspectiveInstance, base: &str) -> Vec<String> {
        p.get_links(&LinkQuery {
            source: Some(base.into()),
            ..Default::default()
        })
        .await
        .unwrap()
        .into_iter()
        .filter_map(|l| l.data.predicate)
        .collect()
    }

    /// A create-overlay: one real value plus a staged `inferred/<p>` snapshot.
    async fn seed_create(
        p: &mut PerspectiveInstance,
        base: &str,
        real_pred: &str,
        val: &str,
        ctx: &AgentContext,
    ) {
        add(p, base, real_pred, val, ctx).await;
        add(p, base, OVERLAY_KIND_PRED, "create", ctx).await;
        add(p, base, OVERLAY_RUN_PRED, "ad4m://interp/run/r1", ctx).await;
        add(p, base, &format!("{INFERRED_PREFIX}{real_pred}"), val, ctx).await;
    }

    #[tokio::test]
    async fn accept_deletes_overlay_keeps_real_value() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/1";
        seed_create(&mut p, base, "soa://title", "literal:string:Fix", &ctx).await;
        assert!(overlay_of(&p, base).await.unwrap().is_some());

        accept_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            overlay_of(&p, base).await.unwrap().is_none(),
            "overlay deleted"
        );
        let preds = preds_on(&p, base).await;
        assert!(
            preds.contains(&"soa://title".to_string()),
            "real value kept"
        );
        assert!(
            !preds.iter().any(|x| x.starts_with(INFERRED_PREFIX)
                || x == OVERLAY_KIND_PRED
                || x == OVERLAY_RUN_PRED),
            "no overlay links remain"
        );
    }

    #[tokio::test]
    async fn reject_create_deletes_whole_instance() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/2";
        seed_create(&mut p, base, "soa://title", "literal:string:Drop", &ctx).await;

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            preds_on(&p, base).await.is_empty(),
            "suggested instance fully removed"
        );
    }

    /// Production kind values are literal-encoded by the SDNA setter path
    /// (`write_overlay` → `create_subject` → `setSingleTarget` on a
    /// non-URI string). A whole-base reject must normalise the target
    /// before comparing to `"create"`, or it silently takes the update
    /// branch and leaves the LLM-authored instance orphaned — CodeRabbit
    /// #881 review. Also verifies inbound-link sweep so an AutoProcessor
    /// `mint_scope` parent link doesn't dangle after the base is gone.
    #[tokio::test]
    async fn reject_create_normalises_literal_kind_and_sweeps_inbound_links() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/lit";
        add(&mut p, base, "soa://title", "literal:string:Ship", &ctx).await;
        // Production-shaped `kind`: literal-encoded, exactly what
        // `write_overlay` writes via the SDNA setter.
        add(
            &mut p,
            base,
            OVERLAY_KIND_PRED,
            "literal:string:create",
            &ctx,
        )
        .await;
        add(
            &mut p,
            base,
            OVERLAY_RUN_PRED,
            "ad4m://interp/run/rlit",
            &ctx,
        )
        .await;
        add(
            &mut p,
            base,
            &format!("{INFERRED_PREFIX}soa://title"),
            "literal:string:Ship",
            &ctx,
        )
        .await;
        // Simulate an inbound `mint_scope` parent link the auto-processor
        // would have written.
        let parent = "soa://project/lit";
        let contains_pred = "soa://contains";
        p.add_link(
            crate::types::Link {
                source: parent.into(),
                predicate: Some(contains_pred.into()),
                target: base.into(),
            },
            crate::types::LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            preds_on(&p, base).await.is_empty(),
            "literal-encoded kind still routes to the create branch: \
             suggested instance fully removed"
        );
        let inbound_after = p
            .get_links(&LinkQuery {
                target: Some(base.into()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert!(
            inbound_after.is_empty(),
            "inbound links to the rejected base are also swept, no dangling \
             parent references"
        );
    }

    #[tokio::test]
    async fn reject_update_keeps_real_drops_overlay() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Sub/3";
        add(&mut p, base, "soa://summary", "literal:string:Human", &ctx).await;
        add(&mut p, base, OVERLAY_KIND_PRED, "update", &ctx).await;
        add(&mut p, base, OVERLAY_RUN_PRED, "ad4m://interp/run/r2", &ctx).await;
        add(
            &mut p,
            base,
            &format!("{INFERRED_PREFIX}soa://summary"),
            "literal:string:LLM",
            &ctx,
        )
        .await;

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        let preds = preds_on(&p, base).await;
        assert!(
            preds.contains(&"soa://summary".to_string()),
            "human value kept"
        );
        assert!(
            overlay_of(&p, base).await.unwrap().is_none(),
            "overlay gone"
        );
    }
}
