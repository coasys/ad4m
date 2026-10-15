//! Persisting the overlay for one base: mint-or-bump the overlay subject and
//! replace its `inferred/<p>` parallel links, plus the presence check that keys
//! off the overlay's mandatory `kind` link.

use super::classes::INTERP_OVERLAY_CLASS;
use super::{OverlayWrite, INFERRED_PREFIX, OVERLAY_KIND_PRED};
use crate::agent::AgentContext;
use crate::perspectives::interpretation::replace_link;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::LinkQuery;
use ad4m_client::literal::Literal;

/// Write (or update in place) the overlay for one base: ensure the overlay
/// subject exists with its `kind` + `run`, then replace each `inferred/<p>`
/// parallel link with the new snapshot value. Exactly one overlay per base — a
/// second pass bumps `run` and the inferred values rather than accumulating.
///
/// `batch_id` groups the subject write and every `inferred/<p>` replace into a
/// single caller-owned batch, so a mid-Phase-3 failure rolls back the whole
/// overlay for this base instead of leaving `inferred/<p>` out of sync with the
/// real value (which `gate_update` would then permanently classify as
/// human-diverged, since real ≠ inferred blocks every subsequent overwrite).
/// Test helpers that write one overlay in isolation pass `None`.
pub(super) async fn write_overlay(
    perspective: &mut PerspectiveInstance,
    ow: &OverlayWrite,
    run_uri: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if overlay_exists(perspective, &ow.base).await? {
        // Overlay already minted (earlier pass): keep its original `kind`, just
        // bump the `run` pointer to this pass.
        perspective
            .update_subject(
                SubjectClassOption {
                    class_name: Some(INTERP_OVERLAY_CLASS.to_string()),
                    query: None,
                },
                ow.base.clone(),
                serde_json::json!({ "run": run_uri }),
                batch_id.clone(),
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_overlay: update_subject(run) failed: {e:#}"))?;
    } else {
        // First overlay on this base: mint it (kind flag) with kind + run.
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(INTERP_OVERLAY_CLASS.to_string()),
                    query: None,
                },
                ow.base.clone(),
                Some(serde_json::json!({ "kind": ow.kind.as_str(), "run": run_uri })),
                batch_id.clone(),
                context,
                None,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_overlay: create_subject(overlay) failed: {e:#}"))?;
    }

    for (pred, value) in &ow.inferred {
        let inferred_pred = format!("{INFERRED_PREFIX}{pred}");
        replace_link(
            perspective,
            &ow.base,
            &inferred_pred,
            &encode_inferred_literal(value)?,
            batch_id.clone(),
            context,
        )
        .await?;
    }
    Ok(())
}

/// True once `base` carries an overlay — detected by its mandatory `kind` link
/// (the overlay's sole discriminator; there is no separate type flag).
pub(super) async fn overlay_exists(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<bool> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(OVERLAY_KIND_PRED.to_string()),
            ..Default::default()
        })
        .await?;
    Ok(!links.is_empty())
}

/// Encode an inferred value as a deterministic `literal:` URI target that
/// [`crate::perspectives::model_query::utils::parse_literal_value`] round-trips
/// back to the same JSON value — so a later pass can compare the real value
/// (decoded via `model_query`) against it.
///
/// Integers are formatted with their exact `i64`/`u64` string representation —
/// **not** routed through `f64` — because `parse_literal_value` decodes a
/// `literal:number:X` back through `i64::from_str` first and falls back to
/// `f64` only for non-integers. Encoding a large `i64` (magnitude beyond the
/// `f64` mantissa's 2^53 exact-integer range) through `f64` rounds it, so the
/// decoded value would differ from the real one and `gate_update` would
/// classify the property as human-diverged permanently. The boolean and
/// string paths already round-trip exactly.
fn encode_inferred_literal(value: &serde_json::Value) -> anyhow::Result<String> {
    let url = match value {
        serde_json::Value::String(s) => Literal::from_string(s.clone()).to_url(),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(format!("literal:number:{i}"))
            } else if let Some(u) = n.as_u64() {
                Ok(format!("literal:number:{u}"))
            } else {
                match n.as_f64() {
                    Some(f) => Literal::from_number(f).to_url(),
                    None => Literal::from_string(n.to_string()).to_url(),
                }
            }
        }
        serde_json::Value::Bool(b) => Ok(format!("literal:boolean:{b}")),
        other => Literal::from_json(other.clone()).to_url(),
    };
    url.map_err(|e| anyhow::anyhow!("encode_inferred_literal: {e:#}"))
}
