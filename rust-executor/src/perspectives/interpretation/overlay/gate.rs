//! The §4 human-divergence gate: decide, per proposed Update value, whether the
//! engine may overwrite the real property or must only stage it as an overlay
//! suggestion — plus the graph reads the decision needs.

use super::write::overlay_exists;
use super::INFERRED_PREFIX;
use crate::perspectives::interpretation::class_local_name;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::LinkQuery;
use std::collections::BTreeMap;

/// The scalar snapshot for an op's values, keyed by the *real* predicate IRI
/// (`ns://title`, …) so it can be stored as the parallel `inferred/<p>` link.
/// Values with no resolvable predicate on the class shape are skipped.
pub(super) fn inferred_snapshot(
    shapes: &[ModelShape],
    class: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> BTreeMap<String, serde_json::Value> {
    let mut out = BTreeMap::new();
    for (name, value) in values {
        if let Some(pred) = predicate_for(shapes, class, name) {
            out.insert(pred, value.clone());
        }
    }
    out
}

/// The real predicate IRI a class declares for a property name (`title` ->
/// `ns://title`). `None` if the class/property is unknown to the shapes.
fn predicate_for(shapes: &[ModelShape], class: &str, name: &str) -> Option<String> {
    shapes
        .iter()
        .find(|s| class_local_name(&s.target_class) == class)?
        .properties
        .iter()
        .find(|p| p.name == name)
        .map(|p| p.predicate.clone())
}

/// The human-divergence gate for an Update (§4). The engine only ever overwrites
/// a real value it can prove the LLM owns — otherwise it stages the proposal in
/// the overlay and leaves the real value alone. Reads the base's current real
/// values (through the same `model_query` read path app code uses) and the
/// overlay's last-inferred snapshot, then per property decides:
///   * real unset → nothing to overwrite → **keep** the real write (a fresh
///     field the LLM is filling in).
///   * the base has **no overlay** yet → the LLM did *not* write this instance
///     (it's human/seed-created), so its real values are **not** the LLM's to
///     overwrite → **drop** the real write; only stage `inferred = proposed` as
///     an overlay suggestion. (This is the §4 protection: an existing value the
///     LLM never authored is never silently clobbered.)
///   * overlay exists and real == overlay `inferred/<p>` → the LLM wrote it and
///     it hasn't been touched since → **keep** the real write and bump
///     `inferred/<p>` (the LLM refining its own inference).
///   * overlay exists and real ≠ overlay `inferred/<p>` → a human diverged from
///     the last inference → **drop** the real write; only record the proposed
///     value as a suggestion.
///
/// Returns `(kept_real_values, inferred_snapshot)`. The inferred snapshot always
/// carries every proposed property (kept or suggested), keyed by predicate — so
/// even a fully-gated (suggestion-only) update still establishes/updates the
/// overlay baseline.
pub(super) async fn gate_update(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    base: &str,
    class: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> anyhow::Result<(
    serde_json::Map<String, serde_json::Value>,
    BTreeMap<String, serde_json::Value>,
)> {
    let real = read_real_values(perspective, class, base, values).await?;
    let inferred_now = read_inferred_values(perspective, base).await?;
    let has_overlay = overlay_exists(perspective, base).await?;

    let mut kept = serde_json::Map::new();
    let mut inferred = BTreeMap::new();
    for (name, proposed) in values {
        let Some(pred) = predicate_for(shapes, class, name) else {
            // Unknown property: keep the write (create_subject ignores it anyway)
            // but don't fabricate an inferred snapshot for a predicate we can't name.
            kept.insert(name.clone(), proposed.clone());
            continue;
        };
        inferred.insert(pred.clone(), proposed.clone());

        let real_val = real.get(name);
        let inferred_val = inferred_now.get(&pred);
        let llm_owns = match real_val {
            // Fresh/unset field → the LLM filling it in is always allowed.
            None => true,
            // No overlay → the LLM never wrote this instance, so an existing real
            // value is not its to overwrite → suggestion-only (protect the human).
            Some(_) if !has_overlay => false,
            // Overlay present → overwrite only while the value is still identical
            // to the last inference (untouched); a human edit diverges it → drop.
            Some(rv) => inferred_val == Some(rv),
        };
        if llm_owns {
            kept.insert(name.clone(), proposed.clone());
        }
    }
    Ok((kept, inferred))
}

/// Read the base's current real values for the given property names via
/// `model_query` (decoded through each property's own getter). Returns a map of
/// property-name -> decoded value for the fields actually present on the row.
async fn read_real_values(
    perspective: &PerspectiveInstance,
    class: &str,
    base: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> anyhow::Result<serde_json::Map<String, serde_json::Value>> {
    let props: Vec<&str> = values.keys().map(String::as_str).collect();
    let query = serde_json::json!({ "properties": props }).to_string();
    let result_json = perspective
        .model_query(class, &query, None)
        .await
        .map_err(|e| anyhow::anyhow!("gate_update: model_query({class}) failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&result_json)
        .map_err(|e| anyhow::anyhow!("gate_update: bad model_query result for {class}: {e:#}"))?;
    let row = result
        .get("instances")
        .and_then(|v| v.as_array())
        .and_then(|rows| {
            rows.iter()
                .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base))
        });
    let mut out = serde_json::Map::new();
    if let Some(obj) = row.and_then(|r| r.as_object()) {
        for name in values.keys() {
            if let Some(v) = obj.get(name) {
                if !v.is_null() {
                    out.insert(name.clone(), v.clone());
                }
            }
        }
    }
    Ok(out)
}

/// Read the base's overlay `inferred/<predicate>` snapshot, decoded to JSON
/// values keyed by the *real* predicate IRI. Empty when the base has no overlay.
async fn read_inferred_values(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<BTreeMap<String, serde_json::Value>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await?;
    let mut out = BTreeMap::new();
    for l in links {
        if let Some(pred) = l.data.predicate.as_deref() {
            if let Some(real_pred) = pred.strip_prefix(INFERRED_PREFIX) {
                out.insert(real_pred.to_string(), parse_literal_value(&l.data.target));
            }
        }
    }
    Ok(out)
}
