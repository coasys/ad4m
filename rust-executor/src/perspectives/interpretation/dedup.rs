use super::graph::filter_already_present;
use super::ProposedInstance;
use std::collections::{HashMap, HashSet};

/// How to decide whether a proposed instance is "already present" in the graph
/// — the identity-dedup policy `run_interpretation` runs after the LLM emits
/// its proposals but before the write path.
///
/// This is **configuration on the runner**, not an SDNA/class hint: an
/// AutoProcessor or a test can swap the strategy per perspective without
/// touching the models themselves. The default is [`Self::NormalizedString`],
/// the cheap, deterministic string match that shipped with the first
/// tree-aware writes; [`Self::Semantic`] embeds the identity strings via an
/// Ollama-compatible endpoint and dedups by cosine similarity — the same
/// primitive an activity runner will later expose per-neighbourhood.
#[derive(Debug, Clone)]
pub enum DedupStrategy {
    /// Trim + collapse whitespace + lowercase, then exact-match against the
    /// per-class set of existing identity values. Zero external calls. This is
    /// the default: existing callers observe no behaviour change.
    NormalizedString,
    /// Embed each identity string through the executor's own
    /// [`AIService`](crate::ai_service::AIService) embedding model (a local
    /// candle Bert, CPU-capable — no external endpoint and no CUDA CI switch).
    /// Proposals whose maximum cosine similarity to any existing identity in
    /// the same class meets or exceeds `threshold` are treated as duplicates
    /// and dropped.
    Semantic {
        /// Id (registered name) of the `AIService` embedding model to use —
        /// must be registered as a `ModelType::Embedding` model and loaded in
        /// the embedding channel. The AutoProcessor supplies the default; tests
        /// register one in the harness.
        model: String,
        /// Cosine similarity above which two identity strings are considered
        /// the same instance.
        threshold: f32,
    },
}

impl Default for DedupStrategy {
    fn default() -> Self {
        Self::NormalizedString
    }
}

impl DedupStrategy {
    /// A `Semantic` strategy naming the `AIService` embedding model from
    /// `INTERPRETATION_EMBED_MODEL` (default `interpretation-embed`), with a
    /// caller-provided threshold. Useful for e2e tests and the future
    /// AutoProcessor default builder. The named model must already be
    /// registered with `AIService` as a `ModelType::Embedding` model.
    pub fn semantic_from_env(threshold: f32) -> Self {
        let model = std::env::var("INTERPRETATION_EMBED_MODEL")
            .unwrap_or_else(|_| "interpretation-embed".to_string());
        Self::Semantic { model, threshold }
    }
}

/// Cosine similarity of two equal-length vectors. Returns `0.0` for a
/// zero-magnitude input (rather than NaN) so callers can compare thresholds
/// without special-casing.
pub(crate) fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    debug_assert_eq!(a.len(), b.len(), "cosine_similarity: length mismatch");
    let mut dot = 0.0f32;
    let mut na = 0.0f32;
    let mut nb = 0.0f32;
    for (x, y) in a.iter().zip(b.iter()) {
        dot += x * y;
        na += x * x;
        nb += y * y;
    }
    let denom = na.sqrt() * nb.sqrt();
    if denom == 0.0 {
        0.0
    } else {
        dot / denom
    }
}

/// Embed `texts` through the executor's own [`AIService`] embedding model
/// (`model_id` must be a registered `ModelType::Embedding` model — a local
/// candle Bert, so this runs on CPU with no external endpoint). One call per
/// text against the shared embedding channel; order is preserved so callers can
/// zip results back onto their inputs.
pub(crate) async fn embed_via_ai_service(
    model_id: &str,
    texts: &[String],
) -> anyhow::Result<Vec<Vec<f32>>> {
    if texts.is_empty() {
        return Ok(Vec::new());
    }
    let service = crate::ai_service::AIService::global_instance()
        .await
        .map_err(|e| anyhow::anyhow!("semantic dedup: AIService not ready: {e:#}"))?;
    let mut out = Vec::with_capacity(texts.len());
    for text in texts {
        let embedded = service
            .embed(model_id.to_string(), text.clone())
            .await
            .map_err(|e| anyhow::anyhow!("semantic dedup: embed('{model_id}') failed: {e:#}"))?;
        out.push(embedded.embeddings);
    }
    Ok(out)
}

/// PURE (no I/O) semantic-dedup filter over pre-computed vectors — the
/// testable core of the semantic path. Drops each proposed index whose max
/// cosine similarity to any existing vector of the same class is
/// `≥ threshold`, preserving [`ProposedInstance`] order for the rest.
///
/// `existing_vecs` and `proposed_vecs` are grouped by class local-name. A
/// proposal whose class has no entry in `proposed_vecs` is kept (nothing to
/// dedup); a class with no existing vectors also passes everything through.
pub(crate) fn semantic_dedup_pure(
    instances: Vec<ProposedInstance>,
    existing_vecs: &HashMap<String, Vec<Vec<f32>>>,
    proposed_vecs: &HashMap<String, Vec<(usize, Vec<f32>)>>,
    threshold: f32,
) -> Vec<ProposedInstance> {
    let mut drop: HashSet<usize> = HashSet::new();
    for (class, entries) in proposed_vecs.iter() {
        let Some(existing) = existing_vecs.get(class) else {
            continue;
        };
        if existing.is_empty() {
            continue;
        }
        for (idx, pv) in entries {
            let max_sim = existing
                .iter()
                .map(|ev| cosine_similarity(pv, ev))
                .fold(f32::MIN, f32::max);
            if max_sim >= threshold {
                log::debug!(
                    "interpretation: semantic-dedup dropping {} #{} (sim {:.3} ≥ {:.3})",
                    class,
                    idx,
                    max_sim,
                    threshold
                );
                drop.insert(*idx);
            }
        }
    }
    instances
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !drop.contains(i))
        .map(|(_, inst)| inst)
        .collect()
}

/// Semantic-dedup filter with live HTTP calls: for each class that has both
/// proposals and existing identities, batch-embed both sides in one
/// round-trip, then delegate to [`semantic_dedup_pure`]. Proposals carrying an
/// `id` (explicit upsert) bypass dedup, same as the string path. Classes with
/// no identity property, or proposals missing that property's value, always
/// survive.
pub async fn filter_already_present_semantic(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
    identity_props: &HashMap<String, String>,
    model: &str,
    threshold: f32,
) -> anyhow::Result<Vec<ProposedInstance>> {
    // Bucket proposal indices by class, but only those subject to dedup.
    let mut per_class: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    for (i, inst) in instances.iter().enumerate() {
        if inst.id.is_some() {
            continue;
        }
        let Some(idp_name) = identity_props.get(&inst.class) else {
            continue;
        };
        let Some(value) = inst.props.get(idp_name).and_then(|v| v.as_str()) else {
            continue;
        };
        per_class
            .entry(inst.class.clone())
            .or_default()
            .push((i, value.to_string()));
    }

    let mut existing_vecs: HashMap<String, Vec<Vec<f32>>> = HashMap::new();
    let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
    for (class, entries) in per_class.iter() {
        let existing_vals: Vec<String> = existing
            .get(class)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|s| !s.trim().is_empty())
            .collect();
        if existing_vals.is_empty() {
            continue;
        }
        let proposed_vals: Vec<String> = entries.iter().map(|(_, v)| v.clone()).collect();
        let mut batch = existing_vals.clone();
        batch.extend(proposed_vals.iter().cloned());
        let vectors = embed_via_ai_service(model, &batch).await?;
        let (ev, pv) = vectors.split_at(existing_vals.len());
        existing_vecs.insert(class.clone(), ev.to_vec());
        let pv_entries: Vec<(usize, Vec<f32>)> = entries
            .iter()
            .zip(pv.iter())
            .map(|((i, _), v)| (*i, v.clone()))
            .collect();
        proposed_vecs.insert(class.clone(), pv_entries);
    }

    Ok(semantic_dedup_pure(
        instances,
        &existing_vecs,
        &proposed_vecs,
        threshold,
    ))
}

/// Strategy dispatcher: pick the string or semantic dedup path based on
/// `strategy`, preserving [`ProposedInstance`] order in both cases so
/// downstream `new:<Class>:<n>` ordinals still line up.
pub async fn filter_already_present_with_strategy(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
    identity_props: &HashMap<String, String>,
    strategy: &DedupStrategy,
) -> anyhow::Result<Vec<ProposedInstance>> {
    match strategy {
        DedupStrategy::NormalizedString => {
            Ok(filter_already_present(instances, existing, identity_props))
        }
        DedupStrategy::Semantic { model, threshold } => {
            filter_already_present_semantic(instances, existing, identity_props, model, *threshold)
                .await
        }
    }
}
