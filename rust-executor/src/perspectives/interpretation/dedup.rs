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
    /// Embed each identity string via an Ollama-compatible embeddings endpoint
    /// (both `/v1/embeddings` OpenAI-compat and `/api/embeddings` native are
    /// verified against the Marvin dev tunnel; we use the OpenAI-compat path).
    /// Proposals whose maximum cosine similarity to any existing identity in
    /// the same class meets or exceeds `threshold` are treated as duplicates
    /// and dropped.
    Semantic {
        /// Base URL of the embeddings endpoint, without the `/embeddings`
        /// suffix (e.g. `http://localhost:11434/v1`).
        base_url: String,
        /// Model tag, e.g. `nomic-embed-text`.
        model: String,
        /// Cosine similarity above which two identity strings are considered
        /// the same instance. Sensible starting point: `0.85` for
        /// `nomic-embed-text`.
        threshold: f32,
    },
}

impl Default for DedupStrategy {
    fn default() -> Self {
        Self::NormalizedString
    }
}

impl DedupStrategy {
    /// A `Semantic` strategy pre-populated from env: `INTERPRETATION_EMBED_BASE_URL`
    /// (default `http://localhost:11434/v1`), `INTERPRETATION_EMBED_MODEL`
    /// (default `nomic-embed-text`), and a caller-provided threshold. Useful for
    /// e2e tests and the future AutoProcessor default builder.
    pub fn semantic_from_env(threshold: f32) -> Self {
        let base_url = std::env::var("INTERPRETATION_EMBED_BASE_URL")
            .unwrap_or_else(|_| "http://localhost:11434/v1".to_string());
        let model = std::env::var("INTERPRETATION_EMBED_MODEL")
            .unwrap_or_else(|_| "nomic-embed-text".to_string());
        Self::Semantic {
            base_url,
            model,
            threshold,
        }
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

/// OpenAI-compatible embeddings client (`POST {base_url}/embeddings`). Works
/// against Ollama's `/v1/embeddings` endpoint and any other OpenAI-shaped
/// provider. `input` is sent as an array so one HTTP round-trip covers all
/// candidates for a given class.
pub async fn embed_openai_compat(
    base_url: &str,
    model: &str,
    texts: &[String],
) -> anyhow::Result<Vec<Vec<f32>>> {
    if texts.is_empty() {
        return Ok(Vec::new());
    }
    #[derive(serde::Serialize)]
    struct Req<'a> {
        model: &'a str,
        input: &'a [String],
    }
    #[derive(serde::Deserialize)]
    struct Row {
        embedding: Vec<f32>,
    }
    #[derive(serde::Deserialize)]
    struct Resp {
        data: Vec<Row>,
    }
    let url = format!("{}/embeddings", base_url.trim_end_matches('/'));
    let client = reqwest::Client::new();
    let resp = client
        .post(&url)
        .json(&Req {
            model,
            input: texts,
        })
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("embed_openai_compat POST {url}: {e:#}"))?
        .error_for_status()
        .map_err(|e| anyhow::anyhow!("embed_openai_compat {url}: {e:#}"))?
        .json::<Resp>()
        .await
        .map_err(|e| anyhow::anyhow!("embed_openai_compat {url}: bad JSON: {e:#}"))?;
    if resp.data.len() != texts.len() {
        return Err(anyhow::anyhow!(
            "embed_openai_compat {url}: expected {} embeddings, got {}",
            texts.len(),
            resp.data.len()
        ));
    }
    Ok(resp.data.into_iter().map(|r| r.embedding).collect())
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
    base_url: &str,
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
        let vectors = embed_openai_compat(base_url, model, &batch).await?;
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
        DedupStrategy::Semantic {
            base_url,
            model,
            threshold,
        } => {
            filter_already_present_semantic(
                instances,
                existing,
                identity_props,
                base_url,
                model,
                *threshold,
            )
            .await
        }
    }
}
