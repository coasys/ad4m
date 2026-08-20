use super::graph::{instances_by_class, normalize_identity};
use super::types::{ExistingInstances, ProposedInstance};
use std::collections::HashMap;

/// How identity-dedup resolved one proposed instance, while **keeping its slot**
/// in the LLM's emission order so relation ordinals (`new:<Class>:<n>`) still
/// line up. Dedup used to *drop* duplicates before the write path planned
/// ordinals, so a removed earlier sibling shifted every later `new:<Class>:<n>`
/// (James #883). Instead we tag every item and let
/// [`plan_interpretation_ops_resolved`](super::graph::plan_interpretation_ops_resolved)
/// index all of them — writing ops only for the kept ones.
#[derive(Debug, Clone, PartialEq)]
pub enum Resolution {
    /// A genuinely new proposal, or an explicit upsert carrying a *trusted* id:
    /// the planner emits its Create/Update op.
    Keep,
    /// Its (class, identity) already exists in the graph. Not re-created; a
    /// relation ref pointing at this slot resolves to `existing_base` (the real
    /// node) rather than a fresh mint.
    DupOfExisting(String),
    /// It duplicates an *earlier* proposal in THIS same response (index into the
    /// emission order). Not written; refs resolve to that earlier slot's base.
    DupOfEarlier(usize),
}

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
#[cfg(test)]
pub(crate) fn semantic_dedup_pure(
    instances: Vec<ProposedInstance>,
    existing_vecs: &HashMap<String, Vec<Vec<f32>>>,
    proposed_vecs: &HashMap<String, Vec<(usize, Vec<f32>)>>,
    threshold: f32,
) -> Vec<ProposedInstance> {
    use std::collections::HashSet;
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

/// PURE argmax companion to [`semantic_dedup_pure`]: for each proposed index
/// whose max cosine similarity to an existing vector of the same class is
/// `≥ threshold`, return that best-matching existing vector's *local index*
/// (position within the class's `existing_vecs` list). Lets the caller map a
/// semantic duplicate back to the real existing base for
/// [`Resolution::DupOfExisting`].
pub(crate) fn semantic_dup_targets(
    existing_vecs: &HashMap<String, Vec<Vec<f32>>>,
    proposed_vecs: &HashMap<String, Vec<(usize, Vec<f32>)>>,
    threshold: f32,
) -> HashMap<usize, usize> {
    let mut targets: HashMap<usize, usize> = HashMap::new();
    for (class, entries) in proposed_vecs.iter() {
        let Some(existing) = existing_vecs.get(class) else {
            continue;
        };
        if existing.is_empty() {
            continue;
        }
        for (idx, pv) in entries {
            let mut best_sim = f32::MIN;
            let mut best_j = 0usize;
            for (j, ev) in existing.iter().enumerate() {
                let sim = cosine_similarity(pv, ev);
                if sim > best_sim {
                    best_sim = sim;
                    best_j = j;
                }
            }
            if best_sim >= threshold {
                targets.insert(*idx, best_j);
            }
        }
    }
    targets
}

/// PURE intra-response companion to [`semantic_dup_targets`] — the semantic
/// analogue of the earlier-proposal pass in [`resolve_already_present`]. Walks
/// each class's proposals in emission order and, for every proposal that is
/// *not* already a duplicate of a pre-existing graph instance (`existing_dup`,
/// the output of [`semantic_dup_targets`]), checks whether it semantically
/// duplicates an EARLIER **kept** proposal of the same class (max cosine
/// `≥ threshold`). Returns a map proposal-index → that earlier kept proposal's
/// index, for [`Resolution::DupOfEarlier`].
///
/// Mirrors [`resolve_already_present`]'s precedence exactly: a pre-existing
/// match wins over an in-response one (so `existing_dup` indices are skipped and
/// never become dup targets), and only proposals that resolve to `Keep` become
/// dedup targets for later ones — a chain of near-duplicates all point back at
/// the first kept occurrence. `proposed_vecs` entries are per-class and already
/// in ascending (emission) index order, so a plain walk is the emission order.
pub(crate) fn semantic_dup_earlier(
    proposed_vecs: &HashMap<String, Vec<(usize, Vec<f32>)>>,
    existing_dup: &HashMap<usize, usize>,
    threshold: f32,
) -> HashMap<usize, usize> {
    let mut earlier: HashMap<usize, usize> = HashMap::new();
    for entries in proposed_vecs.values() {
        // (index, vec) of proposals that resolved to Keep so far in this class,
        // in emission order — the candidates a later proposal can duplicate.
        let mut kept: Vec<(usize, &Vec<f32>)> = Vec::new();
        for (idx, pv) in entries {
            // A pre-existing graph match takes precedence and is not a Keep, so
            // it is neither an earlier-dup itself nor a target for later ones.
            if existing_dup.contains_key(idx) {
                continue;
            }
            let mut best_sim = f32::MIN;
            let mut best_idx = 0usize;
            for (kidx, kv) in kept.iter() {
                let sim = cosine_similarity(pv, kv);
                // `>` keeps the earliest kept occurrence on a tie (kept is in
                // emission order), matching the string path's first-writer-wins.
                if sim > best_sim {
                    best_sim = sim;
                    best_idx = *kidx;
                }
            }
            if best_sim >= threshold {
                earlier.insert(*idx, best_idx);
            } else {
                kept.push((*idx, pv));
            }
        }
    }
    earlier
}

/// Semantic-dedup filter backed by AIService embeddings — thin wrapper over
/// [`resolve_already_present_semantic`] keeping only [`Resolution::Keep`]
/// instances (the historical contract).
pub async fn filter_already_present_semantic(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
    model: &str,
    threshold: f32,
) -> anyhow::Result<Vec<ProposedInstance>> {
    Ok(
        resolve_already_present_semantic(instances, existing, identity_props, model, threshold)
            .await?
            .into_iter()
            .filter_map(|(inst, r)| matches!(r, Resolution::Keep).then_some(inst))
            .collect(),
    )
}

/// Tagging version of the semantic path (see [`resolve_already_present`] for the
/// string equivalent): for each class with both proposals and existing
/// identities, embed both sides (one `AIService::embed` call per string — the
/// embedding channel is single-prompt; a batch API would be a worthwhile future
/// optimisation) and tag each proposal `Keep` / `DupOfExisting(real base)`.
/// Proposals carrying a *trusted* `id` (an explicit upsert) bypass dedup, same
/// as the string path; a hallucinated id is still checked. Classes with no
/// identity property, or proposals missing that property's value, always
/// survive. Like the string path it also runs an intra-response pass: a later
/// proposal semantically duplicating an EARLIER kept proposal of the same class
/// (cosine `≥ threshold`) is tagged `DupOfEarlier(earlier index)`, with a
/// pre-existing graph match taking precedence over an in-response one.
pub async fn resolve_already_present_semantic(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
    model: &str,
    threshold: f32,
) -> anyhow::Result<Vec<(ProposedInstance, Resolution)>> {
    // Per-class (base id, identity value) rows so a semantic match resolves back
    // to the real base, kept parallel to the embedded existing vectors.
    let mut existing_rows_by_class: HashMap<String, Vec<(String, String)>> = HashMap::new();
    for (class, rows) in instances_by_class(existing) {
        existing_rows_by_class.insert(
            class,
            rows.into_iter()
                .map(|r| (r.id.clone(), r.title.clone()))
                .collect(),
        );
    }
    // Bucket proposal indices by class, but only those subject to dedup.
    let mut per_class: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    for (i, inst) in instances.iter().enumerate() {
        // Only a *trusted* id (one the graph actually holds) is a real upsert
        // target that bypasses dedup; a hallucinated id must still be checked
        // (see `filter_already_present`).
        if inst
            .id
            .as_deref()
            .is_some_and(|id| existing.contains_key(id))
        {
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
    let mut existing_ids: HashMap<String, Vec<String>> = HashMap::new();
    let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
    for (class, entries) in per_class.iter() {
        // Filter empties from (id, value) rows together so ids stay parallel to
        // the vectors we embed.
        let filtered: Vec<(String, String)> = existing_rows_by_class
            .get(class)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|(_, title)| !title.trim().is_empty())
            .collect();
        if filtered.is_empty() {
            continue;
        }
        let existing_vals: Vec<String> = filtered.iter().map(|(_, v)| v.clone()).collect();
        let ids: Vec<String> = filtered.iter().map(|(id, _)| id.clone()).collect();
        let proposed_vals: Vec<String> = entries.iter().map(|(_, v)| v.clone()).collect();
        let mut batch = existing_vals.clone();
        batch.extend(proposed_vals.iter().cloned());
        let vectors = embed_via_ai_service(model, &batch).await?;
        let (ev, pv) = vectors.split_at(existing_vals.len());
        existing_vecs.insert(class.clone(), ev.to_vec());
        existing_ids.insert(class.clone(), ids);
        let pv_entries: Vec<(usize, Vec<f32>)> = entries
            .iter()
            .zip(pv.iter())
            .map(|((i, _), v)| (*i, v.clone()))
            .collect();
        proposed_vecs.insert(class.clone(), pv_entries);
    }

    // Precedence mirrors `resolve_already_present`: a pre-existing graph match
    // (`targets`) beats an earlier-in-response one (`earlier`), which beats Keep.
    let targets = semantic_dup_targets(&existing_vecs, &proposed_vecs, threshold);
    let earlier = semantic_dup_earlier(&proposed_vecs, &targets, threshold);
    let out = instances
        .into_iter()
        .enumerate()
        .map(|(i, inst)| {
            if let Some(&existing_local_idx) = targets.get(&i) {
                match existing_ids
                    .get(&inst.class)
                    .and_then(|v| v.get(existing_local_idx))
                    .cloned()
                {
                    Some(base) => (inst, Resolution::DupOfExisting(base)),
                    // Defensive: index parity is maintained above, so this
                    // shouldn't fire — keep the item rather than lose it.
                    None => (inst, Resolution::Keep),
                }
            } else if let Some(&earlier_idx) = earlier.get(&i) {
                (inst, Resolution::DupOfEarlier(earlier_idx))
            } else {
                (inst, Resolution::Keep)
            }
        })
        .collect();
    Ok(out)
}

/// Strategy dispatcher: pick the string or semantic dedup path based on
/// `strategy`, preserving [`ProposedInstance`] order in both cases so
/// downstream `new:<Class>:<n>` ordinals still line up.
pub async fn filter_already_present_with_strategy(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
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

/// Tagging dispatcher — the ordinal-preserving counterpart to
/// [`filter_already_present_with_strategy`]. Returns every proposal in emission
/// order tagged with its [`Resolution`], which the planner needs so
/// `new:<Class>:<n>` ordinals resolve against the model's *full* output even
/// after duplicates are excluded from the write set. This is what
/// `run_interpretation` uses.
pub async fn resolve_already_present_with_strategy(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
    strategy: &DedupStrategy,
) -> anyhow::Result<Vec<(ProposedInstance, Resolution)>> {
    match strategy {
        DedupStrategy::NormalizedString => {
            Ok(resolve_already_present(instances, existing, identity_props))
        }
        DedupStrategy::Semantic { model, threshold } => {
            resolve_already_present_semantic(instances, existing, identity_props, model, *threshold)
                .await
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;
    use std::collections::{BTreeMap, HashMap};

    #[test]
    fn filter_already_present_drops_known_titles() {
        // Tasks declare `title` as their identity. Four Tasks proposed: one
        // duplicates an existing title (case-insensitive), one duplicates it under
        // whitespace normalization, one is new, and a same-title item of a
        // DIFFERENT class (which has no identity here) is untouched (dedup is per
        // class, and only for classes with a declared identity).
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  ship   the   mvp  "},
              {"class":"Task","title":"Write the docs"},
              {"class":"Belief","title":"ship the mvp"}
            ]"#,
        )
        .unwrap();
        let existing = existing_by_identity(&[("Task", "ship the MVP")]);
        // Only Task declares `title` as its identity; Belief has none ⇒ no dedup.
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let kept = filter_already_present(proposed, &existing, &identity_props);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert!(
            !kept_titles.contains(&"Ship the MVP"),
            "existing Task title must be dropped (case-insensitive); got {kept_titles:?}"
        );
        assert!(
            !kept_titles.contains(&"  ship   the   mvp  "),
            "whitespace-normalized duplicate Task title must be dropped; got {kept_titles:?}"
        );
        assert!(
            kept_titles.contains(&"Write the docs"),
            "new Task must survive"
        );
        assert!(
        kept_titles.contains(&"ship the mvp"),
        "same title on a class with no declared identity must NOT be dropped; got {kept_titles:?}"
    );
    }

    #[test]
    fn resolve_already_present_tags_existing_dups_at_their_base() {
        // James #883: dedup must TAG (not drop) so ordinals survive. Emission
        // order [Alpha(new), Ship(existing), ship(existing again), Beta(new)]:
        // both Ship copies resolve to the existing base (a pre-existing graph
        // match beats an in-response one), length is preserved.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Alpha"},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  ship   the   mvp  "},
              {"class":"Task","title":"Beta"}
            ]"#,
        )
        .unwrap();
        let existing = existing_map(vec![InstanceContext {
            id: "soa://existing/task/1".to_string(),
            title: "Ship the MVP".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }]);
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let tagged = resolve_already_present(proposed, &existing, &identity_props);
        let res: Vec<Resolution> = tagged.into_iter().map(|(_, r)| r).collect();
        assert_eq!(res.len(), 4, "length preserved — nothing dropped");
        assert_eq!(res[0], Resolution::Keep);
        assert_eq!(
            res[1],
            Resolution::DupOfExisting("soa://existing/task/1".to_string())
        );
        assert_eq!(
            res[2],
            Resolution::DupOfExisting("soa://existing/task/1".to_string()),
            "a second copy of an already-present identity still points at the real node"
        );
        assert_eq!(res[3], Resolution::Keep);
    }

    #[test]
    fn resolve_already_present_flags_intra_response_dup_as_dup_of_earlier() {
        // Two copies of a NOT-yet-existing identity in one response: the first is
        // Keep, the later normalized-equal copy is DupOfEarlier(first index) — so
        // it keeps its ordinal slot pointing at the first occurrence's base.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Write the docs"},
              {"class":"Task","title":"  WRITE  the  docs "},
              {"class":"Task","title":"Ship"}
            ]"#,
        )
        .unwrap();
        let existing = ExistingInstances::new();
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let tagged = resolve_already_present(proposed, &existing, &identity_props);
        let res: Vec<Resolution> = tagged.into_iter().map(|(_, r)| r).collect();
        assert_eq!(res[0], Resolution::Keep);
        assert_eq!(
            res[1],
            Resolution::DupOfEarlier(0),
            "normalized-equal later copy dups the earlier proposal"
        );
        assert_eq!(res[2], Resolution::Keep);
    }

    #[test]
    fn cosine_similarity_orthogonal_and_parallel() {
        // Sanity floor for the semantic dedup: parallel vectors → 1, opposite → -1,
        // orthogonal → 0, zero → 0 (not NaN, so threshold comparisons still work
        // without a special case in the filter).
        let a = vec![1.0f32, 0.0, 0.0];
        let b = vec![1.0f32, 0.0, 0.0];
        assert!((cosine_similarity(&a, &b) - 1.0).abs() < 1e-6);
        let c = vec![-1.0f32, 0.0, 0.0];
        assert!((cosine_similarity(&a, &c) - (-1.0)).abs() < 1e-6);
        let d = vec![0.0f32, 1.0, 0.0];
        assert!(cosine_similarity(&a, &d).abs() < 1e-6);
        let z = vec![0.0f32; 3];
        assert_eq!(cosine_similarity(&a, &z), 0.0);
    }

    #[test]
    fn semantic_dedup_pure_drops_near_duplicate_keeps_distinct() {
        // The core invariant of the semantic dedup: a proposal whose embedding is
        // near-parallel to an existing one (in this class) is dropped; one that
        // isn't survives. Order of the survivors is preserved so relation
        // `new:<Class>:<n>` ordinals still line up downstream. This test uses
        // hand-crafted vectors so it exercises the filter without an HTTP round
        // trip — the stub "embedder" is just the vectors we pass in.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Ship MVP"},
              {"class":"Task","title":"Write the docs"}
            ]"#,
        )
        .unwrap();
        // Existing vector for the only existing "Ship the MVP" task.
        let mut existing_vecs: HashMap<String, Vec<Vec<f32>>> = HashMap::new();
        existing_vecs.insert("Task".to_string(), vec![vec![1.0, 0.0, 0.0]]);
        // Proposed vectors keyed by their index in `proposed`.
        let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
        proposed_vecs.insert(
            "Task".to_string(),
            vec![
                (0, vec![1.0, 0.0, 0.0]),  // identical → sim 1.0, dropped
                (1, vec![0.98, 0.2, 0.0]), // very close → sim ≈ 0.98, dropped
                (2, vec![0.0, 1.0, 0.0]),  // orthogonal → sim 0, kept
            ],
        );
        let kept = semantic_dedup_pure(proposed, &existing_vecs, &proposed_vecs, 0.85);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert_eq!(
            kept_titles,
            vec!["Write the docs"],
            "only the semantically distinct proposal survives; got {kept_titles:?}"
        );
    }

    #[test]
    fn semantic_dedup_pure_preserves_order_and_upsert() {
        // Mirrors `filter_already_present_keeps_upserts_and_preserves_order` for
        // the semantic path: proposals with an `id` are treated by the caller
        // (`filter_already_present_semantic`) as upsert targets and never appear
        // in `proposed_vecs`, so `semantic_dedup_pure` never drops them.
        // Distinct titles surrounding a near-dup keep their relative order.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Alpha"},
              {"class":"Task","id":"soa://existing/task/1","title":"Ship the MVP"},
              {"class":"Task","title":"Ship MVP"},
              {"class":"Task","title":"Omega"}
            ]"#,
        )
        .unwrap();
        let mut existing_vecs: HashMap<String, Vec<Vec<f32>>> = HashMap::new();
        existing_vecs.insert("Task".to_string(), vec![vec![1.0, 0.0, 0.0]]);
        // Only indices 0, 2, 3 are subject to dedup (index 1 has an `id`, so the
        // outer caller skips it — mirror that here by NOT putting it in the map).
        let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
        proposed_vecs.insert(
            "Task".to_string(),
            vec![
                (0, vec![0.0, 1.0, 0.0]),  // Alpha — orthogonal, kept
                (2, vec![0.98, 0.2, 0.0]), // near-dup of existing, dropped
                (3, vec![0.0, 0.0, 1.0]),  // Omega — orthogonal, kept
            ],
        );
        let kept = semantic_dedup_pure(proposed, &existing_vecs, &proposed_vecs, 0.85);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert_eq!(
            kept_titles,
            vec!["Alpha", "Ship the MVP", "Omega"],
            "upsert not touched, near-dup dropped, order preserved"
        );
        assert_eq!(
            kept[1].id.as_deref(),
            Some("soa://existing/task/1"),
            "surviving Ship the MVP is the id-carrying upsert"
        );
    }

    #[test]
    fn semantic_dup_earlier_flags_intra_response_dup_as_dup_of_earlier() {
        // Semantic analogue of `resolve_already_present_flags_intra_response_dup_as_dup_of_earlier`:
        // [A(new), B(sem-dup-of-A), C(new)] with no existing graph vectors → B
        // duplicates the earlier kept A (cosine ≥ threshold) so it maps to
        // DupOfEarlier(0); A and C are kept. Hand-built vectors exercise the pure
        // pass without an embedding round-trip.
        let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
        proposed_vecs.insert(
            "Task".to_string(),
            vec![
                (0, vec![1.0f32, 0.0, 0.0]),  // A
                (1, vec![0.98f32, 0.2, 0.0]), // B ≈ A → sim ≈ 0.98
                (2, vec![0.0f32, 1.0, 0.0]),  // C orthogonal to A
            ],
        );
        let no_existing: HashMap<usize, usize> = HashMap::new();
        let earlier = semantic_dup_earlier(&proposed_vecs, &no_existing, 0.85);
        assert_eq!(
            earlier.get(&1),
            Some(&0),
            "B semantically dups the earlier kept A"
        );
        assert_eq!(earlier.get(&0), None, "A is the first occurrence — kept");
        assert_eq!(
            earlier.get(&2),
            None,
            "C is orthogonal to the only kept proposal — kept"
        );
    }

    #[test]
    fn semantic_dup_earlier_matches_string_path_precedence() {
        // Mixed DupOfExisting + DupOfEarlier, proving the semantic tagging's
        // precedence is byte-for-byte the string path's. Same emission shape on
        // both sides: [Ship(existing), Ship(existing), Write(new), Write(dup)].
        // The string side resolves against a real graph + normalized titles; the
        // semantic side against hand-built vectors within threshold. Both must
        // yield [DupOfExisting(base), DupOfExisting(base), Keep, DupOfEarlier(2)].
        let base = "soa://existing/task/1".to_string();
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        // --- string path (ground truth) ---
        let proposed_str = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Write the docs"},
              {"class":"Task","title":"Write the docs"}
            ]"#,
        )
        .unwrap();
        let existing = existing_map(vec![InstanceContext {
            id: base.clone(),
            title: "Ship the MVP".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }]);
        let string_res: Vec<Resolution> =
            resolve_already_present(proposed_str, &existing, &identity_props)
                .into_iter()
                .map(|(_, r)| r)
                .collect();

        // --- semantic path via the pure fns, mirroring `resolve_already_present_semantic` ---
        let mut existing_vecs: HashMap<String, Vec<Vec<f32>>> = HashMap::new();
        existing_vecs.insert("Task".to_string(), vec![vec![1.0, 0.0, 0.0]]); // Ship the MVP
        let mut existing_ids: HashMap<String, Vec<String>> = HashMap::new();
        existing_ids.insert("Task".to_string(), vec![base.clone()]);
        let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
        proposed_vecs.insert(
            "Task".to_string(),
            vec![
                (0, vec![1.0, 0.0, 0.0]),  // Ship — dup of existing
                (1, vec![0.99, 0.1, 0.0]), // Ship — dup of existing
                (2, vec![0.0, 1.0, 0.0]),  // Write — new
                (3, vec![0.0, 0.99, 0.1]), // Write — dup of earlier #2
            ],
        );
        let threshold = 0.85;
        let targets = semantic_dup_targets(&existing_vecs, &proposed_vecs, threshold);
        let earlier = semantic_dup_earlier(&proposed_vecs, &targets, threshold);
        let semantic_res: Vec<Resolution> = (0..4)
            .map(|i| {
                if let Some(&j) = targets.get(&i) {
                    Resolution::DupOfExisting(existing_ids["Task"][j].clone())
                } else if let Some(&e) = earlier.get(&i) {
                    Resolution::DupOfEarlier(e)
                } else {
                    Resolution::Keep
                }
            })
            .collect();

        assert_eq!(
            semantic_res,
            vec![
                Resolution::DupOfExisting(base.clone()),
                Resolution::DupOfExisting(base.clone()),
                Resolution::Keep,
                Resolution::DupOfEarlier(2),
            ],
            "existing match beats in-response; later Write dups the earlier kept one"
        );
        assert_eq!(
            semantic_res, string_res,
            "semantic tagging precedence must match the string path exactly"
        );
    }

    #[tokio::test]
    async fn dispatcher_normalized_string_matches_direct_call() {
        // `filter_already_present_with_strategy(..., NormalizedString)` must
        // behave identically to calling `filter_already_present` directly — the
        // default path stays byte-for-byte compatible with existing callers.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  ship   the   mvp  "},
              {"class":"Task","title":"Write the docs"}
            ]"#,
        )
        .unwrap();
        let existing = existing_by_identity(&[("Task", "ship the MVP")]);
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let via_dispatcher = filter_already_present_with_strategy(
            proposed.clone(),
            &existing,
            &identity_props,
            &DedupStrategy::default(),
        )
        .await
        .unwrap();
        let via_direct = filter_already_present(proposed, &existing, &identity_props);
        let d_titles: Vec<&str> = via_dispatcher
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        let s_titles: Vec<&str> = via_direct
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert_eq!(d_titles, s_titles);
        assert_eq!(d_titles, vec!["Write the docs"]);
    }

    #[test]
    fn filter_already_present_keeps_upserts_and_preserves_order() {
        // An `id`-carrying proposal is an explicit upsert target: its title
        // deliberately matches an existing one, so dedup must never drop it. And
        // filtering happens IN PLACE — the surviving order is the LLM's emission
        // order, which is what `new:<Class>:<n>` ordinals count against. Both
        // properties together are what make relation ordinals resolve correctly
        // after a dedup pass.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Alpha"},
              {"class":"Task","id":"soa://existing/task/1","title":"Ship the MVP"},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Omega"}
            ]"#,
        )
        .unwrap();
        // The existing instance carries both its id (the trusted upsert target) and
        // its title (the dedup identity) — one row in the single-source map.
        let existing = existing_map(vec![InstanceContext {
            id: "soa://existing/task/1".to_string(),
            title: "Ship the MVP".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }]);
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let kept = filter_already_present(proposed, &existing, &identity_props);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert_eq!(
            kept_titles,
            vec!["Alpha", "Ship the MVP", "Omega"],
            "upsert survives, plain duplicate is dropped, order preserved"
        );
        assert_eq!(
            kept[1].id.as_deref(),
            Some("soa://existing/task/1"),
            "the surviving 'Ship the MVP' must be the id-carrying upsert"
        );
    }

    #[test]
    fn filter_already_present_dedups_hallucinated_id_but_keeps_trusted() {
        // A proposal may carry an `id` the model invented. The planner only routes
        // to Update for ids the graph actually holds (`known_existing_ids`); an
        // untrusted id goes to Create — so it must still be dedup-checked, or a
        // made-up id + duplicate identity mints a duplicate node. A *trusted* id is
        // a real upsert target and bypasses dedup.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","id":"soa://hallucinated/999","title":"Ship the MVP"}
            ]"#,
        )
        .unwrap();
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        // Untrusted id → the graph does not hold `soa://hallucinated/999`, only a
        // same-title Task under a different id, so the proposal is deduped away.
        let existing_untrusted = existing_by_identity(&[("Task", "Ship the MVP")]);
        let dropped =
            filter_already_present(proposed.clone(), &existing_untrusted, &identity_props);
        assert!(
            dropped.is_empty(),
            "hallucinated id + duplicate identity must be deduped, not minted; got {dropped:#?}"
        );

        // Same proposal, but now the graph actually holds that id (title matches) →
        // it is a real upsert target and bypasses dedup.
        let existing_trusted = existing_map(vec![InstanceContext {
            id: "soa://hallucinated/999".to_string(),
            title: "Ship the MVP".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }]);
        let kept = filter_already_present(proposed, &existing_trusted, &identity_props);
        assert_eq!(kept.len(), 1, "a trusted id bypasses dedup; got {kept:#?}");
    }

    #[test]
    fn filter_already_present_dedupes_within_same_response() {
        // The LLM sometimes emits the same (class, identity) twice in one response
        // (verbatim, or under whitespace/case variation). Without intra-response
        // dedup those slip past `filter_already_present` because the pre-existing
        // `known` set does not yet contain them — and `run_interpretation` then
        // mints two subjects for the same identity. Fix: accumulate accepted
        // identities as we scan the response, dropping later same-key proposals
        // exactly like already-persisted ones.
        let proposed = parse_interpretation_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  SHIP  the  mvp  "},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Write the docs"}
            ]"#,
        )
        .unwrap();
        let existing = ExistingInstances::new(); // graph empty
        let mut identity_props = HashMap::new();
        identity_props.insert("Task".to_string(), "title".to_string());

        let kept = filter_already_present(proposed, &existing, &identity_props);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();

        // First occurrence wins; every subsequent normalized-equal proposal drops.
        assert_eq!(
        kept_titles,
        vec!["Ship the MVP", "Write the docs"],
        "intra-response duplicates must be dropped after the first occurrence; got {kept_titles:?}"
    );
    }
}

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, identity-value) already exists in the graph, compared under
/// [`normalize_identity`]. This is the hard guarantee behind the soft
/// `existing` hint in [`build_interpretation_input`] — even if the model
/// re-proposes a known item, it never becomes a new instance.
///
/// This is the **`DedupStrategy::NormalizedString`** implementation, not a
/// separate/legacy path: `run_interpretation` always dedups through
/// [`filter_already_present_with_strategy`](super::filter_already_present_with_strategy),
/// which calls this for the (default) string strategy and the embedding path
/// for `DedupStrategy::Semantic`. No production caller invokes this directly.
///
/// `existing` is the id-keyed [`ExistingInstances`] source of truth; the
/// per-class identity values are projected from it here. `identity_props` maps
/// a class's local name to the NAME of its declared identity property. An
/// instance whose class has no identity property is always kept (no dedup);
/// likewise one missing that property's value.
///
/// Filters **in place**: the surviving instances keep the LLM's emission order,
/// which is what the `new:<Class>:<n>` relation ordinals in
/// [`plan_interpretation_ops_with_context`] resolve against.
///
/// Thin wrapper over [`resolve_already_present`] that keeps only the
/// [`Resolution::Keep`] instances — the historical contract. New callers that
/// need ordinals to survive dedup use `resolve_already_present` directly.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
) -> Vec<ProposedInstance> {
    resolve_already_present(instances, existing, identity_props)
        .into_iter()
        .filter_map(|(inst, r)| matches!(r, Resolution::Keep).then_some(inst))
        .collect()
}

/// Tagging core behind [`filter_already_present`]: classify every proposed
/// instance as [`Resolution::Keep`] / `DupOfExisting` / `DupOfEarlier` while
/// preserving the input order **and length** — nothing is dropped. This is what
/// lets the planner index EVERY emitted item for `new:<Class>:<n>` ordinals
/// even though duplicates never get written (James #883: dedup that *removes*
/// items shifts later ordinals).
///
/// - A *trusted* id (one the graph holds) is an explicit upsert → `Keep`.
/// - A class with no declared identity, or a proposal missing that identity's
///   value, can't be deduped → `Keep`.
/// - Otherwise, under [`normalize_identity`]: a match against a pre-existing
///   graph instance → `DupOfExisting(that base)`; a match against an *earlier*
///   kept proposal in this response → `DupOfEarlier(its index)`; else `Keep`
///   (and remembered so a later copy dedups against it).
pub fn resolve_already_present(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
) -> Vec<(ProposedInstance, Resolution)> {
    // Pre-existing graph identities → the base id that owns them, per class,
    // under identity normalization. First writer of a normalized value wins.
    let mut existing_norm_to_id: HashMap<String, HashMap<String, String>> = HashMap::new();
    for (class, rows) in instances_by_class(existing) {
        let entry = existing_norm_to_id.entry(class).or_default();
        for row in rows {
            entry
                .entry(normalize_identity(&row.title))
                .or_insert_with(|| row.id.clone());
        }
    }
    // (class, normalized identity) → index of the first KEPT proposal in this
    // response, so a later same-key proposal becomes `DupOfEarlier`.
    let mut seen_in_response: HashMap<String, HashMap<String, usize>> = HashMap::new();
    let mut out = Vec::with_capacity(instances.len());
    for (idx, inst) in instances.into_iter().enumerate() {
        // Trusted id (graph holds it) = explicit upsert → keep it verbatim.
        if inst
            .id
            .as_deref()
            .is_some_and(|id| existing.contains_key(id))
        {
            out.push((inst, Resolution::Keep));
            continue;
        }
        let Some(idp_name) = identity_props.get(&inst.class) else {
            out.push((inst, Resolution::Keep));
            continue;
        };
        let Some(value) = inst.props.get(idp_name).and_then(|v| v.as_str()) else {
            out.push((inst, Resolution::Keep));
            continue;
        };
        let normalized = normalize_identity(value);
        // Prefer a pre-existing graph match (a stable base) over an earlier
        // in-response one — a second copy of an already-present identity should
        // still point at the real node.
        if let Some(existing_id) = existing_norm_to_id
            .get(&inst.class)
            .and_then(|m| m.get(&normalized))
            .cloned()
        {
            log::debug!(
                "interpretation: already-present {} '{}' → dup of {}",
                inst.class,
                value,
                existing_id
            );
            out.push((inst, Resolution::DupOfExisting(existing_id)));
            continue;
        }
        if let Some(first_idx) = seen_in_response
            .get(&inst.class)
            .and_then(|m| m.get(&normalized))
            .copied()
        {
            log::debug!(
                "interpretation: {} '{}' duplicates earlier proposal #{first_idx}",
                inst.class,
                value,
            );
            out.push((inst, Resolution::DupOfEarlier(first_idx)));
            continue;
        }
        seen_in_response
            .entry(inst.class.clone())
            .or_default()
            .insert(normalized, idx);
        out.push((inst, Resolution::Keep));
    }
    out
}
