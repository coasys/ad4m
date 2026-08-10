use super::graph::{identity_values_by_class, normalize_identity, ExistingInstances};
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

/// Semantic-dedup filter backed by AIService embeddings: for each class that
/// has both proposals and existing identities, embed both sides (one
/// `AIService::embed` call per string — the embedding channel is single-prompt;
/// a batch API would be a worthwhile future optimisation) and delegate to
/// [`semantic_dedup_pure`]. Proposals carrying a *trusted* `id` (one the graph
/// holds — an explicit upsert) bypass dedup, same as the string path; a
/// hallucinated id is still checked. Classes with no identity property, or
/// proposals missing that property's value, always survive.
pub async fn filter_already_present_semantic(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
    model: &str,
    threshold: f32,
) -> anyhow::Result<Vec<ProposedInstance>> {
    // Per-class identity values projected from the id-keyed source (raw, since
    // embeddings run on the same text the prompt showed the model).
    let existing_by_class = identity_values_by_class(existing);
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
    let mut proposed_vecs: HashMap<String, Vec<(usize, Vec<f32>)>> = HashMap::new();
    for (class, entries) in per_class.iter() {
        let existing_vals: Vec<String> = existing_by_class
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
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &ExistingInstances,
    identity_props: &HashMap<String, String>,
) -> Vec<ProposedInstance> {
    // Seed per-class known sets with pre-existing identities; each accepted
    // proposal is added to its class's set so a same-response duplicate is
    // dropped like an already-persisted one. Without this, an LLM that emits
    // the same (class, identity) twice would slip through and `run_interpretation`
    // would create two subjects for it.
    let mut known: HashMap<String, HashSet<String>> = identity_values_by_class(existing)
        .into_iter()
        .map(|(class, values)| {
            (
                class,
                values.iter().map(|v| normalize_identity(v)).collect(),
            )
        })
        .collect();
    let mut out = Vec::with_capacity(instances.len());
    for inst in instances {
        // A *trusted* id is an explicit upsert target — it names a specific
        // existing node, so its identity value *should* match one already
        // present; never dedup it away. But the planner only routes to Update
        // for ids the graph actually holds; a hallucinated id absent from
        // `existing` would be routed to Create, so it must still be
        // dedup-checked like an ordinary proposal (else a made-up id +
        // duplicate identity mints a duplicate node).
        if inst
            .id
            .as_deref()
            .is_some_and(|id| existing.contains_key(id))
        {
            out.push(inst);
            continue;
        }
        // No declared identity for this class ⇒ no dedup.
        let Some(idp_name) = identity_props.get(&inst.class) else {
            out.push(inst);
            continue;
        };
        let Some(value) = inst.props.get(idp_name).and_then(|v| v.as_str()) else {
            // no identity value to compare on — keep it
            out.push(inst);
            continue;
        };
        let normalized = normalize_identity(value);
        let set = known.entry(inst.class.clone()).or_default();
        if set.contains(&normalized) {
            log::debug!(
                "interpretation: dropping already-present {} '{}'",
                inst.class,
                value
            );
            continue;
        }
        set.insert(normalized);
        out.push(inst);
    }
    out
}
