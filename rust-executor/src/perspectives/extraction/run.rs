use super::{
    build_extraction_input, ensure_extraction_task, existing_instance_titles,
    filter_already_present, parse_extraction_response, plan_extraction_ops, ExtractionOp,
    ProposedInstance,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{LinkQuery, LinkStatus};
use std::collections::HashSet;

/// Max attempts for [`retry_extraction_parse`]. Mirrors Flux's `LLMutils`
/// retry-×5 loop: local models occasionally emit half-valid JSON, so we ask
/// again a few times before giving up on the whole call.
pub const EXTRACTION_MAX_ATTEMPTS: u8 = 5;

/// run `prompt_fn` up to [`EXTRACTION_MAX_ATTEMPTS`] times, parsing each
/// response as an extraction JSON payload. Returns the first successful parse;
/// the last parse error propagates if every attempt fails. `prompt_fn` is an
/// async closure so callers can inject anything (real `AIService`, a canned
/// script, a mock) without a live LLM.
///
/// This is deliberately a thin generic wrapper: it never mutates state, and it
/// is the only place we tolerate LLM flake. Any bug in prompt assembly should
/// fail deterministically in [`build_extraction_input`], not here.
pub async fn retry_extraction_parse<F, Fut>(
    mut prompt_fn: F,
) -> anyhow::Result<Vec<ProposedInstance>>
where
    F: FnMut(u8) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<String>>,
{
    let mut last_err: Option<anyhow::Error> = None;
    for attempt in 1..=EXTRACTION_MAX_ATTEMPTS {
        let raw = match prompt_fn(attempt).await {
            Ok(r) => r,
            Err(e) => {
                log::warn!("extraction: prompt attempt {attempt} failed: {e:#}");
                last_err = Some(e);
                continue;
            }
        };
        match parse_extraction_response(&raw) {
            Ok(instances) => return Ok(instances),
            Err(e) => {
                log::warn!(
                    "extraction: parse attempt {attempt} failed: {e:#}; will retry (max {EXTRACTION_MAX_ATTEMPTS})"
                );
                last_err = Some(e);
            }
        }
    }
    Err(last_err.unwrap_or_else(|| {
        anyhow::anyhow!(
            "extraction: failed after {EXTRACTION_MAX_ATTEMPTS} attempts with no captured error"
        )
    }))
}

/// Persist a planned set of [`ExtractionOp`]s into the perspective. Creates are
/// straight `add_links`; updates use SPARQL "set" semantics — for each scalar
/// predicate being written, existing links `(base, predicate, *)` are removed
/// before the new ones go in, so a scalar field ends up with exactly the new
/// value(s) instead of accumulating stale ones.
///
/// Everything runs in a single batch: a mid-write failure (an update's remove
/// succeeds but its add fails, for example) rolls back the whole run rather
/// than leaving a half-patched node.
pub async fn apply_extraction_ops(
    perspective: &mut PerspectiveInstance,
    ops: &[ExtractionOp],
    link_status: LinkStatus,
    context: &AgentContext,
) -> anyhow::Result<()> {
    // Nothing to write? bail before opening a batch — an empty run should be
    // a no-op, not a stray zero-diff commit.
    let empty = ops.iter().all(|op| match op {
        ExtractionOp::Create { links, .. } => links.is_empty(),
        ExtractionOp::Update { set, .. } => set.is_empty(),
    });
    if empty {
        return Ok(());
    }

    let batch_id = perspective.create_batch().await;

    for op in ops {
        match op {
            ExtractionOp::Create { links, .. } => {
                if links.is_empty() {
                    continue;
                }
                perspective
                    .add_links(
                        links.clone(),
                        link_status.clone(),
                        Some(batch_id.clone()),
                        context,
                    )
                    .await
                    .map_err(|e| {
                        anyhow::anyhow!("apply_extraction_ops: add_links(create) failed: {e:#}")
                    })?;
            }
            ExtractionOp::Update { base, set } => {
                if set.is_empty() {
                    continue;
                }
                // Replace-per-predicate: for every distinct predicate we're
                // writing, drop the existing `(base, predicate, *)` links first.
                // Guards against a "grouping" update leaving a stale
                // summary/relevance in place next to the new one.
                let mut cleared: HashSet<String> = HashSet::new();
                for link in set {
                    let Some(pred) = link.predicate.clone() else {
                        continue;
                    };
                    if !cleared.insert(pred.clone()) {
                        continue;
                    }
                    let existing = perspective
                        .get_links(&LinkQuery {
                            source: Some(base.clone()),
                            predicate: Some(pred.clone()),
                            ..Default::default()
                        })
                        .await
                        .map_err(|e| {
                            anyhow::anyhow!(
                                "apply_extraction_ops: get_links(update {pred}) failed: {e:#}"
                            )
                        })?;
                    for old in existing {
                        perspective
                            .remove_link(old.into(), Some(batch_id.clone()))
                            .await
                            .map_err(|e| {
                                anyhow::anyhow!(
                                    "apply_extraction_ops: remove_link(update {pred}) failed: {e:#}"
                                )
                            })?;
                    }
                }
                perspective
                    .add_links(
                        set.clone(),
                        link_status.clone(),
                        Some(batch_id.clone()),
                        context,
                    )
                    .await
                    .map_err(|e| {
                        anyhow::anyhow!("apply_extraction_ops: add_links(update) failed: {e:#}")
                    })?;
            }
        }
    }

    perspective
        .commit_batch(batch_id, context)
        .await
        .map_err(|e| anyhow::anyhow!("apply_extraction_ops: commit_batch failed: {e:#}"))?;
    Ok(())
}

/// end-to-end extraction driver. Wires everything: build the input from
/// shapes' hints + transcript, call `AIService::prompt` on the registered
/// extraction task, retry parsing up to 5×, then for every proposed
/// instance write its shape-driven links into the perspective via
/// [`apply_extraction_ops`] (creating or upserting per proposal `id`).
/// Returns the planned ops so callers can inspect create/update splits.
///
/// The `shapes` argument is exactly the classes to consider — callers pick
/// which subject classes to extract into (usually all classes carrying an
/// `extraction_hint`). `base_prefix` is the URI namespace under which new
/// instance identities are minted, e.g. `"soa://ext/"`.
///
/// `link_status` is the caller's choice of [`LinkStatus`] for the written
/// links. Pass [`LinkStatus::Local`] (the usual default) so LLM-generated
/// links on shared/neighbourhood perspectives are not auto-published; pass
/// [`LinkStatus::Shared`] only when the extraction is meant to sync.
pub async fn run_extraction(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
    link_status: LinkStatus,
    context: &AgentContext,
) -> anyhow::Result<Vec<ExtractionOp>> {
    let task = ensure_extraction_task()?;
    // Dedup context: what the graph already holds, so the model is steered away
    // from re-proposing known items and we can enforce it deterministically.
    let existing = existing_instance_titles(perspective, shapes).await?;
    let prompt = build_extraction_input(shapes, transcript, &existing);

    let service = crate::ai_service::AIService::global_instance()
        .await
        .map_err(|e| anyhow::anyhow!("run_extraction: AIService not ready: {e:#}"))?;

    let instances = retry_extraction_parse(|_attempt| {
        let service = service.clone();
        let task_id = task.task_id.clone();
        let prompt = prompt.clone();
        async move {
            let result = service
                .prompt(task_id, prompt)
                .await
                .map_err(|e| anyhow::anyhow!("AIService::prompt failed: {e:#}"))?;
            Ok(result.text)
        }
    })
    .await?;

    // Hard dedup guarantee: even if the model ignored the `existing` hint, an
    // already-present (class, title) never becomes a *new* instance. Updates
    // (proposals that carry an `id`) bypass this — they name a specific target.
    let (with_id, without_id): (Vec<_>, Vec<_>) =
        instances.into_iter().partition(|i| i.id.is_some());
    let deduped_creates = filter_already_present(without_id, &existing);
    let mut all: Vec<ProposedInstance> = deduped_creates;
    all.extend(with_id);

    let ops = plan_extraction_ops(shapes, &all, base_prefix);
    apply_extraction_ops(perspective, &ops, link_status, context).await?;
    Ok(ops)
}
