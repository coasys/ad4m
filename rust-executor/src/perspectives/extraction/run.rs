use super::{
    build_extraction_input, ensure_extraction_task, existing_instance_titles,
    filter_already_present, parse_extraction_response, place_instances, ProposedInstance,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkStatus};

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

/// end-to-end extraction driver. Wires everything: build the input from
/// shapes' hints + transcript, call `AIService::prompt` on the registered
/// extraction task, retry parsing up to 5×, then for every proposed
/// instance write its shape-driven links into the perspective via
/// `add_link`. Returns the fresh base URI + links written per instance.
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
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
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
    // already-present (class, title) never becomes a new instance.
    let instances = filter_already_present(instances, &existing);
    let placements = place_instances(shapes, &instances, base_prefix);

    // Write all instance links in a single PerspectiveDiff (add_links) so a
    // mid-write failure can't leave a half-formed instance — e.g. one carrying
    // its `ns://type` flag but missing its `ns://title`. Status is the caller's
    // choice (see `link_status`).
    let all_links: Vec<Link> = placements
        .iter()
        .flat_map(|(_base, links)| links.iter().cloned())
        .collect();
    if !all_links.is_empty() {
        perspective
            .add_links(all_links, link_status, None, context)
            .await
            .map_err(|e| anyhow::anyhow!("run_extraction: add_links failed: {e:#}"))?;
    }
    Ok(placements)
}
