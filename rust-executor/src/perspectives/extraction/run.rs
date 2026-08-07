use super::{
    build_extraction_input, class_local_name, ensure_extraction_task, existing_instance_titles,
    filter_already_present, parse_extraction_response, ProposedInstance,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkQuery};
use std::collections::HashSet;
use uuid::Uuid;

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
/// extraction task, retry parsing up to 5×, then for every proposed instance
/// write it into the perspective via `create_subject` — the same pipeline app
/// code uses, reading each class's `ad4m://constructor` + per-property
/// `ad4m://setter` actions from the SDNA. Returns the fresh base URI + links
/// read back per instance.
///
/// The `shapes` argument is exactly the classes to consider — callers pick
/// which subject classes to extract into (usually all classes carrying an
/// `extraction_hint`). `base_prefix` is the URI namespace under which new
/// instance identities are minted, e.g. `"soa://ext/"`.
///
/// Link status is no longer a caller choice: it now derives from the SDNA's
/// `local` flags via `create_subject`, exactly like app code. The classes must
/// be registered as real subject classes in the perspective (constructor +
/// setter actions) or `create_subject` errors with "No SHACL constructor
/// found".
pub async fn run_extraction(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
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

    // Write each surviving instance through `create_subject` — the same
    // constructor+setter pipeline app code uses — inside one batch so a
    // mid-write failure can't leave a half-formed instance. Relation-typed
    // properties are excluded from `initial_values`: their targets are instance
    // URIs, not literals (relation extraction is a later PR).
    let batch_id = perspective.create_batch().await;
    let mut bases: Vec<String> = Vec::new();
    for inst in &instances {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "extraction: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let rel_names: HashSet<&str> = shape
            .include_relations
            .iter()
            .map(|r| r.name.as_str())
            .collect();
        let initial_values: serde_json::Map<String, serde_json::Value> = inst
            .props
            .iter()
            .filter(|(k, _)| !rel_names.contains(k.as_str()))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let base = format!(
            "{base_prefix}{}/{}",
            inst.class.to_lowercase(),
            Uuid::new_v4()
        );
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(inst.class.clone()),
                    query: None,
                },
                base.clone(),
                Some(serde_json::Value::Object(initial_values)),
                Some(batch_id.clone()),
                context,
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "run_extraction: create_subject({}) failed: {e:#}",
                    inst.class
                )
            })?;
        bases.push(base);
    }
    perspective
        .commit_batch(batch_id, context)
        .await
        .map_err(|e| anyhow::anyhow!("run_extraction: commit_batch failed: {e:#}"))?;

    // Read back the links written per instance, so callers/tests see exactly
    // what landed in the store (proves the write and yields the real targets).
    let mut out = Vec::with_capacity(bases.len());
    for base in bases {
        let stored = perspective
            .get_links(&LinkQuery {
                source: Some(base.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("run_extraction: get_links(readback) failed: {e:#}"))?;
        let links: Vec<Link> = stored.into_iter().map(|d| d.data.clone()).collect();
        out.push((base, links));
    }
    Ok(out)
}
