use super::{
    build_interpretation_input, class_local_name, ensure_interpretation_task,
    existing_instance_identities, filter_already_present, identity_property,
    parse_interpretation_response, ProposedInstance,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// Max attempts for [`retry_interpretation_parse`]. Mirrors Flux's `LLMutils`
/// retry-×5 loop: local models occasionally emit half-valid JSON, so we ask
/// again a few times before giving up on the whole call.
pub const INTERPRETATION_MAX_ATTEMPTS: u8 = 5;

/// run `prompt_fn` up to [`INTERPRETATION_MAX_ATTEMPTS`] times, parsing each
/// response as an interpretation JSON payload. Returns the first successful parse;
/// the last parse error propagates if every attempt fails. `prompt_fn` is an
/// async closure so callers can inject anything (real `AIService`, a canned
/// script, a mock) without a live LLM.
///
/// This is deliberately a thin generic wrapper: it never mutates state, and it
/// is the only place we tolerate LLM flake. Any bug in prompt assembly should
/// fail deterministically in [`build_interpretation_input`], not here.
pub async fn retry_interpretation_parse<F, Fut>(
    mut prompt_fn: F,
) -> anyhow::Result<Vec<ProposedInstance>>
where
    F: FnMut(u8) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<String>>,
{
    let mut last_err: Option<anyhow::Error> = None;
    for attempt in 1..=INTERPRETATION_MAX_ATTEMPTS {
        let raw = match prompt_fn(attempt).await {
            Ok(r) => r,
            Err(e) => {
                log::warn!("interpretation: prompt attempt {attempt} failed: {e:#}");
                last_err = Some(e);
                continue;
            }
        };
        match parse_interpretation_response(&raw) {
            Ok(instances) => return Ok(instances),
            Err(e) => {
                log::warn!(
                    "interpretation: parse attempt {attempt} failed: {e:#}; will retry (max {INTERPRETATION_MAX_ATTEMPTS})"
                );
                last_err = Some(e);
            }
        }
    }
    Err(last_err.unwrap_or_else(|| {
        anyhow::anyhow!(
            "interpretation: failed after {INTERPRETATION_MAX_ATTEMPTS} attempts with no captured error"
        )
    }))
}

/// end-to-end interpretation driver. Wires everything: build the input from
/// shapes' hints + transcript, call `AIService::prompt` on the registered
/// interpretation task, retry parsing up to 5×, then for every proposed instance
/// write it into the perspective via `create_subject` — the same pipeline app
/// code uses, reading each class's `ad4m://constructor` + per-property
/// `ad4m://setter` actions from the SDNA. Returns the base URIs of the affected
/// instances (created or updated); the links are owned by `create_subject`, not
/// this function.
///
/// The `shapes` argument is exactly the classes to consider — callers pick
/// which subject classes to extract into (usually all classes carrying an
/// `interpretation_hint`). `base_prefix` is the URI namespace under which new
/// instance identities are minted, e.g. `"soa://ext/"`.
///
/// Link status is no longer a caller choice: it now derives from the SDNA's
/// `local` flags via `create_subject`, exactly like app code. The classes must
/// be registered as real subject classes in the perspective (constructor +
/// setter actions) or `create_subject` errors with "No SHACL constructor
/// found".
pub async fn run_interpretation(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
    context: &AgentContext,
) -> anyhow::Result<Vec<String>> {
    // Returns a task already spawned into its LLM worker, so `prompt` can use it
    // immediately (see `ensure_interpretation_task`).
    let task = ensure_interpretation_task().await?;
    // Dedup context: what the graph already holds, so the model is steered away
    // from re-proposing known items and we can enforce it deterministically.
    // Keyed by each class's declared `identity` property; classes without one
    // are absent (no dedup).
    let existing = existing_instance_identities(perspective, shapes).await?;
    // class local name → identity property name, for the deterministic
    // safety-net below. Classes with no identity property are omitted.
    let identity_props: HashMap<String, String> = shapes
        .iter()
        .filter_map(|s| {
            identity_property(s).map(|idp| {
                (
                    class_local_name(&s.target_class).to_string(),
                    idp.name.clone(),
                )
            })
        })
        .collect();
    let prompt = build_interpretation_input(shapes, transcript, &existing, &identity_props);

    let service = crate::ai_service::AIService::global_instance()
        .await
        .map_err(|e| anyhow::anyhow!("run_interpretation: AIService not ready: {e:#}"))?;

    let instances = retry_interpretation_parse(|_attempt| {
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
    // already-present (class, identity value) never becomes a new instance.
    let instances = filter_already_present(instances, &existing, &identity_props);

    // Write each surviving instance through `create_subject` — the same
    // constructor+setter pipeline app code uses — inside one batch so a
    // mid-write failure can't leave a half-formed instance. Relation-typed
    // properties are excluded from `initial_values`: their targets are instance
    // URIs, not literals (relation interpretation is a later PR).
    let batch_id = perspective.create_batch().await;
    let mut bases: Vec<String> = Vec::new();
    let mut create_err: Option<anyhow::Error> = None;
    'build: for inst in &instances {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "interpretation: dropping proposed instance for unknown class '{}'",
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
        match perspective
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
        {
            Ok(_) => bases.push(base),
            Err(e) => {
                create_err = Some(anyhow::anyhow!(
                    "run_interpretation: create_subject({}) failed: {e:#}",
                    inst.class
                ));
                break 'build;
            }
        }
    }

    if let Some(e) = create_err {
        // Drop the half-built batch so it does not sit in `batch_store` for
        // BATCH_TIMEOUT_SECS. `commit_batch` removes the batch itself on
        // success, but a mid-loop `create_subject` failure never reaches it.
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(e);
    }

    match perspective.commit_batch(batch_id.clone(), context).await {
        Ok(_) => Ok(bases),
        Err(e) => {
            // Defense-in-depth: `commit_batch` already removes the batch on
            // entry (perspective_instance.rs `commit_batch`), so this is a
            // no-op today. Kept explicit so the invariant "no lingering batch
            // on any error path from run_interpretation" survives future
            // changes to `commit_batch`'s control flow.
            let _ = perspective.discard_batch(&batch_id).await;
            Err(anyhow::anyhow!(
                "run_interpretation: commit_batch failed: {e:#}"
            ))
        }
    }
}
