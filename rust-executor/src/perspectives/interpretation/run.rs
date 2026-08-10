use super::{
    build_interpretation_input, class_local_name, ensure_interpretation_task,
    existing_instance_context, filter_already_present_with_strategy, identities_from_context,
    identity_property, ids_from_context, parse_interpretation_response,
    plan_interpretation_ops_with_context, DedupStrategy, InterpretationOp, ProposedInstance,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::{ModelShape, ParentScope};
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{LinkQuery, LinkStatus};
use std::collections::HashMap;

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

/// Drop [`InterpretationOp::Update`] ops whose new field values already match
/// the perspective's current state (per-property value-set equality). Small LLMs
/// occasionally re-emit an existing `existing` entry verbatim after being
/// taught the upsert path — that's a semantic no-op (a setter "set" of the same
/// value), and letting it through would clear-and-rewrite scalar links for
/// nothing, producing spurious link churn and confusing test placement counts.
///
/// Comparison happens on **decoded** values, not raw link targets: the scalar
/// write path encodes through each property's `resolveLanguage`, and a signed
/// literal envelope is not byte-stable across writes. So the current target of
/// `(base, predicate)` is read back and run through
/// [`parse_literal_value`] — the canonical decoder `model_query` uses — before
/// being compared with the value the model proposed.
///
/// Only Updates are considered: Creates are already deduped upstream by
/// [`filter_already_present`], and `AddLinks` is additive by design. A property
/// whose predicate can't be resolved from the shape is treated as "can't prove
/// it's a no-op", so the op survives. Purely additive Updates (a property not
/// yet set on the base) survive naturally — the current value set is empty, so
/// it can't equal a one-element proposed set.
pub async fn strip_noop_updates(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    ops: Vec<InterpretationOp>,
) -> anyhow::Result<Vec<InterpretationOp>> {
    let mut kept = Vec::with_capacity(ops.len());
    for op in ops {
        let InterpretationOp::Update {
            base,
            class,
            values,
        } = &op
        else {
            kept.push(op);
            continue;
        };
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == class)
        else {
            kept.push(op);
            continue;
        };

        let mut all_noop = !values.is_empty();
        for (name, new_value) in values.iter() {
            let Some(prop) = shape.properties.iter().find(|p| &p.name == name) else {
                all_noop = false;
                break;
            };
            let existing = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    predicate: Some(prop.predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| {
                    anyhow::anyhow!(
                        "strip_noop_updates: get_links({base} {}) failed: {e:#}",
                        prop.predicate
                    )
                })?;
            // Decoded current value set for this predicate, as canonical JSON
            // strings so `Value`s (which aren't `Hash`/`Ord`) can be compared.
            let mut current: Vec<String> = existing
                .iter()
                .map(|l| parse_literal_value(&l.data.target).to_string())
                .collect();
            current.sort();
            current.dedup();
            if current.len() != 1 || current[0] != new_value.to_string() {
                all_noop = false;
                break;
            }
        }

        if all_noop {
            log::debug!("interpretation: dropping no-op update on {base}");
        } else {
            kept.push(op);
        }
    }
    Ok(kept)
}

/// Persist a planned set of [`InterpretationOp`]s into the perspective, inside a
/// single batch so a mid-write failure rolls back the whole run rather than
/// leaving a half-formed node.
///
/// Each op maps onto the same model-level write API app code uses:
///   * `Create` -> `create_subject` (constructor mints the type flag, then the
///     per-property setters run);
///   * `Update` -> `update_subject` (the same setters, no constructor — the node
///     is already of that class);
///   * `AddLinks` -> plain `add_links`, purely additive. Relation targets are
///     instance URIs, so there is nothing for a setter to encode, and a node may
///     hold many edges under one predicate (`hasMany`). These links are written
///     `Shared`, matching the default an SDNA action takes when it declares no
///     `local` flag.
pub async fn apply_interpretation_ops(
    perspective: &mut PerspectiveInstance,
    ops: &[InterpretationOp],
    context: &AgentContext,
) -> anyhow::Result<()> {
    // Nothing to write? bail before opening a batch — an empty run should be
    // a no-op, not a stray zero-diff commit.
    if ops.is_empty() {
        return Ok(());
    }

    let batch_id = perspective.create_batch().await;
    let mut apply_err: Option<anyhow::Error> = None;

    'apply: for op in ops {
        let step = match op {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => perspective
                .create_subject(
                    SubjectClassOption {
                        class_name: Some(class.clone()),
                        query: None,
                    },
                    base.clone(),
                    Some(serde_json::Value::Object(values.clone())),
                    Some(batch_id.clone()),
                    context,
                )
                .await
                .map(|_| ())
                .map_err(|e| {
                    anyhow::anyhow!(
                        "apply_interpretation_ops: create_subject({class}) failed: {e:#}"
                    )
                }),
            InterpretationOp::Update {
                base,
                class,
                values,
            } => perspective
                .update_subject(
                    SubjectClassOption {
                        class_name: Some(class.clone()),
                        query: None,
                    },
                    base.clone(),
                    serde_json::Value::Object(values.clone()),
                    Some(batch_id.clone()),
                    context,
                )
                .await
                .map_err(|e| {
                    anyhow::anyhow!(
                        "apply_interpretation_ops: update_subject({class}) failed: {e:#}"
                    )
                }),
            InterpretationOp::AddLinks { links, .. } => {
                if links.is_empty() {
                    continue;
                }
                perspective
                    .add_links(
                        links.clone(),
                        LinkStatus::Shared,
                        Some(batch_id.clone()),
                        context,
                    )
                    .await
                    .map(|_| ())
                    .map_err(|e| {
                        anyhow::anyhow!("apply_interpretation_ops: add_links failed: {e:#}")
                    })
            }
        };
        if let Err(e) = step {
            apply_err = Some(e);
            break 'apply;
        }
    }

    if let Some(e) = apply_err {
        // Drop the half-built batch so it does not sit in `batch_store` for
        // BATCH_TIMEOUT_SECS. `commit_batch` removes the batch itself on
        // success, but a mid-loop op failure never reaches it.
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(e);
    }

    match perspective.commit_batch(batch_id.clone(), context).await {
        Ok(_) => Ok(()),
        Err(e) => {
            // Defense-in-depth: `commit_batch` already removes the batch on
            // entry (perspective_instance.rs `commit_batch`), so this is a
            // no-op today. Kept explicit so the invariant "no lingering batch
            // on any error path from apply_interpretation_ops" survives future
            // changes to `commit_batch`'s control flow.
            let _ = perspective.discard_batch(&batch_id).await;
            Err(anyhow::anyhow!(
                "apply_interpretation_ops: commit_batch failed: {e:#}"
            ))
        }
    }
}

/// The instance bases an op set touches, in op order and de-duplicated — what
/// [`run_interpretation`] reads back so callers see exactly what landed.
fn touched_bases(ops: &[InterpretationOp]) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    for op in ops {
        let base = match op {
            InterpretationOp::Create { base, .. } | InterpretationOp::Update { base, .. } => base,
            InterpretationOp::AddLinks { source, .. } => source,
        };
        if seen.insert(base.clone()) {
            out.push(base.clone());
        }
    }
    out
}

/// end-to-end interpretation driver. Wires everything: build the input from
/// shapes' hints + transcript, call `AIService::prompt` on the registered
/// interpretation task, retry parsing up to 5×, plan the writes, then apply them
/// through `create_subject` / `update_subject` / `add_links` — the same pipeline
/// app code uses, reading each class's `ad4m://constructor` + per-property
/// `ad4m://setter` actions from the SDNA. Returns the base URIs of the affected
/// instances (created, updated, or given new relations); the links are owned by
/// `create_subject` / `update_subject`, not this function.
///
/// The run is tree-aware: the model is shown the instances already in the graph
/// (`id` + identity value per class), so it can
///   * skip re-proposing a known item (soft dedup, hard-enforced by
///     [`filter_already_present`]),
///   * *attach* to one by emitting its `id` — routed to the update path so an
///     existing node is refined instead of duplicated,
///   * *grow* the graph by referencing another instance (existing `id` or a
///     `new:<Class>:<n>` sibling minted in the same response) from a relation
///     field, which [`plan_interpretation_ops_with_context`] resolves into real
///     links.
///
/// The `shapes` argument is exactly the classes to consider — callers pick
/// which subject classes to interpret into (usually all classes carrying an
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
    scope: Option<&ParentScope>,
) -> anyhow::Result<Vec<String>> {
    run_interpretation_with_strategy(
        perspective,
        shapes,
        transcript,
        base_prefix,
        context,
        &DedupStrategy::default(),
        scope,
    )
    .await
}

/// [`run_interpretation`] with an explicit [`DedupStrategy`] — same pipeline,
/// but the identity-dedup safety net switches between normalized-string
/// matching (default) and semantic (embedding-based) matching per call.
///
/// This is the shape the future AutoProcessor / activity-runner uses to plug
/// in a per-neighbourhood policy without touching the interpretation models
/// themselves; [`run_interpretation`] is just this call with
/// [`DedupStrategy::default`], so all existing callers observe zero behaviour
/// change.
pub async fn run_interpretation_with_strategy(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
    context: &AgentContext,
    dedup_strategy: &DedupStrategy,
    scope: Option<&ParentScope>,
) -> anyhow::Result<Vec<String>> {
    let task = ensure_interpretation_task()?;
    // Existing-instance snapshot: gives the model both the `id` handle to
    // upsert/reference (so it can refine or link an existing node instead of
    // duplicating) and the identity value to recognise it by. The
    // identity-only projection feeds the deterministic dedup safety net below,
    // so both paths agree on what counts as "existing".
    let existing_ctx = existing_instance_context(perspective, shapes, scope).await?;
    let existing_identities = identities_from_context(&existing_ctx);
    // Valid targets for existing-id relation refs — exactly the ids the model is
    // shown in each class's `existing` list.
    let known_existing_ids = ids_from_context(&existing_ctx);
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
    let prompt = build_interpretation_input(shapes, transcript, &existing_ctx);

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
    // already-present (class, identity value) never becomes a *new* instance.
    // Updates (proposals carrying an `id`) bypass this — they name a specific
    // target. Crucially this filters **in place**, preserving the LLM's output
    // order so `new:<Class>:<n>` relation ordinals resolve against the same
    // ordering the model counted.
    let instances = filter_already_present_with_strategy(
        instances,
        &existing_identities,
        &identity_props,
        dedup_strategy,
    )
    .await?;

    let planned =
        plan_interpretation_ops_with_context(shapes, &instances, base_prefix, &known_existing_ids);
    // Filter no-op Updates: the LLM occasionally re-emits an unchanged existing
    // entry, and applying that would clear-and-rewrite scalar links for nothing.
    let ops = strip_noop_updates(perspective, shapes, planned).await?;
    apply_interpretation_ops(perspective, &ops, context).await?;

    // The affected instance base URIs (created, updated, or given new
    // relations). Links are owned by `create_subject` / `update_subject`.
    let bases = touched_bases(&ops);
    Ok(bases)
}
