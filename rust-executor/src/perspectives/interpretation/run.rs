use super::{
    apply_with_overlay, build_interpretation_input, class_label,
    ensure_interpretation_task_for_model, existing_instance_context, existing_relation_links,
    identity_property, normalize_identity, parse_interpretation_response,
    plan_interpretation_ops_resolved, resolve_already_present_with_strategy, DedupStrategy,
    ExistingInstances, ExistingLinks, InterpretationOp, InterpretationRunCursor, ProposedInstance,
    TranscriptTurn,
};
use crate::agent::AgentContext;
use crate::ai_service::harness::propose::{
    class_propose_shape_from_shacl, ProposalBuffer, ProposeWritesProvider,
};
use crate::ai_service::harness::provider::{
    is_read_only, BoundArgsProvider, FilteredProvider, ToolProvider, ToolSchema,
};
use crate::ai_service::harness::{run_with_tools, HarnessConfig};
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::LinkStatus;
use std::collections::HashMap;
use std::sync::Arc;

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
/// Comparison happens on **decoded** values read back through `model_query` —
/// the same read path app code uses — so each property is resolved through its
/// own shape/getter (`resolveLanguage` and all), not hand-decoded on the
/// assumption every scalar is a `literal:`. The instance's current field values
/// are compared with what the model proposed; equal ⇒ the Update is a no-op.
///
/// Only Updates are considered: Creates are already deduped upstream by
/// [`filter_already_present`], and `AddLinks` is additive by design. If the
/// base isn't resolvable as an instance of `class` (or a proposed field isn't
/// present on it), we treat the op as "can't prove it's a no-op" and keep it.
/// Purely additive Updates (a property not yet set on the base) survive
/// naturally — the current value is absent, so it can't equal the proposal.
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
        if shapes
            .iter()
            .all(|s| class_label(&s.target_class, shapes) != *class)
        {
            kept.push(op);
            continue;
        }

        // Read the instance's current state via the generic model-query read
        // path (decodes each property through its own getter), then locate the
        // row for this base.
        let props: Vec<&str> = values.keys().map(String::as_str).collect();
        let query = serde_json::json!({ "properties": props }).to_string();
        let result_json = perspective.model_query(class, &query).await.map_err(|e| {
            anyhow::anyhow!("strip_noop_updates: model_query({class}) failed: {e:#}")
        })?;
        let result: serde_json::Value = serde_json::from_str(&result_json).map_err(|e| {
            anyhow::anyhow!("strip_noop_updates: bad model_query result for {class}: {e:#}")
        })?;
        let current_row = result
            .get("instances")
            .and_then(|v| v.as_array())
            .and_then(|rows| {
                rows.iter()
                    .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base.as_str()))
            });

        // A no-op only if the base exists and every proposed field already holds
        // the proposed value (decoded equality). Missing row / missing field ⇒
        // keep the op.
        let all_noop = !values.is_empty()
            && current_row.is_some_and(|row| {
                values
                    .iter()
                    .all(|(name, new_value)| row.get(name) == Some(new_value))
            });

        if all_noop {
            log::debug!("interpretation: dropping no-op update on {base}");
        } else {
            kept.push(op);
        }
    }
    Ok(kept)
}

/// Collapse duplicate ops from a harness buffer drain against the perspective's
/// existing state — the same identity-property matching the classic path runs
/// via [`resolve_already_present_with_strategy`], but operating on raw
/// [`InterpretationOp`]s rather than [`ProposedInstance`]s.
///
/// - `Create` whose identity-property value (under [`normalize_identity`])
///   matches an existing instance of the same class is rewritten to an `Update`
///   on that existing base, preventing unbounded duplicate accretion when the
///   auto-processor re-proposes the same instance across passes.
/// - Intra-batch `Create` dedup: if two `Create`s in the same drain match on
///   (class, normalized identity), only the first survives.
/// - `AddLinks` whose `(source, predicate, target)` triples all exist in
///   `existing_links` are dropped entirely; partially-new link sets are
///   filtered down to the novel triples.
/// - `Update` ops pass through unchanged (their base already exists by
///   definition).
pub(crate) fn dedup_ops_against_existing(
    ops: Vec<InterpretationOp>,
    existing: &ExistingInstances,
    shapes: &[ModelShape],
    existing_links: &ExistingLinks,
) -> Vec<InterpretationOp> {
    let identity_props: HashMap<String, String> = shapes
        .iter()
        .filter_map(|s| {
            identity_property(s).map(|idp| (class_label(&s.target_class, shapes), idp.name.clone()))
        })
        .collect();

    // Pre-existing graph identities → (normalized value → existing base), per class.
    let mut existing_norm_to_base: HashMap<String, HashMap<String, String>> = HashMap::new();
    for entries in existing.values() {
        for inst in entries {
            let Some(idp_name) = identity_props.get(&inst.class) else {
                continue;
            };
            // InstanceContext stores the identity value in `title`.
            let _ = idp_name; // identity prop name used for Create.values lookup below
            existing_norm_to_base
                .entry(inst.class.clone())
                .or_default()
                .entry(normalize_identity(&inst.title))
                .or_insert_with(|| inst.id.clone());
        }
    }

    // Intra-batch: (class, normalized identity) → index of the first Create
    // we kept, so a later same-key Create is dropped.
    let mut seen_in_batch: HashMap<String, HashMap<String, usize>> = HashMap::new();

    let mut out = Vec::with_capacity(ops.len());
    for (idx, op) in ops.into_iter().enumerate() {
        match op {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                let Some(idp_name) = identity_props.get(&class) else {
                    // No identity property for this class — no dedup possible.
                    out.push(InterpretationOp::Create {
                        base,
                        class,
                        values,
                    });
                    continue;
                };
                let id_value = values.get(idp_name).and_then(|v| v.as_str()).unwrap_or("");
                if id_value.is_empty() {
                    out.push(InterpretationOp::Create {
                        base,
                        class,
                        values,
                    });
                    continue;
                }
                let normalized = normalize_identity(id_value);

                // Check against existing graph instances.
                if let Some(existing_base) = existing_norm_to_base
                    .get(&class)
                    .and_then(|m| m.get(&normalized))
                    .cloned()
                {
                    log::debug!(
                        "harness dedup: Create({class}, {:?}) → Update on existing {existing_base}",
                        id_value,
                    );
                    out.push(InterpretationOp::Update {
                        base: existing_base,
                        class,
                        values,
                    });
                    continue;
                }

                // Intra-batch dedup: drop if we already kept a Create with the
                // same (class, normalized identity) earlier in this drain.
                if seen_in_batch
                    .get(&class)
                    .and_then(|m| m.get(&normalized))
                    .is_some()
                {
                    log::debug!(
                        "harness dedup: dropping intra-batch duplicate Create({class}, {:?})",
                        id_value,
                    );
                    continue;
                }
                seen_in_batch
                    .entry(class.clone())
                    .or_default()
                    .insert(normalized, idx);
                out.push(InterpretationOp::Create {
                    base,
                    class,
                    values,
                });
            }
            InterpretationOp::AddLinks { source, links } => {
                let novel: Vec<_> = links
                    .into_iter()
                    .filter(|link| {
                        let predicate = link.predicate.clone().unwrap_or_default();
                        !existing_links.contains(&(
                            link.source.clone(),
                            predicate,
                            link.target.clone(),
                        ))
                    })
                    .collect();
                if novel.is_empty() {
                    log::debug!(
                        "harness dedup: dropping AddLinks on {source} — all triples already exist"
                    );
                    continue;
                }
                out.push(InterpretationOp::AddLinks {
                    source,
                    links: novel,
                });
            }
            // Update ops pass through — their base already exists.
            op @ InterpretationOp::Update { .. } => out.push(op),
        }
    }
    out
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
                // Relation links are written `Shared`: the interpretation-facing
                // `ShapeRelation` carries no per-relation `local` flag today, so
                // there is nothing to honour. If local relations are added to the
                // shape model, thread that status onto `AddLinks` and use it here.
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
    transcript: &[TranscriptTurn],
    base_prefix: &str,
    context: &AgentContext,
    scope: Option<&Scope>,
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

/// [`run_interpretation`] with live mid-pass telemetry.
///
/// Identical pipeline and defaults to [`run_interpretation`]; the only
/// difference is that `emit_ctx`, when `Some`, makes the engine emit
/// `LlmRequestSent` (with the prompt) and `LlmResponseReceived` (with the
/// response) around the model call.
///
/// Exists because the one-shot WS path had no way to reach that plumbing.
/// The parameters already existed on
/// [`run_interpretation_with_strategy_and_model`], but reaching them meant a
/// call site spelling out eleven arguments — nine of which are the defaults
/// [`run_interpretation`] already picks — so the handler either duplicated
/// those defaults (and drifted from them) or the one-shot path stayed silent.
/// It stayed silent — so a caller blocked on `runInterpretation` observed no
/// progress at all, while the same work under a standing watch produced a full
/// step stream.
pub async fn run_interpretation_observed(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[TranscriptTurn],
    base_prefix: &str,
    context: &AgentContext,
    scope: Option<&Scope>,
    emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
) -> anyhow::Result<Vec<String>> {
    run_interpretation_with_strategy_and_model(
        perspective,
        shapes,
        transcript,
        base_prefix,
        context,
        &DedupStrategy::default(),
        None,
        scope,
        None,
        false,
        emit_ctx,
    )
    .await
    .map(|out| out.bases)
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
    transcript: &[TranscriptTurn],
    base_prefix: &str,
    context: &AgentContext,
    dedup_strategy: &DedupStrategy,
    scope: Option<&Scope>,
) -> anyhow::Result<Vec<String>> {
    run_interpretation_with_strategy_and_model(
        perspective,
        shapes,
        transcript,
        base_prefix,
        context,
        dedup_strategy,
        None,
        scope,
        None,
        false,
        None,
    )
    .await
    .map(|out| out.bases)
}

/// [`run_interpretation_with_strategy`] with an optional per-call LLM model
/// override — routes the interpretation prompt through the AI-task DB row
/// bound to `model_override` (falling back to the shared default row when
/// `None`).
///
/// `model_override = None` reuses the exact task row every existing caller
/// already uses, so behaviour is unchanged for all non-processor callers.
/// Optional live-debug capture for a single interpretation pass — the raw
/// prompt fed to the LLM and its response, verbatim. Populated only when the
/// caller opts in via `emit_debug_events`. Kept out of the base `Vec<String>`
/// return so a normal pass does not carry tens of KB of prompt text through
/// every call site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterpretationDebug {
    pub prompt: String,
    pub response: String,
}

/// Enhanced outcome of one interpretation pass: the affected instance bases
/// (create/update/relations touched) plus optional live-debug capture.
///
/// This is the shape `run_interpretation_with_strategy_and_model` returns
/// now; delegators (`run_interpretation`, `run_interpretation_with_strategy`)
/// preserve their `Vec<String>` return by extracting `.bases`.
#[derive(Debug, Clone)]
pub struct InterpretationOutcome {
    pub bases: Vec<String>,
    pub debug: Option<InterpretationDebug>,
    /// URIs of the `FlowTransitionProposal`s the deterministic flow pass
    /// minted after this pass's writes were committed.
    pub flow_proposals: Vec<String>,
}

pub async fn run_interpretation_with_strategy_and_model(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[TranscriptTurn],
    base_prefix: &str,
    context: &AgentContext,
    dedup_strategy: &DedupStrategy,
    model_override: Option<&str>,
    scope: Option<&Scope>,
    cursor: Option<&InterpretationRunCursor>,
    emit_debug_events: bool,
    emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
) -> anyhow::Result<InterpretationOutcome> {
    // Returns a task already spawned into its LLM worker, so `prompt` can use it
    // immediately (see `ensure_interpretation_task_for_model`).
    let task = ensure_interpretation_task_for_model(model_override).await?;
    let interpretation_started = std::time::Instant::now();
    let class_names: Vec<String> = shapes
        .iter()
        .map(|s| class_label(&s.target_class, shapes))
        .collect();
    log::info!(
        "🧠 interpretation start strategy={:?} model={} classes={:?} transcript_turns={}",
        dedup_strategy,
        model_override.unwrap_or("<default>"),
        class_names,
        transcript.len()
    );
    // Symmetry rule (see rust-executor/LOGGING.md): every `start` info
    // line gets exactly one companion — `done` on success, `failed` on
    // error. Wrap the fallible body so failures also produce a log line
    // instead of leaving a dangling `start`.
    let outcome_result: anyhow::Result<InterpretationOutcome> = async {
        // Existing-instance snapshot: gives the model both the `id` handle to
        // upsert/reference (so it can refine or link an existing node instead of
        // duplicating) and the identity value to recognise it by. This one
        // id-keyed map is the single source: the prompt, the dedup safety net, and
        // Create-vs-Update routing all project what they need from it, so every
        // path agrees on what counts as "existing".
        let existing_ctx = existing_instance_context(perspective, shapes, scope).await?;
        // The relation edges already in the graph, so a repeated continuous pass
        // does not re-emit a link that already exists (James #883 #4). Additive
        // AddLinks would otherwise duplicate the edge — and its reifier node, whose
        // IRI hashes in the link timestamp — on every pass.
        let existing_links = existing_relation_links(perspective, shapes).await?;
        // class local name → identity property name, for the deterministic
        // safety-net below. Classes with no identity property are omitted.
        let identity_props: HashMap<String, String> = shapes
            .iter()
            .filter_map(|s| {
                identity_property(s)
                    .map(|idp| (class_label(&s.target_class, shapes), idp.name.clone()))
            })
            .collect();
        // Slice 10.3c — Model C becomes end-to-end flow-aware. Flow context
        // loads silently-empty on I/O failure so a broken flow-definition
        // never blinds the extraction pass; the fallback is byte-for-byte
        // the pre-slice-10.2 prompt shape.
        //
        // Flow subjects = the URIs the pass is actually interpreting.
        // Prefer `cursor.sources` (the drained batch bases the auto-processor
        // threaded through as `InterpretationRunCursor`); a dedup `Scope` is
        // a legacy fallback for callers that predate the cursor (see J#1,
        // PR #929 James review). Empty subjects → no flow context in the
        // prompt (bounded), not the whole-perspective sweep the pre-fix
        // `None` path did.
        let flow_subjects: Vec<String> = if let Some(c) = cursor {
            c.sources.clone()
        } else if let Some(s) = scope {
            vec![crate::perspectives::flow_context::scope_subject(s).to_string()]
        } else {
            Vec::new()
        };
        let active_flows = crate::perspectives::flow_context::gather_active_flow_contexts(
            perspective,
            &flow_subjects,
        )
        .await;
        let prompt = build_interpretation_input(shapes, transcript, &existing_ctx, &active_flows);

        let service = crate::ai_service::AIService::global_instance()
            .await
            .map_err(|e| anyhow::anyhow!("run_interpretation: AIService not ready: {e:#}"))?;

        // Mid-pass observability (Nico 2026-08-20 + CodeRabbit #903 CR #6):
        // `LlmRequestSent` fires right before EACH `service.prompt` call and
        // `LlmResponseReceived` fires right after EACH successful prompt
        // response — including responses that later fail parsing, so a UI
        // can diagnose why a retry happened. Both live INSIDE the retry
        // callback: emitting them once around the whole retry loop would
        // hide any raw response that wasn't the final parse-successful one.
        //
        // `debug_response_capture` retains the LAST successful raw response
        // for `InterpretationRun` persistence — the same value that ends up
        // on the run node's `debugResponse` scalar. Retries only happen when
        // parsing fails, so the value in the cell after `retry_interpretation_parse`
        // succeeds is by construction the final (parse-successful) attempt.
        let debug_response_capture: std::sync::Arc<std::sync::Mutex<Option<String>>> =
            std::sync::Arc::new(std::sync::Mutex::new(None));
        let instances = retry_interpretation_parse(|_attempt| {
            let service = service.clone();
            let task_id = task.task_id.clone();
            let prompt = prompt.clone();
            let capture = debug_response_capture.clone();
            let emit_ctx_cloned = emit_ctx.cloned();
            async move {
                use crate::perspectives::auto_processor::events::{
                    emit, AutoProcessorEvent, AutoProcessorStep,
                };

                if let Some(ctx) = emit_ctx_cloned.as_ref() {
                    emit(
                        AutoProcessorEvent::new(
                            &ctx.perspective_uuid,
                            &ctx.processor_id,
                            AutoProcessorStep::LlmRequestSent,
                        )
                        .with_agent_did(&ctx.agent_did)
                        .with_items(&ctx.item_ids)
                        .with_batch_key(&ctx.batch_key)
                        .with_llm_input(prompt.clone()),
                    )
                    .await;
                }

                let result = service
                    // Internal caller (interpretation runner) — no user auth context; billing skipped.
                    .prompt(task_id, prompt, None)
                    .await
                    .map_err(|e| anyhow::anyhow!("AIService::prompt failed: {e:#}"))?;

                if let Some(ctx) = emit_ctx_cloned.as_ref() {
                    emit(
                        AutoProcessorEvent::new(
                            &ctx.perspective_uuid,
                            &ctx.processor_id,
                            AutoProcessorStep::LlmResponseReceived,
                        )
                        .with_agent_did(&ctx.agent_did)
                        .with_items(&ctx.item_ids)
                        .with_batch_key(&ctx.batch_key)
                        .with_llm_output(result.text.clone()),
                    )
                    .await;
                }

                if emit_debug_events {
                    if let Ok(mut slot) = capture.lock() {
                        *slot = Some(result.text.clone());
                    }
                }
                Ok(result.text)
            }
        })
        .await?;

        // Hard dedup guarantee: even if the model ignored the `existing` hint, an
        // already-present (class, identity value) never becomes a *new* instance.
        // Updates (proposals carrying an `id`) bypass this — they name a specific
        // target. Rather than *drop* duplicates (which would shift later
        // `new:<Class>:<n>` ordinals — James #883), we TAG every proposal in
        // emission order with its `Resolution`; the planner indexes all of them for
        // ordinal resolution but writes ops only for the kept ones.
        let resolved = resolve_already_present_with_strategy(
            instances,
            &existing_ctx,
            &identity_props,
            dedup_strategy,
        )
        .await?;

        let planned = plan_interpretation_ops_resolved(
            shapes,
            &resolved,
            base_prefix,
            &existing_ctx,
            &existing_links,
        );
        // Filter no-op Updates: the LLM occasionally re-emits an unchanged existing
        // entry, and applying that would clear-and-rewrite scalar links for nothing.
        let ops = strip_noop_updates(perspective, shapes, planned).await?;

        // Apply the writes AND the provenance overlay (#883): every create/update
        // also instantiates/updates an `InterpretationOverlay` over the same base
        // (kind + run + `inferred/<p>` snapshot), and the human-divergence gate keeps
        // real writes only where the value is still the LLM's own. One
        // `InterpretationRun` is minted per pass and threaded onto every overlay.
        let run_id = uuid::Uuid::new_v4().to_string();
        let ran_at = chrono::Utc::now().timestamp_millis().to_string();
        // Build the debug capture struct once so the shared cell's contents live
        // exactly one hop: extracted here, cloned into the meta persisted on the
        // run node, and returned to the caller for the live event.
        let debug = if emit_debug_events {
            let response = debug_response_capture
                .lock()
                .ok()
                .and_then(|slot| slot.clone())
                .unwrap_or_default();
            Some(InterpretationDebug {
                prompt: prompt.clone(),
                response,
            })
        } else {
            None
        };
        let bases = apply_with_overlay(
            perspective,
            shapes,
            ops,
            &task,
            run_id,
            ran_at,
            context,
            cursor,
            debug.as_ref(),
        )
        .await?;

        // The affected instance base URIs (created, updated, or given new
        // relations). Links are owned by `create_subject` / `update_subject`.
        // The LLM's writes are on the graph now, so re-evaluate every active
        // flow's `requires` guards against the fresh evidence.
        let flow_proposals = crate::perspectives::flow_evaluator::run_engine_proposal_pass(
            perspective,
            scope,
            context,
        )
        .await;

        Ok(InterpretationOutcome {
            bases,
            debug,
            flow_proposals,
        })
    }
    .await;
    match outcome_result {
        Ok(outcome) => {
            log::info!(
                "✅ 🧠 interpretation done model={} latency={}ms bases_written={} flow_proposals={}",
                model_override.unwrap_or("<default>"),
                interpretation_started.elapsed().as_millis(),
                outcome.bases.len(),
                outcome.flow_proposals.len()
            );
            Ok(outcome)
        }
        Err(e) => {
            log::error!(
                "❌ 🧠 interpretation failed model={} latency={}ms err={}",
                model_override.unwrap_or("<default>"),
                interpretation_started.elapsed().as_millis(),
                e
            );
            Err(e)
        }
    }
}

/// Harness-dispatched interpretation pass — the tool-calling alternative to
/// [`run_interpretation_with_strategy_and_model`]. Same inputs/outputs, but
/// the LLM sees a live tool surface (`{Class}_query`, `{Class}_get`,
/// `{Class}_propose_create`, `{Class}_propose_link_child`, …) and drives the
/// extraction by tool calls rather than by emitting one big JSON blob.
///
/// Design v3 §6: writes only cross the overlay gate at pass boundary — the
/// LLM's `_propose_*` calls buffer [`InterpretationOp`]s; when the harness
/// loop terminates (plain answer or budget exhausted), the buffer drains
/// straight into [`apply_with_overlay`] with the same run-id + task threading
/// the single-shot path uses.
///
/// When `dedup_on_drain` is `false` (the WS-RPC one-shot default), the
/// buffer flows straight into the overlay gate — the harness trusts the
/// LLM to query-first. When `true` (the auto-processor's recurrent path),
/// the drained ops pass through [`dedup_ops_against_existing`] before
/// `apply_with_overlay`: `Create`s whose identity matches an existing
/// instance collapse to `Update`s, intra-batch duplicate `Create`s are
/// dropped, and `AddLinks` whose triples already exist are filtered out.
/// This bounds duplicate accretion on the indefinitely-running processor.
///
/// The final answer text from the harness is discarded — the LLM already
/// wrote via tools, and any narrative it emits is not consumed by the
/// interpretation pipeline. (If we later want a try-parse-as-fallback, it
/// hooks in here.)
///
/// `max_tool_calls` = 0 is treated as "no harness path"; callers should
/// route to [`run_interpretation_with_strategy_and_model`] instead of
/// calling this with 0.
#[allow(clippy::too_many_arguments)]
pub async fn run_interpretation_with_harness_and_model(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[TranscriptTurn],
    base_prefix: &str,
    context: &AgentContext,
    model_override: Option<&str>,
    scope: Option<&Scope>,
    cursor: Option<&InterpretationRunCursor>,
    max_tool_calls: u32,
    auth_token: Option<String>,
    emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
    dedup_on_drain: bool,
    credit_gate: Option<Arc<dyn crate::ai_service::harness::CreditGate>>,
) -> anyhow::Result<Vec<String>> {
    // Same task-row selection as the single-shot path so the model + system
    // prompt + few-shots + billing meta come from the same row the operator
    // configured — the harness pass is just a different loop, not a
    // different LLM identity.
    let task = ensure_interpretation_task_for_model(model_override).await?;

    // Same existing-instance snapshot as the single-shot path: goes into the
    // prompt so the LLM has context (and can name existing `id`s in
    // propose_link_child) even before its first query tool call.
    let existing_ctx = existing_instance_context(perspective, shapes, scope).await?;
    // Existing relation links: consumed by the dedup-on-drain pass to filter
    // AddLinks ops whose triples already exist in the graph.
    let existing_links = existing_relation_links(perspective, shapes).await?;

    // Slice 10.3c — same flow-context load as the single-shot path.
    // Silently-empty on failure so a broken flow definition can never
    // blind the harness pass. Subjects derived from `cursor.sources`
    // (drained batch bases) with legacy `scope` fallback (J#1, PR #929
    // James review).
    let flow_subjects: Vec<String> = if let Some(c) = cursor {
        c.sources.clone()
    } else if let Some(s) = scope {
        vec![crate::perspectives::flow_context::scope_subject(s).to_string()]
    } else {
        Vec::new()
    };
    let active_flows =
        crate::perspectives::flow_context::gather_active_flow_contexts(perspective, &flow_subjects)
            .await;
    let prompt = build_interpretation_input(shapes, transcript, &existing_ctx, &active_flows);

    // Build per-class propose shapes from the perspective's SHACL classes,
    // filtered to the class-name set the caller passed as `shapes`. Any
    // class in `shapes` that isn't a registered SHACL subject class is
    // skipped — the propose-write surface only makes sense for classes
    // with constructors + setters.
    let all_shacl_classes = crate::mcp::shacl::load_classes(perspective).await;
    let requested_class_names: HashMap<String, ()> = shapes
        .iter()
        .map(|s| (class_label(&s.target_class, shapes), ()))
        .collect();
    let propose_shapes: Vec<_> = all_shacl_classes
        .iter()
        .filter(|c| requested_class_names.contains_key(&c.name))
        .map(class_propose_shape_from_shacl)
        .collect();

    // The AD4M MCP handler is the read-tool surface. Constructed here with a
    // pass-scoped McpContext — admin-credential from env (matches the /v1
    // openai-compat path) + a fresh per-pass auth-token slot. Every existing
    // MCP tool (`query_*`, `get_*`, subject/perspective tools) becomes
    // visible to the LLM through `Ad4mToolProvider`.
    let mcp_context = crate::mcp::server::McpContext {
        admin_credential: std::env::var("AD4M_ADMIN_CREDENTIAL").ok(),
        auth_token: Arc::new(tokio::sync::RwLock::new(auth_token.clone())),
    };
    let mcp_handler = Arc::new(crate::mcp::tools::Ad4mMcpHandler::new(mcp_context));
    let ad4m_provider = Arc::new(crate::mcp::tools::provider_impl::Ad4mToolProvider::new(
        mcp_handler,
    ));

    // Bind `perspective_id` to the pass's own perspective UUID: strip it from
    // every schema (LLM never sees it) and auto-inject on every dispatch.
    // Rationale: every dynamic per-class tool (`extbelief_query`,
    // `extintention_propose_create`, `_get`, `_list`, collection ops) declares
    // `perspective_id` as required, but the LLM has no reliable way to know
    // the UUID. CI job 22287 on `dcaeba21b` failed 8/8 because gemma3:12b
    // hallucinated the string `"ad4m"`, hit "Perspective not found", and
    // bailed in plain text. Binding closes that gap without any tool-schema
    // migration work.
    let ad4m_bound: Arc<dyn ToolProvider> = Arc::new(BoundArgsProvider::new(
        ad4m_provider,
        std::collections::BTreeMap::from([(
            "perspective_id".to_string(),
            serde_json::Value::String(perspective.uuid.clone()),
        )]),
    ));

    // Narrow the Ad4m tool surface to (a) class-scoped tools for the offered
    // classes only, and (b) read verbs only. Rationale: a smaller local LLM
    // like `gemma3:12b` faced with 60+ generic tools (`add_perspective`,
    // neighbourhood/agent/runtime helpers, dynamic write verbs on every
    // registered class) tends to hallucinate simpler tool names and skips
    // the propose-write path — observed in CI job 22252 on `5c34ed868`,
    // where the LLM called dynamic `extintention_create` (bypassing the
    // overlay) and never touched `extintention_propose_create`. Filtering
    // down to the ~2 classes × ~3 read verbs the pass actually needs
    // forces the LLM through the propose-write wrappers below.
    let allowed_class_prefixes: Vec<String> = propose_shapes
        .iter()
        .map(|s| format!("{}_", s.class_name.to_lowercase()))
        .collect();
    let ad4m_filtered: Arc<dyn ToolProvider> =
        Arc::new(FilteredProvider::new(ad4m_bound, move |t: &ToolSchema| {
            if !is_read_only(t) {
                return false;
            }
            allowed_class_prefixes.iter().any(|p| t.name.starts_with(p))
        }));

    // Wrap in ProposeWritesProvider so the LLM also sees the two synthetic
    // per-class writers whose side-effect is "queue an InterpretationOp",
    // not "mutate the graph." The buffer is drained after the loop.
    let buffer = ProposalBuffer::new();
    let classes_offered = propose_shapes.len();
    let provider: Arc<dyn ToolProvider> = Arc::new(ProposeWritesProvider::new(
        ad4m_filtered,
        propose_shapes,
        buffer.clone(),
        base_prefix.to_string(),
    ));

    // OpenAI-compat bridge: real CompletionSource that talks to AIService
    // via the tool-grammar constrained-decoding path Josh cherry-picked
    // into /v1. Local + remote models both go through this one seam.
    let service = crate::ai_service::AIService::global_instance()
        .await
        .map_err(|e| anyhow::anyhow!("run_interpretation_harness: AIService not ready: {e:#}"))?;
    let bridge = Arc::new(
        crate::api::openai_compat::harness_bridge::OpenAiCompatBridge::new(
            Arc::new(service),
            auth_token,
        ),
    );

    // Single user message carrying the entire interpretation prompt —
    // OpenAiCompatBridge prepends the tools-system-prompt automatically
    // when tools are advertised, so we don't split system-vs-user here.
    let initial_messages = vec![serde_json::json!({
        "role": "user",
        "content": prompt,
    })];

    // Drive the loop. The returned text is discarded — the LLM wrote via
    // propose_* tools, which populated `buffer`. If the LLM emitted a final
    // narrative it's not consumed here (see doc comment above).
    let _final_text = run_with_tools(
        &task.model_id,
        initial_messages,
        provider,
        bridge,
        HarnessConfig { max_tool_calls },
        emit_ctx,
        credit_gate,
    )
    .await?;

    // Drain the buffered ops. When `dedup_on_drain` is active (auto-processor
    // path), collapse duplicate Creates and filter stale AddLinks before
    // handing off to the overlay gate.
    let ops = buffer.drain();
    let ops = if dedup_on_drain {
        dedup_ops_against_existing(ops, &existing_ctx, shapes, &existing_links)
    } else {
        ops
    };
    // CI-visible diagnostic: pairs with the per-round log in `run_with_tools`.
    // When the harness silently returns zero bases it's almost always because
    // the LLM chose to answer instead of tool-calling, so the buffer is empty
    // — surfacing that here turns a mysterious empty result into an obvious
    // "LLM refused to write" data point.
    log::warn!(
        "harness: pass complete, ops_buffered={} classes_offered={} model={}",
        ops.len(),
        classes_offered,
        task.model_id,
    );
    let run_id = uuid::Uuid::new_v4().to_string();
    let ran_at = chrono::Utc::now().timestamp_millis().to_string();
    // Harness path has no single "raw prompt / raw response" to persist —
    // it's a multi-turn loop with per-round tool_calls. `None` skips the
    // `InterpretationDebug` payload dev #903 wires into the classic path;
    // a follow-up commit on this branch can carry a per-round transcript
    // once the shape is agreed.
    let bases = apply_with_overlay(
        perspective,
        shapes,
        ops,
        &task,
        run_id,
        ran_at,
        context,
        cursor,
        None,
    )
    .await?;

    log::warn!("harness: apply_with_overlay produced {} bases", bases.len());

    // Same flow post-processing as the single-shot path. The harness
    // returns bases only, so the minted proposals are just logged here.
    let flow_proposals =
        crate::perspectives::flow_evaluator::run_engine_proposal_pass(perspective, scope, context)
            .await;
    if !flow_proposals.is_empty() {
        log::warn!(
            "harness: flow pass minted {} proposal(s): {flow_proposals:?}",
            flow_proposals.len()
        );
    }

    Ok(bases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;
    use crate::types::{AITask, Link};

    #[tokio::test]
    async fn strip_noop_updates_drops_same_value_upsert_keeps_real_change() {
        // Seed an Intention with title+owner. Then plan three ops on it:
        //   (1) same title + same owner   -> no-op, must be dropped.
        //   (2) new title (different value) -> real change, must survive.
        //   (3) a Create -> passed through unchanged (dedup happens elsewhere).
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://existing/intention/noop-target";
        seed_instance(&mut perspective, &ctx, &shapes[0], base, "Ship the parser").await;
        // Seed an owner too so the no-op check covers a multi-field state.
        apply_one(
            &mut perspective,
            &shapes,
            &ctx,
            proposal(
                "Intention",
                Some(base),
                &[("owner", serde_json::json!("Nico"))],
            ),
        )
        .await;

        // Mirror `run_interpretation`: the graph's actual base is what makes an id
        // trusted, otherwise the planner treats it as hallucinated and creates.
        let existing_ctx = existing_instance_context(&perspective, &shapes, None)
            .await
            .expect("existing_instance_context");
        let planned = plan_interpretation_ops_with_context(
            &shapes,
            &[
                // No-op update: title + owner identical to the seeded state.
                proposal(
                    "Intention",
                    Some(base),
                    &[
                        ("title", serde_json::json!("Ship the parser")),
                        ("owner", serde_json::json!("Nico")),
                    ],
                ),
                // Real update: same base, but a rewritten title.
                proposal(
                    "Intention",
                    Some(base),
                    &[("title", serde_json::json!("Ship the parser this week"))],
                ),
                // A Create (no id) — strip_noop_updates only looks at Updates.
                proposal(
                    "Intention",
                    None,
                    &[("title", serde_json::json!("A brand new idea"))],
                ),
            ],
            "soa://ext/",
            &existing_ctx,
        );
        assert_eq!(planned.len(), 3, "sanity: planner emitted all three");

        let kept = strip_noop_updates(&perspective, &shapes, planned)
            .await
            .expect("strip_noop_updates");

        let updates: Vec<&InterpretationOp> = kept
            .iter()
            .filter(|op| matches!(op, InterpretationOp::Update { .. }))
            .collect();
        let creates: Vec<&InterpretationOp> = kept
            .iter()
            .filter(|op| matches!(op, InterpretationOp::Create { .. }))
            .collect();
        assert_eq!(
            updates.len(),
            1,
            "no-op Update dropped, real Update kept; got {kept:#?}"
        );
        assert_eq!(creates.len(), 1, "Create pass-through; got {kept:#?}");
        let InterpretationOp::Update { values, .. } = updates[0] else {
            unreachable!()
        };
        assert_eq!(
            values.get("title").and_then(|v| v.as_str()),
            Some("Ship the parser this week"),
            "kept Update must be the real one"
        );
    }

    #[test]
    fn ensure_interpretation_task_registers_and_is_idempotent() {
        ensure_db_init();

        // Guard: some other test may have inserted the row already; wipe just
        // our name so the first call below is a real insert. (Global DB is
        // shared across the single-threaded test run.)
        let existing: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == INTERPRETATION_TASK_NAME)
            .collect();
        for t in existing {
            Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
        }

        // Target the DB-only primitive: it registers the row without touching the
        // AIService, so this stays a no-model/no-GPU unit test. (The async
        // `ensure_interpretation_task` wrapper additionally spawns the task.)
        let (first, created) = register_interpretation_task().unwrap();
        assert!(created, "first call after wipe must insert the row");
        assert_eq!(first.name, INTERPRETATION_TASK_NAME);
        assert_eq!(first.model_id, "default");
        assert!(first.system_prompt.contains("You extract typed instances"));
        assert!(!first.task_id.is_empty());

        // Second call must find the same row, not insert a duplicate.
        let (second, created_again) = register_interpretation_task().unwrap();
        assert!(!created_again, "second call must find the existing row");
        assert_eq!(first.task_id, second.task_id);

        let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == INTERPRETATION_TASK_NAME)
            .collect();
        assert_eq!(
            rows.len(),
            1,
            "expected exactly one interpretation task row"
        );
    }

    #[tokio::test]
    async fn gather_transcript_sparql_returns_speaker_text_and_timestamp() {
        // The generic SPARQL gather must:
        //   1. run an arbitrary SELECT against the perspective's Oxigraph store,
        //   2. bind `?speaker` from the body-link reifier (`ad4m://ontology/author`
        //      = the signing agent, NOT a separate ns://author property),
        //   3. bind `?text` and, when it's a `literal:string:...` URI, decode it,
        //   4. bind `?timestamp` from the same reifier,
        //   5. preserve caller-visible ordering by returning rows as SPARQL gave
        //      them (deterministic when ORDER BY is in the query).
        use crate::agent::did_for_context;
        use crate::perspectives::interpretation::graph::{
            gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
        };
        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let me = did_for_context(&ctx).expect("test agent DID");
        seed_message(
            &mut perspective,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello world",
            "ns://body",
        )
        .await;
        // Link timestamps are RFC3339 millis; sleep so ORDER BY ?timestamp is
        // deterministic rather than a same-millisecond tie (undefined order).
        std::thread::sleep(std::time::Duration::from_millis(2));
        seed_message(
            &mut perspective,
            &ctx,
            "msg://2",
            "did:key:bob",
            "second turn",
            "ns://body",
        )
        .await;

        let turns = gather_transcript_sparql(&perspective, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
            .await
            .expect("gather_transcript_sparql");
        assert_eq!(turns.len(), 2, "got {turns:#?}");
        assert_eq!(turns[0].text, "hello world");
        assert_eq!(turns[1].text, "second turn");
        // Speaker is the link signer (test agent), not the ns://author target.
        assert_eq!(turns[0].speaker, me);
        assert_eq!(turns[1].speaker, me);
        assert!(
            !turns[0].timestamp.is_empty() && !turns[1].timestamp.is_empty(),
            "reifier timestamp must be bound; got {turns:#?}"
        );
        assert_ne!(
            turns[0].timestamp, turns[1].timestamp,
            "sequentially seeded links must have distinct timestamps"
        );
        assert!(
            turns[0].timestamp < turns[1].timestamp,
            "ORDER BY ?timestamp must be chronological; got {turns:#?}"
        );
    }

    #[tokio::test]
    async fn gather_transcript_sparql_scopes_to_predicate() {
        // Proves this is a real scope, not a rebranded gather-everything. Seed one
        // "body"-predicated message and one "system-log" message; a query that
        // filters on `ns://body` must return exactly the first.
        use crate::perspectives::interpretation::graph::{
            gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
        };
        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        seed_message(
            &mut perspective,
            &ctx,
            "msg://human",
            "did:key:alice",
            "I'll ship the doc",
            "ns://body",
        )
        .await;
        seed_message(
            &mut perspective,
            &ctx,
            "msg://bot",
            "did:key:bot",
            "system boot",
            "ns://system_log",
        )
        .await;

        let scoped = gather_transcript_sparql(&perspective, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
            .await
            .expect("gather_transcript_sparql");
        assert_eq!(
            scoped.len(),
            1,
            "scoped query must exclude messages under other predicates; got {scoped:#?}"
        );
        assert_eq!(scoped[0].text, "I'll ship the doc");
    }

    #[tokio::test]
    async fn gather_transcript_sparql_rejects_missing_timestamp() {
        use crate::perspectives::interpretation::graph::gather_transcript_sparql;
        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        seed_message(
            &mut perspective,
            &ctx,
            "msg://1",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let err = gather_transcript_sparql(
            &perspective,
            "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . ?m <ns://author> ?speaker . }",
        )
        .await
        .expect_err("query without ?timestamp must fail");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("?timestamp"),
            "error must mention the missing binding; got {msg}"
        );
    }

    // ---- interpretation_task_name_for_model + ensure_..._for_model --------

    /// Default (None) is the shared name; Some(model) yields a distinct
    /// `?model=<id>` variant so the DB row and the AIService routing key are
    /// keyed per model. Colons inside the model id (`gemma3:12b`) are preserved.
    #[test]
    fn task_name_default_vs_per_model_variants() {
        assert_eq!(
            interpretation_task_name_for_model(None),
            INTERPRETATION_TASK_NAME
        );
        assert_eq!(
            interpretation_task_name_for_model(Some("gemma3:12b")),
            format!("{INTERPRETATION_TASK_NAME}?model=gemma3:12b")
        );
        assert_ne!(
            interpretation_task_name_for_model(Some("gemma3:12b")),
            interpretation_task_name_for_model(Some("qwen3.5-27b")),
            "per-model names must be distinct"
        );
    }

    /// `ensure_interpretation_task_for_model` creates a separate DB row per model,
    /// re-uses that row on the next call (idempotent), and never touches the
    /// shared default row when a model is specified.
    #[test]
    fn ensure_for_model_creates_isolated_row_per_model_and_is_idempotent() {
        ensure_db_init();

        // Wipe any leftover rows for the two model-specific names we're about to
        // create so this test is a real insert path regardless of order.
        let target_names = [
            interpretation_task_name_for_model(Some("gemma3:12b")),
            interpretation_task_name_for_model(Some("qwen3.5-27b")),
        ];
        let leftover: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| target_names.contains(&t.name))
            .collect();
        for t in leftover {
            Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
        }

        // DB-only primitive (no model/GPU): registers the per-model row and
        // reports whether it minted it. The async `ensure_..._for_model` wrapper
        // additionally spawns the task.
        let (gemma_first, gemma_created) =
            register_interpretation_task_for_model(Some("gemma3:12b")).unwrap();
        assert!(gemma_created, "first call after wipe must insert the row");
        assert_eq!(gemma_first.name, target_names[0]);
        assert_eq!(gemma_first.model_id, "gemma3:12b");
        assert!(gemma_first
            .system_prompt
            .contains("You extract typed instances"));

        // Idempotent: second call must return the same row, not insert a duplicate.
        let (gemma_second, gemma_created_again) =
            register_interpretation_task_for_model(Some("gemma3:12b")).unwrap();
        assert!(
            !gemma_created_again,
            "second call must find the existing row"
        );
        assert_eq!(gemma_first.task_id, gemma_second.task_id);

        // Distinct model → distinct DB row.
        let (qwen, _) = register_interpretation_task_for_model(Some("qwen3.5-27b")).unwrap();
        assert_ne!(qwen.task_id, gemma_first.task_id);
        assert_eq!(qwen.model_id, "qwen3.5-27b");

        // The default row is untouched — model overrides never mutate the shared
        // task every other caller depends on.
        let (default_row, _) = register_interpretation_task().unwrap();
        assert_eq!(default_row.model_id, "default");
        assert_ne!(default_row.task_id, gemma_first.task_id);
        assert_ne!(default_row.task_id, qwen.task_id);

        // Exactly one row per (target_name), no accidental duplicates left behind.
        for name in &target_names {
            let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
                .unwrap()
                .into_iter()
                .filter(|t| &t.name == name)
                .collect();
            assert_eq!(
                rows.len(),
                1,
                "expected exactly one row for {name}, got {}",
                rows.len()
            );
        }
    }

    // ---- dedup_ops_against_existing -----------------------------------------

    #[tokio::test]
    async fn dedup_ops_collapses_matching_creates_to_updates_keeps_new() {
        let (_perspective, shapes, _ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;

        let existing = existing_map(vec![InstanceContext {
            id: "soa://existing/intention/1".to_string(),
            title: "Ship the MVP".to_string(),
            class: "Intention".to_string(),
            properties: std::collections::BTreeMap::new(),
        }]);

        let ops = vec![
            // Matches existing by identity (title).
            InterpretationOp::Create {
                base: "soa://new/1".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("Ship the MVP"),
                )]),
            },
            // Same identity, different whitespace — also matches.
            InterpretationOp::Create {
                base: "soa://new/2".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("  ship   the   mvp  "),
                )]),
            },
            // Genuinely new — no match.
            InterpretationOp::Create {
                base: "soa://new/3".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("Write the docs"),
                )]),
            },
        ];

        let deduped = dedup_ops_against_existing(ops, &existing, &shapes, &ExistingLinks::new());
        assert_eq!(deduped.len(), 3, "all three ops survive (two as Updates)");

        // First two should be Updates on the existing base.
        match &deduped[0] {
            InterpretationOp::Update { base, class, .. } => {
                assert_eq!(base, "soa://existing/intention/1");
                assert_eq!(class, "Intention");
            }
            other => panic!("expected Update, got {other:?}"),
        }
        match &deduped[1] {
            InterpretationOp::Update { base, class, .. } => {
                assert_eq!(base, "soa://existing/intention/1");
                assert_eq!(class, "Intention");
            }
            other => panic!("expected Update, got {other:?}"),
        }
        // Third should remain a Create.
        match &deduped[2] {
            InterpretationOp::Create { class, values, .. } => {
                assert_eq!(class, "Intention");
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the docs")
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn dedup_ops_intra_batch_drops_duplicate_creates() {
        let (_perspective, shapes, _ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let existing = ExistingInstances::new(); // empty graph

        let ops = vec![
            InterpretationOp::Create {
                base: "soa://new/1".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("Ship the MVP"),
                )]),
            },
            // Duplicate of the above (normalized-equal).
            InterpretationOp::Create {
                base: "soa://new/2".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("  SHIP  the  mvp  "),
                )]),
            },
            InterpretationOp::Create {
                base: "soa://new/3".to_string(),
                class: "Intention".to_string(),
                values: serde_json::Map::from_iter([(
                    "title".to_string(),
                    serde_json::json!("Write the docs"),
                )]),
            },
        ];

        let deduped = dedup_ops_against_existing(ops, &existing, &shapes, &ExistingLinks::new());
        assert_eq!(
            deduped.len(),
            2,
            "intra-batch duplicate must be dropped; got {deduped:#?}"
        );
        match &deduped[0] {
            InterpretationOp::Create { values, .. } => {
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Ship the MVP"),
                    "first occurrence wins"
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
        match &deduped[1] {
            InterpretationOp::Create { values, .. } => {
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the docs")
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn dedup_ops_filters_existing_add_links_keeps_novel() {
        let (_perspective, shapes, _ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let existing = ExistingInstances::new();

        let mut existing_links = ExistingLinks::new();
        existing_links.insert((
            "soa://src/1".to_string(),
            "ns://basedOn".to_string(),
            "soa://tgt/old".to_string(),
        ));

        let ops = vec![InterpretationOp::AddLinks {
            source: "soa://src/1".to_string(),
            links: vec![
                // Already exists — should be filtered.
                Link {
                    source: "soa://src/1".to_string(),
                    predicate: Some("ns://basedOn".to_string()),
                    target: "soa://tgt/old".to_string(),
                },
                // Novel — should survive.
                Link {
                    source: "soa://src/1".to_string(),
                    predicate: Some("ns://basedOn".to_string()),
                    target: "soa://tgt/new".to_string(),
                },
            ],
        }];

        let deduped = dedup_ops_against_existing(ops, &existing, &shapes, &existing_links);
        assert_eq!(deduped.len(), 1, "one AddLinks op with novel link survives");
        match &deduped[0] {
            InterpretationOp::AddLinks { links, .. } => {
                assert_eq!(links.len(), 1, "only the novel link survives");
                assert_eq!(links[0].target, "soa://tgt/new");
            }
            other => panic!("expected AddLinks, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn dedup_ops_drops_all_duplicate_add_links() {
        let (_perspective, shapes, _ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let existing = ExistingInstances::new();

        let mut existing_links = ExistingLinks::new();
        existing_links.insert((
            "soa://src/1".to_string(),
            "ns://basedOn".to_string(),
            "soa://tgt/1".to_string(),
        ));

        let ops = vec![InterpretationOp::AddLinks {
            source: "soa://src/1".to_string(),
            links: vec![Link {
                source: "soa://src/1".to_string(),
                predicate: Some("ns://basedOn".to_string()),
                target: "soa://tgt/1".to_string(),
            }],
        }];

        let deduped = dedup_ops_against_existing(ops, &existing, &shapes, &existing_links);
        assert!(
            deduped.is_empty(),
            "all-duplicate AddLinks should be dropped entirely; got {deduped:#?}"
        );
    }

    #[tokio::test]
    async fn dedup_ops_passes_updates_through_unchanged() {
        let (_perspective, shapes, _ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let existing = ExistingInstances::new();

        let ops = vec![InterpretationOp::Update {
            base: "soa://existing/1".to_string(),
            class: "Intention".to_string(),
            values: serde_json::Map::from_iter([(
                "title".to_string(),
                serde_json::json!("Updated title"),
            )]),
        }];

        let deduped = dedup_ops_against_existing(ops, &existing, &shapes, &ExistingLinks::new());
        assert_eq!(deduped.len(), 1);
        assert!(matches!(&deduped[0], InterpretationOp::Update { .. }));
    }
}
