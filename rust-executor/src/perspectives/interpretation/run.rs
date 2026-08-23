use super::{
    apply_with_overlay, build_interpretation_input, class_label,
    ensure_interpretation_task_for_model, existing_instance_context, existing_relation_links,
    identity_property, parse_interpretation_response, plan_interpretation_ops_resolved,
    resolve_already_present_with_strategy, DedupStrategy, InterpretationOp,
    InterpretationRunCursor, ProposedInstance, TranscriptTurn,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::LinkStatus;
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
    )
    .await
}

/// [`run_interpretation_with_strategy`] with an optional per-call LLM model
/// override — routes the interpretation prompt through the AI-task DB row
/// bound to `model_override` (falling back to the shared default row when
/// `None`).
///
/// `model_override = None` reuses the exact task row every existing caller
/// already uses, so behaviour is unchanged for all non-processor callers.
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
) -> anyhow::Result<Vec<String>> {
    // Returns a task already spawned into its LLM worker, so `prompt` can use it
    // immediately (see `ensure_interpretation_task_for_model`).
    let task = ensure_interpretation_task_for_model(model_override).await?;
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
            identity_property(s).map(|idp| (class_label(&s.target_class, shapes), idp.name.clone()))
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
                // Internal caller (interpretation runner) — no user auth context; billing skipped.
                .prompt(task_id, prompt, None)
                .await
                .map_err(|e| anyhow::anyhow!("AIService::prompt failed: {e:#}"))?;
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
    let bases = apply_with_overlay(
        perspective,
        shapes,
        ops,
        &task,
        run_id,
        ran_at,
        context,
        cursor,
    )
    .await?;

    // The affected instance base URIs (created, updated, or given new
    // relations). Links are owned by `create_subject` / `update_subject`.
    Ok(bases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;
    use crate::types::AITask;

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
}
