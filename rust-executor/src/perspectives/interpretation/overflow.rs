//! Split an interpretation batch that does not fit the model window.
//!
//! The watch loop retries a pass that returns an error, and it only retires
//! turns named by an `InterpretationRun`. A half that runs is recorded by
//! [`super::run::run_interpretation_once`]. A single source that still does
//! not fit is recorded the same way, with the error on `debug_response` and
//! not the prompt, so the next tick does not queue it again.
//!
//! The split lives on the interpretation run. The watch loop only needs a
//! failure `InterpretationRun` to stop queueing that source; it does not
//! have to know that the batch was divided.

use super::run::run_interpretation_once;
use super::{
    ensure_interpretation_overlay_classes, ensure_interpretation_task_for_model,
    mint_interpretation_run, DedupStrategy, InterpretationOutcome, InterpretationRunCursor,
    InterpretationRunMeta, TranscriptTurn,
};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::ops::Range;

/// Run `transcript`, splitting it in half whenever the prompt does not fit
/// the model window. A transient error stops the walk: halves already
/// recorded stay recorded, and the rest are left for the next pass.
#[allow(clippy::too_many_arguments)]
pub(super) async fn split_overflowing_batch(
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
    flow_filter: Option<&[String]>,
) -> anyhow::Result<InterpretationOutcome> {
    if transcript.is_empty()
        || cursor.is_some_and(|cursor| cursor.sources.len() != transcript.len())
    {
        if cursor.is_some_and(|cursor| cursor.sources.len() != transcript.len()) {
            log::warn!(
                "interpretation: cursor has {} sources for {} transcript turns; running the batch whole",
                cursor.map(|cursor| cursor.sources.len()).unwrap_or(0),
                transcript.len()
            );
        }
        return run_interpretation_once(
            perspective,
            shapes,
            transcript,
            base_prefix,
            context,
            dedup_strategy,
            model_override,
            scope,
            cursor,
            emit_debug_events,
            emit_ctx,
            flow_filter,
        )
        .await;
    }

    // The left half is pushed last so it runs first. One range, not every index:
    // `vec![0..n]` is a single `Range`, which clippy reads as a mistake.
    let mut pending: Vec<Range<usize>> = Vec::with_capacity(1);
    pending.push(0..transcript.len());
    let mut outcomes: Vec<InterpretationOutcome> = Vec::new();
    let mut failures: Vec<String> = Vec::new();
    while let Some(range) = pending.pop() {
        let slice_cursor = cursor.map(|cursor| InterpretationRunCursor {
            processor: cursor.processor.clone(),
            sources: cursor.sources[range.clone()].to_vec(),
        });
        let slice = &transcript[range.clone()];
        let result = run_interpretation_once(
            perspective,
            shapes,
            slice,
            base_prefix,
            context,
            dedup_strategy,
            model_override,
            scope,
            slice_cursor.as_ref(),
            emit_debug_events,
            emit_ctx,
            flow_filter,
        )
        .await;
        match result {
            Ok(outcome) => outcomes.push(outcome),
            Err(error)
                if crate::ai_service::prompt_does_not_fit(&error) && range_len(&range) > 1 =>
            {
                let mid = range.start + range_len(&range) / 2;
                log::info!(
                    "interpretation: the prompt does not fit the model window; splitting {} turns into {} and {}",
                    range_len(&range),
                    mid - range.start,
                    range.end - mid
                );
                pending.push(mid..range.end);
                pending.push(range.start..mid);
            }
            Err(error) if crate::ai_service::prompt_does_not_fit(&error) => {
                let message = format!(
                    "interpretation context overflow: a single source item does not fit the model window and will not be retried. {error:#}"
                );
                log::warn!("{message}");
                if let Some(slice_cursor) = slice_cursor.as_ref() {
                    if let Err(mint_error) = record_item_that_does_not_fit(
                        perspective,
                        context,
                        model_override,
                        slice_cursor,
                        &message,
                    )
                    .await
                    {
                        return Err(anyhow::anyhow!(
                            "{message}; recording the failure so it is not retried also failed: {mint_error:#}"
                        ));
                    }
                }
                failures.push(message);
            }
            Err(error) => return Err(error),
        }
    }

    if !failures.is_empty() {
        return Err(anyhow::anyhow!("{}", failures.join("\n")));
    }
    Ok(merge_outcomes(outcomes))
}

fn range_len(range: &Range<usize>) -> usize {
    range.end.saturating_sub(range.start)
}

fn merge_outcomes(outcomes: Vec<InterpretationOutcome>) -> InterpretationOutcome {
    let mut bases = Vec::new();
    let mut flow_proposals = Vec::new();
    let mut debug = None;
    for outcome in outcomes {
        bases.extend(outcome.bases);
        flow_proposals.extend(outcome.flow_proposals);
        debug = outcome.debug.or(debug);
    }
    InterpretationOutcome {
        bases,
        debug,
        flow_proposals,
    }
}

/// Mint a run whose only job is to name `cursor`'s source, so the watch
/// cursor does not queue that source again. The prompt is not stored.
async fn record_item_that_does_not_fit(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
    model_override: Option<&str>,
    cursor: &InterpretationRunCursor,
    message: &str,
) -> anyhow::Result<()> {
    let task = ensure_interpretation_task_for_model(model_override)?;
    ensure_interpretation_overlay_classes(perspective, context).await?;
    let mut meta = InterpretationRunMeta::from_task(
        &task,
        uuid::Uuid::new_v4().to_string(),
        chrono::Utc::now().timestamp_millis().to_string(),
    );
    meta.debug_response = Some(message.to_string());
    let batch_id = perspective.create_batch().await;
    match mint_interpretation_run(
        perspective,
        &meta,
        Some(cursor),
        Some(batch_id.clone()),
        context,
    )
    .await
    {
        Ok(_) => {
            if let Err(error) = perspective.commit_batch(batch_id.clone(), context).await {
                let _ = perspective.discard_batch(&batch_id).await;
                return Err(anyhow::anyhow!(
                    "interpretation overflow: commit of the failure run failed: {error:#}"
                ));
            }
        }
        Err(error) => {
            let _ = perspective.discard_batch(&batch_id).await;
            return Err(error);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::{
        build_interpretation_input, build_speaker_name_map, ensure_interpretation_task_for_model,
        run_interpretation_with_strategy_and_model, DedupStrategy, ExistingInstances,
        InterpretationRunCursor, TranscriptTurn, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
    };
    use crate::ai_service::{estimate_token_count, AIService};
    use crate::db::Ad4mDb;
    use crate::perspectives::auto_processor::config::{
        processor_node, write_processor, AutoProcessorConfig,
    };
    use crate::perspectives::auto_processor::cursor::load_processed_source_ids;
    use crate::perspectives::auto_processor::watcher::{PendingTurn, WatcherState};
    use crate::perspectives::interpretation::decode_literal_string;
    use crate::perspectives::interpretation::gather_transcript_sparql;
    use crate::perspectives::interpretation_test_support::{
        ensure_db_init, setup_perspective_no_llm, TASK_SDNA,
    };
    use crate::types::{AITask, AITaskInput, Endpoint, EndpointKind, ModelInput, ModelType};

    /// Per-message and per-request overhead in [`crate::ai_service`] `estimate_turns_tokens`.
    const MESSAGE_OVERHEAD_TOKENS: usize = 8;
    const REQUEST_OVERHEAD_TOKENS: usize = 16;

    /// The chars/4 budget [`AIService::prompt_interpretation`] uses today:
    /// system prompt, each example side, then the user prompt, plus template overhead.
    fn raw_turns_budget(task: &AITask, user_prompt: &str) -> usize {
        let mut tokens = REQUEST_OVERHEAD_TOKENS
            + estimate_token_count(&task.system_prompt)
            + MESSAGE_OVERHEAD_TOKENS;
        for example in &task.prompt_examples {
            tokens += estimate_token_count(&example.input) + MESSAGE_OVERHEAD_TOKENS;
            tokens += estimate_token_count(&example.output) + MESSAGE_OVERHEAD_TOKENS;
        }
        tokens + estimate_token_count(user_prompt) + MESSAGE_OVERHEAD_TOKENS
    }

    /// Ceil of 110%. The production check must refuse what this count refuses.
    fn margin(estimated: usize) -> usize {
        estimated.saturating_mul(11).saturating_add(9) / 10
    }

    const EMPTY_REPLY: &str =
        r#"{"choices":[{"message":{"content":"[]"},"finish_reason":"stop"}]}"#;

    fn body_text(request: &mockito::Request) -> String {
        String::from_utf8(request.body().unwrap().to_vec()).unwrap()
    }

    async fn llama_model(base_url: &str, max_num_ctx: u32) -> (AIService, String) {
        AIService::init_global_instance().await.expect("AIService");
        let service = AIService::global_instance()
            .await
            .expect("AIService instance");
        service.wait_loaded().await;
        // The name and the key are unique per call. Tests share one in-memory
        // database, and mockito reuses a server address, which the endpoint
        // table refuses to store twice under the same key.
        let name = format!("overflow-{}", uuid::Uuid::new_v4());
        let mut endpoint = Endpoint::for_test(&name, EndpointKind::LlamaServer, base_url, &name);
        endpoint.max_num_ctx = Some(max_num_ctx);
        Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).expect("insert endpoint");
        let model_id = service
            .add_model(ModelInput {
                name: format!("overflow-{}", uuid::Uuid::new_v4()),
                model_type: ModelType::Llm,
                endpoint_id: endpoint.id,
                remote_model: "m".into(),
                native_tools: None,
            })
            .await
            .expect("add model");
        service
            .set_default_model(ModelType::Llm, model_id.clone())
            .await
            .expect("default model");
        (service, model_id)
    }

    fn user_prompt(
        shapes: &[crate::perspectives::model_query::types::ModelShape],
        turns: &[TranscriptTurn],
    ) -> String {
        let existing = ExistingInstances::new();
        let names = build_speaker_name_map(turns);
        build_interpretation_input(shapes, turns, &existing, &[], &names)
    }

    /// A prompt the raw chars/4 estimate places exactly on the 512-token floor
    /// must be refused once the count carries a 10% margin.
    ///
    /// System `"s"` is 1 token. `"p" * 36000` is 9,000. With the 16 + 8 + 8
    /// template overhead the raw total is 9,033. A window of `raw + 512` leaves
    /// exactly 512, which [`AIService::interpretation_max_output_tokens`] allows.
    /// Ceil(9033 * 1.1) is 9,937 and leaves no room.
    #[tokio::test]
    async fn a_ten_percent_margin_refuses_a_prompt_the_raw_estimate_allows() {
        ensure_db_init();
        let system = "s";
        let user = "p".repeat(36_000);
        let raw = REQUEST_OVERHEAD_TOKENS
            + estimate_token_count(system)
            + MESSAGE_OVERHEAD_TOKENS
            + estimate_token_count(&user)
            + MESSAGE_OVERHEAD_TOKENS;
        assert_eq!(
            raw, 9_033,
            "the fixture must sit where the staging gap was measured"
        );
        // Staging: 70,206 estimated against 76,870 measured. This margin clears that gap.
        assert!(
            margin(70_206) >= 76_870,
            "10% ceil must cover the measured under-count"
        );
        let window = raw + 512;
        assert_eq!(
            AIService::interpretation_max_output_tokens(Some(window), raw).unwrap(),
            Some(512),
            "the raw estimate is the largest prompt the old check still sends"
        );
        let margined = margin(raw);
        assert!(
            AIService::interpretation_max_output_tokens(Some(window), margined).is_err(),
            "the margined count must be over the floor"
        );

        let mut upstream = mockito::Server::new_async().await;
        let _completion = upstream
            .mock("POST", "/v1/chat/completions")
            .with_body(EMPTY_REPLY)
            .create_async()
            .await;
        let (service, model_id) = llama_model(&upstream.url(), window as u32).await;
        let task = service
            .add_task(
                AITaskInput {
                    name: format!("overflow-margin-{}", uuid::Uuid::new_v4()),
                    model_id,
                    system_prompt: system.into(),
                    prompt_examples: vec![],
                    meta_data: None,
                },
                None,
            )
            .await
            .expect("task");

        let error = service
            .prompt_interpretation(task.task_id, user)
            .await
            .expect_err("a prompt the raw estimate lets through must be refused");
        let message = format!("{error:#}");
        assert!(
            message.contains("context budget"),
            "refusal must be the context-budget error, got {message}"
        );
        assert!(
            message.contains(&margined.to_string()),
            "the budget must be charged the margined count {margined}, got {message}"
        );
    }

    /// Two transcript turns that together exceed the window, and either of
    /// which fits, are run as two halves. The unsplit prompt never reaches
    /// the model.
    #[tokio::test]
    async fn an_over_window_batch_splits_and_both_halves_run() {
        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        let turns = vec![
            TranscriptTurn::from_speaker_text(
                "alice",
                format!("TURN_ALPHA {}", "a".repeat(16_000)),
            ),
            TranscriptTurn::from_speaker_text("bob", format!("TURN_BETA {}", "b".repeat(16_000))),
        ];
        let task = ensure_interpretation_task_for_model(None).expect("interpretation task");
        let raw_one = raw_turns_budget(&task, &user_prompt(&shapes, &turns[..1]));
        let raw_two = raw_turns_budget(&task, &user_prompt(&shapes, &turns));
        let window = margin(raw_one) + 512;
        assert!(
            AIService::interpretation_max_output_tokens(Some(window), raw_one).is_ok(),
            "one turn fits the raw budget (one={raw_one} window={window})"
        );
        assert!(
            AIService::interpretation_max_output_tokens(Some(window), raw_two).is_err(),
            "the unsplit batch does not fit even the raw budget, so today's path refuses it \
             (one={raw_one} two={raw_two} window={window})"
        );
        assert!(
            AIService::interpretation_max_output_tokens(Some(window), margin(raw_one))
                .unwrap()
                .is_some(),
            "a margined half still has room for an answer"
        );

        let mut upstream = mockito::Server::new_async().await;
        let alpha = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_ALPHA") && !body.contains("TURN_BETA")
            })
            .with_body(EMPTY_REPLY)
            .expect(1)
            .create_async()
            .await;
        let beta = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_BETA") && !body.contains("TURN_ALPHA")
            })
            .with_body(EMPTY_REPLY)
            .expect(1)
            .create_async()
            .await;
        // Newest mock wins. An unsplit prompt matches this and not the halves.
        let both = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_ALPHA") && body.contains("TURN_BETA")
            })
            .with_body(EMPTY_REPLY)
            .expect(0)
            .create_async()
            .await;
        llama_model(&upstream.url(), window as u32).await;

        let cursor = InterpretationRunCursor {
            processor: processor_node("overflow-split"),
            sources: vec!["src-alpha".into(), "src-beta".into()],
        };
        let outcome = run_interpretation_with_strategy_and_model(
            &mut perspective,
            &shapes,
            &turns,
            "ad4m://overflow/",
            &ctx,
            &DedupStrategy::default(),
            None,
            None,
            Some(&cursor),
            false,
            None,
            None,
        )
        .await;
        assert!(
            outcome.is_ok(),
            "an over-window batch must split and both halves run, got {outcome:?}"
        );
        alpha.assert_async().await;
        beta.assert_async().await;
        both.assert_async().await;
        let processed = load_processed_source_ids(
            &perspective,
            "overflow-split",
            chrono::Utc::now().timestamp_millis(),
            None,
        )
        .await
        .expect("cursor");
        assert!(
            processed.contains("src-alpha") && processed.contains("src-beta"),
            "both halves must be consumed, got {processed:?}"
        );
    }

    /// The server, not the local budget, rejects the whole batch. The same
    /// split applies: the oversized prompt is not retried as one piece.
    #[tokio::test]
    async fn a_server_context_overflow_splits_the_batch() {
        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        let turns = vec![
            TranscriptTurn::from_speaker_text("alice", format!("TURN_ALPHA {}", "a".repeat(4_000))),
            TranscriptTurn::from_speaker_text("bob", format!("TURN_BETA {}", "b".repeat(4_000))),
        ];
        // Far above the prompt, so the local budget lets the request out and
        // the server is what refuses it.
        let window = 200_000u32;
        let mut upstream = mockito::Server::new_async().await;
        let alpha = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_ALPHA") && !body.contains("TURN_BETA")
            })
            .with_body(EMPTY_REPLY)
            .expect(1)
            .create_async()
            .await;
        let beta = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_BETA") && !body.contains("TURN_ALPHA")
            })
            .with_body(EMPTY_REPLY)
            .expect(1)
            .create_async()
            .await;
        let both = upstream
            .mock("POST", "/v1/chat/completions")
            .match_request(|request| {
                let body = body_text(request);
                body.contains("TURN_ALPHA") && body.contains("TURN_BETA")
            })
            .with_status(400)
            .with_body("context length exceeded")
            .expect(2)
            .create_async()
            .await;
        llama_model(&upstream.url(), window).await;

        let cursor = InterpretationRunCursor {
            processor: processor_node("overflow-server"),
            sources: vec!["src-alpha".into(), "src-beta".into()],
        };
        let outcome = run_interpretation_with_strategy_and_model(
            &mut perspective,
            &shapes,
            &turns,
            "ad4m://overflow/",
            &ctx,
            &DedupStrategy::default(),
            None,
            None,
            Some(&cursor),
            false,
            None,
            None,
        )
        .await;
        assert!(
            outcome.is_ok(),
            "a server context overflow must split the batch, got {outcome:?}"
        );
        alpha.assert_async().await;
        beta.assert_async().await;
        both.assert_async().await;
    }

    fn overflow_notes(
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
    ) -> String {
        let Ok(json) = perspective.sparql_query(
            "SELECT ?resp WHERE { ?run <ad4m://interp/debug_response> ?resp }".into(),
        ) else {
            return String::new();
        };
        let rows: Vec<serde_json::Value> = serde_json::from_str(&json).unwrap_or_default();
        rows.into_iter()
            .filter_map(|row| {
                let raw = row.get("resp")?.as_str()?.to_string();
                Some(decode_literal_string(&raw).unwrap_or(raw))
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// One source that cannot fit fails once, is recorded, and is not queued
    /// again on the next tick.
    #[tokio::test]
    async fn a_single_item_over_the_window_fails_once_and_is_not_retried() {
        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        crate::perspectives::interpretation_test_support::seed_message(
            &mut perspective,
            &ctx,
            "msg://overflow-one",
            "did:key:alice",
            "hello",
            "ns://body",
        )
        .await;
        let turns = gather_transcript_sparql(&perspective, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY)
            .await
            .expect("gather");
        assert_eq!(turns.len(), 1, "the seeded message is the only turn");
        let source_id = PendingTurn::from_transcript(&turns[0]).id;

        let mut upstream = mockito::Server::new_async().await;
        let completions = upstream
            .mock("POST", "/v1/chat/completions")
            .with_body(EMPTY_REPLY)
            .expect(0)
            .create_async()
            .await;
        // Smaller than the interpretation system prompt, so one turn is refused
        // before the model sees it.
        llama_model(&upstream.url(), 1_024).await;

        let cfg = AutoProcessorConfig {
            processor_id: "overflow-once".into(),
            source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            interpretation_classes: vec![shapes[0].target_class.clone()],
            debounce_ms: 0,
            batch_min: 1,
            batch_max: 8,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut perspective, &cfg, Some(false), &ctx)
            .await
            .expect("write processor");

        let now_ms = chrono::Utc::now().timestamp_millis();
        let mut watcher = WatcherState::new();
        let first = perspective
            .run_auto_processor_tick(&mut watcher, now_ms, &ctx)
            .await;
        assert!(
            first.polls_at_base_rate(),
            "the turn is pending when the tick starts"
        );
        let notes = overflow_notes(&perspective);
        assert!(
            notes.contains("will not be retried"),
            "a single over-window item must fail once with a clear error, got {notes:?}"
        );
        let processed = load_processed_source_ids(&perspective, "overflow-once", now_ms, None)
            .await
            .expect("cursor");
        assert!(
            processed.contains(&source_id),
            "the failed item must be consumed so it is not lost and not retried, got {processed:?}"
        );
        let second = perspective
            .run_auto_processor_tick(&mut watcher, now_ms + 1_000, &ctx)
            .await;
        assert!(
            !second.polls_at_base_rate(),
            "a consumed overflow must not be retried on the next tick ({second:?})"
        );
        completions.assert_async().await;
    }
}
