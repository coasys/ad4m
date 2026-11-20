//! A harness pass end to end with a scripted model (#1804): the walk tools,
//! the seed, and the single-shot fallback. No model server.

use super::run::{
    interpretation_tool_cap, run_harness_pass, DEFAULT_INTERPRETATION_TOOL_CALLS,
    RUN_MODE_FALLBACK, RUN_MODE_HARNESS,
};
use super::seed::tests::{FLAT_NOTE_SDNA, NOTE_SDNA, TOPIC_SDNA};
use super::seed::HARNESS_GUIDE;
use super::InterpretationRunCursor;
use super::{DedupStrategy, InterpretationOutcome, TranscriptTurn};
use crate::agent::AgentContext;
use crate::ai_service::harness::provider::ToolSchema;
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
use crate::perspectives::interpretation_test_support::{
    graph_titles_lower, seed_instance, seed_llm_overlay, setup_perspective_no_llm,
};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkQuery, LinkStatus};
const INTERP_RUN_CLASS: &str = "InterpretationRun";
use crate::perspectives::interpretation_test_support::model_instances;
use anyhow::Result;
use serde_json::{json, Value};
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

/// One answer of the scripted model.
enum Step {
    /// A tool call.
    Call(&'static str, Value),
    /// A tool call whose arguments come from the messages so far.
    CallWith(&'static str, fn(&[Value]) -> Value),
    /// A text answer with no tool call.
    Say(&'static str),
    /// An HTTP error from the model server.
    Fail(u16),
    /// The model server refuses the prompt: it does not fit the window.
    Overflow,
    /// An HTTP error whose body is the given text (a server that echoes the
    /// prompt).
    FailWith(u16, &'static str),
}

/// A model that answers from a script, and records what it was sent.
#[derive(Default)]
struct Scripted {
    steps: Mutex<VecDeque<Step>>,
    /// Messages and offered tool names of each completion.
    seen: Mutex<Vec<(Vec<Value>, Vec<String>)>>,
}

impl Scripted {
    fn new(steps: Vec<Step>) -> Arc<Self> {
        Arc::new(Self {
            steps: Mutex::new(steps.into()),
            seen: Mutex::default(),
        })
    }

    fn seen(&self) -> Vec<(Vec<Value>, Vec<String>)> {
        self.seen.lock().unwrap().clone()
    }

    fn left(&self) -> usize {
        self.steps.lock().unwrap().len()
    }
}

fn call(name: &str, arguments: Value) -> HarnessCompletion {
    HarnessCompletion {
        content: String::new(),
        tool_calls: vec![HarnessToolCall {
            id: format!("call-{name}"),
            name: name.to_string(),
            arguments,
        }],
    }
}

#[async_trait::async_trait]
impl CompletionSource for Scripted {
    async fn complete(
        &self,
        _model_id: &str,
        messages: &[Value],
        tools: Vec<ToolSchema>,
    ) -> Result<HarnessCompletion> {
        self.seen.lock().unwrap().push((
            messages.to_vec(),
            tools.iter().map(|t| t.name.clone()).collect(),
        ));
        let step = {
            let mut steps = self.steps.lock().unwrap();
            // A request with no tools (the wind-down, a best-effort answer,
            // the single-shot fallback) skips the scripted tool calls.
            if tools.is_empty() {
                while matches!(steps.front(), Some(Step::Call(..) | Step::CallWith(..))) {
                    steps.pop_front();
                }
            }
            steps.pop_front()
        };
        match step {
            Some(Step::Call(name, args)) => Ok(call(name, args)),
            Some(Step::CallWith(name, args)) => Ok(call(name, args(messages))),
            Some(Step::Say(text)) => Ok(HarnessCompletion {
                content: text.to_string(),
                tool_calls: vec![],
            }),
            Some(Step::Fail(code)) => Err(anyhow::anyhow!(
                crate::ai_service::providers::http::UpstreamStatus {
                    url: "http://127.0.0.1:8080/v1/chat/completions".into(),
                    status: reqwest::StatusCode::from_u16(code).unwrap(),
                    body: "scripted failure".into(),
                    retry_after: None,
                }
            )),
            Some(Step::FailWith(code, body)) => Err(anyhow::anyhow!(
                crate::ai_service::providers::http::UpstreamStatus {
                    url: "http://127.0.0.1:8080/v1/chat/completions".into(),
                    status: reqwest::StatusCode::from_u16(code).unwrap(),
                    body: body.into(),
                    retry_after: None,
                }
            )),
            Some(Step::Overflow) => Err(anyhow::anyhow!(
                crate::ai_service::providers::http::UpstreamStatus {
                    url: "http://127.0.0.1:8080/v1/chat/completions".into(),
                    status: reqwest::StatusCode::BAD_REQUEST,
                    body: "the request exceeds the available context size".into(),
                    retry_after: None,
                }
            )),
            None => Ok(HarnessCompletion {
                content: String::new(),
                tool_calls: vec![],
            }),
        }
    }
}

/// The base of the row the last `note_create` made, from its tool result.
fn created_note(messages: &[Value]) -> String {
    messages
        .iter()
        .rev()
        .filter_map(|m| m["content"].as_str())
        .find_map(|c| c.strip_prefix("created: "))
        .expect("a created row")
        .trim()
        .to_string()
}

/// The content of the tool result for the call `id`.
fn tool_result(messages: &[Value], id: &str) -> String {
    messages
        .iter()
        .find(|m| m["role"] == "tool" && m["tool_call_id"] == id)
        .and_then(|m| m["content"].as_str())
        .unwrap_or_else(|| panic!("no result for {id}"))
        .to_string()
}

/// Unregisters the perspective when the test ends, also on panic.
struct Registered(String);
impl Drop for Registered {
    fn drop(&mut self) {
        crate::perspectives::unregister_perspective(&self.0);
    }
}

/// The walk tools find a perspective by its id.
async fn register(p: &PerspectiveInstance) -> Registered {
    let uuid = p.persisted.lock().await.uuid.clone();
    crate::perspectives::register_perspective(uuid.clone(), p.clone());
    Registered(uuid)
}

async fn pass(
    p: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    ctx: &AgentContext,
    model: Arc<Scripted>,
) -> InterpretationOutcome {
    pass_with_cursor(p, shapes, ctx, model, None).await
}

/// [`pass`] with the cursor of an auto-processor batch.
async fn pass_with_cursor(
    p: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    ctx: &AgentContext,
    model: Arc<Scripted>,
    cursor: Option<&InterpretationRunCursor>,
) -> InterpretationOutcome {
    let _registered = register(p).await;
    run_harness_pass(
        p,
        shapes,
        &[TranscriptTurn::from_speaker_text(
            "Nico",
            "One more point on the budget: we need a second quote.",
        )],
        "run://pass/",
        ctx,
        None,
        None,
        cursor,
        10,
        None,
        None,
        true,
        None,
        None,
        &DedupStrategy::default(),
        false,
        Some(model),
    )
    .await
    .expect("the pass")
}

/// A topic from an earlier source with one note about it.
async fn graph_with_topic() -> (PerspectiveInstance, Vec<ModelShape>, AgentContext) {
    let (mut p, shapes, ctx) =
        setup_perspective_no_llm(&[("Topic", TOPIC_SDNA), ("Note", NOTE_SDNA)]).await;
    seed_instance(&mut p, &ctx, &shapes[0], "old://topic", "Budget").await;
    seed_instance(&mut p, &ctx, &shapes[1], "old://note", "First quote").await;
    about(&mut p, &ctx, "old://note", "old://topic").await;
    (p, shapes, ctx)
}

async fn about(p: &mut PerspectiveInstance, ctx: &AgentContext, note: &str, topic: &str) {
    p.add_link(
        Link {
            source: note.to_string(),
            predicate: Some("ns://about".to_string()),
            target: topic.to_string(),
        },
        LinkStatus::Shared,
        None,
        ctx,
    )
    .await
    .unwrap();
}

async fn about_targets(p: &PerspectiveInstance, note: &str) -> Vec<String> {
    p.get_links(&LinkQuery {
        source: Some(note.to_string()),
        predicate: Some("ns://about".to_string()),
        ..Default::default()
    })
    .await
    .unwrap()
    .into_iter()
    .map(|l| l.data.target)
    .collect()
}

fn count(titles: &[String], title: &str) -> usize {
    titles.iter().filter(|t| *t == title).count()
}

// The model walks from the topic to its neighbours, finds the topic, and
// attaches a new note to it.
#[tokio::test(flavor = "multi_thread")]
async fn the_model_walks_the_graph_and_attaches_a_new_row() {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    let model = Scripted::new(vec![
        Step::Call("get_children", json!({"parent": "old://topic"})),
        Step::Call(
            "instance_query",
            json!({"class_name": "Note", "filter": {"about": "old://topic"}, "limit": 500}),
        ),
        Step::Call(
            "note_create",
            json!({"title": "Second quote", "type": "ns://note"}),
        ),
        Step::CallWith(
            "note_propose_link_child",
            |m| json!({"note_uri": created_note(m), "predicate": "ns://about", "linked_uri": "old://topic"}),
        ),
        Step::Say("Done."),
    ]);
    let outcome = pass(&mut p, &shapes, &ctx, model.clone()).await;
    assert_eq!(outcome.mode, RUN_MODE_HARNESS);
    assert_eq!(model.left(), 0, "the whole script ran");

    let seen = model.seen();
    let (first, tools) = &seen[0];
    assert_eq!(first[0]["content"], HARNESS_GUIDE);
    let prompt: Value = serde_json::from_str(first[1]["content"].as_str().unwrap()).unwrap();
    assert!(prompt["seed"]["note"].is_string(), "{prompt}");
    for walk in ["instance_query", "get_children", "instance_get"] {
        assert!(tools.contains(&walk.to_string()), "{tools:?}");
    }
    assert!(
        !tools
            .iter()
            .any(|t| t.ends_with("_query") && t != "instance_query"),
        "no per-class read: {tools:?}"
    );

    let last = &seen.last().unwrap().0;
    let children = tool_result(last, "call-get_children");
    assert!(
        children.contains("old://note"),
        "via: all reaches the note: {children}"
    );
    assert!(children.ends_with("1 of 1 shown."), "{children}");
    let query = tool_result(last, "call-instance_query");
    assert!(query.contains("old://note"), "{query}");
    let (rows, shown) = query.rsplit_once('\n').unwrap();
    assert!(shown.ends_with("shown."), "{shown}");
    let rows: Value = serde_json::from_str(rows).unwrap();
    assert!(rows["instances"].as_array().unwrap().len() <= 50);

    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "second quote"), 1, "{titles:?}");
    let new_note = outcome
        .bases
        .iter()
        .find(|b| b.starts_with("run://pass/"))
        .expect("the new note")
        .clone();
    assert_eq!(about_targets(&p, &new_note).await, vec!["old://topic"]);
}

/// The single-shot answer of a fallback.
const FALLBACK_ANSWER: &str = r#"{"instances":[{"class":"Note","title":"From fallback"}]}"#;

/// A fallback writes the single-shot rows one time each, and none of the
/// harness proposals from before.
async fn assert_fell_back(steps: Vec<Step>) {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    let model = Scripted::new(steps);
    let outcome = pass(&mut p, &shapes, &ctx, model.clone()).await;
    assert_eq!(outcome.mode, RUN_MODE_FALLBACK);
    assert_eq!(model.left(), 0, "the whole script ran");
    let last = model.seen().last().unwrap().clone();
    assert!(last.1.is_empty(), "the fallback offers no tools");
    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "from fallback"), 1, "{titles:?}");
    assert_eq!(count(&titles, "from harness"), 0, "{titles:?}");
}

// The model server fails a round two times (#1738): the loop degrades, and
// the pass runs single-shot.
#[tokio::test(flavor = "multi_thread")]
async fn a_degraded_harness_falls_back_to_single_shot() {
    assert_fell_back(vec![
        Step::Call(
            "note_create",
            json!({"title": "From harness", "type": "ns://note"}),
        ),
        Step::Fail(500),
        Step::Fail(500),
        Step::Say("Best effort: a note From harness."),
        Step::Say(FALLBACK_ANSWER),
    ])
    .await;
}

// The model answers with text and makes no tool call.
#[tokio::test(flavor = "multi_thread")]
async fn a_text_only_answer_falls_back_to_single_shot() {
    assert_fell_back(vec![
        Step::Say("The transcript has one note: From fallback."),
        Step::Say(FALLBACK_ANSWER),
    ])
    .await;
}

// The model server refuses the request (4xx): the loop returns an error.
#[tokio::test(flavor = "multi_thread")]
async fn a_harness_error_falls_back_to_single_shot() {
    assert_fell_back(vec![
        Step::Call(
            "note_create",
            json!({"title": "From harness", "type": "ns://note"}),
        ),
        Step::Fail(400),
        Step::Say(FALLBACK_ANSWER),
    ])
    .await;
}

// Fixture B: no relation and no containment. The walk and the write work on
// a flat graph.
#[tokio::test(flavor = "multi_thread")]
async fn a_flat_social_dna_passes() {
    let (mut p, shapes, ctx) =
        setup_perspective_no_llm(&[("Topic", TOPIC_SDNA), ("Note", FLAT_NOTE_SDNA)]).await;
    seed_instance(&mut p, &ctx, &shapes[1], "run://pass/note/1", "First quote").await;
    let model = Scripted::new(vec![
        Step::Call("instance_query", json!({"class_name": "Note"})),
        Step::Call("get_children", json!({"parent": "run://pass/note/1"})),
        Step::Call(
            "note_create",
            json!({"title": "Second quote", "type": "ns://note"}),
        ),
        Step::Say("Done."),
    ]);
    let outcome = pass(&mut p, &shapes, &ctx, model.clone()).await;
    assert_eq!(outcome.mode, RUN_MODE_HARNESS);
    let seen = model.seen();
    let prompt: Value = serde_json::from_str(seen[0].0[1]["content"].as_str().unwrap()).unwrap();
    assert_eq!(
        prompt["seed"]["outline"],
        "- [Note] First quote (run://pass/note/1)"
    );
    assert!(
        !seen[0].1.contains(&"note_propose_link_child".to_string()),
        "no relation, no link tool"
    );
    let last = &seen.last().unwrap().0;
    assert!(tool_result(last, "call-get_children").ends_with("0 of 0 shown."));
    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "second quote"), 1, "{titles:?}");
    assert_eq!(count(&titles, "first quote"), 1, "{titles:?}");
}

// The harness is the default path (#1804): an absent `maxToolCalls` starts
// it with the default cap, and 0 starts single-shot.
#[test]
fn an_absent_cap_starts_the_harness_and_zero_starts_single_shot() {
    assert_eq!(
        interpretation_tool_cap(None),
        Some(DEFAULT_INTERPRETATION_TOOL_CALLS)
    );
    assert_eq!(interpretation_tool_cap(Some(0)), None);
    assert_eq!(interpretation_tool_cap(Some(7)), Some(7));
}

// A pass never rewrites a row of another source (#1804): an update of a row
// outside the pass scope keeps its content. A row in the scope changes.
// Both rows carry an interpretation overlay, so only the scope rule decides.
#[tokio::test(flavor = "multi_thread")]
async fn an_update_never_rewrites_a_row_outside_the_scope() {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    seed_llm_overlay(
        &mut p,
        &ctx,
        &shapes[0],
        "old://topic",
        json!({"title": "Budget"}),
    )
    .await;
    seed_instance(&mut p, &ctx, &shapes[0], "run://pass/topic/1", "Quotes").await;
    seed_llm_overlay(
        &mut p,
        &ctx,
        &shapes[0],
        "run://pass/topic/1",
        json!({"title": "Quotes"}),
    )
    .await;
    let model = Scripted::new(vec![
        Step::Say("No tools today."),
        Step::Say(
            r#"{"instances":[
                {"class":"Topic","id":"old://topic","title":"Budget rewritten"},
                {"class":"Topic","id":"run://pass/topic/1","title":"Quotes rewritten"}
            ]}"#,
        ),
    ]);
    let outcome = pass(&mut p, &shapes, &ctx, model).await;
    assert_eq!(outcome.mode, RUN_MODE_FALLBACK);
    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "budget"), 1, "{titles:?}");
    assert_eq!(count(&titles, "budget rewritten"), 0, "{titles:?}");
    assert_eq!(count(&titles, "quotes rewritten"), 1, "{titles:?}");
}

/// A batch whose sources the pass consumes: its run record holds them.
fn batch_cursor() -> InterpretationRunCursor {
    InterpretationRunCursor {
        processor: crate::perspectives::auto_processor::config::processor_node("cap-test"),
        sources: vec!["src://turn/1".to_string(), "src://turn/2".to_string()],
    }
}

/// The pass consumed its sources only with the fallback's result: the one
/// run record says `fallback`, the fallback row is written one time, and no
/// harness proposal is.
async fn assert_sources_go_with_the_fallback(steps: Vec<Step>) {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    let model = Scripted::new(steps);
    let cursor = batch_cursor();
    let outcome = pass_with_cursor(&mut p, &shapes, &ctx, model.clone(), Some(&cursor)).await;
    assert_eq!(outcome.mode, RUN_MODE_FALLBACK);
    assert_eq!(model.left(), 0, "the whole script ran");
    let runs = model_instances(&p, INTERP_RUN_CLASS, &["mode"]).await;
    assert_eq!(
        runs.len(),
        1,
        "one run record consumes the sources: {runs:?}"
    );
    assert_eq!(runs[0]["mode"], RUN_MODE_FALLBACK, "{runs:?}");
    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "from fallback"), 1, "{titles:?}");
    assert_eq!(count(&titles, "from harness"), 0, "{titles:?}");
}

// Lal P1 on #1812: a pass that reaches its cap (10 here) has left rows or
// links undone. It must not consume its sources with the partial buffer.
#[tokio::test(flavor = "multi_thread")]
async fn a_pass_that_reaches_its_cap_falls_back_and_keeps_no_partial_write() {
    let mut steps: Vec<Step> = (0..30)
        .map(|_| {
            Step::Call(
                "note_create",
                json!({"title": "From harness", "type": "ns://note"}),
            )
        })
        .collect();
    steps.push(Step::Say("Done."));
    steps.push(Step::Say(FALLBACK_ANSWER));
    assert_sources_go_with_the_fallback(steps).await;
}

// Lal P1 on #1812: a create refused by validation (no `type`), then a final
// answer. A tool call was made, but nothing was written.
#[tokio::test(flavor = "multi_thread")]
async fn a_pass_whose_proposals_were_all_refused_falls_back() {
    assert_sources_go_with_the_fallback(vec![
        Step::Call("note_create", json!({"title": "From harness"})),
        Step::Say("Done."),
        Step::Say(FALLBACK_ANSWER),
    ])
    .await;
}

/// Allows the first `allowed` completions, then refuses.
struct GateAfter {
    allowed: usize,
    checks: std::sync::atomic::AtomicUsize,
}

#[async_trait::async_trait]
impl crate::ai_service::harness::CreditGate for GateAfter {
    async fn check(&self) -> Result<()> {
        let n = self
            .checks
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if n < self.allowed {
            Ok(())
        } else {
            anyhow::bail!("no credits left")
        }
    }
}

/// A pass with a credit gate that allows `allowed` completions.
async fn gated_pass(
    steps: Vec<Step>,
    allowed: usize,
) -> (Result<InterpretationOutcome>, Arc<Scripted>, Vec<String>) {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    let model = Scripted::new(steps);
    let _registered = register(&p).await;
    let out = run_harness_pass(
        &mut p,
        &shapes,
        &[TranscriptTurn::from_speaker_text(
            "Nico",
            "A second quote came in.",
        )],
        "run://pass/",
        &ctx,
        None,
        None,
        Some(&batch_cursor()),
        10,
        None,
        None,
        true,
        Some(Arc::new(GateAfter {
            allowed,
            checks: Default::default(),
        })),
        None,
        &DedupStrategy::default(),
        false,
        Some(model.clone()),
    )
    .await;
    let titles = graph_titles_lower(&p, &shapes).await;
    let consumed = p
        .get_links(&LinkQuery {
            predicate: Some("ad4m://interp/sources".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert!(consumed.is_empty(), "no source is consumed: {consumed:?}");
    (out, model, titles)
}

// CodeRabbit on #1812: a credit refusal must not fall back to single-shot
// calls that the gate does not check. The pass fails, writes nothing, and
// makes no further completion.
#[tokio::test(flavor = "multi_thread")]
async fn a_credit_halt_returns_the_credit_error_and_does_not_fall_back() {
    // Refused at the first round.
    let (out, model, titles) = gated_pass(
        vec![
            Step::Call(
                "note_create",
                json!({"title": "From harness", "type": "ns://note"}),
            ),
            Step::Say(FALLBACK_ANSWER),
        ],
        0,
    )
    .await;
    let err = out.expect_err("a credit halt is an error");
    assert!(format!("{err:#}").contains("credit"), "{err:#}");
    assert!(model.seen().is_empty(), "no completion after the refusal");
    assert_eq!(count(&titles, "from fallback"), 0, "{titles:?}");

    // The server fails two times; the gate refuses the best-effort answer.
    let (out, model, titles) = gated_pass(
        vec![
            Step::Call(
                "note_create",
                json!({"title": "From harness", "type": "ns://note"}),
            ),
            Step::Fail(500),
            Step::Fail(500),
            Step::Say("Best effort."),
            Step::Say(FALLBACK_ANSWER),
        ],
        3,
    )
    .await;
    let err = out.expect_err("a credit halt is an error");
    assert!(format!("{err:#}").contains("credit"), "{err:#}");
    assert_eq!(
        model.left(),
        2,
        "neither the best-effort answer nor the fallback ran"
    );
    assert_eq!(count(&titles, "from fallback"), 0, "{titles:?}");
    assert_eq!(count(&titles, "from harness"), 0, "{titles:?}");
}

// X1 / F4 on #1821: the fallback goes through the #1808 split. The harness
// fails; the fallback's two-source batch does not fit, so it is halved; the
// first source runs, and the second, which alone does not fit, is recorded
// so the next tick does not take the batch again.
#[tokio::test(flavor = "multi_thread")]
async fn a_fallback_whose_batch_overflows_is_split_and_the_item_that_does_not_fit_is_recorded() {
    let (mut p, shapes, ctx) = graph_with_topic().await;
    let model = Scripted::new(vec![
        Step::Fail(400),
        Step::Overflow,
        Step::Say(FALLBACK_ANSWER),
        Step::Overflow,
    ]);
    let cursor = batch_cursor();
    let _registered = register(&p).await;
    let out = run_harness_pass(
        &mut p,
        &shapes,
        &[
            TranscriptTurn::from_speaker_text("Nico", "We need a second quote."),
            TranscriptTurn::from_speaker_text("Lal", "A very long turn."),
        ],
        "run://pass/",
        &ctx,
        None,
        None,
        Some(&cursor),
        10,
        None,
        None,
        true,
        None,
        None,
        &DedupStrategy::default(),
        false,
        Some(model.clone()),
    )
    .await;
    let err = out.expect_err("a source that does not fit fails the pass");
    assert!(
        format!("{err:#}").contains("does not fit the model window"),
        "{err:#}"
    );
    assert_eq!(model.left(), 0, "the whole script ran");
    let titles = graph_titles_lower(&p, &shapes).await;
    assert_eq!(count(&titles, "from fallback"), 1, "{titles:?}");
    let runs = model_instances(&p, INTERP_RUN_CLASS, &["mode"]).await;
    assert!(
        runs.iter().any(|r| r["mode"] == RUN_MODE_FALLBACK),
        "the half that ran is a fallback run: {runs:?}"
    );
    let processed = crate::perspectives::auto_processor::cursor::load_processed_source_ids(
        &p,
        "cap-test",
        chrono::Utc::now().timestamp_millis(),
        None,
    )
    .await
    .expect("cursor");
    for source in &cursor.sources {
        assert!(
            processed.contains(source),
            "{source} is recorded (run, or does not fit): {processed:?}"
        );
    }
}

/// A tool surface with one tool that answers nothing.
struct OneTool;

#[async_trait::async_trait]
impl crate::ai_service::harness::provider::ToolProvider for OneTool {
    async fn tools(&self) -> Vec<ToolSchema> {
        vec![ToolSchema::zero_arg("lookup", "")]
    }
    async fn call(&self, _name: &str, _args: Value) -> Result<String> {
        Ok(String::new())
    }
}

// F11 on #1821: after the third upstream failure (a round, its retry, and the
// final request without tools) the fallback reason names the status and not
// the response body, which can echo the prompt.
#[tokio::test(flavor = "multi_thread")]
async fn the_fallback_reason_of_an_upstream_failure_carries_no_response_body() {
    const ECHO: &str = "ECHOED-PROMPT-MARKER: Nico said the budget needs a second quote";
    let model = Scripted::new(vec![
        Step::FailWith(500, ECHO),
        Step::FailWith(500, ECHO),
        Step::FailWith(500, ECHO),
    ]);
    let result = crate::ai_service::harness::run_with_tools_outcome(
        "m",
        vec![json!({"role": "user", "content": "find it"})],
        Arc::new(OneTool),
        model.clone(),
        crate::ai_service::harness::HarnessConfig::default(),
        None,
        None,
    )
    .await;
    assert_eq!(
        model.left(),
        0,
        "the round, its retry and the final request ran"
    );
    assert!(result.is_err(), "the final request failed too");
    let reason = super::fallback_reason(&result, 0, 0).expect("an error falls back");
    assert!(reason.contains("HTTP 500"), "{reason}");
    assert!(
        !reason.contains("ECHOED-PROMPT-MARKER"),
        "the reason carries the upstream body: {reason}"
    );
}
