//! A harness pass end to end with a scripted model (#1804): the walk tools,
//! the seed, and the single-shot fallback. No model server.

use super::run::{run_harness_pass, RUN_MODE_FALLBACK};
use super::InterpretationRunCursor;
use super::{DedupStrategy, InterpretationOutcome, TranscriptTurn};
use crate::agent::AgentContext;
use crate::ai_service::harness::provider::ToolSchema;
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
use crate::perspectives::interpretation_test_support::{
    graph_titles_lower, seed_instance, setup_perspective_no_llm,
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

const TOPIC_SDNA: &str = r#"{
  "target_class":"ns://Topic",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://topic"}],
  "properties":[
{"path":"ns://type","name":"type","has_value":"ns://topic","min_count":1,"max_count":1},
{"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

const NOTE_SDNA: &str = r#"{
  "target_class":"ns://Note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://note"}],
  "properties":[
{"path":"ns://type","name":"type","has_value":"ns://note","min_count":1,"max_count":1},
{"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
{"path":"ns://about","name":"about","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape"}
  ]
}"#;

/// One answer of the scripted model.
enum Step {
    /// A tool call.
    Call(&'static str, Value),
    /// A text answer with no tool call.
    Say(&'static str),
    /// An HTTP error from the model server.
    Fail(u16),
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
                while matches!(steps.front(), Some(Step::Call(..))) {
                    steps.pop_front();
                }
            }
            steps.pop_front()
        };
        match step {
            Some(Step::Call(name, args)) => Ok(call(name, args)),
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
            None => Ok(HarnessCompletion {
                content: String::new(),
                tool_calls: vec![],
            }),
        }
    }
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

fn count(titles: &[String], title: &str) -> usize {
    titles.iter().filter(|t| *t == title).count()
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
