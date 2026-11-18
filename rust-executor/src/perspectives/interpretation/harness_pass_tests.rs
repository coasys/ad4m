//! A harness pass end to end with a scripted model (#1804): the walk tools,
//! the seed, and the single-shot fallback. No model server.

use super::run::{
    interpretation_tool_cap, run_harness_pass, DEFAULT_INTERPRETATION_TOOL_CALLS,
    RUN_MODE_FALLBACK, RUN_MODE_HARNESS,
};
use super::seed::tests::{FLAT_NOTE_SDNA, NOTE_SDNA, TOPIC_SDNA};
use super::seed::HARNESS_GUIDE;
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
        let step = self.steps.lock().unwrap().pop_front();
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
        None,
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
