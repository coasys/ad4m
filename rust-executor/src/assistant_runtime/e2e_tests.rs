//! End-to-end tests for the assistant run subsystem.
//!
//! These drive the FULL subsystem — real in-process perspectives, real
//! subject-class link read/write, the real `run_thread` loop, real built-in
//! tool execution, and real streaming/persistence — with only the LLM replaced
//! by [`FixtureModelBackend`], which plays back RAW model text derived from
//! Qwen2.5-7B `/v1` responses captured live (`tests/fixtures/*.json`).
//!
//! The perspective is built with the same in-memory primitives the executor's
//! own perspective tests use (`Ad4mDb(:memory:)` + `AgentService` test
//! instance) and registered in the global registry via
//! [`crate::perspectives::insert_perspective_for_test`] so `get_perspective` /
//! `all_perspectives` resolve it exactly as in production.
//!
//! The tests share process-global state (`Ad4mDb`, `AgentService`), so they
//! serialize on a suite lock; run them with e.g.
//! `cargo test assistant_runtime::e2e_tests`.

use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, OnceLock};

use kalosm::language::ArcParser;
use serde_json::Value;
use tokio::sync::mpsc;
use tokio::sync::Mutex as AsyncMutex;

use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::perspectives::insert_perspective_for_test;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{
    Link, LinkQuery, LinkStatus, LocalModelInput, ModelInput, ModelType, PerspectiveHandle,
    PerspectiveState,
};

use super::entities;
use super::model_backend::{ModelBackend, TokenStream};
use super::run::{run_thread, RunInput};
use super::store;

// ---------------------------------------------------------------------------
// Fixtures (captured live from Qwen2.5-7B via /v1/chat/completions)
// ---------------------------------------------------------------------------

const CHAT_PLAIN: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/chat_plain.json"));
const TOOLCALL_BUILTIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/toolcall_builtin.json"
));
const FINAL_AFTER_TOOL: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/final_after_tool.json"
));

/// Convert a captured `/v1` chat-completion response into the RAW model text
/// the local backend would have produced: plain `content` verbatim, or each
/// tool call rendered as a Hermes `<tool_call>` block with `arguments` parsed
/// from the OpenAI JSON *string* back into an object — the exact form the
/// subsystem's real `extract_tool_calls` + fold-back expects.
fn fixture_to_raw(fixture_json: &str) -> String {
    let v: Value = serde_json::from_str(fixture_json).expect("fixture is valid JSON");
    let message = &v["choices"][0]["message"];

    if let Some(content) = message["content"].as_str() {
        return content.to_string();
    }

    if let Some(tool_calls) = message["tool_calls"].as_array() {
        let mut out = String::new();
        for call in tool_calls {
            let function = &call["function"];
            let name = function["name"].as_str().expect("tool call name");
            let args_str = function["arguments"]
                .as_str()
                .expect("tool call arguments are a JSON string");
            let args_obj: Value =
                serde_json::from_str(args_str).expect("arguments parse to an object");
            let block = serde_json::json!({ "name": name, "arguments": args_obj });
            out.push_str(&format!("<tool_call>\n{}\n</tool_call>\n", block));
        }
        return out.trim().to_string();
    }

    String::new()
}

// ---------------------------------------------------------------------------
// Fixture model backend
// ---------------------------------------------------------------------------

/// A [`ModelBackend`] that replays scripted raw responses in call order. Each
/// `stream` call pops the next response and streams it as space-delimited
/// chunks (so the real per-token content-rewrite/cadence path runs), then
/// closes the channel to signal completion.
struct FixtureModelBackend {
    responses: Mutex<VecDeque<String>>,
}

impl FixtureModelBackend {
    fn new(raw_texts: Vec<String>) -> Self {
        Self {
            responses: Mutex::new(raw_texts.into_iter().collect()),
        }
    }
}

impl ModelBackend for FixtureModelBackend {
    fn stream(
        &self,
        _model_id: String,
        _messages: Vec<(String, String)>,
        _constraint: Option<ArcParser<()>>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<TokenStream>> + Send + '_>> {
        // Pop synchronously (no lock held across the await point).
        let next = self
            .responses
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or_default();
        Box::pin(async move {
            let (tx, rx) = mpsc::unbounded_channel();
            for token in next.split_inclusive(' ') {
                let _ = tx.send(token.to_string());
            }
            // tx drops here → channel closes → the loop sees completion.
            Ok(rx)
        })
    }
}

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/// Serialize e2e tests — they share the process-global `Ad4mDb`/`AgentService`.
fn suite_lock() -> &'static AsyncMutex<()> {
    static LOCK: OnceLock<AsyncMutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| AsyncMutex::new(()))
}

/// Build a fresh in-memory perspective and register it globally.
async fn setup_perspective() -> PerspectiveInstance {
    crate::test_utils::setup_wallet();
    let _ = Ad4mDb::init_global_instance(":memory:");
    AgentService::init_global_test_instance();

    let handle = PerspectiveHandle {
        uuid: uuid::Uuid::new_v4().to_string(),
        name: Some("assistant-e2e".to_string()),
        shared_url: None,
        neighbourhood: None,
        state: PerspectiveState::Private,
        owners: None,
    };
    let instance = PerspectiveInstance::new(handle, None);
    insert_perspective_for_test(instance.clone());
    instance
}

/// Register a minimal LLM model row so the REAL model resolution in
/// `resolve_config` succeeds (the fixture backend ignores which model it is).
fn register_model(name: &str) {
    Ad4mDb::with_global_instance(|db| {
        db.add_model(&ModelInput {
            name: name.to_string(),
            api: None,
            local: Some(LocalModelInput {
                file_name: name.to_string(),
                tokenizer_source: None,
                huggingface_repo: None,
                revision: None,
            }),
            model_type: ModelType::Llm,
        })
    })
    .expect("register model row");
}

async fn add_links(p: &mut PerspectiveInstance, links: Vec<Link>) {
    let ctx = AgentContext::main_agent();
    for link in links {
        p.add_link(link, LinkStatus::Shared, None, &ctx)
            .await
            .expect("add_link");
    }
}

/// Seed an `Assistant` + `Thread`, returning `(assistant_id, thread_id)`.
async fn seed_assistant_and_thread(
    p: &mut PerspectiveInstance,
    model_name: &str,
) -> (String, String) {
    let assistant_id = format!("we://assistant/{}", uuid::Uuid::new_v4());
    let thread_id = format!("we://thread/{}", uuid::Uuid::new_v4());
    add_links(
        p,
        vec![
            entities::flag_link(&assistant_id, entities::CLASS_ASSISTANT),
            entities::property_link(&assistant_id, entities::P_NAME, "Ada"),
            entities::property_link(&assistant_id, entities::P_MODEL_ID, model_name),
            entities::property_link(
                &assistant_id,
                entities::P_SYSTEM_PROMPT,
                "You are Ada, a concise assistant.",
            ),
            entities::flag_link(&thread_id, entities::CLASS_THREAD),
            entities::property_link(&thread_id, entities::P_ASSISTANT_ID, &assistant_id),
        ],
    )
    .await;
    (assistant_id, thread_id)
}

/// Write a completed user `Message` (timestamped before any reply).
async fn write_user_message(p: &mut PerspectiveInstance, thread_id: &str, content: &str) {
    let ctx = AgentContext::main_agent();
    let msg = entities::Message {
        id: format!("we://message/{}", uuid::Uuid::new_v4()),
        thread_id: thread_id.to_string(),
        role: "user".to_string(),
        content: content.to_string(),
        tool_calls: String::new(),
        ts: "2020-01-01T00:00:00Z".to_string(),
        status: "complete".to_string(),
    };
    store::write_message(p, &msg, &ctx)
        .await
        .expect("write user message");
}

fn assistant_reply(msgs: &[entities::Message]) -> &entities::Message {
    msgs.iter()
        .find(|m| m.role == "assistant")
        .expect("an assistant reply exists")
}

async fn run_state_status(p: &PerspectiveInstance, thread_id: &str) -> Option<String> {
    for base in store::find_instances(p, entities::CLASS_RUN_STATE).await {
        let rs = entities::RunState::from_props(base.clone(), &store::load_props(p, &base).await);
        if rs.thread_id == thread_id {
            return Some(rs.status);
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Scenarios
// ---------------------------------------------------------------------------

/// Plain turn: a completed user message → the loop → the model returns plain
/// content → an assistant message with that content, `status=complete`, and a
/// `RunState` of `done`.
#[tokio::test]
async fn e2e_plain_turn_persists_complete_assistant_message() {
    let _guard = suite_lock().lock().await;

    let mut p = setup_perspective().await;
    register_model("qwen2.5");
    let (_assistant_id, thread_id) = seed_assistant_and_thread(&mut p, "qwen2.5").await;
    write_user_message(&mut p, &thread_id, "Hello, who are you?").await;

    let backend = Arc::new(FixtureModelBackend::new(vec![fixture_to_raw(CHAT_PLAIN)]));
    run_thread(
        RunInput {
            perspective_uuid: p.uuid.clone(),
            thread_id: thread_id.clone(),
        },
        backend,
    )
    .await
    .expect("run_thread");

    let msgs = store::thread_messages(&p, &thread_id).await;
    let reply = assistant_reply(&msgs);
    let expected = "Hi there! I'm Qwen, an artificial intelligence developed by Alibaba Cloud. \
                    My main function is to assist with various tasks and provide information on a \
                    wide range of topics. How can I help you today?";
    assert_eq!(reply.content, expected);
    assert_eq!(reply.status, "complete");
    assert_eq!(
        run_state_status(&p, &thread_id).await.as_deref(),
        Some("done")
    );
}

/// Tool loop: the model returns a `perspective_add_link` tool call → the
/// subsystem EXECUTES the real built-in tool (a real link is added to the
/// perspective) → the model returns the final answer. Proves real tool
/// execution + fold-back + persistence.
#[tokio::test]
async fn e2e_tool_loop_executes_builtin_tool_and_folds_result() {
    let _guard = suite_lock().lock().await;

    let mut p = setup_perspective().await;
    register_model("qwen2.5");
    let (_assistant_id, thread_id) = seed_assistant_and_thread(&mut p, "qwen2.5").await;
    write_user_message(&mut p, &thread_id, "Record that the sky is blue.").await;

    // Call 1 → perspective_add_link tool call; Call 2 → final answer.
    let backend = Arc::new(FixtureModelBackend::new(vec![
        fixture_to_raw(TOOLCALL_BUILTIN),
        fixture_to_raw(FINAL_AFTER_TOOL),
    ]));
    run_thread(
        RunInput {
            perspective_uuid: p.uuid.clone(),
            thread_id: thread_id.clone(),
        },
        backend,
    )
    .await
    .expect("run_thread");

    // (a) The tool really added the link to the perspective.
    let links = p
        .get_links(&LinkQuery {
            source: Some("sky_color_blue".to_string()),
            predicate: Some("hasColor".to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    assert_eq!(links.len(), 1, "expected exactly the tool-added link");
    assert_eq!(links[0].data.target, "blue");

    // (b) The assistant reply carries the post-tool final answer, complete.
    let msgs = store::thread_messages(&p, &thread_id).await;
    let reply = assistant_reply(&msgs);
    let expected_final = "The current temperature in San Francisco is 14 degrees Celsius \
                          and the weather condition is foggy.";
    assert_eq!(reply.content, expected_final);
    assert_eq!(reply.status, "complete");

    // (c) Message.toolCalls records the executed call.
    assert!(
        reply.tool_calls.contains("perspective_add_link"),
        "toolCalls JSON should record the call, got: {}",
        reply.tool_calls
    );

    // (d) A role:'tool' message was persisted for history/WE display.
    assert!(
        msgs.iter().any(|m| m.role == "tool"),
        "a tool-result message should be persisted"
    );

    // (e) Durable RunState finished.
    assert_eq!(
        run_state_status(&p, &thread_id).await.as_deref(),
        Some("done")
    );
}
