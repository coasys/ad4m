//! A whole session against a real perspective, with a scripted model in place of an LLM: what the
//! model is offered, what it may call, and what reaches the graph.

use super::agent_session::{
    converse, session_tools, AgentMessage, AgentSessionError, AgentSessionRequest,
};
use crate::ai_service::harness::provider::ToolSchema;
use crate::ai_service::harness::{
    CompletionSource, CreditGate, HarnessCompletion, HarnessToolCall,
};
use crate::mcp::server::McpContext;
use crate::mcp::tools::Ad4mMcpHandler;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::{register_perspective, unregister_perspective};
use anyhow::Result;
use serde_json::{json, Value};
use std::sync::{Arc, Mutex};

const NOTE_SDNA: &str = r#"{
  "target_class": "ns://Note",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Note"}
  ],
  "properties": [
    {"path":"ns://note_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://note_title","target":"value"}]}
  ]
}"#;

/// A model that answers from a script and remembers what it was shown.
struct Scripted {
    script: Mutex<Vec<HarnessCompletion>>,
    seen: Mutex<Vec<(Vec<Value>, Vec<ToolSchema>)>>,
}

impl Scripted {
    fn new(script: Vec<HarnessCompletion>) -> Arc<Self> {
        Arc::new(Self {
            script: Mutex::new(script),
            seen: Mutex::new(Vec::new()),
        })
    }
    fn tools_offered(&self) -> Vec<ToolSchema> {
        self.seen.lock().unwrap()[0].1.clone()
    }
    /// What the tool calls of the last turn answered, as the model was told.
    fn tool_results(&self) -> Vec<String> {
        let seen = self.seen.lock().unwrap();
        seen.last()
            .unwrap()
            .0
            .iter()
            .filter(|m| m["role"] == "tool")
            .map(|m| m["content"].as_str().unwrap_or_default().to_string())
            .collect()
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
        self.seen.lock().unwrap().push((messages.to_vec(), tools));
        let mut script = self.script.lock().unwrap();
        Ok(if script.is_empty() {
            HarnessCompletion {
                content: "done".into(),
                tool_calls: vec![],
            }
        } else {
            script.remove(0)
        })
    }
}

fn call(name: &str, arguments: Value) -> HarnessCompletion {
    HarnessCompletion {
        content: String::new(),
        tool_calls: vec![HarnessToolCall {
            id: format!("call-{name}"),
            name: name.into(),
            arguments,
        }],
    }
}

struct Fixture {
    uuid: String,
    other: String,
    handler: Arc<Ad4mMcpHandler>,
    note: String,
}

impl Drop for Fixture {
    fn drop(&mut self) {
        unregister_perspective(&self.uuid);
        unregister_perspective(&self.other);
    }
}

async fn fixture() -> Fixture {
    let mut uuids = Vec::new();
    for _ in 0..2 {
        let (perspective, _shapes, _ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let uuid = perspective.persisted.lock().await.uuid.clone();
        register_perspective(uuid.clone(), perspective);
        uuids.push(uuid);
    }
    let handler = Arc::new(Ad4mMcpHandler::new(McpContext {
        admin_credential: Some("test-admin".to_string()),
        auth_token: Arc::new(tokio::sync::RwLock::new(Some("test-admin".to_string()))),
        dynamic_class_tools: false,
    }));
    let mut notes = Vec::new();
    for uuid in &uuids {
        let created: Value = serde_json::from_str(
            &handler
                .call_tool_by_name(
                    "instance_create",
                    json!({ "perspective_id": uuid, "class_name": "Note", "properties": { "title": "first" } }),
                )
                .await
                .unwrap(),
        )
        .unwrap();
        notes.push(created["base_uri"].as_str().unwrap().to_string());
    }
    Fixture {
        uuid: uuids[0].clone(),
        other: uuids[1].clone(),
        handler,
        note: notes[0].clone(),
    }
}

async fn title(f: &Fixture, uuid: &str, note: &str) -> Value {
    let got: Value = serde_json::from_str(
        &f.handler
            .call_tool_by_name(
                "instance_get",
                json!({ "perspective_id": uuid, "class_name": "Note", "base_uri": note }),
            )
            .await
            .unwrap(),
    )
    .unwrap();
    got["title"].clone()
}

fn asked(content: &str) -> Vec<AgentMessage> {
    vec![AgentMessage {
        role: "user".into(),
        content: content.into(),
    }]
}

fn request<'a>(
    f: &'a Fixture,
    messages: &'a [AgentMessage],
    write_tools: &'a [String],
) -> AgentSessionRequest<'a> {
    AgentSessionRequest {
        perspective_uuid: &f.uuid,
        messages,
        context: None,
        write_tools,
        model: None,
        max_tool_calls: 10,
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_session_offers_reads_and_only_the_writes_it_was_given() {
    let f = fixture().await;
    let messages = asked("what notes are there?");

    let model = Scripted::new(vec![]);
    let tools = session_tools(f.handler.clone(), &f.uuid, &[]);
    let reply = converse(
        &request(&f, &messages, &[]),
        tools,
        model.clone(),
        "m",
        None,
    )
    .await
    .unwrap();
    assert_eq!(reply.text, "done");
    let offered: Vec<String> = model.tools_offered().into_iter().map(|t| t.name).collect();
    for read in [
        "describe_perspective",
        "instance_query",
        "get_documentation",
    ] {
        assert!(
            offered.contains(&read.to_string()),
            "offers {read}: {offered:?}"
        );
    }
    for absent in [
        "instance_update",
        "instance_create",
        "generate_jwt",
        "login_email",
    ] {
        assert!(
            !offered.contains(&absent.to_string()),
            "does not offer {absent}"
        );
    }
    assert!(
        model
            .tools_offered()
            .iter()
            .all(|t| t.parameters["properties"].get("perspective_id").is_none()),
        "the perspective is bound, never asked for"
    );

    let write = vec!["instance_update".to_string()];
    let model = Scripted::new(vec![]);
    let tools = session_tools(f.handler.clone(), &f.uuid, &write);
    converse(
        &request(&f, &messages, &write),
        tools,
        model.clone(),
        "m",
        None,
    )
    .await
    .unwrap();
    let offered: Vec<String> = model.tools_offered().into_iter().map(|t| t.name).collect();
    assert!(offered.contains(&"instance_update".to_string()));
    assert!(!offered.contains(&"instance_create".to_string()));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_write_it_was_not_given_is_refused_even_when_the_model_names_it() {
    let f = fixture().await;
    let messages = asked("rename it");
    let model = Scripted::new(vec![call(
        "instance_update",
        json!({ "class_name": "Note", "base_uri": f.note, "properties": { "title": "renamed" } }),
    )]);
    let tools = session_tools(f.handler.clone(), &f.uuid, &[]);
    converse(
        &request(&f, &messages, &[]),
        tools,
        model.clone(),
        "m",
        None,
    )
    .await
    .unwrap();
    assert!(
        model.tool_results()[0].contains("not in the current provider's filtered surface"),
        "{:?}",
        model.tool_results()
    );
    assert_eq!(title(&f, &f.uuid, &f.note).await, "first");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_write_it_was_given_lands_in_its_own_perspective_only() {
    let f = fixture().await;
    let messages = asked("rename it");
    let write = vec!["instance_update".to_string()];
    let model = Scripted::new(vec![call(
        "instance_update",
        // The model names the other perspective out of band; the session's binding wins.
        json!({ "perspective_id": f.other, "class_name": "Note", "base_uri": f.note, "properties": { "title": "renamed" } }),
    )]);
    let tools = session_tools(f.handler.clone(), &f.uuid, &write);
    let reply = converse(
        &request(&f, &messages, &write),
        tools,
        model.clone(),
        "m",
        None,
    )
    .await
    .unwrap();
    assert_eq!(reply.text, "done");
    assert_eq!(title(&f, &f.uuid, &f.note).await, "renamed");
}

struct Broke;
#[async_trait::async_trait]
impl CreditGate for Broke {
    async fn check(&self) -> Result<()> {
        anyhow::bail!("no credits")
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_session_stopped_by_credits_says_so_rather_than_answering_empty() {
    let f = fixture().await;
    let messages = asked("anything");
    let model = Scripted::new(vec![]);
    let tools = session_tools(f.handler.clone(), &f.uuid, &[]);
    let outcome = converse(
        &request(&f, &messages, &[]),
        tools,
        model,
        "m",
        Some(Arc::new(Broke)),
    )
    .await;
    assert!(
        matches!(outcome, Err(AgentSessionError::OutOfCredits)),
        "{outcome:?}"
    );
}
