//! `/v1` as an aggregator, driven through the handlers against mock upstreams: what is
//! forwarded and what the executor answers itself, the `model` rewrite, SSE, billing
//! amounts, the error map of plan §5.4, the concurrency cap, embeddings by id and by
//! name, speech on its own endpoint, and the aggregated `/v1/models`.

use axum::response::IntoResponse;
use http_body_util::BodyExt;
use mockito::Matcher;
use serde_json::{json, Value};

use super::errors::OpenAIJson;
use super::model_selector::{resolve, Resolved};
use super::passthrough::Upstream;
use super::{audio, chat, embeddings, models};
use crate::agent::capabilities::{
    AI_PROMPT_CAPABILITY, AI_READ_CAPABILITY, AI_TRANSCRIBE_CAPABILITY,
};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::ai_service::estimate_token_count;
use crate::billing::test_seam;
use crate::db::Ad4mDb;
use crate::secret::Secret;
use crate::types::{
    AiCapability, Endpoint, EndpointHealth, EndpointKind, EndpointState, ModelInput, ModelType,
    RequestContext,
};

const USER: &str = "did:key:passthrough-user";

/// A fresh database, billing recorded and not charged.
fn fresh() {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::Success);
}

fn endpoint(name: &str, kind: EndpointKind, url: &str, key: &str) -> Endpoint {
    Endpoint {
        id: uuid::Uuid::new_v4().to_string(),
        name: name.into(),
        kind,
        base_url: url::Url::parse(url).unwrap(),
        api_key: Secret::new(key),
        capabilities: kind.default_capabilities(),
        max_num_ctx: None,
        request_timeout_secs: None,
        max_concurrent: None,
        health: EndpointHealth::default(),
    }
}

fn save(endpoint: &Endpoint) {
    Ad4mDb::with_global_instance(|db| db.insert_endpoint(endpoint)).unwrap();
}

/// A model `name` serving `remote` on `endpoint`.
fn model(name: &str, model_type: ModelType, endpoint: &Endpoint, remote: &str) -> String {
    model_with_tools(name, model_type, endpoint, remote, None)
}

fn model_with_tools(
    name: &str,
    model_type: ModelType,
    endpoint: &Endpoint,
    remote: &str,
    native_tools: Option<bool>,
) -> String {
    let input = ModelInput {
        name: name.into(),
        model_type,
        endpoint_id: endpoint.id.clone(),
        remote_model: remote.into(),
        native_tools,
    };
    Ad4mDb::with_global_instance(|db| db.add_model(&input)).unwrap()
}

fn rate(name: &str, price: f64) {
    Ad4mDb::with_global_instance(|db| db.set_host_rates(&[(name.to_string(), price)])).unwrap();
}

fn user() -> RequestContext {
    RequestContext::for_session(
        Session::for_test(Principal::Agent(AgentContext::managed(USER.into()))).with_caps(vec![
            AI_PROMPT_CAPABILITY.clone(),
            AI_READ_CAPABILITY.clone(),
        ]),
    )
}

async fn chat(ctx: RequestContext, body: Value) -> axum::response::Response {
    chat::chat_completions(ctx, OpenAIJson(body))
        .await
        .into_response()
}

async fn body_text(response: axum::response::Response) -> String {
    let bytes = response.into_body().collect().await.unwrap().to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

async fn body_json(response: axum::response::Response) -> Value {
    serde_json::from_str(&body_text(response).await).unwrap()
}

fn hello() -> Value {
    json!([{ "role": "user", "content": "hi" }])
}

/// Two mock upstreams' worth of setup: an OpenAI-shaped endpoint with a key and one model.
async fn openai_model(server: &mockito::ServerGuard, key: &str) -> (Endpoint, String) {
    let upstream = endpoint("vllm-box", EndpointKind::Vllm, &server.url(), key);
    save(&upstream);
    let id = model("chat", ModelType::Llm, &upstream, "Qwen/Qwen3-8B");
    (upstream, id)
}

#[tokio::test]
async fn a_chat_reaches_the_endpoint_as_written_with_only_the_model_rewritten() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let (upstream, model_id) = openai_model(&server, "sk-upstream").await;
    rate("chat", 0.5);
    let tools = json!([{ "type": "function", "function": { "name": "lookup", "parameters": {} } }]);
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .match_header("authorization", "Bearer sk-upstream")
        .match_body(Matcher::Json(json!({
            "model": "Qwen/Qwen3-8B",
            "messages": hello(),
            "response_format": { "type": "json_object" },
            "tools": tools,
            "tool_choice": "none",
            "x_vendor_field": 7,
        })))
        .with_body(
            json!({
                "id": "chatcmpl-1",
                "object": "chat.completion",
                "model": "Qwen/Qwen3-8B",
                "choices": [{ "index": 0, "message": { "role": "assistant", "content": "{}" }, "finish_reason": "stop" }],
                "usage": { "prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15 },
            })
            .to_string(),
        )
        .expect(1)
        .create_async()
        .await;

    let response = chat(
        user(),
        json!({
            "model": "chat",
            "messages": hello(),
            "response_format": { "type": "json_object" },
            "tools": tools,
            "tool_choice": "none",
            "x_vendor_field": 7,
        }),
    )
    .await;

    assert_eq!(response.status(), 200);
    let answer = body_json(response).await;
    mock.assert_async().await;
    assert_eq!(answer["model"], "chat", "the client's name comes back");
    assert_eq!(answer["choices"][0]["message"]["content"], "{}");
    assert_eq!(answer["usage"]["total_tokens"], 15);

    let calls = test_seam::calls();
    assert_eq!(calls.len(), 1, "{calls:?}");
    assert_eq!(calls[0].agent_did, USER);
    assert_eq!(calls[0].operation, "ai_prompt");
    assert!(
        (calls[0].amount - 7.5).abs() < 1e-9,
        "15 upstream tokens at 0.5"
    );
    assert_eq!(
        calls[0].summary.as_deref(),
        Some(format!("15 tokens (model: chat, endpoint: {})", upstream.id).as_str())
    );
    let _ = model_id;
}

#[tokio::test]
async fn without_upstream_usage_the_request_and_the_answer_are_estimated() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    rate("chat", 1.0);
    server
        .mock("POST", "/v1/chat/completions")
        .with_body(
            json!({ "model": "Qwen/Qwen3-8B", "choices": [{ "message": { "content": "hello world" } }] })
                .to_string(),
        )
        .create_async()
        .await;

    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 200);

    let expected = estimate_token_count(&hello().to_string()) + estimate_token_count("hello world");
    let calls = test_seam::calls();
    assert_eq!(calls.len(), 1, "{calls:?}");
    assert!(
        (calls[0].amount - expected as f64).abs() < 1e-9,
        "{calls:?}"
    );
}

fn sse(events: &[Value]) -> String {
    let mut body: String = events.iter().map(|e| format!("data: {e}\n\n")).collect();
    body.push_str("data: [DONE]\n\n");
    body
}

fn chunk(content: &str) -> Value {
    json!({ "object": "chat.completion.chunk", "model": "Qwen/Qwen3-8B",
            "choices": [{ "index": 0, "delta": { "content": content } }] })
}

fn usage_chunk() -> Value {
    json!({ "object": "chat.completion.chunk", "model": "Qwen/Qwen3-8B", "choices": [],
            "usage": { "prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5 } })
}

/// The `data` of each SSE event in `text`.
fn events(text: &str) -> Vec<String> {
    text.split("\n\n")
        .filter_map(|event| event.strip_prefix("data: "))
        .map(str::to_string)
        .collect()
}

#[tokio::test]
async fn a_stream_asks_for_usage_bills_it_and_passes_the_events_with_the_clients_model() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    rate("chat", 2.0);
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .match_body(Matcher::PartialJson(json!({
            "model": "Qwen/Qwen3-8B",
            "stream": true,
            "stream_options": { "include_usage": true },
        })))
        .with_header("content-type", "text/event-stream")
        .with_body(sse(&[chunk("Hel"), chunk("lo"), usage_chunk()]))
        .create_async()
        .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["content-type"], "text/event-stream");
    let events = events(&body_text(response).await);
    mock.assert_async().await;

    assert_eq!(
        events.len(),
        3,
        "the usage-only event it did not ask for is dropped: {events:?}"
    );
    let first: Value = serde_json::from_str(&events[0]).unwrap();
    assert_eq!(first["model"], "chat");
    assert_eq!(first["choices"][0]["delta"]["content"], "Hel");
    assert_eq!(events[2], "[DONE]");

    let calls = test_seam::calls();
    assert_eq!(calls.len(), 1, "{calls:?}");
    assert!(
        (calls[0].amount - 10.0).abs() < 1e-9,
        "5 tokens at 2.0: {calls:?}"
    );
}

#[tokio::test]
async fn a_client_that_asks_for_usage_gets_the_usage_event() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    server
        .mock("POST", "/v1/chat/completions")
        .with_body(sse(&[chunk("x"), usage_chunk()]))
        .create_async()
        .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true,
                "stream_options": { "include_usage": true } }),
    )
    .await;
    let events = events(&body_text(response).await);
    let usage: Value = serde_json::from_str(&events[1]).unwrap();
    assert_eq!(usage["usage"]["total_tokens"], 5);
    assert_eq!(usage["model"], "chat");
}

#[tokio::test]
async fn ollama_gets_tools_through_its_openai_compatible_api() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let ollama = endpoint("ollama", EndpointKind::Ollama, &server.url(), "");
    save(&ollama);
    model("qwen", ModelType::Llm, &ollama, "qwen3:8b");
    let tools = json!([{ "type": "function", "function": { "name": "f", "parameters": {} } }]);
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .match_body(Matcher::PartialJson(
            json!({ "model": "qwen3:8b", "tools": tools, "tool_choice": "required" }),
        ))
        .with_body(json!({ "model": "qwen3:8b", "choices": [] }).to_string())
        .expect(1)
        .create_async()
        .await;

    let response = chat(
        user(),
        json!({ "model": "qwen", "messages": hello(), "tools": tools, "tool_choice": "required" }),
    )
    .await;
    assert_eq!(response.status(), 200);
    mock.assert_async().await;
}

#[tokio::test]
async fn a_stream_the_upstream_breaks_off_ends_with_an_error_event_and_bills_nothing() {
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    fresh();
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    tokio::spawn(async move {
        let (mut socket, _) = listener.accept().await.unwrap();
        let mut request = [0u8; 4096];
        let _ = socket.read(&mut request).await;
        let event = format!("data: {}\n\n", chunk("par"));
        let head = "HTTP/1.1 200 OK\r\ncontent-type: text/event-stream\r\ntransfer-encoding: chunked\r\n\r\n";
        // One chunk, then the connection drops before the last one.
        let body = format!("{head}{:x}\r\n{event}\r\n", event.len());
        socket.write_all(body.as_bytes()).await.unwrap();
    });
    let flaky = endpoint(
        "flaky",
        EndpointKind::LlamaServer,
        &format!("http://{address}"),
        "",
    );
    save(&flaky);
    model("chat", ModelType::Llm, &flaky, "m");
    rate("chat", 1.0);

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    let events = events(&body_text(response).await);
    assert_eq!(events.len(), 2, "{events:?}");
    let last: Value = serde_json::from_str(&events[1]).unwrap();
    assert_eq!(last["error"]["type"], "upstream_error");
    assert!(test_seam::calls().is_empty(), "{:?}", test_seam::calls());
}

#[tokio::test]
async fn a_text_completion_is_forwarded_too() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    let mock = server
        .mock("POST", "/v1/completions")
        .match_body(Matcher::Json(
            json!({ "model": "Qwen/Qwen3-8B", "prompt": "Once" }),
        ))
        .with_body(
            json!({ "model": "Qwen/Qwen3-8B", "choices": [{ "text": " upon" }] }).to_string(),
        )
        .expect(1)
        .create_async()
        .await;

    let response = chat::completions(
        user(),
        OpenAIJson(json!({ "model": "chat", "prompt": "Once" })),
    )
    .await
    .into_response();
    assert_eq!(response.status(), 200);
    assert_eq!(body_json(response).await["model"], "chat");
    mock.assert_async().await;
}

/// The client's view of each upstream answer: status, error code, and what reaches it.
async fn failure(
    status: usize,
    body: &str,
    header: Option<(&str, &str)>,
) -> axum::response::Response {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "sk-secret-key").await;
    let mut mock = server
        .mock("POST", "/v1/chat/completions")
        .with_status(status)
        .with_body(body);
    if let Some((name, value)) = header {
        mock = mock.with_header(name, value);
    }
    mock.create_async().await;
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert!(test_seam::calls().is_empty(), "a failure bills nothing");
    response
}

#[tokio::test]
async fn a_rejected_request_keeps_its_status_and_reason_without_the_key() {
    let response = failure(
        400,
        r#"{"error":{"message":"bad schema for key sk-secret-key"}}"#,
        None,
    )
    .await;
    assert_eq!(response.status(), 400);
    let error = body_json(response).await;
    assert_eq!(error["error"]["message"], "bad schema for key <redacted>");

    let response = failure(422, r#"{"detail":"messages: field required"}"#, None).await;
    assert_eq!(response.status(), 422);
    assert_eq!(
        body_json(response).await["error"]["message"],
        "messages: field required"
    );
}

#[tokio::test]
async fn an_upstream_401_is_a_502_never_the_clients_401() {
    for status in [401, 403] {
        let response = failure(status, "invalid key sk-secret-key", None).await;
        assert_eq!(response.status(), 502);
        let text = body_text(response).await;
        assert!(text.contains("upstream_auth"), "{text}");
        assert!(!text.contains("sk-secret-key"), "{text}");
    }
}

#[tokio::test]
async fn a_missing_model_is_a_404_model_not_found() {
    let response = failure(404, r#"{"error":{"message":"model not loaded"}}"#, None).await;
    assert_eq!(response.status(), 404);
    assert_eq!(
        body_json(response).await["error"]["code"],
        "model_not_found"
    );
}

#[tokio::test]
async fn a_throttle_passes_its_retry_after() {
    let response = failure(429, "slow down", Some(("retry-after", "7"))).await;
    assert_eq!(response.status(), 429);
    assert_eq!(response.headers()["retry-after"], "7");
}

#[tokio::test]
async fn an_upstream_5xx_is_a_502_upstream_error() {
    let response = failure(500, "boom", None).await;
    assert_eq!(response.status(), 502);
    assert_eq!(body_json(response).await["error"]["type"], "upstream_error");
}

#[tokio::test]
async fn an_endpoint_that_refuses_connections_is_a_503() {
    fresh();
    // Nothing listens on port 1.
    let down = endpoint("down", EndpointKind::LlamaServer, "http://127.0.0.1:1", "");
    save(&down);
    model("chat", ModelType::Llm, &down, "m");
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 503);
    assert_eq!(
        body_json(response).await["error"]["type"],
        "upstream_unavailable"
    );
}

#[tokio::test]
async fn an_endpoint_that_does_not_answer_in_its_timeout_is_a_504() {
    fresh();
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    let _held = tokio::spawn(async move {
        let (_socket, _) = listener.accept().await.unwrap();
        std::future::pending::<()>().await;
    });
    let mut silent = endpoint(
        "silent",
        EndpointKind::LlamaServer,
        &format!("http://{address}"),
        "",
    );
    silent.request_timeout_secs = Some(1);
    save(&silent);
    model("chat", ModelType::Llm, &silent, "m");

    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 504);
    assert_eq!(
        body_json(response).await["error"]["type"],
        "upstream_timeout"
    );
}

#[tokio::test]
async fn a_capped_endpoint_queues_and_the_wait_counts_against_the_timeout() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .with_body(json!({ "choices": [] }).to_string())
        .expect(1)
        .create_async()
        .await;
    let mut capped = endpoint("capped", EndpointKind::Vllm, &server.url(), "");
    capped.max_concurrent = Some(1);
    capped.request_timeout_secs = Some(1);
    save(&capped);
    model("chat", ModelType::Llm, &capped, "m");

    // The one slot is taken: the request waits for it, then times out.
    let held = crate::ai_service::endpoints::request_slot(&capped).await;
    assert!(held.is_some());
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 504);

    drop(held);
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 200);
    mock.assert_async().await;
}

#[tokio::test]
async fn without_a_cap_there_is_no_slot_to_wait_for() {
    let open = endpoint("open", EndpointKind::Vllm, "http://127.0.0.1:1", "");
    assert!(crate::ai_service::endpoints::request_slot(&open)
        .await
        .is_none());
}

#[tokio::test]
async fn a_caller_without_the_prompt_capability_is_refused_before_any_upstream() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    let mock = server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    let app = || RequestContext::app_for_test(vec![AI_READ_CAPABILITY.clone()]);

    let chat_reply = chat(app(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(chat_reply.status(), 403);
    let embed = embeddings::embeddings(
        app(),
        OpenAIJson(serde_json::from_value(json!({"model": "chat", "input": "x"})).unwrap()),
    )
    .await
    .into_response();
    assert_eq!(embed.status(), 403);
    let speech = audio::speech(app(), OpenAIJson(json!({ "model": "tts", "input": "x" })))
        .await
        .into_response();
    assert_eq!(speech.status(), 403);
    let list = models::list_models(RequestContext::app_for_test(vec![
        AI_PROMPT_CAPABILITY.clone()
    ]))
    .await
    .into_response();
    assert_eq!(list.status(), 403);
    mock.assert_async().await;
}

fn resolved(kind: EndpointKind, native_tools: Option<bool>) -> Resolved {
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let upstream = endpoint("e", kind, "http://127.0.0.1:9", "");
    save(&upstream);
    let id = model_with_tools("m", ModelType::Llm, &upstream, "remote", native_tools);
    Resolved {
        model: Ad4mDb::with_global_instance(|db| db.get_model(id))
            .unwrap()
            .unwrap(),
        endpoint: upstream,
    }
}

// The rule: forward unless the endpoint is Anthropic (translated by its client) or the
// request has active tools the model does not take as definitions (rendered into the
// prompt by the executor).
#[test]
fn what_is_forwarded_and_what_the_executor_answers() {
    let tools = json!({ "tools": [{ "type": "function", "function": { "name": "f" } }] });
    let plain = json!({});

    let openai = resolved(EndpointKind::OpenAiCompatible, None);
    assert!(Upstream::for_chat(&openai, &plain).is_some());
    assert!(
        Upstream::for_chat(&openai, &tools).is_none(),
        "an OpenAI-shaped model takes tools as text unless it says so"
    );
    let native = resolved(EndpointKind::LlamaServer, Some(true));
    assert!(Upstream::for_chat(&native, &tools).is_some());

    let ollama = resolved(EndpointKind::Ollama, None);
    assert!(
        Upstream::for_chat(&ollama, &tools).is_some(),
        "Ollama takes tools"
    );

    let anthropic = resolved(EndpointKind::Anthropic, None);
    assert!(Upstream::of(&anthropic).is_none());
}

#[tokio::test]
async fn a_model_resolves_by_endpoint_name_and_remote_model() {
    fresh();
    let upstream = endpoint("ollama", EndpointKind::Ollama, "http://127.0.0.1:11434", "");
    save(&upstream);
    let id = model("qwen", ModelType::Llm, &upstream, "qwen3:8b");
    let found = resolve("ollama/qwen3:8b", ModelType::Llm).await.unwrap();
    assert_eq!(found.model.id, id);
    assert_eq!(found.endpoint.id, upstream.id);
    assert!(resolve("ollama/other", ModelType::Llm).await.is_err());
}

#[tokio::test]
async fn models_are_listed_by_name_across_endpoints_with_their_status() {
    fresh();
    let mut ollama = endpoint("ollama", EndpointKind::Ollama, "http://127.0.0.1:11434", "");
    ollama.health = EndpointHealth {
        state: EndpointState::Reachable,
        checked_at: Some(1),
        detail: None,
        served_models: vec!["nomic-embed-text:latest".into()],
    };
    save(&ollama);
    let mut speaches = endpoint(
        "speaches",
        EndpointKind::OpenAiAudio,
        "http://127.0.0.1:8000",
        "",
    );
    speaches.health.state = EndpointState::Unreachable;
    save(&speaches);
    model("embed", ModelType::Embedding, &ollama, "nomic-embed-text");
    model("chat", ModelType::Llm, &ollama, "qwen3:8b");
    model("voice", ModelType::Speech, &speaches, "kokoro");

    let list = models::list_models(user()).await.unwrap().0;
    let list = serde_json::to_value(list).unwrap();
    let entry = |id: &str| {
        list["data"]
            .as_array()
            .unwrap()
            .iter()
            .find(|m| m["id"] == id)
            .unwrap_or_else(|| panic!("{id} listed: {list}"))
            .clone()
    };
    let embed = entry("embed");
    assert_eq!(embed["owned_by"], "ollama");
    assert_eq!(embed["ad4m"]["modelType"], "EMBEDDING");
    assert_eq!(embed["ad4m"]["endpointId"], ollama.id.as_str());
    assert_eq!(embed["ad4m"]["remoteModel"], "nomic-embed-text");
    assert_eq!(embed["ad4m"]["status"]["state"], "READY");
    assert_eq!(entry("chat")["ad4m"]["status"]["state"], "MISSING");
    let voice = entry("voice");
    assert_eq!(voice["owned_by"], "speaches");
    assert_eq!(voice["ad4m"]["modelType"], "SPEECH");
    assert_eq!(voice["ad4m"]["status"]["state"], "ENDPOINT_DOWN");
}

/// An embedding model on a mock OpenAI-shaped endpoint, loaded into a fresh AIService.
async fn embedding_model(server: &mockito::ServerGuard) -> (String, crate::ai_service::AIService) {
    fresh();
    let upstream = endpoint("embedder", EndpointKind::LlamaServer, &server.url(), "");
    save(&upstream);
    let id = model(
        "nomic",
        ModelType::Embedding,
        &upstream,
        "nomic-embed-text-v1.5",
    );
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    let service = crate::ai_service::AIService::global_instance()
        .await
        .unwrap();
    service.reload_model(id.clone()).await.unwrap();
    (id, service)
}

async fn embed_mock(
    server: &mut mockito::ServerGuard,
    inputs: Value,
    hits: usize,
) -> mockito::Mock {
    let n = inputs.as_array().unwrap().len();
    server
        .mock("POST", "/v1/embeddings")
        .match_body(Matcher::Json(json!({
            "model": "nomic-embed-text-v1.5",
            "input": inputs,
            "encoding_format": "float",
        })))
        .with_body(
            json!({
                "data": (0..n).map(|i| json!({ "index": i, "embedding": [i as f32, 1.0] })).collect::<Vec<_>>(),
                "usage": { "prompt_tokens": 4, "total_tokens": 4 },
            })
            .to_string(),
        )
        .expect(hits)
        .create_async()
        .await
}

#[tokio::test]
async fn v1_embeddings_by_id_and_by_name_go_to_the_endpoint_in_one_batch() {
    let mut server = mockito::Server::new_async().await;
    let mock = embed_mock(&mut server, json!(["a", "b"]), 2).await;
    let (id, _service) = embedding_model(&server).await;
    rate("nomic", 0.25);

    for requested in [id.as_str(), "nomic"] {
        let request =
            serde_json::from_value(json!({ "model": requested, "input": ["a", "b"] })).unwrap();
        let reply = embeddings::embeddings(user(), OpenAIJson(request))
            .await
            .unwrap_or_else(|e| panic!("{requested}: {e:?}"))
            .0;
        let reply = serde_json::to_value(reply).unwrap();
        assert_eq!(reply["model"], requested);
        assert_eq!(reply["data"][1]["embedding"], json!([1.0, 1.0]));
        assert_eq!(reply["usage"]["prompt_tokens"], 4);
    }
    mock.assert_async().await;

    let calls = test_seam::calls();
    assert_eq!(calls.len(), 2, "{calls:?}");
    assert!(calls
        .iter()
        .all(|c| c.operation == "ai_embedding" && (c.amount - 1.0).abs() < 1e-9));
}

#[tokio::test]
async fn ai_embed_and_semantic_dedup_embed_by_name_on_the_endpoint() {
    let mut server = mockito::Server::new_async().await;
    let single = embed_mock(&mut server, json!(["one"]), 1).await;
    let batch = embed_mock(&mut server, json!(["x", "y", "z"]), 1).await;
    let (_id, _service) = embedding_model(&server).await;

    crate::api::ws_handler::build_handler_map()
        .dispatch(
            "ai.embed",
            json!({ "modelId": "nomic", "text": "one" }),
            std::sync::Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .expect("ai.embed by name");
    single.assert_async().await;

    let texts: Vec<String> = ["x", "y", "z"].iter().map(|s| s.to_string()).collect();
    let vectors = crate::perspectives::interpretation::embed_via_ai_service("nomic", &texts)
        .await
        .unwrap();
    assert_eq!(vectors.len(), 3);
    batch.assert_async().await;
}

#[tokio::test]
async fn speech_goes_to_the_speech_endpoint_with_its_key_only() {
    fresh();
    let mut llm_server = mockito::Server::new_async().await;
    let llm_mock = llm_server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    // An LLM on an OpenAI endpoint with its own key, which the old TTS path borrowed.
    let llm = endpoint("openai", EndpointKind::OpenAi, &llm_server.url(), "sk-llm");
    save(&llm);
    model("gpt", ModelType::Llm, &llm, "gpt-5");

    let mut speech_server = mockito::Server::new_async().await;
    let mut speaches = endpoint(
        "speaches",
        EndpointKind::OpenAiAudio,
        &speech_server.url(),
        "sk-speech",
    );
    speaches.capabilities = vec![AiCapability::Transcription, AiCapability::Speech];
    save(&speaches);
    model("voice", ModelType::Speech, &speaches, "kokoro");
    let speech_mock = speech_server
        .mock("POST", "/v1/audio/speech")
        .match_header("authorization", "Bearer sk-speech")
        .match_body(Matcher::Json(
            json!({ "model": "kokoro", "input": "hello", "voice": "af_sky" }),
        ))
        .with_header("content-type", "audio/wav")
        .with_body(b"RIFFaudio")
        .expect(1)
        .create_async()
        .await;

    let response = audio::speech(
        user(),
        OpenAIJson(json!({ "model": "voice", "input": "hello", "voice": "af_sky" })),
    )
    .await
    .into_response();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["content-type"], "audio/wav");
    assert_eq!(body_text(response).await, "RIFFaudio");
    speech_mock.assert_async().await;
    llm_mock.assert_async().await;

    // An LLM is not a speech model, and a speech model needs an endpoint that speaks.
    let refused = audio::speech(user(), OpenAIJson(json!({ "model": "gpt", "input": "x" })))
        .await
        .into_response();
    assert_eq!(refused.status(), 400);
    let mut mute = endpoint("mute", EndpointKind::LlamaServer, "http://127.0.0.1:9", "");
    mute.capabilities = vec![AiCapability::Chat];
    save(&mute);
    model("mute-voice", ModelType::Speech, &mute, "x");
    // Linking the model gave the endpoint the speech capability; the operator takes it back.
    Ad4mDb::with_global_instance(|db| db.update_endpoint(&mute)).unwrap();
    let refused = audio::speech(
        user(),
        OpenAIJson(json!({ "model": "mute-voice", "input": "x" })),
    )
    .await
    .into_response();
    assert_eq!(refused.status(), 400);
    assert_eq!(
        body_json(refused).await["error"]["code"],
        "model_not_supported"
    );
}

/// Paid hosting, and `USER` has no credits left.
fn user_without_credits() {
    Ad4mDb::with_global_instance(|db| {
        db.set_free_hosting_enabled(false)?;
        db.add_user("broke@example.org", USER, "password")
    })
    .unwrap();
}

fn transcriber() -> RequestContext {
    RequestContext::for_session(
        Session::for_test(Principal::Agent(AgentContext::managed(USER.into())))
            .with_caps(vec![AI_TRANSCRIBE_CAPABILITY.clone()]),
    )
}

/// A `/v1/audio/transcriptions` form for `model` with a few bytes of WAV.
async fn transcription_form(model: &str) -> axum::extract::Multipart {
    use axum::extract::FromRequest;
    let boundary = "passthrough-test-boundary";
    let body = format!(
        "--{boundary}\r\nContent-Disposition: form-data; name=\"model\"\r\n\r\n{model}\r\n\
         --{boundary}\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.wav\"\r\n\
         Content-Type: audio/wav\r\n\r\nRIFFaudio\r\n--{boundary}--\r\n"
    );
    let request = axum::http::Request::builder()
        .method("POST")
        .header(
            "content-type",
            format!("multipart/form-data; boundary={boundary}"),
        )
        .body(axum::body::Body::from(body))
        .unwrap();
    axum::extract::Multipart::from_request(request, &())
        .await
        .unwrap()
}

/// A transcription model `stt` on an audio endpoint at `server`.
fn transcription_model(server: &mockito::ServerGuard) {
    let speaches = endpoint("speaches", EndpointKind::OpenAiAudio, &server.url(), "");
    save(&speaches);
    model(
        "stt",
        ModelType::Transcription,
        &speaches,
        "faster-whisper-small",
    );
}

#[tokio::test]
async fn a_user_without_credits_reaches_no_endpoint() {
    fresh();
    user_without_credits();
    let mut server = mockito::Server::new_async().await;
    let upstream = server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    openai_model(&server, "sk-upstream").await;
    transcription_model(&server);

    let chat = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(chat.status(), 429);
    assert_eq!(body_json(chat).await["error"]["code"], "insufficient_quota");

    let transcription = audio::transcriptions(transcriber(), transcription_form("stt").await)
        .await
        .into_response();
    assert_eq!(transcription.status(), 429);
    upstream.assert_async().await;
    assert!(test_seam::calls().is_empty());
}

#[tokio::test]
async fn a_users_transcription_is_billed_to_the_user_by_its_words() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    server
        .mock("POST", "/v1/audio/transcriptions")
        .with_header("content-type", "application/json")
        .with_body(r#"{"text":"one two three"}"#)
        .create_async()
        .await;
    transcription_model(&server);
    rate("stt", 0.5);

    let response = audio::transcriptions(transcriber(), transcription_form("stt").await)
        .await
        .into_response();

    assert_eq!(response.status(), 200);
    let calls = test_seam::calls();
    assert_eq!(calls.len(), 1, "{calls:?}");
    assert_eq!(calls[0].agent_did, USER);
    assert_eq!(calls[0].operation, concat!("ai_", "transcription"));
    assert_eq!(calls[0].amount, 1.5, "3 words at 0.5");
}
