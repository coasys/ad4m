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
        managed: false,
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

// Forwarded to Ollama's `/v1`, which ignores `options.num_ctx`, a long prompt with tools was
// cut without an error. `/v1` answers Ollama through its native `/api/chat` with the model's
// context window.
#[tokio::test]
async fn ollama_is_answered_through_its_native_api_with_its_context_window() {
    let mut server = mockito::Server::new_async().await;
    let show = server
        .mock("POST", "/api/show")
        .with_body(json!({ "model_info": { "qwen3.context_length": 40960 } }).to_string())
        .create_async()
        .await;
    let native = server
        .mock("POST", "/api/chat")
        .match_body(Matcher::PartialJson(json!({
            "model": "m",
            "options": { "num_ctx": 40960 },
            "tools": [{ "type": "function", "function": { "name": "get_time" } }],
        })))
        .with_body(
            json!({ "model": "m", "message": { "role": "assistant", "content": "noon" },
                    "done": true, "prompt_eval_count": 3, "eval_count": 1 })
            .to_string(),
        )
        .expect(1)
        .create_async()
        .await;
    let openai = server
        .mock("POST", "/v1/chat/completions")
        .expect(0)
        .create_async()
        .await;
    answered_here(EndpointKind::Ollama, &server.url(), "", None).await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": a_tool() }),
    )
    .await;
    assert_eq!(response.status(), 200);
    show.assert_async().await;
    native.assert_async().await;
    openai.assert_async().await;
}

// A stream that failed was billed nothing, though the upstream had generated its text.
#[tokio::test]
async fn a_stream_the_upstream_breaks_off_ends_with_an_error_event_and_bills_what_it_generated() {
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
    assert_billed_estimate("par");
}

/// The bill the test seam recorded: one call, the estimate of `hello()` plus `generated`.
fn assert_billed_estimate(generated: &str) {
    let expected = estimate_token_count(&hello().to_string()) + estimate_token_count(generated);
    let calls = test_seam::calls();
    assert_eq!(calls.len(), 1, "{calls:?}");
    assert!(
        (calls[0].amount - expected as f64).abs() < 1e-9,
        "{calls:?}, expected {expected}"
    );
}

/// A streaming upstream on a raw socket: `chunks` as SSE bytes, each after its delay in
/// milliseconds, then the end of the body when `end`, else silence on an open connection.
/// The endpoint's timeout is `timeout_secs`.
async fn scripted_stream(chunks: Vec<(u64, String)>, end: bool, timeout_secs: u32) {
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    tokio::spawn(async move {
        let (mut socket, _) = listener.accept().await.unwrap();
        let mut request = [0u8; 8192];
        let _ = socket.read(&mut request).await;
        let head = "HTTP/1.1 200 OK\r\ncontent-type: text/event-stream\r\ntransfer-encoding: chunked\r\n\r\n";
        socket.write_all(head.as_bytes()).await.unwrap();
        for (delay, data) in chunks {
            tokio::time::sleep(std::time::Duration::from_millis(delay)).await;
            let frame = format!("{:x}\r\n{data}\r\n", data.len());
            if socket.write_all(frame.as_bytes()).await.is_err() {
                return;
            }
        }
        if end {
            let _ = socket.write_all(b"0\r\n\r\n").await;
        }
        std::future::pending::<()>().await
    });
    let mut upstream = endpoint(
        "scripted",
        EndpointKind::LlamaServer,
        &format!("http://{address}"),
        "",
    );
    upstream.request_timeout_secs = Some(timeout_secs);
    save(&upstream);
    model("chat", ModelType::Llm, &upstream, "m");
    rate("chat", 1.0);
}

fn sse_event(event: &Value) -> String {
    format!("data: {event}\n\n")
}

// The request's timeout bounded the whole stream, so a long answer was cut off (and, until
// the bill settled on failure, free). It bounds the silence between two chunks.
#[tokio::test]
async fn a_stream_runs_past_its_timeout_while_the_endpoint_keeps_answering() {
    fresh();
    scripted_stream(
        vec![
            (0, sse_event(&chunk("a"))),
            (700, sse_event(&chunk("b"))),
            (700, sse_event(&chunk("c"))),
            (0, "data: [DONE]\n\n".into()),
        ],
        true,
        1,
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    let events = events(&body_text(response).await);
    assert_eq!(events.len(), 4, "{events:?}");
    assert_eq!(events[3], "[DONE]");
    assert_billed_estimate("abc");
}

#[tokio::test]
async fn a_stream_that_falls_silent_ends_with_a_timeout_and_bills_what_it_generated() {
    fresh();
    scripted_stream(vec![(0, sse_event(&chunk("par")))], false, 1).await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    let events = events(&body_text(response).await);
    assert_eq!(events.len(), 2, "{events:?}");
    let last: Value = serde_json::from_str(&events[1]).unwrap();
    assert_eq!(last["error"]["type"], "upstream_timeout");
    assert_billed_estimate("par");
}

// A session that ended mid-stream dropped the forwarding task before it billed.
#[tokio::test]
async fn a_stream_whose_session_ends_bills_what_it_generated() {
    fresh();
    scripted_stream(vec![(0, sse_event(&chunk("par")))], false, 60).await;
    let ctx = user();
    let session = ctx.session.clone();

    let response = chat(
        ctx,
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    let mut body = response.into_body();
    let first = body.frame().await.unwrap().unwrap().into_data().unwrap();
    assert!(String::from_utf8_lossy(&first).contains("par"));
    assert!(test_seam::calls().is_empty(), "billed only at the end");

    session.grant().unwrap().ended.cancel();
    let rest = tokio::time::timeout(std::time::Duration::from_secs(5), body.collect()).await;
    assert!(rest.is_ok(), "the stream ends with its session");
    assert_billed_estimate("par");
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

// An endpoint that answers without end: the forward stops reading at `read_json`'s limit,
// answers a 502 and bills nothing.
#[tokio::test]
async fn an_answer_past_the_body_limit_is_a_502_and_bills_nothing() {
    use crate::ai_service::providers::http::MAX_BODY_BYTES;
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    server
        .mock("POST", "/v1/chat/completions")
        .with_body(format!("[{}0]", "0,".repeat(MAX_BODY_BYTES / 2)))
        .create_async()
        .await;

    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 502);
    assert!(test_seam::calls().is_empty(), "a failure bills nothing");
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

// The rule: forward unless the endpoint is Anthropic (translated by its client) or Ollama
// (native `/api/chat`, for `num_ctx`) or the request has active tools the model does not
// take as definitions (rendered into the prompt by the executor).
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
        Upstream::of(&ollama).is_none(),
        "Ollama's `/v1` ignores num_ctx: its native API answers"
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
fn transcription_model(server: &mockito::ServerGuard) -> String {
    let speaches = endpoint("speaches", EndpointKind::OpenAiAudio, &server.url(), "");
    save(&speaches);
    model(
        "stt",
        ModelType::Transcription,
        &speaches,
        "faster-whisper-small",
    )
}

// /v1/audio/transcriptions forwarded to an endpoint whose capabilities leave out
// transcription.
#[tokio::test]
async fn transcription_refuses_an_endpoint_that_does_not_declare_it() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let upstream = server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    transcription_model(&server);
    // The operator took transcription off the endpoint after the model was on it.
    let mut speech_only = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("speaches"))
        .unwrap()
        .unwrap();
    speech_only.capabilities = vec![AiCapability::Speech];
    Ad4mDb::with_global_instance(|db| db.update_endpoint(&speech_only)).unwrap();

    let response = audio::transcriptions(transcriber(), transcription_form("stt").await)
        .await
        .into_response();

    assert_eq!(response.status(), 400);
    assert_eq!(
        body_json(response).await["error"]["code"],
        "model_not_supported"
    );
    upstream.assert_async().await;
}

// A stream kept the client of its model's old endpoint, URL and key, after the endpoint
// changed.
#[tokio::test]
async fn the_streams_of_a_transcription_model_close_when_it_reloads() {
    let mut server = mockito::Server::new_async().await;
    let upstream = server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    fresh();
    let id = transcription_model(&server);
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    let service = crate::ai_service::AIService::global_instance()
        .await
        .unwrap();
    let operator = RequestContext::operator_for_test();
    let stream = service
        .open_transcription_stream(id.clone(), &operator)
        .await
        .unwrap();

    service.reload_model(id).await.unwrap();
    let fed = service
        .feed_transcription_stream(
            &stream,
            crate::ai_service::transcription::speech_fixture(),
            &operator,
        )
        .await;
    assert!(fed.is_err(), "the stream still took audio");
    upstream.assert_async().await;
}

// A stream kept sending audio with the endpoint's key after its model was removed, by
// itself or with its endpoint.
#[tokio::test]
async fn the_streams_of_a_transcription_model_close_when_it_is_removed() {
    for with_endpoint in [false, true] {
        let mut server = mockito::Server::new_async().await;
        let upstream = server
            .mock("POST", Matcher::Any)
            .expect(0)
            .create_async()
            .await;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        fresh();
        let id = transcription_model(&server);
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let service = crate::ai_service::AIService::global_instance()
            .await
            .unwrap();
        let operator = RequestContext::operator_for_test();
        let stream = service
            .open_transcription_stream(id.clone(), &operator)
            .await
            .unwrap();

        if with_endpoint {
            let endpoint = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("speaches"))
                .unwrap()
                .unwrap();
            service.remove_endpoint(&endpoint.id, true).await.unwrap();
        } else {
            service.remove_model(id).await.unwrap();
        }
        let fed = service
            .feed_transcription_stream(
                &stream,
                crate::ai_service::transcription::speech_fixture(),
                &operator,
            )
            .await;
        assert!(
            fed.is_err(),
            "the stream still took audio (endpoint removed: {with_endpoint})"
        );
        upstream.assert_async().await;
    }
}

// A transcription model whose endpoint can no longer serve it was published as ready. Here
// its reload is the error, and no stream opens on it.
#[tokio::test]
async fn a_transcription_model_its_endpoint_cannot_serve_is_not_ready() {
    let server = mockito::Server::new_async().await;
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    fresh();
    let id = transcription_model(&server);
    let mut changed = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("speaches"))
        .unwrap()
        .unwrap();
    changed.kind = EndpointKind::Ollama;
    Ad4mDb::with_global_instance(|db| db.update_endpoint(&changed)).unwrap();
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    let service = crate::ai_service::AIService::global_instance()
        .await
        .unwrap();

    assert!(service.reload_model(id.clone()).await.is_err());
    assert!(service
        .open_transcription_stream(id, &RequestContext::operator_for_test())
        .await
        .is_err());
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

// ── tool_choice on the text path: a decoding constraint, checked ─────────────────────────

/// A chat model on a mock llama-server endpoint, without native tools (the text path),
/// loaded into a fresh AIService.
async fn text_tools_model(server: &mockito::ServerGuard) {
    fresh();
    let upstream = endpoint("llama", EndpointKind::LlamaServer, &server.url(), "");
    save(&upstream);
    let id = model("chat", ModelType::Llm, &upstream, "qwen3.5-0.8b");
    load(&[id]).await;
}

/// The models `ids` loaded into the AIService.
async fn load(ids: &[String]) {
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    let service = crate::ai_service::AIService::global_instance()
        .await
        .unwrap();
    for id in ids {
        service.reload_model(id.clone()).await.unwrap();
    }
}

fn lookup_tool() -> Value {
    json!([{ "type": "function", "function": {
        "name": "lookup",
        "parameters": {
            "type": "object",
            "properties": { "q": { "type": "string", "minLength": 1 } },
            "required": ["q"],
        },
    } }])
}

/// A completion whose text is `content`.
fn completion(content: &str) -> String {
    json!({ "choices": [{ "message": { "role": "assistant", "content": content } }] }).to_string()
}

/// The bodies of the requests an upstream received, in order.
type Received = std::sync::Arc<std::sync::Mutex<Vec<Value>>>;

/// An upstream that answers its n-th chat request with `replies[n]` (the last one after
/// that), and keeps what it received.
async fn replying(server: &mut mockito::ServerGuard, replies: Vec<String>) -> Received {
    let received: Received = Default::default();
    let keep = received.clone();
    server
        .mock("POST", "/v1/chat/completions")
        .with_body_from_request(move |request| {
            let body = serde_json::from_slice(request.body().unwrap()).unwrap();
            let mut bodies = keep.lock().unwrap();
            bodies.push(body);
            replies[(bodies.len() - 1).min(replies.len() - 1)]
                .clone()
                .into_bytes()
        })
        .create_async()
        .await;
    received
}

fn bodies(received: &Received) -> Vec<Value> {
    received.lock().unwrap().clone()
}

/// What a request with `tool_choice: "required"` and one tool sends as its constraint.
fn lookup_constraint(max_calls: usize) -> Value {
    json!({ "response_format": { "type": "json_schema", "json_schema": { "name": "reply", "schema": {
        "type": "object",
        "properties": { "tool_calls": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "name": { "type": "string", "enum": ["lookup"] },
                    "arguments": {
                        "type": "object",
                        "properties": { "q": { "type": "string" } },
                        "required": ["q"],
                    },
                },
                "required": ["name", "arguments"],
            },
            "minItems": 1,
            "maxItems": max_calls,
        } },
        "required": ["tool_calls"],
    } } } })
}

#[tokio::test]
async fn a_required_tool_choice_sends_the_calls_schema_and_answers_with_the_call() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    let received = replying(
        &mut server,
        vec![completion(
            r#"{"tool_calls":[{"name":"lookup","arguments":{"q":"ad4m"}}]}"#,
        )],
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "tool_choice": "required" }),
    )
    .await;

    assert_eq!(response.status(), 200);
    let body = body_json(response).await;
    let sent = bodies(&received);
    assert_eq!(sent.len(), 1);
    assert_eq!(sent[0]["model"], "qwen3.5-0.8b");
    assert_eq!(
        sent[0]["response_format"],
        lookup_constraint(8)["response_format"]
    );
    // The tools reach the model as text too, in the system prompt; not as definitions.
    assert!(sent[0].get("tools").is_none(), "{}", sent[0]);
    let system = sent[0]["messages"][0]["content"].as_str().unwrap();
    assert!(
        system.contains("<tools>") && system.contains("lookup"),
        "{system}"
    );

    let choice = &body["choices"][0];
    assert_eq!(choice["finish_reason"], "tool_calls");
    assert_eq!(choice["message"]["content"], Value::Null);
    let call = &choice["message"]["tool_calls"][0];
    assert_eq!(call["function"]["name"], "lookup");
    assert_eq!(call["function"]["arguments"], r#"{"q":"ad4m"}"#);
    assert_eq!(body["model"], "chat");
}

#[tokio::test]
async fn a_named_function_without_parallel_calls_allows_one_call() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    let received = replying(
        &mut server,
        vec![completion(
            r#"{"tool_calls":[{"name":"lookup","arguments":{"q":"one"}}]}"#,
        )],
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "tool_choice": { "type": "function", "function": { "name": "lookup" } } }),
    )
    .await;

    assert_eq!(response.status(), 200);
    assert_eq!(
        bodies(&received)[0]["response_format"],
        lookup_constraint(1)["response_format"]
    );
    let body = body_json(response).await;
    assert_eq!(
        body["choices"][0]["message"]["tool_calls"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
}

#[tokio::test]
async fn a_reply_that_breaks_the_constraint_is_asked_for_again() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    // A server that does not constrain: prose first, then a call.
    let received = replying(
        &mut server,
        vec![
            completion("I would look that up."),
            completion(r#"<tool_call>{"name":"lookup","arguments":{"q":"x"}}</tool_call>"#),
        ],
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "tool_choice": "required" }),
    )
    .await;

    assert_eq!(response.status(), 200);
    assert_eq!(bodies(&received).len(), 2);
    let body = body_json(response).await;
    assert_eq!(
        body["choices"][0]["message"]["tool_calls"][0]["function"]["arguments"],
        r#"{"q":"x"}"#
    );
}

#[tokio::test]
async fn a_model_that_never_makes_the_required_call_is_a_clear_error() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    // Arguments that miss the required `q`, every time.
    let received = replying(
        &mut server,
        vec![completion(
            r#"{"tool_calls":[{"name":"lookup","arguments":{}}]}"#,
        )],
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "tool_choice": "required", "stream": true }),
    )
    .await;

    assert_eq!(response.status(), 502);
    assert_eq!(bodies(&received).len(), 2);
    let body = body_json(response).await;
    assert_eq!(body["error"]["code"], "tool_call_not_made");
    let message = body["error"]["message"].as_str().unwrap();
    assert!(message.contains("arguments.q is missing"), "{message}");
}

#[tokio::test]
async fn a_streamed_required_call_comes_as_a_tool_call_delta() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    let received = replying(
        &mut server,
        vec![completion(
            r#"{"tool_calls":[{"name":"lookup","arguments":{"q":"a"}},{"name":"lookup","arguments":{"q":"b"}}]}"#,
        )],
    )
    .await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "tool_choice": "required", "stream": true }),
    )
    .await;

    assert_eq!(response.status(), 200);
    assert_eq!(
        bodies(&received)[0]["response_format"],
        lookup_constraint(8)["response_format"]
    );
    let events = events(&body_text(response).await);
    assert_eq!(events.last().unwrap(), "[DONE]");
    let chunks: Vec<Value> = events[..events.len() - 1]
        .iter()
        .map(|e| serde_json::from_str(e).unwrap())
        .collect();
    assert_eq!(chunks[0]["choices"][0]["delta"]["role"], "assistant");
    let calls: Vec<&Value> = chunks
        .iter()
        .filter_map(|c| c["choices"][0]["delta"]["tool_calls"].get(0))
        .collect();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[1]["index"], 1);
    assert_eq!(calls[1]["function"]["arguments"], r#"{"q":"b"}"#);
    assert_eq!(
        chunks.last().unwrap()["choices"][0]["finish_reason"],
        "tool_calls"
    );
}

#[tokio::test]
async fn auto_sends_no_constraint_and_without_parallel_calls_keeps_the_first() {
    let mut server = mockito::Server::new_async().await;
    text_tools_model(&server).await;
    let received = replying(
        &mut server,
        vec![completion(
            "<tool_call>{\"name\":\"lookup\",\"arguments\":{\"q\":\"a\"}}</tool_call>\n\
             <tool_call>{\"name\":\"lookup\",\"arguments\":{\"q\":\"b\"}}</tool_call>",
        )],
    )
    .await;

    let one = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool(),
                "parallel_tool_calls": false }),
    )
    .await;
    let calls = body_json(one).await["choices"][0]["message"]["tool_calls"].clone();
    assert_eq!(calls.as_array().unwrap().len(), 1);
    assert_eq!(calls[0]["function"]["arguments"], r#"{"q":"a"}"#);

    let both = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": lookup_tool() }),
    )
    .await;
    let calls = body_json(both).await["choices"][0]["message"]["tool_calls"].clone();
    assert_eq!(calls.as_array().unwrap().len(), 2);

    let sent = bodies(&received);
    assert_eq!(sent.len(), 2);
    assert!(
        sent.iter()
            .all(|body| body.get("response_format").is_none()),
        "{sent:?}"
    );
}

/// llama-server enforces neither `tool_choice: "required"` nor a named function (it refuses
/// the named form as the wrong type): a forced call is made under the schema also for a
/// model with `nativeTools`, as JSON or SSE, and billed. Adapted from #1627.
#[tokio::test]
async fn a_forced_call_on_llama_server_is_made_by_a_schema_also_with_native_tools() {
    let mut server = mockito::Server::new_async().await;
    fresh();
    let upstream = endpoint("llama", EndpointKind::LlamaServer, &server.url(), "");
    save(&upstream);
    let native = model_with_tools("local-chat", ModelType::Llm, &upstream, "qwen", Some(true));
    let plain = model("plain-chat", ModelType::Llm, &upstream, "qwen-plain");
    // One table: setting the host rates replaces them all.
    Ad4mDb::with_global_instance(|db| {
        db.set_host_rates(&[("local-chat".into(), 0.5), ("plain-chat".into(), 0.5)])
    })
    .unwrap();
    load(&[native, plain]).await;
    let received = replying(
        &mut server,
        vec![completion(
            r#"{"name": "add_note", "arguments": {"text": "tea"}}"#,
        )],
    )
    .await;
    let tools = json!([
        { "type": "function", "function": { "name": "get_weather", "parameters": { "type": "object", "properties": { "city": { "type": "string" } } } } },
        { "type": "function", "function": { "name": "add_note", "parameters": { "type": "object", "properties": { "text": { "type": "string" } } } } }
    ]);
    let named = json!({ "type": "function", "function": { "name": "add_note" } });

    for model_name in ["local-chat", "plain-chat"] {
        let response = chat(
            user(),
            json!({ "model": model_name, "messages": hello(), "tools": tools, "tool_choice": named }),
        )
        .await;
        assert_eq!(response.status(), 200, "{model_name}");
        let answer = body_json(response).await;
        assert_eq!(answer["model"], model_name);
        let choice = &answer["choices"][0];
        assert_eq!(choice["finish_reason"], "tool_calls");
        let call = &choice["message"]["tool_calls"][0]["function"];
        assert_eq!(call["name"], "add_note");
        assert_eq!(call["arguments"], r#"{"text":"tea"}"#);
    }

    let response = chat(
        user(),
        json!({ "model": "local-chat", "messages": hello(), "tools": tools, "tool_choice": named, "stream": true }),
    )
    .await;
    let sse = body_text(response).await;
    assert!(sse.contains("\"name\":\"add_note\""), "{sse}");
    assert!(sse.contains("\"finish_reason\":\"tool_calls\""), "{sse}");
    assert!(sse.ends_with("data: [DONE]\n\n"), "{sse}");

    let sent = bodies(&received);
    assert_eq!(sent.len(), 3);
    for body in &sent {
        assert!(body.get("tools").is_none(), "{body}");
        assert!(body.get("tool_choice").is_none(), "{body}");
        let schema = &body["response_format"]["json_schema"]["schema"];
        let names = &schema["properties"]["tool_calls"]["items"]["properties"]["name"];
        assert_eq!(names["enum"], json!(["add_note"]), "{body}");
    }
    assert_eq!(test_seam::calls().len(), 3, "{:?}", test_seam::calls());

    let unknown = json!({ "type": "function", "function": { "name": "nope" } });
    let response = chat(
        user(),
        json!({ "model": "local-chat", "messages": hello(), "tools": tools, "tool_choice": unknown }),
    )
    .await;
    assert_eq!(response.status(), 400);
}

/// vLLM holds a model to `tool_choice` by structured outputs, and OpenAI does too: a forced
/// call goes to them as the client wrote it.
#[tokio::test]
async fn a_forced_call_goes_as_written_to_a_server_that_enforces_it() {
    for kind in [EndpointKind::Vllm, EndpointKind::OpenAi] {
        fresh();
        let mut server = mockito::Server::new_async().await;
        let upstream = endpoint("enforcing", kind, &server.url(), "");
        save(&upstream);
        model_with_tools("chat", ModelType::Llm, &upstream, "m", Some(true));
        let tools = json!([{ "type": "function", "function": { "name": "f", "parameters": {} } }]);
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_body(Matcher::PartialJson(
                json!({ "model": "m", "tools": tools, "tool_choice": "required" }),
            ))
            .with_body(json!({ "model": "m", "choices": [] }).to_string())
            .expect(1)
            .create_async()
            .await;

        let response = chat(
            user(),
            json!({ "model": "chat", "messages": hello(), "tools": tools, "tool_choice": "required" }),
        )
        .await;
        assert_eq!(response.status(), 200, "{kind:?}");
        mock.assert_async().await;
    }
}

/// Ollama's OpenAI API ignores `tool_choice`: a forced call goes to its native API under
/// the schema, as `format`.
#[tokio::test]
async fn a_forced_call_on_ollama_is_made_by_a_format_schema() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let ollama = endpoint("ollama", EndpointKind::Ollama, &server.url(), "");
    save(&ollama);
    let id = model("qwen", ModelType::Llm, &ollama, "qwen3:8b");
    load(&[id]).await;
    server
        .mock("POST", "/api/show")
        .with_body(json!({ "model_info": { "qwen3.context_length": 8192 } }).to_string())
        .create_async()
        .await;
    let api_chat = server
        .mock("POST", "/api/chat")
        .match_body(Matcher::PartialJson(json!({
            "model": "qwen3:8b",
            "format": { "required": ["tool_calls"] },
        })))
        .with_body(
            json!({
                "message": { "role": "assistant",
                    "content": "{\"tool_calls\":[{\"name\":\"f\",\"arguments\":{}}]}" },
                "done": true,
            })
            .to_string(),
        )
        .expect(1)
        .create_async()
        .await;
    let forwarded = server
        .mock("POST", "/v1/chat/completions")
        .expect(0)
        .create_async()
        .await;

    let tools = json!([{ "type": "function", "function": { "name": "f", "parameters": {} } }]);
    let response = chat(
        user(),
        json!({ "model": "qwen", "messages": hello(), "tools": tools, "tool_choice": "required" }),
    )
    .await;
    assert_eq!(response.status(), 200);
    let body = body_json(response).await;
    assert_eq!(
        body["choices"][0]["message"]["tool_calls"][0]["function"]["name"],
        "f"
    );
    api_chat.assert_async().await;
    forwarded.assert_async().await;
}

/// llama-server still offers the tools when a request says `tool_choice: "none"`, and
/// their call markup comes back as text: the request goes up without them.
#[tokio::test]
async fn none_reaches_llama_server_without_the_tools() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let upstream = endpoint("llama", EndpointKind::LlamaServer, &server.url(), "");
    save(&upstream);
    model_with_tools("chat", ModelType::Llm, &upstream, "m", Some(true));
    let received = replying(&mut server, vec![completion("plain text")]).await;
    let tools = json!([{ "type": "function", "function": { "name": "f", "parameters": {} } }]);

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "tools": tools, "tool_choice": "none",
                "temperature": 0.2 }),
    )
    .await;
    assert_eq!(response.status(), 200);
    let sent = bodies(&received);
    assert_eq!(sent.len(), 1);
    assert!(sent[0].get("tools").is_none(), "{}", sent[0]);
    assert!(sent[0].get("tool_choice").is_none(), "{}", sent[0]);
    assert_eq!(sent[0]["temperature"], 0.2);
}

// An upstream 429 reached the client without its Retry-After, and a 404 without
// `model_not_found`: unlike the other /v1 routes.
#[tokio::test]
async fn a_transcription_upstream_throttle_or_unknown_model_maps_as_on_other_routes() {
    for (status, code) in [(429, "rate_limit_exceeded"), (404, "model_not_found")] {
        fresh();
        let mut server = mockito::Server::new_async().await;
        let upstream = server
            .mock("POST", "/v1/audio/transcriptions")
            .with_status(status)
            .with_header("retry-after", "7")
            .with_body(r#"{"error":{"message":"no"}}"#)
            .expect(1)
            .create_async()
            .await;
        transcription_model(&server);

        let response = audio::transcriptions(transcriber(), transcription_form("stt").await)
            .await
            .into_response();

        assert_eq!(response.status(), status as u16);
        if status == 429 {
            assert_eq!(response.headers()["retry-after"], "7");
        }
        assert_eq!(body_json(response).await["error"]["code"], code, "{status}");
        upstream.assert_async().await;
        assert!(test_seam::calls().is_empty(), "a failure was billed");
    }
}

/// A model the executor answers itself (not forwarded), loaded into a fresh AIService.
async fn answered_here(kind: EndpointKind, url: &str, key: &str, native_tools: Option<bool>) {
    fresh();
    let upstream = endpoint("here", kind, url, key);
    save(&upstream);
    let id = model_with_tools("chat", ModelType::Llm, &upstream, "m", native_tools);
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    crate::ai_service::AIService::global_instance()
        .await
        .unwrap()
        .reload_model(id)
        .await
        .unwrap();
}

fn a_tool() -> Value {
    json!([{ "type": "function", "function": {
        "name": "get_time", "parameters": { "type": "object", "properties": {} } } }])
}

/// The paths the executor answers itself, each with its endpoint kind, the upstream path
/// it calls, whether the model takes tools as definitions, and a request: Anthropic chat
/// (plain and with tools, which go as native tools), Anthropic `/v1/completions`, and the
/// text-tools path of a llama-server model.
fn paths_answered_here() -> [(
    &'static str,
    EndpointKind,
    &'static str,
    Option<bool>,
    Value,
); 4] {
    [
        (
            "anthropic chat",
            EndpointKind::Anthropic,
            "/v1/messages",
            None,
            json!({ "model": "chat", "messages": hello() }),
        ),
        (
            "anthropic tools",
            EndpointKind::Anthropic,
            "/v1/messages",
            None,
            json!({ "model": "chat", "messages": hello(), "tools": a_tool() }),
        ),
        (
            "anthropic completion",
            EndpointKind::Anthropic,
            "/v1/messages",
            None,
            json!({ "model": "chat", "prompt": "hi" }),
        ),
        (
            "llama-server text tools",
            EndpointKind::LlamaServer,
            "/v1/chat/completions",
            Some(false),
            json!({ "model": "chat", "messages": hello(), "tools": a_tool() }),
        ),
    ]
}

/// `request` through the route its shape names: `/v1/completions` with a `prompt`, else
/// `/v1/chat/completions`.
async fn chat_or_completion(request: Value) -> axum::response::Response {
    if request.get("prompt").is_some() {
        chat::completions(user(), OpenAIJson(request))
            .await
            .into_response()
    } else {
        chat(user(), request).await
    }
}

/// What a client gets from each of [`paths_answered_here`] when the upstream answers
/// `status`.
async fn failures_answered_here(
    status: usize,
    body: &str,
) -> Vec<(String, axum::response::Response)> {
    let mut out = Vec::new();
    for (case, kind, path, native, request) in paths_answered_here() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", path)
            .with_status(status)
            .with_header("retry-after", "7")
            .with_body(body)
            .expect(1)
            .create_async()
            .await;
        answered_here(kind, &server.url(), "sk-secret-key", native).await;
        let response = chat_or_completion(request).await;
        mock.assert_async().await;
        assert!(
            test_seam::calls().is_empty(),
            "{case}: a failure bills nothing"
        );
        out.push((case.to_string(), response));
    }
    out
}

#[tokio::test]
async fn the_executors_own_text_paths_keep_the_upstream_status() {
    for (case, response) in
        failures_answered_here(404, r#"{"error":{"message":"no such model"}}"#).await
    {
        assert_eq!(response.status(), 404, "{case}");
        let error = body_json(response).await;
        assert_eq!(error["error"]["code"], "model_not_found", "{case}");
        assert_eq!(error["error"]["message"], "no such model", "{case}");
    }
    for (case, response) in failures_answered_here(429, "slow down").await {
        assert_eq!(response.status(), 429, "{case}");
        assert_eq!(response.headers()["retry-after"], "7", "{case}");
    }
    for (case, response) in failures_answered_here(401, "bad key sk-secret-key").await {
        assert_eq!(response.status(), 502, "{case}");
        let text = body_text(response).await;
        assert!(text.contains("upstream_auth"), "{case}: {text}");
        assert!(!text.contains("sk-secret-key"), "{case}: {text}");
    }
    for (case, response) in failures_answered_here(500, "boom").await {
        assert_eq!(response.status(), 502, "{case}");
        assert_eq!(
            body_json(response).await["error"]["type"],
            "upstream_error",
            "{case}"
        );
    }
}

#[tokio::test]
async fn an_anthropic_endpoint_that_refuses_connections_is_a_503() {
    answered_here(
        EndpointKind::Anthropic,
        "http://127.0.0.1:1",
        "sk-ant",
        None,
    )
    .await;
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 503);
    assert_eq!(
        body_json(response).await["error"]["type"],
        "upstream_unavailable"
    );
}

// ── /v1/realtime on an endpoint ─────────────────────────────────────────────

type RealtimeClient =
    futures::channel::mpsc::UnboundedSender<Result<axum::extract::ws::Message, axum::Error>>;
type RealtimeFrames = futures::channel::mpsc::UnboundedReceiver<axum::extract::ws::Message>;

/// A `/v1/realtime` session of `caller`: where the client sends, and what it receives.
fn realtime(caller: RequestContext) -> (RealtimeClient, RealtimeFrames) {
    let (client, incoming) = futures::channel::mpsc::unbounded();
    let (socket, frames) = futures::channel::mpsc::unbounded();
    tokio::spawn(super::realtime::serve(caller, socket, incoming));
    (client, frames)
}

fn send(client: &RealtimeClient, event: Value) {
    client
        .unbounded_send(Ok(axum::extract::ws::Message::Text(
            event.to_string().into(),
        )))
        .unwrap();
}

/// The next event the client receives, within 10 s.
async fn next_event(frames: &mut RealtimeFrames) -> Value {
    use futures::StreamExt;
    let frame = tokio::time::timeout(std::time::Duration::from_secs(10), frames.next())
        .await
        .expect("an event within 10 s")
        .expect("the socket is open");
    match frame {
        axum::extract::ws::Message::Text(text) => serde_json::from_str(&text).unwrap(),
        other => panic!("not an event: {other:?}"),
    }
}

/// The test recording (speech, so the gate lets it through) as one append.
fn append_speech() -> Value {
    use base64::Engine;
    let wav = std::fs::read(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/transcription_test.wav"
    ))
    .unwrap();
    json!({
        "type": "input_audio_buffer.append",
        "audio": base64::prelude::BASE64_STANDARD.encode(&wav[44..]),
    })
}

/// The `stt` model, a fresh database, the agent's keys and an AIService.
async fn realtime_world(server: &mockito::ServerGuard) {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    fresh();
    transcription_model(server);
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
}

async fn open_stt(client: &RealtimeClient, frames: &mut RealtimeFrames) -> Value {
    send(
        client,
        json!({"type": "transcription_session.update", "session": {"model": "stt"}}),
    );
    next_event(frames).await
}

// /v1/realtime awaited each commit inside its socket loop, so nothing read the stream's
// 64-piece text channel while a streaming upstream answered: the start of a long utterance
// was dropped without notice, and the whole of it was billed.
#[tokio::test]
async fn realtime_passes_every_piece_a_streaming_upstream_sends() {
    const PIECES: usize = 100;
    let mut server = mockito::Server::new_async().await;
    server
        .mock("POST", "/v1/audio/transcriptions")
        .with_header("content-type", "text/event-stream")
        .with_chunked_body(|w| {
            for i in 0..PIECES {
                let event = json!({"type": "transcript.text.delta", "delta": format!(" w{i}")});
                write!(w, "data: {event}\n\n")?;
                w.flush()?;
                std::thread::sleep(std::time::Duration::from_millis(2));
            }
            w.write_all(b"data: [DONE]\n\n")
        })
        .create_async()
        .await;
    realtime_world(&server).await;
    let (client, mut frames) = realtime(RequestContext::operator_for_test());

    let created = open_stt(&client, &mut frames).await;
    assert_eq!(
        created["type"], "transcription_session.created",
        "{created}"
    );
    send(&client, append_speech());
    send(&client, json!({"type": "input_audio_buffer.commit"}));

    let mut deltas = Vec::new();
    while deltas.len() < PIECES {
        let event = next_event(&mut frames).await;
        assert_eq!(
            event["type"], "conversation.item.input_audio_transcription.delta",
            "{event}"
        );
        deltas.push(event["delta"].as_str().unwrap().to_string());
    }
    let sent: Vec<String> = (0..PIECES).map(|i| format!(" w{i}")).collect();
    assert_eq!(deltas, sent);
}

// /v1/realtime opened and fed streams without the credit check of every other metered path,
// so a user at 0 credits transcribed on the operator's key.
#[tokio::test]
async fn realtime_refuses_a_user_without_credits_at_open_and_at_each_commit() {
    let mut server = mockito::Server::new_async().await;
    let upstream = server
        .mock("POST", Matcher::Any)
        .expect(0)
        .create_async()
        .await;
    realtime_world(&server).await;
    let did = crate::test_utils::create_test_user("realtime-broke@example.org");
    let caller = RequestContext::for_session(
        Session::for_test(Principal::Agent(AgentContext::managed(did)))
            .with_caps(vec![AI_TRANSCRIBE_CAPABILITY.clone()]),
    );
    let paid_hosting = |paid: bool| {
        Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(!paid)).unwrap();
    };
    let (client, mut frames) = realtime(caller);

    // Free while the stream opens; the credits are gone by the commit.
    paid_hosting(false);
    let created = open_stt(&client, &mut frames).await;
    assert_eq!(
        created["type"], "transcription_session.created",
        "{created}"
    );
    paid_hosting(true);
    send(&client, append_speech());
    send(&client, json!({"type": "input_audio_buffer.commit"}));
    let refused = next_event(&mut frames).await;
    assert_eq!(refused["error"]["code"], "insufficient_quota", "{refused}");

    let refused = open_stt(&client, &mut frames).await;
    assert_eq!(refused["error"]["code"], "insufficient_quota", "{refused}");

    paid_hosting(false);
    upstream.assert_async().await;
}

/// An upstream that answers each transcription with `text` after `delay`.
async fn slow_transcription(
    server: &mut mockito::ServerGuard,
    text: &'static str,
    delay: std::time::Duration,
) {
    server
        .mock("POST", "/v1/audio/transcriptions")
        .with_header("content-type", "application/json")
        .with_chunked_body(move |w| {
            std::thread::sleep(delay);
            write!(w, "{}", json!({ "text": text }))
        })
        .create_async()
        .await;
}

// A `session.close` right after the last commit closed the socket before the commit was
// answered: the client lost that utterance's text, and it was still billed.
#[tokio::test]
async fn realtime_sends_the_text_of_the_commits_before_a_close() {
    use futures::StreamExt;
    let mut server = mockito::Server::new_async().await;
    slow_transcription(
        &mut server,
        "the last sentence",
        std::time::Duration::from_millis(500),
    )
    .await;
    realtime_world(&server).await;
    let (client, mut frames) = realtime(RequestContext::operator_for_test());
    let created = open_stt(&client, &mut frames).await;
    assert_eq!(
        created["type"], "transcription_session.created",
        "{created}"
    );

    send(&client, append_speech());
    send(&client, json!({"type": "input_audio_buffer.commit"}));
    send(&client, json!({"type": "session.close"}));

    let mut events = Vec::new();
    while let Some(frame) = tokio::time::timeout(std::time::Duration::from_secs(10), frames.next())
        .await
        .expect("the socket closes within 10 s")
    {
        if let axum::extract::ws::Message::Text(text) = frame {
            events.push(serde_json::from_str::<Value>(&text).unwrap());
        }
    }
    assert_eq!(
        events,
        vec![json!({
            "type": "conversation.item.input_audio_transcription.delta",
            "delta": "the last sentence",
        })]
    );
}

// A close waited 30 s for its commits, while one call to the endpoint may take 120 s (or
// the endpoint's own timeout): a slow last utterance was billed and its text lost.
#[tokio::test]
async fn a_realtime_close_waits_as_long_as_one_call_to_the_endpoint_may_take() {
    let server = mockito::Server::new_async().await;
    fresh();
    let id = transcription_model(&server);
    assert_eq!(
        crate::ai_service::transcription::call_timeout(&id),
        std::time::Duration::from_secs(120)
    );

    let mut slow = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("speaches"))
        .unwrap()
        .unwrap();
    slow.request_timeout_secs = Some(300);
    Ad4mDb::with_global_instance(|db| db.update_endpoint(&slow)).unwrap();
    assert_eq!(
        crate::ai_service::transcription::call_timeout(&id),
        std::time::Duration::from_secs(300)
    );
}

// A commit refused for credits emptied the buffer: a commit after the credits came back
// found nothing to send.
#[tokio::test]
async fn realtime_keeps_the_utterance_a_commit_was_refused_for() {
    let mut server = mockito::Server::new_async().await;
    slow_transcription(&mut server, "kept", std::time::Duration::ZERO).await;
    realtime_world(&server).await;
    let did = crate::test_utils::create_test_user("realtime-later@example.org");
    let caller = RequestContext::for_session(
        Session::for_test(Principal::Agent(AgentContext::managed(did)))
            .with_caps(vec![AI_TRANSCRIBE_CAPABILITY.clone()]),
    );
    let paid_hosting = |paid: bool| {
        Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(!paid)).unwrap();
    };
    let (client, mut frames) = realtime(caller);
    paid_hosting(false);
    let created = open_stt(&client, &mut frames).await;
    assert_eq!(
        created["type"], "transcription_session.created",
        "{created}"
    );
    paid_hosting(true);
    send(&client, append_speech());
    send(&client, json!({"type": "input_audio_buffer.commit"}));
    let refused = next_event(&mut frames).await;
    assert_eq!(refused["error"]["code"], "insufficient_quota", "{refused}");

    paid_hosting(false);
    send(&client, json!({"type": "input_audio_buffer.commit"}));
    let delta = next_event(&mut frames).await;
    assert_eq!(delta["delta"], "kept", "{delta}");
}

// Each commit queued up to 10 MB of audio, without a bound on the commits waiting.
#[tokio::test]
async fn realtime_refuses_a_commit_past_the_pending_bound() {
    use super::realtime::MAX_PENDING_COMMITS;
    let mut server = mockito::Server::new_async().await;
    slow_transcription(&mut server, "one", std::time::Duration::from_millis(300)).await;
    realtime_world(&server).await;
    let (client, mut frames) = realtime(RequestContext::operator_for_test());
    let created = open_stt(&client, &mut frames).await;
    assert_eq!(
        created["type"], "transcription_session.created",
        "{created}"
    );

    for _ in 0..=MAX_PENDING_COMMITS {
        send(&client, append_speech());
        send(&client, json!({"type": "input_audio_buffer.commit"}));
    }
    let mut refused = 0;
    let mut deltas = 0;
    while deltas < MAX_PENDING_COMMITS {
        let event = next_event(&mut frames).await;
        if event["type"] == "error" {
            assert_eq!(event["error"]["code"], "rate_limit_exceeded", "{event}");
            refused += 1;
        } else {
            deltas += 1;
        }
    }
    assert_eq!(refused, 1);
}

/// A caller who pays: hosting is not free, and `USER`'s account holds `credits`.
fn paying_user(credits: f64) {
    Ad4mDb::with_global_instance(|db| {
        db.set_free_hosting_enabled(false)?;
        db.add_user("payer@example.com", USER, "password")?;
        db.set_user_credits("payer@example.com", credits)
    })
    .unwrap();
}

// The only gate was a balance above 0: `n`, the token limits and `service_tier` went as sent,
// so a caller with 0.01 credits could run a request of any size on the operator's key.
#[tokio::test]
async fn a_paying_caller_gets_no_more_than_its_credits_buy() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    rate("chat", 1.0);
    paying_user(100.0);
    let forwarded = std::sync::Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
    let seen = forwarded.clone();
    server
        .mock("POST", "/v1/chat/completions")
        .with_body_from_request(move |request| {
            seen.lock()
                .unwrap()
                .push(serde_json::from_slice(request.body().unwrap()).unwrap());
            json!({ "model": "Qwen/Qwen3-8B", "choices": [] })
                .to_string()
                .into()
        })
        .expect(2)
        .create_async()
        .await;
    let left = (100 - estimate_token_count(&hello().to_string())) as u64;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "n": 2, "max_tokens": 100000,
                "max_completion_tokens": 3, "service_tier": "priority" }),
    )
    .await;
    assert_eq!(response.status(), 200);
    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 200);

    let forwarded = forwarded.lock().unwrap().clone();
    assert_eq!(forwarded[0]["n"], 2);
    assert_eq!(forwarded[0]["max_tokens"], left / 2, "{}", forwarded[0]);
    assert_eq!(
        forwarded[0]["max_completion_tokens"], 3,
        "a lower limit stays"
    );
    assert!(
        forwarded[0].get("service_tier").is_none(),
        "{}",
        forwarded[0]
    );
    assert_eq!(
        forwarded[1]["max_tokens"], left,
        "a request without a limit gets one: {}",
        forwarded[1]
    );
}

#[tokio::test]
async fn a_caller_whose_credits_do_not_cover_the_prompt_is_refused_before_the_endpoint() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    rate("chat", 1.0);
    paying_user(1.0);
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .expect(0)
        .create_async()
        .await;

    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 429);
    assert_eq!(
        body_json(response).await["error"]["code"],
        "insufficient_quota"
    );
    mock.assert_async().await;
}

// A caller who pays and set no limit got every token its credits buy as `max_tokens`. Past
// the model's output maximum, OpenAI and vLLM answer such a request 400, so a caller with
// many credits could not use the model at all.
#[tokio::test]
async fn a_paying_caller_who_sets_no_limit_gets_none_the_model_refuses() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    openai_model(&server, "").await;
    rate("chat", 1.0);
    paying_user(100_000.0);
    // The model's output maximum, as the endpoint checks it.
    let output_maximum = 4096;
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .with_status_code_from_request(move |request| {
            let body: Value = serde_json::from_slice(request.body().unwrap()).unwrap();
            let refused = ["max_tokens", "max_completion_tokens"].iter().any(|key| {
                body[key]
                    .as_u64()
                    .is_some_and(|limit| limit > output_maximum)
            });
            if refused {
                400
            } else {
                200
            }
        })
        .with_body(json!({ "model": "Qwen/Qwen3-8B", "choices": [] }).to_string())
        .expect(1)
        .create_async()
        .await;

    let response = chat(user(), json!({ "model": "chat", "messages": hello() })).await;
    assert_eq!(response.status(), 200, "{}", body_text(response).await);
    mock.assert_async().await;
}

// The paths the executor answers itself checked only a balance above 0, so a prompt the
// credits did not cover still went to the endpoint on the operator's key.
#[tokio::test]
async fn a_prompt_the_credits_do_not_cover_is_refused_on_the_paths_the_executor_answers() {
    for (case, kind, path, native, request) in paths_answered_here() {
        let mut server = mockito::Server::new_async().await;
        let mock = server.mock("POST", path).expect(0).create_async().await;
        answered_here(kind, &server.url(), "sk-key", native).await;
        rate("chat", 1.0);
        paying_user(1.0);

        let response = chat_or_completion(request).await;
        assert_eq!(response.status(), 429, "{case}");
        assert_eq!(
            body_json(response).await["error"]["code"],
            "insufficient_quota",
            "{case}"
        );
        mock.assert_async().await;
    }
}

/// A speech model `voice` on an audio endpoint at `server`.
fn speech_model(server: &mockito::ServerGuard) {
    let mut speaches = endpoint("speaches", EndpointKind::OpenAiAudio, &server.url(), "");
    speaches.capabilities = vec![AiCapability::Speech];
    save(&speaches);
    model("voice", ModelType::Speech, &speaches, "kokoro");
}

// Speech checked only a balance above 0 before it sent, so the operator paid the endpoint
// for audio whose bill then failed, and the audio was thrown away.
#[tokio::test]
async fn speech_the_credits_do_not_cover_is_refused_before_the_endpoint() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/audio/speech")
        .expect(0)
        .create_async()
        .await;
    speech_model(&server);
    // The least speech costs is 1 credit.
    paying_user(0.5);

    let response = audio::speech(
        user(),
        OpenAIJson(json!({ "model": "voice", "input": "hello" })),
    )
    .await
    .into_response();
    assert_eq!(response.status(), 429);
    assert_eq!(
        body_json(response).await["error"]["code"],
        "insufficient_quota"
    );
    mock.assert_async().await;
}

// The speech audio was read whole with no bound, so an endpoint that sent without end
// filled the executor's memory.
#[tokio::test]
async fn speech_audio_past_the_bound_is_refused_and_not_billed() {
    fresh();
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/audio/speech")
        .with_header("content-type", "audio/mpeg")
        .with_body(vec![0u8; super::passthrough::MAX_SPEECH_AUDIO + 1])
        .expect(1)
        .create_async()
        .await;
    speech_model(&server);

    let response = audio::speech(
        user(),
        OpenAIJson(json!({ "model": "voice", "input": "hello" })),
    )
    .await
    .into_response();
    mock.assert_async().await;
    assert_eq!(response.status(), 502);
    assert_eq!(body_json(response).await["error"]["type"], "upstream_error");
    assert!(test_seam::calls().is_empty(), "{:?}", test_seam::calls());
}

// When the client had not read the stream for a while, its channel was full and the final
// error event was dropped: the client saw a stream cut off with no error.
#[tokio::test]
async fn a_slow_client_still_gets_the_final_error_event() {
    fresh();
    // As many events as the channel to the client holds, then silence past the timeout.
    let chunks = (0..32)
        .map(|n| (10, sse_event(&chunk(&n.to_string()))))
        .collect();
    scripted_stream(chunks, false, 2).await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    // The upstream sends for about 0.3 s and is silent for 2 s: the error comes at about
    // 2.3 s, while the client has read nothing.
    tokio::time::sleep(std::time::Duration::from_millis(3300)).await;
    let events = events(&body_text(response).await);
    assert_eq!(events.len(), 33, "{events:?}");
    let last: Value = serde_json::from_str(&events[32]).unwrap();
    assert_eq!(last["error"]["type"], "upstream_timeout");
}

// `/v1/models` passed the health check's detail, which names the endpoint's URL, to users.
#[tokio::test]
async fn the_model_list_names_no_endpoint_url() {
    fresh();
    let mut down = endpoint("down", EndpointKind::Vllm, "http://10.9.8.7:8000", "");
    down.health = EndpointHealth {
        state: EndpointState::Unreachable,
        checked_at: Some(1),
        detail: Some("Could not reach http://10.9.8.7:8000/v1/models: refused".into()),
        served_models: vec![],
    };
    save(&down);
    model("chat", ModelType::Llm, &down, "m");

    let list = models::list_models(user()).await.unwrap().0;
    let list = serde_json::to_value(list).unwrap();
    assert_eq!(list["data"][0]["ad4m"]["status"]["state"], "ENDPOINT_DOWN");
    assert!(!list.to_string().contains("10.9.8.7"), "{list}");
}

// An embedding answer the executor could not read gave a 500 whose message named the URL.
#[tokio::test]
async fn an_embedding_answer_the_executor_cannot_read_names_no_url() {
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/embeddings")
        .with_body("{}")
        .create_async()
        .await;
    let (_id, _service) = embedding_model(&server).await;

    let request = serde_json::from_value(json!({ "model": "nomic", "input": ["a"] })).unwrap();
    let error = embeddings::embeddings(user(), OpenAIJson(request))
        .await
        .err()
        .expect("an unreadable answer fails");
    mock.assert_async().await;
    let shown = serde_json::to_string(&error.error).unwrap();
    let address = server.host_with_port();
    assert!(!shown.contains(&address), "{shown}");
    assert_eq!(error.error.code.as_deref(), Some("upstream_error"));
}

// A client that left while the upstream was silent kept the upstream connection and the
// endpoint's slot until the timeout, and was billed only then.
#[tokio::test]
async fn a_client_that_leaves_a_silent_stream_ends_it_and_is_billed() {
    fresh();
    scripted_stream(vec![(0, sse_event(&chunk("par")))], false, 60).await;

    let response = chat(
        user(),
        json!({ "model": "chat", "messages": hello(), "stream": true }),
    )
    .await;
    let mut body = response.into_body();
    body.frame().await.unwrap().unwrap();
    drop(body);
    for _ in 0..40 {
        if !test_seam::calls().is_empty() {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
    }
    assert_billed_estimate("par");
}

// Speech was billed before it was sent, so a request that failed, or timed out waiting for
// a slot, was charged.
#[tokio::test]
async fn speech_the_endpoint_does_not_answer_is_not_billed() {
    fresh();
    let mut speaches = endpoint(
        "speaches",
        EndpointKind::OpenAiAudio,
        "http://127.0.0.1:1",
        "",
    );
    speaches.capabilities = vec![AiCapability::Speech];
    save(&speaches);
    model("voice", ModelType::Speech, &speaches, "kokoro");

    let response = audio::speech(
        user(),
        OpenAIJson(json!({ "model": "voice", "input": "hello" })),
    )
    .await
    .into_response();
    assert_eq!(response.status(), 503);
    assert!(test_seam::calls().is_empty(), "{:?}", test_seam::calls());
}

// Remote chat the executor answers (Anthropic, Ollama, tools as text) skipped the
// endpoint's `max_concurrent`: two models on one endpoint called it at once.
#[tokio::test]
async fn chat_the_executor_answers_keeps_the_endpoints_concurrency_cap() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    fresh();
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    let (active, most) = (Arc::new(AtomicUsize::new(0)), Arc::new(AtomicUsize::new(0)));
    let (active_seen, most_seen) = (active.clone(), most.clone());
    tokio::spawn(async move {
        loop {
            let (mut socket, _) = listener.accept().await.unwrap();
            let (active, most) = (active_seen.clone(), most_seen.clone());
            tokio::spawn(async move {
                let mut request = [0u8; 65536];
                let _ = socket.read(&mut request).await;
                let now = active.fetch_add(1, Ordering::SeqCst) + 1;
                most.fetch_max(now, Ordering::SeqCst);
                tokio::time::sleep(std::time::Duration::from_millis(300)).await;
                let body = json!({ "content": [{ "type": "text", "text": "hi" }],
                                   "stop_reason": "end_turn" })
                .to_string();
                active.fetch_sub(1, Ordering::SeqCst);
                let answer = format!(
                    "HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: {}\r\nconnection: close\r\n\r\n{body}",
                    body.len()
                );
                let _ = socket.write_all(answer.as_bytes()).await;
            });
        }
    });
    let mut capped = endpoint(
        "capped",
        EndpointKind::Anthropic,
        &format!("http://{address}"),
        "sk-ant",
    );
    capped.max_concurrent = Some(1);
    save(&capped);
    let ids = [
        model("a", ModelType::Llm, &capped, "m-a"),
        model("b", ModelType::Llm, &capped, "m-b"),
    ];
    crate::ai_service::AIService::init_global_instance()
        .await
        .unwrap();
    let service = crate::ai_service::AIService::global_instance()
        .await
        .unwrap();
    for id in ids {
        service.reload_model(id).await.unwrap();
    }

    let (a, b) = tokio::join!(
        chat(user(), json!({ "model": "a", "messages": hello() })),
        chat(user(), json!({ "model": "b", "messages": hello() })),
    );
    assert_eq!((a.status().as_u16(), b.status().as_u16()), (200, 200));
    assert_eq!(most.load(Ordering::SeqCst), 1, "one call at a time");
}

// An embedding model whose endpoint became one that serves no embeddings kept its old
// client, so `ai.embed` still sent text to the old URL with the old key.
#[tokio::test]
async fn an_embedder_that_cannot_be_rebuilt_stops_embedding() {
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/embeddings")
        .expect(0)
        .create_async()
        .await;
    let (id, service) = embedding_model(&server).await;
    let mut changed = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("embedder"))
        .unwrap()
        .unwrap();
    changed.kind = EndpointKind::Anthropic;
    Ad4mDb::with_global_instance(|db| db.update_endpoint(&changed)).unwrap();

    assert!(service.reload_model(id.clone()).await.is_err());
    assert!(service
        .embed_batch(&id, vec!["a".into()], None)
        .await
        .is_err());
    mock.assert_async().await;
}

// A model changed from embedding to speech kept its embedder.
#[tokio::test]
async fn a_model_that_stops_being_an_embedding_model_stops_embedding() {
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/embeddings")
        .expect(0)
        .create_async()
        .await;
    let (id, service) = embedding_model(&server).await;
    let speech = Ad4mDb::with_global_instance(|db| db.get_model(id.clone()))
        .unwrap()
        .unwrap();
    let input = ModelInput {
        name: speech.name.clone(),
        model_type: ModelType::Speech,
        endpoint_id: speech.endpoint_id.clone(),
        remote_model: speech.remote_model.clone(),
        native_tools: None,
    };
    Ad4mDb::with_global_instance(|db| db.update_model(&id, &input)).unwrap();

    service.reload_model(id.clone()).await.unwrap();
    assert!(service
        .embed_batch(&id, vec!["a".into()], None)
        .await
        .is_err());
    mock.assert_async().await;
}

// Semantic dedup sent a whole class in one request, past what an endpoint takes.
#[tokio::test]
async fn semantic_dedup_embeds_in_bounded_batches_in_order() {
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/embeddings")
        .with_body_from_request(|request| {
            let body: Value = serde_json::from_slice(request.body().unwrap()).unwrap();
            let inputs = body["input"].as_array().unwrap().clone();
            assert!(inputs.len() <= crate::perspectives::interpretation::EMBED_BATCH);
            let data: Vec<Value> = inputs
                .iter()
                .enumerate()
                .map(|(index, text)| {
                    let n: f32 = text.as_str().unwrap().parse().unwrap();
                    json!({ "index": index, "embedding": [n, 1.0] })
                })
                .collect();
            json!({ "data": data }).to_string().into()
        })
        .expect(2)
        .create_async()
        .await;
    let (_id, _service) = embedding_model(&server).await;

    let texts: Vec<String> = (0..70).map(|n| n.to_string()).collect();
    let vectors = crate::perspectives::interpretation::embed_via_ai_service("nomic", &texts)
        .await
        .unwrap();
    mock.assert_async().await;
    assert_eq!(vectors.len(), 70);
    assert!(vectors
        .iter()
        .enumerate()
        .all(|(n, vector)| vector[0] == n as f32));
}
