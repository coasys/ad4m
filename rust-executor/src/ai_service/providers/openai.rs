//! OpenAI-shaped chat provider — `POST {base}/v1/chat/completions` — and model listing.
//!
//! Serves OpenAI itself and everything that copies its wire format: Groq, OpenRouter,
//! Google's compat endpoint, Ollama's `/v1`, vLLM, llama-server, speaches. Hand-rolled on
//! `reqwest`, as `anthropic.rs` is: the surface is one endpoint.
//!
//! Tools go out as `tools[]` and come back as `tool_calls`, also when streamed: the
//! deltas of one call arrive under one `index` and are joined here. `tool_choice` and
//! `response_format` pass through as the caller wrote them. Whether a model is *handed*
//! tools is the model's `native_tools` flag (`ModelApi::uses_native_tools`), because
//! OpenAI-shaped servers differ per model and per server flag in whether they honour them.
//!
//! A streamed request asks for `stream_options.include_usage`, so the final chunk carries
//! the provider's token counts.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::time::Duration;
use tokio::sync::mpsc;
use url::Url;

use super::discovery::{guess_capabilities, Discover};
use super::{ChatReply, ChatRequest, ChatRole, ChatTurn, ChatUsage, RemoteChat, ToolCall};
use crate::types::{DiscoveredModel, EndpointKind};

/// A completion can run for minutes on a long prompt or a slow local model.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(600);

pub struct OpenAiChat {
    http: reqwest::Client,
    api_key: String,
    /// Fully resolved, e.g. `https://api.groq.com/openai/v1/chat/completions`.
    endpoint: String,
}

impl OpenAiChat {
    /// Build a client for `base_url`, given with or without its trailing `/v1`: every
    /// provider documents the `/v1` form, and a path prefix (Groq's `/openai/v1`, this
    /// executor's `/api/v1/openai/v1`) survives.
    pub fn new(api_key: &str, base_url: Url) -> Self {
        Self {
            // `expect`: the default client follows redirects, so a fallback to it would
            // drop the policy that keeps the key on the host it was set for.
            http: super::http::credentialed_http(REQUEST_TIMEOUT)
                .build()
                .expect("the HTTP client builds"),
            api_key: api_key.to_string(),
            endpoint: super::http::versioned_endpoint(base_url, "chat/completions"),
        }
    }

    async fn send(&self, body: &Value) -> Result<reqwest::Response> {
        let mut request = self.http.post(&self.endpoint).json(body);
        // A local server takes no key, and some refuse an empty `Bearer `.
        if !self.api_key.is_empty() {
            request = request.bearer_auth(&self.api_key);
        }
        let response = request.send().await.map_err(|e| {
            anyhow!(
                "Error connecting to remote LLM API: {}",
                super::http::scrub(&e.to_string(), &self.api_key)
            )
        })?;
        if !response.status().is_success() {
            return Err(
                super::http::UpstreamStatus::read(response, &self.endpoint, &self.api_key)
                    .await
                    .into(),
            );
        }
        Ok(response)
    }
}

/// The request body. Shared by both entry points so they cannot drift in what they send.
fn build_body(request: ChatRequest, stream: bool) -> Value {
    let mut body = json!({
        "model": request.model,
        "messages": request.messages.iter().map(wire_message).collect::<Vec<_>>(),
    });
    if !request.tools.is_empty() {
        body["tools"] = request
            .tools
            .iter()
            .map(|tool| {
                json!({
                    "type": "function",
                    "function": {
                        "name": tool.name,
                        "description": tool.description,
                        "parameters": tool.parameters,
                    },
                })
            })
            .collect();
    }
    if let Some(tool_choice) = request.tool_choice {
        body["tool_choice"] = tool_choice;
    }
    if let Some(response_format) = request.response_format {
        body["response_format"] = response_format;
    }
    if stream {
        body["stream"] = json!(true);
        body["stream_options"] = json!({ "include_usage": true });
    }
    body
}

fn wire_message(turn: &ChatTurn) -> Value {
    if let Some(call_id) = &turn.tool_result_for {
        return json!({ "role": "tool", "tool_call_id": call_id, "content": turn.content });
    }
    let role = match turn.role {
        ChatRole::System => "system",
        ChatRole::User => "user",
        ChatRole::Assistant => "assistant",
    };
    let mut message = json!({ "role": role, "content": turn.content });
    if !turn.tool_calls.is_empty() {
        message["tool_calls"] = turn
            .tool_calls
            .iter()
            .map(|call| {
                json!({
                    "id": call.id,
                    "type": "function",
                    "function": { "name": call.name, "arguments": call.arguments.to_string() },
                })
            })
            .collect();
        if turn.content.is_empty() {
            message["content"] = Value::Null;
        }
    }
    message
}

/// A call's `arguments` string as an object. One that does not parse stays a string, which
/// the compat layer passes on under `_raw` rather than guessing.
fn parse_arguments(raw: &str) -> Value {
    if raw.trim().is_empty() {
        return json!({});
    }
    serde_json::from_str(raw).unwrap_or_else(|_| Value::String(raw.to_string()))
}

/// A call id, minted when the server sent none (older llama-server builds do not).
fn call_id(id: Option<&str>) -> String {
    match id {
        Some(id) if !id.is_empty() => id.to_string(),
        _ => format!("call_{}", uuid::Uuid::new_v4().simple()),
    }
}

/// The provider's token counts. `prompt_tokens` already includes cached tokens.
fn usage_of(usage: &Value) -> ChatUsage {
    ChatUsage {
        input_tokens: usage.get("prompt_tokens").and_then(Value::as_u64),
        output_tokens: usage.get("completion_tokens").and_then(Value::as_u64),
        ..Default::default()
    }
}

fn declined(refusal: &str) -> anyhow::Error {
    anyhow!("The model declined this request: {refusal}. No content was returned.")
}

/// A non-streamed answer as a reply. A refusal is an error: an empty success would bill
/// the caller for an answer they did not get.
fn read_reply(json: &Value) -> Result<ChatReply> {
    let message = json
        .pointer("/choices/0/message")
        .ok_or_else(|| anyhow!("Got response with no choice"))?;
    if let Some(refusal) = message
        .get("refusal")
        .and_then(Value::as_str)
        .filter(|r| !r.is_empty())
    {
        return Err(declined(refusal));
    }
    let tool_calls = message
        .get("tool_calls")
        .and_then(Value::as_array)
        .map(|calls| {
            calls
                .iter()
                .map(|call| ToolCall {
                    id: call_id(call.get("id").and_then(Value::as_str)),
                    name: call
                        .pointer("/function/name")
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_string(),
                    arguments: parse_arguments(
                        call.pointer("/function/arguments")
                            .and_then(Value::as_str)
                            .unwrap_or_default(),
                    ),
                })
                .collect()
        })
        .unwrap_or_default();
    Ok(ChatReply {
        text: message
            .get("content")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_string(),
        tool_calls,
        usage: json.get("usage").map(usage_of).unwrap_or_default(),
    })
}

/// A tool call being assembled from stream deltas.
#[derive(Default)]
struct PartialCall {
    id: Option<String>,
    name: String,
    arguments: String,
}

/// What a stream has said so far.
#[derive(Default)]
struct StreamState {
    text: String,
    calls: BTreeMap<u64, PartialCall>,
    usage: ChatUsage,
}

impl StreamState {
    /// Take one chunk. Answers its text, if any; an error chunk or a refusal fails.
    fn apply(&mut self, chunk: &Value) -> Result<Option<String>> {
        if let Some(error) = chunk.get("error") {
            let message = error
                .get("message")
                .and_then(Value::as_str)
                .map(str::to_string)
                .unwrap_or_else(|| error.to_string());
            return Err(anyhow!("The stream failed: {message}"));
        }
        if let Some(usage) = chunk.get("usage").filter(|u| !u.is_null()) {
            self.usage = usage_of(usage);
        }
        let Some(delta) = chunk.pointer("/choices/0/delta") else {
            return Ok(None);
        };
        if let Some(refusal) = delta
            .get("refusal")
            .and_then(Value::as_str)
            .filter(|r| !r.is_empty())
        {
            return Err(declined(refusal));
        }
        for (position, call) in delta
            .get("tool_calls")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .enumerate()
        {
            let index = call
                .get("index")
                .and_then(Value::as_u64)
                .unwrap_or(position as u64);
            let partial = self.calls.entry(index).or_default();
            if let Some(id) = call.get("id").and_then(Value::as_str) {
                partial.id = Some(id.to_string());
            }
            if let Some(name) = call.pointer("/function/name").and_then(Value::as_str) {
                partial.name.push_str(name);
            }
            if let Some(arguments) = call.pointer("/function/arguments").and_then(Value::as_str) {
                partial.arguments.push_str(arguments);
            }
        }
        let text = delta
            .get("content")
            .and_then(Value::as_str)
            .filter(|t| !t.is_empty())
            .map(str::to_string);
        if let Some(text) = &text {
            self.text.push_str(text);
        }
        Ok(text)
    }

    fn into_reply(self) -> ChatReply {
        ChatReply {
            text: self.text,
            tool_calls: self
                .calls
                .into_values()
                .map(|call| ToolCall {
                    id: call_id(call.id.as_deref()),
                    name: call.name,
                    arguments: parse_arguments(&call.arguments),
                })
                .collect(),
            usage: self.usage,
        }
    }
}

#[async_trait]
impl RemoteChat for OpenAiChat {
    fn supports_native_tools(&self) -> bool {
        true
    }

    async fn chat(&self, request: ChatRequest) -> Result<ChatReply> {
        let response = self.send(&build_body(request, false)).await?;
        let json: Value = response
            .json()
            .await
            .map_err(|e| anyhow!("Could not read the completion: {e}"))?;
        read_reply(&json)
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        use futures::StreamExt;

        let response = self.send(&build_body(request, true)).await?;
        let mut stream = response.bytes_stream();
        // Bytes, decoded a complete line at a time: a chunk boundary can split a
        // multi-byte character.
        let mut buffer: Vec<u8> = Vec::new();
        let mut state = StreamState::default();

        'read: while let Some(chunk) = stream.next().await {
            let chunk = chunk.map_err(|e| anyhow!("The stream failed: {e}"))?;
            buffer.extend_from_slice(&chunk);
            while let Some(newline) = buffer.iter().position(|b| *b == b'\n') {
                let line: Vec<u8> = buffer.drain(..=newline).collect();
                let line = String::from_utf8_lossy(&line);
                let Some(payload) = line.trim_end().strip_prefix("data:").map(str::trim) else {
                    continue;
                };
                if payload == "[DONE]" {
                    break 'read;
                }
                let Ok(event) = serde_json::from_str::<Value>(payload) else {
                    continue;
                };
                if let Some(text) = state.apply(&event)? {
                    if tokens.send(text).is_err() {
                        // The consumer hung up: nobody reads the rest.
                        break 'read;
                    }
                }
            }
        }
        Ok(state.into_reply())
    }
}

/// Model listing for every OpenAI-shaped kind — `GET {base}/v1/models`.
pub struct OpenAiDiscover {
    kind: EndpointKind,
    http: reqwest::Client,
    api_key: String,
    base_url: Url,
}

impl OpenAiDiscover {
    pub fn new(kind: EndpointKind, api_key: &str, base_url: Url, timeout: Duration) -> Self {
        Self {
            kind,
            http: super::http::credentialed_http(timeout)
                .build()
                .expect("the HTTP client builds"),
            api_key: api_key.to_string(),
            base_url,
        }
    }
}

/// The context window an entry of `/v1/models` names: vLLM's `max_model_len`, or
/// llama-server's `meta.n_ctx_train`.
fn context_length(entry: &Value) -> Option<u32> {
    entry
        .get("max_model_len")
        .or_else(|| entry.pointer("/meta/n_ctx_train"))
        .and_then(Value::as_u64)
        .map(|n| n.min(u64::from(u32::MAX)) as u32)
}

#[async_trait]
impl Discover for OpenAiDiscover {
    async fn list_models(&self) -> Result<Vec<DiscoveredModel>> {
        let url = super::http::versioned_endpoint(self.base_url.clone(), "models");
        let mut request = self.http.get(&url);
        if !self.api_key.is_empty() {
            request = request.bearer_auth(&self.api_key);
        }
        let json = super::http::fetch_json(request, &url, &self.api_key).await?;
        Ok(json
            .get("data")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(|entry| {
                // An entry without an id is skipped, not the whole listing.
                let id = entry.get("id")?.as_str()?.to_string();
                Some(DiscoveredModel {
                    capabilities: guess_capabilities(self.kind, &id),
                    context_length: context_length(entry),
                    id,
                })
            })
            .collect())
    }
}

#[cfg(test)]
mod wire_tests {
    use super::super::ToolSpec;
    use super::*;
    use crate::types::AiCapability;
    use mockito::Matcher;

    fn url(s: &str) -> Url {
        Url::parse(s).expect("test URL parses")
    }

    fn discover(server: &mockito::ServerGuard, key: &str) -> OpenAiDiscover {
        OpenAiDiscover::new(
            EndpointKind::OpenAiCompatible,
            key,
            url(&server.url()),
            Duration::from_secs(5),
        )
    }

    fn search_tool() -> ToolSpec {
        ToolSpec {
            name: "search".into(),
            description: "find things".into(),
            parameters: json!({ "type": "object", "properties": { "q": { "type": "string" } } }),
        }
    }

    #[test]
    fn chat_and_listing_resolve_the_same_base_url() {
        // Groq's documented URL: a `/v1` under a path prefix must not be doubled.
        let client = OpenAiChat::new("", url("https://api.groq.com/openai/v1"));
        assert_eq!(
            client.endpoint,
            "https://api.groq.com/openai/v1/chat/completions"
        );
        let bare = OpenAiChat::new("", url("http://localhost:11434"));
        assert_eq!(bare.endpoint, "http://localhost:11434/v1/chat/completions");
    }

    #[test]
    fn a_tool_result_is_a_tool_message_and_a_call_carries_its_arguments_as_a_string() {
        let call = ToolCall {
            id: "call_1".into(),
            name: "search".into(),
            arguments: json!({ "q": "ad4m" }),
        };
        let body = build_body(
            ChatRequest::new(
                "m",
                vec![
                    ChatTurn::system("s"),
                    ChatTurn::assistant_calling("", vec![call]),
                    ChatTurn::tool_result("call_1", "found"),
                ],
            ),
            false,
        );
        let messages = &body["messages"];
        assert_eq!(messages[0], json!({ "role": "system", "content": "s" }));
        assert_eq!(messages[1]["content"], Value::Null);
        assert_eq!(
            messages[1]["tool_calls"][0],
            json!({ "id": "call_1", "type": "function",
                    "function": { "name": "search", "arguments": "{\"q\":\"ad4m\"}" } })
        );
        assert_eq!(
            messages[2],
            json!({ "role": "tool", "tool_call_id": "call_1", "content": "found" })
        );
        assert!(body.get("stream").is_none());
        assert!(body.get("tools").is_none(), "no tools: no key");
    }

    #[tokio::test]
    async fn tools_tool_choice_and_response_format_go_out_and_a_call_comes_back() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_header("authorization", "Bearer sk-test")
            .match_body(Matcher::PartialJson(json!({
                "model": "qwen3",
                "tools": [{ "type": "function", "function": { "name": "search" } }],
                "tool_choice": "required",
                "response_format": { "type": "json_object" },
            })))
            .with_status(200)
            .with_body(
                json!({
                    "choices": [{ "message": { "role": "assistant", "content": null,
                        "tool_calls": [{ "id": "call_9", "type": "function",
                            "function": { "name": "search", "arguments": "{\"q\":\"x\"}" } }] } }],
                    "usage": { "prompt_tokens": 12, "completion_tokens": 5, "total_tokens": 17 },
                })
                .to_string(),
            )
            .create_async()
            .await;

        let mut request =
            ChatRequest::new("qwen3", vec![ChatTurn::user("hi")]).with_tools(vec![search_tool()]);
        request.tool_choice = Some(json!("required"));
        request.response_format = Some(json!({ "type": "json_object" }));
        let reply = OpenAiChat::new("sk-test", url(&server.url()))
            .chat(request)
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
        assert_eq!(reply.text, "");
        assert_eq!(
            reply.tool_calls,
            vec![ToolCall {
                id: "call_9".into(),
                name: "search".into(),
                arguments: json!({ "q": "x" }),
            }]
        );
        assert_eq!(reply.usage.input_tokens, Some(12));
        assert_eq!(reply.usage.output_tokens, Some(5));
    }

    #[tokio::test]
    async fn no_key_means_no_authorization_header() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_header("authorization", Matcher::Missing)
            .with_status(200)
            .with_body(r#"{"choices":[{"message":{"content":"hello"}}]}"#)
            .create_async()
            .await;
        let reply = OpenAiChat::new("", url(&server.url()))
            .chat(ChatRequest::new("m", vec![ChatTurn::user("hi")]))
            .await
            .unwrap();
        mock.assert_async().await;
        assert_eq!(reply.text, "hello");
        assert_eq!(
            reply.usage,
            ChatUsage::default(),
            "no usage block: no counts"
        );
    }

    #[tokio::test]
    async fn a_refusal_is_an_error_not_an_empty_answer() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/chat/completions")
            .with_status(200)
            .with_body(
                r#"{"choices":[{"message":{"content":null,"refusal":"I can't help with that"}}]}"#,
            )
            .create_async()
            .await;
        let error = OpenAiChat::new("", url(&server.url()))
            .chat(ChatRequest::new("m", vec![ChatTurn::user("hi")]))
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("I can't help with that"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn an_error_status_carries_the_providers_reason_without_the_key() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/chat/completions")
            .with_status(400)
            .with_body(r#"{"error":{"message":"bad request for key sk-test"}}"#)
            .create_async()
            .await;
        let error = OpenAiChat::new("sk-test", url("http://127.0.0.1:1"))
            .chat(ChatRequest::new("m", vec![]))
            .await
            .unwrap_err();
        assert!(error.to_string().contains("Error connecting"), "{error}");

        let error = OpenAiChat::new("sk-test", url(&server.url()))
            .chat(ChatRequest::new("m", vec![]))
            .await
            .unwrap_err();
        assert!(error.to_string().contains("400"), "{error}");
        assert!(
            error.to_string().contains("bad request for key <redacted>"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn a_stream_yields_each_delta_assembles_tool_calls_and_reads_usage() {
        let chunk = |v: Value| format!("data: {v}\n\n");
        let sse = [
            chunk(json!({ "choices": [{ "delta": { "role": "assistant", "content": "Hel" } }] })),
            chunk(json!({ "choices": [{ "delta": { "content": "lo" } }] })),
            chunk(json!({ "choices": [{ "delta": { "tool_calls": [
                { "index": 0, "id": "call_a", "type": "function",
                  "function": { "name": "search", "arguments": "{\"q\":" } }] } }] })),
            chunk(json!({ "choices": [{ "delta": { "tool_calls": [
                { "index": 1, "id": "call_b", "function": { "name": "open", "arguments": "{}" } }] } }] })),
            chunk(json!({ "choices": [{ "delta": { "tool_calls": [
                { "index": 0, "function": { "arguments": "\"ad4m\"}" } }] } }] })),
            chunk(json!({ "choices": [{ "delta": {}, "finish_reason": "tool_calls" }] })),
            chunk(json!({ "choices": [], "usage": { "prompt_tokens": 7, "completion_tokens": 3 } })),
            "data: [DONE]\n\n".to_string(),
        ]
        .concat();
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_body(Matcher::PartialJson(json!({
                "stream": true,
                "stream_options": { "include_usage": true },
            })))
            .with_status(200)
            .with_header("content-type", "text/event-stream")
            .with_body(sse)
            .create_async()
            .await;

        let (tx, mut rx) = mpsc::unbounded_channel();
        let reply = OpenAiChat::new("", url(&server.url()))
            .chat_stream(
                ChatRequest::new("m", vec![ChatTurn::user("hi")]).with_tools(vec![search_tool()]),
                tx,
            )
            .await
            .expect("stream succeeds");
        mock.assert_async().await;

        let mut deltas = Vec::new();
        while let Ok(delta) = rx.try_recv() {
            deltas.push(delta);
        }
        assert_eq!(deltas, vec!["Hel", "lo"]);
        assert_eq!(reply.text, "Hello");
        assert_eq!(
            reply.tool_calls,
            vec![
                ToolCall {
                    id: "call_a".into(),
                    name: "search".into(),
                    arguments: json!({ "q": "ad4m" })
                },
                ToolCall {
                    id: "call_b".into(),
                    name: "open".into(),
                    arguments: json!({})
                },
            ]
        );
        assert_eq!(reply.usage.input_tokens, Some(7));
        assert_eq!(reply.usage.output_tokens, Some(3));
    }

    #[test]
    fn a_streamed_refusal_or_error_chunk_fails_the_stream() {
        let mut state = StreamState::default();
        assert!(state
            .apply(&json!({ "choices": [{ "delta": { "refusal": "no" } }] }))
            .is_err());
        let error = StreamState::default()
            .apply(&json!({ "error": { "message": "model crashed" } }))
            .unwrap_err();
        assert!(error.to_string().contains("model crashed"));
    }

    #[test]
    fn a_call_without_an_id_gets_one_and_bad_arguments_stay_visible() {
        let reply = read_reply(&json!({ "choices": [{ "message": { "tool_calls": [
            { "function": { "name": "f", "arguments": "not json" } }] } }] }))
        .unwrap();
        assert!(reply.tool_calls[0].id.starts_with("call_"));
        assert_eq!(reply.tool_calls[0].arguments, json!("not json"));
    }

    #[tokio::test]
    async fn listing_reads_ids_guesses_capabilities_and_context() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/v1/models")
            .match_header("authorization", Matcher::Missing)
            .with_status(200)
            .with_body(
                json!({ "data": [
                    { "id": "qwen3", "max_model_len": 32768 },
                    { "object": "model" },
                    { "id": "bge-m3", "meta": { "n_ctx_train": 8192 } },
                ] })
                .to_string(),
            )
            .create_async()
            .await;

        let models = discover(&server, "").list_models().await.unwrap();

        mock.assert_async().await;
        assert_eq!(
            models,
            vec![
                DiscoveredModel {
                    id: "qwen3".into(),
                    capabilities: vec![AiCapability::Chat],
                    context_length: Some(32768)
                },
                DiscoveredModel {
                    id: "bge-m3".into(),
                    capabilities: vec![AiCapability::Embeddings],
                    context_length: Some(8192)
                },
            ]
        );
    }

    #[tokio::test]
    async fn listing_sends_the_key_as_a_bearer_token_and_reports_a_refusal() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .match_header("authorization", "Bearer sk-test")
            .with_status(401)
            .with_body(r#"{"error":{"message":"Incorrect API key provided"}}"#)
            .create_async()
            .await;

        let error = discover(&server, "sk-test")
            .list_models()
            .await
            .unwrap_err();

        assert!(error.to_string().contains("401"), "{error}");
        assert!(
            error.to_string().contains("Incorrect API key provided"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn an_unreachable_endpoint_names_the_url_it_tried() {
        let unreachable = OpenAiDiscover::new(
            EndpointKind::OpenAi,
            "k",
            url("http://127.0.0.1:1/"),
            Duration::from_secs(5),
        );
        let error = unreachable.list_models().await.unwrap_err();
        assert!(error.to_string().contains("127.0.0.1:1"), "got: {error}");
    }
}
