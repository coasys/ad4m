//! OpenAI-shaped chat provider — `POST {base}/v1/chat/completions` — and model listing.
//!
//! Serves OpenAI itself and everything that copies its wire format: Groq, OpenRouter,
//! Google's compat endpoint, Ollama's `/v1`, vLLM, llama-server, speaches. Hand-rolled on
//! `reqwest`, as `anthropic.rs` is: the surface is one endpoint.
//!
//! Tools go out as `tools[]` and come back as `tool_calls`, also when streamed: the
//! deltas of one call arrive under one `index` and are joined here. `tool_choice` and
//! `response_format` pass through as the caller wrote them. Whether a model is *handed*
//! tools is the model's `native_tools` flag (`Model::uses_native_tools`), because
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
const REQUEST_TIMEOUT: Duration = super::http::CHAT_REQUEST_TIMEOUT;

pub struct OpenAiChat {
    http: reqwest::Client,
    api_key: String,
    /// Fully resolved, e.g. `https://api.groq.com/openai/v1/chat/completions`.
    endpoint: String,
    /// Which OpenAI-shaped server this is. llama-server honours
    /// `chat_template_kwargs`; a strict OpenAI endpoint rejects that field,
    /// so it is sent only for [`EndpointKind::LlamaServer`].
    kind: EndpointKind,
}

impl OpenAiChat {
    /// Build a client for `base_url`, given with or without its trailing `/v1`: every
    /// provider documents the `/v1` form, and a path prefix (Groq's `/openai/v1`, this
    /// executor's `/api/v1/openai/v1`) survives.
    pub fn new(api_key: &str, base_url: Url, kind: EndpointKind) -> Self {
        Self {
            // `expect`: the default client follows redirects, so a fallback to it would
            // drop the policy that keeps the key on the host it was set for.
            http: super::http::credentialed_http(REQUEST_TIMEOUT)
                .build()
                .expect("the HTTP client builds"),
            api_key: api_key.to_string(),
            endpoint: super::http::versioned_endpoint(base_url, "chat/completions"),
            kind,
        }
    }

    /// A client for tests: a strict OpenAI endpoint, so a body assertion that
    /// omits `chat_template_kwargs` stays the default.
    #[cfg(test)]
    fn new_test(api_key: &str, base_url: Url) -> Self {
        Self::new(api_key, base_url, EndpointKind::OpenAi)
    }

    /// `error` without the key: an upstream's error or refusal text may echo it.
    fn scrubbed(&self, error: anyhow::Error) -> anyhow::Error {
        anyhow!(super::http::scrub(&error.to_string(), &self.api_key))
    }

    async fn send(&self, body: &Value) -> Result<reqwest::Response> {
        let mut request = self.http.post(&self.endpoint).json(body);
        // A local server takes no key, and some refuse an empty `Bearer `.
        if !self.api_key.is_empty() {
            request = request.bearer_auth(&self.api_key);
        }
        let response = request.send().await.map_err(|e| {
            super::http::UpstreamUnreachable::new(&self.endpoint, &e, &self.api_key)
        })?;
        if !response.status().is_success() {
            let error: anyhow::Error =
                super::http::UpstreamStatus::read(response, &self.endpoint, &self.api_key)
                    .await
                    .into();
            super::http::log_upstream_error("OpenAI-compatible", &error);
            return Err(error);
        }
        Ok(response)
    }
}

/// The request body. Shared by both entry points so they cannot drift in what they send.
///
/// `chat_template_kwargs.enable_thinking` is llama-server's per-request switch
/// (it reads the field into the chat template; `-rea off` is the server-wide
/// one). A strict OpenAI endpoint rejects unknown fields, so only a
/// llama-server endpoint gets it. `max_tokens` goes with it: where the thinking
/// switch cannot be sent, a cap would only be spent on reasoning, so it is not sent.
fn build_body(request: ChatRequest, stream: bool, kind: EndpointKind) -> Value {
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
    } else if let Some(schema) = request.json_schema {
        // OpenAI's structured-output shape. llama-server reads `json_schema.schema` from it
        // into its grammar (tools/server/server-common.cpp, "Handle response_format"), and
        // vLLM's OpenAI-compatible server decodes against it (docs: features/structured_outputs).
        body["response_format"] = json!({
            "type": "json_schema",
            "json_schema": { "name": "reply", "schema": schema },
        });
    }
    if stream {
        body["stream"] = json!(true);
        body["stream_options"] = json!({ "include_usage": true });
    }
    if request.disable_thinking && kind == EndpointKind::LlamaServer {
        body["chat_template_kwargs"] = json!({ "enable_thinking": false });
    }
    // A cap without the switch is worse than none: a thinking model on an endpoint that
    // cannot be told to stop thinking spends the cap on reasoning and answers empty. When
    // the caller asked for no thinking and this kind cannot do it, the cap stays off too.
    let thinking_stays_on = request.disable_thinking && kind != EndpointKind::LlamaServer;
    if let Some(max_tokens) = request.max_output_tokens {
        if !thinking_stays_on {
            // OpenAI's reasoning models (o-series, gpt-5) reject `max_tokens`
            // and take `max_completion_tokens`. Other OpenAI-shaped servers
            // (llama-server, vLLM, clones) keep `max_tokens`.
            let field =
                if kind == EndpointKind::OpenAi && takes_max_completion_tokens(&request.model) {
                    "max_completion_tokens"
                } else {
                    "max_tokens"
                };
            body[field] = json!(max_tokens);
        }
    }
    body
}

/// Whether an OpenAI model name is a reasoning model that only accepts
/// `max_completion_tokens`: the o-series (`o1`, `o3-mini`, `o4-mini`, ...) and
/// the gpt-5 family. A provider prefix (`openai/o3`) is ignored.
fn takes_max_completion_tokens(model: &str) -> bool {
    let name = model
        .rsplit('/')
        .next()
        .unwrap_or(model)
        .to_ascii_lowercase();
    let o_series = name
        .strip_prefix('o')
        .and_then(|rest| rest.chars().next())
        .is_some_and(|c| c.is_ascii_digit());
    o_series || name.starts_with("gpt-5")
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
        stop_reason: json
            .pointer("/choices/0/finish_reason")
            .and_then(Value::as_str)
            .map(str::to_string),
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
            stop_reason: None,
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
        let response = self.send(&build_body(request, false, self.kind)).await?;
        let json: Value = super::http::read_json(response)
            .await
            .map_err(|e| anyhow!("Could not read the completion: {e}"))?;
        // A refusal is the upstream's text, which may echo the key.
        read_reply(&json).map_err(|e| self.scrubbed(e))
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        use futures::StreamExt;

        let response = self.send(&build_body(request, true, self.kind)).await?;
        let mut stream = response.bytes_stream();
        // Bytes, decoded a complete line at a time: a chunk boundary can split a
        // multi-byte character.
        let mut buffer: Vec<u8> = Vec::new();
        let mut state = StreamState::default();
        // `[DONE]`, or a chunk with a `finish_reason`: a stream that stops before either
        // was cut, and its reply is not whole.
        let mut finished = false;

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
                    finished = true;
                    break 'read;
                }
                let Ok(event) = serde_json::from_str::<Value>(payload) else {
                    continue;
                };
                if event
                    .pointer("/choices/0/finish_reason")
                    .is_some_and(|reason| !reason.is_null())
                {
                    finished = true;
                }
                if let Some(text) = state.apply(&event).map_err(|e| self.scrubbed(e))? {
                    if tokens.send(text).is_err() {
                        // The consumer hung up: nobody reads the rest.
                        return Ok(state.into_reply());
                    }
                }
            }
        }
        if !finished {
            return Err(anyhow!("The stream ended before the reply did"));
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
        // A 200 without the list (a gateway's error object) is no listing: the endpoint
        // would count as reachable.
        let entries = json
            .get("data")
            .and_then(Value::as_array)
            .ok_or_else(|| anyhow!("{url} answered without a model list"))?;
        Ok(entries
            .iter()
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
        let client = OpenAiChat::new_test("", url("https://api.groq.com/openai/v1"));
        assert_eq!(
            client.endpoint,
            "https://api.groq.com/openai/v1/chat/completions"
        );
        let bare = OpenAiChat::new_test("", url("http://localhost:11434"));
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
            EndpointKind::OpenAi,
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
        let reply = OpenAiChat::new_test("sk-test", url(&server.url()))
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

    #[test]
    fn a_json_schema_goes_out_as_response_format_and_the_callers_own_format_wins() {
        let schema = json!({ "type": "object", "properties": { "a": { "type": "string" } } });
        let constrained = ChatRequest::new("m", vec![ChatTurn::user("hi")])
            .with_json_schema(Some(schema.clone()));
        for stream in [false, true] {
            let body = build_body(constrained.clone(), stream, EndpointKind::OpenAi);
            assert_eq!(
                body["response_format"],
                json!({
                    "type": "json_schema",
                    "json_schema": { "name": "reply", "schema": schema },
                }),
                "stream: {stream}"
            );
        }

        let mut own = constrained;
        own.response_format = Some(json!({ "type": "json_object" }));
        assert_eq!(
            build_body(own, false, EndpointKind::OpenAi)["response_format"],
            json!({ "type": "json_object" })
        );

        let free = build_body(
            ChatRequest::new("m", vec![ChatTurn::user("hi")]),
            false,
            EndpointKind::OpenAi,
        );
        assert!(free.get("response_format").is_none(), "{free}");
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
        let reply = OpenAiChat::new_test("", url(&server.url()))
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
    async fn a_llama_server_no_thinking_request_sends_enable_thinking_false_and_max_tokens() {
        // llama-server honours `chat_template_kwargs.enable_thinking` (and
        // `max_tokens`). Interpretation of a thinking model otherwise fills
        // the window with reasoning and returns an empty answer.
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/chat/completions")
            .match_body(Matcher::PartialJson(json!({
                "chat_template_kwargs": { "enable_thinking": false },
                "max_tokens": 2048
            })))
            .with_status(200)
            .with_body(r#"{"choices":[{"message":{"content":"{}"}}]}"#)
            .create_async()
            .await;

        let client = OpenAiChat::new("", url(&server.url()), EndpointKind::LlamaServer);
        client
            .chat(
                ChatRequest::new("qwen3.8", vec![ChatTurn::user("extract")])
                    .with_generation(true, Some(2048)),
            )
            .await
            .expect("llama-server accepts the thinking switch");
        mock.assert_async().await;
    }

    #[test]
    fn a_strict_openai_endpoint_gets_neither_the_thinking_switch_nor_a_cap_that_needs_it() {
        // OpenAI rejects unknown fields, so `chat_template_kwargs` stays off. A no-thinking
        // request's cap would then only be spent on a thinking model's reasoning, so
        // `max_tokens` stays off too. A cap the caller set without asking for no thinking
        // is the caller's own and is sent.
        let no_thinking = build_body(
            ChatRequest::new("gpt-4o", vec![ChatTurn::user("extract")])
                .with_generation(true, Some(512)),
            false,
            EndpointKind::OpenAi,
        );
        assert!(
            no_thinking.get("chat_template_kwargs").is_none(),
            "{no_thinking}"
        );
        assert!(no_thinking.get("max_tokens").is_none(), "{no_thinking}");

        let capped_only = build_body(
            ChatRequest::new("gpt-4o", vec![ChatTurn::user("extract")])
                .with_generation(false, Some(512)),
            false,
            EndpointKind::OpenAi,
        );
        assert_eq!(capped_only["max_tokens"], 512, "{capped_only}");
    }

    #[test]
    fn an_openai_reasoning_model_gets_max_completion_tokens_and_others_max_tokens() {
        let capped = |model: &str, kind| {
            build_body(
                ChatRequest::new(model, vec![ChatTurn::user("x")])
                    .with_generation(false, Some(512)),
                false,
                kind,
            )
        };
        for model in [
            "o3-mini",
            "o1",
            "o4-mini",
            "openai/o3",
            "gpt-5",
            "gpt-5-mini",
        ] {
            let body = capped(model, EndpointKind::OpenAi);
            assert_eq!(body["max_completion_tokens"], 512, "{model}: {body}");
            assert!(body.get("max_tokens").is_none(), "{model}: {body}");
        }
        for model in ["gpt-4o", "gpt-4.1", "omni-x"] {
            let body = capped(model, EndpointKind::OpenAi);
            assert_eq!(body["max_tokens"], 512, "{model}: {body}");
            assert!(
                body.get("max_completion_tokens").is_none(),
                "{model}: {body}"
            );
        }
        // llama-server keeps max_tokens whatever the model is called.
        let body = capped("o3-mini", EndpointKind::LlamaServer);
        assert_eq!(body["max_tokens"], 512, "{body}");
        assert!(body.get("max_completion_tokens").is_none(), "{body}");
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
        let error = OpenAiChat::new_test("", url(&server.url()))
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
        let error = OpenAiChat::new_test("sk-test", url("http://127.0.0.1:1"))
            .chat(ChatRequest::new("m", vec![]))
            .await
            .unwrap_err();
        // Typed, so a caller can tell the endpoint is down (and re-check it), and `/v1`
        // answers a 503 for it (`OpenAIError::from_upstream`).
        assert!(
            error
                .downcast_ref::<crate::ai_service::providers::http::UpstreamUnreachable>()
                .is_some(),
            "{error}"
        );
        assert!(!error.to_string().contains("sk-test"), "{error}");

        let error = OpenAiChat::new_test("sk-test", url(&server.url()))
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
        let reply = OpenAiChat::new_test("", url(&server.url()))
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

    #[tokio::test]
    async fn a_refusal_or_stream_error_that_echoes_the_key_is_scrubbed() {
        let mut streaming = mockito::Server::new_async().await;
        streaming
            .mock("POST", "/v1/chat/completions")
            .with_status(200)
            .with_header("content-type", "text/event-stream")
            .with_body("data: {\"error\":{\"message\":\"bad key sk-echo\"}}\n\n")
            .create_async()
            .await;
        let mut refusing = mockito::Server::new_async().await;
        refusing
            .mock("POST", "/v1/chat/completions")
            .with_status(200)
            .with_body(r#"{"choices":[{"message":{"refusal":"not for sk-echo"}}]}"#)
            .create_async()
            .await;

        let refusal = OpenAiChat::new_test("sk-echo", url(&refusing.url()))
            .chat(ChatRequest::new("m", vec![ChatTurn::user("hi")]))
            .await
            .unwrap_err();
        let (tx, _rx) = mpsc::unbounded_channel();
        let stream = OpenAiChat::new_test("sk-echo", url(&streaming.url()))
            .chat_stream(ChatRequest::new("m", vec![ChatTurn::user("hi")]), tx)
            .await
            .unwrap_err();

        for error in [refusal, stream] {
            assert!(!error.to_string().contains("sk-echo"), "{error}");
            assert!(error.to_string().contains("<redacted>"), "{error}");
        }
    }

    #[tokio::test]
    async fn a_stream_that_stops_before_it_finishes_fails() {
        let cut = "data: {\"choices\":[{\"delta\":{\"content\":\"Hal\"}}]}\n\n";
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/chat/completions")
            .with_status(200)
            .with_header("content-type", "text/event-stream")
            .with_body(cut)
            .create_async()
            .await;

        let (tx, _rx) = mpsc::unbounded_channel();
        let error = OpenAiChat::new_test("", url(&server.url()))
            .chat_stream(ChatRequest::new("m", vec![ChatTurn::user("hi")]), tx)
            .await
            .unwrap_err();

        assert!(error.to_string().contains("ended before"), "{error}");
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
    async fn a_listing_without_a_data_array_is_an_error() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(200)
            .with_body(r#"{"error":"no such route"}"#)
            .create_async()
            .await;
        let error = discover(&server, "").list_models().await.unwrap_err();
        assert!(
            error.to_string().contains("without a model list"),
            "{error}"
        );
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
