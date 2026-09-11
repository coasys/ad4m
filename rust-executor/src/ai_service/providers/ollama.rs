//! Ollama native provider — `POST {base}/api/chat`.
//!
//! Ollama publishes an OpenAI-compatible endpoint at `/v1/chat/completions`,
//! but it silently ignores `options.num_ctx` and hard-caps context at ~16K
//! tokens.  The native `/api/chat` endpoint honours it.  For a prompt that
//! exceeds 16K — WE's schema context alone runs ~75K — the OpenAI-compat
//! path truncates the input without reporting an error, and the model never
//! sees tool definitions.
//!
//! The native endpoint also returns tool-call arguments as parsed objects
//! rather than JSON strings, streams ndjson lines rather than SSE, and uses
//! `GET /api/tags` (not `/v1/models`) for model discovery.
//!
//! ## Tools
//!
//! Ollama has supported native `tools[]` in `/api/chat` since v0.6.  The
//! schema shape matches OpenAI's `function` wrapper, and the response carries
//! `message.tool_calls[].function.{name, arguments}` with `arguments` already
//! parsed.  This provider sets `supports_native_tools` to `true`, so the
//! harness hands tool definitions through as structured data rather than
//! injecting them into the system prompt.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Mutex;
use std::time::Duration;
use tokio::sync::mpsc;
use url::Url;

use super::{
    ChatReply, ChatRequest, ChatRole, ChatTurn, ChatUsage, RemoteChat, ToolCall, ToolSpec,
};

/// Cap: never request more context than this, even if the model advertises a
/// larger window.  131072 tokens covers the WE schema context (~75K) plus
/// conversation history with room to spare.
const MAX_NUM_CTX: u32 = 131_072;

/// Fallback if `/api/show` cannot determine the model's native window.
/// Matches Ollama's own default, which allocates a KV cache the model
/// can actually serve without VRAM exhaustion on typical hardware.
const FALLBACK_NUM_CTX: u32 = 2_048;

/// Same reasoning as the Anthropic timeout — a long prompt on a local model
/// can take minutes.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(600);

pub struct OllamaChat {
    http: reqwest::Client,
    /// Base URL with `/api/chat` appended.
    endpoint: String,
    /// Base URL without any path suffix — used for `/api/show`.
    base: String,
    /// Per-model context window cache.  Populated on first use via `/api/show`.
    model_ctx_cache: Mutex<HashMap<String, u32>>,
}

impl OllamaChat {
    /// Build a client for `base_url`.
    ///
    /// Accepts the base URL with or without a trailing slash.  A `/v1` suffix
    /// is stripped — saved URLs from previous configurations may carry the
    /// OpenAI-compat path, and appending `/api/chat` to that would break.
    pub fn new(_api_key: &str, base_url: Url) -> Self {
        let trimmed = base_url.as_str().trim_end_matches('/');
        let root = trimmed.strip_suffix("/v1").unwrap_or(trimmed);
        Self {
            http: reqwest::Client::builder()
                .timeout(REQUEST_TIMEOUT)
                .build()
                .unwrap_or_default(),
            endpoint: format!("{root}/api/chat"),
            base: root.to_string(),
            model_ctx_cache: Mutex::new(HashMap::new()),
        }
    }

    /// Determine `num_ctx` for a model by querying `/api/show`.
    ///
    /// Ollama sizes the KV cache from `num_ctx` at model load.  Requesting
    /// 131K on a model whose native window is 4K or 8K causes multi-GB KV
    /// allocations that can exhaust VRAM or force layers to CPU.  Querying
    /// the model's own context length and capping at `MAX_NUM_CTX` avoids
    /// this: a 4K model gets 4K; a 128K model gets 128K; nothing exceeds
    /// the cap.
    ///
    /// Result is cached per model name for the lifetime of this client.
    async fn resolve_num_ctx(&self, model: &str) -> u32 {
        // Check cache first (lock scope: just the lookup).
        if let Some(&cached) = self.model_ctx_cache.lock().unwrap().get(model) {
            return cached;
        }

        let ctx = self.query_model_ctx(model).await;

        // Cache the result.
        self.model_ctx_cache
            .lock()
            .unwrap()
            .insert(model.to_string(), ctx);
        ctx
    }

    /// Query `/api/show` for a model's context length.
    ///
    /// Walks the `model_info` map looking for `<arch>.context_length`.  Falls
    /// back to `FALLBACK_NUM_CTX` on any error — the request still goes out,
    /// just with a conservative window.
    async fn query_model_ctx(&self, model: &str) -> u32 {
        let url = format!("{}/api/show", self.base);
        let response = match self
            .http
            .post(&url)
            .json(&serde_json::json!({ "model": model }))
            .send()
            .await
        {
            Ok(r) if r.status().is_success() => r,
            Ok(r) => {
                log::warn!(
                    "Ollama /api/show returned {} for {model}; falling back to num_ctx={FALLBACK_NUM_CTX}",
                    r.status()
                );
                return FALLBACK_NUM_CTX;
            }
            Err(e) => {
                log::warn!(
                    "Could not query Ollama /api/show for {model}: {e}; falling back to num_ctx={FALLBACK_NUM_CTX}"
                );
                return FALLBACK_NUM_CTX;
            }
        };

        let json: serde_json::Value = match response.json().await {
            Ok(v) => v,
            Err(e) => {
                log::warn!("Could not parse /api/show response for {model}: {e}");
                return FALLBACK_NUM_CTX;
            }
        };

        // model_info contains architecture-specific keys like
        // "qwen3.context_length", "llama.context_length", etc.
        if let Some(info) = json.get("model_info").and_then(|v| v.as_object()) {
            for (key, val) in info {
                if key.ends_with(".context_length") {
                    if let Some(ctx) = val.as_u64() {
                        let capped = (ctx as u32).min(MAX_NUM_CTX);
                        log::info!(
                            "Ollama model {model}: context_length={ctx}, using num_ctx={capped}"
                        );
                        return capped;
                    }
                }
            }
        }

        // Some Modelfiles set num_ctx in parameters directly.
        if let Some(params) = json.get("parameters").and_then(|v| v.as_str()) {
            for line in params.lines() {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 && parts[0] == "num_ctx" {
                    if let Ok(ctx) = parts[1].parse::<u32>() {
                        let capped = ctx.min(MAX_NUM_CTX);
                        log::info!("Ollama model {model}: Modelfile num_ctx={ctx}, using {capped}");
                        return capped;
                    }
                }
            }
        }

        log::warn!("Could not determine context_length for {model}; falling back to num_ctx={FALLBACK_NUM_CTX}");
        FALLBACK_NUM_CTX
    }
}

/// Ask Ollama which models are pulled — `GET {base}/api/tags`.
///
/// The response shape differs from OpenAI: `{"models": [{"name": …}, …]}`
/// rather than `{"data": [{"id": …}, …]}`.  No auth required — Ollama runs
/// without credentials by default.
pub async fn list_models(_api_key: &str, base_url: Url) -> Result<Vec<String>> {
    let trimmed = base_url.as_str().trim_end_matches('/');
    let root = trimmed.strip_suffix("/v1").unwrap_or(trimmed);
    let endpoint = format!("{root}/api/tags");

    let response = reqwest::Client::new()
        .get(&endpoint)
        .send()
        .await
        .map_err(|e| anyhow!("Could not reach {endpoint}: {e}"))?;

    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(anyhow!("Model listing failed ({status}): {body}"));
    }

    let json: serde_json::Value = response
        .json()
        .await
        .map_err(|e| anyhow!("Could not read the model list: {e}"))?;

    Ok(model_names_from_tags(&json))
}

/// Extract model names from Ollama's `/api/tags` response.
///
/// `{"models": [{"name": "qwen3:32b", ...}, ...]}` — the `name` field
/// includes the tag, which is what `model` in a chat request expects.
fn model_names_from_tags(json: &serde_json::Value) -> Vec<String> {
    json.get("models")
        .and_then(|m| m.as_array())
        .map(|entries| {
            entries
                .iter()
                .filter_map(|entry| entry.get("name")?.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Wire types
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct ChatRequestBody {
    model: String,
    messages: Vec<WireMessage>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<WireTool>,
    /// Controls whether Ollama streams ndjson lines.
    stream: bool,
    options: ChatOptions,
}

#[derive(Serialize)]
struct ChatOptions {
    num_ctx: u32,
}

#[derive(Serialize)]
struct WireMessage {
    role: &'static str,
    content: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tool_calls: Vec<WireToolCall>,
}

#[derive(Serialize)]
struct WireTool {
    #[serde(rename = "type")]
    kind: &'static str,
    function: WireToolFunction,
}

#[derive(Serialize)]
struct WireToolFunction {
    name: String,
    description: String,
    parameters: serde_json::Value,
}

/// A tool call on the wire — wraps name + arguments in a `function` envelope,
/// matching the OpenAI-like shape Ollama uses.
#[derive(Serialize, Deserialize, Clone)]
struct WireToolCall {
    #[serde(default)]
    id: Option<String>,
    function: WireToolCallFunction,
}

#[derive(Serialize, Deserialize, Clone)]
struct WireToolCallFunction {
    name: String,
    /// Ollama returns arguments as a parsed object, not a JSON string.
    arguments: serde_json::Value,
}

#[derive(Deserialize)]
struct ChatResponse {
    message: Option<ResponseMessage>,
    #[serde(default)]
    done: bool,
    /// Present on the final chunk; not read but kept so the struct
    /// deserialises the full response without `deny_unknown_fields`.
    #[serde(default)]
    #[allow(dead_code)]
    done_reason: Option<String>,
    #[serde(default)]
    prompt_eval_count: Option<u64>,
    #[serde(default)]
    eval_count: Option<u64>,
}

#[derive(Deserialize)]
struct ResponseMessage {
    #[serde(default)]
    content: String,
    #[serde(default)]
    tool_calls: Vec<WireToolCall>,
}

// ---------------------------------------------------------------------------
// Conversion
// ---------------------------------------------------------------------------

fn role_str(role: ChatRole) -> &'static str {
    match role {
        ChatRole::System => "system",
        ChatRole::User => "user",
        ChatRole::Assistant => "assistant",
    }
}

/// Build a wire message from a neutral turn.
///
/// Tool results go as a `tool` role message.  Tool calls on assistant turns
/// carry the calls array so the model sees its own prior invocations.
fn to_wire_message(turn: &ChatTurn) -> WireMessage {
    // Tool result → role: "tool"
    if turn.tool_result_for.is_some() {
        return WireMessage {
            role: "tool",
            content: turn.content.clone(),
            tool_calls: Vec::new(),
        };
    }

    let tool_calls: Vec<WireToolCall> = turn
        .tool_calls
        .iter()
        .map(|call| WireToolCall {
            id: Some(call.id.clone()),
            function: WireToolCallFunction {
                name: call.name.clone(),
                arguments: call.arguments.clone(),
            },
        })
        .collect();

    WireMessage {
        role: role_str(turn.role),
        content: turn.content.clone(),
        tool_calls,
    }
}

fn to_wire_tool(spec: &ToolSpec) -> WireTool {
    WireTool {
        kind: "function",
        function: WireToolFunction {
            name: spec.name.clone(),
            description: spec.description.clone(),
            parameters: spec.parameters.clone(),
        },
    }
}

/// Extract tool calls from the response message.
///
/// Ollama returns `arguments` as a parsed object.  The id field may be absent;
/// we generate one from the index to satisfy callers that need correlation.
fn extract_tool_calls(wire_calls: &[WireToolCall]) -> Vec<ToolCall> {
    wire_calls
        .iter()
        .enumerate()
        .map(|(i, call)| ToolCall {
            id: call.id.clone().unwrap_or_else(|| format!("call_{i}")),
            name: call.function.name.clone(),
            arguments: call.function.arguments.clone(),
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Trait implementation
// ---------------------------------------------------------------------------

#[async_trait]
impl RemoteChat for OllamaChat {
    fn supports_native_tools(&self) -> bool {
        true
    }

    async fn chat(&self, request: ChatRequest) -> Result<ChatReply> {
        let num_ctx = self.resolve_num_ctx(&request.model).await;
        let body = ChatRequestBody {
            model: request.model,
            messages: request.messages.iter().map(to_wire_message).collect(),
            tools: request.tools.iter().map(to_wire_tool).collect(),
            stream: false,
            options: ChatOptions { num_ctx },
        };

        let response = self
            .http
            .post(&self.endpoint)
            .json(&body)
            .send()
            .await
            .map_err(|e| anyhow!("Could not reach Ollama at {}: {e}", self.endpoint))?;

        let status = response.status();
        if !status.is_success() {
            let err_body = response.text().await.unwrap_or_default();
            return Err(anyhow!("Ollama API error {status}: {err_body}"));
        }

        let parsed: ChatResponse = response
            .json()
            .await
            .map_err(|e| anyhow!("Could not read Ollama response: {e}"))?;

        let message = parsed
            .message
            .ok_or_else(|| anyhow!("Ollama response contained no message"))?;

        let tool_calls = extract_tool_calls(&message.tool_calls);

        Ok(ChatReply {
            text: message.content,
            tool_calls,
            usage: ChatUsage {
                input_tokens: parsed.prompt_eval_count,
                output_tokens: parsed.eval_count,
                ..Default::default()
            },
        })
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        use futures::StreamExt;

        let num_ctx = self.resolve_num_ctx(&request.model).await;
        let body = ChatRequestBody {
            model: request.model,
            messages: request.messages.iter().map(to_wire_message).collect(),
            tools: request.tools.iter().map(to_wire_tool).collect(),
            stream: true,
            options: ChatOptions { num_ctx },
        };

        let response = self
            .http
            .post(&self.endpoint)
            .json(&body)
            .send()
            .await
            .map_err(|e| anyhow!("Could not reach Ollama at {}: {e}", self.endpoint))?;

        let status = response.status();
        if !status.is_success() {
            let err_body = response.text().await.unwrap_or_default();
            return Err(anyhow!("Ollama API error {status}: {err_body}"));
        }

        let mut stream = response.bytes_stream();
        let mut buffer: Vec<u8> = Vec::new();
        let mut text = String::new();
        let mut tool_calls = Vec::new();
        let mut usage = ChatUsage::default();

        while let Some(chunk) = stream.next().await {
            let chunk = chunk.map_err(|e| anyhow!("Ollama stream failed: {e}"))?;
            buffer.extend_from_slice(&chunk);

            // Ollama streams ndjson: one JSON object per line.
            while let Some(newline) = buffer.iter().position(|b| *b == b'\n') {
                let line: Vec<u8> = buffer.drain(..=newline).collect();
                let line = String::from_utf8_lossy(&line);
                let line = line.trim();

                if line.is_empty() {
                    continue;
                }

                let parsed: ChatResponse = match serde_json::from_str(line) {
                    Ok(p) => p,
                    Err(_) => continue,
                };

                if let Some(ref message) = parsed.message {
                    if !message.content.is_empty() {
                        text.push_str(&message.content);
                        if tokens.send(message.content.clone()).is_err() {
                            // Consumer hung up.
                            return Ok(ChatReply {
                                text,
                                tool_calls,
                                usage,
                            });
                        }
                    }

                    if !message.tool_calls.is_empty() {
                        tool_calls = extract_tool_calls(&message.tool_calls);
                    }
                }

                if parsed.done {
                    usage = ChatUsage {
                        input_tokens: parsed.prompt_eval_count,
                        output_tokens: parsed.eval_count,
                        ..Default::default()
                    };
                }
            }
        }

        Ok(ChatReply {
            text,
            tool_calls,
            usage,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(s: &str) -> Url {
        Url::parse(s).expect("test URL parses")
    }

    #[test]
    fn the_endpoint_strips_a_trailing_v1() {
        let client = OllamaChat::new("", url("http://localhost:11434/v1"));
        assert_eq!(client.endpoint, "http://localhost:11434/api/chat");
    }

    #[test]
    fn a_bare_origin_gets_api_chat_appended() {
        let client = OllamaChat::new("", url("http://localhost:11434"));
        assert_eq!(client.endpoint, "http://localhost:11434/api/chat");
    }

    #[test]
    fn a_trailing_slash_is_tolerated() {
        let client = OllamaChat::new("", url("http://localhost:11434/"));
        assert_eq!(client.endpoint, "http://localhost:11434/api/chat");
    }

    #[test]
    fn model_names_are_read_from_the_tags_response() {
        let json = serde_json::json!({
            "models": [
                {"name": "qwen3:32b", "size": 19000000000_u64},
                {"name": "llama3.1:8b", "size": 4700000000_u64},
            ]
        });
        assert_eq!(
            model_names_from_tags(&json),
            vec!["qwen3:32b", "llama3.1:8b"]
        );
    }

    #[test]
    fn an_entry_without_a_name_is_skipped() {
        let json = serde_json::json!({
            "models": [
                {"name": "a"},
                {"size": 100},
                {"name": "b"},
            ]
        });
        assert_eq!(model_names_from_tags(&json), vec!["a", "b"]);
    }

    #[test]
    fn a_response_with_no_models_array_lists_nothing() {
        assert_eq!(
            model_names_from_tags(&serde_json::json!({"error": "nope"})),
            Vec::<String>::new()
        );
    }

    #[test]
    fn this_provider_advertises_native_tool_support() {
        let client = OllamaChat::new("", url("http://localhost:11434"));
        assert!(client.supports_native_tools());
    }

    #[test]
    fn tool_calls_are_extracted_with_generated_ids_when_absent() {
        let wire = vec![WireToolCall {
            id: None,
            function: WireToolCallFunction {
                name: "search".into(),
                arguments: serde_json::json!({"q": "x"}),
            },
        }];

        let calls = extract_tool_calls(&wire);
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].id, "call_0");
        assert_eq!(calls[0].name, "search");
        assert_eq!(calls[0].arguments["q"], "x");
    }

    #[test]
    fn tool_calls_preserve_provided_ids() {
        let wire = vec![WireToolCall {
            id: Some("toolu_99".into()),
            function: WireToolCallFunction {
                name: "count".into(),
                arguments: serde_json::json!({}),
            },
        }];

        let calls = extract_tool_calls(&wire);
        assert_eq!(calls[0].id, "toolu_99");
    }

    #[test]
    fn a_tool_result_turn_becomes_a_tool_role_message() {
        let turn = ChatTurn::tool_result("call_0", "42");
        let wire = to_wire_message(&turn);
        assert_eq!(wire.role, "tool");
        assert_eq!(wire.content, "42");
    }

    #[test]
    fn an_assistant_turn_with_calls_carries_them() {
        let turn = ChatTurn::assistant_calling(
            "Let me look that up.",
            vec![ToolCall {
                id: "call_0".into(),
                name: "search".into(),
                arguments: serde_json::json!({"q": "x"}),
            }],
        );

        let wire = to_wire_message(&turn);
        assert_eq!(wire.role, "assistant");
        assert_eq!(wire.content, "Let me look that up.");
        assert_eq!(wire.tool_calls.len(), 1);
        assert_eq!(wire.tool_calls[0].function.name, "search");
    }

    #[test]
    fn a_tool_spec_serialises_in_the_function_wrapper() {
        let wire = to_wire_tool(&ToolSpec {
            name: "search".into(),
            description: "find things".into(),
            parameters: serde_json::json!({"type": "object"}),
        });

        let json = serde_json::to_value(&wire).expect("tool serialises");
        assert_eq!(json["type"], "function");
        assert_eq!(json["function"]["name"], "search");
        assert_eq!(json["function"]["parameters"]["type"], "object");
    }

    #[test]
    fn the_request_body_carries_num_ctx_and_tools() {
        let body = ChatRequestBody {
            model: "qwen3:32b".into(),
            messages: vec![WireMessage {
                role: "user",
                content: "hello".into(),
                tool_calls: Vec::new(),
            }],
            tools: vec![to_wire_tool(&ToolSpec {
                name: "test".into(),
                description: "a test tool".into(),
                parameters: serde_json::json!({"type": "object"}),
            })],
            stream: false,
            options: ChatOptions {
                num_ctx: MAX_NUM_CTX,
            },
        };

        let json = serde_json::to_value(&body).expect("body serialises");
        assert_eq!(json["options"]["num_ctx"], MAX_NUM_CTX);
        assert_eq!(json["tools"][0]["type"], "function");
        assert_eq!(json["stream"], false);
    }

    #[test]
    fn empty_tools_are_omitted_from_the_body() {
        let body = ChatRequestBody {
            model: "qwen3:32b".into(),
            messages: vec![WireMessage {
                role: "user",
                content: "hello".into(),
                tool_calls: Vec::new(),
            }],
            tools: Vec::new(),
            stream: false,
            options: ChatOptions {
                num_ctx: FALLBACK_NUM_CTX,
            },
        };

        let json = serde_json::to_value(&body).expect("body serialises");
        assert!(json.get("tools").is_none());
    }
}

/// Wire-level tests against a mock server.
#[cfg(test)]
mod wire_tests {
    use super::*;
    use serde_json::json;

    fn turns() -> Vec<ChatTurn> {
        vec![ChatTurn::system("be terse"), ChatTurn::user("hello")]
    }

    /// Mock `/api/show` to return model info with a given context length.
    async fn mock_show(server: &mut mockito::ServerGuard, ctx_len: u64) -> mockito::Mock {
        server
            .mock("POST", "/api/show")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "model_info": {
                        "general.architecture": "qwen3",
                        "qwen3.context_length": ctx_len,
                    }
                })
                .to_string(),
            )
            .create_async()
            .await
    }

    /// Mock `/api/show` to return a 404 (model not found).
    async fn mock_show_missing(server: &mut mockito::ServerGuard) -> mockito::Mock {
        server
            .mock("POST", "/api/show")
            .with_status(404)
            .with_body(r#"{"error":"model not found"}"#)
            .create_async()
            .await
    }

    #[tokio::test]
    async fn a_completion_is_posted_to_api_chat() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        let mock = server
            .mock("POST", "/api/chat")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "message": {"role": "assistant", "content": "hi"},
                    "done": true,
                    "done_reason": "stop",
                    "prompt_eval_count": 10,
                    "eval_count": 2,
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat(ChatRequest::new("qwen3:32b", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
        assert_eq!(reply.text, "hi");
        assert_eq!(reply.usage.input_tokens, Some(10));
        assert_eq!(reply.usage.output_tokens, Some(2));
    }

    #[tokio::test]
    async fn num_ctx_comes_from_the_model_via_api_show() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        let mock = server
            .mock("POST", "/api/chat")
            .match_body(mockito::Matcher::PartialJson(json!({
                "model": "qwen3:32b",
                "options": {"num_ctx": 32768},
                "stream": false,
            })))
            .with_status(200)
            .with_body(
                json!({
                    "message": {"role": "assistant", "content": "ok"},
                    "done": true,
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        client
            .chat(ChatRequest::new("qwen3:32b", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn large_model_ctx_is_capped_at_max() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 1_048_576).await;
        let mock = server
            .mock("POST", "/api/chat")
            .match_body(mockito::Matcher::PartialJson(json!({
                "options": {"num_ctx": MAX_NUM_CTX},
            })))
            .with_status(200)
            .with_body(
                json!({
                    "message": {"role": "assistant", "content": "ok"},
                    "done": true,
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        client
            .chat(ChatRequest::new("big-model:latest", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn show_failure_falls_back_to_conservative_ctx() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show_missing(&mut server).await;
        let mock = server
            .mock("POST", "/api/chat")
            .match_body(mockito::Matcher::PartialJson(json!({
                "options": {"num_ctx": FALLBACK_NUM_CTX},
            })))
            .with_status(200)
            .with_body(
                json!({
                    "message": {"role": "assistant", "content": "ok"},
                    "done": true,
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        client
            .chat(ChatRequest::new("missing:latest", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn tools_go_out_in_the_function_wrapper() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        let mock = server
            .mock("POST", "/api/chat")
            .match_body(mockito::Matcher::PartialJson(json!({
                "tools": [{
                    "type": "function",
                    "function": {
                        "name": "search",
                        "description": "find things",
                        "parameters": {"type": "object"},
                    },
                }],
            })))
            .with_status(200)
            .with_body(
                json!({
                    "message": {"role": "assistant", "content": ""},
                    "done": true,
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        client
            .chat(
                ChatRequest::new("qwen3:32b", turns()).with_tools(vec![ToolSpec {
                    name: "search".into(),
                    description: "find things".into(),
                    parameters: json!({"type": "object"}),
                }]),
            )
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn a_tool_call_response_comes_back_as_a_structured_call() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        server
            .mock("POST", "/api/chat")
            .with_status(200)
            .with_body(
                json!({
                    "message": {
                        "role": "assistant",
                        "content": "",
                        "tool_calls": [{
                            "function": {
                                "name": "search",
                                "arguments": {"q": "rust"},
                            },
                        }],
                    },
                    "done": true,
                    "done_reason": "stop",
                })
                .to_string(),
            )
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat(ChatRequest::new("qwen3:32b", turns()))
            .await
            .expect("completion succeeds");

        assert_eq!(reply.tool_calls.len(), 1);
        assert_eq!(reply.tool_calls[0].name, "search");
        assert_eq!(reply.tool_calls[0].arguments["q"], "rust");
        assert_eq!(reply.tool_calls[0].id, "call_0");
    }

    #[tokio::test]
    async fn a_server_error_surfaces_the_response_body() {
        let mut server = mockito::Server::new_async().await;
        // /api/show may or may not fire before the 500 — does not matter
        let _show = mock_show(&mut server, 32768).await;
        server
            .mock("POST", "/api/chat")
            .with_status(500)
            .with_body(r#"{"error":"model not found"}"#)
            .create_async()
            .await;

        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        let error = client
            .chat(ChatRequest::new("qwen3:32b", turns()))
            .await
            .expect_err("a 500 is an error");

        let msg = error.to_string();
        assert!(msg.contains("500"), "got: {msg}");
        assert!(msg.contains("model not found"), "got: {msg}");
    }

    #[tokio::test]
    async fn streamed_ndjson_yields_each_delta() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        let ndjson = format!(
            "{}\n{}\n{}\n",
            json!({"message": {"role": "assistant", "content": "Hello"}, "done": false}),
            json!({"message": {"role": "assistant", "content": ", world"}, "done": false}),
            json!({"message": {"role": "assistant", "content": ""}, "done": true, "done_reason": "stop", "prompt_eval_count": 10, "eval_count": 5}),
        );

        let mock = server
            .mock("POST", "/api/chat")
            .match_body(mockito::Matcher::PartialJson(json!({"stream": true})))
            .with_status(200)
            .with_body(ndjson)
            .create_async()
            .await;

        let (tx, mut rx) = mpsc::unbounded_channel();
        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat_stream(ChatRequest::new("qwen3:32b", turns()), tx)
            .await
            .expect("stream succeeds");

        mock.assert_async().await;

        let mut deltas = Vec::new();
        while let Ok(delta) = rx.try_recv() {
            deltas.push(delta);
        }

        assert_eq!(deltas, vec!["Hello", ", world"]);
        assert_eq!(reply.text, "Hello, world");
        assert_eq!(reply.usage.input_tokens, Some(10));
        assert_eq!(reply.usage.output_tokens, Some(5));
    }

    #[tokio::test]
    async fn tool_calls_arrive_on_the_final_streamed_chunk() {
        let mut server = mockito::Server::new_async().await;
        let _show = mock_show(&mut server, 32768).await;
        let ndjson = format!(
            "{}\n{}\n",
            json!({"message": {"role": "assistant", "content": "Let me search."}, "done": false}),
            json!({
                "message": {
                    "role": "assistant",
                    "content": "",
                    "tool_calls": [{"function": {"name": "search", "arguments": {"q": "x"}}}],
                },
                "done": true,
                "done_reason": "stop",
            }),
        );

        server
            .mock("POST", "/api/chat")
            .with_status(200)
            .with_body(ndjson)
            .create_async()
            .await;

        let (tx, _rx) = mpsc::unbounded_channel();
        let client = OllamaChat::new("", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat_stream(ChatRequest::new("qwen3:32b", turns()), tx)
            .await
            .expect("stream succeeds");

        assert_eq!(reply.text, "Let me search.");
        assert_eq!(reply.tool_calls.len(), 1);
        assert_eq!(reply.tool_calls[0].name, "search");
    }

    #[tokio::test]
    async fn listing_models_reads_the_tags_array() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/api/tags")
            .with_status(200)
            .with_body(
                json!({
                    "models": [
                        {"name": "qwen3:32b"},
                        {"name": "llama3.1:8b"},
                    ]
                })
                .to_string(),
            )
            .create_async()
            .await;

        let models = list_models("", Url::parse(&server.url()).unwrap())
            .await
            .expect("listing succeeds");

        mock.assert_async().await;
        assert_eq!(models, vec!["qwen3:32b", "llama3.1:8b"]);
    }

    #[tokio::test]
    async fn an_unreachable_endpoint_names_the_url_it_tried() {
        let error = list_models("", Url::parse("http://127.0.0.1:1/").unwrap())
            .await
            .expect_err("nothing listens there");

        assert!(error.to_string().contains("127.0.0.1:1"), "got: {error}");
    }
}
