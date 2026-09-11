//! Anthropic Messages API — `POST {base}/v1/messages`.
//!
//! Hand-rolled on `reqwest` rather than pulling a vendor SDK: the surface we
//! need is one endpoint, and the crate's other model clients are already
//! coasys forks, so a third-party dependency here would be the odd one out.
//!
//! ## Why not route Claude through the OpenAI client
//!
//! Anthropic publishes an OpenAI-compatible shim, and it would have been the
//! cheap option. Two things rule it out. It does not support prompt caching,
//! and their own docs call it a testing convenience rather than a production
//! path. Caching is the deciding one: an interpretation pass makes up to
//! `max_tool_calls` completions against a system prompt that does not change
//! between them, and without a cache breakpoint the node pays full input price
//! on every one of those turns.
//!
//! ## Caching policy
//!
//! The system prompt is marked `cache_control: ephemeral` and nothing else is.
//! It is the one part of a request that is stable by construction — the task's
//! system prompt plus, on the tool path, the rendered tool definitions — while
//! everything after it grows a turn at a time. Callers do not ask for this and
//! cannot switch it off: there is no way to express a breakpoint through the
//! OpenAI-shaped surface this executor exposes, so the decision has to be made
//! here, and marking a stable prefix is never the wrong call. Anthropic
//! silently declines to cache a prefix below its minimum, so a short prompt
//! costs nothing and errors nowhere.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::sync::mpsc;
use url::Url;

use super::{
    ChatReply, ChatRequest, ChatRole, ChatTurn, ChatUsage, RemoteChat, ToolCall, ToolSpec,
};

/// Wire version. Anthropic requires it on every request and treats it as the
/// contract: pinning it is what stops a server-side change reshaping our
/// responses. Bump deliberately, never to chase a new feature by accident.
const ANTHROPIC_VERSION: &str = "2023-06-01";

/// `max_tokens` is mandatory on this API — unlike the OpenAI path, where
/// omitting it lets the server pick. 16k is large enough that a long
/// interpretation answer is not clipped and small enough to bound a runaway.
///
/// Nothing overrides it. `ModelApi` carries no options, and the `max_tokens`
/// a client sends to the compat surface is dropped before it reaches either
/// provider. Threading that through is worth doing and is not done here.
const DEFAULT_MAX_TOKENS: u32 = 16_384;

/// A completion can legitimately run for minutes on a long prompt. Without a
/// timeout a hung upstream would park this model's worker thread forever,
/// since the thread handles one request at a time.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(600);

pub struct AnthropicChat {
    http: reqwest::Client,
    api_key: String,
    /// Fully-resolved messages endpoint, e.g. `https://api.anthropic.com/v1/messages`.
    endpoint: String,
}

impl AnthropicChat {
    pub fn new(api_key: &str, base_url: Url) -> Self {
        Self {
            http: reqwest::Client::builder()
                .timeout(REQUEST_TIMEOUT)
                .build()
                .unwrap_or_default(),
            api_key: api_key.to_string(),
            endpoint: super::versioned_endpoint(base_url, "messages"),
        }
    }
}

/// Ask Anthropic which models this key can reach — `GET {base}/v1/models`.
///
/// Same role as the OpenAI listing (see that module for why it is standalone),
/// with different auth: an `x-api-key` header and the pinned wire version
/// rather than a bearer token.
pub async fn list_models(api_key: &str, base_url: Url) -> Result<Vec<String>> {
    let endpoint = super::versioned_endpoint(base_url, "models");

    let response = reqwest::Client::new()
        .get(&endpoint)
        .header("x-api-key", api_key)
        .header("anthropic-version", ANTHROPIC_VERSION)
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

    Ok(super::model_ids_from_data(&json))
}

// ---------------------------------------------------------------------------
// Wire types
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct MessagesRequest {
    model: String,
    max_tokens: u32,
    /// Sent as a block list rather than a bare string so it can carry
    /// `cache_control`. Omitted when the conversation had no system turn.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    system: Vec<SystemBlock>,
    messages: Vec<WireMessage>,
    /// Omitted when empty, so a request without tools is byte-identical to
    /// what it was before tools existed.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<WireTool>,
    /// Omitted entirely rather than sent as `false`, so the non-streaming
    /// request stays byte-identical to what it was before streaming existed.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    stream: bool,
}

#[derive(Serialize)]
struct SystemBlock {
    #[serde(rename = "type")]
    kind: &'static str,
    text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    cache_control: Option<CacheControl>,
}

#[derive(Serialize)]
struct CacheControl {
    #[serde(rename = "type")]
    kind: &'static str,
}

/// A turn on the wire. `content` is either a plain string or a list of
/// content blocks — the API accepts both, and a turn only needs blocks when it
/// carries a tool call or a tool result.
#[derive(Serialize)]
struct WireMessage {
    role: &'static str,
    content: serde_json::Value,
}

#[derive(Serialize)]
struct WireTool {
    name: String,
    description: String,
    /// Anthropic's name for the JSON Schema every other provider calls
    /// `parameters`. Same document, different key.
    input_schema: serde_json::Value,
}

#[derive(Deserialize)]
struct MessagesResponse {
    #[serde(default)]
    content: Vec<ContentBlock>,
    #[serde(default)]
    usage: Option<WireUsage>,
    /// Why generation ended. `refusal` arrives with HTTP 200 and empty
    /// content, so a caller that reads `content` without checking this sees a
    /// successful empty answer.
    #[serde(default)]
    stop_reason: Option<String>,
    #[serde(default)]
    stop_details: Option<StopDetails>,
}

#[derive(Deserialize)]
struct StopDetails {
    #[serde(default)]
    category: Option<String>,
}

/// Anthropic's own token counts. Absent on an older wire version, so every
/// field is optional and a missing block reports nothing rather than zero —
/// "not told" and "none" are different answers, and a cache assertion turns
/// on which one it got.
#[derive(Default, Deserialize)]
struct WireUsage {
    #[serde(default)]
    input_tokens: Option<u64>,
    #[serde(default)]
    output_tokens: Option<u64>,
    #[serde(default)]
    cache_read_input_tokens: Option<u64>,
    #[serde(default)]
    cache_creation_input_tokens: Option<u64>,
}

impl From<WireUsage> for ChatUsage {
    fn from(w: WireUsage) -> Self {
        ChatUsage {
            input_tokens: w.input_tokens,
            output_tokens: w.output_tokens,
            cache_read_tokens: w.cache_read_input_tokens,
            cache_write_tokens: w.cache_creation_input_tokens,
        }
    }
}

#[derive(Deserialize)]
struct ContentBlock {
    #[serde(rename = "type")]
    kind: String,
    #[serde(default)]
    text: String,
    // --- present on `tool_use` blocks only ---
    #[serde(default)]
    id: String,
    #[serde(default)]
    name: String,
    #[serde(default)]
    input: serde_json::Value,
}

// ---------------------------------------------------------------------------

/// Split a turn list into Anthropic's shape: system turns are hoisted out of
/// the conversation and concatenated, the rest stay in order.
///
/// Concatenation with a newline matches how `api::openai_compat` already
/// hoists system/developer messages, so a conversation behaves the same
/// whichever provider answers it. Consecutive same-role turns are left alone —
/// the API merges them itself.
fn split_system(request: &ChatRequest) -> (Vec<String>, Vec<WireMessage>) {
    let mut system = Vec::new();
    let mut messages = Vec::new();

    for turn in &request.messages {
        match turn.role {
            ChatRole::System => system.push(turn.content.clone()),
            ChatRole::User => messages.push(WireMessage {
                role: "user",
                content: user_content(turn),
            }),
            ChatRole::Assistant => messages.push(WireMessage {
                role: "assistant",
                content: assistant_content(turn),
            }),
        }
    }

    (system, messages)
}

fn to_wire_tool(spec: &ToolSpec) -> WireTool {
    WireTool {
        name: spec.name.clone(),
        description: spec.description.clone(),
        input_schema: spec.parameters.clone(),
    }
}

/// A user turn is plain text unless it is answering a tool call, in which case
/// it is a single `tool_result` block naming the call it answers.
fn user_content(turn: &ChatTurn) -> serde_json::Value {
    match &turn.tool_result_for {
        Some(call_id) => serde_json::json!([{
            "type": "tool_result",
            "tool_use_id": call_id,
            "content": turn.content,
        }]),
        None => serde_json::Value::String(turn.content.clone()),
    }
}

/// An assistant turn is plain text unless it made tool calls, in which case it
/// is any text it wrote followed by one `tool_use` block per call.
///
/// The text is kept rather than dropped: models routinely write a sentence of
/// intent before calling something, and losing it leaves the next turn's
/// prompt with an unexplained call in the history.
fn assistant_content(turn: &ChatTurn) -> serde_json::Value {
    if turn.tool_calls.is_empty() {
        return serde_json::Value::String(turn.content.clone());
    }

    let mut blocks = Vec::with_capacity(turn.tool_calls.len() + 1);
    if !turn.content.is_empty() {
        blocks.push(serde_json::json!({ "type": "text", "text": turn.content }));
    }
    for call in &turn.tool_calls {
        blocks.push(serde_json::json!({
            "type": "tool_use",
            "id": call.id,
            "name": call.name,
            "input": call.arguments,
        }));
    }

    serde_json::Value::Array(blocks)
}

impl AnthropicChat {
    /// Assemble the wire request. Shared by both entry points so the streaming
    /// and non-streaming calls cannot drift in what they send — including the
    /// cache breakpoint, which would otherwise be easy to set in one and
    /// forget in the other.
    fn build_body(&self, request: ChatRequest, stream: bool) -> MessagesRequest {
        let (system_parts, messages) = split_system(&request);
        let tools: Vec<WireTool> = request.tools.iter().map(to_wire_tool).collect();

        let system = if system_parts.is_empty() {
            Vec::new()
        } else {
            vec![SystemBlock {
                kind: "text",
                text: system_parts.join("\n"),
                cache_control: Some(CacheControl { kind: "ephemeral" }),
            }]
        };

        MessagesRequest {
            model: request.model,
            max_tokens: DEFAULT_MAX_TOKENS,
            system,
            messages,
            tools,
            stream,
        }
    }

    /// POST the body, returning the response only if the status was a success.
    async fn send(&self, body: &MessagesRequest) -> Result<reqwest::Response> {
        let response = self
            .http
            .post(&self.endpoint)
            .header("x-api-key", &self.api_key)
            .header("anthropic-version", ANTHROPIC_VERSION)
            .json(body)
            .send()
            .await
            .map_err(|e| anyhow!("Error connecting to remote LLM API: {:?}", e))?;

        let status = response.status();
        if !status.is_success() {
            // Anthropic puts the useful part in the body (`error.message`);
            // the status alone does not distinguish a bad key from a bad
            // model name, and both are things an operator has to fix.
            let body = response.text().await.unwrap_or_default();
            return Err(anyhow!("Anthropic API error {}: {}", status, body));
        }

        Ok(response)
    }
}

/// Read a reply's content blocks into text and tool calls.
///
/// The two are separated rather than concatenated: a `tool_use` block is data,
/// and stringifying it into the answer is exactly the confusion that
/// text-extracted tool calling has to live with and native tool calling exists
/// to avoid.
fn read_content(blocks: &[ContentBlock]) -> (String, Vec<ToolCall>) {
    let mut text = String::new();
    let mut tool_calls = Vec::new();

    for block in blocks {
        match block.kind.as_str() {
            "text" => text.push_str(&block.text),
            "tool_use" => tool_calls.push(ToolCall {
                id: block.id.clone(),
                name: block.name.clone(),
                arguments: block.input.clone(),
            }),
            // A block type we do not know about is skipped rather than
            // guessed at — a future one appearing in the answer should not
            // corrupt the text or invent a call.
            _ => {}
        }
    }

    (text, tool_calls)
}

#[async_trait]
impl RemoteChat for AnthropicChat {
    fn supports_native_tools(&self) -> bool {
        true
    }

    async fn chat(&self, request: ChatRequest) -> Result<ChatReply> {
        let body = self.build_body(request, false);
        let response = self.send(&body).await?;

        let parsed: MessagesResponse = response
            .json()
            .await
            .map_err(|e| anyhow!("Could not read Anthropic response: {:?}", e))?;

        // A refusal is HTTP 200 with no content. Reporting it as an empty
        // success would bill the caller for an answer they did not get, and
        // hand a silent empty string to whatever asked. This is the same
        // failure the OpenAI arm used to have with `unwrap_or_default`.
        if parsed.stop_reason.as_deref() == Some("refusal") {
            let category = parsed
                .stop_details
                .and_then(|d| d.category)
                .unwrap_or_else(|| "unspecified".to_string());
            return Err(anyhow!(
                "Anthropic declined this request (category: {category}). No content was returned."
            ));
        }

        let (text, tool_calls) = read_content(&parsed.content);

        Ok(ChatReply {
            text,
            tool_calls,
            usage: parsed.usage.map(ChatUsage::from).unwrap_or_default(),
        })
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        use futures::StreamExt;

        // `build_body` is shared so the two calls cannot drift in what they
        // send — which means tools go out on this path too, and the read side
        // has no matching half: a `tool_use` block arrives as a run of
        // `input_json_delta` events that nothing here accumulates. `text_delta`
        // correctly refuses to treat them as text, so the call does not corrupt
        // the reply; it disappears, and the caller sees a successful completion
        // with no calls, which reads as "the model chose not to call anything".
        //
        // Refuse loudly instead. Accumulating the deltas per
        // `content_block_index` and flushing on `content_block_stop` is the
        // real fix and needs its own tests. Nothing streams with tools today.
        if !request.tools.is_empty() {
            return Err(anyhow!(
                "Streaming with native tools is not supported on this provider yet: \
                 tool_use blocks arrive as input_json_delta and are not accumulated. \
                 Use the non-streaming call for a turn that carries tools."
            ));
        }

        let body = self.build_body(request, true);
        let response = self.send(&body).await?;

        let mut stream = response.bytes_stream();
        // Raw bytes, not a String. A chunk boundary can fall inside a
        // multi-byte character, and decoding per chunk would replace both
        // halves with U+FFFD before the line buffer ever sees them — the
        // corruption would already have happened. Bytes accumulate here and
        // only a complete line is decoded, which is always valid UTF-8.
        let mut buffer: Vec<u8> = Vec::new();
        let mut text = String::new();
        let mut usage = ChatUsage::default();

        while let Some(chunk) = stream.next().await {
            let chunk = chunk.map_err(|e| anyhow!("Anthropic stream failed: {:?}", e))?;
            buffer.extend_from_slice(&chunk);

            while let Some(newline) = buffer.iter().position(|b| *b == b'\n') {
                let line: Vec<u8> = buffer.drain(..=newline).collect();
                let line = String::from_utf8_lossy(&line);
                let line = line.trim_end();

                // A refusal arrives mid-stream as a stop reason on a
                // `message_delta`, not as a status code. Without this the
                // caller receives whatever text arrived before the refusal,
                // reads it as a finished answer, and is billed for it — the
                // same failure the non-streaming path had.
                if let Some(category) = refusal_category(line) {
                    return Err(anyhow!(
                        "Anthropic declined this request (category: {category}). \
                         The stream ended without a complete answer."
                    ));
                }

                if let Some(reported) = usage_from_event(line) {
                    usage = merge_usage(usage, reported);
                }

                if let Some(delta) = text_delta(line) {
                    text.push_str(&delta);
                    if tokens.send(delta).is_err() {
                        // Consumer hung up. Stop reading rather than
                        // draining a response nobody will see.
                        return Ok(ChatReply {
                            text,
                            tool_calls: Vec::new(),
                            usage,
                        });
                    }
                }
            }
        }

        Ok(ChatReply {
            text,
            tool_calls: Vec::new(),
            usage,
        })
    }
}

/// The refusal category carried by a `message_delta`, if this line is one.
///
/// Streaming reports a refusal as a stop reason rather than a status, so it is
/// invisible to the error handling around the request itself.
fn refusal_category(line: &str) -> Option<String> {
    let event = sse_event(line)?;
    let delta = event.get("delta")?;
    if delta.get("stop_reason")?.as_str()? != "refusal" {
        return None;
    }
    Some(
        delta
            .get("stop_details")
            .and_then(|d| d.get("category"))
            .and_then(|c| c.as_str())
            .unwrap_or("unspecified")
            .to_string(),
    )
}

/// Token counts carried by a streamed event.
///
/// They arrive in pieces: `message_start` reports the input side, including
/// what was read from or written to the cache, and `message_delta` reports the
/// output side as it grows.
fn usage_from_event(line: &str) -> Option<ChatUsage> {
    let event = sse_event(line)?;
    let usage = event
        .get("usage")
        .or_else(|| event.get("message").and_then(|m| m.get("usage")))?;
    let wire: WireUsage = serde_json::from_value(usage.clone()).ok()?;
    Some(ChatUsage::from(wire))
}

/// Later reports override earlier ones, field by field. A `message_delta`
/// carries only what changed, so a blanket replace would drop the input and
/// cache counts that arrived with `message_start`.
fn merge_usage(mut into: ChatUsage, from: ChatUsage) -> ChatUsage {
    into.input_tokens = from.input_tokens.or(into.input_tokens);
    into.output_tokens = from.output_tokens.or(into.output_tokens);
    into.cache_read_tokens = from.cache_read_tokens.or(into.cache_read_tokens);
    into.cache_write_tokens = from.cache_write_tokens.or(into.cache_write_tokens);
    into
}

/// The JSON carried by one `data:` line, if it carries any.
fn sse_event(line: &str) -> Option<serde_json::Value> {
    let payload = line.strip_prefix("data:")?.trim();
    if payload.is_empty() {
        return None;
    }
    serde_json::from_str(payload).ok()
}

/// Pull the text out of one SSE line, if it carries any.
///
/// Only `content_block_delta` events with a `text_delta` matter here. The
/// lifecycle events (`message_start`, `content_block_start`, `ping`,
/// `message_stop`) and the `event:` lines carry no text, and a `tool_use`
/// block's `input_json_delta` is not text either — it is partial JSON that
/// would corrupt the reply if concatenated into it.
fn text_delta(line: &str) -> Option<String> {
    let event = sse_event(line)?;
    if event.get("type")?.as_str()? != "content_block_delta" {
        return None;
    }

    let delta = event.get("delta")?;
    if delta.get("type")?.as_str()? != "text_delta" {
        return None;
    }

    Some(delta.get("text")?.as_str()?.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(s: &str) -> Url {
        Url::parse(s).expect("test URL parses")
    }

    #[test]
    fn system_turns_are_hoisted_out_of_the_conversation() {
        let request = ChatRequest::new(
            "claude-opus-5",
            vec![
                super::super::ChatTurn::system("be terse"),
                super::super::ChatTurn::user("hello"),
                super::super::ChatTurn::assistant("hi"),
                super::super::ChatTurn::user("again"),
            ],
        );

        let (system, messages) = split_system(&request);

        assert_eq!(system, vec!["be terse".to_string()]);
        assert_eq!(messages.len(), 3);
        assert_eq!(messages[0].role, "user");
        assert_eq!(messages[1].role, "assistant");
        assert_eq!(messages[2].role, "user");
    }

    #[test]
    fn several_system_turns_concatenate() {
        let request = ChatRequest::new(
            "claude-opus-5",
            vec![
                super::super::ChatTurn::system("first"),
                super::super::ChatTurn::system("second"),
                super::super::ChatTurn::user("go"),
            ],
        );

        let (system, _) = split_system(&request);
        assert_eq!(system.join("\n"), "first\nsecond");
    }

    #[test]
    fn the_system_block_carries_a_cache_breakpoint() {
        let body = MessagesRequest {
            model: "claude-opus-5".to_string(),
            max_tokens: DEFAULT_MAX_TOKENS,
            system: vec![SystemBlock {
                kind: "text",
                text: "stable prefix".to_string(),
                cache_control: Some(CacheControl { kind: "ephemeral" }),
            }],
            messages: vec![],
            tools: vec![],
            stream: false,
        };

        let json = serde_json::to_value(&body).expect("request serialises");
        assert_eq!(json["system"][0]["cache_control"]["type"], "ephemeral");
    }

    #[test]
    fn a_text_delta_yields_its_text() {
        let line = r#"data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}"#;
        assert_eq!(text_delta(line), Some("Hello".to_string()));
    }

    #[test]
    fn a_data_line_without_a_space_after_the_colon_still_parses() {
        // The spec allows `data:{...}`; Anthropic sends `data: {...}`. Accept
        // both rather than depending on which one the far end chose.
        let line =
            r#"data:{"type":"content_block_delta","delta":{"type":"text_delta","text":"x"}}"#;
        assert_eq!(text_delta(line), Some("x".to_string()));
    }

    #[test]
    fn lifecycle_events_carry_no_text() {
        for line in [
            "event: message_start",
            r#"data: {"type":"message_start","message":{"id":"msg_1"}}"#,
            r#"data: {"type":"content_block_start","index":0}"#,
            r#"data: {"type":"message_stop"}"#,
            r#"data: {"type":"ping"}"#,
            "",
            "data:",
        ] {
            assert_eq!(text_delta(line), None, "expected no text from {line:?}");
        }
    }

    #[test]
    fn a_tool_use_json_delta_is_not_treated_as_text() {
        // `input_json_delta` carries partial JSON for a tool call's arguments.
        // Concatenating it into the reply would corrupt the visible answer
        // with fragments of a structure that belongs somewhere else.
        let line = r#"data: {"type":"content_block_delta","delta":{"type":"input_json_delta","partial_json":"{\"a\":"}}"#;
        assert_eq!(text_delta(line), None);
    }

    #[test]
    fn a_malformed_event_is_skipped_rather_than_failing_the_stream() {
        // A half-written line can reach us if the upstream truncates; losing
        // one delta beats aborting a completion the caller is paying for.
        assert_eq!(text_delta(r#"data: {"type":"content_bl"#), None);
    }

    #[test]
    fn a_tool_result_turn_becomes_a_tool_result_block() {
        let turn = ChatTurn::tool_result("toolu_1", "42");
        let content = user_content(&turn);

        assert_eq!(content[0]["type"], "tool_result");
        assert_eq!(content[0]["tool_use_id"], "toolu_1");
        assert_eq!(content[0]["content"], "42");
    }

    #[test]
    fn an_ordinary_user_turn_stays_a_plain_string() {
        let content = user_content(&ChatTurn::user("hello"));
        assert_eq!(content, serde_json::json!("hello"));
    }

    #[test]
    fn an_assistant_turn_with_calls_carries_its_text_and_every_call() {
        // Models routinely write a sentence of intent before calling
        // something; dropping it leaves an unexplained call in the history.
        let turn = ChatTurn::assistant_calling(
            "Looking that up.",
            vec![
                ToolCall {
                    id: "toolu_1".into(),
                    name: "search".into(),
                    arguments: serde_json::json!({ "q": "x" }),
                },
                ToolCall {
                    id: "toolu_2".into(),
                    name: "count".into(),
                    arguments: serde_json::json!({}),
                },
            ],
        );

        let content = assistant_content(&turn);

        assert_eq!(content[0]["type"], "text");
        assert_eq!(content[0]["text"], "Looking that up.");
        assert_eq!(content[1]["type"], "tool_use");
        assert_eq!(content[1]["id"], "toolu_1");
        assert_eq!(content[1]["input"]["q"], "x");
        assert_eq!(content[2]["id"], "toolu_2");
    }

    #[test]
    fn an_assistant_turn_that_only_called_omits_the_empty_text_block() {
        let turn = ChatTurn::assistant_calling(
            "",
            vec![ToolCall {
                id: "toolu_1".into(),
                name: "search".into(),
                arguments: serde_json::json!({}),
            }],
        );

        let content = assistant_content(&turn);
        assert_eq!(content.as_array().map(|a| a.len()), Some(1));
        assert_eq!(content[0]["type"], "tool_use");
    }

    #[test]
    fn an_assistant_turn_without_calls_stays_a_plain_string() {
        let content = assistant_content(&ChatTurn::assistant("just talking"));
        assert_eq!(content, serde_json::json!("just talking"));
    }

    #[test]
    fn a_tool_schema_is_sent_as_input_schema() {
        let wire = to_wire_tool(&ToolSpec {
            name: "search".into(),
            description: "find things".into(),
            parameters: serde_json::json!({ "type": "object" }),
        });

        let json = serde_json::to_value(&wire).expect("tool serialises");
        assert_eq!(json["name"], "search");
        assert_eq!(json["input_schema"]["type"], "object");
        assert!(json.get("parameters").is_none());
    }

    #[test]
    fn text_and_tool_calls_are_read_apart_rather_than_concatenated() {
        let blocks = vec![
            ContentBlock {
                kind: "text".into(),
                text: "one moment".into(),
                id: String::new(),
                name: String::new(),
                input: serde_json::Value::Null,
            },
            ContentBlock {
                kind: "tool_use".into(),
                text: String::new(),
                id: "toolu_9".into(),
                name: "search".into(),
                input: serde_json::json!({ "q": "y" }),
            },
        ];

        let (text, calls) = read_content(&blocks);

        assert_eq!(text, "one moment");
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].id, "toolu_9");
        assert_eq!(calls[0].arguments["q"], "y");
    }

    #[test]
    fn an_unknown_block_type_is_skipped_rather_than_guessed_at() {
        let blocks = vec![ContentBlock {
            kind: "something_new".into(),
            text: "should not appear".into(),
            id: String::new(),
            name: String::new(),
            input: serde_json::Value::Null,
        }];

        let (text, calls) = read_content(&blocks);
        assert_eq!(text, "");
        assert!(calls.is_empty());
    }

    #[test]
    fn this_provider_advertises_native_tool_support() {
        // The harness branches on this to decide between handing tools over
        // and rendering them into the prompt; answering it wrongly sends a
        // model down a path its wire format cannot serve.
        let client = AnthropicChat::new("k", url("https://api.anthropic.com"));
        assert!(client.supports_native_tools());
    }

    #[test]
    fn a_streamed_refusal_is_recognised_with_its_category() {
        let line = r#"data: {"type":"message_delta","delta":{"stop_reason":"refusal","stop_details":{"category":"cyber"}}}"#;
        assert_eq!(refusal_category(line), Some("cyber".to_string()));
    }

    #[test]
    fn a_streamed_refusal_without_a_category_still_reports_one() {
        let line = r#"data: {"type":"message_delta","delta":{"stop_reason":"refusal"}}"#;
        assert_eq!(refusal_category(line), Some("unspecified".to_string()));
    }

    #[test]
    fn an_ordinary_stop_reason_is_not_a_refusal() {
        for line in [
            r#"data: {"type":"message_delta","delta":{"stop_reason":"end_turn"}}"#,
            r#"data: {"type":"message_delta","delta":{"stop_reason":"max_tokens"}}"#,
            r#"data: {"type":"content_block_delta","delta":{"type":"text_delta","text":"hi"}}"#,
        ] {
            assert_eq!(refusal_category(line), None, "for {line}");
        }
    }

    #[test]
    fn message_start_carries_the_input_and_cache_counts() {
        let line = r#"data: {"type":"message_start","message":{"usage":{"input_tokens":12,"cache_read_input_tokens":900}}}"#;
        let usage = usage_from_event(line).expect("usage is present");
        assert_eq!(usage.input_tokens, Some(12));
        assert_eq!(usage.cache_read_tokens, Some(900));
    }

    #[test]
    fn message_delta_carries_the_output_count() {
        let line = r#"data: {"type":"message_delta","usage":{"output_tokens":40}}"#;
        let usage = usage_from_event(line).expect("usage is present");
        assert_eq!(usage.output_tokens, Some(40));
    }

    #[test]
    fn a_later_report_does_not_erase_an_earlier_one() {
        // `message_delta` carries only what changed. Replacing wholesale would
        // drop the input and cache counts that arrived with `message_start`.
        let start = ChatUsage {
            input_tokens: Some(12),
            cache_read_tokens: Some(900),
            ..Default::default()
        };
        let delta = ChatUsage {
            output_tokens: Some(40),
            ..Default::default()
        };

        let merged = merge_usage(start, delta);
        assert_eq!(merged.input_tokens, Some(12));
        assert_eq!(merged.cache_read_tokens, Some(900));
        assert_eq!(merged.output_tokens, Some(40));
    }

    #[test]
    fn an_absent_system_prompt_is_omitted_rather_than_sent_empty() {
        let body = MessagesRequest {
            model: "claude-opus-5".to_string(),
            max_tokens: DEFAULT_MAX_TOKENS,
            system: vec![],
            messages: vec![WireMessage {
                role: "user",
                content: serde_json::Value::String("go".to_string()),
            }],
            tools: vec![],
            stream: false,
        };

        let json = serde_json::to_value(&body).expect("request serialises");
        assert!(json.get("system").is_none());
    }
}

/// Wire-level tests against a mock server.
///
/// The serialisation tests above prove the structs are shaped right; these
/// prove the bytes that leave the process are, which is a different claim.
/// This client was written from the API documentation without a key, so what
/// is actually sent — headers, path, body — had no coverage at all otherwise.
#[cfg(test)]
mod wire_tests {
    use super::*;
    use mockito::Matcher;
    use serde_json::json;

    fn turns() -> Vec<ChatTurn> {
        vec![ChatTurn::system("be terse"), ChatTurn::user("hello")]
    }

    #[tokio::test]
    async fn a_completion_is_posted_to_v1_messages_with_the_documented_headers() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/messages")
            .match_header("x-api-key", "sk-test")
            .match_header("anthropic-version", ANTHROPIC_VERSION)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"content":[{"type":"text","text":"hi"}]}"#)
            .create_async()
            .await;

        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat(ChatRequest::new("claude-opus-5", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
        assert_eq!(reply.text, "hi");
    }

    #[tokio::test]
    async fn the_request_body_carries_the_model_max_tokens_and_a_cached_system_prompt() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/messages")
            .match_body(Matcher::PartialJson(json!({
                "model": "claude-opus-5",
                "max_tokens": DEFAULT_MAX_TOKENS,
                "system": [{
                    "type": "text",
                    "text": "be terse",
                    "cache_control": { "type": "ephemeral" },
                }],
                "messages": [{ "role": "user", "content": "hello" }],
            })))
            .with_status(200)
            .with_body(r#"{"content":[]}"#)
            .create_async()
            .await;

        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        client
            .chat(ChatRequest::new("claude-opus-5", turns()))
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn tools_go_out_as_input_schema() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/messages")
            .match_body(Matcher::PartialJson(json!({
                "tools": [{
                    "name": "search",
                    "description": "find things",
                    "input_schema": { "type": "object" },
                }],
            })))
            .with_status(200)
            .with_body(r#"{"content":[]}"#)
            .create_async()
            .await;

        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        client
            .chat(
                ChatRequest::new("claude-opus-5", turns()).with_tools(vec![ToolSpec {
                    name: "search".into(),
                    description: "find things".into(),
                    parameters: json!({ "type": "object" }),
                }]),
            )
            .await
            .expect("completion succeeds");

        mock.assert_async().await;
    }

    #[tokio::test]
    async fn a_tool_use_block_comes_back_as_a_structured_call() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/messages")
            .with_status(200)
            .with_body(
                r#"{"content":[
                    {"type":"text","text":"one moment"},
                    {"type":"tool_use","id":"toolu_1","name":"search","input":{"q":"x"}}
                ]}"#,
            )
            .create_async()
            .await;

        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat(ChatRequest::new("claude-opus-5", turns()))
            .await
            .expect("completion succeeds");

        assert_eq!(reply.text, "one moment");
        assert_eq!(reply.tool_calls.len(), 1);
        assert_eq!(reply.tool_calls[0].id, "toolu_1");
        assert_eq!(reply.tool_calls[0].name, "search");
        assert_eq!(reply.tool_calls[0].arguments["q"], "x");
    }

    #[tokio::test]
    async fn a_refused_key_surfaces_the_providers_own_message() {
        // The status alone does not tell an operator whether the key is wrong
        // or the model name is, and both are things only they can fix.
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/messages")
            .with_status(401)
            .with_body(r#"{"error":{"message":"invalid x-api-key"}}"#)
            .create_async()
            .await;

        let client = AnthropicChat::new("bad", Url::parse(&server.url()).unwrap());
        let error = client
            .chat(ChatRequest::new("claude-opus-5", turns()))
            .await
            .expect_err("a 401 is an error");

        let message = error.to_string();
        assert!(message.contains("401"), "got: {message}");
        assert!(message.contains("invalid x-api-key"), "got: {message}");
    }

    #[tokio::test]
    async fn a_streamed_completion_sets_stream_and_yields_each_delta() {
        let mut server = mockito::Server::new_async().await;
        let sse = concat!(
            "event: message_start\n",
            "data: {\"type\":\"message_start\"}\n",
            "\n",
            "event: content_block_delta\n",
            "data: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n",
            "\n",
            "data: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\", world\"}}\n",
            "\n",
            "data: {\"type\":\"message_stop\"}\n",
            "\n",
        );

        let mock = server
            .mock("POST", "/v1/messages")
            .match_body(Matcher::PartialJson(json!({ "stream": true })))
            .with_status(200)
            .with_header("content-type", "text/event-stream")
            .with_body(sse)
            .create_async()
            .await;

        let (tx, mut rx) = mpsc::unbounded_channel();
        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        let reply = client
            .chat_stream(ChatRequest::new("claude-opus-5", turns()), tx)
            .await
            .expect("stream succeeds");

        mock.assert_async().await;

        let mut deltas = Vec::new();
        while let Ok(delta) = rx.try_recv() {
            deltas.push(delta);
        }

        // Each delta reaches the consumer separately — the point of streaming
        // — and the returned reply is the whole text, which is what gets billed.
        assert_eq!(deltas, vec!["Hello".to_string(), ", world".to_string()]);
        assert_eq!(reply.text, "Hello, world");
    }

    #[tokio::test]
    async fn streaming_with_tools_is_refused_rather_than_dropping_the_calls() {
        let mut server = mockito::Server::new_async().await;
        // No `.expect(0)` on a mock here: the assertion is that nothing is
        // posted at all, so any request to the server is an unmatched one and
        // the request never leaves the client.
        let mock = server
            .mock("POST", "/v1/messages")
            .expect(0)
            .with_status(200)
            .create_async()
            .await;

        let (tx, _rx) = mpsc::unbounded_channel();
        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        let error = client
            .chat_stream(
                ChatRequest::new("claude-opus-5", turns()).with_tools(vec![ToolSpec {
                    name: "search".into(),
                    description: "find things".into(),
                    parameters: json!({ "type": "object" }),
                }]),
                tx,
            )
            .await
            .expect_err("a streamed turn carrying tools is refused");

        assert!(
            error.to_string().contains("not supported"),
            "the error names the gap: {error}"
        );
        mock.assert_async().await;
    }

    #[tokio::test]
    async fn a_multi_byte_character_split_across_chunks_survives() {
        // mockito sends the body in one piece, so the split is forced here by
        // driving the same reader the stream loop uses. Decoding per chunk
        // rather than per line would replace both halves of the "é" with
        // U+FFFD, and no later buffering could repair it.
        let whole = "data: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"café\"}}\n";
        let bytes = whole.as_bytes();
        let split = bytes.len() - 4; // lands inside the two-byte é

        let mut buffer: Vec<u8> = Vec::new();
        let mut text = String::new();
        for chunk in [&bytes[..split], &bytes[split..]] {
            buffer.extend_from_slice(chunk);
            while let Some(newline) = buffer.iter().position(|b| *b == b'\n') {
                let line: Vec<u8> = buffer.drain(..=newline).collect();
                let line = String::from_utf8_lossy(&line);
                if let Some(delta) = text_delta(line.trim_end()) {
                    text.push_str(&delta);
                }
            }
        }

        assert_eq!(text, "café");
        assert!(!text.contains('\u{FFFD}'), "the character was corrupted");
    }

    #[tokio::test]
    async fn a_streamed_error_status_never_reaches_the_parser() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/messages")
            .with_status(429)
            .with_body(r#"{"error":{"message":"rate limited"}}"#)
            .create_async()
            .await;

        let (tx, mut rx) = mpsc::unbounded_channel();
        let client = AnthropicChat::new("sk-test", Url::parse(&server.url()).unwrap());
        let error = client
            .chat_stream(ChatRequest::new("claude-opus-5", turns()), tx)
            .await
            .expect_err("a 429 is an error");

        assert!(error.to_string().contains("rate limited"));
        // An error body is not a stream of deltas; nothing should have been
        // pushed to a consumer that is about to be told the call failed.
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn listing_models_reads_the_data_array() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/v1/models")
            .match_header("x-api-key", "sk-test")
            .match_header("anthropic-version", ANTHROPIC_VERSION)
            .with_status(200)
            .with_body(r#"{"data":[{"id":"claude-opus-5"},{"id":"claude-sonnet-5"}]}"#)
            .create_async()
            .await;

        let models = list_models("sk-test", Url::parse(&server.url()).unwrap())
            .await
            .expect("listing succeeds");

        mock.assert_async().await;
        assert_eq!(models, vec!["claude-opus-5", "claude-sonnet-5"]);
    }

    #[tokio::test]
    async fn listing_models_reports_a_refused_key_rather_than_an_empty_list() {
        // An empty list would read as "this endpoint serves nothing", which
        // sends an operator looking in the wrong place entirely.
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(401)
            .with_body(r#"{"error":{"message":"invalid x-api-key"}}"#)
            .create_async()
            .await;

        let error = list_models("bad", Url::parse(&server.url()).unwrap())
            .await
            .expect_err("a 401 is an error");

        assert!(error.to_string().contains("401"));
        assert!(error.to_string().contains("invalid x-api-key"));
    }
}
