//! `/v1/chat/completions` for a model whose provider carries tools as data.
//!
//! [`super::chat`] renders tools into a `<tools>` system prompt and recovers
//! calls from the reply text, which is the only option for a local model. A
//! provider with native tool calling (see
//! [`crate::ai_service::providers::api_type_supports_native_tools`]) is handed
//! the definitions instead and answers with structured calls, through
//! [`AIService::prompt_with_tools`] — the path the interpretation harness
//! already takes via [`super::harness_bridge`].
//!
//! The conversation goes through the bridge's `structured_turns`, not a second
//! mapping of its own: a provider refuses a `tool_use` or `tool_result` left
//! unpaired, and that fold is where pairing is repaired.
//!
//! Streaming is not token-level here. Providers refuse to stream with tools
//! (`RemoteChat::chat_stream`), so a `stream: true` request is answered in
//! full and framed as SSE afterwards — which is also what the injected path
//! does whenever tools are active, so a client sees no difference in shape.
//!
//! `tool_choice: "required"` and a named function are not enforced on this
//! path. They are not enforced for a remote model on the injected path either:
//! the decoding constraint that implements them only reaches local models.
//! `parallel_tool_calls: false` is not passed on either, for the same reason:
//! it reaches [`super::tool_grammar::build_tool_call_parser`] on the injected
//! path, and that constraint binds only for [`crate::ai_service::LlmModel::Local`].
//! Wiring either one through means a provider-level request field, not a change
//! here.

use std::convert::Infallible;

use axum::response::{sse::Event, IntoResponse, Json, Sse};
use serde_json::{json, Value};
use uuid::Uuid;

use super::chat::{epoch_seconds, user_email};
use super::errors::OpenAIError;
use super::harness_bridge::structured_turns;
use super::types::{
    ChatChoice, ChatChunkChoice, ChatChunkDelta, ChatCompletionChunk, ChatCompletionResponse,
    ChatMessage, ChatMessageContent, ChatResponseMessage, FunctionCall, FunctionCallDelta, Role,
    ToolCall, ToolCallDelta, ToolDef, Usage,
};
use crate::ai_service::providers::{ChatReply, ChatTurn, ChatUsage, ToolSpec};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::billing::check_compute_credits;

/// Answer a tool-carrying chat request through the model's native tool calling.
pub async fn chat_with_native_tools(
    auth: AuthContext,
    requested_model: String,
    model_id: String,
    messages: &[ChatMessage],
    tools: &[ToolDef],
    stream: bool,
) -> Result<axum::response::Response, OpenAIError> {
    if let Some(email) = user_email(&auth) {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    if let Some(name) = duplicate_tool_name(tools) {
        return Err(OpenAIError::invalid_request(format!(
            "`tools` names `{name}` twice"
        )));
    }

    let turns = to_turns(messages).map_err(|e| OpenAIError::invalid_request(e.to_string()))?;
    let specs = to_specs(tools);

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    // Billing happens inside prompt_with_tools, on success only.
    let reply = service
        .prompt_with_tools(model_id, turns, specs, Some(auth.auth_token.clone()))
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    let id = format!("chatcmpl-{}", Uuid::new_v4());
    let created = epoch_seconds();

    if stream {
        let events: Vec<Result<Event, Infallible>> =
            reply_chunks(&reply, &id, &requested_model, created)
                .iter()
                .map(|chunk| Ok(Event::default().data(serde_json::to_string(chunk).unwrap())))
                .chain(std::iter::once(Ok(Event::default().data("[DONE]"))))
                .collect();
        return Ok(Sse::new(futures::stream::iter(events)).into_response());
    }

    let usage = usage_of(&reply.usage);
    let (message, finish_reason) = response_message(reply);
    Ok(Json(ChatCompletionResponse {
        id,
        object: "chat.completion",
        created,
        model: requested_model,
        choices: vec![ChatChoice {
            index: 0,
            message,
            finish_reason,
        }],
        usage,
    })
    .into_response())
}

/// The request's messages as provider turns, calls and results paired.
///
/// Each message is put in the shape the harness builds — `role`, text
/// `content`, OpenAI `tool_calls`, `tool_call_id` — so it runs through the same
/// fold rather than a copy of it.
///
/// A turn the fold leaves blank is then dropped. A provider refuses an empty
/// message the way it refuses an unpaired call, and this is the first path that
/// can produce one: the harness writes its own messages and never emits an
/// empty one, but `{"role": "assistant", "content": null}` with no `tool_calls`
/// is legal on the OpenAI wire and clients send it as a placeholder. The
/// injected path swallowed it as empty prose. A turn that carries calls or
/// answers one is kept whatever its text, because dropping it is the
/// unbalanced conversation `structured_turns` exists to prevent.
pub(super) fn to_turns(messages: &[ChatMessage]) -> anyhow::Result<Vec<ChatTurn>> {
    let values: Vec<Value> = messages.iter().map(message_value).collect();
    let mut turns = structured_turns(&values)?;
    turns.retain(|t| {
        !t.content.trim().is_empty() || !t.tool_calls.is_empty() || t.tool_result_for.is_some()
    });
    if turns.is_empty() {
        anyhow::bail!("no message carries content");
    }
    Ok(turns)
}

fn message_value(m: &ChatMessage) -> Value {
    let role = match m.role {
        Role::System | Role::Developer => "system",
        Role::User => "user",
        Role::Assistant => "assistant",
        Role::Tool | Role::Function => "tool",
    };
    let mut value = json!({
        "role": role,
        "content": m.content.as_ref().map(ChatMessageContent::flatten_to_text).unwrap_or_default(),
    });
    if let Some(calls) = &m.tool_calls {
        value["tool_calls"] = json!(calls);
    }
    if let Some(id) = &m.tool_call_id {
        value["tool_call_id"] = json!(id);
    }
    value
}

pub(super) fn to_specs(tools: &[ToolDef]) -> Vec<ToolSpec> {
    tools
        .iter()
        .map(|tool| ToolSpec {
            name: tool.function.name.clone(),
            description: tool.function.description.clone().unwrap_or_default(),
            // A function with no parameters still needs an object schema on the
            // wire; Anthropic rejects a tool whose `input_schema` is missing.
            // `parameters: []` or `""` is rejected for the same reason, and the
            // caller writes that field, so anything that is not an object is
            // replaced here rather than sent and refused.
            parameters: tool
                .function
                .parameters
                .clone()
                .filter(Value::is_object)
                .unwrap_or_else(|| json!({ "type": "object", "properties": {} })),
        })
        .collect()
}

/// The first name `tools[]` gives twice, if any.
///
/// A provider refuses a request with two tools of one name, and the caller
/// wrote both, so this is a bad request rather than an executor fault. Keeping
/// only one of them would be a guess about which the model should call.
pub(super) fn duplicate_tool_name(tools: &[ToolDef]) -> Option<&str> {
    let mut seen = std::collections::HashSet::new();
    tools
        .iter()
        .map(|tool| tool.function.name.as_str())
        .find(|name| !seen.insert(*name))
}

/// The reply as an OpenAI assistant message, keeping text written alongside
/// calls — unlike the injected path, which has no text to keep once the call
/// blocks are parsed out.
pub(super) fn response_message(reply: ChatReply) -> (ChatResponseMessage, &'static str) {
    let finish_reason = if reply.tool_calls.is_empty() {
        "stop"
    } else {
        "tool_calls"
    };
    let content = if reply.text.is_empty() && !reply.tool_calls.is_empty() {
        None
    } else {
        Some(reply.text)
    };
    let tool_calls = if reply.tool_calls.is_empty() {
        None
    } else {
        Some(reply.tool_calls.into_iter().map(wire_call).collect())
    };
    (
        ChatResponseMessage {
            role: "assistant",
            content,
            tool_calls,
        },
        finish_reason,
    )
}

/// The reply as the chunks of an SSE stream: role, text, one chunk per call,
/// then the finish reason.
pub(super) fn reply_chunks(
    reply: &ChatReply,
    id: &str,
    model: &str,
    created: i64,
) -> Vec<ChatCompletionChunk> {
    let chunk = |delta: ChatChunkDelta, finish_reason: Option<&'static str>| ChatCompletionChunk {
        id: id.to_string(),
        object: "chat.completion.chunk",
        created,
        model: model.to_string(),
        choices: vec![ChatChunkChoice {
            index: 0,
            delta,
            finish_reason,
        }],
    };

    let mut chunks = vec![chunk(
        ChatChunkDelta {
            role: Some("assistant"),
            ..Default::default()
        },
        None,
    )];
    if !reply.text.is_empty() {
        chunks.push(chunk(
            ChatChunkDelta {
                content: Some(reply.text.clone()),
                ..Default::default()
            },
            None,
        ));
    }
    for (index, call) in reply.tool_calls.iter().cloned().enumerate() {
        let call = wire_call(call);
        chunks.push(chunk(
            ChatChunkDelta {
                tool_calls: Some(vec![ToolCallDelta {
                    index: index as u32,
                    id: Some(call.id),
                    kind: Some("function"),
                    function: Some(FunctionCallDelta {
                        name: Some(call.function.name),
                        arguments: Some(call.function.arguments),
                    }),
                }]),
                ..Default::default()
            },
            None,
        ));
    }
    let finish_reason = if reply.tool_calls.is_empty() {
        "stop"
    } else {
        "tool_calls"
    };
    chunks.push(chunk(ChatChunkDelta::default(), Some(finish_reason)));
    chunks
}

fn wire_call(call: crate::ai_service::providers::ToolCall) -> ToolCall {
    ToolCall {
        id: call.id,
        kind: "function".to_string(),
        function: FunctionCall {
            name: call.name,
            // The wire carries arguments as a JSON string; providers parse it.
            arguments: call.arguments.to_string(),
        },
    }
}

/// The provider's own token counts, which are exact.
///
/// Anthropic reports cached input apart from `input_tokens`; OpenAI's
/// `prompt_tokens` includes it, so the three are summed. A provider that
/// reported nothing reads as zero.
pub(super) fn usage_of(usage: &ChatUsage) -> Usage {
    let prompt_tokens = usage.input_tokens.unwrap_or(0)
        + usage.cache_read_tokens.unwrap_or(0)
        + usage.cache_write_tokens.unwrap_or(0);
    let completion_tokens = usage.output_tokens.unwrap_or(0);
    Usage {
        prompt_tokens,
        completion_tokens,
        total_tokens: prompt_tokens + completion_tokens,
    }
}
