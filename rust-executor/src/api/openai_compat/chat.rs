//! `POST /v1/chat/completions` and `POST /v1/completions`.
//!
//! Both endpoints translate an OpenAI request into an ephemeral
//! `AIService::prompt_messages{,_stream}` call.  No DB-backed task is
//! created; the model thread spawns the task in-memory for the duration
//! of the call.
//!
//! ## Tool / function calling
//!
//! When the request carries `tools`, we inject the Hermes/Qwen
//! `<tools>…</tools>` block as a system message and, for
//! `tool_choice: "required"` or a named function, hand the local model a
//! grammar constraint (see [`super::tool_grammar`]) so decoding is forced
//! onto a well-formed `<tool_call>` block.  `auto`/`none` generate freely;
//! any tool calls are recovered from the text afterwards.  Assistant
//! `tool_calls` and `role:"tool"` results are folded back into prompt text
//! because the local chat template has no tool role.

use std::convert::Infallible;
use std::time::SystemTime;

use axum::{
    response::{sse::Event, IntoResponse, Sse},
    Json,
};
use futures::Stream;
use kalosm::language::ArcParser;
use uuid::Uuid;

use super::errors::{OpenAIError, OpenAIJson, OpenAIResult};
use super::model_selector::resolve_model;
use super::tool_grammar::{self, ExtractedToolCall, ToolChoice};
use super::types::{
    ChatChoice, ChatChunkChoice, ChatChunkDelta, ChatCompletionChunk, ChatCompletionRequest,
    ChatCompletionResponse, ChatMessage, ChatMessageContent, ChatResponseMessage, CompletionChoice,
    CompletionRequest, CompletionResponse, FunctionCall, FunctionCallDelta, Role, ToolCall,
    ToolCallDelta, ToolDef, Usage,
};
use crate::agent::capabilities::{check_capability, AI_PROMPT_CAPABILITY};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::billing::check_compute_credits;
use crate::types::ModelType;

/// `POST /v1/chat/completions` — handles both streaming (`stream: true`)
/// and non-streaming responses.
pub async fn chat_completions(
    auth: AuthContext,
    OpenAIJson(req): OpenAIJson<ChatCompletionRequest>,
) -> Result<axum::response::Response, OpenAIError> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

    // Resolve the OpenAI `model` string to an AD4M model_id.
    let model_id = resolve_model(&req.model, ModelType::Llm).await?;

    let tools: Vec<ToolDef> = req.tools.clone().unwrap_or_default();
    let has_tools = !tools.is_empty();
    let choice = tool_grammar::parse_tool_choice(&req.tool_choice, has_tools);
    let parallel = req.parallel_tool_calls.unwrap_or(true);
    // Tools are "active" (rendered + potentially constrained + parsed out)
    // unless the caller explicitly disabled them with tool_choice: "none".
    let tools_active = has_tools && choice != ToolChoice::None;

    // Assemble (role, content) pairs.  Fold assistant tool calls and
    // `role:"tool"` results into text, and prepend the tools system prompt.
    let mut messages: Vec<(String, String)> = Vec::with_capacity(req.messages.len() + 1);
    if tools_active {
        messages.push((
            "system".to_string(),
            tool_grammar::render_tools_system_prompt(&tools),
        ));
    }
    for m in &req.messages {
        messages.push(flatten_message(m));
    }

    // Constrain decoding for required / named choices (guarantees a
    // well-formed call).  Auto/None return `None` and generate freely.
    let constraint = if tools_active {
        tool_grammar::build_tool_call_parser(&tools, &choice, parallel)
    } else {
        None
    };

    if req.stream {
        chat_stream(
            auth,
            req.model.clone(),
            model_id,
            messages,
            constraint,
            tools_active,
        )
        .await
    } else {
        chat_oneshot(
            auth,
            req.model.clone(),
            model_id,
            messages,
            constraint,
            tools_active,
        )
        .await
    }
}

/// `POST /v1/completions` (legacy text-completion).  Treats `prompt` as a
/// single user message with no system prompt.  Tools are not supported on
/// the legacy endpoint.
pub async fn completions(
    auth: AuthContext,
    OpenAIJson(req): OpenAIJson<CompletionRequest>,
) -> OpenAIResult<Json<CompletionResponse>> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

    let model_id = resolve_model(&req.model, ModelType::Llm).await?;
    let prompt = req
        .prompt
        .into_single()
        .map_err(OpenAIError::invalid_request)?;
    let messages = vec![("user".to_string(), prompt)];

    if let Some(email) = user_email(&auth) {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let result = service
        .prompt_messages(model_id, messages, Some(auth.auth_token.clone()), None)
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    // Billing is now inside AIService::prompt_messages via bill_ai_operation
    // (host_rates-based, shared with WS-RPC path). No handler-level bill_compute here.

    Ok(Json(CompletionResponse {
        id: format!("cmpl-{}", Uuid::new_v4()),
        object: "text_completion",
        created: epoch_seconds(),
        model: req.model,
        choices: vec![CompletionChoice {
            index: 0,
            text: result.text,
            finish_reason: "stop",
        }],
        usage: Usage {
            prompt_tokens: result.prompt_tokens as u64,
            completion_tokens: result.completion_tokens as u64,
            total_tokens: (result.prompt_tokens + result.completion_tokens) as u64,
        },
    }))
}

async fn chat_oneshot(
    auth: AuthContext,
    requested_model: String,
    model_id: String,
    messages: Vec<(String, String)>,
    constraint: Option<ArcParser<()>>,
    tools_active: bool,
) -> Result<axum::response::Response, OpenAIError> {
    // NOTE: WS-RPC `ai.prompt` on dev does not bill today (only pre-checks).
    // /v1 billing is correct for the public API surface; the WS-RPC gap
    // should be aligned in a separate PR against dev.
    if let Some(email) = user_email(&auth) {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let result = service
        .prompt_messages(model_id, messages, Some(auth.auth_token.clone()), constraint)
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    // Billing is now inside AIService::prompt_messages via bill_ai_operation.

    let tool_calls = if tools_active {
        to_openai_tool_calls(tool_grammar::extract_tool_calls(&result.text))
    } else {
        Vec::new()
    };

    let (message, finish_reason) = if tool_calls.is_empty() {
        (
            ChatResponseMessage {
                role: "assistant",
                content: Some(result.text),
                tool_calls: None,
            },
            "stop",
        )
    } else {
        (
            ChatResponseMessage {
                role: "assistant",
                content: None,
                tool_calls: Some(tool_calls),
            },
            "tool_calls",
        )
    };

    let body = ChatCompletionResponse {
        id: format!("chatcmpl-{}", Uuid::new_v4()),
        object: "chat.completion",
        created: epoch_seconds(),
        model: requested_model,
        choices: vec![ChatChoice {
            index: 0,
            message,
            finish_reason,
        }],
        usage: Usage {
            prompt_tokens: result.prompt_tokens as u64,
            completion_tokens: result.completion_tokens as u64,
            total_tokens: (result.prompt_tokens + result.completion_tokens) as u64,
        },
    };
    Ok(Json(body).into_response())
}

async fn chat_stream(
    auth: AuthContext,
    requested_model: String,
    model_id: String,
    messages: Vec<(String, String)>,
    constraint: Option<ArcParser<()>>,
    tools_active: bool,
) -> Result<axum::response::Response, OpenAIError> {
    if let Some(email) = user_email(&auth) {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let (token_rx, done_rx) = service
        .prompt_messages_stream(model_id, messages, Some(auth.auth_token.clone()), constraint)
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    let id = format!("chatcmpl-{}", Uuid::new_v4());
    let created = epoch_seconds();
    let stream_model = requested_model.clone();

    // We construct the SSE stream by spawning a forwarder task that
    // drains tokens from the LLM thread + emits one `Event` per chunk
    // into a bounded channel.  axum's `Sse` consumes the resulting
    // receiver.  This avoids pulling in `async-stream` for the sole
    // sake of one `yield`-style generator.
    let (event_tx, event_rx) = tokio::sync::mpsc::unbounded_channel::<Result<Event, Infallible>>();
    let auth_clone = auth.clone();

    // Initial role event.
    let role_chunk = ChatCompletionChunk {
        id: id.clone(),
        object: "chat.completion.chunk",
        created,
        model: stream_model.clone(),
        choices: vec![ChatChunkChoice {
            index: 0,
            delta: ChatChunkDelta {
                role: Some("assistant"),
                content: None,
                tool_calls: None,
            },
            finish_reason: None,
        }],
    };
    let _ = event_tx.send(Ok(
        Event::default().data(serde_json::to_string(&role_chunk).unwrap())
    ));

    tokio::spawn({
        let event_tx = event_tx.clone();
        let stream_model = stream_model.clone();
        let id = id.clone();
        async move {
            let mut token_rx = token_rx;

            if tools_active {
                // Tool-enabled requests buffer the full generation, then emit
                // tool-call framing at the end.  Constrained decoding streams
                // the on-grammar `<tool_call>` text token-by-token, but we
                // can only translate it into OpenAI `tool_calls[]` deltas once
                // whole; likewise auto-mode output must be inspected as a
                // whole to avoid leaking raw `<tool_call>` tags as content.
                // This trades token streaming for correct tool structuring.
                let mut accumulated = String::new();
                while let Some(token) = token_rx.recv().await {
                    accumulated.push_str(&token);
                }

                let tool_calls =
                    to_openai_tool_calls(tool_grammar::extract_tool_calls(&accumulated));

                if !tool_calls.is_empty() {
                    for (i, call) in tool_calls.into_iter().enumerate() {
                        let chunk = ChatCompletionChunk {
                            id: id.clone(),
                            object: "chat.completion.chunk",
                            created,
                            model: stream_model.clone(),
                            choices: vec![ChatChunkChoice {
                                index: 0,
                                delta: ChatChunkDelta {
                                    role: None,
                                    content: None,
                                    tool_calls: Some(vec![ToolCallDelta {
                                        index: i as u32,
                                        id: Some(call.id),
                                        kind: Some("function"),
                                        function: Some(FunctionCallDelta {
                                            name: Some(call.function.name),
                                            arguments: Some(call.function.arguments),
                                        }),
                                    }]),
                                },
                                finish_reason: None,
                            }],
                        };
                        if event_tx
                            .send(Ok(
                                Event::default().data(serde_json::to_string(&chunk).unwrap())
                            ))
                            .is_err()
                        {
                            return;
                        }
                    }
                    emit_final(&event_tx, &id, &stream_model, created, "tool_calls");
                } else {
                    // Model answered normally — emit the buffered text as one
                    // content delta, then a normal stop.
                    if !accumulated.is_empty() {
                        let chunk = ChatCompletionChunk {
                            id: id.clone(),
                            object: "chat.completion.chunk",
                            created,
                            model: stream_model.clone(),
                            choices: vec![ChatChunkChoice {
                                index: 0,
                                delta: ChatChunkDelta {
                                    role: None,
                                    content: Some(accumulated),
                                    tool_calls: None,
                                },
                                finish_reason: None,
                            }],
                        };
                        let _ = event_tx.send(Ok(
                            Event::default().data(serde_json::to_string(&chunk).unwrap())
                        ));
                    }
                    emit_final(&event_tx, &id, &stream_model, created, "stop");
                }
            } else {
                // Unchanged per-token streaming.
                while let Some(token) = token_rx.recv().await {
                    let chunk = ChatCompletionChunk {
                        id: id.clone(),
                        object: "chat.completion.chunk",
                        created,
                        model: stream_model.clone(),
                        choices: vec![ChatChunkChoice {
                            index: 0,
                            delta: ChatChunkDelta {
                                role: None,
                                content: Some(token),
                                tool_calls: None,
                            },
                            finish_reason: None,
                        }],
                    };
                    if event_tx
                        .send(Ok(
                            Event::default().data(serde_json::to_string(&chunk).unwrap())
                        ))
                        .is_err()
                    {
                        return;
                    }
                }
                emit_final(&event_tx, &id, &stream_model, created, "stop");
            }

            // Billing lives in AIService::prompt_messages_stream now,
            // charged when done_rx resolves with the final PromptResult
            // (tokens known then). Handler used to double-bill here; that
            // path is gone. Just wait for the LLM to signal completion.
            let _ = done_rx.await;

            // OpenAI SSE terminator.
            let _ = event_tx.send(Ok(Event::default().data("[DONE]")));
        }
    });

    let stream = tokio_stream::wrappers::UnboundedReceiverStream::new(event_rx);
    Ok(Sse::new(stream).into_response())
}

/// Emit the final SSE chunk carrying an empty delta and `finish_reason`.
fn emit_final(
    event_tx: &tokio::sync::mpsc::UnboundedSender<Result<Event, Infallible>>,
    id: &str,
    model: &str,
    created: i64,
    finish_reason: &'static str,
) {
    let final_chunk = ChatCompletionChunk {
        id: id.to_string(),
        object: "chat.completion.chunk",
        created,
        model: model.to_string(),
        choices: vec![ChatChunkChoice {
            index: 0,
            delta: ChatChunkDelta::default(),
            finish_reason: Some(finish_reason),
        }],
    };
    let _ = event_tx.send(Ok(
        Event::default().data(serde_json::to_string(&final_chunk).unwrap())
    ));
}

/// Convert recovered tool calls into OpenAI response `tool_calls[]`,
/// minting a stable `call_…` id for each.
fn to_openai_tool_calls(extracted: Vec<ExtractedToolCall>) -> Vec<ToolCall> {
    extracted
        .into_iter()
        .map(|c| ToolCall {
            id: format!("call_{}", Uuid::new_v4()),
            kind: "function".to_string(),
            function: FunctionCall {
                name: c.name,
                arguments: c.arguments,
            },
        })
        .collect()
}

/// Flatten one inbound message to a `(role, text)` pair.  Assistant tool
/// calls and `role:"tool"` results are rendered into text using the Qwen
/// `<tool_call>` / `<tool_response>` convention, since the local chat
/// template has no tool role.
fn flatten_message(m: &ChatMessage) -> (String, String) {
    let base_text = m
        .content
        .as_ref()
        .map(ChatMessageContent::flatten_to_text)
        .unwrap_or_default();

    match m.role {
        // Tool result → a `<tool_response>` block in a user turn.
        Role::Tool => (
            "user".to_string(),
            format!("<tool_response>\n{}\n</tool_response>", base_text),
        ),
        // Assistant turn that called tools → re-render the calls so the model
        // sees its own prior invocations.
        Role::Assistant if m.tool_calls.as_ref().map_or(false, |c| !c.is_empty()) => {
            let mut text = base_text;
            if let Some(calls) = &m.tool_calls {
                for call in calls {
                    if !text.is_empty() {
                        text.push('\n');
                    }
                    let args = call.function.arguments.trim();
                    let args = if args.is_empty() { "{}" } else { args };
                    text.push_str(&format!(
                        "<tool_call>\n{{\"name\": \"{}\", \"arguments\": {}}}\n</tool_call>",
                        call.function.name, args
                    ));
                }
            }
            ("assistant".to_string(), text)
        }
        _ => (role_to_str(&m.role).to_string(), base_text),
    }
}

fn role_to_str(role: &Role) -> &'static str {
    match role {
        Role::System => "system",
        Role::User => "user",
        Role::Assistant => "assistant",
        Role::Tool => "tool",
        Role::Function => "function",
        Role::Developer => "developer",
    }
}

fn epoch_seconds() -> i64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

fn user_email(auth: &AuthContext) -> Option<String> {
    crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
}

// Re-export a Stream alias for documentation purposes.
#[allow(dead_code)]
type EventStream = dyn Stream<Item = Result<Event, Infallible>> + Send;
