//! `POST /v1/chat/completions` and `POST /v1/completions`.
//!
//! Both endpoints translate an OpenAI request into an ephemeral
//! `AIService::prompt_messages{,_stream}` call.  No DB-backed task is
//! created; the model thread spawns the task in-memory for the duration
//! of the call.

use std::convert::Infallible;
use std::time::SystemTime;

use axum::{
    response::{sse::Event, IntoResponse, Sse},
    Json,
};
use futures::Stream;
use uuid::Uuid;

use super::errors::{OpenAIError, OpenAIResult};
use super::model_selector::resolve_model;
use super::types::{
    ChatChoice, ChatChunkChoice, ChatChunkDelta, ChatCompletionChunk, ChatCompletionRequest,
    ChatCompletionResponse, ChatResponseMessage, CompletionChoice, CompletionRequest,
    CompletionResponse, Role, Usage,
};
use crate::agent::capabilities::{check_capability, AI_PROMPT_CAPABILITY};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::billing::{bill_compute, BillingError};
use crate::types::ModelType;

/// `POST /v1/chat/completions` — handles both streaming (`stream: true`)
/// and non-streaming responses.
pub async fn chat_completions(
    auth: AuthContext,
    Json(req): Json<ChatCompletionRequest>,
) -> Result<axum::response::Response, OpenAIError> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

    // Resolve the OpenAI `model` string to an AD4M model_id.
    let model_id = resolve_model(&req.model, ModelType::Llm).await?;

    // Flatten messages to (role, content) pairs.
    let messages: Vec<(String, String)> = req
        .messages
        .iter()
        .map(|m| {
            (
                role_to_str(&m.role).to_string(),
                m.content.flatten_to_text(),
            )
        })
        .collect();

    if req.stream {
        chat_stream(auth, req.model.clone(), model_id, messages).await
    } else {
        chat_oneshot(auth, req.model.clone(), model_id, messages).await
    }
}

/// `POST /v1/completions` (legacy text-completion).  Treats `prompt` as a
/// single user message with no system prompt.
pub async fn completions(
    auth: AuthContext,
    Json(req): Json<CompletionRequest>,
) -> OpenAIResult<Json<CompletionResponse>> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

    let model_id = resolve_model(&req.model, ModelType::Llm).await?;
    let messages = vec![("user".to_string(), req.prompt.first())];

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let result = service
        .prompt_messages(model_id, messages)
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    if let Some(email) = user_email(&auth) {
        if let Err(BillingError::InsufficientCredits) =
            bill_compute(&email, 1.0, "ai_prompt", Some("v1/completions"))
        {
            return Err(OpenAIError::insufficient_quota(
                "Insufficient compute credits",
            ));
        }
    }

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
) -> Result<axum::response::Response, OpenAIError> {
    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let result = service
        .prompt_messages(model_id, messages)
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    if let Some(email) = user_email(&auth) {
        if let Err(BillingError::InsufficientCredits) =
            bill_compute(&email, 1.0, "ai_prompt", Some("v1/chat/completions"))
        {
            return Err(OpenAIError::insufficient_quota(
                "Insufficient compute credits",
            ));
        }
    }

    let body = ChatCompletionResponse {
        id: format!("chatcmpl-{}", Uuid::new_v4()),
        object: "chat.completion",
        created: epoch_seconds(),
        model: requested_model,
        choices: vec![ChatChoice {
            index: 0,
            message: ChatResponseMessage {
                role: "assistant",
                content: result.text,
            },
            finish_reason: "stop",
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
) -> Result<axum::response::Response, OpenAIError> {
    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let (token_rx, done_rx) = service
        .prompt_messages_stream(model_id, messages)
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

            // Final event with finish_reason.
            let final_chunk = ChatCompletionChunk {
                id: id.clone(),
                object: "chat.completion.chunk",
                created,
                model: stream_model.clone(),
                choices: vec![ChatChunkChoice {
                    index: 0,
                    delta: ChatChunkDelta::default(),
                    finish_reason: Some("stop"),
                }],
            };
            let _ = event_tx.send(Ok(
                Event::default().data(serde_json::to_string(&final_chunk).unwrap())
            ));

            // Billing — best-effort, charged once per completed stream.
            // Per-token billing requires tokenizer-exact counts which the
            // Kalosm backend doesn't expose today; a flat charge matches
            // the non-stream `chat.completions` policy.
            if let Some(email) = user_email(&auth_clone) {
                let _ = bill_compute(
                    &email,
                    1.0,
                    "ai_prompt",
                    Some("v1/chat/completions[stream]"),
                );
            }
            let _ = done_rx.await;

            // OpenAI SSE terminator.
            let _ = event_tx.send(Ok(Event::default().data("[DONE]")));
        }
    });

    let stream = tokio_stream::wrappers::UnboundedReceiverStream::new(event_rx);
    Ok(Sse::new(stream).into_response())
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
