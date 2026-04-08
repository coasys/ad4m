//! AI REST endpoints: /api/v1/ai/*
//!
//! Endpoints for model management, tasks, prompts, and embeddings.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::pubsub::mark_credits_dirty;
use crate::types::{AITask, AITaskInput, Model, ModelInput, ModelType, VoiceActivityParamsInput};
use base64::Engine;
use serde::Deserialize;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

// Default pricing
const DEFAULT_TOKEN_RATE: f64 = 12.5;
const DEFAULT_EMBEDDING_TOKEN_RATE: f64 = 0.1;

/// Read-only credit check.
// TODO: implement proper credit pre-estimation — currently only rejects at zero balance,
// allowing calls that exceed remaining credits. This is pre-existing behaviour from GraphQL.
fn check_compute_credits(auth_token: &str) -> Result<(), ApiError> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            if credits <= 0.0 {
                return Err(ApiError::Forbidden("Insufficient compute credits".into()));
            }
        }
    }
    Ok(())
}

fn reserve_compute_credits(auth_token: &str, amount: f64) -> Result<(), ApiError> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        if !free {
            Ad4mDb::with_global_instance(|db| db.deduct_user_credits_if_available(email, amount))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            mark_credits_dirty(email);
        }
    }
    Ok(())
}

fn get_rate(description: &str, default: f64) -> Result<f64, ApiError> {
    match Ad4mDb::with_global_instance(|db| db.get_host_rate(description)) {
        Ok(Some(rate)) => Ok(rate),
        Ok(None) => Ok(default),
        Err(e) => Err(ApiError::Internal(format!(
            "Failed to read host rate: {}",
            e
        ))),
    }
}

/// GET /ai/models — list all models
pub async fn list_models(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(models))
}

/// POST /ai/models — add model
pub async fn add_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ModelInput>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let id = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .add_model(body)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(id))
}

/// PUT /ai/models/:id — update model
pub async fn update_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<ModelInput>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .update_model(id, body)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// DELETE /ai/models/:id — remove model
pub async fn remove_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .remove_model(id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /ai/models/:id/default — set default model for a model type
pub async fn set_default_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<SetDefaultModelRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Verify model exists
    let maybe_model = Ad4mDb::with_global_instance(|db| db.get_model(id.clone()))
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    if maybe_model.is_none() {
        return Err(ApiError::NotFound(format!("Model not found: {}", id)));
    }

    AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .set_default_model(body.model_type, id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /ai/tasks — list tasks
pub async fn list_tasks(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<AITask>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let tasks = AIService::get_tasks().map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(tasks))
}

/// POST /ai/tasks — add task
pub async fn add_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<AITaskInput>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let task = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .add_task(body)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(task))
}

/// PUT /ai/tasks/:id — update task
pub async fn update_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<AITaskInput>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let mut task: AITask = body.into();
    task.task_id = id;
    let result = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .update_task(task)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// DELETE /ai/tasks/:id — remove task
pub async fn remove_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let task = AIService::get_tasks()
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .into_iter()
        .find(|t| t.task_id == id)
        .ok_or_else(|| ApiError::NotFound(format!("Task not found: {}", id)))?;

    AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .delete_task(id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(task))
}

/// POST /ai/prompt — send prompt
pub async fn ai_prompt(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PromptRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    check_compute_credits(&context.auth_token)?;

    let result = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .prompt(body.task_id, body.prompt)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let total_tokens = result.prompt_tokens + result.completion_tokens;
    let model_name = match Ad4mDb::with_global_instance(|db| db.get_model(result.model_id.clone()))
    {
        Ok(Some(m)) => m.name,
        _ => String::new(),
    };
    if let Err(e) = reserve_compute_credits(
        &context.auth_token,
        total_tokens as f64 * get_rate(&model_name, DEFAULT_TOKEN_RATE)?,
    ) {
        log::warn!(
            "Call exceeded compute credits (ai_prompt, model={}, tokens={}): {:?}",
            model_name,
            total_tokens,
            e
        );
    }

    Ok(Json(result.text))
}

/// POST /ai/embed — generate embeddings
pub async fn ai_embed(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<EmbedRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    check_compute_credits(&context.auth_token)?;

    let result = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .embed(body.model_id, body.text)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    if let Err(e) = reserve_compute_credits(
        &context.auth_token,
        result.token_count as f64 * get_rate("embedding per token", DEFAULT_EMBEDDING_TOKEN_RATE)?,
    ) {
        log::warn!(
            "Call exceeded compute credits (ai_embed, tokens={}): {:?}",
            result.token_count,
            e
        );
    }

    let json_string = serde_json::to_string(&result.embeddings)
        .map_err(|e| ApiError::Internal(format!("Failed to serialize vector: {}", e)))?;

    // Compress with zlib
    let compressed_bytes = deflate::deflate_bytes_zlib(json_string.as_bytes());
    Ok(Json(
        base64::prelude::BASE64_STANDARD.encode(&compressed_bytes),
    ))
}

// ── Transcription endpoints ──

/// Check whether billing is active for the given auth token.
fn is_billing_active(auth_token: &str) -> bool {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
        if global_free {
            return false;
        }
        let free =
            Ad4mDb::with_global_instance(|db| db.get_user_free_access(email)).unwrap_or(false);
        !free
    } else {
        false
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OpenTranscriptionRequest {
    pub model_id: String,
    pub params: Option<VoiceActivityParamsInput>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FeedTranscriptionRequest {
    pub stream_ids: Vec<String>,
    pub audio: Vec<f64>, // f64 for JSON compat, cast to f32
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CloseTranscriptionRequest {
    pub stream_id: String,
}

/// POST /ai/transcription/open — open a Whisper transcription stream
pub async fn open_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<OpenTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    // When billing is active, verify a rate is configured for this model
    if is_billing_active(&context.auth_token) {
        let rate_key = Ad4mDb::with_global_instance(|db| db.get_model(body.model_id.clone()))
            .ok()
            .flatten()
            .map(|m| m.name)
            .unwrap_or_else(|| body.model_id.clone());
        let has_rate = Ad4mDb::with_global_instance(|db| db.get_host_rate(&rate_key))
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        if has_rate.is_none() {
            return Err(ApiError::BadRequest(format!(
                "No host rate configured for '{}' — cannot open transcription stream",
                rate_key
            )));
        }
    }

    let stream_id = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .open_transcription_stream(
            body.model_id,
            body.params.map(|p| p.into()),
            context.auth_token.clone(),
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(stream_id))
}

/// POST /ai/transcription/feed — feed audio samples to transcription streams
pub async fn feed_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<FeedTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let audio_f32: Vec<f32> = body.audio.into_iter().map(|x| x as f32).collect();
    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    for stream_id in &body.stream_ids {
        if let Err(e) = service
            .feed_transcription_stream(stream_id, audio_f32.clone(), &context.auth_token)
            .await
        {
            log::warn!("Error feeding stream {}: {}", stream_id, e);
        }
    }

    Ok(Json("true".to_string()))
}

/// POST /ai/transcription/close — close a transcription stream
pub async fn close_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CloseTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .close_transcription_stream(&body.stream_id, &context.auth_token)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json("true".to_string()))
}
