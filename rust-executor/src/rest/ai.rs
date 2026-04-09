//! AI REST endpoints: /api/v1/ai/*
//!
//! Endpoints for model management, tasks, prompts, embeddings, and transcription.

use axum::{
    extract::{Path, Query, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::pubsub::mark_credits_dirty;
use crate::types::{AITask, AITaskInput, Model, ModelInput, ModelType, VoiceActivityParamsInput};
use base64::Engine;
use serde::Deserialize;
use std::collections::HashMap;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

// Default pricing
const DEFAULT_TOKEN_RATE: f64 = 12.5;
const DEFAULT_EMBEDDING_TOKEN_RATE: f64 = 0.1;

/// Read-only credit check.
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

// ── Models ──

/// GET /ai/models
pub async fn list_models(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_QUERY_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(models))
}

/// POST /ai/models
pub async fn add_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model: ModelInput = serde_json::from_value(body["model"].clone())
        .map_err(|e| ApiError::BadRequest(e.to_string()))?;

    let id = Ad4mDb::with_global_instance(|db| db.add_model(model))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(id))
}

/// PUT /ai/models/:id
pub async fn update_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model: ModelInput = serde_json::from_value(body["model"].clone())
        .map_err(|e| ApiError::BadRequest(e.to_string()))?;

    Ad4mDb::with_global_instance(|db| db.update_model(&id, model))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// DELETE /ai/models/:id
pub async fn remove_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    Ad4mDb::with_global_instance(|db| db.remove_model(&id))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /ai/models/:id/default
pub async fn set_default_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<SetDefaultModelRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    Ad4mDb::with_global_instance(|db| db.set_default_model(body.model_type, &id))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /ai/models/default?modelType=...
pub async fn get_default_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<Option<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_QUERY_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model_type_str = params
        .get("modelType")
        .ok_or_else(|| ApiError::BadRequest("modelType query parameter required".into()))?;

    let model_type: ModelType = serde_json::from_str(&format!("\"{}\"", model_type_str))
        .map_err(|e| ApiError::BadRequest(format!("Invalid modelType: {}", e)))?;

    let model = Ad4mDb::with_global_instance(|db| db.get_default_model(model_type))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(model))
}

/// GET /ai/model-loading-status?model=...
pub async fn get_model_loading_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_QUERY_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model = params
        .get("model")
        .ok_or_else(|| ApiError::BadRequest("model query parameter required".into()))?;

    let status = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .get_model_loading_status(model)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(status).unwrap_or_default()))
}

// ── Tasks ──

/// GET /ai/tasks
pub async fn list_tasks(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<AITask>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_QUERY_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(tasks))
}

/// POST /ai/tasks
pub async fn add_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let task: AITaskInput = serde_json::from_value(body["task"].clone())
        .map_err(|e| ApiError::BadRequest(e.to_string()))?;

    let result = Ad4mDb::with_global_instance(|db| db.add_task(task))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// PUT /ai/tasks/:id
pub async fn update_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let task: AITaskInput = serde_json::from_value(body["task"].clone())
        .map_err(|e| ApiError::BadRequest(e.to_string()))?;

    let result = Ad4mDb::with_global_instance(|db| db.update_task(&id, task))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// DELETE /ai/tasks/:id
pub async fn remove_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let result = Ad4mDb::with_global_instance(|db| db.remove_task(&id))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

// ── Prompt & Embed ──

/// POST /ai/prompt
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
        .prompt(&body.task_id, &body.prompt, &context.auth_token)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// POST /ai/embed
pub async fn ai_embed(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<EmbedRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_EMBED_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let embedding = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .embed(&body.model_id, &body.text, &context.auth_token)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    // Return as base64-encoded zlib-compressed JSON (matching GraphQL format)
    let json_string = serde_json::to_string(&embedding)
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    let compressed_bytes = deflate::deflate_bytes_zlib(json_string.as_bytes());
    Ok(Json(
        base64::prelude::BASE64_STANDARD.encode(&compressed_bytes),
    ))
}

// ── Transcription ──

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
    pub audio: Vec<f64>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CloseTranscriptionRequest {
    pub stream_id: String,
}

/// POST /ai/transcription/open
pub async fn open_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<OpenTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

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

/// POST /ai/transcription/feed
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

/// POST /ai/transcription/close
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
