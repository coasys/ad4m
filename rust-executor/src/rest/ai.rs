//! AI REST endpoints: /api/v1/ai/*

use axum::{
    extract::{Path, Query, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::types::{AITask, AITaskInput, Model, ModelInput, ModelType, VoiceActivityParamsInput};
use base64::Engine;
use serde::Deserialize;
use std::collections::HashMap;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;
use ad4m_rest_macros::rest_handler;

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

#[allow(dead_code)]
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
#[rest_handler(GET, "/ai/models", response = "Model[]")]
pub async fn list_models(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let _service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    // Get models from the AI service's internal state
    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(models))
}

/// POST /ai/models
#[rest_handler(
    POST,
    "/ai/models",
    request = "Record<string, unknown>",
    response = "string"
)]
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

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let id = service
        .add_model(model)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(id))
}

/// PUT /ai/models/:id
#[rest_handler(
    PUT,
    "/ai/models/:id",
    request = "Record<string, unknown>",
    response = "boolean"
)]
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

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    service
        .update_model(id, model)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to update model: {}", e)))?;

    Ok(Json(true))
}

/// DELETE /ai/models/:id
#[rest_handler(DELETE, "/ai/models/:id", response = "boolean")]
pub async fn remove_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    service
        .remove_model(id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /ai/models/:id/default
#[rest_handler(
    PUT,
    "/ai/models/:id/default",
    request = "SetDefaultModelRequest",
    response = "boolean"
)]
pub async fn set_default_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<SetDefaultModelRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    service
        .set_default_model(body.model_type, id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /ai/models/default?modelType=...
#[rest_handler(GET, "/ai/models/default", response = "Model | null")]
pub async fn get_default_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<Option<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model_type_str = params
        .get("modelType")
        .ok_or_else(|| ApiError::BadRequest("modelType query parameter required".into()))?;

    let model_type: ModelType = serde_json::from_str(&format!("\"{}\"", model_type_str))
        .map_err(|e| ApiError::BadRequest(format!("Invalid modelType: {}", e)))?;

    let model_id = Ad4mDb::with_global_instance(|db| db.get_default_model(model_type))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let model = if let Some(id) = model_id {
        Ad4mDb::with_global_instance(|db| db.get_model(id))
            .map_err(|e| ApiError::Internal(e.to_string()))?
    } else {
        None
    };

    Ok(Json(model))
}

/// GET /ai/model-loading-status?model=...
#[rest_handler(GET, "/ai/model-loading-status", response = "AIModelLoadingStatus")]
pub async fn get_model_loading_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let model = params
        .get("model")
        .ok_or_else(|| ApiError::BadRequest("model query parameter required".into()))?;

    let status = AIService::model_status(model.clone())
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(status).unwrap_or_default()))
}

// ── Tasks ──

/// GET /ai/tasks
#[rest_handler(GET, "/ai/tasks", response = "AITask[]")]
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

/// POST /ai/tasks
#[rest_handler(
    POST,
    "/ai/tasks",
    request = "Record<string, unknown>",
    response = "AITask"
)]
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

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let result = service
        .add_task(task)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// PUT /ai/tasks/:id
#[rest_handler(
    PUT,
    "/ai/tasks/:id",
    request = "Record<string, unknown>",
    response = "AITask"
)]
pub async fn update_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(_id): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<AITask>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let task: AITask = serde_json::from_value(body["task"].clone())
        .map_err(|e| ApiError::BadRequest(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let result = service
        .update_task(task)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// DELETE /ai/tasks/:id
#[rest_handler(DELETE, "/ai/tasks/:id", response = "boolean")]
pub async fn remove_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    service
        .delete_task(id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

// ── Prompt & Embed ──

/// POST /ai/prompt
#[rest_handler(POST, "/ai/prompt", request = "PromptRequest", response = "string")]
pub async fn ai_prompt(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PromptRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let result = service
        .prompt(body.task_id, body.prompt)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result.text))
}

/// POST /ai/embed
#[rest_handler(POST, "/ai/embed", request = "EmbedRequest", response = "string")]
pub async fn ai_embed(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<EmbedRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let embedding = service
        .embed(body.model_id, body.text)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    // Return as base64-encoded zlib-compressed JSON
    let json_string = serde_json::to_string(&embedding.embeddings)
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
#[rest_handler(
    POST,
    "/ai/transcription/open",
    request = "OpenTranscriptionRequest",
    response = "string"
)]
pub async fn open_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<OpenTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let stream_id = service
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
///
/// Accepts raw PCM Float32 little-endian bytes (application/octet-stream).
/// Stream IDs are passed via `X-Stream-Ids` header (comma-separated).
/// Transcription text results are delivered via the existing SSE channel
/// at `/events/ai` (type: "transcription-text").
#[rest_handler(
    POST,
    "/ai/transcription/feed",
    request = "bytes",
    response = "string"
)]
pub async fn feed_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    headers: axum::http::HeaderMap,
    body: axum::body::Bytes,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    // Parse stream IDs from header
    let stream_ids_header = headers
        .get("x-stream-ids")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let stream_ids: Vec<String> = stream_ids_header
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if stream_ids.is_empty() {
        return Err(ApiError::BadRequest(
            "X-Stream-Ids header is required".into(),
        ));
    }

    // Interpret raw bytes as f32 little-endian samples (zero-copy)
    if body.len() % 4 != 0 {
        return Err(ApiError::BadRequest(
            "Body length must be a multiple of 4 (Float32 samples)".into(),
        ));
    }
    let audio_f32: Vec<f32> = body
        .chunks_exact(4)
        .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
        .collect();

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    for stream_id in &stream_ids {
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
#[rest_handler(
    POST,
    "/ai/transcription/close",
    request = "CloseTranscriptionRequest",
    response = "string"
)]
pub async fn close_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CloseTranscriptionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    service
        .close_transcription_stream(&body.stream_id, &context.auth_token)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json("true".to_string()))
}
