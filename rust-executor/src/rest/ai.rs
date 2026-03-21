//! AI REST endpoints: /api/v1/ai/*
//!
//! 8 endpoints + WS placeholder for transcription.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::ai_service::AIService;
use crate::types::{AITask, Model, ModelType};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /ai/models — list all models (includes default + loading status)
pub async fn list_models(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let models = AIService::with_global_instance(|ai| {
        ai.get_models()
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    let default_model = AIService::with_global_instance(|ai| {
        ai.get_default_model()
    })
    .ok();

    let loading_status = AIService::with_global_instance(|ai| {
        ai.model_loading_status()
    })
    .ok();

    Ok(Json(serde_json::json!({
        "models": models,
        "defaultModel": default_model,
        "loadingStatus": loading_status,
    })))
}

/// POST /ai/models — add model
pub async fn add_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<AddModelRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = AIService::with_global_instance(|ai| {
        ai.add_model(serde_json::to_string(&body.model).unwrap_or_default())
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

/// PUT /ai/models/:id — update model (including set-default)
pub async fn update_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = AIService::with_global_instance(|ai| {
        ai.update_model(&id, serde_json::to_string(&body).unwrap_or_default())
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

/// DELETE /ai/models/:id — remove model
pub async fn remove_model(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    AIService::with_global_instance(|ai| {
        ai.remove_model(&id)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /ai/tasks — list tasks
pub async fn list_tasks(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let tasks = AIService::with_global_instance(|ai| {
        ai.get_tasks()
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(tasks).unwrap_or_default()))
}

/// POST /ai/tasks — add/update/remove task (action field)
pub async fn manage_task(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<TaskRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    match body.action.as_str() {
        "add" => {
            let task = body.task.ok_or_else(|| ApiError::BadRequest("'task' required".into()))?;
            let result = AIService::with_global_instance(|ai| {
                ai.add_task(serde_json::to_string(&task).unwrap_or_default())
            })
            .map_err(|e| ApiError::Internal(e.to_string()))?;
            Ok(Json(serde_json::to_value(result).unwrap_or_default()))
        }
        "update" => {
            let task_id = body.task_id.ok_or_else(|| ApiError::BadRequest("'taskId' required".into()))?;
            let task = body.task.ok_or_else(|| ApiError::BadRequest("'task' required".into()))?;
            let result = AIService::with_global_instance(|ai| {
                ai.update_task(&task_id, serde_json::to_string(&task).unwrap_or_default())
            })
            .map_err(|e| ApiError::Internal(e.to_string()))?;
            Ok(Json(serde_json::to_value(result).unwrap_or_default()))
        }
        "remove" => {
            let task_id = body.task_id.ok_or_else(|| ApiError::BadRequest("'taskId' required".into()))?;
            AIService::with_global_instance(|ai| {
                ai.remove_task(&task_id)
            })
            .map_err(|e| ApiError::Internal(e.to_string()))?;
            Ok(Json(serde_json::json!(true)))
        }
        other => Err(ApiError::BadRequest(format!("Unknown action: {}", other))),
    }
}

/// POST /ai/prompt — send prompt
pub async fn ai_prompt(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PromptRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = AIService::with_global_instance(|ai| {
        ai.prompt(
            body.model_id.as_deref(),
            &body.prompt,
            body.system.as_deref(),
        )
    })
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// POST /ai/embed — generate embeddings
pub async fn ai_embed(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<EmbedRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = AIService::with_global_instance(|ai| {
        ai.embed(body.model_id.as_deref(), &body.text)
    })
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

// Note: WS /ai/transcription is documented as a WebSocket endpoint.
// Implementing full WS handling requires axum's WebSocket support and
// is left as a follow-up since it needs bidirectional audio streaming.
// For now, the endpoint is defined in the router as a placeholder.
