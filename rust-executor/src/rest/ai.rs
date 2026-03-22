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
use crate::types::{AITask, AITaskInput, Model, ModelInput, ModelType};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

// Default pricing (matches GraphQL)
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

fn get_rate(description: &str, default: f64) -> Result<f64, ApiError> {
    match Ad4mDb::with_global_instance(|db| db.get_host_rate(description)) {
        Ok(Some(rate)) => Ok(rate),
        Ok(None) => Ok(default),
        Err(e) => Err(ApiError::Internal(format!("Failed to read host rate: {}", e))),
    }
}

/// GET /ai/models — list all models
pub async fn list_models(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Model>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let tasks = AIService::get_tasks()
        .map_err(|e| ApiError::Internal(e.to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    check_compute_credits(&context.auth_token)?;

    let result = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .prompt(body.task_id, body.prompt)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let total_tokens = result.prompt_tokens + result.completion_tokens;
    let model_name = match Ad4mDb::with_global_instance(|db| db.get_model(result.model_id.clone())) {
        Ok(Some(m)) => m.name,
        _ => String::new(),
    };
    if let Err(e) = reserve_compute_credits(
        &context.auth_token,
        total_tokens as f64 * get_rate(&model_name, DEFAULT_TOKEN_RATE)?,
    ) {
        log::warn!("Call exceeded compute credits (ai_prompt, model={}, tokens={}): {:?}", model_name, total_tokens, e);
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        log::warn!("Call exceeded compute credits (ai_embed, tokens={}): {:?}", result.token_count, e);
    }

    let json_string = serde_json::to_string(&result.embeddings)
        .map_err(|e| ApiError::Internal(format!("Failed to serialize vector: {}", e)))?;

    // Compress with zlib like GraphQL does
    let compressed_bytes = deflate::deflate_bytes_zlib(json_string.as_bytes());
    Ok(Json(base64::prelude::BASE64_STANDARD.encode(&compressed_bytes)))
}
