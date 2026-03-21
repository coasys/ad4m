//! Language REST endpoints: /api/v1/languages/*
//!
//! 6 harmonised endpoints.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::languages::LanguageController;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;

/// GET /languages — list (with ?filter= param)
pub async fn list_languages(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let filter = params.get("filter").cloned();
    let languages = LanguageController::languages_list(filter.as_deref())
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(languages).unwrap_or_default()))
}

/// GET /languages/:address — get language (with ?include=meta,source query params)
pub async fn get_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let language = LanguageController::language_by_address(&address)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(language).unwrap_or_default()))
}

/// POST /languages/publish — publish a language
pub async fn publish_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    // Extract language_path and language_meta from body
    let language_path = body.get("languagePath")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let language_meta = body.get("languageMeta").cloned().unwrap_or_default();

    let result = LanguageController::language_publish(
        &language_path,
        &serde_json::to_string(&language_meta).unwrap_or_default(),
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

/// POST /languages/apply-template — apply template and publish
pub async fn apply_template_and_publish(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let source_language_hash = body.get("sourceLanguageHash")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let template_data = body.get("templateData")
        .and_then(|v| v.as_str())
        .unwrap_or("{}")
        .to_string();

    let result = LanguageController::language_apply_template_and_publish(
        &source_language_hash,
        &template_data,
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

/// DELETE /languages/:address — remove a language
pub async fn remove_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    LanguageController::language_remove(&address)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /languages/:address/settings — write settings
pub async fn write_settings(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let settings = serde_json::to_string(&body).unwrap_or_default();

    LanguageController::language_write_settings(&address, &settings)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}
