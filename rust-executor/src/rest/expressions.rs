//! Expression REST endpoints: /api/v1/expressions/*
//!
//! 5 harmonised endpoints.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::agent::create_signed_expression;
use crate::languages::LanguageController;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /expressions/:url — get expression (?raw=true for raw)
pub async fn get_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let raw = params.get("raw").map(|v| v == "true").unwrap_or(false);

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let expr = LanguageController::expression_get(&decoded_url)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(expr).unwrap_or(serde_json::Value::Null)))
}

/// GET /expressions/:url/interactions — get interactions
pub async fn get_interactions(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let interactions = LanguageController::expression_interactions(&decoded_url)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(interactions).unwrap_or_default()))
}

/// POST /expressions — create expression
pub async fn create_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreateExpressionRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let url = LanguageController::expression_create(
        &body.language_address,
        &body.content,
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(url))
}

/// POST /expressions/many — get multiple expressions
pub async fn get_many_expressions(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ExpressionManyRequest>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let mut results = Vec::new();
    for url in &body.urls {
        let expr = LanguageController::expression_get(url)
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        results.push(serde_json::to_value(expr).unwrap_or(serde_json::Value::Null));
    }

    Ok(Json(results))
}

/// POST /expressions/:url/interact — interact with expression
pub async fn interact_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
    Json(body): Json<InteractRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let result = LanguageController::expression_interact(
        &decoded_url,
        &body.interaction_call,
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}
