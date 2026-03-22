//! Expression REST endpoints: /api/v1/expressions/*
//!
//! 5 harmonised endpoints.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::agent::AgentContext;
use crate::languages::LanguageController;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;

/// GET /expressions/:url — get expression
pub async fn get_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
) -> Result<Json<Option<serde_json::Value>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let controller = LanguageController::global_instance();
    let parsed = LanguageController::parse_expr_url(&decoded_url);

    if let Ok((lang_address, expression_address)) = parsed {
        let is_literal = lang_address == "literal";
        let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

        if is_loaded {
            match controller
                .get_expression(&lang_address, &expression_address)
                .await
            {
                Ok(Some(expr_json)) => {
                    return Ok(Json(Some(expr_json)));
                }
                Ok(None) => return Ok(Json(None)),
                Err(e) => {
                    return Err(ApiError::Internal(format!(
                        "Failed to get expression {}: {}",
                        decoded_url, e
                    )));
                }
            }
        }
    }

    Ok(Json(None))
}

/// GET /expressions/:url/interactions — get interactions
pub async fn get_interactions(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
) -> Result<Json<Vec<InteractionMeta>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            let interactions = controller
                .expression_interactions(&decoded_url)
                .await
                .map_err(|e| {
                    ApiError::Internal(format!(
                        "Failed to get expression interactions for {}: {}",
                        decoded_url, e
                    ))
                })?;
            return Ok(Json(interactions));
        }
    }

    Ok(Json(vec![]))
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

    let controller = LanguageController::global_instance();
    let content_json: serde_json::Value =
        serde_json::from_str(&body.content).unwrap_or(serde_json::Value::String(body.content));
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let url = controller
        .expression_create(&body.language_address, content_json, &agent_context)
        .await
        .map_err(|e| {
            ApiError::Internal(format!(
                "Failed to create expression on {}: {}",
                body.language_address, e
            ))
        })?;

    Ok(Json(url))
}

/// POST /expressions/many — get multiple expressions
pub async fn get_many_expressions(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ExpressionManyRequest>,
) -> Result<Json<Vec<Option<serde_json::Value>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let controller = LanguageController::global_instance();
    let mut results = Vec::new();

    for url in &body.urls {
        if let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(url) {
            let is_literal = lang_address == "literal";
            let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

            if is_loaded {
                match controller
                    .get_expression(&lang_address, &expression_address)
                    .await
                {
                    Ok(expr) => results.push(expr),
                    Err(_) => results.push(None),
                }
            } else {
                results.push(None);
            }
        } else {
            results.push(None);
        }
    }

    Ok(Json(results))
}

/// POST /expressions/:url/interact — interact with expression
pub async fn interact_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(url): Path<String>,
    Json(body): Json<InteractionCall>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &EXPRESSION_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url.clone());

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            match controller.expression_interact(&decoded_url, &body).await {
                Ok(Some(result)) => return Ok(Json(result)),
                Ok(None) => return Ok(Json("null".to_string())),
                Err(e) => {
                    return Err(ApiError::Internal(format!(
                        "expression_interact failed for {}: {}",
                        decoded_url, e
                    )));
                }
            }
        }
    }

    Err(ApiError::NotFound(format!(
        "Language not loaded for expression URL: {}",
        decoded_url
    )))
}
