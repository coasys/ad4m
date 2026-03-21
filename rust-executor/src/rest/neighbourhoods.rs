//! Neighbourhood REST endpoints: /api/v1/neighbourhoods/*
//!
//! 7 harmonised endpoints. No `*U` variants — use `signed: bool` in body.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::neighbourhoods;
use crate::perspectives::get_perspective;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// POST /neighbourhoods/join — join from URL
pub async fn join_neighbourhood(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<JoinNeighbourhoodRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let handle = neighbourhoods::install_neighbourhood_with_context(&body.url, &context.auth_token)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(handle).unwrap_or_default()))
}

/// POST /neighbourhoods/publish — publish from perspective
pub async fn publish_neighbourhood(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PublishNeighbourhoodRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let meta_str = body
        .meta
        .map(|m| serde_json::to_string(&m).unwrap_or_default());

    let result = neighbourhoods::publish_from_perspective(
        &body.uuid,
        &body.link_language,
        meta_str.as_deref(),
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

/// POST /neighbourhoods/:uuid/broadcast — send broadcast (signed: true|false in body)
pub async fn send_broadcast(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SendBroadcastRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let signed = body.signed.unwrap_or(true);
    perspective
        .send_broadcast(
            serde_json::to_string(&body.perspective).unwrap_or_default(),
            signed,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /neighbourhoods/:uuid/signal — send signal (signed: true|false in body)
pub async fn send_signal(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SendSignalRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let signed = body.signed.unwrap_or(true);
    perspective
        .send_signal(
            &body.recipient,
            serde_json::to_string(&body.payload).unwrap_or_default(),
            signed,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /neighbourhoods/:uuid/online-status — set online status (signed: true|false)
pub async fn set_online_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SetOnlineStatusRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let signed = body.signed.unwrap_or(true);
    perspective
        .set_online_status(
            serde_json::to_string(&body.perspective).unwrap_or_default(),
            signed,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /neighbourhoods/:uuid/agents — list agents (?online=true for online only)
pub async fn list_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let online_only = params.get("online").map(|v| v == "true").unwrap_or(false);

    let agents = if online_only {
        perspective
            .online_agents()
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?
    } else {
        perspective
            .other_agents()
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?
    };

    Ok(Json(serde_json::to_value(agents).unwrap_or_default()))
}

/// GET /neighbourhoods/:uuid/telepresence — has telepresence adapter
pub async fn has_telepresence(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let has = perspective
        .has_telepresence_adapter()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(has))
}
