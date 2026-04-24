//! Neighbourhood REST endpoints: /api/v1/neighbourhoods/*
//!
//! Harmonised endpoints with signed/unsigned unified via `signed: bool` field.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::agent::{create_signed_expression, AgentContext};
use crate::neighbourhoods::{self, install_neighbourhood_with_context};
use crate::perspectives::get_perspective;
use crate::types::domain::Perspective as DomainPerspective;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;
use ad4m_rest_macros::rest_handler;

/// POST /neighbourhoods/join — join a neighbourhood by URL
#[rest_handler(
    POST,
    "/neighbourhoods/join",
    request = "JoinNeighbourhoodRequest",
    response = "PerspectiveHandle"
)]
pub async fn join_neighbourhood(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<JoinNeighbourhoodRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let handle = install_neighbourhood_with_context(body.url, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(handle))
}

/// POST /neighbourhoods/publish — publish perspective as neighbourhood
#[rest_handler(
    POST,
    "/neighbourhoods/publish",
    request = "PublishNeighbourhoodRequest",
    response = "string"
)]
pub async fn publish_neighbourhood(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PublishNeighbourhoodRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let url = neighbourhoods::neighbourhood_publish_from_perspective_with_context(
        &body.perspective_uuid,
        body.link_language,
        body.meta,
        &agent_context,
    )
    .await
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(url))
}

/// POST /neighbourhoods/:uuid/broadcast — send broadcast (signed or unsigned)
#[rest_handler(
    POST,
    "/neighbourhoods/:uuid/broadcast",
    request = "BroadcastRequest",
    response = "boolean"
)]
pub async fn send_broadcast(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<BroadcastRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective = if body.signed.unwrap_or(true) {
        // Pre-signed payload: deserialize as domain PerspectiveInput (links are LinkExpressionInput)
        let perspective_input: crate::types::PerspectiveInput =
            serde_json::from_value(body.payload.clone())
                .map_err(|e| ApiError::BadRequest(format!("Invalid perspective input: {}", e)))?;
        let perspective = DomainPerspective::from(perspective_input);
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    } else {
        // Unsigned: deserialize as PerspectiveUnsignedInput (links are flat LinkInput)
        let unsigned: PerspectiveUnsignedInput = serde_json::from_value(body.payload.clone())
            .map_err(|e| {
                ApiError::BadRequest(format!("Invalid unsigned perspective input: {}", e))
            })?;
        let links: Vec<DecoratedLinkExpression> = unsigned
            .links
            .into_iter()
            .map(|l| Link::from(l).normalize())
            .map(|l| create_signed_expression(l, &agent_context))
            .filter_map(Result::ok)
            .map(LinkExpression::from)
            .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
            .collect();
        let perspective = DomainPerspective { links };
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    };

    perspective_instance
        .send_broadcast(signed_perspective.into(), body.loopback.unwrap_or(false))
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /neighbourhoods/:uuid/signal — send signal to remote agent (signed or unsigned)
#[rest_handler(
    POST,
    "/neighbourhoods/:uuid/signal",
    request = "SignalRequest",
    response = "boolean"
)]
pub async fn send_signal(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SignalRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective = if body.signed.unwrap_or(true) {
        let perspective_input: crate::types::PerspectiveInput =
            serde_json::from_value(body.payload.clone())
                .map_err(|e| ApiError::BadRequest(format!("Invalid perspective input: {}", e)))?;
        let perspective = DomainPerspective::from(perspective_input);
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    } else {
        let unsigned: PerspectiveUnsignedInput = serde_json::from_value(body.payload.clone())
            .map_err(|e| {
                ApiError::BadRequest(format!("Invalid unsigned perspective input: {}", e))
            })?;
        let links: Vec<DecoratedLinkExpression> = unsigned
            .links
            .into_iter()
            .map(|l| Link::from(l).normalize())
            .map(|l| create_signed_expression(l, &agent_context))
            .filter_map(Result::ok)
            .map(LinkExpression::from)
            .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
            .collect();
        let perspective = DomainPerspective { links };
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    };

    perspective_instance
        .send_signal(body.remote_agent_did, signed_perspective.into())
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /neighbourhoods/:uuid/online-status — set online status (signed or unsigned)
#[rest_handler(
    PUT,
    "/neighbourhoods/:uuid/online-status",
    request = "SetOnlineStatusRequest",
    response = "boolean"
)]
pub async fn set_online_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SetOnlineStatusRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective = if body.signed.unwrap_or(true) {
        let perspective_input: crate::types::PerspectiveInput =
            serde_json::from_value(body.status.clone())
                .map_err(|e| ApiError::BadRequest(format!("Invalid perspective input: {}", e)))?;
        let perspective = DomainPerspective::from(perspective_input);
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    } else {
        let unsigned: PerspectiveUnsignedInput = serde_json::from_value(body.status.clone())
            .map_err(|e| {
                ApiError::BadRequest(format!("Invalid unsigned perspective input: {}", e))
            })?;
        let links: Vec<DecoratedLinkExpression> = unsigned
            .links
            .into_iter()
            .map(|l| Link::from(l).normalize())
            .map(|l| create_signed_expression(l, &agent_context))
            .filter_map(Result::ok)
            .map(LinkExpression::from)
            .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
            .collect();
        let perspective = DomainPerspective { links };
        create_signed_expression(perspective, &agent_context)
            .map_err(|e| ApiError::Internal(e.to_string()))?
    };

    perspective_instance
        .set_online_status(signed_perspective.into())
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /neighbourhoods/:uuid/has-telepresence — check if neighbourhood has telepresence adapter
#[rest_handler(GET, "/neighbourhoods/:uuid/has-telepresence", response = "boolean")]
pub async fn has_telepresence(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    Ok(Json(perspective.has_telepresence_adapter().await))
}

/// GET /neighbourhoods/:uuid/online-agents — list online agents
#[rest_handler(GET, "/neighbourhoods/:uuid/online-agents", response = "OnlineAgent[]")]
pub async fn online_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<Vec<OnlineAgent>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    let agents = perspective
        .online_agents()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(agents))
}

/// GET /neighbourhoods/:uuid/other-agents — list other agents (excluding current user)
#[rest_handler(GET, "/neighbourhoods/:uuid/other-agents", response = "string[]")]
pub async fn other_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let current_user_did = crate::agent::did_for_context(&agent_context)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("No perspective found with uuid {}", uuid)))?;

    let handle = perspective.persisted.lock().await.clone();

    // Check ownership
    if let Some(owners) = &handle.owners {
        if !owners.contains(&current_user_did) {
            return Err(ApiError::Forbidden(
                "Access denied: You are not an owner of this neighbourhood perspective".into(),
            ));
        }
    }

    let all_dids = perspective
        .others()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let others: Vec<String> = all_dids
        .into_iter()
        .filter(|did| did != &current_user_did)
        .collect();

    Ok(Json(others))
}
