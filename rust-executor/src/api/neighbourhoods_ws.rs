//! Neighbourhood WS-native handlers.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::agent::{create_signed_expression, AgentContext};
use crate::neighbourhoods::{self, install_neighbourhood_with_context};
use crate::perspectives::get_perspective;
use crate::types::domain::Perspective as DomainPerspective;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

async fn join_neighbourhood(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: JoinNeighbourhoodRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let handle = install_neighbourhood_with_context(body.url, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(handle)?)
}

async fn publish_neighbourhood(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: PublishNeighbourhoodRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let url = neighbourhoods::neighbourhood_publish_from_perspective_with_context(
        &body.perspective_uuid,
        body.link_language,
        body.meta,
        &agent_context,
    )
    .await
    .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(url))
}

fn build_signed_perspective(
    payload: &Value,
    signed: bool,
    agent_context: &AgentContext,
) -> Result<PerspectiveExpression, WsRpcError> {
    if signed {
        let perspective_input: crate::types::PerspectiveInput =
            serde_json::from_value(payload.clone()).map_err(|e| {
                WsRpcError::bad_request(format!("Invalid perspective input: {}", e))
            })?;
        let perspective = DomainPerspective::from(perspective_input);
        let signed = create_signed_expression(perspective, agent_context)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        Ok(PerspectiveExpression::from(signed))
    } else {
        let unsigned: PerspectiveUnsignedInput =
            serde_json::from_value(payload.clone()).map_err(|e| {
                WsRpcError::bad_request(format!("Invalid unsigned perspective input: {}", e))
            })?;
        let links: Vec<DecoratedLinkExpression> = unsigned
            .links
            .into_iter()
            .map(|l| Link::from(l).normalize())
            .map(|l| create_signed_expression(l, agent_context))
            .filter_map(Result::ok)
            .map(LinkExpression::from)
            .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
            .collect();
        let perspective = DomainPerspective { links };
        let signed = create_signed_expression(perspective, agent_context)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        Ok(PerspectiveExpression::from(signed))
    }
}

async fn send_broadcast(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let body: BroadcastRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective =
        build_signed_perspective(&body.payload, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .send_broadcast(signed_perspective, body.loopback.unwrap_or(false))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn send_signal(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let body: SignalRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective =
        build_signed_perspective(&body.payload, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .send_signal(body.remote_agent_did, signed_perspective)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn set_online_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let body: SetOnlineStatusRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let perspective_instance = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    let signed_perspective =
        build_signed_perspective(&body.status, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .set_online_status(signed_perspective)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn has_telepresence(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let perspective = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    Ok(Value::Bool(perspective.has_telepresence_adapter().await))
}

async fn online_agents(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let perspective = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    let agents = perspective
        .online_agents()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(agents)?)
}

async fn other_agents(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let uuid = params.require_str("uuid")?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let current_user_did = crate::agent::did_for_context(&agent_context)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("No perspective found with uuid {}", uuid)))?;

    let handle = perspective.persisted.lock().await.clone();

    if let Some(owners) = &handle.owners {
        if !owners.contains(&current_user_did) {
            return Err(WsRpcError::forbidden(
                "Access denied: You are not an owner of this neighbourhood perspective",
            ));
        }
    }

    let all_dids = perspective
        .others()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let others: Vec<String> = all_dids
        .into_iter()
        .filter(|did| did != &current_user_did)
        .collect();

    Ok(serde_json::to_value(others)?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<JoinNeighbourhoodRequest, PerspectiveHandle>(
        "neighbourhood.join",
        join_neighbourhood,
    )
    .long();
    map.method::<PublishNeighbourhoodRequest, String>(
        "neighbourhood.publish",
        publish_neighbourhood,
    )
    .long();
    map.method::<NeighbourhoodBroadcastParams, bool>("neighbourhood.sendBroadcast", send_broadcast);
    map.method::<NeighbourhoodSignalParams, bool>("neighbourhood.sendSignal", send_signal);
    map.method::<NeighbourhoodOnlineStatusParams, bool>(
        "neighbourhood.setOnlineStatus",
        set_online_status,
    );
    map.method::<NeighbourhoodUuidParams, bool>("neighbourhood.hasTelepresence", has_telepresence)
        .read();
    map.method::<NeighbourhoodUuidParams, Vec<OnlineAgent>>(
        "neighbourhood.onlineAgents",
        online_agents,
    )
    .read();
    map.method::<NeighbourhoodUuidParams, Vec<String>>("neighbourhood.otherAgents", other_agents)
        .read();
}

// ── Contracts ──

/// A perspective to sign and send: signed links when `signed` is true or
/// absent, bare links when `signed` is false.
#[derive(Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum NeighbourhoodPerspectivePayload {
    Signed(NeighbourhoodSignedPerspective),
    Unsigned(PerspectiveUnsignedInput),
}

/// Wire shape of `crate::types::PerspectiveInput`, named apart from the
/// api `PerspectiveInput` so the two TypeScript files do not collide.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodSignedPerspective {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodUuidParams {
    pub uuid: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodBroadcastParams {
    pub uuid: String,
    pub payload: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
    #[ts(optional)]
    pub loopback: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodSignalParams {
    pub uuid: String,
    pub remote_agent_did: String,
    pub payload: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodOnlineStatusParams {
    pub uuid: String,
    pub status: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
}
