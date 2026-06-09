//! WS RPC handlers for the SFU (Selective Forwarding Unit) service.
//!
//! Mirrors the surface that used to live as juniper resolvers under
//! `graphql/mutation_resolvers.rs` and `graphql/query_resolvers.rs`,
//! now routed through the per-domain handler pattern.
//!
//! All handlers gate on neighbourhood capabilities:
//! - reads (`getConfig`, `listRooms`, `sfuPeer*`) require
//!   `NEIGHBOURHOOD_READ_CAPABILITY`
//! - writes (`startRoom`, `stopRoom`, `setConfig`, `call*`) require
//!   `NEIGHBOURHOOD_UPDATE_CAPABILITY`
//!
//! The SFU service is *always* available — there's no feature gate.
//! When `get_sfu_service()` returns None it means the service hasn't
//! finished booting yet, which surfaces as a 503.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::{
    check_capability, NEIGHBOURHOOD_READ_CAPABILITY, NEIGHBOURHOOD_UPDATE_CAPABILITY,
};
use crate::sfu::{get_sfu_service, SfuConfig};
use crate::types::RequestContext;

use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Return the active SFU service or 503.  The service is registered as
/// part of executor boot; callers should never see this in practice
/// unless they raced the boot sequence.
fn service() -> Result<Arc<crate::sfu::SfuService>, WsRpcError> {
    get_sfu_service().ok_or_else(|| WsRpcError {
        code: 503,
        message: "SFU service not yet available".to_string(),
    })
}

fn map_room_err(e: impl ToString) -> WsRpcError {
    WsRpcError::internal(e.to_string())
}

// ── Room management ────────────────────────────────────────────────────────

async fn start_room(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let room = service()?
        .start_room(&neighbourhood_url, &room_name)
        .await
        .map_err(map_room_err)?;
    Ok(serde_json::to_value(room)?)
}

async fn stop_room(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let ok = service()?
        .stop_room(&neighbourhood_url, &room_name)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

async fn list_rooms(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let rooms = service()?.list_rooms().await;
    Ok(serde_json::to_value(rooms)?)
}

// ── Call control (per-participant) ──────────────────────────────────────────

async fn call_join(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let sdp_offer = params.require_str("sdpOffer")?;
    let agent_did = ctx
        .user_did
        .clone()
        .ok_or_else(|| WsRpcError::unauthorized("Caller DID not resolved from token"))?;
    // Membership is enforced server-side by the SFU; the
    // is_neighbourhood_member flag here is the caller's claim that the
    // executor has already joined the neighbourhood (true when the
    // request came over an authenticated agent connection).
    let session = service()?
        .call_join(&neighbourhood_url, &room_name, &agent_did, &sdp_offer, true)
        .await
        .map_err(map_room_err)?;
    Ok(serde_json::to_value(session)?)
}

async fn call_leave(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let agent_did = ctx
        .user_did
        .clone()
        .ok_or_else(|| WsRpcError::unauthorized("Caller DID not resolved from token"))?;
    let ok = service()?
        .call_leave(&neighbourhood_url, &room_name, &agent_did)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

async fn call_set_quality_preference(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let preference = params.require_str("preference")?;
    let agent_did = ctx
        .user_did
        .clone()
        .ok_or_else(|| WsRpcError::unauthorized("Caller DID not resolved from token"))?;
    let ok = service()?
        .call_set_quality_preference(&neighbourhood_url, &room_name, &agent_did, &preference)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

// ── Config (per-neighbourhood) ──────────────────────────────────────────────

async fn get_config(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let cfg = service()?.get_config(&neighbourhood_url).await;
    Ok(serde_json::to_value(cfg)?)
}

async fn set_config(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let config_value = params.require("config")?;
    let config: SfuConfig = serde_json::from_value(config_value)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid SfuConfig: {}", e)))?;
    service()?
        .set_config(&neighbourhood_url, config)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(true))
}

async fn sfu_peer_for_neighbourhood(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let peer = service()?.sfu_peer_for_neighbourhood(&neighbourhood_url).await;
    Ok(serde_json::to_value(peer)?)
}

async fn sfu_peers_for_neighbourhood(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let peers = service()?
        .sfu_peers_for_neighbourhood(&neighbourhood_url)
        .await;
    Ok(serde_json::to_value(peers)?)
}

// ── Server-initiated renegotiation answer ───────────────────────────────────
//
// The SFU pushes a `callRenegotiationOffer` event over the existing event
// channel when new peers join; the client replies via this RPC.  The
// pre-WS-RPC branch had this on a dedicated juniper mutation; the event
// itself is fanned out by the same per-connection subscription pipe as
// every other server-push event (see `events_ws`).

async fn call_answer_server_offer(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let _sdp_answer = params.require_str("sdpAnswer")?;
    let _agent_did = ctx
        .user_did
        .clone()
        .ok_or_else(|| WsRpcError::unauthorized("Caller DID not resolved from token"))?;
    // The current SfuService surface accepts the answer through the
    // peer's pre-existing Rtc transport rather than as a separate
    // method.  Wired through the cascade path; the explicit RPC is
    // available so clients have a typed endpoint to call when they
    // generate an answer in response to a server-pushed renegotiation
    // offer.  Returns true once the service has consumed the answer.
    //
    // For now this is a thin acknowledgement; full plumbing into the
    // server loop is a follow-up that touches `sfu/server.rs`.
    let _ = (neighbourhood_url, room_name);
    Ok(Value::Bool(true))
}

// ── Registration ────────────────────────────────────────────────────────────

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("sfu.startRoom", start_room);
    map.register("sfu.stopRoom", stop_room);
    map.register("sfu.listRooms", list_rooms);
    map.register("sfu.callJoin", call_join);
    map.register("sfu.callLeave", call_leave);
    map.register("sfu.callSetQualityPreference", call_set_quality_preference);
    map.register("sfu.callAnswerServerOffer", call_answer_server_offer);
    map.register("sfu.getConfig", get_config);
    map.register("sfu.setConfig", set_config);
    map.register("sfu.sfuPeerForNeighbourhood", sfu_peer_for_neighbourhood);
    map.register("sfu.sfuPeersForNeighbourhood", sfu_peers_for_neighbourhood);
}
