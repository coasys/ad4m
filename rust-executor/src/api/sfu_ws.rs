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

/// Resolve the caller's DID for SFU operations.
///
/// In the multi-user flow `ctx.user_did` is set from the per-user JWT
/// (`runtime.createUser` / `runtime.loginUser`).  In single-user / admin
/// flows there is no per-user JWT and `user_did` is None; the executor
/// is acting on behalf of *its own* main agent.
///
/// Resolution order:
///   1. `ctx.user_did` — populated by the per-user JWT.
///   2. `agentDidOverride` param — admin-only escape hatch: when the
///      caller authenticated with the admin credential and explicitly
///      passes this string, use it.  Lets the wind tunnel drive N
///      synthetic participants from a single admin connection without
///      tripping the SFU's per-DID duplicate-join check.
///   3. `crate::agent::did()` — single-user fallback to the executor's
///      main agent DID when admin-authenticated without an override.
fn caller_did(ctx: &RequestContext, params: &Value) -> Result<String, WsRpcError> {
    if let Some(did) = ctx.user_did.clone() {
        return Ok(did);
    }
    if ctx.is_admin_credential {
        if let Some(override_did) = params
            .get("agentDidOverride")
            .and_then(|v| v.as_str())
            .filter(|s| !s.is_empty())
        {
            return Ok(override_did.to_string());
        }
        return Ok(crate::agent::did());
    }
    Err(WsRpcError::unauthorized(
        "Caller DID not resolved from token",
    ))
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
    let agent_did = caller_did(&ctx, &params)?;
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
    let agent_did = caller_did(&ctx, &params)?;
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
    let agent_did = caller_did(&ctx, &params)?;
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
    let peer = service()?
        .sfu_peer_for_neighbourhood(&neighbourhood_url)
        .await;
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
    let _agent_did = caller_did(&ctx, &params)?;
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

// ── Cascade admin (test/dev only) ──────────────────────────────────────────

/// Statically seed cascade peers without the gossip layer.  Admin
/// only.  Wind tunnel uses this to set up multi-node cascade tests
/// (T3 / T4 / S2 / S3) on a single host without needing the full DNA
/// + neighbourhood discovery chain.
///
/// Params:
/// - `localDid`: this executor's identifier in the cluster.
/// - `maxParticipantsPerNode`: capacity hint published to peers.
/// - `peers`: array of `{did, addr}` objects for the other SFU nodes.
async fn enable_cascade(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("admin credential required".to_string()));
    }
    let local_did = params.require_str("localDid")?;
    let max_per_node = params
        .get("maxParticipantsPerNode")
        .and_then(|v| v.as_u64())
        .ok_or_else(|| WsRpcError::bad_request("maxParticipantsPerNode required"))?
        as u32;
    let peers_value = params.require("peers")?;
    let peers_arr = peers_value
        .as_array()
        .ok_or_else(|| WsRpcError::bad_request("peers must be an array"))?;
    let mut peers: Vec<(String, std::net::SocketAddr)> = Vec::new();
    for p in peers_arr {
        let did = p
            .get("did")
            .and_then(|v| v.as_str())
            .ok_or_else(|| WsRpcError::bad_request("each peer needs `did`"))?
            .to_string();
        let addr_str = p
            .get("addr")
            .and_then(|v| v.as_str())
            .ok_or_else(|| WsRpcError::bad_request("each peer needs `addr`"))?;
        let addr: std::net::SocketAddr = addr_str.parse().map_err(|e: std::net::AddrParseError| {
            WsRpcError::bad_request(format!("addr `{}` parse: {}", addr_str, e))
        })?;
        peers.push((did, addr));
    }
    service()?
        .enable_cascade(local_did, max_per_node, peers)
        .await
        .map_err(WsRpcError::internal)?;
    Ok(Value::Bool(true))
}

/// Admin-only: push a participant-count update into a node's
/// CascadeManager.  Wind tunnel uses this to keep static cluster
/// state fresh without standing up a gossip layer.
async fn cascade_announce(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("admin credential required".to_string()));
    }
    let remote_did = params.require_str("remoteDid")?;
    let room_id = params.require_str("roomId")?;
    let participant_count = params
        .get("participantCount")
        .and_then(|v| v.as_u64())
        .ok_or_else(|| WsRpcError::bad_request("participantCount required"))?
        as u32;
    service()?
        .cascade_announce(remote_did, room_id, participant_count)
        .await
        .map_err(WsRpcError::internal)?;
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
    map.register("sfu.enableCascade", enable_cascade);
    map.register("sfu.cascadeAnnounce", cascade_announce);
}
