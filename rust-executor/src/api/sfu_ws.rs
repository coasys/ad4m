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
use crate::db::Ad4mDb;
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
/// (`runtime.createUser` / `runtime.loginUser`) — this is the
/// production case and how multi-participant calls authenticate.  In
/// single-user / admin flows there is no per-user JWT and `user_did`
/// is None; the executor is acting on behalf of *its own* main agent,
/// so we fall through to `crate::agent::did()`.
fn caller_did(ctx: &RequestContext) -> Result<String, WsRpcError> {
    if let Some(did) = ctx.user_did.clone() {
        return Ok(did);
    }
    if ctx.is_admin_credential {
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
    let agent_did = caller_did(&ctx)?;

    // Neighbourhood membership gate — the sole check.
    // If the caller's DID appears in the perspective owners for this
    // neighbourhood URL, they have joined the neighbourhood and can
    // join a call.  No admin bypass, no separate whitelist.
    let is_member =
        Ad4mDb::with_global_instance(|db| db.get_neighbourhood_owners(&neighbourhood_url))
            .unwrap_or_default()
            .contains(&agent_did);
    if !is_member {
        return Err(WsRpcError::forbidden(
            "Not a member of this neighbourhood".to_string(),
        ));
    }

    let session = service()?
        .call_join(&neighbourhood_url, &room_name, &agent_did, &sdp_offer)
        .await
        .map_err(map_room_err)?;
    Ok(serde_json::to_value(session)?)
}

async fn call_leave(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let agent_did = caller_did(&ctx)?;
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
    let agent_did = caller_did(&ctx)?;
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
    let sdp_answer = params.require_str("sdpAnswer")?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .call_answer_server_offer(&neighbourhood_url, &room_name, &agent_did, &sdp_answer)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

// ── Trickle ICE ───────────────────────────────────────────────────────────────
//
// Companion to `callJoin`: when the client gathers ICE candidates
// incrementally (trickle) instead of waiting for gathering to complete,
// each candidate arrives here.  The SFU adds it to the peer's str0m
// Rtc instance via `Rtc::add_remote_candidate`.

async fn add_ice_candidate(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let candidate_sdp = params.require_str("candidate")?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .add_ice_candidate(&neighbourhood_url, &room_name, &agent_did, &candidate_sdp)
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

// ── Data channel relay ────────────────────────────────────────────────────────
//
// Applications that use WebRTC data channels in mesh mode (chat,
// cursor sync, file transfer) need those messages to flow through
// the SFU too.  This RPC lets them push data; the SFU relays it to
// all other participants' matching data channels and publishes it
// on the `sfu-data` events_ws topic.

async fn send_data(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let room_name = params.require_str("roomName")?;
    let channel_label = params.require_str("channelLabel")?;
    let binary = params
        .get("binary")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let data_value = params.require("data")?;
    let data: Vec<u8> = if binary {
        // Expect base64-encoded string for binary data.
        let encoded = data_value
            .as_str()
            .ok_or_else(|| WsRpcError::bad_request("binary data must be a base64 string"))?;
        base64::Engine::decode(&base64::engine::general_purpose::STANDARD, encoded)
            .map_err(|e| WsRpcError::bad_request(format!("Invalid base64: {}", e)))?
    } else {
        // Text data — UTF-8 string.
        data_value
            .as_str()
            .ok_or_else(|| WsRpcError::bad_request("text data must be a string"))?
            .as_bytes()
            .to_vec()
    };
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .send_data(
            &neighbourhood_url,
            &room_name,
            &agent_did,
            &channel_label,
            data,
            binary,
        )
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

/// Read-only query: SFU service status including public reachability.
/// Clients use this to determine whether this executor can serve as an
/// SFU relay for remote participants.
async fn sfu_status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let svc = service()?;
    let reach = svc.reachability();
    let mut out = serde_json::Map::new();
    out.insert(
        "reachability".to_string(),
        Value::String(reach.label().to_string()),
    );
    out.insert("isPublic".to_string(), Value::Bool(reach.is_public()));
    out.insert(
        "bindAddress".to_string(),
        Value::String(svc.local_addr().to_string()),
    );
    out.insert("detail".to_string(), Value::String(format!("{}", reach)));
    Ok(Value::Object(out))
}

/// Read-only query: how many SFU↔SFU pipe transports are fully
/// established right now.  The cascade scenarios poll this to assert
/// the gossip-driven offer/answer round-trip lit up.
async fn cascade_status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let svc = service()?;
    let established_count = svc.cascade_established_pipe_count().await;
    let pipes = svc.cascade_established_pipes().await;
    let pipe_list: Vec<Value> = pipes
        .into_iter()
        .map(|(room_id, remote_did)| {
            let mut o = serde_json::Map::new();
            o.insert("roomId".to_string(), Value::String(room_id));
            o.insert("remoteDid".to_string(), Value::String(remote_did));
            Value::Object(o)
        })
        .collect();
    let mut out = serde_json::Map::new();
    out.insert(
        "establishedCount".to_string(),
        Value::Number(serde_json::Number::from(established_count)),
    );
    out.insert("pipes".to_string(), Value::Array(pipe_list));
    Ok(Value::Object(out))
}

// ── Neighbourhood membership registration ─────────────────────────────────
//
// Integration hook for registering DIDs as neighbourhood members on
// this executor.  In production the neighbourhood join flow handles
// this automatically; this RPC exists so test harnesses and bridge
// deployments can set up membership for synthetic neighbourhood URLs.
//
// Writes directly to the perspective_handle owners list — the same
// data that callJoin queries via get_neighbourhood_owners.  No
// separate whitelist, no backdoor.

async fn ensure_membership(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let neighbourhood_url = params.require_str("neighbourhoodUrl")?;
    let did = params.require_str("did")?;
    Ad4mDb::with_global_instance(|db| db.ensure_neighbourhood_member(&neighbourhood_url, &did))
        .map_err(|e| WsRpcError::internal(format!("Failed to register membership: {}", e)))?;
    Ok(Value::Bool(true))
}

/// Query the quality preferences the SFU event loop holds for each
/// participant.  Returns `[{participantId, preference}, ...]`.
/// Wind tunnel uses this to verify cascade propagation reached the
/// sender's node.
async fn quality_preferences(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let svc = service()?;
    let prefs = svc.get_quality_preferences().await;
    let list: Vec<Value> = prefs
        .into_iter()
        .map(|(pid, pref)| {
            let mut o = serde_json::Map::new();
            o.insert("participantId".to_string(), Value::String(pid));
            o.insert("preference".to_string(), Value::String(pref));
            Value::Object(o)
        })
        .collect();
    Ok(Value::Array(list))
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
    map.register("sfu.addIceCandidate", add_ice_candidate);
    map.register("sfu.sendData", send_data);
    map.register("sfu.status", sfu_status);
    map.register("sfu.cascadeStatus", cascade_status);
    map.register("sfu.qualityPreferences", quality_preferences);
    map.register("sfu.ensureMembership", ensure_membership);
}
