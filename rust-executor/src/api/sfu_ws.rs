//! WS RPC handlers for the SFU (Selective Forwarding Unit) service.
//!
//! Mirrors the surface that used to live as juniper resolvers under
//! `graphql/mutation_resolvers.rs` and `graphql/query_resolvers.rs`,
//! now routed through the per-domain handler pattern.
//!
//! All handlers gate on neighbourhood capabilities:
//! - reads (`getConfig`, `listRooms`, `sfuPeer*`, `status`,
//!   `cascadeStatus`, `qualityPreferences`) require
//!   `NEIGHBOURHOOD_READ_CAPABILITY`
//! - writes (`startRoom`, `stopRoom`, `setConfig`, `call*`,
//!   `addIceCandidate`, `sendData`) require `NEIGHBOURHOOD_UPDATE_CAPABILITY`
//! - `ensureMembership` requires the admin credential
//!
//! The SFU service is *always* available — there's no feature gate.
//! When `get_sfu_service()` returns None it means the service hasn't
//! finished booting yet, which surfaces as a 503.

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::{
    check_capability, NEIGHBOURHOOD_READ_CAPABILITY, NEIGHBOURHOOD_UPDATE_CAPABILITY,
};
use crate::db::Ad4mDb;
use crate::sfu::{get_sfu_service, CallSessionInfo, SfuConfig, SfuRoomInfo};
use crate::types::RequestContext;

use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

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

/// Read the params into their contract type. Dispatch has already
/// checked them against it, so this fails only for a direct call.
fn parse<P: DeserializeOwned>(params: Value) -> Result<P, WsRpcError> {
    serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))
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
    let p: SfuRoomParams = parse(params)?;
    let room = service()?
        .start_room(&p.neighbourhood_url, &p.room_name)
        .await
        .map_err(map_room_err)?;
    Ok(serde_json::to_value(room)?)
}

async fn stop_room(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuRoomParams = parse(params)?;
    let ok = service()?
        .stop_room(&p.neighbourhood_url, &p.room_name)
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
    let p: SfuCallJoinParams = parse(params)?;
    let agent_did = caller_did(&ctx)?;

    // Neighbourhood membership gate — the sole check.
    // If the caller's DID appears in the perspective owners for this
    // neighbourhood URL, they have joined the neighbourhood and can
    // join a call.  No admin bypass, no separate whitelist.
    let is_member =
        Ad4mDb::with_global_instance(|db| db.get_neighbourhood_owners(&p.neighbourhood_url))
            .unwrap_or_default()
            .contains(&agent_did);
    if !ctx.is_admin_credential && !is_member {
        return Err(WsRpcError::forbidden(
            "Not a member of this neighbourhood".to_string(),
        ));
    }

    let session = service()?
        .call_join(&p.neighbourhood_url, &p.room_name, &agent_did, &p.sdp_offer)
        .await
        .map_err(map_room_err)?;
    Ok(serde_json::to_value(session)?)
}

async fn call_leave(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuRoomParams = parse(params)?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .call_leave(&p.neighbourhood_url, &p.room_name, &agent_did)
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
    let p: SfuQualityPreferenceParams = parse(params)?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .call_set_quality_preference(
            &p.neighbourhood_url,
            &p.room_name,
            &agent_did,
            &p.preference,
        )
        .await
        .map_err(map_room_err)?;
    Ok(Value::Bool(ok))
}

// ── Config (per-neighbourhood) ──────────────────────────────────────────────

async fn get_config(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuNeighbourhoodParams = parse(params)?;
    let cfg = service()?.get_config(&p.neighbourhood_url).await;
    Ok(serde_json::to_value(cfg)?)
}

async fn set_config(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuSetConfigParams = parse(params)?;
    service()?
        .set_config(&p.neighbourhood_url, p.config)
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
    let p: SfuNeighbourhoodParams = parse(params)?;
    let peer = service()?
        .sfu_peer_for_neighbourhood(&p.neighbourhood_url)
        .await;
    Ok(serde_json::to_value(peer)?)
}

async fn sfu_peers_for_neighbourhood(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuNeighbourhoodParams = parse(params)?;
    let peers = service()?
        .sfu_peers_for_neighbourhood(&p.neighbourhood_url)
        .await;
    Ok(serde_json::to_value(peers)?)
}

// ── Server-initiated renegotiation answer ───────────────────────────────────
//
// The SFU pushes an `sfu-call-renegotiation-offer` event over the existing
// event channel when new peers join; the client replies via this RPC.  The
// event itself is fanned out by the same per-connection subscription pipe as
// every other server-push event (see `events_ws`).

async fn call_answer_server_offer(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuAnswerServerOfferParams = parse(params)?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .call_answer_server_offer(
            &p.neighbourhood_url,
            &p.room_name,
            &agent_did,
            &p.sdp_answer,
        )
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
    let p: SfuAddIceCandidateParams = parse(params)?;
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .add_ice_candidate(&p.neighbourhood_url, &p.room_name, &agent_did, &p.candidate)
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
// on the `sfu-data` event.

async fn send_data(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let p: SfuSendDataParams = parse(params)?;
    let binary = p.binary.unwrap_or(false);
    let data: Vec<u8> = if binary {
        // Binary data arrives base64-encoded.
        base64::Engine::decode(&base64::engine::general_purpose::STANDARD, &p.data)
            .map_err(|e| WsRpcError::bad_request(format!("Invalid base64: {}", e)))?
    } else {
        p.data.into_bytes()
    };
    let agent_did = caller_did(&ctx)?;
    let ok = service()?
        .send_data(
            &p.neighbourhood_url,
            &p.room_name,
            &agent_did,
            &p.channel_label,
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
    Ok(serde_json::to_value(SfuStatus {
        reachability: reach.label().to_string(),
        is_public: reach.is_public(),
        bind_address: svc.local_addr().to_string(),
        detail: reach.to_string(),
    })?)
}

/// Read-only query: how many SFU↔SFU pipe transports are fully
/// established right now.  The cascade scenarios poll this to assert
/// the gossip-driven offer/answer round-trip lit up.
async fn cascade_status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let svc = service()?;
    let established_count = svc.cascade_established_pipe_count().await;
    let pipes = svc
        .cascade_established_pipes()
        .await
        .into_iter()
        .map(|(room_id, remote_did)| SfuCascadePipe {
            room_id,
            remote_did,
        })
        .collect();
    Ok(serde_json::to_value(SfuCascadeStatus {
        established_count,
        pipes,
    })?)
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
    // Admin-only: arbitrary DID registration for test harnesses and bridge
    // deployments.  Regular users get membership through the neighbourhood
    // join flow, not this RPC.
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden(
            "Admin credential required for ensureMembership".to_string(),
        ));
    }
    let p: SfuEnsureMembershipParams = parse(params)?;
    Ad4mDb::with_global_instance(|db| db.ensure_neighbourhood_member(&p.neighbourhood_url, &p.did))
        .map_err(|e| WsRpcError::internal(format!("Failed to register membership: {}", e)))?;
    Ok(Value::Bool(true))
}

/// Query the quality preferences the SFU event loop holds for each
/// participant.  Wind tunnel uses this to verify cascade propagation
/// reached the sender's node.
async fn quality_preferences(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    let prefs: Vec<SfuParticipantQualityPreference> = service()?
        .get_quality_preferences()
        .await
        .into_iter()
        .map(
            |(participant_id, preference)| SfuParticipantQualityPreference {
                participant_id,
                preference,
            },
        )
        .collect();
    Ok(serde_json::to_value(prefs)?)
}

// ── Registration ────────────────────────────────────────────────────────────

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<SfuRoomParams, SfuRoomInfo>("sfu.startRoom", start_room);
    map.method::<SfuRoomParams, bool>("sfu.stopRoom", stop_room);
    map.method::<NoParams, Vec<SfuRoomInfo>>("sfu.listRooms", list_rooms)
        .read();
    map.method::<SfuCallJoinParams, CallSessionInfo>("sfu.callJoin", call_join);
    map.method::<SfuRoomParams, bool>("sfu.callLeave", call_leave);
    map.method::<SfuQualityPreferenceParams, bool>(
        "sfu.callSetQualityPreference",
        call_set_quality_preference,
    );
    map.method::<SfuAnswerServerOfferParams, bool>(
        "sfu.callAnswerServerOffer",
        call_answer_server_offer,
    );
    map.method::<SfuNeighbourhoodParams, SfuConfig>("sfu.getConfig", get_config)
        .read();
    map.method::<SfuSetConfigParams, bool>("sfu.setConfig", set_config);
    map.method::<SfuNeighbourhoodParams, Option<String>>(
        "sfu.sfuPeerForNeighbourhood",
        sfu_peer_for_neighbourhood,
    )
    .read();
    map.method::<SfuNeighbourhoodParams, Vec<String>>(
        "sfu.sfuPeersForNeighbourhood",
        sfu_peers_for_neighbourhood,
    )
    .read();
    map.method::<SfuAddIceCandidateParams, bool>("sfu.addIceCandidate", add_ice_candidate);
    map.method::<SfuSendDataParams, bool>("sfu.sendData", send_data);
    map.method::<NoParams, SfuStatus>("sfu.status", sfu_status)
        .read();
    map.method::<NoParams, SfuCascadeStatus>("sfu.cascadeStatus", cascade_status)
        .read();
    map.method::<NoParams, Vec<SfuParticipantQualityPreference>>(
        "sfu.qualityPreferences",
        quality_preferences,
    )
    .read();
    map.method::<SfuEnsureMembershipParams, bool>("sfu.ensureMembership", ensure_membership);
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuNeighbourhoodParams {
    pub neighbourhood_url: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuRoomParams {
    pub neighbourhood_url: String,
    pub room_name: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuCallJoinParams {
    pub neighbourhood_url: String,
    pub room_name: String,
    /// JSON-encoded `RTCSessionDescriptionInit`.
    pub sdp_offer: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuQualityPreferenceParams {
    pub neighbourhood_url: String,
    pub room_name: String,
    /// The service rejects any other value.
    #[ts(type = r#""high" | "medium" | "low" | "auto""#)]
    pub preference: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuAnswerServerOfferParams {
    pub neighbourhood_url: String,
    pub room_name: String,
    /// JSON-encoded `RTCSessionDescriptionInit`.
    pub sdp_answer: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuSetConfigParams {
    pub neighbourhood_url: String,
    pub config: SfuConfig,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuAddIceCandidateParams {
    pub neighbourhood_url: String,
    pub room_name: String,
    /// The candidate's SDP attribute line.
    pub candidate: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuSendDataParams {
    pub neighbourhood_url: String,
    pub room_name: String,
    pub channel_label: String,
    /// UTF-8 text, or base64 when `binary` is true.
    pub data: String,
    #[ts(optional)]
    pub binary: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuEnsureMembershipParams {
    pub neighbourhood_url: String,
    pub did: String,
}

/// `sfu.status`: whether this executor can relay media to remote
/// participants.
#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuStatus {
    #[ts(type = r#""public" | "nat" | "unknown""#)]
    pub reachability: String,
    /// True when the SFU accepts inbound connections from the internet.
    pub is_public: bool,
    /// The SFU server's bound UDP address (`ip:port`).
    pub bind_address: String,
    /// Human-readable reachability detail.
    pub detail: String,
}

/// `sfu.cascadeStatus`: the SFU↔SFU pipes that are fully established.
#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuCascadeStatus {
    #[ts(type = "number")]
    pub established_count: usize,
    pub pipes: Vec<SfuCascadePipe>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuCascadePipe {
    pub room_id: String,
    pub remote_did: String,
}

/// One entry of `sfu.qualityPreferences`.
#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuParticipantQualityPreference {
    pub participant_id: String,
    pub preference: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::tests::support::admin_ctx;
    use crate::api::ws_handler::build_handler_map;
    use serde_json::json;

    /// Dispatch rejects params outside an `sfu.*` contract before the
    /// handler runs, so no SFU service is needed.
    #[tokio::test]
    async fn sfu_params_outside_the_contract_are_rejected() {
        let map = build_handler_map();
        let room = json!({ "neighbourhoodUrl": "neighbourhood://n", "roomName": "r" });
        let cases = [
            (
                "sfu.startRoom",
                json!({ "neighbourhoodUrl": "neighbourhood://n" }),
            ),
            (
                "sfu.sendData",
                json!({ "neighbourhoodUrl": "n", "roomName": "r", "channelLabel": "c", "data": 1 }),
            ),
            (
                "sfu.sendData",
                json!({ "neighbourhoodUrl": "n", "roomName": "r", "channelLabel": "c", "data": "x", "binary": "yes" }),
            ),
            ("sfu.setConfig", json!({ "neighbourhoodUrl": "n" })),
            (
                "sfu.setConfig",
                json!({ "neighbourhoodUrl": "n", "config": { "maxMeshParticipants": "four" } }),
            ),
            ("sfu.callJoin", room.clone()),
            ("sfu.ensureMembership", json!({ "neighbourhoodUrl": "n" })),
        ];
        for (method, params) in cases {
            let err = map
                .dispatch(method, params.clone(), admin_ctx())
                .await
                .expect_err(&format!("{method} {params}"));
            assert_eq!(err.code, 400, "{method} {params}: {}", err.message);
            assert!(
                err.message
                    .starts_with(&format!("Invalid params for {method}")),
                "{}",
                err.message
            );
        }
    }

    /// `setConfig` takes a partial config: absent fields get their defaults.
    #[test]
    fn set_config_params_fill_config_defaults() {
        let p: SfuSetConfigParams = serde_json::from_value(
            json!({ "neighbourhoodUrl": "n", "config": { "mode": "cascaded" } }),
        )
        .unwrap();
        assert_eq!(p.config.mode, "cascaded");
        assert_eq!(p.config.fallback, "mesh");
        assert_eq!(p.config.max_mesh_participants, 4);
        assert!(p.config.sfu_peers.is_empty());
    }

    #[test]
    fn diagnostic_results_keep_their_wire_fields() {
        let status = serde_json::to_value(SfuStatus {
            reachability: "nat".into(),
            is_public: false,
            bind_address: "10.0.0.1:9000".into(),
            detail: "nat".into(),
        })
        .unwrap();
        assert_eq!(
            status,
            json!({ "reachability": "nat", "isPublic": false, "bindAddress": "10.0.0.1:9000", "detail": "nat" })
        );
        let cascade = serde_json::to_value(SfuCascadeStatus {
            established_count: 1,
            pipes: vec![SfuCascadePipe {
                room_id: "room".into(),
                remote_did: "did:x".into(),
            }],
        })
        .unwrap();
        assert_eq!(
            cascade,
            json!({ "establishedCount": 1, "pipes": [{ "roomId": "room", "remoteDid": "did:x" }] })
        );
        let pref = serde_json::to_value(SfuParticipantQualityPreference {
            participant_id: "p".into(),
            preference: "low".into(),
        })
        .unwrap();
        assert_eq!(pref, json!({ "participantId": "p", "preference": "low" }));
    }

    #[test]
    fn sfu_reads_are_marked_read() {
        let map = build_handler_map();
        let reads: Vec<&str> = map
            .specs()
            .into_iter()
            .filter(|s| s.name.starts_with("sfu.") && s.read)
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(
            reads,
            [
                "sfu.cascadeStatus",
                "sfu.getConfig",
                "sfu.listRooms",
                "sfu.qualityPreferences",
                "sfu.sfuPeerForNeighbourhood",
                "sfu.sfuPeersForNeighbourhood",
                "sfu.status",
            ]
        );
    }
}
