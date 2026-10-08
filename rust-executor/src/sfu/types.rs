//! Public, serde-only types exposed by the SFU service.
//!
//! These previously lived as juniper GraphQL types in `graphql_types.rs`.
//! After the GraphQL → WS RPC migration on dev they are plain serde
//! structs serialised straight to the WebSocket reply. The `#[ts(export)]`
//! ones are part of the SDK contract (`core/src/generated/api/`).

use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// A neighbourhood's call configuration, stored as a link in the
/// neighbourhood (see `sfu::config_store`).
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuConfig {
    /// `"mesh"` | `"designated"` | `"gateway"` | `"cascaded"`
    #[serde(default = "default_mode")]
    #[ts(type = r#""mesh" | "designated" | "gateway" | "cascaded""#)]
    pub mode: String,
    /// DID of the designated SFU peer (only used when `mode = "designated"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub designated_peer: Option<String>,
    /// Fallback mode when SFU is unavailable.
    #[serde(default = "default_fallback")]
    #[ts(type = r#""mesh" | "designated" | "gateway" | "cascaded""#)]
    pub fallback: String,
    /// Maximum participants before mesh is degraded.
    #[serde(default = "default_max_mesh")]
    pub max_mesh_participants: u32,
    /// DIDs of SFU peers in cascaded mode.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(as = "Option<Vec<String>>", optional)]
    pub sfu_peers: Vec<String>,
    /// Max participants per SFU node in cascaded mode.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub max_participants_per_node: Option<u32>,
    /// DID of the preferred SFU node in cascaded mode.  The cascade
    /// manager routes new joins to this node first; overflow spills to
    /// other nodes only when the preferred one reaches capacity.
    /// Useful when a powerful hosted multi-user node serves most
    /// participants and lighter personal executors act as fallbacks.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub preferred_sfu_did: Option<String>,
    /// ICE servers (STUN + TURN) the SFU advertises to clients.  Empty
    /// means "use whatever defaults the client ships with".  Clients
    /// MUST treat this as authoritative when present — running the
    /// TURN credential lifecycle from the SFU lets the host application
    /// rotate keys without redeploying clients.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(as = "Option<Vec<IceServer>>", optional)]
    pub ice_servers: Vec<IceServer>,
}

/// One ICE server entry as understood by browser `RTCConfiguration` —
/// mirrors the WebIDL shape so clients can pass it through unchanged.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct IceServer {
    /// One or more URLs (`stun:`, `turn:`, `turns:`).
    pub urls: Vec<String>,
    /// TURN username, when applicable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub username: Option<String>,
    /// TURN long-term credential, when applicable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub credential: Option<String>,
}

fn default_mode() -> String {
    "mesh".to_string()
}
fn default_fallback() -> String {
    "mesh".to_string()
}
fn default_max_mesh() -> u32 {
    4
}

impl Default for SfuConfig {
    fn default() -> Self {
        Self {
            mode: default_mode(),
            designated_peer: None,
            fallback: default_fallback(),
            max_mesh_participants: default_max_mesh(),
            sfu_peers: Vec::new(),
            max_participants_per_node: None,
            preferred_sfu_did: None,
            ice_servers: Vec::new(),
        }
    }
}

/// Snapshot of an SFU room, exposed over the WS RPC API.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuRoomInfo {
    pub neighbourhood_url: String,
    pub room_name: String,
    #[ts(type = "number")]
    pub participant_count: usize,
    pub participants: Vec<SfuParticipantInfo>,
    #[ts(type = "number")]
    pub created_at_ms: u64,
}

/// One participant of an [`SfuRoomInfo`]. The SDK names it
/// `SfuRoomParticipantInfo`: its `SfuParticipantInfo` carries a live
/// `MediaStream`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "SfuRoomParticipantInfo")]
pub struct SfuParticipantInfo {
    pub agent_did: String,
    pub has_audio: bool,
    pub has_video: bool,
    pub is_active_speaker: bool,
}

/// Maps one outbound SDP m-line to the originating participant's DID.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TrackMapEntry {
    /// The SDP mid (media-line identifier) in the offer.
    pub mid: String,
    /// DID of the participant whose media this m-line carries.
    pub agent_did: String,
    /// `"audio"` or `"video"`.
    pub media_kind: String,
}

/// Server-pushed SDP offer delivered to one specific participant when
/// the SFU needs them to renegotiate — typically because a new peer
/// joined the same room and the relay now has additional outbound
/// tracks to forward.  The client applies this as a remote offer,
/// generates an answer, and posts the answer via
/// `sfu.callAnswerServerOffer`.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuCallRenegotiationOffer {
    /// DID of the participant this offer is addressed to.  Used by the
    /// events_ws fanout to filter per-user.
    pub target_did: String,
    pub neighbourhood_url: String,
    pub room_name: String,
    /// JSON-encoded `RTCSessionDescriptionInit` for the new offer.
    pub sdp_offer: String,
    /// Mid-to-DID attribution for newly added outbound m-lines in this
    /// offer.  The client uses these to correlate incoming tracks to
    /// participant DIDs (arrival order alone cannot guarantee this —
    /// HashMap iteration order on the server makes both stream_mapping
    /// and m-line ordering non-deterministic).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[ts(as = "Option<Vec<TrackMapEntry>>", optional)]
    pub track_mapping: Vec<TrackMapEntry>,
}

/// Internal payload: the SFU event loop emits a fresh SDP offer for a
/// *pipe-bound* renegotiation (its local end of an inter-SFU pipe
/// gained outbound tracks).  The cascade router translates the topic
/// publish into a `CascadeSignal::PipeOffer` and ships it via gossip
/// to `remote_did`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SfuPipeRenegotiationOffer {
    /// DID of the remote SFU node to send this offer to.
    pub remote_did: String,
    /// String form of the [`crate::sfu::room::RoomId`].
    pub room_id: String,
    /// JSON-encoded `RTCSessionDescriptionInit`.
    pub sdp_offer: String,
}

/// Internal payload: the SFU event loop emits an SDP answer for a
/// pipe-renegotiation offer it received and applied.  The cascade
/// router translates the publish into a `CascadeSignal::PipeAnswer`
/// and ships it via gossip back to `remote_did`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SfuPipeRenegotiationAnswer {
    /// DID of the remote SFU node to send this answer to.
    pub remote_did: String,
    /// String form of the [`crate::sfu::room::RoomId`].
    pub room_id: String,
    /// JSON-encoded `RTCSessionDescriptionInit`.
    pub sdp_answer: String,
}

/// Server-pushed event telling a participant to leave their current
/// SFU node and rejoin on `target_did`.  The cascade rebalancer
/// publishes this when it detects a significant load imbalance across
/// the cluster.  The client should call `leave()`, set the connected
/// node DID to `target_did`, then call `join()` — the same flow as
/// cascade failover.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuMigrateEvent {
    /// DID of the participant being migrated.  Used by the events_ws
    /// fanout to filter per-user.
    pub target_did: String,
    pub neighbourhood_url: String,
    pub room_name: String,
    /// DID of the SFU node the participant should reconnect to.
    pub migrate_to_did: String,
}

/// Data channel message relayed through the SFU, published on the
/// `sfu-data` event to the room's members.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SfuDataMessage {
    /// DID of the participant who sent the data.
    pub sender_did: String,
    pub neighbourhood_url: String,
    pub room_name: String,
    /// Label of the data channel.
    pub channel_label: String,
    /// True when the payload is binary (base64-encoded in `data`).
    pub binary: bool,
    /// The payload — UTF-8 text or base64-encoded binary.
    pub data: String,
}

impl SfuDataMessage {
    /// Encode `data` as the wire form: base64 when `binary`, else UTF-8
    /// text (lossy).
    pub fn new(
        sender_did: String,
        neighbourhood_url: String,
        room_name: String,
        channel_label: String,
        binary: bool,
        data: &[u8],
    ) -> Self {
        let data = if binary {
            base64::Engine::encode(&base64::engine::general_purpose::STANDARD, data)
        } else {
            String::from_utf8_lossy(data).into_owned()
        };
        Self {
            sender_did,
            neighbourhood_url,
            room_name,
            channel_label,
            binary,
            data,
        }
    }
}

/// Result of a `call_join` — SDP answer + optional cascade redirect +
/// stream mapping for the joining peer.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CallSessionInfo {
    pub room_name: String,
    pub neighbourhood_url: String,
    pub participant_id: String,
    pub sdp_answer: String,
    /// When set, the join was not served here: the caller should join this
    /// DID's SFU node instead (cascade load redirect). Only a `callJoin` with
    /// `acceptRedirect` gets one.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub redirect_to: Option<String>,
    /// Roster of existing participants at join time, format:
    /// `"participantId:did"` (local) or `"remote-did:did"` (cascade).
    /// For track-to-DID attribution, use the `track_mapping` field on
    /// each renegotiation offer — the SDP mids are assigned there.
    pub stream_mapping: Vec<String>,
}
