//! Public, serde-only types exposed by the SFU service.
//!
//! These previously lived as juniper GraphQL types in `graphql_types.rs`.
//! After the GraphQL → WS RPC migration on dev they are plain serde
//! structs serialised straight to the WebSocket reply.

use serde::{Deserialize, Serialize};

/// Social DNA SFU configuration for a neighbourhood.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SfuConfig {
    /// `"mesh"` | `"designated"` | `"gateway"` | `"cascaded"`
    #[serde(default = "default_mode")]
    pub mode: String,
    /// DID of the designated SFU peer (only used when `mode = "designated"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub designated_peer: Option<String>,
    /// Fallback mode when SFU is unavailable.
    #[serde(default = "default_fallback")]
    pub fallback: String,
    /// Maximum participants before mesh is degraded.
    #[serde(default = "default_max_mesh")]
    pub max_mesh_participants: u32,
    /// DIDs of SFU peers in cascaded mode.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sfu_peers: Vec<String>,
    /// Max participants per SFU node in cascaded mode.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_participants_per_node: Option<u32>,
    /// ICE servers (STUN + TURN) the SFU advertises to clients.  Empty
    /// means "use whatever defaults the client ships with".  Clients
    /// MUST treat this as authoritative when present — running the
    /// TURN credential lifecycle from the SFU lets the host application
    /// rotate keys without redeploying clients.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub ice_servers: Vec<IceServer>,
}

/// One ICE server entry as understood by browser `RTCConfiguration` —
/// mirrors the WebIDL shape so clients can pass it through unchanged.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IceServer {
    /// One or more URLs (`stun:`, `turn:`, `turns:`).
    pub urls: Vec<String>,
    /// TURN username, when applicable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub username: Option<String>,
    /// TURN long-term credential, when applicable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
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
            ice_servers: Vec::new(),
        }
    }
}

/// Snapshot of an SFU room, exposed over the WS RPC API.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SfuRoomInfo {
    pub neighbourhood_url: String,
    pub room_name: String,
    pub participant_count: usize,
    pub participants: Vec<SfuParticipantInfo>,
    pub created_at_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SfuParticipantInfo {
    pub agent_did: String,
    pub has_audio: bool,
    pub has_video: bool,
    pub is_active_speaker: bool,
}

/// Maps one outbound SDP m-line to the originating participant's DID.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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

/// Result of a `call_join` — SDP answer + optional cascade redirect +
/// stream mapping for the joining peer.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CallSessionInfo {
    pub room_name: String,
    pub neighbourhood_url: String,
    pub participant_id: String,
    pub sdp_answer: String,
    /// When set, the joining peer should reconnect to this DID's SFU
    /// node (cascaded mode load redirect).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub redirect_to: Option<String>,
    /// Roster of existing participants at join time, format:
    /// `"participantId:did"` (local) or `"remote-did:did"` (cascade).
    /// For track-to-DID attribution, use the `track_mapping` field on
    /// each renegotiation offer — the SDP mids are assigned there.
    pub stream_mapping: Vec<String>,
}
