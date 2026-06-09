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
    /// Stream-to-participant DID mapping, format: `"participantId:did"`.
    pub stream_mapping: Vec<String>,
}
