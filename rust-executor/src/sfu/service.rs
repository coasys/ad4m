//! SFU service — the top-level service that integrates with the executor.
//!
//! Follows the same pattern as HolochainService, SurrealService, etc.
//! Provides room management, peer authentication, and the interface
//! consumed by the WS RPC handlers in `crate::api::sfu_ws`.

use std::collections::HashMap;
use std::sync::Arc;

use log::info;
use once_cell::sync::OnceCell;
use str0m::change::SdpOffer;
use tokio::sync::RwLock;

use super::cascade::CascadeManager;
use super::room::{ParticipantId, RoomError, RoomId, RoomManager, SfuRoom};
use super::server::{SfuCommand, SfuPeer, SfuServer, SfuServerConfig};

/// Global SFU service instance.
static SFU_SERVICE: OnceCell<Arc<SfuService>> = OnceCell::new();

/// Get the global SFU service instance, if initialized.
pub fn get_sfu_service() -> Option<Arc<SfuService>> {
    SFU_SERVICE.get().cloned()
}

// Public types live in `super::types`.  Re-exported from the SFU module
// root so external callers can `use crate::sfu::SfuConfig` etc.
use super::types::{CallSessionInfo, SfuConfig, SfuParticipantInfo, SfuRoomInfo};

/// The global SFU service, analogous to HolochainService.
pub struct SfuService {
    server: SfuServer,
    rooms: Arc<RwLock<RoomManager>>,
    /// Maps neighbourhood URLs to their SFU configuration (from Social DNA).
    configs: Arc<RwLock<HashMap<String, SfuConfig>>>,
    /// Cascade manager for multi-node SFU deployments.
    cascade_manager: Arc<RwLock<Option<CascadeManager>>>,
}

impl SfuService {
    /// Start the SFU service and set the global instance.
    pub async fn start(config: SfuServerConfig) -> Result<Arc<Self>, String> {
        let server = SfuServer::start(config)
            .await
            .map_err(|e| format!("Failed to start SFU server: {}", e))?;

        info!("SFU service started on {}", server.local_addr);

        let service = Arc::new(Self {
            server,
            rooms: Arc::new(RwLock::new(RoomManager::new())),
            configs: Arc::new(RwLock::new(HashMap::new())),
            cascade_manager: Arc::new(RwLock::new(None)),
        });

        SFU_SERVICE
            .set(service.clone())
            .map_err(|_| "SFU service already initialized".to_string())?;

        Ok(service)
    }

    pub fn is_available() -> bool {
        true
    }

    /// Get the local address of the SFU server.
    pub fn local_addr(&self) -> std::net::SocketAddr {
        self.server.local_addr
    }

    /// Enable cascade mode and seed the CascadeManager with a set of
    /// known peer nodes.  Wind tunnel / admin entry point — production
    /// cascade discovery is via the gossip layer on top of the
    /// neighbourhood DNA.
    ///
    /// `peers` is a list of `(did, addr)` tuples for every other SFU
    /// node in the cluster.  Each is registered as a `SfuNodeInfo` for
    /// every active room (and the empty `""` room as a catch-all when
    /// no rooms exist yet).
    pub async fn enable_cascade(
        &self,
        local_did: String,
        max_participants_per_node: u32,
        peers: Vec<(String, std::net::SocketAddr)>,
    ) -> Result<(), String> {
        use super::cascade::SfuNodeInfo;
        let mut cascade_lock = self.cascade_manager.write().await;
        let mgr = cascade_lock.get_or_insert_with(|| {
            super::cascade::CascadeManager::new(
                local_did.clone(),
                self.server.local_addr,
                max_participants_per_node,
            )
        });

        // Catch-all room id "" so pick_redirect_node sees these peers
        // even before the room is created.
        let known = &mut mgr.known_nodes_mut();
        let bucket = known.entry(String::new()).or_default();
        // Re-seed: drop the existing entries (so an enable_cascade with
        // an empty peer list correctly partitions the node from its
        // peers) and rewrite.
        bucket.clear();
        for (did, _addr) in &peers {
            if *did == local_did {
                continue;
            }
            bucket.insert(
                did.clone(),
                SfuNodeInfo {
                    did: did.clone(),
                    participant_count: 0,
                    capacity_hint: max_participants_per_node,
                },
            );
        }
        Ok(())
    }

    /// Push a participant-count update for a remote SFU node.  In
    /// production this happens via the gossip announce path; for the
    /// wind tunnel's static cluster we expose it as an admin RPC
    /// (`sfu.cascadeAnnounce`) so the harness can keep each node's
    /// view of its peers fresh without standing up a gossip layer.
    pub async fn cascade_announce(
        &self,
        remote_did: String,
        room_id: String,
        participant_count: u32,
    ) -> Result<(), String> {
        let mut cascade_lock = self.cascade_manager.write().await;
        let mgr = cascade_lock
            .as_mut()
            .ok_or_else(|| "cascade not enabled on this node".to_string())?;
        let capacity_hint = mgr.max_participants_per_node();
        mgr.handle_sfu_announce(remote_did, room_id, participant_count, capacity_hint);
        Ok(())
    }

    // ---- Room management ----

    /// Create or get an SFU room for a neighbourhood call.
    pub async fn start_room(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
    ) -> Result<SfuRoomInfo, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let mut rooms = self.rooms.write().await;

        // Get config for max participants
        let configs = self.configs.read().await;
        let max = configs
            .get(neighbourhood_url)
            .map(|c| c.max_mesh_participants as usize * 4); // SFU supports ~4x mesh limit

        rooms
            .create_room(room_id.clone(), max)
            .map_err(|e| e.to_string())?;

        let room = rooms.get_room(&room_id).unwrap();
        Ok(self.room_to_info(room))
    }

    /// Stop an SFU room, disconnecting all participants.
    pub async fn stop_room(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
    ) -> Result<bool, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let mut rooms = self.rooms.write().await;

        let participant_ids = rooms.destroy_room(&room_id).map_err(|e| e.to_string())?;

        // Send remove commands for all participants
        for pid in participant_ids {
            let _ = self
                .server
                .command_tx
                .send(SfuCommand::RemovePeer(pid))
                .await;
        }

        Ok(true)
    }

    /// List all active rooms.
    pub async fn list_rooms(&self) -> Vec<SfuRoomInfo> {
        let rooms = self.rooms.read().await;
        rooms
            .list_rooms()
            .iter()
            .map(|r| self.room_to_info(r))
            .collect()
    }

    // ---- Call join/leave ----

    /// Join a call. Performs DID authentication check, creates an Rtc instance,
    /// processes the SDP offer, and returns the answer.
    pub async fn call_join(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        sdp_offer_json: &str,
        is_neighbourhood_member: bool,
    ) -> Result<CallSessionInfo, String> {
        // DID authentication: verify neighbourhood membership
        if !is_neighbourhood_member {
            return Err(RoomError::NotMember.to_string());
        }

        let room_id = RoomId::new(neighbourhood_url, room_name);
        let pid = ParticipantId::next();

        // Check cascade redirect before accepting the participant
        {
            let cascade = self.cascade_manager.read().await;
            if let Some(ref mgr) = *cascade {
                let rooms = self.rooms.read().await;
                let local_count = rooms
                    .get_room(&room_id)
                    .map(|r| r.participant_count() as u32)
                    .unwrap_or(0);
                if let Some(node) = mgr.pick_redirect_node(&room_id.to_string(), local_count) {
                    return Ok(CallSessionInfo {
                        room_name: room_name.to_string(),
                        neighbourhood_url: neighbourhood_url.to_string(),
                        participant_id: String::new(),
                        sdp_answer: String::new(),
                        redirect_to: Some(node.did.clone()),
                        stream_mapping: Vec::new(),
                    });
                }
            }
        }

        // Ensure room exists
        {
            let mut rooms = self.rooms.write().await;
            let configs = self.configs.read().await;
            let max = configs
                .get(neighbourhood_url)
                .map(|c| c.max_mesh_participants as usize * 4);

            rooms.create_room(room_id.clone(), max).ok(); // idempotent

            let room = rooms
                .get_room_mut(&room_id)
                .ok_or_else(|| RoomError::NotFound.to_string())?;

            room.add_participant(pid.clone(), agent_did.to_string())
                .map_err(|e| e.to_string())?;
        }

        // Parse SDP offer and create Rtc instance
        let offer: SdpOffer = serde_json::from_str(sdp_offer_json)
            .map_err(|e| format!("Invalid SDP offer: {}", e))?;

        let (rtc, sdp_answer) = SfuServer::create_rtc_for_offer(offer, self.server.local_addr)?;

        // Create the SFU peer and send it to the event loop
        let peer = SfuPeer {
            id: pid.clone(),
            room_id: room_id.clone(),
            agent_did: agent_did.to_string(),
            rtc,
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
        };

        self.server
            .command_tx
            .send(SfuCommand::AddPeer(peer))
            .await
            .map_err(|e| format!("Failed to add peer to SFU: {}", e))?;

        // Build stream mapping from existing participants in the room
        let stream_mapping = {
            let rooms = self.rooms.read().await;
            if let Some(room) = rooms.get_room(&room_id) {
                let mut mapping: Vec<String> = room
                    .participants
                    .values()
                    .filter(|p| p.id != pid)
                    .map(|p| format!("{}:{}", p.id.0, p.agent_did))
                    .collect();
                for (did, _remote) in &room.remote_participants {
                    mapping.push(format!("remote-{}:{}", did, did));
                }
                mapping
            } else {
                Vec::new()
            }
        };

        Ok(CallSessionInfo {
            room_name: room_name.to_string(),
            neighbourhood_url: neighbourhood_url.to_string(),
            participant_id: pid.to_string(),
            sdp_answer,
            redirect_to: None,
            stream_mapping,
        })
    }

    /// Leave a call.
    pub async fn call_leave(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
    ) -> Result<bool, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let mut rooms = self.rooms.write().await;

        let room = rooms
            .get_room_mut(&room_id)
            .ok_or_else(|| RoomError::NotFound.to_string())?;

        // Find participant by DID
        let pid = room
            .participants
            .iter()
            .find(|(_, p)| p.agent_did == agent_did)
            .map(|(pid, _)| pid.clone())
            .ok_or_else(|| "Agent not in room".to_string())?;

        let is_empty = room.remove_participant(&pid);

        // Notify event loop
        let _ = self
            .server
            .command_tx
            .send(SfuCommand::RemovePeer(pid))
            .await;

        // Clean up empty room
        if is_empty {
            rooms.destroy_room(&room_id).ok();
        }

        Ok(true)
    }

    /// Set the quality preference for a participant's received video streams.
    pub async fn call_set_quality_preference(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        preference: &str,
    ) -> Result<bool, String> {
        // Validate preference
        match preference {
            "high" | "medium" | "low" | "auto" => {}
            other => {
                return Err(format!(
                    "Invalid quality preference: '{}'. Must be 'high', 'medium', 'low', or 'auto'",
                    other
                ))
            }
        }

        let room_id = RoomId::new(neighbourhood_url, room_name);
        let rooms = self.rooms.read().await;

        let room = rooms
            .get_room(&room_id)
            .ok_or_else(|| RoomError::NotFound.to_string())?;

        // Find participant by DID
        let pid = room
            .participants
            .iter()
            .find(|(_, p)| p.agent_did == agent_did)
            .map(|(pid, _)| pid.clone())
            .ok_or_else(|| "Agent not in room".to_string())?;

        self.server
            .command_tx
            .send(SfuCommand::SetQualityPreference {
                participant_id: pid,
                preference: preference.to_string(),
            })
            .await
            .map_err(|e| format!("Failed to send quality preference command: {}", e))?;

        Ok(true)
    }

    // ---- SFU configuration (Social DNA) ----

    /// Get the SFU config for a neighbourhood.
    pub async fn get_config(&self, neighbourhood_url: &str) -> SfuConfig {
        let configs = self.configs.read().await;
        configs.get(neighbourhood_url).cloned().unwrap_or_default()
    }

    /// Set the SFU config for a neighbourhood (from Social DNA).
    pub async fn set_config(
        &self,
        neighbourhood_url: &str,
        config: SfuConfig,
    ) -> Result<(), String> {
        // Validate mode
        match config.mode.as_str() {
            "gateway" | "designated" | "mesh" | "cascaded" => {}
            other => {
                return Err(format!(
                "Invalid SFU mode: '{}'. Must be 'gateway', 'designated', 'mesh', or 'cascaded'",
                other
            ))
            }
        }

        // Validate designated peer has a DID
        if config.mode == "designated" && config.designated_peer.is_none() {
            return Err("Designated mode requires a designatedPeer DID".to_string());
        }

        let mut configs = self.configs.write().await;
        configs.insert(neighbourhood_url.to_string(), config);
        Ok(())
    }

    /// Get the designated SFU peer DID for a neighbourhood.
    pub async fn sfu_peer_for_neighbourhood(&self, neighbourhood_url: &str) -> Option<String> {
        let configs = self.configs.read().await;
        configs
            .get(neighbourhood_url)
            .and_then(|c| match c.mode.as_str() {
                "designated" => c.designated_peer.clone(),
                "gateway" => Some("gateway".to_string()), // Sentinel — caller resolves gateway DID
                _ => None,
            })
    }

    /// Get the SFU peer DIDs for a neighbourhood (cascaded mode returns multiple).
    pub async fn sfu_peers_for_neighbourhood(&self, neighbourhood_url: &str) -> Vec<String> {
        let configs = self.configs.read().await;
        match configs.get(neighbourhood_url) {
            Some(c) if c.mode == "cascaded" => c.sfu_peers.clone(),
            Some(c) if c.mode == "designated" => c.designated_peer.iter().cloned().collect(),
            Some(c) if c.mode == "gateway" => vec!["gateway".to_string()],
            _ => vec![],
        }
    }

    /// Shut down the SFU service.
    pub async fn shutdown(&self) {
        let _ = self.server.command_tx.send(SfuCommand::Shutdown).await;
        info!("SFU service shut down");
    }

    // ---- Internal helpers ----

    fn room_to_info(&self, room: &SfuRoom) -> SfuRoomInfo {
        SfuRoomInfo {
            neighbourhood_url: room.id.neighbourhood_url.clone(),
            room_name: room.id.room_name.clone(),
            participant_count: room.participant_count(),
            participants: room
                .participants
                .values()
                .map(|p| SfuParticipantInfo {
                    agent_did: p.agent_did.clone(),
                    has_audio: p.has_audio,
                    has_video: p.has_video,
                    is_active_speaker: p.is_active_speaker,
                })
                .collect(),
            created_at_ms: room.created_at.elapsed().as_millis() as u64,
        }
    }
}
