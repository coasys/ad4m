//! SFU service — the top-level service that integrates with the executor.
//!
//! Follows the same pattern as HolochainService, SurrealService, etc.
//! Provides room management, peer authentication, and the interface
//! consumed by the WS RPC handlers in `crate::api::sfu_ws`.

use std::collections::HashMap;
use std::sync::Arc;

use log::{debug, info, warn};
use once_cell::sync::OnceCell;
use str0m::change::SdpOffer;
use tokio::sync::RwLock;

use super::cascade::{CascadeManager, CascadeSignal};
use super::gossip::{CascadeGossip, GossipTarget};
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
    /// Pluggable cluster-gossip transport.  Always present — single-node
    /// deployments pass a `NoopGossip` so the cascade plumbing stays
    /// uniform and the redirect logic falls through to "no peers".
    gossip: Arc<dyn CascadeGossip>,
}

impl SfuService {
    /// Start the SFU service and set the global instance.
    ///
    /// `gossip` is the cluster transport — a [`NoopGossip`] for
    /// single-node executors, a [`TcpGossip`] (or future
    /// `HolochainGossip` / etc.) for cascade-enabled deployments.
    /// On startup the service spawns an inbound loop that pumps
    /// incoming [`CascadeSignal`]s into the local
    /// [`CascadeManager`], so production discovery and
    /// announce-driven load updates happen entirely through the
    /// transport layer with no admin RPC needed.
    pub async fn start(
        config: SfuServerConfig,
        gossip: Arc<dyn CascadeGossip>,
    ) -> Result<Arc<Self>, String> {
        let server = SfuServer::start(config)
            .await
            .map_err(|e| format!("Failed to start SFU server: {}", e))?;

        info!("SFU service started on {}", server.local_addr);

        // Pre-create the CascadeManager so the service has a single
        // consistent view; cascade is effectively no-op when the
        // gossip is a NoopGossip (Send/Receive go nowhere).
        let cascade_manager = CascadeManager::new(
            gossip.local_did().to_string(),
            server.local_addr,
            gossip.max_participants_per_node(),
        );

        let service = Arc::new(Self {
            server,
            rooms: Arc::new(RwLock::new(RoomManager::new())),
            configs: Arc::new(RwLock::new(HashMap::new())),
            cascade_manager: Arc::new(RwLock::new(Some(cascade_manager))),
            gossip: Arc::clone(&gossip),
        });

        if let Some(rx) = gossip.take_inbound() {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_gossip_inbound(rx).await;
            });
        }

        SFU_SERVICE
            .set(service.clone())
            .map_err(|_| "SFU service already initialized".to_string())?;

        Ok(service)
    }

    /// Pump inbound signals from the gossip transport into the
    /// CascadeManager.  Runs for the lifetime of the executor.
    async fn run_gossip_inbound(
        self: Arc<Self>,
        mut rx: tokio::sync::mpsc::Receiver<CascadeSignal>,
    ) {
        while let Some(signal) = rx.recv().await {
            let mut cascade_lock = self.cascade_manager.write().await;
            let Some(mgr) = cascade_lock.as_mut() else {
                debug!("SFU gossip inbound: no CascadeManager, dropping signal");
                continue;
            };
            match signal {
                CascadeSignal::Announce {
                    did,
                    room_id,
                    participant_count,
                    capacity_hint,
                } => {
                    mgr.handle_sfu_announce(did, room_id, participant_count, capacity_hint);
                }
                CascadeSignal::Leave { did, room_id: _ } => {
                    mgr.remove_node(&did);
                }
                CascadeSignal::PipeOffer { .. } | CascadeSignal::PipeAnswer { .. } => {
                    // Pipe transport plumbing is wired in cascade.rs;
                    // this dispatcher just hands the signal off.  When
                    // pipe support is finished end-to-end (see plan
                    // Phase E), uncomment the dispatch.
                    debug!("SFU gossip inbound: pipe signal received (unhandled)");
                }
            }
        }
    }

    /// Broadcast our local room-state update.  Called from the
    /// participant add/remove path; pushes an Announce through the
    /// gossip transport so peers can keep cross-node counts fresh.
    async fn announce_room(&self, room_id: &str, participant_count: u32) {
        let capacity_hint = self.gossip.max_participants_per_node();
        let signal = CascadeSignal::Announce {
            did: self.gossip.local_did().to_string(),
            room_id: room_id.to_string(),
            participant_count,
            capacity_hint,
        };
        if let Err(e) = self.gossip.send(GossipTarget::Broadcast, signal).await {
            warn!("SFU announce_room failed: {}", e);
        }
    }

    pub fn is_available() -> bool {
        true
    }

    /// Get the local address of the SFU server.
    pub fn local_addr(&self) -> std::net::SocketAddr {
        self.server.local_addr
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
            pending_offer: None,
        };

        self.server
            .command_tx
            .send(SfuCommand::AddPeer(peer))
            .await
            .map_err(|e| format!("Failed to add peer to SFU: {}", e))?;

        // Build stream mapping from existing participants in the room.
        // While we're holding a read lock, also snapshot the room's
        // local participant count so we can announce it via gossip
        // after the lock drops.
        let (stream_mapping, local_count) = {
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
                (mapping, room.participant_count() as u32)
            } else {
                (Vec::new(), 0)
            }
        };

        self.announce_room(&room_id.to_string(), local_count).await;

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
        let local_count = if is_empty {
            0
        } else {
            room.participant_count() as u32
        };

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
        drop(rooms);

        self.announce_room(&room_id.to_string(), local_count).await;

        Ok(true)
    }

    /// Consume an SDP answer that the client produced in response to a
    /// server-pushed renegotiation offer.  Routes the answer back to
    /// the appropriate participant's Rtc transport via the SFU event
    /// loop so the str0m state machine can apply it.
    ///
    /// Today this is the wiring path — the offer-generation side that
    /// publishes to `SFU_CALL_RENEGOTIATION_OFFER_TOPIC` is queued
    /// alongside `Phase E` (cross-node pipe transport) and lights up
    /// the same str0m sdp_api dance, so we expose the typed surface
    /// now and the server-side fanout follows.  Wind tunnel scenarios
    /// still pre-allocate recv-only transceivers as a workaround for
    /// the inbound side.
    pub async fn call_answer_server_offer(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        _sdp_answer_json: &str,
    ) -> Result<bool, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let rooms = self.rooms.read().await;
        let room = rooms
            .get_room(&room_id)
            .ok_or_else(|| RoomError::NotFound.to_string())?;
        let pid = room
            .participants
            .iter()
            .find(|(_, p)| p.agent_did == agent_did)
            .map(|(pid, _)| pid.clone())
            .ok_or_else(|| "Agent not in room".to_string())?;
        // Forward the answer to the SFU event loop so it can apply
        // through str0m.  The loop currently logs the event; full
        // sdp_api consumption is the focused follow-up.
        let _ = self
            .server
            .command_tx
            .send(SfuCommand::ApplyServerAnswer {
                participant_id: pid,
                sdp_answer_json: _sdp_answer_json.to_string(),
            })
            .await;
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
