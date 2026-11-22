//! SFU service — the top-level service that integrates with the executor.
//!
//! Follows the same pattern as HolochainService, SurrealService, etc.
//! Provides room management, peer authentication, and the interface
//! consumed by the WS RPC handlers in `crate::api::sfu_ws`.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use log::{debug, info, warn};
use once_cell::sync::OnceCell;
use str0m::change::SdpOffer;
use tokio::sync::RwLock;

use super::cascade::{CascadeManager, CascadeSignal};
use super::config_store;
use super::gossip::{CascadeGossip, GossipTarget};
use super::reachability::SfuReachability;
use super::room::{ParticipantId, RoomError, RoomId, RoomManager, SfuRoom};
use super::server::{SfuCommand, SfuPeer, SfuServer, SfuServerConfig};
use super::types::{SfuMigrateEvent, TrackMapEntry};

/// Global SFU service instance.
static SFU_SERVICE: OnceCell<Arc<SfuService>> = OnceCell::new();

/// Most rooms one node holds at once. A join or `startRoom` that would open
/// another is refused.
pub const MAX_ROOMS_PER_NODE: usize = 256;
/// Most rooms one neighbourhood holds on this node, so one neighbourhood's
/// owner cannot use up the node's allowance.
pub const MAX_ROOMS_PER_NEIGHBOURHOOD: usize = 16;
/// An empty room — one `startRoom` opened that nobody joined — closes after
/// this long. A room whose last participant leaves closes at once.
pub const EMPTY_ROOM_TTL: Duration = Duration::from_secs(600);
/// Most calls one DID is in at once on this node. Within one call, each
/// device is a session of its own, up to `room::MAX_DEVICES_PER_DID`.
pub const MAX_CALLS_PER_DID: usize = 4;
/// Longest device id a join may name: an opaque id, not a description.
const MAX_DEVICE_ID_LEN: usize = 128;
/// Upper bound on `maxMeshParticipants`. A room's SFU capacity is four times
/// this, so it also bounds every room.
pub const MAX_MESH_PARTICIPANTS: u32 = 32;

/// Get the global SFU service instance, if initialized.
pub fn get_sfu_service() -> Option<Arc<SfuService>> {
    SFU_SERVICE.get().cloned()
}

// Public types live in `super::types`.  Re-exported from the SFU module
// root so external callers can `use crate::sfu::SfuConfig` etc.
use super::types::{CallSessionInfo, SfuConfig, SfuParticipantInfo, SfuRoomInfo};

/// Compute the max quality preference across a set of values.
///
/// Quality ordering: high/auto > medium > low.
/// Returns "high" when the iterator yields no values.
fn max_quality_preference<'a>(values: impl Iterator<Item = &'a String>) -> String {
    fn rank(pref: &str) -> u8 {
        match pref {
            "low" => 0,
            "medium" => 1,
            _ => 2, // "high", "auto", unknown → highest tier
        }
    }
    let max_rank = values.map(|v| rank(v)).max().unwrap_or(2);
    match max_rank {
        0 => "low",
        1 => "medium",
        _ => "high",
    }
    .to_string()
}

/// The policy `sfu.setConfig` stores for `config`, or why it refuses it.
///
/// Only `mode` and `maxMeshParticipants` are stored (`fallback` is not:
/// nothing reads it). The other fields describe a deployment (executor
/// flags), and ICE credentials must not go into a link every member syncs,
/// so a config that sets them is refused rather than stored in part.
fn call_policy(config: SfuConfig) -> Result<config_store::CallPolicy, String> {
    match config.mode.as_str() {
        "gateway" | "designated" | "mesh" | "cascaded" => {}
        other => {
            return Err(format!(
                "Invalid SFU mode: '{}'. Must be 'gateway', 'designated', 'mesh', or 'cascaded'",
                other
            ))
        }
    }

    let executor_fields: Vec<&str> = [
        ("designatedPeer", config.designated_peer.is_some()),
        ("sfuPeers", !config.sfu_peers.is_empty()),
        (
            "maxParticipantsPerNode",
            config.max_participants_per_node.is_some(),
        ),
        ("preferredSfuDid", config.preferred_sfu_did.is_some()),
        ("iceServers", !config.ice_servers.is_empty()),
    ]
    .into_iter()
    .filter_map(|(name, set)| set.then_some(name))
    .collect();
    if !executor_fields.is_empty() {
        return Err(format!(
            "{} cannot be stored in the neighbourhood: SFU nodes and capacity are \
             executor settings (--sfu-* flags), and ICE servers are passed to createSession",
            executor_fields.join(", ")
        ));
    }

    // A room's SFU capacity is four times this, so it bounds rooms too.
    if !(2..=MAX_MESH_PARTICIPANTS).contains(&config.max_mesh_participants) {
        return Err(format!(
            "maxMeshParticipants must be between 2 and {}",
            MAX_MESH_PARTICIPANTS
        ));
    }

    Ok(config_store::CallPolicy {
        mode: config.mode,
        max_mesh_participants: config.max_mesh_participants,
    })
}

/// Why [`SfuService::call_join`] refused a join.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallJoinError {
    /// The join's session key was replaced on its device by a later join
    /// (another tab): the session must stop, not rebuild.
    Replaced(String),
    /// The caller's token stopped holding (revoked, removed or expired)
    /// before the join reached the room: nothing changed.
    Unauthorized(String),
    /// Any other refusal or failure.
    Failed(String),
}

impl std::fmt::Display for CallJoinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallJoinError::Replaced(m)
            | CallJoinError::Unauthorized(m)
            | CallJoinError::Failed(m) => f.write_str(m),
        }
    }
}

impl From<String> for CallJoinError {
    fn from(message: String) -> Self {
        CallJoinError::Failed(message)
    }
}

impl From<RoomError> for CallJoinError {
    fn from(e: RoomError) -> Self {
        match e {
            RoomError::Replaced => CallJoinError::Replaced(e.to_string()),
            e => CallJoinError::Failed(e.to_string()),
        }
    }
}

/// The global SFU service, analogous to HolochainService.
pub struct SfuService {
    server: SfuServer,
    rooms: Arc<RwLock<RoomManager>>,
    /// Cascade manager for multi-node SFU deployments.
    cascade_manager: Arc<RwLock<CascadeManager>>,
    /// Pluggable cluster-gossip transport.  Always present — single-node
    /// deployments pass a `NoopGossip` so the cascade plumbing stays
    /// uniform and the redirect logic falls through to "no peers".
    gossip: Arc<dyn CascadeGossip>,
    /// Local subscriber quality preferences tracked for cascade
    /// aggregation.  Maps `room_id_str → (participant_id → preference)`:
    /// one per session, so two devices of one agent each count.
    /// When any subscriber changes preference, the aggregate (max) gets
    /// gossiped to pipe peers so the sender node forwards the correct
    /// simulcast layer.
    local_quality_preferences: RwLock<HashMap<String, HashMap<String, String>>>,
    /// Result of the startup reachability probe.  `sfu.status` reports it;
    /// a call's `auto` topology uses this SFU only when it is public.
    reachability: SfuReachability,
    /// DID of the cascade node that takes redirected joins first
    /// (`--sfu-preferred-node`).
    preferred_node: Option<String>,
    /// DIDs of the cascade peers this node was configured with
    /// (`--sfu-cascade-peers`).
    cascade_peer_dids: Vec<String>,
    /// Serialises [`Self::publish_roster`]: a roster read before another
    /// join must not be published after that join's roster.
    roster_order: tokio::sync::Mutex<()>,
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
        let preferred_node = config.preferred_node.clone();
        let cascade_peer_dids = config.cascade_peer_dids.clone();
        let server = SfuServer::start(config)
            .await
            .map_err(|e| format!("Failed to start SFU server: {}", e))?;

        let reachability = server.reachability.clone();

        // Pre-create the CascadeManager so the service has a single
        // consistent view; cascade is effectively no-op when the
        // gossip is a NoopGossip (Send/Receive go nowhere).
        let cascade_manager = CascadeManager::new(
            gossip.local_did().to_string(),
            server.sockets.clone(),
            gossip.max_participants_per_node(),
        );

        let service = Arc::new(Self {
            server,
            rooms: Arc::new(RwLock::new(RoomManager::new())),
            cascade_manager: Arc::new(RwLock::new(cascade_manager)),
            gossip: Arc::clone(&gossip),
            local_quality_preferences: RwLock::new(HashMap::new()),
            reachability,
            preferred_node,
            cascade_peer_dids,
            roster_order: tokio::sync::Mutex::new(()),
        });

        if let Some(rx) = gossip.take_inbound() {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_gossip_inbound(rx).await;
            });
        }

        // Pump the pipe-renegotiation bus → gossip.  Phase F
        // routes media negotiation across the cluster: when the SFU
        // event loop wants to add an outbound track to a pipe-bound
        // peer it publishes the offer to a dedicated topic and waits
        // for the answer to come back on its counterpart.  We bridge
        // both directions to the cascade gossip transport here so the
        // event loop doesn't need to know about CascadeGossip.
        {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_pipe_renegotiation_offer_bridge().await;
            });
        }
        {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_pipe_renegotiation_answer_bridge().await;
            });
        }

        // Cleanup: process dead peer notifications from the server event
        // loop and run periodic stale-node eviction.
        {
            let dead_peer_rx = service
                .server
                .dead_peer_rx
                .lock()
                .expect("dead_peer_rx mutex poisoned")
                .take();
            if let Some(rx) = dead_peer_rx {
                let svc = Arc::clone(&service);
                tokio::spawn(async move {
                    svc.run_cascade_cleanup(rx).await;
                });
            }
        }

        // Periodic heartbeat: re-announce all active rooms every 10s so
        // that cascade peers maintain a fresh view.  Without this,
        // rooms announced during startup get evicted by the 30s stale
        // timeout if no peers join within that window.
        {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_cascade_heartbeat().await;
            });
        }

        SFU_SERVICE
            .set(service.clone())
            .map_err(|_| "SFU service already initialized".to_string())?;

        Ok(service)
    }

    /// Subscribe to the pipe bus's offers and translate
    /// each publish into a `CascadeSignal::PipeOffer` directed at the
    /// remote SFU.  Runs for the lifetime of the executor.
    async fn run_pipe_renegotiation_offer_bridge(self: Arc<Self>) {
        let mut rx = super::pipe_bus::subscribe_offers();
        loop {
            let payload = match rx.recv().await {
                Ok(m) => m,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
            };
            let signal = CascadeSignal::PipeOffer {
                from_did: self.gossip.local_did().to_string(),
                to_did: payload.remote_did.clone(),
                room_id: payload.room_id,
                sdp_offer: payload.sdp_offer,
                track_mapping: payload.track_mapping,
            };
            if let Err(e) = self
                .gossip
                .send(GossipTarget::PeerDid(payload.remote_did), signal)
                .await
            {
                debug!("SFU pipe-reneg offer bridge: gossip send failed: {}", e);
            }
        }
    }

    /// Subscribe to the pipe bus's answers and translate
    /// each publish into a `CascadeSignal::PipeAnswer` directed at the
    /// remote SFU.  Runs for the lifetime of the executor.
    async fn run_pipe_renegotiation_answer_bridge(self: Arc<Self>) {
        let mut rx = super::pipe_bus::subscribe_answers();
        loop {
            let payload = match rx.recv().await {
                Ok(m) => m,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
            };
            let signal = CascadeSignal::PipeAnswer {
                from_did: self.gossip.local_did().to_string(),
                to_did: payload.remote_did.clone(),
                room_id: payload.room_id,
                sdp_answer: payload.sdp_answer,
            };
            if let Err(e) = self
                .gossip
                .send(GossipTarget::PeerDid(payload.remote_did), signal)
                .await
            {
                debug!("SFU pipe-reneg answer bridge: gossip send failed: {}", e);
            }
        }
    }

    /// Pump inbound signals from the gossip transport into the
    /// CascadeManager.  Runs for the lifetime of the executor.
    async fn run_gossip_inbound(
        self: Arc<Self>,
        mut rx: tokio::sync::mpsc::Receiver<CascadeSignal>,
    ) {
        while let Some(signal) = rx.recv().await {
            // Outbound signals produced as a side-effect of processing
            // the inbound one — drained outside the cascade lock so we
            // don't deadlock against the gossip transport.
            let mut outbound: Vec<(GossipTarget, CascadeSignal)> = Vec::new();
            // Pipe-side SFU peers produced when we either initiate or
            // accept a pipe handshake; we hand each one off to the SFU
            // server's event loop via `SfuCommand::AddPeer` once the
            // cascade lock is released.
            let mut new_pipe_peers: Vec<SfuPeer> = Vec::new();
            // Pipes whose remote answered our offer.  Once we know the
            // ParticipantId of the dialer-side pipe peer we route the
            // SDP answer into the SFU event loop the same way client
            // peers' answers flow (`SfuCommand::ApplyServerAnswer`).
            let mut pipe_answers: Vec<(ParticipantId, String)> = Vec::new();
            // Pipes to tear down because the remote sent a Leave.
            let mut pipes_to_drop: Vec<ParticipantId> = Vec::new();
            // Pipe-side renegotiation offers — when a remote SFU
            // resends a PipeOffer for an existing pipe (e.g. it added
            // outbound tracks for a freshly-joined peer), the event
            // loop applies the offer and publishes the answer back
            // through the pipe bus's answer bridge.
            // Tuple: (local pipe participant_id, sdp_offer_json,
            //         remote_did, room_id_str, track_mapping).
            let mut pipe_renegotiation_offers: Vec<(
                ParticipantId,
                String,
                String,
                String,
                Vec<TrackMapEntry>,
            )> = Vec::new();
            // Rooms whose remote sessions changed (a node announced
            // other sessions, left or went stale): their local roster
            // is republished once the cascade lock is released.
            let mut rosters_changed: Vec<String> = Vec::new();
            // Quality preferences to set on pipe peers — collected from
            // QualityPreference gossip messages.
            let mut pipe_quality_prefs: Vec<(ParticipantId, String)> = Vec::new();

            // Resolve local-room state up front for the Announce branch
            // so we never hold the cascade lock + rooms lock at the same
            // time.  Cheap: a single HashMap read.
            let pre_has_room = if let CascadeSignal::Announce { ref room_id, .. } = signal {
                self.local_has_room(room_id).await
            } else {
                false
            };

            {
                let mut mgr = self.cascade_manager.write().await;
                match signal {
                    CascadeSignal::Announce {
                        did,
                        room_id,
                        participant_count,
                        capacity_hint,
                        sessions,
                    } => {
                        let remote_did = did.clone();
                        if mgr.handle_sfu_announce(
                            did,
                            room_id.clone(),
                            participant_count,
                            capacity_hint,
                            sessions,
                        ) {
                            rosters_changed.push(room_id.clone());
                        }

                        // Evict nodes that stopped announcing (>30s stale).
                        // Cheap: one HashMap scan per Announce.
                        let evicted = mgr.evict_stale_nodes(Duration::from_secs(30));
                        for (evicted_room, evicted_did) in evicted {
                            rosters_changed.push(evicted_room.clone());
                            if let Some(pid) =
                                mgr.remove_node_from_room(&evicted_room, &evicted_did)
                            {
                                pipes_to_drop.push(pid);
                            }
                        }

                        // Auto-establish pipe if we have local participants
                        // in this room and don't yet have a pipe to the
                        // announcer.  Tie-break by DID order so only ONE
                        // side initiates: the lexically-higher DID is the
                        // dialer; the other waits for the offer.
                        if pre_has_room
                            && mgr.local_did() > remote_did.as_str()
                            && !mgr.has_pipe(&room_id, &remote_did)
                        {
                            let room = RoomId::parse(&room_id);
                            match mgr.establish_pipe(&remote_did, &room) {
                                Ok((pipe_peer, offer_signal)) => {
                                    info!(
                                        "SFU cascade: initiating pipe to {} for room {}",
                                        remote_did, room_id
                                    );
                                    new_pipe_peers.push(pipe_peer);
                                    outbound
                                        .push((GossipTarget::PeerDid(remote_did), offer_signal));
                                }
                                Err(e) => {
                                    debug!(
                                        "SFU cascade: establish_pipe to {} skipped: {}",
                                        remote_did, e
                                    );
                                }
                            }
                        }
                    }
                    CascadeSignal::Leave { did, room_id } => {
                        // Targeted: remove only this (room, did) pair so
                        // other rooms the remote serves stay intact.  If
                        // we had a pipe SfuPeer for that pair, the
                        // returned ParticipantId tells the event loop
                        // which entry to drop.
                        rosters_changed.push(room_id.clone());
                        if let Some(pid) = mgr.remove_node_from_room(&room_id, &did) {
                            pipes_to_drop.push(pid);
                        }
                    }
                    CascadeSignal::PipeOffer {
                        from_did,
                        to_did,
                        room_id,
                        sdp_offer,
                        track_mapping,
                    } => {
                        if to_did != mgr.local_did() {
                            debug!(
                                "SFU cascade: PipeOffer not addressed to us (to={}), ignoring",
                                to_did
                            );
                        } else if let Some(meta) = mgr.pipe_meta(&room_id, &from_did) {
                            // Renegotiation against an existing pipe —
                            // dispatch the offer into the event loop so
                            // it can apply it against the live pipe RTC
                            // and emit the answer via the pubsub bridge.
                            pipe_renegotiation_offers.push((
                                meta.participant_id.clone(),
                                sdp_offer,
                                from_did,
                                room_id,
                                track_mapping,
                            ));
                        } else {
                            match mgr.handle_pipe_offer(&from_did, &room_id, &sdp_offer) {
                                Ok((pipe_peer, answer_signal)) => {
                                    info!(
                                        "SFU cascade: pipe answer ready for {} (room {})",
                                        from_did, room_id
                                    );
                                    new_pipe_peers.push(pipe_peer);
                                    outbound.push((GossipTarget::PeerDid(from_did), answer_signal));
                                }
                                Err(e) => {
                                    warn!(
                                        "SFU cascade: handle_pipe_offer failed for {}: {}",
                                        from_did, e
                                    );
                                }
                            }
                        }
                    }
                    CascadeSignal::PipeAnswer {
                        from_did,
                        to_did,
                        room_id,
                        sdp_answer,
                    } => {
                        if to_did != mgr.local_did() {
                            debug!(
                                "SFU cascade: PipeAnswer not addressed to us (to={}), ignoring",
                                to_did
                            );
                        } else {
                            match mgr.handle_pipe_answer(&from_did, &room_id, &sdp_answer) {
                                Ok((pid, answer_json)) => {
                                    pipe_answers.push((pid, answer_json));
                                }
                                Err(e) => {
                                    warn!(
                                        "SFU cascade: handle_pipe_answer failed for {}: {}",
                                        from_did, e
                                    );
                                }
                            }
                        }
                    }
                    CascadeSignal::QualityPreference {
                        from_did,
                        room_id,
                        preference,
                    } => {
                        // A remote node's subscribers want this quality
                        // level for simulcast media we forward through
                        // the pipe.  Look up the pipe peer for that
                        // remote node and set its quality preference so
                        // should_forward_rid() selects the right layer.
                        if let Some(meta) = mgr.pipe_meta(&room_id, &from_did) {
                            let pid = meta.participant_id.clone();
                            info!(
                                "SFU cascade: quality preference '{}' from {} for room {} → pipe peer {}",
                                preference, from_did, room_id, pid
                            );
                            pipe_quality_prefs.push((pid, preference));
                        } else {
                            debug!(
                                "SFU cascade: QualityPreference from {} for room {} but no pipe found",
                                from_did, room_id
                            );
                        }
                    }
                }
            }

            // Dispatch the pipe-side SFU peers we just minted into the
            // event loop so its tick can drive the str0m state machine
            // forward (ICE, SDP, media).
            for peer in new_pipe_peers {
                if let Err(e) = self.server.command_tx.send(SfuCommand::AddPeer(peer)).await {
                    warn!("SFU cascade: failed to enqueue pipe peer: {}", e);
                }
            }
            // For dialer-side answer application — the pipe peer is
            // already in the event loop's peers map; the answer goes
            // through the same ApplyServerAnswer code path client peers
            // use.
            for (pid, sdp_answer_json) in pipe_answers {
                if let Err(e) = self
                    .server
                    .command_tx
                    .send(SfuCommand::ApplyServerAnswer {
                        participant_id: pid,
                        sdp_answer_json,
                    })
                    .await
                {
                    warn!("SFU cascade: failed to enqueue pipe answer: {}", e);
                }
            }
            for pid in pipes_to_drop {
                if let Err(e) = self
                    .server
                    .command_tx
                    .send(SfuCommand::RemovePeer(pid))
                    .await
                {
                    warn!("SFU cascade: failed to enqueue pipe removal: {}", e);
                }
            }
            for (participant_id, sdp_offer_json, remote_did, room_id, track_mapping) in
                pipe_renegotiation_offers
            {
                if let Err(e) = self
                    .server
                    .command_tx
                    .send(SfuCommand::ApplyPipeRenegotiationOffer {
                        participant_id,
                        sdp_offer_json,
                        remote_did,
                        room_id,
                        track_mapping,
                    })
                    .await
                {
                    warn!(
                        "SFU cascade: failed to enqueue pipe renegotiation offer: {}",
                        e
                    );
                }
            }

            // Apply quality preferences received from remote cascade
            // nodes — each one sets the pipe peer's preference so
            // should_forward_rid() selects the correct simulcast layer.
            for (pid, preference) in pipe_quality_prefs {
                if let Err(e) = self
                    .server
                    .command_tx
                    .send(SfuCommand::SetQualityPreference {
                        participant_id: pid,
                        preference,
                    })
                    .await
                {
                    warn!(
                        "SFU cascade: failed to enqueue pipe quality preference: {}",
                        e
                    );
                }
            }

            rosters_changed.sort();
            rosters_changed.dedup();
            for room_key in rosters_changed {
                self.republish_roster(&room_key).await;
            }

            for (target, signal) in outbound {
                if let Err(e) = self.gossip.send(target, signal).await {
                    debug!("SFU cascade: gossip send failed: {}", e);
                }
            }
        }
    }

    /// Re-announces every local room every 10 s so cascade peers keep a
    /// fresh view.
    async fn run_cascade_heartbeat(self: Arc<Self>) {
        let mut interval = tokio::time::interval(Duration::from_secs(10));
        interval.tick().await; // skip immediate first tick
        loop {
            interval.tick().await;
            let room_ids: Vec<RoomId> = {
                let rooms = self.rooms.read().await;
                rooms.list_rooms().iter().map(|r| r.id.clone()).collect()
            };
            for room_id in room_ids {
                self.announce_room(&room_id).await;
            }
        }
    }

    /// Cleanup loop: two duties.
    ///
    /// 1. **Dead peers** — the server event loop drops peers whose
    ///    `Rtc::is_alive()` turned false and reports them here. A client's
    ///    room slot is freed as a leave would free it; a pipe's `PipeMeta`
    ///    entry is removed so `cascade_established_pipes` reflects reality.
    ///
    /// 2. **Periodic stale-node sweep** — every 5 seconds, run
    ///    `evict_stale_nodes(30s)` regardless of whether an Announce
    ///    arrived.  Without this, a crashed node that stops announcing
    ///    never gets evicted because the sweep only ran inside the
    ///    Announce handler.
    async fn run_cascade_cleanup(
        self: Arc<Self>,
        mut dead_peer_rx: tokio::sync::mpsc::UnboundedReceiver<super::server::DeadPeer>,
    ) {
        let mut sweep_interval = tokio::time::interval(Duration::from_secs(5));
        // The first tick fires immediately; skip it so the sweep starts
        // 5 seconds after boot (everything is healthy at startup).
        sweep_interval.tick().await;

        loop {
            tokio::select! {
                dead = dead_peer_rx.recv() => {
                    let Some(dead) = dead else { return; };
                    if dead.is_pipe {
                        info!(
                            "SFU cascade cleanup: pipe peer {} (room {} ↔ {}) died, removing",
                            dead.participant_id, dead.room_id, dead.agent_did
                        );
                        let room_key = dead.room_id.to_string();
                        self.cascade_manager
                            .write()
                            .await
                            .remove_node_from_room(&room_key, &dead.agent_did);
                        self.republish_roster(&room_key).await;
                    } else {
                        self.remove_dead_participant(dead).await;
                    }
                }
                _ = sweep_interval.tick() => {
                    self.reap_empty_rooms().await;
                    let mut pipes_to_drop: Vec<ParticipantId> = Vec::new();
                    let mut rosters_changed: Vec<String> = Vec::new();
                    {
                        let mut mgr = self.cascade_manager.write().await;
                        let evicted = mgr.evict_stale_nodes(Duration::from_secs(30));
                        for (room_id, did) in evicted {
                            info!(
                                "SFU cascade sweep: evicting stale node {} from room {}",
                                did, room_id
                            );
                            rosters_changed.push(room_id.clone());
                            if let Some(pid) = mgr.remove_node_from_room(&room_id, &did) {
                                pipes_to_drop.push(pid);
                            }
                        }
                    }
                    for pid in pipes_to_drop {
                        let _ = self
                            .server
                            .command_tx
                            .send(SfuCommand::RemovePeer(pid))
                            .await;
                    }
                    rosters_changed.sort();
                    rosters_changed.dedup();
                    for room_key in rosters_changed {
                        self.republish_roster(&room_key).await;
                    }

                    // ── Rebalance check ──
                    // Walk local rooms, ask the cascade manager if any
                    // room exceeds the 90% threshold with a viable
                    // target node.  suggest_rebalance() enforces its
                    // own 30-second per-room cooldown, so running this
                    // on every 5s tick adds no extra migration chatter.
                    let migrate_events = {
                        // Acquire cascade_manager before rooms — matches
                        // the lock order in call_join to prevent deadlock.
                        let mut mgr = self.cascade_manager.write().await;
                        let rooms = self.rooms.read().await;
                        let mut events: Vec<SfuMigrateEvent> = Vec::new();
                        for room in rooms.list_rooms() {
                            let local_count = room.participant_count() as u32;
                            let room_id_str = room.id.to_string();
                            if let Some(target_did) =
                                mgr.suggest_rebalance(&room_id_str, local_count)
                            {
                                // Pick the most-recently-joined participant
                                // that can reach another node — least
                                // disruption to established sessions. A
                                // client that reaches only this node is
                                // never asked to move.
                                if let Some(victim) = room
                                    .participants
                                    .values()
                                    .filter(|p| p.accepts_redirect)
                                    .max_by_key(|p| p.joined_at)
                                {
                                    events.push(SfuMigrateEvent {
                                        target_did: victim.agent_did.clone(),
                                        neighbourhood_url: room.id.neighbourhood_url.clone(),
                                        room_name: room.id.room_name.clone(),
                                        migrate_to_did: target_did,
                                    });
                                    mgr.record_rebalance(&room_id_str);
                                }
                            }
                        }
                        events
                    };
                    // Publish outside the lock
                    if !migrate_events.is_empty() {
                        let pubsub = crate::pubsub::get_global_pubsub().await;
                        for evt in migrate_events {
                            info!(
                                "SFU rebalance: migrating {} from {}:{} → {}",
                                evt.target_did,
                                evt.neighbourhood_url,
                                evt.room_name,
                                evt.migrate_to_did
                            );
                            pubsub.publish_event(crate::events::Event::sfu_migrate(&evt));
                        }
                    }
                }
            }
        }
    }

    /// Returns true if this node has at least one participant in `room_id`.
    /// Used by the cascade auto-establish logic so we only build pipes for
    /// rooms we're actively serving.
    async fn local_has_room(&self, room_id_str: &str) -> bool {
        let room_id = RoomId::parse(room_id_str);
        let rooms = self.rooms.read().await;
        rooms.get_room(&room_id).is_some()
    }

    /// Republishes the roster of a room this node serves after its
    /// remote sessions changed (a cascade node announced other
    /// sessions, left, died or went stale). A room this node does not
    /// serve has no local listeners, so nothing is sent.
    async fn republish_roster(&self, room_key: &str) {
        if self.local_has_room(room_key).await {
            self.publish_roster(&RoomId::parse(room_key), None).await;
        }
    }

    /// Broadcast our local room-state update.  Called from the
    /// participant add/remove path and the heartbeat; pushes an Announce
    /// through the gossip transport so peers can keep cross-node counts
    /// fresh and list this node's sessions in their rosters.
    async fn announce_room(&self, room_id: &RoomId) {
        let sessions = self.local_sessions(room_id).await;
        let capacity_hint = self.gossip.max_participants_per_node();
        let signal = CascadeSignal::Announce {
            did: self.gossip.local_did().to_string(),
            room_id: room_id.to_string(),
            participant_count: sessions.len() as u32,
            capacity_hint,
            sessions,
        };
        if let Err(e) = self.gossip.send(GossipTarget::Broadcast, signal).await {
            warn!("SFU announce_room failed: {}", e);
        }
    }

    /// The sessions this node serves in `room_id`, sorted.
    async fn local_sessions(&self, room_id: &RoomId) -> Vec<super::SfuRosterParticipant> {
        let mut sessions: Vec<super::SfuRosterParticipant> = self
            .rooms
            .read()
            .await
            .get_room(room_id)
            .map(|room| {
                room.participants
                    .values()
                    .map(|p| super::SfuRosterParticipant {
                        agent_did: p.agent_did.clone(),
                        participant_id: p.id.to_string(),
                    })
                    .collect()
            })
            .unwrap_or_default();
        sessions.sort();
        sessions
    }

    /// How many people a room in `neighbourhood_url` holds: four times the
    /// space's mesh limit. A space with no policy gets the most any policy
    /// may set — bounded, never unlimited. A cascade's per-node capacity
    /// plays no part.
    async fn room_capacity(neighbourhood_url: &str) -> usize {
        config_store::stored_policy(neighbourhood_url)
            .await
            .map_or(MAX_MESH_PARTICIPANTS, |c| c.max_mesh_participants) as usize
            * 4
    }

    /// Tell everyone in `room_id` who is in it now: this node's sessions
    /// and, in a cascade, the sessions other nodes announced for the room
    /// (ids qualified by node, `P3@did:key:...`). Called after every join
    /// and every departure, and when a cascade node's sessions change, so
    /// clients follow membership without inferring it from media. A
    /// departure also reaches the removed DID, including when the room
    /// became empty; the payload still lists only current participants.
    async fn publish_roster(&self, room_id: &RoomId, departed: Option<&str>) {
        let _order = self.roster_order.lock().await;
        let mut participants = self.local_sessions(room_id).await;
        let recipients: Vec<String> = participants
            .iter()
            .map(|p| p.agent_did.clone())
            .chain(departed.map(str::to_string))
            .collect();
        participants.extend(
            self.cascade_manager
                .read()
                .await
                .remote_sessions(&room_id.to_string()),
        );
        participants.sort();
        let roster = super::SfuRoomRoster {
            neighbourhood_url: room_id.neighbourhood_url.clone(),
            room_name: room_id.room_name.clone(),
            participants,
        };
        crate::pubsub::get_global_pubsub()
            .await
            .publish_event(crate::events::Event::sfu_room_roster(&roster, recipients));
    }

    /// Get the local address of the SFU server.
    pub fn local_addr(&self) -> std::net::SocketAddr {
        self.server.local_addr
    }

    /// Get the reachability probe result from startup.
    pub fn reachability(&self) -> &SfuReachability {
        &self.reachability
    }

    /// The public address offered next to the bind address, if any.
    pub fn advertised_addr(&self) -> Option<std::net::SocketAddr> {
        self.server.advertised.map(|a| a.addr)
    }

    // ---- Room management ----

    /// Whether `room_id` may be opened: refused once the node, or the room's
    /// neighbourhood, holds its maximum of rooms.
    fn check_new_room(rooms: &RoomManager, room_id: &RoomId) -> Result<(), String> {
        let all = rooms.list_rooms();
        if all.len() >= MAX_ROOMS_PER_NODE {
            return Err(format!(
                "This node already holds {} rooms",
                MAX_ROOMS_PER_NODE
            ));
        }
        let in_neighbourhood = all
            .iter()
            .filter(|r| r.id.neighbourhood_url == room_id.neighbourhood_url)
            .count();
        if in_neighbourhood >= MAX_ROOMS_PER_NEIGHBOURHOOD {
            return Err(format!(
                "This neighbourhood already has {} rooms on this node",
                MAX_ROOMS_PER_NEIGHBOURHOOD
            ));
        }
        Ok(())
    }

    /// Close rooms that have stayed empty for [`EMPTY_ROOM_TTL`].
    async fn reap_empty_rooms(&self) {
        let now_ms = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64;
        let ttl_ms = EMPTY_ROOM_TTL.as_millis() as u64;
        let mut rooms = self.rooms.write().await;
        let stale: Vec<RoomId> = rooms
            .list_rooms()
            .iter()
            .filter(|r| r.is_empty() && now_ms.saturating_sub(r.created_at) >= ttl_ms)
            .map(|r| r.id.clone())
            .collect();
        for id in stale {
            info!("SFU: closing room {}, empty for {:?}", id, EMPTY_ROOM_TTL);
            rooms.destroy_room(&id).ok();
        }
    }

    /// Create or get an SFU room for a neighbourhood call.
    pub async fn start_room(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
    ) -> Result<SfuRoomInfo, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        // Read before taking the rooms lock: it reads the neighbourhood's links.
        let max = Self::room_capacity(neighbourhood_url).await;
        let mut rooms = self.rooms.write().await;
        if rooms.get_room(&room_id).is_none() {
            Self::check_new_room(&rooms, &room_id)?;
        }

        rooms
            .create_room(room_id.clone(), Some(max))
            .map_err(|e| e.to_string())?;

        let room = rooms.get_room(&room_id).unwrap();
        let info = self.room_to_info(room);
        // Drop the write lock before awaiting the async announce.
        drop(rooms);
        // Broadcast the new room so cascade peers learn our capacity
        // BEFORE any clients join.  Without this, pick_redirect_node
        // sees an empty known_nodes map and accepts all joins locally.
        self.announce_room(&room_id).await;
        Ok(info)
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

    /// Join a call.  Membership checking happens in the WS handler
    /// layer (sfu_ws.rs) via `get_neighbourhood_owners` — this method
    /// trusts that the caller already passed the gate.
    ///
    /// `accept_redirect`: the caller can reach other cascade nodes (a test
    /// harness, a gateway), so a busy room may send it elsewhere. An SDK
    /// client reaches only its own executor and never sets it: it is served
    /// here or refused with `RoomFull`.
    ///
    /// One session per device: `device_id` names the device the caller joins
    /// from. A session of the same DID from the same device (another browser
    /// tab) is replaced — the newest join wins: its peer is dropped and it is
    /// told so (`sfu-session-replaced`). A session from another device stays,
    /// as another participant. A reconnecting client joins from its own
    /// device with its `session_key`, so it replaces its dead session rather
    /// than being refused. A join whose key a join with another key replaced
    /// on this device (an older tab that missed its notice) is refused with
    /// [`CallJoinError::Replaced`]: it must stop, not take the call back.
    ///
    /// Cancel safe: the join runs to its end in a task of its own, so a
    /// caller that stops waiting (`request.cancel`, a closed socket) never
    /// leaves the room holding a session whose peer the media loop never
    /// got, or a replaced session that was never removed or told.
    ///
    /// `authorised` tells whether the caller's session still holds. The
    /// task asks once more when it holds the room, before changing it: a
    /// join whose token was revoked (or expired) meanwhile ends with
    /// [`CallJoinError::Unauthorized`] and changes nothing. A caller that
    /// only stopped waiting still holds, so its join completes.
    #[allow(clippy::too_many_arguments)]
    pub async fn call_join(
        self: &Arc<Self>,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        device_id: &str,
        session_key: &str,
        sdp_offer_json: &str,
        accept_redirect: bool,
        authorised: impl Fn() -> bool + Send + Sync + 'static,
    ) -> Result<CallSessionInfo, CallJoinError> {
        let this = self.clone();
        let (url, room, did, device, key, offer) = (
            neighbourhood_url.to_string(),
            room_name.to_string(),
            agent_did.to_string(),
            device_id.to_string(),
            session_key.to_string(),
            sdp_offer_json.to_string(),
        );
        tokio::spawn(async move {
            this.join_call(
                &url,
                &room,
                &did,
                &device,
                &key,
                &offer,
                accept_redirect,
                &authorised,
            )
            .await
        })
        .await
        .map_err(|e| CallJoinError::Failed(format!("SFU join failed: {}", e)))?
    }

    #[allow(clippy::too_many_arguments)]
    async fn join_call(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        device_id: &str,
        session_key: &str,
        sdp_offer_json: &str,
        accept_redirect: bool,
        authorised: &(dyn Fn() -> bool + Send + Sync),
    ) -> Result<CallSessionInfo, CallJoinError> {
        for (name, value) in [("deviceId", device_id), ("sessionKey", session_key)] {
            if value.is_empty() || value.chars().count() > MAX_DEVICE_ID_LEN {
                return Err(CallJoinError::Failed(format!(
                    "{} must be 1 to {} characters",
                    name, MAX_DEVICE_ID_LEN
                )));
            }
        }
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let pid = ParticipantId::next();

        // Cascade redirect check — read-only lock, no room creation yet.
        // Room creation defers until after SDP parsing succeeds so a
        // malformed offer never leaves an empty room.
        let preferred = if accept_redirect {
            self.preferred_node.clone()
        } else {
            None
        };
        // A cascade's per-node capacity decides only where a redirectable
        // joiner goes, never whether a room is full.
        if accept_redirect {
            let mgr = self.cascade_manager.read().await;
            let rooms = self.rooms.read().await;
            let local_count = rooms
                .get_room(&room_id)
                .map(|r| r.participant_count() as u32)
                .unwrap_or(0);
            if let Some(node) =
                mgr.pick_redirect_node(&room_id.to_string(), local_count, preferred.as_deref())
            {
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
        let max_per_room = Self::room_capacity(neighbourhood_url).await;

        // Parse SDP offer and create Rtc instance — do this BEFORE
        // creating the room or registering the participant so failures
        // never leave stale state.
        let offer: SdpOffer = serde_json::from_str(sdp_offer_json)
            .map_err(|e| format!("Invalid SDP offer: {}", e))?;

        let (rtc, sdp_answer) = SfuServer::create_rtc_for_offer(
            offer,
            &self.server.sockets.host_addrs(),
            self.server
                .advertised
                .map(|a| (a.addr, self.server.sockets.primary())),
        )?;

        let peer = SfuPeer::new(
            pid.clone(),
            room_id.clone(),
            agent_did.to_string(),
            rtc,
            false,
        );

        // SDP and RTC succeeded — create the room if needed, register the
        // participant and hand its peer to the event loop in a single
        // write-lock scope. The capacity and cap checks hold against
        // concurrent joins, and the event loop gets each room change in the
        // order the room made it: a session's peer is always added before
        // the removal of a later replacement, so the media loop holds the
        // sessions the room holds.
        let replaced = {
            let mut rooms = self.rooms.write().await;
            // The caller may have lost its token while the join waited (the
            // join outlives its request). Asked under the lock, so a
            // revocation that came before the room changes stops the join.
            if !authorised() {
                return Err(CallJoinError::Unauthorized(
                    "The caller's token was revoked or expired before the join completed"
                        .to_string(),
                ));
            }
            let created = rooms.get_room(&room_id).is_none();
            if created {
                Self::check_new_room(&rooms, &room_id)?;
            }
            let other_calls = rooms
                .list_rooms()
                .iter()
                .filter(|r| r.id != room_id && r.has_did(agent_did))
                .count();
            if other_calls >= MAX_CALLS_PER_DID {
                return Err(CallJoinError::Failed(format!(
                    "Already in {} calls on this node",
                    MAX_CALLS_PER_DID
                )));
            }
            rooms
                .create_room(room_id.clone(), Some(max_per_room))
                .map_err(|e| e.to_string())?;
            let room = rooms
                .get_room_mut(&room_id)
                .ok_or_else(|| RoomError::NotFound.to_string())?;
            let replaced = room.add_participant(
                pid.clone(),
                agent_did.to_string(),
                device_id.to_string(),
                session_key.to_string(),
            )?;
            if accept_redirect {
                room.mark_redirectable(&pid);
            }

            // The replaced session is out of the room: drop its peer.
            if let Some(old) = &replaced {
                let _ = self
                    .server
                    .command_tx
                    .send(SfuCommand::RemovePeer(old.clone()))
                    .await;
            }
            if let Err(e) = self.server.command_tx.send(SfuCommand::AddPeer(peer)).await {
                // AddPeer failed — roll back the participant, and the room
                // only if this join created it and it is empty again. A room
                // `start_room` opened stays.
                if let Some(room) = rooms.get_room_mut(&room_id) {
                    room.remove_participant(&pid);
                    if created && room.participant_count() == 0 {
                        let _ = rooms.destroy_room(&room_id);
                    }
                }
                return Err(CallJoinError::Failed(format!(
                    "Failed to add peer to SFU: {}",
                    e
                )));
            }
            replaced
        };

        // Tell the replaced session, and drop its preference.
        if let Some(old) = &replaced {
            info!(
                "SFU: {} joined {} again from the same device; replaced {} with {}",
                agent_did, room_id, old, pid
            );
            self.forget_quality_preference(&room_id.to_string(), old)
                .await;
            crate::pubsub::get_global_pubsub().await.publish_event(
                crate::events::Event::sfu_session_replaced(&super::SfuSessionReplaced {
                    target_did: agent_did.to_string(),
                    neighbourhood_url: neighbourhood_url.to_string(),
                    room_name: room_name.to_string(),
                    participant_id: old.to_string(),
                }),
            );
        }

        // Build stream mapping from existing participants in the room.
        let stream_mapping: Vec<String> = {
            let rooms = self.rooms.read().await;
            rooms
                .get_room(&room_id)
                .map(|room| {
                    room.participants
                        .values()
                        .filter(|p| p.id != pid)
                        .map(|p| format!("{}:{}", p.id, p.agent_did))
                        .collect()
                })
                .unwrap_or_default()
        };

        self.announce_room(&room_id).await;
        self.publish_roster(&room_id, None).await;

        Ok(CallSessionInfo {
            room_name: room_name.to_string(),
            neighbourhood_url: neighbourhood_url.to_string(),
            participant_id: pid.to_string(),
            sdp_answer,
            redirect_to: None,
            stream_mapping,
        })
    }

    /// Leave the call session `participant_id`. `false` when it is not in
    /// the call (any more) or is not `agent_did`'s: a leave never ends
    /// another session, not even another of the same agent.
    ///
    /// Cancel safe like [`Self::call_join`]: once the room has let the
    /// session go, its peer is removed and its departure published.
    pub async fn call_leave(
        self: &Arc<Self>,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
    ) -> Result<bool, String> {
        let this = self.clone();
        let (url, room, did, participant) = (
            neighbourhood_url.to_string(),
            room_name.to_string(),
            agent_did.to_string(),
            participant_id.to_string(),
        );
        tokio::spawn(async move { this.leave_call(&url, &room, &did, &participant).await })
            .await
            .map_err(|e| format!("SFU leave failed: {}", e))?
    }

    async fn leave_call(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
    ) -> Result<bool, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let (pid, local_count) = {
            let mut rooms = self.rooms.write().await;
            // Already gone — the server dropped the connection first, the
            // room ended with it, or a later join replaced it: there is
            // nothing to leave.
            let Some(pid) = rooms
                .get_room(&room_id)
                .and_then(|room| room.session_of(participant_id, agent_did))
            else {
                return Ok(false);
            };
            (
                pid.clone(),
                Self::remove_from_room(&mut rooms, &room_id, &pid),
            )
        };

        // Notify event loop
        let _ = self
            .server
            .command_tx
            .send(SfuCommand::RemovePeer(pid.clone()))
            .await;

        self.after_participant_left(&room_id, &pid, agent_did, local_count)
            .await;
        Ok(true)
    }

    /// Free the room slot of a client whose RTC died. The event loop has
    /// already dropped the peer. Matched by participant id, so a late
    /// notice for an old session never evicts the same DID's rejoin.
    async fn remove_dead_participant(&self, dead: super::server::DeadPeer) {
        let local_count = {
            let mut rooms = self.rooms.write().await;
            let in_room = rooms
                .get_room(&dead.room_id)
                .is_some_and(|room| room.participants.contains_key(&dead.participant_id));
            if !in_room {
                return; // already left
            }
            Self::remove_from_room(&mut rooms, &dead.room_id, &dead.participant_id)
        };
        info!(
            "SFU: participant {} ({}) in {} died, freeing its slot",
            dead.participant_id, dead.agent_did, dead.room_id
        );
        self.after_participant_left(
            &dead.room_id,
            &dead.participant_id,
            &dead.agent_did,
            local_count,
        )
        .await;
    }

    /// Remove `pid` from its room, destroying the room once empty. Returns
    /// the room's remaining local participant count.
    fn remove_from_room(rooms: &mut RoomManager, room_id: &RoomId, pid: &ParticipantId) -> u32 {
        let Some(room) = rooms.get_room_mut(room_id) else {
            return 0;
        };
        if room.remove_participant(pid) {
            rooms.destroy_room(room_id).ok();
            0
        } else {
            room.participant_count() as u32
        }
    }

    /// Gossip, roster and preference bookkeeping after the session `pid` of
    /// `agent_did` left, by leaving or by dying. The roster also goes to
    /// `agent_did`, so the departed session sees that it is no longer listed
    /// even when no other session of that agent remains.
    async fn after_participant_left(
        &self,
        room_id: &RoomId,
        pid: &ParticipantId,
        agent_did: &str,
        local_count: u32,
    ) {
        // Always re-announce the fresh local count (lets healthy peers
        // refresh their view).  If the room is now empty on this node,
        // also send a directed Leave so peers prune their
        // `known_nodes` entry for this (room, did) tuple — without
        // Leave, the lifetime of a known_nodes entry is bounded only
        // by the next non-zero Announce from us, which for a vacated
        // room will never come.
        let room_key = room_id.to_string();
        self.announce_room(room_id).await;
        self.publish_roster(room_id, Some(agent_did)).await;
        if local_count == 0 {
            let signal = CascadeSignal::Leave {
                did: self.gossip.local_did().to_string(),
                room_id: room_key.clone(),
            };
            if let Err(e) = self.gossip.send(GossipTarget::Broadcast, signal).await {
                warn!("SFU: gossip Leave broadcast failed: {}", e);
            }
        }

        // Clean up the session's quality preference and re-propagate the
        // aggregate to pipe peers (the max may have changed).
        self.forget_quality_preference(&room_key, pid).await;
    }

    /// Drop the quality preference of a session that left `room_key`, and
    /// re-propagate the room's aggregate.
    async fn forget_quality_preference(&self, room_key: &str, pid: &ParticipantId) {
        {
            let mut prefs = self.local_quality_preferences.write().await;
            if let Some(room_prefs) = prefs.get_mut(room_key) {
                room_prefs.remove(&pid.to_string());
                if room_prefs.is_empty() {
                    prefs.remove(room_key);
                }
            }
        }
        self.propagate_quality_preference(room_key).await;
    }

    /// Consume an SDP answer that the client produced in response to a
    /// server-pushed renegotiation offer.  Routes the answer back to
    /// the appropriate participant's Rtc transport via the SFU event
    /// loop so the str0m state machine can apply it through `sdp_api()`.
    pub async fn call_answer_server_offer(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
        sdp_answer_json: &str,
    ) -> Result<bool, String> {
        let pid = self
            .own_session(neighbourhood_url, room_name, agent_did, participant_id)
            .await?;
        let _ = self
            .server
            .command_tx
            .send(SfuCommand::ApplyServerAnswer {
                participant_id: pid,
                sdp_answer_json: sdp_answer_json.to_string(),
            })
            .await;
        Ok(true)
    }

    /// The client asks for an ICE restart of its own session `participant_id`,
    /// e.g. after its network changed. The SFU offers new ICE credentials
    /// over the server-offer path (`sfu-call-renegotiation-offer`); the client
    /// answers with `call_answer_server_offer`. The same connection, DTLS
    /// session and tracks carry on. Refused when the session is not
    /// `agent_did`'s: a restart never touches another session, not even
    /// another device of the same agent.
    pub async fn call_restart_ice(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
    ) -> Result<bool, String> {
        let pid = self
            .own_session(neighbourhood_url, room_name, agent_did, participant_id)
            .await?;
        self.server
            .command_tx
            .send(SfuCommand::RestartIce {
                participant_id: pid,
            })
            .await
            .map_err(|e| format!("Failed to send RestartIce command: {}", e))?;
        Ok(true)
    }

    /// Trickle ICE: add a remote ICE candidate to a participant's
    /// WebRTC connection.  The candidate arrives as the raw SDP
    /// candidate-attribute string (the `candidate:` line extracted from
    /// an `RTCIceCandidateInit`).
    pub async fn add_ice_candidate(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
        candidate_sdp: &str,
    ) -> Result<bool, String> {
        let pid = self
            .own_session(neighbourhood_url, room_name, agent_did, participant_id)
            .await?;
        self.server
            .command_tx
            .send(SfuCommand::AddIceCandidate {
                participant_id: pid,
                candidate_sdp: candidate_sdp.to_string(),
            })
            .await
            .map_err(|e| format!("Failed to send AddIceCandidate command: {}", e))?;
        Ok(true)
    }

    /// Relay data through the SFU to all other participants in a room.
    /// The data is written to their matching data channel (by label) if
    /// one exists, and always published to the `sfu-data` events_ws
    /// topic so non-WebRTC subscribers can receive it.
    #[allow(clippy::too_many_arguments)]
    pub async fn send_data(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
        channel_label: &str,
        data: Vec<u8>,
        binary: bool,
    ) -> Result<bool, String> {
        let pid = self
            .own_session(neighbourhood_url, room_name, agent_did, participant_id)
            .await?;
        self.server
            .command_tx
            .send(SfuCommand::RelayData {
                participant_id: pid,
                channel_label: channel_label.to_string(),
                data,
                binary,
            })
            .await
            .map_err(|e| format!("Failed to send RelayData command: {}", e))?;
        Ok(true)
    }

    /// Detailed list of established pipes as `(room_id, remote_did)`
    /// tuples.  Lets scenarios verify which specific node-pairs are
    /// connected.
    pub async fn cascade_established_pipes(&self) -> Vec<(String, String)> {
        let mgr = self.cascade_manager.read().await;
        mgr.established_pipes()
    }

    /// Query the quality preferences map from the SFU event loop.
    /// Returns `(participant_id_string, preference)` pairs.  Used by
    /// wind tunnel tests to verify cascade quality preference propagation.
    pub async fn get_quality_preferences(&self) -> Vec<(String, String)> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        if self
            .server
            .command_tx
            .send(SfuCommand::GetQualityPreferences { reply: tx })
            .await
            .is_err()
        {
            return Vec::new();
        }
        match rx.await {
            Ok(map) => map
                .into_iter()
                .map(|(pid, pref)| (pid.to_string(), pref))
                .collect(),
            Err(_) => Vec::new(),
        }
    }

    /// The session `participant_id` of `agent_did` in the call, or why there
    /// is none: the room is gone, or the session is not in it (any more) or
    /// is another agent's. A call about a session never falls back to
    /// another session of the same agent.
    async fn own_session(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
    ) -> Result<ParticipantId, String> {
        let rooms = self.rooms.read().await;
        let room = rooms
            .get_room(&RoomId::new(neighbourhood_url, room_name))
            .ok_or_else(|| RoomError::NotFound.to_string())?;
        room.session_of(participant_id, agent_did)
            .ok_or_else(|| format!("Session {} is not in this call", participant_id))
    }

    /// The neighbourhood each local participant's room belongs to, keyed by
    /// participant id. Lets the RPC layer show a caller only its own
    /// neighbourhoods' entries.
    pub async fn participant_neighbourhoods(&self) -> HashMap<String, String> {
        let rooms = self.rooms.read().await;
        rooms
            .list_rooms()
            .into_iter()
            .flat_map(|room| {
                room.participants
                    .keys()
                    .map(|pid| (pid.to_string(), room.id.neighbourhood_url.clone()))
            })
            .collect()
    }

    /// The neighbourhood of each local room, keyed by its room key (`url:room`,
    /// which cannot be split back reliably: URLs contain `:`).
    pub async fn room_neighbourhoods(&self) -> HashMap<String, String> {
        let rooms = self.rooms.read().await;
        rooms
            .list_rooms()
            .into_iter()
            .map(|room| (room.id.to_string(), room.id.neighbourhood_url.clone()))
            .collect()
    }

    /// Set the quality preference for a participant's received video streams.
    ///
    /// In cascade deployments, this also propagates the aggregate
    /// preference (max across all local non-pipe subscribers) to each
    /// connected pipe node so the sender's SFU forwards the correct
    /// simulcast layer through the pipe.
    pub async fn call_set_quality_preference(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        participant_id: &str,
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
        let pid = self
            .own_session(neighbourhood_url, room_name, agent_did, participant_id)
            .await?;

        self.server
            .command_tx
            .send(SfuCommand::SetQualityPreference {
                participant_id: pid.clone(),
                preference: preference.to_string(),
            })
            .await
            .map_err(|e| format!("Failed to send quality preference command: {}", e))?;

        // Track this preference for cascade aggregation and propagate. Only
        // while the session is still in the room, checked under the
        // preference lock: a departure leaves the room before it forgets the
        // session's preference, so a session that left during the send above
        // is either seen gone here or has its entry forgotten after this.
        let room_key = room_id.to_string();
        {
            let mut prefs = self.local_quality_preferences.write().await;
            let in_room = self
                .rooms
                .read()
                .await
                .get_room(&room_id)
                .is_some_and(|room| room.participants.contains_key(&pid));
            if !in_room {
                return Ok(true);
            }
            prefs
                .entry(room_key.clone())
                .or_default()
                .insert(pid.to_string(), preference.to_string());
        }

        self.propagate_quality_preference(&room_key).await;

        Ok(true)
    }

    /// Compute the max quality preference across all local subscribers
    /// in a room and gossip it to each pipe peer so the sender node
    /// forwards the correct simulcast layer.
    ///
    /// Quality ordering: high/auto > medium > low.
    async fn propagate_quality_preference(&self, room_key: &str) {
        let mgr = self.cascade_manager.read().await;
        let pipe_peers = mgr.pipe_peers_for_room(room_key);
        if pipe_peers.is_empty() {
            return; // single-node room — nothing to propagate
        }
        let local_did = mgr.local_did().to_string();
        drop(mgr);

        // Compute aggregate (max across local subscribers).
        let aggregate = {
            let prefs = self.local_quality_preferences.read().await;
            match prefs.get(room_key) {
                Some(room_prefs) => max_quality_preference(room_prefs.values()),
                None => "high".to_string(), // default when no preferences set
            }
        };

        info!(
            "SFU cascade: propagating quality preference '{}' to {} pipe peer(s) for room {}",
            aggregate,
            pipe_peers.len(),
            room_key
        );

        // Send to each connected pipe node.
        for (remote_did, _pid) in &pipe_peers {
            let signal = CascadeSignal::QualityPreference {
                from_did: local_did.clone(),
                room_id: room_key.to_string(),
                preference: aggregate.clone(),
            };
            if let Err(e) = self
                .gossip
                .send(GossipTarget::PeerDid(remote_did.clone()), signal)
                .await
            {
                warn!(
                    "SFU cascade: failed to send quality preference to {}: {}",
                    remote_did, e
                );
            } else {
                info!(
                    "SFU cascade: quality preference '{}' sent to {} for room {}",
                    aggregate, remote_did, room_key
                );
            }
        }
    }

    // ---- SFU configuration ----

    /// The neighbourhood's call config: its policy (`mode`,
    /// `maxMeshParticipants`) from its links, the rest from this executor.
    /// ICE servers stay empty: clients bring their own (`iceServers` on
    /// `createSession`).
    pub async fn get_config(&self, neighbourhood_url: &str) -> SfuConfig {
        SfuConfig {
            max_participants_per_node: Some(self.gossip.max_participants_per_node()),
            sfu_peers: self.cascade_peer_dids.clone(),
            preferred_sfu_did: self.preferred_node.clone(),
            ..config_store::read_policy(neighbourhood_url)
                .await
                .into_config()
        }
    }

    /// Validate `config` and store its policy in the neighbourhood as a link
    /// signed by `context`, replacing the one `context` stored before. The
    /// caller checks that `context` is the creator.
    pub async fn set_config(
        &self,
        neighbourhood_url: &str,
        config: SfuConfig,
        context: &crate::agent::AgentContext,
    ) -> Result<(), String> {
        let policy = call_policy(config)?;
        config_store::write_policy(neighbourhood_url, &policy, context).await
    }

    /// The SFU node a client of this executor reaches under the
    /// neighbourhood's mode: this node itself for `designated`, the
    /// `"gateway"` sentinel for `gateway`, none otherwise.
    pub async fn sfu_peer_for_neighbourhood(&self, neighbourhood_url: &str) -> Option<String> {
        match config_store::read_policy(neighbourhood_url)
            .await
            .mode
            .as_str()
        {
            "designated" => Some(self.gossip.local_did().to_string()),
            "gateway" => Some("gateway".to_string()), // Sentinel — caller resolves gateway DID
            _ => None,
        }
    }

    /// The SFU nodes under the neighbourhood's mode: for `cascaded`, this
    /// node and its configured cascade peers.
    pub async fn sfu_peers_for_neighbourhood(&self, neighbourhood_url: &str) -> Vec<String> {
        let policy = config_store::read_policy(neighbourhood_url).await;
        let local = self.gossip.local_did().to_string();
        match policy.mode.as_str() {
            "cascaded" => std::iter::once(local)
                .chain(self.cascade_peer_dids.iter().cloned())
                .collect(),
            "designated" => vec![local],
            "gateway" => vec!["gateway".to_string()],
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
                    participant_id: p.id.to_string(),
                    agent_did: p.agent_did.clone(),
                    has_audio: p.has_audio,
                    has_video: p.has_video,
                    is_active_speaker: p.is_active_speaker,
                })
                .collect(),
            created_at_ms: room.created_at,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::events::Topic;
    use crate::sfu::IceServer;
    use crate::sfu::NoopGossip;

    /// A service of the test's own. Registering the global one would leave
    /// later tests a server whose runtime has ended.
    async fn test_service() -> Arc<SfuService> {
        let server = SfuServer::start(SfuServerConfig::default()).await.unwrap();
        Arc::new(SfuService {
            cascade_manager: Arc::new(RwLock::new(CascadeManager::new(
                "did:node".into(),
                server.sockets.clone(),
                8,
            ))),
            reachability: server.reachability.clone(),
            server,
            rooms: Arc::new(RwLock::new(RoomManager::new())),
            gossip: Arc::new(NoopGossip::new("did:node".into(), 8)),
            local_quality_preferences: RwLock::new(HashMap::new()),
            preferred_node: None,
            cascade_peer_dids: vec![],
            roster_order: tokio::sync::Mutex::new(()),
        })
    }

    /// A browser's offer: one audio section, as a caller sends to join.
    fn offer() -> String {
        let mut caller = str0m::Rtc::builder().build(std::time::Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        serde_json::to_string(&change.apply().unwrap().0).unwrap()
    }

    async fn join(svc: &Arc<SfuService>, url: &str, did: &str, device: &str) -> String {
        // A new tab: a session key of its own.
        let key = format!("key-{}", ParticipantId::next());
        svc.call_join(url, "r", did, device, &key, &offer(), false, || true)
            .await
            .unwrap()
            .participant_id
    }

    /// The sessions in room `r` of `url`, as `(participant id, DID)`, sorted.
    async fn sessions(svc: &SfuService, url: &str) -> Vec<(String, String)> {
        let mut sessions: Vec<(String, String)> = svc
            .list_rooms()
            .await
            .into_iter()
            .filter(|r| r.neighbourhood_url == url)
            .flat_map(|r| r.participants)
            .map(|p| (p.participant_id, p.agent_did))
            .collect();
        sessions.sort();
        sessions
    }

    /// The participant ids the SFU event loop holds a quality preference for.
    async fn peers_with_preferences(svc: &SfuService) -> Vec<String> {
        let mut pids: Vec<String> = svc
            .get_quality_preferences()
            .await
            .into_iter()
            .map(|(pid, _)| pid)
            .collect();
        pids.sort();
        pids
    }

    /// Every event published so far on `events`.
    fn drain(
        events: &mut tokio::sync::broadcast::Receiver<Arc<crate::events::Event>>,
    ) -> Vec<Arc<crate::events::Event>> {
        std::iter::from_fn(|| events.try_recv().ok()).collect()
    }

    /// The payloads of the events of `topic` about `url` among `events`.
    fn published(
        events: &[Arc<crate::events::Event>],
        topic: Topic,
        url: &str,
    ) -> Vec<serde_json::Value> {
        events
            .iter()
            .filter(|e| e.topic == topic && e.payload["neighbourhoodUrl"] == url)
            .map(|e| e.payload.clone())
            .collect()
    }

    /// Another tab on the same device joins: the newest join wins. The old
    /// session leaves the room, its peer leaves the SFU, and it is told.
    #[tokio::test]
    async fn a_join_from_the_same_device_replaces_the_session_and_tells_it() {
        let svc = test_service().await;
        let url = "neighbourhood://same-device";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let first = join(&svc, url, "did:a", "laptop").await;
        svc.call_set_quality_preference(url, "r", "did:a", &first, "low")
            .await
            .unwrap();
        assert_eq!(peers_with_preferences(&svc).await, vec![first.clone()]);

        let second = join(&svc, url, "did:a", "laptop").await;

        assert_ne!(first, second);
        assert_eq!(
            sessions(&svc, url).await,
            vec![(second.clone(), "did:a".to_string())]
        );
        assert!(
            peers_with_preferences(&svc).await.is_empty(),
            "the replaced peer left the SFU"
        );
        assert!(
            svc.local_quality_preferences.read().await.is_empty(),
            "the replaced session's preference no longer counts towards the cascade's"
        );
        let seen = drain(&mut events);
        let replaced = published(&seen, Topic::SfuSessionReplaced, url);
        assert_eq!(replaced.len(), 1, "{replaced:?}");
        assert_eq!(replaced[0]["participantId"], first.as_str());
        assert_eq!(replaced[0]["targetDid"], "did:a");
        let rosters = published(&seen, Topic::SfuRoomRoster, url);
        assert_eq!(
            rosters.last().unwrap()["participants"],
            serde_json::json!([{ "agentDid": "did:a", "participantId": second }])
        );
        // The old id acts on nothing any more: a late leave from the old tab
        // cannot end the new one.
        assert!(!svc.call_leave(url, "r", "did:a", &first).await.unwrap());
        assert_eq!(sessions(&svc, url).await.len(), 1);
    }

    /// The participants of the next roster published about `url`.
    async fn next_roster(
        events: &mut tokio::sync::broadcast::Receiver<Arc<crate::events::Event>>,
        url: &str,
    ) -> serde_json::Value {
        next_roster_event(events, url).await.payload["participants"].clone()
    }

    async fn next_roster_event(
        events: &mut tokio::sync::broadcast::Receiver<Arc<crate::events::Event>>,
        url: &str,
    ) -> Arc<crate::events::Event> {
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let event = events.recv().await.unwrap();
                if event.topic == Topic::SfuRoomRoster && event.payload["neighbourhoodUrl"] == url {
                    return event;
                }
            }
        })
        .await
        .expect("no roster in time")
    }

    /// In a cascade the roster lists everyone in the call: the sessions
    /// another node announces for the room come in under ids qualified by
    /// that node (both nodes number theirs from P1), and the roster is
    /// published again when they change or the node leaves.
    #[tokio::test]
    async fn the_roster_lists_the_sessions_of_other_nodes() {
        let svc = test_service().await;
        let url = "neighbourhood://cascade-roster";
        let room_key = RoomId::new(url, "r").to_string();
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let local = join(&svc, url, "did:a", "laptop").await;
        assert_eq!(
            next_roster(&mut events, url).await,
            serde_json::json!([{ "agentDid": "did:a", "participantId": local }])
        );

        let (tx, rx) = tokio::sync::mpsc::channel(8);
        tokio::spawn(svc.clone().run_gossip_inbound(rx));
        // "did:other" sorts above this node's DID, so it would dial the
        // pipe: this node only listens.
        tx.send(CascadeSignal::Announce {
            did: "did:other".into(),
            room_id: room_key.clone(),
            participant_count: 1,
            capacity_hint: 8,
            sessions: vec![crate::sfu::SfuRosterParticipant {
                agent_did: "did:b".into(),
                participant_id: "P1".into(),
            }],
        })
        .await
        .unwrap();
        let roster = next_roster_event(&mut events, url).await;
        assert_eq!(
            roster.payload["participants"],
            serde_json::json!([
                { "agentDid": "did:a", "participantId": local },
                { "agentDid": "did:b", "participantId": "P1@did:other" },
            ])
        );
        // Only this node's sessions get it from here: did:b hears from its own
        // node, and an announce naming any DID must not send it this room.
        assert_eq!(
            roster.audience,
            crate::events::Audience::Dids(vec!["did:a".into()])
        );

        tx.send(CascadeSignal::Leave {
            did: "did:other".into(),
            room_id: room_key,
        })
        .await
        .unwrap();
        assert_eq!(
            next_roster(&mut events, url).await,
            serde_json::json!([{ "agentDid": "did:a", "participantId": local }])
        );
    }

    /// Two joins from the same device overlap: the second replaces the
    /// first's session before the first join has returned. The media loop
    /// must end up holding the sessions the room holds, so each session's
    /// peer reaches it before the removal that ends it.
    #[tokio::test]
    async fn overlapping_replacements_keep_the_media_loop_in_step_with_the_room() {
        let svc = test_service().await;
        let url = "neighbourhood://overlapping-replacements";
        let first = join(&svc, url, "did:a", "laptop").await;

        // A replacing join pauses on the preference bookkeeping while this
        // guard is held, after the room has taken its session.
        let barrier = svc.local_quality_preferences.write().await;
        let a = tokio::spawn({
            let svc = svc.clone();
            async move { join(&svc, url, "did:a", "laptop").await }
        });
        let second = only_session_other_than(&svc, url, &first).await;
        // The media loop keeps a preference per session until it removes
        // that session's peer, so a preference left over means a removal
        // came before the peer it was meant for.
        let preference = tokio::spawn({
            let (svc, second) = (svc.clone(), second.clone());
            async move {
                svc.call_set_quality_preference(url, "r", "did:a", &second, "low")
                    .await
            }
        });
        eventually(|| async { peers_with_preferences(&svc).await == vec![second.clone()] }).await;
        let b = tokio::spawn({
            let svc = svc.clone();
            async move { join(&svc, url, "did:a", "laptop").await }
        });
        let third = only_session_other_than(&svc, url, &second).await;
        // A round trip: the media loop has handled every command sent so far.
        svc.get_quality_preferences().await;
        drop(barrier);

        assert_eq!(a.await.unwrap(), second);
        assert_eq!(b.await.unwrap(), third);
        assert!(preference.await.unwrap().unwrap());
        assert_eq!(
            sessions(&svc, url).await,
            vec![(third, "did:a".to_string())]
        );
        assert!(
            peers_with_preferences(&svc).await.is_empty(),
            "the media loop still holds the replaced session {second}"
        );
    }

    /// The one session in room `r` of `url` once it is no longer `old`.
    async fn only_session_other_than(svc: &SfuService, url: &str, old: &str) -> String {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                if let [(pid, _)] = sessions(svc, url).await.as_slice() {
                    if pid != old {
                        return pid.clone();
                    }
                }
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
        })
        .await
        .expect("no new session in time")
    }

    /// Wait until `check` holds, failing the test after five seconds.
    async fn eventually<F, Fut>(mut check: F)
    where
        F: FnMut() -> Fut,
        Fut: std::future::Future<Output = bool>,
    {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            while !check().await {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
        })
        .await
        .expect("condition not reached in time");
    }

    /// A session leaves while its quality preference waits to reach the
    /// media loop. The departure has already forgotten the session's
    /// preferences, so the late write must not store one: nothing would
    /// remove it, and it would count towards the cascade's aggregate.
    #[tokio::test]
    async fn a_preference_for_a_session_that_left_meanwhile_is_not_kept() {
        let mut svc = test_service().await;
        let url = "neighbourhood://late-preference";
        let p1 = join(&svc, url, "did:a", "laptop").await;

        // A full command channel of one: the preference waits on its send.
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        Arc::get_mut(&mut svc).unwrap().server.command_tx = tx.clone();
        tx.send(SfuCommand::RemovePeer(ParticipantId::next()))
            .await
            .unwrap();
        let preference = tokio::spawn({
            let (svc, p1) = (svc.clone(), p1.clone());
            async move {
                svc.call_set_quality_preference(url, "r", "did:a", &p1, "high")
                    .await
            }
        });
        for _ in 0..20 {
            tokio::task::yield_now().await;
        }
        assert!(
            !preference.is_finished(),
            "the preference waits on its send"
        );

        svc.remove_dead_participant(super::super::server::DeadPeer {
            room_id: RoomId::new(url, "r"),
            agent_did: "did:a".to_string(),
            participant_id: ParticipantId::parse(&p1).unwrap(),
            is_pipe: false,
        })
        .await;
        assert!(sessions(&svc, url).await.is_empty());

        // Drain the filler, then the preference's command.
        while !matches!(
            rx.recv().await,
            Some(SfuCommand::SetQualityPreference { .. })
        ) {}
        preference.await.unwrap().unwrap();

        let prefs = svc.local_quality_preferences.read().await;
        assert!(
            prefs.values().all(|room| !room.contains_key(&p1)),
            "stale preference for {p1}: {prefs:?}"
        );
    }

    /// Every command a test's channel received within `wait`.
    async fn commands_within(
        rx: &mut tokio::sync::mpsc::Receiver<SfuCommand>,
        wait: std::time::Duration,
    ) -> Vec<SfuCommand> {
        let mut commands = Vec::new();
        while let Ok(Some(command)) = tokio::time::timeout(wait, rx.recv()).await {
            commands.push(command);
        }
        commands
    }

    /// A replacing join is cancelled while it waits to hand the media loop
    /// its commands. The join still finishes: the new session's peer is
    /// added, the replaced one's removed, and the replaced tab is told.
    #[tokio::test]
    async fn a_join_cancelled_while_it_sends_its_peer_still_completes() {
        let mut svc = test_service().await;
        let url = "neighbourhood://cancelled-join-send";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let p1 = join(&svc, url, "did:a", "laptop").await;

        // A full command channel of one: the join waits on its sends.
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        Arc::get_mut(&mut svc).unwrap().server.command_tx = tx.clone();
        tx.send(SfuCommand::RemovePeer(ParticipantId::next()))
            .await
            .unwrap();
        let second = tokio::spawn({
            let svc = svc.clone();
            async move { join(&svc, url, "did:a", "laptop").await }
        });
        for _ in 0..20 {
            tokio::task::yield_now().await;
        }
        assert!(!second.is_finished(), "the join waits on its send");
        second.abort();
        assert!(second.await.unwrap_err().is_cancelled());

        let commands = commands_within(&mut rx, std::time::Duration::from_millis(500)).await;
        let listed = sessions(&svc, url).await;
        assert_eq!(listed.len(), 1, "{listed:?}");
        let p2 = listed[0].0.clone();
        assert_ne!(p2, p1, "the room holds the new session");
        assert!(
            commands
                .iter()
                .any(|c| matches!(c, SfuCommand::RemovePeer(pid) if pid.to_string() == p1)),
            "the replaced peer was never removed"
        );
        assert!(
            commands
                .iter()
                .any(|c| matches!(c, SfuCommand::AddPeer(peer) if peer.id.to_string() == p2)),
            "the media loop never got the new session's peer"
        );
        let replaced = published(&drain(&mut events), Topic::SfuSessionReplaced, url);
        assert_eq!(replaced.len(), 1, "{replaced:?}");
        assert_eq!(replaced[0]["participantId"], p1.as_str());
    }

    /// A replacing join is cancelled after the room took its session, while
    /// it waits on the preference bookkeeping: the replaced tab is still told.
    #[tokio::test]
    async fn a_join_cancelled_after_it_replaced_a_session_still_tells_it() {
        let svc = test_service().await;
        let url = "neighbourhood://cancelled-join-notice";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let p1 = join(&svc, url, "did:a", "laptop").await;

        let barrier = svc.local_quality_preferences.write().await;
        let second = tokio::spawn({
            let svc = svc.clone();
            async move { join(&svc, url, "did:a", "laptop").await }
        });
        only_session_other_than(&svc, url, &p1).await;
        second.abort();
        assert!(second.await.unwrap_err().is_cancelled());
        drop(barrier);

        let mut replaced = Vec::new();
        eventually(|| {
            replaced.extend(published(
                &drain(&mut events),
                Topic::SfuSessionReplaced,
                url,
            ));
            let told = !replaced.is_empty();
            async move { told }
        })
        .await;
        assert_eq!(replaced.len(), 1, "{replaced:?}");
        assert_eq!(replaced[0]["participantId"], p1.as_str());
    }

    /// The caller's token is revoked while its join waits for the room: the
    /// join ends without touching the room. No session is added, the tab it
    /// would have replaced stays in the call and is not told, and the media
    /// loop gets no peer. A join the client merely stopped waiting for still
    /// completes (the tests above).
    #[tokio::test]
    async fn a_join_whose_token_is_revoked_while_it_waits_changes_nothing() {
        let mut svc = test_service().await;
        let url = "neighbourhood://revoked-join";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let p1 = join(&svc, url, "did:a", "laptop").await;
        drain(&mut events);

        let (tx, mut rx) = tokio::sync::mpsc::channel(8);
        Arc::get_mut(&mut svc).unwrap().server.command_tx = tx;
        let held = Arc::new(std::sync::atomic::AtomicBool::new(true));
        let barrier = svc.rooms.write().await;
        let second = tokio::spawn({
            let (svc, held) = (svc.clone(), held.clone());
            async move {
                svc.call_join(
                    url,
                    "r",
                    "did:a",
                    "laptop",
                    "key-revoked",
                    &offer(),
                    false,
                    move || held.load(std::sync::atomic::Ordering::SeqCst),
                )
                .await
            }
        });
        for _ in 0..20 {
            tokio::task::yield_now().await;
        }
        assert!(!second.is_finished(), "the join waits for the room");
        held.store(false, std::sync::atomic::Ordering::SeqCst);
        drop(barrier);

        let refused = second.await.unwrap().unwrap_err();
        assert!(
            matches!(refused, CallJoinError::Unauthorized(_)),
            "{refused:?}"
        );
        assert_eq!(sessions(&svc, url).await, vec![(p1, "did:a".to_string())]);
        let commands = commands_within(&mut rx, std::time::Duration::from_millis(200)).await;
        assert!(commands.is_empty(), "the media loop got {commands:?}");
        let seen = drain(&mut events);
        assert!(published(&seen, Topic::SfuSessionReplaced, url).is_empty());
        assert!(published(&seen, Topic::SfuRoomRoster, url).is_empty());
    }

    /// A phone and a laptop: two participants, listed apart in the roster,
    /// and leaving on one leaves the other in the call.
    #[tokio::test]
    async fn the_same_did_on_another_device_joins_beside_it() {
        let svc = test_service().await;
        let url = "neighbourhood://two-devices";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let laptop = join(&svc, url, "did:a", "laptop").await;
        let phone = join(&svc, url, "did:a", "phone").await;

        let mut both = vec![
            (laptop.clone(), "did:a".to_string()),
            (phone.clone(), "did:a".to_string()),
        ];
        both.sort();
        assert_eq!(sessions(&svc, url).await, both);
        let seen = drain(&mut events);
        assert!(published(&seen, Topic::SfuSessionReplaced, url).is_empty());
        let rosters = published(&seen, Topic::SfuRoomRoster, url);
        let listed: Vec<&str> = rosters.last().unwrap()["participants"]
            .as_array()
            .unwrap()
            .iter()
            .map(|p| p["participantId"].as_str().unwrap())
            .collect();
        assert_eq!(listed.len(), 2, "{rosters:?}");
        assert!(listed.contains(&laptop.as_str()) && listed.contains(&phone.as_str()));

        assert!(svc.call_leave(url, "r", "did:a", &phone).await.unwrap());
        assert_eq!(
            sessions(&svc, url).await,
            vec![(laptop, "did:a".to_string())]
        );
    }

    /// A call about a session acts on that session only: never on another
    /// session of the same DID, never on another DID's.
    #[tokio::test]
    async fn per_session_calls_touch_only_the_session_they_name() {
        let svc = test_service().await;
        let url = "neighbourhood://per-session";
        let laptop = join(&svc, url, "did:a", "laptop").await;
        let phone = join(&svc, url, "did:a", "phone").await;
        let bobs = join(&svc, url, "did:b", "laptop").await;
        let everyone = sessions(&svc, url).await;

        // Someone else's session, or one that does not exist.
        for (did, pid) in [
            ("did:a", bobs.as_str()),
            ("did:b", laptop.as_str()),
            ("did:a", "P0"),
            ("did:a", ""),
        ] {
            assert!(
                !svc.call_leave(url, "r", did, pid).await.unwrap(),
                "{did} {pid}"
            );
            assert!(svc
                .call_answer_server_offer(url, "r", did, pid, "{}")
                .await
                .is_err());
            assert!(svc.call_restart_ice(url, "r", did, pid).await.is_err());
            assert!(svc
                .add_ice_candidate(
                    url,
                    "r",
                    did,
                    pid,
                    "candidate:1 1 UDP 1 127.0.0.1 9 typ host"
                )
                .await
                .is_err());
            assert!(svc
                .call_set_quality_preference(url, "r", did, pid, "low")
                .await
                .is_err());
            assert!(svc
                .send_data(url, "r", did, pid, "chat", b"hi".to_vec(), false)
                .await
                .is_err());
        }
        assert_eq!(sessions(&svc, url).await, everyone);
        assert!(peers_with_preferences(&svc).await.is_empty());

        // The phone's own call reaches the phone, not the laptop.
        svc.call_set_quality_preference(url, "r", "did:a", &phone, "low")
            .await
            .unwrap();
        assert_eq!(peers_with_preferences(&svc).await, vec![phone.clone()]);
        assert!(svc.call_leave(url, "r", "did:a", &phone).await.unwrap());
        let mut rest = vec![(laptop, "did:a".to_string()), (bobs, "did:b".to_string())];
        rest.sort();
        assert_eq!(sessions(&svc, url).await, rest);
    }

    /// An ICE restart is offered to the session that asked for it only: not
    /// to the same agent's other device, and never for a session the caller
    /// does not own.
    #[tokio::test]
    async fn an_ice_restart_reaches_only_the_session_that_asked() {
        let svc = test_service().await;
        let url = "neighbourhood://restart-ice";
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let laptop = join(&svc, url, "did:a", "laptop").await;
        let replaced = join(&svc, url, "did:a", "phone").await;
        let phone = join(&svc, url, "did:a", "phone").await;
        let bobs = join(&svc, url, "did:b", "laptop").await;

        // The phone's replaced session is gone: its id restarts nothing.
        let refused = svc
            .call_restart_ice(url, "r", "did:a", &replaced)
            .await
            .unwrap_err();
        assert!(refused.contains("not in this call"), "{refused}");

        // Bob cannot restart Alice's laptop, nor Alice Bob's session.
        let refused = svc
            .call_restart_ice(url, "r", "did:b", &laptop)
            .await
            .unwrap_err();
        assert!(refused.contains("not in this call"), "{refused}");
        assert!(svc
            .call_restart_ice(url, "r", "did:a", &bobs)
            .await
            .is_err());
        assert!(svc
            .call_restart_ice("neighbourhood://no-such-room", "r", "did:a", &laptop)
            .await
            .is_err());

        assert!(svc
            .call_restart_ice(url, "r", "did:a", &phone)
            .await
            .unwrap());

        let mut offered = Vec::new();
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                let seen = drain(&mut events);
                offered.extend(
                    published(&seen, Topic::SfuCallRenegotiationOffer, url)
                        .into_iter()
                        .map(|o| (o["participantId"].clone(), o["targetDid"].clone())),
                );
                if !offered.is_empty() {
                    return;
                }
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
        })
        .await
        .expect("no restart offer in time");
        // A round trip: the media loop has handled every command sent so far,
        // so a stray offer to another session would have been published.
        svc.get_quality_preferences().await;
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        offered.extend(
            published(&drain(&mut events), Topic::SfuCallRenegotiationOffer, url)
                .into_iter()
                .map(|o| (o["participantId"].clone(), o["targetDid"].clone())),
        );
        assert_eq!(
            offered,
            vec![(serde_json::json!(phone), serde_json::json!("did:a"))]
        );
    }

    #[tokio::test]
    async fn a_join_names_its_device_and_its_session_key() {
        let svc = test_service().await;
        let url = "neighbourhood://device";
        for bad in [String::new(), "d".repeat(MAX_DEVICE_ID_LEN + 1)] {
            let cases = [
                (bad.as_str(), "k", "deviceId"),
                ("d", bad.as_str(), "sessionKey"),
            ];
            for (device, key, field) in cases {
                let refused = svc
                    .call_join(url, "r", "did:a", device, key, &offer(), false, || true)
                    .await
                    .unwrap_err();
                assert!(
                    matches!(&refused, CallJoinError::Failed(m) if m.contains(field)),
                    "{refused:?}"
                );
            }
        }
        assert!(sessions(&svc, url).await.is_empty());
        // The limit counts characters: 128 four-byte characters are 512 bytes and allowed.
        let long = "🎧".repeat(MAX_DEVICE_ID_LEN);
        svc.call_join(url, "r", "did:a", &long, &long, &offer(), false, || true)
            .await
            .unwrap();
    }

    /// Tab A (key K1) is replaced by tab B (K2) and misses the notice. Its
    /// rebuild joins with K1 again: refused as replaced, and tab B keeps the
    /// call, untold. Tab B's own rebuild (K2) still replaces its session.
    #[tokio::test]
    async fn a_replaced_tab_cannot_take_the_call_back() {
        let svc = test_service().await;
        let url = "neighbourhood://take-back";
        let join_keyed = |key: &'static str| {
            let svc = svc.clone();
            async move {
                svc.call_join(url, "r", "did:a", "phone", key, &offer(), false, || true)
                    .await
                    .map(|s| s.participant_id)
            }
        };
        let a = join_keyed("K1").await.unwrap();
        let b = join_keyed("K2").await.unwrap();
        assert_ne!(a, b);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let refused = join_keyed("K1").await.unwrap_err();
        assert!(matches!(refused, CallJoinError::Replaced(_)), "{refused:?}");
        assert_eq!(
            sessions(&svc, url).await,
            vec![(b.clone(), "did:a".to_string())]
        );
        let seen = drain(&mut events);
        assert!(
            published(&seen, Topic::SfuSessionReplaced, url).is_empty(),
            "tab B was told it was replaced"
        );

        let b2 = join_keyed("K2").await.unwrap();
        assert_eq!(sessions(&svc, url).await, vec![(b2, "did:a".to_string())]);
        let replaced = published(&drain(&mut events), Topic::SfuSessionReplaced, url);
        assert_eq!(replaced.len(), 1, "{replaced:?}");
        assert_eq!(replaced[0]["participantId"], b.as_str());
    }

    fn config(mode: &str) -> SfuConfig {
        SfuConfig {
            mode: mode.into(),
            ..Default::default()
        }
    }

    /// The production departure path publishes through the real event stream,
    /// not a fake SDK API that bypasses audience filtering. The departed
    /// session's DID receives the roster that no longer lists that session:
    /// when it was the agent's only session, when the room became empty, and
    /// when another device of the same agent stays in the room.
    #[tokio::test]
    async fn departed_session_receives_the_roster_without_itself() {
        use crate::agent::capabilities::NEIGHBOURHOOD_READ_CAPABILITY;
        use crate::agent::session::{Principal, Session};
        use crate::agent::AgentContext;
        use futures::StreamExt;

        let session = |did: &str, read: bool| {
            Arc::new(
                Session::for_test(Principal::Agent(AgentContext::managed(did.to_string())))
                    .with_did(did)
                    .with_caps(if read {
                        vec![NEIGHBOURHOOD_READ_CAPABILITY.clone()]
                    } else {
                        vec![]
                    }),
            )
        };
        let service = test_service().await;
        // Who stays in the room after P1 of did:departed goes.
        let stays: [Option<(&str, &str)>; 3] = [
            None,
            Some(("did:remaining", "device-r")),
            Some(("did:departed", "device-b")),
        ];
        for dead in [false, true] {
            for (case, stayer) in stays.iter().enumerate() {
                let room_id = RoomId::new("departure-delivery", format!("{dead}-{case}"));
                {
                    let mut rooms = service.rooms.write().await;
                    rooms.create_room(room_id.clone(), None).unwrap();
                    let room = rooms.get_room_mut(&room_id).unwrap();
                    assert_eq!(
                        room.add_participant(
                            ParticipantId(1),
                            "did:departed".into(),
                            "device-a".into(),
                            "key-a".into()
                        )
                        .unwrap(),
                        None
                    );
                    if let Some((did, device)) = stayer {
                        assert_eq!(
                            room.add_participant(
                                ParticipantId(2),
                                did.to_string(),
                                device.to_string(),
                                "key-r".to_string()
                            )
                            .unwrap(),
                            None
                        );
                    }
                }
                let departed = session("did:departed", true);
                let mut stream = crate::api::events_ws::build_event_stream(
                    departed.clone(),
                    crate::api::event_interest::watching_all(),
                    None,
                )
                .await;
                let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
                if dead {
                    service
                        .remove_dead_participant(super::super::server::DeadPeer {
                            room_id: room_id.clone(),
                            agent_did: "did:departed".into(),
                            participant_id: ParticipantId(1),
                            is_pipe: false,
                        })
                        .await;
                } else {
                    assert!(service
                        .call_leave(
                            &room_id.neighbourhood_url,
                            &room_id.room_name,
                            "did:departed",
                            "P1"
                        )
                        .await
                        .unwrap());
                }
                // Tests in other threads publish on the same global pubsub and
                // stream: look for this room's roster, not the first event.
                let ours = |payload: &serde_json::Value| {
                    payload["neighbourhoodUrl"] == room_id.neighbourhood_url.as_str()
                        && payload["roomName"] == room_id.room_name.as_str()
                };
                let event = drain(&mut events)
                    .into_iter()
                    .find(|e| e.topic == Topic::SfuRoomRoster && ours(&e.payload))
                    .expect("the departure publishes the room's roster");
                assert!(
                    departed.may_receive(&event),
                    "departure must reach the removed DID"
                );
                assert!(!session("did:outsider", true).may_receive(&event));
                assert!(!session("did:departed", false).may_receive(&event));
                assert_eq!(
                    session("did:remaining", true).may_receive(&event),
                    matches!(stayer, Some(("did:remaining", _)))
                );
                let message = tokio::time::timeout(Duration::from_secs(1), async {
                    loop {
                        let message: serde_json::Value =
                            serde_json::from_str(&stream.next().await.unwrap()).unwrap();
                        if message["type"] == "sfu-room-roster" && ours(&message) {
                            break message;
                        }
                    }
                })
                .await
                .expect("departed session receives the real stream event");
                assert_eq!(
                    message["participants"],
                    match stayer {
                        None => serde_json::json!([]),
                        Some((did, _)) =>
                            serde_json::json!([{ "agentDid": did, "participantId": "P2" }]),
                    }
                );
            }
        }
        service.shutdown().await;
    }

    #[test]
    fn set_config_stores_only_the_call_policy() {
        let policy = call_policy(SfuConfig {
            max_mesh_participants: 6,
            ..config("designated")
        })
        .unwrap();
        assert_eq!(policy.mode, "designated");
        assert_eq!(policy.max_mesh_participants, 6);
    }

    /// Node fields and ICE servers are refused, each by name, never stored
    /// in part.
    #[test]
    fn set_config_refuses_executor_fields_and_ice_servers() {
        let refused = |c: SfuConfig, field: &str| {
            let err = call_policy(c).unwrap_err();
            assert!(err.contains(field), "{err}");
        };
        refused(
            SfuConfig {
                designated_peer: Some("did:node".into()),
                ..config("designated")
            },
            "designatedPeer",
        );
        refused(
            SfuConfig {
                sfu_peers: vec!["did:a".into()],
                ..config("cascaded")
            },
            "sfuPeers",
        );
        refused(
            SfuConfig {
                max_participants_per_node: Some(9),
                ..config("cascaded")
            },
            "maxParticipantsPerNode",
        );
        refused(
            SfuConfig {
                preferred_sfu_did: Some("did:a".into()),
                ..config("cascaded")
            },
            "preferredSfuDid",
        );
        refused(
            SfuConfig {
                ice_servers: vec![IceServer {
                    urls: vec!["turn:t".into()],
                    username: Some("u".into()),
                    credential: Some("c".into()),
                }],
                ..config("designated")
            },
            "iceServers",
        );
    }

    #[test]
    fn set_config_refuses_an_unknown_mode_or_a_mesh_limit_out_of_range() {
        assert!(call_policy(config("p2p")).is_err());
        assert!(call_policy(SfuConfig {
            max_mesh_participants: 1,
            ..config("mesh")
        })
        .is_err());
    }
}
