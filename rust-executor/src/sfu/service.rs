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
use super::gossip::{CascadeGossip, GossipTarget};
use super::room::{ParticipantId, RoomError, RoomId, RoomManager, SfuRoom};
use super::server::{SfuCommand, SfuPeer, SfuServer, SfuServerConfig};
use super::types::SfuMigrateEvent;

/// Global SFU service instance.
static SFU_SERVICE: OnceCell<Arc<SfuService>> = OnceCell::new();

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

/// The global SFU service, analogous to HolochainService.
pub struct SfuService {
    server: SfuServer,
    rooms: Arc<RwLock<RoomManager>>,
    /// Maps neighbourhood URLs to their SFU configuration (from Social DNA).
    configs: Arc<RwLock<HashMap<String, SfuConfig>>>,
    /// Cascade manager for multi-node SFU deployments.
    cascade_manager: Arc<RwLock<CascadeManager>>,
    /// Pluggable cluster-gossip transport.  Always present — single-node
    /// deployments pass a `NoopGossip` so the cascade plumbing stays
    /// uniform and the redirect logic falls through to "no peers".
    gossip: Arc<dyn CascadeGossip>,
    /// Local subscriber quality preferences tracked for cascade
    /// aggregation.  Maps `room_id_str → (subscriber_did → preference)`.
    /// When any subscriber changes preference, the aggregate (max) gets
    /// gossiped to pipe peers so the sender node forwards the correct
    /// simulcast layer.
    local_quality_preferences: RwLock<HashMap<String, HashMap<String, String>>>,
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
            cascade_manager: Arc::new(RwLock::new(cascade_manager)),
            gossip: Arc::clone(&gossip),
            local_quality_preferences: RwLock::new(HashMap::new()),
        });

        if let Some(rx) = gossip.take_inbound() {
            let svc = Arc::clone(&service);
            tokio::spawn(async move {
                svc.run_gossip_inbound(rx).await;
            });
        }

        // Pump pipe-renegotiation pubsub topics → gossip.  Phase F
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

        // Cascade cleanup: process dead pipe notifications from the
        // server event loop and run periodic stale-node eviction.
        {
            let dead_pipe_rx = service
                .server
                .dead_pipe_rx
                .lock()
                .expect("dead_pipe_rx mutex poisoned")
                .take();
            if let Some(rx) = dead_pipe_rx {
                let svc = Arc::clone(&service);
                tokio::spawn(async move {
                    svc.run_cascade_cleanup(rx).await;
                });
            }
        }

        SFU_SERVICE
            .set(service.clone())
            .map_err(|_| "SFU service already initialized".to_string())?;

        Ok(service)
    }

    /// Subscribe to `SFU_PIPE_RENEGOTIATION_OFFER_TOPIC` and translate
    /// each publish into a `CascadeSignal::PipeOffer` directed at the
    /// remote SFU.  Runs for the lifetime of the executor.
    async fn run_pipe_renegotiation_offer_bridge(self: Arc<Self>) {
        let pubsub = crate::pubsub::get_global_pubsub().await;
        let mut rx = pubsub
            .subscribe(&crate::pubsub::SFU_PIPE_RENEGOTIATION_OFFER_TOPIC)
            .await;
        loop {
            let msg = match rx.recv().await {
                Ok(m) => m,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
            };
            let payload: crate::sfu::SfuPipeRenegotiationOffer = match serde_json::from_str(&msg) {
                Ok(p) => p,
                Err(e) => {
                    warn!("SFU pipe-reneg offer bridge: parse error: {}", e);
                    continue;
                }
            };
            let signal = CascadeSignal::PipeOffer {
                from_did: self.gossip.local_did().to_string(),
                to_did: payload.remote_did.clone(),
                room_id: payload.room_id,
                sdp_offer: payload.sdp_offer,
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

    /// Subscribe to `SFU_PIPE_RENEGOTIATION_ANSWER_TOPIC` and translate
    /// each publish into a `CascadeSignal::PipeAnswer` directed at the
    /// remote SFU.  Runs for the lifetime of the executor.
    async fn run_pipe_renegotiation_answer_bridge(self: Arc<Self>) {
        let pubsub = crate::pubsub::get_global_pubsub().await;
        let mut rx = pubsub
            .subscribe(&crate::pubsub::SFU_PIPE_RENEGOTIATION_ANSWER_TOPIC)
            .await;
        loop {
            let msg = match rx.recv().await {
                Ok(m) => m,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
            };
            let payload: crate::sfu::SfuPipeRenegotiationAnswer = match serde_json::from_str(&msg) {
                Ok(p) => p,
                Err(e) => {
                    warn!("SFU pipe-reneg answer bridge: parse error: {}", e);
                    continue;
                }
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
            // through the SFU_PIPE_RENEGOTIATION_ANSWER_TOPIC bridge.
            // Tuple: (local pipe participant_id, sdp_offer_json,
            //         remote_did, room_id_str).
            let mut pipe_renegotiation_offers: Vec<(ParticipantId, String, String, String)> =
                Vec::new();
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
                    } => {
                        let remote_did = did.clone();
                        mgr.handle_sfu_announce(
                            did,
                            room_id.clone(),
                            participant_count,
                            capacity_hint,
                        );

                        // Evict nodes that stopped announcing (>30s stale).
                        // Cheap: one HashMap scan per Announce.
                        let evicted = mgr.evict_stale_nodes(Duration::from_secs(30));
                        for (evicted_room, evicted_did) in evicted {
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
                        if let Some(pid) = mgr.remove_node_from_room(&room_id, &did) {
                            pipes_to_drop.push(pid);
                        }
                    }
                    CascadeSignal::PipeOffer {
                        from_did,
                        to_did,
                        room_id,
                        sdp_offer,
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
            for (participant_id, sdp_offer_json, remote_did, room_id) in pipe_renegotiation_offers {
                if let Err(e) = self
                    .server
                    .command_tx
                    .send(SfuCommand::ApplyPipeRenegotiationOffer {
                        participant_id,
                        sdp_offer_json,
                        remote_did,
                        room_id,
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

            for (target, signal) in outbound {
                if let Err(e) = self.gossip.send(target, signal).await {
                    debug!("SFU cascade: gossip send failed: {}", e);
                }
            }
        }
    }

    /// Cascade cleanup loop: two duties.
    ///
    /// 1. **Dead pipe notifications** — the server event loop detects
    ///    pipe peers whose `Rtc::is_alive()` returns false and pushes
    ///    them here.  We remove the corresponding `PipeMeta` entry
    ///    from the cascade manager so `cascade_established_pipe_count`
    ///    reflects reality.
    ///
    /// 2. **Periodic stale-node sweep** — every 5 seconds, run
    ///    `evict_stale_nodes(30s)` regardless of whether an Announce
    ///    arrived.  Without this, a crashed node that stops announcing
    ///    never gets evicted because the sweep only ran inside the
    ///    Announce handler.
    async fn run_cascade_cleanup(
        self: Arc<Self>,
        mut dead_pipe_rx: tokio::sync::mpsc::Receiver<super::server::DeadPipe>,
    ) {
        let mut sweep_interval = tokio::time::interval(Duration::from_secs(5));
        // The first tick fires immediately; skip it so the sweep starts
        // 5 seconds after boot (everything is healthy at startup).
        sweep_interval.tick().await;

        loop {
            tokio::select! {
                dp = dead_pipe_rx.recv() => {
                    let Some(dp) = dp else { return; };
                    info!(
                        "SFU cascade cleanup: pipe peer {} (room {} ↔ {}) died, removing",
                        dp.participant_id, dp.room_id, dp.remote_did
                    );
                    let mut mgr = self.cascade_manager.write().await;
                    mgr.remove_node_from_room(&dp.room_id, &dp.remote_did);
                }
                _ = sweep_interval.tick() => {
                    let mut pipes_to_drop: Vec<ParticipantId> = Vec::new();
                    {
                        let mut mgr = self.cascade_manager.write().await;
                        let evicted = mgr.evict_stale_nodes(Duration::from_secs(30));
                        for (room_id, did) in evicted {
                            info!(
                                "SFU cascade sweep: evicting stale node {} from room {}",
                                did, room_id
                            );
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

                    // ── Rebalance check ──
                    // Walk local rooms, ask the cascade manager if any
                    // room exceeds the 90% threshold with a viable
                    // target node.  suggest_rebalance() enforces its
                    // own 30-second per-room cooldown, so running this
                    // on every 5s tick adds no extra migration chatter.
                    let migrate_events = {
                        let rooms = self.rooms.read().await;
                        let mut mgr = self.cascade_manager.write().await;
                        let mut events: Vec<SfuMigrateEvent> = Vec::new();
                        for room in rooms.list_rooms() {
                            let local_count = room.participant_count() as u32;
                            let room_id_str = room.id.to_string();
                            if let Some(target_did) =
                                mgr.suggest_rebalance(&room_id_str, local_count)
                            {
                                // Pick the most-recently-joined non-pipe
                                // participant — least disruption to
                                // established sessions.
                                if let Some(victim) = room
                                    .participants
                                    .values()
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
                            if let Ok(json) = serde_json::to_string(&evt) {
                                pubsub
                                    .publish(
                                        &crate::pubsub::SFU_MIGRATE_TOPIC,
                                        &json,
                                    )
                                    .await;
                            }
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
        let info = self.room_to_info(room);
        // Drop the write lock before awaiting the async announce.
        drop(configs);
        drop(rooms);
        // Broadcast the new room so cascade peers learn our capacity
        // BEFORE any clients join.  Without this, pick_redirect_node
        // sees an empty known_nodes map and accepts all joins locally.
        self.announce_room(&room_id.to_string(), 0).await;
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
    pub async fn call_join(
        &self,
        neighbourhood_url: &str,
        room_name: &str,
        agent_did: &str,
        sdp_offer_json: &str,
    ) -> Result<CallSessionInfo, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let pid = ParticipantId::next();

        // Cascade redirect check + participant add in a single lock
        // scope to prevent TOCTOU races where concurrent joins both
        // read the same stale count and both accept.
        {
            let mgr = self.cascade_manager.read().await;
            let mut rooms = self.rooms.write().await;
            let configs = self.configs.read().await;

            // Ensure room exists — use max_participants_per_node as the
            // hard cap, not max_mesh_participants (which controls the
            // mesh→SFU topology threshold).
            let max = Some(mgr.max_participants_per_node() as usize);
            rooms.create_room(room_id.clone(), max).ok(); // idempotent

            let local_count = rooms
                .get_room(&room_id)
                .map(|r| r.participant_count() as u32)
                .unwrap_or(0);
            let preferred = configs
                .get(neighbourhood_url)
                .and_then(|c| c.preferred_sfu_did.as_deref());

            if let Some(node) = mgr.pick_redirect_node(&room_id.to_string(), local_count, preferred)
            {
                // Redirect to a remote node (may or may not have
                // headroom — client-side cycle detection handles the
                // all-full case).
                return Ok(CallSessionInfo {
                    room_name: room_name.to_string(),
                    neighbourhood_url: neighbourhood_url.to_string(),
                    participant_id: String::new(),
                    sdp_answer: String::new(),
                    redirect_to: Some(node.did.clone()),
                    stream_mapping: Vec::new(),
                });
            } else {
                // Accept locally — add participant while we still
                // hold the write lock.
                let room = rooms
                    .get_room_mut(&room_id)
                    .ok_or_else(|| RoomError::NotFound.to_string())?;
                room.add_participant(pid.clone(), agent_did.to_string())
                    .map_err(|e| e.to_string())?;
            }
        }

        // Parse SDP offer and create Rtc instance
        let offer: SdpOffer = serde_json::from_str(sdp_offer_json)
            .map_err(|e| format!("Invalid SDP offer: {}", e))?;

        let (rtc, sdp_answer) = SfuServer::create_rtc_for_offer(offer, self.server.local_addr)?;

        // Create the SFU peer and send it to the event loop
        let peer = SfuPeer::new(
            pid.clone(),
            room_id.clone(),
            agent_did.to_string(),
            rtc,
            false,
        );

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
                let mapping: Vec<String> = room
                    .participants
                    .values()
                    .filter(|p| p.id != pid)
                    .map(|p| format!("{}:{}", p.id.0, p.agent_did))
                    .collect();
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

        let pid = room
            .participant_id_for_did(agent_did)
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

        // Always re-announce the fresh local count (lets healthy peers
        // refresh their view).  If the room is now empty on this node,
        // also send a directed Leave so peers prune their
        // `known_nodes` entry for this (room, did) tuple — without
        // Leave, the lifetime of a known_nodes entry is bounded only
        // by the next non-zero Announce from us, which for a vacated
        // room will never come.
        let room_key = room_id.to_string();
        self.announce_room(&room_key, local_count).await;
        if is_empty {
            let signal = CascadeSignal::Leave {
                did: self.gossip.local_did().to_string(),
                room_id: room_key.clone(),
            };
            if let Err(e) = self.gossip.send(GossipTarget::Broadcast, signal).await {
                warn!("SFU call_leave: gossip Leave broadcast failed: {}", e);
            }
        }

        // Clean up local quality preference tracking and re-propagate
        // the aggregate to pipe peers (the max may have changed).
        {
            let mut prefs = self.local_quality_preferences.write().await;
            if let Some(room_prefs) = prefs.get_mut(&room_key) {
                room_prefs.remove(agent_did);
                if room_prefs.is_empty() {
                    prefs.remove(&room_key);
                }
            }
        }
        self.propagate_quality_preference(&room_key).await;

        Ok(true)
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
        sdp_answer_json: &str,
    ) -> Result<bool, String> {
        let room_id = RoomId::new(neighbourhood_url, room_name);
        let rooms = self.rooms.read().await;
        let room = rooms
            .get_room(&room_id)
            .ok_or_else(|| RoomError::NotFound.to_string())?;
        let pid = room
            .participant_id_for_did(agent_did)
            .ok_or_else(|| "Agent not in room".to_string())?;
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

    /// Number of fully-established pipe transports to other SFU nodes
    /// across all rooms.  Used by the wind tunnel cascade scenarios to
    /// assert that auto-establish + the gossip-driven offer/answer
    /// round-trip lights up node-to-node pipes.
    pub async fn cascade_established_pipe_count(&self) -> usize {
        let mgr = self.cascade_manager.read().await;
        mgr.established_pipe_count()
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

        let pid = room
            .participant_id_for_did(agent_did)
            .ok_or_else(|| "Agent not in room".to_string())?;

        self.server
            .command_tx
            .send(SfuCommand::SetQualityPreference {
                participant_id: pid,
                preference: preference.to_string(),
            })
            .await
            .map_err(|e| format!("Failed to send quality preference command: {}", e))?;

        // Drop rooms lock before acquiring other locks.
        drop(rooms);

        // Track this preference for cascade aggregation and propagate.
        let room_key = room_id.to_string();
        {
            let mut prefs = self.local_quality_preferences.write().await;
            prefs
                .entry(room_key.clone())
                .or_default()
                .insert(agent_did.to_string(), preference.to_string());
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
            created_at_ms: room.created_at,
        }
    }
}
