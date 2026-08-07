//! Cascaded SFU — manages pipe transports between SFU nodes in a cluster.
//!
//! When multiple executor nodes act as SFU peers for the same neighbourhood,
//! they establish str0m peer connections ("pipe transports") between each
//! other to relay media tracks across the cluster.
//!
//! ## Ownership of pipe RTC instances
//!
//! Pipe transports are str0m [`Rtc`] peer connections, but unlike client
//! peers they have no human on the far end — they're SFU↔SFU bridges.
//! The RTC itself lives in the SFU server's `peers: HashMap<ParticipantId,
//! SfuPeer>` map (flagged with `is_pipe = true`) so the event loop drives
//! it uniformly with client peers.  The [`CascadeManager`] only retains
//! [`PipeMeta`] — the bookkeeping needed to:
//!
//! - look up the pipe's `ParticipantId` by `(room_id, remote_did)` when an
//!   inbound signal arrives;
//! - know whether the SDP round-trip has completed
//!   (`established` flag);
//! - prune entries when the gossip layer reports a remote node leaving.

use std::collections::HashMap;
use std::net::SocketAddr;
use std::time::{Duration, Instant};

use log::{info, warn};
use str0m::change::SdpOffer;
use str0m::{Candidate, Rtc};

use super::room::{ParticipantId, RoomId};
use super::server::SfuPeer;

/// Represents a remote SFU node in the cascade cluster.
#[derive(Debug, Clone)]
pub struct SfuNodeInfo {
    pub did: String,
    pub participant_count: u32,
    pub capacity_hint: u32,
    /// Last time this node sent an Announce.  Used by the TTL eviction
    /// sweep to prune nodes that stopped announcing.
    pub last_seen: Instant,
}

/// Bookkeeping for an inter-SFU pipe transport — see the module docs.
///
/// The actual [`Rtc`] lives in the server `peers` map under
/// [`participant_id`].  We hold this metadata in `CascadeManager` so
/// gossip-driven flows (`handle_pipe_answer`, `remove_node`,
/// auto-establish) can resolve a pipe by `(room_id, remote_did)`
/// without having to walk the peers map.
#[derive(Debug, Clone)]
pub struct PipeMeta {
    pub remote_did: String,
    pub room_id: RoomId,
    pub participant_id: ParticipantId,
    pub established: bool,
}

/// Messages used for SFU cluster discovery and pipe transport signalling.
/// These are sent via neighbourhood signalling channels.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type")]
pub enum CascadeSignal {
    /// Broadcast: "I am an SFU node for this room"
    #[serde(rename = "sfu-announce")]
    Announce {
        did: String,
        room_id: String,
        participant_count: u32,
        capacity_hint: u32,
    },
    /// SDP offer to establish a pipe transport between two SFU nodes
    #[serde(rename = "sfu-pipe-offer")]
    PipeOffer {
        from_did: String,
        to_did: String,
        room_id: String,
        sdp_offer: String,
    },
    /// SDP answer for pipe transport
    #[serde(rename = "sfu-pipe-answer")]
    PipeAnswer {
        from_did: String,
        to_did: String,
        room_id: String,
        sdp_answer: String,
    },
    /// SFU node is leaving the cluster
    #[serde(rename = "sfu-leave")]
    Leave { did: String, room_id: String },
    /// Quality preference propagation — a remote node's local
    /// subscribers want this quality level for simulcast media
    /// forwarded through the pipe.
    #[serde(rename = "sfu-quality-preference")]
    QualityPreference {
        from_did: String,
        room_id: String,
        /// Aggregate preference across all local subscribers on the
        /// sending node: "low", "medium", "high", or "auto".
        preference: String,
    },
}

impl CascadeSignal {
    /// Human-readable variant name for debug/trace logging.
    pub fn variant_name(&self) -> &'static str {
        match self {
            CascadeSignal::Announce { .. } => "Announce",
            CascadeSignal::Leave { .. } => "Leave",
            CascadeSignal::PipeOffer { .. } => "PipeOffer",
            CascadeSignal::PipeAnswer { .. } => "PipeAnswer",
            CascadeSignal::QualityPreference { .. } => "QualityPreference",
        }
    }
}

/// Manages the cascade cluster for a single SFU node.
///
/// Tracks known peer SFU nodes, the bookkeeping side of pipe transports
/// (the RTC instances themselves live in the server peers map — see
/// [`PipeMeta`]), and handles forwarding decisions for cross-node media
/// relay.
pub struct CascadeManager {
    /// Our DID
    local_did: String,
    /// Local SFU server address for creating pipe transport RTCs
    local_addr: SocketAddr,
    /// Known SFU nodes per room: room_id_str -> (did -> node_info)
    known_nodes: HashMap<String, HashMap<String, SfuNodeInfo>>,
    /// Active pipe transport metadata keyed by `(room_id_str, remote_did)`.
    /// The actual `Rtc` lives in the server `peers` map under
    /// `meta.participant_id`.
    pipes: HashMap<(String, String), PipeMeta>,
    /// Max participants this node will accept per room
    max_participants_per_node: u32,
    /// Per-room cooldown: last time a rebalance migration was issued.
    /// Prevents rapid-fire migrations (anti-pingpong).
    rebalance_cooldowns: HashMap<String, Instant>,
}

impl CascadeManager {
    pub fn new(local_did: String, local_addr: SocketAddr, max_participants_per_node: u32) -> Self {
        Self {
            local_did,
            local_addr,
            known_nodes: HashMap::new(),
            pipes: HashMap::new(),
            max_participants_per_node,
            rebalance_cooldowns: HashMap::new(),
        }
    }

    /// Maximum participants this node accepts per room.
    pub fn max_participants_per_node(&self) -> u32 {
        self.max_participants_per_node
    }

    /// Handle an incoming SFU announce from a peer node.
    pub fn handle_sfu_announce(
        &mut self,
        did: String,
        room_id: String,
        participant_count: u32,
        capacity_hint: u32,
    ) {
        if did == self.local_did {
            return; // Ignore our own announces
        }

        let nodes = self.known_nodes.entry(room_id).or_default();
        nodes.insert(
            did.clone(),
            SfuNodeInfo {
                did,
                participant_count,
                capacity_hint,
                last_seen: Instant::now(),
            },
        );
    }

    /// Evict nodes that have not announced within `max_age`.  Returns
    /// the list of evicted `(room_id_str, did)` pairs so the caller
    /// can tear down any associated pipes.
    pub fn evict_stale_nodes(&mut self, max_age: Duration) -> Vec<(String, String)> {
        let mut evicted: Vec<(String, String)> = Vec::new();
        for (room_id, nodes) in self.known_nodes.iter_mut() {
            let stale_dids: Vec<String> = nodes
                .iter()
                .filter(|(_, info)| info.last_seen.elapsed() > max_age)
                .map(|(did, _)| did.clone())
                .collect();
            for did in stale_dids {
                nodes.remove(&did);
                warn!(
                    "Cascade: evicted stale node {} from room {} (no announce for {:?})",
                    did, room_id, max_age
                );
                evicted.push((room_id.clone(), did));
            }
        }
        // Clean up empty room entries.
        self.known_nodes.retain(|_, nodes| !nodes.is_empty());
        evicted
    }

    /// Create an SDP offer to establish a pipe transport to a remote SFU
    /// node.  Returns the freshly-built [`SfuPeer`] (with pending offer
    /// already queued on the RTC) for the caller to insert into the
    /// server peers map, plus the [`CascadeSignal::PipeOffer`] to ship
    /// over gossip.
    ///
    /// CascadeManager records bookkeeping in [`PipeMeta`] so subsequent
    /// inbound signals can be routed back to the right participant.
    pub fn establish_pipe(
        &mut self,
        remote_did: &str,
        room_id: &RoomId,
    ) -> Result<(SfuPeer, CascadeSignal), String> {
        let room_key = room_id.to_string();
        let pipe_key = (room_key.clone(), remote_did.to_string());

        if self.pipes.contains_key(&pipe_key) {
            return Err("Pipe transport already exists".to_string());
        }

        let mut rtc = Rtc::builder().build();
        let candidate = Candidate::host(self.local_addr, "udp")
            .map_err(|e| format!("Failed to create candidate: {}", e))?;
        rtc.add_local_candidate(candidate);

        // Create offer via SDP API.  str0m 0.9 requires at least one
        // queued change before `apply()` produces an offer, so we open
        // a control data channel for the pipe transport.  Media
        // transceivers are added later via the renegotiation pipeline
        // when local peers add tracks.
        let mut sdp_api = rtc.sdp_api();
        let _control_channel = sdp_api.add_channel("pipe-control".to_string());
        let (offer, pending) = sdp_api
            .apply()
            .ok_or_else(|| "Failed to create pipe offer: no changes to apply".to_string())?;

        let sdp_offer = serde_json::to_string(&offer)
            .map_err(|e| format!("Failed to serialize offer: {}", e))?;

        let participant_id = ParticipantId::next();
        let mut peer = SfuPeer::new(
            participant_id.clone(),
            room_id.clone(),
            remote_did.to_string(),
            rtc,
            true,
        );
        peer.pending_offer = Some(pending);

        self.pipes.insert(
            pipe_key,
            PipeMeta {
                remote_did: remote_did.to_string(),
                room_id: room_id.clone(),
                participant_id,
                established: false,
            },
        );

        Ok((
            peer,
            CascadeSignal::PipeOffer {
                from_did: self.local_did.clone(),
                to_did: remote_did.to_string(),
                room_id: room_key,
                sdp_offer,
            },
        ))
    }

    /// Handle an incoming pipe offer from a remote SFU node.  Builds the
    /// pipe-side [`SfuPeer`] (answer already applied) for the caller to
    /// insert into the server peers map, plus the
    /// [`CascadeSignal::PipeAnswer`] to ship back over gossip.
    pub fn handle_pipe_offer(
        &mut self,
        from_did: &str,
        room_id: &str,
        sdp_offer_json: &str,
    ) -> Result<(SfuPeer, CascadeSignal), String> {
        let offer: SdpOffer = serde_json::from_str(sdp_offer_json)
            .map_err(|e| format!("Invalid pipe SDP offer: {}", e))?;

        let mut rtc = Rtc::builder().build();
        let candidate = Candidate::host(self.local_addr, "udp")
            .map_err(|e| format!("Failed to create candidate: {}", e))?;
        rtc.add_local_candidate(candidate);

        let answer = rtc
            .sdp_api()
            .accept_offer(offer)
            .map_err(|e| format!("Failed to accept pipe offer: {}", e))?;

        let sdp_answer = serde_json::to_string(&answer)
            .map_err(|e| format!("Failed to serialize pipe answer: {}", e))?;

        let parsed_room = RoomId::parse(room_id);

        let participant_id = ParticipantId::next();
        let peer = SfuPeer::new(
            participant_id.clone(),
            parsed_room.clone(),
            from_did.to_string(),
            rtc,
            true,
        );

        let pipe_key = (room_id.to_string(), from_did.to_string());
        self.pipes.insert(
            pipe_key,
            PipeMeta {
                remote_did: from_did.to_string(),
                room_id: parsed_room,
                participant_id,
                established: true,
            },
        );

        Ok((
            peer,
            CascadeSignal::PipeAnswer {
                from_did: self.local_did.clone(),
                to_did: from_did.to_string(),
                room_id: room_id.to_string(),
                sdp_answer,
            },
        ))
    }

    /// Handle an incoming pipe answer from a remote SFU node.  Returns
    /// the local pipe's `ParticipantId` and the raw answer JSON so the
    /// caller can dispatch
    /// `SfuCommand::ApplyServerAnswer { participant_id, sdp_answer_json }`
    /// to the SFU event loop (the pipe's RTC lives in the peers map).
    pub fn handle_pipe_answer(
        &mut self,
        from_did: &str,
        room_id: &str,
        sdp_answer_json: &str,
    ) -> Result<(ParticipantId, String), String> {
        let pipe_key = (room_id.to_string(), from_did.to_string());
        let meta = self
            .pipes
            .get_mut(&pipe_key)
            .ok_or_else(|| "No pending pipe transport for this node".to_string())?;
        meta.established = true;
        info!(
            "Pipe transport established to SFU node {} for room {}",
            from_did, room_id
        );
        Ok((meta.participant_id.clone(), sdp_answer_json.to_string()))
    }

    /// Targeted leave: drop just the `(room, did)` entry instead of
    /// purging the node from every room.  Returns the removed pipe's
    /// `ParticipantId` if there was one.
    pub fn remove_node_from_room(&mut self, room_id: &str, did: &str) -> Option<ParticipantId> {
        if let Some(nodes) = self.known_nodes.get_mut(room_id) {
            nodes.remove(did);
            if nodes.is_empty() {
                self.known_nodes.remove(room_id);
            }
        }
        let key = (room_id.to_string(), did.to_string());
        if let Some(meta) = self.pipes.remove(&key) {
            info!(
                "Removed pipe transport to SFU node {} for room {}",
                did, room_id
            );
            Some(meta.participant_id)
        } else {
            None
        }
    }

    /// Pick the least-loaded SFU node for a new participant.
    /// Returns None if this local node should accept the participant.
    ///
    /// Looks up nodes under the specific `room_id` first, falls back to
    /// the catch-all empty-string entry (populated by statically-configured
    /// clusters that haven't gone through the gossip announce path yet).
    ///
    /// `local_count` tracks only direct WebRTC peer connections on this
    /// node — remote (cascade-piped) participants connect to their own
    /// SFU node and do not consume local capacity.
    ///
    /// Rule: when the local node has capacity AND no preferred node
    /// pulls the join elsewhere, accept.  When at capacity, redirect
    /// to a peer with headroom.
    ///
    /// `preferred_did` — when `Some`, the cascade preferentially
    /// routes to that node (even if the local node has capacity),
    /// unless the preferred node has no headroom.  This lets a
    /// neighbourhood funnel most participants to a powerful hosted
    /// multi-user node while lighter personal executors absorb
    /// overflow.
    ///
    /// Tie-breaking among eligible non-preferred nodes uses the
    /// least-loaded heuristic.  No proactive rebalancing — the wind
    /// tunnel cycle tests caught a pingpong between under-loaded
    /// peers when a threshold-based rebalancing heuristic was active.
    /// Decide whether to accept, redirect, or reject a join.
    ///
    /// Returns:
    /// - `None` — accept locally (we have capacity, or no remote
    ///   nodes exist / cluster view degraded).
    /// - `Some(node)` — redirect to this remote node (it may also
    ///   be full — client-side cycle detection handles that case).
    pub fn pick_redirect_node(
        &self,
        room_id: &str,
        local_count: u32,
        preferred_did: Option<&str>,
    ) -> Option<&SfuNodeInfo> {
        // If a preferred node exists and it lives in the cluster (not
        // us), try to route there first — even when the local node
        // has capacity.
        if let Some(pref) = preferred_did {
            if pref != self.local_did {
                let nodes = self
                    .known_nodes
                    .get(room_id)
                    .or_else(|| self.known_nodes.get(""));
                if let Some(map) = nodes {
                    if let Some(pref_node) = map.get(pref) {
                        if pref_node.participant_count < pref_node.capacity_hint {
                            return Some(pref_node);
                        }
                    }
                }
            }
            // Preferred node points to us — fall through to normal
            // local-capacity check.
        }

        if local_count < self.max_participants_per_node {
            return None;
        }
        // Local node full — find a remote node with headroom, or
        // redirect to the least-loaded one so the client can detect
        // the redirect cycle.
        let nodes = self
            .known_nodes
            .get(room_id)
            .or_else(|| self.known_nodes.get(""));
        match nodes {
            Some(map) if !map.is_empty() => {
                // Prefer a node with actual headroom.
                map.values()
                    .filter(|n| n.participant_count < n.capacity_hint)
                    .min_by_key(|n| n.participant_count)
                    // All full — redirect to least-loaded anyway
                    // (client detects the cycle and surfaces overflow).
                    .or_else(|| map.values().min_by_key(|n| n.participant_count))
            }
            // No remote nodes known (single-node or partitioned).
            // Accept locally — overflow beats rejection when the
            // cluster view has degraded.
            _ => None,
        }
    }

    /// Lookup pipe metadata by `(room_id, remote_did)`.  The pipe's
    /// `Rtc` lives in the server peers map under `meta.participant_id`.
    pub fn pipe_meta(&self, room_id: &str, remote_did: &str) -> Option<&PipeMeta> {
        self.pipes
            .get(&(room_id.to_string(), remote_did.to_string()))
    }

    /// Our local DID (used by gossip dispatch to filter self-targeted signals).
    pub fn local_did(&self) -> &str {
        &self.local_did
    }

    /// Is there an active pipe to `remote_did` for `room_id`?
    pub fn has_pipe(&self, room_id: &str, remote_did: &str) -> bool {
        self.pipes
            .contains_key(&(room_id.to_string(), remote_did.to_string()))
    }

    /// Count of fully-established pipes (offer + answer round-trip
    /// complete).  Used by the wind tunnel to assert the Phase E e2e
    /// pipe handshake landed.
    pub fn established_pipe_count(&self) -> usize {
        self.pipes.values().filter(|p| p.established).count()
    }

    /// Evaluate whether the local node should migrate one participant
    /// to a less-loaded peer.  Returns `Some(target_did)` when a
    /// migration makes sense.  The caller picks the participant and
    /// publishes an `SfuMigrateEvent`.
    ///
    /// Anti-pingpong measures:
    /// 1. Per-room 30-second cooldown between migrations.
    /// 2. Local load must exceed 90% of capacity.
    /// 3. A peer must have at least 30% free capacity.
    /// 4. The gap between local and target must exceed 3 participants.
    ///    (Moving one closes the gap by 2: local -1, target +1.
    ///    A gap of 3 means the post-move gap drops to 1 — stable.)
    ///
    /// Returns `None` when no migration action makes sense.
    pub fn suggest_rebalance(
        &mut self,
        room_id: &str,
        local_count: u32,
    ) -> Option<String> {
        // Cooldown check: skip if we migrated from this room recently
        if let Some(last) = self.rebalance_cooldowns.get(room_id) {
            if last.elapsed() < Duration::from_secs(30) {
                return None;
            }
        }

        // Only consider rebalancing when above 90% capacity
        let threshold = (self.max_participants_per_node as f64 * 0.9).ceil() as u32;
        if local_count < threshold {
            return None;
        }

        let nodes = self
            .known_nodes
            .get(room_id)
            .or_else(|| self.known_nodes.get(""))?;

        // Find the peer with the most headroom
        let best = nodes
            .values()
            .filter(|n| {
                let free = n.capacity_hint.saturating_sub(n.participant_count);
                let free_pct = if n.capacity_hint > 0 {
                    free as f64 / n.capacity_hint as f64
                } else {
                    0.0
                };
                // Peer must have at least 30% free capacity
                free_pct >= 0.3
            })
            .max_by_key(|n| n.capacity_hint.saturating_sub(n.participant_count))?;

        // Minimum gap of 4 participants to justify migration.
        // Moving one participant closes the gap by 2 (local -1,
        // target +1).  With gap >= 4, post-move gap = gap - 2 >= 2,
        // which keeps the next tick below the threshold.
        let gap = local_count.saturating_sub(best.participant_count);
        if gap < 4 {
            return None;
        }

        Some(best.did.clone())
    }

    /// Record that a migration was issued for `room_id`.  Starts the
    /// cooldown timer so `suggest_rebalance` suppresses further
    /// migrations for 30 seconds.
    pub fn record_rebalance(&mut self, room_id: &str) {
        self.rebalance_cooldowns
            .insert(room_id.to_string(), Instant::now());
    }

    /// All established pipes in a specific room, as
    /// `(remote_did, participant_id)` pairs.  Used by the quality
    /// preference propagation path to iterate pipe peers per room.
    pub fn pipe_peers_for_room(&self, room_id: &str) -> Vec<(String, ParticipantId)> {
        self.pipes
            .iter()
            .filter(|((rid, _), meta)| rid == room_id && meta.established)
            .map(|((_, remote_did), meta)| (remote_did.clone(), meta.participant_id.clone()))
            .collect()
    }

    /// All `(room_id, remote_did)` keys for established pipes — for
    /// the `sfu.cascadePipeStatus` query so the wind tunnel can verify
    /// which specific node-pairs have a live pipe.
    pub fn established_pipes(&self) -> Vec<(String, String)> {
        self.pipes
            .iter()
            .filter(|(_, p)| p.established)
            .map(|(k, _)| k.clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::SocketAddr;

    fn test_addr(port: u16) -> SocketAddr {
        format!("127.0.0.1:{}", port).parse().unwrap()
    }

    #[test]
    fn test_cascade_pipe_offer_answer() {
        let mut node_a = CascadeManager::new("did:key:nodeA".into(), test_addr(10003), 8);
        let mut node_b = CascadeManager::new("did:key:nodeB".into(), test_addr(10004), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // node_a creates pipe offer (also returns the dialer-side SfuPeer
        // that the SFU server would normally insert into its peers map).
        let (peer_a, offer_signal) = node_a.establish_pipe("did:key:nodeB", &room_id).unwrap();
        assert!(peer_a.is_pipe);
        assert_eq!(peer_a.agent_did, "did:key:nodeB");
        let sdp_offer = match &offer_signal {
            CascadeSignal::PipeOffer {
                sdp_offer,
                from_did,
                to_did,
                ..
            } => {
                assert_eq!(from_did, "did:key:nodeA");
                assert_eq!(to_did, "did:key:nodeB");
                sdp_offer.clone()
            }
            _ => panic!("Expected PipeOffer signal"),
        };

        // node_b handles offer, produces answer (and the receiver-side
        // SfuPeer).
        let (peer_b, answer_signal) = node_b
            .handle_pipe_offer("did:key:nodeA", &room_id.to_string(), &sdp_offer)
            .unwrap();
        assert!(peer_b.is_pipe);
        assert_eq!(peer_b.agent_did, "did:key:nodeA");
        let sdp_answer = match &answer_signal {
            CascadeSignal::PipeAnswer {
                sdp_answer,
                from_did,
                to_did,
                ..
            } => {
                assert_eq!(from_did, "did:key:nodeB");
                assert_eq!(to_did, "did:key:nodeA");
                sdp_answer.clone()
            }
            _ => panic!("Expected PipeAnswer signal"),
        };

        // node_a handles answer (returns the participant_id to look up
        // the pipe SfuPeer in the server peers map).
        let (apply_pid, apply_sdp) = node_a
            .handle_pipe_answer("did:key:nodeB", &room_id.to_string(), &sdp_answer)
            .unwrap();
        assert_eq!(apply_pid, peer_a.id);
        assert_eq!(apply_sdp, sdp_answer);

        // Duplicate pipe should error
        assert!(node_a.establish_pipe("did:key:nodeB", &room_id).is_err());
    }

    #[test]
    fn test_cascade_pick_redirect() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10007), 4);
        let room_id = RoomId::new("test-nh", "room1");

        // Add remote node with low load
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 1, 8);

        // local_count=5 (over capacity=4) → must redirect
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 5, None);
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // local_count=2 (under capacity) → accept locally
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2, None);
        assert!(redirect.is_none());

        // local_count=2, remote has 0 → still under local cap, accept
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 0, 8);
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2, None);
        assert!(redirect.is_none());

        // local_count=4 (at capacity), remote has 1 → must redirect
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 1, 8);
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 4, None);
        assert!(redirect.is_some());

        // local_count=4, ALL remote nodes also full → still redirect
        // (client-side cycle detection handles the overflow).
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 8, 8);
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 4, None);
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // ── Partition-safe: no known nodes → accept locally ──
        let fresh_mgr = CascadeManager::new("did:key:local".into(), test_addr(10008), 4);
        let redirect = fresh_mgr.pick_redirect_node(&room_id.to_string(), 5, None);
        assert!(redirect.is_none()); // overflow locally, no remote view

        // ── Preferred-node routing ──
        // When a preferred node has capacity, redirect there even if
        // the local node also has capacity.
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 0, 8);
        let redirect = mgr.pick_redirect_node(
            &room_id.to_string(),
            1,                             // local well under capacity
            Some("did:key:remote"),         // but remote is preferred
        );
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // When preferred node is at capacity, fall through to normal logic
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 8, 8);
        let redirect = mgr.pick_redirect_node(
            &room_id.to_string(),
            1,
            Some("did:key:remote"),
        );
        assert!(redirect.is_none()); // local has capacity, preferred full

        // When preferred DID matches local, no redirect (we ARE the preferred)
        let redirect = mgr.pick_redirect_node(
            &room_id.to_string(),
            1,
            Some("did:key:local"),
        );
        assert!(redirect.is_none());
    }

    #[test]
    fn test_cascade_pipe_meta_lookup() {
        let mgr = CascadeManager::new("did:key:local".into(), test_addr(10009), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // No pipes registered → lookup returns None.
        assert!(mgr
            .pipe_meta(&room_id.to_string(), "did:key:remote")
            .is_none());
    }

    #[test]
    fn test_rebalance_below_threshold() {
        // With capacity=10, threshold = ceil(10*0.9) = 9.
        // local_count=8 stays below threshold → no suggestion.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10010), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10);
        assert!(mgr.suggest_rebalance(room, 8).is_none());
    }

    #[test]
    fn test_rebalance_above_threshold_with_target() {
        // capacity=10, threshold=9. local=9, peer has 2/10 (80% free).
        // gap=9-2=7 >= 4. Peer has 80% free >= 30%.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10011), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10);
        let target = mgr.suggest_rebalance(room, 9);
        assert_eq!(target.as_deref(), Some("did:key:peer"));
    }

    #[test]
    fn test_rebalance_cooldown() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10012), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10);

        // First call succeeds
        assert!(mgr.suggest_rebalance(room, 9).is_some());
        mgr.record_rebalance(room);

        // Second call within 30s returns None (cooldown active)
        assert!(mgr.suggest_rebalance(room, 9).is_none());
    }

    #[test]
    fn test_rebalance_gap_too_small() {
        // Peer has 7/10 participants. local=9. gap=9-7=2 < 4 → no migration.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10013), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 7, 10);
        assert!(mgr.suggest_rebalance(room, 9).is_none());
    }

    #[test]
    fn test_rebalance_peer_too_full() {
        // Peer has 8/10 (only 20% free < 30% threshold) → no migration.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10014), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 8, 10);
        assert!(mgr.suggest_rebalance(room, 10).is_none());
    }

    #[test]
    fn test_rebalance_picks_emptiest_peer() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10015), 20);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peerA".into(), room.into(), 5, 20);
        mgr.handle_sfu_announce("did:key:peerB".into(), room.into(), 2, 20);
        // Both qualify (>= 30% free, gap >= 4). peerB has most headroom.
        let target = mgr.suggest_rebalance(room, 18);
        assert_eq!(target.as_deref(), Some("did:key:peerB"));
    }

    #[test]
    fn test_rebalance_separate_room_cooldown() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10016), 10);
        let room_a = "nh://test:roomA";
        let room_b = "nh://test:roomB";
        mgr.handle_sfu_announce("did:key:peer".into(), room_a.into(), 2, 10);
        mgr.handle_sfu_announce("did:key:peer".into(), room_b.into(), 2, 10);

        // Rebalance room A and record cooldown
        assert!(mgr.suggest_rebalance(room_a, 9).is_some());
        mgr.record_rebalance(room_a);

        // Room B should still allow rebalancing (independent cooldown)
        assert!(mgr.suggest_rebalance(room_b, 9).is_some());
    }

}
