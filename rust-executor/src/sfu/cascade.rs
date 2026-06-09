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

use log::info;
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
}

impl CascadeManager {
    pub fn new(local_did: String, local_addr: SocketAddr, max_participants_per_node: u32) -> Self {
        Self {
            local_did,
            local_addr,
            known_nodes: HashMap::new(),
            pipes: HashMap::new(),
            max_participants_per_node,
        }
    }

    /// Generate an announce signal for broadcasting.
    pub fn announce_sfu_node(&self, room_id: &RoomId, participant_count: u32) -> CascadeSignal {
        CascadeSignal::Announce {
            did: self.local_did.clone(),
            room_id: room_id.to_string(),
            participant_count,
            capacity_hint: self.max_participants_per_node,
        }
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
            },
        );
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
        let peer = SfuPeer {
            id: participant_id.clone(),
            room_id: room_id.clone(),
            agent_did: remote_did.to_string(),
            rtc,
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
            pending_offer: Some(pending),
            is_pipe: true,
        };

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

        // Parse room_id from the string format
        // `{neighbourhood_url}:{room_name}`.  Neighbourhood URLs
        // contain their own `://`, so split on the LAST `:`.
        let (nh_url, room_name) = room_id.rsplit_once(':').unwrap_or((room_id, "default"));
        let parsed_room = RoomId::new(nh_url, room_name);

        let participant_id = ParticipantId::next();
        let peer = SfuPeer {
            id: participant_id.clone(),
            room_id: parsed_room.clone(),
            agent_did: from_did.to_string(),
            rtc,
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
            pending_offer: None,
            is_pipe: true,
        };

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

    /// Remove an SFU node from the cluster (handles sfu-leave).
    /// Returns the list of `ParticipantId`s of pipes that were removed
    /// so the caller can dispatch `SfuCommand::RemovePeer` for each.
    pub fn remove_node(&mut self, did: &str) -> Vec<ParticipantId> {
        // Remove from known nodes
        for nodes in self.known_nodes.values_mut() {
            nodes.remove(did);
        }

        // Remove pipe metadata for this DID across all rooms.
        let keys_to_remove: Vec<_> = self
            .pipes
            .keys()
            .filter(|(_, d)| d == did)
            .cloned()
            .collect();

        let mut removed_pids = Vec::new();
        for key in keys_to_remove {
            if let Some(meta) = self.pipes.remove(&key) {
                info!("Removed pipe transport to SFU node {}", did);
                removed_pids.push(meta.participant_id);
            }
        }
        removed_pids
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

    /// Get known SFU nodes for a room (for the sfuNodesForRoom query).
    pub fn nodes_for_room(&self, room_id: &str) -> Vec<SfuNodeInfo> {
        self.known_nodes
            .get(room_id)
            .map(|nodes| nodes.values().cloned().collect())
            .unwrap_or_default()
    }

    /// Mutable access to the known-nodes map.  Used by SfuService's
    /// admin-only `enable_cascade` to seed a static peer set without
    /// going through the announce/gossip path.
    pub fn known_nodes_mut(&mut self) -> &mut HashMap<String, HashMap<String, SfuNodeInfo>> {
        &mut self.known_nodes
    }

    /// Read the configured capacity per node — used as the
    /// `capacity_hint` on announce-driven node info updates.
    pub fn max_participants_per_node(&self) -> u32 {
        self.max_participants_per_node
    }

    /// Pick the least-loaded SFU node for a new participant.
    /// Returns None if this local node should accept the participant.
    ///
    /// Looks up nodes under the specific `room_id` first, falls back to
    /// the catch-all empty-string entry (populated by
    /// `SfuService::enable_cascade` for statically-configured clusters
    /// that haven't gone through the gossip announce path yet).
    ///
    /// Rule: when the local node has capacity, accept.  When at
    /// capacity, redirect to the least-loaded peer that still has
    /// headroom.  No proactive rebalancing — that requires fresher
    /// cross-node count visibility than the static `enable_cascade`
    /// path provides, and the wind tunnel cycle tests caught a
    /// pingpong between under-loaded peers when the threshold was
    /// active.
    pub fn pick_redirect_node(&self, room_id: &str, local_count: u32) -> Option<&SfuNodeInfo> {
        if local_count < self.max_participants_per_node {
            return None;
        }
        let nodes = self
            .known_nodes
            .get(room_id)
            .or_else(|| self.known_nodes.get(""))?;
        nodes
            .values()
            .filter(|n| n.participant_count < n.capacity_hint)
            .min_by_key(|n| n.participant_count)
    }

    /// Lookup pipe metadata by `(room_id, remote_did)`.  The pipe's
    /// `Rtc` lives in the server peers map under `meta.participant_id`.
    pub fn pipe_meta(&self, room_id: &str, remote_did: &str) -> Option<&PipeMeta> {
        self.pipes
            .get(&(room_id.to_string(), remote_did.to_string()))
    }

    /// All pipes for a given `room_id` — caller iterates the
    /// `ParticipantId`s to find the corresponding `SfuPeer`s in the
    /// server peers map.
    pub fn pipes_for_room(&self, room_id: &str) -> Vec<&PipeMeta> {
        self.pipes
            .values()
            .filter(|m| m.room_id.to_string() == room_id)
            .collect()
    }

    /// Check if we're in cascaded mode for a room (have known peer nodes).
    pub fn is_cascaded(&self, room_id: &str) -> bool {
        self.known_nodes
            .get(room_id)
            .map(|n| !n.is_empty())
            .unwrap_or(false)
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
    fn test_cascade_announce_and_discovery() {
        let mut node_a = CascadeManager::new("did:key:nodeA".into(), test_addr(10001), 8);
        let mut node_b = CascadeManager::new("did:key:nodeB".into(), test_addr(10002), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // node_a announces
        let signal = node_a.announce_sfu_node(&room_id, 3);
        match &signal {
            CascadeSignal::Announce {
                did,
                room_id: _rid,
                participant_count,
                capacity_hint: _,
            } => {
                assert_eq!(did, "did:key:nodeA");
                assert_eq!(*participant_count, 3);
            }
            _ => panic!("Expected Announce signal"),
        }

        // node_b handles announce
        node_b.handle_sfu_announce("did:key:nodeA".into(), room_id.to_string(), 3, 8);
        let nodes = node_b.nodes_for_room(&room_id.to_string());
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].did, "did:key:nodeA");
        assert_eq!(nodes[0].participant_count, 3);

        // node_a ignores its own announce
        node_a.handle_sfu_announce("did:key:nodeA".into(), room_id.to_string(), 3, 8);
        let nodes = node_a.nodes_for_room(&room_id.to_string());
        assert_eq!(nodes.len(), 0);

        // Verify cascaded detection
        assert!(node_b.is_cascaded(&room_id.to_string()));
        assert!(!node_a.is_cascaded(&room_id.to_string()));
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
    fn test_cascade_remove_node() {
        let mut node_a = CascadeManager::new("did:key:nodeA".into(), test_addr(10005), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // Add node_b as known
        node_a.handle_sfu_announce("did:key:nodeB".into(), room_id.to_string(), 2, 8);
        assert_eq!(node_a.nodes_for_room(&room_id.to_string()).len(), 1);

        // Remove node_b — no pipes registered so the returned Vec is empty.
        let removed = node_a.remove_node("did:key:nodeB");
        assert!(removed.is_empty());
        assert_eq!(node_a.nodes_for_room(&room_id.to_string()).len(), 0);
        assert!(!node_a.is_cascaded(&room_id.to_string()));
    }

    #[test]
    fn test_cascade_pick_redirect() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10007), 4);
        let room_id = RoomId::new("test-nh", "room1");

        // Add remote node with low load
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 1, 8);

        // local_count=5 (over capacity=4) → must redirect
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 5);
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // local_count=2 (under capacity), remote has 1 → difference < 2, no redirect
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2);
        assert!(redirect.is_none());

        // local_count=2, remote has 0 → difference = 2, threshold is strictly less, so None
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 0, 8);
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2);
        assert!(redirect.is_none());

        // local_count=4 (at capacity), remote has 1 → must redirect
        mgr.handle_sfu_announce("did:key:remote".into(), room_id.to_string(), 1, 8);
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 4);
        assert!(redirect.is_some());
    }

    #[test]
    fn test_cascade_pipe_meta_lookup() {
        let mgr = CascadeManager::new("did:key:local".into(), test_addr(10009), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // No pipes registered → lookup returns None and pipes_for_room is empty.
        assert!(mgr
            .pipe_meta(&room_id.to_string(), "did:key:remote")
            .is_none());
        assert!(mgr.pipes_for_room(&room_id.to_string()).is_empty());
    }

    #[test]
    fn test_room_remote_participants() {
        use super::super::room::{ParticipantId, RoomId, SfuRoom};

        let room_id = RoomId::new("test-nh", "room1");
        let mut room = SfuRoom::new(room_id, None);

        // Add local participant
        let p1 = ParticipantId::next();
        room.add_participant(p1.clone(), "did:key:local1".to_string())
            .unwrap();
        assert_eq!(room.participant_count(), 1);
        assert_eq!(room.total_participant_count(), 1);

        // Add remote participants
        room.add_remote_participant("did:key:remote1".to_string(), "did:key:sfuB".to_string());
        room.add_remote_participant("did:key:remote2".to_string(), "did:key:sfuB".to_string());
        assert_eq!(room.participant_count(), 1); // local only
        assert_eq!(room.total_participant_count(), 3); // local + remote

        // Active speaker on local participant
        room.set_active_speaker(&p1, true);

        // Remove remote participant
        assert!(room.remove_remote_participant("did:key:remote1"));
        assert_eq!(room.total_participant_count(), 2);

        // Remove all from SFU node
        room.remove_remote_participants_from_node("did:key:sfuB");
        assert_eq!(room.total_participant_count(), 1);
    }
}
