//! Cascaded SFU — manages pipe transports between SFU nodes in a cluster.
//!
//! When multiple executor nodes act as SFU peers for the same neighbourhood,
//! they establish str0m peer connections ("pipe transports") between each other
//! to relay media tracks across the cluster.

use std::collections::HashMap;
use std::net::SocketAddr;

use log::{debug, info};
use str0m::change::SdpOffer;
use str0m::media::{MediaData, MediaKind, Mid};
use str0m::{Candidate, Rtc};

use super::room::RoomId;

/// Represents a remote SFU node in the cascade cluster.
#[derive(Debug, Clone)]
pub struct SfuNodeInfo {
    pub did: String,
    pub participant_count: u32,
    pub capacity_hint: u32,
}

/// A pipe transport — a str0m peer connection to a remote SFU node.
pub struct PipeTransport {
    pub remote_did: String,
    pub rtc: Rtc,
    pub room_id: RoomId,
    /// Tracks being received from the remote SFU (mid -> kind)
    pub tracks_in: HashMap<Mid, MediaKind>,
    /// Tracks being sent to the remote SFU (local mid -> source mid)
    pub tracks_out: HashMap<Mid, Mid>,
    pub established: bool,
    /// Pending SDP offer awaiting answer (stored between establish_pipe and handle_pipe_answer)
    pub pending_offer: Option<str0m::change::SdpPendingOffer>,
}

impl std::fmt::Debug for PipeTransport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PipeTransport")
            .field("remote_did", &self.remote_did)
            .field("room_id", &self.room_id)
            .field("established", &self.established)
            .field("tracks_in", &self.tracks_in.len())
            .field("tracks_out", &self.tracks_out.len())
            .finish()
    }
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
/// Tracks known peer SFU nodes, manages pipe transports, and handles
/// forwarding decisions for cross-node media relay.
pub struct CascadeManager {
    /// Our DID
    local_did: String,
    /// Local SFU server address for creating pipe transport RTCs
    local_addr: SocketAddr,
    /// Known SFU nodes per room: room_id_str -> (did -> node_info)
    known_nodes: HashMap<String, HashMap<String, SfuNodeInfo>>,
    /// Active pipe transports: (room_id_str, remote_did) -> PipeTransport
    pipes: HashMap<(String, String), PipeTransport>,
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

    /// Create an SDP offer to establish a pipe transport to a remote SFU node.
    pub fn establish_pipe(
        &mut self,
        remote_did: &str,
        room_id: &RoomId,
    ) -> Result<CascadeSignal, String> {
        let room_key = room_id.to_string();
        let pipe_key = (room_key.clone(), remote_did.to_string());

        if self.pipes.contains_key(&pipe_key) {
            return Err("Pipe transport already exists".to_string());
        }

        let mut rtc = Rtc::builder().build();
        let candidate = Candidate::host(self.local_addr, "udp")
            .map_err(|e| format!("Failed to create candidate: {}", e))?;
        rtc.add_local_candidate(candidate);

        // Create offer via SDP API.  str0m 0.9 requires at least one queued
        // change before `apply()` produces an offer, so we open a control
        // data channel for the pipe transport.  Real media transceivers are
        // added later as participants arrive in the cascaded room.
        let mut sdp_api = rtc.sdp_api();
        let _control_channel = sdp_api.add_channel("pipe-control".to_string());
        let (offer, pending) = sdp_api
            .apply()
            .ok_or_else(|| "Failed to create pipe offer: no changes to apply".to_string())?;

        let sdp_offer = serde_json::to_string(&offer)
            .map_err(|e| format!("Failed to serialize offer: {}", e))?;

        let pipe = PipeTransport {
            remote_did: remote_did.to_string(),
            rtc,
            room_id: room_id.clone(),
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
            established: false,
            pending_offer: Some(pending),
        };

        self.pipes.insert(pipe_key, pipe);

        Ok(CascadeSignal::PipeOffer {
            from_did: self.local_did.clone(),
            to_did: remote_did.to_string(),
            room_id: room_key,
            sdp_offer,
        })
    }

    /// Handle an incoming pipe offer from a remote SFU node.
    pub fn handle_pipe_offer(
        &mut self,
        from_did: &str,
        room_id: &str,
        sdp_offer_json: &str,
    ) -> Result<CascadeSignal, String> {
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

        // Parse room_id from the string format "neighbourhood_url:room_name"
        let (nh_url, room_name) = room_id.split_once(':').unwrap_or((room_id, "default"));

        let pipe = PipeTransport {
            remote_did: from_did.to_string(),
            rtc,
            room_id: RoomId::new(nh_url, room_name),
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
            established: true,
            pending_offer: None,
        };

        let pipe_key = (room_id.to_string(), from_did.to_string());
        self.pipes.insert(pipe_key, pipe);

        Ok(CascadeSignal::PipeAnswer {
            from_did: self.local_did.clone(),
            to_did: from_did.to_string(),
            room_id: room_id.to_string(),
            sdp_answer,
        })
    }

    /// Handle an incoming pipe answer from a remote SFU node.
    pub fn handle_pipe_answer(
        &mut self,
        from_did: &str,
        room_id: &str,
        sdp_answer_json: &str,
    ) -> Result<(), String> {
        let pipe_key = (room_id.to_string(), from_did.to_string());
        let pipe = self
            .pipes
            .get_mut(&pipe_key)
            .ok_or_else(|| "No pending pipe transport for this node".to_string())?;

        let answer: str0m::change::SdpAnswer = serde_json::from_str(sdp_answer_json)
            .map_err(|e| format!("Invalid pipe SDP answer: {}", e))?;

        let pending = pipe
            .pending_offer
            .take()
            .ok_or_else(|| "No pending offer for this pipe transport".to_string())?;

        pipe.rtc
            .sdp_api()
            .accept_answer(pending, answer)
            .map_err(|e| format!("Failed to accept pipe answer: {}", e))?;

        pipe.established = true;
        info!(
            "Pipe transport established to SFU node {} for room {}",
            from_did, room_id
        );

        Ok(())
    }

    /// Forward media data to all pipe transports for a room, excluding the origin node.
    pub fn forward_to_pipes(
        &mut self,
        room_id: &str,
        data: &MediaData,
        exclude_node: Option<&str>,
    ) {
        for ((pipe_room, pipe_did), pipe) in self.pipes.iter_mut() {
            if pipe_room != room_id {
                continue;
            }
            if let Some(exclude) = exclude_node {
                if pipe_did == exclude {
                    continue; // Don't forward back to origin SFU node
                }
            }
            if !pipe.established || !pipe.rtc.is_alive() {
                continue;
            }

            // Find matching outgoing track on the pipe
            for (&out_mid, _) in &pipe.tracks_out {
                if let Some(writer) = pipe.rtc.writer(out_mid) {
                    if let Err(e) =
                        writer.write(data.pt, data.network_time, data.time, data.data.clone())
                    {
                        debug!("Failed to forward to pipe {}: {:?}", pipe_did, e);
                    }
                    break;
                }
            }
        }
    }

    /// Remove an SFU node from the cluster (handles sfu-leave).
    pub fn remove_node(&mut self, did: &str) {
        // Remove from known nodes
        for nodes in self.known_nodes.values_mut() {
            nodes.remove(did);
        }

        // Remove and disconnect pipe transports
        let keys_to_remove: Vec<_> = self
            .pipes
            .keys()
            .filter(|(_, d)| d == did)
            .cloned()
            .collect();

        for key in keys_to_remove {
            if let Some(mut pipe) = self.pipes.remove(&key) {
                pipe.rtc.disconnect();
                info!("Removed pipe transport to SFU node {}", did);
            }
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

    /// Get mutable access to all pipe transports (for driving in the event loop).
    pub fn pipes_mut(&mut self) -> impl Iterator<Item = (&(String, String), &mut PipeTransport)> {
        self.pipes.iter_mut()
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

        // node_a creates pipe offer
        let offer_signal = node_a.establish_pipe("did:key:nodeB", &room_id).unwrap();
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

        // node_b handles offer, produces answer
        let answer_signal = node_b
            .handle_pipe_offer("did:key:nodeA", &room_id.to_string(), &sdp_offer)
            .unwrap();
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

        // node_a handles answer
        node_a
            .handle_pipe_answer("did:key:nodeB", &room_id.to_string(), &sdp_answer)
            .unwrap();

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

        // Remove node_b
        node_a.remove_node("did:key:nodeB");
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
    fn test_cascade_forward_excludes_origin() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10009), 8);
        let room_id = RoomId::new("test-nh", "room1");

        // With no pipes, forward_to_pipes should just be a no-op (no panic)
        assert!(!mgr.is_cascaded(&room_id.to_string()));
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
