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
use std::time::{Duration, Instant};

use log::{debug, info, warn};
use str0m::change::SdpOffer;
use str0m::{Candidate, Rtc};

use super::hosts::MediaSockets;
use super::room::{ParticipantId, RoomId};
use super::server::SfuPeer;
use super::types::{SfuRosterParticipant, TrackMapEntry};

/// How this node names session `participant_id` of the SFU node `node_did`.
/// Every node numbers its own sessions from `P1`, so a remote session's
/// own id could name a local one; qualified with its node's DID, it cannot.
/// An id already qualified (a node relaying another node's track) is kept.
pub fn remote_session_id(participant_id: &str, node_did: &str) -> String {
    match ParticipantId::parse(participant_id) {
        Some(pid) => format!("{}@{}", pid, node_did),
        None => participant_id.to_string(),
    }
}

/// Most sessions one node's announce lists for a room: the most any room
/// holds on one node (four times the largest `maxMeshParticipants`). A
/// longer list is cut to this length.
pub const MAX_ANNOUNCED_SESSIONS: usize = super::service::MAX_MESH_PARTICIPANTS as usize * 4;

/// `"<type> from <sender>"` for a gossip frame that did not decode, read
/// from whatever of it parses as JSON, for the log line that drops it.
pub fn describe_frame(frame: &str) -> String {
    let value: serde_json::Value = serde_json::from_str(frame).unwrap_or_default();
    let field = |names: &[&str]| {
        names
            .iter()
            .find_map(|name| value.get(*name).and_then(|v| v.as_str()))
            .map(str::to_string)
    };
    format!(
        "{} from {}",
        field(&["type"]).unwrap_or_else(|| "unknown type".into()),
        field(&["did", "from_did"]).unwrap_or_else(|| "unknown sender".into())
    )
}

/// Represents a remote SFU node in the cascade cluster.
#[derive(Debug, Clone)]
pub struct SfuNodeInfo {
    pub did: String,
    pub participant_count: u32,
    pub capacity_hint: u32,
    /// The node's sessions in the room, by the ids this node gives them
    /// ([`remote_session_id`]).
    pub sessions: Vec<SfuRosterParticipant>,
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
        /// The announcing node's sessions in the room, by its own ids:
        /// the other nodes list them in their room rosters. Absent from a
        /// node on an older build: it announces none.
        #[serde(default)]
        sessions: Vec<SfuRosterParticipant>,
    },
    /// SDP offer to establish a pipe transport between two SFU nodes, or to
    /// add tracks to one.
    #[serde(rename = "sfu-pipe-offer")]
    PipeOffer {
        from_did: String,
        to_did: String,
        room_id: String,
        sdp_offer: String,
        /// The session each new m-line carries, as the sending node names
        /// it: the receiving node attributes the track to that session, not
        /// to the pipe. Absent from a node on an older build: its tracks
        /// are attributed to the pipe.
        #[serde(default)]
        track_mapping: Vec<TrackMapEntry>,
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
    /// The DID the frame says sent it. A transport checks it against the
    /// peer the frame actually came from.
    pub fn sender_did(&self) -> &str {
        match self {
            CascadeSignal::Announce { did, .. } | CascadeSignal::Leave { did, .. } => did,
            CascadeSignal::PipeOffer { from_did, .. }
            | CascadeSignal::PipeAnswer { from_did, .. }
            | CascadeSignal::QualityPreference { from_did, .. } => from_did,
        }
    }

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

/// How long the cascade waits before redialling a pipe that has ended
/// twice in a row: a locally rejected answer, an offer still unanswered
/// after the second resend, or an ICE drop on the end that dials. The
/// first of those is immediate, as a single drop was before the flap
/// guard. Announce arrives every 10 s, so without a wait on the repeats
/// the next announce would rebuild the pipe while the other end still
/// held its entry. Each further consecutive failure doubles this, up to
/// [`PIPE_REDIAL_BACKOFF_CAP`]. A timeout (the pipe never connected) does
/// not wait, and a pipe whose connected stretch reached
/// [`PIPE_HEALTHY_RESET`] starts the count again. The stretch is measured
/// until ICE reports `Disconnected`. Time spent disconnected does not count.
pub const PIPE_REDIAL_BACKOFF: Duration = Duration::from_secs(30);

/// The longest redial wait. Further consecutive failures of the same pipe stay here.
pub const PIPE_REDIAL_BACKOFF_CAP: Duration = Duration::from_secs(120);

/// How long a pipe must stay connected before its failure count is cleared.
/// Measured until ICE reports `Disconnected`, or until the pipe is dropped
/// while it is still connected. Time spent disconnected does not count, and
/// two shorter stretches are not added. The next end is then a first
/// failure, and the redial is immediate.
pub const PIPE_HEALTHY_RESET: Duration = Duration::from_secs(120);

/// One pipe's run of consecutive ended offers and dialler drops, and when
/// it may be dialled. `reset` means the next recorded end counts as a first
/// failure, because a connected stretch reached [`PIPE_HEALTHY_RESET`].
struct PipeRedial {
    failures: u32,
    not_before: Instant,
    reset: bool,
}

/// The wait after `failures` consecutive ends. The first is immediate.
/// The second is [`PIPE_REDIAL_BACKOFF`], then double, capped at
/// [`PIPE_REDIAL_BACKOFF_CAP`].
fn pipe_redial_wait(failures: u32) -> Duration {
    if failures <= 1 {
        return Duration::ZERO;
    }
    let mut wait = PIPE_REDIAL_BACKOFF;
    for _ in 2..failures {
        wait = wait.saturating_mul(2).min(PIPE_REDIAL_BACKOFF_CAP);
    }
    wait
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
    /// The SFU's sockets: every pipe offers their host addresses
    /// ([`MediaSockets::host_addrs`]) as candidates.
    hosts: MediaSockets,
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
    /// Consecutive failures of a pipe the dialling end must remember, keyed
    /// by `(room_id, remote_did)`. An ended offer records one, and so does
    /// an ICE drop on the end that dials. The first failure still dials
    /// now. Absent when that pair has not failed since it was last healthy.
    /// See [`PIPE_REDIAL_BACKOFF`].
    redials: HashMap<(String, String), PipeRedial>,
    /// Tests set the clock [`Self::establish_pipe`] compares to the redial
    /// wait. Production leaves it unset.
    #[cfg(test)]
    test_now: Option<Instant>,
}

impl CascadeManager {
    pub fn new(
        local_did: String,
        hosts: impl Into<MediaSockets>,
        max_participants_per_node: u32,
    ) -> Self {
        Self {
            local_did,
            hosts: hosts.into(),
            known_nodes: HashMap::new(),
            pipes: HashMap::new(),
            max_participants_per_node,
            rebalance_cooldowns: HashMap::new(),
            redials: HashMap::new(),
            #[cfg(test)]
            test_now: None,
        }
    }

    /// The clock a redial wait is compared to.
    fn dial_instant(&self) -> Instant {
        #[cfg(test)]
        if let Some(now) = self.test_now {
            return now;
        }
        Instant::now()
    }

    /// Pin the redial clock. Production never calls this.
    #[cfg(test)]
    pub(crate) fn set_now(&mut self, now: Instant) {
        self.test_now = Some(now);
    }

    /// The connected stretch to `remote_did` reached [`PIPE_HEALTHY_RESET`].
    /// The next end of that pipe counts as a first failure. No entry yet
    /// means the next end is already a first failure.
    fn note_healthy_stretch(&mut self, room_id: &str, remote_did: &str) {
        let key = (room_id.to_string(), remote_did.to_string());
        if let Some(redial) = self.redials.get_mut(&key) {
            redial.reset = true;
        }
    }

    /// An established pipe to `remote_did` ended: a locally rejected answer,
    /// an unanswered offer, or an ICE drop on the end that dials. The first
    /// consecutive end dials at once. From the second, [`Self::establish_pipe`]
    /// waits. A stretch that reached [`PIPE_HEALTHY_RESET`] counts this end
    /// as a first failure again. The peer decides that, at the disconnect;
    /// this does not compare clocks.
    fn note_pipe_offer_ended(&mut self, room_id: &str, remote_did: &str, now: Instant) {
        let key = (room_id.to_string(), remote_did.to_string());
        let previous = self.redials.get(&key);
        let reset = previous.is_some_and(|redial| redial.reset);
        let failures = if reset {
            1
        } else {
            previous
                .map(|redial| redial.failures)
                .unwrap_or(0)
                .saturating_add(1)
        };
        let wait = pipe_redial_wait(failures);
        debug!(
            "SFU cascade: pipe to {remote_did} for room {room_id} ended ({failures}); redial waits {} s",
            wait.as_secs()
        );
        self.redials.insert(
            key,
            PipeRedial {
                failures,
                not_before: now + wait,
                reset: false,
            },
        );
    }

    /// A pipe peer died, and `healthy_stretch` says whether a connected
    /// stretch reached [`PIPE_HEALTHY_RESET`] before this death. When it
    /// did, the failure count resets and this end is a first failure.
    /// Dead-pipe cleanup calls this. The comparison is the peer's: a
    /// disconnected period is not measured here.
    pub fn apply_pipe_death(
        &mut self,
        room_id: &str,
        remote_did: &str,
        reason: &str,
        now: Instant,
        healthy_stretch: bool,
    ) -> Option<ParticipantId> {
        if healthy_stretch {
            self.note_healthy_stretch(room_id, remote_did);
        }
        self.on_pipe_died(room_id, remote_did, reason, now)
    }

    /// A pipe peer died. An ended offer (`reason` `"offer"`) arms the redial
    /// wait. An ICE drop does too, but only on the end that dials (the
    /// lexically higher DID): the other end's ended offer arrives here as an
    /// ordinary drop, and that is the end that would rebuild the pipe. A
    /// timeout only drops the entry, so the next announce dials at once. A
    /// leave does not come through here.
    pub fn on_pipe_died(
        &mut self,
        room_id: &str,
        remote_did: &str,
        reason: &str,
        now: Instant,
    ) -> Option<ParticipantId> {
        let dials = self.local_did.as_str() > remote_did;
        if reason == "offer" || (reason == "drop" && dials) {
            self.note_pipe_offer_ended(room_id, remote_did, now);
        }
        self.remove_node_from_room(room_id, remote_did)
    }

    /// Add a host candidate for each address the SFU offers now.
    fn add_host_candidates(&self, rtc: &mut Rtc) -> Result<(), String> {
        let hosts = self.hosts.host_addrs();
        if hosts.is_empty() {
            return Err("The SFU has no address to offer".to_string());
        }
        for host in hosts {
            let candidate = Candidate::host(host, "udp")
                .map_err(|e| format!("Failed to create candidate: {}", e))?;
            rtc.add_local_candidate(candidate);
        }
        Ok(())
    }

    /// Maximum participants this node accepts per room.
    pub fn max_participants_per_node(&self) -> u32 {
        self.max_participants_per_node
    }

    /// Handle an incoming SFU announce from a peer node. Returns whether
    /// the node's sessions in the room changed, so the room's roster is due.
    pub fn handle_sfu_announce(
        &mut self,
        did: String,
        room_id: String,
        participant_count: u32,
        capacity_hint: u32,
        sessions: Vec<SfuRosterParticipant>,
    ) -> bool {
        if did == self.local_did {
            return false; // Ignore our own announces
        }

        // A node announces its own sessions only: an entry that is no
        // session, or names another node, is dropped.
        let own_suffix = format!("@{}", did);
        let listed = sessions.len();
        let mut sessions: Vec<SfuRosterParticipant> = sessions
            .into_iter()
            .filter_map(|s| {
                let participant_id = remote_session_id(&s.participant_id, &did);
                let own = participant_id
                    .strip_suffix(&own_suffix)
                    .is_some_and(|pid| ParticipantId::parse(pid).is_some());
                own.then_some(SfuRosterParticipant {
                    participant_id,
                    agent_did: s.agent_did,
                })
            })
            .collect();
        sessions.sort();
        sessions.dedup();
        sessions.truncate(MAX_ANNOUNCED_SESSIONS);
        if sessions.len() != listed {
            warn!(
                "SFU cascade: {} announced {} sessions in {}; kept {} of its own (at most {})",
                did,
                listed,
                room_id,
                sessions.len(),
                MAX_ANNOUNCED_SESSIONS
            );
        }
        let nodes = self.known_nodes.entry(room_id).or_default();
        let changed = nodes
            .get(&did)
            .map_or(!sessions.is_empty(), |n| n.sessions != sessions);
        nodes.insert(
            did.clone(),
            SfuNodeInfo {
                did,
                participant_count,
                capacity_hint,
                sessions,
                last_seen: Instant::now(),
            },
        );
        changed
    }

    /// The sessions in `room_id` on the other nodes that announced it.
    pub fn remote_sessions(&self, room_id: &str) -> Vec<SfuRosterParticipant> {
        self.known_nodes
            .get(room_id)
            .map(|nodes| {
                nodes
                    .values()
                    .flat_map(|n| n.sessions.iter().cloned())
                    .collect()
            })
            .unwrap_or_default()
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
        if let Some(redial) = self.redials.get(&pipe_key) {
            let now = self.dial_instant();
            if now < redial.not_before {
                return Err(format!(
                    "pipe redial waits {} s",
                    redial.not_before.saturating_duration_since(now).as_secs()
                ));
            }
            warn!(
                "SFU cascade: redialling pipe to {remote_did} for room {room_key} after {} failures",
                redial.failures
            );
        }

        let mut rtc = Rtc::builder().build(Instant::now());
        self.add_host_candidates(&mut rtc)?;

        // Create offer via SDP API.  str0m requires at least one
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
        peer.note_initial_offer(pending, sdp_offer.clone(), Instant::now());

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
                // The pipe opens with its control channel only: tracks come
                // in later offers, with their sessions.
                track_mapping: Vec::new(),
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

        let mut rtc = Rtc::builder().build(Instant::now());
        self.add_host_candidates(&mut rtc)?;

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
    /// `SfuCommand::ApplyServerAnswer { participant_id, offer_id: None, sdp_answer_json, reply: None }`
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
    pub fn suggest_rebalance(&mut self, room_id: &str, local_count: u32) -> Option<String> {
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

    fn session(agent_did: &str, participant_id: &str) -> SfuRosterParticipant {
        SfuRosterParticipant {
            agent_did: agent_did.into(),
            participant_id: participant_id.into(),
        }
    }

    /// Every node numbers its sessions from P1, so the sessions another
    /// node announces are listed under ids qualified by that node, and an
    /// announce reports when they changed so the roster is republished.
    #[test]
    fn announced_sessions_are_listed_under_their_node() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10030), 8);
        let room = "test-nh:room1".to_string();
        let announce = |mgr: &mut CascadeManager, did: &str, sessions| {
            mgr.handle_sfu_announce(did.into(), room.clone(), 1, 8, sessions)
        };

        assert!(!announce(&mut mgr, "did:key:nodeA", Vec::new()));
        assert!(announce(
            &mut mgr,
            "did:key:nodeA",
            vec![session("did:key:alice", "P1")]
        ));
        assert!(!announce(
            &mut mgr,
            "did:key:nodeA",
            vec![session("did:key:alice", "P1")]
        ));
        assert!(announce(
            &mut mgr,
            "did:key:nodeB",
            vec![session("did:key:bob", "P1")]
        ));
        let mut listed = mgr.remote_sessions(&room);
        listed.sort();
        assert_eq!(
            listed,
            vec![
                session("did:key:alice", "P1@did:key:nodeA"),
                session("did:key:bob", "P1@did:key:nodeB"),
            ]
        );

        // Our own announce lists nothing; a node that leaves takes its
        // sessions with it.
        assert!(!announce(
            &mut mgr,
            "did:key:local",
            vec![session("did:key:me", "P1")]
        ));
        mgr.remove_node_from_room(&room, "did:key:nodeA");
        assert_eq!(
            mgr.remote_sessions(&room),
            vec![session("did:key:bob", "P1@did:key:nodeB")]
        );
        assert!(mgr.remote_sessions("test-nh:other").is_empty());
    }

    #[test]
    fn a_qualified_session_id_is_kept() {
        assert_eq!(remote_session_id("P3", "did:key:n"), "P3@did:key:n");
        assert_eq!(
            remote_session_id("P3@did:key:m", "did:key:n"),
            "P3@did:key:m"
        );
    }

    /// A node on the build before sessions and track mappings sends
    /// announces without `sessions` and pipe offers without
    /// `track_mapping`. They decode as before: no sessions, no mapping.
    #[test]
    fn frames_from_a_node_without_sessions_or_track_mappings_decode() {
        let announce: CascadeSignal = serde_json::from_str(
            r#"{"type":"sfu-announce","did":"did:key:old","room_id":"n:r","participant_count":2,"capacity_hint":8}"#,
        )
        .expect("an announce without sessions decodes");
        assert!(
            matches!(&announce, CascadeSignal::Announce { did, sessions, .. } if did == "did:key:old" && sessions.is_empty()),
            "{announce:?}"
        );
        let offer: CascadeSignal = serde_json::from_str(
            r#"{"type":"sfu-pipe-offer","from_did":"did:key:old","to_did":"did:key:new","room_id":"n:r","sdp_offer":"{}"}"#,
        )
        .expect("a pipe offer without a track mapping decodes");
        assert!(
            matches!(&offer, CascadeSignal::PipeOffer { track_mapping, .. } if track_mapping.is_empty()),
            "{offer:?}"
        );
    }

    /// A node announces only its own sessions: an entry that names another
    /// node, or no session at all, is dropped, and a list longer than a room
    /// holds is cut to that length.
    #[test]
    fn an_announce_lists_only_the_senders_sessions_up_to_a_room() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10031), 8);
        let room = "test-nh:room1".to_string();
        mgr.handle_sfu_announce(
            "did:key:nodeA".into(),
            room.clone(),
            4,
            8,
            vec![
                session("did:key:alice", "P1"),
                session("did:key:bob", "P2@did:key:nodeA"),
                session("did:key:mallory", "P3@did:key:nodeB"),
                session("did:key:mallory", "P4@did:key:local"),
                session("did:key:mallory", "not-a-session"),
                session("did:key:mallory", "x@did:key:nodeA"),
            ],
        );
        assert_eq!(
            mgr.remote_sessions(&room),
            vec![
                session("did:key:alice", "P1@did:key:nodeA"),
                session("did:key:bob", "P2@did:key:nodeA"),
            ]
        );

        let many = (1..=MAX_ANNOUNCED_SESSIONS + 10)
            .map(|i| session("did:key:alice", &format!("P{}", i)))
            .collect();
        mgr.handle_sfu_announce("did:key:nodeA".into(), room.clone(), 1, 8, many);
        assert_eq!(mgr.remote_sessions(&room).len(), MAX_ANNOUNCED_SESSIONS);
    }

    /// A frame that does not decode is logged by its type and sender, read
    /// from whatever of it parses.
    #[test]
    fn an_undecodable_frame_is_described_by_type_and_sender() {
        assert_eq!(
            describe_frame(r#"{"type":"sfu-announce","did":"did:key:a","room_id":5}"#),
            "sfu-announce from did:key:a"
        );
        assert_eq!(
            describe_frame(r#"{"type":"sfu-pipe-offer","from_did":"did:key:b"}"#),
            "sfu-pipe-offer from did:key:b"
        );
        assert_eq!(
            describe_frame("not json"),
            "unknown type from unknown sender"
        );
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
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            1,
            8,
            Vec::new(),
        );

        // local_count=5 (over capacity=4) → must redirect
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 5, None);
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // local_count=2 (under capacity) → accept locally
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2, None);
        assert!(redirect.is_none());

        // local_count=2, remote has 0 → still under local cap, accept
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            0,
            8,
            Vec::new(),
        );
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 2, None);
        assert!(redirect.is_none());

        // local_count=4 (at capacity), remote has 1 → must redirect
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            1,
            8,
            Vec::new(),
        );
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 4, None);
        assert!(redirect.is_some());

        // local_count=4, ALL remote nodes also full → still redirect
        // (client-side cycle detection handles the overflow).
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            8,
            8,
            Vec::new(),
        );
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
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            0,
            8,
            Vec::new(),
        );
        let redirect = mgr.pick_redirect_node(
            &room_id.to_string(),
            1,                      // local well under capacity
            Some("did:key:remote"), // but remote is preferred
        );
        assert!(redirect.is_some());
        assert_eq!(redirect.unwrap().did, "did:key:remote");

        // When preferred node is at capacity, fall through to normal logic
        mgr.handle_sfu_announce(
            "did:key:remote".into(),
            room_id.to_string(),
            8,
            8,
            Vec::new(),
        );
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 1, Some("did:key:remote"));
        assert!(redirect.is_none()); // local has capacity, preferred full

        // When preferred DID matches local, no redirect (we ARE the preferred)
        let redirect = mgr.pick_redirect_node(&room_id.to_string(), 1, Some("did:key:local"));
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
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10, Vec::new());
        assert!(mgr.suggest_rebalance(room, 8).is_none());
    }

    #[test]
    fn test_rebalance_above_threshold_with_target() {
        // capacity=10, threshold=9. local=9, peer has 2/10 (80% free).
        // gap=9-2=7 >= 4. Peer has 80% free >= 30%.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10011), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10, Vec::new());
        let target = mgr.suggest_rebalance(room, 9);
        assert_eq!(target.as_deref(), Some("did:key:peer"));
    }

    #[test]
    fn test_rebalance_cooldown() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10012), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 2, 10, Vec::new());

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
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 7, 10, Vec::new());
        assert!(mgr.suggest_rebalance(room, 9).is_none());
    }

    #[test]
    fn test_rebalance_peer_too_full() {
        // Peer has 8/10 (only 20% free < 30% threshold) → no migration.
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10014), 10);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peer".into(), room.into(), 8, 10, Vec::new());
        assert!(mgr.suggest_rebalance(room, 10).is_none());
    }

    #[test]
    fn test_rebalance_picks_emptiest_peer() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10015), 20);
        let room = "nh://test:room1";
        mgr.handle_sfu_announce("did:key:peerA".into(), room.into(), 5, 20, Vec::new());
        mgr.handle_sfu_announce("did:key:peerB".into(), room.into(), 2, 20, Vec::new());
        // Both qualify (>= 30% free, gap >= 4). peerB has most headroom.
        let target = mgr.suggest_rebalance(room, 18);
        assert_eq!(target.as_deref(), Some("did:key:peerB"));
    }

    #[test]
    fn test_rebalance_separate_room_cooldown() {
        let mut mgr = CascadeManager::new("did:key:local".into(), test_addr(10016), 10);
        let room_a = "nh://test:roomA";
        let room_b = "nh://test:roomB";
        mgr.handle_sfu_announce("did:key:peer".into(), room_a.into(), 2, 10, Vec::new());
        mgr.handle_sfu_announce("did:key:peer".into(), room_b.into(), 2, 10, Vec::new());

        // Rebalance room A and record cooldown
        assert!(mgr.suggest_rebalance(room_a, 9).is_some());
        mgr.record_rebalance(room_a);

        // Room B should still allow rebalancing (independent cooldown)
        assert!(mgr.suggest_rebalance(room_b, 9).is_some());
    }

    /// A pipe ended for an unanswered offer, or an answer rejected locally,
    /// dials again at once the first time. The next consecutive end waits,
    /// the wait doubles, and it stops at the cap. A timeout still dials at
    /// once. An ICE drop on the dialler uses the same count; see
    /// [`the_dialler_waits_out_the_backoff_when_the_other_end_ends_the_pipe`].
    #[test]
    fn a_redialled_pipe_that_fails_again_waits_out_its_backoff() {
        let room = RoomId::new("nh", "r");
        let remote = "did:key:nodeA";
        let key = room.to_string();
        let t0 = Instant::now();

        let mut timed_out = CascadeManager::new("did:key:nodeB".into(), test_addr(10020), 8);
        timed_out.set_now(t0);
        timed_out.establish_pipe(remote, &room).unwrap();
        timed_out.on_pipe_died(&key, remote, "timeout", t0);
        timed_out
            .establish_pipe(remote, &room)
            .expect("a timeout still dials on the next announce");

        let mut mgr = CascadeManager::new("did:key:nodeB".into(), test_addr(10021), 8);
        mgr.set_now(t0);
        let (first, _) = mgr.establish_pipe(remote, &room).unwrap();
        let first_id = first.id.clone();
        mgr.on_pipe_died(&key, remote, "offer", t0);
        assert!(
            !mgr.has_pipe(&key, remote),
            "the ended pipe's entry is gone"
        );
        let (second, _) = mgr
            .establish_pipe(remote, &room)
            .expect("the first drop redials at once");
        assert_ne!(second.id, first_id);

        let t1 = t0;
        mgr.on_pipe_died(&key, remote, "offer", t1);
        mgr.set_now(t1 + PIPE_REDIAL_BACKOFF - Duration::from_millis(1));
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "a redialled pipe that fails again is not redialled on the same wait"
        );
        mgr.set_now(t1 + PIPE_REDIAL_BACKOFF);
        mgr.establish_pipe(remote, &room)
            .expect("the second failure waits one backoff");

        let t2 = t1 + PIPE_REDIAL_BACKOFF;
        mgr.on_pipe_died(&key, remote, "offer", t2);
        mgr.set_now(t2 + PIPE_REDIAL_BACKOFF);
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "the third failure is not on the same wait"
        );
        mgr.set_now(t2 + PIPE_REDIAL_BACKOFF * 2);
        mgr.establish_pipe(remote, &room)
            .expect("the third failure waits twice as long");

        let t3 = t2 + PIPE_REDIAL_BACKOFF * 2;
        mgr.on_pipe_died(&key, remote, "offer", t3);
        mgr.set_now(t3 + PIPE_REDIAL_BACKOFF_CAP - Duration::from_millis(1));
        assert!(mgr.establish_pipe(remote, &room).is_err());
        mgr.set_now(t3 + PIPE_REDIAL_BACKOFF_CAP);
        mgr.establish_pipe(remote, &room)
            .expect("the wait reaches the cap");

        let t4 = t3 + PIPE_REDIAL_BACKOFF_CAP;
        mgr.on_pipe_died(&key, remote, "offer", t4);
        mgr.set_now(t4 + PIPE_REDIAL_BACKOFF_CAP);
        mgr.establish_pipe(remote, &room)
            .expect("a further failure stays at the cap");
    }

    /// The higher DID is the only end that dials. When the lower DID ends
    /// its offer, this end only sees the ICE drop. The first such drop
    /// dials at once. The next consecutive one waits, so a pipe that keeps
    /// failing is not rebuilt on the next announce. A timeout still dials
    /// at once.
    #[test]
    fn the_dialler_waits_out_the_backoff_when_the_other_end_ends_the_pipe() {
        let room = RoomId::new("nh", "r");
        let key = room.to_string();
        let lower = "did:key:nodeA";
        let higher = "did:key:nodeB";
        assert!(higher > lower);
        let t0 = Instant::now();

        let mut timed = CascadeManager::new(higher.into(), test_addr(10032), 8);
        timed.set_now(t0);
        timed.establish_pipe(lower, &room).unwrap();
        timed.on_pipe_died(&key, lower, "timeout", t0);
        timed
            .establish_pipe(lower, &room)
            .expect("a timeout still dials on the next announce");

        let mut dialler = CascadeManager::new(higher.into(), test_addr(10030), 8);
        let mut other = CascadeManager::new(lower.into(), test_addr(10031), 8);
        dialler.set_now(t0);
        other.set_now(t0);
        dialler.establish_pipe(lower, &room).unwrap();
        // The lower DID ended its offer. It never dials.
        other.on_pipe_died(&key, higher, "offer", t0);
        // The dialler sees an ordinary drop. The first one is free.
        dialler.on_pipe_died(&key, lower, "drop", t0);
        assert!(
            !dialler.has_pipe(&key, lower),
            "the dialler's pipe entry is gone"
        );
        dialler
            .establish_pipe(lower, &room)
            .expect("the first dialler drop redials at once");

        dialler.on_pipe_died(&key, lower, "drop", t0);
        dialler.set_now(t0 + PIPE_REDIAL_BACKOFF - Duration::from_millis(1));
        assert!(
            dialler.establish_pipe(lower, &room).is_err(),
            "the dialling end waits out the backoff after the other end ends the pipe again"
        );
        dialler.set_now(t0 + PIPE_REDIAL_BACKOFF);
        dialler
            .establish_pipe(lower, &room)
            .expect("the second dialler drop waits 30 s");
    }

    /// One ordinary end, offer or dialler drop, redials in the same moment.
    #[test]
    fn the_first_drop_redials_at_once() {
        let room = RoomId::new("nh", "r");
        let remote = "did:key:nodeA";
        let key = room.to_string();
        let t0 = Instant::now();

        let mut offered = CascadeManager::new("did:key:nodeB".into(), test_addr(10040), 8);
        offered.set_now(t0);
        offered.establish_pipe(remote, &room).unwrap();
        offered.on_pipe_died(&key, remote, "offer", t0);
        offered
            .establish_pipe(remote, &room)
            .expect("the first drop redials at once");

        let mut dialler = CascadeManager::new("did:key:nodeB".into(), test_addr(10041), 8);
        dialler.set_now(t0);
        dialler.establish_pipe(remote, &room).unwrap();
        dialler.on_pipe_died(&key, remote, "drop", t0);
        dialler
            .establish_pipe(remote, &room)
            .expect("the first dialler drop redials at once");
    }

    /// The second consecutive end waits 30 s, the third 60 s, and the
    /// fourth reaches the 120 s cap.
    #[test]
    fn consecutive_pipe_failures_back_off() {
        let room = RoomId::new("nh", "r");
        let remote = "did:key:nodeA";
        let key = room.to_string();
        let t0 = Instant::now();
        let mut mgr = CascadeManager::new("did:key:nodeB".into(), test_addr(10043), 8);
        mgr.set_now(t0);
        mgr.establish_pipe(remote, &room).unwrap();
        mgr.on_pipe_died(&key, remote, "offer", t0);
        mgr.establish_pipe(remote, &room)
            .expect("the first drop redials at once");

        mgr.on_pipe_died(&key, remote, "offer", t0);
        mgr.set_now(t0 + PIPE_REDIAL_BACKOFF - Duration::from_millis(1));
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "the second consecutive failure waits 30 s"
        );
        mgr.set_now(t0 + PIPE_REDIAL_BACKOFF);
        mgr.establish_pipe(remote, &room)
            .expect("the second consecutive failure waits 30 s");

        let t2 = t0 + PIPE_REDIAL_BACKOFF;
        mgr.on_pipe_died(&key, remote, "drop", t2);
        mgr.set_now(t2 + PIPE_REDIAL_BACKOFF);
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "the third consecutive failure waits 60 s"
        );
        mgr.set_now(t2 + PIPE_REDIAL_BACKOFF * 2);
        mgr.establish_pipe(remote, &room)
            .expect("the third consecutive failure waits 60 s");

        let t3 = t2 + PIPE_REDIAL_BACKOFF * 2;
        mgr.on_pipe_died(&key, remote, "offer", t3);
        mgr.set_now(t3 + PIPE_REDIAL_BACKOFF_CAP - Duration::from_millis(1));
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "the fourth consecutive failure waits 120 s"
        );
        mgr.set_now(t3 + PIPE_REDIAL_BACKOFF_CAP);
        mgr.establish_pipe(remote, &room)
            .expect("the fourth consecutive failure waits 120 s");
    }

    /// Cleanup reporting a healthy stretch drops the failure count. The end
    /// after that is free again. Without that report the count stands, so
    /// the end still waits. The peer decides the bool from the connected
    /// stretch; see the server drop tests.
    #[test]
    fn a_pipe_healthy_for_the_interval_resets_the_redial_count() {
        let room = RoomId::new("nh", "r");
        let remote = "did:key:nodeA";
        let key = room.to_string();
        let t0 = Instant::now();
        let mut mgr = CascadeManager::new("did:key:nodeB".into(), test_addr(10044), 8);
        mgr.set_now(t0);
        mgr.establish_pipe(remote, &room).unwrap();
        mgr.on_pipe_died(&key, remote, "drop", t0);
        mgr.establish_pipe(remote, &room).unwrap();
        mgr.on_pipe_died(&key, remote, "drop", t0);
        mgr.set_now(t0 + PIPE_REDIAL_BACKOFF);
        mgr.establish_pipe(remote, &room)
            .expect("the second failure waits 30 s");

        let t_short = t0 + PIPE_REDIAL_BACKOFF;
        let t_early = t_short + PIPE_HEALTHY_RESET - Duration::from_millis(1);
        mgr.set_now(t_early);
        mgr.apply_pipe_death(&key, remote, "drop", t_early, false);
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "a pipe connected for less than the healthy interval keeps its backoff"
        );

        let t_up = t_early + PIPE_REDIAL_BACKOFF * 2;
        mgr.set_now(t_up);
        mgr.establish_pipe(remote, &room)
            .expect("the third failure waits 60 s");
        let t_drop = t_up + PIPE_HEALTHY_RESET;
        mgr.set_now(t_drop);
        mgr.apply_pipe_death(&key, remote, "drop", t_drop, true);
        mgr.establish_pipe(remote, &room)
            .expect("a pipe that stayed connected for the healthy interval redials at once");

        mgr.on_pipe_died(&key, remote, "drop", t_drop);
        assert!(
            mgr.establish_pipe(remote, &room).is_err(),
            "the count starts again, so the next consecutive failure waits"
        );
        mgr.set_now(t_drop + PIPE_REDIAL_BACKOFF);
        mgr.establish_pipe(remote, &room)
            .expect("backoff starts again at 30 s");
    }
}
