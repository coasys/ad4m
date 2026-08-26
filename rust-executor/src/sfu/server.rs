//! SFU server — manages the shared UDP socket, str0m Rtc instances,
//! and the Sans I/O event loop driven by tokio.

use std::collections::{HashMap, HashSet};
use std::net::SocketAddr;
use std::sync::Mutex as StdMutex;
use std::time::{Duration, Instant};

use log::{debug, error, info, warn};
use str0m::change::SdpOffer;
use str0m::media::{KeyframeRequest, KeyframeRequestKind, MediaData, MediaKind, Mid, Rid};
use str0m::net::Protocol;
use str0m::{net::Receive, Candidate, Event, IceConnectionState, Input, Output, Rtc};
use tokio::net::UdpSocket;
use tokio::sync::mpsc;

use super::relay::MediaRelay;
use super::room::{ParticipantId, RoomId};

/// A connected WebRTC peer managed by the SFU server.
///
/// Two flavours share this struct so the event loop can drive them
/// uniformly:
///
/// - **Client peer** — `is_pipe = false`. `agent_did` is the user's
///   DID; renegotiation offers go out via the events_ws pubsub topic
///   for the client's events subscription to receive.
/// - **Pipe peer** — `is_pipe = true`. Represents the local end of a
///   pipe transport to a remote SFU node. `agent_did` is the remote
///   node's DID; renegotiation offers go out via the cascade gossip
///   transport instead of pubsub so the remote node receives them.
pub struct SfuPeer {
    pub id: ParticipantId,
    pub room_id: RoomId,
    pub agent_did: String,
    pub rtc: Rtc,
    /// Maps incoming Mid (media the peer sends us) to MediaKind
    pub tracks_in: HashMap<Mid, MediaKind>,
    /// Maps outgoing Mid (media we send to the peer) to the source (origin participant, origin mid)
    pub tracks_out: HashMap<Mid, (ParticipantId, Mid)>,
    /// Pending server-initiated SDP renegotiation.  When `Some`, we
    /// already issued an offer to this peer and are waiting for their
    /// `sfu.callAnswerServerOffer` to land before we can renegotiate
    /// again — str0m doesn't allow multiple in-flight changes.
    pub pending_offer: Option<str0m::change::SdpPendingOffer>,
    /// Timestamp when the pending offer was sent.  Used to detect
    /// stale offers that never received an answer.
    pub pending_offer_sent: Option<Instant>,
    /// Reverse index: (origin_pid, origin_mid) -> outbound Mid on this peer.
    /// Provides O(1) lookup during media relay instead of scanning `tracks_out`.
    pub tracks_out_rev: HashMap<(ParticipantId, Mid), Mid>,
    /// Tracks deferred because a pending offer blocked renegotiation
    /// when the additions arrived.  Each entry stores:
    /// (origin_pid, origin_mid, kind, origin_agent_did).
    pub deferred_tracks: Vec<(ParticipantId, Mid, MediaKind, String)>,
    /// True for cross-node pipe transports — see [`SfuPeer`] docs.
    /// Affects how renegotiation offers are routed and how forward
    /// loops treat the participant.
    pub is_pipe: bool,
}

impl SfuPeer {
    pub fn new(
        id: ParticipantId,
        room_id: RoomId,
        agent_did: String,
        rtc: Rtc,
        is_pipe: bool,
    ) -> Self {
        Self {
            id,
            room_id,
            agent_did,
            rtc,
            tracks_in: HashMap::new(),
            tracks_out: HashMap::new(),
            tracks_out_rev: HashMap::new(),
            pending_offer: None,
            pending_offer_sent: None,
            deferred_tracks: Vec::new(),
            is_pipe,
        }
    }
}

impl std::fmt::Debug for SfuPeer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SfuPeer")
            .field("id", &self.id)
            .field("room_id", &self.room_id)
            .field("agent_did", &self.agent_did)
            .field("tracks_in", &self.tracks_in.len())
            .field("tracks_out", &self.tracks_out.len())
            .field("tracks_out_rev", &self.tracks_out_rev.len())
            .field("deferred_tracks", &self.deferred_tracks.len())
            .field("pending_offer", &self.pending_offer.is_some())
            .field("pending_offer_sent", &self.pending_offer_sent)
            .field("is_pipe", &self.is_pipe)
            .finish()
    }
}

/// Determine whether a media packet should pass the simulcast filter.
///
/// When the sender uses simulcast, each `MediaData` event carries a
/// `rid` that identifies the layer ("high", "medium", "low").  The
/// subscriber's quality preference (defaulting to "high") selects
/// which layer to forward.  Non-simulcast sources (`rid = None`)
/// always pass — there's only one layer to forward.
///
/// "auto" maps to "high" in this implementation.  True adaptive
/// quality selection requires TWCC/REMB bandwidth estimation, which
/// is a separate feature.
pub(crate) fn should_forward_rid(rid: Option<&Rid>, preference: Option<&str>) -> bool {
    let Some(rid) = rid else {
        return true; // non-simulcast source — always forward
    };
    let desired: Rid = match preference.unwrap_or("high") {
        "low" => Rid::from("low"),
        "medium" => Rid::from("medium"),
        // "high", "auto", and any unexpected value default to high
        _ => Rid::from("high"),
    };
    *rid == desired
}

/// Commands sent to the SFU event loop from the GraphQL API / signalling layer.
#[derive(Debug)]
pub enum SfuCommand {
    /// A new peer has completed SDP negotiation and should be added to the event loop.
    AddPeer(SfuPeer),
    /// A peer is leaving (explicit leave or disconnect).
    RemovePeer(ParticipantId),
    /// Set quality preference for a participant's received video.
    SetQualityPreference {
        participant_id: ParticipantId,
        /// "high", "medium", "low", or "auto"
        preference: String,
    },
    /// Client answered a server-pushed renegotiation offer.  The event
    /// loop applies the answer through the participant's str0m
    /// `sdp_api()` so the new outbound tracks (added when peer X joined
    /// the room) become live.
    ApplyServerAnswer {
        participant_id: ParticipantId,
        sdp_answer_json: String,
    },
    /// A remote SFU sent a renegotiation offer over the cascade
    /// gossip transport, addressed to our local end of an existing
    /// pipe.  The event loop applies the offer through the pipe's
    /// `sdp_api()`, produces an answer, and republishes it to
    /// `SFU_PIPE_RENEGOTIATION_ANSWER_TOPIC` so the cascade router
    /// can ship it back over gossip.
    ApplyPipeRenegotiationOffer {
        participant_id: ParticipantId,
        sdp_offer_json: String,
        /// DID of the remote SFU that sent the offer.  Echoed back so
        /// the answer can be addressed correctly.
        remote_did: String,
        /// String form of the [`crate::sfu::room::RoomId`].
        room_id: String,
    },
    /// Query the quality preferences map.  Returns a snapshot via
    /// oneshot so callers outside the event loop can read it.
    GetQualityPreferences {
        reply: tokio::sync::oneshot::Sender<HashMap<ParticipantId, String>>,
    },
    /// Shut down the SFU server.
    Shutdown,
}

/// Configuration for the SFU server.
#[derive(Debug, Clone)]
pub struct SfuServerConfig {
    /// Address to bind the UDP socket to. Use 0.0.0.0:0 for auto-assignment.
    pub bind_addr: SocketAddr,
    /// STUN server URLs for ICE candidates.
    pub stun_servers: Vec<String>,
    /// TURN server URLs for ICE relay candidates.
    pub turn_servers: Vec<TurnServer>,
}

#[derive(Debug, Clone)]
pub struct TurnServer {
    pub url: String,
    pub username: String,
    pub credential: String,
}

impl Default for SfuServerConfig {
    fn default() -> Self {
        Self {
            // Bind to loopback so str0m accepts the bound socket address
            // as a `Candidate::host` (str0m rejects 0.0.0.0 with "invalid
            // ip 0.0.0.0" — it can't be used as a candidate IP).
            // Production deployments should override this with the
            // executor's public/LAN IP; the wind tunnel + local-dev case
            // wants 127.0.0.1.
            bind_addr: "127.0.0.1:0".parse().unwrap(),
            stun_servers: vec!["stun:stun.l.google.com:19302".to_string()],
            turn_servers: vec![],
        }
    }
}

/// Notification emitted by the event loop when a pipe peer's RTC dies.
/// The service uses this to clean up the corresponding cascade entry.
#[derive(Debug)]
pub struct DeadPipe {
    pub room_id: String,
    pub remote_did: String,
    pub participant_id: ParticipantId,
}

/// The SFU server. Owns the UDP socket and drives the str0m event loop.
pub struct SfuServer {
    /// The bound local address of the UDP socket.
    pub local_addr: SocketAddr,
    /// Channel to send commands to the event loop.
    pub command_tx: mpsc::Sender<SfuCommand>,
    /// Notifications about pipe peers whose RTC connection died.
    /// The cascade cleanup task reads from the corresponding receiver.
    pub dead_pipe_rx: StdMutex<Option<mpsc::Receiver<DeadPipe>>>,
}

impl SfuServer {
    /// Start the SFU server. Binds a UDP socket and spawns the event loop on tokio.
    pub async fn start(config: SfuServerConfig) -> Result<Self, std::io::Error> {
        let socket = UdpSocket::bind(config.bind_addr).await?;
        let local_addr = socket.local_addr()?;
        info!("SFU server bound to UDP {}", local_addr);

        let (command_tx, command_rx) = mpsc::channel(256);
        let (dead_pipe_tx, dead_pipe_rx) = mpsc::channel(64);

        tokio::spawn(Self::event_loop(
            socket,
            command_rx,
            local_addr,
            dead_pipe_tx,
        ));

        Ok(Self {
            local_addr,
            command_tx,
            dead_pipe_rx: StdMutex::new(Some(dead_pipe_rx)),
        })
    }

    /// Create an Rtc instance for a new peer, process the SDP offer, and return the answer.
    /// The Rtc instance is NOT yet added to the event loop — call `add_peer` after.
    pub fn create_rtc_for_offer(
        offer: SdpOffer,
        local_addr: SocketAddr,
    ) -> Result<(Rtc, String), String> {
        let mut rtc = Rtc::builder().build();

        let candidate = Candidate::host(local_addr, "udp")
            .map_err(|e| format!("Failed to create host candidate: {}", e))?;
        rtc.add_local_candidate(candidate);

        let answer = rtc
            .sdp_api()
            .accept_offer(offer)
            .map_err(|e| format!("Failed to accept SDP offer: {}", e))?;

        let answer_json = serde_json::to_string(&answer)
            .map_err(|e| format!("Failed to serialize SDP answer: {}", e))?;

        Ok((rtc, answer_json))
    }

    /// The main event loop. Reads UDP packets, drives str0m, and relays media.
    async fn event_loop(
        socket: UdpSocket,
        mut command_rx: mpsc::Receiver<SfuCommand>,
        local_addr: SocketAddr,
        dead_pipe_tx: mpsc::Sender<DeadPipe>,
    ) {
        let mut peers: HashMap<ParticipantId, SfuPeer> = HashMap::new();
        let mut relay = MediaRelay::new();
        let mut quality_preferences: HashMap<ParticipantId, String> = HashMap::new();
        let mut buf = vec![0u8; 2000];

        info!("SFU event loop started on {}", local_addr);

        loop {
            // Process commands
            loop {
                match command_rx.try_recv() {
                    Ok(SfuCommand::AddPeer(peer)) => {
                        let pid = peer.id.clone();
                        let room_id = peer.room_id.clone();
                        info!(
                            "SFU: peer {} (DID: {}) joined room {}",
                            pid, peer.agent_did, room_id
                        );

                        // Register existing tracks from other peers as outgoing tracks for the new peer
                        // This will be handled during negotiation

                        peers.insert(pid, peer);
                    }
                    Ok(SfuCommand::RemovePeer(pid)) => {
                        if let Some(peer) = peers.remove(&pid) {
                            let room_id = peer.room_id.clone();
                            info!("SFU: peer {} left room {}", pid, room_id);
                            relay.remove_participant(&pid);
                            quality_preferences.remove(&pid);
                            clean_stale_track_refs(&mut peers, &pid, &room_id);
                        }
                    }
                    Ok(SfuCommand::SetQualityPreference {
                        participant_id,
                        preference,
                    }) => {
                        info!(
                            "SFU: peer {} quality preference set to '{}'",
                            participant_id, preference
                        );
                        quality_preferences.insert(participant_id.clone(), preference);

                        // Request a keyframe from every video origin
                        // that feeds this subscriber.  The new layer's
                        // decoder needs a keyframe to start rendering
                        // after a layer switch.
                        if let Some(sub_peer) = peers.get(&participant_id) {
                            let room_id = sub_peer.room_id.clone();
                            let video_origins: Vec<(ParticipantId, Mid)> = sub_peer
                                .tracks_out
                                .values()
                                .filter_map(|(origin_pid, origin_mid)| {
                                    peers.get(origin_pid).and_then(|op| {
                                        op.tracks_in.get(origin_mid).and_then(|kind| {
                                            if kind.is_video() {
                                                Some((origin_pid.clone(), *origin_mid))
                                            } else {
                                                None
                                            }
                                        })
                                    })
                                })
                                .collect();
                            for (origin_pid, origin_mid) in video_origins {
                                if let Some(origin_peer) = peers.get_mut(&origin_pid) {
                                    if origin_peer.room_id == room_id {
                                        if let Some(mut writer) = origin_peer.rtc.writer(origin_mid)
                                        {
                                            let _ = writer
                                                .request_keyframe(None, KeyframeRequestKind::Pli);
                                            debug!(
                                                "SFU: requested keyframe from {} mid {} for layer switch on {}",
                                                origin_pid, origin_mid, participant_id
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Ok(SfuCommand::ApplyServerAnswer {
                        participant_id,
                        sdp_answer_json,
                    }) => {
                        let Some(peer) = peers.get_mut(&participant_id) else {
                            debug!(
                                "SFU: ApplyServerAnswer for unknown participant {}",
                                participant_id
                            );
                            continue;
                        };
                        let Some(pending) = peer.pending_offer.take() else {
                            debug!(
                                "SFU: ApplyServerAnswer for {} with no pending offer",
                                participant_id
                            );
                            continue;
                        };
                        peer.pending_offer_sent = None;
                        let answer: str0m::change::SdpAnswer =
                            match serde_json::from_str(&sdp_answer_json) {
                                Ok(a) => a,
                                Err(e) => {
                                    warn!(
                                        "SFU: ApplyServerAnswer parse failed for {}: {}",
                                        participant_id, e
                                    );
                                    // Restore the pending offer so deferred
                                    // tracks don't get orphaned.
                                    peer.pending_offer = Some(pending);
                                    continue;
                                }
                            };
                        if let Err(e) = peer.rtc.sdp_api().accept_answer(pending, answer) {
                            warn!("SFU: accept_answer for {} failed: {:?}", participant_id, e);
                        } else {
                            info!(
                                "SFU: server-offer answer accepted for {} ({} outbound tracks live)",
                                participant_id,
                                peer.tracks_out.len()
                            );
                        }
                        // Deferred tracks drain AFTER poll_output — see
                        // the post-poll sweep below.  Draining here would
                        // call sdp_api().apply() before str0m has
                        // processed the accept_answer, producing offers
                        // that never materialise into working media.
                    }
                    Ok(SfuCommand::ApplyPipeRenegotiationOffer {
                        participant_id,
                        sdp_offer_json,
                        remote_did,
                        room_id,
                    }) => {
                        let Some(peer) = peers.get_mut(&participant_id) else {
                            debug!(
                                "SFU: ApplyPipeRenegotiationOffer for unknown participant {}",
                                participant_id
                            );
                            continue;
                        };
                        if !peer.is_pipe {
                            warn!(
                                "SFU: ApplyPipeRenegotiationOffer landed on non-pipe peer {} — dropping",
                                participant_id
                            );
                            continue;
                        }
                        let offer: SdpOffer = match serde_json::from_str(&sdp_offer_json) {
                            Ok(o) => o,
                            Err(e) => {
                                warn!(
                                    "SFU: ApplyPipeRenegotiationOffer parse failed for {}: {}",
                                    participant_id, e
                                );
                                continue;
                            }
                        };
                        match peer.rtc.sdp_api().accept_offer(offer) {
                            Ok(answer) => match serde_json::to_string(&answer) {
                                Ok(answer_json) => {
                                    let payload = crate::sfu::SfuPipeRenegotiationAnswer {
                                        remote_did,
                                        room_id,
                                        sdp_answer: answer_json,
                                    };
                                    if let Ok(payload_json) = serde_json::to_string(&payload) {
                                        crate::pubsub::get_global_pubsub_sync().publish_sync(
                                            &crate::pubsub::SFU_PIPE_RENEGOTIATION_ANSWER_TOPIC,
                                            &payload_json,
                                        );
                                    }
                                    info!(
                                        "SFU: pipe renegotiation answer published for {}",
                                        participant_id
                                    );
                                }
                                Err(e) => {
                                    warn!(
                                            "SFU: serialize pipe renegotiation answer for {} failed: {}",
                                            participant_id, e
                                        );
                                }
                            },
                            Err(e) => {
                                warn!(
                                    "SFU: accept_offer for pipe {} failed: {:?}",
                                    participant_id, e
                                );
                            }
                        }
                    }
                    Ok(SfuCommand::GetQualityPreferences { reply }) => {
                        let _ = reply.send(quality_preferences.clone());
                    }
                    Ok(SfuCommand::Shutdown) => {
                        info!("SFU event loop shutting down");
                        return;
                    }
                    Err(mpsc::error::TryRecvError::Empty) => break,
                    Err(mpsc::error::TryRecvError::Disconnected) => {
                        info!("SFU command channel closed, shutting down");
                        return;
                    }
                }
            }

            // Clean out disconnected peers.  For pipe peers, notify the
            // cascade cleanup task so it can remove the stale PipeMeta
            // entry — otherwise the cascade manager reports phantom
            // established pipes after a remote node crash.
            {
                let mut dead_pipes: Vec<DeadPipe> = Vec::new();
                peers.retain(|pid, peer| {
                    if !peer.rtc.is_alive() {
                        info!("SFU: peer {} disconnected", pid);
                        relay.remove_participant(pid);
                        if peer.is_pipe {
                            dead_pipes.push(DeadPipe {
                                room_id: peer.room_id.to_string(),
                                remote_did: peer.agent_did.clone(),
                                participant_id: pid.clone(),
                            });
                        }
                        false
                    } else {
                        true
                    }
                });
                for dp in dead_pipes {
                    let _ = dead_pipe_tx.try_send(dp);
                }
            }

            // Sweep stale pending offers — if the client (or remote SFU)
            // never answered within 10 seconds, clear the pending state
            // so the next renegotiation cycle can retry.
            let stale_threshold = Duration::from_secs(10);
            for (pid, peer) in peers.iter_mut() {
                if peer.pending_offer.is_some() {
                    if let Some(sent_at) = peer.pending_offer_sent {
                        if sent_at.elapsed() > stale_threshold {
                            warn!("SFU: peer {} pending offer stale (>10s), clearing", pid);
                            peer.pending_offer = None;
                            peer.pending_offer_sent = None;
                            // Deferred tracks drain in the post-poll
                            // sweep — same reasoning as ApplyServerAnswer.
                        }
                    }
                }
            }

            // Poll all peers for output
            let mut earliest_timeout = Instant::now() + Duration::from_millis(100);
            let mut media_to_relay: Vec<(ParticipantId, MediaData)> = Vec::new();
            let mut tracks_opened: Vec<(ParticipantId, Mid, MediaKind)> = Vec::new();
            let mut keyframe_requests: Vec<(ParticipantId, KeyframeRequest)> = Vec::new();

            for (pid, peer) in peers.iter_mut() {
                loop {
                    if !peer.rtc.is_alive() {
                        break;
                    }

                    match peer.rtc.poll_output() {
                        Ok(Output::Transmit(transmit)) => {
                            if let Err(e) =
                                socket.try_send_to(&transmit.contents, transmit.destination)
                            {
                                debug!(
                                    "SFU: failed to send UDP to {}: {}",
                                    transmit.destination, e
                                );
                            }
                        }
                        Ok(Output::Timeout(t)) => {
                            earliest_timeout = earliest_timeout.min(t);
                            break;
                        }
                        Ok(Output::Event(event)) => match event {
                            Event::IceConnectionStateChange(state) => {
                                info!("SFU: peer {} ICE state: {:?}", pid, state);
                                if state == IceConnectionState::Disconnected {
                                    peer.rtc.disconnect();
                                }
                            }
                            Event::MediaAdded(e) => {
                                info!("SFU: peer {} added {:?} track mid={}", pid, e.kind, e.mid);
                                peer.tracks_in.insert(e.mid, e.kind);
                                tracks_opened.push((pid.clone(), e.mid, e.kind));
                            }
                            Event::MediaData(data) => {
                                media_to_relay.push((pid.clone(), data));
                            }
                            Event::KeyframeRequest(req) => {
                                keyframe_requests.push((pid.clone(), req));
                            }
                            _ => {}
                        },
                        Err(e) => {
                            warn!("SFU: peer {} poll error: {:?}", pid, e);
                            peer.rtc.disconnect();
                            break;
                        }
                    }
                }
            }

            // Relay media data to other peers in the same room
            for (origin_pid, data) in &media_to_relay {
                let origin_room = match peers.get(origin_pid) {
                    Some(p) => p.room_id.clone(),
                    None => continue,
                };

                // Update relay with voice activity for active speaker detection
                if data.params.spec().codec.is_audio() {
                    relay.update_voice_activity(origin_pid, data);
                }

                // Forward to all other peers in the same room
                for (target_pid, target_peer) in peers.iter_mut() {
                    if target_pid == origin_pid {
                        continue;
                    }
                    if target_peer.room_id != origin_room {
                        continue;
                    }

                    // Simulcast filter: when the source sends multiple
                    // layers (data.rid = Some("high"|"medium"|"low")),
                    // only forward the layer matching the subscriber's
                    // quality preference.  Non-simulcast packets
                    // (rid = None) always pass through.
                    if !should_forward_rid(
                        data.rid.as_ref(),
                        quality_preferences.get(target_pid).map(|s| s.as_str()),
                    ) {
                        continue;
                    }

                    // Find the outgoing Mid on the target peer that maps to this origin track
                    if let Some(&out_mid) = target_peer
                        .tracks_out_rev
                        .get(&(origin_pid.clone(), data.mid))
                    {
                        if let Some(writer) = target_peer.rtc.writer(out_mid) {
                            if let Err(e) = writer.write(
                                data.pt,
                                data.network_time,
                                data.time,
                                data.data.clone(),
                            ) {
                                warn!(
                                    "SFU: failed to write media to peer {} mid {}: {:?}",
                                    target_pid, out_mid, e
                                );
                            }
                        }
                    }
                }
            }

            // Drain deferred tracks for peers whose pending_offer just
            // cleared.  Running here — AFTER poll_output — ensures str0m
            // has fully processed the previous accept_answer before we
            // create a new SDP offer via sdp_api().apply().  The command-
            // processing phase (ApplyServerAnswer) and stale-offer sweep
            // only clear pending_offer; the actual drain happens here.
            for (pid, peer) in peers.iter_mut() {
                if peer.pending_offer.is_none() && !peer.deferred_tracks.is_empty() {
                    if let Some((sdp_offer_json, track_mapping)) = apply_deferred_tracks(peer) {
                        publish_renegotiation_offer(peer, pid, sdp_offer_json, track_mapping);
                    }
                }
            }

            // Propagate freshly-opened inbound tracks bidirectionally.
            //
            // When peer X opens a new inbound track, the SFU needs to
            // (a) plumb that track outbound on every OTHER peer Y so Y
            // receives it, AND (b) plumb every OTHER peer's existing
            // inbound tracks outbound on X so X receives them.  Without
            // (b) the newly-joined peer never gets forwarded media.
            //
            // We collect the per-target (kind, origin_pid, origin_mid)
            // triples first, then materialise the add_media + publish
            // for each target.  str0m only allows one in-flight change
            // per peer, so we skip targets that already have a pending
            // offer; the next tick (after the answer lands) will retry.
            //
            // Identify "new" peers in this tick — peers that opened at
            // least one inbound track.  These are the peers that need
            // both (a) outbound on others and (b) outbound for others'
            // existing tracks on themselves.
            let mut adds_per_target: HashMap<ParticipantId, Vec<(MediaKind, ParticipantId, Mid)>> =
                HashMap::new();
            let mut new_peers: HashSet<ParticipantId> = HashSet::new();
            for (origin_pid, _mid, _kind) in &tracks_opened {
                new_peers.insert(origin_pid.clone());
            }

            // (a) For each freshly-opened track, every OTHER peer in
            // the same room gets an outbound m-line for it.
            for (origin_pid, mid, kind) in &tracks_opened {
                let Some(origin_peer) = peers.get(origin_pid) else {
                    continue;
                };
                let room_id = origin_peer.room_id.clone();
                for (target_pid, target_peer) in peers.iter() {
                    if target_pid == origin_pid {
                        continue;
                    }
                    if target_peer.room_id != room_id {
                        continue;
                    }
                    adds_per_target
                        .entry(target_pid.clone())
                        .or_default()
                        .push((*kind, origin_pid.clone(), *mid));
                }
            }

            // (b) For each newly-joined peer, plumb every OTHER peer's
            // pre-existing inbound tracks outbound on the new peer.
            // This covers the asymmetric case where Y was already in
            // the room when X joined.  We skip tracks_in entries that
            // were opened in *this* tick because (a) already handled
            // them, but checking is cheap and idempotency saves a bug.
            for new_pid in &new_peers {
                let Some(new_peer) = peers.get(new_pid) else {
                    continue;
                };
                let room_id = new_peer.room_id.clone();
                for (other_pid, other_peer) in peers.iter() {
                    if other_pid == new_pid {
                        continue;
                    }
                    if other_peer.room_id != room_id {
                        continue;
                    }
                    for (other_mid, other_kind) in &other_peer.tracks_in {
                        // Skip if this triple was already added via (a).
                        let already_added = adds_per_target
                            .get(new_pid)
                            .map(|v| {
                                v.iter()
                                    .any(|(_, opid, omid)| opid == other_pid && omid == other_mid)
                            })
                            .unwrap_or(false);
                        if already_added {
                            continue;
                        }
                        adds_per_target.entry(new_pid.clone()).or_default().push((
                            *other_kind,
                            other_pid.clone(),
                            *other_mid,
                        ));
                    }
                }
            }

            // Snapshot pid → agent_did so the renegotiation loop can
            // build track attribution without re-borrowing peers.
            let pid_to_did: HashMap<ParticipantId, String> = peers
                .iter()
                .map(|(pid, peer)| (pid.clone(), peer.agent_did.clone()))
                .collect();

            // Materialise per-target.  One sdp_api batch per target
            // peer so a single offer carries every new outbound m-line
            // (str0m permits exactly one in-flight change at a time).
            for (target_pid, additions) in adds_per_target.into_iter() {
                let Some(target_peer) = peers.get_mut(&target_pid) else {
                    continue;
                };
                if target_peer.pending_offer.is_some() {
                    for (kind, origin_pid, origin_mid) in &additions {
                        if !target_peer
                            .tracks_out_rev
                            .contains_key(&(origin_pid.clone(), *origin_mid))
                        {
                            if let Some(did) = pid_to_did.get(origin_pid) {
                                target_peer.deferred_tracks.push((
                                    origin_pid.clone(),
                                    *origin_mid,
                                    *kind,
                                    did.clone(),
                                ));
                            }
                        }
                    }
                    continue;
                }
                let room_id = target_peer.room_id.clone();
                let mut api = target_peer.rtc.sdp_api();
                let mut new_outbound: Vec<(Mid, ParticipantId, Mid, MediaKind)> = Vec::new();
                for (kind, origin_pid, origin_mid) in &additions {
                    let new_mid =
                        api.add_media(*kind, str0m::media::Direction::SendOnly, None, None, None);
                    new_outbound.push((new_mid, origin_pid.clone(), *origin_mid, *kind));
                }
                let Some((offer, pending)) = api.apply() else {
                    continue;
                };

                // Build track attribution from the newly assigned mids.
                let track_mapping: Vec<crate::sfu::TrackMapEntry> = new_outbound
                    .iter()
                    .filter_map(|(new_mid, origin_pid, _, kind)| {
                        let did = pid_to_did.get(origin_pid)?;
                        Some(crate::sfu::TrackMapEntry {
                            mid: new_mid.to_string(),
                            agent_did: did.clone(),
                            media_kind: if kind.is_audio() {
                                "audio".to_string()
                            } else {
                                "video".to_string()
                            },
                        })
                    })
                    .collect();

                for (new_mid, origin_pid, origin_mid, _kind) in new_outbound {
                    target_peer
                        .tracks_out
                        .insert(new_mid, (origin_pid.clone(), origin_mid));
                    target_peer
                        .tracks_out_rev
                        .insert((origin_pid, origin_mid), new_mid);
                }
                target_peer.pending_offer = Some(pending);
                target_peer.pending_offer_sent = Some(Instant::now());
                info!(
                    "SFU: peer {} renegotiation offer prepared ({} new outbound m-lines)",
                    target_pid,
                    additions.len()
                );

                let sdp_offer_json = match serde_json::to_string(&offer) {
                    Ok(s) => s,
                    Err(e) => {
                        warn!(
                            "SFU: serialise renegotiation offer for {} failed: {}",
                            target_pid, e
                        );
                        continue;
                    }
                };
                if target_peer.is_pipe {
                    let payload = crate::sfu::SfuPipeRenegotiationOffer {
                        remote_did: target_peer.agent_did.clone(),
                        room_id: room_id.to_string(),
                        sdp_offer: sdp_offer_json,
                    };
                    if let Ok(payload_json) = serde_json::to_string(&payload) {
                        crate::pubsub::get_global_pubsub_sync().publish_sync(
                            &crate::pubsub::SFU_PIPE_RENEGOTIATION_OFFER_TOPIC,
                            &payload_json,
                        );
                    }
                } else {
                    let payload = crate::sfu::SfuCallRenegotiationOffer {
                        target_did: target_peer.agent_did.clone(),
                        neighbourhood_url: room_id.neighbourhood_url.clone(),
                        room_name: room_id.room_name.clone(),
                        sdp_offer: sdp_offer_json,
                        track_mapping,
                    };
                    if let Ok(payload_json) = serde_json::to_string(&payload) {
                        crate::pubsub::get_global_pubsub_sync().publish_sync(
                            &crate::pubsub::SFU_CALL_RENEGOTIATION_OFFER_TOPIC,
                            &payload_json,
                        );
                    }
                }
            }

            // Handle keyframe requests — route to the originating peer
            for (requesting_pid, req) in &keyframe_requests {
                let requesting_peer = match peers.get(requesting_pid) {
                    Some(p) => p,
                    None => continue,
                };

                // The keyframe request is for an outgoing track on the requesting peer.
                // Find which origin peer owns that track.
                if let Some((origin_pid, origin_mid)) =
                    requesting_peer.tracks_out.get(&req.mid).cloned()
                {
                    if let Some(origin_peer) = peers.get_mut(&origin_pid) {
                        if let Some(mut writer) = origin_peer.rtc.writer(origin_mid) {
                            let _ = writer.request_keyframe(None, KeyframeRequestKind::Pli);
                        }
                    }
                }
            }

            // Read from the UDP socket with timeout
            let duration = (earliest_timeout - Instant::now()).max(Duration::from_millis(1));

            tokio::select! {
                result = socket.recv_from(&mut buf) => {
                    match result {
                        Ok((n, source)) => {
                            let data = &buf[..n];
                            if let Ok(contents) = data.try_into() {
                                let input = Input::Receive(
                                    Instant::now(),
                                    Receive {
                                        proto: Protocol::Udp,
                                        source,
                                        destination: local_addr,
                                        contents,
                                    },
                                );

                                // Demultiplex: find which peer accepts this packet
                                if let Some((_pid, peer)) = peers.iter_mut().find(|(_, p)| p.rtc.accepts(&input)) {
                                    if let Err(e) = peer.rtc.handle_input(input) {
                                        warn!("SFU: peer input error: {:?}", e);
                                        peer.rtc.disconnect();
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            error!("SFU: UDP recv error: {:?}", e);
                        }
                    }
                }
                _ = tokio::time::sleep(duration) => {
                    // Timeout — drive all peers forward
                }
            }

            // Drive time forward for all peers
            let now = Instant::now();
            for (_pid, peer) in peers.iter_mut() {
                let _ = peer.rtc.handle_input(Input::Timeout(now));
            }
        }
    }
}

/// Remove all `tracks_out` / `tracks_out_rev` entries that reference a
/// departed participant from every remaining peer in the same room.
/// Also purges any deferred tracks referencing the departed peer.
fn clean_stale_track_refs(
    peers: &mut HashMap<ParticipantId, SfuPeer>,
    departed_pid: &ParticipantId,
    room_id: &RoomId,
) {
    for (_pid, peer) in peers.iter_mut() {
        if peer.room_id != *room_id {
            continue;
        }
        let stale_mids: Vec<Mid> = peer
            .tracks_out
            .iter()
            .filter(|(_, (origin_pid, _))| origin_pid == departed_pid)
            .map(|(mid, _)| *mid)
            .collect();
        for mid in stale_mids {
            if let Some((origin_pid, origin_mid)) = peer.tracks_out.remove(&mid) {
                peer.tracks_out_rev.remove(&(origin_pid, origin_mid));
            }
        }
        peer.deferred_tracks
            .retain(|(origin_pid, _, _, _)| origin_pid != departed_pid);
    }
}

/// Drain deferred tracks from a peer and produce a renegotiation
/// offer.  Returns `(sdp_offer_json, track_mapping)` or `None` when
/// no actionable tracks remain.
fn apply_deferred_tracks(peer: &mut SfuPeer) -> Option<(String, Vec<crate::sfu::TrackMapEntry>)> {
    if peer.deferred_tracks.is_empty() {
        return None;
    }
    let deferred: Vec<_> = peer.deferred_tracks.drain(..).collect();

    // Filter out tracks already plumbed — must happen before
    // borrowing rtc via sdp_api().
    let to_add: Vec<_> = deferred
        .into_iter()
        .filter(|(origin_pid, origin_mid, _, _)| {
            !peer
                .tracks_out_rev
                .contains_key(&(origin_pid.clone(), *origin_mid))
        })
        .collect();

    if to_add.is_empty() {
        return None;
    }

    let mut api = peer.rtc.sdp_api();
    let mut new_outbound: Vec<(Mid, ParticipantId, Mid, MediaKind, String)> = Vec::new();
    for (origin_pid, origin_mid, kind, agent_did) in to_add {
        let new_mid = api.add_media(kind, str0m::media::Direction::SendOnly, None, None, None);
        new_outbound.push((new_mid, origin_pid, origin_mid, kind, agent_did));
    }

    let (offer, pending) = api.apply()?;

    let track_mapping: Vec<crate::sfu::TrackMapEntry> = new_outbound
        .iter()
        .map(
            |(new_mid, _, _, kind, agent_did)| crate::sfu::TrackMapEntry {
                mid: new_mid.to_string(),
                agent_did: agent_did.clone(),
                media_kind: if kind.is_audio() {
                    "audio".to_string()
                } else {
                    "video".to_string()
                },
            },
        )
        .collect();

    for (new_mid, origin_pid, origin_mid, _, _) in new_outbound {
        peer.tracks_out
            .insert(new_mid, (origin_pid.clone(), origin_mid));
        peer.tracks_out_rev
            .insert((origin_pid, origin_mid), new_mid);
    }

    peer.pending_offer = Some(pending);
    peer.pending_offer_sent = Some(Instant::now());

    let sdp_offer_json = serde_json::to_string(&offer).ok()?;
    Some((sdp_offer_json, track_mapping))
}

/// Publish a renegotiation offer for a peer's deferred tracks.
/// Routes to the appropriate pubsub topic based on whether the peer
/// represents a client or a pipe transport.
fn publish_renegotiation_offer(
    peer: &SfuPeer,
    pid: &ParticipantId,
    sdp_offer_json: String,
    track_mapping: Vec<crate::sfu::TrackMapEntry>,
) {
    if peer.is_pipe {
        let payload = crate::sfu::SfuPipeRenegotiationOffer {
            remote_did: peer.agent_did.clone(),
            room_id: peer.room_id.to_string(),
            sdp_offer: sdp_offer_json,
        };
        if let Ok(payload_json) = serde_json::to_string(&payload) {
            crate::pubsub::get_global_pubsub_sync().publish_sync(
                &crate::pubsub::SFU_PIPE_RENEGOTIATION_OFFER_TOPIC,
                &payload_json,
            );
        }
    } else {
        let payload = crate::sfu::SfuCallRenegotiationOffer {
            target_did: peer.agent_did.clone(),
            neighbourhood_url: peer.room_id.neighbourhood_url.clone(),
            room_name: peer.room_id.room_name.clone(),
            sdp_offer: sdp_offer_json,
            track_mapping,
        };
        if let Ok(payload_json) = serde_json::to_string(&payload) {
            crate::pubsub::get_global_pubsub_sync().publish_sync(
                &crate::pubsub::SFU_CALL_RENEGOTIATION_OFFER_TOPIC,
                &payload_json,
            );
        }
    }
    info!(
        "SFU: deferred-track renegotiation offer published for peer {}",
        pid
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_forward_rid_non_simulcast() {
        // No RID (non-simulcast sender) — always forward regardless
        // of subscriber preference.
        assert!(should_forward_rid(None, None));
        assert!(should_forward_rid(None, Some("high")));
        assert!(should_forward_rid(None, Some("medium")));
        assert!(should_forward_rid(None, Some("low")));
        assert!(should_forward_rid(None, Some("auto")));
    }

    #[test]
    fn test_forward_rid_default_high() {
        // No preference set → default to "high".
        let high = Rid::from("high");
        let medium = Rid::from("medium");
        let low = Rid::from("low");

        assert!(should_forward_rid(Some(&high), None));
        assert!(!should_forward_rid(Some(&medium), None));
        assert!(!should_forward_rid(Some(&low), None));
    }

    #[test]
    fn test_forward_rid_explicit_high() {
        let high = Rid::from("high");
        let medium = Rid::from("medium");
        let low = Rid::from("low");

        assert!(should_forward_rid(Some(&high), Some("high")));
        assert!(!should_forward_rid(Some(&medium), Some("high")));
        assert!(!should_forward_rid(Some(&low), Some("high")));
    }

    #[test]
    fn test_forward_rid_medium() {
        let high = Rid::from("high");
        let medium = Rid::from("medium");
        let low = Rid::from("low");

        assert!(!should_forward_rid(Some(&high), Some("medium")));
        assert!(should_forward_rid(Some(&medium), Some("medium")));
        assert!(!should_forward_rid(Some(&low), Some("medium")));
    }

    #[test]
    fn test_forward_rid_low() {
        let high = Rid::from("high");
        let medium = Rid::from("medium");
        let low = Rid::from("low");

        assert!(!should_forward_rid(Some(&high), Some("low")));
        assert!(!should_forward_rid(Some(&medium), Some("low")));
        assert!(should_forward_rid(Some(&low), Some("low")));
    }

    #[test]
    fn test_forward_rid_auto_maps_to_high() {
        let high = Rid::from("high");
        let medium = Rid::from("medium");
        let low = Rid::from("low");

        // "auto" forwards "high" layer only
        assert!(should_forward_rid(Some(&high), Some("auto")));
        assert!(!should_forward_rid(Some(&medium), Some("auto")));
        assert!(!should_forward_rid(Some(&low), Some("auto")));
    }

    #[test]
    fn test_forward_rid_unknown_preference_defaults_high() {
        let high = Rid::from("high");
        let medium = Rid::from("medium");

        // Unknown preference string falls through to high
        assert!(should_forward_rid(Some(&high), Some("ultra")));
        assert!(!should_forward_rid(Some(&medium), Some("ultra")));
    }

    #[test]
    fn test_stale_track_cleanup_after_peer_departure() {
        let room_id = RoomId::new("neighbourhood://test", "room1");
        let pid_a = ParticipantId::next();
        let pid_b = ParticipantId::next();
        let pid_c = ParticipantId::next();

        let mut peer_b = SfuPeer::new(
            pid_b.clone(),
            room_id.clone(),
            "did:key:peerB".to_string(),
            Rtc::builder().build(),
            false,
        );
        let mut peer_c = SfuPeer::new(
            pid_c.clone(),
            room_id.clone(),
            "did:key:peerC".to_string(),
            Rtc::builder().build(),
            false,
        );

        // Allocate outbound Mids via sdp_api
        let mid_b_out = peer_b.rtc.sdp_api().add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendOnly,
            None,
            None,
            None,
        );
        let mid_c_out = peer_c.rtc.sdp_api().add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendOnly,
            None,
            None,
            None,
        );

        // Use a separate Rtc to allocate an origin Mid
        let mut scratch_rtc = Rtc::builder().build();
        let origin_mid = scratch_rtc.sdp_api().add_media(
            MediaKind::Audio,
            str0m::media::Direction::RecvOnly,
            None,
            None,
            None,
        );

        // Peer B has outbound track referencing peer A
        peer_b
            .tracks_out
            .insert(mid_b_out, (pid_a.clone(), origin_mid));
        peer_b
            .tracks_out_rev
            .insert((pid_a.clone(), origin_mid), mid_b_out);

        // Peer C also has outbound track referencing peer A
        peer_c
            .tracks_out
            .insert(mid_c_out, (pid_a.clone(), origin_mid));
        peer_c
            .tracks_out_rev
            .insert((pid_a.clone(), origin_mid), mid_c_out);

        // Peer B also has a deferred track referencing peer A
        peer_b.deferred_tracks.push((
            pid_a.clone(),
            origin_mid,
            MediaKind::Audio,
            "did:key:peerA".to_string(),
        ));

        let mut peers: HashMap<ParticipantId, SfuPeer> = HashMap::new();
        peers.insert(pid_b.clone(), peer_b);
        peers.insert(pid_c.clone(), peer_c);

        // Clean up after peer A departs
        clean_stale_track_refs(&mut peers, &pid_a, &room_id);

        // Verify no remaining peer references the departed PID
        for (pid, peer) in &peers {
            for (_, (ref_pid, _)) in &peer.tracks_out {
                assert_ne!(
                    ref_pid, &pid_a,
                    "peer {} tracks_out still references departed peer",
                    pid
                );
            }
            for ((ref_pid, _), _) in &peer.tracks_out_rev {
                assert_ne!(
                    ref_pid, &pid_a,
                    "peer {} tracks_out_rev still references departed peer",
                    pid
                );
            }
            for (ref_pid, _, _, _) in &peer.deferred_tracks {
                assert_ne!(
                    ref_pid, &pid_a,
                    "peer {} deferred_tracks still references departed peer",
                    pid
                );
            }
        }

        // All entries pointed to peer A — maps should now be empty
        assert!(peers.get(&pid_b).unwrap().tracks_out.is_empty());
        assert!(peers.get(&pid_b).unwrap().tracks_out_rev.is_empty());
        assert!(peers.get(&pid_b).unwrap().deferred_tracks.is_empty());
        assert!(peers.get(&pid_c).unwrap().tracks_out.is_empty());
        assert!(peers.get(&pid_c).unwrap().tracks_out_rev.is_empty());
    }
}
