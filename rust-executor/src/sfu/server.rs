//! SFU server — manages the shared UDP socket, str0m Rtc instances,
//! and the Sans I/O event loop driven by tokio.

use std::collections::{HashMap, HashSet};
use std::net::SocketAddr;
use std::time::{Duration, Instant};

use log::{debug, error, info, warn};
use str0m::change::SdpOffer;
use str0m::media::{KeyframeRequest, KeyframeRequestKind, MediaData, MediaKind, Mid};
use str0m::net::Protocol;
use str0m::{net::Receive, Candidate, Event, IceConnectionState, Input, Output, Rtc};
use tokio::net::UdpSocket;
use tokio::sync::mpsc;

use super::relay::MediaRelay;
use super::room::{ParticipantId, RoomId};

/// A connected WebRTC peer managed by the SFU server.
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
}

impl std::fmt::Debug for SfuPeer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SfuPeer")
            .field("id", &self.id)
            .field("room_id", &self.room_id)
            .field("agent_did", &self.agent_did)
            .field("tracks_in", &self.tracks_in.len())
            .field("tracks_out", &self.tracks_out.len())
            .field("pending_offer", &self.pending_offer.is_some())
            .finish()
    }
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

/// The SFU server. Owns the UDP socket and drives the str0m event loop.
pub struct SfuServer {
    /// The bound local address of the UDP socket.
    pub local_addr: SocketAddr,
    /// Channel to send commands to the event loop.
    pub command_tx: mpsc::Sender<SfuCommand>,
}

impl SfuServer {
    /// Start the SFU server. Binds a UDP socket and spawns the event loop on tokio.
    pub async fn start(config: SfuServerConfig) -> Result<Self, std::io::Error> {
        let socket = UdpSocket::bind(config.bind_addr).await?;
        let local_addr = socket.local_addr()?;
        info!("SFU server bound to UDP {}", local_addr);

        let (command_tx, command_rx) = mpsc::channel(256);

        tokio::spawn(Self::event_loop(socket, command_rx, local_addr));

        Ok(Self {
            local_addr,
            command_tx,
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
                            info!("SFU: peer {} left room {}", pid, peer.room_id);
                            relay.remove_participant(&pid);
                            quality_preferences.remove(&pid);
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
                        quality_preferences.insert(participant_id, preference);
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
                        let answer: str0m::change::SdpAnswer =
                            match serde_json::from_str(&sdp_answer_json) {
                                Ok(a) => a,
                                Err(e) => {
                                    warn!(
                                        "SFU: ApplyServerAnswer parse failed for {}: {}",
                                        participant_id, e
                                    );
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

            // Clean out disconnected peers
            peers.retain(|pid, peer| {
                if !peer.rtc.is_alive() {
                    info!("SFU: peer {} disconnected", pid);
                    relay.remove_participant(pid);
                    false
                } else {
                    true
                }
            });

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
                            if let Err(e) = socket
                                .try_send_to(&transmit.contents, transmit.destination)
                                .map_err(|e| e)
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

                    // Apply quality preference filtering for video
                    if data.params.spec().codec.is_video() {
                        if let Some(rid) = &data.rid {
                            let pref = quality_preferences
                                .get(target_pid)
                                .map(|s| s.as_str())
                                .unwrap_or("high");
                            let rid_str = rid.to_string();
                            let skip = match pref {
                                "low" => rid_str != "low" && rid_str != "q",
                                "medium" => rid_str == "high" || rid_str == "f",
                                _ => false, // "high" and "auto" forward all
                            };
                            if skip {
                                continue;
                            }
                        }
                    }

                    // Find the outgoing Mid on the target peer that maps to this origin track
                    if let Some((&out_mid, _)) =
                        target_peer
                            .tracks_out
                            .iter()
                            .find(|(_, (src_pid, src_mid))| {
                                src_pid == origin_pid && *src_mid == data.mid
                            })
                    {
                        if let Some(writer) = target_peer.rtc.writer(out_mid) {
                            if let Err(e) = writer.write(
                                data.pt,
                                data.network_time,
                                data.time,
                                data.data.clone(),
                            ) {
                                debug!(
                                    "SFU: failed to write media to peer {} mid {}: {:?}",
                                    target_pid, out_mid, e
                                );
                            }
                        }
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

            // Materialise per-target.  One sdp_api batch per target
            // peer so a single offer carries every new outbound m-line
            // (str0m permits exactly one in-flight change at a time).
            for (target_pid, additions) in adds_per_target.into_iter() {
                let Some(target_peer) = peers.get_mut(&target_pid) else {
                    continue;
                };
                if target_peer.pending_offer.is_some() {
                    continue;
                }
                let room_id = target_peer.room_id.clone();
                let mut api = target_peer.rtc.sdp_api();
                let mut new_outbound: Vec<(Mid, ParticipantId, Mid)> = Vec::new();
                for (kind, origin_pid, origin_mid) in &additions {
                    let new_mid =
                        api.add_media(*kind, str0m::media::Direction::SendOnly, None, None, None);
                    new_outbound.push((new_mid, origin_pid.clone(), *origin_mid));
                }
                let Some((offer, pending)) = api.apply() else {
                    continue;
                };
                for (new_mid, origin_pid, origin_mid) in new_outbound {
                    target_peer
                        .tracks_out
                        .insert(new_mid, (origin_pid, origin_mid));
                }
                target_peer.pending_offer = Some(pending);
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
                let payload = crate::sfu::SfuCallRenegotiationOffer {
                    target_did: target_peer.agent_did.clone(),
                    neighbourhood_url: room_id.neighbourhood_url.clone(),
                    room_name: room_id.room_name.clone(),
                    sdp_offer: sdp_offer_json,
                };
                if let Ok(payload_json) = serde_json::to_string(&payload) {
                    crate::pubsub::get_global_pubsub_sync().publish_sync(
                        &crate::pubsub::SFU_CALL_RENEGOTIATION_OFFER_TOPIC,
                        &payload_json,
                    );
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
                            if e.kind() == std::io::ErrorKind::WouldBlock {
                                // Non-blocking mode, expected
                            } else {
                                error!("SFU: UDP recv error: {:?}", e);
                            }
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
