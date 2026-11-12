//! SFU server — the media UDP sockets ([`MediaSockets`]), str0m Rtc instances,
//! and the Sans I/O event loop driven by tokio.

use std::collections::{HashMap, HashSet};
use std::mem::Discriminant;
use std::net::SocketAddr;
use std::sync::Mutex as StdMutex;
use std::time::{Duration, Instant};

use log::{debug, error, info, warn};
use str0m::change::SdpOffer;
use str0m::channel::ChannelId;
use str0m::format::Codec;
use str0m::media::{
    KeyframeRequest, KeyframeRequestKind, MediaData, MediaKind, MediaTime, Mid, Rid,
};
use str0m::net::Protocol;
use str0m::{net::Receive, Candidate, Event, IceConnectionState, Input, Output, Rtc, RtcError};
use tokio::net::UdpSocket;
use tokio::sync::mpsc;

use super::hosts::{socket_for, MediaSockets};
use super::reachability::{assess, stun_reflexive, Advertised, SfuReachability};
use super::relay::MediaRelay;
use super::room::{ParticipantId, RoomId};
use super::simulcast::{is_keyframe, LayerSelector};

/// The call session whose media a track carries, by the id the clients of
/// this node know it by: a local session's own id, a remote one's
/// qualified with its node ([`super::cascade::remote_session_id`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TrackOrigin {
    pub participant_id: String,
    pub agent_did: String,
}

/// A connected WebRTC peer managed by the SFU server.
///
/// Two flavours share this struct so the event loop can drive them
/// uniformly:
///
/// - **Client peer** — `is_pipe = false`. `agent_did` is the user's
///   DID; renegotiation offers go out as an `sfu-call-renegotiation-offer`
///   event to that agent.
/// - **Pipe peer** — `is_pipe = true`. Represents the local end of a
///   pipe transport to a remote SFU node. `agent_did` is the remote
///   node's DID; renegotiation offers go out via the cascade gossip
///   transport instead (through `pipe_bus`) so the remote node receives them.
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
    /// (origin_pid, origin_mid, kind, the session the track carries).
    pub deferred_tracks: Vec<(ParticipantId, Mid, MediaKind, TrackOrigin)>,
    /// Pipe only: the remote session each inbound mid carries, from the
    /// track mappings of the remote node's offers. See [`SfuPeer::track_origin`].
    pub remote_origins: HashMap<Mid, TrackOrigin>,
    /// True for cross-node pipe transports — see [`SfuPeer`] docs.
    /// Affects how renegotiation offers are routed and how forward
    /// loops treat the participant.
    pub is_pipe: bool,
    /// Open data channels: ChannelId → label.  Populated from
    /// `Event::ChannelOpen`; used for data relay routing.
    pub channels: HashMap<ChannelId, String>,
    /// When ICE last reported `Disconnected` without coming back, or `None`.
    /// See [`ICE_DISCONNECT_GRACE`].
    pub disconnected_since: Option<Instant>,
    /// An ICE restart the client asked for (`sfu.callRestartIce`), from the
    /// request until its answer lands or its offer goes stale.
    pub ice_restart: Option<IceRestart>,
    /// (outbound mid, codec) of forwarded packets dropped because the
    /// peer negotiated no matching codec on that mid — each logged once,
    /// not per packet, so a track the peer cannot take does not flood
    /// the log at media rate.
    pub drops_logged: HashSet<(Mid, Discriminant<Codec>)>,
    /// (outbound mid, error kind) of failed media writes already logged
    /// — once each, not per packet.
    pub write_errors_logged: HashSet<(Mid, Discriminant<RtcError>)>,
}

/// A client's ICE restart in flight. The SFU stays the only offerer: the
/// restart travels as a server renegotiation offer, so it cannot cross an
/// offer of the SFU's own.
#[derive(Debug)]
pub struct IceRestart {
    /// When the client asked. The disconnect grace runs at least until
    /// `since + ICE_RESTART_GRACE`.
    pub since: Instant,
    /// The offer that carries the restart, once made, with its track
    /// mapping. `None` until the post-poll sweep makes it, after any other
    /// pending offer is answered.
    pub offer: Option<(String, Vec<crate::sfu::TrackMapEntry>)>,
    /// The client asked again while the offer waited: it moved again, or
    /// lost the answer. The offer is republished, and a fresh restart
    /// follows its answer, so the newest network gets new credentials.
    pub again: bool,
    /// Candidates trickled while the offer waits for its answer. The
    /// restart clears the remote candidates when the answer lands, so they
    /// are added after it.
    pub candidates: Vec<Candidate>,
}

/// How long a peer may stay ICE-disconnected before it is dropped.
/// `Disconnected` is transient by design — missed consent checks on a
/// busy host or a brief network blip — and usually returns to
/// `Connected` within seconds; dropping at once tore down live calls.
pub const ICE_DISCONNECT_GRACE: Duration = Duration::from_secs(10);

/// How long a peer whose client asked for an ICE restart is kept, from the
/// request, however long it was disconnected before. The restart's answer
/// ends it and restarts the disconnect clock: ICE checks again, but its state
/// arrives only on the next poll.
pub const ICE_RESTART_GRACE: Duration = Duration::from_secs(10);

/// The most trickled candidates held while an ICE restart's offer waits for
/// its answer. A client gathers a handful per network; later ones are dropped.
const MAX_HELD_CANDIDATES: usize = 64;

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
            remote_origins: HashMap::new(),
            is_pipe,
            channels: HashMap::new(),
            disconnected_since: None,
            ice_restart: None,
            drops_logged: HashSet::new(),
            write_errors_logged: HashSet::new(),
        }
    }

    /// Track an ICE state change: `Disconnected` starts the disconnect
    /// clock, any other state stops it.
    pub fn on_ice_state(&mut self, state: IceConnectionState, now: Instant) {
        self.disconnected_since = match state {
            IceConnectionState::Disconnected => self.disconnected_since.or(Some(now)),
            _ => None,
        };
    }

    /// Whether the peer stayed disconnected past its grace and is to be
    /// dropped. See [`ICE_DISCONNECT_GRACE`] and [`ICE_RESTART_GRACE`].
    pub fn stayed_disconnected(&self, now: Instant) -> bool {
        let Some(since) = self.disconnected_since else {
            return false;
        };
        if now < since + ICE_DISCONNECT_GRACE {
            return false;
        }
        match &self.ice_restart {
            Some(restart) => now >= restart.since + ICE_RESTART_GRACE,
            None => true,
        }
    }

    /// The session whose media this peer's inbound track `mid` carries. A
    /// client peer sends its own; a pipe carries other nodes' sessions, as
    /// their offers named them. A mid no offer named (a node on an older
    /// build sends no mapping) is the pipe's own: its id and the remote
    /// node's DID, as before track mappings.
    pub fn track_origin(&self, mid: Mid) -> TrackOrigin {
        if self.is_pipe {
            if let Some(origin) = self.remote_origins.get(&mid) {
                return origin.clone();
            }
        }
        TrackOrigin {
            participant_id: self.id.to_string(),
            agent_did: self.agent_did.clone(),
        }
    }

    /// Pipe: remember the session each m-line of the remote node's offer
    /// carries (`track_mapping`, by the remote node's ids).
    pub fn note_remote_origins(&mut self, track_mapping: &[crate::sfu::TrackMapEntry]) {
        for entry in track_mapping {
            self.remote_origins.insert(
                Mid::from(entry.mid.as_str()),
                TrackOrigin {
                    participant_id: super::cascade::remote_session_id(
                        &entry.participant_id,
                        &self.agent_did,
                    ),
                    agent_did: entry.agent_did.clone(),
                },
            );
        }
    }

    /// The client asked for an ICE restart: new ICE credentials, offered
    /// with the SFU's candidates. The offer is made in the post-poll sweep,
    /// once str0m has processed any answer accepted in this tick, so this
    /// only records the request. Asked again while its offer waits, it
    /// returns that offer to republish and restarts again after its answer.
    pub fn restart_ice(
        &mut self,
        now: Instant,
    ) -> Option<(String, Vec<crate::sfu::TrackMapEntry>)> {
        match &mut self.ice_restart {
            Some(IceRestart {
                offer: Some(offer),
                again,
                ..
            }) => {
                *again = true;
                Some(offer.clone())
            }
            Some(_) => None,
            None => {
                self.ice_restart = Some(IceRestart {
                    since: now,
                    offer: None,
                    again: false,
                    candidates: Vec::new(),
                });
                None
            }
        }
    }

    /// Whether a change waits for the next offer: deferred tracks, or an
    /// ICE restart not yet offered.
    fn has_deferred_changes(&self) -> bool {
        !self.deferred_tracks.is_empty()
            || self
                .ice_restart
                .as_ref()
                .is_some_and(|restart| restart.offer.is_none())
    }

    /// A trickled remote candidate. Held while an ICE restart's offer waits
    /// for its answer: the restart would discard it.
    pub fn add_remote_candidate(&mut self, candidate: Candidate) {
        match &mut self.ice_restart {
            Some(restart) if restart.offer.is_some() => {
                if restart.candidates.len() < MAX_HELD_CANDIDATES {
                    restart.candidates.push(candidate);
                } else {
                    debug!(
                        "SFU: peer {} trickled too many candidates during an ICE restart",
                        self.id
                    );
                }
            }
            _ => self.rtc.add_remote_candidate(candidate),
        }
    }

    /// Apply the client's answer to the SFU's pending offer. An answer to
    /// an ICE restart's offer ends the restart and adds the candidates
    /// trickled meanwhile, then starts the next restart if one was asked for.
    pub fn accept_server_answer(&mut self, sdp_answer_json: &str) -> Result<(), String> {
        let Some(pending) = self.pending_offer.take() else {
            return Err("no pending offer".to_string());
        };
        let answer: str0m::change::SdpAnswer = match serde_json::from_str(sdp_answer_json) {
            Ok(a) => a,
            Err(e) => {
                // Restore the pending offer so deferred tracks don't get
                // orphaned. Keep pending_offer_sent so the stale-offer
                // sweep can eventually clear and retry.
                self.pending_offer = Some(pending);
                return Err(format!("answer parse failed: {}", e));
            }
        };
        // SDP parsed successfully — clear the stale-offer timestamp so the
        // sweep no longer considers this peer.
        self.pending_offer_sent = None;
        let accepted = self
            .rtc
            .sdp_api()
            .accept_answer(pending, answer)
            .map_err(|e| format!("accept_answer failed: {:?}", e));
        if self
            .ice_restart
            .as_ref()
            .is_some_and(|restart| restart.offer.is_some())
        {
            if let Some(restart) = self.ice_restart.take() {
                // ICE checks again from here, but its state arrives only on
                // the next poll, after the disconnect sweep: restart the clock.
                if accepted.is_ok() {
                    if let Some(since) = self.disconnected_since.as_mut() {
                        *since = Instant::now();
                    }
                }
                for candidate in restart.candidates {
                    self.rtc.add_remote_candidate(candidate);
                }
                if restart.again {
                    self.ice_restart = Some(IceRestart {
                        since: Instant::now(),
                        offer: None,
                        again: false,
                        candidates: Vec::new(),
                    });
                }
            }
        }
        accepted
    }

    /// The SFU's pending offer got no answer in time: forget it, and the
    /// ICE restart it carried.
    fn drop_stale_offer(&mut self) {
        self.pending_offer = None;
        self.pending_offer_sent = None;
        if self
            .ice_restart
            .as_ref()
            .is_some_and(|restart| restart.offer.is_some())
        {
            self.ice_restart = None;
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
            .field("channels", &self.channels.len())
            .field("ice_restart", &self.ice_restart.as_ref().map(|r| r.since))
            .field("drops_logged", &self.drops_logged.len())
            .field("write_errors_logged", &self.write_errors_logged.len())
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
    /// A remote SFU sent a renegotiation offer over the cascade
    /// gossip transport, addressed to our local end of an existing
    /// pipe.  The event loop applies the offer through the pipe's
    /// `sdp_api()`, produces an answer, and republishes it to
    /// the pipe bus (`pipe_bus::publish_answer`) so the cascade router
    /// can ship it back over gossip.
    ApplyPipeRenegotiationOffer {
        participant_id: ParticipantId,
        sdp_offer_json: String,
        /// DID of the remote SFU that sent the offer.  Echoed back so
        /// the answer can be addressed correctly.
        remote_did: String,
        /// String form of the [`crate::sfu::room::RoomId`].
        room_id: String,
        /// The remote session each new m-line carries, by the remote node's ids.
        track_mapping: Vec<crate::sfu::TrackMapEntry>,
    },
    /// Trickle ICE: add a remote ICE candidate to an existing peer.
    /// The candidate arrives as an SDP candidate-attribute string
    /// (the `candidate:` line from an `RTCIceCandidateInit`).
    AddIceCandidate {
        participant_id: ParticipantId,
        /// SDP candidate-attribute string, e.g.
        /// `"candidate:0 1 UDP 2130706431 192.168.0.10 12345 typ host"`.
        candidate_sdp: String,
    },
    /// Relay data to other peers in a room via the data-channel path.
    /// The event loop writes the payload to every other peer's
    /// matching data channel (by label).
    RelayData {
        participant_id: ParticipantId,
        channel_label: String,
        data: Vec<u8>,
        binary: bool,
    },
    /// The client asked for an ICE restart of its own peer, e.g. after a
    /// network change. The SFU offers new ICE credentials through the
    /// server-offer path; the client answers with `ApplyServerAnswer`.
    RestartIce { participant_id: ParticipantId },
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
    /// Address to bind the UDP socket on.  The IP becomes the ICE host
    /// candidate in every SDP answer, so it must equal the address
    /// remote clients can reach. See [`sfu_bind_ip`] for how the executor
    /// picks it.
    ///
    /// An unspecified IP (`0.0.0.0`) is the LAN default: a socket on
    /// loopback and one on the LAN address, both offered (see
    /// [`MediaSockets`]). Nothing binds `0.0.0.0` itself.
    ///
    /// Port `:0` lets the OS pick a free UDP port; any other port is bound
    /// exactly, or startup fails.
    pub bind_addr: SocketAddr,
    /// STUN server (`host:port`) for the startup probe of the SFU's public
    /// address; `None` skips it. See [`assess`].
    pub stun_server: Option<String>,
    /// Public IP to advertise behind NAT (`--sfu-public-addr`). `None`: the
    /// probe's reflexive IP.
    pub public_ip: Option<std::net::IpAddr>,
    /// DID of the cascade node that takes redirected joins first.
    pub preferred_node: Option<String>,
    /// DIDs of the configured cascade peers, as `sfu.getConfig` reports them.
    pub cascade_peer_dids: Vec<String>,
}

impl Default for SfuServerConfig {
    /// Loopback: media reaches only this machine until a bind address
    /// says otherwise.
    fn default() -> Self {
        Self {
            bind_addr: SocketAddr::new(std::net::Ipv4Addr::LOCALHOST.into(), 0),
            stun_server: None,
            public_ip: None,
            preferred_node: None,
            cascade_peer_dids: Vec::new(),
        }
    }
}

/// The IP the SFU binds.
///
/// - `--sfu-bind-addr` wins: that address, and only it, is bound and offered.
/// - Otherwise the LAN default, returned as `0.0.0.0`: loopback plus the LAN
///   address, whatever `--localhost` says. Firefox gathers no loopback
///   candidate and cannot reach an SFU on 127.0.0.1 alone, even on the same
///   machine. In localhost mode the plain RPC port stays on loopback; the SFU
///   adds only its media UDP port on the LAN, and ICE credentials plus DTLS
///   guard it (a TLS listener binds `0.0.0.0` on its own).
///   The LAN address is detected again for every call (a laptop that changes
///   networks); with none, calls offer loopback only. See [`MediaSockets`].
///
/// A malformed address, or an unspecified one (`0.0.0.0`, `::`: str0m cannot
/// offer it as a candidate), falls back to loopback, never to a wider
/// interface.
pub fn sfu_bind_ip(explicit: Option<&str>) -> std::net::IpAddr {
    let loopback = std::net::Ipv4Addr::LOCALHOST.into();
    match explicit {
        Some(addr) => match addr.parse::<std::net::IpAddr>() {
            Ok(ip) if !ip.is_unspecified() => ip,
            Ok(_) => {
                warn!(
                    "SFU bind address `{}` is unspecified; it needs a real interface IP — binding loopback",
                    addr
                );
                loopback
            }
            Err(e) => {
                warn!(
                    "SFU bind address `{}` parse error: {} — binding loopback",
                    addr, e
                );
                loopback
            }
        },
        None => std::net::Ipv4Addr::UNSPECIFIED.into(),
    }
}

/// The SFU's bind address and the public IP it advertises, from the
/// `sfu_*` settings, or why they cannot work. `run` stops on an error:
/// with a port forwarded to this box, falling back to loopback or offering an
/// address no caller can use would leave every call broken with nothing to
/// say why.
///
/// - With a fixed `udp_port` or a `public_addr`, `bind_addr` must be a real
///   interface IP; otherwise a bad one still falls back to loopback, as
///   [`sfu_bind_ip`] does.
/// - `public_addr` must be an IP of the bind's family, and one a candidate
///   can carry: not unspecified, multicast, broadcast or link-local. It needs
///   a fixed `udp_port`: the router forwards only a port it knows.
pub fn sfu_media_addrs(
    bind_addr: Option<&str>,
    udp_port: Option<u16>,
    public_addr: Option<&str>,
) -> Result<(SocketAddr, Option<std::net::IpAddr>), String> {
    use std::net::IpAddr;
    let port = udp_port.unwrap_or(0);
    let strict = port != 0 || public_addr.is_some();
    let bind_ip = match bind_addr {
        Some(addr) if strict => match addr.parse::<IpAddr>() {
            Ok(ip) if !ip.is_unspecified() => ip,
            _ => {
                return Err(format!(
                    "sfu_bind_addr `{}` is not an IP address of this machine's interface: \
                     give the LAN IP the router forwards to",
                    addr
                ))
            }
        },
        _ => sfu_bind_ip(bind_addr),
    };
    let public_ip = match public_addr {
        None => None,
        Some(addr) => {
            let ip: IpAddr = addr.parse().map_err(|e| {
                format!(
                    "sfu_public_addr `{}` is not an IP address ({}): give the public IP, without a port",
                    addr, e
                )
            })?;
            let unusable = match ip {
                IpAddr::V4(v4) => {
                    v4.is_unspecified()
                        || v4.is_multicast()
                        || v4.is_broadcast()
                        || v4.is_link_local()
                }
                IpAddr::V6(v6) => {
                    v6.is_unspecified()
                        || v6.is_multicast()
                        || (v6.segments()[0] & 0xffc0) == 0xfe80
                }
            };
            if unusable {
                return Err(format!(
                    "sfu_public_addr `{}` cannot be offered to callers: give the public IP callers reach",
                    addr
                ));
            }
            if ip.is_ipv4() != bind_ip.is_ipv4() {
                return Err(format!(
                    "sfu_public_addr `{}` and the SFU's bind address {} are of different IP families",
                    addr, bind_ip
                ));
            }
            Some(ip)
        }
    };
    if public_ip.is_some() && port == 0 {
        return Err(
            "sfu_public_addr needs a fixed sfu_udp_port: callers reach the public address \
             only on a port the router forwards"
                .to_string(),
        );
    }
    Ok((SocketAddr::new(bind_ip, port), public_ip))
}

/// Notification emitted by the event loop when a peer's RTC dies. The
/// event loop has already dropped the peer; the service removes what it
/// holds: a client's room slot, or a pipe's cascade entry.
#[derive(Debug)]
pub struct DeadPeer {
    pub room_id: RoomId,
    /// The client's DID, or for a pipe the remote node's.
    pub agent_did: String,
    pub participant_id: ParticipantId,
    pub is_pipe: bool,
}

/// The SFU server. Owns the UDP sockets and drives the str0m event loop.
pub struct SfuServer {
    /// The address the SFU reports as bound: the explicit one, or in the LAN
    /// default the LAN socket bound at startup (loopback with no LAN).
    pub local_addr: SocketAddr,
    /// The bound sockets, and the host candidates a new connection gets
    /// ([`MediaSockets::host_addrs`]).
    pub sockets: MediaSockets,
    /// The public address offered next to `local_addr` behind NAT.
    pub advertised: Option<Advertised>,
    /// What the startup probe found.
    pub reachability: SfuReachability,
    /// Channel to send commands to the event loop.
    pub command_tx: mpsc::Sender<SfuCommand>,
    /// Notifications about peers whose RTC connection died. Unbounded: a
    /// dropped notification would leave the peer's room slot taken for good.
    pub dead_peer_rx: StdMutex<Option<mpsc::UnboundedReceiver<DeadPeer>>>,
}

impl SfuServer {
    /// Start the SFU server. Binds its UDP sockets and spawns the event loop on tokio.
    pub async fn start(config: SfuServerConfig) -> Result<Self, std::io::Error> {
        let sockets = MediaSockets::bind(config.bind_addr).await?;
        Self::start_on(config, sockets).await
    }

    /// Start the SFU server on sockets already bound.
    pub async fn start_on(
        config: SfuServerConfig,
        sockets: MediaSockets,
    ) -> Result<Self, std::io::Error> {
        let fixed_port = config.bind_addr.port() != 0;
        let local_addr = sockets.primary();

        // On the SFU's own socket, before the event loop reads it: the mapping
        // a STUN server sees is the one callers' packets come back through.
        // Loopback cannot reach a STUN server, so it is not probed.
        let probe = match (&config.stun_server, sockets.socket(local_addr)) {
            (Some(server), Some(socket)) if !local_addr.ip().is_loopback() => {
                Some(stun_reflexive(&socket, server).await)
            }
            _ => None,
        };
        let (advertised, reachability) =
            assess(local_addr, fixed_port, config.public_ip, probe.as_ref());
        info!(
            "SFU media: bind {} ({} port); advertised {}; reachability {}",
            local_addr,
            if fixed_port { "fixed" } else { "ephemeral" },
            match advertised {
                Some(a) => format!("{} (from {})", a.addr, a.source),
                None => "none".to_string(),
            },
            reachability
        );
        if let (true, Some(Ok(reflexive)), Some(a)) = (fixed_port, &probe, advertised) {
            if reflexive.port() != local_addr.port() {
                info!(
                    "SFU: the router maps outbound traffic to port {}; callers reach {} only \
                     through a port forward of UDP {}",
                    reflexive.port(),
                    a.addr,
                    local_addr.port()
                );
            }
        }

        let (command_tx, command_rx) = mpsc::channel(256);
        let (dead_peer_tx, dead_peer_rx) = mpsc::unbounded_channel();

        tokio::spawn(Self::event_loop(sockets.clone(), command_rx, dead_peer_tx));

        Ok(Self {
            local_addr,
            sockets,
            advertised,
            reachability,
            command_tx,
            dead_peer_rx: StdMutex::new(Some(dead_peer_rx)),
        })
    }

    /// Create an Rtc instance for a new peer, process the SDP offer, and return the answer.
    /// The Rtc instance is NOT yet added to the event loop — call `add_peer` after.
    ///
    /// The answer offers every address in `hosts` (a bound socket each, from
    /// [`MediaSockets::host_addrs`]) as a host candidate and, behind NAT,
    /// `advertised` as a server-reflexive one whose base is the first
    /// non-loopback host (the LAN socket the router forwards to). A caller's
    /// packets to the advertised address arrive addressed to that socket, so
    /// str0m matches them to its host candidate.
    pub fn create_rtc_for_offer(
        offer: SdpOffer,
        hosts: &[SocketAddr],
        advertised: Option<SocketAddr>,
    ) -> Result<(Rtc, String), String> {
        let mut rtc = Rtc::builder().build(Instant::now());

        for host in hosts {
            let candidate = Candidate::host(*host, "udp")
                .map_err(|e| format!("Failed to create host candidate: {}", e))?;
            rtc.add_local_candidate(candidate);
        }
        let base = hosts
            .iter()
            .find(|h| !h.ip().is_loopback())
            .or(hosts.first())
            .ok_or("The SFU has no address to offer")?;
        if let Some(public) = advertised {
            let candidate = Candidate::server_reflexive(public, *base, "udp")
                .map_err(|e| format!("Failed to create server-reflexive candidate: {}", e))?;
            rtc.add_local_candidate(candidate);
        }

        let answer = rtc
            .sdp_api()
            .accept_offer(offer)
            .map_err(|e| format!("Failed to accept SDP offer: {}", e))?;

        let answer_json = serde_json::to_string(&answer)
            .map_err(|e| format!("Failed to serialize SDP answer: {}", e))?;

        Ok((rtc, answer_json))
    }

    /// The main event loop. Reads UDP packets, drives str0m, and relays media.
    ///
    /// Reads every bound socket ([`MediaSockets`]); a packet's destination
    /// is the address of the socket it arrived on, which str0m needs to match
    /// a STUN request to its host candidate. A packet str0m sends leaves on
    /// the socket bound at its source. The socket set is reread when a call
    /// binds a new LAN address.
    async fn event_loop(
        media: MediaSockets,
        mut command_rx: mpsc::Receiver<SfuCommand>,
        dead_peer_tx: mpsc::UnboundedSender<DeadPeer>,
    ) {
        let mut peers: HashMap<ParticipantId, SfuPeer> = HashMap::new();
        let mut relay = MediaRelay::new();
        let mut quality_preferences: HashMap<ParticipantId, String> = HashMap::new();
        let mut layers = LayerSelector::default();
        let mut buf = vec![0u8; 2000];
        let (mut sockets_version, mut sockets) = media.snapshot();
        // The socket read first, rotated so a busy one cannot starve the rest.
        let mut first_read = 0usize;

        info!(
            "SFU event loop started on {:?}",
            sockets.iter().map(|(addr, _)| *addr).collect::<Vec<_>>()
        );

        loop {
            if media.version() != sockets_version {
                (sockets_version, sockets) = media.snapshot();
                info!(
                    "SFU event loop now reads {:?}",
                    sockets.iter().map(|(addr, _)| *addr).collect::<Vec<_>>()
                );
            }
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
                            layers.forget(&pid);
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
                        // The layer selector switches on the next packets
                        // and asks the senders for the keyframes it needs.
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
                        // A second answer to a republished restart offer finds none pending.
                        if peer.pending_offer.is_none() {
                            debug!(
                                "SFU: ApplyServerAnswer for {} with no pending offer",
                                participant_id
                            );
                            continue;
                        }
                        match peer.accept_server_answer(&sdp_answer_json) {
                            Ok(()) => info!(
                                "SFU: server-offer answer accepted for {} ({} outbound tracks live)",
                                participant_id,
                                peer.tracks_out.len()
                            ),
                            Err(e) => warn!("SFU: ApplyServerAnswer for {}: {}", participant_id, e),
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
                        track_mapping,
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
                        // Before the tracks open: they are attributed as they do.
                        peer.note_remote_origins(&track_mapping);
                        match peer.rtc.sdp_api().accept_offer(offer) {
                            Ok(answer) => match serde_json::to_string(&answer) {
                                Ok(answer_json) => {
                                    let payload = crate::sfu::SfuPipeRenegotiationAnswer {
                                        remote_did,
                                        room_id,
                                        sdp_answer: answer_json,
                                    };
                                    super::pipe_bus::publish_answer(payload);
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
                    Ok(SfuCommand::AddIceCandidate {
                        participant_id,
                        candidate_sdp,
                    }) => {
                        let Some(peer) = peers.get_mut(&participant_id) else {
                            debug!(
                                "SFU: AddIceCandidate for unknown participant {}",
                                participant_id
                            );
                            continue;
                        };
                        match Candidate::from_sdp_string(&candidate_sdp) {
                            Ok(candidate) => {
                                peer.add_remote_candidate(candidate);
                                debug!("SFU: trickle ICE candidate added for {}", participant_id);
                            }
                            Err(e) => {
                                warn!(
                                    "SFU: failed to parse ICE candidate for {}: {:?}",
                                    participant_id, e
                                );
                            }
                        }
                    }
                    Ok(SfuCommand::RelayData {
                        participant_id,
                        channel_label,
                        data,
                        binary,
                    }) => {
                        let origin_room = match peers.get(&participant_id) {
                            Some(p) => p.room_id.clone(),
                            None => {
                                debug!("SFU: RelayData for unknown participant {}", participant_id);
                                continue;
                            }
                        };
                        let origin_did = peers
                            .get(&participant_id)
                            .map(|p| p.agent_did.clone())
                            .unwrap_or_default();

                        // Relay to all other peers in the same room that
                        // have an open data channel with a matching label.
                        for (target_pid, target_peer) in peers.iter_mut() {
                            if *target_pid == participant_id {
                                continue;
                            }
                            if target_peer.room_id != origin_room {
                                continue;
                            }
                            if let Some((&cid, _)) = target_peer
                                .channels
                                .iter()
                                .find(|(_, label)| *label == &channel_label)
                            {
                                if let Some(mut ch) = target_peer.rtc.channel(cid) {
                                    if let Err(e) = ch.write(binary, &data) {
                                        debug!(
                                            "SFU: data relay to {} channel '{}' failed: {:?}",
                                            target_pid, channel_label, e
                                        );
                                    }
                                }
                            }
                        }

                        // Publish to events_ws so non-WebRTC clients can
                        // receive data channel messages too.
                        let payload = crate::sfu::types::SfuDataMessage::new(
                            origin_did.clone(),
                            participant_id.to_string(),
                            origin_room.neighbourhood_url.clone(),
                            origin_room.room_name.clone(),
                            channel_label.clone(),
                            binary,
                            &data,
                        );
                        let recipients = data_recipients(&peers, &origin_room, &participant_id);
                        crate::pubsub::get_global_pubsub_sync()
                            .publish_event(crate::events::Event::sfu_data(&payload, recipients));
                    }
                    Ok(SfuCommand::RestartIce { participant_id }) => {
                        let Some(peer) = peers.get_mut(&participant_id) else {
                            debug!("SFU: RestartIce for unknown participant {}", participant_id);
                            continue;
                        };
                        // A pipe renegotiates with its remote node, not a client.
                        if peer.is_pipe {
                            warn!("SFU: RestartIce for pipe {} ignored", participant_id);
                            continue;
                        }
                        info!("SFU: peer {} asked for an ICE restart", participant_id);
                        if let Some((sdp_offer_json, track_mapping)) =
                            peer.restart_ice(Instant::now())
                        {
                            publish_renegotiation_offer(
                                peer,
                                &participant_id,
                                sdp_offer_json,
                                track_mapping,
                            );
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

            // Clean out disconnected peers, and tell the service: a client
            // still holds a room slot (capacity, and its DID could not
            // rejoin), a pipe a cascade entry (phantom established pipes
            // after a remote node crash).
            {
                let mut dead_peers: Vec<DeadPeer> = Vec::new();
                let now = Instant::now();
                for (pid, peer) in peers.iter_mut() {
                    if peer.stayed_disconnected(now) {
                        info!("SFU: peer {} stayed disconnected, dropping it", pid);
                        peer.rtc.disconnect();
                    }
                }
                peers.retain(|pid, peer| {
                    if !peer.rtc.is_alive() {
                        info!("SFU: peer {} disconnected", pid);
                        relay.remove_participant(pid);
                        quality_preferences.remove(pid);
                        layers.forget(pid);
                        dead_peers.push(DeadPeer {
                            room_id: peer.room_id.clone(),
                            agent_did: peer.agent_did.clone(),
                            participant_id: pid.clone(),
                            is_pipe: peer.is_pipe,
                        });
                        false
                    } else {
                        true
                    }
                });
                for dead in dead_peers {
                    // Stale track references other peers held for it —
                    // matches the RemovePeer path.
                    clean_stale_track_refs(&mut peers, &dead.participant_id, &dead.room_id);
                    let _ = dead_peer_tx.send(dead);
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
                            peer.drop_stale_offer();
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
            // Data channel messages, published once the poll loop no longer borrows `peers`.
            let mut data_received: Vec<(ParticipantId, crate::sfu::types::SfuDataMessage)> =
                Vec::new();

            for (pid, peer) in peers.iter_mut() {
                loop {
                    if !peer.rtc.is_alive() {
                        break;
                    }

                    match peer.rtc.poll_output() {
                        Ok(Output::Transmit(transmit)) => {
                            let sent = match socket_for(&sockets, transmit.source) {
                                Some(socket) => socket
                                    .try_send_to(&transmit.contents, transmit.destination)
                                    .map(|_| ()),
                                None => Err(std::io::Error::other("no socket")),
                            };
                            if let Err(e) = sent {
                                debug!(
                                    "SFU: failed to send UDP from {} to {}: {}",
                                    transmit.source, transmit.destination, e
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
                                peer.on_ice_state(state, Instant::now());
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
                            Event::ChannelOpen(cid, label) => {
                                info!(
                                    "SFU: peer {} opened data channel '{}' (id {:?})",
                                    pid, label, cid
                                );
                                peer.channels.insert(cid, label);
                            }
                            Event::ChannelData(data) => {
                                // Relay data to all other peers in the same
                                // room that have a channel with the same
                                // label.  Collected here and relayed after
                                // the poll loop (same pattern as media).
                                if let Some(label) = peer.channels.get(&data.id).cloned() {
                                    let origin_room = peer.room_id.clone();
                                    let origin_did = peer.agent_did.clone();
                                    let relay_binary = data.binary;
                                    let relay_data = data.data;

                                    // Can't relay inside this loop (borrows
                                    // peers mutably): published after it, to
                                    // the room's agents, as `sfu-data`.
                                    let payload = crate::sfu::types::SfuDataMessage::new(
                                        origin_did,
                                        pid.to_string(),
                                        origin_room.neighbourhood_url,
                                        origin_room.room_name,
                                        label,
                                        relay_binary,
                                        &relay_data,
                                    );
                                    data_received.push((pid.clone(), payload));
                                }
                            }
                            Event::ChannelClose(cid) => {
                                if let Some(label) = peer.channels.remove(&cid) {
                                    info!("SFU: peer {} closed data channel '{}'", pid, label);
                                }
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

            if !data_received.is_empty() {
                let pubsub = crate::pubsub::get_global_pubsub_sync();
                for (sender, payload) in data_received {
                    let room = RoomId::new(&payload.neighbourhood_url, &payload.room_name);
                    let recipients = data_recipients(&peers, &room, &sender);
                    pubsub.publish_event(crate::events::Event::sfu_data(&payload, recipients));
                }
            }

            // Relay media data to other peers in the same room
            let now = Instant::now();
            let mut layer_keyframes: Vec<(ParticipantId, Mid, Option<Rid>)> = Vec::new();
            for (origin_pid, data) in &media_to_relay {
                let origin_room = match peers.get(origin_pid) {
                    Some(p) => p.room_id.clone(),
                    None => continue,
                };

                // Update relay with voice activity for active speaker detection
                if data.params.spec().codec.is_audio() {
                    relay.update_voice_activity(origin_pid, data);
                }

                let track = (origin_pid.clone(), data.mid);
                layers.observe(now, &track, data.rid.as_ref());
                let keyframe = is_keyframe(&data.codec_extra);
                // A gap in the origin's sequence (e.g. after an outage or an
                // ICE restart): receivers may need a keyframe to decode again.
                if !data.contiguous
                    && data.params.spec().codec.is_video()
                    && layers.gap(now, &track, data.rid.as_ref())
                {
                    layer_keyframes.push((origin_pid.clone(), data.mid, data.rid));
                }

                // Forward to all other peers in the same room
                for (target_pid, target_peer) in peers.iter_mut() {
                    if target_pid == origin_pid {
                        continue;
                    }
                    if target_peer.room_id != origin_room {
                        continue;
                    }

                    // The outgoing Mid on the target peer that maps to this
                    // origin track; a peer not (yet) subscribed gets nothing
                    // and needs no layer.
                    if let Some(&out_mid) = target_peer.tracks_out_rev.get(&track) {
                        // One simulcast layer per receiver (see `simulcast`).
                        let route = layers.route(
                            now,
                            target_pid,
                            &track,
                            data.rid.as_ref(),
                            keyframe,
                            quality_preferences.get(target_pid).map(|s| s.as_str()),
                        );
                        if let Some(rid) = route.request_keyframe {
                            layer_keyframes.push((origin_pid.clone(), data.mid, Some(rid)));
                        }
                        if !route.forward {
                            continue;
                        }
                        // One RTP clock per receiver across layer switches.
                        let time = layers.rtp_time(
                            data.network_time,
                            target_pid,
                            &track,
                            data.rid.as_ref(),
                            data.time,
                        );
                        write_forwarded(target_peer, out_mid, data, time);
                    }
                }
            }
            for (origin_pid, mid, rid) in layer_keyframes {
                if let Some(mut writer) = peers.get_mut(&origin_pid).and_then(|p| p.rtc.writer(mid))
                {
                    let _ = writer.request_keyframe(rid, KeyframeRequestKind::Pli);
                }
            }

            // Drain deferred tracks for peers whose pending_offer just
            // cleared.  Running here — AFTER poll_output — ensures str0m
            // has fully processed the previous accept_answer before we
            // create a new SDP offer via sdp_api().apply().  The command-
            // processing phase (ApplyServerAnswer) and stale-offer sweep
            // only clear pending_offer; the actual drain happens here.
            for (pid, peer) in peers.iter_mut() {
                if peer.pending_offer.is_none() && peer.has_deferred_changes() {
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

            for (target_pid, sdp_offer_json, track_mapping) in
                offer_new_tracks(&mut peers, adds_per_target)
            {
                if let Some(target_peer) = peers.get(&target_pid) {
                    publish_renegotiation_offer(
                        target_peer,
                        &target_pid,
                        sdp_offer_json,
                        track_mapping,
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
                    // A simulcast origin keys its streams by rid: ask for
                    // the layer this receiver gets.
                    let rid = layers.layer_of(requesting_pid, &(origin_pid.clone(), origin_mid));
                    if let Some(origin_peer) = peers.get_mut(&origin_pid) {
                        if let Some(mut writer) = origin_peer.rtc.writer(origin_mid) {
                            let _ = writer.request_keyframe(rid, req.kind);
                        }
                    }
                }
            }

            // Read from the UDP sockets with timeout
            let duration = (earliest_timeout - Instant::now()).max(Duration::from_millis(1));
            first_read = first_read.wrapping_add(1);

            tokio::select! {
                result = recv_any(&sockets, first_read, &mut buf) => {
                    match result {
                        Ok((n, source, destination)) => {
                            let data = &buf[..n];
                            if let Ok(contents) = data.try_into() {
                                let input = Input::Receive(
                                    Instant::now(),
                                    Receive {
                                        proto: Protocol::Udp,
                                        source,
                                        destination,
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

/// The next packet on any of `sockets`, read from `first % len` on: its
/// length, its source, and the address of the socket it arrived on (its
/// destination). Pending forever with no sockets.
async fn recv_any(
    sockets: &[(SocketAddr, std::sync::Arc<UdpSocket>)],
    first: usize,
    buf: &mut [u8],
) -> std::io::Result<(usize, SocketAddr, SocketAddr)> {
    std::future::poll_fn(|cx| {
        let count = sockets.len();
        for i in 0..count {
            let (addr, socket) = &sockets[(first + i) % count];
            let mut read = tokio::io::ReadBuf::new(buf);
            match socket.poll_recv_from(cx, &mut read) {
                std::task::Poll::Ready(Ok(source)) => {
                    return std::task::Poll::Ready(Ok((read.filled().len(), source, *addr)))
                }
                std::task::Poll::Ready(Err(e)) => return std::task::Poll::Ready(Err(e)),
                std::task::Poll::Pending => {}
            }
        }
        std::task::Poll::Pending
    })
    .await
}

/// Write one forwarded packet to `peer`'s outbound `mid`, as the media
/// relay does for every packet it decided this peer gets.
///
/// `data` carries the ORIGIN's payload type, which means nothing to this
/// peer: every side numbers its own PTs — a browser answers on its own
/// numbering (Chrome's opus is 111, Firefox's 109), so `data.pt` names
/// only what the SENDER negotiated and is only logged, never written.
/// The write goes out on the PT `peer` negotiated for the same codec on
/// `mid`: [`str0m::media::Writer::match_params`] scores the codec spec
/// (codec, clock rate, channels and format parameters such as H264's
/// profile-level-id and packetization-mode), and the
/// [`str0m::media::Writer::payload_params`] check keeps it to a PT this
/// mid actually negotiated — `match_params` scores the whole session
/// config, which can hold the codec locked for another mid.
///
/// A peer with no matching codec on `mid` gets nothing; the drop logs
/// once per (mid, codec) and a failed write once per (mid, error kind)
/// — never per packet.
fn write_forwarded(peer: &mut SfuPeer, mid: Mid, data: &MediaData, time: MediaTime) {
    let Some(writer) = peer.rtc.writer(mid) else {
        return;
    };
    let pt = writer
        .match_params(data.params)
        .filter(|&pt| writer.payload_params().any(|p| p.pt() == pt));
    let Some(pt) = pt else {
        let codec = data.params.spec().codec;
        if peer
            .drops_logged
            .insert((mid, std::mem::discriminant(&codec)))
        {
            warn!(
                "SFU: peer {} mid {} negotiated no codec matching {:?} (PT {}); \
                 dropping this track's media",
                peer.id, mid, codec, data.pt
            );
        }
        return;
    };
    if let Err(e) = writer.write(pt, data.network_time, time, data.data.clone()) {
        if peer
            .write_errors_logged
            .insert((mid, std::mem::discriminant(&e)))
        {
            warn!(
                "SFU: failed to write media to peer {} mid {}: {:?}",
                peer.id, mid, e
            );
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

/// One SDP change adding a send-only m-line per entry of `kinds`, and an ICE
/// restart when `restart`: its offer, the pending offer, and the new mids in
/// the order of `kinds`. `None` when the change holds nothing.
///
/// str0m draws each mid at random (three characters) and checks it only
/// against the m-lines the session already has, not against the others
/// added in the same change, so a batch can name two m-lines alike. The
/// client refuses such an offer ("Duplicate a=mid value") and rebuilds its
/// connection, and `tracks_out_rev` would send both origins to the one mid.
/// A batch with a repeated mid is dropped unapplied (adding media only
/// queues a change) and drawn again.
fn offer_send_only(
    rtc: &mut Rtc,
    kinds: &[MediaKind],
    restart: bool,
) -> Option<(SdpOffer, str0m::change::SdpPendingOffer, Vec<Mid>)> {
    loop {
        let mut api = rtc.sdp_api();
        if restart {
            // The SFU keeps its candidates: its socket did not move.
            api.ice_restart(true);
        }
        let mids: Vec<Mid> = kinds
            .iter()
            .map(|kind| api.add_media(*kind, str0m::media::Direction::SendOnly, None, None, None))
            .collect();
        if mids.iter().collect::<HashSet<_>>().len() == mids.len() {
            return api.apply().map(|(offer, pending)| (offer, pending, mids));
        }
        debug!("SFU: an SDP batch drew a repeated mid; drawing again");
    }
}

/// Drain deferred changes from a peer — tracks, and an ICE restart not yet
/// offered — into one renegotiation offer. Returns `(sdp_offer_json,
/// track_mapping)` or `None` when nothing actionable remains.
fn apply_deferred_tracks(peer: &mut SfuPeer) -> Option<(String, Vec<crate::sfu::TrackMapEntry>)> {
    let restart = peer
        .ice_restart
        .as_ref()
        .is_some_and(|restart| restart.offer.is_none());
    if peer.deferred_tracks.is_empty() && !restart {
        return None;
    }
    let deferred: Vec<_> = peer.deferred_tracks.drain(..).collect();

    // Filter out tracks already plumbed, and repeats: a track is queued
    // again each time this peer opens one of its own while an offer is
    // pending. Must happen before borrowing rtc via sdp_api().
    let mut queued = HashSet::new();
    let to_add: Vec<_> = deferred
        .into_iter()
        .filter(|(origin_pid, origin_mid, _, _)| {
            let track = (origin_pid.clone(), *origin_mid);
            !peer.tracks_out_rev.contains_key(&track) && queued.insert(track)
        })
        .collect();

    if to_add.is_empty() && !restart {
        return None;
    }

    let kinds: Vec<MediaKind> = to_add.iter().map(|(_, _, kind, _)| *kind).collect();
    let (offer, pending, mids) = offer_send_only(&mut peer.rtc, &kinds, restart)?;
    let mut track_mapping = Vec::with_capacity(mids.len());
    for (new_mid, (origin_pid, origin_mid, kind, origin)) in mids.into_iter().zip(to_add) {
        track_mapping.push(mapping_entry(new_mid, kind, &origin));
        peer.tracks_out
            .insert(new_mid, (origin_pid.clone(), origin_mid));
        peer.tracks_out_rev
            .insert((origin_pid, origin_mid), new_mid);
    }

    peer.pending_offer = Some(pending);
    peer.pending_offer_sent = Some(Instant::now());

    let sdp_offer_json = serde_json::to_string(&offer).ok()?;
    if let Some(restart) = peer.ice_restart.as_mut().filter(|_| restart) {
        restart.offer = Some((sdp_offer_json.clone(), track_mapping.clone()));
    }
    Some((sdp_offer_json, track_mapping))
}

/// The track mapping entry for outbound m-line `mid` carrying `origin`'s media.
fn mapping_entry(mid: Mid, kind: MediaKind, origin: &TrackOrigin) -> crate::sfu::TrackMapEntry {
    crate::sfu::TrackMapEntry {
        mid: mid.to_string(),
        participant_id: origin.participant_id.clone(),
        agent_did: origin.agent_did.clone(),
        media_kind: if kind.is_audio() { "audio" } else { "video" }.to_string(),
    }
}

/// Offer each target peer the tracks of `adds_per_target` it does not send
/// yet, `(kind, origin peer, origin mid)`: one SDP change per target, with
/// the track mapping naming the session each new m-line carries. A target
/// with an offer pending gets them deferred to after its answer. Returns
/// the offers to publish: `(target, sdp_offer_json, track_mapping)`.
fn offer_new_tracks(
    peers: &mut HashMap<ParticipantId, SfuPeer>,
    adds_per_target: HashMap<ParticipantId, Vec<(MediaKind, ParticipantId, Mid)>>,
) -> Vec<(ParticipantId, String, Vec<crate::sfu::TrackMapEntry>)> {
    // Who each inbound track carries, read before any target is borrowed.
    let origins: HashMap<(ParticipantId, Mid), TrackOrigin> = peers
        .values()
        .flat_map(|peer| {
            peer.tracks_in
                .keys()
                .map(|mid| ((peer.id.clone(), *mid), peer.track_origin(*mid)))
        })
        .collect();

    let mut offers = Vec::new();
    // One sdp_api batch per target peer so a single offer carries every new
    // outbound m-line (str0m permits exactly one in-flight change at a time).
    for (target_pid, additions) in adds_per_target.into_iter() {
        let Some(target_peer) = peers.get_mut(&target_pid) else {
            continue;
        };
        if target_peer.pending_offer.is_some() {
            for (kind, origin_pid, origin_mid) in &additions {
                let track = (origin_pid.clone(), *origin_mid);
                if target_peer.tracks_out_rev.contains_key(&track) {
                    continue;
                }
                if let Some(origin) = origins.get(&track) {
                    target_peer.deferred_tracks.push((
                        origin_pid.clone(),
                        *origin_mid,
                        *kind,
                        origin.clone(),
                    ));
                }
            }
            continue;
        }
        // Skip tracks already forwarded, and tracks no peer receives (any
        // more): the same tests the deferred path makes above, so a track
        // goes out with its mapping entry on both paths or on neither.
        let to_add: Vec<(MediaKind, ParticipantId, Mid, &TrackOrigin)> = additions
            .into_iter()
            .filter_map(|(kind, origin_pid, origin_mid)| {
                let track = (origin_pid.clone(), origin_mid);
                if target_peer.tracks_out_rev.contains_key(&track) {
                    return None;
                }
                let origin = origins.get(&track)?;
                Some((kind, origin_pid, origin_mid, origin))
            })
            .collect();
        let kinds: Vec<MediaKind> = to_add.iter().map(|(kind, _, _, _)| *kind).collect();
        let Some((offer, pending, mids)) = offer_send_only(&mut target_peer.rtc, &kinds, false)
        else {
            continue;
        };

        let mut track_mapping = Vec::with_capacity(mids.len());
        for (new_mid, (kind, origin_pid, origin_mid, origin)) in mids.into_iter().zip(to_add) {
            track_mapping.push(mapping_entry(new_mid, kind, origin));
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
            kinds.len()
        );

        match serde_json::to_string(&offer) {
            Ok(sdp_offer_json) => offers.push((target_pid, sdp_offer_json, track_mapping)),
            Err(e) => warn!(
                "SFU: serialise renegotiation offer for {} failed: {}",
                target_pid, e
            ),
        }
    }
    offers
}

/// The agents that get a data channel message the session `sender` sent in `room`: the agent
/// of every other client peer in that room on this executor. The sender's own agent is in it
/// only when it is in the call on another device too; the sending session drops its own
/// message by `senderParticipantId`. Pipe peers stand for other SFU nodes, not agents, so
/// none of them is in it.
fn data_recipients(
    peers: &HashMap<ParticipantId, SfuPeer>,
    room: &RoomId,
    sender: &ParticipantId,
) -> Vec<String> {
    let mut dids: Vec<String> = peers
        .values()
        .filter(|p| !p.is_pipe && p.room_id == *room && p.id != *sender)
        .map(|p| p.agent_did.clone())
        .collect();
    dids.sort();
    dids.dedup();
    dids
}

/// Publish a renegotiation offer. A client peer gets an
/// `sfu-call-renegotiation-offer` event; a pipe transport's offer goes to the
/// cascade router through the pipe bus. Both carry the track mapping.
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
            track_mapping,
        };
        super::pipe_bus::publish_offer(payload);
    } else {
        let payload = crate::sfu::SfuCallRenegotiationOffer {
            target_did: peer.agent_did.clone(),
            participant_id: pid.to_string(),
            neighbourhood_url: peer.room_id.neighbourhood_url.clone(),
            room_name: peer.room_id.room_name.clone(),
            sdp_offer: sdp_offer_json,
            track_mapping,
        };
        crate::pubsub::get_global_pubsub_sync()
            .publish_event(crate::events::Event::sfu_renegotiation_offer(&payload));
    }
    info!("SFU: renegotiation offer published for peer {}", pid);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A browser's offer: one audio section, as a caller sends to join.
    fn caller_offer() -> SdpOffer {
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        change.apply().unwrap().0
    }

    /// The `a=candidate` lines of an answer as `create_rtc_for_offer` returns it.
    fn candidates(answer_json: &str) -> Vec<String> {
        let answer: serde_json::Value = serde_json::from_str(answer_json).unwrap();
        answer["sdp"]
            .as_str()
            .unwrap()
            .lines()
            .filter_map(|l| l.strip_prefix("a=candidate:"))
            .map(str::to_string)
            .collect()
    }

    #[test]
    fn a_batch_of_new_tracks_never_repeats_a_mid() {
        // str0m's mids are three random characters, unchecked within one
        // change: a 600-line batch repeats one about half the time, so 20
        // batches all but surely draw a repeat unless the batch is redrawn.
        let kinds: Vec<MediaKind> = (0..600)
            .map(|i| {
                if i % 2 == 0 {
                    MediaKind::Audio
                } else {
                    MediaKind::Video
                }
            })
            .collect();
        for _ in 0..20 {
            let mut rtc = Rtc::new(Instant::now());
            let (offer, _pending, mids) =
                offer_send_only(&mut rtc, &kinds, false).expect("an offer for a non-empty batch");
            assert_eq!(mids.len(), kinds.len());
            let distinct: HashSet<&Mid> = mids.iter().collect();
            assert_eq!(distinct.len(), mids.len(), "a mid repeats within the batch");
            let sdp = offer.to_sdp_string();
            let offered: Vec<&str> = sdp
                .lines()
                .filter_map(|l| l.strip_prefix("a=mid:"))
                .collect();
            let offered_distinct: HashSet<&str> = offered.iter().copied().collect();
            assert_eq!(offered.len(), kinds.len());
            assert_eq!(
                offered_distinct.len(),
                offered.len(),
                "the offer repeats an a=mid"
            );
            // The mids come back in the order of `kinds`, so the caller's
            // track mapping names the right m-line.
            for (mid, kind) in mids.iter().zip(&kinds) {
                let section = sdp
                    .split("\r\nm=")
                    .find(|section| section.contains(&format!("a=mid:{}\r\n", mid)))
                    .expect("the mid's m-line");
                assert!(section.starts_with(if kind.is_audio() { "audio" } else { "video" }));
            }
        }
        assert!(offer_send_only(&mut Rtc::new(Instant::now()), &[], false).is_none());
    }

    /// A track queued twice while an offer was pending (a peer that opens
    /// a second track has every other peer's tracks queued for it again)
    /// goes out on one m-line, not one per time it was queued.
    #[test]
    fn a_track_queued_twice_is_offered_once() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let (rtc, _) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:target".to_string(),
            rtc,
            false,
        );
        let origin = ParticipantId::next();
        let origin_mid = Mid::from("0");
        for _ in 0..2 {
            peer.deferred_tracks.push((
                origin.clone(),
                origin_mid,
                MediaKind::Audio,
                TrackOrigin {
                    participant_id: origin.to_string(),
                    agent_did: "did:key:origin".to_string(),
                },
            ));
        }
        let (_, mapping) = apply_deferred_tracks(&mut peer).expect("the track is offered");
        assert_eq!(mapping.len(), 1, "{mapping:?}");
        assert_eq!(peer.tracks_out.len(), 1);
    }

    fn mapped(mid: &str, participant_id: &str, agent_did: &str) -> crate::sfu::TrackMapEntry {
        crate::sfu::TrackMapEntry {
            mid: mid.to_string(),
            participant_id: participant_id.to_string(),
            agent_did: agent_did.to_string(),
            media_kind: "audio".to_string(),
        }
    }

    /// A track that arrives through a cascade pipe is offered to a client
    /// as the remote session's, the one the remote node's offer named, not
    /// as the pipe's: its id qualified by the node, its agent's DID. A mid
    /// no offer named (a node on an older build sends no mapping) is offered
    /// as the pipe's: its id, the remote node's DID. Deferred while an offer
    /// is pending, either keeps its origin.
    #[test]
    fn a_track_from_a_pipe_is_offered_as_the_remote_session() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let room = RoomId::new("neighbourhood://test", "room");
        let (rtc, _) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let client_id = ParticipantId::next();
        let client = SfuPeer::new(
            client_id.clone(),
            room.clone(),
            "did:key:bob".to_string(),
            rtc,
            false,
        );
        let pipe_id = ParticipantId::next();
        let mut pipe = SfuPeer::new(
            pipe_id.clone(),
            room.clone(),
            "did:key:nodeA".to_string(),
            Rtc::new(Instant::now()),
            true,
        );
        let (named, unnamed, later, later_unnamed) = (
            Mid::from("7"),
            Mid::from("9"),
            Mid::from("8"),
            Mid::from("6"),
        );
        pipe.note_remote_origins(&[
            mapped("7", "P1", "did:key:alice"),
            mapped("8", "P2", "did:key:carol"),
        ]);
        for mid in [named, unnamed, later, later_unnamed] {
            pipe.tracks_in.insert(mid, MediaKind::Audio);
        }
        let mut peers = HashMap::from([(client_id.clone(), client), (pipe_id.clone(), pipe)]);
        let as_pipe = TrackOrigin {
            participant_id: pipe_id.to_string(),
            agent_did: "did:key:nodeA".to_string(),
        };

        let offers = offer_new_tracks(
            &mut peers,
            HashMap::from([(
                client_id.clone(),
                vec![
                    (MediaKind::Audio, pipe_id.clone(), named),
                    (MediaKind::Audio, pipe_id.clone(), unnamed),
                ],
            )]),
        );
        assert_eq!(offers.len(), 1);
        let (target, _, mapping) = &offers[0];
        assert_eq!(target, &client_id);
        assert_eq!(mapping.len(), 2, "{mapping:?}");
        let by_mid = |origin_mid: Mid| {
            let out = peers[&client_id].tracks_out_rev[&(pipe_id.clone(), origin_mid)];
            mapping
                .iter()
                .find(|e| e.mid == out.to_string())
                .map(|e| (e.participant_id.clone(), e.agent_did.clone()))
                .expect("a mapping entry for the track")
        };
        assert_eq!(
            by_mid(named),
            ("P1@did:key:nodeA".to_string(), "did:key:alice".to_string())
        );
        assert_eq!(
            by_mid(unnamed),
            (as_pipe.participant_id.clone(), as_pipe.agent_did.clone())
        );
        assert_eq!(peers[&client_id].tracks_out.len(), 2);

        // The client has not answered yet: the next tracks wait, and keep
        // their origins, named or not.
        let offers = offer_new_tracks(
            &mut peers,
            HashMap::from([(
                client_id.clone(),
                vec![
                    (MediaKind::Audio, pipe_id.clone(), later),
                    (MediaKind::Audio, pipe_id.clone(), later_unnamed),
                ],
            )]),
        );
        assert!(offers.is_empty());
        let deferred: Vec<(Mid, TrackOrigin)> = peers[&client_id]
            .deferred_tracks
            .iter()
            .map(|(_, mid, _, origin)| (*mid, origin.clone()))
            .collect();
        assert_eq!(
            deferred,
            vec![
                (
                    later,
                    TrackOrigin {
                        participant_id: "P2@did:key:nodeA".to_string(),
                        agent_did: "did:key:carol".to_string(),
                    }
                ),
                (later_unnamed, as_pipe.clone()),
            ]
        );

        // Once the offer is answered, the deferred tracks go out with the
        // same mapping the direct path gives them.
        let client = peers.get_mut(&client_id).unwrap();
        client.pending_offer = None;
        let (_, mapping) = apply_deferred_tracks(client).expect("the deferred tracks are offered");
        let mut named_in_mapping: Vec<(String, String)> = mapping
            .iter()
            .map(|e| (e.participant_id.clone(), e.agent_did.clone()))
            .collect();
        named_in_mapping.sort();
        let mut expected = vec![
            ("P2@did:key:nodeA".to_string(), "did:key:carol".to_string()),
            (as_pipe.participant_id, as_pipe.agent_did),
        ];
        expected.sort();
        assert_eq!(named_in_mapping, expected);
    }

    /// A client's own tracks are offered as its session's.
    #[test]
    fn a_track_from_a_client_is_offered_as_its_session() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let room = RoomId::new("neighbourhood://test", "room");
        let (rtc, _) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let target_id = ParticipantId::next();
        let target = SfuPeer::new(
            target_id.clone(),
            room.clone(),
            "did:key:bob".to_string(),
            rtc,
            false,
        );
        let sender_id = ParticipantId::next();
        let mut sender = SfuPeer::new(
            sender_id.clone(),
            room,
            "did:key:alice".to_string(),
            Rtc::new(Instant::now()),
            false,
        );
        let mid = Mid::from("0");
        sender.tracks_in.insert(mid, MediaKind::Audio);
        let mut peers = HashMap::from([(target_id.clone(), target), (sender_id.clone(), sender)]);
        let offers = offer_new_tracks(
            &mut peers,
            HashMap::from([(target_id, vec![(MediaKind::Audio, sender_id.clone(), mid)])]),
        );
        assert_eq!(offers.len(), 1);
        let mapping = &offers[0].2;
        assert_eq!(mapping.len(), 1, "{mapping:?}");
        assert_eq!(mapping[0].participant_id, sender_id.to_string());
        assert_eq!(mapping[0].agent_did, "did:key:alice");
        assert_eq!(mapping[0].media_kind, "audio");
    }

    /// Behind NAT the answer offers the bound socket and the public address
    /// on the same, fixed port: a srflx candidate whose base is the socket.
    #[test]
    fn the_answer_offers_the_host_and_the_advertised_address() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let public: SocketAddr = "198.51.100.7:40000".parse().unwrap();
        let (_, answer) =
            SfuServer::create_rtc_for_offer(caller_offer(), &[local], Some(public)).unwrap();
        let lines = candidates(&answer);
        assert_eq!(lines.len(), 2, "{lines:?}");
        assert!(
            lines
                .iter()
                .any(|l| l.contains("192.168.0.10 40000 typ host")),
            "{lines:?}"
        );
        // str0m does not reveal the base: `raddr 0.0.0.0 rport 0`.
        assert!(
            lines
                .iter()
                .any(|l| l.contains("198.51.100.7 40000 typ srflx")),
            "{lines:?}"
        );
    }

    #[test]
    fn without_an_advertised_address_the_answer_offers_the_host_only() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let (_, answer) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let lines = candidates(&answer);
        assert_eq!(lines.len(), 1, "{lines:?}");
        assert!(lines[0].contains("typ host"), "{lines:?}");
    }

    /// What one side of an in-memory call did, on the simulated clock.
    #[derive(Default)]
    struct Seen {
        ice: Vec<(Duration, IceConnectionState)>,
        data: Vec<Duration>,
        channel: Option<ChannelId>,
        media: Vec<MediaData>,
    }

    /// Feed `rtc` the clock, then drain it: events go to `seen`, packets
    /// are returned for the network to route. Returns the next deadline.
    fn drive(
        rtc: &mut Rtc,
        now: Instant,
        start: Instant,
        seen: &mut Seen,
        sent: &mut Vec<(SocketAddr, SocketAddr, Vec<u8>)>,
    ) -> Instant {
        rtc.handle_input(Input::Timeout(now)).unwrap();
        loop {
            match rtc.poll_output().unwrap() {
                Output::Timeout(t) => return t,
                Output::Transmit(t) => sent.push((t.source, t.destination, t.contents.to_vec())),
                Output::Event(Event::IceConnectionStateChange(s)) => {
                    seen.ice.push((now - start, s))
                }
                Output::Event(Event::ChannelOpen(id, _)) => seen.channel = Some(id),
                Output::Event(Event::ChannelData(_)) => seen.data.push(now - start),
                Output::Event(Event::MediaData(d)) => seen.media.push(d),
                Output::Event(_) => {}
            }
        }
    }

    fn receive(
        rtc: &mut Rtc,
        now: Instant,
        source: SocketAddr,
        destination: SocketAddr,
        data: &[u8],
    ) {
        rtc.handle_input(Input::Receive(
            now,
            Receive {
                proto: Protocol::Udp,
                source,
                destination,
                contents: data.try_into().unwrap(),
            },
        ))
        .unwrap();
    }

    /// A STUN Binding request (RFC 5389: message type 0x0001).
    fn is_binding_request(packet: &[u8]) -> bool {
        packet.len() >= 20 && packet[0] == 0x00 && packet[1] == 0x01
    }

    /// The production failure (Chrome/Edge 144+, str0m issue #846): a caller
    /// whose own checks reach the SFU through the router's port forward, but
    /// which never answers the SFU's checks. The SFU must keep that caller as
    /// long as the caller's checks keep arriving. str0m before 0.16.1 prunes
    /// the pair, nominated or not, once its own checks time out (~18.75 s),
    /// and goes Disconnected.
    ///
    /// The network: the SFU binds 192.168.0.10:40000 behind a router that
    /// forwards 198.51.100.7:40000 to it; the caller cannot reach the
    /// private address. Every binding request the SFU sends is lost;
    /// responses, DTLS, SCTP and media pass.
    #[test]
    fn a_caller_whose_network_drops_the_sfus_checks_stays_connected() {
        let bind: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let public: SocketAddr = "198.51.100.7:40000".parse().unwrap();
        let caller_addr: SocketAddr = "203.0.113.20:50000".parse().unwrap();

        let mut caller = Rtc::builder().build(Instant::now());
        caller.add_local_candidate(Candidate::host(caller_addr, "udp").unwrap());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let cid = change.add_channel("sfu-data".into());
        let (offer, pending) = change.apply().unwrap();
        let (mut sfu, answer) =
            SfuServer::create_rtc_for_offer(offer, &[bind], Some(public)).unwrap();
        let answer: str0m::change::SdpAnswer = serde_json::from_str(&answer).unwrap();
        caller.sdp_api().accept_answer(pending, answer).unwrap();

        let start = Instant::now();
        let mut now = start;
        let (mut caller_seen, mut sfu_seen) = (Seen::default(), Seen::default());
        let mut next_write = Duration::ZERO;
        let mut sfu_checks_dropped = 0;
        let end = Duration::from_secs(60);
        while now - start < end {
            let mut sent = Vec::new();
            let caller_next = drive(&mut caller, now, start, &mut caller_seen, &mut sent);
            for (_, destination, data) in sent.drain(..) {
                // The router forwards the public address to the bind
                // address; the private address is unreachable.
                if destination == public {
                    receive(&mut sfu, now, caller_addr, bind, &data);
                }
            }
            let sfu_next = drive(&mut sfu, now, start, &mut sfu_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                assert_eq!(source, bind, "the SFU sends from its socket");
                assert_eq!(destination, caller_addr);
                if is_binding_request(&data) {
                    sfu_checks_dropped += 1;
                    continue;
                }
                // The router rewrites the source to the public address.
                receive(&mut caller, now, public, caller_addr, &data);
            }

            // Both sides talk on the data channel twice a second once open.
            if now - start >= next_write {
                if let Some(mut ch) = caller.channel(cid) {
                    ch.write(true, b"caller").unwrap();
                }
                if let Some(mut ch) = sfu_seen.channel.and_then(|id| sfu.channel(id)) {
                    ch.write(true, b"sfu").unwrap();
                }
                next_write += Duration::from_millis(500);
            }

            let next = caller_next.min(sfu_next).min(start + next_write);
            now = next.clamp(
                now + Duration::from_millis(1),
                now + Duration::from_millis(50),
            );
        }

        // What counts is losing the caller once connected.
        let up = sfu_seen
            .ice
            .iter()
            .position(|(_, s)| {
                matches!(
                    s,
                    IceConnectionState::Connected | IceConnectionState::Completed
                )
            })
            .unwrap_or_else(|| panic!("the SFU never connected: {:?}", sfu_seen.ice));
        assert!(
            sfu_seen.ice[up].0 < Duration::from_secs(1),
            "{:?}",
            sfu_seen.ice
        );
        let disconnected = sfu_seen.ice[up..]
            .iter()
            .find(|(_, s)| *s == IceConnectionState::Disconnected);
        assert!(
            disconnected.is_none(),
            "the SFU dropped a caller whose checks keep arriving: {:?} (states {:?}, \
             {sfu_checks_dropped} SFU binding requests dropped)",
            disconnected,
            sfu_seen.ice
        );
        let recent = end - Duration::from_secs(5);
        assert!(
            sfu_seen.data.last().is_some_and(|t| *t >= recent),
            "the SFU stopped receiving at {:?}",
            sfu_seen.data.last()
        );
        assert!(
            caller_seen.data.last().is_some_and(|t| *t >= recent),
            "the caller stopped receiving at {:?}",
            caller_seen.data.last()
        );
    }

    /// When the caller asks for an ICE restart after its network changed.
    #[derive(Clone, Copy)]
    enum Restart {
        Never,
        /// This long after the move, as a client that hears of the change does.
        AfterMove(Duration),
        /// This long after the SFU saw the caller disconnected: a client
        /// that noticed late, with slow signalling.
        AfterSfuDisconnected(Duration),
    }

    /// What happened to the SFU's peer, on the simulated clock.
    struct NetworkChange {
        dropped_at: Option<Duration>,
        restarted_at: Option<Duration>,
        sfu_seen: Seen,
        caller_seen: Seen,
    }

    /// A caller on 203.0.113.20 moves to 203.0.113.30 at `MOVE` (a Wi-Fi
    /// switch): from then on everything to or from the old address is lost.
    /// The SFU's peer is driven as the event loop drives it: ICE states
    /// through `on_ice_state`, the drop through `stayed_disconnected`. A
    /// restart goes through `restart_ice` and the post-poll sweep
    /// (`apply_deferred_tracks`); the caller applies the offer
    /// with its new address as a candidate, as a browser gathers one after
    /// a restart, and the SFU applies the answer, each `signalling` later.
    fn network_change(restart: Restart, signalling: Duration) -> NetworkChange {
        const MOVE: Duration = Duration::from_secs(5);
        let bind: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let old_addr: SocketAddr = "203.0.113.20:50000".parse().unwrap();
        let new_addr: SocketAddr = "203.0.113.30:50000".parse().unwrap();

        let mut caller = Rtc::builder().build(Instant::now());
        caller.add_local_candidate(Candidate::host(old_addr, "udp").unwrap());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let cid = change.add_channel("sfu-data".into());
        let (offer, pending) = change.apply().unwrap();
        let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &[bind], None).unwrap();
        let answer: str0m::change::SdpAnswer = serde_json::from_str(&answer).unwrap();
        caller.sdp_api().accept_answer(pending, answer).unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );

        let start = Instant::now();
        let mut now = start;
        let (mut caller_seen, mut sfu_seen) = (Seen::default(), Seen::default());
        let mut next_write = Duration::ZERO;
        let mut restarted_at = None;
        let mut dropped_at = None;
        // Signalling in flight: the offer on its way to the caller, the
        // answer on its way back.
        let mut offer_due: Option<(Duration, SdpOffer)> = None;
        let mut answer_due: Option<(Duration, String)> = None;
        let end = Duration::from_secs(60);
        while now - start < end {
            let t = now - start;
            let caller_addr = if t < MOVE { old_addr } else { new_addr };

            let due = match restart {
                Restart::Never => false,
                Restart::AfterMove(d) => t >= MOVE + d,
                Restart::AfterSfuDisconnected(d) => {
                    peer.disconnected_since.is_some_and(|s| now >= s + d)
                }
            };
            if due && restarted_at.is_none() {
                restarted_at = Some(t);
                assert!(peer.restart_ice(now).is_none(), "offered after the poll");
                let (json, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");
                offer_due = Some((t + signalling, serde_json::from_str(&json).unwrap()));
            }
            if offer_due.as_ref().is_some_and(|(at, _)| t >= *at) {
                let (_, offer) = offer_due.take().unwrap();
                caller.add_local_candidate(Candidate::host(new_addr, "udp").unwrap());
                let answer = caller.sdp_api().accept_offer(offer).unwrap();
                answer_due = Some((t + signalling, serde_json::to_string(&answer).unwrap()));
            }
            if answer_due.as_ref().is_some_and(|(at, _)| t >= *at) {
                let (_, answer) = answer_due.take().unwrap();
                peer.accept_server_answer(&answer).unwrap();
            }

            let mut sent = Vec::new();
            let caller_next = drive(&mut caller, now, start, &mut caller_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                // Only the address the caller has now still routes.
                if source == caller_addr && destination == bind {
                    receive(&mut peer.rtc, now, source, bind, &data);
                }
            }
            let states = sfu_seen.ice.len();
            let sfu_next = drive(&mut peer.rtc, now, start, &mut sfu_seen, &mut sent);
            for (_, state) in sfu_seen.ice[states..].to_vec() {
                peer.on_ice_state(state, now);
            }
            if peer.stayed_disconnected(now) {
                peer.rtc.disconnect();
                dropped_at = Some(t);
                break;
            }
            for (_, destination, data) in sent.drain(..) {
                if destination == caller_addr {
                    receive(&mut caller, now, bind, destination, &data);
                }
            }

            if t >= next_write {
                if let Some(mut ch) = caller.channel(cid) {
                    ch.write(true, b"caller").unwrap();
                }
                if let Some(mut ch) = sfu_seen.channel.and_then(|id| peer.rtc.channel(id)) {
                    ch.write(true, b"sfu").unwrap();
                }
                next_write += Duration::from_millis(500);
            }

            let next = caller_next.min(sfu_next).min(start + next_write);
            now = next.clamp(
                now + Duration::from_millis(1),
                now + Duration::from_millis(50),
            );
        }
        NetworkChange {
            dropped_at,
            restarted_at,
            sfu_seen,
            caller_seen,
        }
    }

    /// The first data each side received at or after `from`.
    fn resumed(seen: &Seen, from: Duration) -> Option<Duration> {
        seen.data.iter().copied().find(|t| *t >= from)
    }

    /// Without a restart, a caller that moved is lost: the SFU drops it once
    /// it stayed disconnected for the grace.
    #[test]
    fn a_caller_that_moved_without_an_ice_restart_is_dropped() {
        let run = network_change(Restart::Never, Duration::from_millis(100));
        let dropped = run
            .dropped_at
            .unwrap_or_else(|| panic!("never dropped: {:?}", run.sfu_seen.ice));
        assert!(
            dropped > Duration::from_secs(5) + ICE_DISCONNECT_GRACE,
            "{dropped:?}"
        );
    }

    /// An ICE restart right after the move brings the caller back on its new
    /// address within a second or two, on the same connection: the data
    /// channel carries on both ways and the SFU never drops the peer.
    #[test]
    fn an_ice_restart_after_a_move_resumes_within_two_seconds() {
        let run = network_change(
            Restart::AfterMove(Duration::from_millis(200)),
            Duration::from_millis(100),
        );
        assert_eq!(run.dropped_at, None, "{:?}", run.sfu_seen.ice);
        let restarted = run.restarted_at.unwrap();
        for (side, seen) in [("SFU", &run.sfu_seen), ("caller", &run.caller_seen)] {
            let back = resumed(seen, restarted)
                .unwrap_or_else(|| panic!("the {side} never heard again: {:?}", run.sfu_seen.ice));
            assert!(
                back - restarted < Duration::from_secs(2),
                "the {side} heard again {:?} after the restart",
                back - restarted
            );
            assert!(
                seen.data.last().unwrap() >= &Duration::from_secs(55),
                "the {side} stopped receiving at {:?}",
                seen.data.last()
            );
        }
    }

    /// A restart asked for late, when the SFU has seen the caller
    /// disconnected for almost the whole grace, and answered slowly: the
    /// peer is kept past the grace until the restart can work, and ICE
    /// connects again on the new address. (The data channel does not come
    /// back here: after half a minute without an answer SCTP has given up
    /// on the association. Media, which has no such retry limit, would.)
    #[test]
    fn a_late_ice_restart_extends_the_disconnect_grace() {
        let run = network_change(
            Restart::AfterSfuDisconnected(Duration::from_millis(9_500)),
            Duration::from_millis(500),
        );
        assert!(run.restarted_at.is_some(), "{:?}", run.sfu_seen.ice);
        assert_eq!(run.dropped_at, None, "{:?}", run.sfu_seen.ice);
        let restarted = run.restarted_at.unwrap();
        let back = run
            .sfu_seen
            .ice
            .iter()
            .find(|(t, s)| *t > restarted && *s == IceConnectionState::Connected)
            .map(|(t, _)| *t)
            .unwrap_or_else(|| panic!("never connected again: {:?}", run.sfu_seen.ice));
        assert!(
            back - restarted < Duration::from_secs(3),
            "{:?}",
            run.sfu_seen.ice
        );
    }

    /// The restart waits behind an offer already pending, and a second
    /// request while its offer waits gets that same offer.
    #[test]
    fn an_ice_restart_waits_for_a_pending_offer_and_is_offered_once() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let (rtc, _) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        let mut change = peer.rtc.sdp_api();
        change.add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendOnly,
            None,
            None,
            None,
        );
        let (_, pending) = change.apply().unwrap();
        peer.pending_offer = Some(pending);

        let now = Instant::now();
        assert!(peer.restart_ice(now).is_none(), "an offer is pending");
        assert!(peer.has_deferred_changes());

        // The pending offer goes stale; the restart is offered next.
        peer.drop_stale_offer();
        let (offer, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");
        assert!(offer.contains("ice-ufrag"), "{offer}");
        assert!(!peer.has_deferred_changes());
        assert_eq!(peer.restart_ice(now).map(|(o, _)| o), Some(offer));
    }

    /// A second request while the restart's offer waits gets that offer
    /// again (its answer may have been lost), and once the answer lands a
    /// fresh restart follows, with new ICE credentials for the newest
    /// network.
    #[test]
    fn a_second_request_during_a_restart_restarts_again_after_its_answer() {
        let bind: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let (offer, pending) = change.apply().unwrap();
        let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &[bind], None).unwrap();
        caller
            .sdp_api()
            .accept_answer(pending, serde_json::from_str(&answer).unwrap())
            .unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        let ufrag = |offer: &str| {
            let offer: SdpOffer = serde_json::from_str(offer).unwrap();
            offer
                .to_sdp_string()
                .lines()
                .find_map(|l| l.strip_prefix("a=ice-ufrag:").map(str::to_string))
                .unwrap()
        };

        let now = Instant::now();
        assert!(peer.restart_ice(now).is_none(), "offered after the poll");
        let (first, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");
        assert_eq!(peer.restart_ice(now).map(|(o, _)| o), Some(first.clone()));
        assert!(!peer.has_deferred_changes());

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&first).unwrap())
            .unwrap();
        peer.accept_server_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(peer.has_deferred_changes(), "the second request is pending");
        let (second, _) = apply_deferred_tracks(&mut peer).expect("a fresh restart");
        assert_ne!(ufrag(&second), ufrag(&first));

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&second).unwrap())
            .unwrap();
        peer.accept_server_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(peer.ice_restart.is_none());
        assert!(!peer.has_deferred_changes());
    }

    /// The event loop applies commands, then drops peers that stayed
    /// disconnected, and only then polls str0m for the ICE state the answer
    /// starts. A restart answered after the disconnect grace ran out must
    /// not be dropped in between: its answer restarts the clock.
    #[test]
    fn a_restart_answered_late_keeps_the_peer_until_ice_reports() {
        let bind: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let (offer, pending) = change.apply().unwrap();
        let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &[bind], None).unwrap();
        caller
            .sdp_api()
            .accept_answer(pending, serde_json::from_str(&answer).unwrap())
            .unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        let now = Instant::now();
        let lost_at = now
            .checked_sub(ICE_DISCONNECT_GRACE + Duration::from_secs(5))
            .unwrap();
        peer.on_ice_state(IceConnectionState::Disconnected, lost_at);
        assert!(peer.restart_ice(lost_at + ICE_DISCONNECT_GRACE).is_none());
        let (offer, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");
        assert!(
            !peer.stayed_disconnected(now),
            "the restart extends the grace"
        );

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&offer).unwrap())
            .unwrap();
        peer.accept_server_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(
            !peer.stayed_disconnected(Instant::now()),
            "dropped between the answer and its ICE state"
        );
    }

    /// Candidates the caller trickles while the restart's offer waits for its
    /// answer are held, up to a cap, and handed over once the answer lands:
    /// the restart would discard any added before it.
    #[test]
    fn candidates_trickled_during_a_restart_are_held_until_its_answer() {
        let bind: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let (offer, pending) = change.apply().unwrap();
        let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &[bind], None).unwrap();
        caller
            .sdp_api()
            .accept_answer(pending, serde_json::from_str(&answer).unwrap())
            .unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        let candidate = |port: u16| {
            Candidate::host(
                SocketAddr::new("203.0.113.30".parse().unwrap(), port),
                "udp",
            )
            .unwrap()
        };

        let now = Instant::now();
        assert!(peer.restart_ice(now).is_none());
        // Before the offer exists nothing is held: the restart has not begun.
        peer.add_remote_candidate(candidate(49_999));
        assert!(peer.ice_restart.as_ref().unwrap().candidates.is_empty());

        let (offer, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");
        for port in 0..(MAX_HELD_CANDIDATES as u16 + 5) {
            peer.add_remote_candidate(candidate(50_000 + port));
        }
        let held = &peer.ice_restart.as_ref().unwrap().candidates;
        assert_eq!(held.len(), MAX_HELD_CANDIDATES);

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&offer).unwrap())
            .unwrap();
        peer.accept_server_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(peer.ice_restart.is_none(), "handed over with the answer");
    }

    /// A restart whose offer went stale is forgotten with it: the next
    /// request makes a fresh offer instead of republishing one the SFU no
    /// longer waits on, and candidates go to str0m again.
    #[test]
    fn a_stale_restart_offer_ends_the_restart() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let (rtc, _) = SfuServer::create_rtc_for_offer(caller_offer(), &[local], None).unwrap();
        let mut peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        let now = Instant::now();
        assert!(peer.restart_ice(now).is_none());
        let (first, _) = apply_deferred_tracks(&mut peer).expect("the restart is offered");

        peer.drop_stale_offer();
        assert!(peer.ice_restart.is_none());
        assert!(
            peer.restart_ice(now).is_none(),
            "a new restart, not the stale offer"
        );
        let (second, _) = apply_deferred_tracks(&mut peer).expect("offered afresh");
        assert_ne!(second, first);
    }

    /// A fixed port is bound exactly; the STUN probe runs on that socket and
    /// the public address is advertised on it.
    #[tokio::test]
    async fn a_fixed_port_is_bound_and_advertised_with_the_probes_ip() {
        // Loopback is never probed, and a public address is not behind NAT:
        // this needs a private, non-loopback interface.
        let Ok(ip) = super::super::detect_outbound_ip() else {
            return;
        };
        if ip.is_loopback() || !crate::sfu::reachability::is_private_ip(ip) {
            return;
        }
        let stun = UdpSocket::bind(SocketAddr::new(ip, 0)).await.unwrap();
        let stun_addr = stun.local_addr().unwrap();
        tokio::spawn(async move {
            let mut buf = [0u8; 64];
            let (_, from) = stun.recv_from(&mut buf).await.unwrap();
            let mapped: SocketAddr = "198.51.100.7:61111".parse().unwrap();
            let resp = crate::sfu::reachability::tests::binding_response(&buf[8..20], mapped);
            stun.send_to(&resp, from).await.unwrap();
        });
        let port = UdpSocket::bind(SocketAddr::new(ip, 0))
            .await
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let server = SfuServer::start(SfuServerConfig {
            bind_addr: SocketAddr::new(ip, port),
            stun_server: Some(stun_addr.to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
        assert_eq!(server.local_addr, SocketAddr::new(ip, port));
        let advertised = server
            .advertised
            .expect("behind NAT, something is advertised");
        assert_eq!(
            advertised.addr,
            format!("198.51.100.7:{port}")
                .parse::<SocketAddr>()
                .unwrap()
        );
        assert!(server.reachability.is_public(), "{}", server.reachability);
    }

    #[tokio::test]
    async fn a_taken_fixed_port_fails_with_a_message_that_names_it() {
        let taken = UdpSocket::bind("127.0.0.1:0").await.unwrap();
        let port = taken.local_addr().unwrap().port();
        let err = SfuServer::start(SfuServerConfig {
            bind_addr: SocketAddr::new(std::net::Ipv4Addr::LOCALHOST.into(), port),
            ..Default::default()
        })
        .await
        .err()
        .expect("the port is taken");
        let message = err.to_string();
        assert!(
            message.contains(&format!("SFU UDP port {port}")),
            "{message}"
        );
        assert!(message.contains("--sfu-udp-port"), "{message}");
    }

    #[test]
    fn the_media_settings_refuse_what_no_caller_could_use() {
        let ok = |b: Option<&str>, port: Option<u16>, public: Option<&str>| {
            sfu_media_addrs(b, port, public)
        };
        assert_eq!(
            ok(Some("192.168.0.10"), Some(40000), Some("198.51.100.7")).unwrap(),
            (
                "192.168.0.10:40000".parse().unwrap(),
                Some("198.51.100.7".parse().unwrap())
            )
        );
        // No bind address: the LAN default, on the fixed port when one is
        // given, with the public address advertised beside it.
        let lan_default: std::net::IpAddr = std::net::Ipv4Addr::UNSPECIFIED.into();
        assert_eq!(
            ok(None, None, None).unwrap(),
            (SocketAddr::new(lan_default, 0), None)
        );
        assert_eq!(
            ok(None, Some(40000), Some("198.51.100.7")).unwrap(),
            (
                SocketAddr::new(lan_default, 40000),
                Some("198.51.100.7".parse().unwrap())
            )
        );
        // No fixed port and no public address: a bad bind address still
        // falls back to loopback, as before.
        assert_eq!(
            ok(Some("lan"), None, None).unwrap().0.ip(),
            std::net::IpAddr::from(std::net::Ipv4Addr::LOCALHOST)
        );
        for (bind, port, public, says) in [
            (Some("lan"), Some(40000), None, "sfu_bind_addr"),
            (Some("0.0.0.0"), Some(40000), None, "sfu_bind_addr"),
            (
                Some("192.168.0.10"),
                None,
                Some("198.51.100.7:40000"),
                "without a port",
            ),
            (
                Some("192.168.0.10"),
                None,
                Some("0.0.0.0"),
                "cannot be offered",
            ),
            (
                Some("192.168.0.10"),
                None,
                Some("224.0.0.1"),
                "cannot be offered",
            ),
            (
                Some("192.168.0.10"),
                None,
                Some("169.254.1.1"),
                "cannot be offered",
            ),
            (
                Some("192.168.0.10"),
                None,
                Some("2001:db8::1"),
                "different IP families",
            ),
            (
                Some("fd00::10"),
                None,
                Some("198.51.100.7"),
                "different IP families",
            ),
            (
                Some("192.168.0.10"),
                None,
                Some("198.51.100.7"),
                "needs a fixed sfu_udp_port",
            ),
            (
                Some("192.168.0.10"),
                Some(0),
                Some("198.51.100.7"),
                "needs a fixed sfu_udp_port",
            ),
        ] {
            let err = ok(bind, port, public).unwrap_err();
            assert!(err.contains(says), "{bind:?} {port:?} {public:?}: {err}");
        }
    }

    /// With no `--sfu-bind-addr`, the SFU takes loopback and the LAN address
    /// (`--localhost` no longer narrows it; the API listeners still follow it). An
    /// explicit address is bound exactly; a malformed or unspecified one
    /// falls back to loopback.
    #[test]
    fn sfu_binds_loopback_and_the_lan_unless_an_address_is_given() {
        let loopback: std::net::IpAddr = std::net::Ipv4Addr::LOCALHOST.into();
        let lan_default: std::net::IpAddr = std::net::Ipv4Addr::UNSPECIFIED.into();
        assert_eq!(SfuServerConfig::default().bind_addr.ip(), loopback);
        assert_eq!(sfu_bind_ip(None), lan_default);
        assert_eq!(
            sfu_bind_ip(Some("::1")),
            "::1".parse::<std::net::IpAddr>().unwrap()
        );
        assert_eq!(
            sfu_bind_ip(Some("10.1.2.3")),
            "10.1.2.3".parse::<std::net::IpAddr>().unwrap()
        );
        assert_eq!(sfu_bind_ip(Some("127.0.0.1")), loopback);
        assert_eq!(sfu_bind_ip(Some("0.0.0.0")), loopback);
        assert_eq!(sfu_bind_ip(Some("::")), loopback);
        assert_eq!(sfu_bind_ip(Some("not-an-ip")), loopback);
    }

    /// A caller on a real UDP socket that reaches the SFU only at `target_ip`
    /// (every other candidate in the answer is dropped, as for a browser
    /// that pairs no loopback candidate). Returns the address it reached once
    /// its data channel opened through the running event loop; panics after 15 s.
    async fn call_through(server: &SfuServer, target_ip: std::net::IpAddr) -> SocketAddr {
        let socket = UdpSocket::bind("127.0.0.1:0").await.unwrap();
        let local = socket.local_addr().unwrap();
        let mut caller = Rtc::builder().build(Instant::now());
        caller.add_local_candidate(Candidate::host(local, "udp").unwrap());
        let mut change = caller.sdp_api();
        change.add_channel("sfu-data".into());
        let (offer, pending) = change.apply().unwrap();

        let hosts = server.sockets.host_addrs();
        let target = *hosts
            .iter()
            .find(|h| h.ip() == target_ip)
            .unwrap_or_else(|| panic!("{target_ip} is not offered: {hosts:?}"));
        let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &hosts, None).unwrap();
        let answer: str0m::change::SdpAnswer = serde_json::from_str(&answer).unwrap();
        caller.sdp_api().accept_answer(pending, answer).unwrap();
        server
            .command_tx
            .send(SfuCommand::AddPeer(SfuPeer::new(
                ParticipantId::next(),
                RoomId::new("neighbourhood://test", "room"),
                "did:key:caller".to_string(),
                rtc,
                false,
            )))
            .await
            .unwrap();

        let deadline = Instant::now() + Duration::from_secs(15);
        let mut buf = vec![0u8; 2000];
        loop {
            assert!(
                Instant::now() < deadline,
                "no data channel through {target} within 15 s"
            );
            let wake = match caller.poll_output().unwrap() {
                Output::Event(Event::ChannelOpen(..)) => return target,
                Output::Event(_) => continue,
                Output::Transmit(t) => {
                    if t.destination == target {
                        socket.send_to(&t.contents, t.destination).await.unwrap();
                    }
                    continue;
                }
                Output::Timeout(t) => t.min(deadline),
            };
            let wait = wake.saturating_duration_since(Instant::now());
            match tokio::time::timeout(wait, socket.recv_from(&mut buf)).await {
                Ok(Ok((n, source))) => {
                    receive(&mut caller, Instant::now(), source, local, &buf[..n])
                }
                Ok(Err(e)) => panic!("caller socket: {e}"),
                Err(_) => caller.handle_input(Input::Timeout(Instant::now())).unwrap(),
            }
        }
    }

    /// The LAN default end to end: a caller reaching only the LAN candidate
    /// connects through the running event loop (the packet's destination is
    /// the LAN socket's, so str0m answers its STUN). When the LAN address
    /// changes, the next call binds the new one, the event loop reads it, and
    /// a caller reaching only that address connects; loopback still works.
    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn a_caller_connects_through_the_lan_socket_and_the_one_after_a_change() {
        use std::sync::atomic::{AtomicU8, Ordering};
        use std::sync::Arc;
        let lan = Arc::new(AtomicU8::new(2));
        let detect: super::super::hosts::DetectLan = {
            let lan = lan.clone();
            Arc::new(move || {
                Ok(std::net::Ipv4Addr::new(127, 0, 0, lan.load(Ordering::SeqCst)).into())
            })
        };
        let sockets = MediaSockets::bind_with("0.0.0.0:0".parse().unwrap(), detect)
            .await
            .unwrap();
        let server = SfuServer::start_on(SfuServerConfig::default(), sockets)
            .await
            .unwrap();
        let ip = |s: &str| s.parse::<std::net::IpAddr>().unwrap();

        call_through(&server, ip("127.0.0.2")).await;

        lan.store(3, Ordering::SeqCst);
        let target = call_through(&server, ip("127.0.0.3")).await;
        assert!(server.sockets.bound_addrs().contains(&target));
        call_through(&server, ip("127.0.0.1")).await;
        let _ = server.command_tx.send(SfuCommand::Shutdown).await;
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
            Rtc::builder().build(Instant::now()),
            false,
        );
        let mut peer_c = SfuPeer::new(
            pid_c.clone(),
            room_id.clone(),
            "did:key:peerC".to_string(),
            Rtc::builder().build(Instant::now()),
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
        let mut scratch_rtc = Rtc::builder().build(Instant::now());
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
            TrackOrigin {
                participant_id: pid_a.to_string(),
                agent_did: "did:key:peerA".to_string(),
            },
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

    mod payload_types {
        use super::*;
        use str0m::format::{Codec, FormatParams};
        use str0m::media::{Frequency, Pt};

        /// A caller whose codec config numbers PTs the way a browser does:
        /// `firefox` gets opus 109 and VP8 120 (+RTX 121); otherwise str0m's
        /// defaults, which are Chrome's (opus 111, VP8 96 + RTX 97).
        /// `video_only` negotiates a VP8-only caller — one with no common
        /// codec for audio or for other video codecs.
        fn browser_rtc(firefox: bool, video_only: bool, addr: SocketAddr) -> Rtc {
            let mut config = Rtc::builder();
            if firefox {
                let codecs = config.codec_config();
                codecs.clear();
                if !video_only {
                    codecs.add_config(
                        109.into(),
                        None,
                        Codec::Opus,
                        Frequency::FORTY_EIGHT_KHZ,
                        Some(2),
                        FormatParams {
                            min_p_time: Some(10),
                            use_inband_fec: Some(true),
                            ..FormatParams::default()
                        },
                    );
                }
                codecs.add_config(
                    120.into(),
                    Some(121.into()),
                    Codec::Vp8,
                    Frequency::NINETY_KHZ,
                    None,
                    FormatParams::default(),
                );
            }
            let mut rtc = config.build(Instant::now());
            rtc.add_local_candidate(Candidate::host(addr, "udp").unwrap());
            rtc
        }

        /// One end of a call: the caller-side `Rtc` (the "browser"), the
        /// [`SfuPeer`] the event loop holds for it, and what each saw.
        struct CallEnd {
            caller: Rtc,
            peer: SfuPeer,
            pid: ParticipantId,
            addr: SocketAddr,
            bind: SocketAddr,
            /// The caller's own mids — the mids MediaData arrives under
            /// on `peer` (a mid keeps the offerer's name on both ends).
            mid_audio: Option<Mid>,
            mid_video: Mid,
            caller_seen: Seen,
            peer_seen: Seen,
        }

        /// A caller that offered `audio`/`video` sendrecv and got an ICE
        /// connection to its SFU peer up — the state the event loop holds
        /// once the join's signalling settled.
        fn call_end(
            firefox: bool,
            video_only: bool,
            addr: &str,
            bind: &str,
            did: &str,
            room: &RoomId,
        ) -> CallEnd {
            let caller_addr: SocketAddr = addr.parse().unwrap();
            let bind_addr: SocketAddr = bind.parse().unwrap();
            let mut caller = browser_rtc(firefox, video_only, caller_addr);
            let (offer, pending, mid_audio, mid_video) = loop {
                let mut change = caller.sdp_api();
                let mid_audio = (!video_only).then(|| {
                    change.add_media(
                        MediaKind::Audio,
                        str0m::media::Direction::SendRecv,
                        None,
                        None,
                        None,
                    )
                });
                let mid_video = change.add_media(
                    MediaKind::Video,
                    str0m::media::Direction::SendRecv,
                    None,
                    None,
                    None,
                );
                // str0m checks a new mid only against the session's m-lines,
                // not the batch's: two equal mids make one m-line shadow the
                // other, and the next offer fails "Changed order for m-line".
                // A browser never repeats a mid, so draw again, as
                // `offer_send_only` does.
                if mid_audio == Some(mid_video) {
                    continue;
                }
                let (offer, pending) = change.apply().unwrap();
                break (offer, pending, mid_audio, mid_video);
            };
            let (rtc, answer) = SfuServer::create_rtc_for_offer(offer, &[bind_addr], None).unwrap();
            caller
                .sdp_api()
                .accept_answer(pending, serde_json::from_str(&answer).unwrap())
                .unwrap();
            let peer = SfuPeer::new(
                ParticipantId::next(),
                room.clone(),
                did.to_string(),
                rtc,
                false,
            );
            let pid = peer.id.clone();

            let mut end = CallEnd {
                caller,
                peer,
                pid,
                addr: caller_addr,
                bind: bind_addr,
                mid_audio,
                mid_video,
                caller_seen: Seen::default(),
                peer_seen: Seen::default(),
            };

            let start = Instant::now();
            let mut now = start;
            let deadline = start + Duration::from_secs(10);
            loop {
                let (caller_next, peer_next) = exchange(&mut end, now, start);
                let up = |seen: &Seen| {
                    seen.ice.iter().any(|(_, s)| {
                        matches!(
                            s,
                            IceConnectionState::Connected | IceConnectionState::Completed
                        )
                    })
                };
                if up(&end.caller_seen) && up(&end.peer_seen) {
                    return end;
                }
                assert!(now < deadline, "the call never connected");
                now = caller_next.min(peer_next).clamp(
                    now + Duration::from_millis(1),
                    now + Duration::from_millis(50),
                );
            }
        }

        /// One round on one end's network link: the caller's packets go to
        /// the SFU's peer, the peer's packets come back. Returns each
        /// side's next str0m deadline.
        fn exchange(end: &mut CallEnd, now: Instant, start: Instant) -> (Instant, Instant) {
            let mut sent = Vec::new();
            let caller_next = drive(&mut end.caller, now, start, &mut end.caller_seen, &mut sent);
            for (_, destination, data) in sent.drain(..) {
                assert_eq!(destination, end.bind, "the caller sends to the SFU only");
                receive(&mut end.peer.rtc, now, end.addr, end.bind, &data);
            }
            let peer_next = drive(&mut end.peer.rtc, now, start, &mut end.peer_seen, &mut sent);
            for (_, destination, data) in sent.drain(..) {
                assert_eq!(destination, end.addr, "the SFU sends to the caller only");
                receive(&mut end.caller, now, end.bind, end.addr, &data);
            }
            (caller_next, peer_next)
        }

        /// The SFU offers `origin`'s tracks outbound on `end`'s peer, the
        /// caller answers, and the track mappings land as the event loop's
        /// renegotiation leaves them.
        fn plumb(end: &mut CallEnd, origin_pid: &ParticipantId, tracks: &[(Mid, MediaKind)]) {
            let kinds: Vec<MediaKind> = tracks.iter().map(|(_, kind)| *kind).collect();
            let (offer, pending, mids) = offer_send_only(&mut end.peer.rtc, &kinds, false).unwrap();
            end.peer.pending_offer = Some(pending);
            end.peer.pending_offer_sent = Some(Instant::now());
            let answer = end.caller.sdp_api().accept_offer(offer).unwrap();
            end.peer
                .accept_server_answer(&serde_json::to_string(&answer).unwrap())
                .unwrap();
            for ((origin_mid, _), out_mid) in tracks.iter().zip(&mids) {
                end.peer
                    .tracks_out
                    .insert(*out_mid, (origin_pid.clone(), *origin_mid));
                end.peer
                    .tracks_out_rev
                    .insert((origin_pid.clone(), *origin_mid), *out_mid);
            }
        }

        /// The caller sends `frame` on `mid` encoded as `codec`.
        fn write_media(caller: &mut Rtc, mid: Mid, codec: Codec, ticks: u64, frame: &[u8]) {
            let pt = caller
                .codec_config()
                .find(|p| p.spec().codec == codec)
                .unwrap_or_else(|| panic!("the caller negotiated no {codec:?}"))
                .pt();
            let time = if codec.is_audio() {
                MediaTime::new(ticks * 480, Frequency::FORTY_EIGHT_KHZ)
            } else {
                MediaTime::from_90khz(ticks * 3_000)
            };
            caller
                .writer(mid)
                .expect("a mid the caller sends on")
                .write(pt, Instant::now(), time, frame.to_vec())
                .expect("the caller writes with its own PT");
        }

        /// Forward every MediaData that landed on `origin`'s peer to
        /// `target`'s peer, as the event loop's relay does.
        fn relay(origin: &mut CallEnd, target: &mut CallEnd) {
            for data in origin.peer_seen.media.drain(..) {
                let track = (origin.pid.clone(), data.mid);
                let Some(&mid) = target.peer.tracks_out_rev.get(&track) else {
                    continue;
                };
                write_forwarded(&mut target.peer, mid, &data, data.time);
            }
        }

        /// Two callers whose SDPs number the same codecs differently —
        /// Chrome's opus/VP8 are 111/96, Firefox's 109/120 — hear each
        /// other because the SFU rewrites every packet's PT to the one
        /// the receiver negotiated. Before the fix the writes went out
        /// on the sender's PT and str0m refused them (UnknownPt), so
        /// each side heard and saw nothing.
        #[test]
        fn media_crossing_peers_arrives_on_the_receivers_payload_type() {
            let room = RoomId::new("neighbourhood://test", "room");
            let mut a = call_end(
                false,
                false,
                "203.0.113.20:50000",
                "192.168.0.10:40000",
                "did:key:alice",
                &room,
            );
            let mut b = call_end(
                true,
                false,
                "203.0.113.30:50000",
                "192.168.0.10:40001",
                "did:key:bob",
                &room,
            );

            // Each caller's tracks, outbound on the other's peer.
            plumb(
                &mut b,
                &a.pid,
                &[
                    (a.mid_audio.unwrap(), MediaKind::Audio),
                    (a.mid_video, MediaKind::Video),
                ],
            );
            plumb(
                &mut a,
                &b.pid,
                &[
                    (b.mid_audio.unwrap(), MediaKind::Audio),
                    (b.mid_video, MediaKind::Video),
                ],
            );

            const OPUS_A: &[u8] = b"alice's opus frame";
            const VP8_A: &[u8] = b"alice's vp8 frame, a longer one";
            const OPUS_B: &[u8] = b"bob's opus frame";
            const VP8_B: &[u8] = b"bob's vp8 frame, a longer one";
            let start = Instant::now();
            let mut now = start;
            write_media(&mut a.caller, a.mid_audio.unwrap(), Codec::Opus, 1, OPUS_A);
            write_media(&mut a.caller, a.mid_video, Codec::Vp8, 1, VP8_A);
            write_media(&mut b.caller, b.mid_audio.unwrap(), Codec::Opus, 1, OPUS_B);
            write_media(&mut b.caller, b.mid_video, Codec::Vp8, 1, VP8_B);

            // Run the room: media lands on each origin's peer, is
            // forwarded on the other's outbound mid as the relay does,
            // and reaches the other caller.
            let (mut at_a, mut at_b) = (Vec::new(), Vec::new());
            for _ in 0..100 {
                exchange(&mut a, now, start);
                exchange(&mut b, now, start);
                relay(&mut a, &mut b);
                relay(&mut b, &mut a);
                at_a.append(&mut a.caller_seen.media);
                at_b.append(&mut b.caller_seen.media);
                if at_a.len() >= 2 && at_b.len() >= 2 {
                    break;
                }
                now += Duration::from_millis(1);
            }

            // Bob hears Alice on his own numbering: opus 109, VP8 120.
            let opus = at_b
                .iter()
                .find(|d| &*d.data == OPUS_A)
                .expect("Alice's opus reached Bob");
            assert_eq!(opus.pt, Pt::new_with_value(109));
            let vp8 = at_b
                .iter()
                .find(|d| &*d.data == VP8_A)
                .expect("Alice's VP8 reached Bob");
            assert_eq!(vp8.pt, Pt::new_with_value(120));

            // And Alice hears Bob on hers: opus 111, VP8 96.
            let opus = at_a
                .iter()
                .find(|d| &*d.data == OPUS_B)
                .expect("Bob's opus reached Alice");
            assert_eq!(opus.pt, Pt::new_with_value(111));
            let vp8 = at_a
                .iter()
                .find(|d| &*d.data == VP8_B)
                .expect("Bob's VP8 reached Alice");
            assert_eq!(vp8.pt, Pt::new_with_value(96));
        }

        /// A track whose codec the receiver never negotiated is dropped
        /// — and logged once per (mid, codec), not once per packet.
        #[test]
        fn a_receiver_with_no_matching_codec_drops_the_track_once() {
            let room = RoomId::new("neighbourhood://test", "room");
            let mut a = call_end(
                false,
                false,
                "203.0.113.20:50000",
                "192.168.0.10:40000",
                "did:key:alice",
                &room,
            );
            // Bob negotiates VP8 only: no audio, no other video codec.
            let mut b = call_end(
                true,
                true,
                "203.0.113.30:50000",
                "192.168.0.10:40001",
                "did:key:bob",
                &room,
            );
            plumb(&mut b, &a.pid, &[(a.mid_video, MediaKind::Video)]);

            const VP8_A: &[u8] = b"alice's vp8 frame, a longer one";
            let start = Instant::now();
            let mut now = start;
            // Alice sends two VP9 frames and one VP8 frame; Bob's video
            // mid negotiated no VP9.
            write_media(
                &mut a.caller,
                a.mid_video,
                Codec::Vp9,
                1,
                b"alice's first vp9 frame",
            );
            write_media(
                &mut a.caller,
                a.mid_video,
                Codec::Vp9,
                2,
                b"alice's second vp9 frame",
            );
            write_media(&mut a.caller, a.mid_video, Codec::Vp8, 3, VP8_A);

            let mut at_b = Vec::new();
            for _ in 0..100 {
                exchange(&mut a, now, start);
                exchange(&mut b, now, start);
                relay(&mut a, &mut b);
                at_b.append(&mut b.caller_seen.media);
                now += Duration::from_millis(1);
            }

            // The VP8 frame arrived on Bob's numbering; both VP9 frames
            // were dropped, logged once for the (mid, codec) pair.
            let vp8 = at_b
                .iter()
                .find(|d| &*d.data == VP8_A)
                .expect("VP8 reached Bob");
            assert_eq!(vp8.pt, Pt::new_with_value(120));
            assert_eq!(at_b.len(), 1, "only VP8 may arrive: {at_b:?}");
            assert_eq!(b.peer.drops_logged.len(), 1, "two dropped packets log once");
        }
    }
}
