//! SFU server — the media UDP sockets ([`MediaSockets`]), str0m Rtc instances,
//! and the Sans I/O event loop driven by tokio.

use std::collections::{HashMap, HashSet};
use std::mem::Discriminant;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::LazyLock;
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
use str0m::stats::PeerStats;
use str0m::{net::Receive, Candidate, Event, IceConnectionState, Input, Output, Rtc, RtcError};
use tokio::net::UdpSocket;
use tokio::sync::mpsc;

use super::hosts::{socket_for, MediaSockets};
use super::reachability::{assess, stun_reflexive, Advertised, SfuReachability};
use super::relay::MediaRelay;
use super::room::{random_upper_half_u64, ParticipantId, RoomId};
use super::simulcast::{is_keyframe, LayerSelector};
use super::types::{SfuAnswerRefusal, SfuAnswerResult, SfuCallRenegotiationOffer};
use str0m::bwe::{Bitrate, BweKind};

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
    /// The one in-flight server offer. str0m allows one change at a time.
    /// The offer has an id the answer echoes, so a retry republishes this
    /// record's SDP and id instead of building another.
    pending: Option<PendingNegotiation>,
    /// The offer id last applied on this session. An answer that names it
    /// again is accepted and changes nothing, including while a newer offer
    /// waits.
    last_accepted_offer_id: Option<String>,
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
    /// Pipe only: the room's existing origins have been offered once, when
    /// the pipe became usable. Later tracks use the fresh-track path. A
    /// client peer leaves this false.
    origins_seeded: bool,
    /// Open data channels: ChannelId → label.  Populated from
    /// `Event::ChannelOpen`; used for data relay routing.
    pub channels: HashMap<ChannelId, String>,
    /// When ICE last reported `Disconnected` without coming back, or `None`.
    /// See [`ICE_DISCONNECT_GRACE`].
    pub disconnected_since: Option<Instant>,
    /// Whether ICE has reached `Connected` or `Completed` on this peer. A
    /// later fall back to `Checking` means its path failed.
    pub had_path: bool,
    /// When the peer was made. See [`PEER_ESTABLISH_TIMEOUT`].
    pub created_at: Instant,
    /// When this pipe's current usable stretch began (ICE connected or
    /// completed, or its control channel opened). Cleared on
    /// `Disconnected`, so that stretch is not counted toward
    /// [`super::cascade::PIPE_HEALTHY_RESET`]. A client peer leaves it unset.
    usable_since: Option<Instant>,
    /// A usable stretch that ended on `Disconnected` had already reached
    /// [`super::cascade::PIPE_HEALTHY_RESET`]. A later shorter stretch does
    /// not clear it. The drop after the disconnect grace still sees it.
    /// A client peer leaves it false.
    healthy_stretch: bool,
    /// An ICE restart the client asked for (`sfu.callRestartIce`), from the
    /// request until its answer lands or the offer's retry budget ends.
    /// The offer SDP lives in [`PendingNegotiation`].
    pub ice_restart: Option<IceRestart>,
    /// (outbound mid, codec) of forwarded packets dropped because the
    /// peer negotiated no matching codec on that mid — each logged once,
    /// not per packet, so a track the peer cannot take does not flood
    /// the log at media rate.
    pub drops_logged: HashSet<(Mid, Discriminant<Codec>)>,
    /// (outbound mid, error kind) of failed media writes already logged
    /// — once each, not per packet.
    pub write_errors_logged: HashSet<(Mid, Discriminant<RtcError>)>,
    /// When media (RTP) from this peer last arrived, or `None`.
    /// See [`MEDIA_LIVE_WINDOW`].
    pub last_media_at: Option<Instant>,
    /// Why this peer is being dropped, set before `rtc.disconnect()`.
    /// `None` when the peer died for a reason the sweep did not name.
    end_reason: Option<&'static str>,
    /// The media byte counters of the last stats line, for its bitrates.
    /// See [`PEER_STATS_INTERVAL`].
    stats_last: Option<StatsSample>,
}

/// Media bytes in and out at one [`PeerStats`] event.
#[derive(Debug, Clone, Copy)]
struct StatsSample {
    at: Instant,
    bytes_rx: u64,
    bytes_tx: u64,
}

/// One server offer waiting for its answer: the str0m token, the id the
/// answer echoes, and the exact SDP and track mapping to publish again.
/// The id is minted once. A resend keeps it. The record stays until the
/// answer lands.
struct PendingNegotiation {
    /// Minted in [`SfuPeer::install_pending`]. A resend does not change it.
    offer_id: String,
    token: str0m::change::SdpPendingOffer,
    /// When the offer was first published. A resend does not move it.
    sent_at: Instant,
    sdp: String,
    track_mapping: Vec<crate::sfu::TrackMapEntry>,
    /// Resends already published. The first publication is not one of them.
    resends: u32,
    /// When the next resend is due.
    next_deadline: Instant,
    /// This offer carries an ICE restart.
    carries_restart: bool,
}

/// What [`SfuPeer::on_offer_deadline`] decided for one tick.
enum OfferDeadline {
    /// Nothing is due, or this deadline was already taken.
    Wait,
    /// Publish the cached SDP again. The token stays pending.
    Resend,
    /// An established pipe's offer is still unanswered after the second
    /// resend. Disconnect the pipe. The cascade redials it.
    End,
}

/// A client's ICE restart in flight. The SFU stays the only offerer: the
/// restart travels as a server renegotiation offer, so it cannot cross an
/// offer of the SFU's own. The offer SDP is the pending negotiation; this
/// struct keeps the request clock, a follow-up request, and candidates held
/// until the answer.
#[derive(Debug)]
pub struct IceRestart {
    /// When the client asked. The disconnect grace runs until
    /// `since + ICE_RESTART_GRACE`. A resend does not move it.
    pub since: Instant,
    /// The client asked again while the restart's offer waited: it moved
    /// again, or lost the answer. A fresh restart follows this answer, so
    /// the newest network gets new credentials. The resend of this offer
    /// uses the offer deadline, not an immediate publish.
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
/// request, however long it was disconnected before. The first resend of
/// that offer is due after 10 s. This grace is that wait plus 5 s, so the
/// offer is published while the peer is still up and the client still has
/// time to answer. A resend does not move the clock. The restart's answer
/// ends the grace and the disconnect clock starts again: ICE checks again,
/// but its state arrives only on the next poll.
pub const ICE_RESTART_GRACE: Duration = Duration::from_secs(15);

/// How long a peer may go without ever reaching ICE `Connected` before it
/// is dropped. A cascade pipe whose answer never arrives has no remote
/// candidate: str0m keeps it in `Checking` and never reports
/// `Disconnected`, so the disconnect grace never starts. Dropped, its pipe
/// entry goes ([`super::service`]'s dead-pipe cleanup) and the next announce
/// dials a new pipe. A client that never connects ends the same way. A peer
/// that had a path is never ended by this.
///
/// The second resend of the initial offer is due at 30 s. The window runs
/// that long plus [`ICE_RESTART_GRACE`], so an answer that arrives with the
/// second resend still has time to complete ICE before the pipe is swept.
pub const PEER_ESTABLISH_TIMEOUT: Duration = Duration::from_secs(
    // Second resend at 30 s, then [`ICE_RESTART_GRACE`] for ICE to finish.
    OFFER_RETRY_BASE.as_secs() * 3 + ICE_RESTART_GRACE.as_secs(),
);

/// Wait before the first resend of an unanswered offer. The wait doubles
/// once: resends at t=10s and t=30s, then every [`OFFER_RESEND_STEADY`]
/// (t=60s, 90s, ...) for as long as the offer is unanswered.
///
/// An unanswered offer never drops a client peer. #1802 dropped it at
/// t=70s, also while ICE was up and its media flowed: a client whose
/// socket was down for a minute lost a healthy call. A client keeps the
/// offer and resends it. An established pipe ends after the back-off
/// resends ([`PIPE_OFFER_RESEND_LIMIT`]): the pipe is disconnected and the
/// cascade redials it, so a token the remote may already have applied is
/// not offered again with new mids. A pipe that never had a path keeps the
/// initial offer until [`PEER_ESTABLISH_TIMEOUT`]. A peer whose client is
/// gone ends by the ICE rules ([`ICE_DISCONNECT_GRACE`], [`ICE_RESTART_GRACE`]).
const OFFER_RETRY_BASE: Duration = Duration::from_secs(10);

/// Resends that back off from [`OFFER_RETRY_BASE`] before the steady wait.
const OFFER_BACKOFF_RESENDS: u32 = 2;

/// An established pipe ends an unanswered offer after this many resends
/// (the back-off: t=10 s and t=30 s). The next deadline disconnects the
/// pipe. The cascade redials the first end at once. The next consecutive
/// failure waits [`super::cascade::PIPE_REDIAL_BACKOFF`], which doubles up
/// to the cap, and a pipe that stayed connected for
/// [`super::cascade::PIPE_HEALTHY_RESET`] starts that count again. An initial pipe
/// offer, before any path, is not ended here: it carries the pipe-control
/// channel, and [`PEER_ESTABLISH_TIMEOUT`] drops that pipe. A client offer
/// is never ended here. A crossed offer ([`RtcError::ChangesOutOfOrder`])
/// is not an end either: the tracks are queued again on this pipe.
const PIPE_OFFER_RESEND_LIMIT: u32 = OFFER_BACKOFF_RESENDS;

/// Wait between resends after the back-off, and after a `sfu.callResume`.
const OFFER_RESEND_STEADY: Duration = Duration::from_secs(30);

/// A client's bandwidth estimate before its first feedback (kbit/s). It
/// budgets a low layer and the audio; the estimator probes up from there.
const BWE_START_KBPS: u64 = 800;

/// How often each client peer logs one stats line (str0m's `PeerStats`):
/// bitrate and bytes in and out, loss each way, RTT, the simulcast layers it
/// receives, and how long ago its media last arrived.
pub const PEER_STATS_INTERVAL: Duration = Duration::from_secs(30);

/// Media from a peer within this window counts as flowing.
pub const MEDIA_LIVE_WINDOW: Duration = Duration::from_secs(10);

/// The most trickled candidates held while an ICE restart's offer waits for
/// its answer. A client gathers a handful per network; later ones are dropped.
const MAX_HELD_CANDIDATES: usize = 64;

/// How long to wait after `resends_done` publications. `0` is the wait
/// after the first send.
fn offer_retry_wait(resends_done: u32) -> Duration {
    if resends_done < OFFER_BACKOFF_RESENDS {
        OFFER_RETRY_BASE * (1u32 << resends_done)
    } else {
        OFFER_RESEND_STEADY
    }
}

/// An id for one server offer. Minted once in [`SfuPeer::install_pending`]
/// and kept for every resend of that offer.
static OFFER_IDS: LazyLock<AtomicU64> = LazyLock::new(|| AtomicU64::new(random_upper_half_u64()));

fn next_offer_id() -> String {
    format!("o{}", OFFER_IDS.fetch_add(1, Ordering::Relaxed))
}

/// The next offer id starts again, as it does when the process starts.
#[cfg(test)]
pub(crate) fn restart_offer_ids() {
    OFFER_IDS.store(random_upper_half_u64(), Ordering::SeqCst);
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
            pending: None,
            last_accepted_offer_id: None,
            deferred_tracks: Vec::new(),
            remote_origins: HashMap::new(),
            is_pipe,
            origins_seeded: false,
            channels: HashMap::new(),
            disconnected_since: None,
            had_path: false,
            created_at: Instant::now(),
            usable_since: None,
            healthy_stretch: false,
            ice_restart: None,
            drops_logged: HashSet::new(),
            write_errors_logged: HashSet::new(),
            last_media_at: None,
            stats_last: None,
            end_reason: None,
        }
    }

    /// Media (RTP) from this peer arrived at `now`.
    pub fn note_media(&mut self, now: Instant) {
        self.last_media_at = Some(now);
    }

    /// Whether media from this peer arrived within [`MEDIA_LIVE_WINDOW`].
    pub fn media_flowing(&self, now: Instant) -> bool {
        self.last_media_at
            .is_some_and(|at| now.saturating_duration_since(at) < MEDIA_LIVE_WINDOW)
    }

    /// Track an ICE state change: `Disconnected` starts the disconnect
    /// clock, any other state stops it. A pipe's usable stretch starts on
    /// `Connected` or `Completed` and ends on `Disconnected`, so a
    /// disconnected stretch does not count as healthy. The stretch that
    /// just ended is recorded before the clock is cleared.
    pub fn on_ice_state(&mut self, state: IceConnectionState, now: Instant) {
        self.disconnected_since = match state {
            IceConnectionState::Disconnected => {
                if self.is_pipe {
                    self.end_usable_stretch(now);
                }
                self.disconnected_since.or(Some(now))
            }
            _ => None,
        };
        if pipe_event_is_usable(Some(state), false) {
            self.had_path = true;
            self.note_usable(now);
        }
    }

    /// The usable stretch ended. Remember whether it reached the healthy
    /// interval, then clear it. A disconnected period is not added, and
    /// the later drop still knows.
    fn end_usable_stretch(&mut self, now: Instant) {
        if let Some(since) = self.usable_since {
            if now.saturating_duration_since(since) >= super::cascade::PIPE_HEALTHY_RESET {
                self.healthy_stretch = true;
            }
        }
        self.usable_since = None;
    }

    /// Remember the start of this pipe's usable stretch. A second event in
    /// the same stretch does not move it. A client peer has no stretch.
    fn note_usable(&mut self, now: Instant) {
        if self.is_pipe && self.usable_since.is_none() {
            self.usable_since = Some(now);
        }
    }

    /// Whether a connected stretch reached [`super::cascade::PIPE_HEALTHY_RESET`].
    /// The stretch that ended on `Disconnected` was recorded then. A stretch
    /// still open at `now` (the pipe dropped while ICE is up) is measured
    /// here. A disconnected period is in neither.
    fn reached_healthy(&self, now: Instant) -> bool {
        if !self.is_pipe {
            return false;
        }
        if self.healthy_stretch {
            return true;
        }
        self.usable_since.is_some_and(|since| {
            now.saturating_duration_since(since) >= super::cascade::PIPE_HEALTHY_RESET
        })
    }

    /// The dead-peer notice the sweep sends. `now` is the sweep's clock.
    fn dead_peer(&self, now: Instant) -> DeadPeer {
        DeadPeer {
            room_id: self.room_id.clone(),
            agent_did: self.agent_did.clone(),
            participant_id: self.id.clone(),
            is_pipe: self.is_pipe,
            reason: self.end_reason.unwrap_or("drop"),
            healthy_stretch: self.reached_healthy(now),
        }
    }

    /// Whether ICE never had a path within [`PEER_ESTABLISH_TIMEOUT`] of the
    /// peer being made.
    pub fn never_established(&self, now: Instant) -> bool {
        !self.had_path && now >= self.created_at + PEER_ESTABLISH_TIMEOUT
    }

    /// Why the sweep is dropping this peer, once it has been named.
    #[cfg(test)]
    fn drop_reason(&self) -> Option<&'static str> {
        self.end_reason
    }

    /// Disconnect an established pipe whose answer str0m rejected locally,
    /// or whose offer went unanswered after the second resend. One warning:
    /// the pipe ended. The pending token stays, so this tick does not offer
    /// those tracks again on the pipe that is going away. The cascade
    /// removes the entry and redials. This is not a remote refusal: a pipe
    /// answer carries none.
    fn end_for_offer(&mut self, why: &'static str) {
        warn!(
            "SFU: pipe {} ended ({why}); the cascade can redial",
            self.id
        );
        self.end_reason = Some("offer");
        self.rtc.disconnect();
    }

    /// Whether the peer stayed disconnected past its grace, or never had a
    /// path, and is to be dropped. See [`ICE_DISCONNECT_GRACE`],
    /// [`ICE_RESTART_GRACE`] and [`PEER_ESTABLISH_TIMEOUT`].
    pub fn stayed_disconnected(&self, now: Instant) -> bool {
        if self.never_established(now) {
            return true;
        }
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
    /// only records the request. Asked again while the restart's offer is
    /// the pending negotiation, a fresh restart follows that answer. The
    /// resend of the offer in hand uses the offer deadline.
    pub fn restart_ice(&mut self, now: Instant) {
        match self.ice_restart.as_mut() {
            Some(restart) => {
                if self
                    .pending
                    .as_ref()
                    .is_some_and(|pending| pending.carries_restart)
                {
                    restart.again = true;
                }
            }
            None => {
                self.ice_restart = Some(IceRestart {
                    since: now,
                    again: false,
                    candidates: Vec::new(),
                });
            }
        }
    }

    /// Whether a change waits for the next offer: deferred tracks, or an
    /// ICE restart that is not the current pending offer.
    fn has_deferred_changes(&self) -> bool {
        !self.deferred_tracks.is_empty() || self.restart_waiting()
    }

    /// An ICE restart was requested and its offer is not the pending one.
    fn restart_waiting(&self) -> bool {
        self.ice_restart.is_some()
            && !self
                .pending
                .as_ref()
                .is_some_and(|pending| pending.carries_restart)
    }

    /// A trickled remote candidate. Held while an ICE restart's offer waits
    /// for its answer: the restart would discard it. A resend keeps the
    /// offer pending, so the candidates stay held.
    pub fn add_remote_candidate(&mut self, candidate: Candidate) {
        let hold = self
            .pending
            .as_ref()
            .is_some_and(|pending| pending.carries_restart);
        if let Some(restart) = self.ice_restart.as_mut().filter(|_| hold) {
            if restart.candidates.len() < MAX_HELD_CANDIDATES {
                restart.candidates.push(candidate);
            } else {
                debug!(
                    "SFU: peer {} trickled too many candidates during an ICE restart",
                    self.id
                );
            }
        } else {
            self.rtc.add_remote_candidate(candidate);
        }
    }

    /// Apply the client's answer to the SFU's pending offer. An answer to
    /// an ICE restart's offer ends the restart and adds the candidates
    /// trickled meanwhile, then starts the next restart if one was asked for.
    ///
    /// An answer is taken however late it comes: the offer stays pending
    /// until it is answered. `offer_id` is the match: an answer that names
    /// another offer stays pending, and so does one whose m-lines are not
    /// this offer's. A JSON parse failure leaves the pending negotiation
    /// unchanged. str0m consuming the token and rejecting
    /// the answer locally does not: the caller disconnects the peer. An answer that
    /// crossed another offer has consumed the token; the caller keeps the peer.
    fn accept_server_answer(
        &mut self,
        offer_id: &str,
        sdp_answer_json: &str,
    ) -> Result<(), ServerAnswerError> {
        // A retry of the offer already applied: the reply was lost, or the
        // answer arrived again after a newer offer. Nothing moves.
        if self.last_accepted_offer_id.as_deref() == Some(offer_id) {
            return Ok(());
        }
        let Some(pending) = self.pending.as_ref() else {
            return Err(ServerAnswerError::NoPending);
        };
        // The offer id is the match. An ICE restart reuses m-lines, so two
        // offers can share a mid list; a stale answer with those mids must
        // not be taken. The id is minted once and kept across resends.
        if pending.offer_id != offer_id {
            return Err(ServerAnswerError::NotThisOffer);
        }
        let answer: str0m::change::SdpAnswer = match serde_json::from_str(sdp_answer_json) {
            Ok(answer) => answer,
            Err(e) => return Err(ServerAnswerError::Parse(e.to_string())),
        };
        // After the id matches, and before the token is taken. A right id
        // with the wrong SDP can still change the session inside str0m
        // before str0m rejects it. The mid list is that check, not the match.
        let offer_mids = sdp_mid_list(&stored_offer_sdp_text(&pending.sdp));
        let answer_mids = sdp_mid_list(&answer.to_sdp_string());
        if offer_mids != answer_mids {
            return Err(ServerAnswerError::NotThisOffer);
        }
        let pending = self.pending.take().expect("the offer was pending above");
        let carries_restart = pending.carries_restart;
        let track_mapping = pending.track_mapping.clone();
        match self.rtc.sdp_api().accept_answer(pending.token, answer) {
            Ok(()) => {}
            Err(RtcError::ChangesOutOfOrder) => {
                // The token is already consumed. The mids were recorded when
                // the offer was built and str0m never added them: queue them
                // again or the next negotiation skips them.
                self.requeue_offer_tracks(&track_mapping);
                return Err(ServerAnswerError::OutOfOrder);
            }
            Err(e) => return Err(ServerAnswerError::Rejected(format!("{e:?}"))),
        }
        if carries_restart {
            if let Some(restart) = self.ice_restart.take() {
                // ICE checks again from here, but its state arrives only on
                // the next poll, after the disconnect sweep: restart the clock.
                if let Some(since) = self.disconnected_since.as_mut() {
                    *since = Instant::now();
                }
                for candidate in restart.candidates {
                    self.rtc.add_remote_candidate(candidate);
                }
                if restart.again {
                    self.ice_restart = Some(IceRestart {
                        since: Instant::now(),
                        again: false,
                        candidates: Vec::new(),
                    });
                }
            }
        }
        self.last_accepted_offer_id = Some(offer_id.to_string());
        Ok(())
    }

    /// Apply an answer the way the event loop does, and say what the RPC
    /// answers. Only an answer that belongs to the pending offer and that
    /// str0m rejected locally disconnects an established pipe. A late
    /// answer, an answer for an older offer, and an answer that crossed
    /// another offer leave the peer and its media in place. A repeat of an
    /// offer already accepted is accepted and changes nothing.
    ///
    /// The reply has three refusals. An answer that does not parse, or whose
    /// m-lines are not the pending offer's, leaves that offer pending
    /// (`stale`). One whose token str0m has already taken answers
    /// `no-pending`.
    fn settle_answer(&mut self, offer_id: &str, sdp_answer_json: &str) -> SfuAnswerResult {
        let already = self.last_accepted_offer_id.as_deref() == Some(offer_id);
        match self.accept_server_answer(offer_id, sdp_answer_json) {
            Ok(()) => {
                if already {
                    debug!(
                        "SFU: server-offer answer for {} repeats accepted offer {offer_id}",
                        self.id
                    );
                } else {
                    info!(
                        "SFU: server-offer answer accepted for {} ({} outbound tracks live)",
                        self.id,
                        self.tracks_out.len()
                    );
                }
                SfuAnswerResult::accepted()
            }
            Err(ServerAnswerError::Rejected(error)) => {
                // str0m consumed the token and rejected the answer locally.
                // A pipe answer carries no refusal, and a remote that rejects
                // an offer only logs it, so that sender waits until the offer
                // goes unanswered. A resend would answer a change that no
                // longer exists. An established pipe ends, so the cascade can
                // redial it. A crossed offer is not this arm.
                if self.is_pipe && self.had_path {
                    self.end_for_offer("answer rejected locally");
                } else {
                    warn!("SFU: ApplyServerAnswer for {}: {error}", self.id);
                    self.rtc.disconnect();
                }
                SfuAnswerResult::refused(SfuAnswerRefusal::NoPending)
            }
            Err(ServerAnswerError::NoPending) => {
                debug!(
                    "SFU: ApplyServerAnswer for {} with no pending offer",
                    self.id
                );
                SfuAnswerResult::refused(SfuAnswerRefusal::NoPending)
            }
            Err(ServerAnswerError::OutOfOrder) => {
                warn!(
                    "SFU: ApplyServerAnswer for {}: answer crossed another offer",
                    self.id
                );
                SfuAnswerResult::refused(SfuAnswerRefusal::NoPending)
            }
            Err(error) => {
                warn!("SFU: ApplyServerAnswer for {}: {error}", self.id);
                SfuAnswerResult::refused(SfuAnswerRefusal::Stale)
            }
        }
    }

    /// The pipe's first offer. The cascade manager has already serialised it.
    pub fn note_initial_offer(
        &mut self,
        token: str0m::change::SdpPendingOffer,
        sdp: String,
        now: Instant,
    ) {
        self.install_pending(token, sdp, Vec::new(), now, false);
    }

    /// Put `track_mapping`'s outbound mids back on the deferred queue. The
    /// maps were written when the offer was built; str0m has no writer for
    /// them until the answer is accepted, and a later offer skips a mid
    /// that is still in `tracks_out_rev`.
    fn requeue_offer_tracks(&mut self, track_mapping: &[crate::sfu::TrackMapEntry]) {
        for entry in track_mapping {
            let mid = Mid::from(entry.mid.as_str());
            let Some((origin_pid, origin_mid)) = self.tracks_out.remove(&mid) else {
                continue;
            };
            self.tracks_out_rev
                .remove(&(origin_pid.clone(), origin_mid));
            let kind = if entry.media_kind == "video" {
                MediaKind::Video
            } else {
                MediaKind::Audio
            };
            self.deferred_tracks.push((
                origin_pid,
                origin_mid,
                kind,
                TrackOrigin {
                    participant_id: entry.participant_id.clone(),
                    agent_did: entry.agent_did.clone(),
                },
            ));
        }
    }

    /// The remote offer bumped str0m's change id, so this token can no
    /// longer be answered. Queue its tracks again on this same pipe. This
    /// is the crossed-offer case: the pipe stays, and the deadline path
    /// does not use it. No-op when nothing is pending.
    fn requeue_invalidated_offer(&mut self) {
        let Some(pending) = self.pending.take() else {
            return;
        };
        drop(pending.token);
        self.requeue_offer_tracks(&pending.track_mapping);
    }

    fn install_pending(
        &mut self,
        token: str0m::change::SdpPendingOffer,
        sdp: String,
        track_mapping: Vec<crate::sfu::TrackMapEntry>,
        now: Instant,
        carries_restart: bool,
    ) {
        self.pending = Some(PendingNegotiation {
            offer_id: next_offer_id(),
            token,
            sent_at: now,
            sdp,
            track_mapping,
            resends: 0,
            next_deadline: now + offer_retry_wait(0),
            carries_restart,
        });
    }

    /// The id of the pending offer, when one waits.
    #[cfg(test)]
    fn pending_offer_id(&self) -> Option<&str> {
        self.pending
            .as_ref()
            .map(|pending| pending.offer_id.as_str())
    }

    /// [`Self::accept_server_answer`] for the offer that is pending now.
    #[cfg(test)]
    fn accept_current_answer(&mut self, sdp_answer_json: &str) -> Result<(), ServerAnswerError> {
        let id = self.pending_offer_id().unwrap_or("").to_string();
        self.accept_server_answer(&id, sdp_answer_json)
    }

    /// [`Self::settle_answer`] for the offer that is pending now.
    #[cfg(test)]
    fn settle_current_answer(&mut self, sdp_answer_json: &str) {
        let id = self.pending_offer_id().unwrap_or("").to_string();
        self.settle_answer(&id, sdp_answer_json);
    }

    /// The client asked for its call state again (`sfu.callResume`), e.g.
    /// after its socket came back: an offer published meanwhile may never
    /// have reached it. Returns the pending offer's id, SDP and mapping to
    /// publish at once, or `None` when no offer waits. The id is the one
    /// minted for this offer. The resend counts as one, so the next
    /// deadline resend comes [`OFFER_RESEND_STEADY`] later.
    pub fn resume_offer(
        &mut self,
        now: Instant,
    ) -> Option<(String, String, Vec<crate::sfu::TrackMapEntry>)> {
        let pending = self.pending.as_mut()?;
        pending.resends += 1;
        pending.next_deadline = now + OFFER_RESEND_STEADY;
        Some((
            pending.offer_id.clone(),
            pending.sdp.clone(),
            pending.track_mapping.clone(),
        ))
    }

    /// One debug line from str0m's `PeerStats`: media bitrate in and out
    /// since the last line (bytes in and out in total), loss each way, RTT,
    /// the simulcast layer of each video track it receives (`-`: none
    /// forwarded yet), its quality preference, and how long ago its media
    /// last arrived.
    fn stats_line(
        &mut self,
        stats: &PeerStats,
        layers_in: &[Option<Rid>],
        preference: Option<&str>,
        now: Instant,
    ) -> String {
        let sample = StatsSample {
            at: stats.timestamp,
            bytes_rx: stats.bytes_rx,
            bytes_tx: stats.bytes_tx,
        };
        let kbps = |bytes: u64, since: Duration| {
            let secs = since.as_secs_f64();
            if secs > 0.0 {
                format!("{:.0}", bytes as f64 * 8.0 / 1000.0 / secs)
            } else {
                "-".to_string()
            }
        };
        let (kbps_in, kbps_out) = match self.stats_last {
            Some(last) => {
                let since = sample.at.saturating_duration_since(last.at);
                (
                    kbps(sample.bytes_rx.saturating_sub(last.bytes_rx), since),
                    kbps(sample.bytes_tx.saturating_sub(last.bytes_tx), since),
                )
            }
            None => ("-".to_string(), "-".to_string()),
        };
        self.stats_last = Some(sample);
        let percent = |loss: Option<f32>| match loss {
            Some(fraction) => format!("{:.1}%", fraction * 100.0),
            None => "-".to_string(),
        };
        let rtt = stats.rtt.or_else(|| {
            stats
                .selected_candidate_pair
                .as_ref()
                .and_then(|pair| pair.current_round_trip_time)
        });
        let mut layers: Vec<String> = layers_in
            .iter()
            .map(|rid| rid.map_or_else(|| "-".to_string(), |rid| rid.to_string()))
            .collect();
        layers.sort();
        format!(
            "SFU stats: peer {} ({}) room {} in {} kbit/s out {} kbit/s (total {} kB in, {} kB out) loss in {} out {} rtt {} layers [{}] pref {} media {}",
            self.id,
            self.agent_did,
            self.room_id,
            kbps_in,
            kbps_out,
            stats.bytes_rx / 1000,
            stats.bytes_tx / 1000,
            percent(stats.ingress_loss_fraction),
            percent(stats.egress_loss_fraction),
            rtt.map_or_else(|| "-".to_string(), |rtt| format!("{} ms", rtt.as_millis())),
            layers.join(","),
            preference.unwrap_or("auto"),
            match self.last_media_at {
                Some(at) => format!("{:.1} s ago", now.saturating_duration_since(at).as_secs_f64()),
                None => "never".to_string(),
            },
        )
    }

    /// `now` reached the pending offer's deadline. One call publishes at
    /// most one resend and moves the deadline, so repeated ticks at the
    /// same instant publish once. The token stays pending across resends.
    /// A client peer stays: see [`OFFER_RETRY_BASE`]. An established pipe
    /// that has used its back-off resends is [`OfferDeadline::End`]: the
    /// pipe is disconnected and the cascade redials it.
    fn on_offer_deadline(&mut self, now: Instant) -> OfferDeadline {
        let media_flowing = self.media_flowing(now);
        let end = self.is_pipe
            && self.had_path
            && self.pending.as_ref().is_some_and(|pending| {
                now >= pending.next_deadline && pending.resends >= PIPE_OFFER_RESEND_LIMIT
            });
        if end {
            return OfferDeadline::End;
        }
        let Some(pending) = self.pending.as_mut() else {
            return OfferDeadline::Wait;
        };
        if now < pending.next_deadline {
            return OfferDeadline::Wait;
        }
        pending.resends += 1;
        pending.next_deadline = now + offer_retry_wait(pending.resends);
        if pending.resends > OFFER_BACKOFF_RESENDS {
            debug!(
                "SFU: peer {} offer unanswered for {} s ({} resends, media {}); keeping the peer, resending every {} s",
                self.id,
                now.saturating_duration_since(pending.sent_at).as_secs(),
                pending.resends,
                if media_flowing { "flowing" } else { "silent" },
                OFFER_RESEND_STEADY.as_secs(),
            );
        }
        OfferDeadline::Resend
    }
}

/// A pipe is usable for seeding and for the healthy-interval clock when
/// ICE is `Connected` or `Completed`, or when its control channel opens.
/// The event loop and the recovery test share this.
fn pipe_event_is_usable(ice: Option<IceConnectionState>, channel_opened: bool) -> bool {
    channel_opened
        || matches!(
            ice,
            Some(IceConnectionState::Connected | IceConnectionState::Completed)
        )
}

/// Apply the ICE state `poll_output` just yielded, then whether the
/// establishment sweep drops the peer. The event loop polls before that
/// sweep, so a `Connected` queued at [`PEER_ESTABLISH_TIMEOUT`] is visible
/// and the peer stays. The loop does this inline; the function is what the
/// edge test calls.
#[cfg(test)]
fn establishment_expired_after_poll(
    peer: &mut SfuPeer,
    now: Instant,
    polled_ice: Option<IceConnectionState>,
) -> bool {
    if let Some(state) = polled_ice {
        peer.on_ice_state(state, now);
    }
    peer.never_established(now) || peer.stayed_disconnected(now)
}

/// Why [`SfuPeer::accept_server_answer`] rejected an answer.
#[derive(Debug)]
enum ServerAnswerError {
    /// This peer has no offer waiting.
    NoPending,
    /// The JSON did not parse. The pending negotiation is unchanged.
    Parse(String),
    /// The answer does not name the pending offer, or its m-lines are not
    /// that offer's. The token stays.
    NotThisOffer,
    /// The answer crossed another offer. str0m has already dropped the token.
    OutOfOrder,
    /// str0m took the token and rejected the answer locally.
    Rejected(String),
}

impl std::fmt::Display for ServerAnswerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoPending => write!(f, "no pending offer"),
            Self::Parse(error) => write!(f, "answer parse failed: {error}"),
            Self::NotThisOffer => write!(f, "answer does not belong to the pending offer"),
            Self::OutOfOrder => write!(f, "answer crossed another offer"),
            Self::Rejected(error) => write!(f, "accept_answer failed: {error}"),
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
            .field("pending_offer", &self.pending.is_some())
            .field(
                "pending_resends",
                &self.pending.as_ref().map(|pending| pending.resends),
            )
            .field(
                "pending_deadline",
                &self.pending.as_ref().map(|pending| pending.next_deadline),
            )
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
        /// The offer this answer names. `None` is a pipe: the cascade wire
        /// has no offer id, so the pending offer's id is used.
        offer_id: Option<String>,
        sdp_answer_json: String,
        /// The RPC waits on this. A pipe answer has none.
        reply: Option<tokio::sync::oneshot::Sender<SfuAnswerResult>>,
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
    /// The client's socket came back (`sfu.callResume`): publish the pending
    /// server offer, if any, again at once, and answer whether this session
    /// is still in the loop.
    Resume {
        participant_id: ParticipantId,
        reply: tokio::sync::oneshot::Sender<ResumeLoopReply>,
    },
    /// Query the quality preferences map.  Returns a snapshot via
    /// oneshot so callers outside the event loop can read it.
    GetQualityPreferences {
        reply: tokio::sync::oneshot::Sender<HashMap<ParticipantId, String>>,
    },
    /// Shut down the SFU server.
    Shutdown,
}

/// What the event loop answers to `SfuCommand::Resume`. The service adds
/// the room roster; the offer, when there is one, is the pending server offer.
#[derive(Debug)]
pub struct ResumeLoopReply {
    pub listed: bool,
    pub offer: Option<SfuCallRenegotiationOffer>,
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
    /// Why the peer was dropped: `timeout` when it never connected,
    /// `drop` when it stayed disconnected or died otherwise, `offer` when
    /// an established pipe's answer was rejected locally or its offer went
    /// unanswered. `offer` makes the cascade count a failure before it dials
    /// that pipe again. A `drop` does the same on the end that dials. The
    /// first consecutive failure still dials at once.
    pub reason: &'static str,
    /// Whether a connected stretch reached
    /// [`super::cascade::PIPE_HEALTHY_RESET`] before this death. Recorded
    /// when ICE went `Disconnected`, so the drop after the grace still
    /// knows, and a disconnected period is not counted. An offer-ended
    /// pipe that is still connected reports the open stretch. A client
    /// peer leaves this false. Cleanup resets the redial count when it
    /// is true.
    pub healthy_stretch: bool,
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
    /// `advertised` = `(public, base)` as a server-reflexive one. `base` is
    /// the socket the public mapping was learned on (the router forwards to
    /// it, [`MediaSockets::primary`]); it is offered as a host too, even after
    /// the LAN address moved on, since a caller's packets to the public
    /// address arrive addressed to it and str0m matches them to its host
    /// candidate.
    pub fn create_rtc_for_offer(
        offer: SdpOffer,
        hosts: &[SocketAddr],
        advertised: Option<(SocketAddr, SocketAddr)>,
    ) -> Result<(Rtc, String), String> {
        // A client's downlink is estimated (TWCC) so its video layers can
        // follow it; see `simulcast`. Pipes, which this does not build, do not.
        let mut rtc = Rtc::builder()
            .set_stats_interval(Some(PEER_STATS_INTERVAL))
            .enable_bwe(Some(Bitrate::kbps(BWE_START_KBPS)))
            .build(Instant::now());

        if hosts.is_empty() {
            return Err("The SFU has no address to offer".to_string());
        }
        let base = advertised.map(|(_, base)| base);
        let extra = base.filter(|base| !hosts.contains(base));
        for host in hosts.iter().chain(extra.iter()) {
            let candidate = Candidate::host(*host, "udp")
                .map_err(|e| format!("Failed to create host candidate: {}", e))?;
            rtc.add_local_candidate(candidate);
        }
        if let Some((public, base)) = advertised {
            let candidate = Candidate::server_reflexive(public, base, "udp")
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
        // A command that ended the wait below; the drain at the top handles it.
        let mut woken: Option<SfuCommand> = None;

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
            // Process commands: the one that ended the wait first, then
            // whatever else is queued.
            loop {
                let next = woken
                    .take()
                    .map(Ok)
                    .unwrap_or_else(|| command_rx.try_recv());
                match next {
                    Ok(SfuCommand::AddPeer(peer)) => {
                        let pid = peer.id.clone();
                        let room_id = peer.room_id.clone();
                        info!(
                            "SFU: peer {} (DID: {}) joined room {}",
                            pid, peer.agent_did, room_id
                        );

                        // Existing origins are offered when a pipe becomes usable
                        // (ICE connected, or its control channel opens), not here.
                        if !peer.is_pipe {
                            layers.set_bwe(&pid);
                        }
                        peers.insert(pid, peer);
                    }
                    Ok(SfuCommand::RemovePeer(pid)) => {
                        if let Some(peer) = peers.remove(&pid) {
                            let room_id = peer.room_id.clone();
                            info!("SFU: peer {} left room {}", pid, room_id);
                            relay.remove_participant(&pid);
                            quality_preferences.remove(&pid);
                            layers.forget(Instant::now(), &pid);
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
                        // A peer that has left keeps nothing: ids are never
                        // reused, so state made for it would never be freed.
                        if peers.contains_key(&participant_id) {
                            // The layer selector switches on the next packets
                            // and asks the senders for the keyframes it needs.
                            layers.set_ceiling(Instant::now(), &participant_id, &preference);
                            quality_preferences.insert(participant_id, preference);
                        }
                    }
                    Ok(SfuCommand::ApplyServerAnswer {
                        participant_id,
                        offer_id,
                        sdp_answer_json,
                        reply,
                    }) => {
                        let verdict = if let Some(peer) = peers.get_mut(&participant_id) {
                            // A pipe answer names no offer id: the cascade wire
                            // has none, so the pending offer's id is used. A
                            // client answer names the offer it belongs to.
                            let id = offer_id.unwrap_or_else(|| {
                                peer.pending
                                    .as_ref()
                                    .map(|pending| pending.offer_id.clone())
                                    .unwrap_or_default()
                            });
                            // Settled before the deadline sweep below: an answer
                            // in the tick of a resend cancels that resend.
                            peer.settle_answer(&id, &sdp_answer_json)
                            // Deferred tracks drain AFTER poll_output — see
                            // the post-poll sweep below.  Draining here would
                            // call sdp_api().apply() before str0m has
                            // processed the accept_answer, producing offers
                            // that never materialise into working media.
                        } else {
                            debug!(
                                "SFU: ApplyServerAnswer for unknown participant {}",
                                participant_id
                            );
                            SfuAnswerResult::refused(SfuAnswerRefusal::UnknownSession)
                        };
                        if let Some(reply) = reply {
                            let _ = reply.send(verdict);
                        }
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
                        apply_pipe_renegotiation_offer(
                            peer,
                            &sdp_offer_json,
                            remote_did,
                            room_id,
                            &track_mapping,
                        );
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
                        match parse_trickle_candidate(&candidate_sdp) {
                            Ok(None) => {
                                debug!("SFU: end-of-candidates for {}", participant_id);
                            }
                            Ok(Some(candidate)) => {
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
                        peer.restart_ice(Instant::now());
                    }
                    Ok(SfuCommand::Resume {
                        participant_id,
                        reply,
                    }) => {
                        let report = if let Some(peer) = peers.get_mut(&participant_id) {
                            resume_loop_reply(peer, Instant::now())
                        } else {
                            debug!("SFU: Resume for unknown participant {}", participant_id);
                            ResumeLoopReply {
                                listed: false,
                                offer: None,
                            }
                        };
                        let _ = reply.send(report);
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

            // An unanswered offer is published again from its cached SDP.
            // A client peer is never dropped for it. An established pipe
            // whose offer is still unanswered after the second resend is
            // disconnected here; the sweep after poll removes it, and the
            // cascade redials. A resend is published after that sweep: the
            // token stays pending, and the deadline has already moved, so
            // this tick publishes it once.
            let now = Instant::now();
            let due_resends = sweep_offer_deadlines(&mut peers, now);
            // A pending upgrade whose hold ended, or whose layer is measured.
            layers.run_due(now);

            // Poll all peers for output
            let mut earliest_timeout = Instant::now() + Duration::from_millis(100);
            let mut media_to_relay: Vec<(ParticipantId, MediaData)> = Vec::new();
            let mut tracks_opened: Vec<(ParticipantId, Mid, MediaKind)> = Vec::new();
            // Pipe peers that reached ICE Connected/Completed, or opened a
            // channel, this tick. A control-only pipe never opens an inbound
            // track, so this is what seeds it with the room's existing origins.
            let mut pipes_usable: Vec<ParticipantId> = Vec::new();
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
                                if peer.is_pipe && pipe_event_is_usable(Some(state), false) {
                                    pipes_usable.push(pid.clone());
                                }
                            }
                            Event::MediaAdded(e) => {
                                info!("SFU: peer {} added {:?} track mid={}", pid, e.kind, e.mid);
                                peer.tracks_in.insert(e.mid, e.kind);
                                tracks_opened.push((pid.clone(), e.mid, e.kind));
                            }
                            Event::MediaData(data) => {
                                peer.note_media(Instant::now());
                                media_to_relay.push((pid.clone(), data));
                            }
                            Event::KeyframeRequest(req) => {
                                keyframe_requests.push((pid.clone(), req));
                            }
                            Event::EgressBitrateEstimate(BweKind::Twcc {
                                estimate,
                                can_probe,
                            }) => {
                                // REMB is ignored: the budget follows TWCC.
                                layers.set_estimate(
                                    Instant::now(),
                                    pid,
                                    estimate.as_u64(),
                                    can_probe,
                                );
                            }
                            Event::ChannelOpen(cid, label) => {
                                info!(
                                    "SFU: peer {} opened data channel '{}' (id {:?})",
                                    pid, label, cid
                                );
                                peer.channels.insert(cid, label);
                                if peer.is_pipe && pipe_event_is_usable(None, true) {
                                    peer.note_usable(Instant::now());
                                    pipes_usable.push(pid.clone());
                                }
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
                            Event::PeerStats(stats) => {
                                // The layer of each video track this peer receives.
                                // One line per peer per interval: debug, not info.
                                // A timer must not sit on info.
                                let layers_in: Vec<Option<Rid>> = peer
                                    .tracks_out
                                    .iter()
                                    .filter(|(out_mid, _)| {
                                        peer.rtc
                                            .media(**out_mid)
                                            .is_some_and(|media| media.kind().is_video())
                                    })
                                    .map(|(_, track)| layers.layer_of(pid, track))
                                    .collect();
                                debug!(
                                    "{}",
                                    peer.stats_line(
                                        &stats,
                                        &layers_in,
                                        quality_preferences.get(pid).map(String::as_str),
                                        Instant::now(),
                                    )
                                );
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

            // Clean out disconnected peers after poll, and tell the service:
            // a client still holds a room slot (capacity, and its DID could
            // not rejoin), a pipe a cascade entry. Poll runs first so a
            // `Connected` queued at `PEER_ESTABLISH_TIMEOUT` is applied
            // before `never_established` (see
            // `establishment_expired_after_poll`). A pipe ended for its
            // offer is removed here, before deferred tracks drain, so the
            // dying peer does not publish a replacement offer.
            {
                let mut dead_peers: Vec<DeadPeer> = Vec::new();
                let now = Instant::now();
                end_expired_peers(&mut peers, now);
                peers.retain(|pid, peer| {
                    if !peer.rtc.is_alive() {
                        info!("SFU: peer {} disconnected", pid);
                        relay.remove_participant(pid);
                        quality_preferences.remove(pid);
                        layers.forget(now, pid);
                        dead_peers.push(peer.dead_peer(now));
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
                let keyframe = is_keyframe(&data.codec_extra);
                layers.observe(
                    now,
                    &track,
                    data.rid.as_ref(),
                    data.data.len(),
                    // The frame's RTP packets (extended sequence numbers).
                    (**data.seq_range.end()).saturating_sub(**data.seq_range.start()) + 1,
                    data.params.spec().codec.is_audio(),
                );
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
                        let route =
                            layers.route(now, target_pid, &track, data.rid.as_ref(), keyframe);
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

            // Republish offers whose deadline fired before this poll. The
            // peer may have been removed since, by the disconnect sweep.
            for due in due_resends {
                let Some(peer) = peers.get(&due.pid) else {
                    continue;
                };
                if !peer.rtc.is_alive() {
                    continue;
                }
                publish_renegotiation_offer(
                    peer,
                    &due.pid,
                    &due.offer_id,
                    due.sdp,
                    due.track_mapping,
                );
            }

            // Drain deferred tracks for peers whose pending offer just
            // cleared.  Running here — AFTER poll_output and the dead-peer
            // sweep — ensures str0m has fully processed the previous
            // accept_answer before we create a new SDP offer via
            // sdp_api().apply().  A resend does not clear the pending
            // offer, so it does not drain here. A crossed offer does: its
            // tracks were queued again and the pipe is still here. A pipe
            // ended for its offer was removed above and does not.
            for (pid, peer) in peers.iter_mut() {
                if peer.pending.is_none() && peer.has_deferred_changes() {
                    if let Some((sdp_offer_json, track_mapping)) =
                        apply_deferred_tracks(peer, Instant::now())
                    {
                        let offer_id = peer
                            .pending
                            .as_ref()
                            .expect("the deferred offer was installed")
                            .offer_id
                            .clone();
                        publish_renegotiation_offer(
                            peer,
                            pid,
                            &offer_id,
                            sdp_offer_json,
                            track_mapping,
                        );
                    }
                }
            }

            let adds_per_target = outbound_adds(&mut peers, &tracks_opened, &pipes_usable);

            for (target_pid, sdp_offer_json, track_mapping) in
                offer_new_tracks(&mut peers, adds_per_target)
            {
                if let Some(target_peer) = peers.get(&target_pid) {
                    let offer_id = target_peer
                        .pending
                        .as_ref()
                        .expect("the new-track offer was installed")
                        .offer_id
                        .clone();
                    publish_renegotiation_offer(
                        target_peer,
                        &target_pid,
                        &offer_id,
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

            // What each receiver's transport should probe for.
            for (pid, bps) in layers.take_demands() {
                if let Some(peer) = peers.get_mut(&pid) {
                    peer.rtc.bwe().set_desired_bitrate(Bitrate::bps(bps));
                }
            }
            // Wake for the earliest pending upgrade: the estimator is silent
            // on a steady link, so no event would run it.
            if let Some(wake) = layers.next_wake() {
                earliest_timeout = earliest_timeout.min(wake);
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
                command = command_rx.recv() => {
                    // A command ends the wait at once instead of at the next
                    // packet or timer.
                    match command {
                        Some(command) => woken = Some(command),
                        None => {
                            info!("SFU command channel closed, shutting down");
                            return;
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
/// track_mapping)` or `None` when nothing actionable remains, or when an
/// offer is already pending (that offer is resent, not replaced). The
/// offer's id is on the pending negotiation.
fn apply_deferred_tracks(
    peer: &mut SfuPeer,
    now: Instant,
) -> Option<(String, Vec<crate::sfu::TrackMapEntry>)> {
    if peer.pending.is_some() {
        return None;
    }
    let restart = peer.ice_restart.is_some();
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
    let (offer, token, mids) = offer_send_only(&mut peer.rtc, &kinds, restart)?;
    let entries = mids
        .into_iter()
        .zip(to_add)
        .map(|(new_mid, (origin_pid, origin_mid, kind, origin))| {
            (new_mid, kind, origin_pid, origin_mid, origin)
        })
        .collect();
    commit_offer(peer, offer, token, entries, now, restart)
}

/// Install mappings and the pending negotiation only after `offer` serialises,
/// so a serialisation failure leaves no half-built negotiation.
fn commit_offer(
    peer: &mut SfuPeer,
    offer: SdpOffer,
    token: str0m::change::SdpPendingOffer,
    entries: Vec<(Mid, MediaKind, ParticipantId, Mid, TrackOrigin)>,
    now: Instant,
    carries_restart: bool,
) -> Option<(String, Vec<crate::sfu::TrackMapEntry>)> {
    let sdp = match serde_json::to_string(&offer) {
        Ok(sdp) => sdp,
        Err(e) => {
            warn!(
                "SFU: serialise renegotiation offer for {} failed: {}",
                peer.id, e
            );
            return None;
        }
    };
    let mut track_mapping = Vec::with_capacity(entries.len());
    for (new_mid, kind, origin_pid, origin_mid, origin) in entries {
        track_mapping.push(mapping_entry(new_mid, kind, &origin));
        peer.tracks_out
            .insert(new_mid, (origin_pid.clone(), origin_mid));
        peer.tracks_out_rev
            .insert((origin_pid, origin_mid), new_mid);
    }
    peer.install_pending(
        token,
        sdp.clone(),
        track_mapping.clone(),
        now,
        carries_restart,
    );
    Some((sdp, track_mapping))
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

/// Outbound tracks to offer this tick: `(target, [(kind, origin peer, origin mid)])`.
///
/// When peer X opens an inbound track, every other peer in the room is
/// offered it, and X is offered every other peer's existing inbound tracks.
/// `pipes_usable` names pipe peers that became usable this tick (ICE
/// connected, or the control channel opened). A control-only pipe opens no
/// inbound track, so without it a dialled pipe never takes that second path
/// and media waits for some later track.
fn outbound_adds(
    peers: &mut HashMap<ParticipantId, SfuPeer>,
    tracks_opened: &[(ParticipantId, Mid, MediaKind)],
    pipes_usable: &[ParticipantId],
) -> HashMap<ParticipantId, Vec<(MediaKind, ParticipantId, Mid)>> {
    let mut adds_per_target: HashMap<ParticipantId, Vec<(MediaKind, ParticipantId, Mid)>> =
        HashMap::new();
    let mut new_peers: HashSet<ParticipantId> = HashSet::new();
    for (origin_pid, _, _) in tracks_opened {
        new_peers.insert(origin_pid.clone());
    }

    // (a) Each freshly-opened track goes outbound on every other peer in the room.
    for (origin_pid, mid, kind) in tracks_opened {
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

    // A control-only pipe opens no inbound track, so it is not in
    // `tracks_opened`. Once it is usable, offer it the origins already in
    // the room, once. A later track uses path (a).
    for pid in pipes_usable {
        let Some(peer) = peers.get_mut(pid) else {
            continue;
        };
        if !peer.is_pipe || peer.origins_seeded {
            continue;
        }
        peer.origins_seeded = true;
        new_peers.insert(pid.clone());
    }

    // (b) A peer that just opened a track, or a pipe that just became
    // usable, also receives everyone else's existing inbound tracks.
    // Tracks opened this tick are already in (a).
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

    adds_per_target
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
        if target_peer.pending.is_some() {
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
        let Some((offer, token, mids)) = offer_send_only(&mut target_peer.rtc, &kinds, false)
        else {
            continue;
        };
        let entries: Vec<_> = mids
            .into_iter()
            .zip(to_add)
            .map(|(new_mid, (kind, origin_pid, origin_mid, origin))| {
                (new_mid, kind, origin_pid, origin_mid, origin.clone())
            })
            .collect();
        let m_lines = entries.len();
        let Some((sdp_offer_json, track_mapping)) =
            commit_offer(target_peer, offer, token, entries, Instant::now(), false)
        else {
            continue;
        };
        let offer_id = target_peer
            .pending
            .as_ref()
            .map(|pending| pending.offer_id.as_str())
            .unwrap_or("");
        info!(
            "SFU: peer {} renegotiation offer {offer_id} prepared ({} new outbound m-lines)",
            target_pid, m_lines
        );
        offers.push((target_pid, sdp_offer_json, track_mapping));
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
/// Returns the client payload when the peer is a client, so a resume reply
/// can carry the same value the event does.
fn publish_renegotiation_offer(
    peer: &SfuPeer,
    pid: &ParticipantId,
    offer_id: &str,
    sdp_offer_json: String,
    track_mapping: Vec<crate::sfu::TrackMapEntry>,
) -> Option<SfuCallRenegotiationOffer> {
    let client_offer = if peer.is_pipe {
        let payload = crate::sfu::SfuPipeRenegotiationOffer {
            remote_did: peer.agent_did.clone(),
            room_id: peer.room_id.to_string(),
            sdp_offer: sdp_offer_json,
            track_mapping,
        };
        super::pipe_bus::publish_offer(payload);
        None
    } else {
        let payload = SfuCallRenegotiationOffer {
            target_did: peer.agent_did.clone(),
            participant_id: pid.to_string(),
            offer_id: offer_id.to_string(),
            neighbourhood_url: peer.room_id.neighbourhood_url.clone(),
            room_name: peer.room_id.room_name.clone(),
            sdp_offer: sdp_offer_json,
            track_mapping,
        };
        crate::pubsub::get_global_pubsub_sync()
            .publish_event(crate::events::Event::sfu_renegotiation_offer(&payload));
        Some(payload)
    };
    if let Some(pending) = &peer.pending {
        let kind = if pending.resends == 0 {
            "initial"
        } else {
            "retry"
        };
        let age_ms = Instant::now()
            .saturating_duration_since(pending.sent_at)
            .as_millis();
        let attempt = pending.resends;
        let fingerprint = sdp_fingerprint(&pending.sdp);
        // A resend is on a timer. The first publication of an offer is not.
        if attempt == 0 {
            info!(
                "SFU: renegotiation offer {kind} {offer_id} for peer {pid} attempt {attempt} age_ms {age_ms} sdp {fingerprint:016x}"
            );
        } else {
            debug!(
                "SFU: renegotiation offer {kind} {offer_id} for peer {pid} attempt {attempt} age_ms {age_ms} sdp {fingerprint:016x}"
            );
        }
    }
    client_offer
}

/// What `sfu.callResume` answers for a peer the loop still holds. A pipe is
/// not a client session. A client whose RTC has already closed is not
/// listed: the disconnect sweep removes it after this command drain, so a
/// resume in the same tick would otherwise report it as resumable.
fn resume_loop_reply(peer: &mut SfuPeer, now: Instant) -> ResumeLoopReply {
    if peer.is_pipe {
        warn!("SFU: Resume for pipe {} ignored", peer.id);
        return ResumeLoopReply {
            listed: false,
            offer: None,
        };
    }
    if !peer.rtc.is_alive() {
        debug!(
            "SFU: Resume for {} ignored; its RTC is already closed",
            peer.id
        );
        return ResumeLoopReply {
            listed: false,
            offer: None,
        };
    }
    let pid = peer.id.clone();
    let offer = match peer.resume_offer(now) {
        Some((offer_id, sdp, track_mapping)) => {
            info!("SFU: peer {pid} resumed; pending offer {offer_id} sent again");
            publish_renegotiation_offer(peer, &pid, &offer_id, sdp, track_mapping)
        }
        None => {
            info!("SFU: peer {pid} resumed; no offer pending");
            None
        }
    };
    ResumeLoopReply {
        listed: true,
        offer,
    }
}

/// Apply a remote pipe's renegotiation offer. `accept_offer` bumps str0m's
/// change id before it validates the SDP, so the local pending offer is
/// dead whether the remote offer is taken or rejected locally: drop the
/// token and queue its tracks again on this same pipe. A rejection here
/// only logs. A pipe answer carries no refusal, so the sender keeps
/// waiting until its offer goes unanswered. A JSON parse failure does not
/// touch str0m, so that offer stays pending. A crossed offer
/// (`ChangesOutOfOrder`) does not end the pipe.
fn apply_pipe_renegotiation_offer(
    peer: &mut SfuPeer,
    sdp_offer_json: &str,
    remote_did: String,
    room_id: String,
    track_mapping: &[crate::sfu::TrackMapEntry],
) {
    let offer: SdpOffer = match serde_json::from_str(sdp_offer_json) {
        Ok(offer) => offer,
        Err(e) => {
            warn!(
                "SFU: ApplyPipeRenegotiationOffer parse failed for {}: {}",
                peer.id, e
            );
            return;
        }
    };
    let answer = match peer.rtc.sdp_api().accept_offer(offer) {
        Ok(answer) => answer,
        Err(e) => {
            warn!("SFU: accept_offer for pipe {} failed: {:?}", peer.id, e);
            peer.requeue_invalidated_offer();
            return;
        }
    };
    // Only once the offer is in the session: an offer rejected locally names nothing.
    peer.note_remote_origins(track_mapping);
    // The local token was invalidated by accept_offer. Its tracks were
    // never negotiated.
    peer.requeue_invalidated_offer();
    match serde_json::to_string(&answer) {
        Ok(answer_json) => {
            let payload = crate::sfu::SfuPipeRenegotiationAnswer {
                remote_did,
                room_id,
                sdp_answer: answer_json,
            };
            super::pipe_bus::publish_answer(payload);
            info!("SFU: pipe renegotiation answer published for {}", peer.id);
        }
        Err(e) => {
            warn!(
                "SFU: serialize pipe renegotiation answer for {} failed: {}",
                peer.id, e
            );
        }
    }
}

/// Ordered `a=mid` values in SDP text.
fn sdp_mid_list(sdp: &str) -> Vec<String> {
    sdp.lines()
        .filter_map(|line| line.trim().strip_prefix("a=mid:").map(str::to_string))
        .collect()
}

/// The SDP text stored in a pending offer's JSON. Empty when it does not parse.
fn stored_offer_sdp_text(sdp_json: &str) -> String {
    serde_json::from_str::<serde_json::Value>(sdp_json)
        .ok()
        .and_then(|value| {
            value
                .get("sdp")
                .and_then(|sdp| sdp.as_str())
                .map(str::to_string)
        })
        .unwrap_or_default()
}

/// A stable-enough fingerprint of an offer, so a log can tell a resend of
/// the same SDP from a new one without writing the SDP itself.
fn sdp_fingerprint(sdp: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    sdp.hash(&mut hasher);
    hasher.finish()
}

/// One cached offer whose deadline fired on this tick.
struct DueResend {
    pid: ParticipantId,
    offer_id: String,
    sdp: String,
    track_mapping: Vec<crate::sfu::TrackMapEntry>,
}

/// Name and end every peer whose ICE time has run out. A peer already ended
/// keeps its reason: an offer that expired on the tick its disconnect grace
/// ran out stays `"offer"`, so the cascade still counts the failure.
fn end_expired_peers(peers: &mut HashMap<ParticipantId, SfuPeer>, now: Instant) {
    for (pid, peer) in peers.iter_mut() {
        if peer.end_reason.is_some() {
            continue;
        }
        if peer.never_established(now) {
            info!(
                "SFU: peer {} never connected within {} s, dropping it",
                pid,
                PEER_ESTABLISH_TIMEOUT.as_secs()
            );
            peer.end_reason = Some("timeout");
            peer.rtc.disconnect();
        } else if peer.stayed_disconnected(now) {
            info!("SFU: peer {} stayed disconnected, dropping it", pid);
            peer.end_reason = Some("drop");
            peer.rtc.disconnect();
        }
    }
}

/// Advance every pending offer's deadline by one step. A resend is returned
/// for publication after the dead-peer sweep. An established pipe whose
/// offer is still unanswered after the second resend is disconnected here.
/// The sweep removes it before deferred tracks drain, so those tracks are
/// not offered again on the pipe that is going away.
fn sweep_offer_deadlines(
    peers: &mut HashMap<ParticipantId, SfuPeer>,
    now: Instant,
) -> Vec<DueResend> {
    let mut due = Vec::new();
    for (pid, peer) in peers.iter_mut() {
        match peer.on_offer_deadline(now) {
            OfferDeadline::Wait => {}
            OfferDeadline::Resend => {
                let pending = peer
                    .pending
                    .as_ref()
                    .expect("a resend leaves the offer pending");
                due.push(DueResend {
                    pid: pid.clone(),
                    offer_id: pending.offer_id.clone(),
                    sdp: pending.sdp.clone(),
                    track_mapping: pending.track_mapping.clone(),
                });
            }
            OfferDeadline::End => {
                peer.end_for_offer("unanswered");
            }
        }
    }
    due
}

/// A trickled ICE candidate string.
///
/// `Ok(None)` is end-of-candidates: empty, whitespace, or `candidate:` with
/// an empty body. A nonempty string that is not a candidate is an error,
/// including one that lacks the `candidate:` prefix.
pub fn parse_trickle_candidate(sdp: &str) -> Result<Option<Candidate>, str0m::error::IceError> {
    let trimmed = sdp.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    if let Some(body) = trimmed.strip_prefix("candidate:") {
        if body.trim().is_empty() {
            return Ok(None);
        }
    }
    Candidate::from_sdp_string(trimmed).map(Some)
}

#[cfg(test)]
mod tests {
    use super::super::cascade::{CascadeManager, CascadeSignal};
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
        let (_, mapping) =
            apply_deferred_tracks(&mut peer, Instant::now()).expect("the track is offered");
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
        client.pending = None;
        let (_, mapping) =
            apply_deferred_tracks(client, Instant::now()).expect("the deferred tracks are offered");
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
            SfuServer::create_rtc_for_offer(caller_offer(), &[local], Some((public, local)))
                .unwrap();
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

    /// The receiver's bandwidth estimate comes from the caller's transport-wide
    /// congestion control reports: its answer must accept both the feedback
    /// type and the header extension for video.
    #[test]
    fn a_video_answer_negotiates_transport_wide_congestion_control() {
        let local: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Video,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let offer = change.apply().unwrap().0;
        let (_, answer) = SfuServer::create_rtc_for_offer(offer, &[local], None).unwrap();
        let answer: serde_json::Value = serde_json::from_str(&answer).unwrap();
        let sdp = answer["sdp"].as_str().unwrap();
        assert!(
            sdp.contains("transport-cc"),
            "no transport-cc feedback: {sdp}"
        );
        assert!(
            sdp.contains("transport-wide-cc-extensions"),
            "no transport-wide-cc extension: {sdp}"
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
        /// `PeerStats` events, one per [`PEER_STATS_INTERVAL`] when enabled.
        peer_stats: usize,
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
                Output::Event(Event::PeerStats(_)) => seen.peer_stats += 1,
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
            SfuServer::create_rtc_for_offer(offer, &[bind], Some((public, bind))).unwrap();
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

    /// The LAN address moved on after startup, but the router still forwards
    /// the public address to the startup socket (a second interface took the
    /// default route). The answer's srflx candidate keeps the startup socket
    /// as its base, and offers it as a host, so a caller that reaches the
    /// SFU only through the forward connects.
    #[test]
    fn a_caller_through_the_forward_connects_after_the_lan_moved_on() {
        let startup: SocketAddr = "192.168.0.10:40000".parse().unwrap();
        let loopback: SocketAddr = "127.0.0.1:40000".parse().unwrap();
        let moved: SocketAddr = "10.0.0.5:40000".parse().unwrap();
        let public: SocketAddr = "198.51.100.7:40000".parse().unwrap();
        let caller_addr: SocketAddr = "203.0.113.20:50000".parse().unwrap();

        let mut caller = Rtc::builder().build(Instant::now());
        caller.add_local_candidate(Candidate::host(caller_addr, "udp").unwrap());
        let mut change = caller.sdp_api();
        let cid = change.add_channel("sfu-data".into());
        let (offer, pending) = change.apply().unwrap();
        let (mut sfu, answer) =
            SfuServer::create_rtc_for_offer(offer, &[loopback, moved], Some((public, startup)))
                .unwrap();
        assert!(
            candidates(&answer)
                .iter()
                .any(|l| l.contains("192.168.0.10 40000 typ host")),
            "the startup socket is offered: {:?}",
            candidates(&answer)
        );
        let answer: str0m::change::SdpAnswer = serde_json::from_str(&answer).unwrap();
        caller.sdp_api().accept_answer(pending, answer).unwrap();

        let start = Instant::now();
        let mut now = start;
        let (mut caller_seen, mut sfu_seen) = (Seen::default(), Seen::default());
        let mut next_write = Duration::ZERO;
        while now - start < Duration::from_secs(10) {
            let mut sent = Vec::new();
            let caller_next = drive(&mut caller, now, start, &mut caller_seen, &mut sent);
            for (_, destination, data) in sent.drain(..) {
                // Only the forward reaches the SFU, at the startup socket.
                if destination == public {
                    receive(&mut sfu, now, caller_addr, startup, &data);
                }
            }
            let sfu_next = drive(&mut sfu, now, start, &mut sfu_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                if source == startup && destination == caller_addr {
                    receive(&mut caller, now, public, caller_addr, &data);
                }
            }
            if now - start >= next_write {
                if let Some(mut ch) = caller.channel(cid) {
                    ch.write(true, b"caller").unwrap();
                }
                next_write += Duration::from_millis(500);
            }
            let next = caller_next.min(sfu_next).min(start + next_write);
            now = next.clamp(
                now + Duration::from_millis(1),
                now + Duration::from_millis(50),
            );
        }
        assert!(
            sfu_seen.ice.iter().any(|(_, s)| matches!(
                s,
                IceConnectionState::Connected | IceConnectionState::Completed
            )),
            "the SFU never connected through the forward: {:?}",
            sfu_seen.ice
        );
        assert!(
            !sfu_seen.data.is_empty(),
            "the SFU received no data through the forward"
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
                peer.restart_ice(now);
                let (json, _) =
                    apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
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
                peer.accept_current_answer(&answer).unwrap();
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

    /// The restart waits behind an offer already pending. The deadline
    /// resends that offer. The restart is offered once the answer lands,
    /// and a further request does not publish a second offer.
    #[test]
    fn an_ice_restart_waits_for_a_pending_offer_and_is_offered_once() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (track_sdp, _, _) = offer_outbound_audio(&mut peer, now);
        peer.restart_ice(now);
        assert!(peer.has_deferred_changes(), "the restart waits");
        assert!(!peer.pending.as_ref().unwrap().carries_restart);

        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Resend
        ));
        assert_eq!(peer.pending.as_ref().unwrap().sdp, track_sdp);
        assert!(peer.has_deferred_changes(), "the restart is still waiting");

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&track_sdp).unwrap())
            .unwrap();
        peer.accept_current_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        let (restart_sdp, _) =
            apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        assert!(restart_sdp.contains("ice-ufrag"), "{restart_sdp}");
        assert!(peer.pending.as_ref().unwrap().carries_restart);
        assert!(!peer.has_deferred_changes());

        peer.restart_ice(now);
        assert!(peer.ice_restart.as_ref().unwrap().again);
        assert!(apply_deferred_tracks(&mut peer, now).is_none());
        assert_eq!(peer.pending.as_ref().unwrap().sdp, restart_sdp);
    }

    /// A second request while the restart's offer waits sets a follow-up
    /// restart and does not publish immediately. Once the answer lands a
    /// fresh restart follows, with new ICE credentials for the newest
    /// network. The resend of the offer in hand uses the offer deadline.
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
        peer.restart_ice(now);
        let (first, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        peer.restart_ice(now);
        assert!(peer.ice_restart.as_ref().unwrap().again);
        assert_eq!(peer.pending.as_ref().unwrap().sdp, first);
        assert!(matches!(peer.on_offer_deadline(now), OfferDeadline::Wait));
        assert!(!peer.has_deferred_changes());

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&first).unwrap())
            .unwrap();
        peer.accept_current_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(peer.has_deferred_changes(), "the second request is pending");
        let (second, _) = apply_deferred_tracks(&mut peer, now).expect("a fresh restart");
        assert_ne!(ufrag(&second), ufrag(&first));

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&second).unwrap())
            .unwrap();
        peer.accept_current_answer(&serde_json::to_string(&answer).unwrap())
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
        peer.restart_ice(lost_at + ICE_DISCONNECT_GRACE);
        let (offer, _) = apply_deferred_tracks(&mut peer, lost_at + ICE_DISCONNECT_GRACE)
            .expect("the restart is offered");
        assert!(
            !peer.stayed_disconnected(now),
            "the restart extends the grace"
        );

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&offer).unwrap())
            .unwrap();
        peer.accept_current_answer(&serde_json::to_string(&answer).unwrap())
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
        peer.restart_ice(now);
        // Before the offer exists nothing is held: the restart has not begun.
        peer.add_remote_candidate(candidate(49_999));
        assert!(peer.ice_restart.as_ref().unwrap().candidates.is_empty());

        let (offer, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        for port in 0..(MAX_HELD_CANDIDATES as u16 + 5) {
            peer.add_remote_candidate(candidate(50_000 + port));
        }
        assert_eq!(
            peer.ice_restart.as_ref().unwrap().candidates.len(),
            MAX_HELD_CANDIDATES
        );

        // A resend keeps the offer, and the candidates held for its answer.
        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Resend
        ));
        assert_eq!(peer.pending.as_ref().unwrap().sdp, offer);
        assert_eq!(
            peer.ice_restart.as_ref().unwrap().candidates.len(),
            MAX_HELD_CANDIDATES
        );

        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(&offer).unwrap())
            .unwrap();
        peer.accept_current_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(peer.ice_restart.is_none(), "handed over with the answer");
    }

    /// An ICE restart that is never answered is resent, always as the same
    /// offer. There is no replacement offer, and the resends do not drop the
    /// peer: the restart grace does, if ICE stays down.
    #[test]
    fn an_unanswered_ice_restart_is_resent_without_a_new_offer() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.restart_ice(now);
        let (first, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        let since = peer.ice_restart.as_ref().unwrap().since;
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);

        let first_resend = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(first_resend.len(), 1);
        assert_eq!(first_resend[0].sdp, first);
        assert!(sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE).is_empty());

        let second_resend = sweep_offer_deadlines(&mut peers, now + Duration::from_secs(30));
        assert_eq!(second_resend.len(), 1);
        assert_eq!(second_resend[0].sdp, first);
        assert_eq!(peers[&pid].ice_restart.as_ref().unwrap().since, since);

        let third_resend = sweep_offer_deadlines(&mut peers, now + Duration::from_secs(60));
        assert_eq!(third_resend.len(), 1);
        assert_eq!(third_resend[0].sdp, first);
        assert!(peers[&pid].rtc.is_alive());
        assert!(apply_deferred_tracks(peers.get_mut(&pid).unwrap(), now).is_none());
    }

    /// Addresses the in-memory caller and the SFU bind to. No socket is open.
    fn pair_addrs() -> (SocketAddr, SocketAddr) {
        (
            "192.168.0.10:40000".parse().unwrap(),
            "203.0.113.20:50000".parse().unwrap(),
        )
    }

    /// A caller that has joined, and the SFU peer that answered it. No media
    /// has moved: the tests drive the two `Rtc` values themselves.
    fn connected_pair() -> (Rtc, SfuPeer) {
        let (bind, caller_addr) = pair_addrs();
        let mut caller = Rtc::builder().build(Instant::now());
        caller.add_local_candidate(Candidate::host(caller_addr, "udp").unwrap());
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
        let peer = SfuPeer::new(
            ParticipantId::next(),
            RoomId::new("neighbourhood://test", "room"),
            "did:key:caller".to_string(),
            rtc,
            false,
        );
        (caller, peer)
    }

    /// Offer one outbound audio track and return its SDP, mapping, and mid.
    fn offer_outbound_audio(
        peer: &mut SfuPeer,
        now: Instant,
    ) -> (String, Vec<crate::sfu::TrackMapEntry>, Mid) {
        let origin = ParticipantId::next();
        peer.deferred_tracks.push((
            origin.clone(),
            Mid::from("o1"),
            MediaKind::Audio,
            TrackOrigin {
                participant_id: origin.to_string(),
                agent_did: "did:key:origin".to_string(),
            },
        ));
        let (sdp, mapping) = apply_deferred_tracks(peer, now).expect("the track is offered");
        let mid = Mid::from(mapping[0].mid.as_str());
        (sdp, mapping, mid)
    }

    /// The track is negotiated: str0m has a writer, and that writer has an
    /// audio payload. `tracks_out` is not this check.
    fn assert_audio_writer(peer: &mut SfuPeer, mid: Mid) {
        let writer = peer.rtc.writer(mid).expect("the track has no writer");
        assert!(
            writer.payload_params().any(|p| p.spec().codec.is_audio()),
            "the writer has no audio payload"
        );
    }

    /// `a=mid` values of an offer, in SDP order.
    fn sdp_mids(sdp_json: &str) -> Vec<String> {
        let offer: serde_json::Value = serde_json::from_str(sdp_json).unwrap();
        offer["sdp"]
            .as_str()
            .unwrap()
            .lines()
            .filter_map(|line| line.trim().strip_prefix("a=mid:").map(str::to_string))
            .collect()
    }

    fn assert_same_mapping(
        left: &[crate::sfu::TrackMapEntry],
        right: &[crate::sfu::TrackMapEntry],
    ) {
        assert_eq!(left.len(), right.len());
        for (a, b) in left.iter().zip(right) {
            assert_eq!(a.mid, b.mid);
            assert_eq!(a.participant_id, b.participant_id);
            assert_eq!(a.agent_did, b.agent_did);
            assert_eq!(a.media_kind, b.media_kind);
        }
    }

    /// The caller's answer to `sdp`.
    fn answer_json(caller: &mut Rtc, sdp: &str) -> String {
        let answer = caller
            .sdp_api()
            .accept_offer(serde_json::from_str(sdp).unwrap())
            .unwrap();
        serde_json::to_string(&answer).unwrap()
    }

    /// Move packets between the caller and the SFU peer until `end` of
    /// simulated time has passed. Steps are 1–50 ms. Nothing sleeps.
    fn exchange_until(
        caller: &mut Rtc,
        peer: &mut Rtc,
        bind: SocketAddr,
        caller_addr: SocketAddr,
        start: Instant,
        end: Duration,
    ) {
        let mut now = start;
        let (mut caller_seen, mut peer_seen) = (Seen::default(), Seen::default());
        while now - start < end {
            let mut sent = Vec::new();
            let caller_next = drive(caller, now, start, &mut caller_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                if destination == bind {
                    receive(peer, now, source, destination, &data);
                }
            }
            let peer_next = drive(peer, now, start, &mut peer_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                if destination == caller_addr {
                    receive(caller, now, source, destination, &data);
                }
            }
            let next = caller_next.min(peer_next);
            now = next.clamp(
                now + Duration::from_millis(1),
                now + Duration::from_millis(50),
            );
        }
    }

    /// A lost renegotiation answer used to drop the pending token and leave
    /// the track mapped, so the SFU never offered it again. The caller
    /// applies the first offer and that answer is dropped. The same SDP is
    /// sent once more. The answer to that resend negotiates the track.
    #[test]
    fn a_lost_renegotiation_answer_renegotiates_on_the_same_offer() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, mapping, mid) = offer_outbound_audio(&mut peer, now);
        let mids = sdp_mids(&sdp);
        assert!(
            peer.rtc.writer(mid).is_none(),
            "the track is mapped before it is negotiated"
        );
        assert!(matches!(peer.on_offer_deadline(now), OfferDeadline::Wait));

        // The caller applied the first offer. That answer never reaches the SFU.
        let dropped = answer_json(&mut caller, &sdp);
        assert!(!dropped.is_empty());

        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let resend = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(resend.len(), 1, "the stale offer is sent again");
        assert_eq!(resend[0].sdp, sdp);
        assert_same_mapping(&resend[0].track_mapping, &mapping);
        assert_eq!(sdp_mids(&resend[0].sdp), mids);
        assert!(sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE).is_empty());
        let peer = peers.get_mut(&pid).unwrap();
        assert_eq!(peer.tracks_out.len(), 1, "the resend adds no m-line");
        assert!(peer.pending.is_some());
        assert_eq!(peer.pending.as_ref().unwrap().resends, 1);
        assert!(peer.rtc.writer(mid).is_none());

        let answer = answer_json(&mut caller, &resend[0].sdp);
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none(), "the answer cancels the resend");
        exchange_until(
            &mut caller,
            &mut peer.rtc,
            bind,
            caller_addr,
            now,
            Duration::from_millis(300),
        );
        assert_audio_writer(peer, mid);
        let media = caller.media(mid).expect("the caller negotiated the mid");
        assert!(media.kind().is_audio());
    }

    /// Walk the two back-off resends and return the deadline after them,
    /// where #1802 dropped the peer.
    fn final_offer_deadline(
        peers: &mut HashMap<ParticipantId, SfuPeer>,
        pid: &ParticipantId,
        now: Instant,
        sdp: &str,
    ) -> Instant {
        let first = sweep_offer_deadlines(peers, now + OFFER_RETRY_BASE);
        assert_eq!(first.len(), 1);
        assert_eq!(first[0].sdp, sdp);
        let second = sweep_offer_deadlines(peers, now + Duration::from_secs(30));
        assert_eq!(second.len(), 1);
        assert_eq!(second[0].sdp, sdp);
        let deadline = peers[pid].pending.as_ref().unwrap().next_deadline;
        assert_eq!(deadline, now + Duration::from_secs(60));
        assert_eq!(
            peers[pid].pending.as_ref().unwrap().resends,
            OFFER_BACKOFF_RESENDS
        );
        deadline
    }

    /// An answer one millisecond before the deadline after the back-off is
    /// applied. The sweep then has nothing to resend.
    #[test]
    fn an_answer_just_before_the_final_deadline_is_applied() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        let answer = answer_json(&mut caller, &sdp);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let deadline = final_offer_deadline(&mut peers, &pid, now, &sdp);
        let peer = peers.get_mut(&pid).unwrap();
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none(), "the answer cancels the resend");
        assert!(peer.rtc.is_alive());
        assert!(sweep_offer_deadlines(&mut peers, deadline).is_empty());
        assert!(peers[&pid].rtc.is_alive());
        let peer = peers.get_mut(&pid).unwrap();
        exchange_until(
            &mut caller,
            &mut peer.rtc,
            bind,
            caller_addr,
            now,
            Duration::from_millis(300),
        );
        assert_audio_writer(peer, mid);
    }

    /// An answer at a resend instant still belongs to the offer. The resend
    /// does not happen.
    #[test]
    fn an_answer_at_a_resend_deadline_cancels_the_resend() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        let answer = answer_json(&mut caller, &sdp);
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none());
        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Wait
        ));
        assert_audio_writer(&mut peer, mid);
    }

    /// The command handler runs before the deadline sweep. An answer at the
    /// deadline where #1802 dropped the peer, or later, is applied: the
    /// offer was resent, never withdrawn, and the peer stays.
    #[test]
    fn an_answer_after_the_back_off_is_applied() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        let answer = answer_json(&mut caller, &sdp);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let deadline = final_offer_deadline(&mut peers, &pid, now, &sdp);

        let peer = peers.get_mut(&pid).unwrap();
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none());
        assert_audio_writer(peer, mid);
        assert!(sweep_offer_deadlines(&mut peers, deadline).is_empty());
        assert!(peers[&pid].rtc.is_alive());

        let (mut caller, mut peer) = connected_pair();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        let answer = answer_json(&mut caller, &sdp);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let deadline = final_offer_deadline(&mut peers, &pid, now, &sdp);
        let resent = sweep_offer_deadlines(&mut peers, deadline);
        assert_eq!(resent.len(), 1, "the sweep resends instead of dropping");
        assert!(peers[&pid].rtc.is_alive());
        let peer = peers.get_mut(&pid).unwrap();
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none());
        assert_audio_writer(peer, mid);
    }

    /// Two pipes that already share one track each offer another. Each
    /// applies the other's offer, which drops its own token. The answer to
    /// the local offer is out of order. The pipe stays up and the first
    /// track keeps its writer.
    #[test]
    fn a_crossed_pipe_offer_keeps_the_established_track() {
        let now = Instant::now();
        let (mut left, mut right, mid) = negotiated_pipe_ends();
        left.on_ice_state(IceConnectionState::Connected, now);
        right.on_ice_state(IceConnectionState::Connected, now);
        assert!(left.had_path && right.had_path);
        assert_audio_writer(&mut left, mid);

        let (left_sdp, _, _) = offer_outbound_audio(&mut left, now);
        let (right_sdp, _, _) = offer_outbound_audio(&mut right, now);
        let left_offer: SdpOffer = serde_json::from_str(&left_sdp).unwrap();
        let right_offer: SdpOffer = serde_json::from_str(&right_sdp).unwrap();
        let answer_for_right = left
            .rtc
            .sdp_api()
            .accept_offer(right_offer)
            .expect("the crossed offer applies on the left pipe");
        let answer_for_left = right
            .rtc
            .sdp_api()
            .accept_offer(left_offer)
            .expect("the crossed offer applies on the right pipe");

        let err = left
            .accept_current_answer(&serde_json::to_string(&answer_for_left).unwrap())
            .unwrap_err();
        assert!(matches!(err, ServerAnswerError::OutOfOrder));
        assert!(left.pending.is_none(), "the crossed offer took the token");
        assert!(left.rtc.is_alive());
        assert_audio_writer(&mut left, mid);

        right.settle_current_answer(&serde_json::to_string(&answer_for_right).unwrap());
        assert!(right.rtc.is_alive(), "the loop keeps the pipe");
        assert!(right.pending.is_none());
        assert_ne!(left.drop_reason(), Some("offer"));
        assert_ne!(right.drop_reason(), Some("offer"));
        assert!(
            !left.deferred_tracks.is_empty(),
            "the crossed offer's tracks are queued again"
        );
        assert!(!right.deferred_tracks.is_empty());

        let pid = left.id.clone();
        let mut peers = HashMap::from([(pid.clone(), left)]);
        assert!(
            sweep_offer_deadlines(&mut peers, now + Duration::from_secs(60)).is_empty(),
            "a crossed offer does not end the pipe"
        );
        assert!(peers[&pid].rtc.is_alive());
        assert_ne!(peers[&pid].drop_reason(), Some("offer"));
        let (_again, mapping) = apply_deferred_tracks(peers.get_mut(&pid).unwrap(), now)
            .expect("the requeued track is offered again on this pipe");
        assert_eq!(mapping.len(), 1);
    }

    /// A fresh process does not hand out a low offer id, and the wire form
    /// stays `o{n}`. Consecutive mints still increase by one.
    #[test]
    fn a_fresh_offer_id_starts_in_the_upper_half_and_keeps_its_form() {
        let first = next_offer_id();
        let second = next_offer_id();
        let n1: u64 = first[1..]
            .parse()
            .unwrap_or_else(|_| panic!("offer id {first} is not o{{n}}"));
        let n2: u64 = second[1..]
            .parse()
            .unwrap_or_else(|_| panic!("offer id {second} is not o{{n}}"));
        assert!(
            first.starts_with('o') && first == format!("o{n1}"),
            "offer id {first} lost its o{{n}} form"
        );
        assert!(n1 >= 1u64 << 63, "fresh offer id {first} is below 2^63");
        assert_eq!(n2, n1 + 1, "consecutive offer ids {first} then {second}");
        assert_eq!(second, format!("o{n2}"));
    }

    /// A second process start does not hand out the first process's first
    /// offer id. The wire form stays `o{n}`.
    #[test]
    fn two_boots_do_not_hand_out_the_same_first_offer_id() {
        restart_offer_ids();
        let first = next_offer_id();
        restart_offer_ids();
        let second = next_offer_id();
        assert_ne!(
            first, second,
            "two boots handed out the same first offer id {first}"
        );
        let n1: u64 = first[1..]
            .parse()
            .unwrap_or_else(|_| panic!("offer id {first} is not o{{n}}"));
        let n2: u64 = second[1..]
            .parse()
            .unwrap_or_else(|_| panic!("offer id {second} is not o{{n}}"));
        assert!(
            first == format!("o{n1}") && second == format!("o{n2}"),
            "a restarted offer id lost its o{{n}} form: {first} then {second}"
        );
        assert!(
            n1 >= 1u64 << 63 && n2 >= 1u64 << 63,
            "a restarted offer id is below 2^63: {first} then {second}"
        );
    }

    /// An answer that carries this offer's SDP but another offer's id is
    /// refused. The pending offer stays, and the answer that names it is
    /// accepted. Two offers can share a mid list (an ICE restart reuses
    /// the m-lines), so the id is what tells them apart.
    #[test]
    fn an_answer_with_the_wrong_offer_id_is_refused() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        let id = peer.pending_offer_id().unwrap().to_string();
        let answer = answer_json(&mut caller, &sdp);
        let err = peer.accept_server_answer("o-other", &answer).unwrap_err();
        assert!(matches!(err, ServerAnswerError::NotThisOffer));
        assert_eq!(peer.pending_offer_id(), Some(id.as_str()));
        peer.accept_server_answer(&id, &answer).unwrap();
        assert!(peer.pending.is_none());
        assert_audio_writer(&mut peer, mid);
    }

    /// A second answer that names an offer already accepted is accepted and
    /// changes nothing: not when the session is idle, and not while a newer
    /// offer waits. A wrong id, and an answer with nothing waiting that was
    /// never accepted, are refusals.
    #[test]
    fn a_retried_accepted_answer_is_accepted_and_changes_nothing() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp_a, _, mid_a) = offer_outbound_audio(&mut peer, now);
        let id_a = peer.pending_offer_id().unwrap().to_string();
        let answer_a = answer_json(&mut caller, &sdp_a);
        assert!(peer.settle_answer("o-other", &answer_a).reason == Some(SfuAnswerRefusal::Stale));
        assert_eq!(peer.pending_offer_id(), Some(id_a.as_str()));
        assert!(peer.settle_answer(&id_a, &answer_a).accepted);
        assert!(peer.pending.is_none());
        assert_audio_writer(&mut peer, mid_a);

        let again = peer.settle_answer(&id_a, "not-sdp");
        assert!(again.accepted);
        assert!(again.reason.is_none());
        assert!(peer.pending.is_none());
        assert_audio_writer(&mut peer, mid_a);
        assert!(peer.rtc.is_alive());

        let (sdp_b, _, mid_b) = offer_outbound_audio(&mut peer, now);
        let id_b = peer.pending_offer_id().unwrap().to_string();
        assert_ne!(id_a, id_b);
        let repeated = peer.settle_answer(&id_a, &answer_a);
        assert!(repeated.accepted);
        assert_eq!(peer.pending_offer_id(), Some(id_b.as_str()));
        assert_eq!(peer.pending.as_ref().unwrap().sdp, sdp_b);
        assert!(peer.rtc.writer(mid_b).is_none());

        let idle = {
            let (_caller, mut quiet) = connected_pair();
            quiet.settle_answer("o-never", "{}")
        };
        assert_eq!(idle.reason, Some(SfuAnswerRefusal::NoPending));
        assert!(!idle.accepted);
    }

    /// A copy of the answer to offer A arrives after offer B, which added an
    /// m-line. The copy names A's id, so it does not take B's token, and
    /// B's own answer still negotiates.
    #[test]
    fn a_delayed_answer_for_an_older_offer_keeps_the_peer() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp_a, _, mid_a) = offer_outbound_audio(&mut peer, now);
        let id_a = peer.pending_offer_id().unwrap().to_string();
        let answer_a = answer_json(&mut caller, &sdp_a);
        peer.accept_server_answer(&id_a, &answer_a).unwrap();
        assert_audio_writer(&mut peer, mid_a);

        let (sdp_b, _, mid_b) = offer_outbound_audio(&mut peer, now);
        let id_b = peer.pending_offer_id().unwrap().to_string();
        assert_ne!(id_a, id_b);
        assert_ne!(sdp_mids(&sdp_a), sdp_mids(&sdp_b));
        // A was already accepted. Repeating it while B waits is accepted
        // and leaves B's token where it is.
        peer.accept_server_answer(&id_a, &answer_a).unwrap();
        peer.settle_answer(&id_a, &answer_a);
        assert!(peer.rtc.is_alive());
        assert_eq!(peer.pending.as_ref().unwrap().sdp, sdp_b);
        assert_audio_writer(&mut peer, mid_a);
        assert!(peer.rtc.writer(mid_b).is_none());

        let answer_b = answer_json(&mut caller, &sdp_b);
        peer.settle_answer(&id_b, &answer_b);
        assert!(peer.pending.is_none());
        assert!(peer.rtc.is_alive());
        exchange_until(
            &mut caller,
            &mut peer.rtc,
            bind,
            caller_addr,
            now,
            Duration::from_millis(300),
        );
        assert_audio_writer(&mut peer, mid_a);
        assert_audio_writer(&mut peer, mid_b);
    }

    /// Two pipe ends that have finished one audio negotiation.
    fn negotiated_pipe_ends() -> (SfuPeer, SfuPeer, Mid) {
        let now = Instant::now();
        let left_addr: SocketAddr = "192.168.0.11:41001".parse().unwrap();
        let right_addr: SocketAddr = "192.168.0.12:41002".parse().unwrap();
        let mut left_rtc = Rtc::builder().build(now);
        left_rtc.add_local_candidate(Candidate::host(left_addr, "udp").unwrap());
        let mut change = left_rtc.sdp_api();
        let mid = change.add_media(
            MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let (offer, pending) = change.apply().unwrap();
        let mut right_rtc = Rtc::builder().build(now);
        right_rtc.add_local_candidate(Candidate::host(right_addr, "udp").unwrap());
        let answer = right_rtc.sdp_api().accept_offer(offer).unwrap();
        left_rtc.sdp_api().accept_answer(pending, answer).unwrap();
        let room = RoomId::new("neighbourhood://test", "room");
        let left = SfuPeer::new(
            ParticipantId::next(),
            room.clone(),
            "did:key:node-b".to_string(),
            left_rtc,
            true,
        );
        let right = SfuPeer::new(
            ParticipantId::next(),
            room,
            "did:key:node-a".to_string(),
            right_rtc,
            true,
        );
        (left, right, mid)
    }

    /// Two resends that back off, then one every 30 s. The peer stays and
    /// keeps the track mapped, and the mapping goes out with every resend.
    #[test]
    fn an_offer_is_resent_twice_and_then_every_thirty_seconds() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, mapping, _) = offer_outbound_audio(&mut peer, now);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);

        assert!(sweep_offer_deadlines(&mut peers, now + Duration::from_secs(9)).is_empty());
        let first = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(first.len(), 1);
        assert_eq!(first[0].sdp, sdp);
        assert!(sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE).is_empty());
        let second = sweep_offer_deadlines(&mut peers, now + Duration::from_secs(30));
        assert_eq!(second.len(), 1);
        assert_eq!(second[0].sdp, sdp);
        assert!(sweep_offer_deadlines(&mut peers, now + Duration::from_secs(59)).is_empty());
        for at in [60, 90, 120] {
            let due = sweep_offer_deadlines(&mut peers, now + Duration::from_secs(at));
            assert_eq!(due.len(), 1, "resend at {at} s");
            assert_eq!(due[0].sdp, sdp);
            assert_same_mapping(&due[0].track_mapping, &mapping);
            assert!(peers[&pid].rtc.is_alive());
        }
        assert_eq!(peers[&pid].pending.as_ref().unwrap().resends, 5);
        assert_eq!(peers[&pid].tracks_out.len(), 1);
        assert!(apply_deferred_tracks(peers.get_mut(&pid).unwrap(), now).is_none());
    }

    /// #1802 dropped a peer whose offer stayed unanswered for 70 s, also
    /// while its media flowed (a caller dropped mid-call on 2026-11-19). The
    /// offer is resent at 10 s and 30 s, then every 30 s, and the peer stays.
    /// Its answer still lands after two minutes.
    #[test]
    fn an_offer_unanswered_for_two_minutes_while_media_flows_keeps_the_peer() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        peer.on_ice_state(IceConnectionState::Completed, now);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);

        let mut resent = Vec::new();
        for second in 0..=120 {
            let at = now + Duration::from_secs(second);
            peers.get_mut(&pid).unwrap().note_media(at);
            assert!(peers[&pid].media_flowing(at));
            for due in sweep_offer_deadlines(&mut peers, at) {
                assert_eq!(due.sdp, sdp);
                resent.push(second);
            }
            assert!(peers[&pid].rtc.is_alive(), "dropped at {second} s");
            assert!(!peers[&pid].stayed_disconnected(at));
        }
        assert_eq!(resent, vec![10, 30, 60, 90, 120]);

        let answer = answer_json(&mut caller, &sdp);
        let peer = peers.get_mut(&pid).unwrap();
        peer.accept_current_answer(&answer).unwrap();
        assert!(peer.pending.is_none());
        assert_audio_writer(peer, mid);
    }

    /// A peer that sends no media (camera and microphone off, or a guest
    /// with no devices) while ICE stays up is not dropped for an unanswered
    /// offer either: only the ICE rules end a silent peer.
    #[test]
    fn a_silent_peer_with_ice_up_keeps_its_unanswered_offer() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, _) = offer_outbound_audio(&mut peer, now);
        peer.on_ice_state(IceConnectionState::Connected, now);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        for second in 0..=180 {
            let at = now + Duration::from_secs(second);
            for due in sweep_offer_deadlines(&mut peers, at) {
                assert_eq!(due.sdp, sdp);
            }
            assert!(!peers[&pid].media_flowing(at));
            assert!(peers[&pid].rtc.is_alive(), "dropped at {second} s");
            assert!(!peers[&pid].stayed_disconnected(at));
        }
        assert_eq!(peers[&pid].pending.as_ref().unwrap().sdp, sdp);
    }

    /// A deadline resend and `sfu.callResume` publish the id minted with
    /// the offer. Neither draws a new one.
    #[test]
    fn a_resend_and_a_resume_keep_the_same_offer_id() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, _) = offer_outbound_audio(&mut peer, now);
        let id = peer.pending_offer_id().unwrap().to_string();
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let due = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(due.len(), 1);
        assert_eq!(due[0].offer_id, id);
        assert_eq!(due[0].sdp, sdp);
        assert_eq!(peers[&pid].pending_offer_id(), Some(id.as_str()));
        let peer = peers.get_mut(&pid).unwrap();
        let (resumed_id, resumed_sdp, _) = peer
            .resume_offer(now + Duration::from_secs(20))
            .expect("the offer waits");
        assert_eq!(resumed_id, id);
        assert_eq!(resumed_sdp, sdp);
        assert_eq!(peer.pending_offer_id(), Some(id.as_str()));
    }

    /// `sfu.callResume` while an offer waits: the pending offer goes out
    /// again at once, the same SDP and mapping, and the next deadline resend
    /// waits 30 s from the resume. Without a pending offer it sends nothing.
    #[test]
    fn a_resume_sends_the_pending_offer_at_once() {
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        assert!(peer.resume_offer(now).is_none(), "no offer waits");
        let (sdp, mapping, mid) = offer_outbound_audio(&mut peer, now);
        peer.note_media(now);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let resent = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(resent.len(), 1);

        let resumed_at = now + Duration::from_secs(95);
        let peer = peers.get_mut(&pid).unwrap();
        let (_resumed_id, resumed_sdp, resumed_mapping) =
            peer.resume_offer(resumed_at).expect("the offer waits");
        assert_eq!(resumed_sdp, sdp);
        assert_same_mapping(&resumed_mapping, &mapping);
        assert!(sweep_offer_deadlines(&mut peers, resumed_at).is_empty());
        assert!(sweep_offer_deadlines(
            &mut peers,
            resumed_at + OFFER_RESEND_STEADY - Duration::from_millis(1)
        )
        .is_empty());
        assert_eq!(
            sweep_offer_deadlines(&mut peers, resumed_at + OFFER_RESEND_STEADY).len(),
            1
        );

        let peer = peers.get_mut(&pid).unwrap();
        peer.accept_current_answer(&answer_json(&mut caller, &resumed_sdp))
            .unwrap();
        assert_audio_writer(peer, mid);
        assert!(peer.resume_offer(resumed_at).is_none());
        assert!(peer.rtc.is_alive());
    }

    /// A client peer's `Rtc` reports `PeerStats` every 30 s, for the stats
    /// line. Before, the SFU logged no bitrate, loss or layer per peer.
    #[test]
    fn a_client_peer_reports_its_stats_every_thirty_seconds() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, mut peer) = connected_pair();
        let start = Instant::now();
        let (mut caller_seen, mut peer_seen) = (Seen::default(), Seen::default());
        let mut sent = Vec::new();
        let mut now = start;
        while now - start < PEER_STATS_INTERVAL * 2 + Duration::from_secs(1) {
            let caller_next = drive(&mut caller, now, start, &mut caller_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                if destination == bind {
                    receive(&mut peer.rtc, now, source, destination, &data);
                }
            }
            let peer_next = drive(&mut peer.rtc, now, start, &mut peer_seen, &mut sent);
            for (source, destination, data) in sent.drain(..) {
                if destination == caller_addr {
                    receive(&mut caller, now, source, destination, &data);
                }
            }
            now = caller_next.min(peer_next).clamp(
                now + Duration::from_millis(1),
                now + Duration::from_millis(50),
            );
        }
        assert!(peer.rtc.is_alive());
        assert_eq!(peer_seen.peer_stats, 2);
    }

    /// The stats line: bitrates since the previous line, totals, loss each
    /// way, RTT, the layer of each video track received, the preference and
    /// the media age.
    #[test]
    fn the_stats_line_names_bitrate_loss_rtt_layers_and_media_age() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.note_media(now);
        let stats = |at: Instant, bytes_rx: u64, bytes_tx: u64| PeerStats {
            peer_bytes_rx: bytes_rx,
            peer_bytes_tx: bytes_tx,
            bytes_rx,
            bytes_tx,
            timestamp: at,
            bwe_tx: None,
            egress_loss_fraction: Some(0.012),
            ingress_loss_fraction: None,
            rtt: Some(Duration::from_millis(34)),
            selected_candidate_pair: None,
        };
        let first = peer.stats_line(&stats(now, 1_000_000, 3_000_000), &[], None, now);
        assert!(
            first.contains(&format!("peer {} (did:key:caller)", peer.id)),
            "{first}"
        );
        assert!(first.contains("in - kbit/s out - kbit/s"), "{first}");
        assert!(first.contains("total 1000 kB in, 3000 kB out"), "{first}");

        let later = now + PEER_STATS_INTERVAL;
        let line = peer.stats_line(
            &stats(later, 1_000_000 + 3_000_000, 3_000_000 + 7_500_000),
            &[Some(Rid::from("h")), None, Some(Rid::from("l"))],
            Some("medium"),
            later + Duration::from_millis(1500),
        );
        assert!(line.contains("in 800 kbit/s out 2000 kbit/s"), "{line}");
        assert!(line.contains("loss in - out 1.2%"), "{line}");
        assert!(line.contains("rtt 34 ms"), "{line}");
        assert!(line.contains("layers [-,h,l]"), "{line}");
        assert!(line.contains("pref medium"), "{line}");
        assert!(line.contains("media 31.5 s ago"), "{line}");
    }

    /// The establishment window covers the second resend (t=30 s) and the
    /// ICE grace after it. A shorter window drops the pipe on the tick that
    /// queues that resend.
    #[test]
    fn the_establishment_timeout_covers_the_second_resend_and_ice() {
        let second_resend = OFFER_RETRY_BASE + OFFER_RETRY_BASE * 2;
        assert_eq!(second_resend, Duration::from_secs(30));
        assert!(
            PEER_ESTABLISH_TIMEOUT >= second_resend + ICE_RESTART_GRACE,
            "establishment {} s ends before the second resend plus ICE ({} s)",
            PEER_ESTABLISH_TIMEOUT.as_secs(),
            (second_resend + ICE_RESTART_GRACE).as_secs(),
        );
    }

    /// The SDP of a pipe's first offer.
    fn pipe_offer_sdp(signal: super::super::cascade::CascadeSignal) -> String {
        match signal {
            super::super::cascade::CascadeSignal::PipeOffer { sdp_offer, .. } => sdp_offer,
            _ => panic!("establish_pipe returns a pipe offer"),
        }
    }

    /// An initial pipe is still up when its second resend is due, and an
    /// answer that lands after the first resend and before the second does
    /// not get the pipe swept while ICE is still checking.
    #[test]
    fn an_initial_pipe_is_alive_at_the_second_resend() {
        let mut mgr = super::super::cascade::CascadeManager::new(
            "did:key:nodeB".into(),
            "127.0.0.1:10041".parse::<std::net::SocketAddr>().unwrap(),
            8,
        );
        let room = RoomId::new("neighbourhood://pipe-resend", "r");
        let before = Instant::now();
        let (pipe, signal) = mgr.establish_pipe("did:key:nodeA", &room).unwrap();
        let after = Instant::now();
        let sdp = pipe_offer_sdp(signal);
        let pid = pipe.id.clone();
        let mut peers = HashMap::from([(pid.clone(), pipe)]);

        let first = sweep_offer_deadlines(&mut peers, after + OFFER_RETRY_BASE);
        assert_eq!(first.len(), 1, "the first resend is due");
        assert_eq!(first[0].sdp, sdp);
        let second_at = after + OFFER_RETRY_BASE * 3;
        let second = sweep_offer_deadlines(&mut peers, second_at);
        assert_eq!(second.len(), 1, "the second resend is still this offer");
        assert_eq!(second[0].sdp, sdp);
        assert!(peers[&pid].rtc.is_alive());
        assert!(
            !peers[&pid].never_established(second_at),
            "the pipe is swept on the tick of its second resend"
        );
        assert!(!peers[&pid].stayed_disconnected(second_at));
        assert!(before <= peers[&pid].created_at && peers[&pid].created_at <= after);

        let (mut answered, signal) = mgr.establish_pipe("did:key:nodeC", &room).unwrap();
        let answered_after = Instant::now();
        let sdp = pipe_offer_sdp(signal);
        let mut remote = Rtc::builder().build(answered_after);
        let answer = remote
            .sdp_api()
            .accept_offer(serde_json::from_str(&sdp).unwrap())
            .expect("the remote accepts the initial pipe offer");
        answered
            .accept_current_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();
        assert!(answered.pending.is_none(), "the answer landed");
        assert!(!answered.had_path, "ICE has not connected");
        assert!(!answered.never_established(answered_after + Duration::from_secs(30)));
        assert!(!answered.never_established(
            answered.created_at + PEER_ESTABLISH_TIMEOUT - Duration::from_millis(1)
        ));
        assert!(answered.rtc.is_alive());
        assert!(answered.never_established(answered.created_at + PEER_ESTABLISH_TIMEOUT));
    }

    /// One more outbound audio track, held until the pending offer clears.
    fn defer_extra_audio(peer: &mut SfuPeer) {
        let origin = ParticipantId::next();
        peer.deferred_tracks.push((
            origin.clone(),
            Mid::from("o2"),
            MediaKind::Audio,
            TrackOrigin {
                participant_id: origin.to_string(),
                agent_did: "did:key:extra".to_string(),
            },
        ));
    }

    /// One outbound audio track named as a session, held until the pending
    /// offer clears.
    fn defer_named(peer: &mut SfuPeer, participant_id: &str, agent_did: &str) {
        peer.deferred_tracks.push((
            ParticipantId::next(),
            Mid::from("o"),
            MediaKind::Audio,
            TrackOrigin {
                participant_id: participant_id.to_string(),
                agent_did: agent_did.to_string(),
            },
        ));
    }

    /// Offer Alice's and Bob's audio on `peer`.
    fn offer_two_origins(
        peer: &mut SfuPeer,
        now: Instant,
    ) -> (String, Vec<crate::sfu::TrackMapEntry>) {
        defer_named(peer, "P-alice", "did:key:alice");
        defer_named(peer, "P-bob", "did:key:bob");
        apply_deferred_tracks(peer, now).expect("both origins are offered")
    }

    /// The SDP text of a pipe answer signal.
    fn pipe_answer_sdp(signal: CascadeSignal) -> String {
        match signal {
            CascadeSignal::PipeAnswer { sdp_answer, .. } => sdp_answer,
            _ => panic!("expected a pipe answer"),
        }
    }

    /// Two cascade nodes with one pipe whose ICE has connected. `nodeB`
    /// dials: its DID is the higher one.
    fn connected_cascade_pipe() -> (CascadeManager, CascadeManager, SfuPeer, SfuPeer, RoomId) {
        let mut dialer = CascadeManager::new(
            "did:key:nodeB".into(),
            "192.168.0.22:41022"
                .parse::<std::net::SocketAddr>()
                .unwrap(),
            8,
        );
        let mut answerer = CascadeManager::new(
            "did:key:nodeA".into(),
            "192.168.0.21:41021"
                .parse::<std::net::SocketAddr>()
                .unwrap(),
            8,
        );
        let room = RoomId::new("neighbourhood://pipe-end", "r");
        let (mut dial, offer) = dialer.establish_pipe("did:key:nodeA", &room).unwrap();
        let (mut answer, answer_signal) = answerer
            .handle_pipe_offer("did:key:nodeB", &room.to_string(), &pipe_offer_sdp(offer))
            .unwrap();
        let answer_json = pipe_answer_sdp(answer_signal);
        let (_pid, json) = dialer
            .handle_pipe_answer("did:key:nodeA", &room.to_string(), &answer_json)
            .unwrap();
        dial.accept_current_answer(&json).unwrap();
        let now = Instant::now();
        dial.on_ice_state(IceConnectionState::Connected, now);
        answer.on_ice_state(IceConnectionState::Connected, now);
        assert!(dial.had_path && answer.had_path);
        assert!(dial.pending.is_none());
        (dialer, answerer, dial, answer, room)
    }

    /// The answer, with a different ICE ufrag, so str0m rejects it locally
    /// as an ICE restart this offer did not ask for. The m-lines stay this
    /// offer's. There is no refusal on the wire.
    fn with_other_ice_ufrag(answer_json: &str) -> String {
        let mut value: serde_json::Value = serde_json::from_str(answer_json).unwrap();
        let sdp = value["sdp"].as_str().unwrap();
        let mut out = String::new();
        for line in sdp.split_inclusive('\n') {
            let bare = line.trim_end_matches(['\r', '\n']);
            if bare.starts_with("a=ice-ufrag:") {
                out.push_str("a=ice-ufrag:rejected1");
                out.push_str(&line[bare.len()..]);
            } else {
                out.push_str(line);
            }
        }
        value["sdp"] = serde_json::Value::String(out);
        serde_json::to_string(&value).unwrap()
    }

    /// One caller whose audio the SFU already receives. `tracks_in` is what
    /// the event loop records on `MediaAdded`.
    struct AudioLeg {
        caller: Rtc,
        peer: SfuPeer,
        mid: Mid,
        caller_addr: SocketAddr,
        bind: SocketAddr,
    }

    impl AudioLeg {
        fn new(room: &RoomId, did: &str, caller_addr: &str, bind: &str) -> Self {
            let caller_addr: SocketAddr = caller_addr.parse().unwrap();
            let bind: SocketAddr = bind.parse().unwrap();
            let mut caller = Rtc::builder().build(Instant::now());
            caller.add_local_candidate(Candidate::host(caller_addr, "udp").unwrap());
            let mut change = caller.sdp_api();
            let mid = change.add_media(
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
                room.clone(),
                did.to_string(),
                rtc,
                false,
            );
            peer.tracks_in.insert(mid, MediaKind::Audio);
            Self {
                caller,
                peer,
                mid,
                caller_addr,
                bind,
            }
        }
    }

    /// Move packets between two RTCs for one step. Media each side emits is
    /// appended. The clock stays on the simulated timeline the other sfu
    /// tests use (`drive` / `receive`), never wall-clock sleep.
    fn pump_pair(
        left: &mut Rtc,
        left_addr: SocketAddr,
        right: &mut Rtc,
        right_addr: SocketAddr,
        now: &mut Instant,
        start: Instant,
        left_media: &mut Vec<MediaData>,
        right_media: &mut Vec<MediaData>,
    ) -> (bool, bool) {
        let mut sent = Vec::new();
        let mut left_seen = Seen::default();
        let mut right_seen = Seen::default();
        let left_next = drive(left, *now, start, &mut left_seen, &mut sent);
        for (src, dst, data) in sent.drain(..) {
            if dst == right_addr {
                receive(right, *now, src, dst, &data);
            }
        }
        let right_next = drive(right, *now, start, &mut right_seen, &mut sent);
        for (src, dst, data) in sent.drain(..) {
            if dst == left_addr {
                receive(left, *now, src, dst, &data);
            }
        }
        left_media.append(&mut left_seen.media);
        right_media.append(&mut right_seen.media);
        let step = left_next.min(right_next);
        *now = step.clamp(
            *now + Duration::from_millis(1),
            *now + Duration::from_millis(20),
        );
        let up = |seen: &Seen| {
            seen.ice.iter().any(|(_, state)| {
                matches!(
                    state,
                    IceConnectionState::Connected | IceConnectionState::Completed
                )
            })
        };
        (up(&left_seen), up(&right_seen))
    }

    fn connect_pair(
        left: &mut Rtc,
        left_addr: SocketAddr,
        right: &mut Rtc,
        right_addr: SocketAddr,
        now: &mut Instant,
        start: Instant,
    ) {
        let deadline = *now + Duration::from_secs(5);
        let mut left_up = false;
        let mut right_up = false;
        let mut left_discard = Vec::new();
        let mut right_discard = Vec::new();
        while !left_up || !right_up {
            assert!(
                *now < deadline,
                "ICE did not connect between {left_addr} and {right_addr}"
            );
            let (left_step, right_step) = pump_pair(
                left,
                left_addr,
                right,
                right_addr,
                now,
                start,
                &mut left_discard,
                &mut right_discard,
            );
            left_up |= left_step;
            right_up |= right_step;
        }
    }

    fn write_opus(caller: &mut Rtc, mid: Mid, now: Instant, ticks: u64, frame: &[u8]) {
        let pt = caller
            .codec_config()
            .find(|params| params.spec().codec == Codec::Opus)
            .expect("the caller negotiated opus")
            .pt();
        let time = MediaTime::new(ticks * 960, str0m::media::Frequency::FORTY_EIGHT_KHZ);
        caller
            .writer(mid)
            .expect("the caller has an audio writer")
            .write(pt, now, time, frame.to_vec())
            .expect("the caller writes opus");
    }

    /// What the server loop pushes onto `pipes_usable`: the same events
    /// [`pipe_event_is_usable`] names. A hand-made list is not this.
    fn pipe_poll_is_usable(seen: &Seen) -> bool {
        pipe_event_is_usable(None, seen.channel.is_some())
            || seen
                .ice
                .iter()
                .any(|(_, state)| pipe_event_is_usable(Some(*state), false))
    }

    /// Drive the control pipe until `dial` reports that event. Returns
    /// whether it was seen. The seed offer is not built before this.
    fn dial_saw_usable_pipe(
        dial: &mut Rtc,
        dial_addr: SocketAddr,
        remote: &mut Rtc,
        remote_addr: SocketAddr,
        now: &mut Instant,
        start: Instant,
    ) -> bool {
        let deadline = *now + Duration::from_secs(5);
        loop {
            assert!(
                *now < deadline,
                "the pipe did not reach Connected or open its control channel"
            );
            let mut sent = Vec::new();
            let mut left_seen = Seen::default();
            let mut right_seen = Seen::default();
            let left_next = drive(dial, *now, start, &mut left_seen, &mut sent);
            for (src, dst, data) in sent.drain(..) {
                if dst == remote_addr {
                    receive(remote, *now, src, dst, &data);
                }
            }
            let right_next = drive(remote, *now, start, &mut right_seen, &mut sent);
            for (src, dst, data) in sent.drain(..) {
                if dst == dial_addr {
                    receive(dial, *now, src, dst, &data);
                }
            }
            let step = left_next.min(right_next);
            *now = step.clamp(
                *now + Duration::from_millis(1),
                *now + Duration::from_millis(20),
            );
            if pipe_poll_is_usable(&left_seen) {
                return true;
            }
        }
    }

    /// Seed `dial` (a control-only pipe) with the room's existing origins and
    /// return how many tracks were offered, plus how many of Alice's and
    /// Bob's RTP payloads arrived on the remote end of that pipe.
    ///
    /// The exchange is the one `media_crossing_peers_arrives_on_the_receivers_payload_type`
    /// uses: write a frame, move the packets, count `MediaData`. Here the
    /// receiver is the new pipe, not a second caller. The usable list is the
    /// Connected or channel-open event from that exchange, not a list passed
    /// in ahead of it.
    fn rtp_counts_for_control_pipe(
        room: &RoomId,
        dial: SfuPeer,
        remote: &mut SfuPeer,
        dial_addr: SocketAddr,
        remote_addr: SocketAddr,
    ) -> (usize, usize, usize) {
        let alice_frame: &[u8] = b"alice-rtp-on-the-new-pipe";
        let bob_frame: &[u8] = b"bob-rtp-on-the-new-pipe";
        let mut alice = AudioLeg::new(
            room,
            "did:key:alice",
            "203.0.113.40:50100",
            "192.168.0.40:42100",
        );
        let mut bob = AudioLeg::new(
            room,
            "did:key:bob",
            "203.0.113.41:50101",
            "192.168.0.41:42101",
        );
        let pipe_id = dial.id.clone();
        let alice_id = alice.peer.id.clone();
        let bob_id = bob.peer.id.clone();
        let alice_mid = alice.mid;
        let bob_mid = bob.mid;
        let mut peers = HashMap::from([
            (alice_id.clone(), alice.peer),
            (bob_id.clone(), bob.peer),
            (pipe_id.clone(), dial),
        ]);
        // The pipe opened no inbound track. Drive it until the same event
        // the loop records, and seed from that list only.
        let start = Instant::now();
        let mut now = start;
        let saw_usable = dial_saw_usable_pipe(
            &mut peers.get_mut(&pipe_id).unwrap().rtc,
            dial_addr,
            &mut remote.rtc,
            remote_addr,
            &mut now,
            start,
        );
        let usable: Vec<ParticipantId> = if saw_usable {
            vec![pipe_id.clone()]
        } else {
            Vec::new()
        };
        let adds = outbound_adds(&mut peers, &[], &usable);
        let seeded = adds.get(&pipe_id).map(|tracks| tracks.len()).unwrap_or(0);
        let offers = offer_new_tracks(&mut peers, adds);
        if offers.is_empty() {
            return (0, 0, seeded);
        }
        assert_eq!(offers.len(), 1, "one offer carries every existing origin");
        let (_pid, sdp, mapping) = offers.into_iter().next().unwrap();
        let dids: Vec<&str> = mapping
            .iter()
            .map(|entry| entry.agent_did.as_str())
            .collect();
        assert!(
            dids.contains(&"did:key:alice") && dids.contains(&"did:key:bob"),
            "the seeded offer names both origins: {dids:?}"
        );
        let offer: SdpOffer = serde_json::from_str(&sdp).unwrap();
        let answer = remote
            .rtc
            .sdp_api()
            .accept_offer(offer)
            .expect("the new pipe takes the existing origins");
        peers
            .get_mut(&pipe_id)
            .unwrap()
            .accept_current_answer(&serde_json::to_string(&answer).unwrap())
            .unwrap();

        connect_pair(
            &mut alice.caller,
            alice.caller_addr,
            &mut peers.get_mut(&alice_id).unwrap().rtc,
            alice.bind,
            &mut now,
            start,
        );
        connect_pair(
            &mut bob.caller,
            bob.caller_addr,
            &mut peers.get_mut(&bob_id).unwrap().rtc,
            bob.bind,
            &mut now,
            start,
        );
        // The pipe is already up: `dial_saw_usable_pipe` drove it until the
        // Connected or channel-open event the seed used. A second wait never
        // sees that event again.

        // Write again across the exchange. A frame queued before SRTP has
        // keys is dropped on the next poll, the same way the payload-type
        // test keeps the room running while media moves.
        let mut alice_media = Vec::new();
        let mut bob_media = Vec::new();
        let mut on_pipe = Vec::new();
        let mut alice_fwd = 0usize;
        let mut bob_fwd = 0usize;
        for step in 0..80 {
            let ticks = step as u64 + 1;
            write_opus(&mut alice.caller, alice_mid, now, ticks, alice_frame);
            write_opus(&mut bob.caller, bob_mid, now, ticks, bob_frame);
            let mut discard = Vec::new();
            pump_pair(
                &mut alice.caller,
                alice.caller_addr,
                &mut peers.get_mut(&alice_id).unwrap().rtc,
                alice.bind,
                &mut now,
                start,
                &mut discard,
                &mut alice_media,
            );
            pump_pair(
                &mut bob.caller,
                bob.caller_addr,
                &mut peers.get_mut(&bob_id).unwrap().rtc,
                bob.bind,
                &mut now,
                start,
                &mut discard,
                &mut bob_media,
            );
            let mut left_discard = Vec::new();
            pump_pair(
                &mut peers.get_mut(&pipe_id).unwrap().rtc,
                dial_addr,
                &mut remote.rtc,
                remote_addr,
                &mut now,
                start,
                &mut left_discard,
                &mut on_pipe,
            );
            let dial = peers.get_mut(&pipe_id).unwrap();
            for data in alice_media
                .iter()
                .skip(alice_fwd)
                .chain(bob_media.iter().skip(bob_fwd))
            {
                let origin = if &*data.data == alice_frame {
                    &alice_id
                } else if &*data.data == bob_frame {
                    &bob_id
                } else {
                    continue;
                };
                let mid = data.mid;
                let expected = if origin == &alice_id {
                    alice_mid
                } else {
                    bob_mid
                };
                assert_eq!(mid, expected, "MediaData mid is the origin's inbound mid");
                let out_mid = *dial
                    .tracks_out_rev
                    .get(&(origin.clone(), mid))
                    .expect("the seeded pipe forwards this origin");
                write_forwarded(dial, out_mid, data, data.time);
            }
            alice_fwd = alice_media.len();
            bob_fwd = bob_media.len();
            let alice_n = on_pipe
                .iter()
                .filter(|data| &*data.data == alice_frame)
                .count();
            let bob_n = on_pipe
                .iter()
                .filter(|data| &*data.data == bob_frame)
                .count();
            if alice_n >= 1 && bob_n >= 1 {
                return (alice_n, bob_n, seeded);
            }
        }
        let mut left_discard = Vec::new();
        pump_pair(
            &mut peers.get_mut(&pipe_id).unwrap().rtc,
            dial_addr,
            &mut remote.rtc,
            remote_addr,
            &mut now,
            start,
            &mut left_discard,
            &mut on_pipe,
        );
        let alice_n = on_pipe
            .iter()
            .filter(|data| &*data.data == alice_frame)
            .count();
        let bob_n = on_pipe
            .iter()
            .filter(|data| &*data.data == bob_frame)
            .count();
        (alice_n, bob_n, seeded)
    }

    /// Both cascade entries are gone before the redial. The far end drops
    /// when ICE stays down. The first end redials at once, then each
    /// origin's RTP arrives on the new pipe.
    fn both_ends_drop_then_redial(
        dialer: &mut CascadeManager,
        answerer: &mut CascadeManager,
        answer: &mut SfuPeer,
        room: &RoomId,
        ended: Instant,
        old_id: &ParticipantId,
    ) {
        let key = room.to_string();
        assert!(dialer.has_pipe(&key, "did:key:nodeA"));
        assert!(answerer.has_pipe(&key, "did:key:nodeB"));

        answer.on_ice_state(IceConnectionState::Disconnected, ended);
        assert!(
            !answer.stayed_disconnected(ended + ICE_DISCONNECT_GRACE - Duration::from_millis(1))
        );
        let drop_at = ended + ICE_DISCONNECT_GRACE;
        assert!(
            answer.stayed_disconnected(drop_at),
            "the other end drops once ICE stays down"
        );
        answer.rtc.disconnect();
        answerer.on_pipe_died(&key, "did:key:nodeB", "drop", drop_at);
        dialer.on_pipe_died(&key, "did:key:nodeA", "offer", ended);
        assert!(
            answerer.pipe_meta(&key, "did:key:nodeB").is_none()
                && !answerer.has_pipe(&key, "did:key:nodeB"),
            "the far end's pipe entry is gone before the redial"
        );
        assert!(
            dialer.pipe_meta(&key, "did:key:nodeA").is_none()
                && !dialer.has_pipe(&key, "did:key:nodeA"),
            "the dialler's pipe entry is gone before the redial"
        );

        dialer.set_now(ended);
        let (mut redial, offer) = dialer
            .establish_pipe("did:key:nodeA", room)
            .expect("the first end redials at once");
        assert_ne!(&redial.id, old_id);

        let (mut remote, answer_signal) = answerer
            .handle_pipe_offer("did:key:nodeB", &key, &pipe_offer_sdp(offer))
            .expect("the other end accepts the redial");
        let answer_json = pipe_answer_sdp(answer_signal);
        let (_pid, json) = dialer
            .handle_pipe_answer("did:key:nodeA", &key, &answer_json)
            .unwrap();
        redial.accept_current_answer(&json).unwrap();

        let (alice_n, bob_n, _) = rtp_counts_for_control_pipe(
            room,
            redial,
            &mut remote,
            "192.168.0.22:41022".parse().unwrap(),
            "192.168.0.21:41021".parse().unwrap(),
        );
        assert!(
            alice_n >= 1,
            "RTP for alice did not arrive on the new pipe ({alice_n} packets)"
        );
        assert!(
            bob_n >= 1,
            "RTP for bob did not arrive on the new pipe ({bob_n} packets)"
        );
    }

    /// A pipe dialled again while the room already has media is control-only.
    /// Becoming usable seeds the existing origins, and each origin's RTP
    /// arrives on that new pipe.
    #[test]
    fn a_redialled_control_only_pipe_is_seeded_and_carries_each_origins_rtp() {
        let (mut dialer, mut answerer, dial, answer, room) = connected_cascade_pipe();
        drop((dial, answer));
        let key = room.to_string();
        let now = Instant::now();
        // A timeout is not an ended offer, so the redial is not waiting out
        // a backoff. The new pipe is still only the control channel.
        dialer.on_pipe_died(&key, "did:key:nodeA", "timeout", now);
        answerer.on_pipe_died(&key, "did:key:nodeB", "timeout", now);
        let (mut redial, offer) = dialer
            .establish_pipe("did:key:nodeA", &room)
            .expect("the timeout redials at once");
        let (mut remote, answer_signal) = answerer
            .handle_pipe_offer("did:key:nodeB", &key, &pipe_offer_sdp(offer))
            .unwrap();
        let answer_json = pipe_answer_sdp(answer_signal);
        let (_pid, json) = dialer
            .handle_pipe_answer("did:key:nodeA", &key, &answer_json)
            .unwrap();
        redial.accept_current_answer(&json).unwrap();
        assert!(
            redial.tracks_out.is_empty(),
            "the redialled pipe negotiated only its control channel"
        );

        let (alice_n, bob_n, seeded) = rtp_counts_for_control_pipe(
            &room,
            redial,
            &mut remote,
            "192.168.0.22:41022".parse().unwrap(),
            "192.168.0.21:41021".parse().unwrap(),
        );
        assert!(
            seeded >= 2,
            "a redialled control-only pipe is seeded with the existing tracks (got {seeded})"
        );
        assert!(
            alice_n >= 1,
            "alice's RTP arrived on the new pipe ({alice_n} packets)"
        );
        assert!(
            bob_n >= 1,
            "bob's RTP arrived on the new pipe ({bob_n} packets)"
        );
    }

    /// Hand `sdp` to the remote pipe and drop the answer it produces. The
    /// sender keeps its pending offer: that is the lost-answer state.
    fn hand_the_remote_the_offer_and_drop_the_answer(remote: &mut SfuPeer, sdp: &str) {
        let offer: SdpOffer = serde_json::from_str(sdp).expect("the offer is SDP");
        let _answer = remote
            .rtc
            .sdp_api()
            .accept_offer(offer)
            .expect("the remote takes the offer; the answer is dropped");
    }

    /// Lose the answers to an established pipe's offer, and both resends.
    /// Each one is delivered to the remote. The pipe ends on both ends, the
    /// cascade redials, and each origin's audio is on the new pipe.
    #[test]
    fn an_unanswered_established_pipe_ends_and_the_cascade_redials() {
        let now = Instant::now();
        let (mut dialer, mut answerer, mut dial, mut answer, room) = connected_cascade_pipe();
        let (sdp, mapping) = offer_two_origins(&mut dial, now);
        let offer_id = dial.pending_offer_id().unwrap().to_string();
        hand_the_remote_the_offer_and_drop_the_answer(&mut answer, &sdp);
        assert_eq!(
            dial.pending_offer_id().map(str::to_string),
            Some(offer_id.clone()),
            "the answer to the offer was dropped"
        );
        let old_id = dial.id.clone();
        let pid = dial.id.clone();
        let mut peers = HashMap::from([(pid.clone(), dial)]);

        let at_10 = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(at_10.len(), 1);
        assert_eq!(at_10[0].sdp, sdp);
        assert_same_mapping(&at_10[0].track_mapping, &mapping);
        hand_the_remote_the_offer_and_drop_the_answer(&mut answer, &at_10[0].sdp);
        assert_eq!(
            peers[&pid].pending_offer_id(),
            Some(offer_id.as_str()),
            "the answer to the first resend was dropped"
        );
        let at_30 = sweep_offer_deadlines(&mut peers, now + Duration::from_secs(30));
        assert_eq!(at_30.len(), 1);
        assert_eq!(at_30[0].sdp, sdp);
        hand_the_remote_the_offer_and_drop_the_answer(&mut answer, &at_30[0].sdp);
        assert_eq!(
            peers[&pid].pending_offer_id(),
            Some(offer_id.as_str()),
            "the answer to the second resend was dropped"
        );
        assert!(apply_deferred_tracks(peers.get_mut(&pid).unwrap(), now).is_none());

        let ended = now + Duration::from_secs(60);
        let at_60 = sweep_offer_deadlines(&mut peers, ended);
        assert!(
            at_60.is_empty(),
            "the pipe ends instead of sending a replacement offer"
        );
        let mut dial = peers.remove(&pid).unwrap();
        assert!(!dial.rtc.is_alive());
        assert_eq!(dial.drop_reason(), Some("offer"));
        assert!(
            dial.pending.is_some(),
            "the dying pipe does not clear the token into a new offer"
        );
        assert!(
            apply_deferred_tracks(&mut dial, ended).is_none(),
            "the drain does not publish a replacement on the dying pipe"
        );

        both_ends_drop_then_redial(
            &mut dialer,
            &mut answerer,
            &mut answer,
            &room,
            ended,
            &old_id,
        );
    }

    /// An answer str0m rejects locally, for an offer whose shape it had
    /// accepted, ends the pipe the same way. A remote that rejects an
    /// offer only logs it, and this end then waits until the offer goes
    /// unanswered.
    #[test]
    fn a_locally_rejected_answer_ends_the_pipe_and_the_cascade_redials() {
        let now = Instant::now();
        let (mut dialer, mut answerer, mut dial, mut answer, room) = connected_cascade_pipe();
        let (sdp, _mapping) = offer_two_origins(&mut dial, now);
        let old_id = dial.id.clone();
        let offer: SdpOffer = serde_json::from_str(&sdp).unwrap();
        let real = answer
            .rtc
            .sdp_api()
            .accept_offer(offer)
            .expect("the far end can answer this offer");
        let answer_json = serde_json::to_string(&real).unwrap();
        let rejected = with_other_ice_ufrag(&answer_json);
        assert_ne!(rejected, answer_json);
        let id = dial.pending_offer_id().unwrap().to_string();
        let verdict = dial.settle_answer(&id, &rejected);
        assert!(!verdict.accepted);
        assert!(!dial.rtc.is_alive());
        assert_eq!(dial.drop_reason(), Some("offer"));
        assert!(
            dial.pending.is_none(),
            "str0m took the token when it rejected the answer locally"
        );
        assert!(
            apply_deferred_tracks(&mut dial, now).is_none(),
            "the locally rejected offer is not replaced on the dying pipe"
        );

        both_ends_drop_then_redial(&mut dialer, &mut answerer, &mut answer, &room, now, &old_id);
    }

    /// A `Connected` event already queued when the establishment window
    /// ends is applied before the sweep. The pipe is not dropped as
    /// never-established.
    #[test]
    fn a_connected_event_at_the_establishment_edge_keeps_the_pipe() {
        let (_caller, mut peer) = connected_pair();
        peer.is_pipe = true;
        let edge = Instant::now();
        peer.created_at = edge - PEER_ESTABLISH_TIMEOUT;
        assert!(
            peer.never_established(edge),
            "before the poll the peer looks never-established"
        );
        assert!(
            !establishment_expired_after_poll(&mut peer, edge, Some(IceConnectionState::Connected)),
            "the connected event is seen, so the pipe is not swept"
        );
        assert!(peer.had_path);
        assert!(peer.rtc.is_alive());
        assert!(!peer.never_established(edge));
        assert!(!peer.stayed_disconnected(edge));
    }

    /// The offer deadline and the ICE disconnect grace can run out on one
    /// tick. The pipe ended for its offer, and the sweep must keep that
    /// reason: `"drop"` would let the cascade redial without the backoff.
    #[test]
    fn the_sweep_keeps_the_reason_of_a_peer_already_ended_for_its_offer() {
        let (_caller, mut peer) = connected_pair();
        peer.is_pipe = true;
        let now = Instant::now();
        peer.disconnected_since = Some(now - ICE_DISCONNECT_GRACE);
        peer.ice_restart = None;
        assert!(peer.stayed_disconnected(now), "the grace has run out");
        peer.end_for_offer("unanswered");
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        end_expired_peers(&mut peers, now);
        assert_eq!(
            peers[&pid].dead_peer(now).reason,
            "offer",
            "an ended offer is not reclassified as a drop"
        );
    }

    /// A remote offer str0m refuses still bumps the change id, so the local
    /// offer can no longer be answered. Its tracks are offered again, and
    /// the track that was already negotiated keeps its writer.
    #[test]
    fn a_pipe_whose_accept_offer_fails_offers_its_deferred_tracks_again() {
        let now = Instant::now();
        let (mut pipe, _remote, established) = negotiated_pipe_ends();
        pipe.on_ice_state(IceConnectionState::Connected, now);
        let (_sdp, _, unanswered) = offer_outbound_audio(&mut pipe, now);
        defer_extra_audio(&mut pipe);
        let bare = SdpOffer::from_sdp_string("v=0\r\no=- 0 0 IN IP4 0.0.0.0\r\ns=-\r\nt=0 0\r\n")
            .expect("a session-only offer parses");
        let json = serde_json::to_string(&bare).unwrap();
        let room_id = pipe.room_id.to_string();
        apply_pipe_renegotiation_offer(
            &mut pipe,
            &json,
            "did:key:node-a".to_string(),
            room_id,
            &[],
        );
        assert!(
            pipe.pending.is_none(),
            "a remote offer rejected locally does not leave the local offer pending"
        );
        assert!(pipe.rtc.is_alive());
        assert_audio_writer(&mut pipe, established);
        assert!(pipe.rtc.writer(unanswered).is_none());
        let (again, _) =
            apply_deferred_tracks(&mut pipe, now).expect("deferred tracks are offered again");
        assert_ne!(again, json);
    }

    /// Resume of a peer whose RTC has already closed is unlisted, and it
    /// does not spend a resend of the offer still recorded. A live peer
    /// with nothing pending is listed.
    #[test]
    fn a_resume_of_a_closed_rtc_is_unlisted() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        offer_outbound_audio(&mut peer, now);
        let resends = peer.pending.as_ref().unwrap().resends;
        peer.rtc.disconnect();
        let reply = resume_loop_reply(&mut peer, now);
        assert!(!reply.listed, "a closed RTC is not resumable");
        assert!(reply.offer.is_none());
        assert!(peer.pending.is_some());
        assert_eq!(peer.pending.as_ref().unwrap().resends, resends);

        let (_caller, mut live) = connected_pair();
        let reply = resume_loop_reply(&mut live, now);
        assert!(reply.listed);
        assert!(reply.offer.is_none());
    }

    /// A cascade pipe whose answer never arrives has no remote candidate, so
    /// ICE never leaves `Checking` and never reports `Disconnected`. Before
    /// this PR the 70 s offer expiry ended it; without that, nothing did, and
    /// `has_pipe` kept the node from dialling again. A peer that never had a
    /// path is dropped [`PEER_ESTABLISH_TIMEOUT`] after it was made; the
    /// dead-pipe cleanup removes its entry, and the next announce dials a
    /// new pipe.
    #[test]
    fn a_pipe_that_never_gets_a_path_is_dropped_and_can_be_replaced() {
        let establish = PEER_ESTABLISH_TIMEOUT;
        let mut mgr = super::super::cascade::CascadeManager::new(
            "did:key:nodeB".into(),
            "127.0.0.1:10040".parse::<std::net::SocketAddr>().unwrap(),
            8,
        );
        let room = RoomId::new("neighbourhood://pipe", "r");
        let (mut pipe, _offer) = mgr.establish_pipe("did:key:nodeA", &room).unwrap();
        let start = Instant::now();
        let (mut seen, mut sent) = (Seen::default(), Vec::new());
        let mut now = start;
        while now - start < establish + Duration::from_secs(5) {
            let next = drive(&mut pipe.rtc, now, start, &mut seen, &mut sent);
            sent.clear();
            for (_, state) in seen.ice.drain(..) {
                pipe.on_ice_state(state, now);
            }
            now = next.clamp(now + Duration::from_millis(1), now + Duration::from_secs(1));
        }
        assert!(pipe.rtc.is_alive(), "str0m itself never gives up on it");
        assert!(
            pipe.disconnected_since.is_none(),
            "ICE never reported a loss"
        );
        assert!(!pipe.stayed_disconnected(start + establish - Duration::from_secs(1)));
        assert!(
            pipe.stayed_disconnected(start + establish),
            "a pipe with no path for {} s is dropped",
            establish.as_secs()
        );

        // What the cascade cleanup does with the dead pipe.
        assert!(mgr.has_pipe(&room.to_string(), "did:key:nodeA"));
        mgr.remove_node_from_room(&room.to_string(), "did:key:nodeA");
        assert!(!mgr.has_pipe(&room.to_string(), "did:key:nodeA"));
        let (replacement, _) = mgr
            .establish_pipe("did:key:nodeA", &room)
            .expect("a new pipe can be dialled");
        assert_ne!(replacement.id, pipe.id);
    }

    /// One prior drop, then a connected stretch of `up_for`, then the
    /// `stayed_disconnected` death. Returns the redial the cleanup allows
    /// at that death. The dialler is the higher DID, so the drop counts.
    /// The clock is the same injectable `Instant` the backoff tests use.
    fn redial_after_connected_drop(
        up_for: Duration,
    ) -> Result<(SfuPeer, super::super::cascade::CascadeSignal), String> {
        let remote = "did:key:nodeA";
        let local = "did:key:nodeB";
        assert!(local > remote, "the local DID has to be the end that dials");
        let room = RoomId::new("nh", "healthy-drop");
        let key = room.to_string();
        let t0 = Instant::now();
        let mut mgr = CascadeManager::new(
            local.into(),
            "127.0.0.1:10180".parse::<std::net::SocketAddr>().unwrap(),
            8,
        );
        mgr.set_now(t0);
        mgr.establish_pipe(remote, &room).unwrap();
        mgr.on_pipe_died(&key, remote, "drop", t0);
        let (mut peer, _) = mgr
            .establish_pipe(remote, &room)
            .expect("the first drop redials at once");

        let up = t0;
        peer.on_ice_state(IceConnectionState::Connected, up);
        let down = up + up_for;
        peer.on_ice_state(IceConnectionState::Disconnected, down);
        let drop_at = down + ICE_DISCONNECT_GRACE;
        assert!(
            !peer.never_established(drop_at),
            "the pipe had a path, so this is not an establish timeout"
        );
        assert!(
            peer.stayed_disconnected(drop_at),
            "the drop death comes from stayed_disconnected"
        );

        // The sweep names the death. Cleanup resets on the bool the peer
        // recorded when the stretch ended, not on a clock it no longer has.
        peer.end_reason = Some("drop");
        let dead = peer.dead_peer(drop_at);
        assert_eq!(dead.reason, "drop");
        assert_eq!(
            dead.healthy_stretch,
            up_for >= super::super::cascade::PIPE_HEALTHY_RESET,
            "the drop carries whether the stretch that ended reached the healthy interval"
        );
        mgr.set_now(drop_at);
        mgr.apply_pipe_death(
            &dead.participant_id,
            &dead.room_id.to_string(),
            &dead.agent_did,
            dead.reason,
            drop_at,
            dead.healthy_stretch,
        );
        mgr.establish_pipe(remote, &room)
    }

    /// Connected for the healthy interval, then a drop: the backoff count
    /// resets and the next redial is free.
    #[test]
    fn a_drop_after_the_healthy_interval_redials_at_once() {
        redial_after_connected_drop(super::super::cascade::PIPE_HEALTHY_RESET)
            .expect("a pipe connected for the healthy interval redials at once");
    }

    /// Connected for 111 s, under the healthy interval. The 10 s disconnect
    /// grace would put the death at 121 s, so counting that grace would
    /// reset. The next redial still backs off.
    #[test]
    fn a_drop_after_111_seconds_keeps_the_backoff() {
        let short = Duration::from_secs(111);
        assert!(short < super::super::cascade::PIPE_HEALTHY_RESET);
        assert!(
            short + ICE_DISCONNECT_GRACE > super::super::cascade::PIPE_HEALTHY_RESET,
            "111 s plus the disconnect grace passes the healthy interval"
        );
        let err = redial_after_connected_drop(short)
            .expect_err("a pipe connected for 111 s keeps its backoff");
        assert!(
            err.starts_with("pipe redial waits"),
            "the next redial backs off, got: {err}"
        );
    }

    /// Two stretches of 60 s with a disconnect between them add up to the
    /// healthy interval only if the gap counts. It does not, so the drop
    /// keeps the backoff.
    #[test]
    fn a_disconnect_gap_does_not_add_up_to_a_healthy_stretch() {
        let remote = "did:key:nodeA";
        let local = "did:key:nodeB";
        let room = RoomId::new("nh", "healthy-gap");
        let key = room.to_string();
        let t0 = Instant::now();
        let mut mgr = CascadeManager::new(
            local.into(),
            "127.0.0.1:10181".parse::<std::net::SocketAddr>().unwrap(),
            8,
        );
        mgr.set_now(t0);
        mgr.establish_pipe(remote, &room).unwrap();
        mgr.on_pipe_died(&key, remote, "drop", t0);
        let (mut peer, _) = mgr
            .establish_pipe(remote, &room)
            .expect("the first drop redials at once");

        let up = t0;
        peer.on_ice_state(IceConnectionState::Connected, up);
        let down = up + Duration::from_secs(60);
        peer.on_ice_state(IceConnectionState::Disconnected, down);
        assert!(!peer.healthy_stretch, "60 s is under the healthy interval");
        let back = down + Duration::from_secs(1);
        peer.on_ice_state(IceConnectionState::Connected, back);
        let down_again = back + Duration::from_secs(60);
        peer.on_ice_state(IceConnectionState::Disconnected, down_again);
        assert!(
            !peer.healthy_stretch,
            "a disconnected gap does not join two short stretches"
        );
        let drop_at = down_again + ICE_DISCONNECT_GRACE;
        assert!(drop_at.saturating_duration_since(up) > super::super::cascade::PIPE_HEALTHY_RESET);
        assert!(peer.stayed_disconnected(drop_at));
        peer.end_reason = Some("drop");
        let dead = peer.dead_peer(drop_at);
        assert!(!dead.healthy_stretch);
        mgr.set_now(drop_at);
        mgr.apply_pipe_death(
            &dead.participant_id,
            &dead.room_id.to_string(),
            &dead.agent_did,
            dead.reason,
            drop_at,
            dead.healthy_stretch,
        );
        let err = mgr
            .establish_pipe(remote, &room)
            .expect_err("two short stretches do not reset the backoff");
        assert!(err.starts_with("pipe redial waits"), "got: {err}");
    }

    /// An offer-ended pipe is still connected, so the open stretch counts.
    /// One that reached the healthy interval redials at once. One of 111 s
    /// does not.
    #[test]
    fn an_offer_end_uses_the_open_stretch() {
        for (up_for, free) in [
            (super::super::cascade::PIPE_HEALTHY_RESET, true),
            (Duration::from_secs(111), false),
        ] {
            let remote = "did:key:nodeA";
            let local = "did:key:nodeB";
            let room = RoomId::new("nh", "healthy-offer");
            let key = room.to_string();
            let t0 = Instant::now();
            let mut mgr = CascadeManager::new(
                local.into(),
                "127.0.0.1:10182".parse::<std::net::SocketAddr>().unwrap(),
                8,
            );
            mgr.set_now(t0);
            mgr.establish_pipe(remote, &room).unwrap();
            mgr.on_pipe_died(&key, remote, "drop", t0);
            let (mut peer, _) = mgr
                .establish_pipe(remote, &room)
                .expect("the first drop redials at once");
            peer.on_ice_state(IceConnectionState::Connected, t0);
            let ended = t0 + up_for;
            assert!(!peer.stayed_disconnected(ended));
            peer.end_reason = Some("offer");
            let dead = peer.dead_peer(ended);
            assert_eq!(dead.reason, "offer");
            assert_eq!(dead.healthy_stretch, free);
            mgr.set_now(ended);
            mgr.apply_pipe_death(
                &dead.participant_id,
                &dead.room_id.to_string(),
                &dead.agent_did,
                dead.reason,
                ended,
                dead.healthy_stretch,
            );
            let redial = mgr.establish_pipe(remote, &room);
            if free {
                redial.expect("an offer end after the healthy interval redials at once");
            } else {
                let err = redial.expect_err("an offer end after 111 s keeps the backoff");
                assert!(err.starts_with("pipe redial waits"), "got: {err}");
            }
        }
    }

    /// A disconnected stretch does not stay inside the pipe's usable
    /// interval. The next connect starts that interval again.
    #[test]
    fn a_disconnect_clears_the_pipes_usable_interval() {
        let (_caller, mut peer) = connected_pair();
        peer.is_pipe = true;
        let up = Instant::now();
        peer.on_ice_state(IceConnectionState::Connected, up);
        assert_eq!(peer.usable_since, Some(up));
        let down = up + Duration::from_secs(111);
        peer.on_ice_state(IceConnectionState::Disconnected, down);
        assert!(
            peer.usable_since.is_none(),
            "a disconnected stretch is not part of the healthy interval"
        );
        assert!(
            !peer.healthy_stretch,
            "111 s does not mark the stretch healthy"
        );
        let back = down + Duration::from_secs(1);
        peer.on_ice_state(IceConnectionState::Connected, back);
        assert_eq!(peer.usable_since, Some(back));
        peer.on_ice_state(IceConnectionState::Completed, back + Duration::from_secs(1));
        assert_eq!(
            peer.usable_since,
            Some(back),
            "a later connected state in the same stretch does not move the start"
        );
    }

    /// The establishment timeout only ends a peer that never had a path. One
    /// that connected and then goes quiet (no media, no ICE change) stays.
    #[test]
    fn an_established_peer_that_goes_quiet_is_not_hit_by_the_establishment_timeout() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.on_ice_state(IceConnectionState::Checking, now);
        peer.on_ice_state(IceConnectionState::Connected, now + Duration::from_secs(1));
        for minutes in [1, 5, 30] {
            assert!(!peer.stayed_disconnected(now + Duration::from_secs(60 * minutes)));
        }
        assert!(!peer.media_flowing(now + Duration::from_secs(60)));
    }

    /// No media, an unanswered offer, and ICE down: the ICE rules still end
    /// the peer, 10 s after ICE went `Disconnected`, or 15 s after an ICE
    /// restart request. The offer resends keep neither alive.
    #[test]
    fn a_silent_peer_with_ice_down_still_ends_by_the_ice_rules() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        offer_outbound_audio(&mut peer, now);
        peer.note_media(now);
        let down = now + Duration::from_secs(5);
        peer.on_ice_state(IceConnectionState::Disconnected, down);
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let resent = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(resent.len(), 1);
        let peer = &peers[&pid];
        assert!(!peer.media_flowing(down + ICE_DISCONNECT_GRACE));
        assert!(!peer.stayed_disconnected(down + ICE_DISCONNECT_GRACE - Duration::from_millis(1)));
        assert!(peer.stayed_disconnected(down + ICE_DISCONNECT_GRACE));

        let (_caller, mut peer) = connected_pair();
        offer_outbound_audio(&mut peer, now);
        peer.on_ice_state(IceConnectionState::Disconnected, now);
        let asked = now + Duration::from_secs(8);
        peer.restart_ice(asked);
        assert!(!peer.stayed_disconnected(asked + ICE_RESTART_GRACE - Duration::from_millis(1)));
        assert!(peer.stayed_disconnected(asked + ICE_RESTART_GRACE));
    }

    /// A track that arrives while an offer waits stays deferred across the
    /// resend, then negotiates on its own offer after the first answer.
    #[test]
    fn a_track_added_during_backoff_waits_for_the_first_answer() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, client) = connected_pair();
        let client_id = client.id.clone();
        let room = client.room_id.clone();
        let sender_id = ParticipantId::next();
        let mut sender = SfuPeer::new(
            sender_id.clone(),
            room,
            "did:key:alice".to_string(),
            Rtc::new(Instant::now()),
            false,
        );
        let first_origin = Mid::from("a1");
        let second_origin = Mid::from("a2");
        sender.tracks_in.insert(first_origin, MediaKind::Audio);
        sender.tracks_in.insert(second_origin, MediaKind::Audio);
        let mut peers = HashMap::from([(client_id.clone(), client), (sender_id.clone(), sender)]);

        let offers = offer_new_tracks(
            &mut peers,
            HashMap::from([(
                client_id.clone(),
                vec![(MediaKind::Audio, sender_id.clone(), first_origin)],
            )]),
        );
        assert_eq!(offers.len(), 1);
        let sdp = offers[0].1.clone();
        let mapping = offers[0].2.clone();
        let mid = Mid::from(mapping[0].mid.as_str());
        // `offer_new_tracks` stamps the deadline from `Instant::now()`.
        let deadline = peers[&client_id].pending.as_ref().unwrap().next_deadline;

        let more = offer_new_tracks(
            &mut peers,
            HashMap::from([(
                client_id.clone(),
                vec![(MediaKind::Audio, sender_id.clone(), second_origin)],
            )]),
        );
        assert!(more.is_empty(), "the second track waits");
        assert_eq!(peers[&client_id].deferred_tracks.len(), 1);
        assert_eq!(peers[&client_id].tracks_out.len(), 1);

        let resend = sweep_offer_deadlines(&mut peers, deadline);
        assert_eq!(resend.len(), 1);
        assert_eq!(resend[0].sdp, sdp);
        assert_same_mapping(&resend[0].track_mapping, &mapping);
        assert_eq!(peers[&client_id].deferred_tracks.len(), 1);
        assert_eq!(peers[&client_id].tracks_out.len(), 1);

        let client = peers.get_mut(&client_id).unwrap();
        let answer = answer_json(&mut caller, &sdp);
        client.accept_current_answer(&answer).unwrap();
        assert!(client.pending.is_none());
        assert_audio_writer(client, mid);

        let (second_sdp, second_mapping) =
            apply_deferred_tracks(client, Instant::now()).expect("the deferred track is offered");
        assert_eq!(second_mapping.len(), 1);
        assert_ne!(second_mapping[0].mid, mapping[0].mid);
        let second_mid = Mid::from(second_mapping[0].mid.as_str());
        assert!(client.rtc.writer(second_mid).is_none());
        let second_answer = answer_json(&mut caller, &second_sdp);
        client.accept_current_answer(&second_answer).unwrap();
        exchange_until(
            &mut caller,
            &mut client.rtc,
            bind,
            caller_addr,
            Instant::now(),
            Duration::from_millis(300),
        );
        assert_audio_writer(client, mid);
        assert_audio_writer(client, second_mid);
        assert_eq!(client.tracks_out.len(), 2);
        assert!(caller.media(mid).unwrap().kind().is_audio());
        assert!(caller.media(second_mid).unwrap().kind().is_audio());
    }

    /// An answer before the deadline clears the pending offer. A later tick
    /// does not send it again.
    #[test]
    fn an_answer_during_backoff_cancels_the_resend() {
        let (bind, caller_addr) = pair_addrs();
        let (mut caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, _, mid) = offer_outbound_audio(&mut peer, now);
        assert!(peer.rtc.writer(mid).is_none());
        peer.accept_current_answer(&answer_json(&mut caller, &sdp))
            .unwrap();
        assert!(peer.pending.is_none());
        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Wait
        ));
        assert!(matches!(
            peer.on_offer_deadline(now + Duration::from_secs(70)),
            OfferDeadline::Wait
        ));
        exchange_until(
            &mut caller,
            &mut peer.rtc,
            bind,
            caller_addr,
            now,
            Duration::from_millis(300),
        );
        assert_audio_writer(&mut peer, mid);
        assert!(caller.media(mid).unwrap().kind().is_audio());
    }

    /// JSON that does not parse puts the whole pending record back.
    #[test]
    fn a_malformed_answer_keeps_the_pending_offer() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        let (sdp, mapping, _) = offer_outbound_audio(&mut peer, now);
        let deadline = peer.pending.as_ref().unwrap().next_deadline;
        let err = peer.accept_current_answer("{").unwrap_err();
        assert!(matches!(err, ServerAnswerError::Parse(_)));
        let pending = peer.pending.as_ref().unwrap();
        assert_eq!(pending.sdp, sdp);
        assert_same_mapping(&pending.track_mapping, &mapping);
        assert_eq!(pending.resends, 0);
        assert_eq!(pending.next_deadline, deadline);
        assert!(matches!(peer.on_offer_deadline(now), OfferDeadline::Wait));
    }

    /// Restart requests while an offer waits set a follow-up. They do not
    /// publish, and they do not spend or reset the resend budget.
    #[test]
    fn restart_requests_do_not_bypass_the_resend_budget() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.restart_ice(now);
        let (sdp, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        for _ in 0..20 {
            peer.restart_ice(now);
        }
        assert!(peer.ice_restart.as_ref().unwrap().again);
        assert_eq!(peer.pending.as_ref().unwrap().resends, 0);
        assert_eq!(peer.pending.as_ref().unwrap().sdp, sdp);
        assert!(matches!(
            peer.on_offer_deadline(now + Duration::from_secs(9)),
            OfferDeadline::Wait
        ));
        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Resend
        ));
        assert_eq!(peer.pending.as_ref().unwrap().sdp, sdp);
        assert_eq!(peer.pending.as_ref().unwrap().resends, 1);
    }

    /// A resend does not move the ICE restart clock. The disconnect grace
    /// still ends at the original request.
    #[test]
    fn an_offer_resend_does_not_extend_the_ice_restart_grace() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.on_ice_state(IceConnectionState::Disconnected, now);
        peer.restart_ice(now);
        let (sdp, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        assert!(matches!(
            peer.on_offer_deadline(now + OFFER_RETRY_BASE),
            OfferDeadline::Resend
        ));
        assert_eq!(peer.pending.as_ref().unwrap().sdp, sdp);
        assert_eq!(peer.ice_restart.as_ref().unwrap().since, now);
        assert!(peer.stayed_disconnected(now + ICE_RESTART_GRACE));
    }

    /// The first resend of a lost restart offer is published while the
    /// restart grace still holds the peer. The grace then ends on the
    /// original request clock.
    #[test]
    fn a_restart_resend_is_published_while_the_peer_is_kept() {
        let (_caller, mut peer) = connected_pair();
        let now = Instant::now();
        peer.on_ice_state(IceConnectionState::Disconnected, now);
        peer.restart_ice(now);
        let (sdp, _) = apply_deferred_tracks(&mut peer, now).expect("the restart is offered");
        let pid = peer.id.clone();
        let mut peers = HashMap::from([(pid.clone(), peer)]);
        let due = sweep_offer_deadlines(&mut peers, now + OFFER_RETRY_BASE);
        assert_eq!(due.len(), 1);
        assert_eq!(due[0].sdp, sdp);
        assert!(
            !peers[&pid].stayed_disconnected(now + OFFER_RETRY_BASE),
            "the peer is dropped before the resend can be published"
        );
        assert!(peers[&pid].rtc.is_alive());
        assert!(peers[&pid].stayed_disconnected(now + ICE_RESTART_GRACE));
    }

    /// Empty trickle text is end-of-candidates. A bad candidate stays an error.
    #[test]
    fn an_empty_end_of_candidates_is_not_a_candidate() {
        for sdp in [
            "",
            " ",
            "   ",
            "candidate:",
            "candidate:   ",
            "  candidate:  ",
        ] {
            assert!(
                parse_trickle_candidate(sdp).unwrap().is_none(),
                "{sdp:?} is end-of-candidates"
            );
        }
        let parsed =
            parse_trickle_candidate("candidate:1 1 udp 2113929471 203.0.113.100 10100 typ host")
                .unwrap()
                .expect("a host candidate");
        assert_eq!(
            parsed.addr(),
            "203.0.113.100:10100".parse::<SocketAddr>().unwrap()
        );
        for sdp in [
            "not-a-candidate",
            "1 1 udp 2113929471 203.0.113.100 10100 typ host",
            "candidate:garbage",
        ] {
            assert!(parse_trickle_candidate(sdp).is_err(), "{sdp:?}");
        }
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

    #[tokio::test]
    async fn a_preference_for_a_peer_that_left_leaves_nothing_behind() {
        let server = SfuServer::start(SfuServerConfig {
            bind_addr: "127.0.0.1:0".parse().unwrap(),
            ..Default::default()
        })
        .await
        .unwrap();
        let gone = ParticipantId::next();
        server
            .command_tx
            .send(SfuCommand::SetQualityPreference {
                participant_id: gone,
                preference: "low".into(),
            })
            .await
            .unwrap();
        // The loop handles commands in order: the reply follows the set.
        let (reply, kept) = tokio::sync::oneshot::channel();
        server
            .command_tx
            .send(SfuCommand::GetQualityPreferences { reply })
            .await
            .unwrap();
        assert!(kept.await.unwrap().is_empty());
        let _ = server.command_tx.send(SfuCommand::Shutdown).await;
    }

    /// A command sent to an idle loop (no packet, no timer due) is handled
    /// at once, not at the next 100 ms wake.
    #[tokio::test]
    async fn a_command_wakes_an_idle_loop_at_once() {
        let server = SfuServer::start(SfuServerConfig {
            bind_addr: "127.0.0.1:0".parse().unwrap(),
            ..Default::default()
        })
        .await
        .unwrap();
        let ask = || async {
            let (reply, answer) = tokio::sync::oneshot::channel();
            let sent = std::time::Instant::now();
            server
                .command_tx
                .send(SfuCommand::GetQualityPreferences { reply })
                .await
                .unwrap();
            answer.await.unwrap();
            sent.elapsed()
        };
        // The first answer proves the loop is running; it then waits in select.
        ask().await;
        let mut slowest = Duration::ZERO;
        for _ in 0..5 {
            slowest = slowest.max(ask().await);
        }
        assert!(
            slowest < Duration::from_millis(50),
            "a command waited {slowest:?} on an idle loop"
        );
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
            let sdp = serde_json::to_string(&offer).unwrap();
            end.peer
                .install_pending(pending, sdp, Vec::new(), Instant::now(), false);
            let answer = end.caller.sdp_api().accept_offer(offer).unwrap();
            end.peer
                .accept_current_answer(&serde_json::to_string(&answer).unwrap())
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
