//! Simulcast layer selection.
//!
//! A simulcast sender sends one video track as up to three layers, each an
//! RTP stream with its own rid ("high", "medium", "low"). The sender's
//! encoder decides which layers flow: it drops layers the resolution or the
//! bandwidth cannot carry. For each receiver of the track, the selector
//! forwards exactly one layer:
//!
//! - the layer the receiver's bandwidth budget allows (see below), at or
//!   below the receiver's ceiling; when no flowing layer is at or below it,
//!   the lowest flowing layer, so a track never goes black;
//! - a change of layer waits for a keyframe on the new layer — a decoder
//!   cannot start from a delta frame — and the old layer keeps flowing until
//!   then;
//! - a keyframe the switch needs is requested from the sender, for that rid,
//!   at most once per [`KEYFRAME_REQUEST_INTERVAL`].
//!
//! A packet without a rid comes from a sender without simulcast and always
//! passes.
//!
//! # Bandwidth budget
//!
//! A client receiver's transport reports an estimate of its available
//! bandwidth (str0m's TWCC estimator, [`LayerSelector::set_estimate`]). The
//! selector measures each incoming layer's bitrate ([`Meter`]) and, per
//! receiver, keeps a cap per video track:
//!
//! - a new track starts at the lowest flowing layer; low is the floor;
//! - the budget is [`BUDGET_FRACTION`] of the estimate minus what audio
//!   needs ([`AUDIO_RESERVE_BPS`] per audio track, or its measured rate).
//!   Audio is never touched;
//! - when the layers forwarded cost more than the budget, one track steps
//!   down one flowing layer: the one that saves the most;
//! - one track steps up one flowing layer only after the budget has held
//!   [`UPGRADE_HEADROOM`] times the cost with that layer for
//!   [`UPGRADE_HOLD`], and only once an estimate arrived that can probe;
//! - a layer switch counts the old layer's cost until it commits on a
//!   keyframe, and no second switch is stacked on its track.
//!
//! There is no timer. An allocation runs when a receiver's estimate arrives,
//! its tracks or ceiling change, or a switch commits, and at the one
//! [`LayerSelector::next_wake`] instant of a receiver with a pending upgrade
//! (the hold's end, or the moment a layer's rate is first measured). The
//! estimator is silent on a steady link, so its events cannot be the clock.
//!
//! A receiver the selector does not manage (a cascade pipe has no
//! estimate) gets the best layer its ceiling allows.

use std::collections::{HashMap, HashSet, VecDeque};
use std::time::{Duration, Instant};

use str0m::format::CodecExtra;
use str0m::media::{MediaTime, Mid, Rid};

use super::room::ParticipantId;

/// A layer that sent nothing for this long no longer flows.
const LAYER_TIMEOUT: Duration = Duration::from_secs(3);
/// The shortest time between two keyframe requests for one layer.
const KEYFRAME_REQUEST_INTERVAL: Duration = Duration::from_millis(500);
/// A layer's bitrate is the bytes of this long a window.
const RATE_WINDOW: Duration = Duration::from_secs(1);
/// Bytes the wire adds to each RTP packet (header and extensions, SRTP tag),
/// counted into a layer's cost once per packet.
const PACKET_OVERHEAD_BYTES: u64 = 32;
/// The share of a receiver's estimate the forwarded video and audio may use.
const BUDGET_FRACTION: f64 = 0.85;
/// An upgrade needs a budget this many times the cost with the new layer.
const UPGRADE_HEADROOM: f64 = 1.20;
/// How long that headroom must hold, without a dip, before an upgrade.
const UPGRADE_HOLD: Duration = Duration::from_secs(4);
/// What each forwarded audio track is budgeted at least.
const AUDIO_RESERVE_BPS: u64 = 64_000;
/// The layers' rids, lowest first. A layer's rank is its index.
const LAYERS: [&str; 3] = ["low", "medium", "high"];
const HIGHEST: usize = LAYERS.len() - 1;

/// One video track a participant sends.
type Track = (ParticipantId, Mid);

/// What to do with one packet for one receiver.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Route {
    pub forward: bool,
    /// Ask the sender for a keyframe on this layer.
    pub request_keyframe: Option<Rid>,
}

#[derive(Default)]
pub(crate) struct LayerSelector {
    /// What each track sends, measured.
    meters: HashMap<Track, TrackMeters>,
    /// What each receiver is budgeted.
    receivers: HashMap<ParticipantId, Receiver>,
    /// Bitrates the receivers' transports should probe for, changed since
    /// [`Self::take_demands`].
    demands: HashMap<ParticipantId, u64>,
    /// The layer each receiver gets of each track.
    forwarding: HashMap<(ParticipantId, Track), Rid>,
    /// When a keyframe was last asked for, per layer (`None`: a sender
    /// without simulcast).
    last_request: HashMap<(Track, Option<Rid>), Instant>,
    /// Each receiver's RTP clock of each track.
    timelines: HashMap<(ParticipantId, Track), Timeline>,
}

/// The packets of one layer (`None`: a track without simulcast, or audio).
struct Meter {
    /// The first packet of this stretch of the layer.
    first: Instant,
    last: Instant,
    /// (arrival, bytes on the wire) of the packets within [`RATE_WINDOW`].
    window: VecDeque<(Instant, u64)>,
}

impl Meter {
    /// A frame of `payload` bytes sent as `packets` RTP packets. A keyframe
    /// is not counted: it is a burst the sender makes when a receiver (this
    /// selector's own switches) or a loss asks for one, several times the
    /// size of the layer's steady frames, and a window it falls in would
    /// read the layer at two or three times its rate. The upgrade headroom
    /// covers the burst.
    ///
    /// True when this frame starts a new stretch (the layer stopped and
    /// resumed): its rate is unknown for a window.
    fn record(&mut self, now: Instant, payload: usize, packets: u64, keyframe: bool) -> bool {
        let restarted = now.saturating_duration_since(self.last) >= LAYER_TIMEOUT;
        if restarted {
            // The layer stopped and resumed: a new stretch.
            self.first = now;
            self.window.clear();
        }
        self.last = now;
        let bytes = if keyframe {
            0
        } else {
            payload as u64 + packets * PACKET_OVERHEAD_BYTES
        };
        self.window.push_back((now, bytes));
        while self
            .window
            .front()
            .is_some_and(|(at, _)| now.saturating_duration_since(*at) >= RATE_WINDOW)
        {
            self.window.pop_front();
        }
        restarted
    }

    fn flows(&self, now: Instant) -> bool {
        now.saturating_duration_since(self.last) < LAYER_TIMEOUT
    }

    /// Bits per second over the last [`RATE_WINDOW`], once the layer has
    /// flowed that long.
    fn rate(&self, now: Instant) -> Option<u64> {
        if !self.flows(now) || now.saturating_duration_since(self.first) < RATE_WINDOW {
            return None;
        }
        let bytes: u64 = self
            .window
            .iter()
            .filter(|(at, _)| now.saturating_duration_since(*at) < RATE_WINDOW)
            .map(|(_, bytes)| bytes)
            .sum();
        Some(bytes * 8)
    }
}

struct TrackMeters {
    audio: bool,
    layers: HashMap<Option<Rid>, Meter>,
    /// Which layers (bit = rank) flowed when the last packet arrived.
    live: u8,
}

impl TrackMeters {
    /// The flowing layers as a bit set by rank.
    fn live_set(&self, now: Instant) -> u8 {
        self.flowing(now)
            .iter()
            .enumerate()
            .filter(|(_, meter)| meter.is_some())
            .fold(0, |set, (rank, _)| set | 1 << rank)
    }

    /// The flowing layers of a simulcast track, by rank.
    fn flowing(&self, now: Instant) -> [Option<&Meter>; LAYERS.len()] {
        std::array::from_fn(|rank| {
            self.layers
                .get(&Some(Rid::from(LAYERS[rank])))
                .filter(|meter| meter.flows(now))
        })
    }

    fn simulcast(&self) -> bool {
        self.layers.keys().any(Option::is_some)
    }
}

/// The highest flowing layer at or below `cap`, else the lowest flowing one.
fn pick(flowing: &[Option<&Meter>], cap: usize) -> Option<usize> {
    (0..=cap.min(HIGHEST))
        .rev()
        .find(|&rank| flowing[rank].is_some())
        .or_else(|| flowing.iter().position(Option::is_some))
}

/// A receiver's bandwidth state.
struct Receiver {
    /// The receiver is budgeted by its estimate (a client peer). Else it gets
    /// the best layer its ceiling allows.
    bwe: bool,
    /// The highest layer rank the receiver asked for.
    ceiling: usize,
    /// The last estimate (bits per second) and whether it can probe.
    /// `None` until the first TWCC event: no upgrade before.
    estimate: Option<(u64, bool)>,
    /// The tracks the receiver is subscribed to and has had packets of.
    tracks: HashSet<Track>,
    /// The highest layer rank the budget allows each simulcast track.
    caps: HashMap<Track, usize>,
    /// Since when the budget has held for an upgrade.
    hold_since: Option<Instant>,
    /// When the next allocation is due without an event.
    wake: Option<Instant>,
    /// The last probing demand handed out.
    demand: u64,
}

impl Default for Receiver {
    fn default() -> Self {
        Self {
            bwe: false,
            ceiling: HIGHEST,
            estimate: None,
            tracks: HashSet::new(),
            caps: HashMap::new(),
            hold_since: None,
            wake: None,
            demand: 0,
        }
    }
}

impl Receiver {
    /// The cap of a track: the budget's, or the start (lowest layer for a
    /// budgeted receiver). Never above the ceiling.
    fn cap(&self, track: &Track) -> usize {
        let cap = self
            .caps
            .get(track)
            .copied()
            .unwrap_or(if self.bwe { 0 } else { HIGHEST });
        cap.min(self.ceiling)
    }
}

/// Set a receiver's cap of a track, and log a change with `why` (`None`: a
/// fallback committed).
fn set_cap(
    receiver: &ParticipantId,
    state: &mut Receiver,
    track: &Track,
    rank: usize,
    why: Option<CapReason>,
) {
    let old = state.cap(track);
    state.caps.insert(track.clone(), rank);
    if old != rank {
        log::debug!(
            "SFU layers: receiver {} track {}/{}: cap {} -> {} ({})",
            receiver,
            track.0,
            track.1,
            LAYERS[old],
            LAYERS[rank],
            why.map_or_else(
                || "a fallback committed".to_string(),
                |w| format!(
                    "estimate {} bit/s, budget {:.0}, spent {}, layer rates low/medium/high {:?}",
                    w.estimate, w.budget, w.spent, w.rates
                )
            ),
        );
    }
}

/// What a budget decision rests on, for the log.
struct CapReason {
    estimate: u64,
    budget: f64,
    spent: u64,
    /// The track's layers' measured rates, by rank.
    rates: [Option<u64>; LAYERS.len()],
}

impl CapReason {
    fn new(
        estimate: u64,
        budget: f64,
        spent: u64,
        flowing: &[Option<&Meter>],
        now: Instant,
    ) -> Self {
        Self {
            estimate,
            budget,
            spent,
            rates: std::array::from_fn(|rank| flowing[rank].and_then(|m| m.rate(now))),
        }
    }
}

/// A stable order of tracks, for ties.
fn track_key(track: &Track) -> (u64, Mid) {
    (track.0 .0, track.1)
}

/// Where a receiver's RTP clock of one track stands.
struct Timeline {
    /// The layer the clock follows now.
    rid: Rid,
    /// Added to that layer's RTP time.
    offset: i64,
    /// The last RTP time the receiver got, and when.
    last: u64,
    last_at: Instant,
}

/// Whether a frame is a keyframe, when the codec says.
pub(crate) fn is_keyframe(extra: &CodecExtra) -> Option<bool> {
    match extra {
        CodecExtra::Vp8(e) => Some(e.is_keyframe),
        CodecExtra::H264(e) => Some(e.is_keyframe),
        _ => None,
    }
}

impl LayerSelector {
    /// Record a frame of `track`: `payload` bytes in `packets` RTP packets,
    /// on layer `rid`; `keyframe` when it is a keyframe (see [`Meter::record`]).
    /// Call once per frame, before [`Self::route`].
    ///
    /// A change in the set of flowing layers of the track (one started, or
    /// one stopped) runs the allocation of the track's receivers: a layer
    /// that arrives after the last allocation is an upgrade candidate the
    /// estimator's silence would never reveal.
    #[allow(clippy::too_many_arguments)]
    pub fn observe(
        &mut self,
        now: Instant,
        track: &Track,
        rid: Option<&Rid>,
        payload: usize,
        packets: u64,
        keyframe: bool,
        audio: bool,
    ) {
        let meters = self
            .meters
            .entry(track.clone())
            .or_insert_with(|| TrackMeters {
                audio,
                layers: HashMap::new(),
                live: 0,
            });
        let restarted = meters
            .layers
            .entry(rid.copied())
            .or_insert_with(|| Meter {
                first: now,
                last: now,
                window: VecDeque::new(),
            })
            .record(now, payload, packets, keyframe);
        let live = meters.live_set(now);
        let before = std::mem::replace(&mut meters.live, live);
        // A layer that stopped and came back has no rate for a window: the
        // allocation of its receivers runs now, and arms its wake for then.
        if live == before {
            if restarted {
                self.allocate_track_receivers(now, track);
            }
            return;
        }
        log::debug!(
            "SFU layers: track {}/{}: live layers {:03b} -> {:03b} (bit 0 low, 1 medium, 2 high)",
            track.0,
            track.1,
            before,
            live
        );
        self.allocate_track_receivers(now, track);
    }

    /// Run the allocation of every budgeted receiver of `track`.
    fn allocate_track_receivers(&mut self, now: Instant, track: &Track) {
        let affected: Vec<ParticipantId> = self
            .receivers
            .iter()
            .filter(|(_, r)| r.bwe && r.tracks.contains(track))
            .map(|(id, _)| id.clone())
            .collect();
        for id in affected {
            self.allocate(now, &id);
        }
    }

    /// `receiver` is a client peer: its transport reports an estimate, and
    /// its layers follow the budget. Call when the peer is added.
    pub fn set_bwe(&mut self, receiver: &ParticipantId) {
        self.receivers.entry(receiver.clone()).or_default().bwe = true;
    }

    /// `receiver`'s transport estimated `bps` of available bandwidth, and
    /// whether it can probe for more (str0m's `BweKind::Twcc`).
    pub fn set_estimate(
        &mut self,
        now: Instant,
        receiver: &ParticipantId,
        bps: u64,
        can_probe: bool,
    ) {
        self.receivers.entry(receiver.clone()).or_default().estimate = Some((bps, can_probe));
        self.allocate(now, receiver);
    }

    /// `receiver` asked for at most the layer named by `preference` (`low`,
    /// `medium`; `high`, `auto` and anything else leave the budget to decide).
    pub fn set_ceiling(&mut self, now: Instant, receiver: &ParticipantId, preference: &str) {
        let ceiling = match preference {
            "low" => 0,
            "medium" => 1,
            _ => HIGHEST,
        };
        let state = self.receivers.entry(receiver.clone()).or_default();
        if state.ceiling == ceiling {
            return;
        }
        state.ceiling = ceiling;
        // A raised ceiling does not jump to a layer the budget never allowed.
        for cap in state.caps.values_mut() {
            *cap = (*cap).min(ceiling);
        }
        state.hold_since = None;
        self.allocate(now, receiver);
    }

    /// The receivers' probing demands that changed since the last call: the
    /// estimate (bits per second) that would let each go to its highest
    /// permitted layers, with an upgrade's headroom.
    pub fn take_demands(&mut self) -> HashMap<ParticipantId, u64> {
        std::mem::take(&mut self.demands)
    }

    /// The earliest instant a receiver's allocation is due without an event.
    pub fn next_wake(&self) -> Option<Instant> {
        self.receivers.values().filter_map(|r| r.wake).min()
    }

    /// Run the allocations whose [`Self::next_wake`] has come.
    pub fn run_due(&mut self, now: Instant) {
        let due: Vec<ParticipantId> = self
            .receivers
            .iter()
            .filter(|(_, r)| r.wake.is_some_and(|at| at <= now))
            .map(|(id, _)| id.clone())
            .collect();
        for id in due {
            self.allocate(now, &id);
        }
    }

    /// `receiver`'s tracks changed, or a switch committed: the hold starts
    /// over, and the budget is applied.
    fn restart(&mut self, now: Instant, receiver: &ParticipantId) {
        if let Some(state) = self.receivers.get_mut(receiver) {
            state.hold_since = None;
        }
        self.allocate(now, receiver);
    }

    /// Apply `receiver`'s budget to its tracks.
    fn allocate(&mut self, now: Instant, receiver: &ParticipantId) {
        let Some(mut state) = self.receivers.remove(receiver) else {
            return;
        };
        let demand = self.apply_budget(now, receiver, &mut state);
        if demand != state.demand {
            state.demand = demand;
            self.demands.insert(receiver.clone(), demand);
        }
        self.receivers.insert(receiver.clone(), state);
    }

    /// One allocation run; the probing demand.
    fn apply_budget(&self, now: Instant, receiver: &ParticipantId, state: &mut Receiver) -> u64 {
        /// A simulcast video track of the receiver.
        struct Video<'a> {
            track: Track,
            flowing: [Option<&'a Meter>; LAYERS.len()],
            /// The layer the budget and ceiling pick now.
            current: usize,
            /// The layer's cost, until a switch commits the old layer's.
            cost: u64,
            pending: bool,
        }
        let cost_of = |meter: Option<&Meter>| meter.and_then(|m| m.rate(now));

        state.wake = None;
        let mut fixed = 0u64;
        // The earliest first packet of a stretch whose rate is not known yet.
        let mut unmeasured: Option<Instant> = None;
        let mut videos: Vec<Video> = Vec::new();
        for track in &state.tracks {
            let Some(meters) = self.meters.get(track) else {
                continue;
            };
            if meters.audio {
                let meter = meters.layers.get(&None).filter(|m| m.flows(now));
                if let Some(meter) = meter {
                    fixed += AUDIO_RESERVE_BPS.max(meter.rate(now).unwrap_or(0));
                }
            } else if !meters.simulcast() {
                let meter = meters.layers.get(&None).filter(|m| m.flows(now));
                fixed += cost_of(meter).unwrap_or(0);
                // A video of no layers costs nothing until measured.
                if let Some(meter) = meter.filter(|m| m.rate(now).is_none()) {
                    unmeasured =
                        Some(unmeasured.map_or(meter.first, |at: Instant| at.min(meter.first)));
                }
            } else {
                let flowing = meters.flowing(now);
                let Some(current) = pick(&flowing, state.cap(track)) else {
                    continue;
                };
                // The old layer flows until the switch commits.
                let forwarded = self
                    .forwarding
                    .get(&(receiver.clone(), track.clone()))
                    .and_then(|rid| LAYERS.iter().position(|name| Rid::from(*name) == *rid))
                    .filter(|&rank| flowing[rank].is_some());
                let counted = forwarded.unwrap_or(current);
                videos.push(Video {
                    track: track.clone(),
                    flowing,
                    current,
                    cost: cost_of(flowing[counted]).unwrap_or(0),
                    pending: forwarded.is_some_and(|rank| rank != current),
                });
            }
        }
        videos.sort_by_key(|v| track_key(&v.track));

        // What it takes for each track to go to its highest permitted layer.
        let highest: u64 = videos
            .iter()
            .map(|v| {
                let rank = pick(&v.flowing, state.ceiling).unwrap_or(v.current);
                cost_of(v.flowing[rank]).unwrap_or(0)
            })
            .sum();
        // The estimate whose budget holds the upgrade's headroom over the
        // highest layers: `0.85 * E - fixed >= 1.20 * highest`.
        let demand =
            ((UPGRADE_HEADROOM * highest as f64 + fixed as f64) / BUDGET_FRACTION).ceil() as u64;

        // A layer's rate is known a window after its first packet. Until then
        // it counts as nothing: run again when the first is measured, so the
        // demand and the upgrade candidates see it.
        if state.bwe {
            let layers = videos.iter().flat_map(|v| {
                // Up to the layer it forwards now, which a lowered
                // ceiling may leave above the highest permitted.
                let top = pick(&v.flowing, state.ceiling)
                    .unwrap_or(v.current)
                    .max(v.current);
                (0..=top).filter_map(|rank| {
                    let meter = v.flowing[rank]?;
                    meter.rate(now).is_none().then_some(meter.first)
                })
            });
            state.wake = layers
                .chain(unmeasured)
                .min()
                .map(|first| first + RATE_WINDOW);
        }

        let Some((estimate, can_probe)) = state.estimate.filter(|_| state.bwe) else {
            state.hold_since = None;
            return demand;
        };
        let budget = (estimate as f64 * BUDGET_FRACTION - fixed as f64).max(0.0);
        let spent: u64 = videos.iter().map(|v| v.cost).sum();

        if spent as f64 > budget {
            // Over budget: one track, one layer down, the largest saving first.
            state.hold_since = None;
            let lowered = videos
                .iter()
                .filter(|v| !v.pending)
                .filter_map(|v| {
                    let lower = (0..v.current).rev().find(|&r| v.flowing[r].is_some())?;
                    let saving = v
                        .cost
                        .saturating_sub(cost_of(v.flowing[lower]).unwrap_or(0));
                    Some((saving, v, lower))
                })
                // The largest saving; the first of the stable order on a tie.
                .reduce(|best, next| if next.0 > best.0 { next } else { best });
            if let Some((_, video, lower)) = lowered {
                set_cap(
                    receiver,
                    state,
                    &video.track,
                    lower,
                    Some(CapReason::new(estimate, budget, spent, &video.flowing, now)),
                );
            }
            return demand;
        }

        // Within budget: look for an upgrade, never while a switch is pending
        // or before the transport can probe.
        if !can_probe || videos.iter().any(|v| v.pending) {
            state.hold_since = None;
            return demand;
        }
        let cheapest = videos
            .iter()
            .filter_map(|v| {
                let higher = (v.current + 1..=state.ceiling.min(HIGHEST))
                    .find(|&r| v.flowing[r].is_some())?;
                let meter = v.flowing[higher]?;
                let cost = meter.rate(now)?;
                Some((cost.saturating_sub(v.cost), v, higher))
            })
            // The smallest step; the first of the stable order on a tie.
            .reduce(|best, next| if next.0 < best.0 { next } else { best });
        let Some((delta, video, higher)) = cheapest else {
            state.hold_since = None;
            return demand;
        };
        if budget < UPGRADE_HEADROOM * (spent + delta) as f64 {
            state.hold_since = None;
            return demand;
        }
        let since = *state.hold_since.get_or_insert(now);
        let due = since + UPGRADE_HOLD;
        if now >= due {
            state.hold_since = None;
            set_cap(
                receiver,
                state,
                &video.track,
                higher,
                Some(CapReason::new(estimate, budget, spent, &video.flowing, now)),
            );
        } else {
            state.wake = Some(state.wake.map_or(due, |at| at.min(due)));
        }
        demand
    }

    /// Decide whether `receiver` gets a packet of `track` on layer `rid`.
    /// `keyframe` is [`is_keyframe`] of the packet's frame.
    pub fn route(
        &mut self,
        now: Instant,
        receiver: &ParticipantId,
        track: &Track,
        rid: Option<&Rid>,
        keyframe: Option<bool>,
    ) -> Route {
        if !self
            .receivers
            .get(receiver)
            .is_some_and(|r| r.tracks.contains(track))
        {
            self.receivers
                .entry(receiver.clone())
                .or_default()
                .tracks
                .insert(track.clone());
            self.restart(now, receiver);
        }
        let Some(&rid) = rid else {
            return Route {
                forward: true,
                request_keyframe: None,
            };
        };
        let wanted = self
            .receivers
            .get(receiver)
            .zip(self.meters.get(track))
            .and_then(|(state, meters)| pick(&meters.flowing(now), state.cap(track)))
            .map_or(rid, |rank| Rid::from(LAYERS[rank]));

        let key = (receiver.clone(), track.clone());
        let current = self.forwarding.get(&key).copied();
        if current == Some(wanted) {
            return Route {
                forward: rid == wanted,
                request_keyframe: None,
            };
        }
        // A switch: take the new layer at its next keyframe. A codec that
        // does not mark keyframes switches at once and asks for one.
        if rid == wanted && keyframe != Some(false) {
            self.forwarding.insert(key, wanted);
            // A fallback below the cap (its layer stopped) lowers the cap to
            // what flows: the layer that returns earns its upgrade again,
            // through the measurement, the hold and `can_probe`.
            if let (Some(state), Some(rank)) = (
                self.receivers.get_mut(receiver),
                LAYERS.iter().position(|name| Rid::from(*name) == wanted),
            ) {
                if state.bwe && rank < state.cap(track) {
                    set_cap(receiver, state, track, rank, None);
                }
            }
            let request_keyframe = if keyframe.is_none() {
                self.throttle(now, track, wanted)
            } else {
                None
            };
            // The switch is done: its new cost counts, and the budget runs.
            self.restart(now, receiver);
            return Route {
                forward: true,
                request_keyframe,
            };
        }
        Route {
            forward: current == Some(rid),
            request_keyframe: self.throttle(now, track, wanted),
        }
    }

    /// The RTP time `receiver` gets for a packet of `track` that
    /// [`Self::route`] forwarded, arriving at `at`.
    ///
    /// Each layer is its own RTP stream with its own random timestamp base,
    /// but a receiver gets every layer on one stream. Forwarded as they are,
    /// a layer switch would jump the receiver's clock by up to 2^32 ticks,
    /// and a receiver schedules decoding by that clock: it holds or drops
    /// frames until its timing recovers. So the receiver's clock follows
    /// one layer at a time, and a switch carries it on from the last frame
    /// by the time that passed since.
    pub fn rtp_time(
        &mut self,
        at: Instant,
        receiver: &ParticipantId,
        track: &Track,
        rid: Option<&Rid>,
        time: MediaTime,
    ) -> MediaTime {
        let Some(&rid) = rid else {
            return time;
        };
        let input = time.numer() as i64;
        let key = (receiver.clone(), track.clone());
        let timeline = self.timelines.entry(key).or_insert(Timeline {
            rid,
            offset: 0,
            last: time.numer(),
            last_at: at,
        });
        if timeline.rid != rid {
            let elapsed = at.saturating_duration_since(timeline.last_at);
            let ticks = (elapsed.as_secs_f64() * f64::from(time.denom())).round() as i64;
            timeline.offset = timeline.last as i64 + ticks.max(1) - input;
            timeline.rid = rid;
        }
        let output = (input + timeline.offset).max(0) as u64;
        if output >= timeline.last {
            timeline.last = output;
            timeline.last_at = at;
        }
        MediaTime::new(output, time.frequency())
    }

    /// The layer `receiver` gets of `track`, for its keyframe requests.
    pub fn layer_of(&self, receiver: &ParticipantId, track: &Track) -> Option<Rid> {
        self.forwarding
            .get(&(receiver.clone(), track.clone()))
            .copied()
    }

    /// A video packet of `track` on layer `rid` came after a gap in its
    /// sequence (str0m's `contiguous == false`, e.g. after an outage): the
    /// receivers' decoders may have lost a reference frame and need a
    /// keyframe. Whether to ask the sender for one on that layer now.
    pub fn gap(&mut self, now: Instant, track: &Track, rid: Option<&Rid>) -> bool {
        self.may_request(now, track, rid.copied())
    }

    /// Drop all state of a participant that left, as sender and as receiver.
    pub fn forget(&mut self, now: Instant, participant: &ParticipantId) {
        self.meters.retain(|(sender, _), _| sender != participant);
        self.receivers.remove(participant);
        self.demands.remove(participant);
        let affected: Vec<ParticipantId> = self
            .receivers
            .iter_mut()
            .filter_map(|(id, state)| {
                let before = state.tracks.len();
                state.tracks.retain(|(sender, _)| sender != participant);
                state.caps.retain(|(sender, _), _| sender != participant);
                (state.tracks.len() != before).then(|| id.clone())
            })
            .collect();
        for id in affected {
            self.restart(now, &id);
        }
        self.forwarding
            .retain(|(receiver, (sender, _)), _| receiver != participant && sender != participant);
        self.last_request
            .retain(|((sender, _), _), _| sender != participant);
        self.timelines
            .retain(|(receiver, (sender, _)), _| receiver != participant && sender != participant);
    }

    fn throttle(&mut self, now: Instant, track: &Track, rid: Rid) -> Option<Rid> {
        self.may_request(now, track, Some(rid)).then_some(rid)
    }

    /// Whether a keyframe request for `rid` of `track` is due, recording it
    /// when it is: at most one per [`KEYFRAME_REQUEST_INTERVAL`].
    fn may_request(&mut self, now: Instant, track: &Track, rid: Option<Rid>) -> bool {
        let key = (track.clone(), rid);
        if let Some(last) = self.last_request.get(&key) {
            if now.duration_since(*last) < KEYFRAME_REQUEST_INTERVAL {
                return false;
            }
        }
        self.last_request.insert(key, now);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SENDER: ParticipantId = ParticipantId(1);
    const RECEIVER: ParticipantId = ParticipantId(2);

    fn track() -> Track {
        (SENDER, Mid::from("1"))
    }

    /// A sender with `layers` flowing at `now`.
    fn selector(now: Instant, layers: &[&str]) -> LayerSelector {
        let mut s = LayerSelector::default();
        for layer in layers {
            s.observe(
                now,
                &track(),
                Some(&Rid::from(*layer)),
                1000,
                1,
                false,
                false,
            );
        }
        s
    }

    fn route(
        s: &mut LayerSelector,
        now: Instant,
        rid: &str,
        keyframe: bool,
        pref: Option<&str>,
    ) -> Route {
        s.observe(now, &track(), Some(&Rid::from(rid)), 1000, 1, false, false);
        // The receiver is not budgeted: its ceiling is its preference.
        s.set_ceiling(now, &RECEIVER, pref.unwrap_or("auto"));
        s.route(
            now,
            &RECEIVER,
            &track(),
            Some(&Rid::from(rid)),
            Some(keyframe),
        )
    }

    const FORWARD: Route = Route {
        forward: true,
        request_keyframe: None,
    };
    const DROP: Route = Route {
        forward: false,
        request_keyframe: None,
    };

    #[test]
    fn a_sender_without_simulcast_always_passes() {
        let mut s = LayerSelector::default();
        let now = Instant::now();
        assert_eq!(
            s.route(now, &RECEIVER, &track(), None, Some(false)),
            FORWARD
        );
    }

    #[test]
    fn the_preferred_layer_starts_at_a_keyframe() {
        let now = Instant::now();
        let mut s = selector(now, &["high", "medium", "low"]);
        let first = route(&mut s, now, "low", false, Some("low"));
        assert_eq!(
            first,
            Route {
                forward: false,
                request_keyframe: Some(Rid::from("low"))
            }
        );
        assert_eq!(route(&mut s, now, "high", true, Some("low")), DROP);
        assert_eq!(route(&mut s, now, "low", true, Some("low")), FORWARD);
        assert_eq!(route(&mut s, now, "low", false, Some("low")), FORWARD);
        assert_eq!(route(&mut s, now, "medium", true, Some("low")), DROP);
        assert_eq!(s.layer_of(&RECEIVER, &track()), Some(Rid::from("low")));
    }

    #[test]
    fn a_switch_keeps_the_old_layer_until_the_new_one_has_a_keyframe() {
        let now = Instant::now();
        let mut s = selector(now, &["high", "low"]);
        assert_eq!(route(&mut s, now, "high", true, None), FORWARD);
        // The receiver now asks for low.
        assert_eq!(
            route(&mut s, now, "low", false, Some("low")),
            Route {
                forward: false,
                request_keyframe: Some(Rid::from("low"))
            }
        );
        assert_eq!(route(&mut s, now, "high", false, Some("low")), FORWARD);
        assert_eq!(route(&mut s, now, "low", true, Some("low")), FORWARD);
        assert_eq!(route(&mut s, now, "high", false, Some("low")), DROP);
    }

    #[test]
    fn a_missing_layer_falls_back_below_then_above() {
        let now = Instant::now();
        // The encoder sends two layers only: medium is preferred below high.
        let mut s = selector(now, &["medium", "low"]);
        assert_eq!(route(&mut s, now, "medium", true, Some("high")), FORWARD);
        // Only high flows: low takes the nearest layer above.
        let mut s = selector(now, &["high"]);
        assert_eq!(route(&mut s, now, "high", true, Some("low")), FORWARD);
    }

    #[test]
    fn a_layer_that_stops_is_replaced() {
        let start = Instant::now();
        let mut s = selector(start, &["high", "low"]);
        assert_eq!(route(&mut s, start, "high", true, None), FORWARD);
        // High stops (bandwidth); a second later only low flows.
        let later = start + LAYER_TIMEOUT + Duration::from_millis(1);
        assert_eq!(route(&mut s, later, "low", true, None), FORWARD);
    }

    #[test]
    fn keyframe_requests_are_throttled_per_layer() {
        let now = Instant::now();
        let mut s = selector(now, &["high", "low"]);
        assert!(route(&mut s, now, "low", false, Some("low"))
            .request_keyframe
            .is_some());
        assert!(route(&mut s, now, "low", false, Some("low"))
            .request_keyframe
            .is_none());
        let later = now + KEYFRAME_REQUEST_INTERVAL;
        assert!(route(&mut s, later, "low", false, Some("low"))
            .request_keyframe
            .is_some());
    }

    #[test]
    fn a_codec_without_keyframe_marks_switches_at_once_and_asks_for_one() {
        let now = Instant::now();
        let mut s = selector(now, &["high"]);
        s.observe(
            now,
            &track(),
            Some(&Rid::from("high")),
            1000,
            1,
            false,
            false,
        );
        let routed = s.route(now, &RECEIVER, &track(), Some(&Rid::from("high")), None);
        assert_eq!(
            routed,
            Route {
                forward: true,
                request_keyframe: Some(Rid::from("high"))
            }
        );
    }

    fn at_90khz(ticks: u64) -> MediaTime {
        MediaTime::from_90khz(ticks)
    }

    /// Forward one frame of `rid` with the sender's RTP time `ticks` at
    /// `at`; the RTP time the receiver gets.
    fn forwarded(s: &mut LayerSelector, at: Instant, rid: &str, ticks: u64) -> u64 {
        s.rtp_time(
            at,
            &RECEIVER,
            &track(),
            Some(&Rid::from(rid)),
            at_90khz(ticks),
        )
        .numer()
    }

    // Each simulcast layer is its own RTP stream with its own random
    // timestamp base: the same capture instant on two layers, from a
    // libwebrtc sender in the e2e suite.
    const HIGH_BASE: u64 = 344_404_849;
    const MEDIUM_BASE: u64 = 3_474_388_391;

    #[test]
    fn a_layer_switch_continues_the_receivers_rtp_clock() {
        let start = Instant::now();
        let mut s = LayerSelector::default();
        let first = forwarded(&mut s, start, "high", HIGH_BASE);
        let high = forwarded(
            &mut s,
            start + Duration::from_millis(66),
            "high",
            HIGH_BASE + 6_000,
        );
        assert_eq!(high - first, 6_000);

        // 100 ms later the receiver switches to medium, whose clock reads
        // three billion ticks apart: the receiver's clock moves on by 100 ms.
        let switched = start + Duration::from_millis(166);
        let medium = forwarded(&mut s, switched, "medium", MEDIUM_BASE + 15_000);
        assert_eq!(medium, high + 9_000);
        // Within the layer, its own spacing.
        let next = forwarded(
            &mut s,
            switched + Duration::from_millis(66),
            "medium",
            MEDIUM_BASE + 21_000,
        );
        assert_eq!(next, medium + 6_000);

        // And back to high after 8 s without any frame (a network switch).
        let back = switched + Duration::from_millis(66) + Duration::from_secs(8);
        let high_again = forwarded(&mut s, back, "high", HIGH_BASE + 747_000);
        assert_eq!(high_again, next + 720_000);
    }

    #[test]
    fn a_layer_that_resumes_keeps_the_senders_spacing() {
        let start = Instant::now();
        let mut s = LayerSelector::default();
        let before = forwarded(&mut s, start, "high", HIGH_BASE);
        // The sender kept capturing through an 8.7 s outage.
        let after = forwarded(
            &mut s,
            start + Duration::from_millis(8_700),
            "high",
            HIGH_BASE + 783_000,
        );
        assert_eq!(after - before, 783_000);
    }

    #[test]
    fn a_sender_without_simulcast_keeps_its_rtp_time() {
        let mut s = LayerSelector::default();
        let time = at_90khz(MEDIUM_BASE);
        assert_eq!(
            s.rtp_time(Instant::now(), &RECEIVER, &track(), None, time),
            time
        );
    }

    #[test]
    fn a_gap_asks_for_a_keyframe_on_its_layer_throttled() {
        let now = Instant::now();
        let high = Rid::from("high");
        let mut s = selector(now, &["high", "low"]);
        assert!(s.gap(now, &track(), Some(&high)));
        // The next packets after the gap ask no more within the interval,
        let soon = now + Duration::from_millis(20);
        assert!(!s.gap(soon, &track(), Some(&high)));
        // and neither does a switch that waits for a keyframe on that layer.
        assert_eq!(route(&mut s, soon, "high", false, Some("high")), DROP);
        // Another layer has its own throttle.
        assert!(s.gap(soon, &track(), Some(&Rid::from("low"))));
        assert!(s.gap(now + KEYFRAME_REQUEST_INTERVAL, &track(), Some(&high)));
        // A sender without simulcast is asked too.
        assert!(s.gap(now, &(SENDER, Mid::from("2")), None));
    }

    #[test]
    fn forget_drops_a_participant_as_sender_and_receiver() {
        let now = Instant::now();
        let mut s = selector(now, &["high"]);
        assert_eq!(route(&mut s, now, "high", true, None), FORWARD);
        s.forget(now, &SENDER);
        assert_eq!(s.layer_of(&RECEIVER, &track()), None);
        assert!(s.meters.is_empty() && s.last_request.is_empty());
        assert!(s.receivers[&RECEIVER].tracks.is_empty());
        forwarded(&mut s, Instant::now(), "high", HIGH_BASE);
        s.forget(now, &RECEIVER);
        assert!(s.receivers.is_empty());
        assert!(s.timelines.is_empty());
    }

    // ---- the bandwidth budget ----

    /// Wire bitrates of the low, medium and high layer.
    const RATES: [u64; 3] = [200_000, 600_000, 1_500_000];
    const BIG_HIGH: [u64; 3] = [200_000, 600_000, 2_500_000];
    const MS10: Duration = Duration::from_millis(10);

    /// A budgeted receiver of some video tracks, every layer sending at a
    /// steady rate (a packet every 10 ms), and a clock only the test moves.
    struct Sim {
        s: LayerSelector,
        now: Instant,
        tracks: Vec<(Track, [u64; 3])>,
        /// Audio tracks, each a packet of 100 bytes every 10 ms.
        audio: Vec<Track>,
    }

    impl Sim {
        fn new(rates: &[[u64; 3]]) -> Sim {
            let mut s = LayerSelector::default();
            s.set_bwe(&RECEIVER);
            let tracks = rates
                .iter()
                .enumerate()
                .map(|(i, r)| ((ParticipantId(10 + i as u64), Mid::from("1")), *r))
                .collect();
            let mut sim = Sim {
                s,
                now: Instant::now(),
                tracks,
                audio: Vec::new(),
            };
            // Long enough for every layer's rate to be measured.
            sim.advance(1_500);
            sim.commit();
            sim
        }

        /// Packets of every layer of every track for `ms`, in steps of 10 ms.
        fn advance(&mut self, ms: u64) {
            self.advance_layers(ms, &[0, 1, 2]);
        }

        /// The same, with only the layers of these ranks sending.
        fn advance_layers(&mut self, ms: u64, ranks: &[usize]) {
            for _ in 0..ms / 10 {
                self.now += MS10;
                for track in &self.audio {
                    self.s.observe(self.now, track, None, 100, 1, false, true);
                }
                for (track, rates) in &self.tracks {
                    for (rank, rate) in rates.iter().enumerate() {
                        if !ranks.contains(&rank) {
                            continue;
                        }
                        let payload = (rate / 800 - PACKET_OVERHEAD_BYTES) as usize;
                        let rid = Rid::from(LAYERS[rank]);
                        self.s
                            .observe(self.now, track, Some(&rid), payload, 1, false, false);
                    }
                }
            }
        }

        /// Every layer sends a keyframe: switches that wait for one commit.
        fn commit(&mut self) {
            for (track, _) in self.tracks.clone() {
                for name in LAYERS {
                    let rid = Rid::from(name);
                    self.s
                        .route(self.now, &RECEIVER, &track, Some(&rid), Some(true));
                }
            }
        }

        fn estimate(&mut self, bps: u64, can_probe: bool) {
            self.s.set_estimate(self.now, &RECEIVER, bps, can_probe);
        }

        /// The cap of each track.
        fn caps(&self) -> Vec<usize> {
            self.tracks
                .iter()
                .map(|(t, _)| self.s.receivers[&RECEIVER].cap(t))
                .collect()
        }

        /// Each track's forwarded layer's rank.
        fn forwarded(&self) -> Vec<usize> {
            self.tracks
                .iter()
                .map(|(t, _)| {
                    let rid = self.s.layer_of(&RECEIVER, t).unwrap();
                    LAYERS.iter().position(|n| Rid::from(*n) == rid).unwrap()
                })
                .collect()
        }

        /// Put every track on `rank`, forwarded and allowed.
        fn place(&mut self, rank: usize) {
            let state = self.s.receivers.get_mut(&RECEIVER).unwrap();
            for (track, _) in &self.tracks {
                state.caps.insert(track.clone(), rank);
                self.s
                    .forwarding
                    .insert((RECEIVER, track.clone()), Rid::from(LAYERS[rank]));
            }
        }
    }

    #[test]
    fn a_layers_rate_is_measured_over_a_window_with_the_packet_overhead() {
        let sim = Sim::new(&[RATES]);
        let track = sim.tracks[0].0.clone();
        let meters = &sim.s.meters[&track];
        let rate = |rank: usize| meters.flowing(sim.now)[rank].unwrap().rate(sim.now);
        // The packets' sizes are the layers' wire rates less the overhead.
        assert_eq!([rate(0), rate(1), rate(2)], RATES.map(Some));
        // A layer measured for less than a window has no rate yet.
        let fresh = Instant::now();
        let mut s = LayerSelector::default();
        s.observe(
            fresh,
            &track,
            Some(&Rid::from("low")),
            1000,
            1,
            false,
            false,
        );
        s.observe(
            fresh + RATE_WINDOW / 2,
            &track,
            Some(&Rid::from("low")),
            1000,
            1,
            false,
            false,
        );
        let meter = &s.meters[&track].layers[&Some(Rid::from("low"))];
        assert_eq!(meter.rate(fresh + RATE_WINDOW / 2), None);
        // One that stopped for LAYER_TIMEOUT is gone.
        assert_eq!(meter.rate(fresh + RATE_WINDOW / 2 + LAYER_TIMEOUT), None);
    }

    #[test]
    fn a_budgeted_track_starts_on_the_lowest_layer_and_stays_without_an_estimate() {
        let mut sim = Sim::new(&[RATES]);
        assert_eq!(sim.forwarded(), [0]);
        // No event, no allocation: nothing is pending, whatever the time.
        sim.advance(10_000);
        sim.s.run_due(sim.now);
        assert_eq!(sim.s.next_wake(), None);
        assert_eq!(sim.caps(), [0]);
        // The first estimate that cannot probe allows no upgrade either.
        sim.estimate(10_000_000, false);
        sim.advance(10_000);
        sim.s.run_due(sim.now);
        assert_eq!((sim.s.next_wake(), sim.caps()), (None, vec![0]));
    }

    #[test]
    fn a_falling_estimate_lowers_one_track_one_layer_per_run_the_largest_saving_first() {
        let mut sim = Sim::new(&[RATES, BIG_HIGH, RATES]);
        sim.place(2);
        // 1.5 + 2.5 + 1.5 Mbit/s against 0.85 * 4 = 3.4: over.
        sim.estimate(4_000_000, true);
        // The track that saves the most steps down, one layer.
        assert_eq!(sim.caps(), [2, 1, 2]);
        // It has not switched yet: its old cost still counts, so the next
        // run lowers another track. The pending one is left alone, and the
        // tie between the others goes to the lower track id.
        sim.estimate(4_000_000, true);
        assert_eq!(sim.caps(), [1, 1, 2]);
        sim.estimate(4_000_000, true);
        assert_eq!(sim.caps(), [1, 1, 1]);
    }

    #[test]
    fn the_old_layers_cost_counts_until_the_switch_commits() {
        let mut sim = Sim::new(&[RATES, BIG_HIGH]);
        sim.place(2);
        // 1.5 + 2.5 = 4.0 against 0.85 * 2.8 = 2.38: the big one steps down.
        sim.estimate(2_800_000, true);
        assert_eq!(sim.caps(), [2, 1]);
        // With its new cost (1.5 + 0.6 = 2.1) the budget would be met, but
        // its high layer still flows: the other track steps down too.
        sim.estimate(2_800_000, true);
        assert_eq!(sim.caps(), [1, 1]);
    }

    #[test]
    fn low_is_the_floor() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(0, true);
        sim.estimate(0, true);
        assert_eq!(sim.caps(), [0]);
        assert_eq!(sim.forwarded(), [0]);
    }

    #[test]
    fn audio_is_always_forwarded_and_reserved_before_video() {
        let audio = (ParticipantId(30), Mid::from("2"));
        let mut sim = Sim::new(&[RATES]);
        sim.audio.push(audio.clone());
        sim.advance(1_500);
        sim.place(1);
        assert_eq!(sim.s.route(sim.now, &RECEIVER, &audio, None, None), FORWARD);
        // 0.85 * 0.72 = 0.612 Mbit/s would hold medium (0.6) without audio;
        // audio's rate (105.6 kbit/s, above the 64 kbit/s reserve) leaves 0.506.
        sim.estimate(720_000, true);
        assert_eq!(sim.caps(), [0]);
        // Zero budget: video at the floor, audio still forwarded.
        sim.estimate(0, true);
        assert_eq!(sim.caps(), [0]);
        assert_eq!(sim.s.route(sim.now, &RECEIVER, &audio, None, None), FORWARD);
    }

    #[test]
    fn a_rising_estimate_upgrades_one_layer_once_after_the_hold() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(3_000_000, true);
        let start = sim.now;
        // One event, and the loop is told when to wake.
        assert_eq!(sim.s.next_wake(), Some(start + UPGRADE_HOLD));
        sim.advance(3_990);
        // The same estimate again (an event) does not restart the hold.
        sim.estimate(3_000_000, true);
        sim.s
            .run_due(start + UPGRADE_HOLD - Duration::from_millis(1));
        assert_eq!(sim.caps(), [0]);
        // At the deadline, without any further event: one step up.
        sim.s.run_due(start + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [1]);
        sim.s.run_due(start + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [1]);
        // The new layer is pending: no second step stacks on it.
        assert_eq!(sim.s.next_wake(), None);
    }

    #[test]
    fn each_further_upgrade_needs_a_new_hold_after_the_switch_commits() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(3_000_000, true);
        sim.advance(4_000);
        sim.s.run_due(sim.now);
        assert_eq!(sim.caps(), [1]);
        sim.advance(100);
        sim.commit();
        assert_eq!(sim.forwarded(), [1]);
        let committed = sim.now;
        // The commit started a new hold; it is not the old one.
        assert_eq!(sim.s.next_wake(), Some(committed + UPGRADE_HOLD));
        sim.advance(3_990);
        sim.s
            .run_due(committed + UPGRADE_HOLD - Duration::from_millis(1));
        assert_eq!(sim.caps(), [1]);
        sim.s.run_due(committed + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [2]);
    }

    #[test]
    fn a_dip_restarts_the_hold() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(3_000_000, true);
        sim.advance(2_000);
        // The budget falls below the headroom: the hold is dropped.
        sim.estimate(500_000, true);
        assert_eq!(sim.s.next_wake(), None);
        sim.advance(1_000);
        sim.estimate(3_000_000, true);
        let again = sim.now;
        assert_eq!(sim.s.next_wake(), Some(again + UPGRADE_HOLD));
        // The first hold's deadline passes with no upgrade.
        sim.advance(1_000);
        sim.s.run_due(sim.now);
        assert_eq!(sim.caps(), [0]);
        sim.advance(2_990);
        sim.s.run_due(again + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [1]);
    }

    #[test]
    fn a_ceiling_or_track_change_restarts_the_hold() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(3_000_000, true);
        sim.advance(2_000);
        // A ceiling change (low, then back to high) starts the hold over.
        sim.s.set_ceiling(sim.now, &RECEIVER, "low");
        assert_eq!(sim.s.next_wake(), None);
        sim.s.set_ceiling(sim.now, &RECEIVER, "auto");
        assert_eq!(sim.s.next_wake(), Some(sim.now + UPGRADE_HOLD));
        // So does another track appearing.
        sim.advance(1_000);
        let other = (ParticipantId(40), Mid::from("1"));
        sim.s.observe(
            sim.now,
            &other,
            Some(&Rid::from("low")),
            100,
            1,
            false,
            false,
        );
        sim.s.route(
            sim.now,
            &RECEIVER,
            &other,
            Some(&Rid::from("low")),
            Some(true),
        );
        // (The new track's rate is measured a window on: that wake is sooner.)
        assert_eq!(sim.s.receivers[&RECEIVER].hold_since, Some(sim.now));
    }

    #[test]
    fn an_estimate_that_cannot_probe_blocks_upgrades() {
        let mut sim = Sim::new(&[RATES]);
        sim.estimate(3_000_000, true);
        assert!(sim.s.next_wake().is_some());
        sim.advance(2_000);
        // The estimator lost its ability to probe: the hold is dropped.
        sim.estimate(3_000_000, false);
        assert_eq!(sim.s.next_wake(), None);
        sim.advance(10_000);
        sim.s.run_due(sim.now);
        assert_eq!(sim.caps(), [0]);
        // It can again.
        sim.estimate(3_000_000, true);
        assert_eq!(sim.s.next_wake(), Some(sim.now + UPGRADE_HOLD));
    }

    #[test]
    fn an_upgrade_waits_for_the_target_layers_rate_to_be_measured() {
        let mut s = LayerSelector::default();
        s.set_bwe(&RECEIVER);
        let track = track_n(0);
        let (low, medium) = (Rid::from("low"), Rid::from("medium"));
        let start = Instant::now();
        // Low flows from the start; medium began at packet 130.
        let feed = |s: &mut LayerSelector, from: u32, to: u32| {
            for i in from..to {
                let at = start + MS10 * i;
                s.observe(at, &track, Some(&low), 218, 1, false, false);
                if i >= 130 {
                    s.observe(at, &track, Some(&medium), 718, 1, false, false);
                }
            }
        };
        feed(&mut s, 0, 150);
        let now = start + MS10 * 149;
        s.route(now, &RECEIVER, &track, Some(&low), Some(true));
        s.set_estimate(now, &RECEIVER, 3_000_000, true);
        // The upgrade cannot be judged yet: the allocator wakes when the
        // medium layer has been measured for a window, not before.
        let measured = start + MS10 * 130 + RATE_WINDOW;
        assert_eq!(s.receivers[&RECEIVER].hold_since, None);
        assert_eq!(s.next_wake(), Some(measured));
        feed(&mut s, 150, 231);
        s.run_due(measured);
        assert_eq!(s.next_wake(), Some(measured + UPGRADE_HOLD));
    }

    #[test]
    fn a_manual_low_is_never_exceeded_and_a_missing_low_falls_to_the_lowest_flowing() {
        let mut sim = Sim::new(&[RATES]);
        sim.s.set_ceiling(sim.now, &RECEIVER, "low");
        sim.estimate(100_000_000, true);
        sim.advance(10_000);
        sim.s.run_due(sim.now);
        assert_eq!((sim.caps(), sim.s.next_wake()), (vec![0], None));
        let track = sim.tracks[0].0.clone();
        let high = Rid::from("high");
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&high), Some(true)),
            DROP
        );

        // A sender with no low layer: the lowest flowing one, never black.
        let mut s = LayerSelector::default();
        let t = track_n(1);
        s.observe(
            sim.now,
            &t,
            Some(&Rid::from("medium")),
            500,
            1,
            false,
            false,
        );
        s.observe(sim.now, &t, Some(&Rid::from("high")), 500, 1, false, false);
        s.set_ceiling(sim.now, &RECEIVER, "low");
        let medium = Rid::from("medium");
        assert_eq!(
            s.route(sim.now, &RECEIVER, &t, Some(&medium), Some(true)),
            FORWARD
        );
        assert_eq!(
            s.route(sim.now, &RECEIVER, &t, Some(&high), Some(true)),
            DROP
        );
    }

    fn track_n(n: u64) -> Track {
        (ParticipantId(50 + n), Mid::from("1"))
    }

    #[test]
    fn a_lowered_ceiling_takes_effect_on_the_next_keyframe() {
        let mut sim = Sim::new(&[RATES]);
        sim.place(2);
        let track = sim.tracks[0].0.clone();
        sim.s.set_ceiling(sim.now, &RECEIVER, "medium");
        let (medium, high) = (Rid::from("medium"), Rid::from("high"));
        // High flows on until medium has a keyframe, then medium takes over.
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&medium), Some(false)),
            Route {
                forward: false,
                request_keyframe: Some(medium)
            }
        );
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&high), Some(false)),
            FORWARD
        );
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&medium), Some(true)),
            FORWARD
        );
        assert_eq!(sim.forwarded(), [1]);
    }

    #[test]
    fn the_probing_demand_is_the_highest_permitted_layers_with_headroom() {
        let mut sim = Sim::new(&[RATES, BIG_HIGH]);
        sim.estimate(10_000_000, true);
        // (1.5 + 2.5) Mbit/s, with the upgrade headroom, over the budget share.
        let video = (4_000_000.0 * UPGRADE_HEADROOM / BUDGET_FRACTION) as u64;
        let demand = sim.s.take_demands()[&RECEIVER];
        assert!(demand.abs_diff(video) < 120_000, "{demand} vs {video}");
        // Unchanged: nothing to report.
        sim.estimate(10_000_000, true);
        assert!(sim.s.take_demands().is_empty());
        // A manual medium lowers what is worth probing for.
        sim.s.set_ceiling(sim.now, &RECEIVER, "medium");
        let lower = sim.s.take_demands()[&RECEIVER];
        assert!(lower < demand / 2);
    }

    #[test]
    fn an_estimate_at_the_demand_reaches_the_last_upgrade_with_audio_flowing() {
        /// Medium forwarded, audio flowing, the estimate unknown.
        fn with_audio() -> Sim {
            let mut sim = Sim::new(&[RATES]);
            let audio = (ParticipantId(30), Mid::from("2"));
            sim.audio.push(audio.clone());
            sim.advance(1_500);
            sim.s.route(sim.now, &RECEIVER, &audio, None, None);
            sim.place(1);
            sim
        }
        let mut sim = with_audio();
        sim.estimate(10_000_000, true);
        // Audio's measured rate: a 100 byte packet every 10 ms, with overhead.
        let audio = (100 + PACKET_OVERHEAD_BYTES) * 8 * 100;
        let demand = sim.s.take_demands()[&RECEIVER];
        // `0.85 * E - audio >= 1.20 * high`, and not more than that.
        let needed = (UPGRADE_HEADROOM * RATES[2] as f64 + audio as f64) / BUDGET_FRACTION;
        assert!(demand as f64 >= needed && (demand as f64) < needed + 1.0);
        // The estimator settles at the demand and stays silent: medium to
        // high happens, after the hold, on that estimate alone.
        sim.estimate(demand, true);
        let start = sim.now;
        sim.advance(3_990);
        sim.s
            .run_due(start + UPGRADE_HOLD - Duration::from_millis(1));
        assert_eq!(sim.caps(), [1]);
        sim.s.run_due(start + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [2]);
        // An estimate 100 bit/s short of it does not.
        let mut sim = with_audio();
        sim.estimate(demand - 100, true);
        sim.advance(4_000);
        sim.s.run_due(sim.now);
        assert_eq!(sim.caps(), [1]);
    }

    #[test]
    fn a_frame_costs_the_overhead_of_each_of_its_packets() {
        let mut s = LayerSelector::default();
        let track = track_n(2);
        let high = Rid::from("high");
        let start = Instant::now();
        // A frame of 3000 bytes in 3 packets every 10 ms.
        for i in 0..150 {
            s.observe(
                start + MS10 * i,
                &track,
                Some(&high),
                3_000,
                3,
                false,
                false,
            );
        }
        let now = start + MS10 * 149;
        let rate = s.meters[&track].layers[&Some(high)].rate(now);
        assert_eq!(rate, Some((3_000 + 3 * PACKET_OVERHEAD_BYTES) * 8 * 100));
    }

    #[test]
    fn a_keyframe_adds_no_bytes_to_the_layers_rate() {
        let mut s = LayerSelector::default();
        let track = track_n(2);
        let high = Rid::from("high");
        let start = Instant::now();
        for i in 0..150 {
            // Every 30th frame is a keyframe 20 times the size.
            let key = i % 30 == 0;
            let size = if key { 60_000 } else { 3_000 };
            s.observe(start + MS10 * i, &track, Some(&high), size, 3, key, false);
        }
        let now = start + MS10 * 149;
        let rate = s.meters[&track].layers[&Some(high)].rate(now);
        let rate = rate.expect("a rate");
        let per_frame = 3_000 + 3 * PACKET_OVERHEAD_BYTES;
        assert!(rate <= per_frame * 8 * 100, "{rate}");
        assert!(rate >= per_frame * 8 * 90, "{rate}");
    }

    #[test]
    fn a_layer_that_returns_earns_its_upgrade_again() {
        let mut sim = Sim::new(&[RATES]);
        sim.place(2);
        let track = sim.tracks[0].0.clone();
        let (medium, high) = (Rid::from("medium"), Rid::from("high"));
        // The estimator cannot probe: no upgrade, however long.
        sim.estimate(10_000_000, false);
        // High stops; medium takes over on its keyframe, and the cap with it.
        sim.advance_layers(LAYER_TIMEOUT.as_millis() as u64 + 500, &[0, 1]);
        sim.s
            .route(sim.now, &RECEIVER, &track, Some(&medium), Some(true));
        assert_eq!((sim.caps(), sim.forwarded()), (vec![1], vec![1]));
        // High returns: its keyframe is not taken.
        sim.advance(10_000);
        sim.s.run_due(sim.now);
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&high), Some(true)),
            DROP
        );
        assert_eq!((sim.caps(), sim.forwarded()), (vec![1], vec![1]));
        // Once the transport can probe, it takes the hold, and only then.
        sim.estimate(10_000_000, true);
        let start = sim.now;
        sim.advance(3_990);
        sim.s
            .run_due(start + UPGRADE_HOLD - Duration::from_millis(1));
        assert_eq!(sim.caps(), [1]);
        sim.s.run_due(start + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [2]);
        assert_eq!(
            sim.s
                .route(sim.now, &RECEIVER, &track, Some(&high), Some(true)),
            FORWARD
        );
    }

    #[test]
    fn a_video_that_resumes_is_costed_again_a_window_after_its_first_packet() {
        let mut sim = Sim::new(&[RATES]);
        sim.place(2);
        // A video with no layers, 1 Mbit/s on the wire, beside the simulcast one.
        let plain = track_n(60);
        let payload = (1_000_000 / 800 - PACKET_OVERHEAD_BYTES) as usize;
        let feed = |sim: &mut Sim, ms: u64, plain_flows: bool| {
            for _ in 0..ms / 10 {
                sim.advance(10);
                if plain_flows {
                    sim.s
                        .observe(sim.now, &plain, None, payload, 1, false, false);
                }
            }
        };
        feed(&mut sim, 1_500, true);
        sim.s.route(sim.now, &RECEIVER, &plain, None, None);
        // 1.5 + 1.0 Mbit/s fit the budget: nothing is lowered.
        sim.estimate(3_200_000, false);
        assert_eq!(sim.caps(), [2]);
        // The plain video freezes for longer than a layer may be silent, and
        // the budget falls (an estimate with the video not flowing: nothing
        // to lower for).
        feed(&mut sim, LAYER_TIMEOUT.as_millis() as u64 + 500, false);
        sim.estimate(2_000_000, false);
        assert_eq!(sim.caps(), [2]);
        // It resumes: no estimate event follows. Its rate is unknown until a
        // window has passed, and the loop is told when that is.
        feed(&mut sim, 10, true);
        let measured = sim.now + RATE_WINDOW;
        assert_eq!(sim.caps(), [2]);
        assert_eq!(sim.s.next_wake(), Some(measured));
        feed(&mut sim, 990, true);
        sim.s.run_due(measured);
        // 1.5 + 1.0 Mbit/s no longer fit 0.85 * 2 Mbit/s: the step-down.
        assert_eq!(sim.caps(), [1]);
    }

    #[test]
    fn layers_that_start_after_the_last_allocation_are_upgrade_candidates() {
        let mut s = LayerSelector::default();
        s.set_bwe(&RECEIVER);
        let mut sim = Sim {
            s,
            now: Instant::now(),
            tracks: vec![(track_n(3), RATES)],
            audio: Vec::new(),
        };
        let track = sim.tracks[0].0.clone();
        // Only low flows when the estimate comes: nothing to upgrade to.
        sim.advance_layers(1_500, &[0]);
        sim.s.route(
            sim.now,
            &RECEIVER,
            &track,
            Some(&Rid::from("low")),
            Some(true),
        );
        sim.estimate(3_000_000, true);
        assert_eq!((sim.s.next_wake(), sim.caps()), (None, vec![0]));
        // Medium and high start. No estimate, no other event: their first
        // packets are the event, and the loop is told when their rates can
        // be measured.
        sim.advance(10);
        let first = sim.now;
        let measured = first + RATE_WINDOW;
        assert_eq!(sim.s.next_wake(), Some(measured));
        sim.advance(990);
        sim.s.run_due(measured);
        assert_eq!(sim.s.next_wake(), Some(measured + UPGRADE_HOLD));
        sim.advance(3_990);
        sim.s
            .run_due(measured + UPGRADE_HOLD - Duration::from_millis(1));
        assert_eq!(sim.caps(), [0]);
        sim.s.run_due(measured + UPGRADE_HOLD);
        assert_eq!(sim.caps(), [1]);
        // The demand now reaches for the high layer that started late.
        let demand = sim.s.take_demands()[&RECEIVER];
        // (The window's last packet is 20 ms old: its rate reads 1 % short.)
        let high = 0.98 * RATES[2] as f64;
        assert!(demand as f64 >= UPGRADE_HEADROOM * high / BUDGET_FRACTION);
    }

    #[test]
    fn a_receiver_that_is_not_budgeted_gets_the_best_layer_its_ceiling_allows() {
        let now = Instant::now();
        let mut s = selector(now, &["high", "medium", "low"]);
        s.set_ceiling(now, &RECEIVER, "auto");
        assert_eq!(route(&mut s, now, "high", true, Some("auto")), FORWARD);
        assert_eq!(s.next_wake(), None);
    }

    #[test]
    fn removals_clear_the_budget_state() {
        let mut sim = Sim::new(&[RATES, RATES]);
        sim.estimate(3_000_000, true);
        assert!(sim.s.next_wake().is_some());
        sim.place(0);
        let sender = sim.tracks[0].0 .0.clone();
        sim.s.forget(sim.now, &sender);
        let state = &sim.s.receivers[&RECEIVER];
        assert_eq!(state.tracks.len(), 1);
        assert_eq!(state.caps.len(), 1);
        assert_eq!(sim.s.meters.len(), 1);
        sim.s.forget(sim.now, &RECEIVER);
        assert!(sim.s.receivers.is_empty());
        assert_eq!(sim.s.next_wake(), None);
        assert!(sim.s.take_demands().is_empty());
    }
}
