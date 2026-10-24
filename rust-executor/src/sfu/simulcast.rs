//! Simulcast layer selection.
//!
//! A simulcast sender sends one video track as up to three layers, each an
//! RTP stream with its own rid ("high", "medium", "low"). The sender's
//! encoder decides which layers flow: it drops layers the resolution or the
//! bandwidth cannot carry. For each receiver of the track, the selector
//! forwards exactly one layer:
//!
//! - the receiver's preference when that layer flows, otherwise the nearest
//!   flowing layer below it, otherwise the nearest above it;
//! - a change of layer waits for a keyframe on the new layer — a decoder
//!   cannot start from a delta frame — and the old layer keeps flowing until
//!   then;
//! - a keyframe the switch needs is requested from the sender, for that rid,
//!   at most once per [`KEYFRAME_REQUEST_INTERVAL`].
//!
//! A packet without a rid comes from a sender without simulcast and always
//! passes.

use std::collections::HashMap;
use std::time::{Duration, Instant};

use str0m::format::CodecExtra;
use str0m::media::{MediaTime, Mid, Rid};

use super::room::ParticipantId;

/// A layer that sent nothing for this long no longer flows.
const LAYER_TIMEOUT: Duration = Duration::from_secs(1);
/// The shortest time between two keyframe requests for one layer.
const KEYFRAME_REQUEST_INTERVAL: Duration = Duration::from_millis(500);

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
    /// When each layer of each track last sent a packet.
    last_seen: HashMap<Track, HashMap<Rid, Instant>>,
    /// The layer each receiver gets of each track.
    forwarding: HashMap<(ParticipantId, Track), Rid>,
    /// When a keyframe was last asked for, per layer (`None`: a sender
    /// without simulcast).
    last_request: HashMap<(Track, Option<Rid>), Instant>,
    /// Each receiver's RTP clock of each track.
    timelines: HashMap<(ParticipantId, Track), Timeline>,
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

/// Layers in the order a preference accepts them.
fn layers_for(preference: Option<&str>) -> [&'static str; 3] {
    match preference {
        Some("low") => ["low", "medium", "high"],
        Some("medium") => ["medium", "low", "high"],
        // "high", "auto", none and unknown values
        _ => ["high", "medium", "low"],
    }
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
    /// Record a packet of `track`. Call once per packet, before [`Self::route`].
    pub fn observe(&mut self, now: Instant, track: &Track, rid: Option<&Rid>) {
        if let Some(rid) = rid {
            self.last_seen
                .entry(track.clone())
                .or_default()
                .insert(*rid, now);
        }
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
        preference: Option<&str>,
    ) -> Route {
        let Some(&rid) = rid else {
            return Route {
                forward: true,
                request_keyframe: None,
            };
        };
        let seen = self.last_seen.get(track);
        let flows = |layer: &Rid| {
            seen.and_then(|s| s.get(layer))
                .is_some_and(|at| now.duration_since(*at) < LAYER_TIMEOUT)
        };
        let wanted = layers_for(preference)
            .into_iter()
            .map(Rid::from)
            .find(|layer| flows(layer))
            .unwrap_or(rid);

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
            let request_keyframe = if keyframe.is_none() {
                self.throttle(now, track, wanted)
            } else {
                None
            };
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
    pub fn forget(&mut self, participant: &ParticipantId) {
        self.last_seen
            .retain(|(sender, _), _| sender != participant);
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
            s.observe(now, &track(), Some(&Rid::from(*layer)));
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
        s.observe(now, &track(), Some(&Rid::from(rid)));
        s.route(
            now,
            &RECEIVER,
            &track(),
            Some(&Rid::from(rid)),
            Some(keyframe),
            pref,
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
            s.route(now, &RECEIVER, &track(), None, Some(false), Some("low")),
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
        s.observe(now, &track(), Some(&Rid::from("high")));
        let routed = s.route(
            now,
            &RECEIVER,
            &track(),
            Some(&Rid::from("high")),
            None,
            None,
        );
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
        s.forget(&SENDER);
        assert_eq!(s.layer_of(&RECEIVER, &track()), None);
        assert!(s.last_seen.is_empty() && s.last_request.is_empty());
        forwarded(&mut s, Instant::now(), "high", HIGH_BASE);
        s.forget(&RECEIVER);
        assert!(s.timelines.is_empty());
    }
}
