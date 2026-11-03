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
use str0m::media::{Mid, Rid};

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
    last_request: HashMap<(Track, Rid), Instant>,
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

    /// The layer `receiver` gets of `track`, for its keyframe requests.
    pub fn layer_of(&self, receiver: &ParticipantId, track: &Track) -> Option<Rid> {
        self.forwarding
            .get(&(receiver.clone(), track.clone()))
            .copied()
    }

    /// Drop all state of a participant that left, as sender and as receiver.
    pub fn forget(&mut self, participant: &ParticipantId) {
        self.last_seen
            .retain(|(sender, _), _| sender != participant);
        self.forwarding
            .retain(|(receiver, (sender, _)), _| receiver != participant && sender != participant);
        self.last_request
            .retain(|((sender, _), _), _| sender != participant);
    }

    fn throttle(&mut self, now: Instant, track: &Track, rid: Rid) -> Option<Rid> {
        let key = (track.clone(), rid);
        if let Some(last) = self.last_request.get(&key) {
            if now.duration_since(*last) < KEYFRAME_REQUEST_INTERVAL {
                return None;
            }
        }
        self.last_request.insert(key, now);
        Some(rid)
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

    #[test]
    fn forget_drops_a_participant_as_sender_and_receiver() {
        let now = Instant::now();
        let mut s = selector(now, &["high"]);
        assert_eq!(route(&mut s, now, "high", true, None), FORWARD);
        s.forget(&SENDER);
        assert_eq!(s.layer_of(&RECEIVER, &track()), None);
        assert!(s.last_seen.is_empty() && s.last_request.is_empty());
    }
}
