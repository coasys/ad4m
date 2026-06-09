//! Media relay — selective forwarding and active speaker detection.
//!
//! The relay tracks voice activity per participant and provides
//! active speaker detection based on audio energy levels.

use std::collections::{HashMap, HashSet};
use std::time::Instant;

use str0m::media::MediaData;

use super::room::ParticipantId;

/// Tracks voice activity for active speaker detection.
#[derive(Debug)]
struct VoiceActivityState {
    /// Smoothed audio level (0.0 = silence, 1.0 = max)
    level: f64,
    /// Last time we received audio from this participant
    last_audio: Instant,
    /// Whether currently considered "speaking"
    is_speaking: bool,
}

impl Default for VoiceActivityState {
    fn default() -> Self {
        Self {
            level: 0.0,
            last_audio: Instant::now(),
            is_speaking: false,
        }
    }
}

/// The media relay tracks voice activity and determines active speakers.
///
/// Actual media forwarding is handled in the server event loop (server.rs),
/// since it needs direct access to the str0m Rtc instances. The relay module
/// provides the intelligence layer: which participant is the active speaker,
/// voice activity tracking, etc.
#[derive(Debug)]
pub struct MediaRelay {
    voice_activity: HashMap<ParticipantId, VoiceActivityState>,
    /// The current active speaker (participant with highest sustained audio level)
    active_speaker: Option<ParticipantId>,
    /// Set of participant IDs that represent pipe transports (not real users).
    /// Media from these should not be forwarded back to the originating pipe.
    pipe_transport_ids: HashSet<ParticipantId>,
}

/// Threshold for considering a participant as "speaking"
const SPEAKING_THRESHOLD: f64 = 0.01;
/// Time after last audio before a participant is no longer considered speaking
const SPEAKING_TIMEOUT_MS: u128 = 500;
/// Approximate size of a "full energy" Opus packet for normalization
const OPUS_FULL_ENERGY_SIZE: f64 = 200.0;

impl MediaRelay {
    pub fn new() -> Self {
        Self {
            voice_activity: HashMap::new(),
            active_speaker: None,
            pipe_transport_ids: HashSet::new(),
        }
    }

    /// Update voice activity for a participant based on incoming audio data.
    /// Uses a simple energy estimation from the first few bytes of the RTP payload.
    pub fn update_voice_activity(&mut self, pid: &ParticipantId, data: &MediaData) {
        if !data.params.spec().codec.is_audio() {
            return;
        }

        let state = self.voice_activity.entry(pid.clone()).or_default();

        // Simple audio energy estimation from RTP payload
        // This is a rough heuristic — real VAD would decode the Opus frame
        let energy = if data.data.len() > 2 {
            // Opus silence packets are very small (typically 1-3 bytes)
            // Larger packets generally indicate voice activity
            let size_energy = (data.data.len() as f64 / OPUS_FULL_ENERGY_SIZE).min(1.0);
            size_energy
        } else {
            0.0
        };

        // Exponential moving average for smoothing
        state.level = state.level * 0.7 + energy * 0.3;
        state.last_audio = Instant::now();
        state.is_speaking = state.level > SPEAKING_THRESHOLD;

        // Update active speaker
        self.update_active_speaker();
    }

    /// Determine the current active speaker.
    fn update_active_speaker(&mut self) {
        let now = Instant::now();
        let mut best: Option<(ParticipantId, f64)> = None;

        for (pid, state) in &self.voice_activity {
            // Skip if no recent audio
            if now.duration_since(state.last_audio).as_millis() > SPEAKING_TIMEOUT_MS {
                continue;
            }

            if state.is_speaking {
                match &best {
                    Some((_, best_level)) if state.level > *best_level => {
                        best = Some((pid.clone(), state.level));
                    }
                    None => {
                        best = Some((pid.clone(), state.level));
                    }
                    _ => {}
                }
            }
        }

        self.active_speaker = best.map(|(pid, _)| pid);
    }

    /// Get the current active speaker, if any.
    pub fn active_speaker(&self) -> Option<&ParticipantId> {
        self.active_speaker.as_ref()
    }

    /// Check if a specific participant is currently speaking.
    pub fn is_speaking(&self, pid: &ParticipantId) -> bool {
        self.voice_activity
            .get(pid)
            .map(|s| {
                s.is_speaking
                    && Instant::now().duration_since(s.last_audio).as_millis()
                        <= SPEAKING_TIMEOUT_MS
            })
            .unwrap_or(false)
    }

    /// Remove a participant from voice activity tracking.
    pub fn remove_participant(&mut self, pid: &ParticipantId) {
        self.voice_activity.remove(pid);
        self.pipe_transport_ids.remove(pid);
        if self.active_speaker.as_ref() == Some(pid) {
            self.active_speaker = None;
            self.update_active_speaker();
        }
    }

    /// Register a participant ID as a pipe transport endpoint.
    pub fn register_pipe_transport(&mut self, pid: ParticipantId) {
        self.pipe_transport_ids.insert(pid);
    }

    /// Check if a participant ID represents a pipe transport.
    pub fn is_pipe_transport(&self, pid: &ParticipantId) -> bool {
        self.pipe_transport_ids.contains(pid)
    }

    /// Unregister a pipe transport participant.
    pub fn unregister_pipe_transport(&mut self, pid: &ParticipantId) {
        self.pipe_transport_ids.remove(pid);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_active_speaker_detection() {
        let relay = MediaRelay::new();
        assert!(relay.active_speaker().is_none());
    }

    #[test]
    fn test_remove_participant() {
        let mut relay = MediaRelay::new();
        let p1 = ParticipantId::next();

        // Add some voice activity state
        relay.voice_activity.insert(
            p1.clone(),
            VoiceActivityState {
                level: 0.5,
                last_audio: Instant::now(),
                is_speaking: true,
            },
        );
        relay.active_speaker = Some(p1.clone());

        relay.remove_participant(&p1);
        assert!(relay.active_speaker().is_none());
        assert!(!relay.is_speaking(&p1));
    }
}
