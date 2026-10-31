//! Transcription on an endpoint, and what streams and `/v1/audio/transcriptions` share:
//! billing and the `transcription-text` event.
//!
//! A stream has no executor-side voice-activity gate: the client cuts
//! utterances (WE's worklet) and the upstream filters silence. Each utterance fed is encoded
//! as a 16 kHz mono PCM16 WAV and sent as one `/v1/audio/transcriptions` call, one at a time,
//! so text comes out in the order the audio went in.

use anyhow::anyhow;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::{broadcast, mpsc, oneshot};

use super::providers::audio::{AudioFile, OpenAiTranscribe, RemoteTranscribe, TranscribeFailure};
use super::Result;
use crate::db::Ad4mDb;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use crate::types::{ModelType, TranscriptionTextFilter};

/// What every feed carries: 16 kHz mono `f32`.
pub(crate) const SAMPLE_RATE: u32 = 16_000;

/// The client of the endpoint that serves transcription model `model_id`, and the id the
/// endpoint knows the model by.
pub(crate) fn endpoint_transcriber(model_id: &str) -> Result<(OpenAiTranscribe, String)> {
    let model = Ad4mDb::with_global_instance(|db| db.get_model(model_id.to_string()))?
        .ok_or_else(|| anyhow!("No transcription model has the id '{model_id}'"))?;
    if model.model_type != ModelType::Transcription {
        return Err(anyhow!(
            "Model '{}' is not a transcription model",
            model.name
        ));
    }
    let endpoint = Ad4mDb::with_global_instance(|db| db.get_endpoint(&model.endpoint_id))?
        .ok_or_else(|| {
            anyhow!(
                "The endpoint of transcription model '{}' is gone",
                model.name
            )
        })?;
    Ok((
        OpenAiTranscribe::for_endpoint(&endpoint)?,
        model.remote_model,
    ))
}

/// Bill `words` transcribed by `model_id` to the managed agent `agent_did` (`None`: the main
/// agent, which pays nothing). The one place transcription is billed: streams and
/// `/v1/audio/transcriptions` both come here, once per transcript.
pub(crate) fn bill_transcription(agent_did: Option<&str>, model_id: &str, words: usize) {
    if let Some(agent_did) = agent_did {
        // Insufficient credits: the text is already computed and is delivered; the next feed
        // is refused by the credit check.
        let _ = crate::billing::bill_ai_operation(
            agent_did,
            model_id,
            "ai_transcription",
            words,
            "words",
        );
    }
}

/// Send `text` of stream `stream_id` to the sessions of its owner `user_did`, and to the
/// stream's own listeners (`/v1/realtime`).
pub(crate) async fn publish_text(
    stream_id: &str,
    user_did: &str,
    text: &str,
    listeners: &broadcast::Sender<String>,
) {
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AiTranscriptionText,
        Audience::agent(Some(user_did)),
        TranscriptionTextFilter {
            stream_id: stream_id.to_string(),
            text: text.to_string(),
            user_did: Some(user_did.to_string()),
        },
    ));
    let _ = listeners.send(text.to_string());
}

/// `samples` as a mono PCM16 WAV file at [`SAMPLE_RATE`]. Out-of-range samples clip.
pub(crate) fn wav_pcm16(samples: &[f32]) -> Vec<u8> {
    let data_len = (samples.len() * 2) as u32;
    let mut wav = Vec::with_capacity(44 + samples.len() * 2);
    wav.extend_from_slice(b"RIFF");
    wav.extend_from_slice(&(36 + data_len).to_le_bytes());
    wav.extend_from_slice(b"WAVEfmt ");
    wav.extend_from_slice(&16u32.to_le_bytes());
    wav.extend_from_slice(&1u16.to_le_bytes()); // PCM
    wav.extend_from_slice(&1u16.to_le_bytes()); // mono
    wav.extend_from_slice(&SAMPLE_RATE.to_le_bytes());
    wav.extend_from_slice(&(SAMPLE_RATE * 2).to_le_bytes()); // bytes per second
    wav.extend_from_slice(&2u16.to_le_bytes()); // block align
    wav.extend_from_slice(&16u16.to_le_bytes()); // bits per sample
    wav.extend_from_slice(b"data");
    wav.extend_from_slice(&data_len.to_le_bytes());
    for sample in samples {
        let pcm = (sample.clamp(-1.0, 1.0) * i16::MAX as f32).round() as i16;
        wav.extend_from_slice(&pcm.to_le_bytes());
    }
    wav
}

/// One fed utterance, and where its outcome goes.
pub(super) struct Utterance {
    pub samples: Vec<f32>,
    pub done: oneshot::Sender<Result<()>>,
}

/// A stream on an endpoint. Its worker takes the utterances in the order they were fed.
pub(super) struct RemoteStream {
    pub stream_id: String,
    /// The executor's model id: the billing key.
    pub model_id: String,
    /// The id the upstream knows.
    pub remote_model: String,
    pub client: Arc<dyn RemoteTranscribe>,
    pub agent_did: Option<String>,
    pub user_did: String,
    pub listeners: broadcast::Sender<String>,
}

impl RemoteStream {
    /// Transcribe each utterance in turn until the stream closes (its sender is dropped);
    /// the ones already fed are still answered.
    pub(super) async fn run(self, mut utterances: mpsc::UnboundedReceiver<Utterance>) {
        while let Some(utterance) = utterances.recv().await {
            let outcome = self.transcribe(utterance.samples).await;
            let _ = utterance.done.send(outcome);
        }
        log::info!("Transcription stream {} closed", self.stream_id);
    }

    /// One upstream call. A failure is logged and returned to the feed; nothing is
    /// published or billed.
    async fn transcribe(&self, samples: Vec<f32>) -> Result<()> {
        if samples.is_empty() {
            return Ok(());
        }
        let audio_ms = samples.len() as u64 * 1000 / SAMPLE_RATE as u64;
        let audio = AudioFile {
            bytes: wav_pcm16(&samples),
            file_name: "utterance.wav".into(),
            mime: "audio/wav".into(),
        };
        let started = Instant::now();
        let reply = match self
            .client
            .transcribe(&self.remote_model, audio, vec![])
            .await
        {
            Ok(reply) => reply,
            Err(e) => {
                log::warn!(
                    "transcription stream={} model={} audio_ms={audio_ms} failed: {e:#}",
                    self.stream_id,
                    self.remote_model
                );
                return Err(anyhow!(TranscribeFailure::of(&e).to_string()));
            }
        };
        let words = reply.text.split_whitespace().count();
        log::info!(
            "transcription stream={} model={} audio_ms={audio_ms} latency_ms={} words={words}",
            self.stream_id,
            self.remote_model,
            started.elapsed().as_millis()
        );
        if words == 0 {
            return Ok(());
        }
        bill_transcription(self.agent_did.as_deref(), &self.model_id, words);
        publish_text(
            &self.stream_id,
            &self.user_did,
            &reply.text,
            &self.listeners,
        )
        .await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_wav_is_16_khz_mono_pcm16_and_clips() {
        let wav = wav_pcm16(&[0.0, 1.0, -1.0, 2.0, 0.5]);
        assert_eq!(&wav[0..4], b"RIFF");
        assert_eq!(&wav[8..16], b"WAVEfmt ");
        assert_eq!(u32::from_le_bytes(wav[4..8].try_into().unwrap()), 36 + 10);
        assert_eq!(u16::from_le_bytes([wav[22], wav[23]]), 1, "mono");
        assert_eq!(u32::from_le_bytes(wav[24..28].try_into().unwrap()), 16_000);
        assert_eq!(u16::from_le_bytes([wav[34], wav[35]]), 16, "bits");
        assert_eq!(&wav[36..40], b"data");
        assert_eq!(u32::from_le_bytes(wav[40..44].try_into().unwrap()), 10);
        let pcm: Vec<i16> = wav[44..]
            .chunks_exact(2)
            .map(|c| i16::from_le_bytes([c[0], c[1]]))
            .collect();
        assert_eq!(pcm, vec![0, i16::MAX, -i16::MAX, i16::MAX, 16384]);
    }

    // The guard of round-1 finding #2 (double billing), now that two paths transcribe: the
    // operation label appears once in the transcription code, here.
    #[test]
    fn transcription_is_billed_in_one_place() {
        let label = concat!("\"ai_", "transcription\"");
        let sources = [
            ("ai_service/mod.rs", include_str!("mod.rs")),
            (
                "ai_service/transcription.rs",
                include_str!("transcription.rs"),
            ),
            ("api/ai_ws.rs", include_str!("../api/ai_ws.rs")),
            (
                "api/openai_compat/audio.rs",
                include_str!("../api/openai_compat/audio.rs"),
            ),
            (
                "api/openai_compat/realtime.rs",
                include_str!("../api/openai_compat/realtime.rs"),
            ),
        ];
        let places: Vec<&str> = sources
            .iter()
            .filter(|(_, source)| source.contains(label))
            .map(|(file, _)| *file)
            .collect();
        assert_eq!(places, vec!["ai_service/transcription.rs"]);
        assert_eq!(include_str!("transcription.rs").matches(label).count(), 1);
    }
}
