//! Transcription on an endpoint, and what both transcription paths share: billing and the
//! `transcription-text` event.
//!
//! The client cuts utterances (WE's worklet, by energy); the executor scores each one with
//! Silero VAD ([`SpeechGate`], dev's gate and thresholds) and drops one that is not speech
//! (a cough, a keyboard, a fan) before any request: a model without VAD of its own makes
//! text up for it. Each utterance that passes is encoded as a 16 kHz mono PCM16 WAV and
//! sent as one `/v1/audio/transcriptions` call, one at a time, so text comes out in the
//! order the audio went in. An upstream that streams its transcript publishes each piece as
//! it decodes it, as dev published each Whisper segment
//! ([`RemoteTranscribe::transcribe_pieces`]); one that does not, the whole utterance.

use anyhow::anyhow;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::{broadcast, mpsc, oneshot};

use super::providers::audio::{
    require_transcription, AudioFile, OpenAiTranscribe, RemoteTranscribe, TranscribeFailure,
};
use super::Result;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use crate::types::{ModelType, TranscriptionTextFilter};

/// What every feed carries: 16 kHz mono `f32`.
pub(crate) const SAMPLE_RATE: u32 = 16_000;

/// Silero's window at [`SAMPLE_RATE`]: 512 samples, 32 ms.
pub(crate) const VAD_WINDOW: usize = 512;
/// A window is speech above this probability (dev: the Kalosm rechunker's start threshold).
pub(crate) const VAD_SPEECH_PROBABILITY: f32 = 0.6;
/// An utterance is speech when at least this share of its windows is (dev: rejects
/// transients).
pub(crate) const VAD_MIN_SPEECH_RATIO: f32 = 0.33;

/// The server-side speech gate of a stream: Silero VAD over each fed utterance, its state
/// carried from one utterance to the next as dev's was. `None` when the model did not load
/// (ONNX Runtime missing where it is loaded at run time, macOS): every utterance passes,
/// with a warning at open, rather than the stream refusing all audio.
pub(crate) struct SpeechGate {
    vad: Option<voice_activity_detector::VoiceActivityDetector>,
}

impl SpeechGate {
    pub(crate) fn new() -> Self {
        // ort panics, rather than returning an error, when it loads ONNX Runtime at run time
        // and the library is not there (macOS): caught, so the stream still opens.
        let built = std::panic::catch_unwind(|| {
            voice_activity_detector::VoiceActivityDetector::builder()
                .sample_rate(SAMPLE_RATE as i64)
                .chunk_size(VAD_WINDOW)
                .build()
        });
        let vad = match built {
            Ok(Ok(vad)) => Some(vad),
            Ok(Err(e)) => {
                log::warn!("Silero VAD did not load ({e:?}): this stream sends all audio");
                None
            }
            Err(_) => {
                log::warn!("Silero VAD did not load (ONNX Runtime is missing): this stream sends all audio");
                None
            }
        };
        SpeechGate { vad }
    }

    /// The share of `samples`' whole windows that are speech; 0 for fewer than one window,
    /// 1 without a model.
    pub(crate) fn speech_ratio(&mut self, samples: &[f32]) -> f32 {
        let Some(vad) = self.vad.as_mut() else {
            return 1.0;
        };
        let mut total = 0usize;
        let mut speech = 0usize;
        for window in samples.chunks_exact(VAD_WINDOW) {
            total += 1;
            if vad.predict(window.iter().copied()) > VAD_SPEECH_PROBABILITY {
                speech += 1;
            }
        }
        if total == 0 {
            0.0
        } else {
            speech as f32 / total as f32
        }
    }

    /// Whether `samples` is speech, by dev's thresholds.
    pub(crate) fn passes(&mut self, samples: &[f32]) -> bool {
        self.speech_ratio(samples) >= VAD_MIN_SPEECH_RATIO
    }
}

/// The client and upstream model id of the transcription model `model_id` when an endpoint
/// serves it. `None` for local Whisper: a preset name, or a row without `api`.
pub(crate) fn endpoint_transcriber(model_id: &str) -> Result<Option<(OpenAiTranscribe, String)>> {
    let Some(model) = crate::db_backend::db_backend().get_model(model_id.to_string())? else {
        return Ok(None);
    };
    if model.model_type != ModelType::Transcription {
        return Err(anyhow!(
            "Model '{}' is not a transcription model",
            model.name
        ));
    }
    let Some(api) = model.api else {
        return Ok(None);
    };
    let endpoint_id = model
        .endpoint_id
        .ok_or_else(|| anyhow!("Transcription model '{}' has no endpoint", model.name))?;
    let endpoint = crate::db_backend::db_backend()
        .get_endpoint(&endpoint_id)?
        .ok_or_else(|| {
            anyhow!(
                "The endpoint of transcription model '{}' is gone",
                model.name
            )
        })?;
    require_transcription(&endpoint)?;
    Ok(Some((
        OpenAiTranscribe::for_endpoint(&endpoint)?,
        api.model,
    )))
}

/// How long one upstream call of the transcription model `model_id` may take: its
/// endpoint's `request_timeout_secs`, else the provider's
/// [`REQUEST_TIMEOUT`](super::providers::audio::REQUEST_TIMEOUT). A model without an
/// endpoint gets the provider's timeout too.
pub(crate) fn call_timeout(model_id: &str) -> std::time::Duration {
    let default = super::providers::audio::REQUEST_TIMEOUT;
    let db = crate::db_backend::db_backend();
    db.get_model(model_id.to_string())
        .ok()
        .flatten()
        .and_then(|model| model.endpoint_id)
        .and_then(|id| db.get_endpoint(&id).ok().flatten())
        .map_or(default, |endpoint| {
            super::endpoints::request_timeout(&endpoint, default)
        })
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
    /// `None` once the gate failed.
    pub gate: Option<SpeechGate>,
}

impl RemoteStream {
    /// Transcribe each utterance in turn until the stream closes (its sender is dropped);
    /// the ones already fed are still answered.
    pub(super) async fn run(mut self, mut utterances: mpsc::UnboundedReceiver<Utterance>) {
        while let Some(utterance) = utterances.recv().await {
            let outcome = self.transcribe(utterance.samples).await;
            let _ = utterance.done.send(outcome);
        }
        log::info!("Transcription stream {} closed", self.stream_id);
    }

    /// One upstream call, for an utterance the gate takes as speech. Each piece of text a
    /// streaming upstream decodes is published as it arrives; the words are billed once,
    /// from the whole transcript. A failure is logged and returned to the feed; nothing more
    /// is published, and only what was published is billed.
    async fn transcribe(&mut self, samples: Vec<f32>) -> Result<()> {
        if samples.is_empty() {
            return Ok(());
        }
        let audio_ms = samples.len() as u64 * 1000 / SAMPLE_RATE as u64;
        // Silero takes milliseconds per second of audio: on a blocking thread, not on a
        // worker of the runtime. The gate goes along and comes back with its state; when it
        // panicked it does not come back, and the stream sends no more audio.
        let Some(gate) = self.gate.take() else {
            return Err(anyhow!(
                "The speech gate of this stream failed: it transcribes no more audio"
            ));
        };
        let (gate, samples, speech_ratio) = tokio::task::spawn_blocking(move || {
            let mut gate = gate;
            let ratio = gate.speech_ratio(&samples);
            (gate, samples, ratio)
        })
        .await
        .map_err(|e| anyhow!("The speech gate failed: {e}"))?;
        self.gate = Some(gate);
        if speech_ratio < VAD_MIN_SPEECH_RATIO {
            log::debug!(
                "transcription stream={} audio_ms={audio_ms} dropped by the speech gate (speech_ratio={speech_ratio:.2})",
                self.stream_id
            );
            return Ok(());
        }
        // Checked again here, not only when the utterance was fed: utterances queue, and the
        // bill of one lands before the next is sent.
        if let Some(agent_did) = self.agent_did.as_deref() {
            crate::billing::check_compute_credits(agent_did)
                .map_err(|_| anyhow!("Insufficient compute credits"))?;
        }
        let audio = AudioFile {
            bytes: wav_pcm16(&samples),
            file_name: "utterance.wav".into(),
            mime: "audio/wav".into(),
        };
        let started = Instant::now();
        let (pieces_tx, mut pieces) = mpsc::unbounded_channel::<String>();
        let call = self
            .client
            .transcribe_pieces(&self.remote_model, audio, &pieces_tx);
        tokio::pin!(call);
        let mut published = String::new();
        let outcome = loop {
            tokio::select! {
                biased;
                Some(piece) = pieces.recv() => self.publish_piece(&piece, &mut published).await,
                outcome = &mut call => break outcome,
            }
        };
        while let Ok(piece) = pieces.try_recv() {
            self.publish_piece(&piece, &mut published).await;
        }
        let text = match outcome {
            Ok(text) => text,
            Err(e) => {
                log::warn!(
                    "transcription stream={} model={} audio_ms={audio_ms} failed: {e:#}",
                    self.stream_id,
                    self.remote_model
                );
                let words = published.split_whitespace().count();
                if words > 0 {
                    bill_transcription(self.agent_did.as_deref(), &self.model_id, words);
                }
                return Err(anyhow!(TranscribeFailure::of(&e).to_string()));
            }
        };
        let words = text.split_whitespace().count();
        log::info!(
            "transcription stream={} model={} audio_ms={audio_ms} latency_ms={} words={words} streamed={}",
            self.stream_id,
            self.remote_model,
            started.elapsed().as_millis(),
            !published.is_empty()
        );
        if words == 0 {
            return Ok(());
        }
        bill_transcription(self.agent_did.as_deref(), &self.model_id, words);
        if published.is_empty() {
            publish_text(&self.stream_id, &self.user_did, &text, &self.listeners).await;
        }
        Ok(())
    }

    /// Publish one streamed piece as the upstream sent it, its spacing kept: a piece that is
    /// only a space separates the words on each side of it. Empty ones are skipped.
    async fn publish_piece(&self, piece: &str, published: &mut String) {
        if piece.is_empty() {
            return;
        }
        published.push_str(piece);
        publish_text(&self.stream_id, &self.user_did, piece, &self.listeners).await;
    }
}

/// "If you can read this, transcription is working": `tests/transcription_test.wav`, 16 kHz
/// mono PCM16, 4.9 s, as samples.
#[cfg(test)]
pub(crate) fn speech_fixture() -> Vec<f32> {
    let wav = std::fs::read(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/transcription_test.wav"
    ))
    .expect("the test recording is in the repo");
    assert_eq!(&wav[36..40], b"data", "a plain 44-byte header");
    wav[44..]
        .chunks_exact(2)
        .map(|c| i16::from_le_bytes([c[0], c[1]]) as f32 / i16::MAX as f32)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    // The gate's model loads from the weights compiled in, and scores the recording as
    // speech and silence as not.
    #[test]
    fn the_speech_gate_takes_speech_and_drops_silence_and_short_clicks() {
        let mut gate = SpeechGate::new();
        let speech = speech_fixture();
        let ratio = gate.speech_ratio(&speech);
        assert!(ratio >= VAD_MIN_SPEECH_RATIO, "speech scored {ratio}");
        assert!(gate.passes(&speech));
        assert!(!gate.passes(&vec![0.0; 16_000]));
        assert_eq!(
            gate.speech_ratio(&[0.5; VAD_WINDOW - 1]),
            0.0,
            "under one window"
        );
    }

    /// An upstream that must not be called.
    struct NoUpstream;

    #[async_trait::async_trait]
    impl RemoteTranscribe for NoUpstream {
        async fn transcribe(
            &self,
            _model: &str,
            _audio: AudioFile,
            _fields: Vec<(String, String)>,
        ) -> anyhow::Result<super::super::providers::audio::TranscribeReply> {
            panic!("the upstream was called");
        }
    }

    // A gate that panicked left the stream with no gate, and every later utterance, silence
    // too, went to the upstream.
    #[tokio::test]
    async fn a_stream_whose_gate_failed_sends_nothing_more() {
        let (listeners, _) = broadcast::channel(4);
        let mut stream = RemoteStream {
            stream_id: "s".into(),
            model_id: "m".into(),
            remote_model: "r".into(),
            client: Arc::new(NoUpstream),
            agent_did: None,
            user_did: "did:key:owner".into(),
            listeners,
            gate: None,
        };
        assert!(stream.transcribe(vec![0.0; 16_000]).await.is_err());
    }

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
