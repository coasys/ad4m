//! Transcription against a real OpenAI-shaped server (speaches, a Parakeet wrapper, ...).
//!
//! Skipped unless `TRANSCRIPTION_E2E_BASE_URL` (e.g. `http://127.0.0.1:8000`) and
//! `TRANSCRIPTION_E2E_MODEL` (the id the server knows) are set; `TRANSCRIPTION_E2E_API_KEY`
//! when the server takes one. Run:
//!
//! ```sh
//! TRANSCRIPTION_E2E_BASE_URL=http://127.0.0.1:8000 \
//! TRANSCRIPTION_E2E_MODEL=Systran/faster-whisper-small \
//! cargo test --release --lib ai_service::transcription_e2e -- --test-threads=1 --nocapture
//! ```

use std::sync::Arc;

use super::providers::audio::{AudioFile, OpenAiTranscribe, RemoteTranscribe};
use super::transcription::SAMPLE_RATE;
use super::AIService;
use crate::db::Ad4mDb;
use crate::types::RequestContext;

/// The configured server and model, or `None` (the test is skipped).
fn upstream() -> Option<(OpenAiTranscribe, String)> {
    let base_url = std::env::var("TRANSCRIPTION_E2E_BASE_URL").ok()?;
    let model = std::env::var("TRANSCRIPTION_E2E_MODEL").ok()?;
    let key = std::env::var("TRANSCRIPTION_E2E_API_KEY").unwrap_or_default();
    let base_url = url::Url::parse(&base_url).expect("TRANSCRIPTION_E2E_BASE_URL is a URL");
    Some((OpenAiTranscribe::new(&key, base_url), model))
}

// The passthrough's case: the client's own container (here m4a) goes up as it is.
#[tokio::test]
async fn a_recording_is_transcribed_as_uploaded() {
    let Some((client, model)) = upstream() else {
        eprintln!("skipped: TRANSCRIPTION_E2E_BASE_URL / TRANSCRIPTION_E2E_MODEL not set");
        return;
    };
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/transcription_test.m4a"
    );
    let audio = AudioFile {
        bytes: std::fs::read(path).expect("the test recording is in the repo"),
        file_name: "transcription_test.m4a".into(),
        mime: "audio/mp4".into(),
    };

    let reply = client.transcribe(&model, audio, vec![]).await.unwrap();

    eprintln!("transcript: {}", reply.text);
    // The recording says "If you can read this, transcription is working".
    assert!(
        reply
            .text
            .to_lowercase()
            .contains("transcription is working"),
        "{:?}",
        reply.text
    );
}

// A stream's case: the executor's own WAV encoding is accepted, and the utterance is answered.
#[tokio::test]
async fn a_streamed_utterance_is_accepted_and_answered() {
    let Some((client, model)) = upstream() else {
        eprintln!("skipped: TRANSCRIPTION_E2E_BASE_URL / TRANSCRIPTION_E2E_MODEL not set");
        return;
    };
    let _ = Ad4mDb::init_global_instance(":memory:");
    let service = AIService::empty();
    let id = service
        .open_remote_stream(
            "e2e".into(),
            model,
            Arc::new(client),
            None,
            "did:key:e2e".into(),
        )
        .await;
    // One second of a quiet tone: no speech, so any text is the model's own.
    let tone: Vec<f32> = (0..SAMPLE_RATE)
        .map(|i| 0.05 * (i as f32 * 440.0 * std::f32::consts::TAU / SAMPLE_RATE as f32).sin())
        .collect();

    service
        .feed_transcription_stream(&id, tone, &RequestContext::operator_for_test())
        .await
        .expect("the upstream takes the executor's WAV");
}
