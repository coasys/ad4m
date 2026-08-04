//! `POST /v1/audio/transcriptions` (batch STT) + `POST /v1/audio/speech` (TTS).
//!
//! ## Transcription (Phase 3)
//!
//! Accepts a standard OpenAI multipart upload: `file`, `model`,
//! `response_format`, `language`, `temperature`.  We decode the upload to
//! 16 kHz mono `f32` samples and drive the existing Whisper pipeline in
//! one shot via [`AIService::transcribe_buffer`].
//!
//! Audio decode is delegated to the new [`audio_decode`] helper.  For
//! the first cut we accept raw 16 kHz mono PCM (wav) directly; for
//! other formats the helper returns an error pointing at the
//! `Content-Type` and the caller is expected to convert client-side
//! (matches the proposal — full container support via `symphonia` is
//! scoped to a follow-up to keep the dependency surface lean).
//!
//! ## TTS (Phase 4)
//!
//! `audio/speech` forwards the request to a remote OpenAI-compatible
//! upstream configured via a TTS-typed `Model` registration — see
//! [`super::tts_passthrough`] for the HTTP client.  A local Kokoro
//! backend is in the design but not in this PR; the proposal allows
//! passthrough as a valid Phase-4 deliverable.

use axum::{extract::Multipart, http::header, response::IntoResponse, response::Response, Json};

use super::errors::{OpenAIError, OpenAIResult};
use super::model_selector::resolve_model;
use super::tts_passthrough;
use super::types::{SpeechRequest, TranscriptionResponse};
use crate::agent::capabilities::{
    check_capability, AI_PROMPT_CAPABILITY, AI_TRANSCRIBE_CAPABILITY,
};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::billing::{bill_compute, check_compute_credits};
use crate::types::ModelType;

pub async fn transcriptions(
    auth: AuthContext,
    mut multipart: Multipart,
) -> Result<Response, OpenAIError> {
    check_capability(&auth.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(OpenAIError::forbidden)?;

    let mut model_field: Option<String> = None;
    let mut audio_bytes: Option<Vec<u8>> = None;
    let mut content_type: Option<String> = None;
    let mut response_format = "json".to_string();

    while let Some(field) = multipart
        .next_field()
        .await
        .map_err(|e| OpenAIError::invalid_request(format!("Multipart parse error: {e}")))?
    {
        match field.name().unwrap_or("") {
            "model" => {
                model_field = field
                    .text()
                    .await
                    .ok()
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty());
            }
            "file" => {
                content_type = field
                    .content_type()
                    .map(|s| s.to_string())
                    .or_else(|| Some("application/octet-stream".to_string()));
                let bytes = field
                    .bytes()
                    .await
                    .map_err(|e| OpenAIError::invalid_request(format!("File read error: {e}")))?;
                audio_bytes = Some(bytes.to_vec());
            }
            "response_format" => {
                if let Ok(v) = field.text().await {
                    response_format = v.trim().to_string();
                }
            }
            _ => {
                // Other OpenAI fields (`language`, `temperature`,
                // `prompt`) are not yet plumbed through to Whisper — we
                // accept and drop them so callers don't 400.
                let _ = field.bytes().await;
            }
        }
    }

    let model = model_field
        .ok_or_else(|| OpenAIError::invalid_request("Missing required form field: model"))?;
    let bytes = audio_bytes
        .ok_or_else(|| OpenAIError::invalid_request("Missing required form field: file"))?;
    let model_id = resolve_model(&model, ModelType::Transcription).await?;

    let samples = audio_decode(&bytes, content_type.as_deref())?;

    if let Some(email) = crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
    {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;
    let text = service
        .transcribe_buffer(model_id, samples, auth.auth_token.clone())
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    if response_format == "text" {
        return Ok(([(header::CONTENT_TYPE, "text/plain")], text).into_response());
    }
    Ok(Json(TranscriptionResponse { text }).into_response())
}

pub async fn speech(
    auth: AuthContext,
    Json(req): Json<SpeechRequest>,
) -> Result<Response, OpenAIError> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

    if let Some(email) = crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
    {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let response_format = req
        .response_format
        .clone()
        .unwrap_or_else(|| "mp3".to_string());
    let audio = tts_passthrough::synthesize(&auth, &req)
        .await
        .map_err(|e| match e {
            tts_passthrough::TtsPassthroughError::NotConfigured => OpenAIError::not_implemented(
                "No TTS backend is configured on this executor. \
                 Register a TTS upstream via the AD4M config or wait for the local Kokoro backend.",
            ),
            tts_passthrough::TtsPassthroughError::Upstream(msg) => {
                OpenAIError::internal(format!("Upstream TTS error: {msg}"))
            }
        })?;

    if let Some(email) = crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
    {
        let amount = (req.input.len() as f64 / 1000.0).max(1.0);
        bill_compute(&email, amount, "ai_tts", Some("v1/audio/speech"))?;
    }

    let content_type = match response_format.as_str() {
        "mp3" => "audio/mpeg",
        "wav" => "audio/wav",
        "opus" => "audio/opus",
        "flac" => "audio/flac",
        "pcm" => "application/octet-stream",
        _ => "application/octet-stream",
    };
    Response::builder()
        .status(200)
        .header(header::CONTENT_TYPE, content_type)
        .body(audio.into())
        .map_err(|e| OpenAIError::internal(format!("Response build error: {e}")))
}

/// Decode an uploaded audio file to 16 kHz mono `f32` samples.
///
/// The first-cut implementation supports raw WAV (16-bit PCM, mono,
/// 16 kHz) — the simplest case that requires no external dependencies
/// and matches what the existing transcription WS feed already expects.
/// For other containers, return a descriptive 400 so the client can
/// transcode locally.
///
/// Full `symphonia` / `rubato` decode is a follow-up: it's the right
/// home for that work, but adding two heavy crates in the same PR as
/// the rest of the OpenAI surface would dominate the diff for a single
/// endpoint.
pub(super) fn audio_decode(bytes: &[u8], content_type: Option<&str>) -> OpenAIResult<Vec<f32>> {
    // Quick WAV header sniff — RIFF....WAVEfmt
    if bytes.len() >= 44 && &bytes[0..4] == b"RIFF" && &bytes[8..12] == b"WAVE" {
        return decode_pcm_wav(bytes);
    }
    Err(OpenAIError::invalid_request(format!(
        "Unsupported audio format (content-type: {}). \
         The current /v1/audio/transcriptions endpoint accepts 16 kHz mono PCM WAV. \
         Full container decode (mp3/m4a/flac/ogg) is on the roadmap; \
         transcode client-side as a workaround.",
        content_type.unwrap_or("(unknown)"),
    )))
}

/// Minimal WAV decoder: 16-bit PCM, mono, 16 kHz.
///
/// We don't attempt to handle every WAV variant — the goal is to clear
/// the most common upload path (whisper's reference format) without
/// pulling in `symphonia`.  Mismatched format → 400 with a clear hint.
pub(super) fn decode_pcm_wav(bytes: &[u8]) -> OpenAIResult<Vec<f32>> {
    // Find the fmt and data chunks.  WAV is a RIFF container: after
    // the 12-byte RIFF header, chunks alternate {id (4 bytes), size
    // (u32 LE), payload}.
    let mut pos = 12;
    let mut audio_format: u16 = 0;
    let mut channels: u16 = 0;
    let mut sample_rate: u32 = 0;
    let mut bits_per_sample: u16 = 0;
    let mut data_offset: Option<usize> = None;
    let mut data_size: usize = 0;

    while pos + 8 <= bytes.len() {
        let id = &bytes[pos..pos + 4];
        let size = u32::from_le_bytes([
            bytes[pos + 4],
            bytes[pos + 5],
            bytes[pos + 6],
            bytes[pos + 7],
        ]) as usize;
        pos += 8;
        let size = size.min(bytes.len().saturating_sub(pos));
        match id {
            b"fmt " => {
                if pos + 16 > bytes.len() {
                    return Err(OpenAIError::invalid_request("Truncated WAV fmt chunk"));
                }
                audio_format = u16::from_le_bytes([bytes[pos], bytes[pos + 1]]);
                channels = u16::from_le_bytes([bytes[pos + 2], bytes[pos + 3]]);
                sample_rate = u32::from_le_bytes([
                    bytes[pos + 4],
                    bytes[pos + 5],
                    bytes[pos + 6],
                    bytes[pos + 7],
                ]);
                bits_per_sample = u16::from_le_bytes([bytes[pos + 14], bytes[pos + 15]]);
                pos += size;
            }
            b"data" => {
                data_offset = Some(pos);
                data_size = size;
                pos += size;
            }
            _ => {
                // Skip unknown chunks (LIST, INFO, etc).
                pos += size;
            }
        }
        if pos % 2 != 0 {
            pos += 1; // RIFF chunks are word-aligned.
        }
    }

    let data_offset =
        data_offset.ok_or_else(|| OpenAIError::invalid_request("WAV file has no data chunk"))?;
    if audio_format != 1 {
        return Err(OpenAIError::invalid_request(format!(
            "Only uncompressed PCM WAV is supported (audio_format = {audio_format}). \
             Re-encode as PCM and retry."
        )));
    }
    if channels != 1 {
        return Err(OpenAIError::invalid_request(format!(
            "Only mono WAV is supported (channels = {channels}). Downmix to mono."
        )));
    }
    if sample_rate != 16_000 {
        return Err(OpenAIError::invalid_request(format!(
            "Only 16 kHz WAV is supported (sample_rate = {sample_rate}). Resample to 16 kHz."
        )));
    }
    if bits_per_sample != 16 {
        return Err(OpenAIError::invalid_request(format!(
            "Only 16-bit PCM WAV is supported (bits_per_sample = {bits_per_sample})."
        )));
    }

    let payload = &bytes[data_offset..data_offset + data_size.min(bytes.len() - data_offset)];
    let mut samples = Vec::with_capacity(payload.len() / 2);
    for chunk in payload.chunks_exact(2) {
        let s = i16::from_le_bytes([chunk[0], chunk[1]]);
        samples.push((s as f32) / (i16::MAX as f32));
    }
    Ok(samples)
}
