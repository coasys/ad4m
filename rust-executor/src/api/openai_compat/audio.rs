//! `POST /v1/audio/transcriptions` (batch STT) + `POST /v1/audio/speech` (TTS).
//!
//! ## Transcription
//!
//! A passthrough to the model's endpoint: the OpenAI multipart upload (`file`, `model`,
//! `language`, `prompt`, `response_format`, `temperature`, ...) goes on as it came, with
//! `model` replaced by the id the upstream knows, and the upstream's answer comes back as
//! it was sent. Any container the upstream decodes is accepted.
//!
//! ## TTS
//!
//! `audio/speech` resolves a `SPEECH` model and forwards the request to its
//! endpoint, which must have the speech capability, with that endpoint's key
//! only ([`super::passthrough`]).

use axum::{
    extract::Multipart, http::header, http::StatusCode, response::IntoResponse, response::Response,
};

use super::billing_amounts;
use super::errors::{OpenAIError, OpenAIJson};
use super::model_selector::{resolve, resolve_model};
use super::passthrough::{self, Upstream};
use super::types::SpeechRequest;
use crate::agent::capabilities::{AI_PROMPT_CAPABILITY, AI_TRANSCRIBE_CAPABILITY};
use crate::ai_service::providers::audio::{AudioFile, RemoteTranscribe, TranscribeFailure};
use crate::ai_service::transcription;
use crate::billing::{bill_compute, check_compute_credits};
use crate::types::{AiCapability, ModelType, RequestContext};

pub async fn transcriptions(
    auth: RequestContext,
    mut multipart: Multipart,
) -> Result<Response, OpenAIError> {
    auth.require(&AI_TRANSCRIBE_CAPABILITY)
        .map_err(OpenAIError::forbidden)?;

    let mut model_field: Option<String> = None;
    let mut file: Option<AudioFile> = None;
    let mut fields: Vec<(String, String)> = Vec::new();
    let multipart_error = |e: axum::extract::multipart::MultipartError| {
        OpenAIError::invalid_request(format!("Multipart parse error: {e}"))
    };
    while let Some(field) = multipart.next_field().await.map_err(multipart_error)? {
        let name = field.name().unwrap_or("").to_string();
        match name.as_str() {
            "model" => {
                model_field = Some(
                    field
                        .text()
                        .await
                        .map_err(multipart_error)?
                        .trim()
                        .to_string(),
                )
                .filter(|s| !s.is_empty());
            }
            "file" => {
                let file_name = field.file_name().unwrap_or("audio").to_string();
                let mime = field
                    .content_type()
                    .unwrap_or("application/octet-stream")
                    .to_string();
                let bytes = field
                    .bytes()
                    .await
                    .map_err(|e| OpenAIError::invalid_request(format!("File read error: {e}")))?;
                file = Some(AudioFile {
                    bytes: bytes.to_vec(),
                    file_name,
                    mime,
                });
            }
            _ => fields.push((name, field.text().await.map_err(multipart_error)?)),
        }
    }

    let model = model_field
        .ok_or_else(|| OpenAIError::invalid_request("Missing required form field: model"))?;
    let file =
        file.ok_or_else(|| OpenAIError::invalid_request("Missing required form field: file"))?;
    let model_id = resolve_model(&model, ModelType::Transcription).await?;
    // The model resolved: what is left to refuse is an endpoint that cannot transcribe.
    let (client, remote_model) = transcription::endpoint_transcriber(&model_id)
        .map_err(|e| OpenAIError::invalid_request(e.to_string()))?;

    if let Some(agent_did) = auth.user_did() {
        check_compute_credits(agent_did)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let started = std::time::Instant::now();
    let reply = client
        .transcribe(&remote_model, file, fields)
        .await
        .map_err(|e| {
            log::warn!("v1/audio/transcriptions model={model_id} ({remote_model}) failed: {e:#}");
            upstream_error(&e)
        })?;
    let words = reply.text.split_whitespace().count();
    log::info!(
        "v1/audio/transcriptions model={model_id} ({remote_model}) latency_ms={} words={words}",
        started.elapsed().as_millis()
    );
    transcription::bill_transcription(auth.user_did(), &model_id, words);

    Ok(([(header::CONTENT_TYPE, reply.content_type)], reply.body).into_response())
}

/// What the client sees of a failed upstream call (plan §5.4): the upstream's refusal with
/// its status, else a gateway error. Never the endpoint's URL or key.
fn upstream_error(error: &anyhow::Error) -> OpenAIError {
    let failure = TranscribeFailure::of(error);
    let (status, code) = match &failure {
        TranscribeFailure::Refused { status, .. } => {
            let status = StatusCode::from_u16(status.as_u16()).unwrap_or(StatusCode::BAD_REQUEST);
            return OpenAIError::new(status, failure.to_string(), "invalid_request_error", None);
        }
        TranscribeFailure::Unauthorized => (StatusCode::BAD_GATEWAY, "upstream_auth"),
        TranscribeFailure::Failed(_) => (StatusCode::BAD_GATEWAY, "upstream_error"),
        TranscribeFailure::Timeout => (StatusCode::GATEWAY_TIMEOUT, "upstream_timeout"),
        TranscribeFailure::Unreachable => (StatusCode::SERVICE_UNAVAILABLE, "upstream_unavailable"),
    };
    OpenAIError::new(status, failure.to_string(), code, Some(code))
}

pub async fn speech(
    auth: RequestContext,
    OpenAIJson(body): OpenAIJson<serde_json::Value>,
) -> Result<Response, OpenAIError> {
    auth.require(&AI_PROMPT_CAPABILITY)
        .map_err(OpenAIError::forbidden)?;

    let req: SpeechRequest = serde_json::from_value(body.clone())
        .map_err(|e| OpenAIError::invalid_request(e.to_string()))?;
    let resolved = resolve(&req.model, ModelType::Speech).await?;
    let upstream = Upstream::of(&resolved)
        .filter(|upstream| {
            upstream
                .endpoint
                .capabilities
                .contains(&AiCapability::Speech)
        })
        .ok_or_else(|| {
            OpenAIError::model_not_supported(format!(
                "Model '{}' is not on an endpoint that serves speech",
                req.model
            ))
        })?;

    if let Some(agent_did) = auth.user_did() {
        check_compute_credits(agent_did)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
        let amount = billing_amounts::speech_amount(req.input.chars().count());
        bill_compute(agent_did, amount, "ai_tts", Some("v1/audio/speech"))?;
    }

    passthrough::forward_speech(upstream, body).await
}
