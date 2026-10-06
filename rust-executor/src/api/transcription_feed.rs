//! The binary audio side channel of `ai.inference` transcription streams:
//! raw PCM Float32 little-endian bytes over HTTP POST, which the JSON RPC
//! socket cannot carry. Streams open and close through `ai.inference`.

use axum::extract::State;
use axum::response::Json;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use crate::agent::capabilities::check_capability;
use crate::ai_service::AIService;
use crate::services::builtins::{self, Builtin};

/// POST /ai/transcription/feed
///
/// Accepts raw PCM Float32 little-endian bytes (application/octet-stream).
/// Stream IDs are passed via `X-Stream-Ids` header (comma-separated).
pub async fn feed_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    headers: axum::http::HeaderMap,
    body: axum::body::Bytes,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &builtins::capability(Builtin::AiInference, "TRANSCRIBE"),
    )
    .map_err(ApiError::Forbidden)?;
    // Same credit check the service host applies to metered methods.
    let ctx = crate::services::ServiceHost::context_for_request(&context);
    if !builtins::billing::may_spend(&ctx) {
        return Err(ApiError::Forbidden("Insufficient compute credits".into()));
    }

    let stream_ids_header = headers
        .get("x-stream-ids")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let stream_ids: Vec<String> = stream_ids_header
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if stream_ids.is_empty() {
        return Err(ApiError::BadRequest(
            "X-Stream-Ids header is required".into(),
        ));
    }

    if stream_ids.len() > 32 {
        return Err(ApiError::BadRequest("Too many stream IDs (max 32)".into()));
    }

    // Limit audio buffer to 10 MB (2.5M float32 samples)
    const MAX_AUDIO_BYTES: usize = 10 * 1024 * 1024;
    if body.len() > MAX_AUDIO_BYTES {
        return Err(ApiError::BadRequest(format!(
            "Audio buffer too large ({} bytes, max {})",
            body.len(),
            MAX_AUDIO_BYTES
        )));
    }

    if body.len() % 4 != 0 {
        return Err(ApiError::BadRequest(
            "Body length must be a multiple of 4 (Float32 samples)".into(),
        ));
    }
    let audio_f32: Vec<f32> = body
        .chunks_exact(4)
        .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
        .collect();

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let mut errors: Vec<String> = Vec::new();
    for stream_id in &stream_ids {
        if let Err(e) = service
            .feed_transcription_stream(stream_id, audio_f32.clone(), &context.auth_token)
            .await
        {
            log::warn!("Error feeding stream {}: {}", stream_id, e);
            errors.push(format!("{}: {}", stream_id, e));
        }
    }

    feed_outcome(&errors, stream_ids.len())?;
    Ok(Json("true".to_string()))
}

/// Whether a feed to `stream_count` streams succeeded, given the streams that failed.
///
/// Any failure fails the request. When only some streams failed, the audio has already reached
/// the others, so the message says so and names the failures: a caller retrying the whole feed
/// would give the streams that took it the same audio twice.
pub(crate) fn feed_outcome(errors: &[String], stream_count: usize) -> Result<(), ApiError> {
    if errors.is_empty() {
        return Ok(());
    }
    if errors.len() == stream_count {
        return Err(ApiError::Internal(format!(
            "All streams failed: {}",
            errors.join("; ")
        )));
    }
    Err(ApiError::Internal(format!(
        "{} of {} streams failed; the others were fed: {}",
        errors.len(),
        stream_count,
        errors.join("; ")
    )))
}
