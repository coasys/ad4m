//! The binary audio side channel of `ai.inference` transcription streams:
//! raw PCM Float32 little-endian bytes over HTTP POST, which the JSON RPC
//! socket cannot carry. Streams open and close through `ai.inference`.

use axum::extract::State;
use axum::response::Json;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use base64::Engine;

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
    // `ai.inference.transcriptionFeed` checks the grant and the credits.
    let ctx = crate::services::ServiceHost::context_for_request(&auth.to_request_context());

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
    let failed: Vec<builtins::ai::FeedFailure> = builtins::call(
        Builtin::AiInference,
        "transcriptionFeed",
        serde_json::json!({
            "streamIds": stream_ids,
            "audio": base64::prelude::BASE64_STANDARD.encode(&body),
        }),
        &ctx,
    )
    .await
    .map_err(|e| match e.code {
        402 | 403 => ApiError::Forbidden(e.message),
        400 | 422 => ApiError::BadRequest(e.message),
        _ => ApiError::Internal(e.message),
    })?;
    for f in failed {
        log::warn!("Error feeding stream {}: {}", f.stream_id, f.error);
    }

    Ok(Json("true".to_string()))
}
