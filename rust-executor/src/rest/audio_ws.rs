//! WebSocket binary audio transport for transcription streams.
//!
//! Accepts raw PCM f32 little-endian bytes, eliminating JSON serialisation
//! and HTTP overhead for real-time audio feeding.

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Query, State,
    },
    response::IntoResponse,
};
use serde::Deserialize;

use crate::agent::capabilities::*;
use crate::ai_service::AIService;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;

#[derive(Deserialize)]
pub struct AudioWsParams {
    pub stream_ids: String, // comma-separated stream IDs
                            // token is also in query but handled by AuthContext extractor
}

/// GET /ws/audio — WebSocket endpoint for binary audio transport.
///
/// Authentication is via `token` query parameter (same as SSE endpoints).
/// Stream IDs are comma-separated in `stream_ids`.
pub async fn audio_websocket(
    ws: WebSocketUpgrade,
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<AudioWsParams>,
) -> Result<impl IntoResponse, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let auth_token = context.auth_token;
    let stream_ids: Vec<String> = params
        .stream_ids
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    Ok(ws.on_upgrade(move |socket| handle_audio_ws(socket, auth_token, stream_ids)))
}

async fn handle_audio_ws(mut socket: WebSocket, auth_token: String, stream_ids: Vec<String>) {
    let service = match AIService::global_instance().await {
        Ok(s) => s,
        Err(e) => {
            log::error!("Failed to get AI service for audio WS: {}", e);
            return;
        }
    };

    log::info!("Audio WebSocket connected for streams: {:?}", stream_ids);

    while let Some(msg) = socket.recv().await {
        match msg {
            Ok(Message::Binary(bytes)) => {
                // Client sends raw Float32Array.buffer bytes (little-endian f32)
                if bytes.len() % 4 != 0 {
                    log::warn!(
                        "Audio WS received non-aligned binary frame: {} bytes",
                        bytes.len()
                    );
                    continue;
                }

                let samples: Vec<f32> = bytes
                    .chunks_exact(4)
                    .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                    .collect();

                for stream_id in &stream_ids {
                    if let Err(e) = service
                        .feed_transcription_stream(stream_id, samples.clone(), &auth_token)
                        .await
                    {
                        log::warn!("Error feeding audio WS stream {}: {}", stream_id, e);
                    }
                }
            }
            Ok(Message::Close(_)) => {
                log::info!("Audio WebSocket closed for streams: {:?}", stream_ids);
                break;
            }
            Ok(_) => {} // ignore text, ping, pong
            Err(e) => {
                log::error!("Audio WebSocket error: {}", e);
                break;
            }
        }
    }
}
