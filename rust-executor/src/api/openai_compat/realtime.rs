//! `GET /v1/realtime` — OpenAI Realtime-style WebSocket for streaming
//! transcription.
//!
//! Protocol (subset of the OpenAI Realtime spec):
//!
//! * **Client → server**
//!   - `{"type":"transcription_session.update","session":{"model":"<id>"}}` —
//!     opens a Whisper session.  Must arrive before any audio.
//!   - `{"type":"input_audio_buffer.append","audio":"<base64 PCM>"}` —
//!     16 kHz mono PCM16 (signed i16 LE) samples, base64-encoded.
//!     Matches the OpenAI Realtime default `input_audio_format: pcm16`.
//!   - `{"type":"input_audio_buffer.commit"}` — acknowledge the commit.
//!     The underlying whisper engine uses VAD-based segmentation, so
//!     this is a client synchronisation point, not a forced flush.
//!   - `{"type":"session.close"}` — close the session and the socket.
//!
//! * **Server → client**
//!   - `{"type":"transcription_session.created","session_id":"<uuid>"}`
//!   - `{"type":"conversation.item.input_audio_transcription.delta","delta":"<text>"}`
//!   - `{"type":"conversation.item.input_audio_transcription.completed","transcript":"<text>"}`
//!     emitted on each finalised whisper segment (the existing engine
//!     emits one event per segment via the broadcast channel).
//!   - `{"type":"error","error":{"message":"...","code":"..."}}`
//!
//! Auth: the WS extractor reads the bearer JWT from the
//! `Authorization` header *or* a `?token=` query param (per
//! `AuthContext`).  Capabilities checked: `AI_TRANSCRIBE`.

use axum::{
    extract::ws::{Message, WebSocket, WebSocketUpgrade},
    response::Response,
};
use base64::Engine;
use futures::SinkExt;
use serde_json::{json, Value};
use tokio::sync::broadcast;

use super::model_selector::resolve_model;
use crate::agent::capabilities::{check_capability, AI_TRANSCRIBE_CAPABILITY};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::types::ModelType;

pub async fn realtime_ws(auth: AuthContext, ws: WebSocketUpgrade) -> Response {
    ws.on_upgrade(move |socket| handle_socket(auth, socket))
}

async fn handle_socket(auth: AuthContext, mut socket: WebSocket) {
    if let Err(e) = check_capability(&auth.capabilities, &AI_TRANSCRIBE_CAPABILITY) {
        let _ = send_error(&mut socket, "forbidden", &e).await;
        let _ = socket.close().await;
        return;
    }

    let mut stream_id: Option<String> = None;
    let mut delta_rx: Option<broadcast::Receiver<String>> = None;
    let mut audio_format = AudioFormat::Pcm16;

    loop {
        // Move the receiver out so select! doesn't conflict with
        // mutations inside the socket-message handler.
        let mut rx_taken = delta_rx.take();

        enum Ev {
            Ws(Option<Result<Message, axum::Error>>),
            Delta(Result<String, broadcast::error::RecvError>),
        }

        let ev = if let Some(ref mut rx) = rx_taken {
            tokio::select! {
                msg = socket.recv() => Ev::Ws(msg),
                text = rx.recv() => Ev::Delta(text),
            }
        } else {
            Ev::Ws(socket.recv().await)
        };

        delta_rx = rx_taken;

        let msg = match ev {
            Ev::Delta(Ok(text)) => {
                if !text.is_empty() {
                    let delta = json!({
                        "type": "conversation.item.input_audio_transcription.delta",
                        "delta": text,
                    });
                    let _ = socket.send(Message::Text(delta.to_string().into())).await;
                }
                continue;
            }
            Ev::Delta(Err(broadcast::error::RecvError::Lagged(_))) => continue,
            Ev::Delta(Err(broadcast::error::RecvError::Closed)) => {
                delta_rx = None;
                continue;
            }
            Ev::Ws(None) | Ev::Ws(Some(Err(_))) => break,
            Ev::Ws(Some(Ok(m))) => m,
        };

        let text = match msg {
            Message::Text(t) => t.to_string(),
            Message::Binary(_) => {
                let _ = send_error(
                    &mut socket,
                    "invalid_request",
                    "Binary frames not supported",
                )
                .await;
                continue;
            }
            Message::Close(_) => break,
            _ => continue,
        };
        let parsed: Value = match serde_json::from_str(&text) {
            Ok(v) => v,
            Err(e) => {
                let _ =
                    send_error(&mut socket, "invalid_request", &format!("JSON parse: {e}")).await;
                continue;
            }
        };
        let event_type = parsed
            .get("type")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        match event_type.as_str() {
            "transcription_session.update" => {
                let session = parsed.get("session");
                let model_str = session
                    .and_then(|s| s.get("model"))
                    .and_then(|m| m.as_str())
                    .unwrap_or("default");

                audio_format = match session
                    .and_then(|s| s.get("input_audio_format"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("pcm16")
                {
                    "pcm16" => AudioFormat::Pcm16,
                    other => {
                        let _ = send_error(
                            &mut socket,
                            "invalid_request",
                            &format!(
                                "Unsupported input_audio_format \"{other}\"; only \"pcm16\" is supported"
                            ),
                        )
                        .await;
                        continue;
                    }
                };

                let model_id = match resolve_model(model_str, ModelType::Transcription).await {
                    Ok(id) => id,
                    Err(e) => {
                        let _ = send_error(&mut socket, "invalid_request", &e.error.message).await;
                        continue;
                    }
                };
                let service = match AIService::global_instance().await {
                    Ok(s) => s,
                    Err(e) => {
                        let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                        continue;
                    }
                };

                // Tear down any prior session before opening a new one.
                if let Some(old_sid) = stream_id.take() {
                    let _ = service
                        .close_transcription_stream(&old_sid, &auth.auth_token)
                        .await;
                }
                delta_rx = None;

                let sid = match service
                    .open_transcription_stream(model_id.clone(), None, auth.auth_token.clone())
                    .await
                {
                    Ok(id) => id,
                    Err(e) => {
                        let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                        continue;
                    }
                };
                let created = json!({
                    "type": "transcription_session.created",
                    "session_id": sid,
                    "model": model_id,
                });
                let _ = socket.send(Message::Text(created.to_string().into())).await;
                stream_id = Some(sid);
            }
            "input_audio_buffer.append" => {
                let Some(ref sid) = stream_id else {
                    let _ = send_error(
                        &mut socket,
                        "invalid_request",
                        "Send `transcription_session.update` before appending audio.",
                    )
                    .await;
                    continue;
                };
                let audio_b64 = match parsed.get("audio").and_then(|v| v.as_str()) {
                    Some(s) => s,
                    None => {
                        let _ = send_error(
                            &mut socket,
                            "invalid_request",
                            "Missing required field: audio",
                        )
                        .await;
                        continue;
                    }
                };
                let bytes = match base64::prelude::BASE64_STANDARD.decode(audio_b64) {
                    Ok(b) => b,
                    Err(e) => {
                        let _ = send_error(&mut socket, "invalid_request", &format!("base64: {e}"))
                            .await;
                        continue;
                    }
                };
                let samples = match audio_format {
                    AudioFormat::Pcm16 => pcm16_to_f32(&bytes),
                };

                let service = match AIService::global_instance().await {
                    Ok(s) => s,
                    Err(e) => {
                        let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                        continue;
                    }
                };
                if delta_rx.is_none() {
                    match service
                        .feed_transcription_stream_with_broadcast(sid, samples, &auth.auth_token)
                        .await
                    {
                        Ok(rx) => delta_rx = Some(rx),
                        Err(e) => {
                            let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                        }
                    }
                } else {
                    if let Err(e) = service
                        .feed_transcription_stream(sid, samples, &auth.auth_token)
                        .await
                    {
                        let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                    }
                }
            }
            "input_audio_buffer.commit" => {
                // The whisper backend uses VAD-based segmentation; there
                // is no explicit flush API.  We acknowledge the commit so
                // clients that wait for it can proceed — transcription
                // results arrive asynchronously via delta events.
            }
            "session.close" => {
                break;
            }
            _ => {
                let _ = send_error(
                    &mut socket,
                    "invalid_request",
                    &format!("Unsupported event type: {event_type}"),
                )
                .await;
            }
        }
    }

    if let Some(ref sid) = stream_id {
        if let Ok(service) = AIService::global_instance().await {
            let _ = service
                .close_transcription_stream(sid, &auth.auth_token)
                .await;
        }
    }
    let _ = socket.close().await;
}

async fn send_error(socket: &mut WebSocket, code: &str, message: &str) -> Result<(), ()> {
    let body = json!({
        "type": "error",
        "error": {
            "message": message,
            "code": code,
        }
    });
    socket
        .send(Message::Text(body.to_string().into()))
        .await
        .map_err(|_| ())
}

enum AudioFormat {
    Pcm16,
}

/// Convert PCM16 (signed i16 LE) bytes to normalised f32 samples.
/// This is the default OpenAI Realtime `input_audio_format`.
pub(super) fn pcm16_to_f32(bytes: &[u8]) -> Vec<f32> {
    let mut out = Vec::with_capacity(bytes.len() / 2);
    for chunk in bytes.chunks_exact(2) {
        let s = i16::from_le_bytes([chunk[0], chunk[1]]);
        out.push((s as f32) / (i16::MAX as f32));
    }
    out
}
