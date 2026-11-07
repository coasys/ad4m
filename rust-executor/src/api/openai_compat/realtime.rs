//! `GET /v1/realtime` — OpenAI Realtime-style WebSocket for streaming
//! transcription.
//!
//! Protocol (subset of the OpenAI Realtime spec):
//!
//! * **Client → server**
//!   - `{"type":"transcription_session.update","session":{"model":"<id>"}}` —
//!     opens a transcription stream.  Must arrive before any audio.
//!   - `{"type":"input_audio_buffer.append","audio":"<base64 PCM>"}` —
//!     16 kHz mono PCM16 (signed i16 LE) samples, base64-encoded, added to the buffer.
//!     Matches the OpenAI Realtime default `input_audio_format: pcm16`.
//!   - `{"type":"input_audio_buffer.commit"}` — the buffer is one utterance: it is
//!     transcribed as one call (one upstream request on an endpoint) and emptied. The
//!     client decides where an utterance ends; the executor's speech gate drops one that
//!     is not speech. Each commit needs compute credits, as the session's opening does.
//!     At most `MAX_PENDING_COMMITS` (8) commits wait for their transcript; one more gets
//!     `rate_limit_exceeded`. A refused commit keeps the buffer, so a later commit sends it.
//!   - `{"type":"session.close"}` — close the session and the socket, once the commits sent
//!     before it are answered and their text is sent (at most the timeout of one call to
//!     the model's endpoint: its `request_timeout_secs`, else 120 s).
//!
//! * **Server → client**
//!   - `{"type":"transcription_session.created","session_id":"<uuid>"}`
//!   - `{"type":"conversation.item.input_audio_transcription.delta","delta":"<text>"}`
//!     for each text the stream publishes (one per committed utterance).
//!   - `{"type":"error","error":{"message":"...","code":"..."}}`; `insufficient_quota`
//!     when the caller's user has no compute credits left.
//!
//! Auth: the WS extractor reads the bearer JWT from the
//! `Authorization` header *or* a `?token=` query param (per
//! `RequestContext`).  Capabilities checked: `AI_TRANSCRIBE`.

use axum::{
    extract::ws::{Message, WebSocket, WebSocketUpgrade},
    response::{IntoResponse, Response},
};
use base64::Engine;
use futures::{Sink, SinkExt, Stream, StreamExt};
use serde_json::{json, Value};
use tokio::sync::broadcast;

use super::model_selector::resolve_model;
use crate::agent::capabilities::AI_TRANSCRIBE_CAPABILITY;
use crate::agent::session::{AuthError, Session};
use crate::ai_service::{AIService, FeedAnswer};
use crate::api::errors::ApiError;
use crate::types::{ModelType, RequestContext};

/// The most audio one utterance may buffer: 10 MB of `f32`, as the HTTP feed accepts.
const MAX_BUFFERED_SAMPLES: usize = 10 * 1024 * 1024 / 4;

/// The most commits a session may have waiting for their answer: each holds up to
/// [`MAX_BUFFERED_SAMPLES`] of audio until it is transcribed.
pub(super) const MAX_PENDING_COMMITS: usize = 8;

/// A token that opens no session gets the upgrade, an `unauthorized` error event and a close
/// with 1008, before any work: a browser cannot read the HTTP status of a refused upgrade.
pub async fn realtime_ws(
    caller: Result<RequestContext, ApiError>,
    ws: WebSocketUpgrade,
) -> Response {
    match caller {
        Ok(caller) => ws.on_upgrade(move |socket| handle_socket(caller, socket)),
        Err(ApiError::Unauthorized(reason)) => ws.on_upgrade(move |mut socket| async move {
            let _ = send_error(&mut socket, "unauthorized", &reason).await;
            let code = crate::api::ws_rpc::CLOSE_TOKEN_ENDED;
            crate::api::ws_rpc::close_refused(socket, code, reason).await;
        }),
        Err(ApiError::Unavailable(reason)) => ws.on_upgrade(move |mut socket| async move {
            let _ = send_error(&mut socket, "unavailable", &reason).await;
            let code = crate::api::ws_rpc::CLOSE_TRY_AGAIN_LATER;
            crate::api::ws_rpc::close_refused(socket, code, reason).await;
        }),
        Err(e) => e.into_response(),
    }
}

async fn handle_socket(caller: RequestContext, socket: WebSocket) {
    let (sink, incoming) = socket.split();
    serve(caller, sink, incoming).await;
}

/// One `/v1/realtime` session: the client's events from `incoming`, the server's to `socket`.
pub(super) async fn serve<Si, St>(caller: RequestContext, mut socket: Si, mut incoming: St)
where
    Si: Sink<Message> + Unpin,
    St: Stream<Item = Result<Message, axum::Error>> + Unpin,
{
    if let Err(e) = caller.require(&AI_TRANSCRIBE_CAPABILITY) {
        let _ = send_error(&mut socket, "forbidden", &e).await;
        let _ = socket.close().await;
        return;
    }
    let session = caller.session.clone();
    // Completes when the grant ends or the token expires: the socket closes at once, not at
    // the next message.
    let over = session.over();
    tokio::pin!(over);
    // Set when the token stopped working: the socket then closes with 1008.
    let mut token_ended = false;

    let mut stream_id: Option<String> = None;
    // The model of the open stream: its call timeout bounds the wait of a close.
    let mut stream_model: Option<String> = None;
    let mut delta_rx: Option<broadcast::Receiver<String>> = None;
    let mut audio_format = AudioFormat::Pcm16;
    // The utterance being appended, sent on commit.
    let mut buffered: Vec<f32> = Vec::new();
    // The answers of committed utterances, awaited beside the deltas: the deltas of an
    // utterance arrive while it is transcribed.
    let mut answers = futures::stream::FuturesUnordered::<FeedAnswer>::new();
    // Set by `session.close`: when the wait for the commits queued before it ends.
    let mut closing: Option<tokio::time::Instant> = None;

    loop {
        if closing.is_some() && answers.is_empty() {
            // Every queued commit is answered, and its text waits in the receiver.
            if let Some(rx) = delta_rx.as_mut() {
                while let Ok(text) = rx.try_recv() {
                    match delta_frame(&session, &text) {
                        Ok(Some(frame)) => {
                            let _ = socket.send(Message::Text(frame.into())).await;
                        }
                        Ok(None) => {}
                        Err(_) => {
                            token_ended = true;
                            break;
                        }
                    }
                }
            }
            break;
        }

        // Move the receiver out so select! doesn't conflict with
        // mutations inside the socket-message handler.
        let mut rx_taken = delta_rx.take();

        enum Ev {
            Over,
            Ws(Option<Result<Message, axum::Error>>),
            Delta(Result<String, broadcast::error::RecvError>),
            Answered(crate::ai_service::Result<()>),
            CloseWaited,
        }

        let ev = tokio::select! {
            _ = &mut over => Ev::Over,
            msg = incoming.next(), if closing.is_none() => Ev::Ws(msg),
            _ = async {
                match closing {
                    Some(at) => tokio::time::sleep_until(at).await,
                    None => std::future::pending().await,
                }
            } => Ev::CloseWaited,
            text = async {
                match rx_taken.as_mut() {
                    Some(rx) => rx.recv().await,
                    None => std::future::pending().await,
                }
            } => Ev::Delta(text),
            Some(answered) = answers.next(), if !answers.is_empty() => Ev::Answered(answered),
        };

        delta_rx = rx_taken;

        let msg = match ev {
            Ev::Over => {
                token_ended = true;
                break;
            }
            Ev::CloseWaited => break,
            Ev::Delta(Ok(text)) => {
                match delta_frame(&session, &text) {
                    Ok(Some(frame)) => {
                        let _ = socket.send(Message::Text(frame.into())).await;
                    }
                    Ok(None) => {}
                    Err(e) => {
                        let _ = send_error(&mut socket, "unauthorized", &e.to_string()).await;
                        token_ended = true;
                        break;
                    }
                }
                continue;
            }
            Ev::Delta(Err(broadcast::error::RecvError::Lagged(lost))) => {
                let message = format!("{lost} transcript pieces were lost: the socket was busy");
                log::warn!("/v1/realtime: {message}");
                let _ = send_error(&mut socket, "server_error", &message).await;
                continue;
            }
            Ev::Answered(Ok(())) => continue,
            Ev::Answered(Err(e)) => {
                let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                continue;
            }
            Ev::Delta(Err(broadcast::error::RecvError::Closed)) => {
                delta_rx = None;
                continue;
            }
            Ev::Ws(None) | Ev::Ws(Some(Err(_))) => break,
            Ev::Ws(Some(Ok(m))) => m,
        };

        // Check the token again on every client message: expiry and revokeToken() end the
        // session, as they do on the RPC socket.
        if let Err(e) = session.check() {
            let _ = send_error(&mut socket, "unauthorized", &e.to_string()).await;
            token_ended = true;
            break;
        }

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

                if let Err(refused) = credits_left(&caller) {
                    let _ = send_error(&mut socket, "insufficient_quota", &refused).await;
                    continue;
                }

                // Tear down any prior session before opening a new one.
                if let Some(old_sid) = stream_id.take() {
                    let _ = service.close_transcription_stream(&old_sid, &caller).await;
                }
                delta_rx = None;
                buffered.clear();

                let opened = match service
                    .open_transcription_stream(model_id.clone(), &caller)
                    .await
                {
                    Ok(sid) => service
                        .subscribe_transcription_text(&sid, &caller)
                        .await
                        .map(|rx| (sid, rx)),
                    Err(e) => Err(e),
                };
                let sid = match opened {
                    Ok((sid, rx)) => {
                        delta_rx = Some(rx);
                        sid
                    }
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
                stream_model = Some(model_id);
            }
            "input_audio_buffer.append" => {
                if stream_id.is_none() {
                    let _ = send_error(
                        &mut socket,
                        "invalid_request",
                        "Send `transcription_session.update` before appending audio.",
                    )
                    .await;
                    continue;
                }
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
                if buffered.len() + samples.len() > MAX_BUFFERED_SAMPLES {
                    let _ = send_error(
                        &mut socket,
                        "invalid_request",
                        "input_audio_buffer is full: commit the utterance first",
                    )
                    .await;
                    continue;
                }
                buffered.extend(samples);
            }
            "input_audio_buffer.commit" => {
                let Some(ref sid) = stream_id else {
                    let _ = send_error(
                        &mut socket,
                        "invalid_request",
                        "Send `transcription_session.update` before committing audio.",
                    )
                    .await;
                    continue;
                };
                if buffered.is_empty() {
                    let _ = send_error(
                        &mut socket,
                        "invalid_request",
                        "input_audio_buffer is empty",
                    )
                    .await;
                    continue;
                }
                // Both refusals keep the buffer: a commit after them sends it.
                if let Err(refused) = credits_left(&caller) {
                    let _ = send_error(&mut socket, "insufficient_quota", &refused).await;
                    continue;
                }
                if answers.len() >= MAX_PENDING_COMMITS {
                    let _ = send_error(
                        &mut socket,
                        "rate_limit_exceeded",
                        &format!(
                            "{MAX_PENDING_COMMITS} commits wait for their transcript: commit again once one is answered"
                        ),
                    )
                    .await;
                    continue;
                }
                let utterance = std::mem::take(&mut buffered);
                // Queued here, so commits keep their order; answered in the loop, so the
                // deltas of a long utterance are read while it is transcribed.
                let queued = match AIService::global_instance().await {
                    Ok(service) => service.queue_utterance(sid, utterance, &caller).await,
                    Err(e) => Err(e),
                };
                match queued {
                    Ok(answer) => answers.push(answer),
                    Err(e) => {
                        let _ = send_error(&mut socket, "server_error", &e.to_string()).await;
                    }
                }
            }
            // The text of the commits queued before the close still goes out, and they are
            // billed: wait for their answers, at most as long as one upstream call may take.
            "session.close" => {
                let wait = stream_model
                    .as_deref()
                    .map(crate::ai_service::transcription::call_timeout)
                    .unwrap_or(crate::ai_service::providers::audio::REQUEST_TIMEOUT);
                closing = Some(tokio::time::Instant::now() + wait);
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
            let _ = service.close_transcription_stream(sid, &caller).await;
        }
    }
    if token_ended {
        crate::api::ws_rpc::close_token_ended(&mut socket).await;
    } else {
        let _ = socket.close().await;
    }
}

/// Whether the caller may spend compute: `Err` with the reason when its user has no credits
/// left. A caller without a user (the operator, an app on the main agent) pays nothing.
fn credits_left(caller: &RequestContext) -> Result<(), String> {
    match caller.user_did() {
        Some(did) => crate::billing::check_compute_credits(did)
            .map_err(|_| "Insufficient compute credits".to_string()),
        None => Ok(()),
    }
}

/// The frame that forwards the transcription delta `text` (`None` for an empty one), or the
/// error of a session whose token stopped holding: a transcript goes out only while it holds.
pub(super) fn delta_frame(session: &Session, text: &str) -> Result<Option<String>, AuthError> {
    session.check()?;
    if text.is_empty() {
        return Ok(None);
    }
    let delta = json!({
        "type": "conversation.item.input_audio_transcription.delta",
        "delta": text,
    });
    Ok(Some(delta.to_string()))
}

async fn send_error<S: Sink<Message> + Unpin>(
    socket: &mut S,
    code: &str,
    message: &str,
) -> Result<(), ()> {
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
