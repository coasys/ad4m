//! WebSocket RPC endpoint: GET /api/v1/ws
//!
//! Single WebSocket connection per client. Auth happens once on connection
//! upgrade (token in query param). All SDK operations are dispatched directly
//! to handler functions — no HTTP proxy layer.
//!
//! **Request:**  `{ "id": "<correlation-id>", "type": "<operation>", "params": {...} }`
//! **Response:** `{ "id": "<correlation-id>", "result": ... }` or
//!               `{ "id": "<correlation-id>", "error": { "code": N, "message": "..." } }`
//! **Events:**   `{ "type": "<event-type>", ...payload }` (no `id`)

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Query, State,
    },
    response::IntoResponse,
};
use futures::stream::StreamExt;
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::Arc;
use tokio::sync::mpsc;

use crate::agent::capabilities::*;
use crate::types::RequestContext;

use super::auth::AppState;
use super::ws_handler::HandlerMap;

// ── Auth query param ────────────────────────────────────────────────────────
#[derive(Deserialize, Default)]
pub struct WsAuthQuery {
    token: Option<String>,
}

// ── Entry point ─────────────────────────────────────────────────────────────

/// Axum handler for the `/api/v1/ws` upgrade.
///
/// The `HandlerMap` is built once at server startup and shared via `Arc`.
pub async fn ws_rpc(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
    Query(query): Query<WsAuthQuery>,
    axum::extract::Extension(handler_map): axum::extract::Extension<Arc<HandlerMap>>,
) -> impl IntoResponse {
    let token = query.token.unwrap_or_default();

    // Build RequestContext once for the lifetime of this connection.
    let capabilities = capabilities_from_token(token.clone(), state.admin_credential.clone());
    let is_admin = is_admin_credential_token(&token, &state.admin_credential);

    let ctx = Arc::new(RequestContext {
        capabilities,
        auto_permit_cap_requests: state.auto_permit_cap_requests,
        auth_token: token.clone(),
        is_admin_credential: is_admin,
    });

    ws.on_upgrade(move |socket| handle_ws(socket, handler_map, ctx, token))
}

// ── Connection handler ──────────────────────────────────────────────────────

async fn handle_ws(
    socket: WebSocket,
    handler_map: Arc<HandlerMap>,
    ctx: Arc<RequestContext>,
    token: String,
) {
    // Track last_seen once at connect time
    track_last_seen_from_token(token.clone()).await;

    let (mut ws_sink, mut ws_stream) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();

    // ── Event broadcast ─────────────────────────────────────────────────
    let token_for_events = token.clone();
    let tx_events = tx.clone();
    tokio::spawn(async move {
        let user_email = user_email_from_token(token_for_events.clone());
        let event_stream = super::events_ws::build_event_stream(token_for_events, user_email).await;
        tokio::pin!(event_stream);
        while let Some(msg) = event_stream.next().await {
            if tx_events.send(msg).is_err() {
                break;
            }
        }
    });

    // ── Writer task ─────────────────────────────────────────────────────
    let write_handle = tokio::spawn(async move {
        use futures::SinkExt;
        while let Some(msg) = rx.recv().await {
            if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                break;
            }
        }
    });

    // ── Reader loop — direct dispatch ───────────────────────────────────
    while let Some(Ok(msg)) = ws_stream.next().await {
        let text = match &msg {
            Message::Text(t) => t.to_string(),
            Message::Close(_) => break,
            Message::Ping(_) | Message::Pong(_) => continue,
            _ => continue,
        };

        // Parse JSON
        let parsed: Value = match serde_json::from_str(&text) {
            Ok(v) => v,
            Err(_) => {
                let _ = tx.send(json!({"error":{"code":400,"message":"Invalid JSON"}}).to_string());
                continue;
            }
        };

        // Handle ping/pong keepalive
        if parsed.get("type").and_then(|v| v.as_str()) == Some("ping") {
            let _ = tx.send(json!({"type":"pong"}).to_string());
            continue;
        }

        // Extract id and type
        let id = parsed
            .get("id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let msg_type = match parsed.get("type").and_then(|v| v.as_str()) {
            Some(t) => t.to_string(),
            None => {
                let _ = tx.send(
                    json!({"id": id, "error":{"code":400,"message":"Missing 'type' field"}})
                        .to_string(),
                );
                continue;
            }
        };

        let params = parsed.get("params").cloned().unwrap_or(json!({}));
        let handler_map = handler_map.clone();
        let ctx = ctx.clone();
        let tx_clone = tx.clone();

        tokio::spawn(async move {
            let result = handler_map.dispatch(&msg_type, params, ctx).await;
            let response = match result {
                Ok(val) => json!({"id": id, "result": val}),
                Err(e) => json!({"id": id, "error": {"code": e.code, "message": e.message}}),
            };
            let _ = tx_clone.send(response.to_string());
        });
    }

    drop(tx);
    let _ = write_handle.await;
}
