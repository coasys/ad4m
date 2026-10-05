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
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use tokio_util::sync::CancellationToken;

use crate::agent::capabilities::*;
use crate::agent::AgentService;
use crate::types::RequestContext;

use super::auth::AppState;
use super::ws_handler::HandlerMap;

/// Per-connection registry of in-flight request ids → cancellation
/// tokens.  A client cancels an in-flight request by sending
/// `{"id": "<some-cancel-id>", "type": "request.cancel",
///   "params": {"targetId": "<original-request-id>"}}`.
/// The dispatcher cancels the matching token; the handler races its
/// work against `token.cancelled()` and returns early.
type InflightRegistry = Arc<Mutex<HashMap<String, CancellationToken>>>;

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

    let user_email = user_email_from_token(token.clone());
    let user_did = user_email
        .as_ref()
        .and_then(|email| AgentService::get_user_did_by_email(email).ok());

    // Per-connection base context.  The dispatcher clones this and
    // injects a fresh `cancel_token` for each in-flight request.
    let ctx = Arc::new(RequestContext {
        capabilities,
        auto_permit_cap_requests: state.auto_permit_cap_requests,
        auth_token: token.clone(),
        is_admin_credential: is_admin,
        user_email: user_email.clone(),
        user_did,
        cancel_token: None,
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
    // Track last_seen at connect time (also updated on each RPC call below)
    track_last_seen_from_token(token.clone()).await;

    let (mut ws_sink, mut ws_stream) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();
    let inflight: InflightRegistry = Arc::new(Mutex::new(HashMap::new()));

    // ── Event broadcast ─────────────────────────────────────────────────
    let token_for_events = token.clone();
    let user_email_for_events = ctx.user_email.clone();
    let is_admin_for_events = ctx.is_admin_credential;
    let tx_events = tx.clone();
    tokio::spawn(async move {
        let event_stream = super::events_ws::build_event_stream(
            token_for_events,
            user_email_for_events,
            is_admin_for_events,
        )
        .await;
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

        // ── `request.cancel` is dispatched inline ────────────────────────
        //
        // It needs access to the per-connection in-flight registry, so we
        // can't route it through the global HandlerMap.  The shape is:
        //   { id: "<cancel-msg-id>", type: "request.cancel",
        //     params: { targetId: "<original-request-id>" } }
        // and the reply is:
        //   { id: "<cancel-msg-id>",
        //     result: { cancelled: true|false, targetId: "..." } }
        //
        // `cancelled: false` is returned when the target id isn't in
        // flight — could mean it already completed, was already
        // cancelled, or never existed.  Always idempotent.
        if msg_type == "request.cancel" {
            let target_id = params
                .get("targetId")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .unwrap_or_default();
            let mut guard = inflight.lock().await;
            let cancelled = if let Some(token) = guard.remove(&target_id) {
                token.cancel();
                true
            } else {
                false
            };
            drop(guard);
            let _ = tx.send(
                json!({
                    "id": id,
                    "result": { "cancelled": cancelled, "targetId": target_id }
                })
                .to_string(),
            );
            continue;
        }

        let handler_map = handler_map.clone();
        let base_ctx = ctx.clone();
        let tx_clone = tx.clone();
        let token_for_dispatch = token.clone();
        let inflight_clone = inflight.clone();

        // Allocate a CancellationToken for this request and stash it in
        // the registry under the request id.  The handler races its
        // work against `cancel_token.cancelled()`; if the client sends
        // `request.cancel`, the racing future fires immediately and we
        // reply with an `AbortError` (code 499).
        let cancel_token = CancellationToken::new();
        {
            let mut guard = inflight.lock().await;
            guard.insert(id.clone(), cancel_token.clone());
        }

        tokio::spawn(async move {
            // Re-check token revocation on every request so that
            // revokeToken() takes effect immediately for existing connections.
            if let Err(e) = check_token_revoked(&token_for_dispatch) {
                // Remove the inflight entry BEFORE sending the terminal
                // reply — same ordering as the normal-completion path
                // below, and for the same reason: a `request.cancel`
                // landing in the gap between send and remove would find
                // the token, cancel it, and report `cancelled: true` for
                // a request that already got its (401) answer.
                let mut guard = inflight_clone.lock().await;
                guard.remove(&id);
                drop(guard);
                let _ = tx_clone
                    .send(json!({"id": id, "error": {"code": 401, "message": e}}).to_string());
                return;
            }
            // Refresh last_seen on every RPC dispatch so long-lived
            // connections don't appear stale. Internally throttled to one
            // DB write per 5 minutes per user.
            track_last_seen_from_token(token_for_dispatch.clone()).await;

            // Build a per-request context that carries the cancel token,
            // so handlers can clone it into long-running operations.
            let mut req_ctx = (*base_ctx).clone();
            req_ctx.cancel_token = Some(cancel_token.clone());
            let req_ctx = Arc::new(req_ctx);

            // Race the handler against cancellation.  On cancel, we
            // drop the handler future — the work it was doing (e.g.
            // a `spawn_blocking` SPARQL eval inside Oxigraph) will
            // continue to run because Rust can't cancel arbitrary
            // CPU-bound work, but the network reply is skipped and
            // the next select arm fires first.
            let response = tokio::select! {
                biased;
                _ = cancel_token.cancelled() => {
                    // 499 mirrors nginx's "Client Closed Request" status —
                    // the OpenAPI SDKs and most HTTP clients recognise it.
                    json!({"id": id, "error": {"code": 499, "message": "Request cancelled by client"}})
                }
                result = handler_map.dispatch(&msg_type, params, req_ctx) => {
                    match result {
                        Ok(val) => json!({"id": id, "result": val}),
                        Err(e) => json!({"id": id, "error": {"code": e.code, "message": e.message}}),
                    }
                }
            };
            // Clean up the registry entry BEFORE sending the response —
            // otherwise a late `request.cancel` arriving between the send
            // and the remove would find the token, cancel it, and report
            // success even though the response already left.
            let mut guard = inflight_clone.lock().await;
            guard.remove(&id);
            drop(guard);
            let _ = tx_clone.send(response.to_string());
        });
    }

    drop(tx);
    if let Err(e) = write_handle.await {
        log::error!("WS RPC writer task failed: {}", e);
    }
}
