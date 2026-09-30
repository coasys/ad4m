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
use super::event_interest;
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
        connection_id: Some(uuid::Uuid::new_v4().to_string()),
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

    let (mut ws_sink, ws_stream) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();

    // ── Writer task ─────────────────────────────────────────────────────
    let write_handle = tokio::spawn(async move {
        use futures::SinkExt;
        while let Some(msg) = rx.recv().await {
            if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                break;
            }
        }
    });

    let events = super::events_ws::build_event_stream(
        token.clone(),
        ctx.user_email.clone(),
        ctx.is_admin_credential,
        ctx.connection_id.clone(),
    )
    .await;
    // Text frames until the socket closes or errors; pings and binary
    // frames are skipped.
    let incoming = ws_stream
        .take_while(|msg| {
            futures::future::ready(matches!(msg, Ok(m) if !matches!(m, Message::Close(_))))
        })
        .filter_map(|msg| {
            futures::future::ready(match msg {
                Ok(Message::Text(t)) => Some(t.to_string()),
                _ => None,
            })
        });
    serve(
        Connection::new(handler_map, ctx, token, tx),
        incoming,
        events,
    )
    .await;

    // The writer ends once calls still in flight drop their senders.
    if let Err(e) = write_handle.await {
        log::error!("WS RPC writer task failed: {}", e);
    }
}

/// Per-connection state: what the reader needs to answer a message.
pub(crate) struct Connection {
    handler_map: Arc<HandlerMap>,
    ctx: Arc<RequestContext>,
    token: String,
    tx: mpsc::UnboundedSender<String>,
    inflight: InflightRegistry,
    interest: event_interest::SharedInterest,
    /// Dispatched calls still running.
    calls: std::sync::Mutex<tokio::task::JoinSet<()>>,
}

impl Connection {
    /// A connection whose replies and events go to `tx`.
    pub(crate) fn new(
        handler_map: Arc<HandlerMap>,
        ctx: Arc<RequestContext>,
        token: String,
        tx: mpsc::UnboundedSender<String>,
    ) -> Self {
        Self {
            handler_map,
            ctx,
            token,
            tx,
            inflight: Default::default(),
            interest: Default::default(),
            calls: Default::default(),
        }
    }

    /// Handle one text message from the client.
    pub(crate) async fn handle_text(&self, text: &str) {
        let tx = &self.tx;

        // Parse JSON
        let parsed: Value = match serde_json::from_str(text) {
            Ok(v) => v,
            Err(_) => {
                let _ = tx.send(json!({"error":{"code":400,"message":"Invalid JSON"}}).to_string());
                return;
            }
        };

        // Handle ping/pong keepalive
        if parsed.get("type").and_then(|v| v.as_str()) == Some("ping") {
            let _ = tx.send(json!({"type":"pong"}).to_string());
            return;
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
                return;
            }
        };

        let params = parsed.get("params").cloned().unwrap_or(json!({}));

        // `events.watch` / `events.unwatch` set this connection's event
        // filter (see `event_interest`), so they are handled inline too.
        if let Some(reply) = event_interest::handle_control(
            &msg_type,
            &Value::String(id.clone()),
            &params,
            &self.interest,
        ) {
            let _ = tx.send(reply);
            return;
        }

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
            let mut guard = self.inflight.lock().await;
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
            return;
        }

        // Allocate a CancellationToken for this request and stash it in
        // the registry under the request id.  The handler races its
        // work against `cancel_token.cancelled()`; if the client sends
        // `request.cancel`, the racing future fires immediately and we
        // reply with an `AbortError` (code 499).
        let cancel_token = CancellationToken::new();
        {
            let mut guard = self.inflight.lock().await;
            guard.insert(id.clone(), cancel_token.clone());
        }

        let handler_map = self.handler_map.clone();
        let base_ctx = self.ctx.clone();
        let token = self.token.clone();
        let inflight = self.inflight.clone();
        let tx = tx.clone();
        let mut calls = self.calls.lock().unwrap_or_else(|e| e.into_inner());
        while calls.try_join_next().is_some() {}
        calls.spawn(async move {
            // Re-check token revocation on every request so that
            // revokeToken() takes effect immediately for existing connections.
            if let Err(e) = check_token_revoked(&token) {
                // Remove the inflight entry BEFORE sending the terminal
                // reply — same ordering as the normal-completion path
                // below, and for the same reason: a `request.cancel`
                // landing in the gap between send and remove would find
                // the token, cancel it, and report `cancelled: true` for
                // a request that already got its (401) answer.
                let mut guard = inflight.lock().await;
                guard.remove(&id);
                drop(guard);
                let _ =
                    tx.send(json!({"id": id, "error": {"code": 401, "message": e}}).to_string());
                return;
            }
            // Refresh last_seen on every RPC dispatch so long-lived
            // connections don't appear stale. Internally throttled to one
            // DB write per 5 minutes per user.
            track_last_seen_from_token(token.clone()).await;

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
            let mut guard = inflight.lock().await;
            guard.remove(&id);
            drop(guard);
            let _ = tx.send(response.to_string());
        });
    }
}

/// Serve one connection until `incoming` ends: answer each client message
/// and forward `events` that match the connection's interest. When the
/// client is gone the event task stops, the connection's subscriptions end,
/// and calls still in flight finish.
pub(crate) async fn serve<S>(
    conn: Connection,
    incoming: S,
    events: std::pin::Pin<Box<dyn futures::Stream<Item = String> + Send>>,
) where
    S: futures::Stream<Item = String>,
{
    let tx_events = conn.tx.clone();
    let event_stream = event_interest::filter_stream(events, conn.interest.clone());
    let event_task = tokio::spawn(async move {
        tokio::pin!(event_stream);
        while let Some(msg) = event_stream.next().await {
            if tx_events.send(msg).is_err() {
                break;
            }
        }
    });

    tokio::pin!(incoming);
    while let Some(text) = incoming.next().await {
        conn.handle_text(&text).await;
    }

    event_task.abort();
    let _ = event_task.await;

    // End the live queries now. A subscribe still in flight adds one after
    // this sweep, so sweep again as each call ends.
    let dispose = || async {
        if let Some(connection_id) = &conn.ctx.connection_id {
            crate::perspectives::dispose_connection_subscriptions(connection_id).await;
        }
    };
    dispose().await;
    let mut calls = std::mem::take(&mut *conn.calls.lock().unwrap_or_else(|e| e.into_inner()));
    while calls.join_next().await.is_some() {
        dispose().await;
    }
}
