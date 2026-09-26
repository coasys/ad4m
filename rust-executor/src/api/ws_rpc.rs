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
        ws::{CloseFrame, Message, WebSocket, WebSocketUpgrade},
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
    let is_admin = is_admin_credential_token(&token, &state.admin_credential);
    let (capabilities, user_email, user_did) = super::auth::resolve_user_session(
        &token,
        capabilities_from_token(token.clone(), state.admin_credential.clone()),
    );

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

    let token_check = TokenCheck::new(&token, is_admin);
    ws.on_upgrade(move |socket| handle_ws(socket, handler_map, ctx, token, token_check))
}

/// Whether a connection receives the event stream: any authenticated session, meaning the
/// admin credential or a token whose capabilities loaded. A socket with no valid token (for
/// example no token on a node that has an admin credential) gets none. The stream's own
/// filters then decide which events reach the session.
pub(crate) fn may_receive_events(ctx: &RequestContext) -> bool {
    ctx.capabilities.is_ok() && (ctx.is_admin_credential || !ctx.auth_token.is_empty())
}

/// Close code for a connection whose token stopped working (RFC 6455 "policy violation").
const CLOSE_TOKEN_ENDED: u16 = 1008;

// ── Connection handler ──────────────────────────────────────────────────────

async fn handle_ws(
    socket: WebSocket,
    handler_map: Arc<HandlerMap>,
    ctx: Arc<RequestContext>,
    token: String,
    token_check: TokenCheck,
) {
    // Track last_seen at connect time (also updated on each RPC call below)
    track_last_seen_from_token(token.clone()).await;

    let (mut ws_sink, mut ws_stream) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();
    let inflight: InflightRegistry = Arc::new(Mutex::new(HashMap::new()));
    // Fires when the token stops working. The connection then closes, so the client
    // authenticates again instead of waiting on a socket that answers nothing.
    let session_over = CancellationToken::new();

    // ── Event broadcast ─────────────────────────────────────────────────
    if may_receive_events(&ctx) {
        let token_check = token_check.clone();
        let user_email = ctx.user_email.clone();
        let is_admin = ctx.is_admin_credential;
        let tx_events = tx.clone();
        let session_over = session_over.clone();
        tokio::spawn(async move {
            let event_stream =
                super::events_ws::build_event_stream(token_check, user_email, is_admin).await;
            tokio::pin!(event_stream);
            while let Some(msg) = event_stream.next().await {
                if tx_events.send(msg).is_err() {
                    return;
                }
            }
            // The stream ends when the token stops working.
            session_over.cancel();
        });
    }

    // ── Writer task ─────────────────────────────────────────────────────
    let writer_session_over = session_over.clone();
    let write_handle = tokio::spawn(async move {
        use futures::SinkExt;
        loop {
            tokio::select! {
                biased;
                _ = writer_session_over.cancelled() => {
                    // Deliver what is already queued (the 401 reply), then close.
                    while let Ok(msg) = rx.try_recv() {
                        if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                            return;
                        }
                    }
                    let close = CloseFrame {
                        code: CLOSE_TOKEN_ENDED,
                        reason: "token expired or revoked".into(),
                    };
                    let _ = ws_sink.send(Message::Close(Some(close))).await;
                    return;
                }
                msg = rx.recv() => match msg {
                    Some(msg) => {
                        if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                            return;
                        }
                    }
                    None => return,
                },
            }
        }
    });

    // ── Reader loop — direct dispatch ───────────────────────────────────
    loop {
        let msg = tokio::select! {
            _ = session_over.cancelled() => break,
            msg = ws_stream.next() => msg,
        };
        let Some(Ok(msg)) = msg else { break };
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
        let token_check = token_check.clone();
        let session_over = session_over.clone();
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
            // Check the token again on every request: expiry and revokeToken() take
            // effect at once for open connections, and end the connection.
            if let Err(e) = token_check.check() {
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
                session_over.cancel();
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

#[cfg(test)]
mod event_gate_tests {
    use super::*;

    fn ctx(token: &str, capabilities: Result<Vec<Capability>, String>) -> RequestContext {
        RequestContext {
            capabilities,
            auto_permit_cap_requests: false,
            auth_token: token.to_string(),
            is_admin_credential: false,
            user_email: None,
            user_did: None,
            cancel_token: None,
        }
    }

    // On a node with an admin credential, a socket with no token gets only these
    // capabilities. It used to receive the main agent's event stream all the same.
    #[test]
    fn an_unauthenticated_socket_receives_no_events() {
        let unauthenticated = ctx(
            "",
            Ok(vec![
                AGENT_AUTH_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
            ]),
        );
        assert!(!may_receive_events(&unauthenticated));
    }

    #[test]
    fn a_session_whose_token_failed_receives_no_events() {
        let failed = ctx("expired-jwt", Err("token expired".to_string()));
        assert!(!may_receive_events(&failed));
    }

    // Apps with narrow capabilities still need their query-subscription and link events.
    #[test]
    fn authenticated_sessions_receive_events_whatever_their_capabilities() {
        let narrow_app = ctx(
            "app-jwt",
            Ok(vec![perspective_query_capability(vec![
                "some-uuid".to_string()
            ])]),
        );
        assert!(may_receive_events(&narrow_app));
        assert!(may_receive_events(&ctx(
            "user-jwt",
            Ok(get_user_default_capabilities())
        )));
        let mut admin = ctx("the-admin-credential", Ok(vec![ALL_CAPABILITY.clone()]));
        admin.is_admin_credential = true;
        assert!(may_receive_events(&admin));
    }
}

#[cfg(test)]
mod socket_tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::pubsub::{get_global_pubsub, AI_MODEL_LOADING_STATUS};
    use crate::test_utils::{setup_agent, setup_wallet};
    use futures::SinkExt;
    use std::time::Duration;
    use tokio_tungstenite::tungstenite::Message as WsMessage;

    type Client = tokio_tungstenite::WebSocketStream<
        tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>,
    >;

    const ADMIN: &str = "socket-test-admin";

    /// Serves the real API router on a free loopback port.
    async fn start_server() -> std::net::SocketAddr {
        setup_wallet();
        setup_agent();
        let _ = Ad4mDb::init_global_instance(":memory:");
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let app = crate::api::api_router(AppState {
            admin_credential: Some(ADMIN.to_string()),
            auto_permit_cap_requests: false,
        });
        tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
        addr
    }

    async fn connect(addr: std::net::SocketAddr, token: &str) -> Client {
        let url = format!("ws://{addr}/api/v1/ws?token={token}");
        tokio_tungstenite::connect_async(url).await.unwrap().0
    }

    /// Sends one request and returns its reply, skipping pushed events.
    async fn call(ws: &mut Client, id: &str, op: &str) -> Value {
        let request = json!({"id": id, "type": op, "params": {}}).to_string();
        ws.send(WsMessage::Text(request.into())).await.unwrap();
        loop {
            let frame = tokio::time::timeout(Duration::from_secs(10), ws.next())
                .await
                .expect("a reply")
                .expect("an open socket")
                .unwrap();
            if let WsMessage::Text(text) = frame {
                let reply: Value = serde_json::from_str(&text).unwrap();
                if reply["id"] == id {
                    return reply;
                }
            }
        }
    }

    /// The close code the server sends, skipping any other frame.
    async fn close_code(ws: &mut Client) -> Option<u16> {
        loop {
            match tokio::time::timeout(Duration::from_secs(10), ws.next()).await {
                Ok(Some(Ok(WsMessage::Close(frame)))) => return frame.map(|f| u16::from(f.code)),
                Ok(Some(Ok(_))) => continue,
                _ => return None,
            }
        }
    }

    // revokeToken() used to leave open connections working: the token was checked at the
    // upgrade only.
    #[tokio::test]
    async fn a_token_revoked_mid_connection_gets_401_and_the_socket_closes() {
        let addr = start_server().await;
        crate::test_utils::use_test_apps_file();
        let token = generate_jwt(
            "socket-test".to_string(),
            3600,
            AuthInfo {
                capabilities: Some(vec![ALL_CAPABILITY.clone()]),
                ..AuthInfo::default()
            },
        )
        .unwrap();
        let request_key = format!("key-{}", uuid::Uuid::new_v4());
        let app = AuthInfoExtended {
            request_id: request_key.clone(),
            auth: AuthInfo::default(),
        };
        apps_map::insert_app(request_key.clone(), app, token.clone()).unwrap();

        let mut ws = connect(addr, &token).await;
        let before = call(&mut ws, "1", "agent.status").await;
        assert_ne!(before["error"]["code"], 401, "{before}");

        apps_map::revoke_app(&request_key).unwrap();
        let after = call(&mut ws, "2", "agent.status").await;
        assert_eq!(after["error"]["code"], 401, "{after}");
        assert_eq!(close_code(&mut ws).await, Some(CLOSE_TOKEN_ENDED));
        apps_map::remove_app(&request_key).unwrap();
    }

    // With an admin credential set, a socket with no token used to get the main agent's
    // event stream.
    #[tokio::test]
    async fn a_socket_without_a_token_gets_no_events() {
        let addr = start_server().await;
        let mut admin = connect(addr, ADMIN).await;
        let mut anonymous = connect(addr, "").await;

        // Publish until the admin socket sees an event, so both streams had time to subscribe.
        let event = r#"{"model":"socket-test","status":"loading"}"#.to_string();
        let seen = tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                get_global_pubsub()
                    .await
                    .publish(&AI_MODEL_LOADING_STATUS, &event)
                    .await;
                if let Ok(Some(Ok(WsMessage::Text(text)))) =
                    tokio::time::timeout(Duration::from_millis(200), admin.next()).await
                {
                    if text.contains("socket-test") {
                        return;
                    }
                }
            }
        })
        .await;
        assert!(seen.is_ok(), "the admin socket must receive the event");

        let leaked = tokio::time::timeout(Duration::from_millis(500), async {
            while let Some(Ok(frame)) = anonymous.next().await {
                if let WsMessage::Text(text) = frame {
                    if text.contains("socket-test") {
                        return true;
                    }
                }
            }
            false
        })
        .await;
        assert!(
            !matches!(leaked, Ok(true)),
            "a socket without a token got an event"
        );
    }
}
