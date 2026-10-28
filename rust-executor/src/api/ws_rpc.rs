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
use std::time::Duration;
use tokio::sync::{mpsc, Mutex};
use tokio::time::Instant;
use tokio_util::sync::CancellationToken;

use crate::agent::capabilities::*;
use crate::agent::session::AuthError;
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
    reach: ListenerReach,
) -> impl IntoResponse {
    let token = query.token.unwrap_or_default();
    match connection_context(&token, &state.admin_credential, reach) {
        Ok(ctx) => ws.on_upgrade(move |socket| handle_ws(socket, handler_map, ctx)),
        Err(e) => {
            let code = e.close_code();
            ws.on_upgrade(move |socket| close_refused(socket, code, e.to_string()))
        }
    }
}

/// The context a connection keeps for its lifetime. Its session's `check` runs again on
/// every request and event. The dispatcher clones the context and injects a fresh
/// `cancel_token` per request.
///
/// Fails for a token that opens no session (invalid, expired, revoked, or a user whose DID
/// does not load): the socket then closes at once with [`CLOSE_TOKEN_ENDED`].
fn connection_context(
    token: &str,
    admin_credential: &Option<String>,
    reach: ListenerReach,
) -> Result<Arc<RequestContext>, AuthError> {
    let mut ctx = RequestContext::open(token, admin_credential, reach)?;
    // Live queries opened on this connection belong to it and end when it closes.
    ctx.connection_id = Some(uuid::Uuid::new_v4().to_string());
    Ok(Arc::new(ctx))
}

/// Whether a connection receives the event stream: the operator or a grant. An anonymous
/// socket (no token, for example on an executor that has an admin credential) gets none.
/// The stream's own filters then decide which events reach the session.
pub(crate) fn may_receive_events(ctx: &RequestContext) -> bool {
    ctx.session.is_authenticated()
}

/// Close code for a connection whose token opens no session or stopped working (RFC 6455
/// "policy violation"). Clients read it as "log in again".
pub(crate) const CLOSE_TOKEN_ENDED: u16 = 1008;

/// Close code for a connection refused because the executor's wallet is locked (RFC 6455 "try
/// again later"). Clients keep their token and reconnect.
pub(crate) const CLOSE_TRY_AGAIN_LATER: u16 = 1013;

/// Closes a socket whose token opened no session, before anything else runs on it, with
/// `code` ([`CLOSE_TOKEN_ENDED`] or [`CLOSE_TRY_AGAIN_LATER`]). `reason` says why: expired,
/// unauthorized, the identity did not load, or the wallet is locked.
///
/// The upgrade succeeds first: a browser cannot read the HTTP status of a refused upgrade,
/// but it reads the close code and reason.
pub(crate) async fn close_refused(mut socket: WebSocket, code: u16, reason: String) {
    let close = CloseFrame {
        code,
        reason: close_reason(&reason).into(),
    };
    let _ = socket.send(Message::Close(Some(close))).await;
}

/// `reason`, cut to the 123 bytes a close frame carries.
fn close_reason(reason: &str) -> String {
    const MAX: usize = 123;
    if reason.len() <= MAX {
        return reason.to_string();
    }
    let end = (0..=MAX)
        .rev()
        .find(|&i| reason.is_char_boundary(i))
        .unwrap_or(0);
    reason[..end].to_string()
}

// ── Connection handler ──────────────────────────────────────────────────────

async fn handle_ws(socket: WebSocket, handler_map: Arc<HandlerMap>, ctx: Arc<RequestContext>) {
    // Track last_seen at connect time (also updated on each RPC call below)
    if let Some(did) = ctx.user_did() {
        track_last_seen(did).await;
    }

    let (ws_sink, ws_stream) = socket.split();
    let (tx, rx) = mpsc::unbounded_channel::<String>();

    // A socket without a token gets no events: a stream that never yields.
    let events: EventStream = if may_receive_events(&ctx) {
        // Capability requests, app changes and notification install requests go
        // only to sessions that can approve apps.
        let can_approve_apps = ctx.session.may(&AGENT_PERMIT_CAPABILITY);
        super::events_ws::build_event_stream(
            ctx.session.clone(),
            ctx.is_operator(),
            can_approve_apps,
            super::events_ws::sfu_events_allowed(&ctx),
            ctx.connection_id.clone(),
        )
        .await
    } else {
        Box::pin(futures::stream::pending())
    };
    let conn = Connection::new(handler_map, ctx, tx);

    // ── Writer task ─────────────────────────────────────────────────────
    let liveness = Arc::new(std::sync::Mutex::new(Liveness::new(Instant::now())));
    let mut write_handle = tokio::spawn(write_to(
        ws_sink,
        rx,
        conn.session_over.clone(),
        liveness.clone(),
    ));

    // Text frames until the socket closes, errors or goes silent; pongs, pings
    // and binary frames count as heard and are skipped.
    let incoming = until_silent(ws_stream, liveness)
        .take_while(|msg| {
            futures::future::ready(matches!(msg, Ok(m) if !matches!(m, Message::Close(_))))
        })
        .filter_map(|msg| {
            futures::future::ready(match msg {
                Ok(Message::Text(t)) => Some(t.to_string()),
                _ => None,
            })
        });
    serve(conn, incoming, events).await;

    // The writer ends once calls still in flight drop their senders and what
    // they queued is sent. A client that went silent takes nothing, and the
    // send would wait until TCP gives up on it: close the socket instead.
    match tokio::time::timeout(SILENCE_LIMIT, &mut write_handle).await {
        Ok(Err(e)) => log::error!("WS RPC writer task failed: {}", e),
        Ok(Ok(())) => {}
        Err(_) => write_handle.abort(),
    }
}

/// How often the executor sends its client a WebSocket ping. Every client answers
/// one by itself, so a live client is heard from at least this often, once what
/// the executor wrote before the ping has reached it.
const PING_INTERVAL: Duration = Duration::from_secs(20);

/// How long a client may send no frame at all before its connection ends. After
/// a network switch a dead client's socket stays open with no close for as long
/// as TCP retries, while its live queries run and its events queue up.
const SILENCE_LIMIT: Duration = Duration::from_secs(60);

/// The slowest link a client is given time for, as in the SDK. A pong waits
/// behind what the executor wrote before its ping, so silence counts only once
/// that could have crossed this link, if that takes longer than SILENCE_LIMIT.
const SLOWEST_LINK_BYTES_PER_SEC: u64 = 16_000;

/// How long a pong gets to come back after its ping could have reached the
/// client behind a large write.
const PONG_RETURN: Duration = Duration::from_secs(5);

/// How long `bytes` take to cross a SLOWEST_LINK_BYTES_PER_SEC link.
fn crossing(bytes: usize) -> Duration {
    Duration::from_micros(bytes as u64 * 1_000_000 / SLOWEST_LINK_BYTES_PER_SEC)
}

/// What the executor knows about whether its client is still there. The writer
/// counts what it sends; the reader notes each frame.
#[derive(Debug)]
struct Liveness {
    /// When the client was last heard from.
    heard_at: Instant,
    /// When everything written so far could have crossed the slowest link. A
    /// frame from the client does not move it back: hearing from the client says
    /// nothing about how much of a large reply it has read.
    sent_by: Instant,
    /// When the first ping written since the client was last heard could have
    /// reached it; `None` while no ping went out since. Writes after that ping
    /// do not move it: they queue behind it, so they cannot delay its pong.
    ping_arrives_by: Option<Instant>,
}

impl Liveness {
    fn new(now: Instant) -> Self {
        Liveness {
            heard_at: now,
            sent_by: now,
            ping_arrives_by: None,
        }
    }

    fn wrote(&mut self, bytes: usize, now: Instant) {
        self.sent_by = self.sent_by.max(now) + crossing(bytes);
    }

    fn pinged(&mut self, now: Instant) {
        self.sent_by = self.sent_by.max(now);
        self.ping_arrives_by.get_or_insert(self.sent_by);
    }

    fn heard(&mut self, now: Instant) {
        self.heard_at = now;
        self.ping_arrives_by = None;
    }

    /// When the client counts as gone if it has not been heard from: SILENCE_LIMIT
    /// after it was last heard, or PONG_RETURN after the ping sent since then
    /// could have reached it, whichever is later. Until that ping goes out, after
    /// what was written so far could have reached it.
    fn deadline(&self) -> Instant {
        let reached_by = self.ping_arrives_by.unwrap_or(self.sent_by);
        (self.heard_at + SILENCE_LIMIT).max(reached_by + PONG_RETURN)
    }
}

/// Send what `rx` carries to `sink`, and a ping every PING_INTERVAL, until `rx`
/// closes or a send fails. Once the session is over, deliver what is already
/// queued (the 401 reply), then close.
async fn write_to<S>(
    mut sink: S,
    mut rx: mpsc::UnboundedReceiver<String>,
    session_over: CancellationToken,
    liveness: Arc<std::sync::Mutex<Liveness>>,
) where
    S: futures::Sink<Message> + Unpin,
{
    use futures::SinkExt;
    let mut ping = tokio::time::interval_at(Instant::now() + PING_INTERVAL, PING_INTERVAL);
    // A send that blocks for longer than an interval is followed by one ping, not a burst.
    ping.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    loop {
        tokio::select! {
            biased;
            _ = session_over.cancelled() => {
                while let Ok(msg) = rx.try_recv() {
                    if sink.send(Message::Text(msg.into())).await.is_err() {
                        return;
                    }
                }
                let close = CloseFrame {
                    code: CLOSE_TOKEN_ENDED,
                    reason: "token expired or revoked".into(),
                };
                let _ = sink.send(Message::Close(Some(close))).await;
                return;
            }
            // Before the queue: with replies or events always waiting, a ping
            // would otherwise never go out. It fires at most once an interval.
            _ = ping.tick() => {
                liveness.lock().unwrap().pinged(Instant::now());
                if sink.send(Message::Ping(Default::default())).await.is_err() {
                    return;
                }
            }
            msg = rx.recv() => match msg {
                Some(msg) => {
                    liveness.lock().unwrap().wrote(msg.len(), Instant::now());
                    if sink.send(Message::Text(msg.into())).await.is_err() {
                        return;
                    }
                }
                None => return,
            },
        }
    }
}

/// `frames` until the client has been silent for longer than a live one could
/// be ([`Liveness::deadline`]).
fn until_silent<S>(
    frames: S,
    liveness: Arc<std::sync::Mutex<Liveness>>,
) -> impl futures::Stream<Item = S::Item>
where
    S: futures::Stream + Unpin,
{
    futures::stream::unfold(frames, move |mut frames| {
        let liveness = liveness.clone();
        async move {
            loop {
                let deadline = liveness.lock().unwrap().deadline();
                if Instant::now() >= deadline {
                    let heard_at = liveness.lock().unwrap().heard_at;
                    log::info!(
                        "WS RPC: client silent for {:?}; ending the connection",
                        deadline - heard_at
                    );
                    return None;
                }
                // Woken at the deadline, check it again: more may have been written.
                if let Ok(frame) = tokio::time::timeout_at(deadline, frames.next()).await {
                    liveness.lock().unwrap().heard(Instant::now());
                    return frame.map(|frame| (frame, frames));
                }
            }
        }
    })
}

type EventStream = std::pin::Pin<Box<dyn futures::Stream<Item = String> + Send>>;

/// Per-connection state: what the reader needs to answer a message.
pub(crate) struct Connection {
    handler_map: Arc<HandlerMap>,
    /// Carries the session that every request checks again.
    ctx: Arc<RequestContext>,
    /// Fires when the token stops working. The connection then closes, so the client
    /// authenticates again instead of waiting on a socket that answers nothing.
    session_over: CancellationToken,
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
        tx: mpsc::UnboundedSender<String>,
    ) -> Self {
        Self {
            handler_map,
            ctx,
            session_over: CancellationToken::new(),
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
        let inflight = self.inflight.clone();
        let session_over = self.session_over.clone();
        let tx = tx.clone();
        let mut calls = self.calls.lock().unwrap_or_else(|e| e.into_inner());
        while calls.try_join_next().is_some() {}
        calls.spawn(async move {
            // Check the session again on every request: expiry and revokeToken() take
            // effect at once for open connections, and end the connection.
            if let Err(e) = base_ctx.session.check() {
                // Remove the inflight entry BEFORE sending the terminal
                // reply — same ordering as the normal-completion path
                // below, and for the same reason: a `request.cancel`
                // landing in the gap between send and remove would find
                // the token, cancel it, and report `cancelled: true` for
                // a request that already got its (401) answer.
                let mut guard = inflight.lock().await;
                guard.remove(&id);
                drop(guard);
                let _ = tx.send(
                    json!({"id": id, "error": {"code": 401, "message": e.to_string()}}).to_string(),
                );
                session_over.cancel();
                return;
            }
            // Refresh last_seen on every RPC dispatch so long-lived
            // connections don't appear stale. Internally throttled to one
            // DB write per 5 minutes per user.
            if let Some(did) = base_ctx.user_did() {
                track_last_seen(did).await;
            }

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

/// Serve one connection until `incoming` ends or the token stops working:
/// answer each client message and forward `events` that match the
/// connection's interest. When the client is gone the event task stops, the
/// connection's subscriptions end, and calls still in flight finish and reply
/// to `tx`. `events` ends when the token stops working (see
/// `build_event_stream`), and that ends the session too.
pub(crate) async fn serve<S>(conn: Connection, incoming: S, events: EventStream)
where
    S: futures::Stream<Item = String>,
{
    let tx_events = conn.tx.clone();
    let event_session_over = conn.session_over.clone();
    let event_stream = event_interest::filter_stream(events, conn.interest.clone());
    let event_task = tokio::spawn(async move {
        tokio::pin!(event_stream);
        while let Some(msg) = event_stream.next().await {
            if tx_events.send(msg).is_err() {
                return;
            }
        }
        // The stream ends when the token stops working.
        event_session_over.cancel();
    });

    tokio::pin!(incoming);
    loop {
        let text = tokio::select! {
            _ = conn.session_over.cancelled() => break,
            text = incoming.next() => text,
        };
        let Some(text) = text else { break };
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

#[cfg(test)]
mod connection_context_tests {
    //! The context a connection keeps comes from its session. How each kind of token
    //! resolves is pinned in `session::resolution_tests`.
    use super::*;
    use crate::test_utils::{setup_agent, setup_wallet};

    const ADMIN: &str = "the-admin-credential";

    fn jwt(capabilities: Option<Vec<Capability>>) -> String {
        token::generate_jwt(
            "an app".to_string(),
            3600,
            AuthInfo {
                capabilities,
                ..AuthInfo::default()
            },
        )
        .unwrap()
    }

    // These tokens used to get a connection whose every capability check failed.
    #[test]
    fn a_token_that_opens_no_session_gets_no_connection() {
        setup_wallet();
        setup_agent();
        crate::test_utils::use_test_apps_file();
        let revoked = jwt(Some(vec![AGENT_READ_CAPABILITY.clone()]));
        let request_id = format!("context-{}", uuid::Uuid::new_v4());
        let app = AuthInfoExtended {
            request_id: request_id.clone(),
            auth: AuthInfo::default(),
        };
        apps_map::insert_app(request_id.clone(), app, revoked.clone()).unwrap();
        apps_map::revoke_app(&request_id).unwrap();

        let refused = [
            crate::test_utils::keyless_user_token("context.nokey@example.org"),
            crate::test_utils::expired_user_token("context.expired@example.org"),
            revoked,
            "not-a-jwt".to_string(),
        ];
        for token in &refused {
            for reach in [ListenerReach::Loopback, ListenerReach::Network] {
                assert!(
                    connection_context(token, &Some(ADMIN.to_string()), reach).is_err(),
                    "token {token:.20}…, {reach:?}"
                );
            }
        }
    }

    // Live queries belong to the connection that opened them, so each connection needs its
    // own id; an HTTP request (`RequestContext::open` alone) has none.
    #[test]
    fn every_connection_gets_its_own_connection_id() {
        setup_wallet();
        setup_agent();
        let admin = Some(ADMIN.to_string());
        let a = connection_context(ADMIN, &admin, ListenerReach::Network).unwrap();
        let b = connection_context(ADMIN, &admin, ListenerReach::Network).unwrap();
        assert!(a.connection_id.is_some());
        assert_ne!(a.connection_id, b.connection_id);
        let http = RequestContext::open(ADMIN, &admin, ListenerReach::Network).unwrap();
        assert_eq!(http.connection_id, None);
    }

    #[test]
    fn a_session_connection_stops_working_once_its_token_is_revoked() {
        setup_wallet();
        setup_agent();
        crate::test_utils::use_test_apps_file();
        let token = jwt(Some(vec![AGENT_READ_CAPABILITY.clone()]));
        let request_id = format!("context-{}", uuid::Uuid::new_v4());
        let app = AuthInfoExtended {
            request_id: request_id.clone(),
            auth: AuthInfo::default(),
        };
        apps_map::insert_app(request_id.clone(), app, token.clone()).unwrap();

        let ctx =
            connection_context(&token, &Some(ADMIN.to_string()), ListenerReach::Network).unwrap();
        assert!(ctx.session.check().is_ok());
        apps_map::revoke_app(&request_id).unwrap();
        assert_eq!(
            ctx.session.check().unwrap_err().to_string(),
            "Unauthorized access"
        );
    }

    #[test]
    fn a_session_connection_acts_as_the_agent_its_token_names() {
        setup_wallet();
        setup_agent();
        crate::test_utils::use_test_apps_file();
        let user = "context.actsas@example.org";
        let user_did = crate::test_utils::create_test_user(user);
        let tokens = [
            ADMIN.to_string(),
            jwt(None),
            crate::user_management::generate_user_jwt(user, "test").unwrap(),
        ];

        let expected = [
            crate::agent::AgentContext::main_agent(),
            crate::agent::AgentContext::main_agent(),
            crate::agent::AgentContext::managed(user_did),
        ];
        for (token, expected) in tokens.iter().zip(expected) {
            let ctx = connection_context(token, &Some(ADMIN.to_string()), ListenerReach::Network)
                .unwrap_or_else(|e| panic!("token {token:.20}… opens a session: {e}"));
            assert_eq!(ctx.acts_as(), expected, "token {token:.20}…");
        }
    }
}

#[cfg(test)]
mod event_gate_tests {
    use super::*;
    use crate::agent::session::{Principal, Session};

    // On an executor with an admin credential, a socket with no token gets only these
    // capabilities. It used to receive the main agent's event stream all the same.
    #[test]
    fn an_unauthenticated_socket_receives_no_events() {
        let unauthenticated =
            RequestContext::for_session(Session::for_test(Principal::Anonymous).with_caps(vec![
                AGENT_AUTH_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
            ]));
        assert!(!may_receive_events(&unauthenticated));
    }

    // Apps with narrow capabilities still need their query-subscription and link events.
    #[test]
    fn authenticated_sessions_receive_events_whatever_their_capabilities() {
        let narrow_app = RequestContext::app_for_test(vec![perspective_query_capability(vec![
            "some-uuid".to_string(),
        ])]);
        assert!(may_receive_events(&narrow_app));
        assert!(may_receive_events(&RequestContext::user_for_test(
            "did:key:events-gate",
            get_user_default_capabilities()
        )));
        assert!(may_receive_events(&RequestContext::operator_for_test()));
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
        call_with(ws, id, op, json!({})).await
    }

    async fn call_with(ws: &mut Client, id: &str, op: &str, params: Value) -> Value {
        let request = json!({"id": id, "type": op, "params": params}).to_string();
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
    // upgrade only. The operator revokes through the RPC, by the app's JWT (#1060).
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

        let mut operator = connect(addr, ADMIN).await;
        let revoked = call_with(
            &mut operator,
            "revoke",
            "agent.revokeToken",
            json!({ "token": token }),
        )
        .await;
        assert!(revoked.get("error").is_none(), "{revoked}");
        let after = call(&mut ws, "2", "agent.status").await;
        assert_eq!(after["error"]["code"], 401, "{after}");
        assert_eq!(close_code(&mut ws).await, Some(CLOSE_TOKEN_ENDED));
        apps_map::remove_app(&request_key).unwrap();
    }

    // A dead token used to get a connection whose every call failed a capability check, so
    // clients kept it open and never logged in again.
    #[tokio::test]
    async fn a_dead_token_is_closed_with_1008_right_after_the_upgrade() {
        let addr = start_server().await;
        crate::test_utils::use_test_apps_file();
        let revoked = generate_jwt(
            "socket-test-revoked".to_string(),
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
        apps_map::insert_app(request_key.clone(), app, revoked.clone()).unwrap();
        apps_map::revoke_app(&request_key).unwrap();
        let expired = crate::test_utils::expired_user_token("socket.expired@example.org");

        for (name, token) in [("revoked", revoked.as_str()), ("expired", expired.as_str())] {
            // The upgrade succeeds, so a browser can read the close code.
            let mut ws = connect(addr, token).await;
            let request = json!({"id": "1", "type": "agent.status", "params": {}}).to_string();
            // The server may have closed already; the send result does not matter.
            let _ = ws.send(WsMessage::Text(request.into())).await;
            let close = loop {
                match tokio::time::timeout(Duration::from_secs(10), ws.next())
                    .await
                    .unwrap_or_else(|_| panic!("{name}: the socket stayed open"))
                {
                    Some(Ok(WsMessage::Close(frame))) => break frame,
                    Some(Ok(WsMessage::Text(text))) => panic!("{name}: got a reply: {text}"),
                    Some(Ok(_)) => continue,
                    other => panic!("{name}: no close frame: {other:?}"),
                }
            };
            let close = close.unwrap_or_else(|| panic!("{name}: a close frame without a code"));
            assert_eq!(u16::from(close.code), CLOSE_TOKEN_ENDED, "{name}");
            assert!(
                !close.reason.is_empty(),
                "{name}: the close names the reason"
            );
        }
        apps_map::remove_app(&request_key).unwrap();
    }

    // A wallet locked at connect says nothing about the token: before sessions, such a
    // connection opened and `agent.isLocked` answered. The socket closes with 1013, which
    // clients read as "try again later" and not as "log in again", so they keep the token.
    #[tokio::test]
    async fn a_locked_wallet_closes_with_1013_and_the_token_works_after_the_unlock() {
        let addr = start_server().await;
        let token = generate_jwt(
            "socket-test-locked".to_string(),
            3600,
            AuthInfo {
                capabilities: Some(vec![ALL_CAPABILITY.clone()]),
                ..AuthInfo::default()
            },
        )
        .unwrap();
        let backend = crate::wallet::wallet_backend();
        backend.lock("socket-locked-test");
        let mut ws = connect(addr, &token).await;
        let first = tokio::time::timeout(Duration::from_secs(10), ws.next()).await;
        let http = reqwest::Client::new()
            .get(format!("http://{addr}/v1/models"))
            .bearer_auth(&token)
            .send()
            .await
            .map(|r| r.status().as_u16());
        // Unlock before asserting, so a failure cannot leave the shared wallet locked.
        backend
            .unlock("socket-locked-test")
            .expect("restore the shared wallet");

        match first.expect("the socket stayed open") {
            Some(Ok(WsMessage::Close(Some(frame)))) => {
                assert_eq!(u16::from(frame.code), CLOSE_TRY_AGAIN_LATER);
                assert!(
                    frame.reason.starts_with("Executor is locked"),
                    "{}",
                    frame.reason
                );
            }
            other => panic!("expected a close frame, got {other:?}"),
        }
        assert_eq!(
            http.unwrap(),
            503,
            "HTTP says unavailable, not unauthorized"
        );

        // After the unlock the same token opens a working connection.
        let mut ws = connect(addr, &token).await;
        let request = json!({"id": "1", "type": "agent.status", "params": {}}).to_string();
        ws.send(WsMessage::Text(request.into())).await.unwrap();
        match tokio::time::timeout(Duration::from_secs(10), ws.next()).await {
            Ok(Some(Ok(WsMessage::Text(text)))) => {
                let reply: Value = serde_json::from_str(&text).unwrap();
                assert!(reply.get("error").is_none(), "{text}");
                assert_eq!(reply["result"]["isUnlocked"], true, "{text}");
            }
            other => panic!("expected a reply, got {other:?}"),
        }
    }

    // The events socket used to answer a dead token with a refused upgrade (the HTTP
    // extractor's error), and the realtime socket opened and then refused each request. A
    // browser reads neither as "log in again".
    #[tokio::test]
    async fn the_events_and_realtime_sockets_close_a_dead_token_with_1008() {
        let addr = start_server().await;
        for path in ["/api/v1/ws/events", "/v1/realtime"] {
            let url = format!("ws://{addr}{path}?token=not-a-jwt");
            let mut ws = tokio_tungstenite::connect_async(url)
                .await
                .unwrap_or_else(|e| panic!("{path}: the upgrade must succeed: {e}"))
                .0;
            assert_eq!(close_code(&mut ws).await, Some(CLOSE_TOKEN_ENDED), "{path}");
        }
    }

    #[test]
    fn a_close_reason_fits_a_close_frame() {
        assert_eq!(close_reason("Unauthorized access"), "Unauthorized access");
        let long = "é".repeat(100);
        let cut = close_reason(&long);
        assert!(cut.len() <= 123);
        assert!(long.starts_with(&cut));
    }

    // With an admin credential set, a socket with no token used to get the main agent's
    // event stream.
    #[tokio::test]
    async fn a_socket_without_a_token_gets_no_events() {
        let addr = start_server().await;
        let mut admin = connect(addr, ADMIN).await;
        let mut anonymous = connect(addr, "").await;
        // Both sockets watch the event, so only the token gate can keep it from one.
        let watch = json!({ "model-loading-status": null });
        for ws in [&mut admin, &mut anonymous] {
            let reply = call_with(ws, "watch", "events.watch", watch.clone()).await;
            assert_eq!(reply["result"], true, "{reply}");
        }

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

#[cfg(test)]
mod liveness_tests {
    //! A client that stops sending frames is let go: its stream of frames ends, and
    //! `serve` then ends the connection's subscriptions and its event queue. A
    //! client still reading a large reply on a slow link is not.
    use super::*;
    use std::time::Duration;
    use tokio::time::Instant;

    fn fresh() -> Arc<std::sync::Mutex<Liveness>> {
        Arc::new(std::sync::Mutex::new(Liveness::new(Instant::now())))
    }

    fn secs(s: f64) -> Duration {
        Duration::from_secs_f64(s)
    }

    /// When the stream ends, measured from `start`. A stream still going after
    /// 1000 s fails the test instead of running for ever on the paused clock.
    async fn ended_after<S: futures::Stream + Unpin>(mut frames: S, start: Instant) -> Duration {
        let drained = async { while frames.next().await.is_some() {} };
        tokio::time::timeout(Duration::from_secs(1000), drained)
            .await
            .expect("the client was never let go");
        start.elapsed()
    }

    /// Frames sent at the given times after the start, then silence, the socket held open.
    fn frames_at(times: Vec<f64>) -> impl futures::Stream<Item = ()> + Unpin {
        let (tx, rx) = mpsc::unbounded_channel();
        let start = Instant::now();
        tokio::spawn(async move {
            for t in times {
                tokio::time::sleep_until(start + secs(t)).await;
                let _ = tx.send(());
            }
            std::future::pending::<()>().await;
        });
        tokio_stream::wrappers::UnboundedReceiverStream::new(rx)
    }

    #[tokio::test(start_paused = true)]
    async fn a_silent_client_is_let_go_after_the_limit() {
        let start = Instant::now();
        let frames = until_silent(futures::stream::pending::<()>(), fresh());
        assert_eq!(ended_after(Box::pin(frames), start).await, SILENCE_LIMIT);
    }

    #[tokio::test(start_paused = true)]
    async fn every_frame_restarts_the_limit() {
        let start = Instant::now();
        let step = SILENCE_LIMIT.as_secs_f64() - 1.0;
        let frames = until_silent(frames_at(vec![step, 2.0 * step, 3.0 * step]), fresh());
        assert_eq!(
            ended_after(Box::pin(frames), start).await,
            secs(3.0 * step) + SILENCE_LIMIT
        );
    }

    /// A 3.2 MB reply needs 200 s at 16 kB/s. A request the client sends at 1 s
    /// says nothing about how much of the reply it has read.
    #[tokio::test(start_paused = true)]
    async fn an_unrelated_frame_does_not_take_a_large_reply_s_time() {
        let liveness = fresh();
        let start = Instant::now();
        liveness.lock().unwrap().wrote(3_200_000, start);
        let frames = until_silent(frames_at(vec![1.0]), liveness);
        assert_eq!(
            ended_after(Box::pin(frames), start).await,
            secs(200.0) + PONG_RETURN
        );
    }

    /// 1.6 MB written at 50 s cannot have crossed before 150 s.
    #[tokio::test(start_paused = true)]
    async fn a_late_write_is_given_its_time_from_when_it_was_written() {
        let liveness = fresh();
        let start = Instant::now();
        let frames = until_silent(futures::stream::pending::<()>(), liveness.clone());
        tokio::spawn(async move {
            tokio::time::sleep(secs(50.0)).await;
            liveness.lock().unwrap().wrote(1_600_000, Instant::now());
        });
        assert_eq!(
            ended_after(Box::pin(frames), start).await,
            secs(150.0) + PONG_RETURN
        );
    }

    /// Once a ping is out, what is written after it queues behind it and cannot
    /// delay its pong: a client that stopped reading is let go even while events
    /// keep coming.
    #[tokio::test(start_paused = true)]
    async fn writes_after_an_unanswered_ping_do_not_postpone_the_end() {
        let liveness = fresh();
        let start = Instant::now();
        liveness.lock().unwrap().wrote(1_600_000, start);
        let frames = until_silent(futures::stream::pending::<()>(), liveness.clone());
        tokio::spawn(async move {
            for t in 1.. {
                tokio::time::sleep_until(start + secs(t as f64)).await;
                let mut liveness = liveness.lock().unwrap();
                // 16 kB a second, as fast as the link drains.
                liveness.wrote(16_000, Instant::now());
                if t == 20 {
                    liveness.pinged(Instant::now());
                }
            }
        });
        // At 20 s the link is busy until 120 s (100 s of reply, 20 s of events).
        assert_eq!(
            ended_after(Box::pin(frames), start).await,
            secs(120.0) + PONG_RETURN
        );
    }

    /// The writer and the reader together: a 3.2 MB reply, a request at 1 s, and
    /// the pong to the 20 s ping, which reaches the client behind the reply at
    /// 200 s and comes back half a second later. The client then goes silent.
    #[tokio::test(start_paused = true)]
    async fn a_slow_client_reading_a_large_reply_keeps_its_connection() {
        let liveness = fresh();
        let start = Instant::now();
        let (tx, rx) = mpsc::unbounded_channel::<String>();
        let sink = futures::sink::drain::<Message>();
        tokio::spawn(write_to(
            sink,
            rx,
            CancellationToken::new(),
            liveness.clone(),
        ));
        tx.send("x".repeat(3_200_000)).unwrap();
        let frames = until_silent(frames_at(vec![1.0, 200.5]), liveness);
        assert_eq!(
            ended_after(Box::pin(frames), start).await,
            secs(200.5) + SILENCE_LIMIT
        );
        drop(tx);
    }

    /// With replies queued without a break, the writer still sends a ping each
    /// interval: it is the client's chance to be heard.
    #[tokio::test(start_paused = true)]
    async fn the_writer_pings_while_replies_are_queued() {
        let start = Instant::now();
        let sent = Arc::new(std::sync::Mutex::new(Vec::<(Duration, bool)>::new()));
        let record = sent.clone();
        // Each send takes a second: a slow link.
        let sink = Box::pin(futures::sink::unfold((), move |(), msg: Message| {
            let record = record.clone();
            async move {
                tokio::time::sleep(Duration::from_secs(1)).await;
                let ping = matches!(msg, Message::Ping(_));
                record.lock().unwrap().push((start.elapsed(), ping));
                Ok::<_, std::convert::Infallible>(())
            }
        }));
        let (tx, rx) = mpsc::unbounded_channel::<String>();
        for _ in 0..100 {
            tx.send("reply".to_string()).unwrap();
        }
        let writer = tokio::spawn(write_to(sink, rx, CancellationToken::new(), fresh()));
        tokio::time::sleep(PING_INTERVAL + Duration::from_secs(2)).await;
        let first_ping = sent
            .lock()
            .unwrap()
            .iter()
            .find(|(_, ping)| *ping)
            .map(|(at, _)| *at);
        assert!(
            first_ping.is_some_and(|at| at <= PING_INTERVAL + Duration::from_secs(2)),
            "first ping at {:?}",
            first_ping
        );
        drop(tx);
        writer.abort();
    }
}
