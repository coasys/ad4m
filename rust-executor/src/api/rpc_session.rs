//! One RPC connection, apart from the socket that carries it: [`Connection`] and [`serve`].
//!
//! A connection takes text frames in (a stream) and puts text frames out (a channel), so the
//! same loop serves the axum WebSocket (`ws_rpc.rs`), the tests that drive it through
//! channels, and the web executor's Worker (`web/session.rs`, frames over `postMessage`).
//! What one connection holds lives here: the request loop, `request.cancel`, the
//! `events.watch` filter, the live queries bound to its `connection_id`, and its query slots.
//!
//! The frames are those of `ws_rpc.rs`'s module doc.

use futures::stream::StreamExt;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use tokio_util::sync::CancellationToken;

use crate::agent::capabilities::track_last_seen;
use crate::types::RequestContext;

use super::event_interest;
use super::ws_handler::{HandlerMap, WsRpcError};
use super::ws_rpc::token_ended_frame;

/// Per-connection registry of in-flight request ids → cancellation
/// tokens.  A client cancels an in-flight request by sending
/// `{"id": "<some-cancel-id>", "type": "request.cancel",
///   "params": {"targetId": "<original-request-id>"}}`.
/// The dispatcher cancels the matching tokens: `.0` drops the call's
/// future, `.1` is its `ctx.cancel_token`, which stops its eval.
type InflightRegistry = Arc<Mutex<HashMap<String, (CancellationToken, CancellationToken)>>>;

pub(crate) type EventStream = std::pin::Pin<Box<dyn futures::Stream<Item = String> + Send>>;

/// Per-connection state: what the reader needs to answer a message.
pub(crate) struct Connection {
    handler_map: Arc<HandlerMap>,
    /// Carries the session that every request checks again.
    ctx: Arc<RequestContext>,
    /// Fires when the token stops working: a failed check, the end of its grant (a child of
    /// the grant's token, so a revocation closes the connection at once) or its expiry
    /// ([`Session::over`](crate::agent::session::Session::over), watched by `serve`). It
    /// cancels the calls in flight, and the connection then closes, so the client
    /// authenticates again instead of waiting on a socket that answers nothing.
    pub(crate) session_over: CancellationToken,
    tx: mpsc::UnboundedSender<String>,
    inflight: InflightRegistry,
    interest: event_interest::SharedInterest,
    /// Dispatched calls still running.
    calls: std::sync::Mutex<tokio::task::JoinSet<()>>,
    /// Fires when the connection closes (and with `session_over`, its parent). Every call's
    /// `ctx.cancel_token` is a child of it, so closing the socket stops the eval of any
    /// query still running — its first result has nobody left to receive it, and an
    /// abandoned one kept a blocking thread busy for minutes (#1595). A call that opens
    /// a live query drops with it; other calls still finish, so a write the client sent
    /// completes, and a subscribe that lands after the close is swept as its call ends.
    closed: CancellationToken,
    /// Bounds how many calls that open a live query one connection runs at once; the rest
    /// wait their turn. A client that gives up on its subscribe call and asks again (the waker
    /// retried every 30 s) piled up one first-result computation per attempt: ~200 busy
    /// threads on staging (#1595).
    opening: Arc<tokio::sync::Semaphore>,
    /// Bounds how many query-running calls one connection evaluates at once (#1736):
    /// `perspective.querySparql`, `perspective.getLinks`, `perspective.modelQuery`,
    /// `perspective.evaluateGetters`, `perspective.subscribeQuery` and
    /// `perspective.modelSubscribe`. A call acquires one slot in
    /// `sparql_store::query_gate`, which hangs the permit on the eval's stop:
    /// the slot frees only when the blocking evaluation has actually ended,
    /// not when the call's future ends.
    queries: Arc<tokio::sync::Semaphore>,
}

/// How many calls that open a live query one connection runs at once (see
/// [`Connection::opening`]).
pub(crate) const MAX_OPENING_LIVE_QUERIES: usize = 8;

/// How many SPARQL evaluations one connection runs at once (see
/// [`Connection::queries`]): 8, the same bound the opening cap already set —
/// each Oxigraph eval is a blocking thread, so 8 keeps one connection from
/// taking every blocking slot the way 83 mention re-checks did on staging.
pub(crate) const MAX_RUNNING_QUERIES_PER_CONNECTION: usize = 8;

impl Connection {
    /// A connection whose replies and events go to `tx`.
    pub(crate) fn new(
        handler_map: Arc<HandlerMap>,
        ctx: Arc<RequestContext>,
        tx: mpsc::UnboundedSender<String>,
    ) -> Self {
        // A grant revoked before this line has already fired, so its child starts cancelled:
        // no window in which a revoked token's connection stays open.
        let session_over = ctx.session.ended();
        let closed = session_over.child_token();
        Self {
            handler_map,
            ctx,
            session_over,
            tx,
            inflight: Default::default(),
            interest: Default::default(),
            calls: Default::default(),
            closed,
            opening: Arc::new(tokio::sync::Semaphore::new(MAX_OPENING_LIVE_QUERIES)),
            queries: Arc::new(tokio::sync::Semaphore::new(
                MAX_RUNNING_QUERIES_PER_CONNECTION,
            )),
        }
    }

    /// The events this connection asked for with `events.watch`: its event stream reads it.
    pub(crate) fn interest(&self) -> event_interest::SharedInterest {
        self.interest.clone()
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
            let cancelled = if let Some((drop_token, ctx_token)) = guard.remove(&target_id) {
                drop_token.cancel();
                ctx_token.cancel();
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

        // Allocate the request's CancellationTokens and stash them in
        // the registry under the request id.  If the client sends
        // `request.cancel`, the drop token's race fires immediately and we
        // reply with an `AbortError` (code 499).
        let opens_live_query = self.handler_map.opens(&msg_type);
        // Two tokens, two jobs:
        // - `drop_token` drops the call's future in the select below, on
        //   `request.cancel` or the session ending — and, for a call that
        //   opens a live query, on the connection closing too (#1595: its
        //   first result has nobody left to receive it). Other calls still
        //   finish once the socket closes: their evals end on `ctx_token`,
        //   and a subscribe that still completes is swept as the call ends
        //   (the dispose loop in `serve`).
        // - `ctx_token` (a `closed` child) rides `req_ctx` into the eval gate:
        //   a `request.cancel`, the connection closing, a revocation or an
        //   expiry stops the call's eval — for every query-running method, not
        //   only live-query openers (#1736). `request.cancel` cancels both.
        let drop_token = if opens_live_query {
            self.closed.child_token()
        } else {
            self.session_over.child_token()
        };
        let ctx_token = self.closed.child_token();
        let opening = opens_live_query.then(|| self.opening.clone());
        let queries = self.queries.clone();
        {
            let mut guard = self.inflight.lock().await;
            guard.insert(id.clone(), (drop_token.clone(), ctx_token.clone()));
        }

        // An event that names a member twice, which `params` (a `Value`) no longer shows.
        let refused = super::identity_ws::duplicate_member_refusal(&msg_type, text);
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
            req_ctx.cancel_token = Some(ctx_token.clone());
            req_ctx.query_slots = Some(queries);
            let req_ctx = Arc::new(req_ctx);

            // Race the handler against a `request.cancel` or the session
            // ending.  On cancel, we drop the handler future — its eval
            // gate's drop stops the work it was doing (e.g. a
            // `spawn_blocking` SPARQL eval inside Oxigraph) — the network
            // reply is skipped and the first select arm fires first.
            // A socket that merely closed does not fire `drop_token`:
            // `ctx_token` still stops the call's eval, and the call ends
            // with it.
            let response = tokio::select! {
                biased;
                _ = drop_token.cancelled() => {
                    if session_over.is_cancelled() {
                        json!({"id": id, "error": {"code": 401, "message": token_ended_frame().reason.as_str()}})
                    } else {
                        // 499 mirrors nginx's "Client Closed Request" status —
                        // the OpenAPI SDKs and most HTTP clients recognise it.
                        json!({"id": id, "error": {"code": 499, "message": "Request cancelled by client"}})
                    }
                }
                result = async {
                    // Waits for a turn, cancellably: `request.cancel` drops the
                    // wait with the call's future; the connection closing ends
                    // it on `ctx_token` — a queued live query has nobody left
                    // to receive its first result.
                    let _turn = match opening {
                        Some(opening) => match tokio::select! {
                            p = opening.acquire_owned() => p.ok(),
                            _ = ctx_token.cancelled() => None,
                        } {
                            Some(permit) => Some(permit),
                            None => {
                                return Err(WsRpcError {
                                    data: None,
                                    code: 499,
                                    message: "Request cancelled by client".to_string(),
                                });
                            }
                        },
                        None => None,
                    };
                    match refused {
                        // The access answer first: a caller the guard refuses learns nothing
                        // of its frame.
                        Some(e) => handler_map
                            .admit(&msg_type, &params, &req_ctx)
                            .await
                            .and(Err(e)),
                        None => handler_map.dispatch(&msg_type, params, req_ctx).await,
                    }
                } => {
                    match result {
                        Ok(val) => json!({"id": id, "result": val}),
                        Err(e) => json!({"id": id, "error": e.to_wire()}),
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
/// answer each client message and forward `events`, a stream built with the
/// connection's [`Connection::interest`] and its `connection_id`. When the
/// client is gone the event task stops, the connection's subscriptions end,
/// and calls still in flight finish and reply to `tx`. `events` ends when the
/// token stops working (see `build_event_stream`), and that ends the session
/// too.
pub(crate) async fn serve<S>(conn: Connection, incoming: S, mut events: EventStream)
where
    S: futures::Stream<Item = String>,
{
    let tx_events = conn.tx.clone();
    let event_session_over = conn.session_over.clone();
    let event_task = tokio::spawn(async move {
        while let Some(msg) = events.next().await {
            if tx_events.send(msg).is_err() {
                return;
            }
        }
        // The stream ends when the token stops working.
        event_session_over.cancel();
    });

    let over = conn.ctx.session.over();
    tokio::pin!(incoming, over);
    loop {
        let text = tokio::select! {
            _ = conn.session_over.cancelled() => break,
            _ = &mut over => {
                conn.session_over.cancel();
                break;
            }
            text = incoming.next() => text,
        };
        let Some(text) = text else { break };
        conn.handle_text(&text).await;
    }

    event_task.abort();
    let _ = event_task.await;
    // Nobody receives the first result of a live query still opening.
    conn.closed.cancel();

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
