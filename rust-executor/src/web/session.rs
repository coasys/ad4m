//! One RPC connection of the web executor: [`connect`]. The Worker opens one per `WorkerSocket`
//! and feeds it the socket's text frames from `postMessage`.
//!
//! The loop is native's own (`api::rpc_session`: `Connection` + `serve`), so `ping`,
//! `request.cancel`, the `events.watch` filter, the live queries bound to the connection and
//! its query slots behave as on the axum socket. Events come from `build_event_stream`, per
//! connection, as `ws_rpc.rs` builds them.
//!
//! Each connection is the operator, with no token (decision 6): `RequestContext::open("",
//! &None, Loopback)`, as `boot` opens it, with a fresh `connection_id`.
//!
//! No JS type is named here: the `web-executor` crate wraps it.

use std::sync::Arc;

use futures::channel::mpsc as futures_mpsc;

use crate::agent::capabilities::ListenerReach;
use crate::api::events_ws::build_event_stream;
use crate::api::rpc_session::{serve, Connection, EventStream};
use crate::types::RequestContext;

/// The page's end of one connection: frames in with [`send`](Self::send), and
/// [`close`](Self::close) (or a drop) to end it.
pub struct Socket {
    input: futures_mpsc::UnboundedSender<String>,
    connection_id: String,
}

impl Socket {
    /// Hands one client frame (`{"id", "type", "params"}`, `{"type": "ping"}`, ...) to the
    /// loop. `false` once the connection has ended.
    pub fn send(&self, frame: String) -> bool {
        self.input.unbounded_send(frame).is_ok()
    }

    /// Ends the connection as a closed socket does: its live queries end, and calls still in
    /// flight finish and their replies still go out.
    pub fn close(&self) {
        self.input.close_channel();
    }

    /// The connection's id: its live queries are bound to it.
    pub fn connection_id(&self) -> &str {
        &self.connection_id
    }
}

impl Drop for Socket {
    fn drop(&mut self) {
        self.close();
    }
}

/// Opens a connection. Every frame the loop sends (replies, `pong`, events) goes to
/// `on_frame` as its text. `on_closed` runs once, after the loop has ended and the last reply
/// has gone out. Fails before `boot`.
pub fn connect(
    on_frame: impl Fn(String) + 'static,
    on_closed: impl FnOnce() + 'static,
) -> Result<Socket, String> {
    let handlers = super::boot::handlers().ok_or("the web executor is not booted")?;
    let mut ctx = RequestContext::open("", &None, ListenerReach::Loopback)
        .map_err(|e| format!("operator session: {e}"))?;
    let connection_id = uuid::Uuid::new_v4().to_string();
    // Live queries opened on this connection belong to it and end when it closes.
    ctx.connection_id = Some(connection_id.clone());
    let ctx = Arc::new(ctx);

    let (input, incoming) = futures_mpsc::unbounded::<String>();
    let (tx, mut out) = tokio::sync::mpsc::unbounded_channel::<String>();
    crate::rt::spawn(async move {
        while let Some(frame) = out.recv().await {
            on_frame(frame);
        }
        on_closed();
    });
    crate::rt::spawn(async move {
        let conn = Connection::new(handlers, ctx.clone(), tx);
        let events: EventStream = if ctx.session.is_authenticated() {
            build_event_stream(
                ctx.session.clone(),
                conn.interest(),
                ctx.connection_id.clone(),
            )
            .await
        } else {
            Box::pin(futures::stream::pending())
        };
        serve(conn, incoming, events).await;
    });
    Ok(Socket {
        input,
        connection_id,
    })
}
