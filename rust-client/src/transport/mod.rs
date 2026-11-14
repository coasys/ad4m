//! The WebSocket transport — the only part of the client that differs
//! between native and browser builds.
//!
//! The target selects one backend: `native` is tokio + tokio-tungstenite
//! (unchanged behaviour), `browser` is the browser's own `WebSocket` via
//! web-sys. `ws_rpc` sees one shape either way: [`connect`] returns a
//! [`Socket`] — a text-frame sink plus a stream of [`InFrame`]s — and all
//! RPC/pending/reconnect/live-query code above it is one implementation.
//!
//! The browser needs no capability a native socket has and this interface
//! lacks: text sends, a stream of text frames, and the close code (the
//! executor's 1008 auth refusal). Ping/pong are answered by the transport
//! itself on both targets, so they never surface as frames.

use anyhow::Result;

/// One incoming event the reader loop cares about.
#[derive(Debug)]
pub(crate) enum InFrame {
    Text(String),
    /// Server-initiated close; the code identifies an auth refusal (1008).
    Close(Option<u16>),
    /// A dead-socket read error.
    Error(String),
}

/// The send half of either transport: text frames only — the RPC protocol
/// never sends binary.
pub(crate) trait SendText {
    async fn send_text(&mut self, text: String) -> Result<()>;
}

/// A connected socket, split for the reader task and the call path.
pub(crate) struct Socket {
    pub sink: WsSink,
    pub stream: WsStream,
}

#[cfg(not(target_arch = "wasm32"))]
mod native;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native::{connect, WsSink, WsStream};

#[cfg(target_arch = "wasm32")]
mod browser;
#[cfg(target_arch = "wasm32")]
pub(crate) use browser::{connect, WsSink, WsStream};
