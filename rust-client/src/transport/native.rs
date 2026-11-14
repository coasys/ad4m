//! Native transport: tokio + tokio-tungstenite, exactly the behaviour the
//! client always had (`connect_async_with_config` with the 256 MiB message
//! / 64 MiB frame limits). Ping, pong and binary frames are filtered into
//! the [`InFrame`] stream so the reader loop sees text, closes and errors
//! only.

use super::{InFrame, SendText, Socket};
use anyhow::{anyhow, Result};
use futures_util::{SinkExt, StreamExt};
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::protocol::WebSocketConfig;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async_with_config, MaybeTlsStream, WebSocketStream};

/// The write half of the tungstenite socket.
pub(crate) type WsSink =
    futures_util::stream::SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>;
/// The read half, mapped to [`InFrame`]s.
pub(crate) type WsStream = futures_util::stream::BoxStream<'static, InFrame>;

impl SendText for WsSink {
    async fn send_text(&mut self, text: String) -> Result<()> {
        self.send(Message::Text(text))
            .await
            .map_err(|e| anyhow!("{e}"))
    }
}

fn ws_config() -> WebSocketConfig {
    WebSocketConfig {
        max_message_size: Some(256 * 1024 * 1024),
        max_frame_size: Some(64 * 1024 * 1024),
        ..Default::default()
    }
}

/// Open the socket and split it. `display_url` is the same URL without the
/// capability token — safe to print inside the error.
pub(crate) async fn connect(url: &str, display_url: &str) -> Result<Socket> {
    let (ws_stream, _) = connect_async_with_config(url, Some(ws_config()), false)
        .await
        .map_err(|e| anyhow!("WebSocket connection to {} failed: {}", display_url, e))?;
    let (sink, read) = ws_stream.split();
    let stream = read
        .filter_map(|item| async {
            match item {
                Ok(Message::Text(t)) => Some(InFrame::Text(t)),
                Ok(Message::Close(frame)) => Some(InFrame::Close(frame.map(|f| u16::from(f.code)))),
                Err(e) => Some(InFrame::Error(e.to_string())),
                // Ping/Pong/Binary — ping is answered by the transport.
                _ => None,
            }
        })
        .boxed();
    Ok(Socket { sink, stream })
}
