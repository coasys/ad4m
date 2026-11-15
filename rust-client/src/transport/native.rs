//! Native transport: tokio + tokio-tungstenite, exactly the behaviour the
//! client always had (`connect_async_with_config` with the 256 MiB message
//! / 64 MiB frame limits), plus the loopback pin for `ws://localhost`
//! ([`loopback_dial_addrs`]). Ping, pong and binary frames are filtered into
//! the [`InFrame`] stream so the reader loop sees text, closes and errors
//! only.

use super::{InFrame, SendText, Socket};
use anyhow::{anyhow, Result};
use futures_util::{SinkExt, StreamExt};
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::protocol::WebSocketConfig;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{
    client_async_with_config, connect_async_with_config, MaybeTlsStream, WebSocketStream,
};

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

/// The loopback addresses to dial for a plain `ws://localhost` URL, so the
/// name never goes to the resolver: `localhost` is exempt from the plaintext
/// refusal by name (connect's `plaintext_remote`), so it must also be
/// loopback by destination (N1). `None`: dial as usual.
pub(crate) fn loopback_dial_addrs(url: &str) -> Option<Vec<std::net::SocketAddr>> {
    let url = reqwest::Url::parse(url).ok()?;
    if url.scheme() != "ws" || !url.host_str()?.eq_ignore_ascii_case("localhost") {
        return None;
    }
    let port = url.port_or_known_default()?;
    Some(vec![
        std::net::SocketAddr::from((std::net::Ipv4Addr::LOCALHOST, port)),
        std::net::SocketAddr::from((std::net::Ipv6Addr::LOCALHOST, port)),
    ])
}

/// Test hook: the destination every [`connect`] dial used, in order —
/// `Some(addrs)` for a loopback-pinned dial (the literal SocketAddrs the
/// TCP connect went to; the name never reached the resolver), `None` for
/// a dial through the resolver. The reconnect test asserts both the first
/// connect and the supervisor's redial take the pin: a dial that bypasses
/// this function leaves no record at all.
#[cfg(test)]
static DIAL_LOG: std::sync::Mutex<Vec<(String, Option<Vec<std::net::SocketAddr>>)>> =
    std::sync::Mutex::new(Vec::new());

#[cfg(test)]
fn record_dial(url: &str, addrs: Option<Vec<std::net::SocketAddr>>) {
    DIAL_LOG.lock().unwrap().push((url.to_string(), addrs));
}

/// The dial records for `url` (each [`connect`] records against the URL it
/// was given — token included), in the order they happened.
#[cfg(test)]
pub(crate) fn dialed_addrs(url: &str) -> Vec<Option<Vec<std::net::SocketAddr>>> {
    DIAL_LOG
        .lock()
        .unwrap()
        .iter()
        .filter(|(u, _)| u == url)
        .map(|(_, addrs)| addrs.clone())
        .collect()
}

/// Open the socket and split it. `display_url` is the same URL without the
/// capability token — safe to print inside the error.
pub(crate) async fn connect(url: &str, display_url: &str) -> Result<Socket> {
    let opened = match loopback_dial_addrs(url) {
        // The handshake keeps the URL (Host: localhost); only the TCP
        // destination is pinned.
        Some(addrs) => {
            #[cfg(test)]
            record_dial(url, Some(addrs.clone()));
            match TcpStream::connect(&addrs[..]).await {
                Ok(tcp) => {
                    client_async_with_config(url, MaybeTlsStream::Plain(tcp), Some(ws_config()))
                        .await
                        .map_err(|e| e.to_string())
                }
                Err(e) => Err(e.to_string()),
            }
        }
        None => {
            #[cfg(test)]
            record_dial(url, None);
            connect_async_with_config(url, Some(ws_config()), false)
                .await
                .map_err(|e| e.to_string())
        }
    };
    let (ws_stream, _) =
        opened.map_err(|e| anyhow!("WebSocket connection to {} failed: {}", display_url, e))?;
    let (sink, read) = ws_stream.split();
    let stream = read
        .filter_map(|item| async {
            match item {
                Ok(Message::Text(t)) => Some(InFrame::Text(t)),
                Ok(Message::Close(frame)) => Some(InFrame::Close {
                    code: frame.as_ref().map(|f| u16::from(f.code)),
                    reason: frame.map(|f| f.reason.to_string()).unwrap_or_default(),
                }),
                Err(e) => Some(InFrame::Error(e.to_string())),
                // Ping/Pong/Binary — ping is answered by the transport.
                _ => None,
            }
        })
        .boxed();
    Ok(Socket { sink, stream })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// N1 (round 3): plain `ws://localhost` dials 127.0.0.1 and ::1, never
    /// the resolver; other hosts and TLS dial as usual.
    #[test]
    fn localhost_dials_loopback_literals() {
        use std::net::{Ipv4Addr, Ipv6Addr, SocketAddr};
        let both = Some(vec![
            SocketAddr::from((Ipv4Addr::LOCALHOST, 12000)),
            SocketAddr::from((Ipv6Addr::LOCALHOST, 12000)),
        ]);
        assert_eq!(
            loopback_dial_addrs("ws://localhost:12000/api/v1/ws?token=t"),
            both
        );
        assert_eq!(loopback_dial_addrs("ws://LocalHost:12000/api/v1/ws"), both);
        assert_eq!(
            loopback_dial_addrs("ws://localhost/api/v1/ws"),
            Some(vec![
                SocketAddr::from((Ipv4Addr::LOCALHOST, 80)),
                SocketAddr::from((Ipv6Addr::LOCALHOST, 80)),
            ])
        );
        assert_eq!(loopback_dial_addrs("ws://localhost.evil.example:1/"), None);
        assert_eq!(loopback_dial_addrs("ws://127.0.0.1:12000/"), None);
        assert_eq!(loopback_dial_addrs("wss://localhost:12000/"), None);
    }
}
