//! Browser transport over `web_sys::WebSocket` — the browser's own socket,
//! so TLS, proxies, cookies and the handshake are the platform's job, like
//! every other web app on the page.
//!
//! Events feed an `mpsc` queue of [`InFrame`]s; sends go through
//! `send_with_str`. The `CloseEvent` carries its code, so the executor's
//! 1008 auth refusal reaches the reader exactly like the native close
//! frame. Ping/pong never appear — the browser answers them itself.
//!
//! Chosen over gloo-net / ws_stream_wasm because this is the thinnest layer
//! that still exposes the close code (gloo-net's stream ends without one;
//! ws_stream_wasm adds several deps to deliver it) — one web-sys type and
//! three callbacks map the whole surface `ws_rpc` needs.

use super::{InFrame, SendText, Socket};
use anyhow::{anyhow, Result};
use futures_channel::{mpsc, oneshot};
use futures_util::StreamExt;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CloseEvent, Event, MessageEvent, WebSocket};

/// The event queue — `mpsc` because the browser's event loop is the driver.
pub(crate) type WsStream = futures_util::stream::LocalBoxStream<'static, InFrame>;

/// The send half: owns the live socket.
pub(crate) struct WsSink {
    owned: Owned,
}

/// A `WebSocket` and the four closures registered on it, owned from the
/// moment the handlers are set — before the handshake ends. Dropping it
/// (the sink dropped, a failed handshake, or a `connect` future cancelled
/// before open) detaches every handler first, so the browser never calls
/// a function whose Rust closure is gone, then closes the socket, so no
/// connection outlives its owner.
struct Owned {
    ws: WebSocket,
    _onopen: Closure<dyn FnMut()>,
    _onmessage: Closure<dyn FnMut(MessageEvent)>,
    _onerror: Closure<dyn FnMut(Event)>,
    _onclose: Closure<dyn FnMut(CloseEvent)>,
}

impl Drop for Owned {
    fn drop(&mut self) {
        self.ws.set_onopen(None);
        self.ws.set_onmessage(None);
        self.ws.set_onerror(None);
        self.ws.set_onclose(None);
        // Valid in CONNECTING too: the handshake is abandoned.
        let _ = self.ws.close();
    }
}

impl SendText for WsSink {
    async fn send_text(&mut self, text: String) -> Result<()> {
        // `send` throws only in CONNECTING; in CLOSING/CLOSED the browser
        // drops the data silently. Only an OPEN socket may report "sent".
        let state = self.owned.ws.ready_state();
        if state != WebSocket::OPEN {
            return Err(anyhow!(
                "WebSocket send failed: the socket is {}",
                match state {
                    WebSocket::CONNECTING => "still connecting",
                    WebSocket::CLOSING => "closing",
                    _ => "closed",
                }
            ));
        }
        self.owned
            .ws
            .send_with_str(&text)
            .map_err(|e| anyhow!("WebSocket send failed: {e:?}"))
    }
}

// Dropping the send half closes the socket (`Owned`'s drop) — like dropping
// the native write half, which closes the TCP stream executor-side. This is
// what makes `disconnect_for_testing`/`close` sever a live browser socket.
// The stream half then ends: the detached handlers held its only senders.

/// Start the handshake and resolve once it ends — `Ok` on the open event,
/// `Err` on a pre-open error or close. `display_url` is the token-less URL
/// for error text.
pub(crate) async fn connect(url: &str, display_url: &str) -> Result<Socket> {
    let ws = WebSocket::new(url)
        .map_err(|e| anyhow!("WebSocket connection to {display_url} failed: {e:?}"))?;
    // The server only speaks text; accept and ignore binary by not wiring
    // it (binaryType set so a stray binary frame can't become a Blob we
    // would have to read out).
    ws.set_binary_type(web_sys::BinaryType::Arraybuffer);

    let (tx, rx) = mpsc::unbounded::<InFrame>();
    // An error/close *before* the open event fails the connect instead of
    // feeding the stream — shared between the three handlers below.
    let (open_tx, open_rx) = oneshot::channel::<Result<(), String>>();
    let open_tx = Rc::new(RefCell::new(Some(open_tx)));

    let onopen = Closure::wrap(Box::new({
        let open_tx = open_tx.clone();
        move || {
            if let Some(t) = open_tx.borrow_mut().take() {
                let _ = t.send(Ok(()));
            }
        }
    }) as Box<dyn FnMut()>);
    ws.set_onopen(Some(onopen.as_ref().unchecked_ref()));

    // A WebSocket `error` is a plain `Event`: no message, no code (the
    // browser hides network details from pages). Before open it fails the
    // handshake; after open it only announces the `close` that always
    // follows, and `onclose` owns the disconnect and its code.
    let onerror = Closure::wrap(Box::new({
        let open_tx = open_tx.clone();
        move |_: Event| {
            if let Some(t) = open_tx.borrow_mut().take() {
                let _ = t.send(Err("the handshake failed (network error)".to_string()));
            }
        }
    }) as Box<dyn FnMut(_)>);
    ws.set_onerror(Some(onerror.as_ref().unchecked_ref()));

    let onclose = Closure::wrap(Box::new({
        let tx = tx.clone();
        let open_tx = open_tx.clone();
        move |ev: CloseEvent| {
            if let Some(t) = open_tx.borrow_mut().take() {
                let _ = t.send(Err(format!("closed before open (code {})", ev.code())));
            }
            let _ = tx.unbounded_send(InFrame::Close(Some(ev.code())));
        }
    }) as Box<dyn FnMut(_)>);
    ws.set_onclose(Some(onclose.as_ref().unchecked_ref()));

    let onmessage = Closure::wrap(Box::new(move |ev: MessageEvent| {
        if let Some(text) = ev.data().as_string() {
            let _ = tx.unbounded_send(InFrame::Text(text));
        }
    }) as Box<dyn FnMut(_)>);
    ws.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));

    // Owned before the first await: every exit from here — open, failure,
    // or this future dropped mid-handshake — goes through `Owned::drop`.
    let owned = Owned {
        ws,
        _onopen: onopen,
        _onmessage: onmessage,
        _onerror: onerror,
        _onclose: onclose,
    };

    match open_rx.await {
        Ok(Ok(())) => Ok(Socket {
            sink: WsSink { owned },
            stream: rx.boxed_local(),
        }),
        Ok(Err(e)) => Err(anyhow!("WebSocket connection to {display_url} failed: {e}")),
        Err(_) => Err(anyhow!(
            "WebSocket connection to {display_url} failed: event handlers dropped"
        )),
    }
}

/// In-browser tests of this transport against the scripted peer
/// (`scripts/wasm-test-server.py`), run by `scripts/test-wasm-client.sh`.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::rt;
    use std::time::Duration;
    use wasm_bindgen_futures::JsFuture;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    fn peer_http() -> &'static str {
        option_env!("FAKE_PEER_URL")
            .expect("FAKE_PEER_URL: run through scripts/test-wasm-client.sh")
    }

    /// A `ws://` URL of the scripted peer.
    fn peer(path: &str) -> String {
        format!("{}/{path}", peer_http().replacen("http://", "ws://", 1))
    }

    /// The peer's `(accepted, open)` connection counts under `key`.
    async fn counts(key: &str) -> (u64, u64) {
        let window = web_sys::window().unwrap();
        let resp = JsFuture::from(window.fetch_with_str(&format!("{}/count/{key}", peer_http())))
            .await
            .unwrap();
        let resp: web_sys::Response = resp.dyn_into().unwrap();
        let text = JsFuture::from(resp.text().unwrap()).await.unwrap();
        let v: serde_json::Value = serde_json::from_str(&text.as_string().unwrap()).unwrap();
        (v["accepted"].as_u64().unwrap(), v["open"].as_u64().unwrap())
    }

    /// Wait until the peer holds no open connection under `key`.
    async fn wait_closed(key: &str, within: Duration) -> (u64, u64) {
        let mut waited = Duration::ZERO;
        loop {
            let c = counts(key).await;
            if c.1 == 0 || waited >= within {
                return c;
            }
            rt::sleep(Duration::from_millis(50)).await;
            waited += Duration::from_millis(50);
        }
    }

    /// Records every exception that reaches the page uncaught — a browser
    /// event handler that throws (a failed cast, a dropped closure) lands
    /// here and nowhere else.
    struct Uncaught {
        errors: Rc<RefCell<Vec<String>>>,
        handler: Closure<dyn FnMut(web_sys::Event)>,
    }

    impl Uncaught {
        fn trap() -> Self {
            let errors = Rc::new(RefCell::new(Vec::new()));
            let handler = Closure::wrap(Box::new({
                let errors = errors.clone();
                move |ev: web_sys::Event| {
                    let msg = js_sys::Reflect::get(&ev, &"message".into())
                        .ok()
                        .and_then(|m| m.as_string())
                        .unwrap_or_else(|| ev.type_());
                    errors.borrow_mut().push(msg);
                }
            }) as Box<dyn FnMut(_)>);
            web_sys::window()
                .unwrap()
                .add_event_listener_with_callback("error", handler.as_ref().unchecked_ref())
                .unwrap();
            Self { errors, handler }
        }

        /// The exceptions caught so far, after letting queued events run.
        async fn after(&self, settle: Duration) -> Vec<String> {
            rt::sleep(settle).await;
            self.errors.borrow().clone()
        }
    }

    impl Drop for Uncaught {
        fn drop(&mut self) {
            let _ = web_sys::window()
                .unwrap()
                .remove_event_listener_with_callback(
                    "error",
                    self.handler.as_ref().unchecked_ref(),
                );
        }
    }

    /// A6-2: dropping a live socket (what `close`/`disconnect_for_testing`
    /// do) closes it at the peer and leaves no handler behind for the
    /// `close` event the browser fires afterwards.
    #[wasm_bindgen_test]
    async fn dropping_a_socket_closes_it_and_detaches_every_handler() {
        let trap = Uncaught::trap();
        let key = "drop-open";
        let socket = connect(&peer(&format!("echo/{key}")), "echo")
            .await
            .unwrap();
        assert_eq!(counts(key).await, (1, 1));
        drop(socket);
        assert_eq!(wait_closed(key, Duration::from_secs(3)).await, (1, 0));
        assert_eq!(
            trap.after(Duration::from_millis(300)).await,
            Vec::<String>::new()
        );
    }

    /// A6-2: the stream half outliving the sink still ends: the socket's
    /// close is delivered, then nothing else (no handler fires later).
    #[wasm_bindgen_test]
    async fn a_dropped_sink_ends_its_stream() {
        let trap = Uncaught::trap();
        let Socket { sink, mut stream } = connect(&peer("echo/drop-sink"), "echo").await.unwrap();
        drop(sink);
        let next = rt::timeout(Duration::from_secs(3), stream.next()).await;
        assert!(
            matches!(next, Ok(None) | Ok(Some(InFrame::Close(_)))),
            "the stream did not end: {next:?}"
        );
        assert_eq!(
            trap.after(Duration::from_millis(300)).await,
            Vec::<String>::new()
        );
    }

    /// A6-2: a connect cancelled before the open event (the caller timed
    /// out, or `close` aborted the reconnect supervisor mid-dial) closes
    /// and releases its socket: the peer sees the connection end.
    #[wasm_bindgen_test]
    async fn a_handshake_cancelled_before_open_closes_its_socket() {
        let trap = Uncaught::trap();
        let key = "hang-cancel";
        let url = peer(&format!("hang/{key}"));
        let r = rt::timeout(Duration::from_millis(300), connect(&url, "hang")).await;
        assert!(r.is_err(), "the hanging handshake resolved");
        assert_eq!(counts(key).await.0, 1, "the peer never saw the dial");
        assert_eq!(wait_closed(key, Duration::from_secs(3)).await, (1, 0));
        assert_eq!(
            trap.after(Duration::from_millis(300)).await,
            Vec::<String>::new()
        );
    }

    /// A6-1: a refused TCP connect fires `error` with a plain `Event` (no
    /// `message`), then `close`. The handshake must fail with the URL in
    /// the error and no handler may throw.
    #[wasm_bindgen_test]
    async fn a_refused_connect_fails_without_an_uncaught_exception() {
        let trap = Uncaught::trap();
        let port = option_env!("CLOSED_PORT").expect("CLOSED_PORT");
        let url = format!("ws://127.0.0.1:{port}/");
        let err = match connect(&url, &url).await {
            Ok(_) => panic!("a connect to a closed port succeeded"),
            Err(e) => e.to_string(),
        };
        assert!(err.contains(&url), "the error names the URL: {err}");
        assert_eq!(
            trap.after(Duration::from_millis(300)).await,
            Vec::<String>::new()
        );
    }

    /// A6-4: the browser discards a send in CLOSING/CLOSED without
    /// throwing; the adapter must report it as failed, not sent.
    #[wasm_bindgen_test]
    async fn a_send_while_closing_or_closed_fails() {
        let Socket {
            mut sink,
            mut stream,
        } = connect(&peer("echo/closing"), "echo").await.unwrap();
        sink.send_text("before".into())
            .await
            .expect("a send while OPEN");
        assert!(matches!(stream.next().await, Some(InFrame::Text(t)) if t == "before"));

        sink.owned.ws.close().unwrap();
        assert_eq!(sink.owned.ws.ready_state(), WebSocket::CLOSING);
        let closing = sink.send_text("while closing".into()).await;
        assert!(closing.is_err(), "a send while CLOSING reported success");

        let closed = rt::timeout(Duration::from_secs(3), stream.next()).await;
        assert!(matches!(closed, Ok(Some(InFrame::Close(_)))), "{closed:?}");
        assert_eq!(sink.owned.ws.ready_state(), WebSocket::CLOSED);
        let after = sink.send_text("after close".into()).await;
        assert!(after.is_err(), "a send while CLOSED reported success");
    }
}
