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
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CloseEvent, Event, MessageEvent, WebSocket};

/// The largest message either way: the native transport's
/// `max_message_size` (256 MiB). A bigger send is refused before it is
/// queued; a bigger inbound message ends the stream with an error.
#[cfg(not(test))]
const MAX_MESSAGE_BYTES: usize = 256 << 20;
/// `send` only queues (it never waits for the network), so a peer that
/// stays connected but stops reading would grow `bufferedAmount` without
/// limit. A send is refused once more than this is queued: the queue holds
/// at most this plus one message. 16 MiB is many seconds of any normal RPC
/// traffic, and the native write path has the same idea in its 30 s write
/// budget (`CALL_SEND_TIMEOUT`).
#[cfg(not(test))]
const MAX_BUFFERED_BYTES: usize = 16 << 20;
/// Inbound frames are queued between the browser's `message` events and
/// the reader. The reader normally drains them at once; if it stalls, the
/// queue holds at most this many bytes (each frame counted at least
/// `FRAME_OVERHEAD`), then the stream ends with an error and the socket
/// closes, so the client reconnects instead of growing without bound.
#[cfg(not(test))]
const MAX_INBOUND_BYTES: usize = 256 << 20;
// The tests use small bounds, so they need no hundreds of MiB.
#[cfg(test)]
const MAX_MESSAGE_BYTES: usize = 1 << 20;
#[cfg(test)]
const MAX_BUFFERED_BYTES: usize = 1 << 20;
#[cfg(test)]
const MAX_INBOUND_BYTES: usize = 1 << 20;
/// What a queued frame costs beyond its text (channel node, `String`).
const FRAME_OVERHEAD: usize = 64;

/// The event queue — `mpsc` because the browser's event loop is the driver;
/// bounded in bytes by `onmessage` (`MAX_INBOUND_BYTES`).
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
        if text.len() > MAX_MESSAGE_BYTES {
            return Err(anyhow!(
                "WebSocket send failed: message too large ({} bytes, limit {MAX_MESSAGE_BYTES})",
                text.len()
            ));
        }
        let queued = self.owned.ws.buffered_amount() as usize;
        if queued > MAX_BUFFERED_BYTES {
            return Err(anyhow!(
                "WebSocket send failed: {queued} bytes still queued, the peer is not reading \
                 (limit {MAX_BUFFERED_BYTES})"
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

    // Bytes queued for the reader: added here, taken off as the stream
    // yields each frame.
    let queued = Rc::new(Cell::new(0usize));
    let onmessage = Closure::wrap(Box::new({
        let queued = queued.clone();
        let ws = ws.clone();
        let mut overflowed = false;
        move |ev: MessageEvent| {
            if overflowed {
                return;
            }
            let Some(text) = ev.data().as_string() else {
                return;
            };
            let cost = text.len().max(FRAME_OVERHEAD);
            let error = if text.len() > MAX_MESSAGE_BYTES {
                Some(format!(
                    "WebSocket message too large ({} bytes, limit {MAX_MESSAGE_BYTES})",
                    text.len()
                ))
            } else if queued.get() + cost > MAX_INBOUND_BYTES {
                Some(format!(
                    "WebSocket inbound queue full ({} bytes waiting for the reader, \
                     limit {MAX_INBOUND_BYTES})",
                    queued.get()
                ))
            } else {
                None
            };
            match error {
                Some(e) => {
                    // The stream ends here: the reader disconnects on the
                    // error, and nothing more is queued for it.
                    overflowed = true;
                    let _ = tx.unbounded_send(InFrame::Error(e));
                    tx.close_channel();
                    let _ = ws.close();
                }
                None => {
                    queued.set(queued.get() + cost);
                    let _ = tx.unbounded_send(InFrame::Text(text));
                }
            }
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
            stream: rx
                .map(move |frame| {
                    if let InFrame::Text(t) = &frame {
                        queued.set(queued.get() - t.len().max(FRAME_OVERHEAD));
                    }
                    frame
                })
                .boxed_local(),
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
        // Firefox keeps the TCP connection of a socket closed while
        // CONNECTING open until the peer answers or its own timeout: a raw
        // `WebSocket` closed the same way stays open at the peer for 10 s+
        // there (closed at once in Chromium). The page cannot do more than
        // `close()`, so on Firefox only the handlers are checked.
        let ua = web_sys::window().unwrap().navigator().user_agent().unwrap();
        if ua.contains("Firefox") {
            web_sys::console::warn_1(
                &"Firefox: a socket closed while CONNECTING keeps its TCP connection; \
                  the peer-side check runs in Chromium"
                    .into(),
            );
        } else {
            assert_eq!(wait_closed(key, Duration::from_secs(3)).await, (1, 0));
        }
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

    /// The test build's bounds (1 MiB each; see the constants).
    const MIB: usize = 1 << 20;

    /// A6-3: a peer that stays connected but stops reading. Sends must
    /// start failing with a clear error once the browser's queue is past
    /// the bound, instead of queueing without limit; the socket still
    /// shuts down at once.
    #[wasm_bindgen_test]
    async fn sends_to_a_peer_that_stops_reading_fail_at_the_bound() {
        let Socket {
            mut sink,
            mut stream,
        } = connect(&peer("silent/slow-peer"), "silent").await.unwrap();
        let chunk = "x".repeat(256 * 1024);
        let mut refused = None;
        // 400 × 256 KiB = 100 MiB, far past any kernel buffer.
        for i in 0..400 {
            if let Err(e) = sink.send_text(chunk.clone()).await {
                refused = Some((i, e.to_string()));
                break;
            }
            rt::yield_now().await;
        }
        let queued = sink.owned.ws.buffered_amount() as usize;
        let (i, err) =
            refused.unwrap_or_else(|| panic!("400 sends queued, {queued} bytes buffered"));
        assert!(
            err.contains("not reading"),
            "unclear error after {i} sends: {err}"
        );
        assert!(
            queued <= MIB + chunk.len(),
            "{queued} bytes queued past the bound"
        );

        drop(sink);
        let ended = rt::timeout(Duration::from_secs(1), stream.next()).await;
        assert!(matches!(ended, Ok(None)), "shutdown stalled: {ended:?}");
    }

    /// A6-3: a message over the size bound is refused before it is queued.
    #[wasm_bindgen_test]
    async fn an_oversized_send_is_refused_before_it_is_queued() {
        let Socket { mut sink, .. } = connect(&peer("echo/oversized"), "echo").await.unwrap();
        let err = sink
            .send_text("y".repeat(MIB + 1))
            .await
            .expect_err("an oversized send was queued");
        assert!(err.to_string().contains("too large"), "{err}");
        assert_eq!(sink.owned.ws.buffered_amount(), 0);
    }

    /// A6-3: frames arriving faster than the reader takes them are queued
    /// only up to the bound: past it the stream reports an error, then
    /// ends, and the socket is closed.
    #[wasm_bindgen_test]
    async fn an_inbound_flood_past_the_bound_ends_the_stream() {
        let key = "flood";
        // 64 × 64 KiB = 4 MiB, sent while nobody reads the stream.
        let Socket { sink, mut stream } = connect(&peer(&format!("flood/64/65536/{key}")), "flood")
            .await
            .unwrap();
        rt::sleep(Duration::from_millis(1500)).await;
        let mut texts = 0usize;
        let mut error = None;
        while let Ok(Some(frame)) = rt::timeout(Duration::from_secs(1), stream.next()).await {
            match frame {
                InFrame::Text(t) => texts += t.len(),
                InFrame::Error(e) => {
                    error = Some(e);
                    break;
                }
                InFrame::Close(_) => break,
            }
        }
        assert!(texts <= MIB, "{texts} bytes were queued past the bound");
        let error = error.unwrap_or_else(|| panic!("no overflow error after {texts} bytes"));
        assert!(error.contains("inbound"), "{error}");
        assert_eq!(wait_closed(key, Duration::from_secs(3)).await.1, 0);
        drop(sink);
    }

    /// A6-3: one inbound message over the size bound (native: 256 MiB) is
    /// an error, not a queued frame.
    #[wasm_bindgen_test]
    async fn an_oversized_inbound_message_is_an_error() {
        let Socket {
            sink: _sink,
            mut stream,
        } = connect(&peer(&format!("flood/1/{}/big-inbound", MIB + 1)), "flood")
            .await
            .unwrap();
        let first = rt::timeout(Duration::from_secs(3), stream.next()).await;
        assert!(
            matches!(&first, Ok(Some(InFrame::Error(e))) if e.contains("too large")),
            "{:?}",
            first.map(|f| f.map(|f| match f {
                InFrame::Text(t) => format!("Text({} bytes)", t.len()),
                other => format!("{other:?}"),
            }))
        );
    }
}
