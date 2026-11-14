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
use web_sys::{CloseEvent, ErrorEvent, MessageEvent, WebSocket};

/// The event queue — `mpsc` because the browser's event loop is the driver.
pub(crate) type WsStream = futures_util::stream::LocalBoxStream<'static, InFrame>;

/// The send half: the live `WebSocket`, plus the event closures registered
/// on it. The closures must outlive the socket's use — they are stored
/// here, so dropping the sink unregisters them and closes the socket.
pub(crate) struct WsSink {
    ws: WebSocket,
    _onopen: Closure<dyn FnMut()>,
    _onmessage: Closure<dyn FnMut(MessageEvent)>,
    _onerror: Closure<dyn FnMut(ErrorEvent)>,
    _onclose: Closure<dyn FnMut(CloseEvent)>,
}

impl SendText for WsSink {
    async fn send_text(&mut self, text: String) -> Result<()> {
        self.ws
            .send_with_str(&text)
            .map_err(|e| anyhow!("WebSocket send failed: {e:?}"))
    }
}

impl Drop for WsSink {
    /// Dropping the send half closes the socket — like dropping the native
    /// write half (which closes the TCP stream executor-side). This is what
    /// makes `disconnect_for_testing`/`close` sever a live browser socket.
    fn drop(&mut self) {
        let _ = self.ws.close();
    }
}

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

    let onerror = Closure::wrap(Box::new({
        let tx = tx.clone();
        let open_tx = open_tx.clone();
        move |ev: ErrorEvent| {
            let msg = ev.message();
            if let Some(t) = open_tx.borrow_mut().take() {
                let _ = t.send(Err(format!("error during handshake: {msg}")));
            } else {
                let _ = tx.unbounded_send(InFrame::Error(format!("WebSocket error: {msg}")));
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

    match open_rx.await {
        Ok(Ok(())) => Ok(Socket {
            sink: WsSink {
                ws,
                _onopen: onopen,
                _onmessage: onmessage,
                _onerror: onerror,
                _onclose: onclose,
            },
            stream: rx.boxed_local(),
        }),
        Ok(Err(e)) => Err(anyhow!("WebSocket connection to {display_url} failed: {e}")),
        Err(_) => Err(anyhow!(
            "WebSocket connection to {display_url} failed: event handlers dropped"
        )),
    }
}
