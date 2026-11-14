//! Test-only scaffolding: a scripted executor peer.
//!
//! [`FakeExecutor`] is a loopback WebSocket server that hands the test every
//! frame the client sent (`next_request`) and writes back whatever the test
//! queues (`respond`, `reply`, `push`). All synchronisation is explicit —
//! replies, connection epochs and socket drops go through channels, so a
//! test orchestrates the wire exactly, with no sleeps that could race.
//!
//! A miniature link store ([`LinkStore`]) backs `perspective.queryLinks`,
//! `perspective.addLink` and `perspective.linkMutations` for tests that need
//! to assert what a call wrote rather than that it was sent.

#![cfg(test)]

use futures_util::{SinkExt, StreamExt};
use serde_json::{json, Value};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::net::TcpListener;
use tokio::sync::{mpsc, watch, Mutex};
use tokio_tungstenite::tungstenite::Message;

use crate::types::{LinkExpression, LinkExpressionInput, LinkInput};

/// The guard on every blocking wait — generous enough to never flake, short
/// enough that a wedged test still fails rather than hang the suite.
const WAIT: Duration = Duration::from_secs(10);

pub(crate) struct FakeExecutor {
    /// Every client frame received, in wire order (parsed JSON).
    incoming: Mutex<mpsc::UnboundedReceiver<Value>>,
    /// Frames to write to the live socket — one channel per connection,
    /// swapped on accept so a queued frame never strands in a dead socket's
    /// task. `None` while no socket is live.
    outgoing: Mutex<Option<mpsc::UnboundedSender<Value>>>,
    /// Bump to sever the live socket; the accept loop keeps listening.
    /// The payload pairs a drop counter with an optional close code —
    /// `None` sends a bare `Close`, `Some(1008)` a policy (auth) one.
    drop_conn: watch::Sender<(u64, Option<u16>)>,
    /// Counts accepted sockets: epoch 1 is the initial `connect`.
    epoch: Arc<AtomicU64>,
    /// Signalled each time a socket is accepted, carrying the new epoch.
    accepted: Mutex<mpsc::UnboundedReceiver<u64>>,
    url: String,
}

impl FakeExecutor {
    /// Bind a loopback port and serve frames until dropped.
    pub async fn start() -> Arc<Self> {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        let (incoming_tx, incoming_rx) = mpsc::unbounded_channel();
        let (drop_tx, _) = watch::channel((0u64, None));
        let (accepted_tx, accepted_rx) = mpsc::unbounded_channel();
        let epoch = Arc::new(AtomicU64::new(0));

        let server = Arc::new(Self {
            incoming: Mutex::new(incoming_rx),
            outgoing: Mutex::new(None),
            drop_conn: drop_tx.clone(),
            epoch: epoch.clone(),
            accepted: Mutex::new(accepted_rx),
            url: format!("http://127.0.0.1:{port}"),
        });

        let server2 = server.clone();
        tokio::spawn(async move {
            while let Ok((stream, _)) = listener.accept().await {
                let e = epoch.fetch_add(1, Ordering::SeqCst) + 1;
                // Route server→client frames to this socket.
                let (tx, rx) = mpsc::unbounded_channel();
                *server2.outgoing.lock().await = Some(tx);
                let _ = accepted_tx.send(e);
                tokio::spawn(serve_conn(
                    stream,
                    incoming_tx.clone(),
                    rx,
                    drop_tx.clone(),
                    e,
                ));
            }
        });
        server
    }

    /// `WsRpcClient::connect(&server.url(), "")`.
    pub fn url(&self) -> &str {
        &self.url
    }

    /// The next frame the client sent. Panics after [`WAIT`] — a test that
    /// expected a call that never came fails here, not on a hang.
    pub async fn next_request(&self) -> Value {
        tokio::time::timeout(WAIT, async { self.incoming.lock().await.recv().await })
            .await
            .expect("timed out waiting for a client frame")
            .expect("client frame channel closed")
    }

    /// Like [`next_request`](Self::next_request) but `None` when nothing
    /// arrived within `within` — for asserting a call did *not* happen.
    #[allow(dead_code)] // the reconnect findings use this
    pub async fn maybe_request(&self, within: Duration) -> Option<Value> {
        tokio::time::timeout(within, async { self.incoming.lock().await.recv().await })
            .await
            .ok()
            .flatten()
    }

    /// Wait until the socket epoch reaches `n` (1 = the first `connect`).
    #[allow(dead_code)] // the reconnect findings use this
    pub async fn connection(&self, n: u64) {
        tokio::time::timeout(WAIT, async {
            loop {
                if self.epoch.load(Ordering::SeqCst) >= n {
                    return;
                }
                match self.accepted.lock().await.recv().await {
                    Some(e) if e >= n => return,
                    Some(_) => continue,
                    None => panic!("accept channel closed"),
                }
            }
        })
        .await
        .unwrap_or_else(|_| panic!("timed out waiting for socket epoch {n}"));
    }

    /// Write `frame` to the live socket. Dropped when no socket is up —
    /// as a real executor's write to a dead one would be.
    pub async fn send(&self, frame: Value) {
        if let Some(tx) = self.outgoing.lock().await.as_ref() {
            let _ = tx.send(frame);
        }
    }

    /// Answer `request` (reads its `id`) with `result`.
    pub async fn reply(&self, request: &Value, result: Value) {
        self.send(json!({ "id": request["id"], "result": result }))
            .await;
    }

    /// Answer `request` with an RPC error.
    #[allow(dead_code)] // the reconnect findings use this
    pub async fn reply_error(&self, request: &Value, code: i32, message: &str) {
        self.send(json!({ "id": request["id"], "error": {"code": code, "message": message} }))
            .await;
    }

    /// Sever the live socket server-side, as a dead executor or a dead
    /// network would; the accept loop keeps listening for the reconnect.
    #[allow(dead_code)] // the reconnect findings use this
    pub fn drop_connection(&self) {
        self.bump_drop(None);
    }

    /// Sever the live socket with a close code — `1008` (policy) is the
    /// executor's auth refusal.
    #[allow(dead_code)] // the reconnect findings use this
    pub fn drop_connection_with_code(&self, code: u16) {
        self.bump_drop(Some(code));
    }

    fn bump_drop(&self, code: Option<u16>) {
        let next = self.drop_conn.borrow().0.wrapping_add(1);
        self.drop_conn.send_replace((next, code));
    }
}

async fn serve_conn(
    stream: tokio::net::TcpStream,
    incoming: mpsc::UnboundedSender<Value>,
    mut outgoing: mpsc::UnboundedReceiver<Value>,
    drop_conn: watch::Sender<(u64, Option<u16>)>,
    epoch: u64,
) {
    // Subscribe before the handshake: the drop counter is cumulative, so a
    // `drop_connection` fired while this socket was still handshaking — the
    // receiver's already-seen initial value — must close it too.
    let mut drop_rx = drop_conn.subscribe();
    let ws = match tokio_tungstenite::accept_async(stream).await {
        Ok(ws) => ws,
        Err(_) => return,
    };
    let (mut write, mut read) = ws.split();
    if drop_rx.borrow_and_update().0 >= epoch {
        let _ = write.send(Message::Close(None)).await;
        return;
    }

    loop {
        tokio::select! {
            frame = read.next() => {
                match frame {
                    Some(Ok(Message::Text(t))) => {
                        if let Ok(v) = serde_json::from_str(&t) {
                            if incoming.send(v).is_err() { return; }
                        }
                    }
                    Some(Ok(Message::Ping(p))) => {
                        let _ = write.send(Message::Pong(p)).await;
                    }
                    // Close frame, error, or clean end of stream: the
                    // socket is gone either way.
                    _ => return,
                }
            }
            // The test's queued replies/pushes, on this socket's own
            // channel — nothing strands in a dead socket's task.
            frame = outgoing.recv() => {
                match frame {
                    Some(v) => {
                        if write.send(Message::Text(v.to_string())).await.is_err() {
                            return;
                        }
                    }
                    None => return,
                }
            }
            _ = drop_rx.changed() => {
                let (n, code) = *drop_rx.borrow();
                if n >= epoch {
                    let frame = code.map(|c| {
                        tokio_tungstenite::tungstenite::protocol::frame::CloseFrame {
                            code: tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode::from(c),
                            reason: "".into(),
                        }
                    });
                    let _ = write.send(Message::Close(frame)).await;
                    return;
                }
            }
        }
    }
}

/// A scratch perspective store: the triples a test's scripted executor
/// holds. `handle` answers `perspective.queryLinks`, `perspective.addLink`
/// and `perspective.linkMutations` the way the real perspective does —
/// removals match on author+timestamp — returning the reply, or `None` for
/// a call the test answers (or fails) itself.
#[derive(Default)]
pub(crate) struct LinkStore {
    links: Vec<LinkExpression>,
    seq: u64,
}

impl LinkStore {
    /// The stored links, newest last.
    #[allow(dead_code)]
    pub fn links(&self) -> &[LinkExpression] {
        &self.links
    }

    /// The (source, predicate, target) triples, for set assertions.
    pub fn triples(&self) -> Vec<(String, String, String)> {
        self.links
            .iter()
            .map(|l| {
                (
                    l.data.source.clone(),
                    l.data.predicate.clone().unwrap_or_default(),
                    l.data.target.clone(),
                )
            })
            .collect()
    }

    /// Seed a stored link (e.g. the leftovers of an earlier definition).
    pub fn seed(&mut self, source: &str, predicate: &str, target: &str) {
        let expr = self.expr(source, Some(predicate.to_string()), target);
        self.links.push(expr);
    }

    /// Answer `request` if it is a link call this store knows —
    /// `Some(result)` to reply with, `None` for anything else.
    pub fn handle(&mut self, request: &Value) -> Option<Value> {
        match request["type"].as_str() {
            Some("perspective.queryLinks") => {
                let params = &request["params"];
                let out: Vec<LinkExpression> = self
                    .links
                    .iter()
                    .filter(|l| {
                        params["source"].as_str().is_none_or(|s| l.data.source == s)
                            && params["predicate"]
                                .as_str()
                                .is_none_or(|p| l.data.predicate.as_deref() == Some(p))
                            && params["target"].as_str().is_none_or(|t| l.data.target == t)
                    })
                    .cloned()
                    .collect();
                Some(serde_json::to_value(out).unwrap())
            }
            Some("perspective.addLink") => {
                let link = &request["params"]["link"];
                let expr = self.expr(
                    link["source"].as_str().unwrap_or_default(),
                    link["predicate"].as_str().map(str::to_string),
                    link["target"].as_str().unwrap_or_default(),
                );
                self.links.push(expr);
                Some(Value::Bool(true))
            }
            Some("perspective.linkMutations") => {
                let mutations = &request["params"]["mutations"];
                let removals: Vec<LinkExpressionInput> =
                    serde_json::from_value(mutations["removals"].clone()).unwrap_or_default();
                for removal in &removals {
                    self.links.retain(|l| {
                        !(l.data.source == removal.data.source
                            && l.data.predicate == removal.data.predicate
                            && l.data.target == removal.data.target
                            && l.author == removal.author
                            && l.timestamp == removal.timestamp)
                    });
                }
                let additions: Vec<LinkInput> =
                    serde_json::from_value(mutations["additions"].clone()).unwrap_or_default();
                for link in &additions {
                    // A stored triple dedupes — the graph is a set.
                    if !self.links.iter().any(|l| {
                        l.data.source == link.source
                            && l.data.predicate == link.predicate
                            && l.data.target == link.target
                    }) {
                        let expr = self.expr(&link.source, link.predicate.clone(), &link.target);
                        self.links.push(expr);
                    }
                }
                Some(json!({"additions": [], "removals": []}))
            }
            _ => None,
        }
    }

    fn expr(&mut self, source: &str, predicate: Option<String>, target: &str) -> LinkExpression {
        self.seq += 1;
        LinkExpression {
            author: "did:test:fake".to_string(),
            data: crate::types::Link {
                predicate,
                source: source.to_string(),
                target: target.to_string(),
            },
            proof: crate::types::ExpressionProof {
                invalid: None,
                key: Some("k".to_string()),
                signature: Some(format!("sig{}", self.seq)),
                valid: Some(true),
            },
            timestamp: format!("t{}", self.seq),
            status: Some("SHARED".to_string()),
        }
    }
}
