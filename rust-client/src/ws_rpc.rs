use anyhow::{anyhow, Result};
use futures_util::{SinkExt, StreamExt};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, oneshot, watch, Mutex};
use tokio::task::JoinHandle;
use tokio_tungstenite::tungstenite::protocol::WebSocketConfig;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async_with_config, MaybeTlsStream, WebSocketStream};

static ID_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_id() -> String {
    ID_COUNTER.fetch_add(1, Ordering::Relaxed).to_string()
}

/// First delay between socket reconnect attempts; it doubles per failure.
pub(crate) const RECONNECT_BASE_DELAY_MS: u64 = 1_000;
/// Longest delay between two socket reconnect attempts.
pub(crate) const RECONNECT_MAX_DELAY_MS: u64 = 30_000;

/// A call's budget — covering the wait for a socket and the reply — as TS
/// `DEFAULT_TIMEOUT_MS` (`apiClient.ts`).
const DEFAULT_TIMEOUT: Duration = Duration::from_millis(30_000);
/// The budget of a method the executor marks long (LLM work, Holochain,
/// publishing) — TS `LONG_TIMEOUT_MS`, 20 minutes.
const LONG_TIMEOUT: Duration = Duration::from_secs(20 * 60);
/// The `LONG_METHODS` of the generated contract
/// (`core/src/generated/api/RpcMethods.ts`) — kept in step by
/// `rpc_methods_stay_in_the_generated_contract` in `drift.rs`.
pub(crate) const LONG_METHODS: &[&str] = &[
    "agent.generate",
    "agent.unlock",
    "ai.deleteUpstreamModel",
    "ai.embed",
    "ai.localInstall",
    "ai.localStart",
    "ai.prompt",
    "ai.pullModel",
    "ai.removeEndpoint",
    "ai.updateEndpoint",
    "language.applyTemplate",
    "language.publish",
    "neighbourhood.join",
    "neighbourhood.publish",
    "perspective.runAgent",
    "perspective.runInterpretation",
    "perspective.runInterpretationWithHarness",
    "runtime.restartHolochain",
];

/// The delay before reconnect attempt `failures + 1`: between half and all of
/// `base * 2^(failures - 1)`, capped at `max`. The random half spreads the
/// clients of a restarted executor apart. Same formula as `LiveQuery`'s
/// re-open delay (`subscription.rs`).
pub(crate) fn backoff_delay_ms(failures: u64, base_ms: u64, max_ms: u64) -> u64 {
    let exp = failures.saturating_sub(1).min(20);
    let ceiling = base_ms.saturating_mul(1 << exp).min(max_ms);
    let jitter = rand_jitter(ceiling / 2);
    ceiling / 2 + jitter
}

/// A uniform `0..=max` random — the jitter that spreads the clients of a
/// restarted executor apart.
fn rand_jitter(max: u64) -> u64 {
    if max == 0 {
        return 0;
    }
    rand::random::<u64>() % (max + 1)
}

#[derive(Serialize)]
struct RpcMessage {
    id: String,
    #[serde(rename = "type")]
    msg_type: String,
    params: Value,
}

#[derive(Deserialize)]
struct RpcResponse {
    #[allow(dead_code)] // Present in JSON wire format but matched via PendingMap key
    id: Option<String>,
    result: Option<Value>,
    error: Option<RpcError>,
}

#[derive(Deserialize)]
struct RpcError {
    code: Option<i32>,
    message: Option<String>,
    data: Option<Value>,
}

/// An error reply from the executor. `call` returns it inside the `anyhow::Error`, so a caller
/// reads the code and the structured `data` with `err.downcast_ref::<RpcCallError>()`.
#[derive(Debug, Clone, PartialEq)]
pub struct RpcCallError {
    pub code: i32,
    pub message: String,
    /// The reply's `error.data`, when the executor attached some (e.g. the `InstanceError` of a
    /// refused `perspective.instance*` write).
    pub data: Option<Value>,
}

impl std::fmt::Display for RpcCallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RPC error {}: {}", self.code, self.message)
    }
}

impl std::error::Error for RpcCallError {}

type PendingMap = Arc<Mutex<HashMap<String, oneshot::Sender<Result<Value>>>>>;
type WsSink = futures_util::stream::SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>;
type WsSource = futures_util::stream::SplitStream<WebSocketStream<MaybeTlsStream<TcpStream>>>;

/// One named event interest. `spec` is the `events.watch` params fragment for
/// this watcher: `{ "<event type>": null | ["uuid", ...] }`.
type WatchMap = BTreeMap<String, Value>;
/// A callback fired once a fresh socket is installed (post `events.watch`).
type ReconnectCb = Arc<dyn Fn() + Send + Sync>;

struct Shared {
    /// The WS URL (token already inside) — needed by the reconnect task.
    url: String,
    sink: Mutex<Option<WsSink>>,
    pending: PendingMap,
    events: broadcast::Sender<Value>,
    /// `true` while a socket is installed and callable.
    connected: watch::Sender<bool>,
    /// Named event interests; the union goes out as `events.watch` on
    /// (re)connect and on every change. Key `""` is the user watch —
    /// `watch_events` replaces it; live queries register their own keys.
    watches: Mutex<WatchMap>,
    on_reconnect: Mutex<Vec<(u64, ReconnectCb)>>,
    closed: AtomicBool,
    /// Auth refusal (close 1008) — do not reconnect, mirror TS.
    auth_failed: AtomicBool,
    reader: Mutex<Option<JoinHandle<()>>>,
    /// The single task that ever dials again (spawned once at `connect`,
    /// aborted by `close`). A socket dying while it restores leaves
    /// `connected` false, which is exactly what it waits on — there is no
    /// "reconnect in progress" guard a disconnect can hide behind.
    supervisor: Mutex<Option<JoinHandle<()>>>,
}

/// Merge every named interest into the `events.watch` params: for each event
/// type, `null` (all perspectives) wins over any uuid list; lists union.
fn merge_watches(watches: &WatchMap) -> Value {
    let mut by_type: BTreeMap<String, Option<Vec<String>>> = BTreeMap::new();
    for spec in watches.values() {
        if let Some(obj) = spec.as_object() {
            for (event, filter) in obj {
                match filter {
                    Value::Null => {
                        by_type.insert(event.clone(), None);
                    }
                    Value::Array(uuids) => {
                        let entry = by_type.entry(event.clone()).or_insert_with(|| Some(vec![]));
                        if let Some(list) = entry {
                            for u in uuids {
                                if let Some(s) = u.as_str() {
                                    if !list.iter().any(|x| x == s) {
                                        list.push(s.to_string());
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    Value::Object(
        by_type
            .into_iter()
            .map(|(k, v)| (k, v.map(Value::from).unwrap_or(Value::Null)))
            .collect(),
    )
}

pub struct WsRpcClient {
    shared: Arc<Shared>,
}

impl std::fmt::Debug for WsRpcClient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WsRpcClient")
            .field("connected", &self.is_connected())
            .finish()
    }
}

fn to_ws_url(executor_url: &str, token: &str) -> String {
    let ws_base = executor_url
        .replace("http://", "ws://")
        .replace("https://", "wss://")
        .trim_end_matches('/')
        .to_string();
    if token.is_empty() {
        format!("{}/api/v1/ws", ws_base)
    } else {
        format!("{}/api/v1/ws?token={}", ws_base, urlencoding::encode(token))
    }
}

fn ws_config() -> WebSocketConfig {
    WebSocketConfig {
        max_message_size: Some(256 * 1024 * 1024),
        max_frame_size: Some(64 * 1024 * 1024),
        ..Default::default()
    }
}

impl WsRpcClient {
    pub async fn connect(executor_url: &str, token: &str) -> Result<Self> {
        let url = to_ws_url(executor_url, token);
        let (write, read) = Self::open_socket(&url, executor_url).await?;

        let (events_tx, _) = broadcast::channel::<Value>(512);
        let (connected_tx, _) = watch::channel(true);

        let shared = Arc::new(Shared {
            url: url.clone(),
            sink: Mutex::new(Some(write)),
            pending: Arc::new(Mutex::new(HashMap::new())),
            events: events_tx,
            connected: connected_tx,
            watches: Mutex::new(WatchMap::new()),
            on_reconnect: Mutex::new(Vec::new()),
            closed: AtomicBool::new(false),
            auth_failed: AtomicBool::new(false),
            reader: Mutex::new(None),
            supervisor: Mutex::new(None),
        });

        let reader = Self::spawn_reader(read, shared.clone());
        *shared.reader.lock().await = Some(reader);
        let supervisor = Self::spawn_supervisor(shared.clone());
        *shared.supervisor.lock().await = Some(supervisor);

        Ok(Self { shared })
    }

    async fn open_socket(url: &str, executor_url_for_errors: &str) -> Result<(WsSink, WsSource)> {
        let (ws_stream, _) = connect_async_with_config(url, Some(ws_config()), false)
            .await
            // Named without the token: callers print this error.
            .map_err(|e| {
                anyhow!(
                    "WebSocket connection to {} failed: {}",
                    to_ws_url(executor_url_for_errors, ""),
                    e
                )
            })?;
        Ok(ws_stream.split())
    }

    /// Background reader: RPC replies go to pending callers, everything else
    /// to the event broadcast. On exit it drives the disconnect path.
    fn spawn_reader(mut read: WsSource, shared: Arc<Shared>) -> JoinHandle<()> {
        let shared_reader = shared.clone();
        tokio::spawn(async move {
            let mut auth_failed = false;
            while let Some(msg) = read.next().await {
                let text = match msg {
                    Ok(Message::Text(t)) => t,
                    Ok(Message::Close(frame)) => {
                        auth_failed = matches!(
                            &frame,
                            Some(f) if f.code == tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode::Policy
                        );
                        eprintln!("[ws-rpc] connection closed by server: {:?}", frame);
                        break;
                    }
                    Err(e) => {
                        eprintln!("[ws-rpc] read error: {:?}", e);
                        break;
                    }
                    _ => continue,
                };

                let parsed: Value = match serde_json::from_str(&text) {
                    Ok(v) => v,
                    Err(_) => continue,
                };

                // RPC responses have an "id" field; events do not.
                if let Some(id) = parsed.get("id").and_then(|v| v.as_str()).map(str::to_owned) {
                    let resp: RpcResponse = match serde_json::from_value(parsed) {
                        Ok(r) => r,
                        Err(_) => continue,
                    };
                    if let Some(tx) = shared_reader.pending.lock().await.remove(&id) {
                        if let Some(error) = resp.error {
                            let _ = tx.send(Err(anyhow::Error::new(RpcCallError {
                                code: error.code.unwrap_or(500),
                                message: error.message.unwrap_or_default(),
                                data: error.data,
                            })));
                        } else {
                            let _ = tx.send(Ok(resp.result.unwrap_or(Value::Null)));
                        }
                    }
                } else {
                    // Server-push event — broadcast to subscribers
                    let _ = shared_reader.events.send(parsed);
                }
            }

            if auth_failed {
                shared.auth_failed.store(true, Ordering::SeqCst);
            }
            Self::handle_disconnect(&shared, false).await;
        })
    }

    /// The socket is gone: drain pending calls and mark the client
    /// disconnected. Reconnecting is the supervisor's job — it watches the
    /// `connected` flag, so a death can never hide behind a
    /// "reconnect already running" guard. `abort_reader` is `false` from the
    /// reader itself — aborting its own handle would cancel the rest of
    /// this cleanup at the next yield.
    async fn handle_disconnect(shared: &Arc<Shared>, abort_reader: bool) {
        {
            let mut sink = shared.sink.lock().await;
            *sink = None; // drop whatever half remains
        }
        // Abort the reader when this runs from `disconnect_for_testing` —
        // dropping `read` closes the TCP stream executor-side too.
        if abort_reader {
            if let Some(reader) = shared.reader.lock().await.take() {
                reader.abort();
            }
        }
        // `send_replace`, not `send`: with no live receivers `send` drops
        // the value and `is_connected` keeps reporting the stale flag.
        shared.connected.send_replace(false);

        // Fail every in-flight call — the executor can't answer them now.
        let mut map = shared.pending.lock().await;
        for (_, tx) in map.drain() {
            let _ = tx.send(Err(anyhow!("WebSocket connection closed")));
        }
    }

    /// The one task that ever dials again. It sleeps on the `connected`
    /// flag; a disconnect stores false there, the supervisor dials with
    /// backoff, installs the socket, re-sends the watch and fires the
    /// reconnect callbacks — then waits on the flag again. A socket that
    /// dies mid-restore leaves the flag false, so the loop just dials the
    /// next one (F6): ownership of reconnecting never lapses.
    fn spawn_supervisor(shared: Arc<Shared>) -> JoinHandle<()> {
        tokio::spawn(async move {
            let mut conn = shared.connected.subscribe();
            let mut failures: u64 = 0;
            loop {
                // Wait for a dead socket: `borrow_and_update` after each
                // restore absorbs a death that arrived while we were
                // installing, so it is neither lost nor double-handled.
                while *conn.borrow_and_update() {
                    if conn.changed().await.is_err() {
                        return;
                    }
                }
                if shared.closed.load(Ordering::SeqCst) || shared.auth_failed.load(Ordering::SeqCst)
                {
                    return;
                }
                failures += 1;
                let delay =
                    backoff_delay_ms(failures, RECONNECT_BASE_DELAY_MS, RECONNECT_MAX_DELAY_MS);
                tokio::time::sleep(Duration::from_millis(delay)).await;

                match connect_async_with_config(&shared.url, Some(ws_config()), false).await {
                    Ok((ws_stream, _)) => {
                        let (write, read) = ws_stream.split();
                        {
                            let mut sink = shared.sink.lock().await;
                            *sink = Some(write);
                        }
                        let reader = Self::spawn_reader(read, shared.clone());
                        *shared.reader.lock().await = Some(reader);
                        shared.connected.send_replace(true);

                        // Re-register interest first — subscription re-opens
                        // fired below need `query-subscription-update` watched.
                        Self::send_watch(&shared).await;

                        let callbacks: Vec<ReconnectCb> = shared
                            .on_reconnect
                            .lock()
                            .await
                            .iter()
                            .map(|(_, cb)| cb.clone())
                            .collect();
                        for cb in callbacks {
                            // A panicking callback must not take the
                            // supervisor down with it.
                            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| cb()));
                        }
                        if *conn.borrow() {
                            failures = 0;
                        }
                    }
                    Err(e) => {
                        eprintln!("[ws-rpc] reconnect failed (attempt {failures}): {e}");
                    }
                }
            }
        })
    }

    /// Merge the named interests and send `events.watch` — an empty map too:
    /// it tells the executor to stop sending events (`events.watch` replaces
    /// the socket's entire interest, TS `ApiClient._flushWatch`).
    async fn send_watch(shared: &Arc<Shared>) {
        let merged = {
            let watches = shared.watches.lock().await;
            merge_watches(&watches)
        };
        // Send directly rather than via `call` — `call` on the freshly
        // installed socket works, but this keeps ordering explicit.
        let id = next_id();
        let msg = RpcMessage {
            id: id.clone(),
            msg_type: "events.watch".to_string(),
            params: merged,
        };
        let (tx, rx) = oneshot::channel();
        shared.pending.lock().await.insert(id, tx);
        if let Ok(json) = serde_json::to_string(&msg) {
            let mut sink = shared.sink.lock().await;
            if let Some(s) = sink.as_mut() {
                let _ = s.send(Message::Text(json)).await;
            }
        }
        // Fire-and-forget: the reply is drained by the reader; a 5s guard
        // avoids a leaked pending entry when it never comes.
        tokio::spawn(async move {
            let _ = tokio::time::timeout(Duration::from_secs(5), rx).await;
        });
    }

    /// Register `key`'s event interest; the merged union is sent on the socket.
    /// Live queries use this so a subscription watch survives reconnects.
    pub(crate) async fn set_watch(&self, key: &str, spec: Value) {
        {
            let mut watches = self.shared.watches.lock().await;
            watches.insert(key.to_string(), spec);
        }
        if *self.shared.connected.borrow() {
            Self::send_watch(&self.shared).await;
        }
    }

    pub(crate) async fn remove_watch(&self, key: &str) {
        {
            let mut watches = self.shared.watches.lock().await;
            watches.remove(key);
        }
        if *self.shared.connected.borrow() {
            Self::send_watch(&self.shared).await;
        }
    }

    /// `callback` runs after each successful reconnect, once the watch was
    /// re-sent. Returns an id for `remove_on_reconnect`.
    pub async fn on_reconnect(&self, callback: impl Fn() + Send + Sync + 'static) -> u64 {
        static CB_ID: AtomicU64 = AtomicU64::new(0);
        let id = CB_ID.fetch_add(1, Ordering::Relaxed);
        self.shared
            .on_reconnect
            .lock()
            .await
            .push((id, Arc::new(callback)));
        id
    }

    pub async fn remove_on_reconnect(&self, id: u64) {
        self.shared
            .on_reconnect
            .lock()
            .await
            .retain(|(cb_id, _)| *cb_id != id);
    }

    /// Whether a socket is currently installed.
    pub fn is_connected(&self) -> bool {
        *self.shared.connected.borrow()
    }

    /// The raw server-push event stream (before any `events.watch` filter —
    /// the executor only sends what a watched socket asked for).
    pub(crate) fn events(&self) -> broadcast::Receiver<Value> {
        self.shared.events.subscribe()
    }

    /// Permanently close the client: socket down, no reconnect.
    pub async fn close(&self) {
        self.shared.closed.store(true, Ordering::SeqCst);
        if let Some(h) = self.shared.supervisor.lock().await.take() {
            h.abort();
        }
        if let Some(h) = self.shared.reader.lock().await.take() {
            h.abort();
        }
        *self.shared.sink.lock().await = None;
        self.shared.connected.send_replace(false);
        let mut map = self.shared.pending.lock().await;
        for (_, tx) in map.drain() {
            let _ = tx.send(Err(anyhow!("WebSocket connection closed")));
        }
    }

    /// Drop the live socket and walk the same disconnect path a dead socket
    /// walks: TCP closes server-side (subscriptions die), then the client
    /// reconnects. For tests that exercise reconnect/resync.
    #[doc(hidden)]
    pub async fn disconnect_for_testing(&self) {
        Self::handle_disconnect(&self.shared, true).await;
    }

    /// Wait for a socket (or permanent close), then send and await the reply.
    /// Times out as TS `ApiClient._request` does: 30 s, 20 min on a
    /// `LONG_METHODS` call; the budget covers reconnecting.
    pub async fn call<T: DeserializeOwned>(&self, msg_type: &str, params: Value) -> Result<T> {
        let result = self.call_value(msg_type, params).await?;
        serde_json::from_value(result.clone()).map_err(|e| {
            anyhow!(
                "Failed to deserialize response for '{}': {} (raw: {})",
                msg_type,
                e,
                result
            )
        })
    }

    /// `call` returning the raw result `Value` — for callers whose result
    /// needs a parse serde can't give (e.g. `modelQuery`'s JSON-in-a-string).
    pub async fn call_value(&self, msg_type: &str, params: Value) -> Result<Value> {
        let id = next_id();
        let timeout = if LONG_METHODS.contains(&msg_type) {
            LONG_TIMEOUT
        } else {
            DEFAULT_TIMEOUT
        };
        match tokio::time::timeout(timeout, self.call_inner(&id, msg_type, params)).await {
            Ok(r) => r,
            Err(_) => {
                // The pending entry would sit until a reply lands (or the
                // socket drains it); drop it now so a late reply answers no
                // one.
                self.shared.pending.lock().await.remove(&id);
                Err(anyhow!(
                    "RPC call '{}' timed out after {}ms",
                    msg_type,
                    timeout.as_millis()
                ))
            }
        }
    }

    async fn call_inner(&self, id: &str, msg_type: &str, params: Value) -> Result<Value> {
        let msg = RpcMessage {
            id: id.to_string(),
            msg_type: msg_type.to_string(),
            params,
        };

        let (tx, rx) = oneshot::channel();
        self.shared.pending.lock().await.insert(id.to_string(), tx);

        let json = serde_json::to_string(&msg)?;
        let send_result = {
            let mut connected_rx = self.shared.connected.subscribe();
            loop {
                if self.shared.closed.load(Ordering::SeqCst) {
                    self.shared.pending.lock().await.remove(id);
                    return Err(anyhow!("WebSocket client is closed"));
                }
                let mut sink = self.shared.sink.lock().await;
                if let Some(s) = sink.as_mut() {
                    break s.send(Message::Text(json.clone())).await;
                }
                drop(sink);
                // Reconnecting — wait for a socket or the final close.
                if connected_rx.changed().await.is_err() {
                    self.shared.pending.lock().await.remove(id);
                    return Err(anyhow!("WebSocket client is closed"));
                }
            }
        };

        if let Err(e) = send_result {
            self.shared.pending.lock().await.remove(id);
            return Err(anyhow!("Failed to send WS message: {}", e));
        }

        rx.await.map_err(|_| anyhow!("Response channel closed"))?
    }

    /// Fire-and-forget call that discards the result.
    pub async fn call_void(&self, msg_type: &str, params: Value) -> Result<()> {
        let _: Value = self.call(msg_type, params).await?;
        Ok(())
    }

    /// Receive server-push events. The executor sends a socket only the
    /// events it asked for: `events` maps each event type to the perspective
    /// uuids wanted, or `null` for all (`events.watch`). Replaces the user
    /// interest — named interests (e.g. live queries' `query-subscription-update`)
    /// persist alongside it. Each event has a `"type"` field.
    pub async fn watch_events(&self, events: Value) -> Result<broadcast::Receiver<Value>> {
        let rx = self.shared.events.subscribe();
        self.set_watch("", events).await;
        Ok(rx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_ws_url_converts_http_to_ws() {
        let url = to_ws_url("http://localhost:12000", "");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws");
    }

    #[test]
    fn to_ws_url_converts_https_to_wss() {
        let url = to_ws_url("https://remote.host:443", "");
        assert_eq!(url, "wss://remote.host:443/api/v1/ws");
    }

    #[test]
    fn to_ws_url_appends_token_query_param() {
        let url = to_ws_url("http://localhost:12000", "my-jwt-token");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws?token=my-jwt-token");
    }

    #[test]
    fn to_ws_url_encodes_special_chars_in_token() {
        let url = to_ws_url("http://localhost:12000", "tok en+special=chars&more");
        assert!(url.contains("token=tok%20en%2Bspecial%3Dchars%26more"));
    }

    /// A failed connect names the executor but never the token (the URL carries it).
    #[tokio::test]
    async fn a_failed_connect_never_shows_the_token() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};
        // The port stays ours: the server refuses the upgrade with a plain HTTP answer.
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        tokio::spawn(async move {
            let (mut socket, _) = listener.accept().await.unwrap();
            let mut request = [0u8; 4096];
            let _ = socket.read(&mut request).await;
            let _ = socket
                .write_all(
                    b"HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
                )
                .await;
        });
        let error = WsRpcClient::connect(&format!("http://127.0.0.1:{port}"), "secret-cap-token")
            .await
            .expect_err("the upgrade is refused")
            .to_string();
        assert!(!error.contains("secret-cap-token"), "{error}");
        assert!(error.contains(&format!("127.0.0.1:{port}")), "{error}");
    }

    #[test]
    fn to_ws_url_strips_trailing_slash() {
        let url = to_ws_url("http://localhost:12000/", "");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws");
    }

    #[test]
    fn to_ws_url_empty_token_no_query_param() {
        let url = to_ws_url("http://localhost:12000", "");
        assert!(!url.contains("?token="));
        assert!(!url.contains("?token"));
    }

    #[test]
    fn next_id_increments() {
        let id1 = next_id();
        let id2 = next_id();
        let n1: u64 = id1.parse().unwrap();
        let n2: u64 = id2.parse().unwrap();
        assert_eq!(n2, n1 + 1);
    }

    #[test]
    fn backoff_delay_grows_and_caps() {
        let d1 = backoff_delay_ms(1, 1000, 30_000);
        assert!((500..=1000).contains(&d1), "{d1}");
        let d5 = backoff_delay_ms(5, 1000, 30_000);
        assert!((8000..=16000).contains(&d5), "{d5}");
        let d_big = backoff_delay_ms(50, 1000, 30_000);
        assert!((15_000..=30_000).contains(&d_big), "{d_big}");
    }

    #[test]
    fn merge_watches_unions_named_interests() {
        let mut watches = WatchMap::new();
        watches.insert("".into(), serde_json::json!({"perspective-added": null}));
        watches.insert(
            "lq:1".into(),
            serde_json::json!({"query-subscription-update": null, "link-added": ["p1"]}),
        );
        watches.insert(
            "lq:2".into(),
            serde_json::json!({"link-added": ["p2", "p1"]}),
        );
        let merged = merge_watches(&watches);
        assert_eq!(merged["perspective-added"], serde_json::Value::Null);
        assert_eq!(merged["query-subscription-update"], serde_json::Value::Null);
        let mut got: Vec<String> = serde_json::from_value(merged["link-added"].clone()).unwrap();
        got.sort();
        assert_eq!(got, vec!["p1".to_string(), "p2".to_string()]);
    }

    #[test]
    fn merge_watches_null_wins_over_uuid_lists() {
        let mut watches = WatchMap::new();
        watches.insert("a".into(), serde_json::json!({"ev": ["p1"]}));
        watches.insert("b".into(), serde_json::json!({"ev": null}));
        assert_eq!(merge_watches(&watches)["ev"], serde_json::Value::Null);
    }

    #[test]
    fn rpc_message_serializes_correctly() {
        let msg = RpcMessage {
            id: "42".to_string(),
            msg_type: "agent.status".to_string(),
            params: serde_json::json!({}),
        };
        let json = serde_json::to_value(&msg).unwrap();
        assert_eq!(json["id"], "42");
        assert_eq!(json["type"], "agent.status");
        assert_eq!(json["params"], serde_json::json!({}));
    }

    #[test]
    fn rpc_response_deserializes_success() {
        let json = r#"{"id": "1", "result": {"status": "ok"}}"#;
        let resp: RpcResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.id, Some("1".to_string()));
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn rpc_response_deserializes_error() {
        let json = r#"{"id": "2", "error": {"code": 401, "message": "Unauthorized"}}"#;
        let resp: RpcResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.id, Some("2".to_string()));
        assert!(resp.result.is_none());
        let err = resp.error.unwrap();
        assert_eq!(err.code, Some(401));
        assert_eq!(err.message, Some("Unauthorized".to_string()));
    }

    #[tokio::test]
    async fn call_fails_on_closed_channel() {
        // Create a client with a mock sender that simulates a closed connection
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that immediately closes
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let ws = accept_async(stream).await.unwrap();
            drop(ws); // Close immediately
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        // Wait for server to close
        server.await.unwrap();
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;

        // The client starts reconnecting (executor is down); close() makes the
        // call fail rather than wait forever.
        client.close().await;
        let result: Result<Value> = client.call("test.method", serde_json::json!({})).await;
        assert!(result.is_err());
    }

    /// F7: an idle close must flip `is_connected`. `watch::Sender::send`
    /// drops the new value when no receiver is live — which is precisely
    /// when nothing reads `is_connected` — so the flag must be stored with
    /// `send_replace` instead.
    #[tokio::test]
    async fn an_idle_socket_close_marks_the_client_disconnected() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();
        assert!(client.is_connected());

        // A call in flight when the socket dies is failed by
        // `handle_disconnect` — its error is the barrier that the whole
        // disconnect path (flag included) ran.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let call =
            tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        let req = fake.next_request().await;
        assert_eq!(req["type"], "x.y");
        fake.drop_connection();
        assert!(call.await.unwrap().is_err());

        // Reconnect cannot land for at least the first backoff (~500 ms),
        // so this reads the flag during the provably-disconnected window.
        assert!(
            !client.is_connected(),
            "is_connected stayed true after the socket died"
        );

        // A permanent close reports disconnected too.
        client.close().await;
        assert!(!client.is_connected());
    }

    /// F6: a socket that dies *while the reconnect task is still restoring*
    /// must still schedule the next dial. The reconnect task holds the
    /// `reconnect` slot until it returns; a disconnect observed meanwhile
    /// early-returns and nobody ever dials again. Multi-threaded: the
    /// callback parks a worker on a `Barrier`; the test drives on another.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_disconnect_during_restore_still_reconnects() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // Park the reconnect task mid-restore: its callback blocks on a
        // barrier the test holds until after socket 2 is dead.
        let gate = std::sync::Arc::new(std::sync::Barrier::new(2));
        let cb_gate = gate.clone();
        client
            .on_reconnect(move || {
                cb_gate.wait();
            })
            .await;

        // Socket 1 dies; a drained in-flight call is the barrier that the
        // disconnect path ran.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection();
        assert!(c0.await.unwrap().is_err());

        // The reconnect lands; restoration starts (the watch goes out).
        fake.connection(2).await;
        let w = fake.next_request().await;
        assert_eq!(w["type"], "events.watch");
        fake.reply(&w, Value::Bool(true)).await;

        // Give the task a beat to reach the parked callback... actually the
        // callback runs right after send_watch; c1 riding socket 2 proves
        // the socket is installed before we drop it under the parked task.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c1 = tokio::spawn(async move { caller.call_value("x.z", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection();
        // socket 2's drain fails c1 — the barrier that ITS disconnect ran.
        assert!(c1.await.unwrap().is_err());

        // Let the parked callback finish. With the guard bug the task then
        // exits and nobody ever dials again; a supervisor notices the dead
        // socket and dials epoch 3.
        gate.wait();
        fake.connection(3).await;

        client.close().await;
    }

    #[tokio::test]
    async fn call_receives_response() {
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that echoes back a result
        tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut ws = accept_async(stream).await.unwrap();
            if let Some(Ok(Message::Text(text))) = ws.next().await {
                let req: Value = serde_json::from_str(&text).unwrap();
                let id = req["id"].as_str().unwrap().to_string();
                let response = serde_json::json!({
                    "id": id,
                    "result": {"hello": "world"}
                });
                ws.send(Message::Text(response.to_string())).await.unwrap();
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        let result: Value = client
            .call("test.echo", serde_json::json!({"key": "value"}))
            .await
            .unwrap();
        assert_eq!(result, serde_json::json!({"hello": "world"}));
    }

    #[tokio::test]
    async fn call_returns_error_from_server() {
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that returns an error response
        tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut ws = accept_async(stream).await.unwrap();
            if let Some(Ok(Message::Text(text))) = ws.next().await {
                let req: Value = serde_json::from_str(&text).unwrap();
                let id = req["id"].as_str().unwrap().to_string();
                let response = serde_json::json!({
                    "id": id,
                    "error": {"code": 403, "message": "Forbidden"}
                });
                ws.send(Message::Text(response.to_string())).await.unwrap();
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        let result: Result<Value> = client.call("test.forbidden", serde_json::json!({})).await;
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("403"),
            "Error should contain code: {}",
            err_msg
        );
        assert!(
            err_msg.contains("Forbidden"),
            "Error should contain message: {}",
            err_msg
        );
    }

    /// Reconnect: the client's socket dies, a second server accepts the
    /// reconnect, the watch is re-sent and `on_reconnect` fires.
    #[tokio::test]
    async fn reconnects_after_socket_drop_and_resends_watch() {
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let (watches_tx, mut watches_rx) = tokio::sync::mpsc::unbounded_channel::<Value>();
        let (second_conn_tx, mut second_conn_rx) = tokio::sync::mpsc::unbounded_channel::<()>();

        tokio::spawn(async move {
            let mut conn_count = 0;
            while let Ok((stream, _)) = listener.accept().await {
                conn_count += 1;
                let watches_tx = watches_tx.clone();
                let second_conn_tx = second_conn_tx.clone();
                tokio::spawn(async move {
                    let mut ws = accept_async(stream).await.unwrap();
                    if conn_count == 2 {
                        let _ = second_conn_tx.send(());
                    }
                    while let Some(Ok(Message::Text(text))) = ws.next().await {
                        let req: Value = serde_json::from_str(&text).unwrap();
                        if req["type"] == "events.watch" {
                            let _ = watches_tx.send(req["params"].clone());
                        }
                        let response = serde_json::json!({"id": req["id"], "result": true});
                        if ws.send(Message::Text(response.to_string())).await.is_err() {
                            break;
                        }
                    }
                });
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();
        let rx = client
            .watch_events(serde_json::json!({"query-subscription-update": null}))
            .await
            .unwrap();
        let first_watch = tokio::time::timeout(Duration::from_secs(5), watches_rx.recv())
            .await
            .unwrap()
            .unwrap();
        assert_eq!(first_watch["query-subscription-update"], Value::Null);

        let (reconnected_tx, reconnected_rx) = oneshot::channel::<()>();
        let reconnected_tx = std::sync::Mutex::new(Some(reconnected_tx));
        client
            .on_reconnect(move || {
                if let Some(tx) = reconnected_tx.lock().unwrap().take() {
                    let _ = tx.send(());
                }
            })
            .await;

        client.disconnect_for_testing().await;
        // Give the reader task a beat to observe the aborted socket.
        tokio::time::sleep(Duration::from_millis(50)).await;

        tokio::time::timeout(Duration::from_secs(15), second_conn_rx.recv())
            .await
            .expect("executor never saw the reconnect")
            .unwrap();
        tokio::time::timeout(Duration::from_secs(5), reconnected_rx)
            .await
            .expect("on_reconnect never fired")
            .unwrap();

        // The watch must have been re-sent on the new socket.
        let mut saw_watch = false;
        while let Ok(Some(w)) =
            tokio::time::timeout(Duration::from_secs(5), watches_rx.recv()).await
        {
            if w["query-subscription-update"] == Value::Null {
                saw_watch = true;
                break;
            }
        }
        assert!(saw_watch, "events.watch was not re-sent after reconnect");

        // A call on the reconnected socket answers.
        let result: Value = client
            .call("test.echo", serde_json::json!({}))
            .await
            .unwrap();
        assert_eq!(result, Value::Bool(true));
        drop(rx);
        client.close().await;
    }
}
