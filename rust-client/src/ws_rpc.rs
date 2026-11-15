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

/// Dead sockets an unsent call survives before it too is failed — TS
/// `MAX_CONNECT_ATTEMPTS` (`apiClient.ts`).
const MAX_FAILED_CONNECTS: u8 = 3;
/// Bound on an `events.watch` reply — a socket that stays up but never
/// answers must not hold `watch_flush` or its pending entry forever (F12).
const WATCH_REPLY_TIMEOUT: Duration = Duration::from_secs(5);
/// Bound on a socket write itself — `SinkExt::send` on a peer that keeps
/// TCP open but stops reading can park forever, holding `sink` (and
/// `watch_flush`/`pending` behind it) while disconnect and close wait on
/// those very locks (R5).
const WATCH_SEND_TIMEOUT: Duration = Duration::from_secs(5);
/// The budget of one call's socket write, independent of the call's own
/// (up to 20-minute) reply budget: a peer that stops reading fails the
/// write, instead of pinning `pending` and `sink` for the whole call (N2).
/// Close and disconnect still unwind it at once through `send_abort`.
const CALL_SEND_TIMEOUT: Duration = DEFAULT_TIMEOUT;

/// A reader task that is not installed yet: dropping the guard aborts it.
/// The supervisor holds its fresh socket's reader in one until publication
/// — aborting the supervisor (close) then aborts the reader too, and with
/// it the read half, instead of detaching it (N4). An abort only takes
/// effect at the task's next poll, so the handle is not dropped but
/// handed to `retired_readers`, where close joins it (R4-3).
struct AbortOnDrop {
    handle: Option<JoinHandle<()>>,
    shared: Arc<Shared>,
}

impl AbortOnDrop {
    fn new(handle: JoinHandle<()>, shared: Arc<Shared>) -> Self {
        Self {
            handle: Some(handle),
            shared,
        }
    }

    /// Hand the task over to whoever installs it; the guard is disarmed.
    fn disarm(mut self) -> JoinHandle<()> {
        self.handle
            .take()
            .expect("reader handle present until disarmed")
    }
}

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        if let Some(h) = self.handle.take() {
            h.abort();
            if let Ok(mut retired) = self.shared.retired_readers.lock() {
                retired.push(h);
            }
        }
    }
}

/// How one send attempt of [`WsRpcClient::call_inner`] ended.
enum Attempt {
    Sent,
    /// The call left `pending`: timed out or failed by someone else.
    Gone,
    /// `close` ran: seen under the locks.
    Closed,
    /// No socket installed, or the installed one is being torn down
    /// (its stamp is stale): wait for the next one.
    NoSocket,
    /// Close or disconnect signalled `send_abort` mid-attempt.
    Aborted,
    Failed(anyhow::Error),
}

/// One RPC awaiting a reply — or still waiting for a socket to send on.
/// `sent` flips only while `pending` and `sink` are held together, in that
/// order, so the disconnect drain can never fail a call whose frame then
/// still goes out (F8).
struct PendingCall {
    tx: oneshot::Sender<Result<Value>>,
    /// The wire send happened — a socket death fails the call. An unsent
    /// call is a *waiting* call: it survives the disconnect and sends on
    /// the next socket, like TS `_onClose` (`!pending.sent → continue`).
    sent: bool,
    /// Dead sockets seen while still unsent; at [`MAX_FAILED_CONNECTS`]
    /// the call is failed too.
    failed_connects: u8,
}

type PendingMap = Arc<Mutex<HashMap<String, PendingCall>>>;
type WsSink = futures_util::stream::SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>;
type WsSource = futures_util::stream::SplitStream<WebSocketStream<MaybeTlsStream<TcpStream>>>;

/// One named event interest. `spec` is the `events.watch` params fragment for
/// this watcher: `{ "<event type>": null | ["uuid", ...] }`.
type WatchMap = BTreeMap<String, Value>;

/// The named interests plus a revision that bumps on every mutation. A
/// restore flush re-sends until the acknowledged union is the union that
/// exists — interest recorded mid-restore is never silently dropped (R2).
struct Watches {
    map: WatchMap,
    rev: u64,
}

/// A callback fired once a fresh socket is installed (post `events.watch`).
pub(crate) type ReconnectCb = Arc<dyn Fn() + Send + Sync>;

/// A callback fired once when the executor refuses the token (close 1008).
type AuthFailedCb = Arc<dyn Fn(&str) + Send + Sync>;

/// The token refusal (close 1008) and who is waiting for it.
#[derive(Default)]
struct Refusal {
    /// The close frame's reason ("Unauthorized access", "… expired");
    /// `Some` once refused.
    reason: Option<String>,
    /// Listeners not yet run; the refusal takes them all.
    callbacks: Vec<(u64, AuthFailedCb)>,
}

struct Shared {
    /// The WS URL (token already inside) — needed by the reconnect task.
    url: String,
    /// The installed write half, stamped with the `teardowns` value it
    /// was published under. A stamp that no longer matches means a
    /// teardown of this socket has begun: writers refuse it (R4-1).
    sink: Mutex<Option<(u64, WsSink)>>,
    pending: PendingMap,
    events: broadcast::Sender<Value>,
    /// `true` while a socket is installed and callable.
    connected: watch::Sender<bool>,
    /// Named event interests; the union goes out as `events.watch` on
    /// (re)connect and on every change. Key `""` is the user watch —
    /// `watch_events` replaces it; live queries register their own keys.
    watches: Mutex<Watches>,
    /// Serializes `events.watch` flushes: snapshot+send+reply run under one
    /// gate, so a slower flush can never write its older union over a newer
    /// one on the wire (F9).
    watch_flush: Mutex<()>,
    /// How many teardowns (disconnects and closes) have begun. Bumped
    /// *before* `send_abort` is notified, so it is the persistent half of
    /// that transient signal: a writer that registers its waiter after the
    /// notify still finds the bumped value under the sink lock and refuses
    /// the stamped sink (R4-1). The supervisor samples it before spawning
    /// a socket's reader; under `conn_guard` a changed value means the
    /// socket died mid-install and its disconnect owns the flag (R1).
    teardowns: AtomicU64,
    /// Serializes a disconnect's flag write against an install's
    /// epoch-check-and-publish, so a `true` can never land over a `false`
    /// a close already stored (R1).
    conn_guard: Mutex<()>,
    /// Never held across an await: registration is synchronous, so a
    /// caller cannot be cancelled between registering and owning the id.
    on_reconnect: std::sync::Mutex<Vec<(u64, ReconnectCb)>>,
    closed: AtomicBool,
    /// Terminal signal — `true` once `close()` ran or auth was refused.
    /// Live queries end their streams on it; plain disconnects don't set
    /// it (those reconnect).
    closed_tx: watch::Sender<bool>,
    /// Auth refusal (close 1008) — do not reconnect, mirror TS. A fast
    /// read of `refusal.reason.is_some()`; stored under the `refusal` lock.
    auth_failed: AtomicBool,
    /// The refusal and its listeners under one lock: the reader records
    /// the reason and takes the list in one step, a registration checks
    /// the reason and pushes in one step, so each callback runs exactly
    /// once whichever comes first (F3). Never held across an await or a
    /// callback.
    refusal: std::sync::Mutex<Refusal>,
    reader: Mutex<Option<JoinHandle<()>>>,
    /// Aborted readers of sockets that were never published, kept until
    /// they finished: close joins them, so no reader still holds a socket
    /// half once close returns (R4-3). The supervisor prunes finished ones.
    retired_readers: std::sync::Mutex<Vec<JoinHandle<()>>>,
    /// The single task that ever dials again (spawned once at `connect`,
    /// aborted by `close`). A socket dying while it restores leaves
    /// `connected` false, which is exactly what it waits on — there is no
    /// "reconnect in progress" guard a disconnect can hide behind.
    supervisor: Mutex<Option<JoinHandle<()>>>,
    /// `notify_waiters` unwinds every send parked on a socket write: a
    /// backpressured peer can hold `sink` (and `pending`/`watch_flush`
    /// behind it) indefinitely, and the disconnect/close paths need those
    /// locks — so they signal this *before* waiting on them (R5). A
    /// notify reaches only waiters already registered; `teardowns` is
    /// what excludes a writer that registers later (R4-1).
    send_abort: tokio::sync::Notify,
    /// Test hook: when set, socket writes park inside their critical
    /// section — held locks stay held — until aborted or released, like a
    /// peer that keeps TCP open but stops reading (R5).
    #[cfg(test)]
    send_block: Mutex<Option<Arc<SendBlocker>>>,
    /// Test hook: named park points ([`WsRpcClient::park_at`]) — a code
    /// path that reaches an armed point announces it and waits for a permit.
    #[cfg(test)]
    park_points: std::sync::Mutex<HashMap<&'static str, Arc<SendBlocker>>>,
}

/// The park point behind [`Shared::send_block`]: `parked` announces each
/// write that reached the critical section; `gate` (zero permits) keeps
/// it there. The park only ends when the send is aborted or the test adds
/// a permit — whichever an R5 test wants to prove.
#[cfg(test)]
pub(crate) struct SendBlocker {
    parked: tokio::sync::mpsc::UnboundedSender<()>,
    pub(crate) gate: tokio::sync::Semaphore,
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
        let (closed_tx, _) = watch::channel(false);

        let shared = Arc::new(Shared {
            url: url.clone(),
            sink: Mutex::new(Some((0, write))),
            pending: Arc::new(Mutex::new(HashMap::new())),
            events: events_tx,
            connected: connected_tx,
            watches: Mutex::new(Watches {
                map: WatchMap::new(),
                rev: 0,
            }),
            watch_flush: Mutex::new(()),
            teardowns: AtomicU64::new(0),
            conn_guard: Mutex::new(()),
            on_reconnect: std::sync::Mutex::new(Vec::new()),
            closed: AtomicBool::new(false),
            closed_tx,
            auth_failed: AtomicBool::new(false),
            refusal: std::sync::Mutex::new(Refusal::default()),
            reader: Mutex::new(None),
            retired_readers: std::sync::Mutex::new(Vec::new()),
            supervisor: Mutex::new(None),
            send_abort: tokio::sync::Notify::new(),
            #[cfg(test)]
            send_block: Mutex::new(None),
            #[cfg(test)]
            park_points: std::sync::Mutex::new(HashMap::new()),
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
            let mut auth_reason: Option<String> = None;
            while let Some(msg) = read.next().await {
                let text = match msg {
                    Ok(Message::Text(t)) => {
                        // Blocks this worker mid-poll: an abort cannot
                        // end the task until the park is released.
                        #[cfg(test)]
                        WsRpcClient {
                            shared: shared_reader.clone(),
                        }
                        .test_sync_park("reader:frame");
                        t
                    }
                    Ok(Message::Close(frame)) => {
                        eprintln!("[ws-rpc] connection closed by server: {:?}", frame);
                        auth_reason = frame.and_then(|f| {
                            (f.code
                                == tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode::Policy)
                                .then(|| f.reason.to_string())
                        });
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
                    if let Some(call) = shared_reader.pending.lock().await.remove(&id) {
                        if let Some(error) = resp.error {
                            let _ = call.tx.send(Err(anyhow::Error::new(RpcCallError {
                                code: error.code.unwrap_or(500),
                                message: error.message.unwrap_or_default(),
                                data: error.data,
                            })));
                        } else {
                            let _ = call.tx.send(Ok(resp.result.unwrap_or(Value::Null)));
                        }
                    }
                } else {
                    // Server-push event — broadcast to subscribers
                    let _ = shared_reader.events.send(parsed);
                }
            }

            if let Some(reason) = auth_reason {
                let callbacks = {
                    let mut refusal = shared.refusal.lock().unwrap();
                    refusal.reason = Some(reason.clone());
                    shared.auth_failed.store(true, Ordering::SeqCst);
                    std::mem::take(&mut refusal.callbacks)
                };
                for (_, cb) in callbacks {
                    // A panicking callback must not skip the later ones or
                    // the terminal cleanup below (the list is already taken).
                    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| cb(&reason)));
                }
                // Terminal, like `close`: no socket will ever come back.
                shared.closed_tx.send_replace(true);
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
        // Unwind writes parked on this socket FIRST: a backpressured send
        // holds `sink` (and `pending`/`watch_flush` behind it) — the
        // locks this path takes next (R5). The epoch moves before the
        // notify: a writer registering after it still sees the teardown
        // under the sink lock (R4-1).
        shared.teardowns.fetch_add(1, Ordering::SeqCst);
        shared.send_abort.notify_waiters();
        #[cfg(test)]
        Self::park_point(shared, "disconnect:notified").await;
        {
            // `conn_guard` makes the disconnect's flag write atomic against
            // an install's epoch-check-and-publish: whichever holds it last
            // wins, and a publish that loses the race sees the bumped epoch
            // and refuses to overwrite this `false` (R1).
            let _guard = shared.conn_guard.lock().await;
            {
                let mut sink = shared.sink.lock().await;
                *sink = None; // drop whatever half remains
            }
            // Abort the reader when this runs from `disconnect_for_testing`
            // — dropping `read` closes the TCP stream executor-side too.
            if abort_reader {
                if let Some(reader) = shared.reader.lock().await.take() {
                    reader.abort();
                }
            }
            // `send_replace`, not `send`: with no live receivers `send` drops
            // the value and `is_connected` keeps reporting the stale flag.
            shared.connected.send_replace(false);
        }

        // Fail every *sent* call — the executor can't answer them now. An
        // unsent call is only waiting: its frame never went out, so there is
        // nothing to fail; it rides the next socket instead (F8, TS
        // `_onClose`). After `MAX_FAILED_CONNECTS` dead sockets it gives up.
        let mut map = shared.pending.lock().await;
        let ids: Vec<String> = map.keys().cloned().collect();
        for id in ids {
            let Some(call) = map.get_mut(&id) else {
                continue;
            };
            call.failed_connects += 1;
            if call.sent || call.failed_connects >= MAX_FAILED_CONNECTS {
                let call = map.remove(&id).unwrap();
                let _ = call.tx.send(Err(anyhow!("WebSocket connection closed")));
            }
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
                shared
                    .retired_readers
                    .lock()
                    .unwrap()
                    .retain(|h| !h.is_finished());
                failures += 1;
                let delay =
                    backoff_delay_ms(failures, RECONNECT_BASE_DELAY_MS, RECONNECT_MAX_DELAY_MS);
                tokio::time::sleep(Duration::from_millis(delay)).await;

                match connect_async_with_config(&shared.url, Some(ws_config()), false).await {
                    Ok((ws_stream, _)) => {
                        let (mut write, read) = ws_stream.split();
                        // The epoch this install owns: the reader does not
                        // exist yet, so `teardowns` can only move after
                        // this — a close landing while we restore is always
                        // caught at the guarded publish below (R1).
                        let epoch = shared.teardowns.load(Ordering::SeqCst);
                        let reader = AbortOnDrop::new(
                            Self::spawn_reader(read, shared.clone()),
                            shared.clone(),
                        );
                        // Restore interest *before* publishing: `connected`
                        // flips only once the executor acknowledged
                        // `events.watch`, so a call that waited out the
                        // outage — a `modelSubscribe` above all — can never
                        // slip past the filter it needs (R2). The frames go
                        // over the still-private write half: nothing else
                        // can see this socket yet.
                        let watches = match Self::restore_watches(&shared, &mut write).await {
                            Ok(watches) => watches,
                            Err(e) => {
                                eprintln!("[ws-rpc] watch restore failed: {e}");
                                // No publish, no callbacks, no re-open on a
                                // filter that is not there — drop the socket
                                // (the guard aborts its reader) and dial
                                // again.
                                drop(write);
                                drop(reader);
                                continue;
                            }
                        };
                        // `watches` is still held from the final revision
                        // check: a `set_watch` either landed before it (and
                        // the restore re-flushed) or waits until the
                        // publish below, then sees `connected` and flushes
                        // itself. No interest can fall between (N1).
                        #[cfg(test)]
                        Self::park_point(&shared, "install:checked").await;
                        {
                            let _guard = shared.conn_guard.lock().await;
                            if shared.closed.load(Ordering::SeqCst)
                                || shared.teardowns.load(Ordering::SeqCst) != epoch
                            {
                                // Died while we restored: its disconnect
                                // already ran and owns the flag. Drop what
                                // is left and dial again.
                                drop(write);
                                drop(reader);
                                continue;
                            }
                            *shared.sink.lock().await = Some((epoch, write));
                            *shared.reader.lock().await = Some(reader.disarm());
                            shared.connected.send_replace(true);
                        }
                        drop(watches);

                        // Receivers woken by `send_replace` were stashed in
                        // this worker's LIFO slot — a callback that parks
                        // the thread (a re-open that blocks, a test
                        // barrier) would trap them there and nothing would
                        // ever drain it. Yield so the waiters leave the
                        // slot before user code can park this worker.
                        tokio::task::yield_now().await;

                        // Subscription re-opens fired below need the filter
                        // the acknowledged restore just installed (R2).
                        let callbacks: Vec<ReconnectCb> = shared
                            .on_reconnect
                            .lock()
                            .unwrap()
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

    /// Snapshot the union and register its pending entry. Caller holds
    /// `watch_flush`. Returns the flushed watches revision, the request id,
    /// the serialized frame and the reply channel — everything a send needs
    /// except the sink itself, so no `watches`/`pending` acquisition can
    /// ever run under a held `shared.sink` (call paths take them in the
    /// opposite order).
    async fn watch_frame_parts(
        shared: &Arc<Shared>,
    ) -> Result<(u64, String, String, oneshot::Receiver<Result<Value>>)> {
        let (merged, rev) = {
            let watches = shared.watches.lock().await;
            (merge_watches(&watches.map), watches.rev)
        };
        // Send directly rather than via `call` — `call` on the freshly
        // installed socket works, but this keeps ordering explicit.
        let id = next_id();
        let msg = RpcMessage {
            id: id.clone(),
            msg_type: "events.watch".to_string(),
            params: merged,
        };
        let json = serde_json::to_string(&msg)?;
        let (tx, rx) = oneshot::channel();
        shared.pending.lock().await.insert(
            id.clone(),
            PendingCall {
                tx,
                sent: true,
                failed_connects: 0,
            },
        );
        Ok((rev, id, json, rx))
    }

    /// Await one `events.watch` reply — a real RPC, so a rejection (or a
    /// dropped socket, which fails the pending entry) reaches the caller
    /// (F9). Bounded: on timeout the pending entry is removed — just
    /// dropping the receiver would leave it to linger until disconnect
    /// (F12).
    async fn await_watch_reply(
        shared: &Arc<Shared>,
        id: &str,
        rx: oneshot::Receiver<Result<Value>>,
    ) -> Result<()> {
        match tokio::time::timeout(WATCH_REPLY_TIMEOUT, rx).await {
            Ok(reply) => reply
                .map_err(|_| anyhow!("Response channel closed"))?
                .map(|_| ()),
            Err(_) => {
                shared.pending.lock().await.remove(id);
                Err(anyhow!("events.watch reply timed out"))
            }
        }
    }

    /// Merge the named interests and send `events.watch` on the installed
    /// socket, awaiting the reply — an empty map too: it tells the executor
    /// to stop sending events (`events.watch` replaces the socket's entire
    /// interest, TS `ApiClient._flushWatch`). The `watch_flush` gate
    /// serializes the flush: a snapshot taken earlier can never write its
    /// stale union over a newer one on the wire (F9). The sink is held
    /// only across the frame send, never across the reply wait.
    async fn send_watch(shared: &Arc<Shared>) -> Result<()> {
        let _gate = shared.watch_flush.lock().await;
        let (_rev, id, json, rx) = Self::watch_frame_parts(shared).await?;
        #[cfg(test)]
        Self::park_point(shared, "watch:built").await;
        // The send — lock acquisition included — is bounded and
        // abortable: a peer that stops reading must not hold `sink` (and
        // `watch_flush` behind it) while disconnect or close waits on
        // those locks; they signal `send_abort` before waiting (R5).
        // The sink is held only across the send, never the reply wait.
        let abort = shared.send_abort.notified();
        tokio::pin!(abort);
        abort.as_mut().enable();
        // `None` inside: no socket. `Some(None)`: a teardown began before
        // the sink was taken (the notify may have come before this waiter
        // registered), so the stamped sink is refused (R4-1).
        let sent = tokio::select! {
            r = tokio::time::timeout(WATCH_SEND_TIMEOUT, async {
                let mut sink = shared.sink.lock().await;
                match sink.as_mut() {
                    Some((epoch, _)) if *epoch != shared.teardowns.load(Ordering::SeqCst) => {
                        Some(None)
                    }
                    Some((_, s)) => {
                        #[cfg(test)]
                        Self::park_for_send_test(shared).await;
                        Some(Some(s.send(Message::Text(json)).await))
                    }
                    None => None,
                }
            }) => Some(r),
            _ = &mut abort => None,
        };
        match sent {
            Some(Ok(Some(Some(Ok(()))))) => Self::await_watch_reply(shared, &id, rx).await,
            other => {
                shared.pending.lock().await.remove(&id);
                Err(match other {
                    Some(Ok(Some(Some(Err(e))))) => anyhow!("Failed to send events.watch: {e}"),
                    Some(Err(_)) => anyhow!("events.watch send timed out"),
                    None | Some(Ok(Some(None))) => {
                        anyhow!("events.watch send aborted: socket is closing")
                    }
                    _ => anyhow!("events.watch: no socket"),
                })
            }
        }
    }

    /// `send_watch` for the install path: the same serialized flush, but on
    /// the still-private write half before the socket exists for callers —
    /// and repeated until the acknowledged union is the union that exists,
    /// so interest recorded mid-restore is never silently dropped (R2).
    /// Returns the `watches` guard of the final revision check: the caller
    /// publishes the socket under it, so no `set_watch` can record interest
    /// between that check and publication (N1).
    async fn restore_watches<'a>(
        shared: &'a Arc<Shared>,
        sink: &mut WsSink,
    ) -> Result<tokio::sync::MutexGuard<'a, Watches>> {
        let _gate = shared.watch_flush.lock().await;
        loop {
            let (rev, id, json, rx) = Self::watch_frame_parts(shared).await?;
            // The same bound + abort as the installed-socket flush: a
            // peer that stops reading mid-restore must not pin the
            // supervisor on this write (R5).
            let abort = shared.send_abort.notified();
            tokio::pin!(abort);
            abort.as_mut().enable();
            let sent = tokio::select! {
                r = tokio::time::timeout(WATCH_SEND_TIMEOUT, async {
                    #[cfg(test)]
                    Self::park_for_send_test(shared).await;
                    sink.send(Message::Text(json)).await
                }) => Some(r),
                _ = &mut abort => None,
            };
            match sent {
                Some(Ok(Ok(()))) => Self::await_watch_reply(shared, &id, rx).await?,
                other => {
                    shared.pending.lock().await.remove(&id);
                    return Err(match other {
                        Some(Ok(Err(e))) => anyhow!("Failed to send events.watch: {e}"),
                        Some(Err(_)) => anyhow!("events.watch send timed out"),
                        None => anyhow!("events.watch restore aborted: socket is closing"),
                        Some(Ok(Ok(()))) => unreachable!(),
                    });
                }
            }
            let watches = shared.watches.lock().await;
            if rev >= watches.rev {
                return Ok(watches);
            }
        }
    }

    /// Register `key`'s event interest; the merged union is sent on the socket.
    /// Live queries use this so a subscription watch survives reconnects.
    pub(crate) async fn set_watch(&self, key: &str, spec: Value) -> Result<()> {
        // `connected` is read under the same `watches` lock an install
        // holds from its final revision check through publication (N1).
        let connected = {
            let mut watches = self.shared.watches.lock().await;
            watches.map.insert(key.to_string(), spec);
            watches.rev += 1;
            *self.shared.connected.borrow()
        };
        // A dead socket sends nothing — the next install's restore flush
        // picks the interest up instead (R2); a live socket flushes now.
        if connected {
            Self::send_watch(&self.shared).await?;
        }
        Ok(())
    }

    pub(crate) async fn remove_watch(&self, key: &str) -> Result<()> {
        let (removed, connected) = {
            let mut watches = self.shared.watches.lock().await;
            let removed = watches.map.remove(key).is_some();
            if removed {
                watches.rev += 1;
            }
            (removed, *self.shared.connected.borrow())
        };
        // A key that was never installed changed nothing — a flush would
        // only resend the same union.
        if removed && connected {
            Self::send_watch(&self.shared).await?;
        }
        Ok(())
    }

    /// `callback` runs after each successful reconnect, once the watch was
    /// re-sent. Returns an id for `remove_on_reconnect`.
    pub async fn on_reconnect(&self, callback: impl Fn() + Send + Sync + 'static) -> u64 {
        self.register_on_reconnect(Arc::new(callback))
    }

    pub async fn remove_on_reconnect(&self, id: u64) {
        self.unregister_on_reconnect(id);
    }

    /// [`on_reconnect`](Self::on_reconnect) without an await: the id is the
    /// caller's the moment the callback is in, with no cancellation point
    /// between (N6).
    pub(crate) fn register_on_reconnect(&self, callback: ReconnectCb) -> u64 {
        static CB_ID: AtomicU64 = AtomicU64::new(0);
        let id = CB_ID.fetch_add(1, Ordering::Relaxed);
        self.shared
            .on_reconnect
            .lock()
            .unwrap()
            .push((id, callback));
        id
    }

    pub(crate) fn unregister_on_reconnect(&self, id: u64) {
        self.shared
            .on_reconnect
            .lock()
            .unwrap()
            .retain(|(cb_id, _)| *cb_id != id);
    }

    /// `callback(reason)` runs once if the executor closes the socket refusing
    /// the token (close code 1008): expired, revoked, or bad signature — like
    /// TS `ApiClient.onAuthFailed`. No reconnect follows; `reason` is the
    /// server's close reason. If the refusal already happened, the callback
    /// fires at once (on the caller's task). A callback that panics in the
    /// reader does not stop the later callbacks or the terminal cleanup.
    /// Returns an id for `remove_on_auth_failed`.
    pub async fn on_auth_failed(&self, callback: impl Fn(&str) + Send + Sync + 'static) -> u64 {
        static CB_ID: AtomicU64 = AtomicU64::new(0);
        let id = CB_ID.fetch_add(1, Ordering::Relaxed);
        #[cfg(test)]
        Self::park_point(&self.shared, "auth:register").await;
        let reason = {
            let mut refusal = self.shared.refusal.lock().unwrap();
            match refusal.reason.clone() {
                Some(reason) => reason,
                None => {
                    refusal.callbacks.push((id, Arc::new(callback)));
                    return id;
                }
            }
        };
        // Refused before this registration: the reader already ran its
        // list, so this one is ours to run — outside the lock.
        callback(&reason);
        id
    }

    pub async fn remove_on_auth_failed(&self, id: u64) {
        self.shared
            .refusal
            .lock()
            .unwrap()
            .callbacks
            .retain(|(cb_id, _)| *cb_id != id);
    }

    /// Whether the executor closed the socket refusing the token (close code
    /// 1008). When true, no reconnect runs and new calls fail fast.
    pub fn auth_failed(&self) -> bool {
        self.shared.auth_failed.load(Ordering::SeqCst)
    }

    /// The close reason the server sent with the refusal, if any.
    pub async fn auth_failed_reason(&self) -> Option<String> {
        self.shared.refusal.lock().unwrap().reason.clone()
    }

    /// Whether a socket is currently installed.
    pub fn is_connected(&self) -> bool {
        *self.shared.connected.borrow()
    }

    /// Permanent-close signal: `true` once `close()` ran or the executor
    /// refused auth. Live queries end their streams on it.
    pub(crate) fn closed(&self) -> watch::Receiver<bool> {
        self.shared.closed_tx.subscribe()
    }

    /// The raw server-push event stream (before any `events.watch` filter —
    /// the executor only sends what a watched socket asked for).
    pub(crate) fn events(&self) -> broadcast::Receiver<Value> {
        self.shared.events.subscribe()
    }

    /// Permanently close the client: socket down, no reconnect.
    pub async fn close(&self) {
        self.shared.closed.store(true, Ordering::SeqCst);
        // Same first moves as the disconnect: mark the teardown, then
        // unwind parked socket writes before this path waits on the locks
        // they hold (R5, R4-1).
        self.shared.teardowns.fetch_add(1, Ordering::SeqCst);
        self.shared.send_abort.notify_waiters();
        #[cfg(test)]
        Self::park_point(&self.shared, "close:notified").await;
        // Abort, then wait for the task to end: an aborted supervisor drops
        // its install state — an unpublished socket's reader guard and
        // write half included (N4). The guard aborts that reader and
        // retires its handle; the reader is joined below with the rest,
        // so no task holds the socket once close returns (R4-3). The
        // locks are released before the wait: the tasks' own teardown
        // takes them.
        let supervisor = self.shared.supervisor.lock().await.take();
        if let Some(h) = supervisor {
            h.abort();
            let _ = h.await;
        }
        let retired = std::mem::take(&mut *self.shared.retired_readers.lock().unwrap());
        #[cfg(test)]
        Self::park_point(&self.shared, "close:readers").await;
        for h in retired {
            h.abort();
            let _ = h.await;
        }
        let reader = self.shared.reader.lock().await.take();
        if let Some(h) = reader {
            h.abort();
            let _ = h.await;
        }
        *self.shared.sink.lock().await = None;
        self.shared.connected.send_replace(false);
        // Wake the live queries' listeners: their streams end here too.
        self.shared.closed_tx.send_replace(true);
        let mut map = self.shared.pending.lock().await;
        for (_, call) in map.drain() {
            let _ = call.tx.send(Err(anyhow!("WebSocket connection closed")));
        }
    }

    /// Drop the live socket and walk the same disconnect path a dead socket
    /// walks: TCP closes server-side (subscriptions die), then the client
    /// reconnects. For tests that exercise reconnect/resync.
    #[doc(hidden)]
    pub async fn disconnect_for_testing(&self) {
        Self::handle_disconnect(&self.shared, true).await;
    }

    /// How many `on_reconnect` callbacks are currently registered — a leak
    /// shows up as a count that never returns to what the test installed.
    #[cfg(test)]
    pub(crate) async fn reconnect_callback_count(&self) -> usize {
        self.shared.on_reconnect.lock().unwrap().len()
    }

    /// Install a [`SendBlocker`]: socket writes now park inside their
    /// critical section — held locks stay held — and the returned
    /// receiver announces each park (R5).
    #[cfg(test)]
    pub(crate) async fn park_sends(
        &self,
    ) -> (Arc<SendBlocker>, tokio::sync::mpsc::UnboundedReceiver<()>) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let blocker = Arc::new(SendBlocker {
            parked: tx,
            gate: tokio::sync::Semaphore::new(0),
        });
        *self.shared.send_block.lock().await = Some(blocker.clone());
        (blocker, rx)
    }

    /// Park inside a socket write's critical section when a test put a
    /// [`SendBlocker`] in place — the locks the caller holds stay held,
    /// like a `SinkExt::send` a backpressured peer stalls forever (R5).
    #[cfg(test)]
    async fn park_for_send_test(shared: &Arc<Shared>) {
        let blocker = shared.send_block.lock().await.clone();
        if let Some(b) = blocker {
            let _ = b.parked.send(());
            let _ = b.gate.acquire().await;
        }
    }

    /// Arm the park point `name`: the next code paths that reach it
    /// announce it on the returned receiver and wait for a permit on the
    /// returned blocker's gate.
    #[cfg(test)]
    pub(crate) fn park_at(
        &self,
        name: &'static str,
    ) -> (Arc<SendBlocker>, tokio::sync::mpsc::UnboundedReceiver<()>) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let blocker = Arc::new(SendBlocker {
            parked: tx,
            gate: tokio::sync::Semaphore::new(0),
        });
        self.shared
            .park_points
            .lock()
            .unwrap()
            .insert(name, blocker.clone());
        (blocker, rx)
    }

    /// Park at `name` if a test armed it ([`park_at`](Self::park_at)).
    #[cfg(test)]
    async fn park_point(shared: &Arc<Shared>, name: &'static str) {
        let blocker = shared.park_points.lock().unwrap().get(name).cloned();
        if let Some(b) = blocker {
            let _ = b.parked.send(());
            let _ = b.gate.acquire().await.map(|p| p.forget());
        }
    }

    /// A park point for synchronous code (a constructor): it blocks the
    /// calling thread until a permit arrives, so call it only from a
    /// thread that may block (`spawn_blocking`, a blocked reader's worker
    /// in a multi-thread runtime).
    #[cfg(test)]
    pub(crate) fn test_sync_park(&self, name: &'static str) {
        let blocker = self.shared.park_points.lock().unwrap().get(name).cloned();
        if let Some(b) = blocker {
            let _ = b.parked.send(());
            loop {
                if let Ok(p) = b.gate.try_acquire() {
                    p.forget();
                    return;
                }
                std::thread::sleep(Duration::from_millis(1));
            }
        }
    }

    /// How many receivers of the terminal signal exist: a listener that
    /// handled the close dropped its own.
    #[cfg(test)]
    pub(crate) fn closed_receivers(&self) -> usize {
        self.shared.closed_tx.receiver_count()
    }

    /// [`park_point`](Self::park_point) for code outside this module that
    /// holds the client (live queries, models).
    #[cfg(test)]
    pub(crate) async fn test_park(&self, name: &'static str) {
        Self::park_point(&self.shared, name).await;
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
        let json = serde_json::to_string(&msg)?;

        let (tx, rx) = oneshot::channel();
        self.shared.pending.lock().await.insert(
            id.to_string(),
            PendingCall {
                tx,
                sent: false,
                failed_connects: 0,
            },
        );

        let mut connected_rx = self.shared.connected.subscribe();
        loop {
            // Register for the teardown signal FIRST, before the closed
            // check and before competing for any lock: `notify_waiters`
            // keeps nothing for a waiter that registers later, so a call
            // that registered after a close's notify could take the freed
            // sink and start a write close then waits behind (N2). Fresh
            // per attempt: a `Notified` an earlier socket's death resolved
            // would otherwise abort every later attempt.
            let abort = self.shared.send_abort.notified();
            tokio::pin!(abort);
            abort.as_mut().enable();
            if self.shared.closed.load(Ordering::SeqCst) {
                self.shared.pending.lock().await.remove(id);
                return Err(anyhow!("WebSocket client is closed"));
            }
            if self.shared.auth_failed.load(Ordering::SeqCst) {
                self.shared.pending.lock().await.remove(id);
                let reason = self
                    .shared
                    .refusal
                    .lock()
                    .unwrap()
                    .reason
                    .clone()
                    .unwrap_or_else(|| "token refused".to_string());
                return Err(anyhow!(
                    "WebSocket authentication refused: the executor refused the token ({reason})"
                ));
            }
            // `pending` then `sink` — the drain's order — so a socket death
            // cannot interleave between "marked sent" and "sent": either
            // the frame went out before the drain (an honest in-flight
            // failure) or the call stays a waiting one and rides the next
            // socket (F8). Lock waits and the write all race the abort.
            let attempt = async {
                let mut pending = self.shared.pending.lock().await;
                if pending.get(id).is_none() {
                    // Timed out or closed while waiting — whoever failed
                    // the call already told the caller; never send now.
                    return Attempt::Gone;
                }
                #[cfg(test)]
                Self::park_point(&self.shared, "call:before-sink").await;
                let mut sink = self.shared.sink.lock().await;
                // Recheck under the locks: a close that stored `closed`
                // before this attempt registered never reaches it through
                // the notify, only through the flag.
                if self.shared.closed.load(Ordering::SeqCst) {
                    return Attempt::Closed;
                }
                let s = match sink.as_mut() {
                    // A teardown began: the notify may have come before
                    // this attempt registered, so only the epoch tells.
                    // Nothing was written; the call waits (R4-1).
                    Some((epoch, _)) if *epoch != self.shared.teardowns.load(Ordering::SeqCst) => {
                        #[cfg(test)]
                        Self::park_point(&self.shared, "call:refused").await;
                        return Attempt::NoSocket;
                    }
                    Some((_, s)) => s,
                    None => return Attempt::NoSocket,
                };
                if let Some(call) = pending.get_mut(id) {
                    call.sent = true;
                }
                match tokio::time::timeout(CALL_SEND_TIMEOUT, async {
                    #[cfg(test)]
                    Self::park_for_send_test(&self.shared).await;
                    s.send(Message::Text(json.clone())).await
                })
                .await
                {
                    Ok(Ok(())) => Attempt::Sent,
                    Ok(Err(e)) => Attempt::Failed(anyhow!("Failed to send WS message: {e}")),
                    Err(_) => Attempt::Failed(anyhow!(
                        "Failed to send WS message: the write timed out after {}ms",
                        CALL_SEND_TIMEOUT.as_millis()
                    )),
                }
            };
            let outcome = tokio::select! {
                o = attempt => o,
                _ = &mut abort => Attempt::Aborted,
            };
            match outcome {
                Attempt::Sent => break,
                Attempt::Gone => return Err(anyhow!("WebSocket connection closed")),
                Attempt::Closed => {
                    self.shared.pending.lock().await.remove(id);
                    return Err(anyhow!("WebSocket client is closed"));
                }
                Attempt::Failed(e) => {
                    self.shared.pending.lock().await.remove(id);
                    return Err(e);
                }
                Attempt::Aborted => {
                    if self.shared.closed.load(Ordering::SeqCst) {
                        self.shared.pending.lock().await.remove(id);
                        return Err(anyhow!("WebSocket client is closed"));
                    }
                    // A disconnect: a call whose frame may have gone out
                    // fails; one that never wrote keeps waiting for the
                    // next socket (F8).
                    let mut pending = self.shared.pending.lock().await;
                    match pending.get(id) {
                        Some(call) if !call.sent => continue,
                        _ => {
                            pending.remove(id);
                            return Err(anyhow!(
                                "Failed to send WS message: the socket closed mid-write"
                            ));
                        }
                    }
                }
                Attempt::NoSocket => {
                    // Reconnecting — wait for a socket or the final close.
                    if connected_rx.changed().await.is_err() {
                        self.shared.pending.lock().await.remove(id);
                        return Err(anyhow!("WebSocket client is closed"));
                    }
                }
            }
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
        self.set_watch("", events).await?;
        Ok(rx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;
    use std::task::Poll;

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
        // The whole object: `merged[key] == Null` also holds for a key
        // the merge dropped.
        assert_eq!(
            merge_watches(&watches),
            serde_json::json!({
                "perspective-added": null,
                "query-subscription-update": null,
                "link-added": ["p1", "p2"],
            })
        );
    }

    #[test]
    fn merge_watches_null_wins_over_uuid_lists() {
        let mut watches = WatchMap::new();
        watches.insert("a".into(), serde_json::json!({"ev": ["p1"]}));
        watches.insert("b".into(), serde_json::json!({"ev": null}));
        assert_eq!(merge_watches(&watches), serde_json::json!({"ev": null}));
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

    /// R1: `connected` must go out before the reader can report a close —
    /// only then can a mid-install disconnect never be overwritten. The
    /// wrong order lets a close that lands between `spawn_reader` and
    /// `send_replace(true)` store `false`, the publish overwrite it with
    /// `true`, and the supervisor sleep on a flag that never flips again:
    /// dead socket, `is_connected` true, no redial ever.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn an_immediate_close_during_install_still_reconnects() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // Socket 1 dies; a drained in-flight call is the barrier that the
        // whole disconnect path ran.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection();
        assert!(c0.await.unwrap().is_err());

        // Park the restore: the fresh socket's reader already runs, but no
        // `events.watch` can go out — and no publish can land — while the
        // test holds the flush gate. The fresh socket dies meanwhile.
        let parked = client.shared.watch_flush.lock().await;
        fake.connection(2).await;

        // `teardowns` reaching 2 is the barrier that socket 2's disconnect
        // began before we release: the publish must refuse from then on.
        fake.drop_connection();
        tokio::time::timeout(Duration::from_secs(10), async {
            while client.shared.teardowns.load(Ordering::SeqCst) < 2 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("socket 2's disconnect never ran");
        drop(parked);

        // A publish that lands after that disconnect leaves `connected`
        // stuck at true on a dead socket: the supervisor waits on a change
        // that never comes and nobody dials epoch 3.
        fake.connection(3).await;
        client.close().await;
    }

    /// R2: a call parked while the client was down — a `modelSubscribe`
    /// above all — must not wake until the restored `events.watch` was
    /// acknowledged. If `connected` flips before the filter is in place,
    /// the subscribe slips past it and the live query never receives an
    /// update while believing itself ready.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_subscribe_while_disconnected_waits_for_the_watch_ack() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // Socket 1 dies; a drained in-flight call is the barrier that the
        // whole disconnect path ran.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection();
        assert!(c0.await.unwrap().is_err());

        // While offline the query records its interest — nothing can be
        // sent yet — and the subscribe call parks waiting for a socket.
        client
            .set_watch(
                "lq:1",
                serde_json::json!({"query-subscription-update": null}),
            )
            .await
            .unwrap();
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let sub = tokio::spawn(async move {
            caller
                .call_value(
                    "perspective.modelSubscribe",
                    serde_json::json!({"uuid": "p1"}),
                )
                .await
        });

        // Hold the flush gate: the restore cannot send `events.watch` while
        // the test holds it, so any frame that arrives now provably went
        // out without the filter.
        let gate = client.shared.watch_flush.lock().await;
        fake.connection(2).await;
        assert!(
            fake.maybe_request(Duration::from_millis(400))
                .await
                .is_none(),
            "the subscribe went out before events.watch was acknowledged"
        );

        // Released, the restore must send the union the offline record
        // joined — and while its ack is outstanding, still nothing else.
        drop(gate);
        let w = fake.next_request().await;
        assert_eq!(w["type"], "events.watch");
        assert_eq!(
            w["params"],
            serde_json::json!({"query-subscription-update": null})
        );
        assert!(
            fake.maybe_request(Duration::from_millis(400))
                .await
                .is_none(),
            "the subscribe went out before events.watch was acknowledged"
        );

        // Only the acknowledgement may wake the parked call.
        fake.reply(&w, Value::Bool(true)).await;
        let s = fake.next_request().await;
        assert_eq!(s["type"], "perspective.modelSubscribe");
        fake.reply(
            &s,
            serde_json::json!({"subscriptionId": "s1", "result": {"instances": []}, "revision": 0}),
        )
        .await;
        sub.await.unwrap().unwrap();
        client.close().await;
    }

    /// R2: a rejected `events.watch` restore is a socket without the event
    /// filter — publishing it and firing the reconnect callbacks re-opens
    /// live queries that then receive nothing. The socket must be dropped
    /// and re-dialed, callbacks only after an acknowledged restore.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_rejected_watch_restore_never_publishes_and_redials() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let fired = std::sync::Arc::new(AtomicU64::new(0));
        let fired2 = fired.clone();
        client
            .on_reconnect(move || {
                fired2.fetch_add(1, Ordering::SeqCst);
            })
            .await;

        // Register interest while connected — socket 2 must restore it.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch =
            tokio::spawn(
                async move { caller.set_watch("k", serde_json::json!({"e": null})).await },
            );
        let w0 = fake.next_request().await;
        fake.reply(&w0, Value::Bool(true)).await;
        watch.await.unwrap().unwrap();

        // Socket 1 dies; a drained in-flight call is the barrier that the
        // whole disconnect path ran.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection();
        assert!(c0.await.unwrap().is_err());

        // Socket 2's restore is rejected: on the buggy path the socket is
        // published anyway, the callbacks fire and nobody ever dials
        // again — `connection(3)` times out.
        fake.connection(2).await;
        let w = fake.next_request().await;
        assert_eq!(w["type"], "events.watch");
        fake.reply_error(&w, 400, "unknown event type").await;

        fake.connection(3).await;
        assert_eq!(
            fired.load(Ordering::SeqCst),
            0,
            "reconnect callbacks ran on a filter that failed"
        );

        // The retry restores the same union; this acknowledgement is what
        // finally publishes the socket and fires the callbacks.
        let w3 = fake.next_request().await;
        assert_eq!(w3["type"], "events.watch");
        assert_eq!(w3["params"], serde_json::json!({"e": null}));
        fake.reply(&w3, Value::Bool(true)).await;
        tokio::time::timeout(Duration::from_secs(10), async {
            while fired.load(Ordering::SeqCst) == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the successful restore never fired the callbacks");
        client.close().await;
    }

    /// F8: a call that never reached the wire is a *waiting* call, not an
    /// in-flight one — TS `_onClose` rejects only `sent` calls. Draining it
    /// as if it were sent fails the caller while the call still sends on
    /// the next socket: the executor commits a write the caller was told
    /// failed, and a retry duplicates it.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_waiting_call_is_not_failed_and_sends_on_the_next_socket() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // c0 rides socket 1 and dies with it — the disconnect barrier.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        client.disconnect_for_testing().await;
        assert!(c0.await.unwrap().is_err());

        // c1 now waits for a socket: it is pending but was never sent. Wait
        // for its entry to land — otherwise the drain below could race the
        // insert itself.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c1 = tokio::spawn(async move { caller.call_value("x.z", serde_json::json!({})).await });
        tokio::time::timeout(Duration::from_secs(10), async {
            while client.shared.pending.lock().await.is_empty() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("c1 never entered pending");

        // A second socket death (executor restarted again before the client
        // re-dialed) must not fail c1 — its frame never went out, so there is
        // nothing to fail.
        client.disconnect_for_testing().await;

        // The supervisor dials; on the new socket c1 finally sends. Ack the
        // restore frame — `connected` only publishes once it lands (R2) —
        // then keep draining until c1's call arrives.
        fake.connection(2).await;
        let c1_frame = loop {
            let req = fake.next_request().await;
            if req["type"] == "events.watch" {
                fake.reply(&req, Value::Bool(true)).await;
            } else if req["type"] == "x.z" {
                break req;
            }
        };
        // The caller gets the real reply — not a synthetic "closed" for a
        // write that went out anyway.
        fake.reply(&c1_frame, serde_json::json!({"done": true}))
            .await;
        let result = c1.await.unwrap();
        assert_eq!(result.unwrap(), serde_json::json!({"done": true}));
        client.close().await;
    }

    // R5: a peer that keeps TCP open but stops reading backpressures a
    // socket write forever — `SinkExt::send` holds `sink` (and
    // `watch_flush`/`pending` behind it) the whole time, while
    // disconnect and close wait on those very locks. The abort signal
    // must unwind a parked write the moment teardown needs its locks.

    /// A blocked `events.watch` send holding `sink` must not stall the
    /// disconnect — or the redial after it.
    #[tokio::test]
    async fn a_blocked_watch_send_does_not_hold_the_sink() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (_blocker, mut parked) = client.park_sends().await;
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch =
            tokio::spawn(
                async move { caller.set_watch("t", serde_json::json!({"e": null})).await },
            );
        parked.recv().await; // parked inside the critical section

        tokio::time::timeout(PROMPT, client.disconnect_for_testing())
            .await
            .expect("disconnect parked behind a blocked events.watch send");
        // Unwound by the abort, well inside the 5 s send timeout — not by it.
        let err = tokio::time::timeout(PROMPT, watch)
            .await
            .expect("the blocked watch send outlived the disconnect")
            .unwrap()
            .unwrap_err();
        assert!(err.to_string().contains("aborted"), "{err}");
        fake.connection(2).await;
        client.close().await;
    }

    /// A blocked call send holds `pending` while it parks — the
    /// disconnect drain needs `pending`, so the abort must release both.
    #[tokio::test]
    async fn a_blocked_call_send_does_not_hold_pending() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (_blocker, mut parked) = client.park_sends().await;
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let call =
            tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        parked.recv().await; // parked holding pending + sink

        tokio::time::timeout(PROMPT, client.disconnect_for_testing())
            .await
            .expect("disconnect parked behind a blocked call send");
        let err = tokio::time::timeout(PROMPT, call)
            .await
            .expect("the blocked call send outlived the disconnect")
            .unwrap()
            .unwrap_err();
        assert!(err.to_string().contains("mid-write"), "{err}");
        fake.connection(2).await;
        client.close().await;
    }

    /// A blocked call send must not stall the permanent-close path: the
    /// close signals parked writers *before* it waits on the sink.
    #[tokio::test]
    async fn a_blocked_call_send_does_not_delay_close() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (_blocker, mut parked) = client.park_sends().await;
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let call =
            tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        parked.recv().await;

        tokio::time::timeout(PROMPT, client.close())
            .await
            .expect("close parked behind a blocked call send");
        let err = tokio::time::timeout(PROMPT, call)
            .await
            .expect("the blocked call send outlived close")
            .unwrap()
            .unwrap_err();
        assert!(err.to_string().contains("closed"), "{err}");
    }

    /// A restore's blocked send sits on the private write half holding
    /// `watch_flush`; the socket dying mid-restore must unwind it and
    /// keep dialing — the park must not pin the client in limbo.
    #[tokio::test]
    async fn a_blocked_watch_restore_does_not_stop_reconnect() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // Live interest — every fresh socket must send a restore frame.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch =
            tokio::spawn(
                async move { caller.set_watch("t", serde_json::json!({"e": null})).await },
            );
        let req = fake.next_request().await;
        assert_eq!(req["type"], "events.watch");
        fake.reply(&req, Value::Bool(true)).await;
        watch.await.unwrap().unwrap();

        // Socket 2's restore send parks mid-install.
        let (_blocker, mut parked) = client.park_sends().await;
        client.disconnect_for_testing().await;
        fake.connection(2).await;
        parked.recv().await;

        // Killing this socket too must unwind the parked restore at once —
        // well inside the 5 s send timeout, which would also free it — and
        // let the supervisor dial socket 3.
        client.disconnect_for_testing().await;
        tokio::time::timeout(PROMPT, async {
            loop {
                if client.shared.watch_flush.try_lock().is_ok() {
                    return;
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the parked restore was not unwound by the disconnect");
        fake.connection(3).await;
        client.close().await;
    }

    /// F8 (terminal path): an auth refusal is permanent — a later call must
    /// fail immediately, not sit out its whole timeout on a client that can
    /// never reconnect.
    #[tokio::test]
    async fn an_auth_refused_client_fails_calls_fast() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // Close 1008 (policy): the executor's token refusal. The drained
        // in-flight call is the barrier that auth_failed was recorded.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let c0 = tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        fake.next_request().await;
        fake.drop_connection_with_code(1008);
        assert!(c0.await.unwrap().is_err());

        // No reconnect may come: a new call has to fail now — bounded by a
        // fraction of the 30 s call timeout the buggy wait would sit out.
        let result = tokio::time::timeout(
            Duration::from_secs(5),
            client.call::<Value>("x.z", serde_json::json!({})),
        )
        .await
        .expect("a call on an auth-refused client must not wait out its timeout");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("auth"), "unexpected error: {err}");
        client.close().await;
    }

    /// F9: `events.watch` is a real RPC — a rejection must reach the
    /// caller, not vanish into a fire-and-forget oneshot.
    #[tokio::test]
    async fn watch_events_propagates_the_executors_rejection() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch = tokio::spawn(async move {
            caller
                .watch_events(serde_json::json!({"bogus-event": null}))
                .await
        });
        let req = fake.next_request().await;
        assert_eq!(req["type"], "events.watch");
        fake.reply_error(&req, 400, "unknown event type").await;

        let result = watch.await.unwrap();
        assert!(
            result.is_err(),
            "a rejected events.watch must not report success"
        );
        client.close().await;
    }

    /// F12: a socket that stays up but never answers `events.watch` must
    /// not keep the pending entry — the reply wait is bounded, and the
    /// timeout removes the entry rather than just dropping the receiver.
    #[tokio::test]
    async fn a_watch_reply_timeout_removes_its_pending_entry() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch =
            tokio::spawn(
                async move { caller.set_watch("k", serde_json::json!({"e": null})).await },
            );
        let req = fake.next_request().await;
        assert_eq!(req["type"], "events.watch");
        // Never reply: the watch's own timeout must end the wait.
        let result = tokio::time::timeout(Duration::from_secs(8), watch)
            .await
            .expect("a silent executor must not hold a watch flush forever")
            .unwrap();
        assert!(result.is_err(), "the unanswered watch must fail");
        assert!(
            client.shared.pending.lock().await.is_empty(),
            "the watch's pending entry was left behind"
        );
        client.close().await;
    }

    /// The serialized flush: each change sends the union as of its own
    /// snapshot — sequential changes land on the wire in order, cumulative.
    #[tokio::test]
    async fn watch_changes_send_each_union_in_order() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // set_watch awaits the watch's reply — drive it from a task so the
        // test can answer.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let s1 = tokio::spawn(async move {
            caller
                .set_watch("k1", serde_json::json!({"e1": null}))
                .await
        });
        let w1 = fake.next_request().await;
        fake.reply(&w1, Value::Bool(true)).await;
        s1.await.unwrap().unwrap();

        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let s2 = tokio::spawn(async move {
            caller
                .set_watch("k2", serde_json::json!({"e2": null}))
                .await
        });
        let w2 = fake.next_request().await;
        fake.reply(&w2, Value::Bool(true)).await;
        s2.await.unwrap().unwrap();

        assert_eq!(w1["type"], "events.watch");
        assert_eq!(w1["params"], serde_json::json!({"e1": null}));
        assert_eq!(w2["params"], serde_json::json!({"e1": null, "e2": null}));
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
        // The whole filter, not `w[key] == Null`: indexing an absent key
        // also yields `Null`, so that form passes on an empty watch.
        let filter = serde_json::json!({"query-subscription-update": null});
        assert_eq!(first_watch, filter);

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

        // The watch must have been re-sent on the new socket, with the
        // same filter: the key present, every perspective (`null`).
        let restored = tokio::time::timeout(Duration::from_secs(5), watches_rx.recv())
            .await
            .expect("events.watch was not re-sent after reconnect")
            .unwrap();
        assert_eq!(restored, filter, "the restored watch lost its filter");

        // A call on the reconnected socket answers.
        let result: Value = client
            .call("test.echo", serde_json::json!({}))
            .await
            .unwrap();
        assert_eq!(result, Value::Bool(true));
        drop(rx);
        client.close().await;
    }

    /// Close 1008 = the token was refused: `auth_failed`, the callback fires
    /// with the server's reason, calls fail fast, and no reconnect is tried.
    #[tokio::test]
    async fn a_policy_close_marks_auth_failed_and_stops_reconnecting() {
        use std::sync::atomic::AtomicU64;
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;
        use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
        use tokio_tungstenite::tungstenite::protocol::frame::CloseFrame;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let conns = Arc::new(AtomicU64::new(0));
        let conns2 = conns.clone();

        tokio::spawn(async move {
            while let Ok((stream, _)) = listener.accept().await {
                conns2.fetch_add(1, Ordering::SeqCst);
                tokio::spawn(async move {
                    let mut ws = accept_async(stream).await.unwrap();
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    let _ = ws
                        .send(Message::Close(Some(CloseFrame {
                            code: CloseCode::Policy,
                            reason: "Unauthorized access".into(),
                        })))
                        .await;
                });
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "some-token").await.unwrap();

        let (tx, rx) = oneshot::channel::<String>();
        let tx = std::sync::Mutex::new(Some(tx));
        client
            .on_auth_failed(move |reason| {
                if let Some(tx) = tx.lock().unwrap().take() {
                    let _ = tx.send(reason.to_string());
                }
            })
            .await;

        let reason = tokio::time::timeout(Duration::from_secs(5), rx)
            .await
            .expect("on_auth_failed never fired")
            .unwrap();
        assert_eq!(reason, "Unauthorized access");
        assert!(client.auth_failed());
        assert_eq!(
            client.auth_failed_reason().await.as_deref(),
            Some("Unauthorized access")
        );

        // A call fails fast — not after its 30 s budget.
        let started = std::time::Instant::now();
        let err = client
            .call::<Value>("test.method", serde_json::json!({}))
            .await
            .unwrap_err();
        assert!(started.elapsed() < Duration::from_secs(5));
        assert!(err.to_string().contains("refused the token"), "{err}");

        // A refused token must not dial again: the first backoff is ~1 s, so
        // a second accept inside ~2 s would mean a reconnect was tried.
        tokio::time::sleep(Duration::from_millis(2200)).await;
        assert_eq!(conns.load(Ordering::SeqCst), 1);
        client.close().await;
    }

    /// Close 1013 = the wallet is locked: a retryable state, not a dead
    /// token — `auth_failed` stays false and the reconnect task runs.
    #[tokio::test]
    async fn a_locked_close_reconnects_instead_of_failing_auth() {
        use std::sync::atomic::AtomicU64;
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;
        use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
        use tokio_tungstenite::tungstenite::protocol::frame::CloseFrame;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let (second_conn_tx, mut second_conn_rx) = tokio::sync::mpsc::unbounded_channel::<()>();
        let conns = Arc::new(AtomicU64::new(0));
        let conns2 = conns.clone();

        tokio::spawn(async move {
            while let Ok((stream, _)) = listener.accept().await {
                if conns2.fetch_add(1, Ordering::SeqCst) == 1 {
                    let _ = second_conn_tx.send(());
                }
                tokio::spawn(async move {
                    let mut ws = accept_async(stream).await.unwrap();
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    let _ = ws
                        .send(Message::Close(Some(CloseFrame {
                            code: CloseCode::from(1013u16),
                            reason: "Executor is locked".into(),
                        })))
                        .await;
                });
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "some-token").await.unwrap();

        tokio::time::timeout(Duration::from_secs(10), second_conn_rx.recv())
            .await
            .expect("the executor never saw a reconnect")
            .unwrap();
        assert!(!client.auth_failed());
        client.close().await;
    }

    // Round 4: the install/close/registration windows (N1, N2, N4, and
    // post-ack coverage for R1). Every barrier is a park point or a fake
    // executor event; the only timeouts are failure guards, each well
    // under the send/reply budgets (5 s) a fallback could pass on.

    /// The prompt-unwind bound: a teardown that only finishes on a 5 s
    /// send/reply timeout (or a 30 s call budget) fails it.
    const PROMPT: Duration = Duration::from_millis(1_500);

    /// F3: an `on_auth_failed` registration racing the refusal still runs
    /// exactly once. The registration parks after it decided "not refused
    /// yet"; the reader then records the 1008 and runs its callbacks (the
    /// terminal signal is the barrier that it finished); the registration
    /// resumes. Before the fix the callback landed in a list nobody would
    /// ever read again.
    #[tokio::test]
    async fn an_auth_failed_registration_racing_the_refusal_runs_once() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();
        let (gate, mut parked) = client.park_at("auth:register");

        let runs = Arc::new(AtomicU64::new(0));
        let (ran_tx, mut ran) = tokio::sync::mpsc::unbounded_channel::<String>();
        let registering = {
            let c = WsRpcClient {
                shared: client.shared.clone(),
            };
            let runs = runs.clone();
            tokio::spawn(async move {
                c.on_auth_failed(move |reason| {
                    runs.fetch_add(1, Ordering::SeqCst);
                    let _ = ran_tx.send(reason.to_string());
                })
                .await
            })
        };
        // Parked: the registration has not stored its callback yet (or,
        // fixed, is about to take the lock the reader's snapshot uses).
        tokio::time::timeout(PROMPT, parked.recv())
            .await
            .expect("the registration never reached its park point");

        let mut closed = client.shared.closed_tx.subscribe();
        fake.drop_connection_with_code(1008);
        tokio::time::timeout(PROMPT, closed.wait_for(|c| *c))
            .await
            .expect("the reader never recorded the refusal")
            .unwrap();
        assert!(client.auth_failed());

        gate.gate.add_permits(1);
        tokio::time::timeout(PROMPT, registering)
            .await
            .expect("the registration never returned")
            .unwrap();
        tokio::time::timeout(PROMPT, ran.recv())
            .await
            .expect("the callback registered across the refusal never ran");
        assert_eq!(runs.load(Ordering::SeqCst), 1);

        // A registration after the refusal runs at once, exactly once too
        // (the park point stays armed: let it through).
        gate.gate.add_permits(1);
        let late = Arc::new(AtomicU64::new(0));
        let late2 = late.clone();
        client
            .on_auth_failed(move |_| {
                late2.fetch_add(1, Ordering::SeqCst);
            })
            .await;
        assert_eq!(late.load(Ordering::SeqCst), 1);
        assert_eq!(runs.load(Ordering::SeqCst), 1);
        client.close().await;
    }

    /// Round 3 N2: a panicking refusal callback must not skip the later
    /// callbacks or the terminal cleanup (closed signal, disconnected).
    #[tokio::test]
    async fn a_panicking_auth_failed_callback_does_not_skip_the_rest() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();
        client
            .on_auth_failed(|_| panic!("the first refusal callback panics"))
            .await;
        let (ran_tx, mut ran) = tokio::sync::mpsc::unbounded_channel::<String>();
        client
            .on_auth_failed(move |reason| {
                let _ = ran_tx.send(reason.to_string());
            })
            .await;

        let mut closed = client.shared.closed_tx.subscribe();
        let mut connected = client.shared.connected.subscribe();
        fake.drop_connection_with_code(1008);
        tokio::time::timeout(PROMPT, ran.recv())
            .await
            .expect("the callback after the panicking one never ran")
            .expect("the callback after the panicking one was dropped unrun");
        tokio::time::timeout(PROMPT, closed.wait_for(|c| *c))
            .await
            .expect("no terminal signal after the panicking callback")
            .unwrap();
        tokio::time::timeout(PROMPT, connected.wait_for(|c| !*c))
            .await
            .expect("the disconnect cleanup never ran")
            .unwrap();
        assert!(client.auth_failed());
        client.close().await;
    }

    /// N1: a watch recorded after the restore's final revision check, but
    /// before publication, must still reach the executor. Before the fix
    /// `set_watch` saw `connected == false`, recorded the interest and
    /// returned — the publish followed without another flush, so no frame
    /// ever installed the filter.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_watch_recorded_after_the_final_restore_check_is_installed() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (gate, mut parked) = client.park_at("install:checked");
        client.disconnect_for_testing().await;
        fake.connection(2).await;
        let restore = fake.next_request().await;
        assert_eq!(restore["type"], "events.watch");
        assert_eq!(restore["params"], serde_json::json!({}));
        fake.reply(&restore, Value::Bool(true)).await;
        tokio::time::timeout(PROMPT, parked.recv())
            .await
            .expect("the install never passed its final revision check");

        // First live-query interest, between check and publication. The
        // future is polled once by hand with this task's waker, not
        // spawned: the install holds `watches`, so a fixed client cannot
        // finish it now. The buggy one read `connected == false`, recorded
        // the interest and returned Ready, with nothing left to flush it.
        let watch = client.set_watch("q", serde_json::json!({"query-subscription-update": null}));
        tokio::pin!(watch);
        let first = std::future::poll_fn(|cx| Poll::Ready(watch.as_mut().poll(cx))).await;
        assert!(
            first.is_pending(),
            "set_watch finished while the install held its final revision check: {first:?}"
        );
        gate.gate.add_permits(1);

        // The same future, driven while the executor is served: it must
        // send the interest itself once the socket is published.
        let serve = async {
            let frame = fake
                .maybe_request(PROMPT)
                .await
                .expect("no events.watch ever installed the interest recorded before publication");
            assert_eq!(frame["type"], "events.watch");
            assert_eq!(
                frame["params"],
                serde_json::json!({"query-subscription-update": null})
            );
            fake.reply(&frame, Value::Bool(true)).await;
        };
        let (result, ()) = tokio::time::timeout(PROMPT, async { tokio::join!(watch, serve) })
            .await
            .expect("set_watch never completed after publication");
        result.unwrap();
        assert!(client.is_connected());
        client.close().await;
    }

    /// R1, post-ack: the socket dies after its restore was acknowledged but
    /// before publication. The epoch guard must refuse the publish (the
    /// disconnect owns the flag) and the supervisor must dial again. The
    /// older R1 test kills the socket before the ack, where the failed
    /// restore alone prevents publication.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_close_after_the_restore_ack_but_before_publication_redials() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (gate, mut parked) = client.park_at("install:checked");
        // Through the fake: its drop counter must match the socket epoch.
        fake.drop_connection();
        fake.connection(2).await;
        let restore = fake.next_request().await;
        fake.reply(&restore, Value::Bool(true)).await;
        tokio::time::timeout(PROMPT, parked.recv())
            .await
            .expect("the install never passed its final revision check");

        // Socket 2 dies, acknowledged but unpublished; its disconnect has
        // run once the epoch moved.
        let before = client.shared.teardowns.load(Ordering::SeqCst);
        fake.drop_connection();
        tokio::time::timeout(PROMPT, async {
            while client.shared.teardowns.load(Ordering::SeqCst) == before {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("socket 2's disconnect never ran");
        // Only then may the parked install try to publish.
        gate.gate.add_permits(1);

        fake.connection(3).await;
        let restore = fake.next_request().await;
        fake.reply(&restore, Value::Bool(true)).await;
        client.close().await;
    }

    /// N2: close must unwind a call that queued for the sink *before*
    /// registering its cancellation. The call passes the closed check and
    /// takes `pending`, then waits behind a blocked watch send. Close
    /// stores `closed` and notifies; the watch unwinds and frees the sink.
    /// The queued call must not then start a fresh blocked write that pins
    /// close behind the sink lock.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn close_unwinds_a_call_queued_before_its_cancellation_registered() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        // A watch send parks inside the sink critical section.
        let (_blocker, mut send_parked) = client.park_sends().await;
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let watch =
            tokio::spawn(
                async move { caller.set_watch("t", serde_json::json!({"e": null})).await },
            );
        send_parked.recv().await;

        // A call reaches the point just before it competes for the sink.
        let (call_gate, mut call_parked) = client.park_at("call:before-sink");
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let call =
            tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        call_parked.recv().await;

        // Close notifies, then waits on its locks. The test holds the
        // first of them, so the queued call reaches the freed sink ahead
        // of close — as a call queued on the sink mutex before close does.
        let held = client.shared.supervisor.lock().await;
        let closer = WsRpcClient {
            shared: client.shared.clone(),
        };
        let close = tokio::spawn(async move { closer.close().await });
        // The notify unwound the watch send: the sink is free.
        tokio::time::timeout(PROMPT, async {
            loop {
                if client.shared.closed.load(Ordering::SeqCst)
                    && client.shared.sink.try_lock().is_ok()
                {
                    return;
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("close never unwound the blocked watch send");
        // Now the queued call goes on: it either starts a fresh write
        // (parks in it) or sees the close and ends.
        call_gate.gate.add_permits(1);
        let mut call = call;
        let ended = tokio::time::timeout(PROMPT, async {
            tokio::select! {
                _ = send_parked.recv() => None,
                r = &mut call => Some(r),
            }
        })
        .await
        .expect("the queued call neither wrote nor ended");
        drop(held);

        tokio::time::timeout(PROMPT, close)
            .await
            .expect("close waited behind a call that registered its cancellation too late")
            .unwrap();
        let result = match ended {
            Some(r) => r,
            None => tokio::time::timeout(PROMPT, call)
                .await
                .expect("the queued call outlived close"),
        };
        result.unwrap().unwrap_err();
        watch.await.unwrap().unwrap_err();
    }

    /// N4: close while the restore waits for its (withheld) reply must
    /// release the fresh, unpublished socket — its reader and both halves —
    /// not detach the reader with the supervisor's local handle.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn close_during_restore_releases_the_unpublished_socket() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        client.disconnect_for_testing().await;
        assert!(fake.sockets_ended(1, PROMPT).await, "socket 1 never closed");
        fake.connection(2).await;
        let restore = fake.next_request().await;
        assert_eq!(restore["type"], "events.watch");
        // The reply is withheld: the supervisor waits on it.

        tokio::time::timeout(PROMPT, client.close())
            .await
            .expect("close stalled");
        assert!(
            fake.sockets_ended(2, PROMPT).await,
            "the unpublished socket stayed open after close"
        );
        // Nothing but the client itself still holds the shared state: no
        // reader (or supervisor) task outlived close.
        assert_eq!(Arc::strong_count(&client.shared), 1);
    }

    /// R4-1, call path: a disconnect has notified `send_abort` but not yet
    /// cleared the sink. A call that registers its waiter only now (a new
    /// call, or an unsent one retrying after the first abort) must not
    /// start a write on the socket being torn down: that write would hold
    /// `sink` and `pending` while the disconnect waits on them.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_call_admitted_after_the_disconnect_notified_never_writes_on_the_old_socket() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (_blocker, mut send_parked) = client.park_sends().await;
        let (disconnect_gate, mut disconnect_parked) = client.park_at("disconnect:notified");
        let (refused_gate, mut refused) = client.park_at("call:refused");
        refused_gate.gate.add_permits(1000);
        let disconnector = WsRpcClient {
            shared: client.shared.clone(),
        };
        let disconnect = tokio::spawn(async move { disconnector.disconnect_for_testing().await });
        disconnect_parked.recv().await;

        // The disconnect notified and paused before it takes the sink.
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let call =
            tokio::spawn(async move { caller.call_value("x.y", serde_json::json!({})).await });
        let wrote = tokio::time::timeout(PROMPT, async {
            tokio::select! {
                _ = send_parked.recv() => true,
                _ = refused.recv() => false,
            }
        })
        .await
        .expect("the late call neither wrote nor was refused");
        disconnect_gate.gate.add_permits(1);
        tokio::time::timeout(PROMPT, disconnect)
            .await
            .expect("the disconnect waited behind a write admitted after its notify")
            .unwrap();
        assert!(
            !wrote,
            "a call admitted after the notify wrote on the old socket"
        );

        // Never sent, so the call rides the next socket.
        *client.shared.send_block.lock().await = None;
        fake.connection(2).await;
        let restore = fake.next_request().await;
        assert_eq!(restore["type"], "events.watch");
        fake.reply(&restore, Value::Bool(true)).await;
        let frame = fake.next_request().await;
        assert_eq!(frame["type"], "x.y");
        fake.reply(&frame, serde_json::json!(7)).await;
        let result = tokio::time::timeout(PROMPT, call)
            .await
            .expect("the call never completed on socket 2")
            .unwrap()
            .unwrap();
        assert_eq!(result, serde_json::json!(7));
        client.close().await;
    }

    /// R4-1, watch path: a watch flush has built its pending entry but not
    /// registered its waiter when close stores `closed` and notifies. The
    /// flush then reaches the old sink before close clears it; it must
    /// refuse, not start a write close then waits behind.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_watch_flush_admitted_after_close_notified_never_writes() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        let (_blocker, mut send_parked) = client.park_sends().await;
        let (built_gate, mut built) = client.park_at("watch:built");
        let (close_gate, mut close_parked) = client.park_at("close:notified");
        let caller = WsRpcClient {
            shared: client.shared.clone(),
        };
        let mut watch =
            tokio::spawn(
                async move { caller.set_watch("t", serde_json::json!({"e": null})).await },
            );
        built.recv().await;

        let closer = WsRpcClient {
            shared: client.shared.clone(),
        };
        let close = tokio::spawn(async move { closer.close().await });
        close_parked.recv().await;
        // Close notified and paused before it takes the sink; the flush
        // registers its waiter only now.
        built_gate.gate.add_permits(1);
        let ended = tokio::time::timeout(PROMPT, async {
            tokio::select! {
                _ = send_parked.recv() => None,
                r = &mut watch => Some(r),
            }
        })
        .await
        .expect("the late watch flush neither wrote nor ended");
        close_gate.gate.add_permits(1);
        tokio::time::timeout(PROMPT, close)
            .await
            .expect("close waited behind a watch write admitted after its notify")
            .unwrap();
        let result = match ended {
            Some(r) => r,
            None => tokio::time::timeout(PROMPT, watch)
                .await
                .expect("the late watch flush outlived close"),
        };
        let err = result.unwrap().unwrap_err();
        assert!(err.to_string().contains("closing"), "{err}");
    }

    /// R4-3: close must not return while the unpublished socket's reader
    /// still runs. The reader of socket 2 (restore reply withheld) is
    /// mid-poll on another worker when close aborts the supervisor: the
    /// abort cannot take effect until that poll ends, so only a join
    /// proves the reader (and its socket half) is gone.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn close_returns_only_after_the_unpublished_reader_ended() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = WsRpcClient::connect(fake.url(), "").await.unwrap();

        client.disconnect_for_testing().await;
        fake.connection(2).await;
        let restore = fake.next_request().await;
        assert_eq!(restore["type"], "events.watch");
        // The reply is withheld; socket 2's reader gets a frame and parks
        // its worker inside the poll.
        let (reader_gate, mut reader_parked) = client.park_at("reader:frame");
        fake.send(serde_json::json!({"type": "noise"})).await;
        reader_parked.recv().await;

        let (close_gate, mut close_parked) = client.park_at("close:readers");
        let closer = WsRpcClient {
            shared: client.shared.clone(),
        };
        let mut close = tokio::spawn(async move { closer.close().await });
        let returned = tokio::time::timeout(PROMPT, async {
            tokio::select! {
                r = &mut close => { r.unwrap(); true }
                _ = close_parked.recv() => false,
            }
        })
        .await
        .expect("close neither returned nor reached the reader join");
        let holders = Arc::strong_count(&client.shared);
        reader_gate.gate.add_permits(1);
        assert!(
            !returned,
            "close returned while the unpublished reader still ran ({holders} holders of the shared state)"
        );
        close_gate.gate.add_permits(1);
        tokio::time::timeout(PROMPT, close)
            .await
            .expect("close never finished after the reader was released")
            .unwrap();
        assert_eq!(Arc::strong_count(&client.shared), 1);
        assert!(fake.sockets_ended(2, PROMPT).await, "socket 2 stayed open");
    }
}
