//! One live query subscription on the executor — a port of the TypeScript
//! SDK's `LiveQuery` (`core/src/perspectives/LiveQuery.ts`).
//!
//! Holds the last result, applies each `query-subscription-update` delta to
//! it, resyncs via `perspective.resyncSubscription` when a revision is missing
//! or the executor reports lag, and re-opens after a socket reconnect (the
//! executor ends a socket's subscriptions when it closes — `resyncSubscription`
//! cannot work across sockets, so a re-open mints a fresh subscription id).
//! A re-open that fails retries with [`reopen_delay`] backoff until it
//! succeeds or the query is disposed.

use anyhow::{anyhow, Result};
use serde_json::{Map, Value};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{broadcast, Mutex};

use crate::rt::{self, Task};
use crate::ws_rpc::WsRpcClient;

/// Reply to `perspective.subscribeQuery` / `perspective.modelSubscribe`: the
/// first result, as JSON, at its revision.
#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Subscribed {
    pub subscription_id: String,
    pub result: Value,
    pub revision: u64,
}

/// A `query-subscription-update` event: the change from the previous
/// revision. Model results carry `ids` (the new order), `upsert` (new or
/// changed instances) and `totalCount`; query results carry `added` /
/// `removed` rows; a result that could not be diffed comes whole as `result`.
/// `{ lagged: true }` means the executor dropped updates — every live query
/// on the socket must resync.
#[derive(Debug, Clone, Default)]
pub struct QueryUpdate {
    pub subscription_id: Option<String>,
    pub revision: Option<u64>,
    pub ids: Option<Vec<String>>,
    pub upsert: Option<Vec<Value>>,
    pub total_count: Option<Value>,
    pub added: Option<Vec<Value>>,
    pub removed: Option<Vec<Value>>,
    pub result: Option<Value>,
    pub lagged: bool,
}

impl QueryUpdate {
    /// Parse one event object; `None` for events that are not
    /// `query-subscription-update`.
    fn from_event(event: &Value) -> Option<Self> {
        if event.get("type")?.as_str()? != "query-subscription-update" {
            return None;
        }
        if event.get("lagged").and_then(Value::as_bool) == Some(true) {
            return Some(Self {
                lagged: true,
                ..Default::default()
            });
        }
        Some(Self {
            subscription_id: event
                .get("subscriptionId")
                .and_then(Value::as_str)
                .map(String::from),
            revision: event.get("revision").and_then(Value::as_u64),
            ids: event
                .get("ids")
                .and_then(|v| serde_json::from_value(v.clone()).ok()),
            upsert: event.get("upsert").and_then(Value::as_array).cloned(),
            total_count: event.get("totalCount").cloned(),
            added: event.get("added").and_then(Value::as_array).cloned(),
            removed: event.get("removed").and_then(Value::as_array).cloned(),
            result: event.get("result").cloned(),
            lagged: false,
        })
    }
}

/// Delay before the first retry of a failed re-open; it doubles per failure.
pub const REOPEN_BASE_DELAY_MS: u64 = 1_000;
/// Longest delay between two re-open attempts.
pub const REOPEN_MAX_DELAY_MS: u64 = 60_000;

/// The delay before re-open attempt `failures + 1`, mirroring TS
/// `reopenDelay`: between half and all of
/// `REOPEN_BASE_DELAY_MS * 2^(failures - 1)`, capped at `REOPEN_MAX_DELAY_MS`.
pub fn reopen_delay(failures: u64) -> u64 {
    crate::ws_rpc::backoff_delay_ms(failures, REOPEN_BASE_DELAY_MS, REOPEN_MAX_DELAY_MS)
}

/// A new live query's id (a UUID v4). The client mints it, so it can end the
/// subscription without the subscribe call's reply — see TS `mintSubscriptionId`.
/// Executor requires 1–64 ASCII alphanumeric/hyphen characters.
pub fn mint_subscription_id() -> String {
    let mut b = rand::random::<[u8; 16]>();
    b[6] = (b[6] & 0x0f) | 0x40;
    b[8] = (b[8] & 0x3f) | 0x80;
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8], b[9], b[10], b[11], b[12], b[13],
        b[14], b[15]
    )
}

/// Apply one update to the last result — a port of TS `applyUpdate`.
/// Query rows are a multiset: added rows are appended.
/// Errors when an `ids` update names an instance the last result and the
/// update do not hold: the last result was wrong, and the caller must resync.
pub fn apply_update(result: &Value, update: &QueryUpdate) -> Result<Value> {
    if let Some(new_result) = &update.result {
        return Ok(new_result.clone());
    }
    if let Some(ids) = &update.ids {
        let mut by_id: Map<String, Value> = Map::new();
        if let Some(instances) = result.get("instances").and_then(Value::as_array) {
            for inst in instances {
                if let Some(id) = inst.get("id").and_then(Value::as_str) {
                    by_id.insert(id.to_string(), inst.clone());
                }
            }
        }
        for inst in update.upsert.clone().unwrap_or_default() {
            if let Some(id) = inst.get("id").and_then(Value::as_str) {
                by_id.insert(id.to_string(), inst);
            }
        }
        let mut instances = Vec::with_capacity(ids.len());
        for id in ids {
            let inst = by_id
                .get(id)
                .ok_or_else(|| anyhow!("Live query update names unknown instance {id}"))?;
            instances.push(inst.clone());
        }
        let mut out = result.clone();
        if let Some(obj) = out.as_object_mut() {
            obj.insert("instances".into(), Value::Array(instances));
            obj.insert(
                "totalCount".into(),
                update.total_count.clone().unwrap_or(Value::Null),
            );
        }
        return Ok(out);
    }
    // Row result — a multiset of JSON rows.
    let rows = result
        .as_array()
        .ok_or_else(|| anyhow!("Live query row delta on a non-array result"))?;
    let mut to_remove: Vec<Value> = update.removed.clone().unwrap_or_default();
    let mut kept: Vec<Value> = Vec::with_capacity(rows.len());
    'outer: for row in rows {
        for (i, r) in to_remove.iter().enumerate() {
            if r == row {
                to_remove.remove(i);
                continue 'outer;
            }
        }
        kept.push(row.clone());
    }
    kept.extend(update.added.clone().unwrap_or_default());
    Ok(Value::Array(kept))
}

/// What one `open` call returns. The closure mints a fresh subscription id
/// per call, so a re-open is a brand-new subscription on the executor.
/// Send+Sync bounds are native-only — the browser transport types are
/// single-threaded.
#[cfg(not(target_arch = "wasm32"))]
pub type OpenFn =
    Arc<dyn Fn() -> Pin<Box<dyn Future<Output = Result<Subscribed>> + Send>> + Send + Sync>;
#[cfg(target_arch = "wasm32")]
pub type OpenFn = Arc<dyn Fn() -> Pin<Box<dyn Future<Output = Result<Subscribed>>>>>;

struct State {
    id: Option<String>,
    revision: u64,
    result: Option<Value>,
    started: bool,
    disposed: bool,
    generation: u64,
    /// `Some` while a subscribe or resync reply is outstanding — updates that
    /// arrive meanwhile apply after the reply lands.
    buffer: Option<Vec<QueryUpdate>>,
    reopening: bool,
}

struct Inner {
    ws: Arc<WsRpcClient>,
    uuid: String,
    open: OpenFn,
    state: Mutex<State>,
    /// Every result after the first (the first `start` returns it instead).
    /// `None` once the query terminated — dropping the sender is what ends
    /// every receiver's `recv()` (F5).
    results: std::sync::Mutex<Option<broadcast::Sender<Value>>>,
    watch_key: String,
    /// The `events.watch` install — run once, and awaited by `start`
    /// before the subscribe call goes out, so the executor's event filter
    /// is in place before the subscription it feeds exists (F4).
    watch_ready: tokio::sync::OnceCell<()>,
    listener: std::sync::Mutex<Option<Task>>,
    /// The detached `on_reconnect` registration — aborted on dispose so a
    /// dispose racing it can never leave a callback behind (F5).
    reconnect_cb: std::sync::Mutex<Option<u64>>,
    /// Wakes a sleeping re-open retry early: `start` notifies on every
    /// attempt so an obsolete loop re-checks the generation and dies
    /// instead of sleeping out its backoff (F10 — TS clears the pending
    /// retry timer on every start).
    reopen_wake: tokio::sync::Notify,
    /// Live `LiveQuery` handles. Clones share `Inner`, so the query must be
    /// torn down by the *last* handle's drop, not any handle's (R4).
    handles: AtomicUsize,
}

/// One live query on the executor. Cheap to clone — clones share the query.
pub struct LiveQuery {
    inner: Arc<Inner>,
}

impl Clone for LiveQuery {
    fn clone(&self) -> Self {
        self.inner.handles.fetch_add(1, Ordering::Relaxed);
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl Drop for LiveQuery {
    /// The last handle's drop tears the query down like `dispose()` —
    /// executor-side subscription, watch interest, listener — or every
    /// abandoned query leaks all three server-side and locally (R4).
    /// Async teardown needs a runtime; off one, `dispose().await` is the
    /// clean path (native: no runtime in `Drop` context; browser:
    /// `spawn_local` always has the event loop, see `rt::try_spawn`).
    fn drop(&mut self) {
        if self.inner.handles.fetch_sub(1, Ordering::SeqCst) == 1 {
            let inner = self.inner.clone();
            rt::try_spawn(async move { inner.dispose_shared().await });
        }
    }
}

static WATCH_COUNTER: AtomicU64 = AtomicU64::new(0);

impl LiveQuery {
    /// Register the event listener and the reconnect hook; `start()` opens
    /// the subscription.
    pub fn new(ws: Arc<WsRpcClient>, uuid: &str, open: OpenFn) -> Self {
        let (results, _) = broadcast::channel(256);
        let watch_key = format!(
            "live-query:{}",
            WATCH_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let inner = Arc::new(Inner {
            ws: ws.clone(),
            uuid: uuid.to_string(),
            open,
            state: Mutex::new(State {
                id: None,
                revision: 0,
                result: None,
                started: false,
                disposed: false,
                generation: 0,
                buffer: None,
                reopening: false,
            }),
            results: std::sync::Mutex::new(Some(results)),
            watch_key,
            watch_ready: tokio::sync::OnceCell::new(),
            listener: std::sync::Mutex::new(None),
            reconnect_cb: std::sync::Mutex::new(None),
            reopen_wake: tokio::sync::Notify::new(),
            handles: AtomicUsize::new(1),
        });

        // Reconnect → re-open (the executor's subscriptions died with the old
        // socket, so `resyncSubscription` cannot help: a fresh subscribe).
        // Weak: the callback must not pin `Inner` alive — a dead query
        // upgrades to nothing (R3). Registered synchronously, its id stored
        // before `new` returns: no registration task can be abandoned
        // between the push and the store, leaving a callback nobody can
        // remove (N6). Stored before the listener exists: a terminal
        // teardown — which only the listener runs — always finds it (R4-2).
        let weak = Arc::downgrade(&inner);
        let cb_id = inner.ws.register_on_reconnect(Arc::new(move || {
            if let Some(inner) = weak.upgrade() {
                rt::spawn(async move { inner.reopen().await });
            }
        }));
        *inner.reconnect_cb.lock().unwrap() = Some(cb_id);
        #[cfg(all(test, not(target_arch = "wasm32")))]
        inner.ws.test_sync_park("new:midway");

        // Event listener — filters update events, feeds `receive` IN ORDER:
        // the listener awaits each receive, so a resync or a revision gap
        // is decided before the next event is read. It also watches the
        // client's terminal signal: a permanently closed socket ends this
        // stream too (F5).
        let inner2 = inner.clone();
        let mut events = inner.ws.events();
        let mut closed = inner.ws.closed();
        let listener = rt::spawn(async move {
            // `changed()` reports only values stored after the subscription:
            // a close that came first is read here (R4-2).
            if *closed.borrow_and_update() {
                inner2.terminate_stream().await;
                return;
            }
            loop {
                tokio::select! {
                    event = events.recv() => match event {
                        Ok(event) => {
                            if let Some(update) = QueryUpdate::from_event(&event) {
                                inner2.receive(update).await;
                            }
                        }
                        Err(broadcast::error::RecvError::Lagged(_)) => {
                            // Missed event(s): every live query must resync.
                            inner2.resync().await;
                        }
                        Err(broadcast::error::RecvError::Closed) => return,
                    },
                    _ = closed.changed() => {
                        // The socket is gone for good — remote teardown is
                        // impossible and the executor already dropped the
                        // socket's subscriptions.
                        inner2.terminate_stream().await;
                        return;
                    }
                }
            }
        });
        *inner.listener.lock().unwrap() = Some(listener);

        Self { inner }
    }

    /// The current subscription id, if opened.
    pub async fn id(&self) -> Option<String> {
        self.inner.state.lock().await.id.clone()
    }

    /// The last result.
    pub async fn result(&self) -> Option<Value> {
        self.inner.state.lock().await.result.clone()
    }

    /// Whether a re-open failed and the query is retrying.
    pub async fn reopening(&self) -> bool {
        self.inner.state.lock().await.reopening
    }

    /// Every result after the initial one (resyncs and re-opens included —
    /// always the full current result). A terminated query yields a
    /// receiver that reports `Closed` at once.
    pub fn results(&self) -> broadcast::Receiver<Value> {
        match self.inner.results.lock().unwrap().as_ref() {
            Some(tx) => tx.subscribe(),
            None => broadcast::channel(1).1,
        }
    }

    /// Open the subscription (again) and return its snapshot. Updates that
    /// arrived while the open was in flight are emitted on `results()`,
    /// like TS `LiveQuery.start` — they do not fold into the snapshot. A
    /// start that a newer one overtakes resolves with the newer result.
    pub async fn start(&self) -> Result<Value> {
        self.inner.start().await
    }

    /// End the subscription: stop listening, dispose executor-side.
    pub async fn dispose(&self) {
        self.inner.dispose_shared().await;
    }
}

impl Inner {
    /// Install this query's `query-subscription-update` interest — once.
    /// `start` awaits it before opening, so the watch is acknowledged
    /// before the subscribe call it feeds can reach the executor (F4).
    async fn ensure_watch(self: &Arc<Self>) -> Result<()> {
        self.watch_ready
            .get_or_try_init(|| async {
                self.ws
                    .set_watch(
                        &self.watch_key,
                        serde_json::json!({"query-subscription-update": null}),
                    )
                    .await
            })
            .await
            .map(|_| ())
    }

    /// `LiveQuery::start` on the shared inner — a re-open (or the
    /// last-handle teardown) must not mint a handle to call it: a handle
    /// dropping mid-teardown would run the teardown again (R4).
    ///
    /// One spawned task owns the whole open-to-install sequence: the
    /// caller only awaits it. A caller cancelled after the open succeeded
    /// — its reply queued but unread, or read but the id not yet in
    /// `state` — leaves the task to install the id (or, on a disposed
    /// query, release it), so the subscription is never stranded with
    /// nobody holding its id (N3). This holds for any [`OpenFn`].
    async fn start(self: &Arc<Self>) -> Result<Value> {
        let this = self.clone();
        rt::spawn_detached(async move { this.start_owned().await })
            .await
            .map_err(|e| anyhow::anyhow!("live query start ended: {e}"))?
    }

    async fn start_owned(self: &Arc<Self>) -> Result<Value> {
        self.ensure_watch().await?;
        let generation = {
            let mut s = self.state.lock().await;
            s.generation += 1;
            s.buffer = Some(Vec::new());
            s.generation
        };
        // A re-open retry sleeping off a failure is obsolete the moment a
        // newer start exists — wake it so it dies on the generation check
        // (F10; TS clears the pending retry timer on every start).
        self.reopen_wake.notify_waiters();
        let open = self.open.clone();
        let subscribed = match open().await {
            Ok(s) => s,
            Err(e) => {
                let mut state = self.state.lock().await;
                if state.generation == generation {
                    state.buffer = None;
                }
                return Err(e);
            }
        };

        #[cfg(all(test, not(target_arch = "wasm32")))]
        self.ws.test_park("start:opened").await;
        let mut state = self.state.lock().await;
        if state.disposed || state.generation != generation {
            let dispose_id = subscribed.subscription_id.clone();
            let ws = self.ws.clone();
            let uuid = self.uuid.clone();
            drop(state);
            let _ = dispose_query(&ws, &uuid, &dispose_id).await;
            // Disposed while opening: the caller still gets the snapshot.
            let state = self.state.lock().await;
            return Ok(state
                .result
                .clone()
                .unwrap_or_else(|| subscribed.result.clone()));
        }
        let superseded = state.id.replace(subscribed.subscription_id.clone());
        let snapshot = subscribed.result.clone();
        self.replace_locked(state, subscribed.revision, subscribed.result)
            .await;
        // The subscription this start replaced is still live on the
        // executor — release it, or every re-open leaks a second one
        // while only the newest is tracked (F10). Fire-and-forget: it's
        // cleanup, not part of the open.
        if let Some(old) = superseded {
            let ws = self.ws.clone();
            let uuid = self.uuid.clone();
            rt::spawn(async move {
                let _ = dispose_query(&ws, &uuid, &old).await;
            });
        }
        Ok(snapshot)
    }

    /// `LiveQuery::dispose` without a handle — the last-drop teardown uses
    /// it too, and minting a handle to call it would recurse (R4).
    async fn dispose_shared(self: &Arc<Self>) {
        let id = {
            let mut s = self.state.lock().await;
            s.disposed = true;
            s.generation += 1;
            s.buffer = None;
            s.id.take()
        };
        if let Some(h) = self.listener.lock().unwrap().take() {
            h.abort();
        }
        // End every receiver's `recv()` — a disposed query's stream closes.
        self.results.lock().unwrap().take();
        self.unregister_reconnect();
        // An install still in flight must finish before the removal, or the
        // `events.watch` could land after its unwatch (F4).
        let _ = self.ensure_watch().await;
        let _ = self.ws.remove_watch(&self.watch_key).await;
        if let Some(id) = id {
            let _ = dispose_query(&self.ws, &self.uuid, &id).await;
        }
    }
}

impl Inner {
    /// Terminal local teardown for a permanently dead socket: mark
    /// disposed, cancel the pending registration, end the results
    /// channel. Remote teardown is impossible — the executor dropped the
    /// socket's subscriptions with it. The listener calls this on its own
    /// way out, so it deliberately doesn't touch the listener handle.
    async fn terminate_stream(self: &Arc<Self>) {
        {
            let mut s = self.state.lock().await;
            s.disposed = true;
            s.generation += 1;
            s.buffer = None;
        }
        // No socket comes back: the reconnect callback goes now, not when
        // the handle is dropped (N6).
        self.unregister_reconnect();
        // Dropping the sender ends every receiver's `recv()` (F5).
        self.results.lock().unwrap().take();
    }

    /// Remove this query's reconnect callback, once.
    fn unregister_reconnect(&self) {
        let cb = self.reconnect_cb.lock().unwrap().take();
        if let Some(cb) = cb {
            self.ws.unregister_on_reconnect(cb);
        }
    }

    /// Take `result` at `revision`, then apply the updates buffered meanwhile.
    /// Takes the state lock guard itself — released before the buffered
    /// updates drain, since `receive` re-acquires it.
    async fn replace_locked(
        self: &Arc<Self>,
        mut state: tokio::sync::MutexGuard<'_, State>,
        revision: u64,
        result: Value,
    ) {
        state.revision = revision;
        state.result = Some(result.clone());
        let buffered = state.buffer.take().unwrap_or_default();
        if state.started {
            if let Some(tx) = self.results.lock().unwrap().as_ref() {
                let _ = tx.send(result);
            }
        }
        state.started = true;
        let this = self.clone();
        drop(state);
        for update in buffered {
            this.receive(update).await;
        }
    }

    async fn receive(self: &Arc<Self>, update: QueryUpdate) {
        let mut state = self.state.lock().await;
        if let Some(buffer) = state.buffer.as_mut() {
            buffer.push(update);
            return;
        }
        if update.lagged {
            self.arm_and_spawn_resync(&mut state);
            return;
        }
        let revision = match (&state.id, &update.subscription_id) {
            (Some(id), Some(uid)) if id == uid => update.revision,
            _ => return,
        };
        let revision = match revision {
            Some(r) => r,
            None => return,
        };
        if revision <= state.revision {
            return;
        }
        if revision != state.revision + 1 {
            self.arm_and_spawn_resync(&mut state);
            return;
        }
        let result = match state.result.as_ref() {
            Some(r) => match apply_update(r, &update) {
                Ok(r) => r,
                Err(_) => {
                    self.arm_and_spawn_resync(&mut state);
                    return;
                }
            },
            None => {
                self.arm_and_spawn_resync(&mut state);
                return;
            }
        };
        state.revision = revision;
        state.result = Some(result.clone());
        if let Some(tx) = self.results.lock().unwrap().as_ref() {
            let _ = tx.send(result);
        }
    }

    /// Mark the query as resyncing (updates arriving meanwhile buffer) and
    /// spawn the resync task. No-op when an open or resync is already
    /// outstanding, or when there is no subscription to resync. Called with
    /// the state lock held; the spawned task runs after it is released.
    fn arm_and_spawn_resync(self: &Arc<Self>, state: &mut State) {
        if state.buffer.is_some() {
            return;
        }
        let Some(id) = state.id.clone() else {
            return;
        };
        let generation = state.generation;
        state.buffer = Some(Vec::new());
        let this = self.clone();
        rt::spawn(async move { this.resync_perform(generation, id).await });
    }

    /// Arm a resync for a lagged event (an executor `lagged` notice, or a
    /// missed event on our own broadcast).
    async fn resync(self: &Arc<Self>) {
        let mut state = self.state.lock().await;
        self.arm_and_spawn_resync(&mut state);
    }

    /// The resync `arm_and_spawn_resync` armed: fetch the current
    /// result/revision of `id` and replace the last result with it. A failed
    /// resync replaces the subscription (the subscription may be gone — e.g.
    /// after reconnect).
    async fn resync_perform(self: &Arc<Self>, generation: u64, id: String) {
        let resynced: Result<Value> = self
            .ws
            .call(
                "perspective.resyncSubscription",
                serde_json::json!({ "uuid": self.uuid, "subscriptionId": id }),
            )
            .await;

        match resynced {
            Ok(v) => {
                let revision = v.get("revision").and_then(Value::as_u64).unwrap_or(0);
                let result = v.get("result").cloned().unwrap_or(Value::Null);
                let state = self.state.lock().await;
                if !state.disposed && state.generation == generation {
                    self.replace_locked(state, revision, result).await;
                }
                // else: stale — a newer open owns the buffer; leave it.
            }
            Err(_) => {
                let mut state = self.state.lock().await;
                if !state.disposed && state.generation == generation {
                    state.buffer = None;
                    drop(state);
                    let _ = dispose_query(&self.ws, &self.uuid, &id).await;
                    self.reopen().await;
                }
            }
        }
    }

    /// Open the subscription again, and keep trying with backoff
    /// (`reopen_delay`) while each attempt fails, until one succeeds, a newer
    /// `start()` takes over, or the query is disposed.
    async fn reopen(self: &Arc<Self>) {
        let mut failures: u64 = 0;
        // The generation our failed attempt owned. Once another start
        // advances it, this loop is obsolete — a wake that ignores that
        // would open a second live subscription over the winner's (F10).
        let mut expected: Option<u64> = None;
        loop {
            let before = {
                let mut s = self.state.lock().await;
                if s.disposed {
                    return;
                }
                if let Some(g) = expected {
                    if s.generation != g {
                        // Another start/reopen owns the live subscription.
                        s.reopening = false;
                        return;
                    }
                }
                s.generation
            };
            // `start` bumps `generation`, so this attempt's generation is
            // `before + 1` — anything newer means another start overtook us.
            // Straight on the Arc: minting a `LiveQuery` handle for the
            // duration would corrupt the handle count (R4).
            match self.start().await {
                Ok(_) => {
                    let mut state = self.state.lock().await;
                    if !state.disposed && state.generation == before + 1 {
                        state.reopening = false;
                    }
                    return;
                }
                Err(e) => {
                    let mut state = self.state.lock().await;
                    if state.disposed || state.generation != before + 1 {
                        return;
                    }
                    failures += 1;
                    state.reopening = true;
                    expected = Some(before + 1);
                    let retry = reopen_delay(failures);
                    drop(state);
                    rt::log(&format!(
                        "[live-query] re-open failed (attempt {failures}), retrying in {retry}ms: {e}"
                    ));
                    // A newer start wakes the sleep early — the generation
                    // check above then ends this loop (F10).
                    tokio::select! {
                        _ = rt::sleep(Duration::from_millis(retry)) => {}
                        _ = self.reopen_wake.notified() => {}
                    }
                }
            }
        }
    }
}

/// `perspective.disposeQuery` — ends a subscription on the executor.
pub(crate) async fn dispose_query(
    ws: &Arc<WsRpcClient>,
    uuid: &str,
    subscription_id: &str,
) -> Result<()> {
    ws.call_void(
        "perspective.disposeQuery",
        serde_json::json!({ "uuid": uuid, "subscriptionId": subscription_id }),
    )
    .await
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use serde_json::json;

    /// Bound on a wait the fix makes immediate; a fallback would exceed it.
    const PROMPT: Duration = Duration::from_millis(1_500);

    #[test]
    fn apply_update_replaces_on_full_result() {
        let old = json!({"instances": [{"id": "a"}], "totalCount": 1});
        let update = QueryUpdate {
            result: Some(json!({"instances": [{"id": "b"}], "totalCount": 1})),
            ..Default::default()
        };
        let out = apply_update(&old, &update).unwrap();
        assert_eq!(out["instances"][0]["id"], "b");
    }

    #[test]
    fn apply_update_applies_model_delta() {
        let old = json!({"instances": [{"id": "a", "v": 1}, {"id": "b", "v": 2}], "totalCount": 2});
        let update = QueryUpdate {
            ids: Some(vec!["b".into(), "c".into()]),
            upsert: Some(vec![json!({"id": "b", "v": 3}), json!({"id": "c", "v": 4})]),
            total_count: Some(json!(5)),
            ..Default::default()
        };
        let out = apply_update(&old, &update).unwrap();
        assert_eq!(out["instances"].as_array().unwrap().len(), 2);
        assert_eq!(out["instances"][0], json!({"id": "b", "v": 3}));
        assert_eq!(out["instances"][1], json!({"id": "c", "v": 4}));
        assert_eq!(out["totalCount"], json!(5));
    }

    #[test]
    fn apply_update_errors_on_unknown_id() {
        let old = json!({"instances": [{"id": "a"}], "totalCount": 1});
        let update = QueryUpdate {
            ids: Some(vec!["zzz".into()]),
            ..Default::default()
        };
        assert!(apply_update(&old, &update).is_err());
    }

    #[test]
    fn apply_update_row_delta_multiset() {
        let old = json!([{"x": 1}, {"x": 2}, {"x": 2}]);
        let update = QueryUpdate {
            removed: Some(vec![json!({"x": 2})]),
            added: Some(vec![json!({"x": 3})]),
            ..Default::default()
        };
        let out = apply_update(&old, &update).unwrap();
        assert_eq!(out, json!([{"x": 1}, {"x": 2}, {"x": 3}]));
    }

    #[test]
    fn subscription_id_mints_uuid_v4_shape() {
        let id = mint_subscription_id();
        assert_eq!(id.len(), 36);
        assert!(id.chars().all(|c| c.is_ascii_alphanumeric() || c == '-'));
        assert_eq!(&id[14..15], "4");
    }

    #[test]
    fn reopen_delay_bounds() {
        let d1 = reopen_delay(1);
        assert!((500..=1000).contains(&d1));
        let d_big = reopen_delay(20);
        assert!((30_000..=60_000).contains(&d_big));
    }

    /// Drive one LiveQuery through watch+open against the fake; returns the
    /// query once `start` has resolved.
    async fn started_query(
        fake: &crate::testkit::FakeExecutor,
        client: &Arc<WsRpcClient>,
    ) -> LiveQuery {
        let open: OpenFn = {
            let ws = client.clone();
            Arc::new(move || {
                let ws = ws.clone();
                Box::pin(async move {
                    ws.call(
                        "perspective.subscribeQuery",
                        serde_json::json!({"uuid": "p1", "subscriptionId": "s1"}),
                    )
                    .await
                    .map(|v| Subscribed {
                        subscription_id: "s1".into(),
                        result: v,
                        revision: 0,
                    })
                })
            })
        };
        let query = LiveQuery::new(client.clone(), "p1", open);
        let q = query.clone();
        let start = tokio::spawn(async move { q.start().await });
        for _ in 0..2 {
            let req = fake.next_request().await;
            match req["type"].as_str() {
                Some("events.watch") => fake.reply(&req, serde_json::json!(true)).await,
                _ => {
                    fake.reply(&req, serde_json::json!({"result": [], "revision": 0}))
                        .await
                }
            }
        }
        start.await.unwrap().unwrap();
        query
    }

    /// F10: a failed re-open sleeps before retrying. If another re-open
    /// succeeds meanwhile, the sleeping loop must notice the generation
    /// moved and die — not wake up and open a third subscription while
    /// `state.id` still tracks the second.
    #[tokio::test]
    async fn an_obsolete_reopen_retry_does_not_open_a_second_subscription() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());

        let opens = Arc::new(AtomicU64::new(0));
        let open: OpenFn = {
            let ws = client.clone();
            let opens = opens.clone();
            Arc::new(move || {
                let ws = ws.clone();
                let opens = opens.clone();
                Box::pin(async move {
                    let n = opens.fetch_add(1, Ordering::Relaxed) + 1;
                    let sid = format!("sub-{n}");
                    ws.call(
                        "perspective.subscribeQuery",
                        serde_json::json!({"uuid": "p1", "subscriptionId": sid}),
                    )
                    .await
                    .map(|v| Subscribed {
                        subscription_id: sid,
                        result: v,
                        revision: 0,
                    })
                })
            })
        };
        let query = LiveQuery::new(client.clone(), "p1", open);
        let q = query.clone();
        let start = tokio::spawn(async move { q.start().await });
        for _ in 0..2 {
            let req = fake.next_request().await;
            match req["type"].as_str() {
                Some("events.watch") => fake.reply(&req, serde_json::json!(true)).await,
                _ => {
                    fake.reply(&req, serde_json::json!({"result": [], "revision": 0}))
                        .await
                }
            }
        }
        start.await.unwrap().unwrap();

        // Re-open A fails → it sleeps before retrying.
        let inner_a = query.inner.clone();
        let retry_a = tokio::spawn(async move { inner_a.reopen().await });
        let req_a = fake.next_request().await;
        assert_eq!(req_a["type"], "perspective.subscribeQuery");
        fake.reply_error(&req_a, 500, "socket just came back").await;

        // The bug window needs A *asleep* before B's start bumps the
        // generation — `reopening` is set inside the lock just before the
        // sleep, so it is the barrier.
        for _ in 0..100 {
            if query.reopening().await {
                break;
            }
            tokio::task::yield_now().await;
        }
        assert!(
            query.reopening().await,
            "re-open A must be asleep before B starts"
        );

        // Re-open B lands while A sleeps — it succeeds.
        let inner_b = query.inner.clone();
        let retry_b = tokio::spawn(async move { inner_b.reopen().await });
        let req_b = fake.next_request().await;
        assert_eq!(req_b["type"], "perspective.subscribeQuery");
        fake.reply(&req_b, serde_json::json!({"result": [], "revision": 0}))
            .await;
        retry_b.await.unwrap();

        // A's sleep outlasts B's open. When it wakes it must see the world
        // moved on — a third subscribeQuery means it clobbered B's live
        // subscription with an untracked one. (A disposeQuery here is B
        // releasing the subscription its own start replaced — expected.)
        let third = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let req = fake.maybe_request(Duration::from_millis(300)).await;
                let Some(req) = req else { continue };
                eprintln!("[test] frame: {}", req["type"]);
                if req["type"] == "perspective.subscribeQuery" {
                    return req;
                }
                fake.reply(&req, serde_json::json!(true)).await;
            }
        })
        .await;
        assert!(
            third.is_err(),
            "an obsolete re-open opened a second live subscription: {third:?}"
        );
        retry_a.await.unwrap();
        client.close().await;
    }

    /// F10: a start that replaces a live `state.id` must dispose the
    /// subscription it supersedes — both server subscriptions otherwise
    /// stay active while only the newest is tracked.
    #[tokio::test]
    async fn start_disposes_the_subscription_it_replaces() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());

        let opens = Arc::new(AtomicU64::new(0));
        let open: OpenFn = {
            let ws = client.clone();
            let opens = opens.clone();
            Arc::new(move || {
                let ws = ws.clone();
                let opens = opens.clone();
                Box::pin(async move {
                    let n = opens.fetch_add(1, Ordering::Relaxed) + 1;
                    let sid = format!("sub-{n}");
                    ws.call(
                        "perspective.subscribeQuery",
                        serde_json::json!({"uuid": "p1", "subscriptionId": sid}),
                    )
                    .await
                    .map(|v| Subscribed {
                        subscription_id: sid,
                        result: v,
                        revision: 0,
                    })
                })
            })
        };
        let query = LiveQuery::new(client.clone(), "p1", open);
        let q = query.clone();
        let start = tokio::spawn(async move { q.start().await });
        for _ in 0..2 {
            let req = fake.next_request().await;
            match req["type"].as_str() {
                Some("events.watch") => fake.reply(&req, serde_json::json!(true)).await,
                _ => {
                    fake.reply(&req, serde_json::json!({"result": [], "revision": 0}))
                        .await
                }
            }
        }
        start.await.unwrap().unwrap();
        assert_eq!(query.id().await.as_deref(), Some("sub-1"));

        // A second start over the same live socket: sub-1 must be released.
        let q2 = query.clone();
        let restart = tokio::spawn(async move { q2.start().await });
        let reopened = fake.next_request().await;
        assert_eq!(reopened["type"], "perspective.subscribeQuery");
        fake.reply(&reopened, serde_json::json!({"result": [], "revision": 0}))
            .await;
        let disposed = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let req = fake.next_request().await;
                if req["type"] == "perspective.disposeQuery" {
                    fake.reply(&req, serde_json::json!(true)).await;
                    return req;
                }
                fake.reply(&req, serde_json::json!(true)).await;
            }
        })
        .await
        .expect("the superseded subscription must be disposed");
        assert_eq!(disposed["params"]["subscriptionId"], "sub-1");
        restart.await.unwrap().unwrap();
        assert_eq!(query.id().await.as_deref(), Some("sub-2"));
        client.close().await;
    }

    /// F5: `dispose` must end the stream — after the buffer drains,
    /// `rx.recv()` reports `Closed`, not another endless wait.
    #[tokio::test]
    async fn dispose_ends_the_results_stream() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let query = started_query(&fake, &client).await;
        let mut rx = query.results();

        let q = query.clone();
        let dispose = tokio::spawn(async move { q.dispose().await });
        // dispose awaits the unwatch reply — answer every request it sends.
        for _ in 0..2 {
            let req = fake.next_request().await;
            fake.reply(&req, serde_json::json!(true)).await;
        }
        dispose.await.unwrap();
        match tokio::time::timeout(Duration::from_secs(1), rx.recv()).await {
            Ok(Err(broadcast::error::RecvError::Closed)) => {}
            other => panic!("a disposed query's stream must close, got {other:?}"),
        }
        client.close().await;
    }

    /// R3 + N6: the reconnect callback holds only a weak ref, so it can
    /// never pin a dead query alive; and it is registered synchronously —
    /// its id is stored before `new` returns, so no abort can land between
    /// the push and the store (the old registration task's window). The
    /// last handle's teardown removes it: the count returns to baseline.
    #[tokio::test]
    async fn a_dropped_query_leaves_no_callback_and_no_strong_ref() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let baseline = client.reconnect_callback_count().await;

        let query = LiveQuery::new(client.clone(), "p1", scripted_open(&client));
        assert!(
            query.inner.reconnect_cb.lock().unwrap().is_some(),
            "the callback id is not owned the moment `new` returns"
        );
        assert_eq!(client.reconnect_callback_count().await, baseline + 1);

        // Take the listener's pin too and drop the handle — a strong
        // callback would keep `Inner` alive forever.
        let listener = query.inner.listener.lock().unwrap().take();
        if let Some(h) = listener {
            h.abort();
            let _ = h.join().await;
        }
        let weak = Arc::downgrade(&query.inner);
        drop(query);
        // The last handle's drop runs the teardown detached: it legitimately
        // holds `Inner` until the executor RPCs finish, so serve them while
        // it dies.
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if weak.upgrade().is_none() && client.reconnect_callback_count().await == baseline {
                    break;
                }
                match tokio::time::timeout(Duration::from_millis(200), fake.next_request()).await {
                    Ok(req) => fake.reply(&req, serde_json::json!(true)).await,
                    Err(_) => tokio::task::yield_now().await,
                }
            }
        })
        .await
        .expect("the dropped query left its callback or a strong Inner behind");
        client.close().await;
    }

    /// R4: `LiveQuery` is cloneable over a shared `Arc<Inner>` — dropping
    /// the last handle must tear the query down: the subscription is
    /// disposed executor-side and its `query-subscription-update` interest
    /// leaves the watch union. An earlier handle's drop must not.
    #[tokio::test]
    async fn a_dropped_live_query_releases_its_watch_and_subscription() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let query = started_query(&fake, &client).await;

        // A surviving clone keeps the query — nothing may go out yet.
        let clone = query.clone();
        drop(query);
        assert!(
            fake.maybe_request(Duration::from_millis(300))
                .await
                .is_none(),
            "a surviving handle still owns the query"
        );

        // The last handle's drop runs the teardown: unwatch + disposeQuery.
        drop(clone);
        let mut disposed = false;
        let mut unwatched = false;
        for _ in 0..8 {
            if disposed && unwatched {
                break;
            }
            let req = tokio::time::timeout(Duration::from_secs(5), fake.next_request())
                .await
                .expect("the dropped query's teardown stalled");
            match req["type"].as_str() {
                Some("perspective.disposeQuery") => {
                    assert_eq!(req["params"]["subscriptionId"], "s1");
                    disposed = true;
                }
                Some("events.watch") => {
                    assert_eq!(req["params"], json!({}));
                    unwatched = true;
                }
                _ => {}
            }
            fake.reply(&req, json!(true)).await;
        }
        assert!(
            disposed && unwatched,
            "dropping the last handle must dispose s1 and remove its watch"
        );
        client.close().await;
    }

    /// F5: closing the client for good ends every live query's stream —
    /// the executor dropped the socket's subscriptions, and waiting
    /// forever for events that can no longer arrive is a leak, not
    /// patience.
    #[tokio::test]
    async fn a_permanently_closed_socket_ends_the_results_stream() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let query = started_query(&fake, &client).await;
        let mut rx = query.results();

        client.close().await;
        match tokio::time::timeout(Duration::from_secs(1), rx.recv()).await {
            Ok(Err(broadcast::error::RecvError::Closed)) => {}
            other => panic!("a closed socket must end its live queries, got {other:?}"),
        }
    }

    /// F4: the subscribe call must not reach the executor while the
    /// `events.watch` that feeds its updates is still unacknowledged — a
    /// change published in that window is filtered out server-side and the
    /// subscriber stays stale forever.
    #[tokio::test]
    async fn start_waits_for_the_watch_reply_before_opening() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());

        let open: OpenFn = {
            let ws = client.clone();
            Arc::new(move || {
                let ws = ws.clone();
                Box::pin(async move {
                    ws.call(
                        "perspective.subscribeQuery",
                        serde_json::json!({"uuid": "p1", "subscriptionId": "s1"}),
                    )
                    .await
                    .map(|v| Subscribed {
                        subscription_id: "s1".into(),
                        result: v,
                        revision: 0,
                    })
                })
            })
        };
        let query = LiveQuery::new(client.clone(), "p1", open);
        let start = tokio::spawn(async move { query.start().await });

        // The watch must be the first thing on the wire…
        let watch = fake.next_request().await;
        assert_eq!(watch["type"], "events.watch");
        // …and while its reply is outstanding, no subscribe may leave.
        assert!(
            fake.maybe_request(Duration::from_millis(300))
                .await
                .is_none(),
            "the subscribe call went out before events.watch was acknowledged"
        );
        fake.reply(&watch, serde_json::json!(true)).await;
        let sub = fake.next_request().await;
        assert_eq!(sub["type"], "perspective.subscribeQuery");
        fake.reply(&sub, serde_json::json!({"result": [], "revision": 0}))
            .await;
        start.await.unwrap().unwrap();
        client.close().await;
    }

    #[test]
    fn parses_update_and_lagged_events() {
        let update = QueryUpdate::from_event(&json!({
            "type": "query-subscription-update",
            "subscriptionId": "s1",
            "revision": 7,
            "ids": ["a"],
            "totalCount": 3,
        }))
        .unwrap();
        assert_eq!(update.subscription_id.as_deref(), Some("s1"));
        assert_eq!(update.revision, Some(7));
        assert!(!update.lagged);

        let lagged = QueryUpdate::from_event(&json!({
            "type": "query-subscription-update", "lagged": true
        }))
        .unwrap();
        assert!(lagged.lagged);

        assert!(QueryUpdate::from_event(&json!({"type": "other"})).is_none());
    }

    /// A scripted open on `client`: `perspective.subscribeQuery` answered
    /// by the test, id `s1`.
    fn scripted_open(client: &Arc<WsRpcClient>) -> OpenFn {
        let ws = client.clone();
        Arc::new(move || {
            let ws = ws.clone();
            Box::pin(async move {
                ws.call(
                    "perspective.subscribeQuery",
                    serde_json::json!({"uuid": "p1", "subscriptionId": "s1"}),
                )
                .await
                .map(|v| Subscribed {
                    subscription_id: "s1".into(),
                    result: v,
                    revision: 0,
                })
            })
        })
    }

    /// N6: terminal teardown (the client closed for good) must remove the
    /// query's reconnect callback even while its handle stays alive: the
    /// registry returns to its baseline.
    #[tokio::test]
    async fn terminal_teardown_removes_the_reconnect_callback() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let baseline = client.reconnect_callback_count().await;

        let query = LiveQuery::new(client.clone(), "p1", scripted_open(&client));
        let start = tokio::spawn({
            let inner = query.inner.clone();
            async move { inner.start().await }
        });
        let watch = fake.next_request().await;
        assert_eq!(watch["type"], "events.watch");
        fake.reply(&watch, json!(true)).await;
        let sub = fake.next_request().await;
        fake.reply(&sub, json!({"instances": []})).await;
        start.await.unwrap().unwrap();
        tokio::time::timeout(Duration::from_secs(2), async {
            while query.inner.reconnect_cb.lock().unwrap().is_none() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the reconnect callback was never recorded");
        assert_eq!(client.reconnect_callback_count().await, baseline + 1);

        // Terminal: the query's stream ends (its receiver reports Closed).
        let mut results = query.results();
        client.close().await;
        tokio::time::timeout(Duration::from_secs(2), async {
            while !matches!(
                results.recv().await,
                Err(broadcast::error::RecvError::Closed)
            ) {}
        })
        .await
        .expect("the stream never ended on close");
        tokio::time::timeout(Duration::from_secs(2), async {
            while client.reconnect_callback_count().await != baseline {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("terminal teardown left the reconnect callback registered");
        drop(query);
    }

    /// N6: create/dispose cycles leave no callback behind — the registry
    /// returns to its baseline after every dispose.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn create_dispose_cycles_return_the_callback_count_to_baseline() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let baseline = client.reconnect_callback_count().await;
        // Serve every teardown RPC (unwatch, dispose) as it comes.
        let server = {
            let fake = fake.clone();
            tokio::spawn(async move {
                loop {
                    let req = fake.next_request().await;
                    fake.reply(&req, json!(true)).await;
                }
            })
        };
        for _ in 0..50 {
            let query = LiveQuery::new(client.clone(), "p1", scripted_open(&client));
            query.dispose().await;
        }
        assert_eq!(client.reconnect_callback_count().await, baseline);
        server.abort();
        client.close().await;
    }

    /// R4-2: close lands while the constructor is between its two steps
    /// (listener and callback registration). Whatever the order, the
    /// terminal teardown must still remove the callback and end the
    /// stream: a listener that already ran must not leave a callback the
    /// constructor registers afterwards, and a listener started after the
    /// close must see the `true` that predates it.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_close_during_construction_still_removes_the_reconnect_callback() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let client = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let baseline = client.reconnect_callback_count().await;
        let receivers = client.closed_receivers();

        let (gate, mut parked) = client.park_at("new:midway");
        let ctor = tokio::task::spawn_blocking({
            let client = client.clone();
            move || LiveQuery::new(client.clone(), "p1", scripted_open(&client))
        });
        parked.recv().await;
        client.close().await;
        // Every listener that exists has handled the close: it dropped its
        // receiver of the terminal signal on the way out.
        tokio::time::timeout(PROMPT, async {
            while client.closed_receivers() > receivers {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("a running listener never handled the close");
        gate.gate.add_permits(1);
        let query = ctor.await.unwrap();

        tokio::time::timeout(PROMPT, async {
            while client.reconnect_callback_count().await != baseline {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("a close during construction left the reconnect callback registered");
        let mut results = query.results();
        let ended = tokio::time::timeout(PROMPT, results.recv())
            .await
            .expect("the stream of a query built across close never ended");
        assert!(matches!(ended, Err(broadcast::error::RecvError::Closed)));
    }
}
