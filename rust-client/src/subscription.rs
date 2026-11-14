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
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{broadcast, Mutex};
use tokio::task::JoinHandle;

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
pub type OpenFn =
    Arc<dyn Fn() -> Pin<Box<dyn Future<Output = Result<Subscribed>> + Send>> + Send + Sync>;

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
    results: broadcast::Sender<Value>,
    watch_key: String,
    listener: std::sync::Mutex<Option<JoinHandle<()>>>,
    reconnect_cb: std::sync::Mutex<Option<u64>>,
}

/// One live query on the executor. Cheap to clone — clones share the query.
#[derive(Clone)]
pub struct LiveQuery {
    inner: Arc<Inner>,
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
            results,
            watch_key,
            listener: std::sync::Mutex::new(None),
            reconnect_cb: std::sync::Mutex::new(None),
        });

        // Watch `query-subscription-update` under our own key so the interest
        // survives both other watchers and reconnects.
        let watch_key = inner.watch_key.clone();
        tokio::spawn(async move {
            ws.set_watch(
                &watch_key,
                serde_json::json!({"query-subscription-update": null}),
            )
            .await;
        });

        // Event listener — filters update events, feeds `receive` IN ORDER:
        // the listener awaits each receive, so a resync or a revision gap
        // is decided before the next event is read.
        let inner2 = inner.clone();
        let mut events = inner.ws.events();
        let listener = tokio::spawn(async move {
            loop {
                match events.recv().await {
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
                }
            }
        });
        *inner.listener.lock().unwrap() = Some(listener);

        // Reconnect → re-open (the executor's subscriptions died with the old
        // socket, so `resyncSubscription` cannot help: a fresh subscribe).
        let ws2 = inner.ws.clone();
        let inner5 = inner.clone();
        let inner6 = inner.clone();
        tokio::spawn(async move {
            let cb_id = ws2
                .on_reconnect(move || {
                    let inner7 = inner6.clone();
                    tokio::spawn(async move { inner7.reopen().await });
                })
                .await;
            *inner5.reconnect_cb.lock().unwrap() = Some(cb_id);
        });

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
    /// always the full current result).
    pub fn results(&self) -> broadcast::Receiver<Value> {
        self.inner.results.subscribe()
    }

    /// Open the subscription (again) and return its initial result. A start
    /// that a newer one overtakes resolves with the newer one's result.
    pub async fn start(&self) -> Result<Value> {
        let generation = {
            let mut s = self.inner.state.lock().await;
            s.generation += 1;
            s.buffer = Some(Vec::new());
            s.generation
        };
        let open = self.inner.open.clone();
        let subscribed = match open().await {
            Ok(s) => s,
            Err(e) => {
                let mut state = self.inner.state.lock().await;
                if state.generation == generation {
                    state.buffer = None;
                }
                return Err(e);
            }
        };

        let mut state = self.inner.state.lock().await;
        if state.disposed || state.generation != generation {
            let dispose_id = subscribed.subscription_id.clone();
            let ws = self.inner.ws.clone();
            let uuid = self.inner.uuid.clone();
            drop(state);
            let _ = dispose_query(&ws, &uuid, &dispose_id).await;
            // Disposed while opening: the caller still gets the snapshot.
            let state = self.inner.state.lock().await;
            return Ok(state
                .result
                .clone()
                .unwrap_or_else(|| subscribed.result.clone()));
        }
        state.id = Some(subscribed.subscription_id.clone());
        self.inner
            .replace_locked(state, subscribed.revision, subscribed.result.clone())
            .await;
        Ok(self
            .inner
            .state
            .lock()
            .await
            .result
            .clone()
            .unwrap_or(subscribed.result))
    }

    /// End the subscription: stop listening, dispose executor-side.
    pub async fn dispose(&self) {
        let id = {
            let mut s = self.inner.state.lock().await;
            s.disposed = true;
            s.generation += 1;
            s.buffer = None;
            s.id.take()
        };
        if let Some(h) = self.inner.listener.lock().unwrap().take() {
            h.abort();
        }
        let cb = self.inner.reconnect_cb.lock().unwrap().take();
        if let Some(cb) = cb {
            self.inner.ws.remove_on_reconnect(cb).await;
        }
        self.inner.ws.remove_watch(&self.inner.watch_key).await;
        if let Some(id) = id {
            let _ = dispose_query(&self.inner.ws, &self.inner.uuid, &id).await;
        }
    }
}

impl Inner {
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
            let _ = self.results.send(result);
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
        let _ = self.results.send(result);
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
        tokio::spawn(async move { this.resync_perform(generation, id).await });
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
        loop {
            let before = {
                let s = self.state.lock().await;
                if s.disposed {
                    return;
                }
                s.generation
            };
            // `start` bumps `generation`, so this attempt's generation is
            // `before + 1` — anything newer means another start overtook us.
            match LiveQuery::start(&LiveQuery {
                inner: self.clone(),
            })
            .await
            {
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
                    let retry = reopen_delay(failures);
                    drop(state);
                    eprintln!(
                        "[live-query] re-open failed (attempt {failures}), retrying in {retry}ms: {e}"
                    );
                    tokio::time::sleep(Duration::from_millis(retry)).await;
                }
            }
        }
    }
}

/// `perspective.disposeQuery` — ends a subscription on the executor.
async fn dispose_query(ws: &Arc<WsRpcClient>, uuid: &str, subscription_id: &str) -> Result<()> {
    ws.call_void(
        "perspective.disposeQuery",
        serde_json::json!({ "uuid": uuid, "subscriptionId": subscription_id }),
    )
    .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

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
}
