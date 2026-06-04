//! `HolographSpace` — lifecycle wrapper that wires our Step-3
//! `HolographIntegrationQueue` into a live Kitsune2 space.
//!
//! Responsibilities:
//!
//! - Construct (or accept) a `DynSpace` and use its `DynFetch` /
//!   `DynPeerStore` to back the queue's `OpFetcher` / `PeerPicker` trait
//!   surface via `K2FetcherAdapter` and `K2PeerPickerAdapter`.
//! - Bridge the queue's `NotifyUp` to a `tokio::sync::mpsc` channel the
//!   AD4M Language module (Step 5) will drain (`ChannelNotifier` +
//!   `EmittedOp`).
//! - `on_local_commit(envelope_bytes)` — for ops produced by the local
//!   commit path: feed through the queue (parents already present, so
//!   the queue stores + notifies straight away), then notify K2 via
//!   `inform_ops_stored` (so gossip will include the op) and
//!   `publish_ops` (eager hint to known peers).
//! - K2 `SpaceHandler::recv_notify` — passthrough into a telepresence
//!   sink (`TelepresenceNotification`); the JS side of telepresence is
//!   Step 5/6.
//! - `K2OpStoreShim` — a thin `OpStore` impl K2 sees in its builder
//!   slot. Routes inbound `process_incoming_ops` through the queue if
//!   installed, falls back to direct `KvOpStore` otherwise. All other
//!   `OpStore` methods delegate to the underlying store unchanged.
//!
//! Tokio runtime nesting (SPIKE §2.6): every async path through this
//! module is reached either from K2's own runtime (gossip/fetch
//! callbacks) or from the runtime handle stored on the queue. We never
//! `block_on` from inside the executor's main runtime.

use std::collections::HashSet;
use std::sync::Arc;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{
    DhtArc, DynFetch, DynPeerStore, DynSpace, K2Error, K2Result, OpId, OpStore, SpaceHandler,
    SpaceId, StoredOp, Timestamp, Url,
};
use tokio::sync::mpsc;

use crate::config::SpaceConfig;
use crate::integration_queue::{
    AlwaysValid, HolographIntegrationQueue, IntegrationQueueConfig, NotifyUp, OpFetcher,
    PeerPicker, SigVerifier,
};
use crate::op_store::{EnvelopeDecoder, KvOpStore};

/// What `ChannelNotifier` pushes onto its receiver for every op the
/// queue integrates. Carries enough for the AD4M Language module to
/// emit a `StoredOp` to gossip and surface the diff to JS subscribers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedOp {
    pub op_id: OpId,
    pub created_at: Timestamp,
    pub envelope_bytes: Bytes,
}

/// What `HolographSpace`'s K2 SpaceHandler forwards on each
/// `recv_notify`. Carries the sender's URL and the raw payload — the
/// Language module decodes and dispatches to telepresence subscribers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TelepresenceNotification {
    pub from_peer: Url,
    pub data: Bytes,
}

/// Adapter that wraps a K2 `DynFetch` so the integration queue's
/// `OpFetcher` calls hit real K2 fetch logic. Trait signature matches
/// `Fetch::request_ops` verbatim — this is type plumbing, no logic.
#[derive(Debug)]
pub struct K2FetcherAdapter {
    inner: DynFetch,
}

impl K2FetcherAdapter {
    pub fn new(inner: DynFetch) -> Arc<Self> {
        Arc::new(Self { inner })
    }
}

impl OpFetcher for K2FetcherAdapter {
    fn request_ops(&self, op_ids: Vec<OpId>, source: Url) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async move { self.inner.request_ops(op_ids, source).await })
    }
}

/// Adapter that wraps a K2 `DynPeerStore` so the queue's `PeerPicker`
/// finds real peers via `get_by_overlapping_storage_arc`.
///
/// The queue's contract: pick a peer whose storage arc overlaps `loc`
/// and is not already in `tried`. We materialize the K2 result and
/// pick the first non-tried agent with a known URL.
#[derive(Debug)]
pub struct K2PeerPickerAdapter {
    inner: DynPeerStore,
}

impl K2PeerPickerAdapter {
    pub fn new(inner: DynPeerStore) -> Arc<Self> {
        Arc::new(Self { inner })
    }
}

impl PeerPicker for K2PeerPickerAdapter {
    fn pick_arc_overlap_peer(
        &self,
        loc: u32,
        tried: HashSet<Url>,
    ) -> BoxFuture<'_, K2Result<Option<Url>>> {
        Box::pin(async move {
            // Build a 1-loc arc to query overlap against. K2's
            // `get_by_overlapping_storage_arc` returns all agents
            // whose storage_arc overlaps this range.
            let target = DhtArc::Arc(loc, loc);
            let agents = self.inner.get_by_overlapping_storage_arc(target).await?;
            for agent in agents {
                if let Some(url) = agent.url.clone() {
                    if !tried.contains(&url) {
                        return Ok(Some(url));
                    }
                }
            }
            Ok(None)
        })
    }
}

/// Channel-based `NotifyUp` — every integrated op pushes an
/// [`EmittedOp`] onto a `tokio::sync::mpsc::UnboundedSender`. The
/// Step-5 Language module owns the receiver and streams events to the
/// JS subscriber via `holograph_wires`.
#[derive(Debug)]
pub struct ChannelNotifier {
    tx: mpsc::UnboundedSender<EmittedOp>,
}

impl ChannelNotifier {
    pub fn new() -> (Arc<Self>, mpsc::UnboundedReceiver<EmittedOp>) {
        let (tx, rx) = mpsc::unbounded_channel();
        (Arc::new(Self { tx }), rx)
    }
}

impl NotifyUp for ChannelNotifier {
    fn emit_perspective_diff(&self, op_id: OpId, created_at: Timestamp, envelope_bytes: Bytes) {
        let item = EmittedOp {
            op_id,
            created_at,
            envelope_bytes,
        };
        // Channel send only fails if the receiver was dropped — log and move on.
        // Production should never see this (the Language module owns the receiver
        // for the lifetime of the space).
        if let Err(e) = self.tx.send(item) {
            tracing::warn!("ChannelNotifier: receiver gone, dropping diff: {e}");
        }
    }
}

/// What `HolographSpace::on_local_commit` needs from the live K2 space.
/// Real wiring goes through `DynSpace` via `K2DynSpaceTarget`; tests
/// use a mock impl so the commit-side logic is verifiable without
/// standing up the full K2 stack.
pub trait LocalCommitTarget: Send + Sync + std::fmt::Debug + 'static {
    /// Notify K2 that the listed ops are persisted and should be
    /// included in the DHT model going forward (gossip will sync them).
    fn inform_ops_stored(&self, ops: Vec<StoredOp>) -> BoxFuture<'_, K2Result<()>>;
    /// Eagerly hint to known peers that we have the listed op-ids
    /// available. Implementations fan out via `Publish::publish_ops`.
    fn publish_ops_to_peers(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<()>>;
    /// Wake-18 D3: best-effort transport teardown.
    /// `HolographSpace::shutdown` calls this so the K2 stack can drop
    /// transport handles, close iroh endpoints, etc. The default no-op
    /// covers test mocks and the in-process `K2DynSpaceTarget` for
    /// which the DynSpace's own Drop suffices.
    fn close<'a>(&'a self) -> BoxFuture<'a, K2Result<()>> {
        Box::pin(async move { Ok(()) })
    }
}

/// Production `LocalCommitTarget` backed by a K2 `DynSpace`. Publishes
/// to every peer in the local peer store with a non-empty URL — v1 uses
/// full-arc replication, so every peer should hold every op, hence the
/// fan-out.
#[derive(Debug)]
pub struct K2DynSpaceTarget {
    space: DynSpace,
}

impl K2DynSpaceTarget {
    pub fn new(space: DynSpace) -> Arc<Self> {
        Arc::new(Self { space })
    }
}

impl LocalCommitTarget for K2DynSpaceTarget {
    fn inform_ops_stored(&self, ops: Vec<StoredOp>) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async move { self.space.inform_ops_stored(ops).await })
    }

    fn publish_ops_to_peers(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<()>> {
        let space = self.space.clone();
        Box::pin(async move {
            let agents = space.peer_store().get_all().await?;
            let publish = space.publish();
            let me = space.current_url();
            tracing::info!(
                target: "holograph",
                "publish_ops_to_peers: op_ids={} peers={} self_url={:?}",
                op_ids.len(),
                agents.len(),
                me.as_ref().map(|u| u.to_string()),
            );
            let mut sent = 0usize;
            let mut skipped_self = 0usize;
            let mut skipped_no_url = 0usize;
            for agent in agents {
                let agent_url = agent.url.clone();
                if let Some(my_url) = &me {
                    if agent.url.as_ref() == Some(my_url) {
                        skipped_self += 1;
                        continue;
                    }
                }
                if let Some(target) = agent_url {
                    tracing::info!(
                        target: "holograph",
                        "publish_ops_to_peers: -> {}",
                        target
                    );
                    publish.publish_ops(op_ids.clone(), target).await?;
                    sent += 1;
                } else {
                    skipped_no_url += 1;
                }
            }
            tracing::info!(
                target: "holograph",
                "publish_ops_to_peers: sent={} skipped_self={} skipped_no_url={}",
                sent,
                skipped_self,
                skipped_no_url,
            );
            Ok(())
        })
    }
}

/// SpaceHandler bridge — K2 calls into this on `recv_notify` (peer-to-peer
/// notification, used by telepresence). Forwards onto an mpsc that the
/// Language module drains. The JS-facing side is Step 5/6.
#[derive(Debug)]
pub struct HolographSpaceHandler {
    notify_tx: mpsc::UnboundedSender<TelepresenceNotification>,
}

impl HolographSpaceHandler {
    pub fn new() -> (Arc<Self>, mpsc::UnboundedReceiver<TelepresenceNotification>) {
        let (tx, rx) = mpsc::unbounded_channel();
        (Arc::new(Self { notify_tx: tx }), rx)
    }
}

impl SpaceHandler for HolographSpaceHandler {
    fn recv_notify(&self, from_peer: Url, _space_id: SpaceId, data: bytes::Bytes) -> K2Result<()> {
        let msg = TelepresenceNotification { from_peer, data };
        if let Err(e) = self.notify_tx.send(msg) {
            tracing::warn!("HolographSpaceHandler: telepresence receiver gone, dropping: {e}");
        }
        Ok(())
    }
}

/// Build inputs for `HolographSpace::new`. Split out from K2
/// construction so unit tests can pass a mock `LocalCommitTarget` and
/// the integration test wires the real K2 `DynSpace`.
pub struct HolographSpaceConfig {
    pub config: SpaceConfig,
    pub op_store: Arc<KvOpStore>,
    pub pending: sled::Tree,
    pub decode_envelope: EnvelopeDecoder,
    pub fetcher: Arc<dyn OpFetcher>,
    pub peer_picker: Arc<dyn PeerPicker>,
    pub notify: Arc<dyn NotifyUp>,
    pub commit_target: Arc<dyn LocalCommitTarget>,
    pub sig_verifier: Arc<dyn SigVerifier>,
    pub runtime: tokio::runtime::Handle,
    pub watcher_tick: std::time::Duration,
}

impl HolographSpaceConfig {
    /// Sensible-default builder opts: `AlwaysValid` sig verifier, 1s
    /// watcher tick. Fetch-fallback timings come from
    /// `SpaceConfig::fetch_fallback_policy` (defaults: 5s/3-peers/30s,
    /// see `FetchFallbackPolicy::default`). Tests and production
    /// usually start from this.
    #[allow(clippy::too_many_arguments)]
    pub fn defaults(
        config: SpaceConfig,
        op_store: Arc<KvOpStore>,
        pending: sled::Tree,
        decode_envelope: EnvelopeDecoder,
        fetcher: Arc<dyn OpFetcher>,
        peer_picker: Arc<dyn PeerPicker>,
        notify: Arc<dyn NotifyUp>,
        commit_target: Arc<dyn LocalCommitTarget>,
        runtime: tokio::runtime::Handle,
    ) -> Self {
        Self {
            config,
            op_store,
            pending,
            decode_envelope,
            fetcher,
            peer_picker,
            notify,
            commit_target,
            sig_verifier: Arc::new(AlwaysValid),
            runtime,
            watcher_tick: std::time::Duration::from_secs(1),
        }
    }
}

/// The top-level Holograph substrate handle for a single AD4M
/// neighborhood. Owns the queue, the op-store, and the K2 commit-target.
pub struct HolographSpace {
    config: SpaceConfig,
    queue: Arc<HolographIntegrationQueue>,
    op_store: Arc<KvOpStore>,
    decode_envelope: EnvelopeDecoder,
    commit_target: Arc<dyn LocalCommitTarget>,
    /// Wake-18 D3 shutdown flag. `on_local_commit` consults this and
    /// rejects new commits once flipped — drains-in-flight finish but
    /// no new work piles up.
    shutdown_requested: std::sync::Arc<std::sync::atomic::AtomicBool>,
}

impl std::fmt::Debug for HolographSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HolographSpace")
            .field("config", &self.config)
            .field("queue", &self.queue)
            .finish()
    }
}

impl HolographSpace {
    /// Construct a `HolographSpace`. The queue is created from the
    /// passed-in op-store + adapters; the watcher is started on the
    /// supplied runtime handle.
    ///
    /// `commit_target` is the K2-side sink for `on_local_commit`. In
    /// production this is `K2DynSpaceTarget` wrapping `DynSpace`; in
    /// unit tests it's a recording mock.
    pub fn new(cfg: HolographSpaceConfig) -> Arc<Self> {
        let queue = HolographIntegrationQueue::new(IntegrationQueueConfig {
            op_store: Arc::clone(&cfg.op_store),
            pending: cfg.pending,
            decode_envelope: Arc::clone(&cfg.decode_envelope),
            arc_policy: cfg.config.arc_policy,
            notify: cfg.notify,
            fetcher: cfg.fetcher,
            peer_picker: cfg.peer_picker,
            sig_verifier: cfg.sig_verifier,
            fallback_policy: cfg.config.fetch_fallback_policy,
            watcher_tick: cfg.watcher_tick,
            runtime: cfg.runtime,
        });
        queue.start_watcher();
        // Wake-18 D6: if SpaceConfig didn't carry an explicit relay
        // URL, resolve from env. The resolved value is folded back
        // into the stored config so downstream consumers
        // (`HolographSpace::config()`) see one canonical surface and
        // never have to reach for `std::env::var` themselves.
        let mut config = cfg.config;
        if config.iroh_relay_url.is_none() {
            config.iroh_relay_url = crate::config::resolve_iroh_relay();
        }
        Arc::new(Self {
            config,
            queue,
            op_store: cfg.op_store,
            decode_envelope: cfg.decode_envelope,
            commit_target: cfg.commit_target,
            shutdown_requested: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        })
    }

    /// Inbound entry for fetched/gossiped ops from K2 (also reachable
    /// from `K2OpStoreShim::process_incoming_ops`).
    pub async fn process_incoming_ops(
        &self,
        op_list: Vec<Bytes>,
        source: Option<Url>,
    ) -> K2Result<Vec<OpId>> {
        self.queue.process_incoming_ops(op_list, source).await
    }

    /// Locally committed op: route through the queue (parents are
    /// already present locally, so the queue takes the
    /// all-parents-present branch and stores + notifies), then notify
    /// K2 of the new persisted op + publish to peers.
    pub async fn on_local_commit(&self, envelope_bytes: Bytes) -> K2Result<OpId> {
        if self
            .shutdown_requested
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(K2Error::other(
                "HolographSpace::on_local_commit: shutdown in progress",
            ));
        }

        let (op_id, created_at) = (self.decode_envelope)(envelope_bytes.as_ref())?;

        let accepted = self
            .queue
            .process_incoming_ops(vec![envelope_bytes], None)
            .await?;
        if accepted.is_empty() {
            return Err(K2Error::other(
                "HolographSpace::on_local_commit: queue rejected op (arc filter?)",
            ));
        }

        self.commit_target
            .inform_ops_stored(vec![StoredOp {
                op_id: op_id.clone(),
                created_at,
            }])
            .await?;

        self.commit_target
            .publish_ops_to_peers(vec![op_id.clone()])
            .await?;

        Ok(op_id)
    }

    pub fn config(&self) -> &SpaceConfig {
        &self.config
    }

    pub fn queue(&self) -> &Arc<HolographIntegrationQueue> {
        &self.queue
    }

    pub fn op_store(&self) -> &Arc<KvOpStore> {
        &self.op_store
    }

    /// Read the current persisted op count without going through the
    /// async OpStore trait — useful for tests + smoketests + status
    /// observability.
    pub fn op_count(&self) -> u64 {
        self.op_store.op_count_blocking()
    }

    /// Wake-18 D3 — graceful shutdown.
    ///
    /// 1. Stop accepting new commits (sets the shutdown flag observed by
    ///    `on_local_commit`).
    /// 2. Stop the queue's fallback watcher so no new fetches are issued.
    /// 3. Drain the integration queue: poll `pending_len() == 0` or 10s
    ///    timeout, whichever comes first.
    /// 4. `flush_async` the sled DB so the on-disk state is durable.
    /// 5. `commit_target.close()` so the K2 transport (iroh) tears down.
    ///
    /// Returns the unflushed pending count if step 3 timed out so the
    /// caller can surface a "drain didn't complete in time" signal.
    /// Step 4 / 5 always run regardless.
    pub async fn shutdown(&self) -> K2Result<usize> {
        self.shutdown_requested
            .store(true, std::sync::atomic::Ordering::Release);
        self.queue.stop_watcher();

        let drain_deadline =
            std::time::Instant::now() + std::time::Duration::from_secs(10);
        let mut remaining = self.queue.pending_len();
        while remaining > 0 && std::time::Instant::now() < drain_deadline {
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            remaining = self.queue.pending_len();
        }
        if remaining > 0 {
            tracing::warn!(
                "HolographSpace::shutdown: drain timed out with {} pending",
                remaining
            );
        }

        if let Err(e) = self.op_store.flush_async().await {
            tracing::warn!("HolographSpace::shutdown: flush_async failed: {e}");
        }

        if let Err(e) = self.commit_target.close().await {
            tracing::warn!("HolographSpace::shutdown: commit_target.close failed: {e}");
        }

        Ok(remaining)
    }
}

impl Drop for HolographSpace {
    /// Wake-18 D3 — best-effort sync flush on drop.
    ///
    /// The async `shutdown()` is the preferred path; `Drop` is the
    /// safety net for "process exit before shutdown was called." We
    /// can only do a sync flush here (no async runtime guaranteed),
    /// and we log + swallow errors instead of panicking — a `Drop`
    /// that panics during unwinding aborts the process.
    fn drop(&mut self) {
        self.shutdown_requested
            .store(true, std::sync::atomic::Ordering::Release);
        self.queue.stop_watcher();
        if let Err(e) = self.op_store.flush_blocking() {
            tracing::warn!("HolographSpace::drop: flush_blocking failed: {e}");
        }
    }
}

/// `OpStore` shim installed into K2's `Builder.op_store` slot. K2 will
/// call this for incoming gossip/fetched ops; this routes through the
/// integration queue if installed, falling back to direct `KvOpStore`
/// storage otherwise (queue isn't built yet during the brief
/// construction window).
///
/// All non-`process_incoming_ops` calls delegate to the underlying
/// `KvOpStore` unchanged — the queue only intercepts the integration
/// path; persistence + gossip Merkle bookkeeping stay on the store.
pub struct K2OpStoreShim {
    op_store: Arc<KvOpStore>,
    queue: std::sync::RwLock<Option<Arc<HolographIntegrationQueue>>>,
}

impl std::fmt::Debug for K2OpStoreShim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("K2OpStoreShim")
            .field("op_store", &self.op_store)
            .field(
                "queue_installed",
                &self.queue.read().map(|q| q.is_some()).unwrap_or(false),
            )
            .finish()
    }
}

impl K2OpStoreShim {
    pub fn new(op_store: Arc<KvOpStore>) -> Arc<Self> {
        Arc::new(Self {
            op_store,
            queue: std::sync::RwLock::new(None),
        })
    }

    /// Install the integration queue. Called after `HolographSpace`
    /// finishes building the K2 stack and has the queue ready.
    pub fn install_queue(&self, queue: Arc<HolographIntegrationQueue>) {
        let mut slot = self.queue.write().expect("queue rwlock poisoned");
        *slot = Some(queue);
    }
}

impl OpStore for K2OpStoreShim {
    fn process_incoming_ops(&self, op_list: Vec<Bytes>) -> BoxFuture<'_, K2Result<Vec<OpId>>> {
        Box::pin(async move {
            let queue_opt = self.queue.read().expect("queue rwlock poisoned").clone();
            if let Some(q) = queue_opt {
                // K2's OpStore::process_incoming_ops doesn't thread a
                // source URL through, so we pass None; the multi-peer
                // fallback watcher will surface any missing parents
                // via `PeerPicker` once the timeout fires.
                q.process_incoming_ops(op_list, None).await
            } else {
                self.op_store.process_incoming_ops(op_list).await
            }
        })
    }

    fn retrieve_op_hashes_in_time_slice(
        &self,
        arc: DhtArc,
        start: Timestamp,
        end: Timestamp,
    ) -> BoxFuture<'_, K2Result<(Vec<OpId>, u32)>> {
        self.op_store
            .retrieve_op_hashes_in_time_slice(arc, start, end)
    }

    fn retrieve_ops(
        &self,
        op_ids: Vec<OpId>,
    ) -> BoxFuture<'_, K2Result<Vec<kitsune2_api::MetaOp>>> {
        self.op_store.retrieve_ops(op_ids)
    }

    fn filter_out_existing_ops(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<Vec<OpId>>> {
        self.op_store.filter_out_existing_ops(op_ids)
    }

    fn retrieve_op_ids_bounded(
        &self,
        arc: DhtArc,
        start: Timestamp,
        limit_bytes: u32,
    ) -> BoxFuture<'_, K2Result<(Vec<OpId>, u32, Timestamp)>> {
        self.op_store
            .retrieve_op_ids_bounded(arc, start, limit_bytes)
    }

    fn earliest_timestamp_in_arc(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<Option<Timestamp>>> {
        self.op_store.earliest_timestamp_in_arc(arc)
    }

    fn store_slice_hash(
        &self,
        arc: DhtArc,
        slice_index: u64,
        slice_hash: Bytes,
    ) -> BoxFuture<'_, K2Result<()>> {
        self.op_store.store_slice_hash(arc, slice_index, slice_hash)
    }

    fn slice_hash_count(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<u64>> {
        self.op_store.slice_hash_count(arc)
    }

    fn retrieve_slice_hash(
        &self,
        arc: DhtArc,
        slice_index: u64,
    ) -> BoxFuture<'_, K2Result<Option<Bytes>>> {
        self.op_store.retrieve_slice_hash(arc, slice_index)
    }

    fn retrieve_slice_hashes(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<Vec<(u64, Bytes)>>> {
        self.op_store.retrieve_slice_hashes(arc)
    }

    fn query_total_op_count(&self) -> BoxFuture<'_, K2Result<u64>> {
        self.op_store.query_total_op_count()
    }
}

/// The envelope decoder Holograph spaces install on their `KvOpStore`.
/// Re-exported from `retriever_kitsune` so the space module can stay
/// independent of the retriever module's internals.
pub use crate::retriever_kitsune::holograph_envelope_decoder;

#[cfg(test)]
mod tests;
