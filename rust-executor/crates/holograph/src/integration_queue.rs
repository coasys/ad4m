//! `HolographIntegrationQueue` — sits above `KvOpStore` and is the
//! K2-facing entry point for incoming ops.
//!
//! Pipeline per inbound envelope:
//!
//! 1. Decode envelope, compute op-id via the shared `EnvelopeDecoder`.
//! 2. Verify signature (`SigVerifier`). Reject envelopes that fail —
//!    they never touch the OpStore or the pending tree.
//! 3. Consult the arc policy. Ops outside the local arc are silently
//!    skipped (sharding-ready commitment 1).
//! 4. Check parent presence in `KvOpStore`:
//!    - all present → store via `KvOpStore::process_incoming_ops`,
//!      notify-up, cascade-promote pending ops waiting on this op-id.
//!    - some missing → pend in the sled `pending` tree keyed by op-id,
//!      call `OpFetcher::request_ops(missing_parents, source)` so K2
//!      goes fetch them.
//!
//! Multi-peer fallback (SPIKE §1.1): a `tokio::spawn`'d watcher task
//! periodically scans pending entries whose first-seen timestamp is
//! older than `fallback_timeout_ms` and re-requests their missing
//! parents from an alternative peer chosen via `PeerPicker`. This is
//! the *load-bearing* piece SPIKE §1.1 calls out: K2's fetch is
//! source-bound and will silently drop a request if the source goes
//! offline, so without this loop a stalled fetch leaves an op
//! permanently pending.
//!
//! Cascade promotion is recursive — a promoted op may itself be a
//! parent of other pending ops. We use a worklist (not recursion) to
//! bound stack depth at long chain depths.
//!
//! Restart survives state: the pending tree is sled-backed, so a
//! re-instantiated queue resumes outstanding fetches on next watcher
//! tick.
//!
//! Tokio runtime nesting (SPIKE §2.6 risk): the queue owns no runtime
//! itself; it accepts an `Arc<tokio::runtime::Runtime>` (or any handle
//! that can spawn) at construction. v1's wiring (Step 4) hands it the
//! same dedicated runtime `KitsuneRetreiverState` uses. Production
//! must not pass the executor's main runtime here for the same reason.

use std::collections::{HashSet, VecDeque};
use std::sync::Arc;
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{K2Error, K2Result, OpId, OpStore, Timestamp, Url};
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

use crate::config::{ArcPolicy, FetchFallbackPolicy};
use crate::envelope::OpEnvelope;
use crate::op_store::{EnvelopeDecoder, KvOpStore};

/// Sink for "this op is integration-ready; propagate it to subscribers."
/// Step 4 plugs both AD4M's perspective-diff emit and K2's
/// `Space::inform_ops_stored` (gossip bookkeeping) here. `created_at` is
/// the envelope's authoring timestamp — propagated unchanged from the
/// origin so every peer derives the same `StoredOp{op_id, created_at}`
/// pair for gossip.
pub trait NotifyUp: Send + Sync + std::fmt::Debug + 'static {
    fn emit_perspective_diff(&self, op_id: OpId, created_at: Timestamp, envelope_bytes: Bytes);

    /// Wake-18 D5: notify upstream that we've given up on fetching the
    /// missing parents for a pending op. The op is dropped from the
    /// pending tree; upstream consumers may want to surface this as a
    /// signal or escalate. Default no-op so older NotifyUp impls keep
    /// working.
    fn notify_parent_fetch_permanent_failure(
        &self,
        _op_id: OpId,
        _missing_parents: Vec<OpId>,
        _last_error: String,
    ) {
    }
}

/// What the queue needs from K2's fetch module. Trait surface matches
/// `kitsune2_api::Fetch::request_ops` so Step 4 can plug `DynFetch`
/// directly.
pub trait OpFetcher: Send + Sync + std::fmt::Debug + 'static {
    fn request_ops(&self, op_ids: Vec<OpId>, source: Url) -> BoxFuture<'_, K2Result<()>>;
}

/// What the queue needs from the peer store for fallback peer selection.
/// Picks any peer with arc overlap on `loc`, excluding any URL in
/// `tried`. Returns `None` if no alternative is available — the queue
/// then leaves the entry pending; the next gossip round may surface it.
pub trait PeerPicker: Send + Sync + std::fmt::Debug + 'static {
    fn pick_arc_overlap_peer(
        &self,
        loc: u32,
        tried: HashSet<Url>,
    ) -> BoxFuture<'_, K2Result<Option<Url>>>;
}

/// Validation of an envelope's signature. v1 doesn't actually do any
/// cryptography (SPIKE §2.4 — "no SHACL, no real validation"); the
/// `AlwaysValid` impl is the production default. Tests can plug in
/// rejecting impls to exercise the rejection path.
pub trait SigVerifier: Send + Sync + std::fmt::Debug + 'static {
    fn verify(&self, envelope: &OpEnvelope) -> bool;
}

/// Default sig verifier — every envelope passes. Real validation lives
/// in a future spike (deferred from SPIKE §2.4).
#[derive(Debug, Default)]
pub struct AlwaysValid;
impl SigVerifier for AlwaysValid {
    fn verify(&self, _: &OpEnvelope) -> bool {
        true
    }
}

/// Sled-encoded pending entry. CBOR'd via ciborium.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct PendingEntry {
    /// Original envelope bytes, ready to be re-fed to `process_incoming_ops`
    /// on promotion. We can't decode-then-re-encode round-trip without
    /// risking subtle field reordering; storing raw bytes keeps the
    /// op-id stable.
    envelope_bytes: Vec<u8>,
    /// Parents we're still waiting on. Drained as parents arrive.
    missing_parents: Vec<Vec<u8>>,
    /// The peer URL we originally fetched from. May be `None` if the
    /// op was pended via a local commit path (rare).
    source: Option<String>,
    /// Wall-clock at first ingest (micros since Unix epoch). The
    /// watcher uses this for the fallback timeout.
    first_seen_micros: i64,
    /// Peers we've already tried, deduped. We won't re-request from
    /// any of these.
    tried_peers: Vec<String>,
}

fn now_micros() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_micros() as i64)
        .unwrap_or(0)
}

fn opid_bytes(op_id: &OpId) -> Vec<u8> {
    Bytes::from(op_id.clone()).to_vec()
}

fn bytes_to_opid(b: &[u8]) -> OpId {
    OpId::from(Bytes::copy_from_slice(b))
}

/// Builder-style configuration for the queue.
pub struct IntegrationQueueConfig {
    pub op_store: Arc<KvOpStore>,
    pub pending: sled::Tree,
    pub decode_envelope: EnvelopeDecoder,
    pub arc_policy: ArcPolicy,
    pub notify: Arc<dyn NotifyUp>,
    pub fetcher: Arc<dyn OpFetcher>,
    pub peer_picker: Arc<dyn PeerPicker>,
    pub sig_verifier: Arc<dyn SigVerifier>,
    /// Wake-18 D2: structured fallback policy (initial_timeout,
    /// max_attempts, retry_budget). Replaces the previous `fallback_timeout`
    /// + `max_retry_peers` pair.
    pub fallback_policy: FetchFallbackPolicy,
    pub watcher_tick: Duration,
    pub runtime: tokio::runtime::Handle,
}

pub struct HolographIntegrationQueue {
    op_store: Arc<KvOpStore>,
    pending: sled::Tree,
    decode_envelope: EnvelopeDecoder,
    arc_policy: ArcPolicy,
    notify: Arc<dyn NotifyUp>,
    fetcher: Arc<dyn OpFetcher>,
    peer_picker: Arc<dyn PeerPicker>,
    sig_verifier: Arc<dyn SigVerifier>,
    fallback_policy: FetchFallbackPolicy,
    watcher_tick: Duration,
    runtime: tokio::runtime::Handle,
    /// Coarse async lock around process/cascade. The pending tree and
    /// op-store are individually transactional, but cascade is a
    /// read-modify-write across multiple keys; the lock keeps it
    /// race-free without us reaching for sled transactions.
    gate: Mutex<()>,
    watcher_handle: std::sync::Mutex<Option<JoinHandle<()>>>,
}

impl std::fmt::Debug for HolographIntegrationQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HolographIntegrationQueue")
            .field("arc_policy", &self.arc_policy)
            .field("fallback_policy", &self.fallback_policy)
            .field("watcher_tick", &self.watcher_tick)
            .finish()
    }
}

impl HolographIntegrationQueue {
    pub fn new(cfg: IntegrationQueueConfig) -> Arc<Self> {
        Arc::new(Self {
            op_store: cfg.op_store,
            pending: cfg.pending,
            decode_envelope: cfg.decode_envelope,
            arc_policy: cfg.arc_policy,
            notify: cfg.notify,
            fetcher: cfg.fetcher,
            peer_picker: cfg.peer_picker,
            sig_verifier: cfg.sig_verifier,
            fallback_policy: cfg.fallback_policy,
            watcher_tick: cfg.watcher_tick,
            runtime: cfg.runtime,
            gate: Mutex::new(()),
            watcher_handle: std::sync::Mutex::new(None),
        })
    }

    /// Process a batch of inbound envelopes. Returns op-ids that were
    /// either stored or pended (i.e., the queue has taken ownership of
    /// them — they won't be silently dropped).
    ///
    /// `source` is the peer URL the bytes came from, used as the
    /// initial fetch target for any missing parents. Pass `None` for
    /// locally-originated ops (a local commit on this node).
    pub async fn process_incoming_ops(
        &self,
        op_list: Vec<Bytes>,
        source: Option<Url>,
    ) -> K2Result<Vec<OpId>> {
        let _guard = self.gate.lock().await;
        let mut accepted_or_pended = Vec::with_capacity(op_list.len());
        for bytes in op_list {
            if let Some(op_id) = self.integrate_one(bytes, source.clone()).await? {
                accepted_or_pended.push(op_id);
            }
        }
        Ok(accepted_or_pended)
    }

    /// Internal: decode/verify/arc-filter/persistence logic for a
    /// single envelope. Returns `Some(op_id)` if the op was either
    /// stored or pended (the caller now owns its lifecycle); `None`
    /// if it was dropped (sig fail, outside arc, decode fail).
    async fn integrate_one(
        &self,
        envelope_bytes: Bytes,
        source: Option<Url>,
    ) -> K2Result<Option<OpId>> {
        // 1. Decode envelope to inspect parents + (in the future)
        //    signature contents.
        let env = match OpEnvelope::decode(envelope_bytes.as_ref()) {
            Ok(e) => e,
            Err(_) => return Ok(None),
        };

        // 2. Signature verification. v1 default = always valid.
        if !self.sig_verifier.verify(&env) {
            return Ok(None);
        }

        // 3. Op-id derivation via the same decoder KvOpStore uses,
        //    so an op identified here is the same op identified there.
        let (op_id, created_at) = (self.decode_envelope)(envelope_bytes.as_ref())?;

        // 4. Arc filter. Sharding-ready commitment 1.
        if !self.arc_policy.target_arc().contains(op_id.loc()) {
            return Ok(None);
        }

        // 5. Already-have shortcut: don't re-process. Dedup hits this
        //    when the same op arrives twice (gossip + publish, or a
        //    duplicate fetch response).
        if self.op_has(&op_id).await? {
            return Ok(Some(op_id));
        }
        if self.pending_contains(&op_id)? {
            // Already pending; nothing to do.
            return Ok(Some(op_id));
        }

        // 6. Parent presence check.
        let parents = env
            .parents
            .iter()
            .map(|b| OpId::from(b.clone()))
            .collect::<Vec<_>>();
        let missing = self.missing_parents(&parents).await?;

        if missing.is_empty() {
            // All parents present (or no parents) → store + notify +
            // cascade.
            self.store_and_promote(op_id.clone(), created_at, envelope_bytes)
                .await?;
            Ok(Some(op_id))
        } else {
            // Pend and request from source.
            self.pend(&op_id, envelope_bytes, &missing, source.clone())?;
            if let Some(src) = source {
                self.fetcher
                    .request_ops(missing, src)
                    .await
                    .map_err(|e| K2Error::other_src("fetcher.request_ops", e))?;
            }
            // We took ownership.
            Ok(Some(op_id))
        }
    }

    /// Hand `envelope_bytes` to KvOpStore, then notify-up, then
    /// cascade-promote any pending ops that were waiting on `op_id`.
    /// The cascade is a worklist, not recursion — long chains stay
    /// stack-safe.
    async fn store_and_promote(
        &self,
        op_id: OpId,
        created_at: Timestamp,
        envelope_bytes: Bytes,
    ) -> K2Result<()> {
        // Delegate raw storage to KvOpStore. It re-decodes, but the
        // closure-injected EnvelopeDecoder will produce the same
        // op-id we computed above.
        let stored = self
            .op_store
            .process_incoming_ops(vec![envelope_bytes.clone()])
            .await?;
        if stored.is_empty() {
            // arc filter inside the op-store rejected it. We already
            // arc-checked, so this would be surprising — but stay quiet
            // rather than break the cascade.
            return Ok(());
        }

        self.notify
            .emit_perspective_diff(op_id.clone(), created_at, envelope_bytes);

        // Cascade worklist: every newly-stored op-id may unblock
        // pending entries waiting on it as a parent.
        let mut worklist: VecDeque<OpId> = VecDeque::new();
        worklist.push_back(op_id);

        while let Some(parent_id) = worklist.pop_front() {
            let unblocked = self.drain_pending_for(&parent_id)?;
            for (child_id, child_envelope) in unblocked {
                let stored = self
                    .op_store
                    .process_incoming_ops(vec![child_envelope.clone()])
                    .await?;
                if stored.is_empty() {
                    continue;
                }
                // Re-derive timestamp for the promoted child — it lives
                // in its envelope bytes, which we've held in the
                // pending tree.
                let (_re_id, child_ts) = (self.decode_envelope)(child_envelope.as_ref())?;
                self.notify
                    .emit_perspective_diff(child_id.clone(), child_ts, child_envelope);
                worklist.push_back(child_id);
            }
        }

        Ok(())
    }

    /// Scan `pending` for entries whose `missing_parents` contains
    /// `parent_id`. For each, drop `parent_id` from the missing list;
    /// if it becomes empty, remove the entry and return it for
    /// promotion. Otherwise persist the updated entry.
    fn drain_pending_for(&self, parent_id: &OpId) -> K2Result<Vec<(OpId, Bytes)>> {
        let parent_bytes = opid_bytes(parent_id);
        let mut promotable = Vec::new();
        // Snapshot the keys we need to inspect; we'll re-fetch each
        // under decode below so we never mutate while iterating.
        let keys: Vec<sled::IVec> = self
            .pending
            .iter()
            .keys()
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| K2Error::other_src("pending.iter.keys", e))?;
        for k in keys {
            let v = match self
                .pending
                .get(&k)
                .map_err(|e| K2Error::other_src("pending.get", e))?
            {
                Some(v) => v,
                None => continue,
            };
            let mut entry: PendingEntry = match ciborium::from_reader(v.as_ref()) {
                Ok(e) => e,
                Err(_) => continue,
            };
            if !entry.missing_parents.iter().any(|p| p == &parent_bytes) {
                continue;
            }
            entry.missing_parents.retain(|p| p != &parent_bytes);
            if entry.missing_parents.is_empty() {
                let child_id = OpId::from(Bytes::copy_from_slice(&k));
                let child_envelope = Bytes::from(entry.envelope_bytes.clone());
                self.pending
                    .remove(&k)
                    .map_err(|e| K2Error::other_src("pending.remove", e))?;
                promotable.push((child_id, child_envelope));
            } else {
                let mut buf = Vec::new();
                ciborium::into_writer(&entry, &mut buf)
                    .map_err(|e| K2Error::other_src("encode pending", e))?;
                self.pending
                    .insert(&k, buf)
                    .map_err(|e| K2Error::other_src("pending.insert", e))?;
            }
        }
        Ok(promotable)
    }

    fn pend(
        &self,
        op_id: &OpId,
        envelope_bytes: Bytes,
        missing: &[OpId],
        source: Option<Url>,
    ) -> K2Result<()> {
        let entry = PendingEntry {
            envelope_bytes: envelope_bytes.to_vec(),
            missing_parents: missing.iter().map(opid_bytes).collect(),
            source: source.as_ref().map(|u| u.as_str().to_string()),
            first_seen_micros: now_micros(),
            tried_peers: source
                .as_ref()
                .map(|u| vec![u.as_str().to_string()])
                .unwrap_or_default(),
        };
        let mut buf = Vec::new();
        ciborium::into_writer(&entry, &mut buf)
            .map_err(|e| K2Error::other_src("encode pending", e))?;
        self.pending
            .insert(opid_bytes(op_id), buf)
            .map_err(|e| K2Error::other_src("pending.insert", e))?;
        Ok(())
    }

    fn pending_contains(&self, op_id: &OpId) -> K2Result<bool> {
        Ok(self
            .pending
            .contains_key(opid_bytes(op_id))
            .map_err(|e| K2Error::other_src("pending.contains_key", e))?)
    }

    async fn op_has(&self, op_id: &OpId) -> K2Result<bool> {
        let still_missing = self
            .op_store
            .filter_out_existing_ops(vec![op_id.clone()])
            .await?;
        Ok(still_missing.is_empty())
    }

    async fn missing_parents(&self, parents: &[OpId]) -> K2Result<Vec<OpId>> {
        if parents.is_empty() {
            return Ok(Vec::new());
        }
        let missing = self
            .op_store
            .filter_out_existing_ops(parents.to_vec())
            .await?;
        // filter_out_existing_ops returns parents we DON'T have — those
        // are exactly the ones missing.
        Ok(missing)
    }

    /// Pending count — handy for tests + a "what's still in flight"
    /// observability hook.
    pub fn pending_len(&self) -> usize {
        self.pending.len()
    }

    /// Take a snapshot of pending op-ids. Tests use this; observability
    /// may also want it later.
    pub fn pending_op_ids(&self) -> Vec<OpId> {
        self.pending
            .iter()
            .keys()
            .filter_map(|k| k.ok())
            .map(|k| OpId::from(Bytes::copy_from_slice(&k)))
            .collect()
    }

    /// Spawn the fallback watcher onto the configured runtime. Returns
    /// the running queue (so callers can chain). Safe to call multiple
    /// times — subsequent calls are no-ops.
    pub fn start_watcher(self: &Arc<Self>) {
        let mut slot = self.watcher_handle.lock().expect("watcher_handle poisoned");
        if slot.is_some() {
            return;
        }
        let queue = Arc::clone(self);
        let handle = self.runtime.spawn(async move {
            queue.watcher_loop().await;
        });
        *slot = Some(handle);
    }

    /// Stop the watcher. Tests use this to ensure the spawn doesn't
    /// outlive the test runtime.
    pub fn stop_watcher(&self) {
        let mut slot = self.watcher_handle.lock().expect("watcher_handle poisoned");
        if let Some(h) = slot.take() {
            h.abort();
        }
    }

    async fn watcher_loop(self: Arc<Self>) {
        loop {
            tokio::time::sleep(self.watcher_tick).await;
            if let Err(e) = self.fallback_pass().await {
                tracing::warn!("fallback pass failed: {e}");
            }
        }
    }

    /// One pass of the multi-peer fallback loop. Test-callable.
    ///
    /// Wake-18 D2/D5: each pass round-robins through arc-overlapping
    /// peers in a single tick (instead of one peer per tick). The
    /// policy caps both the per-entry peer count
    /// (`max_attempts`) and total wall-clock spent on the entry
    /// (`retry_budget`). When either cap is hit the entry is dropped
    /// and `NotifyUp::notify_parent_fetch_permanent_failure` fires so
    /// upstream layers (the perspective-diff emit path, AD4M Signal
    /// surface) can react.
    pub async fn fallback_pass(&self) -> K2Result<()> {
        let _guard = self.gate.lock().await;
        let now = now_micros();
        let policy = self.fallback_policy;
        let initial_timeout_micros = policy.initial_timeout.as_micros() as i64;
        let retry_budget_micros = policy.retry_budget.as_micros() as i64;

        let mut work: Vec<(sled::IVec, PendingEntry)> = Vec::new();
        for kv in self.pending.iter() {
            let (k, v) = kv.map_err(|e| K2Error::other_src("pending.iter", e))?;
            let entry: PendingEntry = match ciborium::from_reader(v.as_ref()) {
                Ok(e) => e,
                Err(_) => continue,
            };
            // Skip entries that haven't even hit the initial timeout —
            // give the original source a chance to deliver first.
            if (now - entry.first_seen_micros) < initial_timeout_micros {
                continue;
            }
            work.push((k, entry));
        }

        for (k, mut entry) in work {
            // Budget check — if we've spent more than retry_budget on
            // this entry, drop it as a permanent failure.
            let age_micros = now - entry.first_seen_micros;
            let budget_exhausted = age_micros >= retry_budget_micros;
            let attempts_exhausted = (entry.tried_peers.len() as u8) >= policy.max_attempts;

            if budget_exhausted || attempts_exhausted {
                self.drop_pending_permanent_failure(
                    &k,
                    &entry,
                    if budget_exhausted {
                        "retry_budget exhausted"
                    } else {
                        "max_attempts exhausted"
                    },
                )?;
                continue;
            }

            // Wake-18 D2: round-robin all arc-overlapping peers in this
            // tick, up to the remaining attempt budget.
            let remaining_attempts =
                (policy.max_attempts as usize).saturating_sub(entry.tried_peers.len());
            let mut tried: HashSet<Url> = entry
                .tried_peers
                .iter()
                .filter_map(|s| Url::from_str(s).ok())
                .collect();
            // Location pick: use the first missing parent's loc; close
            // enough for v1, v1.5 may want per-parent picking.
            let parent_id = bytes_to_opid(&entry.missing_parents[0]);
            let loc = parent_id.loc();

            let mut requested_any = false;
            let mut last_err: Option<String> = None;
            for _ in 0..remaining_attempts {
                let alt = self
                    .peer_picker
                    .pick_arc_overlap_peer(loc, tried.clone())
                    .await?;
                let Some(alt) = alt else { break };

                let missing_ops: Vec<OpId> = entry
                    .missing_parents
                    .iter()
                    .map(|b| bytes_to_opid(b))
                    .collect();
                match self.fetcher.request_ops(missing_ops, alt.clone()).await {
                    Ok(()) => {
                        // K2 accepted the request — we've now "tried"
                        // this peer; whether the op actually arrives is
                        // surfaced on the next process_incoming_ops call.
                        tried.insert(alt.clone());
                        entry.tried_peers.push(alt.as_str().to_string());
                        requested_any = true;
                        // First successful request_ops in this tick is
                        // enough — give K2 a chance to actually fetch
                        // before we burn more attempts.
                        break;
                    }
                    Err(e) => {
                        // request_ops itself failed (peer unreachable etc.).
                        // Record the peer as tried so we don't pick it
                        // again, log the error, move to the next peer.
                        last_err = Some(format!("{e}"));
                        tried.insert(alt.clone());
                        entry.tried_peers.push(alt.as_str().to_string());
                        tracing::warn!(
                            "fetcher.request_ops failed against {}: {}",
                            alt.as_str(),
                            e
                        );
                    }
                }
            }

            if !requested_any && (entry.tried_peers.len() as u8) >= policy.max_attempts {
                // We exhausted attempts without K2 ever accepting a
                // request — drop with permanent failure.
                let reason = last_err.unwrap_or_else(|| {
                    "no arc-overlap peer available within max_attempts".to_string()
                });
                self.drop_pending_permanent_failure(&k, &entry, &reason)?;
                continue;
            }

            // Persist updated entry (tried_peers grew). Leave
            // first_seen alone — retry_budget is measured from
            // original ingest, not last-attempt time.
            let mut buf = Vec::new();
            ciborium::into_writer(&entry, &mut buf)
                .map_err(|e| K2Error::other_src("encode pending", e))?;
            self.pending
                .insert(&k, buf)
                .map_err(|e| K2Error::other_src("pending.insert", e))?;
        }

        Ok(())
    }

    /// Drop the pending entry and notify upstream that we've given up
    /// on this op. Used by D5's retry-budget enforcement.
    fn drop_pending_permanent_failure(
        &self,
        key: &sled::IVec,
        entry: &PendingEntry,
        reason: &str,
    ) -> K2Result<()> {
        let op_id = bytes_to_opid(key);
        let missing_parents: Vec<OpId> = entry
            .missing_parents
            .iter()
            .map(|b| bytes_to_opid(b))
            .collect();
        tracing::warn!(
            "dropping pending op {:?}: {} (tried {} peers across {} micros)",
            op_id,
            reason,
            entry.tried_peers.len(),
            now_micros() - entry.first_seen_micros
        );
        self.notify.notify_parent_fetch_permanent_failure(
            op_id,
            missing_parents,
            reason.to_string(),
        );
        self.pending
            .remove(key)
            .map_err(|e| K2Error::other_src("pending.remove (permanent failure)", e))?;
        Ok(())
    }
}

impl Drop for HolographIntegrationQueue {
    fn drop(&mut self) {
        if let Ok(mut slot) = self.watcher_handle.lock() {
            if let Some(h) = slot.take() {
                h.abort();
            }
        }
    }
}

#[cfg(test)]
mod tests;
