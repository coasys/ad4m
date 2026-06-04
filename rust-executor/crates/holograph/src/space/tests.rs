//! Step 4 unit tests for `HolographSpace`, the K2 adapters, and the
//! `K2OpStoreShim` queue-routing wrapper.
//!
//! Adapter behavior against real K2 transport is covered by the
//! integration test in `tests/space_two_node.rs`; unit tests stay at
//! the trait boundary.

use std::collections::HashSet;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{
    K2Error, K2Result, MetaOp, OpId, OpStore, SpaceHandler, SpaceId, StoredOp, Timestamp, Url,
};

use super::*;
use crate::config::{ArcPolicy, SpaceConfig};
use crate::envelope::OpEnvelope;
use crate::integration_queue::{AlwaysValid, NotifyUp, OpFetcher, PeerPicker};
use crate::op_store::{EnvelopeDecoder, KvOpStore};

// ---------------- Test helpers ----------------

const ALICE: &str = "ws://alice.example:80";

fn url(s: &str) -> Url {
    Url::from_str(s).expect("valid url")
}

fn envelope_decoder() -> EnvelopeDecoder {
    use sha2::{Digest, Sha256};
    Arc::new(|bytes: &[u8]| -> Result<(OpId, Timestamp), K2Error> {
        let env = OpEnvelope::decode(bytes).map_err(|e| K2Error::other_src("decode", e))?;
        let mut hasher = Sha256::new();
        hasher.update(env.payload.as_ref());
        let digest = hasher.finalize();
        let mut id_bytes = [0u8; 36];
        id_bytes[..32].copy_from_slice(&digest);
        id_bytes[32..].copy_from_slice(&[0xdb, 0xdb, 0xdb, 0xdb]);
        let op_id = OpId::from(Bytes::copy_from_slice(&id_bytes));
        let ts = Timestamp::from_micros(env.created_at_micros);
        Ok((op_id, ts))
    })
}

fn make_envelope(payload: &[u8], parents: Vec<OpId>) -> (Bytes, OpId) {
    let env = OpEnvelope::new_at(
        parents,
        Bytes::copy_from_slice(payload),
        Bytes::from_static(b"pk"),
        Bytes::from_static(b"sig"),
        None,
        1_700_000_000_000_000,
    );
    let bytes = Bytes::from(env.encode().expect("encode"));
    let (op_id, _) = envelope_decoder()(&bytes).expect("decoder");
    (bytes, op_id)
}

// ---------------- Mock LocalCommitTarget ----------------

#[derive(Debug, Default)]
struct MockCommitTarget {
    informed: StdMutex<Vec<Vec<OpId>>>,
    published: StdMutex<Vec<Vec<OpId>>>,
}

impl LocalCommitTarget for MockCommitTarget {
    fn inform_ops_stored(&self, ops: Vec<StoredOp>) -> BoxFuture<'_, K2Result<()>> {
        let ids: Vec<OpId> = ops.into_iter().map(|o| o.op_id).collect();
        self.informed.lock().unwrap().push(ids);
        Box::pin(async { Ok(()) })
    }

    fn publish_ops_to_peers(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<()>> {
        self.published.lock().unwrap().push(op_ids);
        Box::pin(async { Ok(()) })
    }
}

impl MockCommitTarget {
    fn inform_count(&self) -> usize {
        self.informed.lock().unwrap().len()
    }
    fn publish_count(&self) -> usize {
        self.published.lock().unwrap().len()
    }
    fn last_informed(&self) -> Option<Vec<OpId>> {
        self.informed.lock().unwrap().last().cloned()
    }
    fn last_published(&self) -> Option<Vec<OpId>> {
        self.published.lock().unwrap().last().cloned()
    }
}

// ---------------- Mock OpFetcher / PeerPicker / NotifyUp ----------------

#[derive(Debug, Default)]
struct NoopFetcher;
impl OpFetcher for NoopFetcher {
    fn request_ops(&self, _: Vec<OpId>, _: Url) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async { Ok(()) })
    }
}

#[derive(Debug, Default)]
struct NoopPeerPicker;
impl PeerPicker for NoopPeerPicker {
    fn pick_arc_overlap_peer(
        &self,
        _: u32,
        _: HashSet<Url>,
    ) -> BoxFuture<'_, K2Result<Option<Url>>> {
        Box::pin(async { Ok(None) })
    }
}

#[derive(Debug, Default)]
struct RecordingNotifier {
    received: StdMutex<Vec<(OpId, Timestamp, Bytes)>>,
}

impl NotifyUp for RecordingNotifier {
    fn emit_perspective_diff(&self, op_id: OpId, created_at: Timestamp, envelope_bytes: Bytes) {
        self.received
            .lock()
            .unwrap()
            .push((op_id, created_at, envelope_bytes));
    }
}

impl RecordingNotifier {
    fn count(&self) -> usize {
        self.received.lock().unwrap().len()
    }
    fn last_id(&self) -> Option<OpId> {
        self.received
            .lock()
            .unwrap()
            .last()
            .map(|(id, _, _)| id.clone())
    }
}

// ---------------- Build the space under test ----------------

struct Harness {
    space: Arc<HolographSpace>,
    commit_target: Arc<MockCommitTarget>,
    notify: Arc<RecordingNotifier>,
    op_store: Arc<KvOpStore>,
    _dir: tempfile::TempDir,
}

fn build_space() -> Harness {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();

    let op_store = KvOpStore::open(
        dir.path().join("ops"),
        SpaceId::from(Bytes::from_static(b"space-test")),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .unwrap();
    let pending_db = sled::open(dir.path().join("pending")).unwrap();
    let pending = pending_db.open_tree(b"pending").unwrap();

    let commit_target = Arc::new(MockCommitTarget::default());
    let notify = Arc::new(RecordingNotifier::default());

    let opts = HolographSpaceConfig {
        config: SpaceConfig::full_replication_single_doc(),
        op_store: Arc::clone(&op_store),
        pending,
        decode_envelope: envelope_decoder(),
        fetcher: Arc::new(NoopFetcher),
        peer_picker: Arc::new(NoopPeerPicker),
        notify: Arc::clone(&notify) as Arc<dyn NotifyUp>,
        commit_target: Arc::clone(&commit_target) as Arc<dyn LocalCommitTarget>,
        sig_verifier: Arc::new(AlwaysValid),
        runtime: handle,
        watcher_tick: Duration::from_millis(100),
    };
    let space = HolographSpace::new(opts);

    Harness {
        space,
        commit_target,
        notify,
        op_store,
        _dir: dir,
    }
}

// ---------------- Tests ----------------

/// `on_local_commit` stores the op via the queue, notifies K2 via
/// `inform_ops_stored`, and publishes via `publish_ops_to_peers`. Each
/// call exactly once for a single committed op.
#[tokio::test]
async fn on_local_commit_stores_informs_publishes() {
    let h = build_space();
    let (bytes, op_id) = make_envelope(b"local-commit", vec![]);

    let returned = h
        .space
        .on_local_commit(bytes.clone())
        .await
        .expect("commit");
    assert_eq!(returned, op_id);

    assert_eq!(h.op_store.op_count_blocking(), 1);
    assert_eq!(h.notify.count(), 1);
    assert_eq!(h.notify.last_id().unwrap(), op_id);

    assert_eq!(h.commit_target.inform_count(), 1);
    assert_eq!(
        h.commit_target.last_informed().unwrap(),
        vec![op_id.clone()]
    );
    assert_eq!(h.commit_target.publish_count(), 1);
    assert_eq!(h.commit_target.last_published().unwrap(), vec![op_id]);
}

/// `process_incoming_ops` (from K2) routes through the queue and ends
/// up stored + notified, matching the same path the queue tests cover.
#[tokio::test]
async fn process_incoming_ops_routes_through_queue() {
    let h = build_space();
    let (bytes, op_id) = make_envelope(b"from-peer", vec![]);

    let accepted = h
        .space
        .process_incoming_ops(vec![bytes], Some(url(ALICE)))
        .await
        .expect("process");

    assert_eq!(accepted, vec![op_id.clone()]);
    assert_eq!(h.op_store.op_count_blocking(), 1);
    assert_eq!(h.notify.last_id().unwrap(), op_id);

    // Crucially, K2 was NOT told to publish — incoming ops don't get
    // re-published, they get gossiped by K2 directly.
    assert_eq!(h.commit_target.publish_count(), 0);
    assert_eq!(h.commit_target.inform_count(), 0);
}

/// `ChannelNotifier` delivers an `EmittedOp` to its receiver.
#[tokio::test]
async fn channel_notifier_delivers_emitted_op() {
    let (notifier, mut rx) = ChannelNotifier::new();
    let op_id = OpId::from(Bytes::from_static(b"abc"));
    let ts = Timestamp::from_micros(42);
    let bytes = Bytes::from_static(b"env");

    notifier.emit_perspective_diff(op_id.clone(), ts, bytes.clone());

    let item = rx.recv().await.expect("recv");
    assert_eq!(item.op_id, op_id);
    assert_eq!(item.created_at, ts);
    assert_eq!(item.envelope_bytes, bytes);
}

/// `HolographSpaceHandler::recv_notify` forwards a `TelepresenceNotification`
/// to its receiver. The K2 contract returns `Ok(())` even when the sink
/// receiver is gone — the connection shouldn't be closed because of a
/// local telepresence-sink failure.
#[tokio::test]
async fn space_handler_recv_notify_forwards() {
    let (handler, mut rx) = HolographSpaceHandler::new();
    let from = url(ALICE);
    let data = Bytes::from_static(b"telepresence-payload");

    let result = handler.recv_notify(
        from.clone(),
        SpaceId::from(Bytes::from_static(b"sp")),
        data.clone(),
    );
    assert!(result.is_ok());

    let msg = rx.recv().await.expect("recv");
    assert_eq!(msg.from_peer, from);
    assert_eq!(msg.data, data);

    // Dropping rx and emitting again is also Ok.
    drop(rx);
    let result = handler.recv_notify(
        from,
        SpaceId::from(Bytes::from_static(b"sp")),
        Bytes::from_static(b"dropped"),
    );
    assert!(result.is_ok());
}

/// `K2OpStoreShim` routes incoming ops through the queue when one
/// is installed. Prior to install, it falls back to direct `KvOpStore`
/// storage.
#[tokio::test]
async fn holograph_op_store_routes_through_queue_when_installed() {
    let h = build_space();

    // The wrapper around the same store + queue. Note: this isn't the
    // same wrapper the `HolographSpace` uses internally; the space goes
    // through queue directly. We're testing the OpStore-trait wrapper
    // here so K2's fetch path (which only sees a DynOpStore) gets
    // queue-routed behavior.
    let wrapper = K2OpStoreShim::new(Arc::clone(&h.op_store));
    wrapper.install_queue(Arc::clone(h.space.queue()));

    let (bytes, op_id) = make_envelope(b"via-wrapper", vec![]);
    let stored = wrapper
        .process_incoming_ops(vec![bytes])
        .await
        .expect("process");
    assert_eq!(stored, vec![op_id.clone()]);
    assert_eq!(h.notify.last_id().unwrap(), op_id);
}

/// Without an installed queue, `K2OpStoreShim::process_incoming_ops`
/// goes straight to the store — used during the brief construction
/// window. No notify fires because the queue is what calls notify-up.
#[tokio::test]
async fn holograph_op_store_falls_through_to_store_pre_install() {
    let h = build_space();
    let wrapper = K2OpStoreShim::new(Arc::clone(&h.op_store));

    let (bytes, op_id) = make_envelope(b"pre-install", vec![]);
    let stored = wrapper
        .process_incoming_ops(vec![bytes])
        .await
        .expect("process");

    // Op-id stored by the underlying KvOpStore; no notify since the
    // queue's notify path was bypassed.
    assert_eq!(stored, vec![op_id]);
    assert_eq!(h.op_store.op_count_blocking(), 1);
    assert_eq!(h.notify.count(), 0);
}

/// `K2OpStoreShim`'s passthrough delegates `retrieve_ops` to the
/// underlying `KvOpStore` so that K2's fetch-response path returns the
/// real op-data bytes a peer asked for.
#[tokio::test]
async fn holograph_op_store_passthrough_retrieve_ops() {
    let h = build_space();
    let wrapper = K2OpStoreShim::new(Arc::clone(&h.op_store));

    let (bytes, op_id) = make_envelope(b"served", vec![]);
    // Store via the wrapper (queue not installed → direct).
    wrapper
        .process_incoming_ops(vec![bytes.clone()])
        .await
        .unwrap();

    let served = wrapper.retrieve_ops(vec![op_id.clone()]).await.unwrap();
    assert_eq!(served.len(), 1);
    assert_eq!(served[0].op_id, op_id);
    assert_eq!(served[0].op_data, bytes);
}

/// `K2DynSpaceTarget` exists primarily as a thin shim. Its unit
/// coverage is in the two-node integration test (which exercises
/// inform_ops_stored + publish_ops_to_peers through real K2). At the
/// unit-test layer we just verify the type implements
/// `LocalCommitTarget` so the trait object construction at the
/// `HolographSpace` builder is sound — this assert is a compile-time
/// dyn-bound check, not a runtime test.
#[test]
fn k2_dyn_space_target_is_local_commit_target() {
    fn assert_lct<T: LocalCommitTarget>() {}
    assert_lct::<K2DynSpaceTarget>();
}

/// Same compile-time bound check for the adapter newtypes: they
/// implement the queue's trait surface so a `HolographSpace` built
/// against them is well-typed.
#[test]
fn k2_adapters_implement_queue_traits() {
    fn assert_fetcher<T: OpFetcher>() {}
    fn assert_picker<T: PeerPicker>() {}
    assert_fetcher::<K2FetcherAdapter>();
    assert_picker::<K2PeerPickerAdapter>();
}

/// `K2OpStoreShim`'s other passthrough methods (slice-hash store/
/// retrieve, filter_out_existing, query_total_op_count) round-trip via
/// the underlying `KvOpStore`. Covers the full delegation surface so a
/// future refactor doesn't accidentally drop a method.
#[tokio::test]
async fn holograph_op_store_full_passthrough_surface() {
    let h = build_space();
    let wrapper = K2OpStoreShim::new(Arc::clone(&h.op_store));

    // filter_out_existing
    let some_id = OpId::from(Bytes::from_static(b"never-stored-______________other"));
    let missing = wrapper
        .filter_out_existing_ops(vec![some_id.clone()])
        .await
        .unwrap();
    assert_eq!(missing, vec![some_id]);

    // slice_hash round-trip via the wrapper.
    wrapper
        .store_slice_hash(kitsune2_api::DhtArc::FULL, 0, Bytes::from_static(b"hash0"))
        .await
        .unwrap();
    let h0 = wrapper
        .retrieve_slice_hash(kitsune2_api::DhtArc::FULL, 0)
        .await
        .unwrap();
    assert_eq!(h0.as_deref(), Some(&b"hash0"[..]));

    let count = wrapper
        .slice_hash_count(kitsune2_api::DhtArc::FULL)
        .await
        .unwrap();
    assert_eq!(count, 1);

    let total = wrapper.query_total_op_count().await.unwrap();
    assert_eq!(total, 0);
}

// Touch the imports used only by tests to keep clippy quiet about
// "unused" in non-test builds.
#[test]
fn _unused_import_guard() {
    let _: Option<MetaOp> = None;
}
