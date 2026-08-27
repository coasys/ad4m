//! Step 3 unit tests for `HolographIntegrationQueue`.
//!
//! All tests are driven against mock implementations of the queue's
//! trait surface (`OpFetcher`, `PeerPicker`, `NotifyUp`, `SigVerifier`)
//! and the sled-backed `KvOpStore` underneath. No real K2 transport is
//! involved — Step 4 wires the real modules in.

use std::collections::HashSet;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{K2Error, K2Result, OpId, SpaceId, Timestamp, Url};

use super::*;
use crate::config::ArcPolicy;
use crate::envelope::OpEnvelope;
use crate::op_store::{EnvelopeDecoder, KvOpStore};

// ---------------- Mocks ----------------

#[derive(Debug, Default)]
struct MockNotifier {
    received: StdMutex<Vec<(OpId, Timestamp, Bytes)>>,
}

impl NotifyUp for MockNotifier {
    fn emit_perspective_diff(&self, op_id: OpId, created_at: Timestamp, envelope_bytes: Bytes) {
        self.received
            .lock()
            .unwrap()
            .push((op_id, created_at, envelope_bytes));
    }
}

impl MockNotifier {
    fn emitted_ids(&self) -> Vec<OpId> {
        self.received
            .lock()
            .unwrap()
            .iter()
            .map(|(id, _, _)| id.clone())
            .collect()
    }
}

#[derive(Debug, Default)]
struct MockFetcher {
    requests: StdMutex<Vec<(Vec<OpId>, Url)>>,
}

impl OpFetcher for MockFetcher {
    fn request_ops(&self, op_ids: Vec<OpId>, source: Url) -> BoxFuture<'_, K2Result<()>> {
        let mut log = self.requests.lock().unwrap();
        log.push((op_ids, source));
        Box::pin(async move { Ok(()) })
    }
}

impl MockFetcher {
    fn request_count(&self) -> usize {
        self.requests.lock().unwrap().len()
    }

    fn last_source(&self) -> Option<Url> {
        self.requests.lock().unwrap().last().map(|(_, u)| u.clone())
    }

    fn sources(&self) -> Vec<Url> {
        self.requests
            .lock()
            .unwrap()
            .iter()
            .map(|(_, u)| u.clone())
            .collect()
    }
}

#[derive(Debug)]
struct MockPeerPicker {
    /// Pop from the front for each pick. Empty queue → returns None.
    queue: StdMutex<std::collections::VecDeque<Option<Url>>>,
}

impl MockPeerPicker {
    fn new(picks: Vec<Option<Url>>) -> Self {
        Self {
            queue: StdMutex::new(picks.into()),
        }
    }
}

impl PeerPicker for MockPeerPicker {
    fn pick_arc_overlap_peer(
        &self,
        _loc: u32,
        _tried: HashSet<Url>,
    ) -> BoxFuture<'_, K2Result<Option<Url>>> {
        let next = self.queue.lock().unwrap().pop_front().flatten();
        Box::pin(async move { Ok(next) })
    }
}

#[derive(Debug)]
struct RejectVerifier;

impl SigVerifier for RejectVerifier {
    fn verify(&self, _: &OpEnvelope) -> bool {
        false
    }
}

// ---------------- Envelope helpers ----------------

/// Deterministic op-id derivation: sha-256 of payload + 4 tag bytes.
/// Matches the production decoder in `retriever_kitsune.rs`.
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
    let decoder = envelope_decoder();
    let (op_id, _) = decoder(&bytes).expect("decoder");
    (bytes, op_id)
}

fn url(s: &str) -> Url {
    Url::from_str(s).expect("valid url")
}

// K2's `Url::from_str` requires canonical form (explicit port). Stick
// to these well-formed test URLs everywhere.
const ALICE: &str = "ws://alice.example:80";
const BOB: &str = "ws://bob.example:80";
const CHARLIE: &str = "ws://charlie.example:80";

// ---------------- Harness ----------------

struct Harness {
    queue: Arc<HolographIntegrationQueue>,
    op_store: Arc<KvOpStore>,
    notify: Arc<MockNotifier>,
    fetcher: Arc<MockFetcher>,
    _dir: tempfile::TempDir,
}

struct HarnessOpts {
    sig_verifier: Arc<dyn SigVerifier>,
    peer_picker: Arc<dyn PeerPicker>,
    fallback_timeout: Duration,
    watcher_tick: Duration,
    max_retry_peers: usize,
}

impl Default for HarnessOpts {
    fn default() -> Self {
        Self {
            sig_verifier: Arc::new(AlwaysValid),
            peer_picker: Arc::new(MockPeerPicker::new(vec![])),
            fallback_timeout: Duration::from_secs(15),
            watcher_tick: Duration::from_millis(100),
            max_retry_peers: 3,
        }
    }
}

fn harness_with(opts: HarnessOpts) -> Harness {
    let dir = tempfile::tempdir().unwrap();
    // Use the currently-running tokio runtime (the one #[tokio::test]
    // provided). Step 4's HolographSpace will pass a dedicated handle;
    // for unit-test scope sharing the test runtime keeps drop semantics
    // simple — dropping the queue inside the async context doesn't try
    // to tear down a runtime.
    let handle = tokio::runtime::Handle::current();
    let op_store = KvOpStore::open(
        dir.path().join("ops"),
        SpaceId::from(Bytes::from_static(b"queue-test")),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .unwrap();
    let pending_db = sled::open(dir.path().join("pending")).unwrap();
    let pending = pending_db.open_tree(b"pending").unwrap();
    let notify = Arc::new(MockNotifier::default());
    let fetcher = Arc::new(MockFetcher::default());
    let queue = HolographIntegrationQueue::new(IntegrationQueueConfig {
        op_store: Arc::clone(&op_store),
        pending,
        decode_envelope: envelope_decoder(),
        arc_policy: ArcPolicy::Full,
        notify: Arc::clone(&notify) as Arc<dyn NotifyUp>,
        fetcher: Arc::clone(&fetcher) as Arc<dyn OpFetcher>,
        peer_picker: opts.peer_picker,
        sig_verifier: opts.sig_verifier,
        fallback_timeout: opts.fallback_timeout,
        max_retry_peers: opts.max_retry_peers,
        watcher_tick: opts.watcher_tick,
        runtime: handle,
    });
    Harness {
        queue,
        op_store,
        notify,
        fetcher,
        _dir: dir,
    }
}

fn harness() -> Harness {
    harness_with(HarnessOpts::default())
}

// ---------------- Tests ----------------

/// An op with no parents lands straight in the op-store, emits a notify,
/// and never touches the pending tree or the fetcher.
#[tokio::test]
async fn happy_path_root_op() {
    let h = harness();
    let (bytes, op_id) = make_envelope(b"root-payload", vec![]);

    let accepted = h
        .queue
        .process_incoming_ops(vec![bytes.clone()], Some(url(ALICE)))
        .await
        .expect("process");
    assert_eq!(accepted, vec![op_id.clone()]);

    assert_eq!(h.op_store.op_count_blocking(), 1);
    assert_eq!(h.queue.pending_len(), 0);
    assert_eq!(h.fetcher.request_count(), 0);
    assert_eq!(h.notify.emitted_ids(), vec![op_id]);
}

/// An op whose only parent is missing gets pended; the fetcher is
/// called with the parent op-id and the source url. When the parent
/// arrives via a later process_incoming_ops, the child is cascade-promoted
/// (store + notify) and the pending tree drains.
#[tokio::test]
async fn one_missing_parent_pends_then_promotes() {
    let h = harness();
    let alice = url(ALICE);

    // Build root and child envelopes.
    let (root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, child_id) = make_envelope(b"child", vec![root_id.clone()]);

    // Bob sees the child first (out of order). Parent is missing.
    let accepted = h
        .queue
        .process_incoming_ops(vec![child_bytes.clone()], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(accepted, vec![child_id.clone()]);
    assert_eq!(h.op_store.op_count_blocking(), 0, "child not stored yet");
    assert_eq!(h.queue.pending_len(), 1, "child pending");
    assert_eq!(h.fetcher.request_count(), 1, "fetched the missing parent");
    assert_eq!(h.fetcher.last_source().unwrap(), alice);
    assert!(h.notify.emitted_ids().is_empty());

    // Now the parent shows up.
    let accepted = h
        .queue
        .process_incoming_ops(vec![root_bytes], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(accepted, vec![root_id.clone()]);

    // Both stored, both notified, pending empty.
    assert_eq!(h.op_store.op_count_blocking(), 2);
    assert_eq!(h.queue.pending_len(), 0);
    let emitted = h.notify.emitted_ids();
    assert_eq!(emitted.len(), 2);
    assert_eq!(emitted[0], root_id);
    assert_eq!(emitted[1], child_id);
}

/// Depth-3 missing chain: c -> b -> a -> root.
/// Bob receives c, b, a in arbitrary order. When root arrives, all four
/// cascade-promote in topo order.
#[tokio::test]
async fn depth_three_missing_chain_cascades() {
    let h = harness();
    let alice = url(ALICE);

    let (root_bytes, root_id) = make_envelope(b"r", vec![]);
    let (a_bytes, a_id) = make_envelope(b"a", vec![root_id.clone()]);
    let (b_bytes, b_id) = make_envelope(b"b", vec![a_id.clone()]);
    let (c_bytes, c_id) = make_envelope(b"c", vec![b_id.clone()]);

    // Pend c, b, a (in that order — every one waits on the previous).
    for bytes in [c_bytes, b_bytes, a_bytes] {
        h.queue
            .process_incoming_ops(vec![bytes], Some(alice.clone()))
            .await
            .unwrap();
    }
    assert_eq!(h.op_store.op_count_blocking(), 0);
    assert_eq!(h.queue.pending_len(), 3);

    // Root arrives → cascade should integrate root, a, b, c.
    h.queue
        .process_incoming_ops(vec![root_bytes], Some(alice.clone()))
        .await
        .unwrap();

    assert_eq!(h.op_store.op_count_blocking(), 4, "all four stored");
    assert_eq!(h.queue.pending_len(), 0, "pending drained");
    let emitted = h.notify.emitted_ids();
    assert_eq!(emitted.len(), 4);
    // Topo order: root first, then a, b, c.
    assert_eq!(emitted, vec![root_id, a_id, b_id, c_id]);
}

/// A sig-verify failure drops the op entirely: not stored, not pended,
/// no fetch fired.
#[tokio::test]
async fn signature_failure_is_dropped() {
    let h = harness_with(HarnessOpts {
        sig_verifier: Arc::new(RejectVerifier),
        ..HarnessOpts::default()
    });
    let (bytes, _) = make_envelope(b"rejected", vec![]);
    let accepted = h
        .queue
        .process_incoming_ops(vec![bytes], Some(url(CHARLIE)))
        .await
        .unwrap();
    assert!(accepted.is_empty());
    assert_eq!(h.op_store.op_count_blocking(), 0);
    assert_eq!(h.queue.pending_len(), 0);
    assert_eq!(h.fetcher.request_count(), 0);
    assert!(h.notify.emitted_ids().is_empty());
}

/// The fallback watcher re-requests missing parents from an alternative
/// peer when the original source hasn't delivered within the timeout.
#[tokio::test]
async fn fallback_pass_re_requests_via_alt_peer() {
    let alice = url(ALICE);
    let bob = url(BOB);
    let h = harness_with(HarnessOpts {
        peer_picker: Arc::new(MockPeerPicker::new(vec![Some(bob.clone())])),
        fallback_timeout: Duration::from_millis(0),
        ..HarnessOpts::default()
    });

    // Pend an op with a missing parent, sourced from alice.
    let (_root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, _child_id) = make_envelope(b"child", vec![root_id]);
    h.queue
        .process_incoming_ops(vec![child_bytes], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(h.fetcher.request_count(), 1);
    assert_eq!(h.fetcher.last_source().unwrap(), alice);

    // Now run the fallback pass. Timeout is 0 so the entry is eligible
    // immediately. The picker hands out bob; we should see a re-request
    // against bob.
    h.queue.fallback_pass().await.expect("fallback");

    assert_eq!(h.fetcher.request_count(), 2);
    let sources = h.fetcher.sources();
    assert_eq!(sources[0], alice);
    assert_eq!(sources[1], bob);
}

/// The fallback watcher stops re-requesting once `max_retry_peers` has
/// been exhausted.
#[tokio::test]
async fn fallback_bounded_by_max_retry_peers() {
    let alice = url(ALICE);
    let bob = url(BOB);
    let h = harness_with(HarnessOpts {
        peer_picker: Arc::new(MockPeerPicker::new(vec![Some(bob.clone())])),
        fallback_timeout: Duration::from_millis(0),
        max_retry_peers: 2,
        ..HarnessOpts::default()
    });
    let (_root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, _child_id) = make_envelope(b"child", vec![root_id]);
    h.queue
        .process_incoming_ops(vec![child_bytes], Some(alice))
        .await
        .unwrap();
    // First fallback pass uses up the bob entry from the picker.
    h.queue.fallback_pass().await.unwrap();
    assert_eq!(h.fetcher.request_count(), 2);
    // Second pass: tried_peers = [alice, bob], == max_retry_peers. Skip.
    h.queue.fallback_pass().await.unwrap();
    assert_eq!(h.fetcher.request_count(), 2);
}

/// Pending entries survive queue restart — load from sled, resume on
/// the next watcher tick. Matches SPIKE §2.5 exit check #5 at the queue
/// layer.
#[tokio::test]
async fn pending_persists_across_restart() {
    let dir = tempfile::tempdir().unwrap();
    let alice = url(ALICE);
    let bob = url(BOB);
    let handle = tokio::runtime::Handle::current();

    let (_root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, child_id) = make_envelope(b"child", vec![root_id]);

    // First queue instance: pend the child.
    {
        let op_store = KvOpStore::open(
            dir.path().join("ops"),
            SpaceId::from(Bytes::from_static(b"restart-test")),
            ArcPolicy::Full,
            envelope_decoder(),
        )
        .unwrap();
        let pending_db = sled::open(dir.path().join("pending")).unwrap();
        let pending = pending_db.open_tree(b"pending").unwrap();
        let queue = HolographIntegrationQueue::new(IntegrationQueueConfig {
            op_store: Arc::clone(&op_store),
            pending,
            decode_envelope: envelope_decoder(),
            arc_policy: ArcPolicy::Full,
            notify: Arc::new(MockNotifier::default()),
            fetcher: Arc::new(MockFetcher::default()),
            peer_picker: Arc::new(MockPeerPicker::new(vec![])),
            sig_verifier: Arc::new(AlwaysValid),
            fallback_timeout: Duration::from_secs(15),
            max_retry_peers: 3,
            watcher_tick: Duration::from_millis(100),
            runtime: handle.clone(),
        });
        queue
            .process_incoming_ops(vec![child_bytes.clone()], Some(alice.clone()))
            .await
            .unwrap();
        assert_eq!(queue.pending_len(), 1);
        // Drop queue, store, db — explicit drop so sled flushes before
        // we reopen. Sled is single-process exclusive-lock; reopening
        // is only safe after the previous handle has fully dropped.
        drop(queue);
        drop(op_store);
        drop(pending_db);
    }

    // Reopen everything. Pending should still hold the child.
    let op_store = KvOpStore::open(
        dir.path().join("ops"),
        SpaceId::from(Bytes::from_static(b"restart-test")),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .unwrap();
    let pending_db = sled::open(dir.path().join("pending")).unwrap();
    let pending = pending_db.open_tree(b"pending").unwrap();
    let picker = MockPeerPicker::new(vec![Some(bob.clone())]);
    let fetcher = Arc::new(MockFetcher::default());
    let queue = HolographIntegrationQueue::new(IntegrationQueueConfig {
        op_store: Arc::clone(&op_store),
        pending,
        decode_envelope: envelope_decoder(),
        arc_policy: ArcPolicy::Full,
        notify: Arc::new(MockNotifier::default()),
        fetcher: Arc::clone(&fetcher) as Arc<dyn OpFetcher>,
        peer_picker: Arc::new(picker),
        sig_verifier: Arc::new(AlwaysValid),
        fallback_timeout: Duration::from_millis(0),
        max_retry_peers: 3,
        watcher_tick: Duration::from_millis(100),
        runtime: handle,
    });

    assert_eq!(queue.pending_len(), 1, "pending survived restart");
    assert_eq!(queue.pending_op_ids()[0], child_id);

    // Run a fallback pass — confirms we can interact with the restored
    // entry through the watcher's path too.
    queue.fallback_pass().await.unwrap();
    assert_eq!(fetcher.request_count(), 1);
    assert_eq!(fetcher.last_source().unwrap(), bob);
}

/// Receiving the same envelope twice while it's pending must not
/// trigger a second fetch or a second pending entry.
#[tokio::test]
async fn duplicate_pending_does_not_double_fetch() {
    let h = harness();
    let alice = url(ALICE);

    let (_root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, _child_id) = make_envelope(b"child", vec![root_id]);

    h.queue
        .process_incoming_ops(vec![child_bytes.clone()], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(h.fetcher.request_count(), 1);
    assert_eq!(h.queue.pending_len(), 1);

    // Replay.
    h.queue
        .process_incoming_ops(vec![child_bytes], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(h.fetcher.request_count(), 1, "no extra fetch");
    assert_eq!(h.queue.pending_len(), 1, "no extra pending entry");
}

/// Receiving the same already-stored op twice must be a no-op (the
/// dedup branch returns `Some(op_id)` without re-storing or re-notifying).
#[tokio::test]
async fn duplicate_stored_op_is_noop() {
    let h = harness();
    let (bytes, op_id) = make_envelope(b"root", vec![]);
    h.queue
        .process_incoming_ops(vec![bytes.clone()], None)
        .await
        .unwrap();
    let before = h.notify.emitted_ids().len();

    let again = h
        .queue
        .process_incoming_ops(vec![bytes], None)
        .await
        .unwrap();
    assert_eq!(again, vec![op_id]);
    assert_eq!(h.op_store.op_count_blocking(), 1);
    assert_eq!(h.notify.emitted_ids().len(), before, "no extra notify");
}

/// Sharding-ready commitment 1: ops outside the configured arc are
/// silently dropped (not stored, not pended, no fetch fired). Default
/// arc is `Full`, so we configure `Empty` here to verify the rejection
/// path.
#[tokio::test]
async fn outside_arc_dropped() {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();
    // Op-store uses Full so it accepts; the queue uses an empty arc to
    // hit the filter.
    let op_store = KvOpStore::open(
        dir.path().join("ops"),
        SpaceId::from(Bytes::from_static(b"arc-test")),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .unwrap();
    let pending_db = sled::open(dir.path().join("pending")).unwrap();
    let pending = pending_db.open_tree(b"pending").unwrap();
    let notify = Arc::new(MockNotifier::default());
    let fetcher = Arc::new(MockFetcher::default());
    let queue = HolographIntegrationQueue::new(IntegrationQueueConfig {
        op_store: Arc::clone(&op_store),
        pending,
        decode_envelope: envelope_decoder(),
        arc_policy: ArcPolicy::Sharded(kitsune2_api::DhtArc::Empty),
        notify: Arc::clone(&notify) as Arc<dyn NotifyUp>,
        fetcher: Arc::clone(&fetcher) as Arc<dyn OpFetcher>,
        peer_picker: Arc::new(MockPeerPicker::new(vec![])),
        sig_verifier: Arc::new(AlwaysValid),
        fallback_timeout: Duration::from_secs(15),
        max_retry_peers: 3,
        watcher_tick: Duration::from_millis(100),
        runtime: handle,
    });

    let (bytes, _) = make_envelope(b"out-of-arc", vec![]);
    let accepted = queue
        .process_incoming_ops(vec![bytes], Some(url(ALICE)))
        .await
        .unwrap();

    assert!(accepted.is_empty());
    assert_eq!(op_store.op_count_blocking(), 0);
    assert_eq!(queue.pending_len(), 0);
    assert_eq!(fetcher.request_count(), 0);
    assert!(notify.emitted_ids().is_empty());
}

/// The watcher's spawn lifecycle is well-behaved: start_watcher spawns,
/// stop_watcher aborts. We also verify start_watcher is idempotent.
#[tokio::test]
async fn watcher_start_stop_lifecycle() {
    let h = harness();
    h.queue.start_watcher();
    h.queue.start_watcher(); // idempotent — no panic, no second spawn.
                             // Let it tick once.
    tokio::time::sleep(Duration::from_millis(50)).await;
    h.queue.stop_watcher();
    // Calling stop again is also fine.
    h.queue.stop_watcher();
}

/// End-to-end: the watcher running on the dedicated runtime actually
/// picks up a stale pending entry and re-requests. This is the
/// load-bearing "watcher works in a real runtime" claim from SPIKE §1.1.
#[tokio::test]
async fn watcher_loop_triggers_fallback() {
    let alice = url(ALICE);
    let bob = url(BOB);
    let h = harness_with(HarnessOpts {
        peer_picker: Arc::new(MockPeerPicker::new(vec![Some(bob.clone())])),
        fallback_timeout: Duration::from_millis(0),
        watcher_tick: Duration::from_millis(20),
        ..HarnessOpts::default()
    });
    let (_root_bytes, root_id) = make_envelope(b"root", vec![]);
    let (child_bytes, _child_id) = make_envelope(b"child", vec![root_id]);
    h.queue
        .process_incoming_ops(vec![child_bytes], Some(alice.clone()))
        .await
        .unwrap();
    assert_eq!(h.fetcher.request_count(), 1);

    h.queue.start_watcher();
    // Give the watcher a couple of ticks.
    tokio::time::sleep(Duration::from_millis(120)).await;
    h.queue.stop_watcher();

    let count = h.fetcher.request_count();
    assert!(
        count >= 2,
        "watcher should have re-requested at least once (got {count})"
    );
    let sources = h.fetcher.sources();
    assert!(
        sources.iter().any(|u| *u == bob),
        "bob should have been re-requested"
    );
}
