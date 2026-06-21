//! Step 4d end-to-end: two `HolographSpace`s wired to real K2 modules
//! (mem transport + mem peer store + core fetch/publish) propagate an
//! op across the in-process "network".
//!
//! Alice commits an envelope; Bob's `ChannelNotifier` receives an
//! `EmittedOp` for it within a generous timeout.  Then Alice commits a
//! second envelope listing the first as a parent; Bob's queue confirms
//! the parent is present and cascade-promotes the child.
//!
//! Peer discovery is manual (cross-registering agent infos via
//! `peer_store().insert`), matching the pattern K2's own
//! `core_space::test` uses for two-node tests. Mem bootstrap is not in
//! the picture here — we just want to exercise our publish/fetch
//! round-trip through the K2 transport, not test K2's discovery layer.
//!
//! No real K2 fork — uses `kitsune2_core::default_test_builder`
//! (MemTransport, MemPeerStore, CoreFetch, CorePublish, CoreGossipStub,
//! etc.) with our `K2OpStoreShim` substituted into the op-store slot.

use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{
    BoxFut, Builder, Config, DhtArc, DynKitsuneHandler, DynLocalAgent, DynOpStore, DynSpaceHandler,
    K2Error, K2Result, KitsuneHandler, OpStoreFactory, SpaceId, Timestamp, Url,
};
use kitsune2_core::default_test_builder;
use kitsune2_test_utils::agent::{AgentBuilder, TestLocalAgent, TestVerifier};

use holograph::{
    ArcPolicy, ChannelNotifier, EmittedOp, EnvelopeDecoder, HolographSpace, HolographSpaceConfig,
    HolographSpaceHandler, K2DynSpaceTarget, K2FetcherAdapter, K2OpStoreShim, K2PeerPickerAdapter,
    KvOpStore, NotifyUp, OpEnvelope, SpaceConfig,
};

// -------- Test shared infra --------

/// SHA-256 over payload, tag with 0xdb*4, matches the production
/// decoder in `retriever_kitsune`.
fn envelope_decoder() -> EnvelopeDecoder {
    use sha2::{Digest, Sha256};
    Arc::new(
        |bytes: &[u8]| -> Result<(kitsune2_api::OpId, Timestamp), K2Error> {
            let env = OpEnvelope::decode(bytes).map_err(|e| K2Error::other_src("decode", e))?;
            let mut hasher = Sha256::new();
            hasher.update(env.payload.as_ref());
            let digest = hasher.finalize();
            let mut id_bytes = [0u8; 36];
            id_bytes[..32].copy_from_slice(&digest);
            id_bytes[32..].copy_from_slice(&[0xdb, 0xdb, 0xdb, 0xdb]);
            let op_id = kitsune2_api::OpId::from(Bytes::copy_from_slice(&id_bytes));
            let ts = Timestamp::from_micros(env.created_at_micros);
            Ok((op_id, ts))
        },
    )
}

fn make_envelope(payload: &[u8], parents: Vec<kitsune2_api::OpId>) -> (Bytes, kitsune2_api::OpId) {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_micros() as i64;
    let env = OpEnvelope::new_at(
        parents,
        Bytes::copy_from_slice(payload),
        Bytes::from_static(b"pk"),
        Bytes::from_static(b"sig"),
        None,
        now,
    );
    let bytes = Bytes::from(env.encode().expect("encode"));
    let (op_id, _) = envelope_decoder()(&bytes).expect("decoder");
    (bytes, op_id)
}

fn test_space_id() -> SpaceId {
    SpaceId::from(Bytes::from_static(b"holograph-test-space"))
}

// -------- OpStoreFactory that hands the test an `Arc<K2OpStoreShim>` --------

/// K2's `OpStoreFactory::create` is called during space build — we need
/// to return our `K2OpStoreShim` (so K2 routes inbound ops through it),
/// but also capture an `Arc<K2OpStoreShim>` the test can reach into to
/// install the queue post-construction.
#[derive(Debug)]
struct ShimFactory {
    op_store: Arc<KvOpStore>,
    shim_slot: Arc<StdMutex<Option<Arc<K2OpStoreShim>>>>,
}

impl OpStoreFactory for ShimFactory {
    fn default_config(&self, _config: &mut Config) -> K2Result<()> {
        Ok(())
    }
    fn validate_config(&self, _config: &Config) -> K2Result<()> {
        Ok(())
    }
    fn create(
        &self,
        _builder: Arc<Builder>,
        _space_id: SpaceId,
    ) -> BoxFuture<'static, K2Result<DynOpStore>> {
        let op_store = Arc::clone(&self.op_store);
        let slot = Arc::clone(&self.shim_slot);
        Box::pin(async move {
            let shim = K2OpStoreShim::new(op_store);
            *slot.lock().unwrap() = Some(Arc::clone(&shim));
            let dyn_store: DynOpStore = shim;
            Ok(dyn_store)
        })
    }
}

/// Minimal KitsuneHandler that:
/// - Captures `new_listening_address(this_url)` so the test can learn
///   our K2 URL (needed to build cross-side agent infos).
/// - Hands K2 a `HolographSpaceHandler` on `create_space`.
#[derive(Debug)]
struct Handler {
    space_handler: Arc<HolographSpaceHandler>,
    url_tx: tokio::sync::mpsc::UnboundedSender<Url>,
}

impl KitsuneHandler for Handler {
    fn new_listening_address(&self, this_url: Url) -> BoxFut<'static, ()> {
        let _ = self.url_tx.send(this_url);
        Box::pin(async move {})
    }

    fn create_space(
        &self,
        _space_id: SpaceId,
        _config_override: Option<&Config>,
    ) -> futures::future::BoxFuture<'_, K2Result<DynSpaceHandler>> {
        let s = Arc::clone(&self.space_handler);
        Box::pin(async move {
            let s: DynSpaceHandler = s;
            Ok(s)
        })
    }
}

// -------- The actual node setup --------

struct Node {
    name: &'static str,
    space: Arc<HolographSpace>,
    emitted_rx: tokio::sync::mpsc::UnboundedReceiver<EmittedOp>,
    url: Url,
    agent: DynLocalAgent,
    _kitsune: kitsune2_api::DynKitsune,
    dyn_space: kitsune2_api::DynSpace,
    _dir: tempfile::TempDir,
}

async fn build_node(name: &'static str) -> Node {
    let dir = tempfile::tempdir().unwrap();

    let op_store = KvOpStore::open(
        dir.path().join("ops"),
        test_space_id(),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .unwrap();

    let pending_db = sled::open(dir.path().join("pending")).unwrap();
    let pending = pending_db.open_tree(b"pending").unwrap();

    let shim_slot: Arc<StdMutex<Option<Arc<K2OpStoreShim>>>> = Arc::new(StdMutex::new(None));

    let (handler, _telepresence_rx) = HolographSpaceHandler::new();
    let (url_tx, mut url_rx) = tokio::sync::mpsc::unbounded_channel::<Url>();

    let kitsune = Builder {
        verifier: Arc::new(TestVerifier),
        op_store: Arc::new(ShimFactory {
            op_store: Arc::clone(&op_store),
            shim_slot: Arc::clone(&shim_slot),
        }),
        ..default_test_builder()
    }
    .with_default_config()
    .unwrap()
    .build()
    .await
    .unwrap();

    let kitsune_handler: DynKitsuneHandler = Arc::new(Handler {
        space_handler: Arc::clone(&handler),
        url_tx,
    });
    kitsune.register_handler(kitsune_handler).await.unwrap();

    let dyn_space = kitsune.space(test_space_id(), None).await.unwrap();

    // K2 emits new_listening_address shortly after transport is bound;
    // that's how we learn our URL.
    let url = tokio::time::timeout(Duration::from_secs(5), url_rx.recv())
        .await
        .expect("timed out waiting for local URL")
        .expect("url channel closed");

    let shim = shim_slot
        .lock()
        .unwrap()
        .clone()
        .expect("ShimFactory should have populated the slot");

    let fetcher = K2FetcherAdapter::new(dyn_space.fetch().clone());
    let peer_picker = K2PeerPickerAdapter::new(dyn_space.peer_store().clone());
    let (notifier, emitted_rx) = ChannelNotifier::new();
    let commit_target = K2DynSpaceTarget::new(dyn_space.clone());

    let space = HolographSpace::new(HolographSpaceConfig::defaults(
        SpaceConfig::full_replication_single_doc(),
        Arc::clone(&op_store),
        pending,
        envelope_decoder(),
        fetcher,
        peer_picker,
        notifier as Arc<dyn NotifyUp>,
        commit_target,
        tokio::runtime::Handle::current(),
    ));

    shim.install_queue(Arc::clone(space.queue()));

    let agent = Arc::new(TestLocalAgent::default()) as DynLocalAgent;
    agent.set_cur_storage_arc(DhtArc::FULL);
    agent.set_tgt_storage_arc_hint(DhtArc::FULL);
    dyn_space.local_agent_join(agent.clone()).await.unwrap();

    tracing::info!(node = name, %url, "node built and joined");

    Node {
        name,
        space,
        emitted_rx,
        url,
        agent,
        _kitsune: kitsune,
        dyn_space,
        _dir: dir,
    }
}

/// Cross-register: insert `other`'s agent info (with `other`'s URL and a
/// FULL storage arc) into `self_node`'s peer_store, so this side knows
/// where to reach the other side via K2 publish/fetch.
async fn cross_register(self_node: &Node, other: &Node) {
    let info = AgentBuilder {
        url: Some(Some(other.url.clone())),
        storage_arc: Some(DhtArc::FULL),
        space_id: Some(test_space_id()),
        ..Default::default()
    }
    .build(other.agent.clone());
    self_node
        .dyn_space
        .peer_store()
        .insert(vec![info])
        .await
        .unwrap();
}

async fn wait_for_emit(
    node: &mut Node,
    expect: &kitsune2_api::OpId,
    timeout: Duration,
) -> Result<EmittedOp, String> {
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
        if remaining.is_zero() {
            return Err(format!(
                "{}: did not receive emit for op-id within {:?}",
                node.name, timeout
            ));
        }
        match tokio::time::timeout(remaining, node.emitted_rx.recv()).await {
            Ok(Some(emit)) => {
                if &emit.op_id == expect {
                    return Ok(emit);
                }
                tracing::debug!(node = node.name, "unrelated emit, continuing");
            }
            Ok(None) => return Err(format!("{}: notifier channel closed", node.name)),
            Err(_) => return Err(format!("{}: timeout waiting for op-id emit", node.name)),
        }
    }
}

/// Two `HolographSpace`s on the same in-process K2 mem-transport network.
/// Alice commits → Bob's notifier receives. Then Alice commits a child
/// whose parent is the first op → Bob receives and cascade-promotes.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn two_node_commit_propagates_via_real_k2() {
    let _ = tracing_subscriber::fmt::try_init();

    let mut alice = build_node("alice").await;
    let mut bob = build_node("bob").await;

    // Cross-register peer infos so publish_ops on either side can find
    // the other peer's URL.
    cross_register(&alice, &bob).await;
    cross_register(&bob, &alice).await;

    // Give K2 a beat to register the peer infos and set up direct
    // connections via mem transport.
    tokio::time::sleep(Duration::from_millis(200)).await;

    // -------- Commit 1: root envelope --------
    let (root_bytes, root_id) = make_envelope(b"alice-root", vec![]);
    let returned = alice
        .space
        .on_local_commit(root_bytes.clone())
        .await
        .expect("alice commit root");
    assert_eq!(returned, root_id);

    // Alice's own notifier emits straight away because on_local_commit
    // routes through her queue locally.
    let alice_emit = wait_for_emit(&mut alice, &root_id, Duration::from_secs(5))
        .await
        .expect("alice should self-emit the local commit");
    assert_eq!(alice_emit.envelope_bytes, root_bytes);

    // Bob should receive the root via K2 publish_ops + fetch round-trip.
    let bob_emit = wait_for_emit(&mut bob, &root_id, Duration::from_secs(30))
        .await
        .expect("bob should receive alice's root envelope within 30s");
    assert_eq!(bob_emit.op_id, root_id);
    assert_eq!(bob_emit.envelope_bytes, root_bytes);

    // -------- Commit 2: child envelope with parent = root --------
    let (child_bytes, child_id) = make_envelope(b"alice-child", vec![root_id.clone()]);
    alice
        .space
        .on_local_commit(child_bytes.clone())
        .await
        .expect("alice commit child");

    let _alice_child_emit = wait_for_emit(&mut alice, &child_id, Duration::from_secs(5))
        .await
        .expect("alice should self-emit the child");

    // Bob's queue:
    //   1. Receives the child via publish_ops + fetch.
    //   2. Sees parent_id == root_id is already in its op-store.
    //   3. Takes the all-parents-present branch → store + emit.
    let bob_child_emit = wait_for_emit(&mut bob, &child_id, Duration::from_secs(30))
        .await
        .expect("bob should receive alice's child envelope within 30s");
    assert_eq!(bob_child_emit.envelope_bytes, child_bytes);

    assert_eq!(bob.space.op_count(), 2);
    assert_eq!(alice.space.op_count(), 2);
}
