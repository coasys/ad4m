//! Step 4d end-to-end: two `HolographSpace`s wired to real K2 modules
//! (mem bootstrap + mem transport + core gossip stub) gossip an op
//! across the in-process "network".
//!
//! Alice commits an envelope; Bob's `ChannelNotifier` receives an
//! `EmittedOp` for it within a generous timeout.  Then Alice commits a
//! second envelope listing the first as a parent; Bob's queue confirms
//! the parent is present and cascade-promotes the child.
//!
//! No real K2 fork — the test uses `kitsune2_core::default_test_builder`
//! (MemTransport, MemBootstrap, MemPeerStore, CoreGossipStub, etc.)
//! with our `K2OpStoreShim` substituted into the op-store slot.

use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{
    Builder, Config, DhtArc, DynLocalAgent, DynOpStore, DynSpaceHandler, K2Error, K2Result,
    KitsuneHandler, OpStoreFactory, SpaceId, Timestamp,
};
use kitsune2_core::default_test_builder;
use kitsune2_test_utils::agent::{TestLocalAgent, TestVerifier};

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
    Arc::new(|bytes: &[u8]| -> Result<(kitsune2_api::OpId, Timestamp), K2Error> {
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
    })
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

/// Minimal KitsuneHandler that hands K2 a `HolographSpaceHandler` on
/// `create_space`. Holds an `Arc<HolographSpaceHandler>` slot the test
/// fills in so we can also reach the recv_notify receiver if needed.
#[derive(Debug)]
struct Handler {
    space_handler: Arc<HolographSpaceHandler>,
}

impl KitsuneHandler for Handler {
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
    /// Drained by the test to observe emitted ops.
    emitted_rx: tokio::sync::mpsc::UnboundedReceiver<EmittedOp>,
    /// Holds K2 lifetimes so the test can keep them alive.
    _kitsune: kitsune2_api::DynKitsune,
    _dyn_space: kitsune2_api::DynSpace,
    _agent: DynLocalAgent,
    _dir: tempfile::TempDir,
}

async fn build_node(name: &'static str, mem_bootstrap_test_id: String) -> Node {
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

    let shim_slot: Arc<StdMutex<Option<Arc<K2OpStoreShim>>>> =
        Arc::new(StdMutex::new(None));

    let (handler, _telepresence_rx) = HolographSpaceHandler::new();

    // Start from K2's default test builder (mem transport + mem bootstrap
    // + core gossip stub) and substitute our op-store factory.
    let mut builder = Builder {
        verifier: Arc::new(TestVerifier),
        op_store: Arc::new(ShimFactory {
            op_store: Arc::clone(&op_store),
            shim_slot: Arc::clone(&shim_slot),
        }),
        ..default_test_builder()
    };

    // Bind all nodes in the same mem-bootstrap "test instance" so they
    // discover each other in-process.
    use kitsune2_core::factories::MemBootstrapModConfig;
    builder
        .config
        .set_module_config(&MemBootstrapModConfig {
            mem_bootstrap: kitsune2_core::factories::MemBootstrapConfig {
                test_id: mem_bootstrap_test_id,
                poll_freq_ms: 100,
            },
        })
        .unwrap();

    let kitsune = builder
        .with_default_config()
        .unwrap()
        .build()
        .await
        .unwrap();

    let kitsune_handler: Arc<dyn KitsuneHandler> = Arc::new(Handler {
        space_handler: Arc::clone(&handler),
    });
    kitsune.register_handler(kitsune_handler).await.unwrap();

    // Build the space — this calls our ShimFactory::create which fills
    // shim_slot.
    let dyn_space = kitsune.space(test_space_id(), None).await.unwrap();
    let shim = shim_slot
        .lock()
        .unwrap()
        .clone()
        .expect("ShimFactory should have populated the slot");

    // Wire holograph above the K2 modules.
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

    // Install the queue into the K2-facing shim so inbound ops route
    // through the integration pipeline.
    shim.install_queue(Arc::clone(space.queue()));

    // Join a local agent on full arc so this node participates in gossip
    // for everything.
    let agent = Arc::new(TestLocalAgent::default()) as DynLocalAgent;
    agent.set_cur_storage_arc(DhtArc::FULL);
    agent.set_tgt_storage_arc_hint(DhtArc::FULL);
    dyn_space.local_agent_join(agent.clone()).await.unwrap();

    tracing::info!(node = name, "node built and joined");

    Node {
        name,
        space,
        emitted_rx,
        _kitsune: kitsune,
        _dyn_space: dyn_space,
        _agent: agent,
        _dir: dir,
    }
}

/// Wait for the receiver to produce an `EmittedOp` matching `expect`
/// within `timeout`. Polls every 100ms.
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
            Err(_) => {
                return Err(format!(
                    "{}: timeout waiting for op-id emit",
                    node.name
                ))
            }
        }
    }
}

/// Boot two nodes. Alice commits an envelope; Bob's notifier receives
/// it.  Then Alice commits a child whose parent is the first op; Bob's
/// queue ingests the child, recognizes the parent is present, and
/// promotes it.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn two_node_commit_propagates_via_real_k2() {
    let _ = tracing_subscriber::fmt::try_init();
    // Per-test mem-bootstrap id so this test doesn't see ghosts from
    // other tests sharing the same process.
    let mem_id = format!(
        "holograph-two-node-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );

    let mut alice = build_node("alice", mem_id.clone()).await;
    let mut bob = build_node("bob", mem_id.clone()).await;

    // Force the mem bootstrap to poll immediately so both peers learn
    // about each other promptly.
    kitsune2_core::factories::MemBootstrapFactory::trigger_immediate_poll();

    // Give the bootstrap loop a moment to insert each side into the
    // other's peer store. The mem bootstrap poll_freq is 100ms so a
    // short wait should be plenty.
    tokio::time::sleep(Duration::from_millis(800)).await;

    // Sanity: Bob should know about Alice and vice versa via the mem
    // bootstrap (each node's peer store has both agents).
    let bob_peers = bob._dyn_space.peer_store().get_all().await.unwrap();
    assert!(
        bob_peers.iter().any(|p| !p.is_tombstone),
        "Bob should know at least one peer after bootstrap"
    );

    // -------- Commit 1: root envelope --------
    let (root_bytes, root_id) = make_envelope(b"alice-root", vec![]);
    let returned = alice
        .space
        .on_local_commit(root_bytes.clone())
        .await
        .expect("alice commit root");
    assert_eq!(returned, root_id);

    // Alice's own notifier emits straight away because on_local_commit
    // goes through her queue.
    let alice_emit = wait_for_emit(&mut alice, &root_id, Duration::from_secs(5))
        .await
        .expect("alice should self-emit the local commit");
    assert_eq!(alice_emit.envelope_bytes, root_bytes);

    // Bob should receive the root via K2 publish_ops (eager hint to peers).
    let bob_emit = wait_for_emit(&mut bob, &root_id, Duration::from_secs(30))
        .await
        .expect("bob should receive alice's root envelope within 30s");
    assert_eq!(bob_emit.op_id, root_id);
    assert_eq!(bob_emit.envelope_bytes, root_bytes);

    // -------- Commit 2: child envelope with parent = root --------
    let (child_bytes, child_id) =
        make_envelope(b"alice-child", vec![root_id.clone()]);
    alice
        .space
        .on_local_commit(child_bytes.clone())
        .await
        .expect("alice commit child");

    // Alice should self-emit.
    let _alice_child_emit = wait_for_emit(&mut alice, &child_id, Duration::from_secs(5))
        .await
        .expect("alice should self-emit the child");

    // Bob's queue should:
    //   1. Receive the child via publish_ops.
    //   2. See parent_id == root_id is already in its op-store.
    //   3. Take the all-parents-present branch → store + emit.
    let bob_child_emit =
        wait_for_emit(&mut bob, &child_id, Duration::from_secs(30))
            .await
            .expect("bob should receive alice's child envelope within 30s");
    assert_eq!(bob_child_emit.envelope_bytes, child_bytes);

    // Bob's op-store now holds both ops.
    assert_eq!(bob.space.op_count(), 2);
    assert_eq!(alice.space.op_count(), 2);
}
