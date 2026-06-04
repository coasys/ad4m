//! Wake-19 E5 + E6 — two-op-class integration tests.
//!
//! Covers the substrate-level invariants for E1..E4 end-to-end against
//! a real `HolographSpace`:
//!
//! - **Legacy envelope round-trip (E5)** — a hand-crafted pre-Wake-19
//!   envelope (no `op_class`, no `head_pointer`) decodes as `Ancestry`,
//!   stores normally, and *does not* auto-publish a Head when received
//!   via `process_incoming_ops` (only `on_local_commit` triggers the
//!   auto-Head; gossiped Ancestry ops just store).
//!
//! - **Dominance over a long parent chain (E6)** — 10 sequential commits
//!   produce 10 Ancestry + 10 Head ops; `current_heads()` returns
//!   exactly 1 op (the latest Head), the prior 9 are dominated.
//!
//! - **Sibling commits don't dominate each other** — two fork branches
//!   from a shared root each produce a Head; both stay in
//!   `current_heads()`.
//!
//! The K2 partial-arc Head-replication test from the dispatch's E6a/E6c
//! is deferred — it needs a real K2 multi-node setup with non-FULL arc
//! claims, which is out of scope for the single-process test harness.
//! See the wake-19-summary.md "Deferred" section for the rationale.

use std::sync::Arc;
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use holograph::{
    holograph_envelope_decoder, ArcPolicy, EnvelopeDecoder, FetchFallbackPolicy, HolographSpace,
    HolographSpaceConfig, KvOpStore, LocalCommitTarget, NotifyUp, OpClass, OpEnvelope, OpFetcher,
    PeerPicker, SpaceConfig,
};
use kitsune2_api::{K2Result, OpId, SpaceId, StoredOp, Url};

// --- minimal mock adapters --------------------------------------------------

#[derive(Debug, Default)]
struct NoopFetcher;
impl OpFetcher for NoopFetcher {
    fn request_ops(&self, _: Vec<OpId>, _: Url) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async { Ok(()) })
    }
}

#[derive(Debug, Default)]
struct NoopPicker;
impl PeerPicker for NoopPicker {
    fn pick_arc_overlap_peer(
        &self,
        _: u32,
        _: std::collections::HashSet<Url>,
    ) -> BoxFuture<'_, K2Result<Option<Url>>> {
        Box::pin(async { Ok(None) })
    }
}

#[derive(Debug, Default)]
struct Recorder;
impl NotifyUp for Recorder {
    fn emit_perspective_diff(&self, _: OpId, _: kitsune2_api::Timestamp, _: Bytes) {}
}

#[derive(Debug, Default)]
struct Sink;
impl LocalCommitTarget for Sink {
    fn inform_ops_stored(&self, _: Vec<StoredOp>) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async { Ok(()) })
    }
    fn publish_ops_to_peers(&self, _: Vec<OpId>) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async { Ok(()) })
    }
}

// --- helpers ---------------------------------------------------------------

fn envelope_decoder() -> EnvelopeDecoder {
    holograph_envelope_decoder()
}

fn space_id() -> SpaceId {
    SpaceId::from(Bytes::from_static(b"two-op-class-test"))
}

fn build_space(dir: &std::path::Path, handle: tokio::runtime::Handle) -> Arc<HolographSpace> {
    let op_store = KvOpStore::open(
        dir.join("ops"),
        space_id(),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .expect("open op_store");
    let pending_db = sled::open(dir.join("pending")).expect("open pending");
    let pending = pending_db.open_tree(b"pending").expect("open pending tree");

    let cfg = HolographSpaceConfig {
        config: SpaceConfig {
            fetch_fallback_policy: FetchFallbackPolicy {
                initial_timeout: Duration::from_secs(60),
                max_attempts: 1,
                retry_budget: Duration::from_secs(60),
            },
            ..SpaceConfig::full_replication_single_doc()
        },
        op_store,
        pending,
        decode_envelope: envelope_decoder(),
        fetcher: Arc::new(NoopFetcher),
        peer_picker: Arc::new(NoopPicker),
        notify: Arc::new(Recorder),
        commit_target: Arc::new(Sink),
        sig_verifier: Arc::new(holograph::AlwaysValid),
        runtime: handle,
        watcher_tick: Duration::from_secs(60),
    };
    Box::leak(Box::new(pending_db));
    HolographSpace::new(cfg)
}

/// Build an Ancestry envelope with `payload` and `parents`. Timestamps
/// vary by `seq` so distinct envelopes always hash to distinct op-ids.
fn ancestry(seq: usize, parents: Vec<OpId>) -> (Bytes, OpId) {
    let env = OpEnvelope::new_at(
        parents,
        Bytes::from(format!("payload-{}", seq).into_bytes()),
        Bytes::from_static(b"pk"),
        Bytes::from_static(b"sg"),
        None,
        1_700_000_000_000_000 + seq as i64,
    );
    let bytes = Bytes::from(env.encode().expect("encode ancestry"));
    let (op_id, _) = envelope_decoder()(&bytes).expect("decode ancestry");
    (bytes, op_id)
}

// --------------------------- Tests ----------------------------------------

/// Wake-19 E6 — 10-deep commit chain dominates down to one head.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn long_parent_chain_dominates_to_one_head() {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();
    let space = build_space(dir.path(), handle.clone());

    let mut prev: Option<OpId> = None;
    let mut ancestry_ids: Vec<OpId> = Vec::with_capacity(10);
    for seq in 0..10 {
        let parents = prev.iter().cloned().collect::<Vec<_>>();
        let (bytes, op_id) = ancestry(seq, parents);
        space.on_local_commit(bytes).await.expect("commit");
        ancestry_ids.push(op_id.clone());
        prev = Some(op_id);
    }

    // 10 Ancestry + 10 Head = 20 stored ops.
    assert_eq!(space.op_count(), 20);

    // Only the final Head should be a current head. The other 9
    // dominated heads remain in the `ops` tree but are not in
    // `current_heads`.
    assert_eq!(space.op_store().current_heads_count(), 1);
    let heads = space.op_store().current_head_op_ids().unwrap();
    assert_eq!(heads.len(), 1);

    // The current Head should be the one registered for the latest
    // ancestry op-id.
    let latest_ancestry = ancestry_ids.last().unwrap();
    let head_for_latest = space
        .op_store()
        .head_for_ancestry(latest_ancestry)
        .unwrap()
        .expect("latest ancestry has a registered head");
    assert_eq!(heads[0], head_for_latest);
}

/// Wake-19 E6 — sibling commits from a shared root keep both Heads.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn sibling_commits_keep_both_heads() {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();
    let space = build_space(dir.path(), handle.clone());

    // Root.
    let (root_bytes, root_id) = ancestry(0, vec![]);
    space
        .on_local_commit(root_bytes)
        .await
        .expect("commit root");

    // Sibling A.
    let (a_bytes, a_id) = ancestry(1, vec![root_id.clone()]);
    space.on_local_commit(a_bytes).await.expect("commit a");

    // Sibling B.
    let (b_bytes, b_id) = ancestry(2, vec![root_id.clone()]);
    space.on_local_commit(b_bytes).await.expect("commit b");

    // 3 Ancestry + 3 Head = 6 ops.
    assert_eq!(space.op_count(), 6);

    // Heads: A's Head and B's Head both survive (neither is on the
    // other's parent walk). Root's Head got dominated by A (since
    // root is on A's parent walk); B then runs its walk hitting root
    // again but root is no longer in current_heads, so nothing to
    // dominate.
    assert_eq!(space.op_store().current_heads_count(), 2);

    let head_a = space.op_store().head_for_ancestry(&a_id).unwrap();
    let head_b = space.op_store().head_for_ancestry(&b_id).unwrap();
    assert!(head_a.is_some(), "a has a registered head");
    assert!(head_b.is_some(), "b has a registered head");
    let mut heads = space.op_store().current_head_op_ids().unwrap();
    heads.sort_by_key(|h| Bytes::from(h.clone()).to_vec());
    let mut want = vec![head_a.unwrap(), head_b.unwrap()];
    want.sort_by_key(|h| Bytes::from(h.clone()).to_vec());
    assert_eq!(heads, want);
}

/// Wake-19 E5 — a legacy pre-Wake-19 Ancestry envelope (no `op_class`
/// field) stores normally when delivered via `process_incoming_ops`
/// (the gossip path), without triggering a Head auto-publish. Heads
/// are only emitted by `on_local_commit`.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn legacy_ancestry_gossip_does_not_auto_publish_head() {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();
    let space = build_space(dir.path(), handle.clone());

    // Hand-craft a legacy envelope (no op_class / head_pointer fields).
    #[derive(serde::Serialize)]
    struct LegacyOpEnvelope {
        parents: Vec<Bytes>,
        payload: Bytes,
        author_pubkey: Bytes,
        signature: Bytes,
        #[serde(default)]
        created_at_micros: i64,
        #[serde(skip_serializing_if = "Option::is_none")]
        doc_id: Option<Bytes>,
    }
    let legacy = LegacyOpEnvelope {
        parents: vec![],
        payload: Bytes::from_static(b"legacy-payload"),
        author_pubkey: Bytes::from_static(b"pk"),
        signature: Bytes::from_static(b"sg"),
        created_at_micros: 1_700_000_000_000_000,
        doc_id: None,
    };
    let mut buf = Vec::new();
    ciborium::into_writer(&legacy, &mut buf).expect("encode legacy");
    let legacy_bytes = Bytes::from(buf);

    // Sanity: the same bytes decode as Ancestry.
    let decoded = OpEnvelope::decode(&legacy_bytes).expect("decode legacy");
    assert_eq!(decoded.op_class, OpClass::Ancestry);
    assert!(decoded.head_pointer.is_none());

    // Inject via the gossip path. The legacy peer didn't send a Head,
    // and our local substrate must NOT synthesize one from a peer's
    // op — Heads are only auto-published on `on_local_commit`.
    let accepted = space
        .process_incoming_ops(
            vec![legacy_bytes],
            Some(Url::from_str("ws://peer:1").unwrap()),
        )
        .await
        .expect("process_incoming_ops");
    assert_eq!(accepted.len(), 1);

    // Only the Ancestry op should be stored — no Head was auto-emitted.
    assert_eq!(space.op_count(), 1);
    assert_eq!(space.op_store().current_heads_count(), 0);
}

/// Wake-19 E5 — locally committing a legacy-shape envelope (we use
/// the new code to encode an Ancestry envelope, which is byte-stable
/// with the legacy shape) DOES auto-publish a Head, because the
/// commit path always emits a Head regardless of how the envelope
/// was constructed.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn local_commit_of_legacy_shape_still_publishes_head() {
    let dir = tempfile::tempdir().unwrap();
    let handle = tokio::runtime::Handle::current();
    let space = build_space(dir.path(), handle.clone());

    // Build an Ancestry envelope with the new code. Its bytes are
    // identical to what a pre-Wake-19 peer would produce (covered by
    // envelope::tests::ancestry_envelope_bytes_match_legacy_shape).
    let (bytes, op_id) = ancestry(0, vec![]);
    space.on_local_commit(bytes).await.expect("commit");

    assert_eq!(space.op_count(), 2, "Ancestry + auto-Head");
    assert_eq!(space.op_store().current_heads_count(), 1);
    assert!(
        space
            .op_store()
            .head_for_ancestry(&op_id)
            .unwrap()
            .is_some(),
        "local commit registered a Head for the ancestry op-id"
    );
}
