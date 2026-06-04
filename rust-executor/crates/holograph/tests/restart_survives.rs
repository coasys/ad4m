//! Wake-18 D4 — full restart-survives-state integration test.
//!
//! Formalizes SPIKE §2.5 exit-check #6: "restart survives state via
//! sled."  The unit-level `state_persists_across_reopen` covers a
//! single op; this exercises the substrate at scale (100 ops across
//! 3 logical agents, exhaustive retrieval check after restart).
//!
//! Flow:
//!   1. Open a `HolographSpace` at path P, on a fresh tempdir.
//!   2. Commit 100 distinct envelopes — 3 logical "agents" round-robin
//!      via the envelope's author tag, so the ops cover the full id
//!      space (different SHA-256 prefixes).
//!   3. Call `space.shutdown()` so the sled DB is flushed.
//!   4. Drop the space.
//!   5. Reopen `KvOpStore` at the same path.
//!   6. Assert: op_count == 100; every op_id from step 2 is retrievable
//!      and its bytes match.

use std::sync::Arc;
use std::time::Duration;

use bytes::Bytes;
use futures::future::BoxFuture;
use holograph::{
    holograph_envelope_decoder, ArcPolicy, EnvelopeDecoder, FetchFallbackPolicy, HolographSpace,
    HolographSpaceConfig, KvOpStore, LocalCommitTarget, NotifyUp, OpEnvelope, OpFetcher,
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

// --- helpers ----------------------------------------------------------------

fn envelope_decoder() -> EnvelopeDecoder {
    holograph_envelope_decoder()
}

fn space_id() -> SpaceId {
    SpaceId::from(Bytes::from_static(b"restart-test"))
}

fn open_space_at(path: &std::path::Path, handle: tokio::runtime::Handle) -> Arc<HolographSpace> {
    let op_store = KvOpStore::open(
        path.join("ops"),
        space_id(),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .expect("open op_store");
    let pending_db = sled::open(path.join("pending")).expect("open pending");
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
    // Keep pending_db alive for the whole test via leak — drop after
    // the space drops. Cleaner alternatives (carrying the Db) require
    // extending HolographSpace's API.
    Box::leak(Box::new(pending_db));
    HolographSpace::new(cfg)
}

/// Encode one envelope for `agent_idx`'s `seq`-th commit. Payload is
/// `agent-seq` so distinct (agent, seq) pairs hash to distinct ops.
fn make_envelope_for(agent_idx: usize, seq: usize) -> (Bytes, OpId) {
    let payload = format!("agent{}-op{}", agent_idx, seq).into_bytes();
    let env = OpEnvelope::new_at(
        std::iter::empty(),
        Bytes::from(payload),
        Bytes::from(format!("pk-agent{}", agent_idx).into_bytes()),
        Bytes::from_static(b"sig"),
        None,
        1_700_000_000_000_000 + (agent_idx as i64) * 1000 + seq as i64,
    );
    let bytes = Bytes::from(env.encode().expect("encode"));
    let (op_id, _) = envelope_decoder()(&bytes).expect("decode");
    (bytes, op_id)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn restart_survives_state_100_ops_3_agents() {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().to_path_buf();
    let handle = tokio::runtime::Handle::current();

    // Phase 1: open the space and commit 100 ops across 3 agents.
    let mut all_ids: Vec<OpId> = Vec::with_capacity(100);
    let mut all_bytes: Vec<Bytes> = Vec::with_capacity(100);
    {
        let space = open_space_at(&path, handle.clone());
        for seq in 0..100 {
            let agent = seq % 3;
            let (bytes, op_id) = make_envelope_for(agent, seq);
            space
                .on_local_commit(bytes.clone())
                .await
                .expect("on_local_commit");
            all_ids.push(op_id);
            all_bytes.push(bytes);
        }
        // Wake-19 E4: each Ancestry commit auto-publishes a matching
        // Head, doubling the op count to 200.
        assert_eq!(
            space.op_count(),
            200,
            "100 Ancestry + 100 auto-Head ops persisted in phase 1"
        );

        // Phase 2: graceful shutdown drains + flushes.
        let remaining = space.shutdown().await.expect("shutdown");
        assert_eq!(remaining, 0, "no pending entries at drain-time");
        // Drop the Arc — Drop runs flush_blocking as the safety net.
        drop(space);
    }

    // Phase 3: reopen the substrate at the same path and verify the
    // entire op set survived.
    let reopened = KvOpStore::open(
        path.join("ops"),
        space_id(),
        ArcPolicy::Full,
        envelope_decoder(),
    )
    .expect("reopen op_store");

    assert_eq!(
        reopened.op_count_blocking(),
        200,
        "all 200 ops (100 Ancestry + 100 Head) still present after restart"
    );

    // Spot-check every op individually: retrieve_ops round-trips bytes.
    use kitsune2_api::OpStore;
    let fetched = reopened
        .retrieve_ops(all_ids.clone())
        .await
        .expect("retrieve_ops");
    assert_eq!(fetched.len(), 100, "retrieved every op id");

    // Build a lookup from op_id → bytes, then assert each input id
    // round-trips to its original bytes.
    let by_id: std::collections::HashMap<_, _> = fetched
        .into_iter()
        .map(|m| (m.op_id.clone(), m.op_data.clone()))
        .collect();
    for (id, original) in all_ids.iter().zip(all_bytes.iter()) {
        let got = by_id.get(id).expect("op id present after restart");
        assert_eq!(got, original, "bytes round-trip for {:?}", id);
    }
}
