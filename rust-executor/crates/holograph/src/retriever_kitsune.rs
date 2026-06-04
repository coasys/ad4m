//! `KitsuneRetreiver` — the Kitsune2-backed implementation of
//! `perspective_diff_sync::PerspectiveDiffRetreiver`.
//!
//! Bridges three things:
//!
//! 1. The synchronous static `PerspectiveDiffRetreiver` trait surface
//!    (no `&self`; methods reach for a process-global state).
//! 2. The async `kitsune2_api::OpStore` trait (every call returns
//!    `BoxFuture`).
//! 3. The integrity-zome data types
//!    (`PerspectiveDiffEntryReference`, `EntryTypes`, `HashReference`,
//!    `LocalHashReference`) and their `SerializedBytes` serialization.
//!
//! Per SPIKE §2.6 ("Tokio runtime nesting … deadlocks"), every async
//! K2 call goes through a *dedicated* worker `tokio::runtime::Runtime`
//! owned by the installed state — not the executor's runtime. That makes
//! `block_on` safe to call from the sync trait-method path: the inner
//! runtime's worker threads are guaranteed not to be the same threads
//! `block_on` is being called from.
//!
//! Per orchestrator's Option A (`.spike-status/blocker-step-1.5.md`),
//! this crate depending on p-diff-sync/HDK transitively is accepted for
//! the spike; the architectural cleanup is parked for PR-B.

use std::sync::Arc;

use bytes::Bytes;
use chrono::{DateTime, Utc};
use hdk::prelude::{HoloHash, SerializedBytes, UnsafeBytes};
use holo_hash::hash_type;
use kitsune2_api::{K2Error, OpId, OpStore, SpaceId, Timestamp};
use once_cell::sync::Lazy;
use sha2::{Digest, Sha256};
use std::sync::RwLock;
use tokio::runtime::Runtime;

use perspective_diff_algorithm as algo;
use perspective_diff_sync::errors::{SocialContextError, SocialContextResult};
use perspective_diff_sync::link_adapter::conversions::{
    entry_ref_from_algo, entry_ref_to_algo, hash_from_algo, hash_ref_to_algo, hash_to_algo,
    local_hash_ref_to_algo,
};
use perspective_diff_sync::retriever::PerspectiveDiffRetreiver;
use perspective_diff_sync_integrity::{
    EntryTypes, HashReference, LocalHashReference, PerspectiveDiffEntryReference,
};

use crate::config::ArcPolicy;
use crate::envelope::OpEnvelope;
use crate::op_store::{EnvelopeDecoder, KvOpStore};

type Hash = HoloHash<hash_type::Action>;

/// Tag bytes appended to a SHA-256 digest to produce a 36-byte
/// `HoloHash<Action>`-shaped value. Matches `MockPerspectiveGraph`'s
/// scheme (its `create_entry` does the same) so test fixtures map 1:1.
///
/// Wake-19 E2: this is now an alias for
/// `envelope::ANCESTRY_OP_TAG` — Head ops use `envelope::HEAD_OP_TAG`
/// so the loc-callback can route by trailer without decoding the
/// payload.
const HASH_TAG: [u8; 4] = crate::envelope::ANCESTRY_OP_TAG;

/// The process-global registered state. Installed once at substrate
/// construction time (Step 4 will wire `HolographSpace` to call this);
/// the static `PerspectiveDiffRetreiver` methods reach for it via
/// `state()`. Stored behind a `RwLock` so tests can reset between runs
/// — production code installs once and never resets.
static STATE: Lazy<RwLock<Option<Arc<KitsuneRetreiverState>>>> = Lazy::new(|| RwLock::new(None));

/// Mutable state backing a single `KitsuneRetreiver`. One installs at a
/// time per process — matches the existing `MockPerspectiveGraph` /
/// `HolochainRetreiver` model where there is one substrate active per
/// running zome.
pub struct KitsuneRetreiverState {
    op_store: Arc<KvOpStore>,
    /// Small sled tree holding the `current_revision` / `latest_revision`
    /// pointers. These are *out-of-band* relative to the op DAG — the
    /// algorithm uses them to know where to start walking. K2 gossip
    /// surfaces the diff ops themselves; the pointers are local mutable
    /// state.
    revisions: sled::Tree,
    runtime: Arc<Runtime>,
}

impl std::fmt::Debug for KitsuneRetreiverState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KitsuneRetreiverState").finish()
    }
}

impl KitsuneRetreiverState {
    /// Open or create the substrate state at `path`. `space_id` is
    /// passed through to the K2 op-store; the revisions tree lives in
    /// the same sled DB.
    pub fn open(
        path: impl AsRef<std::path::Path>,
        space_id: SpaceId,
        arc_policy: ArcPolicy,
    ) -> Result<Arc<Self>, K2Error> {
        let db_path = path.as_ref().to_path_buf();
        let op_store = KvOpStore::open(
            db_path.join("ops"),
            space_id,
            arc_policy,
            holograph_envelope_decoder(),
        )?;

        // Reuse a single sled::Db for the revisions tree so the
        // KitsuneRetreiver only owns one filesystem footprint per space.
        let rev_db =
            sled::open(db_path.join("revisions")).map_err(|e| K2Error::other_src("sled", e))?;
        let revisions = rev_db
            .open_tree(b"revisions")
            .map_err(|e| K2Error::other_src("open revisions tree", e))?;

        let runtime = Arc::new(
            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(2)
                .thread_name("holograph-worker")
                .enable_all()
                .build()
                .map_err(|e| K2Error::other_src("tokio rt build", e))?,
        );

        Ok(Arc::new(Self {
            op_store,
            revisions,
            runtime,
        }))
    }

    /// Borrow the underlying op store (e.g., for tests, smoketests, or
    /// for `HolographSpace` wiring later).
    pub fn op_store(&self) -> &Arc<KvOpStore> {
        &self.op_store
    }
}

/// Holochain-style 36-byte hash over `bytes` (SHA-256 + 4 tag bytes).
///
/// Wake-19 E2 — the trailing 4 bytes are picked from
/// `envelope::ANCESTRY_OP_TAG` / `HEAD_OP_TAG` so the K2
/// `loc_callback` can route an op without decoding the envelope.
fn hash_bytes_with_tag(bytes: &[u8], tag: [u8; 4]) -> [u8; 36] {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    let mut out = [0u8; 36];
    out[..32].copy_from_slice(&digest);
    out[32..].copy_from_slice(&tag);
    out
}

/// Legacy entry-point — Ancestry tag. Kept for the `create_entry`
/// path which still hashes raw integrity payload bytes.
fn hash_bytes(bytes: &[u8]) -> [u8; 36] {
    hash_bytes_with_tag(bytes, HASH_TAG)
}

fn hash_to_op_id(hash: &Hash) -> OpId {
    OpId::from(Bytes::copy_from_slice(hash.get_raw_36()))
}

#[allow(dead_code)]
fn op_id_to_hash(op_id: &OpId) -> Hash {
    HoloHash::<hash_type::Action>::from_raw_36(op_id.0 .0.to_vec())
}

/// The envelope decoder Holograph spaces install on their `KvOpStore`.
///
/// Wake-19 E2: op-id derivation now depends on `env.op_class`.
///
/// - **Ancestry**: `sha256(env.payload) || ANCESTRY_OP_TAG ([0xdb;4])`.
///   This is byte-stable with the pre-Wake-19 derivation so existing
///   ops keep the same op-id across the upgrade.
/// - **Head**: `sha256(envelope_bytes) || HEAD_OP_TAG ([0xa1;4])` —
///   hashes the *whole envelope* so distinct Head ops (same target
///   ancestry, different author / timestamp) get distinct op-ids.
///   The `HEAD_OP_TAG` trailer is what the loc-callback inspects
///   to route Head ops to the fixed sector.
///
/// Timestamp is read from `env.created_at_micros` unchanged.
pub fn holograph_envelope_decoder() -> EnvelopeDecoder {
    Arc::new(|bytes: &[u8]| -> Result<(OpId, Timestamp), K2Error> {
        let env =
            OpEnvelope::decode(bytes).map_err(|e| K2Error::other_src("decode envelope", e))?;
        let id_bytes = match env.op_class {
            crate::envelope::OpClass::Ancestry => {
                hash_bytes_with_tag(env.payload.as_ref(), crate::envelope::ANCESTRY_OP_TAG)
            }
            crate::envelope::OpClass::Head => {
                hash_bytes_with_tag(bytes, crate::envelope::HEAD_OP_TAG)
            }
        };
        let op_id = OpId::from(Bytes::copy_from_slice(&id_bytes));
        let ts = Timestamp::from_micros(env.created_at_micros);
        Ok((op_id, ts))
    })
}

/// The substrate-level marker type the trait impl is hung off of.
///
/// All methods are static (`fn foo(args) -> …`, no `&self`); they reach
/// for the installed `KitsuneRetreiverState` via `state()`. Algorithm
/// callers say `Retriever::get::<KitsuneRetreiver>(hash)` and don't have
/// to pass state through every call site.
pub struct KitsuneRetreiver;

impl KitsuneRetreiver {
    /// Install the global state. Returns an error if state is already
    /// installed (one substrate per process for v1).
    pub fn install(state: Arc<KitsuneRetreiverState>) -> Result<(), &'static str> {
        let mut slot = STATE.write().map_err(|_| "STATE rwlock poisoned")?;
        if slot.is_some() {
            return Err("KitsuneRetreiver state already installed");
        }
        *slot = Some(state);
        Ok(())
    }

    /// Reset the global state. Test-only — production code never resets.
    #[cfg(test)]
    pub(crate) fn reset_for_test() {
        let mut slot = STATE.write().expect("STATE rwlock poisoned");
        *slot = None;
    }

    /// Reset the global state. Public escape hatch for integration tests
    /// in this crate's `tests/` directory, which can't reach the `#[cfg(test)]`
    /// helper. Don't call this from production code.
    #[doc(hidden)]
    pub fn __clear_state_for_tests__() {
        let mut slot = STATE.write().expect("STATE rwlock poisoned");
        *slot = None;
    }

    fn state() -> Arc<KitsuneRetreiverState> {
        STATE
            .read()
            .expect("STATE rwlock poisoned")
            .as_ref()
            .expect("KitsuneRetreiver state not installed — call KitsuneRetreiver::install first")
            .clone()
    }

    fn err(reason: &'static str) -> SocialContextError {
        SocialContextError::InternalError(reason)
    }
}

impl PerspectiveDiffRetreiver for KitsuneRetreiver {
    fn get(hash: Hash) -> SocialContextResult<PerspectiveDiffEntryReference> {
        let state = Self::state();
        let op_id = hash_to_op_id(&hash);

        let ops = state
            .runtime
            .block_on(state.op_store.retrieve_ops(vec![op_id.clone()]))
            .map_err(|_| Self::err("KvOpStore::retrieve_ops failed"))?;

        let meta = ops
            .into_iter()
            .next()
            .ok_or_else(|| Self::err("KitsuneRetreiver: op not found"))?;

        let env = OpEnvelope::decode(meta.op_data.as_ref())
            .map_err(|_| Self::err("KitsuneRetreiver: envelope decode"))?;

        let sb = SerializedBytes::from(UnsafeBytes::from(env.payload.to_vec()));
        let entry = PerspectiveDiffEntryReference::try_from(sb)?;
        Ok(entry)
    }

    fn get_with_timestamp(
        hash: Hash,
    ) -> SocialContextResult<(PerspectiveDiffEntryReference, DateTime<Utc>)> {
        let state = Self::state();
        let op_id = hash_to_op_id(&hash);

        let ops = state
            .runtime
            .block_on(state.op_store.retrieve_ops(vec![op_id]))
            .map_err(|_| Self::err("KvOpStore::retrieve_ops failed"))?;

        let meta = ops
            .into_iter()
            .next()
            .ok_or_else(|| Self::err("KitsuneRetreiver: op not found"))?;

        let env = OpEnvelope::decode(meta.op_data.as_ref())
            .map_err(|_| Self::err("KitsuneRetreiver: envelope decode"))?;

        let sb = SerializedBytes::from(UnsafeBytes::from(env.payload.to_vec()));
        let entry = PerspectiveDiffEntryReference::try_from(sb)?;
        let ts =
            DateTime::<Utc>::from_timestamp_micros(env.created_at_micros).unwrap_or_else(Utc::now);
        Ok((entry, ts))
    }

    fn create_entry(entry: EntryTypes) -> SocialContextResult<Hash> {
        let state = Self::state();

        // Convert the integrity union to its `SerializedBytes` shape.
        let sb: SerializedBytes = match entry {
            EntryTypes::PerspectiveDiffEntryReference(r) => r.try_into()?,
            EntryTypes::Snapshot(s) => s.try_into()?,
            EntryTypes::HashReference(r) => r.try_into()?,
            EntryTypes::LocalHashReference(r) => r.try_into()?,
            EntryTypes::Anchor(a) => a.try_into()?,
            EntryTypes::LocalTimestampReference(t) => t.try_into()?,
            EntryTypes::PrivateOnlineStatus(s) => s.try_into()?,
        };
        let payload = sb.bytes().to_vec();

        let hash_36 = hash_bytes(&payload);
        let hash = HoloHash::<hash_type::Action>::from_raw_36(hash_36.to_vec());
        let now_micros = Utc::now().timestamp_micros();

        let env = OpEnvelope::new_at(
            std::iter::empty(),
            Bytes::from(payload),
            // v1: no real signature here — the spike's `process_incoming_ops`
            // doesn't validate signatures yet. Step 3's integration queue
            // will. See SPIKE §2.4.
            Bytes::from_static(b"holograph-v1-author"),
            Bytes::from_static(b"holograph-v1-sig"),
            None,
            now_micros,
        );

        let env_bytes = env.encode().map_err(|_| Self::err("envelope encode"))?;

        let accepted = state
            .runtime
            .block_on(
                state
                    .op_store
                    .process_incoming_ops(vec![Bytes::from(env_bytes)]),
            )
            .map_err(|_| Self::err("KvOpStore::process_incoming_ops failed"))?;

        if accepted.is_empty() {
            return Err(Self::err("op rejected by KvOpStore (arc?)"));
        }
        Ok(hash)
    }

    fn current_revision() -> SocialContextResult<Option<LocalHashReference>> {
        let state = Self::state();
        match state
            .revisions
            .get(b"current")
            .map_err(|_| Self::err("sled get current"))?
        {
            None => Ok(None),
            Some(ivec) => {
                let (hash_bytes, ts_micros): (Vec<u8>, i64) = ciborium::from_reader(ivec.as_ref())
                    .map_err(|_| Self::err("decode current revision"))?;
                let hash = HoloHash::<hash_type::Action>::from_raw_36(hash_bytes);
                let timestamp =
                    DateTime::<Utc>::from_timestamp_micros(ts_micros).unwrap_or_else(Utc::now);
                Ok(Some(LocalHashReference { hash, timestamp }))
            }
        }
    }

    fn latest_revision() -> SocialContextResult<Option<HashReference>> {
        let state = Self::state();
        match state
            .revisions
            .get(b"latest")
            .map_err(|_| Self::err("sled get latest"))?
        {
            None => Ok(None),
            Some(ivec) => {
                let (hash_bytes, ts_micros): (Vec<u8>, i64) = ciborium::from_reader(ivec.as_ref())
                    .map_err(|_| Self::err("decode latest revision"))?;
                let hash = HoloHash::<hash_type::Action>::from_raw_36(hash_bytes);
                let timestamp =
                    DateTime::<Utc>::from_timestamp_micros(ts_micros).unwrap_or_else(Utc::now);
                Ok(Some(HashReference { hash, timestamp }))
            }
        }
    }

    fn update_current_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let state = Self::state();
        let payload: (Vec<u8>, i64) = (hash.get_raw_36().to_vec(), timestamp.timestamp_micros());
        let mut buf = Vec::new();
        ciborium::into_writer(&payload, &mut buf).map_err(|_| Self::err("encode current"))?;
        state
            .revisions
            .insert(b"current", buf)
            .map_err(|_| Self::err("sled put current"))?;
        Ok(())
    }

    fn update_latest_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let state = Self::state();
        let payload: (Vec<u8>, i64) = (hash.get_raw_36().to_vec(), timestamp.timestamp_micros());
        let mut buf = Vec::new();
        ciborium::into_writer(&payload, &mut buf).map_err(|_| Self::err("encode latest"))?;
        state
            .revisions
            .insert(b"latest", buf)
            .map_err(|_| Self::err("sled put latest"))?;
        Ok(())
    }
}

// Step 13b-C phase 2 bridge — see the same impl on `HolochainRetreiver`
// and `MockPerspectiveGraph`. Snapshots aren't recorded on the K2 path
// for the spike (SPIKE §1.5 narrowing), so `get_snapshot_by_target`
// returns `Ok(None)`.
impl algo::WorkspaceRetriever for KitsuneRetreiver {
    fn get_p_diff_reference(
        hash: &algo::Hash,
    ) -> algo::AlgoResult<algo::PerspectiveDiffEntryReference> {
        let h = hash_from_algo(hash);
        let entry = <Self as PerspectiveDiffRetreiver>::get(h)
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(entry_ref_to_algo(entry))
    }

    fn get_snapshot_by_target(
        _target_hash: &algo::Hash,
    ) -> algo::AlgoResult<Option<algo::Snapshot>> {
        Ok(None)
    }
}

// Step 13b-D — round-trips through the existing
// `PerspectiveDiffRetreiver::create_entry`, which writes the entry to
// the K2 OpStore and returns the deterministic content-hash.
impl algo::SnapshotRetriever for KitsuneRetreiver {
    fn create_diff_entry(
        entry: algo::PerspectiveDiffEntryReference,
    ) -> algo::AlgoResult<algo::Hash> {
        let integrity = entry_ref_from_algo(entry);
        let hash = <Self as PerspectiveDiffRetreiver>::create_entry(
            perspective_diff_sync_integrity::EntryTypes::PerspectiveDiffEntryReference(integrity),
        )
        .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(hash_to_algo(&hash))
    }
}

// Step 13b-E — forwards to the existing sled-backed
// `PerspectiveDiffRetreiver` revision methods.
impl algo::RevisionsRetriever for KitsuneRetreiver {
    fn current_revision() -> algo::AlgoResult<Option<algo::LocalHashReference>> {
        let rev = <Self as PerspectiveDiffRetreiver>::current_revision()
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(rev.map(local_hash_ref_to_algo))
    }

    fn latest_revision() -> algo::AlgoResult<Option<algo::HashReference>> {
        let rev = <Self as PerspectiveDiffRetreiver>::latest_revision()
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(rev.map(hash_ref_to_algo))
    }

    fn update_current_revision(
        hash: algo::Hash,
        timestamp: chrono::DateTime<chrono::Utc>,
    ) -> algo::AlgoResult<()> {
        <Self as PerspectiveDiffRetreiver>::update_current_revision(
            hash_from_algo(&hash),
            timestamp,
        )
        .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use perspective_diff_sync_integrity::PerspectiveDiff;
    use std::sync::Mutex;

    // Global test mutex — `STATE` is process-global and `install` is
    // one-shot; serialize tests against it.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn setup() -> (tempfile::TempDir, std::sync::MutexGuard<'static, ()>) {
        let guard = TEST_LOCK.lock().unwrap();
        KitsuneRetreiver::reset_for_test();
        let dir = tempfile::tempdir().unwrap();
        let state = KitsuneRetreiverState::open(
            dir.path(),
            SpaceId::from(Bytes::from_static(b"test-space")),
            ArcPolicy::Full,
        )
        .expect("open state");
        KitsuneRetreiver::install(state).expect("install");
        (dir, guard)
    }

    #[test]
    fn create_then_get_roundtrip() {
        let (_dir, _guard) = setup();

        let entry = PerspectiveDiffEntryReference::new(
            PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
            None,
        );
        let hash = KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(
            entry.clone(),
        ))
        .expect("create");
        let fetched = KitsuneRetreiver::get(hash).expect("get");
        assert_eq!(fetched, entry);
    }

    #[test]
    fn create_is_deterministic_in_hash() {
        let (_dir, _guard) = setup();

        let entry = PerspectiveDiffEntryReference::new(
            PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
            None,
        );
        let h1 = KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(
            entry.clone(),
        ))
        .expect("create");
        let h2 = KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(entry))
            .expect("create");
        // Same content -> same hash (and the second store is a no-op).
        assert_eq!(h1, h2);
    }

    #[test]
    fn revisions_roundtrip() {
        let (_dir, _guard) = setup();

        assert!(KitsuneRetreiver::current_revision().unwrap().is_none());
        assert!(KitsuneRetreiver::latest_revision().unwrap().is_none());

        let hash = HoloHash::<hash_type::Action>::from_raw_36(vec![7; 36]);
        let now = Utc::now();
        KitsuneRetreiver::update_current_revision(hash.clone(), now).unwrap();
        KitsuneRetreiver::update_latest_revision(hash.clone(), now).unwrap();

        let cur = KitsuneRetreiver::current_revision().unwrap().unwrap();
        assert_eq!(cur.hash, hash);
        let lat = KitsuneRetreiver::latest_revision().unwrap().unwrap();
        assert_eq!(lat.hash, hash);
    }

    #[test]
    fn get_with_timestamp_returns_creation_time() {
        let (_dir, _guard) = setup();

        let entry = PerspectiveDiffEntryReference::new(
            PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
            None,
        );
        let before = Utc::now();
        let hash = KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(entry))
            .expect("create");
        let (_entry, ts) = KitsuneRetreiver::get_with_timestamp(hash).expect("get");
        let after = Utc::now();

        assert!(
            ts >= before && ts <= after,
            "timestamp should be in [before, after], got {ts}"
        );
    }
}
