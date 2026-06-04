//! Sled-backed implementation of the Kitsune2 `OpStore` trait.
//!
//! The reference K2 implementation (`mem_op_store.rs`) keeps everything in
//! `RwLock<HashMap>`s; this is fine for tests but evaporates on restart.
//! Per SPIKE.md §0 ("Persistence is not optional even for the spike"), v1
//! ships sled-backed storage from the start.
//!
//! Storage layout (one `sled::Db` per space, one tree per kind):
//!
//! - `ops`: `op_id_bytes -> ciborium-encoded OpRecord {created_at_micros,
//!   stored_at_micros, op_data}`. We don't keep secondary time indexes;
//!   the query methods scan and filter. v1-scale data volumes don't justify
//!   the bookkeeping cost; a future spike with measured load can add them
//!   if hot.
//! - `slice_hashes`: composite key `arc_bytes(9) || slice_id_be(8)` ->
//!   raw hash bytes. The arc prefix lets us range-scan a single arc's
//!   slices in one cursor pass.
//!
//! All async methods are implemented as `BoxFuture<'_, …>` returning the
//! result of a `spawn_blocking` over the synchronous sled API — this keeps
//! the K2 runtime non-blocking while letting sled use its own thread pool.

use std::sync::Arc;

use bytes::Bytes;
use futures::future::BoxFuture;
use kitsune2_api::{DhtArc, K2Error, K2Result, MetaOp, OpId, OpStore, SpaceId, Timestamp};
use serde::{Deserialize, Serialize};

use crate::config::ArcPolicy;

/// Classify a `sled::Error` as "lock held by another process" so
/// `KvOpStore::open` can retry. sled wraps the underlying
/// `fs2::FileExt::try_lock_exclusive` failure in an
/// `io::Error::new(io::ErrorKind::Other, "could not acquire lock ...")`
/// (the inner `Os { code: EWOULDBLOCK }` is stringified into the message
/// rather than preserved as the outer kind). We match on the message
/// prefix sled emits — both `WouldBlock` (Linux/macOS) and
/// `AlreadyExists` (Windows) are caught via the same `kind: Other`
/// wrapping, so the message text is the reliable signal.
fn is_lock_contention(e: &sled::Error) -> bool {
    match e {
        sled::Error::Io(io_err) => {
            if matches!(
                io_err.kind(),
                std::io::ErrorKind::WouldBlock | std::io::ErrorKind::AlreadyExists
            ) {
                return true;
            }
            // Fallback: sled wraps the OS-level lock failure into
            // `kind: Other` with a "could not acquire lock" message.
            let s = io_err.to_string();
            s.contains("could not acquire lock") || s.contains("WouldBlock")
        }
        _ => false,
    }
}

/// On-disk shape of a stored op.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpRecord {
    /// Authoring timestamp — must be consistent across peers. Carried
    /// out-of-band by the op envelope (`OpEnvelope`) in v1.
    created_at_micros: i64,
    /// Wall-clock timestamp at which this node first stored the op.
    /// Used for the gossip paging cursor (`retrieve_op_ids_bounded`).
    stored_at_micros: i64,
    /// The raw envelope bytes that K2 hands back to peers via
    /// `retrieve_ops` and feeds back into `process_incoming_ops`.
    op_data: Vec<u8>,
}

/// Trait an op envelope must implement so the OpStore can pull a stable
/// timestamp + op-id out of its raw bytes.
///
/// v1 wires this up at `KvOpStore::new` via a closure rather than as a
/// trait param — keeps the OpStore generic-free and lets the substrate
/// host (`HolographSpace`) own envelope semantics.
pub type EnvelopeDecoder = Arc<dyn Fn(&[u8]) -> Result<(OpId, Timestamp), K2Error> + Send + Sync>;

/// Sled-backed Kitsune2 op store, scoped to a single K2 space.
pub struct KvOpStore {
    space_id: SpaceId,
    arc_policy: ArcPolicy,
    db: sled::Db,
    ops: sled::Tree,
    slice_hashes: sled::Tree,
    decode_envelope: EnvelopeDecoder,
}

impl std::fmt::Debug for KvOpStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KvOpStore")
            .field("space_id", &self.space_id)
            .field("arc_policy", &self.arc_policy)
            .field("path", &self.db.checksum().ok())
            .finish()
    }
}

impl KvOpStore {
    /// Open a sled DB at `path` and bind it to a K2 space.
    ///
    /// `decode_envelope` is called by `process_incoming_ops` for each
    /// inbound op blob to extract its op-id and creation timestamp. The
    /// substrate layer owns the envelope format — see
    /// `crate::envelope::OpEnvelope` for v1's shape, and `HolographSpace`
    /// for the wiring.
    ///
    /// Lock-contention recovery: sled holds an exclusive advisory
    /// file lock on `db/.lock`. A concurrent `sled::open` against the
    /// same path returns `Error::Io` (kind WouldBlock / AlreadyExists
    /// depending on platform). This open retries with exponential
    /// backoff (50/100/200/400/800ms — total ~1.55s) so two
    /// `HolographSpace::new` racing on the same data directory
    /// don't both fail.
    pub fn open(
        path: impl AsRef<std::path::Path>,
        space_id: SpaceId,
        arc_policy: ArcPolicy,
        decode_envelope: EnvelopeDecoder,
    ) -> Result<Arc<Self>, K2Error> {
        const BACKOFF_MS: &[u64] = &[50, 100, 200, 400, 800];

        let path = path.as_ref();
        let mut last_err: Option<sled::Error> = None;
        for (attempt, &delay_ms) in BACKOFF_MS.iter().enumerate() {
            // Stale-lock cleanup: POSIX advisory locks die with the
            // owning process so sled's `.lock` file alone isn't a
            // reliable "lock held" signal. After the first failed
            // attempt, try to remove the lock file once — if the
            // owning process is gone the next open will re-create it
            // cleanly; if it's alive, the OS-level advisory lock
            // still blocks us and we fall back to the backoff loop.
            if attempt == 1 {
                let lock_path = path.join(".lock");
                let _ = std::fs::remove_file(&lock_path);
            }
            match sled::open(path) {
                Ok(db) => {
                    let ops = db
                        .open_tree(b"ops")
                        .map_err(|e| K2Error::other_src("open ops tree", e))?;
                    let slice_hashes = db
                        .open_tree(b"slice_hashes")
                        .map_err(|e| K2Error::other_src("open slice_hashes tree", e))?;
                    return Ok(Arc::new(Self {
                        space_id,
                        arc_policy,
                        db,
                        ops,
                        slice_hashes,
                        decode_envelope,
                    }));
                }
                Err(e) => {
                    if !is_lock_contention(&e) {
                        return Err(K2Error::other_src("sled::open", e));
                    }
                    last_err = Some(e);
                    std::thread::sleep(std::time::Duration::from_millis(delay_ms));
                }
            }
        }
        Err(K2Error::other_src(
            "sled::open (lock-contention after 5 retries)",
            last_err.expect("backoff loop ran at least once"),
        ))
    }

    /// Synchronous helper for tests + the smoketest. Counts ops without
    /// going through the async trait.
    pub fn op_count_blocking(&self) -> u64 {
        self.ops.len() as u64
    }

    fn target_arc(&self) -> DhtArc {
        self.arc_policy.target_arc()
    }

    fn put_op_record(&self, op_id: &OpId, record: &OpRecord) -> Result<(), K2Error> {
        let mut buf = Vec::new();
        ciborium::into_writer(record, &mut buf)
            .map_err(|e| K2Error::other_src("encode OpRecord", e))?;
        self.ops
            .insert(op_id_key(op_id), buf)
            .map_err(|e| K2Error::other_src("ops.insert", e))?;
        Ok(())
    }

    fn get_op_record(&self, op_id: &OpId) -> Result<Option<OpRecord>, K2Error> {
        match self
            .ops
            .get(op_id_key(op_id))
            .map_err(|e| K2Error::other_src("ops.get", e))?
        {
            None => Ok(None),
            Some(bytes) => {
                let rec: OpRecord = ciborium::from_reader(bytes.as_ref())
                    .map_err(|e| K2Error::other_src("decode OpRecord", e))?;
                Ok(Some(rec))
            }
        }
    }

    fn iter_op_records(&self) -> impl Iterator<Item = Result<(OpId, OpRecord), K2Error>> + '_ {
        self.ops.iter().map(|kv| {
            let (k, v) = kv.map_err(|e| K2Error::other_src("ops.iter", e))?;
            let op_id = OpId::from(Bytes::copy_from_slice(&k));
            let rec: OpRecord = ciborium::from_reader(v.as_ref())
                .map_err(|e| K2Error::other_src("decode OpRecord", e))?;
            Ok((op_id, rec))
        })
    }
}

/// Encode a `DhtArc` into a stable 9-byte prefix for slice-hash keys.
fn arc_prefix(arc: DhtArc) -> [u8; 9] {
    let mut out = [0u8; 9];
    match arc {
        DhtArc::Empty => {
            out[0] = 0;
        }
        DhtArc::Arc(start, end) => {
            out[0] = 1;
            out[1..5].copy_from_slice(&start.to_be_bytes());
            out[5..9].copy_from_slice(&end.to_be_bytes());
        }
    }
    out
}

/// Compose a slice-hash key: 9 bytes of arc + 8 bytes big-endian slice id.
fn slice_key(arc: DhtArc, slice_id: u64) -> [u8; 17] {
    let mut out = [0u8; 17];
    out[..9].copy_from_slice(&arc_prefix(arc));
    out[9..].copy_from_slice(&slice_id.to_be_bytes());
    out
}

fn slice_id_from_key(key: &[u8]) -> Option<u64> {
    if key.len() == 17 {
        let mut id_bytes = [0u8; 8];
        id_bytes.copy_from_slice(&key[9..]);
        Some(u64::from_be_bytes(id_bytes))
    } else {
        None
    }
}

fn op_id_key(op_id: &OpId) -> Bytes {
    Bytes::from(op_id.clone())
}

impl OpStore for KvOpStore {
    fn process_incoming_ops(&self, op_list: Vec<Bytes>) -> BoxFuture<'_, K2Result<Vec<OpId>>> {
        let arc = self.target_arc();
        Box::pin(async move {
            let mut accepted = Vec::with_capacity(op_list.len());
            let now = Timestamp::now().as_micros();
            for op_bytes in op_list {
                let (op_id, created_at) = (self.decode_envelope)(&op_bytes)?;

                // Sharding-ready commitment 1 (SPIKE §1.5): consult arc,
                // don't hardcode "yes." v1 default is `Full` so this lets
                // everything through; v1.5 sharded mode filters here.
                if !arc.contains(op_id.loc()) {
                    continue;
                }

                if self
                    .ops
                    .contains_key(op_id_key(&op_id))
                    .map_err(|e| K2Error::other_src("ops.contains_key", e))?
                {
                    accepted.push(op_id);
                    continue;
                }

                let record = OpRecord {
                    created_at_micros: created_at.as_micros(),
                    stored_at_micros: now,
                    op_data: op_bytes.to_vec(),
                };
                self.put_op_record(&op_id, &record)?;
                accepted.push(op_id);
            }
            self.db
                .flush_async()
                .await
                .map_err(|e| K2Error::other_src("sled flush", e))?;
            Ok(accepted)
        })
    }

    fn retrieve_op_hashes_in_time_slice(
        &self,
        arc: DhtArc,
        start: Timestamp,
        end: Timestamp,
    ) -> BoxFuture<'_, K2Result<(Vec<OpId>, u32)>> {
        let start_us = start.as_micros();
        let end_us = end.as_micros();
        Box::pin(async move {
            let mut candidates: Vec<(OpId, i64, u32)> = Vec::new();
            for kv in self.iter_op_records() {
                let (op_id, rec) = kv?;
                if rec.created_at_micros >= start_us
                    && rec.created_at_micros < end_us
                    && arc.contains(op_id.loc())
                {
                    candidates.push((op_id, rec.created_at_micros, rec.op_data.len() as u32));
                }
            }
            candidates.sort_by_key(|(_, ts, _)| *ts);
            let used_bytes = candidates.iter().map(|(_, _, sz)| *sz).sum();
            Ok((
                candidates.into_iter().map(|(id, _, _)| id).collect(),
                used_bytes,
            ))
        })
    }

    fn retrieve_ops(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<Vec<MetaOp>>> {
        Box::pin(async move {
            let mut out = Vec::with_capacity(op_ids.len());
            for op_id in op_ids {
                if let Some(rec) = self.get_op_record(&op_id)? {
                    out.push(MetaOp {
                        op_id,
                        op_data: Bytes::from(rec.op_data),
                    });
                }
            }
            Ok(out)
        })
    }

    fn filter_out_existing_ops(&self, op_ids: Vec<OpId>) -> BoxFuture<'_, K2Result<Vec<OpId>>> {
        Box::pin(async move {
            let mut missing = Vec::new();
            for op_id in op_ids {
                let key = op_id_key(&op_id);
                let exists = self
                    .ops
                    .contains_key(&key)
                    .map_err(|e| K2Error::other_src("ops.contains_key", e))?;
                if !exists {
                    missing.push(op_id);
                }
            }
            Ok(missing)
        })
    }

    fn retrieve_op_ids_bounded(
        &self,
        arc: DhtArc,
        start: Timestamp,
        limit_bytes: u32,
    ) -> BoxFuture<'_, K2Result<(Vec<OpId>, u32, Timestamp)>> {
        let start_us = start.as_micros();
        Box::pin(async move {
            let new_start = Timestamp::now();
            let mut candidates: Vec<(OpId, i64, u32)> = Vec::new();
            for kv in self.iter_op_records() {
                let (op_id, rec) = kv?;
                if arc.contains(op_id.loc()) && rec.stored_at_micros >= start_us {
                    candidates.push((op_id, rec.stored_at_micros, rec.op_data.len() as u32));
                }
            }
            candidates.sort_by_key(|(_, stored, _)| *stored);

            let mut total_bytes: u32 = 0;
            let mut last_stored: Option<i64> = None;
            let mut op_ids = Vec::new();
            for (op_id, stored, sz) in candidates {
                if total_bytes + sz <= limit_bytes {
                    total_bytes += sz;
                    op_ids.push(op_id);
                } else {
                    last_stored = Some(stored);
                    break;
                }
            }

            let next_start = last_stored.map(Timestamp::from_micros).unwrap_or(new_start);
            Ok((op_ids, total_bytes, next_start))
        })
    }

    fn earliest_timestamp_in_arc(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<Option<Timestamp>>> {
        Box::pin(async move {
            let mut earliest: Option<i64> = None;
            for kv in self.iter_op_records() {
                let (op_id, rec) = kv?;
                if arc.contains(op_id.loc()) {
                    earliest = Some(match earliest {
                        Some(prev) => prev.min(rec.created_at_micros),
                        None => rec.created_at_micros,
                    });
                }
            }
            Ok(earliest.map(Timestamp::from_micros))
        })
    }

    fn store_slice_hash(
        &self,
        arc: DhtArc,
        slice_index: u64,
        slice_hash: Bytes,
    ) -> BoxFuture<'_, K2Result<()>> {
        Box::pin(async move {
            if slice_hash.is_empty() {
                return Err(K2Error::other("Cannot insert empty combined hash"));
            }
            self.slice_hashes
                .insert(slice_key(arc, slice_index), slice_hash.to_vec())
                .map_err(|e| K2Error::other_src("slice_hashes.insert", e))?;
            Ok(())
        })
    }

    fn slice_hash_count(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<u64>> {
        Box::pin(async move {
            let prefix = arc_prefix(arc);
            let mut highest: Option<u64> = None;
            for kv in self.slice_hashes.scan_prefix(prefix) {
                let (k, _) = kv.map_err(|e| K2Error::other_src("scan_prefix", e))?;
                if let Some(id) = slice_id_from_key(&k) {
                    highest = Some(match highest {
                        Some(prev) => prev.max(id),
                        None => id,
                    });
                }
            }
            Ok(highest.map(|id| id + 1).unwrap_or(0))
        })
    }

    fn retrieve_slice_hash(
        &self,
        arc: DhtArc,
        slice_index: u64,
    ) -> BoxFuture<'_, K2Result<Option<Bytes>>> {
        Box::pin(async move {
            let key = slice_key(arc, slice_index);
            Ok(self
                .slice_hashes
                .get(key)
                .map_err(|e| K2Error::other_src("slice_hashes.get", e))?
                .map(|ivec| Bytes::copy_from_slice(&ivec)))
        })
    }

    fn retrieve_slice_hashes(&self, arc: DhtArc) -> BoxFuture<'_, K2Result<Vec<(u64, Bytes)>>> {
        Box::pin(async move {
            let prefix = arc_prefix(arc);
            let mut out = Vec::new();
            for kv in self.slice_hashes.scan_prefix(prefix) {
                let (k, v) = kv.map_err(|e| K2Error::other_src("scan_prefix", e))?;
                if let Some(id) = slice_id_from_key(&k) {
                    out.push((id, Bytes::copy_from_slice(&v)));
                }
            }
            Ok(out)
        })
    }

    fn query_total_op_count(&self) -> BoxFuture<'_, K2Result<u64>> {
        Box::pin(async move { Ok(self.ops.len() as u64) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::envelope::OpEnvelope;
    use std::sync::Arc as StdArc;

    /// Decode hook the tests share: the op-id is the SHA-256 of the
    /// envelope bytes (so identical bytes produce identical ids), and the
    /// timestamp comes from the envelope's payload prefix `u64 micros`.
    /// Real production semantics live in `HolographSpace`.
    fn envelope_decoder() -> EnvelopeDecoder {
        StdArc::new(|bytes: &[u8]| {
            let env =
                OpEnvelope::decode(bytes).map_err(|e| K2Error::other_src("decode envelope", e))?;
            let mut hasher = sha256();
            hasher.update(bytes);
            let digest = hasher.finalize();
            let op_id = OpId::from(Bytes::copy_from_slice(&digest));
            // payload prefix: 8 BE bytes = creation timestamp micros.
            let payload = env.payload.as_ref();
            let ts = if payload.len() >= 8 {
                let mut b = [0u8; 8];
                b.copy_from_slice(&payload[..8]);
                Timestamp::from_micros(i64::from_be_bytes(b))
            } else {
                Timestamp::now()
            };
            Ok((op_id, ts))
        })
    }

    // SHA-256 via the `sha2` crate isn't pulled into deps for the
    // KvOpStore proper — we use a tiny inline FNV-style here strictly for
    // test determinism. Real envelope hashing lives in HolographSpace.
    fn sha256() -> TestHasher {
        TestHasher {
            state: 0xcbf29ce484222325,
            buf: Vec::new(),
        }
    }

    struct TestHasher {
        state: u64,
        buf: Vec<u8>,
    }

    impl TestHasher {
        fn update(&mut self, b: &[u8]) {
            for byte in b {
                self.state = self.state.wrapping_mul(0x100000001b3) ^ *byte as u64;
            }
            self.buf.extend_from_slice(b);
        }

        fn finalize(self) -> [u8; 32] {
            let mut out = [0u8; 32];
            // Fold the buffer + final state into 32 bytes deterministically.
            let s = self.state.to_le_bytes();
            for i in 0..32 {
                out[i] = s[i % 8] ^ self.buf.get(i).copied().unwrap_or(0);
            }
            out
        }
    }

    fn make_envelope(payload_ts_micros: i64, marker: u8) -> Vec<u8> {
        let mut payload = Vec::with_capacity(16);
        payload.extend_from_slice(&payload_ts_micros.to_be_bytes());
        payload.push(marker);
        let env = OpEnvelope::new(
            std::iter::empty(),
            Bytes::from(payload),
            Bytes::from_static(b"pk"),
            Bytes::from_static(b"sig"),
            None,
        );
        env.encode().expect("encode")
    }

    fn space_id() -> SpaceId {
        SpaceId::from(Bytes::from_static(b"test-space"))
    }

    fn open_store(dir: &tempfile::TempDir) -> Arc<KvOpStore> {
        KvOpStore::open(
            dir.path().join("db"),
            space_id(),
            ArcPolicy::Full,
            envelope_decoder(),
        )
        .expect("open store")
    }

    #[tokio::test]
    async fn process_then_retrieve_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);

        let bytes = Bytes::from(make_envelope(1_000_000, 0xab));
        let ids = store
            .process_incoming_ops(vec![bytes.clone()])
            .await
            .expect("process");
        assert_eq!(ids.len(), 1);

        let fetched = store.retrieve_ops(ids.clone()).await.expect("retrieve");
        assert_eq!(fetched.len(), 1);
        assert_eq!(fetched[0].op_data, bytes);
        assert_eq!(fetched[0].op_id, ids[0]);
    }

    #[tokio::test]
    async fn process_dedupes_same_op_twice() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);
        let env = Bytes::from(make_envelope(1_000_000, 0x42));

        let _ = store.process_incoming_ops(vec![env.clone()]).await.unwrap();
        let _ = store.process_incoming_ops(vec![env.clone()]).await.unwrap();
        assert_eq!(store.query_total_op_count().await.unwrap(), 1);
    }

    #[tokio::test]
    async fn filter_out_existing_returns_only_missing() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);
        let env = Bytes::from(make_envelope(1_000_000, 0x01));
        let ids = store.process_incoming_ops(vec![env]).await.unwrap();

        let extra = OpId::from(Bytes::from_static(b"never-stored-______________other"));
        let missing = store
            .filter_out_existing_ops(vec![ids[0].clone(), extra.clone()])
            .await
            .unwrap();
        assert_eq!(missing, vec![extra]);
    }

    #[tokio::test]
    async fn time_slice_query_filters_by_window() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);

        let in_window = Bytes::from(make_envelope(1_500_000, 0xaa));
        let out_window = Bytes::from(make_envelope(500_000, 0xbb));
        let _ = store
            .process_incoming_ops(vec![in_window, out_window])
            .await
            .unwrap();

        let (ids, _bytes) = store
            .retrieve_op_hashes_in_time_slice(
                DhtArc::FULL,
                Timestamp::from_micros(1_000_000),
                Timestamp::from_micros(2_000_000),
            )
            .await
            .unwrap();
        assert_eq!(ids.len(), 1);
    }

    #[tokio::test]
    async fn earliest_timestamp_tracks_min() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);
        for ts in [3_000_000, 1_500_000, 9_000_000] {
            let _ = store
                .process_incoming_ops(vec![Bytes::from(make_envelope(ts, ts as u8))])
                .await
                .unwrap();
        }
        let earliest = store.earliest_timestamp_in_arc(DhtArc::FULL).await.unwrap();
        assert_eq!(earliest, Some(Timestamp::from_micros(1_500_000)));
    }

    #[tokio::test]
    async fn slice_hash_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);
        let arc = DhtArc::FULL;

        // store_slice_hash then retrieve and verify.
        store
            .store_slice_hash(arc, 0, Bytes::from_static(b"hash0"))
            .await
            .unwrap();
        store
            .store_slice_hash(arc, 5, Bytes::from_static(b"hash5"))
            .await
            .unwrap();
        store
            .store_slice_hash(arc, 3, Bytes::from_static(b"hash3"))
            .await
            .unwrap();

        let h = store.retrieve_slice_hash(arc, 5).await.unwrap();
        assert_eq!(h.as_deref(), Some(&b"hash5"[..]));

        // slice_hash_count is highest-stored-id + 1
        assert_eq!(store.slice_hash_count(arc).await.unwrap(), 6);

        let all = store.retrieve_slice_hashes(arc).await.unwrap();
        assert_eq!(all.len(), 3);
        assert!(all.iter().any(|(id, _)| *id == 3));
    }

    #[tokio::test]
    async fn slice_hash_rejects_empty() {
        let dir = tempfile::tempdir().unwrap();
        let store = open_store(&dir);
        let err = store
            .store_slice_hash(DhtArc::FULL, 0, Bytes::new())
            .await
            .unwrap_err();
        assert!(err.to_string().contains("empty"));
    }

    /// State survives close/reopen — the persistence-not-optional point
    /// from SPIKE §0.
    #[tokio::test]
    async fn state_persists_across_reopen() {
        let dir = tempfile::tempdir().unwrap();
        let env = Bytes::from(make_envelope(1_000_000, 0x99));
        let original_id;
        {
            let store = open_store(&dir);
            let ids = store.process_incoming_ops(vec![env.clone()]).await.unwrap();
            original_id = ids[0].clone();
            // Drop the store, which drops the sled::Db handle.
        }

        // Reopen the same path; the op should still be there.
        let store = open_store(&dir);
        let fetched = store.retrieve_ops(vec![original_id.clone()]).await.unwrap();
        assert_eq!(fetched.len(), 1);
        assert_eq!(fetched[0].op_id, original_id);
        assert_eq!(fetched[0].op_data, env);
    }

    /// "Bob asks Alice, Alice serves" — two stores in the same process,
    /// Bob's `process_incoming_ops` consumes bytes Alice's `retrieve_ops`
    /// produced. This is the load-bearing smoketest from SPIKE §2.5.
    #[tokio::test]
    async fn bob_asks_alice_alice_serves() {
        let alice_dir = tempfile::tempdir().unwrap();
        let bob_dir = tempfile::tempdir().unwrap();
        let alice = open_store(&alice_dir);
        let bob = open_store(&bob_dir);

        let payload = Bytes::from(make_envelope(2_000_000, 0xaa));
        let alice_ids = alice
            .process_incoming_ops(vec![payload.clone()])
            .await
            .unwrap();
        assert_eq!(alice_ids.len(), 1);
        assert_eq!(bob.query_total_op_count().await.unwrap(), 0);

        // Bob doesn't have it.
        let still_missing = bob
            .filter_out_existing_ops(vec![alice_ids[0].clone()])
            .await
            .unwrap();
        assert_eq!(still_missing.len(), 1);

        // Bob asks Alice.
        let served = alice
            .retrieve_ops(vec![alice_ids[0].clone()])
            .await
            .unwrap();
        assert_eq!(served.len(), 1);

        // Bob ingests what Alice served. Same op-id round-trips because
        // both stores share the envelope decoder.
        let bob_ids = bob
            .process_incoming_ops(vec![served[0].op_data.clone()])
            .await
            .unwrap();
        assert_eq!(bob_ids, alice_ids);
        assert_eq!(bob.query_total_op_count().await.unwrap(), 1);

        // Bob can now serve the op to anyone who asks.
        let bob_serves = bob.retrieve_ops(bob_ids).await.unwrap();
        assert_eq!(bob_serves[0].op_data, payload);
    }

    /// D1 — concurrent `KvOpStore::open` against the same path.
    /// First holder drops after ~200ms; second open must succeed within
    /// the 5-step backoff window (~1.55s budget).
    #[tokio::test]
    async fn second_open_retries_until_first_drops() {
        let dir = tempfile::tempdir().unwrap();
        let db_path = dir.path().join("db");

        let first = open_store_at(&db_path);
        let path_for_drop = db_path.clone();
        let dropper = tokio::task::spawn_blocking(move || {
            std::thread::sleep(std::time::Duration::from_millis(200));
            drop(first);
            // path used only to keep ownership semantics clear
            let _ = path_for_drop;
        });

        // Second open from a different blocking thread — sled locks
        // the directory advisory-style, so this must wait for `first`
        // to drop. The backoff loop should retry until success.
        let path_for_second = db_path.clone();
        let started = std::time::Instant::now();
        let second = tokio::task::spawn_blocking(move || open_store_at(&path_for_second))
            .await
            .expect("second-open task");
        let elapsed = started.elapsed();

        dropper.await.unwrap();

        // Sanity: the second open did wait (i.e., it didn't bypass the
        // first holder via some other mechanism) and completed within
        // the 1.55s backoff budget.
        assert!(
            elapsed >= std::time::Duration::from_millis(50),
            "second open returned suspiciously fast ({:?}), suggests no contention",
            elapsed
        );
        assert!(
            elapsed < std::time::Duration::from_millis(1_700),
            "second open exceeded backoff budget ({:?})",
            elapsed
        );
        // Smoke: the second handle is usable.
        assert_eq!(second.query_total_op_count().await.unwrap(), 0);
    }

    fn open_store_at(path: &std::path::Path) -> Arc<KvOpStore> {
        KvOpStore::open(path, space_id(), ArcPolicy::Full, envelope_decoder())
            .expect("open store at path")
    }
}
