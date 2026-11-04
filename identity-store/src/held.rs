//! The folded logs the store holds in memory, so a signed request, an offer or an append checks
//! against a log's state without replaying the log.
//!
//! Each entry is the folded log at one stored head. [`Store::held_chain`] reads the stored head
//! (one primary-key lookup of `chain_heads`) and answers the entry only when it ends at exactly
//! that head; an entry for any other head is dropped and the log folds once from
//! `chain_entries`. So an entry can be stale only in memory, never in an answer: whatever writes
//! the log, the head check catches it. An append puts its new fold in once its transaction has
//! committed (`log.rs`), so the next request finds the new head's state at once.
//!
//! The entries are bounded by the heap their folds take together ([`Settings::held_log_bytes`],
//! each fold sized by [`held_bytes_of`]); the least recently used go first. A fold larger than
//! the bound is never held: its log is folded on every use. The per-log budget
//! ([`Settings::log_fold_bytes`], at most the bound) keeps every log the store takes below it,
//! and a miss folds with the core's `fold_stored` (`db::load_chain_before`), no signature
//! checked. The bound is in bytes, not events,
//! because one event can cost the heap a hundred times another: an `incept` lists as many keys
//! as its 64 kB body holds, and a `scope` may name its actions many times over.
//!
//! [`Settings::held_log_bytes`]: crate::Settings::held_log_bytes
//! [`Settings::log_fold_bytes`]: crate::Settings::log_fold_bytes

use std::collections::{BTreeMap, HashMap};
use std::mem::size_of;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use ad4m_hash_chain::store::SqliteChainStore;
use ad4m_hash_chain::Head;
use ad4m_identity_core::{BotScope, KelChain, KeyRecord};
use rusqlite::Connection;

use crate::db::{self, KEL};
use crate::error::ApiError;
use crate::Store;

/// The held folds, least recently used first out.
pub struct HeldLogs {
    inner: Mutex<Inner>,
    /// The most heap the held folds take together, by [`held_bytes_of`].
    max_bytes: usize,
    /// How many times a log was folded from the database (tests and operators read it).
    folds: AtomicU64,
}

#[derive(Default)]
struct Inner {
    logs: HashMap<String, Slot>,
    /// Use order: tick → scid. The first is the least recently used.
    order: BTreeMap<u64, String>,
    tick: u64,
    bytes: usize,
}

struct Slot {
    chain: Arc<KelChain>,
    tick: u64,
    bytes: usize,
}

impl Inner {
    fn remove(&mut self, scid: &str) {
        if let Some(slot) = self.logs.remove(scid) {
            self.order.remove(&slot.tick);
            self.bytes -= slot.bytes;
        }
    }

    fn next_tick(&mut self) -> u64 {
        self.tick += 1;
        self.tick
    }
}

/// The heap a held fold of `chain` takes, approximately: every key record (masters and device
/// keys) with its strings and bot scopes, every event hash, and the fold's fixed part. Vectors
/// of the state count their capacity; the hash list, whose capacity is private, counts twice
/// its headers (a vector grown by doubling holds at most that). It reads the state once, as a
/// fold or an append already has. `tests/held_memory.rs` checks it against a counting
/// allocator: never below the heap a fold allocates, and within twice it.
pub fn held_bytes_of(chain: &KelChain) -> usize {
    /// A string's bytes, rounded up to the allocator's 16-byte grain.
    fn bytes(s: &str) -> usize {
        s.len().next_multiple_of(16)
    }
    fn scope(scope: &BotScope) -> usize {
        scope.can.capacity() * size_of::<String>()
            + scope.can.iter().map(|c| bytes(c)).sum::<usize>()
    }
    /// A record's heap, its own slot not included.
    fn record(r: &KeyRecord) -> usize {
        bytes(&r.id)
            + bytes(&r.sign)
            + r.enc.as_ref().map_or(0, |e| bytes(e))
            + r.scopes.capacity() * size_of::<(u64, BotScope)>()
            + r.scopes.iter().map(|(_, s)| scope(s)).sum::<usize>()
    }
    let state = chain.state();
    let masters = state.masters.capacity() * size_of::<KeyRecord>()
        + state.masters.iter().map(record).sum::<usize>();
    // A device key sits in a B-tree under its id: the entry, the key's bytes, and a node's
    // spare room (a node can be half empty).
    let entry = (size_of::<String>() + size_of::<KeyRecord>()) * 2;
    let keys = state
        .keys
        .iter()
        .map(|(id, r)| entry + bytes(id) + record(r))
        .sum::<usize>();
    let hashes = usize::try_from(chain.head().seq)
        .unwrap_or(usize::MAX)
        .saturating_add(1)
        .saturating_mul(2 * size_of::<String>() + bytes(&chain.head().hash));
    let fixed = size_of::<KelChain>() + bytes(&state.did) + bytes(&state.scid) * 2 + 256;
    masters + keys + hashes + fixed
}

impl HeldLogs {
    pub fn new(max_bytes: usize) -> HeldLogs {
        HeldLogs {
            inner: Mutex::new(Inner::default()),
            max_bytes,
            folds: AtomicU64::new(0),
        }
    }

    /// The held fold of `scid` when it ends at `head`. A fold for another head is dropped.
    fn get(&self, scid: &str, head: &Head) -> Option<Arc<KelChain>> {
        let mut inner = self.inner.lock().ok()?;
        let tick = inner.next_tick();
        let inner = &mut *inner;
        let slot = inner.logs.get_mut(scid)?;
        if slot.chain.head() != *head {
            inner.remove(scid);
            return None;
        }
        inner.order.remove(&slot.tick);
        slot.tick = tick;
        inner.order.insert(tick, scid.to_string());
        Some(slot.chain.clone())
    }

    /// Holds `chain` as the fold of `scid`, in place of any earlier one, and drops the least
    /// recently used folds until the bound holds. A fold larger than the bound is not held.
    pub(crate) fn put(&self, scid: &str, chain: Arc<KelChain>) {
        let bytes = held_bytes_of(&chain);
        let Ok(mut inner) = self.inner.lock() else {
            return;
        };
        inner.remove(scid);
        if bytes > self.max_bytes {
            return;
        }
        while inner.bytes + bytes > self.max_bytes {
            let Some((_, oldest)) = inner.order.pop_first() else {
                break;
            };
            if let Some(slot) = inner.logs.remove(&oldest) {
                inner.bytes -= slot.bytes;
            }
        }
        let tick = inner.next_tick();
        inner.order.insert(tick, scid.to_string());
        inner.bytes += bytes;
        inner
            .logs
            .insert(scid.to_string(), Slot { chain, tick, bytes });
    }

    /// Counts a replay of a log, or of a prefix of one, from the database.
    pub(crate) fn count_fold(&self) {
        self.folds.fetch_add(1, Ordering::SeqCst);
    }

    /// How many times a log (or a prefix of one, for a fork) was replayed from the database
    /// since the store started.
    pub fn folds(&self) -> u64 {
        self.folds.load(Ordering::SeqCst)
    }

    /// Whether a fold of `scid` is held now.
    pub fn holds(&self, scid: &str) -> bool {
        self.inner
            .lock()
            .map(|inner| inner.logs.contains_key(scid))
            .unwrap_or(false)
    }

    /// How much heap the held folds take together, by [`held_bytes_of`].
    pub fn held_bytes(&self) -> usize {
        self.inner.lock().map(|inner| inner.bytes).unwrap_or(0)
    }
}

impl Store {
    /// The stored log of `scid`, folded: the held fold when it ends at the stored head, else one
    /// fold from `chain_entries` that is then held. `None` when the store holds no log of `scid`.
    /// Read it inside the caller's transaction, before that transaction writes any event.
    pub(crate) fn held_chain(
        &self,
        conn: &Connection,
        scid: &str,
    ) -> Result<Option<Arc<KelChain>>, ApiError> {
        let Some(head) = SqliteChainStore::head_in(conn, KEL, &scid.to_string())? else {
            return Ok(None);
        };
        if let Some(chain) = self.held.get(scid, &head) {
            return Ok(Some(chain));
        }
        self.held.count_fold();
        let chain = db::load_chain(conn, scid)?
            .ok_or_else(|| ApiError::internal(format!("log {scid} has a head and no events")))?;
        if chain.head() != head {
            return Err(ApiError::internal(format!(
                "log {scid} does not end at its stored head"
            )));
        }
        let chain = Arc::new(chain);
        self.held.put(scid, chain.clone());
        Ok(Some(chain))
    }
}
