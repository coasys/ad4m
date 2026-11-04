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
//! The entries are bounded by the events they cover ([`Settings::held_log_events`]); the least
//! recently used go first. A log longer than the bound is folded on every use and never held.
//!
//! [`Settings::held_log_events`]: crate::Settings::held_log_events

use std::collections::{BTreeMap, HashMap};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use ad4m_hash_chain::store::SqliteChainStore;
use ad4m_hash_chain::Head;
use ad4m_identity_core::KelChain;
use rusqlite::Connection;

use crate::db::{self, KEL};
use crate::error::ApiError;
use crate::Store;

/// The held folds, least recently used first out.
pub struct HeldLogs {
    inner: Mutex<Inner>,
    /// The most events the held logs cover together.
    max_events: usize,
    /// How many times a log was folded from the database (tests and operators read it).
    folds: AtomicU64,
}

#[derive(Default)]
struct Inner {
    logs: HashMap<String, Slot>,
    /// Use order: tick → scid. The first is the least recently used.
    order: BTreeMap<u64, String>,
    tick: u64,
    events: usize,
}

struct Slot {
    chain: Arc<KelChain>,
    tick: u64,
    events: usize,
}

impl Inner {
    fn remove(&mut self, scid: &str) {
        if let Some(slot) = self.logs.remove(scid) {
            self.order.remove(&slot.tick);
            self.events -= slot.events;
        }
    }

    fn next_tick(&mut self) -> u64 {
        self.tick += 1;
        self.tick
    }
}

fn events_of(chain: &KelChain) -> usize {
    usize::try_from(chain.head().seq)
        .unwrap_or(usize::MAX)
        .saturating_add(1)
}

impl HeldLogs {
    pub fn new(max_events: usize) -> HeldLogs {
        HeldLogs {
            inner: Mutex::new(Inner::default()),
            max_events,
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
    /// recently used folds until the bound holds. A log longer than the bound is not held.
    pub(crate) fn put(&self, scid: &str, chain: Arc<KelChain>) {
        let Ok(mut inner) = self.inner.lock() else {
            return;
        };
        inner.remove(scid);
        let events = events_of(&chain);
        if events > self.max_events {
            return;
        }
        while inner.events + events > self.max_events {
            let Some((_, oldest)) = inner.order.pop_first() else {
                break;
            };
            if let Some(slot) = inner.logs.remove(&oldest) {
                inner.events -= slot.events;
            }
        }
        let tick = inner.next_tick();
        inner.order.insert(tick, scid.to_string());
        inner.events += events;
        inner.logs.insert(
            scid.to_string(),
            Slot {
                chain,
                tick,
                events,
            },
        );
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

    /// How many events the held folds cover together.
    pub fn held_events(&self) -> usize {
        self.inner.lock().map(|inner| inner.events).unwrap_or(0)
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
