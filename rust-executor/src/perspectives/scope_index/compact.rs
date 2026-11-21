//! Compaction of scope heads after many removals.
//!
//! A removed entry leaves a tombstone in RocksDB until a compaction drops it.
//! Readers of a scope start at its newest end (the head), so tombstones there
//! slow every page read: a reverse scan steps over each of them. Removals at
//! the head are common (a message posted and deleted, a draft replaced).
//!
//! [`Compactor::note`] counts, per scope, removals of entries whose time is
//! within [`HEAD_WINDOW_MS`] of now. When one scope gets [`THRESHOLD`] of them
//! within [`PERIOD`], a background thread compacts that scope's entry range
//! with [`Store::compact_aux_range`]. One compaction per scope runs at a time,
//! and at most [`MAX_RUNNING`] per store; a scope that finds no free slot
//! keeps its count and tries again at its next head removal. A scope's count
//! is dropped once its period ends. On in-memory stores compaction does
//! nothing.

use oxigraph::store::Store;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex, Weak};
use std::time::{Duration, Instant};

use super::writer::{ScopeChange, ScopeDelta};
use super::{entry_prefix, prefix_end, ScopeId};

/// How far from now a removed entry counts as at the head.
pub const HEAD_WINDOW_MS: u64 = 7 * 24 * 60 * 60 * 1000;
/// Head removals in one scope that start a compaction.
pub const THRESHOLD: u32 = 256;
/// The period the removals must fall in.
pub const PERIOD: Duration = Duration::from_secs(60);
/// Compactions of one store that run at the same time.
pub const MAX_RUNNING: usize = 2;

#[derive(Default)]
pub struct Compactor {
    /// Per scope: head removals since `Instant`.
    counts: Mutex<HashMap<ScopeId, (u32, Instant)>>,
    running: Arc<Mutex<HashSet<ScopeId>>>,
    /// Compactions started, for tests and diagnostics.
    started: std::sync::atomic::AtomicU64,
}

impl Compactor {
    /// Count the head removals in `deltas` (from a committed transaction), and
    /// start a compaction for each scope that crossed the threshold. Returns
    /// the scopes it started compacting.
    pub fn note(&self, store: &Arc<Store>, deltas: &[ScopeDelta]) -> Vec<ScopeId> {
        self.note_at(store, deltas, now_ms(), Instant::now())
    }

    pub(crate) fn note_at(
        &self,
        store: &Arc<Store>,
        deltas: &[ScopeDelta],
        now_ms: u64,
        now: Instant,
    ) -> Vec<ScopeId> {
        let mut due = Vec::new();
        {
            let Ok(mut counts) = self.counts.lock() else {
                return due;
            };
            // A count whose period ended would start again at 0: drop it.
            counts.retain(|_, (_, since)| now.duration_since(*since) <= PERIOD);
            for d in deltas {
                if d.change != ScopeChange::Removed
                    || now_ms.saturating_sub(d.key.t_ms()) > HEAD_WINDOW_MS
                {
                    continue;
                }
                let entry = counts.entry(d.scope).or_insert((0, now));
                entry.0 += 1;
                if entry.0 >= THRESHOLD && !due.contains(&d.scope) {
                    due.push(d.scope);
                }
            }
        }
        due.retain(|scope| self.start(store, *scope));
        if let Ok(mut counts) = self.counts.lock() {
            for scope in &due {
                counts.remove(scope);
            }
        }
        due
    }

    /// Compact `scope`'s entries in a background thread, unless a compaction
    /// of it is running or [`MAX_RUNNING`] compactions are. Returns whether
    /// one started.
    fn start(&self, store: &Arc<Store>, scope: ScopeId) -> bool {
        {
            let Ok(mut running) = self.running.lock() else {
                return false;
            };
            if running.len() >= MAX_RUNNING || !running.insert(scope) {
                return false;
            }
        }
        self.started
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let weak: Weak<Store> = Arc::downgrade(store);
        let running = self.running.clone();
        let spawned = std::thread::Builder::new()
            .name("scope-index-compact".into())
            .spawn(move || {
                if let Some(store) = weak.upgrade() {
                    let prefix = entry_prefix(scope);
                    if let Err(e) = store.compact_aux_range(&prefix, &prefix_end(&prefix)) {
                        log::warn!("Scope index: compaction failed: {e}");
                    }
                }
                if let Ok(mut running) = running.lock() {
                    running.remove(&scope);
                }
            });
        if spawned.is_err() {
            if let Ok(mut running) = self.running.lock() {
                running.remove(&scope);
            }
            return false;
        }
        true
    }

    /// How many compactions this compactor started.
    pub fn started(&self) -> u64 {
        self.started.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// How many scopes have a count.
    #[cfg(test)]
    pub(crate) fn tracked(&self) -> usize {
        self.counts.lock().map(|c| c.len()).unwrap_or(0)
    }

    /// How many compactions run.
    #[cfg(test)]
    pub(crate) fn running(&self) -> usize {
        self.running.lock().map(|r| r.len()).unwrap_or(0)
    }
}

fn now_ms() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}
