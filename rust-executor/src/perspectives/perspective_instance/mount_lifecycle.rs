//! Each mount's lifecycle in this perspective: whether it is active, and
//! its generation. The mount link in the store is the persisted intent;
//! this is the in-memory state every apply checks. `mount`, `unmount` and
//! a restore move a mount between phases, and every move takes a new
//! generation.
//!
//! Each mount has one apply lock (its `MountSlot` mutex). Every write of
//! the mount — a snapshot, a pushed delta, a call's delta, the shape
//! install, the unmount — runs under it, and checks the generation it
//! started with before writing: a result fetched before an unmount finds
//! the generation moved on and is dropped, so the unmount wins. The lock
//! is never held across a call into a language: a run fetches from the
//! language first and takes the lock only to apply (a language whose
//! signal delivery waited on the lock would otherwise deadlock).
//!
//! Order: everything a mounted language reports carries a sequence number
//! (`next_mount_seq`) taken where its order is known — a pushed delta as
//! the language emits it, a call's delta as the call returns, a snapshot
//! before it is asked for. A delta applies to a record only when it is
//! newer than what the record last got (`seen`) and than the last applied
//! snapshot; a snapshot skips a record a newer delta wrote and is dropped
//! whole when a newer snapshot was applied. So an older delta never
//! regresses a record, and a snapshot read before a delta never sweeps
//! the delta's record.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use tokio::sync::{Mutex as AsyncMutex, OwnedMutexGuard};

/// The order of everything mounted languages report (see the module doc).
/// One counter for all languages; only the order matters.
static MOUNT_SEQ: AtomicU64 = AtomicU64::new(1);

/// The next sequence number for a delta or snapshot.
pub(crate) fn next_mount_seq() -> u64 {
    MOUNT_SEQ.fetch_add(1, Ordering::SeqCst)
}

/// Where a mount is in its lifecycle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MountPhase {
    /// Not mounted (never, or unmounted, or a mount link a restore has not
    /// picked up yet).
    Inactive,
    /// `mount` is fetching and installing; nothing else applies.
    Mounting,
    /// Mounted: syncs and deltas apply.
    Active,
}

/// One mount's state, behind its apply lock.
#[derive(Debug)]
pub(crate) struct MountSlot {
    pub(crate) phase: MountPhase,
    pub(crate) generation: u64,
    /// The sequence number of the last applied snapshot.
    last_snapshot: u64,
    /// Per record address, the sequence number of the delta that last
    /// wrote it, kept only while newer than `last_snapshot`.
    seen: HashMap<String, u64>,
}

impl MountSlot {
    /// Whether a run that started at `generation` may still apply.
    pub(crate) fn is_current(&self, generation: u64) -> bool {
        self.phase == MountPhase::Active && self.generation == generation
    }

    /// Whether a delta taken at `seq` is newer than what `address` holds.
    pub(crate) fn delta_applies(&self, address: &str, seq: u64) -> bool {
        seq > self.last_snapshot && self.seen.get(address).is_none_or(|s| seq > *s)
    }

    /// Record that a delta taken at `seq` wrote `address`.
    pub(crate) fn delta_applied(&mut self, address: &str, seq: u64) {
        self.seen.insert(address.to_string(), seq);
    }

    /// Whether a snapshot taken at `seq` is older than one already applied.
    pub(crate) fn snapshot_is_stale(&self, seq: u64) -> bool {
        seq < self.last_snapshot
    }

    /// Whether a delta newer than a snapshot taken at `seq` wrote `address`:
    /// the snapshot leaves it alone.
    pub(crate) fn newer_than_snapshot(&self, address: &str, seq: u64) -> bool {
        self.seen.get(address).is_some_and(|s| *s > seq)
    }

    /// Record that a snapshot taken at `seq` was applied.
    pub(crate) fn snapshot_applied(&mut self, seq: u64) {
        self.last_snapshot = seq;
        self.seen.retain(|_, s| *s > seq);
    }
}

/// Every mount slot of one perspective (`PerspectiveInstance::mount_lifecycles`).
#[derive(Debug, Default)]
pub(crate) struct MountLifecycles {
    slots: Mutex<HashMap<String, Arc<AsyncMutex<MountSlot>>>>,
    generations: AtomicU64,
    /// Held from a mount's class ownership check through its shape
    /// install (`install_mount_shapes`). Taken under a slot's lock, never
    /// the other way round.
    install: AsyncMutex<()>,
}

impl MountLifecycles {
    /// Take `lang`'s apply lock (a new slot starts `Inactive`).
    pub(crate) async fn lock(&self, lang: &str) -> OwnedMutexGuard<MountSlot> {
        let slot = self
            .slots
            .lock()
            .unwrap()
            .entry(lang.to_string())
            .or_insert_with(|| {
                Arc::new(AsyncMutex::new(MountSlot {
                    phase: MountPhase::Inactive,
                    generation: 0,
                    last_snapshot: 0,
                    seen: HashMap::new(),
                }))
            })
            .clone();
        slot.lock_owned().await
    }

    /// The perspective's shape install lock (see `install`).
    pub(crate) async fn install_lock(&self) -> tokio::sync::MutexGuard<'_, ()> {
        self.install.lock().await
    }

    /// Move `slot` to `phase` under a new generation; returns it.
    pub(crate) fn enter(&self, slot: &mut MountSlot, phase: MountPhase) -> u64 {
        slot.phase = phase;
        slot.last_snapshot = 0;
        slot.seen.clear();
        slot.generation = self.generations.fetch_add(1, Ordering::SeqCst) + 1;
        slot.generation
    }

    /// The generation of `lang`'s mount when it is active.
    pub(crate) async fn active_generation(&self, lang: &str) -> Option<u64> {
        let slot = self.lock(lang).await;
        (slot.phase == MountPhase::Active).then_some(slot.generation)
    }

    /// `lang`'s apply lock if its mount is still the one a run started
    /// with at `generation`; `None` when it was unmounted (or remounted)
    /// since.
    pub(crate) async fn lock_current(
        &self,
        lang: &str,
        generation: u64,
    ) -> Option<OwnedMutexGuard<MountSlot>> {
        let slot = self.lock(lang).await;
        slot.is_current(generation).then_some(slot)
    }
}
