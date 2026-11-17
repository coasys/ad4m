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

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use tokio::sync::{Mutex as AsyncMutex, OwnedMutexGuard};

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
}

impl MountSlot {
    /// Whether a run that started at `generation` may still apply.
    pub(crate) fn is_current(&self, generation: u64) -> bool {
        self.phase == MountPhase::Active && self.generation == generation
    }
}

/// Every mount slot of one perspective (`PerspectiveInstance::mount_lifecycles`).
#[derive(Debug, Default)]
pub(crate) struct MountLifecycles {
    slots: Mutex<HashMap<String, Arc<AsyncMutex<MountSlot>>>>,
    generations: AtomicU64,
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
                }))
            })
            .clone();
        slot.lock_owned().await
    }

    /// Move `slot` to `phase` under a new generation; returns it.
    pub(crate) fn enter(&self, slot: &mut MountSlot, phase: MountPhase) -> u64 {
        slot.phase = phase;
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
