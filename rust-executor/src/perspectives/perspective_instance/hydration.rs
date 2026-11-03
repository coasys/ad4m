//! Lazy loading: deferred perspectives and their hydration.
//!
//! A multi-tenant executor (shared DB backend) does not open every stored perspective at
//! start-up. It registers each one deferred ([`PerspectiveInstance::new_deferred`]): an
//! empty in-memory store, no background task. The first access through the API
//! ([`crate::api::perspectives_ws::get_perspective_with_access`], the gate of every
//! perspective method) hydrates it ([`PerspectiveInstance::hydrate`]): download its archive
//! from the snapshot backend, open the persistent store, run the start-up checks
//! ([`crate::perspectives::prepare_opened_store`]), swap the store in, start the background
//! tasks.

use super::PerspectiveInstance;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::PerspectiveHandle;
use std::sync::atomic::Ordering;
use std::sync::{Arc, PoisonError};

/// Where perspective `uuid` keeps its persistent store: `{app data}/perspectives/{uuid}`.
/// `None` without an app data path; the store is in memory then.
pub(crate) fn store_data_path(uuid: &str) -> Option<String> {
    crate::perspectives::get_app_data_path().map(|base| {
        std::path::PathBuf::from(&base)
            .join("perspectives")
            .join(uuid)
            .to_string_lossy()
            .to_string()
    })
}

impl PerspectiveInstance {
    /// A deferred perspective: an empty in-memory store and no background task until
    /// [`Self::hydrate`].
    pub fn new_deferred(handle: PerspectiveHandle) -> Self {
        let store = SparqlStore::new(None).expect("Failed to create in-memory SPARQL store");
        Self::with_store(handle, None, store, false)
    }

    /// The perspective's SPARQL store now. Hold it for one operation: a hydration swaps
    /// the store, and the one a reader holds stays alive until it drops it.
    pub fn store(&self) -> Arc<SparqlStore> {
        self.store_slot.load_full()
    }

    /// False while the perspective is deferred (see [`Self::new_deferred`]).
    pub fn is_hydrated(&self) -> bool {
        self.hydrated.load(Ordering::Acquire)
    }

    /// Put `store` in the slot, between writes, and drop the shapes read from the store it
    /// replaces.
    fn replace_store(&self, store: Arc<SparqlStore>) {
        {
            let _write = self
                .persist_lock
                .lock()
                .unwrap_or_else(PoisonError::into_inner);
            self.store_slot.store(store);
        }
        self.clear_shape_cache();
    }

    /// Open a deferred perspective's persistent store and start its background tasks. A
    /// no-op once hydrated; concurrent calls open the store once.
    ///
    /// With snapshots on ([`crate::perspective_snapshot::snapshots_enabled`]), the
    /// perspective's archive is downloaded and extracted first. A failed download, a store
    /// that does not open, or failed start-up checks leave the perspective deferred, so the
    /// next access tries again; it never serves (or backs up) an empty store in place of
    /// one it could not fetch.
    pub async fn hydrate(&self) -> Result<(), String> {
        if self.is_hydrated() {
            return Ok(());
        }
        let _hydrating = self.hydration_lock.lock().await;
        if self.is_hydrated() {
            return Ok(());
        }

        let uuid = self.uuid.clone();
        let store = tokio::task::spawn_blocking(move || open_persistent_store(&uuid))
            .await
            .map_err(|e| format!("Hydrating perspective {}: task failed: {}", self.uuid, e))??;

        let deferred = self.store();
        self.replace_store(Arc::new(store));
        if let Err(e) = crate::perspectives::prepare_opened_store(self).await {
            self.replace_store(deferred);
            return Err(format!("Not opening perspective {}: {}", self.uuid, e));
        }
        self.hydrated.store(true, Ordering::Release);
        log::info!("✅ Hydrated perspective {}", self.uuid);

        tokio::spawn(self.clone().start_background_tasks());
        Ok(())
    }

    /// Run `f` while the perspective stays deferred, holding its hydration off; `None`
    /// (without running `f`) once it is hydrated. Blocking: call it off the async runtime.
    pub(crate) fn while_deferred<R>(&self, f: impl FnOnce() -> R) -> Option<R> {
        let _hydrating = self.hydration_lock.blocking_lock();
        if self.is_hydrated() {
            None
        } else {
            Some(f())
        }
    }

    /// Test seam: replace the store and mark the perspective hydrated, without opening
    /// anything or starting background tasks.
    #[cfg(test)]
    pub(crate) fn swap_store(&self, store: SparqlStore) {
        self.replace_store(Arc::new(store));
        self.hydrated.store(true, Ordering::Release);
    }
}

/// Fetch perspective `uuid`'s archive when snapshots are on, then open its persistent
/// store. Blocking: the download is a blocking HTTP call.
fn open_persistent_store(uuid: &str) -> Result<SparqlStore, String> {
    let config = crate::config::GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .clone()
        .filter(crate::perspective_snapshot::snapshots_enabled);
    if let Some(config) = config {
        match crate::perspective_snapshot::restore_perspective_archive(&config, uuid) {
            Ok(true) => log::info!("📥 Downloaded archive for perspective {}", uuid),
            Ok(false) => log::info!("No remote archive for {}: opening its local store", uuid),
            Err(e) => {
                return Err(format!(
                    "Hydrating perspective {}: archive download failed: {}",
                    uuid, e
                ))
            }
        }
    }
    SparqlStore::new(store_data_path(uuid).as_deref()).map_err(|e| {
        format!(
            "Hydrating perspective {}: its store did not open: {}",
            uuid, e
        )
    })
}
