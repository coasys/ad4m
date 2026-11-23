use crate::types::PerspectiveHandle;
use arc_swap::ArcSwap;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Immutable handle snapshots. Only writers queue; readers never take the mutex.
pub struct PersistedHandle {
    current: ArcSwap<PerspectiveHandle>,
    write: Mutex<()>,
}

impl PersistedHandle {
    pub fn new(handle: PerspectiveHandle) -> Self {
        Self {
            current: ArcSwap::from_pointee(handle),
            write: Mutex::new(()),
        }
    }

    pub fn load(&self) -> Arc<PerspectiveHandle> {
        self.current.load_full()
    }

    pub fn snapshot(&self) -> PerspectiveHandle {
        (*self.load()).clone()
    }

    /// Apply a synchronous change to the latest value, then publish it atomically.
    /// Keep ordered DB/cache writes in this closure. No await is allowed inside it.
    /// A returned error does not roll back the change (as with the old mutable guard).
    pub async fn update<R>(&self, change: impl FnOnce(&mut PerspectiveHandle) -> R) -> R {
        let _write = self.write.lock().await;
        let mut handle = self.snapshot();
        let result = change(&mut handle);
        self.current.store(Arc::new(handle));
        result
    }

    #[cfg(test)]
    pub(crate) fn try_hold_writer(&self) -> Option<tokio::sync::MutexGuard<'_, ()>> {
        self.write.try_lock().ok()
    }

    /// Pause handle mutations without preventing snapshot reads.
    #[cfg(test)]
    pub(crate) async fn hold_writer(&self) -> tokio::sync::MutexGuard<'_, ()> {
        self.write.lock().await
    }
}
