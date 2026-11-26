use crate::types::PerspectiveHandle;
use arc_swap::ArcSwap;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Immutable handle snapshots. Only writers queue; readers never take the mutex.
pub struct PersistedHandle {
    current: ArcSwap<PerspectiveHandle>,
    write: Mutex<()>,
    /// Writers queued for `write`, so a test can wait for a change to reach the lock instead of
    /// sleeping.
    #[cfg(test)]
    queued_writers: std::sync::atomic::AtomicUsize,
}

impl PersistedHandle {
    pub fn new(handle: PerspectiveHandle) -> Self {
        Self {
            current: ArcSwap::from_pointee(handle),
            write: Mutex::new(()),
            #[cfg(test)]
            queued_writers: std::sync::atomic::AtomicUsize::new(0),
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
        #[cfg(test)]
        self.queued_writers
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let _write = self.write.lock().await;
        #[cfg(test)]
        self.queued_writers
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        let mut handle = self.snapshot();
        let result = change(&mut handle);
        self.current.store(Arc::new(handle));
        result
    }

    #[cfg(test)]
    pub(crate) fn try_hold_writer(&self) -> Option<tokio::sync::MutexGuard<'_, ()>> {
        self.write.try_lock().ok()
    }

    /// Wait until `n` writers are queued for the writer lock. No sleeps: it yields between
    /// checks, and a test bounds it with a timeout.
    #[cfg(test)]
    pub(crate) async fn writers_queued(&self, n: usize) {
        while self
            .queued_writers
            .load(std::sync::atomic::Ordering::SeqCst)
            < n
        {
            tokio::task::yield_now().await;
        }
    }

    /// Pause handle mutations without preventing snapshot reads.
    #[cfg(test)]
    pub(crate) async fn hold_writer(&self) -> tokio::sync::MutexGuard<'_, ()> {
        self.write.lock().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::{future::join_all, FutureExt};

    #[tokio::test]
    async fn readers_do_not_queue_behind_a_burst_of_handle_writes() {
        const WRITERS: usize = 64;
        const READERS: usize = 64;
        let persisted = Arc::new(PersistedHandle::new(PerspectiveHandle {
            name: Some("0".into()),
            owners: Some(vec![]),
            ..Default::default()
        }));
        let before = persisted.load();
        let held = persisted.hold_writer().await;
        let writes = join_all((0..WRITERS).map(|i| {
            let persisted = persisted.clone();
            async move {
                persisted
                    .update(|h| {
                        h.add_owner(&format!("did:test:writer-{i}"));
                        h.name = Some(h.owners.as_ref().unwrap().len().to_string());
                    })
                    .await;
                tokio::task::yield_now().await;
            }
        }));
        tokio::pin!(writes);
        // Poll every writer into the queue before reading. No sleeps or deadlines.
        assert!(writes.as_mut().now_or_never().is_none());
        let reads = join_all((0..READERS).map(|_| {
            let persisted = persisted.clone();
            async move {
                // The same owned snapshot used by send_broadcast.
                let handle = persisted.snapshot();
                assert_eq!(handle.name.as_deref(), Some("0"));
                assert!(handle.owners.unwrap().is_empty());
            }
        }));
        reads
            .now_or_never()
            .expect("reads complete with the writer mutex held");
        drop(held);

        let readers = join_all((0..READERS).map(|_| {
            let persisted = persisted.clone();
            async move {
                for _ in 0..WRITERS {
                    let handle = persisted.load();
                    let count = handle.owners.as_ref().unwrap().len();
                    assert_eq!(handle.name.as_ref().unwrap(), &count.to_string());
                    tokio::task::yield_now().await;
                }
            }
        }));
        tokio::join!(writes, readers);
        let after = persisted.load();
        assert_eq!(after.owners.as_ref().unwrap().len(), WRITERS);
        for i in 0..WRITERS {
            assert!(after.is_owned_by(&format!("did:test:writer-{i}")));
        }
        assert!(
            before.owners.as_ref().unwrap().is_empty(),
            "snapshots stay immutable"
        );
    }
}
