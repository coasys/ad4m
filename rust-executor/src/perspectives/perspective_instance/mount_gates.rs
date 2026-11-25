//! Test seam for the mount engine's races: a test arms a gate at one
//! point of one language's mount work, the first run that reaches it
//! stops there, and the test lets it go when it has run what it wants
//! in between. Compiled to nothing outside tests.

/// Where a mount run can be stopped.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MountPoint {
    /// `sync_mount`, after `mountSnapshot()` answered, before the apply.
    SnapshotFetched,
    /// The first mount, after its snapshot read and before any writes.
    InitialSnapshotFetched,
    /// A delta (pushed or a call's result), before it is applied.
    DeltaReceived,
    /// The per-record diff, after it read the record's current links and
    /// before it writes.
    EntryRead,
    /// The shape install, after the class ownership check and before the
    /// claim and the install.
    OwnershipChecked,
}

/// Stop here if a test armed a gate for `(lang, point)`.
pub(crate) async fn pause(lang: &str, point: MountPoint) {
    #[cfg(test)]
    gates::reach(lang, point).await;
    #[cfg(not(test))]
    let _ = (lang, point);
}

#[cfg(test)]
pub(crate) mod gates {
    use super::MountPoint;
    use std::sync::{Arc, Mutex};
    use tokio::sync::Semaphore;

    /// One armed stop: `arrived` gets a permit when a run reaches it,
    /// `release` lets that run go on.
    pub(crate) struct Gate {
        arrived: Semaphore,
        release: Semaphore,
    }

    impl Gate {
        /// Wait until a run stops at the gate.
        pub(crate) async fn arrived(&self) {
            self.arrived.acquire().await.unwrap().forget();
        }

        /// Let the stopped run go on.
        pub(crate) fn release(&self) {
            self.release.add_permits(1);
        }
    }

    static ARMED: Mutex<Vec<(String, MountPoint, Arc<Gate>)>> = Mutex::new(Vec::new());

    /// Arm a gate: the next run of `lang`'s mount work to reach `point`
    /// stops there (later runs pass).
    pub(crate) fn arm(lang: &str, point: MountPoint) -> Arc<Gate> {
        let gate = Arc::new(Gate {
            arrived: Semaphore::new(0),
            release: Semaphore::new(0),
        });
        ARMED
            .lock()
            .unwrap()
            .push((lang.to_string(), point, gate.clone()));
        gate
    }

    pub(super) async fn reach(lang: &str, point: MountPoint) {
        let gate = {
            let mut armed = ARMED.lock().unwrap();
            let Some(i) = armed.iter().position(|(l, p, _)| l == lang && *p == point) else {
                return;
            };
            armed.remove(i).2
        };
        gate.arrived.add_permits(1);
        gate.release.acquire().await.unwrap().forget();
    }
}
