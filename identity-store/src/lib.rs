//! `identity-store`: the shared store of `did:scid:coasys:1` key logs (AR-01 4.5) with the
//! offers and claims of device linking (AR-02 4.3).
//!
//! One SQLite file holds the hash-chain module's tables (`chain_heads`, `chain_entries`,
//! `chain_forks`, type `coasys:kel`) and the store's own (`logs`, `heads`, `keys`, `agents`,
//! `nonces`, `feed`, `offers`, `claims`). The store replays every event with the identity core
//! before it appends, and commits each append with its head, key-index rows, feed items and
//! the claim it spends in one transaction (rule A6). It can withhold an event, never forge one:
//! every client checks the events' own signatures again.
//!
//! Slice 1 leaves out the brake's buckets, vouching notes and the sign-up pairing (slice 3):
//! see [`log::admit_new_log`] for where the brake attaches.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use tokio::sync::watch;

pub mod agents;
pub mod db;
pub mod error;
pub mod head;
pub mod log;
pub mod offers;
pub mod request;
pub mod routes;

pub use error::ApiError;
pub use head::StoreKey;
pub use routes::router;

/// The store's clock in unix seconds. Tests inject their own.
pub type Clock = Arc<dyn Fn() -> u64 + Send + Sync>;

/// The system clock.
pub fn system_clock() -> Clock {
    Arc::new(|| {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0)
    })
}

/// What an operator configures.
#[derive(Clone, Debug)]
pub struct Settings {
    /// `POW_BITS`: the proof-of-work cost published at `GET /v1/pow` (default 22). In slice 1 a
    /// new log needs no proof; a proof that is sent must meet it.
    pub pow_bits: u8,
    /// `FEED_TOKEN`: the mirror token of `GET /v1/feed`. None: the feed answers `401` to all.
    pub feed_token: Option<String>,
    /// `CORS_ORIGINS`: the origins allowed to call from a browser (the auth domain, the web app).
    pub cors_origins: Vec<String>,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            pow_bits: 22,
            feed_token: None,
            cors_origins: Vec::new(),
        }
    }
}

/// A point inside a write where a test makes the store fail, to check that the write's
/// transaction leaves nothing behind (AR-01 R18, AR-02 R7).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Fault {
    /// No fault.
    None = 0,
    /// In an append, after the events are inserted and before the key index, the feed, the
    /// spent claims and the head.
    AfterEvents = 1,
}

/// The running store: its database, head key, clock and settings.
pub struct Store {
    pub db: db::Db,
    pub key: StoreKey,
    pub settings: Settings,
    clock: Clock,
    started_at: u64,
    changes: watch::Sender<u64>,
    fault: AtomicU8,
}

impl Store {
    pub fn new(db: db::Db, key: StoreKey, clock: Clock, settings: Settings) -> Arc<Store> {
        let started_at = clock();
        Arc::new(Store {
            db,
            key,
            settings,
            clock,
            started_at,
            changes: watch::channel(0).0,
            fault: AtomicU8::new(Fault::None as u8),
        })
    }

    /// Makes every later write fail at `fault` (`Fault::None` clears it). For tests.
    pub fn inject_fault(&self, fault: Fault) {
        self.fault.store(fault as u8, Ordering::SeqCst);
    }

    /// Fails the running write when a test injected `at`.
    pub(crate) fn fault_point(&self, at: Fault) -> Result<(), ApiError> {
        if self.fault.load(Ordering::SeqCst) == at as u8 {
            return Err(ApiError::internal(format!("injected fault {at:?}")));
        }
        Ok(())
    }

    /// Now by the store's clock (unix seconds).
    pub fn now(&self) -> u64 {
        (self.clock)()
    }

    /// When the store started (the `changedAt` of `GET /v1/pow`).
    pub fn started_at(&self) -> u64 {
        self.started_at
    }

    /// Wakes every waiting read after a committed write.
    pub fn changed(&self) {
        self.changes.send_modify(|n| *n = n.wrapping_add(1));
    }

    /// Calls `probe` now and after every committed write until it answers `Some`, for at most
    /// `wait_s` seconds; then probes once more and answers what it gives.
    pub async fn wait_for<T>(
        &self,
        wait_s: u64,
        mut probe: impl FnMut() -> Result<Option<T>, ApiError>,
    ) -> Result<Option<T>, ApiError> {
        let deadline = Instant::now() + Duration::from_secs(wait_s);
        let mut rx = self.changes.subscribe();
        loop {
            rx.borrow_and_update();
            if let Some(found) = probe()? {
                return Ok(Some(found));
            }
            let now = Instant::now();
            if now >= deadline {
                return Ok(None);
            }
            match tokio::time::timeout(deadline - now, rx.changed()).await {
                Ok(Ok(())) => continue,
                _ => return probe(),
            }
        }
    }
}

/// The longest `wait` a read accepts: a language's `httpFetch` gives up after 10 s.
pub const MAX_WAIT_S: u64 = 8;
