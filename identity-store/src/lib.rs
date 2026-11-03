//! `identity-store`: the shared store of `did:scid:coasys:1` key logs with the offers and
//! claims of device linking, the device inbox, the private details record and the request box.
//!
//! One SQLite file holds the hash-chain module's tables (`chain_heads`, `chain_entries`,
//! `chain_forks`, type `coasys:kel`) and the store's own (`logs`, `heads`, `keys`, `agents`,
//! `nonces`, `feed`, `offers`, `claims`, `inbox`, `records`, `requests`, `buckets`). The store
//! replays every event with the identity core before it appends, and commits each append with
//! its head, key-index rows, feed items, the claim it spends and the inbox messages and
//! requests it ends in one transaction (the one-transaction rule of
//! `identity-store/AGENTS.md`). It can withhold an event, never forge one: every client checks
//! the events' own signatures again. Of inbox messages and records it keeps only ciphertext.
//!
//! The store has no brake buckets, vouching notes or sign-up pairing yet:
//! see [`log::admit_new_log`] for where the brake attaches.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use tokio::sync::watch;

pub mod agents;
pub mod db;
pub mod error;
pub mod head;
pub mod inbox;
pub mod limits;
pub mod log;
pub mod offers;
pub mod private;
pub mod request;
pub mod request_box;
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
    /// `POW_BITS`: the proof-of-work cost published at `GET /v1/pow` (default 22). Without a
    /// brake a new log needs no proof; a proof that is sent must meet it.
    pub pow_bits: u8,
    /// `FEED_TOKEN`: the mirror token of `GET /v1/feed`. None: the feed answers `401` to all.
    pub feed_token: Option<String>,
    /// `CORS_ORIGINS`: the origins allowed to call from a browser (the auth domain, the web app).
    pub cors_origins: Vec<String>,
    /// `IDENTITY_BURST`: writes per identity at once (offer opens, inbox writes, record writes,
    /// request posts), the identity bucket's size (default 5).
    pub identity_burst: u32,
    /// `IDENTITY_REFILL_S`: seconds per write the identity bucket refills (default 60).
    pub identity_refill_s: u64,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            pow_bits: 22,
            feed_token: None,
            cors_origins: Vec::new(),
            identity_burst: 5,
            identity_refill_s: 60,
        }
    }
}

/// A point inside a write where a test makes the store fail, to check that the write's
/// transaction leaves nothing behind (an append, a claim's spend).
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

    /// Deletes the inbox messages and requests whose time has passed, and nonces older than
    /// their window. `serve` runs it every [`SWEEP_EVERY_S`]; reads never answer an expired
    /// row in between. Answers the rows it deleted.
    pub fn sweep(&self) -> Result<usize, ApiError> {
        let now = db::to_sql(self.now())?;
        let conn = self.db.lock()?;
        let mut gone = conn.execute("DELETE FROM inbox WHERE expires <= ?1", [now])?;
        gone += conn.execute("DELETE FROM requests WHERE expires <= ?1", [now])?;
        let window = db::to_sql(request::NONCE_WINDOW_S)?;
        gone += conn.execute("DELETE FROM nonces WHERE seen_at <= ?1 - ?2", [now, window])?;
        Ok(gone)
    }
}

/// The longest `wait` a read accepts: a language's `httpFetch` gives up after 10 s.
pub const MAX_WAIT_S: u64 = 8;
/// How often `serve` runs [`Store::sweep`].
pub const SWEEP_EVERY_S: u64 = 60;
