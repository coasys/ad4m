//! `identity-store`: the shared store of `did:scid:coasys:1` key logs with the offers and
//! claims of device linking, the device inbox, the private details record and the request box.
//!
//! One SQLite file holds the hash-chain module's tables (`chain_heads`, `chain_entries`,
//! `chain_forks`, type `coasys:kel`) and the store's own (`logs`, `heads`, `keys`, `agents`,
//! `nonces`, `feed`, `offers`, `claims`, `fork_signers`, `inbox`, `records`, `requests`,
//! `buckets`). The store replays every event with the identity core before it appends, and
//! commits each append with its head, key-index rows, feed items, the claim it spends and the
//! inbox messages and requests it ends in one transaction (the one-transaction rule of
//! `identity-store/AGENTS.md`). It can withhold an event, never forge one: every client checks
//! the events' own signatures again. Of inbox messages and records it keeps only ciphertext.
//!
//! The store has no brake buckets, vouching notes or sign-up pairing yet:
//! see [`log::admit_new_log`] for where the brake attaches.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, Mutex};
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
    waits: Mutex<Waits>,
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
            waits: Mutex::new(Waits::default()),
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

    /// Wakes the reads waiting on `topics` after a committed write that concerns them.
    pub fn changed(&self, topics: &[Topic]) {
        let Ok(waits) = self.waits.lock() else {
            return;
        };
        for topic in topics {
            if let Some((sender, _)) = waits.topics.get(topic) {
                sender.send_modify(|n| *n = n.wrapping_add(1));
            }
        }
    }

    /// How many reads wait on `topic` now.
    #[cfg(test)]
    fn waiting(&self, topic: &Topic) -> usize {
        self.waits
            .lock()
            .ok()
            .and_then(|w| w.topics.get(topic).map(|(_, count)| *count))
            .unwrap_or(0)
    }

    /// A place among the waiters of `topic`, or `None` when the topic already has
    /// [`MAX_WAITERS_PER_TOPIC`] or the store [`MAX_WAITERS`].
    fn join(&self, topic: &Topic) -> Option<Waiter<'_>> {
        let mut waits = self.waits.lock().ok()?;
        if waits.total >= MAX_WAITERS {
            return None;
        }
        let (sender, count) = waits
            .topics
            .entry(topic.clone())
            .or_insert_with(|| (watch::channel(0).0, 0));
        if *count >= MAX_WAITERS_PER_TOPIC {
            return None;
        }
        *count += 1;
        let rx = sender.subscribe();
        waits.total += 1;
        Some(Waiter {
            waits: &self.waits,
            topic: topic.clone(),
            rx,
        })
    }

    /// Calls `probe` now and after every committed write that concerns `topic` until it
    /// answers `Some`, for at most `wait_s` seconds; then probes once more and answers what it
    /// gives. A read over the waiter caps does not wait: it answers the first probe.
    pub async fn wait_for<T>(
        &self,
        topic: Topic,
        wait_s: u64,
        mut probe: impl FnMut() -> Result<Option<T>, ApiError>,
    ) -> Result<Option<T>, ApiError> {
        let Some(mut waiter) = self.join(&topic) else {
            return probe();
        };
        let deadline = Instant::now() + Duration::from_secs(wait_s);
        let rx = &mut waiter.rx;
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

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicUsize;

    use super::*;

    fn store() -> Arc<Store> {
        Store::new(
            db::Db::open_in_memory().unwrap(),
            StoreKey::from_secret([7; 32]),
            Arc::new(|| 1_800_000_000),
            Settings::default(),
        )
    }

    /// A waiter on `topic` that counts its probes and never finds anything.
    fn count_probes(
        store: &Arc<Store>,
        topic: Topic,
        wait_s: u64,
    ) -> (Arc<AtomicUsize>, tokio::task::JoinHandle<()>) {
        let probes = Arc::new(AtomicUsize::new(0));
        let (store, counted) = (store.clone(), probes.clone());
        let task = tokio::spawn(async move {
            store
                .wait_for(topic, wait_s, || {
                    counted.fetch_add(1, Ordering::SeqCst);
                    Ok(None::<()>)
                })
                .await
                .unwrap();
        });
        (probes, task)
    }

    async fn settle() {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    #[tokio::test]
    async fn a_waiter_probes_again_only_on_writes_to_its_topic() {
        let store = store();
        let (probes, task) = count_probes(&store, Topic::offers("A"), 8);
        settle().await;
        assert_eq!(probes.load(Ordering::SeqCst), 1);
        for _ in 0..5 {
            store.changed(&[Topic::offers("B"), Topic::key("A")]);
        }
        settle().await;
        assert_eq!(probes.load(Ordering::SeqCst), 1);
        store.changed(&[Topic::offers("A")]);
        settle().await;
        assert_eq!(probes.load(Ordering::SeqCst), 2);
        task.abort();
        let _ = task.await;
        assert_eq!(store.waiting(&Topic::offers("A")), 0);
    }

    #[tokio::test]
    async fn a_waiter_over_the_cap_answers_at_once_and_leaving_frees_the_place() {
        let store = store();
        let topic = Topic::key("z6Mk");
        let tasks: Vec<_> = (0..MAX_WAITERS_PER_TOPIC)
            .map(|_| count_probes(&store, topic.clone(), 8).1)
            .collect();
        settle().await;
        assert_eq!(store.waiting(&topic), MAX_WAITERS_PER_TOPIC);
        let started = Instant::now();
        let (probes, over) = count_probes(&store, topic.clone(), 8);
        over.await.unwrap();
        assert!(started.elapsed() < Duration::from_secs(1));
        assert_eq!(probes.load(Ordering::SeqCst), 1);
        // Another topic still waits.
        let (_, other) = count_probes(&store, Topic::key("z6Other"), 8);
        settle().await;
        assert_eq!(store.waiting(&Topic::key("z6Other")), 1);
        other.abort();
        for task in tasks {
            task.abort();
            let _ = task.await;
        }
        let _ = other.await;
        assert_eq!(store.waiting(&topic), 0);
        assert_eq!(store.waits.lock().unwrap().total, 0);
    }
}

/// The longest `wait` a read accepts: a language's `httpFetch` gives up after 10 s.
pub const MAX_WAIT_S: u64 = 8;
/// How often `serve` runs [`Store::sweep`].
pub const SWEEP_EVERY_S: u64 = 60;
/// The most reads that wait on one topic (one identity's offers, one key, one inbox) at a time. The
/// routes that wait are unauthenticated, so the cap is per thing waited on, not per caller:
/// behind a proxy every caller has one address. A read over the cap answers at once.
pub const MAX_WAITERS_PER_TOPIC: usize = 16;
/// The most reads that wait across the store at a time.
pub const MAX_WAITERS: usize = 4096;

/// What a waiting read watches, and what a write wakes.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Topic {
    /// The offers of one identity: an offer opened, claimed, cancelled, expired, spent or
    /// superseded, or the log frozen by a fork.
    Offers(String),
    /// One signing key: an append that lists it.
    Key(String),
    /// One inbox address (`pre:<a>` or `<scid>/<keyId>`): a message written to it.
    Inbox(String),
}

impl Topic {
    pub fn offers(scid: &str) -> Topic {
        Topic::Offers(scid.to_string())
    }

    pub fn key(multikey: &str) -> Topic {
        Topic::Key(multikey.to_string())
    }

    pub fn inbox(address: &str) -> Topic {
        Topic::Inbox(address.to_string())
    }
}

#[derive(Default)]
struct Waits {
    /// Per topic: the change signal and how many reads wait on it.
    topics: HashMap<Topic, (watch::Sender<u64>, usize)>,
    total: usize,
}

/// One waiting read's place; leaving drops the topic when it was the last.
struct Waiter<'a> {
    waits: &'a Mutex<Waits>,
    topic: Topic,
    rx: watch::Receiver<u64>,
}

impl Drop for Waiter<'_> {
    fn drop(&mut self) {
        let Ok(mut waits) = self.waits.lock() else {
            return;
        };
        waits.total = waits.total.saturating_sub(1);
        let last = match waits.topics.get_mut(&self.topic) {
            Some((_, count)) => {
                *count = count.saturating_sub(1);
                *count == 0
            }
            None => false,
        };
        if last {
            waits.topics.remove(&self.topic);
        }
    }
}
