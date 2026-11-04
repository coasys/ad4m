//! `identity-store`: the shared store of `did:scid:coasys:1` key logs with the offers and
//! claims of device linking.
//!
//! One SQLite file holds the hash-chain module's tables (`chain_heads`, `chain_entries`,
//! `chain_forks`, type `coasys:kel`) and the store's own (`logs`, `heads`, `keys`, `agents`,
//! `nonces`, `feed`, `offers`, `claims`, `fork_signers`). The store replays every event with the identity core
//! before it appends, and commits each append with its head, key-index rows, feed items and
//! the claim it spends in one transaction (the one-transaction rule of
//! `identity-store/AGENTS.md`). It can withhold an event, never forge one: every client checks
//! the events' own signatures again.
//!
//! The store has no brake buckets, vouching notes or sign-up pairing yet:
//! see [`log::admit_new_log`] for where the brake attaches.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use tokio::sync::{watch, Notify};

pub mod agents;
pub mod db;
pub mod error;
pub mod head;
pub mod held;
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
    /// `POW_BITS`: the proof-of-work cost published at `GET /v1/pow` (default 22). Without a
    /// brake a new log needs no proof; a proof that is sent must meet it.
    pub pow_bits: u8,
    /// `FEED_TOKEN`: the mirror token of `GET /v1/feed`. None: the feed answers `401` to all.
    pub feed_token: Option<String>,
    /// `CORS_ORIGINS`: the origins allowed to call from a browser (the auth domain, the web app).
    pub cors_origins: Vec<String>,
    /// `HELD_LOG_EVENTS`: the most events the logs held folded in memory cover together
    /// (default 1 048 576; `held.rs`). A log longer than this is folded on every use.
    pub held_log_events: usize,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            pow_bits: 22,
            feed_token: None,
            cors_origins: Vec::new(),
            held_log_events: DEFAULT_HELD_LOG_EVENTS,
        }
    }
}

/// The default of [`Settings::held_log_events`].
pub const DEFAULT_HELD_LOG_EVENTS: usize = 1 << 20;

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
    /// The folded logs held in memory (`held.rs`).
    pub held: held::HeldLogs,
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
            held: held::HeldLogs::new(settings.held_log_events),
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

    /// Wakes the reads waiting on `topics` after a committed write that concerns them (the
    /// device's state poll with the identity's offers).
    pub fn changed(&self, topics: &[Topic]) {
        let Ok(waits) = self.waits.lock() else {
            return;
        };
        let wake = |topic: &Topic| {
            if let Some(waiting) = waits.topics.get(topic) {
                waiting.signal.send_modify(|n| *n = n.wrapping_add(1));
            }
        };
        for topic in topics {
            wake(topic);
            if let Topic::Offers(scid) = topic {
                wake(&Topic::OfferState(scid.clone()));
            }
        }
    }

    /// How many reads wait in places on `topic` now.
    #[cfg(test)]
    fn waiting(&self, topic: &Topic) -> usize {
        self.waits
            .lock()
            .ok()
            .and_then(|w| w.topics.get(topic).map(|t| t.waiters.len()))
            .unwrap_or(0)
    }

    /// A place among the waiters of `topic`, or `None` when the store already has
    /// [`MAX_WAITERS`]. On a topic with [`MAX_WAITERS_PER_TOPIC`] the oldest waiter gives up its
    /// place (it answers at once) and the new one takes it.
    fn join(&self, topic: &Topic) -> Option<Waiter<'_>> {
        let mut guard = self.waits.lock().ok()?;
        let waits = &mut *guard;
        if waits.total >= MAX_WAITERS {
            return None;
        }
        let id = waits.next_id;
        waits.next_id += 1;
        let waiting = waits
            .topics
            .entry(topic.clone())
            .or_insert_with(|| TopicWaits {
                signal: watch::channel(0).0,
                waiters: VecDeque::new(),
            });
        if waiting.waiters.len() >= MAX_WAITERS_PER_TOPIC {
            if let Some((_, oldest)) = waiting.waiters.pop_front() {
                oldest.notify_one();
            }
        }
        let displaced = Arc::new(Notify::new());
        waiting.waiters.push_back((id, displaced.clone()));
        let rx = waiting.signal.subscribe();
        waits.total += 1;
        Some(Waiter {
            waits: &self.waits,
            topic: topic.clone(),
            id,
            rx,
            displaced,
        })
    }

    /// Calls `probe` now and after every committed write that concerns `topic` until it
    /// answers `Some`, for at most `wait_s` seconds; then probes once more and answers what it
    /// gives. A read over the store's waiter cap does not wait: it answers the first probe. A
    /// read displaced from a full topic by a newer one probes once more and answers.
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
        let displaced = waiter.displaced.clone();
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
            tokio::select! {
                changed = tokio::time::timeout(deadline - now, rx.changed()) => match changed {
                    Ok(Ok(())) => continue,
                    _ => return probe(),
                },
                // A newer read took this place: answer what there is now.
                _ = displaced.notified() => return probe(),
            }
        }
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
    async fn a_waiter_on_a_full_topic_takes_the_oldest_place_and_leaving_frees_it() {
        let store = store();
        let topic = Topic::key("z6Mk");
        let (oldest_probes, oldest) = count_probes(&store, topic.clone(), 8);
        settle().await;
        let tasks: Vec<_> = (1..MAX_WAITERS_PER_TOPIC)
            .map(|_| count_probes(&store, topic.clone(), 8).1)
            .collect();
        settle().await;
        assert_eq!(store.waiting(&topic), MAX_WAITERS_PER_TOPIC);
        // A newer read waits; the oldest gives up its place and answers at once, after one more
        // probe.
        let started = Instant::now();
        let (newest_probes, newest) = count_probes(&store, topic.clone(), 8);
        oldest.await.unwrap();
        assert!(started.elapsed() < Duration::from_secs(1));
        assert_eq!(oldest_probes.load(Ordering::SeqCst), 2);
        settle().await;
        assert_eq!(store.waiting(&topic), MAX_WAITERS_PER_TOPIC);
        assert_eq!(newest_probes.load(Ordering::SeqCst), 1);
        // The newest is woken by a write like any other.
        store.changed(std::slice::from_ref(&topic));
        settle().await;
        assert_eq!(newest_probes.load(Ordering::SeqCst), 2);
        // Another topic still waits.
        let (_, other) = count_probes(&store, Topic::key("z6Other"), 8);
        settle().await;
        assert_eq!(store.waiting(&Topic::key("z6Other")), 1);
        other.abort();
        newest.abort();
        for task in tasks {
            task.abort();
            let _ = task.await;
        }
        let _ = other.await;
        let _ = newest.await;
        assert_eq!(store.waiting(&topic), 0);
        assert_eq!(store.waits.lock().unwrap().total, 0);
    }

    #[tokio::test]
    async fn the_device_poll_never_takes_the_owners_places() {
        let store = store();
        let owner = Topic::offers("A");
        let polls = Topic::offer_state("A");
        let (owner_probes, owner_task) = count_probes(&store, owner.clone(), 8);
        settle().await;
        let squatters: Vec<_> = (0..2 * MAX_WAITERS_PER_TOPIC)
            .map(|_| count_probes(&store, polls.clone(), 8).1)
            .collect();
        settle().await;
        assert_eq!(store.waiting(&owner), 1);
        assert_eq!(store.waiting(&polls), MAX_WAITERS_PER_TOPIC);
        assert_eq!(owner_probes.load(Ordering::SeqCst), 1);
        // A write to the identity's offers wakes both.
        store.changed(std::slice::from_ref(&owner));
        settle().await;
        assert_eq!(owner_probes.load(Ordering::SeqCst), 2);
        owner_task.abort();
        let _ = owner_task.await;
        for task in squatters {
            task.abort();
            let _ = task.await;
        }
        assert_eq!(store.waits.lock().unwrap().total, 0);
    }

    #[tokio::test]
    async fn the_store_wide_cap_refuses_a_place_on_any_topic_until_one_leaves() {
        let store = store();
        let mut places: Vec<_> = (0..MAX_WAITERS)
            .map(|i| {
                store
                    .join(&Topic::key(&format!("z6Mk{i}")))
                    .expect("a place under the cap")
            })
            .collect();
        assert_eq!(store.waits.lock().unwrap().total, MAX_WAITERS);
        // A read on a fresh topic gets no place: it answers its first probe at once.
        assert!(store.join(&Topic::key("z6Fresh")).is_none());
        let started = Instant::now();
        let (probes, over) = count_probes(&store, Topic::key("z6Fresh"), 8);
        over.await.unwrap();
        assert!(started.elapsed() < Duration::from_secs(1));
        assert_eq!(probes.load(Ordering::SeqCst), 1);
        // One leaves: the next read gets its place.
        drop(places.pop());
        assert!(store.join(&Topic::key("z6Fresh")).is_some());
        assert_eq!(store.waits.lock().unwrap().total, MAX_WAITERS - 1);
    }
}

/// The longest `wait` a read accepts: a language's `httpFetch` gives up after 10 s.
pub const MAX_WAIT_S: u64 = 8;
/// The most reads that wait on one topic (one identity's offers, one key) at a time. The
/// state poll and the key lookup are unauthenticated, so the cap is per thing waited on, not
/// per caller: behind a proxy every caller has one address. A read that finds its topic full
/// takes the place of the oldest, which answers at once (as with `wait=0`): squatters who fill
/// a topic cannot shut out a later read, they only shorten each other's waits.
pub const MAX_WAITERS_PER_TOPIC: usize = 16;
/// The most reads that wait across the store at a time.
pub const MAX_WAITERS: usize = 4096;

/// What a waiting read watches, and what a write wakes.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Topic {
    /// The offers of one identity: an offer opened, claimed, cancelled, expired, spent or
    /// superseded, or the log frozen by a fork. The owner's master-signed list waits here.
    Offers(String),
    /// The same changes, for the device's unauthenticated state poll: its own places, so
    /// pollers can never take the owner's.
    OfferState(String),
    /// One signing key: an append that lists it.
    Key(String),
}

impl Topic {
    pub fn offers(scid: &str) -> Topic {
        Topic::Offers(scid.to_string())
    }

    pub fn offer_state(scid: &str) -> Topic {
        Topic::OfferState(scid.to_string())
    }

    pub fn key(multikey: &str) -> Topic {
        Topic::Key(multikey.to_string())
    }
}

#[derive(Default)]
struct Waits {
    /// Per topic: the change signal and the reads that wait on it.
    topics: HashMap<Topic, TopicWaits>,
    /// Reads waiting across the store, displaced ones included until they answer.
    total: usize,
    next_id: u64,
}

struct TopicWaits {
    signal: watch::Sender<u64>,
    /// The reads in their places, oldest first: each one's id and the signal that lets it go.
    waiters: VecDeque<(u64, Arc<Notify>)>,
}

/// One waiting read's place; leaving drops the topic when it was the last.
struct Waiter<'a> {
    waits: &'a Mutex<Waits>,
    topic: Topic,
    id: u64,
    rx: watch::Receiver<u64>,
    /// Fires when a newer read on a full topic takes this read's place.
    displaced: Arc<Notify>,
}

impl Drop for Waiter<'_> {
    fn drop(&mut self) {
        let Ok(mut waits) = self.waits.lock() else {
            return;
        };
        waits.total = waits.total.saturating_sub(1);
        let last = match waits.topics.get_mut(&self.topic) {
            Some(topic) => {
                topic.waiters.retain(|(id, _)| *id != self.id);
                topic.waiters.is_empty()
            }
            None => false,
        };
        if last {
            waits.topics.remove(&self.topic);
        }
    }
}
