//! `identity-store`: the shared store of `did:scid:coasys:1` key logs with the offers and
//! claims of device linking, the device inbox, the private details record and the request box.
//!
//! One SQLite file holds the hash-chain module's tables (`chain_heads`, `chain_entries`,
//! `chain_forks`, type `coasys:kel`) and the store's own (`logs`, `heads`, `keys`, `agents`,
//! `nonces`, `pow_seen`, `feed`, `offers`, `claims`, `fork_signers`, `inbox`, `records`,
//! `requests`, `buckets`, `brake`, `notes`). The store replays every event with the identity core before it appends, and
//! commits each append with its head, key-index rows, feed items, the claim it spends and the
//! inbox messages and requests it ends in one transaction (the one-transaction rule of
//! `identity-store/AGENTS.md`). It can withhold an event, never forge one: every client checks
//! the events' own signatures again. Of inbox messages and records it keeps only ciphertext.
//!
//! A new log is admitted by the brake (`brake.rs`: buckets per address and store-wide, a
//! failure bucket and the proof of work, all before any signature replay) or by a vouching note
//! the store redeems at the platform (`vouch.rs`). The store has no sign-up pairing yet.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use tokio::sync::{watch, Notify};

pub mod agents;
pub mod brake;
pub mod db;
pub mod error;
pub mod head;
pub mod held;
pub mod inbox;
pub mod limits;
pub mod log;
pub mod offers;
pub mod private;
pub mod request;
pub mod request_box;
pub mod routes;
pub mod vouch;

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
    /// `POW_BITS`: the proof-of-work cost published at `GET /v1/pow` (default 22) that a new log
    /// with no vouching note must meet (`brake.rs`). Each challenge carries the bits it was issued at, so after a
    /// change a challenge in flight keeps its old bits until it expires (300 s).
    pub pow_bits: u8,
    /// `BRAKE_GLOBAL_PER_HOUR`: no-note logs store-wide per hour, the global bucket's size and
    /// hourly refill (default 120).
    pub brake_global_per_hour: u32,
    /// `BRAKE_ADDR_BURST`: no-note logs per address at once, the address bucket's size
    /// (default 3).
    pub brake_addr_burst: u32,
    /// `BRAKE_ADDR_PER_DAY`: what the address bucket refills per 24 h (default 3).
    pub brake_addr_per_day: u32,
    /// `TRUSTED_PROXIES`: comma-separated addresses or ranges (`10.0.0.0/8`) of the proxies in
    /// front of the store. Only a request whose socket peer is one of them is read for its
    /// client's address, from the one header `client_ip_header` names. Default none —
    /// required when the store runs behind a proxy: without it every client counts as the
    /// proxy's one address for the brake, and the store says so on the first request that
    /// carries a proxy header (`Store::note_untrusted_proxy_headers`).
    pub trusted_proxies: Vec<brake::Cidr>,
    /// `CLIENT_IP_HEADER`: the one header a trusted proxy fills with the client's address:
    /// `x-forwarded-for` (default) or `cf-connecting-ip` (Cloudflare ingress). The other is
    /// never read: a proxy that passes it through would let a caller choose its own address.
    pub client_ip_header: brake::ClientIpHeader,
    /// `VOUCH_URL`: the vouching service's base URL (`POST {VOUCH_URL}/api/vouch/redeem`). None:
    /// every token gets `503 vouch-unavailable`.
    pub vouch_url: Option<String>,
    /// `VOUCH_CREDENTIAL`: the store's own credential at the vouching service, sent as a bearer
    /// token with every redeem.
    pub vouch_credential: Option<vouch::Credential>,
    /// `VOUCH_KEYS`: comma-separated multikeys of the vouching keys whose notes the store takes
    /// and reads as `vouched` (`GET {VOUCH_URL}/api/vouch/key` answers the current one).
    pub vouch_keys: Vec<String>,
    /// `FEED_TOKEN`: the mirror token of `GET /v1/feed`. None: the feed answers `401` to all.
    pub feed_token: Option<String>,
    /// `CORS_ORIGINS`: the origins allowed to call from a browser (the auth domain, the web app).
    pub cors_origins: Vec<String>,
    /// `IDENTITY_BURST`: writes per identity at once (offer opens, inbox writes, record writes,
    /// request posts), the identity bucket's size (default 10).
    pub identity_burst: u32,
    /// `IDENTITY_REFILL_S`: seconds per write the identity bucket refills (default 60).
    pub identity_refill_s: u64,
    /// `HELD_LOG_BYTES`: the most heap the logs held folded in memory take together, by
    /// `held::held_bytes_of` (default 256 MiB; `held.rs`). A log whose fold is larger is folded
    /// on every use. At least twice [`Settings::log_fold_bytes`] ([`Settings::check`]), so every
    /// log within its budget can be held.
    pub held_log_bytes: usize,
    /// `LOG_FOLD_BYTES`: the per-log budget (default 16 MiB), by `held::fold_bytes_of`. Two
    /// bounds: the live part of a log's fold (without the records of keys revoked at the head)
    /// at most this, and the whole fold at most twice this. An append past either is
    /// `400 log-too-large`, unless all its events are `rotate`s (the recovery from a stolen
    /// master is never refused by it). A thief can pad the live part to the budget; after the
    /// owner rotates and revokes the thief's keys, the live part drops and the owner appends
    /// again, up to the whole-fold bound. A log stolen and padded twice can reach that bound
    /// and stays full. It bounds what a miss in the held folds costs (the parse and hashing of
    /// one log) and keeps every log within it holdable.
    pub log_fold_bytes: usize,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            pow_bits: 22,
            brake_global_per_hour: 120,
            brake_addr_burst: 3,
            brake_addr_per_day: 3,
            trusted_proxies: Vec::new(),
            client_ip_header: brake::ClientIpHeader::default(),
            vouch_url: None,
            vouch_credential: None,
            vouch_keys: Vec::new(),
            feed_token: None,
            cors_origins: Vec::new(),
            identity_burst: 10,
            identity_refill_s: 60,
            held_log_bytes: DEFAULT_HELD_LOG_BYTES,
            log_fold_bytes: DEFAULT_LOG_FOLD_BYTES,
        }
    }
}

/// The default of [`Settings::held_log_bytes`]: 256 MiB, about 670 000 events of the costliest
/// common kind (a rotation, about 400 B held; `tests/held_memory.rs`), so a full log of
/// rotations (`log::MAX_LOG_EVENTS`) takes under 3 % of it.
pub const DEFAULT_HELD_LOG_BYTES: usize = 256 << 20;

/// The default of [`Settings::log_fold_bytes`]: 16 MiB. A full log of rotations
/// (`log::MAX_LOG_EVENTS`, the costliest common kind) takes about 6.5 MB, so no ordinary log
/// comes near it; what reaches it is a log padded on purpose (a `scope` may name its actions
/// many times over). The held folds' default bound holds 8 logs at the whole-fold bound
/// (32 MiB each).
pub const DEFAULT_LOG_FOLD_BYTES: usize = 16 << 20;

impl Settings {
    /// The settings of the configuration `get` reads (`std::env::var` for `serve`): each key
    /// that is unset keeps its default (`Settings::default`). Refuses a value that does not
    /// parse, and settings [`Settings::check`] refuses.
    pub fn from_config(get: impl Fn(&str) -> Option<String>) -> Result<Settings, String> {
        fn number<T: std::str::FromStr>(
            get: &impl Fn(&str) -> Option<String>,
            name: &str,
            default: T,
        ) -> Result<T, String> {
            match get(name) {
                Some(v) => v
                    .trim()
                    .parse::<T>()
                    .map_err(|_| format!("{name}={v} is not a whole number in range")),
                None => Ok(default),
            }
        }
        let list = |name: &str| -> Vec<String> {
            get(name)
                .map(|v| {
                    v.split(',')
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty())
                        .collect()
                })
                .unwrap_or_default()
        };
        let text = |name: &str| {
            get(name)
                .map(|v| v.trim().to_string())
                .filter(|v| !v.is_empty())
        };
        let defaults = Settings::default();
        let trusted_proxies = list("TRUSTED_PROXIES")
            .iter()
            .map(|s| {
                s.parse::<brake::Cidr>()
                    .map_err(|e| format!("TRUSTED_PROXIES: {e}"))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let client_ip_header = match text("CLIENT_IP_HEADER") {
            Some(v) => v
                .parse::<brake::ClientIpHeader>()
                .map_err(|e| format!("CLIENT_IP_HEADER: {e}"))?,
            None => defaults.client_ip_header,
        };
        let vouch_keys = list("VOUCH_KEYS");
        if let Some(bad) = vouch_keys
            .iter()
            .find(|k| !ad4m_identity_core::is_ed25519_multikey(k))
        {
            return Err(format!("VOUCH_KEYS: {bad} is no Ed25519 multikey"));
        }
        let settings = Settings {
            pow_bits: number(&get, "POW_BITS", defaults.pow_bits)?,
            brake_global_per_hour: number(
                &get,
                "BRAKE_GLOBAL_PER_HOUR",
                defaults.brake_global_per_hour,
            )?,
            brake_addr_burst: number(&get, "BRAKE_ADDR_BURST", defaults.brake_addr_burst)?,
            brake_addr_per_day: number(&get, "BRAKE_ADDR_PER_DAY", defaults.brake_addr_per_day)?,
            trusted_proxies,
            client_ip_header,
            vouch_url: text("VOUCH_URL"),
            vouch_credential: text("VOUCH_CREDENTIAL").map(vouch::Credential),
            vouch_keys,
            feed_token: text("FEED_TOKEN"),
            cors_origins: list("CORS_ORIGINS"),
            identity_burst: number(&get, "IDENTITY_BURST", defaults.identity_burst)?,
            identity_refill_s: number(&get, "IDENTITY_REFILL_S", defaults.identity_refill_s)?,
            held_log_bytes: number(&get, "HELD_LOG_BYTES", defaults.held_log_bytes)?,
            log_fold_bytes: number(&get, "LOG_FOLD_BYTES", defaults.log_fold_bytes)?,
        };
        settings.check()?;
        Ok(settings)
    }

    /// Refuses settings under which an accepted log could not be held: a held bound below the
    /// per-log whole-fold bound (twice `LOG_FOLD_BYTES`) would fold such a log on every use.
    pub fn check(&self) -> Result<(), String> {
        if self.held_log_bytes < self.log_fold_bytes.saturating_mul(2) {
            return Err(format!(
                "HELD_LOG_BYTES={} is below twice LOG_FOLD_BYTES={}: a log the store takes may \
                 fold to twice LOG_FOLD_BYTES and must fit in the held folds",
                self.held_log_bytes, self.log_fold_bytes
            ));
        }
        Ok(())
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
    /// The folded logs held in memory (`held.rs`).
    pub held: held::HeldLogs,
    /// The client of the vouching service.
    http: reqwest::Client,
    clock: Clock,
    started_at: u64,
    waits: Mutex<Waits>,
    fault: AtomicU8,
    /// [`brake::GLOBAL_EMPTY_WARNING`] was logged and no log has been admitted since
    /// (one line per empty episode of the global bucket).
    global_empty_logged: AtomicBool,
    /// The untrusted-proxy-headers warning was logged once already.
    untrusted_proxy_warned: AtomicBool,
}

impl Store {
    pub fn new(db: db::Db, key: StoreKey, clock: Clock, settings: Settings) -> Arc<Store> {
        let started_at = clock();
        Arc::new(Store {
            db,
            key,
            http: vouch::client(),
            held: held::HeldLogs::new(settings.held_log_bytes),
            settings,
            clock,
            started_at,
            waits: Mutex::new(Waits::default()),
            fault: AtomicU8::new(Fault::None as u8),
            global_empty_logged: AtomicBool::new(false),
            untrusted_proxy_warned: AtomicBool::new(false),
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

    /// When the store started.
    pub fn started_at(&self) -> u64 {
        self.started_at
    }

    /// A fresh challenge for `GET /v1/pow`'s next proof (`brake::challenge`), tagged with the
    /// configured `POW_BITS`: one use, good until `expires`.
    pub fn pow_challenge(&self) -> ad4m_ldk::identity::PowChallenge {
        brake::challenge(
            &self.key.pow_challenge_key(),
            self.settings.pow_bits,
            self.now(),
        )
    }

    /// Logs [`brake::GLOBAL_EMPTY_WARNING`] when the global bucket is empty: once per empty
    /// episode, so an alert can match the one line without a log line per refused request.
    /// The flag resets on the next log the brake admits (`log.rs`), so the next empty episode
    /// logs it again.
    pub(crate) fn notice_global_empty(&self) {
        if !self.global_empty_logged.swap(true, Ordering::SeqCst) {
            eprintln!(
                "identity-store: warning: {} ({} no-note logs an hour); new logs without a \
                 vouching note get 429 global",
                brake::GLOBAL_EMPTY_WARNING,
                self.settings.brake_global_per_hour
            );
        }
    }

    /// For tests: the empty-global-bucket line was logged and no log has been admitted since.
    pub fn global_empty_logged(&self) -> bool {
        self.global_empty_logged.load(Ordering::SeqCst)
    }

    /// Resets the one-line-per-episode flag after the brake admitted a log.
    pub(crate) fn clear_global_empty_logged(&self) {
        self.global_empty_logged.store(false, Ordering::SeqCst);
    }

    /// Logs once when a request carries `CF-Connecting-IP` or `X-Forwarded-For` but
    /// `TRUSTED_PROXIES` is empty: behind a proxy every client then counts as the proxy's one
    /// address for the brake (`brake::Sender`).
    pub(crate) fn note_untrusted_proxy_headers(&self) {
        if !self.untrusted_proxy_warned.swap(true, Ordering::SeqCst) {
            eprintln!(
                "identity-store: warning: a request carried proxy headers but TRUSTED_PROXIES \
                 is empty; behind a proxy set TRUSTED_PROXIES to the proxy's address or \
                 range, or every client counts as the proxy's one address for the brake"
            );
        }
    }

    /// For tests: the untrusted-proxy-headers warning was logged.
    pub fn untrusted_proxy_warned(&self) -> bool {
        self.untrusted_proxy_warned.load(Ordering::SeqCst)
    }

    /// `GET /v1/metrics`: each brake bucket's level, one per line (`brake::metrics`).
    pub fn metrics(&self) -> Result<String, ApiError> {
        let now = self.now();
        let conn = self.db.lock()?;
        brake::metrics(&conn, &self.settings, now)
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
        gone += conn.execute("DELETE FROM pow_seen WHERE expires <= ?1", [now])?;
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
/// How often `serve` runs [`Store::sweep`].
pub const SWEEP_EVERY_S: u64 = 60;
/// The most reads that wait on one topic (one identity's offers, one key, one inbox) at a time.
/// The state poll and the key lookup are unauthenticated, so the cap is per thing waited on, not
/// per caller: behind a proxy every caller has one address. A read that finds its topic full takes the place of the oldest, which
/// answers at once (as with `wait=0`): squatters who fill a topic cannot shut out a later read,
/// they only shorten each other's waits.
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
    /// One inbox address (`pre:<a>` or `<scid>/<keyId>`): a message written to it.
    Inbox(String),
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

    pub fn inbox(address: &str) -> Topic {
        Topic::Inbox(address.to_string())
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
