//! [`KelCache`]: the key logs this executor checked, kept in the executor database.
//!
//! The events sit in the hash-chain module's `SqliteChainStore` tables (type `coasys:kel`) in
//! `ad4m_db.sqlite`; the last signed head the cache accepted for each log sits beside them in
//! `identity_heads`. Every event passes the identity core's `import` (order, links, every rule
//! of the log) before it is stored, and the store only ever extends a log.
//!
//! Extend only: the cache accepts only events that continue its head. A different event at a
//! seq it holds that is valid on the held prefix below it ([`rival_holds`]) records a fork with
//! `record_fork`, appends nothing, keeps the cached history and calls the fork listener once per
//! new fork (the executor publishes `identity-fork`). A store that answers a shorter log never
//! truncates a held one, and is no fork by itself: over the cached event at its head's seq it is
//! behind ([`CacheError::Behind`]); over another hash it is a [`CacheError::Conflict`] unless
//! its own event there holds. Neither is recorded. Signed heads: a head from the agent language
//! counts only under a key of `identity.storeKeys`; events that arrive through
//! [`KelCache::ingest`] need none.
//! [`KelCache::reset`] (`identity.resetLog`) is the one way a held log leaves: the operator
//! drops this executor's copy, and the next read takes the store's branch.

use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, Mutex as StdMutex};
use std::time::{Duration, Instant};

use ad4m_hash_chain::store::{ChainStore, SqliteChainStore, StoredEntry};
use ad4m_hash_chain::ChainType;
use ad4m_identity_core::{
    ChainError, Event, FoldError, ForkEvidence, Head, ImportOutcome, KelChain, KeyState,
};
use ad4m_ldk::identity::SignedHead;
use rusqlite::{params, Connection, OptionalExtension};
use tokio::sync::{broadcast, Mutex};

use super::backend::{IdentityBackend, IdentityError};
use super::head::verify_head;
use super::verdict::{key_verdict, VerdictChange};

/// The chain type of key logs in the hash-chain tables.
pub const KEL_TYPE: ChainType = "coasys:kel";

/// The most events one log page holds (`identity-store`'s page size).
const PAGE: u64 = 256;
/// The most pages one read of a log takes from one source (the store's refresh here, the
/// resolver's reads too): 64 × 256 events, far past any person's log.
pub(super) const MAX_PAGES: usize = 64;
/// The most logs whose refresh time the cache remembers; past it the oldest time goes (that
/// log is stale once), never the others.
const REFRESHED_BOUND: usize = 4096;

/// How many verdict changes wait for a slow receiver before it lags.
const CHANGES_CAPACITY: usize = 1024;
/// How long a "no log lists this key" answer holds: a device-only key can gain a log later (a
/// guest's claim lists it as `d1`), so after this the next read asks the store again.
pub const NO_LOG_TTL: Duration = Duration::from_secs(15 * 60);
/// The most logs, and the most "no log" answers, the key map holds. Past it the map starts
/// over and fills again from the cache and the store.
const KEY_MAP_BOUND: usize = 16_384;

/// The trusted part of one log: its state and the seq of its accepted, store-signed head.
/// Nothing past `trusted` counts.
#[derive(Debug)]
pub struct TrustedLog {
    pub state: KeyState,
    pub trusted: u64,
}

/// What the key map knows about one signing key.
#[derive(Clone, Debug)]
pub enum KeyLookup {
    /// A trusted log lists the key (as a device or a master key) at or below its trusted seq.
    Listed { scid: String, log: Arc<TrustedLog> },
    /// No log lists the key, as the store said less than [`NO_LOG_TTL`] ago.
    NoLog,
    /// Nothing known.
    Unknown,
}

#[derive(Default)]
struct KeyMapInner {
    /// Signing multikey → SCID, for every key a trusted log lists at or below its trusted seq.
    keys: HashMap<String, String>,
    logs: HashMap<String, Arc<TrustedLog>>,
    /// Keys no log lists, with when the store said so.
    no_log: HashMap<String, Instant>,
    /// The SCIDs of local identities (a row in `identity_agents`): the bound never drops them.
    pinned: HashSet<String>,
}

/// The key map: what every trusted log in the cache says, readable without `await`, so the
/// link check reads the same answer wherever it runs. The cache feeds it (each accepted head,
/// each read of a trusted log); the resolver adds the store's "no log" answers. A key never
/// leaves its log, so a listing stays for good (within the bound).
///
/// The logs of local identities are pinned ([`KeyMap::pin`]): the bound never drops them, so
/// the anchor of a link a local agent signs ([`super::anchor_for`]) is always here to read.
#[derive(Default)]
pub struct KeyMap {
    inner: StdMutex<KeyMapInner>,
}

impl KeyMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// What the map knows about the signing multikey `key`.
    pub fn lookup(&self, key: &str) -> KeyLookup {
        let Ok(inner) = self.inner.lock() else {
            return KeyLookup::Unknown;
        };
        if let Some(scid) = inner.keys.get(key) {
            if let Some(log) = inner.logs.get(scid) {
                return KeyLookup::Listed {
                    scid: scid.clone(),
                    log: log.clone(),
                };
            }
        }
        match inner.no_log.get(key) {
            Some(at) if at.elapsed() < NO_LOG_TTL => KeyLookup::NoLog,
            _ => KeyLookup::Unknown,
        }
    }

    /// The trusted log of `scid` the map holds.
    pub fn log(&self, scid: &str) -> Option<Arc<TrustedLog>> {
        self.inner.lock().ok()?.logs.get(scid).cloned()
    }

    /// Puts `log` in place of what the map held for `scid`, with every key it lists at or below
    /// its trusted seq; those keys lose their "no log" answers.
    pub fn publish(&self, scid: &str, log: TrustedLog) {
        let Ok(mut inner) = self.inner.lock() else {
            return;
        };
        if inner.logs.len() >= KEY_MAP_BOUND && !inner.logs.contains_key(scid) {
            let KeyMapInner {
                keys, logs, pinned, ..
            } = &mut *inner;
            logs.retain(|held, _| pinned.contains(held));
            keys.retain(|_, held| pinned.contains(held));
        }
        let trusted = log.trusted;
        for record in log.state.masters.iter().chain(log.state.keys.values()) {
            if record.added <= trusted {
                inner.no_log.remove(&record.sign);
                inner.keys.insert(record.sign.clone(), scid.to_string());
            }
        }
        inner.logs.insert(scid.to_string(), Arc::new(log));
    }

    /// The store's answer that no log lists `key`.
    pub fn note_no_log(&self, key: &str) {
        let Ok(mut inner) = self.inner.lock() else {
            return;
        };
        if inner.keys.contains_key(key) {
            return;
        }
        if inner.no_log.len() >= KEY_MAP_BOUND {
            inner.no_log.retain(|_, at| at.elapsed() < NO_LOG_TTL);
            if inner.no_log.len() >= KEY_MAP_BOUND {
                inner.no_log.clear();
            }
        }
        inner.no_log.insert(key.to_string(), Instant::now());
    }

    /// The keys of `state` (listed at or below `trusted`) that hold a "no log" answer, fresh or
    /// not.
    fn no_log_keys(&self, state: &KeyState, trusted: u64) -> Vec<String> {
        let Ok(inner) = self.inner.lock() else {
            return vec![];
        };
        state
            .masters
            .iter()
            .chain(state.keys.values())
            .filter(|r| r.added <= trusted && inner.no_log.contains_key(&r.sign))
            .map(|r| r.sign.clone())
            .collect()
    }

    /// Every signing key a log in the map lists.
    pub fn listed_keys(&self) -> Vec<String> {
        self.inner
            .lock()
            .map(|inner| inner.keys.keys().cloned().collect())
            .unwrap_or_default()
    }

    /// Keeps the log of the local identity `scid` through the bound, from now on (an agent of
    /// this executor speaks for it). The pin outlives [`KeyMap::forget`].
    pub fn pin(&self, scid: &str) {
        if let Ok(mut inner) = self.inner.lock() {
            inner.pinned.insert(scid.to_string());
        }
    }

    /// Drops the log of `scid` and its keys (`identity.resetLog`). A pin stays: the next
    /// accepted head of the log puts it back for good.
    pub fn forget(&self, scid: &str) {
        if let Ok(mut inner) = self.inner.lock() {
            inner.logs.remove(scid);
            inner.keys.retain(|_, held| held != scid);
        }
    }
}

static KEY_MAP: LazyLock<Arc<KeyMap>> = LazyLock::new(|| Arc::new(KeyMap::new()));

/// The executor's key map: the one the cache of [`super::init`] feeds and every link check
/// reads.
pub fn global_key_map() -> Arc<KeyMap> {
    KEY_MAP.clone()
}

/// Why the cache refused or could not answer.
#[derive(Debug, thiserror::Error)]
pub enum CacheError {
    /// Two different events at one seq, the other one valid on the held prefix below it
    /// ([`rival_holds`]): recorded. Nothing was appended; the cached history stays.
    #[error("fork at seq {}: ours {}, theirs {}", .0.seq, .0.ours, .0.theirs)]
    Fork(ForkEvidence),
    /// The store's signed head names another hash at a seq the cache holds, and no event of
    /// the store's that holds there came with it: a signed head names a hash, not an event, so
    /// this is no fork. Nothing recorded, nobody alerted; the cached log answers.
    #[error(
        "the store's head names {} at seq {}, the cached log holds {} there, and no valid event of the store's shows it",
        .0.theirs, .0.seq, .0.ours
    )]
    Conflict(ForkEvidence),
    /// The store answered less than the cache holds on the same branch: a signed head below the
    /// cached one over the cached event at its seq (`store`), or no log (`None`) of one it
    /// signed a head of. A store restored from a backup, or not caught up. Nothing recorded,
    /// nobody alerted; the cached log and its accepted head stay, and the next refresh tries
    /// again.
    #[error(
        "the store is behind the cache: its head is at seq {}, the cached log's at {ours}",
        .store.map_or_else(|| "none (no log)".to_string(), |s| s.to_string())
    )]
    Behind { store: Option<u64>, ours: u64 },
    /// The events do not continue the log, or break one of its rules.
    #[error("the events do not fold: {0}")]
    Fold(ChainError<FoldError>),
    /// The events start past the cached head (`have`: the cached head's seq, if any).
    #[error("the events start at seq {first}, after a gap")]
    Gap { have: Option<u64>, first: u64 },
    /// The agent language holds no log of the identity and the cache holds none either.
    #[error("no log of this identity")]
    NotFound,
    #[error("the store's head is not signed by a key of identity.storeKeys")]
    BadHeadSignature,
    #[error("the identity store is unavailable: {0}")]
    Unavailable(String),
    /// The cache's own storage failed.
    #[error("the key-log cache: {0}")]
    Storage(String),
}

impl From<IdentityError> for CacheError {
    fn from(e: IdentityError) -> Self {
        match e {
            IdentityError::BadHeadSignature => CacheError::BadHeadSignature,
            IdentityError::NotFound => CacheError::NotFound,
            other => CacheError::Unavailable(other.to_string()),
        }
    }
}

fn storage(e: impl std::fmt::Display) -> CacheError {
    CacheError::Storage(e.to_string())
}

/// What [`KelCache::ingest`] did.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ingested {
    /// New events continued the log; its head now.
    Appended(Head),
    /// The cache held every event already.
    AlreadyHave,
}

/// Called once for each fork the cache records for the first time: `(did, evidence)`.
pub type ForkListener = Box<dyn Fn(&str, &ForkEvidence) + Send + Sync>;

/// One identity's fetch gate: one fetch at a time, and which of them succeeded.
#[derive(Default)]
struct Gate {
    one_fetch: Mutex<()>,
    /// The fetches begun under this gate.
    begun: AtomicU64,
    /// The number (counted in `begun`) of the last fetch that succeeded.
    succeeded: AtomicU64,
}

/// The verified key logs, by SCID.
pub struct KelCache {
    store: SqliteChainStore,
    /// `identity_heads`: the last signed head accepted per log.
    meta: StdMutex<Connection>,
    chains: Mutex<HashMap<String, KelChain>>,
    /// One fetch per identity at a time, so concurrent callers share store round trips.
    fetching: Mutex<HashMap<String, Arc<Gate>>>,
    /// When each log last refreshed from the configured store (unix seconds), in memory only:
    /// after a restart every log counts as stale ([`KelCache::refreshed_within`]).
    refreshed: StdMutex<HashMap<String, u64>>,
    store_keys: Vec<String>,
    on_fork: Option<ForkListener>,
    /// What every trusted log says, for synchronous readers (the link check).
    key_map: Arc<KeyMap>,
    /// Key verdicts that an accepted head changed ([`KelCache::subscribe`]).
    changes: broadcast::Sender<VerdictChange>,
}

const META_SCHEMA: &str = "CREATE TABLE IF NOT EXISTS identity_heads (
    scid TEXT PRIMARY KEY,
    head TEXT NOT NULL,
    accepted_at INTEGER NOT NULL
)";

fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// The SCID of `did` (`did:scid:coasys:1:<scid>`), or `did` itself when it is a bare SCID.
pub fn scid_from(did: &str) -> &str {
    did.strip_prefix(ad4m_identity_core::DID_PREFIX)
        .unwrap_or(did)
}

/// The log that `events` start (from seq 0, in any order, repeats dropped), checked to be
/// `scid`'s; with the events in seq order.
pub fn fold_log(scid: &str, events: &[Event]) -> Result<(KelChain, Vec<Event>), CacheError> {
    let mut sorted: Vec<Event> = events.to_vec();
    sorted.sort_by_key(|e| e.seq);
    sorted.dedup_by(|a, b| a.seq == b.seq && a == b);
    match sorted.first() {
        None => return Err(CacheError::NotFound),
        Some(first) if first.seq != 0 => {
            return Err(CacheError::Gap {
                have: None,
                first: first.seq,
            })
        }
        Some(_) => {}
    }
    let chain = ad4m_identity_core::fold(&sorted).map_err(CacheError::Fold)?;
    if chain.id() != scid {
        return Err(CacheError::Fold(ChainError::WrongChain {
            want: scid.to_string(),
            got: chain.id().clone(),
        }));
    }
    Ok((chain, sorted))
}

/// Checks that `rival`, an event at a seq `held` holds with another hash, is a valid event of
/// `scid`'s log: the module's `import` compares hashes only and checks no proof of an event at a
/// held seq, so a hash-only conflict is no evidence until the rival steps through the core's own
/// rules on the held prefix below it (`fold` of `held[..rival.seq]` and the rival: seq, prev,
/// chain id, every rule of `KelPolicy`, the proofs). Without it anybody could raise forks with
/// no key. `Err` says why the rival does not hold.
pub fn rival_holds(scid: &str, held: &[Event], rival: &Event) -> Result<(), CacheError> {
    let mut prefix: Vec<Event> = held.iter().filter(|e| e.seq < rival.seq).cloned().collect();
    prefix.push(rival.clone());
    fold_log(scid, &prefix).map(|_| ())
}

impl KelCache {
    /// The events of a store's page, each checked by `Event::from_json`.
    fn page_events(page: &ad4m_ldk::identity::LogPage) -> Result<Vec<Event>, CacheError> {
        page.events
            .iter()
            .map(Event::from_json)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| CacheError::Fold(ChainError::Policy(e)))
    }

    /// The cache in the database file at `path` (the executor's `ad4m_db.sqlite`), trusting
    /// heads signed by `store_keys` (multikeys).
    pub fn open(path: impl AsRef<Path>, store_keys: Vec<String>) -> Result<Self, CacheError> {
        let store = SqliteChainStore::open(path.as_ref()).map_err(storage)?;
        let meta = Connection::open(path.as_ref()).map_err(storage)?;
        // The store's connection writes the same file; `reset` deletes through this one.
        meta.busy_timeout(std::time::Duration::from_secs(5))
            .map_err(storage)?;
        Self::with(store, meta, store_keys, global_key_map())
    }

    fn with(
        store: SqliteChainStore,
        meta: Connection,
        store_keys: Vec<String>,
        key_map: Arc<KeyMap>,
    ) -> Result<Self, CacheError> {
        meta.execute_batch(META_SCHEMA).map_err(storage)?;
        Ok(KelCache {
            store,
            meta: StdMutex::new(meta),
            chains: Mutex::new(HashMap::new()),
            fetching: Mutex::new(HashMap::new()),
            refreshed: StdMutex::new(HashMap::new()),
            store_keys,
            on_fork: None,
            key_map,
            changes: broadcast::channel(CHANGES_CAPACITY).0,
        })
    }

    /// The key map this cache feeds.
    pub fn key_map(&self) -> &Arc<KeyMap> {
        &self.key_map
    }

    /// Fires when an accepted head changes a key's verdict at the trusted seq (a revocation,
    /// an expiry the head reaches, a key a log lists for the first time after the key map
    /// answered "no log" for it). A receiver that lags misses changes: it re-checks everything
    /// it holds then.
    pub fn subscribe(&self) -> broadcast::Receiver<VerdictChange> {
        self.changes.subscribe()
    }

    /// Puts the trusted part of `state` (up to `trusted`) into the key map and fires a
    /// [`VerdictChange`] for each key whose verdict differs from what the map held (for a log
    /// the map did not hold: each key it had answered "no log" for).
    fn publish_trusted(&self, scid: &str, state: &KeyState, trusted: u64) {
        let held = self.key_map.log(scid);
        if held.as_ref().is_some_and(|h| h.trusted == trusted) {
            return;
        }
        let now = now();
        let had_no_log = self.key_map.no_log_keys(state, trusted);
        self.key_map.publish(
            scid,
            TrustedLog {
                state: state.clone(),
                trusted,
            },
        );
        let keys = state
            .masters
            .iter()
            .chain(state.keys.values())
            .map(|r| r.sign.clone());
        for key in keys {
            let verdict = key_verdict(state, trusted, &key, now);
            let changed = match &held {
                Some(h) => key_verdict(&h.state, h.trusted, &key, now) != verdict,
                None => had_no_log.contains(&key),
            };
            if changed {
                // No receiver is not an error: nothing re-checks then.
                let _ = self.changes.send(VerdictChange {
                    key,
                    scid: scid.to_string(),
                    verdict,
                });
            }
        }
    }

    /// Pins the local identity `scid` in the key map ([`KeyMap::pin`]) and loads its trusted
    /// log there, so a link its agents sign carries its anchor with no read of the cache. At
    /// startup for every bound identity, and when an agent binds to one. `Ok(false)`: the
    /// cache holds no trusted log of it yet; its next accepted head puts it in the map.
    pub async fn keep_local(&self, scid: &str) -> Result<bool, CacheError> {
        self.key_map.pin(scid);
        Ok(self.trusted(scid).await?.is_some())
    }

    /// Whether `identity.storeKeys` names at least one key: without one the cache accepts no
    /// head, so no log ever has a trusted seq and no publish or claim can bind an agent.
    pub fn trusts_a_store(&self) -> bool {
        !self.store_keys.is_empty()
    }

    /// Calls `listener` for each fork recorded for the first time.
    pub fn set_fork_listener(&mut self, listener: ForkListener) {
        self.on_fork = Some(listener);
    }

    /// The cached log of `scid` (memory, else rebuilt from the database with `fold`).
    pub async fn chain(&self, scid: &str) -> Result<Option<KelChain>, CacheError> {
        let mut chains = self.chains.lock().await;
        self.chain_locked(&mut chains, scid).await
    }

    async fn chain_locked(
        &self,
        chains: &mut HashMap<String, KelChain>,
        scid: &str,
    ) -> Result<Option<KelChain>, CacheError> {
        if let Some(chain) = chains.get(scid) {
            return Ok(Some(chain.clone()));
        }
        let events = self.stored_events(scid, 0).await?;
        if events.is_empty() {
            return Ok(None);
        }
        let chain = ad4m_identity_core::fold(&events).map_err(CacheError::Fold)?;
        chains.insert(scid.to_string(), chain.clone());
        Ok(Some(chain))
    }

    /// The cached events of `scid` from seq `from` on.
    pub async fn stored_events(&self, scid: &str, from: u64) -> Result<Vec<Event>, CacheError> {
        let entries = self
            .store
            .range(KEL_TYPE, &scid.to_string(), from, u32::MAX)
            .await
            .map_err(storage)?;
        entries
            .iter()
            .map(|e| e.entry::<Event>().map_err(storage))
            .collect()
    }

    /// The last signed head accepted for `scid`.
    pub fn accepted_head(&self, scid: &str) -> Result<Option<SignedHead>, CacheError> {
        let meta = self.meta.lock().map_err(storage)?;
        let row: Option<String> = meta
            .query_row(
                "SELECT head FROM identity_heads WHERE scid = ?1",
                params![scid],
                |row| row.get(0),
            )
            .optional()
            .map_err(storage)?;
        row.map(|head| serde_json::from_str(&head).map_err(storage))
            .transpose()
    }

    /// The cached log of `scid` with its trusted seq: the seq of the accepted head, while that
    /// head still verifies under `identity.storeKeys` (heads persist across a change of the
    /// keys) and the cached log reaches it with the same hash. `None` when the cache holds no
    /// log or no such head.
    ///
    /// A listing proves nothing about the listed key's holder (an `add` carries only the
    /// master's signature; the store checks the claim behind it), so every read that maps a key
    /// to its person reads the log only up to this seq. Events past it (from a foreign `src`,
    /// or not yet under a store head) are for resolution display only.
    pub async fn trusted(&self, scid: &str) -> Result<Option<(KelChain, u64)>, CacheError> {
        let Some(head) = self.accepted_head(scid)? else {
            return Ok(None);
        };
        if !verify_head(&head, &self.store_keys) {
            return Ok(None);
        }
        let Some(chain) = self.chain(scid).await? else {
            return Ok(None);
        };
        let held = self
            .store
            .range(KEL_TYPE, &scid.to_string(), head.seq, 1)
            .await
            .map_err(storage)?;
        let matches = held
            .first()
            .is_some_and(|entry| entry.seq == head.seq && entry.hash == head.hash);
        if matches {
            self.publish_trusted(scid, chain.state(), head.seq);
        }
        Ok(matches.then_some((chain, head.seq)))
    }

    fn accept_head(&self, head: &SignedHead) -> Result<(), CacheError> {
        let text = serde_json::to_string(head).map_err(storage)?;
        let meta = self.meta.lock().map_err(storage)?;
        meta.execute(
            "INSERT INTO identity_heads (scid, head, accepted_at) VALUES (?1, ?2, ?3)
             ON CONFLICT(scid) DO UPDATE SET head = ?2, accepted_at = ?3
             WHERE excluded.head != identity_heads.head",
            params![head.scid, text, now() as i64],
        )
        .map_err(storage)?;
        Ok(())
    }

    /// Every fork recorded for `scid`, in seq order.
    pub async fn forks(&self, scid: &str) -> Result<Vec<ForkEvidence>, CacheError> {
        self.store
            .forks(KEL_TYPE, &scid.to_string())
            .await
            .map_err(storage)
    }

    /// Drops this executor's copy of `scid`'s log: its events, its accepted head, its recorded
    /// forks and the log in memory (`identity.resetLog`). The cache never leaves a branch it
    /// holds by itself, so this is the way out of a fork: the next read refreshes from the
    /// configured store and takes the store's branch. The rows sit in the hash-chain tables of
    /// this executor's own database; the shared stores never drop a log.
    pub async fn reset(&self, scid: &str) -> Result<(), CacheError> {
        let mut chains = self.chains.lock().await;
        {
            let mut meta = self.meta.lock().map_err(storage)?;
            let tx = meta.transaction().map_err(storage)?;
            for table in ["chain_entries", "chain_heads", "chain_forks"] {
                tx.execute(
                    &format!("DELETE FROM {table} WHERE type = ?1 AND chain = ?2"),
                    params![KEL_TYPE, scid],
                )
                .map_err(storage)?;
            }
            tx.execute("DELETE FROM identity_heads WHERE scid = ?1", params![scid])
                .map_err(storage)?;
            tx.commit().map_err(storage)?;
        }
        chains.remove(scid);
        if let Ok(mut refreshed) = self.refreshed.lock() {
            refreshed.remove(scid);
        }
        self.key_map.forget(scid);
        Ok(())
    }

    /// Records `fork` unless the same fork is recorded already; calls the listener for a new one.
    async fn record_fork(&self, fork: &ForkEvidence, theirs_json: &str) -> Result<(), CacheError> {
        let known = self.forks(&fork.chain).await?;
        if known
            .iter()
            .any(|k| k.seq == fork.seq && k.theirs == fork.theirs)
        {
            return Ok(());
        }
        self.store
            .record_fork(KEL_TYPE, fork, theirs_json)
            .await
            .map_err(storage)?;
        log::warn!(
            "identity: fork in the log of {} at seq {} (ours {}, theirs {}); the cache keeps its history",
            fork.chain,
            fork.seq,
            fork.ours,
            fork.theirs
        );
        if let Some(listener) = &self.on_fork {
            listener(&ad4m_identity_core::did_of(&fork.chain), fork);
        }
        Ok(())
    }

    /// Events with no head that counts: the same checks, no head signature. `events` are of one
    /// log and may come in any order. The resolver takes the configured store's events this way
    /// when its head is not signed under `identity.storeKeys`; a foreign `src`'s events never
    /// reach the cache (the resolver folds them in memory for its one answer).
    pub async fn ingest(&self, scid: &str, events: &[Event]) -> Result<Ingested, CacheError> {
        let mut chains = self.chains.lock().await;
        self.ingest_locked(&mut chains, scid, events).await
    }

    async fn ingest_locked(
        &self,
        chains: &mut HashMap<String, KelChain>,
        scid: &str,
        events: &[Event],
    ) -> Result<Ingested, CacheError> {
        if events.is_empty() {
            return Ok(Ingested::AlreadyHave);
        }
        if let Some(other) = events.iter().find(|e| e.scid != scid && e.seq > 0) {
            return Err(CacheError::Fold(ChainError::WrongChain {
                want: scid.to_string(),
                got: other.scid.clone(),
            }));
        }
        let Some(cached) = self.chain_locked(chains, scid).await? else {
            return self.start_log(chains, scid, events).await;
        };
        let mut next = cached.clone();
        let before = cached.head();
        match ad4m_identity_core::import(&mut next, events).map_err(CacheError::Fold)? {
            ImportOutcome::AlreadyHave => Ok(Ingested::AlreadyHave),
            ImportOutcome::Gap { have, first } => Err(CacheError::Gap {
                have: Some(have),
                first,
            }),
            ImportOutcome::Fork(fork) => {
                // A fork counts only when the competing event is one the log's own rules admit
                // on the held prefix; an unsigned or otherwise invalid body is refused as an
                // event that does not fold, records nothing and alerts nobody.
                let rival = events
                    .iter()
                    .find(|e| {
                        e.seq == fork.seq
                            && ad4m_identity_core::event_hash(e).is_ok_and(|h| h == fork.theirs)
                    })
                    .ok_or_else(|| {
                        CacheError::Fold(ChainError::Malformed(format!(
                            "no event at seq {} hashes to {}",
                            fork.seq, fork.theirs
                        )))
                    })?;
                let held = self.stored_events(scid, 0).await?;
                rival_holds(scid, &held, rival)?;
                let theirs = rival.to_canonical_json().unwrap_or_default();
                self.record_fork(&fork, &theirs).await?;
                Err(CacheError::Fork(fork))
            }
            ImportOutcome::Appended { head, entries } => {
                // Store the copies the import appended, never a pick of our own from the batch:
                // a copy with its proofs stripped hashes the same as the signed one.
                let fresh: Vec<&Event> = entries.iter().collect();
                self.persist(scid, Some(&before), &fresh).await?;
                chains.insert(scid.to_string(), next);
                Ok(Ingested::Appended(head))
            }
        }
    }

    async fn start_log(
        &self,
        chains: &mut HashMap<String, KelChain>,
        scid: &str,
        events: &[Event],
    ) -> Result<Ingested, CacheError> {
        let (chain, sorted) = fold_log(scid, events)?;
        let head = chain.head();
        let refs: Vec<&Event> = sorted.iter().collect();
        self.persist(scid, None, &refs).await?;
        chains.insert(scid.to_string(), chain);
        Ok(Ingested::Appended(head))
    }

    async fn persist(
        &self,
        scid: &str,
        expect: Option<&Head>,
        events: &[&Event],
    ) -> Result<(), CacheError> {
        let at = now();
        let entries = events
            .iter()
            .map(|e| StoredEntry::from_entry(*e, at).map_err(storage))
            .collect::<Result<Vec<_>, _>>()?;
        self.store
            .append(KEL_TYPE, &scid.to_string(), expect, &entries)
            .await
            .map_err(storage)?;
        Ok(())
    }

    /// Takes a signed head and the events that reach it from the agent language: the head must
    /// verify under `identity.storeKeys`; the events must continue the cached log up to the
    /// head. Then the head is the log's accepted head.
    ///
    /// A head below the cached one is never taken (a shorter log never truncates a held one).
    /// When the cached event at the head's seq has the head's hash, the store is behind on the
    /// cached branch (restored from a backup, or not caught up): [`CacheError::Behind`], nothing
    /// recorded. When it has another hash, the head alone names a hash, not an event, so it is no
    /// fork: the store's `events` at or below the head's seq go through the same comparison as
    /// [`KelCache::ingest`], which records a fork only for a rival event that holds on the held
    /// prefix ([`rival_holds`]); without one the answer is [`CacheError::Conflict`], nothing
    /// recorded. A head at the cached seq with another hash than the cached event is a
    /// `Conflict` too.
    pub async fn accept(
        &self,
        scid: &str,
        head: &SignedHead,
        events: &[Event],
    ) -> Result<Option<Head>, CacheError> {
        if !verify_head(head, &self.store_keys) {
            return Err(CacheError::BadHeadSignature);
        }
        if head.scid != scid {
            return Err(CacheError::Fold(ChainError::WrongChain {
                want: scid.to_string(),
                got: head.scid.clone(),
            }));
        }
        let mut chains = self.chains.lock().await;
        if let Some(cached) = self.chain_locked(&mut chains, scid).await? {
            let ours = cached.head();
            if head.seq < ours.seq {
                let held = cached.hash_at(head.seq).cloned().unwrap_or_default();
                if held == head.hash {
                    return Err(CacheError::Behind {
                        store: Some(head.seq),
                        ours: ours.seq,
                    });
                }
                let conflict = ForkEvidence {
                    chain: scid.to_string(),
                    seq: head.seq,
                    ours: held,
                    theirs: head.hash.clone(),
                };
                let below: Vec<Event> = events
                    .iter()
                    .filter(|e| e.seq <= head.seq)
                    .cloned()
                    .collect();
                return match self.ingest_locked(&mut chains, scid, &below).await {
                    Err(e @ (CacheError::Fork(_) | CacheError::Storage(_))) => Err(e),
                    _ => Err(CacheError::Conflict(conflict)),
                };
            }
        }
        self.ingest_locked(&mut chains, scid, events).await?;
        let Some(chain) = chains.get(scid) else {
            return Ok(None);
        };
        let ours = chain.head();
        if ours.seq == head.seq && ours.hash != head.hash {
            return Err(CacheError::Conflict(ForkEvidence {
                chain: scid.to_string(),
                seq: ours.seq,
                ours: ours.hash.clone(),
                theirs: head.hash.clone(),
            }));
        }
        if ours.seq == head.seq {
            self.accept_head(head)?;
            self.publish_trusted(scid, chain.state(), head.seq);
        }
        Ok(Some(ours))
    }

    /// Refreshes `did`'s log from the agent language: reads the pages from the cached head on
    /// (the head's own event included, so a different event there shows as a fork) and accepts
    /// them with [`KelCache::accept`]. One fetch per identity at a time; a caller that waited
    /// on the gate takes the cached log when a fetch that began after it called succeeded
    /// meanwhile (one that began earlier may predate what it asks for, such as an append it
    /// just made). The identity's gate is dropped when no other fetch waits on it, so looking
    /// up many identities keeps no state.
    pub async fn refresh(
        &self,
        backend: &dyn IdentityBackend,
        did: &str,
    ) -> Result<KelChain, CacheError> {
        let scid = scid_from(did).to_string();
        let gate = {
            let mut fetching = self.fetching.lock().await;
            fetching
                .entry(scid.clone())
                .or_insert_with(|| Arc::new(Gate::default()))
                .clone()
        };
        let asked = gate.begun.load(Ordering::SeqCst);
        let result = {
            let _one_fetch = gate.one_fetch.lock().await;
            let reused = if gate.succeeded.load(Ordering::SeqCst) > asked {
                self.chain(&scid).await.ok().flatten()
            } else {
                None
            };
            match reused {
                Some(chain) => Ok(chain),
                None => {
                    let number = gate.begun.fetch_add(1, Ordering::SeqCst) + 1;
                    let fetched = self.fetch(backend, &scid).await;
                    if fetched.is_ok() {
                        gate.succeeded.store(number, Ordering::SeqCst);
                    }
                    fetched
                }
            }
        };
        if result.is_ok() {
            self.note_refreshed(&scid, now());
        }
        let mut fetching = self.fetching.lock().await;
        // The map's handle and ours: nobody else waits on this gate.
        if Arc::strong_count(&gate) <= 2 {
            fetching.remove(&scid);
        }
        result
    }

    /// Marks `scid`'s log as read from the store (or tried) at `at`. At [`REFRESHED_BOUND`] logs
    /// the one read longest ago is forgotten, so many SCIDs (each failing read notes one) cannot
    /// make every log stale at once.
    pub(crate) fn note_refreshed(&self, scid: &str, at: u64) {
        if let Ok(mut refreshed) = self.refreshed.lock() {
            if refreshed.len() >= REFRESHED_BOUND && !refreshed.contains_key(scid) {
                let oldest = refreshed
                    .iter()
                    .min_by_key(|(_, at)| **at)
                    .map(|(scid, _)| scid.clone());
                if let Some(oldest) = oldest {
                    refreshed.remove(&oldest);
                }
            }
            refreshed.insert(scid.to_string(), at);
        }
    }

    /// Whether `scid`'s log refreshed from the configured store within `window` seconds before
    /// `now` (in this run of the executor).
    pub fn refreshed_within(&self, scid: &str, now: u64, window: u64) -> bool {
        self.refreshed
            .lock()
            .ok()
            .and_then(|refreshed| refreshed.get(scid).copied())
            .is_some_and(|at| now.saturating_sub(at) < window)
    }

    /// The pages of [`KelCache::refresh`], under the identity's gate.
    async fn fetch(
        &self,
        backend: &dyn IdentityBackend,
        scid: &str,
    ) -> Result<KelChain, CacheError> {
        let did = ad4m_identity_core::did_of(scid);
        for _ in 0..MAX_PAGES {
            let cached = self.chain(scid).await?;
            let from = cached.as_ref().map_or(0, |c| c.head().seq);
            let Some(page) = backend.log(&did, from).await? else {
                return match cached {
                    // The store answers no log of one it signed a head of: it is behind (a
                    // store restored from a backup older than the log). No event names a rival,
                    // so it is no fork. A log only another host served is no evidence about the
                    // store at all, nor is a head no key of `identity.storeKeys` verifies now
                    // (a former store's).
                    Some(chain)
                        if self
                            .accepted_head(scid)?
                            .is_some_and(|head| verify_head(&head, &self.store_keys)) =>
                    {
                        Err(CacheError::Behind {
                            store: None,
                            ours: chain.head().seq,
                        })
                    }
                    _ => Err(CacheError::NotFound),
                };
            };
            let reached = match self
                .accept(scid, &page.head, &Self::page_events(&page)?)
                .await
            {
                // The head disagrees with the cached log below this page: read the store's log
                // from seq 0 once, so a rival event of its that holds is recorded as a fork.
                Err(CacheError::Conflict(_)) if page.head.seq < from => {
                    let Some(page) = backend.log(&did, 0).await? else {
                        return Err(CacheError::NotFound);
                    };
                    self.accept(scid, &page.head, &Self::page_events(&page)?)
                        .await?
                }
                other => other?,
            };
            match reached {
                Some(head) if head.seq >= page.head.seq => {
                    return self.chain(scid).await?.ok_or(CacheError::NotFound);
                }
                // A page holds at most PAGE events: read on from where it ended.
                Some(head) if head.seq > from || cached.is_none() => continue,
                _ => {
                    return Err(CacheError::Unavailable(format!(
                        "the store's page from seq {from} does not reach its head {} (page of at most {PAGE})",
                        page.head.seq
                    )))
                }
            }
        }
        Err(CacheError::Unavailable(format!(
            "the store's log of {did} runs past {MAX_PAGES} pages"
        )))
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::identity::head::test_store_key::TestStoreKey;
    use ad4m_identity_core::build::{cosign, incept_human, revoke, Signer};
    use ad4m_identity_core::{DeviceKey, KeyLife};
    use ad4m_ldk::identity::{
        AgentRecord, AppendRequest, Claim, ClaimReceipt, KeyIndexEntry, LogPage, OfferListing,
        OfferOpened, OfferProgress, SignedOffer, SignedRequest,
    };
    use async_trait::async_trait;
    use ed25519_dalek::SigningKey;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use tokio::sync::Notify;

    /// A cache in memory, trusting heads signed by `store_keys`.
    pub(crate) fn in_memory(store_keys: Vec<String>) -> KelCache {
        // One in-memory database for both connections, as `open` shares one file.
        let name = format!(
            "file:kel-cache-{}?mode=memory&cache=shared",
            uuid::Uuid::new_v4().simple()
        );
        let flags = rusqlite::OpenFlags::SQLITE_OPEN_READ_WRITE
            | rusqlite::OpenFlags::SQLITE_OPEN_CREATE
            | rusqlite::OpenFlags::SQLITE_OPEN_URI;
        let store =
            SqliteChainStore::from_connection(Connection::open_with_flags(&name, flags).unwrap())
                .unwrap();
        let meta = Connection::open_with_flags(&name, flags).unwrap();
        KelCache::with(store, meta, store_keys, Arc::new(KeyMap::new())).unwrap()
    }

    /// A cache on the database file at `path`, as [`KelCache::open`] but with a key map of its
    /// own: a new one is what a restart of the executor sees.
    pub(crate) fn on_file(path: &Path, store_keys: Vec<String>) -> KelCache {
        let store = SqliteChainStore::open(path).unwrap();
        let meta = Connection::open(path).unwrap();
        meta.busy_timeout(Duration::from_secs(5)).unwrap();
        KelCache::with(store, meta, store_keys, Arc::new(KeyMap::new())).unwrap()
    }

    /// `trusted` answers only while the cached event at the accepted head's seq has the head's
    /// hash: a store-signed head over another event at that seq trusts nothing.
    #[tokio::test]
    async fn a_head_whose_hash_is_not_the_cached_events_trusts_nothing() {
        let log = TestLog::new(94);
        let cache = trusting();
        accept_signed(&cache, &log.events).await;
        let scid = log.scid();
        assert_eq!(
            cache.trusted(&scid).await.unwrap().map(|(_, seq)| seq),
            Some(0)
        );

        let other = TestLog::new(95).chain().head().hash.clone();
        assert_ne!(other, log.chain().head().hash);
        let signed = store_key().sign_head(&scid, 0, &other, 2);
        cache.accept_head(&signed).unwrap();
        assert!(cache.trusted(&scid).await.unwrap().is_none());
    }

    /// At the bound the oldest refresh time goes and every other log stays fresh: many SCIDs
    /// (failing reads note one each) never make every log stale at once.
    #[test]
    fn past_the_bound_only_the_oldest_refresh_time_goes() {
        let cache = trusting();
        let now = 1_000_000;
        cache.note_refreshed("oldest", now - 50);
        for i in 0..REFRESHED_BOUND - 2 {
            cache.note_refreshed(&format!("junk{i}"), now - 10);
        }
        cache.note_refreshed("real", now);
        cache.note_refreshed("another", now);
        assert!(
            !cache.refreshed_within("oldest", now, 60),
            "the oldest went"
        );
        assert!(
            cache.refreshed_within("real", now, 60),
            "the others stay fresh"
        );
        assert!(cache.refreshed_within("junk0", now, 60));
        assert!(cache.refreshed_within("another", now, 60));
        assert_eq!(cache.refreshed.lock().unwrap().len(), REFRESHED_BOUND);
    }

    /// The store key [`FakeStore`] signs its heads with.
    pub(crate) fn store_key() -> TestStoreKey {
        TestStoreKey::new(9)
    }

    /// A cache in memory trusting [`store_key`] (and so [`FakeStore`]'s heads).
    pub(crate) fn trusting() -> KelCache {
        in_memory(vec![store_key().multikey()])
    }

    /// Takes `events` (a log from seq 0) into `cache` as the configured store would answer
    /// them: with a head signed by [`store_key`] at their last event, so that event is the log's
    /// trusted seq. The events may continue what the cache holds.
    pub(crate) async fn accept_signed(cache: &KelCache, events: &[Event]) {
        let chain = ad4m_identity_core::fold(events).unwrap();
        let head = chain.head();
        let signed = store_key().sign_head(chain.id(), head.seq, &head.hash, 1);
        cache.accept(chain.id(), &signed, events).await.unwrap();
    }

    /// A person's log built with the core's builders: master m0 and one device d1.
    pub(crate) struct TestLog {
        pub master: SigningKey,
        pub device: SigningKey,
        pub events: Vec<Event>,
    }

    impl TestLog {
        pub fn new(seed: u8) -> Self {
            let master = SigningKey::from_bytes(&[seed; 32]);
            let device = SigningKey::from_bytes(&[seed.wrapping_add(100); 32]);
            let next = ad4m_identity_core::commitment(&master.key(), "salt");
            let enc = ad4m_identity_core::encode_x25519_multikey(&[seed; 32]);
            let mut incept = incept_human(
                &master,
                next,
                vec![DeviceKey {
                    id: "d1".into(),
                    sign: device.key(),
                    enc,
                    life: KeyLife::Standing,
                }],
            )
            .unwrap();
            cosign(&mut incept, "d1", &device).unwrap();
            TestLog {
                master,
                device,
                events: vec![incept],
            }
        }

        pub fn scid(&self) -> String {
            self.events[0].scid.clone()
        }

        pub fn chain(&self) -> KelChain {
            ad4m_identity_core::fold(&self.events).unwrap()
        }

        /// Appends a `revoke` of d1 signed by m0.
        pub fn revoke_d1(&mut self) {
            let event = revoke(&self.chain(), &self.master, &["d1".to_string()]).unwrap();
            self.events.push(event);
        }
    }

    /// A store that serves one log through the backend interface and signs its heads.
    pub(crate) struct FakeStore {
        pub key: TestStoreKey,
        pub events: StdMutex<Vec<Event>>,
        pub log_calls: AtomicUsize,
        /// The key index's answer for every key, when set: a did:scid that need not list it.
        pub key_owner_override: StdMutex<Option<String>>,
        /// When set, the first `log` call notifies `.0`, then waits for `.1`.
        pub pause_store: StdMutex<Option<(Arc<Notify>, Arc<Notify>)>>,
        /// A `log` call fails as unavailable.
        pub store_down: AtomicBool,
        /// A `key_owner` call fails as unavailable.
        pub key_index_down: AtomicBool,
        pub key_owner_calls: AtomicUsize,
        /// The agent records written, with their signed requests.
        pub puts: StdMutex<Vec<(AgentRecord, Option<SignedRequest>)>>,
        /// A `put_agent` call fails as unavailable.
        pub put_down: AtomicBool,
    }

    impl FakeStore {
        pub fn new(events: Vec<Event>) -> Self {
            FakeStore {
                key: store_key(),
                events: StdMutex::new(events),
                log_calls: AtomicUsize::new(0),
                key_owner_override: StdMutex::new(None),
                pause_store: StdMutex::new(None),
                store_down: AtomicBool::new(false),
                key_index_down: AtomicBool::new(false),
                key_owner_calls: AtomicUsize::new(0),
                puts: StdMutex::new(vec![]),
                put_down: AtomicBool::new(false),
            }
        }

        /// The page from seq `from` with a signed head; `None` while it holds no log. Also
        /// what the resolver's HTTP test hosts serve.
        pub fn page(&self, from: u64) -> Option<LogPage> {
            let events = self.events.lock().unwrap().clone();
            if events.is_empty() {
                return None;
            }
            let chain = ad4m_identity_core::fold(&events).unwrap();
            let head = chain.head();
            let page: Vec<serde_json::Value> = events
                .iter()
                .filter(|e| e.seq >= from)
                .map(|e| serde_json::to_value(e).unwrap())
                .collect();
            Some(LogPage {
                appended_at: vec![0; page.len()],
                events: page,
                head: self.key.sign_head(chain.id(), head.seq, &head.hash, 1),
                note: None,
                vouched: false,
                pauses: vec![],
            })
        }
    }

    #[async_trait]
    impl IdentityBackend for FakeStore {
        async fn log(&self, _did: &str, from: u64) -> Result<Option<LogPage>, IdentityError> {
            self.log_calls.fetch_add(1, Ordering::SeqCst);
            let pause = self.pause_store.lock().unwrap().take();
            if let Some((entered, release)) = pause {
                entered.notify_one();
                release.notified().await;
            }
            if self.store_down.load(Ordering::SeqCst) {
                return Err(IdentityError::Unavailable("down".into()));
            }
            Ok(self.page(from))
        }
        /// The index of the one log it holds (every device key, revoked ones too), unless
        /// `key_owner_override` names another identity.
        async fn key_owner(
            &self,
            multikey: &str,
            _wait_s: u8,
        ) -> Result<Option<KeyIndexEntry>, IdentityError> {
            self.key_owner_calls.fetch_add(1, Ordering::SeqCst);
            if self.key_index_down.load(Ordering::SeqCst) {
                return Err(IdentityError::Unavailable("down".into()));
            }
            let events = self.events.lock().unwrap().clone();
            if events.is_empty() {
                return Ok(None);
            }
            let chain = ad4m_identity_core::fold(&events).unwrap();
            let head = chain.head();
            let signed = self.key.sign_head(chain.id(), head.seq, &head.hash, 1);
            let overridden = self.key_owner_override.lock().unwrap().clone();
            let record = chain
                .state()
                .keys
                .values()
                .find(|r| r.sign == multikey)
                .cloned();
            Ok(match (overridden, record) {
                (Some(did), _) => Some(KeyIndexEntry {
                    did,
                    key_id: "d1".into(),
                    added: 0,
                    revoked: None,
                    bot: None,
                    expires: None,
                    paused: false,
                    head: signed,
                }),
                (None, Some(record)) => Some(KeyIndexEntry {
                    did: chain.state().did.clone(),
                    key_id: record.id,
                    added: record.added,
                    revoked: record.revoked,
                    bot: None,
                    expires: record.scopes.last().map(|(_, scope)| scope.expires),
                    paused: false,
                    head: signed,
                }),
                (None, None) => None,
            })
        }
        async fn append(&self, _: &str, _: &AppendRequest) -> Result<SignedHead, IdentityError> {
            unimplemented_call("append")
        }
        async fn put_agent(
            &self,
            record: &AgentRecord,
            req: Option<&SignedRequest>,
        ) -> Result<(), IdentityError> {
            if self.put_down.load(Ordering::SeqCst) {
                return Err(IdentityError::Unavailable("down".into()));
            }
            // As the store: only a greater version replaces the record.
            if self
                .agent_version(&record.did)
                .await?
                .is_some_and(|stored| record.version <= stored)
            {
                return Err(IdentityError::Invalid("version".into()));
            }
            self.puts
                .lock()
                .unwrap()
                .push((record.clone(), req.cloned()));
            Ok(())
        }
        async fn agent_version(&self, did: &str) -> Result<Option<u64>, IdentityError> {
            Ok(self
                .puts
                .lock()
                .unwrap()
                .iter()
                .rev()
                .find(|(r, _)| r.did == did)
                .map(|(r, _)| r.version))
        }
        async fn open_offer(&self, _: &SignedOffer) -> Result<OfferOpened, IdentityError> {
            unimplemented_call("open_offer")
        }
        async fn list_offers(
            &self,
            _: &str,
            _: &SignedRequest,
            _: u8,
        ) -> Result<Vec<OfferListing>, IdentityError> {
            unimplemented_call("list_offers")
        }
        async fn claim_offer(
            &self,
            _: &str,
            _: &str,
            _: &Claim,
        ) -> Result<ClaimReceipt, IdentityError> {
            unimplemented_call("claim_offer")
        }
        async fn offer_state(
            &self,
            _: &str,
            _: &str,
            _: &str,
            _: u8,
        ) -> Result<OfferProgress, IdentityError> {
            unimplemented_call("offer_state")
        }
    }

    /// The fake store serves logs only; a cache test that reaches another call is wrong.
    fn unimplemented_call<T>(name: &str) -> Result<T, IdentityError> {
        Err(IdentityError::Unavailable(format!(
            "the cache tests' store has no {name}"
        )))
    }

    fn counting_cache(cache: &mut KelCache) -> Arc<AtomicUsize> {
        let forks = Arc::new(AtomicUsize::new(0));
        let seen = forks.clone();
        cache.set_fork_listener(Box::new(move |_, _| {
            seen.fetch_add(1, Ordering::SeqCst);
        }));
        forks
    }

    /// A "no log" answer holds for `NO_LOG_TTL`, then the key counts as unknown again, so a
    /// later claim of the key shows.
    #[test]
    fn a_no_log_answer_expires() {
        let map = KeyMap::new();
        map.note_no_log("z6MkGuest");
        assert!(matches!(map.lookup("z6MkGuest"), KeyLookup::NoLog));
        let Some(old) = Instant::now().checked_sub(NO_LOG_TTL) else {
            return;
        };
        map.inner
            .lock()
            .unwrap()
            .no_log
            .insert("z6MkGuest".into(), old);
        assert!(matches!(map.lookup("z6MkGuest"), KeyLookup::Unknown));
    }

    /// An accepted head puts its log's keys in the key map; a listing stays until the log is
    /// reset (`identity.resetLog`).
    #[tokio::test]
    async fn the_key_map_lists_keys_up_to_the_trusted_seq() {
        let log = TestLog::new(2);
        let cache = trusting();
        accept_signed(&cache, &log.events).await;
        let device = log.device.key();
        match cache.key_map().lookup(&device) {
            KeyLookup::Listed { scid, log: trusted } => {
                assert_eq!(scid, log.scid());
                assert_eq!(trusted.trusted, 0);
            }
            other => panic!("not listed: {other:?}"),
        }
        cache.key_map().note_no_log(&device);
        assert!(
            matches!(cache.key_map().lookup(&device), KeyLookup::Listed { .. }),
            "a listed key never turns into no log"
        );
        cache.reset(&log.scid()).await.unwrap();
        assert!(matches!(
            cache.key_map().lookup(&device),
            KeyLookup::Unknown
        ));
    }

    /// Past the bound the key map starts over, but keeps the logs (and keys) of the pinned
    /// local identities; a reset drops a pinned log, not its pin.
    #[test]
    fn the_bound_keeps_pinned_logs_and_drops_the_others() {
        let local = TestLog::new(3);
        let other = TestLog::new(4);
        let map = KeyMap::new();
        let trusted = |log: &TestLog| TrustedLog {
            state: log.chain().state().clone(),
            trusted: 0,
        };
        map.pin(&local.scid());
        map.publish(&local.scid(), trusted(&local));
        map.publish(&other.scid(), trusted(&other));
        let fill = |map: &KeyMap| {
            let filler = TestLog::new(5).chain().state().clone();
            for i in 0..KEY_MAP_BOUND {
                map.publish(
                    &format!("QmFiller{i}"),
                    TrustedLog {
                        state: filler.clone(),
                        trusted: 0,
                    },
                );
            }
        };
        fill(&map);
        assert!(map.log(&local.scid()).is_some(), "the pinned log stays");
        assert!(matches!(
            map.lookup(&local.device.key()),
            KeyLookup::Listed { scid, .. } if scid == local.scid()
        ));
        assert!(map.log(&other.scid()).is_none(), "an unpinned log goes");
        assert!(matches!(
            map.lookup(&other.device.key()),
            KeyLookup::Unknown
        ));

        map.forget(&local.scid());
        assert!(map.log(&local.scid()).is_none());
        map.publish(&local.scid(), trusted(&local));
        fill(&map);
        assert!(map.log(&local.scid()).is_some(), "the pin outlives a reset");
    }

    /// `keep_local` pins the identity and puts its trusted log in the key map.
    #[tokio::test]
    async fn keep_local_pins_and_loads_the_trusted_log() {
        let log = TestLog::new(6);
        let cache = trusting();
        assert!(!cache.keep_local(&log.scid()).await.unwrap(), "no log yet");
        accept_signed(&cache, &log.events).await;
        cache.key_map().forget(&log.scid());
        assert!(cache.keep_local(&log.scid()).await.unwrap());
        assert_eq!(cache.key_map().log(&log.scid()).unwrap().trusted, 0);
        assert!(cache
            .key_map()
            .inner
            .lock()
            .unwrap()
            .pinned
            .contains(&log.scid()));
    }

    #[tokio::test]
    async fn a_refresh_imports_the_log_and_its_head() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let cache = in_memory(vec![store.key.multikey()]);
        let chain = cache.refresh(&store, &log.scid()).await.unwrap();
        assert_eq!(chain.head(), log.chain().head());
        assert_eq!(chain.state().keys["d1"].revoked, Some(1));
        let head = cache.accepted_head(&log.scid()).unwrap().unwrap();
        assert_eq!(head.seq, 1);
        // Up to date: a second refresh changes nothing.
        cache.refresh(&store, &log.scid()).await.unwrap();
        assert_eq!(cache.stored_events(&log.scid(), 0).await.unwrap().len(), 2);
    }

    #[tokio::test]
    async fn a_head_under_an_unknown_key_is_refused() {
        let log = TestLog::new(1);
        let store = FakeStore::new(log.events.clone());
        let cache = in_memory(vec![TestStoreKey::new(3).multikey()]);
        assert!(matches!(
            cache.refresh(&store, &log.scid()).await,
            Err(CacheError::BadHeadSignature)
        ));
        assert!(cache.chain(&log.scid()).await.unwrap().is_none());
        // Events from another source need no head.
        assert_eq!(
            cache.ingest(&log.scid(), &log.events).await.unwrap(),
            Ingested::Appended(log.chain().head())
        );
    }

    /// A store restored from a backup answers a shorter log on the cached branch: its signed
    /// head is below the cached one, over the cached event at its seq. It is behind, not a fork:
    /// nothing is recorded, nobody is alerted, and the cached log and its accepted head stay.
    #[tokio::test]
    async fn a_store_behind_on_the_same_branch_records_no_fork() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let mut cache = in_memory(vec![store.key.multikey()]);
        let forks = counting_cache(&mut cache);
        cache.refresh(&store, &log.scid()).await.unwrap();

        // The store loses the revoke.
        store.events.lock().unwrap().truncate(1);
        for _ in 0..2 {
            match cache.refresh(&store, &log.scid()).await {
                Err(CacheError::Behind {
                    store: Some(0),
                    ours: 1,
                }) => {}
                other => panic!("expected the store behind, got {other:?}"),
            }
        }
        assert_eq!(forks.load(Ordering::SeqCst), 0, "no identity-fork");
        assert!(cache.forks(&log.scid()).await.unwrap().is_empty());
        let kept = cache.chain(&log.scid()).await.unwrap().unwrap();
        assert_eq!(kept.head(), log.chain().head(), "the cached history stays");
        assert_eq!(cache.accepted_head(&log.scid()).unwrap().unwrap().seq, 1);
        assert_eq!(
            cache
                .trusted(&log.scid())
                .await
                .unwrap()
                .map(|(_, seq)| seq),
            Some(1)
        );

        // The store catches up: the next refresh takes it, as ever.
        *store.events.lock().unwrap() = log.events.clone();
        cache.refresh(&store, &log.scid()).await.unwrap();
    }

    /// An `add` of a new device (seed `seed`) on `chain`, signed by `master` (m0).
    pub(crate) fn add_device(chain: &KelChain, master: &SigningKey, seed: u8) -> Event {
        let device = SigningKey::from_bytes(&[seed; 32]);
        let mut event = ad4m_identity_core::build::add_unsigned(
            chain,
            DeviceKey {
                id: chain.state().next_device_id(),
                sign: device.key(),
                enc: ad4m_identity_core::encode_x25519_multikey(&[seed; 32]),
                life: KeyLife::Standing,
            },
            "offer",
            "coSig",
        )
        .unwrap();
        cosign(&mut event, "m0", master).unwrap();
        event
    }

    /// A signed head below the cached one naming another hash at its seq, with no event of the
    /// store's there: a head names a hash, not an event, so it is a conflict, not a fork. The
    /// evidence is at the head's own seq: our hash there against the head's.
    #[tokio::test]
    async fn a_lower_head_with_another_hash_and_no_rival_is_an_unrecorded_conflict() {
        let mut log = TestLog::new(1);
        let at_incept = log.chain();
        log.events.push(add_device(&at_incept, &log.master, 40));
        log.events.push(add_device(&log.chain(), &log.master, 41));
        let mut cache = trusting();
        let forks = counting_cache(&mut cache);
        accept_signed(&cache, &log.events).await;
        let scid = log.scid();

        let theirs =
            ad4m_identity_core::event_hash(&add_device(&at_incept, &log.master, 42)).unwrap();
        let head = store_key().sign_head(&scid, 1, &theirs, 2);
        match cache.accept(&scid, &head, &[]).await {
            Err(CacheError::Conflict(evidence)) => {
                assert_eq!(evidence.seq, 1);
                assert_eq!(&evidence.ours, log.chain().hash_at(1).unwrap());
                assert_eq!(evidence.theirs, theirs);
            }
            other => panic!("expected a conflict, got {other:?}"),
        }
        assert_eq!(forks.load(Ordering::SeqCst), 0);
        assert!(cache.forks(&scid).await.unwrap().is_empty());
        assert_eq!(
            cache.trusted(&scid).await.unwrap().map(|(_, seq)| seq),
            Some(2)
        );
    }

    /// A signed head at the cached head's seq naming another hash, with no event of the
    /// store's there: a conflict, recorded nowhere. The accepted head stays the one over the
    /// cached event, so the cached log stays trusted.
    #[tokio::test]
    async fn a_head_at_the_cached_seq_with_another_hash_is_an_unrecorded_conflict() {
        let mut log = TestLog::new(1);
        let at_incept = log.chain();
        log.events.push(add_device(&at_incept, &log.master, 40));
        let mut cache = trusting();
        let forks = counting_cache(&mut cache);
        accept_signed(&cache, &log.events).await;
        let scid = log.scid();

        let theirs =
            ad4m_identity_core::event_hash(&add_device(&at_incept, &log.master, 42)).unwrap();
        let head = store_key().sign_head(&scid, 1, &theirs, 2);
        match cache.accept(&scid, &head, &[]).await {
            Err(CacheError::Conflict(evidence)) => {
                assert_eq!(evidence.seq, 1);
                assert_eq!(&evidence.ours, log.chain().hash_at(1).unwrap());
                assert_eq!(evidence.theirs, theirs);
            }
            other => panic!("expected a conflict, got {other:?}"),
        }
        assert_eq!(forks.load(Ordering::SeqCst), 0);
        assert!(cache.forks(&scid).await.unwrap().is_empty());
        assert_eq!(
            cache.accepted_head(&scid).unwrap().unwrap().hash,
            log.chain().head().hash
        );
        assert_eq!(
            cache.trusted(&scid).await.unwrap().map(|(_, seq)| seq),
            Some(1)
        );
    }

    /// A store on another branch whose head is below the cached one: the refresh reads the
    /// store's log from seq 0, and its event at the head's seq holds on the held prefix, so the
    /// fork is recorded once, with evidence at that one seq.
    #[tokio::test]
    async fn a_lower_head_on_another_branch_with_a_valid_rival_records_one_fork() {
        let mut log = TestLog::new(1);
        let at_incept = log.chain();
        log.events.push(add_device(&at_incept, &log.master, 40));
        log.events.push(add_device(&log.chain(), &log.master, 41));
        let rival = add_device(&at_incept, &log.master, 42);
        let store = FakeStore::new(vec![log.events[0].clone(), rival.clone()]);
        let mut cache = trusting();
        let forks = counting_cache(&mut cache);
        accept_signed(&cache, &log.events).await;
        let scid = log.scid();

        let theirs = ad4m_identity_core::event_hash(&rival).unwrap();
        for _ in 0..2 {
            match cache.refresh(&store, &scid).await {
                Err(CacheError::Fork(fork)) => {
                    assert_eq!(fork.seq, 1);
                    assert_eq!(&fork.ours, log.chain().hash_at(1).unwrap());
                    assert_eq!(fork.theirs, theirs);
                }
                other => panic!("expected a fork, got {other:?}"),
            }
        }
        assert_eq!(
            forks.load(Ordering::SeqCst),
            1,
            "one identity-fork per fork"
        );
        let recorded = cache.forks(&scid).await.unwrap();
        assert_eq!(recorded.len(), 1);
        assert_eq!(recorded[0].seq, 1);
        assert_eq!(
            cache.chain(&scid).await.unwrap().unwrap().head(),
            log.chain().head(),
            "the cached history stays"
        );
    }

    #[tokio::test]
    async fn an_unsigned_event_at_a_held_seq_raises_no_fork() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let mut cache = in_memory(vec![store.key.multikey()]);
        let forks = counting_cache(&mut cache);
        cache.refresh(&store, &log.scid()).await.unwrap();

        // Another body at the held seq 1 that no key signed: `import` sees a hash-only
        // conflict, but the rival does not step through the log's rules, so it is refused as
        // an event that does not fold. A store with no key raises no fork, however many bodies.
        for extra in ["d1", "d2"] {
            let mut other = log.events[1].clone();
            other.proof.clear();
            if let ad4m_identity_core::Op::Revoke { revoke } = &mut other.op {
                revoke.push(extra.into());
            }
            assert!(matches!(
                cache.ingest(&log.scid(), &[other]).await,
                Err(CacheError::Fold(_))
            ));
        }
        assert_eq!(forks.load(Ordering::SeqCst), 0);
        assert!(cache.forks(&log.scid()).await.unwrap().is_empty());
        assert_eq!(
            cache.chain(&log.scid()).await.unwrap().unwrap().head(),
            log.chain().head()
        );
    }

    #[tokio::test]
    async fn a_signed_competing_event_at_a_held_seq_raises_one_fork() {
        let mut log = TestLog::new(1);
        let at_incept = log.chain();
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let mut cache = in_memory(vec![store.key.multikey()]);
        let forks = counting_cache(&mut cache);
        cache.refresh(&store, &log.scid()).await.unwrap();

        // The master signs another event at seq 1: an `add` instead of the revoke. It holds on
        // the incept, so the conflict is a fork.
        let other_device = SigningKey::from_bytes(&[42; 32]);
        let mut other = ad4m_identity_core::build::add_unsigned(
            &at_incept,
            DeviceKey {
                id: at_incept.state().next_device_id(),
                sign: other_device.key(),
                enc: ad4m_identity_core::encode_x25519_multikey(&[42; 32]),
                life: KeyLife::Standing,
            },
            "offer",
            "coSig",
        )
        .unwrap();
        cosign(&mut other, "m0", &log.master).unwrap();
        rival_holds(&log.scid(), &log.events, &other).unwrap();
        let theirs_hash = ad4m_identity_core::event_hash(&other).unwrap();
        for _ in 0..2 {
            match cache.ingest(&log.scid(), &[other.clone()]).await {
                Err(CacheError::Fork(fork)) => {
                    assert_eq!(fork.seq, 1);
                    assert_eq!(fork.theirs, theirs_hash);
                }
                other => panic!("expected a fork, got {other:?}"),
            }
        }
        assert_eq!(forks.load(Ordering::SeqCst), 1);
        assert_eq!(cache.forks(&log.scid()).await.unwrap().len(), 1);
        assert_eq!(
            cache.chain(&log.scid()).await.unwrap().unwrap().head(),
            log.chain().head()
        );
    }

    #[tokio::test]
    async fn events_that_do_not_fold_are_refused_and_change_nothing() {
        let mut log = TestLog::new(1);
        let cache = in_memory(vec![]);
        cache.ingest(&log.scid(), &log.events[..1]).await.unwrap();
        log.revoke_d1();
        let mut forged = log.events[1].clone();
        forged.proof.clear();
        assert!(matches!(
            cache.ingest(&log.scid(), &[forged]).await,
            Err(CacheError::Fold(_))
        ));
        // A gap: an event two past the head.
        let mut ahead = log.events[1].clone();
        ahead.seq = 5;
        assert!(matches!(
            cache.ingest(&log.scid(), &[ahead]).await,
            Err(CacheError::Gap {
                have: Some(0),
                first: 5
            }) | Err(CacheError::Fold(_))
        ));
        assert_eq!(cache.stored_events(&log.scid(), 0).await.unwrap().len(), 1);
    }

    #[tokio::test]
    async fn a_restart_keeps_the_cache() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        {
            let cache = KelCache::open(&path, vec![store.key.multikey()]).unwrap();
            cache.refresh(&store, &log.scid()).await.unwrap();
        }
        // A new cache on the same file serves the log with no call to the store.
        let cache = KelCache::open(&path, vec![store.key.multikey()]).unwrap();
        let calls = store.log_calls.load(Ordering::SeqCst);
        let chain = cache.chain(&log.scid()).await.unwrap().unwrap();
        assert_eq!(chain.head(), log.chain().head());
        assert_eq!(cache.accepted_head(&log.scid()).unwrap().unwrap().seq, 1);
        assert_eq!(store.log_calls.load(Ordering::SeqCst), calls);
    }

    /// A batch whose first copy of a new event has its proofs stripped (it hashes the same as the
    /// signed one) stores the signed copy: the cache keeps what `import` appended, so a restart
    /// still folds the log.
    #[tokio::test]
    async fn a_stripped_twin_ahead_of_the_signed_event_is_never_stored() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let mut stripped = log.events[1].clone();
        stripped.proof.clear();
        {
            let cache = KelCache::open(&path, vec![]).unwrap();
            cache.ingest(&log.scid(), &log.events[..1]).await.unwrap();
            let appended = cache
                .ingest(&log.scid(), &[stripped, log.events[1].clone()])
                .await
                .unwrap();
            assert!(matches!(appended, Ingested::Appended(_)), "{appended:?}");
            assert_eq!(
                cache.stored_events(&log.scid(), 1).await.unwrap(),
                vec![log.events[1].clone()]
            );
        }
        let cache = KelCache::open(&path, vec![]).unwrap();
        let chain = cache.chain(&log.scid()).await.unwrap().unwrap();
        assert_eq!(chain.head(), log.chain().head());
    }

    /// Callers that wait on one identity's gate share fetches: the first fetch began before
    /// they called, so the next one runs, and every later waiter takes its answer.
    #[tokio::test]
    async fn concurrent_refreshes_of_one_identity_share_the_fetch_after_theirs_began() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let cache = Arc::new(in_memory(vec![store.key.multikey()]));
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let refresh = || {
            let (cache, store, did) = (cache.clone(), store.clone(), log.scid());
            tokio::spawn(async move { cache.refresh(store.as_ref(), &did).await.map(|c| c.head()) })
        };
        let mut tasks = vec![refresh()];
        entered.notified().await;
        for _ in 0..19 {
            tasks.push(refresh());
        }
        // Every caller holds the gate's handle, with the map's: all of them wait on it.
        while cache
            .fetching
            .lock()
            .await
            .get(&log.scid())
            .map(Arc::strong_count)
            != Some(21)
        {
            tokio::task::yield_now().await;
        }
        release.notify_one();
        for task in tasks {
            assert_eq!(task.await.unwrap().unwrap(), log.chain().head());
        }
        // The fetches ran one after another, so no fetch saw another's half-written state.
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 2);
        assert_eq!(cache.stored_events(&log.scid(), 0).await.unwrap().len(), 1);
        assert!(cache.forks(&log.scid()).await.unwrap().is_empty());
        assert!(
            cache.fetching.lock().await.is_empty(),
            "the last fetch drops the gate"
        );
    }

    /// Looking up identities the store does not hold (any caller can name any DID) leaves no
    /// state behind.
    #[tokio::test]
    async fn refreshes_of_unknown_identities_keep_no_gate() {
        let store = FakeStore::new(vec![]);
        let cache = in_memory(vec![store.key.multikey()]);
        for seed in 1..=5 {
            assert!(matches!(
                cache.refresh(&store, &TestLog::new(seed).scid()).await,
                Err(CacheError::NotFound)
            ));
        }
        assert!(cache.fetching.lock().await.is_empty());
    }

    /// The trusted seq is the accepted, store-signed head's: events from `ingest` (any source)
    /// extend the log but not that seq.
    #[tokio::test]
    async fn only_a_store_signed_head_moves_the_trusted_seq() {
        let mut log = TestLog::new(1);
        let cache = trusting();
        cache.ingest(&log.scid(), &log.events).await.unwrap();
        assert!(
            cache.trusted(&log.scid()).await.unwrap().is_none(),
            "no head"
        );

        accept_signed(&cache, &log.events).await;
        let (_, seq) = cache.trusted(&log.scid()).await.unwrap().unwrap();
        assert_eq!(seq, 0);

        log.revoke_d1();
        cache.ingest(&log.scid(), &log.events[1..]).await.unwrap();
        let (chain, seq) = cache.trusted(&log.scid()).await.unwrap().unwrap();
        assert_eq!(
            (chain.head().seq, seq),
            (1, 0),
            "the log holds more than is trusted"
        );

        accept_signed(&cache, &log.events).await;
        assert_eq!(cache.trusted(&log.scid()).await.unwrap().unwrap().1, 1);
    }

    /// An accepted head persists; once its key leaves `identity.storeKeys` it is trusted no
    /// more.
    #[tokio::test]
    async fn a_head_under_a_key_no_longer_configured_is_not_trusted() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let log = TestLog::new(1);
        let store = FakeStore::new(log.events.clone());
        {
            let cache = KelCache::open(&path, vec![store.key.multikey()]).unwrap();
            cache.refresh(&store, &log.scid()).await.unwrap();
            assert!(cache.trusted(&log.scid()).await.unwrap().is_some());
        }
        let cache = KelCache::open(&path, vec![TestStoreKey::new(3).multikey()]).unwrap();
        assert!(cache.accepted_head(&log.scid()).unwrap().is_some());
        assert!(cache.trusted(&log.scid()).await.unwrap().is_none());
    }

    /// A store that answers no log records a fork only against a head the configured store
    /// keys verify: a former store's head is no evidence against this one.
    #[tokio::test]
    async fn no_log_from_a_new_store_records_no_fork_against_a_former_stores_head() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let log = TestLog::new(1);
        let former = FakeStore::new(log.events.clone());
        {
            let cache = KelCache::open(&path, vec![former.key.multikey()]).unwrap();
            cache.refresh(&former, &log.scid()).await.unwrap();
        }
        let new = FakeStore::new(vec![]);
        let cache = KelCache::open(&path, vec![TestStoreKey::new(3).multikey()]).unwrap();
        assert!(matches!(
            cache.refresh(&new, &log.scid()).await,
            Err(CacheError::NotFound)
        ));
        assert!(cache.forks(&log.scid()).await.unwrap().is_empty());

        // Under the key that signed the held head, the same answer is a store behind the cache
        // (restored from a backup older than the log): no fork either.
        let cache = KelCache::open(&path, vec![former.key.multikey()]).unwrap();
        assert!(matches!(
            cache.refresh(&new, &log.scid()).await,
            Err(CacheError::Behind {
                store: None,
                ours: 0
            })
        ));
        assert!(cache.forks(&log.scid()).await.unwrap().is_empty());
        assert!(cache.trusted(&log.scid()).await.unwrap().is_some());
    }
}
