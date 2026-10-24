//! [`KelCache`]: the key logs this executor checked, kept in the executor database (AR-01 4.8,
//! cache rules of 4.8.2).
//!
//! The events sit in the hash-chain module's `SqliteChainStore` tables (type `coasys:kel`) in
//! `ad4m_db.sqlite`; the last signed head the cache accepted for each log sits beside them in
//! `identity_heads`. Every event passes the identity core's `import` (order, links, every rule
//! of the log) before it is stored, and the store only ever extends a log.
//!
//! Rule 3: the cache accepts only events that continue its head. A different event at a seq it
//! holds, or a store that answers a shorter log (a signed head below the cached one, decision
//! 14.2), records a fork with `record_fork`, appends nothing, keeps the cached history and calls
//! the fork listener once per new fork (the executor publishes `identity-fork`). Rule 4: a head
//! from the agent language counts only under a key of `identity.storeKeys`; events that arrive
//! through [`KelCache::ingest`] need none.

use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex as StdMutex};

use ad4m_hash_chain::store::{ChainStore, SqliteChainStore, StoredEntry};
use ad4m_hash_chain::ChainType;
use ad4m_identity_core::{
    ChainError, Event, FoldError, ForkEvidence, Head, ImportOutcome, KelChain,
};
use ad4m_ldk::identity::SignedHead;
use rusqlite::{params, Connection, OptionalExtension};
use tokio::sync::Mutex;

use super::backend::{IdentityBackend, IdentityError};
use super::head::verify_head;

/// The chain type of key logs in the hash-chain tables.
pub const KEL_TYPE: ChainType = "coasys:kel";

/// The most events one log page holds (`identity-store`'s page size).
const PAGE: u64 = 256;

/// Why the cache refused or could not answer.
#[derive(Debug, thiserror::Error)]
pub enum CacheError {
    /// Two different events at one seq, or a store that answered a shorter log. Nothing was
    /// appended; the cached history stays.
    #[error("fork at seq {}: ours {}, theirs {}", .0.seq, .0.ours, .0.theirs)]
    Fork(ForkEvidence),
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

/// The verified key logs, by SCID.
pub struct KelCache {
    store: SqliteChainStore,
    /// `identity_heads`: the last signed head accepted per log.
    meta: StdMutex<Connection>,
    chains: Mutex<HashMap<String, KelChain>>,
    /// One fetch per identity at a time (rule 2).
    fetching: Mutex<HashMap<String, Arc<Mutex<()>>>>,
    store_keys: Vec<String>,
    on_fork: Option<ForkListener>,
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

impl KelCache {
    /// The cache in the database file at `path` (the executor's `ad4m_db.sqlite`), trusting
    /// heads signed by `store_keys` (multikeys).
    pub fn open(path: impl AsRef<Path>, store_keys: Vec<String>) -> Result<Self, CacheError> {
        let store = SqliteChainStore::open(path.as_ref()).map_err(storage)?;
        let meta = Connection::open(path.as_ref()).map_err(storage)?;
        Self::with(store, meta, store_keys)
    }

    fn with(
        store: SqliteChainStore,
        meta: Connection,
        store_keys: Vec<String>,
    ) -> Result<Self, CacheError> {
        meta.execute_batch(META_SCHEMA).map_err(storage)?;
        Ok(KelCache {
            store,
            meta: StdMutex::new(meta),
            chains: Mutex::new(HashMap::new()),
            fetching: Mutex::new(HashMap::new()),
            store_keys,
            on_fork: None,
        })
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

    /// Events from any source (a store, a space, another executor, an app): the same checks,
    /// no head signature (AR-01 4.8.1). `events` are of one log and may come in any order.
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
                let theirs = events
                    .iter()
                    .find(|e| e.seq == fork.seq)
                    .and_then(|e| e.to_canonical_json().ok())
                    .unwrap_or_default();
                self.record_fork(&fork, &theirs).await?;
                Err(CacheError::Fork(fork))
            }
            ImportOutcome::Appended { head, .. } => {
                let mut fresh: Vec<&Event> = events.iter().filter(|e| e.seq > before.seq).collect();
                fresh.sort_by_key(|e| e.seq);
                fresh.dedup_by_key(|e| e.seq);
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
        let mut sorted: Vec<Event> = events.to_vec();
        sorted.sort_by_key(|e| e.seq);
        sorted.dedup_by(|a, b| a.seq == b.seq && a == b);
        if sorted[0].seq != 0 {
            return Err(CacheError::Gap {
                have: None,
                first: sorted[0].seq,
            });
        }
        let chain = ad4m_identity_core::fold(&sorted).map_err(CacheError::Fold)?;
        if chain.id() != scid {
            return Err(CacheError::Fold(ChainError::WrongChain {
                want: scid.to_string(),
                got: chain.id().clone(),
            }));
        }
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
    /// verify (rule 4); a head below the cached one is a fork (decision 14.2); the events must
    /// continue the cached log up to the head. Then the head is the log's accepted head.
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
                let fork = ForkEvidence {
                    chain: scid.to_string(),
                    seq: ours.seq,
                    ours: ours.hash,
                    theirs: head.hash.clone(),
                };
                let theirs = serde_json::to_string(head).map_err(storage)?;
                self.record_fork(&fork, &theirs).await?;
                return Err(CacheError::Fork(fork));
            }
        }
        self.ingest_locked(&mut chains, scid, events).await?;
        let Some(chain) = chains.get(scid) else {
            return Ok(None);
        };
        let ours = chain.head();
        if ours.seq == head.seq && ours.hash != head.hash {
            let fork = ForkEvidence {
                chain: scid.to_string(),
                seq: ours.seq,
                ours: ours.hash.clone(),
                theirs: head.hash.clone(),
            };
            let theirs = serde_json::to_string(head).map_err(storage)?;
            self.record_fork(&fork, &theirs).await?;
            return Err(CacheError::Fork(fork));
        }
        if ours.seq == head.seq {
            self.accept_head(head)?;
        }
        Ok(Some(ours))
    }

    /// Refreshes `did`'s log from the agent language: reads the pages from the cached head on
    /// (the head's own event included, so a different event there shows as a fork) and accepts
    /// them with [`KelCache::accept`]. One fetch per identity at a time; the identity's gate is
    /// dropped when no other fetch waits on it, so looking up many identities keeps no state.
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
                .or_insert_with(|| Arc::new(Mutex::new(())))
                .clone()
        };
        let result = {
            let _one_fetch = gate.lock().await;
            self.fetch(backend, &scid).await
        };
        let mut fetching = self.fetching.lock().await;
        // The map's handle and ours: nobody else waits on this gate.
        if Arc::strong_count(&gate) <= 2 {
            fetching.remove(&scid);
        }
        result
    }

    /// The pages of [`KelCache::refresh`], under the identity's gate.
    async fn fetch(
        &self,
        backend: &dyn IdentityBackend,
        scid: &str,
    ) -> Result<KelChain, CacheError> {
        let did = ad4m_identity_core::did_of(scid);
        loop {
            let cached = self.chain(scid).await?;
            let from = cached.as_ref().map_or(0, |c| c.head().seq);
            let Some(page) = backend.log(&did, from).await? else {
                return match cached {
                    // The store answers no log while the cache holds one: the shortest log.
                    Some(chain) => {
                        let ours = chain.head();
                        let fork = ForkEvidence {
                            chain: scid.to_string(),
                            seq: ours.seq,
                            ours: ours.hash,
                            theirs: String::new(),
                        };
                        self.record_fork(&fork, "null").await?;
                        Err(CacheError::Fork(fork))
                    }
                    None => Err(CacheError::NotFound),
                };
            };
            let events = page
                .events
                .iter()
                .map(Event::from_json)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| CacheError::Fold(ChainError::Policy(e)))?;
            let reached = self.accept(scid, &page.head, &events).await?;
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
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::identity::head::test_store_key::TestStoreKey;
    use ad4m_identity_core::build::{cosign, incept_human, revoke, Signer};
    use ad4m_identity_core::{DeviceKey, KeyLife};
    use ad4m_ldk::identity::{
        AppendRequest, Claim, ClaimReceipt, LogPage, OfferListing, OfferOpened, OfferProgress,
        SignedOffer, SignedRequest,
    };
    use async_trait::async_trait;
    use ed25519_dalek::SigningKey;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// A cache in memory, trusting heads signed by `store_keys`.
    pub(crate) fn in_memory(store_keys: Vec<String>) -> KelCache {
        let store = SqliteChainStore::open_in_memory().unwrap();
        let meta = Connection::open_in_memory().unwrap();
        KelCache::with(store, meta, store_keys).unwrap()
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
    }

    impl FakeStore {
        pub fn new(events: Vec<Event>) -> Self {
            FakeStore {
                key: TestStoreKey::new(9),
                events: StdMutex::new(events),
                log_calls: AtomicUsize::new(0),
            }
        }
    }

    #[async_trait]
    impl IdentityBackend for FakeStore {
        async fn log(&self, _did: &str, from: u64) -> Result<Option<LogPage>, IdentityError> {
            self.log_calls.fetch_add(1, Ordering::SeqCst);
            let events = self.events.lock().unwrap().clone();
            if events.is_empty() {
                return Ok(None);
            }
            let chain = ad4m_identity_core::fold(&events).unwrap();
            let head = chain.head();
            let page: Vec<serde_json::Value> = events
                .iter()
                .filter(|e| e.seq >= from)
                .map(|e| serde_json::to_value(e).unwrap())
                .collect();
            Ok(Some(LogPage {
                appended_at: vec![0; page.len()],
                events: page,
                head: self.key.sign_head(chain.id(), head.seq, &head.hash, 1),
                note: None,
                vouched: false,
                pauses: vec![],
            }))
        }
        async fn append(&self, _: &str, _: &AppendRequest) -> Result<SignedHead, IdentityError> {
            unimplemented_call("append")
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

    #[tokio::test]
    async fn a_shorter_log_raises_one_fork_and_keeps_history() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let mut cache = in_memory(vec![store.key.multikey()]);
        let forks = counting_cache(&mut cache);
        cache.refresh(&store, &log.scid()).await.unwrap();

        // The store now withholds the revoke.
        store.events.lock().unwrap().truncate(1);
        for _ in 0..2 {
            match cache.refresh(&store, &log.scid()).await {
                Err(CacheError::Fork(fork)) => {
                    assert_eq!(fork.seq, 1);
                    assert_eq!(fork.ours, log.chain().head().hash);
                    assert_eq!(fork.theirs, log.chain().hash_at(0).unwrap().clone());
                }
                other => panic!("expected a fork, got {other:?}"),
            }
        }
        assert_eq!(
            forks.load(Ordering::SeqCst),
            1,
            "one identity-fork per fork"
        );
        assert_eq!(cache.forks(&log.scid()).await.unwrap().len(), 1);
        let kept = cache.chain(&log.scid()).await.unwrap().unwrap();
        assert_eq!(kept.head(), log.chain().head(), "the cached history stays");
        assert_eq!(cache.accepted_head(&log.scid()).unwrap().unwrap().seq, 1);
    }

    #[tokio::test]
    async fn a_different_event_at_a_held_seq_raises_one_fork() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = FakeStore::new(log.events.clone());
        let mut cache = in_memory(vec![store.key.multikey()]);
        let forks = counting_cache(&mut cache);
        cache.refresh(&store, &log.scid()).await.unwrap();

        // Another event at the held seq 1. `import` compares the hash of an event at a held
        // seq before any rule, so a changed body is enough to show the fork.
        let mut other = log.events[1].clone();
        other.proof.clear();
        if let ad4m_identity_core::Op::Revoke { revoke } = &mut other.op {
            revoke.push("d1".into());
        }
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

    #[tokio::test]
    async fn concurrent_refreshes_of_one_identity_fetch_once() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let cache = Arc::new(in_memory(vec![store.key.multikey()]));
        let mut tasks = Vec::new();
        for _ in 0..20 {
            let (cache, store, did) = (cache.clone(), store.clone(), log.scid());
            tasks.push(tokio::spawn(async move {
                cache.refresh(store.as_ref(), &did).await.map(|c| c.head())
            }));
        }
        for task in tasks {
            assert_eq!(task.await.unwrap().unwrap(), log.chain().head());
        }
        // The fetches ran one after another: each later one found the log current, so the
        // log was stored once and no fetch saw another's half-written state.
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 20);
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
}
