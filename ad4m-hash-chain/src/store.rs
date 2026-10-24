//! Storage of chains (feature `store`): the [`ChainStore`] trait and [`MemoryChainStore`]. The
//! SQLite adapter is [`SqliteChainStore`] (feature `sqlite`).
//!
//! A store checks no hash, link or proof: the caller runs `Chain::append` or `import` first.
//! It checks only that an append continues the head the caller expects, so two writers that
//! race on one chain get one success and one [`StoreError::Conflict`].

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Mutex;

use crate::{entry_hash_json, CanonError, ChainId, ChainType, Entry, ForkEvidence, Hash, Head};

#[cfg(feature = "sqlite")]
mod sqlite;
#[cfg(feature = "sqlite")]
pub use sqlite::SqliteChainStore;

/// One stored entry: its seq, its hash (computed by the holder), its JCS text with proofs,
/// and when this holder stored it (store data; never part of the entry).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StoredEntry {
    pub seq: u64,
    pub hash: Hash,
    pub json: String,
    pub stored_at: u64,
}

impl StoredEntry {
    /// The stored form of `entry`: its hash and its JCS text, proofs included.
    pub fn from_entry<E: Entry>(entry: &E, stored_at: u64) -> Result<Self, CanonError> {
        let value = serde_json::to_value(entry).map_err(|e| CanonError::Serde(e.to_string()))?;
        Ok(StoredEntry {
            seq: entry.seq(),
            hash: entry_hash_json(&value)?,
            json: crate::canonical_json(&value)?,
            stored_at,
        })
    }

    /// Parses the stored text back into an entry.
    pub fn entry<E: Entry>(&self) -> Result<E, serde_json::Error> {
        serde_json::from_str(&self.json)
    }
}

/// Why a store refused or failed.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum StoreError {
    /// The stored head is not the head the append expected. Carries the stored head
    /// (`None` when the store holds no entry of the chain).
    #[error("append conflict: the stored head is {0:?}")]
    Conflict(Option<Head>),
    #[error("store backend: {0}")]
    Backend(String),
}

#[async_trait]
pub trait ChainStore: Send + Sync {
    async fn head(&self, ty: ChainType, chain: &ChainId) -> Result<Option<Head>, StoreError>;
    /// Up to `limit` entries with `seq >= from`, in seq order.
    async fn range(
        &self,
        ty: ChainType,
        chain: &ChainId,
        from: u64,
        limit: u32,
    ) -> Result<Vec<StoredEntry>, StoreError>;
    /// Appends only when the stored head equals `expect` (None for a new chain). The store checks nothing else:
    /// the caller ran `Chain::append` or `import` first.
    /// (It does refuse, as `Backend`, an empty batch or entries that do not run on from `expect`.)
    async fn append(
        &self,
        ty: ChainType,
        chain: &ChainId,
        expect: Option<&Head>,
        entries: &[StoredEntry],
    ) -> Result<Head, StoreError>;
    /// Keeps the evidence and the other entry's text. Recording the same fork twice keeps one record.
    async fn record_fork(
        &self,
        ty: ChainType,
        fork: &ForkEvidence,
        theirs_json: &str,
    ) -> Result<(), StoreError>;
    /// Every fork recorded for the chain, in seq order.
    async fn forks(&self, ty: ChainType, chain: &ChainId) -> Result<Vec<ForkEvidence>, StoreError>;
}

/// Checks that `entries` run on from `expect` without a hole, and gives the new head.
pub(crate) fn continued_head(
    expect: Option<&Head>,
    entries: &[StoredEntry],
) -> Result<Head, StoreError> {
    let start = expect.map_or(0, |h| h.seq + 1);
    for (want, entry) in (start..).zip(entries) {
        if entry.seq != want {
            return Err(StoreError::Backend(format!(
                "append of seq {} does not continue at seq {want}",
                entry.seq
            )));
        }
    }
    let last = entries
        .last()
        .ok_or_else(|| StoreError::Backend("append of no entries".into()))?;
    Ok(Head {
        seq: last.seq,
        hash: last.hash.clone(),
    })
}

type Key = (String, ChainId);

#[derive(Default)]
struct MemoryInner {
    entries: HashMap<Key, Vec<StoredEntry>>,
    forks: HashMap<Key, Vec<(ForkEvidence, String)>>,
}

/// A [`ChainStore`] in memory, for tests and short-lived holders.
#[derive(Default)]
pub struct MemoryChainStore {
    inner: Mutex<MemoryInner>,
}

impl MemoryChainStore {
    pub fn new() -> Self {
        Self::default()
    }

    fn lock(&self) -> Result<std::sync::MutexGuard<'_, MemoryInner>, StoreError> {
        self.inner
            .lock()
            .map_err(|_| StoreError::Backend("memory store lock poisoned".into()))
    }
}

fn head_of(entries: Option<&Vec<StoredEntry>>) -> Option<Head> {
    entries.and_then(|v| v.last()).map(|e| Head {
        seq: e.seq,
        hash: e.hash.clone(),
    })
}

#[async_trait]
impl ChainStore for MemoryChainStore {
    async fn head(&self, ty: ChainType, chain: &ChainId) -> Result<Option<Head>, StoreError> {
        let inner = self.lock()?;
        Ok(head_of(inner.entries.get(&(ty.to_string(), chain.clone()))))
    }

    async fn range(
        &self,
        ty: ChainType,
        chain: &ChainId,
        from: u64,
        limit: u32,
    ) -> Result<Vec<StoredEntry>, StoreError> {
        let inner = self.lock()?;
        Ok(inner
            .entries
            .get(&(ty.to_string(), chain.clone()))
            .map(|v| {
                v.iter()
                    .filter(|e| e.seq >= from)
                    .take(limit as usize)
                    .cloned()
                    .collect()
            })
            .unwrap_or_default())
    }

    async fn append(
        &self,
        ty: ChainType,
        chain: &ChainId,
        expect: Option<&Head>,
        entries: &[StoredEntry],
    ) -> Result<Head, StoreError> {
        let mut inner = self.lock()?;
        let key = (ty.to_string(), chain.clone());
        let current = head_of(inner.entries.get(&key));
        if current.as_ref() != expect {
            return Err(StoreError::Conflict(current));
        }
        let head = continued_head(expect, entries)?;
        inner
            .entries
            .entry(key)
            .or_default()
            .extend_from_slice(entries);
        Ok(head)
    }

    async fn record_fork(
        &self,
        ty: ChainType,
        fork: &ForkEvidence,
        theirs_json: &str,
    ) -> Result<(), StoreError> {
        let mut inner = self.lock()?;
        let list = inner
            .forks
            .entry((ty.to_string(), fork.chain.clone()))
            .or_default();
        if !list
            .iter()
            .any(|(f, _)| f.seq == fork.seq && f.theirs == fork.theirs)
        {
            list.push((fork.clone(), theirs_json.to_string()));
        }
        Ok(())
    }

    async fn forks(&self, ty: ChainType, chain: &ChainId) -> Result<Vec<ForkEvidence>, StoreError> {
        let inner = self.lock()?;
        let mut forks: Vec<ForkEvidence> = inner
            .forks
            .get(&(ty.to_string(), chain.clone()))
            .map(|v| v.iter().map(|(f, _)| f.clone()).collect())
            .unwrap_or_default();
        forks.sort_by_key(|f| f.seq);
        Ok(forks)
    }
}
