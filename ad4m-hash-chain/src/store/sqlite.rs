//! [`SqliteChainStore`]: chains in SQLite (feature `sqlite`), for the shared store and the
//! executor database.
//!
//! Tables (created by [`SqliteChainStore::create_tables`]):
//! `chain_heads(type, chain, seq, hash)`, `chain_entries(type, chain, seq, hash, json, stored_at)`,
//! `chain_forks(type, chain, seq, ours, theirs, theirs_json, seen_at)`.

use async_trait::async_trait;
use rusqlite::{params, Connection, OptionalExtension, Transaction, TransactionBehavior};
use std::path::Path;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use super::{continued_head, ChainStore, StoreError, StoredEntry};
use crate::{ChainId, ChainType, ForkEvidence, Head};

const SCHEMA: &str = "
CREATE TABLE IF NOT EXISTS chain_heads (
    type  TEXT NOT NULL,
    chain TEXT NOT NULL,
    seq   INTEGER NOT NULL,
    hash  TEXT NOT NULL,
    PRIMARY KEY (type, chain)
);
CREATE TABLE IF NOT EXISTS chain_entries (
    type      TEXT NOT NULL,
    chain     TEXT NOT NULL,
    seq       INTEGER NOT NULL,
    hash      TEXT NOT NULL,
    json      TEXT NOT NULL,
    stored_at INTEGER NOT NULL,
    PRIMARY KEY (type, chain, seq)
);
CREATE TABLE IF NOT EXISTS chain_forks (
    type        TEXT NOT NULL,
    chain       TEXT NOT NULL,
    seq         INTEGER NOT NULL,
    ours        TEXT NOT NULL,
    theirs      TEXT NOT NULL,
    theirs_json TEXT NOT NULL,
    seen_at     INTEGER NOT NULL DEFAULT (CAST(strftime('%s', 'now') AS INTEGER)),
    PRIMARY KEY (type, chain, seq, theirs)
);
";

fn backend(e: rusqlite::Error) -> StoreError {
    StoreError::Backend(e.to_string())
}

fn to_sql(n: u64) -> Result<i64, StoreError> {
    i64::try_from(n).map_err(|_| StoreError::Backend(format!("{n} does not fit an SQLite integer")))
}

fn from_sql(n: i64) -> u64 {
    n.max(0) as u64
}

/// A [`ChainStore`] on one SQLite connection. Calls block the calling task for the time
/// of one statement or transaction.
pub struct SqliteChainStore {
    conn: Mutex<Connection>,
}

impl SqliteChainStore {
    /// Opens (or creates) the database file and its tables. Waits up to 5 s for a lock
    /// another connection holds.
    pub fn open(path: impl AsRef<Path>) -> Result<Self, StoreError> {
        let conn = Connection::open(path).map_err(backend)?;
        conn.busy_timeout(Duration::from_secs(5)).map_err(backend)?;
        Self::from_connection(conn)
    }

    /// A store in a private in-memory database.
    pub fn open_in_memory() -> Result<Self, StoreError> {
        Self::from_connection(Connection::open_in_memory().map_err(backend)?)
    }

    /// Takes an open connection and creates the tables it lacks.
    pub fn from_connection(conn: Connection) -> Result<Self, StoreError> {
        Self::create_tables(&conn)?;
        Ok(SqliteChainStore {
            conn: Mutex::new(conn),
        })
    }

    /// Creates the three tables in a database the caller owns (idempotent).
    pub fn create_tables(conn: &Connection) -> Result<(), StoreError> {
        conn.execute_batch(SCHEMA).map_err(backend)
    }

    /// The stored head, read through any connection or transaction.
    pub fn head_in(
        conn: &Connection,
        ty: ChainType,
        chain: &ChainId,
    ) -> Result<Option<Head>, StoreError> {
        conn.query_row(
            "SELECT seq, hash FROM chain_heads WHERE type = ?1 AND chain = ?2",
            params![ty, chain],
            |row| {
                Ok(Head {
                    seq: from_sql(row.get(0)?),
                    hash: row.get(1)?,
                })
            },
        )
        .optional()
        .map_err(backend)
    }

    /// [`ChainStore::append`] inside the caller's transaction, so the caller commits the
    /// append with its own writes (the identity store's one-transaction rule) or rolls all of
    /// them back.
    /// On `Err` this writes nothing (it runs inside its own savepoint).
    pub fn append_in(
        tx: &Transaction<'_>,
        ty: ChainType,
        chain: &ChainId,
        expect: Option<&Head>,
        entries: &[StoredEntry],
    ) -> Result<Head, StoreError> {
        tx.execute_batch("SAVEPOINT ad4m_chain_append")
            .map_err(backend)?;
        match append_steps(tx, ty, chain, expect, entries) {
            Ok(head) => {
                tx.execute_batch("RELEASE ad4m_chain_append")
                    .map_err(backend)?;
                Ok(head)
            }
            Err(e) => {
                tx.execute_batch("ROLLBACK TO ad4m_chain_append; RELEASE ad4m_chain_append")
                    .map_err(backend)?;
                Err(e)
            }
        }
    }

    fn lock(&self) -> Result<MutexGuard<'_, Connection>, StoreError> {
        self.conn
            .lock()
            .map_err(|_| StoreError::Backend("sqlite store lock poisoned".into()))
    }
}

fn append_steps(
    tx: &Transaction<'_>,
    ty: ChainType,
    chain: &ChainId,
    expect: Option<&Head>,
    entries: &[StoredEntry],
) -> Result<Head, StoreError> {
    let current = SqliteChainStore::head_in(tx, ty, chain)?;
    if current.as_ref() != expect {
        return Err(StoreError::Conflict(current));
    }
    let head = continued_head(expect, entries)?;
    // Compare-and-set on the head row, so a writer on another connection that slipped in
    // between the read and this write still loses.
    let changed = match expect {
        Some(old) => tx
            .execute(
                "UPDATE chain_heads SET seq = ?1, hash = ?2
                 WHERE type = ?3 AND chain = ?4 AND seq = ?5 AND hash = ?6",
                params![
                    to_sql(head.seq)?,
                    head.hash,
                    ty,
                    chain,
                    to_sql(old.seq)?,
                    old.hash
                ],
            )
            .map_err(backend)?,
        None => tx
            .execute(
                "INSERT OR IGNORE INTO chain_heads (type, chain, seq, hash) VALUES (?1, ?2, ?3, ?4)",
                params![ty, chain, to_sql(head.seq)?, head.hash],
            )
            .map_err(backend)?,
    };
    if changed != 1 {
        return Err(StoreError::Conflict(SqliteChainStore::head_in(
            tx, ty, chain,
        )?));
    }
    let mut insert = tx
        .prepare_cached(
            "INSERT INTO chain_entries (type, chain, seq, hash, json, stored_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
        )
        .map_err(backend)?;
    for entry in entries {
        insert
            .execute(params![
                ty,
                chain,
                to_sql(entry.seq)?,
                entry.hash,
                entry.json,
                to_sql(entry.stored_at)?
            ])
            .map_err(backend)?;
    }
    Ok(head)
}

#[async_trait]
impl ChainStore for SqliteChainStore {
    async fn head(&self, ty: ChainType, chain: &ChainId) -> Result<Option<Head>, StoreError> {
        let conn = self.lock()?;
        Self::head_in(&conn, ty, chain)
    }

    async fn range(
        &self,
        ty: ChainType,
        chain: &ChainId,
        from: u64,
        limit: u32,
    ) -> Result<Vec<StoredEntry>, StoreError> {
        let conn = self.lock()?;
        let mut stmt = conn
            .prepare_cached(
                "SELECT seq, hash, json, stored_at FROM chain_entries
                 WHERE type = ?1 AND chain = ?2 AND seq >= ?3 ORDER BY seq LIMIT ?4",
            )
            .map_err(backend)?;
        let rows = stmt
            .query_map(params![ty, chain, to_sql(from)?, limit], |row| {
                Ok(StoredEntry {
                    seq: from_sql(row.get(0)?),
                    hash: row.get(1)?,
                    json: row.get(2)?,
                    stored_at: from_sql(row.get(3)?),
                })
            })
            .map_err(backend)?;
        rows.collect::<Result<Vec<_>, _>>().map_err(backend)
    }

    async fn append(
        &self,
        ty: ChainType,
        chain: &ChainId,
        expect: Option<&Head>,
        entries: &[StoredEntry],
    ) -> Result<Head, StoreError> {
        let mut conn = self.lock()?;
        let tx = conn
            .transaction_with_behavior(TransactionBehavior::Immediate)
            .map_err(backend)?;
        let head = Self::append_in(&tx, ty, chain, expect, entries)?;
        tx.commit().map_err(backend)?;
        Ok(head)
    }

    async fn record_fork(
        &self,
        ty: ChainType,
        fork: &ForkEvidence,
        theirs_json: &str,
    ) -> Result<(), StoreError> {
        let conn = self.lock()?;
        conn.execute(
            "INSERT OR IGNORE INTO chain_forks (type, chain, seq, ours, theirs, theirs_json)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                ty,
                fork.chain,
                to_sql(fork.seq)?,
                fork.ours,
                fork.theirs,
                theirs_json
            ],
        )
        .map(|_| ())
        .map_err(backend)
    }

    async fn forks(&self, ty: ChainType, chain: &ChainId) -> Result<Vec<ForkEvidence>, StoreError> {
        let conn = self.lock()?;
        let mut stmt = conn
            .prepare_cached(
                "SELECT seq, ours, theirs FROM chain_forks
                 WHERE type = ?1 AND chain = ?2 ORDER BY seq, rowid",
            )
            .map_err(backend)?;
        let rows = stmt
            .query_map(params![ty, chain], |row| {
                Ok(ForkEvidence {
                    chain: chain.clone(),
                    seq: from_sql(row.get(0)?),
                    ours: row.get(1)?,
                    theirs: row.get(2)?,
                })
            })
            .map_err(backend)?;
        rows.collect::<Result<Vec<_>, _>>().map_err(backend)
    }
}
