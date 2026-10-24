//! The SQLite file: the hash-chain module's tables and the store's own (AR-01 4.5.1, AR-02 4.3.5).
//!
//! The store keeps one connection behind a mutex; every write runs in one immediate
//! transaction. Reads replay a log from `chain_entries` with the identity core each time: a log
//! is short, and the store keeps no second copy of what the module's tables hold.

use std::path::Path;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use ad4m_hash_chain::store::SqliteChainStore;
use ad4m_hash_chain::ChainType;
use ad4m_identity_core::{fold, Event, KelChain, KeyState};
use ad4m_ldk::identity::{KeyIndexEntry, SignedHead};
use rusqlite::{params, Connection, OptionalExtension};

use crate::error::ApiError;

/// The chain type of key logs in the module's tables.
pub const KEL: ChainType = "coasys:kel";

const SCHEMA: &str = "
CREATE TABLE IF NOT EXISTS logs (
    scid        TEXT PRIMARY KEY,
    created_at  INTEGER NOT NULL,
    admitted_by TEXT NOT NULL CHECK (admitted_by IN ('vouch', 'brake'))
);
CREATE TABLE IF NOT EXISTS heads (
    scid        TEXT PRIMARY KEY,
    signed_head TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS keys (
    sign    TEXT PRIMARY KEY,
    scid    TEXT NOT NULL,
    key_id  TEXT NOT NULL,
    added   INTEGER NOT NULL,
    revoked INTEGER,
    bot     TEXT,
    expires INTEGER
);
CREATE TABLE IF NOT EXISTS agents (
    did       TEXT PRIMARY KEY,
    record    TEXT NOT NULL,
    version   INTEGER NOT NULL,
    stored_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS nonces (
    nonce   TEXT PRIMARY KEY,
    seen_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS feed (
    cursor INTEGER PRIMARY KEY AUTOINCREMENT,
    scid   TEXT NOT NULL,
    seq    INTEGER NOT NULL,
    hash   TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS offers (
    scid          TEXT NOT NULL,
    offer_id      TEXT NOT NULL,
    signed        TEXT NOT NULL,
    kind          TEXT NOT NULL CHECK (kind IN ('device', 'node', 'bot')),
    commit_hash   TEXT NOT NULL,
    bound_key     TEXT,
    expires       INTEGER NOT NULL,
    state         TEXT NOT NULL CHECK (state IN ('open', 'claimed', 'spent', 'expired', 'cancelled',
                                                 'contested', 'locked', 'superseded')),
    wrong_secrets INTEGER NOT NULL DEFAULT 0,
    spent_seq     INTEGER,
    created_at    INTEGER NOT NULL,
    ended_at      INTEGER,
    PRIMARY KEY (scid, offer_id)
);
CREATE INDEX IF NOT EXISTS offers_open ON offers(scid) WHERE state IN ('open', 'claimed');
CREATE TABLE IF NOT EXISTS claims (
    scid       TEXT NOT NULL,
    offer_id   TEXT NOT NULL,
    claim      TEXT NOT NULL,
    device_key TEXT NOT NULL,
    claimed_at INTEGER NOT NULL,
    PRIMARY KEY (scid, offer_id)
);
";

/// The store's database.
pub struct Db {
    conn: Mutex<Connection>,
}

impl Db {
    /// Opens (or creates) the file and its tables. WAL, and a 5 s wait for another connection's lock.
    pub fn open(path: impl AsRef<Path>) -> Result<Db, ApiError> {
        let conn = Connection::open(path)?;
        conn.busy_timeout(Duration::from_secs(5))?;
        conn.pragma_update(None, "journal_mode", "WAL")?;
        Self::from_connection(conn)
    }

    /// A database in memory.
    pub fn open_in_memory() -> Result<Db, ApiError> {
        Self::from_connection(Connection::open_in_memory()?)
    }

    fn from_connection(conn: Connection) -> Result<Db, ApiError> {
        SqliteChainStore::create_tables(&conn)?;
        conn.execute_batch(SCHEMA)?;
        Ok(Db {
            conn: Mutex::new(conn),
        })
    }

    pub fn lock(&self) -> Result<MutexGuard<'_, Connection>, ApiError> {
        self.conn
            .lock()
            .map_err(|_| ApiError::internal("database lock poisoned"))
    }
}

pub(crate) fn to_sql(n: u64) -> Result<i64, ApiError> {
    i64::try_from(n).map_err(|_| ApiError::invalid("malformed", format!("{n} is too large")))
}

pub(crate) fn from_sql(n: i64) -> u64 {
    n.max(0) as u64
}

/// The events of a log, replayed: `None` when the store holds no event of `scid`.
pub fn load_chain(conn: &Connection, scid: &str) -> Result<Option<KelChain>, ApiError> {
    let mut stmt = conn.prepare_cached(
        "SELECT json FROM chain_entries WHERE type = ?1 AND chain = ?2 ORDER BY seq",
    )?;
    let texts = stmt
        .query_map(params![KEL, scid], |row| row.get::<_, String>(0))?
        .collect::<Result<Vec<_>, _>>()?;
    if texts.is_empty() {
        return Ok(None);
    }
    let events = texts
        .iter()
        .map(|t| {
            let value: serde_json::Value = serde_json::from_str(t)
                .map_err(|e| ApiError::internal(format!("stored event of {scid}: {e}")))?;
            Event::from_json(&value)
                .map_err(|e| ApiError::internal(format!("stored event of {scid}: {e}")))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let chain = fold(&events)
        .map_err(|e| ApiError::internal(format!("stored log {scid} does not replay: {e}")))?;
    Ok(Some(chain))
}

/// Up to `limit` stored events of `scid` from `from`: their JSON and when they were stored.
pub fn events_from(
    conn: &Connection,
    scid: &str,
    from: u64,
    limit: u32,
) -> Result<Vec<(serde_json::Value, u64)>, ApiError> {
    let mut stmt = conn.prepare_cached(
        "SELECT json, stored_at FROM chain_entries
         WHERE type = ?1 AND chain = ?2 AND seq >= ?3 ORDER BY seq LIMIT ?4",
    )?;
    let rows = stmt
        .query_map(params![KEL, scid, to_sql(from)?, limit], |row| {
            Ok((row.get::<_, String>(0)?, from_sql(row.get(1)?)))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    rows.into_iter()
        .map(|(text, at)| {
            serde_json::from_str(&text)
                .map(|v| (v, at))
                .map_err(|e| ApiError::internal(format!("stored event of {scid}: {e}")))
        })
        .collect()
}

/// The latest signed head of `scid`.
pub fn signed_head(conn: &Connection, scid: &str) -> Result<Option<SignedHead>, ApiError> {
    let text: Option<String> = conn
        .query_row(
            "SELECT signed_head FROM heads WHERE scid = ?1",
            params![scid],
            |row| row.get(0),
        )
        .optional()?;
    text.map(|t| {
        serde_json::from_str(&t).map_err(|e| ApiError::internal(format!("stored head: {e}")))
    })
    .transpose()
}

pub(crate) fn put_head(conn: &Connection, head: &SignedHead) -> Result<(), ApiError> {
    let text = serde_json::to_string(head).map_err(|e| ApiError::internal(e.to_string()))?;
    conn.execute(
        "INSERT INTO heads (scid, signed_head) VALUES (?1, ?2)
         ON CONFLICT(scid) DO UPDATE SET signed_head = excluded.signed_head",
        params![head.scid, text],
    )?;
    Ok(())
}

/// The identity a signing key sits in, if any (rule A4 looks across every log).
pub fn key_owner(conn: &Connection, sign: &str) -> Result<Option<String>, ApiError> {
    Ok(conn
        .query_row(
            "SELECT scid FROM keys WHERE sign = ?1",
            params![sign],
            |row| row.get(0),
        )
        .optional()?)
}

/// Writes the key-index rows of every key of `state` as they stand at `head_seq`.
pub(crate) fn put_keys(conn: &Connection, state: &KeyState, head_seq: u64) -> Result<(), ApiError> {
    let mut stmt = conn.prepare_cached(
        "INSERT INTO keys (sign, scid, key_id, added, revoked, bot, expires)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)
         ON CONFLICT(sign) DO UPDATE SET revoked = excluded.revoked, bot = excluded.bot,
                                         expires = excluded.expires
         WHERE keys.scid = excluded.scid",
    )?;
    for record in state.masters.iter().chain(state.keys.values()) {
        let bot = state.bot_scope_at(&record.id, head_seq);
        let expires = record.expires.or(bot.map(|b| b.expires));
        let bot = bot
            .map(serde_json::to_string)
            .transpose()
            .map_err(|e| ApiError::internal(e.to_string()))?;
        stmt.execute(params![
            record.sign,
            state.scid,
            record.id,
            to_sql(record.added)?,
            record.revoked.map(to_sql).transpose()?,
            bot,
            expires.map(to_sql).transpose()?,
        ])?;
    }
    Ok(())
}

/// The key-index entry of `sign`, with the head it describes.
pub fn key_entry(conn: &Connection, sign: &str) -> Result<Option<KeyIndexEntry>, ApiError> {
    type Row = (
        String,
        String,
        i64,
        Option<i64>,
        Option<String>,
        Option<i64>,
    );
    let row: Option<Row> = conn
        .query_row(
            "SELECT scid, key_id, added, revoked, bot, expires FROM keys WHERE sign = ?1",
            params![sign],
            |r| {
                Ok((
                    r.get(0)?,
                    r.get(1)?,
                    r.get(2)?,
                    r.get(3)?,
                    r.get(4)?,
                    r.get(5)?,
                ))
            },
        )
        .optional()?;
    let Some((scid, key_id, added, revoked, bot, expires)) = row else {
        return Ok(None);
    };
    let head = signed_head(conn, &scid)?
        .ok_or_else(|| ApiError::internal(format!("key of {scid} without a head")))?;
    let bot = bot
        .map(|b| serde_json::from_str(&b))
        .transpose()
        .map_err(|e| ApiError::internal(e.to_string()))?;
    Ok(Some(KeyIndexEntry {
        did: ad4m_identity_core::did_of(&scid),
        key_id,
        added: from_sql(added),
        revoked: revoked.map(from_sql),
        bot,
        expires: expires.map(from_sql),
        paused: false,
        head,
    }))
}
