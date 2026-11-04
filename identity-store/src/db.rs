//! The SQLite file: the hash-chain module's tables and the store's own.
//!
//! The store keeps one connection behind a mutex; every write runs in one immediate
//! transaction. A log is replayed from `chain_entries` with the identity core when its fold for
//! the stored head is not held in memory (`held.rs`); the database keeps no second copy of the
//! folded state. What it keeps beside the events is the key index at the stored head, written
//! in the append's transaction: every key's row in `keys` (masters marked `master = 1`, with the
//! seqs that added and retired them) and the next-key commitment in `logs.next`. That is what a
//! write is authenticated against before any replay (`log.rs`, `request.rs`, `offers.rs`).

use std::path::Path;
use std::sync::{Mutex, MutexGuard};
use std::time::Duration;

use ad4m_hash_chain::store::SqliteChainStore;
use ad4m_hash_chain::{ChainType, ForkEvidence, Multikey};
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
    admitted_by TEXT NOT NULL CHECK (admitted_by IN ('vouch', 'brake')),
    next        TEXT
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
    expires INTEGER,
    master  INTEGER NOT NULL CHECK (master IN (0, 1))
);
CREATE INDEX IF NOT EXISTS keys_of_log ON keys(scid, key_id);
CREATE INDEX IF NOT EXISTS masters_of_log ON keys(scid, added) WHERE master = 1;
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
                                                 'contested', 'superseded')),
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
CREATE TABLE IF NOT EXISTS fork_signers (
    scid   TEXT NOT NULL,
    signer TEXT NOT NULL,
    seq    INTEGER NOT NULL,
    theirs TEXT NOT NULL,
    PRIMARY KEY (scid, signer)
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
    load_chain_before(conn, scid, u64::MAX)
}

/// The stored events of `scid` below seq `end`, replayed: `None` when there are none.
pub fn load_chain_before(
    conn: &Connection,
    scid: &str,
    end: u64,
) -> Result<Option<KelChain>, ApiError> {
    let mut stmt = conn.prepare_cached(
        "SELECT json FROM chain_entries WHERE type = ?1 AND chain = ?2 AND seq < ?3 ORDER BY seq",
    )?;
    let end = i64::try_from(end).unwrap_or(i64::MAX);
    let texts = stmt
        .query_map(params![KEL, scid, end], |row| row.get::<_, String>(0))?
        .collect::<Result<Vec<_>, _>>()?;
    if texts.is_empty() {
        return Ok(None);
    }
    let events = texts
        .iter()
        .map(|t| {
            Event::from_json_text(t)
                .map_err(|e| ApiError::internal(format!("stored event of {scid}: {e}")))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let chain = fold(&events)
        .map_err(|e| ApiError::internal(format!("stored log {scid} does not replay: {e}")))?;
    Ok(Some(chain))
}

/// The recorded fork that freezes `scid`, if any (the fork rule of `identity-store/AGENTS.md`,
/// section 14 of `docs-src/identity/did-scid-coasys-1.md`): a fork whose rival event was signed
/// by the master that is current at the head of the held log `held`. A fork signed by a master
/// the held log has rotated away from (at the fork's seq or later) stays recorded and served as
/// evidence but freezes nothing, so a retired (stolen) master cannot lock the identity out. A
/// `rotate` appended on the held log retires the rival's signer and so ends the freeze. Derived
/// from the fork rows and `held` each time (one primary-key lookup in `fork_signers`): nothing
/// about a freeze is stored.
pub fn freezing_fork(
    conn: &Connection,
    scid: &str,
    held: &KelChain,
) -> Result<Option<ForkEvidence>, ApiError> {
    let Some(current) = held.state().current_master() else {
        return Ok(None);
    };
    signer_fork(conn, scid, &current.sign)
}

/// The fork recorded for `scid` whose rival event `signer` signed, if any.
pub(crate) fn signer_fork(
    conn: &Connection,
    scid: &str,
    signer: &Multikey,
) -> Result<Option<ForkEvidence>, ApiError> {
    let mut stmt = conn.prepare_cached(
        "SELECT f.seq, c.ours, f.theirs FROM fork_signers f
         JOIN chain_forks c ON c.type = ?1 AND c.chain = f.scid AND c.seq = f.seq
                           AND c.theirs = f.theirs
         WHERE f.scid = ?2 AND f.signer = ?3",
    )?;
    Ok(stmt
        .query_row(params![KEL, scid, signer], |row| {
            Ok(ForkEvidence {
                chain: scid.to_string(),
                seq: from_sql(row.get(0)?),
                ours: row.get(1)?,
                theirs: row.get(2)?,
            })
        })
        .optional()?)
}

/// Records the fork `fork` (rival event `theirs_json`, signed by the master `signer`) and
/// answers the evidence to send back. The store keeps at most one fork per identity and signing
/// master: a second rival by a master that already has one adds nothing and is answered with the
/// recorded fork, so the fork rows of an identity are bounded by the masters its log ever had.
/// Call inside the append's transaction.
pub(crate) fn record_fork(
    conn: &Connection,
    fork: ForkEvidence,
    theirs_json: &str,
    signer: &Multikey,
) -> Result<ForkEvidence, ApiError> {
    if let Some(recorded) = signer_fork(conn, &fork.chain, signer)? {
        return Ok(recorded);
    }
    SqliteChainStore::record_fork_in(conn, KEL, &fork, theirs_json)?;
    conn.execute(
        "INSERT INTO fork_signers (scid, signer, seq, theirs) VALUES (?1, ?2, ?3, ?4)",
        params![fork.chain, signer, to_sql(fork.seq)?, fork.theirs],
    )?;
    Ok(fork)
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

/// The identity a signing key sits in, if any (the one-key check looks across every
/// log).
pub fn key_owner(conn: &Connection, sign: &str) -> Result<Option<String>, ApiError> {
    Ok(conn
        .query_row(
            "SELECT scid FROM keys WHERE sign = ?1",
            params![sign],
            |row| row.get(0),
        )
        .optional()?)
}

/// The signing keys the key index lists under `key_id` in the log of `scid` (one, unless the
/// index is damaged). A signed request's signature is checked against these before the log is
/// read, so a forged request costs one indexed lookup and one signature check.
pub fn indexed_signs(conn: &Connection, scid: &str, key_id: &str) -> Result<Vec<String>, ApiError> {
    let mut stmt = conn.prepare_cached("SELECT sign FROM keys WHERE scid = ?1 AND key_id = ?2")?;
    let rows = stmt.query_map(params![scid, key_id], |row| row.get(0))?;
    Ok(rows.collect::<Result<_, _>>()?)
}

/// Writes the key-index rows of every key of `state` as they stand at `head_seq`, and the
/// next-key commitment of `state` (`logs.next`, `NULL` once the log has ended). The `logs` row
/// must exist.
pub(crate) fn put_keys(conn: &Connection, state: &KeyState, head_seq: u64) -> Result<(), ApiError> {
    let mut stmt = conn.prepare_cached(
        "INSERT INTO keys (sign, scid, key_id, added, revoked, bot, expires, master)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)
         ON CONFLICT(sign) DO UPDATE SET revoked = excluded.revoked, bot = excluded.bot,
                                         expires = excluded.expires
         WHERE keys.scid = excluded.scid",
    )?;
    let masters = state.masters.iter().map(|m| (m, true));
    for (record, master) in masters.chain(state.keys.values().map(|k| (k, false))) {
        let bot = state.bot_scope_at(&record.id, head_seq);
        let expires = bot.map(|b| b.expires);
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
            master,
        ])?;
    }
    conn.execute(
        "UPDATE logs SET next = ?2 WHERE scid = ?1",
        params![state.scid, state.next],
    )?;
    Ok(())
}

/// A master key of a log as the key index lists it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct IndexedMaster {
    pub id: String,
    pub sign: Multikey,
    pub revoked: Option<u64>,
}

fn indexed_master(
    conn: &Connection,
    sql: &str,
    scid: &str,
    seq: u64,
) -> Result<Option<IndexedMaster>, ApiError> {
    let mut stmt = conn.prepare_cached(sql)?;
    Ok(stmt
        .query_row(params![scid, to_sql(seq)?], |row| {
            Ok(IndexedMaster {
                id: row.get(0)?,
                sign: row.get(1)?,
                revoked: row.get::<_, Option<i64>>(2)?.map(from_sql),
            })
        })
        .optional()?)
}

/// The master of `scid` that signs an event at `seq` (≥ 1), by the key index: the master added
/// before `seq` and not retired before it. At `seq` = head + 1 that is the current master;
/// `None` there means the log has ended.
pub(crate) fn master_at(
    conn: &Connection,
    scid: &str,
    seq: u64,
) -> Result<Option<IndexedMaster>, ApiError> {
    indexed_master(
        conn,
        "SELECT key_id, sign, revoked FROM keys
         WHERE scid = ?1 AND master = 1 AND added < ?2 AND (revoked IS NULL OR revoked >= ?2)",
        scid,
        seq,
    )
}

/// The master of `scid` that the `rotate` at `seq` revealed, by the key index.
pub(crate) fn master_added_at(
    conn: &Connection,
    scid: &str,
    seq: u64,
) -> Result<Option<IndexedMaster>, ApiError> {
    indexed_master(
        conn,
        "SELECT key_id, sign, revoked FROM keys WHERE scid = ?1 AND master = 1 AND added = ?2",
        scid,
        seq,
    )
}

/// How many masters the log of `scid` has had (the next one is `m<count>`).
pub(crate) fn master_count(conn: &Connection, scid: &str) -> Result<u64, ApiError> {
    let count: i64 = conn.query_row(
        "SELECT COUNT(*) FROM keys WHERE scid = ?1 AND master = 1",
        params![scid],
        |row| row.get(0),
    )?;
    Ok(from_sql(count))
}

/// The next-key commitment of `scid` at its stored head; `None` once the log has ended.
pub(crate) fn next_commitment(conn: &Connection, scid: &str) -> Result<Option<String>, ApiError> {
    Ok(conn
        .query_row(
            "SELECT next FROM logs WHERE scid = ?1",
            params![scid],
            |row| row.get::<_, Option<String>>(0),
        )
        .optional()?
        .flatten())
}

/// The seq of the stored head of `scid` (one primary-key lookup of `chain_heads`).
pub(crate) fn head_seq(conn: &Connection, scid: &str) -> Result<Option<u64>, ApiError> {
    Ok(SqliteChainStore::head_in(conn, KEL, &scid.to_string())?.map(|head| head.seq))
}

/// The hash of the stored event of `scid` at `seq`.
pub(crate) fn stored_hash(
    conn: &Connection,
    scid: &str,
    seq: u64,
) -> Result<Option<String>, ApiError> {
    Ok(conn
        .query_row(
            "SELECT hash FROM chain_entries WHERE type = ?1 AND chain = ?2 AND seq = ?3",
            params![KEL, scid, to_sql(seq)?],
            |row| row.get(0),
        )
        .optional()?)
}

/// A key's standing at the stored head of its log, by the key index.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Standing {
    /// Revoked, replaced (a master) or ended at or before the head.
    pub revoked: bool,
    /// The `expires` of the bot scope in force at the head; `None` for a key without the bot
    /// mark.
    pub bot_expires: Option<u64>,
}

/// The standing of the signing key `sign` at the head of the log that lists it.
pub(crate) fn standing(conn: &Connection, sign: &str) -> Result<Option<Standing>, ApiError> {
    let mut stmt = conn.prepare_cached("SELECT revoked, bot, expires FROM keys WHERE sign = ?1")?;
    Ok(stmt
        .query_row(params![sign], |row| {
            let bot: Option<String> = row.get(1)?;
            Ok(Standing {
                revoked: row.get::<_, Option<i64>>(0)?.is_some(),
                bot_expires: bot.and(row.get::<_, Option<i64>>(2)?).map(from_sql),
            })
        })
        .optional()?)
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
