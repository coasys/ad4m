//! The executor's bots: one row per bot agent (`bots`), with the grant its owner set, and the
//! bot activity log (`bot_activity`), read by `crate::bots`.
//!
//! A bot is a managed agent: its account is a row of `users` like a person's. The `bots` row
//! marks it as a bot and holds whose it is (the owner's SCID, from the offer it claimed) and
//! what it may do now (spaces, actions, expiry, state). The grant's `version` goes up by one
//! with every change; a change names the version it read, so two owners' edits never cross.

use rusqlite::{params, Connection, OptionalExtension, Row};

use super::{Ad4mDb, Ad4mDbResult};

/// What a bot may do now: its owner's grant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BotGrantRow {
    /// The neighbourhood URLs and perspective UUIDs it may act in.
    pub spaces: Vec<String>,
    /// Of `read`, `write`, `join`.
    pub can: Vec<String>,
    /// Unix seconds; `None` for the expiry of the bot's key in the owner's log.
    pub expires: Option<u64>,
    /// `active`, `paused` or `revoked` (for good).
    pub state: String,
    pub version: u64,
    pub updated_at: u64,
}

/// One bot agent.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BotRow {
    /// The bot's agent, a `did:key`.
    pub did: String,
    /// Its account's login name in `users`.
    pub username: String,
    /// The name its owner gave it.
    pub name: String,
    /// The SCID of the identity whose log lists its key (bare).
    pub owner: String,
    /// The offer its key claimed.
    pub offer_id: String,
    pub created_at: u64,
    pub grant: BotGrantRow,
}

/// One entry of the bot activity log.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BotActivityRow {
    /// Set by the database; ignored on insert.
    pub id: i64,
    pub bot_did: String,
    pub at: u64,
    pub method: String,
    /// `read`, `write` or `join`; `None` for a call outside every action.
    pub action: Option<String>,
    /// The space the call named, if any.
    pub space: Option<String>,
    /// The reason the bot gave.
    pub reason: Option<String>,
    /// `allowed`, or the refusal's code (`bot-grant`, `bot-paused`, ...).
    pub outcome: String,
}

fn to_sql(n: u64) -> i64 {
    i64::try_from(n).unwrap_or(i64::MAX)
}

fn from_sql(n: i64) -> u64 {
    u64::try_from(n).unwrap_or(0)
}

fn json_list(text: String) -> rusqlite::Result<Vec<String>> {
    serde_json::from_str(&text).map_err(|e| {
        rusqlite::Error::FromSqlConversionFailure(0, rusqlite::types::Type::Text, Box::new(e))
    })
}

fn list_json(list: &[String]) -> String {
    serde_json::to_string(list).unwrap_or_else(|_| "[]".to_string())
}

const BOT_COLUMNS: &str = "did, username, name, owner, offer_id, created_at, spaces, can, \
                           expires, state, version, updated_at";

fn bot_of(row: &Row) -> rusqlite::Result<BotRow> {
    Ok(BotRow {
        did: row.get(0)?,
        username: row.get(1)?,
        name: row.get(2)?,
        owner: row.get(3)?,
        offer_id: row.get(4)?,
        created_at: from_sql(row.get(5)?),
        grant: BotGrantRow {
            spaces: json_list(row.get(6)?)?,
            can: json_list(row.get(7)?)?,
            expires: row.get::<_, Option<i64>>(8)?.map(from_sql),
            state: row.get(9)?,
            version: from_sql(row.get(10)?),
            updated_at: from_sql(row.get(11)?),
        },
    })
}

fn activity_of(row: &Row) -> rusqlite::Result<BotActivityRow> {
    Ok(BotActivityRow {
        id: row.get(0)?,
        bot_did: row.get(1)?,
        at: from_sql(row.get(2)?),
        method: row.get(3)?,
        action: row.get(4)?,
        space: row.get(5)?,
        reason: row.get(6)?,
        outcome: row.get(7)?,
    })
}

/// Creates the bot tables (idempotent).
pub(crate) fn create_bot_tables(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS bots (
            did TEXT PRIMARY KEY,
            username TEXT NOT NULL UNIQUE,
            name TEXT NOT NULL,
            owner TEXT NOT NULL,
            offer_id TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            spaces TEXT NOT NULL,
            can TEXT NOT NULL,
            expires INTEGER,
            state TEXT NOT NULL CHECK (state IN ('active','paused','revoked')),
            version INTEGER NOT NULL,
            updated_at INTEGER NOT NULL
         );
         CREATE TABLE IF NOT EXISTS bot_activity (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            bot_did TEXT NOT NULL,
            at INTEGER NOT NULL,
            method TEXT NOT NULL,
            action TEXT,
            space TEXT,
            reason TEXT,
            outcome TEXT NOT NULL
         );
         CREATE INDEX IF NOT EXISTS bot_activity_by_bot ON bot_activity (bot_did, id);
         CREATE INDEX IF NOT EXISTS bot_activity_by_age ON bot_activity (at);",
    )
}

impl Ad4mDb {
    pub(super) fn create_bot_tables(conn: &Connection) -> rusqlite::Result<()> {
        create_bot_tables(conn)
    }

    /// Records a new bot. Fails when its DID or login name is taken.
    pub fn add_bot(&self, bot: &BotRow) -> Ad4mDbResult<()> {
        self.conn.execute(
            &format!(
                "INSERT INTO bots ({BOT_COLUMNS}) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12)"
            ),
            params![
                bot.did,
                bot.username,
                bot.name,
                bot.owner,
                bot.offer_id,
                to_sql(bot.created_at),
                list_json(&bot.grant.spaces),
                list_json(&bot.grant.can),
                bot.grant.expires.map(to_sql),
                bot.grant.state,
                to_sql(bot.grant.version),
                to_sql(bot.grant.updated_at),
            ],
        )?;
        Ok(())
    }

    /// The bot whose agent is `did`, if it is one.
    pub fn bot(&self, did: &str) -> Ad4mDbResult<Option<BotRow>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {BOT_COLUMNS} FROM bots WHERE did = ?1"),
                params![did],
                bot_of,
            )
            .optional()?)
    }

    /// Every bot, oldest first.
    pub fn bots(&self) -> Ad4mDbResult<Vec<BotRow>> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {BOT_COLUMNS} FROM bots ORDER BY created_at, did"
        ))?;
        let rows = stmt.query_map([], bot_of)?;
        Ok(rows.collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Replaces the grant of the bot `did` when its version is still `read_version` and it is
    /// not revoked. `false` when another change came first, or the grant is revoked.
    pub fn set_bot_grant(
        &self,
        did: &str,
        grant: &BotGrantRow,
        read_version: u64,
    ) -> Ad4mDbResult<bool> {
        let changed = self.conn.execute(
            "UPDATE bots SET spaces = ?1, can = ?2, expires = ?3, state = ?4, version = ?5,
                updated_at = ?6
             WHERE did = ?7 AND version = ?8 AND state != 'revoked'",
            params![
                list_json(&grant.spaces),
                list_json(&grant.can),
                grant.expires.map(to_sql),
                grant.state,
                to_sql(grant.version),
                to_sql(grant.updated_at),
                did,
                to_sql(read_version),
            ],
        )?;
        Ok(changed == 1)
    }

    /// Adds `entry` to the activity log and keeps at most `keep` entries of its bot, the
    /// newest. Returns the entry's id.
    pub fn add_bot_activity(&self, entry: &BotActivityRow, keep: usize) -> Ad4mDbResult<i64> {
        self.conn.execute(
            "INSERT INTO bot_activity (bot_did, at, method, action, space, reason, outcome)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            params![
                entry.bot_did,
                to_sql(entry.at),
                entry.method,
                entry.action,
                entry.space,
                entry.reason,
                entry.outcome,
            ],
        )?;
        let id = self.conn.last_insert_rowid();
        self.conn.execute(
            "DELETE FROM bot_activity WHERE bot_did = ?1 AND id <= (
                SELECT id FROM bot_activity WHERE bot_did = ?1 ORDER BY id DESC LIMIT 1 OFFSET ?2
             )",
            params![entry.bot_did, i64::try_from(keep).unwrap_or(i64::MAX)],
        )?;
        Ok(id)
    }

    /// Updates the outcome of the activity entry `id` (the attempt entry of a call settles
    /// with its answer). `false` when there is no such entry.
    pub fn set_bot_activity_outcome(&self, id: i64, outcome: &str) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE bot_activity SET outcome = ?2 WHERE id = ?1",
            params![id, outcome],
        )? == 1)
    }

    /// The activity of the bot `did`, newest first: at most `limit` entries, older than the
    /// entry `before` when given.
    pub fn bot_activity(
        &self,
        did: &str,
        before: Option<i64>,
        limit: usize,
    ) -> Ad4mDbResult<Vec<BotActivityRow>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, bot_did, at, method, action, space, reason, outcome FROM bot_activity
             WHERE bot_did = ?1 AND id < ?2 ORDER BY id DESC LIMIT ?3",
        )?;
        let rows = stmt.query_map(
            params![
                did,
                before.unwrap_or(i64::MAX),
                i64::try_from(limit).unwrap_or(i64::MAX)
            ],
            activity_of,
        )?;
        Ok(rows.collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Removes the activity entries older than `cutoff` (unix seconds). Returns how many.
    pub fn sweep_bot_activity(&self, cutoff: u64) -> Ad4mDbResult<usize> {
        Ok(self.conn.execute(
            "DELETE FROM bot_activity WHERE at < ?1",
            params![to_sql(cutoff)],
        )?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn db() -> (tempfile::TempDir, Ad4mDb) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let db = Ad4mDb::new(path.to_str().unwrap()).unwrap();
        (dir, db)
    }

    fn bot(did: &str) -> BotRow {
        BotRow {
            did: did.to_string(),
            username: format!("{did}@bot"),
            name: "Helper".to_string(),
            owner: "scid1".to_string(),
            offer_id: "offer1".to_string(),
            created_at: 10,
            grant: BotGrantRow {
                spaces: vec![],
                can: vec![],
                expires: None,
                state: "active".to_string(),
                version: 1,
                updated_at: 10,
            },
        }
    }

    fn entry(did: &str, at: u64, method: &str) -> BotActivityRow {
        BotActivityRow {
            id: 0,
            bot_did: did.to_string(),
            at,
            method: method.to_string(),
            action: Some("write".to_string()),
            space: Some("p1".to_string()),
            reason: Some("tidy".to_string()),
            outcome: "allowed".to_string(),
        }
    }

    #[test]
    fn a_bot_reads_back_as_written_and_a_second_with_its_did_is_refused() {
        let (_dir, db) = db();
        db.add_bot(&bot("did:key:b1")).unwrap();
        assert_eq!(db.bot("did:key:b1").unwrap(), Some(bot("did:key:b1")));
        assert_eq!(db.bot("did:key:other").unwrap(), None);
        assert!(db.add_bot(&bot("did:key:b1")).is_err());
        assert_eq!(db.bots().unwrap().len(), 1);
    }

    #[test]
    fn a_grant_change_names_the_version_it_read_and_a_revoked_grant_stays_revoked() {
        let (_dir, db) = db();
        db.add_bot(&bot("did:key:b1")).unwrap();
        let mut grant = bot("did:key:b1").grant;
        grant.spaces = vec!["p1".into()];
        grant.can = vec!["read".into()];
        grant.expires = Some(99);
        grant.version = 2;
        assert!(db.set_bot_grant("did:key:b1", &grant, 1).unwrap());
        assert_eq!(db.bot("did:key:b1").unwrap().unwrap().grant, grant);
        // A change that read version 1 comes too late.
        assert!(!db.set_bot_grant("did:key:b1", &grant, 1).unwrap());

        let mut revoked = grant.clone();
        revoked.state = "revoked".into();
        revoked.version = 3;
        assert!(db.set_bot_grant("did:key:b1", &revoked, 2).unwrap());
        let mut again = revoked.clone();
        again.state = "active".into();
        again.version = 4;
        assert!(!db.set_bot_grant("did:key:b1", &again, 3).unwrap());
        assert_eq!(
            db.bot("did:key:b1").unwrap().unwrap().grant.state,
            "revoked"
        );
    }

    #[test]
    fn the_activity_log_pages_newest_first_keeps_a_cap_per_bot_and_sweeps_by_age() {
        let (_dir, db) = db();
        for at in 1..=5 {
            db.add_bot_activity(&entry("did:key:b1", at, &format!("m{at}")), 3)
                .unwrap();
        }
        db.add_bot_activity(&entry("did:key:b2", 1, "other"), 3)
            .unwrap();
        let page = db.bot_activity("did:key:b1", None, 10).unwrap();
        assert_eq!(
            page.iter().map(|e| e.method.as_str()).collect::<Vec<_>>(),
            vec!["m5", "m4", "m3"],
            "the cap keeps the newest three"
        );
        let older = db.bot_activity("did:key:b1", Some(page[0].id), 1).unwrap();
        assert_eq!(older[0].method, "m4");
        assert_eq!(db.bot_activity("did:key:b2", None, 10).unwrap().len(), 1);

        assert_eq!(db.sweep_bot_activity(4).unwrap(), 2, "m3 and b2's entry");
        let left = db.bot_activity("did:key:b1", None, 10).unwrap();
        assert_eq!(left.len(), 2);
        assert_eq!(left[0].reason.as_deref(), Some("tidy"));
    }
}
