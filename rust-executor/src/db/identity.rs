//! The executor's identity rows: the operator record,
//! which identity each local agent speaks for, and the claims this executor made and waits on.
//! The key logs themselves live in the hash-chain tables of the same file (`identity::cache`).
//!
//! The operator record functions also take a bare connection, because `ad4m-executor init
//! --operator` writes the record before any executor runs.

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::{params, Connection, OptionalExtension};

/// How the operator record was set (`operator_record.set_by`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorSetBy {
    /// A desktop app's executor at its first run: its first claim the store took, on a
    /// `--custody-channel` executor with an empty record (`ClaimService::claim_for_main`).
    FirstRun,
    /// `ad4m-executor init --operator`.
    Cli,
    /// The recorded operator handed the executor to another identity.
    Operator,
}

impl OperatorSetBy {
    fn as_str(self) -> &'static str {
        match self {
            OperatorSetBy::FirstRun => "first-run",
            OperatorSetBy::Cli => "cli",
            OperatorSetBy::Operator => "operator",
        }
    }

    fn parse(text: &str) -> Option<Self> {
        match text {
            "first-run" => Some(OperatorSetBy::FirstRun),
            "cli" => Some(OperatorSetBy::Cli),
            "operator" => Some(OperatorSetBy::Operator),
            _ => None,
        }
    }
}

/// The one row of `operator_record`: the operator's `did:scid`, when and how it was set.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OperatorRecordRow {
    pub scid: String,
    pub set_at: u64,
    pub set_by: OperatorSetBy,
}

/// A claim this executor made for its main agent, until the offer ends (`identity.joinStatus`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinRow {
    pub offer_id: String,
    /// The identity's SCID.
    pub scid: String,
    pub claimed_at: u64,
}

fn to_sql(n: u64) -> i64 {
    i64::try_from(n).unwrap_or(i64::MAX)
}

fn from_sql(n: i64) -> u64 {
    u64::try_from(n).unwrap_or(0)
}

/// Creates the identity tables (idempotent).
pub(crate) fn create_identity_tables(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS operator_record (
            id INTEGER PRIMARY KEY CHECK (id = 1),
            scid TEXT NOT NULL,
            set_at INTEGER NOT NULL,
            set_by TEXT NOT NULL CHECK (set_by IN ('first-run','cli','operator'))
         );
         CREATE TABLE IF NOT EXISTS identity_agents (
            agent_did TEXT PRIMARY KEY,
            scid TEXT NOT NULL,
            set_at INTEGER NOT NULL
         );
         CREATE TABLE IF NOT EXISTS identity_joins (
            offer_id TEXT PRIMARY KEY,
            scid TEXT NOT NULL,
            claimed_at INTEGER NOT NULL
         );",
    )
}

/// The operator record in `conn`, if set.
pub(crate) fn operator_record_in(conn: &Connection) -> Ad4mDbResult<Option<OperatorRecordRow>> {
    create_identity_tables(conn)?;
    let row = conn
        .query_row(
            "SELECT scid, set_at, set_by FROM operator_record WHERE id = 1",
            [],
            |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, String>(2)?,
                ))
            },
        )
        .optional()?;
    row.map(|(scid, set_at, set_by)| {
        Ok(OperatorRecordRow {
            scid,
            set_at: from_sql(set_at),
            set_by: OperatorSetBy::parse(&set_by)
                .ok_or_else(|| anyhow::anyhow!("operator_record.set_by {set_by:?} is unknown"))?,
        })
    })
    .transpose()
}

/// Sets the operator record in `conn` only while it is empty. Answers the record as it stands
/// afterwards: the caller compares it with what it asked for.
pub(crate) fn set_operator_record_if_empty_in(
    conn: &Connection,
    scid: &str,
    set_at: u64,
    set_by: OperatorSetBy,
) -> Ad4mDbResult<OperatorRecordRow> {
    create_identity_tables(conn)?;
    conn.execute(
        "INSERT OR IGNORE INTO operator_record (id, scid, set_at, set_by) VALUES (1, ?1, ?2, ?3)",
        params![scid, to_sql(set_at), set_by.as_str()],
    )?;
    operator_record_in(conn)?
        .ok_or_else(|| anyhow::anyhow!("operator_record is empty right after the insert"))
}

/// The SCID the local agent `agent_did` speaks for in `conn`, if any.
fn agent_identity_in(conn: &Connection, agent_did: &str) -> Ad4mDbResult<Option<String>> {
    Ok(conn
        .query_row(
            "SELECT scid FROM identity_agents WHERE agent_did = ?1",
            params![agent_did],
            |row| row.get(0),
        )
        .optional()?)
}

/// Records in `conn` that the local agent `agent_did` speaks for `scid`; refuses a second
/// identity.
fn set_agent_identity_in(
    conn: &Connection,
    agent_did: &str,
    scid: &str,
    set_at: u64,
) -> Ad4mDbResult<()> {
    conn.execute(
        "INSERT OR IGNORE INTO identity_agents (agent_did, scid, set_at) VALUES (?1, ?2, ?3)",
        params![agent_did, scid, to_sql(set_at)],
    )?;
    match agent_identity_in(conn, agent_did)? {
        Some(held) if held == scid => Ok(()),
        Some(held) => Err(anyhow::anyhow!(
            "the agent already speaks for {held}, not {scid}"
        )),
        None => Err(anyhow::anyhow!(
            "identity_agents is empty right after the insert"
        )),
    }
}

impl Ad4mDb {
    pub(super) fn create_identity_tables(conn: &Connection) -> rusqlite::Result<()> {
        create_identity_tables(conn)
    }

    /// The operator record, if set.
    pub fn operator_record(&self) -> Ad4mDbResult<Option<OperatorRecordRow>> {
        operator_record_in(&self.conn)
    }

    /// Sets the operator record only while it is empty; answers the record as it stands
    /// afterwards, so the caller sees whether its value or an earlier one holds.
    pub fn set_operator_record_if_empty(
        &self,
        scid: &str,
        set_at: u64,
        set_by: OperatorSetBy,
    ) -> Ad4mDbResult<OperatorRecordRow> {
        set_operator_record_if_empty_in(&self.conn, scid, set_at, set_by)
    }

    /// Tests on the shared database: sets the operator record (`Some`) or clears it (`None`).
    #[cfg(test)]
    pub(crate) fn set_operator_record_for_test(&self, scid: Option<&str>) -> Ad4mDbResult<()> {
        self.conn.execute("DELETE FROM operator_record", [])?;
        if let Some(scid) = scid {
            set_operator_record_if_empty_in(&self.conn, scid, 1, OperatorSetBy::Cli)?;
        }
        Ok(())
    }

    /// The SCID the local agent `agent_did` (its `did:key`) speaks for, if any.
    pub fn agent_identity(&self, agent_did: &str) -> Ad4mDbResult<Option<String>> {
        agent_identity_in(&self.conn, agent_did)
    }

    /// Records that the local agent `agent_did` speaks for `scid`. An agent keeps the first
    /// identity it joined: a second one is refused.
    pub fn set_agent_identity(&self, agent_did: &str, scid: &str, set_at: u64) -> Ad4mDbResult<()> {
        set_agent_identity_in(&self.conn, agent_did, scid, set_at)
    }

    /// The end of a desktop app's first link, in one transaction: records that the local agent
    /// `agent_did` speaks for `scid` (as [`Ad4mDb::set_agent_identity`]) and, while the operator
    /// record is empty, that `operator_did` (the identity's DID) is the operator
    /// (`OperatorSetBy::FirstRun`). Both or neither: a process that stops between them leaves
    /// no binding without its operator. Answers the operator record as it stands afterwards.
    pub fn set_agent_identity_and_first_run_operator(
        &self,
        agent_did: &str,
        scid: &str,
        operator_did: &str,
        set_at: u64,
    ) -> Ad4mDbResult<OperatorRecordRow> {
        let tx = self.conn.unchecked_transaction()?;
        set_agent_identity_in(&tx, agent_did, scid, set_at)?;
        let record =
            set_operator_record_if_empty_in(&tx, operator_did, set_at, OperatorSetBy::FirstRun)?;
        tx.commit()?;
        Ok(record)
    }

    /// Tests on the shared database: makes every write of the operator record fail (`true`), as
    /// a process that stops before it would, or lets them through again (`false`).
    #[cfg(test)]
    pub(crate) fn fail_operator_record_writes_for_test(&self, fail: bool) -> Ad4mDbResult<()> {
        create_identity_tables(&self.conn)?;
        self.conn.execute_batch(if fail {
            "CREATE TRIGGER IF NOT EXISTS operator_record_fails BEFORE INSERT ON operator_record
             BEGIN SELECT RAISE(ABORT, 'the operator record write failed'); END;"
        } else {
            "DROP TRIGGER IF EXISTS operator_record_fails;"
        })?;
        Ok(())
    }

    /// Tests on the shared database: drops the binding of `agent_did`.
    #[cfg(test)]
    pub(crate) fn clear_agent_identity_for_test(&self, agent_did: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM identity_agents WHERE agent_did = ?1",
            params![agent_did],
        )?;
        Ok(())
    }

    /// The local agents (`did:key`s) that speak for `scid`.
    pub fn agents_of_identity(&self, scid: &str) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self
            .conn
            .prepare("SELECT agent_did FROM identity_agents WHERE scid = ?1 ORDER BY agent_did")?;
        let rows = stmt.query_map(params![scid], |row| row.get(0))?;
        Ok(rows.collect::<rusqlite::Result<Vec<String>>>()?)
    }

    /// Every identity a local agent speaks for, once each.
    pub fn local_identities(&self) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self
            .conn
            .prepare("SELECT DISTINCT scid FROM identity_agents ORDER BY scid")?;
        let rows = stmt.query_map([], |row| row.get(0))?;
        Ok(rows.collect::<rusqlite::Result<Vec<String>>>()?)
    }

    /// Remembers a claim this executor made, so `identity.joinStatus` can follow it.
    pub fn add_join(&self, join: &JoinRow) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR REPLACE INTO identity_joins (offer_id, scid, claimed_at) VALUES (?1, ?2, ?3)",
            params![join.offer_id, join.scid, to_sql(join.claimed_at)],
        )?;
        Ok(())
    }

    /// The claim this executor made on `offer_id`, if any.
    pub fn join(&self, offer_id: &str) -> Ad4mDbResult<Option<JoinRow>> {
        Ok(self
            .conn
            .query_row(
                "SELECT offer_id, scid, claimed_at FROM identity_joins WHERE offer_id = ?1",
                params![offer_id],
                |row| {
                    Ok(JoinRow {
                        offer_id: row.get(0)?,
                        scid: row.get(1)?,
                        claimed_at: from_sql(row.get(2)?),
                    })
                },
            )
            .optional()?)
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

    #[test]
    fn the_operator_record_is_set_once() {
        let (_dir, db) = db();
        assert_eq!(db.operator_record().unwrap(), None);
        let set = set_operator_record_if_empty_in(
            &db.conn,
            "did:scid:coasys:1:QmA",
            10,
            OperatorSetBy::Cli,
        )
        .unwrap();
        assert_eq!(set.scid, "did:scid:coasys:1:QmA");
        assert_eq!(set.set_by, OperatorSetBy::Cli);
        // A second identity does not replace it: the caller sees the record that stands.
        let again = set_operator_record_if_empty_in(
            &db.conn,
            "did:scid:coasys:1:QmB",
            20,
            OperatorSetBy::Cli,
        )
        .unwrap();
        assert_eq!(again, set);
        assert_eq!(db.operator_record().unwrap(), Some(set));
    }

    #[test]
    fn the_local_identities_are_each_bound_scid_once() {
        let (_dir, db) = db();
        assert!(db.local_identities().unwrap().is_empty());
        db.set_agent_identity("did:key:z6MkA", "QmB", 1).unwrap();
        db.set_agent_identity("did:key:z6MkB", "QmB", 1).unwrap();
        db.set_agent_identity("did:key:z6MkC", "QmA", 1).unwrap();
        assert_eq!(
            db.local_identities().unwrap(),
            vec!["QmA".to_string(), "QmB".to_string()]
        );
    }

    #[test]
    fn an_agent_keeps_the_first_identity_it_joined() {
        let (_dir, db) = db();
        assert_eq!(db.agent_identity("did:key:z6MkA").unwrap(), None);
        db.set_agent_identity("did:key:z6MkA", "QmA", 1).unwrap();
        db.set_agent_identity("did:key:z6MkA", "QmA", 2).unwrap();
        assert!(db.set_agent_identity("did:key:z6MkA", "QmB", 3).is_err());
        assert_eq!(
            db.agent_identity("did:key:z6MkA").unwrap().as_deref(),
            Some("QmA")
        );
        assert_eq!(
            db.agents_of_identity("QmA").unwrap(),
            vec!["did:key:z6MkA".to_string()]
        );
    }

    /// The binding and the first-run operator land together or not at all: a write of the
    /// record that fails takes the binding back, and a refused binding records no operator.
    #[test]
    fn the_first_run_binding_and_operator_are_one_write() {
        let (_dir, db) = db();
        db.fail_operator_record_writes_for_test(true).unwrap();
        assert!(db
            .set_agent_identity_and_first_run_operator("did:key:z6MkA", "QmA", "did:scid:A", 1)
            .is_err());
        assert_eq!(db.agent_identity("did:key:z6MkA").unwrap(), None);
        db.fail_operator_record_writes_for_test(false).unwrap();

        db.set_agent_identity("did:key:z6MkB", "QmB", 1).unwrap();
        assert!(db
            .set_agent_identity_and_first_run_operator("did:key:z6MkB", "QmA", "did:scid:A", 2)
            .is_err());
        assert_eq!(db.operator_record().unwrap(), None);

        let set = db
            .set_agent_identity_and_first_run_operator("did:key:z6MkA", "QmA", "did:scid:A", 3)
            .unwrap();
        assert_eq!(
            (set.scid.as_str(), set.set_by),
            ("did:scid:A", OperatorSetBy::FirstRun)
        );
        assert_eq!(
            db.agent_identity("did:key:z6MkA").unwrap().as_deref(),
            Some("QmA")
        );
        // A record that stands is kept: the binding lands, the record does not change.
        let again = db
            .set_agent_identity_and_first_run_operator("did:key:z6MkC", "QmC", "did:scid:C", 4)
            .unwrap();
        assert_eq!(again, set);
        assert_eq!(
            db.agent_identity("did:key:z6MkC").unwrap().as_deref(),
            Some("QmC")
        );
    }

    #[test]
    fn a_join_round_trips() {
        let (_dir, db) = db();
        let join = JoinRow {
            offer_id: "offer".into(),
            scid: "QmA".into(),
            claimed_at: 5,
        };
        db.add_join(&join).unwrap();
        assert_eq!(db.join("offer").unwrap(), Some(join));
        assert_eq!(db.join("other").unwrap(), None);
    }
}
