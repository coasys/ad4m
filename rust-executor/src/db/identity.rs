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
    /// Whether the person at this computer must accept the claim before the main agent joins.
    pub accept: JoinAccept,
}

/// A claim's local accept (`identity_joins.accept`). A claim that would set the operator
/// record (a desktop's first run, `FirstLink::FirstRun`) waits for the person at this computer
/// (`identity.acceptJoin`); any claim may be cancelled here (`identity.cancelJoin`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum JoinAccept {
    /// A claim that sets no operator record: the main agent joins once the offer is spent. A
    /// row written before the column existed reads as this.
    NotNeeded,
    /// A first-run claim: the main agent does not join until the person accepts it.
    Awaiting,
    /// The person at this computer accepted the claim.
    Accepted,
    /// The person at this computer cancelled the claim: it never binds.
    Cancelled,
}

/// What [`Ad4mDb::cancel_join`] did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum JoinCancel {
    /// The claim is cancelled (now or before).
    Cancelled,
    /// No claim on that offer.
    NoClaim,
    /// The agent already speaks for the claim's identity: too late to cancel.
    AlreadyJoined,
}

impl JoinAccept {
    fn as_str(self) -> &'static str {
        match self {
            JoinAccept::NotNeeded => "not-needed",
            JoinAccept::Awaiting => "awaiting",
            JoinAccept::Accepted => "accepted",
            JoinAccept::Cancelled => "cancelled",
        }
    }

    fn parse(text: &str) -> Option<Self> {
        match text {
            "not-needed" => Some(JoinAccept::NotNeeded),
            "awaiting" => Some(JoinAccept::Awaiting),
            "accepted" => Some(JoinAccept::Accepted),
            "cancelled" => Some(JoinAccept::Cancelled),
            _ => None,
        }
    }
}

fn to_sql(n: u64) -> i64 {
    i64::try_from(n).unwrap_or(i64::MAX)
}

fn from_sql(n: i64) -> u64 {
    u64::try_from(n).unwrap_or(0)
}

/// Creates the identity tables (idempotent), and adds `identity_joins.accept` to a table made
/// before it existed (its rows read `not-needed`).
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
            claimed_at INTEGER NOT NULL,
            accept TEXT NOT NULL DEFAULT 'not-needed'
         );",
    )?;
    let has_accept: bool = conn.query_row(
        "SELECT COUNT(*) > 0 FROM pragma_table_info('identity_joins') WHERE name = 'accept'",
        [],
        |row| row.get(0),
    )?;
    if !has_accept {
        conn.execute_batch(
            "ALTER TABLE identity_joins ADD COLUMN accept TEXT NOT NULL DEFAULT 'not-needed'",
        )?;
    }
    Ok(())
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

/// The local accept of the claim on `offer_id` in `conn`, if there is one.
fn join_accept_in(conn: &Connection, offer_id: &str) -> Ad4mDbResult<Option<JoinAccept>> {
    let text: Option<String> = conn
        .query_row(
            "SELECT accept FROM identity_joins WHERE offer_id = ?1",
            params![offer_id],
            |row| row.get(0),
        )
        .optional()?;
    text.map(|t| {
        JoinAccept::parse(&t)
            .ok_or_else(|| anyhow::anyhow!("identity_joins.accept {t:?} is unknown"))
    })
    .transpose()
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

    /// The end of a claim this executor made (`offer_id`), in one transaction: records that the
    /// local agent `agent_did` speaks for `scid` (as [`Ad4mDb::set_agent_identity`]) unless the
    /// claim is cancelled, so a cancel that lands first binds nothing.
    pub fn set_agent_identity_of_join(
        &self,
        agent_did: &str,
        scid: &str,
        set_at: u64,
        offer_id: &str,
    ) -> Ad4mDbResult<()> {
        let tx = self.conn.unchecked_transaction()?;
        match join_accept_in(&tx, offer_id)? {
            Some(JoinAccept::Cancelled) => {
                return Err(anyhow::anyhow!(
                    "the claim of offer {offer_id} is cancelled"
                ))
            }
            Some(_) => {}
            None => return Err(anyhow::anyhow!("no claim of offer {offer_id}")),
        }
        set_agent_identity_in(&tx, agent_did, scid, set_at)?;
        tx.commit()?;
        Ok(())
    }

    /// The end of a desktop app's first link, in one transaction: records that the local agent
    /// `agent_did` speaks for `scid` (as [`Ad4mDb::set_agent_identity`]) and, while the operator
    /// record is empty, that `operator_did` (the identity's DID) is the operator
    /// (`OperatorSetBy::FirstRun`). Both or neither: a process that stops between them leaves
    /// no binding without its operator. `offer_id` is the claim that joins: it must read
    /// `accepted` inside the same transaction, so a cancel that lands first binds nothing.
    /// Answers the operator record as it stands afterwards.
    pub fn set_agent_identity_and_first_run_operator(
        &self,
        agent_did: &str,
        scid: &str,
        operator_did: &str,
        set_at: u64,
        offer_id: &str,
    ) -> Ad4mDbResult<OperatorRecordRow> {
        let tx = self.conn.unchecked_transaction()?;
        match join_accept_in(&tx, offer_id)? {
            Some(JoinAccept::Accepted) => {}
            Some(other) => {
                return Err(anyhow::anyhow!(
                    "the claim of offer {offer_id} is {}, not accepted",
                    other.as_str()
                ))
            }
            None => return Err(anyhow::anyhow!("no claim of offer {offer_id}")),
        }
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
            "INSERT OR REPLACE INTO identity_joins (offer_id, scid, claimed_at, accept) VALUES (?1, ?2, ?3, ?4)",
            params![join.offer_id, join.scid, to_sql(join.claimed_at), join.accept.as_str()],
        )?;
        Ok(())
    }

    /// Sets the local accept of the claim on `offer_id`; false when there is no such claim or
    /// it is cancelled (a cancel is final: an accept racing it never undoes it).
    pub fn set_join_accept(&self, offer_id: &str, accept: JoinAccept) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE identity_joins SET accept = ?2 WHERE offer_id = ?1 AND accept != 'cancelled'",
            params![offer_id, accept.as_str()],
        )? > 0)
    }

    /// Cancels the claim on `offer_id` unless `agent_did` already speaks for its identity, in
    /// one transaction with that check: a binding written first wins, and one written after
    /// is refused ([`Ad4mDb::set_agent_identity_of_join`]). Cancelling twice is `Cancelled`.
    pub fn cancel_join(&self, offer_id: &str, agent_did: &str) -> Ad4mDbResult<JoinCancel> {
        let tx = self.conn.unchecked_transaction()?;
        let scid: Option<String> = tx
            .query_row(
                "SELECT scid FROM identity_joins WHERE offer_id = ?1",
                params![offer_id],
                |row| row.get(0),
            )
            .optional()?;
        let Some(scid) = scid else {
            return Ok(JoinCancel::NoClaim);
        };
        if agent_identity_in(&tx, agent_did)?.as_deref() == Some(scid.as_str()) {
            return Ok(JoinCancel::AlreadyJoined);
        }
        tx.execute(
            "UPDATE identity_joins SET accept = ?2 WHERE offer_id = ?1",
            params![offer_id, JoinAccept::Cancelled.as_str()],
        )?;
        tx.commit()?;
        Ok(JoinCancel::Cancelled)
    }

    /// The claim this executor made on `offer_id`, if any.
    pub fn join(&self, offer_id: &str) -> Ad4mDbResult<Option<JoinRow>> {
        let row = self
            .conn
            .query_row(
                "SELECT offer_id, scid, claimed_at, accept FROM identity_joins WHERE offer_id = ?1",
                params![offer_id],
                |row| {
                    Ok((
                        row.get::<_, String>(0)?,
                        row.get::<_, String>(1)?,
                        row.get::<_, i64>(2)?,
                        row.get::<_, String>(3)?,
                    ))
                },
            )
            .optional()?;
        row.map(|(offer_id, scid, claimed_at, accept)| {
            Ok(JoinRow {
                offer_id,
                scid,
                claimed_at: from_sql(claimed_at),
                accept: JoinAccept::parse(&accept).ok_or_else(|| {
                    anyhow::anyhow!("identity_joins.accept {accept:?} is unknown")
                })?,
            })
        })
        .transpose()
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
        for offer in ["a", "b", "c"] {
            db.add_join(&accepted_join(offer)).unwrap();
        }
        db.fail_operator_record_writes_for_test(true).unwrap();
        assert!(db
            .set_agent_identity_and_first_run_operator("did:key:z6MkA", "QmA", "did:scid:A", 1, "a")
            .is_err());
        assert_eq!(db.agent_identity("did:key:z6MkA").unwrap(), None);
        db.fail_operator_record_writes_for_test(false).unwrap();

        db.set_agent_identity("did:key:z6MkB", "QmB", 1).unwrap();
        assert!(db
            .set_agent_identity_and_first_run_operator("did:key:z6MkB", "QmA", "did:scid:A", 2, "b")
            .is_err());
        assert_eq!(db.operator_record().unwrap(), None);

        let set = db
            .set_agent_identity_and_first_run_operator("did:key:z6MkA", "QmA", "did:scid:A", 3, "a")
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
            .set_agent_identity_and_first_run_operator("did:key:z6MkC", "QmC", "did:scid:C", 4, "c")
            .unwrap();
        assert_eq!(again, set);
        assert_eq!(
            db.agent_identity("did:key:z6MkC").unwrap().as_deref(),
            Some("QmC")
        );
    }

    fn accepted_join(offer_id: &str) -> JoinRow {
        JoinRow {
            offer_id: offer_id.into(),
            scid: "QmA".into(),
            claimed_at: 1,
            accept: JoinAccept::Accepted,
        }
    }

    /// The first-run write reads the claim's accept in its own transaction: a claim awaiting
    /// accept, a cancelled one and an unknown offer bind nothing and record no operator.
    #[test]
    fn the_first_run_write_binds_only_an_accepted_claim() {
        let (_dir, db) = db();
        for (offer, accept) in [
            ("waiting", JoinAccept::Awaiting),
            ("cancelled", JoinAccept::Cancelled),
            ("plain", JoinAccept::NotNeeded),
        ] {
            db.add_join(&JoinRow {
                accept,
                ..accepted_join(offer)
            })
            .unwrap();
        }
        for offer in ["waiting", "cancelled", "plain", "unknown"] {
            assert!(
                db.set_agent_identity_and_first_run_operator(
                    "did:key:z6MkA",
                    "QmA",
                    "did:scid:A",
                    1,
                    offer
                )
                .is_err(),
                "{offer}"
            );
        }
        assert_eq!(db.agent_identity("did:key:z6MkA").unwrap(), None);
        assert_eq!(db.operator_record().unwrap(), None);
    }

    /// A claim's binding is refused once the claim is cancelled, or with no claim; a claim
    /// that needs no accept binds without one. A cancel after the binding is refused
    /// `AlreadyJoined` and leaves the claim as it was.
    #[test]
    fn a_cancelled_claim_binds_nothing_and_a_bound_one_is_not_cancelled() {
        let (_dir, db) = db();
        for (offer, accept) in [
            ("plain", JoinAccept::NotNeeded),
            ("cancelled", JoinAccept::Cancelled),
        ] {
            db.add_join(&JoinRow {
                accept,
                ..accepted_join(offer)
            })
            .unwrap();
        }
        for offer in ["cancelled", "unknown"] {
            assert!(
                db.set_agent_identity_of_join("did:key:z6MkA", "QmA", 1, offer)
                    .is_err(),
                "{offer}"
            );
        }
        assert_eq!(db.agent_identity("did:key:z6MkA").unwrap(), None);
        assert_eq!(
            db.cancel_join("unknown", "did:key:z6MkA").unwrap(),
            JoinCancel::NoClaim
        );
        assert_eq!(
            db.cancel_join("cancelled", "did:key:z6MkA").unwrap(),
            JoinCancel::Cancelled
        );

        db.set_agent_identity_of_join("did:key:z6MkA", "QmA", 1, "plain")
            .unwrap();
        assert_eq!(
            db.agent_identity("did:key:z6MkA").unwrap().as_deref(),
            Some("QmA")
        );
        assert_eq!(
            db.cancel_join("plain", "did:key:z6MkA").unwrap(),
            JoinCancel::AlreadyJoined
        );
        assert_eq!(
            db.join("plain").unwrap().map(|j| j.accept),
            Some(JoinAccept::NotNeeded)
        );
        // Another agent's claim on the same identity is still cancelled.
        assert_eq!(
            db.cancel_join("plain", "did:key:z6MkB").unwrap(),
            JoinCancel::Cancelled
        );
    }

    #[test]
    fn a_join_round_trips() {
        let (_dir, db) = db();
        let join = JoinRow {
            offer_id: "offer".into(),
            scid: "QmA".into(),
            claimed_at: 5,
            accept: JoinAccept::Awaiting,
        };
        db.add_join(&join).unwrap();
        assert_eq!(db.join("offer").unwrap(), Some(join.clone()));
        assert_eq!(db.join("other").unwrap(), None);
        assert!(db.set_join_accept("offer", JoinAccept::Accepted).unwrap());
        assert!(!db.set_join_accept("other", JoinAccept::Accepted).unwrap());
        assert_eq!(
            db.join("offer").unwrap().map(|j| j.accept),
            Some(JoinAccept::Accepted)
        );
        // A cancel is final: a later accept changes nothing.
        assert!(db.set_join_accept("offer", JoinAccept::Cancelled).unwrap());
        assert!(!db.set_join_accept("offer", JoinAccept::Accepted).unwrap());
        assert_eq!(
            db.join("offer").unwrap().map(|j| j.accept),
            Some(JoinAccept::Cancelled)
        );
    }

    /// A database made before `identity_joins.accept` gets the column at open; its rows read
    /// `not-needed` (`joinStatus` still holds them while there is no operator record).
    #[test]
    fn a_joins_table_without_accept_gets_the_column() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        {
            let conn = Connection::open(&path).unwrap();
            conn.execute_batch(
                "CREATE TABLE identity_joins (
                    offer_id TEXT PRIMARY KEY, scid TEXT NOT NULL, claimed_at INTEGER NOT NULL);
                 INSERT INTO identity_joins VALUES ('old', 'QmA', 7);",
            )
            .unwrap();
        }
        let db = Ad4mDb::new(path.to_str().unwrap()).unwrap();
        assert_eq!(
            db.join("old").unwrap(),
            Some(JoinRow {
                offer_id: "old".into(),
                scid: "QmA".into(),
                claimed_at: 7,
                accept: JoinAccept::NotNeeded,
            })
        );
    }
}
