//! Which spaces a revoked key reached (`revocation_reach`, AR-08 section 4.11.3), read by
//! `crate::revocation_reach` for `agent.revocationReach`.
//!
//! - `revocation_reach`: one row per revoked key and space its person held when the
//!   revocation reached this executor, with when the space's server dropped the key and when
//!   its room key rotated past it (both `NULL` until its link language says so: a Holochain
//!   space never does). Rows live 30 days from `recorded_at`.
//! - `revocation_reach_keys`: the keys whose revocation was recorded, never pruned. The key
//!   cache fires a revocation again for every log it loads after a restart; this table keeps
//!   that from recording a pruned key's rows a second time.

use rusqlite::{params, Connection, Row};

use super::{Ad4mDb, Ad4mDbResult};

/// One space a revoked key reached.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReachRow {
    /// The revoked key, `did:key:<multikey>`.
    pub did_key: String,
    pub perspective_uuid: String,
    /// The perspective's name when the row was recorded.
    pub name: String,
    /// Unix ms.
    pub recorded_at: u64,
    /// Unix ms: the space's server closed the key's connection.
    pub dropped_at: Option<u64>,
    /// Unix ms: the space's room key rotated past the key.
    pub rotated_at: Option<u64>,
}

/// What a space's link language reported about a removed key.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReachMark {
    Dropped,
    Rotated,
}

fn to_sql(n: u64) -> i64 {
    i64::try_from(n).unwrap_or(i64::MAX)
}

fn from_sql(n: i64) -> u64 {
    u64::try_from(n).unwrap_or(0)
}

fn reach_of(row: &Row) -> rusqlite::Result<ReachRow> {
    Ok(ReachRow {
        did_key: row.get(0)?,
        perspective_uuid: row.get(1)?,
        name: row.get(2)?,
        recorded_at: from_sql(row.get(3)?),
        dropped_at: row.get::<_, Option<i64>>(4)?.map(from_sql),
        rotated_at: row.get::<_, Option<i64>>(5)?.map(from_sql),
    })
}

pub(crate) fn create_revocation_reach_tables(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS revocation_reach (
            did_key TEXT NOT NULL,
            perspective_uuid TEXT NOT NULL,
            name TEXT NOT NULL,
            recorded_at INTEGER NOT NULL,
            dropped_at INTEGER,
            rotated_at INTEGER,
            PRIMARY KEY (did_key, perspective_uuid)
        );
        CREATE INDEX IF NOT EXISTS revocation_reach_recorded_at
            ON revocation_reach (recorded_at);
        CREATE TABLE IF NOT EXISTS revocation_reach_keys (
            did_key TEXT PRIMARY KEY,
            recorded_at INTEGER NOT NULL
        );",
    )
}

impl Ad4mDb {
    pub(super) fn create_revocation_reach_tables(conn: &Connection) -> rusqlite::Result<()> {
        create_revocation_reach_tables(conn)
    }

    /// Records that `did_key` was revoked at `at` while its person held `spaces`
    /// (`(perspective_uuid, name)`): a row per space. `false`, and nothing written, when the
    /// key's revocation was recorded before. A row the space's link language made already
    /// ([`Ad4mDb::mark_revocation_reach`]) keeps its times.
    pub fn add_revocation_reach(
        &self,
        did_key: &str,
        spaces: &[(String, String)],
        at: u64,
    ) -> Ad4mDbResult<bool> {
        let tx = self.conn.unchecked_transaction()?;
        let new = tx.execute(
            "INSERT OR IGNORE INTO revocation_reach_keys (did_key, recorded_at) VALUES (?1, ?2)",
            params![did_key, to_sql(at)],
        )? == 1;
        if new {
            for (uuid, name) in spaces {
                tx.execute(
                    "INSERT OR IGNORE INTO revocation_reach
                        (did_key, perspective_uuid, name, recorded_at)
                     VALUES (?1, ?2, ?3, ?4)",
                    params![did_key, uuid, name, to_sql(at)],
                )?;
            }
        }
        tx.commit()?;
        Ok(new)
    }

    /// Notes that the space `perspective_uuid` (named `name`) shut `did_key` out at `at`:
    /// sets the row's `dropped_at` or `rotated_at` unless it is set already, and makes the row
    /// when the link language reports before the revocation reached the key cache.
    pub fn mark_revocation_reach(
        &self,
        did_key: &str,
        perspective_uuid: &str,
        name: &str,
        mark: ReachMark,
        at: u64,
    ) -> Ad4mDbResult<()> {
        let (dropped, rotated) = match mark {
            ReachMark::Dropped => (Some(to_sql(at)), None),
            ReachMark::Rotated => (None, Some(to_sql(at))),
        };
        self.conn.execute(
            "INSERT INTO revocation_reach
                (did_key, perspective_uuid, name, recorded_at, dropped_at, rotated_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)
             ON CONFLICT (did_key, perspective_uuid) DO UPDATE SET
                dropped_at = COALESCE(dropped_at, excluded.dropped_at),
                rotated_at = COALESCE(rotated_at, excluded.rotated_at)",
            params![
                did_key,
                perspective_uuid,
                name,
                to_sql(at),
                dropped,
                rotated
            ],
        )?;
        Ok(())
    }

    /// The spaces `did_key` reached, recorded at or after `since`, by name.
    pub fn revocation_reach(&self, did_key: &str, since: u64) -> Ad4mDbResult<Vec<ReachRow>> {
        let mut stmt = self.conn.prepare(
            "SELECT did_key, perspective_uuid, name, recorded_at, dropped_at, rotated_at
             FROM revocation_reach WHERE did_key = ?1 AND recorded_at >= ?2
             ORDER BY name, perspective_uuid",
        )?;
        let rows = stmt.query_map(params![did_key, to_sql(since)], reach_of)?;
        Ok(rows.collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Deletes the rows recorded before `cutoff`; the count deleted.
    pub fn prune_revocation_reach(&self, cutoff: u64) -> Ad4mDbResult<usize> {
        Ok(self.conn.execute(
            "DELETE FROM revocation_reach WHERE recorded_at < ?1",
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

    fn space(uuid: &str, name: &str) -> (String, String) {
        (uuid.to_string(), name.to_string())
    }

    /// A key's revocation is recorded once: a second report (the cache fires it again after a
    /// restart) adds nothing, even once its rows are pruned.
    #[test]
    fn a_revocation_is_recorded_once() {
        let (_dir, db) = db();
        let spaces = [space("p1", "Garden"), space("p2", "Allotment")];
        assert!(db
            .add_revocation_reach("did:key:zA", &spaces, 1_000)
            .unwrap());
        assert!(!db
            .add_revocation_reach("did:key:zA", &[space("p3", "Other")], 2_000)
            .unwrap());
        let rows = db.revocation_reach("did:key:zA", 0).unwrap();
        let named: Vec<&str> = rows.iter().map(|r| r.name.as_str()).collect();
        assert_eq!(named, vec!["Allotment", "Garden"]);
        assert!(rows
            .iter()
            .all(|r| r.recorded_at == 1_000 && r.dropped_at.is_none() && r.rotated_at.is_none()));

        assert_eq!(db.prune_revocation_reach(1_001).unwrap(), 2);
        assert!(!db
            .add_revocation_reach("did:key:zA", &spaces, 3_000)
            .unwrap());
        assert!(db.revocation_reach("did:key:zA", 0).unwrap().is_empty());
    }

    /// A mark sets its time once, before or after the revocation is recorded.
    #[test]
    fn a_mark_sets_its_time_once_in_either_order() {
        let (_dir, db) = db();
        db.add_revocation_reach("did:key:zA", &[space("p1", "Garden")], 1_000)
            .unwrap();
        db.mark_revocation_reach("did:key:zA", "p1", "Garden", ReachMark::Dropped, 1_100)
            .unwrap();
        db.mark_revocation_reach("did:key:zA", "p1", "Garden", ReachMark::Rotated, 1_200)
            .unwrap();
        db.mark_revocation_reach("did:key:zA", "p1", "Garden", ReachMark::Dropped, 1_300)
            .unwrap();
        let rows = db.revocation_reach("did:key:zA", 0).unwrap();
        assert_eq!(
            rows,
            vec![ReachRow {
                did_key: "did:key:zA".into(),
                perspective_uuid: "p1".into(),
                name: "Garden".into(),
                recorded_at: 1_000,
                dropped_at: Some(1_100),
                rotated_at: Some(1_200),
            }]
        );

        // The link language first: the revocation keeps the row's time.
        db.mark_revocation_reach("did:key:zB", "p1", "Garden", ReachMark::Rotated, 2_000)
            .unwrap();
        assert!(db
            .add_revocation_reach("did:key:zB", &[space("p1", "Garden")], 2_100)
            .unwrap());
        let rows = db.revocation_reach("did:key:zB", 0).unwrap();
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].rotated_at, Some(2_000));
        assert_eq!(rows[0].recorded_at, 2_000);
    }

    /// A read leaves out rows recorded before `since`; a prune deletes them.
    #[test]
    fn reads_and_prunes_go_by_the_recorded_time() {
        let (_dir, db) = db();
        db.add_revocation_reach("did:key:zA", &[space("p1", "Old")], 1_000)
            .unwrap();
        db.add_revocation_reach("did:key:zB", &[space("p1", "New")], 5_000)
            .unwrap();
        assert!(db.revocation_reach("did:key:zA", 2_000).unwrap().is_empty());
        assert_eq!(db.revocation_reach("did:key:zB", 2_000).unwrap().len(), 1);
        assert_eq!(db.prune_revocation_reach(2_000).unwrap(), 1);
        assert!(db.revocation_reach("did:key:zA", 0).unwrap().is_empty());
        assert_eq!(db.revocation_reach("did:key:zB", 0).unwrap().len(), 1);
    }
}
