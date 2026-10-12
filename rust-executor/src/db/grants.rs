//! The `grants` table: one row per token the executor issued. See `agent::grants`.

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::params;

/// A row of the `grants` table.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GrantRow {
    /// The token's id (its `nonce` claim).
    pub id: String,
    /// The capability request the grant answers. A login grant uses its own id.
    pub request_id: String,
    /// The managed agent's user email. `None`: the main agent.
    pub agent_email: Option<String>,
    pub agent_did: String,
    /// The app's `AuthInfo`, as JSON: its metadata and capabilities.
    pub auth_info: String,
    pub revoked: bool,
    /// The token's expiry (Unix seconds).
    pub expires_at: u64,
    pub created_at: u64,
}

fn to_sql(secs: u64) -> i64 {
    i64::try_from(secs).unwrap_or(i64::MAX)
}

fn from_sql(secs: i64) -> u64 {
    u64::try_from(secs).unwrap_or(0)
}

fn state(revoked: bool) -> &'static str {
    if revoked {
        "revoked"
    } else {
        "active"
    }
}

impl Ad4mDb {
    pub(super) fn create_grants_table(conn: &rusqlite::Connection) -> rusqlite::Result<()> {
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS grants (
                id TEXT PRIMARY KEY,
                request_id TEXT NOT NULL,
                agent_email TEXT,
                agent_did TEXT NOT NULL,
                auth_info TEXT NOT NULL,
                state TEXT NOT NULL CHECK (state IN ('active', 'revoked')),
                expires_at INTEGER NOT NULL,
                created_at INTEGER NOT NULL
             );
             CREATE INDEX IF NOT EXISTS idx_grants_agent_email ON grants(agent_email);
             DROP TABLE IF EXISTS revoked_tokens;",
        )
    }

    /// Adds a grant. A row with the same id stays as it is.
    pub fn add_grant(&self, grant: &GrantRow) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO grants
                (id, request_id, agent_email, agent_did, auth_info, state, expires_at, created_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![
                grant.id,
                grant.request_id,
                grant.agent_email,
                grant.agent_did,
                grant.auth_info,
                state(grant.revoked),
                to_sql(grant.expires_at),
                to_sql(grant.created_at),
            ],
        )?;
        Ok(())
    }

    /// Every grant.
    pub fn grants(&self) -> Ad4mDbResult<Vec<GrantRow>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, request_id, agent_email, agent_did, auth_info, state, expires_at, created_at
             FROM grants",
        )?;
        let rows = stmt.query_map([], |row| {
            let state: String = row.get(5)?;
            Ok(GrantRow {
                id: row.get(0)?,
                request_id: row.get(1)?,
                agent_email: row.get(2)?,
                agent_did: row.get(3)?,
                auth_info: row.get(4)?,
                revoked: state != "active",
                expires_at: from_sql(row.get(6)?),
                created_at: from_sql(row.get(7)?),
            })
        })?;
        Ok(rows.collect::<Result<_, _>>()?)
    }

    /// Marks the grants `ids` revoked.
    pub fn revoke_grants(&self, ids: &[String]) -> Ad4mDbResult<()> {
        for id in ids {
            self.conn.execute(
                "UPDATE grants SET state = 'revoked' WHERE id = ?1",
                params![id],
            )?;
        }
        Ok(())
    }

    /// Deletes the grants `ids`.
    pub fn delete_grants(&self, ids: &[String]) -> Ad4mDbResult<()> {
        for id in ids {
            self.conn
                .execute("DELETE FROM grants WHERE id = ?1", params![id])?;
        }
        Ok(())
    }

    /// Deletes the grants whose tokens expired before `cutoff` (Unix seconds).
    pub fn delete_grants_expired_before(&self, cutoff: u64) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM grants WHERE expires_at < ?1",
            params![to_sql(cutoff)],
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(id: &str, expires_at: u64) -> GrantRow {
        GrantRow {
            id: id.to_string(),
            request_id: format!("request-{id}"),
            agent_email: None,
            agent_did: "did:key:main".to_string(),
            auth_info: "{}".to_string(),
            revoked: false,
            expires_at,
            created_at: 1,
        }
    }

    #[test]
    fn a_grant_is_stored_revoked_and_deleted() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.add_grant(&row("a", 2_000)).unwrap();
        db.add_grant(&row("b", 2_000)).unwrap();
        assert_eq!(db.grants().unwrap().len(), 2);

        db.revoke_grants(&["a".to_string()]).unwrap();
        let rows = db.grants().unwrap();
        let a = rows.iter().find(|r| r.id == "a").unwrap();
        assert!(a.revoked);
        assert!(!rows.iter().find(|r| r.id == "b").unwrap().revoked);

        db.delete_grants(&["a".to_string()]).unwrap();
        assert_eq!(db.grants().unwrap(), vec![row("b", 2_000)]);
    }

    #[test]
    fn adding_an_existing_grant_keeps_the_stored_row() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.add_grant(&row("a", 2_000)).unwrap();
        db.revoke_grants(&["a".to_string()]).unwrap();
        db.add_grant(&row("a", 2_000)).unwrap();
        assert!(
            db.grants().unwrap()[0].revoked,
            "a re-run migration revokes nothing back"
        );
    }

    #[test]
    fn expired_grants_are_deleted() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.add_grant(&row("old", 999)).unwrap();
        db.add_grant(&row("live", 1_000)).unwrap();
        db.delete_grants_expired_before(1_000).unwrap();
        let ids: Vec<String> = db.grants().unwrap().into_iter().map(|r| r.id).collect();
        assert_eq!(ids, ["live"]);
    }
}
