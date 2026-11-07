//! The `grants` table: one row per token the executor issued. See `agent::grants`.

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::params;

/// A row of the `grants` table.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GrantRow {
    /// The token's id (its `nonce` claim).
    pub id: String,
    /// The capability request the grant answers. A login grant uses its own id.
    pub request_id: String,
    /// The managed agent's DID. `None`: the main agent.
    pub agent_did: Option<String>,
    /// The app's `AuthInfo`, as JSON: its metadata and capabilities.
    pub auth_info: String,
    pub revoked: bool,
    /// The token's expiry (Unix seconds).
    pub expires_at: u64,
    pub created_at: u64,
    /// The grant carries the operator role (column `role` = `operator`), not a grant on one
    /// agent (`agent`).
    pub operator: bool,
    /// The device key that a key login made the grant for; `None` for every other grant.
    pub key: Option<GrantKeyRow>,
}

/// The key binding columns of a grant that a key login made.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GrantKeyRow {
    /// The identity: `did:scid:coasys:1:…`.
    pub scid: String,
    /// The device key that signed the login: its multikey.
    pub signing_key: String,
    /// Its id in the key log (`d2`).
    pub key_id: String,
    /// The trusted seq of the log the last valid verdict was read at.
    pub head_seq: u64,
    /// When the key was last found valid (Unix seconds).
    pub checked_at: u64,
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

fn role(operator: bool) -> &'static str {
    if operator {
        "operator"
    } else {
        "agent"
    }
}

/// The columns a key login added to `grants`, with their definitions: a table from before key
/// login gains them on start, its rows as grants on one agent with no key.
const KEY_LOGIN_COLUMNS: &[(&str, &str)] = &[
    (
        "role",
        "TEXT NOT NULL DEFAULT 'agent' CHECK (role IN ('agent', 'operator'))",
    ),
    ("scid", "TEXT"),
    ("signing_key", "TEXT"),
    ("key_id", "TEXT"),
    ("head_seq", "INTEGER"),
    ("checked_at", "INTEGER"),
];

const COLUMNS: &str = "id, request_id, agent_did, auth_info, state, expires_at, created_at, \
     role, scid, signing_key, key_id, head_seq, checked_at";

impl Ad4mDb {
    pub(super) fn create_grants_table(conn: &rusqlite::Connection) -> rusqlite::Result<()> {
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS grants (
                id TEXT PRIMARY KEY,
                request_id TEXT NOT NULL,
                agent_did TEXT,
                auth_info TEXT NOT NULL,
                state TEXT NOT NULL CHECK (state IN ('active', 'revoked')),
                expires_at INTEGER NOT NULL,
                created_at INTEGER NOT NULL
             );
             DROP TABLE IF EXISTS revoked_tokens;",
        )?;
        conn.execute_batch(
            "CREATE INDEX IF NOT EXISTS idx_grants_agent_did ON grants(agent_did);",
        )?;
        Self::add_key_login_columns(conn)
    }

    /// Adds the key login columns that `grants` lacks, in one transaction, and their indexes.
    fn add_key_login_columns(conn: &rusqlite::Connection) -> rusqlite::Result<()> {
        let existing: Vec<String> = conn
            .prepare("SELECT name FROM pragma_table_info('grants')")?
            .query_map([], |row| row.get(0))?
            .collect::<Result<_, _>>()?;
        let tx = conn.unchecked_transaction()?;
        for (name, definition) in KEY_LOGIN_COLUMNS {
            if !existing.iter().any(|c| c == name) {
                tx.execute(
                    &format!("ALTER TABLE grants ADD COLUMN {name} {definition}"),
                    [],
                )?;
            }
        }
        tx.execute_batch(
            "CREATE INDEX IF NOT EXISTS idx_grants_signing_key ON grants(signing_key)
                WHERE signing_key IS NOT NULL;
             CREATE INDEX IF NOT EXISTS idx_grants_scid ON grants(scid) WHERE scid IS NOT NULL;",
        )?;
        tx.commit()
    }

    /// Adds a grant. A row with the same id stays as it is.
    pub fn add_grant(&self, grant: &GrantRow) -> Ad4mDbResult<()> {
        let key = grant.key.as_ref();
        self.conn.execute(
            &format!(
                "INSERT OR IGNORE INTO grants ({COLUMNS})
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)"
            ),
            params![
                grant.id,
                grant.request_id,
                grant.agent_did,
                grant.auth_info,
                state(grant.revoked),
                to_sql(grant.expires_at),
                to_sql(grant.created_at),
                role(grant.operator),
                key.map(|k| &k.scid),
                key.map(|k| &k.signing_key),
                key.map(|k| &k.key_id),
                key.map(|k| to_sql(k.head_seq)),
                key.map(|k| to_sql(k.checked_at)),
            ],
        )?;
        Ok(())
    }

    /// Every grant.
    pub fn grants(&self) -> Ad4mDbResult<Vec<GrantRow>> {
        let mut stmt = self
            .conn
            .prepare(&format!("SELECT {COLUMNS} FROM grants"))?;
        let rows = stmt.query_map([], |row| {
            let state: String = row.get(4)?;
            let role: String = row.get(7)?;
            let scid: Option<String> = row.get(8)?;
            let signing_key: Option<String> = row.get(9)?;
            let key = match (scid, signing_key) {
                (Some(scid), Some(signing_key)) => Some(GrantKeyRow {
                    scid,
                    signing_key,
                    key_id: row.get::<_, Option<String>>(10)?.unwrap_or_default(),
                    head_seq: from_sql(row.get::<_, Option<i64>>(11)?.unwrap_or(0)),
                    checked_at: from_sql(row.get::<_, Option<i64>>(12)?.unwrap_or(0)),
                }),
                _ => None,
            };
            Ok(GrantRow {
                id: row.get(0)?,
                request_id: row.get(1)?,
                agent_did: row.get(2)?,
                auth_info: row.get(3)?,
                revoked: state != "active",
                expires_at: from_sql(row.get(5)?),
                created_at: from_sql(row.get(6)?),
                operator: role == "operator",
                key,
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

    /// A renewal of the key-bound grant `id`: its new expiry and the verdict it was renewed
    /// on. A revoked grant stays as it is.
    pub fn renew_grant(
        &self,
        id: &str,
        expires_at: u64,
        head_seq: u64,
        checked_at: u64,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE grants SET expires_at = ?2, head_seq = ?3, checked_at = ?4
             WHERE id = ?1 AND state = 'active'",
            params![id, to_sql(expires_at), to_sql(head_seq), to_sql(checked_at)],
        )?;
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
            agent_did: None,
            auth_info: "{}".to_string(),
            revoked: false,
            expires_at,
            created_at: 1,
            ..GrantRow::default()
        }
    }

    fn key_row(id: &str) -> GrantRow {
        GrantRow {
            operator: true,
            key: Some(GrantKeyRow {
                scid: "did:scid:coasys:1:QmA".into(),
                signing_key: "z6MkA".into(),
                key_id: "d2".into(),
                head_seq: 3,
                checked_at: 40,
            }),
            ..row(id, 2_000)
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

    // A key login's grant keeps its role and its key binding; a renewal moves its expiry and
    // its last verdict, and never touches a revoked grant.
    #[test]
    fn a_key_grant_keeps_its_role_and_binding_and_a_renewal_moves_its_expiry() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.add_grant(&key_row("k")).unwrap();
        assert_eq!(db.grants().unwrap(), vec![key_row("k")]);

        db.renew_grant("k", 5_000, 4, 90).unwrap();
        let renewed = db.grants().unwrap().remove(0);
        assert_eq!(renewed.expires_at, 5_000);
        let key = renewed.key.unwrap();
        assert_eq!((key.head_seq, key.checked_at), (4, 90));

        db.revoke_grants(&["k".to_string()]).unwrap();
        db.renew_grant("k", 9_000, 5, 100).unwrap();
        assert_eq!(db.grants().unwrap()[0].expires_at, 5_000);
    }

    // A `grants` table from before key login gains the columns once, and its rows read back as
    // grants on one agent with no key.
    #[test]
    fn a_table_from_before_key_login_gains_the_columns_and_keeps_its_rows() {
        let conn = rusqlite::Connection::open_in_memory().unwrap();
        conn.execute_batch(
            "CREATE TABLE grants (
                id TEXT PRIMARY KEY,
                request_id TEXT NOT NULL,
                agent_did TEXT,
                auth_info TEXT NOT NULL,
                state TEXT NOT NULL CHECK (state IN ('active', 'revoked')),
                expires_at INTEGER NOT NULL,
                created_at INTEGER NOT NULL
             );
             INSERT INTO grants VALUES ('old', 'old', NULL, '{}', 'active', 2000, 1);",
        )
        .unwrap();
        Ad4mDb::create_grants_table(&conn).unwrap();
        Ad4mDb::create_grants_table(&conn).unwrap();
        let db = Ad4mDb { conn };
        assert_eq!(
            db.grants().unwrap(),
            vec![GrantRow {
                request_id: "old".into(),
                ..row("old", 2_000)
            }]
        );
    }
}
