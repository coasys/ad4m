//! The rows of a host: its tenants, its invites and the tenants' AI-run counts. The meaning
//! of the states and the join route is `hosting::tenants`; this layer keeps the text and
//! the rules the database can hold (one live tenant per identity, an invite spent once).

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::{params, Connection, OptionalExtension};

/// One row of `tenants`. Times are unix seconds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TenantRow {
    pub id: String,
    pub did_scid: String,
    pub device_key_name: Option<String>,
    pub device_did: Option<String>,
    /// `pending`, `active`, `held`, `detached` or `deleted`.
    pub state: String,
    /// The JSON of the join route.
    pub joined_via: String,
    pub created_at: u64,
    pub activated_at: Option<u64>,
    pub detached_at: Option<u64>,
    pub pending_expires_at: Option<u64>,
}

/// What [`Ad4mDb::insert_tenant`] did.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TenantInsert {
    Inserted,
    /// A record of this identity that is not deleted exists already.
    LiveExists,
}

/// One row of `host_invites`. Times are unix seconds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InviteRow {
    pub id: String,
    /// SHA-256 of the invite's secret; the secret is never stored.
    pub secret_hash: Vec<u8>,
    pub created_by: String,
    pub created_at: u64,
    pub expires_at: u64,
    pub used_by: Option<String>,
    pub revoked: bool,
    pub note: Option<String>,
}

pub(crate) fn create_hosting_tables(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS tenants (
            id TEXT PRIMARY KEY,
            did_scid TEXT NOT NULL,
            device_key_name TEXT,
            device_did TEXT,
            state TEXT NOT NULL CHECK (state IN ('pending','active','held','detached','deleted')),
            joined_via TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            activated_at INTEGER,
            detached_at INTEGER,
            pending_expires_at INTEGER
         );
         CREATE UNIQUE INDEX IF NOT EXISTS tenants_live_did
            ON tenants(did_scid) WHERE state <> 'deleted';
         CREATE TABLE IF NOT EXISTS host_invites (
            id TEXT PRIMARY KEY,
            secret_hash BLOB NOT NULL,
            created_by TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            expires_at INTEGER NOT NULL,
            used_by TEXT,
            revoked INTEGER NOT NULL DEFAULT 0,
            note TEXT
         );
         CREATE TABLE IF NOT EXISTS tenant_ai_runs (
            tenant_id TEXT NOT NULL,
            day TEXT NOT NULL,
            runs INTEGER NOT NULL DEFAULT 0,
            PRIMARY KEY (tenant_id, day)
         );",
    )
}

const TENANT_COLUMNS: &str = "id, did_scid, device_key_name, device_did, state, joined_via, \
     created_at, activated_at, detached_at, pending_expires_at";

fn tenant_of(row: &rusqlite::Row<'_>) -> rusqlite::Result<TenantRow> {
    Ok(TenantRow {
        id: row.get(0)?,
        did_scid: row.get(1)?,
        device_key_name: row.get(2)?,
        device_did: row.get(3)?,
        state: row.get(4)?,
        joined_via: row.get(5)?,
        created_at: row.get(6)?,
        activated_at: row.get(7)?,
        detached_at: row.get(8)?,
        pending_expires_at: row.get(9)?,
    })
}

const INVITE_COLUMNS: &str = "id, secret_hash, created_by, created_at, expires_at, used_by, \
     revoked, note";

fn invite_of(row: &rusqlite::Row<'_>) -> rusqlite::Result<InviteRow> {
    Ok(InviteRow {
        id: row.get(0)?,
        secret_hash: row.get(1)?,
        created_by: row.get(2)?,
        created_at: row.get(3)?,
        expires_at: row.get(4)?,
        used_by: row.get(5)?,
        revoked: row.get::<_, i64>(6)? != 0,
        note: row.get(7)?,
    })
}

impl Ad4mDb {
    /// Inserts `tenant`; [`TenantInsert::LiveExists`] when its identity has a record that is
    /// not deleted (the unique index).
    pub fn insert_tenant(&self, tenant: &TenantRow) -> Ad4mDbResult<TenantInsert> {
        let live: Option<String> = self
            .conn
            .query_row(
                "SELECT id FROM tenants WHERE did_scid = ?1 AND state <> 'deleted'",
                [&tenant.did_scid],
                |row| row.get(0),
            )
            .optional()?;
        if live.is_some() {
            return Ok(TenantInsert::LiveExists);
        }
        self.conn.execute(
            &format!(
                "INSERT INTO tenants ({TENANT_COLUMNS}) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
            ),
            params![
                tenant.id,
                tenant.did_scid,
                tenant.device_key_name,
                tenant.device_did,
                tenant.state,
                tenant.joined_via,
                tenant.created_at,
                tenant.activated_at,
                tenant.detached_at,
                tenant.pending_expires_at,
            ],
        )?;
        Ok(TenantInsert::Inserted)
    }

    /// The tenant record `id`, in any state.
    pub fn tenant(&self, id: &str) -> Ad4mDbResult<Option<TenantRow>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {TENANT_COLUMNS} FROM tenants WHERE id = ?1"),
                [id],
                tenant_of,
            )
            .optional()?)
    }

    /// The record of `did_scid` that is not deleted.
    pub fn live_tenant_of(&self, did_scid: &str) -> Ad4mDbResult<Option<TenantRow>> {
        Ok(self
            .conn
            .query_row(
                &format!(
                    "SELECT {TENANT_COLUMNS} FROM tenants WHERE did_scid = ?1 AND state <> 'deleted'"
                ),
                [did_scid],
                tenant_of,
            )
            .optional()?)
    }

    /// Every record that is not deleted, oldest first.
    pub fn live_tenants(&self) -> Ad4mDbResult<Vec<TenantRow>> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {TENANT_COLUMNS} FROM tenants WHERE state <> 'deleted' ORDER BY created_at, id"
        ))?;
        let rows = stmt.query_map([], tenant_of)?;
        Ok(rows.collect::<Result<_, _>>()?)
    }

    /// The `Pending` records whose time ran out by `now`.
    pub fn expired_pending_tenants(&self, now: u64) -> Ad4mDbResult<Vec<TenantRow>> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {TENANT_COLUMNS} FROM tenants
             WHERE state = 'pending' AND pending_expires_at IS NOT NULL AND pending_expires_at <= ?1"
        ))?;
        let rows = stmt.query_map([now], tenant_of)?;
        Ok(rows.collect::<Result<_, _>>()?)
    }

    /// The number of `Active` and `Pending` records.
    pub fn count_serving_tenants(&self) -> Ad4mDbResult<u32> {
        Ok(self.conn.query_row(
            "SELECT COUNT(*) FROM tenants WHERE state IN ('active','pending')",
            [],
            |row| row.get(0),
        )?)
    }

    /// Moves record `id` to `to` when it is in one of the states `from`, writing the other
    /// columns as given, in one statement. `false` when the record is in no state of `from`.
    pub fn transition_tenant(&self, id: &str, from: &[&str], to: &TenantRow) -> Ad4mDbResult<bool> {
        let marks = vec!["?"; from.len()].join(",");
        let mut args: Vec<Box<dyn rusqlite::ToSql>> = vec![
            Box::new(to.device_key_name.clone()),
            Box::new(to.device_did.clone()),
            Box::new(to.state.clone()),
            Box::new(to.joined_via.clone()),
            Box::new(to.activated_at),
            Box::new(to.detached_at),
            Box::new(to.pending_expires_at),
            Box::new(id.to_string()),
        ];
        for state in from {
            args.push(Box::new(state.to_string()));
        }
        let changed = self.conn.execute(
            &format!(
                "UPDATE tenants SET device_key_name = ?1, device_did = ?2, state = ?3,
                    joined_via = ?4, activated_at = ?5, detached_at = ?6, pending_expires_at = ?7
                 WHERE id = ?8 AND state IN ({marks})"
            ),
            rusqlite::params_from_iter(args.iter().map(|a| a.as_ref())),
        )?;
        Ok(changed == 1)
    }

    /// Removes a record that never became a tenant (a `Pending` one).
    pub fn remove_tenant_row(&self, id: &str) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "DELETE FROM tenants WHERE id = ?1 AND state = 'pending'",
            [id],
        )? == 1)
    }

    /// Turns record `id` into its tombstone: `deleted`, with no identity, key or route.
    /// `did_scid` becomes the hash the caller gives, so the record names no person.
    pub fn tombstone_tenant(&self, id: &str, did_hash: &str, now: u64) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE tenants SET state = 'deleted', did_scid = ?2, device_key_name = NULL,
                device_did = NULL, joined_via = '{}', detached_at = ?3, pending_expires_at = NULL
             WHERE id = ?1 AND state <> 'deleted'",
            params![id, did_hash, now],
        )? == 1)
    }

    /// Stores a new invite.
    pub fn insert_invite(&self, invite: &InviteRow) -> Ad4mDbResult<()> {
        self.conn.execute(
            &format!(
                "INSERT INTO host_invites ({INVITE_COLUMNS}) VALUES (?1,?2,?3,?4,?5,?6,?7,?8)"
            ),
            params![
                invite.id,
                invite.secret_hash,
                invite.created_by,
                invite.created_at,
                invite.expires_at,
                invite.used_by,
                invite.revoked as i64,
                invite.note,
            ],
        )?;
        Ok(())
    }

    pub fn invite(&self, id: &str) -> Ad4mDbResult<Option<InviteRow>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {INVITE_COLUMNS} FROM host_invites WHERE id = ?1"),
                [id],
                invite_of,
            )
            .optional()?)
    }

    /// The invite whose secret hashes to `hash`.
    pub fn invite_by_hash(&self, hash: &[u8]) -> Ad4mDbResult<Option<InviteRow>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {INVITE_COLUMNS} FROM host_invites WHERE secret_hash = ?1"),
                [hash],
                invite_of,
            )
            .optional()?)
    }

    pub fn invites(&self) -> Ad4mDbResult<Vec<InviteRow>> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {INVITE_COLUMNS} FROM host_invites ORDER BY created_at, id"
        ))?;
        let rows = stmt.query_map([], invite_of)?;
        Ok(rows.collect::<Result<_, _>>()?)
    }

    /// The invites that are not used, not revoked and not expired at `now`.
    pub fn count_open_invites(&self, now: u64) -> Ad4mDbResult<u32> {
        Ok(self.conn.query_row(
            "SELECT COUNT(*) FROM host_invites
             WHERE used_by IS NULL AND revoked = 0 AND expires_at > ?1",
            [now],
            |row| row.get(0),
        )?)
    }

    /// Spends invite `id` for `tenant_id`, once: `false` when it is used, revoked, expired
    /// at `now` or unknown.
    pub fn spend_invite(&self, id: &str, tenant_id: &str, now: u64) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE host_invites SET used_by = ?2
             WHERE id = ?1 AND used_by IS NULL AND revoked = 0 AND expires_at > ?3",
            params![id, tenant_id, now],
        )? == 1)
    }

    /// Makes the invite that `tenant_id` spent usable again. `false` when it spent none.
    pub fn unspend_invite(&self, tenant_id: &str) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE host_invites SET used_by = NULL WHERE used_by = ?1",
            [tenant_id],
        )? > 0)
    }

    /// Revokes an invite that nobody used. `false` for an unknown or used one.
    pub fn revoke_invite(&self, id: &str) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "UPDATE host_invites SET revoked = 1 WHERE id = ?1 AND used_by IS NULL",
            [id],
        )? == 1)
    }

    /// Counts one AI run of `tenant_id` on `day` (`YYYY-MM-DD`, UTC).
    pub fn count_ai_run(&self, tenant_id: &str, day: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO tenant_ai_runs (tenant_id, day, runs) VALUES (?1, ?2, 1)
             ON CONFLICT(tenant_id, day) DO UPDATE SET runs = runs + 1",
            params![tenant_id, day],
        )?;
        Ok(())
    }

    /// The AI runs of `tenant_id` on `day`.
    pub fn ai_runs(&self, tenant_id: &str, day: &str) -> Ad4mDbResult<u64> {
        Ok(self
            .conn
            .query_row(
                "SELECT runs FROM tenant_ai_runs WHERE tenant_id = ?1 AND day = ?2",
                params![tenant_id, day],
                |row| row.get(0),
            )
            .optional()?
            .unwrap_or(0))
    }

    /// Tests: removes the setting `key`.
    #[cfg(test)]
    pub(crate) fn clear_setting_for_test(&self, key: &str) -> Ad4mDbResult<()> {
        self.conn
            .execute("DELETE FROM settings WHERE key = ?1", [key])?;
        Ok(())
    }

    /// Deletes the `users` row `username` (a tenant's legacy row, at its delete).
    pub fn delete_user(&self, username: &str) -> Ad4mDbResult<bool> {
        Ok(self
            .conn
            .execute("DELETE FROM users WHERE username = ?1", [username])?
            == 1)
    }

    /// Drops the identity the local agent `agent_did` speaks for: its device key is gone.
    pub fn unbind_agent_identity(&self, agent_did: &str) -> Ad4mDbResult<bool> {
        Ok(self.conn.execute(
            "DELETE FROM identity_agents WHERE agent_did = ?1",
            [agent_did],
        )? == 1)
    }

    /// Empties the tenants, invites and AI-run tables, for tests on the shared database.
    #[cfg(test)]
    pub fn clear_hosting_for_test(&self) -> Ad4mDbResult<()> {
        self.conn.execute_batch(
            "DELETE FROM tenants; DELETE FROM host_invites; DELETE FROM tenant_ai_runs;",
        )?;
        Ok(())
    }

    /// Deletes every AI-run row of `tenant_id`.
    pub fn delete_ai_runs(&self, tenant_id: &str) -> Ad4mDbResult<usize> {
        Ok(self.conn.execute(
            "DELETE FROM tenant_ai_runs WHERE tenant_id = ?1",
            [tenant_id],
        )?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(id: &str, did: &str, state: &str) -> TenantRow {
        TenantRow {
            id: id.into(),
            did_scid: did.into(),
            device_key_name: Some(format!("user:{id}")),
            device_did: Some("did:key:z".into()),
            state: state.into(),
            joined_via: "\"anyone\"".into(),
            created_at: 10,
            activated_at: None,
            detached_at: None,
            pending_expires_at: Some(610),
        }
    }

    #[test]
    fn one_live_record_per_identity() {
        let db = Ad4mDb::new(":memory:").unwrap();
        assert_eq!(
            db.insert_tenant(&row("t-1", "did:scid:coasys:1:a", "active"))
                .unwrap(),
            TenantInsert::Inserted
        );
        assert_eq!(
            db.insert_tenant(&row("t-2", "did:scid:coasys:1:a", "pending"))
                .unwrap(),
            TenantInsert::LiveExists
        );
        db.tombstone_tenant("t-1", "hash", 20).unwrap();
        assert_eq!(
            db.insert_tenant(&row("t-2", "did:scid:coasys:1:a", "pending"))
                .unwrap(),
            TenantInsert::Inserted,
            "a deleted record frees the identity"
        );
    }

    #[test]
    fn transitions_need_the_from_state() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.insert_tenant(&row("t-1", "did:scid:coasys:1:a", "pending"))
            .unwrap();
        let mut next = row("t-1", "did:scid:coasys:1:a", "active");
        next.activated_at = Some(30);
        assert!(!db.transition_tenant("t-1", &["held"], &next).unwrap());
        assert_eq!(db.tenant("t-1").unwrap().unwrap().state, "pending");
        assert!(db.transition_tenant("t-1", &["pending"], &next).unwrap());
        assert_eq!(db.tenant("t-1").unwrap().unwrap().activated_at, Some(30));
    }

    #[test]
    fn an_invite_is_spent_once_and_comes_back() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.insert_invite(&InviteRow {
            id: "i1".into(),
            secret_hash: vec![1; 32],
            created_by: "op".into(),
            created_at: 1,
            expires_at: 100,
            used_by: None,
            revoked: false,
            note: None,
        })
        .unwrap();
        assert!(db.spend_invite("i1", "t-1", 50).unwrap());
        assert!(!db.spend_invite("i1", "t-2", 50).unwrap());
        assert!(db.unspend_invite("t-1").unwrap());
        assert!(!db.spend_invite("i1", "t-2", 100).unwrap(), "expired");
        assert!(db.spend_invite("i1", "t-2", 99).unwrap());
        assert!(
            !db.revoke_invite("i1").unwrap(),
            "a used invite is not revoked"
        );
    }

    #[test]
    fn ai_runs_count_per_day() {
        let db = Ad4mDb::new(":memory:").unwrap();
        for _ in 0..3 {
            db.count_ai_run("t-1", "2026-11-20").unwrap();
        }
        assert_eq!(db.ai_runs("t-1", "2026-11-20").unwrap(), 3);
        assert_eq!(db.ai_runs("t-1", "2026-11-21").unwrap(), 0);
        db.delete_ai_runs("t-1").unwrap();
        assert_eq!(db.ai_runs("t-1", "2026-11-20").unwrap(), 0);
    }
}
