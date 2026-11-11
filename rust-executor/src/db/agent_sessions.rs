//! The `agent_sessions` and `agent_messages` tables: the executor's own record of an agent
//! session and its transcript.
//!
//! `agent_sessions.owner_did` names the managed agent that owns the session; `NULL` is a
//! session of the main agent (the `tasks.agent_did` convention, D16). Every read and write
//! takes the owner's DID and scopes to it (`owner_did IS ?`, which `=` never does for NULL),
//! so a caller can never see or touch another agent's session. Deleting a session deletes
//! its messages in the same transaction.
//!
//! `agent_messages.seq` is the order of a session's transcript, assigned inside the INSERT
//! (`MAX(seq) + 1` of the session), so entries append atomically under the database lock.
//! `status` is set on the assistant's turns and on tool steps: `running` while the turn's
//! task is alive, then `done`, `failed`, `cancelled` or `interrupted`.

use rusqlite::{params, Connection, OptionalExtension};

use super::Ad4mDb;
use crate::agent_sessions::types::{
    AgentSession, AgentSessionMessage, AgentSessionMessageRole, AgentSessionUpdate, AgentTurnStatus,
};

/// The `agent_sessions` and `agent_messages` tables. Called from `Ad4mDb::new` with the
/// rest of the schema.
pub(crate) fn create_agent_session_tables(conn: &Connection) -> Result<(), rusqlite::Error> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS agent_sessions (
            id TEXT PRIMARY KEY NOT NULL,
            owner_did TEXT,
            title TEXT NOT NULL DEFAULT '',
            model TEXT,
            default_perspective TEXT,
            library_perspective TEXT,
            capabilities TEXT NOT NULL DEFAULT '[]',
            created_at TEXT NOT NULL,
            updated_at TEXT NOT NULL
         );
         CREATE INDEX IF NOT EXISTS idx_agent_sessions_owner
             ON agent_sessions (owner_did);
         CREATE TABLE IF NOT EXISTS agent_messages (
            id TEXT PRIMARY KEY NOT NULL,
            session_id TEXT NOT NULL,
            seq INTEGER NOT NULL,
            role TEXT NOT NULL,
            content TEXT NOT NULL,
            status TEXT,
            error TEXT,
            created_at TEXT NOT NULL,
            UNIQUE (session_id, seq)
         );
         CREATE INDEX IF NOT EXISTS idx_agent_messages_session
             ON agent_messages (session_id, seq);",
    )
}

fn session_from(row: &rusqlite::Row<'_>) -> Result<AgentSession, rusqlite::Error> {
    let capabilities: String = row.get(6)?;
    Ok(AgentSession {
        id: row.get(0)?,
        owner_did: row.get(1)?,
        title: row.get(2)?,
        model: row.get(3)?,
        default_perspective: row.get(4)?,
        library_perspective: row.get(5)?,
        capabilities: serde_json::from_str(&capabilities).map_err(|e| {
            rusqlite::Error::FromSqlConversionFailure(6, rusqlite::types::Type::Text, Box::new(e))
        })?,
        created_at: row.get(7)?,
        updated_at: row.get(8)?,
    })
}

const SESSION_COLUMNS: &str =
    "id, owner_did, title, model, default_perspective, library_perspective, capabilities, \
     created_at, updated_at";

fn message_from(row: &rusqlite::Row<'_>) -> Result<AgentSessionMessage, rusqlite::Error> {
    let role: String = row.get(3)?;
    let content: String = row.get(4)?;
    let status: Option<String> = row.get(5)?;
    Ok(AgentSessionMessage {
        id: row.get(0)?,
        session_id: row.get(1)?,
        seq: row.get(2)?,
        role: AgentSessionMessageRole::parse(&role).ok_or_else(|| {
            rusqlite::Error::FromSqlConversionFailure(
                3,
                rusqlite::types::Type::Text,
                format!("unknown message role {role:?}").into(),
            )
        })?,
        content: serde_json::from_str(&content).map_err(|e| {
            rusqlite::Error::FromSqlConversionFailure(4, rusqlite::types::Type::Text, Box::new(e))
        })?,
        status: status.and_then(|s| match s.as_str() {
            "running" => Some(AgentTurnStatus::Running),
            "done" => Some(AgentTurnStatus::Done),
            "failed" => Some(AgentTurnStatus::Failed),
            "cancelled" => Some(AgentTurnStatus::Cancelled),
            "interrupted" => Some(AgentTurnStatus::Interrupted),
            _ => None,
        }),
        error: row.get(6)?,
        created_at: row.get(7)?,
    })
}

const MESSAGE_COLUMNS: &str = "id, session_id, seq, role, content, status, error, created_at";

impl Ad4mDb {
    /// `session`, written. Its `owner_did` is the managed agent's DID, `None` for the main
    /// agent's sessions.
    pub fn create_agent_session(&self, session: &AgentSession) -> Result<(), rusqlite::Error> {
        let capabilities = serde_json::to_string(&session.capabilities)
            .map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))?;
        self.conn.execute(
            "INSERT INTO agent_sessions
                (id, owner_did, title, model, default_perspective, library_perspective,
                 capabilities, created_at, updated_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
            params![
                session.id,
                session.owner_did,
                session.title,
                session.model,
                session.default_perspective,
                session.library_perspective,
                capabilities,
                session.created_at,
                session.updated_at,
            ],
        )?;
        Ok(())
    }

    /// The session `id` of the agent `owner_did` (`None`: the main agent), `None` when it
    /// is missing or another agent's.
    pub fn get_agent_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AgentSession>, rusqlite::Error> {
        self.conn
            .query_row(
                &format!(
                    "SELECT {SESSION_COLUMNS} FROM agent_sessions
                     WHERE id = ?1 AND owner_did IS ?2"
                ),
                params![id, owner_did],
                session_from,
            )
            .optional()
    }

    /// The session `id` whoever owns it: for the executor's own lookups (the task a run
    /// belongs to). Callers that take a user's word for an id use `get_agent_session`.
    pub fn get_agent_session_any(&self, id: &str) -> Result<Option<AgentSession>, rusqlite::Error> {
        self.conn
            .query_row(
                &format!("SELECT {SESSION_COLUMNS} FROM agent_sessions WHERE id = ?1"),
                params![id],
                session_from,
            )
            .optional()
    }

    /// Every session of the agent `owner_did` (`None`: the main agent), newest first.
    pub fn list_agent_sessions(
        &self,
        owner_did: Option<&str>,
    ) -> Result<Vec<AgentSession>, rusqlite::Error> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {SESSION_COLUMNS} FROM agent_sessions
             WHERE owner_did IS ?1 ORDER BY updated_at DESC, id"
        ))?;
        let rows = stmt.query_map(params![owner_did], session_from)?;
        rows.collect()
    }

    /// The fields `update` names changed on `id` of `owner_did`. `false`: no such session
    /// of this owner. `updated_at` moves with the change.
    pub fn update_agent_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
        update: &AgentSessionUpdate,
    ) -> Result<bool, rusqlite::Error> {
        let mut sets: Vec<&str> = vec!["updated_at = ?NOW"];
        let mut values: Vec<rusqlite::types::Value> = Vec::new();
        if let Some(title) = &update.title {
            sets.push("title = ?");
            values.push(title.clone().into());
        }
        if let Some(model) = &update.model {
            sets.push("model = ?");
            values.push(match model {
                Some(m) => m.clone().into(),
                None => rusqlite::types::Value::Null,
            });
        }
        if let Some(perspective) = &update.default_perspective {
            sets.push("default_perspective = ?");
            values.push(match perspective {
                Some(p) => p.clone().into(),
                None => rusqlite::types::Value::Null,
            });
        }
        if let Some(library) = &update.library_perspective {
            sets.push("library_perspective = ?");
            values.push(match library {
                Some(l) => l.clone().into(),
                None => rusqlite::types::Value::Null,
            });
        }
        if let Some(capabilities) = &update.capabilities {
            sets.push("capabilities = ?");
            let caps = serde_json::to_string(capabilities)
                .map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))?;
            values.push(caps.into());
        }
        let now = chrono::Utc::now().to_rfc3339();
        let sql = format!(
            "UPDATE agent_sessions SET {} WHERE id = ? AND owner_did IS ?",
            sets.join(", ").replace("?NOW", "?"),
        );
        values.push(id.to_string().into());
        values.push(match owner_did {
            Some(did) => did.to_string().into(),
            None => rusqlite::types::Value::Null,
        });
        let mut bound = vec![now.into()];
        bound.extend(values);
        let changed = self.conn.execute(&sql, rusqlite::params_from_iter(bound))?;
        Ok(changed > 0)
    }

    /// The session `id` of `owner_did` and every message of it, gone in one transaction.
    /// `false`: no such session of this owner.
    pub fn delete_agent_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<bool, rusqlite::Error> {
        let tx = self.conn.unchecked_transaction()?;
        tx.execute(
            "DELETE FROM agent_messages WHERE session_id IN
                (SELECT id FROM agent_sessions WHERE id = ?1 AND owner_did IS ?2)",
            params![id, owner_did],
        )?;
        let changed = tx.execute(
            "DELETE FROM agent_sessions WHERE id = ?1 AND owner_did IS ?2",
            params![id, owner_did],
        )?;
        tx.commit()?;
        Ok(changed > 0)
    }

    /// The transcript entry `role`/`content`/`status` appended to the session `session_id`
    /// of `owner_did`, with the session's next `seq`. `None`: no such session of this
    /// owner. `status` stays NULL on a `user` row.
    pub fn append_agent_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        session_id: &str,
        role: &str,
        content: &str,
        status: Option<&str>,
    ) -> Result<Option<AgentSessionMessage>, rusqlite::Error> {
        // The INSERT's EXISTS keeps the append owner-scoped like every other access: a
        // caller that knows another session's id still writes nothing to it.
        self.conn.execute(
            "INSERT INTO agent_messages (id, session_id, seq, role, content, status, error, created_at)
             SELECT ?1, ?2,
                    COALESCE((SELECT MAX(seq) FROM agent_messages WHERE session_id = ?2), 0) + 1,
                    ?3, ?4, ?5, NULL, ?6
             WHERE EXISTS (SELECT 1 FROM agent_sessions WHERE id = ?2 AND owner_did IS ?7)",
            params![
                id,
                session_id,
                role,
                content,
                status,
                chrono::Utc::now().to_rfc3339(),
                owner_did,
            ],
        )?;
        self.get_agent_message(owner_did, id)
    }

    /// The message `id` of a session the agent `owner_did` owns, `None` when it is missing
    /// or another agent's.
    pub fn get_agent_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AgentSessionMessage>, rusqlite::Error> {
        self.conn
            .query_row(
                &format!(
                    "SELECT {MESSAGE_COLUMNS} FROM agent_messages m
                     WHERE m.id = ?1 AND EXISTS (
                        SELECT 1 FROM agent_sessions s
                        WHERE s.id = m.session_id AND s.owner_did IS ?2
                     )"
                ),
                params![id, owner_did],
                message_from,
            )
            .optional()
    }

    /// `status` (and `error`, `content_patch` merged into `content`) written on the message
    /// `id` of a session `owner_did` owns. `false`: no such message of this owner.
    pub fn finish_agent_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        status: &str,
        error: Option<&str>,
        content_patch: Option<&serde_json::Value>,
    ) -> Result<bool, rusqlite::Error> {
        let patch = match content_patch {
            Some(patch) => Some(
                serde_json::to_string(patch)
                    .map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))?,
            ),
            None => None,
        };
        let changed = self.conn.execute(
            "UPDATE agent_messages SET
                status = ?3,
                error = ?4,
                content = CASE WHEN ?5 IS NULL THEN content ELSE json_patch(content, ?5) END
             WHERE id = ?1 AND EXISTS (
                SELECT 1 FROM agent_sessions s
                WHERE s.id = agent_messages.session_id AND s.owner_did IS ?2
             )",
            params![id, owner_did, status, error, patch],
        )?;
        Ok(changed > 0)
    }

    /// The newest `limit` transcript entries of the session `session_id` of `owner_did`,
    /// in `seq` order; with `before`, only those older than that `seq`. Empty when the
    /// session is missing or another agent's.
    pub fn list_agent_messages(
        &self,
        owner_did: Option<&str>,
        session_id: &str,
        limit: u32,
        before: Option<i64>,
    ) -> Result<Vec<AgentSessionMessage>, rusqlite::Error> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {MESSAGE_COLUMNS} FROM (
                SELECT * FROM agent_messages m
                WHERE m.session_id = ?2
                  AND (?3 IS NULL OR m.seq < ?3)
                  AND EXISTS (
                    SELECT 1 FROM agent_sessions s
                    WHERE s.id = m.session_id AND s.owner_did IS ?1
                  )
                ORDER BY m.seq DESC LIMIT ?4
             ) ORDER BY seq"
        ))?;
        let rows = stmt.query_map(
            params![owner_did, session_id, before, limit as i64],
            message_from,
        )?;
        rows.collect()
    }

    /// How many of `owner_did`'s sessions have an assistant turn marked `running`: the
    /// durable half of the per-caller run limit, so a leaked task handle can never hide
    /// a running turn.
    pub fn count_running_agent_turns(
        &self,
        owner_did: Option<&str>,
    ) -> Result<i64, rusqlite::Error> {
        self.conn.query_row(
            "SELECT COUNT(*) FROM agent_messages m
             JOIN agent_sessions s ON s.id = m.session_id
             WHERE s.owner_did IS ?1 AND m.status = 'running'",
            params![owner_did],
            |row| row.get(0),
        )
    }

    /// Every row still marked `running` — of the session `session_id` when one is named,
    /// of the whole table at startup — marked `interrupted`. A row is `running` while its
    /// task is alive; a `running` row that survives the task that wrote it was cut off.
    pub fn interrupt_running_agent_messages(
        &self,
        session_id: Option<&str>,
    ) -> Result<u64, rusqlite::Error> {
        let changed = self.conn.execute(
            "UPDATE agent_messages SET
                status = 'interrupted',
                error = COALESCE(error, 'the executor restarted, or the task that ran it ended')
             WHERE status = 'running' AND (?1 IS NULL OR session_id = ?1)",
            params![session_id],
        )?;
        Ok(changed as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::Capability;
    use serde_json::json;

    fn session(id: &str, owner: Option<&str>) -> AgentSession {
        AgentSession {
            id: id.into(),
            owner_did: owner.map(str::to_string),
            title: format!("session {id}"),
            model: None,
            default_perspective: None,
            library_perspective: None,
            capabilities: vec![Capability {
                with: crate::agent::capabilities::types::Resource {
                    domain: "*".into(),
                    pointers: vec!["*".into()],
                },
                can: vec!["*".into()],
            }],
            created_at: "2026-01-01T00:00:00Z".into(),
            updated_at: "2026-01-01T00:00:00Z".into(),
        }
    }

    fn append(
        db: &Ad4mDb,
        owner: Option<&str>,
        id: &str,
        session: &str,
        role: &str,
        seq_wanted: i64,
    ) {
        let message = db
            .append_agent_message(owner, id, session, role, "{}", None)
            .unwrap()
            .expect("append succeeds");
        assert_eq!(message.seq, seq_wanted, "seq of {id}");
    }

    #[test]
    fn sessions_and_messages_are_scoped_to_their_owner() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let alice = Some("did:key:alice");
        let bob = Some("did:key:bob");
        db.create_agent_session(&session("s1", alice)).unwrap();
        db.create_agent_session(&session("s2", bob)).unwrap();
        db.create_agent_session(&session("s3", None)).unwrap();

        assert!(db.get_agent_session(alice, "s1").unwrap().is_some());
        assert!(db.get_agent_session(alice, "s2").unwrap().is_none());
        assert!(db.get_agent_session(bob, "s1").unwrap().is_none());
        assert!(db.get_agent_session(None, "s3").unwrap().is_some());
        assert!(db.get_agent_session(alice, "s3").unwrap().is_none());

        assert_eq!(db.list_agent_sessions(alice).unwrap().len(), 1);
        assert_eq!(db.list_agent_sessions(bob).unwrap().len(), 1);
        assert_eq!(db.list_agent_sessions(None).unwrap().len(), 1);

        // A caller that knows another session's id can append nothing to it.
        assert!(db
            .append_agent_message(bob, "m-x", "s1", "user", "{}", None)
            .unwrap()
            .is_none());
        append(&db, alice, "m1", "s1", "user", 1);
        append(&db, alice, "m2", "s1", "assistant", 2);
        assert_eq!(
            db.list_agent_messages(alice, "s1", 100, None)
                .unwrap()
                .len(),
            2
        );
        assert!(db
            .list_agent_messages(bob, "s1", 100, None)
            .unwrap()
            .is_empty());

        assert!(db
            .update_agent_session(
                alice,
                "s1",
                &AgentSessionUpdate {
                    title: Some("renamed".into()),
                    ..Default::default()
                }
            )
            .unwrap());
        assert!(!db
            .update_agent_session(
                bob,
                "s1",
                &AgentSessionUpdate {
                    title: Some("stolen".into()),
                    ..Default::default()
                }
            )
            .unwrap());
        assert_eq!(
            db.get_agent_session(alice, "s1").unwrap().unwrap().title,
            "renamed"
        );

        assert!(!db.delete_agent_session(bob, "s1").unwrap());
        assert!(db.delete_agent_session(alice, "s1").unwrap());
        assert!(db.get_agent_session(alice, "s1").unwrap().is_none());
        assert!(db
            .list_agent_messages(alice, "s1", 100, None)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn a_session_update_clears_nullable_fields_and_keeps_the_rest() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let alice = Some("did:key:alice");
        let mut s = session("s1", alice);
        s.model = Some("model-a".into());
        s.default_perspective = Some("p1".into());
        db.create_agent_session(&s).unwrap();

        db.update_agent_session(
            alice,
            "s1",
            &AgentSessionUpdate {
                model: Some(None),
                default_perspective: Some(Some("p2".into())),
                ..Default::default()
            },
        )
        .unwrap();
        let s = db.get_agent_session(alice, "s1").unwrap().unwrap();
        assert_eq!(s.model, None);
        assert_eq!(s.default_perspective.as_deref(), Some("p2"));
        assert_eq!(s.capabilities.len(), 1, "unsent fields are kept");
    }

    #[test]
    fn finish_writes_the_end_state_and_the_sweep_marks_what_is_left_running() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let alice = Some("did:key:alice");
        db.create_agent_session(&session("s1", alice)).unwrap();
        append(&db, alice, "u1", "s1", "user", 1);
        db.append_agent_message(alice, "a1", "s1", "assistant", "{}", Some("running"))
            .unwrap();
        db.append_agent_message(alice, "t1", "s1", "tool", "{}", Some("running"))
            .unwrap();

        assert_eq!(db.count_running_agent_turns(alice).unwrap(), 2);
        assert!(db
            .finish_agent_message(alice, "a1", "done", None, Some(&json!({ "text": "hi" })),)
            .unwrap());
        let a1 = db.get_agent_message(alice, "a1").unwrap().unwrap();
        assert_eq!(a1.status, Some(AgentTurnStatus::Done));
        assert_eq!(a1.content["text"], "hi");
        // Bob cannot finish Alice's row.
        assert!(!db
            .finish_agent_message(Some("did:key:bob"), "t1", "done", None, None)
            .unwrap());

        assert_eq!(db.interrupt_running_agent_messages(None).unwrap(), 1);
        let t1 = db.get_agent_message(alice, "t1").unwrap().unwrap();
        assert_eq!(t1.status, Some(AgentTurnStatus::Interrupted));
        assert!(t1.error.is_some());
        assert_eq!(db.interrupt_running_agent_messages(None).unwrap(), 0);
    }
}
