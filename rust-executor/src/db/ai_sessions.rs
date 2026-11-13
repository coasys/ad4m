//! The `ai_sessions` and `ai_messages` tables: the executor's own record of an agent
//! session and its transcript.
//!
//! `ai_sessions.owner_did` names the managed agent that owns the session; `NULL` is a
//! session of the main agent (the `tasks.agent_did` convention, D16). Every read and write
//! takes the owner's DID and scopes to it (`owner_did IS ?`, which `=` never does for NULL),
//! so a caller can never see or touch another agent's session. Deleting a session deletes
//! its messages in the same transaction.
//!
//! `ai_messages.seq` is the order of a session's transcript, assigned inside the INSERT
//! (`MAX(seq) + 1` of the session), so entries append atomically under the database lock.
//! `status` is set on the assistant's turns and on tool steps: `running` while the turn's
//! task is alive, then `done`, `failed`, `cancelled` or `interrupted`.
//!
//! A conversation is its owner's private data: a session's `title`, `default_perspective`,
//! `library_perspective` and `capabilities`, and a message's `content` and `error`, are
//! sealed under the owner's store ([`super::sealed`]); a message's owner is its session's.
//! The ids, the owner, `model`, `role`, `seq`, `status` and the timestamps stay in the
//! clear: the queries select, scope and order by them. A patch to a message's content is
//! merged here, on the opened value, not by SQLite's `json_patch` on the sealed one.

use rusqlite::{params, Connection, OptionalExtension};
use serde_json::Value;

use super::sealed::{self, AI_MESSAGES, AI_MESSAGE_OWNER, AI_SESSIONS};
use super::Ad4mDb;
use crate::ai_sessions::types::{
    AiSession, AiSessionMessage, AiSessionMessageRole, AiSessionUpdate, AiTurnStatus,
};

/// The `error` an interrupted row gets when it has none.
const INTERRUPTED: &str = "the executor restarted, or the task that ran it ended";

/// The `ai_sessions` and `ai_messages` tables. Called from `Ad4mDb::new` with the
/// rest of the schema.
pub(crate) fn create_ai_session_tables(conn: &Connection) -> Result<(), rusqlite::Error> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS ai_sessions (
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
         CREATE INDEX IF NOT EXISTS idx_ai_sessions_owner
             ON ai_sessions (owner_did);
         CREATE TABLE IF NOT EXISTS ai_messages (
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
         CREATE INDEX IF NOT EXISTS idx_ai_messages_session
             ON ai_messages (session_id, seq);",
    )
}

fn not_json(column: usize, e: serde_json::Error) -> rusqlite::Error {
    rusqlite::Error::FromSqlConversionFailure(column, rusqlite::types::Type::Text, Box::new(e))
}

fn to_json<T: serde::Serialize>(value: &T) -> Result<String, rusqlite::Error> {
    serde_json::to_string(value).map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))
}

/// `patch` merged into `target` as JSON Merge Patch (RFC 7396), what SQLite's `json_patch`
/// does: an object member set to `null` is removed, an object merges member by member, any
/// other value replaces.
fn merge_patch(target: &mut Value, patch: &Value) {
    let Value::Object(members) = patch else {
        *target = patch.clone();
        return;
    };
    if !target.is_object() {
        *target = Value::Object(Default::default());
    }
    let Value::Object(target) = target else {
        unreachable!("made an object above")
    };
    for (name, value) in members {
        if value.is_null() {
            target.remove(name);
        } else {
            merge_patch(target.entry(name.clone()).or_insert(Value::Null), value);
        }
    }
}

const SESSION_COLUMNS: &str =
    "id, owner_did, title, model, default_perspective, library_perspective, capabilities, \
     created_at, updated_at";

const MESSAGE_COLUMNS: &str = "id, session_id, seq, role, content, status, error, created_at";

impl Ad4mDb {
    /// A session from a row of [`SESSION_COLUMNS`], its sealed values opened.
    fn session_from(&self, row: &rusqlite::Row<'_>) -> Result<AiSession, rusqlite::Error> {
        let id: String = row.get(0)?;
        let owner_did: Option<String> = row.get(1)?;
        let seal = self.reader(&AI_SESSIONS, owner_did.as_deref(), &[&id])?;
        let capabilities = seal.open("capabilities", row.get(6)?)?;
        Ok(AiSession {
            title: seal.open("title", row.get(2)?)?,
            model: row.get(3)?,
            default_perspective: seal.open_opt("default_perspective", row.get(4)?)?,
            library_perspective: seal.open_opt("library_perspective", row.get(5)?)?,
            capabilities: serde_json::from_str(&capabilities).map_err(|e| not_json(6, e))?,
            created_at: row.get(7)?,
            updated_at: row.get(8)?,
            id,
            owner_did,
        })
    }

    /// A message from a row of [`MESSAGE_COLUMNS`] of a session `owner_did` owns, its sealed
    /// values opened.
    fn message_from(
        &self,
        owner_did: Option<&str>,
        row: &rusqlite::Row<'_>,
    ) -> Result<AiSessionMessage, rusqlite::Error> {
        let id: String = row.get(0)?;
        let seal = self.reader(&AI_MESSAGES, owner_did, &[&id])?;
        let role: String = row.get(3)?;
        let content = seal.open("content", row.get(4)?)?;
        let status: Option<String> = row.get(5)?;
        Ok(AiSessionMessage {
            session_id: row.get(1)?,
            seq: row.get(2)?,
            role: AiSessionMessageRole::parse(&role).ok_or_else(|| {
                rusqlite::Error::FromSqlConversionFailure(
                    3,
                    rusqlite::types::Type::Text,
                    format!("unknown message role {role:?}").into(),
                )
            })?,
            content: serde_json::from_str(&content).map_err(|e| not_json(4, e))?,
            status: status.and_then(|s| match s.as_str() {
                "running" => Some(AiTurnStatus::Running),
                "done" => Some(AiTurnStatus::Done),
                "failed" => Some(AiTurnStatus::Failed),
                "cancelled" => Some(AiTurnStatus::Cancelled),
                "interrupted" => Some(AiTurnStatus::Interrupted),
                _ => None,
            }),
            error: seal.open_opt("error", row.get(6)?)?,
            created_at: row.get(7)?,
            id,
        })
    }

    /// `session`, written. Its `owner_did` is the managed agent's DID, `None` for the main
    /// agent's sessions.
    pub fn create_ai_session(&self, session: &AiSession) -> Result<(), rusqlite::Error> {
        let seal = self.writer(
            &AI_SESSIONS,
            session.owner_did.as_deref(),
            &[&session.id],
            None,
        )?;
        self.conn.execute(
            "INSERT INTO ai_sessions
                (id, owner_did, title, model, default_perspective, library_perspective,
                 capabilities, created_at, updated_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
            params![
                session.id,
                session.owner_did,
                seal.seal("title", &session.title)?,
                session.model,
                seal.seal_opt(
                    "default_perspective",
                    session.default_perspective.as_deref()
                )?,
                seal.seal_opt(
                    "library_perspective",
                    session.library_perspective.as_deref()
                )?,
                seal.seal("capabilities", &to_json(&session.capabilities)?)?,
                session.created_at,
                session.updated_at,
            ],
        )?;
        Ok(())
    }

    /// The session `id` of the agent `owner_did` (`None`: the main agent), `None` when it
    /// is missing or another agent's, or when its sealed values do not open.
    pub fn get_ai_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AiSession>, rusqlite::Error> {
        let session = self
            .conn
            .query_row(
                &format!(
                    "SELECT {SESSION_COLUMNS} FROM ai_sessions
                     WHERE id = ?1 AND owner_did IS ?2"
                ),
                params![id, owner_did],
                |row| self.session_from(row),
            )
            .optional();
        Ok(sealed::readable("ai_sessions", session)?.flatten())
    }

    /// The session `id` whoever owns it: for the executor's own lookups (the task a run
    /// belongs to). Callers that take a user's word for an id use `get_ai_session`.
    pub fn get_ai_session_any(&self, id: &str) -> Result<Option<AiSession>, rusqlite::Error> {
        let session = self
            .conn
            .query_row(
                &format!("SELECT {SESSION_COLUMNS} FROM ai_sessions WHERE id = ?1"),
                params![id],
                |row| self.session_from(row),
            )
            .optional();
        Ok(sealed::readable("ai_sessions", session)?.flatten())
    }

    /// Every session of the agent `owner_did` (`None`: the main agent), newest first. A
    /// session whose sealed values do not open is left out.
    pub fn list_ai_sessions(
        &self,
        owner_did: Option<&str>,
    ) -> Result<Vec<AiSession>, rusqlite::Error> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {SESSION_COLUMNS} FROM ai_sessions
             WHERE owner_did IS ?1 ORDER BY updated_at DESC, id"
        ))?;
        let mut sessions = Vec::new();
        for session in stmt.query_map(params![owner_did], |row| self.session_from(row))? {
            sessions.extend(sealed::readable("ai_sessions", session)?);
        }
        Ok(sessions)
    }

    /// The fields `update` names changed on `id` of `owner_did`. `false`: no such session
    /// of this owner. `updated_at` moves with the change.
    pub fn update_ai_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
        update: &AiSessionUpdate,
    ) -> Result<bool, rusqlite::Error> {
        let exists: Option<i64> = self
            .conn
            .query_row(
                "SELECT 1 FROM ai_sessions WHERE id = ?1 AND owner_did IS ?2",
                params![id, owner_did],
                |row| row.get(0),
            )
            .optional()?;
        if exists.is_none() {
            return Ok(false);
        }
        // The owner is the session's: the values are sealed under its store.
        let seal = self.writer(&AI_SESSIONS, owner_did, &[id], None)?;
        let sealed_or_null = |column: &str, value: &Option<String>| {
            Ok::<_, rusqlite::Error>(match seal.seal_opt(column, value.as_deref())? {
                Some(v) => v.into(),
                None => rusqlite::types::Value::Null,
            })
        };
        let mut sets: Vec<&str> = vec!["updated_at = ?"];
        let mut values: Vec<rusqlite::types::Value> = vec![chrono::Utc::now().to_rfc3339().into()];
        if let Some(title) = &update.title {
            sets.push("title = ?");
            values.push(seal.seal("title", title)?.into());
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
            values.push(sealed_or_null("default_perspective", perspective)?);
        }
        if let Some(library) = &update.library_perspective {
            sets.push("library_perspective = ?");
            values.push(sealed_or_null("library_perspective", library)?);
        }
        if let Some(capabilities) = &update.capabilities {
            sets.push("capabilities = ?");
            values.push(seal.seal("capabilities", &to_json(capabilities)?)?.into());
        }
        let sql = format!(
            "UPDATE ai_sessions SET {} WHERE id = ? AND owner_did IS ?",
            sets.join(", "),
        );
        values.push(id.to_string().into());
        values.push(match owner_did {
            Some(did) => did.to_string().into(),
            None => rusqlite::types::Value::Null,
        });
        let changed = self
            .conn
            .execute(&sql, rusqlite::params_from_iter(values))?;
        Ok(changed > 0)
    }

    /// The session `id` of `owner_did` and every message of it, gone in one transaction.
    /// `false`: no such session of this owner.
    pub fn delete_ai_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<bool, rusqlite::Error> {
        let tx = self.conn.unchecked_transaction()?;
        tx.execute(
            "DELETE FROM ai_messages WHERE session_id IN
                (SELECT id FROM ai_sessions WHERE id = ?1 AND owner_did IS ?2)",
            params![id, owner_did],
        )?;
        let changed = tx.execute(
            "DELETE FROM ai_sessions WHERE id = ?1 AND owner_did IS ?2",
            params![id, owner_did],
        )?;
        tx.commit()?;
        Ok(changed > 0)
    }

    /// The transcript entry `role`/`content`/`status` appended to the session `session_id`
    /// of `owner_did`, with the session's next `seq`. `None`: no such session of this
    /// owner. `status` stays NULL on a `user` row.
    pub fn append_ai_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        session_id: &str,
        role: &str,
        content: &str,
        status: Option<&str>,
    ) -> Result<Option<AiSessionMessage>, rusqlite::Error> {
        // Sealed for `owner_did`, which the INSERT's EXISTS holds to be the session's owner.
        let seal = self.writer(&AI_MESSAGES, owner_did, &[id], None)?;
        // The INSERT's EXISTS keeps the append owner-scoped like every other access: a
        // caller that knows another session's id still writes nothing to it.
        self.conn.execute(
            "INSERT INTO ai_messages (id, session_id, seq, role, content, status, error, created_at)
             SELECT ?1, ?2,
                    COALESCE((SELECT MAX(seq) FROM ai_messages WHERE session_id = ?2), 0) + 1,
                    ?3, ?4, ?5, NULL, ?6
             WHERE EXISTS (SELECT 1 FROM ai_sessions WHERE id = ?2 AND owner_did IS ?7)",
            params![
                id,
                session_id,
                role,
                seal.seal("content", content)?,
                status,
                chrono::Utc::now().to_rfc3339(),
                owner_did,
            ],
        )?;
        self.get_ai_message(owner_did, id)
    }

    /// The message `id` of a session the agent `owner_did` owns, `None` when it is missing
    /// or another agent's, or when its sealed values do not open.
    pub fn get_ai_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AiSessionMessage>, rusqlite::Error> {
        let message = self
            .conn
            .query_row(
                &format!(
                    "SELECT {MESSAGE_COLUMNS} FROM ai_messages m
                     WHERE m.id = ?1 AND EXISTS (
                        SELECT 1 FROM ai_sessions s
                        WHERE s.id = m.session_id AND s.owner_did IS ?2
                     )"
                ),
                params![id, owner_did],
                |row| self.message_from(owner_did, row),
            )
            .optional();
        Ok(sealed::readable("ai_messages", message)?.flatten())
    }

    /// `status` (and `error`, `content_patch` merged into `content`) written on the message
    /// `id` of a session `owner_did` owns. `false`: no such message of this owner.
    pub fn finish_ai_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        status: &str,
        error: Option<&str>,
        content_patch: Option<&Value>,
    ) -> Result<bool, rusqlite::Error> {
        let tx = self.conn.unchecked_transaction()?;
        let stored: Option<String> = tx
            .query_row(
                "SELECT m.content FROM ai_messages m
                 WHERE m.id = ?1 AND EXISTS (
                    SELECT 1 FROM ai_sessions s
                    WHERE s.id = m.session_id AND s.owner_did IS ?2
                 )",
                params![id, owner_did],
                |row| row.get(0),
            )
            .optional()?;
        let Some(stored) = stored else {
            return Ok(false);
        };
        // The EXISTS above held `owner_did` to be the session's owner.
        let seal = self.writer(&AI_MESSAGES, owner_did, &[id], None)?;
        // The content is sealed: the patch merges into the opened value.
        let content = match content_patch {
            Some(patch) => {
                let mut content: Value = serde_json::from_str(&seal.open("content", stored)?)
                    .map_err(|e| not_json(4, e))?;
                merge_patch(&mut content, patch);
                Some(seal.seal("content", &content.to_string())?)
            }
            None => None,
        };
        let changed = tx.execute(
            "UPDATE ai_messages SET
                status = ?2,
                error = ?3,
                content = COALESCE(?4, content)
             WHERE id = ?1",
            params![id, status, seal.seal_opt("error", error)?, content],
        )?;
        tx.commit()?;
        Ok(changed > 0)
    }

    /// The newest `limit` transcript entries of the session `session_id` of `owner_did`,
    /// in `seq` order; with `before`, only those older than that `seq`. Empty when the
    /// session is missing or another agent's. An entry whose sealed values do not open is
    /// left out.
    pub fn list_ai_messages(
        &self,
        owner_did: Option<&str>,
        session_id: &str,
        limit: u32,
        before: Option<i64>,
    ) -> Result<Vec<AiSessionMessage>, rusqlite::Error> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {MESSAGE_COLUMNS} FROM (
                SELECT * FROM ai_messages m
                WHERE m.session_id = ?2
                  AND (?3 IS NULL OR m.seq < ?3)
                  AND EXISTS (
                    SELECT 1 FROM ai_sessions s
                    WHERE s.id = m.session_id AND s.owner_did IS ?1
                  )
                ORDER BY m.seq DESC LIMIT ?4
             ) ORDER BY seq"
        ))?;
        let rows = stmt.query_map(
            params![owner_did, session_id, before, limit as i64],
            |row| self.message_from(owner_did, row),
        )?;
        let mut messages = Vec::new();
        for message in rows {
            messages.extend(sealed::readable("ai_messages", message)?);
        }
        Ok(messages)
    }

    /// Every row still marked `running` — of the session `session_id` when one is named,
    /// of the whole table at startup — marked `interrupted`. A row is `running` while its
    /// task is alive; a `running` row that survives the task that wrote it was cut off. A
    /// row without an `error` gets one, sealed for its session's owner.
    pub fn interrupt_running_ai_messages(
        &self,
        session_id: Option<&str>,
    ) -> Result<u64, rusqlite::Error> {
        let tx = self.conn.unchecked_transaction()?;
        let running: Vec<(String, Option<String>, bool)> = tx
            .prepare(&format!(
                "SELECT id, {AI_MESSAGE_OWNER}, error IS NULL FROM ai_messages
                 WHERE status = 'running' AND (?1 IS NULL OR session_id = ?1)"
            ))?
            .query_map(params![session_id], |row| {
                Ok((row.get(0)?, row.get(1)?, row.get(2)?))
            })?
            .collect::<Result<_, _>>()?;
        let mut changed = 0;
        for (id, owner, without_error) in running {
            let error = if without_error {
                Some(
                    self.writer(&AI_MESSAGES, owner.as_deref(), &[&id], None)?
                        .seal("error", INTERRUPTED)?,
                )
            } else {
                None
            };
            changed += tx.execute(
                "UPDATE ai_messages SET status = 'interrupted', error = COALESCE(error, ?2)
                 WHERE id = ?1 AND status = 'running'",
                params![id, error],
            )?;
        }
        tx.commit()?;
        Ok(changed as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::Capability;
    use crate::at_rest::keyring_file::{path_in, KeyOwner, StoreId};
    use crate::at_rest::sqlite::{
        self, database_key, open_keyed, sealed_version, Cell, SealError, SEALED_PREFIX,
    };
    use crate::at_rest::DataKeys;
    use ad4m_keyring::KeyVersion;
    use serde_json::json;
    use std::path::Path;
    use std::sync::Arc;

    const SECRET_TITLE: &str = "secret-session-title";
    const SECRET_PROMPT: &str = "secret-prompt-text";
    const SECRET_REPLY: &str = "secret-model-reply";
    const SECRET_ERROR: &str = "secret-provider-error";
    const SECRET_PERSPECTIVE: &str = "secret-perspective-id";

    fn session(id: &str, owner: Option<&str>) -> AiSession {
        AiSession {
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
            .append_ai_message(owner, id, session, role, "{}", None)
            .unwrap()
            .expect("append succeeds");
        assert_eq!(message.seq, seq_wanted, "seq of {id}");
    }

    fn keyring(folder: &Path, byte: u8) -> Arc<DataKeys> {
        let keys = DataKeys::load(path_in(folder)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([byte; 32])).unwrap();
        Arc::new(keys)
    }

    /// A keyed database in `folder` where alice and bob are managed users.
    fn keyed(folder: &Path) -> (Ad4mDb, Arc<DataKeys>) {
        let keys = keyring(folder, 1);
        let key = database_key(&keys).unwrap();
        let db = Ad4mDb::open(&folder.join("db.sqlite"), Some((&key, keys.clone()))).unwrap();
        db.add_user("alice", "did:key:alice", "pw").unwrap();
        db.add_user("bob", "did:key:bob", "pw").unwrap();
        (db, keys)
    }

    /// `test` on a database without keys, then on a keyed one where alice and bob are
    /// managed users.
    fn with_and_without_keys(test: impl Fn(&Ad4mDb)) {
        test(&Ad4mDb::new(":memory:").unwrap());
        let folder = tempfile::tempdir().unwrap();
        let (db, _keys) = keyed(folder.path());
        assert!(db.seals());
        test(&db);
    }

    fn stored(db: &Ad4mDb, sql: &str, id: &str) -> String {
        db.conn.query_row(sql, [id], |r| r.get(0)).unwrap()
    }

    fn alices() -> StoreId {
        StoreId::Person(KeyOwner::User("alice".into()))
    }

    #[test]
    fn merge_patch_is_json_merge_patch() {
        let mut target = json!({ "text": "", "keep": 1, "drop": 2, "nested": { "a": 1 } });
        merge_patch(
            &mut target,
            &json!({ "text": "hi", "drop": null, "nested": { "b": 2 }, "new": [1] }),
        );
        assert_eq!(
            target,
            json!({ "text": "hi", "keep": 1, "nested": { "a": 1, "b": 2 }, "new": [1] })
        );
        let mut scalar = json!("text");
        merge_patch(&mut scalar, &json!({ "a": 1 }));
        assert_eq!(scalar, json!({ "a": 1 }));
        merge_patch(&mut scalar, &json!(3));
        assert_eq!(scalar, json!(3));
    }

    #[test]
    fn sessions_and_messages_are_scoped_to_their_owner() {
        with_and_without_keys(|db| {
            let alice = Some("did:key:alice");
            let bob = Some("did:key:bob");
            db.create_ai_session(&session("s1", alice)).unwrap();
            db.create_ai_session(&session("s2", bob)).unwrap();
            db.create_ai_session(&session("s3", None)).unwrap();

            assert!(db.get_ai_session(alice, "s1").unwrap().is_some());
            assert!(db.get_ai_session(alice, "s2").unwrap().is_none());
            assert!(db.get_ai_session(bob, "s1").unwrap().is_none());
            assert!(db.get_ai_session(None, "s3").unwrap().is_some());
            assert!(db.get_ai_session(alice, "s3").unwrap().is_none());
            assert_eq!(
                db.get_ai_session_any("s2").unwrap().unwrap().title,
                "session s2"
            );

            assert_eq!(db.list_ai_sessions(alice).unwrap().len(), 1);
            assert_eq!(db.list_ai_sessions(bob).unwrap().len(), 1);
            assert_eq!(db.list_ai_sessions(None).unwrap().len(), 1);

            // A caller that knows another session's id can append nothing to it.
            assert!(db
                .append_ai_message(bob, "m-x", "s1", "user", "{}", None)
                .unwrap()
                .is_none());
            append(db, alice, "m1", "s1", "user", 1);
            append(db, alice, "m2", "s1", "assistant", 2);
            assert_eq!(
                db.list_ai_messages(alice, "s1", 100, None).unwrap().len(),
                2
            );
            assert!(db
                .list_ai_messages(bob, "s1", 100, None)
                .unwrap()
                .is_empty());
            // Nor read one of its messages by id.
            assert!(db.get_ai_message(alice, "m1").unwrap().is_some());
            assert!(db.get_ai_message(bob, "m1").unwrap().is_none());
            assert!(db.get_ai_message(None, "m1").unwrap().is_none());

            assert!(db
                .update_ai_session(
                    alice,
                    "s1",
                    &AiSessionUpdate {
                        title: Some("renamed".into()),
                        ..Default::default()
                    }
                )
                .unwrap());
            assert!(!db
                .update_ai_session(
                    bob,
                    "s1",
                    &AiSessionUpdate {
                        title: Some("stolen".into()),
                        ..Default::default()
                    }
                )
                .unwrap());
            assert_eq!(
                db.get_ai_session(alice, "s1").unwrap().unwrap().title,
                "renamed"
            );

            assert!(!db.delete_ai_session(bob, "s1").unwrap());
            assert!(db.delete_ai_session(alice, "s1").unwrap());
            assert!(db.get_ai_session(alice, "s1").unwrap().is_none());
            assert!(db
                .list_ai_messages(alice, "s1", 100, None)
                .unwrap()
                .is_empty());
        });
    }

    #[test]
    fn a_session_update_clears_nullable_fields_and_keeps_the_rest() {
        with_and_without_keys(|db| {
            let alice = Some("did:key:alice");
            let mut s = session("s1", alice);
            s.model = Some("model-a".into());
            s.default_perspective = Some("p1".into());
            s.library_perspective = Some("lib".into());
            db.create_ai_session(&s).unwrap();

            db.update_ai_session(
                alice,
                "s1",
                &AiSessionUpdate {
                    model: Some(None),
                    default_perspective: Some(Some("p2".into())),
                    library_perspective: Some(None),
                    ..Default::default()
                },
            )
            .unwrap();
            let s = db.get_ai_session(alice, "s1").unwrap().unwrap();
            assert_eq!(s.model, None);
            assert_eq!(s.default_perspective.as_deref(), Some("p2"));
            assert_eq!(s.library_perspective, None);
            assert_eq!(s.capabilities.len(), 1, "unsent fields are kept");
            assert_eq!(s.title, "session s1", "unsent fields are kept");
        });
    }

    #[test]
    fn finish_writes_the_end_state_and_the_sweep_marks_what_is_left_running() {
        with_and_without_keys(|db| {
            let alice = Some("did:key:alice");
            db.create_ai_session(&session("s1", alice)).unwrap();
            db.create_ai_session(&session("s2", None)).unwrap();
            append(db, alice, "u1", "s1", "user", 1);
            db.append_ai_message(
                alice,
                "a1",
                "s1",
                "assistant",
                r#"{"text":"","keep":true}"#,
                Some("running"),
            )
            .unwrap();
            db.append_ai_message(alice, "t1", "s1", "tool", "{}", Some("running"))
                .unwrap();
            db.append_ai_message(None, "t2", "s2", "tool", "{}", Some("running"))
                .unwrap();

            assert!(db
                .finish_ai_message(
                    alice,
                    "a1",
                    "done",
                    None,
                    Some(&json!({ "text": "hi", "keep": null }))
                )
                .unwrap());
            let a1 = db.get_ai_message(alice, "a1").unwrap().unwrap();
            assert_eq!(a1.status, Some(AiTurnStatus::Done));
            assert_eq!(a1.content, json!({ "text": "hi" }));
            // Bob cannot finish Alice's row.
            assert!(!db
                .finish_ai_message(Some("did:key:bob"), "t1", "done", None, None)
                .unwrap());
            // Without a patch the content stays.
            assert!(db
                .finish_ai_message(alice, "u1", "failed", Some("it broke"), None)
                .unwrap());
            let u1 = db.get_ai_message(alice, "u1").unwrap().unwrap();
            assert_eq!(u1.content, json!({}));
            assert_eq!(u1.error.as_deref(), Some("it broke"));

            // One session's sweep leaves the other's running row.
            assert_eq!(db.interrupt_running_ai_messages(Some("s1")).unwrap(), 1);
            let t1 = db.get_ai_message(alice, "t1").unwrap().unwrap();
            assert_eq!(t1.status, Some(AiTurnStatus::Interrupted));
            assert_eq!(t1.error.as_deref(), Some(INTERRUPTED));
            assert_eq!(db.interrupt_running_ai_messages(None).unwrap(), 1);
            let t2 = db.get_ai_message(None, "t2").unwrap().unwrap();
            assert_eq!(t2.status, Some(AiTurnStatus::Interrupted));
            assert_eq!(t2.error.as_deref(), Some(INTERRUPTED));
            assert_eq!(db.interrupt_running_ai_messages(None).unwrap(), 0);
        });
    }

    /// A conversation in a keyed database: every content column is sealed under its owner's
    /// store (a message's owner is its session's), reads back through the database, does not
    /// open under another person's store, and is not readable with the database key alone.
    #[test]
    fn a_conversation_is_sealed_under_its_owners_store() {
        let folder = tempfile::tempdir().unwrap();
        let (db, keys) = keyed(folder.path());
        let alice = Some("did:key:alice");
        let mut s = session("s1", alice);
        s.title = SECRET_TITLE.into();
        s.default_perspective = Some(SECRET_PERSPECTIVE.into());
        s.library_perspective = Some(SECRET_PERSPECTIVE.into());
        db.create_ai_session(&s).unwrap();
        db.create_ai_session(&session("s-main", None)).unwrap();
        db.append_ai_message(
            alice,
            "m1",
            "s1",
            "user",
            &json!({ "text": SECRET_PROMPT }).to_string(),
            None,
        )
        .unwrap()
        .unwrap();
        db.append_ai_message(alice, "m2", "s1", "assistant", "{}", Some("running"))
            .unwrap()
            .unwrap();
        db.finish_ai_message(
            alice,
            "m2",
            "failed",
            Some(SECRET_ERROR),
            Some(&json!({ "text": SECRET_REPLY })),
        )
        .unwrap();
        db.append_ai_message(None, "m3", "s-main", "user", "{}", None)
            .unwrap()
            .unwrap();

        // Every sealed column holds a sealed value.
        for table in [&AI_SESSIONS, &AI_MESSAGES] {
            for column in table.columns {
                let values: Vec<Option<String>> = db
                    .conn
                    .prepare(&format!("SELECT {column} FROM {}", table.table))
                    .unwrap()
                    .query_map([], |r| r.get(0))
                    .unwrap()
                    .collect::<Result<_, _>>()
                    .unwrap();
                assert!(
                    values.iter().flatten().count() > 0,
                    "{}.{column}",
                    table.table
                );
                for value in values.into_iter().flatten() {
                    assert!(
                        value.starts_with(SEALED_PREFIX),
                        "{}.{column} is not sealed: {value}",
                        table.table
                    );
                }
            }
        }

        // It reads back for its owner.
        let read = db.get_ai_session(alice, "s1").unwrap().unwrap();
        assert_eq!(read.title, SECRET_TITLE);
        assert_eq!(
            read.default_perspective.as_deref(),
            Some(SECRET_PERSPECTIVE)
        );
        assert_eq!(read.capabilities.len(), 1);
        let messages = db.list_ai_messages(alice, "s1", 10, None).unwrap();
        assert_eq!(messages[0].content["text"], SECRET_PROMPT);
        assert_eq!(messages[1].content["text"], SECRET_REPLY);
        assert_eq!(messages[1].error.as_deref(), Some(SECRET_ERROR));

        // Alice's values open under alice's store and not under the main agent's; the main
        // agent's session is under the main agent's.
        let title = stored(&db, "SELECT title FROM ai_sessions WHERE id = ?1", "s1");
        let content = stored(&db, "SELECT content FROM ai_messages WHERE id = ?1", "m1");
        let title_cell = Cell {
            table: "ai_sessions",
            column: "title",
            row: "s1",
        };
        let content_cell = Cell {
            table: "ai_messages",
            column: "content",
            row: "m1",
        };
        let mains = StoreId::Person(KeyOwner::Main);
        assert_eq!(
            sqlite::open_value(&keys, &alices(), &title_cell, &title).unwrap(),
            SECRET_TITLE
        );
        assert!(
            sqlite::open_value(&keys, &alices(), &content_cell, &content)
                .unwrap()
                .contains(SECRET_PROMPT)
        );
        for (cell, value) in [(&title_cell, &title), (&content_cell, &content)] {
            assert!(matches!(
                sqlite::open_value(&keys, &mains, cell, value),
                Err(SealError::DoesNotOpen { .. })
            ));
        }
        let main_content = stored(&db, "SELECT content FROM ai_messages WHERE id = ?1", "m3");
        let main_cell = Cell {
            table: "ai_messages",
            column: "content",
            row: "m3",
        };
        assert_eq!(
            sqlite::open_value(&keys, &mains, &main_cell, &main_content).unwrap(),
            "{}"
        );
        assert!(sqlite::open_value(&keys, &alices(), &main_cell, &main_content).is_err());

        // The database key alone (another keyring holding it, without the persons' store
        // keys) opens the file, not the values.
        let key = database_key(&keys).unwrap();
        drop(db);
        let conn = open_keyed(&folder.path().join("db.sqlite"), &key).unwrap();
        let other = tempfile::tempdir().unwrap();
        let without_stores = keyring(other.path(), 2);
        let rows: Vec<String> = conn
            .prepare(
                "SELECT title FROM ai_sessions UNION ALL SELECT content FROM ai_messages
                 UNION ALL SELECT error FROM ai_messages WHERE error IS NOT NULL",
            )
            .unwrap()
            .query_map([], |r| r.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        for value in &rows {
            for secret in [SECRET_TITLE, SECRET_PROMPT, SECRET_REPLY, SECRET_ERROR] {
                assert!(!value.contains(secret), "{value} shows {secret}");
            }
        }
        assert!(sqlite::open_value(&without_stores, &alices(), &title_cell, &title).is_err());
        let bytes = std::fs::read(folder.path().join("db.sqlite")).unwrap();
        for secret in [SECRET_TITLE, SECRET_PROMPT, SECRET_REPLY, SECRET_ERROR] {
            assert!(
                !bytes.windows(secret.len()).any(|w| w == secret.as_bytes()),
                "{secret} is in the file"
            );
        }
    }

    /// A sealed value moved into another row (another person's session, another message)
    /// does not open there: that row is left out of reads and listings, the others stay.
    #[test]
    fn a_value_moved_into_another_row_is_left_out() {
        let folder = tempfile::tempdir().unwrap();
        let (db, _keys) = keyed(folder.path());
        let alice = Some("did:key:alice");
        db.create_ai_session(&session("s1", alice)).unwrap();
        db.create_ai_session(&session("s2", alice)).unwrap();
        db.create_ai_session(&session("s3", None)).unwrap();
        append(&db, alice, "m1", "s1", "user", 1);
        append(&db, alice, "m2", "s1", "assistant", 2);
        append(&db, None, "m3", "s3", "user", 1);

        // Alice's title copied into her other session's row.
        let title = stored(&db, "SELECT title FROM ai_sessions WHERE id = ?1", "s1");
        db.conn
            .execute(
                "UPDATE ai_sessions SET title = ?1 WHERE id = 's2'",
                params![title],
            )
            .unwrap();
        assert!(db.get_ai_session(alice, "s2").unwrap().is_none());
        assert!(db.get_ai_session_any("s2").unwrap().is_none());
        let listed = db.list_ai_sessions(alice).unwrap();
        assert_eq!(listed.len(), 1);
        assert_eq!(listed[0].id, "s1");

        // Alice's message content copied into the main agent's message.
        let content = stored(&db, "SELECT content FROM ai_messages WHERE id = ?1", "m1");
        db.conn
            .execute(
                "UPDATE ai_messages SET content = ?1 WHERE id = 'm3'",
                params![content],
            )
            .unwrap();
        assert!(db.get_ai_message(None, "m3").unwrap().is_none());
        assert!(db
            .list_ai_messages(None, "s3", 10, None)
            .unwrap()
            .is_empty());
        // Or into her own next message: still another row.
        db.conn
            .execute(
                "UPDATE ai_messages SET content = ?1 WHERE id = 'm2'",
                params![content],
            )
            .unwrap();
        let left = db.list_ai_messages(alice, "s1", 10, None).unwrap();
        assert_eq!(left.len(), 1);
        assert_eq!(left[0].id, "m1");

        // A value in the clear in a keyed database is refused too.
        db.conn
            .execute("UPDATE ai_messages SET content = '{}' WHERE id = 'm1'", [])
            .unwrap();
        assert!(db
            .list_ai_messages(alice, "s1", 10, None)
            .unwrap()
            .is_empty());
    }

    /// Rows written in the clear (before the tables were sealed, or by an import) are sealed
    /// by `seal_plain_values`, each under its owner's store, and read back.
    #[test]
    fn plaintext_conversations_are_sealed_by_the_migration() {
        let folder = tempfile::tempdir().unwrap();
        let (db, keys) = keyed(folder.path());
        db.conn
            .execute_batch(&format!(
                "INSERT INTO ai_sessions (id, owner_did, title, model, default_perspective,
                    library_perspective, capabilities, created_at, updated_at)
                 VALUES ('s1', 'did:key:alice', '{SECRET_TITLE}', 'model', 'p1', NULL, '[]', 't', 't'),
                        ('s2', NULL, 'main title', NULL, NULL, NULL, '[]', 't', 't');
                 INSERT INTO ai_messages (id, session_id, seq, role, content, status, error, created_at)
                 VALUES ('m1', 's1', 1, 'user', '{{\"text\":\"{SECRET_PROMPT}\"}}', NULL, NULL, 't'),
                        ('m2', 's1', 2, 'assistant', '{{}}', 'failed', '{SECRET_ERROR}', 't'),
                        ('m3', 's2', 1, 'user', '{{}}', NULL, NULL, 't');"
            ))
            .unwrap();

        // s1: title, default_perspective, capabilities; s2: title, capabilities; m1: content;
        // m2: content, error; m3: content.
        assert_eq!(db.seal_plain_values().unwrap(), 9);
        assert_eq!(db.seal_plain_values().unwrap(), 0, "sealed once");

        let title = stored(&db, "SELECT title FROM ai_sessions WHERE id = ?1", "s1");
        assert!(title.starts_with(SEALED_PREFIX));
        let cell = Cell {
            table: "ai_sessions",
            column: "title",
            row: "s1",
        };
        assert_eq!(
            sqlite::open_value(&keys, &alices(), &cell, &title).unwrap(),
            SECRET_TITLE
        );
        let error = stored(&db, "SELECT error FROM ai_messages WHERE id = ?1", "m2");
        let cell = Cell {
            table: "ai_messages",
            column: "error",
            row: "m2",
        };
        assert_eq!(
            sqlite::open_value(&keys, &alices(), &cell, &error).unwrap(),
            SECRET_ERROR
        );

        let alice = Some("did:key:alice");
        let s1 = db.get_ai_session(alice, "s1").unwrap().unwrap();
        assert_eq!(s1.title, SECRET_TITLE);
        assert_eq!(s1.default_perspective.as_deref(), Some("p1"));
        assert_eq!(
            s1.model.as_deref(),
            Some("model"),
            "model stays in the clear"
        );
        let messages = db.list_ai_messages(alice, "s1", 10, None).unwrap();
        assert_eq!(messages[0].content["text"], SECRET_PROMPT);
        assert_eq!(messages[1].error.as_deref(), Some(SECRET_ERROR));
        assert_eq!(
            db.get_ai_session(None, "s2").unwrap().unwrap().title,
            "main title"
        );
        assert_eq!(db.list_ai_messages(None, "s2", 10, None).unwrap().len(), 1);
    }

    /// A rotation of alice's store key reseals her conversation under the new version and
    /// leaves the main agent's as it was.
    #[test]
    fn a_rotation_reseals_a_persons_conversation() {
        let folder = tempfile::tempdir().unwrap();
        let (db, keys) = keyed(folder.path());
        let alice = Some("did:key:alice");
        db.create_ai_session(&session("s1", alice)).unwrap();
        db.create_ai_session(&session("s2", None)).unwrap();
        db.append_ai_message(alice, "m1", "s1", "user", r#"{"text":"hi"}"#, None)
            .unwrap();
        db.append_ai_message(alice, "m2", "s1", "assistant", "{}", Some("running"))
            .unwrap();
        db.finish_ai_message(alice, "m2", "failed", Some("why"), None)
            .unwrap();
        append(&db, None, "m3", "s2", "user", 1);

        let (version, _) = keys.begin_store_rotation(&alices()).unwrap();
        assert_eq!(version, KeyVersion(2));
        assert_eq!(
            db.count_store_values_not_under(&alices(), version).unwrap(),
            5
        );
        let report = db.reseal_store_values(&alices()).unwrap();
        // s1: title, capabilities; m1: content; m2: content, error.
        assert_eq!((report.resealed, report.unopenable), (5, 0));
        assert_eq!(
            db.count_store_values_not_under(&alices(), version).unwrap(),
            0
        );

        let version_of = |sql: &str, id: &str| sealed_version(&stored(&db, sql, id)).unwrap();
        assert_eq!(
            version_of("SELECT title FROM ai_sessions WHERE id = ?1", "s1"),
            KeyVersion(2)
        );
        assert_eq!(
            version_of("SELECT error FROM ai_messages WHERE id = ?1", "m2"),
            KeyVersion(2)
        );
        assert_eq!(
            version_of("SELECT title FROM ai_sessions WHERE id = ?1", "s2"),
            KeyVersion(1)
        );
        assert_eq!(
            version_of("SELECT content FROM ai_messages WHERE id = ?1", "m3"),
            KeyVersion(1)
        );
        let messages = db.list_ai_messages(alice, "s1", 10, None).unwrap();
        assert_eq!(messages[0].content["text"], "hi");
        assert_eq!(messages[1].error.as_deref(), Some("why"));
        assert_eq!(db.list_ai_messages(None, "s2", 10, None).unwrap().len(), 1);
    }

    /// Removing a managed user deletes their conversations (sessions and, through them,
    /// messages) with the rest of their person's rows; the main agent's stay.
    #[test]
    fn a_persons_conversations_go_with_their_rows() {
        let folder = tempfile::tempdir().unwrap();
        let (db, _keys) = keyed(folder.path());
        let alice = Some("did:key:alice");
        db.create_ai_session(&session("s1", alice)).unwrap();
        db.create_ai_session(&session("s2", None)).unwrap();
        append(&db, alice, "m1", "s1", "user", 1);
        append(&db, alice, "m2", "s1", "user", 2);
        append(&db, None, "m3", "s2", "user", 1);

        assert_eq!(db.delete_person_rows("did:key:alice").unwrap(), 3);
        let count = |table: &str| -> i64 {
            db.conn
                .query_row(&format!("SELECT count(*) FROM {table}"), [], |r| r.get(0))
                .unwrap()
        };
        assert_eq!((count("ai_sessions"), count("ai_messages")), (1, 1));
        assert_eq!(db.list_ai_messages(None, "s2", 10, None).unwrap().len(), 1);
    }
}
