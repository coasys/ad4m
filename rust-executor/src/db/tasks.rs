//! Owner-scoped reads of the `tasks` table.
//!
//! `tasks.agent_did` names the managed agent whose session created a task (D16: an agent is
//! its DID). `NULL` marks a task of the main agent: one created by the admin credential, by
//! an app token on the main agent, by the executor itself (the interpretation tasks), or
//! before the column existed. `Ad4mDb::migrate_task_owner_to_agent_did` moves a table that
//! named its owner by `user_email`.

use rusqlite::{params, OptionalExtension};

use super::Ad4mDb;
use crate::types::{AIPromptExamples, AITask};

impl Ad4mDb {
    /// Owner of task `id`, by agent DID: `None` when no such task exists, `Some(None)` for a
    /// task of the main agent.
    pub fn get_task_owner(&self, id: &str) -> Result<Option<Option<String>>, rusqlite::Error> {
        self.conn
            .query_row(
                "SELECT agent_did FROM tasks WHERE id = ?1",
                params![id],
                |row| row.get(0),
            )
            .optional()
    }

    /// Tasks owned by the managed agent `agent_did`, or the main agent's when it is `None`.
    pub fn get_tasks_owned_by(
        &self,
        agent_did: Option<&str>,
    ) -> Result<Vec<AITask>, rusqlite::Error> {
        // `IS` also matches NULL to NULL, which `=` never does.
        let mut stmt = self.conn.prepare(
            "SELECT id, name, model_id, system_prompt, prompt_examples, metadata, created_at, updated_at
             FROM tasks WHERE agent_did IS ?1",
        )?;
        let tasks = stmt.query_map(params![agent_did], |row| {
            let prompt_examples: Vec<AIPromptExamples> =
                serde_json::from_str(&row.get::<_, String>(4)?).map_err(|e| {
                    rusqlite::Error::FromSqlConversionFailure(
                        4,
                        rusqlite::types::Type::Text,
                        Box::new(e),
                    )
                })?;
            Ok(AITask {
                task_id: row.get(0)?,
                name: row.get(1)?,
                model_id: row.get(2)?,
                system_prompt: row.get(3)?,
                prompt_examples,
                meta_data: row.get(5)?,
                created_at: row.get(6)?,
                updated_at: row.get(7)?,
            })
        })?;
        tasks.collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusqlite::Connection;

    fn add(db: &Ad4mDb, name: &str, owner: Option<&str>) -> String {
        db.add_task(
            name.to_string(),
            "model".to_string(),
            format!("system prompt of {name}"),
            vec![],
            None,
            owner,
        )
        .unwrap()
    }

    fn names(tasks: Vec<AITask>) -> Vec<String> {
        tasks.into_iter().map(|t| t.name).collect()
    }

    #[test]
    fn tasks_are_listed_and_owned_per_user() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let alice = add(&db, "alice's", Some("did:key:alice"));
        add(&db, "bob's", Some("did:key:bob"));
        let operator = add(&db, "operator's", None);

        assert_eq!(
            names(db.get_tasks_owned_by(Some("did:key:alice")).unwrap()),
            vec!["alice's"]
        );
        assert_eq!(
            names(db.get_tasks_owned_by(None).unwrap()),
            vec!["operator's"]
        );
        assert!(db
            .get_tasks_owned_by(Some("did:key:nobody"))
            .unwrap()
            .is_empty());

        assert_eq!(
            db.get_task_owner(&alice).unwrap(),
            Some(Some("did:key:alice".to_string()))
        );
        assert_eq!(db.get_task_owner(&operator).unwrap(), Some(None));
        assert_eq!(db.get_task_owner("no-such-task").unwrap(), None);

        // Rewriting a task must not move it to another owner.
        db.update_task(
            alice.clone(),
            "alice's".to_string(),
            "model".to_string(),
            "rewritten".to_string(),
            vec![],
            None,
        )
        .unwrap();
        assert_eq!(
            db.get_task_owner(&alice).unwrap(),
            Some(Some("did:key:alice".to_string()))
        );
    }

    /// A database written before tasks had owners gains the column on open, and every task
    /// already in it becomes an operator task: invisible to user sessions, still there for
    /// operators and the executor.
    #[test]
    fn opening_a_pre_owner_database_adds_the_column_and_keeps_old_tasks_for_operators() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("ad4m.db");
        let path = path.to_str().unwrap();

        {
            let legacy = Connection::open(path).unwrap();
            legacy
                .execute(
                    "CREATE TABLE tasks (
                        id TEXT PRIMARY KEY,
                        name TEXT NOT NULL,
                        model_id TEXT NOT NULL,
                        system_prompt TEXT NOT NULL,
                        prompt_examples TEXT NOT NULL,
                        metadata TEXT NULL,
                        created_at TEXT NOT NULL,
                        updated_at TEXT NOT NULL
                     )",
                    [],
                )
                .unwrap();
            legacy
                .execute(
                    "INSERT INTO tasks VALUES ('legacy-task', 'legacy', 'model', 'old prompt', '[]', NULL, 'then', 'then')",
                    [],
                )
                .unwrap();
        }

        let db = Ad4mDb::new(path).unwrap();
        let columns: Vec<String> = db
            .conn
            .prepare("SELECT name FROM pragma_table_info('tasks')")
            .unwrap()
            .query_map([], |row| row.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert_eq!(columns.last().map(String::as_str), Some("agent_did"));

        assert_eq!(db.get_task_owner("legacy-task").unwrap(), Some(None));
        assert_eq!(names(db.get_tasks_owned_by(None).unwrap()), vec!["legacy"]);
        assert!(db
            .get_tasks_owned_by(Some("did:key:alice"))
            .unwrap()
            .is_empty());
        let legacy = db.get_task("legacy-task".to_string()).unwrap().unwrap();
        assert_eq!(legacy.system_prompt, "old prompt");

        let owned = add(&db, "alice's", Some("did:key:alice"));
        assert_eq!(
            db.get_task_owner(&owned).unwrap(),
            Some(Some("did:key:alice".to_string()))
        );
        drop(db);

        // The migration runs on every start; the second run must be a no-op.
        let reopened = Ad4mDb::new(path).unwrap();
        assert_eq!(reopened.get_tasks().unwrap().len(), 2);
    }

    /// A database whose tasks named their owner by `user_email` (PR 5 before D16) moves to
    /// `agent_did`: each managed user keeps its own task and reaches no other's, the main
    /// agent's tasks keep `NULL`, and a task whose user is gone is deleted (it was that
    /// user's and nobody could reach it).
    #[test]
    fn tasks_keyed_by_email_move_to_the_owners_agent_did() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("ad4m.db");
        let path = path.to_str().unwrap();

        {
            let legacy = Connection::open(path).unwrap();
            legacy
                .execute_batch(
                    "CREATE TABLE tasks (
                        id TEXT PRIMARY KEY,
                        name TEXT NOT NULL,
                        model_id TEXT NOT NULL,
                        system_prompt TEXT NOT NULL,
                        prompt_examples TEXT NOT NULL,
                        metadata TEXT NULL,
                        created_at TEXT NOT NULL,
                        updated_at TEXT NOT NULL,
                        user_email TEXT
                     );
                     CREATE TABLE users (
                        username TEXT PRIMARY KEY,
                        did TEXT NOT NULL,
                        password_hash TEXT NOT NULL,
                        last_seen INTEGER
                     );
                     INSERT INTO users (username, did, password_hash) VALUES ('alice@example.org', 'did:key:alice', 'h');
                     INSERT INTO users (username, did, password_hash) VALUES ('bob@example.org', 'did:key:bob', 'h');
                     INSERT INTO users (username, did, password_hash) VALUES ('nodid@example.org', '', 'h');
                     INSERT INTO tasks VALUES ('alice-task', 'alice''s', 'model', 'p', '[]', NULL, 'then', 'then', 'alice@example.org');
                     INSERT INTO tasks VALUES ('bob-task', 'bob''s', 'model', 'p', '[]', NULL, 'then', 'then', 'bob@example.org');
                     INSERT INTO tasks VALUES ('main-task', 'main''s', 'model', 'p', '[]', NULL, 'then', 'then', NULL);
                     INSERT INTO tasks VALUES ('gone-task', 'gone''s', 'model', 'p', '[]', NULL, 'then', 'then', 'gone@example.org');
                     INSERT INTO tasks VALUES ('nodid-task', 'nodid''s', 'model', 'p', '[]', NULL, 'then', 'then', 'nodid@example.org');",
                )
                .unwrap();
        }

        let db = Ad4mDb::new(path).unwrap();
        let columns: Vec<String> = db
            .conn
            .prepare("SELECT name FROM pragma_table_info('tasks')")
            .unwrap()
            .query_map([], |row| row.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert!(!columns.iter().any(|c| c == "user_email"), "{columns:?}");
        assert_eq!(columns.last().map(String::as_str), Some("agent_did"));

        assert_eq!(
            names(db.get_tasks_owned_by(Some("did:key:alice")).unwrap()),
            vec!["alice's"]
        );
        assert_eq!(
            names(db.get_tasks_owned_by(Some("did:key:bob")).unwrap()),
            vec!["bob's"]
        );
        assert_eq!(
            db.get_task_owner("alice-task").unwrap(),
            Some(Some("did:key:alice".to_string()))
        );
        assert_ne!(
            db.get_task_owner("bob-task").unwrap(),
            Some(Some("did:key:alice".to_string())),
            "alice must not own bob's task"
        );
        assert_eq!(names(db.get_tasks_owned_by(None).unwrap()), vec!["main's"]);
        assert_eq!(db.get_task_owner("main-task").unwrap(), Some(None));
        assert_eq!(db.get_task_owner("gone-task").unwrap(), None);
        assert_eq!(db.get_task_owner("nodid-task").unwrap(), None);
        assert_eq!(db.get_tasks().unwrap().len(), 3);
        drop(db);

        // The migration runs on every start; the second run must be a no-op.
        let reopened = Ad4mDb::new(path).unwrap();
        assert_eq!(reopened.get_tasks().unwrap().len(), 3);
        assert_eq!(
            reopened.get_task_owner("alice-task").unwrap(),
            Some(Some("did:key:alice".to_string()))
        );
    }
}
