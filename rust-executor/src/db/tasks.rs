//! Owner-scoped reads of the `tasks` table.
//!
//! `tasks.agent_did` names the managed agent whose session created a task (D16: an agent is
//! its DID). `NULL` marks a task of the main agent: one created by the admin credential, by
//! an app token on the main agent, or by the executor itself (the interpretation tasks).

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
}
