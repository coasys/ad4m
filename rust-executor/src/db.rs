use crate::graphql::graphql_types::{
    AIModelLoadingStatus, EntanglementProof, LinkStatus, ModelInput, NotificationInput,
    PerspectiveExpression, PerspectiveHandle, SentMessage,
};
use crate::types::{
    AIPromptExamples, AITask, Expression, ExpressionProof, Link, LinkExpression, LocalModel, Model,
    ModelApi, ModelApiType, ModelType, Notification, PerspectiveDiff,
};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use rusqlite::{params, Connection, OptionalExtension};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::str::FromStr;
use url::Url;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
struct LinkSchema {
    perspective: String,
    link_expression: JsonValue,
    source: String,
    predicate: String,
    target: String,
    author: String,
    timestamp: String,
    status: String,
}

#[derive(Serialize, Deserialize)]
struct ExpressionSchema {
    url: String,
    data: JsonValue,
}

pub type Ad4mDbResult<T> = Result<T, AnyError>;

use std::sync::{Arc, Mutex};

lazy_static! {
    static ref AD4M_DB_INSTANCE: Arc<Mutex<Option<Ad4mDb>>> = Arc::new(Mutex::new(None));
}

pub struct Ad4mDb {
    conn: Connection,
}

impl Ad4mDb {
    pub fn init_global_instance(db_path: &str) -> Ad4mDbResult<()> {
        let mut db_instance = AD4M_DB_INSTANCE.lock().unwrap();
        *db_instance = Some(Ad4mDb::new(db_path)?);
        Ok(())
    }

    pub fn global_instance() -> Arc<Mutex<Option<Ad4mDb>>> {
        AD4M_DB_INSTANCE.clone()
    }

    fn new(db_path: &str) -> Ad4mDbResult<Self> {
        let conn = Connection::open(db_path)?;

        // Create tables if they don't exist

        conn.execute(
            "CREATE TABLE IF NOT EXISTS perspective_handle (
                uuid TEXT PRIMARY KEY,
                name TEXT,
                neighbourhood TEXT,
                shared_url TEXT,
                state TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS link (
                id INTEGER PRIMARY KEY,
                perspective TEXT NOT NULL,
                source TEXT NOT NULL,
                predicate TEXT NOT NULL,
                target TEXT NOT NULL,
                author TEXT NOT NULL,
                timestamp TEXT NOT NULL,
                signature TEXT NOT NULL,
                key TEXT NOT NULL,
                status TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS expression (
                id INTEGER PRIMARY KEY,
                url TEXT NOT NULL UNIQUE,
                data TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS perspective_diff (
                id INTEGER PRIMARY KEY,
                perspective TEXT NOT NULL,
                additions TEXT NOT NULL,
                removals TEXT NOT NULL,
                is_pending BOOLEAN NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS trusted_agent (
                id INTEGER PRIMARY KEY,
                agent TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS known_link_languages (
                id INTEGER PRIMARY KEY,
                language TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS friends (
                id INTEGER PRIMARY KEY,
                friend TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS outbox (
                id INTEGER PRIMARY KEY,
                message TEXT NOT NULL,
                recipient TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS entanglement_proof (
                id INTEGER PRIMARY KEY,
                proof TEXT NOT NULL
             )",
            [],
        )?;

        // Start Generation Here
        conn.execute(
            "CREATE TABLE IF NOT EXISTS notifications (
                id TEXT PRIMARY KEY,
                granted BOOLEAN NOT NULL,
                description TEXT NOT NULL,
                appName TEXT NOT NULL,
                appUrl TEXT NOT NULL,
                appIconPath TEXT,
                trigger TEXT NOT NULL,
                perspective_ids TEXT NOT NULL,
                webhookUrl TEXT NOT NULL,
                webhookAuth TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS tasks (
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
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS models (
                id TEXT PRIMARY KEY,
                name TEXT NOT NULL,
                api_base_url TEXT,
                api_key TEXT,
                api_type TEXT,
                local_file_name TEXT,
                local_tokenizer_source TEXT,
                local_model_parameters TEXT,
                type TEXT NOT NULL
            )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS model_status (
                model TEXT PRIMARY KEY,
                progress DOUBLE NOT NULL,
                status TEXT NOT NULL,
                downloaded BOOLEAN NOT NULL,
                loaded BOOLEAN NOT NULL
            )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS default_models (
                model_type TEXT PRIMARY KEY,
                model_id TEXT NOT NULL
            )",
            [],
        )?;

        Ok(Self { conn })
    }

    pub fn create_or_update_model_status(
        &self,
        model: &str,
        progress: f64,
        status: &str,
        downloaded: bool,
        loaded: bool,
    ) -> Result<(), rusqlite::Error> {
        let conn = &self.conn;
        conn.execute(
            "INSERT INTO model_status (model, progress, status, downloaded, loaded)
             VALUES (?1, ?2, ?3, ?4, ?5)
             ON CONFLICT(model) DO UPDATE SET
             progress = excluded.progress,
             status = excluded.status,
             downloaded = excluded.downloaded,
             loaded = excluded.loaded",
            params![model, progress, status, downloaded, loaded],
        )?;
        Ok(())
    }

    pub fn get_model_status(
        &self,
        model: &str,
    ) -> Result<Option<AIModelLoadingStatus>, rusqlite::Error> {
        let conn = &self.conn;
        let mut stmt = conn.prepare(
            "SELECT model, progress, status, downloaded, loaded FROM model_status WHERE model = ?1",
        )?;
        let mut rows = stmt.query(params![model])?;

        if let Some(row) = rows.next()? {
            let model = AIModelLoadingStatus {
                model: row.get(0)?,
                progress: row.get(1)?,
                status: row.get(2)?,
                downloaded: row.get(3)?,
                loaded: row.get(4)?,
            };
            Ok(Some(model))
        } else {
            Ok(None)
        }
    }

    pub fn add_task(
        &self,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Result<String, rusqlite::Error> {
        let created_at = chrono::Utc::now().to_string();
        let updated_at = created_at.clone();
        let id = uuid::Uuid::new_v4().to_string();
        self.conn.execute(
            "INSERT INTO tasks (id, name, model_id, system_prompt, prompt_examples, metadata, created_at, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![id, name, model_id, system_prompt, serde_json::to_string(&prompt_examples).unwrap(), metadata, created_at, updated_at],
        )?;
        Ok(id)
    }

    pub fn remove_task(&self, id: String) -> Result<(), rusqlite::Error> {
        self.conn.execute("DELETE FROM tasks WHERE id = ?", [id])?;
        Ok(())
    }

    pub fn get_task(&self, id: String) -> Result<Option<AITask>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT * FROM tasks WHERE id = ?")?;
        let mut rows = stmt.query(params![id])?;

        if let Some(row) = rows.next()? {
            let prompt_examples: Vec<AIPromptExamples> =
                serde_json::from_str(&row.get::<_, String>(4)?).unwrap();
            Ok(Some(AITask {
                task_id: row.get(0)?,
                name: row.get(1)?,
                model_id: row.get(2)?,
                system_prompt: row.get(3)?,
                prompt_examples,
                meta_data: row.get(5)?,
                created_at: row.get(6)?,
                updated_at: row.get(7)?,
            }))
        } else {
            Ok(None)
        }
    }

    pub fn get_tasks(&self) -> Result<Vec<AITask>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT * FROM tasks")?;
        let task_iter = stmt.query_map([], |row| {
            let prompt_examples: Vec<AIPromptExamples> =
                serde_json::from_str(&row.get::<_, String>(4)?).unwrap();
            let task = AITask {
                task_id: row.get(0)?,
                name: row.get(1)?,
                model_id: row.get(2)?,
                system_prompt: row.get(3)?,
                prompt_examples,
                meta_data: row.get(5)?,
                created_at: row.get(6)?,
                updated_at: row.get(7)?,
            };
            Ok(task)
        })?;

        let mut tasks = Vec::new();
        for task in task_iter {
            tasks.push(task?);
        }
        Ok(tasks)
    }

    pub fn update_task(
        &self,
        id: String,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Result<bool, rusqlite::Error> {
        let updated_at = chrono::Utc::now().to_string();

        let result = self.conn.execute(
            "UPDATE tasks SET name = ?2, model_id = ?3, system_prompt = ?4, prompt_examples = ?5, metadata = ?6, updated_at = ?7 WHERE id = ?1",
            params![id, name, model_id, system_prompt, serde_json::to_string(&prompt_examples).unwrap(), metadata, updated_at],
        )?;
        Ok(result > 0)
    }

    pub fn add_notification(
        &self,
        notification: NotificationInput,
    ) -> Result<String, rusqlite::Error> {
        let id = uuid::Uuid::new_v4().to_string();
        self.conn.execute(
            "INSERT INTO notifications (id, granted, description, appName, appUrl, appIconPath, trigger, perspective_ids, webhookUrl, webhookAuth) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
            params![
                id,
                false,
                notification.description,
                notification.app_name,
                notification.app_url,
                notification.app_icon_path,
                notification.trigger,
                serde_json::to_string(&notification.perspective_ids).unwrap(),
                notification.webhook_url,
                notification.webhook_auth,
            ],
        )?;
        Ok(id)
    }

    pub fn get_notifications(&self) -> Result<Vec<Notification>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT * FROM notifications")?;
        let notification_iter = stmt.query_map([], |row| {
            Ok(Notification {
                id: row.get(0)?,
                granted: row.get(1)?,
                description: row.get(2)?,
                app_name: row.get(3)?,
                app_url: row.get(4)?,
                app_icon_path: row.get(5)?,
                trigger: row.get(6)?,
                perspective_ids: serde_json::from_str(&row.get::<_, String>(7)?).unwrap(),
                webhook_url: row.get(8)?,
                webhook_auth: row.get(9)?,
            })
        })?;

        let mut notifications = Vec::new();
        for notification in notification_iter {
            notifications.push(notification?);
        }
        Ok(notifications)
    }

    pub fn get_notification(&self, id: String) -> Result<Option<Notification>, rusqlite::Error> {
        let mut stmt = self
            .conn
            .prepare("SELECT * FROM notifications WHERE id = ?")?;
        let mut rows = stmt.query(params![id])?;

        if let Some(row) = rows.next()? {
            Ok(Some(Notification {
                id: row.get(0)?,
                granted: row.get(1)?,
                description: row.get(2)?,
                app_name: row.get(3)?,
                app_url: row.get(4)?,
                app_icon_path: row.get(5)?,
                trigger: row.get(6)?,
                perspective_ids: serde_json::from_str(&row.get::<_, String>(7)?).unwrap(),
                webhook_url: row.get(8)?,
                webhook_auth: row.get(9)?,
            }))
        } else {
            Ok(None)
        }
    }

    pub fn remove_notification(&self, id: String) -> Result<(), rusqlite::Error> {
        self.conn
            .execute("DELETE FROM notifications WHERE id = ?", [id])?;
        Ok(())
    }

    pub fn update_notification(
        &self,
        id: String,
        updated_notification: &Notification,
    ) -> Result<bool, rusqlite::Error> {
        let result = self.conn.execute(
            "UPDATE notifications SET description = ?2, appName = ?3, appUrl = ?4, appIconPath = ?5, trigger = ?6, perspective_ids = ?7, webhookUrl = ?8, webhookAuth = ?9, granted = ?10 WHERE id = ?1",
            params![
                id,
                updated_notification.description,
                updated_notification.app_name,
                updated_notification.app_url,
                updated_notification.app_icon_path,
                updated_notification.trigger,
                serde_json::to_string(&updated_notification.perspective_ids).unwrap(),
                updated_notification.webhook_url,
                updated_notification.webhook_auth,
                updated_notification.granted,
            ],
        )?;
        Ok(result > 0)
    }

    pub fn add_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<(), rusqlite::Error> {
        for proof in proofs {
            let proof = serde_json::to_string(&proof).unwrap();
            self.conn
                .execute("INSERT INTO entanglement_proof (proof) VALUES (?)", [proof])?;
        }
        Ok(())
    }

    pub fn get_all_entanglement_proofs(&self) -> Result<Vec<EntanglementProof>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT proof FROM entanglement_proof")?;
        let proof_iter = stmt.query_map([], |row| {
            let proof_json: String = row.get(0)?;
            let proof: EntanglementProof = serde_json::from_str(&proof_json).unwrap();
            Ok(proof)
        })?;

        let mut proofs = Vec::new();
        for proof in proof_iter {
            proofs.push(proof?);
        }
        Ok(proofs)
    }

    pub fn remove_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<(), rusqlite::Error> {
        for proof in proofs {
            let proof_json = serde_json::to_string(&proof).unwrap();
            self.conn.execute(
                "DELETE FROM entanglement_proof WHERE proof = ?",
                [proof_json],
            )?;
        }
        Ok(())
    }

    pub fn add_to_outbox(
        &self,
        message: &PerspectiveExpression,
        recipient: String,
    ) -> Result<(), rusqlite::Error> {
        let message_json = serde_json::to_string(message).unwrap();
        self.conn.execute(
            "INSERT INTO outbox (message, recipient) VALUES (?, ?)",
            [message_json, recipient],
        )?;
        Ok(())
    }

    pub fn get_all_from_outbox(&self) -> Result<Vec<SentMessage>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT message, recipient FROM outbox")?;
        let outbox_iter = stmt.query_map([], |row| {
            let message_json: String = row.get(0)?;
            let message: PerspectiveExpression = serde_json::from_str(&message_json).unwrap();
            let recipient: String = row.get(1)?;
            Ok(SentMessage { message, recipient })
        })?;

        let mut outbox = Vec::new();
        for message in outbox_iter {
            outbox.push(message?);
        }
        Ok(outbox)
    }

    pub fn add_friends(&self, friends: Vec<String>) -> Result<(), rusqlite::Error> {
        for friend in friends {
            self.conn
                .execute("INSERT INTO friends (friend) VALUES (?)", [friend])?;
        }
        Ok(())
    }

    pub fn remove_friends(&self, friends: Vec<String>) -> Result<(), rusqlite::Error> {
        for friend in friends {
            self.conn
                .execute("DELETE FROM friends WHERE friend = ?", [friend])?;
        }
        Ok(())
    }

    pub fn get_all_friends(&self) -> Result<Vec<String>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT friend FROM friends")?;
        let friend_iter = stmt.query_map([], |row| row.get(0))?;

        let mut friends = Vec::new();
        for friend in friend_iter {
            friends.push(friend?);
        }

        friends.sort();

        friends.dedup();

        Ok(friends)
    }

    pub fn add_known_link_languages(&self, languages: Vec<String>) -> Result<(), rusqlite::Error> {
        for language in languages {
            self.conn.execute(
                "INSERT INTO known_link_languages (language) VALUES (?)",
                [language],
            )?;
        }
        Ok(())
    }

    pub fn remove_known_link_languages(
        &self,
        languages: Vec<String>,
    ) -> Result<(), rusqlite::Error> {
        for language in languages {
            self.conn.execute(
                "DELETE FROM known_link_languages WHERE language = ?",
                [language],
            )?;
        }
        Ok(())
    }

    pub fn get_all_known_link_languages(&self) -> Result<Vec<String>, rusqlite::Error> {
        let mut stmt = self
            .conn
            .prepare("SELECT language FROM known_link_languages")?;
        let language_iter = stmt.query_map([], |row| row.get(0))?;

        let mut languages = Vec::new();
        for language in language_iter {
            languages.push(language?);
        }

        languages.sort();

        languages.dedup();

        Ok(languages)
    }

    pub fn add_trusted_agents(&self, agents: Vec<String>) -> Result<(), rusqlite::Error> {
        for agent in agents {
            self.conn
                .execute("INSERT INTO trusted_agent (agent) VALUES (?)", [agent])?;
        }
        Ok(())
    }

    pub fn get_all_trusted_agents(&self) -> Result<Vec<String>, rusqlite::Error> {
        let mut stmt = self.conn.prepare("SELECT agent FROM trusted_agent")?;
        let agent_iter = stmt.query_map([], |row| row.get(0))?;

        let mut agents = Vec::new();
        for agent in agent_iter {
            agents.push(agent?);
        }
        Ok(agents)
    }

    pub fn remove_trusted_agents(&self, agents: Vec<String>) -> Result<(), rusqlite::Error> {
        for agent in agents {
            self.conn
                .execute("DELETE FROM trusted_agent WHERE agent = ?", [agent])?;
        }
        Ok(())
    }

    pub fn add_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO perspective_handle (name, uuid, neighbourhood, shared_url, state)
             VALUES (?1, ?2, ?3, ?4, ?5)",
            params![
                perspective.name,
                perspective.uuid,
                perspective
                    .neighbourhood
                    .as_ref()
                    .and_then(|n| serde_json::to_string(n).ok()),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
            ],
        )?;
        Ok(())
    }

    pub fn _get_perspective(&self, uuid: &str) -> Ad4mDbResult<Option<PerspectiveHandle>> {
        let mut stmt = self.conn.prepare(
            "SELECT name, uuid, neighbourhood, shared_url, state FROM perspective_handle WHERE uuid = ?1",
        )?;

        let found_perspective = stmt
            .query_map([uuid], |row| {
                Ok(PerspectiveHandle {
                    name: row.get(0)?,
                    uuid: row.get(1)?,
                    neighbourhood: row
                        .get::<usize, Option<String>>(3)?
                        .and_then(|n| serde_json::from_str(&n).ok()),
                    shared_url: row.get(4)?,
                    state: serde_json::from_str(row.get::<usize, String>(5)?.as_str())
                        .expect("Could not deserialize perspective state from DB"),
                })
            })?
            .map(|p| p.ok())
            .next()
            .ok_or(anyhow!("No perspective found with given uuid"))?
            .clone();

        Ok(found_perspective)
    }

    pub fn get_all_perspectives(&self) -> Ad4mDbResult<Vec<PerspectiveHandle>> {
        let mut stmt = self.conn.prepare(
            "SELECT name, uuid, neighbourhood, shared_url, state FROM perspective_handle",
        )?;
        let perspective_iter = stmt.query_map([], |row| {
            Ok(PerspectiveHandle {
                name: row.get(0)?,
                uuid: row.get(1)?,
                neighbourhood: row
                    .get::<usize, Option<String>>(2)?
                    .and_then(|n| serde_json::from_str(&n).ok()),
                shared_url: row.get(3)?,
                state: serde_json::from_str(row.get::<usize, String>(4)?.as_str())
                    .expect("Could not deserialize perspective state from DB"),
            })
        })?;

        let mut perspectives = Vec::new();
        for perspective in perspective_iter {
            perspectives.push(perspective?);
        }

        Ok(perspectives)
    }

    pub fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE perspective_handle SET name = ?1, neighbourhood = ?2, shared_url = ?3, state = ?4 WHERE uuid = ?5",
            params![
                perspective.name,
                perspective.neighbourhood.as_ref().and_then(|n| serde_json::to_string(n).ok()),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
                perspective.uuid,
            ],
        )?;
        Ok(())
    }

    pub fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()> {
        self.conn
            .execute("DELETE FROM perspective_handle WHERE uuid = ?1", [uuid])?;
        Ok(())
    }

    pub fn add_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO link (perspective, source, predicate, target, author, timestamp, signature, key, status)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
            params![
                perspective_uuid,
                link.data.source,
                link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                link.data.target,
                link.author,
                link.timestamp,
                link.proof.signature,
                link.proof.key,
                serde_json::to_string(status)?,
            ],
        )?;
        Ok(())
    }

    pub fn add_many_links(
        &self,
        perspective_uuid: &str,
        links: Vec<LinkExpression>,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()> {
        for link in links.iter() {
            self.conn.execute(
                "INSERT INTO link (perspective, source, predicate, target, author, timestamp, signature, key, status)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                params![
                    perspective_uuid,
                    link.data.source,
                    link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                    link.data.target,
                    link.author,
                    link.timestamp,
                    link.proof.signature,
                    link.proof.key,
                    serde_json::to_string(&status)?,
                ],
            )?;
        }
        Ok(())
    }

    pub fn update_link(
        &self,
        perspective_uuid: &str,
        old_link: &LinkExpression,
        new_link: &LinkExpression,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE link SET source = ?1, predicate = ?2, target = ?3, author = ?4, timestamp = ?5, signature = ?6, key = ?7
             WHERE perspective = ?8 AND source = ?9 AND predicate = ?10 AND target = ?11 AND author = ?12 AND timestamp = ?13",
            params![
                new_link.data.source,
                new_link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                new_link.data.target,
                new_link.author,
                new_link.timestamp,
                new_link.proof.signature,
                new_link.proof.key,
                perspective_uuid,
                old_link.data.source,
                old_link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                old_link.data.target,
                old_link.author,
                old_link.timestamp,
            ],
        )?;
        Ok(())
    }

    pub fn remove_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM link WHERE perspective = ?1 AND source = ?2 AND predicate = ?3 AND target = ?4 AND author = ?5 AND timestamp = ?6",
            params![
                perspective_uuid,
                link.data.source,
                link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                link.data.target,
                link.author,
                link.timestamp,
            ],
        )?;
        Ok(())
    }

    pub fn get_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
    ) -> Ad4mDbResult<Option<(LinkExpression, LinkStatus)>> {
        let mut stmt = self.conn.prepare(
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 AND source = ?2 AND predicate = ?3 AND target = ?4 AND author = ?5 AND timestamp = ?6",
        )?;
        let link_expression: Option<(LinkExpression, LinkStatus)> = stmt
            .query_row(
                params![
                    perspective_uuid,
                    link.data.source,
                    link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                    link.data.target,
                    link.author,
                    link.timestamp
                ],
                |row| {
                    let status: LinkStatus = serde_json::from_str(&row.get::<_, String>(8)?)
                        .map_err(|e| {
                            rusqlite::Error::FromSqlConversionFailure(
                                8,
                                rusqlite::types::Type::Text,
                                Box::new(e),
                            )
                        })?;

                    let link = LinkExpression {
                        data: Link {
                            source: row.get(1)?,
                            predicate: row.get(2).map(|p: Option<String>| match p.as_deref() {
                                Some("") => None,
                                _ => p,
                            })?,
                            target: row.get(3)?,
                        },
                        proof: ExpressionProof {
                            signature: row.get(6)?,
                            key: row.get(7)?,
                        },
                        author: row.get(4)?,
                        timestamp: row.get(5)?,
                        status: Some(status.clone()),
                    };

                    Ok((link, status))
                },
            )
            .optional()?;
        Ok(link_expression)
    }

    pub fn get_all_links(
        &self,
        perspective_uuid: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        let mut stmt = self.conn.prepare(
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1",
        )?;
        let link_iter = stmt.query_map(params![perspective_uuid], |row| {
            let status: LinkStatus =
                serde_json::from_str(&row.get::<_, String>(8)?).map_err(|e| {
                    rusqlite::Error::FromSqlConversionFailure(
                        8,
                        rusqlite::types::Type::Text,
                        Box::new(e),
                    )
                })?;
            let link_expression = LinkExpression {
                data: Link {
                    source: row.get(1)?,
                    predicate: row.get(2)?,
                    target: row.get(3)?,
                },
                proof: ExpressionProof {
                    signature: row.get(6)?,
                    key: row.get(7)?,
                },
                author: row.get(4)?,
                timestamp: row.get(5)?,
                status: Some(status.clone()),
            };
            Ok((link_expression, status))
        })?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn get_links_by_source(
        &self,
        perspective_uuid: &str,
        source: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        let mut stmt = self.conn.prepare(
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 AND source = ?2",
        )?;
        let link_iter = stmt.query_map(params![perspective_uuid, source], |row| {
            let status: LinkStatus =
                serde_json::from_str(&row.get::<_, String>(8)?).map_err(|e| {
                    rusqlite::Error::FromSqlConversionFailure(
                        8,
                        rusqlite::types::Type::Text,
                        Box::new(e),
                    )
                })?;
            let link_expression = LinkExpression {
                data: Link {
                    source: row.get(1)?,
                    predicate: row.get(2)?,
                    target: row.get(3)?,
                },
                proof: ExpressionProof {
                    signature: row.get(6)?,
                    key: row.get(7)?,
                },
                author: row.get(4)?,
                timestamp: row.get(5)?,
                status: Some(status.clone()),
            };
            Ok((link_expression, status))
        })?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn get_links_by_target(
        &self,
        perspective_uuid: &str,
        target: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        let mut stmt = self.conn.prepare(
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 AND target = ?2",
        )?;
        let link_iter = stmt.query_map(params![perspective_uuid, target], |row| {
            let status: LinkStatus =
                serde_json::from_str(&row.get::<_, String>(8)?).map_err(|e| {
                    rusqlite::Error::FromSqlConversionFailure(
                        8,
                        rusqlite::types::Type::Text,
                        Box::new(e),
                    )
                })?;
            let link_expression = LinkExpression {
                data: Link {
                    source: row.get(1)?,
                    predicate: row.get(2)?,
                    target: row.get(3)?,
                },
                proof: ExpressionProof {
                    signature: row.get(6)?,
                    key: row.get(7)?,
                },
                author: row.get(4)?,
                timestamp: row.get(5)?,
                status: Some(status.clone()),
            };
            Ok((link_expression, status))
        })?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn add_pending_diff(
        &self,
        perspective_uuid: &str,
        diff: &PerspectiveDiff,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO perspective_diff (perspective, additions, removals, is_pending)
             VALUES (?1, ?2, ?3, ?4)",
            params![
                perspective_uuid,
                serde_json::to_string(&diff.additions)?,
                serde_json::to_string(&diff.removals)?,
                true,
            ],
        )?;
        Ok(())
    }

    pub fn get_pending_diffs(
        &self,
        perspective_uuid: &str,
        max_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)> {
        let mut stmt = self.conn.prepare(
            "SELECT additions, removals, id FROM perspective_diff WHERE perspective = ?1 AND is_pending = ?2",
        )?;
        let diffs_iter = stmt.query_map(params![perspective_uuid, true], |row| {
            let additions: Vec<LinkExpression> =
                serde_json::from_str(&row.get::<_, String>(0).unwrap()).unwrap();
            let removals: Vec<LinkExpression> =
                serde_json::from_str(&row.get::<_, String>(1).unwrap()).unwrap();
            let id = row.get::<_, u64>(2).unwrap();
            Ok((
                PerspectiveDiff {
                    additions,
                    removals,
                },
                id,
            ))
        })?;
        let mut diffs = Vec::new();
        let mut ids = Vec::new();
        for (count, diff_result) in diffs_iter.enumerate() {
            let (diff, id) = diff_result?;
            diffs.push(diff);
            ids.push(id);
            if max_count.map(|max| count >= max-1).unwrap_or(false) {
                break;
            }
        }
        // Assuming we want to concatenate all additions and removals from different diffs
        let mut all_additions = Vec::new();
        let mut all_removals = Vec::new();
        for diff in diffs {
            all_additions.extend(diff.additions);
            all_removals.extend(diff.removals);
        }
        Ok((
            PerspectiveDiff {
                additions: all_additions,
                removals: all_removals,
            },
            ids,
        ))
    }

    pub fn clear_pending_diffs(&self, perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()> {
        let id_list = ids
            .iter()
            .map(|id| id.to_string())
            .collect::<Vec<String>>()
            .join(",");

        self.conn.execute(
            &format!(
                "DELETE FROM perspective_diff WHERE perspective = ?1 AND id IN ({})",
                id_list
            ),
            params![perspective_uuid],
        )?;
        Ok(())
    }

    // Expression Methods

    pub fn _add_expression<T: Serialize>(
        &self,
        url: &str,
        expression: &Expression<T>,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO expression (url, data)
             VALUES (?1, ?2)",
            params![url, serde_json::to_string(expression)?,],
        )?;
        Ok(())
    }

    pub fn _get_expression(
        &self,
        url: &str,
    ) -> Ad4mDbResult<Option<Expression<serde_json::Value>>> {
        let mut stmt = self
            .conn
            .prepare("SELECT data FROM expression WHERE url = ?1")?;
        let expression: Option<String> =
            stmt.query_row(params![url], |row| row.get(0)).optional()?;
        Ok(expression.map(|e| serde_json::from_str(&e).unwrap()))
    }

    pub fn add_model(&self, model: &ModelInput) -> Ad4mDbResult<String> {
        let id = Uuid::new_v4().to_string();
        self.conn.execute(
            "INSERT INTO models (id, name, api_base_url, api_key, api_type, local_file_name, local_tokenizer_source, local_model_parameters, type)
            VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
            params![
                id,
                model.name,
                model.api.as_ref().map(|api| api.base_url.to_string()),
                model.api.as_ref().map(|api| api.api_key.clone()),
                model.api.as_ref().map(|api| serde_json::to_string(&api.api_type).unwrap()),
                model.local.as_ref().map(|local| local.file_name.clone()),
                model.local.as_ref().map(|local| local.tokenizer_source.clone()),
                model.local.as_ref().map(|local| local.model_parameters.clone()),
                serde_json::to_string(&model.model_type).unwrap(),
            ],
        )?;
        Ok(id)
    }

    pub fn get_model(&self, model_id: String) -> Ad4mDbResult<Option<Model>> {
        let mut stmt = self.conn.prepare("SELECT * FROM models WHERE id = ?1")?;
        let model = stmt
            .query_row(params![model_id], |row| {
                let api = if let (Some(base_url), Some(api_key), Some(api_type)) = (
                    row.get::<_, Option<String>>(2)?.map(|s| s.to_string()),
                    row.get::<_, Option<String>>(3)?,
                    row.get::<_, Option<String>>(4)?,
                ) {
                    Some(ModelApi {
                        base_url: Url::parse(&base_url).unwrap(),
                        api_key,
                        api_type: ModelApiType::from_str(&api_type).unwrap(),
                    })
                } else {
                    None
                };

                let local =
                    if let (Some(file_name), Some(tokenizer_source), Some(model_parameters)) =
                        (row.get(5)?, row.get(6)?, row.get(7)?)
                    {
                        Some(LocalModel {
                            file_name,
                            tokenizer_source,
                            model_parameters,
                        })
                    } else {
                        None
                    };

                Ok(Model {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    api,
                    local,
                    model_type: serde_json::from_str(&row.get::<_, String>(8)?).unwrap(),
                })
            })
            .optional()?;
        Ok(model)
    }

    pub fn get_models(&self) -> Ad4mDbResult<Vec<Model>> {
        let mut stmt = self.conn.prepare("SELECT * FROM models")?;
        let model_iter = stmt.query_map([], |row| {
            let api = if let (Some(base_url), Some(api_key), Some(api_type)) = (
                row.get::<_, Option<String>>(2)?.map(|s| s.to_string()),
                row.get::<_, Option<String>>(3)?,
                row.get::<_, Option<String>>(4)?,
            ) {
                Some(ModelApi {
                    base_url: Url::parse(&base_url).unwrap(),
                    api_key,
                    api_type: ModelApiType::from_str(&api_type).unwrap(),
                })
            } else {
                None
            };

            let local = if let (Some(file_name), Some(tokenizer_source), Some(model_parameters)) =
                (row.get(5)?, row.get(6)?, row.get(7)?)
            {
                Some(LocalModel {
                    file_name,
                    tokenizer_source,
                    model_parameters,
                })
            } else {
                None
            };

            Ok(Model {
                id: row.get(0)?,
                name: row.get(1)?,
                api,
                local,
                model_type: serde_json::from_str(&row.get::<_, String>(8)?).unwrap(),
            })
        })?;

        let mut models = Vec::new();
        for model in model_iter {
            models.push(model?);
        }
        Ok(models)
    }

    pub fn update_model(&self, id: &str, model: &ModelInput) -> Ad4mDbResult<()> {
        let api_base_url = model.api.as_ref().map(|api| api.base_url.to_string());
        let api_key = model.api.as_ref().map(|api| api.api_key.clone());
        let api_type = model.api.as_ref().map(|api| api.api_type.to_string());
        let local_file_name = model.local.as_ref().map(|local| local.file_name.clone());
        let local_tokenizer = model
            .local
            .as_ref()
            .map(|local| local.tokenizer_source.clone());
        let local_params = model
            .local
            .as_ref()
            .map(|local| local.model_parameters.clone());

        self.conn.execute(
            "UPDATE models SET 
                name = ?1,
                api_base_url = ?2,
                api_key = ?3,
                api_type = ?4,
                local_file_name = ?5,
                local_tokenizer_source = ?6,
                local_model_parameters = ?7,
                type = ?8
             WHERE id = ?9",
            params![
                model.name,
                api_base_url,
                api_key,
                api_type,
                local_file_name,
                local_tokenizer,
                local_params,
                serde_json::to_string(&model.model_type).unwrap(),
                id
            ],
        )?;
        Ok(())
    }

    pub fn remove_model(&self, id: &str) -> Ad4mDbResult<()> {
        self.conn
            .execute("DELETE FROM models WHERE id = ?1", params![id])?;
        Ok(())
    }

    pub fn set_default_model(&self, model_type: ModelType, model_id: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO default_models (model_type, model_id) 
             VALUES (?1, ?2)
             ON CONFLICT(model_type) DO UPDATE SET
             model_id = excluded.model_id",
            params![serde_json::to_string(&model_type).unwrap(), model_id],
        )?;
        Ok(())
    }

    pub fn get_default_model(&self, model_type: ModelType) -> Ad4mDbResult<Option<String>> {
        let mut stmt = self
            .conn
            .prepare("SELECT model_id FROM default_models WHERE model_type = ?1")?;
        let mut rows = stmt.query(params![serde_json::to_string(&model_type).unwrap()])?;

        if let Some(row) = rows.next()? {
            Ok(Some(row.get(0)?))
        } else {
            Ok(None)
        }
    }

    pub fn with_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&Ad4mDb) -> R,
    {
        let global_instance_arc = Ad4mDb::global_instance();
        let lock_result = global_instance_arc.lock();
        let ad4m_db_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let ad4m_db_ref = ad4m_db_lock.as_ref().expect("Ad4mDb not initialized");
        func(ad4m_db_ref)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        graphql::graphql_types::{LocalModelInput, ModelApiInput},
        types::{ExpressionProof, Link, LinkExpression, ModelApiType, ModelType},
    };
    use chrono::Utc;
    use fake::{Fake, Faker};
    use uuid::Uuid;

    fn construct_dummy_link_expression(status: LinkStatus) -> LinkExpression {
        LinkExpression {
            data: Link {
                source: Faker.fake::<String>(),
                target: Faker.fake::<String>(),
                predicate: Some(Faker.fake::<String>()),
            },
            proof: ExpressionProof {
                signature: "signature".to_string(),
                key: "key".to_string(),
            },
            author: "did:test:key".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            status: Some(status),
        }
    }

    #[test]
    fn can_store_and_retrieve_links() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link, &LinkStatus::Shared).unwrap();

        let result: Option<(LinkExpression, LinkStatus)> = db.get_link(&p_uuid, &link).unwrap();
        assert!(result.is_some());
        assert_eq!(result.unwrap(), (link, LinkStatus::Shared));
    }

    #[test]
    fn can_store_and_get_link_with_missing_predicate() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let mut link = construct_dummy_link_expression(LinkStatus::Shared);
        link.data.predicate = None;
        db.add_link(&p_uuid, &link, &LinkStatus::Shared).unwrap();

        let result = db.get_link(&p_uuid, &link).unwrap();
        assert_eq!(result, Some((link, LinkStatus::Shared)));
    }

    #[test]
    fn can_call_get_link_multiple_times() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link = construct_dummy_link_expression(LinkStatus::Local);
        db.add_link(&p_uuid, &link, &LinkStatus::Local).unwrap();

        for _ in 0..3 {
            let result = db.get_link(&p_uuid, &link).unwrap();
            assert_eq!(result, Some((link.clone(), LinkStatus::Local)));
        }
    }

    #[test]
    fn can_get_all_links() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression(LinkStatus::Local);
        db.add_link(&p_uuid, &link2, &LinkStatus::Local).unwrap();

        let all_links = db.get_all_links(&p_uuid).unwrap();
        assert_eq!(
            all_links,
            vec![(link1, LinkStatus::Shared), (link2, LinkStatus::Local)]
        );
    }

    #[test]
    fn can_call_get_all_links_multiple_times() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();

        for _ in 0..3 {
            let all_links = db.get_all_links(&p_uuid).unwrap();
            assert_eq!(all_links, vec![(link1.clone(), LinkStatus::Shared)]);
        }
    }

    #[test]
    fn can_get_links_by_source() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link2, &LinkStatus::Shared).unwrap();

        let result = db.get_links_by_source(&p_uuid, &link1.data.source).unwrap();
        assert_eq!(result, vec![(link1, LinkStatus::Shared)]);
    }

    #[test]
    fn can_get_links_by_target() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link2, &LinkStatus::Shared).unwrap();

        let result = db.get_links_by_target(&p_uuid, &link1.data.target).unwrap();
        assert_eq!(result, vec![(link1, LinkStatus::Shared)]);
    }

    #[test]
    fn can_update_link() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression(LinkStatus::Shared);
        db.update_link(&p_uuid, &link1, &link2).unwrap();

        assert!(db.get_link(&p_uuid, &link1).unwrap().is_none());
        let result = db.get_link(&p_uuid, &link2).unwrap();
        assert_eq!(result, Some((link2, LinkStatus::Shared)));
    }

    #[test]
    fn can_remove_link() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();

        let result = db.get_link(&p_uuid, &link1).unwrap();
        assert_eq!(result, Some((link1.clone(), LinkStatus::Shared)));
        db.remove_link(&p_uuid, &link1).unwrap();
        assert!(db.get_link(&p_uuid, &link1).unwrap().is_none());
    }

    #[test]
    fn can_get_and_remove_pending_diffs() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let addition = construct_dummy_link_expression(LinkStatus::Shared);
        let removal = construct_dummy_link_expression(LinkStatus::Shared);
        db.add_pending_diff(
            &p_uuid,
            &PerspectiveDiff {
                additions: vec![addition.clone()],
                removals: vec![removal.clone()],
            },
        )
        .unwrap();

        let (diff, ids) = db.get_pending_diffs(&p_uuid, None).unwrap();
        assert_eq!(ids.len(), 1);
        assert_eq!(diff.additions.len(), 1);
        assert_eq!(diff.removals.len(), 1);
        assert_eq!(
            diff,
            PerspectiveDiff {
                additions: vec![addition],
                removals: vec![removal],
            }
        );

        db.clear_pending_diffs(&p_uuid, ids).unwrap();
        let (diff, ids) = db.get_pending_diffs(&p_uuid, None).unwrap();
        assert_eq!(ids.len(), 0);
        assert_eq!(diff.additions.len(), 0);

        // Create 3 different diffs
        let diff1 = PerspectiveDiff {
            additions: vec![construct_dummy_link_expression(LinkStatus::Shared)],
            removals: vec![],
        };
        let diff2 = PerspectiveDiff {
            additions: vec![construct_dummy_link_expression(LinkStatus::Shared)],
            removals: vec![],
        };
        let diff3 = PerspectiveDiff {
            additions: vec![construct_dummy_link_expression(LinkStatus::Shared)],
            removals: vec![],
        };

        // Add all 3 diffs
        db.add_pending_diff(&p_uuid, &diff1).unwrap();
        db.add_pending_diff(&p_uuid, &diff2).unwrap();
        db.add_pending_diff(&p_uuid, &diff3).unwrap();

        // Get only 2 diffs using max_count
        let (diff, ids) = db.get_pending_diffs(&p_uuid, Some(2)).unwrap();

        // Assert we only got 2 diffs
        assert_eq!(ids.len(), 2);
        assert_eq!(diff.additions.len(), 2);
        assert_eq!(diff.removals.len(), 0);
    }

    #[test]
    fn can_handle_notifications() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create a test notification
        let notification = NotificationInput {
            description: "Test Description".to_string(),
            app_name: "Test App Name".to_string(),
            app_url: "Test App URL".to_string(),
            app_icon_path: "Test App Icon Path".to_string(),
            trigger: "Test Trigger".to_string(),
            perspective_ids: vec!["Test Perspective ID".to_string()],
            webhook_url: "Test Webhook URL".to_string(),
            webhook_auth: "Test Webhook Auth".to_string(),
        };

        // Add the test notification
        let notification_id = db.add_notification(notification).unwrap();
        // Get all notifications
        let notifications = db.get_notifications().unwrap();

        // Ensure the test notification is in the list of notifications and has all properties set
        let test_notification = notifications
            .iter()
            .find(|n| n.id == notification_id)
            .unwrap();
        assert_eq!(test_notification.description, "Test Description");
        assert_eq!(test_notification.app_name, "Test App Name");
        assert_eq!(test_notification.app_url, "Test App URL");
        assert_eq!(
            test_notification.app_icon_path,
            "Test App Icon Path".to_string()
        );
        assert_eq!(test_notification.trigger, "Test Trigger");
        assert_eq!(
            test_notification.perspective_ids,
            vec!["Test Perspective ID".to_string()]
        );
        assert_eq!(test_notification.webhook_url, "Test Webhook URL");
        assert_eq!(test_notification.webhook_auth, "Test Webhook Auth");

        // Modify the test notification
        let updated_notification = Notification {
            id: notification_id.clone(),
            granted: true,
            description: "Update Test Description".to_string(),
            app_name: "Test App Name".to_string(),
            app_url: "Test App URL".to_string(),
            app_icon_path: "Test App Icon Path".to_string(),
            trigger: "Test Trigger".to_string(),
            perspective_ids: vec!["Test Perspective ID".to_string()],
            webhook_url: "Test Webhook URL".to_string(),
            webhook_auth: "Test Webhook Auth".to_string(),
        };

        // Update the test notification
        let updated = db
            .update_notification(notification_id.clone(), &updated_notification)
            .unwrap();
        assert!(updated);

        // Check if the notification is updated
        let updated_notifications = db.get_notifications().unwrap();
        let updated_test_notification = updated_notifications
            .iter()
            .find(|n| n.id == notification_id)
            .unwrap();
        assert_eq!(
            updated_test_notification.description,
            "Update Test Description"
        );

        // Remove the test notification
        db.remove_notification(notification_id.clone()).unwrap();
        // Ensure the test notification is removed
        let notifications_after_removal = db.get_notifications().unwrap();
        assert!(notifications_after_removal
            .iter()
            .all(|n| n.id != notification_id));
    }

    #[test]
    fn test_task_operations() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Test adding a task
        let name = "Test Task".to_string();
        let model_id = "test_model".to_string();
        let system_prompt = "Test system prompt".to_string();
        let prompt_examples = vec![AIPromptExamples {
            input: "Test human prompt".to_string(),
            output: "Test AI response".to_string(),
        }];

        let task_id = db
            .add_task(
                name.clone(),
                model_id.clone(),
                system_prompt.clone(),
                prompt_examples.clone(),
                None,
            )
            .unwrap();

        // Test getting the task
        let retrieved_task = db.get_task(task_id.clone()).unwrap().unwrap();
        assert_eq!(retrieved_task.task_id, task_id);
        assert_eq!(retrieved_task.model_id, model_id);
        assert_eq!(retrieved_task.system_prompt, system_prompt);
        assert_eq!(retrieved_task.prompt_examples, prompt_examples);

        // Test getting all tasks
        let all_tasks = db.get_tasks().unwrap();
        assert_eq!(all_tasks.len(), 1);
        assert_eq!(all_tasks[0].task_id, task_id);

        // Test removing the task
        db.remove_task(task_id.clone()).unwrap();

        // Ensure the task is removed
        let task_after_removal = db.get_task(task_id.clone()).unwrap();
        assert!(task_after_removal.is_none());

        let all_tasks_after_removal = db.get_tasks().unwrap();
        assert!(all_tasks_after_removal.is_empty());
    }

    #[test]
    fn test_models_crud() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create a test model with ModelApi
        let test_model_api = ModelInput {
            name: "Test Model API".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.com".to_string(),
                api_key: "test_api_key".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Llm,
        };

        // Create a test model with LocalModel
        let test_model_local = ModelInput {
            name: "Test Model Local".to_string(),
            api: None,
            local: Some(LocalModelInput {
                file_name: "test_model.bin".to_string(),
                tokenizer_source: "test_tokenizer".to_string(),
                model_parameters: "test_parameters".to_string(),
            }),
            model_type: ModelType::Llm,
        };

        // Add the test models
        let id_model_api = db.add_model(&test_model_api).unwrap();
        let id_model_local = db.add_model(&test_model_local).unwrap();

        // Get all models
        let models = db.get_models().unwrap();

        // Ensure the test models are in the list of models and have all properties set
        let retrieved_model_api = models.iter().find(|m| m.name == "Test Model API").unwrap();
        assert_eq!(retrieved_model_api.id, id_model_api);
        assert_eq!(retrieved_model_api.name, "Test Model API");
        assert!(retrieved_model_api.api.is_some());
        assert!(retrieved_model_api.local.is_none());
        assert_eq!(
            retrieved_model_api.api.as_ref().unwrap().base_url,
            Url::parse("https://api.example.com").unwrap()
        );
        assert_eq!(
            retrieved_model_api.api.as_ref().unwrap().api_key,
            "test_api_key"
        );
        assert_eq!(
            retrieved_model_api.api.as_ref().unwrap().api_type,
            ModelApiType::OpenAi
        );
        assert_eq!(retrieved_model_api.model_type, ModelType::Llm);

        let retrieved_model_local = models.iter().find(|m| m.id == id_model_local).unwrap();
        assert_eq!(retrieved_model_local.name, "Test Model Local");
        assert!(retrieved_model_local.api.is_none());
        assert!(retrieved_model_local.local.is_some());
        assert_eq!(
            retrieved_model_local.local.as_ref().unwrap().file_name,
            "test_model.bin"
        );
        assert_eq!(
            retrieved_model_local
                .local
                .as_ref()
                .unwrap()
                .tokenizer_source,
            "test_tokenizer"
        );
        assert_eq!(
            retrieved_model_local
                .local
                .as_ref()
                .unwrap()
                .model_parameters,
            "test_parameters"
        );
        assert_eq!(retrieved_model_local.model_type, ModelType::Llm);

        // Remove the test models
        db.remove_model(&id_model_api).unwrap();
        db.remove_model(&id_model_local).unwrap();

        // Ensure the test models are removed
        let models_after_removal = db.get_models().unwrap();
        assert!(models_after_removal
            .iter()
            .all(|m| m.name != "Test Model API" && m.name != "Test Model Local"));
    }

    #[test]
    fn test_update_model() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create initial model
        let initial_model = ModelInput {
            name: "Test Model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.com".to_string(),
                api_key: "test_key".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Llm,
        };

        // Add model and get its ID
        let model_id = db.add_model(&initial_model).unwrap();

        // Create updated model
        let updated_model = ModelInput {
            name: "Updated Model".to_string(),
            api: None,
            local: Some(LocalModelInput {
                file_name: "local_model.bin".to_string(),
                tokenizer_source: "tokenizer.json".to_string(),
                model_parameters: "{\"param\": \"value\"}".to_string(),
            }),
            model_type: ModelType::Embedding,
        };

        // Update the model
        db.update_model(&model_id, &updated_model).unwrap();

        // Retrieve and verify the updated model
        let retrieved_model = db.get_model(model_id.clone()).unwrap().unwrap();

        assert_eq!(retrieved_model.name, "Updated Model");
        assert!(retrieved_model.api.is_none());
        assert!(retrieved_model.local.is_some());

        let local = retrieved_model.local.unwrap();
        assert_eq!(local.file_name, "local_model.bin");
        assert_eq!(local.tokenizer_source, "tokenizer.json");
        assert_eq!(local.model_parameters, "{\"param\": \"value\"}");
        assert_eq!(retrieved_model.model_type, ModelType::Embedding);

        // Clean up
        db.remove_model(&model_id).unwrap();
    }

    #[test]
    fn test_model_status() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create three models of different types
        let model_llm = ModelInput {
            name: "Test LLM Model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.example.com".to_string(),
                api_key: "llm_key".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Llm,
        };

        let model_embedding = ModelInput {
            name: "Test Embedding Model".to_string(),
            local: Some(LocalModelInput {
                file_name: "embedding.bin".to_string(),
                tokenizer_source: "embedding_tokenizer".to_string(),
                model_parameters: "{}".to_string(),
            }),
            api: None,
            model_type: ModelType::Embedding,
        };

        let model_transcription = ModelInput {
            name: "Test Transcription Model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.transcribe.com".to_string(),
                api_key: "transcribe_key".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Transcription,
        };

        // Add first model and test its status
        db.add_model(&model_llm).unwrap();
        db.create_or_update_model_status(&model_llm.name, 1.0, "ready", true, true)
            .unwrap();
        assert!(
            db.get_model_status(&model_llm.name)
                .unwrap()
                .unwrap()
                .loaded
        );

        // Add second model and test both statuses
        db.add_model(&model_embedding).unwrap();
        db.create_or_update_model_status(&model_embedding.name, 1.0, "ready", false, false)
            .unwrap();
        assert!(
            db.get_model_status(&model_llm.name)
                .unwrap()
                .unwrap()
                .loaded
        );
        assert!(
            !db.get_model_status(&model_embedding.name)
                .unwrap()
                .unwrap()
                .loaded
        );

        // Add third model and test all statuses
        db.add_model(&model_transcription).unwrap();
        db.create_or_update_model_status(&model_transcription.name, 1.0, "ready", true, true)
            .unwrap();
        assert!(
            db.get_model_status(&model_llm.name)
                .unwrap()
                .unwrap()
                .loaded
        );
        assert!(
            !db.get_model_status(&model_embedding.name)
                .unwrap()
                .unwrap()
                .loaded
        );
        assert!(
            db.get_model_status(&model_transcription.name)
                .unwrap()
                .unwrap()
                .loaded
        );

        // Update some statuses and verify all still work
        db.create_or_update_model_status(&model_transcription.name, 1.0, "ready", false, false)
            .unwrap();
        db.create_or_update_model_status(&model_embedding.name, 1.0, "ready", true, true)
            .unwrap();

        assert!(
            db.get_model_status(&model_llm.name)
                .unwrap()
                .unwrap()
                .loaded
        );
        assert!(
            db.get_model_status(&model_embedding.name)
                .unwrap()
                .unwrap()
                .loaded
        );
        assert!(
            !db.get_model_status(&model_transcription.name)
                .unwrap()
                .unwrap()
                .loaded
        );

        // Clean up
        db.remove_model(&model_llm.name).unwrap();
        db.remove_model(&model_embedding.name).unwrap();
        db.remove_model(&model_transcription.name).unwrap();
    }

    #[test]
    fn can_set_and_get_default_model() {
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create a test model
        let model = ModelInput {
            name: "test-model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.test.com".to_string(),
                api_key: "test-key".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Llm,
        };

        // Add model to DB
        let model_id = db.add_model(&model).unwrap();

        // Initially no default model set
        let default = db.get_default_model(ModelType::Llm).unwrap();
        assert!(default.is_none());

        // Set default model
        db.set_default_model(ModelType::Llm, &model_id).unwrap();

        // Verify default model is set correctly
        let default = db.get_default_model(ModelType::Llm).unwrap();
        assert!(default.is_some());
        let retrieved_default_model = db
            .get_model(default.unwrap())
            .expect("to get added model")
            .expect("to get added model");
        assert_eq!(retrieved_default_model.name, "test-model");

        // Update default model
        let model2 = ModelInput {
            name: "test-model-2".to_string(),
            api: None,
            local: Some(LocalModelInput {
                file_name: "model.bin".to_string(),
                tokenizer_source: "tokenizer".to_string(),
                model_parameters: "{}".to_string(),
            }),
            model_type: ModelType::Transcription,
        };
        let model2_id = db.add_model(&model2).unwrap();
        db.set_default_model(ModelType::Transcription, &model2_id)
            .unwrap();

        // Verify default was updated
        let default = db.get_default_model(ModelType::Transcription).unwrap();
        assert_eq!(default, Some(model2_id));

        // Clean up
        db.remove_model(&model.name).unwrap();
        db.remove_model(&model2.name).unwrap();
    }
}
