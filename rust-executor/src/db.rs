use crate::graphql::graphql_types::{
    AIModelLoadingStatus, EntanglementProof, ImportResult, LinkStatus, ModelInput,
    NotificationInput, PerspectiveExpression, PerspectiveHandle, PerspectiveState, SentMessage,
};
use crate::types::{
    AIPromptExamples, AITask, Expression, ExpressionProof, Link, LinkExpression, LocalModel, Model,
    ModelApi, ModelApiType, ModelType, Notification, PerspectiveDiff, TokenizerSource, User,
    UserInfo,
};
use crate::utils::constant_time_eq;
use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use rand::Rng;
use rusqlite::{params, Connection, OptionalExtension};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use sha2::{Digest, Sha256};
use std::str::FromStr;
use std::time::{SystemTime, UNIX_EPOCH};
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
        // Ensure we don't have duplicate link expressions and enforce uniqueness going forward
        conn.execute(
            "DELETE FROM link
             WHERE id NOT IN (
               SELECT MIN(id) FROM link
               GROUP BY perspective, source, predicate, target, author, timestamp
             )",
            [],
        )?;
        conn.execute(
            "CREATE UNIQUE INDEX IF NOT EXISTS link_unique_idx
             ON link (perspective, source, predicate, target, author, timestamp)",
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
                model TEXT,
                api_type TEXT,
                local_file_name TEXT,
                local_tokenizer_repo TEXT,
                local_tokenizer_revision TEXT,
                local_tokenizer_file_name TEXT,
                local_huggingface_repo TEXT,
                local_revision TEXT,
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

        conn.execute(
            "CREATE TABLE IF NOT EXISTS users (
                username TEXT PRIMARY KEY,
                did TEXT NOT NULL,
                password_hash TEXT NOT NULL,
                last_seen INTEGER
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS settings (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS email_verifications (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                email TEXT NOT NULL,
                code_hash TEXT NOT NULL,
                created_at INTEGER NOT NULL,
                expires_at INTEGER NOT NULL,
                verified BOOLEAN DEFAULT 0,
                verification_type TEXT NOT NULL,
                UNIQUE(email, verification_type)
             )",
            [],
        )?;

        conn.execute(
            "CREATE INDEX IF NOT EXISTS idx_email_verifications_email ON email_verifications(email)",
            [],
        )?;

        conn.execute(
            "CREATE INDEX IF NOT EXISTS idx_email_verifications_expires ON email_verifications(expires_at)",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS email_rate_limits (
                email TEXT PRIMARY KEY,
                last_sent_at INTEGER NOT NULL
             )",
            [],
        )?;

        // Add failed_attempts column to email_verifications table if it doesn't exist
        // This column tracks failed verification attempts to prevent brute force attacks
        let _ = conn.execute(
            "ALTER TABLE email_verifications ADD COLUMN failed_attempts INTEGER DEFAULT 0",
            [],
        );

        // Add owner_did column to existing perspective_handle table if it doesn't exist
        let _ = conn.execute(
            "ALTER TABLE perspective_handle ADD COLUMN owner_did TEXT",
            [],
        );

        // Add owners column for multi-user neighbourhood support
        let _ = conn.execute(
            "ALTER TABLE perspective_handle ADD COLUMN owners TEXT DEFAULT '[]'",
            [],
        );

        // Migrate existing owner_did values to owners array
        let _ = conn.execute(
            "UPDATE perspective_handle SET owners = json_array(owner_did) WHERE owner_did IS NOT NULL AND owners = '[]'",
            [],
        );

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
        let owners_json = serde_json::to_string(&perspective.owners.clone().unwrap_or_default())?;

        self.conn.execute(
            "INSERT INTO perspective_handle (name, uuid, neighbourhood, shared_url, state, owners)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                perspective.name,
                perspective.uuid,
                perspective
                    .neighbourhood
                    .as_ref()
                    .and_then(|n| serde_json::to_string(n).ok()),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
                owners_json,
            ],
        )?;
        Ok(())
    }

    pub fn _get_perspective(&self, uuid: &str) -> Ad4mDbResult<Option<PerspectiveHandle>> {
        let mut stmt = self.conn.prepare(
            "SELECT name, uuid, neighbourhood, shared_url, state, owners FROM perspective_handle WHERE uuid = ?1",
        )?;

        let found_perspective = stmt
            .query_map([uuid], |row| {
                let owners_json: Option<String> = row.get(5)?;
                let owners = owners_json.and_then(|json| serde_json::from_str(&json).ok());

                Ok(PerspectiveHandle {
                    name: row.get(0)?,
                    uuid: row.get(1)?,
                    neighbourhood: row
                        .get::<usize, Option<String>>(2)?
                        .and_then(|n| serde_json::from_str(&n).ok()),
                    shared_url: row.get(3)?,
                    state: serde_json::from_str(row.get::<usize, String>(4)?.as_str())
                        .expect("Could not deserialize perspective state from DB"),
                    owners,
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
            "SELECT name, uuid, neighbourhood, shared_url, state, owners FROM perspective_handle",
        )?;
        let perspective_iter = stmt.query_map([], |row| {
            let owners_json: Option<String> = row.get(5)?;
            let owners = owners_json.and_then(|json| serde_json::from_str(&json).ok());

            Ok(PerspectiveHandle {
                name: row.get(0)?,
                uuid: row.get(1)?,
                neighbourhood: row
                    .get::<usize, Option<String>>(2)?
                    .and_then(|n| serde_json::from_str(&n).ok()),
                shared_url: row.get(3)?,
                state: serde_json::from_str(row.get::<usize, String>(4)?.as_str())
                    .expect("Could not deserialize perspective state from DB"),
                owners,
            })
        })?;

        let mut perspectives = Vec::new();
        for perspective in perspective_iter {
            perspectives.push(perspective?);
        }

        Ok(perspectives)
    }

    pub fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        let owners_json = serde_json::to_string(&perspective.owners.clone().unwrap_or_default())?;

        self.conn.execute(
            "UPDATE perspective_handle SET name = ?1, neighbourhood = ?2, shared_url = ?3, state = ?4, owners = ?5 WHERE uuid = ?6",
            params![
                perspective.name,
                perspective.neighbourhood.as_ref().and_then(|n| serde_json::to_string(n).ok()),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
                owners_json,
                perspective.uuid,
            ],
        )?;
        Ok(())
    }

    pub fn add_owner_to_neighbourhood(
        &self,
        neighbourhood_url: &str,
        user_did: &str,
    ) -> Ad4mDbResult<()> {
        // First verify the perspective exists
        let perspective_exists: bool = self
            .conn
            .prepare("SELECT EXISTS(SELECT 1 FROM perspective_handle WHERE shared_url = ?1)")?
            .query_row([neighbourhood_url], |row| row.get(0))
            .map_err(|e| anyhow!("Failed to check if perspective exists: {}", e))?;

        if !perspective_exists {
            return Err(anyhow!(
                "Perspective with shared_url '{}' not found",
                neighbourhood_url
            ));
        }

        // Get current owners - propagate query errors instead of using unwrap_or_default
        let current_owners: Vec<String> = self
            .conn
            .prepare("SELECT owners FROM perspective_handle WHERE shared_url = ?1")?
            .query_row([neighbourhood_url], |row| {
                let owners_json: String = row.get(0)?;
                serde_json::from_str(&owners_json).map_err(|e| {
                    rusqlite::Error::FromSqlConversionFailure(
                        0,
                        rusqlite::types::Type::Text,
                        Box::new(e),
                    )
                })
            })
            .map_err(|e| match e {
                rusqlite::Error::QueryReturnedNoRows => {
                    anyhow!(
                        "Perspective with shared_url '{}' not found",
                        neighbourhood_url
                    )
                }
                e => anyhow!("Failed to fetch owners: {}", e),
            })?;

        // Add new owner if not already present
        let mut updated_owners = current_owners;
        if !updated_owners.contains(&user_did.to_string()) {
            updated_owners.push(user_did.to_string());
        }

        // Update database and check affected row count
        let owners_json = serde_json::to_string(&updated_owners)?;
        let affected_rows = self.conn.execute(
            "UPDATE perspective_handle SET owners = ?1 WHERE shared_url = ?2",
            params![owners_json, neighbourhood_url],
        )?;

        if affected_rows == 0 {
            return Err(anyhow!(
                "Failed to add owner: perspective with shared_url '{}' not found or could not be updated",
                neighbourhood_url
            ));
        }

        Ok(())
    }

    pub fn get_neighbourhood_owners(&self, neighbourhood_url: &str) -> Ad4mDbResult<Vec<String>> {
        let owners: Vec<String> = self
            .conn
            .prepare("SELECT owners FROM perspective_handle WHERE shared_url = ?1")?
            .query_row([neighbourhood_url], |row| {
                let owners_json: String = row.get(0)?;
                Ok(serde_json::from_str(&owners_json).unwrap_or_default())
            })
            .unwrap_or_default();

        Ok(owners)
    }

    pub fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()> {
        // Delete the perspective handle itself
        self.conn
            .execute("DELETE FROM perspective_handle WHERE uuid = ?1", [uuid])?;

        // Delete all links associated with this perspective
        self.conn
            .execute("DELETE FROM link WHERE perspective = ?1", [uuid])?;

        // Delete all pending diffs associated with this perspective
        self.conn.execute(
            "DELETE FROM perspective_diff WHERE perspective = ?1",
            [uuid],
        )?;

        Ok(())
    }

    pub fn add_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO link (perspective, source, predicate, target, author, timestamp, signature, key, status)
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
                "INSERT OR IGNORE INTO link (perspective, source, predicate, target, author, timestamp, signature, key, status)
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
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 ORDER BY timestamp, source, predicate, target, author",
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
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 AND source = ?2 ORDER BY timestamp, predicate, target, author",
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
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link WHERE perspective = ?1 AND target = ?2 ORDER BY timestamp, source, predicate, author",
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
            if max_count.map(|max| count >= max - 1).unwrap_or(false) {
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

    pub fn get_pending_diffs_by_size(
        &self,
        perspective_uuid: &str,
        max_bytes: usize,
        initial_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)> {
        let mut count = initial_count.unwrap_or(100); // Start with provided count or default to 100
        loop {
            let (diffs, ids) = self.get_pending_diffs(perspective_uuid, Some(count))?;

            // Check serialized size
            let serialized = serde_json::to_string(&diffs)?;
            if serialized.len() <= max_bytes || count == 1 {
                return Ok((diffs, ids));
            }

            // Reduce count and try again
            count /= 2;
        }
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
        let id = uuid::Uuid::new_v4().to_string();
        self.conn.execute(
            "INSERT INTO models (id, name, api_base_url, api_key, model, api_type, local_file_name, local_tokenizer_repo, local_tokenizer_revision, local_tokenizer_file_name, local_huggingface_repo, local_revision, type)
            VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)",
            params![
                id,
                model.name,
                model.api.as_ref().map(|api| api.base_url.to_string()),
                model.api.as_ref().map(|api| api.api_key.clone()),
                model.api.as_ref().map(|api| api.model.clone()),
                model.api.as_ref().map(|api| serde_json::to_string(&api.api_type).unwrap()),
                model.local.as_ref().map(|local| local.file_name.clone()),
                model.local.as_ref().and_then(|local| local.tokenizer_source.as_ref().map(|ts| ts.repo.clone())),
                model.local.as_ref().and_then(|local| local.tokenizer_source.as_ref().map(|ts| ts.revision.clone())),
                model.local.as_ref().and_then(|local| local.tokenizer_source.as_ref().map(|ts| ts.file_name.clone())),
                model.local.as_ref().and_then(|local| local.huggingface_repo.clone()),
                model.local.as_ref().and_then(|local| local.revision.clone()),
                serde_json::to_string(&model.model_type).unwrap(),
            ],
        )?;
        Ok(id)
    }

    pub fn get_model(&self, model_id: String) -> Ad4mDbResult<Option<Model>> {
        let mut stmt = self.conn.prepare("SELECT * FROM models WHERE id = ?1")?;
        let model = stmt
            .query_row(params![model_id], |row| {
                let api = if let (Some(base_url), Some(api_key), Some(model), Some(api_type)) = (
                    row.get::<_, Option<String>>(2)?.map(|s| s.to_string()),
                    row.get::<_, Option<String>>(3)?,
                    row.get::<_, Option<String>>(4)?,
                    row.get::<_, Option<String>>(5)?,
                ) {
                    Some(ModelApi {
                        base_url: Url::parse(&base_url).unwrap(),
                        api_key,
                        model,
                        api_type: ModelApiType::from_str(&api_type).unwrap(),
                    })
                } else {
                    None
                };

                let local: Option<LocalModel> = if let Some(file_name) =
                    row.get::<_, Option<String>>(6)?
                {
                    let tokenizer_source = if let (Some(repo), Some(revision), Some(file_name)) = (
                        row.get::<_, Option<String>>(7)?,
                        row.get::<_, Option<String>>(8)?,
                        row.get::<_, Option<String>>(9)?,
                    ) {
                        Some(TokenizerSource {
                            repo,
                            revision,
                            file_name,
                        })
                    } else {
                        None
                    };

                    Some(LocalModel {
                        file_name,
                        tokenizer_source,
                        huggingface_repo: row.get::<_, Option<String>>(10)?,
                        revision: row.get::<_, Option<String>>(11)?,
                    })
                } else {
                    None
                };

                Ok(Model {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    api,
                    local,
                    model_type: serde_json::from_str(&row.get::<_, String>(12)?).unwrap(),
                })
            })
            .optional()?;
        Ok(model)
    }

    pub fn get_models(&self) -> Ad4mDbResult<Vec<Model>> {
        let mut stmt = self.conn.prepare("SELECT * FROM models")?;
        let model_iter = stmt.query_map([], |row| {
            let api = if let (Some(base_url), Some(api_key), Some(model), Some(api_type)) = (
                row.get::<_, Option<String>>(2)?.map(|s| s.to_string()),
                row.get::<_, Option<String>>(3)?,
                row.get::<_, Option<String>>(4)?,
                row.get::<_, Option<String>>(5)?,
            ) {
                Some(ModelApi {
                    base_url: Url::parse(&base_url).unwrap(),
                    api_key,
                    model,
                    api_type: ModelApiType::from_str(&api_type).unwrap(),
                })
            } else {
                None
            };

            let local: Option<LocalModel> =
                if let Some(file_name) = row.get::<_, Option<String>>(6)? {
                    let tokenizer_source = if let (Some(repo), Some(revision), Some(file_name)) = (
                        row.get::<_, Option<String>>(7)?,
                        row.get::<_, Option<String>>(8)?,
                        row.get::<_, Option<String>>(9)?,
                    ) {
                        Some(TokenizerSource {
                            repo,
                            revision,
                            file_name,
                        })
                    } else {
                        None
                    };

                    Some(LocalModel {
                        file_name,
                        tokenizer_source,
                        huggingface_repo: row.get::<_, Option<String>>(10)?,
                        revision: row.get::<_, Option<String>>(11)?,
                    })
                } else {
                    None
                };

            Ok(Model {
                id: row.get(0)?,
                name: row.get(1)?,
                api,
                local,
                model_type: serde_json::from_str(&row.get::<_, String>(12)?).unwrap(),
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
        let api_model = model.api.as_ref().map(|api| api.model.clone());
        let api_type = model.api.as_ref().map(|api| api.api_type.to_string());
        let local_file_name = model.local.as_ref().map(|local| local.file_name.clone());
        let local_tokenizer = model
            .local
            .as_ref()
            .and_then(|local| local.tokenizer_source.as_ref());
        let local_huggingface_repo = model
            .local
            .as_ref()
            .and_then(|local| local.huggingface_repo.clone());
        let local_revision = model
            .local
            .as_ref()
            .and_then(|local| local.revision.clone());

        self.conn.execute(
            "UPDATE models SET 
                name = ?1,
                api_base_url = ?2,
                api_key = ?3,
                model = ?4,
                api_type = ?5,
                local_file_name = ?6,
                local_tokenizer_repo = ?7,
                local_tokenizer_revision = ?8,
                local_tokenizer_file_name = ?9,
                local_huggingface_repo = ?10,
                local_revision = ?11,
                type = ?12
             WHERE id = ?13",
            params![
                model.name,
                api_base_url,
                api_key,
                api_model,
                api_type,
                local_file_name,
                local_tokenizer.as_ref().map(|ts| ts.repo.clone()),
                local_tokenizer.as_ref().map(|ts| ts.revision.clone()),
                local_tokenizer.as_ref().map(|ts| ts.file_name.clone()),
                local_huggingface_repo,
                local_revision,
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

    pub fn export_all_to_json(&self) -> Ad4mDbResult<serde_json::Value> {
        let mut export_data = serde_json::Map::new();

        // Export perspectives
        let perspectives: Vec<serde_json::Value> = self
            .conn
            .prepare("SELECT uuid, name, neighbourhood, shared_url, state, owners FROM perspective_handle")?
            .query_map([], |row| {
                let owners_json: Option<String> = row.get(5)?;
                let owners: Vec<String> = owners_json
                    .and_then(|json| serde_json::from_str(&json).ok())
                    .unwrap_or_default();
                Ok(serde_json::json!({
                    "uuid": row.get::<_, String>(0)?,
                    "name": row.get::<_, Option<String>>(1)?,
                    "neighbourhood": row.get::<_, Option<String>>(2)?,
                    "shared_url": row.get::<_, Option<String>>(3)?,
                    "state": row.get::<_, String>(4)?,
                    "owners": owners
                }))
            })?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "perspectives".to_string(),
            serde_json::to_value(perspectives)?,
        );

        // Export links
        let links: Vec<(LinkSchema, String, String)> = self.conn.prepare(
            "SELECT perspective, source, predicate, target, author, timestamp, signature, key, status FROM link"
        )?.query_map([], |row| {
            Ok((
                LinkSchema {
                    perspective: row.get(0)?,
                    link_expression: serde_json::Value::Null, // Will be constructed from fields
                    source: row.get(1)?,
                    predicate: row.get(2)?,
                    target: row.get(3)?,
                    author: row.get(4)?,
                    timestamp: row.get(5)?,
                    status: row.get(8)?,
                },
                row.get::<_, String>(6)?, // signature
                row.get::<_, String>(7)?, // key
            ))
        })?.collect::<Result<Vec<_>, _>>()?;

        export_data.insert("links".to_string(), serde_json::to_value(links)?);

        // Export expressions
        let expressions: Vec<ExpressionSchema> = self
            .conn
            .prepare("SELECT url, data FROM expression")?
            .query_map([], |row| {
                Ok(ExpressionSchema {
                    url: row.get(0)?,
                    data: serde_json::from_str(&row.get::<_, String>(1)?)
                        .unwrap_or(serde_json::Value::Null),
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "expressions".to_string(),
            serde_json::to_value(expressions)?,
        );

        // Export perspective_diffs
        let diffs: Vec<serde_json::Value> = self.conn.prepare(
            "SELECT perspective, additions, removals, is_pending FROM perspective_diff"
        )?.query_map([], |row| {
            Ok(serde_json::json!({
                "perspective": row.get::<_, String>(0)?,
                "additions": serde_json::from_str::<serde_json::Value>(&row.get::<_, String>(1)?).unwrap_or(serde_json::Value::Null),
                "removals": serde_json::from_str::<serde_json::Value>(&row.get::<_, String>(2)?).unwrap_or(serde_json::Value::Null),
                "is_pending": row.get::<_, bool>(3)?
            }))
        })?.collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "perspective_diffs".to_string(),
            serde_json::to_value(diffs)?,
        );

        // Export notifications
        let notifications: Vec<serde_json::Value> = self.conn.prepare(
            "SELECT id, description, appName, appUrl, appIconPath, trigger, perspective_ids, webhookUrl, webhookAuth, granted FROM notifications"
        )?.query_map([], |row| {
            Ok(serde_json::json!({
                "id": row.get::<_, String>(0)?,
                "description": row.get::<_, String>(1)?,
                "app_name": row.get::<_, String>(2)?,
                "app_url": row.get::<_, String>(3)?,
                "app_icon_path": row.get::<_, String>(4)?,
                "trigger": row.get::<_, String>(5)?,
                "perspective_ids": row.get::<_, String>(6)?,
                "webhook_url": row.get::<_, String>(7)?,
                "webhook_auth": row.get::<_, String>(8)?,
                "granted": row.get::<_, bool>(9)?
            }))
        })?.collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "notifications".to_string(),
            serde_json::to_value(notifications)?,
        );

        // Export tasks
        let tasks: Vec<serde_json::Value> = self.conn.prepare(
            "SELECT id, name, model_id, system_prompt, prompt_examples, metadata, created_at, updated_at FROM tasks"
        )?.query_map([], |row| {
            Ok(serde_json::json!({
                "id": row.get::<_, String>(0)?,
                "name": row.get::<_, String>(1)?,
                "model_id": row.get::<_, String>(2)?,
                "system_prompt": row.get::<_, String>(3)?,
                "prompt_examples": serde_json::from_str::<serde_json::Value>(&row.get::<_, String>(4)?).unwrap_or(serde_json::Value::Null),
                "metadata": row.get::<_, Option<String>>(5)?,
                "created_at": row.get::<_, String>(6)?,
                "updated_at": row.get::<_, String>(7)?
            }))
        })?.collect::<Result<Vec<_>, _>>()?;
        export_data.insert("tasks".to_string(), serde_json::to_value(tasks)?);

        // Export models
        let models: Vec<serde_json::Value> = self.conn.prepare(
            "SELECT id, name, type, api_type, api_key, api_base_url, model, local_file_name, local_huggingface_repo, local_revision, local_tokenizer_repo, local_tokenizer_revision, local_tokenizer_file_name FROM models"
        )?.query_map([], |row| {
            Ok(serde_json::json!({
                "id": row.get::<_, String>(0)?,
                "name": row.get::<_, String>(1)?,
                "model_type": row.get::<_, String>(2)?,
                "api_type": row.get::<_, Option<String>>(3)?,
                "api_key": row.get::<_, Option<String>>(4)?,
                "api_base_url": row.get::<_, Option<String>>(5)?,
                "model": row.get::<_, Option<String>>(6)?,
                "local_file_name": row.get::<_, Option<String>>(7)?,
                "local_huggingface_repo": row.get::<_, Option<String>>(8)?,
                "local_revision": row.get::<_, Option<String>>(9)?,
                "local_tokenizer_repo": row.get::<_, Option<String>>(10)?,
                "local_tokenizer_revision": row.get::<_, Option<String>>(11)?,
                "local_tokenizer_file_name": row.get::<_, Option<String>>(12)?
            }))
        })?.collect::<Result<Vec<_>, _>>()?;
        export_data.insert("models".to_string(), serde_json::to_value(models)?);

        // Export default_models
        let default_models: Vec<serde_json::Value> = self
            .conn
            .prepare("SELECT model_type, model_id FROM default_models")?
            .query_map([], |row| {
                Ok(serde_json::json!({
                    "model_type": row.get::<_, String>(0)?,
                    "model_id": row.get::<_, String>(1)?
                }))
            })?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "default_models".to_string(),
            serde_json::to_value(default_models)?,
        );

        // Nah, we don't need to export model_status
        // It's kinda transient, but in the DB so the model loading percentage is not lost over restarts
        // but doesn't make sense to import it into another instance
        // let model_status: Vec<serde_json::Value> = self
        //     .conn
        //     .prepare("SELECT model, progress, status, downloaded, loaded FROM model_status")?
        //     .query_map([], |row| {
        //         Ok(serde_json::json!({
        //             "model": row.get::<_, String>(0)?,
        //             "progress": row.get::<_, f64>(1)?,
        //             "status": row.get::<_, String>(2)?,
        //             "downloaded": row.get::<_, bool>(3)?,
        //             "loaded": row.get::<_, bool>(4)?
        //         }))
        //     })?
        //     .collect::<Result<Vec<_>, _>>()?;
        // export_data.insert(
        //     "model_status".to_string(),
        //     serde_json::to_value(model_status)?,
        // );

        // Export friends
        let mut stmt = self.conn.prepare("SELECT friend FROM friends")?;
        let friends: Vec<String> = stmt
            .query_map([], |row| row.get(0))?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert("friends".to_string(), serde_json::to_value(friends)?);

        // Export trusted agents
        let mut stmt = self.conn.prepare("SELECT agent FROM trusted_agent")?;
        let agents: Vec<String> = stmt
            .query_map([], |row| row.get(0))?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert("trusted_agents".to_string(), serde_json::to_value(agents)?);

        // Export known link languages
        let mut stmt = self
            .conn
            .prepare("SELECT language FROM known_link_languages")?;
        let languages: Vec<String> = stmt
            .query_map([], |row| row.get(0))?
            .collect::<Result<Vec<_>, _>>()?;
        export_data.insert(
            "known_link_languages".to_string(),
            serde_json::to_value(languages)?,
        );

        Ok(serde_json::Value::Object(export_data))
    }

    pub fn import_from_json(&self, data: serde_json::Value) -> Ad4mDbResult<ImportResult> {
        log::info!("Importing DB data from JSON");
        let data = data
            .as_object()
            .ok_or_else(|| anyhow::anyhow!("Invalid JSON data"))?;

        let mut result = ImportResult::new();

        // Import perspectives first since other items may depend on them
        if let Some(perspectives) = data.get("perspectives") {
            match serde_json::from_value::<Vec<serde_json::Value>>(perspectives.clone()) {
                Ok(perspectives) => {
                    result.perspectives.total = perspectives.len() as i32;
                    log::info!("Importing {} perspectives", perspectives.len());
                    for perspective in perspectives {
                        let name = perspective
                            .get("name")
                            .and_then(|n| n.as_str())
                            .unwrap_or("<unknown>");
                        let uuid = perspective
                            .get("uuid")
                            .and_then(|u| u.as_str())
                            .unwrap_or("<unknown>");
                        let result_status = {
                            let state = if perspective.get("shared_url").is_some() {
                                serde_json::to_string(
                                    &PerspectiveState::NeighbourhoodCreationInitiated,
                                )
                            } else {
                                serde_json::to_string(&PerspectiveState::Private)
                            }
                            .expect("to serialize PerspectiveState");

                            let owners_json =
                                match perspective.get("owners").and_then(|o| o.as_array()) {
                                    Some(arr) => {
                                        let owners: Vec<String> = arr
                                            .iter()
                                            .filter_map(|v| v.as_str().map(|s| s.to_string()))
                                            .collect();
                                        serde_json::to_string(&owners)
                                            .unwrap_or_else(|_| "[]".to_string())
                                    }
                                    None => "[]".to_string(),
                                };

                            self.conn.execute(
                                "INSERT INTO perspective_handle (uuid, name, neighbourhood, shared_url, state, owners) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                                params![
                                    perspective["uuid"].as_str().unwrap_or(Uuid::new_v4().to_string().as_str()),
                                    perspective["name"].as_str(),
                                    perspective.get("neighbourhood").and_then(|n| n.as_str()),
                                    perspective["shared_url"].as_str(),
                                    perspective["state"].as_str().unwrap_or(state.as_str()),
                                    owners_json
                                ],
                            )
                        };

                        match result_status {
                            Ok(_) => {
                                result.perspectives.imported += 1;
                                log::info!("Successfully imported perspective: {} ({})", name, uuid)
                            }
                            Err(e) => {
                                result.perspectives.failed += 1;
                                result.perspectives.errors.push(format!(
                                    "Failed to import perspective {} ({}): {}",
                                    name, uuid, e
                                ));
                                log::warn!(
                                    "Failed to import perspective {} ({}): {}",
                                    name,
                                    uuid,
                                    e
                                )
                            }
                        }
                    }
                }
                Err(e) => {
                    result.perspectives.failed = 1;
                    result
                        .perspectives
                        .errors
                        .push(format!("Failed to parse perspectives: {}", e));
                    log::warn!("Failed to parse perspectives: {}", e)
                }
            }
        }

        // Import links
        if let Some(links) = data.get("links") {
            match serde_json::from_value::<Vec<(LinkSchema, String, String)>>(links.clone()) {
                Ok(links) => {
                    result.links.total = links.len() as i32;
                    log::debug!("Importing {} links", links.len());
                    for (link, signature, key) in links {
                        match self.conn.execute(
                            "INSERT OR IGNORE INTO link (perspective, source, predicate, target, author, timestamp, signature, key, status) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                            params![
                                link.perspective,
                                link.source,
                                link.predicate,
                                link.target,
                                link.author,
                                link.timestamp,
                                signature,
                                key,
                                link.status
                            ],
                        ) {
                            Ok(_) => result.links.imported += 1,
                            Err(e) => {
                                result.links.failed += 1;
                                result.links.errors.push(format!("Failed to import link: {}", e));
                                log::warn!("Failed to import link: {}", e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.links.failed = 1;
                    result
                        .links
                        .errors
                        .push(format!("Failed to parse links: {}", e));
                    log::warn!("Failed to parse links: {}", e)
                }
            }
        }

        // Import expressions
        if let Some(expressions) = data.get("expressions") {
            match serde_json::from_value::<Vec<ExpressionSchema>>(expressions.clone()) {
                Ok(expressions) => {
                    result.expressions.total = expressions.len() as i32;
                    log::debug!("Importing {} expressions", expressions.len());
                    for expr in expressions {
                        match self.conn.execute(
                            "INSERT INTO expression (url, data) VALUES (?1, ?2)",
                            params![expr.url, expr.data.to_string()],
                        ) {
                            Ok(_) => result.expressions.imported += 1,
                            Err(e) => {
                                result.expressions.failed += 1;
                                result.expressions.errors.push(format!(
                                    "Failed to import expression {}: {}",
                                    expr.url, e
                                ));
                                log::warn!("Failed to import expression {}: {}", expr.url, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.expressions.failed = 1;
                    result
                        .expressions
                        .errors
                        .push(format!("Failed to parse expressions: {}", e));
                    log::warn!("Failed to parse expressions: {}", e)
                }
            }
        }

        // Import perspective_diffs
        if let Some(diffs) = data.get("perspective_diffs") {
            match serde_json::from_value::<Vec<serde_json::Value>>(diffs.clone()) {
                Ok(diffs) => {
                    result.perspective_diffs.total = diffs.len() as i32;
                    log::debug!("Importing {} perspective_diffs", diffs.len());
                    for diff in diffs {
                        let perspective_id = diff["perspective"].as_str().unwrap_or("");

                        // Check if perspective exists
                        let perspective_exists = self
                            .conn
                            .query_row(
                                "SELECT 1 FROM perspective_handle WHERE uuid = ?1",
                                params![perspective_id],
                                |_| Ok(true),
                            )
                            .optional()
                            .unwrap_or(Some(false))
                            .unwrap_or(false);

                        if !perspective_exists {
                            result.perspective_diffs.omitted += 1;
                            log::info!("Omitting perspective diff import because perspective {} does not exist", perspective_id);
                            continue;
                        }

                        match self.conn.execute(
                            "INSERT INTO perspective_diff (perspective, additions, removals, is_pending) VALUES (?1, ?2, ?3, ?4)",
                            params![
                                perspective_id,
                                diff["additions"].to_string(),
                                diff["removals"].to_string(),
                                diff["is_pending"].as_bool().unwrap_or(false)
                            ],
                        ) {
                            Ok(_) => result.perspective_diffs.imported += 1,
                            Err(e) => {
                                result.perspective_diffs.failed += 1;
                                result.perspective_diffs.errors.push(format!("Failed to import perspective diff: {}", e));
                                log::warn!("Failed to import perspective diff: {}", e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.perspective_diffs.failed = 1;
                    result
                        .perspective_diffs
                        .errors
                        .push(format!("Failed to parse perspective diffs: {}", e));
                    log::warn!("Failed to parse perspective diffs: {}", e)
                }
            }
        }

        // Import notifications
        if let Some(notifications) = data.get("notifications") {
            match serde_json::from_value::<Vec<serde_json::Value>>(notifications.clone()) {
                Ok(notifications) => {
                    result.notifications.total = notifications.len() as i32;
                    log::debug!("Importing {} notifications", notifications.len());
                    for notification in notifications {
                        let id = notification
                            .get("id")
                            .and_then(|id| id.as_str())
                            .unwrap_or("<unknown>");
                        match self.conn.execute(
                            "INSERT INTO notifications (id, description, appName, appUrl, appIconPath, trigger, perspective_ids, webhookUrl, webhookAuth, granted) 
                             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
                            params![
                                notification["id"].as_str().unwrap_or(""),
                                notification["description"].as_str().unwrap_or(""),
                                notification["app_name"].as_str().unwrap_or(""),
                                notification["app_url"].as_str().unwrap_or(""),
                                notification["app_icon_path"].as_str().unwrap_or(""),
                                notification["trigger"].as_str().unwrap_or(""),
                                notification["perspective_ids"].as_str().unwrap_or(""),
                                notification["webhook_url"].as_str().unwrap_or(""),
                                notification["webhook_auth"].as_str().unwrap_or(""),
                                notification["granted"].as_bool().unwrap_or(false)
                            ],
                        ) {
                            Ok(_) => result.notifications.imported += 1,
                            Err(e) => {
                                result.notifications.failed += 1;
                                result.notifications.errors.push(format!("Failed to import notification {}: {}", id, e));
                                log::warn!("Failed to import notification {}: {}", id, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.notifications.failed = 1;
                    result
                        .notifications
                        .errors
                        .push(format!("Failed to parse notifications: {}", e));
                    log::warn!("Failed to parse notifications: {}", e)
                }
            }
        }

        // Import models
        if let Some(models) = data.get("models") {
            match serde_json::from_value::<Vec<serde_json::Value>>(models.clone()) {
                Ok(models) => {
                    result.models.total = models.len() as i32;
                    log::debug!("Importing {} models", models.len());
                    for model in models {
                        let name = model
                            .get("name")
                            .and_then(|n| n.as_str())
                            .unwrap_or("<unknown>");
                        match self.conn.execute(
                            "INSERT INTO models (id, name, type, api_type, api_key, api_base_url, model, local_file_name, local_huggingface_repo, local_revision, local_tokenizer_repo, local_tokenizer_revision, local_tokenizer_file_name) 
                             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)",
                            params![
                                model["id"].as_str().unwrap_or(""),
                                model["name"].as_str().unwrap_or(""),
                                model["model_type"].as_str().unwrap_or(""),
                                model["api_type"].as_str(),
                                model["api_key"].as_str(),
                                model["api_base_url"].as_str(),
                                model["model"].as_str(),
                                model["local_file_name"].as_str(),
                                model["local_huggingface_repo"].as_str(),
                                model["local_revision"].as_str(),
                                model["local_tokenizer_repo"].as_str(),
                                model["local_tokenizer_revision"].as_str(),
                                model["local_tokenizer_file_name"].as_str()
                            ],
                        ) {
                            Ok(_) => result.models.imported += 1,
                            Err(e) => {
                                result.models.failed += 1;
                                result.models.errors.push(format!("Failed to import model {}: {}", name, e));
                                log::warn!("Failed to import model {}: {}", name, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.models.failed = 1;
                    result
                        .models
                        .errors
                        .push(format!("Failed to parse models: {}", e));
                    log::warn!("Failed to parse models: {}", e)
                }
            }
        }

        // Import default_models
        if let Some(default_models) = data.get("default_models") {
            match serde_json::from_value::<Vec<serde_json::Value>>(default_models.clone()) {
                Ok(default_models) => {
                    result.default_models.total = default_models.len() as i32;
                    log::debug!("Importing {} default model mappings", default_models.len());
                    for default_model in default_models {
                        let model_type =
                            default_model["model_type"].as_str().unwrap_or("<unknown>");
                        match self.conn.execute(
                            "INSERT INTO default_models (model_type, model_id) VALUES (?1, ?2)",
                            params![
                                default_model["model_type"].as_str().unwrap_or(""),
                                default_model["model_id"].as_str().unwrap_or("")
                            ],
                        ) {
                            Ok(_) => result.default_models.imported += 1,
                            Err(e) => {
                                result.default_models.failed += 1;
                                result.default_models.errors.push(format!(
                                    "Failed to import default model mapping for type {}: {}",
                                    model_type, e
                                ));
                                log::warn!(
                                    "Failed to import default model mapping for type {}: {}",
                                    model_type,
                                    e
                                )
                            }
                        }
                    }
                }
                Err(e) => {
                    result.default_models.failed = 1;
                    result
                        .default_models
                        .errors
                        .push(format!("Failed to parse default models: {}", e));
                    log::warn!("Failed to parse default models: {}", e)
                }
            }
        }

        // Import tasks
        if let Some(tasks) = data.get("tasks") {
            match serde_json::from_value::<Vec<serde_json::Value>>(tasks.clone()) {
                Ok(tasks) => {
                    result.tasks.total = tasks.len() as i32;
                    log::debug!("Importing {} tasks", tasks.len());
                    for task in tasks {
                        let name = task
                            .get("name")
                            .and_then(|n| n.as_str())
                            .unwrap_or("<unknown>");
                        match self.conn.execute(
                            "INSERT OR REPLACE INTO tasks (id, name, model_id, system_prompt, prompt_examples, metadata, created_at, updated_at) 
                                VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                            params![
                                task["id"].as_str().unwrap_or(""),
                                task["name"].as_str().unwrap_or(""),
                                task["model_id"].as_str().unwrap_or(""),
                                task["system_prompt"].as_str().unwrap_or(""),
                                task["prompt_examples"].to_string(),
                                task["metadata"].as_str(),
                                task["created_at"].as_str().unwrap_or(""),
                                task["updated_at"].as_str().unwrap_or("")
                            ],
                        ) {
                            Ok(_) => result.tasks.imported += 1,
                            Err(e) => {
                                result.tasks.failed += 1;
                                result.tasks.errors.push(format!("Failed to import task {}: {}", name, e));
                                log::warn!("Failed to import task {}: {}", name, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.tasks.failed = 1;
                    result
                        .tasks
                        .errors
                        .push(format!("Failed to parse tasks: {}", e));
                    log::warn!("Failed to parse tasks: {}", e)
                }
            }
        }

        // Don't need model_status, see above in export
        // if let Some(model_status) = data.get("model_status") {
        //     let model_status: Vec<serde_json::Value> =
        //         serde_json::from_value(model_status.clone())?;
        //     log::debug!("Importing {} model_status", model_status.len());
        //     for status in model_status {
        //         self.conn.execute(
        //             "INSERT INTO model_status (model, progress, status, downloaded, loaded) VALUES (?1, ?2, ?3, ?4, ?5)",
        //             params![
        //                 status["model"].as_str().unwrap_or(""),
        //                 status["progress"].as_f64().unwrap_or(0.0),
        //                 status["status"].as_str().unwrap_or(""),
        //                 status["downloaded"].as_bool().unwrap_or(false),
        //                 status["loaded"].as_bool().unwrap_or(false)
        //             ],
        //         )?;
        //     }
        // }

        // Import friends
        if let Some(friends) = data.get("friends") {
            match serde_json::from_value::<Vec<String>>(friends.clone()) {
                Ok(friends) => {
                    result.friends.total = friends.len() as i32;
                    log::debug!("Importing {} friends", friends.len());
                    for friend in friends {
                        match self
                            .conn
                            .execute("INSERT INTO friends (friend) VALUES (?1)", params![friend])
                        {
                            Ok(_) => result.friends.imported += 1,
                            Err(e) => {
                                result.friends.failed += 1;
                                result
                                    .friends
                                    .errors
                                    .push(format!("Failed to import friend {}: {}", friend, e));
                                log::warn!("Failed to import friend {}: {}", friend, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.friends.failed = 1;
                    result
                        .friends
                        .errors
                        .push(format!("Failed to parse friends: {}", e));
                    log::warn!("Failed to parse friends: {}", e)
                }
            }
        }

        // Import trusted agents
        if let Some(agents) = data.get("trusted_agents") {
            match serde_json::from_value::<Vec<String>>(agents.clone()) {
                Ok(agents) => {
                    result.trusted_agents.total = agents.len() as i32;
                    log::debug!("Importing {} trusted agents", agents.len());
                    for agent in agents {
                        match self.conn.execute(
                            "INSERT INTO trusted_agent (agent) VALUES (?1)",
                            params![agent],
                        ) {
                            Ok(_) => result.trusted_agents.imported += 1,
                            Err(e) => {
                                result.trusted_agents.failed += 1;
                                result.trusted_agents.errors.push(format!(
                                    "Failed to import trusted agent {}: {}",
                                    agent, e
                                ));
                                log::warn!("Failed to import trusted agent {}: {}", agent, e)
                            }
                        }
                    }
                }
                Err(e) => {
                    result.trusted_agents.failed = 1;
                    result
                        .trusted_agents
                        .errors
                        .push(format!("Failed to parse trusted agents: {}", e));
                    log::warn!("Failed to parse trusted agents: {}", e)
                }
            }
        }

        // Import known link languages
        if let Some(languages) = data.get("known_link_languages") {
            match serde_json::from_value::<Vec<String>>(languages.clone()) {
                Ok(languages) => {
                    result.known_link_languages.total = languages.len() as i32;
                    log::debug!("Importing {} known link languages", languages.len());
                    for language in languages {
                        match self.conn.execute(
                            "INSERT INTO known_link_languages (language) VALUES (?1)",
                            params![language],
                        ) {
                            Ok(_) => result.known_link_languages.imported += 1,
                            Err(e) => {
                                result.known_link_languages.failed += 1;
                                result.known_link_languages.errors.push(format!(
                                    "Failed to import known link language {}: {}",
                                    language, e
                                ));
                                log::warn!(
                                    "Failed to import known link language {}: {}",
                                    language,
                                    e
                                )
                            }
                        }
                    }
                }
                Err(e) => {
                    result.known_link_languages.failed = 1;
                    result
                        .known_link_languages
                        .errors
                        .push(format!("Failed to parse known link languages: {}", e));
                    log::warn!("Failed to parse known link languages: {}", e)
                }
            }
        }

        Ok(result)
    }

    // Password hashing and verification helpers
    fn hash_password(password: &str) -> Ad4mDbResult<String> {
        let salt = SaltString::generate(&mut OsRng);
        let argon2 = Argon2::default();
        let password_hash = argon2
            .hash_password(password.as_bytes(), &salt)
            .map_err(|e| anyhow!("Failed to hash password: {}", e))?
            .to_string();
        Ok(password_hash)
    }

    fn verify_password(password: &str, password_hash: &str) -> Ad4mDbResult<bool> {
        let parsed_hash = PasswordHash::new(password_hash)
            .map_err(|e| anyhow!("Failed to parse password hash: {}", e))?;
        let argon2 = Argon2::default();
        Ok(argon2
            .verify_password(password.as_bytes(), &parsed_hash)
            .is_ok())
    }

    // User management functions
    pub fn add_user(&self, username: &str, did: &str, password: &str) -> Ad4mDbResult<()> {
        let password_hash = Self::hash_password(password)?;
        self.conn.execute(
            "INSERT INTO users (username, did, password_hash) VALUES (?1, ?2, ?3)",
            params![username, did, password_hash],
        )?;
        Ok(())
    }

    // Internal function to get user with password_hash - only for internal use
    fn get_user_internal(&self, username: &str) -> Ad4mDbResult<User> {
        let mut stmt = self.conn.prepare(
            "SELECT username, did, password_hash, last_seen FROM users WHERE username = ?1",
        )?;
        let user = stmt.query_row([username], |row| {
            Ok(User {
                username: row.get(0)?,
                did: row.get(1)?,
                password_hash: row.get(2)?,
                last_seen: row.get(3).ok(),
            })
        })?;
        Ok(user)
    }

    // Public function to get user without password_hash
    pub fn get_user(&self, username: &str) -> Ad4mDbResult<UserInfo> {
        let mut stmt = self
            .conn
            .prepare("SELECT username, did, last_seen FROM users WHERE username = ?1")?;
        let user = stmt.query_row([username], |row| {
            Ok(UserInfo {
                username: row.get(0)?,
                did: row.get(1)?,
                last_seen: row.get(2).ok(),
            })
        })?;
        Ok(user)
    }

    pub fn update_user_last_seen(&self, email: &str) -> Ad4mDbResult<()> {
        let timestamp = chrono::Utc::now().timestamp();
        self.conn.execute(
            "UPDATE users SET last_seen = ?1 WHERE username = ?2",
            params![timestamp, email],
        )?;
        Ok(())
    }

    pub fn list_users(&self) -> Ad4mDbResult<Vec<UserInfo>> {
        let mut stmt = self.conn.prepare(
            "SELECT username, did, last_seen FROM users ORDER BY last_seen DESC NULLS LAST",
        )?;

        let users = stmt
            .query_map([], |row| {
                Ok(UserInfo {
                    username: row.get(0)?,
                    did: row.get(1)?,
                    last_seen: row.get(2).ok(),
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;

        Ok(users)
    }

    // Verify user password against stored hash
    // Uses internal function to access password_hash
    pub fn verify_user_password(&self, username: &str, password: &str) -> Ad4mDbResult<bool> {
        let user = self.get_user_internal(username)?;
        Self::verify_password(password, &user.password_hash)
    }

    // Settings management functions
    pub fn get_setting(&self, key: &str) -> Ad4mDbResult<Option<String>> {
        let result = self
            .conn
            .query_row("SELECT value FROM settings WHERE key = ?1", [key], |row| {
                row.get(0)
            })
            .optional()?;
        Ok(result)
    }

    pub fn set_setting(&self, key: &str, value: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO settings (key, value) VALUES (?1, ?2)
             ON CONFLICT(key) DO UPDATE SET value = excluded.value",
            params![key, value],
        )?;
        Ok(())
    }

    pub fn get_multi_user_enabled(&self) -> Ad4mDbResult<bool> {
        match self.get_setting("multi_user_enabled")? {
            Some(value) => Ok(value == "true"),
            None => Ok(false), // Default to disabled
        }
    }

    pub fn set_multi_user_enabled(&self, enabled: bool) -> Ad4mDbResult<()> {
        self.set_setting("multi_user_enabled", if enabled { "true" } else { "false" })
    }

    // Email verification functions

    /// Generates a 6-digit verification code, stores its SHA-256 hash, and returns the plaintext code
    pub fn create_verification_code(
        &self,
        email: &str,
        verification_type: &str,
    ) -> Ad4mDbResult<String> {
        // Generate random 6-digit code
        let code = rand::thread_rng().gen_range(100000..1000000).to_string();

        // Hash the code
        let mut hasher = Sha256::new();
        hasher.update(code.as_bytes());
        let code_hash = format!("{:x}", hasher.finalize());

        // Get current timestamp
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;
        let expires_at = now + (15 * 60); // 15 minutes

        // Delete any existing verification for this email+type combination
        self.conn.execute(
            "DELETE FROM email_verifications WHERE email = ?1 AND verification_type = ?2",
            params![email, verification_type],
        )?;

        // Insert new verification (initialize failed_attempts to 0)
        self.conn.execute(
            "INSERT INTO email_verifications (email, code_hash, created_at, expires_at, verified, verification_type, failed_attempts)
             VALUES (?1, ?2, ?3, ?4, 0, ?5, 0)",
            params![email, code_hash, now, expires_at, verification_type],
        )?;

        Ok(code)
    }

    /// Verifies a code against the stored hash for a given email and verification type
    /// Returns Ok(true) on success, Ok(false) on failure, or Err if code is invalidated due to too many attempts
    pub fn verify_code(
        &self,
        email: &str,
        code: &str,
        verification_type: &str,
    ) -> Ad4mDbResult<bool> {
        // Maximum allowed failed attempts before invalidating the code
        const MAX_FAILED_ATTEMPTS: i32 = 5;

        // Hash the provided code
        let mut hasher = Sha256::new();
        hasher.update(code.as_bytes());
        let code_hash = format!("{:x}", hasher.finalize());

        // Get current timestamp
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        // Query for matching verification that hasn't expired, including failed_attempts
        // Use COALESCE to handle NULL values for existing rows created before the migration
        let result: Result<(String, i64, i32), _> = self.conn.query_row(
            "SELECT code_hash, expires_at, COALESCE(failed_attempts, 0) FROM email_verifications
             WHERE email = ?1 AND verification_type = ?2 AND verified = 0",
            params![email, verification_type],
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        match result {
            Ok((stored_hash, expires_at, failed_attempts)) => {
                // Check if code has been invalidated due to too many failed attempts
                if failed_attempts >= MAX_FAILED_ATTEMPTS {
                    // Delete the invalidated code
                    self.conn.execute(
                        "DELETE FROM email_verifications WHERE email = ?1 AND verification_type = ?2",
                        params![email, verification_type],
                    )?;
                    return Err(anyhow!(
                        "Verification code has been invalidated due to too many failed attempts. Please request a new code."
                    ));
                }

                // Check if expired
                if now > expires_at {
                    // Clean up expired code
                    self.conn.execute(
                        "DELETE FROM email_verifications WHERE email = ?1 AND verification_type = ?2",
                        params![email, verification_type],
                    )?;
                    return Ok(false);
                }

                // Constant-time comparison to prevent timing attacks
                if constant_time_eq(&stored_hash, &code_hash) {
                    // Mark as verified and delete
                    self.conn.execute(
                        "DELETE FROM email_verifications WHERE email = ?1 AND verification_type = ?2",
                        params![email, verification_type],
                    )?;
                    Ok(true)
                } else {
                    // Increment failed attempts counter
                    let new_failed_attempts = failed_attempts + 1;
                    self.conn.execute(
                        "UPDATE email_verifications SET failed_attempts = ?1 WHERE email = ?2 AND verification_type = ?3",
                        params![new_failed_attempts, email, verification_type],
                    )?;

                    // If we've reached the max attempts, invalidate the code
                    if new_failed_attempts >= MAX_FAILED_ATTEMPTS {
                        self.conn.execute(
                            "DELETE FROM email_verifications WHERE email = ?1 AND verification_type = ?2",
                            params![email, verification_type],
                        )?;
                        return Err(anyhow!(
                            "Verification code has been invalidated due to too many failed attempts. Please request a new code."
                        ));
                    }

                    Ok(false)
                }
            }
            Err(_) => Ok(false),
        }
    }

    /// Removes all expired verification codes
    pub fn cleanup_expired_codes(&self) -> Ad4mDbResult<()> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        self.conn.execute(
            "DELETE FROM email_verifications WHERE expires_at < ?1",
            params![now],
        )?;

        Ok(())
    }

    /// Check if a verification code exists for the given email (for any verification type)
    /// Returns true if an active (non-expired, non-verified) code exists
    pub fn has_verification_code(&self, email: &str) -> Ad4mDbResult<bool> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let result: Result<i32, _> = self.conn.query_row(
            "SELECT COUNT(*) FROM email_verifications
             WHERE email = ?1 AND verified = 0 AND expires_at > ?2",
            params![email, now],
            |row| row.get(0),
        );

        match result {
            Ok(count) => Ok(count > 0),
            Err(_) => Ok(false),
        }
    }

    /// Checks if an email is rate-limited (returns error if rate-limited)
    pub fn check_rate_limit(&self, email: &str) -> Ad4mDbResult<()> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let result: Result<i64, _> = self.conn.query_row(
            "SELECT last_sent_at FROM email_rate_limits WHERE email = ?1",
            params![email],
            |row| row.get(0),
        );

        match result {
            Ok(last_sent_at) => {
                let seconds_since_last = now - last_sent_at;
                if seconds_since_last < 60 {
                    return Err(anyhow!(
                        "Please wait {} seconds before requesting another verification code",
                        60 - seconds_since_last
                    ));
                }
                Ok(())
            }
            Err(_) => Ok(()), // No rate limit entry exists yet
        }
    }

    /// Updates the rate limit timestamp for an email
    pub fn update_rate_limit(&self, email: &str) -> Ad4mDbResult<()> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        self.conn.execute(
            "INSERT INTO email_rate_limits (email, last_sent_at) VALUES (?1, ?2)
             ON CONFLICT(email) DO UPDATE SET last_sent_at = excluded.last_sent_at",
            params![email, now],
        )?;

        Ok(())
    }

    /// Test helper: Set expiry time for a verification code (for testing expiration)
    pub fn set_verification_code_expiry(
        &self,
        email: &str,
        verification_type: &str,
        expires_at: i64,
    ) -> Ad4mDbResult<()> {
        let rows_affected = self.conn.execute(
            "UPDATE email_verifications SET expires_at = ?1 WHERE email = ?2 AND verification_type = ?3",
            params![expires_at, email, verification_type],
        )?;

        if rows_affected == 0 {
            return Err(anyhow!(
                "No verification code found for email {} and type {}",
                email,
                verification_type
            ));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphql::graphql_types::{PerspectiveHandle, PerspectiveState};
    use crate::{
        graphql::graphql_types::{LocalModelInput, ModelApiInput, TokenizerSourceInput},
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
    fn test_export_import_all_tables() {
        use crate::graphql::graphql_types::{
            DecoratedNeighbourhoodExpression, Neighbourhood, Perspective, PerspectiveState,
        };
        use crate::types::DecoratedExpressionProof;

        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // 1. Add test data to all tables
        // Add trusted agents
        db.add_trusted_agents(vec!["test-agent-1".to_string(), "test-agent-2".to_string()])
            .unwrap();

        // Add friends
        db.add_friends(vec![
            "test-friend-1".to_string(),
            "test-friend-2".to_string(),
        ])
        .unwrap();

        // Add known link languages
        db.add_known_link_languages(vec!["test-lang-1".to_string(), "test-lang-2".to_string()])
            .unwrap();

        // Create a test neighbourhood expression
        let test_neighbourhood = DecoratedNeighbourhoodExpression {
            author: "test-author".to_string(),
            timestamp: "test-timestamp".to_string(),
            data: Neighbourhood {
                link_language: "test-link-language".to_string(),
                meta: Perspective::default(),
            },
            proof: DecoratedExpressionProof {
                signature: "test-signature".to_string(),
                key: "test-key".to_string(),
                valid: None,
                invalid: None,
            },
        };

        // Add perspectives with links
        let perspective1 = PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("Test Perspective 1".to_string()),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
            owners: None,
        };
        let perspective2 = PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("Test Perspective 2".to_string()),
            neighbourhood: Some(test_neighbourhood),
            shared_url: Some("test-shared-url".to_string()),
            state: PerspectiveState::Synced,
            owners: None,
        };

        db.add_perspective(&perspective1).unwrap();
        db.add_perspective(&perspective2).unwrap();

        // Add some links to the perspectives
        let link1 = construct_dummy_link_expression(LinkStatus::Shared);
        let link2 = construct_dummy_link_expression(LinkStatus::Local);

        db.add_link(&perspective1.uuid, &link1, &LinkStatus::Shared)
            .unwrap();
        db.add_link(&perspective2.uuid, &link2, &LinkStatus::Local)
            .unwrap();

        // Add notifications
        let notification = NotificationInput {
            description: "Test Description".to_string(),
            app_name: "Test App".to_string(),
            app_url: "http://test.app".to_string(),
            app_icon_path: "/test/icon.png".to_string(),
            trigger: "test-trigger".to_string(),
            perspective_ids: vec![perspective1.uuid.clone()],
            webhook_url: "http://test.webhook".to_string(),
            webhook_auth: "test-auth".to_string(),
        };
        let notification_id = db.add_notification(notification).unwrap();

        // Add tasks
        let task = AIPromptExamples {
            input: "test input".to_string(),
            output: "test output".to_string(),
        };
        let task_id = db
            .add_task(
                "Test Task".to_string(),
                "test-model".to_string(),
                "test system prompt".to_string(),
                vec![task],
                Some("test metadata".to_string()),
            )
            .unwrap();

        // Add models
        let model_input = ModelInput {
            name: "Test Model".to_string(),
            model_type: ModelType::Llm,
            api: Some(ModelApiInput {
                base_url: "https://api.example.com".to_string(),
                api_key: "test-key".to_string(),
                model: "gpt-4".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
        };
        let model_id = db.add_model(&model_input).unwrap();

        // Set default model
        db.set_default_model(ModelType::Llm, &model_id).unwrap();

        // 2. Export all data
        let exported_data = db.export_all_to_json().unwrap();

        //println!("exported_data: {}", serde_json::to_string_pretty(&exported_data).unwrap());

        // 3. Create a new database for import
        let import_db = Ad4mDb::new(":memory:").unwrap();

        // 4. Import the data
        import_db.import_from_json(exported_data).unwrap();

        // 5. Verify imported data
        // Verify trusted agents
        let trusted_agents = import_db.get_all_trusted_agents().unwrap();
        assert_eq!(trusted_agents, vec!["test-agent-1", "test-agent-2"]);

        // Verify friends
        let friends = import_db.get_all_friends().unwrap();
        assert_eq!(friends, vec!["test-friend-1", "test-friend-2"]);

        // Verify known link languages
        let languages = import_db.get_all_known_link_languages().unwrap();
        assert_eq!(languages, vec!["test-lang-1", "test-lang-2"]);

        // Verify notifications
        let notifications = import_db.get_notifications().unwrap();
        assert_eq!(notifications.len(), 1);
        let imported_notification = notifications.first().unwrap();
        assert_eq!(imported_notification.id, notification_id);
        assert_eq!(imported_notification.app_name, "Test App");

        // Verify tasks
        let tasks = import_db.get_tasks().unwrap();
        assert_eq!(tasks.len(), 1);
        let imported_task = tasks.first().unwrap();
        assert_eq!(imported_task.task_id, task_id);
        assert_eq!(imported_task.name, "Test Task");

        // Verify models
        let models = import_db.get_models().unwrap();
        assert_eq!(models.len(), 1);
        let imported_model = models.first().unwrap();
        assert_eq!(imported_model.id, model_id);
        assert_eq!(imported_model.name, "Test Model");

        // Verify default model mapping was imported
        let imported_default_model = import_db.get_default_model(ModelType::Llm).unwrap();
        assert_eq!(imported_default_model, Some(model_id));

        // Verify perspectives
        let imported_perspectives = import_db.get_all_perspectives().unwrap();
        assert_eq!(imported_perspectives.len(), 2);

        // Find and verify first perspective
        let imported_perspective1 = imported_perspectives
            .iter()
            .find(|p| p.name == Some("Test Perspective 1".to_string()))
            .unwrap();
        assert_eq!(imported_perspective1.uuid, perspective1.uuid);
        assert_eq!(imported_perspective1.shared_url, None);
        assert_eq!(imported_perspective1.state, PerspectiveState::Private);

        // Find and verify second perspective
        let imported_perspective2 = imported_perspectives
            .iter()
            .find(|p| p.name == Some("Test Perspective 2".to_string()))
            .unwrap();
        assert_eq!(imported_perspective2.uuid, perspective2.uuid);
        assert_eq!(
            imported_perspective2
                .neighbourhood
                .as_ref()
                .unwrap()
                .data
                .link_language,
            "test-link-language"
        );
        assert_eq!(
            imported_perspective2.shared_url,
            Some("test-shared-url".to_string())
        );
        assert_eq!(imported_perspective2.state, PerspectiveState::Synced);

        // Verify perspective links
        let imported_links1 = import_db.get_all_links(&perspective1.uuid).unwrap();
        assert_eq!(imported_links1.len(), 1);
        assert_eq!(imported_links1[0], (link1, LinkStatus::Shared));

        let imported_links2 = import_db.get_all_links(&perspective2.uuid).unwrap();
        assert_eq!(imported_links2.len(), 1);
        assert_eq!(imported_links2[0], (link2, LinkStatus::Local));
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
                model: "llama3".to_string(),
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
                tokenizer_source: Some(TokenizerSourceInput {
                    repo: "test_repo".to_string(),
                    revision: "main".to_string(),
                    file_name: "tokenizer.json".to_string(),
                }),
                huggingface_repo: Some("huggingface/test".to_string()),
                revision: Some("main".to_string()),
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
        assert_eq!(retrieved_model_api.api.as_ref().unwrap().model, "llama3");
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
                .tokenizer_source
                .as_ref()
                .unwrap()
                .repo,
            "test_repo"
        );
        assert_eq!(
            retrieved_model_local
                .local
                .as_ref()
                .unwrap()
                .huggingface_repo
                .as_ref()
                .unwrap(),
            "huggingface/test"
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
                model: "llama3".to_string(),
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
            api: Some(ModelApiInput {
                base_url: "https://api.example.org".to_string(),
                api_key: "test_key".to_string(),
                model: "llama4".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Embedding,
        };

        // Update the model
        db.update_model(&model_id, &updated_model).unwrap();

        // Retrieve and verify the updated model
        let retrieved_model = db.get_model(model_id.clone()).unwrap().unwrap();

        assert_eq!(retrieved_model.name, "Updated Model");
        assert!(retrieved_model.api.is_some());
        assert!(retrieved_model.local.is_none());

        let api = retrieved_model.api.unwrap();
        assert_eq!(api.base_url, Url::parse("https://api.example.org").unwrap());
        assert_eq!(api.api_key, "test_key");
        assert_eq!(api.model, "llama4");
        assert_eq!(api.api_type, ModelApiType::OpenAi);

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
                model: "llama".to_string(),
                api_type: ModelApiType::OpenAi.to_string(),
            }),
            local: None,
            model_type: ModelType::Llm,
        };

        let model_embedding = ModelInput {
            name: "Test Embedding Model".to_string(),
            local: Some(LocalModelInput {
                file_name: "embedding.bin".to_string(),
                tokenizer_source: Some(TokenizerSourceInput {
                    repo: "test_repo".to_string(),
                    revision: "main".to_string(),
                    file_name: "tokenizer.json".to_string(),
                }),
                huggingface_repo: Some("huggingface/test".to_string()),
                revision: Some("main".to_string()),
            }),
            api: None,
            model_type: ModelType::Embedding,
        };

        let model_transcription = ModelInput {
            name: "Test Transcription Model".to_string(),
            api: Some(ModelApiInput {
                base_url: "https://api.transcribe.com".to_string(),
                api_key: "transcribe_key".to_string(),
                model: "llama".to_string(),
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
                model: "llama".to_string(),
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
                tokenizer_source: Some(TokenizerSourceInput {
                    repo: "test_repo".to_string(),
                    revision: "main".to_string(),
                    file_name: "tokenizer.json".to_string(),
                }),
                huggingface_repo: Some("huggingface/test".to_string()),
                revision: Some("main".to_string()),
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

    #[test]
    fn can_get_pending_diffs_by_size() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();

        // Create 10 diffs with large content
        for _ in 0..10 {
            let diff = PerspectiveDiff {
                additions: vec![
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                ],
                removals: vec![
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                    construct_dummy_link_expression(LinkStatus::Shared),
                ],
            };
            db.add_pending_diff(&p_uuid, &diff).unwrap();
        }

        // Get all diffs first to calculate total size
        let (all_diffs, all_ids) = db.get_pending_diffs(&p_uuid, None).unwrap();
        assert_eq!(all_ids.len(), 10);

        let total_serialized = serde_json::to_string(&all_diffs).unwrap();
        let total_size = total_serialized.len();

        // Use half of total size as limit - should get roughly half the diffs
        let half_size = total_size / 2;
        let (half_diffs, half_ids) = db
            .get_pending_diffs_by_size(&p_uuid, half_size, Some(10))
            .unwrap();

        // Verify we got fewer diffs than total
        assert!(half_ids.len() < all_ids.len());

        // Verify the serialized size is under our limit
        let half_serialized = serde_json::to_string(&half_diffs).unwrap();
        assert!(half_serialized.len() <= half_size || half_ids.len() == 1);

        // Test with size bigger than total - should get all diffs
        let (full_diffs, full_ids) = db
            .get_pending_diffs_by_size(&p_uuid, total_size * 2, Some(10))
            .unwrap();
        assert_eq!(full_ids.len(), 10);
        assert_eq!(full_diffs.additions.len(), 50); // 10 diffs * 5 additions
        assert_eq!(full_diffs.removals.len(), 50); // 10 diffs * 5 removals
    }

    #[test]
    fn test_user_management() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Test user creation
        let test_username = "test@example.com";
        let test_did = "did:key:z6MkiTBz1ymuepAQ4HEHYSF1H8quG5GLVVQR3djdX3mDooWp";
        let test_password = "testpassword123";

        // Add user should succeed
        let add_result = db.add_user(test_username, test_did, test_password);
        assert!(
            add_result.is_ok(),
            "Failed to add user: {:?}",
            add_result.err()
        );

        // Get user should return the same user (without password_hash)
        let retrieved_user = db.get_user(test_username).unwrap();
        assert_eq!(retrieved_user.username, test_username);
        assert_eq!(retrieved_user.did, test_did);
        // Password verification should work (tests that password is properly hashed)

        // Verify password should work
        assert!(db
            .verify_user_password(test_username, test_password)
            .unwrap());
        assert!(!db
            .verify_user_password(test_username, "wrongpassword")
            .unwrap());

        // Test duplicate user creation should fail
        let duplicate_result = db.add_user(test_username, test_did, test_password);
        assert!(
            duplicate_result.is_err(),
            "Duplicate user creation should fail"
        );

        // Test getting non-existent user should fail
        let non_existent_result = db.get_user("nonexistent@example.com");
        assert!(
            non_existent_result.is_err(),
            "Getting non-existent user should fail"
        );

        println!("✅ User management tests passed");
    }

    #[test]
    fn test_multiple_users() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create multiple test users
        let user1_username = "user1@example.com";
        let user1_did = "did:key:z6MkiTBz1ymuepAQ4HEHYSF1H8quG5GLVVQR3djdX3mDooWp";
        let user1_password = "password1";

        let user2_username = "user2@example.com";
        let user2_did = "did:key:z6MkjRagNiMu4CWTaH5SBDeKHyYoPP2ooXqPoy3GmASHLtF6";
        let user2_password = "password2";

        // Add both users
        db.add_user(user1_username, user1_did, user1_password)
            .unwrap();
        db.add_user(user2_username, user2_did, user2_password)
            .unwrap();

        // Verify both users can be retrieved independently
        let retrieved_user1 = db.get_user(user1_username).unwrap();
        let retrieved_user2 = db.get_user(user2_username).unwrap();

        assert_eq!(retrieved_user1.username, user1_username);
        assert_eq!(retrieved_user1.did, user1_did);
        // Verify password hashing works
        assert!(db
            .verify_user_password(user1_username, user1_password)
            .unwrap());

        assert_eq!(retrieved_user2.username, user2_username);
        assert_eq!(retrieved_user2.did, user2_did);
        // Verify password hashing works
        assert!(db
            .verify_user_password(user2_username, user2_password)
            .unwrap());

        // Verify users have different DIDs
        assert_ne!(retrieved_user1.did, retrieved_user2.did);

        println!("✅ Multiple users management tests passed");
    }

    #[test]
    fn test_user_table_schema() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Verify the users table was created by trying to query it
        let result = db
            .conn
            .prepare("SELECT name FROM sqlite_master WHERE type='table' AND name='users'");
        assert!(result.is_ok(), "Users table should exist");

        let mut stmt = result.unwrap();
        let table_exists = stmt.query_row([], |row| {
            let table_name: String = row.get(0)?;
            Ok(table_name == "users")
        });

        assert!(
            table_exists.is_ok() && table_exists.unwrap(),
            "Users table should exist and be named 'users'"
        );

        // Verify table schema by checking column info
        let schema_result = db.conn.prepare("PRAGMA table_info(users)");
        assert!(
            schema_result.is_ok(),
            "Should be able to get users table schema"
        );

        println!("✅ User table schema tests passed");
    }

    #[test]
    fn test_user_password_handling() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Test user with various password formats
        let users = vec![
            (
                "user_simple@example.com",
                "did:key:z6MkiTBz1ymuepAQ4HEHYSF1H8quG5GLVVQR3djdX3mDooWp",
                "simple",
            ),
            (
                "user_complex@example.com",
                "did:key:z6MkjRagNiMu4CWTaH5SBDeKHyYoPP2ooXqPoy3GmASHLtF6",
                "Complex!Password123@#$",
            ),
            (
                "user_unicode@example.com",
                "did:key:z6MkrJVnaZjsXc2WrVjsXc2WrVjsXc2WrVjsXc2WrVjsXc2W",
                "пароль🔐密码",
            ),
        ];

        // Add all users
        for (username, did, password) in &users {
            let result = db.add_user(username, did, password);
            assert!(
                result.is_ok(),
                "Failed to add user {}: {:?}",
                username,
                result.err()
            );
        }

        // Verify all users can be retrieved and passwords can be verified
        for (username, _did, password) in &users {
            let retrieved = db.get_user(username).unwrap();
            // Verify user info is correct
            assert_eq!(
                retrieved.username, *username,
                "Username should match for {}",
                username
            );

            // Password verification should work (tests that password is properly hashed)
            assert!(
                db.verify_user_password(username, password).unwrap(),
                "Password verification should succeed for {}",
                username
            );
            assert!(
                !db.verify_user_password(username, "wrongpassword").unwrap(),
                "Wrong password should fail for {}",
                username
            );
        }

        println!("✅ User password handling tests passed");
    }

    #[test]
    fn test_neighbourhood_owners_management() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create a neighbourhood perspective
        let neighbourhood_url = "neighbourhood://test123";
        let perspective = PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("Test Neighbourhood".to_string()),
            neighbourhood: None,
            shared_url: Some(neighbourhood_url.to_string()),
            state: PerspectiveState::Synced,
            owners: Some(vec!["did:key:user1".to_string()]),
        };

        // Add perspective to database
        db.add_perspective(&perspective).unwrap();

        // Test adding owners
        db.add_owner_to_neighbourhood(neighbourhood_url, "did:key:user2")
            .unwrap();
        db.add_owner_to_neighbourhood(neighbourhood_url, "did:key:user3")
            .unwrap();

        // Test duplicate owner (should not add twice)
        db.add_owner_to_neighbourhood(neighbourhood_url, "did:key:user2")
            .unwrap();

        // Retrieve owners
        let owners = db.get_neighbourhood_owners(neighbourhood_url).unwrap();

        // Verify owners
        assert_eq!(owners.len(), 3);
        assert!(owners.contains(&"did:key:user1".to_string()));
        assert!(owners.contains(&"did:key:user2".to_string()));
        assert!(owners.contains(&"did:key:user3".to_string()));

        // Test retrieving perspective with owners
        let all_perspectives = db.get_all_perspectives().unwrap();
        let retrieved_perspective = all_perspectives
            .iter()
            .find(|p| p.shared_url.as_ref() == Some(&neighbourhood_url.to_string()))
            .unwrap();

        assert_eq!(retrieved_perspective.get_owners().len(), 3);
        assert!(retrieved_perspective.is_owned_by("did:key:user2"));
        assert!(!retrieved_perspective.is_owned_by("did:key:user4"));

        println!("✅ Neighbourhood owners management tests passed");
    }

    #[test]
    fn test_user_last_seen_tracking() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create test user
        db.add_user("test@example.com", "did:key:test123", "testpassword")
            .unwrap();

        // Verify initial last_seen is None
        let user = db.get_user("test@example.com").unwrap();
        assert!(user.last_seen.is_none(), "Initial last_seen should be None");

        // Update last_seen
        db.update_user_last_seen("test@example.com").unwrap();

        // Verify last_seen was updated
        let user = db.get_user("test@example.com").unwrap();
        assert!(
            user.last_seen.is_some(),
            "last_seen should be set after update"
        );

        let last_seen_timestamp = user.last_seen.unwrap();
        let now = chrono::Utc::now().timestamp();
        assert!(
            (now - last_seen_timestamp).abs() < 2,
            "last_seen should be within 2 seconds of now"
        );

        // Update again and verify it changes
        std::thread::sleep(std::time::Duration::from_secs(1));
        db.update_user_last_seen("test@example.com").unwrap();

        let user_updated = db.get_user("test@example.com").unwrap();
        assert!(
            user_updated.last_seen.unwrap() > last_seen_timestamp,
            "last_seen should be updated to newer timestamp"
        );

        println!("✅ User last_seen tracking tests passed");
    }

    #[test]
    fn test_list_users_ordering() {
        // Initialize test database
        let db = Ad4mDb::new(":memory:").unwrap();

        // Create users with different last_seen times
        db.add_user("user1@example.com", "did:key:user1", "pass1")
            .unwrap();
        db.add_user("user2@example.com", "did:key:user2", "pass2")
            .unwrap();
        db.add_user("user3@example.com", "did:key:user3", "pass3")
            .unwrap();

        // Update last_seen for users in specific order
        db.update_user_last_seen("user2@example.com").unwrap();
        std::thread::sleep(std::time::Duration::from_secs(2)); // Sleep 2 seconds to ensure different timestamps (i32 seconds)
        db.update_user_last_seen("user3@example.com").unwrap();
        // user1 has no last_seen

        // List users - should be ordered by last_seen DESC (most recent first, nulls last)
        let users = db.list_users().unwrap();
        assert_eq!(users.len(), 3);
        assert_eq!(
            users[0].username, "user3@example.com",
            "Most recent should be first"
        );
        assert_eq!(users[1].username, "user2@example.com", "Second most recent");
        assert_eq!(
            users[2].username, "user1@example.com",
            "Null last_seen should be last"
        );

        println!("✅ User list ordering tests passed");
    }
}
