//! The move of `models` from a model's own connection (`api_*` columns) or local weights
//! (`local_*` columns) to a reference to an endpoint (`endpoint_id`, `remote_model`).
//!
//! Runs once, at startup, on a database whose `models` table still has `api_base_url`:
//! each API model gets the endpoint with its protocol, URL and key (one is created when
//! none exists), each local model is deleted with one warning line, and the table is
//! rebuilt in its new shape. `model_status` (local download progress) goes too. An import
//! of an export written before the move uses [`Ad4mDb::endpoint_for_legacy_api`] the same
//! way.

use rusqlite::{params, Connection};
use url::Url;

use super::{Ad4mDb, Ad4mDbResult};
use crate::secret::Secret;
use crate::types::{
    same_endpoint, AiCapability, Endpoint, EndpointHealth, EndpointKind, ModelApiType, ModelType,
};

/// The `models` table in its current shape.
pub(super) const CREATE_MODELS: &str = "CREATE TABLE IF NOT EXISTS models (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    type TEXT NOT NULL,
    endpoint_id TEXT NOT NULL,
    remote_model TEXT NOT NULL,
    native_tools INTEGER
)";

/// A legacy model's connection: what its `api_*` columns (or export fields) held.
pub(super) struct LegacyApi {
    pub base_url: Url,
    pub api_key: String,
    pub api_type: ModelApiType,
    pub max_num_ctx: Option<u32>,
}

/// The name and kind of the endpoint created for a legacy `api` none serves yet: named by
/// its host and port, of the kind its protocol and host say.
fn endpoint_for_api(api_type: &ModelApiType, base_url: &Url) -> (String, EndpointKind) {
    let host = base_url.host_str().unwrap_or("endpoint");
    let name = match base_url.port() {
        Some(port) => format!("{host}:{port}"),
        None => host.to_string(),
    };
    let kind = match api_type {
        ModelApiType::Anthropic => EndpointKind::Anthropic,
        ModelApiType::Ollama => EndpointKind::Ollama,
        ModelApiType::OpenAi if host == "api.openai.com" => EndpointKind::OpenAi,
        ModelApiType::OpenAi => EndpointKind::OpenAiCompatible,
    };
    (name, kind)
}

fn has_column(conn: &Connection, table: &str, column: &str) -> rusqlite::Result<bool> {
    let mut stmt = conn.prepare(&format!("PRAGMA table_info({table})"))?;
    let names = stmt.query_map([], |row| row.get::<_, String>(1))?;
    for name in names {
        if name? == column {
            return Ok(true);
        }
    }
    Ok(false)
}

impl Ad4mDb {
    /// The id of the endpoint that serves `api`: the one with its protocol, URL and key,
    /// else a new one named by the URL's host (a suffix when the name is taken). The
    /// endpoint gains `capability` when it lacks it.
    pub(super) fn endpoint_for_legacy_api(
        &self,
        api: LegacyApi,
        capability: AiCapability,
    ) -> Ad4mDbResult<String> {
        let existing = self.get_endpoints()?.into_iter().find(|endpoint| {
            endpoint.kind.protocol() == api.api_type
                && same_endpoint(endpoint.base_url.as_str(), api.base_url.as_str())
                && endpoint.api_key.expose_for_compare(&api.api_key)
        });
        if let Some(endpoint) = existing {
            self.add_endpoint_capability(&endpoint, capability)?;
            return Ok(endpoint.id);
        }
        let (host_name, kind) = endpoint_for_api(&api.api_type, &api.base_url);
        let taken: Vec<String> = self.get_endpoints()?.into_iter().map(|e| e.name).collect();
        let name = std::iter::once(host_name.clone())
            .chain((2..).map(|n| format!("{host_name}-{n}")))
            .find(|name| !taken.contains(name))
            .expect("an unbounded list of names has a free one");
        let endpoint = Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            kind,
            base_url: api.base_url,
            api_key: Secret::new(api.api_key),
            capabilities: vec![capability],
            max_num_ctx: api.max_num_ctx,
            request_timeout_secs: None,
            max_concurrent: None,
            health: EndpointHealth::default(),
        };
        self.insert_endpoint(&endpoint)?;
        Ok(endpoint.id)
    }

    /// Moves a `models` table with `api_*` / `local_*` columns to its current shape. A
    /// no-op on a table already moved. Drops `model_status` either way.
    pub(super) fn migrate_models_to_endpoints(&self) -> Ad4mDbResult<()> {
        self.conn.execute("DROP TABLE IF EXISTS model_status", [])?;
        if !has_column(&self.conn, "models", "api_base_url")? {
            return Ok(());
        }
        // A database from before endpoints lacks the columns the link reads.
        for column in [
            "api_max_num_ctx INTEGER",
            "endpoint_id TEXT",
            "api_native_tools INTEGER",
        ] {
            match self
                .conn
                .execute(&format!("ALTER TABLE models ADD COLUMN {column}"), [])
            {
                Err(e) if !e.to_string().contains("duplicate column name") => return Err(e.into()),
                _ => {}
            }
        }

        let tx = self.conn.unchecked_transaction()?;
        let rows: Vec<LegacyRow> = {
            let mut stmt = tx.prepare(
                "SELECT id, name, type, api_base_url, COALESCE(api_key, ''), api_type,
                        api_max_num_ctx, endpoint_id
                 FROM models",
            )?;
            let rows = stmt.query_map([], |row| {
                Ok(LegacyRow {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    model_type: row.get(2)?,
                    base_url: row.get(3)?,
                    api_key: row.get(4)?,
                    api_type: row.get(5)?,
                    max_num_ctx: row.get(6)?,
                    endpoint_id: row.get(7)?,
                })
            })?;
            rows.collect::<Result<_, _>>()?
        };

        for row in rows {
            if row.endpoint_id.is_some() {
                continue;
            }
            match row.legacy_api() {
                Some((api, capability)) => {
                    let endpoint_id = self.endpoint_for_legacy_api(api, capability)?;
                    tx.execute(
                        "UPDATE models SET endpoint_id = ?2 WHERE id = ?1",
                        params![row.id, endpoint_id],
                    )?;
                }
                None => {
                    log::warn!(
                        "AI model {:?} ({}) ran in-process, which this executor no longer does: \
                         it is deleted. Register a model on an endpoint (Ollama, llama-server, \
                         speaches, ...) instead.",
                        row.name,
                        row.model_type.trim_matches('"'),
                    );
                    tx.execute("DELETE FROM models WHERE id = ?1", params![row.id])?;
                    tx.execute(
                        "DELETE FROM default_models WHERE model_id = ?1",
                        params![row.id],
                    )?;
                }
            }
        }

        tx.execute_batch(&format!(
            "{};
             INSERT INTO models_new (id, name, type, endpoint_id, remote_model, native_tools)
                 SELECT id, name, type, endpoint_id, model, api_native_tools FROM models
                 WHERE endpoint_id IS NOT NULL AND model IS NOT NULL;
             DROP TABLE models;
             ALTER TABLE models_new RENAME TO models;",
            CREATE_MODELS.replacen("models", "models_new", 1)
        ))?;
        tx.commit()?;
        Ok(())
    }
}

impl Ad4mDb {
    /// Inserts one `ai_endpoints` entry of an export.
    pub(super) fn import_endpoint(&self, entry: &serde_json::Value) -> Ad4mDbResult<()> {
        let text = |field: &str| {
            entry[field]
                .as_str()
                .map(str::to_string)
                .ok_or_else(|| anyhow::anyhow!("{field} missing"))
        };
        let optional_u32 = |field: &str| entry[field].as_u64().and_then(|v| u32::try_from(v).ok());
        let endpoint = Endpoint {
            id: text("id")?,
            name: text("name")?,
            kind: serde_json::from_value(entry["kind"].clone())?,
            base_url: Url::parse(&text("base_url")?)?,
            api_key: Secret::new(entry["api_key"].as_str().unwrap_or_default()),
            capabilities: serde_json::from_value(entry["capabilities"].clone())?,
            max_num_ctx: optional_u32("max_num_ctx"),
            request_timeout_secs: optional_u32("request_timeout_secs"),
            max_concurrent: optional_u32("max_concurrent"),
            health: EndpointHealth::default(),
        };
        self.insert_endpoint(&endpoint)
    }

    /// Inserts one `models` entry of an export: `false` when it was left out. An entry of an
    /// export written before endpoints carries its connection (`api_*`): it gets the
    /// endpoint that serves it, as the migration gives one. A local model is left out.
    pub(super) fn import_model(&self, entry: &serde_json::Value) -> Ad4mDbResult<bool> {
        let text = |field: &str| entry[field].as_str().map(str::to_string);
        let id = text("id").ok_or_else(|| anyhow::anyhow!("id missing"))?;
        let name = text("name").ok_or_else(|| anyhow::anyhow!("name missing"))?;
        let model_type = text("model_type").ok_or_else(|| anyhow::anyhow!("model_type missing"))?;
        let (endpoint_id, remote_model, native_tools) =
            match (text("endpoint_id"), text("remote_model")) {
                (Some(endpoint_id), Some(remote_model)) => {
                    (endpoint_id, remote_model, entry["native_tools"].as_bool())
                }
                _ => {
                    let row = LegacyRow {
                        id: id.clone(),
                        name: name.clone(),
                        model_type: model_type.clone(),
                        base_url: text("api_base_url"),
                        api_key: text("api_key").unwrap_or_default(),
                        api_type: text("api_type"),
                        max_num_ctx: entry["api_max_num_ctx"]
                            .as_u64()
                            .and_then(|v| u32::try_from(v).ok())
                            .filter(|ctx| *ctx > 0),
                        endpoint_id: None,
                    };
                    let (Some((api, capability)), Some(remote_model)) =
                        (row.legacy_api(), text("model"))
                    else {
                        log::warn!(
                            "AI model {name:?} of the import ran in-process, which this executor \
                             no longer does: it is left out"
                        );
                        return Ok(false);
                    };
                    (
                        self.endpoint_for_legacy_api(api, capability)?,
                        remote_model,
                        None,
                    )
                }
            };
        self.conn.execute(
            "INSERT INTO models (id, name, type, endpoint_id, remote_model, native_tools)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                id,
                name,
                model_type,
                endpoint_id,
                remote_model,
                native_tools
            ],
        )?;
        Ok(true)
    }
}

/// One row of a `models` table in its legacy shape.
struct LegacyRow {
    id: String,
    name: String,
    model_type: String,
    base_url: Option<String>,
    api_key: String,
    api_type: Option<String>,
    max_num_ctx: Option<u32>,
    endpoint_id: Option<String>,
}

impl LegacyRow {
    /// The row's connection and the capability its type needs. `None` for a local model,
    /// or an `api` that does not read.
    fn legacy_api(&self) -> Option<(LegacyApi, AiCapability)> {
        let base_url = Url::parse(self.base_url.as_deref()?).ok()?;
        let api_type = self.api_type.as_deref()?.parse::<ModelApiType>().ok()?;
        let model_type = serde_json::from_str::<ModelType>(&self.model_type).ok()?;
        Some((
            LegacyApi {
                base_url,
                api_key: self.api_key.clone(),
                api_type,
                max_num_ctx: self.max_num_ctx,
            },
            AiCapability::from(&model_type),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `models` table as the executor wrote it before endpoints: `columns` after `type`.
    fn legacy_db(extra_columns: &str) -> (tempfile::TempDir, String) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m.db").to_string_lossy().to_string();
        let conn = Connection::open(&path).unwrap();
        conn.execute_batch(&format!(
            "CREATE TABLE models (
                id TEXT PRIMARY KEY, name TEXT NOT NULL, api_base_url TEXT, api_key TEXT,
                model TEXT, api_type TEXT, local_file_name TEXT, local_tokenizer_repo TEXT,
                local_tokenizer_revision TEXT, local_tokenizer_file_name TEXT,
                local_huggingface_repo TEXT, local_revision TEXT, type TEXT NOT NULL
                {extra_columns});
             CREATE TABLE model_status (model TEXT PRIMARY KEY, progress DOUBLE NOT NULL,
                status TEXT NOT NULL, downloaded BOOLEAN NOT NULL, loaded BOOLEAN NOT NULL);
             CREATE TABLE default_models (model_type TEXT PRIMARY KEY, model_id TEXT NOT NULL);
             INSERT INTO model_status VALUES ('whisper', 100, 'Loaded', 1, 1);"
        ))
        .unwrap();
        (dir, path)
    }

    fn insert_api(conn: &Connection, id: &str, url: &str, key: &str, kind: &str, ty: &str) {
        conn.execute(
            "INSERT INTO models (id, name, api_base_url, api_key, model, api_type, type)
             VALUES (?1, ?1, ?2, ?3, ?4, ?5, ?6)",
            params![id, url, key, format!("{id}-remote"), kind, ty],
        )
        .unwrap();
    }

    fn insert_local(conn: &Connection, id: &str, ty: &str) {
        conn.execute(
            "INSERT INTO models (id, name, local_file_name, type) VALUES (?1, ?1, 'bert', ?2)",
            params![id, ty],
        )
        .unwrap();
    }

    #[test]
    fn api_models_move_to_endpoints_and_local_models_are_deleted() {
        let (_dir, path) = legacy_db("");
        {
            let conn = Connection::open(&path).unwrap();
            insert_api(
                &conn,
                "gpt",
                "https://api.openai.com/v1",
                "k1",
                "\"OPEN_AI\"",
                "\"LLM\"",
            );
            insert_api(
                &conn,
                "embed",
                "https://api.openai.com/v1",
                "k1",
                "OPEN_AI",
                "\"EMBEDDING\"",
            );
            insert_api(
                &conn,
                "other-key",
                "https://api.openai.com/v1",
                "k2",
                "OPEN_AI",
                "\"LLM\"",
            );
            insert_api(
                &conn,
                "qwen",
                "http://localhost:11434",
                "",
                "OLLAMA",
                "\"LLM\"",
            );
            insert_local(&conn, "llama", "\"LLM\"");
            insert_local(&conn, "whisper", "\"TRANSCRIPTION\"");
            conn.execute_batch(
                "INSERT INTO default_models VALUES ('\"LLM\"', 'llama');
                 INSERT INTO default_models VALUES ('\"EMBEDDING\"', 'embed');",
            )
            .unwrap();
        }

        let db = Ad4mDb::new(&path).unwrap();

        let mut names: Vec<String> = db
            .get_models()
            .unwrap()
            .into_iter()
            .map(|m| m.name)
            .collect();
        names.sort();
        assert_eq!(names, vec!["embed", "gpt", "other-key", "qwen"]);

        let gpt = db.get_model("gpt".into()).unwrap().unwrap();
        let embed = db.get_model("embed".into()).unwrap().unwrap();
        let other = db.get_model("other-key".into()).unwrap().unwrap();
        assert_eq!(gpt.remote_model, "gpt-remote");
        assert_eq!(gpt.model_type, ModelType::Llm);
        assert_eq!(
            gpt.endpoint_id, embed.endpoint_id,
            "same URL, key and protocol"
        );
        assert_ne!(gpt.endpoint_id, other.endpoint_id, "another key");

        let openai = db.get_endpoint(&gpt.endpoint_id).unwrap().unwrap();
        assert_eq!(openai.name, "api.openai.com");
        assert_eq!(openai.kind, EndpointKind::OpenAi);
        assert!(openai.api_key.expose_for_compare("k1"));
        assert_eq!(
            openai.capabilities,
            vec![AiCapability::Chat, AiCapability::Embeddings]
        );
        let ollama = db
            .get_endpoint(&db.get_model("qwen".into()).unwrap().unwrap().endpoint_id)
            .unwrap()
            .unwrap();
        assert_eq!(
            (ollama.name.as_str(), ollama.kind),
            ("localhost:11434", EndpointKind::Ollama)
        );

        assert_eq!(db.get_default_model(ModelType::Llm).unwrap(), None);
        assert_eq!(
            db.get_default_model(ModelType::Embedding)
                .unwrap()
                .as_deref(),
            Some("embed")
        );
        for gone in [
            "api_base_url",
            "api_key",
            "local_file_name",
            "api_max_num_ctx",
        ] {
            assert!(!has_column(&db.conn, "models", gone).unwrap(), "{gone}");
        }
        let status_table: i64 = db
            .conn
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE name = 'model_status'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(status_table, 0);
    }

    #[test]
    fn a_database_linked_by_the_endpoint_registry_keeps_its_links_and_settings() {
        let (_dir, path) =
            legacy_db(", api_max_num_ctx INTEGER, endpoint_id TEXT, api_native_tools INTEGER");
        {
            let conn = Connection::open(&path).unwrap();
            conn.execute_batch(
                "CREATE TABLE ai_endpoints (id TEXT PRIMARY KEY, name TEXT NOT NULL UNIQUE,
                    kind TEXT NOT NULL, base_url TEXT NOT NULL, api_key TEXT,
                    capabilities TEXT NOT NULL, max_num_ctx INTEGER, health TEXT);
                 INSERT INTO ai_endpoints VALUES ('e1', 'box', 'VLLM', 'http://127.0.0.1:8000/',
                    '', '[\"chat\"]', NULL, NULL);
                 INSERT INTO models (id, name, api_base_url, api_key, model, api_type, type,
                    endpoint_id, api_native_tools)
                 VALUES ('m1', 'chat', 'http://127.0.0.1:8000/', '', 'Qwen/Qwen3-8B',
                    'OPEN_AI', '\"LLM\"', 'e1', 1);",
            )
            .unwrap();
        }

        let db = Ad4mDb::new(&path).unwrap();
        let model = db.get_model("m1".into()).unwrap().unwrap();
        assert_eq!(model.endpoint_id, "e1");
        assert_eq!(model.remote_model, "Qwen/Qwen3-8B");
        assert_eq!(model.native_tools, Some(true));
        assert_eq!(db.get_endpoints().unwrap().len(), 1);
    }

    #[test]
    fn a_moved_table_is_left_alone() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m.db").to_string_lossy().to_string();
        let db = Ad4mDb::new(&path).unwrap();
        let endpoint = Endpoint::for_test("box", EndpointKind::Vllm, "http://127.0.0.1:8000", "");
        db.insert_endpoint(&endpoint).unwrap();
        let id = db
            .add_model(&crate::types::ModelInput {
                name: "chat".into(),
                model_type: ModelType::Llm,
                endpoint_id: endpoint.id.clone(),
                remote_model: "qwen".into(),
                native_tools: None,
            })
            .unwrap();
        drop(db);

        let db = Ad4mDb::new(&path).unwrap();
        assert_eq!(db.get_model(id).unwrap().unwrap().remote_model, "qwen");
    }
}
