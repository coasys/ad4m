//! The `ai_endpoints` table: the upstream model servers. An endpoint owns the URL, the key,
//! the protocol and the health; a model (`models.endpoint_id`) names the endpoint it runs
//! on and the id the endpoint knows it by.

use rusqlite::{params, Connection, OptionalExtension};
use url::Url;

use super::{Ad4mDb, Ad4mDbResult, ApiKeyNotResent};
use crate::secret::Secret;
use crate::types::{same_endpoint, AiCapability, Endpoint, EndpointHealth, EndpointKind};

/// A write the endpoint registry refuses: a name or `(protocol, URL, key)` already taken, or
/// a removal while models use the endpoint. The caller's mistake: a bad request.
#[derive(Debug)]
pub struct InvalidEndpoint(pub String);

impl std::fmt::Display for InvalidEndpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for InvalidEndpoint {}

pub(super) fn create_table(conn: &Connection) -> Result<(), rusqlite::Error> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS ai_endpoints (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL UNIQUE,
            kind TEXT NOT NULL,
            base_url TEXT NOT NULL,
            api_key TEXT,
            capabilities TEXT NOT NULL,
            max_num_ctx INTEGER,
            health TEXT
        )",
    )?;
    // Added after the table first shipped; appended so `ENDPOINT_COLUMNS` reads both shapes.
    for column in [
        "request_timeout_secs INTEGER",
        "max_concurrent INTEGER",
        "managed INTEGER NOT NULL DEFAULT 0",
    ] {
        match conn.execute(&format!("ALTER TABLE ai_endpoints ADD COLUMN {column}"), []) {
            Err(e) if !e.to_string().contains("duplicate column name") => return Err(e),
            _ => {}
        }
    }
    Ok(())
}

const ENDPOINT_COLUMNS: &str = "id, name, kind, base_url, api_key, capabilities, max_num_ctx, health, request_timeout_secs, max_concurrent, managed";

fn json_err(column: usize, error: serde_json::Error) -> rusqlite::Error {
    rusqlite::Error::FromSqlConversionFailure(column, rusqlite::types::Type::Text, Box::new(error))
}

fn endpoint_from_row(row: &rusqlite::Row) -> rusqlite::Result<Endpoint> {
    let kind: String = row.get(2)?;
    let base_url: String = row.get(3)?;
    let capabilities: String = row.get(5)?;
    let health: Option<String> = row.get(7)?;
    Ok(Endpoint {
        id: row.get(0)?,
        name: row.get(1)?,
        kind: serde_json::from_value(serde_json::Value::String(kind))
            .map_err(|e| json_err(2, e))?,
        base_url: Url::parse(&base_url).map_err(|e| {
            rusqlite::Error::FromSqlConversionFailure(3, rusqlite::types::Type::Text, Box::new(e))
        })?,
        api_key: Secret::new(row.get::<_, Option<String>>(4)?.unwrap_or_default()),
        capabilities: serde_json::from_str(&capabilities).map_err(|e| json_err(5, e))?,
        max_num_ctx: row.get(6)?,
        request_timeout_secs: row.get(8)?,
        max_concurrent: row.get(9)?,
        managed: row.get(10)?,
        health: match health {
            Some(health) => serde_json::from_str(&health).map_err(|e| json_err(7, e))?,
            None => EndpointHealth::default(),
        },
    })
}

fn kind_column(kind: EndpointKind) -> String {
    match serde_json::to_value(kind) {
        Ok(serde_json::Value::String(kind)) => kind,
        _ => unreachable!("an EndpointKind serialises as a string"),
    }
}

impl Ad4mDb {
    pub fn get_endpoints(&self) -> Ad4mDbResult<Vec<Endpoint>> {
        let mut stmt = self.conn.prepare(&format!(
            "SELECT {ENDPOINT_COLUMNS} FROM ai_endpoints ORDER BY name"
        ))?;
        let endpoints = stmt.query_map([], endpoint_from_row)?;
        Ok(endpoints.collect::<Result<_, _>>()?)
    }

    pub fn get_endpoint(&self, id: &str) -> Ad4mDbResult<Option<Endpoint>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {ENDPOINT_COLUMNS} FROM ai_endpoints WHERE id = ?1"),
                params![id],
                endpoint_from_row,
            )
            .optional()?)
    }

    pub fn get_endpoint_by_name(&self, name: &str) -> Ad4mDbResult<Option<Endpoint>> {
        Ok(self
            .conn
            .query_row(
                &format!("SELECT {ENDPOINT_COLUMNS} FROM ai_endpoints WHERE name = ?1"),
                params![name],
                endpoint_from_row,
            )
            .optional()?)
    }

    /// The registry as a backup carries it: each endpoint with its key, as the backup
    /// carries the models' keys. Health is not kept.
    pub(super) fn export_endpoints(&self) -> Ad4mDbResult<Vec<serde_json::Value>> {
        Ok(self
            .get_endpoints()?
            .iter()
            .map(|endpoint| {
                serde_json::json!({
                    "id": endpoint.id,
                    "name": endpoint.name,
                    "kind": kind_column(endpoint.kind),
                    "base_url": endpoint.base_url.as_str(),
                    "api_key": endpoint.api_key.expose(),
                    "capabilities": endpoint.capabilities,
                    "max_num_ctx": endpoint.max_num_ctx,
                    "request_timeout_secs": endpoint.request_timeout_secs,
                    "max_concurrent": endpoint.max_concurrent,
                })
            })
            .collect())
    }

    /// Adds one endpoint of a backup, with unknown health. Refused as any insert is: a
    /// name or server already taken.
    pub(super) fn import_endpoint(&self, backup: &serde_json::Value) -> Ad4mDbResult<()> {
        #[derive(serde::Deserialize)]
        struct Backup {
            id: String,
            name: String,
            kind: EndpointKind,
            base_url: Url,
            #[serde(default)]
            api_key: String,
            capabilities: Vec<AiCapability>,
            max_num_ctx: Option<u32>,
            // Absent from a backup made before the per-endpoint limits existed.
            #[serde(default)]
            request_timeout_secs: Option<u32>,
            #[serde(default)]
            max_concurrent: Option<u32>,
        }
        let backup: Backup = serde_json::from_value(backup.clone())?;
        if backup.max_num_ctx == Some(0) {
            return Err(InvalidEndpoint("max_num_ctx must be positive".into()).into());
        }
        self.insert_endpoint(&Endpoint {
            id: backup.id,
            name: backup.name,
            kind: backup.kind,
            base_url: backup.base_url,
            api_key: Secret::new(backup.api_key),
            capabilities: backup.capabilities,
            max_num_ctx: backup.max_num_ctx,
            request_timeout_secs: backup.request_timeout_secs,
            max_concurrent: backup.max_concurrent,
            // A restored endpoint is the operator's: local AI manages only what it installs.
            managed: false,
            health: EndpointHealth::default(),
        })
    }

    /// Refuses `endpoint` when another one has its name, or the same protocol, URL and key
    /// (two such endpoints are one server), and a concurrency cap or a request timeout of 0,
    /// either of which would let no request through.
    fn refuse_invalid(&self, endpoint: &Endpoint) -> Ad4mDbResult<()> {
        if endpoint.max_concurrent == Some(0) {
            return Err(InvalidEndpoint(
                "maxConcurrent is at least 1; leave it out for no limit".into(),
            )
            .into());
        }
        if endpoint.request_timeout_secs == Some(0) {
            return Err(InvalidEndpoint(
                "requestTimeoutSecs is at least 1; leave it out for the default".into(),
            )
            .into());
        }
        for other in self.get_endpoints()? {
            if other.id == endpoint.id {
                continue;
            }
            if other.name == endpoint.name {
                return Err(InvalidEndpoint(format!(
                    "An endpoint named {:?} exists already",
                    endpoint.name
                ))
                .into());
            }
            if other.kind.protocol() == endpoint.kind.protocol()
                && other.base_url == endpoint.base_url
                && other.api_key.expose_for_compare(endpoint.api_key.expose())
            {
                return Err(InvalidEndpoint(format!(
                    "Endpoint {:?} already serves {} with this key",
                    other.name, endpoint.base_url
                ))
                .into());
            }
        }
        Ok(())
    }

    pub fn insert_endpoint(&self, endpoint: &Endpoint) -> Ad4mDbResult<()> {
        self.refuse_invalid(endpoint)?;
        self.conn.execute(
            &format!("INSERT INTO ai_endpoints ({ENDPOINT_COLUMNS}) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)"),
            params![
                endpoint.id,
                endpoint.name,
                kind_column(endpoint.kind),
                endpoint.base_url.to_string(),
                // The database is where the executor keeps the key.
                endpoint.api_key.expose(),
                serde_json::to_string(&endpoint.capabilities)?,
                endpoint.max_num_ctx,
                serde_json::to_string(&endpoint.health)?,
                endpoint.request_timeout_secs,
                endpoint.max_concurrent,
                endpoint.managed,
            ],
        )?;
        Ok(())
    }

    /// Writes `endpoint` over the stored one. Answers the ids of the models it serves: their
    /// clients have to be built again.
    pub fn update_endpoint(&self, endpoint: &Endpoint) -> Ad4mDbResult<Vec<String>> {
        self.refuse_invalid(endpoint)?;
        self.conn.execute(
            "UPDATE ai_endpoints SET name = ?2, kind = ?3, base_url = ?4, api_key = ?5,
                capabilities = ?6, max_num_ctx = ?7, health = ?8, request_timeout_secs = ?9,
                max_concurrent = ?10, managed = ?11
             WHERE id = ?1",
            params![
                endpoint.id,
                endpoint.name,
                kind_column(endpoint.kind),
                endpoint.base_url.to_string(),
                endpoint.api_key.expose(),
                serde_json::to_string(&endpoint.capabilities)?,
                endpoint.max_num_ctx,
                serde_json::to_string(&endpoint.health)?,
                endpoint.request_timeout_secs,
                endpoint.max_concurrent,
                endpoint.managed,
            ],
        )?;
        self.endpoint_model_ids(&endpoint.id)
    }

    /// The stored key an update without one keeps: refused when one is set and `base_url`
    /// differs from the stored URL, because a key goes only to the endpoint it was set for.
    pub fn kept_endpoint_key(&self, id: &str, base_url: &Url) -> Ad4mDbResult<Secret> {
        let Some(stored) = self.get_endpoint(id)? else {
            return Ok(Secret::new(""));
        };
        if !stored.api_key.is_empty() && !same_endpoint(stored.base_url.as_str(), base_url.as_str())
        {
            return Err(ApiKeyNotResent.into());
        }
        Ok(stored.api_key)
    }

    /// Gives `endpoint` `capability` when it lacks it.
    pub fn add_endpoint_capability(
        &self,
        endpoint: &Endpoint,
        capability: AiCapability,
    ) -> Ad4mDbResult<()> {
        if endpoint.capabilities.contains(&capability) {
            return Ok(());
        }
        let mut capabilities = endpoint.capabilities.clone();
        capabilities.push(capability);
        capabilities.sort();
        self.conn.execute(
            "UPDATE ai_endpoints SET capabilities = ?2 WHERE id = ?1",
            params![endpoint.id, serde_json::to_string(&capabilities)?],
        )?;
        Ok(())
    }

    /// Stores what a check of endpoint `id` at `base_url` found: its health and capabilities.
    /// Answers false, storing nothing, when the endpoint has moved to another URL since: an
    /// update probed the new one while the health loop checked the old.
    pub fn set_endpoint_health(
        &self,
        id: &str,
        base_url: &Url,
        health: &EndpointHealth,
        capabilities: &[AiCapability],
    ) -> Ad4mDbResult<bool> {
        let updated = self.conn.execute(
            "UPDATE ai_endpoints SET health = ?3, capabilities = ?4
             WHERE id = ?1 AND base_url = ?2",
            params![
                id,
                base_url.to_string(),
                serde_json::to_string(health)?,
                serde_json::to_string(capabilities)?
            ],
        )?;
        Ok(updated > 0)
    }

    /// The models that run on endpoint `id`.
    pub fn endpoint_model_ids(&self, id: &str) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self
            .conn
            .prepare("SELECT id FROM models WHERE endpoint_id = ?1 ORDER BY name")?;
        let ids = stmt.query_map(params![id], |row| row.get(0))?;
        Ok(ids.collect::<Result<_, _>>()?)
    }

    /// The upstream model ids the models on endpoint `id` use, each once.
    pub fn endpoint_remote_models(&self, id: &str) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self.conn.prepare(
            "SELECT DISTINCT remote_model FROM models WHERE endpoint_id = ?1 ORDER BY remote_model",
        )?;
        let models = stmt.query_map(params![id], |row| row.get(0))?;
        Ok(models.collect::<Result<_, _>>()?)
    }

    /// The names of the models that use `remote_model` on endpoint `id`. A name without a
    /// tag is the `:latest` tag, as Ollama reads it: `qwen3` and `qwen3:latest` are one model.
    pub fn models_using_remote(&self, id: &str, remote_model: &str) -> Ad4mDbResult<Vec<String>> {
        fn tagged(model: &str) -> String {
            let name = model.rsplit('/').next().unwrap_or(model);
            if name.contains(':') {
                model.to_string()
            } else {
                format!("{model}:latest")
            }
        }
        let wanted = tagged(remote_model);
        let mut stmt = self.conn.prepare(
            "SELECT name, remote_model FROM models WHERE endpoint_id = ?1 ORDER BY name",
        )?;
        let rows = stmt.query_map(params![id], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
        })?;
        let mut names = Vec::new();
        for row in rows {
            let (name, model) = row?;
            if tagged(&model) == wanted {
                names.push(name);
            }
        }
        Ok(names)
    }

    /// Removes endpoint `id`. Refused while a model runs on it.
    pub fn remove_endpoint(&self, id: &str) -> Ad4mDbResult<()> {
        let models = self.endpoint_model_ids(id)?;
        if !models.is_empty() {
            return Err(InvalidEndpoint(format!(
                "{} model(s) run on this endpoint: remove them first, or pass cascade: true",
                models.len()
            ))
            .into());
        }
        self.conn
            .execute("DELETE FROM ai_endpoints WHERE id = ?1", params![id])?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::InvalidModel;
    use crate::types::{EndpointState, ModelInput, ModelType};

    fn db() -> Ad4mDb {
        Ad4mDb::new(":memory:").unwrap()
    }

    fn endpoint(name: &str, url: &str, key: &str) -> Endpoint {
        Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name: name.to_string(),
            kind: EndpointKind::LlamaServer,
            base_url: Url::parse(url).unwrap(),
            api_key: Secret::new(key),
            capabilities: vec![AiCapability::Chat],
            max_num_ctx: Some(8192),
            request_timeout_secs: Some(90),
            max_concurrent: Some(2),
            managed: false,
            health: EndpointHealth::default(),
        }
    }

    /// Model `name` on endpoint `endpoint_id`, known there as `<name>-remote`.
    fn on(endpoint_id: &str, name: &str, model_type: ModelType) -> ModelInput {
        ModelInput {
            name: name.to_string(),
            model_type,
            endpoint_id: endpoint_id.to_string(),
            remote_model: format!("{name}-remote"),
            native_tools: None,
        }
    }

    /// A saved endpoint `name` at `url`, without a key.
    fn saved(db: &Ad4mDb, name: &str, url: &str) -> Endpoint {
        let endpoint = endpoint(name, url, "");
        db.insert_endpoint(&endpoint).unwrap();
        endpoint
    }

    #[test]
    fn an_endpoint_round_trips_with_its_key_capabilities_and_health() {
        let db = db();
        let mut stored = endpoint("local", "http://127.0.0.1:9931", "sk-1");
        stored.health = EndpointHealth {
            state: EndpointState::Reachable,
            checked_at: Some(42),
            detail: None,
            served_models: vec!["qwen".into()],
        };
        db.insert_endpoint(&stored).unwrap();

        let read = db.get_endpoint(&stored.id).unwrap().unwrap();
        assert_eq!(read.name, "local");
        assert_eq!(read.kind, EndpointKind::LlamaServer);
        assert_eq!(read.base_url, stored.base_url);
        assert!(read.api_key.expose_for_compare("sk-1"));
        assert_eq!(read.capabilities, vec![AiCapability::Chat]);
        assert_eq!(read.max_num_ctx, Some(8192));
        assert_eq!(read.request_timeout_secs, Some(90));
        assert_eq!(read.max_concurrent, Some(2));
        assert_eq!(read.health, stored.health);
        assert_eq!(
            db.get_endpoint_by_name("local").unwrap().unwrap().id,
            stored.id
        );
        assert_eq!(db.get_endpoints().unwrap().len(), 1);
    }

    #[test]
    fn a_taken_name_or_the_same_url_and_key_is_refused() {
        let db = db();
        db.insert_endpoint(&endpoint("a", "http://127.0.0.1:9931", "k"))
            .unwrap();
        let same_name = db.insert_endpoint(&endpoint("a", "http://127.0.0.1:8000", ""));
        assert!(same_name
            .unwrap_err()
            .downcast_ref::<InvalidEndpoint>()
            .is_some());
        let same_target = db.insert_endpoint(&endpoint("b", "http://127.0.0.1:9931/", "k"));
        assert!(same_target
            .unwrap_err()
            .downcast_ref::<InvalidEndpoint>()
            .is_some());
        // Another key on the same URL is another account: allowed.
        db.insert_endpoint(&endpoint("c", "http://127.0.0.1:9931", "k2"))
            .unwrap();
    }

    #[test]
    fn a_concurrency_cap_of_zero_is_refused() {
        let db = db();
        let mut capped = endpoint("a", "http://127.0.0.1:9931", "");
        capped.max_concurrent = Some(0);
        let refused = db.insert_endpoint(&capped).unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());
    }

    // A request timeout of 0 was stored, and every call to the endpoint then timed out at
    // once.
    #[test]
    fn a_request_timeout_of_zero_is_refused() {
        let db = db();
        let mut instant = endpoint("a", "http://127.0.0.1:9931", "");
        instant.request_timeout_secs = Some(0);
        let refused = db.insert_endpoint(&instant).unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());

        instant.request_timeout_secs = Some(1);
        db.insert_endpoint(&instant).unwrap();
        instant.request_timeout_secs = Some(0);
        let refused = db.update_endpoint(&instant).unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());
    }

    #[test]
    fn a_model_names_an_endpoint_that_exists_and_gives_it_its_capability() {
        let db = db();
        let refused = db
            .add_model(&on("no-such-endpoint", "m", ModelType::Llm))
            .unwrap_err();
        assert!(refused.downcast_ref::<InvalidModel>().is_some());

        let box_ = saved(&db, "box", "http://127.0.0.1:9931");
        let mut nameless = on(&box_.id, "m", ModelType::Llm);
        nameless.remote_model = " ".into();
        let refused = db.add_model(&nameless).unwrap_err();
        assert!(refused.downcast_ref::<InvalidModel>().is_some());

        let id = db
            .add_model(&on(&box_.id, "embed", ModelType::Embedding))
            .unwrap();
        let model = db.get_model(id).unwrap().unwrap();
        assert_eq!(model.endpoint_id, box_.id);
        assert_eq!(model.remote_model, "embed-remote");
        assert_eq!(model.model_type, ModelType::Embedding);
        assert_eq!(
            db.get_endpoint(&box_.id).unwrap().unwrap().capabilities,
            vec![AiCapability::Chat, AiCapability::Embeddings]
        );
    }

    #[test]
    fn an_endpoint_update_names_the_models_it_serves() {
        let db = db();
        let mut box_ = saved(&db, "box", "http://127.0.0.1:9931");
        let id = db.add_model(&on(&box_.id, "m", ModelType::Llm)).unwrap();
        box_.base_url = Url::parse("https://gpu.example.org/v1").unwrap();
        assert_eq!(db.update_endpoint(&box_).unwrap(), vec![id.clone()]);
        let model = db.get_model(id).unwrap().unwrap();
        assert_eq!(
            model.remote_model, "m-remote",
            "the remote model id stays the model's"
        );
    }

    #[test]
    fn a_kept_key_goes_only_to_the_url_it_was_set_for() {
        let db = db();
        let stored = endpoint("a", "https://api.example.org/v1", "k");
        db.insert_endpoint(&stored).unwrap();
        let same = Url::parse("https://api.example.org/v1").unwrap();
        assert!(db
            .kept_endpoint_key(&stored.id, &same)
            .unwrap()
            .expose_for_compare("k"));
        let moved = Url::parse("https://elsewhere.example.org/v1").unwrap();
        let refused = db.kept_endpoint_key(&stored.id, &moved).unwrap_err();
        assert!(refused.downcast_ref::<ApiKeyNotResent>().is_some());
    }

    #[test]
    fn an_endpoint_in_use_is_not_removed() {
        let db = db();
        let endpoint_id = saved(&db, "box", "http://127.0.0.1:9931").id;
        let id = db
            .add_model(&on(&endpoint_id, "m", ModelType::Llm))
            .unwrap();
        assert_eq!(
            db.models_using_remote(&endpoint_id, "m-remote").unwrap(),
            vec!["m"]
        );
        let refused = db.remove_endpoint(&endpoint_id).unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());

        db.remove_model(&id).unwrap();
        db.remove_endpoint(&endpoint_id).unwrap();
        assert!(db.get_endpoint(&endpoint_id).unwrap().is_none());
    }

    // Ollama reads a name without a tag as `:latest`.
    #[test]
    fn a_remote_model_in_use_is_found_with_or_without_its_latest_tag() {
        let db = db();
        let endpoint_id = saved(&db, "ollama", "http://127.0.0.1:11434").id;
        for (name, remote_model) in [("a", "qwen3"), ("b", "qwen3:latest"), ("c", "qwen3:8b")] {
            let mut model = on(&endpoint_id, name, ModelType::Llm);
            model.remote_model = remote_model.into();
            db.add_model(&model).unwrap();
        }

        assert_eq!(
            db.models_using_remote(&endpoint_id, "qwen3:latest")
                .unwrap(),
            vec!["a", "b"]
        );
        assert_eq!(
            db.models_using_remote(&endpoint_id, "qwen3").unwrap(),
            vec!["a", "b"]
        );
        assert_eq!(
            db.models_using_remote(&endpoint_id, "qwen3:8b").unwrap(),
            vec!["c"]
        );
    }

    // The health loop checked the old URL while an update moved the endpoint.
    #[test]
    fn health_checked_at_an_old_url_is_not_stored() {
        let db = db();
        let mut stored = endpoint("e", "http://127.0.0.1:8000", "");
        db.insert_endpoint(&stored).unwrap();
        let old_url = stored.base_url.clone();
        stored.base_url = Url::parse("http://127.0.0.1:9931").unwrap();
        db.update_endpoint(&stored).unwrap();

        let stale = EndpointHealth {
            state: EndpointState::Unreachable,
            checked_at: Some(1),
            detail: Some("old".into()),
            served_models: vec![],
        };
        let capabilities = vec![AiCapability::Embeddings];
        assert!(!db
            .set_endpoint_health(&stored.id, &old_url, &stale, &capabilities)
            .unwrap());
        let read = db.get_endpoint(&stored.id).unwrap().unwrap();
        assert_eq!(read.health, EndpointHealth::default());
        assert_eq!(read.capabilities, vec![AiCapability::Chat]);

        assert!(db
            .set_endpoint_health(&stored.id, &stored.base_url, &stale, &capabilities)
            .unwrap());
        let read = db.get_endpoint(&stored.id).unwrap().unwrap();
        assert_eq!(read.health, stale);
        assert_eq!(read.capabilities, capabilities);
    }

    #[test]
    fn a_model_moves_to_another_endpoint() {
        let db = db();
        let first = saved(&db, "first", "http://127.0.0.1:9931");
        let second = saved(&db, "second", "http://127.0.0.1:8000");
        let id = db.add_model(&on(&first.id, "m", ModelType::Llm)).unwrap();
        db.update_model(&id, &on(&second.id, "m", ModelType::Llm))
            .unwrap();
        assert_eq!(
            db.get_model(id.clone()).unwrap().unwrap().endpoint_id,
            second.id
        );
        assert!(db.endpoint_model_ids(&first.id).unwrap().is_empty());
        assert_eq!(db.endpoint_model_ids(&second.id).unwrap(), vec![id]);
    }

    // A backup restored into a fresh executor keeps the registry: an endpoint no model
    // uses, each endpoint's id, name, kind and capabilities, and the models' links.
    #[test]
    fn a_backup_restores_the_endpoints_and_links_the_models_to_them() {
        let db = db();
        let mut unused = endpoint("unused", "https://api.example.org/v1", "sk-unused");
        unused.capabilities = vec![AiCapability::Embeddings];
        db.insert_endpoint(&unused).unwrap();
        let llama = endpoint("llama", "http://127.0.0.1:9931", "");
        db.insert_endpoint(&llama).unwrap();
        let mut model = on(&llama.id, "m", ModelType::Llm);
        model.native_tools = Some(true);
        db.add_model(&model).unwrap();

        let restored = Ad4mDb::new(":memory:").unwrap();
        let result = restored
            .import_from_json(db.export_all_to_json().unwrap(), None)
            .unwrap();

        assert_eq!(
            (result.ai_endpoints.imported, result.ai_endpoints.failed),
            (2, 0)
        );
        let endpoints = restored.get_endpoints().unwrap();
        assert_eq!(endpoints.len(), 2);
        let unused_back = restored.get_endpoint(&unused.id).unwrap().unwrap();
        assert_eq!(unused_back.name, "unused");
        assert_eq!(unused_back.kind, EndpointKind::LlamaServer);
        assert_eq!(unused_back.capabilities, vec![AiCapability::Embeddings]);
        assert!(unused_back.api_key.expose_for_compare("sk-unused"));
        let model = restored.get_models().unwrap().remove(0);
        assert_eq!(model.endpoint_id, llama.id);
        assert_eq!(model.remote_model, "m-remote");
        assert_eq!(model.native_tools, Some(true));
    }
}
