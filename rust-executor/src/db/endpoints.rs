//! The `ai_endpoints` table: the upstream model servers, and the link from each remote
//! model (`models.endpoint_id`) to the endpoint that serves it.
//!
//! An endpoint owns the URL, the key and the health. A model still carries its own copy of
//! the URL, key and protocol in its `api_*` columns (models switch to endpoint references
//! only in a later step); [`Ad4mDb::update_endpoint`] rewrites those copies, so a linked
//! model always connects where its endpoint says. [`Ad4mDb::link_models_to_endpoints`]
//! gives every remote model an endpoint: at startup, and after each model write.

use rusqlite::{params, Connection, OptionalExtension};
use url::Url;

use super::{Ad4mDb, Ad4mDbResult, ApiKeyNotResent};
use crate::secret::Secret;
use crate::types::{
    same_endpoint, AiCapability, Endpoint, EndpointHealth, EndpointKind, ModelApiType, ModelType,
};

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
    for column in ["request_timeout_secs INTEGER", "max_concurrent INTEGER"] {
        match conn.execute(&format!("ALTER TABLE ai_endpoints ADD COLUMN {column}"), []) {
            Err(e) if !e.to_string().contains("duplicate column name") => return Err(e),
            _ => {}
        }
    }
    Ok(())
}

const ENDPOINT_COLUMNS: &str = "id, name, kind, base_url, api_key, capabilities, max_num_ctx, health, request_timeout_secs, max_concurrent";

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

/// The endpoint a model with this `api` would get when none serves it yet: named by its
/// host and port, of the kind its protocol and host say (plan §8.1 step 2).
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

    /// Refuses `endpoint` when another one has its name, or the same protocol, URL and key
    /// (a model links to the endpoint its `api` names, so two such endpoints could not be
    /// told apart), and a concurrency cap of 0, which would let no request through.
    fn refuse_invalid(&self, endpoint: &Endpoint) -> Ad4mDbResult<()> {
        if endpoint.max_concurrent == Some(0) {
            return Err(InvalidEndpoint(
                "maxConcurrent is at least 1; leave it out for no limit".into(),
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
            &format!("INSERT INTO ai_endpoints ({ENDPOINT_COLUMNS}) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)"),
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
            ],
        )?;
        Ok(())
    }

    /// Writes `endpoint` over the stored one, and its URL, key, protocol and context
    /// ceiling over the models it serves. Answers those models' ids: their clients have to
    /// be built again.
    pub fn update_endpoint(&self, endpoint: &Endpoint) -> Ad4mDbResult<Vec<String>> {
        self.refuse_invalid(endpoint)?;
        self.conn.execute(
            "UPDATE ai_endpoints SET name = ?2, kind = ?3, base_url = ?4, api_key = ?5,
                capabilities = ?6, max_num_ctx = ?7, health = ?8, request_timeout_secs = ?9,
                max_concurrent = ?10
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
            ],
        )?;
        self.conn.execute(
            "UPDATE models SET api_base_url = ?2, api_key = ?3, api_type = ?4, api_max_num_ctx = ?5
             WHERE endpoint_id = ?1",
            params![
                endpoint.id,
                endpoint.base_url.to_string(),
                endpoint.api_key.expose(),
                endpoint.kind.protocol().to_string(),
                endpoint.max_num_ctx,
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

    pub fn set_endpoint_health(&self, id: &str, health: &EndpointHealth) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE ai_endpoints SET health = ?2 WHERE id = ?1",
            params![id, serde_json::to_string(health)?],
        )?;
        Ok(())
    }

    /// The models that run on endpoint `id`.
    pub fn endpoint_model_ids(&self, id: &str) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self
            .conn
            .prepare("SELECT id FROM models WHERE endpoint_id = ?1 ORDER BY name")?;
        let ids = stmt.query_map(params![id], |row| row.get(0))?;
        Ok(ids.collect::<Result<_, _>>()?)
    }

    /// The names of the models that use `remote_model` on endpoint `id`.
    pub fn models_using_remote(&self, id: &str, remote_model: &str) -> Ad4mDbResult<Vec<String>> {
        let mut stmt = self.conn.prepare(
            "SELECT name FROM models WHERE endpoint_id = ?1 AND model = ?2 ORDER BY name",
        )?;
        let names = stmt.query_map(params![id, remote_model], |row| row.get(0))?;
        Ok(names.collect::<Result<_, _>>()?)
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

    /// Gives each remote model without an endpoint the one that serves its `api`: the
    /// endpoint with the same protocol, URL and key, else a new one named by the URL's host
    /// (plan §8.1 step 2). The endpoint gains the capability of each model type it serves.
    /// A no-op when every remote model has an endpoint.
    pub fn link_models_to_endpoints(&self) -> Ad4mDbResult<()> {
        let unlinked: Vec<(String, String, String, String, String, Option<u32>)> = {
            let mut stmt = self.conn.prepare(
                "SELECT id, api_base_url, COALESCE(api_key, ''), api_type, type, api_max_num_ctx
                 FROM models
                 WHERE endpoint_id IS NULL AND api_base_url IS NOT NULL AND api_type IS NOT NULL",
            )?;
            let rows = stmt.query_map([], |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            })?;
            rows.collect::<Result<_, _>>()?
        };

        for (model_id, base_url, api_key, api_type, model_type, max_num_ctx) in unlinked {
            let (Ok(base_url), Ok(api_type), Ok(model_type)) = (
                Url::parse(&base_url),
                api_type.parse::<ModelApiType>(),
                serde_json::from_str::<ModelType>(&model_type),
            ) else {
                log::warn!("Model {model_id} has an unreadable api; it gets no endpoint");
                continue;
            };
            let capability = AiCapability::from(&model_type);

            let existing = self.get_endpoints()?.into_iter().find(|endpoint| {
                endpoint.kind.protocol() == api_type
                    && same_endpoint(endpoint.base_url.as_str(), base_url.as_str())
                    && endpoint.api_key.expose_for_compare(&api_key)
            });
            let endpoint_id = match existing {
                Some(mut endpoint) => {
                    if !endpoint.capabilities.contains(&capability) {
                        endpoint.capabilities.push(capability);
                        endpoint.capabilities.sort();
                        self.conn.execute(
                            "UPDATE ai_endpoints SET capabilities = ?2 WHERE id = ?1",
                            params![endpoint.id, serde_json::to_string(&endpoint.capabilities)?],
                        )?;
                    }
                    endpoint.id
                }
                None => {
                    let (host_name, kind) = endpoint_for_api(&api_type, &base_url);
                    let taken: Vec<String> =
                        self.get_endpoints()?.into_iter().map(|e| e.name).collect();
                    let name = std::iter::once(host_name.clone())
                        .chain((2..).map(|n| format!("{host_name}-{n}")))
                        .find(|name| !taken.contains(name))
                        .expect("an unbounded list of names has a free one");
                    let endpoint = Endpoint {
                        id: uuid::Uuid::new_v4().to_string(),
                        name,
                        kind,
                        base_url,
                        api_key: Secret::new(api_key),
                        capabilities: vec![capability],
                        max_num_ctx,
                        request_timeout_secs: None,
                        max_concurrent: None,
                        health: EndpointHealth::default(),
                    };
                    self.insert_endpoint(&endpoint)?;
                    endpoint.id
                }
            };
            self.conn.execute(
                "UPDATE models SET endpoint_id = ?2 WHERE id = ?1",
                params![model_id, endpoint_id],
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{EndpointState, ModelApiInput, ModelInput};

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
            health: EndpointHealth::default(),
        }
    }

    fn remote(name: &str, url: &str, key: Option<&str>, api_type: &str) -> ModelInput {
        ModelInput {
            name: name.to_string(),
            api: Some(ModelApiInput {
                base_url: url.to_string(),
                api_key: key.map(Into::into),
                model: format!("{name}-remote"),
                api_type: api_type.to_string(),
                max_num_ctx: None,
                native_tools: None,
            }),
            local: None,
            model_type: ModelType::Llm,
        }
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

    // Plan §8.1 step 2: rows that share protocol, URL and key share one endpoint; a
    // different key gets its own; names come from the host and are suffixed on a clash.
    #[test]
    fn remote_models_are_linked_to_one_endpoint_per_protocol_url_and_key() {
        let db = db();
        let a = db
            .add_model(&remote(
                "a",
                "https://api.openai.com/v1",
                Some("k1"),
                "OPEN_AI",
            ))
            .unwrap();
        let b = db
            .add_model(&remote(
                "b",
                "https://api.openai.com/v1",
                Some("k1"),
                "OPEN_AI",
            ))
            .unwrap();
        let c = db
            .add_model(&remote(
                "c",
                "https://api.openai.com/v1",
                Some("k2"),
                "OPEN_AI",
            ))
            .unwrap();
        let d = db
            .add_model(&remote("d", "http://localhost:11434", None, "OLLAMA"))
            .unwrap();
        let local = db
            .add_model(&ModelInput {
                name: "local".into(),
                api: None,
                local: Some(crate::types::LocalModelInput {
                    file_name: "bert".into(),
                    ..Default::default()
                }),
                model_type: ModelType::Embedding,
            })
            .unwrap();

        let endpoint_of = |id: &str| db.get_model(id.to_string()).unwrap().unwrap().endpoint_id;
        assert_eq!(endpoint_of(&a), endpoint_of(&b));
        assert_ne!(endpoint_of(&a), endpoint_of(&c));
        assert_eq!(endpoint_of(&local), None, "a local model has no endpoint");

        let first = db.get_endpoint(&endpoint_of(&a).unwrap()).unwrap().unwrap();
        assert_eq!(first.name, "api.openai.com");
        assert_eq!(first.kind, EndpointKind::OpenAi);
        assert!(first.api_key.expose_for_compare("k1"));
        let second = db.get_endpoint(&endpoint_of(&c).unwrap()).unwrap().unwrap();
        assert_eq!(second.name, "api.openai.com-2");
        let ollama = db.get_endpoint(&endpoint_of(&d).unwrap()).unwrap().unwrap();
        assert_eq!(
            (ollama.name.as_str(), ollama.kind),
            ("localhost:11434", EndpointKind::Ollama)
        );
        assert_eq!(ollama.capabilities, vec![AiCapability::Chat]);

        // Linking again changes nothing.
        db.link_models_to_endpoints().unwrap();
        assert_eq!(db.get_endpoints().unwrap().len(), 3);
    }

    #[test]
    fn an_endpoint_update_rewrites_the_models_it_serves() {
        let db = db();
        let id = db
            .add_model(&remote("m", "http://127.0.0.1:9931", Some("k"), "OPEN_AI"))
            .unwrap();
        let endpoint_id = db
            .get_model(id.clone())
            .unwrap()
            .unwrap()
            .endpoint_id
            .unwrap();
        let mut endpoint = db.get_endpoint(&endpoint_id).unwrap().unwrap();
        endpoint.base_url = Url::parse("https://gpu.example.org/v1").unwrap();
        endpoint.api_key = Secret::new("k-new");
        endpoint.kind = EndpointKind::Ollama;
        endpoint.max_num_ctx = Some(4096);

        assert_eq!(db.update_endpoint(&endpoint).unwrap(), vec![id.clone()]);

        let api = db.get_model(id).unwrap().unwrap().api.unwrap();
        assert_eq!(api.base_url.as_str(), "https://gpu.example.org/v1");
        assert!(api.api_key.expose_for_compare("k-new"));
        assert_eq!(api.api_type, ModelApiType::Ollama);
        assert_eq!(api.max_num_ctx, Some(4096));
        assert_eq!(
            api.model, "m-remote",
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
        let id = db
            .add_model(&remote("m", "http://127.0.0.1:9931", None, "OPEN_AI"))
            .unwrap();
        let endpoint_id = db
            .get_model(id.clone())
            .unwrap()
            .unwrap()
            .endpoint_id
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

    #[test]
    fn a_model_moved_to_another_url_moves_to_its_endpoint() {
        let db = db();
        let id = db
            .add_model(&remote("m", "http://127.0.0.1:9931", None, "OPEN_AI"))
            .unwrap();
        let before = db.get_model(id.clone()).unwrap().unwrap().endpoint_id;
        db.update_model(&id, &remote("m", "http://127.0.0.1:8000", None, "OPEN_AI"))
            .unwrap();
        let after = db.get_model(id).unwrap().unwrap().endpoint_id;
        assert_ne!(before, after);
        let endpoint = db.get_endpoint(&after.unwrap()).unwrap().unwrap();
        assert_eq!(endpoint.base_url.as_str(), "http://127.0.0.1:8000/");
    }
}
