//! The config file's AI keys (`ai_endpoints`, `ai_models`, `ai_defaults`, plan §7.3) and
//! their apply at startup.
//!
//! The apply is idempotent by name: an entry that exists is brought to the file's settings,
//! one that does not is added, and nothing the file does not name is touched. An endpoint
//! entry whose name is new takes over the endpoint the start-up migration made for the same
//! protocol, URL and key, so a model that used the server before stays on it. It runs
//! before the AI service loads, so the models start with it, and makes no request: the
//! health loop checks the endpoints.

use serde::{Deserialize, Serialize};
use url::Url;

use super::super::providers::http::is_transport_safe;
use crate::secret::Secret;
use crate::types::{
    same_endpoint, AiCapability, Endpoint, EndpointHealth, EndpointKind, Model, ModelApiInput,
    ModelInput, ModelType,
};

/// One `ai_endpoints` entry. The key is read from the environment variable `api_key_env`
/// names, never written in the file.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AiEndpointSetting {
    pub name: String,
    pub kind: EndpointKind,
    pub base_url: Url,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_key_env: Option<String>,
    /// Unset: the capabilities the endpoint has, or its kind's default.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<Vec<AiCapability>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_num_ctx: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub request_timeout_secs: Option<u32>,
    /// Unset: no limit.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_concurrent: Option<u32>,
}

/// One `ai_models` entry: `remote_model` on the endpoint named `endpoint`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AiModelSetting {
    pub name: String,
    pub model_type: ModelType,
    pub endpoint: String,
    pub remote_model: String,
    /// See `ModelApiView::native_tools`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub native_tools: Option<bool>,
}

/// `ai_defaults`: the default model of each type, by model name.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AiDefaultsSetting {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub llm: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub embedding: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub transcription: Option<String>,
}

/// The three keys together, as the executor config carries them.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AiSettings {
    pub endpoints: Vec<AiEndpointSetting>,
    pub models: Vec<AiModelSetting>,
    pub defaults: AiDefaultsSetting,
}

/// Bring the database to `settings`. `env` reads an environment variable. An entry that
/// cannot apply (a key variable that is not set, a key over plain HTTP, an unknown
/// endpoint or model name) is skipped with a warning naming it; the others apply.
pub fn apply(settings: &AiSettings, env: impl Fn(&str) -> Option<String>) {
    let named: Vec<&str> = settings.endpoints.iter().map(|e| e.name.as_str()).collect();
    for setting in &settings.endpoints {
        if let Err(e) = apply_endpoint(setting, &named, &env) {
            log::warn!("Config ai_endpoints {:?} not applied: {e:#}", setting.name);
        }
    }
    for setting in &settings.models {
        if let Err(e) = apply_model(setting) {
            log::warn!("Config ai_models {:?} not applied: {e:#}", setting.name);
        }
    }
    let defaults = [
        (ModelType::Llm, &settings.defaults.llm),
        (ModelType::Embedding, &settings.defaults.embedding),
        (ModelType::Transcription, &settings.defaults.transcription),
    ];
    for (model_type, name) in defaults {
        let Some(name) = name else { continue };
        if let Err(e) = apply_default(model_type.clone(), name) {
            log::warn!("Config ai_defaults {model_type} = {name:?} not applied: {e:#}");
        }
    }
}

/// `named`: the endpoint names the file gives.
fn apply_endpoint(
    setting: &AiEndpointSetting,
    named: &[&str],
    env: &impl Fn(&str) -> Option<String>,
) -> anyhow::Result<()> {
    let api_key = match &setting.api_key_env {
        // An empty value is no key, like an unset one: storing it would clear the key.
        Some(var) => match env(var) {
            None => anyhow::bail!("api_key_env names {var}, which is not set"),
            Some(value) if value.is_empty() => {
                anyhow::bail!("api_key_env names {var}, which is empty")
            }
            Some(value) => Secret::new(value),
        },
        None => Secret::new(""),
    };
    if setting.max_num_ctx == Some(0) {
        anyhow::bail!("max_num_ctx must be positive");
    }
    if super::has_credentials(&setting.base_url) {
        anyhow::bail!("{}", super::URL_CREDENTIALS_REFUSAL);
    }
    if !api_key.is_empty() && !is_transport_safe(&setting.base_url) {
        anyhow::bail!(
            "refusing to send its key over plain HTTP to {}",
            setting.base_url
        );
    }
    let existing = match crate::db_backend::db_backend().get_endpoint_by_name(&setting.name)? {
        Some(stored) => Some(stored),
        // The start-up migration names the endpoint of an existing remote model by its
        // host. The entry for that server takes it over (and renames it), unless another
        // entry of the file names it: the registry refuses two endpoints for one server.
        None => crate::db_backend::db_backend()
            .get_endpoints()?
            .into_iter()
            .find(|stored| {
                stored.kind.protocol() == setting.kind.protocol()
                    && same_endpoint(stored.base_url.as_str(), setting.base_url.as_str())
                    && stored.api_key.expose_for_compare(api_key.expose())
                    && !named.contains(&stored.name.as_str())
            }),
    };
    let endpoint = Endpoint {
        id: existing
            .as_ref()
            .map(|e| e.id.clone())
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
        name: setting.name.clone(),
        kind: setting.kind,
        base_url: setting.base_url.clone(),
        api_key,
        capabilities: setting
            .capabilities
            .clone()
            .or_else(|| existing.as_ref().map(|e| e.capabilities.clone()))
            .unwrap_or_else(|| setting.kind.default_capabilities()),
        max_num_ctx: setting.max_num_ctx,
        request_timeout_secs: setting.request_timeout_secs,
        max_concurrent: setting.max_concurrent,
        health: existing
            .as_ref()
            .map(|e| e.health.clone())
            .unwrap_or_else(EndpointHealth::default),
    };
    match existing {
        None => crate::db_backend::db_backend().insert_endpoint(&endpoint),
        Some(stored) if stored.name == endpoint.name && same_settings(&stored, &endpoint) => Ok(()),
        Some(_) => crate::db_backend::db_backend()
            .update_endpoint(&endpoint)
            .map(|_| ()),
    }
}

fn same_settings(stored: &Endpoint, wanted: &Endpoint) -> bool {
    stored.kind == wanted.kind
        && stored.base_url == wanted.base_url
        && stored.api_key.expose_for_compare(wanted.api_key.expose())
        && stored.capabilities == wanted.capabilities
        && stored.max_num_ctx == wanted.max_num_ctx
        && stored.request_timeout_secs == wanted.request_timeout_secs
        && stored.max_concurrent == wanted.max_concurrent
}

fn apply_model(setting: &AiModelSetting) -> anyhow::Result<()> {
    let endpoint = crate::db_backend::db_backend()
        .get_endpoint_by_name(&setting.endpoint)?
        .ok_or_else(|| anyhow::anyhow!("no endpoint is named {:?}", setting.endpoint))?;
    let input = ModelInput {
        name: setting.name.clone(),
        api: Some(ModelApiInput {
            base_url: endpoint.base_url.to_string(),
            api_key: Some(endpoint.api_key.clone()),
            model: setting.remote_model.clone(),
            api_type: endpoint.kind.protocol().to_string(),
            max_num_ctx: endpoint.max_num_ctx,
            native_tools: setting.native_tools,
        }),
        local: None,
        model_type: setting.model_type.clone(),
    };
    let existing = model_named(&setting.name, |_| true)?;
    match existing {
        None => crate::db_backend::db_backend()
            .add_model(&input)
            .map(|_| ()),
        Some(model)
            if model.endpoint_id.as_deref() == Some(endpoint.id.as_str())
                && model.model_type == setting.model_type
                && model.api.as_ref().is_some_and(|api| {
                    api.model == setting.remote_model && api.native_tools == setting.native_tools
                }) =>
        {
            Ok(())
        }
        Some(model) => crate::db_backend::db_backend().update_model(&model.id, &input),
    }
}

fn apply_default(model_type: ModelType, name: &str) -> anyhow::Result<()> {
    let model = model_named(name, |model| model.model_type == model_type)?
        .ok_or_else(|| anyhow::anyhow!("no {model_type} model is named {name:?}"))?;
    crate::db_backend::db_backend().set_default_model(model_type, &model.id)
}

/// The one model named `name` that `wanted` accepts. Model names are not unique: when two
/// have the name, the file does not say which one it means, so neither is changed.
fn model_named(name: &str, wanted: impl Fn(&Model) -> bool) -> anyhow::Result<Option<Model>> {
    let mut named = crate::db_backend::db_backend()
        .get_models()?
        .into_iter()
        .filter(|model| model.name == name && wanted(model));
    let first = named.next();
    if named.next().is_some() {
        anyhow::bail!("more than one model is named {name:?}");
    }
    Ok(first)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;

    fn settings() -> AiSettings {
        serde_json::from_value(serde_json::json!({
            "endpoints": [
                { "name": "ollama", "kind": "OLLAMA", "base_url": "http://127.0.0.1:11434",
                  "max_num_ctx": 8192 },
                { "name": "openai", "kind": "OPEN_AI", "base_url": "https://api.openai.com/v1",
                  "api_key_env": "AI_CONFIG_TEST_KEY" },
            ],
            "models": [
                { "name": "qwen", "model_type": "LLM", "endpoint": "ollama",
                  "remote_model": "qwen3:8b" },
                { "name": "gpt", "model_type": "LLM", "endpoint": "openai",
                  "remote_model": "gpt-5", "native_tools": true },
                { "name": "nomic", "model_type": "EMBEDDING", "endpoint": "ollama",
                  "remote_model": "nomic-embed-text" },
            ],
            "defaults": { "llm": "qwen", "embedding": "nomic" },
        }))
        .unwrap()
    }

    fn env(var: &str) -> Option<String> {
        (var == "AI_CONFIG_TEST_KEY").then(|| "sk-from-env".to_string())
    }

    #[test]
    fn applying_twice_registers_each_entry_once_with_its_key_and_defaults() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        apply(&settings(), env);
        apply(&settings(), env);

        let (endpoints, models, llm, embedding) = Ad4mDb::with_global_instance(|db| {
            Ok::<_, anyhow::Error>((
                db.get_endpoints()?,
                db.get_models()?,
                db.get_default_model(ModelType::Llm)?,
                db.get_default_model(ModelType::Embedding)?,
            ))
        })
        .unwrap();

        assert_eq!(
            endpoints
                .iter()
                .map(|e| e.name.as_str())
                .collect::<Vec<_>>(),
            vec!["ollama", "openai"]
        );
        let openai = &endpoints[1];
        assert!(openai.api_key.expose_for_compare("sk-from-env"));
        assert_eq!(
            openai.capabilities,
            EndpointKind::OpenAi.default_capabilities()
        );
        assert_eq!(models.len(), 3);
        let by_name = |name: &str| models.iter().find(|m| m.name == name).unwrap();
        let gpt = by_name("gpt");
        assert_eq!(gpt.endpoint_id.as_deref(), Some(openai.id.as_str()));
        assert!(gpt
            .api
            .as_ref()
            .unwrap()
            .api_key
            .expose_for_compare("sk-from-env"));
        assert_eq!(gpt.api.as_ref().unwrap().native_tools, Some(true));
        let qwen = by_name("qwen");
        assert_eq!(qwen.api.as_ref().unwrap().max_num_ctx, Some(8192));
        assert_eq!(llm.as_deref(), Some(qwen.id.as_str()));
        assert_eq!(embedding.as_deref(), Some(by_name("nomic").id.as_str()));
    }

    #[test]
    fn an_entry_takes_over_the_endpoint_the_migration_made_for_its_server() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        // A model the launcher added before the registry: the migration gives it an
        // endpoint named by its host.
        let launcher_model = Ad4mDb::with_global_instance(|db| {
            let id = db.add_model(&ModelInput {
                name: "launcher-qwen".into(),
                api: Some(ModelApiInput {
                    base_url: "http://127.0.0.1:11434".into(),
                    api_key: None,
                    model: "qwen3:8b".into(),
                    api_type: "OLLAMA".into(),
                    max_num_ctx: None,
                    native_tools: None,
                }),
                local: None,
                model_type: ModelType::Llm,
            })?;
            db.link_models_to_endpoints()?;
            Ok::<_, anyhow::Error>(id)
        })
        .unwrap();
        let migrated = Ad4mDb::with_global_instance(|db| db.get_endpoints()).unwrap();
        assert_eq!(migrated.len(), 1);
        assert_eq!(migrated[0].name, "127.0.0.1:11434");

        apply(&settings(), env);
        apply(&settings(), env);

        let (endpoints, models, llm) = Ad4mDb::with_global_instance(|db| {
            Ok::<_, anyhow::Error>((
                db.get_endpoints()?,
                db.get_models()?,
                db.get_default_model(ModelType::Llm)?,
            ))
        })
        .unwrap();
        assert_eq!(
            endpoints
                .iter()
                .map(|e| e.name.as_str())
                .collect::<Vec<_>>(),
            vec!["ollama", "openai"]
        );
        let ollama = &endpoints[0];
        assert_eq!(ollama.id, migrated[0].id, "taken over, not added");
        assert_eq!(ollama.kind, EndpointKind::Ollama);
        assert_eq!(ollama.max_num_ctx, Some(8192));
        let endpoint_of = |name: &str| {
            models
                .iter()
                .find(|m| m.name == name)
                .unwrap()
                .endpoint_id
                .clone()
        };
        assert_eq!(endpoint_of("launcher-qwen"), Some(ollama.id.clone()));
        assert_eq!(endpoint_of("qwen"), Some(ollama.id.clone()));
        let qwen = models.iter().find(|m| m.name == "qwen").unwrap();
        assert_eq!(llm.as_deref(), Some(qwen.id.as_str()));
        assert_ne!(qwen.id, launcher_model);
    }

    #[test]
    fn a_changed_file_updates_its_entries_in_place() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        apply(&settings(), env);
        let mut changed = settings();
        changed.endpoints[0].base_url = Url::parse("http://127.0.0.1:11435").unwrap();
        changed.models[0].remote_model = "qwen3:14b".into();
        apply(&changed, env);

        let models = Ad4mDb::with_global_instance(|db| db.get_models()).unwrap();
        let qwen = models.iter().find(|m| m.name == "qwen").unwrap();
        let api = qwen.api.as_ref().unwrap();
        assert_eq!(api.model, "qwen3:14b");
        assert_eq!(api.base_url.as_str(), "http://127.0.0.1:11435/");
        let endpoints = Ad4mDb::with_global_instance(|db| db.get_endpoints()).unwrap();
        assert_eq!(endpoints.len(), 2, "updated in place, not added");
        assert_eq!(qwen.endpoint_id.as_deref(), Some(endpoints[0].id.as_str()));
    }

    #[test]
    fn an_entry_that_cannot_apply_is_skipped_and_the_rest_apply() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        // No key variable: the OpenAI endpoint and its model are skipped.
        apply(&settings(), |_| None);
        let (endpoints, models) = Ad4mDb::with_global_instance(|db| {
            Ok::<_, anyhow::Error>((db.get_endpoints()?, db.get_models()?))
        })
        .unwrap();
        assert_eq!(endpoints.len(), 1);
        assert_eq!(models.len(), 2);
    }

    #[test]
    fn a_key_variable_that_is_unset_or_empty_is_refused_with_its_own_reason() {
        let openai = &settings().endpoints[1];
        let unset = apply_endpoint(openai, &[], &|_| None).unwrap_err();
        assert_eq!(
            unset.to_string(),
            "api_key_env names AI_CONFIG_TEST_KEY, which is not set"
        );
        let empty = apply_endpoint(openai, &[], &|_| Some(String::new())).unwrap_err();
        assert_eq!(
            empty.to_string(),
            "api_key_env names AI_CONFIG_TEST_KEY, which is empty"
        );
    }

    // Model names are not unique: the file cannot say which of two it means.
    #[test]
    fn a_model_name_two_models_have_is_left_alone() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let twin = |remote_model: &str| ModelInput {
            name: "qwen".into(),
            api: Some(ModelApiInput {
                base_url: "http://127.0.0.1:9931".into(),
                api_key: None,
                model: remote_model.into(),
                api_type: "OPEN_AI".into(),
                max_num_ctx: None,
                native_tools: None,
            }),
            local: None,
            model_type: ModelType::Llm,
        };
        Ad4mDb::with_global_instance(|db| {
            db.add_model(&twin("a"))?;
            db.add_model(&twin("b"))
        })
        .unwrap();

        apply(&settings(), env);

        let (models, llm) = Ad4mDb::with_global_instance(|db| {
            Ok::<_, anyhow::Error>((db.get_models()?, db.get_default_model(ModelType::Llm)?))
        })
        .unwrap();
        let mut remotes: Vec<String> = models
            .iter()
            .filter(|m| m.name == "qwen")
            .map(|m| m.api.as_ref().unwrap().model.clone())
            .collect();
        remotes.sort();
        assert_eq!(remotes, vec!["a", "b"]);
        assert_eq!(llm, None);
    }

    #[test]
    fn an_endpoint_with_a_credential_in_its_url_is_skipped() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let mut with_password = settings();
        with_password.endpoints[0].base_url =
            Url::parse("http://user:pass@127.0.0.1:11434").unwrap();
        apply(&with_password, env);
        let endpoints = Ad4mDb::with_global_instance(|db| db.get_endpoints()).unwrap();
        assert_eq!(
            endpoints
                .iter()
                .map(|e| e.name.as_str())
                .collect::<Vec<_>>(),
            vec!["openai"]
        );
    }
}
