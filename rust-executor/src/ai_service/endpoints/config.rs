//! The config file's AI keys (`ai_endpoints`, `ai_models`, `ai_defaults`, plan §7.3) and
//! their apply at startup.
//!
//! The apply is idempotent by name: an entry that exists is brought to the file's settings,
//! one that does not is added, and nothing the file does not name is touched. It runs
//! before the AI service loads, so the models start with it, and makes no request: the
//! health loop checks the endpoints.

use serde::{Deserialize, Serialize};
use url::Url;

use super::super::providers::http::is_transport_safe;
use crate::db::Ad4mDb;
use crate::secret::Secret;
use crate::types::{
    AiCapability, Endpoint, EndpointHealth, EndpointKind, ModelApiInput, ModelInput, ModelType,
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
    for setting in &settings.endpoints {
        if let Err(e) = apply_endpoint(setting, &env) {
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

fn apply_endpoint(
    setting: &AiEndpointSetting,
    env: &impl Fn(&str) -> Option<String>,
) -> anyhow::Result<()> {
    let api_key = match &setting.api_key_env {
        Some(var) => Secret::new(
            env(var).ok_or_else(|| anyhow::anyhow!("api_key_env names {var}, which is not set"))?,
        ),
        None => Secret::new(""),
    };
    if !api_key.is_empty() && !is_transport_safe(&setting.base_url) {
        anyhow::bail!(
            "refusing to send its key over plain HTTP to {}",
            setting.base_url
        );
    }
    let existing = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name(&setting.name))?;
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
        None => Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)),
        Some(stored) if same_settings(&stored, &endpoint) => Ok(()),
        Some(_) => Ad4mDb::with_global_instance(|db| db.update_endpoint(&endpoint)).map(|_| ()),
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
    let endpoint = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name(&setting.endpoint))?
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
    let existing = Ad4mDb::with_global_instance(|db| db.get_models())?
        .into_iter()
        .find(|model| model.name == setting.name);
    match existing {
        None => Ad4mDb::with_global_instance(|db| db.add_model(&input)).map(|_| ()),
        Some(model)
            if model.endpoint_id.as_deref() == Some(endpoint.id.as_str())
                && model.model_type == setting.model_type
                && model.api.as_ref().is_some_and(|api| {
                    api.model == setting.remote_model && api.native_tools == setting.native_tools
                }) =>
        {
            Ok(())
        }
        Some(model) => Ad4mDb::with_global_instance(|db| db.update_model(&model.id, &input)),
    }
}

fn apply_default(model_type: ModelType, name: &str) -> anyhow::Result<()> {
    let model = Ad4mDb::with_global_instance(|db| db.get_models())?
        .into_iter()
        .find(|model| model.name == name && model.model_type == model_type)
        .ok_or_else(|| anyhow::anyhow!("no {model_type} model is named {name:?}"))?;
    Ad4mDb::with_global_instance(|db| db.set_default_model(model_type, &model.id))
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
