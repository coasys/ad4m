//! Model registration and defaults: the names of the managed endpoints, the registration of
//! a runtime as a managed endpoint with its models (the id requests name each model by,
//! native tools, the defaults of their types), the check that a running runtime still has
//! its registration, and the removal of managed endpoints.

use anyhow::{bail, Result};
use std::time::Duration;
use url::Url;

use super::data::{Provider, Service};
use super::supervisor::Phase;
use super::{AIService, SelectedModel, SelectedRuntime, Selection, MANAGED_PORTS};
use crate::secret::Secret;
use crate::types::{
    AiCapability, Endpoint, EndpointHealth, EndpointKind, EndpointState, ModelInput,
};

/// The names of the managed endpoints.
pub(super) const LLAMA_ENDPOINT: &str = "This device (llama.cpp)";
pub(super) const NEMO_SPEECH_ENDPOINT: &str = "This device (NeMo Speech)";

pub(super) fn endpoint_name(provider: Provider) -> &'static str {
    match provider {
        Provider::LlamaServer => LLAMA_ENDPOINT,
        Provider::NemoSpeech => NEMO_SPEECH_ENDPOINT,
    }
}

/// Whether `name` is one of the names local AI registers its endpoints under.
pub(crate) fn is_managed_endpoint_name(name: &str) -> bool {
    [Provider::LlamaServer, Provider::NemoSpeech]
        .iter()
        .any(|p| endpoint_name(*p) == name)
}

/// Whether `runtime`, running, lacks its registration: no endpoint recorded, the endpoint
/// gone, or a model of the selection without its row on it.
pub(super) fn registration_missing(
    selection: &Selection,
    runtime: &SelectedRuntime,
    endpoint_id: Option<&str>,
) -> Result<bool> {
    let Some(endpoint_id) = endpoint_id else {
        return Ok(true);
    };
    let db = crate::db_backend::db_backend();
    if db.get_endpoint(endpoint_id)?.is_none() {
        return Ok(true);
    }
    let rows = db.get_models()?;
    Ok(selection.models_of(runtime.provider).iter().any(|model| {
        !rows
            .iter()
            .any(|row| row.endpoint_id == endpoint_id && row.name == model.model_id)
    }))
}

/// The id requests name `model` by. llama-server's router serves each model under its preset
/// name, the manifest id. nemo-speech serves its model under the id it reads from the file
/// (Parakeet v3 Q8_0: `parakeet-tdt-0.6b-v3.oss-align.q8_0`) and refuses any other: the id its
/// `/v1/models` lists, else the id an earlier start registered, else the manifest id.
fn remote_model(
    provider: Provider,
    model: &SelectedModel,
    health: Option<&EndpointHealth>,
    registered: Option<&str>,
) -> String {
    match provider {
        Provider::LlamaServer => model.model_id.clone(),
        Provider::NemoSpeech => health
            .filter(|h| h.state == EndpointState::Reachable)
            .and_then(|h| h.served_models.first().cloned())
            .or_else(|| registered.map(str::to_string))
            .unwrap_or_else(|| model.model_id.clone()),
    }
}

/// Whether the executor sends a model tools as definitions. The managed chat model: yes.
/// llama-server renders them with the model's chat template (`--jinja`), parses the calls
/// and honours `parallel_tool_calls: false`; a call `tool_choice` forces (`"required"`, a
/// named function), which it does not enforce, `/v1` makes by a schema it does enforce
/// (`api::openai_compat::chat::constrained_tool_calls`).
fn native_tools(model: &SelectedModel) -> Option<bool> {
    (model.provider == Provider::LlamaServer && model.service == Service::Llm).then_some(true)
}

/// How often, and how far apart, registration asks a running nemo-speech for its listing.
const NEMO_LISTING_TRIES: usize = 5;
const NEMO_LISTING_WAIT: Duration = Duration::from_secs(1);

/// Whether the managed runtime on `port` is running.
fn runs_on(port: u16) -> bool {
    MANAGED_PORTS
        .read()
        .unwrap_or_else(|p| p.into_inner())
        .get(&port)
        .is_some_and(|m| m.status.borrow().phase == Phase::Running)
}

/// Register `runtime` as a managed endpoint with its models; make each the default of its
/// type when `make_default`, or when the type has none. Answers the endpoint id.
pub(super) async fn register(
    selection: &Selection,
    runtime: &SelectedRuntime,
    port: u16,
    key: &str,
    make_default: bool,
) -> Result<String> {
    let models = selection.models_of(runtime.provider);
    let (kind, capabilities) = match runtime.provider {
        Provider::LlamaServer => (
            EndpointKind::LlamaServer,
            models
                .iter()
                .map(|m| m.service.capability())
                .collect::<Vec<AiCapability>>(),
        ),
        Provider::NemoSpeech => (EndpointKind::OpenAiAudio, vec![AiCapability::Transcription]),
    };
    let mut capabilities = capabilities;
    capabilities.sort();
    capabilities.dedup();
    let name = endpoint_name(runtime.provider);
    let existing = crate::db_backend::db_backend().get_endpoint_by_name(name)?;
    if existing.as_ref().is_some_and(|e| !e.managed) {
        bail!("An endpoint named {name:?} exists already and is not local AI's: rename it");
    }
    let endpoint = Endpoint {
        id: existing
            .as_ref()
            .map(|e| e.id.clone())
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
        name: name.to_string(),
        kind,
        base_url: Url::parse(&format!("http://127.0.0.1:{port}"))?,
        api_key: Secret::new(key),
        capabilities,
        max_num_ctx: None,
        request_timeout_secs: None,
        max_concurrent: None,
        managed: true,
        // The stored health, so the check below publishes a change.
        health: existing
            .as_ref()
            .map(|e| e.health.clone())
            .unwrap_or_default(),
    };
    let service = AIService::global_instance().await?;
    match &existing {
        None => crate::db_backend::db_backend().insert_endpoint(&endpoint)?,
        Some(_) => {
            for model_id in crate::db_backend::db_backend().update_endpoint(&endpoint)? {
                if let Err(e) = service.reload_model(model_id.clone()).await {
                    log::warn!("Local AI: model {model_id} did not reload: {e:#}");
                }
            }
        }
    }
    let mut health = super::endpoints::check_health(&endpoint)
        .await
        .map_err(|e| log::warn!("Local AI: health of {name}: {e}"))
        .ok();
    // nemo-speech serves its model only under the id its listing names: a listing that
    // failed once would register the manifest id, which it refuses, and nothing corrects
    // it later. A running one gets a few more tries.
    if runtime.provider == Provider::NemoSpeech {
        for _ in 0..NEMO_LISTING_TRIES {
            let listed = health.as_ref().is_some_and(|h| {
                h.state == EndpointState::Reachable && !h.served_models.is_empty()
            });
            if listed || !runs_on(port) {
                break;
            }
            tokio::time::sleep(NEMO_LISTING_WAIT).await;
            health = super::endpoints::probe_health(&endpoint).await.ok();
        }
    }

    // A model a new selection dropped (fewer services, a re-plan): its row goes, and with
    // it a default that named it. Before the models below: a type whose default goes gets
    // the selection's model of that type as its default, also when this start does not make
    // its models the defaults (a re-plan installed with `start: false`, then `ai.localStart`
    // or the start at executor start).
    for row in crate::db_backend::db_backend().get_models()? {
        if row.endpoint_id == endpoint.id && !models.iter().any(|m| m.model_id == row.name) {
            service.remove_model(row.id).await?;
        }
    }
    let rows = crate::db_backend::db_backend().get_models()?;
    for model in &models {
        let model_type = model.service.model_type();
        let row = rows.iter().find(|row| row.name == model.model_id);
        let input = ModelInput {
            name: model.model_id.clone(),
            model_type: model_type.clone(),
            endpoint_id: endpoint.id.clone(),
            remote_model: remote_model(
                runtime.provider,
                model,
                health.as_ref(),
                row.filter(|r| r.endpoint_id == endpoint.id)
                    .map(|r| r.remote_model.as_str()),
            ),
            native_tools: native_tools(model),
        };
        let id = match row {
            Some(row) if row.endpoint_id == endpoint.id => {
                if row.model_type != model_type
                    || row.remote_model != input.remote_model
                    || row.native_tools != input.native_tools
                {
                    service.update_model(row.id.clone(), input).await?;
                }
                row.id.clone()
            }
            Some(_) => {
                log::warn!(
                    "Local AI: a model named {:?} exists on another endpoint; left as it is",
                    model.model_id
                );
                continue;
            }
            None => service.add_model(input).await?,
        };
        let has_default = crate::db_backend::db_backend()
            .get_default_model(model_type.clone())?
            .is_some_and(|default| rows.iter().any(|r| r.id == default) || default == id);
        if make_default || !has_default {
            service.set_default_model(model_type, id).await?;
        }
    }
    Ok(endpoint.id)
}

/// Remove the managed endpoints `chosen` picks, with their models (and the defaults that
/// named them).
pub(super) async fn unregister(chosen: impl Fn(&Endpoint) -> bool) -> Result<()> {
    let db = crate::db_backend::db_backend();
    let dropped: Vec<Endpoint> = db
        .get_endpoints()?
        .into_iter()
        .filter(|e| e.managed && chosen(e))
        .collect();
    if dropped.is_empty() {
        return Ok(());
    }
    let service = AIService::global_instance().await?;
    for endpoint in dropped {
        for model_id in db.endpoint_model_ids(&endpoint.id)? {
            service.remove_model(model_id).await?;
        }
        db.remove_endpoint(&endpoint.id)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::local::hardware::Backend;
    use crate::ai_service::local::runtimes::llama_presets;
    use crate::ai_service::local::supervisor::{Readiness, StopHandle};
    use crate::ai_service::local::test_support::{selected, status_in, TEST_LOCK};
    use crate::ai_service::local::ManagedPort;
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    /// nemo-speech refuses a model name it does not list: requests name the listed id.
    #[test]
    fn nemo_speech_models_are_registered_under_the_id_the_server_lists() {
        let model = selected(
            Service::Stt,
            "parakeet-tdt-0.6b-v3-q8_0",
            Provider::NemoSpeech,
        );
        let listed = EndpointHealth {
            state: EndpointState::Reachable,
            checked_at: None,
            detail: None,
            served_models: vec!["parakeet-tdt-0.6b-v3.oss-align.q8_0".into()],
        };
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, Some(&listed), Some("old")),
            "parakeet-tdt-0.6b-v3.oss-align.q8_0"
        );
        let down = EndpointHealth {
            state: EndpointState::Unreachable,
            served_models: vec![],
            ..listed.clone()
        };
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, Some(&down), Some("old")),
            "old"
        );
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, None, None),
            "parakeet-tdt-0.6b-v3-q8_0"
        );
        let chat = selected(Service::Llm, "qwen", Provider::LlamaServer);
        assert_eq!(
            remote_model(Provider::LlamaServer, &chat, Some(&listed), None),
            "qwen"
        );
    }

    /// The managed chat model takes tools as definitions, rendered by its chat template.
    #[test]
    fn the_managed_chat_model_takes_tools_as_definitions_through_its_template() {
        let chat = selected(Service::Llm, "qwen", Provider::LlamaServer);
        let embed = selected(Service::Embeddings, "nomic", Provider::LlamaServer);
        let stt = selected(Service::Stt, "parakeet", Provider::NemoSpeech);
        assert_eq!(native_tools(&chat), Some(true));
        assert_eq!(native_tools(&embed), None);
        assert_eq!(native_tools(&stt), None);
        let ini = llama_presets(&[&chat, &embed]);
        let section = |name: &str| {
            ini.split(&format!("[{name}]"))
                .nth(1)
                .unwrap()
                .split("\n[")
                .next()
                .unwrap()
                .to_string()
        };
        assert!(section("qwen").contains("jinja = true"), "{ini}");
        assert!(!section("nomic").contains("jinja"), "{ini}");
        let mut off = chat.clone();
        off.server_flags = vec!["--no-jinja".into()];
        let ini = llama_presets(&[&off]);
        assert!(
            ini.contains("no-jinja = true") && !ini.contains("\njinja"),
            "{ini}"
        );
    }

    /// A nemo-speech whose first listing fails: registration asks again, and registers the
    /// model under the id the listing names, not the manifest id the server refuses.
    #[tokio::test]
    async fn registration_asks_nemo_speech_again_for_its_listing() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let clear = || async {
            let service = AIService::global_instance().await.unwrap();
            for endpoint in db.get_endpoints().unwrap() {
                if endpoint.managed {
                    for id in db.endpoint_model_ids(&endpoint.id).unwrap() {
                        service.remove_model(id).await.unwrap();
                    }
                    db.remove_endpoint(&endpoint.id).unwrap();
                }
            }
        };
        clear().await;
        // The first request gets a 503, the ones after it the listing.
        let listener = tokio::net::TcpListener::bind(("127.0.0.1", 0))
            .await
            .unwrap();
        let port = listener.local_addr().unwrap().port();
        let server = tokio::spawn(async move {
            let mut answered = 0;
            while let Ok((mut socket, _)) = listener.accept().await {
                let mut request = vec![0u8; 4096];
                let _ = socket.read(&mut request).await;
                let body = r#"{"object":"list","data":[{"id":"listed-id","object":"model"}]}"#;
                let answer = if answered == 0 {
                    "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".to_string()
                } else {
                    format!(
                        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
                        body.len()
                    )
                };
                answered += 1;
                let _ = socket.write_all(answer.as_bytes()).await;
                let _ = socket.shutdown().await;
            }
        });
        let (_supervisor, status) = tokio::sync::watch::channel(status_in(Phase::Running));
        MANAGED_PORTS.write().unwrap().insert(
            port,
            ManagedPort {
                status,
                readiness: Readiness::Http("/ready".into()),
                served: vec!["parakeet-test".into()],
                lists_models: true,
                stop: StopHandle::default(),
            },
        );
        let runtime = SelectedRuntime {
            provider: Provider::NemoSpeech,
            runtime_id: "nemo-speech".into(),
            version: "0".into(),
            build: "linux-x64-cpu".into(),
            binary: PathBuf::from("/store/runtime/nemo-speech"),
            lib_dirs: vec![],
            services: vec![Service::Stt],
            backend: Some(Backend::Cpu),
        };
        let selection = Selection {
            runtimes: vec![runtime.clone()],
            models: vec![selected(
                Service::Stt,
                "parakeet-test",
                Provider::NemoSpeech,
            )],
            ports: BTreeMap::new(),
        };
        let outcome = register(&selection, &runtime, port, "k", false).await;
        let remote = outcome.as_ref().ok().and_then(|endpoint| {
            db.endpoint_model_ids(endpoint)
                .unwrap()
                .into_iter()
                .filter_map(|id| db.get_model(id).unwrap())
                .find(|m| m.name == "parakeet-test")
                .map(|m| m.remote_model)
        });
        MANAGED_PORTS.write().unwrap().remove(&port);
        clear().await;
        *super::super::AI_SERVICE.lock().await = saved;
        server.abort();

        outcome.unwrap();
        assert_eq!(remote.as_deref(), Some("listed-id"));
    }

    /// A new selection that drops a model (`services` given, or a re-plan): its row on the
    /// managed endpoint goes, and with it the default it was.
    #[tokio::test]
    async fn registration_removes_the_models_a_new_selection_drops() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let clear = || async {
            let service = AIService::global_instance().await.unwrap();
            for endpoint in db.get_endpoints().unwrap() {
                if endpoint.managed {
                    for id in db.endpoint_model_ids(&endpoint.id).unwrap() {
                        service.remove_model(id).await.unwrap();
                    }
                    db.remove_endpoint(&endpoint.id).unwrap();
                }
            }
        };
        clear().await;

        let runtime = SelectedRuntime {
            provider: Provider::LlamaServer,
            runtime_id: "llama.cpp".into(),
            version: "b1".into(),
            build: "linux-x64-cpu".into(),
            binary: PathBuf::from("/store/runtime/llama-server"),
            lib_dirs: vec![],
            services: vec![Service::Llm, Service::Embeddings],
            backend: Some(Backend::Cpu),
        };
        let chat = selected(Service::Llm, "wt-chat", Provider::LlamaServer);
        let embed = selected(Service::Embeddings, "wt-embed", Provider::LlamaServer);
        let both = Selection {
            runtimes: vec![runtime.clone()],
            models: vec![chat.clone(), embed],
            ports: BTreeMap::new(),
        };
        let names = |endpoint: &str| -> Vec<String> {
            let mut names: Vec<String> = db
                .endpoint_model_ids(endpoint)
                .unwrap()
                .into_iter()
                .filter_map(|id| db.get_model(id).unwrap().map(|m| m.name))
                .collect();
            names.sort();
            names
        };

        let endpoint = register(&both, &runtime, 40011, "k3y", true).await.unwrap();
        let before = names(&endpoint);
        let default_before = db
            .get_default_model(crate::types::ModelType::Embedding)
            .unwrap();
        let chat_only = Selection {
            runtimes: vec![runtime.clone()],
            models: vec![chat],
            ports: BTreeMap::new(),
        };
        let again = register(&chat_only, &runtime, 40011, "k3y", true)
            .await
            .unwrap();
        let after = names(&endpoint);
        let default_after = db
            .get_default_model(crate::types::ModelType::Embedding)
            .unwrap();
        let chat_default = db.get_default_model(crate::types::ModelType::Llm).unwrap();
        clear().await;
        *super::super::AI_SERVICE.lock().await = saved;

        assert_eq!(again, endpoint);
        assert_eq!(before, vec!["wt-chat", "wt-embed"]);
        assert!(default_before.is_some());
        assert_eq!(after, vec!["wt-chat"], "the dropped model's row is gone");
        assert_eq!(default_after, None, "no default names the removed model");
        assert!(chat_default.is_some());
    }

    /// A re-plan that changes the chat model and starts without making its models the
    /// defaults (`ai.localStart`, autostart): the old model's row goes, and the new
    /// model becomes the default the old one was. The executor keeps a default chat model.
    #[tokio::test]
    async fn a_replan_keeps_a_default_of_each_type_it_registers() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let clear = || async {
            let service = AIService::global_instance().await.unwrap();
            for endpoint in db.get_endpoints().unwrap() {
                if endpoint.managed {
                    for id in db.endpoint_model_ids(&endpoint.id).unwrap() {
                        service.remove_model(id).await.unwrap();
                    }
                    db.remove_endpoint(&endpoint.id).unwrap();
                }
            }
        };
        clear().await;
        let runtime = SelectedRuntime {
            provider: Provider::LlamaServer,
            runtime_id: "llama.cpp".into(),
            version: "b1".into(),
            build: "linux-x64-cpu".into(),
            binary: PathBuf::from("/store/runtime/llama-server"),
            lib_dirs: vec![],
            services: vec![Service::Llm],
            backend: Some(Backend::Cpu),
        };
        let with = |id: &str| Selection {
            runtimes: vec![runtime.clone()],
            models: vec![selected(Service::Llm, id, Provider::LlamaServer)],
            ports: BTreeMap::new(),
        };
        let endpoint = register(&with("old-chat"), &runtime, 40014, "k", true)
            .await
            .unwrap();
        register(&with("new-chat"), &runtime, 40014, "k", false)
            .await
            .unwrap();
        let default = db.get_default_model(crate::types::ModelType::Llm).unwrap();
        let new_chat = db
            .endpoint_model_ids(&endpoint)
            .unwrap()
            .into_iter()
            .find(|id| {
                db.get_model(id.clone())
                    .unwrap()
                    .is_some_and(|m| m.name == "new-chat")
            });
        clear().await;
        *super::super::AI_SERVICE.lock().await = saved;

        assert!(new_chat.is_some(), "the new chat model is registered");
        assert_eq!(default, new_chat, "the new chat model is the default");
    }

    /// `ai.localInstall({services: ["llm"]})` after a set with transcription: the start
    /// does not start nemo-speech again, and its endpoint goes (its health would say to
    /// start it with `ai.localStart`, which cannot). llama.cpp's stays.
    #[tokio::test]
    async fn a_runtime_the_selection_drops_loses_its_endpoint() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let managed = |name: &str, port: u16| Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name: name.into(),
            kind: EndpointKind::LlamaServer,
            base_url: Url::parse(&format!("http://127.0.0.1:{port}")).unwrap(),
            api_key: Secret::new("k"),
            capabilities: vec![],
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
            managed: true,
            health: Default::default(),
        };
        for endpoint in db.get_endpoints().unwrap() {
            if endpoint.managed {
                db.remove_endpoint(&endpoint.id).unwrap();
            }
        }
        db.insert_endpoint(&managed(LLAMA_ENDPOINT, 40012)).unwrap();
        db.insert_endpoint(&managed(NEMO_SPEECH_ENDPOINT, 40013))
            .unwrap();

        let kept = [LLAMA_ENDPOINT];
        let outcome = unregister(|e| !kept.contains(&e.name.as_str())).await;
        let names: Vec<String> = db
            .get_endpoints()
            .unwrap()
            .into_iter()
            .filter(|e| e.managed)
            .map(|e| e.name)
            .collect();
        for endpoint in db.get_endpoints().unwrap() {
            if endpoint.managed {
                db.remove_endpoint(&endpoint.id).unwrap();
            }
        }
        *super::super::AI_SERVICE.lock().await = saved;

        outcome.unwrap();
        assert_eq!(names, vec![LLAMA_ENDPOINT.to_string()]);
    }
}
