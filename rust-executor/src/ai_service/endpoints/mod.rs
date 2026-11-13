//! The endpoint registry: add, update and remove the upstream model servers, check their
//! health, and pull or delete Ollama models.
//!
//! Storage is `db/endpoints.rs`; listing and health per kind are
//! `providers::discovery`. This module adds what needs the network or the model workers:
//! the probe before a save, the reload of the models an endpoint update moves, the
//! 60-second health loop, the `model-status` / `ai-endpoint-status` events, and the
//! per-request limits an operator sets on an endpoint (timeout, concurrency cap).

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, OwnedSemaphorePermit, Semaphore};

use super::providers::discovery::{
    discover_for, endpoint_capabilities, health_of, listing_or_registered, PROBE_TIMEOUT,
};
use super::providers::http::{
    is_transport_safe, UpstreamStatus, UpstreamUnreachable, CLEARTEXT_KEY_REFUSAL,
};
use super::providers::ollama;
use super::{AIService, Result};
use crate::agent::AgentService;
use crate::db::InvalidEndpoint;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use crate::secret::Secret;
use crate::types::{
    AiCapability, AiEndpointStatusEvent, DiscoveredModel, Endpoint, EndpointHealth, EndpointInput,
    EndpointKind, EndpointState, EndpointView, ModelState, ModelStatus, ModelStatusEvent,
};

pub mod config;
pub mod detect;

/// How often the health loop checks every endpoint.
const HEALTH_INTERVAL: Duration = Duration::from_secs(60);

/// At most this often a pull publishes its progress (4 per second).
const PULL_EVENT_INTERVAL: Duration = Duration::from_millis(250);

lazy_static! {
    /// The semaphore of each endpoint with a concurrency cap, with the cap it was made for.
    static ref SLOTS: std::sync::Mutex<HashMap<String, (u32, Arc<Semaphore>)>> =
        std::sync::Mutex::new(HashMap::new());
}

/// How long one request to `endpoint` may take, queueing included: the operator's
/// `request_timeout_secs`, else `default` (the route's).
pub fn request_timeout(endpoint: &Endpoint, default: Duration) -> Duration {
    endpoint
        .request_timeout_secs
        .map(|secs| Duration::from_secs(secs.into()))
        .unwrap_or(default)
}

/// A slot for one request to `endpoint`, once one is free. `None` when the operator set no
/// `max_concurrent`: no limit, the upstream queues on its own. Hold it until the answer is
/// read to its end.
pub async fn request_slot(endpoint: &Endpoint) -> Option<OwnedSemaphorePermit> {
    let cap = endpoint.max_concurrent?;
    let semaphore = {
        let mut slots = SLOTS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        match slots.get(&endpoint.id) {
            Some((size, semaphore)) if *size == cap => semaphore.clone(),
            // New, or the cap changed: requests already holding a slot of the old one finish.
            _ => {
                let semaphore = Arc::new(Semaphore::new(cap as usize));
                slots.insert(endpoint.id.clone(), (cap, semaphore.clone()));
                semaphore
            }
        }
    };
    semaphore.acquire_owned().await.ok()
}

/// No endpoint has this id. Missing is the caller's mistake: 404.
#[derive(Debug)]
pub struct EndpointNotFound;

impl std::fmt::Display for EndpointNotFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No AI endpoint with this id")
    }
}

impl std::error::Error for EndpointNotFound {}

fn stored_endpoint(id: &str) -> Result<Endpoint> {
    crate::db_backend::db_backend()
        .get_endpoint(id)?
        .ok_or_else(|| EndpointNotFound.into())
}

/// What `endpoint` serves: its listing, or for an audio server without one the models
/// registered on it ([`listing_or_registered`]).
async fn list_models(endpoint: &Endpoint) -> anyhow::Result<Vec<DiscoveredModel>> {
    let listing = discover_for(
        endpoint.kind,
        endpoint.base_url.clone(),
        endpoint.api_key.expose(),
        PROBE_TIMEOUT,
    )
    .list_models()
    .await;
    listing_or_registered(endpoint.kind, listing, || {
        crate::db_backend::db_backend()
            .endpoint_remote_models(&endpoint.id)
            .unwrap_or_else(|e| {
                log::warn!(
                    "AI endpoint {:?}: could not read its models: {e}",
                    endpoint.name
                );
                Vec::new()
            })
    })
}

/// List the endpoint's models once: its health and, unless the operator named them, its
/// capabilities. An endpoint that does not answer still saves, with that health and its
/// kind's default capabilities: a server is often set up before it is started.
async fn probe(endpoint: &mut Endpoint, capabilities: Option<Vec<AiCapability>>) {
    let listing = list_models(endpoint).await;
    endpoint.capabilities = match (capabilities, &listing) {
        (Some(given), _) => given,
        (None, Ok(models)) => endpoint_capabilities(endpoint.kind, models),
        (None, Err(_)) => endpoint.kind.default_capabilities(),
    };
    endpoint.health = health_of(listing);
}

/// What an operator is told when a base URL carries a user name or password.
pub(crate) const URL_CREDENTIALS_REFUSAL: &str =
    "Refusing a URL with a user name or password in it. Give the key as the API key.";

/// Whether `url` carries a user name or password. reqwest sends them as Basic auth, and
/// the URL is shown and logged where a key never is.
pub(crate) fn has_credentials(url: &url::Url) -> bool {
    !url.username().is_empty() || url.password().is_some()
}

/// Refuses a credential in the URL, and a key that would cross the network in the clear
/// (the rule of `ai.discoverModels`).
fn refuse_unsafe_key(endpoint: &Endpoint) -> Result<()> {
    if has_credentials(&endpoint.base_url) {
        return Err(InvalidEndpoint(URL_CREDENTIALS_REFUSAL.to_string()).into());
    }
    if !endpoint.api_key.is_empty() && !is_transport_safe(&endpoint.base_url) {
        return Err(InvalidEndpoint(CLEARTEXT_KEY_REFUSAL.to_string()).into());
    }
    Ok(())
}

/// Refuses an operator's write to an endpoint the local AI supervisor runs: its URL and key
/// change at every start, so an edit would not hold. `ai.localStop` / `ai.localRemove` do
/// what an operator wants there.
fn refuse_managed(endpoint: &Endpoint) -> Result<()> {
    if endpoint.managed {
        return Err(InvalidEndpoint(format!(
            "Endpoint {:?} is this executor's local AI: use ai.localStop or ai.localRemove",
            endpoint.name
        ))
        .into());
    }
    Ok(())
}

/// Refuses a context ceiling of 0: it is copied to the models as Ollama's `num_ctx`.
fn refuse_zero_ceiling(endpoint: &Endpoint) -> Result<()> {
    if endpoint.max_num_ctx == Some(0) {
        return Err(InvalidEndpoint("max_num_ctx must be positive".to_string()).into());
    }
    Ok(())
}

/// Publish an endpoint's health and capabilities to the sessions that may administer the
/// executor: those acting as the main agent (the operator and grants on it), never a
/// managed user (`ai.endpoints` is operator-only). Before the main agent has a DID, the
/// operator's only.
async fn publish_health(endpoint_id: &str, health: &EndpointHealth, capabilities: &[AiCapability]) {
    let main_did = AgentService::global_instance()
        .lock()
        .ok()
        .and_then(|agent| agent.as_ref().and_then(|agent| agent.did.clone()));
    let audience = match main_did {
        Some(did) => Audience::Dids(vec![did]),
        None => Audience::Operator,
    };
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AiEndpointStatus,
        audience,
        &AiEndpointStatusEvent {
            endpoint_id: endpoint_id.to_string(),
            health: health.clone(),
            capabilities: capabilities.to_vec(),
        },
    ));
}

/// Publish where an upstream model stands. Model ids and states only: every session with
/// AI_READ sees them, as it sees `ai.models`.
pub(crate) async fn publish_model_status(
    endpoint_id: &str,
    remote_model: &str,
    status: ModelStatus,
) {
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AiModelStatus,
        Audience::Everyone,
        &ModelStatusEvent {
            endpoint_id: endpoint_id.to_string(),
            remote_model: remote_model.to_string(),
            status,
        },
    ));
}

/// Publish `model-status` for each upstream model on `before` whose status `health` changes.
/// Managed users do not get `ai-endpoint-status` (it names the URL): this is how they learn
/// that a model's endpoint went down or came back.
async fn publish_model_changes(before: &Endpoint, health: &EndpointHealth) {
    let models = match crate::db_backend::db_backend().get_models() {
        Ok(models) => models,
        Err(e) => {
            log::warn!("AI endpoint health: could not read the models: {e}");
            return;
        }
    };
    let after = Endpoint {
        health: health.clone(),
        ..before.clone()
    };
    let mut remote_models: Vec<String> = models
        .into_iter()
        .filter(|model| model.endpoint_id == before.id)
        .map(|model| model.remote_model)
        .collect();
    remote_models.sort();
    remote_models.dedup();
    for remote_model in remote_models {
        let status = after.model_status(&remote_model);
        if status.state != before.model_status(&remote_model).state {
            publish_model_status(&before.id, &remote_model, status).await;
        }
    }
}

/// A check this soon after the last probe of the same endpoint answers the stored health
/// and sends nothing: a burst of checks, or of requests that failed on one endpoint,
/// probes it once. As long as a probe may take, so one in flight is not doubled.
const HEALTH_DEBOUNCE: Duration = PROBE_TIMEOUT;

/// When each endpoint was last probed.
fn last_probes() -> &'static std::sync::Mutex<std::collections::HashMap<String, Instant>> {
    static LAST_PROBES: std::sync::OnceLock<
        std::sync::Mutex<std::collections::HashMap<String, Instant>>,
    > = std::sync::OnceLock::new();
    LAST_PROBES.get_or_init(Default::default)
}

/// Check `endpoint`: [`probe_health`], unless it was probed less than
/// [`HEALTH_DEBOUNCE`] ago, then its stored health.
pub async fn check_health(endpoint: &Endpoint) -> Result<EndpointHealth> {
    let debounced = {
        let mut last = last_probes()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let now = Instant::now();
        match last.get(&endpoint.id) {
            Some(at) if now.duration_since(*at) < HEALTH_DEBOUNCE => true,
            _ => {
                last.insert(endpoint.id.clone(), now);
                false
            }
        }
    };
    if debounced {
        let stored = crate::db_backend::db_backend().get_endpoint(&endpoint.id)?;
        return Ok(stored.map_or_else(|| endpoint.health.clone(), |stored| stored.health));
    }
    probe_health(endpoint).await
}

/// Probe `endpoint` now: store its health, and the capabilities of the models it now
/// serves added to its own, and publish them when they changed. Nothing is stored or
/// published when the endpoint moved to another URL during the check. A pull or a delete
/// calls it directly: what the endpoint serves has just changed.
pub(crate) async fn probe_health(endpoint: &Endpoint) -> Result<EndpointHealth> {
    let mut capabilities = endpoint.capabilities.clone();
    // Add and update refuse such an endpoint, but a migrated or imported one can hold a
    // key for a plain-HTTP remote host, or a credential in its URL: probing it would send
    // the credential.
    let health = if endpoint.managed {
        // The supervisor knows a runtime that is starting or stopped; a running one is
        // checked by its listing (nemo-speech) or liveness (llama-server).
        crate::ai_service::local::managed_health(endpoint).await
    } else if let Err(refusal) = refuse_unsafe_key(endpoint) {
        EndpointHealth {
            state: EndpointState::Unreachable,
            checked_at: Some(chrono::Utc::now().timestamp_millis()),
            detail: Some(refusal.to_string()),
            served_models: Vec::new(),
        }
    } else {
        let listing = list_models(endpoint).await;
        // A model pulled or loaded since the endpoint was added adds what it does. What the
        // endpoint had stays: the operator may have set it. OpenAI's and Anthropic's
        // capabilities come from their kind, not from the listing.
        if let Ok(models) = &listing {
            if !matches!(
                endpoint.kind,
                EndpointKind::OpenAi | EndpointKind::Anthropic
            ) {
                capabilities.extend(models.iter().flat_map(|m| m.capabilities.iter().copied()));
                capabilities.sort();
                capabilities.dedup();
            }
        }
        health_of(listing)
    };
    let stored = crate::db_backend::db_backend().set_endpoint_health(
        &endpoint.id,
        &endpoint.base_url,
        &health,
        &capabilities,
    )?;
    let changed = health.state != endpoint.health.state
        || health.served_models != endpoint.health.served_models
        || health.detail != endpoint.health.detail
        || capabilities != endpoint.capabilities;
    if stored && changed {
        publish_health(&endpoint.id, &health, &capabilities).await;
        publish_model_changes(endpoint, &health).await;
    }
    Ok(health)
}

/// What a failed request says about its endpoint.
#[derive(Debug, PartialEq)]
enum FailureSays {
    /// No connection, or the key refused: the endpoint's health may have changed.
    CheckEndpoint,
    /// The endpoint answered 404: it does not serve the model.
    ModelMissing,
}

impl FailureSays {
    fn of(error: &anyhow::Error) -> Option<Self> {
        if let Some(unreachable) = error.downcast_ref::<UpstreamUnreachable>() {
            return (!unreachable.timed_out).then_some(Self::CheckEndpoint);
        }
        if let Some(answered) = error.downcast_ref::<UpstreamStatus>() {
            return match answered.status.as_u16() {
                401 | 403 => Some(Self::CheckEndpoint),
                404 => Some(Self::ModelMissing),
                _ => None,
            };
        }
        // A client that keeps reqwest's error (transcription) rather than wrapping it.
        let refused = error
            .chain()
            .filter_map(|cause| cause.downcast_ref::<reqwest::Error>())
            .any(reqwest::Error::is_connect);
        refused.then_some(Self::CheckEndpoint)
    }
}

/// Act, in the background, on what a failed request for `remote_model` on endpoint
/// `endpoint_id` says: down or refusing the key re-checks the endpoint's health (which
/// publishes the change), a missing model is published `MISSING`. The one rule for `/v1`,
/// prompts, embeddings and transcription streams, so a status does not wait for the health
/// loop.
pub fn after_failed_request(endpoint_id: &str, remote_model: &str, error: &anyhow::Error) {
    let Some(says) = FailureSays::of(error) else {
        return;
    };
    let (endpoint_id, remote_model) = (endpoint_id.to_string(), remote_model.to_string());
    tokio::spawn(async move {
        match says {
            FailureSays::CheckEndpoint => {
                // The stored endpoint: a client's copy holds the health of when it was built.
                let checked = match stored_endpoint(&endpoint_id) {
                    Ok(endpoint) => check_health(&endpoint).await.map(drop),
                    Err(e) => Err(e),
                };
                if let Err(e) = checked {
                    log::warn!("AI endpoint health after a failed request: {e}");
                }
            }
            FailureSays::ModelMissing => {
                let status = ModelStatus {
                    state: ModelState::Missing,
                    progress: None,
                    detail: Some("The endpoint does not serve this model".into()),
                };
                publish_model_status(&endpoint_id, &remote_model, status).await;
            }
        }
    });
}

/// Check every endpoint now and then every [`HEALTH_INTERVAL`], for as long as the
/// executor runs.
pub fn spawn_health_loop() {
    tokio::spawn(async {
        let mut interval = tokio::time::interval(HEALTH_INTERVAL);
        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        loop {
            interval.tick().await;
            let endpoints = match crate::db_backend::db_backend().get_endpoints() {
                Ok(endpoints) => endpoints,
                Err(e) => {
                    log::warn!("AI endpoint health: could not read the endpoints: {e}");
                    continue;
                }
            };
            for result in futures::future::join_all(endpoints.iter().map(check_health)).await {
                if let Err(e) = result {
                    log::warn!("AI endpoint health: {e}");
                }
            }
        }
    });
}

/// The endpoint `id` as an Ollama, or the refusal: pulls and deletes are Ollama's only.
fn ollama_endpoint(id: &str) -> Result<Endpoint> {
    let endpoint = stored_endpoint(id)?;
    if endpoint.kind != EndpointKind::Ollama {
        return Err(InvalidEndpoint(format!(
            "Endpoint {:?} is not an Ollama: only Ollama pulls and deletes models",
            endpoint.name
        ))
        .into());
    }
    Ok(endpoint)
}

impl AIService {
    pub fn endpoints() -> Result<Vec<EndpointView>> {
        Ok(crate::db_backend::db_backend()
            .get_endpoints()?
            .iter()
            .map(Endpoint::view)
            .collect())
    }

    /// Register an endpoint, probed once first.
    pub async fn add_endpoint(&self, input: EndpointInput) -> Result<EndpointView> {
        let mut endpoint = Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name: input.name,
            kind: input.kind,
            base_url: input.base_url,
            api_key: input.api_key.unwrap_or_else(|| Secret::new("")),
            capabilities: Vec::new(),
            max_num_ctx: input.max_num_ctx,
            request_timeout_secs: input.request_timeout_secs,
            max_concurrent: input.max_concurrent,
            managed: false,
            health: EndpointHealth::default(),
        };
        refuse_unsafe_key(&endpoint)?;
        refuse_zero_ceiling(&endpoint)?;
        probe(&mut endpoint, input.capabilities).await;
        crate::db_backend::db_backend().insert_endpoint(&endpoint)?;
        Ok(endpoint.view())
    }

    /// Replace endpoint `id`'s settings, probed once first. Without a key the stored one
    /// stays, for the same base URL only. The models it serves follow it.
    pub async fn update_endpoint(&self, id: &str, input: EndpointInput) -> Result<EndpointView> {
        refuse_managed(&stored_endpoint(id)?)?;
        let api_key = match input.api_key {
            Some(key) => key,
            None => crate::db_backend::db_backend().kept_endpoint_key(id, &input.base_url)?,
        };
        let mut endpoint = Endpoint {
            id: id.to_string(),
            name: input.name,
            kind: input.kind,
            base_url: input.base_url,
            api_key,
            capabilities: Vec::new(),
            max_num_ctx: input.max_num_ctx,
            request_timeout_secs: input.request_timeout_secs,
            max_concurrent: input.max_concurrent,
            managed: false,
            health: EndpointHealth::default(),
        };
        refuse_unsafe_key(&endpoint)?;
        refuse_zero_ceiling(&endpoint)?;
        probe(&mut endpoint, input.capabilities).await;
        let models = crate::db_backend::db_backend().update_endpoint(&endpoint)?;
        for model_id in models {
            if let Err(e) = self.reload_model(model_id.clone()).await {
                log::warn!("Model {model_id} did not reload after its endpoint changed: {e:#}");
            }
        }
        publish_health(&endpoint.id, &endpoint.health, &endpoint.capabilities).await;
        Ok(endpoint.view())
    }

    /// Remove endpoint `id`. Refused while models run on it, unless `cascade` removes them
    /// first.
    pub async fn remove_endpoint(&self, id: &str, cascade: bool) -> Result<()> {
        refuse_managed(&stored_endpoint(id)?)?;
        if cascade {
            for model_id in crate::db_backend::db_backend().endpoint_model_ids(id)? {
                self.remove_model(model_id).await?;
            }
        }
        crate::db_backend::db_backend().remove_endpoint(id)
    }

    /// Pull `remote_model` onto the Ollama endpoint `endpoint_id`, publishing `model-status`
    /// with the progress (at most 4 times a second), then `READY` or `MISSING` with the
    /// reason. The endpoint's served models are refreshed after.
    pub async fn pull_model(&self, endpoint_id: &str, remote_model: &str) -> Result<()> {
        let endpoint = ollama_endpoint(endpoint_id)?;
        let (progress_tx, mut progress_rx) = mpsc::unbounded_channel();
        let pull = ollama::pull_model(&endpoint.base_url, remote_model, move |progress| {
            let _ = progress_tx.send(progress);
        });
        let publish = async {
            let mut last: Option<Instant> = None;
            while let Some(progress) = progress_rx.recv().await {
                if last.is_some_and(|at| at.elapsed() < PULL_EVENT_INTERVAL) {
                    continue;
                }
                last = Some(Instant::now());
                let status = ModelStatus {
                    state: ModelState::Pulling,
                    progress: progress.percent,
                    detail: Some(progress.status),
                };
                publish_model_status(endpoint_id, remote_model, status).await;
            }
        };
        let (outcome, ()) = tokio::join!(pull, publish);

        let status = match &outcome {
            Ok(()) => ModelStatus {
                state: ModelState::Ready,
                progress: None,
                detail: None,
            },
            // Every AI_READ session sees the event: the error names the endpoint's URL, so it
            // goes to the log and to the caller only.
            Err(e) => {
                log::warn!("The pull of {remote_model} on endpoint {endpoint_id} failed: {e:#}");
                ModelStatus {
                    state: ModelState::Missing,
                    progress: None,
                    detail: Some(format!("The pull of {remote_model} failed")),
                }
            }
        };
        publish_model_status(endpoint_id, remote_model, status).await;
        if let Err(e) = probe_health(&endpoint).await {
            log::warn!("AI endpoint health after a pull: {e}");
        }
        outcome
    }

    /// Delete `remote_model` from the Ollama endpoint `endpoint_id`. Refused while a model
    /// uses it.
    pub async fn delete_upstream_model(&self, endpoint_id: &str, remote_model: &str) -> Result<()> {
        let endpoint = ollama_endpoint(endpoint_id)?;
        let users =
            crate::db_backend::db_backend().models_using_remote(endpoint_id, remote_model)?;
        if !users.is_empty() {
            return Err(InvalidEndpoint(format!(
                "{remote_model} is used by model(s) {}: remove them first",
                users.join(", ")
            ))
            .into());
        }
        ollama::delete_model(&endpoint.base_url, remote_model).await?;
        if let Err(e) = probe_health(&endpoint).await {
            log::warn!("AI endpoint health after a delete: {e}");
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::types::AiCapability;

    fn input(name: &str, url: &str, key: Option<&str>) -> EndpointInput {
        EndpointInput {
            name: name.to_string(),
            kind: EndpointKind::Vllm,
            base_url: url::Url::parse(url).unwrap(),
            api_key: key.map(Into::into),
            capabilities: None,
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
        }
    }

    async fn service() -> AIService {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AIService::init_global_instance().await.unwrap();
        // `AIService::new` spawns `load()`. Let it run now, on the empty database: run
        // later, it would also load the models a test adds, and an embedding model loads
        // local weights and announces it to every session after the test has ended.
        tokio::task::yield_now().await;
        AIService::global_instance().await.unwrap()
    }

    #[tokio::test]
    async fn an_added_endpoint_is_probed_for_health_and_capabilities() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .match_header("authorization", "Bearer sk-probe")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"qwen3"},{"id":"bge-m3"}]}"#)
            .create_async()
            .await;

        let view = service
            .add_endpoint(input("vllm", &server.url(), Some("sk-probe")))
            .await
            .unwrap();

        assert_eq!(view.health.state, EndpointState::Reachable);
        assert_eq!(view.health.served_models, vec!["qwen3", "bge-m3"]);
        assert_eq!(
            view.capabilities,
            vec![AiCapability::Chat, AiCapability::Embeddings]
        );
        assert!(view.has_api_key);
        assert_eq!(AIService::endpoints().unwrap(), vec![view]);
    }

    // whisper.cpp's server: transcribes, answers 404 on the listing.
    #[tokio::test]
    async fn an_audio_server_without_a_listing_is_reachable_with_its_registered_models() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(404)
            .with_body("File Not Found")
            .create_async()
            .await;
        let mut given = input("whisper", &server.url(), None);
        given.kind = EndpointKind::OpenAiAudio;
        let view = service.add_endpoint(given).await.unwrap();
        assert_eq!(view.health.state, EndpointState::Reachable);
        assert_eq!(view.capabilities, vec![AiCapability::Transcription]);

        crate::db_backend::db_backend()
            .add_model(&crate::types::ModelInput {
                name: "whisper".into(),
                model_type: crate::types::ModelType::Transcription,
                endpoint_id: view.id.clone(),
                remote_model: "whisper-1".into(),
                native_tools: None,
            })
            .unwrap();
        let endpoint = stored_endpoint(&view.id).unwrap();
        let health = check_health(&endpoint).await.unwrap();

        assert_eq!(health.state, EndpointState::Reachable);
        assert_eq!(health.served_models, vec!["whisper-1"]);
    }

    #[tokio::test]
    async fn an_unreachable_endpoint_saves_with_its_health_and_the_operators_capabilities() {
        let service = service().await;
        let mut given = input("down", "http://127.0.0.1:1", None);
        given.capabilities = Some(vec![AiCapability::Transcription]);

        let view = service.add_endpoint(given).await.unwrap();

        assert_eq!(view.health.state, EndpointState::Unreachable);
        assert_eq!(view.capabilities, vec![AiCapability::Transcription]);
    }

    #[tokio::test]
    async fn a_key_over_plain_http_to_a_remote_host_is_refused() {
        let service = service().await;
        let error = service
            .add_endpoint(input("remote", "http://203.0.113.9:8000", Some("sk")))
            .await
            .unwrap_err();
        assert!(error.downcast_ref::<InvalidEndpoint>().is_some(), "{error}");
        assert!(AIService::endpoints().unwrap().is_empty());
    }

    #[tokio::test]
    async fn the_health_check_sends_no_key_over_plain_http_to_a_remote_host() {
        // Add and update refuse such an endpoint, but the migration from model-owned
        // connections or an import stores one as it was.
        let service = service().await;
        service
            .add_endpoint(input("legacy", "http://127.0.0.1:1", Some("sk-legacy")))
            .await
            .unwrap();
        let mut endpoint = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name("legacy"))
            .unwrap()
            .unwrap();
        endpoint.base_url = url::Url::parse("http://203.0.113.9:8000/v1").unwrap();

        let health = check_health(&endpoint).await.unwrap();

        assert_eq!(health.state, EndpointState::Unreachable);
        assert_eq!(health.detail.as_deref(), Some(CLEARTEXT_KEY_REFUSAL));
    }

    #[tokio::test]
    async fn an_update_without_a_key_keeps_it_and_a_missing_endpoint_is_not_found() {
        let service = service().await;
        let added = service
            .add_endpoint(input("e", "http://127.0.0.1:1", Some("sk")))
            .await
            .unwrap();

        let updated = service
            .update_endpoint(&added.id, input("renamed", "http://127.0.0.1:1", None))
            .await
            .unwrap();
        assert_eq!(updated.name, "renamed");
        assert!(updated.has_api_key);

        let error = service
            .update_endpoint("nope", input("x", "http://127.0.0.1:1", None))
            .await
            .unwrap_err();
        assert!(error.downcast_ref::<EndpointNotFound>().is_some());
    }

    /// The next `model-status` event about `endpoint_id`, or `None` within a second.
    async fn next_model_status(
        events: &mut tokio::sync::broadcast::Receiver<Arc<Event>>,
        endpoint_id: &str,
    ) -> Option<Arc<Event>> {
        let next = async {
            loop {
                let event = events.recv().await.ok()?;
                if event.topic == Topic::AiModelStatus && event.payload["endpointId"] == endpoint_id
                {
                    return Some(event);
                }
            }
        };
        tokio::time::timeout(Duration::from_secs(1), next)
            .await
            .ok()
            .flatten()
    }

    // Managed users do not get `ai-endpoint-status` (it names the URL): the `model-status`
    // of each model on the endpoint is how they learn it went down or came back.
    #[tokio::test]
    async fn a_change_of_health_publishes_the_status_of_each_model_on_the_endpoint() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"qwen3"}]}"#)
            .create_async()
            .await;
        let view = service
            .add_endpoint(input("vllm", &server.url(), None))
            .await
            .unwrap();
        // Two models on one upstream model: one event names them both.
        for name in ["a", "b"] {
            service
                .add_model(crate::types::ModelInput {
                    name: name.into(),
                    model_type: crate::types::ModelType::Llm,
                    endpoint_id: view.id.clone(),
                    remote_model: "qwen3".into(),
                    native_tools: None,
                })
                .await
                .unwrap();
        }
        let mut events = get_global_pubsub().await.subscribe_events();

        // A check stores and publishes only what it found at the stored URL, and probes an
        // endpoint once per `HEALTH_DEBOUNCE`: each step moves the stored endpoint and
        // forgets the last probe.
        let moved_to = |url: &str| {
            let mut endpoint = stored_endpoint(&view.id).unwrap();
            endpoint.base_url = url::Url::parse(url).unwrap();
            crate::db_backend::db_backend()
                .update_endpoint(&endpoint)
                .unwrap();
            last_probes().lock().unwrap().remove(&view.id);
            endpoint
        };

        let down = moved_to("http://127.0.0.1:1");
        check_health(&down).await.unwrap();
        let event = next_model_status(&mut events, &view.id)
            .await
            .expect("no model-status when the endpoint went down");
        assert_eq!(event.audience, Audience::Everyone);
        assert_eq!(event.payload["remoteModel"], "qwen3");
        assert_eq!(event.payload["status"]["state"], "ENDPOINT_DOWN");
        assert!(
            !event.payload.to_string().contains("127.0.0.1"),
            "{}",
            event.payload
        );
        assert!(
            next_model_status(&mut events, &view.id).await.is_none(),
            "one event per upstream model"
        );

        check_health(&moved_to(&server.url())).await.unwrap();
        let event = next_model_status(&mut events, &view.id)
            .await
            .expect("no model-status when the endpoint came back");
        assert_eq!(event.payload["status"]["state"], "READY");

        // Unchanged: nothing.
        check_health(&moved_to(&server.url())).await.unwrap();
        assert!(next_model_status(&mut events, &view.id).await.is_none());
    }

    #[tokio::test]
    async fn pulls_and_deletes_are_refused_off_ollama() {
        let service = service().await;
        let added = service
            .add_endpoint(input("vllm", "http://127.0.0.1:1", None))
            .await
            .unwrap();
        let pull = service.pull_model(&added.id, "m").await.unwrap_err();
        assert!(pull.downcast_ref::<InvalidEndpoint>().is_some(), "{pull}");
        let delete = service
            .delete_upstream_model(&added.id, "m")
            .await
            .unwrap_err();
        assert!(
            delete.downcast_ref::<InvalidEndpoint>().is_some(),
            "{delete}"
        );
    }

    /// The next event of `topic` about endpoint `endpoint_id`.
    async fn next_event(
        events: &mut tokio::sync::broadcast::Receiver<std::sync::Arc<Event>>,
        topic: Topic,
        endpoint_id: &str,
    ) -> std::sync::Arc<Event> {
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let event = events.recv().await.expect("the event channel stays open");
                if event.topic == topic && event.payload["endpointId"] == endpoint_id {
                    return event;
                }
            }
        })
        .await
        .expect("the event arrives")
    }

    #[tokio::test]
    async fn a_credential_in_the_url_is_refused_and_never_sent() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        let listing = server
            .mock("GET", "/v1/models")
            .expect(0)
            .create_async()
            .await;
        let mut with_password = server.url().parse::<url::Url>().unwrap();
        with_password.set_username("user").unwrap();
        with_password.set_password(Some("pass")).unwrap();

        let refused = service
            .add_endpoint(input("e", with_password.as_str(), None))
            .await
            .unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());

        // A migrated or imported endpoint can hold one: its check sends nothing.
        let added = service
            .add_endpoint(input("e", "http://127.0.0.1:1", None))
            .await
            .unwrap();
        let mut endpoint = stored_endpoint(&added.id).unwrap();
        endpoint.base_url = with_password;
        let health = check_health(&endpoint).await.unwrap();
        assert_eq!(health.state, EndpointState::Unreachable);
        assert_eq!(health.detail.as_deref(), Some(URL_CREDENTIALS_REFUSAL));
        listing.assert_async().await;
    }

    // A managed user's session must not get it: `ai.endpoints` is operator-only.
    // `each_audience_reaches_exactly_its_sessions` shows who a main-agent audience reaches.
    #[tokio::test]
    async fn endpoint_status_goes_to_the_main_agents_sessions_only() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main_did = AgentService::global_instance()
            .lock()
            .unwrap()
            .as_ref()
            .and_then(|agent| agent.did.clone())
            .expect("the main agent has a DID");
        let mut events = get_global_pubsub().await.subscribe_events();

        publish_health("audience-test", &EndpointHealth::default(), &[]).await;

        let event = next_event(&mut events, Topic::AiEndpointStatus, "audience-test").await;
        assert_eq!(event.audience, Audience::Dids(vec![main_did]));
    }

    // An endpoint added while it served chat only, then given an embedding model: the
    // next check makes it advertise embeddings, and says so.
    #[tokio::test]
    async fn a_check_adds_what_a_newly_served_model_does_and_publishes_it() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        let chat_only = server
            .mock("GET", "/v1/models")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"qwen3"}]}"#)
            .create_async()
            .await;
        let view = service
            .add_endpoint(input("vllm", &server.url(), None))
            .await
            .unwrap();
        assert_eq!(view.capabilities, vec![AiCapability::Chat]);
        chat_only.remove_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"qwen3"},{"id":"nomic-embed-text"}]}"#)
            .create_async()
            .await;
        let mut events = get_global_pubsub().await.subscribe_events();

        check_health(&stored_endpoint(&view.id).unwrap())
            .await
            .unwrap();

        let both = vec![AiCapability::Chat, AiCapability::Embeddings];
        assert_eq!(stored_endpoint(&view.id).unwrap().capabilities, both);
        let event = next_event(&mut events, Topic::AiEndpointStatus, &view.id).await;
        let status: AiEndpointStatusEvent = serde_json::from_value(event.payload.clone()).unwrap();
        assert_eq!(status.capabilities, both);
    }

    #[tokio::test]
    async fn a_failed_pull_tells_every_session_without_naming_the_url() {
        let service = service().await;
        let mut ollama = input("ollama", "http://127.0.0.1:1", None);
        ollama.kind = EndpointKind::Ollama;
        let added = service.add_endpoint(ollama).await.unwrap();
        let mut events = get_global_pubsub().await.subscribe_events();

        let error = service.pull_model(&added.id, "qwen3").await.unwrap_err();
        assert!(error.to_string().contains("127.0.0.1:1"), "{error}");

        let event = next_event(&mut events, Topic::AiModelStatus, &added.id).await;
        let status: ModelStatusEvent = serde_json::from_value(event.payload.clone()).unwrap();
        assert_eq!(status.status.state, ModelState::Missing);
        let detail = status.status.detail.unwrap();
        assert!(!detail.contains("127.0.0.1"), "{detail}");
    }

    // A burst of checks probes the endpoint once; the others answer its stored health.
    #[tokio::test]
    async fn checks_in_a_burst_probe_the_endpoint_once() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        let listing = server
            .mock("GET", "/v1/models")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"qwen3"}]}"#)
            // The probe of the add, then one probe for the burst.
            .expect(2)
            .create_async()
            .await;
        let view = service
            .add_endpoint(input("vllm", &server.url(), None))
            .await
            .unwrap();
        let endpoint = stored_endpoint(&view.id).unwrap();

        let healths = futures::future::join_all((0..5).map(|_| check_health(&endpoint))).await;

        listing.assert_async().await;
        for health in healths {
            assert_eq!(health.unwrap().state, EndpointState::Reachable);
        }
    }

    #[tokio::test]
    async fn a_zero_context_ceiling_is_refused() {
        let service = service().await;
        let mut zero = input("e", "http://127.0.0.1:1", None);
        zero.max_num_ctx = Some(0);
        let refused = service.add_endpoint(zero).await.unwrap_err();
        assert!(refused.downcast_ref::<InvalidEndpoint>().is_some());
    }

    // The endpoint update reloads the default LLM: a task bound to "default" is on the
    // new worker.
    #[tokio::test]
    async fn a_reloaded_default_model_keeps_the_tasks_bound_to_default() {
        let service = service().await;
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/chat/completions")
            .with_status(200)
            .with_body(r#"{"choices":[{"message":{"role":"assistant","content":"hi"}}]}"#)
            .create_async()
            .await;
        let endpoint_id = service
            .add_endpoint(input("vllm", &server.url(), None))
            .await
            .unwrap()
            .id;
        let id = service
            .add_model(crate::types::ModelInput {
                name: "m".into(),
                model_type: crate::types::ModelType::Llm,
                endpoint_id: endpoint_id.clone(),
                remote_model: "qwen3".into(),
                native_tools: None,
            })
            .await
            .unwrap();
        service
            .set_default_model(crate::types::ModelType::Llm, id.clone())
            .await
            .unwrap();
        let task = service
            .add_task(
                crate::types::AITaskInput {
                    name: "t".into(),
                    model_id: "default".into(),
                    system_prompt: "".into(),
                    prompt_examples: vec![],
                    meta_data: None,
                },
                None,
            )
            .await
            .unwrap();

        service
            .update_endpoint(&endpoint_id, input("renamed", &server.url(), None))
            .await
            .unwrap();

        service
            .prompt(task.task_id, "hello".into(), None)
            .await
            .expect("the task is on the new worker");
    }

    #[tokio::test]
    async fn a_model_that_stops_being_an_llm_stops_its_llm_client() {
        let service = service().await;
        let endpoint_id = service
            .add_endpoint(input("down", "http://127.0.0.1:1", None))
            .await
            .unwrap()
            .id;
        let model = |model_type| crate::types::ModelInput {
            name: "m".into(),
            model_type,
            endpoint_id: endpoint_id.clone(),
            remote_model: "qwen3".into(),
            native_tools: None,
        };
        let id = service
            .add_model(model(crate::types::ModelType::Llm))
            .await
            .unwrap();
        assert!(service.llms.lock().await.contains_key(&id));

        service
            .update_model(id.clone(), model(crate::types::ModelType::Embedding))
            .await
            .unwrap();

        assert!(!service.llms.lock().await.contains_key(&id));
        assert!(service.embedders.lock().await.contains_key(&id));
    }
}
