//! AI WS-native handlers.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::ai_service::endpoints::EndpointNotFound;
use crate::ai_service::providers::discovery::discover_for;
use crate::ai_service::providers::http::{
    is_transport_safe, CLEARTEXT_KEY_REFUSAL, DISCOVERY_TIMEOUT,
};
use crate::ai_service::AIService;
use crate::secret::Secret;
use crate::types::{
    AITask, AITaskInput, DetectedEndpoint, DiscoveredModel, EndpointInput, EndpointKind,
    EndpointView, ModelInput, ModelStatus, ModelType, ModelView, RequestContext,
};
use base64::Engine;

use super::guards::{Guard, IdParam, Owner};
use super::types::*;
use super::ws_handler::{HandlerMap, NoParams, ParamExt, WsRpcError};

/// Refuses a request whose user has no compute credits left. A caller without a user
/// (the operator, an app) pays nothing. `agent_did` is the managed agent the request acts as.
fn check_compute_credits_ws(agent_did: Option<&str>) -> Result<(), WsRpcError> {
    if agent_did.is_none() {
        return Ok(());
    }
    let global_free = crate::db_backend::db_backend()
        .get_free_hosting_enabled()
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    if global_free {
        return Ok(());
    }
    if let Some(did) = agent_did {
        let free = crate::db_backend::db_backend()
            .get_agent_free_access(did)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !free {
            let credits = crate::db_backend::db_backend()
                .get_agent_credits(did)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            if credits <= 0.0 {
                return Err(WsRpcError::forbidden("Insufficient compute credits"));
            }
        }
    }
    Ok(())
}

// ── Models ──

async fn list_models(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let _service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let models: Vec<ModelView> =
        AIService::model_views().map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(models)?)
}

/// `ai.discoverModels` — what does this endpoint serve, and does this key work?
///
/// Takes a saved endpoint's id, or the kind, URL and key of a server not saved yet: an
/// operator filling in the form wants the list to pick from before there is anything to
/// save. Answers the models the server lists, with their capabilities.
///
/// Gated on AI_CREATE rather than AI_READ. It reads nothing of this executor's,
/// but it makes an outbound request to an arbitrary URL with an
/// arbitrary key, which is the same authority adding a model carries and more
/// than reading the models already configured.
///
/// That includes the failure body. The listing puts the upstream response
/// into its error (key scrubbed), so a caller can point `baseUrl` at a host this
/// executor can reach and read what it answers. Deliberate, on two grounds: a
/// holder of AI_CREATE can already name an arbitrary URL and send it
/// credentials, so this widens reach and not authority; and the body is the
/// reason the endpoint is worth having, because a status alone does not
/// separate a bad key from a bad model name from a host that is not an LLM.
/// The default user capabilities include AI_CREATE, so the guard also refuses
/// user sessions: the executor sends a request to a URL the caller chooses, a cleaner
/// read primitive than `addModel` plus a prompt, needing no model and no completion.
async fn discover_models(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiDiscoverModelsParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let (kind, base_url, api_key) = match body {
        AiDiscoverModelsParams::Endpoint { endpoint_id } => {
            let endpoint = crate::db_backend::db_backend()
                .get_endpoint(&endpoint_id)
                .map_err(|e| WsRpcError::internal(e.to_string()))?
                .ok_or_else(|| WsRpcError::not_found(EndpointNotFound.to_string()))?;
            (endpoint.kind, endpoint.base_url, endpoint.api_key)
        }
        AiDiscoverModelsParams::Server {
            kind,
            base_url,
            api_key,
        } => (kind, base_url, api_key.unwrap_or_else(|| Secret::new(""))),
    };

    // A key sent over plain HTTP crosses the network in the clear, and the
    // OpenAI path sends it as a bearer token. Refuse that rather than leak it
    // on the operator's behalf.
    //
    // Loopback is exempt, because a local Ollama, vLLM or gateway is reached
    // over http by design and nothing leaves the machine. Keyless discovery
    // against any host stays available, which is the case that made this
    // endpoint worth having.
    if !api_key.is_empty() && !is_transport_safe(&base_url) {
        return Err(WsRpcError::bad_request(CLEARTEXT_KEY_REFUSAL));
    }

    let models: Vec<DiscoveredModel> =
        discover_for(kind, base_url, api_key.expose(), DISCOVERY_TIMEOUT)
            .list_models()
            .await
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
    Ok(serde_json::to_value(models)?)
}

// ── Endpoints ──

/// The answer to a failed endpoint call: the caller's mistake (a refused write, a key not
/// sent again, a key over plain HTTP) is a bad request, a missing endpoint 404.
fn endpoint_error(e: anyhow::Error) -> WsRpcError {
    if e.downcast_ref::<crate::db::InvalidEndpoint>().is_some()
        || e.downcast_ref::<crate::db::ApiKeyNotResent>().is_some()
        || e.downcast_ref::<crate::ai_service::providers::http::UpstreamStatus>()
            .is_some()
    {
        WsRpcError::bad_request(e.to_string())
    } else if e.downcast_ref::<EndpointNotFound>().is_some() {
        WsRpcError::not_found(e.to_string())
    } else {
        WsRpcError::internal(format!("{e:#}"))
    }
}

async fn service() -> Result<AIService, WsRpcError> {
    AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))
}

async fn list_endpoints(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let endpoints = AIService::endpoints().map_err(endpoint_error)?;
    Ok(serde_json::to_value(endpoints)?)
}

async fn add_endpoint(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let input: EndpointInput = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let endpoint = service()
        .await?
        .add_endpoint(input)
        .await
        .map_err(endpoint_error)?;
    Ok(serde_json::to_value(endpoint)?)
}

async fn update_endpoint(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiUpdateEndpointParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let endpoint = service()
        .await?
        .update_endpoint(&body.id, body.endpoint)
        .await
        .map_err(endpoint_error)?;
    Ok(serde_json::to_value(endpoint)?)
}

async fn remove_endpoint(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiRemoveEndpointParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service()
        .await?
        .remove_endpoint(&body.id, body.cascade.unwrap_or(false))
        .await
        .map_err(endpoint_error)?;
    Ok(Value::Bool(true))
}

/// Probes this machine's loopback ports for model servers. Saves nothing.
async fn detect_endpoints(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let detected = crate::ai_service::endpoints::detect::detect_endpoints().await;
    Ok(serde_json::to_value(detected)?)
}

async fn pull_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiUpstreamModelParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service()
        .await?
        .pull_model(&body.endpoint_id, &body.remote_model)
        .await
        .map_err(endpoint_error)?;
    Ok(Value::Bool(true))
}

async fn delete_upstream_model(
    params: Value,
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: AiUpstreamModelParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service()
        .await?
        .delete_upstream_model(&body.endpoint_id, &body.remote_model)
        .await
        .map_err(endpoint_error)?;
    Ok(Value::Bool(true))
}

/// The answer to a refused model or task write: the caller's mistake (an endpoint that does
/// not exist, an empty remote model, an endpoint that cannot serve the type, a task on a
/// model that is not an LLM) is a bad request.
fn model_error(e: anyhow::Error) -> WsRpcError {
    if e.downcast_ref::<crate::db::InvalidModel>().is_some() {
        WsRpcError::bad_request(e.to_string())
    } else {
        WsRpcError::internal(format!("{e:#}"))
    }
}

async fn add_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let id = service.add_model(model).await.map_err(model_error)?;

    Ok(Value::String(id))
}

async fn update_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = params.require_str("id")?;
    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service.update_model(id, model).await.map_err(model_error)?;

    Ok(Value::Bool(true))
}

async fn remove_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = params.require_str("id")?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .remove_model(id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn set_default_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = params.require_str("id")?;
    let body: SetDefaultModelRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .set_default_model(body.model_type, id)
        .await
        .map_err(|e| {
            if e.downcast_ref::<crate::db::InvalidDefaultModel>().is_some() {
                WsRpcError::bad_request(e.to_string())
            } else {
                WsRpcError::internal(e.to_string())
            }
        })?;

    Ok(Value::Bool(true))
}

async fn get_default_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let model_type_str = params.require_str("modelType")?;

    let model_type: ModelType = serde_json::from_str(&format!("\"{}\"", model_type_str))
        .map_err(|e| WsRpcError::bad_request(format!("Invalid modelType: {}", e)))?;

    let model_id = crate::db_backend::db_backend()
        .get_default_model(model_type)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let model = match model_id {
        Some(id) => AIService::model_view(&id).map_err(|e| WsRpcError::internal(e.to_string()))?,
        None => None,
    };

    Ok(serde_json::to_value(model)?)
}

/// Where a model stands by its endpoint's last health check.
async fn get_model_status(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiIdParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let model = AIService::model_view(&body.id)
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .ok_or_else(|| WsRpcError::not_found(format!("No model has the id {}", body.id)))?;
    Ok(serde_json::to_value(model.status)?)
}

// ── Tasks ──

/// The id of the task `ai.prompt` runs: its `taskId` param.
const PROMPT_TASK_ID: IdParam = IdParam::Field(&["taskId"]);

/// The id of the task `ai.removeTask` deletes: its `id` param.
const REMOVED_TASK_ID: IdParam = IdParam::Field(&["id"]);

/// The id of the task `ai.updateTask` rewrites: `task.taskId`, the row the update writes.
const UPDATED_TASK_ID: IdParam = IdParam::Field(&["task", "taskId"]);

/// The operator lists every task; any other session the tasks of the agent it acts as (a
/// grant on the main agent the main agent's, which belong to no user), as the task guard
/// (`Owns { Task }`) lets it reach them.
async fn list_tasks(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let tasks = if ctx.is_operator() {
        AIService::get_tasks().map_err(|e| WsRpcError::internal(e.to_string()))?
    } else {
        let agent = ctx.acts_as();
        crate::db_backend::db_backend()
            .get_tasks_owned_by(agent.managed_did())
            .map_err(|e| WsRpcError::internal(e.to_string()))?
    };
    Ok(serde_json::to_value(tasks)?)
}

/// The new task belongs to the caller's agent, by DID: a user session's to its managed agent,
/// the operator's and main-agent apps' to the main agent (`NULL`).
async fn add_task(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let task: AITaskInput =
        serde_json::from_value(params.get("task").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service
        .add_task(task, ctx.user_did())
        .await
        .map_err(model_error)?;

    Ok(serde_json::to_value(result)?)
}

async fn update_task(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let mut task: AITask =
        serde_json::from_value(params.get("task").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
    // The task the guard checked.
    task.task_id = UPDATED_TASK_ID.require(&params)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service.update_task(task).await.map_err(model_error)?;

    Ok(serde_json::to_value(result)?)
}

async fn remove_task(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = REMOVED_TASK_ID.require(&params)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .delete_task(id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

// ── Prompt & Embed ──

/// The answer to a failed prompt or embed. A managed user does not read endpoints
/// (`ai.endpoints` refuses them, and their model statuses carry no detail), so the URLs of
/// the endpoints leave the message they get: a provider's "could not reach" error names
/// the URL it called.
fn upstream_failure(e: &anyhow::Error, ctx: &RequestContext) -> WsRpcError {
    if e.downcast_ref::<crate::db::InvalidModel>().is_some() {
        // The task's model is not an LLM: no endpoint was called.
        return WsRpcError::bad_request(e.to_string());
    }
    let message = e.to_string();
    if ctx.user_did().is_none() {
        return WsRpcError::internal(message);
    }
    let endpoints = crate::db_backend::db_backend()
        .get_endpoints()
        .unwrap_or_default();
    WsRpcError::internal(without_endpoint_urls(&message, &endpoints))
}

/// `message` with each endpoint's base URL, and then its origin (a provider calls paths
/// under it), replaced by a neutral phrase.
fn without_endpoint_urls(message: &str, endpoints: &[crate::types::Endpoint]) -> String {
    const REPLACEMENT: &str = "<the model's endpoint>";
    let mut message = message.to_string();
    for endpoint in endpoints {
        let base = endpoint.base_url.as_str().trim_end_matches('/').to_string();
        let origin = endpoint.base_url.origin();
        let origin = origin.is_tuple().then(|| origin.ascii_serialization());
        for needle in std::iter::once(base).chain(origin) {
            if !needle.is_empty() {
                message = message.replace(&needle, REPLACEMENT);
            }
        }
    }
    message
}

async fn ai_prompt(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_compute_credits_ws(ctx.user_did())?;

    // The task the guard checked.
    let task_id = PROMPT_TASK_ID.require(&params)?;
    let body: PromptRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service
        .prompt(task_id, body.prompt, ctx.user_did().map(str::to_string))
        .await
        .map_err(|e| upstream_failure(&e, &ctx))?;

    Ok(Value::String(result.text))
}

async fn ai_embed(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_compute_credits_ws(ctx.user_did())?;

    let body: EmbedRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let embedding = service
        .embed(body.model_id, body.text, ctx.user_did().map(str::to_string))
        .await
        .map_err(|e| upstream_failure(&e, &ctx))?;

    let json_string = serde_json::to_string(&embedding.embeddings)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    let compressed_bytes = deflate::deflate_bytes_zlib(json_string.as_bytes());
    Ok(Value::String(
        base64::prelude::BASE64_STANDARD.encode(&compressed_bytes),
    ))
}

// ── Transcription ──

async fn open_transcription_stream(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_compute_credits_ws(ctx.user_did())?;

    let body: AiTranscriptionOpenParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let stream_id = service
        .open_transcription_stream(body.model_id, &ctx)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(stream_id))
}

async fn close_transcription_stream(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: AiTranscriptionCloseParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .close_transcription_stream(&body.stream_id, &ctx)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String("true".to_string()))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // The executor's models (without keys), which users read too.
    map.method::<NoParams, Vec<ModelView>>("ai.models", list_models)
        .read()
        .guard(Guard::Cap(AI_READ_CAPABILITY.clone()));
    // The models a server lists, with their capabilities.
    map.method::<AiDiscoverModelsParams, Vec<DiscoveredModel>>(
        "ai.discoverModels",
        discover_models,
    )
    .read()
    .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    // Executor-wide settings: every user's prompts go to the models and keys set here.
    // The new model's id.
    map.method::<AiAddModelParams, String>("ai.addModel", add_model)
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiUpdateModelParams, bool>("ai.updateModel", update_model)
        .guard(Guard::Executor(AI_UPDATE_CAPABILITY.clone()));
    map.method::<AiIdParams, bool>("ai.removeModel", remove_model)
        .guard(Guard::Executor(AI_DELETE_CAPABILITY.clone()));
    map.method::<AiSetDefaultModelParams, bool>("ai.setDefaultModel", set_default_model)
        .guard(Guard::Executor(AI_UPDATE_CAPABILITY.clone()));
    // The upstream servers, with their URLs and health: the operator's view (users read
    // `ai.models`). Keys never leave: `hasApiKey` says whether one is set.
    map.method::<NoParams, Vec<EndpointView>>("ai.endpoints", list_endpoints)
        .read()
        .guard(Guard::Executor(AI_READ_CAPABILITY.clone()));
    // Probed once before it saves.
    map.method::<EndpointInput, EndpointView>("ai.addEndpoint", add_endpoint)
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    // Long: an update reloads the models on the endpoint, and a cascade removes them, each
    // after the inference it runs.
    map.method::<AiUpdateEndpointParams, EndpointView>("ai.updateEndpoint", update_endpoint)
        .long()
        .guard(Guard::Executor(AI_UPDATE_CAPABILITY.clone()));
    map.method::<AiRemoveEndpointParams, bool>("ai.removeEndpoint", remove_endpoint)
        .long()
        .guard(Guard::Executor(AI_DELETE_CAPABILITY.clone()));
    // Requests to this machine's loopback ports; saves nothing.
    map.method::<NoParams, Vec<DetectedEndpoint>>("ai.detectEndpoints", detect_endpoints)
        .read()
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    // Ollama only. Progress goes out as `model-status`.
    map.method::<AiUpstreamModelParams, bool>("ai.pullModel", pull_model)
        .long()
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    // An Ollama delete then a health probe can outlast the default RPC timeout.
    map.method::<AiUpstreamModelParams, bool>("ai.deleteUpstreamModel", delete_upstream_model)
        .long()
        .guard(Guard::Executor(AI_DELETE_CAPABILITY.clone()));
    map.method::<AiGetDefaultModelParams, Option<ModelView>>(
        "ai.getDefaultModel",
        get_default_model,
    )
    .read()
    .guard(Guard::Cap(AI_READ_CAPABILITY.clone()));
    map.method::<AiIdParams, ModelStatus>("ai.modelStatus", get_model_status)
        .read()
        .guard(Guard::Cap(AI_READ_CAPABILITY.clone()));
    // Tasks belong to the agent whose session created them (`tasks.agent_did`, `NULL` for the
    // main agent); the operator reaches every task, any other session its agent's. The list
    // and the new task follow the caller's agent.
    map.method::<NoParams, Vec<AITask>>("ai.tasks", list_tasks)
        .read()
        .guard(Guard::Cap(AI_READ_CAPABILITY.clone()));
    map.method::<AiAddTaskParams, AITask>("ai.addTask", add_task)
        .guard(Guard::Cap(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiUpdateTaskParams, AITask>("ai.updateTask", update_task)
        .guard(Guard::Owns {
            cap: |_| AI_CREATE_CAPABILITY.clone(),
            id: UPDATED_TASK_ID,
            owner: Owner::Task,
        });
    map.method::<AiIdParams, bool>("ai.removeTask", remove_task)
        .guard(Guard::Owns {
            cap: |_| AI_CREATE_CAPABILITY.clone(),
            id: REMOVED_TASK_ID,
            owner: Owner::Task,
        });
    // Prompts, embeddings and transcription bill the caller's user; the handlers check its
    // compute credits after the guard.
    // The completion text.
    map.method::<PromptRequest, String>("ai.prompt", ai_prompt)
        .long()
        .guard(Guard::Owns {
            cap: |_| AI_PROMPT_CAPABILITY.clone(),
            id: PROMPT_TASK_ID,
            owner: Owner::Task,
        });
    // The embedding vector as JSON, zlib-deflated, base64-encoded.
    map.method::<EmbedRequest, String>("ai.embed", ai_embed)
        .long()
        .guard(Guard::Cap(AI_PROMPT_CAPABILITY.clone()));
    // The new stream's id.
    map.method::<AiTranscriptionOpenParams, String>(
        "ai.transcriptionOpen",
        open_transcription_stream,
    )
    .guard(Guard::Cap(AI_TRANSCRIBE_CAPABILITY.clone()));
    // Always the string `"true"`.
    map.method::<AiTranscriptionCloseParams, String>(
        "ai.transcriptionClose",
        close_transcription_stream,
    )
    .guard(Guard::Cap(AI_TRANSCRIBE_CAPABILITY.clone()));
}

// ── HTTP-only: binary transcription feed ────────────────────────────────────
// This endpoint receives raw PCM Float32 LE bytes via HTTP POST.
// It cannot go through WS JSON, so it stays as a regular Axum handler.

use super::auth::AppState;
use super::errors::ApiError;
use axum::extract::State;
use axum::response::Json;

/// POST /ai/transcription/feed
///
/// Accepts raw PCM Float32 little-endian bytes (application/octet-stream).
/// Stream IDs are passed via `X-Stream-Ids` header (comma-separated).
pub async fn feed_transcription_stream(
    State(_state): State<AppState>,
    context: RequestContext,
    headers: axum::http::HeaderMap,
    body: axum::body::Bytes,
) -> Result<Json<String>, ApiError> {
    context
        .require(&AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits_ws(context.user_did()).map_err(|e| ApiError::Forbidden(e.message))?;

    let stream_ids_header = headers
        .get("x-stream-ids")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let stream_ids: Vec<String> = stream_ids_header
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if stream_ids.is_empty() {
        return Err(ApiError::BadRequest(
            "X-Stream-Ids header is required".into(),
        ));
    }

    if stream_ids.len() > 32 {
        return Err(ApiError::BadRequest("Too many stream IDs (max 32)".into()));
    }

    // Limit audio buffer to 10 MB (2.5M float32 samples)
    const MAX_AUDIO_BYTES: usize = 10 * 1024 * 1024;
    if body.len() > MAX_AUDIO_BYTES {
        return Err(ApiError::BadRequest(format!(
            "Audio buffer too large ({} bytes, max {})",
            body.len(),
            MAX_AUDIO_BYTES
        )));
    }

    if body.len() % 4 != 0 {
        return Err(ApiError::BadRequest(
            "Body length must be a multiple of 4 (Float32 samples)".into(),
        ));
    }
    let audio_f32: Vec<f32> = body
        .chunks_exact(4)
        .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
        .collect();

    let service = AIService::global_instance()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    // Concurrently: a stream on an endpoint answers once its utterance is transcribed.
    let outcomes = futures::future::join_all(stream_ids.iter().map(|stream_id| {
        service.feed_transcription_stream(stream_id, audio_f32.clone(), &context)
    }))
    .await;
    let errors: Vec<String> = stream_ids
        .iter()
        .zip(outcomes)
        .filter_map(|(stream_id, outcome)| {
            let e = outcome.err()?;
            log::warn!("Error feeding stream {}: {}", stream_id, e);
            Some(format!("{}: {}", stream_id, e))
        })
        .collect();

    feed_outcome(&errors, stream_ids.len())?;
    Ok(Json("true".to_string()))
}

/// Whether a feed to `stream_count` streams succeeded, given the streams that failed.
///
/// Any failure fails the request. When only some streams failed, the audio has already reached
/// the others, so the message says so and names the failures: a caller retrying the whole feed
/// would give the streams that took it the same audio twice.
pub(crate) fn feed_outcome(errors: &[String], stream_count: usize) -> Result<(), ApiError> {
    if errors.is_empty() {
        return Ok(());
    }
    if errors.len() == stream_count {
        return Err(ApiError::Internal(format!(
            "All streams failed: {}",
            errors.join("; ")
        )));
    }
    Err(ApiError::Internal(format!(
        "{} of {} streams failed; the others were fed: {}",
        errors.len(),
        stream_count,
        errors.join("; ")
    )))
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiIdParams {
    pub id: String,
}

/// A saved endpoint by its id, or a server not saved yet by its kind, URL and key.
#[derive(Deserialize, TS)]
#[serde(untagged, deny_unknown_fields)]
#[ts(export)]
pub enum AiDiscoverModelsParams {
    #[serde(rename_all = "camelCase")]
    Endpoint { endpoint_id: String },
    #[serde(rename_all = "camelCase")]
    Server {
        kind: EndpointKind,
        base_url: url::Url,
        #[ts(optional, as = "Option<String>")]
        api_key: Option<Secret>,
    },
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiUpdateEndpointParams {
    pub id: String,
    pub endpoint: EndpointInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiRemoveEndpointParams {
    pub id: String,
    /// Remove the models that run on the endpoint too. Without it, a removal is refused
    /// while any does.
    #[ts(optional)]
    pub cascade: Option<bool>,
}

/// A model on an Ollama endpoint, by the id Ollama knows it by.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiUpstreamModelParams {
    pub endpoint_id: String,
    pub remote_model: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiAddModelParams {
    pub model: ModelInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiUpdateModelParams {
    pub id: String,
    pub model: ModelInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiSetDefaultModelParams {
    pub id: String,
    pub model_type: ModelType,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiGetDefaultModelParams {
    pub model_type: ModelType,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiAddTaskParams {
    pub task: AITaskInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiUpdateTaskParams {
    pub task: AITask,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiTranscriptionOpenParams {
    pub model_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiTranscriptionCloseParams {
    pub stream_id: String,
}

#[cfg(test)]
mod model_view_tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::types::{Endpoint, EndpointHealth, EndpointState};

    const KEY: &str = "sk-provider-secret";

    /// A saved endpoint holding [`KEY`] that has listed `qwen3:8b`, and an LLM on it.
    fn model_on_a_keyed_endpoint() -> (Endpoint, String) {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let mut endpoint = Endpoint::for_test(
            "openai",
            EndpointKind::OpenAi,
            "https://api.example.org",
            KEY,
        );
        endpoint.health = EndpointHealth {
            state: EndpointState::Reachable,
            checked_at: Some(1),
            detail: None,
            served_models: vec!["qwen3:8b".into()],
        };
        let id = Ad4mDb::with_global_instance(|db| {
            db.insert_endpoint(&endpoint)?;
            db.add_model(&ModelInput {
                name: "Remote".to_string(),
                model_type: ModelType::Llm,
                endpoint_id: endpoint.id.clone(),
                remote_model: "qwen3:8b".to_string(),
                native_tools: Some(true),
            })
        })
        .unwrap();
        (endpoint, id)
    }

    // A model names its endpoint and never carries the endpoint's key, for any caller.
    #[tokio::test]
    async fn every_session_reads_the_model_with_its_endpoint_and_status_and_no_key() {
        let (endpoint, id) = model_on_a_keyed_endpoint();
        Ad4mDb::with_global_instance(|db| db.set_default_model(ModelType::Llm, &id)).unwrap();
        let params = serde_json::json!({ "modelType": "LLM" });
        let read = vec![AI_READ_CAPABILITY.clone()];

        for (who, ctx) in [
            ("operator", RequestContext::operator_for_test()),
            ("app", RequestContext::app_for_test(read.clone())),
            (
                "user",
                RequestContext::user_for_test("did:key:ai-user", read.clone()),
            ),
        ] {
            let model = get_default_model(params.clone(), Arc::new(ctx))
                .await
                .unwrap();
            assert_eq!(
                model,
                serde_json::json!({
                    "id": id,
                    "name": "Remote",
                    "modelType": "LLM",
                    "endpointId": endpoint.id,
                    "remoteModel": "qwen3:8b",
                    "nativeTools": true,
                    "status": { "state": "READY" },
                }),
                "{who}"
            );
            assert!(!model.to_string().contains(KEY), "{who}: {model}");
        }
    }

    #[tokio::test]
    async fn a_models_status_is_its_endpoints_last_check_and_an_unknown_model_is_not_found() {
        let (_, id) = model_on_a_keyed_endpoint();
        let ctx = Arc::new(RequestContext::operator_for_test());
        let status = get_model_status(serde_json::json!({ "id": id }), ctx.clone())
            .await
            .unwrap();
        assert_eq!(status, serde_json::json!({ "state": "READY" }));
        let missing = get_model_status(serde_json::json!({ "id": "nope" }), ctx)
            .await
            .unwrap_err();
        assert_eq!(missing.code, 404);
    }

    #[tokio::test]
    async fn a_model_on_an_endpoint_that_does_not_exist_is_a_bad_request() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AIService::init_global_instance().await.unwrap();
        let err = add_model(
            serde_json::json!({ "model": {
                "name": "m", "modelType": "LLM", "endpointId": "nope", "remoteModel": "r"
            } }),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap_err();
        assert_eq!(err.code, 400, "{}", err.message);
    }

    // A task on a model that is not an LLM is the caller's mistake, as the other refusals of
    // a model's type are: 400, not "No LLM with id … is loaded" as a 500.
    #[tokio::test]
    async fn a_task_on_a_model_that_is_not_an_llm_is_a_bad_request() {
        let (endpoint, _) = model_on_a_keyed_endpoint();
        AIService::init_global_instance().await.unwrap();
        let (embedding, task_id) = Ad4mDb::with_global_instance(|db| {
            let embedding = db.add_model(&ModelInput {
                name: "Embed".to_string(),
                model_type: ModelType::Embedding,
                endpoint_id: endpoint.id.clone(),
                remote_model: "bge-m3".to_string(),
                native_tools: None,
            })?;
            // A task stored before its model became an embedding model.
            let task_id = db.add_task(
                "t".into(),
                embedding.clone(),
                String::new(),
                vec![],
                None,
                None,
            )?;
            Ok::<_, anyhow::Error>((embedding, task_id))
        })
        .unwrap();
        AIService::global_instance()
            .await
            .unwrap()
            .load()
            .await
            .unwrap();
        let ctx = Arc::new(RequestContext::operator_for_test());

        let task = serde_json::json!({ "task": {
            "name": "t", "modelId": embedding, "systemPrompt": "", "promptExamples": []
        } });
        let added = add_task(task, ctx.clone()).await.unwrap_err();
        assert_eq!(added.code, 400, "ai.addTask: {}", added.message);

        let prompt = serde_json::json!({ "taskId": task_id, "prompt": "hi" });
        let prompted = ai_prompt(prompt, ctx).await.unwrap_err();
        assert_eq!(prompted.code, 400, "ai.prompt: {}", prompted.message);
    }

    // An embed on a model this executor does not have (Flux sends 'bert') is the caller's
    // mistake: 400, not 500.
    #[tokio::test]
    async fn an_embed_on_a_model_that_does_not_exist_is_a_bad_request() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AIService::init_global_instance().await.unwrap();
        let err = ai_embed(
            serde_json::json!({ "modelId": "bert", "text": "hi" }),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap_err();
        assert_eq!(err.code, 400, "{}", err.message);
    }
}

#[cfg(test)]
mod compute_credit_tests {
    use super::*;
    use crate::db::Ad4mDb;

    // #1175: the check used to decode the caller's token again to find the user. It now
    // takes the user the request resolved, so it needs no token.
    #[test]
    fn the_check_reads_the_credits_of_the_requests_user() {
        if Ad4mDb::global_instance().lock().unwrap().is_none() {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        }
        let id = uuid::Uuid::new_v4();
        let broke = format!("broke-{id}@example.org");
        let funded = format!("funded-{id}@example.org");
        let broke_did = format!("did:key:broke-{id}");
        let funded_did = format!("did:key:funded-{id}");
        let was_free = Ad4mDb::with_global_instance(|db| {
            db.add_user(&broke, &broke_did, "pw")?;
            db.add_user(&funded, &funded_did, "pw")?;
            db.set_user_credits(&funded, 5.0)?;
            let was_free = db.get_free_hosting_enabled()?;
            db.set_free_hosting_enabled(false)?;
            Ok::<_, anyhow::Error>(was_free)
        })
        .unwrap();

        let results = (
            check_compute_credits_ws(Some(&broke_did)),
            check_compute_credits_ws(Some(&funded_did)),
            check_compute_credits_ws(None),
        );
        crate::db_backend::db_backend()
            .set_free_hosting_enabled(was_free)
            .unwrap();

        assert_eq!(results.0.unwrap_err().code, 403, "a user without credits");
        assert!(results.1.is_ok(), "a user with credits");
        assert!(results.2.is_ok(), "a caller without a user pays nothing");
    }

    // The HTTP feed refuses a user at 0 credits before any stream is fed.
    #[tokio::test]
    async fn the_http_feed_refuses_a_user_without_credits() {
        use crate::agent::session::{Principal, Session};
        use crate::agent::AgentContext;
        if Ad4mDb::global_instance().lock().unwrap().is_none() {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        }
        let id = uuid::Uuid::new_v4();
        let broke_did = format!("did:key:broke-feeder-{id}");
        let was_free = Ad4mDb::with_global_instance(|db| {
            db.add_user(&format!("broke-feeder-{id}@example.org"), &broke_did, "pw")?;
            let was_free = db.get_free_hosting_enabled()?;
            db.set_free_hosting_enabled(false)?;
            Ok::<_, anyhow::Error>(was_free)
        })
        .unwrap();
        let context = RequestContext::for_session(
            Session::for_test(Principal::Agent(AgentContext::managed(broke_did)))
                .with_caps(vec![AI_TRANSCRIBE_CAPABILITY.clone()]),
        );
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("x-stream-ids", "a-stream".parse().unwrap());

        let fed = feed_transcription_stream(
            State(AppState {
                admin_credential: None,
            }),
            context,
            headers,
            axum::body::Bytes::from(vec![0u8; 8]),
        )
        .await;
        crate::db_backend::db_backend()
            .set_free_hosting_enabled(was_free)
            .unwrap();

        match fed {
            Err(ApiError::Forbidden(message)) => assert!(message.contains("credits"), "{message}"),
            other => panic!("a user without credits fed a stream: {other:?}"),
        }
    }

    // On dev a closed database panicked the check, so the request failed. The read error
    // must not count as free hosting: the request would run without a credit check.
    #[test]
    fn a_closed_database_is_not_free_hosting() {
        let _closed = crate::test_utils::DbClosed::now();
        assert!(check_compute_credits_ws(Some("did:key:z6MkClosedDbUser")).is_err());
        assert!(
            check_compute_credits_ws(None).is_ok(),
            "a caller without a user pays nothing and reads no setting"
        );
    }
}

#[cfg(test)]
mod endpoint_guard_tests {
    use super::*;
    use crate::api::ws_handler::build_handler_map;
    use serde_json::json;

    fn params(method: &str) -> Value {
        let endpoint =
            json!({ "name": "e", "kind": "OLLAMA", "baseUrl": "http://127.0.0.1:11434" });
        match method {
            "ai.addEndpoint" => endpoint,
            "ai.updateEndpoint" => json!({ "id": "e1", "endpoint": endpoint }),
            "ai.updateModel" => json!({ "id": "m1", "model": {
                "name": "m", "modelType": "LLM", "endpointId": "e1", "remoteModel": "r"
            } }),
            "ai.setDefaultModel" => json!({ "id": "m1", "modelType": "LLM" }),
            "ai.pullModel" | "ai.deleteUpstreamModel" => {
                json!({ "endpointId": "e1", "remoteModel": "m" })
            }
            "ai.endpoints" | "ai.detectEndpoints" => json!({}),
            _ => json!({ "id": "e1" }),
        }
    }

    /// Who passes each endpoint and model-admin method: an app on the main agent holding
    /// what a user token holds by default (AI READ, CREATE, PROMPT, TRANSCRIBE: no UPDATE
    /// or DELETE), and a user session holding every AI capability.
    #[tokio::test]
    async fn rewrites_and_removals_need_update_and_delete_and_users_are_refused_all() {
        let map = build_handler_map();
        let app = RequestContext::app_for_test(get_user_default_capabilities());
        let user =
            RequestContext::user_for_test("did:key:ai-admin-user", vec![AI_ALL_CAPABILITY.clone()]);

        let app_passes = [
            ("ai.endpoints", true),
            ("ai.addEndpoint", true),
            ("ai.detectEndpoints", true),
            ("ai.pullModel", true),
            ("ai.updateEndpoint", false),
            ("ai.removeEndpoint", false),
            ("ai.deleteUpstreamModel", false),
            ("ai.updateModel", false),
            ("ai.removeModel", false),
            ("ai.setDefaultModel", false),
        ];
        for (method, passes) in app_passes {
            let app_answer = map.admit(method, &params(method), &app).await;
            assert_eq!(
                app_answer.is_ok(),
                passes,
                "{method} as an app: {app_answer:?}"
            );
            if let Err(refused) = app_answer {
                assert_eq!(refused.code, 403, "{method}");
            }
            let user_answer = map.admit(method, &params(method), &user).await;
            assert_eq!(
                user_answer.map_err(|e| e.code),
                Err(403),
                "{method} as a user: endpoints are the operator's"
            );
        }
    }
}
