//! AI WS-native handlers.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::ai_service::providers::http::{is_transport_safe, CLEARTEXT_KEY_REFUSAL};
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::types::{
    AIModelLoadingStatus, AITask, AITaskInput, Model, ModelInput, ModelType, ModelView,
    RequestContext, VoiceActivityParamsInput,
};
use base64::Engine;

use super::guards::{Guard, IdParam, Owner};
use super::types::*;
use super::ws_handler::{HandlerMap, NoParams, ParamExt, WsRpcError};

/// Refuses a request whose user has no compute credits left. A caller without a user
/// (the operator, an app) pays nothing. `agent_did` is the managed agent the request acts as.
fn check_compute_credits_ws(agent_did: Option<&str>) -> Result<(), WsRpcError> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    if let Some(did) = agent_did {
        let free = Ad4mDb::with_global_instance(|db| db.get_agent_free_access(did))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_agent_credits(did))
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

    let models: Vec<ModelView> = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .iter()
        .map(Model::view)
        .collect();
    Ok(serde_json::to_value(models)?)
}

/// `ai.discoverModels` — what does this endpoint serve, and does this key work?
///
/// Takes the credentials of a model that does not exist yet: an operator is
/// filling in the form and wants the list to pick from, before there is
/// anything to list. Answers the provider's own model ids.
///
/// Gated on AI_CREATE rather than AI_READ. It reads nothing of this executor's,
/// but it makes an outbound request to an arbitrary URL with an
/// arbitrary key, which is the same authority adding a model carries and more
/// than reading the models already configured.
///
/// That includes the failure body. `list_models` puts the upstream response
/// verbatim into its error, so a caller can point `baseUrl` at a host this
/// executor can reach and read what it answers. Deliberate, on two grounds: a
/// holder of AI_CREATE can already name an arbitrary URL and send it
/// credentials, so this widens reach and not authority; and the body is the
/// reason the endpoint is worth having, because a status alone does not
/// separate a bad key from a bad model name from a host that is not an LLM.
/// The default user capabilities include AI_CREATE, so the guard also refuses
/// user sessions: the executor sends a request to a URL the caller chooses, a cleaner
/// read primitive than `addModel` plus a prompt, needing no model and no completion.
async fn discover_models(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let base_url = params.require_str("baseUrl")?;
    let base_url = url::Url::parse(&base_url)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid baseUrl: {e}")))?;

    // Defaults to OpenAI, which is what every endpoint that is not Anthropic
    // speaks, and what the field meant before there was a choice.
    let api_type = match params.get("apiType").and_then(|v| v.as_str()) {
        Some(raw) => raw
            .parse::<crate::types::ModelApiType>()
            .map_err(WsRpcError::bad_request)?,
        None => crate::types::ModelApiType::OpenAi,
    };

    let api_key = params
        .get("apiKey")
        .and_then(|v| v.as_str())
        .unwrap_or_default();

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

    let models = crate::ai_service::providers::list_models(&api_type, api_key, base_url)
        .await
        .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    Ok(serde_json::to_value(models)?)
}

/// Refuse a model whose key would cross the network in the clear.
///
/// The same rule as discovery, applied where it matters more: discovery sends
/// a key once, and a saved model sends it with every completion for as long as
/// the model exists. A base URL that does not parse is left for the service to
/// reject with its own error.
///
/// `AIService::build_remote_client` refuses the same model again. This check
/// is the one that keeps it out of the database and answers the caller.
pub(super) fn refuse_cleartext_credential(model: &ModelInput) -> Result<(), WsRpcError> {
    let Some(api) = &model.api else {
        return Ok(());
    };
    // No key sent: an update keeps the stored key only for the URL it was set for.
    if api.api_key.as_ref().is_none_or(|key| key.is_empty()) {
        return Ok(());
    }
    match url::Url::parse(&api.base_url) {
        Ok(url) if !is_transport_safe(&url) => Err(WsRpcError::bad_request(CLEARTEXT_KEY_REFUSAL)),
        _ => Ok(()),
    }
}

async fn add_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
    refuse_cleartext_credential(&model)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let id = service
        .add_model(model)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(id))
}

async fn update_model(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = params.require_str("id")?;
    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
    refuse_cleartext_credential(&model)?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service.update_model(id, model).await.map_err(|e| {
        if let Some(refusal) = e.downcast_ref::<crate::db::ApiKeyNotResent>() {
            WsRpcError::bad_request(refusal.to_string())
        } else {
            WsRpcError::internal(format!("Failed to update model: {:#}", e))
        }
    })?;

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

    let model_id = Ad4mDb::with_global_instance(|db| db.get_default_model(model_type))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let model = if let Some(id) = model_id {
        Ad4mDb::with_global_instance(|db| db.get_model(id))
            .map_err(|e| WsRpcError::internal(e.to_string()))?
    } else {
        None
    };

    Ok(serde_json::to_value(model.as_ref().map(Model::view))?)
}

async fn get_model_loading_status(
    params: Value,
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let model = params
        .opt_str("model")
        .or_else(|| params.opt_str("modelId"))
        .ok_or_else(|| WsRpcError::bad_request("model parameter required"))?;

    let status = AIService::model_status(model)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(status).unwrap_or_default())
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
        Ad4mDb::with_global_instance(|db| db.get_tasks_owned_by(agent.managed_did()))
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
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

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

    let result = service
        .update_task(task)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

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
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

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
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

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
        .open_transcription_stream(body.model_id, body.params.map(|p| p.into()), &ctx)
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
    // The provider's own model ids.
    map.method::<AiDiscoverModelsParams, Vec<String>>("ai.discoverModels", discover_models)
        .read()
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    // Executor-wide settings: every user's prompts go to the models and keys set here.
    // The new model's id.
    map.method::<AiAddModelParams, String>("ai.addModel", add_model)
        .long()
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiUpdateModelParams, bool>("ai.updateModel", update_model)
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiIdParams, bool>("ai.removeModel", remove_model)
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiSetDefaultModelParams, bool>("ai.setDefaultModel", set_default_model)
        .guard(Guard::Executor(AI_CREATE_CAPABILITY.clone()));
    map.method::<AiGetDefaultModelParams, Option<ModelView>>(
        "ai.getDefaultModel",
        get_default_model,
    )
    .read()
    .guard(Guard::Cap(AI_READ_CAPABILITY.clone()));
    map.method::<AiModelLoadingStatusParams, AIModelLoadingStatus>(
        "ai.modelLoadingStatus",
        get_model_loading_status,
    )
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

    let mut errors: Vec<String> = Vec::new();
    for stream_id in &stream_ids {
        if let Err(e) = service
            .feed_transcription_stream(stream_id, audio_f32.clone(), &context)
            .await
        {
            log::warn!("Error feeding stream {}: {}", stream_id, e);
            errors.push(format!("{}: {}", stream_id, e));
        }
    }

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

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiDiscoverModelsParams {
    pub base_url: String,
    /// Parsed leniently (`openai`, `OpenAi`, `OPEN_AI`, ...); defaults to OpenAI.
    #[ts(optional)]
    pub api_type: Option<String>,
    #[ts(optional)]
    pub api_key: Option<String>,
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

/// One of `model` or `modelId` is required; `model` wins when both are set.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiModelLoadingStatusParams {
    #[ts(optional)]
    pub model: Option<String>,
    #[ts(optional)]
    pub model_id: Option<String>,
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
    #[ts(optional)]
    pub params: Option<VoiceActivityParamsInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiTranscriptionCloseParams {
    pub stream_id: String,
}

#[cfg(test)]
mod model_key_tests {
    use super::*;
    use crate::types::{ModelApi, ModelApiInput, ModelApiType};

    const KEY: &str = "sk-provider-secret";

    fn remote_model(api_key: &str) -> Model {
        Model {
            id: "m1".to_string(),
            name: "Remote".to_string(),
            api: Some(ModelApi {
                base_url: url::Url::parse("https://api.example.org/v1").unwrap(),
                api_key: api_key.into(),
                model: "gpt".to_string(),
                api_type: ModelApiType::OpenAi,
                max_num_ctx: None,
            }),
            local: None,
            model_type: ModelType::Llm,
        }
    }

    #[test]
    fn the_view_says_whether_a_key_is_set_and_never_carries_it() {
        let json = serde_json::to_value(remote_model(KEY).view()).unwrap();
        assert!(!json.to_string().contains(KEY), "{json}");
        assert!(json["api"].get("apiKey").is_none(), "{json}");
        assert_eq!(json["api"]["hasApiKey"], true);
        assert_eq!(json["api"]["model"], "gpt");

        let keyless = serde_json::to_value(remote_model("").view()).unwrap();
        assert_eq!(keyless["api"]["hasApiKey"], false);
    }

    // #1167 stripped the key for user sessions by hand and let the operator and apps read
    // it. The view now leaves it out for every caller.
    #[tokio::test]
    async fn no_session_gets_the_default_models_key() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let id = Ad4mDb::with_global_instance(|db| {
            db.add_model(&ModelInput {
                name: "Remote".to_string(),
                api: Some(ModelApiInput {
                    base_url: "https://api.example.org/v1".to_string(),
                    api_key: Some(KEY.into()),
                    model: "gpt".to_string(),
                    api_type: "OPEN_AI".to_string(),
                    max_num_ctx: None,
                }),
                local: None,
                model_type: ModelType::Llm,
            })
        })
        .unwrap();
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
            assert_eq!(model["id"], id.as_str(), "{who}");
            assert!(!model.to_string().contains(KEY), "{who}: {model}");
            assert_eq!(model["api"]["hasApiKey"], true, "{who}");
        }
    }

    /// The stored model's key, read in place for the assertion.
    fn stored_key(id: &str) -> String {
        Ad4mDb::with_global_instance(|db| db.get_model(id.to_string()))
            .unwrap()
            .unwrap()
            .api
            .unwrap()
            .api_key
            .expose()
            .to_string()
    }

    fn input(base_url: &str, api_key: Option<&str>) -> ModelInput {
        ModelInput {
            name: "Remote".to_string(),
            api: Some(ModelApiInput {
                base_url: base_url.to_string(),
                api_key: api_key.map(Into::into),
                model: "gpt".to_string(),
                api_type: "OPEN_AI".to_string(),
                max_num_ctx: None,
            }),
            local: None,
            model_type: ModelType::Llm,
        }
    }

    // A client never reads the key back, so an update that leaves it out keeps it. It
    // goes only to the endpoint it was set for: moving the model needs the key again.
    #[test]
    fn an_update_without_a_key_keeps_the_stored_key_for_the_same_endpoint() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let url = "https://api.example.org/v1";
        let id = Ad4mDb::with_global_instance(|db| db.add_model(&input(url, Some(KEY)))).unwrap();
        let update =
            |model: ModelInput| Ad4mDb::with_global_instance(|db| db.update_model(&id, &model));

        update(input(url, None)).unwrap();
        assert_eq!(stored_key(&id), KEY);

        let moved = update(input("https://elsewhere.example.org/v1", None)).unwrap_err();
        assert!(moved.downcast_ref::<crate::db::ApiKeyNotResent>().is_some());
        assert_eq!(stored_key(&id), KEY, "a refused update changes nothing");

        update(input("https://elsewhere.example.org/v1", Some("sk-new"))).unwrap();
        assert_eq!(stored_key(&id), "sk-new");

        // An empty key clears it, as before; a keyless model then moves freely.
        update(input(url, Some(""))).unwrap();
        assert_eq!(stored_key(&id), "");
        update(input("https://elsewhere.example.org/v1", None)).unwrap();
        assert_eq!(stored_key(&id), "");
    }

    // The refusal reaches the caller as a bad request, through the service's error.
    #[tokio::test]
    async fn moving_a_keyed_model_without_its_key_is_a_bad_request() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AIService::init_global_instance().await.unwrap();
        let url = "https://api.example.org/v1";
        let id = Ad4mDb::with_global_instance(|db| db.add_model(&input(url, Some(KEY)))).unwrap();

        let moved = serde_json::to_value(input("https://elsewhere.example.org/v1", None)).unwrap();
        let err = update_model(
            serde_json::json!({ "id": id, "model": moved }),
            Arc::new(RequestContext::operator_for_test()),
        )
        .await
        .unwrap_err();
        assert_eq!(err.code, 400, "{}", err.message);
        assert!(err.message.contains("send apiKey again"), "{}", err.message);
        assert_eq!(stored_key(&id), KEY);
    }

    #[test]
    fn a_model_added_without_a_key_has_none() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
        let id = Ad4mDb::with_global_instance(|db| {
            db.add_model(&input("https://api.example.org/v1", None))
        })
        .unwrap();
        assert_eq!(stored_key(&id), "");
    }

    #[test]
    fn a_sent_key_never_reaches_a_log_line() {
        let model = input("https://api.example.org/v1", Some(KEY));
        assert!(!format!("{model:?}").contains(KEY));
        assert!(!format!("{:?}", remote_model(KEY)).contains(KEY));
    }
}

#[cfg(test)]
mod compute_credit_tests {
    use super::*;

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
        Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(was_free)).unwrap();

        assert_eq!(results.0.unwrap_err().code, 403, "a user without credits");
        assert!(results.1.is_ok(), "a user with credits");
        assert!(results.2.is_ok(), "a caller without a user pays nothing");
    }
}
