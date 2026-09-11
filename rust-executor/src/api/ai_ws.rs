//! AI WS-native handlers.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::types::{AITask, AITaskInput, ModelInput, ModelType, RequestContext};
use base64::Engine;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

fn check_compute_credits_ws(auth_token: &str) -> Result<(), WsRpcError> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            if credits <= 0.0 {
                return Err(WsRpcError::forbidden("Insufficient compute credits"));
            }
        }
    }
    Ok(())
}

// ── Models ──

async fn list_models(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let _service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(models)?)
}

/// `ai.discoverModels` — what does this endpoint serve, and does this key work?
///
/// Takes the credentials of a model that does not exist yet: an operator is
/// filling in the form and wants the list to pick from, before there is
/// anything to list. Answers the provider's own model ids.
///
/// Gated on AI_CREATE rather than AI_READ. It reads nothing of this node's,
/// but it makes an outbound request to an arbitrary URL with an
/// arbitrary key, which is the same authority adding a model carries and more
/// than reading the models already configured.
///
/// That includes the failure body. `list_models` puts the upstream response
/// verbatim into its error, so a caller can point `baseUrl` at a host this
/// node can reach and read what it answers. Deliberate, on two grounds: a
/// holder of AI_CREATE can already name an arbitrary URL and send it
/// credentials, so this widens reach and not authority; and the body is the
/// reason the endpoint is worth having, because a status alone does not
/// separate a bad key from a bad model name from a host that is not an LLM.
/// Revisit it if AI_CREATE is ever granted more widely than to the operator of
/// the node — the reach is a cleaner read primitive than `addModel` plus a
/// prompt, needing no model and no completion.
async fn discover_models(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

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
        return Err(WsRpcError::bad_request(
            "Refusing to send an API key over plain HTTP. Use https, or omit the key.",
        ));
    }

    let models = crate::ai_service::providers::list_models(&api_type, api_key, base_url)
        .await
        .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    Ok(serde_json::to_value(models)?)
}

/// Whether a credential may be sent to this URL.
///
/// True for https anywhere, and for http on loopback only. A hostname that
/// merely looks local is not enough: `localhost.example.com` resolves
/// wherever its owner points it.
pub(super) fn is_transport_safe(url: &url::Url) -> bool {
    if url.scheme() == "https" {
        return true;
    }

    match url.host() {
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        Some(url::Host::Domain(host)) => host == "localhost" || host.ends_with(".localhost"),
        None => false,
    }
}

async fn add_model(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let id = service
        .add_model(model)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(id))
}

async fn update_model(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let id = params.require_str("id")?;
    let model: ModelInput =
        serde_json::from_value(params.get("model").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .update_model(id, model)
        .await
        .map_err(|e| WsRpcError::internal(format!("Failed to update model: {}", e)))?;

    Ok(Value::Bool(true))
}

async fn remove_model(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

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

async fn set_default_model(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let id = params.require_str("id")?;
    let body: SetDefaultModelRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .set_default_model(body.model_type, id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn get_default_model(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

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

    Ok(serde_json::to_value(model)?)
}

async fn get_model_loading_status(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

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

async fn list_tasks(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let tasks = AIService::get_tasks().map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(tasks)?)
}

async fn add_task(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let task: AITaskInput =
        serde_json::from_value(params.get("task").cloned().unwrap_or(Value::Null))
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service
        .add_task(task)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(result)?)
}

async fn update_task(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let task: AITask = serde_json::from_value(params.get("task").cloned().unwrap_or(Value::Null))
        .map_err(|e| WsRpcError::bad_request(e.to_string()))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service
        .update_task(task)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(result)?)
}

async fn remove_task(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let id = params.require_str("id")?;

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
    check_capability(&ctx.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    check_compute_credits_ws(&ctx.auth_token)?;

    let body: PromptRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let result = service
        .prompt(body.task_id, body.prompt, Some(ctx.auth_token.clone()))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(result.text))
}

async fn ai_embed(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_PROMPT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    check_compute_credits_ws(&ctx.auth_token)?;

    let body: EmbedRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let embedding = service
        .embed(body.model_id, body.text, Some(ctx.auth_token.clone()))
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
    check_capability(&ctx.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    check_compute_credits_ws(&ctx.auth_token)?;

    let body: OpenTranscriptionRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let stream_id = service
        .open_transcription_stream(
            body.model_id,
            body.params.map(|p| p.into()),
            ctx.auth_token.clone(),
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(stream_id))
}

async fn close_transcription_stream(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CloseTranscriptionRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let service = AIService::global_instance()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    service
        .close_transcription_stream(&body.stream_id, &ctx.auth_token)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String("true".to_string()))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("ai.models", list_models);
    map.register("ai.discoverModels", discover_models);
    map.register("ai.addModel", add_model);
    map.register("ai.updateModel", update_model);
    map.register("ai.removeModel", remove_model);
    map.register("ai.setDefaultModel", set_default_model);
    map.register("ai.getDefaultModel", get_default_model);
    map.register("ai.modelLoadingStatus", get_model_loading_status);
    map.register("ai.tasks", list_tasks);
    map.register("ai.addTask", add_task);
    map.register("ai.updateTask", update_task);
    map.register("ai.removeTask", remove_task);
    map.register("ai.prompt", ai_prompt);
    map.register("ai.embed", ai_embed);
    map.register("ai.transcriptionOpen", open_transcription_stream);
    map.register("ai.transcriptionClose", close_transcription_stream);
}

// ── HTTP-only: binary transcription feed ────────────────────────────────────
// This endpoint receives raw PCM Float32 LE bytes via HTTP POST.
// It cannot go through WS JSON, so it stays as a regular Axum handler.

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use axum::extract::State;
use axum::response::Json;

/// POST /ai/transcription/feed
///
/// Accepts raw PCM Float32 little-endian bytes (application/octet-stream).
/// Stream IDs are passed via `X-Stream-Ids` header (comma-separated).
pub async fn feed_transcription_stream(
    State(_state): State<AppState>,
    auth: AuthContext,
    headers: axum::http::HeaderMap,
    body: axum::body::Bytes,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits_ws(&context.auth_token).map_err(|e| ApiError::Forbidden(e.message))?;

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
            .feed_transcription_stream(stream_id, audio_f32.clone(), &context.auth_token)
            .await
        {
            log::warn!("Error feeding stream {}: {}", stream_id, e);
            errors.push(format!("{}: {}", stream_id, e));
        }
    }

    if errors.len() == stream_ids.len() {
        return Err(ApiError::Internal(format!(
            "All streams failed: {}",
            errors.join("; ")
        )));
    }

    Ok(Json("true".to_string()))
}
