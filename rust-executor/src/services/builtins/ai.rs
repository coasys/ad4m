//! AI: `ai.inference` (prompts, embeddings, transcription, tasks) and
//! `ai.models` (model registry and defaults).

use async_trait::async_trait;
use base64::Engine;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::OnceCell;

use super::{internal, params, to_value, token, NoParams};
use crate::ai_service::providers::http::{is_transport_safe, CLEARTEXT_KEY_REFUSAL};
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::services::builtin::{
    CallContext, EventEmitter, EventOwner, ServiceError, ServiceHealth, ServiceImplementation,
    StartContext,
};
use crate::services::interface::{Risk, Selection};
use crate::services::schema_export::{InterfaceBuilder, MethodOptions};
use crate::types::{
    AIModelLoadingStatus, AITask, AITaskInput, Model, ModelInput, ModelType,
    VoiceActivityParamsInput,
};

// ── ai.inference contract ───────────────────────────────────────────────────

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PromptParams {
    /// The task whose model, system prompt and examples frame the prompt.
    pub task_id: String,
    pub prompt: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct EmbedParams {
    pub model_id: String,
    pub text: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct TranscriptionOpenParams {
    pub model_id: String,
    #[serde(default)]
    pub params: Option<VoiceActivityParamsInput>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct StreamIdParams {
    pub stream_id: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct AddTaskParams {
    pub task: AITaskInput,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct UpdateTaskParams {
    pub task: AITask,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct IdParams {
    pub id: String,
}

/// `transcription-text`: one recognised utterance of an open stream.
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TranscriptionText {
    pub stream_id: String,
    pub text: String,
}

pub fn inference_interface() -> Value {
    InterfaceBuilder::new(
        "ai.inference",
        super::AUTHOR,
        "1.0.0",
        Selection::PerUser,
        "Prompts, embeddings and speech transcription on the executor's AI models.",
    )
    .action("PROMPT", "Use AI models", "Send text to an AI model and read the answer.", Risk::Safe)
    .action("TRANSCRIBE", "Transcribe audio", "Turn spoken audio into text.", Risk::Safe)
    .action("TASKS", "Manage AI tasks", "Create, change and delete the tasks prompts run under.", Risk::Write)
    .method::<PromptParams, String>(
        "prompt",
        "PROMPT",
        "Run `prompt` under a task; answers the completion text.",
        MethodOptions { long: true, meter: Some("ai.prompt"), ..Default::default() },
    )
    .method::<EmbedParams, String>(
        "embed",
        "PROMPT",
        "Embed `text` with an embedding model. Answers the vector as JSON, zlib-deflated, base64-encoded.",
        MethodOptions { long: true, meter: Some("ai.embed"), ..Default::default() },
    )
    .method::<TranscriptionOpenParams, String>(
        "transcriptionOpen",
        "TRANSCRIBE",
        "Open a transcription stream; answers its id. Audio goes to `POST /api/v1/ai/transcription/feed`; text arrives as `transcription-text`.",
        MethodOptions { meter: Some("ai.transcription"), ..Default::default() },
    )
    .method::<StreamIdParams, bool>(
        "transcriptionClose",
        "TRANSCRIBE",
        "Close a transcription stream.",
        MethodOptions::default(),
    )
    .method::<NoParams, Vec<AITask>>("tasks", "PROMPT", "Every AI task.", MethodOptions { read: true, ..Default::default() })
    .method::<AddTaskParams, AITask>("addTask", "TASKS", "Create a task.", MethodOptions::default())
    .method::<UpdateTaskParams, AITask>("updateTask", "TASKS", "Change a task.", MethodOptions::default())
    .method::<IdParams, bool>("removeTask", "TASKS", "Delete a task.", MethodOptions::default())
    .event::<TranscriptionText>(
        "transcription-text",
        "TRANSCRIBE",
        "Text recognised in an open transcription stream.",
        Some("streamId"),
    )
    .build()
}

// ── ai.models contract ──────────────────────────────────────────────────────

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct DiscoverModelsParams {
    pub base_url: String,
    #[serde(default)]
    pub api_key: Option<String>,
    /// `OPEN_AI` (default), `ANTHROPIC` or `OLLAMA`; parsed leniently.
    #[serde(default)]
    pub api_type: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct AddModelParams {
    pub model: ModelInput,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct UpdateModelParams {
    pub id: String,
    pub model: ModelInput,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SetDefaultModelParams {
    pub id: String,
    pub model_type: ModelType,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ModelTypeParams {
    pub model_type: ModelType,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ModelParams {
    /// The model id.
    pub model: String,
}

pub fn models_interface() -> Value {
    InterfaceBuilder::new(
        "ai.models",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "The executor's AI models: which exist, which are the defaults, and their loading state.",
    )
    .action("READ", "See AI models", "List the AI models and their loading state.", Risk::Safe)
    .action("MANAGE", "Manage AI models", "Add, change and remove AI models and pick the defaults.", Risk::Admin)
    .method::<NoParams, Vec<Model>>("models", "READ", "Every model.", MethodOptions { read: true, ..Default::default() })
    .method::<DiscoverModelsParams, Vec<String>>(
        "discoverModels",
        "MANAGE",
        "Ask a remote endpoint which models it serves, with credentials of a model not added yet. The upstream error body comes back on failure.",
        MethodOptions {
            read: true,
            errors: vec![("InvalidEndpoint", 422), ("CleartextCredential", 422), ("EndpointRefused", 424)],
            ..Default::default()
        },
    )
    .method::<AddModelParams, String>(
        "addModel",
        "MANAGE",
        "Add a model; answers its id.",
        MethodOptions { long: true, errors: vec![("CleartextCredential", 422)], ..Default::default() },
    )
    .method::<UpdateModelParams, bool>(
        "updateModel",
        "MANAGE",
        "Change a model.",
        MethodOptions { errors: vec![("CleartextCredential", 422)], ..Default::default() },
    )
    .method::<IdParams, bool>("removeModel", "MANAGE", "Remove a model.", MethodOptions::default())
    .method::<SetDefaultModelParams, bool>("setDefaultModel", "MANAGE", "Make a model the default of its type. The model must exist and have that type.", MethodOptions { errors: vec![("InvalidDefaultModel", 422)], ..Default::default() })
    .method::<ModelTypeParams, Option<Model>>("getDefaultModel", "READ", "The default model of a type.", MethodOptions { read: true, ..Default::default() })
    .method::<ModelParams, AIModelLoadingStatus>("modelLoadingStatus", "READ", "Download and load progress of a model.", MethodOptions { read: true, ..Default::default() })
    .event::<AIModelLoadingStatus>("model-loading-status", "READ", "A model's download or load progress changed.", None)
    .build()
}

// ── Implementation ──────────────────────────────────────────────────────────

/// Refuse a model whose API key would cross the network in the clear.
/// Loopback stays allowed: a local Ollama or gateway is reached over http by
/// design. `AIService::build_remote_client` refuses the same model again;
/// this check keeps it out of the database and answers the caller.
pub(crate) fn refuse_cleartext_credential(model: &ModelInput) -> Result<(), ServiceError> {
    let Some(api) = &model.api else {
        return Ok(());
    };
    if api.api_key.is_empty() {
        return Ok(());
    }
    match url::Url::parse(&api.base_url) {
        Ok(url) if !is_transport_safe(&url) => Err(ServiceError::method(
            "CleartextCredential",
            CLEARTEXT_KEY_REFUSAL,
        )),
        _ => Ok(()),
    }
}

async fn service() -> Result<AIService, ServiceError> {
    AIService::global_instance()
        .await
        .map_err(|e| ServiceError::Unavailable(e.to_string()))
}

#[derive(Default)]
pub struct Ai {
    started: OnceCell<()>,
}

/// Forward a core pubsub topic to a service event.
pub(crate) fn bridge(
    topic: &'static String,
    events: EventEmitter,
    event: &'static str,
    map: fn(Value) -> Option<(EventOwner, Value)>,
) {
    tokio::spawn(async move {
        let mut rx = crate::pubsub::get_global_pubsub()
            .await
            .subscribe(topic)
            .await;
        loop {
            match rx.recv().await {
                Ok(msg) => {
                    let Some((owner, payload)) = serde_json::from_str(&msg).ok().and_then(map)
                    else {
                        continue;
                    };
                    if let Err(e) = events.emit(event, owner, payload).await {
                        log::error!("service event `{}` dropped: {}", event, e);
                    }
                }
                Err(tokio::sync::broadcast::error::RecvError::Lagged(n)) => {
                    log::warn!("service event bridge `{}` lagged by {} messages", event, n)
                }
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
            }
        }
    });
}

#[async_trait]
impl ServiceImplementation for Ai {
    async fn start(&self, ctx: StartContext) -> Result<(), String> {
        if self.started.set(()).is_err() {
            return Ok(());
        }
        bridge(
            &crate::pubsub::AI_TRANSCRIPTION_TEXT_TOPIC,
            ctx.events.clone(),
            "transcription-text",
            |v| {
                let owner = match v.get("userDid").and_then(Value::as_str) {
                    Some(did) => EventOwner::Agent(did.to_string()),
                    None => EventOwner::All,
                };
                let payload =
                    serde_json::json!({ "streamId": v.get("streamId")?, "text": v.get("text")? });
                Some((owner, payload))
            },
        );
        bridge(
            &crate::pubsub::AI_MODEL_LOADING_STATUS,
            ctx.events,
            "model-loading-status",
            |v| Some((EventOwner::All, v)),
        );
        Ok(())
    }

    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }

    async fn health(&self) -> ServiceHealth {
        ServiceHealth::Running
    }

    async fn call(&self, method: &str, p: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        match method {
            // ai.inference
            "prompt" => {
                let p: PromptParams = params(p)?;
                let result = service()
                    .await?
                    .prompt(p.task_id, p.prompt, Some(token(&ctx)))
                    .await
                    .map_err(internal)?;
                to_value(result.text)
            }
            "embed" => {
                let p: EmbedParams = params(p)?;
                let embedding = service()
                    .await?
                    .embed(p.model_id, p.text, Some(token(&ctx)))
                    .await
                    .map_err(internal)?;
                let json = serde_json::to_string(&embedding.embeddings).map_err(internal)?;
                let compressed = deflate::deflate_bytes_zlib(json.as_bytes());
                to_value(base64::prelude::BASE64_STANDARD.encode(compressed))
            }
            "transcriptionOpen" => {
                let p: TranscriptionOpenParams = params(p)?;
                let id = service()
                    .await?
                    .open_transcription_stream(p.model_id, p.params.map(Into::into), token(&ctx))
                    .await
                    .map_err(internal)?;
                to_value(id)
            }
            "transcriptionClose" => {
                let p: StreamIdParams = params(p)?;
                service()
                    .await?
                    .close_transcription_stream(&p.stream_id, &token(&ctx))
                    .await
                    .map_err(internal)?;
                to_value(true)
            }
            "tasks" => to_value(AIService::get_tasks().map_err(internal)?),
            "addTask" => {
                let p: AddTaskParams = params(p)?;
                to_value(service().await?.add_task(p.task).await.map_err(internal)?)
            }
            "updateTask" => {
                let p: UpdateTaskParams = params(p)?;
                to_value(
                    service()
                        .await?
                        .update_task(p.task)
                        .await
                        .map_err(internal)?,
                )
            }
            "removeTask" => {
                let p: IdParams = params(p)?;
                service().await?.delete_task(p.id).await.map_err(internal)?;
                to_value(true)
            }
            // ai.models
            "models" => {
                service().await?;
                to_value(Ad4mDb::with_global_instance(|db| db.get_models()).map_err(internal)?)
            }
            "discoverModels" => {
                let p: DiscoverModelsParams = params(p)?;
                let base_url = url::Url::parse(&p.base_url).map_err(|e| {
                    ServiceError::method("InvalidEndpoint", format!("Invalid baseUrl: {e}"))
                })?;
                let api_type = match p.api_type.as_deref() {
                    Some(raw) => raw
                        .parse::<crate::types::ModelApiType>()
                        .map_err(|e| ServiceError::method("InvalidEndpoint", e))?,
                    None => crate::types::ModelApiType::OpenAi,
                };
                let api_key = p.api_key.unwrap_or_default();
                // A key over plain HTTP crosses the network in the clear;
                // keyless discovery against any host stays allowed.
                if !api_key.is_empty() && !is_transport_safe(&base_url) {
                    return Err(ServiceError::method(
                        "CleartextCredential",
                        CLEARTEXT_KEY_REFUSAL,
                    ));
                }
                let models =
                    crate::ai_service::providers::list_models(&api_type, &api_key, base_url)
                        .await
                        .map_err(|e| ServiceError::method("EndpointRefused", e.to_string()))?;
                to_value(models)
            }
            "addModel" => {
                let p: AddModelParams = params(p)?;
                refuse_cleartext_credential(&p.model)?;
                to_value(
                    service()
                        .await?
                        .add_model(p.model)
                        .await
                        .map_err(internal)?,
                )
            }
            "updateModel" => {
                let p: UpdateModelParams = params(p)?;
                refuse_cleartext_credential(&p.model)?;
                service()
                    .await?
                    .update_model(p.id, p.model)
                    .await
                    .map_err(|e| internal(format!("Failed to update model: {}", e)))?;
                to_value(true)
            }
            "removeModel" => {
                let p: IdParams = params(p)?;
                service()
                    .await?
                    .remove_model(p.id)
                    .await
                    .map_err(internal)?;
                to_value(true)
            }
            "setDefaultModel" => {
                let p: SetDefaultModelParams = params(p)?;
                service()
                    .await?
                    .set_default_model(p.model_type, p.id)
                    .await
                    .map_err(
                        |e| match e.downcast_ref::<crate::db::InvalidDefaultModel>() {
                            Some(invalid) => {
                                ServiceError::method("InvalidDefaultModel", invalid.to_string())
                            }
                            None => internal(e),
                        },
                    )?;
                to_value(true)
            }
            "getDefaultModel" => {
                let p: ModelTypeParams = params(p)?;
                let id = Ad4mDb::with_global_instance(|db| db.get_default_model(p.model_type))
                    .map_err(internal)?;
                let model = match id {
                    Some(id) => {
                        Ad4mDb::with_global_instance(|db| db.get_model(id)).map_err(internal)?
                    }
                    None => None,
                };
                to_value(model)
            }
            "modelLoadingStatus" => {
                let p: ModelParams = params(p)?;
                to_value(AIService::model_status(p.model).await.map_err(internal)?)
            }
            other => Err(internal(format!("ai has no method {}", other))),
        }
    }
}
