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

/// One chat message.
#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChatMessage {
    /// `system`, `user` or `assistant`.
    pub role: String,
    pub content: String,
}

/// A tool the model may call when its answer is grammar-constrained.
#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct GrammarTool {
    pub name: String,
    #[serde(default)]
    pub description: Option<String>,
    /// JSON Schema of the arguments.
    #[serde(default)]
    pub parameters: Option<Value>,
}

#[derive(Serialize, Deserialize, JsonSchema, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum GrammarChoice {
    /// The model may answer with a call or with text (not constrained).
    Auto,
    /// The model must call one of the tools.
    Required,
    /// The model must call this tool.
    Named(String),
}

/// Constrain decoding to well-formed tool calls (models without native tools).
#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ToolGrammar {
    pub tools: Vec<GrammarTool>,
    pub choice: GrammarChoice,
    /// Allow several calls in one answer.
    pub parallel: bool,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChatParams {
    pub model_id: String,
    pub messages: Vec<ChatMessage>,
    #[serde(default)]
    pub tool_grammar: Option<ToolGrammar>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChatStreamParams {
    pub model_id: String,
    pub messages: Vec<ChatMessage>,
    #[serde(default)]
    pub tool_grammar: Option<ToolGrammar>,
    pub stream_id: String,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ChatResult {
    pub text: String,
    pub prompt_tokens: u64,
    pub completion_tokens: u64,
    /// The model that answered, after variable resolution.
    pub model_id: String,
}

/// `chat-delta`: the next piece of a streaming answer.
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ChatDelta {
    pub stream_id: String,
    pub delta: String,
}

#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct WireToolCall {
    pub id: String,
    pub name: String,
    /// The arguments object.
    pub arguments: Value,
}

#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChatTurnInput {
    /// `system`, `user` or `assistant`.
    pub role: String,
    pub content: String,
    /// Calls this assistant turn made.
    #[serde(default)]
    pub tool_calls: Vec<WireToolCall>,
    /// Set when this turn is a tool result: the call it answers.
    #[serde(default)]
    pub tool_result_for: Option<String>,
}

#[derive(Serialize, Deserialize, JsonSchema, Clone)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ToolSpecInput {
    pub name: String,
    pub description: String,
    /// JSON Schema of the arguments.
    pub parameters: Value,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChatWithToolsParams {
    pub model_id: String,
    pub turns: Vec<ChatTurnInput>,
    pub tools: Vec<ToolSpecInput>,
}

#[derive(Serialize, Deserialize, JsonSchema, Default)]
#[serde(rename_all = "camelCase")]
pub struct Usage {
    pub input_tokens: Option<u64>,
    pub output_tokens: Option<u64>,
    pub cache_read_tokens: Option<u64>,
    pub cache_write_tokens: Option<u64>,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ToolReply {
    pub text: String,
    pub tool_calls: Vec<WireToolCall>,
    pub usage: Usage,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Embedding {
    pub vector: Vec<f32>,
    pub token_count: u64,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct TranscribeParams {
    pub model_id: String,
    /// Mono PCM, Float32 little-endian, base64.
    #[schemars(extend("contentEncoding" = "base64"))]
    pub audio: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct TranscriptionFeedParams {
    pub stream_ids: Vec<String>,
    /// Mono PCM, Float32 little-endian, base64.
    #[schemars(extend("contentEncoding" = "base64"))]
    pub audio: String,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct FeedFailure {
    pub stream_id: String,
    pub error: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct EnsureTaskParams {
    /// Tasks are matched by name: an existing one is returned as it is.
    pub task: AITaskInput,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct EnsuredTask {
    pub task: AITask,
    /// This call created (and spawned) the task.
    pub created: bool,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ModelIdParams {
    pub model_id: String,
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
    .method::<ChatParams, ChatResult>(
        "chat",
        "PROMPT",
        "Answer a conversation. `toolGrammar` constrains the answer to well-formed tool calls.",
        MethodOptions { long: true, meter: Some("ai.prompt"), ..Default::default() },
    )
    .method::<ChatStreamParams, ChatResult>(
        "chatStream",
        "PROMPT",
        "`chat`, with the answer streamed as `chat-delta` events while it is generated.",
        MethodOptions { long: true, stream: Some("chat-delta"), meter: Some("ai.prompt"), ..Default::default() },
    )
    .method::<ChatWithToolsParams, ToolReply>(
        "chatWithTools",
        "PROMPT",
        "Answer a conversation on a model with native tool calling; answers text and the calls it wants.",
        MethodOptions { long: true, meter: Some("ai.prompt"), errors: vec![("InvalidRole", 422)], ..Default::default() },
    )
    .method::<EmbedParams, Embedding>(
        "embedding",
        "PROMPT",
        "`embed` as a plain vector, with the token count.",
        MethodOptions { long: true, meter: Some("ai.embed"), ..Default::default() },
    )
    .method::<TranscribeParams, String>(
        "transcribe",
        "TRANSCRIBE",
        "Transcribe one buffer of audio.",
        MethodOptions { long: true, meter: Some("ai.transcription"), errors: vec![("InvalidAudio", 422)], ..Default::default() },
    )
    .method::<TranscriptionFeedParams, Vec<FeedFailure>>(
        "transcriptionFeed",
        "TRANSCRIBE",
        "Feed audio to open transcription streams; answers the streams that refused it. Text arrives as `transcription-text`.",
        MethodOptions { meter: Some("ai.transcription"), errors: vec![("AllStreamsFailed", 409), ("InvalidAudio", 422)], ..Default::default() },
    )
    .method::<EnsureTaskParams, EnsuredTask>(
        "ensureTask",
        "TASKS",
        "The task with this name, created and spawned when missing.",
        MethodOptions::default(),
    )
    .event::<ChatDelta>("chat-delta", "PROMPT", "The next piece of a streaming chat answer.", Some("streamId"))
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
    .method::<SetDefaultModelParams, bool>("setDefaultModel", "MANAGE", "Make a model the default of its type.", MethodOptions::default())
    .method::<ModelTypeParams, Option<Model>>("getDefaultModel", "READ", "The default model of a type.", MethodOptions { read: true, ..Default::default() })
    .method::<ModelParams, AIModelLoadingStatus>("modelLoadingStatus", "READ", "Download and load progress of a model.", MethodOptions { read: true, ..Default::default() })
    .method::<ModelIdParams, bool>(
        "supportsNativeTools",
        "READ",
        "Does this model take tools as data (rather than through a rendered prompt)?",
        MethodOptions { read: true, ..Default::default() },
    )
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

// ── Executor-internal callers ───────────────────────────────────────────────

/// The context of an executor-internal AI call with no user behind it (not
/// billed).
pub fn executor_ctx(module: &str) -> CallContext {
    CallContext::system(
        crate::services::Caller::Executor {
            module: module.into(),
        },
        None,
        None,
    )
}

/// `ai.inference.prompt` from executor code.
pub async fn prompt(
    ctx: &CallContext,
    task_id: &str,
    prompt: &str,
) -> Result<String, crate::api::ws_handler::WsRpcError> {
    super::call(
        super::Builtin::AiInference,
        "prompt",
        serde_json::json!({ "taskId": task_id, "prompt": prompt }),
        ctx,
    )
    .await
}

/// `ai.inference.embedding` from executor code.
pub async fn embedding(
    ctx: &CallContext,
    model_id: &str,
    text: &str,
) -> Result<Embedding, crate::api::ws_handler::WsRpcError> {
    super::call(
        super::Builtin::AiInference,
        "embedding",
        serde_json::json!({ "modelId": model_id, "text": text }),
        ctx,
    )
    .await
}

/// `ai.inference.ensureTask` from executor code.
pub async fn ensure_task(
    ctx: &CallContext,
    task: &AITaskInput,
) -> Result<EnsuredTask, crate::api::ws_handler::WsRpcError> {
    super::call(
        super::Builtin::AiInference,
        "ensureTask",
        serde_json::json!({ "task": task }),
        ctx,
    )
    .await
}

/// The context of an AI call made for the session behind `auth_token`,
/// outside a request the host dispatched. It holds that token's grants and
/// admin flag, never more; without a token it is the executor's own call.
pub fn token_ctx(module: &str, auth_token: Option<String>) -> CallContext {
    let Some(token) = auth_token else {
        return executor_ctx(module);
    };
    let admin = crate::config::try_get_global_config().and_then(|c| c.admin_credential);
    CallContext {
        caller: crate::services::Caller::Executor {
            module: module.into(),
        },
        origin: Vec::new(),
        agent_did: None,
        user: crate::agent::capabilities::user_email_from_token(token.clone()),
        is_admin: crate::agent::capabilities::is_admin_credential_token(&token, &admin),
        grants: vec![
            crate::agent::capabilities::capabilities_from_token(token.clone(), admin)
                .unwrap_or_default(),
        ],
        auth_token: Some(token),
        deadline: None,
    }
}

/// `ai.inference.chat` from executor code.
pub async fn chat(
    ctx: &CallContext,
    model_id: &str,
    messages: Vec<(String, String)>,
    tool_grammar: Option<ToolGrammar>,
) -> Result<ChatResult, crate::api::ws_handler::WsRpcError> {
    let messages: Vec<ChatMessage> = messages
        .into_iter()
        .map(|(role, content)| ChatMessage { role, content })
        .collect();
    super::call(
        super::Builtin::AiInference,
        "chat",
        serde_json::json!({ "modelId": model_id, "messages": messages, "toolGrammar": tool_grammar }),
        ctx,
    )
    .await
}

/// `ai.inference.chatWithTools` from executor code.
pub async fn chat_with_tools(
    ctx: &CallContext,
    model_id: &str,
    turns: &[crate::ai_service::providers::ChatTurn],
    tools: &[crate::ai_service::providers::ToolSpec],
) -> Result<ToolReply, crate::api::ws_handler::WsRpcError> {
    use crate::ai_service::providers::ChatRole;
    let turns: Vec<ChatTurnInput> = turns
        .iter()
        .map(|t| ChatTurnInput {
            role: match t.role {
                ChatRole::System => "system",
                ChatRole::User => "user",
                ChatRole::Assistant => "assistant",
            }
            .into(),
            content: t.content.clone(),
            tool_calls: t
                .tool_calls
                .iter()
                .map(|c| WireToolCall {
                    id: c.id.clone(),
                    name: c.name.clone(),
                    arguments: c.arguments.clone(),
                })
                .collect(),
            tool_result_for: t.tool_result_for.clone(),
        })
        .collect();
    let tools: Vec<ToolSpecInput> = tools
        .iter()
        .map(|t| ToolSpecInput {
            name: t.name.clone(),
            description: t.description.clone(),
            parameters: t.parameters.clone(),
        })
        .collect();
    super::call(
        super::Builtin::AiInference,
        "chatWithTools",
        serde_json::json!({ "modelId": model_id, "turns": turns, "tools": tools }),
        ctx,
    )
    .await
}

/// `ai.models.supportsNativeTools` from executor code. A model capability,
/// not user data, so the executor asks: a caller holding only
/// `ai.inference` must not lose native tool calling. `false` on error.
pub async fn supports_native_tools(model_id: &str) -> bool {
    super::call(
        super::Builtin::AiModels,
        "supportsNativeTools",
        serde_json::json!({ "modelId": model_id }),
        &executor_ctx("ai_tools"),
    )
    .await
    .unwrap_or_else(|e| {
        log::warn!("supportsNativeTools({}) failed: {}", model_id, e.message);
        false
    })
}

/// How long a finished stream waits for its end marker.
const STREAM_END_GRACE: std::time::Duration = std::time::Duration::from_secs(10);

/// `ai.inference.chatStream` from executor code: the answer's pieces as they
/// arrive (closed when the stream ends), and the final result.
pub fn chat_stream(
    ctx: CallContext,
    model_id: String,
    messages: Vec<(String, String)>,
    tool_grammar: Option<ToolGrammar>,
) -> (
    tokio::sync::mpsc::UnboundedReceiver<String>,
    tokio::sync::oneshot::Receiver<Result<ChatResult, crate::api::ws_handler::WsRpcError>>,
) {
    let stream_id = uuid::Uuid::new_v4().to_string();
    let mut chunks = crate::services::host().watch_stream(
        super::event_type(super::Builtin::AiInference, "chat-delta"),
        stream_id.clone(),
    );
    let (token_tx, token_rx) = tokio::sync::mpsc::unbounded_channel();
    let (done_tx, done_rx) = tokio::sync::oneshot::channel();
    let messages: Vec<ChatMessage> = messages
        .into_iter()
        .map(|(role, content)| ChatMessage { role, content })
        .collect();
    let params = serde_json::json!({ "modelId": model_id, "messages": messages, "toolGrammar": tool_grammar, "streamId": stream_id });
    tokio::spawn(async move {
        let call =
            super::call::<ChatResult>(super::Builtin::AiInference, "chatStream", params, &ctx);
        tokio::pin!(call);
        let forward = |c: Value| {
            if let Some(delta) = c.get("delta").and_then(Value::as_str) {
                let _ = token_tx.send(delta.to_string());
            }
        };
        let result = loop {
            tokio::select! {
                chunk = chunks.recv() => match chunk {
                    Some(c) => forward(c),
                    // The stream ended: every piece has arrived.
                    None => break call.await,
                },
                r = &mut call => break r,
            }
        };
        // The reply can overtake the last pieces; wait for the stream end
        // behind them, but not forever (a refused call has no stream).
        if result.is_ok() {
            let grace = tokio::time::sleep(STREAM_END_GRACE);
            tokio::pin!(grace);
            loop {
                tokio::select! {
                    chunk = chunks.recv() => match chunk {
                        Some(c) => forward(c),
                        None => break,
                    },
                    _ = &mut grace => break,
                }
            }
        }
        drop(chunks);
        drop(token_tx);
        let _ = done_tx.send(result);
    });
    (token_rx, done_rx)
}

/// Samples → the base64 Float32 little-endian PCM the audio methods take.
pub fn encode_pcm(samples: &[f32]) -> String {
    let bytes: Vec<u8> = samples.iter().flat_map(|s| s.to_le_bytes()).collect();
    base64::prelude::BASE64_STANDARD.encode(bytes)
}

/// `ai.inference.transcribe` from executor code.
pub async fn transcribe(
    ctx: &CallContext,
    model_id: &str,
    samples: &[f32],
) -> Result<String, crate::api::ws_handler::WsRpcError> {
    super::call(
        super::Builtin::AiInference,
        "transcribe",
        serde_json::json!({ "modelId": model_id, "audio": encode_pcm(samples) }),
        ctx,
    )
    .await
}

/// `ai.inference.transcriptionFeed` from executor code: the streams that
/// refused the audio.
pub async fn transcription_feed(
    ctx: &CallContext,
    stream_ids: &[String],
    samples: &[f32],
) -> Result<Vec<FeedFailure>, crate::api::ws_handler::WsRpcError> {
    super::call(
        super::Builtin::AiInference,
        "transcriptionFeed",
        serde_json::json!({ "streamIds": stream_ids, "audio": encode_pcm(samples) }),
        ctx,
    )
    .await
}

fn pairs(messages: Vec<ChatMessage>) -> Vec<(String, String)> {
    messages.into_iter().map(|m| (m.role, m.content)).collect()
}

fn chat_result(r: crate::ai_service::PromptResult) -> ChatResult {
    ChatResult {
        text: r.text,
        prompt_tokens: r.prompt_tokens as u64,
        completion_tokens: r.completion_tokens as u64,
        model_id: r.model_id,
    }
}

/// The decoding constraint a tool grammar asks for; `None` for `auto`.
fn constraint_for(g: &ToolGrammar) -> Option<kalosm::language::ArcParser<()>> {
    use crate::api::openai_compat::{tool_grammar, types};
    let tools: Vec<types::ToolDef> = g
        .tools
        .iter()
        .map(|t| types::ToolDef {
            kind: "function".into(),
            function: types::FunctionDef {
                name: t.name.clone(),
                description: t.description.clone(),
                parameters: t.parameters.clone(),
            },
        })
        .collect();
    let choice = match &g.choice {
        GrammarChoice::Auto => tool_grammar::ToolChoice::Auto,
        GrammarChoice::Required => tool_grammar::ToolChoice::Required,
        GrammarChoice::Named(n) => tool_grammar::ToolChoice::Named(n.clone()),
    };
    tool_grammar::build_tool_call_parser(&tools, &choice, g.parallel)
}

fn turn(t: ChatTurnInput) -> Result<crate::ai_service::providers::ChatTurn, ServiceError> {
    use crate::ai_service::providers::{ChatRole, ChatTurn, ToolCall};
    let role = match t.role.as_str() {
        "system" => ChatRole::System,
        "user" => ChatRole::User,
        "assistant" => ChatRole::Assistant,
        other => {
            return Err(ServiceError::method(
                "InvalidRole",
                format!("unknown role `{}`", other),
            ))
        }
    };
    Ok(ChatTurn {
        role,
        content: t.content,
        tool_calls: t
            .tool_calls
            .into_iter()
            .map(|c| ToolCall {
                id: c.id,
                name: c.name,
                arguments: c.arguments,
            })
            .collect(),
        tool_result_for: t.tool_result_for,
    })
}

/// Base64 Float32 little-endian PCM → samples.
fn pcm_f32le(b64: &str) -> Result<Vec<f32>, ServiceError> {
    let bytes = base64::prelude::BASE64_STANDARD
        .decode(b64)
        .map_err(|e| ServiceError::method("InvalidAudio", format!("audio is not base64: {}", e)))?;
    if bytes.len() % 4 != 0 {
        return Err(ServiceError::method(
            "InvalidAudio",
            "audio length must be a multiple of 4 (Float32 samples)",
        ));
    }
    Ok(bytes
        .chunks_exact(4)
        .map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]]))
        .collect())
}

/// The AI task named like `input`, inserted when missing. Answers whether
/// this call inserted it.
pub fn ensure_task_row(input: AITaskInput) -> Result<(AITask, bool), String> {
    let existing = Ad4mDb::with_global_instance(|db| db.get_tasks()).map_err(|e| e.to_string())?;
    if let Some(task) = existing.into_iter().find(|t| t.name == input.name) {
        return Ok((task, false));
    }
    let examples = input
        .prompt_examples
        .into_iter()
        .map(|e| crate::types::AIPromptExamples {
            input: e.input,
            output: e.output,
        })
        .collect();
    let id = Ad4mDb::with_global_instance(|db| {
        db.add_task(
            input.name.clone(),
            input.model_id,
            input.system_prompt,
            examples,
            input.meta_data,
        )
    })
    .map_err(|e| e.to_string())?;
    let task = Ad4mDb::with_global_instance(|db| db.get_task(id))
        .map_err(|e| e.to_string())?
        .ok_or("task vanished right after it was inserted")?;
    Ok((task, true))
}

async fn service() -> Result<AIService, ServiceError> {
    AIService::global_instance()
        .await
        .map_err(|e| ServiceError::Unavailable(e.to_string()))
}

#[derive(Default)]
pub struct Ai {
    events: OnceCell<EventEmitter>,
}

/// Forward a core pubsub topic to a service event.
fn bridge(
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
        if self.events.set(ctx.events.clone()).is_err() {
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
            "chat" => {
                let p: ChatParams = params(p)?;
                let constraint = p.tool_grammar.as_ref().and_then(constraint_for);
                let r = service()
                    .await?
                    .prompt_messages(
                        p.model_id,
                        pairs(p.messages),
                        ctx.auth_token.clone(),
                        constraint,
                    )
                    .await
                    .map_err(internal)?;
                to_value(chat_result(r))
            }
            "chatStream" => {
                let p: ChatStreamParams = params(p)?;
                let constraint = p.tool_grammar.as_ref().and_then(constraint_for);
                let (mut tokens, done) = service()
                    .await?
                    .prompt_messages_stream(
                        p.model_id,
                        pairs(p.messages),
                        ctx.auth_token.clone(),
                        constraint,
                    )
                    .await
                    .map_err(internal)?;
                let events = self
                    .events
                    .get()
                    .ok_or_else(|| ServiceError::Unavailable("not started".into()))?;
                let owner = ctx
                    .agent_did
                    .clone()
                    .map(EventOwner::Agent)
                    .unwrap_or(EventOwner::Executor);
                while let Some(delta) = tokens.recv().await {
                    let payload = serde_json::json!({ "streamId": p.stream_id, "delta": delta });
                    if let Err(e) = events.emit("chat-delta", owner.clone(), payload).await {
                        log::error!("chat-delta dropped: {}", e);
                    }
                }
                let r = done.await.map_err(internal)?.map_err(internal)?;
                to_value(chat_result(r))
            }
            "chatWithTools" => {
                let p: ChatWithToolsParams = params(p)?;
                let turns = p
                    .turns
                    .into_iter()
                    .map(turn)
                    .collect::<Result<Vec<_>, _>>()?;
                let tools = p
                    .tools
                    .into_iter()
                    .map(|t| crate::ai_service::providers::ToolSpec {
                        name: t.name,
                        description: t.description,
                        parameters: t.parameters,
                    })
                    .collect();
                let reply = service()
                    .await?
                    .prompt_with_tools(p.model_id, turns, tools, ctx.auth_token.clone())
                    .await
                    .map_err(internal)?;
                to_value(ToolReply {
                    text: reply.text,
                    tool_calls: reply
                        .tool_calls
                        .into_iter()
                        .map(|c| WireToolCall {
                            id: c.id,
                            name: c.name,
                            arguments: c.arguments,
                        })
                        .collect(),
                    usage: Usage {
                        input_tokens: reply.usage.input_tokens,
                        output_tokens: reply.usage.output_tokens,
                        cache_read_tokens: reply.usage.cache_read_tokens,
                        cache_write_tokens: reply.usage.cache_write_tokens,
                    },
                })
            }
            "embedding" => {
                let p: EmbedParams = params(p)?;
                let r = service()
                    .await?
                    .embed(p.model_id, p.text, ctx.auth_token.clone())
                    .await
                    .map_err(internal)?;
                to_value(Embedding {
                    vector: r.embeddings,
                    token_count: r.token_count as u64,
                })
            }
            "transcribe" => {
                let p: TranscribeParams = params(p)?;
                let samples = pcm_f32le(&p.audio)?;
                to_value(
                    service()
                        .await?
                        .transcribe_buffer(p.model_id, samples, token(&ctx))
                        .await
                        .map_err(internal)?,
                )
            }
            "transcriptionFeed" => {
                let p: TranscriptionFeedParams = params(p)?;
                let samples = pcm_f32le(&p.audio)?;
                let service = service().await?;
                let mut failed = Vec::new();
                for stream_id in &p.stream_ids {
                    if let Err(e) = service
                        .feed_transcription_stream(stream_id, samples.clone(), &token(&ctx))
                        .await
                    {
                        failed.push(FeedFailure {
                            stream_id: stream_id.clone(),
                            error: e.to_string(),
                        });
                    }
                }
                if !p.stream_ids.is_empty() && failed.len() == p.stream_ids.len() {
                    let all = failed
                        .iter()
                        .map(|f| format!("{}: {}", f.stream_id, f.error))
                        .collect::<Vec<_>>()
                        .join("; ");
                    return Err(ServiceError::Method {
                        name: "AllStreamsFailed".into(),
                        data: None,
                        message: format!("All streams failed: {}", all),
                    });
                }
                to_value(failed)
            }
            "ensureTask" => {
                let p: EnsureTaskParams = params(p)?;
                let (task, created) = ensure_task_row(p.task).map_err(internal)?;
                if created {
                    service()
                        .await?
                        .spawn_registered_task(task.clone())
                        .await
                        .map_err(internal)?;
                }
                to_value(EnsuredTask { task, created })
            }
            "supportsNativeTools" => {
                let p: ModelIdParams = params(p)?;
                to_value(AIService::model_supports_native_tools(&p.model_id))
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
                    .map_err(internal)?;
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
