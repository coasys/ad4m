use self::error::AIServiceError;
use crate::db::Ad4mDb;
use crate::types::RequestContext;
use crate::types::{AITask, AITaskInput, Endpoint, Model, ModelInput, ModelType};
use anyhow::{anyhow, Context};
use deno_core::error::AnyError;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio::time::sleep;

pub mod endpoints;
mod error;
pub mod harness;
pub mod providers;
pub(crate) mod transcription;
#[cfg(test)]
mod transcription_e2e;
use log::error;
use providers::{ChatReply, ChatRequest, ChatTurn, RemoteChat, ToolSpec};

pub type Result<T> = std::result::Result<T, AnyError>;

/// Result of an LLM prompt call, with token counts for billing (estimated, chars/4).
#[derive(Debug, Clone)]
pub struct PromptResult {
    pub text: String,
    pub prompt_tokens: usize,
    pub completion_tokens: usize,
    pub model_id: String,
}

/// Result of an embedding call, with token count for billing.
pub struct EmbedResult {
    pub embeddings: Vec<f32>,
    pub token_count: usize,
}

/// The vectors of a batch, in input order, and the input tokens billed for it: the
/// upstream's count when it reports one, else the estimate.
pub struct EmbedBatch {
    pub vectors: Vec<Vec<f32>>,
    pub token_count: usize,
}

/// Rough token count estimation (~4 chars per token for English text), for the paths that
/// bill without an upstream count.
///
/// Materially off for non-English scripts (CJK, Arabic, Hindi tend to ~1 token/char, so
/// billing under-estimates ~4×), code/JSON (~2 chars/token) and repetitive text (BPE
/// compresses well: over-estimated). `/v1` bills from the upstream's `usage` instead.
///
/// `pub(crate)` so `api::openai_compat::tests` can lock in the current
/// formula and catch accidental changes.
pub(crate) fn estimate_token_count(text: &str) -> usize {
    let chars = text.chars().count();
    (chars + 3) / 4
}

/// Truncate LLM prompt/response text for debug logging. LOGGING.md: debug
/// level carries "prompt/response bodies (truncated)" — long enough to
/// actually debug what the model was asked / returned, bounded so a huge
/// interpretation prompt doesn't flood the log file. Char-boundary safe.
const LOG_TEXT_PREVIEW_CHARS: usize = 2000;

pub(crate) fn truncate_for_log(text: &str) -> String {
    let total = text.chars().count();
    if total <= LOG_TEXT_PREVIEW_CHARS {
        return text.to_string();
    }
    let head: String = text.chars().take(LOG_TEXT_PREVIEW_CHARS).collect();
    format!("{head}… [+{} more chars]", total - LOG_TEXT_PREVIEW_CHARS)
}

static TRANSCRIPTION_TIMEOUT_SECS: u64 = 30; // 30 seconds (was 2 minutes)
static TRANSCRIPTION_CHECK_INTERVAL_SECS: u64 = 5; // 5 seconds (was 10)

lazy_static! {
    static ref AI_SERVICE: Arc<Mutex<Option<AIService>>> = Arc::new(Mutex::new(None));
}

/// An open transcription stream. Removing it from the map closes it: its sender drops, and
/// its worker ([`transcription::RemoteStream`]) ends once it has answered the utterances
/// fed before.
struct TranscriptionSession {
    /// The worker's queue: it answers each utterance after the ones fed before it.
    utterances: mpsc::UnboundedSender<transcription::Utterance>,
    last_activity: Arc<Mutex<std::time::Instant>>,
    /// The stream's text, for `/v1/realtime`.
    text_broadcast_tx: tokio::sync::broadcast::Sender<String>,
    /// The managed agent that opened the stream, by DID: it pays, and only it reaches the
    /// stream. `None`: the main agent.
    agent_did: Option<String>,
}

type TranscriptionStreams = Arc<Mutex<HashMap<String, TranscriptionSession>>>;

#[cfg(test)]
impl TranscriptionSession {
    /// A session of `owner`, and the queue its worker would read.
    fn for_test(owner: Option<&str>) -> (Self, mpsc::UnboundedReceiver<transcription::Utterance>) {
        let (utterances, queue) = mpsc::unbounded_channel();
        let (text_broadcast_tx, _) = tokio::sync::broadcast::channel(1);
        let session = TranscriptionSession {
            utterances,
            last_activity: Arc::new(Mutex::new(std::time::Instant::now())),
            text_broadcast_tx,
            agent_did: owner.map(str::to_string),
        };
        (session, queue)
    }
}

/// Every model the service runs is an upstream model on an endpoint: a client of the
/// endpoint per model, held in a map by model id. A client is `Send + Sync` and takes
/// concurrent requests; an endpoint's `max_concurrent` caps them.
#[derive(Clone)]
pub struct AIService {
    /// The chat client of each LLM, keyed by model id.
    llms: Arc<Mutex<HashMap<String, ChatModel>>>,
    /// The embedding client of each embedding model, keyed by model id.
    embedders: Arc<Mutex<HashMap<String, Embedder>>>,
    transcription_streams: TranscriptionStreams,
}

/// What prompts one LLM: the client of its endpoint, and the id the endpoint knows the
/// model by.
#[derive(Clone)]
struct ChatModel {
    client: Arc<dyn RemoteChat>,
    remote_model: String,
    endpoint: Arc<Endpoint>,
}

impl ChatModel {
    fn new(model: &Model, endpoint: Endpoint) -> Result<Self> {
        Ok(ChatModel {
            client: Arc::from(providers::chat_client(&endpoint)?),
            remote_model: model.remote_model.clone(),
            endpoint: Arc::new(endpoint),
        })
    }

    /// One completion of `turns`, with `tools` as definitions, in a slot of the endpoint's
    /// concurrency cap when it has one.
    async fn chat(&self, turns: Vec<ChatTurn>, tools: Vec<ToolSpec>) -> Result<ChatReply> {
        let _slot = endpoints::request_slot(&self.endpoint).await;
        let request = ChatRequest::new(self.remote_model.clone(), turns).with_tools(tools);
        self.client.chat(request).await
    }

    /// [`Self::chat`] with the text sent to `tokens` as the model writes it (in one chunk
    /// from a provider that does not stream).
    async fn chat_stream(
        &self,
        turns: Vec<ChatTurn>,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        let _slot = endpoints::request_slot(&self.endpoint).await;
        let request = ChatRequest::new(self.remote_model.clone(), turns);
        self.client.chat_stream(request, tokens).await
    }
}

/// What embeds for one embedding model: the client of its endpoint, and the id the
/// endpoint knows the model by.
#[derive(Clone)]
struct Embedder {
    client: Arc<dyn providers::embed::RemoteEmbed>,
    remote_model: String,
    endpoint: Arc<Endpoint>,
}

/// Every part of one turn that reaches the wire.
///
/// A turn is not only its text. An assistant turn that called something also
/// carries the call's id, name and arguments, and a tool result carries the id
/// it answers. The Anthropic provider serialises all of that into content
/// blocks, so the caller pays for it.
///
/// It compounds. Tool definitions are sent once per request, but the calls in
/// the history are resent on every turn of an agentic loop, and their arguments
/// are the payload rather than a label — a schema patch, a query, a document.
/// Counting only `content` under-bills a long tool conversation by more than it
/// under-bills a short one.
fn estimate_turn_tokens(turn: &ChatTurn) -> usize {
    let calls: usize = turn
        .tool_calls
        .iter()
        .map(|call| {
            estimate_token_count(&call.id)
                + estimate_token_count(&call.name)
                + estimate_token_count(&call.arguments.to_string())
        })
        .sum();

    let answered_call = turn
        .tool_result_for
        .as_deref()
        .map(estimate_token_count)
        .unwrap_or(0);

    estimate_token_count(&turn.content) + calls + answered_call
}

/// Estimated completion tokens for a reply that may carry tool calls.
///
/// A call is output the model generated and the caller pays for, so its name
/// and arguments count alongside the text. Its id does not: the provider
/// assigns it, and the model never generates it. `estimate_turn_tokens` does
/// count the id when the call is sent back, because then it is input.
fn estimate_reply_tokens(reply: &ChatReply) -> usize {
    let calls: usize = reply
        .tool_calls
        .iter()
        .map(|call| {
            estimate_token_count(&call.name) + estimate_token_count(&call.arguments.to_string())
        })
        .sum();

    estimate_token_count(&reply.text) + calls
}

/// The turn list for a task-shaped prompt: the task's system prompt, its
/// examples as alternating user/assistant turns, then the live prompt.
fn turns_for_task(task: &AITask, prompt: String) -> Vec<ChatTurn> {
    turns_for(&task.system_prompt, &task.prompt_examples, prompt)
}

/// A system prompt, its examples as alternating user/assistant turns, then `prompt`.
fn turns_for(
    system_prompt: &str,
    examples: &[crate::types::AIPromptExamples],
    prompt: String,
) -> Vec<ChatTurn> {
    let mut turns = Vec::with_capacity(2 + examples.len() * 2);
    turns.push(ChatTurn::system(system_prompt.to_string()));
    for example in examples {
        turns.push(ChatTurn::user(example.input.clone()));
        turns.push(ChatTurn::assistant(example.output.clone()));
    }
    turns.push(ChatTurn::user(prompt));
    turns
}

/// The owner of a new transcription stream: the managed agent's DID (billing, ownership;
/// `None` for the main agent) and the DID of the agent the caller acts as (event filtering).
/// A caller whose agent does not resolve owns nothing and opens no stream: text without an
/// owner DID would reach every session.
fn transcription_stream_owner(
    context: crate::agent::AgentContext,
) -> Result<(Option<String>, String)> {
    if let Some(managed) = context.managed_did() {
        crate::agent::AgentService::wallet_key_for_did(managed)?;
    }
    let did = crate::agent::did_for_context(&context)?;
    Ok((context.managed_did().map(str::to_string), did))
}

impl AIService {
    pub fn new() -> Result<Self> {
        let service = Self::empty();

        let clone = service.clone();
        tokio::spawn(async move {
            if let Err(e) = clone.load().await {
                error!("AIService error while loading models: {:?}", e);
            }
        });

        service.start_stream_reaper();
        Ok(service)
    }

    /// A service with no models, no streams and no background tasks.
    fn empty() -> Self {
        AIService {
            llms: Arc::new(Mutex::new(HashMap::new())),
            embedders: Arc::new(Mutex::new(HashMap::new())),
            transcription_streams: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Close the streams nobody fed for [`TRANSCRIPTION_TIMEOUT_SECS`], every
    /// [`TRANSCRIPTION_CHECK_INTERVAL_SECS`], for as long as the service lives.
    ///
    /// The task holds the stream map weakly: it ends when the last service holding the map
    /// is gone, which for the global instance is never. A clone being dropped (one per
    /// `global_instance()` call) does not end it.
    fn start_stream_reaper(&self) {
        let streams = Arc::downgrade(&self.transcription_streams);
        tokio::spawn(async move {
            loop {
                sleep(Duration::from_secs(TRANSCRIPTION_CHECK_INTERVAL_SECS)).await;
                let Some(streams) = streams.upgrade() else {
                    log::info!("Transcription stream reaper stopped: the service is gone");
                    return;
                };
                Self::close_idle_streams(&streams).await;
            }
        });
    }

    async fn close_idle_streams(streams: &TranscriptionStreams) {
        let mut map_lock = streams.lock().await;
        let mut idle = Vec::new();
        for (id, stream) in map_lock.iter() {
            if stream.last_activity.lock().await.elapsed()
                > Duration::from_secs(TRANSCRIPTION_TIMEOUT_SECS)
            {
                idle.push(id.clone());
            }
        }
        for id in idle {
            map_lock.remove(&id);
            log::info!("Transcription stream {id} closed: idle");
        }
    }

    /// Build the client of every model in the database. A fresh executor has none, and
    /// loads none: AI features answer that no model is configured until the operator adds
    /// one. A model whose client does not build logs why and stays without one.
    pub async fn load(&self) -> Result<()> {
        let models = Ad4mDb::with_global_instance(|db| db.get_models())?;
        for model in models {
            if let Err(e) = self.init_model(&model).await {
                error!(
                    "AI model {:?} ({}) did not load: {e:#}",
                    model.name, model.id
                );
            }
        }
        Ok(())
    }

    /// Build `model`'s client from its endpoint, replacing the one it had. Transcription
    /// streams and speech requests build theirs per stream and per request: nothing to hold.
    async fn init_model(&self, model: &Model) -> Result<()> {
        self.drop_clients(&model.id).await;
        let endpoint = Ad4mDb::with_global_instance(|db| db.get_endpoint(&model.endpoint_id))?
            .ok_or_else(|| anyhow!("The endpoint of model {:?} is gone", model.name))?;
        match model.model_type {
            ModelType::Llm => {
                let chat = ChatModel::new(model, endpoint)?;
                self.llms.lock().await.insert(model.id.clone(), chat);
            }
            ModelType::Embedding => {
                let embedder = Embedder {
                    client: Arc::from(providers::embed::embed_client(&endpoint)?),
                    remote_model: model.remote_model.clone(),
                    endpoint: Arc::new(endpoint),
                };
                self.embedders
                    .lock()
                    .await
                    .insert(model.id.clone(), embedder);
            }
            ModelType::Transcription | ModelType::Speech => {}
        }
        Ok(())
    }

    /// Forget the clients of model `model_id`, whatever its type was.
    async fn drop_clients(&self, model_id: &str) {
        self.llms.lock().await.remove(model_id);
        self.embedders.lock().await.remove(model_id);
    }

    /// Add a model and build its client. A model whose client does not build (an embedding
    /// model on an endpoint that serves none) is not kept.
    pub async fn add_model(&self, input: ModelInput) -> Result<String> {
        let model = Ad4mDb::with_global_instance(|db| {
            let id = db.add_model(&input)?;
            db.get_model(id)
        })?
        .ok_or_else(|| anyhow!("The model just added is gone"))?;
        if let Err(e) = self.init_model(&model).await {
            Ad4mDb::with_global_instance(|db| db.remove_model(&model.id))?;
            return Err(crate::db::InvalidModel(format!("{e:#}")).into());
        }
        Ok(model.id)
    }

    pub async fn set_default_model(&self, model_type: ModelType, model_id: String) -> Result<()> {
        // Tasks on "default" resolve it when they are prompted: nothing to move.
        Ad4mDb::with_global_instance(|db| db.set_default_model(model_type, &model_id))
    }

    /// Every model, as RPC results show it.
    pub fn model_views() -> Result<Vec<crate::types::ModelView>> {
        Ad4mDb::with_global_instance(|db| {
            let endpoints: HashMap<String, Endpoint> = db
                .get_endpoints()?
                .into_iter()
                .map(|e| (e.id.clone(), e))
                .collect();
            Ok(db
                .get_models()?
                .iter()
                .map(|m| m.view(endpoints.get(&m.endpoint_id)))
                .collect())
        })
    }

    /// Model `model_id`, as RPC results show it.
    pub fn model_view(model_id: &str) -> Result<Option<crate::types::ModelView>> {
        Ad4mDb::with_global_instance(|db| {
            let Some(model) = db.get_model(model_id.to_string())? else {
                return Ok(None);
            };
            let endpoint = db.get_endpoint(&model.endpoint_id)?;
            Ok(Some(model.view(endpoint.as_ref())))
        })
    }

    pub async fn init_global_instance() -> Result<()> {
        let new_service = AIService::new()?;
        let mut ai_service = AI_SERVICE.lock().await;
        *ai_service = Some(new_service);
        Ok(())
    }

    pub async fn global_instance() -> Result<AIService> {
        AI_SERVICE
            .lock()
            .await
            .clone()
            .ok_or(anyhow!(AIServiceError::ServiceNotInitialized))
    }

    // -------------------------------------
    // Tasks
    // -------------------------------------

    pub fn get_tasks() -> Result<Vec<AITask>> {
        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(tasks)
    }

    /// `agent_did` is the owning managed agent's DID; `None` makes a task of the main agent.
    /// Refused unless the task's model is a loaded LLM.
    pub async fn add_task(&self, task: AITaskInput, agent_did: Option<&str>) -> Result<AITask> {
        self.chat_model(&Self::replace_model_variables(&task.model_id)?)
            .await?;
        let task_id = Ad4mDb::with_global_instance(|db| {
            db.add_task(
                task.name.clone(),
                task.model_id.clone(),
                task.system_prompt.clone(),
                task.prompt_examples
                    .iter()
                    .map(|p| p.clone().into())
                    .collect(),
                task.meta_data.clone(),
                agent_did,
            )
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        Ok(Ad4mDb::with_global_instance(|db| db.get_task(task_id))?
            .expect("to get task that we just created"))
    }

    fn replace_model_variables(model_id: &String) -> Result<String> {
        Ok(if model_id == "default" {
            Ad4mDb::with_global_instance(|db| db.get_default_model(ModelType::Llm))?
                .ok_or_else(|| anyhow::anyhow!("Task needs default model but no default set"))?
        } else {
            model_id.clone()
        })
    }

    /// The client of the LLM `model_id` (an id, after `default` was resolved).
    async fn chat_model(&self, model_id: &str) -> Result<ChatModel> {
        self.llms
            .lock()
            .await
            .get(model_id)
            .cloned()
            .ok_or_else(|| anyhow!("No LLM with id '{model_id}' is loaded"))
    }

    /// Rewrites a task. Refused unless its model is a loaded LLM.
    pub async fn update_task(&self, task: AITask) -> Result<AITask> {
        self.chat_model(&Self::replace_model_variables(&task.model_id)?)
            .await?;
        let task_id = task.task_id.clone();
        Ad4mDb::with_global_instance(|db| {
            db.update_task(
                task.task_id,
                task.name,
                task.model_id,
                task.system_prompt,
                task.prompt_examples,
                task.meta_data,
            )
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        Ad4mDb::with_global_instance(|db| db.get_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or_else(|| AIServiceError::TaskNotFound.into())
    }

    pub async fn delete_task(&self, task_id: String) -> Result<bool> {
        Ad4mDb::with_global_instance(|db| db.remove_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(true)
    }

    /// The turns of an OpenAI-style `(role, content)` message list, and the final user
    /// message among them (the prompt, for logging and the estimate).
    ///
    /// Leading system messages join into one system turn; user/assistant pairs become the
    /// examples; the last user message is the prompt. Tool and other roles are dropped:
    /// the caller folded them into text before.
    fn turns_for_messages(messages: Vec<(String, String)>) -> (Vec<ChatTurn>, String) {
        let mut system_prompt = String::new();
        let mut examples: Vec<crate::types::AIPromptExamples> = Vec::new();
        let mut current_user: Option<String> = None;

        for (role, content) in messages {
            match role.as_str() {
                "system" => {
                    if !system_prompt.is_empty() {
                        system_prompt.push('\n');
                    }
                    system_prompt.push_str(&content);
                }
                "user" => {
                    current_user = Some(match current_user.take() {
                        Some(previous) => format!("{previous}\n{content}"),
                        None => content,
                    });
                }
                "assistant" => {
                    if let Some(input) = current_user.take() {
                        examples.push(crate::types::AIPromptExamples {
                            input,
                            output: content,
                        });
                    }
                }
                _ => {}
            }
        }
        let prompt = current_user.unwrap_or_default();
        (turns_for(&system_prompt, &examples, prompt.clone()), prompt)
    }

    /// Run a chat-style prompt on `model_id` with a stateless message list and return the
    /// full response text and token counts. Backs the `/v1/chat/completions` paths the
    /// executor answers itself (Anthropic, the text tool fallback) and the harness.
    pub async fn prompt_messages(
        &self,
        model_id: String,
        messages: Vec<(String, String)>,
        bill_to: Option<String>,
    ) -> Result<PromptResult> {
        let resolved = Self::replace_model_variables(&model_id)?;
        let chat = self.chat_model(&resolved).await?;
        let (turns, final_prompt) = Self::turns_for_messages(messages);

        let prompt_tokens = estimate_token_count(&final_prompt);
        log::debug!(
            "🤖 prompt_messages input model={} text={:?}",
            resolved,
            truncate_for_log(&final_prompt)
        );
        let text = chat.chat(turns, Vec::new()).await?.text;
        let completion_tokens = estimate_token_count(&text);
        log::debug!(
            "🤖 prompt_messages output model={} text={:?}",
            resolved,
            truncate_for_log(&text)
        );

        // Rate = 0 (unpriced model) makes this a no-op. Insufficient credits are checked
        // before the call (the /v1 handler answers 429); here they only log.
        Self::bill_prompt_if_authed(
            bill_to.as_deref(),
            &resolved,
            prompt_tokens,
            completion_tokens,
        );

        Ok(PromptResult {
            text,
            prompt_tokens,
            completion_tokens,
            model_id: resolved,
        })
    }

    /// Whether this model takes tools as definitions rather than as text rendered into the
    /// prompt: its endpoint's protocol can, and its `native_tools` flag says it should.
    /// False for a model that does not exist.
    pub fn model_supports_native_tools(model_id: &str) -> bool {
        let Ok(resolved) = Self::replace_model_variables(&model_id.to_string()) else {
            return false;
        };
        Ad4mDb::with_global_instance(|db| -> Result<bool> {
            let Some(model) = db.get_model(resolved)? else {
                return Ok(false);
            };
            Ok(db
                .get_endpoint(&model.endpoint_id)?
                .is_some_and(|endpoint| model.uses_native_tools(&endpoint)))
        })
        .unwrap_or(false)
    }

    /// Prompt a model with a complete turn list and tool definitions, getting
    /// back whatever text it wrote and any calls it wants dispatched.
    ///
    /// The counterpart of [`Self::prompt_messages`] for models that take tools as
    /// definitions. Gate it on [`Self::model_supports_native_tools`]: a client that does
    /// not carry tools refuses rather than silently drops them.
    pub async fn prompt_with_tools(
        &self,
        model_id: String,
        turns: Vec<ChatTurn>,
        tools: Vec<ToolSpec>,
        bill_to: Option<String>,
    ) -> Result<ChatReply> {
        let resolved = Self::replace_model_variables(&model_id)?;
        let chat = self.chat_model(&resolved).await?;

        // Estimated before the turns are handed over, because everything below
        // goes on the wire and the caller is charged for all of it. Billing
        // runs on this number: `ChatReply.usage` reports the provider's exact
        // counts but does not replace the estimate.
        let prompt_tokens: usize = turns
            .iter()
            .map(estimate_turn_tokens)
            .chain(tools.iter().map(|tool| {
                estimate_token_count(&tool.name)
                    + estimate_token_count(&tool.description)
                    + estimate_token_count(&tool.parameters.to_string())
            }))
            .sum();

        let reply = chat.chat(turns, tools).await?;
        let completion_tokens = estimate_reply_tokens(&reply);

        log::debug!(
            "🤖 prompt_with_tools model={} text={:?} calls={}",
            resolved,
            truncate_for_log(&reply.text),
            reply.tool_calls.len()
        );

        Self::bill_prompt_if_authed(
            bill_to.as_deref(),
            &resolved,
            prompt_tokens,
            completion_tokens,
        );

        Ok(reply)
    }

    /// Streaming variant of [`Self::prompt_messages`]. Returns the token stream, and a
    /// oneshot for the final [`PromptResult`] (the full text and token counts, for the
    /// closing SSE event). Billed when the stream completes, on success only.
    pub async fn prompt_messages_stream(
        &self,
        model_id: String,
        messages: Vec<(String, String)>,
        bill_to: Option<String>,
    ) -> Result<(
        mpsc::UnboundedReceiver<String>,
        oneshot::Receiver<Result<PromptResult>>,
    )> {
        let resolved = Self::replace_model_variables(&model_id)?;
        let chat = self.chat_model(&resolved).await?;
        let (turns, final_prompt) = Self::turns_for_messages(messages);

        let (token_tx, token_rx) = mpsc::unbounded_channel::<String>();
        let (done_tx, done_rx) = oneshot::channel::<Result<PromptResult>>();
        tokio::spawn(async move {
            // Whether this really streams is the provider's business: one that can pushes
            // text as the model writes it, one that cannot answers in a single chunk.
            let result = chat
                .chat_stream(turns, token_tx)
                .await
                .map(|reply| PromptResult {
                    prompt_tokens: estimate_token_count(&final_prompt),
                    completion_tokens: estimate_token_count(&reply.text),
                    text: reply.text,
                    model_id: resolved.clone(),
                });
            Self::bill_and_forward_stream_result(bill_to, &resolved, result, done_tx).await;
        });

        Ok((token_rx, done_rx))
    }

    pub async fn prompt(
        &self,
        task_id: String,
        prompt: String,
        bill_to: Option<String>,
    ) -> Result<PromptResult> {
        let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id.clone()))
            .map_err(|e| anyhow::anyhow!("Database error: {}", e))?
            .ok_or_else(|| anyhow::anyhow!("Task not found for task_id: {}", task_id))?;

        let model_id = Self::replace_model_variables(&task.model_id)?;

        let prompt_tokens = estimate_token_count(&prompt);
        let prompt_chars = prompt.chars().count();
        log::info!(
            "🤖 prompt start model={} chars={} tokens_in={}",
            model_id,
            prompt_chars,
            prompt_tokens
        );
        // LOGGING.md policy: "prompt/response bodies (truncated)" belong at
        // debug. The start/done info lines above only carry char/token
        // counts — this is the only place the actual text is ever logged
        // (LlmRequestSent/LlmResponseReceived cover the interpretation
        // engine's own event bus, but that's gated on emit_ctx and never
        // reaches the `log` crate, so it doesn't show up under RUST_LOG=debug).
        log::debug!(
            "🤖 prompt input model={} text={:?}",
            model_id,
            truncate_for_log(&prompt)
        );
        let started = std::time::Instant::now();

        // Symmetry rule (see rust-executor/LOGGING.md): every `start` info
        // line gets exactly one companion — either `done` on success or a
        // `failed` line on error.
        let text_result: Result<String> = async {
            let chat = self.chat_model(&model_id).await?;
            Ok(chat
                .chat(turns_for_task(&task, prompt), Vec::new())
                .await?
                .text)
        }
        .await;

        let text = match text_result {
            Ok(t) => t,
            Err(e) => {
                log::error!(
                    "❌ 🤖 prompt failed model={} latency={}ms tokens_in={} err={}",
                    model_id,
                    started.elapsed().as_millis(),
                    prompt_tokens,
                    e
                );
                return Err(e);
            }
        };
        let completion_tokens = estimate_token_count(&text);
        log::info!(
            "✅ 🤖 prompt done model={} latency={}ms tokens_in={} tokens_out={}",
            model_id,
            started.elapsed().as_millis(),
            prompt_tokens,
            completion_tokens
        );
        log::debug!(
            "🤖 prompt output model={} text={:?}",
            model_id,
            truncate_for_log(&text)
        );

        // Bill via the shared host_rates helper. See prompt_messages.
        Self::bill_prompt_if_authed(
            bill_to.as_deref(),
            &model_id,
            prompt_tokens,
            completion_tokens,
        );

        Ok(PromptResult {
            text,
            prompt_tokens,
            completion_tokens,
            model_id,
        })
    }

    /// Bill a completed stream result (on success only, matching
    /// [`Self::prompt_messages`]) and relay it to the caller's oneshot.
    ///
    /// Runs inside the forwarder spawned by
    /// [`Self::prompt_messages_stream`]; exposed as an associated function
    /// so the streaming billing behaviour is unit-testable without a live
    /// LLM channel.
    pub(crate) async fn bill_and_forward_stream_result(
        bill_to: Option<String>,
        model_id: &str,
        result: Result<PromptResult>,
        done_tx: oneshot::Sender<Result<PromptResult>>,
    ) {
        if let Ok(pr) = &result {
            Self::bill_prompt_if_authed(
                bill_to.as_deref(),
                model_id,
                pr.prompt_tokens,
                pr.completion_tokens,
            );
        }
        let _ = done_tx.send(result);
    }

    /// Shared post-compute billing hook for prompt paths.
    ///
    /// unit = (prompt_tokens + completion_tokens), matching the token-
    /// length estimate that estimate_token_count() computes (chars/4).
    /// This mirrors what the transcription worker does with word_count,
    /// so every AI billing path routes through the same shape.
    ///
    /// `bill_to` is the managed agent the request acts as, by DID; the main agent pays
    /// nothing.
    fn bill_prompt_if_authed(
        bill_to: Option<&str>,
        model_id: &str,
        prompt_tokens: usize,
        completion_tokens: usize,
    ) {
        let Some(agent_did) = bill_to else {
            return;
        };
        let total_tokens = prompt_tokens.saturating_add(completion_tokens);
        // Ignore result: rate=0 no-ops, InsufficientCredits is logged
        // inside bill_ai_operation. Prompt has already run so we don't
        // want to fail the caller on a bookkeeping-only issue.
        let _ = crate::billing::bill_ai_operation(
            agent_did,
            model_id,
            "ai_prompt",
            total_tokens,
            "tokens",
        );
    }

    /// Shared post-compute billing hook for embed paths.
    fn bill_embed_if_authed(bill_to: Option<&str>, model_id: &str, token_count: usize) {
        let Some(agent_did) = bill_to else {
            return;
        };
        let _ = crate::billing::bill_ai_operation(
            agent_did,
            model_id,
            "ai_embedding",
            token_count,
            "tokens",
        );
    }

    // -------------------------------------
    // Embedding
    // -------------------------------------

    /// Embed one text. See [`Self::embed_batch`].
    pub async fn embed(
        &self,
        model: String,
        text: String,
        bill_to: Option<String>,
    ) -> Result<EmbedResult> {
        let batch = self.embed_batch(&model, vec![text], bill_to).await?;
        Ok(EmbedResult {
            embeddings: batch.vectors.into_iter().next().unwrap_or_default(),
            token_count: batch.token_count,
        })
    }

    /// The id of the embedding model `requested` names: a model's id, else the name of an
    /// embedding model.
    fn embedding_model_id(requested: &str) -> Result<String> {
        let models = Ad4mDb::with_global_instance(|db| db.get_models())?;
        models
            .iter()
            .find(|m| m.id == requested)
            .or_else(|| {
                models
                    .iter()
                    .find(|m| m.name == requested && m.model_type == ModelType::Embedding)
            })
            .map(|model| model.id.clone())
            .ok_or_else(|| {
                anyhow!("No EMBEDDING model has the id or name '{requested}' on this executor")
            })
    }

    /// Embed `texts` with the embedding model `model` (an id or a name), in one request to
    /// its endpoint, and bill `bill_to` for the input tokens.
    pub async fn embed_batch(
        &self,
        model: &str,
        texts: Vec<String>,
        bill_to: Option<String>,
    ) -> Result<EmbedBatch> {
        let model_id = Self::embedding_model_id(model)?;
        let estimate: usize = texts.iter().map(|t| estimate_token_count(t)).sum();
        log::debug!(
            "🤖 embed start model={} n={} tokens_in={}",
            model_id,
            texts.len(),
            estimate
        );
        let started = std::time::Instant::now();
        let embedder = self
            .embedders
            .lock()
            .await
            .get(&model_id)
            .cloned()
            .ok_or_else(|| anyhow!("The embedding model '{model}' is not loaded"))?;
        let count = texts.len();
        let timeout =
            endpoints::request_timeout(&embedder.endpoint, providers::embed::EMBED_TIMEOUT);
        let call = async {
            let _slot = endpoints::request_slot(&embedder.endpoint).await;
            embedder.client.embed(&embedder.remote_model, &texts).await
        };
        let reply = tokio::time::timeout(timeout, call).await.map_err(|_| {
            providers::http::UpstreamUnreachable {
                url: embedder.endpoint.base_url.to_string(),
                timed_out: true,
                detail: format!("no answer within {}s", timeout.as_secs()),
            }
        })??;
        let token_count = reply.prompt_tokens.map_or(estimate, |n| n as usize);
        let vectors = reply.vectors;
        log::debug!(
            "✅ 🤖 embed done model={} n={} latency={}ms tokens_in={}",
            model_id,
            count,
            started.elapsed().as_millis(),
            token_count
        );

        Self::bill_embed_if_authed(bill_to.as_deref(), &model_id, token_count);

        Ok(EmbedBatch {
            vectors,
            token_count,
        })
    }

    // -------------------------------------
    // Transcription
    // -------------------------------------

    /// Open a stream on the transcription model `model_id`: each utterance fed goes to its
    /// endpoint as one request.
    pub async fn open_transcription_stream(
        &self,
        model_id: String,
        caller: &RequestContext,
    ) -> Result<String> {
        let (client, remote_model) = transcription::endpoint_transcriber(&model_id)?;
        let (agent_did, user_did) = transcription_stream_owner(caller.acts_as())?;
        Ok(self
            .open_remote_stream(
                model_id,
                remote_model,
                Arc::new(client),
                agent_did,
                user_did,
            )
            .await)
    }

    /// Open a stream whose utterances `client` transcribes with the upstream model
    /// `remote_model`, billed as `model_id` to `agent_did`, its text sent to `user_did`.
    async fn open_remote_stream(
        &self,
        model_id: String,
        remote_model: String,
        client: Arc<dyn providers::audio::RemoteTranscribe>,
        agent_did: Option<String>,
        user_did: String,
    ) -> String {
        let stream_id = uuid::Uuid::new_v4().to_string();
        let (utterances, queue) = mpsc::unbounded_channel();
        let (text_broadcast_tx, _) = tokio::sync::broadcast::channel::<String>(64);
        log::info!("Opening transcription stream {stream_id} on model {model_id} ({remote_model})");
        let worker = transcription::RemoteStream {
            stream_id: stream_id.clone(),
            model_id,
            remote_model,
            client,
            agent_did: agent_did.clone(),
            user_did,
            listeners: text_broadcast_tx.clone(),
        };
        tokio::spawn(worker.run(queue));

        self.transcription_streams.lock().await.insert(
            stream_id.clone(),
            TranscriptionSession {
                utterances,
                last_activity: Arc::new(Mutex::new(std::time::Instant::now())),
                text_broadcast_tx,
                agent_did,
            },
        );
        stream_id
    }

    /// Feed one utterance (16 kHz mono) to a stream.
    ///
    /// Answers once the utterance is transcribed and its text published, after every
    /// utterance fed before it; an upstream failure is the error, and nothing is published.
    pub async fn feed_transcription_stream(
        &self,
        stream_id: &String,
        audio_samples: Vec<f32>,
        caller: &RequestContext,
    ) -> Result<()> {
        let outcome = {
            let map_lock = self.transcription_streams.lock().await;
            let stream = map_lock
                .get(stream_id)
                .ok_or(AIServiceError::StreamNotFound)?;
            Self::verify_stream_ownership(stream, caller)?;
            *stream.last_activity.lock().await = std::time::Instant::now();
            // Queued while the map is locked: two feeds keep the order they arrived in.
            let (done, outcome) = oneshot::channel();
            stream
                .utterances
                .send(transcription::Utterance {
                    samples: audio_samples,
                    done,
                })
                .map_err(|e| {
                    AIServiceError::CrazyError(format!("Failed to feed stream {stream_id}: {e}"))
                })?;
            outcome
        };
        outcome
            .await
            .map_err(|_| anyhow!("Transcription stream {stream_id} ended before it answered"))?
    }

    /// The text of a stream, as it is published (`/v1/realtime`).
    pub async fn subscribe_transcription_text(
        &self,
        stream_id: &String,
        caller: &RequestContext,
    ) -> Result<tokio::sync::broadcast::Receiver<String>> {
        let map_lock = self.transcription_streams.lock().await;
        let stream = map_lock
            .get(stream_id)
            .ok_or(AIServiceError::StreamNotFound)?;
        Self::verify_stream_ownership(stream, caller)?;
        Ok(stream.text_broadcast_tx.subscribe())
    }

    /// Close a stream. Utterances already fed to a stream on an endpoint are still answered.
    pub async fn close_transcription_stream(
        &self,
        stream_id: &String,
        caller: &RequestContext,
    ) -> Result<()> {
        let mut map_lock = self.transcription_streams.lock().await;
        let stream = map_lock
            .get(stream_id)
            .ok_or(AIServiceError::StreamNotFound)?;
        Self::verify_stream_ownership(stream, caller)?;
        map_lock.remove(stream_id);
        Ok(())
    }

    /// Verify that the caller (the agent its request acts as) owns the given transcription
    /// session. In single-user mode, ownership is not enforced. In multi-user mode the rule
    /// of every owned resource (`api::guards::owns`): the operator reaches every stream; any
    /// other authenticated session only the streams of the agent it acts as (a grant on the
    /// main agent only the main agent's, owner `None`; a user only their own); an anonymous
    /// caller none.
    fn verify_stream_ownership(
        session: &TranscriptionSession,
        caller: &RequestContext,
    ) -> Result<()> {
        let multi_user_enabled = crate::db::Ad4mDb::with_global_instance(|db| {
            db.get_multi_user_enabled().unwrap_or(false)
        });

        if !multi_user_enabled {
            // Single-user mode — no ownership enforcement
            return Ok(());
        }

        let agent = caller.acts_as();
        let owned = caller.session.is_authenticated()
            && session.agent_did.as_deref() == agent.managed_did();
        if caller.is_operator() || owned {
            Ok(())
        } else {
            Err(anyhow!(
                "Authorization error: caller does not own this transcription stream"
            ))
        }
    }

    /// Rewrite model `model_id` and build its client again. A refused rewrite (an endpoint
    /// that does not exist) changes nothing; a client that does not build is the error,
    /// with the row already rewritten.
    pub async fn update_model(&self, model_id: String, model_config: ModelInput) -> Result<()> {
        log::info!("Updating model: {} with: {:?}", model_id, model_config);
        Ad4mDb::with_global_instance(|db| db.get_model(model_id.clone()))?
            .ok_or_else(|| anyhow!("Model not found: {}", model_id))?;
        // `context`, not a new error, so the API can still tell an `InvalidModel`.
        Ad4mDb::with_global_instance(|db| db.update_model(&model_id, &model_config))
            .context("Failed to update model in database")?;
        self.reload_model(model_id).await
    }

    /// Builds a model's client again from its stored row, after the row changed: an update
    /// of the model, or of the endpoint it runs on.
    pub(crate) async fn reload_model(&self, model_id: String) -> Result<()> {
        let model = Ad4mDb::with_global_instance(|db| db.get_model(model_id.clone()))?
            .ok_or_else(|| anyhow!("Model not found after update: {}", model_id))?;
        self.init_model(&model).await
    }

    pub async fn remove_model(&self, model_id: String) -> Result<()> {
        Ad4mDb::with_global_instance(|db| db.get_model(model_id.clone()))?
            .ok_or_else(|| anyhow!("Model not found: {}", model_id))?;
        self.drop_clients(&model_id).await;
        Ad4mDb::with_global_instance(|db| db.remove_model(&model_id))
            .map_err(|e| anyhow!("Failed to remove model from database: {}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::providers::{ChatTurn, ToolCall};

    fn call(arguments: serde_json::Value) -> ToolCall {
        ToolCall {
            id: "toolu_01Eg163G7WZHsU3H4jik7nNC".to_string(),
            name: "update_schema".to_string(),
            arguments,
        }
    }

    #[test]
    fn a_turn_with_no_tool_metadata_is_estimated_from_its_text() {
        let turn = ChatTurn::user("hello there");
        assert_eq!(
            estimate_turn_tokens(&turn),
            estimate_token_count("hello there")
        );
    }

    #[test]
    fn a_calls_arguments_are_billed_not_just_its_text() {
        // The arguments are the payload in an agentic loop — a schema patch, a
        // query, a document — and they dwarf whatever the model said alongside.
        let patch = serde_json::json!({
            "patches": [{ "targetId": "root", "node": { "type": "Column", "props": {
                "gap": "400", "p": "500", "bg": "surface", "r": "400"
            }}}]
        });

        let plain = ChatTurn::assistant("Applying that now.");
        let calling = ChatTurn::assistant_calling("Applying that now.", vec![call(patch)]);

        assert!(
            estimate_turn_tokens(&calling) > estimate_turn_tokens(&plain) + 20,
            "the call should add far more than its name, got {} vs {}",
            estimate_turn_tokens(&calling),
            estimate_turn_tokens(&plain)
        );
    }

    #[test]
    fn a_tool_result_is_billed_for_the_id_it_answers() {
        let bare = ChatTurn::user("42");
        let answering = ChatTurn::tool_result("toolu_01Eg163G7WZHsU3H4jik7nNC", "42");

        assert!(estimate_turn_tokens(&answering) > estimate_turn_tokens(&bare));
    }

    #[test]
    fn several_calls_in_one_turn_each_count() {
        let one = ChatTurn::assistant_calling("", vec![call(serde_json::json!({ "a": 1 }))]);
        let two = ChatTurn::assistant_calling(
            "",
            vec![
                call(serde_json::json!({ "a": 1 })),
                call(serde_json::json!({ "a": 1 })),
            ],
        );

        assert_eq!(estimate_turn_tokens(&two), estimate_turn_tokens(&one) * 2);
    }

    #[test]
    fn a_replied_call_bills_its_name_and_arguments_but_not_its_id() {
        let reply = |calls| super::ChatReply {
            text: String::new(),
            tool_calls: calls,
            usage: Default::default(),
        };
        let arguments = serde_json::json!({ "a": 1 });
        let expected = super::estimate_token_count("update_schema")
            + super::estimate_token_count(&arguments.to_string());

        assert_eq!(
            super::estimate_reply_tokens(&reply(vec![call(arguments)])),
            expected
        );
    }

    use super::*;
    use crate::types::{AIPromptExamplesInput, EndpointKind};
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// A fresh service on a fresh in-memory database.
    fn service() -> AIService {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        AIService::empty()
    }

    /// A saved endpoint of `kind` at `url`, without a key.
    fn saved_endpoint(kind: EndpointKind, url: &str) -> Endpoint {
        let endpoint = Endpoint::for_test("upstream", kind, url, "");
        Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).unwrap();
        endpoint
    }

    fn model_on(endpoint: &Endpoint, model_type: ModelType, remote: &str) -> ModelInput {
        ModelInput {
            name: format!("{model_type}-model"),
            model_type,
            endpoint_id: endpoint.id.clone(),
            remote_model: remote.into(),
            native_tools: None,
        }
    }

    #[tokio::test]
    async fn a_fresh_executor_loads_no_model() {
        let service = service();
        service.load().await.unwrap();
        assert!(service.llms.lock().await.is_empty());
        assert!(service.embedders.lock().await.is_empty());
        let error = match service.embed("bert".into(), "text".into(), None).await {
            Ok(_) => panic!("no embedding model is configured"),
            Err(error) => error,
        };
        assert!(error.to_string().contains("No EMBEDDING model"), "{error}");
    }

    #[tokio::test]
    async fn a_task_prompts_its_model_on_its_endpoint() {
        let service = service();
        let mut upstream = mockito::Server::new_async().await;
        let completion = upstream
            .mock("POST", "/v1/chat/completions")
            .match_body(mockito::Matcher::PartialJson(serde_json::json!({
                "model": "qwen3:8b",
                "messages": [
                    { "role": "system", "content": "Answer in French." },
                    { "role": "user", "content": "hi" },
                    { "role": "assistant", "content": "salut" },
                    { "role": "user", "content": "thanks" }
                ]
            })))
            .with_body(r#"{"choices":[{"message":{"content":"merci"}}]}"#)
            .create_async()
            .await;
        let endpoint = saved_endpoint(EndpointKind::LlamaServer, &upstream.url());
        let model_id = service
            .add_model(model_on(&endpoint, ModelType::Llm, "qwen3:8b"))
            .await
            .unwrap();
        let task = service
            .add_task(
                AITaskInput {
                    name: "french".into(),
                    model_id,
                    system_prompt: "Answer in French.".into(),
                    prompt_examples: vec![AIPromptExamplesInput {
                        input: "hi".into(),
                        output: "salut".into(),
                    }],
                    meta_data: None,
                },
                None,
            )
            .await
            .unwrap();

        let answer = service
            .prompt(task.task_id, "thanks".into(), None)
            .await
            .unwrap();
        assert_eq!(answer.text, "merci");
        completion.assert_async().await;
    }

    #[tokio::test]
    async fn a_task_on_a_model_that_is_not_loaded_is_refused() {
        let service = service();
        let refused = service
            .add_task(
                AITaskInput {
                    name: "t".into(),
                    model_id: "no-such-model".into(),
                    system_prompt: String::new(),
                    prompt_examples: vec![],
                    meta_data: None,
                },
                None,
            )
            .await;
        assert!(refused.is_err());
        assert!(AIService::get_tasks().unwrap().is_empty());
    }

    #[tokio::test]
    async fn an_embedding_model_on_an_endpoint_without_embeddings_is_not_kept() {
        let service = service();
        let endpoint = saved_endpoint(EndpointKind::Anthropic, "https://api.anthropic.com");
        assert!(service
            .add_model(model_on(&endpoint, ModelType::Embedding, "claude"))
            .await
            .is_err());
        assert!(Ad4mDb::with_global_instance(|db| db.get_models())
            .unwrap()
            .is_empty());
    }

    #[tokio::test]
    async fn a_model_updated_to_another_type_moves_to_that_types_clients() {
        let service = service();
        let endpoint = saved_endpoint(EndpointKind::Ollama, "http://127.0.0.1:11434");
        let id = service
            .add_model(model_on(&endpoint, ModelType::Llm, "qwen3:8b"))
            .await
            .unwrap();
        assert!(service.llms.lock().await.contains_key(&id));

        service
            .update_model(
                id.clone(),
                model_on(&endpoint, ModelType::Embedding, "nomic-embed-text"),
            )
            .await
            .unwrap();
        assert!(!service.llms.lock().await.contains_key(&id));
        let embedder = service.embedders.lock().await.get(&id).cloned().unwrap();
        assert_eq!(embedder.remote_model, "nomic-embed-text");

        service.remove_model(id.clone()).await.unwrap();
        assert!(!service.embedders.lock().await.contains_key(&id));
    }

    #[test]
    fn a_saved_endpoint_with_a_key_over_plain_http_builds_no_client() {
        // The load path: an endpoint saved before the handlers refused this reaches client
        // construction without passing through them.
        let endpoint = Endpoint::for_test(
            "lan",
            EndpointKind::Anthropic,
            "http://192.168.1.10:8080",
            "sk-ant",
        );
        let error = match providers::chat_client(&endpoint) {
            Ok(_) => panic!("a key over plain http to a remote host is refused"),
            Err(error) => error,
        };
        assert!(error.to_string().contains("plain HTTP"), "{error}");
        assert!(
            error.to_string().contains("192.168.1.10"),
            "names the URL: {error}"
        );
    }

    /// A client that counts the calls it is answering at once, and the most it saw.
    #[derive(Default)]
    struct Counting {
        now: AtomicUsize,
        most: AtomicUsize,
    }

    #[async_trait::async_trait]
    impl RemoteChat for Counting {
        async fn chat(&self, _request: ChatRequest) -> Result<ChatReply> {
            let now = self.now.fetch_add(1, Ordering::SeqCst) + 1;
            self.most.fetch_max(now, Ordering::SeqCst);
            sleep(Duration::from_millis(20)).await;
            self.now.fetch_sub(1, Ordering::SeqCst);
            Ok(ChatReply {
                text: "ok".into(),
                ..Default::default()
            })
        }
    }

    // There is no per-model thread to serialise prompts any more: the endpoint's cap is what
    // limits them, on the task and harness paths as on /v1.
    #[tokio::test]
    async fn an_endpoints_concurrency_cap_holds_for_prompts_and_none_is_no_limit() {
        for (cap, expected) in [(Some(1), 1), (None, 4)] {
            let mut endpoint =
                Endpoint::for_test("capped", EndpointKind::Vllm, "http://127.0.0.1:9", "");
            endpoint.max_concurrent = cap;
            let client = Arc::new(Counting::default());
            let chat = ChatModel {
                client: client.clone(),
                remote_model: "m".into(),
                endpoint: Arc::new(endpoint),
            };
            let calls = (0..4).map(|_| chat.chat(vec![ChatTurn::user("hi")], vec![]));
            for reply in futures::future::join_all(calls).await {
                assert_eq!(reply.unwrap().text, "ok");
            }
            assert_eq!(client.most.load(Ordering::SeqCst), expected, "cap {cap:?}");
        }
    }
}

#[cfg(test)]
mod stream_ownership_tests {
    use super::*;
    use crate::agent::session::Principal;
    use crate::test_utils::MultiUserMode;

    fn stream_of(owner: Option<&str>) -> TranscriptionSession {
        TranscriptionSession::for_test(owner).0
    }

    // The operator's admin credential is not a JWT, so the old token decode refused it: in
    // multi-user mode it could open a stream but not feed or close it.
    #[test]
    fn in_multi_user_mode_the_session_decides_who_reaches_a_stream() {
        let _multi_user = MultiUserMode::on();
        let alice = RequestContext::user_for_test("did:key:alice", vec![]);
        let bob = RequestContext::user_for_test("did:key:bob", vec![]);
        let operator = RequestContext::operator_for_test();
        let app = RequestContext::app_for_test(vec![]);
        let anonymous = RequestContext::for_test(Principal::Anonymous);

        let alices = stream_of(Some("did:key:alice"));
        let main_agents = stream_of(None);
        let reaches = |stream, caller| AIService::verify_stream_ownership(stream, caller).is_ok();

        assert!(reaches(&alices, &alice), "a user reaches their own stream");
        assert!(!reaches(&alices, &bob), "not another user's");
        assert!(!reaches(&main_agents, &bob), "nor the main agent's");
        assert!(
            reaches(&alices, &operator),
            "the operator reaches any stream"
        );
        assert!(reaches(&main_agents, &operator));
        assert!(
            reaches(&main_agents, &app),
            "a grant on the main agent reaches the main agent's streams"
        );
        assert!(
            !reaches(&main_agents, &anonymous),
            "an anonymous caller reaches none"
        );
        assert!(!reaches(&alices, &anonymous));
    }

    // An app on the main agent is not the operator: it reaches the main agent's streams, not
    // a user's, whatever id it knows. Feeding (`feed_transcription_stream`, both forms) and
    // closing (`close_transcription_stream`) all check this rule.
    #[test]
    fn a_main_agent_app_cannot_feed_or_close_a_users_stream() {
        let _multi_user = MultiUserMode::on();
        let alice = RequestContext::user_for_test("did:key:alice", vec![]);
        let operator = RequestContext::operator_for_test();
        let app =
            RequestContext::app_for_test(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]);
        let alices = stream_of(Some("did:key:alice"));
        let reaches = |caller| AIService::verify_stream_ownership(&alices, caller).is_ok();

        assert!(
            !reaches(&app),
            "an app on the main agent, even with every capability, reached a user's stream"
        );
        assert!(reaches(&alice), "the user reaches their own stream");
        assert!(reaches(&operator), "the operator reaches every stream");
    }

    // The feed and close paths themselves: a main-agent app is refused on a user's stream and
    // the stream stays open; the user then closes it.
    #[tokio::test]
    async fn feed_and_close_refuse_a_main_agent_app_on_a_users_stream() {
        let _multi_user = MultiUserMode::on();
        // No models and no cleanup task: only the stream map is used.
        let service = AIService::empty();
        let alice = RequestContext::user_for_test("did:key:alice", vec![]);
        let app =
            RequestContext::app_for_test(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]);
        // The stream's queue stays alive, as its worker's would: a feed and a close by its
        // owner reach it.
        let (session, mut queue) = TranscriptionSession::for_test(Some("did:key:alice"));
        let id = "alices-stream".to_string();
        service
            .transcription_streams
            .lock()
            .await
            .insert(id.clone(), session);

        assert!(service
            .feed_transcription_stream(&id, vec![0.0], &app)
            .await
            .is_err());
        assert!(service.close_transcription_stream(&id, &app).await.is_err());
        assert!(service.transcription_streams.lock().await.contains_key(&id));
        assert!(
            queue.try_recv().is_err(),
            "the app's samples reached the user's stream"
        );

        // The worker answers the user's utterance.
        let worker = tokio::spawn(async move {
            let utterance = queue.recv().await.expect("the user's utterance");
            assert_eq!(utterance.samples, vec![0.5]);
            let _ = utterance.done.send(Ok(()));
            queue
        });
        service
            .feed_transcription_stream(&id, vec![0.5], &alice)
            .await
            .expect("the user feeds their own stream");
        let mut queue = worker.await.unwrap();
        service
            .close_transcription_stream(&id, &alice)
            .await
            .expect("the user closes their own stream");
        assert!(!service.transcription_streams.lock().await.contains_key(&id));
        assert!(queue.recv().await.is_none(), "the close reached the stream");
    }
}

#[cfg(test)]
mod remote_stream_tests {
    use super::providers::audio::{AudioFile, RemoteTranscribe, TranscribeReply};
    use super::*;
    use crate::billing::test_seam;
    use crate::events::{Audience, Topic};
    use crate::pubsub::get_global_pubsub;

    /// An upstream that takes a millisecond per sample, answers `<samples> samples`, and
    /// fails an utterance of 13 samples.
    struct Upstream;

    #[async_trait::async_trait]
    impl RemoteTranscribe for Upstream {
        async fn transcribe(
            &self,
            model: &str,
            audio: AudioFile,
            fields: Vec<(String, String)>,
        ) -> Result<TranscribeReply> {
            assert_eq!(model, "remote-model");
            assert!(fields.is_empty());
            let samples = (audio.bytes.len() - 44) / 2;
            sleep(Duration::from_millis(samples as u64)).await;
            if samples == 13 {
                return Err(providers::http::UpstreamStatus {
                    url: "http://upstream/v1/audio/transcriptions".into(),
                    status: reqwest::StatusCode::INTERNAL_SERVER_ERROR,
                    body: "boom".into(),
                    retry_after: None,
                }
                .into());
            }
            let text = format!("{samples} samples");
            Ok(TranscribeReply {
                body: text.clone().into_bytes(),
                text,
                content_type: "text/plain".into(),
            })
        }
    }

    async fn open(service: &AIService, agent_did: Option<&str>) -> String {
        let _ = Ad4mDb::init_global_instance(":memory:");
        service
            .open_remote_stream(
                "m".into(),
                "remote-model".into(),
                Arc::new(Upstream),
                agent_did.map(str::to_string),
                "did:key:owner".into(),
            )
            .await
    }

    #[tokio::test(start_paused = true)]
    async fn utterances_are_answered_in_the_order_they_were_fed() {
        let service = AIService::empty();
        let id = open(&service, None).await;
        let operator = RequestContext::operator_for_test();
        let mut text = service
            .subscribe_transcription_text(&id, &operator)
            .await
            .unwrap();

        // The first utterance takes longer upstream than the second.
        let (slow, fast) = tokio::join!(
            service.feed_transcription_stream(&id, vec![0.1; 300], &operator),
            service.feed_transcription_stream(&id, vec![0.1; 10], &operator),
        );
        slow.unwrap();
        fast.unwrap();
        assert_eq!(text.recv().await.unwrap(), "300 samples");
        assert_eq!(text.recv().await.unwrap(), "10 samples");
    }

    #[tokio::test(start_paused = true)]
    async fn the_text_goes_to_the_owner_only() {
        let service = AIService::empty();
        let id = open(&service, None).await;
        let operator = RequestContext::operator_for_test();
        let mut events = get_global_pubsub().await.subscribe_events();

        service
            .feed_transcription_stream(&id, vec![0.1; 5], &operator)
            .await
            .unwrap();

        let event = loop {
            let event = events.recv().await.unwrap();
            if event.topic == Topic::AiTranscriptionText {
                break event;
            }
        };
        assert_eq!(
            event.audience,
            Audience::Dids(vec!["did:key:owner".to_string()])
        );
        assert_eq!(event.payload["streamId"], id.as_str());
        assert_eq!(event.payload["text"], "5 samples");
    }

    #[tokio::test(start_paused = true)]
    async fn an_upstream_failure_is_the_feeds_error_and_publishes_nothing() {
        let service = AIService::empty();
        let id = open(&service, None).await;
        let operator = RequestContext::operator_for_test();
        let mut text = service
            .subscribe_transcription_text(&id, &operator)
            .await
            .unwrap();

        let error = service
            .feed_transcription_stream(&id, vec![0.1; 13], &operator)
            .await
            .unwrap_err();
        assert!(error.to_string().contains("failed (500"), "{error}");
        assert!(!error.to_string().contains("http://upstream"), "{error}");
        assert!(
            text.try_recv().is_err(),
            "a failed utterance published text"
        );

        // The stream carries on.
        service
            .feed_transcription_stream(&id, vec![0.1; 4], &operator)
            .await
            .unwrap();
        assert_eq!(text.recv().await.unwrap(), "4 samples");
    }

    #[tokio::test(start_paused = true)]
    async fn a_users_utterance_is_billed_once_and_a_closed_stream_takes_no_more() {
        let service = AIService::empty();
        let id = open(&service, Some("did:key:payer")).await;
        Ad4mDb::with_global_instance(|db| db.set_host_rates(&[("m".to_string(), 0.5)])).unwrap();
        test_seam::reset();
        test_seam::force_result(test_seam::ForcedResult::Success);
        let operator = RequestContext::operator_for_test();

        service
            .feed_transcription_stream(&id, vec![0.1; 7], &operator)
            .await
            .unwrap();

        let calls = test_seam::calls();
        assert_eq!(calls.len(), 1, "{calls:?}");
        assert_eq!(calls[0].agent_did, "did:key:payer");
        assert_eq!(calls[0].operation, concat!("ai_", "transcription"));
        assert_eq!(calls[0].amount, 1.0, "2 words at 0.5");
        test_seam::reset();

        service
            .close_transcription_stream(&id, &operator)
            .await
            .unwrap();
        assert!(service
            .feed_transcription_stream(&id, vec![0.1; 7], &operator)
            .await
            .is_err());
    }
}

#[cfg(test)]
mod stream_reaper_tests {
    use super::*;

    // D4: `global_instance()` hands out clones, and the clone `new()` gave the model load was
    // dropped moments after start. Dropping any clone used to stop the reaper, so an abandoned
    // stream lived until the process ended.
    #[tokio::test(start_paused = true)]
    async fn a_dropped_clone_does_not_stop_the_reaper() {
        let service = AIService::empty();
        service.start_stream_reaper();
        drop(service.clone());

        let (session, _queue) = TranscriptionSession::for_test(None);
        *session.last_activity.lock().await =
            std::time::Instant::now() - Duration::from_secs(TRANSCRIPTION_TIMEOUT_SECS + 1);
        service
            .transcription_streams
            .lock()
            .await
            .insert("abandoned".to_string(), session);

        sleep(Duration::from_secs(TRANSCRIPTION_CHECK_INTERVAL_SECS + 1)).await;
        assert!(
            service.transcription_streams.lock().await.is_empty(),
            "the reaper stopped when a clone of the service was dropped"
        );
    }
}

#[cfg(test)]
mod transcription_owner_tests {
    use super::transcription_stream_owner;
    use crate::agent::AgentContext;
    use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};

    // A stream whose owner did not resolve used to open with a null owner DID, and the event
    // filter delivers such text to every session.
    #[test]
    fn a_user_without_a_key_opens_no_stream() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let owner = AgentContext::managed("did:key:z6MkNoKeyStreamOwner".to_string());
        assert!(transcription_stream_owner(owner).is_err());
    }

    #[test]
    fn a_user_owns_their_stream_and_the_operator_owns_as_the_main_agent() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let user_did = crate::test_utils::create_test_user("stream.owner@example.org");
        let (agent_did, did) =
            transcription_stream_owner(AgentContext::managed(user_did.clone())).unwrap();
        assert_eq!(agent_did.as_deref(), Some(user_did.as_str()));
        assert_eq!(did, user_did);

        let (admin_agent, admin_did) =
            transcription_stream_owner(AgentContext::main_agent()).unwrap();
        assert!(admin_agent.is_none());
        assert_eq!(admin_did, crate::agent::did());
    }

    // An expired token opens no session, so the caller is refused before a transcription
    // handler runs and before a stream has an owner.
    #[test]
    fn a_caller_whose_token_expired_is_refused() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let token = crate::test_utils::expired_user_token("expired@example.org");
        let refused = crate::types::RequestContext::open(
            &token,
            &Some("the-admin-credential".into()),
            crate::agent::capabilities::ListenerReach::Network,
        );
        assert_eq!(
            refused.err(),
            Some(crate::agent::session::AuthError::Expired)
        );
    }
}
