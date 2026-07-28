//! OpenAI request/response schemas.
//!
//! All field names are spelled exactly as the OpenAI HTTP API expects —
//! that's the whole point of this module.  Internal AD4M-specific names
//! go behind serde aliases or under the `ad4m` extension namespace.

use serde::{Deserialize, Serialize};
use serde_json::Value;

// ---------------------------------------------------------------------------
// Common
// ---------------------------------------------------------------------------

/// Standard OpenAI usage object.  All token counts are best-effort: the
/// local Kalosm backend reports `chars/4` estimates today; remote upstreams
/// return exact counts which we forward when available.
#[derive(Debug, Clone, Serialize)]
pub struct Usage {
    pub prompt_tokens: u64,
    pub completion_tokens: u64,
    pub total_tokens: u64,
}

// ---------------------------------------------------------------------------
// /v1/models
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct ModelListResponse {
    pub object: &'static str, // "list"
    pub data: Vec<ModelInfo>,
}

#[derive(Debug, Serialize)]
pub struct ModelInfo {
    pub id: String,
    pub object: &'static str,   // "model"
    pub created: i64,           // unix epoch seconds (we report 0 for static models)
    pub owned_by: &'static str, // "ad4m"
    /// AD4M-specific extension fields.  Tools that don't know about them
    /// ignore them; the proposal's spec carries `ad4m.model_type` here.
    #[serde(rename = "ad4m")]
    pub extensions: ModelExtensions,
}

#[derive(Debug, Serialize)]
pub struct ModelExtensions {
    pub model_type: String, // "llm" | "embedding" | "transcription" | "tts"
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backend: Option<String>, // "local" | "remote" | "passthrough"
}

// ---------------------------------------------------------------------------
// /v1/chat/completions
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Role {
    System,
    User,
    Assistant,
    /// We don't process tool/function messages today — accepted but
    /// ignored in the prompt assembly so the request doesn't reject.
    Tool,
    Function,
    Developer,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ChatMessage {
    pub role: Role,
    #[serde(default)]
    pub content: Option<ChatMessageContent>,
    #[serde(default)]
    pub name: Option<String>,
}

/// OpenAI accepts either a string or an array of content parts.  For now we
/// flatten parts to their text and ignore image/audio inputs — those are
/// out of scope for the first pass.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum ChatMessageContent {
    Text(String),
    Parts(Vec<ContentPart>),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ContentPart {
    Text {
        text: String,
    },
    /// Image input — silently dropped by the prompt assembler today.  We
    /// keep parsing so clients sending mixed content don't 400.
    ImageUrl {
        image_url: Value,
    },
    /// Same — accepted, not consumed.
    InputAudio {
        input_audio: Value,
    },
}

impl ChatMessageContent {
    pub fn flatten_to_text(&self) -> String {
        match self {
            ChatMessageContent::Text(s) => s.clone(),
            ChatMessageContent::Parts(parts) => parts
                .iter()
                .filter_map(|p| match p {
                    ContentPart::Text { text } => Some(text.as_str()),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct ChatCompletionRequest {
    pub model: String,
    pub messages: Vec<ChatMessage>,
    #[serde(default)]
    pub stream: bool,
    #[serde(default)]
    pub temperature: Option<f32>,
    #[serde(default)]
    pub max_tokens: Option<u32>,
    #[serde(default)]
    pub top_p: Option<f32>,
    #[serde(default)]
    pub stop: Option<Value>,
    #[serde(default)]
    pub seed: Option<i64>,
    #[serde(default)]
    pub response_format: Option<Value>,
    #[serde(default)]
    pub user: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ChatCompletionResponse {
    pub id: String,
    pub object: &'static str, // "chat.completion"
    pub created: i64,
    pub model: String,
    pub choices: Vec<ChatChoice>,
    pub usage: Usage,
}

#[derive(Debug, Serialize)]
pub struct ChatChoice {
    pub index: u32,
    pub message: ChatResponseMessage,
    pub finish_reason: &'static str, // "stop" | "length" | "tool_calls"
}

#[derive(Debug, Serialize)]
pub struct ChatResponseMessage {
    pub role: &'static str, // "assistant"
    pub content: String,
}

// ---------------------------------------------------------------------------
// Streaming chunks (SSE)
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct ChatCompletionChunk {
    pub id: String,
    pub object: &'static str, // "chat.completion.chunk"
    pub created: i64,
    pub model: String,
    pub choices: Vec<ChatChunkChoice>,
}

#[derive(Debug, Serialize)]
pub struct ChatChunkChoice {
    pub index: u32,
    pub delta: ChatChunkDelta,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<&'static str>,
}

#[derive(Debug, Default, Serialize)]
pub struct ChatChunkDelta {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub role: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
}

// ---------------------------------------------------------------------------
// /v1/completions  (legacy)
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
pub struct CompletionRequest {
    pub model: String,
    pub prompt: PromptInput,
    #[serde(default)]
    pub stream: bool,
    #[serde(default)]
    pub temperature: Option<f32>,
    #[serde(default)]
    pub max_tokens: Option<u32>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum PromptInput {
    One(String),
    Many(Vec<String>),
}

impl PromptInput {
    pub fn into_single(self) -> Result<String, &'static str> {
        match self {
            PromptInput::One(s) => Ok(s),
            PromptInput::Many(v) if v.len() == 1 => Ok(v.into_iter().next().unwrap()),
            PromptInput::Many(v) if v.is_empty() => Err("`prompt` must not be an empty array"),
            PromptInput::Many(_) => Err(
                "Batch prompts (array with >1 element) are not supported; send one prompt per request",
            ),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct CompletionResponse {
    pub id: String,
    pub object: &'static str, // "text_completion"
    pub created: i64,
    pub model: String,
    pub choices: Vec<CompletionChoice>,
    pub usage: Usage,
}

#[derive(Debug, Serialize)]
pub struct CompletionChoice {
    pub index: u32,
    pub text: String,
    pub finish_reason: &'static str,
}

// ---------------------------------------------------------------------------
// /v1/embeddings
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
pub struct EmbeddingRequest {
    pub model: String,
    pub input: EmbeddingInput,
    #[serde(default)]
    pub encoding_format: Option<String>,
    #[serde(default)]
    pub user: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum EmbeddingInput {
    One(String),
    Many(Vec<String>),
}

impl EmbeddingInput {
    pub fn into_vec(self) -> Vec<String> {
        match self {
            EmbeddingInput::One(s) => vec![s],
            EmbeddingInput::Many(v) => v,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct EmbeddingResponse {
    pub object: &'static str, // "list"
    pub data: Vec<EmbeddingItem>,
    pub model: String,
    pub usage: EmbeddingUsage,
}

#[derive(Debug, Serialize)]
pub struct EmbeddingItem {
    pub object: &'static str, // "embedding"
    pub index: usize,
    pub embedding: Vec<f32>,
}

#[derive(Debug, Serialize)]
pub struct EmbeddingUsage {
    pub prompt_tokens: u64,
    pub total_tokens: u64,
}

// ---------------------------------------------------------------------------
// /v1/audio/transcriptions
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct TranscriptionResponse {
    pub text: String,
}

// ---------------------------------------------------------------------------
// /v1/audio/speech (TTS)
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
pub struct SpeechRequest {
    pub model: String,
    pub input: String,
    #[serde(default)]
    pub voice: Option<String>,
    #[serde(default)]
    pub response_format: Option<String>,
    #[serde(default)]
    pub speed: Option<f32>,
}
