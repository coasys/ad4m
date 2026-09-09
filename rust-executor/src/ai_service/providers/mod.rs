//! Remote LLM providers — one trait, one implementation per wire protocol.
//!
//! Before this module the remote path was a bare `ChatGPTClient` held in
//! `LlmModel::Remote`, so "a remote model" and "an OpenAI-shaped HTTP API"
//! were the same thing. Anthropic is not OpenAI-shaped: it carries tools and
//! tool results as structured content blocks rather than text, and its prompt
//! cache has to be addressed explicitly with `cache_control` breakpoints —
//! neither of which an OpenAI request struct has anywhere to put.
//!
//! So the provider is the seam. [`RemoteChat`] is what the per-model worker
//! thread in `ai_service` talks to; each implementation owns its own wire
//! format, and nothing above this module knows which one answered.
//!
//! Adding a provider: implement [`RemoteChat`], and construct it in
//! [`build`] from the model's [`ModelApiType`](crate::types::ModelApiType).
//! Do not add a variant to `LlmModel` — that enum distinguishes *local
//! weights* from *a remote endpoint*, which is a different question.

use anyhow::Result;
use async_trait::async_trait;
use tokio::sync::mpsc;

pub mod anthropic;
pub mod openai;

/// Ask a configured endpoint which models it serves, before any model has
/// been registered against it.
///
/// The settings form calling this is how an operator finds out what they can
/// pick, and — because a bad key fails here rather than on the first
/// completion — whether their credentials work at all.
pub async fn list_models(
    api_type: &crate::types::ModelApiType,
    api_key: &str,
    base_url: url::Url,
) -> Result<Vec<String>> {
    match api_type {
        crate::types::ModelApiType::OpenAi => openai::list_models(api_key, base_url).await,
        crate::types::ModelApiType::Anthropic => anthropic::list_models(api_key, base_url).await,
    }
}

/// Resolve a base URL to a `/v1/models` endpoint, accepting the URL with or
/// without a `/v1` already on it. Both spellings appear in provider docs, so
/// both are what ends up pasted into a form.
pub(crate) fn models_endpoint(base_url: url::Url) -> String {
    let trimmed = base_url.as_str().trim_end_matches('/').to_string();
    let root = trimmed
        .strip_suffix("/v1")
        .map(|s| s.to_string())
        .unwrap_or(trimmed);
    format!("{root}/v1/models")
}

/// Both providers answer a model listing as `{"data": [{"id": …}, …]}`.
/// Entries without an `id` are skipped rather than failing the listing — a
/// partially-understood response is still useful to somebody filling in a form.
pub(crate) fn model_ids_from_data(json: &serde_json::Value) -> Vec<String> {
    json.get("data")
        .and_then(|d| d.as_array())
        .map(|entries| {
            entries
                .iter()
                .filter_map(|entry| entry.get("id")?.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default()
}

/// Who is speaking in one turn of a conversation.
///
/// Deliberately smaller than either provider's role set: tool results and
/// tool calls reach us already rendered into text by the callers in
/// `api::openai_compat` (see `flatten_message` there), so a provider that
/// cannot express a tool role never has to invent one. A provider that *can*
/// express one reconstructs it from the rendered form.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChatRole {
    System,
    User,
    Assistant,
}

/// One turn of a conversation, as the providers see it.
#[derive(Debug, Clone)]
pub struct ChatTurn {
    pub role: ChatRole,
    pub content: String,
}

impl ChatTurn {
    pub fn system(content: impl Into<String>) -> Self {
        Self {
            role: ChatRole::System,
            content: content.into(),
        }
    }

    pub fn user(content: impl Into<String>) -> Self {
        Self {
            role: ChatRole::User,
            content: content.into(),
        }
    }

    pub fn assistant(content: impl Into<String>) -> Self {
        Self {
            role: ChatRole::Assistant,
            content: content.into(),
        }
    }
}

/// One completion request.
///
/// `model` is the provider's own model string (`gpt-4o`, `claude-opus-5`),
/// not AD4M's model id — resolution from one to the other happens before we
/// get here.
#[derive(Debug, Clone)]
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<ChatTurn>,
}

/// What a provider answered.
#[derive(Debug, Clone, Default)]
pub struct ChatReply {
    pub text: String,
}

/// A remote chat endpoint.
///
/// One instance per configured model, built once when the model's worker
/// thread starts and reused for every prompt on that thread — so an
/// implementation may hold a connection pool, but must not hold per-request
/// state.
#[async_trait]
pub trait RemoteChat: Send + Sync {
    /// Send a conversation, get the assistant's reply.
    async fn chat(&self, request: ChatRequest) -> Result<ChatReply>;

    /// The same, pushing each piece of text through `tokens` as it arrives.
    ///
    /// The default answers the whole reply as one chunk, which is what the
    /// non-streaming providers have always done: an SSE consumer still sees
    /// the streaming protocol, it just sees one delta. Override it where the
    /// upstream can genuinely stream, and the caller gets text as the model
    /// writes it with no change on its side.
    ///
    /// The returned [`ChatReply`] always carries the complete text whether or
    /// not it streamed, because the caller bills on it.
    async fn chat_stream(
        &self,
        request: ChatRequest,
        tokens: mpsc::UnboundedSender<String>,
    ) -> Result<ChatReply> {
        let reply = self.chat(request).await?;
        let _ = tokens.send(reply.text.clone());
        Ok(reply)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(s: &str) -> url::Url {
        url::Url::parse(s).expect("test URL parses")
    }

    #[test]
    fn models_endpoint_accepts_a_bare_host() {
        assert_eq!(
            models_endpoint(url("https://api.openai.com")),
            "https://api.openai.com/v1/models"
        );
    }

    #[test]
    fn models_endpoint_accepts_a_host_already_carrying_v1() {
        assert_eq!(
            models_endpoint(url("https://api.groq.com/openai/v1")),
            "https://api.groq.com/openai/v1/models"
        );
    }

    #[test]
    fn model_ids_are_read_out_of_the_data_array() {
        let json = serde_json::json!({
            "object": "list",
            "data": [{"id": "gpt-4o"}, {"id": "gpt-4o-mini"}],
        });
        assert_eq!(model_ids_from_data(&json), vec!["gpt-4o", "gpt-4o-mini"]);
    }

    #[test]
    fn an_entry_without_an_id_is_skipped_rather_than_failing_the_listing() {
        // A half-understood response is still useful to somebody filling in a
        // form; refusing the whole list because one row is odd is not.
        let json = serde_json::json!({ "data": [{"id": "a"}, {"object": "model"}, {"id": "b"}] });
        assert_eq!(model_ids_from_data(&json), vec!["a", "b"]);
    }

    #[test]
    fn a_response_with_no_data_array_lists_nothing() {
        assert_eq!(
            model_ids_from_data(&serde_json::json!({ "error": "nope" })),
            Vec::<String>::new()
        );
    }
}
