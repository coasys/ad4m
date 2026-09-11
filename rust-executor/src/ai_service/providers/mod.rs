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
pub mod ollama;
pub mod openai;

#[cfg(test)]
mod anthropic_e2e;

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
        crate::types::ModelApiType::Ollama => ollama::list_models(api_key, base_url).await,
    }
}

/// Resolve a configured base URL to a versioned endpoint, e.g.
/// `https://api.anthropic.com` plus `messages` gives
/// `https://api.anthropic.com/v1/messages`.
///
/// Accepts the base URL with or without a `/v1` already on it, because both
/// spellings appear in provider documentation and therefore both are what gets
/// pasted into a model form. A path prefix survives, so a gateway that mounts a
/// provider under one keeps working.
pub(crate) fn versioned_endpoint(base_url: url::Url, path: &str) -> String {
    let trimmed = base_url.as_str().trim_end_matches('/').to_string();
    let root = trimmed
        .strip_suffix("/v1")
        .map(|s| s.to_string())
        .unwrap_or(trimmed);
    format!("{root}/v1/{path}")
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

/// Whether a wire protocol can carry tool definitions and return tool calls
/// as structured data.
///
/// The same fact as [`RemoteChat::supports_native_tools`], asked of a model
/// config rather than of a built client — the harness has to decide how to
/// render tools *before* anything reaches a worker thread, so it cannot ask
/// the instance. Kept beside the trait, and covered by a test asserting the
/// two agree, because two answers drifting apart would route a model down a
/// path its provider cannot serve.
pub fn api_type_supports_native_tools(api_type: &crate::types::ModelApiType) -> bool {
    match api_type {
        crate::types::ModelApiType::Anthropic => true,
        crate::types::ModelApiType::Ollama => true,
        crate::types::ModelApiType::OpenAi => false,
    }
}

/// Who is speaking in one turn of a conversation.
///
/// Smaller than either provider's role set, because a tool result is not a
/// fourth speaker: it is something said back to the model, which both wire
/// formats express as a user turn carrying a marker. That marker is
/// [`ChatTurn::tool_result_for`] rather than a role of its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChatRole {
    System,
    User,
    Assistant,
}

/// A tool the model may call.
///
/// `parameters` is a JSON Schema object. Providers rename the field to suit
/// their wire format — Anthropic calls it `input_schema` — but none of them
/// reinterpret it.
#[derive(Debug, Clone)]
pub struct ToolSpec {
    pub name: String,
    pub description: String,
    pub parameters: serde_json::Value,
}

/// A call the model asked for.
///
/// `arguments` is a parsed object, not the JSON string OpenAI puts on the
/// wire: every caller wants the object, and parsing it once here means a
/// malformed one is caught in the provider rather than three layers up.
///
/// Field-for-field the same as `harness::HarnessToolCall`, and deliberately
/// not shared with it. `harness` sits above this module and already carries an
/// import cycle its own guide asks nobody to widen. A provider reaching up
/// into it to save one struct would buy four lines and cost the layering.
/// `openai_compat::harness_bridge` owns the conversion, because it is the one
/// place that legitimately sees both.
#[derive(Debug, Clone, PartialEq)]
pub struct ToolCall {
    pub id: String,
    pub name: String,
    pub arguments: serde_json::Value,
}

/// One turn of a conversation, as the providers see it.
#[derive(Debug, Clone)]
pub struct ChatTurn {
    pub role: ChatRole,
    pub content: String,
    /// Tool calls this assistant turn made. Empty on every other turn.
    ///
    /// Carried so the model sees its own prior invocations on the next turn,
    /// which is what lets it correlate them with the results that follow.
    pub tool_calls: Vec<ToolCall>,
    /// Set when this turn is a tool *result*, naming the call it answers.
    ///
    /// Providers that express tool results structurally use the id; the ones
    /// that do not ignore it, because their callers rendered the result into
    /// `content` before it ever got here.
    pub tool_result_for: Option<String>,
}

impl ChatTurn {
    fn new(role: ChatRole, content: impl Into<String>) -> Self {
        Self {
            role,
            content: content.into(),
            tool_calls: Vec::new(),
            tool_result_for: None,
        }
    }

    pub fn system(content: impl Into<String>) -> Self {
        Self::new(ChatRole::System, content)
    }

    pub fn user(content: impl Into<String>) -> Self {
        Self::new(ChatRole::User, content)
    }

    pub fn assistant(content: impl Into<String>) -> Self {
        Self::new(ChatRole::Assistant, content)
    }

    /// An assistant turn that called tools.
    pub fn assistant_calling(content: impl Into<String>, tool_calls: Vec<ToolCall>) -> Self {
        Self {
            tool_calls,
            ..Self::new(ChatRole::Assistant, content)
        }
    }

    /// The result of one tool call, going back to the model.
    pub fn tool_result(call_id: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            tool_result_for: Some(call_id.into()),
            ..Self::new(ChatRole::User, content)
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
    /// Tools to advertise. Only ever non-empty for a provider whose
    /// [`RemoteChat::supports_native_tools`] is true — everything else has its
    /// tools rendered into the prompt before the request is built.
    pub tools: Vec<ToolSpec>,
}

impl ChatRequest {
    pub fn new(model: impl Into<String>, messages: Vec<ChatTurn>) -> Self {
        Self {
            model: model.into(),
            messages,
            tools: Vec::new(),
        }
    }

    pub fn with_tools(mut self, tools: Vec<ToolSpec>) -> Self {
        self.tools = tools;
        self
    }
}

/// What a provider reported about token use.
///
/// Every field is optional because not every protocol reports it. The counts
/// this executor bills on are still estimated from character length — these
/// are the exact numbers, carried so a caller can check them and so billing
/// can move onto them later.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ChatUsage {
    pub input_tokens: Option<u64>,
    pub output_tokens: Option<u64>,
    /// Tokens served from the prompt cache. Zero means the breakpoint did not
    /// land, which is otherwise invisible: caching fails silently and the only
    /// symptom is a larger bill.
    pub cache_read_tokens: Option<u64>,
    /// Tokens written to the cache on this call.
    pub cache_write_tokens: Option<u64>,
}

/// What a provider answered.
#[derive(Debug, Clone, Default)]
pub struct ChatReply {
    pub text: String,
    /// Tool calls the model wants dispatched. Always empty from a provider
    /// without native tool support — there, calls are recovered from `text` by
    /// the caller that rendered the tools in.
    pub tool_calls: Vec<ToolCall>,
    /// What the provider reported about token use. Empty when it reported
    /// nothing.
    pub usage: ChatUsage,
}

/// A remote chat endpoint.
///
/// One instance per configured model, built once when the model's worker
/// thread starts and reused for every prompt on that thread — so an
/// implementation may hold a connection pool, but must not hold per-request
/// state.
#[async_trait]
pub trait RemoteChat: Send + Sync {
    /// Whether tools can be handed to this provider as structured
    /// definitions, and calls read back as structured blocks.
    ///
    /// False — the default — means the caller must render tool definitions
    /// into the prompt and recover calls by parsing the reply text, which is
    /// the uniform path in `openai_compat::harness_bridge` and works against
    /// any model at all. True is strictly better where it is available: the
    /// model is handed a schema instead of a description of one, and the calls
    /// come back as data rather than as text that happens to look like data.
    ///
    /// Answering this wrongly is the one way a provider can break a caller, so
    /// it is a plain fact about the wire format, never a preference.
    fn supports_native_tools(&self) -> bool {
        false
    }

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
    fn the_two_answers_about_native_tools_agree() {
        // `api_type_supports_native_tools` decides how the harness renders
        // tools; the trait method describes the built client. If they ever
        // disagree, a model is handed tools its provider will not send, or
        // has them injected into a prompt when it could have had the real
        // thing.
        use crate::types::ModelApiType;
        let base = url("https://example.test");

        for api_type in [
            ModelApiType::OpenAi,
            ModelApiType::Anthropic,
            ModelApiType::Ollama,
        ] {
            let client: Box<dyn RemoteChat> = match api_type {
                ModelApiType::OpenAi => Box::new(openai::OpenAiChat::new("k", base.clone())),
                ModelApiType::Anthropic => {
                    Box::new(anthropic::AnthropicChat::new("k", base.clone()))
                }
                ModelApiType::Ollama => Box::new(ollama::OllamaChat::new("", base.clone())),
            };
            assert_eq!(
                api_type_supports_native_tools(&api_type),
                client.supports_native_tools(),
                "disagreement for {api_type:?}"
            );
        }
    }

    #[test]
    fn an_endpoint_is_built_from_a_bare_host() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
        assert_eq!(
            versioned_endpoint(url("https://api.openai.com"), "models"),
            "https://api.openai.com/v1/models"
        );
    }

    #[test]
    fn a_base_url_already_carrying_v1_does_not_get_a_second_one() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com/v1"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
        assert_eq!(
            versioned_endpoint(url("https://api.groq.com/openai/v1"), "models"),
            "https://api.groq.com/openai/v1/models"
        );
    }

    #[test]
    fn a_trailing_slash_is_tolerated() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com/v1/"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
    }

    #[test]
    fn a_proxy_path_prefix_survives() {
        // A gateway may mount a provider under a path. That prefix has to
        // survive, which is why this appends rather than rewriting the path.
        assert_eq!(
            versioned_endpoint(url("https://gateway.internal/anthropic"), "messages"),
            "https://gateway.internal/anthropic/v1/messages"
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
