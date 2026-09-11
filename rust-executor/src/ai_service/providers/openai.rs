//! OpenAI-shaped chat provider — `POST {base}/v1/chat/completions`.
//!
//! Serves OpenAI itself and everything that copies its wire format: Groq,
//! OpenRouter, Google's compat endpoint, Ollama's `/v1`, vLLM, llama-server.
//! The HTTP work is `chat_gpt_lib_rs` (coasys fork); this file is the mapping
//! between [`ChatTurn`] and that crate's request shape.
//!
//! Tools are not passed natively here. That is a limitation of this client and
//! not of the protocol — the OpenAI format carries `tools[]` and answers with
//! `tool_calls`; this file simply does not build them yet. Callers that want
//! tool use on an OpenAI-shaped endpoint go through the prompt-injection path
//! in `api::openai_compat::harness_bridge`, which renders tool definitions into
//! the system prompt and recovers calls from the reply text — one uniform
//! mechanism that works whether or not the far end supports `tools[]`.
//!
//! A request that arrives carrying tools anyway is refused rather than served
//! without them, so the day someone flips the answer in
//! `api_type_supports_native_tools` the mistake is loud.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use chat_gpt_lib_rs::{ChatGPTClient, ChatInput, Message, Role};
use url::Url;

use super::{ChatReply, ChatRequest, ChatRole, ChatTurn, RemoteChat};

pub struct OpenAiChat {
    client: ChatGPTClient,
}

impl OpenAiChat {
    /// Build a client for `base_url`.
    ///
    /// A trailing `/v1` is stripped because `chat_gpt_lib_rs` appends the
    /// versioned path itself, so a base URL saved as `https://host/v1` would
    /// otherwise resolve to `/v1/v1/chat/completions`. Every provider's docs
    /// show the `/v1` form, so that is what gets pasted into a model form.
    ///
    /// It must be the *trailing* segment that is tested, not the first. An
    /// earlier version checked the first, which worked for
    /// `https://api.openai.com/v1` and failed for every provider that mounts
    /// under a path — including Groq, whose documented URL is
    /// `https://api.groq.com/openai/v1`, and this executor's own compat
    /// surface at `/api/v1/openai/v1`. Discovery already resolved those
    /// correctly through `versioned_endpoint`, so chat and model listing
    /// disagreed about the same URL.
    pub fn new(api_key: &str, base_url: Url) -> Self {
        let trimmed = base_url.as_str().trim_end_matches('/');
        let root = trimmed.strip_suffix("/v1").unwrap_or(trimmed);
        Self {
            client: ChatGPTClient::new(api_key, root),
        }
    }
}

/// The base URL `chat_gpt_lib_rs` is handed, exposed for tests.
#[cfg(test)]
fn chat_base(base_url: Url) -> String {
    let trimmed = base_url.as_str().trim_end_matches('/').to_string();
    trimmed
        .strip_suffix("/v1")
        .map(|s| s.to_string())
        .unwrap_or(trimmed)
}

/// Ask an endpoint which models it serves — `GET {base}/v1/models`.
///
/// Standalone rather than a trait method because discovery happens *before*
/// a model exists: an operator is filling in a form and wants to know what
/// they can pick, so there is no configured model and no worker thread to
/// route through.
///
/// Doubles as a credential check. A wrong key fails here with the provider's
/// own 401 instead of surfacing as a puzzling failure on the first real
/// completion, which is where it used to surface.
pub async fn list_models(api_key: &str, base_url: Url) -> Result<Vec<String>> {
    let endpoint = super::versioned_endpoint(base_url, "models");

    let mut request = reqwest::Client::new().get(&endpoint);
    if !api_key.is_empty() {
        request = request.bearer_auth(api_key);
    }

    let response = request
        .send()
        .await
        .map_err(|e| anyhow!("Could not reach {endpoint}: {e}"))?;

    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(anyhow!("Model listing failed ({status}): {body}"));
    }

    let json: serde_json::Value = response
        .json()
        .await
        .map_err(|e| anyhow!("Could not read the model list: {e}"))?;

    Ok(super::model_ids_from_data(&json))
}

fn to_wire_role(role: ChatRole) -> Role {
    match role {
        ChatRole::System => Role::System,
        ChatRole::User => Role::User,
        ChatRole::Assistant => Role::Assistant,
    }
}

fn to_wire_message(turn: &ChatTurn) -> Message {
    Message {
        role: to_wire_role(turn.role),
        content: turn.content.clone(),
    }
}

#[async_trait]
impl RemoteChat for OpenAiChat {
    async fn chat(&self, request: ChatRequest) -> Result<ChatReply> {
        // `ChatRequest::tools` is documented as only ever non-empty for a
        // provider whose `supports_native_tools` is true. This enforces that
        // invariant rather than asserting it. Building `ChatInput` from
        // `to_wire_message` alone would otherwise drop the definitions
        // silently: the model is handed no tools, answers in prose, and the
        // empty `tool_calls` reads to the harness as "done" on turn one.
        if !request.tools.is_empty() {
            return Err(anyhow!(
                "This client does not pass tools natively, so a request carrying {} of them \
                 cannot be served. The OpenAI wire format supports them; `OpenAiChat` does not \
                 build them yet. Render tools into the prompt instead.",
                request.tools.len()
            ));
        }

        let chat_input = ChatInput {
            model: chat_gpt_lib_rs::Model::Custom(request.model),
            messages: request.messages.iter().map(to_wire_message).collect(),
            ..Default::default()
        };

        let response = self
            .client
            .chat(chat_input)
            .await
            .map_err(|e| anyhow!("Error connecting to remote LLM API: {:?}", e))?;

        let text = response
            .choices
            .first()
            .map(|choice| choice.message.content.clone())
            .ok_or_else(|| anyhow!("Got response with no choice"))?;

        // No native tool support: a caller wanting tools rendered them into
        // the prompt and will recover the calls from this text itself.
        Ok(ChatReply {
            text,
            tool_calls: Vec::new(),
            // `chat_gpt_lib_rs` does not surface the usage block.
            usage: Default::default(),
        })
    }
}

/// Wire-level tests for the discovery path against a mock server.
///
/// The chat path is `chat_gpt_lib_rs`'s to get right and is exercised in
/// production; discovery is ours, and its auth differs from the chat client's
/// in a way worth pinning down — some endpoints that serve models need no key
/// at all, and sending an empty bearer token to one is not the same as sending
/// nothing.
#[cfg(test)]
mod wire_tests {
    use super::super::ToolSpec;
    use super::*;
    use mockito::Matcher;

    fn url(s: &str) -> Url {
        Url::parse(s).expect("test URL parses")
    }

    #[test]
    fn a_trailing_v1_is_stripped_so_the_client_does_not_double_it() {
        assert_eq!(
            chat_base(url("https://api.openai.com/v1")),
            "https://api.openai.com"
        );
    }

    #[test]
    fn a_v1_under_a_path_prefix_is_stripped_too() {
        // Groq's documented URL, and the shape this executor mounts its own
        // compat surface at. Testing the first segment instead of the last
        // sent these to `/openai/v1/v1/chat/completions`.
        assert_eq!(
            chat_base(url("https://api.groq.com/openai/v1")),
            "https://api.groq.com/openai"
        );
        assert_eq!(
            chat_base(url("http://localhost:12000/api/v1/openai/v1")),
            "http://localhost:12000/api/v1/openai"
        );
    }

    #[test]
    fn a_base_url_without_v1_is_left_alone() {
        assert_eq!(
            chat_base(url("http://localhost:11434")),
            "http://localhost:11434"
        );
    }

    #[test]
    fn chat_and_discovery_agree_about_the_same_url() {
        // The bug this pair of assertions exists to prevent: discovery
        // resolved a prefixed URL correctly while chat doubled the `/v1`.
        let configured = url("https://api.groq.com/openai/v1");
        assert_eq!(
            format!("{}/v1/chat/completions", chat_base(configured.clone())),
            "https://api.groq.com/openai/v1/chat/completions"
        );
        assert_eq!(
            super::super::versioned_endpoint(configured, "models"),
            "https://api.groq.com/openai/v1/models"
        );
    }

    #[tokio::test]
    async fn a_request_carrying_tools_is_refused_rather_than_stripped() {
        // The failure this guards is quiet: the tools would be dropped, the
        // model would answer in prose, and the harness would read the empty
        // `tool_calls` as "the model is done" on its first turn. So the
        // assertion is that nothing is sent at all.
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", Matcher::Any)
            .expect(0)
            .with_status(200)
            .create_async()
            .await;

        let client = OpenAiChat::new("sk-test", url(&server.url()));
        let error = client
            .chat(
                ChatRequest::new("gpt-4o", vec![ChatTurn::user("hi")]).with_tools(vec![ToolSpec {
                    name: "search".into(),
                    description: "find things".into(),
                    parameters: serde_json::json!({ "type": "object" }),
                }]),
            )
            .await
            .expect_err("a request carrying tools is refused");

        assert!(
            error.to_string().contains("does not pass tools natively"),
            "the error says whose limitation it is: {error}"
        );
        mock.assert_async().await;
    }

    #[tokio::test]
    async fn a_key_is_sent_as_a_bearer_token() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/v1/models")
            .match_header("authorization", "Bearer sk-test")
            .with_status(200)
            .with_body(r#"{"data":[{"id":"gpt-4o"}]}"#)
            .create_async()
            .await;

        let models = list_models("sk-test", Url::parse(&server.url()).unwrap())
            .await
            .expect("listing succeeds");

        mock.assert_async().await;
        assert_eq!(models, vec!["gpt-4o"]);
    }

    #[tokio::test]
    async fn no_key_means_no_authorization_header_at_all() {
        // A local Ollama or vLLM serves models without credentials, and some
        // reject a malformed `Bearer ` outright — so an absent key must mean
        // an absent header rather than an empty one.
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/v1/models")
            .match_header("authorization", Matcher::Missing)
            .with_status(200)
            .with_body(r#"{"data":[{"id":"llama3.1"}]}"#)
            .create_async()
            .await;

        let models = list_models("", Url::parse(&server.url()).unwrap())
            .await
            .expect("listing succeeds");

        mock.assert_async().await;
        assert_eq!(models, vec!["llama3.1"]);
    }

    #[tokio::test]
    async fn a_refused_key_is_an_error_rather_than_an_empty_list() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(401)
            .with_body(r#"{"error":{"message":"Incorrect API key provided"}}"#)
            .create_async()
            .await;

        let error = list_models("bad", Url::parse(&server.url()).unwrap())
            .await
            .expect_err("a 401 is an error");

        assert!(error.to_string().contains("401"));
        assert!(error.to_string().contains("Incorrect API key provided"));
    }

    #[tokio::test]
    async fn an_unreachable_endpoint_names_the_url_it_tried() {
        // The URL is the thing an operator got wrong, so it belongs in the
        // message rather than only in whatever reqwest says went wrong.
        let error = list_models("k", Url::parse("http://127.0.0.1:1/").unwrap())
            .await
            .expect_err("nothing listens there");

        assert!(error.to_string().contains("127.0.0.1:1"), "got: {error}");
    }
}
