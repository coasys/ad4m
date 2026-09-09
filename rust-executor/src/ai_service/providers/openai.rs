//! OpenAI-shaped chat provider — `POST {base}/v1/chat/completions`.
//!
//! Serves OpenAI itself and everything that copies its wire format: Groq,
//! OpenRouter, Google's compat endpoint, Ollama's `/v1`, vLLM, llama-server.
//! The HTTP work is `chat_gpt_lib_rs` (coasys fork); this file is the mapping
//! between [`ChatTurn`] and that crate's request shape.
//!
//! Tools are not passed natively here. Callers that want tool use on an
//! OpenAI-shaped endpoint go through the prompt-injection path in
//! `api::openai_compat::harness_bridge`, which renders tool definitions into
//! the system prompt and recovers calls from the reply text — one uniform
//! mechanism that works whether or not the far end supports `tools[]`.

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
    /// versioned path itself — so a base URL saved as `https://host/v1`
    /// (which is what every provider's docs show, and therefore what users
    /// paste) would otherwise resolve to `/v1/v1/chat/completions`.
    pub fn new(api_key: &str, base_url: Url) -> Self {
        let mut url = base_url;
        if let Some(segments) = url.path_segments() {
            if segments.clone().next() == Some("v1") {
                url.set_path(&segments.skip(1).collect::<Vec<_>>().join("/"));
            }
        }
        Self {
            client: ChatGPTClient::new(api_key, url.as_ref()),
        }
    }
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
    use super::*;
    use mockito::Matcher;

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
