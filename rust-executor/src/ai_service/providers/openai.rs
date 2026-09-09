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

        Ok(ChatReply { text })
    }
}
