//! Anthropic Messages API — `POST {base}/v1/messages`.
//!
//! Hand-rolled on `reqwest` rather than pulling a vendor SDK: the surface we
//! need is one endpoint, and the crate's other model clients are already
//! coasys forks, so a third-party dependency here would be the odd one out.
//!
//! ## Why not route Claude through the OpenAI client
//!
//! Anthropic publishes an OpenAI-compatible shim, and it would have been the
//! cheap option. Two things rule it out. It does not support prompt caching,
//! and their own docs call it a testing convenience rather than a production
//! path. Caching is the deciding one: an interpretation pass makes up to
//! `max_tool_calls` completions against a system prompt that does not change
//! between them, and without a cache breakpoint the node pays full input price
//! on every one of those turns.
//!
//! ## Caching policy
//!
//! The system prompt is marked `cache_control: ephemeral` and nothing else is.
//! It is the one part of a request that is stable by construction — the task's
//! system prompt plus, on the tool path, the rendered tool definitions — while
//! everything after it grows a turn at a time. Callers do not ask for this and
//! cannot switch it off: there is no way to express a breakpoint through the
//! OpenAI-shaped surface this executor exposes, so the decision has to be made
//! here, and marking a stable prefix is never the wrong call. Anthropic
//! silently declines to cache a prefix below its minimum, so a short prompt
//! costs nothing and errors nowhere.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use url::Url;

use super::{ChatReply, ChatRequest, ChatRole, RemoteChat};

/// Wire version. Anthropic requires it on every request and treats it as the
/// contract: pinning it is what stops a server-side change reshaping our
/// responses. Bump deliberately, never to chase a new feature by accident.
const ANTHROPIC_VERSION: &str = "2023-06-01";

/// `max_tokens` is mandatory on this API — unlike the OpenAI path, where
/// omitting it lets the server pick. 16k is large enough that a long
/// interpretation answer is not clipped and small enough to bound a runaway.
/// Model options override it (`ModelApi.options.max_tokens`).
const DEFAULT_MAX_TOKENS: u32 = 16_384;

/// A completion can legitimately run for minutes on a long prompt. Without a
/// timeout a hung upstream would park this model's worker thread forever,
/// since the thread handles one request at a time.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(600);

pub struct AnthropicChat {
    http: reqwest::Client,
    api_key: String,
    /// Fully-resolved messages endpoint, e.g. `https://api.anthropic.com/v1/messages`.
    endpoint: String,
}

impl AnthropicChat {
    pub fn new(api_key: &str, base_url: Url) -> Self {
        Self {
            http: reqwest::Client::builder()
                .timeout(REQUEST_TIMEOUT)
                .build()
                .unwrap_or_default(),
            api_key: api_key.to_string(),
            endpoint: messages_endpoint(base_url),
        }
    }
}

/// Resolve a configured base URL to the messages endpoint.
///
/// Accepts both `https://api.anthropic.com` and `https://api.anthropic.com/v1`
/// because both appear in Anthropic's own documentation, so both are what
/// people paste into a model form. Normalising here rather than demanding one
/// spelling keeps a wrong-looking-but-reasonable URL from 404ing.
fn messages_endpoint(base_url: Url) -> String {
    let trimmed = base_url.as_str().trim_end_matches('/').to_string();
    let root = trimmed
        .strip_suffix("/v1")
        .map(|s| s.to_string())
        .unwrap_or(trimmed);
    format!("{root}/v1/messages")
}

// ---------------------------------------------------------------------------
// Wire types
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct MessagesRequest {
    model: String,
    max_tokens: u32,
    /// Sent as a block list rather than a bare string so it can carry
    /// `cache_control`. Omitted when the conversation had no system turn.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    system: Vec<SystemBlock>,
    messages: Vec<WireMessage>,
}

#[derive(Serialize)]
struct SystemBlock {
    #[serde(rename = "type")]
    kind: &'static str,
    text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    cache_control: Option<CacheControl>,
}

#[derive(Serialize)]
struct CacheControl {
    #[serde(rename = "type")]
    kind: &'static str,
}

#[derive(Serialize)]
struct WireMessage {
    role: &'static str,
    content: String,
}

#[derive(Deserialize)]
struct MessagesResponse {
    #[serde(default)]
    content: Vec<ContentBlock>,
}

#[derive(Deserialize)]
struct ContentBlock {
    #[serde(rename = "type")]
    kind: String,
    #[serde(default)]
    text: String,
}

// ---------------------------------------------------------------------------

/// Split a turn list into Anthropic's shape: system turns are hoisted out of
/// the conversation and concatenated, the rest stay in order.
///
/// Concatenation with a newline matches how `api::openai_compat` already
/// hoists system/developer messages, so a conversation behaves the same
/// whichever provider answers it. Consecutive same-role turns are left alone —
/// the API merges them itself.
fn split_system(request: &ChatRequest) -> (Vec<String>, Vec<WireMessage>) {
    let mut system = Vec::new();
    let mut messages = Vec::new();

    for turn in &request.messages {
        match turn.role {
            ChatRole::System => system.push(turn.content.clone()),
            ChatRole::User => messages.push(WireMessage {
                role: "user",
                content: turn.content.clone(),
            }),
            ChatRole::Assistant => messages.push(WireMessage {
                role: "assistant",
                content: turn.content.clone(),
            }),
        }
    }

    (system, messages)
}

#[async_trait]
impl RemoteChat for AnthropicChat {
    async fn chat(&self, request: ChatRequest) -> Result<ChatReply> {
        let (system_parts, messages) = split_system(&request);

        let system = if system_parts.is_empty() {
            Vec::new()
        } else {
            vec![SystemBlock {
                kind: "text",
                text: system_parts.join("\n"),
                cache_control: Some(CacheControl { kind: "ephemeral" }),
            }]
        };

        let body = MessagesRequest {
            model: request.model,
            max_tokens: DEFAULT_MAX_TOKENS,
            system,
            messages,
        };

        let response = self
            .http
            .post(&self.endpoint)
            .header("x-api-key", &self.api_key)
            .header("anthropic-version", ANTHROPIC_VERSION)
            .json(&body)
            .send()
            .await
            .map_err(|e| anyhow!("Error connecting to remote LLM API: {:?}", e))?;

        let status = response.status();
        if !status.is_success() {
            // Anthropic puts the useful part in the body (`error.message`);
            // the status alone does not distinguish a bad key from a bad
            // model name, and both are things an operator has to fix.
            let body = response.text().await.unwrap_or_default();
            return Err(anyhow!("Anthropic API error {}: {}", status, body));
        }

        let parsed: MessagesResponse = response
            .json()
            .await
            .map_err(|e| anyhow!("Could not read Anthropic response: {:?}", e))?;

        // A reply is a list of blocks; the text ones concatenate. Anything
        // else (a tool_use block, once tools are passed natively) is not this
        // method's business and is skipped rather than stringified.
        let text = parsed
            .content
            .iter()
            .filter(|b| b.kind == "text")
            .map(|b| b.text.as_str())
            .collect::<Vec<_>>()
            .join("");

        Ok(ChatReply { text })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(s: &str) -> Url {
        Url::parse(s).expect("test URL parses")
    }

    #[test]
    fn endpoint_accepts_a_bare_host() {
        assert_eq!(
            messages_endpoint(url("https://api.anthropic.com")),
            "https://api.anthropic.com/v1/messages"
        );
    }

    #[test]
    fn endpoint_accepts_a_host_already_carrying_v1() {
        assert_eq!(
            messages_endpoint(url("https://api.anthropic.com/v1")),
            "https://api.anthropic.com/v1/messages"
        );
    }

    #[test]
    fn endpoint_tolerates_a_trailing_slash() {
        assert_eq!(
            messages_endpoint(url("https://api.anthropic.com/v1/")),
            "https://api.anthropic.com/v1/messages"
        );
    }

    #[test]
    fn endpoint_keeps_a_proxy_path_prefix() {
        // A gateway in front of Anthropic may mount it under a path; that
        // prefix has to survive, which is why this appends rather than
        // rewriting the whole path.
        assert_eq!(
            messages_endpoint(url("https://gateway.internal/anthropic")),
            "https://gateway.internal/anthropic/v1/messages"
        );
    }

    #[test]
    fn system_turns_are_hoisted_out_of_the_conversation() {
        let request = ChatRequest {
            model: "claude-opus-5".to_string(),
            messages: vec![
                super::super::ChatTurn::system("be terse"),
                super::super::ChatTurn::user("hello"),
                super::super::ChatTurn::assistant("hi"),
                super::super::ChatTurn::user("again"),
            ],
        };

        let (system, messages) = split_system(&request);

        assert_eq!(system, vec!["be terse".to_string()]);
        assert_eq!(messages.len(), 3);
        assert_eq!(messages[0].role, "user");
        assert_eq!(messages[1].role, "assistant");
        assert_eq!(messages[2].role, "user");
    }

    #[test]
    fn several_system_turns_concatenate() {
        let request = ChatRequest {
            model: "claude-opus-5".to_string(),
            messages: vec![
                super::super::ChatTurn::system("first"),
                super::super::ChatTurn::system("second"),
                super::super::ChatTurn::user("go"),
            ],
        };

        let (system, _) = split_system(&request);
        assert_eq!(system.join("\n"), "first\nsecond");
    }

    #[test]
    fn the_system_block_carries_a_cache_breakpoint() {
        let body = MessagesRequest {
            model: "claude-opus-5".to_string(),
            max_tokens: DEFAULT_MAX_TOKENS,
            system: vec![SystemBlock {
                kind: "text",
                text: "stable prefix".to_string(),
                cache_control: Some(CacheControl { kind: "ephemeral" }),
            }],
            messages: vec![],
        };

        let json = serde_json::to_value(&body).expect("request serialises");
        assert_eq!(json["system"][0]["cache_control"]["type"], "ephemeral");
    }

    #[test]
    fn an_absent_system_prompt_is_omitted_rather_than_sent_empty() {
        let body = MessagesRequest {
            model: "claude-opus-5".to_string(),
            max_tokens: DEFAULT_MAX_TOKENS,
            system: vec![],
            messages: vec![WireMessage {
                role: "user",
                content: "go".to_string(),
            }],
        };

        let json = serde_json::to_value(&body).expect("request serialises");
        assert!(json.get("system").is_none());
    }
}
