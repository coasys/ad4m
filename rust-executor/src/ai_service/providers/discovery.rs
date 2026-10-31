//! What an endpoint serves, and whether it answers: one [`Discover`] per endpoint kind.
//!
//! Listing is also the health check. An endpoint that lists its models is reachable and
//! takes the key it was given; a 401 or 403 means the key is refused; anything else means
//! nobody useful answered. So [`Discover::health`] is the listing, classified.

use anyhow::Result;
use async_trait::async_trait;
use std::time::Duration;
use url::Url;

use super::http::UpstreamStatus;
use crate::types::{AiCapability, DiscoveredModel, EndpointHealth, EndpointKind, EndpointState};

/// How long a health check or the probe of a new endpoint waits for an answer. An operator
/// waits on `ai.addEndpoint`, so this is shorter than the listing an operator asks for.
pub const PROBE_TIMEOUT: Duration = Duration::from_secs(10);

/// Lists an endpoint's models. One instance per endpoint; built by [`discover_for`].
#[async_trait]
pub trait Discover: Send + Sync {
    async fn list_models(&self) -> Result<Vec<DiscoveredModel>>;

    /// The listing, classified as the endpoint's health.
    async fn health(&self) -> EndpointHealth {
        health_of(self.list_models().await)
    }
}

/// The [`Discover`] for an endpoint of `kind`. Requests give up after `timeout`.
pub fn discover_for(
    kind: EndpointKind,
    base_url: Url,
    api_key: &str,
    timeout: Duration,
) -> Box<dyn Discover> {
    match kind {
        EndpointKind::Ollama => Box::new(super::ollama::OllamaDiscover::new(base_url, timeout)),
        EndpointKind::Anthropic => Box::new(super::anthropic::AnthropicDiscover::new(
            api_key, base_url, timeout,
        )),
        EndpointKind::LlamaServer
        | EndpointKind::Vllm
        | EndpointKind::OpenAi
        | EndpointKind::OpenAiAudio
        | EndpointKind::OpenAiCompatible => Box::new(super::openai::OpenAiDiscover::new(
            kind, api_key, base_url, timeout,
        )),
    }
}

/// A listing's outcome as health: reachable with the ids it listed, unauthorized on 401 or
/// 403, unreachable otherwise. Errors carry no key: [`super::http::fetch_json`] scrubs it.
pub fn health_of(listing: Result<Vec<DiscoveredModel>>) -> EndpointHealth {
    let checked_at = Some(chrono::Utc::now().timestamp_millis());
    match listing {
        Ok(models) => EndpointHealth {
            state: EndpointState::Reachable,
            checked_at,
            detail: None,
            served_models: models.into_iter().map(|model| model.id).collect(),
        },
        Err(error) => {
            let refused = error
                .downcast_ref::<UpstreamStatus>()
                .is_some_and(|status| matches!(status.status.as_u16(), 401 | 403));
            EndpointHealth {
                state: if refused {
                    EndpointState::Unauthorized
                } else {
                    EndpointState::Unreachable
                },
                checked_at,
                detail: Some(error.to_string()),
                served_models: Vec::new(),
            }
        }
    }
}

/// What a model does, guessed from its id, for endpoints whose listing does not say.
pub fn guess_capabilities(kind: EndpointKind, id: &str) -> Vec<AiCapability> {
    let id = id.to_lowercase();
    let has = |words: &[&str]| words.iter().any(|word| id.contains(word));
    if kind == EndpointKind::Anthropic {
        vec![AiCapability::Chat]
    } else if has(&["embed", "bge-", "e5-", "minilm"]) {
        vec![AiCapability::Embeddings]
    } else if has(&["whisper", "parakeet", "transcribe", "canary"]) {
        vec![AiCapability::Transcription]
    } else if has(&["tts", "kokoro", "piper"]) {
        vec![AiCapability::Speech]
    } else if kind == EndpointKind::OpenAiAudio {
        vec![AiCapability::Transcription]
    } else {
        vec![AiCapability::Chat]
    }
}

/// What an endpoint serves: everything for OpenAI itself, chat for Anthropic, else what its
/// models do, or the kind's default when it lists none.
pub fn endpoint_capabilities(kind: EndpointKind, models: &[DiscoveredModel]) -> Vec<AiCapability> {
    if matches!(kind, EndpointKind::OpenAi | EndpointKind::Anthropic) {
        return kind.default_capabilities();
    }
    let mut capabilities: Vec<AiCapability> = models
        .iter()
        .flat_map(|model| model.capabilities.iter().copied())
        .collect();
    capabilities.sort();
    capabilities.dedup();
    if capabilities.is_empty() {
        kind.default_capabilities()
    } else {
        capabilities
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn model(id: &str, capabilities: Vec<AiCapability>) -> DiscoveredModel {
        DiscoveredModel {
            id: id.to_string(),
            capabilities,
            context_length: None,
        }
    }

    #[test]
    fn a_listing_is_reachable_with_the_ids_it_listed() {
        let health = health_of(Ok(vec![model("a", vec![]), model("b", vec![])]));
        assert_eq!(health.state, EndpointState::Reachable);
        assert_eq!(health.served_models, vec!["a", "b"]);
        assert!(health.checked_at.is_some());
    }

    #[test]
    fn a_refused_key_is_unauthorized_and_any_other_failure_unreachable() {
        let status = |code: u16| {
            anyhow::Error::from(UpstreamStatus {
                url: "http://h/v1/models".into(),
                status: reqwest::StatusCode::from_u16(code).unwrap(),
                body: "no".into(),
                retry_after: None,
            })
        };
        assert_eq!(
            health_of(Err(status(401))).state,
            EndpointState::Unauthorized
        );
        assert_eq!(
            health_of(Err(status(403))).state,
            EndpointState::Unauthorized
        );
        assert_eq!(
            health_of(Err(status(500))).state,
            EndpointState::Unreachable
        );
        let refused = health_of(Err(anyhow::anyhow!("connection refused")));
        assert_eq!(refused.state, EndpointState::Unreachable);
        assert_eq!(refused.detail.as_deref(), Some("connection refused"));
    }

    #[test]
    fn capabilities_are_guessed_from_the_model_id() {
        use AiCapability::*;
        let guess = |kind, id| guess_capabilities(kind, id);
        assert_eq!(guess(EndpointKind::Vllm, "BAAI/bge-m3"), vec![Embeddings]);
        assert_eq!(
            guess(EndpointKind::OpenAi, "text-embedding-3-small"),
            vec![Embeddings]
        );
        assert_eq!(
            guess(EndpointKind::OpenAiAudio, "Systran/faster-whisper-small"),
            vec![Transcription]
        );
        assert_eq!(
            guess(EndpointKind::OpenAiAudio, "speaches-ai/Kokoro-82M"),
            vec![Speech]
        );
        assert_eq!(
            guess(EndpointKind::OpenAiAudio, "some-asr"),
            vec![Transcription]
        );
        assert_eq!(guess(EndpointKind::LlamaServer, "qwen3-8b"), vec![Chat]);
        assert_eq!(
            guess(EndpointKind::Anthropic, "claude-embed-ish"),
            vec![Chat]
        );
    }

    #[test]
    fn an_endpoint_serves_what_its_models_do_or_its_kinds_default() {
        use AiCapability::*;
        let models = [model("a", vec![Chat]), model("b", vec![Embeddings, Chat])];
        assert_eq!(
            endpoint_capabilities(EndpointKind::Vllm, &models),
            vec![Chat, Embeddings]
        );
        assert_eq!(
            endpoint_capabilities(EndpointKind::Ollama, &[]),
            vec![Chat, Embeddings]
        );
        assert_eq!(
            endpoint_capabilities(EndpointKind::Anthropic, &models),
            vec![Chat]
        );
        assert_eq!(
            endpoint_capabilities(EndpointKind::OpenAi, &[]),
            vec![Chat, Embeddings, Transcription, Speech]
        );
    }
}
