//! Which model servers run on this machine (`ai.detectEndpoints`, plan §3.2).
//!
//! Probes the usual ports on `127.0.0.1` and `[::1]` and classifies each answer by its
//! shape, never by the port alone: 8000 and 8080 are popular with unrelated services.
//! Nothing is saved; the operator decides what to register.

use serde_json::Value;
use std::time::Duration;
use url::Url;

use crate::ai_service::providers::discovery::{
    discover_for, endpoint_capabilities, guess_capabilities,
};
use crate::ai_service::providers::http::credentialed_http;
use crate::types::{AiCapability, DetectedEndpoint, EndpointKind};

/// Per request. A local server answers in milliseconds; a closed port refuses at once.
const PROBE_TIMEOUT: Duration = Duration::from_millis(500);

/// Ollama; llama-server (default, and the Docker examples' 8080, which whisper.cpp also
/// uses); vLLM and speaches; the Parakeet server.
const PORTS: [u16; 5] = [11434, 9931, 8080, 8000, 5092];

/// IPv4 first: a server on both answers once, as its IPv4 address.
const HOSTS: [&str; 2] = ["127.0.0.1", "[::1]"];

/// The model servers on this machine's loopback ports.
pub async fn detect_endpoints() -> Vec<DetectedEndpoint> {
    let candidates = HOSTS
        .iter()
        .flat_map(|host| {
            PORTS.iter().map(move |port| {
                Url::parse(&format!("http://{host}:{port}")).expect("a loopback URL parses")
            })
        })
        .collect();
    detect_on(candidates).await
}

/// The model servers among `candidates`, one per port and kind, in candidate order.
pub async fn detect_on(candidates: Vec<Url>) -> Vec<DetectedEndpoint> {
    let found = futures::future::join_all(candidates.into_iter().map(classify)).await;
    let mut seen = Vec::new();
    found
        .into_iter()
        .flatten()
        .filter(|endpoint| {
            let key = (endpoint.base_url.port(), endpoint.kind);
            let first = !seen.contains(&key);
            seen.push(key);
            first
        })
        .collect()
}

async fn get_json(http: &reqwest::Client, url: String) -> Option<Value> {
    let response = http.get(url).send().await.ok()?;
    if !response.status().is_success() {
        return None;
    }
    response.json().await.ok()
}

/// What answers at `base_url`, by the shape of its answers: Ollama's `/api/version`,
/// llama-server's `/props`, else an OpenAI-shaped `/v1/models` (vLLM marks its models
/// `owned_by: "vllm"`; a server that lists only speech models is an audio server).
pub async fn classify(base_url: Url) -> Option<DetectedEndpoint> {
    let http = credentialed_http(PROBE_TIMEOUT).build().ok()?;
    let root = base_url.as_str().trim_end_matches('/').to_string();

    let kind = if get_json(&http, format!("{root}/api/version"))
        .await
        .is_some_and(|v| v.get("version").is_some())
    {
        EndpointKind::Ollama
    } else if get_json(&http, format!("{root}/props"))
        .await
        .is_some_and(|v| v.is_object())
    {
        EndpointKind::LlamaServer
    } else {
        let listing = get_json(&http, format!("{root}/v1/models")).await?;
        let entries = listing.get("data")?.as_array()?;
        let owned_by_vllm = entries
            .iter()
            .any(|e| e.get("owned_by").and_then(Value::as_str) == Some("vllm"));
        let speech_only = !entries.is_empty()
            && entries.iter().all(|e| {
                e.get("id").and_then(Value::as_str).is_some_and(|id| {
                    guess_capabilities(EndpointKind::OpenAiCompatible, id)
                        .iter()
                        .all(|c| matches!(c, AiCapability::Transcription | AiCapability::Speech))
                })
            });
        if owned_by_vllm {
            EndpointKind::Vllm
        } else if speech_only {
            EndpointKind::OpenAiAudio
        } else {
            EndpointKind::OpenAiCompatible
        }
    };

    let models = discover_for(kind, base_url.clone(), "", PROBE_TIMEOUT)
        .list_models()
        .await
        .unwrap_or_default();
    Some(DetectedEndpoint {
        kind,
        capabilities: endpoint_capabilities(kind, &models),
        served_models: models.into_iter().map(|model| model.id).collect(),
        base_url,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    async fn serving(routes: &[(&str, &str, Value)]) -> mockito::ServerGuard {
        let mut server = mockito::Server::new_async().await;
        for (method, path, body) in routes {
            server
                .mock(method, *path)
                .with_status(200)
                .with_body(body.to_string())
                .create_async()
                .await;
        }
        server
    }

    async fn kind_of(server: &mockito::ServerGuard) -> Option<EndpointKind> {
        classify(Url::parse(&server.url()).unwrap())
            .await
            .map(|endpoint| endpoint.kind)
    }

    #[tokio::test]
    async fn ollama_is_recognised_by_its_version_answer_and_lists_its_models() {
        let server = serving(&[
            ("GET", "/api/version", json!({ "version": "0.12.0" })),
            (
                "GET",
                "/api/tags",
                json!({ "models": [{ "name": "qwen3:8b" }] }),
            ),
            (
                "POST",
                "/api/show",
                json!({ "capabilities": ["completion"] }),
            ),
        ])
        .await;
        let found = classify(Url::parse(&server.url()).unwrap()).await.unwrap();
        assert_eq!(found.kind, EndpointKind::Ollama);
        assert_eq!(found.served_models, vec!["qwen3:8b"]);
        assert_eq!(found.capabilities, vec![AiCapability::Chat]);
    }

    #[tokio::test]
    async fn llama_server_is_recognised_by_props() {
        let server = serving(&[
            (
                "GET",
                "/props",
                json!({ "default_generation_settings": { "n_ctx": 4096 } }),
            ),
            (
                "GET",
                "/v1/models",
                json!({ "data": [{ "id": "gemma-3-4b" }] }),
            ),
        ])
        .await;
        assert_eq!(kind_of(&server).await, Some(EndpointKind::LlamaServer));
    }

    #[tokio::test]
    async fn vllm_speaches_and_other_openai_servers_are_told_apart_by_their_listing() {
        let vllm = serving(&[(
            "GET",
            "/v1/models",
            json!({ "data": [{ "id": "Qwen/Qwen3-8B", "owned_by": "vllm" }] }),
        )])
        .await;
        assert_eq!(kind_of(&vllm).await, Some(EndpointKind::Vllm));

        let speaches = serving(&[(
            "GET",
            "/v1/models",
            json!({ "data": [{ "id": "Systran/faster-whisper-small" }, { "id": "speaches-ai/Kokoro-82M" }] }),
        )])
        .await;
        let found = classify(Url::parse(&speaches.url()).unwrap())
            .await
            .unwrap();
        assert_eq!(found.kind, EndpointKind::OpenAiAudio);
        assert_eq!(
            found.capabilities,
            vec![AiCapability::Transcription, AiCapability::Speech]
        );

        let other = serving(&[(
            "GET",
            "/v1/models",
            json!({ "data": [{ "id": "mistral" }] }),
        )])
        .await;
        assert_eq!(kind_of(&other).await, Some(EndpointKind::OpenAiCompatible));
    }

    #[tokio::test]
    async fn a_server_that_is_no_model_server_or_a_closed_port_is_not_detected() {
        let web = serving(&[("GET", "/", json!({ "hello": "world" }))]).await;
        assert_eq!(kind_of(&web).await, None);
        assert_eq!(kind_of_url("http://127.0.0.1:1").await, None);
    }

    async fn kind_of_url(url: &str) -> Option<EndpointKind> {
        classify(Url::parse(url).unwrap()).await.map(|e| e.kind)
    }

    #[tokio::test]
    async fn detection_keeps_one_answer_per_server_and_skips_silent_ports() {
        let ollama = serving(&[
            ("GET", "/api/version", json!({ "version": "0.12.0" })),
            ("GET", "/api/tags", json!({ "models": [] })),
        ])
        .await;
        let url = Url::parse(&ollama.url()).unwrap();
        let found = detect_on(vec![
            url.clone(),
            url.clone(),
            Url::parse("http://127.0.0.1:1").unwrap(),
        ])
        .await;
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].base_url, url);
        assert_eq!(
            found[0].capabilities,
            EndpointKind::Ollama.default_capabilities()
        );
    }
}
