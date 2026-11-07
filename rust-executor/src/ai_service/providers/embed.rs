//! Remote embeddings: one request per batch, to the endpoint that serves the model.
//!
//! OpenAI-shaped servers (OpenAI, llama-server `--embeddings`, vLLM, speaches) take
//! `POST {base}/v1/embeddings`; Ollama takes its native `POST /api/embed`, which reports
//! the input token count. Anthropic serves no embeddings.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::time::Duration;

use super::http::{credentialed_http, fetch_json, is_transport_safe, versioned_endpoint};
use super::ollama::ollama_root;
use crate::types::{Endpoint, EndpointKind};

/// How long an embedding request may take when the endpoint sets no timeout (plan §5.4).
pub const EMBED_TIMEOUT: Duration = Duration::from_secs(60);

/// The vectors of one batch, in the order of its inputs.
#[derive(Debug, Clone, PartialEq)]
pub struct EmbedReply {
    pub vectors: Vec<Vec<f32>>,
    /// The upstream's count of the input tokens, when it reports one.
    pub prompt_tokens: Option<u32>,
}

#[async_trait]
pub trait RemoteEmbed: Send + Sync {
    /// One request for the whole batch. Order kept.
    async fn embed(&self, model: &str, inputs: &[String]) -> Result<EmbedReply>;
}

/// The embedding client of `endpoint`. Refuses a key over plain HTTP, and an endpoint
/// whose protocol has no embeddings.
pub fn embed_client(endpoint: &Endpoint) -> Result<Box<dyn RemoteEmbed>> {
    if !endpoint.api_key.is_empty() && !is_transport_safe(&endpoint.base_url) {
        return Err(anyhow!(
            "{} ({})",
            super::http::CLEARTEXT_KEY_REFUSAL,
            endpoint.base_url
        ));
    }
    let timeout = crate::ai_service::endpoints::request_timeout(endpoint, EMBED_TIMEOUT);
    let http = credentialed_http(timeout)
        .build()
        .expect("the HTTP client builds");
    let api_key = endpoint.api_key.expose().to_string();
    match endpoint.kind {
        EndpointKind::Anthropic => Err(anyhow!(
            "Endpoint {:?} is an Anthropic endpoint, which serves no embeddings",
            endpoint.name
        )),
        EndpointKind::Ollama => Ok(Box::new(OllamaEmbed {
            http,
            url: format!("{}/api/embed", ollama_root(&endpoint.base_url)),
        })),
        _ => Ok(Box::new(OpenAiEmbed {
            http,
            api_key,
            url: versioned_endpoint(endpoint.base_url.clone(), "embeddings"),
        })),
    }
}

fn vector(value: &Value) -> Result<Vec<f32>> {
    value
        .as_array()
        .ok_or_else(|| anyhow!("an embedding is not an array"))?
        .iter()
        .map(|x| {
            x.as_f64()
                .map(|x| x as f32)
                .ok_or_else(|| anyhow!("an embedding holds a value that is not a number"))
        })
        .collect()
}

fn check_count(vectors: &[Vec<f32>], inputs: &[String], url: &str) -> Result<()> {
    if vectors.len() != inputs.len() {
        return Err(anyhow!(
            "{url} answered {} embedding(s) for {} input(s)",
            vectors.len(),
            inputs.len()
        ));
    }
    Ok(())
}

pub struct OpenAiEmbed {
    http: reqwest::Client,
    api_key: String,
    url: String,
}

#[async_trait]
impl RemoteEmbed for OpenAiEmbed {
    async fn embed(&self, model: &str, inputs: &[String]) -> Result<EmbedReply> {
        let mut request = self.http.post(&self.url).json(&json!({
            "model": model,
            "input": inputs,
            "encoding_format": "float",
        }));
        if !self.api_key.is_empty() {
            request = request.bearer_auth(&self.api_key);
        }
        let answer = fetch_json(request, &self.url, &self.api_key).await?;
        let mut items: Vec<(u64, Vec<f32>)> = answer
            .get("data")
            .and_then(Value::as_array)
            .ok_or_else(|| anyhow!("{} answered no `data` array", self.url))?
            .iter()
            .enumerate()
            .map(|(position, item)| {
                let index = item
                    .get("index")
                    .and_then(Value::as_u64)
                    .unwrap_or(position as u64);
                Ok((
                    index,
                    vector(item.get("embedding").unwrap_or(&Value::Null))?,
                ))
            })
            .collect::<Result<_>>()?;
        items.sort_by_key(|(index, _)| *index);
        let vectors: Vec<Vec<f32>> = items.into_iter().map(|(_, v)| v).collect();
        check_count(&vectors, inputs, &self.url)?;
        Ok(EmbedReply {
            vectors,
            prompt_tokens: answer
                .pointer("/usage/prompt_tokens")
                .and_then(Value::as_u64)
                .map(|n| n as u32),
        })
    }
}

/// Ollama runs without credentials, so this client sends none.
pub struct OllamaEmbed {
    http: reqwest::Client,
    url: String,
}

#[async_trait]
impl RemoteEmbed for OllamaEmbed {
    async fn embed(&self, model: &str, inputs: &[String]) -> Result<EmbedReply> {
        let request = self
            .http
            .post(&self.url)
            .json(&json!({ "model": model, "input": inputs }));
        let answer = fetch_json(request, &self.url, "").await?;
        let vectors: Vec<Vec<f32>> = answer
            .get("embeddings")
            .and_then(Value::as_array)
            .ok_or_else(|| anyhow!("{} answered no `embeddings` array", self.url))?
            .iter()
            .map(vector)
            .collect::<Result<_>>()?;
        check_count(&vectors, inputs, &self.url)?;
        Ok(EmbedReply {
            vectors,
            prompt_tokens: answer
                .get("prompt_eval_count")
                .and_then(Value::as_u64)
                .map(|n| n as u32),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::secret::Secret;
    use crate::types::{AiCapability, EndpointHealth};
    use mockito::Matcher;

    fn endpoint(kind: EndpointKind, url: &str, key: &str) -> Endpoint {
        Endpoint {
            id: "e".into(),
            name: "e".into(),
            kind,
            base_url: url::Url::parse(url).unwrap(),
            api_key: Secret::new(key),
            capabilities: vec![AiCapability::Embeddings],
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
            health: EndpointHealth::default(),
        }
    }

    fn texts(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| s.to_string()).collect()
    }

    #[tokio::test]
    async fn an_openai_shaped_batch_is_one_request_and_comes_back_in_input_order() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/embeddings")
            .match_header("authorization", "Bearer sk-embed")
            .match_body(Matcher::Json(json!({
                "model": "text-embedding-3-small",
                "input": ["a", "b"],
                "encoding_format": "float",
            })))
            .with_body(
                json!({
                    "data": [
                        {"index": 1, "embedding": [0.0, 1.0]},
                        {"index": 0, "embedding": [1.0, 0.0]},
                    ],
                    "usage": {"prompt_tokens": 7, "total_tokens": 7},
                })
                .to_string(),
            )
            .expect(1)
            .create_async()
            .await;

        let client =
            embed_client(&endpoint(EndpointKind::OpenAi, &server.url(), "sk-embed")).unwrap();
        let reply = client
            .embed("text-embedding-3-small", &texts(&["a", "b"]))
            .await
            .unwrap();

        mock.assert_async().await;
        assert_eq!(reply.vectors, vec![vec![1.0, 0.0], vec![0.0, 1.0]]);
        assert_eq!(reply.prompt_tokens, Some(7));
    }

    #[tokio::test]
    async fn ollama_embeds_through_its_native_api_and_reports_the_token_count() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/api/embed")
            .match_body(Matcher::Json(
                json!({ "model": "nomic-embed-text", "input": ["a", "b"] }),
            ))
            .with_body(
                json!({
                    "model": "nomic-embed-text",
                    "embeddings": [[0.5, 0.5], [0.25, 0.75]],
                    "prompt_eval_count": 4,
                })
                .to_string(),
            )
            .expect(1)
            .create_async()
            .await;

        // A saved `/v1` suffix is the OpenAI-compat path; the native API is at the root.
        let base = format!("{}/v1", server.url());
        let client = embed_client(&endpoint(EndpointKind::Ollama, &base, "")).unwrap();
        let reply = client
            .embed("nomic-embed-text", &texts(&["a", "b"]))
            .await
            .unwrap();

        mock.assert_async().await;
        assert_eq!(reply.vectors, vec![vec![0.5, 0.5], vec![0.25, 0.75]]);
        assert_eq!(reply.prompt_tokens, Some(4));
    }

    #[tokio::test]
    async fn a_reply_with_the_wrong_number_of_vectors_is_an_error() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/api/embed")
            .with_body(json!({ "embeddings": [[1.0]] }).to_string())
            .create_async()
            .await;
        let client = embed_client(&endpoint(EndpointKind::Ollama, &server.url(), "")).unwrap();
        let error = client.embed("m", &texts(&["a", "b"])).await.unwrap_err();
        assert!(
            error.to_string().contains("1 embedding(s) for 2"),
            "{error}"
        );
    }

    #[test]
    fn anthropic_and_a_key_over_plain_http_get_no_client() {
        let anthropic = endpoint(EndpointKind::Anthropic, "https://api.anthropic.com", "k");
        assert!(embed_client(&anthropic).is_err());
        let cleartext = endpoint(EndpointKind::OpenAi, "http://203.0.113.9:8000", "k");
        assert!(embed_client(&cleartext).is_err());
    }
}
