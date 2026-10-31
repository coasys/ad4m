//! `POST /v1/embeddings`.
//!
//! Returns raw `Vec<f32>` arrays per the OpenAI spec — the zlib+b64
//! wire format used by the native WS `ai.embed` path stays exclusive to
//! AD4M-native clients that consume it for bandwidth reasons.  External
//! SDKs (`openai`, LangChain, …) expect plain JSON numbers and that's
//! what they get here.
//!
//! The batch is one call to [`AIService::embed_batch`], as `ai.embed` and semantic dedup
//! make it: one request to the model's endpoint.

use axum::Json;

use super::errors::OpenAIJson;

use super::errors::{OpenAIError, OpenAIResult};
use super::model_selector::resolve;
use super::passthrough::upstream_failure;
use super::types::{EmbeddingItem, EmbeddingRequest, EmbeddingResponse, EmbeddingUsage};
use crate::agent::capabilities::AI_PROMPT_CAPABILITY;
use crate::ai_service::AIService;
use crate::billing::check_compute_credits;
use crate::types::{EndpointKind, ModelType, RequestContext};

pub async fn embeddings(
    auth: RequestContext,
    OpenAIJson(req): OpenAIJson<EmbeddingRequest>,
) -> OpenAIResult<Json<EmbeddingResponse>> {
    auth.require(&AI_PROMPT_CAPABILITY)
        .map_err(OpenAIError::forbidden)?;

    if let Some(ref fmt) = req.encoding_format {
        if fmt != "float" {
            return Err(OpenAIError::invalid_request(format!(
                "Unsupported encoding_format \"{fmt}\"; only \"float\" is supported",
            )));
        }
    }

    let resolved = resolve(&req.model, ModelType::Embedding).await?;
    if resolved.endpoint.kind == EndpointKind::Anthropic {
        return Err(OpenAIError::model_not_supported(format!(
            "Model '{}' is on an Anthropic endpoint, which serves no embeddings",
            req.model
        )));
    }
    let inputs = req.input.into_vec();

    if let Some(agent_did) = auth.user_did() {
        check_compute_credits(agent_did)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    let batch_started = std::time::Instant::now();
    let batch_n = inputs.len();
    let batch = service
        .embed_batch(
            &resolved.model.id,
            inputs,
            auth.user_did().map(str::to_string),
        )
        .await
        .map_err(|e| upstream_failure(&resolved.endpoint, &resolved.model.remote_model, e))?;
    // One info line per batch; see rust-executor/LOGGING.md.
    log::info!(
        "🤖 embed batch model={} n={} tokens_in={} latency_total={}ms (openai-compat)",
        resolved.model.name,
        batch_n,
        batch.token_count,
        batch_started.elapsed().as_millis()
    );

    let total_tokens = batch.token_count as u64;
    Ok(Json(EmbeddingResponse {
        object: "list",
        data: batch
            .vectors
            .into_iter()
            .enumerate()
            .map(|(index, embedding)| EmbeddingItem {
                object: "embedding",
                index,
                embedding,
            })
            .collect(),
        model: req.model,
        usage: EmbeddingUsage {
            prompt_tokens: total_tokens,
            total_tokens,
        },
    }))
}
