//! `POST /v1/embeddings`.
//!
//! Returns raw `Vec<f32>` arrays per the OpenAI spec — the zlib+b64
//! wire format used by the native WS `ai.embed` path stays exclusive to
//! AD4M-native clients that consume it for bandwidth reasons.  External
//! SDKs (`openai`, LangChain, …) expect plain JSON numbers and that's
//! what they get here.

use axum::Json;

use super::errors::OpenAIJson;

use super::errors::{OpenAIError, OpenAIResult};
use super::model_selector::resolve_model;
use super::types::{EmbeddingItem, EmbeddingRequest, EmbeddingResponse, EmbeddingUsage};
use crate::agent::capabilities::check_capability;
use crate::api::auth::AuthContext;
use crate::types::ModelType;

pub async fn embeddings(
    auth: AuthContext,
    OpenAIJson(req): OpenAIJson<EmbeddingRequest>,
) -> OpenAIResult<Json<EmbeddingResponse>> {
    check_capability(
        &auth.capabilities,
        &crate::services::builtins::capability(
            crate::services::builtins::Builtin::AiInference,
            "PROMPT",
        ),
    )
    .map_err(OpenAIError::forbidden)?;

    if let Some(ref fmt) = req.encoding_format {
        if fmt != "float" {
            return Err(OpenAIError::invalid_request(format!(
                "Unsupported encoding_format \"{fmt}\"; only \"float\" is supported",
            )));
        }
    }

    let model_id = resolve_model(&req.model, ModelType::Embedding).await?;
    let model_id_response = req.model.clone();
    let inputs = req.input.into_vec();

    // The AI service charges per input, and the host checks credits before
    // each one. Check once up front too, so a caller without credits gets
    // 429 before any input is embedded and charged.
    let ctx = crate::services::ServiceHost::context_for_request(&auth.to_request_context());
    if !crate::services::builtins::billing::may_spend(&ctx) {
        return Err(OpenAIError::insufficient_quota(
            "Insufficient compute credits",
        ));
    }

    let mut data: Vec<EmbeddingItem> = Vec::with_capacity(inputs.len());
    let mut total_tokens: u64 = 0;
    let batch_started = std::time::Instant::now();
    let batch_n = inputs.len();

    for (index, text) in inputs.into_iter().enumerate() {
        let result = crate::services::builtins::ai::embedding(&ctx, &model_id, &text)
            .await
            .map_err(super::ai_error)?;
        total_tokens += result.token_count;
        data.push(EmbeddingItem {
            object: "embedding",
            index,
            embedding: result.vector,
        });
    }
    // Batch-level info replaces the per-call info in AIService::embed (now
    // debug). See rust-executor/LOGGING.md.
    log::info!(
        "🤖 embed batch model={} n={} tokens_in={} latency_total={}ms (openai-compat)",
        model_id,
        batch_n,
        total_tokens,
        batch_started.elapsed().as_millis()
    );

    Ok(Json(EmbeddingResponse {
        object: "list",
        data,
        model: model_id_response,
        usage: EmbeddingUsage {
            prompt_tokens: total_tokens,
            total_tokens,
        },
    }))
}
