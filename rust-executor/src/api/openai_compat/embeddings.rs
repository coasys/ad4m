//! `POST /v1/embeddings`.
//!
//! Returns raw `Vec<f32>` arrays per the OpenAI spec — the zlib+b64
//! wire format used by the native WS `ai.embed` path stays exclusive to
//! AD4M-native clients that consume it for bandwidth reasons.  External
//! SDKs (`openai`, LangChain, …) expect plain JSON numbers and that's
//! what they get here.

use axum::Json;

use super::errors::{OpenAIError, OpenAIResult};
use super::model_selector::resolve_model;
use super::types::{EmbeddingItem, EmbeddingRequest, EmbeddingResponse, EmbeddingUsage};
use crate::agent::capabilities::{check_capability, AI_PROMPT_CAPABILITY};
use crate::ai_service::AIService;
use crate::api::auth::AuthContext;
use crate::billing::{bill_compute, check_compute_credits};
use crate::types::ModelType;

pub async fn embeddings(
    auth: AuthContext,
    Json(req): Json<EmbeddingRequest>,
) -> OpenAIResult<Json<EmbeddingResponse>> {
    check_capability(&auth.capabilities, &AI_PROMPT_CAPABILITY).map_err(OpenAIError::forbidden)?;

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

    if let Some(email) = crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
    {
        check_compute_credits(&email)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }

    let service = AIService::global_instance()
        .await
        .map_err(|e| OpenAIError::internal(e.to_string()))?;

    let mut data: Vec<EmbeddingItem> = Vec::with_capacity(inputs.len());
    let mut total_tokens: u64 = 0;

    for (index, text) in inputs.into_iter().enumerate() {
        let result = service
            .embed(model_id.clone(), text)
            .await
            .map_err(|e| OpenAIError::internal(e.to_string()))?;
        total_tokens += result.token_count as u64;
        data.push(EmbeddingItem {
            object: "embedding",
            index,
            embedding: result.embeddings,
        });
    }

    if let Some(email) = crate::agent::capabilities::user_email_from_token(auth.auth_token.clone())
    {
        let _ = bill_compute(&email, 1.0, "ai_embedding", Some("v1/embeddings"));
    }

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
