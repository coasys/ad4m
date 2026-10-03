//! OpenAI-compatible HTTP/WS API surface mounted at `/v1`.
//!
//! Translates the OpenAI JSON schema to and from the existing
//! [`crate::ai_service::AIService`].  The native WS-RPC `ai.*` methods are
//! unchanged; this module is purely additive so existing clients keep
//! working.
//!
//! Surface covered (matches the proposal under
//! `~/workspaces/coasys/.specs/PROPOSAL_AI_OPENAI_COMPATIBLE_ENDPOINT.md`):
//!
//! * `GET  /v1/models`               — list registered models
//! * `POST /v1/chat/completions`     — stateless chat, with optional SSE streaming
//! * `POST /v1/completions`          — legacy text-completion shim
//! * `POST /v1/embeddings`           — raw `f32` arrays (no zlib+b64)
//! * `POST /v1/audio/transcriptions` — batch multipart whisper
//! * `POST /v1/audio/speech`         — TTS (remote OpenAI-compatible
//!                                     passthrough; local TTS backend is
//!                                     scoped to a follow-up)
//! * `GET  /v1/realtime`             — WS, OpenAI Realtime-style streaming STT
//!
//! Auth + billing reuse the existing JWT extractor and `bill_compute`.
//! Errors are wrapped in the canonical
//! `{ "error": { message, type, param, code } }` envelope.

pub mod audio;
pub(crate) mod billing_amounts;
pub mod chat;
pub mod embeddings;
pub mod errors;
pub mod harness_bridge;
pub mod model_selector;
pub mod models;
pub mod native_tools;
pub mod realtime;
pub mod router;
pub mod tool_grammar;
pub mod tts_passthrough;
pub mod types;

pub use router::router;

#[cfg(test)]
mod tests;

/// Refuse compute for an account without credits (through `billing.ledger`).
pub(crate) async fn require_credits(email: &str) -> Result<(), errors::OpenAIError> {
    if crate::services::builtins::billing::check_user("openai_compat", email).await {
        Ok(())
    } else {
        Err(errors::OpenAIError::insufficient_quota(
            "Insufficient compute credits",
        ))
    }
}

/// A failed `billing.ledger` charge as the OpenAI error a client expects:
/// 429 `insufficient_quota` when credits ran out, otherwise a generic 500.
/// The ledger's message can name the account, so it goes to the log only.
pub(crate) fn charge_error(e: crate::api::ws_handler::WsRpcError) -> errors::OpenAIError {
    let insufficient = e
        .data
        .as_ref()
        .and_then(|d| d.get("name"))
        .and_then(|n| n.as_str())
        == Some("InsufficientCredits");
    if insufficient {
        errors::OpenAIError::insufficient_quota("Insufficient compute credits")
    } else {
        log::error!("billing failed: {} ({})", e.message, e.code);
        errors::OpenAIError::internal("Billing operation failed")
    }
}

/// A failed `ai.inference` / `ai.models` call as the OpenAI error a client
/// expects.
pub(crate) fn ai_error(e: crate::api::ws_handler::WsRpcError) -> errors::OpenAIError {
    match e.code {
        402 => errors::OpenAIError::insufficient_quota("Insufficient compute credits"),
        400 | 422 => errors::OpenAIError::invalid_request(e.message),
        403 => errors::OpenAIError::forbidden(e.message),
        _ => errors::OpenAIError::internal(e.message),
    }
}
