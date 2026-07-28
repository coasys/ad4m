//! The model seam.
//!
//! The run loop streams model output through [`ModelBackend`] rather than
//! calling [`AIService`] directly, so it can be driven by the real local/remote
//! model in production and by recorded fixtures in tests. The production impl
//! ([`AiServiceBackend`]) is a thin pass-through: it behaves exactly as a
//! direct `AIService::prompt_messages_stream` call did — same model, same
//! constrained decoding, same token stream — the only difference being that the
//! completion oneshot is drained on a detached task (the loop already treats
//! the token channel closing as completion, so this is behaviourally identical).

use std::future::Future;
use std::pin::Pin;

use anyhow::{anyhow, Result};
use kalosm::language::ArcParser;
use tokio::sync::mpsc;

use crate::ai_service::AIService;

/// A receiver of output *tokens*. The stream is complete when the channel
/// closes — the same signal the loop already keys off.
pub type TokenStream = mpsc::UnboundedReceiver<String>;

/// Abstraction over the LLM used by the run loop.
pub trait ModelBackend: Send + Sync {
    /// Start a streaming completion. Mirrors the token half of
    /// `AIService::prompt_messages_stream`.
    fn stream(
        &self,
        model_id: String,
        messages: Vec<(String, String)>,
        constraint: Option<ArcParser<()>>,
    ) -> Pin<Box<dyn Future<Output = Result<TokenStream>> + Send + '_>>;
}

/// Production backend: the real local/remote model via the global [`AIService`].
pub struct AiServiceBackend;

impl ModelBackend for AiServiceBackend {
    fn stream(
        &self,
        model_id: String,
        messages: Vec<(String, String)>,
        constraint: Option<ArcParser<()>>,
    ) -> Pin<Box<dyn Future<Output = Result<TokenStream>> + Send + '_>> {
        Box::pin(async move {
            let ai = AIService::global_instance()
                .await
                .map_err(|e| anyhow!("AI service unavailable: {e}"))?;
            let (token_rx, done_rx) = ai
                .prompt_messages_stream(model_id, messages, constraint)
                .await
                .map_err(|e| anyhow!("model stream failed: {e}"))?;
            // The token channel closing already signals completion to the loop;
            // drain the completion oneshot (token counts unused) on a detached
            // task so lifecycle/cleanup runs without blocking the caller.
            tokio::spawn(async move {
                let _ = done_rx.await;
            });
            Ok(token_rx)
        })
    }
}
