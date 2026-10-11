//! axum router for the OpenAI-compatible surface.
//!
//! Mount under `/v1` from the top-level [`api_router`](super::super::api_router).
//! All endpoints share the existing [`AppState`](crate::api::auth::AppState)
//! so JWT extraction, capabilities, and admin credentials work the same
//! way as the native WS-RPC surface.

use axum::{
    extract::DefaultBodyLimit,
    routing::{get, post},
    Router,
};

use super::{audio, chat, embeddings, models, realtime};
use crate::api::auth::AppState;

/// Build the `/v1` router.  Mounted by [`super::super::api_router`].
///
/// The router is intentionally minimal — every endpoint delegates to a
/// dedicated handler module so reviewers can audit one phase at a time.
/// Body limit is bumped on audio routes to accommodate file uploads
/// (defaults to 2 MiB in axum, which is small for whisper inputs).
pub fn router() -> Router<AppState> {
    Router::new()
        .route("/models", get(models::list_models))
        .route("/chat/completions", post(chat::chat_completions))
        .route("/completions", post(chat::completions))
        .route("/embeddings", post(embeddings::embeddings))
        .route(
            "/audio/transcriptions",
            post(audio::transcriptions).layer(DefaultBodyLimit::max(50 * 1024 * 1024)),
        )
        .route("/audio/speech", post(audio::speech))
        .route("/realtime", get(realtime::realtime_ws))
}
