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
//! * `GET  /v1/models`               — registered models, across their endpoints
//! * `POST /v1/chat/completions`     — stateless chat, with optional SSE streaming
//! * `POST /v1/completions`          — legacy text completion
//! * `POST /v1/embeddings`           — raw `f32` arrays (no zlib+b64)
//! * `POST /v1/audio/transcriptions` — batch multipart whisper
//! * `POST /v1/audio/speech`         — TTS, forwarded to a `SPEECH` model's endpoint
//! * `GET  /v1/realtime`             — WS, OpenAI Realtime-style streaming STT
//!
//! A chat or text completion for a model on an OpenAI-shaped endpoint is forwarded to
//! it as the client wrote it ([`passthrough`]); the executor answers the rest itself.
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
pub mod passthrough;
pub mod realtime;
pub mod router;
pub mod tool_grammar;
pub mod types;

pub use router::router;

#[cfg(test)]
mod passthrough_tests;
#[cfg(test)]
mod tests;
