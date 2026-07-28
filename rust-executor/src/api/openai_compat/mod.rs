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
pub mod model_selector;
pub mod models;
pub mod realtime;
pub mod router;
pub mod tool_grammar;
pub mod tts_passthrough;
pub mod types;

pub use router::router;

#[cfg(test)]
mod tests;
