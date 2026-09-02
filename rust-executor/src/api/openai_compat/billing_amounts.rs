//! Pure functions that compute the credit amount billed by each `/v1`
//! endpoint that bills at the handler level. Extracted from the handlers so
//! the formulas are:
//!   - easy to read in one place (single source of truth per endpoint),
//!   - unit-testable without spinning up a runtime or mocking `bill_compute`.
//!
//! Handlers MUST route their amount calculation through these helpers and
//! pass the result to `bill_compute` — do NOT inline a different formula.
//!
//! Operation labels used with `bill_compute`:
//!   - `"ai_tts"` → speech
//!   - `"ai_transcription"` → NOT billed at the handler level. Transcription
//!     is billed exactly once in the worker (`ai_service/mod.rs::open_transcription_stream`).
//!     This layout was a deliberate fix for round-1 review finding #2
//!     (double-billing). If you add a `bill_compute("ai_transcription", ...)`
//!     call in `audio::transcriptions`, the `no_bill_compute_in_transcriptions_handler`
//!     test will fail — that's the guard.
//!
//! Chat/completions (streaming and non-streaming) and embeddings are billed
//! by `AIService` via host_rates (`crate::billing::bill_ai_operation`), not
//! here.

/// TTS speech: characters synthesised / 1000, floored at 1.0 so any
/// synthesis bills at least one credit. Aligns with OpenAI's own
/// per-character TTS billing model.
pub(super) fn speech_amount(char_count: usize) -> f64 {
    (char_count as f64 / 1000.0).max(1.0)
}
