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
//!   - `"ai_transcription"` → NOT billed here. Transcription is billed once per
//!     transcript in `ai_service::transcription::bill_transcription`, which both the
//!     streams and `audio::transcriptions` call. This layout was a deliberate fix for
//!     round-1 review finding #2 (double-billing); `transcription_is_billed_in_one_place`
//!     fails if the label appears anywhere else in the transcription code.
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
