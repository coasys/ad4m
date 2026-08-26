//! Pure functions that compute the credit amount billed by each `/v1`
//! endpoint. Extracted from the handlers so the formulas are:
//!   - easy to read in one place (single source of truth per endpoint),
//!   - unit-testable without spinning up a runtime or mocking `bill_compute`,
//!   - trivial to swap in real Kalosm/BPE token counts once available.
//!
//! Handlers MUST route their amount calculation through these helpers and
//! pass the result to `bill_compute` — do NOT inline a different formula.
//!
//! Operation labels used with `bill_compute`:
//!   - `"ai_prompt"` → chat/completions oneshot + streaming + legacy completions
//!   - `"ai_embedding"` → embeddings
//!   - `"ai_tts"` → speech
//!   - `"ai_transcription"` → NOT billed at the handler level. Transcription
//!     is billed exactly once in the worker (`ai_service/mod.rs::open_transcription_stream`).
//!     This layout was a deliberate fix for round-1 review finding #2
//!     (double-billing). If you add a `bill_compute("ai_transcription", ...)`
//!     call in `audio::transcriptions`, the `no_bill_compute_in_transcriptions_handler`
//!     test will fail — that's the guard.
//!
//! Streaming caveat: `stream_prompt` returns a flat `1.0` today because
//! Kalosm doesn't hand back token counts on the streaming path. TODO to
//! plumb real counts when the backend exposes them.

/// Per-request minimum charge so a genuinely tiny prompt still records
/// activity in the compute log. `bill_compute` clamps this again in
/// aggregate, but keeping the floor at the call site makes the invariant
/// visible in the formula.
pub(super) const MIN_PROMPT_BILL: f64 = 0.001;

/// Non-streaming chat / legacy completions: proportional to
/// `(prompt_tokens + completion_tokens) / 1000.0`, floored at
/// `MIN_PROMPT_BILL` so tiny requests still bill something.
pub(super) fn chat_or_completion_amount(prompt_tokens: usize, completion_tokens: usize) -> f64 {
    let total = (prompt_tokens + completion_tokens) as f64;
    (total / 1000.0).max(MIN_PROMPT_BILL)
}

/// Streaming chat: flat `1.0` for now. Kalosm's streaming path doesn't
/// yield per-token counts; when it does, switch to
/// `chat_or_completion_amount(prompt_tokens, streamed_completion_tokens)`.
pub(super) fn stream_prompt_amount() -> f64 {
    1.0
}

/// Embeddings: one credit per returned embedding vector, minimum one.
/// A single call with N inputs bills N credits so batching stays fair.
pub(super) fn embedding_amount(vector_count: usize) -> f64 {
    vector_count.max(1) as f64
}

/// TTS speech: characters synthesised / 1000, floored at 1.0 so any
/// synthesis bills at least one credit. Aligns with OpenAI's own
/// per-character TTS billing model.
pub(super) fn speech_amount(char_count: usize) -> f64 {
    (char_count as f64 / 1000.0).max(1.0)
}
