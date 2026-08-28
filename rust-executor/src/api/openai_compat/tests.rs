use super::audio::{audio_decode, decode_pcm_wav};
use super::billing_amounts::speech_amount;
use super::errors::OpenAIError;
use super::realtime::pcm16_to_f32;
use super::types::*;
use crate::ai_service::estimate_token_count;
use crate::billing::BillingError;

// ---------------------------------------------------------------------------
// ChatMessageContent::flatten_to_text
// ---------------------------------------------------------------------------

#[test]
fn flatten_text_string() {
    let content = ChatMessageContent::Text("hello world".to_string());
    assert_eq!(content.flatten_to_text(), "hello world");
}

#[test]
fn flatten_text_parts() {
    let content = ChatMessageContent::Parts(vec![
        ContentPart::Text {
            text: "line one".to_string(),
        },
        ContentPart::ImageUrl {
            image_url: serde_json::json!({"url": "data:image/png;base64,..."}),
        },
        ContentPart::Text {
            text: "line two".to_string(),
        },
    ]);
    assert_eq!(content.flatten_to_text(), "line one\nline two");
}

#[test]
fn flatten_text_empty_parts() {
    let content = ChatMessageContent::Parts(vec![]);
    assert_eq!(content.flatten_to_text(), "");
}

// ---------------------------------------------------------------------------
// PromptInput::into_single
// ---------------------------------------------------------------------------

#[test]
fn prompt_input_single_string() {
    let input = PromptInput::One("hello".to_string());
    assert_eq!(input.into_single().unwrap(), "hello");
}

#[test]
fn prompt_input_single_element_vec() {
    let input = PromptInput::Many(vec!["hello".to_string()]);
    assert_eq!(input.into_single().unwrap(), "hello");
}

#[test]
fn prompt_input_multi_element_vec_rejected() {
    let input = PromptInput::Many(vec!["a".to_string(), "b".to_string()]);
    assert!(input.into_single().is_err());
}

#[test]
fn prompt_input_empty_vec_rejected() {
    let input = PromptInput::Many(vec![]);
    assert!(input.into_single().is_err());
}

// ---------------------------------------------------------------------------
// EmbeddingInput::into_vec
// ---------------------------------------------------------------------------

#[test]
fn embedding_input_single() {
    let input = EmbeddingInput::One("text".to_string());
    assert_eq!(input.into_vec(), vec!["text".to_string()]);
}

#[test]
fn embedding_input_batch() {
    let input = EmbeddingInput::Many(vec!["a".to_string(), "b".to_string()]);
    assert_eq!(input.into_vec(), vec!["a".to_string(), "b".to_string()]);
}

// ---------------------------------------------------------------------------
// Error envelope serialization
// ---------------------------------------------------------------------------

#[test]
fn error_envelope_format() {
    let err = OpenAIError::invalid_request("bad input");
    let body = serde_json::to_value(&err).unwrap();
    let error = &body["error"];
    assert_eq!(error["message"], "bad input");
    assert_eq!(error["type"], "invalid_request_error");
    assert_eq!(error["code"], "invalid_request");
    assert!(error["param"].is_null());
}

#[test]
fn insufficient_quota_error() {
    let err = OpenAIError::insufficient_quota("no credits");
    assert_eq!(err.status, axum::http::StatusCode::TOO_MANY_REQUESTS);
    let body = serde_json::to_value(&err).unwrap();
    assert_eq!(body["error"]["code"], "insufficient_quota");
}

// ---------------------------------------------------------------------------
// Type deserialization
// ---------------------------------------------------------------------------

#[test]
fn chat_completion_request_minimal() {
    let json = serde_json::json!({
        "model": "gpt-4",
        "messages": [
            {"role": "user", "content": "Hello"}
        ]
    });
    let req: ChatCompletionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model, "gpt-4");
    assert!(!req.stream);
    assert_eq!(req.messages.len(), 1);
}

#[test]
fn chat_completion_request_with_parts() {
    let json = serde_json::json!({
        "model": "gpt-4",
        "messages": [{
            "role": "user",
            "content": [
                {"type": "text", "text": "describe this"},
                {"type": "image_url", "image_url": {"url": "https://example.com/img.png"}}
            ]
        }]
    });
    let req: ChatCompletionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(
        req.messages[0].content.as_ref().unwrap().flatten_to_text(),
        "describe this"
    );
}

#[test]
fn completion_request_string_prompt() {
    let json = serde_json::json!({
        "model": "davinci",
        "prompt": "Once upon a time"
    });
    let req: CompletionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.prompt.into_single().unwrap(), "Once upon a time");
}

#[test]
fn completion_request_array_prompt() {
    let json = serde_json::json!({
        "model": "davinci",
        "prompt": ["hello"]
    });
    let req: CompletionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.prompt.into_single().unwrap(), "hello");
}

#[test]
fn embedding_request_with_encoding_format() {
    let json = serde_json::json!({
        "model": "text-embedding-ada-002",
        "input": "search query",
        "encoding_format": "float"
    });
    let req: EmbeddingRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.encoding_format.as_deref(), Some("float"));
}

#[test]
fn embedding_request_without_encoding_format() {
    let json = serde_json::json!({
        "model": "text-embedding-ada-002",
        "input": "search query"
    });
    let req: EmbeddingRequest = serde_json::from_value(json).unwrap();
    assert!(req.encoding_format.is_none());
}

#[test]
fn speech_request_deser() {
    let json = serde_json::json!({
        "model": "tts-1",
        "input": "Hello world",
        "voice": "nova",
        "speed": 1.5
    });
    let req: SpeechRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model, "tts-1");
    assert_eq!(req.voice.as_deref(), Some("nova"));
    assert_eq!(req.speed, Some(1.5));
}

// ---------------------------------------------------------------------------
// WAV decoding (audio_decode / decode_pcm_wav)
// ---------------------------------------------------------------------------

fn make_wav(sample_rate: u32, channels: u16, bits: u16, samples: &[i16]) -> Vec<u8> {
    let data_size = (samples.len() * 2) as u32;
    let fmt_size: u32 = 16;
    let file_size = 4 + (8 + fmt_size) + (8 + data_size);
    let byte_rate = sample_rate * (channels as u32) * (bits as u32 / 8);
    let block_align = channels * (bits / 8);

    let mut buf = Vec::with_capacity(file_size as usize + 8);
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&file_size.to_le_bytes());
    buf.extend_from_slice(b"WAVE");
    // fmt chunk
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&fmt_size.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes()); // PCM
    buf.extend_from_slice(&channels.to_le_bytes());
    buf.extend_from_slice(&sample_rate.to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits.to_le_bytes());
    // data chunk
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());
    for &s in samples {
        buf.extend_from_slice(&s.to_le_bytes());
    }
    buf
}

#[test]
fn decode_valid_16khz_mono_wav() {
    let samples: Vec<i16> = vec![0, 16383, -16384, i16::MAX, i16::MIN];
    let wav = make_wav(16_000, 1, 16, &samples);
    let result = decode_pcm_wav(&wav).unwrap();
    assert_eq!(result.len(), 5);
    assert!((result[0] - 0.0).abs() < 1e-5);
    assert!((result[3] - 1.0).abs() < 1e-5);
    assert!((result[4] - (-1.0)).abs() < 1e-3);
}

#[test]
fn decode_wav_via_audio_decode() {
    let wav = make_wav(16_000, 1, 16, &[0, 100, -100]);
    let result = audio_decode(&wav, Some("audio/wav")).unwrap();
    assert_eq!(result.len(), 3);
}

#[test]
fn reject_stereo_wav() {
    let wav = make_wav(16_000, 2, 16, &[0, 0, 100, 100]);
    let err = decode_pcm_wav(&wav).unwrap_err();
    let body = serde_json::to_value(&err).unwrap();
    assert!(body["error"]["message"].as_str().unwrap().contains("mono"));
}

#[test]
fn reject_wrong_sample_rate() {
    let wav = make_wav(44_100, 1, 16, &[0, 100]);
    let err = decode_pcm_wav(&wav).unwrap_err();
    let body = serde_json::to_value(&err).unwrap();
    assert!(body["error"]["message"]
        .as_str()
        .unwrap()
        .contains("16 kHz"));
}

#[test]
fn reject_non_pcm_wav() {
    let mut wav = make_wav(16_000, 1, 16, &[0]);
    // Overwrite audio_format (offset 20-21) to 3 (IEEE float)
    wav[20] = 3;
    wav[21] = 0;
    let err = decode_pcm_wav(&wav).unwrap_err();
    let body = serde_json::to_value(&err).unwrap();
    assert!(body["error"]["message"].as_str().unwrap().contains("PCM"));
}

#[test]
fn reject_non_wav_audio() {
    let mp3_header = vec![0xFF, 0xFB, 0x90, 0x00]; // fake MP3 sync
    let err = audio_decode(&mp3_header, Some("audio/mpeg")).unwrap_err();
    let body = serde_json::to_value(&err).unwrap();
    assert!(body["error"]["message"]
        .as_str()
        .unwrap()
        .contains("Unsupported"));
}

#[test]
fn reject_truncated_wav() {
    let wav = make_wav(16_000, 1, 16, &[0, 100]);
    let truncated = &wav[..20]; // cut off mid-fmt
    let err = decode_pcm_wav(truncated).unwrap_err();
    let body = serde_json::to_value(&err).unwrap();
    assert!(body["error"]["message"]
        .as_str()
        .unwrap()
        .contains("Truncated"));
}

#[test]
fn decode_wav_inflated_chunk_size() {
    let mut wav = make_wav(16_000, 1, 16, &[100, -100]);
    let data_size_pos = wav.windows(4).position(|w| w == b"data").unwrap() + 4;
    wav[data_size_pos..data_size_pos + 4].copy_from_slice(&0xFFFF_FFFFu32.to_le_bytes());

    let result = decode_pcm_wav(&wav).unwrap();
    assert_eq!(result.len(), 2);
}

// ---------------------------------------------------------------------------
// pcm16_to_f32
// ---------------------------------------------------------------------------

#[test]
fn pcm16_to_f32_roundtrip() {
    let samples: Vec<i16> = vec![0, i16::MAX, i16::MIN, 1000, -1000];
    let bytes: Vec<u8> = samples.iter().flat_map(|s| s.to_le_bytes()).collect();
    let result = pcm16_to_f32(&bytes);
    assert_eq!(result.len(), 5);
    assert!((result[0] - 0.0).abs() < 1e-6);
    assert!((result[1] - 1.0).abs() < 1e-5);
    assert!(result[2] < -0.999);
    assert!((result[3] - 1000.0 / i16::MAX as f32).abs() < 1e-5);
}

#[test]
fn pcm16_to_f32_empty() {
    assert!(pcm16_to_f32(&[]).is_empty());
}

#[test]
fn pcm16_to_f32_odd_byte_dropped() {
    let bytes = 500_i16.to_le_bytes().to_vec();
    let mut with_trailing = bytes.clone();
    with_trailing.push(0xFF);
    assert_eq!(pcm16_to_f32(&with_trailing).len(), 1);
}

// ---------------------------------------------------------------------------
// Optional ChatMessage.content
// ---------------------------------------------------------------------------

#[test]
fn chat_message_null_content() {
    let json = serde_json::json!({
        "role": "assistant",
        "content": null
    });
    let msg: ChatMessage = serde_json::from_value(json).unwrap();
    assert!(msg.content.is_none());
}

#[test]
fn chat_message_missing_content() {
    let json = serde_json::json!({
        "role": "assistant"
    });
    let msg: ChatMessage = serde_json::from_value(json).unwrap();
    assert!(msg.content.is_none());
}

// ---------------------------------------------------------------------------
// PromptInput error messages
// ---------------------------------------------------------------------------

#[test]
fn prompt_input_empty_vs_batch_distinct_errors() {
    let empty_err = PromptInput::Many(vec![]).into_single().unwrap_err();
    let batch_err = PromptInput::Many(vec!["a".into(), "b".into()])
        .into_single()
        .unwrap_err();
    assert_ne!(empty_err, batch_err);
    assert!(empty_err.contains("empty"));
    assert!(batch_err.contains("Batch"));
}

// ---------------------------------------------------------------------------
// Billing: estimate_token_count
// ---------------------------------------------------------------------------
//
// Locks in the current (chars + 3) / 4 formula that feeds every prompt bill
// amount downstream. Materially wrong for non-Latin scripts / code / JSON
// (see the TODO on the function itself), but that inaccuracy is what
// EVERY currently-produced bill uses — a silent change here changes what
// users pay, so pin it explicitly.
// ---------------------------------------------------------------------------

#[test]
fn estimate_token_count_empty() {
    assert_eq!(estimate_token_count(""), 0);
}

#[test]
fn estimate_token_count_rounds_up() {
    // 1 char → ceil(1/4) = 1
    assert_eq!(estimate_token_count("a"), 1);
    // 4 chars → exact bucket
    assert_eq!(estimate_token_count("abcd"), 1);
    // 5 chars → ceil(5/4) = 2
    assert_eq!(estimate_token_count("abcde"), 2);
}

#[test]
fn estimate_token_count_hundred_chars() {
    assert_eq!(estimate_token_count(&"a".repeat(100)), 25);
}

#[test]
fn estimate_token_count_counts_unicode_scalars_not_bytes() {
    // Non-Latin scripts: this is where the "4 chars per token" heuristic
    // is materially wrong (see the TODO). Test asserts scalar-count
    // behavior, NOT byte count, so if someone accidentally switches to
    // .len() (bytes) we catch it — that would over-count multi-byte
    // chars and quietly over-bill non-English users even further.
    let cjk = "中文测试"; // 4 scalars, 12 UTF-8 bytes
    assert_eq!(estimate_token_count(cjk), 1);
    assert_eq!(cjk.len(), 12); // sanity check on our fixture
}

// ---------------------------------------------------------------------------
// Billing: per-endpoint amount formulas
// ---------------------------------------------------------------------------
//
// Handler-level formulas in `billing_amounts` are pinned here so a silent
// change to billing math is caught before it hits production.
// (Chat — streaming and non-streaming — and embeddings are billed by
// `AIService` via host_rates; see `stream_completion_bills_once_on_success`.)
// ---------------------------------------------------------------------------

#[test]
fn speech_amount_per_thousand_chars() {
    // Empty → floored at 1.0.
    assert!((speech_amount(0) - 1.0).abs() < f64::EPSILON);
    // Below 1000 chars → floored at 1.0.
    assert!((speech_amount(50) - 1.0).abs() < f64::EPSILON);
    assert!((speech_amount(999) - 1.0).abs() < f64::EPSILON);
    // At 1000 chars → 1.0 exact (floor doesn't kick in).
    assert!((speech_amount(1000) - 1.0).abs() < f64::EPSILON);
    // Above → linear.
    assert!((speech_amount(2500) - 2.5).abs() < 1e-12);
    assert!((speech_amount(10_000) - 10.0).abs() < f64::EPSILON);
}

// ---------------------------------------------------------------------------
// Billing: From<BillingError> for OpenAIError
// ---------------------------------------------------------------------------
//
// The whole handler stack relies on `bill_compute(...)?` for error
// propagation via this conversion. If it drifts — wrong status, wrong
// error code, wrong wire message shape — SDK clients that key their retry
// logic off HTTP 429 / `insufficient_quota` silently break.
// ---------------------------------------------------------------------------

#[test]
fn billing_error_insufficient_credits_maps_to_429_quota() {
    let err: OpenAIError = BillingError::InsufficientCredits.into();
    assert_eq!(err.status, axum::http::StatusCode::TOO_MANY_REQUESTS);
    let body = serde_json::to_value(&err).unwrap();
    assert_eq!(body["error"]["type"], "insufficient_quota");
    assert_eq!(body["error"]["code"], "insufficient_quota");
    assert_eq!(body["error"]["message"], "Insufficient compute credits");
    assert!(body["error"]["param"].is_null());
}

#[test]
fn billing_error_user_not_found_maps_to_500_internal() {
    let err: OpenAIError = BillingError::UserNotFound("alice@x".into()).into();
    assert_eq!(err.status, axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    let body = serde_json::to_value(&err).unwrap();
    assert_eq!(body["error"]["type"], "server_error");
    // The specific user email must NOT leak in the client-facing body —
    // the raw error message is only logged, the response gets a generic
    // "Billing operation failed" string.
    let msg = body["error"]["message"].as_str().unwrap();
    assert!(
        !msg.contains("alice@x"),
        "user email leaked to client: {msg}"
    );
    assert_eq!(msg, "Billing operation failed");
}

#[test]
fn billing_error_other_maps_to_500_internal() {
    let err: OpenAIError = BillingError::Other(anyhow::anyhow!("db offline")).into();
    assert_eq!(err.status, axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    let body = serde_json::to_value(&err).unwrap();
    assert_eq!(body["error"]["type"], "server_error");
    // Same redaction rule — details go to the log, generic string to client.
    let msg = body["error"]["message"].as_str().unwrap();
    assert!(!msg.contains("db offline"), "internal detail leaked: {msg}");
    assert_eq!(msg, "Billing operation failed");
}

#[test]
fn billing_error_conversion_is_terminal_via_question_mark() {
    // Simulates the exact call-site shape used in every handler:
    //     bill_compute(...)? → returns OpenAIError automatically.
    // If BillingError implements Display in a way that changes the
    // OpenAIError message we constructed above, this catches it too.
    fn handler() -> Result<(), OpenAIError> {
        Err::<(), BillingError>(BillingError::InsufficientCredits)?;
        Ok(())
    }
    let err = handler().unwrap_err();
    assert_eq!(err.status, axum::http::StatusCode::TOO_MANY_REQUESTS);
    let body = serde_json::to_value(&err).unwrap();
    assert_eq!(body["error"]["code"], "insufficient_quota");
}

// ---------------------------------------------------------------------------
// Billing: behavioural guarantees via the cfg(test) counter seam in
// crate::billing::test_seam. These replace the earlier source-string-scan
// tests, which asserted spelling rather than behaviour.
//
// The counter is thread-local and recorded at the top of bill_compute()
// BEFORE any early return, so tests can drive real handler code paths
// and see exactly which bill_compute calls the handler makes,
// independent of DB / free-hosting state.
//
// Tests that need to exercise the full handler mount an axum router
// with only openai_compat routes and send requests via tower::ServiceExt.
// AIService::global_instance() is not initialised in the test binary, so
// requests that reach the AIService boundary get a 500 back — which is
// fine, because the billing invariants we're testing (does the HANDLER
// call bill_compute? with what label? how many times?) are observable
// BEFORE AIService is hit.
// ---------------------------------------------------------------------------

use crate::api::auth::AppState;
use crate::billing::test_seam;
use axum::{
    body::Body,
    http::{Request, StatusCode},
    Router,
};
use http_body_util::BodyExt;
use tower::ServiceExt;

/// Admin-credential token used by all handler-driven tests. Sending this
/// as `Authorization: Bearer <token>` makes capabilities_from_token()
/// short-circuit to ALL_CAPABILITY without hitting Ad4mDb for the
/// capability lookup itself — but the auth extractor still calls
/// `track_last_seen_from_token` → `user_email_from_token` which reads
/// `multi_user_enabled` from Ad4mDb before any early return. So we
/// initialise an in-memory DB once per test binary. See init_test_db().
const TEST_ADMIN_TOKEN: &str = "marvin-test-admin-credential";

/// Initialise Ad4mDb with an in-memory sqlite once per test binary.
/// Idempotent, poison-safe: if a previous test panicked while holding
/// the DB mutex, we clear the poison and reinstall.
///
/// Called from every handler-driven test (via router_and_db()) because
/// axum's AuthContext extractor unconditionally invokes
/// `track_last_seen_from_token` → `user_email_from_token`, which reads
/// `multi_user_enabled` from Ad4mDb before deciding to no-op.
fn init_test_db() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        // If a prior init attempt panicked, the mutex may be poisoned.
        // Recover by taking the poisoned inner value.
        let arc = crate::db::Ad4mDb::global_instance();
        let guard = match arc.lock() {
            Ok(g) => g,
            Err(poisoned) => poisoned.into_inner(),
        };
        if guard.is_none() {
            // Direct construction is not exposed; use the public init
            // helper by dropping the guard first.
            drop(guard);
            let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        }
    });
}

/// Build a router with only the /v1 openai_compat routes for testing.
/// AIService is not initialised, so any endpoint that reaches
/// `AIService::global_instance()` returns 500 — but tests here only
/// assert on things observable BEFORE that boundary (billing, auth,
/// validation, error envelope shape).
///
/// Also initialises an in-memory Ad4mDb the first time it is called.
fn test_router() -> Router {
    init_test_db();
    let state = AppState {
        admin_credential: Some(TEST_ADMIN_TOKEN.to_string()),
        auto_permit_cap_requests: true,
    };
    super::router::router().with_state(state)
}

fn post_json(uri: &str, body: serde_json::Value) -> Request<Body> {
    Request::builder()
        .uri(uri)
        .method("POST")
        .header("content-type", "application/json")
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from(body.to_string()))
        .unwrap()
}

async fn body_bytes(resp: axum::response::Response) -> Vec<u8> {
    resp.into_body()
        .collect()
        .await
        .expect("collect body")
        .to_bytes()
        .to_vec()
}

async fn body_json(resp: axum::response::Response) -> serde_json::Value {
    let bytes = body_bytes(resp).await;
    serde_json::from_slice(&bytes).unwrap_or_else(|e| {
        panic!(
            "response body was not JSON: {e}\n---body---\n{}",
            String::from_utf8_lossy(&bytes)
        )
    })
}

/// 44-byte RIFF/WAVE header + `n_samples` × 2 bytes of PCM silence.
/// 16 kHz mono s16le — matches what decode_pcm_wav expects.
fn tiny_wav_16khz_mono(n_samples: usize) -> Vec<u8> {
    let byte_rate: u32 = 16000 * 2;
    let data_size: u32 = (n_samples * 2) as u32;
    let riff_size: u32 = 36 + data_size;
    let mut w = Vec::with_capacity(44 + n_samples * 2);
    w.extend_from_slice(b"RIFF");
    w.extend_from_slice(&riff_size.to_le_bytes());
    w.extend_from_slice(b"WAVEfmt ");
    w.extend_from_slice(&16u32.to_le_bytes()); // fmt chunk size
    w.extend_from_slice(&1u16.to_le_bytes()); // PCM
    w.extend_from_slice(&1u16.to_le_bytes()); // mono
    w.extend_from_slice(&16000u32.to_le_bytes()); // sample rate
    w.extend_from_slice(&byte_rate.to_le_bytes());
    w.extend_from_slice(&2u16.to_le_bytes()); // block align
    w.extend_from_slice(&16u16.to_le_bytes()); // bits per sample
    w.extend_from_slice(b"data");
    w.extend_from_slice(&data_size.to_le_bytes());
    for _ in 0..n_samples {
        w.extend_from_slice(&0i16.to_le_bytes());
    }
    w
}

// ---------------------------------------------------------------------------
// Seam self-test — proves the counter records what it should before any
// real handler-integration test relies on it.
// ---------------------------------------------------------------------------

#[test]
fn seam_records_calls_and_resets() {
    test_seam::reset();
    assert_eq!(test_seam::call_count(), 0);

    test_seam::force_result(test_seam::ForcedResult::Success);
    let _ = crate::billing::bill_compute("a@ex.test", 1.5, "ai_prompt", Some("v1/chat"));
    let _ = crate::billing::bill_compute("a@ex.test", 3.0, "ai_tts", None);

    let calls = test_seam::calls();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].email, "a@ex.test");
    assert_eq!(calls[0].amount, 1.5);
    assert_eq!(calls[0].operation, "ai_prompt");
    assert_eq!(calls[0].summary.as_deref(), Some("v1/chat"));
    assert_eq!(calls[1].amount, 3.0);
    assert_eq!(calls[1].operation, "ai_tts");

    test_seam::reset();
    assert_eq!(test_seam::call_count(), 0);
}

#[test]
fn seam_can_force_insufficient_credits() {
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::InsufficientCredits);

    let r = crate::billing::bill_compute("x@ex.test", 1.0, "ai_prompt", None);
    assert!(matches!(r, Err(BillingError::InsufficientCredits)));

    // Even a forced-error call is still counted — that's the whole point:
    // "how many times did the handler ATTEMPT to bill?"
    assert_eq!(test_seam::call_count(), 1);
    test_seam::reset();
}

// ---------------------------------------------------------------------------
// Regression guard for round-1 finding #2: /v1/audio/transcriptions
// double-billed (once at the handler, once in the whisper worker).
//
// Behavioural test: send a real multipart request through the router
// with a valid audio blob. The handler will reach
// AIService::global_instance() and 500 (AIService is not initialised in
// tests) — but by that point it will have made EXACTLY zero bill_compute
// calls if the fix from commit 849681b4 is intact. Any regression that
// re-adds handler-level billing will make call_count > 0 and fail this
// test with a clear message.
// ---------------------------------------------------------------------------

/// Build a minimal but valid multipart body: `model=whisper-1` + a tiny
/// WAV file. Enough to get past parse + audio_decode and reach the
/// AIService boundary.
fn tiny_multipart_transcription() -> (String, Vec<u8>) {
    let boundary = "----marvin-test-boundary-12345";
    let mut body: Vec<u8> = Vec::new();
    // model field
    body.extend_from_slice(
        format!(
            "--{b}\r\n\
             Content-Disposition: form-data; name=\"model\"\r\n\r\n\
             whisper-1\r\n",
            b = boundary
        )
        .as_bytes(),
    );
    // file field with a minimal 16 kHz mono WAV (44-byte header + 32 samples of silence)
    let wav = tiny_wav_16khz_mono(32);
    body.extend_from_slice(
        format!(
            "--{b}\r\n\
             Content-Disposition: form-data; name=\"file\"; filename=\"a.wav\"\r\n\
             Content-Type: audio/wav\r\n\r\n",
            b = boundary
        )
        .as_bytes(),
    );
    body.extend_from_slice(&wav);
    body.extend_from_slice(b"\r\n");
    // terminator
    body.extend_from_slice(format!("--{b}--\r\n", b = boundary).as_bytes());
    (format!("multipart/form-data; boundary={boundary}"), body)
}

#[tokio::test]
async fn transcriptions_handler_does_not_bill() {
    test_seam::reset();
    let (ct, body) = tiny_multipart_transcription();
    let req = Request::builder()
        .uri("/audio/transcriptions")
        .method("POST")
        .header("content-type", ct)
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from(body))
        .unwrap();

    let _ = test_router().oneshot(req).await.unwrap();

    // Whatever status the router returned (400/500 for missing model or
    // AIService), the handler MUST NOT have called bill_compute. All
    // transcription billing happens per-word inside the whisper worker
    // in ai_service/mod.rs::open_transcription_stream.
    let calls = test_seam::calls();
    assert!(
        calls.is_empty(),
        "regression: /v1/audio/transcriptions handler called bill_compute {} time(s): {calls:?}\n\
         Transcription is billed per-word inside the whisper worker \
         (ai_service/mod.rs::open_transcription_stream). Handler-level \
         billing double-charges the user. See round-1 review finding #2.",
        calls.len()
    );
    test_seam::reset();
}

// ---------------------------------------------------------------------------
// Per-endpoint operation-label + amount contract.
//
// Handler-driven router tests can't reach the billing path for speech
// end-to-end here because the handler gates billing on
// `user_email_from_token(auth_token).is_some()`, which requires a
// JWT-shaped token — constructing which would need the Wallet
// initialised (heavy). The admin credential token used by the router
// tests deliberately bypasses that path.
//
// The contract we care about is instead covered by three layered
// assertions elsewhere in this file:
//
//   1. `speech_amount_per_thousand_chars` (pure unit) pins the exact
//      amount formula the speech handler uses.
//   2. `insufficient_quota_error` + `handler_shape_bill_compute_question_mark_propagates_insufficient_credits`
//      (pre-existing) pin the From<BillingError> for OpenAIError
//      conversion at 429 with the insufficient_quota code.
//   3. `seam_records_calls_and_resets` proves the seam records what
//      the handler would call — so if a live handler ever runs against
//      the seam (e.g. in a future e2e binary with Wallet+DB), the
//      assertions above compose.
//
// The transcription "handler MUST NOT bill" invariant IS driven
// end-to-end below — the transcriptions handler doesn't gate its
// (deliberately absent) bill on user_email_from_token, so a router
// request with any token exercises the code path far enough to prove
// the invariant.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// /v1 API surface — OpenAI-shape contract tests.
//
// These drive the axum router end-to-end (short of AIService) and
// assert that the wire responses match what an OpenAI client expects:
//   - error envelope shape { "error": { "message", "type", "param", "code" } }
//   - correct status codes for invalid input, missing fields, bad content-type
//   - JSON parse errors → 400, not 500
// AIService-dependent paths (successful chat completions, transcriptions
// producing text, etc.) require a live AIService and are covered by
// existing integration tests in tests/js.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn chat_completions_invalid_json_returns_400() {
    test_seam::reset();
    let req = Request::builder()
        .uri("/chat/completions")
        .method("POST")
        .header("content-type", "application/json")
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from("{not valid json"))
        .unwrap();

    let resp = test_router().oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
    // No bill_compute call should have happened for a request that
    // failed to even parse.
    assert_eq!(test_seam::call_count(), 0);
    test_seam::reset();
}

#[tokio::test]
async fn embeddings_invalid_json_returns_400() {
    test_seam::reset();
    let req = Request::builder()
        .uri("/embeddings")
        .method("POST")
        .header("content-type", "application/json")
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from("{"))
        .unwrap();

    let resp = test_router().oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
    assert_eq!(test_seam::call_count(), 0);
    test_seam::reset();
}

#[tokio::test]
async fn transcriptions_missing_model_returns_400() {
    test_seam::reset();
    // Multipart with only the file field, no model → handler must 400.
    let boundary = "----marvin-test-nomdl";
    let wav = tiny_wav_16khz_mono(16);
    let mut body: Vec<u8> = Vec::new();
    body.extend_from_slice(
        format!(
            "--{b}\r\n\
             Content-Disposition: form-data; name=\"file\"; filename=\"a.wav\"\r\n\
             Content-Type: audio/wav\r\n\r\n",
            b = boundary
        )
        .as_bytes(),
    );
    body.extend_from_slice(&wav);
    body.extend_from_slice(b"\r\n");
    body.extend_from_slice(format!("--{b}--\r\n", b = boundary).as_bytes());

    let req = Request::builder()
        .uri("/audio/transcriptions")
        .method("POST")
        .header(
            "content-type",
            format!("multipart/form-data; boundary={boundary}"),
        )
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from(body))
        .unwrap();

    let resp = test_router().oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
    let body = body_json(resp).await;
    // OpenAI-shape error envelope
    assert!(body["error"]["message"].is_string());
    assert!(body["error"]["type"].is_string());
    // Must NOT bill for a request that failed validation.
    assert_eq!(test_seam::call_count(), 0);
    test_seam::reset();
}

#[tokio::test]
async fn speech_missing_input_returns_400() {
    test_seam::reset();
    let req = post_json(
        "/audio/speech",
        serde_json::json!({
            "model": "tts-1",
            "voice": "alloy",
            // `input` missing
        }),
    );

    let resp = test_router().oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
    assert_eq!(test_seam::call_count(), 0);
    test_seam::reset();
}

#[tokio::test]
async fn error_envelope_shape_matches_openai() {
    // Send any request that fails cleanly — the response envelope must
    // be exactly the shape OpenAI SDKs expect: an "error" object with
    // "message" (string), "type" (string), and either null or string
    // "param" and "code".
    test_seam::reset();
    let resp = test_router()
        .oneshot(post_json(
            "/chat/completions",
            serde_json::json!({ /* no fields */ }),
        ))
        .await
        .unwrap();
    // JSON deser fails at field level for missing `messages`, `model`
    let body = body_json(resp).await;
    let err = &body["error"];
    assert!(err.is_object(), "response must have an `error` object");
    assert!(err["message"].is_string());
    assert!(err["type"].is_string());
    // param/code may be null but the keys must exist per OpenAI shape
    assert!(err.get("param").is_some());
    assert!(err.get("code").is_some());
    test_seam::reset();
}

// ---------------------------------------------------------------------------
// Streaming chat completions: the round-1 finding #5 note is that the
// stream-forwarder task bills once at the end. Verify that request
// setup does NOT bill up front (stream=true takes a different code path
// than oneshot). Companion to the source-scan-replacement guarantees.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn chat_completions_stream_does_not_bill_at_setup() {
    test_seam::reset();
    let req = Request::builder()
        .uri("/chat/completions")
        .method("POST")
        .header("content-type", "application/json")
        .header("authorization", format!("Bearer {TEST_ADMIN_TOKEN}"))
        .body(Body::from(
            serde_json::json!({
                "model": "gpt-4",
                "stream": true,
                "messages": [{ "role": "user", "content": "hi" }],
            })
            .to_string(),
        ))
        .unwrap();

    let _ = test_router().oneshot(req).await.unwrap();
    // The stream setup path calls check_compute_credits (no seam
    // record) and then attempts to open the stream — which fails at
    // AIService::global_instance(). NO bill_compute call happens here;
    // billing is deferred to end-of-stream. If setup starts calling
    // bill_compute up front, this fails.
    let calls = test_seam::calls();
    assert!(
        calls.is_empty(),
        "streaming chat setup should defer billing to end-of-stream; got calls: {calls:?}"
    );
    test_seam::reset();
}

// ---------------------------------------------------------------------------
// Streaming billing forwarder: the missing charge is the whole point of
// this section. On success the stream must bill exactly once (same shape
// as the non-stream path: ai_prompt, prompt+completion tokens, host-rate
// priced); on error it must not bill at all. The forwarder is exercised
// directly (no live LLM channel needed in unit tests).
// ---------------------------------------------------------------------------

use crate::ai_service::{AIService, PromptResult};

/// Ensure the global wallet has a "main" keypair (decode_jwt signs and
/// verifies with it) and return a user JWT whose `sub` is `email`.
///
/// Mints against the *current* global "main" key. Safe under the project's
/// single-threaded test convention (`--test-threads=1`, see package.json);
/// a parallel test rotating the key via `test_utils::setup_wallet()`
/// between mint and decode would break verification.
fn user_jwt_token(email: &str) -> String {
    use jsonwebtoken::{encode, EncodingKey, Header};
    let wallet = crate::wallet::Wallet::instance();
    let mut w = wallet.lock().unwrap();
    let w = w.as_mut().unwrap();
    if w.get_secret_key(&"main".to_string()).is_none() {
        w.generate_keypair("main".to_string());
    }
    let secret = w.get_secret_key(&"main".to_string()).unwrap();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    encode(
        &Header::default(),
        &serde_json::json!({
            "iss": "ad4m-test",
            "sub": email,
            "aud": "ad4m-test",
            "exp": now + 3600,
            "iat": now,
            "nonce": "test-nonce",
            "capabilities": { "appName": "test", "appDesc": "test" },
        }),
        &EncodingKey::from_secret(secret.as_slice()),
    )
    .unwrap()
}

fn sample_prompt_result() -> PromptResult {
    PromptResult {
        text: "hello world".to_string(),
        prompt_tokens: 10,
        completion_tokens: 5,
        model_id: "gpt-4".to_string(),
    }
}

/// Restores multi-user mode + host rates on drop, so a panicking
/// assertion can't leak settings into the rest of the test binary (the
/// global in-memory DB is shared across all tests).
struct DbSettingsGuard {
    prev_multi_user: bool,
    prev_rates: Vec<(String, f64)>,
}

impl DbSettingsGuard {
    fn set(multi_user: bool, rates: &[(String, f64)]) -> Self {
        let prev_multi_user = crate::db::Ad4mDb::with_global_instance(|db| {
            db.get_multi_user_enabled().unwrap_or(false)
        });
        let prev_rates =
            crate::db::Ad4mDb::with_global_instance(|db| db.get_host_rates().unwrap_or_default());
        let _ = crate::db::Ad4mDb::with_global_instance(|db| {
            db.set_multi_user_enabled(multi_user)?;
            db.set_host_rates(rates)
        });
        Self {
            prev_multi_user,
            prev_rates,
        }
    }
}

impl Drop for DbSettingsGuard {
    fn drop(&mut self) {
        let _ = crate::db::Ad4mDb::with_global_instance(|db| {
            db.set_multi_user_enabled(self.prev_multi_user)?;
            db.set_host_rates(&self.prev_rates)
        });
    }
}

#[tokio::test]
async fn stream_completion_bills_once_on_success() {
    init_test_db();
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::Success);

    // bill_prompt_if_authed needs multi-user mode on + a user JWT, and
    // bill_ai_operation needs a priced model. The guard restores both even
    // if an assertion below panics.
    let _guard = DbSettingsGuard::set(true, &[("gpt-4".to_string(), 0.5)]);

    let token = user_jwt_token("billing-user@ex.test");
    let (tx, rx) = tokio::sync::oneshot::channel();
    AIService::bill_and_forward_stream_result(Some(token), "gpt-4", Ok(sample_prompt_result()), tx)
        .await;
    let forwarded = rx.await.unwrap().unwrap();
    assert_eq!(forwarded.text, "hello world");
    assert_eq!(forwarded.prompt_tokens, 10);
    assert_eq!(forwarded.completion_tokens, 5);

    let calls = test_seam::calls();
    assert_eq!(
        calls.len(),
        1,
        "stream completion must bill exactly once; got calls: {calls:?}"
    );
    assert_eq!(calls[0].email, "billing-user@ex.test");
    assert!(
        (calls[0].amount - 7.5).abs() < f64::EPSILON,
        "15 tokens @ 0.5"
    );
    assert_eq!(calls[0].operation, "ai_prompt");
    assert_eq!(
        calls[0].summary.as_deref(),
        Some("15 tokens (model: gpt-4)")
    );

    test_seam::reset();
}

#[tokio::test]
async fn stream_error_does_not_bill() {
    init_test_db();
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::Success);

    // Multi-user mode + a priced model are enabled on purpose: the only
    // thing that must stop billing here is the Err gate. Without this, the
    // skip would come from the missing user email and the test would pass
    // even if error streams did bill.
    let _guard = DbSettingsGuard::set(true, &[("gpt-4".to_string(), 0.5)]);

    // A valid user token is passed on purpose: the skip must come from
    // the Err gate, not from token absence (matches prompt_messages, which
    // bills only after a successful prompt).
    let result: Result<PromptResult, anyhow::Error> = Err(anyhow::anyhow!("inference failed"));
    let (tx, rx) = tokio::sync::oneshot::channel();
    AIService::bill_and_forward_stream_result(
        Some(user_jwt_token("billing-user@ex.test")),
        "gpt-4",
        result,
        tx,
    )
    .await;
    let err = rx.await.unwrap().unwrap_err();
    assert_eq!(err.to_string(), "inference failed");

    assert!(
        test_seam::calls().is_empty(),
        "failed streams must not bill; got calls: {:?}",
        test_seam::calls()
    );
    test_seam::reset();
}

#[tokio::test]
async fn stream_without_token_does_not_bill() {
    init_test_db();
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::Success);

    let (tx, rx) = tokio::sync::oneshot::channel();
    AIService::bill_and_forward_stream_result(None, "gpt-4", Ok(sample_prompt_result()), tx).await;
    let forwarded = rx.await.unwrap().unwrap();
    assert_eq!(forwarded.text, "hello world");

    assert!(
        test_seam::calls().is_empty(),
        "unauthenticated streams must not bill; got calls: {:?}",
        test_seam::calls()
    );
    test_seam::reset();
}
