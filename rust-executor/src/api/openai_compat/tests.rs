use super::audio::{audio_decode, decode_pcm_wav};
use super::billing_amounts::{
    chat_or_completion_amount, embedding_amount, speech_amount, stream_prompt_amount,
    MIN_PROMPT_BILL,
};
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
// Every /v1 endpoint that debits credits routes its amount through
// `billing_amounts` — the handlers no longer inline formulas. Tests below
// pin the exact formula per endpoint so a silent change to billing math
// is caught before it hits production.
// ---------------------------------------------------------------------------

#[test]
fn chat_amount_zero_tokens_floors_to_minimum() {
    // An "empty" prompt still bills a floor amount so the request shows
    // up in the compute log at all (invariant: MIN_PROMPT_BILL).
    assert!((chat_or_completion_amount(0, 0) - MIN_PROMPT_BILL).abs() < f64::EPSILON);
}

#[test]
fn chat_amount_proportional_above_floor() {
    // Below the 1000-token bucket → still floored.
    let a = chat_or_completion_amount(100, 100);
    assert!(a >= MIN_PROMPT_BILL);
    assert!(a > 0.199 && a < 0.201, "200 tokens → 0.2 credits, got {a}");

    // Exactly at the bucket boundary.
    assert!((chat_or_completion_amount(500, 500) - 1.0).abs() < 1e-12);

    // Above the bucket → scales linearly.
    assert!((chat_or_completion_amount(1500, 2500) - 4.0).abs() < 1e-12);
}

#[test]
fn chat_amount_scaling_is_linear() {
    // 10× the tokens → 10× the amount (once above the floor).
    let ten_k = chat_or_completion_amount(5_000, 5_000);
    let hundred_k = chat_or_completion_amount(50_000, 50_000);
    assert!((hundred_k - 10.0 * ten_k).abs() < 1e-9);
}

#[test]
fn stream_prompt_amount_is_flat_one() {
    // Streaming path bills a flat 1.0 today because Kalosm's stream API
    // doesn't yield per-token counts. Locked in so any switch to
    // proportional streaming billing is a deliberate, tested change.
    assert!((stream_prompt_amount() - 1.0).abs() < f64::EPSILON);
}

#[test]
fn embedding_amount_per_vector() {
    // Batch of N inputs bills N credits — fair scaling for callers who
    // batch to reduce round-trips.
    assert!(
        (embedding_amount(0) - 1.0).abs() < f64::EPSILON,
        "min-one floor"
    );
    assert!((embedding_amount(1) - 1.0).abs() < f64::EPSILON);
    assert!((embedding_amount(5) - 5.0).abs() < f64::EPSILON);
    assert!((embedding_amount(100) - 100.0).abs() < f64::EPSILON);
}

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
// Billing: guard against transcription double-billing regression
// ---------------------------------------------------------------------------
//
// Round-1 review finding #2 was that /v1/audio/transcriptions billed
// twice: once at the handler, once inside the transcription worker.
// The fix was to remove the handler-level `bill_compute("ai_transcription", ...)`
// call and let ONLY the worker (ai_service/mod.rs) bill per word.
//
// A full runtime test of "exactly one debit" would need a mocked
// bill_compute (a trait seam or cfg(test) counter injected globally),
// which is a design decision that touches billing.rs itself. The
// lightweight guard below asserts the same invariant statically: the
// handler source MUST NOT contain a `bill_compute("ai_transcription")`
// call — if someone re-introduces one, this test fails at build+run
// time with a clear message pointing at the review context.
// ---------------------------------------------------------------------------

#[test]
fn no_ai_transcription_bill_in_transcriptions_handler() {
    // Read the handler source at test time so this catches regressions
    // even if the file layout changes.
    let src = include_str!("audio.rs");

    // Locate the `pub async fn transcriptions` body. Slice from the fn
    // signature to `pub async fn speech` (the next handler) so we scan
    // only the transcription flow.
    let start = src
        .find("pub async fn transcriptions")
        .expect("transcriptions handler must exist");
    let end = src[start..]
        .find("pub async fn speech")
        .map(|off| start + off)
        .unwrap_or(src.len());
    let body = &src[start..end];

    // Any bill_compute in this window is a regression. `check_compute_credits`
    // (the pre-check) is fine — it doesn't debit.
    assert!(
        !body.contains("bill_compute"),
        "regression: the transcriptions handler must NOT call bill_compute — \
         transcription is billed per-word inside the whisper worker \
         (ai_service/mod.rs::open_transcription_stream). Handler-level billing \
         double-charges the user. See Marvin's round-1 review finding #2 on PR #854."
    );
}

#[test]
fn transcription_amount_helper_not_defined() {
    // Companion to the above: billing_amounts intentionally has NO
    // `transcription_amount(...)` helper because the handler doesn't
    // bill. If someone adds one here, they've almost certainly wired it
    // into audio.rs too — which is the double-bill regression. Trigger a
    // review by making the source string search fail.
    let src = include_str!("billing_amounts.rs");
    assert!(
        !src.contains("fn transcription_amount"),
        "a transcription_amount helper appeared in billing_amounts.rs — \
         handler-level transcription billing is deliberately absent (see \
         no_ai_transcription_bill_in_transcriptions_handler for context)."
    );
}

// ---------------------------------------------------------------------------
// Billing: per-endpoint operation label integrity
// ---------------------------------------------------------------------------
//
// Every handler passes an operation label as the 3rd arg to bill_compute.
// The labels are contractual — they feed the compute_events log and
// downstream billing reports. A silent typo (`"ai_prompt"` → `"ai_promt"`)
// would still compile and still deduct credits, but reports would break.
//
// Static assertion approach — same principle as the double-bill guard:
// scan the handler source and confirm the labels are exactly what the
// documented contract says they are.
// ---------------------------------------------------------------------------

#[test]
fn chat_and_completions_use_ai_prompt_label() {
    let src = include_str!("chat.rs");
    // Non-stream chat + legacy completions + streaming chat all bill under "ai_prompt".
    // Count occurrences to catch someone silently changing one.
    let occurrences = src.matches("\"ai_prompt\"").count();
    assert!(
        occurrences >= 3,
        "expected >=3 \"ai_prompt\" labels in chat.rs (oneshot, completions, stream); found {occurrences}"
    );
    assert!(
        !src.contains("\"ai_promt\"") && !src.contains("\"ai_prompts\""),
        "typo in operation label"
    );
}

#[test]
fn embeddings_uses_ai_embedding_label() {
    let src = include_str!("embeddings.rs");
    assert!(src.contains("\"ai_embedding\""));
    assert!(
        !src.contains("\"ai_embeddings\""),
        "singular label required"
    );
}

#[test]
fn speech_uses_ai_tts_label() {
    let src = include_str!("audio.rs");
    // Only the speech handler bills (transcription doesn't — see
    // no_ai_transcription_bill_in_transcriptions_handler above), so we
    // expect exactly one "ai_tts" occurrence in audio.rs.
    let occurrences = src.matches("\"ai_tts\"").count();
    assert_eq!(
        occurrences, 1,
        "expected exactly one \"ai_tts\" bill site in audio.rs; found {occurrences}"
    );
}
