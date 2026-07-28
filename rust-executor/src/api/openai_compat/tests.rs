use super::audio::{audio_decode, decode_pcm_wav};
use super::errors::OpenAIError;
use super::realtime::bytes_to_f32_le;
use super::types::*;

// ---------------------------------------------------------------------------
// bytes_to_f32_le
// ---------------------------------------------------------------------------

#[test]
fn bytes_to_f32_le_roundtrip() {
    let values: Vec<f32> = vec![0.0, 1.0, -1.0, 0.5, f32::MIN, f32::MAX];
    let bytes: Vec<u8> = values.iter().flat_map(|v| v.to_le_bytes()).collect();
    assert_eq!(bytes_to_f32_le(&bytes), values);
}

#[test]
fn bytes_to_f32_le_empty() {
    assert!(bytes_to_f32_le(&[]).is_empty());
}

#[test]
fn bytes_to_f32_le_trailing_bytes_ignored() {
    let one_float = 42.0_f32.to_le_bytes();
    let mut bytes = one_float.to_vec();
    bytes.extend_from_slice(&[0xFF, 0xFF]); // 2 extra bytes, not a full f32
    let result = bytes_to_f32_le(&bytes);
    assert_eq!(result.len(), 1);
    assert_eq!(result[0], 42.0);
}

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
    assert_eq!(req.messages[0].content.flatten_to_text(), "describe this");
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
