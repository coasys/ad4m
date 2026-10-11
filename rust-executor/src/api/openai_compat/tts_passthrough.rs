//! Remote-passthrough TTS client.
//!
//! The proposal allows three TTS backends behind a common
//! `AIService::synthesize` trait: local Kokoro, local Piper, or a
//! passthrough to an OpenAI-compatible upstream.  This module implements
//! the passthrough path; the local backends are scoped to a follow-up
//! (they add `ort` + voice model assets, both of which deserve their own
//! review).
//!
//! Configuration: we look for a TTS-typed model in the existing model
//! registry whose `api` field carries an `OPEN_AI` upstream.  When
//! `model_type == Llm` we currently misuse — see TODO below — the
//! configured remote LLM API key for TTS; once `ModelType::TextToSpeech`
//! lands, this module reads from the proper TTS-typed registration.
//!
//! No new dependencies: we reuse the existing `reqwest` client.

use crate::api::auth::AuthContext;
use crate::db::Ad4mDb;
use crate::types::{ModelApiType, ModelType};

use super::types::SpeechRequest;

#[derive(Debug)]
pub enum TtsPassthroughError {
    /// No TTS-capable upstream is configured on this executor.
    NotConfigured,
    Upstream(String),
}

/// Forward a `/v1/audio/speech` request to a configured upstream and
/// return the raw audio bytes.  Returns [`TtsPassthroughError::NotConfigured`]
/// when no upstream is registered.
pub async fn synthesize(
    _auth: &AuthContext,
    req: &SpeechRequest,
) -> Result<Vec<u8>, TtsPassthroughError> {
    let upstream = find_tts_upstream(&req.model)?;

    let body = serde_json::json!({
        "model": upstream.upstream_model,
        "input": req.input,
        "voice": req.voice.clone().unwrap_or_else(|| "alloy".to_string()),
        "response_format": req.response_format.clone().unwrap_or_else(|| "mp3".to_string()),
        "speed": req.speed.unwrap_or(1.0),
    });

    let base = upstream
        .base_url
        .trim_end_matches('/')
        .trim_end_matches("/v1");

    let client = reqwest::Client::new();
    let resp = client
        .post(format!("{base}/v1/audio/speech"))
        .bearer_auth(&upstream.api_key)
        .json(&body)
        .send()
        .await
        .map_err(|e| TtsPassthroughError::Upstream(e.to_string()))?;

    if !resp.status().is_success() {
        let status = resp.status();
        let text = resp.text().await.unwrap_or_default();
        return Err(TtsPassthroughError::Upstream(format!(
            "Upstream returned {status}: {text}"
        )));
    }

    let bytes = resp
        .bytes()
        .await
        .map_err(|e| TtsPassthroughError::Upstream(e.to_string()))?;
    Ok(bytes.to_vec())
}

struct TtsUpstream {
    base_url: String,
    api_key: String,
    upstream_model: String,
}

/// Look up a TTS-capable upstream in the model registry.
///
/// We don't have `ModelType::TextToSpeech` yet (that lands with the
/// local Kokoro PR), so the resolution rule is: any LLM-typed `Model`
/// whose name matches `requested_model` AND has an `OPEN_AI` API
/// upstream is treated as a TTS-capable forwarder.  This makes the
/// passthrough usable today without a schema migration; once the
/// `ModelType::TextToSpeech` enum lands, swap the filter to that type
/// and remove this comment.
fn find_tts_upstream(requested_model: &str) -> Result<TtsUpstream, TtsPassthroughError> {
    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|_| TtsPassthroughError::NotConfigured)?;

    // Prefer an exact name match (callers send the upstream model
    // string verbatim); fall back to the configured "default" remote
    // LLM if none matches.
    let candidate = models
        .iter()
        .find(|m| {
            m.model_type == ModelType::Llm
                && m.api
                    .as_ref()
                    .map(|a| matches!(a.api_type, ModelApiType::OpenAi))
                    .unwrap_or(false)
                && m.name == requested_model
        })
        .or_else(|| {
            models.iter().find(|m| {
                m.model_type == ModelType::Llm
                    && m.api
                        .as_ref()
                        .map(|a| matches!(a.api_type, ModelApiType::OpenAi))
                        .unwrap_or(false)
            })
        })
        .ok_or(TtsPassthroughError::NotConfigured)?;

    let api = candidate
        .api
        .as_ref()
        .ok_or(TtsPassthroughError::NotConfigured)?;

    Ok(TtsUpstream {
        base_url: api.base_url.to_string(),
        api_key: api.api_key.clone(),
        upstream_model: requested_model.to_string(),
    })
}
