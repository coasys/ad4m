//! OpenAI-shaped speech to text: `POST {base}/v1/audio/transcriptions`, a multipart upload.
//!
//! Served by speaches (faster-whisper), OpenAI-shaped Parakeet wrappers, vLLM, OpenAI itself,
//! and whisper.cpp started with `--inference-path /v1/audio/transcriptions`. One call
//! transcribes one file: a stream sends each utterance as its own WAV, `/v1/audio/transcriptions`
//! forwards what its client uploaded.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::Value;
use std::time::Duration;
use url::Url;

use super::http::{credentialed_http, is_transport_safe, scrub, versioned_endpoint};
use super::http::{UpstreamStatus, CLEARTEXT_KEY_REFUSAL};
use crate::types::{Endpoint, ModelApiType};

/// A minute of speech takes seconds on a GPU and can take most of a minute on a CPU.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(120);

/// One encoded audio file, as the upload's `file` part.
pub struct AudioFile {
    pub bytes: Vec<u8>,
    pub file_name: String,
    pub mime: String,
}

/// The upstream's answer: its body and content type as sent, and the transcript in it.
#[derive(Debug)]
pub struct TranscribeReply {
    pub text: String,
    pub body: Vec<u8>,
    pub content_type: String,
}

#[async_trait]
pub trait RemoteTranscribe: Send + Sync {
    /// Transcribe one file with the upstream model `model`. `fields` are further form fields,
    /// sent as given (`language`, `prompt`, `response_format`, `temperature`, ...).
    async fn transcribe(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply>;
}

pub struct OpenAiTranscribe {
    http: reqwest::Client,
    api_key: String,
    /// Fully resolved, e.g. `http://127.0.0.1:8000/v1/audio/transcriptions`.
    endpoint: String,
}

impl OpenAiTranscribe {
    pub fn new(api_key: &str, base_url: Url) -> Self {
        Self {
            // `expect`: the default client follows redirects, and would take the key along.
            http: credentialed_http(REQUEST_TIMEOUT)
                .build()
                .expect("the HTTP client builds"),
            api_key: api_key.to_string(),
            endpoint: versioned_endpoint(base_url, "audio/transcriptions"),
        }
    }

    /// The client for a registered endpoint. Ollama and Anthropic have no transcription
    /// route, and a key goes over https or loopback only.
    pub fn for_endpoint(endpoint: &Endpoint) -> Result<Self> {
        if endpoint.kind.protocol() != ModelApiType::OpenAi {
            return Err(anyhow!(
                "Endpoint '{}' ({:?}) serves no /v1/audio/transcriptions",
                endpoint.name,
                endpoint.kind
            ));
        }
        if !endpoint.api_key.is_empty() && !is_transport_safe(&endpoint.base_url) {
            return Err(anyhow!(
                "{CLEARTEXT_KEY_REFUSAL} (endpoint '{}')",
                endpoint.name
            ));
        }
        Ok(Self::new(
            endpoint.api_key.expose(),
            endpoint.base_url.clone(),
        ))
    }
}

#[async_trait]
impl RemoteTranscribe for OpenAiTranscribe {
    async fn transcribe(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        let file = reqwest::multipart::Part::bytes(audio.bytes)
            .file_name(audio.file_name)
            .mime_str(&audio.mime)
            .map_err(|e| anyhow!("Invalid audio content type: {e}"))?;
        let form = fields.into_iter().fold(
            reqwest::multipart::Form::new()
                .text("model", model.to_string())
                .part("file", file),
            |form, (name, value)| form.text(name, value),
        );

        let mut request = self.http.post(&self.endpoint).multipart(form);
        // A local server takes no key, and some refuse an empty `Bearer `.
        if !self.api_key.is_empty() {
            request = request.bearer_auth(&self.api_key);
        }
        // The reqwest error stays in the chain, so `failure` can tell a timeout. It names the
        // URL, never a header.
        let response = request.send().await.map_err(|e| {
            anyhow::Error::new(e).context(format!("Could not reach {}", self.endpoint))
        })?;
        let status = response.status();
        let content_type = response
            .headers()
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/json")
            .to_string();
        let body = response
            .bytes()
            .await
            .map_err(|e| {
                anyhow::Error::new(e)
                    .context(format!("Could not read the answer of {}", self.endpoint))
            })?
            .to_vec();
        if !status.is_success() {
            return Err(UpstreamStatus {
                url: self.endpoint.clone(),
                status,
                body: scrub(&String::from_utf8_lossy(&body), &self.api_key),
            }
            .into());
        }
        let text = transcript_of(&body, &content_type)
            .map_err(|e| anyhow!("Could not read the answer of {}: {e}", self.endpoint))?;
        Ok(TranscribeReply {
            text,
            body,
            content_type,
        })
    }
}

/// The transcript in an answer: `text` of a JSON answer (`json`, `verbose_json`), else the
/// body itself (`text`, `srt`, `vtt`).
fn transcript_of(body: &[u8], content_type: &str) -> Result<String> {
    if content_type.starts_with("application/json") {
        let json: Value = serde_json::from_slice(body)?;
        return json
            .get("text")
            .and_then(Value::as_str)
            .map(|text| text.trim().to_string())
            .ok_or_else(|| anyhow!("the answer has no `text`"));
    }
    Ok(String::from_utf8_lossy(body).trim().to_string())
}

/// Why a transcription call failed, in words a caller may read: no URL, no key.
#[derive(Debug, PartialEq)]
pub enum TranscribeFailure {
    /// The upstream refused the request (4xx other than 401/403): the caller's to fix.
    Refused {
        status: reqwest::StatusCode,
        reason: String,
    },
    /// The upstream refused this executor's key.
    Unauthorized,
    /// The upstream failed (5xx).
    Failed(reqwest::StatusCode),
    Timeout,
    Unreachable,
}

impl TranscribeFailure {
    pub fn of(error: &anyhow::Error) -> Self {
        if let Some(upstream) = error.downcast_ref::<UpstreamStatus>() {
            return match upstream.status.as_u16() {
                401 | 403 => Self::Unauthorized,
                400..=499 => Self::Refused {
                    status: upstream.status,
                    reason: upstream_message(&upstream.body),
                },
                _ => Self::Failed(upstream.status),
            };
        }
        let timed_out = error
            .chain()
            .filter_map(|cause| cause.downcast_ref::<reqwest::Error>())
            .any(reqwest::Error::is_timeout);
        if timed_out {
            Self::Timeout
        } else {
            Self::Unreachable
        }
    }
}

impl std::fmt::Display for TranscribeFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Refused { status, reason } => {
                write!(
                    f,
                    "The transcription endpoint refused the request ({status}): {reason}"
                )
            }
            Self::Unauthorized => {
                f.write_str("The transcription endpoint refused this executor's key")
            }
            Self::Failed(status) => write!(f, "The transcription endpoint failed ({status})"),
            Self::Timeout => f.write_str("The transcription endpoint did not answer in time"),
            Self::Unreachable => f.write_str("The transcription endpoint is unreachable"),
        }
    }
}

/// `error.message` of an OpenAI error envelope, else the body as sent.
fn upstream_message(body: &str) -> String {
    serde_json::from_str::<Value>(body)
        .ok()
        .and_then(|json| json["error"]["message"].as_str().map(str::to_string))
        .unwrap_or_else(|| body.trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::secret::Secret;
    use crate::types::{EndpointHealth, EndpointKind};
    use mockito::Matcher;

    fn wav() -> AudioFile {
        AudioFile {
            bytes: b"RIFF....WAVE".to_vec(),
            file_name: "utterance.wav".into(),
            mime: "audio/wav".into(),
        }
    }

    fn endpoint(kind: EndpointKind, url: &str, key: &str) -> Endpoint {
        Endpoint {
            id: "e".into(),
            name: "audio".into(),
            kind,
            base_url: Url::parse(url).unwrap(),
            api_key: Secret::new(key),
            capabilities: vec![],
            max_num_ctx: None,
            health: EndpointHealth::default(),
        }
    }

    #[tokio::test]
    async fn one_file_goes_up_as_multipart_with_the_model_and_the_fields() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/audio/transcriptions")
            .match_header("authorization", "Bearer sk-audio")
            .match_header(
                "content-type",
                Matcher::Regex("^multipart/form-data".into()),
            )
            .match_body(Matcher::AllOf(vec![
                Matcher::Regex("name=\"model\"\r\n\r\nparakeet-tdt-0.6b\r\n".into()),
                Matcher::Regex("name=\"file\"; filename=\"utterance.wav\"".into()),
                Matcher::Regex("Content-Type: audio/wav".into()),
                Matcher::Regex("name=\"language\"\r\n\r\nen\r\n".into()),
            ]))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"text":" hello world "}"#)
            .create_async()
            .await;

        let client = OpenAiTranscribe::new("sk-audio", Url::parse(&server.url()).unwrap());
        let reply = client
            .transcribe(
                "parakeet-tdt-0.6b",
                wav(),
                vec![("language".into(), "en".into())],
            )
            .await
            .unwrap();

        mock.assert_async().await;
        assert_eq!(reply.text, "hello world");
        assert_eq!(reply.content_type, "application/json");
        assert_eq!(reply.body, br#"{"text":" hello world "}"#);
    }

    #[tokio::test]
    async fn a_plain_text_answer_is_the_transcript() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_status(200)
            .with_header("content-type", "text/plain; charset=utf-8")
            .with_body("hello\n")
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let reply = client.transcribe("m", wav(), vec![]).await.unwrap();
        assert_eq!(reply.text, "hello");
    }

    #[tokio::test]
    async fn an_upstream_error_names_no_key_and_keeps_its_reason() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_status(400)
            .with_body(r#"{"error":{"message":"bad audio for key sk-secret"}}"#)
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("sk-secret", Url::parse(&server.url()).unwrap());
        let error = client.transcribe("m", wav(), vec![]).await.unwrap_err();

        assert!(!format!("{error:#}").contains("sk-secret"), "{error:#}");
        let failure = TranscribeFailure::of(&error);
        assert!(
            matches!(&failure, TranscribeFailure::Refused { status, reason }
                if status.as_u16() == 400 && reason.starts_with("bad audio")),
            "{failure:?}"
        );
        assert!(!failure.to_string().contains(&server.url()));
    }

    #[tokio::test]
    async fn failures_are_told_apart() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_status(401)
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("k", Url::parse(&server.url()).unwrap());
        let error = client.transcribe("m", wav(), vec![]).await.unwrap_err();
        assert_eq!(
            TranscribeFailure::of(&error),
            TranscribeFailure::Unauthorized
        );

        // Nothing listens on port 9 of loopback.
        let closed = OpenAiTranscribe::new("", Url::parse("http://127.0.0.1:9").unwrap());
        let error = closed.transcribe("m", wav(), vec![]).await.unwrap_err();
        assert_eq!(
            TranscribeFailure::of(&error),
            TranscribeFailure::Unreachable
        );
    }

    #[test]
    fn an_endpoint_without_an_audio_route_or_with_a_key_in_the_clear_is_refused() {
        assert!(OpenAiTranscribe::for_endpoint(&endpoint(
            EndpointKind::Ollama,
            "http://127.0.0.1:11434",
            ""
        ))
        .is_err());
        let cleartext = OpenAiTranscribe::for_endpoint(&endpoint(
            EndpointKind::OpenAiAudio,
            "http://203.0.113.9:8000",
            "sk",
        ));
        assert!(cleartext.is_err());

        let speaches = OpenAiTranscribe::for_endpoint(&endpoint(
            EndpointKind::OpenAiAudio,
            "http://127.0.0.1:8000/v1",
            "",
        ))
        .unwrap();
        assert_eq!(
            speaches.endpoint,
            "http://127.0.0.1:8000/v1/audio/transcriptions"
        );
    }
}
