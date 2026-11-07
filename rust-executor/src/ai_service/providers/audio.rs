//! OpenAI-shaped speech to text: `POST {base}/v1/audio/transcriptions`, a multipart upload.
//!
//! Served by speaches (faster-whisper), OpenAI-shaped Parakeet wrappers, vLLM, OpenAI itself,
//! and whisper.cpp started with `--inference-path /v1/audio/transcriptions`. One call
//! transcribes one file: a stream sends each utterance as its own WAV, `/v1/audio/transcriptions`
//! forwards what its client uploaded.

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use futures::StreamExt;
use serde_json::Value;
use std::sync::atomic::{AtomicU8, Ordering};
use std::time::Duration;
use tokio::sync::mpsc;
use url::Url;

use super::http::{credentialed_http, is_transport_safe, read_capped, versioned_endpoint};
use super::http::{UpstreamStatus, CLEARTEXT_KEY_REFUSAL, MAX_BODY_BYTES};
use crate::ai_service::endpoints::{request_slot, request_timeout};
use crate::types::{AiCapability, Endpoint, ModelApiType};

/// A minute of speech takes seconds on a GPU and can take most of a minute on a CPU.
pub(crate) const REQUEST_TIMEOUT: Duration = Duration::from_secs(120);

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

    /// Transcribe one utterance, sending each piece of the transcript to `pieces` as the
    /// upstream decodes it when it streams, and answer the whole transcript. The default
    /// is one [`Self::transcribe`] call and no pieces: an upstream that does not stream.
    async fn transcribe_pieces(
        &self,
        model: &str,
        audio: AudioFile,
        pieces: &mpsc::UnboundedSender<String>,
    ) -> Result<String> {
        let _ = pieces;
        Ok(self.transcribe(model, audio, vec![]).await?.text)
    }
}

/// The endpoint serves no transcription: its kind has no `/v1/audio/transcriptions`, or its
/// capabilities leave transcription out. The caller's model choice, not a failure here.
#[derive(Debug)]
pub struct NotATranscriptionEndpoint(pub String);

impl std::fmt::Display for NotATranscriptionEndpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for NotATranscriptionEndpoint {}

/// Refuses an endpoint whose capabilities leave out transcription: it gets no audio.
pub fn require_transcription(endpoint: &Endpoint) -> Result<()> {
    if endpoint.capabilities.contains(&AiCapability::Transcription) {
        return Ok(());
    }
    Err(NotATranscriptionEndpoint(format!(
        "Endpoint '{}' does not declare the transcription capability",
        endpoint.name
    ))
    .into())
}

pub struct OpenAiTranscribe {
    http: reqwest::Client,
    api_key: String,
    /// Fully resolved, e.g. `http://127.0.0.1:8000/v1/audio/transcriptions`.
    endpoint: String,
    /// What the upstream did with `stream=true` ([`streaming`]), learned from its first
    /// answer: an endpoint kind does not say (speaches and nemo-speech are both
    /// `OPEN_AI_AUDIO`; OpenAI streams for gpt-4o-transcribe and not for whisper-1).
    streaming: AtomicU8,
    /// How long one call may take, its wait for a slot included.
    timeout: Duration,
    /// The registered endpoint, whose `max_concurrent` slots each call takes one of.
    limits: Option<Endpoint>,
}

/// [`OpenAiTranscribe::streaming`]: not asked yet, it streams, it does not.
mod streaming {
    pub const UNKNOWN: u8 = 0;
    pub const YES: u8 = 1;
    pub const NO: u8 = 2;
}

impl OpenAiTranscribe {
    pub fn new(api_key: &str, base_url: Url) -> Self {
        Self::with_limits(api_key, base_url, REQUEST_TIMEOUT, None)
    }

    fn with_limits(
        api_key: &str,
        base_url: Url,
        timeout: Duration,
        limits: Option<Endpoint>,
    ) -> Self {
        Self {
            // `expect`: the default client follows redirects, and would take the key along.
            http: credentialed_http(timeout)
                .build()
                .expect("the HTTP client builds"),
            api_key: api_key.to_string(),
            endpoint: versioned_endpoint(base_url, "audio/transcriptions"),
            streaming: AtomicU8::new(streaming::UNKNOWN),
            timeout,
            limits,
        }
    }

    /// The client for a registered endpoint, held to its `request_timeout_secs` and
    /// `max_concurrent`. Refused: Ollama and Anthropic, which have no transcription route,
    /// and a key over a transport other than https or loopback. Whether the endpoint
    /// declares transcription is [`require_transcription`]'s to check.
    pub fn for_endpoint(endpoint: &Endpoint) -> Result<Self> {
        if endpoint.kind.protocol() != ModelApiType::OpenAi {
            return Err(NotATranscriptionEndpoint(format!(
                "Endpoint '{}' ({:?}) serves no /v1/audio/transcriptions",
                endpoint.name, endpoint.kind
            ))
            .into());
        }
        if !endpoint.api_key.is_empty() && !is_transport_safe(&endpoint.base_url) {
            return Err(anyhow!(
                "{CLEARTEXT_KEY_REFUSAL} (endpoint '{}')",
                endpoint.name
            ));
        }
        Ok(Self::with_limits(
            endpoint.api_key.expose(),
            endpoint.base_url.clone(),
            request_timeout(endpoint, REQUEST_TIMEOUT),
            Some(endpoint.clone()),
        ))
    }

    /// Run `call` in a slot of the endpoint, within the timeout.
    async fn limited<T>(&self, call: impl std::future::Future<Output = Result<T>>) -> Result<T> {
        let run = async {
            let _slot = match &self.limits {
                Some(endpoint) => request_slot(endpoint).await,
                None => None,
            };
            call.await
        };
        tokio::time::timeout(self.timeout, run)
            .await
            .map_err(|elapsed| {
                anyhow::Error::new(elapsed)
                    .context(format!("{} did not answer in time", self.endpoint))
            })?
    }

    /// One call that does not stream: the answer's body, content type and transcript.
    async fn transcribe_once(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        let response = self.send(model, audio, fields).await?;
        self.reply(response).await
    }

    /// [`RemoteTranscribe::transcribe_pieces`], outside a slot.
    async fn transcribe_streamed(
        &self,
        model: &str,
        audio: AudioFile,
        pieces: &mpsc::UnboundedSender<String>,
    ) -> Result<String> {
        let state = self.streaming.load(Ordering::Relaxed);
        if state == streaming::NO {
            return Ok(self.transcribe_once(model, audio, vec![]).await?.text);
        }
        let again = AudioFile {
            bytes: audio.bytes.clone(),
            file_name: audio.file_name.clone(),
            mime: audio.mime.clone(),
        };
        let response = match self
            .send(model, audio, vec![("stream".into(), "true".into())])
            .await
        {
            Ok(response) => response,
            Err(e) if state == streaming::UNKNOWN && refuses_a_field(&e) => {
                log::info!(
                    "{} refused `stream` ({e}); transcribing per utterance",
                    self.endpoint
                );
                // A 400 about the audio itself fails the retry too: only a retry that
                // succeeds shows the refusal was about `stream`.
                let text = self.transcribe_once(model, again, vec![]).await?.text;
                self.streaming.store(streaming::NO, Ordering::Relaxed);
                return Ok(text);
            }
            Err(e) => return Err(e),
        };
        if content_type(&response).starts_with("text/event-stream") {
            self.streaming.store(streaming::YES, Ordering::Relaxed);
            return self.read_stream(response, pieces).await;
        }
        self.streaming.store(streaming::NO, Ordering::Relaxed);
        Ok(self.reply(response).await?.text)
    }

    /// POST `audio` with `model` and `fields`; the response when its status is a success.
    async fn send(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<reqwest::Response> {
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
        if !response.status().is_success() {
            return Err(
                UpstreamStatus::read(response, &self.endpoint, &self.api_key)
                    .await
                    .into(),
            );
        }
        Ok(response)
    }

    /// The answer of a request that did not stream: its body, content type and transcript.
    /// Refused past [`MAX_BODY_BYTES`].
    async fn reply(&self, response: reqwest::Response) -> Result<TranscribeReply> {
        let content_type = content_type(&response);
        let (body, more) = read_capped(response, MAX_BODY_BYTES).await.map_err(|e| {
            anyhow::Error::new(e).context(format!("Could not read the answer of {}", self.endpoint))
        })?;
        if more {
            return Err(anyhow!(
                "The answer of {} is longer than {MAX_BODY_BYTES} bytes",
                self.endpoint
            ));
        }
        let text = transcript_of(&body, &content_type)
            .map_err(|e| anyhow!("Could not read the answer of {}: {e}", self.endpoint))?;
        Ok(TranscribeReply {
            text,
            body,
            content_type,
        })
    }

    /// Read a `text/event-stream` answer, sending each piece to `pieces`; the transcript.
    /// Refused past [`MAX_BODY_BYTES`], and when it stopped before its end marker
    /// ([`StreamEnd`]).
    async fn read_stream(
        &self,
        response: reqwest::Response,
        pieces: &mpsc::UnboundedSender<String>,
    ) -> Result<String> {
        let mut stream = response.bytes_stream();
        // Bytes, not text: a chunk can end inside a multi-byte character.
        let mut buffer: Vec<u8> = Vec::new();
        let mut read = 0usize;
        let mut sent = String::new();
        let mut end = StreamEnd::default();
        let mut take = |event: StreamEvent, sent: &mut String| {
            if let Some(piece) = end.take(event) {
                sent.push_str(&piece);
                let _ = pieces.send(piece);
            }
        };
        while let Some(chunk) = stream.next().await {
            let chunk = chunk.map_err(|e| {
                anyhow::Error::new(e).context(format!("The stream of {} broke off", self.endpoint))
            })?;
            read += chunk.len();
            if read > MAX_BODY_BYTES {
                return Err(anyhow!(
                    "The stream of {} is longer than {MAX_BODY_BYTES} bytes",
                    self.endpoint
                ));
            }
            buffer.extend_from_slice(&chunk);
            while let Some(at) = buffer.iter().position(|b| *b == b'\n') {
                let line: Vec<u8> = buffer.drain(..=at).collect();
                take(
                    stream_event(String::from_utf8_lossy(&line).trim())?,
                    &mut sent,
                );
            }
        }
        take(
            stream_event(String::from_utf8_lossy(&buffer).trim())?,
            &mut sent,
        );
        end.transcript(sent)
            .map_err(|e| anyhow!("The stream of {} {e}", self.endpoint))
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
        self.limited(self.transcribe_once(model, audio, fields))
            .await
    }

    /// Asks for `stream=true` until the upstream has shown what it does with it: an
    /// `text/event-stream` answer streams (each delta is a piece); a JSON one does not (the
    /// field is ignored: nemo-speech, whisper-1), and a 400 or 422 refuses the field, so the
    /// utterance is asked again without it. Either way the stream does not ask again. The
    /// retry runs in the same slot and within the same timeout.
    async fn transcribe_pieces(
        &self,
        model: &str,
        audio: AudioFile,
        pieces: &mpsc::UnboundedSender<String>,
    ) -> Result<String> {
        self.limited(self.transcribe_streamed(model, audio, pieces))
            .await
    }
}

/// The answer's content type; JSON when it names none.
fn content_type(response: &reqwest::Response) -> String {
    response
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/json")
        .to_string()
}

/// A 400 or 422: the upstream refused a field of the request.
fn refuses_a_field(error: &anyhow::Error) -> bool {
    error
        .downcast_ref::<UpstreamStatus>()
        .is_some_and(|s| matches!(s.status.as_u16(), 400 | 422))
}

/// One line of a streamed transcription.
#[derive(Debug, PartialEq)]
enum StreamEvent {
    /// A bare segment (`{"text": ...}`), spacing as sent. Such a stream has no end marker.
    Piece(String),
    /// A piece of a stream that ends with a marker: an OpenAI `transcript.text.delta` or a
    /// vLLM `transcription.chunk`. Spacing as sent.
    Delta(String),
    /// The whole transcript (`transcript.text.done`).
    Done(String),
    /// `[DONE]`.
    End,
    /// A blank line, a comment, `event:`, the role chunk, a line that is not JSON.
    Nothing,
}

/// What a stream's events said of its whole: the authoritative transcript, and whether it
/// reached its end.
#[derive(Default)]
struct StreamEnd {
    /// A [`StreamEvent::Delta`] came: the stream must end with a marker.
    framed: bool,
    /// `[DONE]` came.
    ended: bool,
    /// `transcript.text.done` came, with this text.
    done: Option<String>,
}

impl StreamEnd {
    /// Note `event`; the piece of the transcript in it.
    fn take(&mut self, event: StreamEvent) -> Option<String> {
        match event {
            StreamEvent::Piece(piece) => Some(piece),
            StreamEvent::Delta(piece) => {
                self.framed = true;
                Some(piece)
            }
            StreamEvent::Done(text) => {
                self.done = Some(text);
                None
            }
            StreamEvent::End => {
                self.ended = true;
                None
            }
            StreamEvent::Nothing => None,
        }
    }

    /// The transcript of a stream whose pieces were `sent`: `transcript.text.done`'s text,
    /// else the pieces. An error when a framed stream stopped before its end marker: its
    /// text is cut off.
    fn transcript(self, sent: String) -> Result<String> {
        if self.framed && !self.ended && self.done.is_none() {
            return Err(anyhow!("stopped before its end marker"));
        }
        Ok(self.done.unwrap_or(sent).trim().to_string())
    }
}

/// The event of the SSE line `line`. The shapes: OpenAI's and speaches'
/// `transcript.text.delta` / `transcript.text.done`, vLLM's `transcription.chunk`
/// (`choices[0].delta.content`), and a bare `{"text": ...}` per segment (speaches before
/// the OpenAI events). An `error` event fails the transcription; its message stays out of
/// the error, as it can carry speech or a key.
fn stream_event(line: &str) -> Result<StreamEvent> {
    let Some(data) = line.strip_prefix("data:").map(str::trim) else {
        return Ok(StreamEvent::Nothing);
    };
    if data.is_empty() {
        return Ok(StreamEvent::Nothing);
    }
    if data == "[DONE]" {
        return Ok(StreamEvent::End);
    }
    let Ok(event) = serde_json::from_str::<Value>(data) else {
        log::debug!(
            "Skipped a transcription event that is not JSON ({} bytes)",
            data.len()
        );
        return Ok(StreamEvent::Nothing);
    };
    if event.get("error").is_some() {
        return Err(anyhow!("The transcription stream sent an error event"));
    }
    let text = |value: Option<&Value>| {
        value
            .and_then(Value::as_str)
            .filter(|piece| !piece.is_empty())
            .map(str::to_string)
    };
    let event = match event.get("type").and_then(Value::as_str) {
        Some("transcript.text.delta") => text(event.get("delta")).map(StreamEvent::Delta),
        Some("transcript.text.done") => text(event.get("text")).map(StreamEvent::Done),
        Some(_) => None,
        None => match event.pointer("/choices/0/delta") {
            Some(delta) => text(delta.get("content")).map(StreamEvent::Delta),
            None => text(event.get("text")).map(StreamEvent::Piece),
        },
    };
    Ok(event.unwrap_or(StreamEvent::Nothing))
}

/// The transcript in an answer: `text` of a JSON answer (`json`, `verbose_json`), the text of
/// an event stream (a client's `stream=true`), the text of the cues of subtitles (`srt`,
/// `vtt`), else the body itself (`text`).
fn transcript_of(body: &[u8], content_type: &str) -> Result<String> {
    if content_type.starts_with("application/json") {
        let json: Value = serde_json::from_slice(body)?;
        return json
            .get("text")
            .and_then(Value::as_str)
            .map(|text| text.trim().to_string())
            .ok_or_else(|| anyhow!("the answer has no `text`"));
    }
    let body = String::from_utf8_lossy(body);
    if content_type.starts_with("text/event-stream") {
        let mut pieces = String::new();
        let mut end = StreamEnd::default();
        for line in body.lines() {
            if let Some(piece) = end.take(stream_event(line.trim())?) {
                pieces.push_str(&piece);
            }
        }
        return Ok(end.done.unwrap_or(pieces).trim().to_string());
    }
    if body.lines().any(is_cue_timing) {
        return Ok(subtitle_text(&body));
    }
    Ok(body.trim().to_string())
}

/// A cue's timing line of SRT or WebVTT: `00:00:01,500 --> 00:00:03,000`, settings after.
fn is_cue_timing(line: &str) -> bool {
    let is_time = |part: &str| {
        !part.is_empty()
            && part.contains(':')
            && part
                .chars()
                .all(|c| c.is_ascii_digit() || matches!(c, ':' | ',' | '.'))
    };
    line.split_once("-->").is_some_and(|(start, rest)| {
        is_time(start.trim()) && rest.split_whitespace().next().is_some_and(is_time)
    })
}

/// The words of SRT or WebVTT subtitles: the cue text, without the header, the cue numbers
/// and the timings.
fn subtitle_text(body: &str) -> String {
    body.lines()
        .map(str::trim)
        .filter(|line| {
            !line.is_empty()
                && !line.starts_with("WEBVTT")
                && !line.chars().all(|c| c.is_ascii_digit())
                && !is_cue_timing(line)
        })
        .collect::<Vec<_>>()
        .join(" ")
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
    /// The upstream answered, but the answer reported a failure (an `error` event) or could
    /// not be read.
    BadAnswer,
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
        let http = || {
            error
                .chain()
                .filter_map(|cause| cause.downcast_ref::<reqwest::Error>())
        };
        let elapsed = error
            .chain()
            .any(|cause| cause.is::<tokio::time::error::Elapsed>());
        if elapsed || http().any(reqwest::Error::is_timeout) {
            Self::Timeout
        } else if http().next().is_none() || http().any(|e| e.is_body() || e.is_decode()) {
            // No transport error (an `error` event, an answer that does not parse), or one
            // while the answer was read: the endpoint was reached.
            Self::BadAnswer
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
            Self::BadAnswer => {
                f.write_str("The transcription endpoint's answer was a failure or unreadable")
            }
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
    use super::super::http::MAX_BODY_BYTES;
    use super::*;
    use crate::secret::Secret;
    use crate::types::{AiCapability, EndpointHealth, EndpointKind};
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
            capabilities: kind.default_capabilities(),
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
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

    #[test]
    fn each_streamed_shape_reads_as_its_piece() {
        let piece = |s: &str| Ok::<_, ()>(StreamEvent::Piece(s.into()));
        let delta = |s: &str| Ok::<_, ()>(StreamEvent::Delta(s.into()));
        let read = |line: &str| stream_event(line).map_err(|_| ());
        assert_eq!(
            read(r#"data: {"type":"transcript.text.delta","delta":" Hello"}"#),
            delta(" Hello")
        );
        assert_eq!(
            read(r#"data: {"type":"transcript.text.done","text":"Hello world"}"#),
            Ok(StreamEvent::Done("Hello world".into()))
        );
        assert_eq!(
            read(
                r#"data: {"object":"transcription.chunk","choices":[{"delta":{"content":"Hi"}}]}"#
            ),
            delta("Hi")
        );
        assert_eq!(read("data: [DONE]"), Ok(StreamEvent::End));
        assert_eq!(read(r#"data: {"text":" a segment"}"#), piece(" a segment"));
        for nothing in [
            "",
            ": keep-alive",
            "event: transcript.text.delta",
            "data: keep-alive",
            r#"data: {"choices":[{"delta":{"role":"assistant","content":""}}]}"#,
            r#"data: {"type":"transcript.text.delta","delta":""}"#,
        ] {
            assert_eq!(read(nothing), Ok(StreamEvent::Nothing), "{nothing}");
        }
        assert!(stream_event(r#"data: {"error":{"message":"engine failed"}}"#).is_err());
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

    // A client's `stream=true` on /v1/audio/transcriptions: the transcript of the event
    // stream was its whole body, keys and event names too, and was billed at about 3x.
    #[tokio::test]
    async fn the_transcript_of_a_streamed_answer_is_its_text() {
        let sse = concat!(
            "data: {\"type\":\"transcript.text.delta\",\"delta\":\"Hello\"}\n\n",
            "data: {\"type\":\"transcript.text.delta\",\"delta\":\" world\"}\n\n",
            "data: {\"type\":\"transcript.text.done\",\"text\":\"Hello world\"}\n\n",
            "data: [DONE]\n\n",
        );
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "text/event-stream")
            .with_body(sse)
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let reply = client
            .transcribe("m", wav(), vec![("stream".into(), "true".into())])
            .await
            .unwrap();
        assert_eq!(reply.text, "Hello world");
        assert_eq!(
            reply.body,
            sse.as_bytes(),
            "the client gets the events as sent"
        );
    }

    // An answer that failed or could not be read was "unreachable": the endpoint answered.
    #[tokio::test]
    async fn a_failed_or_unreadable_answer_is_not_unreachable() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "text/event-stream")
            .with_body("data: {\"error\":{\"message\":\"engine failed\"}}\n\n")
            .create_async()
            .await;
        server
            .mock("POST", "/unreadable/v1/audio/transcriptions")
            .with_header("content-type", "application/json")
            .with_body("not json")
            .create_async()
            .await;
        let base = Url::parse(&server.url()).unwrap();
        let (pieces, _) = mpsc::unbounded_channel();

        let streamed = OpenAiTranscribe::new("", base.clone());
        let error = streamed
            .transcribe_pieces("m", wav(), &pieces)
            .await
            .unwrap_err();
        assert_eq!(TranscribeFailure::of(&error), TranscribeFailure::BadAnswer);
        let error = streamed
            .transcribe("m", wav(), vec![("stream".into(), "true".into())])
            .await
            .unwrap_err();
        assert_eq!(TranscribeFailure::of(&error), TranscribeFailure::BadAnswer);

        let unreadable = OpenAiTranscribe::new("", base.join("unreadable/").unwrap());
        let error = unreadable.transcribe("m", wav(), vec![]).await.unwrap_err();
        assert_eq!(TranscribeFailure::of(&error), TranscribeFailure::BadAnswer);
        assert!(!TranscribeFailure::BadAnswer
            .to_string()
            .contains(&server.url()));
    }

    // A 400 about the audio turned streaming off for the whole stream.
    #[tokio::test]
    async fn a_refusal_of_the_audio_keeps_streaming_on() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_status(400)
            .with_body("audio too short")
            .expect(2)
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let (pieces, _) = mpsc::unbounded_channel();
        assert!(client.transcribe_pieces("m", wav(), &pieces).await.is_err());
        assert_eq!(
            client.streaming.load(Ordering::Relaxed),
            streaming::UNKNOWN,
            "one rejected utterance does not settle whether the server streams"
        );
    }

    // One `data:` line that is not JSON failed the whole utterance.
    #[tokio::test]
    async fn a_stream_line_that_is_not_json_is_skipped() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "text/event-stream")
            .with_body(concat!(
                "data: {\"type\":\"transcript.text.delta\",\"delta\":\"Hello\"}\n\n",
                "data: keep-alive\n\n",
                "data: {\"type\":\"transcript.text.delta\",\"delta\":\" world\"}\n\n",
                "data: [DONE]\n\n",
            ))
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let (pieces, _) = mpsc::unbounded_channel();
        let text = client.transcribe_pieces("m", wav(), &pieces).await.unwrap();
        assert_eq!(text, "Hello world");
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

    // An endpoint whose capabilities leave out transcription got audio all the same.
    #[test]
    fn an_endpoint_that_does_not_declare_transcription_is_refused() {
        let mut speech_only = endpoint(EndpointKind::OpenAiAudio, "http://127.0.0.1:8000", "");
        speech_only.capabilities = vec![AiCapability::Speech];
        let refused = require_transcription(&speech_only).unwrap_err();
        assert!(refused
            .downcast_ref::<NotATranscriptionEndpoint>()
            .is_some());
        speech_only.capabilities.push(AiCapability::Transcription);
        assert!(require_transcription(&speech_only).is_ok());
        assert!(
            OpenAiTranscribe::for_endpoint(&speech_only).is_ok(),
            "the route and the key are for_endpoint's"
        );
    }

    // `reply` read the whole of a successful answer, however long.
    #[tokio::test]
    async fn an_answer_longer_than_the_cap_is_refused() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "text/plain")
            .with_body("a".repeat(MAX_BODY_BYTES + 1))
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let error = client.transcribe("m", wav(), vec![]).await.unwrap_err();
        assert_eq!(TranscribeFailure::of(&error), TranscribeFailure::BadAnswer);
    }

    // `read_stream` kept a line without a newline in memory, however long.
    #[tokio::test]
    async fn a_stream_longer_than_the_cap_is_refused() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "text/event-stream")
            .with_body(format!("data: {}", "a".repeat(MAX_BODY_BYTES)))
            .create_async()
            .await;
        let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
        let (pieces, _) = mpsc::unbounded_channel();
        let error = client
            .transcribe_pieces("m", wav(), &pieces)
            .await
            .unwrap_err();
        assert_eq!(TranscribeFailure::of(&error), TranscribeFailure::BadAnswer);
    }

    // The message of an `error` event went into the error, and from there into the log: an
    // upstream can put speech or a key in it.
    #[test]
    fn an_error_event_keeps_its_message_out_of_the_error() {
        let error =
            stream_event(r#"data: {"error":{"message":"heard: my pin is 1234"}}"#).unwrap_err();
        assert!(!format!("{error:#}").contains("1234"), "{error:#}");
    }

    // A stream of OpenAI events or vLLM chunks that stopped before its end marker was read
    // as the whole transcript, and billed.
    #[tokio::test]
    async fn a_stream_that_stops_before_its_end_is_a_bad_answer() {
        let cases = [
            (
                "OpenAI",
                "data: {\"type\":\"transcript.text.delta\",\"delta\":\"Hello\"}\n\n",
                false,
            ),
            (
                "vLLM",
                "data: {\"object\":\"transcription.chunk\",\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}\n\n",
                false,
            ),
            ("bare segments", "data: {\"text\":\"Hello\"}\n\n", true),
        ];
        for (shape, body, ends_well) in cases {
            let mut server = mockito::Server::new_async().await;
            server
                .mock("POST", "/v1/audio/transcriptions")
                .with_header("content-type", "text/event-stream")
                .with_body(body)
                .create_async()
                .await;
            let client = OpenAiTranscribe::new("", Url::parse(&server.url()).unwrap());
            let (pieces, _) = mpsc::unbounded_channel();
            let outcome = client.transcribe_pieces("m", wav(), &pieces).await;
            match outcome {
                Ok(text) => {
                    assert!(ends_well, "{shape}: {text}");
                    assert_eq!(text, "Hello", "{shape}");
                }
                Err(error) => {
                    assert!(!ends_well, "{shape}: {error:#}");
                    assert_eq!(
                        TranscribeFailure::of(&error),
                        TranscribeFailure::BadAnswer,
                        "{shape}"
                    );
                }
            }
        }
    }

    // A piece that is only a space was dropped, so the words on each side of it ran together.
    #[test]
    fn a_piece_that_is_only_spacing_is_kept() {
        assert_eq!(
            stream_event(r#"data: {"type":"transcript.text.delta","delta":" "}"#).unwrap(),
            StreamEvent::Delta(" ".into())
        );
    }

    // A transcription call took no slot of its endpoint's `max_concurrent`.
    #[tokio::test]
    async fn a_call_waits_for_a_slot_of_its_endpoint() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("POST", "/v1/audio/transcriptions")
            .with_header("content-type", "application/json")
            .with_body(r#"{"text":"hello"}"#)
            .create_async()
            .await;
        let mut capped = endpoint(EndpointKind::OpenAiAudio, &server.url(), "");
        capped.id = uuid::Uuid::new_v4().to_string();
        capped.max_concurrent = Some(1);
        let client = OpenAiTranscribe::for_endpoint(&capped).unwrap();

        let held = crate::ai_service::endpoints::request_slot(&capped).await;
        assert!(held.is_some());
        let waiting = tokio::time::timeout(
            Duration::from_millis(300),
            client.transcribe("m", wav(), vec![]),
        )
        .await;
        assert!(waiting.is_err(), "the call ran beside the held slot");
        drop(held);
        let reply = client.transcribe("m", wav(), vec![]).await.unwrap();
        assert_eq!(reply.text, "hello");
    }

    // A transcription call took the fixed 120 s, not the endpoint's `request_timeout_secs`.
    #[tokio::test]
    async fn a_call_ends_at_its_endpoints_timeout() {
        // Takes the connection and never answers.
        let silent = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let url = format!("http://{}", silent.local_addr().unwrap());
        tokio::spawn(async move {
            let mut held = Vec::new();
            while let Ok((socket, _)) = silent.accept().await {
                held.push(socket);
            }
        });
        let mut quick = endpoint(EndpointKind::OpenAiAudio, &url, "");
        quick.request_timeout_secs = Some(1);
        let client = OpenAiTranscribe::for_endpoint(&quick).unwrap();
        let (pieces, _) = mpsc::unbounded_channel();

        let outcome = tokio::time::timeout(
            Duration::from_secs(10),
            client.transcribe_pieces("m", wav(), &pieces),
        )
        .await
        .expect("the endpoint's timeout ends the call");
        assert_eq!(
            TranscribeFailure::of(&outcome.unwrap_err()),
            TranscribeFailure::Timeout
        );
    }

    // `response_format=srt` or `vtt`: the cue numbers and the times were billed as words.
    #[test]
    fn the_transcript_of_subtitles_is_their_text() {
        let srt = "1\n00:00:00,000 --> 00:00:01,500\nHello there\n\n2\n00:00:01,500 --> 00:00:03,000\nGeneral Kenobi\n";
        assert_eq!(
            transcript_of(srt.as_bytes(), "text/plain; charset=utf-8").unwrap(),
            "Hello there General Kenobi"
        );
        let vtt = "WEBVTT\n\n00:00:00.000 --> 00:00:01.500\nHello there\n\n00:00:01.500 --> 00:00:03.000\nGeneral Kenobi\n";
        assert_eq!(
            transcript_of(vtt.as_bytes(), "text/vtt").unwrap(),
            "Hello there General Kenobi"
        );
        assert_eq!(
            transcript_of(b"2 cats at 10:30\n", "text/plain").unwrap(),
            "2 cats at 10:30",
            "plain text keeps its numbers"
        );
    }
}
