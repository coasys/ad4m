//! `/v1` as an aggregator: a request for a model on a registered endpoint is forwarded to
//! that endpoint as the client wrote it (plan §5).
//!
//! Only `model` is rewritten, to the id the endpoint knows (`ModelApi::model`), and back to
//! the client's name in the answer. Every other field (`tools`, `tool_choice`,
//! `response_format`, fields this executor has never heard of) reaches the upstream, and an
//! SSE stream comes back event by event. A stream asks for
//! `stream_options.include_usage`, so the bill is the upstream's count; the usage-only
//! event it adds is not passed on to a client that did not ask for it. Where the upstream
//! reports no usage, the request and the generated text are estimated.
//!
//! When `/v1` forwards and when the executor answers itself: [`Upstream::for_chat`] and
//! [`Upstream::of`]. Failures map per §5.4 ([`OpenAIError::from_upstream`]); one that says
//! the endpoint is down or refuses the key re-checks its health, and a 404 marks the model
//! `MISSING`. An endpoint with `max_concurrent` lets that many requests through at once;
//! the wait counts against the request's timeout.

use std::time::Duration;

use axum::{
    body::{Body, Bytes},
    http::{header, StatusCode},
    response::Response,
};
use futures::StreamExt;
use serde_json::{json, Value};
use tokio::sync::OwnedSemaphorePermit;
use tokio::time::Instant;

use super::errors::OpenAIError;
use super::model_selector::Resolved;
use crate::ai_service::endpoints::{
    check_health, publish_model_status, request_slot, request_timeout,
};
use crate::ai_service::estimate_token_count;
use crate::ai_service::providers::http::{
    credentialed_http, is_transport_safe, versioned_endpoint, UpstreamStatus, UpstreamUnreachable,
    CLEARTEXT_KEY_REFUSAL,
};
use crate::billing::check_compute_credits;
use crate::types::{Endpoint, EndpointKind, Model, ModelState, ModelStatus, RequestContext};

/// How long a stream may take to start answering (plan §5.4).
const FIRST_BYTE_TIMEOUT: Duration = Duration::from_secs(30);

/// The bound for a request whose timeout the route sets; every request sets one.
const CLIENT_TIMEOUT: Duration = Duration::from_secs(3600);

lazy_static! {
    /// One connection pool for every forwarded request. Follows no redirects
    /// (`credentialed_http`): a key goes only to the endpoint it was set for.
    static ref HTTP: reqwest::Client = credentialed_http(CLIENT_TIMEOUT)
        .build()
        .expect("the HTTP client builds");
}

/// An upstream route `/v1` forwards to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Route {
    ChatCompletions,
    Completions,
    Speech,
}

impl Route {
    fn path(self) -> &'static str {
        match self {
            Route::ChatCompletions => "chat/completions",
            Route::Completions => "completions",
            Route::Speech => "audio/speech",
        }
    }

    /// Unless the endpoint sets `request_timeout_secs`.
    fn default_timeout(self) -> Duration {
        match self {
            Route::ChatCompletions | Route::Completions => Duration::from_secs(300),
            Route::Speech => Duration::from_secs(120),
        }
    }
}

/// A model on an endpoint that speaks OpenAI's protocol, which `/v1` forwards to.
pub struct Upstream {
    pub model: Model,
    pub endpoint: Endpoint,
    /// The id the endpoint knows the model by.
    pub remote_model: String,
}

impl Upstream {
    /// The upstream of `resolved` when `/v1` forwards to it: a model on an endpoint of any
    /// kind but Anthropic (Ollama through its OpenAI-compatible `/v1`). `None`: the executor
    /// answers, through the model's own client (Anthropic is translated there) or a local
    /// model.
    pub fn of(resolved: &Resolved) -> Option<Upstream> {
        let endpoint = resolved.endpoint.clone()?;
        if endpoint.kind == EndpointKind::Anthropic {
            return None;
        }
        Some(Upstream {
            remote_model: resolved.remote_model()?.to_string(),
            model: resolved.model.clone(),
            endpoint,
        })
    }

    /// The upstream of a `/v1/chat/completions` `body` for `resolved`, by the rule of
    /// [`Self::of`] plus one: a request with active tools (`tools` given, `tool_choice` not
    /// `"none"`) is forwarded only to a model that takes tools as definitions
    /// (`ModelApi::uses_native_tools`). For any other, the executor renders the tools into
    /// the prompt and reads the calls back out of the text.
    pub fn for_chat(resolved: &Resolved, body: &Value) -> Option<Upstream> {
        let upstream = Self::of(resolved)?;
        let takes_tools = || {
            resolved
                .model
                .api
                .as_ref()
                .is_some_and(|api| api.uses_native_tools())
        };
        (!tools_active(body) || takes_tools()).then_some(upstream)
    }
}

fn tools_active(body: &Value) -> bool {
    let has_tools = body
        .get("tools")
        .and_then(Value::as_array)
        .is_some_and(|tools| !tools.is_empty());
    has_tools && body.get("tool_choice").and_then(Value::as_str) != Some("none")
}

/// Map a failure of `upstream` for the client, and act on what it says about the endpoint:
/// down or refusing the key re-checks its health (which publishes the change), a missing
/// model is published `MISSING`.
pub(super) fn upstream_failure(
    endpoint: &Endpoint,
    remote_model: &str,
    error: anyhow::Error,
) -> OpenAIError {
    log::warn!("AI endpoint {:?} ({remote_model}): {error}", endpoint.name);
    let mapped = OpenAIError::from_upstream(&error);
    match mapped.error.code.as_deref() {
        Some("upstream_unavailable" | "upstream_auth") => {
            let endpoint = endpoint.clone();
            tokio::spawn(async move {
                if let Err(e) = check_health(&endpoint).await {
                    log::warn!("AI endpoint health after a failed request: {e}");
                }
            });
        }
        Some("model_not_found") => {
            let (endpoint_id, remote_model) = (endpoint.id.clone(), remote_model.to_string());
            tokio::spawn(async move {
                let status = ModelStatus {
                    state: ModelState::Missing,
                    progress: None,
                    detail: Some("The endpoint does not serve this model".into()),
                };
                publish_model_status(&endpoint_id, &remote_model, status).await;
            });
        }
        _ => {}
    }
    mapped
}

/// An upstream answer with a success status, and the slot it holds until it is read.
struct Answer {
    response: reqwest::Response,
    deadline: Instant,
    _slot: Option<OwnedSemaphorePermit>,
}

/// Send `body` to `route` of `upstream`. Waits for a slot when the endpoint caps its
/// concurrency, then for the answer: within the request timeout, or for a stream, within
/// [`FIRST_BYTE_TIMEOUT`] too.
async fn send(
    upstream: &Upstream,
    route: Route,
    body: &Value,
    stream: bool,
) -> Result<Answer, OpenAIError> {
    let endpoint = &upstream.endpoint;
    let api_key = endpoint.api_key.expose();
    if !api_key.is_empty() && !is_transport_safe(&endpoint.base_url) {
        log::warn!(
            "AI endpoint {:?}: {CLEARTEXT_KEY_REFUSAL} ({})",
            endpoint.name,
            endpoint.base_url
        );
        return Err(OpenAIError::internal(CLEARTEXT_KEY_REFUSAL));
    }
    let url = versioned_endpoint(endpoint.base_url.clone(), route.path());
    let timeout = request_timeout(endpoint, route.default_timeout());
    let deadline = Instant::now() + timeout;
    let timed_out = |waited: &str| {
        upstream_failure(
            endpoint,
            &upstream.remote_model,
            UpstreamUnreachable {
                url: url.clone(),
                timed_out: true,
                detail: format!("no answer {waited}"),
            }
            .into(),
        )
    };

    let slot = tokio::time::timeout_at(deadline, request_slot(endpoint))
        .await
        .map_err(|_| timed_out("while queued for a slot"))?;

    let mut request = HTTP
        .post(&url)
        .json(body)
        .timeout(deadline.saturating_duration_since(Instant::now()));
    if !api_key.is_empty() {
        request = request.bearer_auth(api_key);
    }
    let first_byte = match stream {
        true => deadline.min(Instant::now() + FIRST_BYTE_TIMEOUT),
        false => deadline,
    };
    let response = match tokio::time::timeout_at(first_byte, request.send()).await {
        Err(_) => return Err(timed_out("in time")),
        Ok(Err(e)) => {
            let unreachable = UpstreamUnreachable::new(&url, &e, api_key);
            return Err(upstream_failure(
                endpoint,
                &upstream.remote_model,
                unreachable.into(),
            ));
        }
        Ok(Ok(response)) => response,
    };
    if !response.status().is_success() {
        let answered = UpstreamStatus::read(response, &url, api_key).await;
        return Err(upstream_failure(
            endpoint,
            &upstream.remote_model,
            answered.into(),
        ));
    }
    Ok(Answer {
        response,
        deadline,
        _slot: slot,
    })
}

/// Who pays for a forwarded completion, what the request alone is estimated at, and what
/// the request's log line names.
struct Bill {
    bill_to: Option<String>,
    model_id: String,
    prompt_estimate: usize,
    log_label: String,
    started: std::time::Instant,
}

impl Bill {
    /// Bill the upstream's `usage` when it reported one, else the estimate of the prompt
    /// plus `generated`, and log the request.
    fn settle(&self, usage: Option<&Value>, generated: &str) {
        let reported = usage.and_then(usage_tokens);
        let tokens = reported.unwrap_or_else(|| {
            self.prompt_estimate
                .saturating_add(estimate_token_count(generated))
        });
        log::info!(
            "🤖 {} tokens={tokens}{} latency={}ms",
            self.log_label,
            if reported.is_some() {
                ""
            } else {
                " (estimated)"
            },
            self.started.elapsed().as_millis()
        );
        let Some(agent_did) = &self.bill_to else {
            return;
        };
        // An InsufficientCredits here is logged by bill_ai_operation; the answer has gone.
        let _ = crate::billing::bill_ai_operation(
            agent_did,
            &self.model_id,
            "ai_prompt",
            tokens,
            "tokens",
        );
    }
}

fn usage_tokens(usage: &Value) -> Option<usize> {
    let count = |key: &str| usage.get(key).and_then(Value::as_u64);
    let total = count("total_tokens")
        .or_else(|| Some(count("prompt_tokens").unwrap_or(0) + count("completion_tokens")?))?;
    usize::try_from(total).ok()
}

/// The text a completion request carries, for the estimate.
fn prompt_text(body: &Value) -> String {
    let mut text = String::new();
    for key in ["messages", "prompt", "tools"] {
        if let Some(value) = body.get(key) {
            text.push_str(&value.to_string());
        }
    }
    text
}

/// The text an answer or a stream event generated: content, completion text and the
/// arguments of tool calls.
fn generated_text(answer: &Value) -> String {
    let mut text = String::new();
    let choices = answer.get("choices").and_then(Value::as_array);
    for choice in choices.into_iter().flatten() {
        for message in [choice.get("message"), choice.get("delta")]
            .into_iter()
            .flatten()
        {
            if let Some(content) = message.get("content").and_then(Value::as_str) {
                text.push_str(content);
            }
            let calls = message.get("tool_calls").and_then(Value::as_array);
            for call in calls.into_iter().flatten() {
                if let Some(arguments) = call.pointer("/function/arguments").and_then(Value::as_str)
                {
                    text.push_str(arguments);
                }
            }
        }
        if let Some(completion) = choice.get("text").and_then(Value::as_str) {
            text.push_str(completion);
        }
    }
    text
}

/// Forward a chat or text completion `body` for `auth` to `upstream`, the answer back with
/// the client's `model`, and bill the caller's user from its usage.
pub async fn forward_completion(
    auth: &RequestContext,
    upstream: Upstream,
    route: Route,
    mut body: Value,
) -> Result<Response, OpenAIError> {
    if let Some(agent_did) = auth.user_did() {
        check_compute_credits(agent_did)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
    }
    let client_model = body.get("model").cloned().unwrap_or(Value::Null);
    body["model"] = Value::String(upstream.remote_model.clone());
    let stream = body.get("stream").and_then(Value::as_bool) == Some(true);
    let client_wants_usage = body
        .pointer("/stream_options/include_usage")
        .and_then(Value::as_bool)
        == Some(true);
    if stream {
        match body
            .get_mut("stream_options")
            .and_then(Value::as_object_mut)
        {
            Some(options) => {
                options.insert("include_usage".into(), Value::Bool(true));
            }
            None => body["stream_options"] = json!({ "include_usage": true }),
        }
    }
    let bill = Bill {
        bill_to: auth.user_did().map(str::to_string),
        model_id: upstream.model.id.clone(),
        prompt_estimate: estimate_token_count(&prompt_text(&body)),
        log_label: format!(
            "/v1/{} model={} endpoint={} stream={stream}",
            route.path(),
            upstream.model.name,
            upstream.endpoint.name
        ),
        started: std::time::Instant::now(),
    };

    let answer = send(&upstream, route, &body, stream).await?;
    if stream {
        Ok(stream_back(
            auth,
            upstream,
            answer,
            client_model,
            client_wants_usage,
            bill,
        ))
    } else {
        answer_back(upstream, answer, client_model, bill).await
    }
}

/// The whole body of a non-streamed answer; the request's timeout bounds the read too.
async fn read_body(upstream: &Upstream, answer: Answer) -> Result<Bytes, OpenAIError> {
    answer.response.bytes().await.map_err(|e| {
        let endpoint = &upstream.endpoint;
        let failed =
            UpstreamUnreachable::new(endpoint.base_url.as_str(), &e, endpoint.api_key.expose());
        upstream_failure(endpoint, &upstream.remote_model, failed.into())
    })
}

/// A non-streamed answer, with the client's `model`, billed.
async fn answer_back(
    upstream: Upstream,
    answer: Answer,
    client_model: Value,
    bill: Bill,
) -> Result<Response, OpenAIError> {
    let bytes = read_body(&upstream, answer).await?;
    let mut json: Value = serde_json::from_slice(&bytes).map_err(|_| {
        upstream_failure(
            &upstream.endpoint,
            &upstream.remote_model,
            anyhow::anyhow!("the endpoint answered a body that is not JSON"),
        )
    })?;
    bill.settle(json.get("usage"), &generated_text(&json));
    if json.get("model").is_some() {
        json["model"] = client_model;
    }
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(json.to_string()))
        .map_err(|e| OpenAIError::internal(format!("Response build error: {e}")))
}

/// The upstream's SSE events as they come, each with the client's `model`. The bill is
/// settled when the stream ends or the client leaves, not when the upstream fails; a
/// failure is a final `data: {"error": …}` event.
fn stream_back(
    auth: &RequestContext,
    upstream: Upstream,
    answer: Answer,
    client_model: Value,
    client_wants_usage: bool,
    bill: Bill,
) -> Response {
    let (tx, rx) = tokio::sync::mpsc::channel::<Result<Bytes, std::convert::Infallible>>(32);
    super::chat::spawn_until_over(&auth.session, async move {
        let Answer {
            response,
            deadline,
            _slot,
        } = answer;
        let mut upstream_events = response.bytes_stream();
        let mut events = SseEvents::default();
        let mut usage: Option<Value> = None;
        let mut generated = String::new();
        let failure = loop {
            let chunk = match tokio::time::timeout_at(deadline, upstream_events.next()).await {
                Err(_) => {
                    break Some((
                        "upstream_timeout",
                        "The model's endpoint did not finish in time",
                    ))
                }
                Ok(None) => break None,
                Ok(Some(Err(e))) => {
                    log::warn!(
                        "AI endpoint {:?} ({}): the stream broke: {}",
                        upstream.endpoint.name,
                        upstream.remote_model,
                        crate::ai_service::providers::http::scrub(
                            &e.to_string(),
                            upstream.endpoint.api_key.expose()
                        )
                    );
                    break Some((
                        "upstream_error",
                        "The model's endpoint broke off the stream",
                    ));
                }
                Ok(Some(Ok(chunk))) => chunk,
            };
            let mut out = String::new();
            for data in events.push(&chunk) {
                let Ok(mut event) = serde_json::from_str::<Value>(&data) else {
                    // `[DONE]`, or anything that is not JSON: as it came.
                    out.push_str(&format!("data: {data}\n\n"));
                    continue;
                };
                generated.push_str(&generated_text(&event));
                if let Some(reported) = event.get("usage").filter(|u| !u.is_null()) {
                    usage = Some(reported.clone());
                    // The event `include_usage` adds, which this client did not ask for.
                    let only_usage = event
                        .get("choices")
                        .and_then(Value::as_array)
                        .is_some_and(|choices| choices.is_empty());
                    if only_usage && !client_wants_usage {
                        continue;
                    }
                }
                if event.get("model").is_some() {
                    event["model"] = client_model.clone();
                }
                out.push_str(&format!("data: {event}\n\n"));
            }
            if !out.is_empty() && tx.send(Ok(Bytes::from(out))).await.is_err() {
                // The client left: it pays for what reached it.
                break None;
            }
        };
        match failure {
            None => bill.settle(usage.as_ref(), &generated),
            Some((kind, message)) => {
                let error = json!({ "error": { "message": message, "type": kind, "code": kind } });
                let _ = tx.send(Ok(Bytes::from(format!("data: {error}\n\n")))).await;
            }
        }
    });
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "text/event-stream")
        .header(header::CACHE_CONTROL, "no-cache")
        .body(Body::from_stream(
            tokio_stream::wrappers::ReceiverStream::new(rx),
        ))
        .expect("an SSE response builds")
}

/// Splits an SSE byte stream into the `data` of each event, across chunk boundaries.
#[derive(Default)]
struct SseEvents {
    buffer: String,
}

impl SseEvents {
    /// The `data` of every event `chunk` completes. Lines other than `data:` (comments,
    /// `event:`, `id:`) are dropped: OpenAI-shaped streams carry everything in `data`.
    fn push(&mut self, chunk: &[u8]) -> Vec<String> {
        self.buffer
            .push_str(&String::from_utf8_lossy(chunk).replace("\r\n", "\n"));
        let mut complete = Vec::new();
        while let Some(end) = self.buffer.find("\n\n") {
            let event: String = self.buffer.drain(..end + 2).collect();
            let data: Vec<&str> = event
                .lines()
                .filter_map(|line| line.strip_prefix("data:"))
                .map(|data| data.strip_prefix(' ').unwrap_or(data))
                .collect();
            if !data.is_empty() {
                complete.push(data.join("\n"));
            }
        }
        complete
    }
}

/// Forward a `/v1/audio/speech` `body` to `upstream` and pass its audio back.
pub async fn forward_speech(upstream: Upstream, mut body: Value) -> Result<Response, OpenAIError> {
    body["model"] = Value::String(upstream.remote_model.clone());
    let answer = send(&upstream, Route::Speech, &body, false).await?;
    let content_type = answer
        .response
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .unwrap_or("application/octet-stream")
        .to_string();
    let audio = read_body(&upstream, answer).await?;
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, content_type)
        .body(Body::from(audio))
        .map_err(|e| OpenAIError::internal(format!("Response build error: {e}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sse_events_are_split_across_chunks_and_crlf() {
        let mut events = SseEvents::default();
        assert!(events.push(b"data: {\"a\":").is_empty());
        assert_eq!(
            events.push(b"1}\r\n\r\ndata: [DONE]\n\n"),
            vec!["{\"a\":1}", "[DONE]"]
        );
        assert_eq!(
            events.push(b": keep-alive\n\nevent: x\ndata: y\n\n"),
            vec!["y"]
        );
    }

    #[test]
    fn usage_is_the_total_or_the_sum() {
        assert_eq!(usage_tokens(&json!({"total_tokens": 9})), Some(9));
        assert_eq!(
            usage_tokens(&json!({"prompt_tokens": 4, "completion_tokens": 3})),
            Some(7)
        );
        assert_eq!(usage_tokens(&json!({})), None);
    }

    #[test]
    fn generated_text_reads_content_deltas_completions_and_call_arguments() {
        let answer = json!({"choices": [
            {"message": {"content": "hi", "tool_calls": [{"function": {"arguments": "{}"}}]}},
            {"delta": {"content": " there"}},
            {"text": "!"},
        ]});
        assert_eq!(generated_text(&answer), "hi{} there!");
    }

    #[test]
    fn tools_are_active_unless_absent_empty_or_turned_off() {
        let tool = json!([{"type": "function", "function": {"name": "f"}}]);
        assert!(tools_active(&json!({ "tools": tool })));
        assert!(tools_active(
            &json!({ "tools": tool, "tool_choice": "required" })
        ));
        assert!(!tools_active(
            &json!({ "tools": tool, "tool_choice": "none" })
        ));
        assert!(!tools_active(&json!({ "tools": [] })));
        assert!(!tools_active(&json!({})));
    }
}
