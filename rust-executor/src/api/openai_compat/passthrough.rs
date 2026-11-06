//! `/v1` as an aggregator: a request for a model on a registered endpoint is forwarded to
//! that endpoint as the client wrote it (plan §5).
//!
//! Only `model` is rewritten, to the id the endpoint knows (`ModelApi::model`), and back to
//! the client's name in the answer, and, for a caller who pays, what bounds the cost
//! ([`cap_cost`]). Every other field (`tools`, `tool_choice`,
//! `response_format`, fields this executor has never heard of) reaches the upstream, and an
//! SSE stream comes back event by event. A stream asks for
//! `stream_options.include_usage`, so the bill is the upstream's count; the usage-only
//! event it adds is not passed on to a client that did not ask for it. Where the upstream
//! reports no usage, the request and the generated text are estimated. A stream is billed
//! for what it generated however it ends ([`StreamBill`]).
//!
//! When `/v1` forwards and when the executor answers itself: [`Upstream::for_chat`] and
//! [`Upstream::of`]. Failures map per §5.4 ([`OpenAIError::from_endpoint`]); one that says
//! the endpoint is down or refuses the key re-checks its health, and a 404 marks the model
//! `MISSING`. An endpoint with `max_concurrent` lets that many requests through at once;
//! the wait counts against the request's timeout. A stream may stay silent for that
//! timeout between two chunks, not for longer.

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
    credentialed_http, is_transport_safe, read_json, versioned_endpoint, UpstreamStatus,
    UpstreamUnreachable, CLEARTEXT_KEY_REFUSAL,
};
use crate::billing::{affordable_ai_units, check_compute_credits};
use crate::types::{Endpoint, EndpointKind, Model, ModelState, ModelStatus, RequestContext};

/// How long a stream may take to start answering (plan §5.4).
const FIRST_BYTE_TIMEOUT: Duration = Duration::from_secs(30);

/// The bound for a request whose timeout the route sets (every request but a stream sets
/// one), and the longest a stream may run.
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
    /// kind but Anthropic and Ollama. `None`: the executor answers, through the model's own
    /// client (Anthropic is translated there; Ollama goes to its native `/api/chat`, as its
    /// OpenAI-compatible `/v1` ignores `num_ctx` and cuts long prompts without an error) or
    /// a local model.
    pub fn of(resolved: &Resolved) -> Option<Upstream> {
        let endpoint = resolved.endpoint.clone()?;
        if matches!(
            endpoint.kind,
            EndpointKind::Anthropic | EndpointKind::Ollama
        ) {
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
    let mapped = OpenAIError::from_endpoint(&error);
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
    /// The request's timeout: a stream may stay silent this long between two chunks.
    timeout: Duration,
    _slot: Option<OwnedSemaphorePermit>,
}

/// Send `body` to `route` of `upstream`. Waits for a slot when the endpoint caps its
/// concurrency, then for the answer: within the request timeout, or for a stream, within
/// [`FIRST_BYTE_TIMEOUT`] too. The timeout bounds the whole of an answer that is not a
/// stream; a stream's events are timed one by one ([`stream_back`]).
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

    let mut request = HTTP.post(&url).json(body);
    if !stream {
        request = request.timeout(deadline.saturating_duration_since(Instant::now()));
    }
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
        timeout,
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

/// What a stream has generated so far, billed once when it is dropped: at the stream's end,
/// when the client leaves, when the upstream breaks off or stays silent past the timeout,
/// and when the session ends and drops the task. The upstream has done that work in each.
struct StreamBill {
    bill: Bill,
    usage: Option<Value>,
    generated: String,
}

impl Drop for StreamBill {
    fn drop(&mut self) {
        self.bill.settle(self.usage.as_ref(), &self.generated);
    }
}

/// The tokens `usage` reports: its total, else prompt plus completion. `None` when it
/// reports neither, or only one part: the estimate counts the whole request then.
fn usage_tokens(usage: &Value) -> Option<usize> {
    let count = |key: &str| usage.get(key).and_then(Value::as_u64);
    let total = count("total_tokens")
        .or_else(|| Some(count("prompt_tokens")? + count("completion_tokens")?))?;
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

/// The text an answer or a stream event generated: reasoning, content, completion text and
/// the arguments of tool calls.
fn generated_text(answer: &Value) -> String {
    let mut text = String::new();
    let choices = answer.get("choices").and_then(Value::as_array);
    for choice in choices.into_iter().flatten() {
        for message in [choice.get("message"), choice.get("delta")]
            .into_iter()
            .flatten()
        {
            // `reasoning_content` (llama-server, vLLM, DeepSeek) and `reasoning`
            // (OpenRouter, newer vLLM) are generated tokens too.
            for key in ["reasoning_content", "reasoning", "content"] {
                if let Some(part) = message.get(key).and_then(Value::as_str) {
                    text.push_str(part);
                }
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

/// Bound what `body` can cost a caller who can pay for `affordable` tokens of the model.
/// The bill is tokens times the model's rate, and a request's tokens are at most its prompt
/// plus each choice's completion limit. So every completion limit the client set
/// (`max_tokens`, `max_completion_tokens`) is lowered to
/// `(affordable - prompt_estimate) / choices`, where `choices` is the largest of `n`,
/// `best_of` and 1, and `limit_key` is set to it when the client set neither.
/// `service_tier` is dropped: the operator prices the endpoint's default tier. A request
/// that cannot afford one token per choice is refused.
fn cap_cost(
    body: &mut Value,
    limit_key: &str,
    prompt_estimate: usize,
    affordable: usize,
) -> Result<(), OpenAIError> {
    let count = |key: &str| body.get(key).and_then(Value::as_u64).unwrap_or(1);
    let choices = count("n").max(count("best_of")).max(1);
    let per_choice = (affordable.saturating_sub(prompt_estimate) as u64) / choices;
    if per_choice == 0 {
        return Err(OpenAIError::insufficient_quota(
            "Insufficient compute credits for this request",
        ));
    }
    let Some(fields) = body.as_object_mut() else {
        return Ok(());
    };
    fields.remove("service_tier");
    let mut limited = false;
    for key in ["max_tokens", "max_completion_tokens"] {
        if let Some(limit) = fields.get_mut(key).filter(|limit| !limit.is_null()) {
            let asked = limit.as_u64().unwrap_or(u64::MAX);
            *limit = Value::from(asked.min(per_choice));
            limited = true;
        }
    }
    if !limited {
        fields.insert(limit_key.to_string(), Value::from(per_choice));
    }
    Ok(())
}

/// Forward a chat or text completion `body` for `auth` to `upstream`, the answer back with
/// the client's `model`, and bill the caller's user from its usage.
pub async fn forward_completion(
    auth: &RequestContext,
    upstream: Upstream,
    route: Route,
    mut body: Value,
) -> Result<Response, OpenAIError> {
    let prompt_estimate = estimate_token_count(&prompt_text(&body));
    if let Some(agent_did) = auth.user_did() {
        check_compute_credits(agent_did)
            .map_err(|_| OpenAIError::insufficient_quota("Insufficient compute credits"))?;
        let affordable = affordable_ai_units(agent_did, &upstream.model.id)
            .map_err(|e| OpenAIError::internal(format!("Could not read the credits: {e}")))?;
        if let Some(affordable) = affordable {
            let limit_key = match (route, upstream.endpoint.kind) {
                (Route::ChatCompletions, EndpointKind::OpenAi) => "max_completion_tokens",
                _ => "max_tokens",
            };
            cap_cost(&mut body, limit_key, prompt_estimate, affordable)?;
        }
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
        prompt_estimate,
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
    // The slot is held until the answer is read.
    let Answer {
        response, _slot, ..
    } = answer;
    let mut json: Value = read_json(response).await.map_err(|e| {
        let endpoint = &upstream.endpoint;
        let failed = match e.downcast_ref::<reqwest::Error>() {
            Some(e) => {
                UpstreamUnreachable::new(endpoint.base_url.as_str(), e, endpoint.api_key.expose())
                    .into()
            }
            None if e.is::<serde_json::Error>() => {
                anyhow::anyhow!("the endpoint answered a body that is not JSON")
            }
            None => e,
        };
        upstream_failure(endpoint, &upstream.remote_model, failed)
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

/// The upstream's SSE events as they come, each with the client's `model`. The upstream may
/// stay silent for the request's timeout between two chunks. A failure is a final
/// `data: {"error": …}` event; the bill is settled however the stream ends ([`StreamBill`]).
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
            timeout,
            _slot: slot,
        } = answer;
        let mut upstream_events = response.bytes_stream();
        let mut events = SseEvents::default();
        let mut meter = StreamBill {
            bill,
            usage: None,
            generated: String::new(),
        };
        let failure = loop {
            let next = tokio::select! {
                // The client left while the upstream was silent: it pays for what reached it.
                _ = tx.closed() => break None,
                next = tokio::time::timeout(timeout, upstream_events.next()) => next,
            };
            let chunk = match next {
                Err(_) => {
                    break Some(("upstream_timeout", "The model's endpoint stopped answering"))
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
                meter.generated.push_str(&generated_text(&event));
                if let Some(reported) = event.get("usage").filter(|u| !u.is_null()) {
                    meter.usage = Some(reported.clone());
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
            if !out.is_empty() {
                let sent = tokio::time::timeout(timeout, tx.send(Ok(Bytes::from(out)))).await;
                if !matches!(sent, Ok(Ok(()))) {
                    // The client left, or read nothing for the timeout while the slot and
                    // the upstream waited: it pays for what reached it.
                    break None;
                }
            }
            if events.overflowing() {
                log::warn!(
                    "AI endpoint {:?} ({}): an SSE event outgrew {MAX_SSE_EVENT} bytes",
                    upstream.endpoint.name,
                    upstream.remote_model,
                );
                break Some((
                    "upstream_error",
                    "The model's endpoint sent an event too large to pass on",
                ));
            }
        };
        // The endpoint is done with: its slot goes to the next request, and the final event
        // waits for no client.
        drop(upstream_events);
        drop(slot);
        if let Some((kind, message)) = failure {
            let error = json!({ "error": { "message": message, "type": kind, "code": kind } });
            let _ = tx.try_send(Ok(Bytes::from(format!("data: {error}\n\n"))));
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

/// The most one SSE event may hold, read but not complete: an upstream that sends more
/// without ending the event is cut off.
const MAX_SSE_EVENT: usize = 8 << 20;

/// Splits an SSE byte stream into the `data` of each event, across chunk boundaries. Lines
/// end at LF, CR or CRLF; a line is decoded once it is complete, so a character or a CRLF
/// split across two chunks reads as one.
#[derive(Default)]
struct SseEvents {
    /// The bytes of a line not yet complete.
    line: Vec<u8>,
    /// The `data` lines of the event not yet complete, and their length.
    data: Vec<String>,
    held: usize,
}

impl SseEvents {
    /// The `data` of every event `chunk` completes. Lines other than `data:` (comments,
    /// `event:`, `id:`) are dropped: OpenAI-shaped streams carry everything in `data`.
    fn push(&mut self, chunk: &[u8]) -> Vec<String> {
        self.line.extend_from_slice(chunk);
        let mut complete = Vec::new();
        let mut start = 0;
        while let Some(at) = self.line[start..]
            .iter()
            .position(|&b| b == b'\n' || b == b'\r')
        {
            let end = start + at;
            let next = match self.line[end] {
                // A CR the next chunk may follow with its LF.
                b'\r' if end + 1 == self.line.len() => break,
                b'\r' if self.line[end + 1] == b'\n' => end + 2,
                _ => end + 1,
            };
            let line = String::from_utf8_lossy(&self.line[start..end]);
            if line.is_empty() {
                if !self.data.is_empty() {
                    complete.push(self.data.join("\n"));
                    self.data.clear();
                    self.held = 0;
                }
            } else if let Some(data) = line.strip_prefix("data:") {
                let data = data.strip_prefix(' ').unwrap_or(data);
                self.held += data.len();
                self.data.push(data.to_string());
            }
            start = next;
        }
        self.line.drain(..start);
        complete
    }

    /// Whether the event being read holds more than [`MAX_SSE_EVENT`].
    fn overflowing(&self) -> bool {
        self.held + self.line.len() > MAX_SSE_EVENT
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

    // Each network chunk was decoded on its own: a character split in two became two U+FFFD,
    // and a CRLF split as CR | LF merged two events into one.
    #[test]
    fn a_character_or_a_crlf_split_across_chunks_reads_as_one() {
        let mut events = SseEvents::default();
        assert!(events.push(b"data: caf\xC3").is_empty());
        assert_eq!(events.push(b"\xA9\n\n"), vec!["caf\u{e9}"]);

        assert!(events.push(b"data: a\r\n\r").is_empty());
        assert_eq!(events.push(b"\ndata: b\r\n\r\n"), vec!["a", "b"]);

        assert_eq!(events.push(b"data: c\r\rdata: d\n"), vec!["c"]);
        assert_eq!(events.push(b"data: e\n\n"), vec!["d\ne"]);
    }

    #[test]
    fn an_event_that_outgrows_the_limit_is_flagged() {
        let mut events = SseEvents::default();
        let line = vec![b'x'; MAX_SSE_EVENT / 2];
        assert!(events.push(b"data: ").is_empty());
        events.push(&line);
        assert!(!events.overflowing());
        events.push(b"\ndata: ");
        events.push(&line);
        events.push(b"xx");
        assert!(events.overflowing(), "held across lines of one event");
        assert_eq!(events.push(b"\n\n").len(), 1);
        assert!(!events.overflowing(), "a complete event is let go");
    }

    #[test]
    fn a_paying_request_is_capped_at_what_the_credits_buy() {
        let mut body =
            json!({ "n": 2, "best_of": 3, "max_tokens": 1000, "service_tier": "priority" });
        cap_cost(&mut body, "max_tokens", 10, 100).unwrap();
        assert_eq!(body, json!({ "n": 2, "best_of": 3, "max_tokens": 30 }));

        let mut body = json!({ "max_completion_tokens": null });
        cap_cost(&mut body, "max_completion_tokens", 10, 100).unwrap();
        assert_eq!(body, json!({ "max_completion_tokens": 90 }));

        let mut body = json!({ "max_tokens": 5, "max_completion_tokens": "many" });
        cap_cost(&mut body, "max_tokens", 10, 100).unwrap();
        assert_eq!(
            body,
            json!({ "max_tokens": 5, "max_completion_tokens": 90 })
        );

        let error = cap_cost(&mut json!({ "n": 4 }), "max_tokens", 97, 100).unwrap_err();
        assert_eq!(error.error.code.as_deref(), Some("insufficient_quota"));
    }

    #[test]
    fn usage_is_the_total_or_the_sum() {
        assert_eq!(usage_tokens(&json!({"total_tokens": 9})), Some(9));
        assert_eq!(
            usage_tokens(&json!({"prompt_tokens": 4, "completion_tokens": 3})),
            Some(7)
        );
        assert_eq!(usage_tokens(&json!({})), None);
        assert_eq!(
            usage_tokens(&json!({"completion_tokens": 3})),
            None,
            "a part alone is not the request's count"
        );
    }

    #[test]
    fn generated_text_reads_reasoning_content_deltas_completions_and_call_arguments() {
        let answer = json!({"choices": [
            {"message": {"content": "hi", "tool_calls": [{"function": {"arguments": "{}"}}]}},
            {"delta": {"reasoning_content": "hmm"}},
            {"delta": {"reasoning": "so"}},
            {"delta": {"content": " there"}},
            {"text": "!"},
        ]});
        assert_eq!(generated_text(&answer), "hi{}hmmso there!");
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
