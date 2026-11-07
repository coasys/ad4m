//! Sending a credential to a provider over HTTP: the client that carries it,
//! the transports it may travel over, and the endpoint shapes both providers
//! share.

use std::time::Duration;

/// How long model discovery waits for an answer.
///
/// Discovery is an operator waiting on a form. reqwest sets no timeout by
/// default, so a host that accepts the connection and never answers would
/// leave `ai.discoverModels` pending for as long as the socket stays open. A
/// model listing is one small JSON document, so half a minute is generous.
pub(crate) const DISCOVERY_TIMEOUT: Duration = Duration::from_secs(30);

/// How long a chat client waits for one answer. A completion can run for minutes on a long
/// prompt or a slow local model; without a bound a hung upstream would park a model's
/// worker thread forever, since the thread handles one request at a time. The model
/// worker's own bound on a chat call (`within_endpoint_limits`) defaults to the same.
pub(crate) const CHAT_REQUEST_TIMEOUT: Duration = Duration::from_secs(600);

/// An HTTP client for a request that carries a credential. It follows no
/// redirects, and it gives up after `timeout`.
///
/// reqwest strips `Authorization` when a redirect crosses hosts, but not a
/// custom header such as Anthropic's `x-api-key`, and nothing checks the scheme
/// a redirect points at. Following one hands the key to whichever host the
/// first one names, over whatever transport it names — which walks around the
/// https check discovery makes on the URL it was given. No provider API
/// redirects a well-formed request, so refusing costs nothing, and the failure
/// is a visible status error.
///
/// The timeout is a parameter rather than something a caller may add, because
/// reqwest's default is none at all.
pub(crate) fn credentialed_http(timeout: Duration) -> reqwest::ClientBuilder {
    reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .timeout(timeout)
}

/// Resolve a configured base URL to a versioned endpoint, e.g.
/// `https://api.anthropic.com` plus `messages` gives
/// `https://api.anthropic.com/v1/messages`.
///
/// Accepts the base URL with or without a `/v1` already on it, because both
/// spellings appear in provider documentation and therefore both are what gets
/// pasted into a model form. A path prefix survives, so a gateway that mounts a
/// provider under one keeps working.
pub(crate) fn versioned_endpoint(base_url: url::Url, path: &str) -> String {
    let trimmed = base_url.as_str().trim_end_matches('/').to_string();
    let root = trimmed
        .strip_suffix("/v1")
        .map(|s| s.to_string())
        .unwrap_or(trimmed);
    format!("{root}/v1/{path}")
}

/// Both providers answer a model listing as `{"data": [{"id": …}, …]}`.
/// Entries without an `id` are skipped rather than failing the listing — a
/// partially-understood response is still useful to somebody filling in a form.
pub(crate) fn model_ids_from_data(json: &serde_json::Value) -> Vec<String> {
    json.get("data")
        .and_then(|d| d.as_array())
        .map(|entries| {
            entries
                .iter()
                .filter_map(|entry| entry.get("id")?.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default()
}

/// An upstream answered with an error status. `body` is its reason, with the key scrubbed
/// out: an upstream may echo the credential it refused.
#[derive(Debug)]
pub struct UpstreamStatus {
    pub url: String,
    pub status: reqwest::StatusCode,
    pub body: String,
    /// The upstream's `Retry-After`, passed on to a client it throttled.
    pub retry_after: Option<String>,
}

impl UpstreamStatus {
    /// The error answer `response` of `url`, its body with `api_key` scrubbed out.
    pub(crate) async fn read(response: reqwest::Response, url: &str, api_key: &str) -> Self {
        let status = response.status();
        let retry_after = response
            .headers()
            .get(reqwest::header::RETRY_AFTER)
            .and_then(|value| value.to_str().ok())
            .map(str::to_string);
        let body = read_error_text(response).await;
        UpstreamStatus {
            url: url.to_string(),
            status,
            body: scrub(&body, api_key),
            retry_after,
        }
    }
}

impl std::fmt::Display for UpstreamStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} answered {}: {}", self.url, self.status, self.body)
    }
}

impl std::error::Error for UpstreamStatus {}

/// An upstream that could not be asked: no connection, or no answer in time. `detail` has
/// the key scrubbed out.
#[derive(Debug)]
pub struct UpstreamUnreachable {
    pub url: String,
    pub timed_out: bool,
    pub detail: String,
}

impl UpstreamUnreachable {
    pub(crate) fn new(url: &str, error: &reqwest::Error, api_key: &str) -> Self {
        UpstreamUnreachable {
            url: url.to_string(),
            timed_out: error.is_timeout(),
            detail: scrub(&error.to_string(), api_key),
        }
    }
}

impl std::fmt::Display for UpstreamUnreachable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not reach {}: {}", self.url, self.detail)
    }
}

impl std::error::Error for UpstreamUnreachable {}

/// `text` with every occurrence of `api_key` replaced by `<redacted>`.
pub(crate) fn scrub(text: &str, api_key: &str) -> String {
    if api_key.is_empty() {
        text.to_string()
    } else {
        text.replace(api_key, crate::config_file::REDACTED)
    }
}

/// The most of an answer a provider call reads: far more than a model list or a
/// non-streamed completion, so a server that sends without end cannot fill the memory.
pub(crate) const MAX_BODY_BYTES: usize = 16 * 1024 * 1024;

/// The most of an error answer kept as its reason.
pub(crate) const MAX_ERROR_BODY_BYTES: usize = 64 * 1024;

/// Up to `limit` bytes of `response`'s body, and whether there was more. Stops reading at
/// the limit.
async fn read_capped(
    mut response: reqwest::Response,
    limit: usize,
) -> reqwest::Result<(Vec<u8>, bool)> {
    let mut body = Vec::new();
    while let Some(chunk) = response.chunk().await? {
        let room = limit - body.len();
        if chunk.len() > room {
            body.extend_from_slice(&chunk[..room]);
            return Ok((body, true));
        }
        body.extend_from_slice(&chunk);
    }
    Ok((body, false))
}

/// `response`'s body as JSON. Refused past [`MAX_BODY_BYTES`].
pub(crate) async fn read_json<T: serde::de::DeserializeOwned>(
    response: reqwest::Response,
) -> anyhow::Result<T> {
    let (body, more) = read_capped(response, MAX_BODY_BYTES).await?;
    if more {
        anyhow::bail!("the answer is longer than {MAX_BODY_BYTES} bytes");
    }
    Ok(serde_json::from_slice(&body)?)
}

/// An error answer's body as text: its first [`MAX_ERROR_BODY_BYTES`], empty when it cannot
/// be read.
pub(crate) async fn read_error_text(response: reqwest::Response) -> String {
    read_capped(response, MAX_ERROR_BODY_BYTES)
        .await
        .map(|(body, _)| String::from_utf8_lossy(&body).into_owned())
        .unwrap_or_default()
}

/// Send `request` to `url` and read the JSON answer. A failure names the URL and carries
/// the upstream's reason ([`UpstreamStatus`] for an error status), never `api_key`.
pub(crate) async fn fetch_json(
    request: reqwest::RequestBuilder,
    url: &str,
    api_key: &str,
) -> anyhow::Result<serde_json::Value> {
    let response = request
        .send()
        .await
        .map_err(|e| UpstreamUnreachable::new(url, &e, api_key))?;
    if !response.status().is_success() {
        return Err(UpstreamStatus::read(response, url, api_key).await.into());
    }
    read_json(response)
        .await
        .map_err(|e| match e.downcast_ref::<reqwest::Error>() {
            Some(e) if e.is_timeout() => UpstreamUnreachable::new(url, e, api_key).into(),
            _ => anyhow::anyhow!(
                "Could not read the answer of {url}: {}",
                scrub(&e.to_string(), api_key)
            ),
        })
}

/// What an operator is told when [`is_transport_safe`] refuses their key.
pub(crate) const CLEARTEXT_KEY_REFUSAL: &str =
    "Refusing to send an API key over plain HTTP. Use https, or omit the key.";

/// Whether a credential may be sent to this URL.
///
/// True for https anywhere, and for http on loopback only. A hostname that
/// merely looks local is not enough: `localhost.example.com` resolves
/// wherever its owner points it.
///
/// Checked at discovery, when a model is added or updated, and when a remote
/// client is built. The last one is the check that holds: it covers a model
/// saved before the others existed. The earlier ones turn the same refusal
/// into an error on the request that caused it.
pub(crate) fn is_transport_safe(url: &url::Url) -> bool {
    if url.scheme() == "https" {
        return true;
    }

    match url.host() {
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        Some(url::Host::Domain(host)) => host == "localhost" || host.ends_with(".localhost"),
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(s: &str) -> url::Url {
        url::Url::parse(s).expect("test URL parses")
    }

    #[tokio::test]
    async fn a_host_that_accepts_and_never_answers_times_out() {
        // The failure the timeout exists for: the connection succeeds, so
        // nothing about connecting ever fails, and no response ever arrives.
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
            .await
            .expect("binds");
        let address = listener.local_addr().expect("has an address");
        let _held = tokio::spawn(async move {
            let (_socket, _) = listener.accept().await.expect("accepts");
            std::future::pending::<()>().await;
        });

        let error = credentialed_http(Duration::from_millis(200))
            .build()
            .expect("builds")
            .get(format!("http://{address}/v1/models"))
            .send()
            .await
            .expect_err("gives up rather than waiting");

        assert!(error.is_timeout(), "got: {error}");
    }

    #[test]
    fn an_endpoint_is_built_from_a_bare_host() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
        assert_eq!(
            versioned_endpoint(url("https://api.openai.com"), "models"),
            "https://api.openai.com/v1/models"
        );
    }

    #[test]
    fn a_base_url_already_carrying_v1_does_not_get_a_second_one() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com/v1"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
        assert_eq!(
            versioned_endpoint(url("https://api.groq.com/openai/v1"), "models"),
            "https://api.groq.com/openai/v1/models"
        );
    }

    #[test]
    fn a_trailing_slash_is_tolerated() {
        assert_eq!(
            versioned_endpoint(url("https://api.anthropic.com/v1/"), "messages"),
            "https://api.anthropic.com/v1/messages"
        );
    }

    #[test]
    fn a_proxy_path_prefix_survives() {
        // A gateway may mount a provider under a path. That prefix has to
        // survive, which is why this appends rather than rewriting the path.
        assert_eq!(
            versioned_endpoint(url("https://gateway.internal/anthropic"), "messages"),
            "https://gateway.internal/anthropic/v1/messages"
        );
    }

    #[test]
    fn model_ids_are_read_out_of_the_data_array() {
        let json = serde_json::json!({
            "object": "list",
            "data": [{"id": "gpt-4o"}, {"id": "gpt-4o-mini"}],
        });
        assert_eq!(model_ids_from_data(&json), vec!["gpt-4o", "gpt-4o-mini"]);
    }

    #[test]
    fn an_entry_without_an_id_is_skipped_rather_than_failing_the_listing() {
        // A half-understood response is still useful to somebody filling in a
        // form; refusing the whole list because one row is odd is not.
        let json = serde_json::json!({ "data": [{"id": "a"}, {"object": "model"}, {"id": "b"}] });
        assert_eq!(model_ids_from_data(&json), vec!["a", "b"]);
    }

    #[test]
    fn a_response_with_no_data_array_lists_nothing() {
        assert_eq!(
            model_ids_from_data(&serde_json::json!({ "error": "nope" })),
            Vec::<String>::new()
        );
    }

    fn safe(url: &str) -> bool {
        is_transport_safe(&url::Url::parse(url).expect("parses"))
    }

    #[tokio::test]
    async fn an_upstream_that_echoes_the_key_has_it_scrubbed_from_the_error() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/v1/models")
            .with_status(401)
            .with_body(r#"{"error":"bad key sk-echoed-123"}"#)
            .create_async()
            .await;
        let url = format!("{}/v1/models", server.url());
        let request = credentialed_http(Duration::from_secs(5))
            .build()
            .unwrap()
            .get(&url);

        let error = fetch_json(request, &url, "sk-echoed-123")
            .await
            .unwrap_err();

        let status = error
            .downcast_ref::<UpstreamStatus>()
            .expect("a status error");
        assert_eq!(status.status.as_u16(), 401);
        assert!(!error.to_string().contains("sk-echoed-123"), "{error}");
        assert!(error.to_string().contains("bad key <redacted>"), "{error}");
    }

    // A server that answers without end: the answer is refused at the limit, and an
    // error's reason keeps only its start.
    #[tokio::test]
    async fn an_answer_past_the_limit_is_refused_and_an_error_body_is_cut() {
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/big")
            .with_status(200)
            .with_body(format!("[{}0]", "0,".repeat(MAX_BODY_BYTES / 2)))
            .create_async()
            .await;
        server
            .mock("GET", "/big-error")
            .with_status(500)
            .with_body("e".repeat(MAX_ERROR_BODY_BYTES * 2))
            .create_async()
            .await;
        let http = credentialed_http(Duration::from_secs(30)).build().unwrap();

        let url = format!("{}/big", server.url());
        // `err()`, not `unwrap_err`: a failure must not print 16 MiB.
        let error = fetch_json(http.get(&url), &url, "")
            .await
            .err()
            .expect("refused");
        assert!(error.to_string().contains("longer than"), "{error}");

        let url = format!("{}/big-error", server.url());
        let error = fetch_json(http.get(&url), &url, "")
            .await
            .err()
            .expect("refused");
        let status = error.downcast_ref::<UpstreamStatus>().unwrap();
        assert_eq!(status.body.len(), MAX_ERROR_BODY_BYTES);
    }

    #[test]
    fn https_may_carry_a_credential_anywhere() {
        assert!(safe("https://api.openai.com/v1"));
        assert!(safe("https://gateway.internal/openai/v1"));
    }

    #[test]
    fn plain_http_on_loopback_may_carry_a_credential() {
        // A local Ollama, vLLM or gateway is reached over http by design, and
        // nothing leaves the machine.
        assert!(safe("http://localhost:11434"));
        assert!(safe("http://127.0.0.1:12000/api/v1"));
        assert!(safe("http://[::1]:11434"));
    }

    #[test]
    fn plain_http_to_a_remote_host_may_not() {
        assert!(!safe("http://api.openai.com/v1"));
        assert!(!safe("http://192.168.1.10:11434"));
    }

    #[test]
    fn a_hostname_that_merely_looks_local_is_not_loopback() {
        // `localhost.example.com` resolves wherever its owner points it.
        assert!(!safe("http://localhost.example.com/v1"));
        assert!(!safe("http://notlocalhost/v1"));
    }
}
