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
///
/// The OpenAI chat path is not built here: `chat_gpt_lib_rs` owns its client.
/// It sends the key as a bearer token, which is the header reqwest does strip.
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

/// Whether a credential may be sent to this URL.
///
/// True for https anywhere, and for http on loopback only. A hostname that
/// merely looks local is not enough: `localhost.example.com` resolves
/// wherever its owner points it.
///
/// Checked wherever a key arrives with a URL: discovery, and adding or
/// updating a model. A model saved before the check existed is not re-checked
/// when it loads.
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
