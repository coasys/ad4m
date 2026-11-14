//! The link-server directory: the link servers a new space may sync through
//! (`runtime.linkServers`), in the order a person is offered them.
//!
//! Three sources, first to last: the operator's own server (`link_server.url`, checked when
//! written: [`check_operator_url`]), the deployment platform's (`link_server.platform_url`) and
//! the network seed's `linkServers`. The first entry is the default of `neighbourhood.create`.
//! A URL is listed once, under its first source; a URL that is not a link-server base URL
//! (https, or http on a loopback host) is left out.

use std::time::Duration;

use futures::FutureExt;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use crate::config_rpc::{ConfigKeySpec, KeyClass};
use crate::runtime_service::LinkServerSeed;

/// Where a directory entry comes from.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
#[ts(export)]
pub enum LinkServerSource {
    /// `link_server.url` in the config file.
    Operator,
    /// `link_server.platform_url` in the config file.
    Platform,
    /// The network seed's `linkServers`.
    Seed,
}

/// One link server of the directory.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkServerEntry {
    /// The server's base URL, without a trailing slash.
    pub url: String,
    /// The name a person sees: the seed's, else the URL's host.
    pub name: String,
    pub source: LinkServerSource,
}

/// The directory, the default first. `runtime.linkServers` answers [`LinkServerDirectory::all`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LinkServerDirectory {
    servers: Vec<LinkServerEntry>,
}

/// The link-server protocol this executor's server-link language speaks. A server that
/// announces less (protocol 1: no membership, no signed auth) is refused.
pub const MIN_PROTOCOL: u64 = 2;

/// The path a link server announces itself at.
pub const WELL_KNOWN_PATH: &str = "/.well-known/coasys-link-server";

/// How long [`check_operator_url`] waits for the server.
const CHECK_TIMEOUT: Duration = Duration::from_secs(5);

/// The base URL `url` names, without a trailing slash, when it can be a link server's: https,
/// or http on a loopback host (a server on this machine); no query, fragment or credentials.
pub fn base_url(url: &str) -> Result<String, String> {
    let parsed = url::Url::parse(url.trim()).map_err(|e| format!("{url} is not a URL: {e}"))?;
    if parsed.host_str().is_none_or(str::is_empty) {
        return Err(format!("{url} names no host"));
    }
    let loopback = match parsed.host() {
        Some(url::Host::Domain(domain)) => domain == "localhost",
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        None => false,
    };
    match parsed.scheme() {
        "https" => {}
        "http" if loopback => {}
        scheme => {
            let reached = "a link server is reached over https (http only on this machine)";
            return Err(format!("{url}: {reached}, not {scheme}"));
        }
    }
    if !parsed.username().is_empty() || parsed.password().is_some() {
        return Err(format!("{url}: a link server URL carries no credentials"));
    }
    if parsed.query().is_some() || parsed.fragment().is_some() {
        return Err(format!("{url}: a link server URL has no query or fragment"));
    }
    Ok(parsed.as_str().trim_end_matches('/').to_string())
}

fn host_name(url: &str) -> String {
    url::Url::parse(url)
        .ok()
        .and_then(|u| u.host_str().map(str::to_string))
        .unwrap_or_else(|| url.to_string())
}

impl LinkServerDirectory {
    /// The directory of `operator` (`link_server.url`), `platform` (`link_server.platform_url`)
    /// and the seed's `seeds`, in that order.
    pub fn from_sources(
        operator: Option<&str>,
        platform: Option<&str>,
        seeds: &[LinkServerSeed],
    ) -> Self {
        let candidates = operator
            .map(|url| (url, None, LinkServerSource::Operator))
            .into_iter()
            .chain(platform.map(|url| (url, None, LinkServerSource::Platform)))
            .chain(seeds.iter().map(|s| {
                (
                    s.url.as_str(),
                    Some(s.name.as_str()),
                    LinkServerSource::Seed,
                )
            }));
        let mut servers: Vec<LinkServerEntry> = Vec::new();
        for (url, name, source) in candidates {
            let url = match base_url(url) {
                Ok(url) => url,
                Err(e) => {
                    log::warn!("link-server directory: {e}; left out");
                    continue;
                }
            };
            if servers.iter().any(|known| known.url == url) {
                continue;
            }
            let name = name
                .filter(|n| !n.trim().is_empty())
                .map(str::to_string)
                .unwrap_or_else(|| host_name(&url));
            servers.push(LinkServerEntry { url, name, source });
        }
        LinkServerDirectory { servers }
    }

    /// This executor's directory: its config's operator and platform servers, then its seed's.
    pub fn global() -> Self {
        let config = crate::config::get_global_config();
        Self::from_sources(
            config.link_server_url.as_deref(),
            config.link_server_platform_url.as_deref(),
            &crate::runtime_service::RuntimeService::seed_link_servers(),
        )
    }

    /// The server a new space syncs through when the person picks none.
    pub fn default_entry(&self) -> Option<&LinkServerEntry> {
        self.servers.first()
    }

    /// Every entry, the default first.
    pub fn all(&self) -> &[LinkServerEntry] {
        &self.servers
    }
}

/// What a link server answers at [`WELL_KNOWN_PATH`].
#[derive(Clone, Debug, Deserialize)]
struct Announcement {
    protocol: u64,
}

/// Whether `url` is a link server this executor can sync through: it announces itself at
/// [`WELL_KNOWN_PATH`] with a protocol of at least [`MIN_PROTOCOL`]. The validator of
/// `link_server.url`, and the check of an own server a person names in `neighbourhood.create`.
pub async fn check_operator_url(url: &str) -> Result<(), String> {
    let base = base_url(url)?;
    let client = reqwest::Client::builder()
        .timeout(CHECK_TIMEOUT)
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .map_err(|e| e.to_string())?;
    let response = client
        .get(format!("{base}{WELL_KNOWN_PATH}"))
        .send()
        .await
        .map_err(|e| format!("{base} did not answer: {e}"))?;
    if !response.status().is_success() {
        return Err(format!(
            "{base} is not a link server: {WELL_KNOWN_PATH} answered {}",
            response.status()
        ));
    }
    let body = response
        .bytes()
        .await
        .map_err(|e| format!("{base}: cannot read {WELL_KNOWN_PATH}: {e}"))?;
    let announcement: Announcement = serde_json::from_slice(&body)
        .map_err(|e| format!("{base} is not a link server: {WELL_KNOWN_PATH}: {e}"))?;
    if announcement.protocol < MIN_PROTOCOL {
        return Err(format!(
            "{base} speaks link-server protocol {}; this executor needs protocol {MIN_PROTOCOL} \
             or later (rooms with members): upgrade the link server",
            announcement.protocol
        ));
    }
    Ok(())
}

/// The config keys of the directory: `link_server.url` (any operator session, checked with
/// [`check_operator_url`]) and `link_server.platform_url` (this machine only: the deployment
/// writes it).
pub fn config_key_specs() -> Vec<ConfigKeySpec> {
    vec![
        ConfigKeySpec {
            key: "link_server.url",
            class: KeyClass::Restart,
            validator: Some(std::sync::Arc::new(|value: Value| {
                async move {
                    match value {
                        Value::String(url) => check_operator_url(&url).await,
                        _ => Err("link_server.url is a URL string".to_string()),
                    }
                }
                .boxed()
            })),
        },
        ConfigKeySpec {
            key: "link_server.platform_url",
            class: KeyClass::Machine,
            validator: None,
        },
    ]
}

/// A stand-in link server for tests: a real HTTP listener on 127.0.0.1.
#[cfg(test)]
pub(crate) mod test_server {
    use super::WELL_KNOWN_PATH;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    /// Serves `status` and `body` at [`WELL_KNOWN_PATH`] (404 elsewhere) on 127.0.0.1 until
    /// the test's runtime ends; returns its base URL.
    pub(crate) async fn serve(status: &'static str, body: &'static str) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        tokio::spawn(async move {
            while let Ok((mut socket, _)) = listener.accept().await {
                let mut request = vec![0u8; 4096];
                let read = socket.read(&mut request).await.unwrap_or(0);
                let request = String::from_utf8_lossy(&request[..read]).to_string();
                let (status, body) = if request.starts_with(&format!("GET {WELL_KNOWN_PATH} ")) {
                    (status, body)
                } else {
                    ("404 Not Found", "")
                };
                let response = format!(
                    "HTTP/1.1 {status}\r\ncontent-type: application/json\r\ncontent-length: {}\r\nconnection: close\r\n\r\n{body}",
                    body.len()
                );
                socket.write_all(response.as_bytes()).await.ok();
                socket.shutdown().await.ok();
            }
        });
        format!("http://{address}")
    }
}

#[cfg(test)]
mod tests {
    use super::test_server::serve as serve_once;
    use super::*;

    fn seed(url: &str, name: &str) -> LinkServerSeed {
        LinkServerSeed {
            url: url.into(),
            name: name.into(),
        }
    }

    #[test]
    fn the_directory_lists_operator_then_platform_then_seed() {
        let directory = LinkServerDirectory::from_sources(
            Some("https://links.operator.example/"),
            Some("https://link.platform.example"),
            &[
                seed("https://link-staging.coasys.org", "Coasys staging"),
                seed("https://link.other.example", "Other"),
            ],
        );
        assert_eq!(
            directory.servers,
            vec![
                LinkServerEntry {
                    url: "https://links.operator.example".into(),
                    name: "links.operator.example".into(),
                    source: LinkServerSource::Operator,
                },
                LinkServerEntry {
                    url: "https://link.platform.example".into(),
                    name: "link.platform.example".into(),
                    source: LinkServerSource::Platform,
                },
                LinkServerEntry {
                    url: "https://link-staging.coasys.org".into(),
                    name: "Coasys staging".into(),
                    source: LinkServerSource::Seed,
                },
                LinkServerEntry {
                    url: "https://link.other.example".into(),
                    name: "Other".into(),
                    source: LinkServerSource::Seed,
                },
            ]
        );
        assert_eq!(
            directory.default_entry().map(|e| e.source),
            Some(LinkServerSource::Operator)
        );
    }

    #[test]
    fn without_operator_or_platform_the_seed_is_the_default_and_a_url_is_listed_once() {
        let directory = LinkServerDirectory::from_sources(
            None,
            Some("https://link-staging.coasys.org/"),
            &[seed("https://link-staging.coasys.org", "Coasys staging")],
        );
        assert_eq!(directory.servers.len(), 1);
        assert_eq!(directory.servers[0].source, LinkServerSource::Platform);

        let directory = LinkServerDirectory::from_sources(
            None,
            None,
            &[seed("https://link-staging.coasys.org", "Coasys staging")],
        );
        assert_eq!(
            directory.default_entry().map(|e| e.url.as_str()),
            Some("https://link-staging.coasys.org")
        );
        assert_eq!(
            LinkServerDirectory::from_sources(None, None, &[]).default_entry(),
            None
        );
    }

    #[test]
    fn a_url_that_cannot_be_a_link_server_is_left_out() {
        let directory = LinkServerDirectory::from_sources(
            Some("http://links.example"),
            Some("not a url"),
            &[
                seed("ftp://x.example", "x"),
                seed("http://127.0.0.1:3457", "local"),
            ],
        );
        assert_eq!(
            directory.servers,
            vec![LinkServerEntry {
                url: "http://127.0.0.1:3457".into(),
                name: "local".into(),
                source: LinkServerSource::Seed,
            }]
        );
        assert!(base_url("https://user:pw@links.example").is_err());
        assert!(base_url("https://links.example/?a=1").is_err());
        assert_eq!(
            base_url("http://localhost:3457/").as_deref(),
            Ok("http://localhost:3457")
        );
    }

    #[test]
    fn the_entries_serialise_as_the_sdk_reads_them() {
        let directory = LinkServerDirectory::from_sources(Some("https://l.example"), None, &[]);
        assert_eq!(
            serde_json::to_value(directory.all()).unwrap(),
            serde_json::json!([
                { "url": "https://l.example", "name": "l.example", "source": "operator" }
            ])
        );
    }

    #[tokio::test]
    async fn a_protocol_2_link_server_is_accepted() {
        let url = serve_once(
            "200 OK",
            r#"{"protocol":2,"name":"Test links","membership":true}"#,
        )
        .await;
        assert_eq!(check_operator_url(&url).await, Ok(()));
    }

    #[tokio::test]
    async fn a_protocol_1_link_server_is_refused() {
        let url = serve_once("200 OK", r#"{"protocol":1,"name":"Old links"}"#).await;
        let error = check_operator_url(&url).await.unwrap_err();
        assert!(error.contains("protocol 1"), "{error}");
    }

    #[tokio::test]
    async fn a_server_that_is_not_a_link_server_is_refused() {
        let url = serve_once("200 OK", r#"<html>hello</html>"#).await;
        let error = check_operator_url(&url).await.unwrap_err();
        assert!(error.contains("is not a link server"), "{error}");

        let url = serve_once("500 Internal Server Error", r#"{"protocol":2}"#).await;
        let error = check_operator_url(&url).await.unwrap_err();
        assert!(error.contains("500"), "{error}");

        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let closed = format!("http://{}", listener.local_addr().unwrap());
        drop(listener);
        let error = check_operator_url(&closed).await.unwrap_err();
        assert!(error.contains("did not answer"), "{error}");
    }

    #[tokio::test]
    async fn the_config_key_runs_the_check() {
        let specs = config_key_specs();
        let url_key = specs.iter().find(|s| s.key == "link_server.url").unwrap();
        assert_eq!(url_key.class, KeyClass::Restart);
        let validator = url_key
            .validator
            .clone()
            .expect("link_server.url is checked");

        let old = serve_once("200 OK", r#"{"protocol":1}"#).await;
        assert!(validator(Value::String(old)).await.is_err());
        let new = serve_once("200 OK", r#"{"protocol":2}"#).await;
        assert_eq!(validator(Value::String(new)).await, Ok(()));
        assert!(validator(serde_json::json!(5)).await.is_err());

        let platform = specs
            .iter()
            .find(|s| s.key == "link_server.platform_url")
            .unwrap();
        assert_eq!(platform.class, KeyClass::Machine);
    }
}
