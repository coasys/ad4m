//! [`SrcFetch`]: the executor's reader of a foreign `src` (a did:scid host someone else named in
//! a DID URL), over the did:scid host profile: `GET {base}/v1/logs/{scid}?from={n}` answering a
//! log page (section 13 of `docs-src/identity/did-scid-coasys-1.md`).
//!
//! A `src` is an address chosen by whoever wrote the DID URL, and executors run on servers, so
//! the address policy is enforced where the connection is made, not before it:
//!
//! - the client resolves names only through [`PublicOnly`], which answers the public addresses
//!   of a name ([`is_public`]) and fails when none remain, so no connection to a private address
//!   is made, whatever the name answers at the moment of the fetch (no DNS rebinding window);
//! - an IP-literal host never reaches a resolver, so [`refuse_private_host`] checks it before
//!   the request;
//! - a redirect is not followed (a 3xx is a failed fetch), no proxy is used, HTTPS only;
//! - connect and total timeouts, and a body capped at [`MAX_LOG_PAGE_BYTES`].
//!
//! `identity.allowPrivateSrc` (tests, local development) lets private addresses through; the
//! other rules stay. The configured store is never read here: it goes through the agent
//! language (`KelCache::refresh`).

use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;
use std::time::Duration;

use ad4m_ldk::identity::MAX_LOG_PAGE_BYTES;
use futures::future::BoxFuture;
use hyper_014::client::connect::dns::Name;
use reqwest::dns::{Addrs, Resolve, Resolving};
use serde::Deserialize;
use serde_json::Value;

use super::resolver::{is_public, HttpsBase};

/// How long the lookup of a `src` host may take.
const DNS_TIMEOUT: Duration = Duration::from_secs(5);
/// How long the connection to a `src` host may take.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
/// How long one page request may take, body included.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(15);

/// A name lookup: the addresses of a host name.
pub type Lookup =
    Arc<dyn Fn(String) -> BoxFuture<'static, std::io::Result<Vec<IpAddr>>> + Send + Sync>;

/// The system resolver (`getaddrinfo` through tokio).
fn system_lookup() -> Lookup {
    Arc::new(|name: String| {
        Box::pin(async move {
            Ok(tokio::net::lookup_host((name.as_str(), 0))
                .await?
                .map(|a| a.ip())
                .collect())
        })
    })
}

/// The part of a log page the resolver reads from a foreign host: the events, and the head's
/// seq as a hint of how far to page. A host may add members (section 13); its head need not be
/// signed, and is never trusted.
#[derive(Debug, Clone, Deserialize)]
pub struct HostPage {
    pub events: Vec<Value>,
    pub head: HostHead,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HostHead {
    pub seq: u64,
}

/// Refuses a `src` host that names a private address without a lookup: an IP literal that is
/// not [`is_public`] (a literal never reaches the client's resolver), or a `localhost` name.
pub fn refuse_private_host(url: &url::Url) -> Result<(), String> {
    let refuse = |ip: IpAddr| Err(format!("{ip} is not a public address"));
    match url.host() {
        Some(url::Host::Ipv4(ip)) if !is_public(ip.into()) => refuse(ip.into()),
        Some(url::Host::Ipv6(ip)) if !is_public(ip.into()) => refuse(ip.into()),
        Some(url::Host::Ipv4(_)) | Some(url::Host::Ipv6(_)) => Ok(()),
        Some(url::Host::Domain(name)) => {
            let name = name.trim_end_matches('.').to_ascii_lowercase();
            if name == "localhost" || name.ends_with(".localhost") {
                return Err(format!("{name} is a loopback name"));
            }
            Ok(())
        }
        None => Err("no host".into()),
    }
}

/// The client's DNS resolver: a name's public addresses only (all of them under
/// `allow_private`). No public address: the lookup fails and no connection is made.
#[derive(Clone)]
pub struct PublicOnly {
    allow_private: bool,
    lookup: Lookup,
}

impl PublicOnly {
    /// The addresses the client may connect to for `name` (port 0: the client sets the URL's).
    pub async fn addresses(&self, name: &str) -> Result<Vec<SocketAddr>, String> {
        let found = match tokio::time::timeout(DNS_TIMEOUT, (self.lookup)(name.to_string())).await {
            Ok(Ok(found)) => found,
            Ok(Err(e)) => return Err(format!("{name} does not resolve: {e}")),
            Err(_) => return Err(format!("the lookup of {name} timed out")),
        };
        let allowed: Vec<SocketAddr> = found
            .iter()
            .copied()
            .filter(|ip| self.allow_private || is_public(*ip))
            .map(|ip| SocketAddr::new(ip, 0))
            .collect();
        if allowed.is_empty() {
            return Err(match found.first() {
                Some(ip) => format!("refused: {name} resolves to {ip}, not a public address"),
                None => format!("{name} has no address"),
            });
        }
        Ok(allowed)
    }
}

impl Resolve for PublicOnly {
    fn resolve(&self, name: Name) -> Resolving {
        let this = self.clone();
        Box::pin(async move {
            let addrs = this.addresses(name.as_str()).await?;
            Ok(Box::new(addrs.into_iter()) as Addrs)
        })
    }
}

/// Reads log pages from foreign did:scid hosts under the address policy of this module.
pub struct SrcFetch {
    /// `None` when the client did not build: every fetch then fails, never through a client
    /// without the address policy.
    client: Option<reqwest::Client>,
    allow_private: bool,
    /// `https`; `http` only in tests, against local hosts.
    scheme: &'static str,
}

impl SrcFetch {
    /// HTTPS only; `allow_private`: `identity.allowPrivateSrc`.
    pub fn new(allow_private: bool) -> Self {
        Self::build(allow_private, "https", system_lookup())
    }

    /// Plain HTTP with the given name lookup, for tests against local hosts.
    #[cfg(test)]
    pub(crate) fn plain_http(allow_private: bool, lookup: Lookup) -> Self {
        Self::build(allow_private, "http", lookup)
    }

    fn build(allow_private: bool, scheme: &'static str, lookup: Lookup) -> Self {
        let resolver = PublicOnly {
            allow_private,
            lookup,
        };
        let client = reqwest::Client::builder()
            .dns_resolver(Arc::new(resolver))
            .redirect(reqwest::redirect::Policy::none())
            // A proxy would resolve and connect for us, out of the resolver's reach.
            .no_proxy()
            .https_only(scheme == "https")
            .connect_timeout(CONNECT_TIMEOUT)
            .timeout(REQUEST_TIMEOUT)
            .build()
            .map_err(|e| log::error!("identity: the src client does not build ({e})"))
            .ok();
        SrcFetch {
            client,
            allow_private,
            scheme,
        }
    }

    /// One page of `scid`'s log from seq `from` at `base`. `Ok(None)`: the host holds no log
    /// (`404`). An error is why the fetch failed (`refused: …` for the address policy).
    pub async fn page(
        &self,
        base: &HttpsBase,
        scid: &str,
        from: u64,
    ) -> Result<Option<HostPage>, String> {
        if !self.allow_private {
            refuse_private_host(&base.url).map_err(|why| format!("refused: {why}"))?;
        }
        let mut url = url::Url::parse(&format!("{}://{}", self.scheme, base.label))
            .map_err(|e| format!("{}: {e}", base.label))?;
        url.path_segments_mut()
            .map_err(|_| format!("{} has no path", base.label))?
            .pop_if_empty()
            .extend(["v1", "logs", scid]);
        url.query_pairs_mut().append_pair("from", &from.to_string());

        let client = self
            .client
            .as_ref()
            .ok_or_else(|| "the src client did not build".to_string())?;
        let mut response = client
            .get(url)
            .header(reqwest::header::ACCEPT, "application/json")
            .send()
            .await
            .map_err(|e| describe(&e))?;
        let status = response.status();
        if status == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if status.is_redirection() {
            return Err(format!(
                "the host answered {status}: a redirect is not followed"
            ));
        }
        if !status.is_success() {
            return Err(format!("the host answered {status}"));
        }
        let too_large = || format!("the page is larger than {MAX_LOG_PAGE_BYTES} bytes");
        if response
            .content_length()
            .is_some_and(|n| n > MAX_LOG_PAGE_BYTES as u64)
        {
            return Err(too_large());
        }
        let mut body = Vec::new();
        while let Some(chunk) = response.chunk().await.map_err(|e| describe(&e))? {
            if body.len() + chunk.len() > MAX_LOG_PAGE_BYTES {
                return Err(too_large());
            }
            body.extend_from_slice(&chunk);
        }
        serde_json::from_slice(&body)
            .map(Some)
            .map_err(|e| format!("the host answered no log page: {e}"))
    }
}

/// A reqwest error with its causes (reqwest's own text leaves them out, and the resolver's
/// refusal is one of them).
fn describe(e: &reqwest::Error) -> String {
    let mut text = e.to_string();
    let mut source = std::error::Error::source(e);
    while let Some(cause) = source {
        let cause_text = cause.to_string();
        if !text.contains(&cause_text) {
            text.push_str(": ");
            text.push_str(&cause_text);
        }
        source = cause.source();
    }
    text
}

/// Local HTTP hosts for the transport tests: each counts the connections it accepts.
#[cfg(test)]
pub(crate) mod test_host {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    pub(crate) struct TestHost {
        pub port: u16,
        connections: Arc<AtomicUsize>,
    }

    impl TestHost {
        /// A host on 127.0.0.1 that answers every request with `answer(request target)`, a
        /// whole HTTP response.
        pub async fn start(answer: impl Fn(&str) -> Vec<u8> + Send + Sync + 'static) -> TestHost {
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
            let port = listener.local_addr().unwrap().port();
            let connections = Arc::new(AtomicUsize::new(0));
            let counted = connections.clone();
            let answer = Arc::new(answer);
            tokio::spawn(async move {
                while let Ok((mut socket, _)) = listener.accept().await {
                    counted.fetch_add(1, Ordering::SeqCst);
                    let answer = answer.clone();
                    tokio::spawn(async move {
                        let mut request = Vec::new();
                        let mut buf = [0u8; 4096];
                        while !request.windows(4).any(|w| w == b"\r\n\r\n") {
                            match socket.read(&mut buf).await {
                                Ok(0) | Err(_) => return,
                                Ok(n) => request.extend_from_slice(&buf[..n]),
                            }
                        }
                        let text = String::from_utf8_lossy(&request);
                        let target = text.split(' ').nth(1).unwrap_or("/").to_string();
                        // The client may close early (a body over the cap): ignore the error.
                        let _ = socket.write_all(&answer(&target)).await;
                        let _ = socket.shutdown().await;
                    });
                }
            });
            TestHost { port, connections }
        }

        pub fn connections(&self) -> usize {
            self.connections.load(Ordering::SeqCst)
        }
    }

    /// A `status` response with `headers` and `body`, closing the connection.
    pub fn response(status: &str, headers: &[(&str, &str)], body: &[u8]) -> Vec<u8> {
        let mut out = format!(
            "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n",
            body.len()
        );
        for (name, value) in headers {
            out.push_str(&format!("{name}: {value}\r\n"));
        }
        out.push_str("\r\n");
        let mut out = out.into_bytes();
        out.extend_from_slice(body);
        out
    }

    /// The `from` query parameter of a request target.
    pub fn from_of(target: &str) -> u64 {
        target
            .split_once("from=")
            .and_then(|(_, n)| n.split('&').next())
            .and_then(|n| n.parse().ok())
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::test_host::{response, TestHost};
    use super::*;
    use crate::identity::resolver::{parse_src, SrcKind};
    use std::str::FromStr;

    fn base(src: &str) -> HttpsBase {
        match parse_src(src).unwrap() {
            SrcKind::Https(base) => base,
            other => panic!("{other:?}"),
        }
    }

    /// A lookup that answers `ips` for every name.
    fn answering(ips: &[&str]) -> Lookup {
        let ips: Vec<IpAddr> = ips.iter().map(|ip| ip.parse().unwrap()).collect();
        Arc::new(move |_| {
            let ips = ips.clone();
            Box::pin(async move { Ok(ips) })
        })
    }

    fn page_json() -> Vec<u8> {
        serde_json::to_vec(&serde_json::json!({
            "events": [],
            "head": {"scid": "Qm", "seq": 0, "hash": "h"},
            "appendedAt": [],
        }))
        .unwrap()
    }

    /// Lets a connection the client might still make land before counting.
    async fn settle() {
        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    #[tokio::test]
    async fn a_name_that_resolves_only_to_loopback_is_never_connected() {
        let host = TestHost::start(|_| response("200 OK", &[], &page_json())).await;
        let fetch = SrcFetch::plain_http(false, answering(&["127.0.0.1"]));
        let result = fetch
            .page(&base(&format!("log.example:{}", host.port)), "Qm", 0)
            .await;
        let why = result.expect_err("no public address");
        assert!(why.contains("not a public address"), "{why}");
        settle().await;
        assert_eq!(host.connections(), 0, "the listener got no connection");

        // The same host, allowed: the transport itself works.
        let fetch = SrcFetch::plain_http(true, answering(&["127.0.0.1"]));
        let page = fetch
            .page(&base(&format!("log.example:{}", host.port)), "Qm", 0)
            .await
            .unwrap()
            .expect("a page");
        assert_eq!(page.head.seq, 0);
        assert_eq!(host.connections(), 1);
    }

    #[tokio::test]
    async fn only_the_public_answers_of_a_name_are_tried() {
        let lookup = answering(&[
            "10.0.0.1",
            "203.0.113.7",
            "::1",
            "169.254.169.254",
            "2606:4700::1111",
            "::ffff:127.0.0.1",
        ]);
        let public = PublicOnly {
            allow_private: false,
            lookup: lookup.clone(),
        };
        let tried: Vec<IpAddr> = public
            .resolve(Name::from_str("log.example").unwrap())
            .await
            .unwrap()
            .map(|a| a.ip())
            .collect();
        assert_eq!(
            tried,
            vec![
                "203.0.113.7".parse::<IpAddr>().unwrap(),
                "2606:4700::1111".parse().unwrap()
            ]
        );
        let all = PublicOnly {
            allow_private: true,
            lookup,
        };
        assert_eq!(all.addresses("log.example").await.unwrap().len(), 6);
        let none = PublicOnly {
            allow_private: false,
            lookup: answering(&[]),
        };
        assert!(none.addresses("log.example").await.is_err());
    }

    #[tokio::test]
    async fn a_redirect_is_not_followed_even_with_private_srcs_allowed() {
        let target = TestHost::start(|_| response("200 OK", &[], &page_json())).await;
        let location = format!("http://127.0.0.1:{}/v1/logs/Qm?from=0", target.port);
        let redirecting = TestHost::start(move |_| {
            response("302 Found", &[("Location", location.as_str())], b"")
        })
        .await;
        let fetch = SrcFetch::plain_http(true, answering(&["127.0.0.1"]));
        let why = fetch
            .page(&base(&format!("127.0.0.1:{}", redirecting.port)), "Qm", 0)
            .await
            .expect_err("a 3xx is a failed fetch");
        assert!(why.contains("302"), "{why}");
        settle().await;
        assert_eq!(redirecting.connections(), 1);
        assert_eq!(target.connections(), 0, "the redirect was not followed");
    }

    #[tokio::test]
    async fn a_private_ip_literal_is_refused_without_a_connection() {
        let host = TestHost::start(|_| response("200 OK", &[], &page_json())).await;
        let fetch = SrcFetch::plain_http(false, answering(&["203.0.113.7"]));
        for src in [
            format!("127.0.0.1:{}", host.port),
            format!("[::ffff:127.0.0.1]:{}", host.port),
        ] {
            let why = fetch.page(&base(&src), "Qm", 0).await.unwrap_err();
            assert!(why.starts_with("refused"), "{src}: {why}");
        }
        settle().await;
        assert_eq!(host.connections(), 0);
    }

    #[tokio::test]
    async fn a_body_over_the_cap_is_refused() {
        // Declared: refused on the header. Undeclared (read to the close): refused at the cap.
        let declared =
            TestHost::start(|_| response("200 OK", &[], &vec![b' '; MAX_LOG_PAGE_BYTES + 1])).await;
        let streamed = TestHost::start(|_| {
            let mut out = b"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n".to_vec();
            out.extend(std::iter::repeat_n(b' ', MAX_LOG_PAGE_BYTES + 1));
            out
        })
        .await;
        let fetch = SrcFetch::plain_http(true, answering(&["127.0.0.1"]));
        for host in [&declared, &streamed] {
            let why = fetch
                .page(&base(&format!("127.0.0.1:{}", host.port)), "Qm", 0)
                .await
                .unwrap_err();
            assert!(why.contains("larger than"), "{why}");
        }
    }

    #[tokio::test]
    async fn the_request_follows_the_host_profile() {
        let seen = Arc::new(std::sync::Mutex::new(Vec::new()));
        let log = seen.clone();
        let host = TestHost::start(move |target| {
            log.lock().unwrap().push(target.to_string());
            response("404 Not Found", &[], b"")
        })
        .await;
        let fetch = SrcFetch::plain_http(true, answering(&["127.0.0.1"]));
        let answer = fetch
            .page(&base(&format!("127.0.0.1:{}/did/", host.port)), "QmAbc", 7)
            .await
            .unwrap();
        assert!(answer.is_none(), "404: the host holds no log");
        assert_eq!(
            seen.lock().unwrap().as_slice(),
            ["/did/v1/logs/QmAbc?from=7"]
        );
    }
}
