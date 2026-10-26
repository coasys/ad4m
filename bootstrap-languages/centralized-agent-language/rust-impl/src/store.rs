//! The agent language's identity interface on the shared store (section 5.8 of
//! `docs-src/language-interface-spec.md`).
//!
//! Each call is one store route. The language signs nothing and holds no key: events, offers,
//! claims and signed requests pass through unchanged; a `SignedRequest` travels in the
//! `X-Coasys-Request` header (base64url of its canonical JSON); every body goes out as the
//! canonical JSON of the argument, so a signer hashes `JCS(body)` for `bodyHash`. HTTP
//! statuses map one to one onto `IdentityErrorCode`; a network failure or the host's
//! 10-second timeout is `unavailable`. The store URL is read on every call.
//!
//! `log` alone may read from another host: a did:scid `src` hint that is an HTTPS base
//! ([`src_base`]). Every other call goes to the configured store.

use std::future::Future;

use ad4m_hash_chain::canonical_json;
use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, ForbiddenReason, IdentityErrorCode,
    KeyIndexEntry, LogPage, OfferListing, OfferOpened, OfferProgress, PowParams, RateLimitScope,
    SignedHead, SignedOffer, SignedRequest, VouchNote, LOG_PAGE_EVENTS, MAX_LOG_PAGE_BYTES,
};
use ad4m_ldk::prelude::LanguageResult;
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::Value;

/// The production store, used when the settings name none.
pub const DEFAULT_STORE_URL: &str = "https://store.coasys.org";
pub const REQUEST_HEADER: &str = "X-Coasys-Request";

/// What the host's `httpFetch` answers.
#[derive(Debug, Clone)]
pub struct HttpReply {
    pub status: u16,
    pub body: String,
}

/// The host's `httpFetch`: `Err` only on a network failure or the 10-second timeout.
pub trait Http {
    fn fetch(
        &self,
        url: &str,
        method: &str,
        headers_json: &str,
        body: &str,
    ) -> impl Future<Output = Result<HttpReply, String>>;
}

/// The store URL from the language settings JSON (`{"storeUrl": …}`), or the default.
pub fn store_url_from(settings: &str) -> String {
    serde_json::from_str::<Value>(settings)
        .ok()
        .and_then(|v| {
            v.get("storeUrl")
                .and_then(Value::as_str)
                .map(str::to_string)
        })
        .filter(|url| !url.is_empty())
        .unwrap_or_else(|| DEFAULT_STORE_URL.to_string())
}

fn forbidden_reason(error: &str) -> Option<ForbiddenReason> {
    Some(match error {
        "vouch" => ForbiddenReason::Vouch,
        "pow" => ForbiddenReason::Pow,
        "claim" => ForbiddenReason::Claim,
        "signer" => ForbiddenReason::Signer,
        "secret" => ForbiddenReason::Secret,
        "bound" => ForbiddenReason::Bound,
        "cosig" => ForbiddenReason::Cosig,
        "writer" => ForbiddenReason::Writer,
        _ => return None,
    })
}

/// The refusal an HTTP answer carries (statuses map one to one onto
/// `IdentityErrorCode`).
pub fn refusal_of(status: u16, text: &str) -> IdentityErrorCode {
    let body: Value = serde_json::from_str(text).unwrap_or(Value::Null);
    let error = body
        .get("error")
        .and_then(Value::as_str)
        .map(str::to_string);
    let invalid = |fallback: &str| IdentityErrorCode::Invalid {
        reason: error.clone().unwrap_or_else(|| fallback.to_string()),
    };
    match status {
        400 => invalid("malformed"),
        403 => match error.as_deref().and_then(forbidden_reason) {
            Some(reason) => IdentityErrorCode::Forbidden { reason },
            None => invalid("forbidden"),
        },
        404 => IdentityErrorCode::NotFound,
        409 => {
            if let Some(head) = body
                .get("head")
                .and_then(|h| serde_json::from_value::<SignedHead>(h.clone()).ok())
            {
                IdentityErrorCode::Conflict { head }
            } else if error.as_deref() == Some("key-taken") {
                IdentityErrorCode::KeyTaken {
                    key: body
                        .get("key")
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_string(),
                }
            } else {
                invalid("conflict")
            }
        }
        410 => IdentityErrorCode::Gone {
            state: body
                .get("state")
                .and_then(Value::as_str)
                .unwrap_or("gone")
                .to_string(),
        },
        413 => IdentityErrorCode::Invalid {
            reason: "too-large".into(),
        },
        429 => IdentityErrorCode::RateLimited {
            scope: match body.get("scope").and_then(Value::as_str) {
                Some("global") => RateLimitScope::Global,
                Some("identity") => RateLimitScope::Identity,
                _ => RateLimitScope::Address,
            },
            retry_after_sec: body.get("retryAfter").and_then(Value::as_u64).unwrap_or(60),
        },
        s if s >= 500 => IdentityErrorCode::Unavailable,
        s => invalid(&format!("status-{s}")),
    }
}

const B64URL: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

/// base64url without padding.
pub fn base64url(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let n = (u32::from(chunk[0]) << 16)
            | (u32::from(*chunk.get(1).unwrap_or(&0)) << 8)
            | u32::from(*chunk.get(2).unwrap_or(&0));
        for i in 0..=chunk.len() {
            out.push(B64URL[((n >> (18 - 6 * i)) & 63) as usize] as char);
        }
    }
    out
}

/// The header value of a signed request: base64url(JCS(SignedRequest)).
pub fn encode_request(req: &SignedRequest) -> LanguageResult<String> {
    let value = serde_json::to_value(req).map_err(|_| invalid("request"))?;
    let text = canonical_json(&value).map_err(|_| invalid("request"))?;
    Ok(base64url(text.as_bytes()))
}

/// A path segment: unreserved characters and `:` stay, so the path a signer signs (DIDs,
/// SCIDs, multikeys, offer ids) is the path the store receives.
pub fn segment(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for b in text.bytes() {
        if b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.' | b'~' | b':') {
            out.push(b as char);
        } else {
            out.push_str(&format!("%{b:02X}"));
        }
    }
    out
}

fn invalid(reason: &str) -> ad4m_ldk::prelude::LanguageError {
    IdentityErrorCode::Invalid {
        reason: reason.to_string(),
    }
    .into()
}

fn unavailable() -> ad4m_ldk::prelude::LanguageError {
    IdentityErrorCode::Unavailable.into()
}

/// The SCID of a `did:scid:coasys:1`; anything else is refused as `invalid`.
pub fn scid_of(did: &str) -> LanguageResult<&str> {
    ad4m_identity_core::scid_of(did).ok_or_else(|| invalid("did"))
}

/// The HTTPS base a did:scid `src` hint names: `store.example`, `https://store.example`,
/// `store.example:8443/prefix`. A scheme-less `src` is HTTPS. The answer is
/// `https://<host>[:port][/path]`, the host in lower case, without a trailing `/`.
///
/// Refused as `invalid` (`src`): another scheme (`http`, an AD4M language address, a DID
/// method), userinfo, a query or a fragment, a host that is not a DNS name or an IP literal,
/// a port outside 1-65535, and a path with anything but unreserved characters,
/// percent-escapes and `/`, or with a `.` or `..` segment. Only the form is checked: whether
/// the host resolves to a public address is for the executor, which resolves DNS.
pub fn src_base(src: &str) -> LanguageResult<String> {
    let bad = || invalid("src");
    let rest = match src.find("://") {
        Some(at) if src[..at].eq_ignore_ascii_case("https") => &src[at + 3..],
        Some(_) => return Err(bad()),
        None => src,
    };
    if rest.contains(['?', '#', '@', '\\']) {
        return Err(bad());
    }
    let (authority, path) = rest.split_at(rest.find('/').unwrap_or(rest.len()));
    let (host, port) = if let Some(inner) = authority.strip_prefix('[') {
        let (ip, after) = inner.split_once(']').ok_or_else(bad)?;
        ip.parse::<std::net::Ipv6Addr>().map_err(|_| bad())?;
        let port = match after {
            "" => None,
            p => Some(p.strip_prefix(':').ok_or_else(bad)?),
        };
        (format!("[{}]", ip.to_ascii_lowercase()), port)
    } else {
        let (host, port) = match authority.split_once(':') {
            Some((h, p)) => (h, Some(p)),
            None => (authority, None),
        };
        if !is_dns_name(host) {
            return Err(bad());
        }
        (host.to_ascii_lowercase(), port)
    };
    let port = match port {
        None => String::new(),
        Some(p) if !p.is_empty() && p.bytes().all(|b| b.is_ascii_digit()) => {
            match p.parse::<u16>() {
                Ok(n) if n > 0 => format!(":{n}"),
                _ => return Err(bad()),
            }
        }
        Some(_) => return Err(bad()),
    };
    let path = path.trim_end_matches('/');
    if !path.is_empty() && !is_path_prefix(path) {
        return Err(bad());
    }
    Ok(format!("https://{host}{port}{path}"))
}

/// A DNS name (an IPv4 literal is one too): labels of 1-63 letters, digits and `-`, not at
/// either end of a label, at most 253 characters, no trailing dot.
fn is_dns_name(host: &str) -> bool {
    !host.is_empty()
        && host.len() <= 253
        && host.split('.').all(|label| {
            !label.is_empty()
                && label.len() <= 63
                && !label.starts_with('-')
                && !label.ends_with('-')
                && label
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'-')
        })
}

/// A path prefix: `/`-led segments of unreserved characters and percent-escapes, none of
/// them empty, `.` or `..`.
fn is_path_prefix(path: &str) -> bool {
    let Some(rest) = path.strip_prefix('/') else {
        return false;
    };
    rest.split('/').all(|seg| {
        let bytes = seg.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'%' if bytes.get(i + 1).is_some_and(u8::is_ascii_hexdigit)
                    && bytes.get(i + 2).is_some_and(u8::is_ascii_hexdigit) =>
                {
                    i += 3
                }
                b if b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.' | b'~') => i += 1,
                _ => return false,
            }
        }
        !seg.is_empty() && seg != "." && seg != ".."
    })
}

/// The store client over a host's `httpFetch`.
pub struct Store<H: Http> {
    http: H,
    url: Box<dyn Fn() -> String>,
}

/// Options of one request.
#[derive(Default)]
pub struct Send<'a> {
    pub body: Option<Value>,
    pub req: Option<&'a SignedRequest>,
    pub query: Vec<(&'static str, String)>,
}

impl<H: Http> Store<H> {
    pub fn new(http: H, url: impl Fn() -> String + 'static) -> Self {
        Store {
            http,
            url: Box::new(url),
        }
    }

    /// Sends one request to the configured store and answers the raw reply. Only a network
    /// failure fails here.
    pub async fn send(
        &self,
        method: &str,
        path: &str,
        options: Send<'_>,
    ) -> LanguageResult<HttpReply> {
        self.send_to(&(self.url)(), method, path, options).await
    }

    /// Sends one request to the host at `base` and answers the raw reply.
    async fn send_to(
        &self,
        base: &str,
        method: &str,
        path: &str,
        options: Send<'_>,
    ) -> LanguageResult<HttpReply> {
        let base = base.trim_end_matches('/');
        let query: Vec<String> = options
            .query
            .iter()
            .map(|(k, v)| format!("{k}={}", segment(v)))
            .collect();
        let url = if query.is_empty() {
            format!("{base}{path}")
        } else {
            format!("{base}{path}?{}", query.join("&"))
        };
        let mut headers = serde_json::Map::new();
        let body = match &options.body {
            Some(value) => {
                headers.insert("Content-Type".into(), "application/json".into());
                canonical_json(value).map_err(|_| invalid("malformed"))?
            }
            None => String::new(),
        };
        if let Some(req) = options.req {
            headers.insert(REQUEST_HEADER.into(), encode_request(req)?.into());
        }
        let headers = Value::Object(headers).to_string();
        self.http
            .fetch(&url, method, &headers, &body)
            .await
            .map_err(|_| unavailable())
    }

    /// Sends and answers the parsed body of a 2xx reply; any other status is its refusal.
    pub async fn call<T: DeserializeOwned>(
        &self,
        method: &str,
        path: &str,
        options: Send<'_>,
    ) -> LanguageResult<T> {
        let reply = self.send(method, path, options).await?;
        if !(200..300).contains(&reply.status) {
            return Err(refusal_of(reply.status, &reply.body).into());
        }
        let text = if reply.body.is_empty() {
            "null"
        } else {
            &reply.body
        };
        serde_json::from_str(text).map_err(|_| unavailable())
    }

    /// A read: `404` answers `None`.
    pub async fn read<T: DeserializeOwned>(
        &self,
        path: &str,
        query: Vec<(&'static str, String)>,
    ) -> LanguageResult<Option<T>> {
        let reply = self
            .send(
                "GET",
                path,
                Send {
                    query,
                    ..Send::default()
                },
            )
            .await?;
        if reply.status == 404 {
            return Ok(None);
        }
        if !(200..300).contains(&reply.status) {
            return Err(refusal_of(reply.status, &reply.body).into());
        }
        serde_json::from_str(&reply.body)
            .map(Some)
            .map_err(|_| unavailable())
    }

    /// One page of `did`'s log from `from`: from the host `src` names when it is given (see
    /// [`src_base`]), else from the configured store. A page from another host only reads; its
    /// head is passed on as data. A reply larger than [`MAX_LOG_PAGE_BYTES`] or a page of more
    /// than [`LOG_PAGE_EVENTS`] events is refused as `invalid` (`too-large`).
    pub async fn log(
        &self,
        did: &str,
        from: u64,
        src: Option<&str>,
    ) -> LanguageResult<Option<LogPage>> {
        let path = format!("/v1/logs/{}", segment(scid_of(did)?));
        let base = match src {
            Some(src) => src_base(src)?,
            None => (self.url)(),
        };
        let query = Send {
            query: vec![("from", from.to_string())],
            ..Send::default()
        };
        let reply = self.send_to(&base, "GET", &path, query).await?;
        if reply.status == 404 {
            return Ok(None);
        }
        if !(200..300).contains(&reply.status) {
            return Err(refusal_of(reply.status, &reply.body).into());
        }
        if reply.body.len() > MAX_LOG_PAGE_BYTES {
            return Err(invalid("too-large"));
        }
        let page: LogPage = serde_json::from_str(&reply.body).map_err(|_| unavailable())?;
        if page.events.len() > LOG_PAGE_EVENTS as usize {
            return Err(invalid("too-large"));
        }
        Ok(Some(page))
    }

    pub async fn head(&self, did: &str) -> LanguageResult<Option<SignedHead>> {
        self.read(&format!("/v1/heads/{}", segment(scid_of(did)?)), Vec::new())
            .await
    }

    pub async fn append(&self, did: &str, req: &AppendRequest) -> LanguageResult<SignedHead> {
        #[derive(serde::Deserialize)]
        struct Appended {
            head: SignedHead,
        }
        let path = format!("/v1/logs/{}/events", segment(scid_of(did)?));
        let answer: Appended = self.call("POST", &path, body_of(req)?).await?;
        Ok(answer.head)
    }

    pub async fn key_owner(
        &self,
        multikey: &str,
        wait_s: u8,
    ) -> LanguageResult<Option<KeyIndexEntry>> {
        self.read(&format!("/v1/keys/{}", segment(multikey)), wait(wait_s))
            .await
    }

    pub async fn pow_params(&self) -> LanguageResult<PowParams> {
        self.call("GET", "/v1/pow", Send::default()).await
    }

    pub async fn attach_note(
        &self,
        did: &str,
        token: &str,
        req: &SignedRequest,
    ) -> LanguageResult<VouchNote> {
        let path = format!("/v1/notes/{}", segment(scid_of(did)?));
        let options = Send {
            body: Some(serde_json::json!({ "token": token })),
            req: Some(req),
            ..Send::default()
        };
        self.call("POST", &path, options).await
    }

    pub async fn put_agent(
        &self,
        record: &AgentRecord,
        req: Option<&SignedRequest>,
    ) -> LanguageResult<()> {
        let path = format!("/v1/agents/{}", segment(&record.did));
        let mut options = body_of(record)?;
        options.req = req;
        let _: Value = self.call("PUT", &path, options).await?;
        Ok(())
    }

    pub async fn open_offer(&self, offer: &SignedOffer) -> LanguageResult<OfferOpened> {
        let path = format!("/v1/offers/{}", segment(&offer.offer.scid));
        self.call("POST", &path, body_of(offer)?).await
    }

    pub async fn list_offers(
        &self,
        did: &str,
        req: &SignedRequest,
        wait_s: u8,
    ) -> LanguageResult<Vec<OfferListing>> {
        let path = format!("/v1/offers/{}", segment(scid_of(did)?));
        let options = Send {
            req: Some(req),
            query: wait(wait_s),
            ..Send::default()
        };
        self.call("GET", &path, options).await
    }

    pub async fn cancel_offer(
        &self,
        did: &str,
        offer_id: &str,
        req: &SignedRequest,
    ) -> LanguageResult<()> {
        let path = format!(
            "/v1/offers/{}/{}",
            segment(scid_of(did)?),
            segment(offer_id)
        );
        let options = Send {
            req: Some(req),
            ..Send::default()
        };
        let _: Value = self.call("DELETE", &path, options).await?;
        Ok(())
    }

    pub async fn claim_offer(
        &self,
        did: &str,
        offer_id: &str,
        claim: &Claim,
    ) -> LanguageResult<ClaimReceipt> {
        let path = format!(
            "/v1/offers/{}/{}/claim",
            segment(scid_of(did)?),
            segment(offer_id)
        );
        self.call("POST", &path, body_of(claim)?).await
    }

    pub async fn offer_state(
        &self,
        did: &str,
        offer_id: &str,
        key: &str,
        wait_s: u8,
    ) -> LanguageResult<OfferProgress> {
        let path = format!(
            "/v1/offers/{}/{}/state",
            segment(scid_of(did)?),
            segment(offer_id)
        );
        let mut query = vec![("key", key.to_string())];
        query.extend(wait(wait_s));
        let options = Send {
            query,
            ..Send::default()
        };
        self.call("GET", &path, options).await
    }
}

fn body_of<T: Serialize>(value: &T) -> LanguageResult<Send<'static>> {
    Ok(Send {
        body: Some(serde_json::to_value(value).map_err(|_| invalid("malformed"))?),
        ..Send::default()
    })
}

fn wait(wait_s: u8) -> Vec<(&'static str, String)> {
    if wait_s == 0 {
        Vec::new()
    } else {
        vec![("wait", wait_s.to_string())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base64url_follows_rfc4648_without_padding() {
        assert_eq!(base64url(b""), "");
        assert_eq!(base64url(b"f"), "Zg");
        assert_eq!(base64url(b"fo"), "Zm8");
        assert_eq!(base64url(b"foo"), "Zm9v");
        assert_eq!(base64url(b"foob"), "Zm9vYg");
        assert_eq!(base64url(&[0xfb, 0xff]), "-_8");
    }

    #[test]
    fn statuses_map_one_to_one_onto_refusals() {
        assert_eq!(
            refusal_of(400, r#"{"error":"seq"}"#),
            IdentityErrorCode::Invalid {
                reason: "seq".into()
            }
        );
        assert_eq!(
            refusal_of(403, r#"{"error":"cosig"}"#),
            IdentityErrorCode::Forbidden {
                reason: ForbiddenReason::Cosig
            }
        );
        assert_eq!(refusal_of(404, ""), IdentityErrorCode::NotFound);
        assert_eq!(
            refusal_of(409, r#"{"error":"key-taken","key":"z6Mk"}"#),
            IdentityErrorCode::KeyTaken { key: "z6Mk".into() }
        );
        assert_eq!(
            refusal_of(410, r#"{"error":"gone","state":"spent"}"#),
            IdentityErrorCode::Gone {
                state: "spent".into()
            }
        );
        assert_eq!(
            refusal_of(413, ""),
            IdentityErrorCode::Invalid {
                reason: "too-large".into()
            }
        );
        assert_eq!(
            refusal_of(429, r#"{"scope":"identity","retryAfter":7}"#),
            IdentityErrorCode::RateLimited {
                scope: RateLimitScope::Identity,
                retry_after_sec: 7
            }
        );
        assert_eq!(refusal_of(503, "{}"), IdentityErrorCode::Unavailable);
    }

    #[test]
    fn the_store_url_comes_from_the_settings() {
        assert_eq!(store_url_from(r#"{"storeUrl":"http://x"}"#), "http://x");
        assert_eq!(store_url_from(""), DEFAULT_STORE_URL);
        assert_eq!(store_url_from(r#"{"storeUrl":""}"#), DEFAULT_STORE_URL);
    }

    #[test]
    fn a_src_is_normalised_to_an_https_base() {
        for (src, base) in [
            ("store.example", "https://store.example"),
            ("Store.Example/", "https://store.example"),
            ("https://store.example", "https://store.example"),
            ("HTTPS://store.example/", "https://store.example"),
            ("store.example:8443", "https://store.example:8443"),
            (
                "https://store.example/kel/v%2D1/",
                "https://store.example/kel/v%2D1",
            ),
            ("203.0.113.7", "https://203.0.113.7"),
            ("[2001:DB8::1]:443", "https://[2001:db8::1]:443"),
            ("localhost", "https://localhost"),
        ] {
            assert_eq!(src_base(src).unwrap(), base, "{src}");
        }
        for src in [
            "",
            "http://store.example",
            "wss://store.example",
            "https://u@store.example",
            "store.example?a",
            "store.example#a",
            "store.example:",
            "store.example:99999",
            "store..example",
            "store.example/a//b",
            "store.example/./b",
            "store.example/%zz",
            "[::1]x",
            &"a".repeat(64),
            "did:web:store.example",
        ] {
            assert_eq!(
                IdentityErrorCode::from_language_error(&src_base(src).unwrap_err()),
                Some(IdentityErrorCode::Invalid {
                    reason: "src".into()
                }),
                "{src}"
            );
        }
    }

    #[test]
    fn segments_keep_the_colons_of_a_did() {
        assert_eq!(segment("did:key:z6Mk"), "did:key:z6Mk");
        assert_eq!(segment("a b/c"), "a%20b%2Fc");
    }
}
