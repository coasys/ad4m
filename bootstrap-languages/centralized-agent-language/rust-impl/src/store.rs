//! The agent language's identity interface on the shared store (AR-01 4.9, AR-02 4.16), the
//! Rust port of `../store.ts`.
//!
//! Each call is one store route. The language signs nothing and holds no key: events, offers,
//! claims and signed requests pass through unchanged; a `SignedRequest` travels in the
//! `X-Coasys-Request` header (base64url of its canonical JSON); every body goes out as the
//! canonical JSON of the argument, so a signer hashes `JCS(body)` for `bodyHash`. HTTP
//! statuses map one to one onto `IdentityErrorCode`; a network failure or the host's
//! 10-second timeout is `unavailable`. The store URL is read on every call.

use std::future::Future;

use ad4m_hash_chain::canonical_json;
use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, ForbiddenReason, IdentityErrorCode,
    KeyIndexEntry, LogPage, OfferListing, OfferOpened, OfferProgress, PowParams, RateLimitScope,
    SignedHead, SignedOffer, SignedRequest, VouchNote,
};
use ad4m_ldk::prelude::LanguageResult;
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::Value;

/// The production store (AR-01 4.5.3), used when the settings name none.
pub const DEFAULT_STORE_URL: &str = "https://store.coasys.org";
pub const REQUEST_HEADER: &str = "X-Coasys-Request";
pub const SCID_PREFIX: &str = "did:scid:coasys:1:";

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

/// The refusal an HTTP answer carries (AR-01 4.9: statuses map one to one).
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

/// The SCID of a `did:scid`; anything else is refused as `invalid`.
pub fn scid_of(did: &str) -> LanguageResult<&str> {
    did.strip_prefix(SCID_PREFIX)
        .filter(|s| !s.is_empty())
        .ok_or_else(|| invalid("did"))
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

    /// Sends one request and answers the raw reply. Only a network failure fails here.
    pub async fn send(
        &self,
        method: &str,
        path: &str,
        options: Send<'_>,
    ) -> LanguageResult<HttpReply> {
        let base = (self.url)();
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

    pub async fn log(&self, did: &str, from: u64) -> LanguageResult<Option<LogPage>> {
        let path = format!("/v1/logs/{}", segment(scid_of(did)?));
        self.read(&path, vec![("from", from.to_string())]).await
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
    fn segments_keep_the_colons_of_a_did() {
        assert_eq!(segment("did:key:z6Mk"), "did:key:z6Mk");
        assert_eq!(segment("a b/c"), "a%20b%2Fc");
    }
}
