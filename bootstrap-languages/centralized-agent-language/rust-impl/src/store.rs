//! The agent language's identity interface on the shared store (section 5.8 of
//! `docs-src/language-interface-spec.md`).
//!
//! Each call is one store route. The language signs nothing and holds no key: events, offers,
//! claims and signed requests pass through unchanged; a `SignedRequest` travels in the
//! `X-Coasys-Request` header (base64url of its canonical JSON); every body goes out as the
//! canonical JSON of the argument, so a signer hashes `JCS(body)` for `bodyHash`. HTTP
//! statuses map one to one onto `IdentityErrorCode`; a network failure or the host's
//! 10-second timeout is `unavailable`. The store URL is read on every call, and every call goes
//! to that store: a did:scid `src` hint is the executor's to fetch, never the language's.

use std::future::Future;

use ad4m_hash_chain::canonical_json;
use ad4m_identity_core::{Event, FoldError, FormatError};
use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, DeviceRequest, ForbiddenReason,
    IdentityErrorCode, InboxAddress, InboxAuth, InboxMessage, InboxStored, InboxWrite,
    KeyIndexEntry, LogPage, OfferListing, OfferOpened, OfferProgress, PostedRequest, PowParams,
    PreInboxAuth, RateLimitScope, RecordStored, RequestPosted, SealedRecord, SignedHead,
    SignedOffer, SignedRequest, VouchNote, LOG_PAGE_EVENTS, MAX_LOG_PAGE_BYTES,
};
use ad4m_ldk::prelude::LanguageResult;
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::value::RawValue;
use serde_json::Value;

/// The production store, used when the settings name none.
pub const DEFAULT_STORE_URL: &str = "https://store.coasys.org";
pub const REQUEST_HEADER: &str = "X-Coasys-Request";
/// The header of a pre-join inbox read: base64url(JCS(PreInboxAuth)).
pub const PRE_INBOX_HEADER: &str = "X-Coasys-Inbox";
/// The largest reply body the language takes from the store, on every route and status: a log
/// page ([`MAX_LOG_PAGE_BYTES`]) is the largest answer a store gives. The host's `httpFetch`
/// has read the body by then; the cap bounds what the language parses and keeps.
pub const MAX_REPLY_BYTES: usize = MAX_LOG_PAGE_BYTES;

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

/// What the language reads from its settings on every call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoreSettings {
    /// `storeUrl`: the store's base URL (the default store when unset).
    pub url: String,
    /// `storeKeys`: the store keys (Ed25519 multikeys) a signed head must be signed by before
    /// the language reads a log through it (the executor fills it from `identity.storeKeys`).
    /// Empty: no head is trusted, so no `did:scid` record verifies.
    pub keys: Vec<String>,
}

/// The settings from the language settings JSON (`{"storeUrl": …, "storeKeys": […]}`).
pub fn store_settings_from(settings: &str) -> StoreSettings {
    let value = serde_json::from_str::<Value>(settings).unwrap_or(Value::Null);
    let url = value
        .get("storeUrl")
        .and_then(Value::as_str)
        .filter(|url| !url.is_empty())
        .unwrap_or(DEFAULT_STORE_URL)
        .to_string();
    let keys = value
        .get("storeKeys")
        .and_then(Value::as_array)
        .map(|keys| {
            keys.iter()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default();
    StoreSettings { url, keys }
}

/// True when the language may call a store at `url`: `https`, or plain `http` to a loopback
/// address only (a store on the same machine, as in tests). Plain `http` across a network would
/// let anyone on the path change every answer.
pub fn transport_allowed(url: &str) -> bool {
    let Some((scheme, rest)) = url.split_once("://") else {
        return false;
    };
    // A URL parser that follows the WHATWG URL standard (the host's `fetch`) also ends the
    // authority at a backslash for http and https: `http://evil.example\@127.0.0.1` goes to
    // `evil.example`.
    let authority = rest.split(['/', '\\', '?', '#']).next().unwrap_or("");
    let host_port = authority.rsplit_once('@').map_or(authority, |(_, h)| h);
    if host_port.is_empty() {
        return false;
    }
    if scheme.eq_ignore_ascii_case("https") {
        return true;
    }
    if !scheme.eq_ignore_ascii_case("http") {
        return false;
    }
    let host = match host_port.strip_prefix('[') {
        Some(v6) => v6.split(']').next().unwrap_or(""),
        None => host_port.split(':').next().unwrap_or(""),
    };
    host.eq_ignore_ascii_case("localhost")
        || host
            .parse::<std::net::IpAddr>()
            .is_ok_and(|ip| ip.is_loopback())
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
        "store" => ForbiddenReason::Store,
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

/// A header value: base64url(JCS(value)), for a `SignedRequest` (`X-Coasys-Request`) and a
/// `PreInboxAuth` (`X-Coasys-Inbox`).
pub fn encode_header<T: Serialize>(value: &T) -> LanguageResult<String> {
    let value = serde_json::to_value(value).map_err(|_| invalid("request"))?;
    let text = canonical_json(&value).map_err(|_| invalid("request"))?;
    Ok(base64url(text.as_bytes()))
}

/// The store path of an inbox: `/v1/inbox/pre/{pre}` or `/v1/inbox/{scid}/{keyId}`.
pub fn inbox_path(address: &InboxAddress) -> String {
    match address {
        InboxAddress::Pre { pre } => format!("/v1/inbox/pre/{}", segment(pre)),
        InboxAddress::Joined { scid, key_id } => {
            format!("/v1/inbox/{}/{}", segment(scid), segment(key_id))
        }
    }
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

/// The store client over a host's `httpFetch`.
pub struct Store<H: Http> {
    http: H,
    settings: Box<dyn Fn() -> StoreSettings>,
}

/// Options of one request.
#[derive(Default)]
pub struct Send<'a> {
    pub body: Option<Value>,
    pub req: Option<&'a SignedRequest>,
    /// A pre-join inbox read, in `X-Coasys-Inbox`.
    pub pre: Option<&'a PreInboxAuth>,
    pub query: Vec<(&'static str, String)>,
}

impl<'a> Send<'a> {
    /// The headers an inbox read carries for `auth`.
    fn authed(auth: &'a InboxAuth) -> Send<'a> {
        match auth {
            InboxAuth::Request(req) => Send {
                req: Some(req),
                ..Send::default()
            },
            InboxAuth::Pre(pre) => Send {
                pre: Some(pre),
                ..Send::default()
            },
        }
    }
}

impl<H: Http> Store<H> {
    /// A store client that reads its settings (URL and keys) through `settings` on every call.
    pub fn new(http: H, settings: impl Fn() -> StoreSettings + 'static) -> Self {
        Store {
            http,
            settings: Box::new(settings),
        }
    }

    /// The configured store keys a signed head must be signed by.
    pub fn store_keys(&self) -> Vec<String> {
        (self.settings)().keys
    }

    /// Sends one request to the configured store and answers the raw reply. Only a network
    /// failure fails here, a store URL the language does not call ([`transport_allowed`]),
    /// which is `invalid` (`store-url`), and a reply body over [`MAX_REPLY_BYTES`] whatever
    /// its status, which is `invalid` (`too-large`).
    pub async fn send(
        &self,
        method: &str,
        path: &str,
        options: Send<'_>,
    ) -> LanguageResult<HttpReply> {
        let url = (self.settings)().url;
        if !transport_allowed(&url) {
            return Err(invalid("store-url"));
        }
        let base = url.trim_end_matches('/');
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
            headers.insert(REQUEST_HEADER.into(), encode_header(req)?.into());
        }
        if let Some(pre) = options.pre {
            headers.insert(PRE_INBOX_HEADER.into(), encode_header(pre)?.into());
        }
        let headers = Value::Object(headers).to_string();
        let reply = self
            .http
            .fetch(&url, method, &headers, &body)
            .await
            .map_err(|_| unavailable())?;
        if reply.body.len() > MAX_REPLY_BYTES {
            return Err(invalid("too-large"));
        }
        Ok(reply)
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
        options: Send<'_>,
    ) -> LanguageResult<Option<T>> {
        let reply = self.send("GET", path, options).await?;
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

    /// One page of `did`'s log from `from`, from the configured store. A reply larger than
    /// [`MAX_REPLY_BYTES`] or a page of more than [`LOG_PAGE_EVENTS`] events is refused as
    /// `invalid` (`too-large`). An event whose text names a member twice is refused as `invalid`
    /// (`DuplicateMember`): the page leaves here as values, which keep only the last of the two,
    /// so the executor could not see it.
    pub async fn log(&self, did: &str, from: u64) -> LanguageResult<Option<LogPage>> {
        #[derive(serde::Deserialize)]
        struct EventTexts<'a> {
            #[serde(borrow)]
            events: Vec<&'a RawValue>,
        }
        let Some((page, text)) = self.log_with_text(did, from).await? else {
            return Ok(None);
        };
        let texts: EventTexts = serde_json::from_str(&text).map_err(|_| unavailable())?;
        for event in texts.events {
            // Every other format rule the executor applies to the values.
            if let Err(FoldError::Format(FormatError::DuplicateMember(_))) =
                Event::from_json_text(event.get())
            {
                return Err(invalid("DuplicateMember"));
            }
        }
        Ok(Some(page))
    }

    /// [`Store::log`] with the reply body, for a reader that bounds the bytes it reads in total
    /// and parses the events from their own text: a `Value` in `LogPage::events` has kept only
    /// the last of two members of one name, which the event's text still shows.
    pub async fn log_with_text(
        &self,
        did: &str,
        from: u64,
    ) -> LanguageResult<Option<(LogPage, String)>> {
        let path = format!("/v1/logs/{}", segment(scid_of(did)?));
        let query = Send {
            query: vec![("from", from.to_string())],
            ..Send::default()
        };
        let reply = self.send("GET", &path, query).await?;
        if reply.status == 404 {
            return Ok(None);
        }
        if !(200..300).contains(&reply.status) {
            return Err(refusal_of(reply.status, &reply.body).into());
        }
        let page: LogPage = serde_json::from_str(&reply.body).map_err(|_| unavailable())?;
        if page.events.len() > LOG_PAGE_EVENTS as usize {
            return Err(invalid("too-large"));
        }
        Ok(Some((page, reply.body)))
    }

    pub async fn head(&self, did: &str) -> LanguageResult<Option<SignedHead>> {
        self.read(
            &format!("/v1/heads/{}", segment(scid_of(did)?)),
            Send::default(),
        )
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
        let options = Send {
            query: wait(wait_s),
            ..Send::default()
        };
        self.read(&format!("/v1/keys/{}", segment(multikey)), options)
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

    /// `PUT` of an inbox: a pre-join write carries `bind`; both are signed by the current
    /// master key.
    pub async fn inbox_put(
        &self,
        address: &InboxAddress,
        write: &InboxWrite,
        req: &SignedRequest,
    ) -> LanguageResult<InboxStored> {
        let mut options = body_of(write)?;
        options.req = Some(req);
        self.call("PUT", &inbox_path(address), options).await
    }

    /// `GET` of an inbox, oldest first; `wait_s` holds it until a message is there.
    pub async fn inbox_list(
        &self,
        address: &InboxAddress,
        auth: &InboxAuth,
        wait_s: u8,
    ) -> LanguageResult<Vec<InboxMessage>> {
        let mut options = Send::authed(auth);
        options.query = wait(wait_s);
        self.call("GET", &inbox_path(address), options).await
    }

    /// `DELETE` of message `id` of an inbox.
    pub async fn inbox_ack(
        &self,
        address: &InboxAddress,
        id: &str,
        auth: &InboxAuth,
    ) -> LanguageResult<()> {
        let path = format!("{}/{}", inbox_path(address), segment(id));
        let _: Value = self.call("DELETE", &path, Send::authed(auth)).await?;
        Ok(())
    }

    /// `GET /v1/private/{scid}`: `None` when the identity has no record yet.
    pub async fn get_record(
        &self,
        did: &str,
        req: &SignedRequest,
    ) -> LanguageResult<Option<SealedRecord>> {
        let path = format!("/v1/private/{}", segment(scid_of(did)?));
        let options = Send {
            req: Some(req),
            ..Send::default()
        };
        self.read(&path, options).await
    }

    /// `PUT /v1/private/{scid}`.
    pub async fn put_record(
        &self,
        did: &str,
        record: &SealedRecord,
        req: &SignedRequest,
    ) -> LanguageResult<RecordStored> {
        let path = format!("/v1/private/{}", segment(scid_of(did)?));
        let mut options = body_of(record)?;
        options.req = Some(req);
        self.call("PUT", &path, options).await
    }

    /// `POST /v1/requests/{scid}` with `post` as the body.
    pub async fn post_request(
        &self,
        did: &str,
        post: &PostedRequest,
        req: &SignedRequest,
    ) -> LanguageResult<RequestPosted> {
        let path = format!("/v1/requests/{}", segment(scid_of(did)?));
        let mut options = body_of(post)?;
        options.req = Some(req);
        self.call("POST", &path, options).await
    }

    /// `GET /v1/requests/{scid}?since=`.
    pub async fn list_requests(
        &self,
        did: &str,
        since: u64,
        req: &SignedRequest,
    ) -> LanguageResult<Vec<DeviceRequest>> {
        let path = format!("/v1/requests/{}", segment(scid_of(did)?));
        let options = Send {
            req: Some(req),
            query: vec![("since", since.to_string())],
            ..Send::default()
        };
        self.call("GET", &path, options).await
    }

    /// `DELETE /v1/requests/{scid}/{id}`.
    pub async fn delete_request(
        &self,
        did: &str,
        id: &str,
        req: &SignedRequest,
    ) -> LanguageResult<()> {
        let path = format!("/v1/requests/{}/{}", segment(scid_of(did)?), segment(id));
        let options = Send {
            req: Some(req),
            ..Send::default()
        };
        let _: Value = self.call("DELETE", &path, options).await?;
        Ok(())
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
        assert_eq!(
            refusal_of(403, r#"{"error":"store"}"#),
            IdentityErrorCode::Forbidden {
                reason: ForbiddenReason::Store
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
        let url = |settings: &str| store_settings_from(settings).url;
        assert_eq!(url(r#"{"storeUrl":"http://x"}"#), "http://x");
        assert_eq!(url(""), DEFAULT_STORE_URL);
        assert_eq!(url(r#"{"storeUrl":""}"#), DEFAULT_STORE_URL);
        assert_eq!(
            store_settings_from(r#"{"storeUrl":"https://s","storeKeys":["z6MkA",3,"z6MkB"]}"#),
            StoreSettings {
                url: "https://s".into(),
                keys: vec!["z6MkA".into(), "z6MkB".into()]
            }
        );
        assert!(store_settings_from(r#"{"storeKeys":"z6MkA"}"#)
            .keys
            .is_empty());
    }

    #[test]
    fn plain_http_is_taken_only_to_a_loopback_address() {
        for url in [
            "https://store.coasys.org",
            "HTTPS://store.example:8443/base",
            "http://127.0.0.1:8080",
            "http://127.9.9.9",
            "http://localhost:3000/",
            "http://[::1]:8080",
            "http://user@127.0.0.1:1",
        ] {
            assert!(transport_allowed(url), "{url}");
        }
        for url in [
            "http://store.coasys.org",
            "http://10.0.0.1:8080",
            "http://127.0.0.1.attacker.example",
            "http://localhost.attacker.example",
            "http://127.0.0.1@attacker.example",
            "http://attacker.example\\@127.0.0.1",
            "http://attacker.example\\@127.0.0.1/identity",
            "http://[::2]:80",
            "ftp://127.0.0.1",
            "store.coasys.org",
            "https://",
        ] {
            assert!(!transport_allowed(url), "{url}");
        }
    }

    #[test]
    fn a_pre_join_read_header_carries_its_store() {
        let auth = PreInboxAuth {
            address: "zPre".into(),
            sign: "z6MkDev".into(),
            r: "zR".into(),
            store: "did:key:z6MkStore".into(),
            method: "GET".into(),
            path: "/v1/inbox/pre/zPre".into(),
            nonce: "zNonce".into(),
            expires: 60,
            sig: "zSig".into(),
        };
        assert_eq!(
            encode_header(&auth).unwrap(),
            base64url(
                br#"{"address":"zPre","expires":60,"method":"GET","nonce":"zNonce","path":"/v1/inbox/pre/zPre","r":"zR","sig":"zSig","sign":"z6MkDev","store":"did:key:z6MkStore"}"#
            )
        );
    }

    #[test]
    fn segments_keep_the_colons_of_a_did() {
        assert_eq!(segment("did:key:z6Mk"), "did:key:z6Mk");
        assert_eq!(segment("a b/c"), "a%20b%2Fc");
    }
}
