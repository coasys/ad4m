//! `ViaStore`: an [`ExecutorRpc`] over the identity store's routes, for a device that has no
//! executor of its own — the phone. `ViaExecutor<ViaStore>` runs the flows unchanged: what an
//! executor does with the wallet key it keeps for a caller (sign the `X-Coasys-Request` of a
//! signed route, open inbox messages, sign a claim), `ViaStore` does with the ports the
//! caller supplies, so no seed leaves the phone's own key store.
//!
//! The ports:
//!
//! - [`HttpClient`]: one HTTPS request in, one reply out. No transport code lives here; the
//!   surface owns TLS.
//! - `device` (`ad4m_identity_core::build::Signer`): the Ed25519 device key. It signs the
//!   `X-Coasys-Request` header of a signed route, the `X-Coasys-Inbox` header of a pre-join
//!   read, and a claim's co-signature. The seed stays behind the trait.
//! - [`EncKey`]: the X25519 key a device string carries as `enc`. It opens inbox messages
//!   (`identity.inbox`, `identity.takeMaster`, a pre-join ticket's claim) and unwraps the
//!   record key (`identity.unwrapRecordKey`). [`SoftEncKey`] is the software version. An
//!   `EncOpenError::Unavailable` (a locked keystore) keeps the message and answers 503; a
//!   `Sealed` drops it.
//! - `clock` and `random`: a signed request's `expires` and `nonce`, and a device string's
//!   `r`.
//! - [`StoreEndpoint`]: the store's base URL (an offer or device string names it) and the
//!   store keys (Ed25519 multikeys) a signed head must be signed by — the executor's
//!   `identity.storeKeys`. With no key, the calls that sign or commit refuse
//!   `no-store-keys`, as `identity.publish` does on an executor.
//!
//! `identity` is the SCID this device's key speaks for. A caller reloads it from its own
//! storage; when it is `None`, the store's key index (`GET /v1/keys/{key}`) discovers it.
//!
//! Every call is one store route, as the executor's backend maps them (`rust-executor`'s
//! identity module, and the agent language's `store.rs`, which sends the same canonical
//! JSON and headers):
//!
//! | `identity.*` | Route |
//! |---|---|
//! | `status` | `GET /v1/keys/{deviceKey}`, then `GET /v1/logs/{scid}` for `keyId`, `revoked`, `head` |
//! | `log` | `GET /v1/logs/{scid}?from=` paged to the head |
//! | `keyOwner` | `GET /v1/keys/{key}` (`404` → `null`) |
//! | `publish` | `POST /v1/logs/{scid}/events` (`409` → head when the log holds the events) |
//! | `openOffer` | `POST /v1/offers/{scid}` |
//! | `offers` | `GET /v1/offers/{scid}?wait=` with the caller's `X-Coasys-Request` |
//! | `claimOffer` | `POST /v1/offers/{scid}/{offerId}/claim`, the claim co-signed by `device` |
//! | `joinStatus` | `GET /v1/offers/{scid}/{offerId}/state?key=` |
//! | `devices` | folded `GET /v1/logs/{scid}`, the record's names stay in the app |
//! | `prepareDevice` | local device string; `identity.inbox` reads `/v1/inbox/pre/{a}` |
//! | `inbox` | `GET /v1/inbox/pre/{a}` (`X-Coasys-Inbox`), `GET /v1/inbox/{scid}/{keyId}` (signed) |
//! | `inboxAck` | `DELETE /v1/inbox/{scid}/{keyId}/{id}` signed by `device` |
//! | `takeMaster` | the joined read above; each message opened by `enc`, checked against the log |
//! | `inboxPut` | `PUT /v1/inbox/pre/{a}` / `PUT /v1/inbox/{scid}/{keyId}` with the caller's request |
//! | `record` | `GET /v1/private/{scid}`: `device`-signed, or the caller's master-signed request |
//! | `recordPut` | `PUT /v1/private/{scid}` the same way (`404`/`200` → `null`/record) |
//! | `unwrapRecordKey` | local: `enc` unwraps this device's entry |
//! | `requests` | `GET /v1/requests/{scid}?since=` signed by `device` |
//! | `postRequest` | `POST /v1/requests/{scid}` signed by `device` |
//! | `deleteRequest` | `DELETE` to the caller request's signed `path` |
//! | `cosignIncept`, `resetLog` | local / a no-op (the phone keeps no cache to drop) |
//! | `custodyStatus` | the keys this device holds (`{sign, enc}`) |
//! | `resolve`, `rekey`, others | `501 not-served` |
//!
//! The trait's `?Send` matches the rest of the crate: a synchronous surface drives the calls
//! on a current-thread executor (`futures::executor::block_on`, or a tokio
//! `Builder::new_current_thread` runtime), with no tokio multi-thread runtime and no `Send`
//! bound.

use std::cell::RefCell;
use std::collections::BTreeMap;

use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes, Multikey};
use ad4m_identity_core::build::{cosign, Signer};
use ad4m_identity_core::inbox::{
    check_payload, pre_address, sign_pre_auth, Accept, InboxAddress, InboxBody, InboxError,
    InboxPayload, PreInboxAuth,
};
use ad4m_identity_core::link::{decode_secret, make_claim, match_code, DeviceString, OfferString};
use ad4m_identity_core::record::{RecordError, RECORD_KEY_LEN};
use ad4m_identity_core::{
    did_of, is_ed25519_multikey, scid_of, signing_bytes, x25519_multikey_of,
    x25519_secret_of_ed25519, Event, HeadStatement, KelChain, Op, X25519Secret, REQUEST_DOMAIN,
};
use ad4m_ldk::identity::{
    InboxAddress as RouteAddress, InboxBind, InboxWrite, LogPage, PostedRequest, RequestBody,
    SignedHead, SignedRequest, LOG_PAGE_EVENTS,
};
use async_trait::async_trait;
use serde::Serialize;
use serde_json::{json, Value};
use zeroize::Zeroize;

use crate::executor::{ExecutorRpc, RpcError};
use crate::request::REQUEST_TTL_S;
use crate::secret::Secret;
use crate::{Clock, Random};

/// `X-Coasys-Request`: base64url(JCS(SignedRequest)), on every signed route.
const REQUEST_HEADER: &str = "x-coasys-request";
/// `X-Coasys-Inbox`: base64url(JCS(PreInboxAuth)), on a pre-join inbox read or delete.
const PRE_INBOX_HEADER: &str = "x-coasys-inbox";
/// A reply body over one log page is refused (`invalid: too-large`), as the agent language
/// bounds `httpFetch`.
const MAX_REPLY_BYTES: usize = ad4m_ldk::identity::MAX_LOG_PAGE_BYTES;
/// `identity.prepareDevice`'s device string lives this long, as on the executor.
const DEVICE_STRING_TTL_S: u64 = 300;
/// The request box's default time to live, as the executor's `post_request`.
const REQUEST_BOX_TTL_S: u64 = ad4m_identity_core::requests::REQUEST_TTL_S;
/// `503`'s reason when `enc` cannot open right now — the executor's `Unopened::NoKey`
/// answer.
const ENC_UNAVAILABLE: &str = "the wallet is locked, so the inbox does not open";

fn error(code: u16, message: impl Into<String>) -> RpcError {
    RpcError {
        code,
        message: message.into(),
    }
}

fn bad_request(what: impl Into<String>) -> RpcError {
    error(400, format!("invalid: {}", what.into()))
}

fn internal(what: impl Into<String>) -> RpcError {
    error(500, what)
}

/// `503`: the transport failed, as `IdentityError::Unavailable` maps on the executor.
fn unavailable(what: impl Into<String>) -> RpcError {
    error(503, format!("unavailable: {}", what.into()))
}

/// `502`: a reply that does not parse, or carries what it should not.
fn bad_reply(what: impl Into<String>) -> RpcError {
    error(502, what)
}

/// `409`: this device's key speaks for no identity (as the executor's `bound_position`).
fn no_identity() -> RpcError {
    error(
        409,
        "no-identity: this device's key speaks for no identity".to_string(),
    )
}

/// `503`: no `store_keys`, so no signed head verifies (the executor's `no-store-keys`).
fn no_store_keys() -> RpcError {
    error(
        503,
        "no-store-keys: the phone trusts no store key, so it accepts no head from the store"
            .to_string(),
    )
}

/// `501`: a method the store's routes do not serve (did:scid resolution, a wallet rekey).
fn not_served(method: &str) -> RpcError {
    error(501, format!("{method} is not served over the store"))
}

fn forbidden_reason(reason: &str) -> bool {
    matches!(
        reason,
        "vouch" | "pow" | "claim" | "signer" | "secret" | "bound" | "cosig" | "writer" | "store"
    )
}

/// The refusal of a store answer's status and body, as the executor's `backend_error` maps
/// the agent language's `refusal_of`: `400` is `invalid: {error}`, `409` names the head
/// (`conflict:`), a `key-taken`, or falls to `invalid: {error}` (`invalid: version` lets the
/// record's stale-version merge run).
fn refused(status: u16, body: &[u8]) -> RpcError {
    let value: Value = serde_json::from_slice(body).unwrap_or(Value::Null);
    let reason = value
        .get("error")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let invalid = |fallback: &str| {
        error(
            400,
            format!(
                "invalid: {}",
                if reason.is_empty() { fallback } else { reason }
            ),
        )
    };
    match status {
        400 => invalid("malformed"),
        403 => {
            if forbidden_reason(reason) {
                error(403, format!("forbidden: {reason:?}"))
            } else {
                invalid("forbidden")
            }
        }
        404 => error(404, "not-found"),
        409 => {
            if let Some(head) = value.get("head").filter(|h| h.is_object()) {
                error(
                    409,
                    format!(
                        "conflict: the store's head is seq {} ({})",
                        head["seq"].as_u64().unwrap_or(0),
                        head["hash"].as_str().unwrap_or_default()
                    ),
                )
            } else if reason == "key-taken" {
                error(
                    409,
                    format!(
                        "key-taken: {} sits in a log",
                        value["key"].as_str().unwrap_or_default()
                    ),
                )
            } else {
                invalid("conflict")
            }
        }
        410 => error(
            410,
            format!(
                "gone: the offer is {}",
                value["state"].as_str().unwrap_or("gone")
            ),
        ),
        413 => invalid("too-large"),
        429 => error(
            429,
            format!(
                "rate-limited: {}, retry after {} s",
                value["scope"].as_str().unwrap_or("address"),
                value["retryAfter"].as_u64().unwrap_or(60)
            ),
        ),
        s if s >= 500 => unavailable(if reason.is_empty() {
            "the store did not answer".to_string()
        } else {
            reason.to_string()
        }),
        s => error(
            s.max(500),
            format!("{s}: {}", String::from_utf8_lossy(body)),
        ),
    }
}

/// base64url without padding (RFC 4648).
fn base64url(bytes: &[u8]) -> String {
    const B64URL: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
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

/// A `X-Coasys-*` header value: base64url(JCS(value)).
fn encode_header<T: Serialize>(value: &T) -> Result<String, RpcError> {
    let value = serde_json::to_value(value).map_err(|e| internal(format!("request: {e}")))?;
    Ok(base64url(
        canonical_json(&value)
            .map_err(|e| internal(format!("request: {e}")))?
            .as_bytes(),
    ))
}

/// `JCS(value)` — the body bytes a signed request's `bodyHash` covers.
fn body_bytes<T: Serialize>(value: &T) -> Result<Vec<u8>, RpcError> {
    let value = serde_json::to_value(value).map_err(|e| bad_request(e.to_string()))?;
    Ok(canonical_json(&value)
        .map_err(|e| bad_request(e.to_string()))?
        .into_bytes())
}

fn signed_request_of(value: &Value) -> Result<SignedRequest, RpcError> {
    serde_json::from_value(value.clone())
        .map_err(|e| bad_request(format!("request is a SignedRequest: {e}")))
}

fn event_of(value: &Value) -> Result<Event, RpcError> {
    Event::from_json(value).map_err(|e| bad_request(format!("{} ({e})", e.name())))
}

/// A request's `GET` or `PUT` of `/v1/private/{scid}` is for the record its `did` names — as
/// the executor checks before passing one on.
fn record_scid(request: &SignedRequest, method: &str) -> Result<String, RpcError> {
    let scid = scid_of(&request.req.did)
        .ok_or_else(|| bad_request(format!("{} is not a did:scid:coasys:1", request.req.did)))?;
    if request.req.path != format!("/v1/private/{scid}") || request.req.method != method {
        return Err(bad_request("the request is not for this identity's record"));
    }
    Ok(scid.to_string())
}

/// What the caller's transport answers.
#[derive(Clone, Debug)]
pub struct HttpResponse {
    pub status: u16,
    pub headers: Vec<(String, String)>,
    pub body: Vec<u8>,
}

/// One request the store client sends.
#[derive(Clone, Debug)]
pub struct HttpRequest {
    /// `GET`, `POST`, `PUT` or `DELETE`.
    pub method: String,
    /// The endpoint's `url` plus the route's path and query.
    pub url: String,
    /// `content-type` when a body goes out, `x-coasys-request` on a signed route,
    /// `x-coasys-inbox` on a pre-join read.
    pub headers: Vec<(String, String)>,
    /// The canonical JSON of the request's body — the bytes a `bodyHash` covers.
    pub body: Vec<u8>,
}

/// Why `HttpClient::send` gave no reply: a network failure or the caller's own timeout. A
/// status the store answered is a `HttpResponse`, never an `Err`.
#[derive(Debug, Clone, thiserror::Error)]
#[error("{0}")]
pub struct HttpError(pub String);

/// The phone's transport, supplied by the surface: an HTTP client, or the store's router in
/// tests. An `Err` is one `unavailable`.
#[async_trait(?Send)]
pub trait HttpClient {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError>;
}

/// Why [`EncKey::open`] or [`EncKey::unwrap_record_key`] gave no answer, split as the
/// executor's `Unopened` is: `Unavailable` is transient — the keystore is locked or the open
/// failed in between — so the message is kept and the call answers `unavailable` (503);
/// `Sealed` is final — the bytes do not open with this device's key at this address — so the
/// message is for no one here and the caller drops it.
#[derive(Debug, thiserror::Error)]
pub enum EncOpenError<E> {
    /// The key cannot open right now: a locked keystore, an I/O failure. Try again later.
    #[error("{0}")]
    Unavailable(String),
    /// It does not open with this device's key: it is for no one here.
    #[error("{0}")]
    Sealed(E),
}

/// The device's X25519 key, kept behind the port: `enc` opens inbox messages and unwraps the
/// record's key for this device's entry. A phone's keystore implementation holds the secret;
/// [`SoftEncKey`] is the software version for tests and callers without one.
pub trait EncKey {
    /// The `enc` multikey a claim or device string carries.
    fn enc(&self) -> Multikey;
    /// Opens a message sealed for `address` (`coasys:inbox` + address is the seal's `info`, so
    /// a seal for one inbox never opens at another). `Unavailable` keeps the message for the
    /// next try; `Sealed` drops it.
    fn open(
        &self,
        address: &InboxAddress,
        sealed: &str,
    ) -> Result<InboxPayload, EncOpenError<InboxError>>;
    /// The record key that `wrapped` (this device's entry of a record's `wrapped`) seals for
    /// `scid`'s record — the same two error cases as `open`.
    fn unwrap_record_key(
        &self,
        scid: &str,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>>;
}

/// A software [`EncKey`] holding the X25519 secret in memory.
pub struct SoftEncKey {
    secret: X25519Secret,
}

impl SoftEncKey {
    /// An `EncKey` over `secret`.
    pub fn new(secret: X25519Secret) -> Self {
        Self { secret }
    }

    /// The `enc` of a device key kept in one Ed25519 secret (as an executor's wallet derives
    /// it): `x25519_secret_of_ed25519`.
    pub fn from_ed25519(key: &ed25519_dalek::SigningKey) -> Self {
        Self::new(x25519_secret_of_ed25519(key))
    }
}

impl EncKey for SoftEncKey {
    fn enc(&self) -> Multikey {
        x25519_multikey_of(&self.secret)
    }

    fn open(
        &self,
        address: &InboxAddress,
        sealed: &str,
    ) -> Result<InboxPayload, EncOpenError<InboxError>> {
        ad4m_identity_core::inbox::open(&self.secret, address, sealed).map_err(EncOpenError::Sealed)
    }

    fn unwrap_record_key(
        &self,
        scid: &str,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>> {
        ad4m_identity_core::record::unwrap_key(wrapped, scid, &self.secret)
            .map(|key| Secret::new(*key))
            .map_err(EncOpenError::Sealed)
    }
}

/// Where the store sits and which keys' signed heads count.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoreEndpoint {
    /// The store's base URL, as an offer string or device string names it (`offer-store` is
    /// refused when an offer's store is not this one).
    pub url: String,
    /// The store keys (Ed25519 multikeys) a signed head must be signed by — the executor's
    /// `identity.storeKeys`. Empty: `ViaStore` accepts no head, so every call that signs or
    /// commits refuses `no-store-keys`.
    pub keys: Vec<String>,
}

/// A pre-join inbox `identity.prepareDevice` opened and `identity.inbox` reads until
/// `expires`.
#[derive(Clone)]
struct Prepared {
    r: String,
    address: String,
    /// The `store` of a `PreInboxAuth`: the did:key of the first trusted store key.
    store: String,
    expires: u64,
}

/// This device key's position in its identity's log: `key_id` at the verified `head` a
/// signed request names (the executor's `device_position` over its cache's trusted seq).
struct Position {
    scid: String,
    did: String,
    key_id: String,
    head: SignedHead,
}

/// `GET`, `PUT` and friends signed by `device`: the header kind.
enum Auth<'a> {
    None,
    Request(&'a SignedRequest),
    Pre(&'a PreInboxAuth),
}

/// An [`ExecutorRpc`] over the identity store: `ViaExecutor<ViaStore>` runs the flows on a
/// device that has no executor of its own. See the module's docs for the route table.
pub struct ViaStore {
    http: Box<dyn HttpClient>,
    device: Box<dyn Signer>,
    enc: Box<dyn EncKey>,
    clock: Box<dyn Clock>,
    random: Box<dyn Random>,
    endpoint: StoreEndpoint,
    /// `canKeepMaster` of a claim `identity.inbox` makes of a ticket (a claim through
    /// `identity.claimOffer` carries its own).
    can_keep_master: bool,
    /// The SCID this device's key speaks for, once settled.
    identity: RefCell<Option<String>>,
    /// offer id → scid, of claims this device made (`identity.joinStatus` looks one up).
    joins: RefCell<BTreeMap<String, String>>,
    /// The pre-join inbox `identity.prepareDevice` opened, until its device string expires.
    prepared: RefCell<Option<Prepared>>,
}

impl ViaStore {
    /// The executor of a phone. `identity` is the SCID this device's key speaks for, reloaded
    /// from the caller's storage; `None` and the key index discovers it.
    pub fn new(
        http: Box<dyn HttpClient>,
        device: Box<dyn Signer>,
        enc: Box<dyn EncKey>,
        clock: Box<dyn Clock>,
        random: Box<dyn Random>,
        endpoint: StoreEndpoint,
        identity: Option<String>,
    ) -> Self {
        ViaStore {
            http,
            device,
            enc,
            clock,
            random,
            endpoint,
            can_keep_master: true,
            identity: RefCell::new(identity),
            joins: RefCell::new(BTreeMap::new()),
            prepared: RefCell::new(None),
        }
    }

    /// `canKeepMaster` of a claim `identity.inbox` makes of a ticket. `true` by default: a
    /// phone keeps master keys, as an executor's custody channel says for it.
    pub fn with_can_keep_master(mut self, can_keep_master: bool) -> Self {
        self.can_keep_master = can_keep_master;
        self
    }

    /// This device key's Ed25519 multikey.
    pub fn device_key(&self) -> Multikey {
        self.device.key()
    }

    /// This device's `enc` multikey.
    pub fn device_enc(&self) -> Multikey {
        self.enc.enc()
    }

    /// The SCID this device's key speaks for, as the last call settled it.
    pub fn identity(&self) -> Option<String> {
        self.identity.borrow().clone()
    }

    fn trusted(&self) -> bool {
        !self.endpoint.keys.is_empty()
    }

    /// A signed head's signature covers the statement and its `store` key is trusted.
    fn verifies(&self, head: &SignedHead) -> bool {
        HeadStatement {
            scid: &head.scid,
            seq: head.seq,
            hash: &head.hash,
            time: head.time,
            store: &head.store,
        }
        .verify(&head.sig, &self.endpoint.keys)
    }

    /// Sends one request to the store and answers `(status, body)`. A transport failure is
    /// `unavailable` (503); a body over [`MAX_REPLY_BYTES`] is `invalid: too-large`.
    async fn send(
        &self,
        method: &str,
        path: &str,
        body: Option<Vec<u8>>,
        auth: Auth<'_>,
    ) -> Result<(u16, Vec<u8>), RpcError> {
        let mut headers = Vec::with_capacity(3);
        match auth {
            Auth::Request(request) => {
                headers.push((REQUEST_HEADER.to_string(), encode_header(request)?));
            }
            Auth::Pre(pre) => {
                headers.push((PRE_INBOX_HEADER.to_string(), encode_header(pre)?));
            }
            Auth::None => {}
        }
        if body.is_some() {
            headers.push(("content-type".to_string(), "application/json".to_string()));
        }
        let url = format!("{}{}", self.endpoint.url.trim_end_matches('/'), path);
        let reply = self
            .http
            .send(HttpRequest {
                method: method.to_string(),
                url,
                headers,
                body: body.unwrap_or_default(),
            })
            .await
            .map_err(|e| unavailable(e.0))?;
        if reply.body.len() > MAX_REPLY_BYTES {
            return Err(bad_request("too-large"));
        }
        Ok((reply.status, reply.body))
    }

    /// `path`'s `status`, body parsed; any non-2xx is its [`refused`].
    async fn fetch(
        &self,
        method: &str,
        path: &str,
        body: Option<Vec<u8>>,
        auth: Auth<'_>,
    ) -> Result<Value, RpcError> {
        let (status, body) = self.send(method, path, body, auth).await?;
        if !(200..300).contains(&status) {
            return Err(refused(status, &body));
        }
        let text = if body.is_empty() {
            b"null"
        } else {
            body.as_slice()
        };
        serde_json::from_slice(text).map_err(|e| bad_reply(format!("the store served {e}")))
    }

    /// The log's events and head from `from` (paged: a page holds at most `LOG_PAGE_EVENTS`).
    /// With `store_keys`, the head is verified (`502 bad-head-signature` otherwise); without
    /// them it is whatever the store says — `trusted: false` on `status`.
    async fn log_from(&self, scid: &str, from: u64) -> Result<(Vec<Value>, SignedHead), RpcError> {
        let mut events = Vec::new();
        let mut cursor = from;
        let head = loop {
            let (status, body) = self
                .send(
                    "GET",
                    &format!("/v1/logs/{scid}?from={cursor}"),
                    None,
                    Auth::None,
                )
                .await?;
            if status != 200 {
                return Err(refused(status, &body));
            }
            let page: LogPage = serde_json::from_slice(&body)
                .map_err(|e| bad_reply(format!("the store's log page does not parse: {e}")))?;
            if page.events.len() > LOG_PAGE_EVENTS as usize {
                return Err(bad_request("too-large"));
            }
            cursor += page.events.len() as u64;
            let done = page.events.is_empty() || cursor > page.head.seq;
            events.extend(page.events);
            if done {
                break page.head;
            }
        };
        if self.trusted() && !self.verifies(&head) {
            return Err(error(
                502,
                "bad-head-signature: the store's head is not signed by a trusted store key",
            ));
        }
        Ok((events, head))
    }

    /// `scid`'s log folded here (the client re-checks every log it reads) with its verified
    /// head.
    async fn chain(&self, scid: &str) -> Result<(KelChain, SignedHead), RpcError> {
        let (values, head) = self.log_from(scid, 0).await?;
        let events: Vec<Event> = values
            .iter()
            .map(Event::from_json)
            .collect::<Result<_, _>>()
            .map_err(|e| bad_request(format!("{} ({e})", e.name())))?;
        let chain =
            crate::fold(&events).map_err(|e| bad_request(format!("the log does not fold: {e}")))?;
        Ok((chain, head))
    }

    /// The SCID this device's key speaks for: `identity`, else the key index's entry for the
    /// device key (`None` when it sits in no log). The entry goes into `identity`, as the
    /// executor records a key's binding from the log.
    async fn scid_of_self(&self) -> Result<Option<String>, RpcError> {
        if let Some(scid) = self.identity.borrow().as_ref() {
            return Ok(Some(scid.clone()));
        }
        let key = self.device.key();
        let (status, body) = self
            .send("GET", &format!("/v1/keys/{key}"), None, Auth::None)
            .await?;
        match status {
            200 => {
                let entry: ad4m_ldk::identity::KeyIndexEntry = serde_json::from_slice(&body)
                    .map_err(|e| {
                        bad_reply(format!("the store's key index entry does not parse: {e}"))
                    })?;
                let Some(scid) = scid_of(&entry.did).map(str::to_string) else {
                    return Err(bad_reply(format!(
                        "the store's key index names {} for this key",
                        entry.did
                    )));
                };
                *self.identity.borrow_mut() = Some(scid.clone());
                Ok(Some(scid))
            }
            404 => Ok(None),
            _ => Err(refused(status, &body)),
        }
    }

    /// This device key's position in its identity's log: `scid`, `key_id` and the verified
    /// head a signed request names. `409 no-identity` when the key speaks for no log's
    /// identity, or is no longer valid without the bot mark at its head — as the executor's
    /// `bound_position`.
    async fn position(&self) -> Result<Position, RpcError> {
        if !self.trusted() {
            return Err(no_store_keys());
        }
        let Some(scid) = self.scid_of_self().await? else {
            return Err(no_identity());
        };
        let (chain, head) = self.chain(&scid).await?;
        let state = chain.state();
        let now = self.clock.now();
        let Some(key_id) = state
            .device_key_id_of(&self.device.key())
            .filter(|id| {
                state.bot_scope_at(id, chain.head().seq).is_none()
                    && state.key_valid_at_time(id, chain.head().seq, now)
            })
            .cloned()
        else {
            return Err(no_identity());
        };
        Ok(Position {
            did: did_of(&scid),
            scid,
            key_id,
            head,
        })
    }

    /// [`ViaStore::position`] as an option: `None` when this device's key speaks for no
    /// identity, for calls that answer `null` or an empty list then (`takeMaster`, the
    /// joined `inbox`).
    async fn maybe_position(&self) -> Result<Option<Position>, RpcError> {
        match self.position().await {
            Ok(position) => Ok(Some(position)),
            Err(e) if e.message.starts_with("no-identity") => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// A store request signed by `device` at `position` (`coasys:store-request`), as the
    /// executor's `device.rs::store_request`.
    fn sign_request(
        &self,
        position: &Position,
        method: &str,
        path: &str,
        body: &[u8],
    ) -> Result<SignedRequest, RpcError> {
        let mut nonce = [0u8; 16];
        self.random.fill(&mut nonce);
        let req = RequestBody {
            did: position.did.clone(),
            store: position.head.store.clone(),
            key_id: position.key_id.clone(),
            kel_seq: position.head.seq,
            kel_hash: position.head.hash.clone(),
            method: method.to_string(),
            path: path.to_string(),
            body_hash: hash_bytes(body),
            nonce: bs58::encode(nonce).into_string(),
            expires: self.clock.now().saturating_add(REQUEST_TTL_S),
        };
        let bytes = signing_bytes(REQUEST_DOMAIN, &req).map_err(|e| internal(e.to_string()))?;
        Ok(SignedRequest {
            req,
            sig: encode_proof_value(&self.device.sign(&bytes)),
        })
    }

    /// One store call signed by `device` at its position.
    async fn as_device(
        &self,
        position: &Position,
        method: &str,
        path: &str,
        body: Option<Vec<u8>>,
    ) -> Result<(u16, Vec<u8>), RpcError> {
        let signed_path = path.split('?').next().unwrap_or(path);
        let request = self.sign_request(
            position,
            method,
            signed_path,
            body.as_deref().unwrap_or_default(),
        )?;
        self.send(method, path, body, Auth::Request(&request)).await
    }

    /// `DELETE` of an inbox message signed by `device`; a `404` counts (the executor's
    /// `drop_joined` keeps failures out of the answer too).
    async fn drop_joined(&self, position: &Position, id: &str) {
        let path = format!("/v1/inbox/{}/{}/{id}", position.scid, position.key_id);
        let _ = self.as_device(position, "DELETE", &path, None).await;
    }

    /// `identity.status`: the keys this device holds, the identity the key index settles,
    /// and `keyId`, `revoked`, `head` from the folded log.
    async fn status(&self) -> Result<Value, RpcError> {
        let mut out = json!({
            "did": Value::Null,
            "deviceKey": self.device.key(),
            "deviceEnc": self.enc.enc(),
            "keyId": Value::Null,
            "revoked": Value::Null,
            "head": Value::Null,
            "fork": Value::Null,
            "storeBehind": Value::Null,
            "store": {
                "url": self.endpoint.url,
                "trusted": self.trusted(),
            },
        });
        let Some(scid) = self.scid_of_self().await? else {
            return Ok(out);
        };
        out["did"] = json!(did_of(&scid));
        let (chain, head) = self.chain(&scid).await?;
        let state = chain.state();
        let key_id = state.key_id_of(&self.device.key());
        out["keyId"] = key_id.map_or(Value::Null, |id| json!(id));
        out["revoked"] = key_id
            .and_then(|id| state.keys.get(id))
            .and_then(|r| r.revoked)
            .map_or(Value::Null, |r| json!(r));
        out["head"] = serde_json::to_value(head).unwrap_or(Value::Null);
        Ok(out)
    }

    /// `identity.log`: `events` from `from` and the verified `head`, `fork` always `null`
    /// (the phone keeps no cache to have a fork in).
    async fn log(&self, params: &Value) -> Result<Value, RpcError> {
        let did = params["did"].as_str().unwrap_or_default();
        let scid =
            scid_of(did).ok_or_else(|| bad_request(format!("{did} is not a did:scid:coasys:1")))?;
        let from = params["from"].as_u64().unwrap_or(0);
        let (events, head) = self.log_from(scid, from).await?;
        Ok(json!({
            "did": did_of(scid),
            "events": events,
            "head": head,
            "fork": Value::Null,
        }))
    }

    /// `identity.cosignIncept`: this device co-signs an incept that lists its keys — local;
    /// the publish lands the signature.
    async fn cosign_incept(&self, params: &Value) -> Result<Value, RpcError> {
        if let Some(scid) = self.scid_of_self().await? {
            return Err(error(
                409,
                format!(
                    "identity-exists: this device's key speaks for an identity already ({scid})"
                ),
            ));
        }
        let mut event = event_of(&params["event"])?;
        let Op::Incept { add, .. } = &event.op else {
            return Err(bad_request("the event is no incept"));
        };
        let listed = add
            .iter()
            .find(|d| d.sign == self.device.key())
            .map(|d| (d.id.clone(), d.enc.clone()))
            .ok_or_else(|| {
                error(
                    403,
                    "not-listed: the incept does not list this device's key".to_string(),
                )
            })?;
        if listed.1 != self.enc.enc() {
            return Err(error(
                403,
                "wrong-enc: the incept's enc for this key is not this device's".to_string(),
            ));
        }
        cosign(&mut event, &listed.0, self.device.as_ref()).map_err(|e| internal(e.to_string()))?;
        crate::fold(std::slice::from_ref(&event))
            .map_err(|e| bad_request(format!("the log does not fold: {e}")))?;
        serde_json::to_value(&event).map_err(|e| internal(e.to_string()))
    }

    /// `identity.publish`: `POST /v1/logs/{scid}/events`; a `409` answers the head when the
    /// store's log already holds the events (same hashes), else the conflict — the executor's
    /// `appended_elsewhere`.
    async fn publish(&self, params: &Value) -> Result<Value, RpcError> {
        if !self.trusted() {
            return Err(no_store_keys());
        }
        let events: Vec<Event> = params["events"]
            .as_array()
            .ok_or_else(|| bad_request("no events"))?
            .iter()
            .map(event_of)
            .collect::<Result<_, _>>()?;
        let Some(first) = events.iter().min_by_key(|e| e.seq) else {
            return Err(bad_request("no events"));
        };
        let scid = first.scid.clone();
        if events.iter().any(|e| e.scid != scid) {
            return Err(bad_request("the events name two logs"));
        }
        let append = ad4m_ldk::identity::AppendRequest {
            events: params["events"]
                .clone()
                .as_array()
                .cloned()
                .unwrap_or_default(),
            vouch: None,
            pow: None,
        };
        let (status, body) = self
            .send(
                "POST",
                &format!("/v1/logs/{scid}/events"),
                Some(body_bytes(&append)?),
                Auth::None,
            )
            .await?;
        let head: SignedHead = match status {
            200..=299 => serde_json::from_value(
                serde_json::from_slice::<Value>(&body)
                    .map_err(|e| bad_reply(format!("the store served {e}")))?["head"]
                    .clone(),
            )
            .map_err(|e| bad_reply(format!("the store's head does not parse: {e}")))?,
            409 => {
                let refused = || refused(status, &body);
                let _: SignedHead = serde_json::from_value(
                    serde_json::from_slice::<Value>(&body)
                        .map_err(|e| bad_reply(format!("the store served {e}")))?["head"]
                        .clone(),
                )
                .map_err(|_| refused())?;
                let from = events.iter().map(|e| e.seq).min().unwrap_or(0);
                let (held_values, held_head) = self.log_from(&scid, from).await?;
                let held: Vec<Event> = held_values
                    .iter()
                    .map(Event::from_json)
                    .collect::<Result<_, _>>()
                    .map_err(|e| bad_reply(format!("the store served {e}")))?;
                let ours = |a: &Event, b: &Event| {
                    a.seq == b.seq
                        && ad4m_identity_core::event_hash(a).ok()
                            == ad4m_identity_core::event_hash(b).ok()
                };
                if !events.iter().all(|e| held.iter().any(|h| ours(h, e))) {
                    return Err(refused());
                }
                held_head
            }
            _ => return Err(refused(status, &body)),
        };
        if !self.verifies(&head) {
            return Err(error(
                502,
                "bad-head-signature: the store's head is not signed by a trusted store key",
            ));
        }
        // The publish bound this key to `scid` when the log lists it; the index settles it on
        // the next `scid_of_self`.
        serde_json::to_value(head).map_err(|e| internal(e.to_string()))
    }

    /// `identity.openOffer`: the signed offer the caller already signed passes on.
    async fn open_offer(&self, params: &Value) -> Result<Value, RpcError> {
        let offer = &params["signedOffer"];
        let scid = offer["offer"]["scid"]
            .as_str()
            .ok_or_else(|| bad_request("signedOffer.offer.scid"))?;
        self.fetch(
            "POST",
            &format!("/v1/offers/{scid}"),
            Some(body_bytes(offer)?),
            Auth::None,
        )
        .await
    }

    /// `identity.offers`: `GET /v1/offers/{scid}?wait=` carrying the caller's master-signed
    /// request.
    async fn offers(&self, params: &Value) -> Result<Value, RpcError> {
        let request = signed_request_of(&params["request"])?;
        let scid = scid_of(&request.req.did).ok_or_else(|| {
            bad_request(format!("{} is not a did:scid:coasys:1", request.req.did))
        })?;
        let wait = params["wait"].as_u64().unwrap_or(0).min(8);
        self.fetch(
            "GET",
            &format!("/v1/offers/{scid}?wait={wait}"),
            None,
            Auth::Request(&request),
        )
        .await
    }

    /// `identity.claimOffer`: parses the offer string or link, makes the claim with `device`
    /// and `enc`, and posts it. The holder shows the same match code.
    async fn claim_offer(&self, params: &Value) -> Result<Value, RpcError> {
        let offer = params["offer"]
            .as_str()
            .ok_or_else(|| bad_request("offer is an offer string or link"))?;
        let offer =
            OfferString::parse_link(offer).map_err(|e| bad_request(format!("the offer: {e}")))?;
        self.claim(&offer, params["hint"].as_str().map(str::to_string), params)
            .await
    }

    /// The claim of `offer` with this device's keys — `identity.claimOffer`, and a ticket
    /// claimed through `identity.inbox` (which carries `can_keep_master`, not a param).
    async fn claim(
        &self,
        offer: &OfferString,
        hint: Option<String>,
        params: &Value,
    ) -> Result<Value, RpcError> {
        if !self.trusted() {
            return Err(no_store_keys());
        }
        if offer.store.trim_end_matches('/') != self.endpoint.url.trim_end_matches('/') {
            return Err(error(
                400,
                format!(
                    "offer-store: the offer is for {}, this device speaks to {}",
                    offer.store, self.endpoint.url
                ),
            ));
        }
        if let Some(scid) = self.scid_of_self().await? {
            return Err(error(
                409,
                format!(
                    "already-linked: this device's key speaks for an identity already ({scid})"
                ),
            ));
        }
        let secret = decode_secret(&offer.s)
            .ok_or_else(|| bad_request("the offer's secret is malformed"))?;
        let can_keep_master = params["canKeepMaster"]
            .as_bool()
            .unwrap_or(self.can_keep_master);
        let claim = make_claim(
            &offer.scid,
            &offer.offer_id,
            secret,
            self.device.as_ref(),
            self.enc.enc(),
            can_keep_master,
            hint,
        )
        .map_err(|e| bad_request(e.to_string()))?;
        let receipt = self
            .fetch(
                "POST",
                &format!("/v1/offers/{}/{}/claim", offer.scid, offer.offer_id),
                Some(body_bytes(&claim)?),
                Auth::None,
            )
            .await?;
        self.joins
            .borrow_mut()
            .insert(offer.offer_id.clone(), offer.scid.clone());
        Ok(json!({
            "did": did_of(&offer.scid),
            "offerId": offer.offer_id,
            "matchCode": match_code(&offer.scid, &offer.offer_id, &self.device.key()),
            "kind": receipt["kind"],
            "expires": receipt["expires"],
        }))
    }

    /// `identity.joinStatus`: the offer's `state` for this device's key; on `spent` the log
    /// lists the key and `identity` settles.
    async fn join_status(&self, params: &Value) -> Result<Value, RpcError> {
        let offer_id = params["offerId"]
            .as_str()
            .ok_or_else(|| bad_request("offerId"))?;
        let Some(scid) = self.joins.borrow().get(offer_id).cloned() else {
            return Err(error(404, "not-found: no claim of this device"));
        };
        let progress = self
            .fetch(
                "GET",
                &format!(
                    "/v1/offers/{scid}/{offer_id}/state?key={}",
                    self.device.key()
                ),
                None,
                Auth::None,
            )
            .await?;
        let mut out = json!({
            "state": progress["state"],
            "did": did_of(&scid),
            "keyId": Value::Null,
            "seq": progress["seq"],
        });
        if progress["state"].as_str() == Some("spent") {
            let (chain, _head) = self.chain(&scid).await?;
            if let Some(key_id) = chain.state().device_key_id_of(&self.device.key()) {
                *self.identity.borrow_mut() = Some(scid.clone());
                out["keyId"] = json!(key_id);
            }
        }
        Ok(out)
    }

    /// `identity.devices`: the log's device keys as `DeviceView`s; the names stay in the
    /// private record, which the store cannot read.
    async fn devices(&self) -> Result<Value, RpcError> {
        let Some(scid) = self.scid_of_self().await? else {
            return Ok(json!([]));
        };
        let (chain, _head) = self.chain(&scid).await?;
        let key = self.device.key();
        let mut views: Vec<Value> = chain
            .state()
            .keys
            .values()
            .map(|record| {
                let scope = record.scopes.last().map(|(_, scope)| scope);
                json!({
                    "keyId": record.id,
                    "sign": record.sign,
                    "bot": scope.map(|s| json!({ "can": s.can, "expires": s.expires })),
                    "expires": scope.map(|s| s.expires),
                    "addedSeq": record.added,
                    "revokedSeq": record.revoked,
                    "thisDevice": record.sign == key,
                })
            })
            .collect();
        views.sort_by_key(|v| {
            v["keyId"]
                .as_str()
                .unwrap_or("")
                .trim_start_matches('d')
                .parse::<u64>()
                .unwrap_or(u64::MAX)
        });
        Ok(Value::Array(views))
    }

    /// `identity.prepareDevice`: the device string (`coasys:device:1:…`); `identity.inbox`
    /// reads its pre-join inbox until `expires`.
    async fn prepare_device(&self) -> Result<Value, RpcError> {
        if !self.trusted() {
            return Err(no_store_keys());
        }
        let sign = self.device.key();
        let enc = self.enc.enc();
        let mut r = [0u8; 16];
        self.random.fill(&mut r);
        let r = bs58::encode(r).into_string();
        let expires = self.clock.now().saturating_add(DEVICE_STRING_TTL_S);
        let device = DeviceString {
            sign: sign.clone(),
            enc: enc.clone(),
            r: r.clone(),
            exp: expires,
            hint: None,
            store: self.endpoint.url.clone(),
        }
        .encode();
        let address = pre_address(&sign, &r).map_err(|e| internal(e.to_string()))?;
        *self.prepared.borrow_mut() = Some(Prepared {
            r,
            address,
            store: format!("did:key:{}", self.endpoint.keys[0]),
            expires,
        });
        Ok(json!({
            "sign": sign,
            "enc": enc,
            "device": device,
            "expires": expires,
        }))
    }

    /// `identity.inbox`: the waiting messages of the pre-join inbox (while a device string is
    /// live; a ticket that passes its checks is claimed at once, like the executor's
    /// pre-join read) and of the joined inbox, by `id` and `kind` only.
    async fn inbox(&self, params: &Value) -> Result<Value, RpcError> {
        let mut entries = Vec::new();
        let prepared = self.prepared.borrow().clone();
        if let Some(pre) = prepared {
            if self.clock.now() >= pre.expires {
                self.prepared.take();
            } else {
                let wait = params["wait"].as_u64().unwrap_or(0).min(8);
                let path = format!("/v1/inbox/pre/{}", pre.address);
                let auth = self.pre_auth(&pre, "GET", &path)?;
                let listed = self
                    .fetch(
                        "GET",
                        &format!("{path}?wait={wait}"),
                        None,
                        Auth::Pre(&auth),
                    )
                    .await?;
                for message in listed.as_array().cloned().unwrap_or_default() {
                    if let Some(entry) = self.take_ticket(&pre, &message).await? {
                        entries.push(entry);
                    }
                }
            }
        }
        if let Some(position) = self.maybe_position().await? {
            let path = format!("/v1/inbox/{}/{}", position.scid, position.key_id);
            let (status, body) = self.as_device(&position, "GET", &path, None).await?;
            if status != 200 {
                return Err(refused(status, &body));
            }
            let listed: Vec<Value> = serde_json::from_slice(&body)
                .map_err(|e| bad_reply(format!("the store served {e}")))?;
            let sealed_at = InboxAddress::Joined {
                scid: position.scid.clone(),
                key_id: position.key_id.clone(),
            };
            for message in listed {
                let id = message["id"].as_str().unwrap_or_default().to_string();
                match self
                    .enc
                    .open(&sealed_at, message["sealed"].as_str().unwrap_or_default())
                {
                    Ok(mut payload) => {
                        let kind = match &payload.body {
                            InboxBody::Master { .. } => "master",
                            InboxBody::OfferTicket { .. } => "offer-ticket",
                        };
                        wipe_body(&mut payload);
                        entries.push(json!({
                            "id": id,
                            "kind": kind,
                            "storedAt": message["storedAt"],
                            "expires": message["expires"],
                        }));
                    }
                    Err(EncOpenError::Sealed(_)) => self.drop_joined(&position, &id).await,
                    Err(EncOpenError::Unavailable(_)) => return Err(unavailable(ENC_UNAVAILABLE)),
                }
            }
        }
        Ok(Value::Array(entries))
    }

    /// A pre-join read's `X-Coasys-Inbox`, signed by `device` under `coasys:inbox`.
    fn pre_auth(&self, pre: &Prepared, method: &str, path: &str) -> Result<PreInboxAuth, RpcError> {
        let mut nonce = [0u8; 16];
        self.random.fill(&mut nonce);
        sign_pre_auth(
            &pre.r,
            &pre.store,
            method,
            path,
            &bs58::encode(nonce).into_string(),
            self.clock.now().saturating_add(REQUEST_TTL_S),
            self.device.as_ref(),
        )
        .map_err(|e| internal(e.to_string()))
    }

    /// `DELETE`s a pre-join message; failures are kept out of the answer, as `ack_pre`.
    async fn ack_pre(&self, pre: &Prepared, id: &str) {
        let path = format!("/v1/inbox/pre/{}/{id}", pre.address);
        if let Ok(auth) = self.pre_auth(pre, "DELETE", &path) {
            let _ = self.send("DELETE", &path, None, Auth::Pre(&auth)).await;
        }
    }

    /// One pre-join message: a valid ticket is claimed for this device (as the executor's
    /// `take_ticket`); everything else is acknowledged and dropped. Answers the entry for the
    /// caller, with the claim's `did`, `offerId` and `matchCode`.
    async fn take_ticket(
        &self,
        pre: &Prepared,
        message: &Value,
    ) -> Result<Option<Value>, RpcError> {
        let id = message["id"].as_str().unwrap_or_default().to_string();
        let sealed_at = InboxAddress::Pre(pre.address.clone());
        let mut payload = match self
            .enc
            .open(&sealed_at, message["sealed"].as_str().unwrap_or_default())
        {
            Ok(payload) => payload,
            Err(EncOpenError::Sealed(_)) => {
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
            Err(EncOpenError::Unavailable(_)) => return Err(unavailable(ENC_UNAVAILABLE)),
        };
        let scid = match &payload.body {
            InboxBody::OfferTicket(ticket) => ticket.scid.clone(),
            InboxBody::Master { .. } => {
                wipe_body(&mut payload);
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
        };
        let chain = match self.chain(&scid).await {
            Ok((chain, _)) => chain,
            Err(_) => {
                wipe_body(&mut payload);
                return Ok(None);
            }
        };
        let mut ticket = match check_payload(&payload, &self.device.key(), &chain, self.clock.now())
        {
            Ok(Accept::ClaimOffer(ticket)) => ticket,
            Ok(_) => {
                wipe_body(&mut payload);
                return Ok(None);
            }
            Err(_) => {
                wipe_body(&mut payload);
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
        };
        wipe_body(&mut payload);
        let mut offer = ticket.offer_string();
        ticket.s.zeroize();
        let claimed = self
            .claim(
                &offer,
                None,
                &json!({ "canKeepMaster": self.can_keep_master }),
            )
            .await;
        offer.s.zeroize();
        self.ack_pre(pre, &id).await;
        match claimed {
            Ok(claim) => {
                self.prepared.take();
                Ok(Some(json!({
                    "id": id,
                    "kind": "offer-ticket",
                    "storedAt": message["storedAt"],
                    "expires": message["expires"],
                    "did": claim["did"],
                    "offerId": claim["offerId"],
                    "matchCode": claim["matchCode"],
                })))
            }
            // As `take_ticket` on the executor: a claim that the store refuses is logged by
            // the caller; the message is acknowledged and the read goes on.
            Err(_) => Ok(None),
        }
    }

    /// `identity.takeMaster`: the oldest waiting master key sealed to this device, opened by
    /// `enc` and checked against the fresh log; `null` when none waits.
    async fn take_master(&self) -> Result<Value, RpcError> {
        let Some(position) = self.maybe_position().await? else {
            return Ok(Value::Null);
        };
        let (chain, _head) = self.chain(&position.scid).await?;
        let path = format!("/v1/inbox/{}/{}", position.scid, position.key_id);
        let (status, body) = self.as_device(&position, "GET", &path, None).await?;
        if status != 200 {
            return Err(refused(status, &body));
        }
        let listed: Vec<Value> = serde_json::from_slice(&body)
            .map_err(|e| bad_reply(format!("the store served {e}")))?;
        let sealed_at = InboxAddress::Joined {
            scid: position.scid.clone(),
            key_id: position.key_id.clone(),
        };
        for message in listed {
            let id = message["id"].as_str().unwrap_or_default().to_string();
            let mut payload = match self
                .enc
                .open(&sealed_at, message["sealed"].as_str().unwrap_or_default())
            {
                Ok(payload) => payload,
                Err(EncOpenError::Sealed(_)) => {
                    self.drop_joined(&position, &id).await;
                    continue;
                }
                Err(EncOpenError::Unavailable(_)) => return Err(unavailable(ENC_UNAVAILABLE)),
            };
            if !matches!(payload.body, InboxBody::Master { .. }) {
                wipe_body(&mut payload);
                continue;
            }
            match check_payload(&payload, &self.device.key(), &chain, self.clock.now()) {
                Ok(Accept::KeepMaster { .. }) => {
                    let InboxBody::Master { did, index, .. } = &payload.body else {
                        wipe_body(&mut payload);
                        continue;
                    };
                    let out = json!({
                        "messageId": id,
                        "did": did,
                        "index": index,
                        "secret": match &payload.body {
                            InboxBody::Master { secret, .. } => secret.clone(),
                            _ => String::new(),
                        },
                    });
                    wipe_body(&mut payload);
                    return Ok(out);
                }
                Ok(Accept::Stale) | Err(_) => {
                    wipe_body(&mut payload);
                    self.drop_joined(&position, &id).await;
                }
                Ok(Accept::ClaimOffer(_)) => {
                    wipe_body(&mut payload);
                }
            }
        }
        Ok(Value::Null)
    }

    /// `identity.inboxAck`: `DELETE` of message `id` of the joined inbox, signed by `device`;
    /// `404` still answers `true` (the message is gone either way).
    async fn inbox_ack(&self, params: &Value) -> Result<Value, RpcError> {
        let position = self.position().await?;
        let id = params["id"].as_str().ok_or_else(|| bad_request("id"))?;
        let path = format!("/v1/inbox/{}/{}/{id}", position.scid, position.key_id);
        let (status, body) = self.as_device(&position, "DELETE", &path, None).await?;
        match status {
            204 | 404 => Ok(json!(true)),
            _ => Err(refused(status, &body)),
        }
    }

    /// `identity.inboxPut`: the caller's sealed message and master-signed request pass on to
    /// `PUT /v1/inbox/pre/{a}` (with `bind`) or `PUT /v1/inbox/{scid}/{keyId}`.
    async fn inbox_put(&self, params: &Value) -> Result<Value, RpcError> {
        let address: RouteAddress = serde_json::from_value(params["address"].clone())
            .map_err(|e| bad_request(format!("address: {e}")))?;
        let path = match &address {
            RouteAddress::Pre { pre } => format!("/v1/inbox/pre/{pre}"),
            RouteAddress::Joined { scid, key_id } => format!("/v1/inbox/{scid}/{key_id}"),
        };
        let bind = params
            .get("bind")
            .filter(|b| !b.is_null())
            .map(|b| serde_json::from_value::<InboxBind>(b.clone()))
            .transpose()
            .map_err(|e| bad_request(format!("bind: {e}")))?;
        let write = InboxWrite {
            sealed: params["sealed"]
                .as_str()
                .ok_or_else(|| bad_request("sealed"))?
                .to_string(),
            bind,
        };
        let request = signed_request_of(&params["request"])?;
        self.fetch(
            "PUT",
            &path,
            Some(body_bytes(&write)?),
            Auth::Request(&request),
        )
        .await
    }

    /// `identity.record`: `GET /v1/private/{scid}`, with the caller's master-signed request
    /// when given (the restore's read before this device joins), else signed by `device`.
    /// `404` → `null`.
    async fn record(&self, params: &Value) -> Result<Value, RpcError> {
        let (status, body) = if let Some(request) = params.get("request").filter(|r| !r.is_null()) {
            let request = signed_request_of(request)?;
            let scid = record_scid(&request, "GET")?;
            self.send(
                "GET",
                &format!("/v1/private/{scid}"),
                None,
                Auth::Request(&request),
            )
            .await?
        } else {
            let position = self.position().await?;
            self.as_device(
                &position,
                "GET",
                &format!("/v1/private/{}", position.scid),
                None,
            )
            .await?
        };
        match status {
            404 => Ok(Value::Null),
            200 => serde_json::from_slice(&body)
                .map_err(|e| bad_reply(format!("the store served {e}"))),
            _ => Err(refused(status, &body)),
        }
    }

    /// `identity.recordPut`: `PUT /v1/private/{scid}` with the sealed record as its body —
    /// the bytes the request's `bodyHash` covers — and the caller's master-signed request or
    /// `device`'s.
    async fn record_put(&self, params: &Value) -> Result<Value, RpcError> {
        let body = body_bytes(&params["record"])?;
        if let Some(request) = params.get("request").filter(|r| !r.is_null()) {
            let request = signed_request_of(request)?;
            let scid = record_scid(&request, "PUT")?;
            self.fetch(
                "PUT",
                &format!("/v1/private/{scid}"),
                Some(body),
                Auth::Request(&request),
            )
            .await
        } else {
            let position = self.position().await?;
            let path = format!("/v1/private/{}", position.scid);
            let (status, reply) = self.as_device(&position, "PUT", &path, Some(body)).await?;
            if !(200..300).contains(&status) {
                return Err(refused(status, &reply));
            }
            serde_json::from_slice(&reply).map_err(|e| bad_reply(format!("the store served {e}")))
        }
    }

    /// `identity.unwrapRecordKey`: `enc` unwraps this device's entry of the identity's record
    /// — local.
    async fn unwrap_record_key(&self, params: &Value) -> Result<Value, RpcError> {
        let Some(scid) = self.scid_of_self().await? else {
            return Err(no_identity());
        };
        let wrapped = params["wrapped"]
            .as_str()
            .ok_or_else(|| bad_request("wrapped"))?;
        let key = self
            .enc
            .unwrap_record_key(&scid, wrapped)
            .map_err(|e| match e {
                EncOpenError::Sealed(e) => error(400, format!("open: {e}")),
                EncOpenError::Unavailable(_) => unavailable("the wallet is locked"),
            })?;
        Ok(json!({ "key": bs58::encode(key.expose()).into_string() }))
    }

    /// `identity.requests`: `GET /v1/requests/{scid}?since=` signed by `device`.
    async fn requests(&self, params: &Value) -> Result<Value, RpcError> {
        let position = self.position().await?;
        let since = params["since"].as_u64().unwrap_or(0);
        let path = format!("/v1/requests/{}?since={since}", position.scid);
        let (status, body) = self.as_device(&position, "GET", &path, None).await?;
        if !(200..300).contains(&status) {
            return Err(refused(status, &body));
        }
        serde_json::from_slice(&body).map_err(|e| bad_reply(format!("the store served {e}")))
    }

    /// `identity.postRequest`: `POST /v1/requests/{scid}` with `{body, expires}` signed by
    /// `device` — the agent's own device key posts, as on the executor.
    async fn post_request(&self, params: &Value) -> Result<Value, RpcError> {
        let position = self.position().await?;
        let expires = params["expires"]
            .as_u64()
            .unwrap_or_else(|| self.clock.now().saturating_add(REQUEST_BOX_TTL_S));
        let post = PostedRequest {
            body: serde_json::from_value(params["body"].clone())
                .map_err(|e| bad_request(format!("body: {e}")))?,
            expires,
        };
        let (status, reply) = self
            .as_device(
                &position,
                "POST",
                &format!("/v1/requests/{}", position.scid),
                Some(body_bytes(&post)?),
            )
            .await?;
        if !(200..300).contains(&status) {
            return Err(refused(status, &reply));
        }
        serde_json::from_slice(&reply).map_err(|e| bad_reply(format!("the store served {e}")))
    }

    /// `identity.deleteRequest`: `DELETE` to the caller request's own signed `path`.
    async fn delete_request(&self, params: &Value) -> Result<Value, RpcError> {
        let request = signed_request_of(&params["request"])?;
        let (status, body) = self
            .send("DELETE", &request.req.path, None, Auth::Request(&request))
            .await?;
        match status {
            204 => Ok(json!(true)),
            _ => Err(refused(status, &body)),
        }
    }

    /// `identity.keyOwner`: `GET /v1/keys/{key}` — the key index's identity of a key.
    async fn key_owner(&self, params: &Value) -> Result<Value, RpcError> {
        let key = params["key"].as_str().unwrap_or_default();
        if !is_ed25519_multikey(key) {
            return Err(bad_request("key is an Ed25519 multikey"));
        }
        let (status, body) = self
            .send("GET", &format!("/v1/keys/{key}"), None, Auth::None)
            .await?;
        match status {
            200 => {
                let entry: ad4m_ldk::identity::KeyIndexEntry = serde_json::from_slice(&body)
                    .map_err(|e| bad_reply(format!("the store served {e}")))?;
                Ok(json!({
                    "did": entry.did,
                    "keyId": entry.key_id,
                    "added": entry.added,
                    "revoked": entry.revoked,
                }))
            }
            _ => Err(refused(status, &body)),
        }
    }
}

/// Wipes a payload's secrets after its contents were used — a master key's `secret` and a
/// ticket's `s` — as the executor's `Opened` guard does on drop.
fn wipe_body(payload: &mut InboxPayload) {
    match &mut payload.body {
        InboxBody::Master { secret, .. } => secret.zeroize(),
        InboxBody::OfferTicket(ticket) => ticket.s.zeroize(),
    }
}

#[async_trait(?Send)]
impl ExecutorRpc for ViaStore {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        match method {
            "identity.status" => self.status().await,
            "identity.log" => self.log(&params).await,
            "identity.cosignIncept" => self.cosign_incept(&params).await,
            "identity.publish" => self.publish(&params).await,
            "identity.openOffer" => self.open_offer(&params).await,
            "identity.offers" => self.offers(&params).await,
            "identity.claimOffer" => self.claim_offer(&params).await,
            "identity.joinStatus" => self.join_status(&params).await,
            "identity.devices" => self.devices().await,
            "identity.prepareDevice" => self.prepare_device().await,
            "identity.inbox" => self.inbox(&params).await,
            "identity.inboxAck" => self.inbox_ack(&params).await,
            "identity.takeMaster" => self.take_master().await,
            "identity.inboxPut" => self.inbox_put(&params).await,
            "identity.record" => self.record(&params).await,
            "identity.recordPut" => self.record_put(&params).await,
            "identity.unwrapRecordKey" => self.unwrap_record_key(&params).await,
            "identity.requests" => self.requests(&params).await,
            "identity.postRequest" => self.post_request(&params).await,
            "identity.deleteRequest" => self.delete_request(&params).await,
            "identity.keyOwner" => self.key_owner(&params).await,
            // The phone keeps no cache to drop: every read is fresh from the store.
            "identity.resetLog" => Ok(json!(true)),
            // The phone holds its own keys: the custody channel's answer is their multikeys.
            "identity.custodyStatus" => Ok(json!({
                "sign": self.device.key(),
                "enc": self.enc.enc(),
            })),
            other => Err(not_served(other)),
        }
    }
}
