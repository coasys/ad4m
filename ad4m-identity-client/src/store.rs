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
//! - [`LogCheckpoints`] (`with_checkpoints`): where the `(seq, hash)` head each identity's
//!   log was last admitted lives — under it `409 store-behind`, beside it `409 fork`,
//!   `identity.resetLog` forgets it. The default keeps it in memory; a phone that wants
//!   the refusal to survive a restart passes a persisting port. Without one, a restart
//!   starts with no checkpoint.
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
//! | `cosignIncept`, `resetLog` | local: the co-signature, forgetting one identity's checkpoint |
//! | `custodyStatus` | the keys this device holds (`{sign, enc}`) |
//! | `resolve`, `rekey`, others | `501 not-served` |
//!
//! The trait's `?Send` matches the rest of the crate: a synchronous surface drives the calls
//! on a current-thread executor (`futures::executor::block_on`, or a tokio
//! `Builder::new_current_thread` runtime), with no tokio multi-thread runtime and no `Send`
//! bound.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes, Multikey};
use ad4m_identity_core::build::{cosign, Signer};
use ad4m_identity_core::inbox::{
    check_payload, pre_address, sign_pre_auth, Accept, InboxAddress, InboxBody, InboxError,
    InboxPayload, PreInboxAuth,
};
use ad4m_identity_core::link::{decode_secret, make_claim, match_code, DeviceString, OfferString};
use ad4m_identity_core::record::{
    envelope_signing_bytes, RecordError, SealedRecord, RECORD_KEY_LEN,
};
use ad4m_identity_core::{
    did_of, is_ed25519_multikey, scid_of, signing_bytes, x25519_multikey_of,
    x25519_secret_of_ed25519, Event, HeadStatement, KelChain, Op, X25519Secret, REQUEST_DOMAIN,
};
use ad4m_ldk::identity::{
    InboxAddress as RouteAddress, InboxBind, InboxWrite, LogPage, PostedRequest, RequestBody,
    SignedHead, SignedRequest, LOG_PAGE_EVENTS,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
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
/// The most pages one log read takes: 64 × [`LOG_PAGE_EVENTS`] events, far past any
/// person's log — the executor's `MAX_PAGES`.
const MAX_PAGES: usize = 64;
/// The most bytes one log read holds over all its pages, copies included — the executor's
/// `MAX_STORE_LOG_BYTES`. A person's key log is an incept and a few events of about a
/// kilobyte each, so this is far past any log and far below the page cap (64 ×
/// [`MAX_REPLY_BYTES`], about 1 GiB): a store cannot make a phone hold a gigabyte for one
/// answer.
const MAX_STORE_LOG_BYTES: usize = 32 << 20;
/// `identity.prepareDevice`'s device string lives this long, as on the executor.
const DEVICE_STRING_TTL_S: u64 = 300;
/// The request box's default time to live, as the executor's `post_request`.
const REQUEST_BOX_TTL_S: u64 = ad4m_identity_core::requests::REQUEST_POST_TTL_S;
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

/// One request the store client sends. `Debug` never prints the headers or the body:
/// `x-coasys-request` and `x-coasys-inbox` carry signed statements, and a claim's body
/// carries the offer's secret — none of it belongs in a log or a crash report.
#[derive(Clone)]
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

impl std::fmt::Debug for HttpRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HttpRequest")
            .field("method", &self.method)
            .field("url", &self.url)
            .field("headers", &format_args!("[{}]", self.headers.len()))
            .field("body", &format_args!("[{} bytes]", self.body.len()))
            .finish()
    }
}

/// Why `HttpClient::send` gave no reply: a network failure or the caller's own timeout. A
/// status the store answered is a `HttpResponse`, never an `Err`.
#[derive(Debug, Clone, thiserror::Error)]
#[error("{0}")]
pub struct HttpError(pub String);

/// The phone's transport, supplied by the surface: an HTTP client, or the store's router in
/// tests. An `Err` is one `unavailable`.
///
/// What an implementation must hold to — the requests carry signed statements and claim
/// secrets:
///
/// - **Authenticated HTTPS only.** The URL is the store's origin; requests go nowhere
///   else. A redirect is refused, or followed only when it keeps the origin.
/// - **A streaming reply-size limit.** The reply body is read with a cap and the read
///   fails past it; the adapter never allocates past the limit to find out. (This client
///   refuses a body over [`MAX_REPLY_BYTES`]; the limit the surface picks must be at
///   least that.)
/// - **Timeouts**: a connect timeout, a read timeout and a deadline on the whole call —
///   a stalled store must not hang a flow.
/// - **Nothing is logged.** No request or reply headers or bodies — a claim's body
///   carries the offer's secret `s`, the `x-coasys-*` headers carry signed statements.
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

/// The head of one identity's log this phone last accepted ([`ViaStore::admitted`]): the
/// checkpoint a later read is measured against — under it `409 store-behind`, beside it
/// `409 fork`; `identity.resetLog` forgets it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Checkpoint {
    pub seq: u64,
    pub hash: String,
}

/// Where the per-identity [`Checkpoint`]s live — a port the caller supplies with
/// [`ViaStore::with_checkpoints`] so a phone can persist them. The default keeps them in
/// memory only: without a persisting port, a phone restart starts with no checkpoint (a
/// store may then serve an older head once, as before the checkpoint existed).
pub trait LogCheckpoints {
    /// The checkpoint kept for `scid`, if any. An `Err` fails the read closed: a log is not
    /// admitted while the phone cannot tell what it accepted before.
    fn get(&self, scid: &str) -> Result<Option<Checkpoint>, CheckpointError>;
    /// The head `scid`'s log was last admitted at. An `Err` fails the read closed too: a head
    /// the phone could not keep is not one it can hold the store to next time.
    fn set(&self, scid: &str, checkpoint: Checkpoint) -> Result<(), CheckpointError>;
    /// `identity.resetLog`: forget `scid`'s checkpoint.
    fn forget(&self, scid: &str) -> Result<(), CheckpointError>;
}

/// Why a [`LogCheckpoints`] port could not answer: a persistent adapter's storage error, in
/// words a log can show (never the checkpoint itself, which is public anyway).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CheckpointError(pub String);

impl std::fmt::Display for CheckpointError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CheckpointError {}

/// A checkpoint port that failed: the read fails closed as `503 unavailable`.
fn checkpoints_failed(e: CheckpointError) -> RpcError {
    unavailable(format!(
        "the phone's log checkpoints cannot be read or kept: {e}"
    ))
}

/// The in-memory [`LogCheckpoints`]: what a `ViaStore` uses without `with_checkpoints`.
/// `Clone` shares one store, so a surface can hand clones of one instance to the
/// `ViaStore`s that must agree (a restart is a new `ViaStore` over the same port).
#[derive(Clone, Default)]
pub struct MemoryCheckpoints(Rc<RefCell<BTreeMap<String, Checkpoint>>>);

impl LogCheckpoints for MemoryCheckpoints {
    fn get(&self, scid: &str) -> Result<Option<Checkpoint>, CheckpointError> {
        Ok(self.0.borrow().get(scid).cloned())
    }

    fn set(&self, scid: &str, checkpoint: Checkpoint) -> Result<(), CheckpointError> {
        self.0.borrow_mut().insert(scid.to_string(), checkpoint);
        Ok(())
    }

    fn forget(&self, scid: &str) -> Result<(), CheckpointError> {
        self.0.borrow_mut().remove(scid);
        Ok(())
    }
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
    /// Per identity, the head this phone last accepted: a store that serves a lower seq,
    /// or a log that does not pass through that head, is refused (`identity.resetLog`
    /// forgets it, as the executor's does its cache). A port — [`MemoryCheckpoints`]
    /// unless `with_checkpoints` supplies a persisting one.
    checkpoints: Box<dyn LogCheckpoints>,
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
            checkpoints: Box::new(MemoryCheckpoints::default()),
        }
    }

    /// `canKeepMaster` of a claim `identity.inbox` makes of a ticket. `true` by default: a
    /// phone keeps master keys, as an executor's custody channel says for it.
    pub fn with_can_keep_master(mut self, can_keep_master: bool) -> Self {
        self.can_keep_master = can_keep_master;
        self
    }

    /// Where the per-identity accepted heads live (the checkpoint — [`LogCheckpoints`]).
    /// A phone that persists them passes its own port so a restart still refuses a store
    /// serving an older or a beside log; the in-memory default forgets them on restart.
    pub fn with_checkpoints(mut self, checkpoints: Box<dyn LogCheckpoints>) -> Self {
        self.checkpoints = checkpoints;
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

    /// The checks every read's head passes once the pages agree on it: the signature is a
    /// trusted store key's, and the statement names the log read — a signature alone binds
    /// the head to nothing the read asked for.
    fn check_head(&self, scid: &str, head: &SignedHead) -> Result<(), RpcError> {
        if self.trusted() && !self.verifies(head) {
            return Err(error(
                502,
                "bad-head-signature: the store's head is not signed by a trusted store key",
            ));
        }
        if head.scid != scid {
            return Err(error(
                502,
                "bad-head: the store's head names another log than the one read",
            ));
        }
        Ok(())
    }

    /// The log's events and head from `from` (paged: a page holds at most `LOG_PAGE_EVENTS`).
    /// A read is bounded and only ever ends on the head its pages agreed on: at most
    /// [`MAX_PAGES`] pages and [`MAX_STORE_LOG_BYTES`] served bytes, each page advancing the
    /// cursor strictly past the last one, one head across all of them. A store that cannot
    /// end a read so — pages that never reach the head it signs, a head that moves mid-read,
    /// a page that re-serves what was read — is refused, never served as a log.
    async fn log_from(&self, scid: &str, from: u64) -> Result<(Vec<Value>, SignedHead), RpcError> {
        let mut events = Vec::new();
        let mut cursor = from;
        let mut head: Option<SignedHead> = None;
        let mut budget = MAX_STORE_LOG_BYTES;
        let mut done = false;
        for _ in 0..MAX_PAGES {
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
            // The budget covers what the read holds, copies included.
            budget = budget.checked_sub(body.len()).ok_or_else(|| {
                unavailable(format!(
                    "the store's log of {scid} passes {MAX_STORE_LOG_BYTES} bytes"
                ))
            })?;
            let page: LogPage = serde_json::from_slice(&body)
                .map_err(|e| bad_reply(format!("the store's log page does not parse: {e}")))?;
            if page.events.len() > LOG_PAGE_EVENTS as usize {
                return Err(bad_request("too-large"));
            }
            // One read, one head: a head that moves under it confirmed nothing.
            match &head {
                None => head = Some(page.head.clone()),
                Some(first) if *first == page.head => {}
                Some(_) => {
                    return Err(bad_reply(
                        "the store's head changed between the log's pages",
                    ))
                }
            }
            if page.events.is_empty() {
                // The log ends here: reached only when the cursor is already past the head
                // (a `from` beyond it); an end before it is no complete log.
                if cursor > page.head.seq {
                    done = true;
                    break;
                }
                return Err(bad_reply("the store's log ends before the head it signs"));
            }
            let Some(last) = page.events.iter().filter_map(|e| e["seq"].as_u64()).max() else {
                return Err(bad_reply(
                    "the store's log page carries an event with no seq",
                ));
            };
            if last < cursor {
                return Err(bad_reply(format!(
                    "the store's log page does not advance past seq {cursor}"
                )));
            }
            cursor = last + 1;
            events.extend(page.events);
            if last >= page.head.seq {
                done = true;
                break;
            }
        }
        if !done {
            return Err(unavailable(format!(
                "the store's log of {scid} runs past {MAX_PAGES} pages"
            )));
        }
        let head = head.expect("every page keeps its head");
        self.check_head(scid, &head)?;
        Ok((events, head))
    }

    /// `scid`'s log as every read of this phone admits it: the events from the store, folded
    /// here (the client re-checks every log it reads), the verified head bound to the folded
    /// log, and the checkpoint kept. `identity.log`, which the flows read the log through,
    /// goes through here as well as every signed call.
    async fn admitted(&self, scid: &str) -> Result<(Vec<Value>, KelChain, SignedHead), RpcError> {
        let (values, head) = self.log_from(scid, 0).await?;
        let events: Vec<Event> = values
            .iter()
            .map(Event::from_json)
            .collect::<Result<_, _>>()
            .map_err(|e| bad_request(format!("{} ({e})", e.name())))?;
        let chain =
            crate::fold(&events).map_err(|e| bad_request(format!("the log does not fold: {e}")))?;
        // The signed head must be the folded log's head: otherwise the events are not what
        // the store confirmed (a head of another position, or of a prefix).
        let folded = chain.head();
        if folded.seq != head.seq || folded.hash != head.hash {
            return Err(error(
                502,
                "bad-head: the store's head is not the head of the log it served",
            ));
        }
        // Never behind, and never beside, the head this phone accepted before.
        if let Some(checkpoint) = self.checkpoints.get(scid).map_err(checkpoints_failed)? {
            if head.seq < checkpoint.seq {
                return Err(error(
                    409,
                    format!(
                        "store-behind: the store served the log at seq {}, under seq {} read before",
                        head.seq, checkpoint.seq
                    ),
                ));
            }
            if chain.hash_at(checkpoint.seq) != Some(&checkpoint.hash) {
                return Err(error(
                    409,
                    format!(
                        "fork: the log served does not pass through the head accepted at seq {}",
                        checkpoint.seq
                    ),
                ));
            }
        }
        self.checkpoints
            .set(
                scid,
                Checkpoint {
                    seq: head.seq,
                    hash: head.hash.clone(),
                },
            )
            .map_err(checkpoints_failed)?;
        Ok((values, chain, head))
    }

    /// [`Self::admitted`]'s chain and head.
    async fn chain(&self, scid: &str) -> Result<(KelChain, SignedHead), RpcError> {
        let (_, chain, head) = self.admitted(scid).await?;
        Ok((chain, head))
    }

    /// The SCID this device's key speaks for: `identity`, else the key index's entry for the
    /// device key (`None` when it sits in no log). The index's answer is a candidate, never
    /// the binding: its log is admitted here first, and `identity` installs only when that
    /// log lists this key — the executor's `trusted_listing`. A candidate that fails to
    /// read, or names a log the key is not in, installs nothing and is never exposed (a
    /// confirmed-but-revoked binding stays a binding: `position` judges validity).
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
                let (chain, _head) = self.chain(&scid).await?;
                if chain.state().device_key_id_of(&self.device.key()).is_none() {
                    // The index names a log this key is not in: nothing binds.
                    return Ok(None);
                }
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
        // The same admission as every other read (`admitted`): `from` only trims the answer.
        let (values, _chain, head) = self.admitted(scid).await?;
        let events: Vec<Value> = values
            .into_iter()
            .filter(|value| value["seq"].as_u64().is_some_and(|seq| seq >= from))
            .collect();
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
        // A signed head confirms this publish only when it is this log's and reaches the
        // events sent: a head of another log, or of a prefix, does not.
        if head.scid != scid || events.iter().any(|e| e.seq > head.seq) {
            return Err(error(
                502,
                "bad-head: the store's head does not confirm the events published",
            ));
        }
        // And the confirmed log holds the very events sent, at their seqs: the admitted chain
        // (bound to its head, checkpoint kept) has their hashes there.
        let (_, chain, _) = self.admitted(&scid).await?;
        if events
            .iter()
            .any(|e| ad4m_identity_core::event_hash(e).ok().as_ref() != chain.hash_at(e.seq))
        {
            return Err(error(
                502,
                "bad-head: the confirmed log does not hold the events published",
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
                    Ok(payload) => {
                        // `Opened` wipes the payload's secret on drop.
                        let payload = Opened(payload);
                        let kind = match &payload.body {
                            InboxBody::Master { .. } => "master",
                            InboxBody::OfferTicket { .. } => "offer-ticket",
                        };
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
    /// caller, with the claim's `did`, `offerId` and `matchCode`. The payload and the offer
    /// it names live in drop guards (`Opened`, `HeldOffer`), so a call dropped at an `await`
    /// — `chain`, `claim` — wipes them.
    async fn take_ticket(
        &self,
        pre: &Prepared,
        message: &Value,
    ) -> Result<Option<Value>, RpcError> {
        let id = message["id"].as_str().unwrap_or_default().to_string();
        let sealed_at = InboxAddress::Pre(pre.address.clone());
        let payload = match self
            .enc
            .open(&sealed_at, message["sealed"].as_str().unwrap_or_default())
        {
            Ok(payload) => Opened(payload),
            Err(EncOpenError::Sealed(_)) => {
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
            Err(EncOpenError::Unavailable(_)) => return Err(unavailable(ENC_UNAVAILABLE)),
        };
        let scid = match &payload.body {
            InboxBody::OfferTicket(ticket) => ticket.scid.clone(),
            InboxBody::Master { .. } => {
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
        };
        let chain = match self.chain(&scid).await {
            Ok((chain, _)) => chain,
            Err(_) => return Ok(None),
        };
        let mut ticket = match check_payload(&payload, &self.device.key(), &chain, self.clock.now())
        {
            Ok(Accept::ClaimOffer(ticket)) => ticket,
            Ok(_) => return Ok(None),
            Err(_) => {
                self.ack_pre(pre, &id).await;
                return Ok(None);
            }
        };
        drop(payload);
        let offer = HeldOffer(ticket.offer_string());
        ticket.s.zeroize();
        let claimed = self
            .claim(
                &offer.0,
                None,
                &json!({ "canKeepMaster": self.can_keep_master }),
            )
            .await;
        drop(offer);
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
            let payload = match self
                .enc
                .open(&sealed_at, message["sealed"].as_str().unwrap_or_default())
            {
                Ok(payload) => Opened(payload),
                Err(EncOpenError::Sealed(_)) => {
                    self.drop_joined(&position, &id).await;
                    continue;
                }
                Err(EncOpenError::Unavailable(_)) => return Err(unavailable(ENC_UNAVAILABLE)),
            };
            if !matches!(payload.body, InboxBody::Master { .. }) {
                continue;
            }
            match check_payload(&payload, &self.device.key(), &chain, self.clock.now()) {
                Ok(Accept::KeepMaster { .. }) => {
                    let InboxBody::Master { did, index, .. } = &payload.body else {
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
                    return Ok(out);
                }
                Ok(Accept::Stale) | Err(_) => {
                    self.drop_joined(&position, &id).await;
                }
                Ok(Accept::ClaimOffer(_)) => {}
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
    /// `device`'s. An envelope with an empty `sig` is this device's write: its `signer` must
    /// be this device key's id, and `device` signs the envelope here (the executor signs it
    /// in the wallet). A signed envelope — a master-signed write — passes unchanged.
    async fn record_put(&self, params: &Value) -> Result<Value, RpcError> {
        let mut sealed: SealedRecord = serde_json::from_value(params["record"].clone())
            .map_err(|e| bad_request(format!("record: {e}")))?;
        let mut position = None;
        if sealed.sig.is_empty() {
            let at = self.position().await?;
            if sealed.signer != at.key_id {
                return Err(bad_request(format!(
                    "signer: an unsigned record must name this device's key {}, not {}",
                    at.key_id, sealed.signer
                )));
            }
            let bytes = envelope_signing_bytes(&at.scid, &sealed)
                .map_err(|e| bad_request(e.to_string()))?;
            sealed.sig = encode_proof_value(&self.device.sign(&bytes));
            position = Some(at);
        }
        let body = body_bytes(&sealed)?;
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
            let position = match position {
                Some(at) => at,
                None => self.position().await?,
            };
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
            // A key in no log is the documented `null` (the restore's words derive an `m0`
            // of nothing yet), never a refusal.
            404 => Ok(Value::Null),
            _ => Err(refused(status, &body)),
        }
    }
}

/// An opened inbox payload: a secret it carries (a master key's `secret`, a ticket's `s`)
/// is wiped when it drops — the executor's `Opened` guard. A call dropped while it holds
/// one across an `await` leaks nothing.
struct Opened(InboxPayload);

impl Drop for Opened {
    fn drop(&mut self) {
        match &mut self.0.body {
            InboxBody::Master { secret, .. } => secret.zeroize(),
            InboxBody::OfferTicket(ticket) => ticket.s.zeroize(),
        }
    }
}

impl std::ops::Deref for Opened {
    type Target = InboxPayload;

    fn deref(&self) -> &InboxPayload {
        &self.0
    }
}

/// The offer a ticket names: its secret `s` is wiped when it drops, so a `claim` dropped
/// while the offer is held across its `await` leaks none.
struct HeldOffer(OfferString);

impl Drop for HeldOffer {
    fn drop(&mut self) {
        self.0.s.zeroize();
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
            // The phone keeps no log cache to drop; it forgets the head it accepted for the
            // one identity named, as the executor drops that identity's accepted head.
            "identity.resetLog" => {
                let did = params["did"].as_str().unwrap_or_default();
                let scid = scid_of(did)
                    .ok_or_else(|| bad_request(format!("{did} is not a did:scid:coasys:1")))?;
                self.checkpoints.forget(scid).map_err(checkpoints_failed)?;
                Ok(json!(true))
            }
            // The phone holds its own keys: the custody channel's answer is their multikeys.
            "identity.custodyStatus" => Ok(json!({
                "sign": self.device.key(),
                "enc": self.enc.enc(),
            })),
            other => Err(not_served(other)),
        }
    }
}
