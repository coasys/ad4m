//! The request box: short requests between a person's own devices (spec section 17).
//!
//! A device key without the bot mark posts a [`PostedRequest`] with a signed store request;
//! apps that hold the current master key read the box and act only on the person's yes. A
//! request never opens a link, moves a key or changes the log by itself. The store keeps the
//! signed request beside the body, so a reader checks that the device posted it with
//! [`verify_posted`]: its `bodyHash` is the hash of `JCS({body, expires})` ([`posted_bytes`]),
//! the bytes the agent language sends and the only bytes a store takes.

use ad4m_hash_chain::{canonical_json, decode_proof_value, hash_bytes, Hash};
use serde::{Deserialize, Serialize};

use crate::{did_of, verify_signed, KelChain, KeyId, KeyState, REQUEST_DOMAIN};

/// Every request lives at most 7 days.
pub const REQUEST_TTL_S: u64 = 7 * 86_400;
/// How far a poster's clock may run ahead of the store's.
pub const MAX_CLOCK_SKEW_S: u64 = 300;
/// The lifetime a poster gives a request it posts: [`REQUEST_TTL_S`] less [`MAX_CLOCK_SKEW_S`],
/// so a store whose clock is behind the poster's still accepts it.
pub const REQUEST_POST_TTL_S: u64 = REQUEST_TTL_S - MAX_CLOCK_SKEW_S;
/// Open requests per identity.
pub const MAX_OPEN_REQUESTS: usize = 16;
/// Open requests per posting device key. A device needs few at once: one `send-master` (a new
/// one replaces the older) and a `confirm-revoke` or two while the person decides. Four is a
/// quarter of [`MAX_OPEN_REQUESTS`], so one key (a lost or misbehaving device) never fills the
/// box: the identity's other device keys still post until at least four keys are at their cap.
pub const MAX_OPEN_REQUESTS_PER_KEY: usize = 4;
/// Key ids one `confirm-revoke` names, at most.
pub const MAX_REVOKE_KEYS: usize = 32;

/// What a device asks.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
pub enum DeviceRequestBody {
    /// A device whose master key no longer counts asks for the current one. The answering app
    /// seals it to the `enc` key the log lists for the poster. A new one from the same key
    /// replaces the older. (A struct variant, so an extra field is refused like any other.)
    SendMaster {},
    /// An app without the master key asks an app with it to sign a revocation, and the
    /// rotation when `rotate` is set (that one needs the words).
    #[serde(rename_all = "camelCase")]
    ConfirmRevoke { key_ids: Vec<KeyId>, rotate: bool },
}

impl DeviceRequestBody {
    /// The wire name of the kind.
    pub fn kind(&self) -> &'static str {
        match self {
            DeviceRequestBody::SendMaster {} => "send-master",
            DeviceRequestBody::ConfirmRevoke { .. } => "confirm-revoke",
        }
    }
}

/// The body of `POST /v1/requests/{scid}`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PostedRequest {
    pub body: DeviceRequestBody,
    /// Unix seconds; at most [`REQUEST_TTL_S`] after the store takes it.
    pub expires: u64,
}

/// Why a request is refused.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RequestError {
    #[error("malformed: {0}")]
    Malformed(String),
    #[error("the request expires in the past or more than 7 days ahead")]
    Expires,
    #[error("the signed store request is not the post of this request: {0}")]
    Request(String),
    #[error("the log has not reached seq {0}, where the request was signed: read the log first")]
    Behind(u64),
    #[error(
        "{0} is not a device key without the bot mark valid where the request was signed and at the head"
    )]
    Signer(String),
    #[error("the request's signature does not verify")]
    Signature,
}

impl RequestError {
    /// A short name for error bodies.
    pub fn name(&self) -> &'static str {
        match self {
            RequestError::Malformed(_) => "malformed",
            RequestError::Expires => "expires",
            RequestError::Request(_) => "request",
            RequestError::Behind(_) => "behind",
            RequestError::Signer(_) => "signer",
            RequestError::Signature => "signature",
        }
    }
}

/// Checks a posted request against the log at `head_seq` and the store's clock: `expires`
/// after `now` and at most 7 days ahead; a `confirm-revoke` names 1 to 32 distinct device keys
/// of the log, each valid at the head.
pub fn check_post(
    post: &PostedRequest,
    state: &KeyState,
    head_seq: u64,
    now: u64,
) -> Result<(), RequestError> {
    if post.expires <= now || post.expires > now.saturating_add(REQUEST_TTL_S) {
        return Err(RequestError::Expires);
    }
    if let DeviceRequestBody::ConfirmRevoke { key_ids, .. } = &post.body {
        if key_ids.is_empty() || key_ids.len() > MAX_REVOKE_KEYS {
            return Err(RequestError::Malformed(format!(
                "confirm-revoke names 1 to {MAX_REVOKE_KEYS} keys"
            )));
        }
        for (i, id) in key_ids.iter().enumerate() {
            if key_ids[..i].contains(id) {
                return Err(RequestError::Malformed(format!("{id} is named twice")));
            }
            if !state.keys.contains_key(id) || !state.key_valid_at(id, head_seq) {
                return Err(RequestError::Malformed(format!(
                    "{id} is not a device key valid at the head"
                )));
            }
        }
    }
    Ok(())
}

/// The signed part of a store request (`coasys:store-request`): the `req` of the
/// `X-Coasys-Request` header a device posted its request with.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct StoreRequest {
    pub did: String,
    /// The `did:key` of the store the request was for.
    pub store: String,
    pub key_id: KeyId,
    pub kel_seq: u64,
    pub kel_hash: Hash,
    pub method: String,
    pub path: String,
    pub body_hash: Hash,
    pub nonce: String,
    pub expires: u64,
}

/// A store request with its signature (base58btc multibase) by `req.keyId`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SignedStoreRequest {
    pub req: StoreRequest,
    pub sig: String,
}

/// One request of the box as a store lists it (`GET /v1/requests/{scid}`): the posted body and
/// expiry, the posting key's id, and the signed store request it was posted with.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BoxedRequest {
    pub id: String,
    pub body: DeviceRequestBody,
    pub by: KeyId,
    pub created: u64,
    pub expires: u64,
    pub signed: SignedStoreRequest,
}

/// The bytes of a post: `JCS({body, expires})`. A poster sends exactly these (they are what its
/// `bodyHash` covers) and a store refuses any other bytes, so a reader that hashes them gets the
/// poster's `bodyHash`.
pub fn posted_bytes(post: &PostedRequest) -> Result<String, RequestError> {
    let value = serde_json::to_value(post).map_err(|e| RequestError::Malformed(e.to_string()))?;
    canonical_json(&value).map_err(|e| RequestError::Malformed(e.to_string()))
}

/// Checks, for a reader of the box, that the device `request.by` posted `request` to the box of
/// `chain`'s identity: the signed store request names this identity, `POST
/// /v1/requests/<scid>`, the key `by` and the hash of [`posted_bytes`] of the request's body and
/// expiry; it names a position (`kelSeq`, `kelHash`) of `chain` (`Behind` when `chain` has not
/// reached it: read the log first); `by` is a device key without the bot mark, valid at that
/// position and at `chain`'s head (a key revoked since no longer speaks: its requests go with
/// its revocation, spec section 17); and its `coasys:store-request` signature verifies.
///
/// The store request's own `expires` (5 minutes after signing) is not read: a reader reads a
/// request for up to 7 days. Nor is its `store`: a request posted at one store and copied to
/// another is still the device's.
pub fn verify_posted(request: &BoxedRequest, chain: &KelChain) -> Result<(), RequestError> {
    let req = &request.signed.req;
    let scid = chain.id();
    let mismatch = |what: &str| Err(RequestError::Request(what.to_string()));
    if req.did != did_of(scid) {
        return mismatch("it is signed for another identity");
    }
    if !req.method.eq_ignore_ascii_case("POST") || req.path != format!("/v1/requests/{scid}") {
        return mismatch("it is signed for another method or path");
    }
    if req.key_id != request.by {
        return mismatch("it is signed by another key than the poster");
    }
    let post = PostedRequest {
        body: request.body.clone(),
        expires: request.expires,
    };
    if req.body_hash != hash_bytes(posted_bytes(&post)?.as_bytes()) {
        return mismatch("bodyHash is not the hash of JCS({body, expires})");
    }
    let head = chain.head();
    if req.kel_seq > head.seq {
        return Err(RequestError::Behind(req.kel_seq));
    }
    if chain.hash_at(req.kel_seq) != Some(&req.kel_hash) {
        return mismatch("kelSeq and kelHash name no position of the log");
    }
    let state = chain.state();
    let record = state
        .keys
        .get(&request.by)
        .filter(|r| {
            r.scopes.is_empty()
                && state.key_valid_at(&r.id, req.kel_seq)
                && state.key_valid_at(&r.id, head.seq)
        })
        .ok_or_else(|| RequestError::Signer(request.by.clone()))?;
    let sig = decode_proof_value(&request.signed.sig).ok_or(RequestError::Signature)?;
    if !verify_signed(REQUEST_DOMAIN, req, &record.sign, &sig) {
        return Err(RequestError::Signature);
    }
    Ok(())
}
