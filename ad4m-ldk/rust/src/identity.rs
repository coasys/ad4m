//! Identity wire types and the refusals of the agent language's identity
//! interface (section 5.8 of `docs-src/language-interface-spec.md`).
//!
//! Shared by the store, the executor and the agent language. The same
//! types live in `ad4m-ldk/js/src/types.ts`. Events travel as opaque JSON
//! objects: the store and the executor read each one with the identity
//! core, the agent language never does. Hashes, key ids and multikeys are
//! plain strings and a bot scope is plain JSON, so this crate does not
//! depend on the identity core.
//!
//! JSON field names are camelCase. An absent optional field is `null`,
//! except on the signed offer and claim, whose absent fields are left out
//! (their signatures cover the JCS form, so a `null` would change the
//! signed bytes).

use serde::{Deserialize, Serialize};

use crate::errors::{ErrorCode, LanguageError};

/// The store's signature over a log's head (`coasys:head`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignedHead {
    pub scid: String,
    pub seq: u64,
    pub hash: String,
    pub time: u64,
    /// did:key of the store.
    pub store: String,
    pub sig: String,
}

/// `GET /v1/logs/{scid}?from=`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LogPage {
    pub events: Vec<serde_json::Value>,
    /// `chain_entries.stored_at` of each event, one per event.
    pub appended_at: Vec<u64>,
    pub head: SignedHead,
    #[serde(default)]
    pub note: Option<VouchNote>,
    pub vouched: bool,
    #[serde(default)]
    pub pauses: Vec<PauseRecord>,
}

/// The most events a log page carries.
pub const LOG_PAGE_EVENTS: u32 = 256;
/// The largest request body a store takes, so the largest one event can be.
pub const MAX_STORE_BODY: usize = 64 * 1024;
/// The largest log page a client reads: a full page of the largest events, plus one body's
/// room for the head, `appendedAt` and the JSON around them.
pub const MAX_LOG_PAGE_BYTES: usize = (LOG_PAGE_EVENTS as usize + 1) * MAX_STORE_BODY;

/// `POST /v1/logs/{scid}/events`. Carries a vouching token or a proof, never both.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AppendRequest {
    pub events: Vec<serde_json::Value>,
    #[serde(default)]
    pub vouch: Option<String>,
    #[serde(default)]
    pub pow: Option<Pow>,
}

/// A proof of work on a new log with no note (section 14, host admission policy, of
/// `docs-src/identity/did-scid-coasys-1.md`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Pow {
    /// base58btc, 16 bytes.
    pub nonce: String,
    pub bits: u8,
}

/// `GET /v1/pow`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PowParams {
    pub bits: u8,
    pub previous_bits: u8,
    pub changed_at: u64,
}

/// `GET /v1/keys/{multikey}`: the identity a key belongs to.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct KeyIndexEntry {
    /// The owner's did:scid.
    pub did: String,
    /// "d3".
    pub key_id: String,
    pub added: u64,
    /// Seq of the revoke or rotate that ended it.
    #[serde(default)]
    pub revoked: Option<u64>,
    /// The bot scope in force at the head.
    #[serde(default)]
    pub bot: Option<serde_json::Value>,
    /// The bot scope's expiry; `None` for a standing key or a master (only a bot key expires).
    #[serde(default)]
    pub expires: Option<u64>,
    /// A bot key under a pause record in force; false for every other key.
    #[serde(default)]
    pub paused: bool,
    /// The position the answer describes.
    pub head: SignedHead,
}

/// Header `X-Coasys-Request` = base64url(JCS(SignedRequest)).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignedRequest {
    pub req: RequestBody,
    /// `coasys:store-request`.
    pub sig: String,
}

/// The signed part of a store request (`coasys:store-request`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RequestBody {
    pub did: String,
    /// did:key of the store the request is for, as a [`SignedHead`] names its store: a store
    /// refuses a request for another (`403 store`), so a request cannot be replayed to a second
    /// store that holds the same log. A client takes it from its configured store keys
    /// (`identity.storeKeys`) or from the `store` of a signed head it checked against them.
    pub store: String,
    pub key_id: String,
    pub kel_seq: u64,
    pub kel_hash: String,
    pub method: String,
    pub path: String,
    pub body_hash: String,
    /// 16 bytes, base58btc.
    pub nonce: String,
    pub expires: u64,
}

/// `PUT /v1/agents/{did}`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentRecord {
    /// did:scid, or did:key for a bot or a device-only identity.
    pub did: String,
    /// The agent's profile as a signed AD4M expression (`{author, timestamp, data, proof}`, the
    /// data today's Agent: did, perspective, directMessageLanguage); the store does not read it.
    /// A `did:key` agent's expression is signed by that key (the agent language writes it). A
    /// `did:scid` agent's is authored by the did:scid and signed by the record's `signedBy`
    /// (`proof.key`), with no `proof.valid`, `proof.invalid` or `proof.record`: the agent
    /// language answers it with [`AgentRecordProof`] added as `proof.record`.
    pub profile: serde_json::Value,
    pub version: u64,
    /// The device did:key that signed.
    pub signed_by: String,
    /// The content anchor: the seq of the signer's identity log that the signer knew when it
    /// signed. Required for a did:scid record, `null` for a did:key record. The signature holds
    /// only when the log lists `signedBy` and that key is valid at this seq (a key revoked at
    /// seq N signs nothing valid anchored at or after N), and the seq is at or below the log's
    /// head. The person is the record's did and the key id is read from the log, so the anchor
    /// carries neither; nor a hash, since a fork freezes the identity anyway.
    #[serde(default)]
    pub anchor: Option<u64>,
    /// `coasys:link` scheme over {profile, version, anchor}.
    pub sig: String,
}

/// `proof.record` on a `did:scid` profile expression from the agent language: the binding of
/// the agent record it came in. With `proof.key` as the record's `signedBy` and the expression
/// minus `proof.record` as the record's `profile`, a reader rebuilds the [`AgentRecord`] and
/// checks `sig` over [`AgentRecord::signed_value`] under `coasys:link`, then the key at
/// `anchor` in the person's log: a record signed before a revocation stays valid.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentRecordProof {
    /// The record's `version`.
    pub version: u64,
    /// The record's content anchor (always present on a `did:scid` record).
    pub anchor: u64,
    /// The record's `sig`, unchanged.
    pub sig: String,
}

impl AgentRecord {
    /// What `sig` covers under the `coasys:link` scheme: `{profile, version, anchor}`.
    pub fn signed_value(&self) -> serde_json::Value {
        serde_json::json!({ "profile": self.profile, "version": self.version, "anchor": self.anchor })
    }
}

/// The platform's note that it checked one email address for this identity.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VouchNote {
    pub did: String,
    /// "email".
    pub method: String,
    pub time: u64,
    /// did:key of the vouching key.
    pub voucher: String,
    /// `coasys:vouch`.
    pub sig: String,
}

/// The position of the key that signed an offer or a pause record.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignedBy {
    pub key_id: String,
    pub kel_seq: u64,
    pub kel_hash: String,
}

/// A bot key's pause record: the bot's owner stops the bot without revoking
/// its key.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PauseRecord {
    pub scid: String,
    /// A bot key.
    pub key_id: String,
    pub paused: bool,
    pub version: u64,
    /// A non-bot device key of the owner, at the head.
    pub by: SignedBy,
    /// `coasys:pause`.
    pub sig: String,
    /// Set by the store, outside the signature.
    pub stored_at: u64,
}

/// "node" = an executor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OfferKind {
    Device,
    Node,
    Bot,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Offer {
    pub scid: String,
    /// 16 random bytes, base58btc.
    pub offer_id: String,
    pub kind: OfferKind,
    /// multihash_b58(sha256("coasys:offer" ‖ s)).
    pub commit: String,
    /// Unix seconds, at most issue time + 300.
    pub expires: u64,
    /// A bound offer: only this signing key may claim.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub device_key: Option<String>,
    /// A bot offer's scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bot: Option<serde_json::Value>,
}

/// `coasys:offer` over {offer, by}, by the current master key.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignedOffer {
    pub offer: Offer,
    pub by: SignedBy,
    pub sig: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Claim {
    pub offer_id: String,
    /// 32 bytes, base58btc; the store checks it against `commit` and never stores it.
    pub s: String,
    pub device_key: String,
    /// X25519 of the device key.
    pub enc_key: String,
    /// True only for an app that may keep the master key.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub can_keep_master: bool,
    /// "Pixel 8", at most 64 characters; never enters the log.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
    /// The device key over signing_bytes("coasys:claim", ClaimBody).
    pub co_sig: String,
}

/// What the holder sees for a claim; `s` is gone.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClaimView {
    pub device_key: String,
    pub enc_key: String,
    pub can_keep_master: bool,
    #[serde(default)]
    pub hint: Option<String>,
    pub co_sig: String,
    pub claimed_at: u64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "kebab-case")]
pub enum OfferState {
    Open,
    Claimed { claim: ClaimView },
    Spent { seq: u64 },
    Expired,
    Cancelled,
    Contested,
    Superseded,
}

/// One item of `GET /v1/offers/{scid}`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OfferListing {
    pub offer: SignedOffer,
    pub state: OfferState,
}

/// `POST /v1/offers/{scid}`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OfferOpened {
    pub offer_id: String,
}

/// `POST /v1/offers/{scid}/{offerId}/claim`; `kind` and `expires` come from the stored offer.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClaimReceipt {
    /// Always "claimed".
    pub state: String,
    pub kind: OfferKind,
    pub expires: u64,
}

/// `GET /v1/offers/{scid}/{offerId}/state`: `claimed`, `spent` with the add's seq, or a terminal state.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OfferProgress {
    pub state: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub seq: Option<u64>,
}

/// An inbox's address: before the device joins, `{pre}` (the pre-join address of its device
/// string); after, `{scid, keyId}`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged, rename_all_fields = "camelCase")]
pub enum InboxAddress {
    Pre { pre: String },
    Joined { scid: String, key_id: String },
}

/// The bound offer a pre-join write is for: only the holder that scanned the device string
/// (it knows `r`) and holds the master key that opened the offer can write.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InboxBind {
    pub scid: String,
    pub offer_id: String,
    /// The device string's signing key, the offer's bound key.
    pub sign: String,
    /// The device string's `r`.
    pub r: String,
}

/// `PUT /v1/inbox/pre/{address}` (with `bind`) and `PUT /v1/inbox/{scid}/{keyId}` (without).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InboxWrite {
    /// base64url HPKE ciphertext, at most 8 KiB.
    pub sealed: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bind: Option<InboxBind>,
}

/// The answer of an inbox write.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InboxStored {
    pub id: String,
    pub expires: u64,
}

/// Who wrote an inbox message, as the store checked it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(
    tag = "kind",
    rename_all = "kebab-case",
    rename_all_fields = "camelCase"
)]
pub enum InboxWriter {
    /// A pre-join message: the holder of the bound offer.
    Offer { scid: String, offer_id: String },
    /// A joined message: the master key `keyId` of the identity.
    Master { scid: String, key_id: String },
}

/// One stored inbox message. The store reads only these outer fields.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InboxMessage {
    /// 16 bytes, base58btc, set by the store.
    pub id: String,
    pub sealed: String,
    pub writer: InboxWriter,
    pub stored_at: u64,
    pub expires: u64,
}

/// A pre-join read or acknowledgement: header `X-Coasys-Inbox` = base64url(JCS(PreInboxAuth)),
/// signed by the device string's key under `coasys:inbox` over every field but `sig`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PreInboxAuth {
    pub address: String,
    pub sign: String,
    pub r: String,
    /// did:key of the store the read is for, as a [`SignedRequest`]'s `store`: a store refuses
    /// a read for another (`403 store`).
    pub store: String,
    pub method: String,
    pub path: String,
    pub nonce: String,
    pub expires: u64,
    pub sig: String,
}

/// How an inbox read authenticates: a signed request by the device key (joined), or a
/// pre-join read.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum InboxAuth {
    Request(SignedRequest),
    Pre(PreInboxAuth),
}

/// `GET`/`PUT /v1/private/{scid}`: the private details record, encrypted; the store never
/// reads it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SealedRecord {
    pub version: u64,
    /// Key id or `"recovery"` → the record key sealed to that reader.
    pub wrapped: std::collections::BTreeMap<String, String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_key: Option<String>,
    pub nonce: String,
    pub ciphertext: String,
}

/// The answer of a record write.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RecordStored {
    pub version: u64,
}

/// What a device asks in the request box.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(
    tag = "kind",
    rename_all = "kebab-case",
    rename_all_fields = "camelCase"
)]
pub enum DeviceRequestBody {
    SendMaster {},
    ConfirmRevoke { key_ids: Vec<String>, rotate: bool },
}

/// `POST /v1/requests/{scid}`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PostedRequest {
    pub body: DeviceRequestBody,
    /// At most 7 days ahead.
    pub expires: u64,
}

/// The answer of a request post.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RequestPosted {
    pub id: String,
}

/// One request of the box. `signed` is the store request the device posted it with, whose
/// `bodyHash` covers `JCS({body, expires})`, so a reader checks the poster itself.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeviceRequest {
    pub id: String,
    pub body: DeviceRequestBody,
    /// The posting device key's id.
    pub by: String,
    pub created: u64,
    pub expires: u64,
    pub signed: SignedRequest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ForbiddenReason {
    Vouch,
    Pow,
    Claim,
    Signer,
    Secret,
    Bound,
    Cosig,
    Writer,
    /// A signed request or a pre-join inbox read for another store.
    Store,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RateLimitScope {
    Address,
    Global,
    Identity,
}

/// How the identity interface refuses a call; HTTP statuses map one to one.
///
/// It crosses the runtime inside a [`LanguageError`]: the error's message
/// is the refusal as JSON, so [`IdentityErrorCode::from_language_error`]
/// reads it back on the executor's side.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "code", rename_all = "kebab-case")]
pub enum IdentityErrorCode {
    Conflict {
        head: SignedHead,
    },
    KeyTaken {
        key: String,
    },
    Forbidden {
        reason: ForbiddenReason,
    },
    Invalid {
        reason: String,
    },
    RateLimited {
        scope: RateLimitScope,
        #[serde(rename = "retryAfterSec")]
        retry_after_sec: u64,
    },
    /// 410: an offer, inbox message or request that ended, with its state.
    Gone {
        state: String,
    },
    /// 404: an unknown offer, inbox address or record.
    NotFound,
    Unavailable,
}

impl IdentityErrorCode {
    /// The runtime's error class for this refusal.
    pub fn error_class(&self) -> ErrorCode {
        match self {
            Self::Conflict { .. } | Self::KeyTaken { .. } | Self::Invalid { .. } => {
                ErrorCode::InvalidInput
            }
            Self::Forbidden { .. } => ErrorCode::PermissionDenied,
            Self::RateLimited { .. } | Self::Unavailable => ErrorCode::Transient,
            Self::Gone { .. } | Self::NotFound => ErrorCode::NotFound,
        }
    }

    /// The refusal a language error carries, or `None` when it carries none.
    pub fn from_language_error(error: &LanguageError) -> Option<Self> {
        serde_json::from_str(&error.message).ok()
    }
}

impl From<IdentityErrorCode> for LanguageError {
    fn from(refusal: IdentityErrorCode) -> Self {
        let message = serde_json::to_string(&refusal)
            .unwrap_or_else(|e| format!("identity refusal not encodable: {e}"));
        LanguageError {
            code: refusal.error_class(),
            message,
        }
    }
}
