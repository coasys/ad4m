//! Identity wire types (AR-01 4.5.2, AR-02 4.3.1) and the refusals of the
//! agent language's identity interface (AR-01 4.9).
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
//! as AR-02 4.3.1 fixes (their signatures cover the JCS form).

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

/// A proof of work on a new log with no note (AR-01 4.7).
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
    /// An Until key's expiry, or the bot scope's.
    #[serde(default)]
    pub expires: Option<u64>,
    /// A bot key under a pause record in force (AR-02); false for every other key.
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RequestBody {
    pub did: String,
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

/// The log position a signature by a logged key names (AR-01 4.8.3).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Anchor {
    pub kel_did: String,
    pub key_id: String,
    pub kel_seq: u64,
    pub kel_hash: String,
}

/// `PUT /v1/agents/{did}`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentRecord {
    /// did:scid, or did:key for a bot or a device-only identity.
    pub did: String,
    /// Today's Agent: perspective, directMessageLanguage.
    pub profile: serde_json::Value,
    pub version: u64,
    /// The device did:key that signed.
    pub signed_by: String,
    /// Required for a did:scid record.
    #[serde(default)]
    pub anchor: Option<Anchor>,
    /// `coasys:link` scheme over {profile, version, anchor}.
    pub sig: String,
}

/// The platform's note that it checked one email address for this identity (AR-01 4.6).
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

/// A bot key's pause record (AR-02 4.10.6).
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
    Locked,
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
