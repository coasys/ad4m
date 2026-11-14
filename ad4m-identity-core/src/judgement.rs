//! The judgement of a link and the bytes a link signs: one copy for every reader (the executor,
//! the link server and the link language through the wasm exports).
//!
//! The verdict of a link says whether it counts and where it is shown. It is a pure function of
//! the link's signature, its anchor, what the signer's log says up to its trusted seq, and where
//! the link entered the space's ordered history. No I/O: the caller reads the log and the order.
//!
//! | Verdict | Logic reads (values, counts, votes, roles, ordering) | Display reads |
//! |---|---|---|
//! | `Valid` | count | show |
//! | `Invalid(reason)` | exclude | hide |
//! | `CantCheck` | exclude until a re-check | show, with the verdict |
//! | `Flagged` | exclude | show, with the verdict |
//!
//! A link signs one of two byte strings, by whether its proof names an anchor:
//! [`anchored_signing_bytes`] (`coasys:link` over `{data, timestamp, anchor}`) or, without an
//! anchor, the old bytes [`hash_data_and_timestamp`]. `vectors/link-signing-bytes.json` pins
//! both.

use std::str::FromStr;

use chrono::{DateTime, SecondsFormat, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::domain::{signing_bytes, verify_bytes, DomainError, LINK_DOMAIN};
use crate::KeyState;

/// The bot scope action a link needs: a link is a write.
const WRITE: &str = "write";

/// Whether a link counts.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(tag = "kind", content = "reason", rename_all = "camelCase")]
pub enum LinkVerdict {
    /// Every check holds.
    Valid,
    /// A check fails for good.
    Invalid(InvalidReason),
    /// The signer's log is not known up to the anchor yet (the store did not answer, or its
    /// trusted head is below the anchor). A re-check runs when the log is known.
    CantCheck,
    /// The signer's key was valid at the anchor, but the link entered the space's ordered
    /// history after the key's `revoke` entered it.
    Flagged,
}

/// Why a link is `Invalid`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "ts", derive(ts_rs::TS))]
#[serde(rename_all = "camelCase")]
pub enum InvalidReason {
    /// The signature does not hold under the signer's key.
    BadSignature,
    /// The link has an anchor, but no log lists the signer, or the log the key index names
    /// does not list it.
    NotListed,
    /// The signer is a master key. A master signs log events, never content.
    MasterKey,
    /// The anchor is before the seq that added the key.
    NotYetValid,
    /// The key was revoked at or before the anchor.
    Revoked,
    /// A bot key's scope is past its `expires` by the reader's clock.
    Expired,
    /// A bot key whose scope at the anchor does not include `write`: a link is a write, so the
    /// key may not author it. Every reader enforces it from the owner's log.
    NotInScope,
}

impl InvalidReason {
    fn name(self) -> &'static str {
        match self {
            InvalidReason::BadSignature => "badSignature",
            InvalidReason::NotListed => "notListed",
            InvalidReason::MasterKey => "masterKey",
            InvalidReason::NotYetValid => "notYetValid",
            InvalidReason::Revoked => "revoked",
            InvalidReason::Expired => "expired",
            InvalidReason::NotInScope => "notInScope",
        }
    }

    fn from_name(name: &str) -> Option<InvalidReason> {
        [
            InvalidReason::BadSignature,
            InvalidReason::NotListed,
            InvalidReason::MasterKey,
            InvalidReason::NotYetValid,
            InvalidReason::Revoked,
            InvalidReason::Expired,
            InvalidReason::NotInScope,
        ]
        .into_iter()
        .find(|r| r.name() == name)
    }
}

impl LinkVerdict {
    /// Logic reads count the link.
    pub fn counts(&self) -> bool {
        matches!(self, LinkVerdict::Valid)
    }

    /// Display reads show the link (all but `Invalid`).
    pub fn shown(&self) -> bool {
        !matches!(self, LinkVerdict::Invalid(_))
    }

    /// The verdict as the link store keeps it: `valid`, `invalid:<reason>`, `cantCheck`,
    /// `flagged`.
    pub fn stored(&self) -> String {
        match self {
            LinkVerdict::Valid => "valid".into(),
            LinkVerdict::Invalid(reason) => format!("invalid:{}", reason.name()),
            LinkVerdict::CantCheck => "cantCheck".into(),
            LinkVerdict::Flagged => "flagged".into(),
        }
    }

    /// The verdict from its stored form; `None` for a form [`LinkVerdict::stored`] never writes.
    pub fn from_stored(s: &str) -> Option<LinkVerdict> {
        match s {
            "valid" => Some(LinkVerdict::Valid),
            "cantCheck" => Some(LinkVerdict::CantCheck),
            "flagged" => Some(LinkVerdict::Flagged),
            _ => s
                .strip_prefix("invalid:")
                .and_then(InvalidReason::from_name)
                .map(LinkVerdict::Invalid),
        }
    }
}

/// What the signer's log says, as the reader holds it.
#[derive(Clone, Copy, Debug)]
pub enum SignerLog<'a> {
    /// A log lists the signer. `trusted`: the seq of its accepted, store-signed head. Nothing
    /// past it counts.
    Listed { state: &'a KeyState, trusted: u64 },
    /// No log lists the signer: the key index does not know it, or the log it names does not
    /// list it.
    NoLog,
    /// Nothing is known: the lookup failed or did not run.
    Unknown,
}

/// The inputs of [`judge`].
#[derive(Clone, Copy, Debug)]
pub struct LinkFacts<'a> {
    /// The signature holds under the signer's `did:key` (the old signed bytes without an
    /// anchor, `coasys:link` over `{data, timestamp, anchor}` with one).
    pub signature_holds: bool,
    /// The signer's signing multikey.
    pub signer: &'a str,
    /// The seq of the signer's log that the link names; `None` for a link without one.
    pub anchor: Option<u64>,
    /// What the signer's log says.
    pub log: SignerLog<'a>,
    /// Where the link entered the space's ordered history, compared with the `revoke` of its
    /// key: `Some(true)` after it, `Some(false)` before it, `None` when the space gives no
    /// order (a private perspective, or a link language that records no revocations).
    pub entered_after_revoke: Option<bool>,
    /// The reader's clock (unix seconds), for bot scopes only.
    pub now: u64,
}

/// The verdict of a link.
///
/// 1. A signature that does not hold: `Invalid(BadSignature)`.
/// 2. No anchor and no log that lists the signer (a device-only key, or nothing known):
///    `Valid`, as before key logs.
/// 3. No anchor, and a log lists the signer: judged as anchored at the seq that added the key.
/// 4. With an anchor: no log lists the signer, `Invalid(NotListed)`; nothing known, or the
///    anchor past the trusted seq, `CantCheck`; a master key, `Invalid(MasterKey)`; the key not
///    added yet at the anchor, `Invalid(NotYetValid)`; revoked at or before it,
///    `Invalid(Revoked)`; past an expiry, `Invalid(Expired)`; a bot key whose scope at the
///    anchor does not allow `write`, `Invalid(NotInScope)` (a link is a write).
/// 5. Revoked after the anchor (at or below the trusted seq): `Valid` only when the link is
///    known to have entered the space's history before the revocation did
///    (`entered_after_revoke == Some(false)`), else `Flagged`. With no order known, whoever
///    still holds the revoked key could sign today and name an anchor before the revocation
///    (or no anchor at all), so the link is shown with its verdict and does not count.
pub fn judge(facts: &LinkFacts) -> LinkVerdict {
    use LinkVerdict::*;
    if !facts.signature_holds {
        return Invalid(InvalidReason::BadSignature);
    }
    let unlisted = |anchored_verdict: LinkVerdict| match facts.anchor {
        None => Valid,
        Some(_) => anchored_verdict,
    };
    let (state, trusted) = match facts.log {
        SignerLog::Listed { state, trusted } => (state, trusted),
        SignerLog::NoLog => return unlisted(Invalid(InvalidReason::NotListed)),
        SignerLog::Unknown => return unlisted(CantCheck),
    };
    let signer = facts.signer.to_string();
    let Some(id) = state.device_key_id_of(&signer) else {
        if state.key_id_of(&signer).is_some() {
            return Invalid(InvalidReason::MasterKey);
        }
        return unlisted(Invalid(InvalidReason::NotListed));
    };
    let Some(record) = state.keys.get(id) else {
        return unlisted(Invalid(InvalidReason::NotListed));
    };
    if record.added > trusted {
        // Listed only past the trusted seq: no store-signed head proves the listing yet.
        return unlisted(CantCheck);
    }
    let at = facts.anchor.unwrap_or(record.added);
    if at > trusted {
        return CantCheck;
    }
    if at < record.added {
        return Invalid(InvalidReason::NotYetValid);
    }
    let revoked = record.revoked.filter(|r| *r <= trusted);
    if revoked.is_some_and(|r| r <= at) {
        return Invalid(InvalidReason::Revoked);
    }
    if !state.key_valid_at_time(id, at, facts.now) {
        return Invalid(InvalidReason::Expired);
    }
    // A link is a write: a bot key authors one only when the scope in force at the anchor
    // allows `write`.
    if state
        .bot_scope_at(id, at)
        .is_some_and(|scope| !scope.can.iter().any(|a| a == WRITE))
    {
        return Invalid(InvalidReason::NotInScope);
    }
    if revoked.is_some() && facts.entered_after_revoke != Some(false) {
        return Flagged;
    }
    Valid
}

/// Why the signed bytes of a link cannot be built.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum LinkBytesError {
    /// The data has no JSON form.
    #[error("the data has no JSON form: {0}")]
    Data(String),
    /// The timestamp is not an RFC 3339 date.
    #[error("the timestamp is not an RFC 3339 date: {0}")]
    Timestamp(String),
    /// `coasys:link` refuses the value.
    #[error(transparent)]
    Domain(#[from] DomainError),
}

/// What an anchored link signs under `coasys:link`: its data, its timestamp as it carries it,
/// and the anchor.
#[derive(Serialize)]
struct AnchoredLink<'a, T: Serialize> {
    data: &'a T,
    timestamp: &'a str,
    anchor: u64,
}

/// The bytes an anchored expression signs: `coasys:link` over `{data, timestamp, anchor}`, the
/// timestamp as the expression carries it.
pub fn anchored_signing_bytes<T: Serialize>(
    data: &T,
    timestamp: &str,
    anchor: u64,
) -> Result<Vec<u8>, LinkBytesError> {
    Ok(signing_bytes(
        LINK_DOMAIN,
        &AnchoredLink {
            data,
            timestamp,
            anchor,
        },
    )?)
}

/// True when `signature` is the Ed25519 signature (strict) of the signing multikey `key` over
/// [`anchored_signing_bytes`]`(data, timestamp, anchor)`. False when those bytes cannot be built.
pub fn anchored_signature_holds<T: Serialize>(
    data: &T,
    timestamp: &str,
    anchor: u64,
    key: &str,
    signature: &[u8],
) -> bool {
    anchored_signing_bytes(data, timestamp, anchor)
        .is_ok_and(|bytes| verify_bytes(&bytes, key, signature))
}

/// The bytes an expression without an anchor signs: `sha256(json(data) ‖ timestamp)`, where
/// `json(data)` is serde_json's compact text of `data` in its field order (a link:
/// `{"predicate", "source", "target"}`) and `timestamp` is the expression's timestamp read as an
/// RFC 3339 date and written back in UTC with milliseconds (`2026-11-14T10:00:00.000Z`, what
/// JavaScript's `Date.toISOString` gives). A timestamp that is not a date has no bytes.
pub fn hash_data_and_timestamp<T: Serialize>(
    data: &T,
    timestamp: &str,
) -> Result<[u8; 32], LinkBytesError> {
    let data = serde_json::to_vec(data).map_err(|e| LinkBytesError::Data(e.to_string()))?;
    let timestamp = DateTime::<Utc>::from_str(timestamp)
        .map_err(|e| LinkBytesError::Timestamp(e.to_string()))?
        .to_rfc3339_opts(SecondsFormat::Millis, true);
    let mut hasher = Sha256::new();
    hasher.update(&data);
    hasher.update(timestamp.as_bytes());
    Ok(hasher.finalize().into())
}
