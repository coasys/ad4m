//! A signed link as it travels to a link server, and what the server checks with no executor
//! behind it: the link's hash and signature, its judgement into the [`JudgedLink`] the
//! membership replay takes, the trusted seq of a store-signed head, and the `coasys:room-key`
//! signature a device key puts on its X25519 key.
//!
//! The judgement is the executor's (`check_signed_with_provenance`): [`judge`] on the link's
//! facts, then the person of a counted link from the signer's log (a bot key speaks for its
//! owner, marked `by_bot`). A link the server judges arrives now, so a revoke the trusted log
//! holds entered the server's history before it (`entered_after_revoke`).

use std::str::FromStr;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Deserializer, Serialize};
use sha2::{Digest, Sha256};

use crate::domain::{signing_bytes, verify_bytes, DomainError};
use crate::judgement::{
    anchored_signature_holds, hash_data_and_timestamp, judge, LinkFacts, LinkVerdict, SignerLog,
};
use crate::membership::JudgedLink;
use crate::{HeadStatement, KelChain, KeyState};

/// The signing domain of a device key's X25519 key for room keys.
pub const ROOM_KEY_DOMAIN: &str = "coasys:room-key";

/// A signed link: `{author, timestamp, data: {source, predicate, target}, proof}`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WireLink {
    /// The signer's `did:key`.
    pub author: String,
    /// RFC 3339, as the author signed it.
    pub timestamp: String,
    pub data: WireLinkData,
    pub proof: WireProof,
}

/// The triple of a [`WireLink`].
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WireLinkData {
    pub source: String,
    pub predicate: String,
    pub target: String,
}

/// The proof of a [`WireLink`]: the signature as hex, and the anchor (the seq of the signer's
/// log the link names) when it was signed with one.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct WireProof {
    pub key: String,
    pub signature: String,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "anchor_of"
    )]
    pub anchor: Option<u64>,
}

/// An anchor is a seq: a JSON number, or the same number as a decimal string.
fn anchor_of<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Option<u64>, D::Error> {
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Anchor {
        Number(u64),
        Text(String),
    }
    match Option::<Anchor>::deserialize(deserializer)? {
        None => Ok(None),
        Some(Anchor::Number(n)) => Ok(Some(n)),
        Some(Anchor::Text(s)) if !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit()) => s
            .parse()
            .map(Some)
            .map_err(|_| serde::de::Error::custom("the anchor is not a seq")),
        Some(Anchor::Text(_)) => Err(serde::de::Error::custom("the anchor is not a seq")),
    }
}

/// The data a link signs, in the field order the executor's `Link` serializes.
#[derive(Serialize)]
struct SignedData<'a> {
    predicate: &'a str,
    source: &'a str,
    target: &'a str,
}

impl WireLink {
    fn signed_data(&self) -> SignedData<'_> {
        SignedData {
            predicate: &self.data.predicate,
            source: &self.data.source,
            target: &self.data.target,
        }
    }
}

/// Why a [`WireLink`] cannot be judged.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum WireLinkError {
    /// The timestamp is not an RFC 3339 instant at or after the epoch.
    #[error("the timestamp is not an RFC 3339 instant: {0}")]
    Timestamp(String),
}

/// The link's hash: `sha256(author ‖ source ‖ predicate ‖ target ‖ timestamp)`, the executor's
/// `link_hash` for a target already in its canonical form.
pub fn wire_link_hash(link: &WireLink) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(link.author.as_bytes());
    hasher.update(link.data.source.as_bytes());
    hasher.update(link.data.predicate.as_bytes());
    hasher.update(link.data.target.as_bytes());
    hasher.update(link.timestamp.as_bytes());
    hasher.finalize().into()
}

/// True when the proof's signature holds under the author's `did:key`: over
/// [`hash_data_and_timestamp`] without an anchor, over `coasys:link` with one.
pub fn wire_signature_holds(link: &WireLink) -> bool {
    let Some(key) = link.author.strip_prefix("did:key:") else {
        return false;
    };
    let Ok(signature) = hex::decode(&link.proof.signature) else {
        return false;
    };
    match link.proof.anchor {
        Some(anchor) => anchored_signature_holds(
            &link.signed_data(),
            &link.timestamp,
            anchor,
            key,
            &signature,
        ),
        None => hash_data_and_timestamp(&link.signed_data(), &link.timestamp)
            .is_ok_and(|bytes| verify_bytes(&bytes, key, &signature)),
    }
}

/// `timestamp` in ms since the epoch.
fn ms_of(timestamp: &str) -> Result<u64, WireLinkError> {
    DateTime::<Utc>::from_str(timestamp)
        .ok()
        .and_then(|t| u64::try_from(t.timestamp_millis()).ok())
        .ok_or_else(|| WireLinkError::Timestamp(timestamp.to_string()))
}

/// Judges `link`, arriving now, against what its signer's log says (`log`), and gives the
/// [`JudgedLink`] the replay takes. `now_ms`: the reader's clock, for bot scopes. A counted
/// link signed by a device key of a log speaks for that log's person; a bot key's link speaks
/// for its owner with `by_bot`; every other link speaks for no one.
pub fn judge_wire_link(
    link: &WireLink,
    log: SignerLog,
    now_ms: u64,
) -> Result<JudgedLink, WireLinkError> {
    let timestamp = ms_of(&link.timestamp)?;
    let signer = link.author.strip_prefix("did:key:").unwrap_or_default();
    let verdict = judge(&LinkFacts {
        signature_holds: wire_signature_holds(link),
        signer,
        anchor: link.proof.anchor,
        log,
        // It arrives now: a revoke the trusted log holds came first.
        entered_after_revoke: Some(true),
        now: now_ms / 1000,
    });
    let (person, by_bot) = match (&verdict, log) {
        (LinkVerdict::Valid, SignerLog::Listed { state, trusted }) => {
            match state.device_key_id_of(&signer.to_owned()) {
                Some(id) => (
                    Some(state.did.clone()),
                    state.bot_scope_at(id, trusted).is_some(),
                ),
                None => (None, false),
            }
        }
        _ => (None, false),
    };
    Ok(JudgedLink {
        hash: wire_link_hash(link),
        source: link.data.source.clone(),
        predicate: link.data.predicate.clone(),
        target: link.data.target.clone(),
        author_key: link.author.clone(),
        timestamp,
        verdict,
        person,
        by_bot,
    })
}

/// Whom a key speaks for: the person of its log, and whether the key is one of their bots.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct KeyAuthor {
    pub person: String,
    pub by_bot: bool,
}

/// Whom the signing multikey `key` speaks for at the trusted seq `trusted` of the log `state`
/// replays: its person when it is a device key added at or before `trusted`, not revoked at or
/// before it and, for a bot key, not past its scope's expiry by `now_ms`. `None` otherwise (a
/// master key, a key the log does not list, a revoked or expired key). A link server reads it
/// for an authenticated key at auth and at each refresh of the key's head.
pub fn author_of(state: &KeyState, key: &str, trusted: u64, now_ms: u64) -> Option<KeyAuthor> {
    let id = state.device_key_id_of(&key.to_owned())?;
    state
        .key_valid_at_time(id, trusted, now_ms / 1000)
        .then(|| KeyAuthor {
            person: state.did.clone(),
            by_bot: state.bot_scope_at(id, trusted).is_some(),
        })
}

/// A store's signed head of a log (`GET /v1/heads/{scid}`, and `head` in a key index entry).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignedHead {
    pub scid: String,
    pub seq: u64,
    pub hash: String,
    pub time: u64,
    /// The store key's `did:key`.
    pub store: String,
    /// The proof value of the store key's signature under `coasys:head`.
    pub sig: String,
}

/// The seq up to which `chain` is trusted by `head`: `Some(head.seq)` when `head` names this
/// log, a store key in `trusted_store_keys` (multikeys) signed it, and the log holds `head.hash`
/// at `head.seq`. `None` otherwise.
pub fn trusted_seq(
    chain: &KelChain,
    head: &SignedHead,
    trusted_store_keys: &[String],
) -> Option<u64> {
    if &head.scid != chain.id() || chain.hash_at(head.seq) != Some(&head.hash) {
        return None;
    }
    let statement = HeadStatement {
        scid: &head.scid,
        seq: head.seq,
        hash: &head.hash,
        time: head.time,
        store: &head.store,
    };
    statement
        .verify(&head.sig, trusted_store_keys)
        .then_some(head.seq)
}

/// Why a room key statement has no signing bytes.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RoomKeyError {
    /// The DID is no Ed25519 `did:key`, or the X25519 key is not 32 bytes of lowercase hex.
    #[error("a room key statement is an Ed25519 did:key and 32 bytes of lowercase hex")]
    Malformed,
    /// `coasys:room-key` refuses the value.
    #[error(transparent)]
    Domain(#[from] DomainError),
}

/// What a device key signs for its X25519 key: [`ROOM_KEY_DOMAIN`] over
/// `{x25519PublicKey, did}`. `did` is the device key's `did:key`; `x25519_public_key` is 32
/// bytes as lowercase hex.
pub fn room_key_signing_bytes(did: &str, x25519_public_key: &str) -> Result<Vec<u8>, RoomKeyError> {
    let is_did_key = did
        .strip_prefix("did:key:")
        .is_some_and(|k| ad4m_hash_chain::decode_ed25519_multikey(k).is_some());
    let is_x25519 = x25519_public_key.len() == 64
        && x25519_public_key
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b));
    if !is_did_key || !is_x25519 {
        return Err(RoomKeyError::Malformed);
    }
    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    struct RoomKey<'a> {
        x25519_public_key: &'a str,
        did: &'a str,
    }
    Ok(signing_bytes(
        ROOM_KEY_DOMAIN,
        &RoomKey {
            x25519_public_key,
            did,
        },
    )?)
}

/// True when `signature_hex` is the Ed25519 signature of `did`'s key over
/// [`room_key_signing_bytes`]`(did, x25519_public_key)`.
pub fn verify_room_key(did: &str, x25519_public_key: &str, signature_hex: &str) -> bool {
    let Ok(bytes) = room_key_signing_bytes(did, x25519_public_key) else {
        return false;
    };
    let Ok(signature) = hex::decode(signature_hex) else {
        return false;
    };
    did.strip_prefix("did:key:")
        .is_some_and(|key| verify_bytes(&bytes, key, &signature))
}
