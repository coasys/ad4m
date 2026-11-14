//! The registry of signing domains and of the labels that are not signing domains (sections 7
//! and 16 of `docs-src/identity/did-scid-coasys-1.md`).
//!
//! Every signature names its domain, and a verifier states the domain it expects, so a
//! signature made for one purpose never verifies for another. Log events use Data Integrity
//! proofs with a `domain` property (`coasys:kel`); every other domain uses the domain-field
//! scheme of [`signing_bytes`]. A label of the second table enters a hash or a key derivation
//! and is never a signing domain: every signing API of the core refuses it.

use ad4m_hash_chain::{canonical_json, canonical_json_exact, decode_ed25519_multikey, CanonError};
use ed25519_dalek::{Signature, VerifyingKey};
use serde::Serialize;
use serde_json::Value;
use zeroize::{Zeroize, Zeroizing};

/// How a domain's signatures are made.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scheme {
    /// A Data Integrity proof (`eddsa-jcs-2022`) with a `domain` property: log events.
    DataIntegrity,
    /// `JCS(value + {"domain": domain})` signed by an Ed25519 key.
    DomainField,
}

/// One row of the signature table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Domain {
    pub name: &'static str,
    pub signs: &'static str,
    /// The part of the system whose messages the domain signs.
    pub owner: &'static str,
    pub scheme: Scheme,
}

/// The signature table (section 16 of `docs-src/identity/did-scid-coasys-1.md`). A part of the
/// system that needs a new domain adds a row there and here.
pub const SIGNING_DOMAINS: &[Domain] = &[
    Domain {
        name: "coasys:kel",
        signs: "log events",
        owner: "identity",
        scheme: Scheme::DataIntegrity,
    },
    Domain {
        name: "coasys:link",
        signs: "links whose author key sits in a log",
        owner: "identity",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:agent-record",
        signs: "agent records (`{profile, version, anchor}`) at the store's `/v1/agents/{did}`",
        owner: "identity",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:store-request",
        signs: "signed requests to the shared store",
        owner: "identity",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:head",
        signs: "the store's signed heads",
        owner: "identity",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:vouch",
        signs: "vouching notes",
        owner: "identity",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:offer",
        signs: "offers",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:claim",
        signs: "claim co-signatures",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:pause",
        signs: "bot pause records",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:inbox",
        signs: "device inbox messages",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:private-record",
        signs: "private record envelopes, by a device key or the current master",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:recovery-key",
        signs: "the private record's recovery key, by the current master",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:record-receipt",
        signs: "the store's receipt for a private record write it accepted, by its head key",
        owner: "device linking",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:session",
        signs: "key statements for a login on an executor, and the executor's sign-in to the hosting index",
        owner: "executor hosting",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:account",
        signs: "signed requests to the wrapped-key store",
        owner: "wrapped-key store",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:room-key",
        signs: "the server link language's signature over its X25519 public key",
        owner: "server link language",
        scheme: Scheme::DomainField,
    },
    Domain {
        name: "coasys:bot-grant",
        signs: "a bot grant, signed by the owner's current master key",
        owner: "executor hosting",
        scheme: Scheme::DomainField,
    },
];

/// The labels: strings that enter a hash or a key derivation. No key signs
/// them as a domain. (`coasys:offer` is also used as a hash prefix and `coasys:inbox` as the
/// device inbox's HPKE `info` label, but both are signing domains first, so they sit in
/// [`SIGNING_DOMAINS`]. `coasys:record` is the `info` label of a private record's wrapped key.)
pub const LABELS: &[&str] = &[
    "coasys:pow",
    "coasys:pair-id:1",
    "coasys:pair-key:1",
    "coasys:match",
    "coasys:wrap:1",
    "coasys:auth:1",
    "coasys:record",
];

/// Why a domain cannot serve a signature.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum DomainError {
    #[error("{0:?} is a label, not a signing domain")]
    Label(String),
    #[error("{0:?} is not in the signature table")]
    Unknown(String),
    #[error("{0:?} signs with another scheme")]
    Scheme(String),
    #[error("the signed value must be a JSON object")]
    NotObject,
    #[error("the signed value already holds a `domain` member")]
    DomainMember,
    #[error("no canonical form: {0}")]
    Canon(#[from] CanonError),
}

/// The registry row of `name`, if it is a signing domain.
pub fn domain(name: &str) -> Option<&'static Domain> {
    SIGNING_DOMAINS.iter().find(|d| d.name == name)
}

/// Checks that `name` is a signing domain of `scheme`. Every signing API of the core calls it.
pub fn check_domain(name: &str, scheme: Scheme) -> Result<(), DomainError> {
    if LABELS.contains(&name) {
        return Err(DomainError::Label(name.to_string()));
    }
    match domain(name) {
        None => Err(DomainError::Unknown(name.to_string())),
        Some(d) if d.scheme != scheme => Err(DomainError::Scheme(name.to_string())),
        Some(_) => Ok(()),
    }
}

/// Bytes that an Ed25519 key signs for `value` under `domain` (the domain-field scheme).
/// The domain enters the object before canonicalisation, so it is covered by the signature:
/// `JCS(value + {"domain": domain})`. Refuses labels, unknown domains, `coasys:kel` (log events
/// sign with Data Integrity proofs), values that are not objects and values that already
/// hold a `domain` member.
pub fn signing_bytes<T: Serialize>(domain: &str, value: &T) -> Result<Vec<u8>, DomainError> {
    check_domain(domain, Scheme::DomainField)?;
    let mut value = serde_json::to_value(value).map_err(|e| CanonError::Serde(e.to_string()))?;
    add_domain(domain, &mut value)?;
    Ok(canonical_json(&value)?.into_bytes())
}

/// `value` with the member `domain`; refused for a value that is no object or holds one.
fn add_domain(domain: &str, value: &mut Value) -> Result<(), DomainError> {
    let map = value.as_object_mut().ok_or(DomainError::NotObject)?;
    if map.contains_key("domain") {
        return Err(DomainError::DomainMember);
    }
    map.insert("domain".into(), Value::String(domain.to_string()));
    Ok(())
}

/// A JSON value that holds a secret (an inbox `master` body): every string in it is wiped when
/// it drops. Build the value inside it (`WipedJson(json!({..}))`) or with [`WipedJson::of`], so
/// that no unwiped JSON copy of the secret is left.
pub(crate) struct WipedJson(pub(crate) Value);

impl WipedJson {
    /// `value` as JSON.
    pub(crate) fn of<T: Serialize>(value: &T) -> Result<WipedJson, CanonError> {
        Ok(WipedJson(
            serde_json::to_value(value).map_err(|e| CanonError::Serde(e.to_string()))?,
        ))
    }
}

impl Drop for WipedJson {
    fn drop(&mut self) {
        wipe_strings(&mut self.0);
    }
}

/// Zeroes every string of `value`, whole capacity, in place.
pub(crate) fn wipe_strings(value: &mut Value) {
    match value {
        Value::String(s) => s.zeroize(),
        Value::Array(items) => items.iter_mut().for_each(wipe_strings),
        Value::Object(map) => map.values_mut().for_each(wipe_strings),
        _ => {}
    }
}

/// [`signing_bytes`] of a value that holds a secret. The JSON copy is wiped (it is a
/// [`WipedJson`]), and the text is written once at its exact length, so no shorter copy of it
/// is left in freed memory, and wiped when the bytes drop.
pub(crate) fn secret_signing_bytes(
    domain: &str,
    mut value: WipedJson,
) -> Result<Zeroizing<Vec<u8>>, DomainError> {
    check_domain(domain, Scheme::DomainField)?;
    add_domain(domain, &mut value.0)?;
    Ok(Zeroizing::new(canonical_json_exact(&value.0)?.into_bytes()))
}

/// True when `signature` is `key`'s Ed25519 signature (strict) of [`signing_bytes`]`(domain, value)`.
/// False for every domain [`signing_bytes`] refuses.
pub fn verify_signed<T: Serialize>(domain: &str, value: &T, key: &str, signature: &[u8]) -> bool {
    signing_bytes(domain, value).is_ok_and(|bytes| verify_bytes(&bytes, key, signature))
}

/// [`verify_signed`] of a value that holds a secret, its copies wiped as
/// [`secret_signing_bytes`] wipes them.
pub(crate) fn verify_secret_signed(
    domain: &str,
    value: WipedJson,
    key: &str,
    signature: &[u8],
) -> bool {
    secret_signing_bytes(domain, value).is_ok_and(|bytes| verify_bytes(&bytes, key, signature))
}

/// True when `signature` is `key`'s Ed25519 signature (strict) of `bytes`.
pub(crate) fn verify_bytes(bytes: &[u8], key: &str, signature: &[u8]) -> bool {
    let Some(key) = decode_ed25519_multikey(key) else {
        return false;
    };
    let Ok(key) = VerifyingKey::from_bytes(&key) else {
        return false;
    };
    let Ok(signature) = Signature::from_slice(signature) else {
        return false;
    };
    key.verify_strict(bytes, &signature).is_ok()
}

/// The signing domain of links whose author key sits in a log.
pub const LINK_DOMAIN: &str = "coasys:link";
/// The signing domain of agent records: `{profile, version, anchor}` by the record's
/// `signedBy`. Its own domain, so a record signature never verifies as a link and the reverse,
/// whatever the two JSON shapes hold.
pub const AGENT_RECORD_DOMAIN: &str = "coasys:agent-record";
/// The signing domain of the store's signed heads.
pub const HEAD_DOMAIN: &str = "coasys:head";
/// The signing domain of signed requests to the store.
pub const REQUEST_DOMAIN: &str = "coasys:store-request";
/// The signing domain of key statements: a device key's login or renewal on an executor, and an
/// executor's sign-in to the hosting index.
pub const SESSION_DOMAIN: &str = "coasys:session";

/// What a store signs for a log's head under [`HEAD_DOMAIN`]: `store` is the
/// store key's `did:key`. The store signs it; executors check it against `identity.storeKeys`.
#[derive(Clone, Copy, Debug, Serialize)]
pub struct HeadStatement<'a> {
    pub scid: &'a str,
    pub seq: u64,
    pub hash: &'a str,
    pub time: u64,
    pub store: &'a str,
}

impl HeadStatement<'_> {
    /// The bytes the store key signs: [`signing_bytes`] under [`HEAD_DOMAIN`].
    pub fn signing_bytes(&self) -> Result<Vec<u8>, DomainError> {
        signing_bytes(HEAD_DOMAIN, self)
    }

    /// True when `sig` (a proof value) is the signature of the `store` key over the statement
    /// and that key is one of `trusted` (multikeys).
    pub fn verify(&self, sig: &str, trusted: &[String]) -> bool {
        let Some(key) = self.store.strip_prefix("did:key:") else {
            return false;
        };
        if !trusted.iter().any(|t| t == key) {
            return false;
        }
        let Some(sig) = ad4m_hash_chain::decode_proof_value(sig) else {
            return false;
        };
        verify_signed(HEAD_DOMAIN, self, key, &sig)
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn a_secret_value_signs_as_signing_bytes_and_its_strings_are_wiped() {
        let value = json!({ "body": { "secret": "4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi" }, "n": [1, "x"] });
        let bytes = secret_signing_bytes("coasys:inbox", WipedJson(value.clone())).unwrap();
        assert_eq!(*bytes, signing_bytes("coasys:inbox", &value).unwrap());
        assert!(matches!(
            secret_signing_bytes("coasys:kel", WipedJson(value.clone())),
            Err(DomainError::Scheme(_))
        ));

        let mut wiped = value;
        wipe_strings(&mut wiped);
        assert_eq!(wiped, json!({ "body": { "secret": "" }, "n": [1, ""] }));
    }
}
