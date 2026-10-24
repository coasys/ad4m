//! `ad4m-identity-core`: the `did:scid:coasys:1` identity core. The format spec
//! is `docs-src/identity/did-scid-coasys-1.md`; its rule ids (P1, L1, K1, …) are those of its
//! section 5.
//!
//! The key event log (KEL) is one chain type on `ad4m-hash-chain`: [`Event`] implements its
//! `Entry`, [`KelPolicy`] holds the spec's rules K1–K15, and [`fold`] gives a [`KelChain`]
//! whose `state()` is the [`KeyState`]. The core does no I/O and reads no clock (callers pass
//! `now`); it draws randomness only in `words::generate`. Features: `build` (event builders),
//! `words` (the 12 words and master-key derivation), `wasm` (wasm-bindgen exports). [`link`]
//! holds offers, claims and the match code of device linking.

use serde_json::{json, Value};

mod event;
mod keys;
mod policy;
mod state;

pub mod domain;
pub mod link;
pub mod pow;

#[cfg(feature = "build")]
pub mod build;
#[cfg(feature = "wasm")]
pub mod wasm;
#[cfg(feature = "words")]
pub mod words;

pub use ad4m_hash_chain::{
    Chain, ChainError, ChainPolicy, Entry, ForkEvidence, Hash, Head, ImportOutcome, Multikey, Proof,
};
pub use domain::{
    signing_bytes, verify_signed, DomainError, HeadStatement, HEAD_DOMAIN, LINK_DOMAIN,
    REQUEST_DOMAIN,
};
pub use event::{BotScope, DeviceKey, Event, KeyLife, Kind, Op, BOT_ACTIONS};
pub use keys::{
    decode_x25519_multikey, encode_x25519_multikey, is_ed25519_multikey, is_x25519_multikey,
};
pub use policy::{FoldError, FormatError, KelPolicy};
pub use state::{KeyRecord, KeyState};

/// The format identifier every event carries.
pub const FORMAT: &str = "coasys/1";
/// The prefix of every identity.
pub const DID_PREFIX: &str = "did:scid:coasys:1:";
/// The value of `scid` in event #0 while its SCID is computed (spec section 4).
pub const SCID_PLACEHOLDER: &str = "{SCID}";
/// The signing domain of log events.
pub const KEL_DOMAIN: &str = "coasys:kel";
/// The `proofPurpose` of every log proof.
pub const PROOF_PURPOSE: &str = "assertionMethod";

/// A key id: `m<n>` for master key *n*, `d<n>` for device keys.
pub type KeyId = String;

/// One log on the hash-chain module: `head()` and `hash_at()` give the head and the hash at each seq.
pub type KelChain = Chain<KelPolicy>;

/// `ad4m_hash_chain::fold::<KelPolicy>`: order and links in the module, rules K1–K15 in `KelPolicy`.
pub fn fold(events: &[Event]) -> Result<KelChain, ChainError<FoldError>> {
    ad4m_hash_chain::fold(&KelPolicy, events)
}

/// [`fold`] over events given as JSON: each one is parsed under the format-1 rules first, and a
/// parse refusal is `ChainError::Policy(FoldError)` (unknown op or field, float, big integer, …).
pub fn fold_json(events: &[Value]) -> Result<KelChain, ChainError<FoldError>> {
    fold(&parse_events(events)?)
}

/// The module's `import` with `KelPolicy`: sorts by `seq`, answers `Appended`, `AlreadyHave`,
/// `Gap` or `Fork`, and changes nothing unless it appended.
pub fn import(
    chain: &mut KelChain,
    events: &[Event],
) -> Result<ImportOutcome, ChainError<FoldError>> {
    ad4m_hash_chain::import(chain, &KelPolicy, events)
}

/// [`import`] over events given as JSON.
pub fn import_json(
    chain: &mut KelChain,
    events: &[Value],
) -> Result<ImportOutcome, ChainError<FoldError>> {
    import(chain, &parse_events(events)?)
}

fn parse_events(events: &[Value]) -> Result<Vec<Event>, ChainError<FoldError>> {
    events
        .iter()
        .map(|v| Event::from_json(v).map_err(ChainError::Policy))
        .collect()
}

/// The `ChainErrorCode` JSON of a fold or import error, as the wasm exports throw it. A KEL
/// refusal is `{"code": "policy", "reason": <FoldError name>, "detail": <message>}`, plus
/// `"format": <FormatError name>` for a `FoldError::Format`.
pub fn error_code(e: &ChainError<FoldError>) -> Value {
    match e {
        ChainError::Policy(fold) => {
            let mut code =
                json!({"code": "policy", "reason": fold.name(), "detail": fold.to_string()});
            if let FoldError::Format(format) = fold {
                code["format"] = Value::String(format.name().to_string());
            }
            code
        }
        other => other.code(),
    }
}

/// The event hash: `multihash_b58(sha256(JCS(event without proof)))`
/// (`ad4m_hash_chain::entry_hash::<Event>`). Fails only for an integer at or above 2^53.
pub fn event_hash(event: &Event) -> Result<Hash, FormatError> {
    ad4m_hash_chain::entry_hash(event).map_err(FormatError::from)
}

/// The SCID (spec section 4, rule K1): the hash of event #0 with `"scid": "{SCID}"` and no
/// proof. The `scid` and `proof` that `event0` holds are ignored.
pub fn derive_scid(event0: &Event) -> Result<String, FormatError> {
    let mut body = event0.clone();
    body.scid = SCID_PLACEHOLDER.to_string();
    body.proof.clear();
    event_hash(&body)
}

/// The identity of a SCID: `did:scid:coasys:1:<scid>`.
pub fn did_of(scid: &str) -> String {
    format!("{DID_PREFIX}{scid}")
}

/// The SCID of a `did:scid:coasys:1` identity, if `did` is one and its SCID is a sha2-256 multihash.
pub fn scid_of(did: &str) -> Option<&str> {
    let scid = did.strip_prefix(DID_PREFIX)?;
    ad4m_hash_chain::decode_hash(scid).ok()?;
    Some(scid)
}

/// The verification method of a key of an identity: `<did>#<keyId>`.
pub fn verification_method(did: &str, key_id: &str) -> String {
    format!("{did}#{key_id}")
}

/// The next-key commitment (spec section 4):
/// `multihash_b58(sha256(JCS({"key": key, "salt": salt})))`, where `salt` is base58btc (no
/// multibase prefix) of the 16-byte salt.
pub fn commitment(key: &str, salt: &str) -> Hash {
    // An object of two strings holds no number, so it always has a canonical form.
    let text = ad4m_hash_chain::canonical_json(&json!({ "key": key, "salt": salt }))
        .expect("an object of strings has a canonical form");
    ad4m_hash_chain::hash_bytes(text.as_bytes())
}
