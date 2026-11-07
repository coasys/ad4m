//! The private details record: device names, kinds, addresses, which devices hold the master
//! key, and pending revocations, kept beside the log and never in it (spec section 17).
//!
//! A [`PrivateRecord`] is encrypted with XChaCha20-Poly1305 under a fresh 32-byte record key
//! for every version; the record key is sealed with HPKE to the `enc` key of every valid device
//! key of the log without the bot mark, and to the recovery key the words derive, when the
//! record has one ([`seal_record`]). A writer reads the `enc` keys from the log, so it wraps to
//! every device even with no copy of the old record, and a revoked or removed device cannot
//! read a later version.
//!
//! The writer signs the whole envelope ([`envelope_signing_bytes`], domain
//! `coasys:private-record`) with its device key, or with the current master key for a
//! master-signed write; the envelope names the signer's key id. A reader ([`open_record`]) and
//! the store ([`check_sealed`]) verify that signature against the identity's log before they
//! trust any metadata or recipient key: anyone can seal to the log's public keys, only the
//! identity's keys can sign. The store keeps only the [`SealedRecord`] and checks its shape, its
//! signature and that it is wrapped to exactly the log's readers.
//!
//! Every version is a whole envelope, so a store can serve back an older one: a reader keeps the
//! highest version it has read per identity and refuses a lower one.
//!
//! Pure functions: no I/O, no clock, and the caller's RNG for the record key and the nonce.

use std::collections::BTreeMap;

use ad4m_hash_chain::{canonical_json, decode_proof_value, Multikey};
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
#[cfg(feature = "build")]
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{json, Value};
#[cfg(feature = "build")]
use zeroize::Zeroizing;

use crate::domain::{signing_bytes, verify_signed};
use crate::keys::{is_x25519_multikey, X25519Secret};
use crate::seal::{self, b64, is_base64url};
use crate::{KelChain, KeyId, KeyState};

/// The `info` label of a wrapped record key: `"coasys:record" ‖ 0x00 ‖ scid`.
pub const RECORD_LABEL: &str = "coasys:record";
/// The signing domain of a record envelope.
pub const RECORD_DOMAIN: &str = "coasys:private-record";
/// The entry of `wrapped` for the recovery key.
pub const RECOVERY_ENTRY: &str = "recovery";
/// Bytes of the record key.
pub const RECORD_KEY_LEN: usize = 32;
/// Bytes of the XChaCha20-Poly1305 nonce.
pub const NONCE_LEN: usize = 24;

/// What the store keeps: the record encrypted, and its key wrapped to each reader.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SealedRecord {
    /// Rises by one with each write; the store refuses any other than the stored version plus
    /// one.
    pub version: u64,
    /// Key id (`"d3"`) or `"recovery"` → the record key sealed to that key.
    pub wrapped: BTreeMap<String, String>,
    /// The X25519 public key the words derive (`words::recovery_secret`). Every later writer
    /// copies it unchanged; `None` only for a record whose person skipped setting it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_key: Option<Multikey>,
    /// 24 bytes, base58btc.
    pub nonce: String,
    /// base64url of XChaCha20-Poly1305(recordKey, nonce, JCS(PrivateRecord), aad = [`record_aad`]).
    pub ciphertext: String,
    /// The writer's key id: a device key without the bot mark valid at the head (`"d3"`), or
    /// the current master (`"m0"`).
    pub signer: KeyId,
    /// That key over [`envelope_signing_bytes`], base58btc (`z…`).
    pub sig: String,
}

/// Who wrote a record whose signature verified ([`verify_envelope`]).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Writer {
    /// A device key without the bot mark, valid at the head.
    Device(KeyId),
    /// The log's current master key.
    Master(KeyId),
}

/// The record's plaintext. It holds no person setting: settings stay per executor.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PrivateRecord {
    pub devices: BTreeMap<KeyId, DeviceEntry>,
    /// When the person last confirmed the 12 words.
    #[serde(default)]
    pub words_checked_at: Option<u64>,
    /// A revocation that no reachable master key could sign yet.
    #[serde(default)]
    pub pending_revocations: Vec<PendingRevocation>,
    /// Reserved; empty.
    #[serde(default)]
    pub safeguards: serde_json::Map<String, serde_json::Value>,
}

/// One device of the record.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct DeviceEntry {
    /// "MacBook Pro": from the claim's hint, editable.
    pub name: String,
    pub kind: DeviceKind,
    /// For an executor: where it runs.
    #[serde(default)]
    pub place: Option<Place>,
    /// `wss://` or `https://` of an executor that has one.
    #[serde(default)]
    pub address: Option<String>,
    pub runs_executor: bool,
    pub holds_master: HoldsMaster,
    /// For an executor: who operates it, as a bare did:scid, when known.
    #[serde(default)]
    pub operator: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DeviceKind {
    DesktopApp,
    PhoneApp,
    Browser,
    Server,
    CommunityHost,
    CoasysCloud,
    Bot,
}

/// Where an executor runs; the same words as [`DeviceKind`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Place {
    Computer,
    Server,
    CommunityHost,
    CoasysCloud,
}

/// Whether a device holds the master key. Wire: `false`, `"offered"`, `true`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HoldsMaster {
    No,
    /// A master key waits sealed in the device's inbox.
    Offered,
    Yes,
}

impl Serialize for HoldsMaster {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            HoldsMaster::No => s.serialize_bool(false),
            HoldsMaster::Offered => s.serialize_str("offered"),
            HoldsMaster::Yes => s.serialize_bool(true),
        }
    }
}

impl<'de> Deserialize<'de> for HoldsMaster {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Wire {
            Flag(bool),
            Word(String),
        }
        match Wire::deserialize(d)? {
            Wire::Flag(false) => Ok(HoldsMaster::No),
            Wire::Flag(true) => Ok(HoldsMaster::Yes),
            Wire::Word(w) if w == "offered" => Ok(HoldsMaster::Offered),
            Wire::Word(w) => Err(serde::de::Error::custom(format!(
                "holdsMaster is false, \"offered\" or true, not {w:?}"
            ))),
        }
    }
}

/// A revocation waiting for an app with the master key.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PendingRevocation {
    pub key_id: KeyId,
    pub reason: RevocationReason,
    /// Unix seconds.
    pub at: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RevocationReason {
    /// The person left a community host from an app without the master key.
    LeftHost,
    /// The account was deleted while no master key was reachable.
    AccountDeleted,
}

/// Why a record cannot be sealed, opened or stored.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RecordError {
    #[error("malformed: {0}")]
    Malformed(String),
    #[error("the record does not open with this key")]
    Open,
    #[error("no wrapped record key for {0}")]
    NoEntry(String),
    #[error("the record is not wrapped to exactly the log's device keys: {0}")]
    Wrapped(String),
    #[error(
        "{0} is neither a device key without the bot mark valid at the head nor the current master"
    )]
    Signer(String),
    #[error("the record's signature does not verify")]
    Signature,
}

impl RecordError {
    /// A short name for error bodies.
    pub fn name(&self) -> &'static str {
        match self {
            RecordError::Malformed(_) => "malformed",
            RecordError::Open => "open",
            RecordError::NoEntry(_) => "not-found",
            RecordError::Wrapped(_) => "wrapped",
            RecordError::Signer(_) => "signer",
            RecordError::Signature => "signature",
        }
    }
}

fn malformed(what: impl Into<String>) -> RecordError {
    RecordError::Malformed(what.into())
}

/// The record ciphertext's AAD: `JCS({"scid": scid, "version": version, "recoveryKey":
/// recoveryKey or null})`, so a ciphertext of one identity or version, or under another recovery
/// key, does not pass as another. A version above 2^53 − 1 is malformed.
pub fn record_aad(
    scid: &str,
    version: u64,
    recovery_key: Option<&str>,
) -> Result<Vec<u8>, RecordError> {
    canonical_json(&json!({ "scid": scid, "version": version, "recoveryKey": recovery_key }))
        .map(String::into_bytes)
        .map_err(|_| malformed("version is not a safe integer"))
}

/// The value the writer signs for `sealed` of identity `scid`: every field of the envelope but
/// `sig`, an absent one as `null`.
fn envelope_value(scid: &str, sealed: &SealedRecord) -> Value {
    json!({
        "scid": scid,
        "version": sealed.version,
        "wrapped": sealed.wrapped,
        "recoveryKey": sealed.recovery_key,
        "nonce": sealed.nonce,
        "ciphertext": sealed.ciphertext,
        "signer": sealed.signer,
    })
}

/// The bytes the writer signs: `signing_bytes("coasys:private-record", {scid, version, wrapped,
/// recoveryKey, nonce, ciphertext, signer})`, an absent `recoveryKey` as `null`.
pub fn envelope_signing_bytes(scid: &str, sealed: &SealedRecord) -> Result<Vec<u8>, RecordError> {
    signing_bytes(RECORD_DOMAIN, &envelope_value(scid, sealed))
        .map_err(|e| malformed(e.to_string()))
}

/// The writer of `sealed` when its signature verifies against `chain` at its head: `signer`
/// must name a device key without the bot mark valid at the head, or the current master, and
/// `sig` must be that key over [`envelope_signing_bytes`]. A key revoked, removed or replaced
/// before the head no longer counts, nor does a bot key.
pub fn verify_envelope(sealed: &SealedRecord, chain: &KelChain) -> Result<Writer, RecordError> {
    if sealed.version > ad4m_hash_chain::MAX_SAFE_INTEGER {
        return Err(malformed("version is not a safe integer"));
    }
    let (writer, key) = writer_of(&sealed.signer, chain)?;
    let sig = decode_proof_value(&sealed.sig).ok_or(RecordError::Signature)?;
    if !verify_signed(
        RECORD_DOMAIN,
        &envelope_value(chain.id(), sealed),
        key,
        &sig,
    ) {
        return Err(RecordError::Signature);
    }
    Ok(writer)
}

/// The writer `signer` names and its signing key, when the log at its head lets it write: the
/// current master, or a device key without the bot mark valid at the head.
fn writer_of<'a>(signer: &str, chain: &'a KelChain) -> Result<(Writer, &'a Multikey), RecordError> {
    let state = chain.state();
    if let Some(m) = state.current_master().filter(|m| m.id == signer) {
        return Ok((Writer::Master(m.id.clone()), &m.sign));
    }
    match state.keys.get(signer) {
        Some(r) if r.scopes.is_empty() && state.key_valid_at(&r.id, chain.head().seq) => {
            Ok((Writer::Device(r.id.clone()), &r.sign))
        }
        _ => Err(RecordError::Signer(signer.to_string())),
    }
}

/// The `info` of a wrapped record key: `"coasys:record" ‖ 0x00 ‖ scid`.
pub fn wrap_info(scid: &str) -> Vec<u8> {
    seal::info(RECORD_LABEL, scid)
}

/// The keys a record of `state` is wrapped to: every device key valid at `head_seq` without the
/// bot mark, by key id with its `enc`, and `"recovery"` with `recovery_key` when there is one.
/// A bot has no inbox and reads no record.
pub fn readers(
    state: &KeyState,
    head_seq: u64,
    recovery_key: Option<&Multikey>,
) -> BTreeMap<String, Multikey> {
    let mut out: BTreeMap<String, Multikey> = state
        .keys
        .values()
        .filter(|r| r.scopes.is_empty() && state.key_valid_at(&r.id, head_seq))
        .filter_map(|r| r.enc.clone().map(|enc| (r.id.clone(), enc)))
        .collect();
    if let Some(key) = recovery_key {
        out.insert(RECOVERY_ENTRY.to_string(), key.clone());
    }
    out
}

/// Encrypts `record` as `version` under a fresh record key, wraps that key to every reader of
/// the log at its head ([`readers`]), and signs the envelope with `signer`: a device key without
/// the bot mark valid at the head, or the current master.
#[cfg(feature = "build")]
pub fn seal_record<R: CryptoRng + RngCore>(
    chain: &KelChain,
    version: u64,
    record: &PrivateRecord,
    recovery_key: Option<Multikey>,
    signer: &dyn crate::build::Signer,
    rng: &mut R,
) -> Result<SealedRecord, RecordError> {
    use base64::Engine;
    let scid = chain.id();
    if recovery_key
        .as_deref()
        .is_some_and(|k| !is_x25519_multikey(k))
    {
        return Err(malformed("recoveryKey is an X25519 multikey"));
    }
    let aad = record_aad(scid, version, recovery_key.as_deref())?;
    let state = chain.state();
    let signer_key = signer.key();
    let signer_id = state
        .key_id_of(&signer_key)
        .cloned()
        .ok_or_else(|| RecordError::Signer(signer_key.to_string()))?;
    let value = serde_json::to_value(record).map_err(|e| malformed(e.to_string()))?;
    let text = canonical_json(&value).map_err(|e| malformed(e.to_string()))?;
    let mut key = Zeroizing::new([0u8; RECORD_KEY_LEN]);
    rng.fill_bytes(key.as_mut());
    let mut nonce = [0u8; NONCE_LEN];
    rng.fill_bytes(&mut nonce);
    let cipher = XChaCha20Poly1305::new(key.as_ref().into());
    let ciphertext = cipher
        .encrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: text.as_bytes(),
                aad: &aad,
            },
        )
        .map_err(|_| malformed("the record does not encrypt"))?;
    let info = wrap_info(scid);
    let mut wrapped = BTreeMap::new();
    for (entry, enc) in readers(state, chain.head().seq, recovery_key.as_ref()) {
        let sealed = seal::seal(&enc, &info, key.as_ref(), rng)
            .map_err(|_| malformed(format!("the enc key of {entry} is not X25519")))?;
        wrapped.insert(entry, sealed);
    }
    let mut sealed = SealedRecord {
        version,
        wrapped,
        recovery_key,
        nonce: bs58::encode(nonce).into_string(),
        ciphertext: b64().encode(ciphertext),
        signer: signer_id,
        sig: String::new(),
    };
    writer_of(&sealed.signer, chain)?;
    let bytes = envelope_signing_bytes(scid, &sealed)?;
    sealed.sig = ad4m_hash_chain::encode_proof_value(&signer.sign(&bytes));
    Ok(sealed)
}

/// Opens `sealed` of the identity whose log is `chain` through its `wrapped[entry]` (a key id,
/// or `"recovery"`) with that reader's X25519 `secret`. The envelope's signature is checked
/// against the log first ([`verify_envelope`]): a record no key of the identity signed is
/// refused before any of its fields is used.
///
/// It does not know which versions the caller read before: a store can serve back an older
/// envelope whole, so a reader keeps the highest version it has read per identity and refuses
/// a lower one.
pub fn open_record(
    sealed: &SealedRecord,
    chain: &KelChain,
    entry: &str,
    secret: &X25519Secret,
) -> Result<PrivateRecord, RecordError> {
    use base64::Engine;
    verify_envelope(sealed, chain)?;
    let scid = chain.id();
    let wrapped = sealed
        .wrapped
        .get(entry)
        .ok_or_else(|| RecordError::NoEntry(entry.to_string()))?;
    let key = seal::open(secret, &wrap_info(scid), wrapped).map_err(|_| RecordError::Open)?;
    if key.len() != RECORD_KEY_LEN {
        return Err(RecordError::Open);
    }
    let aad = record_aad(scid, sealed.version, sealed.recovery_key.as_deref())?;
    let nonce = decode_nonce(&sealed.nonce)?;
    let ciphertext = b64()
        .decode(&sealed.ciphertext)
        .map_err(|_| malformed("ciphertext is base64url"))?;
    let plain = XChaCha20Poly1305::new(key.as_slice().into())
        .decrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: &ciphertext,
                aad: &aad,
            },
        )
        .map_err(|_| RecordError::Open)?;
    serde_json::from_slice(&plain).map_err(|e| malformed(e.to_string()))
}

fn decode_nonce(text: &str) -> Result<[u8; NONCE_LEN], RecordError> {
    // 24 bytes take at most ceil(24 * 1.3657) = 33 base58 characters.
    if text.len() > 33 {
        return Err(malformed("nonce is 24 bytes of base58btc"));
    }
    bs58::decode(text)
        .into_vec()
        .ok()
        .and_then(|b| b.try_into().ok())
        .ok_or_else(|| malformed("nonce is 24 bytes of base58btc"))
}

/// The store's check of a record it is asked to keep for the log `chain`: well-formed, signed
/// by a writer the log lets write ([`verify_envelope`]), and wrapped to exactly [`readers`] of
/// the log at its head (a key that left the log, a bot key or a missing device is refused),
/// with `wrapped["recovery"]` exactly when `recoveryKey` is set. The store cannot open it, so
/// this is all it can check of the wraps. Answers the writer.
pub fn check_sealed(sealed: &SealedRecord, chain: &KelChain) -> Result<Writer, RecordError> {
    if sealed.version > ad4m_hash_chain::MAX_SAFE_INTEGER {
        return Err(malformed("version is not a safe integer"));
    }
    if sealed
        .recovery_key
        .as_deref()
        .is_some_and(|k| !is_x25519_multikey(k))
    {
        return Err(malformed("recoveryKey is an X25519 multikey"));
    }
    decode_nonce(&sealed.nonce)?;
    if !is_base64url(&sealed.ciphertext) {
        return Err(malformed("ciphertext is base64url"));
    }
    if let Some((entry, _)) = sealed.wrapped.iter().find(|(_, w)| !is_base64url(w)) {
        return Err(malformed(format!("wrapped[{entry}] is base64url")));
    }
    let writer = verify_envelope(sealed, chain)?;
    let want = readers(
        chain.state(),
        chain.head().seq,
        sealed.recovery_key.as_ref(),
    );
    let have: Vec<&String> = sealed.wrapped.keys().collect();
    let want: Vec<&String> = want.keys().collect();
    if have != want {
        return Err(RecordError::Wrapped(format!(
            "wrapped to {have:?}, the log's readers are {want:?}"
        )));
    }
    Ok(writer)
}
