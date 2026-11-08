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
//! A recovery key counts only with an attestation by the log's current master
//! ([`RecoveryAttestation`], domain `coasys:recovery-key`): only the master key's holder installs
//! or replaces it, a device writer copies it with its attestation ([`recovery_of`]), and the
//! store refuses a write whose recovery key the current master has not attested.
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
/// The signing domain of a recovery key's attestation by the current master.
pub const RECOVERY_DOMAIN: &str = "coasys:recovery-key";
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
    /// The X25519 public key the words derive (`words::recovery_secret`); `None` for a record
    /// without one. It counts only with `recovery_attestation` by the current master
    /// ([`recovery_of`]).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_key: Option<Multikey>,
    /// The current master's attestation of `recovery_key`; present exactly when it is.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_attestation: Option<RecoveryAttestation>,
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

/// A master key's word that `recoveryKey` is the identity's recovery key: `sig` is master
/// `keyId` over `signing_bytes("coasys:recovery-key", {scid, recoveryKey, keyId})`. Only the
/// holder of the master key (who holds the words the recovery key comes from) can install or
/// replace a recovery key; a device writer copies the attestation unchanged. It counts while
/// `keyId` is the log's current master: after a rotation the new master attests again, and
/// until it does a device writer drops the recovery key ([`recovery_of`]).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct RecoveryAttestation {
    /// The master key's id (`"m0"`).
    pub key_id: KeyId,
    /// base58btc (`z…`).
    pub sig: String,
}

/// A recovery key with its attestation, as a writer passes it to [`seal_record`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Recovery {
    pub key: Multikey,
    pub attestation: RecoveryAttestation,
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
    #[error("the recovery key is not attested by the current master: {0}")]
    Attestation(String),
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
            RecordError::Attestation(_) => "attestation",
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
        "recoveryAttestation": sealed.recovery_attestation.as_ref().map(|a| json!({
            "keyId": a.key_id,
            "sig": a.sig,
        })),
        "nonce": sealed.nonce,
        "ciphertext": sealed.ciphertext,
        "signer": sealed.signer,
    })
}

/// The bytes the writer signs: `signing_bytes("coasys:private-record", {scid, version, wrapped,
/// recoveryKey, recoveryAttestation, nonce, ciphertext, signer})`, an absent `recoveryKey` or
/// `recoveryAttestation` as `null`.
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
    if sealed.recovery_key.is_some() != sealed.recovery_attestation.is_some() {
        return Err(malformed(
            "recoveryAttestation is present exactly when recoveryKey is",
        ));
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

fn attestation_value(scid: &str, key: &str, key_id: &str) -> Value {
    json!({ "scid": scid, "recoveryKey": key, "keyId": key_id })
}

/// The bytes a master signs to attest `key` as the recovery key of `scid`:
/// `signing_bytes("coasys:recovery-key", {scid, recoveryKey, keyId})`.
pub fn attestation_signing_bytes(
    scid: &str,
    key: &str,
    key_id: &str,
) -> Result<Vec<u8>, RecordError> {
    signing_bytes(RECOVERY_DOMAIN, &attestation_value(scid, key, key_id))
        .map_err(|e| malformed(e.to_string()))
}

/// Checks that `attestation` is the log's current master over `key` for the identity of
/// `chain`. An attestation by a master a rotation replaced no longer counts.
pub fn verify_attestation(
    key: &str,
    attestation: &RecoveryAttestation,
    chain: &KelChain,
) -> Result<(), RecordError> {
    let master = chain
        .state()
        .current_master()
        .filter(|m| m.id == attestation.key_id)
        .ok_or_else(|| {
            RecordError::Attestation(format!(
                "{} is not the current master key",
                attestation.key_id
            ))
        })?;
    attestation_by(key, attestation, chain.id(), &master.sign)
}

fn attestation_by(
    key: &str,
    attestation: &RecoveryAttestation,
    scid: &str,
    master: &Multikey,
) -> Result<(), RecordError> {
    let sig = decode_proof_value(&attestation.sig)
        .ok_or_else(|| RecordError::Attestation("sig is not base58btc".into()))?;
    if verify_signed(
        RECOVERY_DOMAIN,
        &attestation_value(scid, key, &attestation.key_id),
        master,
        &sig,
    ) {
        Ok(())
    } else {
        Err(RecordError::Attestation(format!(
            "{} did not sign this recovery key",
            attestation.key_id
        )))
    }
}

/// The envelope's attestation, if any, checked against the master it names, current or
/// replaced: what a reader refuses outright is a recovery key no master of the identity ever
/// attested. Whether it still counts is [`recovery_of`].
fn check_attested_by_some_master(
    sealed: &SealedRecord,
    chain: &KelChain,
) -> Result<(), RecordError> {
    let (Some(key), Some(attestation)) = (&sealed.recovery_key, &sealed.recovery_attestation)
    else {
        return Ok(());
    };
    let master = chain
        .state()
        .masters
        .iter()
        .find(|m| m.id == attestation.key_id)
        .ok_or_else(|| {
            RecordError::Attestation(format!("{} is not a master key", attestation.key_id))
        })?;
    attestation_by(key, attestation, chain.id(), &master.sign)
}

/// The recovery key of `sealed` that counts: present only with a valid attestation by the log's
/// current master. A reader trusts only this, and a device writer passes it unchanged to
/// [`seal_record`]; `None` after a rotation until the new master attests again.
pub fn recovery_of(sealed: &SealedRecord, chain: &KelChain) -> Option<Recovery> {
    let key = sealed.recovery_key.clone()?;
    let attestation = sealed.recovery_attestation.clone()?;
    verify_attestation(&key, &attestation, chain).ok()?;
    Some(Recovery { key, attestation })
}

/// The current master `master` attests `key` as the recovery key of the identity of `chain`. A
/// master-signed write installs or replaces the recovery key with this.
#[cfg(feature = "build")]
pub fn attest_recovery_key(
    chain: &KelChain,
    key: Multikey,
    master: &dyn crate::build::Signer,
) -> Result<Recovery, RecordError> {
    if !is_x25519_multikey(&key) {
        return Err(malformed("recoveryKey is an X25519 multikey"));
    }
    let signer = master.key();
    let key_id = chain
        .state()
        .current_master()
        .filter(|m| m.sign == signer)
        .map(|m| m.id.clone())
        .ok_or_else(|| {
            RecordError::Attestation(format!("{signer} is not the current master key"))
        })?;
    let bytes = attestation_signing_bytes(chain.id(), &key, &key_id)?;
    let attestation = RecoveryAttestation {
        key_id,
        sig: ad4m_hash_chain::encode_proof_value(&master.sign(&bytes)),
    };
    Ok(Recovery { key, attestation })
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
/// the bot mark valid at the head, or the current master. `recovery` must carry the current
/// master's attestation: a device writer passes [`recovery_of`] of the record it read, a
/// master writer installs or replaces it with [`attest_recovery_key`].
#[cfg(feature = "build")]
pub fn seal_record<R: CryptoRng + RngCore>(
    chain: &KelChain,
    version: u64,
    record: &PrivateRecord,
    recovery: Option<Recovery>,
    signer: &dyn crate::build::Signer,
    rng: &mut R,
) -> Result<SealedRecord, RecordError> {
    let signer_key = signer.key();
    let signer_id = chain
        .state()
        .key_id_of(&signer_key)
        .cloned()
        .ok_or_else(|| RecordError::Signer(signer_key.to_string()))?;
    let mut sealed = seal_unsigned(chain, version, record, recovery, signer_id, rng)?;
    sign_envelope(chain, &mut sealed, signer)?;
    Ok(sealed)
}

/// [`seal_record`] without the signature: the envelope names `signer` (a device key without
/// the bot mark valid at the head, or the current master) and leaves `sig` empty, for a writer
/// whose key signs elsewhere: an app whose device key is in its executor's wallet passes the
/// envelope to `identity.recordPut`, which signs it there ([`sign_envelope`]).
#[cfg(feature = "build")]
pub fn seal_unsigned<R: CryptoRng + RngCore>(
    chain: &KelChain,
    version: u64,
    record: &PrivateRecord,
    recovery: Option<Recovery>,
    signer: KeyId,
    rng: &mut R,
) -> Result<SealedRecord, RecordError> {
    use base64::Engine;
    let scid = chain.id();
    writer_of(&signer, chain)?;
    if let Some(r) = &recovery {
        if !is_x25519_multikey(&r.key) {
            return Err(malformed("recoveryKey is an X25519 multikey"));
        }
        verify_attestation(&r.key, &r.attestation, chain)?;
    }
    let (recovery_key, recovery_attestation) = match recovery {
        Some(r) => (Some(r.key), Some(r.attestation)),
        None => (None, None),
    };
    let aad = record_aad(scid, version, recovery_key.as_deref())?;
    let state = chain.state();
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
    Ok(SealedRecord {
        version,
        wrapped,
        recovery_key,
        recovery_attestation,
        nonce: bs58::encode(nonce).into_string(),
        ciphertext: b64().encode(ciphertext),
        signer,
        sig: String::new(),
    })
}

/// Signs the envelope of `sealed` with `signer`, the key `sealed.signer` names in `chain`.
#[cfg(feature = "build")]
pub fn sign_envelope(
    chain: &KelChain,
    sealed: &mut SealedRecord,
    signer: &dyn crate::build::Signer,
) -> Result<(), RecordError> {
    let (_, key) = writer_of(&sealed.signer, chain)?;
    if *key != signer.key() {
        return Err(RecordError::Signer(signer.key().to_string()));
    }
    let bytes = envelope_signing_bytes(chain.id(), sealed)?;
    sealed.sig = ad4m_hash_chain::encode_proof_value(&signer.sign(&bytes));
    Ok(())
}

/// Opens `sealed` of the identity whose log is `chain` through its `wrapped[entry]` (a key id,
/// or `"recovery"`) with that reader's X25519 `secret`. The envelope's signature is checked
/// against the log first ([`verify_envelope`]): a record no key of the identity signed is
/// refused before any of its fields is used, and so is a recovery key no master of the log
/// attested. A record written before a rotation still opens; whether its recovery key still
/// counts is [`recovery_of`].
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
    verify_envelope(sealed, chain)?;
    check_attested_by_some_master(sealed, chain)?;
    let wrapped = sealed
        .wrapped
        .get(entry)
        .ok_or_else(|| RecordError::NoEntry(entry.to_string()))?;
    let key = unwrap_key(wrapped, chain.id(), secret)?;
    decrypt_verified(sealed, chain.id(), &key)
}

/// The record key that `wrapped` (one entry of a record's `wrapped`) seals for identity `scid`,
/// opened with that reader's X25519 `secret`.
pub fn unwrap_key(
    wrapped: &str,
    scid: &str,
    secret: &X25519Secret,
) -> Result<Zeroizing<[u8; RECORD_KEY_LEN]>, RecordError> {
    let key = seal::open(secret, &wrap_info(scid), wrapped).map_err(|_| RecordError::Open)?;
    let mut out = Zeroizing::new([0u8; RECORD_KEY_LEN]);
    if key.len() != RECORD_KEY_LEN {
        return Err(RecordError::Open);
    }
    out.copy_from_slice(&key);
    Ok(out)
}

/// Decrypts `sealed` of the identity whose log is `chain` with its record `key` (from
/// [`unwrap_key`], or a key the executor unwrapped for the caller). Like [`open_record`] it
/// checks the envelope's signature and recovery attestation against the log first.
pub fn decrypt_record(
    sealed: &SealedRecord,
    chain: &KelChain,
    key: &[u8; RECORD_KEY_LEN],
) -> Result<PrivateRecord, RecordError> {
    verify_envelope(sealed, chain)?;
    check_attested_by_some_master(sealed, chain)?;
    decrypt_verified(sealed, chain.id(), key)
}

/// Decrypts `sealed` of identity `scid` whose envelope the caller has verified.
fn decrypt_verified(
    sealed: &SealedRecord,
    scid: &str,
    key: &[u8; RECORD_KEY_LEN],
) -> Result<PrivateRecord, RecordError> {
    use base64::Engine;
    let aad = record_aad(scid, sealed.version, sealed.recovery_key.as_deref())?;
    let nonce = decode_nonce(&sealed.nonce)?;
    let ciphertext = b64()
        .decode(&sealed.ciphertext)
        .map_err(|_| malformed("ciphertext is base64url"))?;
    let plain = Zeroizing::new(
        XChaCha20Poly1305::new(key.into())
            .decrypt(
                XNonce::from_slice(&nonce),
                Payload {
                    msg: &ciphertext,
                    aad: &aad,
                },
            )
            .map_err(|_| RecordError::Open)?,
    );
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
/// with `wrapped["recovery"]` exactly when `recoveryKey` is set, and a `recoveryKey` only with
/// the current master's attestation ([`verify_attestation`]). The store cannot open it, so
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
    if let (Some(key), Some(attestation)) = (&sealed.recovery_key, &sealed.recovery_attestation) {
        verify_attestation(key, attestation, chain)?;
    }
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
