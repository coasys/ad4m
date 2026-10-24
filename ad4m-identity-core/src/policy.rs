//! `KelPolicy`: the key log as a chain type on `ad4m-hash-chain` (rules K1–K15 in
//! section 5 of `docs-src/identity/did-scid-coasys-1.md`). The module checks canonical form,
//! `seq`, `prev` and the chain id first; the policy then decides who must sign and what the
//! event may do.

use ad4m_hash_chain::{decode_hash, verify_proof, ChainId, ChainPolicy, Hash, Multikey, Proof};
use std::collections::BTreeMap;

use crate::event::{DeviceKey, Event, KeyLife, Op, BOT_ACTIONS};
use crate::keys::{is_ed25519_multikey, is_x25519_multikey};
use crate::state::{KeyRecord, KeyState};
use crate::{commitment, derive_scid, KeyId, DID_PREFIX, FORMAT, KEL_DOMAIN, PROOF_PURPOSE};

/// Why an event does not have the format-1 shape. Reaches callers as `FoldError::Format`.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum FormatError {
    #[error("format {0:?}, expected \"coasys/1\"")]
    Version(String),
    #[error("unknown op {0:?}")]
    UnknownOp(String),
    #[error("unknown field {0:?}")]
    UnknownField(String),
    #[error("missing field {0:?}")]
    Missing(String),
    #[error("field {0:?} has the wrong JSON type")]
    Type(String),
    #[error("floating-point number {0}")]
    Float(String),
    #[error("integer {0} is at or above 2^53")]
    BigInteger(String),
    #[error("kind {0:?} is not supported by format 1")]
    UnsupportedKind(String),
    #[error("bot action {0:?} is not one of read, write, join")]
    UnknownAction(String),
    #[error("not canonical: {0}")]
    NotCanonical(String),
    #[error("bad proof member: {0}")]
    Proof(String),
    #[error("{0:?} is not a sha2-256 multihash")]
    BadHash(String),
    #[error("scid {got:?}, the incept derives {want:?}")]
    Scid { want: String, got: String },
    #[error("the first event of a log must be an incept")]
    NotIncept,
    #[error("an incept may only open a log")]
    InceptAfterGenesis,
    #[error("an incept must list at least one device key")]
    EmptyAdd,
    #[error("a revoke must name at least one key")]
    EmptyRevoke,
}

/// The KEL's own refusals. They reach callers as `ChainError::Policy(FoldError)`.
/// Wrong seq, wrong prev and a different scid are `ChainError::Seq`, `Prev` and `WrongChain`.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum FoldError {
    #[error("Format: {0}")]
    Format(FormatError),
    #[error("MissingProof: no proof by {0}")]
    MissingProof(KeyId),
    #[error("BadProof: the proof by {0} does not hold")]
    BadProof(KeyId),
    #[error("UnknownKey: {0}")]
    UnknownKey(KeyId),
    #[error("RevokedKey: {0}")]
    RevokedKey(KeyId),
    #[error("DuplicateKey: {0} is already in the log")]
    DuplicateKey(Multikey),
    #[error("KeyIdOrder: key id {got}, expected {want}")]
    KeyIdOrder { want: KeyId, got: KeyId },
    #[error("NotABot: {0} carries no bot mark")]
    NotABot(KeyId),
    #[error("BotInIncept: an incept lists no bot key")]
    BotInIncept,
    #[error("EmptyScope: a bot scope names at least one action")]
    EmptyScope,
    #[error("AddNotOne: an add lists exactly one key")]
    AddNotOne,
    #[error("NextMismatch: master and salt do not hash to the committed next key")]
    NextMismatch,
    #[error("Ended: the log has ended")]
    Ended,
    #[error("UnsupportedKeyType: format 1 takes Ed25519 signing and X25519 encryption keys")]
    UnsupportedKeyType,
    #[error("CanWithoutBotMark: `can` on a key without `kind: \"bot\"`")]
    CanWithoutBotMark,
}

impl FormatError {
    /// The variant name (`"UnknownField"`, …).
    pub fn name(&self) -> &'static str {
        match self {
            FormatError::Version(_) => "Version",
            FormatError::UnknownOp(_) => "UnknownOp",
            FormatError::UnknownField(_) => "UnknownField",
            FormatError::Missing(_) => "Missing",
            FormatError::Type(_) => "Type",
            FormatError::Float(_) => "Float",
            FormatError::BigInteger(_) => "BigInteger",
            FormatError::UnsupportedKind(_) => "UnsupportedKind",
            FormatError::UnknownAction(_) => "UnknownAction",
            FormatError::NotCanonical(_) => "NotCanonical",
            FormatError::Proof(_) => "Proof",
            FormatError::BadHash(_) => "BadHash",
            FormatError::Scid { .. } => "Scid",
            FormatError::NotIncept => "NotIncept",
            FormatError::InceptAfterGenesis => "InceptAfterGenesis",
            FormatError::EmptyAdd => "EmptyAdd",
            FormatError::EmptyRevoke => "EmptyRevoke",
        }
    }
}

impl FoldError {
    /// The variant name: the `reason` of a `policy` error code and the store's 400 body.
    pub fn name(&self) -> &'static str {
        match self {
            FoldError::Format(_) => "Format",
            FoldError::MissingProof(_) => "MissingProof",
            FoldError::BadProof(_) => "BadProof",
            FoldError::UnknownKey(_) => "UnknownKey",
            FoldError::RevokedKey(_) => "RevokedKey",
            FoldError::DuplicateKey(_) => "DuplicateKey",
            FoldError::KeyIdOrder { .. } => "KeyIdOrder",
            FoldError::NotABot(_) => "NotABot",
            FoldError::BotInIncept => "BotInIncept",
            FoldError::EmptyScope => "EmptyScope",
            FoldError::AddNotOne => "AddNotOne",
            FoldError::NextMismatch => "NextMismatch",
            FoldError::Ended => "Ended",
            FoldError::UnsupportedKeyType => "UnsupportedKeyType",
            FoldError::CanWithoutBotMark => "CanWithoutBotMark",
        }
    }
}

impl From<FormatError> for FoldError {
    fn from(e: FormatError) -> Self {
        FoldError::Format(e)
    }
}

/// The KEL's chain type. The module checks `seq`, `prev` and the chain id first, then calls the policy.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct KelPolicy;

impl ChainPolicy for KelPolicy {
    type Entry = Event;
    type State = KeyState;
    type Error = FoldError;
    const TYPE: &'static str = "coasys:kel";
    const DOMAIN: &'static str = KEL_DOMAIN;

    /// Event #0 (spec rules K1–K4): the SCID and the `incept` signers. The chain id is the SCID.
    fn genesis(&self, event: &Event) -> Result<(ChainId, KeyState), FoldError> {
        check_format(event)?;
        let Op::Incept {
            kind,
            master,
            next,
            add,
        } = &event.op
        else {
            return Err(FormatError::NotIncept.into());
        };
        let want = derive_scid(event)?;
        if want != event.scid {
            return Err(FormatError::Scid {
                want,
                got: event.scid.clone(),
            }
            .into());
        }
        if !is_ed25519_multikey(master) {
            return Err(FoldError::UnsupportedKeyType);
        }
        check_hash(next)?;
        if add.is_empty() {
            return Err(FormatError::EmptyAdd.into());
        }
        let mut state = KeyState {
            did: format!("{DID_PREFIX}{}", event.scid),
            scid: event.scid.clone(),
            kind: *kind,
            masters: vec![KeyRecord {
                id: "m0".into(),
                sign: master.clone(),
                enc: None,
                added: 0,
                revoked: None,
                expires: None,
                scopes: Vec::new(),
            }],
            next: Some(next.clone()),
            keys: BTreeMap::new(),
            ended: false,
        };
        for key in add {
            if matches!(key.life, KeyLife::Bot(_)) {
                return Err(FoldError::BotInIncept);
            }
            insert_key(&mut state, key, 0)?;
        }
        let mut signers: Vec<(KeyId, Multikey)> = vec![("m0".into(), master.clone())];
        signers.extend(add.iter().map(|k| (k.id.clone(), k.sign.clone())));
        check_signers(&state.did, event, &signers)?;
        Ok((event.scid.clone(), state))
    }

    fn chain_of(&self, event: &Event) -> ChainId {
        event.scid.clone()
    }

    /// Spec rules K5–K15. Calls `verify_proof` with the key that `KeyState` names and the
    /// domain "coasys:kel".
    fn apply(&self, state: &mut KeyState, event: &Event) -> Result<(), FoldError> {
        if state.ended {
            return Err(FoldError::Ended);
        }
        check_format(event)?;
        let seq = event.seq;
        match &event.op {
            Op::Incept { .. } => return Err(FormatError::InceptAfterGenesis.into()),
            Op::Add { add, .. } => {
                let [key] = add.as_slice() else {
                    return Err(FoldError::AddNotOne);
                };
                require_master(state, event)?;
                insert_key(state, key, seq)?;
            }
            Op::Revoke { revoke } => {
                if revoke.is_empty() {
                    return Err(FormatError::EmptyRevoke.into());
                }
                require_master(state, event)?;
                revoke_keys(state, revoke, seq)?;
            }
            Op::Rotate {
                master,
                salt,
                next,
                revoke,
            } => {
                if !is_ed25519_multikey(master) {
                    return Err(FoldError::UnsupportedKeyType);
                }
                if let Some(next) = next {
                    check_hash(next)?;
                }
                if state.next.as_deref() != Some(commitment(master, salt).as_str()) {
                    return Err(FoldError::NextMismatch);
                }
                if state.key_id_of(master).is_some() {
                    return Err(FoldError::DuplicateKey(master.clone()));
                }
                let id = state.next_master_id();
                check_signers(&state.did, event, &[(id.clone(), master.clone())])?;
                revoke_keys(state, revoke, seq)?;
                if let Some(old) = state.masters.last_mut() {
                    old.revoked = Some(seq);
                }
                state.masters.push(KeyRecord {
                    id,
                    sign: master.clone(),
                    enc: None,
                    added: seq,
                    revoked: None,
                    expires: None,
                    scopes: Vec::new(),
                });
                state.next = next.clone();
                if next.is_none() {
                    // `next: null` ends the log: every key stops at this seq.
                    state.ended = true;
                    for record in state.masters.iter_mut().chain(state.keys.values_mut()) {
                        record.revoked.get_or_insert(seq);
                    }
                }
            }
            Op::Scope { key, can, expires } => {
                check_actions(can)?;
                require_master(state, event)?;
                let record = state
                    .keys
                    .get_mut(key)
                    .ok_or_else(|| FoldError::UnknownKey(key.clone()))?;
                if record.revoked.is_some() {
                    return Err(FoldError::RevokedKey(key.clone()));
                }
                if record.scopes.is_empty() {
                    return Err(FoldError::NotABot(key.clone()));
                }
                record.scopes.push((
                    seq,
                    crate::event::BotScope {
                        can: can.clone(),
                        expires: *expires,
                    },
                ));
            }
        }
        Ok(())
    }
}

fn check_format(event: &Event) -> Result<(), FoldError> {
    if event.format != FORMAT {
        return Err(FormatError::Version(event.format.clone()).into());
    }
    Ok(())
}

fn check_hash(hash: &Hash) -> Result<(), FoldError> {
    decode_hash(hash)
        .map(|_| ())
        .map_err(|_| FormatError::BadHash(hash.clone()).into())
}

fn check_actions(can: &[String]) -> Result<(), FoldError> {
    if can.is_empty() {
        return Err(FoldError::EmptyScope);
    }
    match can.iter().find(|a| !BOT_ACTIONS.contains(&a.as_str())) {
        Some(action) => Err(FormatError::UnknownAction(action.clone()).into()),
        None => Ok(()),
    }
}

/// Spec rules K7, K8, K9 and K15 for one listed key, then adds it at `seq`.
fn insert_key(state: &mut KeyState, key: &DeviceKey, seq: u64) -> Result<(), FoldError> {
    let want = state.next_device_id();
    if key.id != want {
        return Err(FoldError::KeyIdOrder {
            want,
            got: key.id.clone(),
        });
    }
    if !is_ed25519_multikey(&key.sign) || !is_x25519_multikey(&key.enc) {
        return Err(FoldError::UnsupportedKeyType);
    }
    if state.key_id_of(&key.sign).is_some() {
        return Err(FoldError::DuplicateKey(key.sign.clone()));
    }
    let (expires, scopes) = match &key.life {
        KeyLife::Standing => (None, Vec::new()),
        KeyLife::Until(expires) => (Some(*expires), Vec::new()),
        KeyLife::Bot(scope) => {
            check_actions(&scope.can)?;
            (None, vec![(seq, scope.clone())])
        }
    };
    state.keys.insert(
        key.id.clone(),
        KeyRecord {
            id: key.id.clone(),
            sign: key.sign.clone(),
            enc: Some(key.enc.clone()),
            added: seq,
            revoked: None,
            expires,
            scopes,
        },
    );
    Ok(())
}

fn revoke_keys(state: &mut KeyState, ids: &[KeyId], seq: u64) -> Result<(), FoldError> {
    for id in ids {
        let record = state
            .keys
            .get_mut(id)
            .ok_or_else(|| FoldError::UnknownKey(id.clone()))?;
        if record.revoked.is_some() {
            return Err(FoldError::RevokedKey(id.clone()));
        }
        record.revoked = Some(seq);
    }
    Ok(())
}

/// `add`, `revoke`, `scope`: the current master key signs, and no other key.
fn require_master(state: &KeyState, event: &Event) -> Result<(), FoldError> {
    let master = state.current_master().ok_or(FoldError::Ended)?;
    check_signers(
        &state.did,
        event,
        &[(master.id.clone(), master.sign.clone())],
    )
}

/// Spec rules K4 and K10: each listed signer has exactly one proof, naming `<did>#<id>`, purpose
/// `assertionMethod`, domain `coasys:kel`, that verifies under its key. Any other proof fails.
fn check_signers(did: &str, event: &Event, signers: &[(KeyId, Multikey)]) -> Result<(), FoldError> {
    for (id, key) in signers {
        let method = format!("{did}#{id}");
        let mut matching = event
            .proof
            .iter()
            .filter(|p| p.verification_method == method);
        let proof: &Proof = matching
            .next()
            .ok_or_else(|| FoldError::MissingProof(id.clone()))?;
        if matching.next().is_some() || proof.proof_purpose != PROOF_PURPOSE {
            return Err(FoldError::BadProof(id.clone()));
        }
        verify_proof(event, proof, key, KEL_DOMAIN).map_err(|_| FoldError::BadProof(id.clone()))?;
    }
    if let Some(extra) = event.proof.iter().find(|p| {
        !signers
            .iter()
            .any(|(id, _)| p.verification_method == format!("{did}#{id}"))
    }) {
        let named = extra
            .verification_method
            .strip_prefix(did)
            .and_then(|rest| rest.strip_prefix('#'))
            .unwrap_or(&extra.verification_method);
        return Err(FoldError::BadProof(named.to_string()));
    }
    Ok(())
}
