//! Event builders (feature `build`). The caller holds the keys: an `*_unsigned` builder gives
//! the event with no proof, [`signing_input`] gives the bytes each key signs, and
//! [`attach_proof`] adds the proof from a signature. The [`Signer`] forms do all three with a
//! key that may live in memory, an OS keychain or an executor wallet.
//!
//! Builders shape events; they do not decide validity. `fold` and `Chain::append` do.

use ad4m_hash_chain::{encode_proof_value, Hash, Multikey, Proof};
use serde::Serialize;

use crate::domain::{signing_bytes, DomainError};
use crate::event::{BotScope, DeviceKey, Event, Kind, Op};
use crate::policy::FormatError;
use crate::{
    commitment, derive_scid, did_of, verification_method, KelChain, KeyId, FORMAT, KEL_DOMAIN,
    PROOF_PURPOSE, SCID_PLACEHOLDER,
};

/// Hides where a key lives: memory, an OS keychain, an executor wallet.
pub trait Signer {
    /// The signing multikey (`z6Mk…`).
    fn key(&self) -> Multikey;
    /// The Ed25519 signature of `bytes`.
    fn sign(&self, bytes: &[u8]) -> [u8; 64];
}

impl Signer for ed25519_dalek::SigningKey {
    fn key(&self) -> Multikey {
        ad4m_hash_chain::encode_ed25519_multikey(self.verifying_key().as_bytes())
    }
    fn sign(&self, bytes: &[u8]) -> [u8; 64] {
        ed25519_dalek::Signer::sign(self, bytes).to_bytes()
    }
}

/// Why a builder cannot shape an event.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum BuildError {
    #[error("{0}")]
    Format(#[from] FormatError),
    #[error("the log has ended")]
    Ended,
    #[error("the signer is not the current master key")]
    NotCurrentMaster,
    #[error("the revealed key and salt do not hash to the committed next key")]
    NextMismatch,
    #[error("the event already carries a proof by {0}")]
    AlreadySigned(KeyId),
}

/// `seq`, `prev` and `scid` of the event after the chain's head.
fn after_head(chain: &KelChain) -> Result<(u64, Hash, String), BuildError> {
    if chain.state().ended {
        return Err(BuildError::Ended);
    }
    let head = chain.head();
    Ok((head.seq + 1, head.hash, chain.id().clone()))
}

fn later(chain: &KelChain, op: Op) -> Result<Event, BuildError> {
    let (seq, prev, scid) = after_head(chain)?;
    Ok(Event {
        format: FORMAT.to_string(),
        scid,
        seq,
        prev: Some(prev),
        op,
        proof: Vec::new(),
    })
}

/// Event #0 of a person's log with its SCID (rule 2) and no proof. Device ids must run `d1`, `d2`, ….
pub fn incept_unsigned(
    master: &Multikey,
    next: Hash,
    devices: Vec<DeviceKey>,
) -> Result<Event, BuildError> {
    let mut event = Event {
        format: FORMAT.to_string(),
        scid: SCID_PLACEHOLDER.to_string(),
        seq: 0,
        prev: None,
        op: Op::Incept {
            kind: Kind::Human,
            master: master.clone(),
            next,
            add: devices,
        },
        proof: Vec::new(),
    };
    event.scid = derive_scid(&event)?;
    Ok(event)
}

/// An `add` of one key after the head. `device.id` must be `state().next_device_id()`.
pub fn add_unsigned(
    chain: &KelChain,
    device: DeviceKey,
    offer: &str,
    co_sig: &str,
) -> Result<Event, BuildError> {
    later(
        chain,
        Op::Add {
            add: vec![device],
            offer: offer.to_string(),
            co_sig: co_sig.to_string(),
        },
    )
}

/// A `revoke` of device keys after the head.
pub fn revoke_unsigned(chain: &KelChain, keys: &[KeyId]) -> Result<Event, BuildError> {
    later(
        chain,
        Op::Revoke {
            revoke: keys.to_vec(),
        },
    )
}

/// A `scope` that replaces a bot key's actions and expiry after the head.
pub fn scope_unsigned(
    chain: &KelChain,
    key: &KeyId,
    scope: &BotScope,
) -> Result<Event, BuildError> {
    later(
        chain,
        Op::Scope {
            key: key.clone(),
            can: scope.can.clone(),
            expires: scope.expires,
        },
    )
}

/// A `rotate` that reveals `master` with its `salt`, commits to `next` (`None` ends the
/// identity) and may revoke device keys.
pub fn rotate_unsigned(
    chain: &KelChain,
    master: &Multikey,
    salt: &str,
    next: Option<Hash>,
    revoke: &[KeyId],
) -> Result<Event, BuildError> {
    later(
        chain,
        Op::Rotate {
            master: master.clone(),
            salt: salt.to_string(),
            next,
            revoke: revoke.to_vec(),
        },
    )
}

/// The proof config (no `proofValue`) of `key_id` over `event`: `<did>#<keyId>`,
/// `assertionMethod`, domain `coasys:kel`.
pub fn proof_config(event: &Event, key_id: &str) -> Proof {
    Proof::unsigned(
        &verification_method(&did_of(&event.scid), key_id),
        PROOF_PURPOSE,
        KEL_DOMAIN,
    )
}

/// The bytes `key_id`'s key signs for `event` (eddsa-jcs-2022, domain `coasys:kel`):
/// `sha256(JCS(proof config)) ‖ sha256(JCS(event without proof))`.
pub fn signing_input(event: &Event, key_id: &str) -> Result<Vec<u8>, BuildError> {
    ad4m_hash_chain::signing_input(event, &proof_config(event, key_id))
        .map_err(|e| BuildError::Format(e.into()))
}

/// Adds the proof of `key_id` made from `signature` over [`signing_input`]. Refuses a second
/// proof by the same key id. The signature is not checked here; `fold` checks it.
pub fn attach_proof(
    event: &mut Event,
    key_id: &str,
    signature: &[u8; 64],
) -> Result<(), BuildError> {
    let mut proof = proof_config(event, key_id);
    if event
        .proof
        .iter()
        .any(|p| p.verification_method == proof.verification_method)
    {
        return Err(BuildError::AlreadySigned(key_id.to_string()));
    }
    proof.proof_value = encode_proof_value(signature);
    event.proof.push(proof);
    Ok(())
}

/// Signs `event` as `key_id` with `signer` and attaches the proof. Each device listed in an
/// `incept` co-signs #0 this way.
pub fn cosign(event: &mut Event, key_id: &str, signer: &dyn Signer) -> Result<(), BuildError> {
    let input = signing_input(event, key_id)?;
    attach_proof(event, key_id, &signer.sign(&input))
}

/// Builds #0 with the master proof (`m0`) only. Each listed device adds its proof with [`cosign`].
pub fn incept_human(
    master: &dyn Signer,
    next: Hash,
    devices: Vec<DeviceKey>,
) -> Result<Event, BuildError> {
    let mut event = incept_unsigned(&master.key(), next, devices)?;
    cosign(&mut event, "m0", master)?;
    Ok(event)
}

fn signed_by_master(
    chain: &KelChain,
    master: &dyn Signer,
    mut event: Event,
) -> Result<Event, BuildError> {
    let current = chain.state().current_master().ok_or(BuildError::Ended)?;
    if current.sign != master.key() {
        return Err(BuildError::NotCurrentMaster);
    }
    let id = current.id.clone();
    cosign(&mut event, &id, master)?;
    Ok(event)
}

/// The `add` that spends a claim (AR-02): one key, its offer id and the claim's co-signature,
/// signed by the current master key.
pub fn add_claimed(
    chain: &KelChain,
    master: &dyn Signer,
    device: DeviceKey,
    offer: &str,
    co_sig: &str,
) -> Result<Event, BuildError> {
    let event = add_unsigned(chain, device, offer, co_sig)?;
    signed_by_master(chain, master, event)
}

/// A `revoke` signed by the current master key.
pub fn revoke(chain: &KelChain, master: &dyn Signer, keys: &[KeyId]) -> Result<Event, BuildError> {
    let event = revoke_unsigned(chain, keys)?;
    signed_by_master(chain, master, event)
}

/// A `scope` signed by the current master key.
pub fn scope(
    chain: &KelChain,
    master: &dyn Signer,
    key: &KeyId,
    scope: &BotScope,
) -> Result<Event, BuildError> {
    let event = scope_unsigned(chain, key, scope)?;
    signed_by_master(chain, master, event)
}

/// A `rotate` signed by the revealed key, which takes the id `m<n+1>`.
pub fn rotate(
    chain: &KelChain,
    revealed: &dyn Signer,
    salt: &str,
    next: Option<Hash>,
    revoke: &[KeyId],
) -> Result<Event, BuildError> {
    let key = revealed.key();
    if chain.state().next.as_deref() != Some(commitment(&key, salt).as_str()) {
        return Err(if chain.state().ended {
            BuildError::Ended
        } else {
            BuildError::NextMismatch
        });
    }
    let mut event = rotate_unsigned(chain, &key, salt, next, revoke)?;
    let id = chain.state().next_master_id();
    cosign(&mut event, &id, revealed)?;
    Ok(event)
}

/// Signs `value` under a domain-field `domain` (see [`signing_bytes`]). Refuses labels,
/// unknown domains and `coasys:kel`.
pub fn sign_domain<T: Serialize>(
    domain: &str,
    value: &T,
    signer: &dyn Signer,
) -> Result<[u8; 64], DomainError> {
    Ok(signer.sign(&signing_bytes(domain, value)?))
}
