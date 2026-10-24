//! W3C Data Integrity proofs with the `eddsa-jcs-2022` cryptosuite (AR-01 4.3.3 rule 3).
//!
//! The key signs `sha256(JCS(proof config)) ‖ sha256(JCS(entry without "proof"))`, where the
//! proof config is the proof without `proofValue` (plus the entry's `@context` when it has
//! one, as the cryptosuite specifies). The proof carries a `domain`; a verifier states the
//! domain it expects and refuses any other.

use ed25519_dalek::{Signature, VerifyingKey};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::codec::{decode_ed25519_multikey, decode_proof_value, sha256, Multikey};
use crate::jcs::{canonical_json, canonical_object_without, CanonError};
use crate::Entry;

/// The proof `type` of every proof the module checks.
pub const PROOF_TYPE: &str = "DataIntegrityProof";
/// The only cryptosuite the module checks.
pub const CRYPTOSUITE: &str = "eddsa-jcs-2022";

/// One Data Integrity proof. It sits in the entry's `proof` array, outside the entry hash.
/// Unknown members are refused, so a proof never loses a signed field on a round trip.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Proof {
    pub r#type: String,
    pub cryptosuite: String,
    pub verification_method: String,
    pub proof_purpose: String,
    pub domain: String,
    pub proof_value: String,
}

impl Proof {
    /// A `DataIntegrityProof` with suite `eddsa-jcs-2022` and an empty `proofValue`: the
    /// proof config a signer passes to [`signing_input`] before it fills in the value.
    pub fn unsigned(verification_method: &str, proof_purpose: &str, domain: &str) -> Proof {
        Proof {
            r#type: PROOF_TYPE.to_string(),
            cryptosuite: CRYPTOSUITE.to_string(),
            verification_method: verification_method.to_string(),
            proof_purpose: proof_purpose.to_string(),
            domain: domain.to_string(),
            proof_value: String::new(),
        }
    }
}

/// Why a proof does not hold for a key and a domain.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum ProofError {
    #[error("proof domain {got:?}, expected {want:?}")]
    Domain { want: String, got: String },
    #[error("unsupported proof suite: {0}")]
    Suite(String),
    #[error("bad key, proof value or entry encoding")]
    Encoding,
    #[error("signature does not verify")]
    Signature,
}

/// The bytes a key signs for `proof_without_value` over `entry` (eddsa-jcs-2022). The
/// caller holds the key and signs. `proof_value` of the given proof is ignored. Fails when
/// the entry has no canonical form (a float, an integer outside ±(2^53 − 1), or not an
/// object).
pub fn signing_input<E: Entry>(
    entry: &E,
    proof_without_value: &Proof,
) -> Result<Vec<u8>, CanonError> {
    let value = serde_json::to_value(entry).map_err(|e| CanonError::Serde(e.to_string()))?;
    signing_input_json(&value, proof_without_value)
}

/// [`signing_input`] over an entry given as JSON.
fn signing_input_json(entry: &Value, proof: &Proof) -> Result<Vec<u8>, CanonError> {
    let map = entry.as_object().ok_or(CanonError::NotObject)?;
    let mut config = serde_json::to_value(proof).map_err(|e| CanonError::Serde(e.to_string()))?;
    let config_map = config
        .as_object_mut()
        .ok_or_else(|| CanonError::Serde("proof is not an object".into()))?;
    config_map.remove("proofValue");
    if let Some(context) = map.get("@context") {
        config_map.insert("@context".into(), context.clone());
    }
    let mut input = Vec::with_capacity(64);
    input.extend_from_slice(&sha256(canonical_json(&config)?.as_bytes()));
    input.extend_from_slice(&sha256(canonical_object_without(map, "proof")?.as_bytes()));
    Ok(input)
}

/// Checks that `proof` over `entry` is a valid `eddsa-jcs-2022` signature by `key` under
/// `domain`. Which key must sign is the chain type's question, not this function's.
pub fn verify_proof<E: Entry>(
    entry: &E,
    proof: &Proof,
    key: &Multikey,
    domain: &str,
) -> Result<(), ProofError> {
    let value = serde_json::to_value(entry).map_err(|_| ProofError::Encoding)?;
    verify_proof_json(&value, proof, key, domain)
}

/// [`verify_proof`] over an entry given as JSON.
pub fn verify_proof_json(
    entry: &Value,
    proof: &Proof,
    key: &str,
    domain: &str,
) -> Result<(), ProofError> {
    if proof.r#type != PROOF_TYPE {
        return Err(ProofError::Suite(format!("type {}", proof.r#type)));
    }
    if proof.cryptosuite != CRYPTOSUITE {
        return Err(ProofError::Suite(format!(
            "cryptosuite {}",
            proof.cryptosuite
        )));
    }
    if proof.domain != domain {
        return Err(ProofError::Domain {
            want: domain.to_string(),
            got: proof.domain.clone(),
        });
    }
    let key = decode_ed25519_multikey(key).ok_or(ProofError::Encoding)?;
    let key = VerifyingKey::from_bytes(&key).map_err(|_| ProofError::Encoding)?;
    let signature = decode_proof_value(&proof.proof_value).ok_or(ProofError::Encoding)?;
    let input = signing_input_json(entry, proof).map_err(|_| ProofError::Encoding)?;
    key.verify_strict(&input, &Signature::from_bytes(&signature))
        .map_err(|_| ProofError::Signature)
}
