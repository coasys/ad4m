use crate::types::Expression;
use chrono::SecondsFormat;
use chrono::{DateTime, Utc};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, PatchedKeyPair};
use log::error;
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::str::FromStr;

pub fn verify_string_signed_by_did(
    did: &str,
    data: &str,
    signed_data: &str,
) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(signed_data)?;
    let message = hash_message(&data.to_string());
    Ok(inner_verify(did, &message, &sig_bytes))
}

pub fn verify<T: Serialize>(expr: &Expression<T>) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(&expr.proof.signature)?;
    let timestamp = DateTime::<Utc>::from_str(&expr.timestamp).map_err(|e| {
        anyhow!(
            "Failed to parse timestamp when trying to verify signature: {}",
            e
        )
    })?;
    let message = hash_data_and_timestamp(&expr.data, &timestamp);
    let result = inner_verify(&expr.author, &message, &sig_bytes);
    Ok(result)
}

pub(super) fn hash_data_and_timestamp<T: Serialize>(
    data: &T,
    timestamp: &DateTime<Utc>,
) -> Vec<u8> {
    let mut hasher = Sha256::new();

    // Serialize and hash the data directly.
    let serialized_data = serde_json::to_vec(data).expect("Failed to serialize data");
    hasher.update(&serialized_data);

    // Serialize and hash the timestamp.
    let timestamp_str = timestamp.to_rfc3339_opts(SecondsFormat::Millis, true);
    hasher.update(timestamp_str.as_bytes());

    // Finalize the hash and return the result.
    hasher.finalize().as_slice().into()
}

pub(super) fn hash_message(message: &String) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(message.as_bytes());
    hasher.finalize().as_slice().into()
}

fn inner_verify(did: &str, message: &[u8], signature: &[u8]) -> bool {
    if let Ok(key_pair) = PatchedKeyPair::try_from(did) {
        match key_pair.verify(message, signature) {
            Ok(_) => true,
            Err(_) => {
                //debug!("Signature verification failed: {:?}", e);
                false
            }
        }
    } else {
        error!("Failed to parse DID as key method: {}", did);
        false
    }
}

/// A second signing identity for tests: a real `did:key` keypair that signs
/// the way [`crate::agent::create_signed_expression`] signs for the local
/// agent, so links it authors verify on the receiving side
/// (`proof.valid == Some(true)`).
///
/// It lives next to [`verify`] deliberately. The flow engine counts a vote
/// only when the stored signature verdict is valid, so a test can no longer
/// stand in for a second agent by writing a link with a made-up signature —
/// it has to hold a key.
#[cfg(test)]
pub struct TestSigner {
    keypair: PatchedKeyPair,
    /// `did:key:…` — the author of everything this signer signs.
    pub did: String,
    /// The verification-method id that goes into `proof.key`.
    pub key_id: String,
}

#[cfg(test)]
impl TestSigner {
    pub fn generate() -> Self {
        use did_key::{DIDCore, Ed25519KeyPair};
        let keypair = did_key::generate::<Ed25519KeyPair>(None);
        let document = keypair.get_did_document(did_key::Config::default());
        TestSigner {
            did: document.id.clone(),
            key_id: document.verification_method[0].id.clone(),
            keypair,
        }
    }

    /// Sign `data` now, producing the same `Expression` shape the wallet
    /// path produces.
    pub fn sign<T: Serialize>(&self, data: T) -> Expression<T> {
        let timestamp = Utc::now();
        let signature = hex::encode(
            self.keypair
                .sign(&hash_data_and_timestamp(&data, &timestamp)),
        );
        Expression {
            author: self.did.clone(),
            timestamp: timestamp.to_rfc3339_opts(SecondsFormat::Millis, true),
            data,
            proof: crate::types::ExpressionProof {
                key: self.key_id.clone(),
                signature,
            },
        }
    }
}
