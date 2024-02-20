use deno_core::error::AnyError;
use serde_json::json;
use sha2::{Sha256, Digest};
use crate::types::Expression;
use did_key::{CoreSign, PatchedKeyPair};
use log::error;

pub fn verify_string_signed_by_did(did: &str, data: &str, signed_data: &str) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(signed_data)?;
    let message = build_message_raw(&serde_json::Value::String(data.to_owned()));
    Ok(inner_verify(did, &message, &sig_bytes))
}

pub fn verify(expr: &Expression) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(&expr.proof.signature)?;
    let message = build_message(&expr.data, &expr.timestamp);
    Ok(inner_verify(&expr.author, &message, &sig_bytes))
}

fn build_message(data: &serde_json::Value, timestamp: &str) -> Vec<u8> {
    let payload  = json!({ "data": data, "timestamp": timestamp });
    let payload_string = serde_json::to_string(&payload).expect("Failed to serialize payload");
    let mut hasher = Sha256::new();
    hasher.update(payload_string.as_bytes());
    hasher.finalize().as_slice().try_into().expect("Hash should be 32 bytes")
}

fn build_message_raw(data: &serde_json::Value) -> Vec<u8> {
    let payload = json!({ "data": data });
    let payload_string = serde_json::to_string(&payload).expect("Failed to serialize payload");
    let mut hasher = Sha256::new();
    hasher.update(payload_string.as_bytes());
    hasher.finalize().as_slice().try_into().expect("Hash should be 32 bytes")
}


fn inner_verify(did: &str, message: &[u8], signature: &[u8]) -> bool {
    if let Ok(key_pair) = PatchedKeyPair::try_from(did) {
        match key_pair.verify(message, signature) {
            Ok(_) => true,
            Err(e) => {
                error!("Signature verification failed: {:?}", e);
                false
            }
        }
    } else {
        error!("Failed to parse DID as key method: {}", did);
        false
    }
}