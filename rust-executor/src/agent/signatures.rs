use std::collections::BTreeMap;
use deno_core::error::AnyError;
use sha2::{Sha256, Digest};
use crate::types::Expression;
use did_key::{CoreSign, PatchedKeyPair};
use log::error;

pub fn verify_string_signed_by_did(did: &str, data: &str, signed_data: &str) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(signed_data)?;
    let message = hash_message(&data.to_string());
    Ok(inner_verify(did, &message, &sig_bytes))
}

pub fn verify(expr: &Expression) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(&expr.proof.signature)?;
    let message = build_message(&expr.data, &expr.timestamp);
    Ok(inner_verify(&expr.author, &message, &sig_bytes))
fn sort_json_value(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(k.clone(), sort_json_value(v));
            }
            serde_json::Value::Object(serde_json::Map::from_iter(map.into_iter()))
        },
        serde_json::Value::Array(arr) => {
            serde_json::Value::Array(arr.iter().map(sort_json_value).collect())
        },
        _ => value.clone(),
    }
}

pub(super) fn build_message(data: &serde_json::Value, timestamp: &str) -> Vec<u8> {

    let sorted_data = sort_json_value(data);
    let mut map = BTreeMap::new();
    map.insert("data".to_string(), sorted_data);
    map.insert("timestamp".to_string(), serde_json::Value::String(timestamp.to_string()));

    let sorted_payload = serde_json::Value::Object(serde_json::Map::from_iter(map.into_iter()));
    let payload_string = serde_json::to_string(&sorted_payload).expect("Failed to serialize payload");

    let mut hasher = Sha256::new();
    hasher.update(payload_string.as_bytes());
    hasher.finalize().as_slice().try_into().expect("Hash should be 32 bytes")
}

pub(super) fn hash_message(message: &String) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(message.as_bytes());
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