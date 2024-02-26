use deno_core::error::AnyError;
use deno_core::anyhow::anyhow;

use crate::types::{Expression, ExpressionProof};
use crate::wallet::Wallet;

pub mod capabilities;
pub mod signatures;

pub fn did_document() -> did_key::Document {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    wallet_ref
        .get_did_document(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))
        .unwrap()
}

pub fn signing_key_id() -> String {
    did_document().verification_method[0].id.clone()
}

pub fn did() -> String {
    did_document().id.clone()
}

pub fn create_signed_expression(data: serde_json::Value) -> Result<Expression, AnyError> {
    let timestamp = chrono::Utc::now().to_rfc3339();
    let payload_bytes = signatures::build_message(&data, &timestamp);

    let signature = sign(&payload_bytes)?;
    let sig_hex = hex::encode(signature);

    let proof = ExpressionProof {
        signature: sig_hex,
        key: signing_key_id(),
    };

    let signed_expression = Expression {
        author: did(),
        timestamp,
        data,
        proof,
    };

    Ok(signed_expression)
}


pub fn sign(payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    let signature = wallet_ref
        .sign(&name, payload)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    Ok(signature)
}

pub fn sign_string_hex(data: String) -> Result<String, AnyError> {
    let payload_bytes = signatures::hash_message(&data);
    let signature = sign(&payload_bytes)?;
    let sig_hex = hex::encode(signature);
    Ok(sig_hex)
}

pub struct AgentSignature {
    pub signature: String,
    pub public_key: String,
}

impl AgentSignature {
    pub fn from_message(message: String) -> Result<AgentSignature, AnyError> {
        let signature = sign_string_hex(message)?;
        Ok(AgentSignature {
            signature,
            public_key: signing_key_id(),
        })
    }
}

impl Into<crate::graphql::graphql_types::AgentSignature> for AgentSignature {
    fn into(self) -> crate::graphql::graphql_types::AgentSignature {
        crate::graphql::graphql_types::AgentSignature {
            signature: self.signature,
            public_key: self.public_key,
        }
    }
}


#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use serde_json::json;

    use super::*;
    use crate::agent::signatures::{verify_string_signed_by_did, verify};
    use crate::types::ExpressionProof;
    use itertools::Itertools;

    fn setup_wallet() {
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_mut().expect("wallet instance");
        wallet_ref.generate_keypair("main".to_string());
    }

    #[test]
    fn test_sign_and_verify_string_hex_roundtrip() {
        setup_wallet();
        let test_message = "Hello, World!".to_string();
        let signature = sign_string_hex(test_message.clone()).expect("Failed to sign message");
        let did = did();

        assert!(
            verify_string_signed_by_did(&did, &test_message, &signature).expect("Verification failed"),
            "Signature verification for sign_string_hex failed"
        );
    }

    #[test]
    fn test_create_signed_expression() {
        setup_wallet();
        let signed_expression = create_signed_expression(json!({"test": "data"})).expect("Failed to create signed expression");
        assert!(
            signatures::verify(&signed_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression failed"
        );

        let mut broken = signed_expression.clone();
        broken.proof.signature = "broken".to_string();

        assert!(signatures::verify(&broken).is_err(), "Broken signature verification should fail");

        let mut changed = signed_expression.clone();
        changed.data = json!({"changed": "data"});

        assert!(
            !signatures::verify(&changed).expect("Verification failed"),
            "Signature invalidation for create_signed_expression failed"
        );
    }


    #[test]
    fn test_agent_signature_roundtrip() {
        setup_wallet();
        let test_message = "Agent signature test".to_string();
        let agent_signature = AgentSignature::from_message(test_message.clone()).expect("Failed to create agent signature");
        let did = did();

        assert!(
            verify_string_signed_by_did(&did, &test_message, &agent_signature.signature).expect("Verification failed"),
            "Signature verification for AgentSignature failed"
        );
    }

    #[test]
    fn test_create_signed_expression_and_verify_with_changed_sorting() {
        setup_wallet();
        let json_value = json!({"key2": "value1", "key1": "value2"});
        let signed_expression = create_signed_expression(json_value).expect("Failed to create signed expression");

        // Simulate changing the sorting of the JSON in the signed expression
        let mut data_map = BTreeMap::new();
        let sorted_keys = signed_expression.data.as_object().unwrap().keys().sorted();
        for key in sorted_keys {
            data_map.insert(key.clone(), signed_expression.data[key].clone());
        }
        let sorted_json = json!(data_map);
        let mut sorted_expression = signed_expression.clone();
        sorted_expression.data = sorted_json;

        // Verify the expression with changed sorting
        assert!(
            signatures::verify(&sorted_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression with changed sorting should succeed"
        );
    }

    #[test]
    fn test_create_signed_expression_with_data_string() {
        setup_wallet();
        let json_value = serde_json::Value::String(r#"{"key2": "value1", "key1": "value2"}"#.to_string());
        let signed_expression = create_signed_expression(json_value).expect("Failed to create signed expression");
        // Verify the expression with changed sorting
        assert!(
            signatures::verify(&signed_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression with string data should succeed"
        );
    }
}

