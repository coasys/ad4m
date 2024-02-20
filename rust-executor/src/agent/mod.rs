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

pub fn create_signed_expression(data: serde_json::Value) -> Result<Expression, Box<dyn std::error::Error>> {
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