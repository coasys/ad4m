use super::types::*;
use crate::wallet::Wallet;
use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};

pub fn generate_jwt(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<String, AnyError> {
    // Get the private key
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");

    if !wallet_ref.is_unlocked() {
        return Err(anyhow!("Wallet is locked. The agent must be unlocked (agentUnlock) before generating JWTs."));
    }

    let name = "main".to_string();

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main signing key not found. Agent may not have been initialized (agentGenerate)."))?;

    let did_document = wallet_ref
        .get_did_document(&name)
        .ok_or(anyhow!("main DID document not found. Agent may not have been initialized (agentGenerate)."))?;

    let payload = Claims::new(did_document.id, audience, expiration_time, capabilities);

    let token = encode(
        &Header::default(),
        &payload,
        &EncodingKey::from_secret(secret_key.as_slice()),
    )?;

    Ok(token)
}

pub fn decode_jwt(token: String) -> Result<Claims, AnyError> {
    //Get the private key
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");
    let name = "main".to_string();

    if !wallet_ref.is_unlocked() {
        return Err(anyhow!("Wallet is locked. The agent must be unlocked (agentUnlock) before decoding JWTs."));
    }

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main signing key not found. Agent may not have been initialized (agentGenerate)."))?;

    let result = jsonwebtoken::decode::<Claims>(
        &token,
        &DecodingKey::from_secret(secret_key.as_slice()),
        &jsonwebtoken::Validation::new(Algorithm::HS256),
    )?;

    Ok(result.claims)
}
