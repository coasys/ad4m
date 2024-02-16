use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};
use crate::wallet::Wallet;
use super::types::*;

pub fn generate_jwt(
    issuer: String,
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<String, AnyError> {
    // Get the private key
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");
    let name = "main".to_string();

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    let payload = Claims::new(issuer, audience, expiration_time, capabilities);

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

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    let result = jsonwebtoken::decode::<Claims>(
        &token,
        &DecodingKey::from_secret(secret_key.as_slice()),
        &jsonwebtoken::Validation::new(Algorithm::HS256),
    )?;

    Ok(result.claims)
}