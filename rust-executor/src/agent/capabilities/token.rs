use super::types::*;
use crate::config::get_global_config;
use crate::wallet::wallet_backend;
use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};

/// Resolve the signing key name from global config.
pub fn signing_key_name() -> String {
    get_global_config().signing_key_name()
}

pub fn generate_jwt(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<String, AnyError> {
    let backend = wallet_backend();
    let name = signing_key_name();

    let secret_key = backend.get_secret_key(&name).ok_or(anyhow!(
        "{} key not found. call createMainKey() first",
        name
    ))?;

    let did_document = backend.get_did_document(&name).ok_or(anyhow!(
        "{} did not found. call createMainKey() first",
        name
    ))?;

    let payload = Claims::new(did_document.id, audience, expiration_time, capabilities);

    let token = encode(
        &Header::default(),
        &payload,
        &EncodingKey::from_secret(secret_key.as_slice()),
    )?;

    Ok(token)
}

pub fn decode_jwt(token: String) -> Result<Claims, AnyError> {
    let backend = wallet_backend();
    let name = signing_key_name();

    let secret_key = backend
        .get_secret_key(&name)
        .ok_or(anyhow!("{} key not found", name))?;

    let result = jsonwebtoken::decode::<Claims>(
        &token,
        &DecodingKey::from_secret(secret_key.as_slice()),
        &jsonwebtoken::Validation::new(Algorithm::HS256),
    )?;

    Ok(result.claims)
}
