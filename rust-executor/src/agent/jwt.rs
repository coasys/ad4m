use std::time::{SystemTime, UNIX_EPOCH};

use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};
use serde::{Deserialize, Serialize};
use juniper::GraphQLObject;
use crate::wallet::Wallet;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoExtended {
    pub request_id: String,
    auth: AuthInfo,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfo {
    pub app_name: String,
    pub app_desc: String,
    pub app_domain: Option<String>,
    pub app_url: Option<String>,
    pub app_icon_path: Option<String>,
    pub capabilities: Option<Vec<Capability>>,
}
#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Capability {
    pub with: Resource,
    pub can: Vec<String>,
}

#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    iss: String,
    aud: String,
    exp: u64,
    iat: u64,
    pub capabilities: AuthInfo,
}

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

    let now = SystemTime::now();
    let unix_timestamp = now
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs();

    let payload = Claims {
        iss: issuer,
        aud: audience,
        exp: unix_timestamp + expiration_time,
        iat: unix_timestamp,
        capabilities: capabilities,
    };

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
