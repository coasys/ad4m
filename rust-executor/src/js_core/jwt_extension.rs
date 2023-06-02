use std::time::{SystemTime, UNIX_EPOCH};

use deno_core::{anyhow::anyhow, error::AnyError, include_js_files, op, Extension};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};
use serde::{Deserialize, Serialize};

use crate::wallet::Wallet;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoExtended {
    request_id: String,
    auth: AuthInfo,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfo {
    app_name: String,
    app_desc: String,
    app_domain: Option<String>,
    app_url: Option<String>,
    app_icon_path: Option<String>,
    capabilities: Option<Vec<Capability>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Capability {
    with: Resource,
    can: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Resource {
    domain: String,
    pointers: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    iss: String,
    aud: String,
    exp: u64,
    iat: u64,
    capabilities: AuthInfo,
}

#[op]
async fn generate_jwt(
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

#[op]
async fn verify_jwt(token: String) -> Result<Claims, AnyError> {
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

pub fn build() -> Extension {
    Extension::builder("jwt")
        .js(include_js_files!(jwt "jwt_extension.js",))
        .ops(vec![generate_jwt::decl(), verify_jwt::decl()])
        .force_op_registration()
        .build()
}
