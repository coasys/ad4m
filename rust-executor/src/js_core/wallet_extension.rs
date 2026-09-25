use base64::{engine::general_purpose as base64engine, Engine as _};
use deno_core::{anyhow::anyhow, op2};
use serde::{Deserialize, Serialize};

use crate::js_core::error::AnyhowWrapperError;
use crate::wallet::wallet_backend;

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Key {
    pub public_key: String,
    pub private_key: String,
    pub encoding: String,
}

#[op2]
#[serde]
fn wallet_get_main_key() -> Result<Key, AnyhowWrapperError> {
    let backend = wallet_backend();
    let name = "main";
    let public_key = backend
        .get_public_key(name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    let private_key = backend
        .get_secret_key(name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    Ok(Key {
        public_key: base64engine::STANDARD.encode(public_key),
        private_key: base64engine::STANDARD.encode(private_key),
        encoding: "base64".to_string(),
    })
}

#[op2]
#[serde]
fn wallet_get_main_key_document() -> Result<did_key::Document, AnyhowWrapperError> {
    let backend = wallet_backend();
    backend
        .get_did_document("main")
        .ok_or(AnyhowWrapperError::from(anyhow!(
            "main key not found. call createMainKey() first"
        )))
}

#[op2]
#[serde]
fn wallet_create_main_key() -> Result<(), AnyhowWrapperError> {
    let backend = wallet_backend();
    backend
        .generate_keypair("main")
        .map_err(AnyhowWrapperError::from)
}

#[op2(fast)]
fn wallet_is_unlocked() -> Result<bool, AnyhowWrapperError> {
    let backend = wallet_backend();
    Ok(backend.is_unlocked())
}

#[op2]
#[serde]
fn wallet_unlock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    let backend = wallet_backend();
    backend
        .unlock(&passphrase)
        .map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn wallet_lock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    let backend = wallet_backend();
    backend.lock(&passphrase);
    Ok(())
}

#[op2]
#[string]
fn wallet_export(#[string] passphrase: String) -> Result<String, AnyhowWrapperError> {
    let backend = wallet_backend();
    Ok(backend.export(&passphrase))
}

#[op2]
#[serde]
fn wallet_load(#[string] data: String) -> Result<(), AnyhowWrapperError> {
    let backend = wallet_backend();
    backend.load(&data);
    Ok(())
}

#[op2]
#[serde]
fn wallet_sign(#[buffer] payload: &[u8]) -> Result<Vec<u8>, AnyhowWrapperError> {
    crate::agent::sign(payload).map_err(AnyhowWrapperError::from)
}

deno_core::extension!(
    wallet_service,
    ops = [wallet_get_main_key, wallet_get_main_key_document, wallet_create_main_key, wallet_is_unlocked, wallet_unlock, wallet_lock, wallet_export, wallet_load, wallet_sign],
    esm_entry_point = "ext:wallet_service/wallet_extension.js",
    esm = [dir "src/js_core", "wallet_extension.js"]
);
