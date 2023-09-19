use std::borrow::Cow;

use base64::{engine::general_purpose as base64engine, Engine as _};
use deno_core::{anyhow::anyhow, error::AnyError, include_js_files, op, Extension, Op};
use did_key::{CoreSign, PatchedKeyPair};
use log::error;
use serde::{Deserialize, Serialize};

use crate::wallet::Wallet;

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Key {
    pub public_key: String,
    pub private_key: String,
    pub encoding: String,
}

#[op]
fn wallet_get_main_key() -> Result<Key, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    let public_key = wallet_ref
        .get_public_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    let private_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    Ok(Key {
        public_key: base64engine::STANDARD.encode(public_key),
        private_key: base64engine::STANDARD.encode(private_key),
        encoding: "base64".to_string(),
    })
}

#[op]
fn wallet_get_main_key_document() -> Result<did_key::Document, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    wallet_ref
        .get_did_document(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))
}

#[op]
fn wallet_create_main_key() -> Result<(), AnyError> {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    wallet_ref.generate_keypair("main".to_string());
    Ok(())
}

#[op]
fn wallet_is_unlocked() -> Result<bool, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    Ok(wallet_ref.is_unlocked())
}

#[op]
fn wallet_unlock(passphrase: String) -> Result<(), AnyError> {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    wallet_ref.unlock(passphrase).map_err(|e| e.into())
}

#[op]
fn wallet_lock(passphrase: String) -> Result<(), AnyError> {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    Ok(wallet_ref.lock(passphrase))
}

#[op]
fn wallet_export(passphrase: String) -> Result<String, AnyError> {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    Ok(wallet_ref.export(passphrase))
}

#[op]
fn wallet_load(data: String) -> Result<(), AnyError> {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    Ok(wallet_ref.load(data))
}

#[op]
fn wallet_sign(payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    let signature = wallet_ref
        .sign(&name, payload)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    Ok(signature)
}

#[op]
fn wallet_verify(did: String, message: &[u8], signature: &[u8]) -> bool {
    if let Ok(key_pair) = PatchedKeyPair::try_from(did.as_str()) {
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

pub fn build() -> Extension {
    Extension {
        name: "wallet",
        js_files: Cow::Borrowed(&include_js_files!(holochain_service "src/js_core/wallet_extension.js",)),
        ops: Cow::Borrowed(&[
            wallet_get_main_key::DECL,
            wallet_get_main_key_document::DECL,
            wallet_create_main_key::DECL,
            wallet_is_unlocked::DECL,
            wallet_unlock::DECL,
            wallet_lock::DECL,
            wallet_export::DECL,
            wallet_load::DECL,
            wallet_sign::DECL,
            wallet_verify::DECL,
        ]),
        ..Default::default()
    }
}
