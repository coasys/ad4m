use base64::{engine::general_purpose as base64engine, Engine as _};
use deno_core::{anyhow::anyhow, error::AnyError, include_js_files, op, Extension};
use did_key::{Ed25519KeyPair, PatchedKeyPair, CoreSign};
use log::{error, info};
use serde::{Deserialize, Serialize};

use crate::wallet::Wallet;

#[derive(Serialize, Deserialize, Clone)]
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
    info!("Verifying signature for DID: {}", did);
    info!("Message: {:?}", message);
    info!("Signature: {:?}", signature);
    if let Ok(key_pair) = PatchedKeyPair::try_from(did.as_str()) {
        info!("got key");
        match key_pair.verify(message, signature) {
            Ok(_) => {
                info!("Signature verified");
                true
            }
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
    Extension::builder("wallet")
        .js(include_js_files!(wallet "wallet_extension.js",))
        .ops(vec![
            wallet_get_main_key::decl(),
            wallet_get_main_key_document::decl(),
            wallet_create_main_key::decl(),
            wallet_is_unlocked::decl(),
            wallet_unlock::decl(),
            wallet_lock::decl(),
            wallet_export::decl(),
            wallet_load::decl(),
            wallet_sign::decl(),
            wallet_verify::decl(),
        ])
        .force_op_registration()
        .build()
}
