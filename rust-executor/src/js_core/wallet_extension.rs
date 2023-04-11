use deno_core::{anyhow::anyhow, error::AnyError, op, Extension, include_js_files};
use secp256k1::SecretKey;
use base64::{Engine as _, engine::{self, general_purpose as base64engine}};
use serde::{Deserialize, Serialize};

use crate::wallet::Wallet;

fn secret_key_to_hex(secret_key: &SecretKey) -> String {
    let secret_key_bytes = secret_key.as_ref(); // Convert SecretKey to byte array
    let secret_key_hex = hex::encode(secret_key_bytes); // Encode the byte array as a hex string
    secret_key_hex
}


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
    let public_key = wallet_ref.get_public_key("main".into()).ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    let private_key = wallet_ref.get_secret_key("main".into()).ok_or(anyhow!("main key not found. call createMainKey() first"))?;
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
    wallet_ref.get_did_document("main".into()).ok_or(anyhow!("main key not found. call createMainKey() first"))
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
        ])
        .build()
}