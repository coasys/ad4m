use deno_core::{op, error::AnyError, anyhow::anyhow};
use secp256k1::SecretKey;

use crate::wallet::Wallet;


fn secret_key_to_hex(secret_key: &SecretKey) -> String {
    let secret_key_bytes = secret_key.as_ref(); // Convert SecretKey to byte array
    let secret_key_hex = hex::encode(secret_key_bytes); // Encode the byte array as a hex string
    secret_key_hex
}

#[op]
fn wallet_get_main_key() -> Result<String, AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    let key = wallet.get_secret_key("main".to_string()).ok_or_else(||anyhow!("main key not found"))?;
    Ok(secret_key_to_hex(&key))
}

#[op]
fn wallet_create_main_key() -> Result<(), AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    wallet.generate_keypair("main".to_string());
    Ok(())
}

#[op]
fn wallet_is_unlocked() -> Result<bool, AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    Ok(wallet.is_unlocked())
}

#[op]
fn wallet_unlock(passphrase: String) -> Result<(), AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    wallet.unlock(passphrase).map_err(|e| e.into())
}

#[op]
fn wallet_lock(passphrase: String) -> Result<(), AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    Ok(wallet.lock(passphrase))
}

#[op]
fn wallet_export(passphrase: String) -> Result<String, AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    Ok(wallet.export(passphrase))
}

#[op]
fn wallet_load(data: String) -> Result<(), AnyError> {
    let wallet = Wallet::instance()
        .lock()
        .expect("wallet lock")
        .as_ref()
        .expect("wallet instance");
    Ok(wallet.load(data))
}