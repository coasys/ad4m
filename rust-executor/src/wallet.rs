use argon2::password_hash::Salt;
use argon2::{self, Argon2, PasswordHasher};
use base64::Engine;
use crypto_box::aead::{Aead, OsRng};
use crypto_box::{Nonce, PublicKey as cPublicKey, SalsaBox, SecretKey as cSecretKey};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use lazy_static::lazy_static;
use secp256k1::ecdsa::Signature;
use secp256k1::Message;
use secp256k1::{PublicKey, SecretKey};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};

fn slice_to_u8_array(slice: &[u8]) -> Option<[u8; 32]> {
    //If length of slice is not 32 then take the first 32 bytes
    let slice_32 = if slice.len() != 32 {
        let mut array: [u8; 32] = [0u8; 32];
        let mut i = 0;
        for byte in slice {
            if i == 32 {
                break;
            }
            array[i] = *byte;
            i += 1;
        }
        array
    } else {
        let array: [u8; 32] = slice.try_into().expect("slice with incorrect length");
        array
    };

    let array: Result<[u8; 32], _> = slice_32.try_into();
    array.ok()
}

fn encrypt(payload: String, passphrase: String) -> String {
    let salt = Salt::from_b64("0000000000000000").expect("salt from zeros to work");

    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    //NOTE: we need to be sure to enforce min password size so we ensure that we will always get 32 bytes to work from
    let derived_secret_key = argon2
        .hash_password(passphrase.as_bytes(), salt)
        .unwrap()
        .to_string();
    let derived_secret_key_bytes = derived_secret_key.as_bytes();
    let secret_key = cSecretKey::from(
        slice_to_u8_array(derived_secret_key_bytes).expect("Could not slice to u8 array"),
    );
    let public_key = cPublicKey::from(&secret_key);

    // Create the Box (encryptor/decryptor) using the derived secret key and the public key
    let crypto_box = SalsaBox::new(&public_key, &secret_key);

    //let nonce = SalsaBox::generate_nonce(&mut OsRng);
    let nonce = Nonce::default();

    // Encrypt
    let encrypted_data = crypto_box.encrypt(&nonce, payload.as_bytes()).unwrap();

    base64::engine::general_purpose::STANDARD_NO_PAD.encode(encrypted_data)
}

fn decrypt(payload: String, passphrase: String) -> Result<String, crypto_box::aead::Error> {
    let salt = Salt::from_b64("0000000000000000").expect("salt from zeros to work");

    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    let derived_secret_key = argon2
        .hash_password(passphrase.as_bytes(), salt)
        .unwrap()
        .to_string();
    let derived_secret_key_bytes = derived_secret_key.as_bytes();
    let secret_key = cSecretKey::from(
        slice_to_u8_array(derived_secret_key_bytes).expect("Could not slice to u8 array"),
    );
    let public_key = cPublicKey::from(&secret_key);

    // Create the Box (encryptor/decryptor) using the derived secret key and the public key
    let crypto_box = SalsaBox::new(&public_key, &secret_key);

    //Pretty sure this not gonna work since this will be a different nonce to what is generated on encrypt
    let nonce = Nonce::default();

    let payload_bytes = base64::engine::general_purpose::STANDARD_NO_PAD
        .decode(payload.as_bytes())
        .expect("Could not decode payload");

    // Decrypt
    let decrypted_data = crypto_box
        .decrypt(&nonce, payload_bytes.as_slice())
        .map(|data| String::from_utf8(data).expect("decrypted array to be a string"));

    decrypted_data
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Key {
    pub secret: SecretKey,
    pub public: PublicKey,
}

#[derive(Serialize, Deserialize, Clone)]
struct Keys {
    keys: BTreeMap<String, Key>,
}

pub struct Wallet {
    cipher: Option<String>,
    keys: Option<BTreeMap<String, Key>>,
}

lazy_static! {
    static ref WALLET: Arc<Mutex<Option<Wallet>>> = Arc::new(Mutex::new(None));
}

impl Wallet {
    pub fn new() -> Self {
        Wallet {
            cipher: None,
            keys: None,
        }
    }

    pub fn instance() -> Arc<Mutex<Option<Wallet>>> {
        let wallet = unsafe { WALLET.clone() };
        {
            let mut w_lock = wallet.lock().unwrap();
            if w_lock.is_none() {
                *w_lock = Some(Wallet::new());
            }
        }
        wallet
    }

    pub fn generate_keypair(&mut self, name: String) {
        if self.keys.is_none() {
            self.keys = Some(BTreeMap::new());
        }
        let secp = secp256k1::Secp256k1::new();
        let (secret, public) = secp.generate_keypair(&mut OsRng);
        self.keys
            .clone()
            .unwrap()
            .insert(name, Key { secret, public });
    }

    pub fn get_public_key(&self, name: String) -> Option<PublicKey> {
        self.keys.clone()?.get(&name).map(|key| key.public)
    }

    pub fn get_secret_key(&self, name: String) -> Option<SecretKey> {
        self.keys.clone()?.get(&name).map(|key| key.secret)
    }

    pub fn sign(&self, name: String, message: &[u8]) -> Option<Signature> {
        // hash the message
        let message = Message::from_slice(message).expect("32 bytes");
        // sign the hash
        self.keys.clone()?.get(&name).map(|key| {
            let secp = secp256k1::Secp256k1::new();
            secp.sign_ecdsa(&message, &key.secret)
        })
    }

    pub fn lock(&mut self, passphrase: String) {
        if let Some(keys) = &self.keys {
            let string = serde_json::to_string(&keys).unwrap();
            let encrypted = encrypt(string, passphrase);
            self.cipher = Some(encrypted);
            self.keys = None;
        }
    }

    pub fn unlock(&mut self, passphrase: String) -> Result<(), AnyError> {
        let string = decrypt(self.cipher.clone().expect("No cypher selected"), passphrase)
            .map_err(|err| anyhow!(err))?;
        let keys: Keys = serde_json::from_str(&string)?;
        self.keys = Some(keys.keys);
        Ok(())
    }

    pub fn is_unlocked(&self) -> bool {
        self.keys.is_some()
    }

    pub fn export(&mut self, passphrase: String) -> String {
        if let Some(keys) = &self.keys {
            let string = serde_json::to_string(keys).unwrap();
            let encrypted = encrypt(string, passphrase);
            self.cipher = Some(encrypted.clone());
            encrypted
        } else {
            String::new()
        }
    }

    pub fn load(&mut self, data: String) {
        self.cipher = Some(data);
    }
}

#[cfg(test)]
mod tests {
    //Test the encryption and decryption of a string
    use super::*;

    #[test]
    fn test_encrypt_decrypt_multiple() {
        let passphrase = "test".to_string();
        let payload = "test".to_string();
        let encrypted = encrypt(payload.clone(), passphrase.clone());
        println!("Got encrypted: {}", encrypted);
        let decrypted = decrypt(encrypted, passphrase);
        println!("Got decrypted: {:?}", decrypted);
        assert_eq!(payload, decrypted.unwrap());

        let passphrase = "test".to_string();
        let payload = "test".to_string();
        let encrypted = encrypt(payload.clone(), passphrase.clone());
        println!("Got encrypted: {}", encrypted);
        let decrypted = decrypt(encrypted, passphrase);
        println!("Got decrypted: {:?}", decrypted);
        assert_eq!(payload, decrypted.unwrap());
    }
}
