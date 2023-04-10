use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use secp256k1::ecdsa::Signature;
use secp256k1::{
    PublicKey, SecretKey,
};
use secp256k1::rand::rngs::OsRng;
use secp256k1::{Message};
use lazy_static::lazy_static;
use crypto_box::{SalsaBox, SecretKey as cSecretKey, PublicKey as cPublicKey};
use argon2::{self, PasswordHasher, Argon2};
use argon2::password_hash::{Salt};
use rand::Rng;
use serde::{Serialize, Deserialize};
use serde_json::Result;
use crypto_box::aead::{Aead, Nonce};

fn encrypt(payload: String, passphrase: String) -> String {
    let salt = Salt::from_b64("0000000000000000").expect("salt from zeros to work");
    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    let derived_secret_key = argon2.hash_password(passphrase.as_bytes(), salt).unwrap();
    let derived_secret_key_string = derived_secret_key.to_string().as_str();
    let secret_key = cSecretKey::from(derived_secret_key_string.as_bytes().clone().take(...10).unwrap().into());
    let public_key = cPublicKey::from(secret_key.into());

    // Create the Box (encryptor/decryptor) using the derived secret key and the public key
    let crypto_box = SalsaBox::new(&public_key, &secret_key);

    let nonce = Nonce::from("000000000".into());
    // Encrypt
    let encrypted_data = crypto_box.encrypt(&nonce, payload.into()).unwrap();

    String::from_utf8(encrypted_data).expect("encrypted array to be a string")
}

fn decrypt(payload: String, passphrase: String) -> Result<String> {
    // Salt for Argon2 key derivation function
    let mut salt = [0u8; 16];
    let mut rng = rand::thread_rng();
    rng.fill(&mut salt);

    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    let derived_secret_key = argon2.hash_password(passphrase.as_bytes(), &salt).unwrap();
    let secret_key = cSecretKey::from_slice(&derived_secret_key[0..32]).unwrap();
    let public_key = cPublicKey::from(secret_key);

    // Create the Box (encryptor/decryptor) using the derived secret key and the public key
    let crypto_box = SalsaBox::new(&public_key, &secret_key);

    // Decrypt
    let decrypted_data = crypto_box.decrypt(&payload);

    decrypted_data
}


pub struct Key {
    pub secret: SecretKey,
    pub public: PublicKey,
}

#[derive(Serialize, Deserialize)]
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
        self.keys.unwrap().insert(name, Key { secret, public });
    }

    pub fn get_public_key(&self, name: String) -> Option<PublicKey> {
        self.keys?.get(&name).map(|key| key.public)
    }

    pub fn get_secret_key(&self, name: String) -> Option<SecretKey> {
        self.keys?.get(&name).map(|key| key.secret)
    }

    pub fn sign(&self, name: String, message: &[u8]) -> Option<Signature> {
        // hash the message
        let message = Message::from_slice(message).expect("32 bytes");
        // sign the hash
        self.keys?.get(&name).map(|key| {
            let secp = secp256k1::Secp256k1::new();
            secp.sign_ecdsa(&message, &key.secret)
        })
    }

    pub fn lock(&mut self, passphrase: String) {
        if let Some(&keys) = self.keys {
            let string = serde_json::to_string(&keys).unwrap();
            let encrypted = encrypt(string, passphrase);
            self.cipher = Some(encrypted);
            self.keys = None;
        }
    }

    pub fn unlock(&mut self, passphrase: String) -> Result<()> {
        let string = decrypt(self.cipher, passphrase)?;
        let keys: Keys = serde_json::from_str(&string)?;
        self.keys = Some(keys);
        Ok(())
    }

    pub fn is_unlocked(&self) -> bool {
        self.keys.is_some()
    }

    pub fn export(&mut self, passphrase: String) -> String {
        if let Some(&keys) = self.keys {
            let string = serde_json::to_string(&keys).unwrap();
            let encrypted = encrypt(string, passphrase);
            self.cipher = Some(encrypted);
            encrypted
        } else {
            String::new()
        }
    }

    pub fn load(&mut self, data: String) {
        self.cipher = Some(data);
    }

}