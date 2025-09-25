use argon2::password_hash::Salt;
use argon2::{self, Argon2, PasswordHasher};
use base64::Engine;
use crypto_box::aead::Aead;
use crypto_box::{Nonce, PublicKey as cPublicKey, SalsaBox, SecretKey as cSecretKey};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, DIDCore, Ed25519KeyPair, KeyMaterial, PatchedKeyPair};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};

fn slice_to_u8_array(slice: &[u8]) -> [u8; 32] {
    //If length of slice is not 32 then take the first 32 bytes

    if slice.len() != 32 {
        let mut array: [u8; 32] = [0u8; 32];
        let _i = 0;
        for (i, byte) in slice.iter().enumerate() {
            if i == 32 {
                break;
            }
            array[i] = *byte;
        }
        array
    } else {
        let array: [u8; 32] = slice.try_into().expect("slice with incorrect length");
        array
    }
}

fn padded(passphrase: String) -> String {
    let mut passphrase = passphrase.clone();
    while passphrase.len() < 32 {
        passphrase.push(' ');
    }
    passphrase
}

fn encrypt(payload: String, passphrase: String) -> String {
    let passphrase = padded(passphrase);
    let b64_passphrase =
        base64::engine::general_purpose::STANDARD_NO_PAD.encode(passphrase.as_bytes());
    let salt = Salt::from_b64(&b64_passphrase).expect("salt from passphrase to work");

    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    //NOTE: we need to be sure to enforce min password size so we ensure that we will always get 32 bytes to work from
    let derived_secret_key = argon2
        .hash_password(passphrase.as_bytes(), salt)
        .unwrap()
        .to_string();

    let preambel = "$argon2id$v=19$m=19456,t=2,p=1$";
    let derived_secret_key = derived_secret_key.replace(preambel, "");

    let derived_secret_key_bytes = derived_secret_key.as_bytes();
    let slice = slice_to_u8_array(derived_secret_key_bytes);
    let secret_key = cSecretKey::from(slice);
    let public_key = cPublicKey::from(&secret_key);

    // Create the Box (encryptor/decryptor) using the derived secret key and the public key
    let crypto_box = SalsaBox::new(&public_key, &secret_key);

    //let nonce = SalsaBox::generate_nonce(&mut OsRng);
    //let nonce: GenericArray<u8, _> = [0u8; 24].into();
    let nonce = Nonce::default();

    // Encrypt
    let encrypted_data = crypto_box.encrypt(&nonce, payload.as_bytes()).unwrap();

    base64::engine::general_purpose::STANDARD_NO_PAD.encode(encrypted_data)
}

fn decrypt(payload: String, passphrase: String) -> Result<String, crypto_box::aead::Error> {
    let passphrase = padded(passphrase);
    let b64_passphrase =
        base64::engine::general_purpose::STANDARD_NO_PAD.encode(passphrase.as_bytes());
    let salt = Salt::from_b64(&b64_passphrase).expect("salt from passphrase to work");

    // Derive secret key from passphrase
    let argon2 = Argon2::default();
    let derived_secret_key = argon2
        .hash_password(passphrase.as_bytes(), salt)
        .unwrap()
        .to_string();

    let preambel = "$argon2id$v=19$m=19456,t=2,p=1$";
    let derived_secret_key = derived_secret_key.replace(preambel, "");
    let derived_secret_key_bytes = derived_secret_key.as_bytes();
    let slice = slice_to_u8_array(derived_secret_key_bytes);
    let secret_key = cSecretKey::from(slice);
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

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Key {
    pub secret: Vec<u8>,
    pub public: Vec<u8>,
}

impl Key {
    pub fn from(did: PatchedKeyPair) -> Key {
        Key {
            secret: did.private_key_bytes(),
            public: did.public_key_bytes(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Keys {
    pub by_name: BTreeMap<String, Key>,
}

impl Keys {
    pub fn new() -> Self {
        Keys {
            by_name: BTreeMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Wallet {
    cipher: Option<String>,
    keys: Option<Keys>,
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
        let wallet = WALLET.clone();
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
            self.keys = Some(Keys::new());
        }

        let key = did_key::generate::<Ed25519KeyPair>(None);
        self.keys
            .as_mut()
            .unwrap()
            .by_name
            .insert(name, Key::from(key));
    }

    pub fn initialize_keys(&mut self, name: String, did: String) -> Option<did_key::Document> {
        if self.keys.is_none() {
            self.keys = Some(Keys::new());
            let key = did_key::resolve(did.as_str()).expect("Failed to get key pair");
            self.keys
                .as_mut()
                .unwrap()
                .by_name
                .insert(name.clone(), Key::from(key));
            let key = did_key::resolve(did.as_str()).expect("Failed to get key pair");
            let did_document = key.get_did_document(did_key::Config::default());
            Some(did_document)
        } else {
            None
        }
    }

    pub fn get_public_key(&self, name: &String) -> Option<Vec<u8>> {
        self.keys
            .as_ref()?
            .by_name
            .get(name)
            .map(|key| key.public.clone())
    }

    pub fn get_secret_key(&self, name: &String) -> Option<Vec<u8>> {
        self.keys
            .as_ref()?
            .by_name
            .get(name)
            .map(|key| key.secret.clone())
    }

    pub fn get_did_document(&self, name: &String) -> Option<did_key::Document> {
        self.keys.as_ref()?.by_name.get(name).map(|key| {
            let key = did_key::from_existing_key::<Ed25519KeyPair>(
                &key.public.clone(),
                Some(&key.secret.clone()),
            );
            key.get_did_document(did_key::Config::default())
        })
    }

    pub fn sign(&self, name: &String, message: &[u8]) -> Option<Vec<u8>> {
        self.keys.as_ref()?.by_name.get(name).map(|key| {
            let key = did_key::from_existing_key::<Ed25519KeyPair>(
                &key.public.clone(),
                Some(&key.secret.clone()),
            );
            key.sign(message)
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
        self.keys = Some(keys);
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

    pub fn list_key_names(&self) -> Vec<String> {
        self.keys
            .as_ref()
            .map(|keys| keys.by_name.keys().cloned().collect())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    //Test the encryption and decryption of a string
    use super::*;

    #[test]
    fn test_slice_to_u8_array() {
        let slice: &[u8] = &[
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31, 32,
        ];
        let result = slice_to_u8_array(slice);
        assert_eq!(slice, &result);

        let slice_short: &[u8] = &[1, 2, 3];
        let result = slice_to_u8_array(slice_short);
        let expected: [u8; 32] = [
            1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ];
        assert_eq!(expected, result);
    }

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

    #[test]
    fn test_encrypt_decrypt_wrong_passphrase() {
        let passphrase = "test_passphrase".to_string();
        let wrong_passphrase = "wrong_passphrase".to_string();
        let payload = "test_payload".to_string();
        let encrypted = encrypt(payload.clone(), passphrase.clone());
        println!("Got encrypted: {}", encrypted);
        assert_ne!(payload, encrypted);
        let decrypted = decrypt(encrypted, wrong_passphrase);
        assert!(decrypted.is_err());
    }

    #[test]
    fn test_create_and_get_key() {
        let mut wallet = Wallet::new();
        let name = "test".to_string();

        wallet.generate_keypair(name.clone());

        assert!(wallet.keys.is_some());
        assert!(wallet.keys.clone().unwrap().by_name.contains_key(&name));
        assert!(wallet.get_public_key(&name).is_some());
        assert!(wallet.get_secret_key(&name).is_some());
    }

    #[test]
    fn test_wallet_did_document_generation() {
        let mut wallet = Wallet::new();
        let key_name = "test_key".to_string();

        wallet.generate_keypair(key_name.clone());
        let did_document = wallet.get_did_document(&key_name);

        assert!(did_document.is_some());
    }

    #[test]
    fn test_wallet_signing() {
        let mut wallet = Wallet::new();
        let key_name = "test_key".to_string();
        let message = b"test message";

        wallet.generate_keypair(key_name.clone());
        let signature = wallet.sign(&key_name, message);

        assert!(signature.is_some());
    }

    #[test]
    fn test_wallet_lock_unlock() {
        let mut wallet = Wallet::new();
        let passphrase = "test_passphrase".to_string();
        let key_name = "test_key".to_string();

        wallet.generate_keypair(key_name.clone());
        wallet.lock(passphrase.clone());
        assert!(!wallet.is_unlocked());
        let unlock_result = wallet.unlock(passphrase.clone());
        assert!(unlock_result.is_ok());
        assert!(wallet.is_unlocked());
    }

    #[test]
    fn test_wallet_lock_unlock_wrong_passphrase() {
        let mut wallet = Wallet::new();
        let passphrase = "test_passphrase".to_string();
        let wrong_passphrase = "wrong_passphrase".to_string();
        let key_name = "test_key".to_string();

        wallet.generate_keypair(key_name.clone());
        wallet.lock(passphrase.clone());
        assert!(!wallet.is_unlocked());
        let unlock_result = wallet.unlock(wrong_passphrase);
        assert!(unlock_result.is_err());
        assert!(!wallet.is_unlocked());
    }

    #[test]
    fn test_wallet_export_and_load() {
        let mut wallet = Wallet::new();
        let passphrase = "test_passphrase".to_string();
        let key_name = "test_key".to_string();

        wallet.generate_keypair(key_name.clone());
        let exported_data = wallet.export(passphrase.clone());
        assert!(!exported_data.is_empty());

        let mut new_wallet = Wallet::new();
        new_wallet.load(exported_data);
        let unlock_result = new_wallet.unlock(passphrase);
        assert!(unlock_result.is_ok());
        assert!(new_wallet.is_unlocked());
    }

    #[test]
    fn test_did_sign_and_verify() {
        let mut wallet = Wallet::new();
        let key_name = "test_key".to_string();

        wallet.generate_keypair(key_name.clone());
        let did_document = wallet.get_did_document(&key_name);
        assert!(did_document.is_some());

        let did = did_document.unwrap().id;

        let message = b"test message";
        let signature = wallet.sign(&key_name, message);
        assert!(signature.is_some());

        let mut signature = signature.unwrap();
        {
            let sig_bytes = signature.as_slice();
            let key_pair = PatchedKeyPair::try_from(did.as_str()).expect("Failed to get key pair");
            let result = key_pair.verify(message, sig_bytes);
            assert!(result.is_ok());
        }

        signature[0] = 0;
        {
            let sig_bytes = signature.as_slice();
            let key_pair = PatchedKeyPair::try_from(did.as_str()).expect("Failed to get key pair");
            let result = key_pair.verify(message, sig_bytes);
            assert!(result.is_err());
        }
    }
}
