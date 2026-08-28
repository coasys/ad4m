use argon2::password_hash::Salt;
use argon2::{self, Argon2, PasswordHasher};
use base64::Engine;
use crypto_box::aead::Aead;
use crypto_box::{Nonce, PublicKey as cPublicKey, SalsaBox, SecretKey as cSecretKey};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, DIDCore, Ed25519KeyPair, KeyMaterial, PatchedKeyPair};
use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::sync::{Arc, Mutex, RwLock};

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

// ── WalletBackend trait ─────────────────────────────────────────────────────

/// Abstracts key operations so different wallet implementations can coexist.
/// `Send + Sync` required — accessed from multiple Deno worker threads.
pub trait WalletBackend: Send + Sync {
    /// Generate a new Ed25519 keypair and store under `name`.
    fn generate_keypair(&self, name: &str) -> Result<(), AnyError>;

    /// Retrieve the secret key bytes for `name`. None if not found.
    fn get_secret_key(&self, name: &str) -> Option<Vec<u8>>;

    /// Retrieve the public key bytes for `name`. None if not found.
    fn get_public_key(&self, name: &str) -> Option<Vec<u8>>;

    /// Retrieve the DID document for `name`. None if not found.
    fn get_did_document(&self, name: &str) -> Option<did_key::Document>;

    /// Sign `message` with the key named `name`. None if key not found.
    fn sign(&self, name: &str, message: &[u8]) -> Option<Vec<u8>>;

    /// List all key names in the backend.
    fn list_key_names(&self) -> Vec<String>;

    /// Check if a key with `name` exists.
    fn key_exists(&self, name: &str) -> bool;

    /// Downcast support for local-only operations (export, unlock, etc.).
    fn as_any(&self) -> &dyn Any;
}

// ── Global accessor ─────────────────────────────────────────────────────────

static WALLET_BACKEND: OnceCell<Arc<dyn WalletBackend>> = OnceCell::new();

/// Get the global wallet backend. Panics if not initialised.
pub fn wallet_backend() -> &'static Arc<dyn WalletBackend> {
    WALLET_BACKEND
        .get()
        .expect("wallet backend not initialised")
}

/// Initialise the global wallet backend. Panics if called twice.
pub fn init_wallet_backend(backend: Arc<dyn WalletBackend>) {
    if WALLET_BACKEND.set(backend).is_err() {
        panic!("wallet backend already initialised");
    }
}

/// Try to initialise the global wallet backend. Returns false if already set.
pub fn try_init_wallet_backend(backend: Arc<dyn WalletBackend>) -> bool {
    WALLET_BACKEND.set(backend).is_ok()
}

// ── LocalWallet ─────────────────────────────────────────────────────────────

/// In-process wallet that wraps the existing `Wallet` with interior mutability.
/// Default backend for standalone / self-hosted executors. Behaviour matches
/// the original `Wallet::instance()` singleton exactly.
pub struct LocalWallet {
    inner: Mutex<Wallet>,
}

impl Default for LocalWallet {
    fn default() -> Self {
        LocalWallet {
            inner: Mutex::new(Wallet::new()),
        }
    }
}

impl LocalWallet {
    pub fn new() -> Self {
        Self::default()
    }

    // ── Local-only operations (not on the trait) ────────────────────────

    /// Export the keystore encrypted with `passphrase`.
    pub fn export(&self, passphrase: &str) -> String {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.export(passphrase.to_string())
    }

    /// Load an encrypted keystore blob (decrypt later with `unlock`).
    pub fn load(&self, data: &str) {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.load(data.to_string());
    }

    /// Decrypt the keystore with `passphrase`, making keys available.
    pub fn unlock(&self, passphrase: &str) -> Result<(), AnyError> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.unlock(passphrase.to_string())
    }

    /// Encrypt and clear keys from memory.
    pub fn lock(&self, passphrase: &str) {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.lock(passphrase.to_string());
    }

    /// Check whether the keystore has been decrypted.
    pub fn is_unlocked(&self) -> bool {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.is_unlocked()
    }

    /// Import a DID's keys by resolving the DID string. Only succeeds if
    /// no keys have been loaded yet (same semantics as `Wallet::initialize_keys`).
    pub fn initialize_keys(&self, name: &str, did: &str) -> Option<did_key::Document> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.initialize_keys(name.to_string(), did.to_string())
    }
}

impl WalletBackend for LocalWallet {
    fn generate_keypair(&self, name: &str) -> Result<(), AnyError> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.generate_keypair(name.to_string());
        Ok(())
    }

    fn get_secret_key(&self, name: &str) -> Option<Vec<u8>> {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.get_secret_key(&name.to_string())
    }

    fn get_public_key(&self, name: &str) -> Option<Vec<u8>> {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.get_public_key(&name.to_string())
    }

    fn get_did_document(&self, name: &str) -> Option<did_key::Document> {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.get_did_document(&name.to_string())
    }

    fn sign(&self, name: &str, message: &[u8]) -> Option<Vec<u8>> {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.sign(&name.to_string(), message)
    }

    fn list_key_names(&self) -> Vec<String> {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.list_key_names()
    }

    fn key_exists(&self, name: &str) -> bool {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.get_did_document(&name.to_string()).is_some()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── SharedWallet ────────────────────────────────────────────────────────────

/// Cached key material fetched from the external wallet service.
struct CachedKey {
    secret: Vec<u8>,
    public: Vec<u8>,
    fetched_at: std::time::Instant,
}

/// TTL for cached key material (5 minutes).
const SHARED_WALLET_CACHE_TTL_SECS: u64 = 300;

/// Wallet backend that delegates key operations to an external HTTP service.
/// Used in the hosted platform where multiple executor instances share one
/// identity store.
///
/// Key material fetched over HTTP gets cached in-process with a 5-minute TTL.
/// Signing always happens locally — the secret key bytes come over the wire
/// but the actual Ed25519 sign operation runs in this process.
pub struct SharedWallet {
    base_url: String,
    client: reqwest::blocking::Client,
    cache: RwLock<std::collections::HashMap<String, CachedKey>>,
}

impl SharedWallet {
    pub fn new(base_url: String) -> Self {
        SharedWallet {
            base_url: base_url.trim_end_matches('/').to_string(),
            client: reqwest::blocking::Client::new(),
            cache: RwLock::new(std::collections::HashMap::new()),
        }
    }

    /// Fetch key material from the backend, populating the cache on success.
    fn fetch_and_cache(&self, name: &str) -> Option<(Vec<u8>, Vec<u8>)> {
        let url = format!("{}/keys/{}", self.base_url, name);
        let resp = self.client.get(&url).send().ok()?;
        if !resp.status().is_success() {
            return None;
        }
        let body: serde_json::Value = resp.json().ok()?;
        let secret_b64 = body.get("secret")?.as_str()?;
        let public_b64 = body.get("public")?.as_str()?;
        let secret = base64::engine::general_purpose::STANDARD
            .decode(secret_b64.as_bytes())
            .ok()?;
        let public = base64::engine::general_purpose::STANDARD
            .decode(public_b64.as_bytes())
            .ok()?;

        // Write-through to cache
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(
                name.to_string(),
                CachedKey {
                    secret: secret.clone(),
                    public: public.clone(),
                    fetched_at: std::time::Instant::now(),
                },
            );
        }
        Some((secret, public))
    }

    /// Get key material from cache (if fresh) or fetch from the backend.
    fn get_key_material(&self, name: &str) -> Option<(Vec<u8>, Vec<u8>)> {
        // Check cache first
        if let Ok(cache) = self.cache.read() {
            if let Some(entry) = cache.get(name) {
                let age = entry.fetched_at.elapsed().as_secs();
                if age < SHARED_WALLET_CACHE_TTL_SECS {
                    return Some((entry.secret.clone(), entry.public.clone()));
                }
            }
        }
        // Cache miss or expired — fetch from backend
        self.fetch_and_cache(name)
    }
}

impl WalletBackend for SharedWallet {
    fn generate_keypair(&self, name: &str) -> Result<(), AnyError> {
        let url = format!("{}/keys/{}", self.base_url, name);
        let resp = self
            .client
            .post(&url)
            .send()
            .map_err(|e| anyhow!("shared wallet: generate_keypair failed: {}", e))?;
        if !resp.status().is_success() {
            return Err(anyhow!(
                "shared wallet: generate_keypair returned {}",
                resp.status()
            ));
        }
        // Write-through: fetch the newly generated key into cache
        self.fetch_and_cache(name);
        Ok(())
    }

    fn get_secret_key(&self, name: &str) -> Option<Vec<u8>> {
        self.get_key_material(name).map(|(secret, _)| secret)
    }

    fn get_public_key(&self, name: &str) -> Option<Vec<u8>> {
        self.get_key_material(name).map(|(_, public)| public)
    }

    fn get_did_document(&self, name: &str) -> Option<did_key::Document> {
        let (secret, public) = self.get_key_material(name)?;
        let key_pair = did_key::from_existing_key::<Ed25519KeyPair>(&public, Some(&secret));
        Some(key_pair.get_did_document(did_key::Config::default()))
    }

    fn sign(&self, name: &str, message: &[u8]) -> Option<Vec<u8>> {
        let (secret, public) = self.get_key_material(name)?;
        let key_pair = did_key::from_existing_key::<Ed25519KeyPair>(&public, Some(&secret));
        Some(key_pair.sign(message))
    }

    fn list_key_names(&self) -> Vec<String> {
        let url = format!("{}/keys", self.base_url);
        let resp = match self.client.get(&url).send() {
            Ok(r) if r.status().is_success() => r,
            _ => return vec![],
        };
        resp.json::<Vec<String>>().unwrap_or_default()
    }

    fn key_exists(&self, name: &str) -> bool {
        // Check cache first
        if let Ok(cache) = self.cache.read() {
            if let Some(entry) = cache.get(name) {
                if entry.fetched_at.elapsed().as_secs() < SHARED_WALLET_CACHE_TTL_SECS {
                    return true;
                }
            }
        }
        // Fall back to HTTP
        let url = format!("{}/keys/{}/exists", self.base_url, name);
        self.client
            .get(&url)
            .send()
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }

    fn as_any(&self) -> &dyn Any {
        self
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

    // ── WalletBackend trait tests ───────────────────────────────────────

    #[test]
    fn test_local_wallet_generate_and_retrieve() {
        let local = LocalWallet::new();
        local
            .generate_keypair("alice")
            .expect("generate_keypair should succeed");

        assert!(local.key_exists("alice"));
        assert!(!local.key_exists("bob"));
        assert!(local.get_secret_key("alice").is_some());
        assert!(local.get_public_key("alice").is_some());
        assert!(local.get_secret_key("bob").is_none());
    }

    #[test]
    fn test_local_wallet_did_document() {
        let local = LocalWallet::new();
        local.generate_keypair("test").expect("generate");

        let doc = local.get_did_document("test");
        assert!(doc.is_some());
        let doc = doc.unwrap();
        assert!(doc.id.starts_with("did:key:"));
    }

    #[test]
    fn test_local_wallet_sign_verify_roundtrip() {
        let local = LocalWallet::new();
        local.generate_keypair("signer").expect("generate");

        let message = b"hello wallet backend";
        let sig = local.sign("signer", message);
        assert!(sig.is_some());

        let sig = sig.unwrap();
        let doc = local.get_did_document("signer").unwrap();
        let key_pair =
            PatchedKeyPair::try_from(doc.id.as_str()).expect("Failed to resolve key pair");
        assert!(key_pair.verify(message, &sig).is_ok());
    }

    #[test]
    fn test_local_wallet_list_key_names() {
        let local = LocalWallet::new();
        assert!(local.list_key_names().is_empty());

        local.generate_keypair("a").expect("generate");
        local.generate_keypair("b").expect("generate");

        let mut names = local.list_key_names();
        names.sort();
        assert_eq!(names, vec!["a", "b"]);
    }

    #[test]
    fn test_local_wallet_lock_unlock() {
        let local = LocalWallet::new();
        local.generate_keypair("main").expect("generate");
        assert!(local.is_unlocked());

        local.lock("passphrase");
        assert!(!local.is_unlocked());
        assert!(local.get_secret_key("main").is_none());

        local.unlock("passphrase").expect("unlock");
        assert!(local.is_unlocked());
        assert!(local.get_secret_key("main").is_some());
    }

    #[test]
    fn test_local_wallet_export_load() {
        let local = LocalWallet::new();
        local.generate_keypair("test").expect("generate");
        let exported = local.export("pass");
        assert!(!exported.is_empty());

        let local2 = LocalWallet::new();
        local2.load(&exported);
        local2.unlock("pass").expect("unlock");
        assert!(local2.key_exists("test"));
    }

    #[test]
    fn test_local_wallet_downcast() {
        let backend: Arc<dyn WalletBackend> = Arc::new(LocalWallet::new());
        let local = backend.as_any().downcast_ref::<LocalWallet>();
        assert!(local.is_some());
    }

    #[test]
    fn test_local_wallet_sign_nonexistent_key() {
        let local = LocalWallet::new();
        assert!(local.sign("missing", b"data").is_none());
    }
}
