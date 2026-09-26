use argon2::{self, Argon2};
use base64::Engine;
use chacha20poly1305::aead::{Aead, KeyInit};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, DIDCore, Ed25519KeyPair, KeyMaterial, PatchedKeyPair};
use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use rand::rngs::OsRng;
use rand::RngCore;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, RwLock};
use zeroize::Zeroizing;

/// On-disk keystore format, version 2.
///
/// Argon2id derives the key from the whole passphrase and a random salt, and every write
/// draws a fresh random nonce for XChaCha20-Poly1305. The KDF parameters travel with the
/// ciphertext, so they can change later without breaking existing files.
#[derive(Serialize, Deserialize)]
struct KeystoreEnvelope {
    v: u8,
    kdf: String,
    m: u32,
    t: u32,
    p: u32,
    salt: String,
    nonce: String,
    ct: String,
}

const KEYSTORE_VERSION: u8 = 2;
// Argon2id with the argon2 crate's defaults, OWASP's minimum: 19 MiB, 2 passes, 1 lane.
// The legacy format paid exactly this cost on every save, without using the result, so
// the fix changes no node's memory or CPU profile. The envelope records the parameters,
// so they can rise later without breaking existing files.
const KDF_MEMORY_KIB: u32 = 19 * 1024;
const KDF_PASSES: u32 = 2;
const KDF_LANES: u32 = 1;
// Upper bounds for parameters read from a file, so a tampered file cannot stall unlock.
const KDF_MEMORY_KIB_MAX: u32 = 1024 * 1024;
const KDF_PASSES_MAX: u32 = 16;
const KDF_LANES_MAX: u32 = 16;

const B64: base64::engine::GeneralPurpose = base64::engine::general_purpose::STANDARD_NO_PAD;

fn derive_key(
    passphrase: &str,
    salt: &[u8],
    m: u32,
    t: u32,
    p: u32,
) -> Result<Zeroizing<[u8; 32]>, AnyError> {
    let params = argon2::Params::new(m, t, p, Some(32))
        .map_err(|e| anyhow!("invalid keystore KDF parameters: {}", e))?;
    let mut key = Zeroizing::new([0u8; 32]);
    Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params)
        .hash_password_into(passphrase.as_bytes(), salt, key.as_mut())
        .map_err(|e| anyhow!("keystore key derivation failed: {}", e))?;
    Ok(key)
}

/// Encrypt the serialised keystore under `passphrase`, in format version 2.
fn encrypt(payload: &str, passphrase: &str) -> String {
    let mut salt = [0u8; 16];
    let mut nonce = [0u8; 24];
    OsRng.fill_bytes(&mut salt);
    OsRng.fill_bytes(&mut nonce);
    let key = derive_key(passphrase, &salt, KDF_MEMORY_KIB, KDF_PASSES, KDF_LANES)
        .expect("the fixed KDF parameters are valid");
    let ct = XChaCha20Poly1305::new(key.as_ref().into())
        .encrypt(XNonce::from_slice(&nonce), payload.as_bytes())
        .expect("XChaCha20-Poly1305 encryption does not fail");
    serde_json::to_string(&KeystoreEnvelope {
        v: KEYSTORE_VERSION,
        kdf: "argon2id".to_string(),
        m: KDF_MEMORY_KIB,
        t: KDF_PASSES,
        p: KDF_LANES,
        salt: B64.encode(salt),
        nonce: B64.encode(nonce),
        ct: B64.encode(ct),
    })
    .expect("the keystore envelope serialises")
}

/// Decrypt a keystore written by [`encrypt`], or one in the legacy format.
fn decrypt(cipher: &str, passphrase: &str) -> Result<Zeroizing<String>, AnyError> {
    if is_legacy_keystore(cipher) {
        return legacy::decrypt(cipher, passphrase);
    }
    let envelope: KeystoreEnvelope =
        serde_json::from_str(cipher).map_err(|e| anyhow!("unreadable keystore: {}", e))?;
    if envelope.v != KEYSTORE_VERSION || envelope.kdf != "argon2id" {
        return Err(anyhow!(
            "unsupported keystore format: version {}, KDF {}",
            envelope.v,
            envelope.kdf
        ));
    }
    if envelope.m > KDF_MEMORY_KIB_MAX || envelope.t > KDF_PASSES_MAX || envelope.p > KDF_LANES_MAX
    {
        return Err(anyhow!(
            "keystore KDF parameters exceed the allowed maximum"
        ));
    }
    let salt = B64
        .decode(&envelope.salt)
        .map_err(|e| anyhow!("unreadable keystore salt: {}", e))?;
    let nonce = B64
        .decode(&envelope.nonce)
        .map_err(|e| anyhow!("unreadable keystore nonce: {}", e))?;
    if nonce.len() != 24 {
        return Err(anyhow!("the keystore nonce must hold 24 bytes"));
    }
    let ct = B64
        .decode(&envelope.ct)
        .map_err(|e| anyhow!("unreadable keystore ciphertext: {}", e))?;
    let key = derive_key(passphrase, &salt, envelope.m, envelope.t, envelope.p)?;
    let mut plain = Zeroizing::new(
        XChaCha20Poly1305::new(key.as_ref().into())
            .decrypt(XNonce::from_slice(&nonce), ct.as_slice())
            .map_err(|_| anyhow!("wrong passphrase or corrupt keystore"))?,
    );
    String::from_utf8(std::mem::take(&mut *plain))
        .map(Zeroizing::new)
        .map_err(|_| anyhow!("the keystore does not hold UTF-8 text"))
}

/// True for a keystore in the legacy format, which nothing writes any more.
/// The next save after an unlock rewrites it in format version 2.
pub fn is_legacy_keystore(cipher: &str) -> bool {
    !cipher.trim().is_empty() && !cipher.trim_start().starts_with('{')
}

/// Re-encrypts a keystore in the legacy format, so tests can reproduce files that older
/// executors left behind.
#[cfg(test)]
pub(crate) fn reencrypt_as_legacy(cipher: &str, passphrase: &str) -> String {
    let plain = decrypt(cipher, passphrase).expect("the test keystore decrypts");
    legacy::encrypt(&plain, passphrase)
}

/// Keystore format 1, kept only to read existing files. Its key came from the first 24
/// bytes of the passphrase alone (the Argon2 output never reached it), and every write
/// reused a zero nonce.
mod legacy {
    use super::B64;
    use argon2::password_hash::Salt;
    use argon2::{Argon2, PasswordHasher};
    use base64::Engine;
    use crypto_box::aead::Aead;
    use crypto_box::{Nonce, PublicKey, SalsaBox, SecretKey};
    use deno_core::anyhow::anyhow;
    use deno_core::error::AnyError;
    use zeroize::Zeroizing;

    pub(super) fn slice_to_u8_array(slice: &[u8]) -> [u8; 32] {
        let mut array = [0u8; 32];
        for (i, byte) in slice.iter().take(32).enumerate() {
            array[i] = *byte;
        }
        array
    }

    /// Pads to 32 bytes (not characters), exactly as the legacy writer did.
    fn padded(passphrase: &str) -> String {
        let mut padded = passphrase.to_string();
        while padded.len() < 32 {
            padded.push(' ');
        }
        padded
    }

    fn salsa_box(passphrase: &str) -> Result<SalsaBox, AnyError> {
        let passphrase = padded(passphrase);
        let b64_passphrase = B64.encode(passphrase.as_bytes());
        // Fails for passphrases over 48 bytes; the legacy writer never produced such a file.
        let salt = Salt::from_b64(&b64_passphrase)
            .map_err(|_| anyhow!("wrong passphrase or corrupt keystore"))?;
        let derived = Argon2::default()
            .hash_password(passphrase.as_bytes(), salt)
            .map_err(|e| anyhow!("legacy keystore key derivation failed: {}", e))?
            .to_string()
            .replace("$argon2id$v=19$m=19456,t=2,p=1$", "");
        let secret_key = SecretKey::from(slice_to_u8_array(derived.as_bytes()));
        let public_key = PublicKey::from(&secret_key);
        Ok(SalsaBox::new(&public_key, &secret_key))
    }

    pub(super) fn decrypt(cipher: &str, passphrase: &str) -> Result<Zeroizing<String>, AnyError> {
        let bytes = B64
            .decode(cipher.as_bytes())
            .map_err(|e| anyhow!("unreadable legacy keystore: {}", e))?;
        let plain = salsa_box(passphrase)?
            .decrypt(&Nonce::default(), bytes.as_slice())
            .map_err(|_| anyhow!("wrong passphrase or corrupt keystore"))?;
        String::from_utf8(plain)
            .map(Zeroizing::new)
            .map_err(|_| anyhow!("the keystore does not hold UTF-8 text"))
    }

    /// Writes the legacy format, so tests can build files that older executors left behind.
    #[cfg(test)]
    pub(super) fn encrypt(payload: &str, passphrase: &str) -> String {
        let ct = salsa_box(passphrase)
            .expect("legacy test passphrases stay within 48 bytes")
            .encrypt(&Nonce::default(), payload.as_bytes())
            .expect("encryption does not fail");
        B64.encode(ct)
    }
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
            let string = Zeroizing::new(serde_json::to_string(&keys).unwrap());
            self.cipher = Some(encrypt(&string, &passphrase));
            self.keys = None;
        }
    }

    pub fn unlock(&mut self, passphrase: String) -> Result<(), AnyError> {
        let cipher = self
            .cipher
            .as_deref()
            .ok_or_else(|| anyhow!("no keystore loaded"))?;
        let string = decrypt(cipher, &passphrase)?;
        let keys: Keys = serde_json::from_str(&string)?;
        self.keys = Some(keys);
        Ok(())
    }

    pub fn is_unlocked(&self) -> bool {
        self.keys.is_some()
    }

    pub fn export(&mut self, passphrase: String) -> String {
        if let Some(keys) = &self.keys {
            let string = Zeroizing::new(serde_json::to_string(keys).unwrap());
            let encrypted = encrypt(&string, &passphrase);
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

// ── Key name constants ─────────────────────────────────────────────────────

/// Key name for the main agent keypair (local mode default).
pub const KEY_NAME_MAIN: &str = "main";
/// Key name for the platform JWT signing keypair (shared mode default).
pub const KEY_NAME_PLATFORM: &str = "platform";

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

    /// Atomically get or create a keypair — if a key with `name` already exists,
    /// return without modification. Otherwise generate a new one.
    /// Default uses `key_exists` + `generate_keypair` (non-atomic fallback).
    fn get_or_create_keypair(&self, name: &str) -> Result<(), AnyError> {
        if !self.key_exists(name) {
            self.generate_keypair(name)?;
        }
        Ok(())
    }

    /// Downcast support for local-only operations (export, unlock, etc.).
    fn as_any(&self) -> &dyn Any;

    // ── Local-only operations with defaults for shared mode ──────────

    /// Check whether the keystore has been decrypted.
    /// Shared mode: always true (keys live server-side, no local encryption).
    fn is_unlocked(&self) -> bool {
        true
    }

    /// Decrypt the keystore with `passphrase`, making keys available.
    /// Shared mode: returns an error. In platform-hosted deployments the
    /// unlock operation belongs to the platform Worker, not the executor.
    /// Callers should check the backend type before calling unlock.
    fn unlock(&self, _passphrase: &str) -> Result<(), AnyError> {
        Err(anyhow!(
            "unlock() not supported on shared wallet backend. \
             Key management belongs to the platform Worker in shared mode."
        ))
    }

    /// Encrypt and clear keys from memory.
    /// Shared mode: no-op (keys persist server-side).
    fn lock(&self, _passphrase: &str) {}

    /// Export the keystore encrypted with `passphrase`.
    /// Shared mode: returns empty string (export not supported remotely).
    fn export(&self, _passphrase: &str) -> String {
        String::new()
    }

    /// Load an encrypted keystore blob (decrypt later with `unlock`).
    /// Shared mode: no-op (keys managed server-side).
    fn load(&self, _data: &str) {}

    /// Import a DID's keys by resolving the DID string.
    /// Default: returns None (shared backends cannot import local key material).
    /// LocalWallet overrides this to resolve the DID and import its keys.
    fn initialize_keys(&self, _name: &str, _did: &str) -> Option<did_key::Document> {
        None
    }
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

    fn get_or_create_keypair(&self, name: &str) -> Result<(), AnyError> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        if wallet.get_did_document(&name.to_string()).is_none() {
            wallet.generate_keypair(name.to_string());
        }
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_unlocked(&self) -> bool {
        let wallet = self.inner.lock().expect("wallet lock");
        wallet.is_unlocked()
    }

    fn unlock(&self, passphrase: &str) -> Result<(), AnyError> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.unlock(passphrase.to_string())
    }

    fn lock(&self, passphrase: &str) {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.lock(passphrase.to_string());
    }

    fn export(&self, passphrase: &str) -> String {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.export(passphrase.to_string())
    }

    fn load(&self, data: &str) {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.load(data.to_string());
    }

    fn initialize_keys(&self, name: &str, did: &str) -> Option<did_key::Document> {
        let mut wallet = self.inner.lock().expect("wallet lock");
        wallet.initialize_keys(name.to_string(), did.to_string())
    }
}

// ── SharedWallet ────────────────────────────────────────────────────────────

/// Cached key material fetched from the external wallet service.
struct CachedKey {
    secret: Vec<u8>,
    public: Vec<u8>,
    fetched_at: std::time::Instant,
}

/// TTL for cached key material (30 seconds).
/// Kept short to limit JWT-verification asymmetry during key rotation.
const SHARED_WALLET_CACHE_TTL_SECS: u64 = 30;

/// Wallet backend that delegates key operations to an external HTTP service.
/// Used in the hosted platform where multiple executor instances share one
/// identity store.
///
/// Key material fetched over HTTP gets cached in-process with a 30-second TTL.
/// Signing always happens locally — the secret key bytes come over the wire
/// but the actual Ed25519 sign operation runs in this process.
pub struct SharedWallet {
    base_url: String,
    token: String,
    client: reqwest::blocking::Client,
    cache: RwLock<std::collections::HashMap<String, CachedKey>>,
}

impl SharedWallet {
    pub fn new(base_url: String, token: String) -> Self {
        SharedWallet {
            base_url: base_url.trim_end_matches('/').to_string(),
            token,
            client: reqwest::blocking::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .expect("Failed to build SharedWallet HTTP client"),
            cache: RwLock::new(std::collections::HashMap::new()),
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }

    /// Fetch key material from the backend, populating the cache on success.
    fn fetch_and_cache(&self, name: &str) -> Option<(Vec<u8>, Vec<u8>)> {
        let url = format!("{}/keys/{}", self.base_url, urlencoding::encode(name));
        let resp = self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
            .ok()?;
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
        let url = format!("{}/keys/{}", self.base_url, urlencoding::encode(name));
        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("shared wallet: generate_keypair failed: {}", e))?;
        if !resp.status().is_success() {
            return Err(anyhow!(
                "shared wallet: generate_keypair returned {}",
                resp.status()
            ));
        }
        // Write-through: fetch the newly generated key into cache so subsequent
        // reads in this process hit the local cache instead of an extra round-trip.
        if self.fetch_and_cache(name).is_none() {
            log::warn!(
                "SharedWallet::generate_keypair: write-through cache miss for '{}' \
                 — key was created but could not be fetched back immediately",
                name
            );
        }
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
        let resp = match self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
        {
            Ok(r) if r.status().is_success() => r,
            Ok(r) => {
                log::warn!(
                    "SharedWallet::list_key_names: HTTP {} from {}",
                    r.status(),
                    url
                );
                return vec![];
            }
            Err(e) => {
                log::warn!("SharedWallet::list_key_names: request failed: {}", e);
                return vec![];
            }
        };
        // Worker returns {"keys": ["name1", "name2", ...]}
        #[derive(serde::Deserialize)]
        struct KeysResp {
            keys: Vec<String>,
        }
        resp.json::<KeysResp>().map(|r| r.keys).unwrap_or_default()
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
        // Fall back to HTTP — parse the JSON body, not just the status code.
        // The endpoint returns 200 {"exists": false} for missing keys.
        let url = format!(
            "{}/keys/{}/exists",
            self.base_url,
            urlencoding::encode(name)
        );
        let resp = match self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
        {
            Ok(r) if r.status().is_success() => r,
            _ => return false,
        };
        #[derive(serde::Deserialize)]
        struct ExistsResp {
            exists: bool,
        }
        resp.json::<ExistsResp>().map(|r| r.exists).unwrap_or(false)
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
        let result = legacy::slice_to_u8_array(slice);
        assert_eq!(slice, &result);

        let slice_short: &[u8] = &[1, 2, 3];
        let result = legacy::slice_to_u8_array(slice_short);
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
        let encrypted = encrypt(&payload, &passphrase);
        let decrypted = decrypt(&encrypted, &passphrase);
        assert_eq!(payload, *decrypted.unwrap());

        let passphrase = "test".to_string();
        let payload = "test".to_string();
        let encrypted = encrypt(&payload, &passphrase);
        let decrypted = decrypt(&encrypted, &passphrase);
        assert_eq!(payload, *decrypted.unwrap());
    }

    #[test]
    fn test_encrypt_decrypt_wrong_passphrase() {
        let passphrase = "test_passphrase".to_string();
        let wrong_passphrase = "wrong_passphrase".to_string();
        let payload = "test_payload".to_string();
        let encrypted = encrypt(&payload, &passphrase);
        assert_ne!(payload, encrypted);
        let decrypted = decrypt(&encrypted, &wrong_passphrase);
        assert!(decrypted.is_err());
    }

    fn wallet_with_a_key() -> Wallet {
        let mut wallet = Wallet::new();
        wallet.generate_keypair("main".to_string());
        wallet
    }

    // Keystore format 1 derived its key from the first 24 bytes of the passphrase only,
    // so a second passphrase with the same first 24 bytes opened the keystore.
    #[test]
    fn keystore_key_depends_on_the_whole_passphrase() {
        let owner = "correct horse battery staple, first";
        let other = "correct horse battery staple, other";
        assert_eq!(owner.as_bytes()[..24], other.as_bytes()[..24]);
        let cipher = wallet_with_a_key().export(owner.to_string());

        let mut wallet = Wallet::new();
        wallet.load(cipher);
        assert!(wallet.unlock(other.to_string()).is_err());
        assert!(wallet.unlock(owner.to_string()).is_ok());
    }

    // Format 1 reused one key and a zero nonce on every write, so two saves of the same
    // keystore produced the same bytes, and two saves of different keystores leaked the
    // XOR of their plaintexts.
    #[test]
    fn every_export_draws_a_fresh_salt_and_nonce() {
        let mut wallet = wallet_with_a_key();
        let first = wallet.export("passphrase".to_string());
        let second = wallet.export("passphrase".to_string());
        assert_ne!(first, second);
        let first: KeystoreEnvelope = serde_json::from_str(&first).unwrap();
        let second: KeystoreEnvelope = serde_json::from_str(&second).unwrap();
        assert_ne!(first.salt, second.salt);
        assert_ne!(first.nonce, second.nonce);
    }

    // Format 1 panicked for passphrases over 48 bytes, which crashed the executor.
    #[test]
    fn long_passphrases_lock_and_unlock() {
        let passphrase = "a long passphrase made of many words. ".repeat(6);
        assert!(passphrase.len() > 200);
        let cipher = wallet_with_a_key().export(passphrase.clone());

        let mut wallet = Wallet::new();
        wallet.load(cipher);
        assert!(wallet.unlock("short".to_string()).is_err());
        assert!(wallet.unlock(passphrase).is_ok());
    }

    #[test]
    fn keystore_records_its_format_and_kdf_parameters() {
        let cipher = wallet_with_a_key().export("passphrase".to_string());
        let envelope: KeystoreEnvelope = serde_json::from_str(&cipher).unwrap();
        assert_eq!(envelope.v, KEYSTORE_VERSION);
        assert_eq!(envelope.kdf, "argon2id");
        assert_eq!(
            (envelope.m, envelope.t, envelope.p),
            (KDF_MEMORY_KIB, KDF_PASSES, KDF_LANES)
        );
        assert_eq!(B64.decode(&envelope.salt).unwrap().len(), 16);
        assert_eq!(B64.decode(&envelope.nonce).unwrap().len(), 24);
    }

    #[test]
    fn legacy_keystore_unlocks_and_exports_in_the_new_format() {
        let keys = serde_json::to_string(wallet_with_a_key().keys.as_ref().unwrap()).unwrap();
        let legacy_cipher = legacy::encrypt(&keys, "legacy passphrase");
        assert!(is_legacy_keystore(&legacy_cipher));

        let mut wallet = Wallet::new();
        wallet.load(legacy_cipher);
        assert!(wallet.unlock("wrong passphrase".to_string()).is_err());
        wallet.unlock("legacy passphrase".to_string()).unwrap();
        assert!(wallet.get_public_key(&"main".to_string()).is_some());

        let migrated = wallet.export("legacy passphrase".to_string());
        assert!(!is_legacy_keystore(&migrated));
        let mut reloaded = Wallet::new();
        reloaded.load(migrated);
        reloaded.unlock("legacy passphrase".to_string()).unwrap();
    }

    #[test]
    fn legacy_keystore_refuses_a_long_wrong_passphrase_without_panicking() {
        let legacy_cipher = legacy::encrypt("{}", "legacy passphrase");
        let mut wallet = Wallet::new();
        wallet.load(legacy_cipher);
        assert!(wallet.unlock("x".repeat(200)).is_err());
    }

    // Byte-based padding: a passphrase with multi-byte characters still opens a legacy file.
    #[test]
    fn legacy_keystore_with_a_non_ascii_passphrase_unlocks() {
        let keys = serde_json::to_string(wallet_with_a_key().keys.as_ref().unwrap()).unwrap();
        let legacy_cipher = legacy::encrypt(&keys, "pässwörd ☕");
        let mut wallet = Wallet::new();
        wallet.load(legacy_cipher);
        wallet.unlock("pässwörd ☕".to_string()).unwrap();
    }

    #[test]
    fn corrupt_or_tampered_keystores_fail_without_panicking() {
        for cipher in ["not base64 at all!", "{\"v\":2}", "{not json"] {
            let mut wallet = Wallet::new();
            wallet.load(cipher.to_string());
            assert!(
                wallet.unlock("passphrase".to_string()).is_err(),
                "{}",
                cipher
            );
        }

        let cipher = wallet_with_a_key().export("passphrase".to_string());
        let mut envelope: KeystoreEnvelope = serde_json::from_str(&cipher).unwrap();
        envelope.m = KDF_MEMORY_KIB_MAX + 1;
        let mut wallet = Wallet::new();
        wallet.load(serde_json::to_string(&envelope).unwrap());
        assert!(wallet.unlock("passphrase".to_string()).is_err());

        let mut wallet = Wallet::new();
        assert!(wallet.unlock("passphrase".to_string()).is_err());
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

        signature[0] ^= 0x01;
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

    // ── SharedWallet tests ────────────────────────────────────────────────

    #[test]
    fn test_shared_wallet_generate_keypair() {
        let mut server = mockito::Server::new();
        let url = server.url();

        // POST /keys/test_key → 201 Created
        let mock_post = server
            .mock("POST", "/keys/test_key")
            .match_header("Authorization", "Bearer test-token")
            .with_status(201)
            .with_body("{}")
            .create();

        // GET /keys/test_key → key material (for write-through cache)
        let secret = base64::engine::general_purpose::STANDARD.encode(&[1u8; 32]);
        let public = base64::engine::general_purpose::STANDARD.encode(&[2u8; 32]);
        let mock_get = server
            .mock("GET", "/keys/test_key")
            .match_header("Authorization", "Bearer test-token")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(
                r#"{{"secret":"{}","public":"{}"}}"#,
                secret, public
            ))
            .create();

        let wallet = SharedWallet::new(url, "test-token".to_string());
        wallet
            .generate_keypair("test_key")
            .expect("generate should succeed");

        mock_post.assert();
        mock_get.assert();
    }

    #[test]
    fn test_shared_wallet_get_key_material_cache() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let secret = base64::engine::general_purpose::STANDARD.encode(&[1u8; 32]);
        let public = base64::engine::general_purpose::STANDARD.encode(&[2u8; 32]);
        let mock = server
            .mock("GET", "/keys/cached_key")
            .match_header("Authorization", "Bearer tok")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(
                r#"{{"secret":"{}","public":"{}"}}"#,
                secret, public
            ))
            .expect(1) // Should only be called once — second call uses cache
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());

        // First call → network
        let sk1 = wallet.get_secret_key("cached_key");
        assert!(sk1.is_some());

        // Second call → cache (within 30s TTL)
        let sk2 = wallet.get_secret_key("cached_key");
        assert_eq!(sk1, sk2);

        mock.assert();
    }

    #[test]
    fn test_shared_wallet_list_key_names() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/keys")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"keys":["main","platform","user@test.com"]}"#)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let names = wallet.list_key_names();

        assert_eq!(names, vec!["main", "platform", "user@test.com"]);
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_list_key_names_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server.mock("GET", "/keys").with_status(500).create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let names = wallet.list_key_names();

        assert!(names.is_empty());
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_key_exists() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock_yes = server
            .mock("GET", "/keys/main/exists")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"exists":true}"#)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        assert!(wallet.key_exists("main"));
        mock_yes.assert();
    }

    #[test]
    fn test_shared_wallet_key_not_exists() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock_no = server
            .mock("GET", "/keys/missing/exists")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"exists":false}"#)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        assert!(!wallet.key_exists("missing"));
        mock_no.assert();
    }

    #[test]
    fn test_shared_wallet_sign() {
        let mut server = mockito::Server::new();
        let url = server.url();

        // Generate a real Ed25519 keypair for test
        let kp = did_key::generate::<Ed25519KeyPair>(None);
        let public = kp.public_key_bytes();
        let secret = kp.private_key_bytes();

        let secret_b64 = base64::engine::general_purpose::STANDARD.encode(&secret);
        let public_b64 = base64::engine::general_purpose::STANDARD.encode(&public);

        let mock = server
            .mock("GET", "/keys/sign_test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(
                r#"{{"secret":"{}","public":"{}"}}"#,
                secret_b64, public_b64
            ))
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let sig = wallet.sign("sign_test", b"hello world");

        assert!(sig.is_some());
        assert!(!sig.unwrap().is_empty());
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_get_did_document() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let kp = did_key::generate::<Ed25519KeyPair>(None);
        let public = kp.public_key_bytes();
        let secret = kp.private_key_bytes();

        let secret_b64 = base64::engine::general_purpose::STANDARD.encode(&secret);
        let public_b64 = base64::engine::general_purpose::STANDARD.encode(&public);

        let mock = server
            .mock("GET", "/keys/did_test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(
                r#"{{"secret":"{}","public":"{}"}}"#,
                secret_b64, public_b64
            ))
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let doc = wallet.get_did_document("did_test");

        assert!(doc.is_some());
        assert!(doc.unwrap().id.starts_with("did:key:"));
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_generate_keypair_server_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/keys/fail_key")
            .with_status(500)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let result = wallet.generate_keypair("fail_key");

        assert!(result.is_err());
        mock.assert();
    }

    #[test]
    fn test_local_wallet_sign_nonexistent_key() {
        let local = LocalWallet::new();
        assert!(local.sign("missing", b"data").is_none());
    }

    #[test]
    fn test_shared_wallet_list_key_names_server_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server.mock("GET", "/keys").with_status(500).create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        let names = wallet.list_key_names();
        assert!(names.is_empty());
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_key_exists_true() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/keys/main/exists")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"exists":true}"#)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        assert!(wallet.key_exists("main"));
        mock.assert();
    }

    #[test]
    fn test_shared_wallet_key_exists_false() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/keys/missing/exists")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"exists":false}"#)
            .create();

        let wallet = SharedWallet::new(url, "tok".to_string());
        assert!(!wallet.key_exists("missing"));
        mock.assert();
    }
}
