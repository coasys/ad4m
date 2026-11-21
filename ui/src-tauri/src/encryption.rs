use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng},
    Aes256Gcm, Key, Nonce,
};
use base64::{engine::general_purpose, Engine as _};
use keyring::Entry;
use std::sync::Mutex;

// Service name for keyring entry
const KEYRING_SERVICE: &str = "ad4m-launcher";
#[cfg(not(test))]
const KEYRING_USER: &str = "smtp-encryption-key";

// Lazy static mutex to ensure thread-safe access to keyring
lazy_static::lazy_static! {
    static ref KEYRING_MUTEX: Mutex<()> = Mutex::new(());
}

/// Tests never read or create the OS keyring entry (on a developer's
/// machine it holds the real launcher's key): a fixed key stands in.
#[cfg(test)]
fn get_encryption_key() -> Result<Key<Aes256Gcm>, Box<dyn std::error::Error>> {
    Ok(*Key::<Aes256Gcm>::from_slice(&[7u8; 32]))
}

/// Get or create the encryption key from the system keyring
#[cfg(not(test))]
fn get_encryption_key() -> Result<Key<Aes256Gcm>, Box<dyn std::error::Error>> {
    let _lock = KEYRING_MUTEX.lock().unwrap();

    let entry = Entry::new(KEYRING_SERVICE, KEYRING_USER)?;

    // Try to get existing key
    match entry.get_password() {
        Ok(key_b64) => {
            // Decode base64 key
            let key_bytes = general_purpose::STANDARD
                .decode(&key_b64)
                .map_err(|e| format!("Failed to decode key: {}", e))?;

            if key_bytes.len() != 32 {
                return Err("Invalid key length".into());
            }

            Ok(*Key::<Aes256Gcm>::from_slice(&key_bytes))
        }
        Err(keyring::Error::NoEntry) => {
            // Generate new key
            let key = Aes256Gcm::generate_key(&mut OsRng);

            // Encode to base64 and store
            let key_b64 = general_purpose::STANDARD.encode(key.as_slice());
            entry.set_password(&key_b64)?;

            Ok(key)
        }
        Err(e) => Err(Box::new(e)),
    }
}

/// The launcher's operator secret, which opens the embedded executor's data at rest.
const STORAGE_SECRET_USER: &str = "storage-secret";

/// What the launcher does about the executor's storage secret at start, from whether the
/// OS keyring holds it and whether the data folder holds a keyring (sealed data).
#[derive(Debug, PartialEq, Eq)]
pub enum StorageSecretDecision {
    /// The OS keyring's secret: hand it to the executor.
    Stored,
    /// No sealed data yet: make a new secret and keep it in the OS keyring.
    Make,
    /// Sealed data but no OS keyring entry: a new secret could not open the keyring, so
    /// none is made. The launcher asks the user for it instead of locking them out.
    Missing,
}

/// The storage-secret decision, split from the keyring so tests need no OS keyring.
pub(crate) fn storage_secret_decision(stored: bool, sealed_data: bool) -> StorageSecretDecision {
    match (stored, sealed_data) {
        (true, _) => StorageSecretDecision::Stored,
        (false, true) => StorageSecretDecision::Missing,
        (false, false) => StorageSecretDecision::Make,
    }
}

fn storage_secret_entry() -> Result<Entry, keyring::Error> {
    Entry::new(KEYRING_SERVICE, STORAGE_SECRET_USER)
}

/// The operator secret the launcher hands its executor in-process: kept in the OS keyring (not
/// on the data folder's disk), made from the OS random source on the first start. `Ok(None)`
/// is [`StorageSecretDecision::Missing`]: the data folder at `data_dir` holds sealed data but
/// the OS keyring lost the secret, so no new one is made — a new one could not open it.
#[cfg(not(test))]
pub fn storage_secret(
    data_dir: &std::path::Path,
) -> Result<Option<rust_executor::at_rest::StorageSecret>, Box<dyn std::error::Error>> {
    use rust_executor::at_rest::{keyring_file, secret_source, SecretSourceKind, StorageSecret};
    let _lock = KEYRING_MUTEX.lock().unwrap();
    let entry = storage_secret_entry()?;
    let stored = match entry.get_password() {
        Ok(hex) => Some(hex),
        Err(keyring::Error::NoEntry) => None,
        Err(e) => return Err(Box::new(e)),
    };
    let sealed_data = keyring_file::path_in(data_dir).exists();
    match storage_secret_decision(stored.is_some(), sealed_data) {
        StorageSecretDecision::Stored => Ok(Some(StorageSecret::from_hex(
            stored.as_deref().unwrap(),
            SecretSourceKind::InProcess,
            None,
        )?)),
        StorageSecretDecision::Make => {
            let hex = secret_source::new_secret_hex();
            entry.set_password(&hex)?;
            Ok(Some(StorageSecret::from_hex(
                &hex,
                SecretSourceKind::InProcess,
                None,
            )?))
        }
        StorageSecretDecision::Missing => Ok(None),
    }
}

/// Tests never touch the OS keyring: a fixed secret stands in.
#[cfg(test)]
pub fn storage_secret(
    _data_dir: &std::path::Path,
) -> Result<Option<rust_executor::at_rest::StorageSecret>, Box<dyn std::error::Error>> {
    Ok(Some(rust_executor::at_rest::StorageSecret::in_process(
        [7u8; 32],
    )))
}

/// Keeps `hex` (64 hex characters) as the executor's storage secret in the OS keyring: a
/// secret the user entered or restored, so the next start finds it.
pub fn set_storage_secret(hex: &str) -> Result<(), Box<dyn std::error::Error>> {
    let _lock = KEYRING_MUTEX.lock().unwrap();
    rust_executor::at_rest::secret_source::parse_hex(hex)?;
    storage_secret_entry()?.set_password(hex)?;
    Ok(())
}

/// The storage secret the OS keyring holds, for the user to back up. `Err` when there is
/// none or the keyring does not answer.
pub fn get_storage_secret() -> Result<String, Box<dyn std::error::Error>> {
    let _lock = KEYRING_MUTEX.lock().unwrap();
    Ok(storage_secret_entry()?.get_password()?)
}

/// Reads a storage secret file (the 64-hex form `ad4m-executor storage new-secret` writes)
/// and keeps it in the OS keyring. Answers the secret so the caller can unlock with it.
pub fn restore_storage_secret_file(
    path: &std::path::Path,
) -> Result<String, Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(path)?;
    let hex = content.lines().next().unwrap_or("").trim().to_string();
    set_storage_secret(&hex)?;
    Ok(hex)
}

/// Encrypt a password using AES-256-GCM
pub fn encrypt_password(password: &str) -> Result<String, Box<dyn std::error::Error>> {
    let key = get_encryption_key()?;
    let cipher = Aes256Gcm::new(&key);
    let nonce = Aes256Gcm::generate_nonce(&mut OsRng);

    let ciphertext = cipher
        .encrypt(&nonce, password.as_bytes())
        .map_err(|e| format!("Encryption failed: {}", e))?;

    // Combine nonce and ciphertext, then encode to base64
    let mut combined = nonce.to_vec();
    combined.extend_from_slice(&ciphertext);

    Ok(general_purpose::STANDARD.encode(&combined))
}

/// Decrypt a password using AES-256-GCM
pub fn decrypt_password(encrypted: &str) -> Result<String, Box<dyn std::error::Error>> {
    let key = get_encryption_key()?;
    let cipher = Aes256Gcm::new(&key);

    // Decode from base64
    let combined = general_purpose::STANDARD
        .decode(encrypted)
        .map_err(|e| format!("Failed to decode encrypted password: {}", e))?;

    if combined.len() < 12 {
        return Err("Invalid encrypted data: too short".into());
    }

    // Extract nonce (first 12 bytes) and ciphertext (rest)
    let nonce = Nonce::from_slice(&combined[..12]);
    let ciphertext = &combined[12..];

    let plaintext = cipher
        .decrypt(nonce, ciphertext)
        .map_err(|e| format!("Decryption failed: {}", e))?;

    String::from_utf8(plaintext)
        .map_err(|e| format!("Invalid UTF-8 in decrypted password: {}", e).into())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// No OS keyring entry and no keyring in the data folder (a first start, or a fresh
    /// agent): a new secret is made and stored.
    #[test]
    fn no_entry_and_no_keyring_makes_a_new_secret() {
        assert_eq!(
            storage_secret_decision(false, false),
            StorageSecretDecision::Make
        );
    }

    /// No OS keyring entry but a keyring in the data folder (sealed data): the missing state
    /// is reported and no new secret is made — one could not open the keyring anyway.
    #[test]
    fn no_entry_with_a_keyring_reports_missing_and_makes_none() {
        assert_eq!(
            storage_secret_decision(false, true),
            StorageSecretDecision::Missing
        );
    }

    /// A stored entry is used, sealed data or not.
    #[test]
    fn a_stored_entry_is_used() {
        for sealed_data in [false, true] {
            assert_eq!(
                storage_secret_decision(true, sealed_data),
                StorageSecretDecision::Stored
            );
        }
    }

    #[test]
    fn test_encrypt_decrypt() {
        let password = "test-password-123";
        let encrypted = encrypt_password(password).unwrap();
        let decrypted = decrypt_password(&encrypted).unwrap();
        assert_eq!(password, decrypted);
    }
}
