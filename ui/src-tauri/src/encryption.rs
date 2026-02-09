use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng},
    Aes256Gcm, Key, Nonce,
};
use base64::{engine::general_purpose, Engine as _};
use keyring::Entry;
use std::sync::Mutex;

// Service name for keyring entry
const KEYRING_SERVICE: &str = "ad4m-launcher";
const KEYRING_USER: &str = "smtp-encryption-key";

// Lazy static mutex to ensure thread-safe access to keyring
lazy_static::lazy_static! {
    static ref KEYRING_MUTEX: Mutex<()> = Mutex::new(());
}

/// Get or create the encryption key from the system keyring
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

    #[test]
    fn test_encrypt_decrypt() {
        let password = "test-password-123";
        let encrypted = encrypt_password(password).unwrap();
        let decrypted = decrypt_password(&encrypted).unwrap();
        assert_eq!(password, decrypted);
    }
}
