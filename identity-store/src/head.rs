//! The store's head key and its signed heads (`coasys:head`, AR-01 4.2 and 4.5.2).
//!
//! A signed head proves what the store said about a log at a time: `sig` is the store key's
//! Ed25519 signature of `signing_bytes("coasys:head", {scid, seq, hash, time, store})`, where
//! `store` is the store key's `did:key` (`ad4m_identity_core::HeadStatement`). Executors check it
//! against `identity.storeKeys`.

use std::path::Path;

use ad4m_hash_chain::{encode_ed25519_multikey, encode_proof_value, Multikey};
use ad4m_identity_core::HeadStatement;
use ad4m_ldk::identity::SignedHead;
use ed25519_dalek::SigningKey;

/// The store's Ed25519 head key.
pub struct StoreKey {
    key: SigningKey,
}

/// Why the key file cannot serve.
#[derive(Debug, thiserror::Error)]
pub enum KeyError {
    #[error("key file {0}: {1}")]
    Io(String, std::io::Error),
    #[error("key file {0} does not hold 32 bytes of base58btc")]
    Format(String),
    #[error("key file {0} exists; keygen does not overwrite a key")]
    Exists(String),
}

impl StoreKey {
    /// A key from its 32-byte secret.
    pub fn from_secret(secret: [u8; 32]) -> Self {
        StoreKey {
            key: SigningKey::from_bytes(&secret),
        }
    }

    /// A fresh key from the OS random source.
    pub fn generate() -> Self {
        Self::from_secret(rand::random())
    }

    /// Reads the key file: the 32-byte secret as base58btc text.
    pub fn load(path: &Path) -> Result<Self, KeyError> {
        let name = path.display().to_string();
        let text = std::fs::read_to_string(path).map_err(|e| KeyError::Io(name.clone(), e))?;
        let bytes = bs58::decode(text.trim())
            .into_vec()
            .map_err(|_| KeyError::Format(name.clone()))?;
        let secret: [u8; 32] = bytes.try_into().map_err(|_| KeyError::Format(name))?;
        Ok(Self::from_secret(secret))
    }

    /// Writes a new key file (owner read and write only). Refuses to overwrite one.
    pub fn create(path: &Path) -> Result<Self, KeyError> {
        let name = path.display().to_string();
        if path.exists() {
            return Err(KeyError::Exists(name));
        }
        let key = Self::generate();
        let text = bs58::encode(key.key.to_bytes()).into_string() + "\n";
        write_private(path, text.as_bytes()).map_err(|e| KeyError::Io(name, e))?;
        Ok(key)
    }

    /// Loads the key file, or makes one when it does not exist.
    pub fn load_or_create(path: &Path) -> Result<Self, KeyError> {
        if path.exists() {
            Self::load(path)
        } else {
            Self::create(path)
        }
    }

    /// The public multikey (`z6Mk…`) for the executors' `identity.storeKeys`.
    pub fn multikey(&self) -> Multikey {
        encode_ed25519_multikey(self.key.verifying_key().as_bytes())
    }

    /// The store's `did:key`.
    pub fn did(&self) -> String {
        format!("did:key:{}", self.multikey())
    }

    /// Signs the head of `scid` at `seq`/`hash`, at `time`.
    pub fn sign_head(&self, scid: &str, seq: u64, hash: &str, time: u64) -> SignedHead {
        let store = self.did();
        let statement = HeadStatement {
            scid,
            seq,
            hash,
            time,
            store: &store,
        };
        let bytes = statement
            .signing_bytes()
            .expect("a head of strings and safe integers has a canonical form");
        let sig = ed25519_dalek::Signer::sign(&self.key, &bytes).to_bytes();
        SignedHead {
            scid: scid.to_string(),
            seq,
            hash: hash.to_string(),
            time,
            store,
            sig: encode_proof_value(&sig),
        }
    }
}

#[cfg(unix)]
fn write_private(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(path)?;
    file.write_all(bytes)
}

#[cfg(not(unix))]
fn write_private(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    file.write_all(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn verify_head(head: &SignedHead, trusted: &[Multikey]) -> bool {
        HeadStatement {
            scid: &head.scid,
            seq: head.seq,
            hash: &head.hash,
            time: head.time,
            store: &head.store,
        }
        .verify(&head.sig, trusted)
    }

    #[test]
    fn a_head_verifies_under_its_key_only() {
        let key = StoreKey::from_secret([1; 32]);
        let other = StoreKey::from_secret([2; 32]);
        let head = key.sign_head("QmScid", 3, "QmHash", 1_700_000_000);
        assert!(verify_head(&head, &[key.multikey()]));
        assert!(!verify_head(&head, &[other.multikey()]));
        let mut moved = head.clone();
        moved.seq = 4;
        assert!(!verify_head(&moved, &[key.multikey()]));
    }

    #[test]
    fn the_key_file_round_trips_and_is_not_overwritten() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("store-key");
        let made = StoreKey::create(&path).unwrap();
        assert_eq!(StoreKey::load(&path).unwrap().multikey(), made.multikey());
        assert!(matches!(StoreKey::create(&path), Err(KeyError::Exists(_))));
        assert_eq!(
            StoreKey::load_or_create(&path).unwrap().multikey(),
            made.multikey()
        );
    }
}
