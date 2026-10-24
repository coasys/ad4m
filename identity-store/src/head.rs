//! The store's head key and its signed heads (`coasys:head`, AR-01 4.2 and 4.5.2).
//!
//! A signed head proves what the store said about a log at a time: `sig` is the store key's
//! Ed25519 signature of `signing_bytes("coasys:head", {scid, seq, hash, time, store})`, where
//! `store` is the store key's `did:key`. Executors check it against `identity.storeKeys`.

use std::path::Path;

use ad4m_hash_chain::{decode_proof_value, encode_ed25519_multikey, encode_proof_value, Multikey};
use ad4m_identity_core::{signing_bytes, verify_signed};
use ad4m_ldk::identity::SignedHead;
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};

/// The signing domain of heads.
pub const HEAD_DOMAIN: &str = "coasys:head";

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
        let value = head_value(scid, seq, hash, time, &store);
        let bytes = signing_bytes(HEAD_DOMAIN, &value)
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

fn head_value(scid: &str, seq: u64, hash: &str, time: u64, store: &str) -> Value {
    json!({ "scid": scid, "seq": seq, "hash": hash, "time": time, "store": store })
}

/// True when `head` is signed by its `store` key and that key is one of `trusted` (multikeys).
pub fn verify_head(head: &SignedHead, trusted: &[Multikey]) -> bool {
    let Some(key) = head.store.strip_prefix("did:key:") else {
        return false;
    };
    if !trusted.iter().any(|t| t == key) {
        return false;
    }
    let Some(sig) = decode_proof_value(&head.sig) else {
        return false;
    };
    let value = head_value(&head.scid, head.seq, &head.hash, head.time, &head.store);
    verify_signed(HEAD_DOMAIN, &value, key, &sig)
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
