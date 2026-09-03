//! Encryption keyring — PR8.
//!
//! Versioned data-encryption keys (DEKs) wrapped to X25519 recipients
//! resolved from their KELs. One implementation serves three consumers:
//! hosted-executor at-rest, server-link-language room keys, and DMs.
//!
//! ## Mechanism
//!
//! Each encrypted context gets a versioned DEK. Each version wraps to the
//! current encryption keys of its members, resolved from their KELs.
//! Membership or device changes mint a new version. A leaver retains the
//! versions they held — they decrypt history up to exit, nothing after.
//!
//! ## Honest limit
//!
//! At-rest encryption protects backups, DB dumps, disk theft, and
//! cross-tenant leaks. A running executor holds plaintext to compute.
//! Protection against a malicious host runtime requires running your own
//! executor.

use chacha20poly1305::aead::{Aead, KeyInit};
use chacha20poly1305::{ChaCha20Poly1305, Nonce};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use x25519_dalek::{PublicKey, StaticSecret};
use zeroize::Zeroize;

// ─── types ─────────────────────────────────────────────────────────────────

/// A wrapped copy of a DEK, encrypted to one recipient's X25519 key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WrappedKey {
    /// DID or key_id the wrapping targets.
    pub recipient: String,
    /// The X25519 public key wrapping targeted.
    pub encryption_key: String,
    /// Encrypted DEK bytes (ChaCha20Poly1305).
    pub ciphertext: Vec<u8>,
    /// Ephemeral X25519 public key used for DH (one per wrap).
    pub ephemeral_pubkey: [u8; 32],
}

/// One version of a data-encryption key, wrapped to its member set.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DekVersion {
    pub version: u32,
    pub wrapped: Vec<WrappedKey>,
    pub created_at_seq: u64,
}

/// An encryption recipient resolved from a KEL.
#[derive(Debug, Clone)]
pub struct EncryptionRecipient {
    /// DID or key_id.
    pub id: String,
    /// X25519 public key (32 bytes, hex-encoded).
    pub encryption_key: String,
}

/// The plaintext DEK — 32 bytes for ChaCha20Poly1305.
#[derive(Clone)]
pub struct Dek {
    pub key: [u8; 32],
}

impl Drop for Dek {
    fn drop(&mut self) {
        self.key.zeroize();
    }
}

impl std::fmt::Debug for Dek {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dek([REDACTED])")
    }
}

// ─── errors ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum KeyringError {
    /// Requested DEK version does not exist.
    VersionNotFound(u32),
    /// DID has no X25519 encryption key in its KEL.
    RecipientKeyMissing { did: String },
    /// No wrapped key matches the provided secret for unwrapping.
    UnwrapFailed,
    /// Encryption/decryption failed.
    CryptoError(String),
    /// Empty recipient set — cannot wrap to nobody.
    EmptyRecipients,
}

impl std::fmt::Display for KeyringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyringError::VersionNotFound(v) => write!(f, "DEK version {} not found", v),
            KeyringError::RecipientKeyMissing { did } => {
                write!(f, "no encryption key for {}", did)
            }
            KeyringError::UnwrapFailed => write!(f, "no matching wrapped key for unwrap"),
            KeyringError::CryptoError(msg) => write!(f, "crypto error: {}", msg),
            KeyringError::EmptyRecipients => write!(f, "empty recipient set"),
        }
    }
}

impl std::error::Error for KeyringError {}

// ─── crypto helpers ────────────────────────────────────────────────────────

/// Generate a fresh 32-byte DEK.
pub fn generate_dek() -> Dek {
    use rand::RngCore;
    let mut key = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut key);
    Dek { key }
}

/// Wrap a DEK to a single X25519 recipient using ephemeral ECDH +
/// ChaCha20Poly1305.
fn wrap_to_recipient(
    dek: &Dek,
    recipient_pubkey: &[u8; 32],
) -> Result<(Vec<u8>, [u8; 32]), KeyringError> {
    let ephemeral_secret = StaticSecret::random_from_rng(rand::thread_rng());
    let ephemeral_pubkey = PublicKey::from(&ephemeral_secret);

    let recipient_pk = PublicKey::from(*recipient_pubkey);
    let shared_secret = ephemeral_secret.diffie_hellman(&recipient_pk);

    // Derive a symmetric key from the shared secret via SHA-256.
    let sym_key = Sha256::digest(shared_secret.as_bytes());
    let cipher =
        ChaCha20Poly1305::new_from_slice(&sym_key).map_err(|e| KeyringError::CryptoError(e.to_string()))?;

    // Use a zero nonce — each ephemeral key produces a unique shared secret,
    // so reuse across keys never happens.
    let nonce = Nonce::default();
    let ciphertext = cipher
        .encrypt(&nonce, dek.key.as_ref())
        .map_err(|e| KeyringError::CryptoError(e.to_string()))?;

    Ok((ciphertext, ephemeral_pubkey.to_bytes()))
}

/// Unwrap a DEK from a wrapped key using the recipient's X25519 secret.
fn unwrap_with_secret(
    wrapped: &WrappedKey,
    recipient_secret: &StaticSecret,
) -> Result<Dek, KeyringError> {
    let ephemeral_pk = PublicKey::from(wrapped.ephemeral_pubkey);
    let shared_secret = recipient_secret.diffie_hellman(&ephemeral_pk);

    let sym_key = Sha256::digest(shared_secret.as_bytes());
    let cipher =
        ChaCha20Poly1305::new_from_slice(&sym_key).map_err(|e| KeyringError::CryptoError(e.to_string()))?;

    let nonce = Nonce::default();
    let plaintext = cipher
        .decrypt(&nonce, wrapped.ciphertext.as_ref())
        .map_err(|_| KeyringError::UnwrapFailed)?;

    let mut key = [0u8; 32];
    if plaintext.len() != 32 {
        return Err(KeyringError::CryptoError("unexpected DEK length".into()));
    }
    key.copy_from_slice(&plaintext);
    Ok(Dek { key })
}

/// Parse a hex-encoded X25519 public key to 32 bytes.
fn parse_x25519_pubkey(hex_key: &str) -> Result<[u8; 32], KeyringError> {
    let bytes =
        hex::decode(hex_key).map_err(|_| KeyringError::CryptoError("invalid hex key".into()))?;
    if bytes.len() != 32 {
        return Err(KeyringError::CryptoError(format!(
            "X25519 key must have 32 bytes, got {}",
            bytes.len()
        )));
    }
    let mut key = [0u8; 32];
    key.copy_from_slice(&bytes);
    Ok(key)
}

// ─── keyring ───────────────────────────────────────────────────────────────

/// A versioned keyring — manages DEK versions and wrapping.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VersionedKeyring {
    versions: Vec<DekVersion>,
}

impl VersionedKeyring {
    pub fn new() -> Self {
        Self {
            versions: Vec::new(),
        }
    }

    /// The current (latest) DEK version, if any.
    pub fn current(&self) -> Option<&DekVersion> {
        self.versions.last()
    }

    /// Look up a specific version.
    pub fn version(&self, v: u32) -> Option<&DekVersion> {
        self.versions.iter().find(|dv| dv.version == v)
    }

    /// The version count.
    pub fn version_count(&self) -> usize {
        self.versions.len()
    }

    /// Mint a new DEK version, wrapped to the given recipients.
    /// Returns the new version reference.
    pub fn mint(
        &mut self,
        recipients: &[EncryptionRecipient],
        at_seq: u64,
    ) -> Result<&DekVersion, KeyringError> {
        if recipients.is_empty() {
            return Err(KeyringError::EmptyRecipients);
        }

        let dek = generate_dek();
        let next_version = self.versions.last().map_or(1, |v| v.version + 1);

        let mut wrapped = Vec::with_capacity(recipients.len());
        for r in recipients {
            let pubkey = parse_x25519_pubkey(&r.encryption_key)?;
            let (ciphertext, ephemeral) = wrap_to_recipient(&dek, &pubkey)?;
            wrapped.push(WrappedKey {
                recipient: r.id.clone(),
                encryption_key: r.encryption_key.clone(),
                ciphertext,
                ephemeral_pubkey: ephemeral,
            });
        }

        self.versions.push(DekVersion {
            version: next_version,
            wrapped,
            created_at_seq: at_seq,
        });

        Ok(self.versions.last().unwrap())
    }

    /// Wrap the current DEK to an additional recipient WITHOUT minting a new
    /// version. Used when enrolling a new device — the DEK stays the same,
    /// the new device just gets a copy.
    pub fn wrap_to_additional(
        &mut self,
        recipient: &EncryptionRecipient,
        unwrap_secret: &StaticSecret,
    ) -> Result<(), KeyringError> {
        let current = self.versions.last_mut().ok_or(KeyringError::VersionNotFound(0))?;

        // Unwrap the DEK using the existing member's secret.
        let dek = unwrap_from_version(current, unwrap_secret)?;

        // Wrap to the new recipient.
        let pubkey = parse_x25519_pubkey(&recipient.encryption_key)?;
        let (ciphertext, ephemeral) = wrap_to_recipient(&dek, &pubkey)?;
        current.wrapped.push(WrappedKey {
            recipient: recipient.id.clone(),
            encryption_key: recipient.encryption_key.clone(),
            ciphertext,
            ephemeral_pubkey: ephemeral,
        });

        Ok(())
    }

    /// Rewrap: mint a new version wrapped to a new set of recipients (e.g.
    /// after revoking a key or changing membership). Requires a secret that
    /// can unwrap the current version.
    pub fn rewrap(
        &mut self,
        recipients: &[EncryptionRecipient],
        unwrap_secret: &StaticSecret,
        at_seq: u64,
    ) -> Result<&DekVersion, KeyringError> {
        if recipients.is_empty() {
            return Err(KeyringError::EmptyRecipients);
        }

        // Unwrap the current DEK.
        let current = self.versions.last().ok_or(KeyringError::VersionNotFound(0))?;
        let _old_dek = unwrap_from_version(current, unwrap_secret)?;

        // Mint a fresh DEK for the new version — forward secrecy.
        // A revoked member can still decrypt old versions they held,
        // but NOT the new DEK.
        self.mint(recipients, at_seq)
    }

    /// Unwrap the DEK from a specific version using a recipient's secret.
    pub fn unwrap_version(
        &self,
        version: u32,
        secret: &StaticSecret,
    ) -> Result<Dek, KeyringError> {
        let v = self
            .version(version)
            .ok_or(KeyringError::VersionNotFound(version))?;
        unwrap_from_version(v, secret)
    }
}

/// Try to unwrap a DEK from a DekVersion using any of the wrapped keys.
fn unwrap_from_version(version: &DekVersion, secret: &StaticSecret) -> Result<Dek, KeyringError> {
    let my_pubkey = PublicKey::from(secret);
    let my_pubkey_hex = hex::encode(my_pubkey.to_bytes());

    for w in &version.wrapped {
        if w.encryption_key == my_pubkey_hex {
            return unwrap_with_secret(w, secret);
        }
    }
    Err(KeyringError::UnwrapFailed)
}

// ─── recipient resolution ──────────────────────────────────────────────────

/// Resolve encryption recipients from key state. Returns the X25519 keys
/// for all currently-valid keys that carry an encryption_key.
pub fn recipients_from_keys(
    did: &str,
    keys: &[crate::agent::kel::KeyEntry],
) -> Result<Vec<EncryptionRecipient>, KeyringError> {
    let recipients: Vec<_> = keys
        .iter()
        .filter_map(|ke| {
            ke.encryption_key.as_ref().map(|ek| EncryptionRecipient {
                id: ke.id.clone(),
                encryption_key: ek.clone(),
            })
        })
        .collect();

    if recipients.is_empty() {
        return Err(KeyringError::RecipientKeyMissing {
            did: did.to_string(),
        });
    }

    Ok(recipients)
}

// ─── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_keypair() -> (StaticSecret, PublicKey) {
        let secret = StaticSecret::random_from_rng(rand::thread_rng());
        let pubkey = PublicKey::from(&secret);
        (secret, pubkey)
    }

    fn test_recipient(id: &str, pubkey: &PublicKey) -> EncryptionRecipient {
        EncryptionRecipient {
            id: id.to_string(),
            encryption_key: hex::encode(pubkey.to_bytes()),
        }
    }

    #[test]
    fn wrap_unwrap_round_trip() {
        let (s1, pk1) = test_keypair();
        let (s2, pk2) = test_keypair();
        let (s3, pk3) = test_keypair();

        let recipients = vec![
            test_recipient("user-1", &pk1),
            test_recipient("user-2", &pk2),
            test_recipient("user-3", &pk3),
        ];

        let mut keyring = VersionedKeyring::new();
        keyring.mint(&recipients, 0).unwrap();

        // Each recipient can unwrap.
        let d1 = keyring.unwrap_version(1, &s1).unwrap();
        let d2 = keyring.unwrap_version(1, &s2).unwrap();
        let d3 = keyring.unwrap_version(1, &s3).unwrap();

        // All get the same DEK.
        assert_eq!(d1.key, d2.key);
        assert_eq!(d2.key, d3.key);
    }

    #[test]
    fn enrol_wraps_without_new_version() {
        let (s1, pk1) = test_keypair();
        let (s2, pk2) = test_keypair();

        let mut keyring = VersionedKeyring::new();
        keyring
            .mint(&[test_recipient("user-1", &pk1)], 0)
            .unwrap();

        assert_eq!(keyring.version_count(), 1);

        // Enrol user-2 without minting a new version.
        keyring
            .wrap_to_additional(&test_recipient("user-2", &pk2), &s1)
            .unwrap();

        // Version count unchanged.
        assert_eq!(keyring.version_count(), 1);

        // Both can unwrap the same version.
        let d1 = keyring.unwrap_version(1, &s1).unwrap();
        let d2 = keyring.unwrap_version(1, &s2).unwrap();
        assert_eq!(d1.key, d2.key);
    }

    #[test]
    fn revoke_mints_excludes() {
        let (s1, pk1) = test_keypair();
        let (s2, pk2) = test_keypair();

        let mut keyring = VersionedKeyring::new();
        keyring
            .mint(
                &[
                    test_recipient("user-1", &pk1),
                    test_recipient("user-2", &pk2),
                ],
                0,
            )
            .unwrap();

        // Revoke user-2: rewrap to only user-1.
        keyring
            .rewrap(&[test_recipient("user-1", &pk1)], &s1, 1)
            .unwrap();

        assert_eq!(keyring.version_count(), 2);

        // User-1 can unwrap version 2.
        keyring.unwrap_version(2, &s1).unwrap();

        // User-2 cannot unwrap version 2.
        let result = keyring.unwrap_version(2, &s2);
        assert!(matches!(result, Err(KeyringError::UnwrapFailed)));
    }

    #[test]
    fn leaver_decrypts_history() {
        let (s1, pk1) = test_keypair();
        let (s2, pk2) = test_keypair();

        let mut keyring = VersionedKeyring::new();

        // Version 1: both members.
        keyring
            .mint(
                &[
                    test_recipient("user-1", &pk1),
                    test_recipient("user-2", &pk2),
                ],
                0,
            )
            .unwrap();

        // Version 2: remove user-2.
        keyring
            .rewrap(&[test_recipient("user-1", &pk1)], &s1, 1)
            .unwrap();

        // Version 3: user-1 only.
        keyring
            .rewrap(&[test_recipient("user-1", &pk1)], &s1, 2)
            .unwrap();

        // User-2 can decrypt version 1 (they were a member).
        keyring.unwrap_version(1, &s2).unwrap();

        // User-2 cannot decrypt versions 2 or 3.
        assert!(matches!(
            keyring.unwrap_version(2, &s2),
            Err(KeyringError::UnwrapFailed)
        ));
        assert!(matches!(
            keyring.unwrap_version(3, &s2),
            Err(KeyringError::UnwrapFailed)
        ));
    }

    #[test]
    fn version_not_found() {
        let keyring = VersionedKeyring::new();
        let (s1, _) = test_keypair();
        let result = keyring.unwrap_version(99, &s1);
        assert!(matches!(result, Err(KeyringError::VersionNotFound(99))));
    }

    #[test]
    fn empty_recipients_fails() {
        let mut keyring = VersionedKeyring::new();
        let result = keyring.mint(&[], 0);
        assert!(matches!(result, Err(KeyringError::EmptyRecipients)));
    }

    #[test]
    fn recipient_key_missing() {
        let keys: Vec<crate::agent::kel::KeyEntry> = vec![crate::agent::kel::KeyEntry {
            id: "test#key-0".to_string(),
            signing_key: "did:key:z6MkTest".to_string(),
            encryption_key: None, // No X25519 key
            scope: crate::agent::kel::Scope::full(),
        }];

        let result = recipients_from_keys("did:scid:ke:1:Etest", &keys);
        assert!(matches!(
            result,
            Err(KeyringError::RecipientKeyMissing { .. })
        ));
    }

    #[test]
    fn recipients_from_keys_extracts_x25519() {
        let (_, pk) = test_keypair();
        let keys = vec![crate::agent::kel::KeyEntry {
            id: "test#key-0".to_string(),
            signing_key: "did:key:z6MkTest".to_string(),
            encryption_key: Some(hex::encode(pk.to_bytes())),
            scope: crate::agent::kel::Scope::full(),
        }];

        let recipients = recipients_from_keys("did:scid:ke:1:Etest", &keys).unwrap();
        assert_eq!(recipients.len(), 1);
        assert_eq!(recipients[0].encryption_key, hex::encode(pk.to_bytes()));
    }

    #[test]
    fn dek_zeroed_on_drop() {
        let dek = generate_dek();
        // Verify the DEK has content (not all zeros).
        assert!(dek.key.iter().any(|&b| b != 0));
        // Drop happens automatically; zeroize ensures the key gets cleared.
        // We can't test post-drop memory, but the Drop impl calls zeroize.
    }
}
