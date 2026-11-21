use std::fmt;

use hkdf::Hkdf;
use rand_core::{OsRng, RngCore};
use sha2::Sha256;
use zeroize::Zeroizing;

use crate::KeyringError;

/// The bytes of every key in the crate.
pub const KEY_LEN: usize = 32;

/// A 32-byte symmetric key.
///
/// The bytes live in one heap buffer for the key's whole life, so moving a `SecretKey` moves a
/// pointer and leaves no copy of the key behind; the buffer is zeroed before it is freed. Not
/// `Clone`; `Debug` shows no bytes.
pub struct SecretKey(Box<Zeroizing<[u8; KEY_LEN]>>);

impl SecretKey {
    /// An all-zero key to fill in place; never handed out unfilled.
    pub(crate) fn zeroed() -> Self {
        SecretKey(Box::new(Zeroizing::new([0u8; KEY_LEN])))
    }

    pub(crate) fn bytes_mut(&mut self) -> &mut [u8; KEY_LEN] {
        &mut self.0
    }

    /// A fresh key from the OS random source.
    pub fn random() -> Self {
        let mut key = Self::zeroed();
        OsRng.fill_bytes(key.bytes_mut());
        key
    }

    /// Copies a key from `bytes`, which must be exactly [`KEY_LEN`] long. The caller still owns
    /// (and wipes) its own copy.
    pub fn from_slice(bytes: &[u8]) -> Result<Self, KeyringError> {
        if bytes.len() != KEY_LEN {
            return Err(KeyringError::KeyLength(bytes.len()));
        }
        let mut key = Self::zeroed();
        key.bytes_mut().copy_from_slice(bytes);
        Ok(key)
    }

    /// The key's bytes, for a cipher or a KDF. Copy them only into a buffer that is wiped.
    pub fn expose_secret(&self) -> &[u8; KEY_LEN] {
        &self.0
    }
}

impl fmt::Debug for SecretKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("SecretKey(..)")
    }
}

/// A fresh key from the OS random source.
pub fn random_key() -> SecretKey {
    SecretKey::random()
}

/// HKDF-SHA256 of `key` with `salt` and `info`, 32 bytes: a key for one purpose. An empty
/// `salt` is RFC 5869's "no salt".
pub fn hkdf_subkey(key: &SecretKey, salt: &[u8], info: &str) -> SecretKey {
    let mut subkey = SecretKey::zeroed();
    Hkdf::<Sha256>::new(Some(salt), key.expose_secret())
        .expand(info.as_bytes(), subkey.bytes_mut())
        .expect("32 bytes is a valid HKDF-SHA256 output length");
    subkey
}
