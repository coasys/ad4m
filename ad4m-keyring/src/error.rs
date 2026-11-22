use thiserror::Error;

use crate::KeyVersion;

/// Why a key did not open, or a version change was refused. No variant carries key bytes.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum KeyringError {
    /// A wrong wrapping key, another context, or an altered wrap: the AEAD tells them apart
    /// from nothing, on purpose.
    #[error("the wrap does not open under this key in this context")]
    Unwrap,
    /// A sealed envelope for another recipient, or an altered one.
    #[error("the sealed envelope does not open with this key")]
    Open,
    /// A key has [`crate::KEY_LEN`] bytes.
    #[error("a key has 32 bytes, this one has {0}")]
    KeyLength(usize),
    /// An X25519 public key of low order: the shared secret would be all zero.
    #[error("a low-order X25519 key: no secret is shared")]
    LowOrderKey,
    /// Bytes that are not an Ed25519 public key of full order.
    #[error("not a usable Ed25519 public key")]
    InvalidEd25519Key,
    /// No such version of the key (never made, or retired).
    #[error("no key version {0}")]
    UnknownVersion(KeyVersion),
    /// The current version is the one new data is written under; it cannot be retired.
    #[error("{0} is the current version and cannot be retired")]
    RetireCurrent(KeyVersion),
    /// No version follows `u32::MAX`.
    #[error("no version follows {0}")]
    VersionsExhausted(KeyVersion),
    /// A stored set of versions that breaks the rules of [`crate::VersionedKey`].
    #[error("invalid key versions: {0}")]
    InvalidVersions(String),
}
