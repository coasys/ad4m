//! `ad4m-keyring`: symmetric keys and the two ways a key is kept for someone else.
//!
//! - [`SecretKey`]: 32 bytes on the heap, zeroed when dropped, never cloned, never printed.
//!   [`random_key`] draws one from the OS source (`crypto.getRandomValues` on wasm32);
//!   [`hkdf_subkey`] derives one for one purpose.
//! - [`wrap`] / [`unwrap`]: a key kept under another symmetric key, for data at rest.
//!   XChaCha20-Poly1305 with a random nonce per wrap, under a key derived from the wrapping
//!   key for wraps alone ([`tags::DEK_WRAP_V1`]); the [`WrapContext`] (the keyring, the
//!   wrapped key and its wrapper, each with its version) is the associated data, so a wrap
//!   opens only in the place it was made for.
//! - [`seal_to_x25519`] / [`open_from_x25519`]: a room key sealed to an X25519 public key,
//!   byte for byte the sealed box of the server link language (`sealToDid` in
//!   `bootstrap-languages/server-link-language/src/encryption.ts`) and of the executor's
//!   `agent/sealed_box.rs`. [`x25519_public_of_ed25519`] and [`x25519_secret_of_ed25519`]
//!   give the X25519 keys of a `did:key`'s Ed25519 key.
//! - [`VersionedKey`]: the versions of one key. Versions start at 1 and only grow; old
//!   versions stay readable until their owner retires them.
//!
//! The crate does no I/O and reads no clock. It builds for `wasm32-unknown-unknown`.

#![forbid(unsafe_code)]

mod error;
mod hex_serde;
mod key;
mod seal;
mod version;
mod wrap;

pub use error::KeyringError;
pub use key::{hkdf_subkey, random_key, SecretKey, KEY_LEN};
pub use seal::{
    open_from_x25519, seal_to_x25519, seal_to_x25519_with_rng, x25519_public_of_ed25519,
    x25519_secret_of_ed25519, SealedEnvelope, SEAL_NONCE_LEN,
};
pub use version::{KeyVersion, VersionedKey};
pub use wrap::{unwrap, wrap, KeyRef, SymWrap, WrapAlg, WrapContext, WRAP_NONCE_LEN};

/// Domain tags. Each use has its own; no tag is shared between uses. Changing one makes
/// everything made under it unreadable.
pub mod tags {
    /// HKDF `info` of the key every wrap is made under, and the first field of its
    /// associated data.
    pub const DEK_WRAP_V1: &str = "ad4m:keyring:dek-wrap:v1";
    /// HKDF `info` of the room-key seal: the server link language's `SEALED_BOX_INFO` and the
    /// executor's sealed boxes.
    pub const ROOM_KEY_SEAL_V1: &str = "ad4m:sealed-box:v1";
}
