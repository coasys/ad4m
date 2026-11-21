//! The room-key seal: a key sealed to an X25519 public key, which only the holder of the
//! matching secret opens.
//!
//! An ephemeral X25519 key pair, ECDH with the recipient's key,
//! `HKDF-SHA256(shared, salt = ephemeral public ‖ recipient public, info = ROOM_KEY_SEAL_V1)`
//! as an AES-256-GCM key, and a random 12-byte nonce; on the wire
//! `{ ephemeralPublicKey, nonce, ciphertext }`, each hex. This is `sealToDid` of the server
//! link language and the executor's sealed box, byte for byte: the same ephemeral secret and
//! nonce give the same envelope (`vectors/room-key-seal.json`). The randomness is drawn in the
//! language's order: the ephemeral secret, then the nonce.
//!
//! A `did:key` recipient's X25519 public key is the RFC 7748 map of its Ed25519 key
//! ([`x25519_public_of_ed25519`]); its holder's X25519 secret is the clamped first half of
//! `SHA-512(seed)` ([`x25519_secret_of_ed25519`]), as libsodium and noble compute them.

use aes_gcm::aead::{AeadInPlace, KeyInit};
use aes_gcm::{Aes256Gcm, Nonce};
use curve25519_dalek::edwards::CompressedEdwardsY;
use hkdf::Hkdf;
use rand_core::{CryptoRng, OsRng, RngCore};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256, Sha512};
use x25519_dalek::{PublicKey, StaticSecret};
use zeroize::Zeroizing;

use crate::{hex_serde, tags, KeyringError, SecretKey, KEY_LEN};

/// The bytes of a seal's AES-GCM nonce.
pub const SEAL_NONCE_LEN: usize = 12;
const TAG_LEN: usize = 16;

/// A key sealed to one X25519 public key. The server link language's `SealedBox`:
/// `{ "ephemeralPublicKey", "nonce", "ciphertext" }`, lowercase hex.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SealedEnvelope {
    #[serde(with = "hex_serde::array")]
    pub ephemeral_public_key: [u8; 32],
    #[serde(with = "hex_serde::array")]
    pub nonce: [u8; SEAL_NONCE_LEN],
    #[serde(with = "hex_serde::vec")]
    pub ciphertext: Vec<u8>,
}

/// The AES-256-GCM key of one seal. Refuses an all-zero shared secret (a low-order key, so
/// nothing secret went in), as noble's `x25519.getSharedSecret` does.
fn seal_cipher(
    secret: &StaticSecret,
    their_public: &[u8; 32],
    ephemeral_public: &[u8; 32],
    recipient_public: &[u8; 32],
) -> Result<Aes256Gcm, KeyringError> {
    let shared = secret.diffie_hellman(&PublicKey::from(*their_public));
    if !shared.was_contributory() {
        return Err(KeyringError::LowOrderKey);
    }
    let mut salt = [0u8; 64];
    salt[..32].copy_from_slice(ephemeral_public);
    salt[32..].copy_from_slice(recipient_public);
    let mut key = Zeroizing::new([0u8; KEY_LEN]);
    Hkdf::<Sha256>::new(Some(&salt), shared.as_bytes())
        .expand(tags::ROOM_KEY_SEAL_V1.as_bytes(), key.as_mut())
        .expect("32 bytes is a valid HKDF-SHA256 output length");
    Ok(Aes256Gcm::new(key.as_ref().into()))
}

/// Seals `key` to `recipient_public` with a fresh ephemeral key and nonce from the OS source.
pub fn seal_to_x25519(
    key: &SecretKey,
    recipient_public: &[u8; 32],
) -> Result<SealedEnvelope, KeyringError> {
    seal_to_x25519_with_rng(key, recipient_public, &mut OsRng)
}

/// [`seal_to_x25519`] drawing from `rng`: 32 bytes of ephemeral secret, then 12 bytes of
/// nonce, the order of the language's `sealToDid`, so a fixed byte stream gives its envelope.
/// Pass anything but a cryptographic RNG only to reproduce a test vector.
pub fn seal_to_x25519_with_rng<R: RngCore + CryptoRng>(
    key: &SecretKey,
    recipient_public: &[u8; 32],
    rng: &mut R,
) -> Result<SealedEnvelope, KeyringError> {
    let mut ephemeral_bytes = Zeroizing::new([0u8; 32]);
    rng.fill_bytes(ephemeral_bytes.as_mut());
    let mut nonce = [0u8; SEAL_NONCE_LEN];
    rng.fill_bytes(&mut nonce);

    let ephemeral = StaticSecret::from(*ephemeral_bytes);
    let ephemeral_public = PublicKey::from(&ephemeral).to_bytes();
    let cipher = seal_cipher(
        &ephemeral,
        recipient_public,
        &ephemeral_public,
        recipient_public,
    )?;

    // Room for the tag up front: the buffer never reallocates, so no unwiped copy of the key
    // is freed.
    let mut buffer = Zeroizing::new(Vec::with_capacity(KEY_LEN + TAG_LEN));
    buffer.extend_from_slice(key.expose_secret());
    cipher
        .encrypt_in_place(Nonce::from_slice(&nonce), b"", &mut *buffer)
        .expect("AES-256-GCM seals 32 bytes");
    Ok(SealedEnvelope {
        ephemeral_public_key: ephemeral_public,
        nonce,
        ciphertext: std::mem::take(&mut *buffer),
    })
}

/// Opens an envelope sealed to the X25519 public key of `my_secret`. Fails with
/// [`KeyringError::Open`] for an envelope sealed to anyone else or altered, and with
/// [`KeyringError::KeyLength`] when it opens to something that is not a 32-byte key.
pub fn open_from_x25519(
    envelope: &SealedEnvelope,
    my_secret: &[u8; 32],
) -> Result<SecretKey, KeyringError> {
    let secret = StaticSecret::from(*my_secret);
    let recipient_public = PublicKey::from(&secret).to_bytes();
    let cipher = seal_cipher(
        &secret,
        &envelope.ephemeral_public_key,
        &envelope.ephemeral_public_key,
        &recipient_public,
    )?;
    let mut buffer = Zeroizing::new(envelope.ciphertext.clone());
    cipher
        .decrypt_in_place(Nonce::from_slice(&envelope.nonce), b"", &mut *buffer)
        .map_err(|_| KeyringError::Open)?;
    SecretKey::from_slice(&buffer)
}

/// The X25519 public key of an Ed25519 public key (the RFC 7748 birational map; libsodium's
/// `crypto_sign_ed25519_pk_to_curve25519`, noble's `ed25519.utils.toMontgomery`). Refuses
/// bytes that are not a point, and points of small order.
pub fn x25519_public_of_ed25519(ed25519_public: &[u8; 32]) -> Result<[u8; 32], KeyringError> {
    CompressedEdwardsY(*ed25519_public)
        .decompress()
        .filter(|point| !point.is_small_order())
        .map(|point| point.to_montgomery().to_bytes())
        .ok_or(KeyringError::InvalidEd25519Key)
}

/// The X25519 secret of an Ed25519 seed: the clamped first half of `SHA-512(seed)` (libsodium's
/// `crypto_sign_ed25519_sk_to_curve25519`, noble's `ed25519.utils.toMontgomerySecret`).
pub fn x25519_secret_of_ed25519(seed: &[u8; 32]) -> Zeroizing<[u8; 32]> {
    let mut hash = Zeroizing::new([0u8; 64]);
    hash.copy_from_slice(&Sha512::digest(seed));
    let mut scalar = Zeroizing::new([0u8; 32]);
    scalar.copy_from_slice(&hash[..32]);
    scalar[0] &= 248;
    scalar[31] &= 127;
    scalar[31] |= 64;
    scalar
}
