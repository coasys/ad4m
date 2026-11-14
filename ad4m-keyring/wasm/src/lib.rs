//! wasm-bindgen exports of `ad4m-keyring` (AR-08 K9): the one implementation of the room-key
//! seal for the server link language (built `--target deno`) and the link server (`--target
//! nodejs`). `build.sh` builds both.
//!
//! ```ts
//! type SealedBox = { ephemeralPublicKey: string; nonce: string; ciphertext: string }; // hex
//! function sealRoomKey(roomKey: Uint8Array, recipientX25519: Uint8Array): SealedBox;
//! function openRoomKey(sealed: SealedBox, myX25519Secret: Uint8Array): Uint8Array;
//! function x25519Public(secret: Uint8Array): Uint8Array;
//! function linkHashKey(roomKeyV1: Uint8Array): Uint8Array;
//! function randomRoomKey(): Uint8Array;
//! ```
//!
//! The envelope is the language's `SealedBox` (`sealToDid`) byte for byte, so nothing on the
//! wire changes; the seal binds no context (the language's seal has no associated data, and
//! `vectors/room-key-seal.json` pins it). Every error is thrown as an `Error` whose message
//! says what was wrong; none carries key bytes. Randomness comes from `crypto.getRandomValues`.

#![forbid(unsafe_code)]

use ad4m_keyring::{
    hkdf_subkey, open_from_x25519, random_key, seal_to_x25519, tags, KeyringError, SealedEnvelope,
    SecretKey,
};
use wasm_bindgen::prelude::*;
use x25519_dalek::{PublicKey, StaticSecret};
use zeroize::Zeroizing;

fn thrown(e: KeyringError) -> JsError {
    JsError::new(&e.to_string())
}

/// Exactly 32 bytes, or a `KeyLength` error.
fn bytes32(bytes: &[u8]) -> Result<Zeroizing<[u8; 32]>, JsError> {
    if bytes.len() != 32 {
        return Err(thrown(KeyringError::KeyLength(bytes.len())));
    }
    let mut out = Zeroizing::new([0u8; 32]);
    out.copy_from_slice(bytes);
    Ok(out)
}

/// Seals the 32-byte `room_key` to the X25519 public key `recipient_x25519` with a fresh
/// ephemeral key and nonce: the `SealedBox` object.
#[wasm_bindgen(js_name = sealRoomKey)]
pub fn seal_room_key(room_key: &[u8], recipient_x25519: &[u8]) -> Result<JsValue, JsError> {
    let key = SecretKey::from_slice(room_key).map_err(thrown)?;
    let recipient = bytes32(recipient_x25519)?;
    let envelope = seal_to_x25519(&key, &recipient).map_err(thrown)?;
    let text = serde_json::to_string(&envelope).map_err(|e| JsError::new(&e.to_string()))?;
    js_sys::JSON::parse(&text).map_err(|_| JsError::new("the envelope has no JSON form"))
}

/// Opens a `SealedBox` sealed to the X25519 public key of `my_x25519_secret`: the room key.
#[wasm_bindgen(js_name = openRoomKey)]
pub fn open_room_key(sealed: JsValue, my_x25519_secret: &[u8]) -> Result<Vec<u8>, JsError> {
    let secret = bytes32(my_x25519_secret)?;
    let text = js_sys::JSON::stringify(&sealed)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| JsError::new("the sealed box is not JSON"))?;
    let envelope: SealedEnvelope =
        serde_json::from_str(&text).map_err(|e| JsError::new(&format!("not a sealed box: {e}")))?;
    let key = open_from_x25519(&envelope, &secret).map_err(thrown)?;
    Ok(key.expose_secret().to_vec())
}

/// The X25519 public key of a 32-byte X25519 secret (clamped as X25519 clamps it).
#[wasm_bindgen(js_name = x25519Public)]
pub fn x25519_public(secret: &[u8]) -> Result<Vec<u8>, JsError> {
    let secret = StaticSecret::from(*bytes32(secret)?);
    Ok(PublicKey::from(&secret).to_bytes().to_vec())
}

/// The link-hash key of a room: HKDF-SHA256 of the room key's version 1, no salt,
/// `info = "adam-server-link-language:link-hash:v1"` (K4).
#[wasm_bindgen(js_name = linkHashKey)]
pub fn link_hash_key(room_key_v1: &[u8]) -> Result<Vec<u8>, JsError> {
    let key = SecretKey::from_slice(room_key_v1).map_err(thrown)?;
    Ok(hkdf_subkey(&key, &[], tags::LINK_HASH_V1)
        .expose_secret()
        .to_vec())
}

/// A fresh 32-byte room key from `crypto.getRandomValues`.
#[wasm_bindgen(js_name = randomRoomKey)]
pub fn random_room_key() -> Vec<u8> {
    random_key().expose_secret().to_vec()
}
