//! wasm-bindgen exports of `ad4m-keyring` (AR-08 K9): the one implementation of the room-key
//! seal for the server link language (built `--target deno`) and the link server (`--target
//! nodejs`). `build.sh` builds both.
//!
//! ```ts
//! type SealedBox = { ephemeralPublicKey: string; nonce: string; ciphertext: string }; // hex
//! function roomKeyContext(roomId: string, version: number): string; // coasys:room-key:<roomId>:<version>
//! function sealRoomKey(roomKey: Uint8Array, recipientX25519: Uint8Array, context: string): SealedBox;
//! function openRoomKey(sealed: SealedBox, myX25519Secret: Uint8Array, context: string): Uint8Array;
//! function x25519Public(secret: Uint8Array): Uint8Array;
//! function linkHashKey(roomKeyV1: Uint8Array): Uint8Array;
//! function randomRoomKey(): Uint8Array;
//! ```
//!
//! The seal is the keyring's room-key seal v2 (`seal_room_key` / `open_room_key`): a room key
//! version is bound to its room and version by the context
//! `coasys:room-key:<roomId>:<version>` (`roomKeyContext`), the AES-GCM associated data, under
//! its own HKDF tag (`ad4m:room-key-seal:v2`). A box opens only under the context it was sealed
//! with, so a server cannot serve one room's box (or one version's) as another's. The envelope
//! keeps the shape of the language's `SealedBox`; `vectors/room-key-seal-v2.json` pins the
//! bytes. An empty context is refused. Every error is thrown as an `Error` whose message says
//! what was wrong; none carries key bytes. Randomness comes from `crypto.getRandomValues`.
//!
//! # Where secret bytes live
//!
//! In the wasm memory, nothing secret outlives a call:
//! - A secret argument (`roomKey`, `myX25519Secret`, `x25519Public`'s `secret`, `roomKeyV1`) is
//!   taken as an owned `Vec<u8>`: the copy wasm-bindgen's glue makes of the caller's array in
//!   the wasm memory is the Rust function's own, and it is wiped before the call returns,
//!   on an error too (`take_secret`). (A `&[u8]` argument would be freed by the glue unwiped.)
//! - Every key the code holds is a `SecretKey`, a `Zeroizing` buffer or an x25519
//!   `StaticSecret` (its `zeroize` feature), all wiped when dropped.
//! - A secret result (`openRoomKey`, `linkHashKey`, `randomRoomKey`) is copied straight from a
//!   wiped buffer into a new JS `Uint8Array`; no `Vec<u8>` of it is left for the glue to free
//!   unwiped.
//! - The JSON text built or read for a `SealedBox` holds only the ephemeral public key, the
//!   nonce and the ciphertext: nothing secret.
//!
//! What stays in JS memory, out of this crate's reach: the caller's own `Uint8Array`s passed
//! in, and the `Uint8Array`s returned. The caller wipes them (`fill(0)`) when done; the JS
//! engine may still have copied them (garbage collection moves objects), which no wasm code
//! can wipe. The wasm allocator does not clear freed memory, and the memory lasts as long as
//! the module, which is why every secret buffer above is wiped before it is freed.

#![forbid(unsafe_code)]

use ad4m_keyring::{
    hkdf_subkey, open_room_key as open_bound, random_key, room_key_context as context_of,
    seal_room_key as seal_bound, tags, KeyringError, SealedEnvelope, SecretKey,
};
use js_sys::Uint8Array;
use wasm_bindgen::prelude::*;
use x25519_dalek::{PublicKey, StaticSecret};
use zeroize::{Zeroize, Zeroizing};

fn thrown(e: KeyringError) -> JsError {
    JsError::new(&e.to_string())
}

/// Exactly 32 bytes, or a `KeyLength` error.
fn bytes32(bytes: &[u8]) -> Result<Zeroizing<[u8; 32]>, KeyringError> {
    if bytes.len() != 32 {
        return Err(KeyringError::KeyLength(bytes.len()));
    }
    let mut out = Zeroizing::new([0u8; 32]);
    out.copy_from_slice(bytes);
    Ok(out)
}

/// Moves a 32-byte secret out of `bytes` into a buffer wiped on drop, and wipes `bytes`,
/// whether it had the right length or not.
fn take_secret(bytes: &mut [u8]) -> Result<Zeroizing<[u8; 32]>, KeyringError> {
    let secret = bytes32(bytes);
    bytes.zeroize();
    secret
}

/// A secret result as a new JS `Uint8Array`, copied from the wiped buffer that holds it.
fn to_js(secret: &[u8]) -> Uint8Array {
    Uint8Array::from(secret)
}

/// Seals the 32-byte `room_key` to the X25519 public key `recipient_x25519`, bound to
/// `context` (`roomKeyContext(roomId, version)`), with a fresh ephemeral key and nonce: the
/// `SealedBox` object.
#[wasm_bindgen(js_name = sealRoomKey)]
pub fn seal_room_key(
    mut room_key: Vec<u8>,
    recipient_x25519: &[u8],
    context: &str,
) -> Result<JsValue, JsError> {
    let room_key = take_secret(&mut room_key).map_err(thrown)?;
    let key = SecretKey::from_slice(&*room_key).map_err(thrown)?;
    let recipient = bytes32(recipient_x25519).map_err(thrown)?;
    let envelope = seal_bound(&key, &recipient, context).map_err(thrown)?;
    let text = serde_json::to_string(&envelope).map_err(|e| JsError::new(&e.to_string()))?;
    js_sys::JSON::parse(&text).map_err(|_| JsError::new("the envelope has no JSON form"))
}

/// Opens a `SealedBox` sealed to the X25519 public key of `my_x25519_secret` under `context`:
/// the room key. Throws for a box sealed for another room or version.
#[wasm_bindgen(js_name = openRoomKey)]
pub fn open_room_key(
    sealed: JsValue,
    mut my_x25519_secret: Vec<u8>,
    context: &str,
) -> Result<Uint8Array, JsError> {
    let secret = take_secret(&mut my_x25519_secret).map_err(thrown)?;
    let text = js_sys::JSON::stringify(&sealed)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| JsError::new("the sealed box is not JSON"))?;
    let envelope: SealedEnvelope =
        serde_json::from_str(&text).map_err(|e| JsError::new(&format!("not a sealed box: {e}")))?;
    let key = open_bound(&envelope, &secret, context).map_err(thrown)?;
    Ok(to_js(key.expose_secret()))
}

/// The context a room key version is sealed under: `coasys:room-key:<roomId>:<version>`.
#[wasm_bindgen(js_name = roomKeyContext)]
pub fn room_key_context(room_id: &str, version: u32) -> String {
    context_of(room_id, version)
}

/// The X25519 public key of a 32-byte X25519 secret (clamped as X25519 clamps it).
#[wasm_bindgen(js_name = x25519Public)]
pub fn x25519_public(mut secret: Vec<u8>) -> Result<Vec<u8>, JsError> {
    let secret = StaticSecret::from(*take_secret(&mut secret).map_err(thrown)?);
    Ok(PublicKey::from(&secret).to_bytes().to_vec())
}

/// The link-hash key of a room: HKDF-SHA256 of the room key's version 1, no salt,
/// `info = "adam-server-link-language:link-hash:v1"` (K4).
#[wasm_bindgen(js_name = linkHashKey)]
pub fn link_hash_key(mut room_key_v1: Vec<u8>) -> Result<Uint8Array, JsError> {
    let room_key_v1 = take_secret(&mut room_key_v1).map_err(thrown)?;
    let key = SecretKey::from_slice(&*room_key_v1).map_err(thrown)?;
    Ok(to_js(
        hkdf_subkey(&key, &[], tags::LINK_HASH_V1).expose_secret(),
    ))
}

/// A fresh 32-byte room key from `crypto.getRandomValues`.
#[wasm_bindgen(js_name = randomRoomKey)]
pub fn random_room_key() -> Uint8Array {
    to_js(random_key().expose_secret())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_secret_argument_is_wiped_once_taken() {
        let mut arg: Vec<u8> = (1..=32).collect();
        let taken = take_secret(&mut arg).unwrap();
        assert_eq!(taken.to_vec(), (1..=32).collect::<Vec<u8>>());
        assert_eq!(arg, vec![0u8; 32]);
    }

    #[test]
    fn a_secret_argument_of_the_wrong_length_is_wiped_too() {
        for len in [0usize, 16, 31, 33, 64] {
            let mut arg = vec![7u8; len];
            assert!(matches!(
                take_secret(&mut arg),
                Err(KeyringError::KeyLength(l)) if l == len
            ));
            assert_eq!(arg, vec![0u8; len]);
        }
    }
}
