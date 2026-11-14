//! The JS exports against the server link language's own envelopes
//! (`ad4m-keyring/vectors/room-key-seal.json`, made by `sealToDid`), byte for byte (K9).
//!
//! The seal draws its randomness from `crypto.getRandomValues`; `serveRandom` makes the next
//! draws hand out the vector's ephemeral secret, then its nonce, so the export itself must give
//! the language's envelope.

#![cfg(target_arch = "wasm32")]

use ad4m_keyring_wasm::{
    link_hash_key, open_room_key, random_room_key, seal_room_key, x25519_public,
};
use hkdf::Hkdf;
use serde::Deserialize;
use sha2::Sha256;
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::wasm_bindgen_test as test;

#[wasm_bindgen(inline_js = "
export function serveRandom(bytes) {
    const crypto = globalThis.crypto;
    const queue = Array.from(bytes);
    crypto.getRandomValues = function (buffer) {
        if (queue.length < buffer.length) {
            delete crypto.getRandomValues;
            throw new Error('the seal drew more than the language does');
        }
        for (let i = 0; i < buffer.length; i++) buffer[i] = queue.shift();
        if (queue.length === 0) delete crypto.getRandomValues;
        return buffer;
    };
}
export function stringify(value) { return JSON.stringify(value); }
export function parse(text) { return JSON.parse(text); }
")]
extern "C" {
    #[wasm_bindgen(js_name = serveRandom)]
    fn serve_random(bytes: &[u8]);
    fn stringify(value: &JsValue) -> String;
    fn parse(text: &str) -> JsValue;
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Vector {
    x25519_public: String,
    x25519_secret: String,
    room_key: String,
    ephemeral_secret: String,
    nonce: String,
    sealed: serde_json::Value,
}

#[derive(Deserialize)]
struct Vectors {
    vectors: Vec<Vector>,
}

fn vectors() -> Vec<Vector> {
    let vectors = serde_json::from_str::<Vectors>(include_str!("../../vectors/room-key-seal.json"))
        .unwrap()
        .vectors;
    assert!(!vectors.is_empty());
    vectors
}

fn bytes(text: &str) -> Vec<u8> {
    hex::decode(text).unwrap()
}

fn json(value: &JsValue) -> serde_json::Value {
    serde_json::from_str(&stringify(value)).unwrap()
}

#[test]
fn seals_each_vector_as_the_language_does() {
    for v in vectors() {
        serve_random(&[bytes(&v.ephemeral_secret), bytes(&v.nonce)].concat());
        let sealed = seal_room_key(&bytes(&v.room_key), &bytes(&v.x25519_public)).unwrap();
        assert_eq!(json(&sealed), v.sealed);
    }
}

#[test]
fn opens_each_vector_to_its_room_key() {
    for v in vectors() {
        let sealed = parse(&v.sealed.to_string());
        let opened = open_room_key(sealed, &bytes(&v.x25519_secret)).unwrap();
        assert_eq!(opened, bytes(&v.room_key));
    }
}

#[test]
fn derives_each_vector_public_key_from_its_secret() {
    for v in vectors() {
        assert_eq!(
            x25519_public(&bytes(&v.x25519_secret)).unwrap(),
            bytes(&v.x25519_public)
        );
    }
}

#[test]
fn a_fresh_seal_opens_only_for_its_recipient() {
    let v = &vectors()[0];
    let other = &vectors()[1];
    let key = random_room_key();
    assert_eq!(key.len(), 32);
    let sealed = seal_room_key(&key, &bytes(&v.x25519_public)).unwrap();
    assert_eq!(
        open_room_key(sealed.clone(), &bytes(&v.x25519_secret)).unwrap(),
        key
    );
    assert!(open_room_key(sealed, &bytes(&other.x25519_secret)).is_err());
}

#[test]
fn refuses_keys_of_the_wrong_length() {
    let v = &vectors()[0];
    assert!(seal_room_key(&[7u8; 31], &bytes(&v.x25519_public)).is_err());
    assert!(seal_room_key(&bytes(&v.room_key), &[7u8; 33]).is_err());
    assert!(x25519_public(&[7u8; 16]).is_err());
    assert!(link_hash_key(&[7u8; 31]).is_err());
    let sealed = parse(&v.sealed.to_string());
    assert!(open_room_key(sealed, &[7u8; 31]).is_err());
    assert!(open_room_key(parse("{\"nonce\":\"00\"}"), &bytes(&v.x25519_secret)).is_err());
}

#[test]
fn the_link_hash_key_is_hkdf_of_room_key_v1() {
    let room_key = bytes(&vectors()[0].room_key);
    let mut expected = [0u8; 32];
    Hkdf::<Sha256>::new(None, &room_key)
        .expand(b"adam-server-link-language:link-hash:v1", &mut expected)
        .unwrap();
    assert_eq!(link_hash_key(&room_key).unwrap(), expected.to_vec());
}
