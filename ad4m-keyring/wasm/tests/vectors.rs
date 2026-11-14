//! The JS exports against the room-key seal v2 vectors
//! (`ad4m-keyring/vectors/room-key-seal-v2.json`, made by the language's noble reference), byte
//! for byte, each bound to its room and version (K9).
//!
//! The seal draws its randomness from `crypto.getRandomValues`; `serveRandom` makes the next
//! draws hand out the vector's ephemeral secret, then its nonce, so the export itself must give
//! the vector's envelope.

#![cfg(target_arch = "wasm32")]

use ad4m_keyring_wasm::{
    link_hash_key, open_room_key, random_room_key, room_key_context, seal_room_key, x25519_public,
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
    room_id: String,
    version: u32,
    context: String,
    ephemeral_secret: String,
    nonce: String,
    sealed: serde_json::Value,
}

#[derive(Deserialize)]
struct Vectors {
    vectors: Vec<Vector>,
}

fn vectors() -> Vec<Vector> {
    let vectors =
        serde_json::from_str::<Vectors>(include_str!("../../vectors/room-key-seal-v2.json"))
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
fn seals_each_vector_as_the_reference_does() {
    for v in vectors() {
        serve_random(&[bytes(&v.ephemeral_secret), bytes(&v.nonce)].concat());
        let sealed =
            seal_room_key(bytes(&v.room_key), &bytes(&v.x25519_public), &v.context).unwrap();
        assert_eq!(json(&sealed), v.sealed);
    }
}

#[test]
fn opens_each_vector_to_its_room_key() {
    for v in vectors() {
        let sealed = parse(&v.sealed.to_string());
        let opened = open_room_key(sealed, bytes(&v.x25519_secret), &v.context).unwrap();
        assert_eq!(opened.to_vec(), bytes(&v.room_key));
    }
}

#[test]
fn the_context_names_the_room_and_the_version() {
    for v in vectors() {
        assert_eq!(room_key_context(&v.room_id, v.version), v.context);
    }
}

#[test]
fn a_vector_does_not_open_as_another_room_or_version() {
    for v in vectors() {
        for wrong in [
            room_key_context(&format!("{}x", v.room_id), v.version),
            room_key_context(&v.room_id, v.version + 1),
            String::new(),
        ] {
            let sealed = parse(&v.sealed.to_string());
            assert!(
                open_room_key(sealed, bytes(&v.x25519_secret), &wrong).is_err(),
                "{wrong}"
            );
        }
    }
}

#[test]
fn derives_each_vector_public_key_from_its_secret() {
    for v in vectors() {
        assert_eq!(
            x25519_public(bytes(&v.x25519_secret)).unwrap(),
            bytes(&v.x25519_public)
        );
    }
}

#[test]
fn a_fresh_seal_opens_only_for_its_recipient() {
    let v = &vectors()[0];
    let other = &vectors()[1];
    let key = random_room_key().to_vec();
    assert_eq!(key.len(), 32);
    let sealed = seal_room_key(key.clone(), &bytes(&v.x25519_public), &v.context).unwrap();
    assert_eq!(
        open_room_key(sealed.clone(), bytes(&v.x25519_secret), &v.context)
            .unwrap()
            .to_vec(),
        key
    );
    assert!(open_room_key(sealed, bytes(&other.x25519_secret), &v.context).is_err());
}

#[test]
fn refuses_keys_of_the_wrong_length() {
    let v = &vectors()[0];
    assert!(seal_room_key(vec![7u8; 31], &bytes(&v.x25519_public), &v.context).is_err());
    assert!(seal_room_key(bytes(&v.room_key), &[7u8; 33], &v.context).is_err());
    assert!(seal_room_key(bytes(&v.room_key), &bytes(&v.x25519_public), "").is_err());
    assert!(x25519_public(vec![7u8; 16]).is_err());
    assert!(link_hash_key(vec![7u8; 31]).is_err());
    let sealed = parse(&v.sealed.to_string());
    assert!(open_room_key(sealed, vec![7u8; 31], &v.context).is_err());
    assert!(open_room_key(
        parse("{\"nonce\":\"00\"}"),
        bytes(&v.x25519_secret),
        &v.context
    )
    .is_err());
}

#[test]
fn the_link_hash_key_is_hkdf_of_room_key_v1() {
    let room_key = bytes(&vectors()[0].room_key);
    let mut expected = [0u8; 32];
    Hkdf::<Sha256>::new(None, &room_key)
        .expand(b"adam-server-link-language:link-hash:v1", &mut expected)
        .unwrap();
    assert_eq!(
        link_hash_key(room_key.clone()).unwrap().to_vec(),
        expected.to_vec()
    );
}
