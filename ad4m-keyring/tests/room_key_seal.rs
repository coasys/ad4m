//! The room-key seal v2: a room key sealed to an X25519 key and bound to its room and version
//! (`coasys:room-key:<roomId>:<version>`, AR-08 K9), against the envelopes of the noble
//! reference (`vectors/room-key-seal-v2.json`, made by
//! `bootstrap-languages/server-link-language/tests/room-key-seal-reference.ts`).

use ad4m_keyring::{
    open_from_x25519, open_room_key, random_key, room_key_context, seal_room_key,
    seal_room_key_with_rng, seal_to_x25519, tags, x25519_public_of_ed25519,
    x25519_secret_of_ed25519, KeyringError, SealedEnvelope, SecretKey,
};
use rand_core::{CryptoRng, RngCore};
use serde::Deserialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

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
    serde_json::from_str::<Vectors>(include_str!("../vectors/room-key-seal-v2.json"))
        .unwrap()
        .vectors
}

fn bytes32(text: &str) -> [u8; 32] {
    hex::decode(text).unwrap().try_into().unwrap()
}

/// Hands out a fixed byte stream: the ephemeral secret, then the nonce.
struct Fixed(Vec<u8>);

impl RngCore for Fixed {
    fn next_u32(&mut self) -> u32 {
        unimplemented!("the seal draws bytes only")
    }
    fn next_u64(&mut self) -> u64 {
        unimplemented!("the seal draws bytes only")
    }
    fn fill_bytes(&mut self, dest: &mut [u8]) {
        assert!(
            self.0.len() >= dest.len(),
            "the seal drew more than 44 bytes"
        );
        let rest = self.0.split_off(dest.len());
        dest.copy_from_slice(&self.0);
        self.0 = rest;
    }
    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand_core::Error> {
        self.fill_bytes(dest);
        Ok(())
    }
}
impl CryptoRng for Fixed {}

#[test]
fn the_tag_is_its_own() {
    assert_eq!(tags::ROOM_KEY_SEAL_V2, "ad4m:room-key-seal:v2");
    assert_ne!(tags::ROOM_KEY_SEAL_V2, tags::ROOM_KEY_SEAL_V1);
}

#[test]
fn the_context_names_the_room_and_the_version() {
    for v in vectors() {
        assert_eq!(room_key_context(&v.room_id, v.version), v.context);
    }
    assert_eq!(room_key_context("r", 3), "coasys:room-key:r:3");
}

#[test]
fn the_same_inputs_give_the_reference_envelope_byte_for_byte() {
    for v in vectors() {
        let room_key = SecretKey::from_slice(&hex::decode(&v.room_key).unwrap()).unwrap();
        let mut stream = hex::decode(&v.ephemeral_secret).unwrap();
        stream.extend(hex::decode(&v.nonce).unwrap());
        let mut rng = Fixed(stream);
        let sealed =
            seal_room_key_with_rng(&room_key, &bytes32(&v.x25519_public), &v.context, &mut rng)
                .unwrap();
        assert!(rng.0.is_empty(), "the seal drew less than 44 bytes");
        assert_eq!(
            serde_json::to_value(&sealed).unwrap(),
            v.sealed,
            "{}",
            v.context
        );
    }
}

#[test]
fn it_opens_the_reference_envelopes_under_their_context() {
    for v in vectors() {
        let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
        let opened = open_room_key(&sealed, &bytes32(&v.x25519_secret), &v.context).unwrap();
        assert_eq!(hex::encode(opened.expose_secret()), v.room_key);
    }
}

#[test]
fn another_room_or_version_does_not_open() {
    for v in vectors() {
        let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
        let secret = bytes32(&v.x25519_secret);
        let other_room = room_key_context(&format!("{}x", v.room_id), v.version);
        let other_version = room_key_context(&v.room_id, v.version + 1);
        let earlier_version = room_key_context(&v.room_id, v.version - 1);
        for wrong in [other_room, other_version, earlier_version] {
            assert_eq!(
                open_room_key(&sealed, &secret, &wrong).unwrap_err(),
                KeyringError::Open,
                "{wrong}"
            );
        }
    }
}

#[test]
fn the_v1_seal_and_the_v2_seal_do_not_open_as_each_other() {
    let room_key = random_key();
    let secret = x25519_secret_of_ed25519(&[1u8; 32]);
    let public = x25519_public_of_ed25519(&bytes32(
        "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c",
    ))
    .unwrap();
    let context = room_key_context("room", 1);

    let v1 = seal_to_x25519(&room_key, &public).unwrap();
    assert_eq!(
        open_room_key(&v1, &secret, &context).unwrap_err(),
        KeyringError::Open
    );
    let v2 = seal_room_key(&room_key, &public, &context).unwrap();
    assert_eq!(
        open_from_x25519(&v2, &secret).unwrap_err(),
        KeyringError::Open
    );
    assert_eq!(
        open_room_key(&v2, &secret, &context)
            .unwrap()
            .expose_secret(),
        room_key.expose_secret()
    );
}

#[test]
fn a_fresh_seal_opens_only_for_its_recipient_and_context() {
    let room_key = random_key();
    let alice = x25519_secret_of_ed25519(&[1u8; 32]);
    let bob = x25519_secret_of_ed25519(&[2u8; 32]);
    let alice_public = x25519_public_of_ed25519(&bytes32(
        "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c",
    ))
    .unwrap();
    let context = room_key_context("room-a", 4);

    let sealed = seal_room_key(&room_key, &alice_public, &context).unwrap();
    assert_eq!(sealed.ciphertext.len(), 32 + 16);
    assert_eq!(
        open_room_key(&sealed, &alice, &context)
            .unwrap()
            .expose_secret(),
        room_key.expose_secret()
    );
    assert_eq!(
        open_room_key(&sealed, &bob, &context).unwrap_err(),
        KeyringError::Open
    );
    let again = seal_room_key(&room_key, &alice_public, &context).unwrap();
    assert_ne!(again.ephemeral_public_key, sealed.ephemeral_public_key);
    assert_ne!(again.nonce, sealed.nonce);
}

#[test]
fn an_empty_context_is_refused_both_ways() {
    let room_key = random_key();
    let alice = x25519_secret_of_ed25519(&[1u8; 32]);
    let alice_public = x25519_public_of_ed25519(&bytes32(
        "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c",
    ))
    .unwrap();
    assert_eq!(
        seal_room_key(&room_key, &alice_public, "").unwrap_err(),
        KeyringError::EmptyContext
    );
    let sealed = seal_room_key(&room_key, &alice_public, "c").unwrap();
    assert_eq!(
        open_room_key(&sealed, &alice, "").unwrap_err(),
        KeyringError::EmptyContext
    );
}

#[test]
fn an_altered_envelope_does_not_open() {
    let v = &vectors()[0];
    let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
    let secret = bytes32(&v.x25519_secret);
    for i in 0..sealed.ciphertext.len() {
        let mut altered = sealed.clone();
        altered.ciphertext[i] ^= 1;
        assert_eq!(
            open_room_key(&altered, &secret, &v.context).unwrap_err(),
            KeyringError::Open,
            "{i}"
        );
    }
    let mut altered = sealed.clone();
    altered.nonce[0] ^= 1;
    assert_eq!(
        open_room_key(&altered, &secret, &v.context).unwrap_err(),
        KeyringError::Open
    );
}

#[test]
fn a_low_order_key_is_refused_both_ways() {
    let room_key = random_key();
    let low = [0u8; 32];
    assert_eq!(
        seal_room_key(&room_key, &low, "c").unwrap_err(),
        KeyringError::LowOrderKey
    );
    let envelope = SealedEnvelope {
        ephemeral_public_key: low,
        nonce: [0; 12],
        ciphertext: vec![0; 48],
    };
    assert_eq!(
        open_room_key(&envelope, &x25519_secret_of_ed25519(&[1u8; 32]), "c").unwrap_err(),
        KeyringError::LowOrderKey
    );
}
