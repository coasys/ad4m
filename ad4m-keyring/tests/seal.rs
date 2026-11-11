//! The room-key seal against the server link language's own envelopes
//! (`vectors/room-key-seal.json`, made by `sealToDid` in
//! `bootstrap-languages/server-link-language/src/encryption.ts`).

use ad4m_keyring::{
    open_from_x25519, random_key, seal_to_x25519, seal_to_x25519_with_rng, tags,
    x25519_public_of_ed25519, x25519_secret_of_ed25519, KeyringError, SealedEnvelope, SecretKey,
};
use rand_core::{CryptoRng, RngCore};
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Vector {
    ed25519_seed: String,
    ed25519_public: String,
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
    serde_json::from_str::<Vectors>(include_str!("../vectors/room-key-seal.json"))
        .unwrap()
        .vectors
}

fn bytes32(text: &str) -> [u8; 32] {
    hex::decode(text).unwrap().try_into().unwrap()
}

/// Hands out a fixed byte stream: the language's ephemeral secret, then its nonce.
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
            "the seal drew more than the language does"
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
fn the_tag_is_the_languages_sealed_box_info() {
    // `SEALED_BOX_INFO` in encryption.ts and in rust-executor/src/agent/sealed_box.rs.
    assert_eq!(tags::ROOM_KEY_SEAL_V1, "ad4m:sealed-box:v1");
}

#[test]
fn the_x25519_keys_of_a_did_key_match_noble() {
    for v in vectors() {
        assert_eq!(
            hex::encode(x25519_public_of_ed25519(&bytes32(&v.ed25519_public)).unwrap()),
            v.x25519_public
        );
        assert_eq!(
            hex::encode(*x25519_secret_of_ed25519(&bytes32(&v.ed25519_seed))),
            v.x25519_secret
        );
    }
}

#[test]
fn the_same_inputs_give_the_languages_envelope_byte_for_byte() {
    for v in vectors() {
        let room_key = SecretKey::from_slice(&hex::decode(&v.room_key).unwrap()).unwrap();
        let mut stream = hex::decode(&v.ephemeral_secret).unwrap();
        stream.extend(hex::decode(&v.nonce).unwrap());
        let mut rng = Fixed(stream);
        let sealed =
            seal_to_x25519_with_rng(&room_key, &bytes32(&v.x25519_public), &mut rng).unwrap();
        assert!(
            rng.0.is_empty(),
            "the seal drew less than the language does"
        );
        assert_eq!(
            serde_json::to_value(&sealed).unwrap(),
            v.sealed,
            "{}",
            v.room_key
        );
    }
}

#[test]
fn it_opens_the_languages_envelopes() {
    for v in vectors() {
        let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
        let secret = x25519_secret_of_ed25519(&bytes32(&v.ed25519_seed));
        let opened = open_from_x25519(&sealed, &secret).unwrap();
        assert_eq!(hex::encode(opened.expose_secret()), v.room_key);
    }
}

#[test]
fn an_envelope_that_opens_to_something_other_than_a_key_is_refused() {
    // `sealToDid(did, "ad4m room key v1", ephemeral 0x02 * 32, nonce 0x03 * 12)`, pinned in the
    // language's and the executor's tests: it opens, to 16 bytes.
    let sealed: SealedEnvelope = serde_json::from_value(serde_json::json!({
        "ephemeralPublicKey": "ce8d3ad1ccb633ec7b70c17814a5c76ecd029685050d344745ba05870e587d59",
        "nonce": "030303030303030303030303",
        "ciphertext": "11b99acbd7bdf173901c669b2ed8c4794cb801ac9c2ac9642531fd873dfde2cc",
    }))
    .unwrap();
    let secret = x25519_secret_of_ed25519(&[1u8; 32]);
    assert_eq!(
        open_from_x25519(&sealed, &secret).unwrap_err(),
        KeyringError::KeyLength(16)
    );
}

#[test]
fn a_fresh_seal_opens_only_for_its_recipient() {
    let room_key = random_key();
    let alice = x25519_secret_of_ed25519(&[1u8; 32]);
    let bob = x25519_secret_of_ed25519(&[2u8; 32]);
    let alice_public = x25519_public_of_ed25519(&bytes32(
        "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c",
    ))
    .unwrap();

    let sealed = seal_to_x25519(&room_key, &alice_public).unwrap();
    assert_eq!(sealed.ciphertext.len(), 32 + 16);
    assert_eq!(
        open_from_x25519(&sealed, &alice).unwrap().expose_secret(),
        room_key.expose_secret()
    );
    assert_eq!(
        open_from_x25519(&sealed, &bob).unwrap_err(),
        KeyringError::Open
    );

    let again = seal_to_x25519(&room_key, &alice_public).unwrap();
    assert_ne!(
        again.ephemeral_public_key, sealed.ephemeral_public_key,
        "a fresh ephemeral key"
    );
    assert_ne!(again.nonce, sealed.nonce, "a fresh nonce");
}

#[test]
fn an_altered_envelope_does_not_open() {
    let v = &vectors()[0];
    let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
    let secret = x25519_secret_of_ed25519(&bytes32(&v.ed25519_seed));
    for i in 0..sealed.ciphertext.len() {
        let mut altered = sealed.clone();
        altered.ciphertext[i] ^= 1;
        assert_eq!(
            open_from_x25519(&altered, &secret).unwrap_err(),
            KeyringError::Open,
            "{i}"
        );
    }
    let mut altered = sealed.clone();
    altered.nonce[0] ^= 1;
    assert_eq!(
        open_from_x25519(&altered, &secret).unwrap_err(),
        KeyringError::Open
    );
    let mut altered = sealed.clone();
    altered.ephemeral_public_key[0] ^= 2;
    assert!(open_from_x25519(&altered, &secret).is_err());
    let mut altered = sealed.clone();
    altered.ciphertext.truncate(10);
    assert_eq!(
        open_from_x25519(&altered, &secret).unwrap_err(),
        KeyringError::Open
    );
}

#[test]
fn a_low_order_key_is_refused_both_ways() {
    let room_key = random_key();
    // The identity point and a point of order 8 on Curve25519.
    for low in [[0u8; 32], {
        let mut p = [0u8; 32];
        p[0] = 1;
        p
    }] {
        assert_eq!(
            seal_to_x25519(&room_key, &low).unwrap_err(),
            KeyringError::LowOrderKey
        );
        let envelope = SealedEnvelope {
            ephemeral_public_key: low,
            nonce: [0; 12],
            ciphertext: vec![0; 48],
        };
        assert_eq!(
            open_from_x25519(&envelope, &x25519_secret_of_ed25519(&[1u8; 32])).unwrap_err(),
            KeyringError::LowOrderKey
        );
    }
}

#[test]
fn only_a_full_order_ed25519_key_maps_to_x25519() {
    // The identity point (small order), and a y with no x on the curve.
    let identity = {
        let mut p = [0u8; 32];
        p[0] = 1;
        p
    };
    assert_eq!(
        x25519_public_of_ed25519(&identity).unwrap_err(),
        KeyringError::InvalidEd25519Key
    );
    let not_a_point = bytes32("0200000000000000000000000000000000000000000000000000000000000000");
    assert_eq!(
        x25519_public_of_ed25519(&not_a_point).unwrap_err(),
        KeyringError::InvalidEd25519Key
    );
}

#[test]
fn the_envelope_json_is_the_languages_sealed_box() {
    let v = &vectors()[1];
    let sealed: SealedEnvelope = serde_json::from_value(v.sealed.clone()).unwrap();
    assert_eq!(serde_json::to_value(&sealed).unwrap(), v.sealed);
    // The language's hexToBytes takes any case and trims; so does the envelope.
    let loud = serde_json::json!({
        "ephemeralPublicKey": format!(" {} ", v.sealed["ephemeralPublicKey"].as_str().unwrap().to_uppercase()),
        "nonce": v.sealed["nonce"],
        "ciphertext": v.sealed["ciphertext"].as_str().unwrap().to_uppercase(),
    });
    assert_eq!(
        serde_json::from_value::<SealedEnvelope>(loud).unwrap(),
        sealed
    );
    for bad in [
        serde_json::json!({ "ephemeralPublicKey": "00", "nonce": v.sealed["nonce"], "ciphertext": v.sealed["ciphertext"] }),
        serde_json::json!({ "ephemeralPublicKey": v.sealed["ephemeralPublicKey"], "nonce": "0303", "ciphertext": v.sealed["ciphertext"] }),
        serde_json::json!({ "ephemeralPublicKey": v.sealed["ephemeralPublicKey"], "nonce": v.sealed["nonce"], "ciphertext": "xyz" }),
        serde_json::json!({ "ephemeral_public_key": v.sealed["ephemeralPublicKey"], "nonce": v.sealed["nonce"], "ciphertext": v.sealed["ciphertext"] }),
    ] {
        assert!(
            serde_json::from_value::<SealedEnvelope>(bad.clone()).is_err(),
            "{bad}"
        );
    }
}
