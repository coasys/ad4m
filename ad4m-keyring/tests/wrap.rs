//! `wrap` / `unwrap`: a key kept under another key, opening only in its own place.

use std::collections::HashSet;

use ad4m_keyring::{
    random_key, unwrap, wrap, KeyRef, KeyringError, SecretKey, SymWrap, WrapAlg, WrapContext,
    WRAP_NONCE_LEN,
};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const KEYRING: [u8; 16] = [7u8; 16];
const OTHER_KEYRING: [u8; 16] = [8u8; 16];

fn ctx() -> WrapContext<'static> {
    WrapContext {
        keyring_id: &KEYRING,
        wrapped: KeyRef::new("store", "perspective:0b6f", 2),
        wrapper: KeyRef::new("person", "main", 1),
    }
}

fn key(byte: u8) -> SecretKey {
    SecretKey::from_slice(&[byte; 32]).unwrap()
}

#[test]
fn a_wrap_opens_under_its_key_in_its_context() {
    let (dek, kek) = (random_key(), random_key());
    let w = wrap(&dek, &kek, &ctx());
    assert_eq!(w.alg, WrapAlg::XChaCha20Poly1305);
    assert_eq!(w.ct.len(), 32 + 16, "the key and the tag");
    assert_ne!(&w.ct[..32], dek.expose_secret(), "the wrap is not the key");
    let opened = unwrap(&w, &kek, &ctx()).unwrap();
    assert_eq!(opened.expose_secret(), dek.expose_secret());
}

#[test]
fn a_wrap_opens_only_in_the_same_context() {
    let (dek, kek) = (key(1), key(2));
    let base = ctx();
    let w = wrap(&dek, &kek, &base);

    let changed: Vec<(&str, WrapContext)> = vec![
        (
            "keyring",
            WrapContext {
                keyring_id: &OTHER_KEYRING,
                ..base
            },
        ),
        (
            "wrapped kind",
            WrapContext {
                wrapped: KeyRef::new("person", "perspective:0b6f", 2),
                ..base
            },
        ),
        (
            "wrapped id",
            WrapContext {
                wrapped: KeyRef::new("store", "perspective:0b70", 2),
                ..base
            },
        ),
        (
            "wrapped version",
            WrapContext {
                wrapped: KeyRef::new("store", "perspective:0b6f", 1),
                ..base
            },
        ),
        (
            "wrapper kind",
            WrapContext {
                wrapper: KeyRef::new("root", "main", 1),
                ..base
            },
        ),
        (
            "wrapper id",
            WrapContext {
                wrapper: KeyRef::new("person", "user:t-1", 1),
                ..base
            },
        ),
        (
            "wrapper version",
            WrapContext {
                wrapper: KeyRef::new("person", "main", 2),
                ..base
            },
        ),
        (
            "wrapped and wrapper swapped",
            WrapContext {
                wrapped: base.wrapper,
                wrapper: base.wrapped,
                ..base
            },
        ),
    ];
    for (field, other) in changed {
        assert_eq!(
            unwrap(&w, &kek, &other).unwrap_err(),
            KeyringError::Unwrap,
            "a wrap opened with another {field}"
        );
    }
    assert!(unwrap(&w, &kek, &base).is_ok());
}

#[test]
fn fields_cannot_shift_between_kind_and_id() {
    // Length prefixes: ("ab", "c") and ("a", "bc") are different contexts.
    let a = WrapContext {
        wrapped: KeyRef::new("ab", "c", 1),
        ..ctx()
    };
    let b = WrapContext {
        wrapped: KeyRef::new("a", "bc", 1),
        ..ctx()
    };
    assert_ne!(a.associated_data(), b.associated_data());
    let (dek, kek) = (key(1), key(2));
    assert_eq!(
        unwrap(&wrap(&dek, &kek, &a), &kek, &b).unwrap_err(),
        KeyringError::Unwrap
    );
}

#[test]
fn a_wrap_does_not_open_under_another_key() {
    let w = wrap(&key(1), &key(2), &ctx());
    assert_eq!(
        unwrap(&w, &key(3), &ctx()).unwrap_err(),
        KeyringError::Unwrap
    );
}

#[test]
fn an_altered_wrap_does_not_open() {
    let kek = key(2);
    let w = wrap(&key(1), &kek, &ctx());
    for i in 0..w.ct.len() {
        let mut altered = w.clone();
        altered.ct[i] ^= 0x01;
        assert_eq!(
            unwrap(&altered, &kek, &ctx()).unwrap_err(),
            KeyringError::Unwrap,
            "byte {i}"
        );
    }
    for i in 0..WRAP_NONCE_LEN {
        let mut altered = w.clone();
        altered.nonce[i] ^= 0x80;
        assert_eq!(
            unwrap(&altered, &kek, &ctx()).unwrap_err(),
            KeyringError::Unwrap,
            "nonce {i}"
        );
    }
    for len in [0, 16, 47, 49] {
        let mut altered = w.clone();
        altered.ct.resize(len, 0);
        assert_eq!(
            unwrap(&altered, &kek, &ctx()).unwrap_err(),
            KeyringError::Unwrap,
            "{len} bytes"
        );
    }
}

#[test]
fn a_thousand_wraps_of_one_key_have_a_thousand_nonces() {
    let (dek, kek) = (key(1), key(2));
    let wraps: Vec<SymWrap> = (0..1000).map(|_| wrap(&dek, &kek, &ctx())).collect();
    let nonces: HashSet<[u8; WRAP_NONCE_LEN]> = wraps.iter().map(|w| w.nonce).collect();
    assert_eq!(nonces.len(), 1000);
    let cts: HashSet<&[u8]> = wraps.iter().map(|w| w.ct.as_slice()).collect();
    assert_eq!(cts.len(), 1000);
    assert!(wraps.iter().all(|w| w.nonce != [0u8; WRAP_NONCE_LEN]));
    for w in wraps.iter().step_by(97) {
        assert_eq!(unwrap(w, &kek, &ctx()).unwrap().expose_secret(), &[1u8; 32]);
    }
}

#[test]
fn the_wrapping_key_is_not_used_raw() {
    // The AEAD key is derived from the wrapping key for wraps alone: a wrap made directly under
    // the raw key with the same nonce and associated data does not open.
    use chacha20poly1305::aead::{Aead, KeyInit, Payload};
    use chacha20poly1305::{XChaCha20Poly1305, XNonce};
    let (dek, kek) = (key(1), key(2));
    let nonce = [9u8; 24];
    let raw = XChaCha20Poly1305::new(kek.expose_secret().into())
        .encrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: dek.expose_secret(),
                aad: &ctx().associated_data(),
            },
        )
        .unwrap();
    let w = SymWrap {
        alg: WrapAlg::XChaCha20Poly1305,
        nonce,
        ct: raw,
    };
    assert_eq!(unwrap(&w, &kek, &ctx()).unwrap_err(), KeyringError::Unwrap);
}

#[test]
fn a_wrap_round_trips_through_json_as_hex() {
    let kek = key(2);
    let w = wrap(&key(1), &kek, &ctx());
    let json = serde_json::to_value(&w).unwrap();
    assert_eq!(json["alg"], "xchacha20poly1305");
    assert_eq!(json["nonce"], hex::encode(w.nonce));
    assert_eq!(json["ct"], hex::encode(&w.ct));
    let back: SymWrap = serde_json::from_value(json).unwrap();
    assert_eq!(back, w);
    assert_eq!(
        unwrap(&back, &kek, &ctx()).unwrap().expose_secret(),
        &[1u8; 32]
    );

    for bad in [
        serde_json::json!({ "alg": "aes-gcm", "nonce": hex::encode(w.nonce), "ct": hex::encode(&w.ct) }),
        serde_json::json!({ "alg": "xchacha20poly1305", "nonce": "00", "ct": hex::encode(&w.ct) }),
        serde_json::json!({ "alg": "xchacha20poly1305", "nonce": hex::encode(w.nonce), "ct": "zz" }),
    ] {
        assert!(
            serde_json::from_value::<SymWrap>(bad.clone()).is_err(),
            "{bad}"
        );
    }
}
