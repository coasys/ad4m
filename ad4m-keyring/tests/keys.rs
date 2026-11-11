//! `SecretKey`, `random_key`, `hkdf_subkey`, and `VersionedKey`'s rules.

use ad4m_keyring::{
    hkdf_subkey, random_key, KeyVersion, KeyringError, SecretKey, VersionedKey, KEY_LEN,
};

#[test]
fn a_key_has_exactly_32_bytes() {
    assert_eq!(KEY_LEN, 32);
    for len in [0, 16, 31, 33, 64] {
        assert_eq!(
            SecretKey::from_slice(&vec![1u8; len]).unwrap_err(),
            KeyringError::KeyLength(len)
        );
    }
    assert_eq!(
        SecretKey::from_slice(&[5u8; 32]).unwrap().expose_secret(),
        &[5u8; 32]
    );
}

#[test]
fn debug_shows_no_key_bytes() {
    let key = SecretKey::from_slice(&[0xab; 32]).unwrap();
    let text = format!("{key:?} {:?}", SecretKey::from_slice(&[0xab; 32]));
    assert!(!text.to_lowercase().contains("ab, "), "{text}");
    assert!(!text.contains("171"), "{text}");
    assert!(text.contains("SecretKey(..)"));
}

#[test]
fn random_keys_differ() {
    let (a, b) = (random_key(), random_key());
    assert_ne!(a.expose_secret(), b.expose_secret());
    assert_ne!(a.expose_secret(), &[0u8; 32]);
}

#[test]
fn hkdf_subkey_is_rfc_5869_hkdf_sha256() {
    // Reference values from an independent HKDF-SHA256 (Python's hmac and hashlib, RFC 5869
    // extract and expand), 32 bytes of output.
    let ikm = SecretKey::from_slice(&[0x0b; 32]).unwrap();
    let salt = hex::decode("000102030405060708090a0b0c").unwrap();
    assert_eq!(
        hex::encode(hkdf_subkey(&ikm, &salt, "ad4m:test:v1").expose_secret()),
        "fd15061380745a2f2b32b4cd40e6748b5c761c24d0922b051e21cedcd56a3542"
    );
    // An empty salt is RFC 5869's "no salt" (32 zero bytes).
    let ikm = SecretKey::from_slice(&[1u8; 32]).unwrap();
    assert_eq!(
        hex::encode(hkdf_subkey(&ikm, &[], "ad4m:at-rest:sqlite:v1").expose_secret()),
        "a050ff78e841fc25bab907c412fbe9f4f4e8cf92a992d1b76089e74b5535e1d8"
    );
    assert_eq!(
        hkdf_subkey(&ikm, &[0u8; 32], "ad4m:at-rest:sqlite:v1").expose_secret(),
        hkdf_subkey(&ikm, &[], "ad4m:at-rest:sqlite:v1").expose_secret()
    );
}

#[test]
fn hkdf_subkeys_are_separated_by_info_and_salt() {
    let key = SecretKey::from_slice(&[3u8; 32]).unwrap();
    let a = hkdf_subkey(&key, b"store-1", "ad4m:at-rest:sqlite:v1");
    let b = hkdf_subkey(&key, b"store-1", "ad4m:at-rest:lang:v1");
    let c = hkdf_subkey(&key, b"store-2", "ad4m:at-rest:sqlite:v1");
    let again = hkdf_subkey(&key, b"store-1", "ad4m:at-rest:sqlite:v1");
    assert_eq!(a.expose_secret(), again.expose_secret(), "deterministic");
    assert_ne!(a.expose_secret(), b.expose_secret());
    assert_ne!(a.expose_secret(), c.expose_secret());
    assert_ne!(a.expose_secret(), key.expose_secret());
}

#[test]
fn versions_start_at_1_and_only_grow() {
    let mut key = VersionedKey::new("a");
    assert_eq!(key.current(), KeyVersion::FIRST);
    assert_eq!(KeyVersion::FIRST, KeyVersion(1));
    assert_eq!(key.next_version().unwrap(), KeyVersion(2));
    assert_eq!(key.push("b").unwrap(), KeyVersion(2));
    assert_eq!(key.push("c").unwrap(), KeyVersion(3));
    assert_eq!(key.current(), KeyVersion(3));
    assert_eq!(*key.current_value(), "c");
    assert_eq!(key.len(), 3);
    assert!(!key.is_empty());
    assert_eq!(
        key.iter().collect::<Vec<_>>(),
        vec![
            (KeyVersion(1), &"a"),
            (KeyVersion(2), &"b"),
            (KeyVersion(3), &"c")
        ]
    );
}

#[test]
fn old_versions_stay_readable_until_retired() {
    let mut key = VersionedKey::new("a");
    key.push("b").unwrap();
    assert_eq!(*key.get(KeyVersion(1)).unwrap(), "a");
    *key.get_mut(KeyVersion(1)).unwrap() = "a, re-wrapped";
    assert_eq!(*key.get(KeyVersion(1)).unwrap(), "a, re-wrapped");
    assert_eq!(key.retire(KeyVersion(1)).unwrap(), "a, re-wrapped");
    assert_eq!(
        key.get(KeyVersion(1)).unwrap_err(),
        KeyringError::UnknownVersion(KeyVersion(1))
    );
    assert_eq!(
        key.retire(KeyVersion(1)).unwrap_err(),
        KeyringError::UnknownVersion(KeyVersion(1))
    );
    assert_eq!(
        key.get(KeyVersion(9)).unwrap_err(),
        KeyringError::UnknownVersion(KeyVersion(9))
    );
}

#[test]
fn the_current_version_cannot_be_retired_and_versions_are_never_reused() {
    let mut key = VersionedKey::new(1);
    assert_eq!(
        key.retire(KeyVersion(1)).unwrap_err(),
        KeyringError::RetireCurrent(KeyVersion(1))
    );
    key.push(2).unwrap();
    key.push(3).unwrap();
    key.retire(KeyVersion(2)).unwrap();
    key.retire(KeyVersion(1)).unwrap();
    assert_eq!(
        key.push(4).unwrap(),
        KeyVersion(4),
        "a retired version is not given again"
    );
}

#[test]
fn versions_end_at_u32_max() {
    let json =
        serde_json::json!({ "current": u32::MAX, "versions": { (u32::MAX.to_string()): 0 } });
    let mut key: VersionedKey<u8> = serde_json::from_value(json).unwrap();
    assert_eq!(
        key.next_version().unwrap_err(),
        KeyringError::VersionsExhausted(KeyVersion(u32::MAX))
    );
    assert_eq!(
        key.push(1).unwrap_err(),
        KeyringError::VersionsExhausted(KeyVersion(u32::MAX))
    );
    assert_eq!(key.len(), 1);
}

#[test]
fn versions_round_trip_through_json_and_bad_ones_are_refused() {
    let mut key = VersionedKey::new("a".to_string());
    key.push("b".into()).unwrap();
    key.push("c".into()).unwrap();
    key.retire(KeyVersion(2)).unwrap();
    let json = serde_json::to_value(&key).unwrap();
    assert_eq!(
        json,
        serde_json::json!({ "current": 3, "versions": { "1": "a", "3": "c" } })
    );
    assert_eq!(
        serde_json::from_value::<VersionedKey<String>>(json).unwrap(),
        key
    );

    for (bad, why) in [
        (
            serde_json::json!({ "current": 1, "versions": {} }),
            "no version",
        ),
        (
            serde_json::json!({ "current": 2, "versions": { "1": "a" } }),
            "current not held",
        ),
        (
            serde_json::json!({ "current": 1, "versions": { "1": "a", "2": "b" } }),
            "current not the highest",
        ),
        (
            serde_json::json!({ "current": 1, "versions": { "0": "z", "1": "a" } }),
            "version 0",
        ),
    ] {
        assert!(
            serde_json::from_value::<VersionedKey<String>>(bad).is_err(),
            "{why}"
        );
    }
}

#[test]
fn version_display() {
    assert_eq!(KeyVersion(4).to_string(), "v4");
    assert_eq!(KeyVersion(1).next(), Some(KeyVersion(2)));
    assert_eq!(KeyVersion(u32::MAX).next(), None);
}
