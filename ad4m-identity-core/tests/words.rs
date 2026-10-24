//! The 12 words and master-key derivation (AR-01 4.4, R14).
#![cfg(all(feature = "words", feature = "build"))]

mod common;

use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::words::{self, WordsError};
use ad4m_identity_core::{fold, KelPolicy, KeyLife, Op};
use common::*;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

fn unhex(text: &str) -> Vec<u8> {
    (0..text.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&text[i..i + 2], 16).unwrap())
        .collect()
}

#[test]
fn generate_gives_twelve_list_words() {
    let a = words::generate();
    let b = words::generate();
    assert_eq!(a.words().len(), 12);
    assert_ne!(a.phrase(), b.phrase());
    assert_eq!(words::parse(a.phrase()).unwrap().phrase(), a.phrase());
    assert_eq!(format!("{a:?}"), "Words(12 words)");
}

#[test]
fn parse_takes_any_spacing_and_case() {
    let w = words::from_entropy(&[0u8; 16]);
    assert_eq!(
        w.phrase(),
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
    );
    let messy = "  Abandon abandon\nABANDON abandon\tabandon abandon\r\nabandon abandon abandon abandon abandon   About \n";
    assert_eq!(words::parse(messy).unwrap().phrase(), w.phrase());
}

#[test]
fn parse_names_the_wrong_word() {
    assert_eq!(
        words::parse(
            "orbit meadow lamp velvet harbor timber copper saddle gardan pepper ribbon quantum"
        )
        .unwrap_err(),
        WordsError::UnknownWord {
            position: 9,
            word: "gardan".into(),
            suggestion: Some("garden".into())
        }
    );
    // A short typo gets the nearest word by edit distance.
    match words::parse("abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abuot")
        .unwrap_err()
    {
        WordsError::UnknownWord {
            position, suggestion, ..
        } => {
            assert_eq!(position, 12);
            assert!(suggestion.is_some());
        }
        other => panic!("{other:?}"),
    }
    match words::parse("zzzzzz abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about").unwrap_err() {
        WordsError::UnknownWord { position: 1, suggestion: None, .. } => {}
        other => panic!("{other:?}"),
    }
    assert_eq!(
        words::parse("abandon abandon").unwrap_err(),
        WordsError::Count(2)
    );
    // Twelve list words with a wrong checksum.
    assert_eq!(
        words::parse("abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon")
            .unwrap_err(),
        WordsError::Checksum
    );
}

#[test]
fn bip39_reference_vector() {
    // BIP-39 English, entropy 0x00 × 16, empty passphrase.
    let seed = words::seed(&words::from_entropy(&[0u8; 16]));
    assert_eq!(
        hex(seed.as_bytes()),
        "5eb00bbddcf069084889a8ab9155568165f5c453ccb85e70811aaed6f6da5fc19a5ac40b389cd370d086206dec8aa6c43daea6690f20ad3d8d48b2d2ce9e38e4"
    );
    // Entropy 0x7f × 16 gives the BIP-39 reference words.
    assert_eq!(
        words::from_entropy(&[0x7f; 16]).phrase(),
        "legal winner thank year wave sausage worth useful legal winner thank yellow"
    );
}

#[test]
fn slip10_ed25519_reference_vectors() {
    // SLIP-0010, test vector 1 for ed25519.
    let seed = unhex("000102030405060708090a0b0c0d0e0f");
    let cases: &[(&[u32], &str, &str)] = &[
        (
            &[],
            "2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7",
            "90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb",
        ),
        (
            &[0],
            "68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3",
            "8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69",
        ),
        (
            &[0, 1],
            "b1d0bad404bf35da785a64ca1ac54b2617211d2777696fbffaf208f746ae84f2",
            "a320425f77d1b5c2505a6b1b27382b37368ee640e3557c315416801243552f14",
        ),
        (
            &[0, 1, 2],
            "92a5b23c0b8a99e37d07df3fb9966917f5d06e02ddbd909c7e184371463e9fc9",
            "2e69929e00b5ab250f49c3fb1c12f252de4fed2c1db88387094a0f8c4c9ccd6c",
        ),
    ];
    for (path, key, chain) in cases {
        let (k, c) = words::derive_ed25519(&seed, path).unwrap();
        assert_eq!(hex(&k[..]), *key, "{path:?}");
        assert_eq!(hex(&c[..]), *chain, "{path:?}");
    }
    // The public key of m/0H/1H (SLIP-0010 prints it with a 0x00 prefix).
    let (k, _) = words::derive_ed25519(&seed, &[0, 1]).unwrap();
    let public = ed25519_dalek::SigningKey::from_bytes(&k).verifying_key();
    assert_eq!(
        hex(public.as_bytes()),
        "1932a5270f335bed617d5b935c80aedb1a35bd9fc1e31acafd5372c30f5c1187"
    );
    // Test vector 2, m/0H.
    let seed2 = unhex("fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542");
    let (k, c) = words::derive_ed25519(&seed2, &[0]).unwrap();
    assert_eq!(
        hex(&k[..]),
        "1559eb2bbec5790b0c65d8693e4d0875b1747f4970ae8b650486ed7470845635"
    );
    assert_eq!(
        hex(&c[..]),
        "0b78a3226f915c082bf118f83618a618ab6dec793752624cbeb622acb562862d"
    );
    assert_eq!(
        words::derive_ed25519(&seed, &[0x8000_0000]).unwrap_err(),
        WordsError::Index(0x8000_0000)
    );
}

#[test]
fn master_key_uses_the_coasys_path() {
    let seed = words::seed(&words::from_entropy(&WORDS_ENTROPY));
    for n in 0..3u32 {
        let (k, _) = words::derive_ed25519(seed.as_bytes(), &[7133, 0, n]).unwrap();
        assert_eq!(words::master_key(&seed, n).unwrap().to_bytes(), *k);
    }
    assert_ne!(
        words::master_key(&seed, 0).unwrap().to_bytes(),
        words::master_key(&seed, 1).unwrap().to_bytes()
    );
    assert!(words::master_key(&seed, 1 << 31).is_err());
}

#[test]
fn salts_are_hkdf_and_differ_per_n() {
    use hkdf::Hkdf;
    use sha2::Sha256;
    let seed = words::seed(&words::from_entropy(&WORDS_ENTROPY));
    let mut want = [0u8; 16];
    Hkdf::<Sha256>::new(Some(b"coasys/1"), seed.as_bytes())
        .expand(b"next-salt/12", &mut want)
        .unwrap();
    assert_eq!(words::salt(&seed, 12), want);
    assert_ne!(words::salt(&seed, 1), words::salt(&seed, 2));
    let (commitment, salt) = words::next_commitment(&seed, 1).unwrap();
    assert_eq!(salt, bs58::encode(words::salt(&seed, 1)).into_string());
    let key = words::master_key(&seed, 1).unwrap();
    assert_eq!(
        commitment,
        ad4m_identity_core::commitment(&Signer::key(&key), &salt)
    );
}

/// The `next` of every event of a log, in order.
fn nexts(events: &[ad4m_identity_core::Event]) -> Vec<Option<String>> {
    events
        .iter()
        .filter_map(|e| match &e.op {
            Op::Incept { next, .. } => Some(Some(next.clone())),
            Op::Rotate { next, .. } => Some(next.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn restore_reproduces_every_commitment_across_three_rotations() {
    let original = words::generate();
    let seed = words::seed(&original);
    let m0 = words::master_key(&seed, 0).unwrap();
    let mut event = build::incept_human(
        &m0,
        words::next_commitment(&seed, 1).unwrap().0,
        vec![device(1, KeyLife::Standing)],
    )
    .unwrap();
    sign(&mut event, "d1", &dev(1));
    let mut events = vec![event];
    let mut chain = fold(&events).unwrap();
    for n in 1..=3u32 {
        let revealed = words::master_key(&seed, n).unwrap();
        let (_, salt) = words::next_commitment(&seed, n).unwrap();
        let next = words::next_commitment(&seed, n + 1).unwrap().0;
        let rotate = build::rotate(&chain, &revealed, &salt, Some(next), &[]).unwrap();
        chain.append(&KelPolicy, &rotate).unwrap();
        events.push(rotate);
    }
    assert_eq!(chain.state().current_master().unwrap().id, "m3");

    // Restore from the words alone, typed as a person might.
    let typed = original.words().join("\n").to_uppercase();
    let restored = words::seed(&words::parse(&typed).unwrap());
    let want: Vec<Option<String>> = (1..=4u32)
        .map(|n| Some(words::next_commitment(&restored, n).unwrap().0))
        .collect();
    assert_eq!(nexts(&events), want);
    for (n, master) in chain.state().masters.iter().enumerate() {
        assert_eq!(
            master.sign,
            Signer::key(&words::master_key(&restored, n as u32).unwrap())
        );
    }
    // The restored words make the next rotation, which the log accepts.
    let (_, salt) = words::next_commitment(&restored, 4).unwrap();
    let rotate = build::rotate(
        &chain,
        &words::master_key(&restored, 4).unwrap(),
        &salt,
        None,
        &["d1".to_string()],
    )
    .unwrap();
    chain.append(&KelPolicy, &rotate).unwrap();
    assert!(chain.state().ended);

    // Other words reproduce none of them.
    let other = words::seed(&words::generate());
    assert_ne!(
        words::next_commitment(&other, 1).unwrap().0,
        want[0].clone().unwrap()
    );
}

#[test]
fn recovery_secret_is_stable_per_words() {
    let w = words::from_entropy(&WORDS_ENTROPY);
    let a = x25519_dalek::PublicKey::from(&words::recovery_secret(&words::seed(&w)));
    let again = words::parse(w.phrase()).unwrap();
    let b = x25519_dalek::PublicKey::from(&words::recovery_secret(&words::seed(&again)));
    assert_eq!(a, b);
    let other = words::from_entropy(&[1u8; 16]);
    let c = x25519_dalek::PublicKey::from(&words::recovery_secret(&words::seed(&other)));
    assert_ne!(a, c);
}
