//! The proof of work bound to the SCID and a host challenge (section 14 of the format spec).

use ad4m_identity_core::pow::{self, Pow};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const SCID: &str = "QmZkP3wW4ytZVGAvdUSYQuKgnuqmC1xdqNx1MhLeD6V8Wq";
const OTHER: &str = "QmPqHxe2MAvkmJ8xD1FvbCJ1m4m4LqWQ2RG8fqZq1w8vNa";
/// A host challenge the proofs below commit to, as `GET /v1/pow`'s `challenge.nonce` carries it.
const CHALLENGE: &str = "4dCHa1LeNgEfRoMtEhOsT";

#[test]
fn solve_then_check_passes() {
    for bits in [0u8, 1, 8, 14] {
        let p = pow::solve(SCID, CHALLENGE, bits);
        assert_eq!(p.bits, bits);
        assert_eq!(p.challenge, CHALLENGE);
        assert!(pow::check(SCID, &p, bits), "{bits}");
        assert_eq!(bs58::decode(&p.nonce).into_vec().unwrap().len(), 16);
    }
}

#[test]
fn a_proof_for_another_scid_fails() {
    for bits in [12u8, 16] {
        let p = pow::solve(SCID, CHALLENGE, bits);
        assert!(pow::check(SCID, &p, bits));
        assert!(!pow::check(OTHER, &p, bits), "{bits}");
    }
}

#[test]
fn a_proof_for_another_challenge_fails() {
    // The proof commits to the challenge it names: a proof made for one challenge does not
    // pass for another (the case the store's one-use check cannot reach).
    for bits in [12u8, 16] {
        let p = pow::solve(SCID, CHALLENGE, bits);
        assert!(pow::check(SCID, &p, bits));
        let other_challenge = Pow {
            challenge: "4aNoThErChAlLeNgE".into(),
            ..p.clone()
        };
        assert!(!pow::check(SCID, &other_challenge, bits), "{bits}");
        let made_ahead = Pow {
            challenge: String::new(),
            ..p
        };
        assert!(!pow::check(SCID, &made_ahead, bits), "{bits}");
    }
}

#[test]
fn a_proof_one_bit_short_fails() {
    for bits in [8u8, 12, 16] {
        // The first nonce with exactly bits − 1 leading zero bits.
        let mut start = 0u128;
        let short = loop {
            let p = pow::solve_range(SCID, CHALLENGE, bits - 1, start, 1).map(|p| (p, start));
            if let Some((p, n)) = p {
                let nonce = n.to_be_bytes();
                if pow::leading_zero_bits(&pow::pow_hash(SCID, CHALLENGE, &nonce))
                    == u32::from(bits - 1)
                {
                    break p;
                }
            }
            start += 1;
        };
        assert!(pow::check(SCID, &short, bits - 1));
        assert!(!pow::check(SCID, &short, bits), "{bits}");
    }
}

#[test]
fn malformed_nonces_fail() {
    let p = pow::solve(SCID, CHALLENGE, 4);
    let short = Pow {
        nonce: bs58::encode([0u8; 15]).into_string(),
        bits: 4,
        challenge: CHALLENGE.into(),
    };
    assert!(!pow::check(SCID, &short, 0));
    let not_b58 = Pow {
        nonce: "0OIl".into(),
        bits: 4,
        challenge: CHALLENGE.into(),
    };
    assert!(!pow::check(SCID, &not_b58, 0));
    // The checker states its own bits; the proof's claim is not read.
    let nonce: [u8; 16] = bs58::decode(&p.nonce)
        .into_vec()
        .unwrap()
        .try_into()
        .unwrap();
    let zeros = pow::leading_zero_bits(&pow::pow_hash(SCID, CHALLENGE, &nonce));
    let claimed = Pow { bits: 200, ..p };
    assert!(pow::check(SCID, &claimed, 4));
    assert!(!pow::check(SCID, &claimed, (zeros + 1) as u8));
}

#[test]
fn the_longest_nonce_checks_and_an_overlong_one_is_refused_before_decoding() {
    let longest = Pow {
        nonce: bs58::encode([0xff; 16]).into_string(),
        bits: 0,
        challenge: CHALLENGE.into(),
    };
    assert_eq!(longest.nonce.len(), 22);
    assert!(pow::check(SCID, &longest, 0));

    let huge = Pow {
        nonce: "2".repeat(1 << 20),
        bits: 0,
        challenge: CHALLENGE.into(),
    };
    // wasm32 has no clock: there the call only has to return.
    #[cfg(not(target_arch = "wasm32"))]
    let started = std::time::Instant::now();
    assert!(!pow::check(SCID, &huge, 0));
    #[cfg(not(target_arch = "wasm32"))]
    assert!(started.elapsed() < std::time::Duration::from_secs(1));
}

#[test]
fn the_hash_is_label_scid_challenge_nonce() {
    use sha2::{Digest, Sha256};
    let nonce = [7u8; 16];
    let mut h = Sha256::new();
    h.update(b"coasys:pow");
    h.update(SCID.as_bytes());
    h.update(CHALLENGE.as_bytes());
    h.update(nonce);
    let want: [u8; 32] = h.finalize().into();
    assert_eq!(pow::pow_hash(SCID, CHALLENGE, &nonce), want);
    assert_eq!(pow::leading_zero_bits(&[0u8; 32]), 256);
    let mut one = [0u8; 32];
    one[1] = 0b0001_0000;
    assert_eq!(pow::leading_zero_bits(&one), 11);
}

#[test]
fn pow_serialises_as_the_wire_type() {
    let p = Pow {
        nonce: "11111111111111111".into(),
        bits: 22,
        challenge: CHALLENGE.into(),
    };
    assert_eq!(
        serde_json::to_value(&p).unwrap(),
        serde_json::json!({"nonce": "11111111111111111", "bits": 22, "challenge": CHALLENGE})
    );
    // A `challenge` left out reads as empty, which a host checking challenges refuses.
    let plain: Pow =
        serde_json::from_value(serde_json::json!({"nonce": "11111111111111111", "bits": 22}))
            .unwrap();
    assert_eq!(plain.challenge, "");
}
