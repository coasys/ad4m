//! The proof of work bound to the SCID (section 9 of the format spec).

use ad4m_identity_core::pow::{self, Pow};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const SCID: &str = "QmZkP3wW4ytZVGAvdUSYQuKgnuqmC1xdqNx1MhLeD6V8Wq";
const OTHER: &str = "QmPqHxe2MAvkmJ8xD1FvbCJ1m4m4LqWQ2RG8fqZq1w8vNa";

#[test]
fn solve_then_check_passes() {
    for bits in [0u8, 1, 8, 14] {
        let p = pow::solve(SCID, bits);
        assert_eq!(p.bits, bits);
        assert!(pow::check(SCID, &p, bits), "{bits}");
        assert_eq!(bs58::decode(&p.nonce).into_vec().unwrap().len(), 16);
    }
}

#[test]
fn a_proof_for_another_scid_fails() {
    for bits in [12u8, 16] {
        let p = pow::solve(SCID, bits);
        assert!(pow::check(SCID, &p, bits));
        assert!(!pow::check(OTHER, &p, bits), "{bits}");
    }
}

#[test]
fn a_proof_one_bit_short_fails() {
    for bits in [8u8, 12, 16] {
        // The first nonce with exactly bits − 1 leading zero bits.
        let mut start = 0u128;
        let short = loop {
            let p = pow::solve_range(SCID, bits - 1, start, 1).map(|p| (p, start));
            if let Some((p, n)) = p {
                let nonce = n.to_be_bytes();
                if pow::leading_zero_bits(&pow::pow_hash(SCID, &nonce)) == u32::from(bits - 1) {
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
    let p = pow::solve(SCID, 4);
    let short = Pow {
        nonce: bs58::encode([0u8; 15]).into_string(),
        bits: 4,
    };
    assert!(!pow::check(SCID, &short, 0));
    let not_b58 = Pow {
        nonce: "0OIl".into(),
        bits: 4,
    };
    assert!(!pow::check(SCID, &not_b58, 0));
    // The checker states its own bits; the proof's claim is not read.
    let nonce: [u8; 16] = bs58::decode(&p.nonce)
        .into_vec()
        .unwrap()
        .try_into()
        .unwrap();
    let zeros = pow::leading_zero_bits(&pow::pow_hash(SCID, &nonce));
    let claimed = Pow { bits: 200, ..p };
    assert!(pow::check(SCID, &claimed, 4));
    assert!(!pow::check(SCID, &claimed, (zeros + 1) as u8));
}

#[test]
fn the_hash_is_label_scid_nonce() {
    use sha2::{Digest, Sha256};
    let nonce = [7u8; 16];
    let mut h = Sha256::new();
    h.update(b"coasys:pow");
    h.update(SCID.as_bytes());
    h.update(nonce);
    let want: [u8; 32] = h.finalize().into();
    assert_eq!(pow::pow_hash(SCID, &nonce), want);
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
    };
    assert_eq!(
        serde_json::to_value(&p).unwrap(),
        serde_json::json!({"nonce": "11111111111111111", "bits": 22})
    );
}
