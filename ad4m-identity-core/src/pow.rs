//! The brake's proof of work for a new log with no vouching note (AR-01 4.7): a 16-byte
//! `nonce` such that `sha256("coasys:pow" ‖ scid ‖ nonce)` has at least `bits` leading zero
//! bits. The proof binds to the SCID, which covers master #0, the next-key commitment and every
//! listed device key, so one proof admits one log. The store keeps no state for a proof.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// The hash prefix (a label, not a signing domain: AR-01 4.2).
pub const POW_LABEL: &str = "coasys:pow";
/// The nonce length in bytes.
pub const NONCE_LEN: usize = 16;

/// A proof of work, as `AppendRequest.pow` carries it. `nonce` is base58btc (no multibase
/// prefix) of 16 bytes; `bits` is the cost the solver aimed at. The checker states its own
/// `bits` and does not trust this one.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Pow {
    pub nonce: String,
    pub bits: u8,
}

/// `sha256("coasys:pow" ‖ scid ‖ nonce)`; `scid` enters as its UTF-8 text.
pub fn pow_hash(scid: &str, nonce: &[u8; NONCE_LEN]) -> [u8; 32] {
    let mut hasher = prefix(scid);
    hasher.update(nonce);
    hasher.finalize().into()
}

/// The number of leading zero bits of `hash`.
pub fn leading_zero_bits(hash: &[u8; 32]) -> u32 {
    let mut bits = 0;
    for byte in hash {
        if *byte == 0 {
            bits += 8;
        } else {
            return bits + byte.leading_zeros();
        }
    }
    bits
}

fn prefix(scid: &str) -> Sha256 {
    let mut hasher = Sha256::new();
    hasher.update(POW_LABEL.as_bytes());
    hasher.update(scid.as_bytes());
    hasher
}

/// The nonce of counter `n`: its 16 big-endian bytes.
fn nonce_of(n: u128) -> [u8; NONCE_LEN] {
    n.to_be_bytes()
}

/// Tries the nonces `start`, `start + 1`, … for at most `tries` of them and gives the first
/// that meets `bits`. A caller that shows progress calls it in rounds.
pub fn solve_range(scid: &str, bits: u8, start: u128, tries: u64) -> Option<Pow> {
    let base = prefix(scid);
    let mut n = start;
    for _ in 0..tries {
        let nonce = nonce_of(n);
        let mut hasher = base.clone();
        hasher.update(nonce);
        let hash: [u8; 32] = hasher.finalize().into();
        if leading_zero_bits(&hash) >= u32::from(bits) {
            return Some(Pow {
                nonce: bs58::encode(nonce).into_string(),
                bits,
            });
        }
        n = n.wrapping_add(1);
    }
    None
}

/// Finds a proof for `scid` at `bits`. Deterministic: the first nonce counted from 0.
/// About 2^bits hashes on average; the store's default is 22 bits.
pub fn solve(scid: &str, bits: u8) -> Pow {
    let mut start = 0u128;
    loop {
        if let Some(pow) = solve_range(scid, bits, start, 1 << 20) {
            return pow;
        }
        start = start.wrapping_add(1 << 20);
    }
}

/// True when `pow.nonce` is 16 bytes of base58btc and `sha256("coasys:pow" ‖ scid ‖ nonce)`
/// has at least `bits` leading zero bits. `pow.bits` is not read.
pub fn check(scid: &str, pow: &Pow, bits: u8) -> bool {
    let Ok(bytes) = bs58::decode(&pow.nonce).into_vec() else {
        return false;
    };
    let Ok(nonce) = <[u8; NONCE_LEN]>::try_from(bytes) else {
        return false;
    };
    leading_zero_bits(&pow_hash(scid, &nonce)) >= u32::from(bits)
}
