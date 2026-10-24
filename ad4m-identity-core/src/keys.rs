//! Key encodings of format 1 (section 2 of `docs-src/identity/did-scid-coasys-1.md`): Ed25519
//! signing multikeys (`z6Mk…`) come from the hash-chain module; X25519 encryption multikeys
//! (`z6LS…`) live here. Every other key type, the reserved P-256 passkey key included, is
//! refused.

use ad4m_hash_chain::{decode_ed25519_multikey, Multikey};

/// The multicodec prefix of an X25519 public key (varint of 0xec).
pub const X25519_PUB: [u8; 2] = [0xec, 0x01];

/// True when `key` is an Ed25519 signing multikey.
pub fn is_ed25519_multikey(key: &str) -> bool {
    decode_ed25519_multikey(key).is_some()
}

/// The multikey of an X25519 public key: `z` + base58btc(0xec 0x01 + 32 bytes), `"z6LS…"`.
pub fn encode_x25519_multikey(public_key: &[u8; 32]) -> Multikey {
    let mut bytes = Vec::with_capacity(34);
    bytes.extend_from_slice(&X25519_PUB);
    bytes.extend_from_slice(public_key);
    format!("z{}", bs58::encode(bytes).into_string())
}

/// The X25519 public key inside a multikey. Any other key type is refused.
pub fn decode_x25519_multikey(multikey: &str) -> Option<[u8; 32]> {
    let body = multikey.strip_prefix('z')?;
    let bytes = bs58::decode(body).into_vec().ok()?;
    if bytes.len() != 34 || bytes[..2] != X25519_PUB {
        return None;
    }
    let mut key = [0u8; 32];
    key.copy_from_slice(&bytes[2..]);
    Some(key)
}

/// True when `key` is an X25519 encryption multikey.
pub fn is_x25519_multikey(key: &str) -> bool {
    decode_x25519_multikey(key).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad4m_hash_chain::encode_ed25519_multikey;

    #[test]
    fn x25519_round_trips_and_refuses_other_types() {
        let key = [5u8; 32];
        let enc = encode_x25519_multikey(&key);
        assert!(enc.starts_with("z6LS"));
        assert_eq!(decode_x25519_multikey(&enc), Some(key));
        assert!(!is_ed25519_multikey(&enc));
        let sign = encode_ed25519_multikey(&key);
        assert!(!is_x25519_multikey(&sign));
        // The reserved P-256 passkey key: 0x80 0x24 + 33 bytes.
        let mut p256 = vec![0x80, 0x24];
        p256.extend_from_slice(&[2u8; 33]);
        let passkey = format!("z{}", bs58::encode(p256).into_string());
        assert!(passkey.starts_with("zDn"));
        assert!(!is_ed25519_multikey(&passkey) && !is_x25519_multikey(&passkey));
    }
}
