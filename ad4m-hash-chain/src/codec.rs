//! Byte encodings fixed by `did:scid:coasys:1` (section 2 of
//! `docs-src/identity/did-scid-coasys-1.md`): the SHA-256 multihash in
//! base58btc without a multibase prefix, the Ed25519 multikey and the proof value.

use sha2::{Digest, Sha256};

/// A base58btc SHA-256 multihash with no multibase prefix: `"Qm…"`.
pub type Hash = String;
/// A public key as a multikey: `z` + base58btc(multicodec prefix + key bytes).
pub type Multikey = String;

/// The multihash code of SHA-256, the only code the module accepts.
pub const SHA2_256: u8 = 0x12;
/// The multicodec prefix of an Ed25519 public key (varint of 0xed).
pub const ED25519_PUB: [u8; 2] = [0xed, 0x01];

/// Why a hash string is not a SHA-256 multihash.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum HashError {
    #[error("not base58btc")]
    Base58,
    #[error("multihash code {0:#x} is not sha2-256 (0x12)")]
    Code(u8),
    #[error("a sha2-256 multihash is 34 bytes with digest length 32")]
    Length,
}

/// SHA-256 of `data`.
pub fn sha256(data: &[u8]) -> [u8; 32] {
    Sha256::digest(data).into()
}

/// The multihash text of a SHA-256 digest.
pub fn encode_hash(digest: &[u8; 32]) -> Hash {
    let mut bytes = Vec::with_capacity(34);
    bytes.push(SHA2_256);
    bytes.push(32);
    bytes.extend_from_slice(digest);
    bs58::encode(bytes).into_string()
}

/// The multihash text of SHA-256 over `data`.
pub fn hash_bytes(data: &[u8]) -> Hash {
    encode_hash(&sha256(data))
}

/// The digest inside a multihash text. Only sha2-256 (code 0x12) is accepted.
pub fn decode_hash(hash: &str) -> Result<[u8; 32], HashError> {
    let bytes = bs58::decode(hash)
        .into_vec()
        .map_err(|_| HashError::Base58)?;
    match bytes.first() {
        Some(&SHA2_256) => {}
        Some(&code) => return Err(HashError::Code(code)),
        None => return Err(HashError::Length),
    }
    if bytes.len() != 34 || bytes[1] != 32 {
        return Err(HashError::Length);
    }
    let mut digest = [0u8; 32];
    digest.copy_from_slice(&bytes[2..]);
    Ok(digest)
}

/// The multikey of an Ed25519 public key: `z` + base58btc(0xed 0x01 + 32 bytes), `"z6Mk…"`.
pub fn encode_ed25519_multikey(public_key: &[u8; 32]) -> Multikey {
    let mut bytes = Vec::with_capacity(34);
    bytes.extend_from_slice(&ED25519_PUB);
    bytes.extend_from_slice(public_key);
    format!("z{}", bs58::encode(bytes).into_string())
}

/// The Ed25519 public key inside a multikey. Any other key type is refused.
pub fn decode_ed25519_multikey(multikey: &str) -> Option<[u8; 32]> {
    let body = multikey.strip_prefix('z')?;
    let bytes = bs58::decode(body).into_vec().ok()?;
    if bytes.len() != 34 || bytes[..2] != ED25519_PUB {
        return None;
    }
    let mut key = [0u8; 32];
    key.copy_from_slice(&bytes[2..]);
    Some(key)
}

/// A proof value: `z` + base58btc(64-byte Ed25519 signature).
pub fn encode_proof_value(signature: &[u8; 64]) -> String {
    format!("z{}", bs58::encode(signature).into_string())
}

/// The signature inside a proof value.
pub fn decode_proof_value(value: &str) -> Option<[u8; 64]> {
    let body = value.strip_prefix('z')?;
    let bytes = bs58::decode(body).into_vec().ok()?;
    bytes.try_into().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_round_trips_and_starts_with_qm() {
        let h = hash_bytes(b"abc");
        assert!(h.starts_with("Qm"));
        assert_eq!(decode_hash(&h).unwrap(), sha256(b"abc"));
    }

    #[test]
    fn hash_refuses_other_codes_and_lengths() {
        let mut blake = vec![0x1e, 32];
        blake.extend_from_slice(&[7u8; 32]);
        assert_eq!(
            decode_hash(&bs58::encode(blake).into_string()),
            Err(HashError::Code(0x1e))
        );
        let short = bs58::encode([0x12, 32, 1, 2]).into_string();
        assert_eq!(decode_hash(&short), Err(HashError::Length));
        assert_eq!(decode_hash("Qm0OIl"), Err(HashError::Base58));
        assert_eq!(decode_hash(""), Err(HashError::Length));
    }

    #[test]
    fn multikey_round_trips_and_refuses_x25519() {
        let key = [9u8; 32];
        let mk = encode_ed25519_multikey(&key);
        assert!(mk.starts_with("z6Mk"));
        assert_eq!(decode_ed25519_multikey(&mk), Some(key));
        let mut x = vec![0xec, 0x01];
        x.extend_from_slice(&key);
        let x25519 = format!("z{}", bs58::encode(x).into_string());
        assert!(x25519.starts_with("z6LS"));
        assert_eq!(decode_ed25519_multikey(&x25519), None);
        assert_eq!(decode_ed25519_multikey(&mk[1..]), None);
    }

    #[test]
    fn proof_value_round_trips() {
        let sig = [3u8; 64];
        let v = encode_proof_value(&sig);
        assert_eq!(decode_proof_value(&v), Some(sig));
        assert_eq!(decode_proof_value(&v[1..]), None);
        assert_eq!(decode_proof_value("z2"), None);
    }
}
