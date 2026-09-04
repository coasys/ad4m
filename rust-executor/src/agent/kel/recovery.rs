//! Recovery authority derivation for `did:scid` agent identity.
//!
//! The mnemonic acts as the **recovery authority** — not a day-to-day signing key.
//! BIP-39 → master seed → SLIP-0010 ed25519 → recovery keypair.
//! Inception commits only `hash(RecoveryAuthority descriptor)`, so guardian
//! membership stays private until recovery runs.

use did_key::{DIDCore, Ed25519KeyPair, PatchedKeyPair};
use hmac::{Hmac, Mac};
use sha2::{Digest, Sha256, Sha512};
use zeroize::Zeroize;

use super::RecoveryAuthority;

type HmacSha512 = Hmac<Sha512>;

// ─── master seed ─────────────────────────────────────────────────────────────

/// A BIP-39-derived master seed. Zeroized on drop.
pub struct MasterSeed([u8; 64]);

impl Drop for MasterSeed {
    fn drop(&mut self) {
        self.0.zeroize();
    }
}

impl MasterSeed {
    /// Derive the master seed from a BIP-39 mnemonic (empty passphrase).
    pub fn from_mnemonic(phrase: &str) -> Result<Self, super::KelError> {
        let m = bip39::Mnemonic::parse_normalized(phrase)
            .map_err(|e| super::KelError::InvalidInception(format!("bad mnemonic: {}", e)))?;
        Ok(MasterSeed(m.to_seed("")))
    }

    /// Generate a fresh 24-word mnemonic and its master seed.
    pub fn generate() -> Result<(String, Self), super::KelError> {
        let mut entropy = [0u8; 32];
        rand::RngCore::fill_bytes(&mut rand::thread_rng(), &mut entropy);
        let m = bip39::Mnemonic::from_entropy(&entropy)
            .map_err(|e| super::KelError::InvalidInception(format!("entropy error: {}", e)))?;
        let seed = m.to_seed("");
        entropy.zeroize();
        Ok((m.to_string(), MasterSeed(seed)))
    }
}

// ─── SLIP-0010 ed25519 HD derivation ─────────────────────────────────────────

/// SLIP-0010 ed25519 derivation. ed25519 supports hardened derivation only, so
/// every path element gets forced hardened. Returns the 32-byte private key.
fn slip10_ed25519(seed: &[u8], path: &[u32]) -> [u8; 32] {
    let mut mac = HmacSha512::new_from_slice(b"ed25519 seed").expect("hmac accepts any key length");
    mac.update(seed);
    let i = mac.finalize().into_bytes();
    let mut key = [0u8; 32];
    let mut chain = [0u8; 32];
    key.copy_from_slice(&i[0..32]);
    chain.copy_from_slice(&i[32..64]);
    for &index in path {
        let hardened = index | 0x8000_0000;
        let mut mac = HmacSha512::new_from_slice(&chain).expect("hmac");
        mac.update(&[0u8]); // 0x00 || key || ser32(index')
        mac.update(&key);
        mac.update(&hardened.to_be_bytes());
        let i = mac.finalize().into_bytes();
        key.copy_from_slice(&i[0..32]);
        chain.copy_from_slice(&i[32..64]);
    }
    chain.zeroize();
    key
}

/// Recovery authority branch `m/44'/0'/1'` — separate from signing keys.
const RECOVERY_BRANCH: [u32; 3] = [44, 0, 1];

/// Derive the recovery authority keypair from the mnemonic seed.
/// Uses path `m/44'/0'/1'/0'` (hardened, single recovery key).
pub fn recovery_keypair(seed: &MasterSeed) -> PatchedKeyPair {
    let mut path = RECOVERY_BRANCH.to_vec();
    path.push(0); // single recovery key at index 0
    let mut sk = slip10_ed25519(&seed.0, &path);
    let kp = did_key::generate::<Ed25519KeyPair>(Some(&sk));
    sk.zeroize();
    kp
}

/// The `did:key` string for a keypair's public key.
pub fn did_key_of(kp: &PatchedKeyPair) -> String {
    kp.get_did_document(did_key::Config::default()).id
}

/// Build a mnemonic-only recovery authority: threshold 1, single derived key.
pub fn mnemonic_recovery_authority(seed: &MasterSeed) -> RecoveryAuthority {
    let kp = recovery_keypair(seed);
    RecoveryAuthority {
        threshold: 1,
        keys: vec![did_key_of(&kp)],
    }
}

/// Compute the commitment hash for a recovery authority descriptor.
/// `sha256(jcs(descriptor))` as hex — committed in inception, revealed at use.
pub fn recovery_commitment(authority: &RecoveryAuthority) -> String {
    let canonical =
        serde_jcs::to_vec(authority).expect("RecoveryAuthority serializes without error");
    hex::encode(Sha256::digest(&canonical))
}

#[cfg(test)]
mod tests {
    use super::*;

    const MNEMONIC: &str =
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";

    #[test]
    fn mnemonic_seed_deterministic() {
        let a = MasterSeed::from_mnemonic(MNEMONIC).unwrap();
        let b = MasterSeed::from_mnemonic(MNEMONIC).unwrap();
        assert_eq!(a.0, b.0);
    }

    #[test]
    fn recovery_key_deterministic() {
        let seed = MasterSeed::from_mnemonic(MNEMONIC).unwrap();
        let a = did_key_of(&recovery_keypair(&seed));
        let b = did_key_of(&recovery_keypair(&seed));
        assert_eq!(a, b);
    }

    #[test]
    fn mnemonic_recovery_authority_commitment_stable() {
        let seed = MasterSeed::from_mnemonic(MNEMONIC).unwrap();
        let auth = mnemonic_recovery_authority(&seed);
        assert_eq!(auth.threshold, 1);
        assert_eq!(auth.keys.len(), 1);
        let c1 = recovery_commitment(&auth);
        let c2 = recovery_commitment(&auth);
        assert_eq!(c1, c2);
    }

    #[test]
    fn generate_produces_valid_mnemonic() {
        let (phrase, _seed) = MasterSeed::generate().unwrap();
        let words: Vec<&str> = phrase.split_whitespace().collect();
        assert_eq!(words.len(), 24);
        // Re-derive from the phrase — must not error.
        MasterSeed::from_mnemonic(&phrase).unwrap();
    }
}
