//! Data sealed to an agent's DID key.
//!
//! A `did:key` holds an Ed25519 public key; its X25519 form (the RFC 7748 birational map,
//! libsodium's `crypto_sign_ed25519_pk_to_curve25519`) is computable from the DID by
//! anyone, and the matching X25519 secret (`crypto_sign_ed25519_sk_to_curve25519`) from
//! the agent's Ed25519 secret by the executor. So anyone can seal to an agent knowing only
//! its DID, and only the executor that holds the agent's key opens it.
//!
//! A box: an ephemeral X25519 keypair, ECDH with the recipient's X25519 key,
//! `HKDF-SHA256(shared, salt = ephemeral public ‖ recipient public, info = SEALED_BOX_INFO)`
//! as an AES-256-GCM key, a random 12-byte nonce. Every field is hex. The server link
//! language seals in the same format with noble (`sealToDid` in `src/encryption.ts`); the
//! executor only opens, and seals only in tests.

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes256Gcm, Nonce};
#[cfg(test)]
use curve25519_dalek::edwards::CompressedEdwardsY;
use curve25519_dalek::montgomery::MontgomeryPoint;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
#[cfg(test)]
use did_key::KeyMaterial;
use hkdf::Hkdf;
#[cfg(test)]
use rand::RngCore;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256, Sha512};
use zeroize::Zeroizing;

use super::AgentContext;
use crate::wallet::wallet_backend;

/// HKDF `info` of every sealed box: changing it makes every sealed box unreadable.
pub const SEALED_BOX_INFO: &[u8] = b"ad4m:sealed-box:v1";

/// `{ ephemeralPublicKey, nonce, ciphertext }`, hex.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SealedBox {
    pub ephemeral_public_key: String,
    pub nonce: String,
    pub ciphertext: String,
}

/// The X25519 public key of an Ed25519 `did:key` (a `#key-id` suffix is ignored).
#[cfg(test)]
fn x25519_public_key_of_did(did: &str) -> Result<[u8; 32], AnyError> {
    let did = did.split('#').next().unwrap_or(did);
    // Multicodec 0xed01 in base58btc: every Ed25519 did:key starts with z6Mk.
    if !did.starts_with("did:key:z6Mk") {
        return Err(anyhow!("{} is not an Ed25519 did:key", did));
    }
    let key = did_key::resolve(did).map_err(|e| anyhow!("Invalid did:key {}: {:?}", did, e))?;
    let ed25519: [u8; 32] = key
        .public_key_bytes()
        .try_into()
        .map_err(|_| anyhow!("{} does not hold a 32-byte Ed25519 key", did))?;
    let point = CompressedEdwardsY(ed25519)
        .decompress()
        .filter(|point| !point.is_small_order())
        .ok_or_else(|| anyhow!("{} does not hold a usable Ed25519 key", did))?;
    Ok(point.to_montgomery().to_bytes())
}

/// The X25519 secret of an Ed25519 secret key (its 32-byte seed, or seed ‖ public key):
/// the clamped first half of `sha512(seed)`, as libsodium computes it.
pub fn x25519_secret_of_ed25519(secret: &[u8]) -> Result<Zeroizing<[u8; 32]>, AnyError> {
    let seed = secret
        .get(..32)
        .ok_or_else(|| anyhow!("An Ed25519 secret key has at least 32 bytes"))?;
    let hash = Zeroizing::new(Sha512::digest(seed));
    let mut scalar = Zeroizing::new([0u8; 32]);
    scalar.copy_from_slice(&hash[..32]);
    scalar[0] &= 248;
    scalar[31] &= 127;
    scalar[31] |= 64;
    Ok(scalar)
}

fn seal_key(
    shared: &MontgomeryPoint,
    ephemeral_public: &[u8; 32],
    recipient_public: &[u8; 32],
) -> Result<Zeroizing<[u8; 32]>, AnyError> {
    // An all-zero shared secret means a low-order point: nothing secret went in.
    if shared.to_bytes() == [0u8; 32] {
        return Err(anyhow!("The sealed box uses a low-order key"));
    }
    let mut salt = [0u8; 64];
    salt[..32].copy_from_slice(ephemeral_public);
    salt[32..].copy_from_slice(recipient_public);
    let mut key = Zeroizing::new([0u8; 32]);
    Hkdf::<Sha256>::new(Some(&salt), shared.as_bytes())
        .expand(SEALED_BOX_INFO, key.as_mut())
        .map_err(|_| anyhow!("HKDF output length"))?;
    Ok(key)
}

/// Seals `plaintext` to `recipient_public` with the given ephemeral secret and nonce.
#[cfg(test)]
fn seal_with(
    recipient_public: &[u8; 32],
    ephemeral_secret: &[u8; 32],
    nonce: &[u8; 12],
    plaintext: &[u8],
) -> Result<SealedBox, AnyError> {
    let ephemeral_public = MontgomeryPoint::mul_base_clamped(*ephemeral_secret).to_bytes();
    let shared = MontgomeryPoint(*recipient_public).mul_clamped(*ephemeral_secret);
    let key = seal_key(&shared, &ephemeral_public, recipient_public)?;
    let ciphertext = Aes256Gcm::new_from_slice(key.as_ref())
        .map_err(|_| anyhow!("AES-256-GCM key length"))?
        .encrypt(Nonce::from_slice(nonce), plaintext)
        .map_err(|_| anyhow!("Could not seal the box"))?;
    Ok(SealedBox {
        ephemeral_public_key: hex::encode(ephemeral_public),
        nonce: hex::encode(nonce),
        ciphertext: hex::encode(ciphertext),
    })
}

/// Seals `plaintext` to the agent `did`. Needs no key: anyone may seal to any DID.
#[cfg(test)]
pub fn seal_to_did(did: &str, plaintext: &[u8]) -> Result<SealedBox, AnyError> {
    let recipient_public = x25519_public_key_of_did(did)?;
    let mut ephemeral_secret = Zeroizing::new([0u8; 32]);
    let mut nonce = [0u8; 12];
    rand::rngs::OsRng.fill_bytes(ephemeral_secret.as_mut());
    rand::rngs::OsRng.fill_bytes(&mut nonce);
    seal_with(&recipient_public, &ephemeral_secret, &nonce, plaintext)
}

fn hex_array<const N: usize>(field: &str, value: &str) -> Result<[u8; N], AnyError> {
    hex::decode(value.trim())
        .ok()
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or_else(|| anyhow!("The sealed box's {} is not {} bytes of hex", field, N))
}

/// Opens `sealed` with the Ed25519 secret key of its recipient.
pub fn open_with_ed25519_secret(
    sealed: &SealedBox,
    ed25519_secret: &[u8],
) -> Result<Vec<u8>, AnyError> {
    let secret = x25519_secret_of_ed25519(ed25519_secret)?;
    let ephemeral_public: [u8; 32] = hex_array("ephemeralPublicKey", &sealed.ephemeral_public_key)?;
    let nonce: [u8; 12] = hex_array("nonce", &sealed.nonce)?;
    let ciphertext = hex::decode(sealed.ciphertext.trim())
        .map_err(|_| anyhow!("The sealed box's ciphertext is not hex"))?;

    let recipient_public = MontgomeryPoint::mul_base_clamped(*secret).to_bytes();
    let shared = MontgomeryPoint(ephemeral_public).mul_clamped(*secret);
    let key = seal_key(&shared, &ephemeral_public, &recipient_public)?;
    Aes256Gcm::new_from_slice(key.as_ref())
        .map_err(|_| anyhow!("AES-256-GCM key length"))?
        .decrypt(Nonce::from_slice(&nonce), ciphertext.as_slice())
        .map_err(|_| anyhow!("The box is not sealed to this agent, or was altered"))
}

/// Opens `sealed` with the key of `context`, the main agent or a managed user. Fails for a
/// box sealed to any other DID.
pub fn open_sealed_for_context(
    sealed: &SealedBox,
    context: &AgentContext,
) -> Result<Vec<u8>, AnyError> {
    let key_name = context.wallet_key_name()?;
    let secret = Zeroizing::new(
        wallet_backend()
            .get_secret_key(&key_name)
            .ok_or_else(|| anyhow!("{} key not found", key_name))?,
    );
    open_with_ed25519_secret(sealed, secret.as_slice())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::did_for_context;

    fn ensure_setup() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
    }

    /// Ed25519 seed 0x01 * 32, as `bootstrap-languages/server-link-language/tests/encryption.test.ts`
    /// pins it: the noble (`ed25519.utils.toMontgomery` / `toMontgomerySecret`) values.
    const SEED: [u8; 32] = [1u8; 32];
    const DID: &str = "did:key:z6Mkon3Necd6NkkyfoGoHxid2znGc59LU3K7mubaRcFbLfLX";
    const X25519_PUBLIC: &str = "1b1b58dd50ea14b60da17b790cd02754d970c9bab864ebb3c0f3016fe51d3f57";
    const X25519_SECRET: &str = "58e86efb75fa4e2c410f46e16de9f6acae1a1703528651b69bc176c088bef36e";

    #[test]
    fn the_x25519_keys_of_a_did_match_the_rfc_7748_map() {
        assert_eq!(
            hex::encode(x25519_public_key_of_did(DID).unwrap()),
            X25519_PUBLIC
        );
        assert_eq!(
            hex::encode(*x25519_secret_of_ed25519(&SEED).unwrap()),
            X25519_SECRET
        );
    }

    #[test]
    fn it_opens_a_box_the_language_tests_sealed() {
        // `sealToDid(DID, "ad4m room key v1", ephemeral 0x02 * 32, nonce 0x03 * 12)` in noble.
        let sealed = SealedBox {
            ephemeral_public_key:
                "ce8d3ad1ccb633ec7b70c17814a5c76ecd029685050d344745ba05870e587d59".into(),
            nonce: "030303030303030303030303".into(),
            ciphertext: "11b99acbd7bdf173901c669b2ed8c4794cb801ac9c2ac9642531fd873dfde2cc".into(),
        };
        assert_eq!(
            open_with_ed25519_secret(&sealed, &SEED).unwrap(),
            b"ad4m room key v1"
        );
        assert_eq!(
            seal_with(
                &x25519_public_key_of_did(DID).unwrap(),
                &[2u8; 32],
                &[3u8; 12],
                b"ad4m room key v1"
            )
            .unwrap(),
            sealed,
            "the same inputs give the same box in Rust and noble"
        );
    }

    #[test]
    fn a_box_opens_only_for_the_agent_it_is_sealed_to() {
        ensure_setup();
        let user = crate::test_utils::create_test_user("sealed.box.user@example.org");
        let main = AgentContext::main_agent();
        let managed = AgentContext::managed(user.clone());

        for (recipient, other) in [(&main, &managed), (&managed, &main)] {
            let did = did_for_context(recipient).unwrap();
            let sealed = seal_to_did(&did, b"room key").unwrap();
            assert_eq!(
                open_sealed_for_context(&sealed, recipient).unwrap(),
                b"room key",
                "{recipient:?} opens a box sealed to its DID"
            );
            assert!(
                open_sealed_for_context(&sealed, other).is_err(),
                "{other:?} opened a box sealed to {recipient:?}"
            );
            assert_ne!(
                seal_to_did(&did, b"room key").unwrap(),
                sealed,
                "each box has its own ephemeral key"
            );
        }
    }

    #[test]
    fn a_tampered_box_does_not_open() {
        let sealed = seal_to_did(DID, b"room key").unwrap();
        let mut ciphertext = hex::decode(&sealed.ciphertext).unwrap();
        ciphertext[0] ^= 1;
        let tampered = SealedBox {
            ciphertext: hex::encode(ciphertext),
            ..sealed.clone()
        };
        assert!(open_with_ed25519_secret(&tampered, &SEED).is_err());
        assert!(open_with_ed25519_secret(&sealed, &[2u8; 32]).is_err());
    }

    #[test]
    fn only_an_ed25519_did_key_is_a_recipient() {
        for did in [
            "did:web:example.org",
            "did:key:zQ3shokFTS3brHcDQrn82RUDfCZESWL1ZdCEJwekUDPQiYBme",
            "did:key:z6Mk",
            "not a did",
        ] {
            assert!(seal_to_did(did, b"x").is_err(), "{did}");
        }
        assert_eq!(
            x25519_public_key_of_did(&format!("{DID}#{}", &DID[8..])).unwrap(),
            x25519_public_key_of_did(DID).unwrap(),
            "a key id names the DID's key"
        );
    }
}
