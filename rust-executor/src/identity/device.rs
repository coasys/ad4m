//! An agent's device key as the key log sees it (AR-01 4.10): the wallet key (`main` for the
//! main agent, the DID for a managed agent), its multikey, the X25519 `enc` key `did_key`
//! derives from it (no second secret), and a [`Signer`] that signs with the wallet.

use std::sync::Mutex;

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::Multikey;
use did_key::KeyMaterial;

use crate::agent::{did_for_context, sign_for_context, AgentContext};

/// The device `did:key` of `agent`, and its multikey (`z6Mk…`).
pub fn device_key(agent: &AgentContext) -> Result<(String, Multikey), String> {
    let did = did_for_context(agent).map_err(|e| e.to_string())?;
    let key = multikey_of(&did)?;
    Ok((did, key))
}

/// The multikey of a `did:key:z6Mk…`.
pub fn multikey_of(did: &str) -> Result<Multikey, String> {
    let key = did
        .strip_prefix("did:key:")
        .ok_or_else(|| format!("{did} is not a did:key"))?;
    if !ad4m_identity_core::is_ed25519_multikey(key) {
        return Err(format!("{did} is not an Ed25519 did:key"));
    }
    Ok(key.to_string())
}

/// The X25519 multikey (`z6LS…`) that `did_key` derives from the Ed25519 multikey `key`.
pub fn enc_key_of(key: &str) -> Result<Multikey, String> {
    let public = ad4m_hash_chain::decode_ed25519_multikey(key)
        .ok_or_else(|| format!("{key} is not an Ed25519 multikey"))?;
    // `get_x25519` unwraps the point's decompression: check it here first.
    if ed25519_dalek::VerifyingKey::from_bytes(&public).is_err() {
        return Err(format!("{key} is not a point on the curve"));
    }
    let x25519 = did_key::Ed25519KeyPair::from_public_key(&public).get_x25519();
    let bytes: [u8; 32] = x25519
        .public_key_bytes()
        .try_into()
        .map_err(|_| "the X25519 key is not 32 bytes".to_string())?;
    Ok(ad4m_identity_core::encode_x25519_multikey(&bytes))
}

/// Signs with `agent`'s wallet key. `Signer::sign` cannot fail, so a wallet failure (a locked
/// wallet, a missing key) leaves a zero signature and is kept: call [`WalletSigner::check`]
/// after signing.
pub struct WalletSigner {
    agent: AgentContext,
    key: Multikey,
    failure: Mutex<Option<String>>,
}

impl WalletSigner {
    pub fn new(agent: &AgentContext) -> Result<Self, String> {
        let (_, key) = device_key(agent)?;
        Ok(WalletSigner {
            agent: agent.clone(),
            key,
            failure: Mutex::new(None),
        })
    }

    /// The first wallet failure while signing, if any.
    pub fn check(&self) -> Result<(), String> {
        match self.failure.lock().map(|f| f.clone()) {
            Ok(None) => Ok(()),
            Ok(Some(e)) => Err(e),
            Err(_) => Err("the signer's failure slot is poisoned".into()),
        }
    }

    fn fail(&self, message: String) {
        if let Ok(mut slot) = self.failure.lock() {
            slot.get_or_insert(message);
        }
    }
}

impl Signer for WalletSigner {
    fn key(&self) -> Multikey {
        self.key.clone()
    }

    fn sign(&self, bytes: &[u8]) -> [u8; 64] {
        match sign_for_context(bytes, &self.agent) {
            Ok(sig) => match <[u8; 64]>::try_from(sig.as_slice()) {
                Ok(sig) => sig,
                Err(_) => {
                    self.fail(format!("the wallet gave a {}-byte signature", sig.len()));
                    [0; 64]
                }
            },
            Err(e) => {
                self.fail(format!("the wallet could not sign: {e}"));
                [0; 64]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_dalek::SigningKey;

    /// The enc key is the X25519 key of the same secret, whichever side derives it.
    #[test]
    fn the_enc_key_matches_the_secret_side_derivation() {
        let secret = SigningKey::from_bytes(&[7; 32]);
        let key = Signer::key(&secret);
        let from_public = enc_key_of(&key).unwrap();
        // The wallet's own path: the key pair from the secret, then its X25519 key.
        let pair = did_key::Ed25519KeyPair::from_seed(&secret.to_bytes());
        let from_secret: [u8; 32] = pair.get_x25519().public_key_bytes().try_into().unwrap();
        assert_eq!(
            from_public,
            ad4m_identity_core::encode_x25519_multikey(&from_secret)
        );
        assert!(ad4m_identity_core::is_x25519_multikey(&from_public));
    }

    #[test]
    fn only_an_ed25519_did_key_has_a_multikey() {
        let secret = SigningKey::from_bytes(&[7; 32]);
        let key = Signer::key(&secret);
        assert_eq!(multikey_of(&format!("did:key:{key}")).unwrap(), key);
        assert!(multikey_of(&key).is_err());
        assert!(multikey_of("did:key:z6LSbysY2xFMRpGMhb7tFTLMpeuPRaqaWM1yECx2AtzE3KCc").is_err());
        assert!(enc_key_of("z6LSbysY2xFMRpGMhb7tFTLMpeuPRaqaWM1yECx2AtzE3KCc").is_err());
    }
}
