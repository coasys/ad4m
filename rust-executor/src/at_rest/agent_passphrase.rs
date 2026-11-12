//! The agent passphrase from the keyring: the passphrase of the wallet (`ad4m/agent.json`) and
//! of Holochain's keystore, kept so that no person types it and no file on the data disk holds
//! it.
//!
//! - **An agent made after the keyring** gets a passphrase derived from the root key
//!   (HKDF-SHA256, info [`DERIVE_INFO`]): 64 lowercase hex characters. The keyring records
//!   that it is derived, and from which root generation.
//! - **An agent made before the keyring** keeps the passphrase it has: the wallet file and
//!   Holochain's keystore stay as they are. The first time an operator unlocks the agent with
//!   it (`agent.unlock`, or `AD4M_UNLOCK_PASSPHRASE_FILE` at start), the executor checks it
//!   against the wallet file and holds it in the keyring, sealed under the root key
//!   (XChaCha20-Poly1305 under the root's subkey for [`HELD_INFO`]). From then on every start
//!   unlocks the agent from the keyring.
//!
//! Either way the passphrase opens only with the storage secret: the keyring file holds the
//! sealed passphrase or nothing at all.

use std::sync::Arc;

use ad4m_keyring::{hkdf_subkey, SecretKey, SymWrap, WrapAlg, WRAP_NONCE_LEN};
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use rand::RngCore;
use zeroize::Zeroizing;

use super::data_keys::{DataKeys, KeyringError};
use super::keyring_file::KeyringId;

/// The HKDF info of the derived agent passphrase: its own, never shared with another key.
pub const DERIVE_INFO: &str = "ad4m:at-rest:agent-passphrase:v1";
/// The HKDF info of the key that seals a held agent passphrase, and its associated data's
/// label.
pub const HELD_INFO: &str = "ad4m:at-rest:held-agent-passphrase:v1";

/// The passphrase derived from `root`: 64 lowercase hex characters.
pub fn derive(root: &SecretKey) -> Zeroizing<String> {
    let key = hkdf_subkey(root, &[], DERIVE_INFO);
    Zeroizing::new(hex::encode(key.expose_secret()))
}

fn held_cipher(root: &SecretKey) -> XChaCha20Poly1305 {
    let key = hkdf_subkey(root, &[], HELD_INFO);
    XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key")
}

/// The associated data of a held passphrase: the label, the keyring and the root generation,
/// so a sealed passphrase copied to another keyring or generation does not open.
fn held_associated_data(keyring_id: &KeyringId, generation: u32) -> Vec<u8> {
    let mut ad = Vec::with_capacity(8 + HELD_INFO.len() + 16 + 4);
    ad.extend_from_slice(&(HELD_INFO.len() as u64).to_be_bytes());
    ad.extend_from_slice(HELD_INFO.as_bytes());
    ad.extend_from_slice(&keyring_id.0);
    ad.extend_from_slice(&generation.to_be_bytes());
    ad
}

/// Seals `passphrase` under `root` of `generation` for the keyring `keyring_id`.
pub fn seal_held(
    passphrase: &str,
    root: &SecretKey,
    keyring_id: &KeyringId,
    generation: u32,
) -> SymWrap {
    let mut nonce = [0u8; WRAP_NONCE_LEN];
    rand::thread_rng().fill_bytes(&mut nonce);
    let ct = held_cipher(root)
        .encrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: passphrase.as_bytes(),
                aad: &held_associated_data(keyring_id, generation),
            },
        )
        .expect("XChaCha20-Poly1305 seals a passphrase");
    SymWrap {
        alg: WrapAlg::XChaCha20Poly1305,
        nonce,
        ct,
    }
}

/// Opens a passphrase [`seal_held`] sealed. `None` for another root, keyring or generation,
/// or an altered seal.
pub fn open_held(
    sealed: &SymWrap,
    root: &SecretKey,
    keyring_id: &KeyringId,
    generation: u32,
) -> Option<Zeroizing<String>> {
    let WrapAlg::XChaCha20Poly1305 = sealed.alg;
    let plain = Zeroizing::new(
        held_cipher(root)
            .decrypt(
                XNonce::from_slice(&sealed.nonce),
                Payload {
                    msg: &sealed.ct,
                    aad: &held_associated_data(keyring_id, generation),
                },
            )
            .ok()?,
    );
    std::str::from_utf8(&plain)
        .ok()
        .map(|text| Zeroizing::new(text.to_string()))
}

/// The keyring of this process, once `run` opened it: `None` in unit tests and before the
/// storage secret opened the keyring.
pub fn keys() -> Option<Arc<DataKeys>> {
    super::gate()
        .map(|gate| gate.keys().clone())
        .filter(|keys| keys.is_open())
}

/// The agent passphrase the keyring holds or derives, if any. A keyring that cannot give it
/// (a seal that does not open) is logged and answers `None`: the agent then stays locked
/// until an operator unlocks it.
pub fn from_keyring(keys: Option<&DataKeys>) -> Option<Zeroizing<String>> {
    match keys?.agent_passphrase() {
        Ok(passphrase) => passphrase,
        Err(KeyringError::Sealed) => None,
        Err(e) => {
            log::error!("🔑 The keyring does not give the agent passphrase: {e}");
            None
        }
    }
}

/// The passphrase of an agent being made now: the keyring's (derived from the root key, and
/// recorded, unless it already holds one), or `typed` without a keyring.
pub fn for_new_agent(
    keys: Option<&DataKeys>,
    typed: &str,
) -> Result<Zeroizing<String>, KeyringError> {
    match keys {
        Some(keys) if keys.is_open() => keys.derive_agent_passphrase(),
        _ => Ok(Zeroizing::new(typed.to_string())),
    }
}

/// Holds `passphrase`, which just opened the wallet file, in the keyring, unless the keyring
/// already gives one. Best effort: a failure is logged and the next unlock tries again.
pub fn capture(keys: Option<&DataKeys>, passphrase: &str) {
    let Some(keys) = keys.filter(|k| k.is_open()) else {
        return;
    };
    match keys.hold_agent_passphrase(passphrase) {
        Ok(true) => log::info!(
            "🔑 The keyring now holds the agent passphrase: later starts unlock the agent \
             without it"
        ),
        Ok(false) => {}
        Err(e) => log::warn!("🔑 Could not hold the agent passphrase in the keyring: {e}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(byte: u8) -> SecretKey {
        SecretKey::from_slice(&[byte; 32]).unwrap()
    }

    #[test]
    fn the_derived_passphrase_is_64_hex_characters_of_its_root() {
        let a = derive(&key(1));
        assert_eq!(a.len(), 64);
        assert!(a
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()));
        assert_eq!(a, derive(&key(1)));
        assert_ne!(a, derive(&key(2)));
    }

    #[test]
    fn a_held_passphrase_opens_only_under_its_root_keyring_and_generation() {
        let id = KeyringId::random();
        let sealed = seal_held("the typed passphrase", &key(1), &id, 1);
        assert_eq!(
            open_held(&sealed, &key(1), &id, 1)
                .as_deref()
                .map(String::as_str),
            Some("the typed passphrase")
        );
        assert!(
            open_held(&sealed, &key(2), &id, 1).is_none(),
            "another root"
        );
        assert!(
            open_held(&sealed, &key(1), &KeyringId::random(), 1).is_none(),
            "another keyring"
        );
        assert!(
            open_held(&sealed, &key(1), &id, 2).is_none(),
            "another generation"
        );
        let mut altered = sealed.clone();
        altered.ct[0] ^= 1;
        assert!(open_held(&altered, &key(1), &id, 1).is_none(), "altered");
        let text = serde_json::to_string(&sealed).unwrap();
        assert!(!text.contains("typed"));
        assert!(!text.contains(&hex::encode("the typed passphrase")));
    }
}
