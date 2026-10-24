//! The vault holds master keys; [`MasterSigner`] signs with one without giving its bytes out
//! (AR-03 4.3.2). Every use of a master key opens the vault once for a whole flow
//! ([`Vault::unlock`]: one prompt per change to the identity) and the flow's [`Unlocked`]
//! guard expires after [`UNLOCK_TTL_S`] and wipes the key when it drops.
//!
//! Implementations: `DesktopVault` (the desktop shell), `PhoneVault`, `WebVault` (later
//! slices), and [`MemoryVault`] for tests (feature `test-vault`, never in a release build).

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::Multikey;

use crate::secret::Secret;

/// How long one unlock serves a flow (AR-03 4.2 rule 2).
pub const UNLOCK_TTL_S: u64 = 300;

/// Why the surface asks for the unlock; shown in the OS prompt where the OS allows it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MasterUse {
    Create,
    Link,
    Revoke,
    Rotate,
    Scope,
    PauseRecordWrite,
    TakeMaster,
    Rewrap,
    PasswordChange,
    RemoveFromDevice,
    DeleteAccount,
    ConfirmRequest,
}

/// How the surface unlocks its vault.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnlockKind {
    TouchIdOrPassword,
    Passphrase,
    Biometry,
    DevicePasscode,
    BrowserSession,
}

/// What the vault holds for an identity (no prompt).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VaultStatus {
    /// The index of the master key held, if any.
    pub master_index: Option<u32>,
    pub unlock: UnlockKind,
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum VaultError {
    #[error("the unlock was cancelled")]
    Cancelled,
    #[error("the device has no screen lock")]
    NoScreenLock,
    #[error("the vault is unavailable: {0}")]
    Unavailable(String),
    #[error("no master key for this identity")]
    NotFound,
    #[error("the unlock expired")]
    Expired,
    #[error("the vault's item was tampered with")]
    Tampered,
}

/// Signs with a master key; never gives its bytes out.
pub trait MasterSigner: Signer {
    /// The master key's index *n*.
    fn index(&self) -> u32;
}

/// One unlocked master key for one flow. Expires [`UNLOCK_TTL_S`] after the unlock; the key
/// is wiped when the guard drops.
pub struct Unlocked {
    signer: Box<dyn MasterSigner + Send + Sync>,
    expires_at: u64,
}

impl Unlocked {
    pub fn new(signer: Box<dyn MasterSigner + Send + Sync>, now: u64) -> Self {
        Unlocked {
            signer,
            expires_at: now.saturating_add(UNLOCK_TTL_S),
        }
    }

    /// The signer, while the unlock lasts at `now`.
    pub fn signer(&self, now: u64) -> Result<&dyn MasterSigner, VaultError> {
        if now >= self.expires_at {
            return Err(VaultError::Expired);
        }
        Ok(self.signer.as_ref())
    }
}

/// The custody of master keys on one surface.
pub trait Vault: Send + Sync {
    /// What the vault holds for `scid` (no prompt).
    fn status(&self, scid: &str) -> Result<VaultStatus, VaultError>;
    /// Stores master `index` for `scid`, replacing an older index.
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: Secret<[u8; 32]>,
    ) -> Result<(), VaultError>;
    /// One unlock (one prompt where the OS needs it) for a flow that uses the master key `why`.
    /// `now` starts the unlock's [`UNLOCK_TTL_S`].
    fn unlock(&self, scid: &str, why: MasterUse, now: u64) -> Result<Unlocked, VaultError>;
    /// Deletes the master key (the device may receive one again).
    fn delete_master(&self, scid: &str) -> Result<(), VaultError>;
    /// Deletes the whole custody record of `scid`.
    fn forget(&self, scid: &str) -> Result<(), VaultError>;
}

/// A master key in memory: an Ed25519 key that zeroes on drop.
pub struct KeySigner {
    key: ed25519_dalek::SigningKey,
    index: u32,
}

impl KeySigner {
    pub fn new(secret: &[u8; 32], index: u32) -> Self {
        KeySigner {
            key: ed25519_dalek::SigningKey::from_bytes(secret),
            index,
        }
    }
}

impl Signer for KeySigner {
    fn key(&self) -> Multikey {
        Signer::key(&self.key)
    }
    fn sign(&self, bytes: &[u8]) -> [u8; 64] {
        Signer::sign(&self.key, bytes)
    }
}

impl MasterSigner for KeySigner {
    fn index(&self) -> u32 {
        self.index
    }
}

#[cfg(any(test, feature = "test-vault"))]
pub use memory::MemoryVault;

#[cfg(any(test, feature = "test-vault"))]
mod memory {
    use std::collections::HashMap;
    use std::sync::Mutex;

    use super::*;

    /// Each identity's master index and key.
    type Keys = HashMap<String, (u32, Secret<[u8; 32]>)>;

    /// Master keys in memory, for tests. Records every unlock so tests can count prompts.
    #[derive(Default)]
    pub struct MemoryVault {
        keys: Mutex<Keys>,
        unlocks: Mutex<Vec<MasterUse>>,
    }

    impl MemoryVault {
        pub fn new() -> Self {
            Self::default()
        }

        /// Every unlock so far, in order.
        pub fn unlocks(&self) -> Vec<MasterUse> {
            self.unlocks.lock().map(|u| u.clone()).unwrap_or_default()
        }
    }

    fn poisoned<T>(_: T) -> VaultError {
        VaultError::Unavailable("the memory vault's lock is poisoned".into())
    }

    impl Vault for MemoryVault {
        fn status(&self, scid: &str) -> Result<VaultStatus, VaultError> {
            let keys = self.keys.lock().map_err(poisoned)?;
            Ok(VaultStatus {
                master_index: keys.get(scid).map(|(index, _)| *index),
                unlock: UnlockKind::Passphrase,
            })
        }

        fn store_master(
            &self,
            scid: &str,
            index: u32,
            secret: Secret<[u8; 32]>,
        ) -> Result<(), VaultError> {
            let mut keys = self.keys.lock().map_err(poisoned)?;
            keys.insert(scid.to_string(), (index, secret));
            Ok(())
        }

        fn unlock(&self, scid: &str, why: MasterUse, now: u64) -> Result<Unlocked, VaultError> {
            let keys = self.keys.lock().map_err(poisoned)?;
            let (index, secret) = keys.get(scid).ok_or(VaultError::NotFound)?;
            self.unlocks.lock().map_err(poisoned)?.push(why);
            Ok(Unlocked::new(
                Box::new(KeySigner::new(secret.expose(), *index)),
                now,
            ))
        }

        fn delete_master(&self, scid: &str) -> Result<(), VaultError> {
            self.keys.lock().map_err(poisoned)?.remove(scid);
            Ok(())
        }

        fn forget(&self, scid: &str) -> Result<(), VaultError> {
            self.delete_master(scid)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn an_unlock_serves_one_flow_until_it_expires() {
            let vault = MemoryVault::new();
            assert_eq!(vault.status("Qm").unwrap().master_index, None);
            assert!(matches!(
                vault.unlock("Qm", MasterUse::Link, 0),
                Err(VaultError::NotFound)
            ));
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            assert_eq!(vault.status("Qm").unwrap().master_index, Some(0));
            let unlocked = vault.unlock("Qm", MasterUse::Link, 100).unwrap();
            let signer = unlocked.signer(100 + UNLOCK_TTL_S - 1).unwrap();
            assert_eq!(signer.index(), 0);
            assert_eq!(
                signer.key(),
                Signer::key(&ed25519_dalek::SigningKey::from_bytes(&[3; 32]))
            );
            assert!(matches!(
                unlocked.signer(100 + UNLOCK_TTL_S),
                Err(VaultError::Expired)
            ));
            assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
            vault.forget("Qm").unwrap();
            assert_eq!(vault.status("Qm").unwrap().master_index, None);
        }
    }
}
