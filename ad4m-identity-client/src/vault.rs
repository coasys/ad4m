//! The vault holds master keys; [`MasterSigner`] signs with one without giving its bytes out
//! Every use of a master key opens the vault once for a whole flow
//! ([`Vault::unlock`]: one prompt per change to the identity) and the flow's [`Unlocked`]
//! guard expires after [`MasterUse::ttl_s`] and wipes the key when it drops.
//!
//! Implementations: `DesktopVault` (the desktop shell), `PhoneVault` and `WebVault` (neither
//! written yet), and [`MemoryVault`] for tests (feature `test-vault`, never in a release build).

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::inbox::{seal, sign_payload, InboxAddress, InboxBody, InboxError};
use ad4m_identity_core::link::SignedBy;
use ad4m_identity_core::{Event, KeyId, Multikey};
use serde::{Deserialize, Serialize};
use zeroize::Zeroizing;

use crate::secret::Secret;
use crate::{Random, RandomRng};

/// How long one unlock serves a revocation or a push of the master key: five minutes, then the
/// surface asks again.
pub const UNLOCK_TTL_S: u64 = 300;

/// Why the surface asks for the unlock; shown in the OS prompt where the OS allows it. The
/// flows unlock for a link, a revocation and sending the master key to a device that keeps it
/// (`create` stores master #0 without an unlock).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MasterUse {
    Link,
    Revoke,
    SendMaster,
}

impl MasterUse {
    /// How long an unlock for this use lasts. A link signs its `add` up to the end of the
    /// store's confirm grace: the offer lives [`OFFER_TTL_S`](ad4m_identity_core::link::OFFER_TTL_S)
    /// from the unlock and the store takes the `add` [`CONFIRM_GRACE_S`](ad4m_identity_core::link::CONFIRM_GRACE_S)
    /// longer. A revocation and a push of the master key (one seal, one inbox put, the record)
    /// last [`UNLOCK_TTL_S`].
    pub fn ttl_s(self) -> u64 {
        match self {
            MasterUse::Link => {
                ad4m_identity_core::link::OFFER_TTL_S + ad4m_identity_core::link::CONFIRM_GRACE_S
            }
            MasterUse::Revoke | MasterUse::SendMaster => UNLOCK_TTL_S,
        }
    }
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
    /// This master key as `InboxBody::Master { did, index, secret }` in a payload to the device
    /// key `to`, signed by this key at `by`, sealed to the device's `enc` key `to_enc` for
    /// `address`. The secret leaves only sealed. The copies this crate and the core make on the
    /// way are wiped, whether the seal succeeds or fails: the encoded secret in the body, the
    /// JSON values built to sign and seal the payload, the signed bytes and the payload's text
    /// before the seal (both written once at their length, so no shorter copy is left behind
    /// as they grow). What the libraries underneath keep is not
    /// covered: the hash state of the Ed25519 signature over the payload, the cipher's working
    /// buffers, and copies on the stack.
    #[allow(clippy::too_many_arguments)]
    fn seal_self(
        &self,
        did: &str,
        to: &str,
        to_enc: &str,
        address: &InboxAddress,
        by: SignedBy,
        sent_at: u64,
        random: &dyn Random,
    ) -> Result<String, InboxError>;
}

/// One unlocked master key for one flow. Expires [`MasterUse::ttl_s`] after the unlock; the key
/// is wiped when the guard drops.
pub struct Unlocked {
    signer: Box<dyn MasterSigner + Send + Sync>,
    expires_at: u64,
}

impl Unlocked {
    /// The unlock of `signer` at `now` for `why`; it lasts `why`'s [`MasterUse::ttl_s`].
    pub fn new(signer: Box<dyn MasterSigner + Send + Sync>, why: MasterUse, now: u64) -> Self {
        Unlocked {
            signer,
            expires_at: now.saturating_add(why.ttl_s()),
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
///
/// `create` keeps master #0 and the co-signed incept here *before* it publishes the incept
/// ([`Vault::store_pending_incept`]), and forgets the incept once the publish is confirmed
/// ([`Vault::incept_published`]): a publish whose answer is lost is finished later from the
/// vault (`flows::create::resume`), with the same master key and the same identity.
pub trait Vault: Send + Sync {
    /// Stores master `index` for `scid`, replacing an older index.
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: Secret<[u8; 32]>,
    ) -> Result<(), VaultError>;
    /// One unlock (one prompt where the OS needs it) for a flow that uses the master key `why`.
    /// `now` starts the unlock's [`MasterUse::ttl_s`] (pass `why` to [`Unlocked::new`]).
    fn unlock(&self, scid: &str, why: MasterUse, now: u64) -> Result<Unlocked, VaultError>;
    /// Keeps the co-signed incept of a new identity (`incept.scid`), whose master #0 the vault
    /// holds, until [`Vault::incept_published`]; replaces a kept incept of the same identity.
    /// The incept holds public keys and proofs only, no secret.
    fn store_pending_incept(&self, incept: &Event) -> Result<(), VaultError>;
    /// The incepts kept and not yet confirmed published.
    fn pending_incepts(&self) -> Result<Vec<Event>, VaultError>;
    /// The store holds `scid`'s incept: the vault forgets it (no error when it holds none).
    fn incept_published(&self, scid: &str) -> Result<(), VaultError>;
    /// Keeps `push` on the retry list before the master key is put into the device's inbox
    /// (`flows::master`); keeping one already listed is no change.
    fn keep_push(&self, push: &PendingPush) -> Result<(), VaultError>;
    /// The pushes kept and not yet confirmed sent.
    fn pending_pushes(&self) -> Result<Vec<PendingPush>, VaultError>;
    /// The push is done (the store took it) or no longer wanted: the vault forgets it (no error
    /// when it holds none).
    fn push_done(&self, push: &PendingPush) -> Result<(), VaultError>;
    /// Notes `version` of `scid`'s private record as read and answers the highest version read
    /// before it (the high-water mark, `None` before the first read). A store can serve back
    /// an older record with a receipt that still verifies, so the reader refuses a version
    /// under the answer (`crate::record`).
    fn record_seen(&self, scid: &str, version: u64) -> Result<Option<u64>, VaultError>;
    /// Forgets the versions of `scid`'s record seen so far: a caller that means to read an
    /// older version on purpose resets the mark first.
    fn record_seen_reset(&self, scid: &str) -> Result<(), VaultError>;
}

/// A master key to put into a device's inbox: the identity and the device's key id. The retry
/// list keeps it until the store takes it; each try seals the master key current then.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PendingPush {
    pub scid: String,
    pub key_id: KeyId,
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

    fn seal_self(
        &self,
        did: &str,
        to: &str,
        to_enc: &str,
        address: &InboxAddress,
        by: SignedBy,
        sent_at: u64,
        random: &dyn Random,
    ) -> Result<String, InboxError> {
        let secret = Zeroizing::new(self.key.to_bytes());
        let body = InboxBody::Master {
            did: did.to_string(),
            index: self.index,
            secret: bs58::encode(secret.as_ref()).into_string(),
        };
        let mut payload = sign_payload(to.to_string(), body, sent_at, by, self)?;
        let sealed = seal(to_enc, address, &payload, &mut RandomRng(random));
        payload.body.wipe();
        sealed
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
        pending: Mutex<Vec<Event>>,
        pushes: Mutex<Vec<PendingPush>>,
        unlocks: Mutex<Vec<MasterUse>>,
        record_seen: Mutex<HashMap<String, u64>>,
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
                why,
                now,
            ))
        }

        fn store_pending_incept(&self, incept: &Event) -> Result<(), VaultError> {
            let mut pending = self.pending.lock().map_err(poisoned)?;
            pending.retain(|e| e.scid != incept.scid);
            pending.push(incept.clone());
            Ok(())
        }

        fn pending_incepts(&self) -> Result<Vec<Event>, VaultError> {
            Ok(self.pending.lock().map_err(poisoned)?.clone())
        }

        fn incept_published(&self, scid: &str) -> Result<(), VaultError> {
            self.pending
                .lock()
                .map_err(poisoned)?
                .retain(|e| e.scid != scid);
            Ok(())
        }

        fn keep_push(&self, push: &PendingPush) -> Result<(), VaultError> {
            let mut pushes = self.pushes.lock().map_err(poisoned)?;
            if !pushes.contains(push) {
                pushes.push(push.clone());
            }
            Ok(())
        }

        fn pending_pushes(&self) -> Result<Vec<PendingPush>, VaultError> {
            Ok(self.pushes.lock().map_err(poisoned)?.clone())
        }

        fn push_done(&self, push: &PendingPush) -> Result<(), VaultError> {
            self.pushes.lock().map_err(poisoned)?.retain(|p| p != push);
            Ok(())
        }

        fn record_seen(&self, scid: &str, version: u64) -> Result<Option<u64>, VaultError> {
            let mut seen = self.record_seen.lock().map_err(poisoned)?;
            let before = seen.get(scid).copied();
            let high = before.map_or(version, |h| h.max(version));
            seen.insert(scid.to_string(), high);
            Ok(before)
        }

        fn record_seen_reset(&self, scid: &str) -> Result<(), VaultError> {
            self.record_seen.lock().map_err(poisoned)?.remove(scid);
            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn an_unlock_serves_one_flow_until_it_expires() {
            let vault = MemoryVault::new();
            assert!(matches!(
                vault.unlock("Qm", MasterUse::Link, 0),
                Err(VaultError::NotFound)
            ));
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::Revoke, 100).unwrap();
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
            assert_eq!(vault.unlocks(), vec![MasterUse::Revoke]);
        }

        /// A link's unlock lasts until the store's confirm grace ends, so the `add` can be
        /// signed while the store still takes it.
        #[test]
        fn a_link_unlock_lasts_through_the_confirm_grace() {
            use ad4m_identity_core::link::{CONFIRM_GRACE_S, OFFER_TTL_S};
            let vault = MemoryVault::new();
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::Link, 100).unwrap();
            let end = 100 + OFFER_TTL_S + CONFIRM_GRACE_S;
            assert!(unlocked.signer(100 + OFFER_TTL_S + 1).is_ok());
            assert!(unlocked.signer(end - 1).is_ok());
            assert!(matches!(unlocked.signer(end), Err(VaultError::Expired)));
        }

        /// A push of the master key (a seal, an inbox put, the record) lasts a revocation's
        /// five minutes, not a link's grace.
        #[test]
        fn a_send_master_unlock_lasts_five_minutes() {
            let vault = MemoryVault::new();
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::SendMaster, 100).unwrap();
            assert!(unlocked.signer(100 + UNLOCK_TTL_S - 1).is_ok());
            assert!(matches!(
                unlocked.signer(100 + UNLOCK_TTL_S),
                Err(VaultError::Expired)
            ));
        }

        /// `record_seen` keeps each identity's highest record version and answers the one
        /// before, so the reader refuses a version served back under it; `record_seen_reset`
        /// clears the mark.
        #[test]
        fn the_record_high_water_rises_only() {
            let vault = MemoryVault::new();
            assert_eq!(vault.record_seen("Qm", 1).unwrap(), None);
            assert_eq!(vault.record_seen("Qm", 3).unwrap(), Some(1));
            // A version under the mark answers the mark it trips on; the mark does not fall.
            assert_eq!(vault.record_seen("Qm", 2).unwrap(), Some(3));
            assert_eq!(vault.record_seen("Qm", 3).unwrap(), Some(3));
            assert_eq!(vault.record_seen("Qm2", 1).unwrap(), None);
            vault.record_seen_reset("Qm").unwrap();
            assert_eq!(vault.record_seen("Qm", 2).unwrap(), None);
        }
    }
}
