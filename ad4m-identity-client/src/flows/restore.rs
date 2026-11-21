//! `restore`: the 12 words alone give the identity back on a new device, and lock out the
//! devices that were lost with the old one.
//!
//! 1. [`RestoreFlow::start`]: the words are parsed (a word not in the list is named, with its
//!    position and the nearest word of the list); they derive master #0, and the store's key
//!    index (`identity.keyOwner`) names the identity whose `m0` it is. The log must list that
//!    key as master #0, and the words must derive its current master. The record is read with
//!    the words' recovery key (the current master key signs the read), so the devices are
//!    shown with their names; without a readable record they are shown by key id.
//! 2. The person picks the devices to lock out. Apps (desktop, phone, browser, and a device
//!    the record does not name) are ticked by default; servers, community hosts, Coasys cloud
//!    and bots are not.
//! 3. [`RestoreFlow::finish`]: one `rotate` to a fresh master *n*+1 that names no key
//!    ([`super::rotate`], with its rebuild after a `409`); the vault stages *n*+1 before the
//!    publish (a vault that refuses leaves nothing public) and promotes it after. Then an
//!    explicit `revoke` of the ticked devices, signed by *n*+1, with the person's `since`
//!    (ms; the time from which a lost device is no longer trusted). Rotating first recovers an
//!    identity a rival event froze (the store then accepts only a `rotate`); the lock-out is a
//!    revoke, not an unlink, because a lost device is the case where a thief signs new links
//!    with an old anchor, which only a revoke flags. The devices are reported locked out
//!    ([`Restored::revoked`]) only once the revoke is published. Then this
//!    device is enrolled ([`RestoreFlow::enrol`]): linked as a device: an offer bound to this executor's device key,
//!    claimed by this executor and spent by an `add` signed by master *n*+1. A device whose key
//!    the log already lists is not linked again.
//! 4. Master *n*+1 goes to every app that kept the master key and was not locked out (the
//!    revoked devices are no devices of the log then), and the record is written with the
//!    recovery key: this device listed as holding the master key, the revoked ones dropped.
//!
//! Once the rotation is published, `finish` answers [`Restored`] whatever comes after: the
//! rotation stands (*n*+1 is current) and the lock-out is revoked. A revoke that failed after
//! the rotation is [`ClientError::RotatedNotRevoked`]: [`RestoreFlow::revoke_locked_out`]
//! publishes only the revoke again on the same flow, with a `since` the caller may correct
//! (the store refuses one before a key's add), and then enrols. Until it is published the lost
//! devices are still valid, so [`RestoreFlow::enrol`] and `finish` refuse. An enrolment that
//! failed (the vault, the link, the store) is [`Restored::enrolled`]'s error, and
//! [`RestoreFlow::enrol`] retries it on the same flow, without the words typed again and
//! without another rotation.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::record::{DeviceEntry, DeviceKind, PrivateRecord};
use ad4m_identity_core::words::{self, Seed};
use ad4m_identity_core::{KelChain, KeyId};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::flows::link::LinkFlow;
use crate::flows::revoke::{chain_head, check_since, publish_pending, PendingRevoke};
use crate::flows::rotate::{
    current_index, hand_over, master_of, promote, publish_rotation, recovery_key_of, ThisDevice,
};
use crate::record::{self, Reader};
use crate::request::{chain_of, scid_of};
use crate::secret::Secret;
use crate::vault::{MasterSigner, Stored, Vault, VaultError};
use crate::{ClientError, Clock, Random};

/// A device key of the log, as the lock-out screen shows it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ListedDevice {
    pub key_id: KeyId,
    /// From the record; `None` when the record does not name it or could not be read.
    pub name: Option<String>,
    pub kind: Option<DeviceKind>,
    /// Ticked by default ([`locked_out_by_default`]).
    pub lock_out: bool,
}

/// Whether the lock-out screen ticks a device of `kind`: apps, which are lost with a phone or
/// a laptop, and a device the record does not name; not servers, community hosts, Coasys
/// cloud or bots, which run elsewhere.
pub fn locked_out_by_default(kind: Option<DeviceKind>) -> bool {
    !matches!(
        kind,
        Some(
            DeviceKind::Server
                | DeviceKind::CommunityHost
                | DeviceKind::CoasysCloud
                | DeviceKind::Bot
        )
    )
}

/// A restore whose rotation is published: it stands whatever `enrolled` says.
#[derive(Debug)]
pub struct Restored {
    pub did: String,
    /// The head after the rotation, or after the revoke that follows it.
    pub head: SignedHead,
    /// The new master's index.
    pub index: u32,
    /// The device keys locked out: revoked by an event published before this answer, with the
    /// `since` the caller gave.
    pub revoked: Vec<KeyId>,
    /// Rotations built (more than 1 when another event landed first).
    pub tries: usize,
    /// This device enrolled after the rotation. An error (the vault did not keep master
    /// *n*+1, the link failed) leaves the rotation as it is: [`RestoreFlow::enrol`] retries.
    pub enrolled: Result<Enrolled, ClientError>,
}

/// This device enrolled after a restore's rotation: the vault keeps the new master, this
/// executor's device key is in the log, the apps that kept the master key were sent it.
#[derive(Debug)]
pub struct Enrolled {
    /// This device's key id in the log.
    pub key_id: KeyId,
    /// The new master key put into each kept holder's inbox.
    pub pushes: Vec<(KeyId, Result<String, ClientError>)>,
    /// The record write that lists this device. The rotation and the link stand either way.
    pub record: Result<u64, ClientError>,
}

/// One restore, between the words typed and the lock-out chosen. Dropping it wipes the seed.
pub struct RestoreFlow<'a, R: ExecutorRpc> {
    executor: &'a ViaExecutor<R>,
    vault: &'a dyn Vault,
    clock: &'a dyn Clock,
    random: &'a dyn Random,
    seed: Seed,
    did: String,
    devices: Vec<ListedDevice>,
    record_error: Option<ClientError>,
    /// The index of the master this flow's rotation made current, once it is published.
    rotated: Option<u32>,
    /// The lock-out whose revoke is not published yet, after a published rotation.
    pending: Option<LockOut>,
}

/// A lock-out waiting for its revoke.
struct LockOut {
    revoke: PendingRevoke,
    /// Rotations the flow built.
    tries: usize,
}

/// The refusal while the lock-out's revoke is not published.
fn pending_revoke() -> ClientError {
    ClientError::Protocol(
        "the lost devices of this restore are not revoked yet; publish the revoke with \
         revoke_locked_out first"
            .into(),
    )
}

/// The device keys of `chain` valid at its head, except `own` (this executor's device key),
/// named from `record`.
fn listed(chain: &KelChain, own: &str, record: Option<&PrivateRecord>) -> Vec<ListedDevice> {
    let state = chain.state();
    let head = chain.head().seq;
    state
        .keys
        .values()
        .filter(|k| k.sign != own && state.key_valid_at(&k.id, head))
        .map(|k| {
            let entry = record.and_then(|r| r.devices.get(&k.id));
            let kind = entry.map(|e| e.kind);
            ListedDevice {
                key_id: k.id.clone(),
                name: entry.map(|e| e.name.clone()),
                kind,
                lock_out: locked_out_by_default(kind),
            }
        })
        .collect()
}

impl<'a, R: ExecutorRpc> RestoreFlow<'a, R> {
    /// Finds the identity of the typed `words` and lists its devices for the lock-out screen.
    /// Refused with [`ClientError::Words`] when a word is not in the list or the checksum
    /// fails, and with [`ClientError::NotTheseWords`] when no identity has the master #0 they
    /// derive or they do not derive its current master.
    pub async fn start(
        executor: &'a ViaExecutor<R>,
        vault: &'a dyn Vault,
        words: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<RestoreFlow<'a, R>, ClientError> {
        let seed = words::seed(&words::parse(words)?);
        let m0 = Signer::key(&words::master_key(&seed, 0)?);
        let owner = executor
            .key_owner(&m0)
            .await?
            .filter(|owner| owner.key_id == "m0")
            .ok_or_else(|| {
                ClientError::NotTheseWords(
                    "no identity has the master key they derive; check each word".into(),
                )
            })?;
        let chain = chain_of(executor, &owner.did).await?;
        if chain.state().masters.first().map(|m| &m.sign) != Some(&m0) {
            return Err(ClientError::NotTheseWords(
                "the log does not list their master key as master #0".into(),
            ));
        }
        let index = current_index(&chain, &seed)?;
        let status = executor.status().await?;
        let master = master_of(&seed, index)?;
        let secret = words::recovery_secret(&seed);
        let reader = Reader::Recovery {
            secret: &secret,
            master: &master,
            clock,
        };
        let (record, record_error) =
            match record::read_as(executor, vault, &owner.did, &chain, &reader, random).await {
                Ok(opened) => (opened.map(|o| o.record), None),
                Err(e) => (None, Some(e)),
            };
        let devices = listed(&chain, &status.device_key, record.as_ref());
        Ok(RestoreFlow {
            executor,
            vault,
            clock,
            random,
            seed,
            did: owner.did,
            devices,
            record_error,
            rotated: None,
            pending: None,
        })
    }

    /// The identity the words are master #0 of.
    pub fn did(&self) -> &str {
        &self.did
    }

    /// The device keys of the log valid at its head (this device's own key left out), with
    /// the default lock-out.
    pub fn devices(&self) -> &[ListedDevice] {
        &self.devices
    }

    /// Why the record could not be read, when it could not: the devices are shown by key id.
    pub fn record_error(&self) -> Option<&ClientError> {
        self.record_error.as_ref()
    }

    /// Rotates to a fresh master on this device, then revokes `lock_out` (key ids of
    /// [`RestoreFlow::devices`]) with an event signed by it, carrying `since` (ms: the time
    /// from which the devices are no longer trusted; `None` is the revoke's append time), then
    /// enrols this device as `this_device` ([`RestoreFlow::enrol`]). An error means nothing
    /// was published (or, after a publish whose answer was lost, that it is not known; the
    /// vault keeps the staged key for `refresh_master`), except
    /// [`ClientError::RotatedNotRevoked`]: the rotation is published and the revoke is not
    /// ([`RestoreFlow::revoke_locked_out`] retries it). `since` later than now is refused
    /// before the rotation. Once the rotation and the revoke are published it answers
    /// [`Restored`], whose `enrolled` says whether the enrolment worked. Refused after a
    /// rotation of this flow was published: retry the enrolment with [`RestoreFlow::enrol`],
    /// or, while its revoke is pending, with [`RestoreFlow::revoke_locked_out`].
    pub async fn finish(
        &mut self,
        lock_out: &[KeyId],
        since: Option<u64>,
        this_device: DeviceEntry,
    ) -> Result<Restored, ClientError> {
        if self.pending.is_some() {
            return Err(pending_revoke());
        }
        if let Some(index) = self.rotated {
            return Err(ClientError::Protocol(format!(
                "this restore already made master #{index} current; enrol this device again"
            )));
        }
        if let Some(id) = lock_out
            .iter()
            .find(|id| !self.devices.iter().any(|d| &d.key_id == *id))
        {
            return Err(ClientError::Protocol(format!(
                "{id} is not a device listed for the lock-out"
            )));
        }
        check_since(since, self.clock)?;
        let scid = scid_of(&self.did)?;
        // The rotation names no key: the lock-out is the revoke that follows.
        let published = publish_rotation(
            self.executor,
            self.vault,
            &scid,
            &self.did,
            &self.seed,
            &[],
            self.random,
        )
        .await?;
        let index = published.signer.index();
        self.rotated = Some(index);
        self.pending = Some(LockOut {
            revoke: PendingRevoke {
                index,
                key_ids: lock_out.to_vec(),
            },
            tries: published.tries,
        });
        self.lock_out_and_enrol(published.head, since, this_device)
            .await
    }

    /// Publishes only the revoke of the lock-out of a [`RestoreFlow::finish`] whose rotation was
    /// published and whose revoke was not ([`ClientError::RotatedNotRevoked`]), with `since`
    /// (ms; the caller may correct the one `finish` used, e.g. one the store refused for being
    /// before a key's add), then enrols this device. Refused when no revoke is pending, and
    /// as [`super::revoke::finish_revoke`] is.
    pub async fn revoke_locked_out(
        &mut self,
        since: Option<u64>,
        this_device: DeviceEntry,
    ) -> Result<Restored, ClientError> {
        if self.pending.is_none() {
            return Err(ClientError::Protocol(
                "this restore has no lock-out waiting for its revoke".into(),
            ));
        }
        let head = chain_head(self.executor, &self.did).await?;
        self.lock_out_and_enrol(head, since, this_device).await
    }

    /// Publishes the pending revoke (the new master, from the words, signs it), then enrols.
    async fn lock_out_and_enrol(
        &mut self,
        head: SignedHead,
        since: Option<u64>,
        this_device: DeviceEntry,
    ) -> Result<Restored, ClientError> {
        let index = self.rotated.ok_or_else(|| {
            ClientError::Protocol("this restore has published no rotation yet".into())
        })?;
        let mut head = head;
        let mut revoked = Vec::new();
        let mut tries = 1;
        if let Some(lock) = &self.pending {
            tries = lock.tries;
            if !lock.revoke.key_ids.is_empty() {
                head = publish_pending(
                    self.executor,
                    &self.did,
                    &self.seed,
                    &lock.revoke,
                    since,
                    self.clock,
                )
                .await?;
                revoked = lock.revoke.key_ids.clone();
            }
        }
        self.pending = None;
        // The rotation and the revoke are public from here: what fails after them is reported,
        // not returned. The record write of the enrolment drops the revoked devices, and the
        // hand-over sends the new master to none of them.
        let enrolled = self.enrol_excluding(this_device, &revoked).await;
        Ok(Restored {
            did: self.did.clone(),
            head,
            index,
            revoked,
            tries,
            enrolled,
        })
    }

    /// Enrols this device after this flow's published rotation (what [`RestoreFlow::finish`]
    /// does after the publish; call it again when [`Restored::enrolled`] is an error): the
    /// vault keeps the new master (it promotes the staged key, or stores it again from the
    /// words this flow holds), this executor is linked as `this_device` when the log does not
    /// list it, the new master goes to the apps that kept the old one, and the record is
    /// written. Refused before a rotation of this flow was published, while its lock-out
    /// revoke is pending (the lost devices are valid until then, and would be sent the new
    /// master: [`RestoreFlow::revoke_locked_out`]), and when the log's current master is no
    /// longer the one this flow made current.
    pub async fn enrol(&self, this_device: DeviceEntry) -> Result<Enrolled, ClientError> {
        if self.pending.is_some() {
            return Err(pending_revoke());
        }
        self.enrol_excluding(this_device, &[]).await
    }

    /// [`RestoreFlow::enrol`] that sends the new master to none of `exclude` (the lock-out).
    async fn enrol_excluding(
        &self,
        this_device: DeviceEntry,
        exclude: &[KeyId],
    ) -> Result<Enrolled, ClientError> {
        let index = self.rotated.ok_or_else(|| {
            ClientError::Protocol("this restore has published no rotation yet".into())
        })?;
        let scid = scid_of(&self.did)?;
        let chain = chain_of(self.executor, &self.did).await?;
        if current_index(&chain, &self.seed)? != index {
            return Err(ClientError::Protocol(format!(
                "master #{index} of this restore was replaced; restore again with the words"
            )));
        }
        // Promoted here or by another app's refresh first ([`promote`]); nothing staged at
        // `index` and the vault keeping no master at or past it: current in the log, stored
        // under the vault's never-go-back rule as a key taken from the inbox would be. `kept`
        // names the index the vault keeps.
        let kept = match promote(self.vault, &scid, index) {
            Ok(kept) => kept,
            Err(VaultError::NotFound) => {
                let key = words::master_key(&self.seed, index)?;
                match self
                    .vault
                    .store_master(&scid, index, Secret::new(key.to_bytes()))?
                {
                    Stored::Stored => index,
                    Stored::AlreadyNewer { kept } => kept,
                }
            }
            Err(e) => return Err(e.into()),
        };
        let key_id = self.link_this(&this_device).await?;
        // The mark the hand-over makes is fenced by the custody epoch the promote or store
        // just produced.
        let fence_epoch = self.vault.custody_epoch(&scid)?;
        let signer = master_of(&self.seed, index)?;
        let secret = words::recovery_secret(&self.seed);
        let reader = Reader::Recovery {
            secret: &secret,
            master: &signer,
            clock: self.clock,
        };
        let (pushes, record) = hand_over(
            self.executor,
            self.vault,
            &self.did,
            &signer,
            &reader,
            Some(ThisDevice {
                key_id: key_id.clone(),
                entry: Some(this_device),
                kept: Some(kept),
            }),
            Some(fence_epoch),
            &recovery_key_of(&self.seed),
            exclude,
            self.clock,
            self.random,
        )
        .await;
        Ok(Enrolled {
            key_id,
            pushes,
            record,
        })
    }

    /// This executor's device key in the log: its id when the log lists it, else a link of
    /// it, with an offer bound to it that this executor claims and the vault's new master
    /// spends.
    async fn link_this(&self, this_device: &DeviceEntry) -> Result<KeyId, ClientError> {
        let status = self.executor.status().await?;
        let chain = chain_of(self.executor, &self.did).await?;
        let state = chain.state();
        if let Some(id) = state.device_key_id_of(&status.device_key) {
            if state.key_valid_at(id, chain.head().seq) {
                return Ok(id.clone());
            }
            return Err(ClientError::Protocol(format!(
                "this device's key was revoked from {}; it cannot join again",
                self.did
            )));
        }
        if let Some(other) = status.did {
            return Err(ClientError::HasIdentity(other));
        }
        let store_url = status
            .store
            .map(|s| s.url)
            .ok_or_else(|| ClientError::Protocol("the executor has no identity store".into()))?;
        let flow = LinkFlow::open(
            self.executor,
            self.vault,
            &self.did,
            OfferKind::Device,
            Some(&status.device_key),
            &store_url,
            self.clock,
            self.random,
        )
        .await?;
        // The restored device holds the new master: the claim says it may keep it, so the log
        // lets it (`may_keep_master`): sent later rotations, a keeper, another copy.
        self.executor
            .claim_offer(&flow.offer_string(), Some(&this_device.name), true)
            .await?;
        let claimed = flow.wait_for_claim().await?;
        if claimed.device_key != status.device_key {
            return Err(ClientError::Protocol(
                "the claim is not this executor's device key".into(),
            ));
        }
        // The link's own record write cannot open the record yet (the newest version is not
        // wrapped to this device); the restore writes it next with the recovery key.
        let linked = flow.confirm(&claimed, this_device.clone(), false).await?;
        Ok(linked.key_id)
    }
}
