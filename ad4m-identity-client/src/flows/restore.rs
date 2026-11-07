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
//! 3. [`RestoreFlow::finish`]: one `rotate` to a fresh master *n*+1 revokes the ticked devices
//!    ([`super::rotate`], with its rebuild after a `409`), and this device's vault keeps it.
//!    Then this device is linked as a device: an offer bound to this executor's device key,
//!    claimed by this executor and spent by an `add` signed by master *n*+1. A device whose key
//!    the log already lists is not linked again.
//! 4. Master *n*+1 goes to every app that kept the master key and was not locked out, and the
//!    record is written with the recovery key: this device listed as holding the master key.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::record::{DeviceEntry, DeviceKind, PrivateRecord};
use ad4m_identity_core::words::{self, Seed};
use ad4m_identity_core::{KelChain, KeyId};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::flows::link::LinkFlow;
use crate::flows::rotate::{
    current_index, hand_over, keep, master_of, publish_rotation, recovery_key_of, ThisDevice,
};
use crate::record::{self, Reader};
use crate::request::{chain_of, scid_of};
use crate::vault::{MasterSigner, Vault};
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

/// A restore done.
#[derive(Debug)]
pub struct Restored {
    pub did: String,
    /// The head after the rotation.
    pub head: SignedHead,
    /// The new master's index.
    pub index: u32,
    /// The device keys the rotation revoked.
    pub revoked: Vec<KeyId>,
    /// Rotations built (more than 1 when another event landed first).
    pub tries: usize,
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
            match record::read_as(executor, &owner.did, &chain, &reader, random).await {
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

    /// Rotates to a fresh master on this device, revoking `lock_out` (key ids of
    /// [`RestoreFlow::devices`]) in the same event, links this device as `this_device` when the
    /// log does not list it, sends the new master to the apps that kept the old one, and writes
    /// the record.
    pub async fn finish(
        self,
        lock_out: &[KeyId],
        this_device: DeviceEntry,
    ) -> Result<Restored, ClientError> {
        if let Some(id) = lock_out
            .iter()
            .find(|id| !self.devices.iter().any(|d| &d.key_id == *id))
        {
            return Err(ClientError::Protocol(format!(
                "{id} is not a device listed for the lock-out"
            )));
        }
        let scid = scid_of(&self.did)?;
        let published = publish_rotation(self.executor, &self.did, &self.seed, lock_out).await?;
        let index = published.signer.index();
        keep(self.vault, &scid, &self.seed, index)?;
        let key_id = self.link_this(&this_device).await?;
        let secret = words::recovery_secret(&self.seed);
        let reader = Reader::Recovery {
            secret: &secret,
            master: &published.signer,
            clock: self.clock,
        };
        let (pushes, record) = hand_over(
            self.executor,
            self.vault,
            &self.did,
            &published.signer,
            &reader,
            Some(ThisDevice {
                key_id: key_id.clone(),
                entry: Some(this_device),
            }),
            &recovery_key_of(&self.seed),
            self.clock,
            self.random,
        )
        .await;
        Ok(Restored {
            did: self.did.clone(),
            head: published.head,
            index,
            revoked: published.revoked,
            tries: published.tries,
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
        self.executor
            .claim_offer(&flow.offer_string(), Some(&this_device.name), false)
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
