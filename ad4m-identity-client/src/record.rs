//! The private details record (spec section 17), opened, merged and written in this one place.
//!
//! The record is sealed to the `enc` key of every device key of the log, so this app reads it
//! as its executor's device: the executor reads the sealed record with its device key
//! (`identity.record`, which answers it with the store's receipt) and unwraps its own entry's
//! record key in its wallet (`identity.unwrapRecordKey`); the record is decrypted here. A write
//! ([`write`]) reads the newest version, applies its [`Change`]s ([`merge`]), seals the result
//! as the next version to every reader of the log at its head, and writes it
//! (`identity.recordPut`), where the executor's device key signs the envelope. When another
//! writer got there first (the store answers a stale version) or the log's readers changed
//! under the write, it reads and merges again.
//!
//! A read checks the store's receipt before it trusts any field ([`decrypt_record`]): the
//! store key the client trusts for signed heads signed over this identity, version and
//! envelope, so the envelope's writer is checked at the seq the store accepted it — a record
//! whose writer was revoked after writing still opens — and the recovery key's attestation
//! against the log. The recovery key that counts is [`recovery_of`]. A device write copies that
//! recovery key and its attestation unchanged: only a master-signed write installs, replaces or
//! drops it. When the stored pair's attestation is stale — by a master a rotation replaced — a
//! device write is refused [`ClientError::RecoveryNeedsReattest`] rather than seal the record
//! without the recovery key (the store takes the stale pair as none, and the words could not
//! open it any more); a device holding the new master re-attests it with a master-signed write
//! ([`Reader::Master`]), as a rotation's rewrite does ([`Reader::Recovery`]). A store can
//! serve back an older version with a receipt that still verifies, so a
//! read also keeps each identity's highest version read in the vault
//! ([`Vault::record_seen`]) and refuses a lower one.
//!
//! A restore reads and writes the record before its device can: as [`Reader::Recovery`], the
//! words' recovery key opens the record's `"recovery"` entry, and the current master key signs
//! the envelope and the store requests (the store lets the current master key read and write
//! the record). A master-signed write is also what a rotation's record rewrite is, since it
//! re-attests the recovery key with the new master.

use ad4m_identity_core::record::{
    attest_recovery_key, decrypt_record, open_record, recovery_of, seal_record, seal_unsigned,
    DeviceEntry, HoldsMaster, PendingRevocation, PrivateRecord, RecordError, Recovery, VaultId,
    RECOVERY_ENTRY,
};
use ad4m_identity_core::{KelChain, KeyId, Multikey, X25519Secret};
use ad4m_ldk::identity::SignedRequest;

use crate::executor::{ExecutorRpc, RpcError, ViaExecutor};
use crate::request::{
    body_bytes, chain_of, master_at_head, master_request, store_of, MasterPosition,
};
use crate::vault::{MasterSigner, Vault};
use crate::{ClientError, Clock, Random, RandomRng};

/// Who opens and writes the record.
pub enum Reader<'a> {
    /// The executor's device: its own entry, unwrapped in its wallet; its device key signs the
    /// envelope and the store requests.
    Executor,
    /// The device as the current master key's holder: opens the record through the executor's
    /// own entry, as [`Reader::Executor`], but `master` signs the envelope and the store
    /// requests. Only a master-signed write installs, replaces or drops the recovery key, so
    /// this write re-attests the stored record's recovery key when its attestation is stale —
    /// by a master a rotation replaced — which is what a device write is refused for
    /// ([`ClientError::RecoveryNeedsReattest`]) until a holder signs again.
    Master {
        master: &'a dyn MasterSigner,
        clock: &'a dyn Clock,
    },
    /// A restore or a rotation's re-attestation: the words' recovery key opens the `"recovery"`
    /// entry (the executor's entry when the record has none), and `master` (the current master
    /// key) signs the envelope and the store requests. Only a master-signed write installs,
    /// replaces or drops the recovery key.
    Recovery {
        secret: &'a X25519Secret,
        master: &'a dyn MasterSigner,
        clock: &'a dyn Clock,
    },
}

/// Writes tried before [`ClientError::RecordBusy`].
pub(crate) const MAX_WRITES: usize = 4;

/// One change to the record. A write applies its changes to the newest version, so two
/// writers' changes to different entries both stay.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Change {
    /// A device the holder linked: its whole entry, replacing an older one.
    Device { key_id: KeyId, entry: DeviceEntry },
    /// A new name for a listed device.
    Rename { key_id: KeyId, name: String },
    /// Whether a listed device holds the master key. Clears the entry's `masterIndex` and
    /// `vault`: a device that keeps a master key says which, and where, with [`Change::Holds`].
    HoldsMaster { key_id: KeyId, holds: HoldsMaster },
    /// A listed device's vault `vault` keeps master `index`: `holdsMaster: true`,
    /// `masterIndex: index`, `vault`. Only the device itself (or the flow that put the key in
    /// its vault) writes it. Monotonic: an entry that already says `true` with a higher index on
    /// the same vault (another app of the device finished a later generation first) stands,
    /// since the vault never goes back one either.
    ///
    /// The vault keeps one key per identity, so every other entry that says `true` on the same
    /// vault with a lower index is carried to `index` too: no entry names a key that its vault
    /// replaced. They count as one copy all the same ([`crate::flows::master`]).
    Holds {
        key_id: KeyId,
        index: u32,
        vault: VaultId,
    },
    /// The vault `vault` forgot the identity's master key: every entry that names it is marked
    /// `holdsMaster: false`, without `masterIndex` and `vault`. Written by a removal through any
    /// device key on that vault, since all its entries named the one copy it forgot.
    VaultEmptied { vault: VaultId },
    /// Master `index` was put into a listed device's inbox: `holdsMaster: "offered"`, unless the
    /// entry already says the device holds `index` or a later master ([`Change::Holds`], written
    /// by the device when it took it), which stands. So a hand-over's write, built on whatever
    /// version is newest, never puts `"offered"` over a take that landed first.
    Offered { key_id: KeyId, index: u32 },
    /// A revocation no reachable master key could sign yet; one per key.
    PendingRevocation(PendingRevocation),
}

/// The record as this app opened it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Opened {
    pub version: u64,
    pub record: PrivateRecord,
    /// The recovery key that counts ([`recovery_of`]): attested by the log's current master.
    /// A device write copies it unchanged into the next version.
    pub recovery: Option<Recovery>,
    /// The recovery key the stored envelope names, counting or not. `Some` while `recovery`
    /// is `None` means the stored pair's attestation is stale — by a master a rotation
    /// replaced — so the record waits for the new master to re-attest it: a device write is
    /// refused [`ClientError::RecoveryNeedsReattest`], and a master-signed write attests this
    /// key again.
    pub recovery_key: Option<Multikey>,
}

/// Applies `changes` to `record`, then keeps only what the log at `chain`'s head still holds:
/// the entries of device keys valid there, and the pending revocations of keys not yet
/// revoked. A rename or a `holdsMaster` of a device the record does not list changes nothing:
/// its entry comes with its link.
fn merge(record: &mut PrivateRecord, chain: &KelChain, changes: &[Change]) {
    for change in changes {
        match change {
            Change::Device { key_id, entry } => {
                let mut entry = entry.clone();
                // A vault id goes only with a holding.
                if entry.holds_master != HoldsMaster::Yes {
                    entry.vault = None;
                }
                record.devices.insert(key_id.clone(), entry);
            }
            Change::Rename { key_id, name } => {
                if let Some(entry) = record.devices.get_mut(key_id) {
                    entry.name = name.clone();
                }
            }
            Change::HoldsMaster { key_id, holds } => {
                if let Some(entry) = record.devices.get_mut(key_id) {
                    entry.holds_master = *holds;
                    entry.master_index = None;
                    entry.vault = None;
                }
            }
            Change::Holds {
                key_id,
                index,
                vault,
            } => {
                let Some(entry) = record.devices.get_mut(key_id) else {
                    continue;
                };
                let newer = entry.holds_master == HoldsMaster::Yes
                    && entry.vault == Some(*vault)
                    && entry.master_index.is_some_and(|held| held > *index);
                if newer {
                    continue;
                }
                entry.holds_master = HoldsMaster::Yes;
                entry.master_index = Some(*index);
                entry.vault = Some(*vault);
                for entry in record.devices.values_mut() {
                    if entry.holds_master == HoldsMaster::Yes
                        && entry.vault == Some(*vault)
                        && entry.master_index.is_none_or(|held| held < *index)
                    {
                        entry.master_index = Some(*index);
                    }
                }
            }
            Change::VaultEmptied { vault } => {
                for entry in record.devices.values_mut() {
                    if entry.vault == Some(*vault) {
                        entry.holds_master = HoldsMaster::No;
                        entry.master_index = None;
                        entry.vault = None;
                    }
                }
            }
            Change::Offered { key_id, index } => {
                if let Some(entry) = record.devices.get_mut(key_id) {
                    let took_it = entry.holds_master == HoldsMaster::Yes
                        && entry.master_index.is_some_and(|held| held >= *index);
                    if !took_it {
                        entry.holds_master = HoldsMaster::Offered;
                        entry.master_index = None;
                        entry.vault = None;
                    }
                }
            }
            Change::PendingRevocation(pending) => {
                if !record
                    .pending_revocations
                    .iter()
                    .any(|p| p.key_id == pending.key_id)
                {
                    record.pending_revocations.push(pending.clone());
                }
            }
        }
    }
    let state = chain.state();
    let head = chain.head().seq;
    record
        .devices
        .retain(|id, _| state.keys.contains_key(id) && state.key_valid_at(id, head));
    record
        .pending_revocations
        .retain(|p| state.key_valid_at(&p.key_id, head));
}

/// The record of `did`, the executor's identity, opened through the executor's entry; `None`
/// before the first write.
pub async fn read<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
) -> Result<Option<Opened>, ClientError> {
    let chain = chain_of(executor, did).await?;
    Ok(read_at(executor, vault, did, &chain)
        .await?
        .map(|(opened, _)| opened))
}

/// [`read`] against the log `chain` of `did`, with the executor's device key id: the store's
/// receipt for the stored record is checked against the store key the client trusts for
/// signed heads (the one whose head the executor accepted, [`store_of`]), so the envelope's
/// writer is checked at the seq the store accepted it, and the version is kept as the
/// identity's high-water in `vault` — a record served back under it is refused.
async fn read_at<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    chain: &KelChain,
) -> Result<Option<(Opened, KeyId)>, ClientError> {
    let status = executor.status().await?;
    if status.did.as_deref() != Some(did) {
        return Err(ClientError::Protocol(format!(
            "the executor's device key is no device of {did}"
        )));
    }
    let key_id = status
        .key_id
        .ok_or_else(|| ClientError::Protocol("the executor's device key is in no log".into()))?;
    let Some(stored) = executor.record().await? else {
        // No record is version 0. A store that answers "missing" after this client read a
        // version has rolled the record back: refused like any lower version.
        if let Some(high) = vault.record_seen(chain.id(), 0)? {
            if high > 0 {
                return Err(ClientError::ServedRecordVersion {
                    served: 0,
                    seen: high,
                });
            }
        }
        return Ok(None);
    };
    let sealed = &stored.record;
    let record = open_as_device(executor, did, sealed, &stored.receipt, chain).await?;
    let opened = opened_of(vault, chain, sealed, record)?;
    Ok(Some((opened, key_id)))
}

/// `sealed` opened through the executor's device entry: its `wrapped[key_id]` unwrapped in
/// the executor's wallet, the receipt checked before the record is trusted.
async fn open_as_device<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    sealed: &ad4m_identity_core::record::SealedRecord,
    receipt: &ad4m_identity_core::record::RecordReceipt,
    chain: &KelChain,
) -> Result<PrivateRecord, ClientError> {
    let status = executor.status().await?;
    if status.did.as_deref() != Some(did) {
        return Err(ClientError::Protocol(format!(
            "the executor's device key is no device of {did}"
        )));
    }
    let key_id = status
        .key_id
        .ok_or_else(|| ClientError::Protocol("the executor's device key is in no log".into()))?;
    let wrapped = sealed
        .wrapped
        .get(&key_id)
        .ok_or_else(|| RecordError::NoEntry(key_id.clone()))?;
    let key = executor.unwrap_record_key(wrapped).await?;
    let stores = trusted_stores(store_of(executor, did).await?);
    Ok(decrypt_record(
        sealed,
        receipt,
        chain,
        &stores,
        key.expose(),
    )?)
}

/// `record`, opened from `sealed`: its version against the identity's high-water in `vault`
/// (a record served back under it is refused), and the recovery key that counts at `chain`'s
/// head.
fn opened_of(
    vault: &dyn Vault,
    chain: &KelChain,
    sealed: &ad4m_identity_core::record::SealedRecord,
    record: PrivateRecord,
) -> Result<Opened, ClientError> {
    if let Some(high) = vault.record_seen(chain.id(), sealed.version)? {
        if sealed.version < high {
            return Err(ClientError::ServedRecordVersion {
                served: sealed.version,
                seen: high,
            });
        }
    }
    Ok(Opened {
        version: sealed.version,
        record,
        recovery: recovery_of(sealed, chain),
        recovery_key: sealed.recovery_key.clone(),
    })
}

/// A store request of the current master key `master` at `chain`'s head for `method` on the
/// record of `did`.
#[allow(clippy::too_many_arguments)]
async fn master_record_request<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    chain: &KelChain,
    master: &dyn MasterSigner,
    method: &str,
    body: &[u8],
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<SignedRequest, ClientError> {
    let store = store_of(executor, did).await?;
    let by = master_at_head(chain, master)?;
    let at = MasterPosition {
        did,
        store: &store,
        by: &by,
    };
    let path = format!("/v1/private/{}", chain.id());
    master_request(&at, master, method, &path, body, random, clock.now())
}

/// The record of `did` as `reader` opens it, at `chain`'s head; `None` before the first write.
/// As [`Reader::Recovery`] the read itself is a store request `master` signs
/// (`identity.record` with `request`), and the `"recovery"` entry opens it — a device the log
/// does not list yet reads the record all the same.
pub async fn read_as<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    chain: &KelChain,
    reader: &Reader<'_>,
    random: &dyn Random,
) -> Result<Option<Opened>, ClientError> {
    let Reader::Recovery {
        secret,
        master,
        clock,
    } = reader
    else {
        return Ok(read_at(executor, vault, did, chain)
            .await?
            .map(|(opened, _)| opened));
    };
    let request =
        master_record_request(executor, did, chain, *master, "GET", b"", *clock, random).await?;
    let Some(stored) = executor.record_with(&request).await? else {
        // No record is version 0, refused after a seen version as `read_at` refuses it.
        if let Some(high) = vault.record_seen(chain.id(), 0)? {
            if high > 0 {
                return Err(ClientError::ServedRecordVersion {
                    served: 0,
                    seen: high,
                });
            }
        }
        return Ok(None);
    };
    let sealed = &stored.record;
    let record = if sealed.wrapped.contains_key(RECOVERY_ENTRY) {
        let stores = trusted_stores(store_of(executor, did).await?);
        open_record(
            sealed,
            &stored.receipt,
            chain,
            &stores,
            RECOVERY_ENTRY,
            secret,
        )?
    } else {
        // Written before the recovery key was set: only a device of the log opens it.
        match open_as_device(executor, did, sealed, &stored.receipt, chain).await {
            Ok(record) => record,
            Err(_) => return Err(RecordError::NoEntry(RECOVERY_ENTRY.into()).into()),
        }
    };
    opened_of(vault, chain, sealed, record).map(Some)
}

/// The store keys a receipt verifies against: the multikeys of the `did:key`s of the stores
/// whose heads the executor accepted.
fn trusted_stores(store: String) -> Vec<Multikey> {
    vec![store.strip_prefix("did:key:").unwrap_or(&store).to_string()]
}

/// The store refused the write because the record or the log moved under it: a stale version
/// (`409 {version}`, which the agent language passes as `invalid: version`), or readers that
/// are no longer the log's (`invalid: wrapped`).
fn moved(e: &RpcError) -> bool {
    e.code == 409
        || (e.code == 400
            && (e.message.starts_with("invalid: version")
                || e.message.starts_with("invalid: wrapped")))
}

/// Writes `changes` into the record of `did`, the executor's identity, as its next version;
/// answers the version written. A stale version or a changed log reads, merges and writes
/// again, at most [`MAX_WRITES`] times. Refused with [`ClientError::RecoveryNeedsReattest`]
/// while the stored record's recovery pair is stale: a device write would seal the record
/// without it, so the record waits for the new master to re-attest it.
pub async fn write<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    changes: &[Change],
    random: &dyn Random,
) -> Result<u64, ClientError> {
    write_as(
        executor,
        vault,
        did,
        &Reader::Executor,
        changes,
        None,
        random,
    )
    .await
}

/// [`write`] as `reader`; with `recovery_key` (the words' recovery key, as
/// `x25519_multikey_of`) a master-signed write ([`Reader::Recovery`], or [`Reader::Master`]
/// with the key the stored record names) gives a record without
/// one the key, attested by `reader`'s master ([`attest_recovery_key`]): only a master-signed
/// write installs or replaces it, so a rotation's rewrite re-attests it with the new master.
/// A device write copies the record's own recovery key and its attestation unchanged, or
/// carries none when it has none — and is refused [`ClientError::RecoveryNeedsReattest`]
/// while the stored pair is stale, since it would seal the record without the key.
pub async fn write_as<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    reader: &Reader<'_>,
    changes: &[Change],
    recovery_key: Option<&Multikey>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    write_checked(
        executor,
        vault,
        did,
        reader,
        changes,
        recovery_key,
        &|_, _| Ok(()),
        random,
    )
    .await
}

/// A condition a write holds to: it sees the version the write builds on (`None` before the
/// first write) and the log at the head, and refuses with the error the write answers.
pub type Precondition<'a> =
    &'a dyn Fn(Option<&PrivateRecord>, &KelChain) -> Result<(), ClientError>;

/// [`write`] only while `check` holds on the version it builds on. The check runs again on
/// each read after a stale version, and the store takes only the stored version plus one, so a
/// version written was built on a version that passed: a change that must not land over some
/// state (an `"offered"` over a device that took the master key) never does, however late it
/// comes. A device write, it is refused [`ClientError::RecoveryNeedsReattest`] while the
/// stored record's recovery pair is stale.
pub async fn write_if<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    changes: &[Change],
    check: Precondition<'_>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    write_checked(
        executor,
        vault,
        did,
        &Reader::Executor,
        changes,
        None,
        check,
        random,
    )
    .await
}

/// [`write_if`] as `reader`: a device write, or a master-signed one ([`Reader::Master`],
/// [`Reader::Recovery`]) that re-attests a stale recovery pair as it writes (with
/// `recovery_key`, as [`write_as`]). `check` runs on every version the write builds on, so a
/// condition it reads from outside the record (this device's custody note in the vault) is
/// checked after that version was read: a change made outside before the record moved on is
/// seen, and one made after it can only be followed by a newer version.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn write_as_if<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    reader: &Reader<'_>,
    changes: &[Change],
    recovery_key: Option<&Multikey>,
    check: Precondition<'_>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    write_checked(
        executor,
        vault,
        did,
        reader,
        changes,
        recovery_key,
        check,
        random,
    )
    .await
}

#[allow(clippy::too_many_arguments)]
async fn write_checked<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    reader: &Reader<'_>,
    changes: &[Change],
    recovery_key: Option<&Multikey>,
    check: Precondition<'_>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    for _ in 0..MAX_WRITES {
        let chain = chain_of(executor, did).await?;
        let (opened, device_key) = match reader {
            Reader::Executor | Reader::Master { .. } => {
                match read_at(executor, vault, did, &chain).await? {
                    Some((opened, key_id)) => (Some(opened), Some(key_id)),
                    None => (None, None),
                }
            }
            Reader::Recovery { .. } => (
                read_as(executor, vault, did, &chain, reader, random).await?,
                None,
            ),
        };
        check(opened.as_ref().map(|o| &o.record), &chain)?;
        let (version, mut record, carried, stored_key) = match opened {
            Some(opened) => (
                opened.version + 1,
                opened.record,
                opened.recovery,
                opened.recovery_key,
            ),
            None => (1, PrivateRecord::default(), None, None),
        };
        merge(&mut record, &chain, changes);
        // A `holdsMaster` (or a master held or offered) for a device the record does not list
        // would be a silent no-op that its caller reports as done: refuse it instead.
        if let Some(missing) = changes.iter().find_map(|change| match change {
            Change::HoldsMaster { key_id, .. }
            | Change::Holds { key_id, .. }
            | Change::Offered { key_id, .. }
                if !record.devices.contains_key(key_id) =>
            {
                Some(key_id.clone())
            }
            _ => None,
        }) {
            return Err(ClientError::NotListed(missing));
        }
        let put = match reader {
            Reader::Executor => {
                // The stored record's recovery pair went stale with a rotation (its
                // attestation is not the current master's, so `carried` is `None`): a device
                // write would seal the record without the recovery key and the store would
                // take it, leaving the words unable to open the record. Refuse instead: the
                // record waits for the new master to re-attest the key.
                if stored_key.is_some() && carried.is_none() {
                    return Err(ClientError::RecoveryNeedsReattest);
                }
                // The executor's device key signs it (`identity.recordPut` with an empty
                // `sig`); a device write carries the still attested recovery key unchanged,
                // or none.
                let key_id = match device_key {
                    Some(key_id) => key_id,
                    None => executor.status().await?.key_id.ok_or_else(|| {
                        ClientError::Protocol("the executor's device key is in no log".into())
                    })?,
                };
                let sealed = seal_unsigned(
                    &chain,
                    version,
                    &record,
                    carried,
                    key_id,
                    &mut RandomRng(random),
                )?;
                executor.record_put(&sealed).await
            }
            Reader::Master { master, clock } | Reader::Recovery { master, clock, .. } => {
                // The master signs the envelope itself, so the write may install the
                // recovery key: kept unchanged when it still counts, else `recovery_key` —
                // or the stale key the stored record names — attested by `master` (a
                // rotation re-attests it with the new master).
                let recovery = match carried {
                    Some(kept) => Some(kept),
                    None => recovery_key
                        .or(stored_key.as_ref())
                        .map(|key| attest_recovery_key(&chain, key.clone(), *master))
                        .transpose()?,
                };
                let sealed = seal_record(
                    &chain,
                    version,
                    &record,
                    recovery,
                    *master,
                    &mut RandomRng(random),
                )?;
                let request = master_record_request(
                    executor,
                    did,
                    &chain,
                    *master,
                    "PUT",
                    &body_bytes(&sealed)?,
                    *clock,
                    random,
                )
                .await?;
                executor.record_put_with(&sealed, &request).await
            }
        };
        match put {
            Ok(version) => {
                // A version this client wrote is one it read, for the high-water: a store
                // serving back an older one trips on it too.
                vault.record_seen(chain.id(), version)?;
                return Ok(version);
            }
            Err(e) if moved(&e) => continue,
            Err(e) => return Err(e.into()),
        }
    }
    Err(ClientError::RecordBusy)
}
