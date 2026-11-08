//! The private details record (spec section 17), opened, merged and written in this one place.
//!
//! The record is sealed to the `enc` key of every device key of the log, so this app reads it
//! as its executor's device: the executor reads the sealed record with its device key
//! (`identity.record`) and unwraps its own entry's record key in its wallet
//! (`identity.unwrapRecordKey`); the record is decrypted here. A write ([`write`]) reads the
//! newest version, applies its [`Change`]s ([`merge`]), seals the result as the next version to
//! every reader of the log at its head, and writes it (`identity.recordPut`), where the
//! executor's device key signs the envelope. When another writer got there first (the store
//! answers a stale version) or the log's readers changed under the write, it reads and merges
//! again.
//!
//! A read verifies the envelope's signature and the recovery key's attestation against the log
//! before it trusts any field ([`decrypt_record`]); the recovery key that counts is
//! [`recovery_of`]. A device write copies that recovery key and its attestation unchanged: only
//! a master-signed write installs, replaces or drops it.

use ad4m_identity_core::record::{
    decrypt_record, recovery_of, seal_unsigned, DeviceEntry, HoldsMaster, PendingRevocation,
    PrivateRecord, RecordError, Recovery,
};
use ad4m_identity_core::{KelChain, KeyId};

use crate::executor::{ExecutorRpc, RpcError, ViaExecutor};
use crate::request::chain_of;
use crate::{ClientError, Random, RandomRng};

/// Writes tried before [`ClientError::RecordBusy`].
const MAX_WRITES: usize = 4;

/// One change to the record. A write applies its changes to the newest version, so two
/// writers' changes to different entries both stay.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Change {
    /// A device the holder linked: its whole entry, replacing an older one.
    Device { key_id: KeyId, entry: DeviceEntry },
    /// A new name for a listed device.
    Rename { key_id: KeyId, name: String },
    /// Whether a listed device holds the master key.
    HoldsMaster { key_id: KeyId, holds: HoldsMaster },
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
}

/// Applies `changes` to `record`, then keeps only what the log at `chain`'s head still holds:
/// the entries of device keys valid there, and the pending revocations of keys not yet
/// revoked. A rename or a `holdsMaster` of a device the record does not list changes nothing:
/// its entry comes with its link.
fn merge(record: &mut PrivateRecord, chain: &KelChain, changes: &[Change]) {
    for change in changes {
        match change {
            Change::Device { key_id, entry } => {
                record.devices.insert(key_id.clone(), entry.clone());
            }
            Change::Rename { key_id, name } => {
                if let Some(entry) = record.devices.get_mut(key_id) {
                    entry.name = name.clone();
                }
            }
            Change::HoldsMaster { key_id, holds } => {
                if let Some(entry) = record.devices.get_mut(key_id) {
                    entry.holds_master = *holds;
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
    did: &str,
) -> Result<Option<Opened>, ClientError> {
    let chain = chain_of(executor, did).await?;
    Ok(read_at(executor, did, &chain)
        .await?
        .map(|(opened, _)| opened))
}

/// [`read`] against the log `chain` of `did`, with the executor's device key id.
async fn read_at<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
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
    let Some(sealed) = executor.record().await? else {
        return Ok(None);
    };
    let wrapped = sealed
        .wrapped
        .get(&key_id)
        .ok_or_else(|| RecordError::NoEntry(key_id.clone()))?;
    let key = executor.unwrap_record_key(wrapped).await?;
    let record = decrypt_record(&sealed, chain, key.expose())?;
    let opened = Opened {
        version: sealed.version,
        record,
        recovery: recovery_of(&sealed, chain),
    };
    Ok(Some((opened, key_id)))
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
/// again, at most [`MAX_WRITES`] times.
pub async fn write<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    changes: &[Change],
    random: &dyn Random,
) -> Result<u64, ClientError> {
    write_if(executor, did, changes, &|_, _| Ok(()), random).await
}

/// A condition a write holds to: it sees the version the write builds on (`None` before the
/// first write) and the log at the head, and refuses with the error the write answers.
pub type Precondition<'a> =
    &'a dyn Fn(Option<&PrivateRecord>, &KelChain) -> Result<(), ClientError>;

/// [`write`] only while `check` holds on the version it builds on. The check runs again on
/// each read after a stale version, and the store takes only the stored version plus one, so a
/// version written was built on a version that passed: a change that must not land over some
/// state (an `"offered"` over a device that took the master key) never does, however late it
/// comes.
pub async fn write_if<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    changes: &[Change],
    check: Precondition<'_>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    for _ in 0..MAX_WRITES {
        let chain = chain_of(executor, did).await?;
        let opened = read_at(executor, did, &chain).await?;
        check(opened.as_ref().map(|(o, _)| &o.record), &chain)?;
        let (version, mut record, recovery, key_id) = match opened {
            Some((opened, key_id)) => (opened.version + 1, opened.record, opened.recovery, key_id),
            None => {
                let key_id = executor.status().await?.key_id.ok_or_else(|| {
                    ClientError::Protocol("the executor's device key is in no log".into())
                })?;
                (1, PrivateRecord::default(), None, key_id)
            }
        };
        merge(&mut record, &chain, changes);
        // The executor's device key signs it (`identity.recordPut` with an empty `sig`).
        let sealed = seal_unsigned(
            &chain,
            version,
            &record,
            recovery,
            key_id,
            &mut RandomRng(random),
        )?;
        match executor.record_put(&sealed).await {
            Ok(version) => return Ok(version),
            Err(e) if moved(&e) => continue,
            Err(e) => return Err(e.into()),
        }
    }
    Err(ClientError::RecordBusy)
}
