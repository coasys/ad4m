//! The private details record (spec section 17), opened, merged and written in this one place.
//!
//! The record is sealed to the `enc` key of every device key of the log, so this app reads it
//! as its executor's device: the executor reads the sealed record with its device key
//! (`identity.record`) and unwraps its own entry's record key in its wallet
//! (`identity.unwrapRecordKey`); the record is decrypted here. A write ([`write`]) reads the
//! newest version, applies its [`Change`]s ([`merge`]), seals the result as the next version to
//! every reader of the log at its head, and writes it (`identity.recordPut`). When another
//! writer got there first (the store answers a stale version) or the log's readers changed
//! under the write, it reads and merges again.
//!
//! A restore reads and writes the record before its device can: as [`Reader::Recovery`], the
//! words' recovery key opens the record's `"recovery"` entry, and the current master key signs
//! the store requests (the store lets the current master key read and write the record).

use ad4m_identity_core::record::{
    decrypt_record, seal_record, DeviceEntry, HoldsMaster, PendingRevocation, PrivateRecord,
    RecordError,
};
use ad4m_identity_core::record::{open_record, SealedRecord, RECOVERY_ENTRY};
use ad4m_identity_core::{KelChain, KeyId, Multikey, X25519Secret};
use ad4m_ldk::identity::SignedRequest;

use crate::executor::{ExecutorRpc, RpcError, ViaExecutor};
use crate::request::{
    body_bytes, chain_of, master_at_head, master_request, scid_of, store_of, MasterPosition,
};
use crate::vault::MasterSigner;
use crate::{ClientError, Clock, Random, RandomRng};

/// Who opens and writes the record.
pub enum Reader<'a> {
    /// The executor's device: its own entry, unwrapped in its wallet; its device key signs the
    /// store requests.
    Executor,
    /// A restore: the words' recovery key opens the `"recovery"` entry, and `master` (the
    /// current master key) signs the store requests. A record without that entry is opened
    /// through the executor's entry when it has one.
    Recovery {
        secret: &'a X25519Secret,
        master: &'a dyn MasterSigner,
        clock: &'a dyn Clock,
    },
}

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
    /// Copied unchanged into every later version.
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
    let Some(sealed) = sealed_for_device(executor, did).await? else {
        return Ok(None);
    };
    open_as_device(executor, did, &sealed).await.map(Some)
}

/// The sealed record of `did`, the executor's identity, read with its device key.
async fn sealed_for_device<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<Option<SealedRecord>, ClientError> {
    let status = executor.status().await?;
    if status.did.as_deref() != Some(did) {
        return Err(ClientError::Protocol(format!(
            "the executor's device key is no device of {did}"
        )));
    }
    Ok(executor.record().await?)
}

/// `sealed` opened through the executor's entry.
async fn open_as_device<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    sealed: &SealedRecord,
) -> Result<Opened, ClientError> {
    let scid = scid_of(did)?;
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
    let record = decrypt_record(sealed, &scid, key.expose())?;
    Ok(Opened {
        version: sealed.version,
        record,
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
pub async fn read_as<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
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
        return read(executor, did).await;
    };
    let request =
        master_record_request(executor, did, chain, *master, "GET", b"", *clock, random).await?;
    let Some(sealed) = executor.record_with(&request).await? else {
        return Ok(None);
    };
    if !sealed.wrapped.contains_key(RECOVERY_ENTRY) {
        // Written before the recovery key was set: only a device of the log opens it.
        return match open_as_device(executor, did, &sealed).await {
            Ok(opened) => Ok(Some(opened)),
            Err(_) => Err(RecordError::NoEntry(RECOVERY_ENTRY.into()).into()),
        };
    }
    let record = open_record(&sealed, chain.id(), RECOVERY_ENTRY, secret)?;
    Ok(Some(Opened {
        version: sealed.version,
        record,
        recovery_key: sealed.recovery_key,
    }))
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
    write_as(executor, did, &Reader::Executor, changes, None, random).await
}

/// [`write`] as `reader`; with `recovery_key` (the words' recovery key, as
/// `x25519_multikey_of`) a record without one gets it. A record keeps the recovery key it has:
/// the store refuses a change.
pub async fn write_as<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    reader: &Reader<'_>,
    changes: &[Change],
    recovery_key: Option<&Multikey>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    for _ in 0..MAX_WRITES {
        let chain = chain_of(executor, did).await?;
        let (version, mut record, kept) =
            match read_as(executor, did, &chain, reader, random).await? {
                Some(opened) => (opened.version + 1, opened.record, opened.recovery_key),
                None => (1, PrivateRecord::default(), None),
            };
        let recovery_key = kept.or_else(|| recovery_key.cloned());
        merge(&mut record, &chain, changes);
        let sealed = seal_record(
            &chain,
            version,
            &record,
            recovery_key,
            &mut RandomRng(random),
        )?;
        let put = match reader {
            Reader::Executor => executor.record_put(&sealed).await,
            Reader::Recovery { master, clock, .. } => {
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
            Ok(version) => return Ok(version),
            Err(e) if moved(&e) => continue,
            Err(e) => return Err(e.into()),
        }
    }
    Err(ClientError::RecordBusy)
}
