//! `revoke`: end device keys of the identity with a `revoke` signed by
//! the current master key, published through the executor. The revoked keys stop speaking
//! for the person from that event on. Earlier signatures stay valid only where they name the
//! seq they were made at: did:key links, and profiles whose `proof.record` anchors them. A
//! did:scid expression without an anchor is judged at the trusted head, so it turns invalid.
//!
//! A key that is lost or stolen, not just retired, gets a `since`: the time (ms) from which the
//! person no longer trusts it. A reader flags the key's links it first received at or after
//! `since`. The store takes a `since` up to the revoke's append time and no
//! earlier than the key's `add`. A key that is only retired is better `unlink`ed first
//! (`flows::unlink`): its older links stay valid.
//!
//! Then the private record is written again: the revoked devices leave it, with any pending
//! revocation of theirs, and the new version's record key is wrapped to the remaining devices
//! only, so a revoked device cannot read it.
//!
//! "Also replace my master key" ([`revoke_and_rotate`]): a `rotate` replaces the master key
//! first (it needs the 12 words), naming no key and never sending the new master to the keys
//! to revoke; then the `revoke` of those keys follows, signed by the new master, with the
//! person's `since`. Rotating first is what lets it recover a frozen identity: after a rival
//! event by the current master the store accepts only a `rotate`, so a revoke signed by the
//! current master would be refused. A device that may have held the master key is revoked
//! this way, so the key it held signs nothing more. A plain rotation ([`super::rotate`])
//! that drops keys only unlinks them: revoking is always this explicit action.

use ad4m_identity_core::build::revoke as revoke_event;
use ad4m_identity_core::{KelChain, KeyId};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::flows::master::transient;
use crate::flows::rotate::MAX_TRIES;
use crate::record;
use crate::request::{chain_of, scid_of};
use crate::vault::{MasterSigner, MasterUse, Vault};
use crate::{fold, ClientError, Clock, Random};

/// A revocation published.
#[derive(Debug)]
pub struct Revoked {
    pub head: SignedHead,
    /// The record write without the revoked devices. The revocation stands either way.
    pub record: Result<u64, ClientError>,
}

/// Revokes the device keys `key_ids` of the identity `did`. `since` (ms since the epoch) is the
/// time from which the keys are no longer trusted; `None` is the revoke's own append time.
pub async fn revoke<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_ids: &[KeyId],
    since: Option<u64>,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Revoked, ClientError> {
    let scid = scid_of(did)?;
    let chain = chain_of(executor, did).await?;
    check_keys(&chain, did, key_ids)?;
    let unlocked = vault.unlock(&scid, MasterUse::Revoke, clock.now())?;
    revoke_with(
        executor,
        vault,
        did,
        &chain,
        unlocked.signer(clock.now())?,
        key_ids,
        since,
        random,
    )
    .await
}

/// Revokes the device keys `key_ids` of `did`, after rotating to master *n*+1 from the 12
/// `words` ("Also replace my master key"). The `rotate` ([`super::rotate::rotate`]) comes first
/// and names no key; it never sends the new master to the keys to revoke; then the `revoke`
/// of those keys follows as its own event, signed by master *n*+1 (the words derive it, so no
/// vault unlock is needed), with the person's `since` (as [`revoke`]). Every call rotates: a
/// master replaced by an earlier call is not a reason to keep the current one, since a device
/// that was sent it may be the one to revoke.
///
/// Refused before anything is published when `key_ids` is empty or names no key of the log,
/// when the words do not derive the current master, when it names this device's own key, or
/// when `since` is later than now (the store would refuse it after the rotation). A key already
/// revoked is not revoked again; with none left it is only the rotation.
///
/// The rotation is public once published. A failure of the revoke after it is
/// [`ClientError::RotatedNotRevoked`]: the master key is replaced and the devices are not
/// yet revoked. It carries the [`PendingRevoke`] for [`finish_revoke`], which publishes only
/// the revoke (with a `since` the caller may correct); calling this again would rotate again.
/// The returned `head` is the revoke's and `record` the write after it, which drops the
/// revoked devices.
#[allow(clippy::too_many_arguments)]
pub async fn revoke_and_rotate<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_ids: &[KeyId],
    since: Option<u64>,
    words: &ad4m_identity_core::words::Words,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<super::rotate::Rotated, ClientError> {
    if key_ids.is_empty() {
        return Err(ClientError::Protocol("a revocation names a key".into()));
    }
    check_since(since, clock)?;
    let status = executor.status().await?;
    if status.did.as_deref() != Some(did) {
        return Err(ClientError::Protocol(format!(
            "the executor's device key is no device of {did}"
        )));
    }
    if status
        .key_id
        .as_ref()
        .is_some_and(|own| key_ids.contains(own))
    {
        return Err(ClientError::Protocol(
            "a device does not revoke its own key in a rotation it keeps".into(),
        ));
    }
    let chain = chain_of(executor, did).await?;
    check_keys(&chain, did, key_ids)?;
    // A key the log already revoked stays as it is (an unlinked one may still be revoked).
    let key_ids = still_valid(&chain, key_ids);
    // Rotate first, naming none of the keys, and never send them the new master: the revoke
    // follows, signed by it. A frozen log (a rival event by the current master) accepts only a
    // rotation, so a revoke signed by the current master would be refused. With nothing left
    // to revoke it is only the rotation.
    let rotated =
        super::rotate::rotate_excluding(executor, vault, did, words, &[], &key_ids, clock, random)
            .await?;
    if key_ids.is_empty() {
        return Ok(rotated);
    }
    let pending = PendingRevoke {
        index: rotated.index,
        key_ids,
    };
    let revoked =
        finish_revoke(executor, vault, did, words, &pending, since, clock, random).await?;
    Ok(super::rotate::Rotated {
        head: revoked.head,
        record: revoked.record,
        ..rotated
    })
}

/// The revoke a rotation left to publish ([`ClientError::RotatedNotRevoked`]): the index of the
/// master that rotation made current, which must sign it, and the keys to revoke.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PendingRevoke {
    pub index: u32,
    pub key_ids: Vec<KeyId>,
}

/// The resume of [`revoke_and_rotate`] after [`ClientError::RotatedNotRevoked`]: publishes only
/// the revoke of `pending`, signed by the master the 12 `words` derive, with `since` (ms; the
/// caller may correct the one the failed call used, as when the store refused it for being
/// before a key's add; `None` is the revoke's append time). No second rotation. Then the record
/// is written without the revoked devices, as in [`revoke_and_rotate`].
///
/// Refused before anything is published when `since` is later than now, when the words do not
/// derive the current master, and when that master is not `pending.index` (the log moved on:
/// start a new [`revoke_and_rotate`]). A key revoked meanwhile is left out; with none left the
/// head is answered. A failure of the revoke itself is [`ClientError::RotatedNotRevoked`]
/// again, with the same `pending`.
#[allow(clippy::too_many_arguments)]
pub async fn finish_revoke<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    words: &ad4m_identity_core::words::Words,
    pending: &PendingRevoke,
    since: Option<u64>,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Revoked, ClientError> {
    let seed = ad4m_identity_core::words::seed(words);
    let head = publish_pending(executor, did, &seed, pending, since, clock).await?;
    // The revoked devices leave the record, which is wrapped to the rest only.
    let record = record::write(executor, vault, did, &[], random).await;
    Ok(Revoked { head, record })
}

/// [`finish_revoke`]'s checks and publish, without the record write (restore's enrolment writes
/// it). `seed` is the words'.
pub(crate) async fn publish_pending<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    seed: &ad4m_identity_core::words::Seed,
    pending: &PendingRevoke,
    since: Option<u64>,
    clock: &dyn Clock,
) -> Result<SignedHead, ClientError> {
    check_since(since, clock)?;
    let chain = chain_of(executor, did).await?;
    let current = super::rotate::current_index(&chain, seed)?;
    if current != pending.index {
        return Err(ClientError::Protocol(format!(
            "master #{current} is current, not #{} that the rotation made; the log moved on: \
             start a new revoke and rotate",
            pending.index
        )));
    }
    publish_revoke(executor, did, seed, &pending.key_ids, since)
        .await
        .map_err(|e| ClientError::RotatedNotRevoked {
            pending: pending.clone(),
            cause: e.to_string(),
        })
}

/// The keys of `key_ids` that the log holds and has not revoked (an unlinked one may still be
/// revoked), each once.
pub(crate) fn still_valid(chain: &KelChain, key_ids: &[KeyId]) -> Vec<KeyId> {
    let state = chain.state();
    let mut keys: Vec<KeyId> = Vec::new();
    for id in key_ids {
        if state.keys.get(id).is_some_and(|k| k.revoked.is_none()) && !keys.contains(id) {
            keys.push(id.clone());
        }
    }
    keys
}

/// Refused when `since` (ms) is later than the clock's now: the store refuses it too, and a
/// rotation must not stand without its revoke for that.
pub(crate) fn check_since(since: Option<u64>, clock: &dyn Clock) -> Result<(), ClientError> {
    match since {
        Some(since) if since / 1000 > clock.now() => {
            Err(ClientError::Protocol("since is later than now".into()))
        }
        _ => Ok(()),
    }
}

/// The head the executor accepted for the log of `did`.
pub(crate) async fn chain_head<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<SignedHead, ClientError> {
    executor
        .log(did, 0, true)
        .await?
        .head
        .ok_or_else(|| ClientError::Protocol("the executor accepted no head for the log".into()))
}

/// Publishes the `revoke` of the keys of `key_ids` that are not revoked, signed by the master
/// the words `seed` derive as the log's current one (the new master, after a rotation). A
/// `409`, or an answer that never came, reads the log again and builds it on the new head, at
/// most [`MAX_TRIES`] times, then [`ClientError::LogBusy`]; keys revoked by then are left out,
/// and when none is left the head is answered. Answers the head after the revoke.
pub(crate) async fn publish_revoke<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    seed: &ad4m_identity_core::words::Seed,
    key_ids: &[KeyId],
    since: Option<u64>,
) -> Result<SignedHead, ClientError> {
    for _ in 0..MAX_TRIES {
        let chain = chain_of(executor, did).await?;
        let keys = still_valid(&chain, key_ids);
        if keys.is_empty() {
            return chain_head(executor, did).await;
        }
        let signer = super::rotate::master_of(seed, super::rotate::current_index(&chain, seed)?)?;
        let event = revoke_event(&chain, &signer, &keys, since)?;
        match executor.publish(std::slice::from_ref(&event)).await {
            Ok(head) => return Ok(head),
            Err(e) if e.code == 409 || transient(&ClientError::Rpc(e.clone())) => {}
            Err(e) => return Err(e.into()),
        }
    }
    Err(ClientError::LogBusy)
}

/// Every key of `key_ids` is a key of the log: checked before the unlock, so the person is not
/// asked for a revocation that cannot pass.
pub(crate) fn check_keys(
    chain: &KelChain,
    did: &str,
    key_ids: &[KeyId],
) -> Result<(), ClientError> {
    match key_ids
        .iter()
        .find(|id| !chain.state().keys.contains_key(*id))
    {
        Some(id) => Err(ClientError::Protocol(format!(
            "{id} is no device key of {did}"
        ))),
        None => Ok(()),
    }
}

/// [`revoke`] under an unlock the caller holds, once the caller checked `key_ids` against the
/// log. The record write after the publish reads the newest version with its receipt: a
/// version a revoked key wrote before the revocation still opens (the writer is checked at
/// the seq the store accepted it), so the write builds on it and keeps what it says.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn revoke_with<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    chain: &KelChain,
    signer: &dyn MasterSigner,
    key_ids: &[KeyId],
    since: Option<u64>,
    random: &dyn Random,
) -> Result<Revoked, ClientError> {
    let event = revoke_event(chain, signer, key_ids, since)?;
    let head = executor.publish(std::slice::from_ref(&event)).await?;
    let record = record::write(executor, vault, did, &[], random).await;
    Ok(Revoked { head, record })
}

/// The id of the device key `device_key` (a multikey) in the log of `did`, if the log lists it.
pub async fn key_id_of<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    device_key: &str,
) -> Result<Option<KeyId>, ClientError> {
    let chain = fold(&executor.events(did).await?)?;
    Ok(chain.state().key_id_of(&device_key.to_string()).cloned())
}
