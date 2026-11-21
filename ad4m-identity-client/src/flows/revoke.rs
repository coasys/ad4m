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
//! "Also replace my master key" ([`revoke_and_rotate`]): the keys are revoked here as above
//! (with the person's `since`), then a `rotate` replaces the master key, which needs the 12
//! words. A device that may have held the master key is revoked this way, so the key it held
//! signs nothing more. A plain rotation ([`super::rotate`]) that drops keys only unlinks them:
//! revoking is always this explicit action.

use ad4m_identity_core::build::revoke as revoke_event;
use ad4m_identity_core::{KelChain, KeyId};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
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

/// Revokes the device keys `key_ids` of `did`, then rotates to master *n*+1 from the 12
/// `words` ("Also replace my master key"). The revoke is its own event, with the person's
/// `since` (as [`revoke`]); the `rotate` that follows ([`super::rotate::rotate`]) names no
/// key, so the keys are revoked, never merely unlinked. The words sign the revoke as master
/// *n* (they derive it, and must derive the current master), so no vault unlock is needed.
///
/// Refused before anything is published when `key_ids` is empty or names no key of the log,
/// when the words do not derive the current master, or when it names this device's own key.
/// A key already revoked is not revoked again. The revoke is public once published: a failure
/// of the rotation after it is returned as an error, and the keys stay revoked (a retry
/// rotates only). The private record is written again by the rotation, without the revoked
/// devices.
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
    let seed = ad4m_identity_core::words::seed(words);
    let index = super::rotate::current_index(&chain, &seed)?;
    let signer = super::rotate::master_of(&seed, index)?;
    // A key the log already revoked stays as it is (an unlinked one may still be revoked).
    let state = chain.state();
    let mut to_revoke: Vec<KeyId> = Vec::new();
    for id in key_ids {
        if state.keys[id].revoked.is_none() && !to_revoke.contains(id) {
            to_revoke.push(id.clone());
        }
    }
    if !to_revoke.is_empty() {
        let event = revoke_event(&chain, &signer, &to_revoke, since)?;
        executor.publish(std::slice::from_ref(&event)).await?;
    }
    super::rotate::rotate(executor, vault, did, words, &[], clock, random).await
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
