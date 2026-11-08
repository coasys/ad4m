//! `revoke`: end device keys of the identity with a `revoke` signed by
//! the current master key, published through the executor. The revoked keys stop speaking
//! for the person from that event on. Earlier signatures stay valid only where they name the
//! seq they were made at: did:key links, and profiles whose `proof.record` anchors them. A
//! did:scid expression without an anchor is judged at the trusted head, so it turns invalid.
//!
//! Then the private record is written again: the revoked devices leave it, with any pending
//! revocation of theirs, and the new version's record key is wrapped to the remaining devices
//! only, so a revoked device cannot read it.

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

/// Revokes the device keys `key_ids` of the identity `did`.
pub async fn revoke<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_ids: &[KeyId],
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
        random,
    )
    .await
}

/// Every key of `key_ids` is a key of the log: checked before the unlock, so the person is not
/// asked for a revocation that cannot pass.
fn check_keys(chain: &KelChain, did: &str, key_ids: &[KeyId]) -> Result<(), ClientError> {
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
    random: &dyn Random,
) -> Result<Revoked, ClientError> {
    let event = revoke_event(chain, signer, key_ids)?;
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
