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
/// log. The record is read before the publish: from the revocation on, a reader refuses a
/// version a revoked key signed, so when the newest version is one, the write builds on the
/// version read here (`record::AfterRevoke`).
pub(crate) async fn revoke_with<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    chain: &KelChain,
    signer: &dyn MasterSigner,
    key_ids: &[KeyId],
    random: &dyn Random,
) -> Result<Revoked, ClientError> {
    let event = revoke_event(chain, signer, key_ids)?;
    let before = record::read(executor, did).await;
    let head = executor.publish(std::slice::from_ref(&event)).await?;
    let record = match before {
        Ok(before) => {
            let after = record::AfterRevoke {
                revoked: key_ids,
                before: before.as_ref().map(|o| &o.record),
            };
            record::write_after_revoke(executor, did, &after, random).await
        }
        Err(e) => Err(e),
    };
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
