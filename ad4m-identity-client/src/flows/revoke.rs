//! `revoke` (AR-02 4.12, slice 1): end device keys of the identity with a `revoke` signed by
//! the current master key, published through the executor. The revoked keys stop speaking
//! for the person from that event on; their earlier signatures keep their verdicts.

use ad4m_identity_core::build::revoke as revoke_event;
use ad4m_identity_core::KeyId;
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::vault::{MasterUse, Vault};
use crate::{fold, ClientError, Clock};

/// Revokes the device keys `key_ids` of the identity `did`.
pub async fn revoke<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_ids: &[KeyId],
    clock: &dyn Clock,
) -> Result<SignedHead, ClientError> {
    let scid = ad4m_identity_core::scid_of(did)
        .ok_or_else(|| ClientError::Protocol(format!("{did} is not a did:scid:coasys:1")))?
        .to_string();
    let chain = fold(&executor.events(did).await?)?;
    for id in key_ids {
        if !chain.state().keys.contains_key(id) {
            return Err(ClientError::Protocol(format!(
                "{id} is no device key of {did}"
            )));
        }
    }
    let unlocked = vault.unlock(&scid, MasterUse::Revoke, clock.now())?;
    let event = revoke_event(&chain, unlocked.signer(clock.now())?, key_ids)?;
    Ok(executor.publish(std::slice::from_ref(&event)).await?)
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
