//! `unlink`: a key signs nothing new from the event on, and what it signed before stays valid.
//! The person's way to retire a device or a node (a stolen or lost key gets a `revoke` with a
//! `since`, `flows::revoke`).
//!
//! - [`unlink_self`]: the device unlinks itself, signed with its own key, no master key. The
//!   caller must destroy that key at once (`SelfUnlinked::destroy_key`): a copy of an unlinked
//!   key still signs a link with an old anchor, which only a `revoke` ends.
//! - [`unlink_key`]: the master key unlinks another key (opens the vault once). Then the
//!   private record is written again, as after a `revoke`: the unlinked device leaves it, and
//!   the new version's record key is wrapped only to the devices that remain.

use ad4m_identity_core::build::{unlink as unlink_event, unlink_self as unlink_self_event, Signer};
use ad4m_identity_core::KeyId;
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::record;
use crate::vault::{MasterUse, Vault};
use crate::{fold, ClientError, Clock, Random};

/// A device that unlinked itself. The key is spent: the caller destroys it now.
#[derive(Clone, Debug, PartialEq)]
#[must_use = "destroy the unlinked device key"]
pub struct SelfUnlinked {
    /// The store's head after the `unlink`.
    pub head: SignedHead,
    /// The id of the key to destroy; the signer passed to [`unlink_self`] is that key.
    pub destroy_key: KeyId,
}

/// A key the master key unlinked.
#[derive(Debug)]
pub struct Unlinked {
    pub head: SignedHead,
    /// The record write without the unlinked device. The unlink stands either way.
    pub record: Result<u64, ClientError>,
}

async fn scid_and_chain<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<(String, ad4m_identity_core::KelChain), ClientError> {
    let scid = ad4m_identity_core::scid_of(did)
        .ok_or_else(|| ClientError::Protocol(format!("{did} is not a did:scid:coasys:1")))?
        .to_string();
    Ok((scid, fold(&executor.events(did).await?)?))
}

/// The device key `signer` unlinks itself from the identity `did`, as `key_id`. Publishes the
/// `unlink` signed by that key. The caller then destroys the key ([`SelfUnlinked::destroy_key`]).
pub async fn unlink_self<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    key_id: &KeyId,
    signer: &dyn Signer,
) -> Result<SelfUnlinked, ClientError> {
    let (_, chain) = scid_and_chain(executor, did).await?;
    let event = unlink_self_event(&chain, key_id, signer)?;
    let head = executor.publish(std::slice::from_ref(&event), None).await?;
    Ok(SelfUnlinked {
        head,
        destroy_key: key_id.clone(),
    })
}

/// Unlinks the device key `key_id` of the identity `did` with an `unlink` signed by the current
/// master key, then writes the private record again without it.
pub async fn unlink_key<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_id: &KeyId,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Unlinked, ClientError> {
    let (scid, chain) = scid_and_chain(executor, did).await?;
    if !chain.state().keys.contains_key(key_id) {
        return Err(ClientError::Protocol(format!(
            "{key_id} is no device key of {did}"
        )));
    }
    let unlocked = vault.unlock(&scid, MasterUse::Unlink, clock.now())?;
    let event = unlink_event(&chain, unlocked.signer(clock.now())?, key_id)?;
    let head = executor.publish(std::slice::from_ref(&event), None).await?;
    let record = record::write(executor, vault, did, &[], random).await;
    Ok(Unlinked { head, record })
}
