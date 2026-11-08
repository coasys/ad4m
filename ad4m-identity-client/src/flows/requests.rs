//! The request box, the holder's side (spec section 17). A device posts a request with its own
//! key (`identity.postRequest`, [`ViaExecutor::post_request`]); an app with the master key lists
//! the box (`identity.requests`, [`ViaExecutor::requests`]), shows each request, and acts only
//! on the person's yes ([`answer`]): a request changes nothing by itself.
//!
//! [`answer`] first checks that the device the request names posted it: the request carries
//! the store request it was posted with, signed by that device key at a position the log
//! holds, over the hash of `JCS({body, expires})`; and that it still holds (not expired, and a
//! `confirm-revoke` names keys valid at the head). Then:
//! - `send-master`: the master key goes to the poster as a link's does ([`super::master`]),
//!   only when the key log lets the poster keep it (a standing key of the incept, or an `add`
//!   whose claim said `canKeepMaster`; never the record's device kind); the record marks it
//!   `holdsMaster: "offered"`.
//! - `confirm-revoke`: the listed keys are revoked ([`super::revoke`]). One that asks for a
//!   rotation too needs the 12 words, and is left for the rotation flow.
//!
//! The answered request is deleted with a store request the master key signs.

use ad4m_hash_chain::{decode_proof_value, hash_bytes};
use ad4m_identity_core::{verify_signed, KelChain, REQUEST_DOMAIN};
use ad4m_ldk::identity::{DeviceRequest, DeviceRequestBody, PostedRequest};

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::flows::master::{self, Sent};
use crate::flows::revoke::{revoke_with, Revoked};
use crate::request::{
    body_bytes, chain_of, log_of, master_at_head, master_request, scid_of, store_of, MasterPosition,
};
use crate::vault::{MasterSigner, MasterUse, Vault};
use crate::{ClientError, Clock, Random};

/// What answering a request did.
#[derive(Debug)]
pub enum Answered {
    /// `send-master`: the master key is in the poster's inbox.
    MasterSent(Sent),
    /// `confirm-revoke`: the keys are revoked.
    Revoked(Revoked),
}

/// Checks that `request` of `did`'s box was posted by the device key it names: a device key
/// of the log without the bot mark, valid at the head and at the position its signed request
/// names, which signed a `POST /v1/requests/{scid}` over this body and expiry.
pub fn check_poster(
    request: &DeviceRequest,
    did: &str,
    chain: &KelChain,
) -> Result<(), ClientError> {
    let refused = |why: &str| ClientError::Poster(why.to_string());
    let state = chain.state();
    let req = &request.signed.req;
    let key = state
        .keys
        .get(&request.by)
        .filter(|k| k.scopes.is_empty() && state.key_valid_at(&k.id, chain.head().seq))
        .ok_or_else(|| refused("the poster is no device key valid at the head"))?;
    if req.key_id != request.by
        || req.did != did
        || req.method != "POST"
        || req.path != format!("/v1/requests/{}", chain.id())
    {
        return Err(refused("the signed request is for another post"));
    }
    let posted = PostedRequest {
        body: request.body.clone(),
        expires: request.expires,
    };
    if req.body_hash != hash_bytes(&body_bytes(&posted)?) {
        return Err(refused("the signed request covers another body"));
    }
    if chain.hash_at(req.kel_seq) != Some(&req.kel_hash)
        || !state.key_valid_at(&key.id, req.kel_seq)
    {
        return Err(refused(
            "the signed request names a position the log does not hold",
        ));
    }
    let sig = decode_proof_value(&request.signed.sig).ok_or_else(|| refused("signature"))?;
    if !verify_signed(REQUEST_DOMAIN, req, &key.sign, &sig) {
        return Err(refused("signature"));
    }
    Ok(())
}

/// Refused when `request` no longer holds at `now` and at `chain`'s head: it expired, or a
/// `confirm-revoke` names a key that is not valid there (revoked since, or never in the log).
fn check_current(request: &DeviceRequest, chain: &KelChain, now: u64) -> Result<(), ClientError> {
    if request.expires <= now {
        return Err(ClientError::Protocol("the request expired".into()));
    }
    if let DeviceRequestBody::ConfirmRevoke { key_ids, .. } = &request.body {
        let state = chain.state();
        if let Some(id) = key_ids
            .iter()
            .find(|id| !state.keys.contains_key(*id) || !state.key_valid_at(id, chain.head().seq))
        {
            return Err(ClientError::Protocol(format!(
                "{id} is no device key valid at the head"
            )));
        }
    }
    Ok(())
}

/// The person said yes to `request` of `did`'s box, the executor's identity: does what it asks
/// with one unlock, then deletes it. Refused before the unlock when its poster did not sign it
/// ([`check_poster`]), when it no longer holds against the log and the clock (expired, or a
/// `confirm-revoke` naming a key not valid at the head), or when the poster may not keep the
/// master key ([`master::check_keeper`]).
pub async fn answer<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    request: &DeviceRequest,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Answered, ClientError> {
    let scid = scid_of(did)?;
    let (events, chain) = log_of(executor, did).await?;
    check_poster(request, did, &chain)?;
    check_current(request, &chain, clock.now())?;
    match &request.body {
        DeviceRequestBody::SendMaster {} => {
            master::check_keeper(&events, &chain, &request.by)?;
            let unlocked = vault.unlock(&scid, MasterUse::SendMaster, clock.now())?;
            let signer = unlocked.signer(clock.now())?;
            let sent = master::send_with(
                executor,
                vault,
                &events,
                &chain,
                did,
                signer,
                &request.by,
                clock,
                random,
            )
            .await?;
            delete(executor, &chain, did, signer, &request.id, clock, random).await?;
            Ok(Answered::MasterSent(sent))
        }
        DeviceRequestBody::ConfirmRevoke { rotate: true, .. } => Err(ClientError::NeedsWords),
        DeviceRequestBody::ConfirmRevoke { key_ids, .. } => {
            let unlocked = vault.unlock(&scid, MasterUse::Revoke, clock.now())?;
            let signer = unlocked.signer(clock.now())?;
            let revoked =
                revoke_with(executor, vault, did, &chain, signer, key_ids, random).await?;
            let chain = chain_of(executor, did).await?;
            delete(executor, &chain, did, signer, &request.id, clock, random).await?;
            Ok(Answered::Revoked(revoked))
        }
    }
}

/// Deletes request `id` with a store request `signer` signs at `chain`'s head. One the store
/// no longer holds (it expired, or its poster's key left the log) is gone already.
async fn delete<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    chain: &KelChain,
    did: &str,
    signer: &dyn MasterSigner,
    id: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<(), ClientError> {
    let store = store_of(executor, did).await?;
    let by = master_at_head(chain, signer)?;
    let at = MasterPosition {
        did,
        store: &store,
        by: &by,
    };
    let path = format!("/v1/requests/{}/{id}", chain.id());
    let request = master_request(&at, signer, "DELETE", &path, b"", random, clock.now())?;
    match executor.delete_request(id, &request).await {
        Err(e) if e.code != 404 => Err(e.into()),
        _ => Ok(()),
    }
}
