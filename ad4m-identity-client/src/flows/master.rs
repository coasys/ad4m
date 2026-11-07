//! The master key for a device that keeps it (spec section 17): a device the key log lets keep it
//! ([`ad4m_identity_core::link::may_keep_master`]: a standing key of the incept, or an `add`
//! whose claim co-signature said `canKeepMaster`), sent at the link (a `device` offer), by the
//! person's choice, or on its `send-master` request. The private record's device kind never
//! decides it: any linked device can write the record.
//!
//! The holder's side ([`send_master`], and inside [`super::link::LinkFlow::confirm`]): the
//! current master key seals itself ([`crate::MasterSigner::seal_self`]) into
//! `InboxBody::Master`, signed by itself, to the `enc` key the log lists for the device key, and
//! goes into that device's joined inbox (`identity.inboxPut`, a store request the master key
//! signs). The record then marks the device `holdsMaster: "offered"`.
//!
//! **The retry list.** Each push is kept on the vault's retry list before the put and forgotten
//! once the store takes it, or when the store refuses it for good. [`retry_pushes`], at each
//! start and unlock, pushes what is left again until the store takes it or the key is no longer
//! a device of the log.
//!
//! The device's side ([`take_master`]): its custody service takes the key its executor opened
//! (`identity.takeMaster`), checks it against its own fold of the log, keeps it in the vault,
//! acknowledges the message, and marks itself `holdsMaster: true` in the record.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::inbox::{InboxAddress, InboxError};
use ad4m_identity_core::link::may_keep_master;
use ad4m_identity_core::record::HoldsMaster;
use ad4m_identity_core::{Event, KelChain, KeyId};
use ad4m_ldk::identity::InboxWrite;

use crate::executor::{ExecutorRpc, RpcError, ViaExecutor};
use crate::record::{self, Change};
use crate::request::{
    body_bytes, chain_of, log_of, master_at_head, master_request, scid_of, store_of, MasterPosition,
};
use crate::vault::{KeySigner, MasterSigner, MasterUse, PendingPush, Vault};
use crate::{ClientError, Clock, Random};

/// A master key put into a device's inbox.
#[derive(Debug)]
pub struct Sent {
    /// The inbox message's id.
    pub message_id: String,
    /// The record write that marks the device `holdsMaster: "offered"`, after the push. The
    /// push stands either way.
    pub record: Result<u64, ClientError>,
}

/// What [`retry_pushes`] did.
#[derive(Debug)]
pub struct Retried {
    /// Each push tried, by device key id: the message id, or why it failed (a failure that may
    /// pass later stays on the retry list).
    pub pushes: Vec<(KeyId, Result<String, ClientError>)>,
    /// The record write after the pushes that passed, if any did.
    pub record: Option<Result<u64, ClientError>>,
}

/// The master key a device took.
#[derive(Debug)]
pub struct Taken {
    pub did: String,
    pub index: u32,
    /// The record write that marks this device `holdsMaster: true`. The vault keeps the key
    /// either way.
    pub record: Result<u64, ClientError>,
}

/// A refusal that a later try may pass: the store or the executor unreachable or busy.
fn transient(e: &ClientError) -> bool {
    matches!(e, ClientError::Rpc(RpcError { code, .. }) if *code >= 500 || *code == 429)
}

/// The device key `key_id` of `chain` that may be sent the master key: valid at the head,
/// without the bot mark, with an `enc` key, and let keep it by the log `events` (`chain`'s
/// events, [`may_keep_master`]). Answers its signing key and `enc` key.
fn keeper<'c>(
    events: &[Event],
    chain: &'c KelChain,
    key_id: &str,
) -> Result<(&'c str, &'c str), ClientError> {
    let state = chain.state();
    state
        .keys
        .get(key_id)
        .filter(|k| k.scopes.is_empty() && state.key_valid_at(key_id, chain.head().seq))
        .filter(|_| may_keep_master(events, key_id))
        .and_then(|k| Some((k.sign.as_str(), k.enc.as_deref()?)))
        .ok_or_else(|| ClientError::NotAKeeper(key_id.to_string()))
}

/// Puts the master key `signer` (the current one at `chain`'s head) into the joined inbox of
/// device `key_id` of `did`, sealed to its `enc` key in the log; refused unless the log
/// `events` lets the device keep it ([`keeper`]). The push is on the vault's
/// retry list from the start, and stays there when it fails in a way a later try may pass (the
/// executor or the store unreachable or busy). Answers the inbox message's id.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn push<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    events: &[Event],
    chain: &KelChain,
    did: &str,
    signer: &dyn MasterSigner,
    key_id: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<String, ClientError> {
    let (sign, enc) = keeper(events, chain, key_id)?;
    let scid = chain.id().to_string();
    let pending = PendingPush {
        scid: scid.clone(),
        key_id: key_id.to_string(),
    };
    vault.keep_push(&pending)?;
    let put = async {
        let by = master_at_head(chain, signer)?;
        let store = store_of(executor, did).await?;
        let address = InboxAddress::Joined {
            scid: scid.clone(),
            key_id: key_id.to_string(),
        };
        let write = InboxWrite {
            sealed: signer.seal_self(did, sign, enc, &address, by.clone(), clock.now(), random)?,
            bind: None,
        };
        let at = MasterPosition {
            did,
            store: &store,
            by: &by,
        };
        let path = format!("/v1/inbox/{scid}/{key_id}");
        let body = body_bytes(&write)?;
        let request = master_request(&at, signer, "PUT", &path, &body, random, clock.now())?;
        let stored = executor
            .inbox_put(&address, &write.sealed, None, &request)
            .await?;
        Ok::<_, ClientError>(stored.id)
    };
    let sent = put.await;
    if !matches!(&sent, Err(e) if transient(e)) {
        vault.push_done(&pending)?;
    }
    sent
}

/// Sends the current master key of `did` to its device `key_id` (the person chose it: "Keep my
/// master key on this device too", or "send it again"), with one unlock, and marks the device
/// `holdsMaster: "offered"` in the record. Refused before the unlock unless [`check_keeper`]
/// passes.
pub async fn send_master<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    key_id: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Sent, ClientError> {
    let scid = scid_of(did)?;
    let (events, chain) = log_of(executor, did).await?;
    check_keeper(&events, &chain, key_id)?;
    let unlocked = vault.unlock(&scid, MasterUse::SendMaster, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    send_with(
        executor, vault, &events, &chain, did, signer, key_id, clock, random,
    )
    .await
}

/// Refused unless the device key `key_id` may be sent the master key: a device key of `chain`,
/// valid at the head, that the log `events` lets keep it ([`keeper`], [`may_keep_master`]). The
/// record's device kind is not read: any linked device can write it.
pub(crate) fn check_keeper(
    events: &[Event],
    chain: &KelChain,
    key_id: &str,
) -> Result<(), ClientError> {
    keeper(events, chain, key_id).map(|_| ())
}

/// [`send_master`] under an unlock the caller holds, after [`check_keeper`].
#[allow(clippy::too_many_arguments)]
pub(crate) async fn send_with<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    events: &[Event],
    chain: &KelChain,
    did: &str,
    signer: &dyn MasterSigner,
    key_id: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Sent, ClientError> {
    let message_id = push(
        executor, vault, events, chain, did, signer, key_id, clock, random,
    )
    .await?;
    let record = record::write(
        executor,
        did,
        &[Change::HoldsMaster {
            key_id: key_id.to_string(),
            holds: HoldsMaster::Offered,
        }],
        random,
    )
    .await;
    Ok(Sent { message_id, record })
}

/// Pushes again what the retry list keeps for `did`, the executor's identity, with one unlock
/// when anything is left: a push whose device key is no longer a device of the log is
/// forgotten; each push that passes marks its device `holdsMaster: "offered"` in one record
/// write.
pub async fn retry_pushes<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Retried, ClientError> {
    let scid = scid_of(did)?;
    let pending: Vec<PendingPush> = vault
        .pending_pushes()?
        .into_iter()
        .filter(|p| p.scid == scid)
        .collect();
    let mut retried = Retried {
        pushes: Vec::new(),
        record: None,
    };
    if pending.is_empty() {
        return Ok(retried);
    }
    let (events, chain) = log_of(executor, did).await?;
    let mut live = Vec::new();
    for push in pending {
        if keeper(&events, &chain, &push.key_id).is_ok() {
            live.push(push.key_id);
        } else {
            vault.push_done(&push)?;
        }
    }
    if live.is_empty() {
        return Ok(retried);
    }
    let unlocked = vault.unlock(&scid, MasterUse::SendMaster, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    let mut offered = Vec::new();
    for key_id in live {
        let sent = push(
            executor, vault, &events, &chain, did, signer, &key_id, clock, random,
        )
        .await;
        if sent.is_ok() {
            offered.push(Change::HoldsMaster {
                key_id: key_id.clone(),
                holds: HoldsMaster::Offered,
            });
        }
        retried.pushes.push((key_id, sent));
    }
    if !offered.is_empty() {
        retried.record = Some(record::write(executor, did, &offered, random).await);
    }
    Ok(retried)
}

/// The device's side, after its own unlock: takes the master key its executor opened
/// (`identity.takeMaster`, the custody channel), checks that it is the current master of the
/// log as this app folds it, keeps it in `vault`, acknowledges the message, and marks this
/// device `holdsMaster: true` in the record. `None` when no master key waits. A key that does
/// not check, or a vault that refuses it, leaves the message for the next try.
pub async fn take_master<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    random: &dyn Random,
) -> Result<Option<Taken>, ClientError> {
    let Some(taken) = executor.take_master().await? else {
        return Ok(None);
    };
    let scid = scid_of(&taken.did)?;
    let chain = chain_of(executor, &taken.did).await?;
    let state = chain.state();
    let wrong = || ClientError::Inbox(InboxError::Master(taken.index));
    let listed = state.masters.get(taken.index as usize).ok_or_else(wrong)?;
    let key = KeySigner::new(taken.secret.expose(), taken.index).key();
    if key != listed.sign || state.current_master().map(|m| &m.id) != Some(&listed.id) {
        return Err(wrong());
    }
    let key_id =
        executor.status().await?.key_id.ok_or_else(|| {
            ClientError::Protocol("the executor's device key is in no log".into())
        })?;
    vault.store_master(&scid, taken.index, taken.secret)?;
    executor.inbox_ack(&taken.message_id).await?;
    let record = record::write(
        executor,
        &taken.did,
        &[Change::HoldsMaster {
            key_id,
            holds: HoldsMaster::Yes,
        }],
        random,
    )
    .await;
    Ok(Some(Taken {
        did: taken.did,
        index: taken.index,
        record,
    }))
}
