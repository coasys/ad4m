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
//! signs). The record then marks the device `holdsMaster: "offered"` ([`mark_offered`]), never
//! over `true`: a device that took the key before that write landed stays `true`.
//!
//! **The retry list.** Each push is kept on the vault's retry list before the put and forgotten
//! once the store takes it, or when the store refuses it for good. [`retry_pushes`], at each
//! start and unlock, pushes what is left again until the store takes it or the key is no longer
//! a device of the log.
//!
//! The device's side ([`take_master`]): its custody service takes the key its executor opened
//! (`identity.takeMaster`), checks it against its own fold of the log, keeps it in the vault
//! — under the vault's never-go-back rule, so an older key taken late never replaces a newer
//! one already kept — notes the mark it owes, acknowledges the message, and marks itself
//! `holdsMaster: true` with the `masterIndex` the vault keeps in the record ([`hold_mark`],
//! fenced by the custody epoch and the note, so the mark never stands for a copy a
//! `remove_master` on the shared vault already took), then clears the note. When that write is
//! refused because the record's recovery pair is stale after a rotation
//! ([`ClientError::RecoveryNeedsReattest`]), the key, the note and the ack stand all the same —
//! the master copy is never lost behind a record that cannot be written — and the take
//! answers the marking owed ([`Taken::record_pending`]): [`complete_pending_mark`] signs the
//! fenced mark with the master just taken, re-attesting the recovery key and landing the mark,
//! and [`refresh_master`] makes it on such a take.
//!
//! **The custody note** ([`CustodyNote`], kept by the vault per identity and per device key
//! id, so two apps with their own executors on one shared vault never read each other's). Its
//! lifecycle:
//!
//! - **take** ([`take_master`]): store the key → note [`CustodyNote::MarkOwed`] (with the
//!   custody epoch read right after the store) → ack the inbox message → mark → clear the
//!   note. A promote of a staged rotation in [`refresh_master`] does the same from its
//!   promote. [`complete_pending_mark`] clears it when its mark lands.
//! - **remove** ([`remove_master`]): read the custody epoch → note
//!   [`CustodyNote::Released`] with it → record `holdsMaster: false` → forget at that epoch
//!   → mark again (`false`). A removal refused or failed puts back the note it replaced; one
//!   whose forget finds a key installed since puts it back too (it released nothing).
//! - **repair** ([`refresh_master`], on a vault that keeps the current master): only a
//!   `MarkOwed` note enrols this device; a `Released` note never marks; no note re-attests or
//!   carries forward only a device the record lists `holdsMaster: true` already. A device
//!   with no note that the record does not list holding never unlocks or prompts.
//! - **acquisitions outrank older releases**: a take or promote replaces a `Released` note
//!   whose epoch is older than its store (that removal's compare-and-delete cannot take the
//!   new key); a `Released` that read the store's epoch or a later one stands, and the take's
//!   mark mirrors it (`false`).
//!
//! Every holder mark reads the note again inside its record write, after the version it
//! builds on was read ([`record::write_as_if`]): the note and the record version are
//! linearized, so a repair decided before a removal's release cannot land its `true` after the
//! removal's `false`.
//!
//! **After a rotation** ([`refresh_master`]): a device whose own rotation was cut off after
//! its publish promotes the key its vault staged once the log's current master index is the
//! staged one, leaves a key staged ahead of the log (its publish may still land) and discards
//! one behind it; a device whose vault keeps a master key the
//! log has replaced takes the new one when the rotating app sent it; when none waits (the push
//! failed, or the device was offline past it), it forgets the replaced key and asks for the
//! new one with a `send-master` request, which a holder answers ([`super::requests::answer`]).
//! It forgets a key only when the vault's authenticated index ([`Vault::master_index`]) is
//! known and behind the log's current master: never on an unknown index.
//!
//! **Removing the master key from a device** ([`remove_master`]): refused when no other
//! device valid at the head holds the current master key: one the record lists as
//! `holdsMaster: true` with the log's current `masterIndex` and the log lets keep it
//! ([`may_keep_master`], never the record's kind; a key only offered, an entry without an
//! index or with a replaced one, or the 12 words, do not count). Otherwise the record marks
//! this device `holdsMaster: false` first, then the vault forgets the key and its retry list
//! for the identity. The check runs on the version the record write builds on, again after
//! each stale version, so two holders removing it at once leave one copy.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::inbox::{InboxAddress, InboxError};
use ad4m_identity_core::link::may_keep_master;
use ad4m_identity_core::record::{
    DeviceEntry, DeviceKind, HoldsMaster, PrivateRecord, RecordError,
};
use ad4m_identity_core::{Event, KelChain, KeyId};
use ad4m_ldk::identity::InboxWrite;

use crate::executor::{ExecutorRpc, RpcError, ViaExecutor};
use crate::record::{self, Change, Reader};
use crate::request::{
    body_bytes, chain_of, log_of, master_at_head, master_request, scid_of, store_of, MasterPosition,
};
use crate::vault::{
    CustodyNote, Forgot, KeySigner, MasterSigner, MasterUse, PendingPush, Stored, Vault,
};
use crate::{ClientError, Clock, Random};

/// A master key put into a device's inbox.
#[derive(Debug)]
pub struct Sent {
    /// The inbox message's id.
    pub message_id: String,
    /// The record write that marks the device `holdsMaster: "offered"`, after the push;
    /// [`ClientError::HoldsMaster`] when the device took the key first. The push stands
    /// either way.
    pub record: Result<u64, ClientError>,
}

/// What [`retry_pushes`] did.
#[derive(Debug)]
pub struct Retried {
    /// Each push tried, by device key id: the message id, or why it failed (a failure that may
    /// pass later stays on the retry list).
    pub pushes: Vec<(KeyId, Result<String, ClientError>)>,
    /// The record write after the pushes that passed, if any did ([`mark_offered`]).
    pub record: Option<Result<u64, ClientError>>,
}

/// The master key a device took.
#[derive(Debug)]
pub struct Taken {
    pub did: String,
    /// The index of the master key the vault keeps now: the key taken, or a newer one the
    /// vault already kept ([`Vault::store_master`] never replaces a newer kept key).
    pub index: u32,
    /// The record write that marks this device `holdsMaster: true` with `index`. The vault
    /// keeps the key either way.
    pub record: Result<u64, ClientError>,
    /// The record's `holdsMaster` write is still owed: refused
    /// [`ClientError::RecoveryNeedsReattest`], since the record's recovery pair went stale
    /// with a rotation and waits for the new master's attestation. The key is stored and the
    /// inbox message acked either way — the master copy is never lost behind a record that
    /// cannot be written; [`complete_pending_mark`], which [`refresh_master`] makes on a
    /// take that answers so, re-attests and lands the mark. The obligation is this device's
    /// [`CustodyNote::MarkOwed`] note, kept by the vault from before the ack: if the
    /// completion is cancelled, fails or never runs, the next [`refresh_master`] makes the
    /// mark from it ([`Refreshed::Repaired`]).
    pub record_pending: bool,
}

/// A refusal that a later try may pass: the store or the executor unreachable or busy.
pub(crate) fn transient(e: &ClientError) -> bool {
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
    let record = mark_offered(executor, vault, did, vec![key_id.to_string()], random).await;
    Ok(Sent { message_id, record })
}

/// Marks the devices `key_ids`, just pushed the master key, `holdsMaster: "offered"` in one
/// record write; answers the version written. An `"offered"` never replaces `true`: the write
/// holds only on a version where none of their entries says `true` ([`record::write_if`], so
/// also on every newer version it reads after a stale one). A device whose entry says `true`
/// took the key before this write landed: it is left out and the rest are written; when none
/// is left, the answer is [`ClientError::HoldsMaster`] of the last one left out. A device the
/// record does not list is left out the same way ([`ClientError::NotListed`]), so one cannot
/// block the rest.
///
/// `true` counts only for the master pushed (the log's current one) or a later one: a device
/// whose entry says `true` with the index of a replaced master holds a stale key and is
/// offered the current one like any other ([`Change::Offered`]).
pub(crate) async fn mark_offered<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    mut key_ids: Vec<KeyId>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    // The master pushed is the log's current one; a rotation landing between this read and the
    // write leaves a take of the newer master standing all the same (`Change::Offered`).
    let current = chain_of(executor, did)
        .await?
        .state()
        .masters
        .len()
        .saturating_sub(1);
    let current = u32::try_from(current)
        .map_err(|_| ClientError::Protocol("the log lists too many masters".into()))?;
    loop {
        let changes: Vec<Change> = key_ids
            .iter()
            .map(|key_id| Change::Offered {
                key_id: key_id.clone(),
                index: current,
            })
            .collect();
        let not_held = |record: Option<&PrivateRecord>, _: &KelChain| {
            let held = key_ids.iter().find(|key_id| {
                record
                    .and_then(|r| r.devices.get(*key_id))
                    .is_some_and(|entry| {
                        entry.holds_master == HoldsMaster::Yes
                            && entry.master_index.is_none_or(|held| held >= current)
                    })
            });
            match held {
                Some(key_id) => Err(ClientError::HoldsMaster(key_id.clone())),
                None => Ok(()),
            }
        };
        let written = record::write_if(executor, vault, did, &changes, &not_held, random).await;
        match written {
            Err(ClientError::HoldsMaster(held)) | Err(ClientError::NotListed(held))
                if key_ids.len() > 1 =>
            {
                key_ids.retain(|key_id| *key_id != held);
            }
            answer => return answer,
        }
    }
}

/// Pushes again what the retry list keeps for `did`, the executor's identity, with one unlock
/// when anything is left: a push whose device key is no longer a device of the log is
/// forgotten; each push that passes marks its device `holdsMaster: "offered"` in one record
/// write. A push whose device the record does not list (its entry write and its fallback both
/// failed when it first ran) writes the entry first, `holdsMaster: false`: a device that took
/// the key could not mark an entry that is not there, and would hold it with no record of it.
/// When that entry write fails the push is skipped and stays on the retry list.
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
    let listed: Vec<KeyId> = record::read(executor, vault, did)
        .await?
        .map(|opened| opened.record.devices.into_keys().collect())
        .unwrap_or_default();
    let unlocked = vault.unlock(&scid, MasterUse::SendMaster, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    let mut offered: Vec<KeyId> = Vec::new();
    for key_id in live {
        if !listed.contains(&key_id) {
            // Insert the entry only while the record still lacks it, checked on every attempt
            // of the write: a device whose entry landed in between (holding the key, say)
            // keeps it, and the push goes on.
            let absent = |record: Option<&PrivateRecord>, _: &KelChain| match record
                .and_then(|r| r.devices.get(key_id.as_str()))
            {
                Some(_) => Err(ClientError::Listed(key_id.clone())),
                None => Ok(()),
            };
            let entry = record::write_if(
                executor,
                vault,
                did,
                &[Change::Device {
                    key_id: key_id.clone(),
                    entry: retry_entry(&key_id),
                }],
                &absent,
                random,
            )
            .await;
            match entry {
                Ok(_) | Err(ClientError::Listed(_)) => {}
                Err(e) => {
                    retried.pushes.push((key_id, Err(e)));
                    continue;
                }
            }
        }
        let sent = push(
            executor, vault, &events, &chain, did, signer, &key_id, clock, random,
        )
        .await;
        if sent.is_ok() {
            offered.push(key_id.clone());
        }
        retried.pushes.push((key_id, sent));
    }
    if !offered.is_empty() {
        retried.record = Some(mark_offered(executor, vault, did, offered, random).await);
    }
    Ok(retried)
}

/// The entry a retried push writes for a device the record does not list (`retry_pushes`):
/// the confirm's name and kind were lost with its failed write, so the entry is generic; a
/// surface can rename it.
fn retry_entry(key_id: &str) -> DeviceEntry {
    DeviceEntry {
        name: key_id.to_string(),
        kind: DeviceKind::DesktopApp,
        place: None,
        address: None,
        runs_executor: true,
        holds_master: HoldsMaster::No,
        master_index: None,
        operator: None,
    }
}

/// The device's side, after its own unlock: takes the master key its executor opened
/// (`identity.takeMaster`, the custody channel), checks that it is the current master of the
/// log as this app folds it, keeps it in `vault`, notes the mark it owes
/// ([`CustodyNote::MarkOwed`]), acknowledges the message, marks this device
/// `holdsMaster: true` in the record ([`hold_mark`], fenced by the custody epoch and the note)
/// and clears the note once that mark landed. `None` when no master key waits. A key that does
/// not check, or a vault that refuses it or its note, leaves the message for the next try. The
/// key, the note and the ack stand even when the record write is refused
/// [`ClientError::RecoveryNeedsReattest`] — the record's recovery pair waiting on the new
/// master — and the take answers it owed ([`Taken::record_pending`]); [`complete_pending_mark`]
/// completes it, and when that does not happen (cancelled, failed, the app restarted) the next
/// [`refresh_master`] does ([`Refreshed::Repaired`]), from the note.
///
/// A removal of this device's copy ([`remove_master`]) that read the custody epoch after this
/// take's store wins: its [`CustodyNote::Released`] stays, nothing is noted owed, and the mark
/// mirrors the release (`holdsMaster: false`).
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
    // Another app sharing the vault may keep a newer master already (its own take or promote
    // landed while this take was in flight): the older key never replaces it, and the answer
    // and the record name the index the vault keeps.
    let index = match vault.store_master(&scid, taken.index, taken.secret)? {
        Stored::Stored => taken.index,
        Stored::AlreadyNewer { kept } => kept,
    };
    // The obligation is durable before the ack: once the message is gone, the note is the only
    // thing that says this device owes its mark. Its epoch, read right after the store, names
    // this acquisition.
    let owed = CustodyNote::MarkOwed {
        index,
        epoch: vault.custody_epoch(&scid)?,
    };
    let noted = note_owed(vault, &scid, &key_id, &owed)?;
    executor.inbox_ack(&taken.message_id).await?;
    let record = hold_mark(
        executor,
        vault,
        &taken.did,
        &scid,
        &key_id,
        &Reader::Executor,
        random,
    )
    .await;
    if record.is_ok() && noted {
        // The mark landed: the obligation is met. A note another flow wrote since (a release)
        // is not this one and stays.
        vault.swap_custody_note(&scid, &key_id, Some(&owed), None)?;
    }
    // A write refused because the record's recovery pair waits for the new master's
    // attestation is owed still: reported pending, the key, the note and the ack stand.
    let record_pending = matches!(record, Err(ClientError::RecoveryNeedsReattest));
    Ok(Some(Taken {
        did: taken.did,
        index,
        record,
        record_pending,
    }))
}

/// Writes `owed` (a [`CustodyNote::MarkOwed`] whose epoch was read right after the store or
/// promote it stands for) as device `key_id`'s custody note for `scid`. Answers `false`,
/// writing nothing, when the note is a release that read that epoch or a later one: that
/// removal checked custody after this acquisition, so its forget may take this very key, and
/// the release stands. Any other note is replaced, a release that read an earlier epoch
/// included: its compare-and-delete forget cannot take a key stored after it read (the store
/// moved the epoch), so this acquisition is a new copy.
pub(crate) fn note_owed(
    vault: &dyn Vault,
    scid: &str,
    key_id: &str,
    owed: &CustodyNote,
) -> Result<bool, ClientError> {
    let stored_at = match owed {
        CustodyNote::MarkOwed { epoch, .. } => *epoch,
        CustodyNote::Released { .. } => {
            return Err(ClientError::Protocol(
                "a release is not an owed mark".into(),
            ))
        }
    };
    for _ in 0..NOTE_SWAPS {
        let now = vault.custody_note(scid, key_id)?;
        if matches!(now, Some(CustodyNote::Released { epoch }) if epoch >= stored_at) {
            return Ok(false);
        }
        if vault.swap_custody_note(scid, key_id, now.as_ref(), Some(owed))? {
            return Ok(true);
        }
    }
    Err(note_busy())
}

/// Compare-and-swaps of a custody note tried before giving up: another flow of the same device
/// wrote it in between every time.
const NOTE_SWAPS: usize = 8;

fn note_busy() -> ClientError {
    ClientError::Vault(crate::VaultError::Unavailable(
        "the device's custody note kept changing".into(),
    ))
}

/// This device's `holdsMaster` as the vault's custody of `scid`, fenced by the custody
/// epoch and this device's custody note. Only the operation that kept the key (a take's
/// [`Vault::store_master`], a [`Vault::promote_master`]) or a repair of the mark it owes
/// writes through here — never a bare re-attestation, which asserts no custody. Each write
/// names the index [`Vault::master_index`] authenticates (`holdsMaster: true` with it,
/// `false` when the vault keeps none, or when this device's note is
/// [`CustodyNote::Released`]: a copy it gave up is not its own, whoever installed the key in
/// the shared vault since). Two fences:
///
/// - the note is read again inside the record write, after the version it builds on was read
///   (a precondition of [`record::write_as_if`]): a release written since is seen there, and
///   the mark is decided again from it — so a `true` decided before a removal's release never
///   lands after that removal's `false`, which is written only after the release;
/// - the custody epoch is read before and after: while it moved under a write, a custody
///   operation won in between — a forget, or another app's store — and the entry is written
///   again from what the vault keeps then.
pub(crate) async fn hold_mark<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    scid: &str,
    key_id: &KeyId,
    reader: &Reader<'_>,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    hold_mark_gated(
        executor,
        vault,
        did,
        scid,
        key_id,
        reader,
        Gate::Custody,
        random,
    )
    .await
}

/// What a holder mark may assume about this device's custody note ([`hold_mark_gated`]).
#[derive(Clone, Copy, Debug)]
enum Gate {
    /// The operation that kept the key, or a removal mirroring what it left: the mark follows
    /// the custody whatever the note is (a release mirrors as none), decided again when the
    /// note changes under the write.
    Custody,
    /// A refresh's repair, decided on this note (`None`: a listed holder without one). Written
    /// only while the note is still exactly this one — and, without a note, while the record
    /// still lists the device `holdsMaster: true` — else refused
    /// [`ClientError::CustodyNoteMoved`]: the repair no longer stands, nothing is written.
    Repair(Option<CustodyNote>),
}

/// [`hold_mark`] under `gate`.
#[allow(clippy::too_many_arguments)]
async fn hold_mark_gated<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    scid: &str,
    key_id: &KeyId,
    reader: &Reader<'_>,
    gate: Gate,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    for _ in 0..record::MAX_WRITES {
        // The epoch first: a custody change after it lands in `held` or in the re-read.
        let fenced = vault.custody_epoch(scid)?;
        let note = vault.custody_note(scid, key_id)?;
        if let Gate::Repair(decided) = gate {
            if note != decided {
                return Err(ClientError::CustodyNoteMoved);
            }
        }
        let held = match note {
            Some(CustodyNote::Released { .. }) => None,
            _ => vault.master_index(scid)?,
        };
        let mut changes = vec![Change::HoldsMaster {
            key_id: key_id.clone(),
            holds: HoldsMaster::No,
        }];
        if let Some(kept) = held {
            // `Change::Holds` alone never lowers a stale `masterIndex`: the entry is
            // cleared first, so it ends naming exactly the index the vault keeps.
            changes.push(Change::Holds {
                key_id: key_id.clone(),
                index: kept,
            });
        }
        // Checked on the version the write builds on, after it was read.
        let still = |record: Option<&PrivateRecord>, _: &KelChain| {
            if vault.custody_note(scid, key_id)? != note {
                return Err(ClientError::CustodyNoteMoved);
            }
            let listed_holding = record
                .and_then(|r| r.devices.get(key_id))
                .is_some_and(|entry| entry.holds_master == HoldsMaster::Yes);
            if matches!(gate, Gate::Repair(None)) && !listed_holding {
                return Err(ClientError::CustodyNoteMoved);
            }
            Ok(())
        };
        let written =
            record::write_as_if(executor, vault, did, reader, &changes, &still, random).await;
        match written {
            // The note changed under the write: decide the mark again from the new one.
            Err(ClientError::CustodyNoteMoved) if matches!(gate, Gate::Custody) => continue,
            Err(e) => return Err(e),
            Ok(written) if vault.custody_epoch(scid)? == fenced => return Ok(written),
            // Custody moved under the write: mirror what it moved to.
            Ok(_) => {}
        }
    }
    Err(ClientError::RecordBusy)
}

/// Re-attests the record's recovery key with the current master key this device's vault
/// keeps — the write a device is refused with [`ClientError::RecoveryNeedsReattest`] while
/// the stored recovery pair is stale after a rotation. One unlock; the master signs the
/// record write itself ([`record::Reader::Master`]), so it attests the key the stored
/// record names. **It writes no `holdsMaster` mark**: a re-attestation asserts no custody —
/// an unlocked signer outlives its kept slot, so a mark here could re-advertise a copy a
/// `remove_master` on the shared vault already took, and the last other holder's only-copy
/// check would trust it. The holder mark comes only from the operation that kept the key
/// ([`hold_mark`]); a take's owed mark is [`complete_pending_mark`]'s. Answers the version
/// written.
pub async fn reattest_recovery<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    let scid = scid_of(did)?;
    let unlocked = vault.unlock(&scid, MasterUse::Record, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    record::write_as(
        executor,
        vault,
        did,
        &Reader::Master {
            master: signer,
            clock,
        },
        &[],
        None,
        random,
    )
    .await
}

/// Completes the mark a pending take owes ([`Taken::record_pending`]), and any other
/// caller's `holdsMaster` write refused [`ClientError::RecoveryNeedsReattest`]: one unlock,
/// then this device's `holdsMaster` as the vault's custody, signed by the master the vault
/// keeps ([`record::Reader::Master`]) — the same write re-attests the record's recovery
/// pair when it is stale. The mark is fenced by the custody epoch and this device's custody
/// note ([`hold_mark`]): it names the index the vault keeps when it lands, and a
/// `remove_master` that won meanwhile leaves the entry `holdsMaster: false`. On success the
/// [`CustodyNote::MarkOwed`] note read before the write is cleared (a note another flow wrote
/// since stays). A caller with a pending take calls this right away; when it is cancelled or
/// fails, the note stays, and the holder's next [`refresh_master`] makes this same write
/// from it ([`Refreshed::Repaired`]). Answers the version written.
pub async fn complete_pending_mark<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    let scid = scid_of(did)?;
    let key_id =
        executor.status().await?.key_id.ok_or_else(|| {
            ClientError::Protocol("the executor's device key is in no log".into())
        })?;
    let note = vault.custody_note(&scid, &key_id)?;
    let unlocked = vault.unlock(&scid, MasterUse::Record, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    let written = hold_mark(
        executor,
        vault,
        did,
        &scid,
        &key_id,
        &Reader::Master {
            master: signer,
            clock,
        },
        random,
    )
    .await?;
    clear_owed(vault, &scid, &key_id, note.as_ref())?;
    Ok(written)
}

/// Clears `note`, read before a holder mark that has landed since, when it was an owed mark:
/// the obligation is met. A note written in between (a release, another acquisition's) is not
/// this one and stays.
fn clear_owed(
    vault: &dyn Vault,
    scid: &str,
    key_id: &str,
    note: Option<&CustodyNote>,
) -> Result<(), ClientError> {
    if let Some(owed @ CustodyNote::MarkOwed { .. }) = note {
        vault.swap_custody_note(scid, key_id, Some(owed), None)?;
    }
    Ok(())
}

/// What [`refresh_master`] found.
#[derive(Debug)]
pub enum Refreshed {
    /// The vault keeps the current master key, and nothing was owed: no record write, no
    /// unlock.
    Current,
    /// The vault keeps the current master key, and the record owed this device's mark for it,
    /// made now. Only two things owe it:
    ///
    /// - this device's custody note says [`CustodyNote::MarkOwed`]: a take (or a promote)
    ///   kept the key and its mark has not landed — its completion cancelled, its put failed,
    ///   the app restarted. The mark is written as the device, or as the master when the
    ///   record's recovery pair is stale ([`MasterUse::Record`], one unlock), while the note is
    ///   still that one, then the note is cleared;
    /// - no note, and the record lists this device `holdsMaster: true` already: its recovery
    ///   pair is stale (only the master re-attests it, and the write carries the entry to the
    ///   index the vault keeps), or its entry names an older index.
    ///
    /// A [`CustodyNote::Released`] note, or a device the record lists otherwise (unlisted,
    /// `offered`, `false`) with no note, owes nothing: answered [`Refreshed::Current`],
    /// without reading past the record or prompting.
    Repaired {
        /// The index the vault keeps: the log's current master.
        index: u32,
        /// The record write (`holdsMaster: true` with `index`), or why the record could not
        /// be read or written (a cancelled unlock, the store down): still owed, the note
        /// stays and the next refresh tries again. The key is kept either way.
        record: Result<u64, ClientError>,
    },
    /// A rotation on this computer had staged the current master key and was cut off before
    /// it promoted it: the vault keeps a master key now, noted owed like a take's
    /// ([`CustodyNote::MarkOwed`]), and the record marks this device holding it — by the
    /// master itself, re-attesting the recovery key, when the record's stored pair went stale
    /// with the rotation. A removal that read the custody epoch after the promote wins, and
    /// the mark mirrors it (`holdsMaster: false`).
    Promoted {
        /// The index the vault keeps: the staged key's, or a newer one another app sharing
        /// the vault kept first.
        index: u32,
        /// The record write (`holdsMaster` as the vault's custody). The key is kept either
        /// way; a failed write leaves the note, so the next refresh repairs it.
        record: Result<u64, ClientError>,
    },
    /// The current master key was waiting in this device's inbox: the vault keeps it now.
    /// A take whose record write was refused while the record's recovery pair waited on the
    /// new master is completed with it ([`complete_pending_mark`]).
    Taken(Taken),
    /// None waited: the vault forgot the replaced key, and a `send-master` request (its id)
    /// asks a holder for the current one.
    Asked(String),
    /// None waited, and the vault keeps no master key of the identity, cannot tell which one
    /// it keeps ([`Vault::master_index`] is `None`), or keeps a still-replaced key at another
    /// index than the one read (installed by another app in between: [`Forgot::Kept`]).
    /// Nothing was forgotten and nothing asked. A device that never held it asks with
    /// [`super::requests::ask_for_master`].
    Unknown,
}

/// A device whose vault may keep a replaced master key of `did`, its executor's identity, or a
/// staged one: brings the vault to the log's current master where it can, in this order.
///
/// 1. A key staged by a rotation on this computer (cut off between its publish and its
///    promote, or whose publish answer was lost), by its master index against the log's
///    current one: **equal**, promoted ([`Refreshed::Promoted`]; a promote another app on the
///    computer made first counts); **ahead**, left staged (a publish may still be on its way,
///    in this app or another one sharing the vault); **behind**, or the identity ended,
///    discarded (a later rotation superseded it, or its publish was refused long ago). The
///    promotion goes by index only: the words derive the key at each index, and the staged key
///    is not checked against the log's commitment. The promote is an acquisition like a
///    take's store: the mark is noted owed ([`CustodyNote::MarkOwed`], unless a removal that
///    read the custody epoch after the promote released the copy), then written fenced by the
///    custody epoch and the note ([`hold_mark`]) as the device, or — when the record's
///    recovery pair went stale with the rotation — as the promoted master itself,
///    re-attesting it; the note is cleared once it lands.
/// 2. The vault keeps the current master: the mark this device owes, by its custody note
///    ([`repair_mark`]). A [`CustodyNote::MarkOwed`] note (a take or promote whose mark has
///    not landed: completion cancelled or failed, the app restarted) is marked now and
///    cleared ([`Refreshed::Repaired`]); a device the record lists `holdsMaster: true`
///    without a note is re-attested when the pair is stale, or carried to the current index.
///    A [`CustodyNote::Released`] note, or no note on a device not listed holding, owes
///    nothing: [`Refreshed::Current`], no write, no unlock. A device that does not keep the
///    current master never reads the record here.
/// 3. The current master waits in the inbox: taken ([`take_master`], [`Refreshed::Taken`]);
///    a take whose record write was refused [`ClientError::RecoveryNeedsReattest`] is
///    completed by [`complete_pending_mark`] with the master just taken.
/// 4. The vault's key is **known** to be replaced ([`Vault::master_index`], authenticated,
///    names an index behind the log's current master, or the identity has ended): the vault
///    forgets it — [`Vault::forget_master_at`] the custody epoch read with `kept`, so a key
///    another app sharing the vault installed since, the same one included, is never removed
///    — and a `send-master` request asks a holder for
///    the current one ([`Refreshed::Asked`]). An unknown index (`None`) never forgets a key:
///    [`Refreshed::Unknown`].
pub async fn refresh_master<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Refreshed, ClientError> {
    let scid = scid_of(did)?;
    let chain = chain_of(executor, did).await?;
    let state = chain.state();
    let live = state.current_master().is_some();
    let current = u32::try_from(state.masters.len().saturating_sub(1))
        .map_err(|_| ClientError::Protocol("the log lists too many masters".into()))?;
    match vault.staged_index(&scid)? {
        // The rotation to it landed: promote (or another app on this computer did first).
        Some(staged) if live && staged == current => {
            let index = super::rotate::promote(vault, &scid, staged)?;
            // Read right after the promote: names this acquisition, like a take's store.
            let owed = CustodyNote::MarkOwed {
                index,
                epoch: vault.custody_epoch(&scid)?,
            };
            let record = async {
                let key_id = executor.status().await?.key_id.ok_or_else(|| {
                    ClientError::Protocol("the executor's device key is in no log".into())
                })?;
                // Owed before the write, so a failed one is repaired by the next refresh.
                let noted = note_owed(vault, &scid, &key_id, &owed)?;
                // The record's recovery pair went stale with the rotation that made this key
                // current: the promoted master signs the fenced mark that re-attests it.
                let written = mark_gated(
                    executor,
                    vault,
                    did,
                    &scid,
                    &key_id,
                    Gate::Custody,
                    false,
                    clock,
                    random,
                )
                .await?;
                if noted {
                    clear_owed(vault, &scid, &key_id, Some(&owed))?;
                }
                Ok(written)
            }
            .await;
            return Ok(Refreshed::Promoted { index, record });
        }
        // Behind the log (a later rotation superseded it, or its publish was refused long
        // ago), or the identity ended: it can never become current.
        Some(staged) if !live || staged < current => vault.discard_staged(&scid, staged, None)?,
        // Ahead of the log: a rotation's publish may be on its way (in this app or another one
        // sharing the vault). Left as it is; the rotating flow discards it when its publish is
        // refused, and the next stage replaces it.
        Some(_) | None => {}
    }
    // Read together, before the take: the forget below is bound to this custody state.
    let epoch = vault.custody_epoch(&scid)?;
    let kept = vault.master_index(&scid)?;
    if live && kept == Some(current) {
        return Ok(repair_mark(executor, vault, did, &chain, current, clock, random).await);
    }
    if let Some(mut taken) = take_master(executor, vault, random).await? {
        if taken.did == did {
            // The take stored and acked the key though its record write was refused: the
            // record's recovery pair waited on the new master's attestation. The repair
            // re-attests it and lands the held mark, fenced by the custody epoch.
            if taken.record_pending {
                taken.record = complete_pending_mark(executor, vault, did, clock, random).await;
                taken.record_pending = taken.record.is_err();
            }
            return Ok(Refreshed::Taken(taken));
        }
    }
    match kept {
        Some(index) if index < current || !live => {
            // `kept` was read before the take. Another app sharing the vault may have
            // installed a key since (its take, or its promote), the same index included: the
            // forget holds only while the custody epoch read with `kept` is unchanged, so it
            // never takes a key installed after the check.
            match vault.forget_master_at(&scid, epoch)? {
                Forgot::Forgot => {
                    let id = executor
                        .post_request(&ad4m_ldk::identity::DeviceRequestBody::SendMaster {}, None)
                        .await?;
                    Ok(Refreshed::Asked(id))
                }
                Forgot::Kept(kept) if live && kept == current => {
                    Ok(repair_mark(executor, vault, did, &chain, current, clock, random).await)
                }
                Forgot::Kept(_) => Ok(Refreshed::Unknown),
            }
        }
        _ => Ok(Refreshed::Unknown),
    }
}

/// The vault keeps the log's current master `current` of `did` (`chain`): the mark this device
/// owes made now ([`Refreshed::Repaired`]), else [`Refreshed::Current`]. What owes it is this
/// device's custody note, never the record alone:
///
/// - [`CustodyNote::Released`]: nothing, not even a record read — the copy was given up, and
///   a key in the shared vault since is another acquisition's to mark;
/// - [`CustodyNote::MarkOwed`]: cleared when the record marks this device `holdsMaster:
///   true` with `current` and its recovery pair is current; else marked from the vault's
///   custody while the note is still that one ([`Gate::Repair`]), then cleared;
/// - no note: only a device the record lists `holdsMaster: true` already, whose recovery
///   pair is stale or whose entry names another index, is written again (re-attesting, or
///   carrying the index forward) — and only while the entry still says `true`, so a removal's
///   `false` that landed first is never overridden.
///
/// A device the record does not list, or lists `offered` or `false`, with no note, owes
/// nothing: no unlock and no prompt, whatever the recovery pair. Without a stale pair the
/// writes go as the device, and as the master (one [`MasterUse::Record`] unlock) only when the
/// device's is refused [`ClientError::RecoveryNeedsReattest`]. A note that changed under the
/// write ([`ClientError::CustodyNoteMoved`]) left nothing to repair here: answered
/// [`Refreshed::Current`].
#[allow(clippy::too_many_arguments)]
async fn repair_mark<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    chain: &KelChain,
    current: u32,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Refreshed {
    let repair = async {
        let scid = scid_of(did)?;
        let key_id = executor.status().await?.key_id.ok_or_else(|| {
            ClientError::Protocol("the executor's device key is in no log".into())
        })?;
        let note = vault.custody_note(&scid, &key_id)?;
        if matches!(note, Some(CustodyNote::Released { .. })) {
            return Ok(None);
        }
        let opened =
            match record::read_as(executor, vault, did, chain, &Reader::Executor, random).await {
                // No record key is wrapped for this device: the record does not list it.
                Err(ClientError::Record(RecordError::NoEntry(_))) => return Ok(None),
                read => read?,
            };
        let Some(opened) = opened else {
            return Ok(None);
        };
        let Some(entry) = opened.record.devices.get(&key_id) else {
            return Ok(None);
        };
        let stale = opened.recovery.is_none() && opened.recovery_key.is_some();
        let holding = entry.holds_master == HoldsMaster::Yes;
        let marked = holding && entry.master_index == Some(current);
        let gate = match note {
            Some(owed @ CustodyNote::MarkOwed { .. }) if marked && !stale => {
                clear_owed(vault, &scid, &key_id, Some(&owed))?;
                return Ok(None);
            }
            Some(owed @ CustodyNote::MarkOwed { .. }) => Gate::Repair(Some(owed)),
            None if holding && (stale || !marked) => Gate::Repair(None),
            _ => return Ok(None),
        };
        let written = mark_gated(
            executor, vault, did, &scid, &key_id, gate, stale, clock, random,
        )
        .await?;
        clear_owed(vault, &scid, &key_id, note.as_ref())?;
        Ok(Some(written))
    };
    match repair.await {
        Ok(None) | Err(ClientError::CustodyNoteMoved) => Refreshed::Current,
        Ok(Some(version)) => Refreshed::Repaired {
            index: current,
            record: Ok(version),
        },
        Err(e) => Refreshed::Repaired {
            index: current,
            record: Err(e),
        },
    }
}

/// [`hold_mark_gated`] under `gate`: as the master the vault keeps when the record's recovery
/// pair is known `stale` (one [`MasterUse::Record`] unlock; the write re-attests it), else as
/// the device, and as the master when the device's write is refused
/// [`ClientError::RecoveryNeedsReattest`] (the pair went stale since the read).
#[allow(clippy::too_many_arguments)]
async fn mark_gated<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    scid: &str,
    key_id: &KeyId,
    gate: Gate,
    stale: bool,
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    if !stale {
        match hold_mark_gated(
            executor,
            vault,
            did,
            scid,
            key_id,
            &Reader::Executor,
            gate,
            random,
        )
        .await
        {
            Err(ClientError::RecoveryNeedsReattest) => {}
            other => return other,
        }
    }
    let unlocked = vault.unlock(scid, MasterUse::Record, clock.now())?;
    let signer = unlocked.signer(clock.now())?;
    hold_mark_gated(
        executor,
        vault,
        did,
        scid,
        key_id,
        &Reader::Master {
            master: signer,
            clock,
        },
        gate,
        random,
    )
    .await
}

/// Removes the master key of `did`, its executor's identity, from this device. Refused with
/// [`ClientError::OnlyCopy`] unless another device valid at the head holds the current master
/// key ([`another_copy`]): the record lists it as `holdsMaster: true` with the log's current
/// master index, and the log lets it keep the master key ([`may_keep_master`]; the record's
/// device kind grants nothing, any linked device can write the record). The check runs on the
/// record version the write builds on, again after each stale version ([`record::write_if`]),
/// so of two holders removing it at once at most one does.
///
/// The order: the custody epoch is read; this device's custody note becomes
/// [`CustodyNote::Released`] with it; the record marks this device `holdsMaster: false`; the
/// vault forgets the key ([`Vault::forget_master_at`] that epoch, so a key another app sharing
/// the vault installed in between, the same one included, is never removed); and, when it
/// forgot, the entry is marked again from the vault ([`hold_mark`]: `false`, the note being a
/// release). The release is durable before the `false` write: every holder mark reads the note
/// on the record version it builds on, so a repair decided before the release either landed
/// before the `false` (which overwrites it) or sees the release and writes nothing. A refused
/// or failed record write puts the note back as it was and leaves the key where it was.
/// Answers the record version written.
pub async fn remove_master<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    random: &dyn Random,
) -> Result<u64, ClientError> {
    let scid = scid_of(did)?;
    let own =
        executor.status().await?.key_id.ok_or_else(|| {
            ClientError::Protocol("the executor's device key is in no log".into())
        })?;
    // Before the only-copy check: the forget below holds only while this custody state lasts.
    let epoch = vault.custody_epoch(&scid)?;
    let released = CustodyNote::Released { epoch };
    let before = release(vault, &scid, &own, &released)?;
    let removed = async {
        let (events, log_chain) = log_of(executor, did).await?;
        let check = |record: Option<&PrivateRecord>, chain: &KelChain| {
            // The events may lag the chain the write read; a key `add`ed since is no copy yet.
            if record.is_some_and(|r| another_copy(r, &events, chain, &own)) {
                Ok(())
            } else {
                Err(ClientError::OnlyCopy)
            }
        };
        let version = record::write_if(
            executor,
            vault,
            did,
            &[Change::HoldsMaster {
                key_id: own.clone(),
                holds: HoldsMaster::No,
            }],
            &check,
            random,
        )
        .await?;
        Ok::<_, ClientError>((version, log_chain))
    }
    .await;
    let (mut version, log_chain) = match removed {
        Ok(removed) => removed,
        Err(e) => {
            // Nothing was released: the note goes back to what it was, unless another flow
            // wrote it since.
            vault.swap_custody_note(&scid, &own, Some(&released), before.as_ref())?;
            return Err(e);
        }
    };
    // Compare-and-delete on the custody epoch read before the only-copy check: a key another
    // app sharing the vault installed since (a duplicate delivery of the same master taken, a
    // promote) is never removed, whatever its index, since the check did not see it; an
    // unknown index (`None`) forgets nothing.
    if vault.master_index(&scid)?.is_some() {
        match vault.forget_master_at(&scid, epoch)? {
            Forgot::Forgot => {
                // A holder mark that read the custody before this forget (between the write
                // above and here) may have written `true` back: mark again from the vault and
                // the release (`false`), so the record matches the vault once the key is gone.
                version =
                    hold_mark(executor, vault, did, &scid, &own, &Reader::Executor, random).await?;
            }
            // A key installed after the epoch was read stays, so this removal released
            // nothing: the note goes back to what it replaced (an owed mark the installing
            // take noted first among them, which the next refresh then makes). The record
            // keeps the `false` written: one vault never counts as a second copy.
            Forgot::Kept(_) => {
                vault.swap_custody_note(&scid, &own, Some(&released), before.as_ref())?;
            }
        }
    }
    // A stage at or behind the log would be promoted or kept by the next refresh, bringing back
    // the key the person just removed. One ahead is a rotation in flight: its owner decides.
    let current = log_chain.state().masters.len().saturating_sub(1);
    if let Some(staged) = vault.staged_index(&scid)? {
        if usize::try_from(staged).is_ok_and(|staged| staged <= current) {
            vault.discard_staged(&scid, staged, None)?;
        }
    }
    for push in vault.pending_pushes()? {
        if push.scid == scid {
            vault.push_done(&push)?;
        }
    }
    Ok(version)
}

/// Writes `released` as device `key_id`'s custody note for `scid`, over whatever it was (an
/// owed mark included: the person gives the copy up). Answers the note it replaced.
fn release(
    vault: &dyn Vault,
    scid: &str,
    key_id: &str,
    released: &CustodyNote,
) -> Result<Option<CustodyNote>, ClientError> {
    for _ in 0..NOTE_SWAPS {
        let now = vault.custody_note(scid, key_id)?;
        if vault.swap_custody_note(scid, key_id, now.as_ref(), Some(released))? {
            return Ok(now);
        }
    }
    Err(note_busy())
}

/// Whether `record` lists a device other than `own` that holds the current master key of
/// `chain`: valid at the head, `holdsMaster: true` with `masterIndex` the index of the log's
/// current master (an entry without an index, or with an older one, proves no current copy),
/// and let keep it by the log `events` ([`may_keep_master`]).
pub(crate) fn another_copy(
    record: &PrivateRecord,
    events: &[Event],
    chain: &KelChain,
    own: &str,
) -> bool {
    let state = chain.state();
    let head = chain.head().seq;
    let Some(current) = state
        .current_master()
        .and_then(|_| u32::try_from(state.masters.len() - 1).ok())
    else {
        return false;
    };
    record.devices.iter().any(|(id, entry)| {
        id != own
            && entry.holds_master == HoldsMaster::Yes
            && entry.master_index == Some(current)
            && state.keys.contains_key(id)
            && state.key_valid_at(id, head)
            && may_keep_master(events, id)
    })
}
