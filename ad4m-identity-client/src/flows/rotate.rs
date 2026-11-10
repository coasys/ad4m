//! `rotate`: replace the master key. A `rotate` reveals master *n*+1 and is signed by it alone,
//! so it needs the 12 words, not master *n*: a stolen device that holds master *n* cannot
//! rotate, and the words rotate even when no device holds a master key any more.
//!
//! 1. The words derive master *n*+1 at `m/7133'/0'/(n+1)'`, its salt, and the commitment to
//!    master *n*+2 the new `next` holds. *n* is the current master's index, and the words must
//!    derive the current master ([`ClientError::NotTheseWords`] otherwise).
//! 2. **Custody before publish**: the vault stages master *n*+1 ([`Vault::stage_master`]); a
//!    vault that refuses leaves nothing public. The event is published through the executor. **A `409`** (another event landed on the
//!    head first) reads the log again and builds the rotation again on the new head, at most
//!    [`MAX_TRIES`] times, then stops with [`ClientError::LogBusy`]. Each rebuild takes *n*
//!    from the new head (after a rotation by another app with the same words, the words derive
//!    the next master on), and revokes only the listed keys still valid there. A rotation that
//!    landed while its answer was lost is found in the log by its hash and not published again.
//! 3. The vault promotes the staged master *n*+1 in place of master *n*
//!    ([`Vault::promote_master`]); a promote another app sharing the vault made first (its
//!    `refresh_master` saw the publish land) counts as kept, as does a newer key another app
//!    kept first — the kept key never goes back a generation, and [`Rotated::kept`] names
//!    the index the vault actually keeps. A publish refused for good
//!    discards the key this flow staged (by its [`StageToken`]: a key another app sharing the
//!    vault staged since is left); one whose answer was lost, or a flow cut off before the
//!    promote, leaves it staged, and [`super::master::refresh_master`] promotes it when the
//!    log's current master index is *n*+1, leaves it while the log is behind it, and discards
//!    it once the log is past it. A promote that fails is reported in [`Rotated::kept`]: the
//!    rotation stands.
//! 4. The record's master-signed write comes first: it re-attests the recovery key with
//!    master *n*+1 — until it lands, the stored attestation is stale and every device write
//!    would be refused rather than seal the record without it — and lists this device.
//! 5. Master *n*+1 goes to every other device that kept the master key: each device the record
//!    lists as holding it (`true` or `"offered"`), valid at the head, that the log after the
//!    rotation lets keep it ([`ad4m_identity_core::link::may_keep_master`]; the record's device
//!    kind grants nothing, any linked device can write the record), sealed to its `enc` key in its joined inbox ([`super::master`], with the retry list). The store
//!    drops every message master *n* wrote when the rotation lands, so no device takes the
//!    replaced key after it. Each device sent the key is marked `holdsMaster: "offered"`.

use ad4m_identity_core::build::{rotate as rotate_event, Signer};
use ad4m_identity_core::link::may_keep_master;
use ad4m_identity_core::record::{DeviceEntry, HoldsMaster, PrivateRecord};
use ad4m_identity_core::words::{self, Seed, Words};
use ad4m_identity_core::{event_hash, x25519_multikey_of, Event, KelChain, KeyId, Multikey};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::flows::master::{self, transient};
use crate::flows::revoke::check_keys;
use crate::record::{self, Change, Reader};
use crate::request::{self, chain_of, scid_of};
use crate::secret::Secret;
use crate::vault::{CustodyNote, KeySigner, MasterSigner, StageToken, Stored, Vault, VaultError};
use crate::{fold, ClientError, Clock, Random};

/// Rotations built and published before [`ClientError::LogBusy`]: the first, and a rebuild on
/// the new head after each `409`.
pub const MAX_TRIES: usize = 3;

/// A rotation done.
#[derive(Debug)]
pub struct Rotated {
    pub head: SignedHead,
    /// The new master's index *n*+1 (its key id is `m<n+1>`).
    pub index: u32,
    /// The device keys the rotation revoked: those asked for that were still valid at the head
    /// it was built on.
    pub revoked: Vec<KeyId>,
    /// Rotations built: more than 1 when another event landed first.
    pub tries: usize,
    /// Master *n*+1 put into each kept holder's inbox, by device key id: the message id, or why
    /// the push failed (a failure that may pass later stays on the vault's retry list).
    pub pushes: Vec<(KeyId, Result<String, ClientError>)>,
    /// The record write after the pushes. The rotation stands either way.
    pub record: Result<u64, ClientError>,
    /// The index of the master key the vault keeps after this rotation ([`Vault::promote_master`]
    /// or the vault rule of [`Vault::store_master`]): this rotation's *n*+1, or a later one
    /// another app sharing the vault kept first ([`Stored::AlreadyNewer`]). An error leaves
    /// *n*+1 staged and this device listed as not holding the master key: the rotation
    /// stands, and `master::refresh_master` promotes the staged key later.
    pub kept: Result<u32, ClientError>,
}

/// Master `index` of `seed`, as a signer.
pub(crate) fn master_of(seed: &Seed, index: u32) -> Result<KeySigner, ClientError> {
    let key = words::master_key(seed, index)?;
    let bytes = zeroize::Zeroizing::new(key.to_bytes());
    Ok(KeySigner::new(&bytes, index))
}

/// The index of `chain`'s current master, which `seed` must derive.
pub(crate) fn current_index(chain: &KelChain, seed: &Seed) -> Result<u32, ClientError> {
    let state = chain.state();
    let current = state
        .current_master()
        .ok_or_else(|| ClientError::Protocol("the identity has ended".into()))?;
    let index = u32::try_from(state.masters.len() - 1)
        .map_err(|_| ClientError::Protocol("the log lists too many masters".into()))?;
    if master_of(seed, index)?.key() != current.sign {
        return Err(ClientError::NotTheseWords(
            "they do not derive the current master key".into(),
        ));
    }
    Ok(index)
}

/// The words' recovery key, as the record names it.
pub(crate) fn recovery_key_of(seed: &Seed) -> Multikey {
    x25519_multikey_of(&words::recovery_secret(seed))
}

/// The vault stages master `index` of `seed` for `scid` with `owner`, before the rotation to it
/// is published ([`Vault::stage_master`]): the kept key stays as it is until
/// [`Vault::promote_master`].
pub(crate) fn stage(
    vault: &dyn Vault,
    scid: &str,
    seed: &Seed,
    index: u32,
    owner: &StageToken,
) -> Result<(), ClientError> {
    let key = words::master_key(seed, index)?;
    vault.stage_master(scid, index, Secret::new(key.to_bytes()), owner)?;
    Ok(())
}

/// The vault promotes staged master `index` of `scid` ([`Vault::promote_master`]). Every app on
/// the computer shares the vault, so another one's `refresh_master` may have promoted it
/// already (a `NotFound` while the vault keeps master `index` or a later one), or kept a later
/// rotation's key before this one's promotion ran ([`Stored::AlreadyNewer`]). Answers the
/// index the vault keeps and whether this call's own promote installed it (`Stored`): only
/// then is the key this device's acquisition. `Err` when the vault keeps no master at `index`
/// or later.
pub(crate) fn promote(
    vault: &dyn Vault,
    scid: &str,
    index: u32,
) -> Result<(u32, bool), VaultError> {
    match vault.promote_master(scid, index) {
        Ok(Stored::Stored) => Ok((index, true)),
        Ok(Stored::AlreadyNewer { kept }) => Ok((kept, false)),
        Err(VaultError::NotFound) => match vault.master_index(scid)? {
            Some(kept) if kept >= index => Ok((kept, false)),
            _ => Err(VaultError::NotFound),
        },
        Err(e) => Err(e),
    }
}

/// The log of `did` read fresh, with the head the executor accepted, folded here.
async fn log_of<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<(Option<SignedHead>, KelChain), ClientError> {
    let log = executor.log(did, 0, true).await?;
    let events = log
        .events
        .iter()
        .map(Event::from_json)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| ClientError::Protocol(format!("the log holds an unparsable event: {e}")))?;
    Ok((log.head, fold(&events)?))
}

/// `event` is in `chain`, at its seq with its hash.
fn landed(chain: &KelChain, event: &Event) -> bool {
    event_hash(event).is_ok_and(|hash| chain.hash_at(event.seq) == Some(&hash))
}

/// A rotation in the log.
pub(crate) struct Published {
    pub head: SignedHead,
    pub signer: KeySigner,
    pub revoked: Vec<KeyId>,
    pub tries: usize,
}

/// Builds the rotation of `did` to the master after its current one, revoking those of
/// `revoke` valid at the head, and publishes it; a `409`, or an answer that never came, reads
/// the log and builds it again, at most [`MAX_TRIES`] times.
///
/// **Custody before publish**: each build stages its master *n*+1 in `vault` (for `scid`)
/// before the publish, so a vault that refuses leaves nothing public. On `Ok` the staged key is
/// left for the caller to promote. On an error the staged key is discarded only when every
/// publish was answered (refused): after one whose answer never came the outcome is unknown and
/// the answer may still land, so it stays staged, and `master::refresh_master` promotes it once
/// the log's current master is its index (or discards it once the log is past it).
pub(crate) async fn publish_rotation<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    scid: &str,
    did: &str,
    seed: &Seed,
    revoke: &[KeyId],
    random: &dyn Random,
) -> Result<Published, ClientError> {
    let mut attempt = Attempt {
        owner: StageToken::new(random),
        staged: None,
        unanswered: false,
    };
    let published = publish_staged(executor, vault, scid, did, seed, revoke, &mut attempt).await;
    if published.is_err() && !attempt.unanswered {
        if let Some(index) = attempt.staged {
            // Best effort, and only the key this flow staged: another app sharing the vault
            // may have staged its own since. A key left staged is discarded by a refresh once
            // the log is past its index.
            let _ = vault.discard_staged(scid, index, Some(&attempt.owner));
        }
    }
    published
}

/// What [`publish_staged`] left behind: the index it staged last, and whether any of its
/// publishes went unanswered (that one may still land, so the staged key must stay), and the
/// token it stages with.
struct Attempt {
    owner: StageToken,
    staged: Option<u32>,
    unanswered: bool,
}

/// [`publish_rotation`]'s builds, staging each one's master before its publish.
#[allow(clippy::too_many_arguments)]
async fn publish_staged<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    scid: &str,
    did: &str,
    seed: &Seed,
    revoke: &[KeyId],
    attempt: &mut Attempt,
) -> Result<Published, ClientError> {
    let mut sent: Vec<(Event, Vec<KeyId>)> = Vec::new();
    let mut tries = 0;
    loop {
        let (head, chain) = log_of(executor, did).await?;
        if let Some((_, revoked)) = sent.iter().find(|(event, _)| landed(&chain, event)) {
            // It landed and its answer was lost: the head read now is past it.
            let head = head.ok_or_else(|| {
                ClientError::Protocol("the executor accepted no head for the log".into())
            })?;
            let index = current_index(&chain, seed)?;
            return Ok(Published {
                head,
                signer: master_of(seed, index)?,
                revoked: revoked.clone(),
                tries,
            });
        }
        if tries == MAX_TRIES {
            return Err(ClientError::LogBusy);
        }
        tries += 1;
        let index = current_index(&chain, seed)? + 1;
        let signer = master_of(seed, index)?;
        let salt = bs58::encode(words::salt(seed, index)).into_string();
        let (next, _) = words::next_commitment(seed, index + 1)?;
        let state = chain.state();
        let at = chain.head().seq;
        let revoked: Vec<KeyId> = revoke
            .iter()
            .filter(|id| state.keys.contains_key(*id) && state.key_valid_at(id, at))
            .cloned()
            .collect();
        let event = rotate_event(&chain, &signer, &salt, Some(next), &revoked)?;
        // The new master is in the vault before anything names it publicly.
        stage(vault, scid, seed, index, &attempt.owner)?;
        attempt.staged = Some(index);
        match executor.publish(std::slice::from_ref(&event)).await {
            Ok(head) => {
                return Ok(Published {
                    head,
                    signer,
                    revoked,
                    tries,
                })
            }
            Err(e) if e.code == 409 => {
                sent.push((event, revoked));
            }
            Err(e) if transient(&ClientError::Rpc(e.clone())) => {
                attempt.unanswered = true;
                sent.push((event, revoked));
            }
            Err(e) => return Err(e.into()),
        }
    }
}

/// The devices of `record` besides `own` that kept the master key: listed as holding it (`true`
/// or `"offered"`), valid at `chain`'s head, and let keep it by the log `events` (`chain`'s
/// events, [`may_keep_master`]). The record's device kind grants nothing: any linked device can
/// write the record.
pub(crate) fn kept_holders(
    record: &PrivateRecord,
    events: &[Event],
    chain: &KelChain,
    own: Option<&str>,
) -> Vec<KeyId> {
    let state = chain.state();
    let head = chain.head().seq;
    record
        .devices
        .iter()
        .filter(|(id, entry)| {
            Some(id.as_str()) != own
                && matches!(entry.holds_master, HoldsMaster::Yes | HoldsMaster::Offered)
                && state.keys.contains_key(*id)
                && state.key_valid_at(id, head)
                && may_keep_master(events, id)
        })
        .map(|(id, _)| id.clone())
        .collect()
}

/// This device after a rotation: the key id it has in the log, its whole entry when the
/// record should list it anew (a restore), else only its `holdsMaster`, and the master index
/// its vault keeps (`holdsMaster: true` with that index — the rotation's, or a later one the
/// vault kept first; `None` keeps none, `holdsMaster: false`).
pub(crate) struct ThisDevice {
    pub key_id: KeyId,
    pub entry: Option<DeviceEntry>,
    pub kept: Option<u32>,
}

/// Steps 4 and 5: the record's re-attestation first — `reader` (the new master, signing)
/// writes the record, attesting the recovery key again and marking `this`, before any key is
/// pushed: until that write lands the stored pair is stale, and every device write — a take's
/// among them — is refused [`ClientError::RecoveryNeedsReattest`] rather than seal the record
/// without it. When `this` kept the key, its entry is written through the custody gate
/// ([`master::mark_this`]): the index the vault keeps, fenced by the custody epoch and this
/// device's custody note, so a removal that released the copy since leaves it
/// `holdsMaster: false`. `owed` is the caller's intent ([`CustodyNote::MarkOwed`], written
/// before its store or promote): cleared once the mark lands under it. Then master `signer`
/// (now current) to every kept holder, then the pushes' marks. Answers the pushes and the
/// last record write.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn hand_over<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    signer: &KeySigner,
    reader: &Reader<'_>,
    this: Option<ThisDevice>,
    owed: Option<CustodyNote>,
    recovery_key: &Multikey,
    clock: &dyn Clock,
    random: &dyn Random,
) -> (
    Vec<(KeyId, Result<String, ClientError>)>,
    Result<u64, ClientError>,
) {
    let scid = match scid_of(did) {
        Ok(scid) => scid,
        Err(e) => return (Vec::new(), Err(e)),
    };
    // Read after the rotation landed: the keeper check sees the post-rotation log.
    let (events, chain) = match request::log_of(executor, did).await {
        Ok(log) => log,
        Err(e) => return (Vec::new(), Err(e)),
    };
    let opened = match record::read_as(executor, vault, did, &chain, reader, random).await {
        Ok(opened) => opened,
        Err(e) => return (Vec::new(), Err(e)),
    };
    let own = this.as_ref().map(|t| t.key_id.as_str());
    let holders = opened
        .map(|o| kept_holders(&o.record, &events, &chain, own))
        .unwrap_or_default();
    let index = signer.index();
    // The master-signed write comes before the pushes: it re-attests the recovery key with
    // the new master — while the stored pair is stale, a device that took the key could not
    // write the record at all — and it lists `this` device.
    let written = match this {
        // This device kept the key: its mark goes through the custody gate like a take's
        // ([`master::mark_this`]) — the vault's index, fenced by the custody epoch and this
        // device's note, decided again on the version the write builds on. A release that won
        // since the caller's intent leaves the entry `holdsMaster: false`: nothing here asserts
        // a copy the release gave up.
        Some(ThisDevice {
            key_id,
            entry,
            kept: Some(_),
        }) => {
            match master::mark_this(
                executor,
                vault,
                did,
                &scid,
                &key_id,
                reader,
                entry.as_ref(),
                Some(recovery_key),
                random,
            )
            .await
            {
                // Met when written under the caller's own intent; a note written since is
                // its writer's.
                Ok(marked) => match &owed {
                    Some(owed) => master::clear_own(vault, &scid, &key_id, owed.token(), &marked)
                        .map(|()| marked.version),
                    None => Ok(marked.version),
                },
                Err(e) => Err(e),
            }
        }
        // This device keeps no key: listed (or marked) `holdsMaster: false`.
        Some(ThisDevice {
            key_id,
            entry: Some(entry),
            kept: None,
        }) => {
            let change = Change::Device {
                key_id,
                entry: DeviceEntry {
                    holds_master: HoldsMaster::No,
                    master_index: None,
                    ..entry
                },
            };
            record::write_as(
                executor,
                vault,
                did,
                reader,
                &[change],
                Some(recovery_key),
                random,
            )
            .await
        }
        Some(ThisDevice {
            key_id,
            entry: None,
            kept: None,
        }) => {
            let change = Change::HoldsMaster {
                key_id,
                holds: HoldsMaster::No,
            };
            record::write_as(
                executor,
                vault,
                did,
                reader,
                &[change],
                Some(recovery_key),
                random,
            )
            .await
        }
        None => {
            record::write_as(
                executor,
                vault,
                did,
                reader,
                &[],
                Some(recovery_key),
                random,
            )
            .await
        }
    };
    if written.is_err() {
        return (Vec::new(), written);
    }
    let mut pushes = Vec::new();
    let mut marks = Vec::new();
    for key_id in holders {
        let sent = master::push(
            executor, vault, &events, &chain, did, signer, &key_id, clock, random,
        )
        .await;
        // Offered, unless the device took it and wrote so before the marking write lands:
        // that write builds on the newest version, and `Change::Offered` keeps a take of
        // `index` (or a later master) as it is. A push the store refused for good: not held.
        marks.push(match &sent {
            // On the retry list when transient: it goes when the store takes it.
            Ok(_) => Change::Offered {
                key_id: key_id.clone(),
                index,
            },
            Err(e) if transient(e) => Change::Offered {
                key_id: key_id.clone(),
                index,
            },
            Err(_) => Change::HoldsMaster {
                key_id: key_id.clone(),
                holds: HoldsMaster::No,
            },
        });
        pushes.push((key_id, sent));
    }
    let record = if marks.is_empty() {
        written
    } else {
        record::write_as(
            executor,
            vault,
            did,
            reader,
            &marks,
            Some(recovery_key),
            random,
        )
        .await
    };
    (pushes, record)
}

/// The vault keeps master `index` of `seed` for `scid` after the rotation to it landed:
/// promoted here or by another app's refresh first ([`promote`]). Nothing staged at `index` and
/// the vault keeping no master at or past it (another app's rotation re-staged the slot): the
/// words make the same key, so it is stored under the vault's never-go-back rule
/// ([`Vault::store_master`]; a key another app kept meanwhile wins). Answers the index the
/// vault keeps (`index`, or a later one's) and whether this call's own promote or store
/// installed it ([`master::settle`]).
pub(crate) fn acquire(
    vault: &dyn Vault,
    scid: &str,
    seed: &Seed,
    index: u32,
) -> Result<(u32, bool), ClientError> {
    match promote(vault, scid, index) {
        Ok(kept) => Ok(kept),
        Err(VaultError::NotFound) => {
            let key = words::master_key(seed, index)?;
            match vault.store_master(scid, index, Secret::new(key.to_bytes()))? {
                Stored::Stored => Ok((index, true)),
                Stored::AlreadyNewer { kept } => Ok((kept, false)),
            }
        }
        Err(e) => Err(e.into()),
    }
}

/// Rotates the master key of `did`, the executor's identity, with the 12 `words`, revoking the
/// device keys `revoke` in the same event; this device keeps master *n*+1, and every other app
/// that kept the master key is sent it. Refused before anything is published when the words do
/// not derive the current master, when a key of `revoke` is not in the log, or when `revoke`
/// names this device's own key.
#[allow(clippy::too_many_arguments)]
pub async fn rotate<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    words: &Words,
    revoke: &[KeyId],
    clock: &dyn Clock,
    random: &dyn Random,
) -> Result<Rotated, ClientError> {
    let scid = scid_of(did)?;
    let seed = words::seed(words);
    let status = executor.status().await?;
    if status.did.as_deref() != Some(did) {
        return Err(ClientError::Protocol(format!(
            "the executor's device key is no device of {did}"
        )));
    }
    let own = status
        .key_id
        .ok_or_else(|| ClientError::Protocol("the executor's device key is in no log".into()))?;
    let chain = chain_of(executor, did).await?;
    check_keys(&chain, did, revoke)?;
    if revoke.contains(&own) {
        return Err(ClientError::Protocol(
            "a device does not revoke its own key in a rotation it keeps".into(),
        ));
    }
    current_index(&chain, &seed)?;
    let published = publish_rotation(executor, vault, &scid, did, &seed, revoke, random).await?;
    let index = published.signer.index();
    // The rotation is public from here: what fails after it is reported, not returned.
    // The vault keeps the new master ([`acquire`]); `kept` names the index it keeps. The
    // intent before the promote or store, with the epoch read before it: a crash right
    // after them leaves the note that says this device owes its mark, and a release of this
    // device's copy written after it wins over it ([`master::note_owed`]). A vault that
    // refuses the note acquires nothing.
    let owed = vault
        .custody_epoch(&scid)
        .map_err(ClientError::from)
        .map(|epoch| CustodyNote::owed(index, epoch, random))
        .and_then(|owed| master::note_owed(vault, &scid, &own, &owed).map(|_| owed));
    // Settled right after: a receipt only for this rotation's own promote or store. A promote
    // that another app made first, a newer key kept, or a failed acquisition take the intent
    // out, so nothing here says this device holds a key it did not install.
    let (kept, owed) = match owed {
        Ok(owed) => {
            let kept = match acquire(vault, &scid, &seed, index) {
                Ok((kept, acquired)) => {
                    master::settle(vault, &scid, &own, &owed, acquired).map(|()| kept)
                }
                Err(e) => master::settle(vault, &scid, &own, &owed, false).and(Err(e)),
            };
            (kept, Some(owed))
        }
        Err(e) => (Err(e), None),
    };
    // The record's rewrite is the new master's: it re-attests the recovery key, which a
    // device-signed write cannot install, and marks this device through the custody gate.
    let secret = words::recovery_secret(&seed);
    let reader = Reader::Recovery {
        secret: &secret,
        master: &published.signer,
        clock,
    };
    let (pushes, record) = hand_over(
        executor,
        vault,
        did,
        &published.signer,
        &reader,
        Some(ThisDevice {
            key_id: own,
            entry: None,
            kept: kept.as_ref().ok().copied(),
        }),
        owed,
        &recovery_key_of(&seed),
        clock,
        random,
    )
    .await;
    Ok(Rotated {
        head: published.head,
        index,
        revoked: published.revoked,
        tries: published.tries,
        pushes,
        record,
        kept,
    })
}
