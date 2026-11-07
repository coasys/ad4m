//! `rotate`: replace the master key. A `rotate` reveals master *n*+1 and is signed by it alone,
//! so it needs the 12 words, not master *n*: a stolen device that holds master *n* cannot
//! rotate, and the words rotate even when no device holds a master key any more.
//!
//! 1. The words derive master *n*+1 at `m/7133'/0'/(n+1)'`, its salt, and the commitment to
//!    master *n*+2 the new `next` holds. *n* is the current master's index, and the words must
//!    derive the current master ([`ClientError::NotTheseWords`] otherwise).
//! 2. The event is published through the executor. **A `409`** (another event landed on the
//!    head first) reads the log again and builds the rotation again on the new head, at most
//!    [`MAX_TRIES`] times, then stops with [`ClientError::LogBusy`]. Each rebuild takes *n*
//!    from the new head (after a rotation by another app with the same words, the words derive
//!    the next master on), and revokes only the listed keys still valid there. A rotation that
//!    landed while its answer was lost is found in the log by its hash and not published again.
//! 3. This device's vault keeps master *n*+1 in place of master *n*.
//! 4. Master *n*+1 goes to every other device that kept the master key: each device the record
//!    lists as holding it (`true` or `"offered"`), valid at the head, that the log after the
//!    rotation lets keep it ([`ad4m_identity_core::link::may_keep_master`]; the record's device
//!    kind grants nothing, any linked device can write the record), sealed to its `enc` key in its joined inbox ([`super::master`], with the retry list). The store
//!    drops every message master *n* wrote when the rotation lands, so no device takes the
//!    replaced key after it.
//! 5. The record is written: each device sent the key is `holdsMaster: "offered"`, this device
//!    `true`, revoked devices leave it, and a record without the recovery key gets it.

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
use crate::vault::{KeySigner, MasterSigner, Vault};
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

/// The vault keeps master `index` of `seed` for `scid`, in place of the one it had.
pub(crate) fn keep(
    vault: &dyn Vault,
    scid: &str,
    seed: &Seed,
    index: u32,
) -> Result<(), ClientError> {
    let key = words::master_key(seed, index)?;
    vault.store_master(scid, index, Secret::new(key.to_bytes()))?;
    Ok(())
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
pub(crate) async fn publish_rotation<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
    seed: &Seed,
    revoke: &[KeyId],
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
        match executor.publish(std::slice::from_ref(&event)).await {
            Ok(head) => {
                return Ok(Published {
                    head,
                    signer,
                    revoked,
                    tries,
                })
            }
            Err(e) if e.code == 409 || transient(&ClientError::Rpc(e.clone())) => {
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

/// This device after a rotation: the key id it has in the log, and its whole entry when the
/// record should list it anew (a restore), else only `holdsMaster: true`.
pub(crate) struct ThisDevice {
    pub key_id: KeyId,
    pub entry: Option<DeviceEntry>,
}

/// Steps 4 and 5: master `signer` (now current) to every kept holder, then the record written
/// as `reader`. Answers the pushes and the record write.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn hand_over<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    did: &str,
    signer: &KeySigner,
    reader: &Reader<'_>,
    this: Option<ThisDevice>,
    recovery_key: &Multikey,
    clock: &dyn Clock,
    random: &dyn Random,
) -> (
    Vec<(KeyId, Result<String, ClientError>)>,
    Result<u64, ClientError>,
) {
    // Read after the rotation landed: the keeper check sees the post-rotation log.
    let (events, chain) = match request::log_of(executor, did).await {
        Ok(log) => log,
        Err(e) => return (Vec::new(), Err(e)),
    };
    let opened = match record::read_as(executor, did, &chain, reader, random).await {
        Ok(opened) => opened,
        Err(e) => return (Vec::new(), Err(e)),
    };
    let own = this.as_ref().map(|t| t.key_id.as_str());
    let holders = opened
        .map(|o| kept_holders(&o.record, &events, &chain, own))
        .unwrap_or_default();
    let mut pushes = Vec::new();
    let mut changes = Vec::new();
    for key_id in holders {
        let sent = master::push(
            executor, vault, &events, &chain, did, signer, &key_id, clock, random,
        )
        .await;
        let holds = match &sent {
            Ok(_) => HoldsMaster::Offered,
            // On the retry list: it goes when the store takes it.
            Err(e) if transient(e) => HoldsMaster::Offered,
            Err(_) => HoldsMaster::No,
        };
        changes.push(Change::HoldsMaster {
            key_id: key_id.clone(),
            holds,
        });
        pushes.push((key_id, sent));
    }
    match this {
        Some(ThisDevice {
            key_id,
            entry: Some(entry),
        }) => changes.push(Change::Device {
            key_id,
            entry: DeviceEntry {
                holds_master: HoldsMaster::Yes,
                ..entry
            },
        }),
        Some(ThisDevice {
            key_id,
            entry: None,
        }) => changes.push(Change::HoldsMaster {
            key_id,
            holds: HoldsMaster::Yes,
        }),
        None => {}
    }
    let record =
        record::write_as(executor, did, reader, &changes, Some(recovery_key), random).await;
    (pushes, record)
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
    let published = publish_rotation(executor, did, &seed, revoke).await?;
    let index = published.signer.index();
    keep(vault, &scid, &seed, index)?;
    let (pushes, record) = hand_over(
        executor,
        vault,
        did,
        &published.signer,
        &Reader::Executor,
        Some(ThisDevice {
            key_id: own,
            entry: None,
        }),
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
    })
}
