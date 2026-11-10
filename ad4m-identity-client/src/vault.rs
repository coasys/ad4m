//! The vault holds master keys; [`MasterSigner`] signs with one without giving its bytes out
//! Every use of a master key opens the vault once for a whole flow
//! ([`Vault::unlock`]: one prompt per change to the identity) and the flow's [`Unlocked`]
//! guard expires after [`MasterUse::ttl_s`] and wipes the key when it drops.
//!
//! Implementations: `DesktopVault` (the desktop shell), `PhoneVault` and `WebVault` (neither
//! written yet), and [`MemoryVault`] for tests (feature `test-vault`, never in a release build).

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::inbox::{seal, sign_payload, InboxAddress, InboxBody, InboxError};
use ad4m_identity_core::link::SignedBy;
use ad4m_identity_core::{Event, KeyId, Multikey};
use serde::{Deserialize, Serialize};
use zeroize::Zeroizing;

use crate::secret::Secret;
use crate::{Random, RandomRng};

/// How long one unlock serves a revocation or a push of the master key: five minutes, then the
/// surface asks again.
pub const UNLOCK_TTL_S: u64 = 300;

/// Why the surface asks for the unlock; shown in the OS prompt where the OS allows it. The
/// flows unlock for a link, a revocation and sending the master key to a device that keeps it
/// (`create` stores master #0 without an unlock).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MasterUse {
    Link,
    Revoke,
    SendMaster,
    /// The master key signs the private record itself: re-attesting its recovery key after a
    /// rotation, the write a device is refused until then.
    Record,
}

impl MasterUse {
    /// How long an unlock for this use lasts. A link signs its `add` up to the end of the
    /// store's confirm grace: the offer lives [`OFFER_TTL_S`](ad4m_identity_core::link::OFFER_TTL_S)
    /// from the unlock and the store takes the `add` [`CONFIRM_GRACE_S`](ad4m_identity_core::link::CONFIRM_GRACE_S)
    /// longer. A revocation and a push of the master key (one seal, one inbox put, the record)
    /// last [`UNLOCK_TTL_S`].
    pub fn ttl_s(self) -> u64 {
        match self {
            MasterUse::Link => {
                ad4m_identity_core::link::OFFER_TTL_S + ad4m_identity_core::link::CONFIRM_GRACE_S
            }
            MasterUse::Revoke | MasterUse::SendMaster | MasterUse::Record => UNLOCK_TTL_S,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum VaultError {
    #[error("the unlock was cancelled")]
    Cancelled,
    #[error("the device has no screen lock")]
    NoScreenLock,
    #[error("the vault is unavailable: {0}")]
    Unavailable(String),
    #[error("no master key for this identity")]
    NotFound,
    #[error("the unlock expired")]
    Expired,
    #[error("the vault's item was tampered with")]
    Tampered,
}

/// What [`Vault::store_master`] and [`Vault::promote_master`] did with the kept key.
///
/// The vault is shared by every app on the computer, so any of them may have kept a newer
/// master since the caller read [`Vault::master_index`]: the writes are generation-aware,
/// and a kept key never goes back a generation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stored {
    /// The key is kept now: none was kept, or the kept one was at this or a lower index.
    Stored,
    /// Nothing changed: the vault keeps a key at a higher index, installed by another app
    /// since the caller checked. Its authenticated index.
    AlreadyNewer { kept: u32 },
}

/// What [`Vault::forget_master`] did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Forgot {
    /// The kept key was at the index named and is forgotten, or none was kept.
    Forgot,
    /// Nothing changed: the vault keeps a key at another index, installed by another app
    /// sharing the vault since the caller checked. Its authenticated index.
    Kept(u32),
}

/// Signs with a master key; never gives its bytes out.
pub trait MasterSigner: Signer {
    /// The master key's index *n*.
    fn index(&self) -> u32;
    /// This master key as `InboxBody::Master { did, index, secret }` in a payload to the device
    /// key `to`, signed by this key at `by`, sealed to the device's `enc` key `to_enc` for
    /// `address`. The secret leaves only sealed. The copies this crate and the core make on the
    /// way are wiped, whether the seal succeeds or fails: the encoded secret in the body, the
    /// JSON values built to sign and seal the payload, the signed bytes and the payload's text
    /// before the seal (both written once at their length, so no shorter copy is left behind
    /// as they grow). What the libraries underneath keep is not
    /// covered: the hash state of the Ed25519 signature over the payload, the cipher's working
    /// buffers, and copies on the stack.
    #[allow(clippy::too_many_arguments)]
    fn seal_self(
        &self,
        did: &str,
        to: &str,
        to_enc: &str,
        address: &InboxAddress,
        by: SignedBy,
        sent_at: u64,
        random: &dyn Random,
    ) -> Result<String, InboxError>;
}

/// One unlocked master key for one flow. Expires [`MasterUse::ttl_s`] after the unlock; the key
/// is wiped when the guard drops.
pub struct Unlocked {
    signer: Box<dyn MasterSigner + Send + Sync>,
    expires_at: u64,
}

impl Unlocked {
    /// The unlock of `signer` at `now` for `why`; it lasts `why`'s [`MasterUse::ttl_s`].
    pub fn new(signer: Box<dyn MasterSigner + Send + Sync>, why: MasterUse, now: u64) -> Self {
        Unlocked {
            signer,
            expires_at: now.saturating_add(why.ttl_s()),
        }
    }

    /// The signer, while the unlock lasts at `now`.
    pub fn signer(&self, now: u64) -> Result<&dyn MasterSigner, VaultError> {
        if now >= self.expires_at {
            return Err(VaultError::Expired);
        }
        Ok(self.signer.as_ref())
    }
}

/// The custody of master keys on one surface.
///
/// `create` keeps master #0 and the co-signed incept here *before* it publishes the incept
/// ([`Vault::store_pending_incept`]), and forgets the incept once the publish is confirmed
/// ([`Vault::incept_published`]): a publish whose answer is lost is finished later from the
/// vault (`flows::create::resume`), with the same master key and the same identity.
///
/// A rotation (`rotate`, `revoke_and_rotate`, `answer_with_words`, a restore) stages master
/// *n*+1 here *before* it publishes the `rotate` ([`Vault::stage_master`]), and promotes it to
/// the kept master key once the publish landed ([`Vault::promote_master`]). A staged key is
/// never used to sign and never replaces the kept one by itself: a publish refused for good
/// discards it ([`Vault::discard_staged`], with the flow's [`StageToken`], so it never discards
/// a key another app staged since); a flow cut off between the publish and the promote leaves
/// it staged. `flows::master::refresh_master`, run by any app sharing the vault, promotes it
/// when the log's current master index is the staged one, leaves it while the staged index is
/// ahead of the log (a publish may be on its way), and discards it once the log is past it. A
/// flow whose promote answers `NotFound` while the vault keeps its index was promoted by
/// another app first: the key is kept. Promotion goes by index alone: the words derive one key
/// per index, so a key staged at the log's current index by any app is the current master.
///
/// **The kept key never goes back a generation.** Since every app on the computer shares the
/// vault, any of them may keep a newer master between a flow's read of [`Vault::master_index`]
/// and its write. The writes are atomic and generation-aware, so a caller never has to
/// re-check after them: [`Vault::store_master`] and [`Vault::promote_master`] report
/// [`Stored::AlreadyNewer`] with the index kept rather than replace a newer key, and
/// [`Vault::forget_master`] is a compare-and-delete on the index the caller checked.
/// Everything that names the master index this device holds (the `Taken`/`Promoted`/`kept`
/// answers, the record's `masterIndex`) names the index the vault keeps, not the one the
/// flow set out to install.
///
/// **A forget is bound to the custody state a flow checked, not to a generation.** The index
/// names the key's generation; a duplicate delivery of the same master, taken by another app
/// between a flow's check and its forget, keeps the index and changes custody. So a flow
/// reads [`Vault::custody_epoch`] before the checks it makes about who holds the key, and
/// forgets with [`Vault::forget_master_at`], which deletes only while that epoch is unchanged.
///
/// **What a persistent vault owes.** Every app on the computer shares one store, so each
/// operation here is linearizable across every handle and process on that store: the
/// authenticated comparison and the mutation it decides are one step, under a cross-process
/// lock or a transaction of the store, never a read in one process and a write in another.
/// `Ok` from a write ([`Vault::store_master`], [`Vault::stage_master`],
/// [`Vault::promote_master`]) means the key is durable: a crash after it leaves the key where
/// the answer said, and a take acknowledges its inbox message only after such an `Ok`. A
/// promote is crash-safe: after a crash in it, the key is staged or kept, never neither. An
/// index or epoch the store cannot vouch for is an error or `None`, never a guess. The
/// in-memory [`MemoryVault`] (tests) is exempt from durability and from the cross-process
/// lock, since one process holds it.
///
/// **The custody note** ([`Vault::custody_note`], [`CustodyNote`]): one small, non-secret value
/// per identity *and per device key* — the key id of the executor the app speaks through, the
/// same key the record's device entry is keyed by. The key is shared by every app on the
/// computer; the note is not: app A's release and app B's take on the same vault are two device
/// entries in the record and two notes here, so neither overwrites the other. Apps that speak
/// through one executor are one device and share its note, as they share its entry. The note
/// is the device's own account of its custody decisions, the one thing the vault and the record
/// cannot tell afterwards.
///
/// The note is a stack of layers. Each operation puts one layer on top, and that layer keeps
/// the note below it (`restores`). Each layer carries the [`NoteToken`] of the operation that
/// wrote it. An operation changes or removes only its own layer, so a take, a removal and a
/// refresh of the same device never clear or put back a layer that another one wrote, even
/// one that looks the same. The layers:
///
/// - [`CustodyNote::MarkOwed`] with `acquired: false` (an intent): this device will store or
///   promote master `index`. A take, a promote and a rotation write it **before** they store
///   or promote the key, and so before a take acknowledges its inbox message. An intent proves
///   nothing: its store can fail, or another app can install the key. A mark never says
///   `holdsMaster: true` from an intent, and a release below an intent stays in effect.
/// - `MarkOwed` with `acquired: true` (a receipt): this operation's own
///   [`Vault::store_master`] or [`Vault::promote_master`] answered [`Stored::Stored`]. Only a
///   receipt lets a mark say `holdsMaster: true`, for its own `index`, and only while the vault
///   keeps that index ([`CustodyNote::landed`]). The receipt is cleared once the fenced mark
///   has met it. A crash, a cancelled unlock or a failed write after the receipt leaves the
///   obligation here, never only in memory. Another holder re-attesting the record's recovery
///   pair erases nothing of it.
/// - [`CustodyNote::Released`]: this device chose to give its copy up. `remove_master` writes
///   it **before** it publishes `holdsMaster: false`, and no refresh, repair or restart
///   re-marks the device while it stands — though the shared vault may keep the key another
///   app installed. A receipt of a new acquisition by this device defeats it: a release below
///   the receipt, and a release above it whose epoch is not later than the receipt's intent.
///   The store moved the custody epoch past such a release, so the removal's compare-and-delete
///   forget cannot take the new key.
///
/// The standing of a note ([`CustodyNote::standing`]) comes from the top layer down: open
/// intents are passed over, and the first release or receipt decides. No note: nothing owed
/// and nothing released.
///
/// The writes are compare-and-swap ([`Vault::swap_custody_note`]) on the whole note, all its
/// layers included. The vault keeps the note as an opaque value: it never reads the layers.
pub trait Vault: Send + Sync {
    /// Stores master `index` for `scid` as the kept key, under the never-go-back rule: a key
    /// kept at a *higher* index is never replaced by a lower one — the call answers
    /// [`Stored::AlreadyNewer`] with the index kept, changing nothing (storing the kept
    /// index again is `Stored`: the words derive the same key). Used where the key is
    /// already current in the log (`create`'s #0 before its incept, a key taken from the
    /// inbox, a rotation's key stored from the words).
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: Secret<[u8; 32]>,
    ) -> Result<Stored, VaultError>;
    /// The index of the master key kept for `scid`. **Authenticated, from the item**: the index
    /// sealed with the key itself (not a cache beside it), so it names the key that
    /// [`Vault::unlock`] would give. `None` when the vault keeps no key for `scid`, *or* when
    /// it cannot tell which index the kept key has (a cache it cannot vouch for): a caller
    /// never forgets a key on `None`. Reads no secret, so it needs no unlock.
    fn master_index(&self, scid: &str) -> Result<Option<u32>, VaultError>;
    /// Stages master `index` of `scid` before the rotation that makes it current is published,
    /// with `owner`, the staging flow's token, kept beside it (not secret; authenticated like the
    /// index). Replaces a staged key of `scid` (any index, any owner); leaves the kept key as it
    /// is. A staged key signs nothing and is not what [`Vault::unlock`] gives. Must be durable
    /// when it answers `Ok`: the rotation is published only after.
    fn stage_master(
        &self,
        scid: &str,
        index: u32,
        secret: Secret<[u8; 32]>,
        owner: &StageToken,
    ) -> Result<(), VaultError>;
    /// The index of the key staged for `scid`, authenticated as [`Vault::master_index`];
    /// `None` when none is staged. Needs no unlock.
    fn staged_index(&self, scid: &str) -> Result<Option<u32>, VaultError>;
    /// The rotation to staged master `index` landed: the staged key becomes the kept master key
    /// of `scid`, replacing the old one, and is no longer staged — under the never-go-back
    /// rule of [`Vault::store_master`]: a key kept at a *higher* index wins
    /// ([`Stored::AlreadyNewer`]), and the staged key is discarded, since it can never become
    /// the kept one. `NotFound` when no key is staged at `index`. Needs no unlock (no secret
    /// leaves the vault).
    fn promote_master(&self, scid: &str, index: u32) -> Result<Stored, VaultError>;
    /// The rotation to staged master `index` did not land: the staged key is forgotten when it
    /// is staged at `index` and, with `owner` given, was staged with that token (a flow
    /// discarding its own key); `None` discards it whoever staged it (a refresh discarding a
    /// key the log is past). No error when none is staged, one at another index, or one with
    /// another owner (each left as it is).
    fn discard_staged(
        &self,
        scid: &str,
        index: u32,
        owner: Option<&StageToken>,
    ) -> Result<(), VaultError>;
    /// Forgets the kept master key of `scid`, only when it is still the key at `index`: a
    /// compare-and-delete, so a key another app sharing the vault installed at another index
    /// since the caller read [`Vault::master_index`] is never removed ([`Forgot::Kept`] names
    /// its index, authenticated; [`Forgot::Forgot`] covers none kept as well). The person
    /// removed it from this device, or it was replaced by a rotation. A caller never forgets
    /// on a `master_index` of `None`. A staged key stays: it may belong to a rotation in
    /// flight in another app sharing the vault, and only [`Vault::discard_staged`] removes it.
    fn forget_master(&self, scid: &str, index: u32) -> Result<Forgot, VaultError>;
    /// The custody epoch of `scid`: a value that moves on every successful [`Vault::store_master`],
    /// [`Vault::promote_master`] and forget of `scid`'s kept key, a store or promote of the SAME
    /// index with the same bytes included (a duplicate delivery re-stores the same key, and a
    /// removal that read the epoch before it must not then forget it), never otherwise
    /// ([`Stored::AlreadyNewer`], a refused promote and [`Forgot::Kept`] leave it), and never
    /// repeats within the store's life. Use a monotonic write counter kept with the item under
    /// the same lock as the writes; never a digest of the key or its sealed bytes, which repeats
    /// for the same key. Needs no unlock.
    fn custody_epoch(&self, scid: &str) -> Result<u64, VaultError>;
    /// Forgets the kept master key of `scid` only while its custody epoch is still `epoch`
    /// ([`Vault::custody_epoch`], read before the caller's checks), as one atomic step: a key
    /// installed since, at any index, the same one included, is never the one forgotten
    /// ([`Forgot::Kept`] names the index kept then). None kept is [`Forgot::Forgot`].
    fn forget_master_at(&self, scid: &str, epoch: u64) -> Result<Forgot, VaultError>;
    /// One unlock (one prompt where the OS needs it) for a flow that uses the master key `why`.
    /// `now` starts the unlock's [`MasterUse::ttl_s`] (pass `why` to [`Unlocked::new`]).
    fn unlock(&self, scid: &str, why: MasterUse, now: u64) -> Result<Unlocked, VaultError>;
    /// Keeps the co-signed incept of a new identity (`incept.scid`), whose master #0 the vault
    /// holds, until [`Vault::incept_published`]; replaces a kept incept of the same identity.
    /// The incept holds public keys and proofs only, no secret.
    fn store_pending_incept(&self, incept: &Event) -> Result<(), VaultError>;
    /// The incepts kept and not yet confirmed published.
    fn pending_incepts(&self) -> Result<Vec<Event>, VaultError>;
    /// The store holds `scid`'s incept: the vault forgets it (no error when it holds none).
    fn incept_published(&self, scid: &str) -> Result<(), VaultError>;
    /// Keeps `push` on the retry list before the master key is put into the device's inbox
    /// (`flows::master`); keeping one already listed is no change.
    fn keep_push(&self, push: &PendingPush) -> Result<(), VaultError>;
    /// The pushes kept and not yet confirmed sent.
    fn pending_pushes(&self) -> Result<Vec<PendingPush>, VaultError>;
    /// The push is done (the store took it) or no longer wanted: the vault forgets it (no error
    /// when it holds none).
    fn push_done(&self, push: &PendingPush) -> Result<(), VaultError>;
    /// Notes `version` of `scid`'s private record as read and answers the highest version read
    /// before it (the high-water mark, `None` before the first read). A store can serve back
    /// an older record with a receipt that still verifies, so the reader refuses a version
    /// under the answer (`crate::record`).
    fn record_seen(&self, scid: &str, version: u64) -> Result<Option<u64>, VaultError>;
    /// Forgets the versions of `scid`'s record seen so far: a caller that means to read an
    /// older version on purpose resets the mark first.
    fn record_seen_reset(&self, scid: &str) -> Result<(), VaultError>;
    /// The custody note of device `key_id` for `scid` ([`CustodyNote`]); `None` when it has
    /// none. Not secret, needs no unlock. Kept per (`scid`, `key_id`), never per `scid` alone:
    /// every app sharing the vault has its own.
    fn custody_note(&self, scid: &str, key_id: &str) -> Result<Option<CustodyNote>, VaultError>;
    /// Sets the custody note of device `key_id` for `scid` to `note` (`None` clears it) only
    /// while it is still `expected` (`None`: none kept), as one atomic step under the same
    /// cross-process lock as the vault's other writes; answers whether it was set. "Still
    /// `expected`" compares the whole note, every layer below the top included: keep the note
    /// as its serde JSON and compare the values, never only the top layer. Durable when
    /// it answers `true`: a take acknowledges its inbox message, and a removal publishes
    /// `holdsMaster: false`, only after.
    fn swap_custody_note(
        &self,
        scid: &str,
        key_id: &str,
        expected: Option<&CustodyNote>,
        note: Option<&CustodyNote>,
    ) -> Result<bool, VaultError>;
}

/// What one device has decided about its custody of an identity's master key, kept in the vault
/// beside the key ([`Vault::custody_note`]); see the [`Vault`] docs for its lifecycle.
///
/// Every note carries the [`NoteToken`] of the operation that wrote it, drawn at random for
/// that operation alone: two operations never write equal notes, even two removals of one
/// device that read the same custody epoch.
///
/// **A note is a stack.** Each operation puts its note on top of the note in place and keeps
/// that note below it (`restores`). An operation changes only its own layer, found by its
/// token: it sets its own receipt, or it takes its own layer out wherever it is. So a failed
/// operation never puts back a note that another operation owns, and it never removes a note
/// that another operation wrote. The flows decide from the layers in order from the top
/// ([`CustodyNote::standing`]).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "camelCase")]
pub enum CustodyNote {
    /// The device is acquiring master `index` (a take's store, a promote, a rotation's or a
    /// restore's store) and owes the record its mark. Written **before** the store or promote
    /// (an intent, `acquired: false`). `epoch` is the custody epoch read before the intent was
    /// written. The operation sets `acquired` (the receipt) only after its own
    /// [`Vault::store_master`] or [`Vault::promote_master`] answered [`Stored::Stored`].
    /// Another device's store, a promote that another app made first, and
    /// [`Stored::AlreadyNewer`] never set it. Only a receipt lets a mark say `holdsMaster:
    /// true`. Cleared once the mark it owes has landed.
    MarkOwed {
        index: u32,
        epoch: u64,
        token: NoteToken,
        /// The receipt: this operation's own store or promote of `index` landed.
        #[serde(default)]
        acquired: bool,
        /// The note this one was written over.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        restores: Option<Box<CustodyNote>>,
    },
    /// The device released its copy (`remove_master`) after it read custody epoch `epoch`.
    /// Its compare-and-delete forget is bound to that epoch. An acquisition that stores after
    /// the release moves the epoch, so the forget can no longer take the new key.
    Released {
        epoch: u64,
        token: NoteToken,
        /// The note this one was written over.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        restores: Option<Box<CustodyNote>>,
    },
}

/// What a [`CustodyNote`] says about this device's copy ([`CustodyNote::standing`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Standing {
    /// A release is the first layer from the top that is not an open intent: the device gave
    /// its copy up. The mark is `holdsMaster: false`.
    Released,
    /// A receipt for master `index` is the first such layer: this device's own store or
    /// promote of `index` landed, and no release was written after it.
    Acquired(u32),
    /// Only open intents (no receipt yet): nothing proves that this device keeps a key.
    Pending,
}

impl CustodyNote {
    /// An owed mark for master `index`, the intent of an acquisition about to store or promote
    /// it; `epoch` is the custody epoch read before it. A fresh token from `random`. No receipt
    /// yet, nothing below it.
    pub fn owed(index: u32, epoch: u64, random: &dyn Random) -> Self {
        CustodyNote::MarkOwed {
            index,
            epoch,
            token: NoteToken::new(random),
            acquired: false,
            restores: None,
        }
    }

    /// A release by a removal that read custody epoch `epoch`. A fresh token from `random`.
    pub fn released(epoch: u64, random: &dyn Random) -> Self {
        CustodyNote::Released {
            epoch,
            token: NoteToken::new(random),
            restores: None,
        }
    }

    /// The token of the operation that wrote this layer.
    pub fn token(&self) -> NoteToken {
        match self {
            CustodyNote::MarkOwed { token, .. } | CustodyNote::Released { token, .. } => *token,
        }
    }

    /// The note this one was written over.
    pub fn restores(&self) -> Option<&CustodyNote> {
        match self {
            CustodyNote::MarkOwed { restores, .. } | CustodyNote::Released { restores, .. } => {
                restores.as_deref()
            }
        }
    }

    /// The layers from the top, each without the notes below it.
    pub fn layers(&self) -> Vec<CustodyNote> {
        let mut layers = Vec::new();
        let mut next = Some(self);
        while let Some(note) = next {
            layers.push(note.bare());
            next = note.restores();
        }
        layers
    }

    /// The note made of `layers` (from the top); `None` for no layer.
    pub fn stacked(layers: Vec<CustodyNote>) -> Option<CustodyNote> {
        layers
            .into_iter()
            .rev()
            .fold(None, |below, layer| Some(layer.over(below)))
    }

    /// This layer written over `below`.
    pub fn over(self, below: Option<CustodyNote>) -> CustodyNote {
        let below = below.map(Box::new);
        match self {
            CustodyNote::MarkOwed {
                index,
                epoch,
                token,
                acquired,
                ..
            } => CustodyNote::MarkOwed {
                index,
                epoch,
                token,
                acquired,
                restores: below,
            },
            CustodyNote::Released { epoch, token, .. } => CustodyNote::Released {
                epoch,
                token,
                restores: below,
            },
        }
    }

    fn bare(&self) -> CustodyNote {
        self.clone().over(None)
    }

    /// What the note says, from the top layer down: open intents are passed over, and the
    /// first release or receipt decides.
    pub fn standing(&self) -> Standing {
        let mut next = Some(self);
        while let Some(note) = next {
            match note {
                CustodyNote::Released { .. } => return Standing::Released,
                CustodyNote::MarkOwed {
                    index,
                    acquired: true,
                    ..
                } => return Standing::Acquired(*index),
                CustodyNote::MarkOwed { .. } => next = note.restores(),
            }
        }
        Standing::Pending
    }

    /// The receipt that decides [`CustodyNote::standing`], when it is one.
    pub fn receipt(&self) -> Option<CustodyNote> {
        let Standing::Acquired(_) = self.standing() else {
            return None;
        };
        self.layers()
            .into_iter()
            .find(|layer| matches!(layer, CustodyNote::MarkOwed { acquired: true, .. }))
    }

    /// Whether this note proves that this device keeps the master key in `vault` for `scid`:
    /// its standing is a receipt for an index, and the vault keeps exactly that index. `false`
    /// for a release and for open intents. An intent is never proof: the key that the vault
    /// keeps can be another device's acquisition on the same vault.
    pub fn landed(&self, vault: &dyn Vault, scid: &str) -> Result<bool, VaultError> {
        let Standing::Acquired(index) = self.standing() else {
            return Ok(false);
        };
        Ok(vault.master_index(scid)? == Some(index))
    }
}

/// The operation that wrote a [`CustodyNote`]: 16 random bytes, drawn once per take, promote,
/// rotation, restore or removal. Not secret.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NoteToken(pub [u8; 16]);

impl NoteToken {
    /// A fresh token from `random`.
    pub fn new(random: &dyn Random) -> Self {
        let mut bytes = [0u8; 16];
        random.fill(&mut bytes);
        NoteToken(bytes)
    }
}

/// Who staged a master key ([`Vault::stage_master`]): an opaque token a rotating flow draws at
/// random once, so that its [`Vault::discard_staged`] forgets only a key it staged itself, never
/// one another app sharing the vault staged after it. Not secret.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StageToken(pub [u8; 16]);

impl StageToken {
    /// A fresh token from `random`.
    pub fn new(random: &dyn Random) -> Self {
        let mut bytes = [0u8; 16];
        random.fill(&mut bytes);
        StageToken(bytes)
    }
}

/// A master key to put into a device's inbox: the identity and the device's key id. The retry
/// list keeps it until the store takes it; each try seals the master key current then.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PendingPush {
    pub scid: String,
    pub key_id: KeyId,
}

/// A master key in memory: an Ed25519 key that zeroes on drop.
pub struct KeySigner {
    key: ed25519_dalek::SigningKey,
    index: u32,
}

impl KeySigner {
    pub fn new(secret: &[u8; 32], index: u32) -> Self {
        KeySigner {
            key: ed25519_dalek::SigningKey::from_bytes(secret),
            index,
        }
    }
}

impl Signer for KeySigner {
    fn key(&self) -> Multikey {
        Signer::key(&self.key)
    }
    fn sign(&self, bytes: &[u8]) -> [u8; 64] {
        Signer::sign(&self.key, bytes)
    }
}

impl MasterSigner for KeySigner {
    fn index(&self) -> u32 {
        self.index
    }

    fn seal_self(
        &self,
        did: &str,
        to: &str,
        to_enc: &str,
        address: &InboxAddress,
        by: SignedBy,
        sent_at: u64,
        random: &dyn Random,
    ) -> Result<String, InboxError> {
        let secret = Zeroizing::new(self.key.to_bytes());
        let body = InboxBody::Master {
            did: did.to_string(),
            index: self.index,
            secret: bs58::encode(secret.as_ref()).into_string(),
        };
        let mut payload = sign_payload(to.to_string(), body, sent_at, by, self)?;
        let sealed = seal(to_enc, address, &payload, &mut RandomRng(random));
        payload.body.wipe();
        sealed
    }
}

#[cfg(any(test, feature = "test-vault"))]
pub use memory::MemoryVault;

#[cfg(any(test, feature = "test-vault"))]
mod memory {
    use std::collections::HashMap;
    use std::sync::Mutex;

    use super::*;

    /// Each identity's master index and key.
    type Keys = HashMap<String, (u32, Secret<[u8; 32]>)>;
    /// Each identity's staged master index, key and owner.
    type Staged = HashMap<String, (u32, Secret<[u8; 32]>, StageToken)>;

    /// Master keys in memory, for tests. Records every unlock so tests can count prompts.
    #[derive(Default)]
    pub struct MemoryVault {
        keys: Mutex<Keys>,
        /// Each identity's custody epoch: bumped on every change of its kept key, under the
        /// `keys` lock.
        epochs: Mutex<HashMap<String, u64>>,
        staged: Mutex<Staged>,
        pending: Mutex<Vec<Event>>,
        pushes: Mutex<Vec<PendingPush>>,
        unlocks: Mutex<Vec<MasterUse>>,
        record_seen: Mutex<HashMap<String, u64>>,
        /// Each (identity, device key id)'s custody note.
        notes: Mutex<HashMap<(String, String), CustodyNote>>,
    }

    impl MemoryVault {
        pub fn new() -> Self {
            Self::default()
        }

        /// Every unlock so far, in order.
        pub fn unlocks(&self) -> Vec<MasterUse> {
            self.unlocks.lock().map(|u| u.clone()).unwrap_or_default()
        }

        /// The kept key of `scid` changed: its custody epoch moves on. Called with the `keys`
        /// lock held, so the change and the epoch are one step.
        fn bump(&self, scid: &str) -> Result<(), VaultError> {
            *self
                .epochs
                .lock()
                .map_err(poisoned)?
                .entry(scid.to_string())
                .or_default() += 1;
            Ok(())
        }
    }

    fn poisoned<T>(_: T) -> VaultError {
        VaultError::Unavailable("the memory vault's lock is poisoned".into())
    }

    impl Vault for MemoryVault {
        fn store_master(
            &self,
            scid: &str,
            index: u32,
            secret: Secret<[u8; 32]>,
        ) -> Result<Stored, VaultError> {
            let mut keys = self.keys.lock().map_err(poisoned)?;
            if let Some((kept, _)) = keys.get(scid) {
                if *kept > index {
                    return Ok(Stored::AlreadyNewer { kept: *kept });
                }
            }
            keys.insert(scid.to_string(), (index, secret));
            self.bump(scid)?;
            Ok(Stored::Stored)
        }

        fn master_index(&self, scid: &str) -> Result<Option<u32>, VaultError> {
            Ok(self
                .keys
                .lock()
                .map_err(poisoned)?
                .get(scid)
                .map(|(index, _)| *index))
        }

        fn forget_master(&self, scid: &str, index: u32) -> Result<Forgot, VaultError> {
            let mut keys = self.keys.lock().map_err(poisoned)?;
            match keys.get(scid).map(|(kept, _)| *kept) {
                Some(kept) if kept != index => Ok(Forgot::Kept(kept)),
                _ => {
                    if keys.remove(scid).is_some() {
                        self.bump(scid)?;
                    }
                    Ok(Forgot::Forgot)
                }
            }
        }

        fn custody_epoch(&self, scid: &str) -> Result<u64, VaultError> {
            let _keys = self.keys.lock().map_err(poisoned)?;
            Ok(self
                .epochs
                .lock()
                .map_err(poisoned)?
                .get(scid)
                .copied()
                .unwrap_or(0))
        }

        fn forget_master_at(&self, scid: &str, epoch: u64) -> Result<Forgot, VaultError> {
            let mut keys = self.keys.lock().map_err(poisoned)?;
            let now = self
                .epochs
                .lock()
                .map_err(poisoned)?
                .get(scid)
                .copied()
                .unwrap_or(0);
            match keys.get(scid).map(|(kept, _)| *kept) {
                Some(kept) if now != epoch => Ok(Forgot::Kept(kept)),
                Some(_) => {
                    keys.remove(scid);
                    self.bump(scid)?;
                    Ok(Forgot::Forgot)
                }
                None => Ok(Forgot::Forgot),
            }
        }

        fn stage_master(
            &self,
            scid: &str,
            index: u32,
            secret: Secret<[u8; 32]>,
            owner: &StageToken,
        ) -> Result<(), VaultError> {
            let mut staged = self.staged.lock().map_err(poisoned)?;
            staged.insert(scid.to_string(), (index, secret, *owner));
            Ok(())
        }

        fn staged_index(&self, scid: &str) -> Result<Option<u32>, VaultError> {
            Ok(self
                .staged
                .lock()
                .map_err(poisoned)?
                .get(scid)
                .map(|(index, _, _)| *index))
        }

        fn promote_master(&self, scid: &str, index: u32) -> Result<Stored, VaultError> {
            let mut staged = self.staged.lock().map_err(poisoned)?;
            if staged.get(scid).map(|(i, _, _)| *i) != Some(index) {
                return Err(VaultError::NotFound);
            }
            let mut keys = self.keys.lock().map_err(poisoned)?;
            if let Some((kept, _)) = keys.get(scid) {
                if *kept > index {
                    // A newer key is kept already: the staged one can never become it.
                    staged.remove(scid);
                    return Ok(Stored::AlreadyNewer { kept: *kept });
                }
            }
            let (index, secret, _) = staged.remove(scid).ok_or(VaultError::NotFound)?;
            keys.insert(scid.to_string(), (index, secret));
            self.bump(scid)?;
            Ok(Stored::Stored)
        }

        fn discard_staged(
            &self,
            scid: &str,
            index: u32,
            owner: Option<&StageToken>,
        ) -> Result<(), VaultError> {
            let mut staged = self.staged.lock().map_err(poisoned)?;
            let theirs = staged
                .get(scid)
                .is_some_and(|(i, _, by)| *i == index && owner.is_none_or(|owner| owner == by));
            if theirs {
                staged.remove(scid);
            }
            Ok(())
        }

        fn unlock(&self, scid: &str, why: MasterUse, now: u64) -> Result<Unlocked, VaultError> {
            let keys = self.keys.lock().map_err(poisoned)?;
            let (index, secret) = keys.get(scid).ok_or(VaultError::NotFound)?;
            self.unlocks.lock().map_err(poisoned)?.push(why);
            Ok(Unlocked::new(
                Box::new(KeySigner::new(secret.expose(), *index)),
                why,
                now,
            ))
        }

        fn store_pending_incept(&self, incept: &Event) -> Result<(), VaultError> {
            let mut pending = self.pending.lock().map_err(poisoned)?;
            pending.retain(|e| e.scid != incept.scid);
            pending.push(incept.clone());
            Ok(())
        }

        fn pending_incepts(&self) -> Result<Vec<Event>, VaultError> {
            Ok(self.pending.lock().map_err(poisoned)?.clone())
        }

        fn incept_published(&self, scid: &str) -> Result<(), VaultError> {
            self.pending
                .lock()
                .map_err(poisoned)?
                .retain(|e| e.scid != scid);
            Ok(())
        }

        fn keep_push(&self, push: &PendingPush) -> Result<(), VaultError> {
            let mut pushes = self.pushes.lock().map_err(poisoned)?;
            if !pushes.contains(push) {
                pushes.push(push.clone());
            }
            Ok(())
        }

        fn pending_pushes(&self) -> Result<Vec<PendingPush>, VaultError> {
            Ok(self.pushes.lock().map_err(poisoned)?.clone())
        }

        fn push_done(&self, push: &PendingPush) -> Result<(), VaultError> {
            self.pushes.lock().map_err(poisoned)?.retain(|p| p != push);
            Ok(())
        }

        fn record_seen(&self, scid: &str, version: u64) -> Result<Option<u64>, VaultError> {
            let mut seen = self.record_seen.lock().map_err(poisoned)?;
            let before = seen.get(scid).copied();
            let high = before.map_or(version, |h| h.max(version));
            seen.insert(scid.to_string(), high);
            Ok(before)
        }

        fn record_seen_reset(&self, scid: &str) -> Result<(), VaultError> {
            self.record_seen.lock().map_err(poisoned)?.remove(scid);
            Ok(())
        }

        fn custody_note(
            &self,
            scid: &str,
            key_id: &str,
        ) -> Result<Option<CustodyNote>, VaultError> {
            Ok(self
                .notes
                .lock()
                .map_err(poisoned)?
                .get(&(scid.to_string(), key_id.to_string()))
                .cloned())
        }

        fn swap_custody_note(
            &self,
            scid: &str,
            key_id: &str,
            expected: Option<&CustodyNote>,
            note: Option<&CustodyNote>,
        ) -> Result<bool, VaultError> {
            let mut notes = self.notes.lock().map_err(poisoned)?;
            let slot = (scid.to_string(), key_id.to_string());
            if notes.get(&slot) != expected {
                return Ok(false);
            }
            match note {
                Some(note) => notes.insert(slot, note.clone()),
                None => notes.remove(&slot),
            };
            Ok(true)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn an_unlock_serves_one_flow_until_it_expires() {
            let vault = MemoryVault::new();
            assert!(matches!(
                vault.unlock("Qm", MasterUse::Link, 0),
                Err(VaultError::NotFound)
            ));
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::Revoke, 100).unwrap();
            let signer = unlocked.signer(100 + UNLOCK_TTL_S - 1).unwrap();
            assert_eq!(signer.index(), 0);
            assert_eq!(
                signer.key(),
                Signer::key(&ed25519_dalek::SigningKey::from_bytes(&[3; 32]))
            );
            assert!(matches!(
                unlocked.signer(100 + UNLOCK_TTL_S),
                Err(VaultError::Expired)
            ));
            assert_eq!(vault.unlocks(), vec![MasterUse::Revoke]);
        }

        #[test]
        fn a_flow_discards_only_the_key_it_staged() {
            let vault = MemoryVault::new();
            let (a, b) = (StageToken([1; 16]), StageToken([2; 16]));
            vault
                .stage_master("Qm", 1, Secret::new([1; 32]), &a)
                .unwrap();
            // Another app stages the same index after it: A's discard leaves B's key.
            vault
                .stage_master("Qm", 1, Secret::new([1; 32]), &b)
                .unwrap();
            vault.discard_staged("Qm", 1, Some(&a)).unwrap();
            assert_eq!(vault.staged_index("Qm").unwrap(), Some(1));
            // Another index is left too, whoever asks.
            vault.discard_staged("Qm", 2, None).unwrap();
            assert_eq!(vault.staged_index("Qm").unwrap(), Some(1));
            vault.discard_staged("Qm", 1, Some(&b)).unwrap();
            assert_eq!(vault.staged_index("Qm").unwrap(), None);
            // Without an owner (a refresh past the index) it goes whoever staged it.
            vault
                .stage_master("Qm", 1, Secret::new([1; 32]), &a)
                .unwrap();
            vault.discard_staged("Qm", 1, None).unwrap();
            assert_eq!(vault.staged_index("Qm").unwrap(), None);
            // A promote needs no token.
            vault
                .stage_master("Qm", 1, Secret::new([1; 32]), &b)
                .unwrap();
            assert_eq!(vault.promote_master("Qm", 1).unwrap(), Stored::Stored);
            assert_eq!(vault.master_index("Qm").unwrap(), Some(1));
        }

        /// The kept key never goes back a generation: a store of an older index answers
        /// `AlreadyNewer` with the index kept and changes nothing, and a promote whose staged
        /// key meets a newer kept one discards the staged key, which can never become it.
        #[test]
        fn an_older_key_never_replaces_a_newer_kept_one() {
            let vault = MemoryVault::new();
            vault.store_master("Qm", 2, Secret::new([3; 32])).unwrap();
            assert_eq!(
                vault.store_master("Qm", 2, Secret::new([3; 32])).unwrap(),
                Stored::Stored,
                "the kept index again"
            );
            assert_eq!(
                vault.store_master("Qm", 1, Secret::new([5; 32])).unwrap(),
                Stored::AlreadyNewer { kept: 2 }
            );
            assert_eq!(vault.master_index("Qm").unwrap(), Some(2));
            let unlocked = vault.unlock("Qm", MasterUse::Link, 0).unwrap();
            assert_eq!(
                unlocked.signer(0).unwrap().key(),
                Signer::key(&ed25519_dalek::SigningKey::from_bytes(&[3; 32])),
                "still the newer key"
            );
            vault
                .stage_master("Qm", 1, Secret::new([6; 32]), &StageToken([7; 16]))
                .unwrap();
            assert_eq!(
                vault.promote_master("Qm", 1).unwrap(),
                Stored::AlreadyNewer { kept: 2 }
            );
            assert_eq!(vault.master_index("Qm").unwrap(), Some(2));
            assert_eq!(
                vault.staged_index("Qm").unwrap(),
                None,
                "the staged key is discarded"
            );
        }

        /// The forget is a compare-and-delete on the index the caller checked: a key another
        /// app installed through a second handle on the same vault, between the read and the
        /// forget, is never removed.
        #[test]
        fn a_forget_removes_only_the_index_checked() {
            let shared = std::sync::Arc::new(MemoryVault::new());
            let (mine, theirs) = (shared.clone(), shared.clone());
            mine.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            // Mine reads #0; theirs installs #1; mine's forget of #0 leaves #1.
            assert_eq!(mine.master_index("Qm").unwrap(), Some(0));
            theirs.store_master("Qm", 1, Secret::new([4; 32])).unwrap();
            assert_eq!(mine.forget_master("Qm", 0).unwrap(), Forgot::Kept(1));
            assert_eq!(mine.master_index("Qm").unwrap(), Some(1));
            // At the kept index it forgets, and over nothing kept it is still Forgot.
            assert_eq!(mine.forget_master("Qm", 1).unwrap(), Forgot::Forgot);
            assert_eq!(mine.forget_master("Qm", 1).unwrap(), Forgot::Forgot);
            assert_eq!(mine.master_index("Qm").unwrap(), None);
        }

        /// The index is read without an unlock, and a forgotten key is gone.
        #[test]
        fn the_index_needs_no_unlock_and_a_forgotten_key_is_gone() {
            let vault = MemoryVault::new();
            assert_eq!(vault.master_index("Qm").unwrap(), None);
            vault.store_master("Qm", 2, Secret::new([3; 32])).unwrap();
            assert_eq!(vault.master_index("Qm").unwrap(), Some(2));
            assert!(vault.unlocks().is_empty());
            vault.forget_master("Qm", 2).unwrap();
            vault.forget_master("Qm", 2).unwrap();
            assert_eq!(vault.master_index("Qm").unwrap(), None);
            assert!(matches!(
                vault.unlock("Qm", MasterUse::Revoke, 0),
                Err(VaultError::NotFound)
            ));
        }

        /// Forgetting the master key forgets the kept key only: a staged key (another app's
        /// rotation in flight) stays until it is promoted or discarded.
        #[test]
        fn forgetting_the_master_key_leaves_a_staged_key() {
            let vault = MemoryVault::new();
            let owner = StageToken([4; 16]);
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            vault
                .stage_master("Qm", 1, Secret::new([5; 32]), &owner)
                .unwrap();
            vault.forget_master("Qm", 0).unwrap();
            assert_eq!(vault.master_index("Qm").unwrap(), None);
            assert_eq!(vault.staged_index("Qm").unwrap(), Some(1));
            vault.promote_master("Qm", 1).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::Link, 0).unwrap();
            assert_eq!(unlocked.signer(0).unwrap().index(), 1);
            assert_eq!(
                unlocked.signer(0).unwrap().key(),
                Signer::key(&ed25519_dalek::SigningKey::from_bytes(&[5; 32]))
            );
        }

        /// A link's unlock lasts until the store's confirm grace ends, so the `add` can be
        /// signed while the store still takes it.
        #[test]
        fn a_link_unlock_lasts_through_the_confirm_grace() {
            use ad4m_identity_core::link::{CONFIRM_GRACE_S, OFFER_TTL_S};
            let vault = MemoryVault::new();
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::Link, 100).unwrap();
            let end = 100 + OFFER_TTL_S + CONFIRM_GRACE_S;
            assert!(unlocked.signer(100 + OFFER_TTL_S + 1).is_ok());
            assert!(unlocked.signer(end - 1).is_ok());
            assert!(matches!(unlocked.signer(end), Err(VaultError::Expired)));
        }

        /// A push of the master key (a seal, an inbox put, the record) lasts a revocation's
        /// five minutes, not a link's grace.
        #[test]
        fn a_send_master_unlock_lasts_five_minutes() {
            let vault = MemoryVault::new();
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            let unlocked = vault.unlock("Qm", MasterUse::SendMaster, 100).unwrap();
            assert!(unlocked.signer(100 + UNLOCK_TTL_S - 1).is_ok());
            assert!(matches!(
                unlocked.signer(100 + UNLOCK_TTL_S),
                Err(VaultError::Expired)
            ));
        }

        /// `record_seen` keeps each identity's highest record version and answers the one
        /// before, so the reader refuses a version served back under it; `record_seen_reset`
        /// clears the mark.
        #[test]
        fn the_record_high_water_rises_only() {
            let vault = MemoryVault::new();
            assert_eq!(vault.record_seen("Qm", 1).unwrap(), None);
            assert_eq!(vault.record_seen("Qm", 3).unwrap(), Some(1));
            // A version under the mark answers the mark it trips on; the mark does not fall.
            assert_eq!(vault.record_seen("Qm", 2).unwrap(), Some(3));
            assert_eq!(vault.record_seen("Qm", 3).unwrap(), Some(3));
            assert_eq!(vault.record_seen("Qm2", 1).unwrap(), None);
            vault.record_seen_reset("Qm").unwrap();
            assert_eq!(vault.record_seen("Qm", 2).unwrap(), None);
        }

        /// The custody note is kept per device key, so two apps sharing the vault (two device
        /// entries) never overwrite each other's; each write is a compare-and-swap, so a clear
        /// never removes a note written since it was read.
        #[test]
        fn custody_notes_are_per_device_and_swapped_only_from_what_was_read() {
            let vault = MemoryVault::new();
            let owed = CustodyNote::MarkOwed {
                index: 1,
                epoch: 4,
                token: NoteToken([1; 16]),
                acquired: false,
                restores: None,
            };
            let released = CustodyNote::Released {
                epoch: 3,
                token: NoteToken([2; 16]),
                restores: None,
            };
            assert_eq!(vault.custody_note("Qm", "d1").unwrap(), None);
            assert!(vault
                .swap_custody_note("Qm", "d1", None, Some(&released))
                .unwrap());
            assert!(vault
                .swap_custody_note("Qm", "d2", None, Some(&owed))
                .unwrap());
            assert_eq!(
                vault.custody_note("Qm", "d1").unwrap(),
                Some(released.clone())
            );
            assert_eq!(vault.custody_note("Qm", "d2").unwrap(), Some(owed.clone()));
            assert_eq!(vault.custody_note("Qm2", "d1").unwrap(), None);
            // A clear that expects the note it wrote leaves another one in place.
            assert!(!vault
                .swap_custody_note("Qm", "d1", Some(&owed), None)
                .unwrap());
            assert_eq!(
                vault.custody_note("Qm", "d1").unwrap(),
                Some(released.clone())
            );
            assert!(vault
                .swap_custody_note("Qm", "d2", Some(&owed), None)
                .unwrap());
            assert_eq!(vault.custody_note("Qm", "d2").unwrap(), None);
            // Two releases at one epoch by two operations are two notes: a rollback that
            // expects the first never touches the second.
            let again = CustodyNote::Released {
                epoch: 3,
                token: NoteToken([3; 16]),
                restores: None,
            };
            assert!(vault
                .swap_custody_note("Qm", "d1", Some(&released), Some(&again))
                .unwrap());
            assert!(!vault
                .swap_custody_note("Qm", "d1", Some(&released), None)
                .unwrap());
            assert_eq!(vault.custody_note("Qm", "d1").unwrap(), Some(again));
        }

        /// Only a receipt lands, and only while the vault keeps exactly its index. An open
        /// intent never lands, though the vault keeps its index (another device's store).
        #[test]
        fn only_a_receipt_for_the_kept_index_lands() {
            let vault = MemoryVault::new();
            let epoch = vault.custody_epoch("Qm").unwrap();
            let intent = CustodyNote::MarkOwed {
                index: 1,
                epoch,
                token: NoteToken([1; 16]),
                acquired: false,
                restores: None,
            };
            let receipt = CustodyNote::MarkOwed {
                index: 1,
                epoch,
                token: NoteToken([1; 16]),
                acquired: true,
                restores: None,
            };
            assert!(!receipt.landed(&vault, "Qm").unwrap(), "nothing stored");
            vault.store_master("Qm", 0, Secret::new([3; 32])).unwrap();
            assert!(!receipt.landed(&vault, "Qm").unwrap(), "an older key");
            vault.store_master("Qm", 1, Secret::new([4; 32])).unwrap();
            assert!(receipt.landed(&vault, "Qm").unwrap());
            assert!(
                !intent.landed(&vault, "Qm").unwrap(),
                "an intent proves nothing"
            );
            vault.store_master("Qm", 2, Secret::new([5; 32])).unwrap();
            assert!(!receipt.landed(&vault, "Qm").unwrap(), "a later key");
            let released = CustodyNote::released(0, &crate::OsRandom);
            assert!(!released.landed(&vault, "Qm").unwrap());
        }

        /// The layers of a stacked note come back from the top, and the first release or
        /// receipt from the top decides its standing; open intents above it change nothing.
        #[test]
        fn a_stacked_note_stands_on_its_first_release_or_receipt() {
            let random = crate::OsRandom;
            let older = CustodyNote::MarkOwed {
                index: 0,
                epoch: 1,
                token: NoteToken([9; 16]),
                acquired: true,
                restores: None,
            };
            let release = CustodyNote::released(2, &random);
            let intent = CustodyNote::owed(0, 2, &random);
            let layers = vec![intent.clone(), release.clone(), older.clone()];
            let note = CustodyNote::stacked(layers.clone()).unwrap();
            assert_eq!(note.layers(), layers);
            assert_eq!(note.standing(), Standing::Released);
            assert_eq!(note.token(), intent.token());
            let below = CustodyNote::stacked(vec![older.clone()]);
            assert_eq!(note.restores().unwrap().restores().cloned(), below);
            let no_release = CustodyNote::stacked(vec![intent.clone(), older.clone()]).unwrap();
            assert_eq!(no_release.standing(), Standing::Acquired(0));
            assert_eq!(no_release.receipt(), Some(older));
            assert_eq!(intent.standing(), Standing::Pending);
            assert_eq!(CustodyNote::stacked(Vec::new()), None);
            // A note written before the stack (no `acquired`, no `restores`) still reads.
            let old: CustodyNote = serde_json::from_value(serde_json::json!({
                "state": "markOwed", "index": 0, "epoch": 1, "token": NoteToken([0; 16])
            }))
            .unwrap();
            assert_eq!(old.standing(), Standing::Pending);
            let json = serde_json::to_value(&note).unwrap();
            assert_eq!(serde_json::from_value::<CustodyNote>(json).unwrap(), note);
        }
    }
}
