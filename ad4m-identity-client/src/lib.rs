//! `ad4m-identity-client`: the master-key side of `did:scid:coasys:1` for apps. The executor
//! never holds a master key or the words; an app does, through this library: a [`Vault`] keeps
//! master keys, a [`MasterSigner`] signs with one, and the flows build the events the executor
//! co-signs and publishes.
//!
//! - [`flows::create`]: new words, master #0, an `incept` listing the executor's device key as
//!   `d1`, the executor's co-signature, publish, then the vault keeps master #0.
//! - [`flows::link`]: an offer signed by the current master key, the offer link, the claim,
//!   the match code both sides show, and the `add` that spends the claim.
//! - [`flows::link`] also links with the holder scanning a device string: an offer bound to
//!   the device's key, its ticket in the device's pre-join inbox.
//! - [`flows::master`]: the master key sealed to a device that keeps it, the retry list of
//!   pushes that failed, and the device's side that takes it from its executor.
//! - [`flows::revoke`]: a `revoke` signed by the current master key, published; or, with the
//!   12 words, a `rotate` that revokes the keys too ("Also replace my master key").
//! - [`flows::rotate`]: master *n*+1 from the 12 words, rebuilt on the new head when another
//!   event lands first, sent to every app that kept the master key, and the record written.
//! - [`flows::restore`]: the 12 words alone find the identity, lock out lost devices with a
//!   rotation to a fresh master on this device, and link this device again.
//! - [`flows::requests`]: the holder's answer to a device's request (send the master key,
//!   confirm a revocation, with the words when it asks for a rotation) and the device's post.
//! - [`flows::master`] also removes the master key from a device that is not its only holder,
//!   and gets a device whose key a rotation replaced the new one.
//! - [`record`]: the private details record, opened, merged and written in one place.
//!
//! No custody key: no service keeps a copy of a master key for the person. The library has no
//! clock, randomness or network of its own: [`Clock`], [`Random`] and [`executor::ExecutorRpc`]
//! come in from the surface, so the same code runs natively and in WASM.

pub mod executor;
pub mod flows;
pub mod record;
mod request;
pub mod secret;
#[cfg(feature = "store")]
pub mod store;
pub mod vault;

pub use executor::{ExecutorRpc, RpcError, ViaExecutor};
pub use secret::Secret;
#[cfg(feature = "store")]
pub use store::{
    Checkpoint, CheckpointError, EncKey, EncOpenError, HttpClient, HttpError, HttpRequest,
    HttpResponse, LogCheckpoints, MemoryCheckpoints, SoftEncKey, StoreEndpoint, ViaStore,
};
#[cfg(any(test, feature = "test-vault"))]
pub use vault::MemoryVault;
pub use vault::{
    CustodyNote, Forgot, MasterSigner, MasterUse, NoteToken, PendingPush, StageToken, Stored,
    Unlocked, Vault, VaultError,
};

/// Unix seconds, from the surface.
pub trait Clock {
    fn now(&self) -> u64;
}

/// Random bytes, from the surface.
pub trait Random {
    fn fill(&self, out: &mut [u8]);
}

/// The system clock (not on wasm32, where the surface passes its own).
#[cfg(not(target_arch = "wasm32"))]
pub struct SystemClock;

#[cfg(not(target_arch = "wasm32"))]
impl Clock for SystemClock {
    fn now(&self) -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0)
    }
}

/// The operating system's random source (the browser's crypto on wasm32).
pub struct OsRandom;

impl Random for OsRandom {
    fn fill(&self, out: &mut [u8]) {
        rand_core::RngCore::fill_bytes(&mut rand_core::OsRng, out);
    }
}

/// A surface's [`Random`] as the RNG the identity core's seals take.
pub(crate) struct RandomRng<'a>(pub &'a dyn Random);

impl rand_core::RngCore for RandomRng<'_> {
    fn next_u32(&mut self) -> u32 {
        rand_core::impls::next_u32_via_fill(self)
    }
    fn next_u64(&mut self) -> u64 {
        rand_core::impls::next_u64_via_fill(self)
    }
    fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.0.fill(dest);
    }
    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand_core::Error> {
        self.0.fill(dest);
        Ok(())
    }
}

/// The surface's random source is a cryptographic one (the OS's, the browser's crypto).
impl rand_core::CryptoRng for RandomRng<'_> {}

/// Why a flow stopped.
#[derive(Debug, thiserror::Error)]
pub enum ClientError {
    #[error("vault: {0}")]
    Vault(#[from] VaultError),
    #[error("executor: {0}")]
    Rpc(#[from] RpcError),
    #[error("words: {0}")]
    Words(#[from] ad4m_identity_core::words::WordsError),
    #[error("building the event: {0}")]
    Build(#[from] ad4m_identity_core::build::BuildError),
    #[error("offer or claim: {0}")]
    Link(#[from] ad4m_identity_core::link::LinkError),
    #[error("inbox message: {0}")]
    Inbox(#[from] ad4m_identity_core::inbox::InboxError),
    #[error("private record: {0}")]
    Record(#[from] ad4m_identity_core::record::RecordError),
    #[error("the log does not fold: {0}")]
    Fold(String),
    /// The executor's device key speaks for an identity already.
    #[error("the executor speaks for {0} already")]
    HasIdentity(String),
    /// The vault keeps another identity's incept for this executor whose publish is not
    /// confirmed: `flows::create::resume` finishes that one first.
    #[error("{0} was created on this surface and its publish is not confirmed")]
    CreatePending(String),
    /// The words typed back are not the words shown.
    #[error("the words do not match")]
    WrongWords,
    /// A step was taken out of order.
    #[error("the flow is not at that step: {0}")]
    Step(&'static str),
    /// The offer ended before it was claimed or confirmed.
    #[error("the offer is {0}")]
    OfferEnded(String),
    /// The master key goes only to a device key, valid at the head, that the key log lets keep
    /// it: a standing key of the incept, or one whose `add` carries a claim that said
    /// `canKeepMaster` (and at a link, only on a `device` offer). Never by the record's kind.
    #[error("{0} may not keep the master key")]
    NotAKeeper(String),
    /// A request asks for a rotation, which needs the 12 words.
    #[error("the request asks for a rotation, which needs the 12 words")]
    NeedsWords,
    /// A request of the box that its poster did not sign as the log lists it.
    #[error("the request is not signed by its poster: {0}")]
    Poster(String),
    /// Not written: the record says this device took the master key already
    /// (`holdsMaster: true`), and an `"offered"` never replaces that.
    #[error("{0} holds the master key already")]
    HoldsMaster(ad4m_identity_core::KeyId),
    /// Not written: the record lists no device `key_id`, so a `holdsMaster` for it would be a
    /// silent no-op its caller reports as done.
    #[error("the record lists no device {0}, so its master key cannot be marked")]
    NotListed(ad4m_identity_core::KeyId),
    /// Not written: the record already lists device `key_id`, and the write was to insert its
    /// entry only while it was missing (a retry whose device got its entry in between).
    #[error("the record already lists device {0}")]
    Listed(ad4m_identity_core::KeyId),
    /// The store served the record's version `served`, under the highest version `seen` this
    /// client already opened for the identity: a replayed older version, refused.
    #[error("the store served version {served} of the record, under version {seen} read before")]
    ServedRecordVersion { served: u64, seen: u64 },
    /// Not written: the stored record's recovery key waits for the current master to re-attest
    /// it — its attestation is by a master a rotation replaced — and a device write would seal
    /// the record without it, which the store takes as no recovery key, so the words could no
    /// longer open it. A device holding the current master re-attests it with a master-signed
    /// write (`flows::master::reattest_recovery`, which `flows::master::refresh_master` makes
    /// on its own), then the write goes through.
    #[error("the record waits for the master to re-attest the recovery key")]
    RecoveryNeedsReattest,
    /// The record's version moved under every write tried.
    #[error("the private record kept changing; try again")]
    RecordBusy,
    /// The words derive no master key of the identity: the key index lists no identity whose
    /// master #0 they derive, or they do not derive its current master. A word that passes the
    /// checksum can still be the wrong one.
    #[error("these words are not the identity's: {0}")]
    NotTheseWords(String),
    /// Another event landed first on every rotation tried (each `409` rebuilt it on the new
    /// head).
    #[error("the log kept changing under the rotation; try again")]
    LogBusy,
    /// This device holds the only usable copy of the master key: no other device valid at the
    /// head that the log lets keep it holds it (per the record). Removing it would leave only the 12 words.
    #[error("this device holds the only copy of the master key")]
    OnlyCopy,
    /// Not written: this device's custody note ([`vault::CustodyNote`]) changed between the
    /// decision to mark it and the record version the write built on — a removal released the
    /// copy, or the owed mark landed and was cleared — so the repair that decided on the old
    /// note writes nothing.
    #[error("this device's custody note changed under the mark")]
    CustodyNoteMoved,
    #[error("{0}")]
    Protocol(String),
}

/// `fold` of `events`, as the client checks every log it reads.
pub(crate) fn fold(
    events: &[ad4m_identity_core::Event],
) -> Result<ad4m_identity_core::KelChain, ClientError> {
    ad4m_identity_core::fold(events)
        .map_err(|e| ClientError::Fold(ad4m_identity_core::error_code(&e).to_string()))
}
