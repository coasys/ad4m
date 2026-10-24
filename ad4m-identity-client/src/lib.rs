//! `ad4m-identity-client`: the master-key side of `did:scid:coasys:1` for apps (AR-03 4.2–4.5,
//! identity plan PR 1.1). The executor never holds a master key or the words; an app does,
//! through this library: a [`Vault`] keeps master keys, a [`MasterSigner`] signs with one, and
//! the flows build the events the executor co-signs and publishes.
//!
//! - [`flows::create`]: new words, master #0, an `incept` listing the executor's device key as
//!   `d1`, the executor's co-signature, publish, then the vault keeps master #0.
//! - [`flows::link`]: an offer signed by the current master key, the offer link, the claim,
//!   the match code both sides show, and the `add` that spends the claim.
//! - [`flows::revoke`]: a `revoke` signed by the current master key, published.
//!
//! No custody key (decision 21.5). The library has no clock, randomness or network of its
//! own: [`Clock`], [`Random`] and [`executor::ExecutorRpc`] come in from the surface, so the
//! same code runs natively and in WASM.

pub mod executor;
pub mod flows;
pub mod secret;
pub mod vault;

pub use executor::{ExecutorRpc, RpcError, ViaExecutor};
pub use secret::Secret;
#[cfg(any(test, feature = "test-vault"))]
pub use vault::MemoryVault;
pub use vault::{MasterSigner, MasterUse, Unlocked, Vault, VaultError};

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
    #[error("the log does not fold: {0}")]
    Fold(String),
    /// The executor's device key speaks for an identity already.
    #[error("the executor speaks for {0} already")]
    HasIdentity(String),
    /// The words typed back are not the words shown.
    #[error("the words do not match")]
    WrongWords,
    /// A step was taken out of order.
    #[error("the flow is not at that step: {0}")]
    Step(&'static str),
    /// The offer ended before it was claimed or confirmed.
    #[error("the offer is {0}")]
    OfferEnded(String),
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
