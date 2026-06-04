//! Holograph — Kitsune2-backed substrate for AD4M link languages.
//!
//! This crate is the host-side runtime for the new "holograph-link" Language:
//! a thin layer between AD4M's perspective-diff algorithm and a Kitsune2
//! `Space`. v1 ships with full-arc, single-doc defaults but the interfaces
//! are designed so a v1.5 spike can flip to sharded mode without refactoring
//! the substrate code.
//!
//! See `.spike-docs/SPIKE.md` §1.5 for the six sharding-ready commitments
//! this crate honors.

pub mod config;
pub mod envelope;
pub mod integration_queue;
pub mod op_store;
pub mod retriever_kitsune;
pub mod space;

pub use config::{
    resolve_iroh_relay, ArcPolicy, FetchFallbackPolicy, LocFnPolicy, SpaceConfig, ValidationRegime,
};
pub use envelope::{EnvelopeError, OpEnvelope};
pub use integration_queue::{
    AlwaysValid, HolographIntegrationQueue, IntegrationQueueConfig, NotifyUp, OpFetcher,
    PeerPicker, SigVerifier,
};
pub use op_store::{EnvelopeDecoder, KvOpStore};
pub use retriever_kitsune::{KitsuneRetreiver, KitsuneRetreiverState};
pub use space::{
    holograph_envelope_decoder, ChannelNotifier, EmittedOp, HolographSpace, HolographSpaceConfig,
    HolographSpaceHandler, K2DynSpaceTarget, K2FetcherAdapter, K2OpStoreShim, K2PeerPickerAdapter,
    LocalCommitTarget, TelepresenceNotification,
};
