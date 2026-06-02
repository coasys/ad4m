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

pub use config::{ArcPolicy, LocFnPolicy, SpaceConfig, ValidationRegime};
pub use envelope::{EnvelopeError, OpEnvelope};
