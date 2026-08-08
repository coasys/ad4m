//! Neighbourhood auto-processing — run the interpretation engine automatically
//! and coordinate *which* peer runs each pass, inside AD4M rather than in app UI
//! code (spec: `planning/neighbourhood-auto-processing-spec.md`).
//!
//! Built in phases, stacked on the tree-aware interpretation engine (#883):
//!   * **P-A — [`claim`]**: the atomic reservation primitive (`ProcessingClaim`
//!     links in the shared perspective) that fixes Flux's double-processing race.
//!   * **P-B1 — [`config`]**: `AutoProcessorConfig` + the `Shared`-link
//!     representation the executor watcher reads back to know which processors
//!     to schedule. *(this commit)*
//!   * **P-B2 — [`watcher`]**: the executor watcher — the pure debounce +
//!     batch-cap state plus [`watcher::run_one_pass`], the standalone runner
//!     that ties claim + shape-load + SPARQL transcript + interpretation
//!     together. Real event-stream wire-up (subscribe → debounce loop →
//!     telepresence-bounded candidacy) is the P-B2b follow-up. *(this
//!     commit — pure logic + standalone one-pass runner)*
//!   * **P-C**: a Flux-parity integration demo.

pub mod claim;
pub mod config;
pub mod signals;
pub mod watcher;
