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
//!   * **P-B2**: the executor watcher itself — debounces new content, elects
//!     a candidate via telepresence presence, wins a claim via [`claim`], and
//!     runs the interpretation passes for each loaded `AutoProcessorConfig`.
//!   * **P-C**: a Flux-parity integration demo.

pub mod claim;
pub mod config;
