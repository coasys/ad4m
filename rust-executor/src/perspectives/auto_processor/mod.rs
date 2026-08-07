//! Neighbourhood auto-processing — run the interpretation engine automatically
//! and coordinate *which* peer runs each pass, inside AD4M rather than in app UI
//! code (spec: `planning/neighbourhood-auto-processing-spec.md`).
//!
//! Built in phases, stacked on the tree-aware interpretation engine (#883):
//!   * **P-A — [`claim`]**: the atomic reservation primitive (`ProcessingClaim`
//!     links in the shared perspective) that fixes Flux's double-processing race.
//!     *(this commit)*
//!   * **P-B**: the `AutoProcessor` subject class + an executor watcher that
//!     debounces new content, elects a candidate via telepresence presence, wins
//!     a claim, and runs the interpretation passes.
//!   * **P-C**: a Flux-parity integration demo.

pub mod claim;
