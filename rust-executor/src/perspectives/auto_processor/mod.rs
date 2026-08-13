//! Neighbourhood auto-processing — run the interpretation engine automatically
//! and coordinate *which* peer runs each pass, inside AD4M rather than in app UI
//! code.
//!
//! Built in phases, stacked on the tree-aware interpretation engine (#883):
//!   * **P-A — [`claim`]**: the atomic reservation primitive (`ProcessingClaim`
//!     instances in the shared perspective) that fixes Flux's double-processing
//!     race.
//!   * **P-B1 — [`config`]**: `AutoProcessorConfig` + the `AutoProcessor`
//!     subject class the executor watcher reads back to know which processors
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
pub mod cursor;
pub mod events;
pub mod watcher;

/// Render a hydrated `model_query` scalar as a `String`. Both classes write
/// their scalars as `literal:string:` targets, but a value that hydrates to a
/// JSON number/bool (hand-edited graph, or a peer that wrote a typed literal)
/// is rendered rather than dropped — the numeric parse at the call site is what
/// decides whether it is usable.
pub(crate) fn scalar_string(value: Option<&serde_json::Value>) -> Option<String> {
    match value? {
        serde_json::Value::String(s) if !s.is_empty() => Some(s.clone()),
        serde_json::Value::Number(n) => Some(n.to_string()),
        serde_json::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}
