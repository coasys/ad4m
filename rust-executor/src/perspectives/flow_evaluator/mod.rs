//! Deterministic post-processing pass over active flows.
//!
//! # What this pass does
//!
//! For each `FlowInstance` alive on the perspective, walk the reachable
//! next-states declared on its `SHACLFlow`, evaluate every state's
//! `requires` guard against the committed graph, and emit a
//! [`SatisfiedTransition`] for every (instance, next-state) pair whose
//! guards fully match. Downstream stages turn those satisfied transitions
//! into on-graph `FlowTransitionProposal` writes.
//!
//! A `requires` guard is an array of `ModelQuery` shapes carrying an
//! optional `count.{min,max}` cardinality. The guard is satisfied when
//! every element matches the target class with the required cardinality;
//! the AND across `requires` is what gates a proposal. The record of what
//! matched (class name + sorted matched-ids per element) is hashed into an
//! `evidence` value on the proposal so a later re-verification can catch a
//! proposal whose evidence no longer resolves.
//!
//! # Module layout
//!
//! [`primitives`] — pure, no-I/O building blocks:
//!
//! - [`SatisfiedTransition`] — the record the writer stage consumes.
//! - `build_query_input_for_requires` — translator from `ModelQuery`
//!   (flow-side type) to `serde_json::Value` (`model_query`'s input
//!   shape). Substitutes `$did` in `didProperty` at translation time.
//!   Recursive over `ModelQuery.or`.
//! - `cardinality_satisfied` — `count.{min,max}` check.
//! - `evidence_hash` — deterministic SHA256 of a (class, sorted
//!   matched-ids) pair, used to seed the proposal's evidence field.
//!
//! [`queryable`] — async layer over the one perspective-side query the
//! evaluator needs:
//!
//! - `RequiresQueryable` trait — factored so tests can stub it without a
//!   live `PerspectiveInstance`. `PerspectiveInstance` gets a blanket impl.
//! - `evaluate_single_query` — one `model_query` call + cardinality check
//!   + evidence extraction.
//! - `evaluate_state_requires` — AND across a state's `requires`;
//!   returns `Some((class_names, evidence_ids))` when all elements match.
//! - `evaluate_flow_transitions` — top composer over all active flows and
//!   their reachable next-states. Silent-skip on unknown flow name,
//!   guardless states, and query errors so a single bad shape cannot
//!   poison the whole pass.
//!
//! [`engine_pass`] — writer + entry point:
//!
//! - `write_engine_proposal` — `SatisfiedTransition` → on-graph
//!   `FlowTransitionProposal` write.
//! - [`LlmProposalHint`] — LLM-attribution boundary type: an LLM-side
//!   proposal carries an optional `rationale` string that the writer
//!   attaches to the proposal when the two match on (flow, from, to).
//! - [`run_engine_proposal_pass`] — the composed load → evaluate →
//!   (optional semantic-check) → write pipeline the auto-processor calls.
//!
//! Tests live in their own files: [`unit_tests`] stubs `RequiresQueryable`
//! for deterministic in-process coverage; [`e2e_tests`] exercises the same
//! contracts against a real `PerspectiveInstance`.
//!
//! # Why pure primitives + trait-backed async layer
//!
//! The writer stage emits `FlowTransitionProposal` writes on behalf of the
//! extraction DID from the pass's results. A bug in the
//! `ModelQuery` → `ModelQueryInput` translation would either miss a
//! satisfied guard (flow silently stalls) or synthesize a wrong-guard
//! proposal (garbage in the flow's evidence chain). Isolating the
//! translation from graph I/O gives fixture-driven unit tests for every
//! `PropertyCondition` variant + `$did` substitution; the
//! `RequiresQueryable` trait gives the same coverage for the composition
//! and error-handling shape without paying the cost of a live perspective
//! per test.

#![allow(dead_code)]

mod engine_pass;
mod primitives;
mod queryable;

#[cfg(test)]
mod e2e_tests;
#[cfg(test)]
mod unit_tests;

pub use engine_pass::{run_engine_proposal_pass, LlmProposalHint};
pub use primitives::SatisfiedTransition;
