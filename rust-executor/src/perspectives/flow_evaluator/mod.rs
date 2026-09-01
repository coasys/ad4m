//! Slice 10.4a of the flow-implementation arc — the deterministic
//! `FlowTransitionProposal` post-processing pass. Turns each active
//! `FlowInstance` and its reachable next-states into a
//! [`SatisfiedTransition`] per (record, next-state) whose `requires` array
//! is fully satisfied against the committed perspective graph.
//!
//! Design authority: `planning/flow-interpretation-hints-design.md` §5 step 5
//! ("Post-processing (engine, deterministic)") and §7 (`ConsensusRule` +
//! `didProperty` role-gate).
//!
//! # What this module owns
//!
//! Pure primitives (slice 10.4a1, [`primitives`]):
//!
//! - [`SatisfiedTransition`] — the record slice 10.4b's writer stage
//!   consumes.
//! - [`build_query_input_for_requires`] — translator from `ModelQuery`
//!   (flow-side type) to `serde_json::Value` (`model_query`'s input
//!   shape). Substitutes `$did` in `didProperty` at translation time.
//!   Recursive over `ModelQuery.or`.
//! - [`cardinality_satisfied`] — `count.{min,max}` cardinality check.
//! - [`evidence_hash`] — deterministic SHA256 of a (class, sorted
//!   matched-ids) pair. Used to seed the evidence field on the
//!   `FlowTransitionProposal` that slice 10.4b emits, so a re-verification
//!   pass in slice 10.6 can catch a tampered proposal.
//!
//! Async layer (slice 10.4a2, [`queryable`]):
//!
//! - [`RequiresQueryable`] — the one perspective-side call the evaluator
//!   needs, factored behind a trait so tests can stub it without a live
//!   `PerspectiveInstance`. `PerspectiveInstance` gets a blanket impl.
//! - [`evaluate_single_query`] — one `model_query` call + cardinality
//!   check + evidence extraction.
//! - [`evaluate_state_requires`] — AND across a state's `requires` array;
//!   returns `Some((class_names, evidence_ids))` when all guards match.
//! - [`evaluate_flow_transitions`] — the top composer that walks every
//!   active flow's reachable next-states and returns
//!   `Vec<SatisfiedTransition>`. Silent-skip on unknown flow name,
//!   guardless states, and query errors so a single bad shape cannot
//!   poison the whole pass.
//!
//! Writer + entry point (slices 10.4b/10.4c, [`engine_pass`]):
//!
//! - [`write_engine_proposal`] — `SatisfiedTransition` → on-graph
//!   `FlowTransitionProposal` write.
//! - [`LlmProposalHint`] — the LLM-attribution boundary type (slice 10.6c).
//! - [`run_engine_proposal_pass`] — the composed load → evaluate →
//!   (optional semantic-check) → write pipeline the auto-processor calls.
//!
//! Tests live in their own files: [`unit_tests`] stubs `RequiresQueryable`
//! for deterministic in-process coverage; [`e2e_tests`] exercises the same
//! contracts against a real `PerspectiveInstance`.
//!
//! # Why pure primitives + trait-backed async layer
//!
//! Slice 10.4b will emit `FlowTransitionProposal` writes on behalf of the
//! extraction DID from these results. Any bug in the ModelQuery→ModelQueryInput
//! translation would either miss a satisfied requires (flow silently
//! stalls) or synthesize a wrong-guard proposal (garbage in the flow's
//! evidence chain). Isolating the translation from graph I/O gives us
//! fixture-driven unit tests for every `PropertyCondition` variant +
//! `$did` substitution; the [`RequiresQueryable`] trait gives us the same
//! coverage for the composition and error-handling shape without paying
//! the cost of a live perspective per test.

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
