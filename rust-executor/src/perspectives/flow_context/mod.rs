//! Model C flow-aware extraction — data shape, rendering, and perspective
//! loading for the "Active flows on this scope" prompt block.
//!
//! Split into three cohesive layers so each can be read + tested in
//! isolation (Nico's PR #929 review R4, 2026-08-27):
//!
//! - [`types`] — the pure data shapes ([`FlowContext`],
//!   [`NextStateSummary`], [`FlowInstanceRecord`]). No I/O, no rendering.
//! - [`render`] — pure prompt-builder helpers: `ModelQuery` → English
//!   sentences, `FlowState` → [`NextStateSummary`], `SHACLFlow` +
//!   scalars → [`FlowContext`]. Isolated so the "hottest correctness
//!   surface in the LLM prompt" (per the original module doc) can grow
//!   fixture-driven tests without touching graph I/O.
//! - [`loader`] — perspective-side reading: hydrated JSON → records,
//!   catalogue discovery ([`load_shacl_flows`]), and the composed
//!   [`gather_active_flow_contexts`] entry point the extraction pass
//!   calls.
//!
//! End-to-end integration coverage lives in the sibling `e2e_tests`
//! submodule (test-only). It seeds a real [`PerspectiveInstance`] with
//! writer-emitted flow links, mints a
//! [`FlowInstance`](crate::perspectives::flow_classes::FLOW_INSTANCE_CLASS)
//! runtime record, and walks the whole `gather_active_flow_contexts →
//! build_interpretation_input` chain without an LLM. That test is the
//! natural onion-shell for PR #929.

#![allow(dead_code)]

mod loader;
mod render;
mod types;

#[cfg(test)]
mod e2e_tests;

#[cfg(test)]
mod real_llm_e2e;

// Public re-exports — callers outside this module (`interpretation::run`,
// `PerspectiveInstance::…`) import from `flow_context::` as before.
pub use loader::{
    build_flow_contexts, gather_active_flow_contexts, load_all_flow_instances, load_flow_instances,
    load_shacl_flows, parse_flow_instance_from_hydrated, parse_flows_from_bag, scope_subject,
};
pub use render::{
    reachable_next_states, render_consensus_rule, render_model_query,
    render_requires_human_readable, summarize_flow_instance, summarize_next_state, FlowTokens,
    FLOW_BASE_TOKEN, FLOW_INSTANCE_TOKEN,
};
pub use types::{FlowContext, FlowInstanceRecord, NextStateSummary};
