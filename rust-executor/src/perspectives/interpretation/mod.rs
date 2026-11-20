//! Generic LLM interpretation: turn conversation text into typed subject-class
//! instances, steered by the natural-language `interpretation_hint` declared on each
//! class/property.
//!
//! Interpretation is **tree-aware**: the model sees what the graph already holds
//! and can attach to it (refine an existing node), grow it (link a new node to
//! an existing or co-minted one), or create fresh nodes.
//!
//! Data flow of one interpretation run (`run_interpretation` wires it end to end):
//!   1. Read the instances already present per class (`existing_instance_context`
//!      — each row is `{id, identity value, class}`, keyed by the class's declared
//!      `identity` property) so the model can be steered away from re-proposing
//!      known items *and* handed the `id`s it needs to update or link them.
//!   2. Render those, plus each class's `interpretation_hint`, scalar fields and
//!      forward relations, and the transcript, into a single prompt
//!      (`build_interpretation_input`). The prompt shows only the rows that fit
//!      a share of the model's window, the pass's own records first, with short
//!      handles (`P1`) for ids (`existing_budget`); the steps below read the
//!      full set.
//!   3. Ask the configured LLM (`AIService::prompt` on the task registered by
//!      `ensure_interpretation_task`), retrying the parse a few times because local
//!      models emit half-valid JSON (`retry_interpretation_parse` +
//!      `parse_interpretation_response` -> `ProposedInstance`s).
//!   4. Drop anything already present (`filter_already_present`, order-preserving),
//!      then plan the writes (`plan_interpretation_ops_with_context`): a proposal
//!      carrying an `id` becomes an `Update`, otherwise a `Create`, and relation
//!      fields — whose values are refs, either an existing `id` or a
//!      `new:<Class>:<n>` ordinal into this run's output — resolve into additive
//!      `AddLinks`.
//!   5. Drop updates that would write what's already there (`strip_noop_updates`),
//!      then apply everything in one batch (`apply_interpretation_ops`) through
//!      `create_subject` / `update_subject` / `add_links` — the same
//!      constructor+setter pipeline app code uses. Scalar values are
//!      literal-encoded by the class's `ad4m://setter` actions; link status
//!      derives from the SDNA's `local` flags, not a caller choice.
//!
//! The pure parse/DB-only units (`parse_interpretation_response`,
//! `filter_already_present`, `plan_interpretation_ops_with_context`,
//! `strip_noop_updates`, `existing_instance_context`) are unit-tested in-file;
//! the real-LLM end-to-end suite lives in `interpretation_e2e.rs`, with shared
//! fixtures/harness in `interpretation_test_support.rs`.

mod dedup;
mod existing_budget;
mod graph;
#[cfg(test)]
mod harness_pass_tests;
mod overflow;
pub(crate) use overflow::SliceScopeLink;
pub(crate) use run::{
    run_interpretation_observed_linked, run_interpretation_with_strategy_and_model_linked,
};
pub(crate) mod overlay;
mod parse;
mod prompt;
mod run;
mod types;
pub(crate) mod walk_tools;

pub use dedup::*;
pub use graph::*;
pub use overlay::InterpretationRunCursor;
pub(crate) use overlay::*;
pub use parse::*;
pub use prompt::*;
pub use run::*;
pub use types::*;
