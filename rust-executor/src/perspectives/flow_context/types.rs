//! Pure data shapes for the Model C flow-aware extraction path.
//!
//! No I/O and no rendering — these are the values carried between the
//! [`super::render`] and [`super::loader`] layers and out to
//! `build_interpretation_input`.

use crate::perspectives::shacl_parser::ConsensusRule;

/// One live `FlowInstance` summarized for the LLM prompt-builder.
///
/// Populated by [`super::loader::gather_active_flow_contexts`] — the
/// [`super::render::summarize_flow_instance`] helper builds it from a
/// parsed [`SHACLFlow`](crate::perspectives::shacl_parser::SHACLFlow)
/// plus the instance's scalar row.
#[derive(Debug, Clone)]
pub struct FlowContext {
    /// The flow's name — matches `SHACLFlow.name` and
    /// `FlowInstance.flow` (the SDNA identity discriminator).
    pub flow_name: String,
    /// Instance URI — `ad4m://flow/instance/{id}`.
    pub instance_uri: String,
    /// Base expression this instance is bound to (`FlowInstance.subject`
    /// — renamed from `baseExpression` in slice 14 to avoid the
    /// Ad4mModel reserved-field collision).
    pub subject: String,
    /// Current state name (matches a `FlowState.name` on the flow).
    pub current_state: String,
    /// Flow-level frame — English description of what the flow is
    /// about. Rendered verbatim into the prompt so the LLM has global
    /// context for the specific next-state decisions.
    pub flow_interpretation_hint: Option<String>,
    /// Every state reachable from `current_state` via one transition,
    /// summarized. Order preserved from `SHACLFlow.transitions`.
    pub reachable_next_states: Vec<NextStateSummary>,
    /// Flow-level default consensus rule. Rendered as trailing context
    /// so the LLM knows how many signers are needed if the state's own
    /// rule is not overridden.
    pub consensus_rule: Option<ConsensusRule>,
}

/// One reachable next-state, ready for prompt insertion.
#[derive(Debug, Clone)]
pub struct NextStateSummary {
    /// State name (matches `FlowState.name`).
    pub name: String,
    /// English hint on when this state applies (from
    /// `FlowState.interpretationHint`).
    pub interpretation_hint: Option<String>,
    /// English rendering of `FlowState.requires` — the LLM reads this
    /// to know what evidence to look for in the transcript. Empty
    /// string when the state has no `requires` (falls back to the
    /// legacy `state_check` link pattern, which is not surfaced to the
    /// LLM).
    pub requires_human_readable: String,
    /// English hint for a targeted 2nd-pass LLM confirmation
    /// (`FlowState.semanticCheck`). Rendered directly; when set, the
    /// engine will fire an extra call after `requires` matches.
    pub semantic_check: Option<String>,
    /// Per-state consensus override — falls back to
    /// [`FlowContext::consensus_rule`] when `None`.
    pub consensus_rule: Option<ConsensusRule>,
}

/// One live `FlowInstance` as read off the perspective graph — the raw
/// scalar row that pairs with a parsed
/// [`SHACLFlow`](crate::perspectives::shacl_parser::SHACLFlow) to
/// produce a [`FlowContext`].
///
/// Kept flat (no reference to the parsed flow definition) so the
/// perspective read can be independent of the SDNA-flow catalogue read.
/// The two are joined by [`super::loader::build_flow_contexts`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowInstanceRecord {
    /// The flow-name discriminator — matches `SHACLFlow.name` and is
    /// the identity property of the `FlowInstance` @Model class.
    pub flow_name: String,
    /// Instance URI — `ad4m://flow/instance/{id}` (see
    /// [`super::super::flow_classes::flow_instance_uri`]).
    pub instance_uri: String,
    /// Base expression this instance is bound to. Named `subject` on
    /// the `FlowInstance` class to avoid the Ad4mModel synthetic-field
    /// collision that broke `baseExpression` in the reserved-field
    /// rename fix (commit `e6362e5ca`).
    pub subject: String,
    /// Current state name (matches a `FlowState.name` on the flow).
    pub current_state: String,
    /// ISO-8601 timestamp the instance was minted at. Sourced from
    /// `Ad4mModel`'s synthesised `createdAt` (earliest link timestamp on
    /// the instance's URI). `None` when hydration didn't produce a
    /// timestamp — rare, but the extraction pass renders "start time
    /// unknown" rather than skipping the record.
    pub created_at: Option<String>,
}
