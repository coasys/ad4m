//! Pure data shapes for the Model C flow-aware extraction path.
//!
//! No I/O and no rendering — these are the values carried between the
//! [`super::render`] and [`super::loader`] layers and out to
//! `build_interpretation_input`.

use crate::perspectives::flow_instance::fold::Contention;
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
    /// The fold's contention verdict for `current_state` — three-state on
    /// purpose. `Option<Contention>` would conflate "a fresh fold verified
    /// no contention" with "contention was never computed" (the cache path
    /// stores only the state name), and that conflation lands in the
    /// permissive direction: an uncontested-*looking* flow the model may
    /// propose into. Same `Option`-conflation family as the role-grant
    /// "unknown ⇒ always" and `revoked_at` "unparseable ⇒ not revoked"
    /// findings (#998 review).
    pub contested: ContentionStatus,
}

/// Contention verdict carried by a [`FlowContext`].
#[derive(Debug, Clone)]
pub enum ContentionStatus {
    /// A fresh fold ran on this replica and found no contention.
    NotContested,
    /// The state came from the `Local`-verified cache, which stores only
    /// the derived state name — contention was **not computed**. Staleness
    /// is bounded by the sync-triggered re-derive (every incoming flow
    /// link re-folds), but a cached flow can look uncontested indefinitely
    /// if no new link arrives. Consumers must not treat this as a verified
    /// all-clear; anything payout-adjacent must re-derive instead (the
    /// mint pass already does — it calls `derive_states` directly and
    /// never sees this variant).
    Unknown,
    /// The fold found two edges out of `current_state` both carrying
    /// quorum — the flow is irreversibly stalled. Renderers MUST surface
    /// this rather than presenting the flow as "awaiting votes"; nothing
    /// may propose into such a flow.
    Contested(Contention),
}

impl ContentionStatus {
    /// A fresh derivation's verdict: `derive_states` computes contention
    /// definitively, so its `None` genuinely means "not contested".
    pub fn from_fresh_derivation(contested: Option<Contention>) -> Self {
        match contested {
            None => ContentionStatus::NotContested,
            Some(c) => ContentionStatus::Contested(c),
        }
    }
}

/// A [`FlowInstanceRecord`] paired with how its contention verdict was
/// obtained — the loader's unit between state resolution (cache or fold)
/// and [`FlowContext`] construction. The cache path yields
/// [`ContentionStatus::Unknown`]; a fresh derivation yields a definitive
/// verdict.
#[derive(Debug, Clone)]
pub struct ResolvedFlow {
    pub record: FlowInstanceRecord,
    pub contention: ContentionStatus,
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
    /// Canonical URI of the `SHACLFlow` this instance runs — matches
    /// `SHACLFlow.flow_uri()` (`${namespace}${name}Flow`, e.g.
    /// `coasys://DeliveryFlow`) and is the identity property of the
    /// `FlowInstance` @Model class. URI-keyed rather than name-keyed so
    /// social-DNA modules from different communities can't collide on a
    /// shared name (James PR #929 R5).
    pub flow_uri: String,
    /// Instance URI — `ad4m://flow/instance/{id}` (see
    /// [`super::super::flow_classes::flow_instance_uri`]).
    pub instance_uri: String,
    /// Base expression this instance is bound to. Named `subject` on
    /// the `FlowInstance` class to avoid the Ad4mModel synthetic-field
    /// collision that broke `baseExpression` in the reserved-field
    /// rename fix (commit `e6362e5ca`).
    pub subject: String,
    /// The state name this replica's consensus pass last cached for the
    /// instance (a `Local` link, #987) — or **empty** when no pass has run
    /// here yet. Never the authority: the fold over the signed proposals is
    /// (`flow_instance::derive_states` replaces this field with its
    /// verdict). Readers that need the live state derive; readers that only
    /// need the row (spawn dedup, lookups) must tolerate the empty value.
    pub current_state: String,
    /// ISO-8601 timestamp the instance was minted at. Sourced from
    /// `Ad4mModel`'s synthesised `createdAt` (earliest link timestamp on
    /// the instance's URI). `None` when hydration didn't produce a
    /// timestamp — rare, but the extraction pass renders "start time
    /// unknown" rather than skipping the record.
    pub created_at: Option<String>,
}
