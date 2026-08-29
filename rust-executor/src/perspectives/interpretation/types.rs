//! The data types that flow across the interpretation pipeline — kept in one
//! place so the read/write/dedup/prompt submodules share a single definition
//! rather than each owning a slice of the vocabulary.
//!
//! - [`ProposedInstance`] — the LLM's parsed output (from `parse`).
//! - [`InstanceContext`] / [`ExistingInstances`] — the existing-graph snapshot
//!   (produced by `graph::read`, consumed by `prompt` + `dedup`).
//! - [`InterpretationOp`] — a planned write (produced by `graph::write`,
//!   applied by `run`).

use crate::types::Link;
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap, HashSet};

/// One instance the LLM proposes writing: the target class name plus a flat
/// map of field-name -> value. Extra/unknown fields are tolerated (kept in
/// `props`); `create_subject` only writes those that have a declared
/// `ad4m://setter` on the class, so unknown fields never become links.
///
/// `id` is the upsert marker. When present it names an instance already in the
/// graph (one the model was shown in that class's `existing` list): the
/// proposal patches that instance's scalar fields, leaving its type flag in
/// place. When absent, a fresh instance is minted. Relation-typed fields carry
/// *refs* rather than values — see `plan_interpretation_ops_with_context`.
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct ProposedInstance {
    pub class: String,
    #[serde(default)]
    pub id: Option<String>,
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}

/// One flow-transition the LLM proposes advancing to, in response to the
/// `active_flows` block in the prompt (see `render_active_flow_for_prompt`).
///
/// The LLM emits only three fields — the rest of the on-graph
/// `FlowTransitionProposal` (proposer DID, `proposedAt`, evidence hashes) is
/// filled in by the engine post-processing pass so the LLM never has to invent
/// crypto identity or run model_queries.
///
/// - `instance` names the FlowInstance URI (`active_flows[i].instance` in the
///   prompt).
/// - `to_state` names one of that instance's `nextStates[j].name` values.
/// - `reason` is an optional short natural-language attribution the LLM
///   attaches to explain why this proposal fires; the engine stores it as a
///   note on the proposal.
///
/// The engine still requires the state's `requires` guard to be satisfied
/// before it fires the proposal — an LLM proposal without evidence is
/// discarded silently (see slice 10.6c). This mirrors design §5.4 step 5:
/// LLM proposal + evidence → store; no LLM + evidence → engine emits; LLM
/// only → discard (LLM cannot bypass the deterministic guard).
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct LlmFlowProposal {
    pub instance: String,
    #[serde(rename = "toState")]
    pub to_state: String,
    #[serde(default)]
    pub reason: Option<String>,
}

/// The full structured payload the LLM returns from one interpretation call:
/// the extracted instances plus optional flow-transition proposals. Both
/// vectors default to empty so a legacy bare-array response (pre-slice-10.6)
/// still parses via [`parse_interpretation_response`]'s array→wrapper
/// fallback, and a response that omits `flow_proposals` entirely just yields
/// an empty vector on that field.
#[derive(Debug, Clone, Deserialize, PartialEq, Default)]
pub struct InterpretationOutput {
    #[serde(default)]
    pub instances: Vec<ProposedInstance>,
    #[serde(default)]
    pub flow_proposals: Vec<LlmFlowProposal>,
}

/// The existing instances in scope for an interpretation pass, keyed by base
/// URI (`id`). A single base can conform to multiple subject classes, so each
/// key maps to a `Vec` of entries. This is the **single source of truth** the
/// whole pass reads: the prompt view ([`build_interpretation_input`]), the
/// deterministic dedup safety net ([`filter_already_present`] / semantic), and
/// Create-vs-Update routing ([`plan_interpretation_ops_with_context`]) all
/// project what they need from this one map.
pub type ExistingInstances = HashMap<String, Vec<InstanceContext>>;

/// The relation edges already present in the graph for an interpretation pass,
/// as canonical `(source, predicate, target)` triples. Threaded into the planner
/// ([`plan_interpretation_ops_resolved`]) so a repeated continuous pass does not
/// re-emit a relation link that already exists — keeping the additive
/// [`InterpretationOp::AddLinks`] idempotent across passes (James #883 #4).
///
/// Without this guard, additive-only AddLinks re-minted a duplicate edge for an
/// unchanged single-cardinality (`hasOne`) relation on every pass and — because
/// each stored link's reifier IRI hashes in its *timestamp* (see
/// `sparql_store::make_reifier_iri`) — a fresh reifier node too. Skipping the
/// re-emission upstream prevents both.
pub type ExistingLinks = HashSet<(String, String, String)>;

/// One existing instance the interpreter should know about — the LLM sees these
/// so it can decide whether an interpreted item is a genuinely new node (no `id`
/// on the output) or the continuation/refinement of an existing one (emit this
/// entry's `id` to trigger the upsert path in
/// [`plan_interpretation_ops_with_context`]).
///
/// `class` is redundant with the enclosing map key, but kept on each row so the
/// JSON entry rendered into the prompt is self-contained and unambiguous when
/// the LLM scans a mixed-class list.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceContext {
    /// Base URI of the existing instance — what the LLM emits as `id` to update.
    pub id: String,
    /// The class's declared `identity` value (usually `title`), decoded. Raw,
    /// not normalized: the prompt shows it to the LLM verbatim, and
    /// [`filter_already_present`] normalizes both sides when comparing.
    pub title: String,
    /// Local class name (e.g. "Task"), matching the map key of the returned map.
    pub class: String,
    /// Currently-set secondary scalar values, keyed by property name. Excludes
    /// the identity property (already rendered as `title`), the class's type
    /// flag, and every relation. Empty when the class declares no other
    /// scalars, or when this instance has none set. Rendered into the prompt
    /// so the LLM sees the existing instance's *state* — not just its
    /// identity label — and can better judge whether a new turn continues an
    /// existing instance or belongs to a fresh one on a different topic.
    /// `BTreeMap` for deterministic prompt ordering across calls.
    pub properties: BTreeMap<String, String>,
}

/// One transcript turn the interpretation engine (and AutoProcessor gather)
/// pass around: the speaker DID, the message body, and the body-link's
/// `ad4m://ontology/timestamp` (RFC3339). `timestamp` may be empty for
/// one-shot callers that never gathered via SPARQL; AutoProcessor scope
/// queries must bind it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TranscriptTurn {
    pub speaker: String,
    pub text: String,
    pub timestamp: String,
}

impl TranscriptTurn {
    pub fn from_speaker_text(speaker: impl Into<String>, text: impl Into<String>) -> Self {
        Self {
            speaker: speaker.into(),
            text: text.into(),
            timestamp: String::new(),
        }
    }
}

/// A single write the interpreter wants to make.
///
/// Post-#884 the scalar write path is `create_subject` / `update_subject`, which
/// own literal encoding (each property's `ad4m://setter` + `resolveLanguage`).
/// So `Create` and `Update` carry the *values* to write, not pre-encoded links —
/// they differ only in whether the class constructor runs (minting the type
/// flag). `AddLinks` is the one op that still carries raw links: relation targets
/// are instance URIs, so there is nothing to encode.
#[derive(Debug, Clone, PartialEq)]
pub enum InterpretationOp {
    /// Mint a new instance at `base`: constructor (type flag) + setters.
    Create {
        base: String,
        class: String,
        values: serde_json::Map<String, serde_json::Value>,
    },
    /// Patch the scalar fields of an existing instance, leaving its type flag in
    /// place — this is how the interpreter grows/refines a tree node (Flux
    /// "grouping": continue an existing subgroup vs. start a new one). Same
    /// per-predicate replace semantics as `Create`, minus the constructor.
    Update {
        base: String,
        class: String,
        values: serde_json::Map<String, serde_json::Value>,
    },
    /// Append relation links onto an instance. Purely additive — a relation to a
    /// freshly-minted node grows the graph and must not clear sibling relations
    /// (unlike scalar `Update`, which replaces-per-predicate). Removing a
    /// relation is out of scope (Phase 3 semantic diff).
    AddLinks { source: String, links: Vec<Link> },
}
