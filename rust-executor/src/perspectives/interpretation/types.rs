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

/// The existing instances in scope for an interpretation pass, keyed by base
/// URI (`id`). This is the **single source of truth** the whole pass reads:
/// the prompt view ([`build_interpretation_input`]), the deterministic dedup
/// safety net ([`filter_already_present`] / semantic), and Create-vs-Update
/// routing ([`plan_interpretation_ops_with_context`]) all project what they
/// need from this one map — instead of separately-threaded `class → identity`
/// and `id-set` views that could drift out of sync. Each [`InstanceContext`]
/// still carries its own `id`, so the value is self-describing; the key is
/// that same id, promoted for O(1) "does the graph hold this id?" checks.
pub type ExistingInstances = HashMap<String, InstanceContext>;

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
