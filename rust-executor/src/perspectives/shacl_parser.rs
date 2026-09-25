use crate::types::Link;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

mod role_gate_keys;

/// AD4M Action - represents a link operation (e.g., addLink, removeLink, setSingleTarget)
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AD4MAction {
    pub action: String,
    pub source: String,
    pub predicate: String,
    pub target: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub local: Option<bool>,
}

/// SHACL Shape structure (from TypeScript)
#[derive(Debug, Deserialize, Serialize)]
pub struct SHACLShape {
    pub target_class: String,
    pub properties: Vec<PropertyShape>,
    /// Natural-language hint describing what this class represents, used to steer
    /// LLM interpretation (generic "English hint → model instance" mechanism).
    /// Emitted as an `ad4m://interpretation_hint` link.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub interpretation_hint: Option<String>,
    /// Constructor actions for creating instances
    #[serde(default)]
    pub constructor_actions: Vec<AD4MAction>,
    /// Destructor actions for removing instances
    #[serde(default)]
    pub destructor_actions: Vec<AD4MAction>,
}

/// A single structured conformance condition for relation filtering.
/// DB-agnostic representation that can be translated to any query language.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ConformanceCondition {
    /// Type of check: "flag" (predicate + value) or "required" (predicate exists)
    #[serde(rename = "type")]
    pub condition_type: String,
    /// The predicate URI to check on the target node
    pub predicate: String,
    /// For "flag" conditions: the expected value
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
}

/// SHACL Property Shape structure
#[derive(Debug, Deserialize, Serialize)]
pub struct PropertyShape {
    pub path: String,
    pub name: Option<String>,
    /// Natural-language hint describing this property's meaning, injected into the
    /// interpretation prompt / generated tool schema as semantic guidance for the LLM.
    /// Emitted as an `ad4m://interpretation_hint` link on the property node.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub interpretation_hint: Option<String>,
    /// Marks this property as the class's dedup identity (the "title-like"
    /// interpretation key). Emitted as an `ad4m://identity` link on the
    /// property node when `Some(true)`. No identity declared ⇒ no dedup.
    #[serde(default)]
    pub identity: Option<bool>,
    /// CRDT ordering strategy for a collection relation, from
    /// `@HasMany({ ordering: { strategy } })`. Emitted as an `ad4m://ordering`
    /// link on the property node, which is the only place
    /// [`load_shape`](super::model_query::shape) reads it back from — so
    /// dropping it here leaves the declaration inert: the setter writes no
    /// ordering entries and hydration never reorders.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ordering: Option<String>,
    pub datatype: Option<String>,
    pub min_count: Option<u32>,
    pub max_count: Option<u32>,
    pub writable: Option<bool>,
    pub local: Option<bool>,
    /// Sole selector of storage mode. `None` → deterministic typed
    /// literal (fast POS-index path, the default). `Some("literal")` →
    /// signed envelope on the built-in literal language. `Some(<addr>)`
    /// → expression on that custom language. Stored as `ad4m://resolveLanguage`.
    pub resolve_language: Option<String>,
    pub node_kind: Option<String>,
    pub collection: Option<bool>,
    /// Setter action for single-valued properties
    #[serde(default)]
    pub setter: Vec<AD4MAction>,
    /// Adder action for collection properties
    #[serde(default)]
    pub adder: Vec<AD4MAction>,
    /// Remover action for collection properties
    #[serde(default)]
    pub remover: Vec<AD4MAction>,
    /// Pre-computed getter expression for reading this relation/property.
    /// For relations with a target model, this encodes conformance filtering.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub getter: Option<String>,
    /// Structured conformance conditions (DB-agnostic).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub conformance_conditions: Vec<ConformanceCondition>,
    /// Target SHACL node shape URI (sh:class). When present, linked nodes
    /// must conform to this shape, enabling typed construction.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub class: Option<String>,
    /// Kind of relation this property describes. One of "hasMany", "hasOne",
    /// "belongsToOne", "belongsToMany".  Drives direction (forward/reverse)
    /// and scalar-vs-collection rendering.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relation_kind: Option<String>,
    /// Bare target class name for a relation property — used by the executor
    /// to look up the target shape through its in-memory cache.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_class_name: Option<String>,
    /// Post-getter where-clause filter for relation properties.  Keys are
    /// property names on the target class; values follow the where-clause DSL.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub where_filter: Option<serde_json::Value>,
    /// Predicate IRI lookup for `where_filter` keys (property name → predicate).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub where_predicates: Option<std::collections::HashMap<String, String>>,
    /// Whether conformance/type filtering is enabled for this relation.
    /// Omitted (defaulting to true) when not explicitly disabled.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<bool>,
    /// Fixed value constraint (sh:hasValue).  When combined with min_count >= 1
    /// the property is interpreted as a `@Flag` — its presence + value mark
    /// the instance as belonging to the class.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub has_value: Option<String>,
    /// Transform expression (SHACL-AF Node Expression).
    /// Serialized as JSON and stored as a `literal:string:` link.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transform: Option<serde_json::Value>,
}

// ============================================================================
// SHACL Flow structures (state machines without Prolog)
// ============================================================================

/// Link pattern for state detection
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LinkPattern {
    /// Optional source pattern (if omitted, uses the expression address)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    /// Required predicate to match
    pub predicate: String,
    /// Required target value to match
    pub target: String,
}

/// A per-property condition for a `ModelQuery` — the shape the flow
/// engine evaluates against a class instance's decoded value. Mirrors
/// `PropertyCondition` in `core/src/shacl/SHACLFlow.ts`.
///
/// String / number / boolean shorthands compile to an equality match;
/// the object forms are the full spec. `#[serde(untagged)]` — the JSON
/// on the wire is either a scalar (`"foo"` / `42` / `true`) or one of
/// the object variants; readers dispatch on shape.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum PropertyCondition {
    Str(String),
    Num(f64),
    Bool(bool),
    Equals {
        equals: serde_json::Value,
    },
    In {
        #[serde(rename = "in")]
        one_of: Vec<serde_json::Value>,
    },
    Exists {
        exists: bool,
    },
    Matches {
        matches: String,
    },
}

/// Model-level query — a flow's `requires` guard evaluates an array of
/// these against the perspective's current class-instance graph. Mirrors
/// `ModelQuery` in `core/src/shacl/SHACLFlow.ts`. All fields optional
/// except `className` to match the TS shape.
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ModelQuery {
    /// Subject-class URI to search for.
    #[serde(rename = "className")]
    pub class_name: String,
    /// Per-property conditions (AND semantics inside one query;
    /// AND semantics across an array of queries too).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub r#where: Option<std::collections::BTreeMap<String, PropertyCondition>>,
    /// Cardinality constraint on matching instances.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub count: Option<ModelQueryCount>,
    /// How the matched instance connects back to the flow.
    #[serde(rename = "linkedTo", default, skip_serializing_if = "Option::is_none")]
    pub linked_to: Option<serde_json::Value>,
    /// DID-property gate — restricts matches to instances whose named
    /// property equals a specific DID. Threaded through by role checks
    /// (§7.2). Value is a template variable (`"$did"`) resolved at
    /// evaluation time.
    #[serde(
        rename = "didProperty",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub did_property: Option<String>,
    /// Composed OR — matches if ANY of the sub-queries do. Mirrors the
    /// TS `or?: ModelQuery[]` field (§7.3 multi-role composition).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub or: Option<Vec<ModelQuery>>,
    /// Role membership granted by another flow completing: each matched
    /// instance must be a valid output of that flow, and the grant is dated
    /// from the run's quorum. Only meaningful on a `fromRole` query; see
    /// [`grant`](crate::perspectives::flow_instance::grant) for the semantics,
    /// the failure directions, and — importantly for anyone configuring one —
    /// what it takes to un-grant.
    #[serde(
        rename = "producedByFlow",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub produced_by_flow: Option<ProducedByFlow>,
}

/// A `fromRole` gate that additionally requires each matched instance to be a
/// valid output of a completed run of a named flow, and dates the grant from
/// that run's quorum rather than from an assignment link.
///
/// The same name and shape as the model-query filter
/// `where: { producedByFlow: { flow, state? } }` (#1127), and decided by the
/// same check ([`produced`](crate::perspectives::flow_instance::produced)).
/// The one difference is that `state` is **required** here: without it, a
/// run that settled into a flow's `rejected` state would grant what its
/// `approved` state was meant to.
///
/// Mirrors `ProducedByFlow` in `core/src/shacl/SHACLFlow.ts`.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct ProducedByFlow {
    /// The granting flow's `flow_uri()` — `{namespace}{name}Flow`. Compared
    /// with the receipt's own `flow_uri`.
    pub flow: String,
    /// The state that run must have settled into, compared with the state the
    /// verifier's **own** fold re-derived, never the one the receipt asserts.
    pub state: String,
}

/// `count` shape on a `ModelQuery`. Default `{ min: 1 }` — at least one
/// match required to satisfy the guard.
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ModelQueryCount {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub min: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max: Option<u32>,
}

/// Consensus firing rule — how many distinct DIDs must sign a proposal
/// before the engine advances the flow to the state. Mirrors
/// `ConsensusRule` in `core/src/shacl/SHACLFlow.ts` §7.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ConsensusRule {
    /// Distinct-DID threshold. `1` = solo-actor / like-button semantics.
    pub n: u32,
    /// Optional role gate: a signer must satisfy this ModelQuery
    /// (with `$did` substituted) to count toward the threshold.
    #[serde(rename = "fromRole", default, skip_serializing_if = "Option::is_none")]
    pub from_role: Option<ModelQuery>,
}

/// What the `consensusRule` predicate at ONE scope (the flow, or one state)
/// amounts to after parsing.
///
/// `Option<ConsensusRule>` cannot express this: it collapses *"the author
/// wrote no rule"* and *"the author wrote a rule and it did not decode"* into
/// the same `None`, and the consumer then applies the permissive default to
/// both (#1078). Those two must stay separable all the way to the consumer,
/// because they warrant opposite answers — see [`crate::perspectives::flow_instance::fold::rule_for`].
#[derive(Debug, Clone)]
pub enum ConsensusRuleSlot<'a> {
    /// No `consensusRule` link at this scope. Defer to the next scope out.
    Absent,
    /// A rule was authored here and decoded.
    Rule(&'a ConsensusRule),
    /// A `consensusRule` was authored at this scope and CANNOT BE READ. The
    /// author's intent is unknown and unrecoverable, either way:
    ///
    /// - the literal did not decode (#1078), or
    /// - two or more *different* literals are present on the one source, so
    ///   no reader can say which the author meant (#1080 variant 1b) — see
    ///   [`read_consensus_rule`].
    ///
    /// At STATE scope there is a third way in, and it is about the scope
    /// rather than the literal: two different states answer to this state's
    /// `name`, so the name does not identify a scope to read a rule from at
    /// all (#1082) — see [`refuse_shadowed_state_names`]. The state may well
    /// carry a perfectly readable rule; what is unreadable is which state the
    /// consumer is asking about.
    ///
    /// All of them reach the same consumer verdict, which is why they share a
    /// variant: the distinction matters to whoever fixes the data, and the
    /// warning log carries it, but it does not change what the engine may do.
    Malformed,
}

/// Build a slot from the two fields the parser writes. Keeping this in one
/// place is what stops a reader from checking `consensus_rule` and forgetting
/// `consensus_rule_malformed`.
///
/// `(Some(rule), true)` is contradictory and resolves to `Malformed`, not to
/// the rule. The parser never writes that pair — `read_consensus_rule`
/// returns a rule or the flag, never both — so this arm is a backstop rather
/// than a live branch. It is fail-closed on purpose: the pair means one
/// writer both found a rule and concluded the scope is unreadable, and
/// reading it as permission is the failure mode #1064 and #1079 both settled
/// against. Anyone hand-building a `SHACLFlow` (the fold tests do) gets the
/// refusal, not the rule.
fn slot_of(rule: Option<&ConsensusRule>, malformed: bool) -> ConsensusRuleSlot<'_> {
    match (rule, malformed) {
        (_, true) => ConsensusRuleSlot::Malformed,
        (Some(r), false) => ConsensusRuleSlot::Rule(r),
        (None, false) => ConsensusRuleSlot::Absent,
    }
}

/// Flow State definition
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FlowState {
    /// State name (e.g., "ready", "doing", "done")
    pub name: String,
    /// Numeric state value for ordering (e.g., 0, 0.5, 1)
    pub value: f64,
    /// English description of what puts a flow instance IN this state.
    /// Read by the extraction pass to steer the LLM's state-transition
    /// suggestions. Mirrors `FlowState.interpretationHint` on the TS side.
    #[serde(
        rename = "interpretationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub interpretation_hint: Option<String>,
    /// Model-level guard: state is satisfied when every ModelQuery in
    /// the array returns at least one match on committed graph state.
    /// AND semantics across the array. Empty / unset = no model-level
    /// guard.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub requires: Option<Vec<ModelQuery>>,
    /// English hint for a targeted LLM confirmation after `requires`
    /// matches. Unset = `requires` matches directly imply state entered.
    #[serde(
        rename = "semanticCheck",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub semantic_check: Option<String>,
    /// Per-state consensus override. Unset = falls back to the flow's
    /// top-level `consensus_rule`.
    #[serde(
        rename = "consensusRule",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub consensus_rule: Option<ConsensusRule>,
    /// Set when the rule governing entry into this state cannot be read:
    /// its `consensusRule` literal did not decode, or the state's source
    /// carried two different ones (#1078/#1080), or another state claims this
    /// state's `name` so the name identifies no single rule at all (#1082 —
    /// [`refuse_shadowed_state_names`]). Never read directly — go through
    /// [`FlowState::consensus_rule_slot`], which is the only place the pair
    /// is interpreted.
    #[serde(
        rename = "consensusRuleMalformed",
        default,
        skip_serializing_if = "std::ops::Not::not"
    )]
    pub consensus_rule_malformed: bool,
}

impl FlowState {
    /// This state's `consensusRule` scope. See [`ConsensusRuleSlot`].
    pub fn consensus_rule_slot(&self) -> ConsensusRuleSlot<'_> {
        slot_of(self.consensus_rule.as_ref(), self.consensus_rule_malformed)
    }
}

/// Flow Transition definition
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FlowTransition {
    /// Name of this action (shown to users, e.g., "Start", "Finish")
    pub action_name: String,
    /// State to transition from
    pub from_state: String,
    /// State to transition to
    pub to_state: String,
    /// Actions to execute for this transition
    pub actions: Vec<AD4MAction>,
}

/// SHACL Flow structure - state machine definition
///
/// `Clone` so a caller can put a definition into its own catalogue without
/// re-parsing the graph; every field was already `Clone`.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SHACLFlow {
    /// Flow name (e.g., "TODO"). Human-readable label; NOT the identity
    /// used in cross-module joins (see [`SHACLFlow::flow_uri`]).
    pub name: String,
    /// Namespace for URIs (e.g., "todo://")
    pub namespace: String,
    /// States in this flow
    #[serde(default)]
    pub states: Vec<FlowState>,
    /// Transitions between states
    #[serde(default)]
    pub transitions: Vec<FlowTransition>,
    /// Top-level frame — English description of what the flow is about.
    /// Read by the extraction pass. Mirrors
    /// `SHACLFlow.interpretationHint` on the TS side.
    #[serde(
        rename = "interpretationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub interpretation_hint: Option<String>,
    /// Subject-class URIs the flow accepts as its base (replaces the
    /// legacy `flowable` field on TS). Mirrors `SHACLFlow.inputTypes`.
    #[serde(rename = "inputTypes", default)]
    pub input_types: Vec<String>,
    /// Subject-class URIs the flow must produce at least one instance
    /// of before it can complete. Mirrors `SHACLFlow.outputTypes`.
    #[serde(rename = "outputTypes", default)]
    pub output_types: Vec<String>,
    /// English hint for how to recognize when a new instance of this
    /// flow should be spawned on a candidate base. Read by the LLM
    /// during the extraction pass — a match ⇒ propose a `startFlow`.
    /// Mirrors `SHACLFlow.creationHint`.
    #[serde(
        rename = "creationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub creation_hint: Option<String>,
    /// Extra ModelQueries pulled into the LLM prompt as BACKGROUND
    /// context (NOT evidence for `requires`). Mirrors `SHACLFlow.context`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub context: Option<Vec<ModelQuery>>,
    /// Flow-level default consensus rule. States without their own
    /// `consensus_rule` inherit this. For zero-state flows (§4.1.1 —
    /// like-button-shape actions), this IS the single consensus rule.
    /// Mirrors `SHACLFlow.consensusRule`.
    #[serde(
        rename = "consensusRule",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub consensus_rule: Option<ConsensusRule>,
    /// Set when the flow carried a top-level `consensusRule` link whose
    /// literal did not decode. Never read directly — go through
    /// [`SHACLFlow::consensus_rule_slot`].
    #[serde(
        rename = "consensusRuleMalformed",
        default,
        skip_serializing_if = "std::ops::Not::not"
    )]
    pub consensus_rule_malformed: bool,
}

impl SHACLFlow {
    /// The flow-level `consensusRule` scope. See [`ConsensusRuleSlot`].
    pub fn consensus_rule_slot(&self) -> ConsensusRuleSlot<'_> {
        slot_of(self.consensus_rule.as_ref(), self.consensus_rule_malformed)
    }

    /// Canonical URI of this flow (`${namespace}${name}Flow`, e.g.
    /// `coasys://DeliveryFlow`). This is the identity used for
    /// cross-community joins — `FlowInstanceRecord.flow_uri` stores it,
    /// and `build_flow_contexts` keys its shape lookup on it. See James
    /// PR #929 R5.
    pub fn flow_uri(&self) -> String {
        format!("{}{}Flow", self.namespace, self.name)
    }
}

/// Parse Flow JSON to RDF links
pub fn parse_flow_to_links(flow_json: &str, flow_name: &str) -> Result<Vec<Link>, AnyError> {
    let flow: SHACLFlow = serde_json::from_str(flow_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse Flow JSON: {}", e))?;

    let mut links = Vec::new();

    let flow_uri = format!("{}{}Flow", flow.namespace, flow_name);

    // Flow type
    links.push(Link {
        source: flow_uri.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://Flow".to_string(),
    });

    // Flow name
    links.push(Link {
        source: flow_uri.clone(),
        predicate: Some("ad4m://flowName".to_string()),
        target: format!("literal:string:{}", urlencoding::encode(flow_name)),
    });

    // Flow-level `interpretationHint` — English frame the LLM sees at the
    // top of the "Active flows" prompt block. Empty-string is treated as
    // unset (mirrors the TS writer: emitting a meaningless empty predicate
    // would round-trip as a real value that consumers then have to filter).
    if let Some(hint) = flow
        .interpretation_hint
        .as_deref()
        .filter(|s| !s.is_empty())
    {
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://interpretationHint".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(hint)),
        });
    }

    // Typed I/O — only serialised when non-empty (empty ≡ unset for
    // round-trip fidelity; matches the TS writer).
    if !flow.input_types.is_empty() {
        let json = serde_json::to_string(&flow.input_types)
            .map_err(|e| anyhow::anyhow!("Failed to serialize inputTypes: {}", e))?;
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://inputTypes".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&json)),
        });
    }
    if !flow.output_types.is_empty() {
        let json = serde_json::to_string(&flow.output_types)
            .map_err(|e| anyhow::anyhow!("Failed to serialize outputTypes: {}", e))?;
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://outputTypes".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&json)),
        });
    }

    // Flow-level `creationHint` — the "when to spawn this flow" English
    // hint the interpretation engine reads to decide whether a base
    // expression warrants a new FlowInstance.
    if let Some(hint) = flow.creation_hint.as_deref().filter(|s| !s.is_empty()) {
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://creationHint".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(hint)),
        });
    }

    // Flow-level `context` — background ModelQueries the LLM sees in the
    // prompt but that do NOT count toward `requires` guards on states.
    // Single JSON literal (matches the reader / TS writer).
    if let Some(ctx) = flow.context.as_ref().filter(|c| !c.is_empty()) {
        let json = serde_json::to_string(ctx)
            .map_err(|e| anyhow::anyhow!("Failed to serialize context: {}", e))?;
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://context".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&json)),
        });
    }

    // Flow-level `consensusRule` — default for state transitions when a
    // FlowState omits its own rule; also the terminal-firing rule for
    // zero-state flows.
    if let Some(rule) = flow.consensus_rule.as_ref() {
        let json = serde_json::to_string(rule)
            .map_err(|e| anyhow::anyhow!("Failed to serialize consensusRule: {}", e))?;
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://consensusRule".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&json)),
        });
    }

    // States
    for state in &flow.states {
        let state_uri = format!("{}{}.{}", flow.namespace, flow_name, state.name);

        // Link flow to state
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://hasState".to_string()),
            target: state_uri.clone(),
        });

        // State type
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://FlowState".to_string(),
        });

        // State name
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("ad4m://stateName".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&state.name)),
        });

        // State value
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("ad4m://stateValue".to_string()),
            target: format!("literal:number:{}", state.value),
        });

        // Per-state `interpretationHint` — the English hint the LLM sees
        // for each reachable next-state in the active-flow prompt block.
        // Empty-string treated as unset (round-trip parity with TS).
        if let Some(hint) = state
            .interpretation_hint
            .as_deref()
            .filter(|s| !s.is_empty())
        {
            links.push(Link {
                source: state_uri.clone(),
                predicate: Some("ad4m://interpretationHint".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(hint)),
            });
        }

        // Per-state `requires` guard — ModelQuery[] the state-transition
        // engine evaluates against the graph after each extraction pass.
        // Single JSON literal on `ad4m://requires`; consumers parse back
        // (see [`decode_model_query_array`]).
        if let Some(qs) = state.requires.as_ref().filter(|q| !q.is_empty()) {
            let json = serde_json::to_string(qs)
                .map_err(|e| anyhow::anyhow!("Failed to serialize requires: {}", e))?;
            links.push(Link {
                source: state_uri.clone(),
                predicate: Some("ad4m://requires".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(&json)),
            });
        }

        // Per-state `semanticCheck` — English prompt for the second-pass
        // LLM confirmation the engine runs after `requires` matches.
        if let Some(check) = state.semantic_check.as_deref().filter(|s| !s.is_empty()) {
            links.push(Link {
                source: state_uri.clone(),
                predicate: Some("ad4m://semanticCheck".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(check)),
            });
        }

        // Per-state `consensusRule` — overrides the flow-level rule
        // when set (e.g. a Resolution state that needs a quorum even
        // though the flow default is 1 signer).
        if let Some(rule) = state.consensus_rule.as_ref() {
            let json = serde_json::to_string(rule)
                .map_err(|e| anyhow::anyhow!("Failed to serialize state consensusRule: {}", e))?;
            links.push(Link {
                source: state_uri.clone(),
                predicate: Some("ad4m://consensusRule".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(&json)),
            });
        }
    }

    // Transitions
    for transition in &flow.transitions {
        let transition_uri = format!(
            "{}{}.{}To{}",
            flow.namespace, flow_name, transition.from_state, transition.to_state
        );
        let from_state_uri = format!("{}{}.{}", flow.namespace, flow_name, transition.from_state);
        let to_state_uri = format!("{}{}.{}", flow.namespace, flow_name, transition.to_state);

        // Link flow to transition
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://hasTransition".to_string()),
            target: transition_uri.clone(),
        });

        // Transition type
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://FlowTransition".to_string(),
        });

        // Action name
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://actionName".to_string()),
            target: format!(
                "literal:string:{}",
                urlencoding::encode(&transition.action_name)
            ),
        });

        // From state
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://fromState".to_string()),
            target: from_state_uri,
        });

        // To state
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://toState".to_string()),
            target: to_state_uri,
        });

        // Transition actions
        if !transition.actions.is_empty() {
            let actions_json = serde_json::to_string(&transition.actions)
                .map_err(|e| anyhow::anyhow!("Failed to serialize transition actions: {}", e))?;
            links.push(Link {
                source: transition_uri.clone(),
                predicate: Some("ad4m://transitionActions".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(&actions_json)),
            });
        }
    }

    Ok(links)
}

// ---------------------------------------------------------------------------
// Reverse of parse_flow_to_links — read flow definitions off the graph.
// Mirrors the canonical TS `SHACLFlow.fromLinks` in core/src/shacl/SHACLFlow.ts
// including every declared predicate. Consumed by Model C (`load_shacl_flows`
// → `build_flow_contexts` → prompt block) so the extraction pass can see what
// flows are declared on the perspective without a JS/RPC round-trip.
// ---------------------------------------------------------------------------

/// Strip a `literal:string:` / `literal://string:` prefix and url-decode
/// the tail. Returns `None` when the target isn't a string literal or
/// when the url-decoded tail isn't valid UTF-8.
fn decode_literal_string(target: &str) -> Option<String> {
    let payload = target
        .strip_prefix("literal://string:")
        .or_else(|| target.strip_prefix("literal:string:"))?;
    urlencoding::decode(payload).ok().map(|c| c.into_owned())
}

/// Strip a `literal:number:` / `literal://number:` prefix and parse
/// the tail as f64. Both prefix shapes are accepted so wire-format
/// migration doesn't require reprocessing every flow node.
fn decode_literal_number(target: &str) -> Option<f64> {
    let payload = target
        .strip_prefix("literal://number:")
        .or_else(|| target.strip_prefix("literal:number:"))?;
    payload.parse().ok()
}

/// How much of an offending literal a warning quotes. Flow literals are
/// author-written JSON; enough to spot the typo, bounded so a pathological
/// target can't dominate the log.
const LITERAL_EXCERPT_LEN: usize = 200;

fn excerpt(target: &str) -> String {
    match target.char_indices().nth(LITERAL_EXCERPT_LEN) {
        Some((cut, _)) => format!("{}…", &target[..cut]),
        None => target.to_string(),
    }
}

/// Decode a JSON payload stored inside a `literal:string:<urlencoded json>`
/// target.
///
/// Returns `Err` — carrying a human-readable reason — rather than `None`, so
/// callers can tell *"the author wrote something and it did not decode"* from
/// *"the author wrote nothing"*. The link's presence is what says a value was
/// intended; the two cases are only distinguishable here, at the decode, and
/// a caller that collapses them can no longer recover the difference
/// (this is the defect in #1078).
///
/// Callers for whom the distinction genuinely does not matter should use
/// [`decode_json_literal_lossy`], which logs and discards.
fn decode_json_literal<T: for<'de> Deserialize<'de>>(target: &str) -> Result<T, String> {
    let s = decode_literal_string(target)
        .ok_or_else(|| "target is not a url-decodable `literal:string:`".to_string())?;
    serde_json::from_str(&s).map_err(|e| e.to_string())
}

/// [`decode_json_literal`] for fields where a malformed value and an absent
/// one lead to the same behaviour: warn, then leave the field unset. The
/// warning is the whole point — before it, a dropped literal was invisible.
fn decode_json_literal_lossy<T: for<'de> Deserialize<'de>>(
    target: &str,
    predicate: &str,
) -> Option<T> {
    decode_json_literal(target)
        .map_err(|e| {
            log::warn!(
                "flow: ignoring malformed `{predicate}` literal `{}`: {e}",
                excerpt(target)
            );
        })
        .ok()
}

fn find_link<'a>(links: &'a [Link], source: &str, predicate: &str) -> Option<&'a Link> {
    links
        .iter()
        .find(|l| l.source == source && l.predicate.as_deref() == Some(predicate))
}

fn find_links<'a>(links: &'a [Link], source: &str, predicate: &str) -> Vec<&'a Link> {
    links
        .iter()
        .filter(|l| l.source == source && l.predicate.as_deref() == Some(predicate))
        .collect()
}

/// Decode one `consensusRule` literal. A key the reader does not know is an
/// error, the same as a missing required field: serde would drop it, and on a
/// role gate a dropped key widens the gate or removes it (#1144). See
/// [`role_gate_keys`].
fn decode_consensus_rule(target: &str) -> Result<ConsensusRule, String> {
    let value: serde_json::Value = decode_json_literal(target)?;
    let errors = role_gate_keys::role_gate_key_errors(&value);
    if !errors.is_empty() {
        return Err(errors.join("; "));
    }
    serde_json::from_value(value).map_err(|e| e.to_string())
}

/// Read the `consensusRule` at ONE scope — a flow URI, or one state URI —
/// into the `(rule, unreadable)` pair the parser stores. Both scopes go
/// through here so that one place decides what an unreadable rule is.
///
/// # Why this selects with `find_links`, not `find_link`
///
/// At this predicate the *selection* can fail, not just the decode. The
/// parser works over a bag of plain [`Link`]s:
/// `flow_context::loader::load_shacl_flows` collects
/// `DecoratedLinkExpression::data`, which drops `author` and `proof`, and
/// [`LinkQuery`](crate::types::LinkQuery) has no author field to filter on
/// either. So when two agents have each put an `ad4m://consensusRule` link
/// on the same source, the bag holds both and carries nothing to tell them
/// apart.
///
/// The singular [`find_link`] is `.iter().find(…)` over a bag in store scan
/// order (`sparql_store::query_links` iterates RocksDB order, not
/// timestamp). It therefore broke that tie by whichever link the scan
/// happened to reach first, and silently: the same link set could gate
/// correctly on one replica and permissively on another, with no error on
/// either. That is #1080 variant 1b — the defect is a race, not a
/// certainty, which is why it cannot be fixed by ordering the bag better.
///
/// Two DIFFERENT rules for one scope is not a tie to be broken, it is
/// unreadable: nobody can say which one the author meant. So it resolves to
/// the same verdict as a literal that did not decode —
/// [`ConsensusRuleSlot::Malformed`], which
/// [`rule_for`](crate::perspectives::flow_instance::fold::rule_for) answers
/// with `Refused` (#1079). A peer who injects a competing rule now causes a
/// **refusal, not a downgrade**. Crucially this needs no authorship, which
/// is the one thing the bag cannot supply.
///
/// # Repeats of the SAME literal are accepted, and have to be
///
/// Ambiguity is counted over DISTINCT `target`s rather than over link
/// count, because the bag is a multiset that the loader itself fills with
/// repeats of one link:
///
/// - `load_shacl_flows` iterates once per `rdf://type ad4m://Flow` link,
///   not once per distinct flow URI, and its per-flow `(source = flow_uri)`
///   query re-collects links the type query already pushed;
/// - two flows that share a state URI re-collect that state's links twice.
///
/// A count-based check would therefore refuse honest flows — and would hand
/// an attacker something cheaper than the downgrade it closes, since one
/// extra `rdf://type` link would wedge every rule on that flow.
///
/// Byte-identical targets are also the right answer on their own terms. A
/// link *is* `(source, predicate, target)`; once authorship is dropped, two
/// links with equal targets are indistinguishable at this type and identical
/// in effect, so there is no ambiguity about intent to refuse. Provenance is
/// still ambiguous — one of them may be a peer's — but an injected copy of
/// the author's own rule changes no gate, so it is not this function's
/// decision to make.
///
/// Equality is on the raw literal, deliberately: two different literals that
/// would decode to equal rules are still refused. That keeps the check
/// fail-closed and needs no `PartialEq` on [`ConsensusRule`] — an equality
/// wrong in the permissive direction would reopen exactly the downgrade this
/// exists to close.
fn read_consensus_rule(links: &[Link], source: &str, scope: &str) -> (Option<ConsensusRule>, bool) {
    let mut distinct: Vec<&str> = Vec::new();
    for link in find_links(links, source, "ad4m://consensusRule") {
        if !distinct.contains(&link.target.as_str()) {
            distinct.push(&link.target);
        }
    }

    match distinct.as_slice() {
        // No link at this scope. Defer to the next scope out — `Absent`.
        [] => (None, false),

        // Exactly one authored rule. Unchanged #1079 behaviour: decode it,
        // and record a decode failure rather than erasing it.
        [only] => match decode_consensus_rule(only) {
            Ok(rule) => (Some(rule), false),
            Err(e) => {
                log::warn!(
                    "{scope}: `consensusRule` literal `{}` did not decode ({e}); \
                     transitions governed by it will be REFUSED, not defaulted",
                    excerpt(only)
                );
                (None, true)
            }
        },

        // Contested scope. Report `unreadable` and — this is the part that
        // matters — decode NOTHING into `rule`. Returning a rule here
        // alongside the flag would still gate on it, because
        // `slot_of(Some(r), _)` reads a present rule as the verdict.
        many => {
            log::warn!(
                "{scope}: {} DIFFERENT `consensusRule` literals on one source [{}]; \
                 the authored rule is unreadable, so transitions governed by it will \
                 be REFUSED rather than settled by store scan order",
                many.len(),
                many.iter()
                    .map(|t| format!("`{}`", excerpt(t)))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            (None, true)
        }
    }
}

/// Mark every state whose `name` is claimed by more than one state as
/// unreadable, so that transitions into that name are REFUSED rather than
/// governed by whichever state the scan happened to reach first (#1082).
///
/// `states[i]` must correspond to `state_uris[i]`; call this before the sort.
///
/// # What it closes
///
/// Nothing dedupes `flow.states` by name, the only ordering applied is by
/// `stateValue`, and all three consumers that resolve a state by name take
/// the first match — [`super::flow_instance::fold::rule_for`],
/// [`super::flow_context::render::reachable_next_states`], and
/// `flow_evaluator::recompute_evidence_hash`. Every one of those inputs is
/// writable by any neighbourhood peer, because `load_shacl_flows` drops
/// `DecoratedLinkExpression::author` before parsing. So a peer could publish
/// a second state carrying the author's `stateName`, a lower `stateValue`,
/// and a weaker `consensusRule`; the sort put it first and its rule governed
/// — **deterministically on every replica**, since the attacker supplies the
/// sort key. #1080's ambiguity refusal cannot see this: the injected rule
/// sits on the attacker's own state URI, so each source still carries exactly
/// one rule.
///
/// # Why "unreadable" is the right verdict
///
/// This is the [`unique_field`](super::flow_instance::atom::unique_field)
/// rule, applied one level up. That reader refuses an atom outright when the
/// proposer published two different values for one field, because no reader
/// can say which was meant, and "pick the first" is an answer chosen by
/// whoever wrote last rather than by the author. Two states answering to one
/// `name` is the same shape of unreadable: the name does not identify a
/// state, and every tie-break over an unauthored bag picks a winner between
/// an author and an attacker.
///
/// # The equivalence: a state's identity is its URI
///
/// - **Byte-identical repeats are ACCEPTED, as one state.** Two `hasState`
///   links with equal `(source, predicate, target)` name the same URI, and
///   every property is re-read from that URI, so the second link cannot
///   change anything. Deduped by the caller before this runs — same decision,
///   and for the same reason, as `read_consensus_rule`'s `distinct` list
///   (#1080). It is also required, not merely defensible: `load_shacl_flows`
///   builds a multiset and re-collects links its type query already pushed,
///   so counting links rather than distinct URIs would refuse honest flows.
/// - **Same `name` on two different URIs is REFUSED — including when their
///   `stateValue`, `consensusRule` and everything else agree.** Equality is
///   on the URI, never on the derived [`FlowState`]. Defining it on the
///   derived record instead would make the refusal conditional on the
///   attacker's fields *currently* matching the author's — a condition the
///   attacker controls and can flip with one more link, at which point the
///   downgrade is back. Refusing needs no equality on `FlowState`,
///   `ConsensusRule` or `ModelQuery` at all (none of which derives
///   `PartialEq`), and an equality wrong in the permissive direction reopens
///   exactly what this closes. The cost is a loud, local wedge on data no
///   canonical writer produces: `parse_flow_to_links` emits one state URI per
///   state.
/// - **Empty names participate.** Two states that both lack a `stateName`
///   link both decode to `""` and are just as unresolvable as two named
///   `approved`. `initial_state_of` already treats a nameless state as
///   half-synced (`flow_spawn.rs`); this treats two of them as unreadable.
///
/// # Why per-name, and not a whole-flow parse failure
///
/// The blast radius is matched to the damage. A collision makes ONE name
/// unresolvable; the flow's other states are still exactly what their author
/// wrote, and refusing them too would hand an attacker a cheaper wedge than
/// the downgrade this closes — one injected link to disable a whole flow.
/// Returning `Err` would also drop the flow out of `load_shacl_flows`
/// entirely, which strands existing instances of it (their definition simply
/// disappears) — quieter and worse than a refusal at the gate, which names
/// the state in a warning and is fixed by deleting the shadow.
///
/// # What it does NOT close
///
/// The `states[0]` initial-state convention (`flow_spawn::initial_state_of`)
/// is positional, not name-based: a peer can capture it with a state carrying
/// a *fresh* name and a low `stateValue`, with no collision for this function
/// to see. That is the unauthored-bag problem (#1080/#1081) and needs the
/// loader to be author-scoped; it is out of reach from here.
fn refuse_shadowed_state_names(states: &mut [FlowState], state_uris: &[&str], flow_uri: &str) {
    debug_assert_eq!(
        states.len(),
        state_uris.len(),
        "call before the sort: state_uris[i] must still describe states[i]"
    );

    // Indices whose name another state also claims. O(n²) on a per-flow state
    // list, which is the same shape as `read_consensus_rule`'s `distinct`
    // scan and for the same reason: n is a handful.
    let shadowed: Vec<usize> = (0..states.len())
        .filter(|&i| {
            states
                .iter()
                .enumerate()
                .any(|(j, other)| j != i && other.name == states[i].name)
        })
        .collect();

    if shadowed.is_empty() {
        return;
    }

    let mut reported: Vec<&str> = Vec::new();
    for &i in &shadowed {
        let name = states[i].name.as_str();
        if reported.contains(&name) {
            continue;
        }
        reported.push(name);
        let claimants: Vec<String> = shadowed
            .iter()
            .filter(|&&j| states[j].name == states[i].name)
            .map(|&j| format!("`{}`", state_uris[j]))
            .collect();
        log::warn!(
            "flow `{flow_uri}`: state name `{}` is claimed by {} different states [{}]; \
             the name identifies none of them, so transitions into it will be REFUSED \
             rather than settled by whichever state sorts first",
            excerpt(name),
            claimants.len(),
            claimants.join(", ")
        );
    }

    for i in shadowed {
        // Both halves, deliberately. `slot_of` reads `(Some(rule), true)` as
        // `Malformed` anyway, but the parser's invariant is that it never
        // writes that pair — a decoded rule left sitting beside the flag is
        // the thing a later refactor gates on by mistake.
        states[i].consensus_rule = None;
        states[i].consensus_rule_malformed = true;
    }
}

/// Reader-side validator: a `Vec<ModelQuery>` payload is only accepted
/// when every entry has a non-empty `className` string. The `#[serde(untagged)]`
/// on `PropertyCondition` makes it too permissive to reject `[{}]` /
/// `[{"className": 42}]` at the serde layer — this catches those before
/// they end up in the returned `SHACLFlow`. Symmetric with the TS
/// `isModelQueryShape` guard.
fn model_query_array_ok(qs: &[ModelQuery]) -> bool {
    qs.iter().all(|q| !q.class_name.is_empty())
}

fn decode_model_query_array(target: &str, predicate: &str) -> Option<Vec<ModelQuery>> {
    let qs: Vec<ModelQuery> = decode_json_literal_lossy(target, predicate)?;
    if model_query_array_ok(&qs) {
        Some(qs)
    } else {
        log::warn!(
            "flow: ignoring `{predicate}` literal `{}`: every ModelQuery needs a non-empty `className`",
            excerpt(target)
        );
        None
    }
}

fn decode_string_array(target: &str, predicate: &str) -> Option<Vec<String>> {
    let arr: Vec<String> = decode_json_literal_lossy(target, predicate)?;
    if arr.iter().any(|s| s.is_empty()) {
        log::warn!(
            "flow: ignoring `{predicate}` literal `{}`: entries must be non-empty strings",
            excerpt(target)
        );
        return None;
    }
    Some(arr)
}

/// Reverse of `parse_flow_to_links` — reconstruct a [`SHACLFlow`] from
/// its RDF representation. Mirrors the canonical TS
/// `SHACLFlow.fromLinks` in `core/src/shacl/SHACLFlow.ts`, including
/// every declared predicate (`interpretationHint`, `requires`,
/// `semanticCheck`, `consensusRule` at both flow and state scope, plus
/// `inputTypes`, `outputTypes`, `creationHint`, `context` on the flow).
///
/// Malformed literals (bad JSON, wrong shape, non-string ModelQuery
/// `className`) leave the field unset rather than propagating the error.
/// Same policy as the TS reader and as [`load_flow_instances`] — a
/// stale / hand-mangled flow definition on-graph shouldn't poison every
/// Model C extraction pass on the perspective until it's manually cleaned.
pub fn parse_flow_from_links(links: &[Link], flow_uri: &str) -> Result<SHACLFlow, AnyError> {
    // Expected format: `{namespace}{Name}Flow` — same rule as the TS side.
    let without_suffix = flow_uri
        .strip_suffix("Flow")
        .ok_or_else(|| anyhow::anyhow!("Invalid flow URI: {flow_uri} (must end with 'Flow')"))?;
    let split_idx = without_suffix
        .rfind(|c: char| c == '/' || c == ':')
        .map(|i| i + 1)
        .unwrap_or(0);
    let (namespace, name) = without_suffix.split_at(split_idx);

    let mut flow = SHACLFlow {
        name: name.to_string(),
        namespace: namespace.to_string(),
        states: Vec::new(),
        transitions: Vec::new(),
        interpretation_hint: None,
        input_types: Vec::new(),
        output_types: Vec::new(),
        creation_hint: None,
        context: None,
        consensus_rule: None,
        consensus_rule_malformed: false,
    };

    // Flow-level `interpretationHint` — non-empty-string only.
    if let Some(link) = find_link(links, flow_uri, "ad4m://interpretationHint") {
        if let Some(hint) = decode_literal_string(&link.target) {
            if !hint.is_empty() {
                flow.interpretation_hint = Some(hint);
            }
        }
    }

    // Flow-level `inputTypes` / `outputTypes` — non-empty string arrays.
    if let Some(link) = find_link(links, flow_uri, "ad4m://inputTypes") {
        if let Some(arr) = decode_string_array(&link.target, "inputTypes") {
            flow.input_types = arr;
        }
    }
    if let Some(link) = find_link(links, flow_uri, "ad4m://outputTypes") {
        if let Some(arr) = decode_string_array(&link.target, "outputTypes") {
            flow.output_types = arr;
        }
    }

    // Flow-level `creationHint` — non-empty-string only.
    if let Some(link) = find_link(links, flow_uri, "ad4m://creationHint") {
        if let Some(hint) = decode_literal_string(&link.target) {
            if !hint.is_empty() {
                flow.creation_hint = Some(hint);
            }
        }
    }

    // Flow-level `context` — ModelQuery[] with the same className guard
    // as `requires`.
    if let Some(link) = find_link(links, flow_uri, "ad4m://context") {
        flow.context = decode_model_query_array(&link.target, "context");
    }

    // Flow-level `consensusRule`. A missing `n` or an invalid `fromRole`
    // still leaves `consensus_rule` unset — half-typed data must not reach
    // the consensus engine — but the failure is now RECORDED, because the
    // consumer's answer to "no rule" and to "unreadable rule" differ (#1078).
    // Two different rules on one source are unreadable for the same reason
    // and get the same answer — see `read_consensus_rule` (#1080).
    let (rule, unreadable) = read_consensus_rule(links, flow_uri, &format!("flow `{flow_uri}`"));
    flow.consensus_rule = rule;
    flow.consensus_rule_malformed = unreadable;

    // States — walk every `hasState` edge, gather each state's own
    // properties. Build a state-uri → state-name index so transition
    // parsing can resolve endpoints.
    let mut state_uri_to_name: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    // Distinct `hasState` targets, in discovery order. A repeated edge to the
    // SAME URI is one state, not two: every property below is re-read from
    // that one URI, so a second pass over it can only build a byte-identical
    // `FlowState`. This is #1080's equivalence — a link *is*
    // `(source, predicate, target)`, so once the loader has dropped
    // authorship, equal triples are indistinguishable AND identical in
    // effect, and a copy must not refuse. Collapsing them here is also what
    // keeps the name check below from firing on one: see
    // [`refuse_shadowed_state_names`].
    let mut state_uris: Vec<&str> = Vec::new();
    for state_link in find_links(links, flow_uri, "ad4m://hasState") {
        if !state_uris.contains(&state_link.target.as_str()) {
            state_uris.push(&state_link.target);
        }
    }

    for state_uri in &state_uris {
        let state_uri = *state_uri;
        let state_name = find_link(links, state_uri, "ad4m://stateName")
            .and_then(|l| decode_literal_string(&l.target))
            .unwrap_or_default();
        state_uri_to_name.insert(state_uri.to_string(), state_name.clone());

        let value = find_link(links, state_uri, "ad4m://stateValue")
            .and_then(|l| decode_literal_number(&l.target))
            .unwrap_or(0.0);

        let interpretation_hint = find_link(links, state_uri, "ad4m://interpretationHint")
            .and_then(|l| decode_literal_string(&l.target).filter(|s| !s.is_empty()));

        let requires = find_link(links, state_uri, "ad4m://requires")
            .and_then(|l| decode_model_query_array(&l.target, "requires"));

        let semantic_check = find_link(links, state_uri, "ad4m://semanticCheck")
            .and_then(|l| decode_literal_string(&l.target).filter(|s| !s.is_empty()));

        // Same policy as the flow-level rule above, through the same
        // reader: record the failure instead of erasing it, and treat a
        // contested scope as one more way to be unreadable.
        let (consensus_rule, consensus_rule_malformed) = read_consensus_rule(
            links,
            state_uri,
            &format!("flow `{flow_uri}` state `{state_name}`"),
        );

        flow.states.push(FlowState {
            name: state_name,
            value,
            interpretation_hint,
            requires,
            semantic_check,
            consensus_rule,
            consensus_rule_malformed,
        });
    }

    // #1082 — a name claimed by two states names neither of them. Runs
    // BEFORE the sort, while `flow.states[i]` still corresponds to
    // `state_uris[i]`, and before any consumer can resolve a name.
    refuse_shadowed_state_names(&mut flow.states, &state_uris, flow_uri);

    // Sort states by `value`, matching TS `SHACLFlow.fromLinks`
    // (`core/src/shacl/SHACLFlow.ts`) and for the same reason: link order is
    // not preserved on the graph, so the only stable ordering is the declared
    // `value`. Parity holds for *finite* values only: TS sorts with
    // `(a, b) => a.value - b.value`, whose NaN comparisons are engine-defined
    // rather than NaN-last, so the runtimes can diverge on exactly the
    // undecodable-value input the guard below handles. Mirroring the guard in
    // TS is a three-line comparator — until then, don't lean on `states[0]`
    // agreeing cross-runtime when a state value is NaN. The convention that rests on it — "the initial state is
    // `states[0]`", which `FlowInstance.start` consumes on the TS side — would
    // otherwise resolve differently in the two runtimes whenever
    // link-discovery order differs from value order, and a Rust-side spawn
    // would mint instances in the wrong starting state. Ties keep discovery
    // order (`sort_by` is stable), which is as arbitrary as the declaration
    // that produced them.
    //
    // `NaN` sorts last rather than comparing equal to everything. The value
    // arrives from `decode_literal_number`, which is `str::parse::<f64>` — so
    // a literal of `NaN` on the graph decodes to one, and
    // `partial_cmp(…).unwrap_or(Equal)` would then break the total order
    // `sort_by` requires, leaving the *finite* states in arbitrary relative
    // order too. Since `states[0]` is the initial state, that would pick the
    // wrong one. A state whose ordering value is undecodable has no claim to
    // being first.
    flow.states
        .sort_by(|a, b| match (a.value.is_nan(), b.value.is_nan()) {
            (false, false) => a.value.total_cmp(&b.value),
            (true, true) => std::cmp::Ordering::Equal,
            (true, false) => std::cmp::Ordering::Greater,
            (false, true) => std::cmp::Ordering::Less,
        });

    // Transitions — walk every `hasTransition` edge, resolve endpoints
    // via the state-name index.
    for transition_link in find_links(links, flow_uri, "ad4m://hasTransition") {
        let transition_uri = &transition_link.target;
        let action_name = find_link(links, transition_uri, "ad4m://actionName")
            .and_then(|l| decode_literal_string(&l.target))
            .unwrap_or_default();
        let from_state = find_link(links, transition_uri, "ad4m://fromState")
            .and_then(|l| state_uri_to_name.get(&l.target).cloned())
            .unwrap_or_default();
        let to_state = find_link(links, transition_uri, "ad4m://toState")
            .and_then(|l| state_uri_to_name.get(&l.target).cloned())
            .unwrap_or_default();
        let actions = find_link(links, transition_uri, "ad4m://transitionActions")
            .and_then(|l| {
                decode_json_literal_lossy::<Vec<AD4MAction>>(&l.target, "transitionActions")
            })
            .unwrap_or_default();
        flow.transitions.push(FlowTransition {
            action_name,
            from_state,
            to_state,
            actions,
        });
    }

    Ok(flow)
}

/// Synthetic keys `hydrate_one` (`model_query/hydration.rs`) always writes onto
/// every hydrated instance, regardless of the class's own properties. A
/// property sharing one of these names collides with it in the flat instance
/// JSON, and the direction of the collision is fixed per key, not uniform:
/// `id`/`baseExpression` are written FIRST in `hydrate_one`, so a matching
/// class property (written later in the same pass) silently overwrites them;
/// `createdAt`/`updatedAt`/`author`/`timestamp` are written LAST, so they
/// silently overwrite a matching class property instead. The second group is
/// also conditional (`if let Some(...)`) on there being a derivable value at
/// all, so on an instance whose links happen not to produce one, the class
/// property's own value survives instead — the same class reads differently
/// depending on which instance you ask. See issue #974: a `hasOne` relation
/// named `author` read back the creating agent's DID instead of the linked
/// instance.
///
/// `__links` is written only when a query asks for `links` (#1046 §3/§4), after
/// every property — the same silent-overwrite direction as the second group.
const RESERVED_PROPERTY_NAMES: [&str; 7] = [
    "id",
    "baseExpression",
    "createdAt",
    "updatedAt",
    "author",
    "timestamp",
    "__links",
];

/// Make the property-shape-level `local: true` authoritative by pushing it
/// down into every action that writes that property.
///
/// Write status is decided in `execute_commands` from the *action's* `local`
/// field alone — the executor synthesises no default actions, so a
/// property-shape `local: true` on its own had no effect whatsoever. A class
/// registered through `add_model` with `"local": true` on a property (and no
/// hand-copied `"local": true` inside each setter/adder/remover) produced
/// Shared links, silently. Decorator-generated SHACL only worked because
/// `shacl-gen.ts` copies the flag into the setter/adder/remover actions — and
/// even there it omits it from the *constructor* entries, so a `local`
/// property that is `required` or carries an `initial` value was written
/// Shared at creation and Local on every later setter write: one property,
/// two statuses, depending on when it was written.
///
/// Doing the propagation once here, at the single ingestion point for SHACL
/// JSON, means both mechanisms converge before anything is persisted:
/// `execute_commands` stays untouched, and the link-level SDNA that
/// `get_shape_actions_from_shacl` reads back already carries the flag.
///
/// An action that states `local` explicitly is left alone — an explicit
/// action-level flag stays the more specific declaration.
fn propagate_property_local(shape: &mut SHACLShape) {
    let local_predicates: std::collections::HashSet<String> = shape
        .properties
        .iter()
        .filter(|prop| prop.local == Some(true))
        .map(|prop| prop.path.clone())
        .collect();

    if local_predicates.is_empty() {
        return;
    }

    // Property-level actions: every action of a `local` property, regardless
    // of predicate. A collection setter also writes companion entries (e.g.
    // `ad4m://collection_order`); leaving those Shared would publish the
    // ordering of a collection whose members stay executor-private.
    for prop in shape.properties.iter_mut() {
        if prop.local != Some(true) {
            continue;
        }
        for action in prop
            .setter
            .iter_mut()
            .chain(prop.adder.iter_mut())
            .chain(prop.remover.iter_mut())
        {
            if action.local.is_none() {
                action.local = Some(true);
            }
        }
    }

    // Class-level actions are shared by all properties, so they are matched by
    // predicate: only the constructor/destructor entries that actually touch a
    // `local` property's predicate become local.
    for action in shape
        .constructor_actions
        .iter_mut()
        .chain(shape.destructor_actions.iter_mut())
    {
        if action.local.is_none() && local_predicates.contains(&action.predicate) {
            action.local = Some(true);
        }
    }
}

/// Parse SHACL JSON to RDF links (Option 3: Named Property Shapes)
pub fn parse_shacl_to_links(shacl_json: &str, class_name: &str) -> Result<Vec<Link>, AnyError> {
    let mut shape: SHACLShape = serde_json::from_str(shacl_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse SHACL JSON: {}", e))?;

    // Before a single link is generated: a property-shape `local: true` is a
    // declaration about the property, not about whichever actions happen to
    // have been authored with the flag copied in. Push it into the actions so
    // that the declaration is what the write path honours.
    propagate_property_local(&mut shape);

    // Reject reserved names before generating a single link: a class that
    // registers cleanly but silently shadows one of its own properties on
    // every read is a much worse failure than a rejection naming the fix.
    let reserved_collisions: Vec<String> = shape
        .properties
        .iter()
        .map(|prop| {
            prop.name
                .clone()
                .unwrap_or_else(|| extract_local_name(&prop.path))
        })
        .filter(|name| RESERVED_PROPERTY_NAMES.contains(&name.as_str()))
        .collect();
    if !reserved_collisions.is_empty() {
        return Err(anyhow::anyhow!(
            "Property name(s) {:?} collide with synthetic fields every hydrated instance \
             already carries ({}). `id`/`baseExpression` would be silently overwritten by \
             this property; `createdAt`/`updatedAt`/`author`/`timestamp` would silently \
             overwrite it instead, and only on instances where a value is derivable; \
             `__links` would overwrite it whenever a query asks for `links` — pick a \
             different property name.",
            reserved_collisions,
            RESERVED_PROPERTY_NAMES.join(", "),
        ));
    }

    let mut links = Vec::new();

    // Extract namespace from target_class (e.g., "recipe://Recipe" -> "recipe://")
    let namespace = extract_namespace(&shape.target_class)?;
    let shape_uri = format!("{}{}Shape", namespace, class_name);

    // Create name mapping for class lookup (needed by isSubjectInstance)
    let name_mapping = format!("literal:string:shacl://{}", class_name);

    links.push(Link {
        source: "ad4m://self".to_string(),
        predicate: Some("ad4m://has_shacl".to_string()),
        target: name_mapping.clone(),
    });

    links.push(Link {
        source: name_mapping,
        predicate: Some("ad4m://shacl_shape_uri".to_string()),
        target: shape_uri.clone(),
    });

    // Class definition links
    // Note: The ad4m://has_subject_class link is created by add_sdna(), not here,
    // to avoid duplication since add_sdna() always creates that link

    links.push(Link {
        source: shape.target_class.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://SubjectClass".to_string(),
    });

    links.push(Link {
        source: shape.target_class.clone(),
        predicate: Some("ad4m://shape".to_string()),
        target: shape_uri.clone(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "sh://NodeShape".to_string(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("sh://targetClass".to_string()),
        target: shape.target_class.clone(),
    });

    // Natural-language interpretation hint (steers LLM interpretation)
    if let Some(hint) = &shape.interpretation_hint {
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://interpretation_hint".to_string()),
            target: format!("literal:string:{}", hint),
        });
    }

    // Constructor actions (stored as JSON in literal)
    if !shape.constructor_actions.is_empty() {
        let constructor_json =
            serde_json::to_string(&shape.constructor_actions).unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://constructor".to_string()),
            target: format!("literal:string:{}", constructor_json),
        });
    }

    // Destructor actions (stored as JSON in literal)
    if !shape.destructor_actions.is_empty() {
        let destructor_json =
            serde_json::to_string(&shape.destructor_actions).unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://destructor".to_string()),
            target: format!("literal:string:{}", destructor_json),
        });
    }

    // Property shape links (Option 3: Named Property Shapes)
    for prop in shape.properties.iter() {
        // Use name field if provided, otherwise extract from path
        let prop_name = prop
            .name
            .as_ref()
            .map(|n| n.clone())
            .unwrap_or_else(|| extract_local_name(&prop.path));

        let prop_shape_uri = format!("{}{}.{}", namespace, class_name, prop_name);

        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("sh://property".to_string()),
            target: prop_shape_uri.clone(),
        });

        // Determine type based on collection flag
        let shape_type = if prop.collection.unwrap_or(false) {
            "ad4m://CollectionShape"
        } else {
            "sh://PropertyShape"
        };

        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: shape_type.to_string(),
        });

        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("sh://path".to_string()),
            target: prop.path.clone(),
        });

        // Natural-language property interpretation hint (semantic guidance for interpretation/tooling)
        if let Some(hint) = &prop.interpretation_hint {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://interpretation_hint".to_string()),
                target: format!("literal:string:{}", hint),
            });
        }

        // Dedup identity marker: the property the extractor treats as the
        // class's title-like interpretation key. No identity ⇒ no dedup.
        if prop.identity == Some(true) {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://identity".to_string()),
                target: "literal:string:true".to_string(),
            });
        }

        // CRDT ordering strategy for a collection relation. `load_shape` reads
        // the declaration back from this link and nowhere else, so both the
        // setter's entry writing and hydration's reconstruction depend on it
        // being emitted here.
        if let Some(ordering) = &prop.ordering {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://ordering".to_string()),
                target: format!("literal:string:{}", ordering),
            });
        }

        // Optional constraints
        if let Some(datatype) = &prop.datatype {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://datatype".to_string()),
                target: datatype.clone(),
            });
        }

        if let Some(min_count) = prop.min_count {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://minCount".to_string()),
                target: format!("literal:{}^^xsd:integer", min_count),
            });
        }

        if let Some(max_count) = prop.max_count {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://maxCount".to_string()),
                target: format!("literal:{}^^xsd:integer", max_count),
            });
        }

        if let Some(writable) = prop.writable {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://writable".to_string()),
                target: format!("literal:{}", writable),
            });
        }

        if let Some(resolve_lang) = &prop.resolve_language {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://resolveLanguage".to_string()),
                target: format!("literal:string:{}", resolve_lang),
            });
        }

        if let Some(node_kind) = &prop.node_kind {
            // Ensure node_kind is a valid URI - prefix bare names with sh://
            // e.g. "IRI" -> "sh://IRI", "Literal" -> "sh://Literal"
            let node_kind_uri = if node_kind.contains("://") {
                node_kind.clone()
            } else {
                format!("sh://{}", node_kind)
            };
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://nodeKind".to_string()),
                target: node_kind_uri,
            });
        }

        if let Some(local) = prop.local {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://local".to_string()),
                target: format!("literal:{}", local),
            });
        }

        // Property-level actions (setter, adder, remover)
        if !prop.setter.is_empty() {
            let setter_json =
                serde_json::to_string(&prop.setter).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://setter".to_string()),
                target: format!("literal:string:{}", setter_json),
            });
        }

        if !prop.adder.is_empty() {
            let adder_json =
                serde_json::to_string(&prop.adder).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://adder".to_string()),
                target: format!("literal:string:{}", adder_json),
            });
        }

        if !prop.remover.is_empty() {
            let remover_json =
                serde_json::to_string(&prop.remover).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://remover".to_string()),
                target: format!("literal:string:{}", remover_json),
            });
        }

        // sh:class — target SHACL node shape URI for typed relation resolution
        if let Some(class_uri) = &prop.class {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://class".to_string()),
                target: class_uri.clone(),
            });
        }

        // Pre-computed getter expression for conformance-filtered relation traversal
        if let Some(getter) = &prop.getter {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://getter".to_string()),
                target: format!("literal:string:{}", getter),
            });
        }

        // Structured conformance conditions for DB-agnostic type filtering
        if !prop.conformance_conditions.is_empty() {
            let conditions_json = serde_json::to_string(&prop.conformance_conditions)
                .unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://conformanceConditions".to_string()),
                target: format!("literal:string:{}", conditions_json),
            });
        }

        // Relation kind — drives direction and scalar-vs-collection rendering.
        if let Some(kind) = &prop.relation_kind {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://relationKind".to_string()),
                target: format!("literal:string:{}", kind),
            });
        }

        // Bare target class name for cache-based include resolution.
        if let Some(target_name) = &prop.target_class_name {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://targetClassName".to_string()),
                target: format!("literal:string:{}", target_name),
            });
        }

        // Post-getter where-clause filter for relations.
        if let Some(where_filter) = &prop.where_filter {
            let filter_json =
                serde_json::to_string(where_filter).unwrap_or_else(|_| "{}".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://whereFilter".to_string()),
                target: format!("literal:string:{}", filter_json),
            });
        }

        // Predicate IRI lookup for where_filter keys.
        if let Some(where_predicates) = &prop.where_predicates {
            if !where_predicates.is_empty() {
                let map_json =
                    serde_json::to_string(where_predicates).unwrap_or_else(|_| "{}".to_string());
                links.push(Link {
                    source: prop_shape_uri.clone(),
                    predicate: Some("ad4m://wherePredicates".to_string()),
                    target: format!("literal:string:{}", map_json),
                });
            }
        }

        // Conformance/type filtering enable flag (only emitted when false).
        if let Some(filter_enabled) = prop.filter {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://filter".to_string()),
                target: format!("literal:{}", filter_enabled),
            });
        }

        // sh:hasValue marks @Flag properties.  The target is stored as either
        // a URI (typical for ad4m://type-style flags) or as a literal value.
        if let Some(has_value) = &prop.has_value {
            let target = if has_value.contains("://") || has_value.starts_with("literal:") {
                has_value.clone()
            } else {
                format!("literal:string:{}", urlencoding::encode(has_value))
            };
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://hasValue".to_string()),
                target,
            });
        }

        if let Some(transform) = &prop.transform {
            let json_str = serde_json::to_string(transform).unwrap_or_default();
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://transform".to_string()),
                target: format!("literal:string:{}", json_str),
            });
        }
    }

    Ok(links)
}
/// Extract namespace from URI (e.g., "recipe://Recipe" -> "recipe://")
/// Matches TypeScript SHACLShape.ts extractNamespace() behavior
pub fn extract_namespace(uri: &str) -> Result<String, AnyError> {
    // Handle protocol-style URIs (://ending) - for AD4M-style URIs like "recipe://Recipe"
    // We want just the scheme + "://" part
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];

        // If nothing after scheme or only simple local name (no / or #), return just scheme://
        if !after_scheme.contains('/') && !after_scheme.contains('#') {
            return Ok(uri[..scheme_pos + 3].to_string());
        }
    }

    // Handle hash fragments (e.g., "http://example.com/ns#Recipe" -> "http://example.com/ns#")
    if let Some(hash_pos) = uri.rfind('#') {
        return Ok(uri[..hash_pos + 1].to_string());
    }

    // Handle slash-based paths (e.g., "http://example.com/ns/Recipe" -> "http://example.com/ns/")
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];
        if let Some(last_slash) = after_scheme.rfind('/') {
            return Ok(uri[..scheme_pos + 3 + last_slash + 1].to_string());
        }
    }

    // Error: malformed URI without proper namespace structure
    Err(anyhow::anyhow!(
        "Cannot extract namespace from malformed URI: '{}'",
        uri
    ))
}

/// Extract local name from URI (e.g., "recipe://name" -> "name")
fn extract_local_name(uri: &str) -> String {
    // Find the last occurrence of namespace delimiters: '#', ':', or '/'
    // This handles URIs like "http://example.com/ns#name" or "prefix:name"
    let last_hash = uri.rfind('#');
    let last_colon = uri.rfind(':');
    let last_slash = uri.rfind('/');

    // Find the rightmost delimiter position
    let delimiter_pos = [last_hash, last_colon, last_slash]
        .iter()
        .filter_map(|&pos| pos)
        .max();

    match delimiter_pos {
        Some(pos) => {
            let local_name = &uri[pos + 1..];
            if local_name.is_empty() {
                "unknown".to_string()
            } else {
                local_name.to_string()
            }
        }
        None => {
            // No delimiter found, return the whole URI if non-empty
            if uri.is_empty() {
                "unknown".to_string()
            } else {
                uri.to_string()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // The #1080 contested-rule tests assert the CONSUMER verdict, not just
    // the parser's representation of it: `Refused` is the behaviour,
    // `Malformed` is only how the parser spells it.
    use crate::perspectives::flow_instance::fold::{rule_for, ResolvedRule};

    #[test]
    fn test_extract_namespace() {
        // AD4M-style URIs (scheme://LocalName) -> just scheme://
        assert_eq!(extract_namespace("recipe://Recipe").unwrap(), "recipe://");
        assert_eq!(extract_namespace("simple://Test").unwrap(), "simple://");

        // W3C-style URIs with hash fragments -> include the hash
        assert_eq!(
            extract_namespace("http://example.com/ns#Recipe").unwrap(),
            "http://example.com/ns#"
        );

        // W3C-style URIs with slash paths -> include trailing slash
        assert_eq!(
            extract_namespace("http://example.com/ns/Recipe").unwrap(),
            "http://example.com/ns/"
        );
    }

    #[test]
    fn test_extract_local_name() {
        assert_eq!(extract_local_name("recipe://name"), "name");
        assert_eq!(
            extract_local_name("http://example.com/property"),
            "property"
        );
        assert_eq!(extract_local_name("simple://test/path/item"), "item");
    }

    /// #974: a property named `author` shadows the hydration metadata field of
    /// the same name — the linked-instance value and the creating agent's DID
    /// collide in the flat instance JSON, and whichever `hydrate_one` writes
    /// last wins silently. Reject it at registration instead.
    #[test]
    fn parse_shacl_to_links_rejects_a_property_named_author() {
        let shacl_json = r#"{
            "target_class": "book://Post",
            "properties": [
                { "path": "book://author", "name": "author", "relation_kind": "hasOne", "target_class_name": "User" }
            ]
        }"#;
        let err = parse_shacl_to_links(shacl_json, "Post").unwrap_err();
        let message = format!("{err}");
        assert!(message.contains("author"), "{message}");
        // Asserting on "overwrite" rather than a specific word like "hydration":
        // that's the actual claim this error makes and the one worth keeping
        // stable, not the exact phrasing around it.
        assert!(message.contains("overwrit"), "{message}");
    }

    /// SHACL with `local: true` declared ONLY on the property shape — no
    /// action carries the flag, which is exactly what an agent registering a
    /// class through `add_model` writes. Every action that touches the
    /// property must come out of the parser carrying `local`, because the
    /// action's flag is the only thing `execute_commands` reads.
    #[test]
    fn property_level_local_propagates_into_every_action() {
        let shacl_json = r#"{
            "target_class": "cache://Cache",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "cache://Cache"},
                {"action": "addLink", "source": "this", "predicate": "cache://state", "target": "literal:string:init"}
            ],
            "destructor_actions": [
                {"action": "removeLink", "source": "this", "predicate": "cache://state", "target": "*"},
                {"action": "removeLink", "source": "this", "predicate": "rdf://type", "target": "*"}
            ],
            "properties": [
                {
                    "path": "cache://state", "name": "state", "datatype": "xsd://string",
                    "min_count": 1, "max_count": 1, "writable": true, "local": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "cache://state", "target": "value"}]
                },
                {
                    "path": "cache://mark", "name": "marks", "collection": true,
                    "writable": true, "local": true,
                    "adder": [{"action": "addLink", "source": "this", "predicate": "cache://mark", "target": "value"}],
                    "remover": [{"action": "removeLink", "source": "this", "predicate": "cache://mark", "target": "value"}]
                },
                {
                    "path": "cache://title", "name": "title", "datatype": "xsd://string",
                    "max_count": 1, "writable": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "cache://title", "target": "value"}]
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Cache").expect("parse SHACL");

        // Helper: the JSON literal stored under `predicate` on the node whose
        // URI ends in `source_suffix` — the same lookup
        // `get_shape_actions_from_shacl` does at write time.
        let actions = |source_suffix: &str, predicate: &str| -> Vec<AD4MAction> {
            let link = links
                .iter()
                .find(|l| {
                    l.source.ends_with(source_suffix) && l.predicate.as_deref() == Some(predicate)
                })
                .unwrap_or_else(|| panic!("no {predicate} link on ...{source_suffix}"));
            let json = link
                .target
                .strip_prefix("literal:string:")
                .expect("action literal");
            serde_json::from_str(json).expect("action JSON")
        };

        let setter = actions("Cache.state", "ad4m://setter");
        assert_eq!(setter[0].local, Some(true), "setter of a local property");

        let adder = actions("Cache.marks", "ad4m://adder");
        assert_eq!(adder[0].local, Some(true), "adder of a local property");

        let remover = actions("Cache.marks", "ad4m://remover");
        assert_eq!(remover[0].local, Some(true), "remover of a local property");

        // Constructor/destructor are class-level: only the entries on a local
        // property's predicate flip, the rest stay shared.
        let constructor = actions("CacheShape", "ad4m://constructor");
        let state_entry = constructor
            .iter()
            .find(|a| a.predicate == "cache://state")
            .expect("constructor entry for cache://state");
        assert_eq!(
            state_entry.local,
            Some(true),
            "the initial value of a local property must be written local too, \
             otherwise creation writes Shared and every later setter writes Local"
        );
        let type_entry = constructor
            .iter()
            .find(|a| a.predicate == "rdf://type")
            .expect("constructor entry for rdf://type");
        assert_eq!(
            type_entry.local, None,
            "a predicate no local property declares must stay shared"
        );

        let destructor = actions("CacheShape", "ad4m://destructor");
        assert_eq!(
            destructor
                .iter()
                .find(|a| a.predicate == "cache://state")
                .expect("destructor entry for cache://state")
                .local,
            Some(true),
        );

        // The non-local property is untouched.
        let title_setter = actions("Cache.title", "ad4m://setter");
        assert_eq!(title_setter[0].local, None, "shared property stays shared");

        // And the declaration itself is still emitted as a link, which is what
        // `load_shape` / `describe_perspective` read back.
        assert!(links.iter().any(|l| l.source.ends_with("Cache.state")
            && l.predicate.as_deref() == Some("ad4m://local")
            && l.target == "literal:true"));
    }

    /// An action stating `local` explicitly is the more specific declaration
    /// and is left exactly as authored — including `local: false`, which is how
    /// a single action opts out of a property-level flag.
    #[test]
    fn explicit_action_level_local_survives_propagation() {
        let shacl_json = r#"{
            "target_class": "cache://Cache",
            "properties": [
                {
                    "path": "cache://state", "name": "state", "writable": true, "local": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "cache://state", "target": "value", "local": false}]
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Cache").expect("parse SHACL");
        let setter_json = links
            .iter()
            .find(|l| {
                l.source.ends_with("Cache.state") && l.predicate.as_deref() == Some("ad4m://setter")
            })
            .expect("setter link")
            .target
            .strip_prefix("literal:string:")
            .expect("action literal")
            .to_string();
        let setter: Vec<AD4MAction> = serde_json::from_str(&setter_json).expect("action JSON");
        assert_eq!(setter[0].local, Some(false));
    }

    /// A class with no local property must serialise byte-for-byte as before
    /// — the propagation is a no-op that returns early.
    #[test]
    fn shared_class_is_untouched_by_propagation() {
        let shacl_json = r#"{
            "target_class": "cache://Cache",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "cache://Cache"}
            ],
            "properties": [
                {
                    "path": "cache://title", "name": "title", "writable": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "cache://title", "target": "value"}]
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Cache").expect("parse SHACL");
        assert!(
            !links.iter().any(|l| l.target.contains("\"local\"")
                || l.predicate.as_deref() == Some("ad4m://local")),
            "no local flag anywhere in a class that never declared one"
        );
    }

    /// Every synthetic key `hydrate_one` writes, checked one at a time so a
    /// future addition to that list without a matching entry here fails loud
    /// rather than leaving a silent gap in the guard.
    #[test]
    fn parse_shacl_to_links_rejects_every_reserved_name() {
        for reserved in RESERVED_PROPERTY_NAMES {
            let shacl_json = format!(
                r#"{{
                    "target_class": "book://Post",
                    "properties": [
                        {{ "path": "book://{reserved}", "name": "{reserved}", "datatype": "xsd://string" }}
                    ]
                }}"#
            );
            assert!(
                parse_shacl_to_links(&shacl_json, "Post").is_err(),
                "'{reserved}' should be rejected"
            );
        }
    }

    /// A property whose `name` is omitted still derives from `path` before
    /// this check runs (matches the derivation immediately below), so a bare
    /// `path` ending in a reserved segment must be caught the same way an
    /// explicit `name` is.
    #[test]
    fn parse_shacl_to_links_rejects_a_reserved_name_derived_from_path() {
        let shacl_json = r#"{
            "target_class": "book://Post",
            "properties": [
                { "path": "book://timestamp", "datatype": "xsd://string" }
            ]
        }"#;
        let err = parse_shacl_to_links(shacl_json, "Post").unwrap_err();
        assert!(format!("{err}").contains("timestamp"));
    }

    /// A property with an ordinary name is unaffected — the guard must not
    /// reject legitimate schemas.
    #[test]
    fn parse_shacl_to_links_accepts_an_ordinary_property_name() {
        let shacl_json = r#"{
            "target_class": "book://Post",
            "properties": [
                { "path": "book://title", "name": "title", "datatype": "xsd://string" }
            ]
        }"#;
        assert!(parse_shacl_to_links(shacl_json, "Post").is_ok());
    }

    #[test]
    fn test_parse_shacl_basic() {
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "properties": [
                {
                    "path": "recipe://name",
                    "name": "name",
                    "datatype": "xsd://string",
                    "min_count": 1,
                    "max_count": 1,
                    "writable": true,
                    "resolve_language": "literal"
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Recipe").unwrap();

        // Should have: class definition (4) + property shape (7) = 11 links minimum
        // Note: ad4m://has_subject_class link is NOT created here - it's created by add_sdna()
        assert!(links.len() >= 11);

        // Check for key links (note: ad4m://self -> literal://string:Recipe is NOT here)
        assert!(links.iter().any(|l| l.source == "recipe://RecipeShape"
            && l.predicate == Some("sh://targetClass".to_string())));
        assert!(links
            .iter()
            .any(|l| l.source == "recipe://Recipe.name"
                && l.predicate == Some("sh://path".to_string())));
    }

    #[test]
    fn test_parse_shacl_with_interpretation_hint() {
        // Natural-language interpretation hints on the class and on a property should be
        // emitted as `ad4m://interpretation_hint` links so the generic extractor / MCP
        // tooling can inject them as semantic guidance for the LLM.
        let shacl_json = r#"{
            "target_class": "soa://Task",
            "interpretation_hint": "A concrete unit of work someone intends to do. Extract when there is an actionable outcome with a plausible owner; ignore vague aspirations.",
            "properties": [
                {
                    "path": "soa://title",
                    "name": "title",
                    "interpretation_hint": "Imperative one-line summary of the work, e.g. 'Extract LLM processing from Flux'.",
                    "datatype": "xsd://string",
                    "min_count": 1,
                    "max_count": 1
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Task").unwrap();

        // Class-level interpretation-hint link on the shape node.
        assert!(
            links.iter().any(|l| l.source == "soa://TaskShape"
                && l.predicate == Some("ad4m://interpretation_hint".to_string())
                && l.target
                    .starts_with("literal:string:A concrete unit of work")),
            "expected an ad4m://interpretation_hint link on the class shape"
        );

        // Property-level interpretation-hint link on the property shape node.
        assert!(
            links.iter().any(|l| l.source == "soa://Task.title"
                && l.predicate == Some("ad4m://interpretation_hint".to_string())
                && l.target
                    .starts_with("literal:string:Imperative one-line summary")),
            "expected an ad4m://interpretation_hint link on the property shape"
        );
    }

    #[test]
    fn test_parse_shacl_without_interpretation_hint_emits_none() {
        // Interpretation hints are optional; absence must not emit an interpretation_hint link.
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "properties": [
                { "path": "recipe://name", "name": "name", "datatype": "xsd://string" }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Recipe").unwrap();
        assert!(
            !links
                .iter()
                .any(|l| l.predicate == Some("ad4m://interpretation_hint".to_string())),
            "no ad4m://interpretation_hint link should be emitted when the hint is absent"
        );
    }

    #[test]
    fn test_parse_shacl_with_actions() {
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "recipe://name", "target": "literal:string:uninitialized"}
            ],
            "destructor_actions": [
                {"action": "removeLink", "source": "this", "predicate": "recipe://name", "target": "*"}
            ],
            "properties": [
                {
                    "path": "recipe://name",
                    "name": "name",
                    "datatype": "xsd://string",
                    "min_count": 1,
                    "max_count": 1,
                    "writable": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "recipe://name", "target": "value"}]
                },
                {
                    "path": "recipe://ingredient",
                    "name": "ingredients",
                    "node_kind": "IRI",
                    "adder": [{"action": "addLink", "source": "this", "predicate": "recipe://ingredient", "target": "value"}],
                    "remover": [{"action": "removeLink", "source": "this", "predicate": "recipe://ingredient", "target": "value"}]
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Recipe").unwrap();

        // Check for constructor action link
        assert!(
            links.iter().any(|l| l.source == "recipe://RecipeShape"
                && l.predicate == Some("ad4m://constructor".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing constructor action link"
        );

        // Check for destructor action link
        assert!(
            links.iter().any(|l| l.source == "recipe://RecipeShape"
                && l.predicate == Some("ad4m://destructor".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing destructor action link"
        );

        // Check for property setter action link
        assert!(
            links.iter().any(|l| l.source == "recipe://Recipe.name"
                && l.predicate == Some("ad4m://setter".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing setter action link"
        );

        // Check for collection adder action link
        assert!(
            links
                .iter()
                .any(|l| l.source == "recipe://Recipe.ingredients"
                    && l.predicate == Some("ad4m://adder".to_string())
                    && l.target.starts_with("literal://string:")
                    || l.target.starts_with("literal:string:")),
            "Missing adder action link"
        );

        // Check for collection remover action link
        assert!(
            links
                .iter()
                .any(|l| l.source == "recipe://Recipe.ingredients"
                    && l.predicate == Some("ad4m://remover".to_string())
                    && l.target.starts_with("literal://string:")
                    || l.target.starts_with("literal:string:")),
            "Missing remover action link"
        );
    }

    #[test]
    fn test_parse_flow_basic() {
        let flow_json = r#"{
            "name": "TODO",
            "namespace": "todo://",
            "states": [
                { "name": "ready", "value": 0.0 },
                { "name": "done",  "value": 1.0 }
            ],
            "transitions": [
                {
                    "action_name": "Complete",
                    "from_state": "ready",
                    "to_state": "done",
                    "actions": [
                        {"action": "addLink", "source": "this", "predicate": "todo://state", "target": "todo://done"},
                        {"action": "removeLink", "source": "this", "predicate": "todo://state", "target": "todo://ready"}
                    ]
                }
            ]
        }"#;

        let links = parse_flow_to_links(flow_json, "TODO").unwrap();

        // Check for flow type link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("rdf://type".to_string())
                && l.target == "ad4m://Flow"),
            "Missing flow type link"
        );

        // Check for state links
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasState".to_string())
                && l.target == "todo://TODO.ready"),
            "Missing ready state link"
        );

        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasState".to_string())
                && l.target == "todo://TODO.done"),
            "Missing done state link"
        );

        // Check for transition link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasTransition".to_string())
                && l.target == "todo://TODO.readyTodone"),
            "Missing transition link"
        );

        // Check for transition action name
        assert!(
            links.iter().any(|l| l.source == "todo://TODO.readyTodone"
                && l.predicate == Some("ad4m://actionName".to_string())),
            "Missing action name link"
        );
    }

    /// The full field set the TS side ships (`interpretationHint`,
    /// `requires`, `semanticCheck`, `consensusRule`, `inputTypes`,
    /// `outputTypes`, `creationHint`, `context`) deserialises cleanly
    /// from the same JSON shape `toJSON()` on
    /// `core/src/shacl/SHACLFlow.ts` emits. This is the wire-format
    /// contract between the TS designer surface and the Rust flow
    /// engine — a drift here breaks Model C's context gathering
    /// silently.
    #[test]
    fn test_parse_flow_full_field_set_deserialises() {
        let flow_json = r#"{
            "name": "Deliberation",
            "namespace": "ns://deliberation/",
            "interpretationHint": "Tracks a group deliberation from proposal to shared understanding.",
            "inputTypes": ["ns://Proposal"],
            "outputTypes": ["ns://Resolution"],
            "creationHint": "Someone raised a proposal that needs group deliberation before a decision.",
            "consensusRule": { "n": 2 },
            "context": [
                { "className": "ns://Perspective", "where": { "about": "$flow.base" } }
            ],
            "start_action": [],
            "states": [
                {
                    "name": "Tension",
                    "value": 1.0,
                    "state_check": { "predicate": "ns://state", "target": "ns://tension" },
                    "interpretationHint": "Participants have voiced opposing views on the proposal.",
                    "semanticCheck": "Confirm the objection is genuine disagreement, not a clarifying question.",
                    "consensusRule": { "n": 2, "fromRole": { "className": "ns://Reviewer" } },
                    "requires": [
                        {
                            "className": "ns://Objection",
                            "where": { "about": "$flow.base" },
                            "count": { "min": 1 },
                            "linkedTo": "base"
                        },
                        {
                            "className": "ns://Perspective",
                            "where": {
                                "about": "$flow.base",
                                "stance": { "in": ["for", "against"] }
                            },
                            "count": { "min": 2 }
                        }
                    ]
                }
            ],
            "transitions": []
        }"#;

        let flow: SHACLFlow =
            serde_json::from_str(flow_json).expect("full field set deserialises cleanly");

        assert_eq!(
            flow.interpretation_hint.as_deref(),
            Some("Tracks a group deliberation from proposal to shared understanding.")
        );
        assert_eq!(flow.input_types, vec!["ns://Proposal".to_string()]);
        assert_eq!(flow.output_types, vec!["ns://Resolution".to_string()]);
        assert!(flow.creation_hint.is_some());
        assert_eq!(flow.consensus_rule.as_ref().map(|c| c.n), Some(2));
        assert!(flow.context.is_some());

        let tension = &flow.states[0];
        assert!(tension.interpretation_hint.is_some());
        assert!(tension.semantic_check.is_some());
        assert_eq!(tension.consensus_rule.as_ref().map(|c| c.n), Some(2));
        assert!(tension.consensus_rule.as_ref().unwrap().from_role.is_some());

        let requires = tension.requires.as_ref().expect("requires present");
        assert_eq!(requires.len(), 2);
        assert_eq!(requires[0].class_name, "ns://Objection");
        assert_eq!(requires[0].count.as_ref().and_then(|c| c.min), Some(1));
        assert_eq!(requires[1].class_name, "ns://Perspective");
        let where_ = requires[1].r#where.as_ref().expect("where clause present");
        assert!(where_.contains_key("about"));
        assert!(where_.contains_key("stance"));
        // The `stance` condition is the `In` object variant — untagged
        // enum dispatch on shape.
        match where_.get("stance").unwrap() {
            PropertyCondition::In { one_of } => {
                assert_eq!(one_of.len(), 2);
            }
            other => panic!("expected In variant, got {other:?}"),
        }
    }

    /// A flow JSON that omits every optional field still parses —
    /// `#[serde(default)]` on each optional field is what keeps this
    /// green. If this test breaks, an optional field lost its default
    /// annotation and any legacy JSON (or a hand-authored minimal flow)
    /// now fails to deserialise.
    #[test]
    fn test_parse_flow_omitting_optional_fields_still_works() {
        let flow_json = r#"{
            "name": "TODO",
            "namespace": "todo://",
            "start_action": [],
            "states": [
                {
                    "name": "ready",
                    "value": 0.0,
                    "state_check": { "predicate": "todo://state", "target": "todo://ready" }
                }
            ],
            "transitions": []
        }"#;

        let flow: SHACLFlow = serde_json::from_str(flow_json)
            .expect("minimal flow (no optional fields) parses cleanly");
        assert_eq!(flow.name, "TODO");
        assert!(flow.interpretation_hint.is_none());
        assert!(flow.input_types.is_empty());
        assert!(flow.output_types.is_empty());
        assert!(flow.creation_hint.is_none());
        assert!(flow.consensus_rule.is_none());
        assert!(flow.context.is_none());
        assert!(flow.states[0].interpretation_hint.is_none());
        assert!(flow.states[0].requires.is_none());
        assert!(flow.states[0].semantic_check.is_none());
        assert!(flow.states[0].consensus_rule.is_none());
    }

    /// `PropertyCondition` scalar shorthand → serde untagged should
    /// pick the right variant for each JSON leaf shape.
    #[test]
    fn test_property_condition_scalar_shorthand_deserialisation() {
        let str: PropertyCondition = serde_json::from_str(r#""hello""#).unwrap();
        assert!(matches!(str, PropertyCondition::Str(_)));

        let num: PropertyCondition = serde_json::from_str(r#"42"#).unwrap();
        assert!(matches!(num, PropertyCondition::Num(_)));

        let boolv: PropertyCondition = serde_json::from_str(r#"true"#).unwrap();
        assert!(matches!(boolv, PropertyCondition::Bool(_)));

        let equals: PropertyCondition = serde_json::from_str(r#"{"equals":"x"}"#).unwrap();
        assert!(matches!(equals, PropertyCondition::Equals { .. }));

        let in_: PropertyCondition = serde_json::from_str(r#"{"in":["a","b"]}"#).unwrap();
        assert!(matches!(in_, PropertyCondition::In { .. }));

        let exists: PropertyCondition = serde_json::from_str(r#"{"exists":true}"#).unwrap();
        assert!(matches!(exists, PropertyCondition::Exists { .. }));

        let matches_: PropertyCondition = serde_json::from_str(r#"{"matches":"^foo"}"#).unwrap();
        assert!(matches!(matches_, PropertyCondition::Matches { .. }));
    }

    /// `ModelQuery.or` composes recursively — the composed guard shape
    /// used by role expressions in §7.3. Round-trip must preserve the
    /// nested structure so the engine can walk it during evaluation.
    #[test]
    fn test_model_query_or_composition() {
        let json = r#"{
            "className": "ns://Role",
            "or": [
                { "className": "ns://Editor", "didProperty": "member" },
                { "className": "ns://Owner",  "didProperty": "owner" }
            ]
        }"#;
        let q: ModelQuery = serde_json::from_str(json).unwrap();
        assert_eq!(q.class_name, "ns://Role");
        let or = q.or.expect("or clause present");
        assert_eq!(or.len(), 2);
        assert_eq!(or[0].did_property.as_deref(), Some("member"));
        assert_eq!(or[1].did_property.as_deref(), Some("owner"));
    }

    // ---------------------------------------------------------------------
    // parse_flow_from_links — reverse-of-parse_flow_to_links round-trips +
    // full-shape read against hand-built links matching the canonical TS
    // toLinks writer output.
    // ---------------------------------------------------------------------

    fn lit_str(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    fn lit_num(n: f64) -> String {
        format!("literal:number:{}", n)
    }

    fn lit_json<T: serde::Serialize>(v: &T) -> String {
        let json = serde_json::to_string(v).expect("serializable");
        format!("literal:string:{}", urlencoding::encode(&json))
    }

    fn mk_link(source: &str, predicate: &str, target: &str) -> Link {
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
    }

    /// Round-trip on a MINIMAL flow (no optional fields set). Locks the
    /// pair of edges the reader must handle when a producer only sets
    /// `states` + `transitions` + `start_action` — every optional
    /// predicate absent, all optional fields read back as `None` /
    /// empty. Guards against a reader change that starts synthesising
    /// values when a predicate is missing.
    #[test]
    fn parse_flow_from_links_roundtrips_minimal_flow() {
        let flow_json = r#"{
            "name": "TODO",
            "namespace": "todo://",
            "start_action": [
                {"action": "addLink", "source": "this", "predicate": "todo://state", "target": "todo://ready"}
            ],
            "states": [
                {"name": "ready", "value": 0.0, "state_check": {"predicate": "todo://state", "target": "todo://ready"}},
                {"name": "done",  "value": 1.0, "state_check": {"predicate": "todo://state", "target": "todo://done"}}
            ],
            "transitions": [
                {
                    "action_name": "Complete",
                    "from_state": "ready",
                    "to_state": "done",
                    "actions": [
                        {"action": "addLink", "source": "this", "predicate": "todo://state", "target": "todo://done"}
                    ]
                }
            ]
        }"#;

        let links = parse_flow_to_links(flow_json, "TODO").expect("writer");
        let flow = parse_flow_from_links(&links, "todo://TODOFlow").expect("reader");

        assert_eq!(flow.name, "TODO");
        assert_eq!(flow.namespace, "todo://");
        assert_eq!(flow.states.len(), 2);
        assert_eq!(flow.states[0].name, "ready");
        assert!((flow.states[0].value - 0.0).abs() < f64::EPSILON);
        assert_eq!(flow.states[1].name, "done");
        assert!((flow.states[1].value - 1.0).abs() < f64::EPSILON);
        assert_eq!(flow.transitions.len(), 1);
        assert_eq!(flow.transitions[0].action_name, "Complete");
        assert_eq!(flow.transitions[0].from_state, "ready");
        assert_eq!(flow.transitions[0].to_state, "done");
        assert_eq!(flow.transitions[0].actions.len(), 1);

        // Optional fields absent in the input JSON → the writer emits
        // no predicate for them → the reader leaves them unset / empty.
        assert!(flow.interpretation_hint.is_none());
        assert!(flow.creation_hint.is_none());
        assert!(flow.consensus_rule.is_none());
        assert!(flow.context.is_none());
        assert!(flow.input_types.is_empty());
        assert!(flow.output_types.is_empty());
        for state in &flow.states {
            assert!(state.interpretation_hint.is_none());
            assert!(state.requires.is_none());
            assert!(state.semantic_check.is_none());
            assert!(state.consensus_rule.is_none());
        }
    }

    /// Full-shape read — hand-built links matching what
    /// `core/src/shacl/SHACLFlow.ts::toLinks()` emits when every field
    /// is set. Independent of the Rust writer (so a Rust-writer bug
    /// can't mask a reader bug) — this is what catches drift between
    /// the TS writer and the Rust reader, the two halves Model C hangs
    /// on.
    #[test]
    fn parse_flow_from_links_reads_all_predicates_from_hand_built_links() {
        let flow_uri = "coasys://DeliberationFlow";
        let state_uri = "coasys://Deliberation.Resolution";
        let transition_uri = "coasys://Deliberation.OverlapToResolution";
        let overlap_uri = "coasys://Deliberation.Overlap";
        let requires_json = r#"[{"className": "coasys://Perspective", "count": {"min": 3}}]"#;
        let context_json = r#"[{"className": "coasys://Proposal"}]"#;
        let consensus_json =
            r#"{"n": 2, "fromRole": {"className": "coasys://Role", "didProperty": "member"}}"#;

        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Deliberation")),
            mk_link(
                flow_uri,
                "ad4m://interpretationHint",
                &lit_str("guide toward overlap"),
            ),
            mk_link(
                flow_uri,
                "ad4m://inputTypes",
                &lit_json(&vec!["coasys://Proposal".to_string()]),
            ),
            mk_link(
                flow_uri,
                "ad4m://outputTypes",
                &lit_json(&vec!["coasys://Resolution".to_string()]),
            ),
            mk_link(
                flow_uri,
                "ad4m://creationHint",
                &lit_str("controversial claim surfaces"),
            ),
            mk_link(
                flow_uri,
                "ad4m://context",
                &format!("literal:string:{}", urlencoding::encode(context_json)),
            ),
            mk_link(
                flow_uri,
                "ad4m://consensusRule",
                &format!("literal:string:{}", urlencoding::encode(consensus_json)),
            ),
            mk_link(flow_uri, "ad4m://hasState", overlap_uri),
            mk_link(overlap_uri, "rdf://type", "ad4m://FlowState"),
            mk_link(overlap_uri, "ad4m://stateName", &lit_str("Overlap")),
            mk_link(overlap_uri, "ad4m://stateValue", &lit_num(0.5)),
            mk_link(
                overlap_uri,
                "ad4m://stateCheck",
                &lit_json(&LinkPattern {
                    source: None,
                    predicate: "coasys://state".to_string(),
                    target: "coasys://overlap".to_string(),
                }),
            ),
            mk_link(flow_uri, "ad4m://hasState", state_uri),
            mk_link(state_uri, "rdf://type", "ad4m://FlowState"),
            mk_link(state_uri, "ad4m://stateName", &lit_str("Resolution")),
            mk_link(state_uri, "ad4m://stateValue", &lit_num(1.0)),
            mk_link(
                state_uri,
                "ad4m://stateCheck",
                &lit_json(&LinkPattern {
                    source: None,
                    predicate: "coasys://state".to_string(),
                    target: "coasys://resolved".to_string(),
                }),
            ),
            mk_link(
                state_uri,
                "ad4m://interpretationHint",
                &lit_str("participants agree"),
            ),
            mk_link(
                state_uri,
                "ad4m://requires",
                &format!("literal:string:{}", urlencoding::encode(requires_json)),
            ),
            mk_link(
                state_uri,
                "ad4m://semanticCheck",
                &lit_str("evidence of convergence"),
            ),
            mk_link(
                state_uri,
                "ad4m://consensusRule",
                &lit_json(&ConsensusRule {
                    n: 3,
                    from_role: None,
                }),
            ),
            mk_link(flow_uri, "ad4m://hasTransition", transition_uri),
            mk_link(transition_uri, "rdf://type", "ad4m://FlowTransition"),
            mk_link(transition_uri, "ad4m://actionName", &lit_str("Resolve")),
            mk_link(transition_uri, "ad4m://fromState", overlap_uri),
            mk_link(transition_uri, "ad4m://toState", state_uri),
        ];

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        assert_eq!(flow.name, "Deliberation");
        assert_eq!(flow.namespace, "coasys://");
        assert_eq!(
            flow.interpretation_hint.as_deref(),
            Some("guide toward overlap")
        );
        assert_eq!(flow.input_types, vec!["coasys://Proposal".to_string()]);
        assert_eq!(flow.output_types, vec!["coasys://Resolution".to_string()]);
        assert_eq!(
            flow.creation_hint.as_deref(),
            Some("controversial claim surfaces")
        );
        let ctx = flow.context.as_ref().expect("context present");
        assert_eq!(ctx.len(), 1);
        assert_eq!(ctx[0].class_name, "coasys://Proposal");
        let rule = flow
            .consensus_rule
            .as_ref()
            .expect("flow consensusRule present");
        assert_eq!(rule.n, 2);
        assert!(rule.from_role.is_some());
        assert_eq!(flow.states.len(), 2);
        let resolution = flow
            .states
            .iter()
            .find(|s| s.name == "Resolution")
            .expect("Resolution state");
        assert_eq!(
            resolution.interpretation_hint.as_deref(),
            Some("participants agree")
        );
        let req = resolution.requires.as_ref().expect("requires present");
        assert_eq!(req.len(), 1);
        assert_eq!(req[0].class_name, "coasys://Perspective");
        assert_eq!(
            resolution.semantic_check.as_deref(),
            Some("evidence of convergence")
        );
        let state_rule = resolution
            .consensus_rule
            .as_ref()
            .expect("state consensusRule present");
        assert_eq!(state_rule.n, 3);
        assert_eq!(flow.transitions.len(), 1);
        let t = &flow.transitions[0];
        assert_eq!(t.action_name, "Resolve");
        assert_eq!(t.from_state, "Overlap");
        assert_eq!(t.to_state, "Resolution");
    }

    /// Round-trip on a FULL flow (every optional field set). Closes
    /// the writer→reader loop the previous "hand-built links" test
    /// only proved one side of. If a new field lands on `SHACLFlow`
    /// and the writer forgets to emit it (or emits it under the wrong
    /// predicate URI), this test fails. Guard against the mismatch
    /// PR #929 review R7 caught the previous time round.
    #[test]
    fn parse_flow_from_links_roundtrips_full_flow() {
        let flow_json = r#"{
            "name": "Deliberation",
            "namespace": "coasys://",
            "interpretationHint": "Guide the group toward overlap.",
            "inputTypes": ["coasys://Proposal"],
            "outputTypes": ["coasys://Resolution"],
            "creationHint": "Fires when a controversial claim surfaces.",
            "context": [{"className": "coasys://Proposal"}],
            "consensusRule": {"n": 2},
            "start_action": [],
            "states": [
                {
                    "name": "Overlap",
                    "value": 0.5,
                    "state_check": {"predicate": "coasys://state", "target": "coasys://overlap"}
                },
                {
                    "name": "Resolution",
                    "value": 1.0,
                    "state_check": {"predicate": "coasys://state", "target": "coasys://resolved"},
                    "interpretationHint": "Participants agree.",
                    "semanticCheck": "Evidence of convergence?",
                    "consensusRule": {"n": 3, "fromRole": {"className": "coasys://Reviewer"}},
                    "requires": [
                        {"className": "coasys://Perspective", "count": {"min": 3}}
                    ]
                }
            ],
            "transitions": [
                {
                    "action_name": "Resolve",
                    "from_state": "Overlap",
                    "to_state": "Resolution",
                    "actions": []
                }
            ]
        }"#;

        let links = parse_flow_to_links(flow_json, "Deliberation").expect("writer");
        let flow = parse_flow_from_links(&links, "coasys://DeliberationFlow").expect("reader");

        // Flow-scope round-trip
        assert_eq!(
            flow.interpretation_hint.as_deref(),
            Some("Guide the group toward overlap.")
        );
        assert_eq!(flow.input_types, vec!["coasys://Proposal".to_string()]);
        assert_eq!(flow.output_types, vec!["coasys://Resolution".to_string()]);
        assert_eq!(
            flow.creation_hint.as_deref(),
            Some("Fires when a controversial claim surfaces.")
        );
        let ctx = flow.context.as_ref().expect("context round-trips");
        assert_eq!(ctx.len(), 1);
        assert_eq!(ctx[0].class_name, "coasys://Proposal");
        let rule = flow
            .consensus_rule
            .as_ref()
            .expect("flow consensusRule round-trips");
        assert_eq!(rule.n, 2);

        // State-scope round-trip on the Resolution state (Overlap
        // deliberately keeps everything optional unset to prove the
        // writer doesn't emit empty predicates when the source is None
        // — a round-trip smell in that direction would break the
        // "no-op is a no-op" invariant the empty-string guards protect).
        let resolution = flow
            .states
            .iter()
            .find(|s| s.name == "Resolution")
            .expect("Resolution state");
        assert_eq!(
            resolution.interpretation_hint.as_deref(),
            Some("Participants agree.")
        );
        assert_eq!(
            resolution.semantic_check.as_deref(),
            Some("Evidence of convergence?")
        );
        let state_rule = resolution
            .consensus_rule
            .as_ref()
            .expect("state consensusRule round-trips");
        assert_eq!(state_rule.n, 3);
        assert!(state_rule.from_role.is_some());
        let req = resolution.requires.as_ref().expect("requires round-trips");
        assert_eq!(req.len(), 1);
        assert_eq!(req[0].class_name, "coasys://Perspective");
        assert_eq!(req[0].count.as_ref().and_then(|c| c.min), Some(3));

        // Overlap has no optional fields set — writer must not have
        // conjured any state-scope predicates for it.
        let overlap = flow
            .states
            .iter()
            .find(|s| s.name == "Overlap")
            .expect("Overlap state");
        assert!(overlap.interpretation_hint.is_none());
        assert!(overlap.semantic_check.is_none());
        assert!(overlap.consensus_rule.is_none());
        assert!(overlap.requires.is_none());
    }

    /// Bad-shape rejection: a `requires` payload whose entries fail
    /// the `className` guard must be dropped (same policy as the TS
    /// `isModelQueryShape` reader). Leaving corrupt ModelQueries in
    /// the returned SHACLFlow would break the consensus engine's
    /// evidence-lookup with a cryptic empty-classname error at eval
    /// time — better to swallow at read time.
    #[test]
    fn parse_flow_from_links_rejects_malformed_requires() {
        let flow_uri = "test://BadFlow";
        let state_uri = "test://Bad.S1";
        let bad_requires = r#"[{"count": {"min": 1}}]"#; // missing className
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Bad")),
            mk_link(flow_uri, "ad4m://hasState", state_uri),
            mk_link(state_uri, "rdf://type", "ad4m://FlowState"),
            mk_link(state_uri, "ad4m://stateName", &lit_str("S1")),
            mk_link(state_uri, "ad4m://stateValue", &lit_num(0.0)),
            mk_link(
                state_uri,
                "ad4m://stateCheck",
                &lit_json(&LinkPattern {
                    source: None,
                    predicate: "".to_string(),
                    target: "".to_string(),
                }),
            ),
            mk_link(
                state_uri,
                "ad4m://requires",
                &format!("literal:string:{}", urlencoding::encode(bad_requires)),
            ),
        ];
        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        assert_eq!(flow.states.len(), 1);
        // Malformed requires → left unset rather than propagated as a
        // ModelQuery with empty className.
        assert!(flow.states[0].requires.is_none());
    }

    /// A `Reviewer`-gated 5-of-N rule with ONE wrongly-cased field inside
    /// the nested `fromRole` — `class_name` for `className`, the realistic
    /// authoring slip of #1078. `className` has no serde default, so the
    /// failure propagates out of `ModelQuery` and takes the whole
    /// `ConsensusRule` with it.
    ///
    /// The first assertion is the premise, not decoration: without it a
    /// green test could mean the literal parsed fine and there was never a
    /// malformed case to record.
    ///
    /// Killing mutation: `consensus_rule_malformed = true` → `= false` in
    /// the state-scope arm of `parse_flow_from_links`. That restores the
    /// exact pre-fix representation — rule unset, nothing recorded — and
    /// `rule_for` answers it with `{n: 1}` again.
    #[test]
    fn a_state_consensus_rule_that_does_not_parse_is_recorded_as_malformed() {
        let bad_rule = r#"{"n":5,"fromRole":{"class_name":"Reviewer","didProperty":"$did"}}"#;
        assert!(
            serde_json::from_str::<ConsensusRule>(bad_rule).is_err(),
            "premise: this literal must actually fail to deserialise"
        );

        let flow_uri = "test://GatedFlow";
        let state_uri = "test://Gated.approved";
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Gated")),
            mk_link(flow_uri, "ad4m://hasState", state_uri),
            mk_link(state_uri, "rdf://type", "ad4m://FlowState"),
            mk_link(state_uri, "ad4m://stateName", &lit_str("approved")),
            mk_link(state_uri, "ad4m://stateValue", &lit_num(1.0)),
            mk_link(
                state_uri,
                "ad4m://consensusRule",
                &format!("literal:string:{}", urlencoding::encode(bad_rule)),
            ),
        ];

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        let state = &flow.states[0];
        assert!(
            state.consensus_rule.is_none(),
            "half-typed rules must not reach the engine"
        );
        assert!(
            matches!(state.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
            "the state must report Malformed, not Absent — those get opposite \
             answers from rule_for"
        );
    }

    /// Same slip at flow scope. Separate test because the two scopes are
    /// parsed by separate blocks, and the state-scope one being right says
    /// nothing about this one.
    ///
    /// Killing mutation: drop `flow.consensus_rule_malformed = true` from
    /// the flow-scope `Err` arm.
    #[test]
    fn a_flow_consensus_rule_that_does_not_parse_is_recorded_as_malformed() {
        let bad_rule = r#"{"threshold":5}"#; // `n` is required and missing
        assert!(
            serde_json::from_str::<ConsensusRule>(bad_rule).is_err(),
            "premise: this literal must actually fail to deserialise"
        );

        let flow_uri = "test://LooseFlow";
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Loose")),
            mk_link(
                flow_uri,
                "ad4m://consensusRule",
                &format!("literal:string:{}", urlencoding::encode(bad_rule)),
            ),
        ];

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        assert!(flow.consensus_rule.is_none());
        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
            "flow scope must report Malformed"
        );
    }

    /// The positive control for both tests above: a flow that declares no
    /// `consensusRule` at all reads as `Absent`, NOT as `Malformed`.
    ///
    /// Without this, a fix that marked every flow malformed — refusing
    /// every transition in the system — would look identical to a working
    /// one.
    ///
    /// Killing mutation: initialise `consensus_rule_malformed: true` in the
    /// `SHACLFlow` / `FlowState` constructors in `parse_flow_from_links`.
    #[test]
    fn an_absent_consensus_rule_is_absent_not_malformed() {
        let flow_uri = "test://PlainFlow";
        let state_uri = "test://Plain.done";
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Plain")),
            mk_link(flow_uri, "ad4m://hasState", state_uri),
            mk_link(state_uri, "rdf://type", "ad4m://FlowState"),
            mk_link(state_uri, "ad4m://stateName", &lit_str("done")),
            mk_link(state_uri, "ad4m://stateValue", &lit_num(1.0)),
        ];

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Absent),
            "no link at flow scope ⇒ Absent"
        );
        assert!(
            matches!(
                flow.states[0].consensus_rule_slot(),
                ConsensusRuleSlot::Absent
            ),
            "no link at state scope ⇒ Absent"
        );
    }

    /// A well-formed rule still decodes, with `fromRole` intact. Guards the
    /// `Result` conversion in `decode_json_literal`: an `Err`-always
    /// implementation would pass every test above.
    ///
    /// Killing mutation: make `decode_json_literal` return
    /// `Err("…".into())` unconditionally.
    #[test]
    fn a_well_formed_consensus_rule_still_decodes() {
        let good_rule = r#"{"n":5,"fromRole":{"className":"Reviewer","didProperty":"$did"}}"#;
        let flow_uri = "test://GoodFlow";
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Good")),
            mk_link(
                flow_uri,
                "ad4m://consensusRule",
                &format!("literal:string:{}", urlencoding::encode(good_rule)),
            ),
        ];

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        let ConsensusRuleSlot::Rule(rule) = flow.consensus_rule_slot() else {
            panic!(
                "a valid rule must read as Rule, got {:?}",
                flow.consensus_rule_slot()
            );
        };
        assert_eq!(rule.n, 5);
        assert_eq!(
            rule.from_role.as_ref().map(|r| r.class_name.as_str()),
            Some("Reviewer"),
            "the role gate must survive the round trip"
        );
        assert!(!flow.consensus_rule_malformed);
    }

    // -------------------------------------------------------------------
    // #1080 variant 1b — a contested `consensusRule` scope.
    //
    // Every fixture below shares one shape: a flow gated at `5 of N, only
    // from Reviewer`, plus an injected `{"n":1}` — the downgrade an
    // arbitrary neighbourhood peer can write. The injected literal is
    // well-formed on purpose, so #1079's malformed path cannot be what
    // refuses it.
    // -------------------------------------------------------------------

    const CONTESTED_URI: &str = "test://ContestedFlow";
    /// A 5-of-N `Reviewer` gate, as the author wrote it.
    const GENUINE_RULE: &str =
        r#"{"n":5,"fromRole":{"className":"Reviewer","didProperty":"$did"}}"#;
    /// What a peer injects to reach one-vote-from-anybody.
    const INJECTED_RULE: &str = r#"{"n":1}"#;

    /// Flow-scope links for `CONTESTED_URI` with the `consensusRule`
    /// literals appended in the given order. Order is a parameter because
    /// order is the defect.
    fn contested_flow_links(rules: &[&str]) -> Vec<Link> {
        let mut links = vec![
            mk_link(CONTESTED_URI, "rdf://type", "ad4m://Flow"),
            mk_link(CONTESTED_URI, "ad4m://flowName", &lit_str("Contested")),
        ];
        for r in rules {
            links.push(mk_link(
                CONTESTED_URI,
                "ad4m://consensusRule",
                &format!("literal:string:{}", urlencoding::encode(r)),
            ));
        }
        links
    }

    /// **The premise for every test in this block.** Both literals decode.
    /// If the injected one did not, `ConsensusRuleSlot::Malformed` would
    /// already refuse it via #1079 and there would be no 1b left to fix — a
    /// green suite would then be proving nothing.
    #[test]
    fn both_contesting_rules_are_well_formed() {
        let genuine = serde_json::from_str::<ConsensusRule>(GENUINE_RULE)
            .expect("premise: the author's rule decodes");
        assert_eq!(genuine.n, 5);
        assert_eq!(
            genuine.from_role.as_ref().map(|r| r.class_name.as_str()),
            Some("Reviewer")
        );

        let injected = serde_json::from_str::<ConsensusRule>(INJECTED_RULE)
            .expect("premise: the injected rule decodes too — that is the point");
        assert_eq!(injected.n, 1);
        assert!(
            injected.from_role.is_none(),
            "premise: the injected rule is the permissive default, so a \
             downgrade would be observable as a change in `n`"
        );
    }

    /// **The race, killed.** Both orderings of the same two links must reach
    /// the SAME verdict.
    ///
    /// This is the property that matters, and it is why one ordering proves
    /// nothing: `find_link` was `.iter().find(…)`, so a first-match
    /// implementation passes whichever ordering puts the author's rule first
    /// and fails the other. Before this fix the two orderings disagreed —
    /// one link set, correct gate on one replica and `{n:1}` on another,
    /// with no error on either.
    ///
    /// Asserted through `rule_for`, not only the slot: `Refused` at the
    /// consumer is the behaviour, `Malformed` is just how the parser spells
    /// it.
    ///
    /// Killing mutation: `read_consensus_rule` takes `distinct.first()`
    /// instead of matching on arity. That restores first-match, and the two
    /// halves of this test then disagree.
    #[test]
    fn a_contested_consensus_rule_is_refused_in_either_order() {
        let genuine_first = parse_flow_from_links(
            &contested_flow_links(&[GENUINE_RULE, INJECTED_RULE]),
            CONTESTED_URI,
        )
        .expect("reader");
        let injected_first = parse_flow_from_links(
            &contested_flow_links(&[INJECTED_RULE, GENUINE_RULE]),
            CONTESTED_URI,
        )
        .expect("reader");

        for (label, flow) in [
            ("author's rule first", &genuine_first),
            ("injected rule first", &injected_first),
        ] {
            assert!(
                flow.consensus_rule.is_none(),
                "{label}: neither literal may be decoded into the rule — a rule \
                 present alongside the flag is what `slot_of` would gate on"
            );
            assert!(
                matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                "{label}: a contested scope is unreadable, got {:?}",
                flow.consensus_rule_slot()
            );
            assert!(
                matches!(rule_for(flow, "any_state"), ResolvedRule::Refused),
                "{label}: the consumer must REFUSE, not resolve"
            );
        }
    }

    /// **The positive control.** The same fixture carrying only the author's
    /// rule still parses and still gates at 5-of-N-from-`Reviewer`.
    ///
    /// Without this, an implementation that refused every flow — or one that
    /// dropped `consensusRule` support altogether — passes the test above
    /// and looks like a working refusal.
    ///
    /// Killing mutation: `read_consensus_rule` returns `(None, true)` from
    /// the `[only]` arm as well as the `many` arm.
    #[test]
    fn a_single_consensus_rule_in_the_same_fixture_still_gates() {
        let flow = parse_flow_from_links(&contested_flow_links(&[GENUINE_RULE]), CONTESTED_URI)
            .expect("reader");

        assert!(
            !flow.consensus_rule_malformed,
            "one link is not contested and must not be flagged"
        );
        let ConsensusRuleSlot::Rule(rule) = flow.consensus_rule_slot() else {
            panic!("one link ⇒ Rule, got {:?}", flow.consensus_rule_slot());
        };
        assert_eq!(rule.n, 5, "the authored threshold must survive");
        assert_eq!(
            rule.from_role.as_ref().map(|r| r.class_name.as_str()),
            Some("Reviewer"),
            "the authored role gate must survive"
        );

        let ResolvedRule::Rule(resolved) = rule_for(&flow, "any_state") else {
            panic!("a single readable rule must resolve, not refuse");
        };
        assert_eq!(resolved.n, 5);
    }

    /// Same contest at STATE scope. A separate test because the two scopes
    /// are separate call sites, and the flow-scope one being right says
    /// nothing about this one — the lesson of #1079's own paired scope
    /// tests.
    ///
    /// The flow-level assertion earns its place: a contested STATE rule must
    /// not fall through to the flow's readable one. That substitution — an
    /// unreadable strict gate silently replaced by a weaker readable gate
    /// one scope out — is precisely what `rule_for` refuses by design.
    #[test]
    fn a_contested_state_consensus_rule_is_refused_in_either_order() {
        let flow_uri = "test://StateContestedFlow";
        let state_uri = "test://StateContested.approved";

        let build = |rules: &[&str]| {
            let mut links = vec![
                mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
                mk_link(flow_uri, "ad4m://flowName", &lit_str("StateContested")),
                // A readable, WEAKER flow-level rule to fall back to, so
                // this test can tell a refusal from a fall-through.
                mk_link(
                    flow_uri,
                    "ad4m://consensusRule",
                    &format!("literal:string:{}", urlencoding::encode(INJECTED_RULE)),
                ),
                mk_link(flow_uri, "ad4m://hasState", state_uri),
                mk_link(state_uri, "rdf://type", "ad4m://FlowState"),
                mk_link(state_uri, "ad4m://stateName", &lit_str("approved")),
                mk_link(state_uri, "ad4m://stateValue", &lit_num(1.0)),
            ];
            for r in rules {
                links.push(mk_link(
                    state_uri,
                    "ad4m://consensusRule",
                    &format!("literal:string:{}", urlencoding::encode(r)),
                ));
            }
            parse_flow_from_links(&links, flow_uri).expect("reader")
        };

        for (label, flow) in [
            ("author's rule first", build(&[GENUINE_RULE, INJECTED_RULE])),
            ("injected rule first", build(&[INJECTED_RULE, GENUINE_RULE])),
        ] {
            let state = &flow.states[0];
            assert!(
                matches!(state.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                "{label}: the contested STATE scope is unreadable, got {:?}",
                state.consensus_rule_slot()
            );
            assert!(
                matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Rule(_)),
                "{label}: premise — the flow scope stays readable, so a \
                 fall-through would be observable"
            );
            assert!(
                matches!(rule_for(&flow, "approved"), ResolvedRule::Refused),
                "{label}: a contested state rule must refuse, NOT fall through \
                 to the flow's weaker readable rule"
            );
        }
    }

    /// **The decided edge case: byte-identical repeats are ACCEPTED, as one
    /// rule.**
    ///
    /// The reasoning lives on `read_consensus_rule`; the short version is
    /// that a link *is* `(source, predicate, target)`, so once the loader
    /// has dropped authorship, two links with equal targets are
    /// indistinguishable at this type and identical in effect. There is no
    /// ambiguity about intent to refuse.
    ///
    /// This is not merely defensible, it is required. The loader builds a
    /// multiset and fills it with repeats itself: `load_shacl_flows`
    /// iterates once per `rdf://type ad4m://Flow` link rather than once per
    /// distinct flow URI, and its per-flow `(source = flow_uri)` query
    /// re-collects links the type query already pushed. Counting links
    /// rather than distinct targets would refuse honest flows — and would
    /// hand an attacker something cheaper than the downgrade this closes,
    /// since one extra `rdf://type` link would then wedge every rule on the
    /// flow.
    ///
    /// Killing mutation: drop the `distinct` de-duplication in
    /// `read_consensus_rule` and match on the raw `find_links` length.
    #[test]
    fn byte_identical_consensus_rule_repeats_are_one_rule_not_a_contest() {
        let flow = parse_flow_from_links(
            &contested_flow_links(&[GENUINE_RULE, GENUINE_RULE, GENUINE_RULE]),
            CONTESTED_URI,
        )
        .expect("reader");

        assert!(
            !flow.consensus_rule_malformed,
            "three copies of ONE literal are one authored rule, not a contest"
        );
        let ConsensusRuleSlot::Rule(rule) = flow.consensus_rule_slot() else {
            panic!(
                "identical repeats ⇒ Rule, got {:?}",
                flow.consensus_rule_slot()
            );
        };
        assert_eq!(rule.n, 5, "and it is still the author's rule");
    }

    /// The other half of that decision, written as a test so it cannot be
    /// softened by accident: equality is on the RAW LITERAL, not on the
    /// decoded value. These two literals decode to equal rules — same `n`,
    /// neither with a role gate — and are still refused.
    ///
    /// Deliberately fail-closed. Accepting them would mean defining equality
    /// on `ConsensusRule` (which derives no `PartialEq`) and on every type
    /// it nests, and an equality wrong in the permissive direction reopens
    /// exactly the downgrade this PR closes. Refusing needs no equality at
    /// all. The cost is a loud, local wedge on data no canonical writer
    /// produces — `parse_flow_to_links` emits exactly one `consensusRule`
    /// link per scope.
    #[test]
    fn semantically_equal_but_textually_different_rules_are_still_refused() {
        let a = r#"{"n":3}"#;
        let b = r#"{"n": 3}"#; // one space — same decoded rule
        assert_eq!(
            serde_json::from_str::<ConsensusRule>(a).expect("decodes").n,
            serde_json::from_str::<ConsensusRule>(b).expect("decodes").n,
            "premise: these two literals decode to the same threshold"
        );
        assert_ne!(a, b, "premise: and they are textually different");

        let flow =
            parse_flow_from_links(&contested_flow_links(&[a, b]), CONTESTED_URI).expect("reader");
        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
            "raw-literal equality is the contract: two different literals are a \
             contest even when they would decode alike"
        );
    }

    /// A contested scope must not become readable just because one of the
    /// competing literals is garbage. Two links, one decodable and one not:
    /// still a contest, because the author's intent is still unknown.
    ///
    /// Without this, an implementation that filtered to the decodable
    /// candidates before counting would quietly reintroduce first-match
    /// whenever the attacker's link happened to be malformed — and an
    /// attacker choosing between "downgrade the gate" and "be ignored"
    /// picks downgrade every time.
    ///
    /// Killing mutation: in `read_consensus_rule`, retain only candidates
    /// that decode before matching on arity.
    #[test]
    fn a_contest_between_a_readable_and_an_unreadable_rule_is_still_refused() {
        let garbage = r#"{"threshold":5}"#; // `n` missing — does not decode
        assert!(
            serde_json::from_str::<ConsensusRule>(garbage).is_err(),
            "premise: this literal must fail to deserialise"
        );

        for (label, order) in [
            ("readable first", [GENUINE_RULE, garbage]),
            ("unreadable first", [garbage, GENUINE_RULE]),
        ] {
            let flow = parse_flow_from_links(&contested_flow_links(&order), CONTESTED_URI)
                .expect("reader");
            assert!(
                flow.consensus_rule.is_none(),
                "{label}: the decodable candidate must NOT be adopted"
            );
            assert!(
                matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                "{label}: still a contest"
            );
        }
    }

    /// `slot_of`'s backstop, pinned directly. The parser never writes
    /// `(Some(rule), malformed = true)` — `read_consensus_rule` returns a
    /// rule or the flag, never both — but anything hand-building a
    /// `SHACLFlow` can, and that pair must read as a refusal rather than as
    /// the rule.
    ///
    /// This is here to pin WHICH guard answered. Without it, the contested
    /// tests above would still pass on an implementation that decoded the
    /// first candidate into `consensus_rule` and merely set the flag
    /// alongside it, because `slot_of` would then be the thing refusing. The
    /// `consensus_rule.is_none()` assertions there pin the parser's half;
    /// this pins `slot_of`'s half.
    ///
    /// Killing mutation: restore `(Some(r), _) => Rule(r)` as the first arm
    /// of `slot_of`.
    #[test]
    fn a_rule_present_alongside_the_unreadable_flag_reads_as_unreadable() {
        let mut flow = parse_flow_from_links(&contested_flow_links(&[GENUINE_RULE]), CONTESTED_URI)
            .expect("reader");
        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Rule(_)),
            "premise: this flow starts out readable"
        );

        // The contradictory pair: a decoded rule AND the unreadable flag.
        flow.consensus_rule_malformed = true;

        assert!(
            flow.consensus_rule.is_some(),
            "premise: the rule is still present — the flag alone is the change"
        );
        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
            "a present rule must not outrank the unreadable flag; \
             `could not be determined` is never a verdict (#1064)"
        );
    }

    // -------------------------------------------------------------------
    // #1082 — a shadowed state name.
    //
    // The author gates `approved` at 5-of-N-from-`Reviewer`. A peer
    // publishes a SECOND state on the same flow, carrying the same
    // `stateName`, a lower `stateValue`, and a weaker rule of their own.
    // Nothing is malformed and no single source carries two rules, so
    // neither #1078's decode check nor #1080's contested-scope check has
    // anything to fire on: the parser saw two well-formed states and the
    // consumers took the first match by name.
    //
    // Every fixture below also carries an UNSHADOWED state with its own
    // readable rule, so a refusal that is merely "refuse everything" is
    // visible as a failure of the positive control rather than as a pass.
    // -------------------------------------------------------------------

    const SHADOW_FLOW_URI: &str = "test://ShadowedFlow";
    /// The state the author declared and gated.
    const AUTHOR_STATE_URI: &str = "test://Shadowed.approved";
    /// The state a peer publishes to answer to the same name.
    const SHADOW_STATE_URI: &str = "test://Shadowed.approved-EVIL";
    /// A state nobody shadows — the positive control, in-fixture.
    const UNSHADOWED_STATE_URI: &str = "test://Shadowed.done";
    const SHADOWED_NAME: &str = "approved";

    /// One state's links, as a test writes them. `rule: None` = the state
    /// carries no `consensusRule` link at all, which is the *absent* half
    /// of the attack (see
    /// `a_shadow_carrying_no_rule_does_not_reopen_the_permissive_default`).
    struct StateSpec<'a> {
        uri: &'a str,
        name: &'a str,
        value: f64,
        rule: Option<&'a str>,
    }

    /// Build a flow whose `hasState` edges appear in exactly the given
    /// order. Discovery order is a parameter because discovery order is
    /// half the defect — the other half is `stateValue`, which each spec
    /// supplies.
    fn shadowed_flow_links(states: &[StateSpec]) -> Vec<Link> {
        let mut links = vec![
            mk_link(SHADOW_FLOW_URI, "rdf://type", "ad4m://Flow"),
            mk_link(SHADOW_FLOW_URI, "ad4m://flowName", &lit_str("Shadowed")),
        ];
        for s in states {
            links.push(mk_link(SHADOW_FLOW_URI, "ad4m://hasState", s.uri));
            links.push(mk_link(s.uri, "rdf://type", "ad4m://FlowState"));
            // An empty name models a state whose `stateName` link is absent
            // — no link at all, not a link carrying `""`.
            if !s.name.is_empty() {
                links.push(mk_link(s.uri, "ad4m://stateName", &lit_str(s.name)));
            }
            links.push(mk_link(s.uri, "ad4m://stateValue", &lit_num(s.value)));
            if let Some(r) = s.rule {
                links.push(mk_link(
                    s.uri,
                    "ad4m://consensusRule",
                    &format!("literal:string:{}", urlencoding::encode(r)),
                ));
            }
        }
        links
    }

    fn author_state() -> StateSpec<'static> {
        StateSpec {
            uri: AUTHOR_STATE_URI,
            name: SHADOWED_NAME,
            value: 1.0,
            rule: Some(GENUINE_RULE),
        }
    }

    /// Lower `stateValue` than the author's, so the sort puts it first
    /// whatever the discovery order — that is what makes the downgrade
    /// deterministic rather than a race.
    fn shadow_state() -> StateSpec<'static> {
        StateSpec {
            uri: SHADOW_STATE_URI,
            name: SHADOWED_NAME,
            value: 0.5,
            rule: Some(INJECTED_RULE),
        }
    }

    fn unshadowed_state() -> StateSpec<'static> {
        StateSpec {
            uri: UNSHADOWED_STATE_URI,
            name: "done",
            value: 2.0,
            rule: Some(GENUINE_RULE),
        }
    }

    fn parse_shadowed(states: &[StateSpec]) -> SHACLFlow {
        parse_flow_from_links(&shadowed_flow_links(states), SHADOW_FLOW_URI).expect("reader")
    }

    /// **The premise.** Both states are well-formed and both rules decode,
    /// so nothing already in the parser can be what refuses them. Without
    /// this, a green suite below could be #1078's decode check firing on a
    /// fixture typo rather than the shadow check firing on the shadow.
    #[test]
    fn the_shadow_state_and_its_rule_are_both_well_formed() {
        let flow = parse_shadowed(&[author_state(), unshadowed_state()]);
        let state = flow
            .states
            .iter()
            .find(|s| s.name == SHADOWED_NAME)
            .expect("premise: the author's state parses");
        assert!(
            !state.consensus_rule_malformed,
            "premise: the author's state on its own is readable"
        );
        assert_eq!(
            state.consensus_rule.as_ref().map(|r| r.n),
            Some(5),
            "premise: and it carries the 5-of-N gate"
        );

        let injected = serde_json::from_str::<ConsensusRule>(INJECTED_RULE)
            .expect("premise: the injected rule decodes too — that is the point");
        assert_eq!(injected.n, 1);
        assert!(
            injected.from_role.is_none(),
            "premise: the injected rule is the permissive default, so a \
             downgrade is observable as a change in `n`"
        );
    }

    /// **The attack, killed — and killed in BOTH discovery orders.**
    ///
    /// A peer's state carrying the author's `stateName` and a lower
    /// `stateValue` sorted ahead of the author's, and `rule_for` took the
    /// first match, so `{n:1}` governed a 5-of-N-`Reviewer` gate on every
    /// replica.
    ///
    /// Both orderings are asserted because a fix that flagged only the
    /// duplicates it met *after* the first would still leave the peer's
    /// rule governing whenever the peer's `hasState` link was discovered
    /// first — and a test pinning one arrangement would pass on it. The
    /// verdict is asserted too, not just agreement between the orderings:
    /// the unfixed parser also *agrees* across discovery orders here, and
    /// agrees on the wrong answer.
    ///
    /// Asserted through `rule_for`, so the test pins the consumer verdict
    /// rather than the parser's spelling of it.
    ///
    /// Killing mutation: `return` at the top of
    /// `refuse_shadowed_state_names`.
    #[test]
    fn a_shadowed_state_name_is_refused_in_either_discovery_order() {
        for (label, flow) in [
            (
                "author's state first",
                parse_shadowed(&[author_state(), shadow_state(), unshadowed_state()]),
            ),
            (
                "shadow discovered first",
                parse_shadowed(&[shadow_state(), author_state(), unshadowed_state()]),
            ),
        ] {
            assert_eq!(
                flow.states
                    .iter()
                    .filter(|s| s.name == SHADOWED_NAME)
                    .count(),
                2,
                "{label}: premise — two DIFFERENT state URIs claim the name, so \
                 this is a shadow and not a deduped repeat"
            );
            for s in flow.states.iter().filter(|s| s.name == SHADOWED_NAME) {
                assert!(
                    s.consensus_rule.is_none(),
                    "{label}: neither rule may be decoded into the slot — a rule \
                     left beside the flag is what a later refactor gates on"
                );
                assert!(
                    matches!(s.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                    "{label}: every claimant of a shadowed name is unreadable, got {:?}",
                    s.consensus_rule_slot()
                );
            }
            assert!(
                matches!(rule_for(&flow, SHADOWED_NAME), ResolvedRule::Refused),
                "{label}: the consumer must REFUSE, not resolve to whichever \
                 state sorted first"
            );

            // In-fixture positive control: the shadow wedges its own name
            // and nothing else.
            let ResolvedRule::Rule(done) = rule_for(&flow, "done") else {
                panic!("{label}: an unshadowed state must still gate normally");
            };
            assert_eq!(
                done.n, 5,
                "{label}: and it must still gate at the author's threshold"
            );
        }
    }

    /// **The positive control, standalone.** The same fixture without the
    /// shadow parses and gates exactly as before.
    ///
    /// Without it, an implementation that flagged every state passes the
    /// test above and looks like a working refusal.
    ///
    /// Killing mutation: in `refuse_shadowed_state_names`, collect
    /// `(0..states.len())` unconditionally instead of filtering on a
    /// competing claimant.
    #[test]
    fn unique_state_names_in_the_same_fixture_still_gate() {
        let flow = parse_shadowed(&[author_state(), unshadowed_state()]);

        assert!(
            flow.states.iter().all(|s| !s.consensus_rule_malformed),
            "no name is claimed twice, so nothing may be flagged"
        );
        for (name, expected_n) in [(SHADOWED_NAME, 5), ("done", 5)] {
            let ResolvedRule::Rule(rule) = rule_for(&flow, name) else {
                panic!("`{name}` has a unique name and must resolve, not refuse");
            };
            assert_eq!(rule.n, expected_n, "the authored threshold must survive");
            assert_eq!(
                rule.from_role.as_ref().map(|r| r.class_name.as_str()),
                Some("Reviewer"),
                "and so must the authored role gate"
            );
        }
    }

    /// **The decided edge case: byte-identical `hasState` repeats are
    /// ACCEPTED, as one state.**
    ///
    /// Two links with equal `(source, predicate, target)` name the same
    /// state URI, and every property is re-read from that URI, so the
    /// second link cannot change anything — the same equivalence, for the
    /// same reason, that `read_consensus_rule` applies to repeated
    /// `consensusRule` literals (#1080). It is also required rather than
    /// merely defensible: `load_shacl_flows` builds a multiset and
    /// re-collects links its own type query already pushed, so refusing on
    /// repeats would wedge honest flows — and would hand an attacker
    /// something cheaper than the downgrade this closes.
    ///
    /// Killing mutation: in `parse_flow_from_links`, drop the `state_uris`
    /// de-duplication and iterate `find_links(…, "ad4m://hasState")`
    /// directly.
    #[test]
    fn byte_identical_has_state_repeats_are_one_state_not_a_shadow() {
        let mut links = shadowed_flow_links(&[author_state(), unshadowed_state()]);
        // The repeat: the same edge to the same URI, twice more.
        links.push(mk_link(
            SHADOW_FLOW_URI,
            "ad4m://hasState",
            AUTHOR_STATE_URI,
        ));
        links.push(mk_link(
            SHADOW_FLOW_URI,
            "ad4m://hasState",
            AUTHOR_STATE_URI,
        ));

        let flow = parse_flow_from_links(&links, SHADOW_FLOW_URI).expect("reader");

        assert_eq!(
            flow.states.len(),
            2,
            "three edges to two URIs are two states: {:?}",
            flow.states.iter().map(|s| &s.name).collect::<Vec<_>>()
        );
        assert!(
            flow.states.iter().all(|s| !s.consensus_rule_malformed),
            "a repeated edge is a copy, and a copy changes nothing"
        );
        let ResolvedRule::Rule(rule) = rule_for(&flow, SHADOWED_NAME) else {
            panic!("identical repeats must not refuse");
        };
        assert_eq!(rule.n, 5, "and it is still the author's rule");
    }

    /// **The other half of that decision, so it cannot be softened by
    /// accident: equality is on the state URI, NOT on the derived
    /// `FlowState`.** Two different URIs agreeing on `stateName`,
    /// `stateValue` and `consensusRule` are still refused.
    ///
    /// Deliberately fail-closed. Accepting them would mean defining
    /// equality on `FlowState` and every type it nests (`ConsensusRule`,
    /// `ModelQuery`, `PropertyCondition` — none of which derives
    /// `PartialEq`), and it would make the refusal conditional on the
    /// attacker's fields *currently* matching the author's: a condition the
    /// attacker controls, and can flip with one more link once the flow is
    /// live. An equality wrong in the permissive direction reopens exactly
    /// the downgrade this closes.
    ///
    /// Equal values also make the `stateValue` sort a TIE, which is
    /// resolved by discovery order (`sort_by` is stable) — so this is the
    /// sharper both-orderings case: pre-fix, these two arrangements
    /// disagreed with each other.
    ///
    /// Killing mutation: skip an index in `refuse_shadowed_state_names`
    /// when the competing claimant's `value` and `consensus_rule`-shaped
    /// fields agree — i.e. compare derived records instead of URIs.
    #[test]
    fn a_shadow_agreeing_on_name_and_value_is_still_refused() {
        let twin = |uri: &'static str| StateSpec {
            uri,
            name: SHADOWED_NAME,
            value: 1.0,
            rule: Some(GENUINE_RULE),
        };

        for (label, flow) in [
            (
                "author's URI first",
                parse_shadowed(&[
                    twin(AUTHOR_STATE_URI),
                    twin(SHADOW_STATE_URI),
                    unshadowed_state(),
                ]),
            ),
            (
                "shadow URI first",
                parse_shadowed(&[
                    twin(SHADOW_STATE_URI),
                    twin(AUTHOR_STATE_URI),
                    unshadowed_state(),
                ]),
            ),
        ] {
            assert!(
                matches!(rule_for(&flow, SHADOWED_NAME), ResolvedRule::Refused),
                "{label}: two URIs are two states even when today's properties \
                 agree; a state's identity is its URI"
            );
            assert!(
                matches!(rule_for(&flow, "done"), ResolvedRule::Rule(_)),
                "{label}: and the refusal is still local to the shadowed name"
            );
        }
    }

    /// The *absent*-rule half of the attack. The peer's shadow carries no
    /// `consensusRule` at all, and there is no flow-level rule to fall back
    /// to — so pre-fix the first match's `Absent` slot resolved to the
    /// `{n: 1}` default, downgrading the author's state-level 5-of-N gate
    /// just as effectively as an injected weak rule, and without the
    /// attacker writing a rule at all.
    ///
    /// This is the case a fix keyed on *rules* rather than on *names* would
    /// miss entirely: there is no second rule anywhere to compare.
    ///
    /// Killing mutation: in `refuse_shadowed_state_names`, flag only states
    /// that carry a `consensus_rule` of their own.
    #[test]
    fn a_shadow_carrying_no_rule_does_not_reopen_the_permissive_default() {
        let ruleless_shadow = StateSpec {
            uri: SHADOW_STATE_URI,
            name: SHADOWED_NAME,
            value: 0.5,
            rule: None,
        };
        let flow = parse_shadowed(&[author_state(), ruleless_shadow, unshadowed_state()]);

        assert!(
            matches!(flow.consensus_rule_slot(), ConsensusRuleSlot::Absent),
            "premise: no flow-level rule, so an unrefused fall-through lands on \
             the permissive `{{n:1}}` default rather than on another gate"
        );
        assert!(
            matches!(rule_for(&flow, SHADOWED_NAME), ResolvedRule::Refused),
            "a shadow with no rule of its own must refuse, not default to \
             one-vote-from-anybody"
        );
    }

    /// Two states that both lack a `stateName` link both decode to `""`,
    /// and `""` identifies neither of them — the same unreadable shape,
    /// reached without anyone writing a colliding name on purpose.
    ///
    /// Included because the empty name is the one a half-synced definition
    /// produces (`flow_spawn::initial_state_of` already treats a single
    /// nameless state as half-synced), so leaving it out of the check would
    /// be an untested carve-out in the permissive direction.
    ///
    /// Killing mutation: in `refuse_shadowed_state_names`, skip indices
    /// whose `name` is empty.
    #[test]
    fn two_nameless_states_shadow_each_other() {
        let nameless = |uri: &'static str, value: f64, rule: Option<&'static str>| StateSpec {
            uri,
            name: "",
            value,
            rule,
        };
        let flow = parse_shadowed(&[
            nameless(AUTHOR_STATE_URI, 1.0, Some(GENUINE_RULE)),
            nameless(SHADOW_STATE_URI, 0.5, Some(INJECTED_RULE)),
            unshadowed_state(),
        ]);

        assert!(
            matches!(rule_for(&flow, ""), ResolvedRule::Refused),
            "two nameless states are as unresolvable as two named ones"
        );
        assert!(
            matches!(rule_for(&flow, "done"), ResolvedRule::Rule(_)),
            "and the named state in the same flow is untouched"
        );
    }

    /// A single nameless state is NOT a shadow — one claimant is one
    /// claimant, whatever its name. Pins that the empty-name handling above
    /// is about collision and not about emptiness, so a half-synced
    /// definition still parses (the spawn path has its own guard for it,
    /// `flow_spawn::initial_state_of`, and that guard stays the one that
    /// decides).
    ///
    /// Killing mutation: in `refuse_shadowed_state_names`, flag any index
    /// whose `name` is empty regardless of a competing claimant.
    #[test]
    fn one_nameless_state_is_not_a_shadow() {
        let flow = parse_shadowed(&[
            StateSpec {
                uri: AUTHOR_STATE_URI,
                name: "",
                value: 0.0,
                rule: Some(GENUINE_RULE),
            },
            unshadowed_state(),
        ]);

        assert!(
            flow.states.iter().all(|s| !s.consensus_rule_malformed),
            "one nameless state collides with nothing"
        );
        assert!(
            matches!(rule_for(&flow, ""), ResolvedRule::Rule(_)),
            "and it still resolves"
        );
    }

    /// Non-Flow-suffix URI → error. Prevents silent misuse where a
    /// caller passes a state URI expecting flow output.
    #[test]
    fn parse_flow_from_links_rejects_non_flow_uri() {
        let err = parse_flow_from_links(&[], "test://NotEndingProperly").unwrap_err();
        assert!(format!("{err}").contains("must end with 'Flow'"));
    }

    /// Empty inputTypes array on-graph → reader treats as unset
    /// (mirrors TS `if (this.inputTypes.length > 0)` on the write side,
    /// so absence and empty-array are indistinguishable — both read as
    /// empty vec).
    #[test]
    fn parse_flow_from_links_treats_absent_input_types_as_empty() {
        let flow_uri = "test://EmptyFlow";
        let links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Empty")),
        ];
        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        assert!(flow.input_types.is_empty());
        assert!(flow.output_types.is_empty());
    }

    #[test]
    fn parse_flow_from_links_sorts_states_by_value() {
        // Link order on the graph is not preserved, so the reader must impose
        // the same `value` ordering TS `SHACLFlow.fromLinks` does. The
        // convention riding on it — "initial state = states[0]", which
        // `FlowInstance.start` consumes — would otherwise resolve differently
        // in the two runtimes, and a Rust-side spawn would mint instances in
        // whichever state happened to be discovered first.
        let flow_uri = "order://OrderFlow";
        let mut links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Order")),
            mk_link(flow_uri, "ad4m://namespace", &lit_str("order://")),
        ];
        // Declared deliberately out of order: last, first, middle.
        for (name, value) in [("done", 1.0), ("identified", 0.0), ("scoped", 0.5)] {
            let state_uri = format!("order://Order.{name}");
            links.push(mk_link(flow_uri, "ad4m://hasState", &state_uri));
            links.push(mk_link(&state_uri, "ad4m://stateName", &lit_str(name)));
            links.push(mk_link(&state_uri, "ad4m://stateValue", &lit_num(value)));
        }

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");

        assert_eq!(
            flow.states
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>(),
            vec!["identified", "scoped", "done"],
            "states must come back ordered by declared value, not link order"
        );
    }

    #[test]
    fn parse_flow_from_links_sorts_nan_state_values_last() {
        // `decode_literal_number` is `str::parse::<f64>`, so a literal of `NaN`
        // on the graph decodes to one. Comparing it as "equal to everything"
        // would break `sort_by`'s total-order contract and scramble the
        // *finite* states along with it — and `states[0]` is the initial state,
        // so the spawn would start in the wrong place. A state whose ordering
        // value is undecodable has no claim to being first.
        let flow_uri = "nan://NanFlow";
        let mut links = vec![
            mk_link(flow_uri, "rdf://type", "ad4m://Flow"),
            mk_link(flow_uri, "ad4m://flowName", &lit_str("Nan")),
            mk_link(flow_uri, "ad4m://namespace", &lit_str("nan://")),
        ];
        for (name, value) in [
            ("done", 1.0_f64),
            ("broken", f64::NAN),
            ("identified", 0.0_f64),
        ] {
            let state_uri = format!("nan://Nan.{name}");
            links.push(mk_link(flow_uri, "ad4m://hasState", &state_uri));
            links.push(mk_link(&state_uri, "ad4m://stateName", &lit_str(name)));
            links.push(mk_link(&state_uri, "ad4m://stateValue", &lit_num(value)));
        }

        let flow = parse_flow_from_links(&links, flow_uri).expect("reader");

        assert!(
            flow.states.iter().any(|s| s.value.is_nan()),
            "fixture must actually round-trip a NaN, otherwise this proves nothing"
        );
        assert_eq!(
            flow.states
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>(),
            vec!["identified", "done", "broken"],
            "finite states keep value order and the NaN one sorts last"
        );
    }
}
