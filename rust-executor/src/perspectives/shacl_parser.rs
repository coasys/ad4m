use crate::types::Link;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

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
#[derive(Debug, Deserialize, Serialize)]
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
}

impl SHACLFlow {
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
// flows are declared on the perspective without a JS/GraphQL round-trip.
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

/// Decode a JSON payload stored inside a `literal:string:<urlencoded json>`
/// target. Silently returns `None` on any decode / parse / shape failure —
/// callers that see `None` leave the corresponding field unset rather than
/// failing the whole flow read (mirrors the TS side's try/catch policy).
fn decode_json_literal<T: for<'de> Deserialize<'de>>(target: &str) -> Option<T> {
    let s = decode_literal_string(target)?;
    serde_json::from_str(&s).ok()
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

/// Reader-side validator: a `Vec<ModelQuery>` payload is only accepted
/// when every entry has a non-empty `className` string. The `#[serde(untagged)]`
/// on `PropertyCondition` makes it too permissive to reject `[{}]` /
/// `[{"className": 42}]` at the serde layer — this catches those before
/// they end up in the returned `SHACLFlow`. Symmetric with the TS
/// `isModelQueryShape` guard.
fn model_query_array_ok(qs: &[ModelQuery]) -> bool {
    qs.iter().all(|q| !q.class_name.is_empty())
}

fn decode_model_query_array(target: &str) -> Option<Vec<ModelQuery>> {
    let qs: Vec<ModelQuery> = decode_json_literal(target)?;
    if model_query_array_ok(&qs) {
        Some(qs)
    } else {
        None
    }
}

fn decode_string_array(target: &str) -> Option<Vec<String>> {
    let arr: Vec<String> = decode_json_literal(target)?;
    if arr.iter().any(|s| s.is_empty()) {
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
        if let Some(arr) = decode_string_array(&link.target) {
            flow.input_types = arr;
        }
    }
    if let Some(link) = find_link(links, flow_uri, "ad4m://outputTypes") {
        if let Some(arr) = decode_string_array(&link.target) {
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
        flow.context = decode_model_query_array(&link.target);
    }

    // Flow-level `consensusRule` — untagged; a missing `n` or invalid
    // `fromRole` leaves the field unset rather than shipping half-typed
    // data to the consensus engine.
    if let Some(link) = find_link(links, flow_uri, "ad4m://consensusRule") {
        if let Some(rule) = decode_json_literal::<ConsensusRule>(&link.target) {
            flow.consensus_rule = Some(rule);
        }
    }

    // States — walk every `hasState` edge, gather each state's own
    // properties. Build a state-uri → state-name index so transition
    // parsing can resolve endpoints.
    let mut state_uri_to_name: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for state_link in find_links(links, flow_uri, "ad4m://hasState") {
        let state_uri = &state_link.target;
        let state_name = find_link(links, state_uri, "ad4m://stateName")
            .and_then(|l| decode_literal_string(&l.target))
            .unwrap_or_default();
        state_uri_to_name.insert(state_uri.clone(), state_name.clone());

        let value = find_link(links, state_uri, "ad4m://stateValue")
            .and_then(|l| decode_literal_number(&l.target))
            .unwrap_or(0.0);

        let interpretation_hint = find_link(links, state_uri, "ad4m://interpretationHint")
            .and_then(|l| decode_literal_string(&l.target).filter(|s| !s.is_empty()));

        let requires = find_link(links, state_uri, "ad4m://requires")
            .and_then(|l| decode_model_query_array(&l.target));

        let semantic_check = find_link(links, state_uri, "ad4m://semanticCheck")
            .and_then(|l| decode_literal_string(&l.target).filter(|s| !s.is_empty()));

        let consensus_rule = find_link(links, state_uri, "ad4m://consensusRule")
            .and_then(|l| decode_json_literal::<ConsensusRule>(&l.target));

        flow.states.push(FlowState {
            name: state_name,
            value,
            interpretation_hint,
            requires,
            semantic_check,
            consensus_rule,
        });
    }

    // Sort states by `value`, matching TS `SHACLFlow.fromLinks`
    // (`core/src/shacl/SHACLFlow.ts`) and for the same reason: link order is
    // not preserved on the graph, so the only stable ordering is the declared
    // `value`. The convention that rests on it — "the initial state is
    // `states[0]`", which `FlowInstance.start` consumes on the TS side — would
    // otherwise resolve differently in the two runtimes whenever
    // link-discovery order differs from value order, and a Rust-side spawn
    // would mint instances in the wrong starting state. Ties keep discovery
    // order (`sort_by` is stable), which is as arbitrary as the declaration
    // that produced them.
    flow.states.sort_by(|a, b| {
        a.value
            .partial_cmp(&b.value)
            .unwrap_or(std::cmp::Ordering::Equal)
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
            .and_then(|l| decode_json_literal::<Vec<AD4MAction>>(&l.target))
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

/// Parse SHACL JSON to RDF links (Option 3: Named Property Shapes)
pub fn parse_shacl_to_links(shacl_json: &str, class_name: &str) -> Result<Vec<Link>, AnyError> {
    let shape: SHACLShape = serde_json::from_str(shacl_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse SHACL JSON: {}", e))?;

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
}
