//! Flow definitions and running flow instances — the Rust analogue of the
//! TypeScript SDK's `FlowInstance` (`core/src/perspectives/FlowInstance.ts`)
//! and `SHACLFlow` (`core/src/shacl/SHACLFlow.ts`), over the executor's
//! `perspective.*` flow RPCs.
//!
//! A [`ShaclFlow`] is the flow definition — states, transitions, consensus
//! rules — stored on the perspective as links and registered with
//! [`FlowApi::add_flow`] (`perspective.addSdna`, `sdnaType: "flow"`). A
//! [`FlowInstance`] wraps a `FlowInstance` graph record plus its parsed
//! definition: `start` mints or reuses the record, `find_all` reads them,
//! and `propose_transition` / `accept_proposal` / `reject_proposal` are the
//! consensus writes — the same methods the TS class carries.
//!
//! Flow *state* is never on the record: each agent's executor derives it
//! from the signed proposals and keeps it per agent (`perspective.flowStates`),
//! so `current_state_name` is a display snapshot — authority lives in the
//! fold executor-side.

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::Arc;

use crate::literal::{Literal, LiteralValue};
use crate::model::{Model, ModelQuery};
use crate::types::{Link, LinkExpression, LinkExpressionInput};
use crate::ws_rpc::WsRpcClient;

// ── Flow definition types (mirror the executor's `SHACLFlow` serde) ────────

/// One link operation a constructor, destructor or transition carries
/// (`rust-executor`'s `AD4MAction`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ad4mAction {
    pub action: String,
    pub source: String,
    pub predicate: String,
    pub target: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub local: Option<bool>,
}

/// A `requires`/`context`/`fromRole` model query. The client never evaluates
/// one — guards run executor-side — so it stays JSON and passes through
/// byte-exact (TS `ModelQuery` of `SHACLFlow.ts`).
pub type FlowModelQuery = Value;

/// How many distinct DIDs must sign a proposal before a transition fires
/// (`ConsensusRule`; `fromRole` restricts which agents count).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConsensusRule {
    pub n: u32,
    #[serde(rename = "fromRole", default, skip_serializing_if = "Option::is_none")]
    pub from_role: Option<FlowModelQuery>,
}

/// One state of a flow — TS `FlowState`, executor `FlowState`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct FlowState {
    pub name: String,
    /// Ordering value: `states[0]` after the value-then-name sort is the
    /// initial state.
    pub value: f64,
    #[serde(
        rename = "interpretationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub interpretation_hint: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub requires: Option<Vec<FlowModelQuery>>,
    #[serde(
        rename = "semanticCheck",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub semantic_check: Option<String>,
    #[serde(
        rename = "consensusRule",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub consensus_rule: Option<ConsensusRule>,
}

/// A state edge — TS `FlowTransition`. The executor's serde keeps the
/// snake_case field names; that is the shape `addSdna` parses.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FlowTransition {
    pub action_name: String,
    pub from_state: String,
    pub to_state: String,
    #[serde(default)]
    pub actions: Vec<Ad4mAction>,
}

/// A flow definition — TS `SHACLFlow`, executor `SHACLFlow`. Serializes to
/// the JSON `perspective.addSdna` parses for `sdnaType: "flow"`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ShaclFlow {
    pub name: String,
    pub namespace: String,
    #[serde(default)]
    pub states: Vec<FlowState>,
    #[serde(default)]
    pub transitions: Vec<FlowTransition>,
    #[serde(
        rename = "interpretationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub interpretation_hint: Option<String>,
    #[serde(rename = "inputTypes", default)]
    pub input_types: Vec<String>,
    #[serde(rename = "outputTypes", default)]
    pub output_types: Vec<String>,
    #[serde(
        rename = "creationHint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub creation_hint: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub context: Option<Vec<FlowModelQuery>>,
    #[serde(
        rename = "consensusRule",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub consensus_rule: Option<ConsensusRule>,
}

impl ShaclFlow {
    pub fn new(name: impl Into<String>, namespace: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            namespace: namespace.into(),
            states: Vec::new(),
            transitions: Vec::new(),
            interpretation_hint: None,
            input_types: Vec::new(),
            output_types: Vec::new(),
            creation_hint: None,
            context: None,
            consensus_rule: None,
        }
    }

    /// Canonical URI — `${namespace}${name}Flow` — the on-graph identity a
    /// `FlowInstance` record stores.
    pub fn flow_uri(&self) -> String {
        format!("{}{}Flow", self.namespace, self.name)
    }

    /// `{namespace}{name}.{state}` — a state's URI under this flow.
    pub fn state_uri(&self, state_name: &str) -> String {
        format!("{}{}.{}", self.namespace, self.name, state_name)
    }

    /// `{namespace}{name}.transition/{from}/{to}/{action}`, each part
    /// percent-encoded — the URI both runtimes write.
    pub fn transition_uri(&self, from_state: &str, to_state: &str, action_name: &str) -> String {
        format!(
            "{}{}.transition/{}/{}/{}",
            self.namespace,
            self.name,
            urlencoding::encode(from_state),
            urlencoding::encode(to_state),
            urlencoding::encode(action_name)
        )
    }

    /// Sort `states` into the canonical order both runtimes enforce on read:
    /// `value` ascending, NaN last, ties by state name (UTF-8 byte order —
    /// TS `compareCodePoints`). `states[0]` is the initial state.
    fn sort_states(&mut self) {
        self.states
            .sort_by(|a, b| match (a.value.is_nan(), b.value.is_nan()) {
                (false, false) => a
                    .value
                    .partial_cmp(&b.value)
                    .unwrap_or(std::cmp::Ordering::Equal)
                    .then_with(|| a.name.cmp(&b.name)),
                (true, true) => a.name.cmp(&b.name),
                (true, false) => std::cmp::Ordering::Greater,
                (false, true) => std::cmp::Ordering::Less,
            });
    }

    /// Rebuild a flow from its stored links — the client-side twin of TS
    /// `SHACLFlow.fromLinks` and executor `parse_flow_from_links`: the same
    /// predicates, the same decoders, the same state sort. Malformed
    /// literals leave their field unset, as both runtimes read them.
    pub fn from_links(links: &[LinkExpression], flow_uri: &str) -> Result<Self> {
        let without_suffix = flow_uri
            .strip_suffix("Flow")
            .ok_or_else(|| anyhow!("Invalid flow URI: {flow_uri} (must end with 'Flow')"))?;
        let split_idx = without_suffix.rfind(['/', ':']).map(|i| i + 1).unwrap_or(0);
        let (namespace, name) = without_suffix.split_at(split_idx);
        let mut flow = Self::new(name, namespace);

        let find = |source: &str, predicate: &str| -> Option<&LinkExpression> {
            links
                .iter()
                .find(|l| l.data.source == source && l.data.predicate.as_deref() == Some(predicate))
        };
        let find_all = |source: &str, predicate: &str| -> Vec<&LinkExpression> {
            links
                .iter()
                .filter(|l| {
                    l.data.source == source && l.data.predicate.as_deref() == Some(predicate)
                })
                .collect()
        };

        if let Some(l) = find(flow_uri, "ad4m://interpretationHint") {
            flow.interpretation_hint =
                decode_literal_string(&l.data.target).filter(|s| !s.is_empty());
        }
        if let Some(l) = find(flow_uri, "ad4m://inputTypes") {
            flow.input_types = decode_string_array(&l.data.target).unwrap_or_default();
        }
        if let Some(l) = find(flow_uri, "ad4m://outputTypes") {
            flow.output_types = decode_string_array(&l.data.target).unwrap_or_default();
        }
        if let Some(l) = find(flow_uri, "ad4m://creationHint") {
            flow.creation_hint = decode_literal_string(&l.data.target).filter(|s| !s.is_empty());
        }
        if let Some(l) = find(flow_uri, "ad4m://context") {
            flow.context = decode_json_literal(&l.data.target);
        }
        if let Some(l) = find(flow_uri, "ad4m://consensusRule") {
            flow.consensus_rule = decode_json_literal(&l.data.target);
        }

        // States — distinct `hasState` targets (a repeated edge is one state).
        let mut state_uri_to_name: HashMap<String, String> = HashMap::new();
        let mut seen: BTreeSet<String> = BTreeSet::new();
        for state_link in find_all(flow_uri, "ad4m://hasState") {
            let state_uri = &state_link.data.target;
            if !seen.insert(state_uri.clone()) {
                continue;
            }
            let state_name = find(state_uri, "ad4m://stateName")
                .and_then(|l| decode_literal_string(&l.data.target))
                .unwrap_or_default();
            state_uri_to_name.insert(state_uri.clone(), state_name.clone());
            // A missing stateValue link is 0.0 (TS/Rust agree); a present
            // one decodes on the shared grammar, NaN when it does not fit.
            let value = find(state_uri, "ad4m://stateValue")
                .map(|l| decode_state_value(&l.data.target))
                .unwrap_or(0.0);
            flow.states.push(FlowState {
                name: state_name,
                value,
                interpretation_hint: find(state_uri, "ad4m://interpretationHint")
                    .and_then(|l| decode_literal_string(&l.data.target))
                    .filter(|s| !s.is_empty()),
                requires: find(state_uri, "ad4m://requires")
                    .and_then(|l| decode_json_literal(&l.data.target)),
                semantic_check: find(state_uri, "ad4m://semanticCheck")
                    .and_then(|l| decode_literal_string(&l.data.target))
                    .filter(|s| !s.is_empty()),
                consensus_rule: find(state_uri, "ad4m://consensusRule")
                    .and_then(|l| decode_json_literal(&l.data.target)),
            });
        }
        flow.sort_states();

        // Transitions — endpoints resolve through the state-name index.
        for t_link in find_all(flow_uri, "ad4m://hasTransition") {
            let uri = &t_link.data.target;
            let action_name = find(uri, "ad4m://actionName")
                .and_then(|l| decode_literal_string(&l.data.target))
                .unwrap_or_default();
            let from_state = find(uri, "ad4m://fromState")
                .and_then(|l| state_uri_to_name.get(&l.data.target).cloned())
                .unwrap_or_default();
            let to_state = find(uri, "ad4m://toState")
                .and_then(|l| state_uri_to_name.get(&l.data.target).cloned())
                .unwrap_or_default();
            let actions = find(uri, "ad4m://transitionActions")
                .and_then(|l| decode_json_literal(&l.data.target))
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

    /// The definition's link set — TS `SHACLFlow.toLinks()`, the `wanted`
    /// of `addFlow`'s diff. Predicates and literal encodings match the
    /// executor's `parse_flow_to_links` except `ad4m://transitionActions`,
    /// which TS writes unconditionally (an empty list is still a written
    /// field) and the executor only when non-empty; this writer follows
    /// TS.
    pub fn to_links(&self) -> Result<Vec<Link>> {
        let mut links = Vec::new();
        let flow_uri = self.flow_uri();
        let mut push = |source: &str, predicate: &str, target: String| {
            links.push(Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target,
            });
        };
        let lit = |s: &str| -> Result<String> { Literal::from_string(s.to_string()).to_url() };
        // `literal:string:` + percent-encoded JSON — the writer format
        // `toLinks` uses for inputTypes/requires/transitionActions/…
        let json_lit = |v: &Value| -> String {
            format!("literal:string:{}", urlencoding::encode(&v.to_string()))
        };

        push(&flow_uri, "rdf://type", "ad4m://Flow".to_string());
        push(&flow_uri, "ad4m://flowName", lit(&self.name)?);
        if let Some(h) = self
            .interpretation_hint
            .as_deref()
            .filter(|s| !s.is_empty())
        {
            push(&flow_uri, "ad4m://interpretationHint", lit(h)?);
        }
        if !self.input_types.is_empty() {
            push(
                &flow_uri,
                "ad4m://inputTypes",
                json_lit(&json!(self.input_types)),
            );
        }
        if !self.output_types.is_empty() {
            push(
                &flow_uri,
                "ad4m://outputTypes",
                json_lit(&json!(self.output_types)),
            );
        }
        if let Some(h) = self.creation_hint.as_deref().filter(|s| !s.is_empty()) {
            push(&flow_uri, "ad4m://creationHint", lit(h)?);
        }
        if let Some(ctx) = self.context.as_ref().filter(|c| !c.is_empty()) {
            push(&flow_uri, "ad4m://context", json_lit(&json!(ctx)));
        }
        if let Some(rule) = &self.consensus_rule {
            push(
                &flow_uri,
                "ad4m://consensusRule",
                json_lit(&serde_json::to_value(rule)?),
            );
        }

        for state in &self.states {
            let state_uri = self.state_uri(&state.name);
            push(&flow_uri, "ad4m://hasState", state_uri.clone());
            push(&state_uri, "rdf://type", "ad4m://FlowState".to_string());
            push(&state_uri, "ad4m://stateName", lit(&state.name)?);
            push(
                &state_uri,
                "ad4m://stateValue",
                format!(
                    "literal:number:{}",
                    urlencoding::encode(&js_number_string(state.value))
                ),
            );
            if let Some(h) = state
                .interpretation_hint
                .as_deref()
                .filter(|s| !s.is_empty())
            {
                push(&state_uri, "ad4m://interpretationHint", lit(h)?);
            }
            if let Some(qs) = state.requires.as_ref().filter(|q| !q.is_empty()) {
                push(&state_uri, "ad4m://requires", json_lit(&json!(qs)));
            }
            if let Some(c) = state.semantic_check.as_deref().filter(|s| !s.is_empty()) {
                push(&state_uri, "ad4m://semanticCheck", lit(c)?);
            }
            if let Some(rule) = &state.consensus_rule {
                push(
                    &state_uri,
                    "ad4m://consensusRule",
                    json_lit(&serde_json::to_value(rule)?),
                );
            }
        }

        for transition in &self.transitions {
            let transition_uri = self.transition_uri(
                &transition.from_state,
                &transition.to_state,
                &transition.action_name,
            );
            push(&flow_uri, "ad4m://hasTransition", transition_uri.clone());
            push(
                &transition_uri,
                "rdf://type",
                "ad4m://FlowTransition".to_string(),
            );
            push(
                &transition_uri,
                "ad4m://actionName",
                lit(&transition.action_name)?,
            );
            push(
                &transition_uri,
                "ad4m://fromState",
                self.state_uri(&transition.from_state),
            );
            push(
                &transition_uri,
                "ad4m://toState",
                self.state_uri(&transition.to_state),
            );
            push(
                &transition_uri,
                "ad4m://transitionActions",
                json_lit(&json!(transition.actions)),
            );
        }
        Ok(links)
    }
}

/// JS `String(number)` — the spelling `Literal.from(number).toUrl()`
/// encodes on the wire: `NaN`, `±Infinity`, decimal inside [1e-6, 1e21)
/// (shortest roundtrip digits, which `Display` also produces), `e±`
/// outside it.
fn js_number_string(v: f64) -> String {
    if v.is_nan() {
        return "NaN".to_string();
    }
    if v.is_infinite() {
        return if v < 0.0 { "-Infinity" } else { "Infinity" }.to_string();
    }
    if v == 0.0 {
        return "0".to_string();
    }
    let a = v.abs();
    if (1e-6..1e21).contains(&a) {
        format!("{v}")
    } else {
        let s = format!("{v:e}");
        let (mantissa, exp) = s.split_once('e').expect("exponential format has e");
        let exp: i32 = exp.parse().expect("exponent is an integer");
        format!(
            "{mantissa}e{}{}",
            if exp >= 0 { "+" } else { "-" },
            exp.abs()
        )
    }
}

/// `SHACLFlow.FLOW_LEVEL_PREDICATES` — every predicate [`ShaclFlow::to_links`]
/// writes from the flow's own URI. That URI also carries links the
/// definition does not own (a receipt index, for one), so replacing a
/// definition removes only these from it.
const FLOW_LEVEL_PREDICATES: &[&str] = &[
    "rdf://type",
    "ad4m://flowName",
    "ad4m://interpretationHint",
    "ad4m://inputTypes",
    "ad4m://outputTypes",
    "ad4m://creationHint",
    "ad4m://context",
    "ad4m://consensusRule",
    "ad4m://hasState",
    "ad4m://hasTransition",
];

/// `SHACLFlow.STATE_AND_TRANSITION_PREDICATES` — every predicate
/// [`ShaclFlow::to_links`] writes from a state's or a transition's URI,
/// plus the retired `ad4m://stateCheck`, so a re-add still clears it from
/// an older definition.
const STATE_AND_TRANSITION_PREDICATES: &[&str] = &[
    "rdf://type",
    "ad4m://stateName",
    "ad4m://stateValue",
    "ad4m://interpretationHint",
    "ad4m://requires",
    "ad4m://semanticCheck",
    "ad4m://consensusRule",
    "ad4m://stateCheck",
    "ad4m://actionName",
    "ad4m://fromState",
    "ad4m://toState",
    "ad4m://transitionActions",
];

/// Decode a `literal:string:` (or a `literal:json:` string payload) target
/// into its text — TS `Literal.fromUrl(t).get()` where the result must be a
/// string.
fn decode_literal_string(target: &str) -> Option<String> {
    match Literal::from_url(target.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        LiteralValue::Json(Value::String(s)) => Some(s),
        _ => None,
    }
}

/// A `literal:string:` target carrying URL-encoded JSON — the writer format
/// `toLinks` uses for `inputTypes`, `requires`, `transitionActions`, …
fn decode_json_literal<T: serde::de::DeserializeOwned>(target: &str) -> Option<T> {
    let payload = target.strip_prefix("literal:string:")?;
    let decoded = urlencoding::decode(payload).ok()?;
    serde_json::from_str(&decoded).ok()
}

/// `string[]` out of a JSON literal — every element must be a non-empty
/// string, or the field stays unset, as TS `fromLinks` reads it.
fn decode_string_array(target: &str) -> Option<Vec<String>> {
    let strings: Vec<String> = decode_json_literal(target)?;
    (strings.iter().all(|s| !s.is_empty())).then_some(strings)
}

/// The stateValue grammar — shared verbatim with the executor's
/// `decode_state_value` and TS `decodeStateValue` (#1202): `literal:number:`,
/// percent-decoded, ASCII decimal or `inf`/`infinity` (any case); anything
/// else is NaN.
fn decode_state_value(target: &str) -> f64 {
    use regex::Regex;
    use std::sync::LazyLock;
    static DECIMAL: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"^[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$").expect("static")
    });
    static INFINITE: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"^[+-]?[iI][nN][fF](?:[iI][nN][iI][tT][yY])?$").expect("static")
    });
    let Some(payload) = target.strip_prefix("literal:number:") else {
        return f64::NAN;
    };
    let Ok(payload) = urlencoding::decode(payload) else {
        return f64::NAN;
    };
    if DECIMAL.is_match(&payload) {
        return payload.parse().unwrap_or(f64::NAN);
    }
    if INFINITE.is_match(&payload) {
        return if payload.starts_with('-') {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        };
    }
    f64::NAN
}

/// The flow's human-readable name from its canonical URI — the inverse of
/// `flow_uri()`: `${namespace}${name}Flow` → `name`. `None` for URIs that
/// do not carry the suffix (a stale record's flow, for one) — TS
/// `flowNameFromUri`.
pub fn flow_name_from_uri(flow_uri: &str) -> Option<String> {
    let without_suffix = flow_uri.strip_suffix("Flow")?;
    let sep = without_suffix.rfind(['/', '#']).map(|i| i + 1).unwrap_or(0);
    let name = &without_suffix[sep..];
    (!name.is_empty()).then(|| name.to_string())
}

// ── Flow RPC result types (the generated contract's shapes) ───────────────

/// One output an instance run produces — `{ className, id }` (`OutputRef`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowOutputRef {
    pub class_name: String,
    pub id: String,
}

/// One `perspective.flowStates` row: the state this agent's replica last
/// derived (`None` = minted here, not derived yet — it stands at its flow's
/// first state).
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowStateEntry {
    pub instance: String,
    pub current_state: Option<String>,
    pub derived_at: Option<String>,
}

/// One fired transition — `FireOutcome`.
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowFireOutcome {
    pub instance_uri: String,
    pub from_state: String,
    pub to_state: String,
    pub voters: Vec<String>,
    pub contributing_proposal_uris: Vec<String>,
}

/// What one `propose_flow_transition` call did — `ProposeOutcome`. Read it
/// like the TS `FlowProposeResult`: `outcomes` non-empty = fired; empty +
/// `recorded_vote` = queued; empty + not `recorded_vote` = a no-op revote;
/// `contested` = stalled.
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowProposeResult {
    pub proposal_uri: String,
    pub minted: bool,
    pub recorded_vote: bool,
    pub outcomes: Vec<FlowFireOutcome>,
    pub derived_state: String,
    pub contested: bool,
}

/// The three-way verdict of `perspective.verifyFlowReceipt` — never collapse
/// it to a bool (`"undecidable"` and `"rejected"` need different callers).
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct FlowReceiptVerdict {
    pub outcome: String,
    pub detail: String,
    #[serde(default)]
    pub terminal_state: Option<String>,
    #[serde(default)]
    pub outputs: Option<Vec<FlowOutputRef>>,
    #[serde(default)]
    pub voters: Option<Vec<String>>,
}

/// One instance a verified flow receipt speaks for — `ValidOutput`.
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowValidOutput {
    pub output: FlowOutputRef,
    pub content: String,
    pub terminal_state: String,
    pub receipt_uri: String,
}

/// What `perspective.mintFlowReceipt` wrote — the receipt's URI and body
/// (opaque; hand it back to `verify_flow_receipt`).
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlowMintedReceipt {
    pub receipt_uri: String,
    pub receipt: Value,
}

// ── FlowApi — the `perspective.*` flow RPCs + link-level flow reads ───────

/// The flow surface of a perspective: definition registration and reads
/// (`add_flow`, `get_flow`, `sdna_flows`, `available_flows`) and the
/// consensus RPCs (`flow_states`, `propose_flow_transition`, …). The Rust
/// analogue of `PerspectiveProxy`'s flow methods.
#[derive(Debug, Clone)]
pub struct FlowApi {
    ws: Arc<WsRpcClient>,
    uuid: String,
}

/// The hardwired `FlowInstance` class shape — identical bytes to the
/// executor's `hardwired_sdna/flow_instance.json`, so registering it writes
/// the shape the engine itself would (the TS/Rust parity tests lock these).
/// Bundled inside the crate: a published `ad4m-client` cannot reach the
/// executor's file, and `sdna_matches_the_executor`s test keeps the copies
/// in step where the repo is present.
const FLOW_INSTANCE_SDNA: &str = include_str!("sdna/flow_instance.json");
/// The hardwired `FlowTransitionProposal` class shape — bundled like
/// [`FLOW_INSTANCE_SDNA`].
const FLOW_TRANSITION_PROPOSAL_SDNA: &str = include_str!("sdna/flow_transition_proposal.json");

impl FlowApi {
    pub fn new(ws: &Arc<WsRpcClient>, uuid: impl Into<String>) -> Self {
        Self {
            ws: ws.clone(),
            uuid: uuid.into(),
        }
    }

    pub fn uuid(&self) -> &str {
        &self.uuid
    }

    /// `perspective.queryLinks` — one link read, the `get(LinkQuery)` the
    /// flow definition loads through.
    async fn query_links(
        &self,
        source: Option<&str>,
        target: Option<&str>,
        predicate: Option<&str>,
    ) -> Result<Vec<LinkExpression>> {
        let mut params = Map::new();
        params.insert("uuid".into(), Value::from(self.uuid.clone()));
        if let Some(v) = source {
            params.insert("source".into(), Value::from(v));
        }
        if let Some(v) = target {
            params.insert("target".into(), Value::from(v));
        }
        if let Some(v) = predicate {
            params.insert("predicate".into(), Value::from(v));
        }
        self.ws
            .call("perspective.queryLinks", Value::Object(params))
            .await
    }

    /// Register a flow definition — TS `PerspectiveProxy.addFlow`. The
    /// wanted links are [`ShaclFlow::to_links`] plus the registration pair
    /// (`ad4m://has_flow` off `ad4m://self`, `ad4m://flow_uri` under the
    /// name literal). Re-adding replaces: the stored links that are not
    /// wanted — read back off the flow URI, each state/transition URI
    /// (definition predicates only: those URIs carry foreign links too)
    /// and the name's registrations — go with the additions in one
    /// `perspective.linkMutations`. An unchanged re-add writes nothing.
    ///
    /// The runtime classes (`FlowInstance`, `FlowTransitionProposal`) are
    /// not registered here — TS leaves that to `FlowInstance.start`'s
    /// `registerAll`, which [`ensure_flow_classes`] mirrors.
    pub async fn add_flow(&self, name: &str, flow: &ShaclFlow) -> Result<()> {
        let name_literal = Literal::from_string(name.to_string()).to_url()?;
        let flow_uri = flow.flow_uri();
        let mut wanted = flow.to_links()?;
        wanted.push(Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://has_flow".to_string()),
            target: name_literal.clone(),
        });
        wanted.push(Link {
            source: name_literal.clone(),
            predicate: Some("ad4m://flow_uri".to_string()),
            target: flow_uri.clone(),
        });

        // What is stored is found through the flow's own links, since state
        // and transition URIs may use an older scheme. Only definition
        // predicates are taken, from the flow's URI and from each state and
        // transition URI: all of them can carry links the definition does
        // not own.
        let own_links: Vec<LinkExpression> = self
            .query_links(Some(&flow_uri), None, None)
            .await?
            .into_iter()
            .filter(|l| {
                l.data
                    .predicate
                    .as_deref()
                    .is_some_and(|p| FLOW_LEVEL_PREDICATES.contains(&p))
            })
            .collect();
        let children: Vec<String> = own_links
            .iter()
            .filter(|l| {
                matches!(
                    l.data.predicate.as_deref(),
                    Some("ad4m://hasState") | Some("ad4m://hasTransition")
                )
            })
            .map(|l| l.data.target.clone())
            .collect();
        let mut stored = own_links;
        for uri in &children {
            stored.extend(
                self.query_links(Some(uri), None, None)
                    .await?
                    .into_iter()
                    .filter(|l| {
                        l.data
                            .predicate
                            .as_deref()
                            .is_some_and(|p| STATE_AND_TRANSITION_PREDICATES.contains(&p))
                    }),
            );
        }
        // A name points at one flow: `get_flow` reads the first flow_uri
        // link, so every flow_uri link of this name is taken and one left
        // from an earlier namespace is removed rather than kept beside the
        // new one.
        stored.extend(
            self.query_links(
                Some("ad4m://self"),
                Some(&name_literal),
                Some("ad4m://has_flow"),
            )
            .await?,
        );
        stored.extend(
            self.query_links(Some(&name_literal), None, Some("ad4m://flow_uri"))
                .await?,
        );

        // Only the difference is written, so an unchanged definition writes
        // nothing and a changed one touches only what changed. The key is
        // the JSON tuple TS keys on.
        let key = |source: &str, predicate: Option<&str>, target: &str| -> String {
            serde_json::to_string(&json!([source, predicate.unwrap_or(""), target]))
                .expect("strings always serialize")
        };
        let wanted_keys: BTreeSet<String> = wanted
            .iter()
            .map(|l| key(&l.source, l.predicate.as_deref(), &l.target))
            .collect();
        let stored_keys: BTreeSet<String> = stored
            .iter()
            .map(|l| key(&l.data.source, l.data.predicate.as_deref(), &l.data.target))
            .collect();
        let mut first_seen = BTreeSet::new();
        let additions: Vec<&Link> = wanted
            .iter()
            .filter(|l| {
                let k = key(&l.source, l.predicate.as_deref(), &l.target);
                !stored_keys.contains(&k) && first_seen.insert(k)
            })
            .collect();
        let removals: Vec<&LinkExpression> = stored
            .iter()
            .filter(|l| {
                !wanted_keys.contains(&key(
                    &l.data.source,
                    l.data.predicate.as_deref(),
                    &l.data.target,
                ))
            })
            .collect();

        if additions.is_empty() && removals.is_empty() {
            return Ok(());
        }
        self.ws
            .call::<Value>(
                "perspective.linkMutations",
                json!({
                    "uuid": self.uuid,
                    "mutations": {
                        "additions": additions,
                        "removals": removals
                            .iter()
                            .map(|l| LinkExpressionInput::from((*l).clone()))
                            .collect::<Vec<_>>(),
                    },
                    "status": "shared",
                }),
            )
            .await?;
        Ok(())
    }

    /// `getFlow` — load a definition by name from the perspective's links
    /// (`ad4m://has_flow` names a `literal:` name; `ad4m://flow_uri` maps it
    /// to the flow URI; `ad4m://hasState`/`ad4m://hasTransition` to the
    /// children). `None` when no flow answers to `name`.
    pub async fn get_flow(&self, name: &str) -> Result<Option<ShaclFlow>> {
        let name_literal = Literal::from_string(name.to_string()).to_url()?;
        let flow_uri_links = self
            .query_links(Some(&name_literal), None, Some("ad4m://flow_uri"))
            .await?;
        let Some(flow_uri) = flow_uri_links.first().map(|l| l.data.target.clone()) else {
            return Ok(None);
        };
        let flow_links = self.query_links(Some(&flow_uri), None, None).await?;
        let mut all = flow_links.clone();
        for l in &flow_links {
            let p = l.data.predicate.as_deref();
            if p == Some("ad4m://hasState") || p == Some("ad4m://hasTransition") {
                all.extend(self.query_links(Some(&l.data.target), None, None).await?);
            }
        }
        Ok(Some(ShaclFlow::from_links(&all, &flow_uri)?))
    }

    /// The registered flow whose URI is `flow_uri`, found through the
    /// registration links (`<name literal> ad4m://flow_uri <flow_uri>`) —
    /// not by parsing a name out of the URI: the registered name need not
    /// be the definition's `name`, and a namespace such as `urn:example:`
    /// has no `/` or `#` to split on. `None` when no registered name
    /// currently resolves to this URI (unregistered, or the name now
    /// points at another flow).
    async fn registered_flow_by_uri(&self, flow_uri: &str) -> Result<Option<ShaclFlow>> {
        let registrations = self
            .query_links(None, Some(flow_uri), Some("ad4m://flow_uri"))
            .await?;
        for link in registrations {
            let Some(name) = decode_literal_string(&link.data.source) else {
                continue;
            };
            if let Some(flow) = self.get_flow(&name).await? {
                if flow.flow_uri() == flow_uri {
                    return Ok(Some(flow));
                }
            }
        }
        Ok(None)
    }

    /// Every flow name registered on the perspective — `sdnaFlows`: the
    /// `ad4m://has_flow` name literals off `ad4m://self`, decoded.
    pub async fn sdna_flows(&self) -> Result<Vec<String>> {
        let links = self
            .query_links(Some("ad4m://self"), None, Some("ad4m://has_flow"))
            .await?;
        Ok(links
            .iter()
            .filter_map(|l| decode_literal_string(&l.data.target))
            .collect())
    }

    /// `availableFlows` — the flow names `expr`'s classes qualify for: an
    /// empty `inputTypes` or one holding `"any"` matches anything; else at
    /// least one of the expression's subject-class URIs must appear.
    /// `subject_classes` is the caller-supplied list of the expression's
    /// registered class URIs (the Rust client has no `subjectClassesOf` —
    /// see NOTES).
    pub async fn available_flows(&self, expr_classes: &[String]) -> Result<Vec<String>> {
        let mut out = Vec::new();
        for name in self.sdna_flows().await? {
            let Some(flow) = self.get_flow(&name).await? else {
                continue;
            };
            if flow.input_types.is_empty()
                || flow.input_types.iter().any(|t| t == "any")
                || expr_classes.iter().any(|c| flow.input_types.contains(c))
            {
                out.push(name);
            }
        }
        Ok(out)
    }

    /// Register the hardwired `FlowInstance` + `FlowTransitionProposal`
    /// classes — `FlowInstance.start`'s write path (`registerAll`), a single
    /// batched `perspective.addSdna` the executor no-ops when present.
    async fn ensure_flow_classes(&self) -> Result<()> {
        self.ws
            .call::<Value>(
                "perspective.addSdna",
                json!({
                    "uuid": self.uuid,
                    "entries": [
                        {
                            "name": "FlowInstance",
                            "sdnaType": "subject_class",
                            "shaclJson": FLOW_INSTANCE_SDNA,
                        },
                        {
                            "name": "FlowTransitionProposal",
                            "sdnaType": "subject_class",
                            "shaclJson": FLOW_TRANSITION_PROPOSAL_SDNA,
                        },
                    ],
                }),
            )
            .await?;
        Ok(())
    }

    /// `perspective.flowStates` — this agent's derived state per instance.
    /// `None` asks for every instance the agent has derived.
    pub async fn flow_states(&self, instances: Option<&[String]>) -> Result<Vec<FlowStateEntry>> {
        self.ws
            .call(
                "perspective.flowStates",
                json!({ "uuid": self.uuid, "instances": instances }),
            )
            .await
    }

    /// `perspective.flowFired` — the proposals of `instance` this replica
    /// recorded as fired.
    pub async fn flow_fired(&self, instance: &str) -> Result<Vec<String>> {
        self.ws
            .call(
                "perspective.flowFired",
                json!({ "uuid": self.uuid, "instance": instance }),
            )
            .await
    }

    /// `perspective.proposeFlowTransition` — propose (or co-sign an
    /// equivalent open proposal of) an edge out of the instance's derived
    /// state.
    pub async fn propose_flow_transition(
        &self,
        instance_uri: &str,
        to_state: &str,
        rationale: Option<&str>,
        outputs: Option<&[FlowOutputRef]>,
    ) -> Result<FlowProposeResult> {
        self.ws
            .call(
                "perspective.proposeFlowTransition",
                json!({
                    "uuid": self.uuid,
                    "instanceUri": instance_uri,
                    "toState": to_state,
                    "rationale": rationale,
                    "outputs": outputs,
                }),
            )
            .await
    }

    /// `perspective.acceptFlowProposal` — count this agent's DID toward a
    /// proposal's quorum; fires the transition at it.
    pub async fn accept_flow_proposal(&self, proposal_uri: &str) -> Result<Vec<FlowFireOutcome>> {
        self.ws
            .call(
                "perspective.acceptFlowProposal",
                json!({ "uuid": self.uuid, "proposalUri": proposal_uri }),
            )
            .await
    }

    /// `perspective.rejectFlowProposal` — withdraw this agent's links from a
    /// proposal; resolves to how many were retracted.
    pub async fn reject_flow_proposal(&self, proposal_uri: &str) -> Result<u64> {
        let result: Value = self
            .ws
            .call(
                "perspective.rejectFlowProposal",
                json!({ "uuid": self.uuid, "proposalUri": proposal_uri }),
            )
            .await?;
        Ok(result
            .get("retractedLinks")
            .and_then(Value::as_u64)
            .unwrap_or(0))
    }

    /// `perspective.verifyFlowReceipt` — decide a receipt under this
    /// perspective's own flow catalogue.
    pub async fn verify_flow_receipt(&self, receipt: &Value) -> Result<FlowReceiptVerdict> {
        self.ws
            .call(
                "perspective.verifyFlowReceipt",
                json!({ "uuid": self.uuid, "receipt": receipt }),
            )
            .await
    }

    /// `perspective.flowValidOutputs` — the instances that stand as valid
    /// outputs of `flow` (optionally of runs settled into `state`).
    pub async fn flow_valid_outputs(
        &self,
        flow: &str,
        state: Option<&str>,
    ) -> Result<Vec<FlowValidOutput>> {
        self.ws
            .call(
                "perspective.flowValidOutputs",
                json!({ "uuid": self.uuid, "flow": flow, "state": state }),
            )
            .await
    }

    /// `perspective.mintFlowReceipt` — mint and store the receipt for a
    /// completed run.
    pub async fn mint_flow_receipt(&self, instance_uri: &str) -> Result<FlowMintedReceipt> {
        self.ws
            .call(
                "perspective.mintFlowReceipt",
                json!({ "uuid": self.uuid, "instanceUri": instance_uri }),
            )
            .await
    }
}

// ── FlowInstance — the TS class ───────────────────────────────────────────

/// The SDNA class of the on-graph record: `"FlowInstance"`.
const RECORD_CLASS: &str = "FlowInstance";
/// The SDNA class of proposals: `"FlowTransitionProposal"`.
const PROPOSAL_CLASS: &str = "FlowTransitionProposal";

/// The error texts the TS `findAll` treats as "no shape registered yet →
/// empty result", not as a failure: a perspective that never minted a flow
/// instance throws these for the `FlowInstance` model query.
fn is_missing_shape_error(message: &str) -> bool {
    let m = message.to_lowercase();
    m.contains("no shacl shape stored")
        || m.contains("shape not found")
        || m.contains("class not registered")
        || m.contains("not registered")
}

/// A running flow instance — the Rust `FlowInstance`. Wraps the on-graph
/// `FlowInstance` record (a JSON model row: `id`, `flowUri`, `subject`,
/// `createdAt`) and its parsed [`ShaclFlow`]; the state snapshot comes from
/// `perspective.flowStates` at load time.
#[derive(Debug, Clone)]
pub struct FlowInstance {
    api: FlowApi,
    /// The flow definition this instance was minted against.
    pub shape: ShaclFlow,
    /// The on-graph record — `id`, `flowUri`, `subject`, `createdAt`.
    pub record: Value,
    state_name: Option<String>,
}

impl FlowInstance {
    fn model(api: &FlowApi) -> Model {
        Model::new(&api.ws, api.uuid.clone(), RECORD_CLASS)
    }

    fn proposals_model(api: &FlowApi) -> Model {
        Model::new(&api.ws, api.uuid.clone(), PROPOSAL_CLASS)
    }

    /// `FlowInstance.start` — mint a new run of `flow_name` on `subject`,
    /// or return the one already running (earliest record, `createdAt` then
    /// `id`). Registers the hardwired runtime classes on first call; refuses
    /// an unknown flow or one with zero states, as TS does.
    pub async fn start(api: &FlowApi, flow_name: &str, subject: &str) -> Result<Self> {
        let flow = api
            .get_flow(flow_name)
            .await?
            .ok_or_else(|| anyhow!("Flow \"{flow_name}\" not found"))?;
        if flow.states.is_empty() {
            return Err(anyhow!(
                "Flow \"{flow_name}\" has no states — FlowInstance.start is for stateful flows only; zero-state flows fire via the atomic-action path"
            ));
        }
        api.ensure_flow_classes().await?;

        let model = Self::model(api);
        let mut existing = model
            .find_all(&ModelQuery::where_(json!({
                "flowUri": flow.flow_uri(),
                "subject": subject,
            })))
            .await?;
        if !existing.is_empty() {
            existing.sort_by(|a, b| {
                started_at(a)
                    .partial_cmp(&started_at(b))
                    .unwrap_or(std::cmp::Ordering::Equal)
                    .then_with(|| record_id(a).cmp(record_id(b)))
            });
            let record = existing.into_iter().next().unwrap();
            let states = api
                .flow_states(Some(&[record_id(&record).to_string()]))
                .await?;
            let state_name = state_of(&flow, &record, &states);
            return Ok(Self {
                api: api.clone(),
                shape: flow,
                record,
                state_name,
            });
        }

        let mut props = Map::new();
        props.insert("flowUri".into(), Value::from(flow.flow_uri()));
        props.insert("subject".into(), Value::from(subject));
        let created = model
            .create(props, crate::model::CreateOptions::default())
            .await?;
        // The hydrated record: the create reply carries the URI; fetch the
        // instance for its synthesized fields (`createdAt`).
        let record = model
            .get(&created.base_uri, None)
            .await?
            .unwrap_or_else(|| {
                json!({
                    "id": created.base_uri,
                    "flowUri": flow.flow_uri(),
                    "subject": subject,
                })
            });
        Ok(Self {
            api: api.clone(),
            state_name: Some(flow.states[0].name.clone()),
            shape: flow,
            record,
        })
    }

    /// `FlowInstance.findAll` — every live run on the perspective, narrowed
    /// by `flow_name` and/or `subject`. Read-only: a missing `FlowInstance`
    /// shape answers `[]` rather than registering classes. Unknown
    /// `flow_name` → `[]`; records whose flow no longer resolves are
    /// skipped.
    pub async fn find_all(api: &FlowApi, filter: Option<FindAllFilter<'_>>) -> Result<Vec<Self>> {
        let (flow_name, subject) = match filter {
            Some(FindAllFilter::FlowName(name)) => (Some(name), None),
            Some(FindAllFilter::By { flow_name, subject }) => (flow_name, subject),
            None => (None, None),
        };

        let mut where_ = Map::new();
        if let Some(flow_name) = flow_name {
            match api.get_flow(flow_name).await? {
                Some(flow) => {
                    where_.insert("flowUri".into(), Value::from(flow.flow_uri()));
                }
                None => return Ok(vec![]),
            }
        }
        if let Some(subject) = subject {
            where_.insert("subject".into(), Value::from(subject));
        }

        let model = Self::model(api);
        let records = match if where_.is_empty() {
            model.find_all(&ModelQuery::default()).await
        } else {
            model
                .find_all(&ModelQuery::where_(Value::Object(where_)))
                .await
        } {
            Ok(records) => records,
            Err(e) if is_missing_shape_error(&e.to_string()) => return Ok(vec![]),
            Err(e) => return Err(e),
        };

        // Pair each record with its parsed flow, one `get_flow` per URI.
        let mut shapes: BTreeMap<String, Option<ShaclFlow>> = BTreeMap::new();
        let mut paired: Vec<(Value, ShaclFlow)> = Vec::new();
        for record in records {
            let Some(flow_uri) = record.get("flowUri").and_then(Value::as_str) else {
                continue;
            };
            let shape = match shapes.get(flow_uri) {
                Some(s) => s.clone(),
                None => {
                    let s = api.registered_flow_by_uri(flow_uri).await?;
                    shapes.insert(flow_uri.to_string(), s.clone());
                    s
                }
            };
            // Stale record — the flow was unregistered, or a different flow
            // now owns the name.
            let Some(shape) = shape else {
                continue;
            };
            paired.push((record, shape));
        }

        // One batched `flowStates` for every record.
        let ids: Vec<String> = paired
            .iter()
            .map(|(r, _)| record_id(r).to_string())
            .collect();
        let entries = api.flow_states(Some(&ids)).await?;
        Ok(paired
            .into_iter()
            .map(|(record, shape)| Self {
                api: api.clone(),
                state_name: state_of(&shape, &record, &entries),
                shape,
                record,
            })
            .collect())
    }

    /// Escape hatch — build a wrapper from a `(record, shape)` pair the
    /// caller already owns (`current_state_name` defaults to "not derived").
    /// TS `FlowInstance.wrap`.
    pub fn wrap(
        api: &FlowApi,
        shape: ShaclFlow,
        record: Value,
        current_state_name: Option<String>,
    ) -> Self {
        Self {
            api: api.clone(),
            shape,
            record,
            state_name: current_state_name,
        }
    }

    // ── Read accessors (TS getters) ───────────────────────────────────────

    /// `uri` — the on-graph node's URI (`record.id`).
    pub fn uri(&self) -> &str {
        record_id(&self.record)
    }

    /// `subject` — the base expression this run is bound to.
    pub fn subject(&self) -> &str {
        self.record
            .get("subject")
            .and_then(Value::as_str)
            .unwrap_or("")
    }

    /// `flowName` — the display label, off the paired definition.
    pub fn flow_name(&self) -> &str {
        &self.shape.name
    }

    /// `flowUri` — the record's canonical flow identity.
    pub fn flow_uri(&self) -> &str {
        self.record
            .get("flowUri")
            .and_then(Value::as_str)
            .unwrap_or("")
    }

    /// `currentStateName` — the state this agent's executor last derived
    /// (at load time). `None` = not derived by this agent yet — show it as
    /// pending, not as the first state.
    pub fn current_state_name(&self) -> Option<&str> {
        self.state_name.as_deref()
    }

    /// `startedAtMillis` — the record's synthesized `createdAt`, or `None`.
    pub fn started_at_millis(&self) -> Option<f64> {
        self.record.get("createdAt").and_then(Value::as_f64)
    }

    /// `currentState` — the `FlowState` matching `current_state_name`.
    /// Errors when the name is not declared on the definition — a stale
    /// record whose flow was edited under it, as TS throws.
    pub fn current_state(&self) -> Result<Option<&FlowState>> {
        let Some(name) = self.current_state_name() else {
            return Ok(None);
        };
        self.shape
            .states
            .iter()
            .find(|s| s.name == name)
            .map(Some)
            .ok_or_else(|| {
                anyhow!(
                    "FlowInstance {}: currentState \"{name}\" is not declared on flow \"{}\". Known states: {}",
                    self.uri(),
                    self.flow_name(),
                    self.shape
                        .states
                        .iter()
                        .map(|s| s.name.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            })
    }

    /// `availableTransitions` — every transition whose `from_state` is the
    /// current state (empty on a terminal or underived state).
    pub fn available_transitions(&self) -> Vec<&FlowTransition> {
        let Some(name) = self.current_state_name() else {
            return vec![];
        };
        self.shape
            .transitions
            .iter()
            .filter(|t| t.from_state == name)
            .collect()
    }

    /// `proposals()` — every `FlowTransitionProposal` record targeting this
    /// instance's URI.
    pub async fn proposals(&self) -> Result<Vec<Value>> {
        Self::proposals_model(&self.api)
            .find_all(&ModelQuery::where_(json!({ "flowInstance": self.uri() })))
            .await
    }

    /// `proposeTransition` — propose the edge to `to_state` (or co-sign an
    /// equivalent open proposal) as this agent.
    pub async fn propose_transition(
        &self,
        to_state: &str,
        rationale: Option<&str>,
        outputs: Option<&[FlowOutputRef]>,
    ) -> Result<FlowProposeResult> {
        self.api
            .propose_flow_transition(self.uri(), to_state, rationale, outputs)
            .await
    }

    /// `acceptProposal` — vote this agent's DID on `proposal_uri`; fires the
    /// transition at quorum.
    pub async fn accept_proposal(&self, proposal_uri: &str) -> Result<Vec<FlowFireOutcome>> {
        self.api.accept_flow_proposal(proposal_uri).await
    }

    /// `rejectProposal` — withdraw this agent's links from `proposal_uri`;
    /// resolves to how many were retracted.
    pub async fn reject_proposal(&self, proposal_uri: &str) -> Result<u64> {
        self.api.reject_flow_proposal(proposal_uri).await
    }
}

/// The `findAll` filter — a bare flow-name string or the `{ flowName?,
/// subject? }` pair (TS accepts either shape).
#[derive(Debug, Clone, Copy)]
pub enum FindAllFilter<'a> {
    FlowName(&'a str),
    By {
        flow_name: Option<&'a str>,
        subject: Option<&'a str>,
    },
}

/// `record.id` — the on-graph URI every flow predicate references.
fn record_id(record: &Value) -> &str {
    record
        .get("id")
        .and_then(Value::as_str)
        .or_else(|| record.get("_baseExpression").and_then(Value::as_str))
        .unwrap_or("")
}

/// `record.createdAt` as a number; missing/unreadable sorts last — the
/// `Number.POSITIVE_INFINITY` of the TS `startedAt` key.
fn started_at(record: &Value) -> f64 {
    record
        .get("createdAt")
        .and_then(Value::as_f64)
        .unwrap_or(f64::INFINITY)
}

/// `FlowInstance.stateOf` — the recorded derivation for `record`; the flow's
/// first state for a minted-but-underived row (`currentState: null`); `None`
/// for one this agent has no bookkeeping for at all.
fn state_of(flow: &ShaclFlow, record: &Value, entries: &[FlowStateEntry]) -> Option<String> {
    let entry = entries.iter().find(|e| e.instance == record_id(record))?;
    entry
        .current_state
        .clone()
        .or_else(|| flow.states.first().map(|s| s.name.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn lit(target: &str, predicate: &str, literal: &str) -> LinkExpression {
        LinkExpression {
            author: "did:test".into(),
            timestamp: "2026-01-01T00:00:00Z".into(),
            status: None,
            data: crate::types::Link {
                source: target.to_string(),
                predicate: Some(predicate.to_string()),
                target: literal.to_string(),
            },
            proof: crate::types::ExpressionProof {
                invalid: None,
                key: None,
                signature: None,
                valid: None,
            },
        }
    }

    fn enc_str(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    fn enc_num(n: &str) -> String {
        format!("literal:number:{}", n)
    }

    /// The links `toLinks` writes for a small two-state one-edge flow.
    fn delivery_links() -> Vec<LinkExpression> {
        let flow_uri = "coasys://DeliveryFlow";
        let s0 = "coasys://Delivery.Identified";
        let s1 = "coasys://Delivery.Done";
        let t = "coasys://Delivery.transition/Identified/Done/Accept";
        vec![
            lit(flow_uri, "ad4m://flowName", &enc_str("Delivery")),
            lit(flow_uri, "ad4m://inputTypes", &enc_str("[\"Task\"]")),
            lit(flow_uri, "ad4m://hasState", s0),
            lit(s0, "ad4m://stateName", &enc_str("Identified")),
            lit(s0, "ad4m://stateValue", &enc_num("0")),
            lit(flow_uri, "ad4m://hasState", s1),
            lit(s1, "ad4m://stateName", &enc_str("Done")),
            lit(s1, "ad4m://stateValue", &enc_num("1")),
            lit(
                s1,
                "ad4m://requires",
                &enc_str("[{\"className\":\"Task\"}]"),
            ),
            lit(flow_uri, "ad4m://hasTransition", t),
            lit(t, "ad4m://actionName", &enc_str("Accept")),
            lit(t, "ad4m://fromState", s0),
            lit(t, "ad4m://toState", s1),
            lit(t, "ad4m://transitionActions", &enc_str("[]")),
        ]
    }

    #[test]
    fn from_links_rebuilds_the_definition() {
        let flow = ShaclFlow::from_links(&delivery_links(), "coasys://DeliveryFlow").unwrap();
        assert_eq!(flow.name, "Delivery");
        assert_eq!(flow.namespace, "coasys://");
        assert_eq!(flow.input_types, vec!["Task".to_string()]);
        assert_eq!(
            flow.states
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>(),
            vec!["Identified", "Done"]
        );
        assert_eq!(flow.transitions.len(), 1);
        let t = &flow.transitions[0];
        assert_eq!(t.action_name, "Accept");
        assert_eq!(t.from_state, "Identified");
        assert_eq!(t.to_state, "Done");
        assert_eq!(flow.states[1].requires.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn state_sort_orders_by_value_then_name_nan_last() {
        let mut flow = ShaclFlow::new("F", "ns://");
        flow.states = vec![
            FlowState {
                name: "c".into(),
                value: 2.0,
                ..Default::default()
            },
            FlowState {
                name: "b".into(),
                value: f64::NAN,
                ..Default::default()
            },
            FlowState {
                name: "z".into(),
                value: 1.0,
                ..Default::default()
            },
            FlowState {
                name: "a".into(),
                value: 1.0,
                ..Default::default()
            },
        ];
        flow.sort_states();
        assert_eq!(
            flow.states
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>(),
            vec!["a", "z", "c", "b"]
        );
    }

    #[test]
    fn decode_state_value_grammar() {
        assert_eq!(decode_state_value(&enc_num("1.5")), 1.5);
        assert_eq!(decode_state_value(&enc_num("-0")), -0.0);
        assert_eq!(decode_state_value(&enc_num("1e2")), 100.0);
        assert_eq!(decode_state_value(&enc_num("inf")), f64::INFINITY);
        assert_eq!(decode_state_value(&enc_num("-Infinity")), f64::NEG_INFINITY);
        assert!(decode_state_value(&enc_num("1abc")).is_nan());
        assert!(decode_state_value(&enc_num("NaN")).is_nan());
        assert!(decode_state_value("not-a-literal").is_nan());
        assert!(decode_state_value(&enc_str("3")).is_nan());
    }

    /// TS `addFlow` replaces a re-registered flow: the stored links that
    /// are not in the new definition go — through one
    /// `perspective.linkMutations` — including a stale `flow_uri` mapping
    /// left by an earlier namespace and a retired `stateCheck`, while
    /// non-definition links on the same URIs (an app's label, the
    /// addSdna-era `ad4m://sdna` link) stay.
    #[tokio::test]
    async fn re_adding_a_flow_replaces_the_stored_definition() {
        use crate::testkit::{FakeExecutor, LinkStore};

        let server = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(server.url(), "").await.unwrap());
        let api = FlowApi::new(&ws, "p1");
        let name_lit = Literal::from_string("F".into()).to_url().unwrap();

        let mut store = LinkStore::default();
        store.seed("ad4m://self", "ad4m://has_flow", &name_lit);
        store.seed(&name_lit, "ad4m://flow_uri", "old://FFlow");
        store.seed("old://FFlow", "rdf://type", "ad4m://Flow");
        store.seed(&name_lit, "ad4m://sdna", "literal:string:");
        store.seed("new://FFlow", "ad4m://hasState", "new://F.S1");
        store.seed("new://F.S1", "rdf://type", "ad4m://FlowState");
        store.seed("new://F.S1", "ad4m://stateName", &enc_str("S1"));
        store.seed("new://F.S1", "ad4m://stateValue", "literal:number:1");
        store.seed("new://F.S1", "ad4m://consensusRule", &enc_str("{\"n\":2}"));
        store.seed("new://F.S1", "ad4m://stateCheck", &enc_str("old"));
        store.seed("new://F.S1", "app://label", &enc_str("mine"));

        let mut flow = ShaclFlow::new("F", "new://");
        flow.states = vec![
            FlowState {
                name: "S1".into(),
                value: 1.0,
                ..Default::default()
            },
            FlowState {
                name: "S2".into(),
                value: 2.0,
                consensus_rule: Some(ConsensusRule {
                    n: 2,
                    from_role: None,
                }),
                ..Default::default()
            },
        ];
        flow.transitions = vec![FlowTransition {
            action_name: "advance".into(),
            from_state: "S1".into(),
            to_state: "S2".into(),
            actions: vec![],
        }];

        let mut add = tokio::spawn({
            let api = api.clone();
            async move { api.add_flow("F", &flow).await }
        });
        let mut saw_mutations = 0;
        let mut saw_add_sdna = false;
        loop {
            tokio::select! {
                req = server.next_request() => {
                    match req["type"].as_str() {
                        Some("perspective.linkMutations") => saw_mutations += 1,
                        Some("perspective.addSdna") => saw_add_sdna = true,
                        _ => {}
                    }
                    match store.handle(&req) {
                        Some(result) => server.reply(&req, result).await,
                        None => server.reply(&req, Value::Bool(true)).await,
                    }
                }
                r = &mut add => { r.unwrap().unwrap(); break; }
            }
        }

        assert_eq!(
            saw_mutations, 1,
            "re-registering must write the diff through perspective.linkMutations"
        );
        assert!(
            !saw_add_sdna,
            "TS addFlow writes links itself; addSdna is the subject-class path"
        );

        let triples: BTreeSet<(String, String, String)> = store.triples().into_iter().collect();
        // Stale definition predicates are removed …
        assert!(
            !triples.contains(&(
                "new://F.S1".into(),
                "ad4m://consensusRule".into(),
                enc_str("{\"n\":2}")
            )),
            "stale consensusRule must go: {triples:?}"
        );
        assert!(!triples.contains(&(
            "new://F.S1".into(),
            "ad4m://stateCheck".into(),
            enc_str("old")
        )));
        // … as is the stale name→URI mapping — but not the orphaned
        // definition it pointed at (TS removes the mapping, not the foreign
        // flow's own links).
        assert!(!triples.contains(&(
            name_lit.clone(),
            "ad4m://flow_uri".into(),
            "old://FFlow".into()
        )));
        assert!(triples.contains(&(
            "old://FFlow".into(),
            "rdf://type".into(),
            "ad4m://Flow".into()
        )));
        // Non-definition links on the same URIs stay.
        assert!(triples.contains(&("new://F.S1".into(), "app://label".into(), enc_str("mine"))));
        assert!(triples.contains(&(
            name_lit.clone(),
            "ad4m://sdna".into(),
            "literal:string:".into()
        )));
        // The new definition and registration are on the graph.
        assert!(triples.contains(&(
            name_lit.clone(),
            "ad4m://flow_uri".into(),
            "new://FFlow".into()
        )));
        assert!(triples.contains(&(
            "new://FFlow".into(),
            "ad4m://hasTransition".into(),
            "new://F.transition/S1/S2/advance".into()
        )));
        assert!(triples.contains(&(
            "new://F.S2".into(),
            "ad4m://consensusRule".into(),
            enc_str("{\"n\":2}")
        )));
        assert!(triples.contains(&(
            "ad4m://self".into(),
            "ad4m://has_flow".into(),
            name_lit.clone()
        )));
    }

    /// An unchanged re-add writes nothing — `addFlow` returns before the
    /// mutations call when the diff is empty.
    #[tokio::test]
    async fn re_adding_an_unchanged_flow_writes_nothing() {
        use crate::testkit::{FakeExecutor, LinkStore};

        let server = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(server.url(), "").await.unwrap());
        let api = FlowApi::new(&ws, "p1");
        let mut store = LinkStore::default();

        let mut flow = ShaclFlow::new("F", "new://");
        flow.states = vec![FlowState {
            name: "S1".into(),
            value: 1.0,
            ..Default::default()
        }];

        async fn round(
            api: &FlowApi,
            server: &FakeExecutor,
            store: &mut LinkStore,
            flow: &ShaclFlow,
        ) -> u64 {
            let mut add = tokio::spawn({
                let api = api.clone();
                let flow = flow.clone();
                async move { api.add_flow("F", &flow).await }
            });
            let mut mutations = 0;
            loop {
                tokio::select! {
                    req = server.next_request() => {
                        if req["type"] == "perspective.linkMutations" {
                            mutations += 1;
                        }
                        match store.handle(&req) {
                            Some(result) => server.reply(&req, result).await,
                            None => server.reply(&req, Value::Bool(true)).await,
                        }
                    }
                    r = &mut add => { r.unwrap().unwrap(); break; }
                }
            }
            mutations
        }

        assert_eq!(round(&api, &server, &mut store, &flow).await, 1);
        assert_eq!(round(&api, &server, &mut store, &flow).await, 0);
    }

    /// `find_all` resolves a record's `flowUri` through the registration
    /// links, not by parsing a name out of the URI: a flow registered as
    /// "Shipping" whose definition is named "Delivery", in a `urn:example:`
    /// namespace, is still found; a record of an unregistered flow is not.
    #[tokio::test]
    async fn find_all_resolves_runs_by_registered_flow_uri() {
        use crate::testkit::{FakeExecutor, LinkStore};

        let server = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(server.url(), "").await.unwrap());
        let api = FlowApi::new(&ws, "p1");
        let mut store = LinkStore::default();

        let mut flow = ShaclFlow::new("Delivery", "urn:example:");
        flow.states = vec![FlowState {
            name: "Open".into(),
            value: 0.0,
            ..Default::default()
        }];
        assert_eq!(flow.flow_uri(), "urn:example:DeliveryFlow");

        async fn serve<T>(
            server: &FakeExecutor,
            store: &mut LinkStore,
            task: &mut tokio::task::JoinHandle<T>,
        ) -> T {
            loop {
                tokio::select! {
                    req = server.next_request() => {
                        let result = match req["type"].as_str() {
                            Some("perspective.modelQuery") => json!(json!({
                                "instances": [
                                    {"id": "run://1", "flowUri": "urn:example:DeliveryFlow",
                                     "subject": "s://1"},
                                    {"id": "run://2", "flowUri": "urn:example:GoneFlow",
                                     "subject": "s://2"},
                                ],
                                "totalCount": 2,
                            }).to_string()),
                            Some("perspective.flowStates") => json!([]),
                            _ => store.handle(&req).unwrap_or(Value::Bool(true)),
                        };
                        server.reply(&req, result).await;
                    }
                    r = &mut *task => return r.unwrap(),
                }
            }
        }

        let mut add = tokio::spawn({
            let api = api.clone();
            let flow = flow.clone();
            async move { api.add_flow("Shipping", &flow).await }
        });
        serve(&server, &mut store, &mut add).await.unwrap();

        let mut find = tokio::spawn({
            let api = api.clone();
            async move { FlowInstance::find_all(&api, None).await }
        });
        let runs = serve(&server, &mut store, &mut find).await.unwrap();
        let ids: Vec<&str> = runs.iter().map(|r| record_id(&r.record)).collect();
        assert_eq!(ids, ["run://1"], "the Shipping run is found, the stale one skipped");
        assert_eq!(runs[0].shape.flow_uri(), "urn:example:DeliveryFlow");
    }

    #[test]
    fn flow_name_from_uri_roundtrip() {
        assert_eq!(
            flow_name_from_uri("coasys://DeliveryFlow"),
            Some("Delivery".to_string())
        );
        assert_eq!(
            flow_name_from_uri("ns://x/Y.Flow"),
            Some("Y.".to_string()) // ends with "Flow" — TS yields "Y." too
        );
        assert_eq!(flow_name_from_uri("plain"), None);
    }

    /// Fetched records come through the model's `hydrateFromLinks` decode
    /// (RFC3339 → epoch millis), so the earliest run is picked on time —
    /// not, as with both sorts at +∞, on whichever URI happens to sort
    /// first.
    #[test]
    fn the_earliest_run_wins_on_time_not_on_uri_order() {
        let mut records = vec![
            json!({"id": "a://run-late", "createdAt": "2026-01-02T00:00:00.000Z"}),
            json!({"id": "z://run-early", "createdAt": "2026-01-01T00:00:00.000Z"}),
        ];
        for r in &mut records {
            crate::model::decode_timestamps_in_instance(r);
        }
        records.sort_by(|a, b| {
            started_at(a)
                .partial_cmp(&started_at(b))
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| record_id(a).cmp(record_id(b)))
        });
        assert_eq!(record_id(&records[0]), "z://run-early");
        assert_eq!(started_at(&records[0]), 1767225600000.0);
    }

    #[test]
    fn state_of_prefers_recorded_then_first_state() {
        let mut flow = ShaclFlow::new("F", "ns://");
        flow.states = vec![
            FlowState {
                name: "s0".into(),
                ..Default::default()
            },
            FlowState {
                name: "s1".into(),
                ..Default::default()
            },
        ];
        let record = json!({"id": "ad4m://flow/instance/1"});
        // No entry → None.
        assert_eq!(state_of(&flow, &record, &[]), None);
        // Entry with null state → first state.
        let entries = vec![FlowStateEntry {
            instance: "ad4m://flow/instance/1".into(),
            current_state: None,
            derived_at: None,
        }];
        assert_eq!(state_of(&flow, &record, &entries), Some("s0".into()));
        // Entry with a state → it.
        let entries = vec![FlowStateEntry {
            instance: "ad4m://flow/instance/1".into(),
            current_state: Some("s1".into()),
            derived_at: None,
        }];
        assert_eq!(state_of(&flow, &record, &entries), Some("s1".into()));
    }

    #[test]
    fn missing_shape_messages_match_ts() {
        for msg in [
            "No SHACL shape stored for class 'FlowInstance'.",
            "Shape not found",
            "class not registered",
            "FlowInstance not registered",
        ] {
            assert!(is_missing_shape_error(msg), "{msg}");
        }
        assert!(!is_missing_shape_error("connection reset"));
    }

    /// The bundled hardwired class JSONs must stay byte-identical with the
    /// executor's — both sides register the same shape, so any drift in
    /// either direction changes what a perspective holds. Repo-only: in the
    /// packaged crate the executor's tree is absent and the check is out of
    /// place (the `include_str!`s above are what a publish compiles).
    #[test]
    fn bundled_sdna_matches_the_executors() {
        let executor_sdna = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../rust-executor/src/perspectives/hardwired_sdna");
        if !executor_sdna.is_dir() {
            eprintln!("SKIP: executor tree not present (packaged crate)");
            return;
        }
        for (bundled, executor_file) in [
            (FLOW_INSTANCE_SDNA, "flow_instance.json"),
            (
                FLOW_TRANSITION_PROPOSAL_SDNA,
                "flow_transition_proposal.json",
            ),
        ] {
            let theirs = std::fs::read_to_string(executor_sdna.join(executor_file)).unwrap();
            assert_eq!(
                bundled, theirs,
                "{executor_file}: bundled copy drifted from the executor's"
            );
        }
    }
}
