//! Slice 10.1a of the flow-implementation arc — the data shape and pure
//! rendering helpers that Model C's extraction prompt-builder (slice 10.2)
//! consumes when it composes an "Active flows on this scope" block.
//!
//! Design authority: `planning/flow-interpretation-hints-design.md` §5.3–§5.4.
//!
//! # What this module owns
//!
//! - [`FlowContext`] — one running `FlowInstance` summarized for the LLM.
//! - [`NextStateSummary`] — one reachable next-state, with `interpretationHint`
//!   and English-rendered `requires` payload.
//! - Pure helpers that turn a parsed [`SHACLFlow`] + current-state name into
//!   the above.
//!
//! # What slice 10.1b adds (this commit)
//!
//! - [`FlowInstanceRecord`] — the raw scalar row for one live
//!   `FlowInstance`, as it comes off the perspective graph.
//! - [`parse_flow_instance_from_hydrated`] — pure JSON→record parser
//!   (isolated for exhaustive testing without a live perspective).
//! - [`load_flow_instances`] — thin `PerspectiveInstance::model_query`
//!   wrapper that returns every active `FlowInstance` (optionally
//!   filtered to one `subject` base expression).
//! - [`build_flow_contexts`] — pairs each record with its parsed
//!   [`SHACLFlow`] and hands the pair to [`summarize_flow_instance`].
//!
//! # What slice 10.1c will add (NOT here)
//!
//! `parse_flow_from_links` — the Rust-side mirror of TS
//! `SHACLFlow.fromLinks`. Needed so [`build_flow_contexts`]' caller can
//! materialise a `flows_by_name: HashMap<String, SHACLFlow>` off the
//! perspective's SDNA links without a JS RPC round-trip. Deferred out
//! of this commit because the TS `fromLinks` is ~400 lines and porting
//! it deserves its own PR scope.
//!
//! # Why pure
//!
//! Rendering `ModelQuery` to English is the single hottest correctness
//! surface in the LLM prompt: an ambiguous or malformed sentence steers
//! the model wrong on every extraction pass. Isolating the rendering
//! from graph I/O makes it cheap to add fixture-driven tests as new
//! `PropertyCondition` variants land.

#![allow(dead_code)]

use crate::perspectives::flow_classes::FLOW_INSTANCE_CLASS;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{
    parse_flow_from_links, ConsensusRule, FlowState, ModelQuery, PropertyCondition, SHACLFlow,
};
use crate::types::{Link, LinkQuery};
use std::collections::{HashMap, HashSet};

/// One live `FlowInstance` summarized for the LLM prompt-builder.
///
/// Populated by slice 10.1b's `gather_active_flow_context` — this slice
/// only defines the shape and the pure helpers that build it.
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

/// Every state reachable from `current_state` via one transition, in
/// declaration order. Duplicates (same `to_state` reached by multiple
/// transitions) collapse to the first occurrence — the state summary is
/// the same regardless of which transition led there.
pub fn reachable_next_states<'a>(flow: &'a SHACLFlow, current_state: &str) -> Vec<&'a FlowState> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    for tr in &flow.transitions {
        if tr.from_state != current_state {
            continue;
        }
        if !seen.insert(tr.to_state.as_str()) {
            continue;
        }
        if let Some(s) = flow.states.iter().find(|s| s.name == tr.to_state) {
            out.push(s);
        }
    }
    out
}

/// Assemble a [`NextStateSummary`] from a `FlowState`. Pure.
pub fn summarize_next_state(state: &FlowState) -> NextStateSummary {
    NextStateSummary {
        name: state.name.clone(),
        interpretation_hint: state.interpretation_hint.clone(),
        requires_human_readable: render_requires_human_readable(state.requires.as_deref()),
        semantic_check: state.semantic_check.clone(),
        consensus_rule: state.consensus_rule.clone(),
    }
}

/// Assemble a [`FlowContext`] from a parsed flow + a live instance's
/// scalar fields (URI + subject + current_state). Pure — the caller
/// (slice 10.1b) is responsible for loading those scalars off the
/// graph.
pub fn summarize_flow_instance(
    flow: &SHACLFlow,
    instance_uri: impl Into<String>,
    subject: impl Into<String>,
    current_state: impl Into<String>,
) -> FlowContext {
    let current_state = current_state.into();
    let reachable_next_states = reachable_next_states(flow, &current_state)
        .into_iter()
        .map(summarize_next_state)
        .collect();
    FlowContext {
        flow_name: flow.name.clone(),
        instance_uri: instance_uri.into(),
        subject: subject.into(),
        current_state,
        flow_interpretation_hint: flow.interpretation_hint.clone(),
        reachable_next_states,
        consensus_rule: flow.consensus_rule.clone(),
    }
}

/// English rendering of a `FlowState.requires` payload. Empty string
/// when the payload is `None` or an empty slice — callers can short-
/// circuit their prompt inclusion on that.
///
/// The rendering is deliberately terse (one sentence per query) so the
/// composed prompt scales linearly with the number of active flows.
pub fn render_requires_human_readable(requires: Option<&[ModelQuery]>) -> String {
    let Some(qs) = requires else {
        return String::new();
    };
    if qs.is_empty() {
        return String::new();
    }
    let sentences: Vec<String> = qs.iter().map(render_model_query).collect();
    sentences.join(" AND ")
}

/// English rendering of a single `ModelQuery`.
///
/// Shape:
/// - `at least K matches of ClassName` (count.min)
/// - `at most K matches of ClassName` (count.max)
/// - `at least K, at most M matches of ClassName` (both bounds)
/// - `where FIELD OP VALUE, ...` appended when the query carries a
///   `where` clause — one clause per property, joined with commas
/// - `signed by the acting DID via <didProperty>` appended when
///   `didProperty` is set (role-gate marker for the LLM)
/// - `[either <sub1>, or <sub2>, ...]` when `or` is set — recurses
///
/// Values are stringified as-is (`serde_json::Value` → `to_string()`)
/// — the LLM does not need a strict typed representation and any
/// quoting the JSON encoder emits is unambiguous.
pub fn render_model_query(q: &ModelQuery) -> String {
    // Count clause — pluralize on n=1 vs n>1
    let noun = |n: u32| if n == 1 { "match" } else { "matches" };
    let count_clause = match q.count.as_ref() {
        None => "at least 1 match of".to_string(),
        Some(c) => match (c.min, c.max) {
            (Some(min), Some(max)) => {
                format!("at least {min}, at most {max} {} of", noun(max))
            }
            (Some(min), None) => format!("at least {min} {} of", noun(min)),
            (None, Some(max)) => format!("at most {max} {} of", noun(max)),
            (None, None) => "at least 1 match of".to_string(),
        },
    };
    let mut out = format!("{count_clause} {}", q.class_name);

    // Where clause
    if let Some(where_map) = q.r#where.as_ref() {
        if !where_map.is_empty() {
            let clauses: Vec<String> = where_map
                .iter()
                .map(|(field, cond)| format!("{field} {}", render_property_condition(cond)))
                .collect();
            out.push_str(" where ");
            out.push_str(&clauses.join(", "));
        }
    }

    // DID gate
    if let Some(did_prop) = q.did_property.as_ref() {
        out.push_str(&format!(" (signed by the acting DID via {did_prop})"));
    }

    // OR composition — recurse
    if let Some(alts) = q.or.as_ref() {
        if !alts.is_empty() {
            let sub_sentences: Vec<String> = alts.iter().map(render_model_query).collect();
            out.push_str(" OR [");
            out.push_str(&sub_sentences.join(" | "));
            out.push(']');
        }
    }

    out
}

/// English rendering of a single `PropertyCondition`. The scalar
/// shorthands compile to `"= <value>"` — matches the flow-parser's
/// runtime semantics.
fn render_property_condition(cond: &PropertyCondition) -> String {
    match cond {
        PropertyCondition::Str(s) => format!("= \"{s}\""),
        PropertyCondition::Num(n) => format!("= {n}"),
        PropertyCondition::Bool(b) => format!("= {b}"),
        PropertyCondition::Equals { equals } => format!("= {}", value_to_prompt_str(equals)),
        PropertyCondition::In { one_of } => {
            let items: Vec<String> = one_of.iter().map(value_to_prompt_str).collect();
            format!("in [{}]", items.join(", "))
        }
        PropertyCondition::Exists { exists } => {
            if *exists {
                "is set".to_string()
            } else {
                "is unset".to_string()
            }
        }
        PropertyCondition::Matches { matches } => format!("matches /{matches}/"),
    }
}

/// Compact stringification of a JSON value for prompt insertion —
/// strings unquoted (so `= "Bob"` doesn't turn into `= "\"Bob\""`),
/// everything else via `serde_json`.
fn value_to_prompt_str(v: &serde_json::Value) -> String {
    match v {
        serde_json::Value::String(s) => format!("\"{s}\""),
        _ => v.to_string(),
    }
}

/// English rendering of a consensus rule: `"1 signer"` or
/// `"3 signers from role: <role sentence>"`. Used both flow-level and
/// state-level.
pub fn render_consensus_rule(rule: &ConsensusRule) -> String {
    let plural = if rule.n == 1 { "signer" } else { "signers" };
    match rule.from_role.as_ref() {
        None => format!("{} {plural}", rule.n),
        Some(role) => format!(
            "{} {plural} from role: {}",
            rule.n,
            render_model_query(role)
        ),
    }
}

// ============================================================================
// Slice 10.1b — perspective-side FlowInstance loading + record→context pairing
// ============================================================================

/// One live `FlowInstance` as read off the perspective graph — the raw
/// scalar row that pairs with a parsed [`SHACLFlow`] to produce a
/// [`FlowContext`].
///
/// Kept flat (no reference to the parsed flow definition) so the
/// perspective read can be independent of the SDNA-flow catalogue read.
/// The two are joined by [`build_flow_contexts`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowInstanceRecord {
    /// The flow-name discriminator — matches `SHACLFlow.name` and is
    /// the identity property of the `FlowInstance` @Model class.
    pub flow_name: String,
    /// Instance URI — `ad4m://flow/instance/{id}` (see
    /// [`super::flow_classes::flow_instance_uri`]).
    pub instance_uri: String,
    /// Base expression this instance is bound to. Named `subject` on
    /// the `FlowInstance` class to avoid the Ad4mModel synthetic-field
    /// collision that broke `baseExpression` in the reserved-field
    /// rename fix (commit `e6362e5ca`).
    pub subject: String,
    /// Current state name (matches a `FlowState.name` on the flow).
    pub current_state: String,
    /// ISO-8601 timestamp the instance was minted at. `None` when the
    /// scalar wasn't set on-graph — rare but not fatal; the extraction
    /// pass renders "start time unknown" rather than skipping the record.
    pub started_at: Option<String>,
}

/// Parse one hydrated `FlowInstance` JSON object (as returned by
/// [`PerspectiveInstance::model_query`]) into a [`FlowInstanceRecord`].
///
/// Returns `None` when any of `id` / `flow` / `subject` / `currentState`
/// is missing — an untyped or half-written FlowInstance is silently
/// skipped rather than failing the whole extraction pass. The typical
/// cause is a mid-mint crash between constructor and setter writes; the
/// half-record shows up in the next model_query result and we don't
/// want that to poison every future extraction until it's hand-cleaned.
pub fn parse_flow_instance_from_hydrated(v: &serde_json::Value) -> Option<FlowInstanceRecord> {
    let instance_uri = v.get("id").and_then(|x| x.as_str())?.to_string();
    let flow_name = v.get("flow").and_then(|x| x.as_str())?.to_string();
    let subject = v.get("subject").and_then(|x| x.as_str())?.to_string();
    let current_state = v.get("currentState").and_then(|x| x.as_str())?.to_string();
    let started_at = v
        .get("startedAt")
        .and_then(|x| x.as_str())
        .map(str::to_string);
    Some(FlowInstanceRecord {
        flow_name,
        instance_uri,
        subject,
        current_state,
        started_at,
    })
}

/// Load every live `FlowInstance` on the perspective. When `subject` is
/// `Some(uri)`, filters to instances tied to that base expression;
/// otherwise returns every instance on the perspective.
///
/// Silently returns `Ok(vec![])` when the `FlowInstance` class hasn't
/// been registered yet on this perspective — a freshly-created
/// perspective simply has no live flows, and treating that as a
/// perspective-wide error would poison the extraction pass on every
/// call before the first flow is ever spawned.
pub async fn load_flow_instances(
    perspective: &PerspectiveInstance,
    subject: Option<&str>,
) -> anyhow::Result<Vec<FlowInstanceRecord>> {
    let query = match subject {
        None => serde_json::json!({}),
        Some(uri) => serde_json::json!({ "where": { "subject": uri } }),
    };
    let json = match perspective
        .model_query(FLOW_INSTANCE_CLASS, &query.to_string())
        .await
    {
        Ok(j) => j,
        Err(e) => {
            // Absent-class case: no FlowInstances have ever been minted
            // on this perspective, so the SHACL shape isn't registered
            // yet. That's a valid steady state — return empty rather
            // than propagating an error that would break Model C's
            // extraction pass on every call.
            let msg = format!("{e:#}");
            if msg.contains("Shape not found") || msg.contains("shape not found") {
                return Ok(vec![]);
            }
            return Err(anyhow::anyhow!(
                "load_flow_instances: model_query failed: {msg}"
            ));
        }
    };
    let parsed: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("load_flow_instances: response not JSON: {e:#}"))?;
    let instances = parsed
        .get("instances")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    Ok(instances
        .iter()
        .filter_map(parse_flow_instance_from_hydrated)
        .collect())
}

/// Pair each active [`FlowInstanceRecord`] with its parsed
/// [`SHACLFlow`] definition and produce [`FlowContext`]s for prompt
/// insertion.
///
/// Records whose `flow_name` is absent from `flows_by_name` are
/// silently skipped — a stale FlowInstance (its flow was
/// unregistered) shouldn't fail the whole extraction pass. Order is
/// preserved from `records`.
pub fn build_flow_contexts(
    records: &[FlowInstanceRecord],
    flows_by_name: &HashMap<String, SHACLFlow>,
) -> Vec<FlowContext> {
    records
        .iter()
        .filter_map(|r| {
            let flow = flows_by_name.get(&r.flow_name)?;
            Some(summarize_flow_instance(
                flow,
                r.instance_uri.clone(),
                r.subject.clone(),
                r.current_state.clone(),
            ))
        })
        .collect()
}

/// Discover every `SHACLFlow` definition present in a flat bag of links
/// and return them keyed by [`SHACLFlow::name`].
///
/// Flow discovery is anchored on `?flow_uri rdf://type ad4m://Flow`. For
/// each such source, the loader gathers:
///
/// - all links whose `source == flow_uri` (the flow-level metadata), and
/// - all links whose `source` matches a `hasState` or `hasTransition`
///   child target of that flow (the per-state / per-transition rows).
///
/// The gathered slice is then handed to
/// [`parse_flow_from_links`]. Flows whose parse fails are skipped with a
/// warning rather than failing the whole load — a single malformed flow
/// definition on the perspective shouldn't blind the extraction pass to
/// every other flow.
///
/// This is the pure, in-memory half; [`load_shacl_flows`] is the
/// [`PerspectiveInstance`] wrapper that fetches the link set with
/// targeted queries and delegates here.
pub fn parse_flows_from_bag(links: &[Link]) -> HashMap<String, SHACLFlow> {
    let mut flows = HashMap::new();

    let flow_uris: Vec<String> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some("rdf://type") && l.target == "ad4m://Flow")
        .map(|l| l.source.clone())
        .filter(|s| !s.is_empty())
        .collect();

    for flow_uri in flow_uris {
        let child_uris: HashSet<String> = links
            .iter()
            .filter(|l| {
                l.source == flow_uri
                    && matches!(
                        l.predicate.as_deref(),
                        Some("ad4m://hasState") | Some("ad4m://hasTransition")
                    )
            })
            .map(|l| l.target.clone())
            .collect();

        let related: Vec<Link> = links
            .iter()
            .filter(|l| l.source == flow_uri || child_uris.contains(&l.source))
            .cloned()
            .collect();

        match parse_flow_from_links(&related, &flow_uri) {
            Ok(flow) => {
                flows.insert(flow.name.clone(), flow);
            }
            Err(e) => {
                log::warn!("parse_flows_from_bag: skipping flow {flow_uri} (parse failed): {e:#}");
            }
        }
    }

    flows
}

/// Load every `SHACLFlow` definition off a live perspective and return
/// them keyed by name — the flow-side companion to
/// [`load_flow_instances`].
///
/// Uses targeted [`PerspectiveInstance::get_links`] calls (rather than a
/// full-perspective scan) to keep the hot-path cost proportional to
/// `F * (S+T)`, where `F` is the flow count and `S+T` is the average
/// state + transition count per flow. On a perspective with millions of
/// links this is what keeps the auto-processor's per-batch flow load
/// bounded.
///
/// Query plan:
///
/// 1. `(predicate = rdf://type, target = ad4m://Flow)` — enumerate every
///    live `flow_uri` on the perspective.
/// 2. Per `flow_uri`: `(source = flow_uri)` — pull the flow-level
///    metadata + `hasState`/`hasTransition` edges.
/// 3. Per state/transition child URI: `(source = child_uri)` — pull the
///    per-state / per-transition rows.
///
/// Then delegates to [`parse_flows_from_bag`] for the actual
/// [`SHACLFlow`] reconstruction, so the parse-side behaviour is
/// exercised by the module's fixture-driven tests without needing a
/// live perspective.
///
/// Errors from the outer type-index query propagate; per-flow child
/// query failures are logged and skipped so one broken flow can't blind
/// the extraction pass to every other flow on the perspective.
pub async fn load_shacl_flows(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<HashMap<String, SHACLFlow>> {
    let type_query = LinkQuery {
        source: None,
        predicate: Some("rdf://type".to_string()),
        target: Some("ad4m://Flow".to_string()),
        from_date: None,
        until_date: None,
        limit: None,
    };
    let type_links = perspective
        .get_links(&type_query)
        .await
        .map_err(|e| anyhow::anyhow!("load_shacl_flows: get_links(type) failed: {e:#}"))?;

    let mut bag: Vec<Link> = Vec::with_capacity(type_links.len());
    for tl in &type_links {
        bag.push(tl.data.clone());
    }

    for tl in type_links {
        let flow_uri = tl.data.source;
        if flow_uri.is_empty() {
            continue;
        }
        let flow_query = LinkQuery {
            source: Some(flow_uri.clone()),
            predicate: None,
            target: None,
            from_date: None,
            until_date: None,
            limit: None,
        };
        let flow_links = match perspective.get_links(&flow_query).await {
            Ok(l) => l,
            Err(e) => {
                log::warn!(
                    "load_shacl_flows: get_links(source={flow_uri}) failed, skipping: {e:#}"
                );
                continue;
            }
        };
        let child_uris: Vec<String> = flow_links
            .iter()
            .filter(|d| {
                matches!(
                    d.data.predicate.as_deref(),
                    Some("ad4m://hasState") | Some("ad4m://hasTransition")
                )
            })
            .map(|d| d.data.target.clone())
            .collect();
        bag.extend(flow_links.into_iter().map(|d| d.data));

        for child in child_uris {
            let child_query = LinkQuery {
                source: Some(child),
                predicate: None,
                target: None,
                from_date: None,
                until_date: None,
                limit: None,
            };
            match perspective.get_links(&child_query).await {
                Ok(child_links) => bag.extend(child_links.into_iter().map(|d| d.data)),
                Err(e) => log::warn!(
                    "load_shacl_flows: get_links(child of {flow_uri}) failed, skipping child: {e:#}"
                ),
            }
        }
    }

    Ok(parse_flows_from_bag(&bag))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::{
        parse_flow_to_links, AD4MAction, FlowTransition, LinkPattern, ModelQueryCount,
    };
    use std::collections::BTreeMap;

    // ------------- fixture builders -------------

    fn empty_link_pattern() -> LinkPattern {
        LinkPattern {
            source: None,
            predicate: String::new(),
            target: String::new(),
        }
    }

    fn state_named(name: &str) -> FlowState {
        FlowState {
            name: name.to_string(),
            value: 0.0,
            state_check: empty_link_pattern(),
            interpretation_hint: None,
            requires: None,
            semantic_check: None,
            consensus_rule: None,
        }
    }

    fn transition(from: &str, to: &str) -> FlowTransition {
        FlowTransition {
            action_name: format!("{from}->{to}"),
            from_state: from.to_string(),
            to_state: to.to_string(),
            actions: Vec::<AD4MAction>::new(),
        }
    }

    fn delivery_flow() -> SHACLFlow {
        SHACLFlow {
            name: "Delivery".to_string(),
            namespace: "ad4m://".to_string(),
            start_action: vec![],
            states: vec![
                state_named("identified"),
                state_named("scoped"),
                state_named("in_progress"),
                state_named("review"),
                state_named("done"),
            ],
            transitions: vec![
                transition("identified", "scoped"),
                transition("scoped", "in_progress"),
                transition("in_progress", "review"),
                transition("in_progress", "identified"), // regression path
                transition("review", "done"),
                transition("review", "in_progress"), // rework path
            ],
            interpretation_hint: Some(
                "A team-scale unit of work moving from identification to done.".to_string(),
            ),
            input_types: vec!["ad4m://Task".to_string()],
            output_types: vec![],
            creation_hint: None,
            context: None,
            consensus_rule: Some(ConsensusRule {
                n: 1,
                from_role: None,
            }),
        }
    }

    // ------------- reachable_next_states -------------

    #[test]
    fn reachable_next_states_returns_immediate_successors_only() {
        let flow = delivery_flow();
        let names: Vec<&str> = reachable_next_states(&flow, "identified")
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, vec!["scoped"]);
    }

    #[test]
    fn reachable_next_states_preserves_transition_order_and_dedups() {
        let flow = delivery_flow();
        // `in_progress` has two forward transitions: review + identified.
        // Order matches `transitions` declaration order.
        let names: Vec<&str> = reachable_next_states(&flow, "in_progress")
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, vec!["review", "identified"]);
    }

    #[test]
    fn reachable_next_states_skips_missing_target_state() {
        let mut flow = delivery_flow();
        flow.transitions
            .push(transition("done", "ghost_state_never_declared"));
        let names: Vec<&str> = reachable_next_states(&flow, "done")
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert!(
            names.is_empty(),
            "unknown target must be skipped, not panic"
        );
    }

    #[test]
    fn reachable_next_states_terminal_state_returns_empty() {
        let flow = delivery_flow();
        assert!(reachable_next_states(&flow, "done").is_empty());
    }

    // ------------- render_model_query -------------

    #[test]
    fn render_model_query_default_count_is_at_least_one() {
        let q = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            ..Default::default()
        };
        assert_eq!(render_model_query(&q), "at least 1 match of ad4m://Task");
    }

    #[test]
    fn render_model_query_count_pluralizes_on_n_equals_one() {
        // Regression: min=1 previously rendered "at least 1 matches" (bad
        // grammar), which is subtly LLM-corrosive — the model treats an
        // ungrammatical guard as noise and downweights it.
        let q = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            count: Some(ModelQueryCount {
                min: Some(1),
                max: None,
            }),
            ..Default::default()
        };
        assert_eq!(render_model_query(&q), "at least 1 match of ad4m://Task");

        let q_max_one = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            count: Some(ModelQueryCount {
                min: None,
                max: Some(1),
            }),
            ..Default::default()
        };
        assert_eq!(
            render_model_query(&q_max_one),
            "at most 1 match of ad4m://Task"
        );
    }

    #[test]
    fn render_model_query_count_variants() {
        let q_min = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            count: Some(ModelQueryCount {
                min: Some(3),
                max: None,
            }),
            ..Default::default()
        };
        assert_eq!(
            render_model_query(&q_min),
            "at least 3 matches of ad4m://Task"
        );

        let q_max = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            count: Some(ModelQueryCount {
                min: None,
                max: Some(2),
            }),
            ..Default::default()
        };
        assert_eq!(
            render_model_query(&q_max),
            "at most 2 matches of ad4m://Task"
        );

        let q_both = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            count: Some(ModelQueryCount {
                min: Some(1),
                max: Some(3),
            }),
            ..Default::default()
        };
        assert_eq!(
            render_model_query(&q_both),
            "at least 1, at most 3 matches of ad4m://Task"
        );
    }

    #[test]
    fn render_model_query_where_scalars_and_object_forms() {
        let mut where_map: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        where_map.insert(
            "state".to_string(),
            PropertyCondition::Str("done".to_string()),
        );
        where_map.insert("count".to_string(), PropertyCondition::Num(2.0));
        where_map.insert("archived".to_string(), PropertyCondition::Bool(false));
        where_map.insert(
            "priority".to_string(),
            PropertyCondition::In {
                one_of: vec![serde_json::json!("high"), serde_json::json!("critical")],
            },
        );
        where_map.insert(
            "assignee".to_string(),
            PropertyCondition::Exists { exists: true },
        );
        let q = ModelQuery {
            class_name: "ad4m://Task".to_string(),
            r#where: Some(where_map),
            ..Default::default()
        };
        let out = render_model_query(&q);
        // BTreeMap iteration order is alphabetical.
        assert!(
            out.contains(
                "at least 1 match of ad4m://Task where archived = false, assignee is set, \
                 count = 2, priority in [\"high\", \"critical\"], state = \"done\""
            ),
            "unexpected rendering: {out}",
        );
    }

    #[test]
    fn render_model_query_did_gate_appended() {
        let q = ModelQuery {
            class_name: "ad4m://Reviewer".to_string(),
            did_property: Some("did".to_string()),
            ..Default::default()
        };
        assert_eq!(
            render_model_query(&q),
            "at least 1 match of ad4m://Reviewer (signed by the acting DID via did)"
        );
    }

    #[test]
    fn render_model_query_or_recurses_and_composes() {
        let alt1 = ModelQuery {
            class_name: "ad4m://Owner".to_string(),
            did_property: Some("did".to_string()),
            ..Default::default()
        };
        let alt2 = ModelQuery {
            class_name: "ad4m://Reviewer".to_string(),
            did_property: Some("did".to_string()),
            ..Default::default()
        };
        let q = ModelQuery {
            class_name: "ad4m://Approver".to_string(),
            or: Some(vec![alt1, alt2]),
            ..Default::default()
        };
        let out = render_model_query(&q);
        assert!(
            out.starts_with("at least 1 match of ad4m://Approver"),
            "outer query rendered first: {out}",
        );
        assert!(
            out.contains(
                "OR [at least 1 match of ad4m://Owner (signed by the acting DID via did) | \
                 at least 1 match of ad4m://Reviewer (signed by the acting DID via did)]"
            ),
            "OR block rendered: {out}",
        );
    }

    // ------------- render_requires_human_readable -------------

    #[test]
    fn render_requires_none_is_empty_string() {
        assert_eq!(render_requires_human_readable(None), "");
    }

    #[test]
    fn render_requires_empty_slice_is_empty_string() {
        assert_eq!(render_requires_human_readable(Some(&[])), "");
    }

    #[test]
    fn render_requires_joins_with_and() {
        let qs = vec![
            ModelQuery {
                class_name: "ad4m://Perspective".to_string(),
                count: Some(ModelQueryCount {
                    min: Some(2),
                    max: None,
                }),
                ..Default::default()
            },
            ModelQuery {
                class_name: "ad4m://Tension".to_string(),
                ..Default::default()
            },
        ];
        let out = render_requires_human_readable(Some(&qs));
        assert_eq!(
            out,
            "at least 2 matches of ad4m://Perspective AND at least 1 match of ad4m://Tension"
        );
    }

    // ------------- render_consensus_rule -------------

    #[test]
    fn render_consensus_rule_solo_actor_pluralizes_correctly() {
        let rule = ConsensusRule {
            n: 1,
            from_role: None,
        };
        assert_eq!(render_consensus_rule(&rule), "1 signer");
    }

    #[test]
    fn render_consensus_rule_multi_signer_no_role() {
        let rule = ConsensusRule {
            n: 3,
            from_role: None,
        };
        assert_eq!(render_consensus_rule(&rule), "3 signers");
    }

    #[test]
    fn render_consensus_rule_with_role() {
        let rule = ConsensusRule {
            n: 2,
            from_role: Some(ModelQuery {
                class_name: "ad4m://Reviewer".to_string(),
                did_property: Some("did".to_string()),
                ..Default::default()
            }),
        };
        assert_eq!(
            render_consensus_rule(&rule),
            "2 signers from role: at least 1 match of ad4m://Reviewer (signed by the acting DID via did)"
        );
    }

    // ------------- summarize_next_state -------------

    #[test]
    fn summarize_next_state_carries_state_hints_and_renders_requires() {
        let mut s = state_named("scoped");
        s.interpretation_hint = Some("Scope has been agreed by all owners.".to_string());
        s.requires = Some(vec![ModelQuery {
            class_name: "ad4m://ScopeAgreement".to_string(),
            count: Some(ModelQueryCount {
                min: Some(1),
                max: None,
            }),
            ..Default::default()
        }]);
        s.semantic_check = Some("Does the scope match what was actually agreed?".to_string());

        let sum = summarize_next_state(&s);
        assert_eq!(sum.name, "scoped");
        assert_eq!(
            sum.interpretation_hint.as_deref(),
            Some("Scope has been agreed by all owners.")
        );
        assert_eq!(
            sum.requires_human_readable,
            "at least 1 match of ad4m://ScopeAgreement"
        );
        assert_eq!(
            sum.semantic_check.as_deref(),
            Some("Does the scope match what was actually agreed?")
        );
    }

    // ------------- parse_flow_instance_from_hydrated (slice 10.1b) -------------

    #[test]
    fn parse_flow_instance_happy_path() {
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/inst-1",
            "baseExpression": "ad4m://flow/instance/inst-1",
            "flow": "Delivery",
            "subject": "ad4m://task/foo",
            "currentState": "scoped",
            "startedAt": "2026-08-26T09:00:00Z",
            "author": "did:key:z6Mk…",
            "timestamp": "2026-08-26T09:00:00.001Z"
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert_eq!(r.instance_uri, "ad4m://flow/instance/inst-1");
        assert_eq!(r.flow_name, "Delivery");
        assert_eq!(r.subject, "ad4m://task/foo");
        assert_eq!(r.current_state, "scoped");
        assert_eq!(r.started_at.as_deref(), Some("2026-08-26T09:00:00Z"));
    }

    #[test]
    fn parse_flow_instance_extra_fields_ignored() {
        // The Ad4mModel hydration path emits synthetic fields (createdAt,
        // updatedAt, author, timestamp) alongside the class properties. The
        // parser must ignore anything it doesn't need — accepting a superset
        // of keys is how the reader stays forward-compatible with new
        // properties (see the reserved-field fix in `e6362e5ca`).
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/inst-x",
            "flow": "Deliberation",
            "subject": "ad4m://proposal/bar",
            "currentState": "perspectives",
            "startedAt": "2026-08-26T10:00:00Z",
            "createdAt": 1787751652750_u64,
            "updatedAt": 1787751652751_u64,
            "author": "did:key:zzzz",
            "some_future_property_the_llm_added": "hello",
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert_eq!(r.flow_name, "Deliberation");
        assert_eq!(r.current_state, "perspectives");
    }

    #[test]
    fn parse_flow_instance_missing_required_returns_none() {
        // Missing `flow` → mid-mint crash between constructor and setter
        // writes. Skip the record rather than pollute Vec<FlowContext>
        // with a half-typed instance.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/half",
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        assert!(parse_flow_instance_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_instance_missing_started_at_still_parses() {
        // startedAt is optional — the record still parses; the extraction
        // pass renders "start time unknown" rather than skipping.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/no-ts",
            "flow": "Delivery",
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert!(r.started_at.is_none());
    }

    #[test]
    fn parse_flow_instance_non_string_required_returns_none() {
        // A scalar that's on-graph as a non-string (e.g. flow name mistakenly
        // stored as a number) is treated the same as missing — one half-typed
        // record must not block extraction of the well-typed rest.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/bad",
            "flow": 42, // wrong type
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        assert!(parse_flow_instance_from_hydrated(&v).is_none());
    }

    // ------------- build_flow_contexts (slice 10.1b) -------------

    fn record(flow: &str, uri: &str, subject: &str, state: &str) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_name: flow.to_string(),
            instance_uri: uri.to_string(),
            subject: subject.to_string(),
            current_state: state.to_string(),
            started_at: Some("2026-08-26T09:00:00Z".to_string()),
        }
    }

    #[test]
    fn build_flow_contexts_pairs_records_with_flows() {
        let flow = delivery_flow();
        let mut catalogue = HashMap::new();
        catalogue.insert("Delivery".to_string(), flow);
        let records = vec![record(
            "Delivery",
            "ad4m://flow/instance/inst-1",
            "ad4m://task/foo",
            "in_progress",
        )];
        let ctxs = build_flow_contexts(&records, &catalogue);
        assert_eq!(ctxs.len(), 1);
        assert_eq!(ctxs[0].flow_name, "Delivery");
        assert_eq!(ctxs[0].current_state, "in_progress");
        // Wiring through summarize_flow_instance — reachable states from
        // `in_progress` on the delivery flow are `review` + `identified`.
        let names: Vec<&str> = ctxs[0]
            .reachable_next_states
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, vec!["review", "identified"]);
    }

    #[test]
    fn build_flow_contexts_skips_records_with_unknown_flow() {
        // A stale FlowInstance whose flow was unregistered must not fail
        // extraction — silently skip and keep processing the rest.
        let records = vec![
            record("Ghost", "ad4m://flow/instance/g", "ad4m://x", "s0"),
            record(
                "Delivery",
                "ad4m://flow/instance/d",
                "ad4m://task/y",
                "identified",
            ),
        ];

        // Empty catalogue → all skipped, empty result (not error).
        let empty: HashMap<String, SHACLFlow> = HashMap::new();
        assert!(build_flow_contexts(&records, &empty).is_empty());

        // Only Delivery known → Ghost skipped, Delivery kept.
        let mut catalogue_with_delivery: HashMap<String, SHACLFlow> = HashMap::new();
        catalogue_with_delivery.insert("Delivery".to_string(), delivery_flow());
        let ctxs = build_flow_contexts(&records, &catalogue_with_delivery);
        assert_eq!(ctxs.len(), 1);
        assert_eq!(ctxs[0].flow_name, "Delivery");
    }

    #[test]
    fn build_flow_contexts_preserves_record_order() {
        // Order matters — the prompt-builder inserts flows in the order
        // the caller supplies. A HashMap-based iteration would be
        // non-deterministic; we iterate `records`.
        let mut catalogue = HashMap::new();
        catalogue.insert("Delivery".to_string(), delivery_flow());
        let records = vec![
            record(
                "Delivery",
                "ad4m://flow/instance/a",
                "ad4m://x",
                "identified",
            ),
            record("Delivery", "ad4m://flow/instance/b", "ad4m://y", "scoped"),
            record("Delivery", "ad4m://flow/instance/c", "ad4m://z", "review"),
        ];
        let ctxs = build_flow_contexts(&records, &catalogue);
        let uris: Vec<&str> = ctxs.iter().map(|c| c.instance_uri.as_str()).collect();
        assert_eq!(
            uris,
            vec![
                "ad4m://flow/instance/a",
                "ad4m://flow/instance/b",
                "ad4m://flow/instance/c",
            ]
        );
    }

    // ------------- parse_flows_from_bag -------------

    fn mk_link(source: &str, predicate: &str, target: &str) -> Link {
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
    }

    fn ready_done_flow_links(name: &str, namespace: &str) -> Vec<Link> {
        let flow_json = format!(
            r#"{{
                "name": "{name}",
                "namespace": "{namespace}",
                "start_action": [],
                "states": [
                    {{"name": "ready", "value": 0.0, "state_check": {{"predicate": "{namespace}state", "target": "{namespace}ready"}}}},
                    {{"name": "done",  "value": 1.0, "state_check": {{"predicate": "{namespace}state", "target": "{namespace}done"}}}}
                ],
                "transitions": [
                    {{"action_name": "Complete", "from_state": "ready", "to_state": "done", "actions": []}}
                ]
            }}"#
        );
        parse_flow_to_links(&flow_json, name).expect("writer builds v4 flow links")
    }

    #[test]
    fn parse_flows_from_bag_empty_returns_empty() {
        let flows = parse_flows_from_bag(&[]);
        assert!(flows.is_empty());
    }

    #[test]
    fn parse_flows_from_bag_no_flow_type_links_returns_empty() {
        // Bag with random unrelated links but no `rdf://type → ad4m://Flow`.
        let bag = vec![
            mk_link("ad4m://task/foo", "rdf://type", "ad4m://Task"),
            mk_link(
                "ad4m://task/foo",
                "ad4m://title",
                "literal:string:Ship%20it",
            ),
        ];
        let flows = parse_flows_from_bag(&bag);
        assert!(flows.is_empty());
    }

    #[test]
    fn parse_flows_from_bag_discovers_single_flow_via_writer_output() {
        let bag = ready_done_flow_links("TODO", "todo://");
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1, "expected exactly one flow");
        let flow = flows.get("TODO").expect("flow keyed by name");
        assert_eq!(flow.namespace, "todo://");
        assert_eq!(flow.states.len(), 2);
        assert_eq!(flow.transitions.len(), 1);
        assert_eq!(flow.transitions[0].action_name, "Complete");
        assert_eq!(flow.transitions[0].from_state, "ready");
        assert_eq!(flow.transitions[0].to_state, "done");
    }

    #[test]
    fn parse_flows_from_bag_discovers_multiple_flows_side_by_side() {
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.extend(ready_done_flow_links("Approval", "gov://"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 2);
        assert!(flows.contains_key("TODO"));
        assert!(flows.contains_key("Approval"));
        // Namespaces stay isolated — cross-contamination would mean the
        // reader crossed hasState/hasTransition child gathering between
        // sibling flows.
        assert_eq!(flows["TODO"].namespace, "todo://");
        assert_eq!(flows["Approval"].namespace, "gov://");
        assert_eq!(flows["TODO"].states.len(), 2);
        assert_eq!(flows["Approval"].states.len(), 2);
    }

    #[test]
    fn parse_flows_from_bag_ignores_unrelated_noise_links() {
        // Real flow + unrelated task/proposal noise. The noise MUST NOT
        // widen the flow's link set, and MUST NOT stop the flow from
        // being discovered.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.extend(vec![
            mk_link("ad4m://task/foo", "rdf://type", "ad4m://Task"),
            mk_link(
                "ad4m://task/foo",
                "ad4m://title",
                "literal:string:Something",
            ),
            mk_link("ad4m://proposal/bar", "rdf://type", "ad4m://Proposal"),
        ]);
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1);
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_skips_flow_uri_without_flow_suffix() {
        // Manufactured type-index row whose source doesn't end in
        // "Flow" — parse_flow_from_links returns Err, loader must skip
        // it rather than propagate.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.push(mk_link("malformed://Broken", "rdf://type", "ad4m://Flow"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1, "well-formed flow still discovered");
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_ignores_type_link_with_empty_source() {
        // A `rdf://type → ad4m://Flow` link with an empty source has no
        // flow URI to hang gathering off — must not be enumerated.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.push(mk_link("", "rdf://type", "ad4m://Flow"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1);
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_child_uri_collision_across_flows_stays_isolated() {
        // Two flows that both happen to reference a state named
        // "shared" — but the state URIs are namespaced by flow name, so
        // gathering must not cross the boundary.
        let mut bag = ready_done_flow_links("Alpha", "ex://");
        bag.extend(ready_done_flow_links("Beta", "ex://"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 2);
        // Each flow's state count must remain 2 — a leak would push
        // one flow's states into the other.
        for name in ["Alpha", "Beta"] {
            let f = flows.get(name).expect("flow present");
            assert_eq!(f.states.len(), 2, "flow {name} states leaked from sibling");
        }
    }

    #[test]
    fn parse_flows_from_bag_key_is_shaclflow_name_not_uri() {
        // The HashMap MUST key on `SHACLFlow.name` (matches
        // FlowInstance.flow discriminator + build_flow_contexts lookup).
        let bag = ready_done_flow_links("TODO", "todo://");
        let flows = parse_flows_from_bag(&bag);
        assert!(
            !flows.contains_key("todo://TODOFlow"),
            "keyed on URI, not name"
        );
        assert!(flows.contains_key("TODO"), "must key on flow.name");
    }

    // ------------- summarize_flow_instance -------------

    #[test]
    fn summarize_flow_instance_end_to_end_wiring() {
        let flow = delivery_flow();
        let ctx = summarize_flow_instance(
            &flow,
            "ad4m://flow/instance/inst-1",
            "ad4m://task/foo",
            "in_progress",
        );
        assert_eq!(ctx.flow_name, "Delivery");
        assert_eq!(ctx.instance_uri, "ad4m://flow/instance/inst-1");
        assert_eq!(ctx.subject, "ad4m://task/foo");
        assert_eq!(ctx.current_state, "in_progress");
        assert_eq!(
            ctx.flow_interpretation_hint.as_deref(),
            Some("A team-scale unit of work moving from identification to done.")
        );
        assert!(ctx.consensus_rule.is_some());
        // `in_progress` reaches `review` and `identified` (rework paths),
        // in transition order, deduped.
        let names: Vec<&str> = ctx
            .reachable_next_states
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, vec!["review", "identified"]);
    }
}
