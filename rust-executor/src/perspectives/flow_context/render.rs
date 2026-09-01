//! Pure prompt-builder helpers. `ModelQuery` → English sentence,
//! `FlowState` → [`NextStateSummary`], `SHACLFlow` + scalars →
//! [`FlowContext`]. No perspective I/O — every input is a value.
//!
//! # Why pure
//!
//! Rendering `ModelQuery` to English is the single hottest correctness
//! surface in the LLM prompt: an ambiguous or malformed sentence steers
//! the model wrong on every extraction pass. Isolating the rendering
//! from graph I/O makes it cheap to add fixture-driven tests as new
//! `PropertyCondition` variants land.

use super::types::{FlowContext, NextStateSummary};
use crate::perspectives::shacl_parser::{
    ConsensusRule, FlowState, ModelQuery, PropertyCondition, SHACLFlow,
};

/// Substitution context for flow tokens in `ModelQuery` values. Flow
/// definitions embed [`FLOW_BASE_TOKEN`] (the item the flow is anchored
/// to) and [`FLOW_INSTANCE_TOKEN`] (the specific `FlowInstance` URI) in
/// `where` clauses. Without substitution the LLM sees them verbatim,
/// e.g. `where forTask = "$flow.base"` — no signal to the extractor.
///
/// `subject` and `instance_uri` are typically taken from the enclosing
/// [`FlowContext`]. `FlowTokens::none()` skips both substitutions;
/// callers that render outside a flow instance context (unit tests,
/// generic prompt scaffolding) use it to keep behaviour byte-identical
/// to pre-substitution renderings.
#[derive(Clone, Copy, Debug, Default)]
pub struct FlowTokens<'a> {
    /// Base subject URI the flow instance is anchored to. `$flow.base`
    /// substitutes to this.
    pub subject: &'a str,
    /// URI of the `FlowInstanceRecord` node on the graph. `$flow.instance`
    /// substitutes to this.
    pub instance_uri: &'a str,
}

/// The `$flow.base` prompt token — resolves to the flow instance's
/// `subject` (the item the flow is anchored to, e.g. a Task URI).
pub const FLOW_BASE_TOKEN: &str = "$flow.base";

/// The `$flow.instance` prompt token — resolves to the flow instance's
/// on-graph URI.
pub const FLOW_INSTANCE_TOKEN: &str = "$flow.instance";

impl<'a> FlowTokens<'a> {
    /// No-op token context — every substitution passes strings through
    /// unchanged. Use in unit tests and any render path that isn't
    /// scoped to a live flow instance.
    pub const fn none() -> FlowTokens<'static> {
        FlowTokens {
            subject: "",
            instance_uri: "",
        }
    }

    /// Substitute every occurrence of the two supported tokens in `s`.
    /// An empty field is treated as "not set" and its token is left in
    /// place — makes [`Self::none`] a true no-op even when one of the
    /// two fields is meaningful.
    pub fn substitute(&self, s: &str) -> String {
        let mut out = s.to_string();
        if !self.subject.is_empty() {
            out = out.replace(FLOW_BASE_TOKEN, self.subject);
        }
        if !self.instance_uri.is_empty() {
            out = out.replace(FLOW_INSTANCE_TOKEN, self.instance_uri);
        }
        out
    }

    /// Build tokens from a [`FlowContext`]. Convenience for the prompt
    /// assembler in `interpretation::prompt`.
    pub fn from_context(fc: &'a FlowContext) -> FlowTokens<'a> {
        FlowTokens {
            subject: &fc.subject,
            instance_uri: &fc.instance_uri,
        }
    }
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

/// Assemble a [`NextStateSummary`] from a `FlowState`. Pure. `requires`
/// renderings preserve their `$flow.base` / `$flow.instance` /
/// `$did` tokens verbatim — the prompt-builder pairs them with a
/// per-flow `tokens` legend so the LLM can look up what each token
/// resolves to (tokens in, tokens out; engine substitutes post-LLM).
/// The post-LLM engine calls [`FlowTokens::substitute`] on any tokenised
/// value the LLM emits back before it hits a query or a link.
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
/// (loader) is responsible for loading those scalars off the graph.
///
/// `requires` renderings on each reachable-next-state summary preserve
/// the tokens (`$flow.base`, `$flow.instance`, `$did`). The prompt-builder
/// pairs them with a per-flow legend telling the LLM what each token
/// resolves to for THIS instance. Substitution happens only on the
/// post-LLM engine side, not at render time.
pub fn summarize_flow_instance(
    flow: &SHACLFlow,
    instance_uri: impl Into<String>,
    subject: impl Into<String>,
    current_state: impl Into<String>,
) -> FlowContext {
    let current_state = current_state.into();
    let instance_uri = instance_uri.into();
    let subject = subject.into();
    let reachable_next_states = reachable_next_states(flow, &current_state)
        .into_iter()
        .map(summarize_next_state)
        .collect();
    FlowContext {
        flow_name: flow.name.clone(),
        instance_uri,
        subject,
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
///
/// Tokens (`$flow.base`, `$flow.instance`, `$did`) are preserved as
/// written — the prompt-builder pairs them with a per-flow `tokens`
/// legend, and the post-LLM engine substitutes them before running any
/// query (Nico 2026-08-31: tokens in, tokens out, engine substitutes).
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
/// String values in `where` clauses are emitted verbatim, so template
/// tokens (`$flow.base` / `$flow.instance` / `$did`) survive to the
/// prompt and get resolved via the per-flow `tokens` legend.
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

    // OR composition — recurse (tokens preserved through)
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
/// runtime semantics. Template tokens embedded in string values are
/// preserved verbatim.
fn render_property_condition(cond: &PropertyCondition) -> String {
    match cond {
        PropertyCondition::Str(s) => format!("= \"{}\"", s),
        PropertyCondition::Num(n) => format!("= {n}"),
        PropertyCondition::Bool(b) => format!("= {b}"),
        PropertyCondition::Equals { equals } => {
            format!("= {}", value_to_prompt_str(equals))
        }
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
/// everything else via `serde_json`. Template tokens inside string
/// values pass through untouched.
fn value_to_prompt_str(v: &serde_json::Value) -> String {
    match v {
        serde_json::Value::String(s) => format!("\"{}\"", s),
        _ => v.to_string(),
    }
}

/// English rendering of a consensus rule: `"1 signer"` or
/// `"3 signers from role: <role sentence>"`. Used both flow-level and
/// state-level. Tokens inside the role-gate query are preserved.
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
// Tests — pure rendering, no perspective needed.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::{AD4MAction, FlowTransition, ModelQueryCount};
    use std::collections::BTreeMap;

    // ------------- fixture builders -------------

    fn state_named(name: &str) -> FlowState {
        FlowState {
            name: name.to_string(),
            value: 0.0,
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

    // ------------- FlowTokens substitution -------------

    #[test]
    fn flow_tokens_none_is_noop() {
        let none = FlowTokens::none();
        assert_eq!(none.substitute("$flow.base"), "$flow.base");
        assert_eq!(none.substitute("$flow.instance"), "$flow.instance");
        assert_eq!(none.substitute("plain"), "plain");
    }

    #[test]
    fn flow_tokens_substitute_replaces_both_tokens() {
        let tokens = FlowTokens {
            subject: "ad4m://task/foo",
            instance_uri: "ad4m://flow/inst-42",
        };
        assert_eq!(tokens.substitute("$flow.base"), "ad4m://task/foo");
        assert_eq!(tokens.substitute("$flow.instance"), "ad4m://flow/inst-42");
        assert_eq!(
            tokens.substitute("under $flow.base scoped to $flow.instance"),
            "under ad4m://task/foo scoped to ad4m://flow/inst-42"
        );
    }

    #[test]
    fn flow_tokens_empty_field_leaves_own_token_intact() {
        // Regression: substituting an unset field with "" would collapse
        // the token into nothing and mask the fact that it was never
        // wired up. Instead, an empty field is treated as "not set".
        let subject_only = FlowTokens {
            subject: "ad4m://task/foo",
            instance_uri: "",
        };
        assert_eq!(subject_only.substitute("$flow.base"), "ad4m://task/foo");
        assert_eq!(
            subject_only.substitute("$flow.instance"),
            "$flow.instance",
            "instance_uri unset must leave the token verbatim, not empty-substitute"
        );
    }

    #[test]
    fn render_model_query_preserves_flow_tokens_in_where_string_value() {
        // Tokens in, tokens out. `$flow.base` and `$flow.instance` survive
        // rendering verbatim so the prompt-builder can carry the concrete
        // URIs in a separate legend and the engine substitutes post-LLM.
        let mut where_map: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        where_map.insert(
            "forTask".to_string(),
            PropertyCondition::Str(FLOW_BASE_TOKEN.to_string()),
        );
        where_map.insert(
            "onInstance".to_string(),
            PropertyCondition::Str(FLOW_INSTANCE_TOKEN.to_string()),
        );
        let q = ModelQuery {
            class_name: "coasys://Scope".to_string(),
            r#where: Some(where_map),
            ..Default::default()
        };
        let out = render_model_query(&q);
        assert!(
            out.contains("forTask = \"$flow.base\""),
            "$flow.base preserved verbatim: {out}",
        );
        assert!(
            out.contains("onInstance = \"$flow.instance\""),
            "$flow.instance preserved verbatim: {out}",
        );
    }

    #[test]
    fn render_model_query_preserves_flow_tokens_in_equals_and_in_variants() {
        // Object-form `equals` + `in` value lists must also preserve
        // tokens without substituting them.
        let mut where_map: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        where_map.insert(
            "parentBase".to_string(),
            PropertyCondition::Equals {
                equals: serde_json::json!("$flow.base"),
            },
        );
        where_map.insert(
            "anchor".to_string(),
            PropertyCondition::In {
                one_of: vec![
                    serde_json::json!("$flow.base"),
                    serde_json::json!("literal-value"),
                ],
            },
        );
        let q = ModelQuery {
            class_name: "coasys://Scope".to_string(),
            r#where: Some(where_map),
            ..Default::default()
        };
        let out = render_model_query(&q);
        assert!(
            out.contains("parentBase = \"$flow.base\""),
            "equals-form token preserved: {out}",
        );
        assert!(
            out.contains("anchor in [\"$flow.base\", \"literal-value\"]"),
            "in-list token preserved per-element: {out}",
        );
    }

    #[test]
    fn render_model_query_preserves_flow_tokens_recursively_through_or() {
        // The `or` branch recurses into `render_model_query` — tokens
        // inside `or` alternatives must survive the same way.
        let alt = ModelQuery {
            class_name: "coasys://Scope".to_string(),
            r#where: Some({
                let mut m: BTreeMap<String, PropertyCondition> = BTreeMap::new();
                m.insert(
                    "forTask".to_string(),
                    PropertyCondition::Str(FLOW_BASE_TOKEN.to_string()),
                );
                m
            }),
            ..Default::default()
        };
        let q = ModelQuery {
            class_name: "coasys://Approver".to_string(),
            or: Some(vec![alt]),
            ..Default::default()
        };
        let out = render_model_query(&q);
        assert!(
            out.contains("forTask = \"$flow.base\""),
            "OR branch preserves token: {out}",
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

    #[test]
    fn summarize_flow_instance_preserves_tokens_in_reachable_states_requires() {
        // `requires` renderings on reachable next-states preserve
        // `$flow.base` / `$flow.instance` verbatim. The prompt-builder
        // pairs each flow entry with a per-instance `tokens` legend so
        // the LLM can resolve the symbolic reference; the post-LLM engine
        // substitutes on the way out.
        let mut flow = delivery_flow();
        let mut where_map: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        where_map.insert(
            "forTask".to_string(),
            PropertyCondition::Str(FLOW_BASE_TOKEN.to_string()),
        );
        where_map.insert(
            "onInstance".to_string(),
            PropertyCondition::Str(FLOW_INSTANCE_TOKEN.to_string()),
        );
        flow.states[1].requires = Some(vec![ModelQuery {
            class_name: "coasys://Scope".to_string(),
            r#where: Some(where_map),
            ..Default::default()
        }]);
        let ctx = summarize_flow_instance(
            &flow,
            "ad4m://flow/instance/inst-42",
            "ad4m://task/foo",
            "identified",
        );
        let scoped = ctx
            .reachable_next_states
            .iter()
            .find(|s| s.name == "scoped")
            .expect("scoped is reachable from identified");
        assert!(
            scoped
                .requires_human_readable
                .contains("forTask = \"$flow.base\""),
            "$flow.base preserved verbatim: {}",
            scoped.requires_human_readable,
        );
        assert!(
            scoped
                .requires_human_readable
                .contains("onInstance = \"$flow.instance\""),
            "$flow.instance preserved verbatim: {}",
            scoped.requires_human_readable,
        );
    }
}
