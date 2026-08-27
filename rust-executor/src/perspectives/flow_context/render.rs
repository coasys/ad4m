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
/// (loader) is responsible for loading those scalars off the graph.
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
// Tests — pure rendering, no perspective needed.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::{
        AD4MAction, FlowTransition, LinkPattern, ModelQueryCount,
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
