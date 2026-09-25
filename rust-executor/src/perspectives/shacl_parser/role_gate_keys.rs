//! The keys a role gate may carry (#1144).
//!
//! A `consensusRule` literal decodes through serde, and serde drops a key it
//! does not know. On a role gate that fails open: a stale or misspelt key is
//! dropped and the gate is read without it, wider than the author wrote it
//! (`grantedByFlow`, renamed to `producedByFlow` in #1076, leaves a plain
//! `fromRole`), or gone altogether (`formRole`). A *missing* required field
//! already fails closed: the literal does not decode, the scope reads as
//! [`Malformed`](super::ConsensusRuleSlot::Malformed), and the edge is
//! refused (#1078). [`decode_consensus_rule`](super::decode_consensus_rule)
//! gives an unknown key the same answer, by running
//! [`role_gate_key_errors`] on the literal's JSON before decoding it.
//!
//! Checked: every object a role gate can carry, which is the rule, the
//! `fromRole` query and each of its `or` arms, `count`, `producedByFlow`, an
//! object `linkedTo`, and each object-valued `where` condition. Not checked:
//! the keys of `where` itself, which are property names, and the value of
//! `equals`, which is passed to `model_query` as it is (its `WhereOps` refuses
//! unknown operator keys itself).
//!
//! Only the rule is strict. `requires` guards and `context` use the same
//! `ModelQuery` type and keep serde's lenient decode, so a flow definition
//! with an extra key there still loads. Which way a guard should fail is a
//! separate question.

use crate::perspectives::flow_evaluator::LINKED_TO_KEYS;
use serde_json::{Map, Value};

const RULE_KEYS: &[&str] = &["n", "fromRole"];
const QUERY_KEYS: &[&str] = &[
    "className",
    "where",
    "count",
    "linkedTo",
    "didProperty",
    "or",
    "producedByFlow",
];
const COUNT_KEYS: &[&str] = &["min", "max"];
const PRODUCED_BY_FLOW_KEYS: &[&str] = &["flow", "state"];
/// The object forms of `PropertyCondition`, one key each.
const CONDITION_KEYS: &[&str] = &["equals", "in", "exists", "matches"];

/// Keys that were renamed, with where they went. Only a hint in the error;
/// the old key is refused like any other unknown key.
const RENAMED: &[(&str, &str)] = &[("grantedByFlow", "renamed to `producedByFlow` in #1076")];

/// Every key in a `consensusRule` literal that the reader would drop, as one
/// message each naming the key's path (`fromRole.grantedByFlow`). Empty when
/// the rule would decode with nothing dropped. A value of the wrong type is
/// not reported here: the decode that follows refuses it.
pub(super) fn role_gate_key_errors(rule: &Value) -> Vec<String> {
    let mut errors = Vec::new();
    if let Some(rule) = rule.as_object() {
        unknown_keys(rule, RULE_KEYS, "", &mut errors);
        if let Some(query) = rule.get("fromRole").and_then(Value::as_object) {
            query_key_errors(query, "fromRole", &mut errors);
        }
    }
    errors
}

/// One `ModelQuery` level, then its nested objects and its `or` arms.
fn query_key_errors(query: &Map<String, Value>, path: &str, errors: &mut Vec<String>) {
    unknown_keys(query, QUERY_KEYS, path, errors);
    for (key, known) in [
        ("count", COUNT_KEYS),
        ("producedByFlow", PRODUCED_BY_FLOW_KEYS),
        ("linkedTo", LINKED_TO_KEYS),
    ] {
        if let Some(object) = query.get(key).and_then(Value::as_object) {
            unknown_keys(object, known, &format!("{path}.{key}"), errors);
        }
    }
    for (field, condition) in query
        .get("where")
        .and_then(Value::as_object)
        .into_iter()
        .flatten()
    {
        let Some(ops) = condition.as_object() else {
            continue;
        };
        let at = format!("{path}.where.{field}");
        unknown_keys(ops, CONDITION_KEYS, &at, errors);
        // `PropertyCondition` is untagged: it keeps the first operator it
        // recognises and drops the rest.
        let operators: Vec<&str> = ops
            .keys()
            .map(String::as_str)
            .filter(|k| CONDITION_KEYS.contains(k))
            .collect();
        if operators.len() > 1 {
            errors.push(format!(
                "`{at}` has {} operators ({}); a condition takes one",
                operators.len(),
                operators
                    .iter()
                    .map(|k| format!("`{k}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
    }
    for (i, arm) in query
        .get("or")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .enumerate()
    {
        if let Some(arm) = arm.as_object() {
            query_key_errors(arm, &format!("{path}.or[{i}]"), errors);
        }
    }
}

fn unknown_keys(object: &Map<String, Value>, known: &[&str], path: &str, errors: &mut Vec<String>) {
    for key in object.keys().filter(|k| !known.contains(&k.as_str())) {
        let at = if path.is_empty() {
            key.clone()
        } else {
            format!("{path}.{key}")
        };
        match RENAMED.iter().find(|(old, _)| old == key) {
            Some((_, hint)) => errors.push(format!("unknown key `{at}` ({hint})")),
            None => errors.push(format!("unknown key `{at}`")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::{
        parse_flow_from_links, ConsensusRule, ConsensusRuleSlot, FlowState, ModelQuery,
        ModelQueryCount, ProducedByFlow, PropertyCondition,
    };
    use super::{
        role_gate_key_errors, CONDITION_KEYS, COUNT_KEYS, PRODUCED_BY_FLOW_KEYS, QUERY_KEYS,
        RULE_KEYS,
    };
    use crate::types::Link;
    use serde_json::{json, Value};
    use std::collections::BTreeSet;

    fn link(source: &str, predicate: &str, target: &str) -> Link {
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
    }

    fn literal(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    /// A one-state flow whose state carries `rule` as its `consensusRule`.
    fn state_with(rule: &str) -> FlowState {
        let flow_uri = "test://GatedFlow";
        let state_uri = "test://Gated.approved";
        let links = vec![
            link(flow_uri, "rdf://type", "ad4m://Flow"),
            link(flow_uri, "ad4m://flowName", &literal("Gated")),
            link(flow_uri, "ad4m://hasState", state_uri),
            link(state_uri, "rdf://type", "ad4m://FlowState"),
            link(state_uri, "ad4m://stateName", &literal("approved")),
            link(state_uri, "ad4m://consensusRule", &literal(rule)),
        ];
        let mut flow = parse_flow_from_links(&links, flow_uri).expect("reader");
        flow.states.remove(0)
    }

    /// Every place a role gate can carry a key, each with one key the reader
    /// does not know. Each literal decodes today with the key dropped (the
    /// premise assertion), and each dropped key widens the gate or removes
    /// it. The rule must read as `Malformed`, which `rule_for` refuses.
    #[test]
    #[rustfmt::skip]
    fn a_role_gate_with_an_unknown_key_is_malformed() {
        let cases: [(&str, &str); 9] = [
            ("the rule: a misspelt `fromRole` removes the gate",
             r#"{"n":1,"formRole":{"className":"ns://Role","didProperty":"agent"}}"#),
            ("the query: the key #1076 renamed",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","grantedByFlow":{"flow":"ns://GrantFlow","state":"Granted"}}}"#),
            ("the query: a misspelt `producedByFlow`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","producedbyFlow":{"flow":"ns://GrantFlow","state":"Granted"}}}"#),
            ("an `or` arm",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","or":[{"className":"ns://Role","wehre":{"rank":"lead"}}]}}"#),
            ("`count`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","count":{"min":1,"mxa":1}}}"#),
            ("`producedByFlow`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","producedByFlow":{"flow":"ns://GrantFlow","state":"Granted","since":"2026-01-01"}}}"#),
            ("an object `linkedTo`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","linkedTo":{"via":"ns://has","to":"base","author":"did:key:admin"}}}"#),
            ("a `where` condition: a key beside `equals`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","where":{"domain":{"equals":"frontend","author":"did:key:admin"}}}}"#),
            ("a `where` condition: two operators, one of them dropped",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","where":{"domain":{"equals":"frontend","in":["backend"]}}}}"#),
        ];
        for (name, rule) in cases {
            assert!(
                serde_json::from_str::<ConsensusRule>(rule).is_ok(),
                "{name}: premise: serde alone decodes this literal, dropping the key"
            );
            let state = state_with(rule);
            assert!(
                matches!(state.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                "{name}: an unknown key must make the rule Malformed, got {:?}",
                state.consensus_rule_slot()
            );
        }
    }

    /// A key written twice. `serde_json::Value` keeps the last of duplicate
    /// keys, so a check that decodes through a `Value` reads a different rule
    /// from the one the author wrote: a gate replaced by `null`, a
    /// `producedByFlow` dropped, `n` lowered. The first three decode to
    /// `Malformed` on the base (its derived `Deserialize` refuses a duplicate
    /// field); the last two sit in a map and a `Value`, where serde keeps the
    /// last without complaint, and are refused here too (fail closed).
    #[test]
    #[rustfmt::skip]
    fn a_role_gate_with_a_duplicate_key_is_malformed() {
        let cases: [(&str, &str); 5] = [
            ("the rule: `fromRole` then `fromRole: null` removes the gate",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent"},"fromRole":null}"#),
            ("the query: `producedByFlow` then `producedByFlow: null` leaves a plain `fromRole`",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","producedByFlow":{"flow":"ns://GrantFlow","state":"Granted"},"producedByFlow":null}}"#),
            ("the rule: `n` 2 then 1 lowers the quorum",
             r#"{"n":2,"fromRole":{"className":"ns://Role","didProperty":"agent"},"n":1}"#),
            ("a `where` property written twice",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","where":{"rank":"lead","rank":"guest"}}}"#),
            ("an object `linkedTo` with `via` twice",
             r#"{"n":1,"fromRole":{"className":"ns://Role","didProperty":"agent","linkedTo":{"via":"ns://has","to":"base","via":"ns://other"}}}"#),
        ];
        for (name, rule) in cases {
            let value: serde_json::Value = serde_json::from_str(rule).expect("valid JSON");
            assert_eq!(
                role_gate_key_errors(&value),
                Vec::<String>::new(),
                "{name}: premise: through a `Value` the duplicate is gone and no key is unknown"
            );
            let state = state_with(rule);
            assert!(
                matches!(state.consensus_rule_slot(), ConsensusRuleSlot::Malformed),
                "{name}: a duplicate key must make the rule Malformed, got {:?}",
                state.consensus_rule_slot()
            );
        }
    }

    /// The positive control: every key the reader knows, at every level, still
    /// decodes to a rule.
    #[test]
    fn a_role_gate_with_only_known_keys_is_a_rule() {
        let rule = r#"{"n":2,"fromRole":{"className":"ns://Role","didProperty":"agent",
            "where":{"domain":"frontend","rank":{"in":["lead","senior"]},"level":3,"active":true,
                     "team":{"equals":{"not":"$did"}},"tag":{"exists":true},"name":{"matches":"^a"}},
            "count":{"min":1,"max":4},"linkedTo":{"via":"ns://has","to":"base"},
            "producedByFlow":{"flow":"ns://GrantFlow","state":"Granted"},
            "or":[{"className":"ns://Role","where":{"author":"did:key:admin"}}]}}"#;
        let state = state_with(rule);
        assert!(
            matches!(state.consensus_rule_slot(), ConsensusRuleSlot::Rule(r) if r.n == 2),
            "got {:?}",
            state.consensus_rule_slot()
        );
    }

    /// The error names the key by its path, so whoever reads the warning can
    /// find it in the flow definition. Every unknown key is named, not only
    /// the first.
    #[test]
    #[rustfmt::skip]
    fn each_unknown_key_is_named_by_its_path() {
        let cases = [
            (json!({ "n": 1, "formRole": {} }),
             vec!["unknown key `formRole`"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "grantedByFlow": {} } }),
             vec!["unknown key `fromRole.grantedByFlow` (renamed to `producedByFlow` in #1076)"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "or": [{ "className": "R" }, { "className": "R", "wehre": {} }] } }),
             vec!["unknown key `fromRole.or[1].wehre`"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "count": { "mxa": 1 }, "producedByFlow": { "flow": "F", "state": "S", "since": 0 } } }),
             vec!["unknown key `fromRole.count.mxa`", "unknown key `fromRole.producedByFlow.since`"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "linkedTo": { "via": "p", "to": "base", "author": "A" } } }),
             vec!["unknown key `fromRole.linkedTo.author`"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "where": { "d": { "equals": "x", "author": "A" } } } }),
             vec!["unknown key `fromRole.where.d.author`"]),
            (json!({ "n": 1, "fromRole": { "className": "R", "where": { "d": { "equals": "x", "in": ["y"] } } } }),
             vec!["`fromRole.where.d` has 2 operators (`equals`, `in`); a condition takes one"]),
        ];
        for (rule, expected) in cases {
            assert_eq!(role_gate_key_errors(&rule), expected, "{rule}");
        }
    }

    /// Pins the key lists to the types. Every field of every type a role gate
    /// can carry is set here, with no `..Default::default()`, so a field added
    /// to one of them does not compile until it is set here too. Once set, it
    /// serialises under its wire name, and this test fails until that name is
    /// added to the lists above. Without it, a new field would be refused on
    /// every role gate while `requires` guards accepted it.
    #[test]
    fn every_field_the_types_serialise_is_a_known_key() {
        let query = |or: Option<Vec<ModelQuery>>| ModelQuery {
            class_name: "ns://Role".into(),
            r#where: Some(
                [
                    ("s", PropertyCondition::Str("x".into())),
                    ("n", PropertyCondition::Num(1.0)),
                    ("b", PropertyCondition::Bool(true)),
                    ("e", PropertyCondition::Equals { equals: json!("x") }),
                    (
                        "i",
                        PropertyCondition::In {
                            one_of: vec![json!("x")],
                        },
                    ),
                    ("x", PropertyCondition::Exists { exists: true }),
                    (
                        "m",
                        PropertyCondition::Matches {
                            matches: "^x".into(),
                        },
                    ),
                ]
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
            ),
            count: Some(ModelQueryCount {
                min: Some(1),
                max: Some(2),
            }),
            linked_to: Some(json!({ "via": "ns://has", "to": "base" })),
            did_property: Some("agent".into()),
            or,
            produced_by_flow: Some(ProducedByFlow {
                flow: "ns://GrantFlow".into(),
                state: "Granted".into(),
            }),
        };
        let rule = ConsensusRule {
            n: 1,
            from_role: Some(query(Some(vec![query(None)]))),
        };
        // The same for a new `PropertyCondition` variant: this match stops
        // compiling until the variant is added above.
        for condition in rule
            .from_role
            .iter()
            .flat_map(|q| q.r#where.iter().flatten())
        {
            match condition.1 {
                PropertyCondition::Str(_)
                | PropertyCondition::Num(_)
                | PropertyCondition::Bool(_)
                | PropertyCondition::Equals { .. }
                | PropertyCondition::In { .. }
                | PropertyCondition::Exists { .. }
                | PropertyCondition::Matches { .. } => {}
            }
        }
        let wire = serde_json::to_value(&rule).expect("serialises");
        assert_eq!(role_gate_key_errors(&wire), Vec::<String>::new());

        // Equality, not only inclusion: a key the types no longer serialise
        // (a stale name left behind by a rename) must leave its list too.
        let keys = |v: &Value| -> BTreeSet<String> {
            v.as_object().expect("object").keys().cloned().collect()
        };
        let list = |l: &[&str]| -> BTreeSet<String> { l.iter().map(|k| k.to_string()).collect() };
        let from_role = &wire["fromRole"];
        let conditions: BTreeSet<String> = from_role["where"]
            .as_object()
            .expect("where")
            .values()
            .filter_map(Value::as_object)
            .flat_map(|o| o.keys().cloned())
            .collect();
        for (level, wire_keys, known) in [
            ("rule", keys(&wire), RULE_KEYS),
            ("query", keys(from_role), QUERY_KEYS),
            ("or arm", keys(&from_role["or"][0]), QUERY_KEYS),
            ("count", keys(&from_role["count"]), COUNT_KEYS),
            ("producedByFlow", keys(&from_role["producedByFlow"]), PRODUCED_BY_FLOW_KEYS),
            ("where condition", conditions, CONDITION_KEYS),
        ] {
            // The `or` arm leaves its own `or` unset.
            let known = match level {
                "or arm" => list(known).into_iter().filter(|k| k != "or").collect(),
                _ => list(known),
            };
            assert_eq!(wire_keys, known, "{level}: wire keys and the known list differ");
        }
    }
}
