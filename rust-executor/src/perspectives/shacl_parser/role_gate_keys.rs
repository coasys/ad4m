#[cfg(test)]
mod tests {
    use super::super::{parse_flow_from_links, ConsensusRuleSlot, FlowState};
    use crate::types::Link;

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
                serde_json::from_str::<super::super::ConsensusRule>(rule).is_ok(),
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
}
