//! Which `consensusRule` governs entry into a state, and what quorum means
//! under it.

use crate::perspectives::shacl_parser::{ConsensusRule, ConsensusRuleSlot, SHACLFlow};
/// What governs entry INTO a state, once the scopes have been resolved.
#[derive(Debug, Clone)]
pub enum ResolvedRule {
    /// Apply this rule. Either one the author wrote, or — when no scope
    /// wrote one — the `{ n: 1 }` default.
    Rule(ConsensusRule),
    /// The governing scope wrote a `consensusRule` that did not parse. The
    /// transition is refused: no number of votes admits the edge, and no
    /// voter is eligible for it.
    Refused,
}

/// The rule governing entry INTO `to_state`: the target state's own
/// `consensusRule`, else the flow-level one, else `{ n: 1 }`.
///
/// # Why a malformed rule refuses instead of defaulting
///
/// `{ n: 1, from_role: None }` means *"one signature, from anybody"* — the
/// most permissive rule the engine can express. That is the right answer for
/// a flow that declares no rule at all: like-button-shaped actions (§4.1.1)
/// are the common case and must stay frictionless.
///
/// It is the wrong answer for a rule that failed to parse. There the author
/// did write a gate, and all that is known is that it could not be read.
/// Defaulting turns *"5 of N, and only from `Reviewer`"* into *"1 vote from
/// any agent"* with no error and no observable difference at configuration
/// time — and the substitution always runs toward less consensus, never more
/// (#1078). So the two cases get separate answers:
///
/// - **absent** → `{ n: 1 }`, exactly as before this was split;
/// - **malformed** → [`ResolvedRule::Refused`].
///
/// Refusing wedges the flow at that state, which is loud, local, and fixed by
/// correcting the literal. Defaulting fails silently and is discoverable only
/// by auditing what already advanced. This is the policy
/// [`resolve_role_grants`](super::roles::resolve_role_grants) already applies
/// when it cannot date a grant, and the one #1064 settled for `proof.valid`:
/// a value meaning *"could not be determined"* must never be read as a
/// verdict.
///
/// Scope precedence is unchanged, and a malformed scope does NOT fall through
/// to the next one out: falling back would let an unreadable state-level gate
/// be silently replaced by a weaker flow-level one, which is the very
/// substitution this function exists to prevent.
pub fn rule_for(flow: &SHACLFlow, to_state: &str) -> ResolvedRule {
    let state_slot = flow
        .states
        .iter()
        .find(|s| s.name == to_state)
        .map(|s| s.consensus_rule_slot())
        .unwrap_or(ConsensusRuleSlot::Absent);

    // First scope that says anything wins — including when what it says is
    // "unreadable".
    let governing = match state_slot {
        ConsensusRuleSlot::Absent => flow.consensus_rule_slot(),
        decided => decided,
    };

    match governing {
        ConsensusRuleSlot::Rule(rule) => ResolvedRule::Rule(rule.clone()),
        ConsensusRuleSlot::Malformed => ResolvedRule::Refused,
        ConsensusRuleSlot::Absent => ResolvedRule::Rule(ConsensusRule {
            n: 1,
            from_role: None,
        }),
    }
}

/// Whether enough distinct eligible voters signed. `{n: 0}` is a
/// misconfigured rule and never satisfies quorum: "nobody set a threshold"
/// must not read as "everybody passes".
pub fn quorum(rule: &ConsensusRule, distinct_voters: usize) -> bool {
    rule.n > 0 && distinct_voters as u32 >= rule.n
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::super::*;
    /// The threshold `rule_for` resolved, for the cases that expect a rule
    /// at all. Panics on [`ResolvedRule::Refused`] so a test asserting `n`
    /// can never pass by way of a refusal.
    fn n_of(flow: &SHACLFlow, to_state: &str) -> u32 {
        match rule_for(flow, to_state) {
            ResolvedRule::Rule(r) => r.n,
            ResolvedRule::Refused => panic!("expected a rule for `{to_state}`, got Refused"),
        }
    }

    /// The rule that governs an edge is the one on the state it ENTERS: the
    /// target's own override first, then the flow-level default.
    #[test]
    fn the_target_states_rule_overrides_the_flows() {
        let mut flow = review_flow(Some(2));
        flow.consensus_rule = Some(ConsensusRule {
            n: 1,
            from_role: None,
        });
        assert_eq!(n_of(&flow, "approved"), 2, "the state's own rule wins");
        assert_eq!(
            n_of(&flow, "changes_requested"),
            1,
            "a state without one inherits the flow's"
        );
    }

    /// `review_flow(None)` with `approved`'s `consensusRule` marked
    /// unreadable — what the parser records for a literal that did not
    /// decode (`shacl_parser::a_state_consensus_rule_that_does_not_parse_is_recorded_as_malformed`).
    fn review_flow_with_unreadable_approved_rule() -> SHACLFlow {
        let mut flow = review_flow(None);
        let approved = flow
            .states
            .iter_mut()
            .find(|s| s.name == "approved")
            .expect("fixture has an `approved` state");
        approved.consensus_rule_malformed = true;
        flow
    }

    /// **The defect in #1078, as behaviour.** A single unqualified vote must
    /// not carry a flow into a state whose consensus rule could not be read.
    /// Before the fix the unreadable rule became `{n: 1, from_role: None}`
    /// and this exact vote settled the edge.
    ///
    /// Paired with `an_absent_rule_still_advances_on_one_vote`, which is the
    /// same flow and the same vote with the rule merely absent. The pair is
    /// the assertion: one fixture field differs, and the outcome flips. A
    /// fold that refused everything, or one that advanced on everything,
    /// fails one of the two.
    ///
    /// Killing mutation: in `settle_edge`, replace the `ResolvedRule::Rule`
    /// let-else with `let rule = match rule_for(flow, to) { Rule(r) => r,
    /// Refused => ConsensusRule { n: 1, from_role: None } }` — i.e. restore
    /// the old default at the point of use.
    #[test]
    fn a_malformed_rule_does_not_advance_on_one_vote() {
        let flow = review_flow_with_unreadable_approved_rule();
        assert!(
            matches!(rule_for(&flow, "approved"), ResolvedRule::Refused),
            "premise: the unreadable rule must resolve to Refused"
        );

        let derived = fold(
            "review",
            &flow,
            &[vouched("p1", "review", "approved", &[(ALICE, T1)])],
        );

        assert_eq!(
            derived.state, "review",
            "an unreadable rule must not be answered with `one vote from anybody`"
        );
        assert!(
            derived.settled.is_empty(),
            "no edge may settle under a rule that could not be read"
        );
        assert!(
            derived.contested.is_none(),
            "a refused edge is not a contested one — refusal is not a race"
        );
    }

    /// The positive control. Identical flow and identical vote, except that
    /// no `consensusRule` was ever written: the `{n: 1}` default still
    /// applies and the flow still advances.
    ///
    /// Killing mutation: make `rule_for` return `ResolvedRule::Refused` for
    /// `ConsensusRuleSlot::Absent` — the fail-closed-everywhere overshoot,
    /// which every other test in this group would still pass.
    #[test]
    fn an_absent_rule_still_advances_on_one_vote() {
        let flow = review_flow(None);
        assert_eq!(
            n_of(&flow, "approved"),
            1,
            "premise: an absent rule still resolves to the {{n: 1}} default"
        );

        let derived = fold(
            "review",
            &flow,
            &[vouched("p1", "review", "approved", &[(ALICE, T1)])],
        );

        assert_eq!(derived.state, "approved");
        assert_eq!(walked(&derived), vec![("review", "approved")]);
    }

    /// An unreadable rule at state scope does not fall through to a
    /// readable flow-level one. The flow-level rule here is `{n: 1}` —
    /// strictly weaker than whatever the author wrote on `approved` — so
    /// falling back would perform the same silent downgrade the fix exists
    /// to stop, just one scope further out.
    ///
    /// Killing mutation: in `rule_for`, change the scope match to
    /// `ConsensusRuleSlot::Rule(r) => …, _ => flow.consensus_rule_slot()`,
    /// i.e. treat `Malformed` at state scope as "nothing said here".
    #[test]
    fn a_malformed_state_rule_does_not_fall_back_to_the_flow_rule() {
        let mut flow = review_flow_with_unreadable_approved_rule();
        flow.consensus_rule = Some(ConsensusRule {
            n: 1,
            from_role: None,
        });
        assert_eq!(
            n_of(&flow, "changes_requested"),
            1,
            "premise: the flow-level rule is readable and would admit one vote"
        );

        assert!(
            matches!(rule_for(&flow, "approved"), ResolvedRule::Refused),
            "the unreadable state rule decides; the flow-level one is not consulted"
        );

        let derived = fold(
            "review",
            &flow,
            &[vouched("p1", "review", "approved", &[(ALICE, T1)])],
        );
        assert_eq!(derived.state, "review");
    }

    /// The refusal is scoped to the state whose rule is unreadable, not to
    /// the flow. `changes_requested` is governed by its own (absent) scope
    /// and still fires — so a single bad literal wedges one edge rather
    /// than bricking every flow that shares the definition.
    ///
    /// Killing mutation: in `settle_edge`, `return None` whenever ANY state
    /// of the flow is malformed rather than the target state.
    #[test]
    fn a_malformed_rule_on_one_state_leaves_the_others_alone() {
        let flow = review_flow_with_unreadable_approved_rule();

        let derived = fold(
            "review",
            &flow,
            &[vouched("p1", "review", "changes_requested", &[(ALICE, T1)])],
        );

        assert_eq!(derived.state, "changes_requested");
        assert_eq!(walked(&derived), vec![("review", "changes_requested")]);
    }
}
