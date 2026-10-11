//! Whether a losing edge is foreclosed: reachability over declared
//! transitions, and the atom-evidenced variant `settle` uses to decide
//! contention.

use super::{rule_for, ResolvedRule, VouchedAtom};
use crate::perspectives::shacl_parser::SHACLFlow;
/// Whether `to` can still be entered once the flow is in `from`, following
/// declared transitions only.
///
/// This asks about the flow's shape, not about votes: an edge that is quorate
/// now but loses a race is only *denied* if the state it leads to drops out of
/// the graph reachable from the winner. A state is reachable from itself, so
/// two transitions sharing a target never contend.
///
/// Used for the unit test that checks pure graph shape. [`settle`] uses
/// [`feasibly_reachable`] instead, which also requires atom evidence on every
/// intermediate hop.
fn reachable(flow: &SHACLFlow, from: &str, to: &str) -> bool {
    let mut seen = vec![from.to_string()];
    let mut frontier = vec![from.to_string()];
    while let Some(state) = frontier.pop() {
        if state == to {
            return true;
        }
        for t in flow.transitions.iter().filter(|t| t.from_state == state) {
            if !seen.iter().any(|s| s == &t.to_state) {
                seen.push(t.to_state.clone());
                frontier.push(t.to_state.clone());
            }
        }
    }
    false
}

/// Whether `to` is reachable from `from` through hops that each carry at
/// least one **admitted vote** — a [`VouchedAtom`] in `atoms` whose
/// `eligible_votes` is non-empty.
///
/// The threshold is deliberate, and role-aware. Declaring an edge is
/// unilateral (any flow author); *proposing* on it is also unilateral (any
/// participant, including one whose votes the role rule excludes — #1030
/// decides whose votes count, and a mere-existence check would route around
/// it). An admitted vote is the first non-unilateral evidence that the
/// cycle is live, and it is the same admission `settle_edge` pools twenty
/// lines down — the two readings of the atom slice must not disagree on
/// whose voice counts. Quorum is NOT required: that would change what
/// "feasible" means for a half-voted edge, a bigger semantic call than
/// contention suppression needs.
///
/// Failing closed here means an unconfirmed-cycle path does not silently
/// suppress contention detection in [`settle`]. A hop whose target's rule
/// is [`ResolvedRule::Refused`] is skipped for the same reason: an edge that
/// cannot fire is not an escape route (#1078).
///
/// A state is reachable from itself regardless of atoms (the two-transitions-
/// to-the-same-target case never contends).
pub(super) fn feasibly_reachable(
    flow: &SHACLFlow,
    from: &str,
    to: &str,
    atoms: &[VouchedAtom],
) -> bool {
    if from == to {
        return true;
    }
    let mut seen = vec![from.to_string()];
    let mut frontier = vec![from.to_string()];
    while let Some(state) = frontier.pop() {
        for t in flow.transitions.iter().filter(|t| t.from_state == state) {
            // A hop into a state whose `consensusRule` could not be read can
            // never fire, however many votes it has collected, so it cannot
            // carry the loser anywhere. Same argument as the phantom
            // back-edge of #999, one step stronger: there the votes were
            // missing, here it is the rule that would admit them (#1078).
            if matches!(rule_for(flow, &t.to_state), ResolvedRule::Refused) {
                continue;
            }
            // Only traverse this hop if some atom on it carries an admitted
            // vote. A phantom hop (no atoms), a vote-less proposal, or an
            // atom voted only by excluded DIDs cannot be relied on to carry
            // the loser to its target.
            let hop_has_admitted_vote = atoms.iter().any(|a| {
                a.atom.from_state == state
                    && a.atom.to_state == t.to_state
                    && !a.eligible_votes.is_empty()
            });
            if !hop_has_admitted_vote {
                continue;
            }
            if t.to_state == to {
                return true;
            }
            if !seen.iter().any(|s| s == &t.to_state) {
                seen.push(t.to_state.clone());
                frontier.push(t.to_state.clone());
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::super::*;
    use super::*;
    /// `triage → approved | rejected`, both terminal: the shape where a
    /// clock-decided winner cannot be undone.
    fn terminal_branch_flow() -> SHACLFlow {
        serde_json::from_value(serde_json::json!({
            "name": "Triage",
            "namespace": "triage://",
            "states": [
                { "name": "triage", "value": 0.0 },
                { "name": "approved", "value": 1.0 },
                { "name": "rejected", "value": 0.0 },
            ],
            "transitions": [
                { "action_name": "Approve", "from_state": "triage", "to_state": "approved", "actions": [] },
                { "action_name": "Reject", "from_state": "triage", "to_state": "rejected", "actions": [] },
            ],
        }))
        .expect("fixture flow parses")
    }

    /// Two quorate edges into states that cannot reach each other: taking the
    /// earliest would let a self-asserted timestamp decide an outcome nothing
    /// can undo, so the walk stops instead.
    #[test]
    fn an_irreversible_branch_stops_the_walk_without_choosing() {
        let flow = terminal_branch_flow();
        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
        ];

        let derived = fold("triage", &flow, &atoms);

        assert_eq!(derived.state, "triage", "the walk must not leave the state");
        assert!(derived.settled.is_empty(), "and must take no edge");
        let contention = derived.contested.expect("contested branch is reported");
        assert_eq!(contention.from_state, "triage");
        // Ranked earliest-first, so a reader sees what would have been taken.
        let targets: Vec<&str> = contention
            .candidates
            .iter()
            .map(|e| e.to_state.as_str())
            .collect();
        assert_eq!(targets, vec!["approved", "rejected"]);
        assert_eq!(
            contention.candidates[0].voters,
            vec![ALICE.to_string()],
            "each candidate still carries the quorum that made it"
        );
    }

    /// The same two proposals in a flow where the loser stays reachable are
    /// not contention: the walk takes the earliest now and the other later, so
    /// the timestamp ordered the edges without denying either.
    #[test]
    fn a_race_the_walk_can_come_back_from_is_not_contention() {
        let flow = review_flow(Some(1));
        let atoms = vec![
            vouched("p1", "review", "changes_requested", &[(ALICE, T1)]),
            vouched("p3", "review", "approved", &[(ALICE, T2)]),
            vouched("p2", "changes_requested", "review", &[(ALICE, T3)]),
        ];

        let derived = fold("review", &flow, &atoms);

        assert!(
            derived.contested.is_none(),
            "approved is reachable via the cycle"
        );
        assert_eq!(derived.state, "approved");
    }

    /// Refusal is scoped to a genuine race: one quorate edge alongside an edge
    /// with votes but not enough of them is not contention.
    #[test]
    fn a_branch_with_only_one_quorate_edge_is_not_contested() {
        let mut flow = terminal_branch_flow();
        // `approved` needs two voters; only Alice signed it.
        flow.states
            .iter_mut()
            .find(|s| s.name == "approved")
            .expect("fixture has approved")
            .consensus_rule = Some(ConsensusRule {
            n: 2,
            from_role: None,
        });
        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
        ];

        let derived = fold("triage", &flow, &atoms);

        assert_eq!(derived.state, "rejected");
        assert!(derived.contested.is_none(), "one candidate is not a race");
    }

    /// An uncontested walk reports no contention, so `contested.is_some()` is a
    /// safe test for "do not pay out on this".
    #[test]
    fn an_ordinary_stall_is_not_contention() {
        let derived = fold("review", &review_flow(None), &[]);
        assert_eq!(derived.state, "review");
        assert!(derived.contested.is_none());
    }

    #[test]
    fn reachability_follows_declared_transitions_only() {
        let review = review_flow(None);
        assert!(
            reachable(&review, "changes_requested", "approved"),
            "via the cycle"
        );
        assert!(
            reachable(&review, "approved", "approved"),
            "a state reaches itself"
        );
        assert!(
            !reachable(&review, "approved", "review"),
            "approved is terminal"
        );
        let triage = terminal_branch_flow();
        assert!(
            !reachable(&triage, "approved", "rejected"),
            "terminal siblings"
        );
    }

    /// A flow with a back-edge `approved → triage` that is declared in the
    /// schema but has zero atoms — a phantom escape route that makes
    /// `reachable()` report `rejected` as recoverable from `approved`.
    /// With `feasibly_reachable()` (no atoms on the back-edge) the branch is
    /// correctly treated as foreclosed and contention is reported.
    fn phantom_back_edge_flow() -> SHACLFlow {
        serde_json::from_value(serde_json::json!({
            "name": "Triage",
            "namespace": "triage://",
            "states": [
                { "name": "triage",   "value": 0.0 },
                { "name": "approved", "value": 1.0 },
                { "name": "rejected", "value": 0.0 },
            ],
            "transitions": [
                { "action_name": "Approve", "from_state": "triage",   "to_state": "approved", "actions": [] },
                { "action_name": "Reject",  "from_state": "triage",   "to_state": "rejected", "actions": [] },
                // Back-edge: a flow author declares this, making `rejected`
                // graph-reachable from `approved`; but if nobody votes on it
                // the cycle is phantom.
                { "action_name": "Reset",   "from_state": "approved", "to_state": "triage",   "actions": [] },
            ],
        }))
        .expect("fixture flow parses")
    }

    /// **Regression for #999.** A back-edge with zero atoms is graph-reachable
    /// but not feasibly traversable. The engine must still report contention
    /// rather than silently take the earliest edge.
    ///
    /// This test fails on the unfixed code (which uses the pure-graph
    /// `reachable()`) and must pass after the fix.
    #[test]
    fn phantom_back_edge_with_no_atoms_does_not_suppress_contention() {
        let flow = phantom_back_edge_flow();
        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
            // No atom for `approved → triage`; the back-edge is declared but phantom.
        ];

        let derived = fold("triage", &flow, &atoms);

        assert_eq!(derived.state, "triage", "walk must not leave triage");
        assert!(derived.settled.is_empty(), "no edge may be taken");
        let contention = derived
            .contested
            .expect("phantom back-edge must not suppress contention");
        assert_eq!(contention.from_state, "triage");
        let targets: Vec<&str> = contention
            .candidates
            .iter()
            .map(|e| e.to_state.as_str())
            .collect();
        assert_eq!(
            targets,
            vec!["approved", "rejected"],
            "both candidates are visible in the reported contention"
        );
    }

    /// When the back-edge in `phantom_back_edge_flow` actually has atoms, the
    /// cycle is confirmed live: `rejected` is feasibly reachable from
    /// `approved`, so taking the earliest edge is correct and no contention is
    /// reported.
    #[test]
    fn confirmed_back_edge_with_atoms_is_not_contention() {
        let flow = phantom_back_edge_flow();
        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
            vouched("a://reset", "approved", "triage", &[(ALICE, T3)]),
        ];

        let derived = fold("triage", &flow, &atoms);

        assert!(
            derived.contested.is_none(),
            "back-edge has atoms: the cycle is live, so no contention"
        );
        // The fold walks: triage→approved (T1), then approved→triage (T3 via back-edge),
        // then triage→rejected (T3, since approved's atom is now consumed). No contention
        // at any step; the cycle is confirmed live by the back-edge atom.
        assert_eq!(
            derived.state, "rejected",
            "cycle completes: approved consumed, rejected fires on revisit"
        );
    }

    /// The differential partner of `confirmed_back_edge_with_atoms_is_not_contention`:
    /// identical flow, identical atoms, identical votes — and `triage`'s
    /// `consensusRule` unreadable, so the back-edge into it can never fire.
    ///
    /// An edge that cannot fire is not an escape route. Extending #999: a
    /// phantom back-edge had no votes, this one has a fully admitted vote
    /// and no rule to admit it under. Suppressing contention on it would let
    /// the earliest clock win an irreversible branch silently — the outcome
    /// `settle` refuses by design.
    ///
    /// Killing mutation: drop the `ResolvedRule::Refused => continue` guard
    /// at the top of `feasibly_reachable`'s transition loop. Note this is the
    /// only unit-level test that reaches that guard: the parallel guard in
    /// `fold_read_set` (which empties `eligible_votes` for a refused target)
    /// is exercised only by the e2e suite, and a mutation of it survives —
    /// see the PR body.
    #[test]
    fn a_refused_back_edge_does_not_suppress_contention() {
        let mut flow = phantom_back_edge_flow();
        flow.states
            .iter_mut()
            .find(|s| s.name == "triage")
            .expect("fixture has a `triage` state")
            .consensus_rule_malformed = true;
        assert!(
            matches!(rule_for(&flow, "triage"), ResolvedRule::Refused),
            "premise: the back-edge's target rule must be Refused"
        );

        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
            // Same admitted vote as the confirmed-cycle test above.
            vouched("a://reset", "approved", "triage", &[(ALICE, T3)]),
        ];

        let derived = fold("triage", &flow, &atoms);

        let contention = derived
            .contested
            .expect("a back-edge that cannot fire must not suppress contention");
        assert_eq!(contention.from_state, "triage");
        assert_eq!(derived.state, "triage", "the walk stops without choosing");
        assert!(derived.settled.is_empty());
    }

    /// The separating case between "someone proposed" and "the cycle is
    /// live": an atom EXISTS on the back-edge but carries zero admitted
    /// votes (a vote-less proposal, or one voted only by DIDs the role rule
    /// excludes — `eligible_votes` is the role-gated list). Proposing is
    /// unilateral, so it must not turn a reported Contention back into a
    /// silent adjudication. This test passes under the eligible-vote
    /// threshold and fails under a mere atom-existence check.
    #[test]
    fn vote_less_back_edge_atom_does_not_suppress_contention() {
        let flow = phantom_back_edge_flow();
        let atoms = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
            // Atom exists on the back-edge, but nobody's vote was admitted.
            vouched("a://reset", "approved", "triage", &[]),
        ];

        let derived = fold("triage", &flow, &atoms);

        assert_eq!(derived.state, "triage", "walk must not leave triage");
        let contention = derived
            .contested
            .expect("a vote-less proposal on the back-edge must not suppress contention");
        assert_eq!(contention.from_state, "triage");
    }

    /// `feasibly_reachable` returns true when every hop on the path has atoms,
    /// false when a hop is phantom, and always true when `from == to`.
    #[test]
    fn feasibly_reachable_requires_atom_evidence_on_every_hop() {
        let flow = phantom_back_edge_flow();

        // Atoms only for triage → {approved,rejected}; back-edge is phantom.
        let without_back = vec![
            vouched("a://approve", "triage", "approved", &[(ALICE, T1)]),
            vouched("a://reject", "triage", "rejected", &[(BOB, T2)]),
        ];
        // rejected is NOT feasibly reachable from approved (back-edge phantom).
        assert!(
            !feasibly_reachable(&flow, "approved", "rejected", &without_back),
            "phantom hop blocks the path"
        );
        // A state is always reachable from itself.
        assert!(
            feasibly_reachable(&flow, "approved", "approved", &without_back),
            "same-state is trivially reachable"
        );

        // Add an atom for the back-edge; now the path is confirmed.
        let mut with_back = without_back.clone();
        with_back.push(vouched("a://reset", "approved", "triage", &[(ALICE, T3)]));
        assert!(
            feasibly_reachable(&flow, "approved", "rejected", &with_back),
            "confirmed back-edge makes rejected reachable"
        );
        // Still not reachable from a dead end.
        assert!(
            !feasibly_reachable(&flow, "rejected", "approved", &with_back),
            "rejected has no outgoing transitions"
        );
    }
}
