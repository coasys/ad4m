//! **The engine's entire belief system.** Everything else in this directory
//! exists to hand [`fold`] honest inputs.
//!
//! The state of a flow is not stored anywhere. It is computed, every time,
//! from the signed links that exist on the graph right now: walk from the
//! flow definition's first state and take, out of each state, whichever
//! declared edge reached quorum earliest; where no edge out of a state has
//! settled, the walk stops and that state is the answer.
//!
//! Three properties follow, and they are the reason this file is pure:
//!
//! - **Deterministic.** Two replicas holding the same links derive the same
//!   state, whatever order their stores returned them in — every choice here
//!   is by an explicit sort key.
//! - **Re-verifiable off-perspective.** [`fold`] takes plain data and does no
//!   I/O, so a verifier outside the neighbourhood can re-run it over a
//!   serialised [`ReadSet`](super::ReadSet) and reach the same verdict. That
//!   is what lets a read-set back a minted token as a proof rather than an
//!   assertion.
//! - **A function of the links present now.** Delete a settled vote and the
//!   fold recomputes without it, so the flow stands where it stood before
//!   that vote. The graph is the truth and the state follows it.
//!
//! Quorum belongs to an **edge**, not to a proposal. Two bots each minting
//! their own proposal for `review → approved` under `{n: 2}` is how a bot
//! flow reaches consensus without a human click: asking "is *this proposal*
//! settled?" answers no forever, while asking "did the *edge* collect two
//! distinct voters?" answers yes.

use super::atom::{TransitionAtom, Vote};
use crate::perspectives::shacl_parser::{ConsensusRule, SHACLFlow};

/// An atom whose votes have already been filtered by its rule's `fromRole`.
/// Role resolution is the one step that needs the store, so the loader does
/// it first and the fold stays pure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VouchedAtom {
    pub atom: TransitionAtom,
    pub eligible_votes: Vec<Vote>,
}

/// One consensus event the fold accepted: a declared edge, the moment it
/// reached quorum, and the atoms whose voters made up that quorum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SettledEdge {
    pub from_state: String,
    pub to_state: String,
    /// When the n-th distinct eligible voter signed — or, for an edge that
    /// was already quorate when the walk arrived, the moment of arrival.
    pub settled_at: String,
    /// The proposals that contributed the counted votes, sorted.
    pub atom_uris: Vec<String>,
    /// The distinct DIDs that made up the quorum, sorted.
    pub voters: Vec<String>,
}

/// The authoritative state of a flow instance, and the chain that produced it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedState {
    pub state: String,
    pub settled: Vec<SettledEdge>,
}

/// The rule governing entry INTO `to_state`: the target state's own
/// `consensusRule`, else the flow-level one, else `{ n: 1 }`.
pub fn rule_for(flow: &SHACLFlow, to_state: &str) -> ConsensusRule {
    flow.states
        .iter()
        .find(|s| s.name == to_state)
        .and_then(|s| s.consensus_rule.as_ref())
        .or(flow.consensus_rule.as_ref())
        .cloned()
        .unwrap_or(ConsensusRule {
            n: 1,
            from_role: None,
        })
}

/// Whether enough distinct eligible voters signed. `{n: 0}` is a
/// misconfigured rule and never satisfies quorum: "nobody set a threshold"
/// must not read as "everybody passes".
pub fn quorum(rule: &ConsensusRule, distinct_voters: usize) -> bool {
    rule.n > 0 && distinct_voters as u32 >= rule.n
}

/// The state of a flow: walk from `genesis`, taking the earliest-settled
/// declared edge out of each state, until no edge out of it has settled.
///
/// Terminates because every settled edge consumes at least one atom (a
/// quorum needs at least one voter, and a voter needs an atom to vote on)
/// and `atoms` is finite.
pub fn fold(genesis: &str, flow: &SHACLFlow, atoms: &[VouchedAtom]) -> DerivedState {
    let mut state = genesis.to_string();
    let mut settled_at = String::new(); // "" sorts before every timestamp
    let mut settled: Vec<SettledEdge> = Vec::new();
    while let Some(edge) = settle(&state, &settled_at, flow, atoms, &settled) {
        state = edge.to_state.clone();
        settled_at = edge.settled_at.clone();
        settled.push(edge);
    }
    DerivedState { state, settled }
}

/// The declared edge out of `state` that settled earliest.
///
/// Votes are not filtered by `after`: an edge that was already quorate when
/// the walk arrived is settled, and `after` only floors the moment it counts
/// as having settled (see [`settle_edge`]). Ties break by target state, then
/// by first atom URI, so two replicas choose the same edge.
fn settle(
    state: &str,
    after: &str,
    flow: &SHACLFlow,
    atoms: &[VouchedAtom],
    consumed: &[SettledEdge],
) -> Option<SettledEdge> {
    flow.transitions
        .iter()
        .filter(|t| t.from_state == state)
        .filter_map(|t| settle_edge(state, &t.to_state, after, flow, atoms, consumed))
        .min_by_key(|e| {
            (
                e.settled_at.clone(),
                e.to_state.clone(),
                e.atom_uris.first().cloned().unwrap_or_default(),
            )
        })
}

/// One edge: pool the eligible votes of every not-yet-consumed atom on
/// `from → to`, and if the rule's `{n}` distinct voters is met, report the
/// edge as settled at the moment the n-th of them signed.
///
/// An edge that was already quorate when the walk arrived settles at the
/// moment of arrival (`max(nth, after)`), so a proposal that lost an earlier
/// race still fires on the next visit rather than wedging a cyclic flow.
///
/// Evidence is not re-checked here: every atom carries a seal each of its
/// voters verified on their own replica before signing (`super::accept`).
fn settle_edge(
    from: &str,
    to: &str,
    after: &str,
    flow: &SHACLFlow,
    atoms: &[VouchedAtom],
    consumed: &[SettledEdge],
) -> Option<SettledEdge> {
    let rule = rule_for(flow, to);
    let mut pooled: Vec<(&Vote, &str)> = atoms
        .iter()
        .filter(|v| v.atom.from_state == from && v.atom.to_state == to)
        .filter(|v| !consumed.iter().any(|e| e.atom_uris.contains(&v.atom.uri)))
        .flat_map(|v| {
            v.eligible_votes
                .iter()
                .map(|vote| (vote, v.atom.uri.as_str()))
        })
        .collect();
    pooled.sort_by(|(a, a_uri), (b, b_uri)| (&a.at, &a.did, a_uri).cmp(&(&b.at, &b.did, b_uri)));

    let mut voters: Vec<String> = Vec::new();
    let mut atom_uris: Vec<String> = Vec::new();
    let mut nth_at = None;
    for (vote, uri) in pooled {
        if voters.contains(&vote.did) {
            continue;
        }
        voters.push(vote.did.clone());
        if !atom_uris.iter().any(|u| u == uri) {
            atom_uris.push(uri.to_string());
        }
        if quorum(&rule, voters.len()) {
            nth_at = Some(vote.at.clone());
            break;
        }
    }
    let nth_at = nth_at?;

    atom_uris.sort();
    voters.sort();
    Some(SettledEdge {
        from_state: from.to_string(),
        to_state: to.to_string(),
        settled_at: nth_at.max(after.to_string()),
        atom_uris,
        voters,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";
    const T1: &str = "2026-01-01T00:00:00.000Z";
    const T2: &str = "2026-01-02T00:00:00.000Z";
    const T3: &str = "2026-01-03T00:00:00.000Z";

    /// `review ⇄ changes_requested`, plus `review → approved` whose `{n}` is
    /// configurable, so every quorum case fits one fixture.
    fn review_flow(approved_n: Option<u32>) -> SHACLFlow {
        let mut approved = serde_json::json!({ "name": "approved", "value": 1.0 });
        if let Some(n) = approved_n {
            approved["consensusRule"] = serde_json::json!({ "n": n });
        }
        serde_json::from_value(serde_json::json!({
            "name": "Review",
            "namespace": "review://",
            "states": [
                { "name": "review", "value": 0.0 },
                { "name": "changes_requested", "value": 0.5 },
                approved,
            ],
            "transitions": [
                { "action_name": "Request", "from_state": "review", "to_state": "changes_requested", "actions": [] },
                { "action_name": "Resubmit", "from_state": "changes_requested", "to_state": "review", "actions": [] },
                { "action_name": "Approve", "from_state": "review", "to_state": "approved", "actions": [] },
            ],
        }))
        .expect("fixture flow parses")
    }

    /// One atom on `from → to` carrying exactly the listed `(did, at)` votes
    /// as eligible. Fields the fold never reads carry placeholders.
    fn vouched(uri: &str, from: &str, to: &str, votes: &[(&str, &str)]) -> VouchedAtom {
        let votes: Vec<Vote> = votes
            .iter()
            .map(|(did, at)| Vote {
                did: did.to_string(),
                at: at.to_string(),
            })
            .collect();
        VouchedAtom {
            atom: TransitionAtom {
                uri: uri.to_string(),
                from_state: from.to_string(),
                to_state: to.to_string(),
                proposer: votes.first().map(|v| v.did.clone()).unwrap_or_default(),
                proposed_at: votes.first().map(|v| v.at.clone()).unwrap_or_default(),
                evidence_hash: "seal".to_string(),
                votes: votes.clone(),
            },
            eligible_votes: votes,
        }
    }

    fn walked(derived: &DerivedState) -> Vec<(&str, &str)> {
        derived
            .settled
            .iter()
            .map(|e| (e.from_state.as_str(), e.to_state.as_str()))
            .collect()
    }

    #[test]
    fn with_no_votes_the_state_is_genesis() {
        let derived = fold("review", &review_flow(None), &[]);
        assert_eq!(derived.state, "review");
        assert!(derived.settled.is_empty());
    }

    /// Test 4. Quorum is a property of the EDGE. Two bots each mint their own
    /// proposal for `review → approved` under `{n: 2}`; neither proposal has
    /// two votes, but the edge has two distinct voters. A fold that asked "is
    /// this proposal settled?" would leave such a flow at genesis forever.
    #[test]
    fn quorum_is_counted_across_twin_proposals_on_one_edge() {
        let flow = review_flow(Some(2));
        let alone = fold(
            "review",
            &flow,
            &[vouched("p1", "review", "approved", &[(ALICE, T1)])],
        );
        assert_eq!(alone.state, "review", "1 < n = 2 must not advance");

        let together = fold(
            "review",
            &flow,
            &[
                vouched("p1", "review", "approved", &[(ALICE, T1)]),
                vouched("p2", "review", "approved", &[(BOB, T2)]),
            ],
        );
        assert_eq!(together.state, "approved");
        assert_eq!(together.settled.len(), 1, "one consensus event, two atoms");
        assert_eq!(together.settled[0].atom_uris, vec!["p1", "p2"]);
        assert_eq!(together.settled[0].voters, vec![ALICE, BOB]);
        assert_eq!(
            together.settled[0].settled_at, T2,
            "the edge settled when the SECOND voter signed"
        );
    }

    /// Test 5. The same DID twice is one voter — otherwise one agent clears
    /// any `{n}` by minting `n` proposals, or by voting on its own twice.
    #[test]
    fn one_did_cannot_reach_quorum_alone() {
        let flow = review_flow(Some(2));
        for (name, atoms) in [
            (
                "one DID, two proposals",
                vec![
                    vouched("p1", "review", "approved", &[(ALICE, T1)]),
                    vouched("p2", "review", "approved", &[(ALICE, T2)]),
                ],
            ),
            (
                "one DID, two votes on one proposal",
                vec![vouched(
                    "p1",
                    "review",
                    "approved",
                    &[(ALICE, T1), (ALICE, T2)],
                )],
            ),
        ] {
            assert_eq!(fold("review", &flow, &atoms).state, "review", "{name}");
        }
    }

    /// Test 6. One walk, four properties: the earliest-settled edge wins the
    /// race out of `review`; the atom that fired an edge is consumed and
    /// cannot fire it again on a revisit; and an edge that was already
    /// quorate when the walk came back settles at the moment of ARRIVAL, so
    /// the loser of the first race fires on the next visit instead of
    /// wedging the cycle.
    #[test]
    fn the_earliest_settled_edge_wins_and_a_cycle_consumes_each_atom_once() {
        let derived = fold(
            "review",
            &review_flow(None),
            &[
                vouched("p1", "review", "changes_requested", &[(ALICE, T1)]),
                vouched("p3", "review", "approved", &[(ALICE, T2)]),
                vouched("p2", "changes_requested", "review", &[(ALICE, T3)]),
            ],
        );
        assert_eq!(
            walked(&derived),
            vec![
                ("review", "changes_requested"),
                ("changes_requested", "review"),
                ("review", "approved"),
            ]
        );
        assert_eq!(derived.state, "approved");
        assert_eq!(
            derived.settled[0].atom_uris,
            vec!["p1"],
            "T1 beat T2 out of `review`"
        );
        assert_eq!(
            derived.settled[2].atom_uris,
            vec!["p3"],
            "the revisit consumed the loser, not the already-consumed winner"
        );
        assert_eq!(
            derived.settled[2].settled_at, T3,
            "a stale-but-valid edge settles when the walk arrives, not when it was quorate"
        );
    }

    /// Test 7. Two replicas hold the same links in whatever order their
    /// stores return them, and must derive the same state.
    #[test]
    fn fold_is_deterministic_under_input_order() {
        let flow = review_flow(Some(2));
        let atoms = vec![
            vouched("p1", "review", "changes_requested", &[(ALICE, T1)]),
            vouched("p2", "changes_requested", "review", &[(BOB, T2)]),
            vouched("p3", "review", "approved", &[(ALICE, T3)]),
            vouched("p4", "review", "approved", &[(BOB, T3)]),
        ];
        let expected = fold("review", &flow, &atoms);
        assert_eq!(expected.state, "approved");
        for rotation in 1..atoms.len() {
            let mut shuffled = atoms.clone();
            shuffled.rotate_left(rotation);
            assert_eq!(
                fold("review", &flow, &shuffled),
                expected,
                "rotation {rotation} derived a different state"
            );
        }
    }

    /// Test 8. Fail-closed on the definition's shape: a proposal is only ever
    /// as good as the edge it names. None of these may move the flow, however
    /// many valid signatures they carry.
    #[test]
    fn undeclared_edge_wrong_from_state_and_zero_threshold_never_settle() {
        for (name, flow, atom) in [
            (
                "the definition declares no `review → review` edge",
                review_flow(None),
                vouched("p1", "review", "review", &[(ALICE, T1), (BOB, T2)]),
            ),
            (
                "leaves a state the flow is not standing in",
                review_flow(None),
                vouched("p2", "approved", "review", &[(ALICE, T1), (BOB, T2)]),
            ),
            (
                "`{n: 0}` is a misconfigured threshold, not an open door",
                review_flow(Some(0)),
                vouched("p3", "review", "approved", &[(ALICE, T1), (BOB, T2)]),
            ),
            (
                "every voter failed the fromRole gate",
                review_flow(None),
                vouched("p4", "review", "approved", &[]),
            ),
        ] {
            let derived = fold("review", &flow, &[atom]);
            assert_eq!(derived.state, "review", "{name}");
            assert!(derived.settled.is_empty(), "{name}");
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
        assert_eq!(
            rule_for(&flow, "approved").n,
            2,
            "the state's own rule wins"
        );
        assert_eq!(
            rule_for(&flow, "changes_requested").n,
            1,
            "a state without one inherits the flow's"
        );
    }
}
