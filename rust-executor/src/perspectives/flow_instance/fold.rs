//! **The engine's entire belief system.** Everything else in this directory
//! exists to hand [`fold`] honest inputs.
//!
//! The state of a flow is not stored anywhere. It is computed, every time,
//! from the signed links that exist on the graph right now: walk from the
//! flow definition's first state and take, out of each state, whichever
//! declared edge reached quorum earliest; where no edge out of a state has
//! settled, the walk stops and that state is the answer. It also stops without
//! choosing when the earliest edge would foreclose another that is equally
//! settled — see *Ordering and time*.
//!
//! Three properties follow, and they are the reason this file is pure:
//!
//! - **Deterministic.** Two replicas holding the same links derive the same
//!   state, whatever order their stores returned them in — every choice here
//!   is by an explicit sort key.
//! - **Re-verifiable off-perspective.** [`fold`] takes plain data and does no
//!   I/O, so a verifier outside the neighbourhood can re-run it over a
//!   serialised [`ReadSet`](super::ReadSet) and reach the same verdict. How
//!   much that verdict is worth differs by half: the proposals and votes are
//!   signed links the verifier re-checks itself, while the `fromRole`
//!   eligibility is a verdict this replica computed — see
//!   [`ReadSet`](super::ReadSet).
//! - **A function of the links present now.** Delete a settled vote and the
//!   fold recomputes without it, so the flow stands where it stood before
//!   that vote. The graph is the truth and the state follows it.
//!
//! Quorum belongs to an **edge**, not to a proposal. Twin proposals for one
//! edge arrive whenever two replicas mint concurrently — each dedupes only
//! against the proposals it has seen — and asking "is *this proposal*
//! settled?" would strand such a flow at genesis forever, while asking "did
//! the *edge* collect `n` distinct voters?" resolves it.
//!
//! What that is *not*, in this PR: the deliberate two-bot path. The mint pass
//! dedupes on `(instance, to_state, evidence_hash)` with no proposer
//! dimension, so a bot that already sees a peer's proposal for the edge
//! declines to mint a twin and must vote instead — and voting is
//! [`super::accept`], whose RPC callers land in #968.
//!
//! ## Ordering and time
//!
//! Each of these properties is a direct consequence of the code; reviewers who
//! want to verify them should start at [`settle_edge`].
//!
//! **An agent can only back-date their own vote's timestamp.** [`Vote::at`] for
//! the proposer comes from the earliest timestamp among the proposer's own
//! signed links ([`super::atom::earliest_proposer_timestamp`]). For subsequent
//! voters, it is the `timestamp` field of the `acceptedBy` link they
//! themselves signed ([`super::atom::valid_votes`]). A link only contributes a
//! timestamp to the DID that signed it.
//!
//! **Settlement time is the n-th distinct eligible voter's timestamp.** Votes
//! are sorted `(at, did, uri)` ascending and counted until the n-th distinct
//! DID; that voter's `at` becomes `nth_at`. A single colluding voter can shift
//! `nth_at` earlier by back-dating their own vote far enough to change their
//! position in the sort — moving to an earlier slot makes a different (later)
//! voter land at position n, pulling `nth_at` down. The floor below is
//! `after` (see next property).
//!
//! **`settled_at = nth_at.max(after)` floors every edge *after the first* at
//! the moment the walk arrived at its `from_state`.** `after` is the
//! `settled_at` of the most recently taken edge. Once that is a real
//! timestamp, a vote back-dated to 1970 settles the edge at the walk's
//! arrival, not at 1970 — the earliest any such edge can settle is when the
//! walk came to its starting state.
//!
//! **The floor is vacuous at genesis, and that is a hole.** `after` is `""`
//! on the first edge, and `""` is a sort sentinel, not an arrival time: every
//! non-empty string wins the `max`. A genesis vote back-dated to 1970 settles
//! at 1970. Nothing bounds the first edge backward, which is what makes the
//! residual below unbounded rather than merely narrow.
//!
//! **Back-dating cannot manufacture a quorum.** Quorum is a count of distinct
//! eligible DIDs, determined by deduplication in [`settle_edge`]. Timestamps
//! affect `settled_at` only; the question "did n distinct eligible voters
//! sign?" is timestamp-independent.
//!
//! **An irreversible race is refused, not won.** Back-dating cannot *undercut*
//! the floor — `max` forbids `settled_at < after` — but it can *meet* it, and
//! at genesis, where the floor is vacuous, meeting it means "any timestamp at
//! all". So a colluding quorum, or a lone proposer under an `{n:1}` rule, could
//! beat every honest wall-clock vote out of the starting state. Rather than
//! trust the clock there, [`settle`] stops the walk and reports [`Contention`].
//!
//! It does so **only when the earliest edge forecloses the others** — when a
//! losing target is no longer reachable from the winner's target in the
//! declared transition graph. Where the loser stays reachable, as in any cycle,
//! the timestamp decided *order* and not *outcome*: the losing edge fires on a
//! later visit exactly as it always did, so there is nothing to refuse. A
//! branch into two terminal states contends; `review ⇄ changes_requested` with
//! a later `approved` does not.
//!
//! **RESIDUAL — a stall is still an outcome an attacker can choose.** On a
//! genuinely irreversible branch, an agent eligible under the rule can hold the
//! flow there by making a second edge quorate; under `{n:1}` one agent
//! suffices. Denial is a weaker win than steering — it is inert until someone
//! resolves it, and the [`Contention`] names the atoms and voters that caused
//! it — but it is not nothing, and no part of the engine prevents it.
//!
//! **Flows can avoid the branch problem by construction**, which is cheaper
//! than anything the engine can do: never branch at the genesis state (one
//! non-branching step gives every later branch a real floor); prefer branches
//! gated on data over branches decided by competing votes; and where competing
//! votes are the point, draw both edges from the same role with `n` over half
//! its members, so any two quorums intersect and a contested branch is provable
//! equivocation rather than a race.
//!
//! **DEFERRED — a causal floor would bound collusion without clocks.** If a
//! proposal were required to cite the settled-edge atoms it observed at mint
//! time, back-dating past a known-later event would be self-contradicting
//! (Lamport-style: "I saw atom A, but I voted before A existed"). With
//! contention refused this no longer guards the choice of edge, but it would
//! narrow the stall above by making a back-dated second proposal detectable.

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

/// Why a walk stopped without taking an edge it could have taken.
///
/// More than one declared edge out of the same state reached quorum. Which of
/// them "happened first" is decided by [`SettledEdge::settled_at`], and that is
/// a self-asserted timestamp — see the residual under *Ordering and time*. So
/// the one case where the clock decides an outcome is the one case where the
/// engine declines to use it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contention {
    /// The state the walk stopped in. Equal to [`DerivedState::state`].
    pub from_state: String,
    /// Every settled edge out of `from_state`, ranked as the earliest-wins rule
    /// would have ranked them: `candidates[0]` is the edge that would have been
    /// taken, so a human resolving this by hand sees both the choice that was
    /// declined and what it beat.
    pub candidates: Vec<SettledEdge>,
}

/// The authoritative state of a flow instance, and the chain that produced it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedState {
    pub state: String,
    pub settled: Vec<SettledEdge>,
    /// Set when the walk stopped because `state` had more than one settled edge
    /// out of it; `None` when it stopped for the ordinary reason — no edge out
    /// of `state` has reached quorum yet.
    ///
    /// A caller that reads a stalled flow as "waiting for votes" has to check
    /// this. A contested flow is not waiting for anything: more votes cannot
    /// move it, because the obstacle is that two edges already carry a quorum.
    /// Anything that pays out on a completed flow must refuse a derivation with
    /// `contested.is_some()`.
    pub contested: Option<Contention>,
}

/// What one visit to a state found.
enum Settlement {
    /// No edge out of this state has reached quorum. The walk ends normally.
    None,
    /// Exactly one edge has settled. The walk takes it.
    One(Box<SettledEdge>),
    /// More than one has settled. The walk stops without choosing.
    Contested(Contention),
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

/// The state of a flow: walk from `genesis`, taking the one settled declared
/// edge out of each state, until no edge out of it has settled — or until a
/// state has more than one, which stops the walk without choosing.
///
/// Terminates because every settled edge consumes at least one atom (a
/// quorum needs at least one voter, and a voter needs an atom to vote on)
/// and `atoms` is finite.
pub fn fold(genesis: &str, flow: &SHACLFlow, atoms: &[VouchedAtom]) -> DerivedState {
    let mut state = genesis.to_string();
    let mut settled_at = String::new(); // "" sorts before every timestamp
    let mut settled: Vec<SettledEdge> = Vec::new();
    let mut contested = None;
    loop {
        match settle(&state, &settled_at, flow, atoms, &settled) {
            Settlement::None => break,
            Settlement::One(edge) => {
                state = edge.to_state.clone();
                settled_at = edge.settled_at.clone();
                settled.push(*edge);
            }
            Settlement::Contested(contention) => {
                contested = Some(contention);
                break;
            }
        }
    }
    DerivedState {
        state,
        settled,
        contested,
    }
}

/// The one declared edge out of `state` that has settled — or the fact that
/// several have.
///
/// Votes are not filtered by `after`: an edge that was already quorate when
/// the walk arrived is settled, and `after` only floors the moment it counts
/// as having settled (see [`settle_edge`]). Candidates are ranked earliest
/// first, ties broken by target state and then by first atom URI, so two
/// replicas agree on both the ranking and on whether the state is contested.
///
/// **Several settled edges stop the walk only when the choice is
/// irreversible.** The earliest-wins rule reads `settled_at`, which an agent
/// asserts about its own vote, so under a branch a colluding quorum could
/// back-date its way past an honest one. But refusing on *any* second quorate
/// edge would break ordinary flows: in `review ⇄ changes_requested` with a
/// third edge to `approved`, a request and an approval are both quorate at the
/// first visit, and the walk is supposed to take the request now and the
/// approval after the cycle returns.
///
/// The distinction is whether taking the earliest edge **forecloses** the
/// others. If every losing target is still reachable from the winner's target
/// in the declared transition graph, the loser is not denied — it fires on a
/// later visit, exactly as before, and the timestamp decided order rather than
/// outcome. If a losing target is *not* reachable, the clock is picking a
/// branch nothing can undo, and that is the case this engine refuses.
///
/// So `{approved, rejected}` out of one state contends, while a cycle that
/// comes back around does not. The residual is that an attacker can still
/// force a stall on a genuinely irreversible branch — see *Ordering and time*.
fn settle(
    state: &str,
    after: &str,
    flow: &SHACLFlow,
    atoms: &[VouchedAtom],
    consumed: &[SettledEdge],
) -> Settlement {
    let mut candidates: Vec<SettledEdge> = flow
        .transitions
        .iter()
        .filter(|t| t.from_state == state)
        .filter_map(|t| settle_edge(state, &t.to_state, after, flow, atoms, consumed))
        .collect();
    candidates.sort_by_key(|e| {
        (
            e.settled_at.clone(),
            e.to_state.clone(),
            e.atom_uris.first().cloned().unwrap_or_default(),
        )
    });
    if candidates.is_empty() {
        return Settlement::None;
    }
    let winner_target = candidates[0].to_state.clone();
    let forecloses = candidates[1..]
        .iter()
        .any(|loser| !reachable(flow, &winner_target, &loser.to_state));
    if forecloses {
        Settlement::Contested(Contention {
            from_state: state.to_string(),
            candidates,
        })
    } else {
        Settlement::One(Box::new(candidates.remove(0)))
    }
}

/// Whether `to` can still be entered once the flow is in `from`, following
/// declared transitions only.
///
/// This asks about the flow's shape, not about votes: an edge that is quorate
/// now but loses a race is only *denied* if the state it leads to drops out of
/// the graph reachable from the winner. A state is reachable from itself, so
/// two transitions sharing a target never contend.
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
}
