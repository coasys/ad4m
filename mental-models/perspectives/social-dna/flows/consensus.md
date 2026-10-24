# Flow consensus

A flow instance's **derived state** is the result of a pure **fold** over a **read set**: the signed links of every transition proposal on the instance, plus the grant and revocation links behind each voter's role. The fold walks from the flow's first state (genesis). Out of each state it takes the declared edge whose distinct voters reached the consensus rule's quorum first. Nothing else moves the state.

**Why:** Every executor must derive the same state from the same links, with no coordinator and no trusted cache. A pure fold (no I/O, deterministic sort keys) gives that. It also lets a verifier outside the neighbourhood run it again over a serialized read set. The rule is "a replica may only refuse its own action". A voter checks the evidence before it signs, so the fold never runs guards again, and history does not unwind when cited content changes later.

**Parent:** [Flows](README.md)

<details><summary>How it works</summary>

1. **Propose** (`perspective.proposeFlowTransition`, `propose.rs`): `toState` must be reachable from the derived state, and a `requires` guard must hold on this replica. The executor computes an **evidence seal** over the cited content and writes a signed `FlowTransitionProposal`, or co-signs the open proposal for the same edge.
2. **Accept** (`accept.rs`): re-compute the seal on this replica's graph. If the content changed, decline and write nothing. Otherwise add this DID's signed vote, then run a pass. A vote into a terminal state also re-checks the outputs commitment ([Flow receipts](receipts.md)).
3. **Reject** (`rejectFlowProposal`): delete only the links this DID signed (its vote or its own proposal). The state can move back.
4. **Read set** (`FlowInstance::read_set`): load the proposal links (`atom::load_proposal_links`), and for each role-gated target state, each candidate's role instances with their grant and tombstone links (`roles::resolve_role_grants`). A store error, or a role query that cannot tell DIDs apart, aborts the read: the state stays unknown. The read set does not filter by link status (`read_set` clears `status`): a vote or role grant written as a local link counts on this executor and does not reach peers.
5. **Fold** (`fold_read_set` → `fold`): build grant windows, keep a vote only if its DID held the role **at the vote's own timestamp**, pool votes per edge across twin proposals, walk from genesis. When two edges out of one state are quorate, the fold takes the earliest; it stalls (contested) only when taking it would make another quorate edge's target unreachable. Result: `DerivedState { state, settled, contested }`.
6. **Pass** (`pass.rs`): record the derived state and new fired marks per agent in `FlowReplicaState` (SQLite, outside the graph) and publish `flow-state-changed`. Passes run after a local vote, at load (`flow_pass_at_load`), and debounced when peers' flow links sync in (`trigger.rs`).

</details>

<details><summary>Where in the code</summary>

- [flow_instance/mod.rs](../../../../rust-executor/src/perspectives/flow_instance/mod.rs): `FlowInstance`, `read_set`, pipeline diagram.
- [flow_instance/fold/](../../../../rust-executor/src/perspectives/flow_instance/fold): `fold`, `settle`, `DerivedState`, `Contention`, ordering and time rules.
- [flow_instance/atom/](../../../../rust-executor/src/perspectives/flow_instance/atom): `TransitionAtom`, what counts as a proposal and a vote.
- [flow_instance/roles/](../../../../rust-executor/src/perspectives/flow_instance/roles): `RoleGrantEvidence`, `RoleGrantWindow`, `eligible_votes`.
- [flow_instance/read_set.rs](../../../../rust-executor/src/perspectives/flow_instance/read_set.rs), [propose.rs](../../../../rust-executor/src/perspectives/flow_instance/propose.rs), [accept.rs](../../../../rust-executor/src/perspectives/flow_instance/accept.rs), [grant.rs](../../../../rust-executor/src/perspectives/flow_instance/grant.rs), [pass.rs](../../../../rust-executor/src/perspectives/flow_instance/pass.rs), [replica_state.rs](../../../../rust-executor/src/perspectives/flow_instance/replica_state.rs), [trigger.rs](../../../../rust-executor/src/perspectives/flow_instance/trigger.rs), [time.rs](../../../../rust-executor/src/perspectives/flow_instance/time.rs).
- End-to-end tests: [perspectives/flow_instance_e2e/](../../../../rust-executor/src/perspectives/flow_instance_e2e).
- Docs: [docs-src/pages/flows.mdx](../../../../docs-src/pages/flows.mdx) (proposing, voting, roles).

</details>
