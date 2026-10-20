# Flows

A **flow** (`SHACLFlow`) is a state machine stored as SDNA: named states with values, transitions, optional **guards** (model queries that must match) and a **consensus rule** (how many distinct DIDs, optionally holding a role, must sign). A **flow instance** binds one flow to one **base**. No link stores the instance's current state; every executor **derives** it from the signed **transition proposals** and votes in the perspective.

**Why:** A group process, such as a task moving to "done", must reach the same answer on every executor without a coordinator. State derived from signed links is the same for anyone with the same links, also for a verifier outside the neighbourhood.

**Parent:** [Social DNA](../README.md)

## Parts
- [Flow consensus](consensus.md): proposals, votes, role gates, and the fold that derives a flow instance's state.
- [Flow spawn and guards](spawn-and-guards.md): how flow instances start (by hand or after interpretation), and how guards write proposals.
- [Flow receipts](receipts.md): the record a finished flow run leaves, and how any executor checks it.

<details><summary>How it works</summary>

```
SHACLFlow (SDNA, has_flow)              ── states, transitions, guards, consensusRule
   │
FlowInstance  ad4m://flow/instance/{id} ── subject = base (e.g. a Task)
   │
FlowTransitionProposal (signed links)   ── toState, proposer, evidence seal, votes
   │
fold(read set) → DerivedState { state, settled, contested }   (every replica, on demand)
   │
FlowReplicaState (SQLite, per agent)    ── cached state + fired marks, `flow-state-changed` event
```

RPC: `perspective.proposeFlowTransition`, `acceptFlowProposal`, `rejectFlowProposal`, `flowStates`, `flowFired`, `flowValidOutputs`, `mintFlowReceipt`, `verifyFlowReceipt`. Minting an instance or a proposal registers the hard-wired `FlowInstance` and `FlowTransitionProposal` classes first (`ensure_flow_model_classes`); so does `add_sdna` with type `flow`.

The derived state and fired marks are kept per agent in SQLite (`flow_replica_state`, `flow_fired_mark`), outside the graph. So clients do not model-query or subscribe to flow state: they read it with `flowStates` and `flowFired` and watch the `flow-state-changed` event.

</details>

<details><summary>Where in the code</summary>

- [perspectives/shacl_parser.rs](../../../../rust-executor/src/perspectives/shacl_parser.rs): `SHACLFlow`, `FlowState`, `FlowTransition`, `ConsensusRule`, `parse_flow_to_links`, `parse_flow_from_links`, `check_consensus_rule_links`.
- [perspectives/flow_classes.rs](../../../../rust-executor/src/perspectives/flow_classes.rs): hard-wired `FlowInstance` / `FlowTransitionProposal`, `mint_flow_instance`, `ensure_flow_model_classes`.
- [perspectives/flow_instance/](../../../../rust-executor/src/perspectives/flow_instance): the consensus engine; read its [mod.rs](../../../../rust-executor/src/perspectives/flow_instance/mod.rs) first.
- [api/perspectives_ws.rs](../../../../rust-executor/src/api/perspectives_ws.rs): flow RPC handlers. MCP: [mcp/tools/flows.rs](../../../../rust-executor/src/mcp/tools/flows.rs).
- SDK: [core/src/shacl/SHACLFlow.ts](../../../../core/src/shacl/SHACLFlow.ts), [core/src/perspectives/FlowInstance.ts](../../../../core/src/perspectives/FlowInstance.ts), [core/src/perspectives/FlowModels.ts](../../../../core/src/perspectives/FlowModels.ts).
- Docs: [docs-src/pages/flows.mdx](../../../../docs-src/pages/flows.mdx), [writing-a-flow.mdx](../../../../docs-src/pages/developer-guides/writing-a-flow.mdx), [docs/flow-interpretation-hints-design.md](../../../../docs/flow-interpretation-hints-design.md).

</details>
