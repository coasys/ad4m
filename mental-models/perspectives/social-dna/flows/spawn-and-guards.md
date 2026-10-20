# Flow spawn and guards

A flow instance starts in one of two ways. A client starts it by hand (`FlowInstance.start` in the SDK), or the executor **spawns** it after an interpretation run. Spawn happens when a newly created instance's subject class is in the flow's `inputTypes`. After the same run, the **flow evaluator** checks the **flow guards** of the states reachable from each affected flow instance. It writes a transition proposal for every flow guard that now holds. An optional **semantic check** asks the LLM to confirm a transition first.

**Why:** Interpretation writes the evidence (a scope, a deliverable), but an LLM often forgets to propose the transition that the evidence allows. A deterministic pass after every run keeps flows moving without trusting the LLM's own proposals. Spawn needs an explicit class match, so automatic starts stay opt-in; a catch-all flow would otherwise start on every instance.

**Parent:** [Flows](README.md)

<details><summary>How it works</summary>

`apply_ops_and_run_flow_passes` (interpretation `run.rs`), after the run's writes:

1. `created_bases_of(ops)`: the instances this run **created** (not updated).
2. `run_flow_spawn_pass`: for each created instance, `spawn_candidates` picks flows whose `inputTypes` contain one of the instance's class names (`subject_classes_of`; the SDK's `availableFlows` also matches by name) and that have no live instance on it. Zero-state flows, and flows with empty `inputTypes` or `"any"`, never auto-spawn. All candidates are minted with `mint_flow_instance` in one batch.
3. `run_flow_post_pass` over everything written plus everything the LLM was shown: the flow evaluator finds, per live instance, the reachable states whose `requires` model query now matches, and writes a `FlowTransitionProposal` as the acting DID.
4. If the target state has a `semanticCheck` hint, `flow_semantic_check` asks the LLM a yes/no question. Only a clear yes lets the proposal through; no, an unclear answer or an error drops it.

Spawn runs only after interpretation: an instance that app code creates does not auto-spawn a flow; the app calls `FlowInstance.start`.

Before the run, `flow_context` loads the live instances in the run's scope and renders an "Active flows" block into the prompt, with the next states and their guards in plain English.

</details>

<details><summary>Where in the code</summary>

- [perspectives/flow_spawn.rs](../../../../rust-executor/src/perspectives/flow_spawn.rs): `spawn_candidates`, `run_flow_spawn_pass`, `created_bases_of`, `initial_state_of`.
- [perspectives/flow_evaluator.rs](../../../../rust-executor/src/perspectives/flow_evaluator.rs): guard evaluation and proposal writing.
- [perspectives/flow_semantic_check.rs](../../../../rust-executor/src/perspectives/flow_semantic_check.rs): the yes/no LLM confirmation.
- [perspectives/flow_context/](../../../../rust-executor/src/perspectives/flow_context): `FlowContext`, `FlowInstanceRecord`, prompt rendering ([render.rs](../../../../rust-executor/src/perspectives/flow_context/render.rs)), loading ([loader.rs](../../../../rust-executor/src/perspectives/flow_context/loader.rs)).
- [perspectives/flow_classes.rs](../../../../rust-executor/src/perspectives/flow_classes.rs): `mint_flow_instance`.
- [perspectives/interpretation/run.rs](../../../../rust-executor/src/perspectives/interpretation/run.rs): `apply_ops_and_run_flow_passes`, `run_flow_post_pass`.
- SDK: `FlowInstance.start`, `availableFlows` in [core/src/perspectives/FlowInstance.ts](../../../../core/src/perspectives/FlowInstance.ts) and [PerspectiveProxy.ts](../../../../core/src/perspectives/PerspectiveProxy.ts).
- Design: [docs/flow-interpretation-hints-design.md](../../../../docs/flow-interpretation-hints-design.md).

</details>
