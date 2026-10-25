# Interpretation

**Interpretation** is an executor pass that sends a transcript (a list of text turns) and the perspective's subject classes to an LLM. It writes what the LLM extracts as instances: it creates new ones, updates known ones and links relations. An **interpretation run** (`InterpretationRun`) records each pass. Each instance written gets an **interpretation overlay** that records what the LLM inferred, so a person can accept or reject it.

**Why:** Apps such as group chat and calls produce text, while the perspective holds typed data. Extraction in the executor uses the same subject classes and write path as app code, so its output is ordinary data that every app can read. It runs next to the data, so it can read existing instances and avoid duplicates.

**Parent:** [Social DNA](../README.md)

## Parts
- [Interpretation overlay](overlay.md): the record of each LLM write, the rule that protects human edits, and the accept or reject step.
- [Auto-processor](auto-processor.md): how the executor runs interpretation by itself on new messages, and how it elects one member to run each batch.

<details><summary>How it works</summary>

`perspective.runInterpretation(uuid, transcript, basePrefix, classes?, existingScope?, mintScope?)`:

1. Read existing instances per class (`existing_instance_context`), keyed by each class's identity property.
2. Build one prompt from those, each class's `interpretationHint`, fields and relations, any active flow context, and the transcript (`build_interpretation_input`).
3. Call the LLM through the AI service's interpretation task (`ensure_interpretation_task_for_model`; see [AI service](../../../agents/executor/services/ai.md)). Make up to 5 attempts when the JSON does not parse (`retry_interpretation_parse`).
4. Drop proposals already present, then plan the writes: an `id` means update, no `id` means create; relation values resolve to existing ids or `new:<Class>:<n>` refs (`plan_interpretation_ops_with_context`).
5. Drop no-op updates; apply all writes in one batch with overlays (`apply_with_overlay`).
6. Run flow spawn and the flow evaluator ([Flow spawn and guards](../flows/spawn-and-guards.md)).

The RPC answers after all six steps: `run_interpretation` awaits `apply_ops_and_run_flow_passes` and returns its flow proposals with the result.

`perspective.runInterpretationWithHarness` replaces step 3 with a tool-calling loop (`ai_service::harness`) in which the LLM reads the graph through the MCP tools. Both paths share steps 4–6.

</details>

<details><summary>Where in the code</summary>

- [perspectives/interpretation/](../../../../rust-executor/src/perspectives/interpretation): [mod.rs](../../../../rust-executor/src/perspectives/interpretation/mod.rs) (data flow), [run.rs](../../../../rust-executor/src/perspectives/interpretation/run.rs) (`run_interpretation`, `apply_ops_and_run_flow_passes`), [prompt.rs](../../../../rust-executor/src/perspectives/interpretation/prompt.rs), [parse.rs](../../../../rust-executor/src/perspectives/interpretation/parse.rs), [dedup.rs](../../../../rust-executor/src/perspectives/interpretation/dedup.rs), [graph/](../../../../rust-executor/src/perspectives/interpretation/graph), [types.rs](../../../../rust-executor/src/perspectives/interpretation/types.rs).
- [api/types.rs](../../../../rust-executor/src/api/types.rs): `RunInterpretationRequest`.
- [ai_service/harness/](../../../../rust-executor/src/ai_service/harness): the tool-calling loop.
- Built-in classes: [hardwired_sdna/interpretation_run.json](../../../../rust-executor/src/perspectives/hardwired_sdna/interpretation_run.json), [interpretation_overlay.json](../../../../rust-executor/src/perspectives/hardwired_sdna/interpretation_overlay.json).
- SDK: [core/src/perspectives/InterpretationModels.ts](../../../../core/src/perspectives/InterpretationModels.ts).
- Tests: [interpretation_e2e.rs](../../../../rust-executor/src/perspectives/interpretation_e2e.rs), [interpretation_test_support.rs](../../../../rust-executor/src/perspectives/interpretation_test_support.rs).

</details>
