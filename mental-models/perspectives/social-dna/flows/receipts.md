# Flow receipts

A **flow receipt** (`FlowReceipt`) is a self-contained, content-addressed record that a flow instance reached a terminal state. It carries the signed read set, the run's outputs and the material each vote's evidence seal covered. Any executor can run the fold again and check the claim without trusting the minter (`perspective.mintFlowReceipt`, `perspective.verifyFlowReceipt`). Receipts stored in a perspective answer "is this instance a valid output of flow F?" (`producedByFlow`).

**Why:** A finished flow can be the basis for something outside the run, such as a role granted by another flow. The receipt makes "this group settled this run" checkable later, on another executor, with the minter gone. The receipt does not carry the flow definition. The verifier folds against the definition in its own catalogue, so the rule that counts is always the verifier's.

**Parent:** [Flows](README.md)

<details><summary>How it works</summary>

```
FlowReceipt
├── flow_uri, flow_dna_hash   which social DNA the run was settled under (hash is set by the minter)
├── terminal_state            derived by the fold at mint time, never asserted
├── outputs                   (class, id, content) per output; must hash to the final edge's signed outputs_hash
├── read_set                  the signed links the fold used (the proof)
└── evidence_preimage         what each counted vote's seal was taken over
```

1. **Mint**: run `fold_read_set` on this replica; refuse unless the derived state is terminal and not contested; refuse outputs that do not hash to the commitment every counted vote signed.
2. **Verify**: re-verify every signature, rebuild role windows from the carried grant links, re-run the fold against the verifier's own flow definition, re-hash the outputs. The result is a `ReceiptVerdict`.
3. Voters bind outputs before they sign: each voter loads every named output through its class on its own replica and recomputes `outputs_hash` (`check_outputs_commitment`).
4. **Consume** (`produced.rs`): an instance is a valid output of flow F when some receipt in the perspective verifies, names F and the instance, and (if asked) settled in state S. Three readers use this: the `producedByFlow` role gate (`grant.rs`), `perspective.flowValidOutputs`, and the model-query filter `where: { producedByFlow }`. `load_flow_receipts` returns an error, not a partial answer, when a flow has more than `MAX_FLOW_RECEIPTS` (256) receipts to read.

</details>

<details><summary>Where in the code</summary>

- [flow_instance/receipt/](../../../../rust-executor/src/perspectives/flow_instance/receipt): [mod.rs](../../../../rust-executor/src/perspectives/flow_instance/receipt/mod.rs) (`FlowReceipt`), [mint.rs](../../../../rust-executor/src/perspectives/flow_instance/receipt/mint.rs), [commitment.rs](../../../../rust-executor/src/perspectives/flow_instance/receipt/commitment.rs), [dna.rs](../../../../rust-executor/src/perspectives/flow_instance/receipt/dna.rs).
- [flow_instance/verify/](../../../../rust-executor/src/perspectives/flow_instance/verify): `verify_receipt`, `ReceiptVerdict` ([verdict.rs](../../../../rust-executor/src/perspectives/flow_instance/verify/verdict.rs)).
- [flow_instance/produced.rs](../../../../rust-executor/src/perspectives/flow_instance/produced.rs): `load_flow_receipts`, "valid output of flow F". [flow_instance/grant.rs](../../../../rust-executor/src/perspectives/flow_instance/grant.rs): `producedByFlow` on a role.
- [api/perspectives_ws.rs](../../../../rust-executor/src/api/perspectives_ws.rs): `mintFlowReceipt`, `verifyFlowReceipt`, `flowValidOutputs` handlers.
- Docs: "Receipts" in [docs-src/pages/flows.mdx](../../../../docs-src/pages/flows.mdx).

</details>
