# Interpretation overlay

An **interpretation overlay** (`InterpretationOverlay`) is a second instance on the same base as an instance the LLM wrote. It records how the instance was written (`kind`: create or update) and which interpretation run wrote it. It holds the LLM's value for each affected property as a parallel `ad4m://interp/inferred/<predicate>` link. Each base has exactly one overlay, updated in place across runs.

**Why:** LLM output must not silently overwrite what a person wrote. The overlay is the evidence that the LLM owns a value: the engine overwrites a value only when the overlay shows the LLM set it and nobody changed it since. The parallel `inferred/` links keep the real property links clean, so readers that do not know about overlays see a plain instance. The link author's DID says who ran the pass, so the run stores no agent field.

**Parent:** [Interpretation](README.md)

<details><summary>How it works</summary>

Update routing (the "human divergence gate", `gate.rs`), per proposed value:

| Situation | Action |
|---|---|
| real property unset | write it, record `inferred/<p>` |
| overlay exists, real value = last `inferred/<p>` | overwrite, and update `inferred/<p>` |
| overlay exists, real value ≠ `inferred/<p>` (a person changed it) | keep the real value, stage the proposal in `inferred/<p>` |
| no overlay (human or seed data) | never overwrite; create an overlay with the proposal as a suggestion |

Resolution (`perspective.acceptInterpretation`, `rejectInterpretation`, per instance or per property):

- **Accept**: the LLM value becomes the real value and the overlay is deleted. With no overlay, later runs treat the base as human-owned.
- **Reject**: drop the suggestion. Rejecting a create removes the suggested instance; rejecting an update drops the overlay and keeps the real value.

`perspective.interpretationOverlays` lists open overlays.

</details>

<details><summary>Where in the code</summary>

- [interpretation/overlay/](../../../../rust-executor/src/perspectives/interpretation/overlay): [mod.rs](../../../../rust-executor/src/perspectives/interpretation/overlay/mod.rs) (`apply_with_overlay`, predicate vocabulary), [gate.rs](../../../../rust-executor/src/perspectives/interpretation/overlay/gate.rs), [write.rs](../../../../rust-executor/src/perspectives/interpretation/overlay/write.rs), [accept.rs](../../../../rust-executor/src/perspectives/interpretation/overlay/accept.rs), [classes.rs](../../../../rust-executor/src/perspectives/interpretation/overlay/classes.rs).
- [hardwired_sdna/interpretation_overlay.json](../../../../rust-executor/src/perspectives/hardwired_sdna/interpretation_overlay.json), [interpretation_run.json](../../../../rust-executor/src/perspectives/hardwired_sdna/interpretation_run.json).
- Design: [docs/interpretation-provenance-design.md](../../../../docs/interpretation-provenance-design.md).

</details>
