# Social DNA

**Social DNA** (SDNA) is the set of definitions, stored as shared links in a perspective, that give its links meaning. **Subject classes** are SHACL shapes that say which links make an instance. **Flows** are state machines that a group moves forward by signed votes. **Interpretation** is an LLM pass that turns text into instances, driven by hints on the classes.

**Why:** Schema in the graph travels with the data. An agent who joins a neighbourhood receives the classes and flows with the links, so every executor reads the same definitions. SHACL is a W3C standard that RDF tools can read.

**Parent:** [Perspectives](../README.md)

## Parts
- [Subject classes](subject-classes.md): what a shape holds, how a class is registered, and how actions create and update instances.
- [Flows](flows/README.md): flow definitions, flow instances, and how each executor derives their state from signed proposals and votes.
- [Interpretation](interpretation/README.md): the LLM pass that writes instances from text, its provenance records, and the auto-processor that runs it.

<details><summary>How it works</summary>

Subject classes (`perspective.addSdna(name, code, "subject_class", shaclJson)`, sent by the SDK's `ensureSubjectClass`):

1. `add_sdna` parses the SHACL JSON to links (`parse_shacl_to_links`) before it touches the store.
2. Under `sdna_change_mutex` it writes `ad4m://self → ad4m://has_subject_class → literal:<name>`, the code under `ad4m://sdna`, and the SHACL links, all `Shared`.
3. Readers parse the links back on demand: `get_shape` → `ModelShape` (cached).

Flows (`PerspectiveProxy.addFlow(name, flow)` in the SDK): the SDK writes `SHACLFlow.toLinks()` plus `ad4m://self → ad4m://has_flow` and `ad4m://flow_uri` itself through `linkMutations`. It sends only the difference to the stored definition. Readers use `parse_flow_from_links` → `SHACLFlow`.

The executor also registers **hard-wired classes** it needs itself (flow instances and proposals, interpretation runs and overlays, auto-processors, `TextContent`) from JSON files in `hardwired_sdna/`.

</details>

<details><summary>Where in the code</summary>

- [perspectives/shacl_parser.rs](../../../rust-executor/src/perspectives/shacl_parser.rs): `SHACLShape`, `SHACLFlow`, `AD4MAction`, `parse_shacl_to_links`, `parse_flow_to_links`, `parse_flow_from_links`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `SdnaType`, `add_sdna`, `add_sdna_batch`, `add_sdna_inner`, `get_subject_classes_from_shacl`.
- [perspectives/hardwired_class.rs](../../../rust-executor/src/perspectives/hardwired_class.rs) and [perspectives/hardwired_sdna/](../../../rust-executor/src/perspectives/hardwired_sdna): built-in classes.
- SDK: [core/src/shacl/](../../../core/src/shacl) (`SHACLShape`, `SHACLFlow`), [core/src/model/](../../../core/src/model) (decorators, `shacl-gen.ts`), `addFlow` in [PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts).
- Docs: [SHACL_SDNA_ARCHITECTURE.md](../../../SHACL_SDNA_ARCHITECTURE.md), [docs-src/pages/social-dna.mdx](../../../docs-src/pages/social-dna.mdx).

</details>
