# Social DNA

**Social DNA** (SDNA) is the set of definitions, stored as shared links in a perspective, that give its links meaning. **Subject classes** are SHACL shapes that say which links make an instance. **Flows** are state machines that a group moves forward by signed votes. **Interpretation** is an LLM pass that turns text into instances, driven by hints on the classes.

**Why:** Schema in the graph travels with the data. An agent who joins a neighbourhood receives the classes and flows with the links, so every executor reads the same definitions. SHACL is a W3C standard that RDF tools can read.

**Parent:** [Perspectives](../README.md)

## Parts
- [Subject classes](subject-classes.md): what a shape holds, how a class is registered, and how actions create and update instances.
- [Flows](flows/README.md): flow definitions, flow instances, and how each executor derives their state from signed proposals and votes.
- [Interpretation](interpretation/README.md): the LLM pass that writes instances from text, its provenance records, and the auto-processor that runs it.

<details><summary>How it works</summary>

Subject classes (the SDK's `Ad4mModel.register` / `registerAll` → `perspective.registerShapes`, read back with `perspective.shapes`):

1. `register_shapes` turns each `SHACLShape` into its links (`shape_links`) before it touches the store. A class is keyed by its target class IRI, not by a name link.
2. Registration is a diff, not a rewrite: it compares the shape's own links with the ones stored. Equal sets write nothing (`RegisterResult.unchanged`); otherwise one `Shared` diff under `sdna_change_mutex` adds the missing links and removes the stale own links.
3. Readers parse the links back on demand: `get_shape` → `ModelShape` (cached). `shapes` reads the registry (`ShapeSummary` per class: name, shape, hash of its canonical JSON).

Flows (`PerspectiveProxy.addFlow(name, flow)` in the SDK): the SDK writes `SHACLFlow.toLinks()` plus `ad4m://self → ad4m://has_flow` and `ad4m://flow_uri` itself through `linkMutations`. It sends only the difference to the stored definition. Readers use `parse_flow_from_links` → `SHACLFlow`.

The executor also registers six **hard-wired classes** it needs itself (`FlowInstance`, `FlowTransitionProposal`, `AutoProcessor`, `ProcessingClaim`, `InterpretationRun`, `InterpretationOverlay`) from JSON files in `hardwired_sdna/`, through `register_shapes` (`ensure_subject_classes` in `hardwired_class.rs`). The built-in abstract classes `TextContent`, `Mentioning` and `Container` come from the same directory, but no perspective stores them (`model_query/builtin_classes.rs`).

</details>

<details><summary>Where in the code</summary>

- [perspectives/shacl_parser.rs](../../../rust-executor/src/perspectives/shacl_parser.rs): `SHACLShape`, `SHACLFlow`, `AD4MAction`, `shape_links`, `shape_from_links`, `is_own_link`, `parse_shacl_to_links`, `parse_flow_to_links`, `parse_flow_from_links`.
- [perspectives/perspective_instance/shapes.rs](../../../rust-executor/src/perspectives/perspective_instance/shapes.rs): `register_shapes`, `register_shape_json`, `shapes`, `class_names`, `class_keys`, `shape_registry`, `RegisterResult`, `ShapeSummary`.
- [perspectives/hardwired_class.rs](../../../rust-executor/src/perspectives/hardwired_class.rs) and [perspectives/hardwired_sdna/](../../../rust-executor/src/perspectives/hardwired_sdna): `ensure_subject_classes`, `ensure_before_write`, `HardwiredClass`, the hard-wired classes.
- SDK: [core/src/shacl/](../../../core/src/shacl) (`SHACLShape`, `SHACLFlow`), [core/src/model/](../../../core/src/model) (decorators, `shacl-gen.ts`), `addFlow` in [PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts).
- Docs: [SHACL_SDNA_ARCHITECTURE.md](../../../SHACL_SDNA_ARCHITECTURE.md), [docs-src/pages/social-dna.mdx](../../../docs-src/pages/social-dna.mdx).

</details>
