# Subject classes

A **subject class** is a named SHACL node shape stored in a perspective. It lists properties and relations (each with a predicate, a datatype or target class, and a cardinality) and **actions**: JSON command lists for the constructor, the destructor and each property's setter, adder and remover. An **instance** is a base whose links satisfy the shape. The executor writes instances by running the actions and reads them with model queries.

**Why:** One stored definition serves every writer: the SDK, MCP tools, interpretation and apps on other executors produce the same links for the same class. The links present decide conformance, not a stored type tag, so any data with the right links is an instance. Actions are data, not code, so the executor runs them without loading app code.

**Parent:** [Social DNA](README.md)

<details><summary>How it works</summary>

Registration:

1. An app's [model class](../../agents/interfaces/typescript-sdk/model-classes.md) generates the SHACL and sends one or more shapes with `perspective.registerShapes` (`register_shapes`, [Social DNA](README.md)). A class is keyed by its **target class IRI**, not by a name: two classes may share a name (`class_name_of` their IRIs), and the bare name then reads the least IRI.
2. `register_shapes` compares the own links each shape gives with the ones stored, as triples. Equal sets write nothing (`unchanged` in `RegisterResult`); the rest get one `Shared` diff under `sdna_change_mutex`. A call whose classes all match the cached registry answers without the mutex and without reading the stored shapes, so registering on every start is cheap. It refuses, writing nothing: two shapes for one target class, a shape URI another class holds, a shape URI not ending in `{Name}Shape`.
3. The write syncs to the neighbourhood like any shared links. SHACL links that arrive by sync get the same treatment: the shape cache and registry are dropped inside the store write ([Link write path](../links/write-path.md)).

Stored shape (links): `<class> rdf://type ad4m://SubjectClass`, `<class> ad4m://shape <shape>`, `<shape> sh://property <propShape>`, plus action literals under `ad4m://constructor`, `ad4m://destructor`, `ad4m://setter`, `ad4m://adder`, `ad4m://remover`, and flags such as `ad4m://local` and `ad4m://interpretation_hint`.

Writes:

| RPC | Runs |
|---|---|
| `perspective.createSubject` | constructor actions, then the setter of each initial value (`create_subject`) |
| `perspective.updateSubject` | setters, `collectionSetter` for arrays (`update_subject`) |
| `perspective.removeSubject` | destructor actions |
| `perspective.executeCommands` | any command list (`execute_commands`) |
| `perspective.instance*` | the same writes checked against the shape first (`perspectives/instances/`), shared with the MCP `instance_*` tools |

`execute_commands` turns each `Command` (`addLink`, `removeLink`, `setSingleTarget`, `collectionSetter`) into link writes; a command marked `local` writes a local link. A `collectionSetter` on a relation with a declared ordering strategy also writes ordering entries on the parent (`apply_collection_ordering` in [perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs), entries under [ordering/](../../../rust-executor/src/perspectives/ordering)).

Reads go through [Model queries](../querying/model-queries.md). `perspective.subjectClassesOf` lists the classes a URI conforms to. An **abstract class** (for example the built-ins `ad4m://TextContent` and `ad4m://Mentioning`) reads several concrete classes. A write through it goes to the instance's own classes; creating one is refused.

</details>

<details><summary>Where in the code</summary>

- [perspectives/shacl_parser.rs](../../../rust-executor/src/perspectives/shacl_parser.rs): `SHACLShape`, `PropertyShape`, `AD4MAction`, `shape_links`, `shape_from_links`, `is_own_link`, `parse_shacl_to_links`, `class_name_of`.
- [perspectives/perspective_instance/shapes.rs](../../../rust-executor/src/perspectives/perspective_instance/shapes.rs): `register_shapes`, `shapes`, `RegisterResult`, `ShapeSummary`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `Command`, `Action`, `create_subject`, `update_subject`, `execute_commands`, `apply_collection_ordering`, `get_constructor_actions`, `resolve_property_value`; [perspectives/perspective_instance/abstract_classes.rs](../../../rust-executor/src/perspectives/perspective_instance/abstract_classes.rs): `remove_subject`.
- [perspectives/instances/](../../../rust-executor/src/perspectives/instances): the validated writes of the `perspective.instance*` RPC and the MCP `instance_*` tools.
- [perspectives/perspective_instance/abstract_classes.rs](../../../rust-executor/src/perspectives/perspective_instance/abstract_classes.rs), [model_query/abstract_classes.rs](../../../rust-executor/src/perspectives/model_query/abstract_classes.rs), [model_query/builtin_classes.rs](../../../rust-executor/src/perspectives/model_query/builtin_classes.rs).
- [perspectives/subject_classes_of.rs](../../../rust-executor/src/perspectives/subject_classes_of.rs), [perspectives/ordering/](../../../rust-executor/src/perspectives/ordering).
- MCP tools generated from SHACL: [mcp/shacl.rs](../../../rust-executor/src/mcp/shacl.rs), [mcp/tools/dynamic.rs](../../../rust-executor/src/mcp/tools/dynamic.rs).
- SDK: [core/src/shacl/SHACLShape.ts](../../../core/src/shacl/SHACLShape.ts); the model class side is on [Model classes](../../agents/interfaces/typescript-sdk/model-classes.md).

</details>
