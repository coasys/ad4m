# Subject classes

A **subject class** is a named SHACL node shape stored in a perspective. It lists properties and relations (each with a predicate, a datatype or target class, and a cardinality) and **actions**: JSON command lists for the constructor, the destructor and each property's setter, adder and remover. An **instance** is a base whose links satisfy the shape. The executor writes instances by running the actions and reads them with model queries.

**Why:** One stored definition serves every writer: the SDK, MCP tools, interpretation and apps on other executors produce the same links for the same class. The links present decide conformance, not a stored type tag, so any data with the right links is an instance. Actions are data, not code, so the executor runs them without loading app code.

**Parent:** [Social DNA](README.md)

<details><summary>How it works</summary>

Registration:

1. An app's [model class](../../agents/interfaces/typescript-sdk/model-classes.md) generates the SHACL and sends it with `perspective.addSdna(name, "", "subject_class", shaclJson)`.
2. `add_sdna_inner` finds every class with the same **name**, deletes its SHACL links, and writes the new ones ([Social DNA](README.md)). The name is the last path segment of the class URI. The executor does this on every registration, also when the shape has not changed, and the change syncs to the neighbourhood.
3. The SDK's `ensureSubjectClass` remembers registered classes per `PerspectiveProxy` (`#ensuredSubjectClasses`), so each new SDK session registers its classes again.

Stored shape (links): `<class> rdf://type ad4m://SubjectClass`, `<class> ad4m://shape <shape>`, `<shape> sh://property <propShape>`, plus action literals under `ad4m://constructor`, `ad4m://destructor`, `ad4m://setter`, `ad4m://adder`, `ad4m://remover`, and flags such as `ad4m://local` and `ad4m://interpretation_hint`.

Writes:

| RPC | Runs |
|---|---|
| `perspective.createSubject` | constructor actions, then the setter of each initial value (`create_subject`) |
| `perspective.updateSubject` | setters, `collectionSetter` for arrays (`update_subject`) |
| `perspective.removeSubject` | destructor actions |
| `perspective.executeCommands` | any command list (`execute_commands`) |

`execute_commands` turns each `Command` (`addLink`, `removeLink`, `setSingleTarget`, `collectionSetter`) into link writes; a command marked `local` writes a local link. A `collectionSetter` on a relation with a declared ordering strategy also writes ordering entries on the parent (`apply_collection_ordering`, [ordering/](../../../rust-executor/src/perspectives/ordering)).

Reads go through [Model queries](../querying/model-queries.md). `perspective.subjectClassesOf` lists the classes a URI conforms to. An **abstract class** (for example the built-in `ad4m://TextContent`) reads several concrete classes. A write through it goes to the instance's own classes; creating one is refused.

</details>

<details><summary>Where in the code</summary>

- [perspectives/shacl_parser.rs](../../../rust-executor/src/perspectives/shacl_parser.rs): `SHACLShape`, `PropertyShape`, `AD4MAction`, `parse_shacl_to_links`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `Command`, `Action`, `add_sdna_inner`, `remove_subject_class_shacl_links`, `create_subject`, `update_subject`, `execute_commands`, `apply_collection_ordering`, `get_constructor_actions`, `resolve_property_value`.
- [perspectives/perspective_instance/abstract_classes.rs](../../../rust-executor/src/perspectives/perspective_instance/abstract_classes.rs), [model_query/abstract_classes.rs](../../../rust-executor/src/perspectives/model_query/abstract_classes.rs), [model_query/builtin_classes.rs](../../../rust-executor/src/perspectives/model_query/builtin_classes.rs).
- [perspectives/subject_classes_of.rs](../../../rust-executor/src/perspectives/subject_classes_of.rs), [perspectives/ordering/](../../../rust-executor/src/perspectives/ordering).
- MCP tools generated from SHACL: [mcp/shacl.rs](../../../rust-executor/src/mcp/shacl.rs), [mcp/tools/dynamic.rs](../../../rust-executor/src/mcp/tools/dynamic.rs).
- SDK: [core/src/shacl/SHACLShape.ts](../../../core/src/shacl/SHACLShape.ts); the model class side is on [Model classes](../../agents/interfaces/typescript-sdk/model-classes.md).

</details>
