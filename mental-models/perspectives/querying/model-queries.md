# Model queries

A **model query** asks for instances of one or more subject classes: `perspective.modelQuery(className, queryJson)`. The executor loads each class's SHACL shape, builds SPARQL from it, runs that on the store, and returns JSON instances with their properties, relations and a total count.

**Why:** The SDK's model classes (`Ad4mModel`) once built and filtered their queries in TypeScript. That moved raw rows over the socket and repeated the logic in every client. With the engine in the executor, MCP tools, live queries, flows and interpretation use the same reader. The shape comes from SHACL stored in the perspective, so the executor needs no client code to read a class.

**Parent:** [Querying](README.md)

<details><summary>How it works</summary>

`execute_model_query` (pipeline diagram in [model_query/mod.rs](../../../rust-executor/src/perspectives/model_query/mod.rs)):

1. **Shape**: `get_shape(class)` returns a cached `Arc<ModelShape>` parsed from SHACL links. In a neighbourhood, `get_shape_or_wait` waits up to 20 s (`MODEL_QUERY_SHAPE_WAIT`) for SHACL that has not synced yet.
2. **SPARQL build** (`sparql_builder`): conformance patterns (which triples make a URI an instance), pushed-down `where` filters, the link guard (status and verdict), paging.
3. **Store**: run on the `SparqlStore`.
4. **Hydration**: group rows by instance, decode `literal:` values, collect relation arrays, apply collection ordering, add author (the device key) and timestamp.
5. **Filtering and sorting** for conditions SPARQL could not express, among them the member filter: in a neighbourhood, drop links whose author maps to no member at the link's time (the replay is not in the store).
6. **Getters and relations**: SPARQL `ASK` / `SELECT` getters, reverse relations, recursive `include`.
7. **Projections**: counts and lists over relation edges.

Query input (`ModelQueryInput`): `where`, `order`, `limit`, `offset`, `count`, `properties`, `include`, `projections`, `parent` scope, `polymorphic`, `preferClasses`, `linkStatus`, `includeUnverified`. A model query is a logic read: by default it reads only `Valid` links ([Link verdicts](../links/verdicts.md)). Several classes in one query go through `execute_union_query`. Abstract classes (for example the built-in `ad4m://TextContent`) resolve to the concrete classes that implement them.

</details>

<details><summary>Where in the code</summary>

- [perspectives/model_query/](../../../rust-executor/src/perspectives/model_query): [query.rs](../../../rust-executor/src/perspectives/model_query/query.rs) (`execute_model_query`), [shape.rs](../../../rust-executor/src/perspectives/model_query/shape.rs), [sparql_builder.rs](../../../rust-executor/src/perspectives/model_query/sparql_builder.rs) (`LinkGuard`), [hydration.rs](../../../rust-executor/src/perspectives/model_query/hydration.rs), [filtering.rs](../../../rust-executor/src/perspectives/model_query/filtering.rs), [types.rs](../../../rust-executor/src/perspectives/model_query/types.rs), [union.rs](../../../rust-executor/src/perspectives/model_query/union.rs), [abstract_classes.rs](../../../rust-executor/src/perspectives/model_query/abstract_classes.rs).
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `model_query`, `model_query_classes`, `get_shape`, `get_shape_or_wait`, `invalidate_shape`, `evaluate_getters`.
- SDK: [core/src/model/Ad4mModel.ts](../../../core/src/model/Ad4mModel.ts), [core/src/model/ModelQueryBuilder.ts](../../../core/src/model/ModelQueryBuilder.ts); see [Model classes](../../agents/interfaces/typescript-sdk/model-classes.md).
- Guide: [docs-src/pages/developer-guides/model-classes.mdx](../../../docs-src/pages/developer-guides/model-classes.mdx).

</details>
