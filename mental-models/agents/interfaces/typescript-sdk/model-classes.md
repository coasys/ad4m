# Model classes

A **model class** is a TypeScript class that extends `Ad4mModel` and carries decorators (`@Model`, `@Property`, `@HasMany`, …). The SDK turns the decorators into a **subject class** (a SHACL shape), registers it in a perspective, and maps the class's methods to the executor's instance and model-query RPC calls. `useLiveQuery` in `ad4m-hooks` wraps the same calls for React and Vue.

**Why:** App authors work with typed objects, while the executor stores only links and SHACL. The decorators are the one place where an app declares its schema. The executor gets the generated SHACL and needs no app code to read or write instances ([Subject classes](../../../perspectives/social-dna/subject-classes.md)). Queries and live queries run in the executor ([Model queries](../../../perspectives/querying/model-queries.md)), so the SDK sends a query and receives instances, not raw links.

**Parent:** [TypeScript SDK](README.md)

<details><summary>How it works</summary>

| App call | SDK path | Executor RPC |
|---|---|---|
| `Message.register(perspective)`, `Ad4mModel.registerAll(perspective, [..])` | `generateSHACL()` from the decorators → `PerspectiveProxy.ensureSubjectClass(es)` | `perspective.addSdna` (one class, or `entries` for several; type `subject_class`) |
| `new Message(perspective); m.save()` (new) | open a batch, `createSubject` with the initial values, relations, `commitBatch` | `perspective.createSubject`, `executeCommands`, `commitBatch` |
| `m.save()` (fetched), `Message.update(..)` | only the changed fields, through each property's setter or adder actions | `perspective.executeCommands` (`updateSubject` for an abstract class) |
| `m.delete()` | destructor actions, or for a class without a destructor the links of its own predicates; then every incoming link | `perspective.removeSubject` or `perspective.removeLinks` |
| `Message.findAll(perspective, query)`, `findOne`, `count`, `paginate` | `prepareModelQueryParams` → class name + query JSON | `perspective.modelQuery` |
| `Message.query(perspective).where(..).subscribe(cb)`, `useLiveQuery` | `ModelQueryBuilder` → `LiveQuery` (applies deltas, resyncs on a gap, a lag or a reconnect) | `perspective.modelSubscribe`, `resyncSubscription` ([Live queries](../../../perspectives/querying/live-queries.md)) |
| `Ad4mModel.transaction(perspective, fn)` | one batch for every call that passes `tx.batchId` | `createBatch` / `commitBatch` ([Link write path](../../../perspectives/links/write-path.md)) |

1. `@Model({ name })` names the class. `@Property`, `@Optional`, `@ReadOnly` and `@Flag` declare properties (predicate, resolve language, `local`). `@HasMany`, `@HasOne`, `@BelongsToOne` and `@BelongsToMany` declare relations.
2. `shacl-gen.ts` writes one `sh:PropertyShape` per property, and the setter, adder and remover actions as AD4M action literals.
3. `Ad4mModel.fromJSONSchema` and `fromSHACL` build a model class at runtime, without decorators.
4. Each `PerspectiveProxy` remembers which classes it registered. A new SDK session sends its classes again, and the executor writes them again ([Subject classes](../../../perspectives/social-dna/subject-classes.md)).

</details>

<details><summary>Where in the code</summary>

- [core/src/model/Ad4mModel.ts](../../../../core/src/model/Ad4mModel.ts): `Ad4mModel` (`register`, `registerAll`, `save`, `delete`, `findAll`, `findOne`, `count`, `paginate`, `query`, `transaction`, `fromJSONSchema`, `fromSHACL`).
- [core/src/model/decorators.ts](../../../../core/src/model/decorators.ts): the decorators and their metadata registries.
- [core/src/model/shacl-gen.ts](../../../../core/src/model/shacl-gen.ts): decorators to `SHACLShape`.
- [core/src/model/ModelQueryBuilder.ts](../../../../core/src/model/ModelQueryBuilder.ts): the fluent query and `subscribe` / `paginateSubscribe`.
- [core/src/model/query-sparql.ts](../../../../core/src/model/query-sparql.ts): the SDK's own SPARQL builder, used by `Ad4mModel.queryToSPARQL`; model queries in the executor use the executor's `sparql_builder`.
- [core/src/perspectives/PerspectiveProxy.ts](../../../../core/src/perspectives/PerspectiveProxy.ts): `ensureSubjectClass`, `ensureSubjectClasses`, `modelQuery`, `modelSubscribe`, `createSubject`, `executeAction`.
- [core/src/perspectives/LiveQuery.ts](../../../../core/src/perspectives/LiveQuery.ts): one live query on the client.
- [ad4m-hooks/react/src/useLiveQuery.ts](../../../../ad4m-hooks/react/src/useLiveQuery.ts), [ad4m-hooks/vue/src/useLiveQuery.ts](../../../../ad4m-hooks/vue/src/useLiveQuery.ts): the hooks.
- Tests against a running executor: [tests/js/tests/model/](../../../../tests/js/tests/model). Guide: [docs-src/pages/developer-guides/model-classes.mdx](../../../../docs-src/pages/developer-guides/model-classes.mdx).

</details>
