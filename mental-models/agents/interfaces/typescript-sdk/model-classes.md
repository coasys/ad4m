# Model classes

A **model class** is a TypeScript class that extends `Ad4mModel` and carries decorators (`@Model`, `@Property`, `@HasMany`, …). The SDK turns the decorators into a **subject class** (a SHACL shape), registers it in a perspective, and maps the class's methods to the executor's instance and model-query RPC calls. `useLiveQuery` in `ad4m-hooks` wraps the same calls for React and Vue.

**Why:** App authors work with typed objects, while the executor stores only links and SHACL. The decorators are the one place where an app declares its schema. The executor gets the generated SHACL and needs no app code to read or write instances ([Subject classes](../../../perspectives/social-dna/subject-classes.md)). Queries and live queries run in the executor ([Model queries](../../../perspectives/querying/model-queries.md)), so the SDK sends a query and receives instances, not raw links.

**Parent:** [TypeScript SDK](README.md)

<details><summary>How it works</summary>

| App call | SDK path | Executor RPC |
|---|---|---|
| `Message.register(perspective)`, `Ad4mModel.registerAll(perspective, [..])` | `generateSHACL()` from the decorators → `PerspectiveProxy.registerShapes`, one call for all classes | `perspective.registerShapes` (`RegisterResult`: `unchanged`, `written`, `generation`) |
| `new Message(perspective); m.save()` (new) | open a batch, `createSubject` with the initial values, relations, `commitBatch` | `perspective.createSubject`, `executeCommands`, `commitBatch` |
| `m.save()` (fetched), `Message.update(..)` | only the changed fields, through each property's setter or adder actions; a value of `null` or `''` sends `removeLink` with target `*` (a required property refuses) | `perspective.executeCommands` (`updateSubject` for an abstract class) |
| `m.delete()` | destructor actions, or for a class without a destructor the links of its own predicates; then every incoming link | `perspective.removeSubject` or `perspective.removeLinks` |
| `Message.findAll(perspective, query)`, `findOne`, `count`, `paginate`, `page` | `prepareModelQueryParams` → class name + query JSON; `page` sends `after`, a keyset cursor on the timestamp order | `perspective.modelQuery` |
| `Ad4mModel.findAllOf(perspective, [A, B], query)` | one query over several classes (a union) | `perspective.modelQuery` |
| `Message.query(perspective, query).subscribe(cb)`, `Ad4mModel.queryOf(perspective, [A, B], query)`, `useLiveQuery` | a `LiveQuery` (`first` for the first page; applies deltas, resyncs on a gap or a lag, subscribes again after a reconnect) | `perspective.subscribe` (with `model`), `resyncSubscription` ([Live queries](../../../perspectives/querying/live-queries.md)) |
| `Ad4mModel.transaction(perspective, fn)` | one batch for every call that passes `tx.batchId` | `createBatch` / `commitBatch` ([Link write path](../../../perspectives/links/write-path.md)) |

1. `@Model({ name })` names the class. `@Property`, `@Optional`, `@ReadOnly` and `@Flag` declare properties (predicate, resolve language, `local`). `@HasMany`, `@HasOne`, `@BelongsToOne` and `@BelongsToMany` declare relations.
2. `shacl-gen.ts` writes one `sh:PropertyShape` per property, and the setter, adder and remover actions as AD4M action literals.
3. `Ad4mModel.fromJSONSchema` and `fromSHACL` build a model class at runtime, without decorators.
4. `registerShapes` is idempotent in the executor: a class whose stored shape already is its shape writes nothing (`unchanged`), so registering on every start is cheap ([Subject classes](../../../perspectives/social-dna/subject-classes.md)).

</details>

<details><summary>Where in the code</summary>

- [core/src/model/Ad4mModel.ts](../../../../core/src/model/Ad4mModel.ts): `Ad4mModel` (`register`, `registerAll`, `save`, `delete`, `findAll`, `findOne`, `count`, `paginate`, `query`, `transaction`, `fromJSONSchema`, `fromSHACL`).
- [core/src/model/decorators.ts](../../../../core/src/model/decorators.ts): the decorators and their metadata registries.
- [core/src/model/shacl-gen.ts](../../../../core/src/model/shacl-gen.ts): decorators to `SHACLShape`.
- [core/src/model/query-sparql.ts](../../../../core/src/model/query-sparql.ts): the SDK's own SPARQL builder, used by `Ad4mModel.queryToSPARQL`; model queries in the executor use the executor's builder (`proof_valid_filter` in [model_query/sparql_builder.rs](../../../../rust-executor/src/perspectives/model_query/sparql_builder.rs)).
- [core/src/perspectives/PerspectiveProxy.ts](../../../../core/src/perspectives/PerspectiveProxy.ts): `registerShapes`, `shapes`, `modelQuery`, `liveSparql`, `createSubject`, `updateSubject`, `removeSubject`, `executeAction`.
- [core/src/perspectives/PerspectiveClient.ts](../../../../core/src/perspectives/PerspectiveClient.ts): `subscribe` opens a live query.
- [core/src/perspectives/LiveQuery.ts](../../../../core/src/perspectives/LiveQuery.ts): one live query on the client.
- [ad4m-hooks/react/src/useLiveQuery.ts](../../../../ad4m-hooks/react/src/useLiveQuery.ts), [ad4m-hooks/vue/src/useLiveQuery.ts](../../../../ad4m-hooks/vue/src/useLiveQuery.ts): the hooks.
- Tests against a running executor: [tests/js/tests/model/](../../../../tests/js/tests/model). Guide: [docs-src/pages/developer-guides/model-classes.mdx](../../../../docs-src/pages/developer-guides/model-classes.mdx).

</details>
