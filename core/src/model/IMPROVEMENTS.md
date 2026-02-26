# Ad4mModel — Improvement Backlog

Identified gaps and improvement areas for future PRs.

---

## 1. Dirty Tracking (Unit of Work)

**Problem:** Every `save()` writes all properties unconditionally. If a model has 20 properties and only one changes, all 20 setter actions are executed, creating unnecessary link churn and network noise.

**Solution:** Maintain a `#dirtyFields: Set<string>` on each instance. Intercept property assignments (via `Proxy` or setter generation in `Property()`) to mark fields dirty. `save()` only writes dirty fields; after commit the set is cleared.

**Prior art:** TypeORM Unit of Work, MikroORM identity map.

---

## 2. SurrealDB-Side Range Filter Push-Down

**Problem:** Comparison operators (`gt`, `lt`, `gte`, `lte`, `between`, `contains`) on scalar properties fall through to post-query JavaScript filtering. Because scalar values are stored as `literal://` URIs, SurrealDB cannot index them for range comparisons without `fn::parse_literal()` unwrapping. This means `findAll(p, { where: { rating: { gt: 4 } } })` fetches **all** instances and filters in JS — a full table scan at scale.

**Options:**

- Store numeric/boolean values in a SurrealDB-native way (separate `value` field alongside the `literal://` URI) so the DB can index them directly.
- Define SurrealDB computed fields that cache the unwrapped scalar value, then filter against the computed field.
- Use `fn::parse_literal(out.uri)` inside the graph traversal WHERE clause directly (requires SurrealDB to evaluate the function per-link, no index — but still better than JS filtering).

---

## 3. N+1 Prevention for Typed Relation Hydration

**Problem:** When `relatedModel` is set on a `@HasMany`, hydrating relations calls `_findAllInternal` once per parent instance. 10 posts × 5 comments each = 10 separate `findAll()` queries. No batching exists.

**Solution:** In `hydrateInstanceFromLinks` (or a post-hydration pass in `operations.ts`), collect all relation IDs across the entire result set, then run a single batched `findAll({ where: { base: [...ids] } })` for each related model type. Equivalent to a DataLoader / SQL `IN (...)` pattern.

---

## 4. Runtime Validation

**Problem:** There is no validation layer. Setting `recipe.rating = "not a number"` silently persists a type-incorrect value. Errors surface only later (if at all) during hydration.

**Solution:** Add an optional `validate` option to `@Property` accepting a predicate function or a class-validator-style decorator. Add a `validate()` method to `Ad4mModel` that checks all properties before `save()`. Optionally throw by default or expose errors as a return value.

```typescript
@Property({ through: "recipe://rating", validate: (v) => typeof v === "number" && v >= 0 })
rating: number = 0;
```

---

## 5. TypeScript Type Inference for `fromJSONSchema`

**Problem:** `fromJSONSchema` returns `typeof Ad4mModel` — TypeScript has no knowledge of the generated class's property types. Autocomplete and type-checking are completely lost for dynamically generated models.

**Options:**

- A type-level schema builder (fluent API returning typed class) that preserves inference without codegen:
  ```typescript
  const Recipe = Ad4mModel.define({
    name: field<string>({ through: "recipe://name" }),
    rating: field<number>({ through: "recipe://rating" }),
  });
  // Recipe is typed: { name: string; rating: number }
  ```
- Code generation from JSON Schema (Prisma-style): a CLI command that produces a `.ts` file with a properly typed class from a `.json` schema file.

---

## 6. Robust Create-vs-Update Detection

**Problem:** `#savedOnce` tracks whether `save()` was called on the current JS instance. Constructing a model wrapper around a known existing ID (`new Recipe(perspective, existingId)`) starts with `#savedOnce = false`, so the first `save()` runs the create path (calling `createSubject`), which may conflict with the already-existing subject.

**Solution:** Before the first `save()` on an instance constructed with an explicit `id`, check the perspective for the subject's actual existence (e.g. `isSubjectInstance`). Cache the result so subsequent saves don't pay the round-trip cost.

---

## 7. Stage 3 Decorator Migration

**Problem:** The decorator system uses TypeScript's legacy `experimentalDecorators` (Stage 2) format. TypeScript 5.x introduced Stage 3 decorators with different metadata semantics (`context` object instead of `target`/`key`). The current code will need migration when Stage 3 becomes the ecosystem default.

**Action:** Track TC39 Stage 3 stabilisation. When `reflect-metadata` is no longer required by major libraries, plan a migration. The `WeakMap`-keyed registry (`propertyRegistry`, `relationRegistry`) already decouples metadata storage from the decorator protocol, so migration should be largely mechanical — change decorator signatures, keep the registry logic intact.

---

## 8. `getter` Option — N+1 Footgun

**Problem:** `@Property({ getter: "..." })` and `@HasMany({ getter: "..." })` allow arbitrary SurrealQL expressions to be evaluated per-instance after the main link fetch. This fires one additional `querySurrealDB` call _per instance_ in `evaluateCustomGetters()` — a guaranteed N+1 on every `findAll`.

**Distinction from `transform`:** `transform` operates on a value already resolved from a link triple (sync, no extra query). `getter` is for properties with **no backing link triple** — values computed entirely from other nodes' data (e.g. `array::len(string::split(...))` aggregations). It covers ground `transform` cannot, so it cannot simply be removed.

**Additional limitations:**

- `getter` properties are invisible to `where` clauses (skipped in `hydrateInstanceFromLinks`) — `findAll(p, { where: { wordCount: { gt: 100 } } })` silently ignores the filter.
- Not subscribed correctly — every subscription re-query re-fires all getter calls.
- Not represented in the SHACL shape — the Rust executor has no knowledge of it.

**Proper fix:** Emit getter expressions as SurrealDB computed fields in the SHACL shape (see item #2), so they are evaluated server-side inside the bulk query rather than in a separate per-instance round-trip. Until that exists, `getter` is a necessary escape hatch but should be used only for single-instance `.find()` calls, never in `findAll`.

**Action:** Add a dev-time `console.warn` when `getter` is present without `readOnly: true`, reinforcing that it is not a writable/filterable property. Document the N+1 caveat clearly.

---

## 9. Abort / Rollback for Transactions

**Problem:** `runTransaction` catches errors and logs a debug message, but there is no explicit `abortBatch()` on `PerspectiveProxy`. The uncommitted batch is silently discarded by the runtime GC. This is invisible to the caller and relies on runtime-level cleanup.

**Solution:** If `PerspectiveProxy` exposes an `abortBatch(id)` call (or can be extended to), call it explicitly in the `catch` block of `runTransaction` for predictable, immediate cleanup rather than relying on GC timing.

---

## 10. tests/js Integration Test Migration

**Background:** The `ad4m/tests/js` Mocha suite is the correct long-term home for all `Ad4mModel` integration tests. The target file is `tests/js/tests/model-decorator-api.test.ts`. Currently scenario 08 (14 tests) lives in the `we` playground. The `tests/js` suite imports deprecated symbols and needs a cleanup pass before new tests can land cleanly.

**Action:**

1. Fix deprecated imports in `prolog-and-literals.test.ts` and `multi-user-simple.test.ts`
2. Establish consistent port allocation and setup pattern across the suite
3. Port scenario 08 models + all current tests into `model-decorator-api.test.ts`
4. Add subscription lifecycle tests (Phase 3d coverage at executor level)
5. Write remaining scenarios (01–07, 09–10) directly in `tests/js`

**Priority:** Post-merge. Blocking a dedicated follow-up PR.

---

## 11. CRDT Ordering (Phase 5)

**Background:** Concurrent link writes from multiple agents can produce non-deterministic ordering — last-writer-wins SurrealDB timestamp semantics may not match causal intent in a distributed graph.

**Design:** See `CRDT-ORDERING-STRATEGY.md` for full detail. Phase 3 (query layer separation) is now complete so the prerequisite is met.

**Scope:** Changes to `hydration.ts`, `SurrealQueryBuilder.ts`, possibly SHACL-level ordering predicates, and the Rust executor's link ordering. Fills in scenario 10 of the playground test harness.

**Priority:** Post-merge. High impact for multi-user apps.

---

## 12. External Consumer Migration — SubjectRepository (Phase G)

**Background:** `flux/packages/api/src/factory/SubjectRepository` and `ad4m-hooks/helpers/src/factory/SubjectRepository` still use Prolog `infer()` + the Subject proxy (`subject.init()`, async getters) for all model queries. They have no knowledge of `Ad4mModel`.

**Migration path:**

- Replace `getAll()` + `getSubjectData()` with `Ad4mModel.findAll()`
- Replace paginated `getAll()` with `Ad4mModel.findPage()`
- Replace Subject proxy getters with `Ad4mModel.getData()`
- Wire `useSubjects`/`useSubject` hooks to `Ad4mModel.subscribe()` for live updates

**Removals after migration:**

- `PerspectiveProxy.getSubjectData()` + `getSubjectProxy()`
- `PerspectiveClient.getSubjectData()`
- Rust `get_subject_data()` + GQL resolver (currently fires 5 Prolog queries per call)
- Both `SubjectRepository` classes

**Prerequisite:** Flux Phase F (decorator rename) must complete first.

**Priority:** Post-merge. Gated on Flux and hooks teams.

---

## 13. sh:inversePath — Rust/Prolog Side

**Background:** `@BelongsToOne`/`@BelongsToMany` set `inversePath: true` on the SHACL shape. The TypeScript SurrealDB hydration path works correctly (reverse `WHERE out.uri = ...` query). However `shacl_parser.rs` has zero handling of `sh:inversePath` — it never emits reverse Prolog predicates, so any Prolog-side lookup for a reverse relation silently returns nothing. `generatePrologFacts.ts` likewise has no reverse-predicate clause.

**Action:**

- `shacl_parser.rs`: detect `sh:inversePath`, emit reverse Prolog clauses (e.g. `channel_of(X, Y) :- triple(Y, 'predicate', X).`)
- `generatePrologFacts.ts`: handle `inversePath: true` shapes, emit reverse predicate clause

**Priority:** Low — only matters if a consumer writes explicit Prolog queries against reverse relations. The SurrealDB path (the common case) already works.

---

## 14. Parameterised SurrealQL Queries

**Problem:** `SurrealQueryBuilder.ts` and `getData()` still use string interpolation with `formatSurrealValue()` to prevent injection. SurrealDB supports parameterized queries via `querySurrealDB(query, bindings)` which are safe by construction.

```typescript
// Current:
`SELECT ... FROM link WHERE in.uri = ${formatSurrealValue(base)}`;

// Target:
perspective.querySurrealDB("SELECT ... FROM link WHERE in.uri = $base", {
  base,
});
```

**Scope:** `queryToSurrealQL`, `getData()`, `fetchInstance.ts`, all raw SurrealQL construction throughout the query layer.

**Priority:** Medium. Not a regression (`formatSurrealValue` prevents injection today) but parameterized queries are the industry standard and eliminate an entire class of potential escaping bugs.

---

## 15. SDNA Wire Protocol Rename (`collection*` → `relation*`)

**Background:** The SDNA wire protocol still uses `collection`-prefixed predicate names: `collection/2`, `collection_getter/4`, `collection_adder/3`, `collection_remover/3`, `collection_setter/3`. These appear in 78+ locations across bootstrap languages and are queried by name throughout the Rust executor.

**Why deferred:** Renaming is a breaking change to the SDNA wire protocol — any live perspective with existing SDNA would stop working. Requires a versioned format, compatibility shim, or coordinated breaking release.

**Scope:**

1. Choose new names (`relation/2`, `relation_getter/4`, etc.)
2. Update all Rust executor references (`engine_pool.rs`, `sdna.rs`, `perspective_instance.rs`)
3. Update all 78+ bootstrap language `.pl` files
4. Update `tests/js/sdna/subject.pl`
5. Write migration strategy

**Priority:** Major version / coordinated release item. Not before Phase G is complete.

---

## 16. Relation Action Duplication — TypeScript vs Rust SHACL

**Problem:** Relation adder/remover/setter mutations call `executeAction` with a TypeScript-generated action array. The Rust executor has `get_collection_adder_actions()` that derives the same structure from SHACL. Two implementations of the same logic that must stay in sync manually.

**Solution:** Have relation mutations fetch SHACL-derived actions from the executor (the same way `createSubject` does for property setters) rather than generating them independently in TypeScript.

**Priority:** Low. The implementations are simple and unlikely to diverge in practice, but the duplication is a maintenance risk for anyone modifying SHACL action formats.

---

## Priority Order (suggested)

| #   | Item                            | Impact | Effort |
| --- | ------------------------------- | ------ | ------ |
| 2   | Range filter push-down          | High   | Medium |
| 1   | Dirty tracking                  | High   | Medium |
| 3   | N+1 batching                    | High   | Medium |
| 8   | `getter` N+1 / push-down        | High   | Medium |
| 11  | CRDT ordering                   | High   | High   |
| 4   | Runtime validation              | Medium | Low    |
| 6   | Create-vs-update detection      | Medium | Low    |
| 14  | Parameterised SurrealQL         | Medium | Medium |
| 10  | tests/js migration              | Medium | Medium |
| 12  | External consumer migration     | Medium | High   |
| 5   | fromJSONSchema type inference   | Medium | High   |
| 13  | sh:inversePath Rust side        | Low    | Low    |
| 16  | Relation action duplication     | Low    | Medium |
| 9   | Transaction abort               | Low    | Low    |
| 15  | SDNA wire rename                | Low    | High   |
| 7   | Stage 3 decorators              | Low    | High   |
| 8   | Transaction abort               | Low    | Low    |
| 5   | `fromJSONSchema` type inference | Medium | High   |
| 7   | Stage 3 decorators              | Low    | High   |
