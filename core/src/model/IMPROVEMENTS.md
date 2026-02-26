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

## 8. Abort / Rollback for Transactions

**Problem:** `runTransaction` catches errors and logs a debug message, but there is no explicit `abortBatch()` on `PerspectiveProxy`. The uncommitted batch is silently discarded by the runtime GC. This is invisible to the caller and relies on runtime-level cleanup.

**Solution:** If `PerspectiveProxy` exposes an `abortBatch(id)` call (or can be extended to), call it explicitly in the `catch` block of `runTransaction` for predictable, immediate cleanup rather than relying on GC timing.

---

## Priority Order (suggested)

| #   | Item                            | Impact | Effort |
| --- | ------------------------------- | ------ | ------ |
| 2   | Range filter push-down          | High   | Medium |
| 1   | Dirty tracking                  | High   | Medium |
| 3   | N+1 batching                    | High   | Medium |
| 4   | Runtime validation              | Medium | Low    |
| 6   | Create-vs-update detection      | Medium | Low    |
| 8   | Transaction abort               | Low    | Low    |
| 5   | `fromJSONSchema` type inference | Medium | High   |
| 7   | Stage 3 decorators              | Low    | High   |
