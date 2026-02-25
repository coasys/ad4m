# AD4M Model Refactor Plan

## Branch Context

This branch (`ad4m-model-refactor`) is based directly on `feat/shacl-sdna-migration` (PR #654).
That PR already completed the foundational migration:

- SHACL links replace Prolog-stored SDNA as the model definition format
- `PROLOG_MODE` set to `Disabled` in the Rust executor
- `generateSHACL()` is now the source of truth in `decorators.ts`
- `shacl_parser.rs` added to Rust — parses SHACL links and generates Prolog facts from them

**We build on top of that work. We do not duplicate it.**

---

## Current Status (as of 2026-02-24)

### Completed

| Item                                                                                                                                                                                                                                       | Commit / Notes                                                                   |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------- |
| Phase 1 (Prolog removal)                                                                                                                                                                                                                   | `bd3a7b6c` in `ad4m-model-refactor`                                              |
| Phase 2 decorator renames (`@Property`, `@HasMany`, `@Model`, `@Flag`)                                                                                                                                                                     | `bd3a7b6c` — `we` repo migrated in follow-up commit                              |
| `@we/models` Phase 2 migration (`Space`, `Block`, block-types)                                                                                                                                                                             | `we` `dev` branch                                                                |
| `declare` → `HasManyMethods<Keys>` interface-merge pattern on `Space` + `Block`                                                                                                                                                            | Replaces `declare add/remove/set` stubs; avoids Babel ordering issues            |
| `HasManyMethods<Keys extends string>` utility type exported from `@coasys/ad4m`                                                                                                                                                            | `decorators.ts`                                                                  |
| `get id()` public alias on `Ad4mModel`                                                                                                                                                                                                     | `Ad4mModel.ts` — alias for `baseExpression`                                      |
| Test app scaffold (`apps/playgrounds/react/ad4m-model-testing`)                                                                                                                                                                            | Scenarios 01–09 fully live; 10 stubs (Phase 5 not started)                       |
| `uuid` field removed from `Space` model                                                                                                                                                                                                    | Was redundant with `baseExpression`/`id`                                         |
| `UserLocation` + `SpaceType` interfaces removed from `Space.ts`                                                                                                                                                                            | Dead code — no consumers outside `Space.ts` itself                               |
| `$perspective` phantom variable removed from `queryToSurrealQL`                                                                                                                                                                            | Fixed — see resolved issues below                                                |
| `->link AS links` hydration bug fixed in `queryToSurrealQL`                                                                                                                                                                                | Fixed — see resolved issues below                                                |
| `setCollection*` → `set*` rename                                                                                                                                                                                                           | `ad4m@46e140e2`, `we@cf19352`                                                    |
| WeakMap metadata registry fix                                                                                                                                                                                                              | `decorators.ts` — eliminates prototype-mutation inheritance bug                  |
| `@BelongsToOne` / `@BelongsToMany` decorators                                                                                                                                                                                              | Implemented with `direction: "reverse"` — no mutator stubs generated             |
| `@HasOne` decorator                                                                                                                                                                                                                        | Implemented with `maxCount: 1`                                                   |
| `@HasMany` / `@HasOne` accept optional model-class factory as first arg                                                                                                                                                                    | `HasMany(() => ModelClass, opts)` — enables typed eager hydration via `include`  |
| `getModelMetadata()` propagates `direction` + `maxCount`                                                                                                                                                                                   | Root-cause fix for `@BelongsToOne` not populating reverse fields                 |
| Constructor skips `add/remove/set` stubs for `direction === "reverse"` relations                                                                                                                                                           | Prevents `addPost`/`removePost` being generated on `@BelongsToOne`               |
| `getData()` reverse-link query (`WHERE out.uri = ...`, maps `l.source`)                                                                                                                                                                    | Single-instance hydration of reverse relations                                   |
| `instancesFromSurrealResult()` batch reverse-link query                                                                                                                                                                                    | `findAll()` hydration of reverse relations (`maxCount === 1` for HasOne)         |
| `generatePrologFacts` updated: `collections` → `relations` in metadata                                                                                                                                                                     | Keeps Prolog bridge in sync with renamed field                                   |
| `PerspectiveProxy.buildQueryFromTemplate` uses `getPropertiesMetadata` / `getRelationsMetadata`                                                                                                                                            | Removed `__properties`/`__collections` prototype hacks                           |
| `collection*` → `relation*` rename (Rust, TypeScript, Prolog facts)                                                                                                                                                                        | `ad4m@2f5eabca`, `we@98c2300` — `collectionSetter` → `relationSetter` throughout |
| Scenario 08 expanded to 12 tests — all 6 decorator types covered                                                                                                                                                                           | `findOne`, re-`save`, `remove*`, `set*`, `delete`, `@BelongsToMany` all tested   |
| Unified `save()` — create vs update routed by SurrealDB existence check                                                                                                                                                                    | `Ad4mModel.ts` — `update()` deprecated, delegates to `save()`                    |
| Rust `create_subject` merge fix — preserves `SetSingleTarget` action on re-save                                                                                                                                                            | `perspective_instance.rs` — replaces `cmd.target` with full command struct       |
| Rust executor rebuilt — all 13 scenario 08 tests passing                                                                                                                                                                                   | `cargo build --release` — `create_subject` fix active, re-save verified ✅       |
| Scenario 08 expanded to 13 tests — `@BelongsToOne pinnedBy` added                                                                                                                                                                          | `TestComment.pinnedBy` field + test verifies null-case                           |
| `@Flag` SHACL wiring — `innerUpdate()` skips flag fields; `generatePropertySetterAction()` guards                                                                                                                                          | `flag?: boolean` added to `PropertyOptions`; flags immutable after creation ✅   |
| Scenario 08 expanded to 14 tests — `@Flag` immutability on re-save                                                                                                                                                                         | Re-saves post, verifies flag value survives and `findAll()` still returns it ✅  |
| Phase 3a: file decomposition — `types.ts`, `SurrealQueryBuilder.ts`, `hydration.ts`, `operations.ts`, `QueryBuilder.ts`, `fetchInstance.ts`, `metadata.ts`, `fromJSONSchema.ts`, `mutation.ts` extracted; `Ad4mModel.ts` 3,917 → 759 lines | `eb2f4b4b` → `6dcc5283` (6 commits)                                              |
| Phase 3b: Transaction API — `transaction.ts`, `runTransaction`, `TransactionContext`; `save()` / `delete()` accept `tx?: TransactionContext` instead of raw `batchId` string                                                               | `a66d833b`                                                                       |
| Phase 3c: `IncludeMap` Prisma-style eager loading — absent = no hydration, present = exactly the named relations; `.include(map)` on `QueryBuilder`; `include?` param on `get()` / `getData()`                                             | `6d02ad2d`                                                                       |

### Pending — Phase 2

**Phase 2 COMPLETE** ✅

### Pending — Phases 3–5

| Phase                         | Status         |
| ----------------------------- | -------------- |
| 3a File decomposition         | ✅ COMPLETE    |
| 3b Transaction API            | ✅ COMPLETE    |
| 3c IncludeMap eager loading   | ✅ COMPLETE    |
| 3d Subscriptions              | ✅ COMPLETE    |
| 3e Subscription infra cleanup | ⏳ PENDING     |
| 4 Model inheritance           | ✅ COMPLETE    |
| 5 CRDT ordering               | ⏳ NOT STARTED |
| 0 tests/js migration          | ⏳ PENDING     |
| G External consumer migration | ⏳ NOT STARTED |

---

## Future Work: SDNA Prolog predicate rename (`collection*` → `relation*`)

The SDNA wire protocol still uses `collection`-prefixed predicate names:

- `collection/2`
- `collection_getter/4`
- `collection_adder/3`
- `collection_remover/3`
- `collection_setter/3`

These are queried by name in the Rust executor (`engine_pool.rs`, `sdna.rs`, `perspective_instance.rs`) and appear in 78+ locations across the bootstrap languages. Renaming them is a **breaking change** to the SDNA wire protocol — any live perspective with existing SDNA (deployed apps, stored data) would stop working.

**What's required for this rename:**

1. Decide on new predicate names (e.g. `relation/2`, `relation_getter/4`, `relation_adder/3`, `relation_remover/3`, `relation_setter/3`)
2. Update all Rust executor references: regex patterns, hardcoded query strings, `discontiguous` declarations, SHACL→SDNA generation in `sdna.rs`
3. Update all bootstrap language `.pl` files (78+ occurrences)
4. Update the test fixture at `tests/js/sdna/subject.pl`
5. Write a migration strategy: either a versioned SDNA format, a compatibility shim that accepts both predicate names during a transition window, or a coordinated breaking release

**Priority:** Not before Phase 3. Track as a major version (breaking) release item alongside any other SDNA wire-protocol changes.

---

## Issue Log

### Open

### String interpolation into SurrealQL — use parameterized queries

`formatSurrealValue()` exists to prevent injection but the pattern is still string interpolation:

```typescript
`WHERE in.uri = ${safeBaseExpression}`;
```

SurrealDB supports parameterized queries via `querySurrealDB(query, bindings)`. Parameterized queries are immune to escaping mistakes by construction — they are the industry standard for query safety and are guaranteed never to produce injection vulnerabilities regardless of input.

```typescript
// Current — string interpolation, relies on formatSurrealValue() being correct:
`SELECT ... FROM link WHERE in.uri = ${formatSurrealValue(base)}`;

// Correct — parameterized, safe by construction:
perspective.querySurrealDB("SELECT ... FROM link WHERE in.uri = $base", {
  base,
});
```

This applies throughout `queryToSurrealQL`, `getData()`, and any other raw SurrealQL construction. Should be addressed in Phase 3a when `SurrealQueryBuilder.ts` is extracted as its own module — the right time to standardize the query construction pattern.

**Priority:** Phase 3a.

### 🟡 `eval()` in `PerspectiveProxy` for setter actions

The rollup build already warns about it:

```
lib/src/perspectives/PerspectiveProxy.js
  const actions = eval(setter.Setter);
```

This is inside the `Subject.ts` setter evaluation path. `eval` is a CSP violation in any app with a strict Content Security Policy, suppresses V8's JIT optimizations for the surrounding function, and is a potential injection vector if `setter.Setter` ever comes from untrusted data.

Phase 1c deletes `Subject.ts` entirely — this goes away then. This is an additional reason not to defer Phase 1c.

**Priority:** Resolved by Phase 1c (already planned).

### 🟡 `generateCollectionAction` duplicates Rust SHACL logic

`addLocations()` generates its own action array in TypeScript:

```typescript
[{ action: "addLink", source: "this", predicate: "...", target: "value" }];
```

The Rust executor has `get_collection_adder_actions` that derives the same structure from SHACL. Two implementations of the same thing that must stay in sync. `save()` uses the SHACL-derived path (correct); collection mutations use the TypeScript-generated path (fragile).

Long-term fix: have collection mutations also fetch SHACL-derived actions via the executor, the same way `createSubject` does. Short-term: add a comment in `generateCollectionAction` explicitly flagging the sync dependency so it's not silently broken by future SHACL changes.

**Priority:** Phase 3 planning item (already noted in Architectural Notes below).

---

### Resolved

### ✅ `save()` batch lifecycle — RESOLVED (Phase 3b, `a66d833b`)

**Fix applied:** `Ad4mModel.transaction(perspective, async (tx) => { ... })` wraps operations in a single batch with automatic commit on success and abort on throw. `save()` and `delete()` accept `tx?: TransactionContext` instead of a raw `batchId` string — `TransactionContext` is an opaque type so callers cannot misuse it. Implemented in `core/src/model/transaction.ts`.

### ✅ Dual hydration implementations (`getData` vs `instancesFromSurrealResult`) — RESOLVED (Phase 3a, `eb2f4b4b`)

**Fix applied:** Both single-instance (`fetchInstanceData` in `fetchInstance.ts`) and bulk query (`instancesFromSurrealResult` in `operations.ts`) paths now delegate to `hydrateInstanceFromLinks(instance, links, perspective, metadata)` in `core/src/model/query/hydration.ts`. The two independent implementations are gone; the divergence cannot recur.

### ✅ WeakMap metadata registry — RESOLVED

**Root cause:** `getPropertiesMetadata` / `getRelationsMetadata` used `target` (the prototype object) as the WeakMap key when registering decorator metadata. Because `Object.getPrototypeOf(ChildClass.prototype) === ParentClass.prototype`, a read for a child class would silently fall through to the parent's WeakMap entry and inherit the parent's property/relation set, even when the child had no declared decorators.

**Fix applied:** Registry key changed to `target.constructor` (the class constructor itself) in all four registry helpers (`registerProperty`, `getPropertiesMetadata`, `registerRelation`, `getRelationsMetadata`). Each class now has its own isolated entry; inheritance of metadata must be explicit.

### ✅ `@BelongsToOne` reverse traversal not populating fields — RESOLVED

**Root cause:** `getModelMetadata()` iterated over `getRelationsMetadata()` entries but only copied `through`, `className`, `model`, and `isCollection` into the returned metadata — silently dropping `direction` and `maxCount`. This meant that at runtime `relation.direction` was always `undefined`, so reverse-link query paths were never taken.

**Fix applied:** `getModelMetadata()` now copies every field from the relation descriptor (`direction`, `maxCount`, `through`, `className`, `model`, `isCollection`) into the outgoing metadata object.

**Verified:** Scenario 08 test 5 ("BelongsToOne reverse traversal") passing — `TestComment.post` correctly populated when fetched independently.

### ✅ `@HasMany` collection writes silently fail (`addLocations`, `addComments` etc.) — RESOLVED

**Root cause (write path):** The write was always working — `executeAction` returns `true` and the raw `perspective.get()` call (GraphQL layer) confirmed the link was persisted. The failure was entirely in the **read/hydration path**.

`queryToSurrealQL` SELECT used `->link AS links` which in SurrealDB graph traversal returns **target node records** (fields: `id`, `uri`), not **edge records** (fields: `predicate`, `target`, `author`, `timestamp`). So `instancesFromSurrealResult` received link objects where every field was `undefined` — properties and collections could never be hydrated.

`save()` appeared to work because it calls `getData()` which queries the `link` table directly with a correct field list — a separate code path.

**Fix:** Replaced `->link AS links` with a correlated subquery returning edge fields:

```sql
(SELECT predicate, out.uri AS target, author, timestamp
 FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links
```

**Verified:** All 4 scenario 08 tests passing (save, findAll, field round-trip, locations collection).

### ✅ `$perspective` phantom variable in `queryToSurrealQL` — RESOLVED

**Root cause:** Every graph traversal filter in `queryToSurrealQL` and `buildGraphTraversalWhereClause` contained `WHERE perspective = $perspective AND ...`. The `link` table has **no `perspective` field** — each perspective is its own isolated SurrealDB database. The variable `$perspective` was also never substituted (unlike `surreal_query_notification` which substitutes `$perspectiveId`).

**Fix applied:** All `perspective = $perspective AND` fragments removed. Isolation is guaranteed at the database level, not by a field filter.

**Verified:** Scenario 08 tests unaffected — filter was a no-op.

### ✅ Perspective not cleared between test runs — RESOLVED

**Fix applied:** Scenario 08 now clears all links at the start of `run()` using `new LinkQuery({})` (plain `{}` fails the TypeScript structural check since the installed `LinkQuery` type has a required `isMatch` method).

### ✅ `save()` on re-save leaves stale property links (`AddLink` instead of `SetSingleTarget`) — RESOLVED

**Root cause (Rust):** `create_subject` in `perspective_instance.rs` merges SHACL-derived setter commands into the constructor command list. The merge logic was:

```rust
// OLD — only replaces target; action stays as AddLink from constructor
cmd.target = Some(target_value.clone());
```

This preserved the constructor's `action: AddLink` even when the SHACL setter said `action: SetSingleTarget`. On re-save, `AddLink` fires again, leaving both the old link and the new link in the graph. `getData()` reads the latest-timestamp link so the value appeared unchanged.

**Fix applied** (`perspective_instance.rs`):

```rust
// NEW — replaces the entire command struct, preserving SetSingleTarget
*cmd = Command {
    target: Some(target_value.clone()),
    ..setter_cmd.clone()
};
```

This ensures `SetSingleTarget` is used, which removes the old link before adding the new one.

**Status:** Fix written to `perspective_instance.rs`. Requires `cargo build --release -p ad4m-executor` to activate.

### ✅ `save()` re-run adds duplicate `has_child` and `@Flag` links — RESOLVED

**Root cause (TypeScript):** `save()` unconditionally called `createSubject` and added a `has_child` link on every call, including when the instance already existed. `createSubject` fires the constructor, which re-runs `AddLink` for every `@Flag` field, producing duplicate flag links and a growing `has_child` link set.

**Fix applied** (`Ad4mModel.ts`): `save()` now performs a SurrealDB existence check before branching:

- **New instance** (`isNew === true`): create path — `createSubject` + `has_child` + `innerUpdate(false)` for relations
- **Existing instance** (`isNew === false`): update path — `innerUpdate(true)` only; no `createSubject`, no `has_child`

`update()` is now a `@deprecated` alias that simply calls `save()`. It is kept for backwards compatibility only and will be removed in a future major version.

---

## Architectural Notes

### How `executeAction` vs `createSubject` differ

`save()` never calls `executeAction` directly. It calls `createSubject`, which goes:

1. Rust fetches the SHACL-derived constructor actions from the perspective's link graph
2. Merges in property setter actions
3. Calls `execute_commands` with the merged set

`addLocations()` calls `executeAction` directly with a TypeScript-generated action:

```typescript
[
  {
    action: "addLink",
    source: "this",
    predicate: "we://has_location",
    target: "value",
  },
];
```

These two paths both eventually hit `execute_commands` in Rust, but the action-building provenance is different. If SHACL-derived actions have different field shapes than TypeScript-generated ones, the Rust deserializer may reject or silently ignore the TypeScript-generated form.

**Implication for Phase 3:** The collection adder/remover/setter should ideally also go through SHACL-derived actions (like `createSubject` does for properties). `generateCollectionAction` is a TypeScript reimplementation of what `get_collection_adder_actions` already does in Rust from SHACL. These two implementations need to stay in sync — or better, consolidate so the TypeScript side fetches SHACL-derived actions rather than generating them independently. Filed for Phase 3 planning.

### `__collections` prototype mutation inheritance bug

Documented fully in Phase 4a. Currently affects any `@HasMany`-decorated class that is subclassed. The `|| {}` fallback in decorator code finds the parent's `__collections` object and writes into it rather than creating a child-specific one. The `Block` TODO comment in `Block.ts` (about `ImageBlock extends Block`) will hit this bug immediately when implemented.

### `declare` statements vs `HasManyMethods<T>` utility

`declare add${Capitalize<K>}` statements co-located with each `@HasMany` decorator are the current approach. They are accurate (TypeScript knows the method exists) but repetitive — every new `@HasMany` property needs 3 declare lines. The `HasManyMethods<Keys extends string>` utility exported from `decorators.ts` is available as an alternative:

```typescript
interface Space extends HasManyMethods<"locations"> {}
// generates addLocations, removeLocations, setLocations
```

Post-rename (`set*` is now live), `HasManyMethods` uses `` `set${Capitalize<K>}` `` and the `declare` stubs are just `declare setLocations`, `declare setComments`, etc.

---

- **SHACL as single source of truth** — model definitions live in SHACL only
- **SurrealDB as default query engine** — all `Ad4mModel` queries use SurrealQL
- **Prolog as an explicit, opt-in tool** — available for hand-crafted queries where it has unique advantages (recursive traversal, constraint solving, multi-hop reachability), but never as a hidden fallback inside the model system
- **SHACL → Prolog derivation** — a utility that converts a model's SHACL definition into Prolog predicate facts, so developers who want to write Prolog queries get the predicate structure for free without hardcoding URIs
- **No surprise query engine switching** — a `findAll()` call never silently falls back to a different engine
- **JSON-first query API** — `Query<T>` object is the canonical form; the fluent `ModelQueryBuilder` is ergonomic sugar that builds a `Query<T>` under the hood and delegates to `findAll()`. One execution path, two syntaxes

---

## Phase 1 — Prolog Removal + Prolog Bridge ✅ COMMITTED (`bd3a7b6c`)

These two things go together: remove the dead auto-query path, and put the
intentional Prolog-from-SHACL bridge in place first so there is a verified
alternative before anything is deleted.

### 1a — SHACL-to-Prolog Fact Generator

Add `core/src/model/prolog/generatePrologFacts.ts`.

Given a model class, read its SHACL metadata (already generated by `generateSHACL()` in `ModelOptions`) and emit Prolog predicate clauses:

```typescript
// Input: Poll model with predicates rdf://title, rdf://description, flux://entry_type
// Output:
poll(X) :- triple(X, 'flux://entry_type', 'flux://has_poll').
poll_title(X, Title) :- triple(X, 'rdf://title', Title).
poll_description(X, Desc) :- triple(X, 'rdf://description', Desc).
```

API sketch:

```typescript
import { generatePrologFacts } from "@coasys/ad4m/model/prolog";

const facts = generatePrologFacts(Poll);
const result = await perspective.infer(`
  ${facts}
  recent_popular_poll(X) :-
    poll(X),
    poll_vote_count(X, N), N > 10,
    poll_created_at(X, T), T > ${yesterday}.
`);
```

The Rust executor (`shacl_parser.rs`) already does the equivalent server-side.
This is the TypeScript client-side counterpart.

### 1b — Remove Dead Ad4mModel Prolog Paths

Once 1a is in place and tested, delete from `Ad4mModel.ts` and `decorators.ts`:

| Item                                               | Location                                                  | Action                                         |
| -------------------------------------------------- | --------------------------------------------------------- | ---------------------------------------------- |
| `queryToProlog()`                                  | `Ad4mModel.ts:970`                                        | Delete                                         |
| `instancesFromPrologResult()`                      | `Ad4mModel.ts:1556`                                       | Delete                                         |
| `countQueryToProlog()`                             | `Ad4mModel.ts:~2159`                                      | Delete                                         |
| `useSurrealDB: boolean` params                     | `findAll`, `find`, `findPage`                             | Delete params, always use SurrealDB            |
| Prolog fallback branches                           | `findAll`, `find`, `findPage`, `ModelQueryBuilder`        | Delete `else` branches                         |
| `prologGetter` / `prologSetter`                    | `PropertyOptions` interface + `Optional()` + `Property()` | Delete                                         |
| `@InstanceQuery` decorator                         | `decorators.ts:26–145`                                    | Delete entirely — use the query API instead    |
| `makeRandomPrologAtom` (used for base expressions) | `decorators.ts:523`                                       | Rename to `makeRandomId`, keep in `util.ts`    |
| `generateSDNA()` remnants                          | `decorators.ts`                                           | Delete (already replaced by `generateSHACL()`) |

### 1c — Delete Subject.ts

`Subject.ts` only exists to support the `@InstanceQuery` Prolog path.
Once 1b deletes `@InstanceQuery` entirely, delete `Subject.ts`.

**Additional motivation:** `Subject.ts` contains an `eval(setter.Setter)` call that the rollup build already warns about. `eval` is a CSP violation, suppresses V8 JIT, and is a potential injection vector. Deleting `Subject.ts` eliminates this entirely — see open issue in Issue Log.

---

## Phase 2 — Decorator Cleanup ✅ COMMITTED (`bd3a7b6c`)

The current decorator names have ergonomic problems:

- `@Optional({ required: true })` is a logical contradiction
- `@Property` and `@ReadOnly` are just `@Optional` with preset args — the hierarchy isn't obvious and adds nothing
- `@Collection` doesn't communicate direction or relationship type

`@ReadOnly` and `@Optional` are all deleted — no deprecated aliases. Both
known consumers (`@we/models`, `flux/packages/api`) are under our control and the
migration is mechanical. Keeping aliases would just be cleanup debt with no benefit.

`@Flag` is kept as its own decorator (not collapsed into `@Property`) because it has
genuinely distinct semantics: it writes a fixed predetermined value on every save,
exists purely for graph queryability, and signals to `findAll()` to use it as a filter
condition. `@Property({ flag: true, value: '...' })` would bury that intent. The name is
kept from the original — it already works, has no prior art confusion, and "flagging a
node as a certain type" is natural language in graph contexts.

**Note on naming — `@Property` not `@Field`:** The scalar decorator is named `@Property`
(not `@Field`) because the rest of the API vocabulary is ORM-family: `@HasMany`,
`@BelongsToOne`, `@HasOne`, `@BelongsToMany`. Those names come from ActiveRecord, not
GraphQL. `@Field` would be the natural pairing for a GraphQL-centric API; `@Property`
is the natural pairing for an ORM-centric one. `metadata.properties` and
`metadata.relations` are the corresponding metadata keys.

### 2a — Changes

| Old             | New         | Notes                                                                   |
| --------------- | ----------- | ----------------------------------------------------------------------- |
| `@Optional`     | `@Property` | **Deleted** — use `@Property` directly                                  |
| `@Property`     | `@Property` | **Kept** — renamed from old `@Optional`; pairs with the ORM vocabulary  |
| `@ReadOnly`     | `@Property` | **Deleted** — was `@Property({ writable: false })`, use that explicitly |
| `@Collection`   | `@HasMany`  | Renamed with relationship semantics                                     |
| `@Flag`         | `@Flag`     | **Unchanged** — already the right name                                  |
| `@ModelOptions` | `@Model`    | Shorter, declarative — "this class is a model", not "here are options"  |

**Generated collection method rename — `setCollection*` → `set*`:** ✅ **IMPLEMENTED** (`ad4m@46e140e2`)

The `setCollection` prefix on the bulk-replace method (`setCollectionComments`, etc.) is
a leftover from the `@Collection` era. Now that the decorator is `@HasMany`, keeping
`setCollectionComments` on a `@HasMany`-decorated property is an inconsistency.

Rename to drop the `Collection` infix — `setComments`, `setLocations`, etc. — matching the
symmetry of `addComments` / `removeComments` / `setComments`.

Files to update in `ad4m/core` as part of this PR:

| File                  | Change                                                                                                                                                                                                                                                                                                                                 |
| --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Ad4mModel.ts`        | `\`setCollection${cap}\`` → `\`set${cap}\`` (line ~487)                                                                                                                                                                                                                                                                                |
| `decorators.ts`       | Two stub assignments: `setCollection${capitalize(value)}` → `set${capitalize(value)}`                                                                                                                                                                                                                                                  |
| `PerspectiveProxy.ts` | Replace `startsWith("setCollection")` / `!startsWith("setCollection")` guards with `startsWith("set")` / `!key.match(/^set[A-Z]/) \|\| isCollectionSetter(key)` — needs a reliable way to distinguish property setters (`setName`) from collection setters (`setComments`). Use the `__collections` metadata on the instance to check. |
| `util.ts`             | `collectionSetterToName`: slice at 3 not 13; `collectionToSetterName`: emit `set${capitalize(...)}` not `setCollection${capitalize(...)}`                                                                                                                                                                                              |
| `HasManyMethods` type | Update mapped key template from `` `setCollection${Capitalize<K>}` `` to `` `set${Capitalize<K>}` ``                                                                                                                                                                                                                                   |

**`PerspectiveProxy` disambiguation note:** The current code uses the `setCollection`
prefix as a sentinel to tell collection setters apart from property setters like
`setName`. After the rename, both start with `set`. The reliable discriminant is
`__collections` metadata already stored on the prototype — check
`obj.__collections?.[lowercasedSuffix]` to determine if a `set*` method is a collection
setter or a property setter. This is more correct than string prefix matching anyway.

**`baseExpression` → `id` public alias:** ✅ **IMPLEMENTED**

`baseExpression` is the internal term used throughout `Ad4mModel.ts`. For consumers it is
unnecessarily verbose and leaks implementation vocabulary ("base expression" is an AD4M
graph concept; `id` is universal). A public getter alias was added in Phase 2:

```typescript
// Ad4mModel.ts
get id(): string {
  return this.baseExpression;
}
```

`baseExpression` stays as-is internally (renaming it would be a much larger refactor with
no benefit). The public `id` getter is purely additive — no breaking changes. Once added,
consumers should use `id`; `baseExpression` remains but is considered internal.

This also eliminates the need for model classes (like `Space`) to define a manual `uuid`
field just to have a stable, queryable identifier — `id` already IS the unique identifier,
and `where: { base: instance.id }` is the correct query pattern.

**`@Flag` and predicate uniqueness:**

AD4M's link graph has no implicit type system — the only things written when a model
instance is saved are the links defined by its constructor actions. This means
`findAll()` can only identify instances of a model by their predicate pattern.

If two models share the same set of predicates, their queries are ambiguous —
`Task.findAll()` and `Note.findAll()` would return the same instances if both only
define `ad4m://title`. This is true regardless of whether the models share a common
base class.

**Rule:** Every model must be unambiguously identifiable in the graph. This means either:

- At least one `@Flag` (a fixed predicate+value written on every instance), **or**
- At least one `@Property({ required: true })` whose predicate is not used by any other model in the same perspective

This is not currently enforced at runtime — it is a developer responsibility. A future
validation pass in `getModelMetadata()` could warn when neither condition is met.

### 2b — New Relationship Decorators

These are net-new additions, not renames. They extend what `@HasMany` (old `@Collection`)
can express:

| Decorator        | Cardinality  | Direction                            | Graph query pattern                                                        |
| ---------------- | ------------ | ------------------------------------ | -------------------------------------------------------------------------- |
| `@HasOne`        | one target   | forward: source → predicate → target | `SELECT target FROM link WHERE source = $base AND predicate = 'p' LIMIT 1` |
| `@HasMany`       | many targets | forward: source → predicate → target | `SELECT target FROM link WHERE source = $base AND predicate = 'p'`         |
| `@BelongsToOne`  | one source   | reverse: source → predicate → target | `SELECT source FROM link WHERE target = $base AND predicate = 'p' LIMIT 1` |
| `@BelongsToMany` | many sources | reverse: source → predicate → target | `SELECT source FROM link WHERE target = $base AND predicate = 'p'`         |

This gives a symmetric set: `@HasOne` / `@HasMany` are forward traversals; `@BelongsToOne` /
`@BelongsToMany` are reverse traversals. `@BelongsToOne` and `@BelongsToMany` produce the same
underlying query — the distinction is semantic cardinality (shapes the TypeScript type as
`Model` vs `Model[]`) and documents intent, exactly the same way `@HasOne` vs `@HasMany`
does on the forward side.

**`@ManyToMany` is intentionally omitted.** In AD4M's directed link graph there is no
join-table concept — a many-to-many relationship is `@HasMany` on one side and
`@BelongsToMany` on the other, which is the correct way to express it.

The full symmetric set is:

|          | **Forward** | **Reverse**      |
| -------- | ----------- | ---------------- |
| **One**  | `@HasOne`   | `@BelongsToOne`  |
| **Many** | `@HasMany`  | `@BelongsToMany` |

**Hydration note:** In Phase 2, all relationship decorators still return `string[]` (base
expression IDs), matching current `@Collection` behaviour. Automatic hydration to typed
model instances (`Message[]` instead of `string[]`) is a separate concern added later via
`.include()` on the query builder — see Phase 3 Key Architectural Notes.

**`@BelongsToOne` / `@BelongsToMany` options:**

```typescript
@BelongsToOne(() => Channel, {
  through: 'ad4m://has_child',  // predicate to traverse in reverse
  as?: string,                  // property name alias
  local?: boolean,
})
channel?: Channel;              // typed as single instance

@BelongsToMany(() => Post, {
  through: 'flux://has_tag',
  as?: string,
  local?: boolean,
})
posts: Post[] = [];             // typed as array
```

**`@HasOne` options:** Same as `@HasMany` options — enforced at the application level,
not at the link level (the graph itself doesn't prevent multiple links).

### 2c — Wiring New Decorators into generateSHACL()

The new relationship decorators must produce valid SHACL property shapes, or the entire
downstream stack (SurrealDB queries, `shacl_parser.rs`, `generatePrologFacts()`) will be
unaware they exist. This is not automatic — it requires explicit additions.

**`@HasOne` → `maxCount: 1`**

`@HasOne` is a forward traversal like `@HasMany`, just with a cardinality constraint.
`generateSHACL()` already emits collection shapes with no `maxCount`. For `@HasOne`, add
`maxCount: 1` to the shape:

```typescript
// @HasOne emits the same shape as @HasMany but with:
propShape.maxCount = 1;
```

This is a small, low-risk addition.

**`@BelongsToOne` / `@BelongsToMany` → `sh:inversePath`**

Reverse traversals require a different SHACL path expression. SHACL natively supports
this via `sh:inversePath`:

```turtle
# Forward (@HasMany): source --flux:hasMessage--> target
sh:property [
  sh:path flux:hasMessage ;
  sh:nodeKind sh:IRI ;
]

# Reverse (@BelongsToMany): find all X where X --flux:hasMessage--> this
sh:property [
  sh:path [ sh:inversePath flux:hasMessage ] ;
  sh:nodeKind sh:IRI ;
]
```

The current code has zero support for `sh:inversePath`. The following changes are
required across the stack:

| Layer                      | Change                                                                                                                                |
| -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `SHACLPropertyShape` type  | Add `inversePath?: boolean` field                                                                                                     |
| `generateSHACL()`          | When decorator is `@BelongsToOne`/`@BelongsToMany`, set `inversePath: true` on the shape                                              |
| `SHACLShape` serialization | Emit `{ sh:inversePath: predicate }` instead of bare `sh:path` when `inversePath` is set                                              |
| `shacl_parser.rs` (Rust)   | Detect `sh:inversePath` and emit reverse Prolog predicates (e.g. `channel_of(X, Channel) :- triple(Channel, 'ad4m://has_child', X).`) |
| `generatePrologFacts.ts`   | Handle `inversePath` shapes — emit reverse predicate clause                                                                           |
| `SurrealQueryBuilder.ts`   | When building hydration queries for `@BelongsToOne`/`@BelongsToMany`, query `WHERE target = $base` not `WHERE source = $base`         |

**How `generateSHACL()` knows which decorator was used:**

The decorator stores its kind on the metadata:

```typescript
// In @Collection / @HasMany / @HasOne decorator:
target["__collections"][key] = { ...opts, direction: "forward" };

// In @BelongsToOne / @BelongsToMany decorator:
target["__collections"][key] = { ...opts, direction: "reverse" };
```

`generateSHACL()` checks `collMeta.direction === 'reverse'` and sets `inversePath: true`.

**Cardinality in reverse decorators:**

SHACL `sh:inversePath` shapes support `maxCount` exactly the same way forward shapes do.
`@BelongsToOne` adds `maxCount: 1` to the inverse shape — semantically meaning "this
instance belongs to at most one parent via this predicate".

### Migration Strategy — Hard Rename, No Aliases

All decorator renames are hard — no deprecated alias re-exports. Both known consumers
(`@we/models` and `flux/packages/api`) are under our control, the rename is purely
mechanical, and aliases would need to be cleaned up anyway in a later PR for no benefit.

**Consumer footprint (confirmed by grep):**

| Consumer                   | Files                                                          | Action                                                         |
| -------------------------- | -------------------------------------------------------------- | -------------------------------------------------------------- |
| `we/packages/models`       | 5 files (Space, Block, TextBlock, CollectionBlock, ImageBlock) | Updated in the same PR as Phase 2                              |
| `flux/packages/api/src/`   | ~15 model files                                                | Separate follow-up PR in Flux after test app validates Phase 2 |
| `ad4m/bootstrap-languages` | 0 files                                                        | No changes needed                                              |

The Flux rename is a pure find-and-replace across one package — no behaviour changes.
It is naturally a separate PR because Flux is a separate monorepo.

### What Gets Dropped

- `@Optional`, `@ReadOnly` — deleted entirely, use `@Property` with explicit options (`required`, `writable`)
- `prologGetter` / `prologSetter` options (removed in Phase 1)
- `@InstanceQuery` entirely (removed in Phase 1) — replaced by static methods using the query API
- `through` being optional when `prologGetter` was set (now always required)

---

## Phase 3 — File Decomposition ✅ COMPLETE (3a/3b/3c/3d all done)

`Ad4mModel.ts` is currently 3,404 lines containing two exported classes, ~15 distinct concerns, and a mix of pure functions and instance/static methods. Split it:

```
core/src/model/
  index.ts                        # public barrel — single import point for consumers
  types.ts                        # Query, Where, Order, PropertyMetadata,
                                  # CollectionMetadata, ModelMetadata,
                                  # ResultsWithTotalCount, PaginationResult, etc.
  Ad4mModel.ts                    # class core only (~800 lines)
                                  #   constructor, private fields
                                  #   getModelMetadata()
                                  #   getData(), assignValuesToInstance()
                                  #   setProperty, setCollection*, innerUpdate, cleanCopy
                                  #   save(), update(), delete()
                                  #   thin findAll/find/count wrappers
                                  #   static query() → new ModelQueryBuilder(this, perspective)
  decorators.ts                   # renamed decorators (Phase 2)
  util.ts                         # makeRandomId, capitalize, etc.

  query/
    index.ts
    SurrealQueryBuilder.ts        # queryToSurrealQL, buildGraphTraversalWhereClause,
                                  # buildSurrealWhereClause, buildSurrealSelectFields,
                                  # formatSurrealValue, countQueryToSurrealQL,
                                  # matchesCondition — all pure functions, no Ad4mModel dep
                                  # ⚠️ migrate string interpolation → parameterized queries
                                  #    throughout (see open issue in Issue Log)
    hydration.ts                  # hydrate<T>(ctor, perspective, query, result) → instances
                                  # + hydrateRelation() for include resolution
                                  # takes ctor as parameter to avoid circular import
                                  # ⚠️ consolidates getData() + instancesFromSurrealResult()
                                  #    into one shared hydrateInstance() — eliminates
                                  #    dual-implementation divergence (see open issue)
    QueryBuilder.ts               # ModelQueryBuilder<T> fluent class — includes .include()
    include.ts                    # resolveIncludes() — walks Include<T> tree, batches fetches

  schema/
    fromJSONSchema.ts             # fromJSONSchema static + determineNamespace,
                                  # determinePredicate, getPropertyOption,
                                  # getDefaultValueForType, normalizeNamespaceString,
                                  # normalizeSchemaType, isSchemaType, isArrayType,
                                  # isObjectType, isNumericType
                                  # + JSONSchemaProperty, JSONSchema, JSONSchemaToModelOptions

  prolog/
    index.ts
    generatePrologFacts.ts        # SHACL → Prolog fact generator (from Phase 1a)

  __tests__/
    Ad4mModel.test.ts             # moved from current location
```

### Key Architectural Notes

**Circular dependency management:** `hydration.ts` takes `ctor: new(...) => Ad4mModel` as a
parameter instead of importing `Ad4mModel` directly. This is the one move that makes the
whole split viable without `tsconfig` path hacks.

**`fromJSONSchema` special case:** This method sets `prototype.__jsonSchema` and
`prototype.__jsonSchemaOptions` as fallback paths in `getModelMetadata()`. This must be
preserved in the `schema/fromJSONSchema.ts` extraction — it bypasses standard decorators
and has a separate metadata path.

**Public API stays the same:** All current exports from `Ad4mModel.ts` and `decorators.ts`
are re-exported from `index.ts`. No consumer changes required for the decomposition alone.

### 3b — Transaction API ✅ COMPLETE (`a66d833b`)

Currently `save()`, `update()`, and `delete()` each accept an optional `batchId?: string`.
Internally, if no `batchId` is provided, each call creates its own batch and commits it
immediately. If one is provided, the caller is responsible for `perspective.createBatch()`
and `perspective.commitBatch()` — and for cleanup if something throws midway.

This is fragile. Replace it with a `transaction()` wrapper:

```typescript
await Ad4mModel.transaction(perspective, async (tx) => {
  await post.save(tx);
  await comment.save(tx);
  await tag.save(tx);
  // auto-commits when the callback returns
  // auto-aborts if the callback throws — no orphaned batch
});
```

`tx` is a thin `TransactionContext` object that holds the `batchId` and passes it
transparently to every `executeAction` call. The raw `batchId` string never leaks to
the caller.

**`TransactionContext` type** (internal, not exported):

```typescript
type TransactionContext = {
  readonly batchId: string;
};
```

**`save` / `update` / `delete` signatures** change from `batchId?: string` to
`tx?: TransactionContext`. Passing a raw string is no longer valid — a breaking change,
but one that makes misuse a type error rather than a runtime bug.

**`Ad4mModel.transaction()` implementation sketch:**

```typescript
static async transaction(
  perspective: PerspectiveProxy,
  fn: (tx: TransactionContext) => Promise<void>
): Promise<void> {
  const batchId = await perspective.createBatch();
  const tx: TransactionContext = { batchId };
  try {
    await fn(tx);
    await perspective.commitBatch(batchId);
  } catch (e) {
    await perspective.abortBatch(batchId); // if PerspectiveProxy supports it; else just rethrow
    throw e;
  }
}
```

**Migration note:** `batchId?: string` is removed entirely — no deprecated overload.
Any existing call sites passing a raw `batchId` string will be a type error, which is
the desired outcome. The Flux API models don't use `batchId` directly (confirmed by grep),
so this is a zero-impact change in practice.

### 3c — Include / Eager Loading ✅ COMPLETE (`6d02ad2d`)

> **Implementation note:** The planned `Include<T>[]` array format (with a `relation` key per object) was replaced with a Prisma-style `IncludeMap = { [relationName: string]: true | Query }`. Semantics: absent = no hydration (not "hydrate all"), present = exactly the named relations. Nested recursive `include` arrays and the separate `include.ts` module are deferred — the current IncludeMap covers the immediate use case with less API surface. The JSON-first query API redesign can be revisited if nested eager loading becomes necessary.

This is what completes Phase 2's relationship decorators. In Phase 2, `@HasMany` /
`@HasOne` / `@BelongsToOne` / `@BelongsToMany` still return `string[]` (base expression IDs).
Phase 3c wires up `include` so those are hydrated into typed model instances.

#### Query API Design: JSON canonical, fluent as sugar

The `Query<T>` JSON object is the **single canonical form**. `findAll` accepts it directly.
The fluent `ModelQueryBuilder` is purely ergonomic — it accumulates fields into a `Query<T>`
and calls `findAll()` on `.exec()`. There is one execution path.

**Why JSON-first:**

- Nesting is unambiguous — `where` at every level clearly belongs to that level
- Serializable — queries can be stored, logged, sent over the wire
- Composable — `{ ...baseQuery, limit: 5 }` just works
- Single code path to maintain; fluent adds zero new logic

```typescript
// ── JSON form (canonical) ───────────────────────────────────────────────────
const channels = await Channel.findAll(perspective, {
  where: { visibility: "public" },
  include: [
    {
      relation: "messages",
      order: { timestamp: "DESC" },
      limit: 20,
      include: [{ relation: "author" }], // Message @BelongsToOne(() => User)
    },
  ],
});
// channels[0].messages[0].author.name ✓

// ── Fluent form (sugar — builds the same Query<T> object under the hood) ────
const channels = await Channel.query(perspective)
  .where({ visibility: "public" })
  .include([
    {
      relation: "messages",
      order: { timestamp: "DESC" },
      limit: 20,
      include: [{ relation: "author" }],
    },
  ])
  .exec();
```

`ModelQueryBuilder.exec()` is literally:

```typescript
exec(): Promise<T[]> {
  return this.ctor.findAll(this.perspective, this.queryParams);
}
```

**New types added to `types.ts`:**

```typescript
export type Include<T = any> = {
  relation: keyof T; // must be a relationship-decorated property
  where?: Where; // filter the related instances
  order?: Order; // order the related instances
  limit?: number; // cap related results
  include?: Include[]; // recursive — nested eager loading
};

export type Query<T = any> = {
  source?: string;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
  properties?: string[];
  collections?: string[];
  include?: Include<T>[]; // NEW
};
```

**Implementation in `query/include.ts`:**

`resolveIncludes(instances, includes, perspective)` runs after the primary query
hydrates the base instances. For each `Include`:

1. Reads the relationship decorator metadata to get the predicate and direction
2. Collects all source/target IDs from the already-hydrated instances
3. Issues a **single batched SurrealDB query** for all related IDs (not N+1)
4. Hydrates the related instances and attaches them to their parents
5. Recurses for nested `include` arrays

**Depth limit:** Default `maxIncludeDepth: 3`, configurable per query:

```typescript
Channel.findAll(perspective, { include: [...], maxIncludeDepth: 5 })
```

Cycles detected by tracking the constructor chain in the current recursion — if the
same class appears twice, that branch stops.

**Lazy by default:** Without `include`, relationship properties remain `string[]`.
No automatic eager loading. P2P bandwidth is a first-class concern.

### 3d — Subscriptions ✅ COMPLETE

**Implementation note:** The approach below was fully implemented in `subscription.ts` and wired into `Ad4mModel.subscribe()` and `ModelQueryBuilder.subscribe()`. Key architectural decision: subscriptions are **client-side only** — they attach to PerspectiveProxy's link-added/link-removed listeners and re-run a SurrealQL query locally on each relevant change. There is no server-side subscription state, no keepalive loop, and no GraphQL subscription channel involved. See [Phase 3e](#3e--subscription-infrastructure-cleanup) for removal of the old server-push system this replaces.

> Not a flag on `Query<T>`. Mixing it into the

query object would make the return type conditional on a runtime boolean, forcing messy
overloads on `findAll`. The query object stays purely descriptive of _what_ data is wanted;
subscribe is about _how_ it is delivered.

**Static method:**

```typescript
const sub = Channel.subscribe(
  perspective,
  {
    where: { visibility: "public" },
    include: [{ relation: "messages", limit: 20 }],
    debounce: 300, // ms — batches rapid link changes, delivery concern not query concern
  },
  (channels: Channel[]) => {
    // called immediately with initial results, then on every relevant link change
  },
);

sub.unsubscribe();
```

**Fluent form** — `.subscribe()` as a terminal alongside `.exec()`:

```typescript
const sub = Channel.query(perspective)
  .where({ visibility: 'public' })
  .include([{ relation: 'messages', limit: 20 }])
  .subscribe((channels) => { ... }); // vs .exec() for one-shot

sub.unsubscribe();
```

`ModelQueryBuilder.subscribe()` is just:

```typescript
subscribe(callback: (results: T[]) => void): Subscription {
  return this.ctor.subscribe(this.perspective, this.queryParams, callback);
}
```

**`Subscription` type** (added to `types.ts`):

```typescript
export type Subscription = {
  unsubscribe(): void;
  lastError: Error | null;
};
```

**`debounce`** belongs on the subscribe call, not in `Query<T>`. When any relevant link
changes, the full query re-runs including all its includes. With deep include trees this
means a chain of batched SurrealDB queries on every change — debouncing prevents thrashing
during rapid writes. Since it has no meaning for `findAll`, it is not part of `Query<T>`.

**Implementation:** `Channel.subscribe()` calls `perspective.addListener('link-added', ...)` and `perspective.addListener('link-removed', ...)`, checks whether the changed link is relevant to the query's predicates, and if so re-runs `findAll(perspective, query)` and invokes the callback with the fresh results.

**Error handling:**

There are two distinct async failure paths, both invisible without explicit handling:

1. **The re-query throws** — `findAll()` fails (e.g. SurrealDB temporarily unavailable).
   Fires from inside an async link listener, so the exception has no caller to propagate
   to and will become a silent unhandled promise rejection.
2. **The callback throws** — the consumer's handler itself throws. Same problem.

Both are handled via an optional `onError` parameter on the subscribe options:

```typescript
const sub = Channel.subscribe(
  perspective,
  {
    where: { visibility: "public" },
    debounce: 300,
    onError: (err) => console.error('Channel subscription failed:', err),
  },
  (channels) => { ... },
);
```

If `onError` is not provided, the implementation falls back to `console.error` — errors
are at least visible in the dev console without requiring every caller to handle them.
The implementation wraps both failure sites:

```typescript
// Inside the link listener:
try {
  const results = await findAll(perspective, query);
  try {
    callback(results);
  } catch (callbackErr) {
    (options.onError ?? console.error)(callbackErr);
  }
} catch (queryErr) {
  (options.onError ?? console.error)(queryErr);
}
```

**`Subscription` type** updated to expose the last error for polling UIs:

```typescript
export type Subscription = {
  unsubscribe(): void;
  lastError: Error | null; // null until a failure occurs
};
```

This means a UI component can show a "reconnecting…" state without needing to wire up
an `onError` callback just to track whether the subscription is healthy.

---

### 3e — Subscription Infrastructure Cleanup ⏳ PENDING

The old server-push subscription system predates the client-side `Ad4mModel.subscribe()` approach. Now that 3d is complete and all consumers have migrated, the old machinery can be removed. It is dead code from the `Ad4mModel` perspective — zero external callers for `subscribeSurrealDB`.

**What it was:** `PerspectiveProxy.subscribeSurrealDB()` / `subscribeInfer()` (SurrealDB path only) used a round-trip GraphQL architecture:

1. Client calls `perspectiveSubscribeSurrealQuery` mutation → Rust registers a `SurrealSubscribedQuery` entry
2. Rust re-runs the query on every link change, pushes result via pubsub → `perspectiveQuerySubscription` GQL subscription
3. `QuerySubscriptionProxy` receives updates + sends a keepalive mutation every 30s to prevent timeout
4. On dispose, client calls `perspectiveDisposeSurrealQuerySubscription` mutation

**Why the client-side approach is better:**

- No server state: no keepalive loop, no timeout/cleanup complexity, no subscription ID management
- No round-trip latency: re-query runs immediately in the same process that received the link event
- Simpler failure modes: if the client disconnects, there is nothing to clean up on the server
- Composable with `IncludeMap`: include hydration runs client-side anyway — the server never had visibility into it

**What `subscribeInfer` is NOT:** `subscribeInfer` is the Prolog query subscription system. It is completely separate, actively used in `tests/js`, and **must not be touched**. Only the SurrealDB branch of `QuerySubscriptionProxy` is dead.

**Scaling considerations for the future:**

- The client-side approach works well for a single connected client with a persistent WebSocket to the executor. For multi-client server deployments (e.g. a REST API serving many agents), the link-listener approach cannot work — each HTTP request has no persistent connection. In that scenario, the server-push model is correct. The right path is to reintroduce server-side SurrealDB LIVE queries (native SurrealDB feature) rather than the polling loop we're removing.
- Debounce is the main scaling knob for busy perspectives. `createSubscription()` shares a single listener entry per unique query fingerprint across all subscribers — adding 10 React components subscribed to the same query does not add 10 listeners.
- If CPU becomes a concern (many concurrent subscriptions × large perspectives), the fix is to make `checkPredicateRelevance()` more precise before re-running the query. Currently it checks if any changed link predicate appears in the query's known predicates. A Bloom-filter variant or predicate index could reduce false positives.

**TypeScript removals (PerspectiveProxy.ts / PerspectiveClient.ts):**

| Symbol                                                             | File                     | Action                                  |
| ------------------------------------------------------------------ | ------------------------ | --------------------------------------- |
| `subscribeSurrealDB()`                                             | `PerspectiveProxy.ts`    | Delete                                  |
| `isSurrealDB` field                                                | `QuerySubscriptionProxy` | Delete — Prolog path doesn't need it    |
| SurrealDB branches in `subscribe()` / `dispose()` / keepalive loop | `QuerySubscriptionProxy` | Delete `if (this.isSurrealDB)` branches |
| `perspectiveSubscribeSurrealQuery()`                               | `PerspectiveClient.ts`   | Delete                                  |
| `perspectiveKeepAliveSurrealQuery()`                               | `PerspectiveClient.ts`   | Delete                                  |
| `perspectiveDisposeSurrealQuerySubscription()`                     | `PerspectiveClient.ts`   | Delete                                  |

**Rust removals (perspective_instance.rs / mutation_resolvers.rs):**

| Symbol                                                             | File                         | Action |
| ------------------------------------------------------------------ | ---------------------------- | ------ |
| `SurrealSubscribedQuery` struct                                    | `perspective_instance.rs`    | Delete |
| `surreal_subscribed_queries` field                                 | `PerspectiveInstance` struct | Delete |
| `trigger_surreal_subscription_check` field                         | `PerspectiveInstance` struct | Delete |
| `subscribe_and_query_surreal()`                                    | `perspective_instance.rs`    | Delete |
| `keepalive_surreal_query()`                                        | `perspective_instance.rs`    | Delete |
| `dispose_surreal_query_subscription()`                             | `perspective_instance.rs`    | Delete |
| `surreal_subscription_cleanup_loop()`                              | `perspective_instance.rs`    | Delete |
| `check_surreal_subscribed_queries()`                               | `perspective_instance.rs`    | Delete |
| `trigger_surreal_subscription_check` setter lines (×2)             | `perspective_instance.rs`    | Delete |
| `perspective_subscribe_surreal_query` mutation resolver            | `mutation_resolvers.rs`      | Delete |
| `perspective_keep_alive_surreal_query` mutation resolver           | `mutation_resolvers.rs`      | Delete |
| `perspective_dispose_surreal_query_subscription` mutation resolver | `mutation_resolvers.rs`      | Delete |

**`surreal_subscription_cleanup_loop` in `run_background_tasks`:** The loop is spawned alongside `subscribed_queries_loop` in the instance background task start. Removing the loop also removes the `surreal_subscription_cleanup_loop()` spawn line.

**`PerspectiveQuerySubscriptionFilter` / `PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC` / `send_subscription_update`:** These are still used by the Prolog `subscribeInfer` path — do NOT remove them.

---

## Phase 4 — WeakMap Metadata Registry + Model Inheritance ✅ COMPLETE

> ⚠️ **Note:** Phase 4a (WeakMap metadata registry) has been identified as a correctness bug fix, not a quality-of-life improvement. It should be implemented in Phase 2, before any model inheritance is attempted. The Phase 4 content below is retained for the full implementation detail; Phase 4b (inheritance patterns) remains a Phase 4 concern.

### 4a — WeakMap Metadata Registry

Currently, decorators write metadata by mutating the class prototype:

```typescript
target["__properties"] = target["__properties"] || {};
target["__properties"][key] = opts;
```

Because `target` is the class prototype, this has a subtle inheritance bug: if
`PollBlock extends BaseBlock` and `PollBlock.prototype` doesn't yet have its own
`__properties`, the `|| {}` check finds `BaseBlock.prototype.__properties` (truthy)
and uses that same object reference — meaning `PollBlock`'s decorators write into
`BaseBlock`'s metadata object. `BaseBlock.getModelMetadata()` then also returns
`PollBlock`'s fields. This is a silent data corruption bug that affects any model
extending another model.

Replace with a module-level `WeakMap` keyed on the constructor function itself:

```typescript
const propertyRegistry = new WeakMap<
  Function,
  Record<string, PropertyOptions>
>();
const collectionRegistry = new WeakMap<
  Function,
  Record<string, CollectionOptions>
>();

// Inside the decorator — no prototype mutation:
const existing = propertyRegistry.get(target.constructor) ?? {};
propertyRegistry.set(target.constructor, { ...existing, [key]: opts });
```

`PollBlock` and `BaseBlock` are different constructor references — they get separate
map entries. The prototype chain mutation bug disappears entirely.

Additional benefits:

- No prototype pollution — `__properties` no longer appears in `Object.keys(instance)`
- Metadata is garbage-collected when the class goes out of scope (WeakMap semantics)
- `getModelMetadata()` becomes a clean function call, not a reflection hack
- Required foundation for tree-shaking and SSR compatibility

**`fromJSONSchema` — second write path that must also be updated:**

Decorators are not the only code that writes `__properties`. `fromJSONSchema()` creates
model classes dynamically and writes metadata directly to the prototype:

```typescript
// Ad4mModel.ts:2754 — current code
DynamicModelClass.prototype.__properties = properties;
DynamicModelClass.prototype.__collections = collections;
```

This bypasses decorators entirely — so if the WeakMap fix only touches the decorator
functions, `fromJSONSchema`-created classes remain on the old prototype-mutation path
and will still exhibit the inheritance bug if subclassed.

The fix: `fromJSONSchema` must write into the same WeakMaps:

```typescript
// After WeakMap fix — fromJSONSchema writes the same way decorators do
propertyRegistry.set(DynamicModelClass, properties);
collectionRegistry.set(DynamicModelClass, collections);
```

`fromJSONSchema` creates a fresh constructor each call so there is no inheritance
ambiguity in the common case — but writing to the WeakMap ensures `getModelMetadata()`
uses the same read path for all classes regardless of how they were created. The
`prototype.__jsonSchema` / `prototype.__jsonSchemaOptions` fields (used as a fallback
in the current `getModelMetadata()`) can remain for now since they carry the raw schema,
not the processed property map.

### 4b — Model Inheritance

With the WeakMap fix in place, true class inheritance becomes safe and useful.
The motivating use case is the WE block system: a `BaseBlock` holding shared
behaviour (comments, reactions) that concrete block types extend.

**True inheritance vs mixins:**

- **True inheritance** (`PollBlock extends BaseBlock`) is correct when the relationship
  is genuinely IS-A. `BaseBlock.findAll()` can return all block types polymorphically,
  `instanceof` works, SHACL `sh:node` natively expresses the constraint hierarchy.
- **Mixins** are the right tool for cross-cutting traits shared across _unrelated_ type
  hierarchies — e.g. a `withReactions()` function shared between `Message` (not a Block)
  and `PollBlock`. Both patterns can coexist; for the WE block system, use inheritance.

**Usage:**

```typescript
@Model({ name: "BaseBlock" })
class BaseBlock extends Ad4mModel {
  @HasMany({ through: "we://hasComment" })
  comments: string[] = [];

  @HasMany({ through: "we://hasReaction" })
  reactions: string[] = [];
}

@Model({ name: "PollBlock" })
class PollBlock extends BaseBlock {
  @Flag({ through: "we://blockType", value: "we://poll" })
  blockType = "we://poll"; // written on every save — makes instances queryable

  @Property({ through: "we://question", required: true })
  question: string = "";
}

// Returns all block types (BaseBlock has no @Flag — no type filter applied):
const allBlocks = await BaseBlock.findAll(perspective);

// Returns only polls (matched by @Flag value):
const polls = await PollBlock.findAll(perspective);
```

**`getModelMetadata()` — prototype chain merge:**

With the WeakMap, `getModelMetadata()` must explicitly walk the prototype chain and
merge parent metadata. Child properties win over parent:

```typescript
static getModelMetadata(): ModelMetadata {
  const allProperties = {};
  const allRelations = {};
  // Walk from root → leaf, so leaf (child) assignments win:
  const chain = [];
  let ctor = this as unknown as Function;
  while (ctor && ctor !== Ad4mModel) {
    chain.unshift(ctor); // prepend — root first
    ctor = Object.getPrototypeOf(ctor);
  }
  for (const c of chain) {
    Object.assign(allProperties, propertyRegistry.get(c) ?? {});
    Object.assign(allCollections, collectionRegistry.get(c) ?? {});
  }
  // ... build ModelMetadata from allProperties + allCollections
}
```

**`generateSHACL()` — `sh:node` parent reference:**

When a model class extends another `@Model`-decorated class, `generateSHACL()` emits
a `sh:node` reference to the parent shape rather than duplicating all its properties:

```turtle
# BaseBlock shape
BaseBlockShape a sh:NodeShape ;
  sh:property [ sh:path we:hasComment ; sh:nodeKind sh:IRI ] ;
  sh:property [ sh:path we:hasReaction ; sh:nodeKind sh:IRI ] .

# PollBlock shape — references BaseBlock, adds only its own properties
PollBlockShape a sh:NodeShape ;
  sh:node BaseBlockShape ;
  sh:property [ sh:path we:blockType ; sh:hasValue we:poll ] ;
  sh:property [ sh:path we:question ; sh:minCount 1 ] .
```

`generateSHACL()` detects a parent model by checking whether the immediate prototype
constructor also exists in the WeakMap registry (i.e. is itself decorated with `@Model`).

---

## Phase 5 — CRDT Ordering ⏳ NOT STARTED

Implement deterministic ordering for concurrent link writes. Details in
`CRDT-ORDERING-STRATEGY.md`. Depends on Phase 3 being complete (query layer needs
to be cleanly separated before ordering logic can be injected cleanly).

---

## Phase G — External Consumer Migration & Prolog Subject Proxy Cleanup ⏳ NOT STARTED

This phase is gated on **flux** and **ad4m-hooks** migrating their `SubjectRepository` implementations to the `Ad4mModel` API. It is not actionable on the `ad4m` side until that migration is complete, but the removal targets are well-understood now and are documented here so they don't get lost.

### Background: What Flux and Hooks Currently Use

**`flux/packages/api/src/factory/SubjectRepository`** (the one inside Flux — separate from `ad4m-hooks`):

- `getAll()` calls `perspective.infer("subject_class(C), instance(C, Base), triple(...).")` — Prolog query
- `getAll()` with pagination calls `perspective.infer("findall([Timestamp, Base], ...)")` — also Prolog
- After `infer()` returns IDs, creates `new Subject(perspective, base, className)` + calls `subject.init()` — Subject proxy (already deleted from `Subject.ts` in Phase 1c, so this is the Rust Subject proxy path via GraphQL)
- `getSubjectData()` reads properties by invoking Subject proxy getters (promise-based async getters), not `perspective.getSubjectData()`

**`ad4m-hooks/helpers/src/factory/SubjectRepository`** (the one inside the `ad4m` monorepo):

- `getSubjectData()` calls `perspective.getSubjectData(this.subject, entry.baseExpression)` — which calls `PerspectiveClient.getSubjectData()` → Rust `get_subject_data()` → 5 separate Prolog queries

### Migration Path for Each Consumer

**Flux `SubjectRepository`:**

Replace `getAll()` + `getSubjectData()` with `Ad4mModel.findAll()`. The Subject proxy pattern (`subject.init()` + async getters) is replaced entirely by the SHACL-backed decorator model. For Flux's paginated case, use `findPage()` or a `limit`/`offset` query.

```typescript
// Before (Flux SubjectRepository.getAllData)
const subjects = await this.getAll(source); // infer() → Prolog
const data = await Promise.all(subjects.map(getSubjectData)); // Subject proxy getters

// After
const data = await MyModel.findAll(perspective, { source });
```

**`ad4m-hooks` `SubjectRepository`:**

Replace `perspective.getSubjectData()` calls with `Ad4mModel.getData()` or `findAll()`. The hooks themselves (`useSubjects`, `useSubject`, etc.) should be wired to `Ad4mModel.subscribe()` for live updates rather than polling.

### What Becomes Removable After Migration

**TypeScript (`PerspectiveProxy.ts` / `PerspectiveClient.ts`):**

| Symbol              | File                   | Action                                                   |
| ------------------- | ---------------------- | -------------------------------------------------------- |
| `getSubjectData()`  | `PerspectiveProxy.ts`  | Delete — only consumer is `ad4m-hooks/SubjectRepository` |
| `getSubjectProxy()` | `PerspectiveProxy.ts`  | Delete — only consumer is `flux/SubjectRepository`       |
| `getSubjectData()`  | `PerspectiveClient.ts` | Delete — no remaining callers                            |

**Rust (`perspective_instance.rs`):**

| Symbol                            | Action | Notes                                                                                                 |
| --------------------------------- | ------ | ----------------------------------------------------------------------------------------------------- |
| `get_subject_data()`              | Delete | Uses 5 Prolog queries: `subject_class`, `instance`, `property`, `property_getter`, `property_resolve` |
| The `getSubjectData` GQL resolver | Delete | The entry point that exposes `get_subject_data` over GraphQL                                          |

**`ad4m-hooks/helpers/src/factory/SubjectRepository.ts`:**

Deprecate and eventually delete the whole class once hooks are rewritten to use `Ad4mModel`. Its `getAll()`/`create()`/`update()` surface maps directly onto `Ad4mModel.findAll()`/`save()`/`save()`. The hooks (`useSubjects` etc.) that depend on it should be rewritten to use `Ad4mModel.subscribe()` for live updates.

**`flux/packages/api/src/factory/SubjectRepository.ts`:**

Deprecate and eventually delete once Flux models are all `@Model`-decorated `Ad4mModel` subclasses. The paginated `getAll()` maps to `findPage()`. The `create()`/`update()` surface maps to `Ad4mModel.save()`.

### Prolog Exposure After Migration

Once Phase G is complete, `infer()` / `subscribeInfer()` remain as **explicit, opt-in** tools for hand-crafted Prolog queries — they are intentionally preserved per the architectural principles. The Prolog facts for model predicates continue to be derived from SHACL via `shacl_to_prolog.rs` so that `infer()` queries can reference model properties by name. What disappears is the **hidden Prolog path** — the one that fired invisibly inside `getSubjectData`, `getSubjectProxy`, and `Subject.init()` without the caller knowing Prolog was involved.

### Prerequisites

- Flux decorator rename (Phase F) must be complete first — there is no point migrating `SubjectRepository` before the model classes themselves use the new decorator API
- `Ad4mModel.findPage()` should be verified with a full integration test before Flux's paginated `getAll()` is migrated

---

## Test Strategy

### `ad4m/tests/js` — canonical integration test suite (post-merge)

The `ad4m/tests/js` Mocha suite is the correct long-term home for all `Ad4mModel` API
tests. However, the suite currently imports deprecated symbols (`Collection`, `ModelOptions`,
`Optional`, `ReadOnly`, `InstanceQuery`, `Subject`) and has ad-hoc port allocation and
heavyweight setup that needs cleaning up before it's a good target for new tests.

**Decision:** Do not port scenario 08 into `tests/js` as part of this branch. The
`tests/js` suite needs its own refactor pass — update deprecated imports, clean up the
test structure, establish a clear pattern. That work belongs in a follow-up after this
branch is merged, not mixed in here.

**Target file (post-merge):** `tests/js/tests/model-decorator-api.test.ts`

**Why NOT in `ad4m/core` Jest:**
The `core` Jest suite mocks `PerspectiveProxy` — it can't catch SurrealDB query edge
cases, SHACL validation failures, batching race conditions, executor-side literal
encoding, or subscription lifecycle bugs. Keep `core` Jest for pure logic (query
building, SHACL generation, `Literal` encoding, `queryToSurrealQL` output shape). Keep
executor-touching behaviour in `tests/js`.

---

### `we` playground (`apps/playgrounds/react/ad4m-model-testing`) — active test harness

The playground was built as a fast iteration environment during Phase 2 development and
served its purpose well (scenario 08, 12/12 passing). Its future role is **not** as a
substitute for `tests/js` — it is a `we`-specific development and integration tool.

**Appropriate use cases going forward:**

- **`@we/models` tests** — `Space`, `Block`, block-types. These models are `we`-specific,
  use `we`-specific predicates, and are naturally tested alongside `we` code
- **`ad4m-hooks` testing** — `useSubjects`, `useSubject`, `useEntry`. These are React
  hooks; subscription re-render behaviour and React lifecycle integration can't be
  tested in Mocha
- **Neighbourhood + joining flows** — anything requiring the full `we` Electron shell
  (creating a neighbourhood, inviting an agent, observing sync across participants)
- **Visual/interactive debugging** — browser devtools, live state inspection, and hot
  reload when diagnosing hard-to-reproduce bugs
- **`we` release smoke tests** — quick sanity check before shipping that the full app
  stack works end-to-end

**Scenario renaming:** Now that the generic `Ad4mModel` scenarios move to `tests/js`,
the playground scenarios should be renamed to reflect their `we`-specific purpose:

| Old name                    | New purpose                                          |
| --------------------------- | ---------------------------------------------------- |
| `08-decorator-api.ts`       | Keep as reference / port to `tests/js`               |
| Future scenarios 01–07      | Write directly in `tests/js`, not here               |
| `08-we-models.ts` (new)     | `Space`/`Block` smoke test against new decorator API |
| `09-hooks.ts` (new)         | `useSubjects`, `useSubject`, `useEntry` hooks        |
| `10-neighbourhood.ts` (new) | Neighbourhood join + cross-agent sync                |

---

## Execution Order

```
0   [POST-MERGE] tests/js refactor + model-decorator-api.test.ts port  ⏳ PENDING
      ← fix deprecated imports in prolog-and-literals.test.ts and multi-user-simple.test.ts
      ← establish consistent port allocation and setup pattern
      ← port scenario 08 models + 14 tests into model-decorator-api.test.ts
      ← add subscription lifecycle tests (3d coverage at executor level)
      ← write remaining scenarios (01–07, 09–10) directly in tests/js
1a  generatePrologFacts.ts ✅                           ← committed on ad4m-model-refactor
1b  Remove dead Prolog paths ✅                         ← committed on ad4m-model-refactor
1c  Delete Subject.ts ✅                                ← committed on ad4m-model-refactor
2   Decorator cleanup (@Property, @Flag, @HasMany, etc.) ✅
    + WeakMap metadata registry ✅
    + update @we/models (5 files) ✅
3a  File decomposition ✅                               ← `eb2f4b4b` → `6dcc5283`
    + shared hydrateInstance() in hydration.ts ✅      ← eliminates dual hydration impls
    + parameterized SurrealQL queries throughout ⏳     ← still uses string interpolation
3b  Transaction API ✅                                  ← `a66d833b`
3c  Include / eager loading (IncludeMap) ✅             ← `6d02ad2d`
3d  Subscriptions ✅                                    ← client-side link listener approach
3e  Subscription infrastructure cleanup ⏳ ← NEXT       ← remove old server-push SurrealDB path
      ← delete subscribeSurrealDB + QuerySubscriptionProxy isSurrealDB branches (TS)
      ← delete SurrealSubscribedQuery, 5 Rust methods, 3 mutation resolvers
      ← write SUBSCRIPTION-ARCHITECTURE.md (why client-side, scaling notes)
Merge origin/dev (SHACL/Prolog PR #654) ✅             ← `9c6c57c0`
4   Model inheritance (WeakMap already done in Phase 2) ✅  ← verified by scenario 09
5   CRDT ordering ⏳                                    ← after 3e; fills in scenario 10
F   Flux decorator rename (~15 files in packages/api)   ← after test app validates Phase 2
G   External consumer migration ⏳                       ← gated on flux + hooks teams
      ← flux/packages/api/SubjectRepository → rewrite using Ad4mModel.findAll()/save()
      ← ad4m-hooks/helpers/SubjectRepository → rewrite using Ad4mModel + subscribe()
      ← delete PerspectiveProxy.getSubjectData() + getSubjectProxy()
      ← delete PerspectiveClient.getSubjectData()
      ← delete Rust get_subject_data() + GQL resolver
      ← prerequisite: Phase F (decorator rename) complete in flux
```
