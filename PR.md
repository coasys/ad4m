# Ad4mModel Refactor — SHACL-native ORM, full test suite refactored

## Summary

This PR completes the full `Ad4mModel` refactor started in the `feat/shacl-sdna-migration` base. The legacy Prolog-based subject class system is removed from the TypeScript ORM layer. `Ad4mModel` is now a clean, SHACL-native ORM backed entirely by SurrealDB, with a Prisma-inspired query/decorator API, atomic transactions, reactive subscriptions, and eager-loading via `IncludeMap`. The monolithic 3,917-line `Ad4mModel.ts` has been decomposed into focused modules and is now 759 lines. The full ad4m test suite has been refactored.

---

## What changed

### 🏗️ Phase 1 — Prolog removal

- **Delete `Subject.ts`** — inlined the necessary bits into `PerspectiveProxy` as a local class (Phase 1c)
- **Strip all Prolog query paths** from `Ad4mModel.ts` and `decorators.ts` (Phase 1b):
  - Deleted `queryToProlog()`, `instancesFromPrologResult()`, `countQueryToProlog()`, and all `build*Query` helpers that only served them
  - Deleted `@InstanceQuery` decorator and `InstanceQueryParams` interface
  - Deleted `generateSDNA()` from `@ModelOptions`
  - Removed `prologGetter` / `prologSetter` from `PropertyOptions`, `PropertyMetadata`, `Optional()`, `Property()`, `ReadOnly()` interfaces
  - Removed `useSurrealDB` flag and `useSurrealDB()` method from `ModelQueryBuilder`
  - Renamed `makeRandomPrologAtom` → `makeRandomId`

---

### 🎨 Phase 2 — New decorator & relation API

- Introduced **`@Model`, `@Field`, `@HasMany`, `@HasOne`, `@BelongsToOne`, `@BelongsToMany`** decorators
- **WeakMap metadata registry** keyed on constructor (not prototype) — eliminates a silent inheritance/data-corruption bug for subclassed models
- **`@BelongsToOne` / `@BelongsToMany`** decorators with `direction='reverse'`/`maxCount`; `HasMany`/`HasOne` accept an optional model factory argument
- **Renamed `setCollection*` → `set*`** throughout the public API; `PerspectiveProxy.isCollectionSetter()` now uses metadata instead of a string prefix check
- **Renamed `collection*` → `relation*`** everywhere in the stack, including Rust: `CollectionSetter` enum variant → `RelationSetter` (with `#[serde(rename)]`)
- **`@HasOne` hydration fix** — `getData()` and `instancesFromSurrealResult()` now apply a `maxCount === 1` guard so `@HasOne` fields resolve to a scalar string, not an array
- **`@Flag` SHACL wiring** — `generatePropertySetterAction()` throws if `metadata.flag` is set; `innerUpdate()` skips flag fields; flags are immutable after creation
- Updated `@we/models` and the test app to the new decorator names

---

### 🔬 Phase 3 — Decompose `Ad4mModel.ts` into focused modules

`Ad4mModel.ts` went from **3,917 → 759 lines** across the following extractions:

| File                                                          | Contents                                                                                                       |
| ------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| `model/types.ts`                                              | `WhereCondition`, `Query`, `IncludeMap`, `ModelMetadata`, subscription types                                   |
| `model/schema/metadata.ts`                                    | `getModelMetadata()`, `assignValuesToInstance()`, `ModelValueTuple`                                            |
| `model/schema/fromJSONSchema.ts`                              | `createModelFromJSONSchema`, `determinePredicate`, `determineNamespace`                                        |
| `model/query/SurrealQueryBuilder.ts` (→ `surrealCompiler.ts`) | Pure SurrealQL builder helpers                                                                                 |
| `model/query/operations.ts`                                   | `queryToSurrealQL`, `instancesFromSurrealResult`, `findAll`, `findOne`, `findAllAndCount`, `paginate`, `count` |
| `model/query/hydration.ts`                                    | `hydrateInstanceFromLinks()`, `evaluateCustomGetters()` — shared between single-instance and bulk paths        |
| `model/query/fetchInstance.ts`                                | 6-stage single-instance hydration pipeline (`getData()` body)                                                  |
| `model/query/QueryBuilder.ts` (→ `ModelQueryBuilder`)         | Fluent query builder class                                                                                     |
| `model/mutation.ts`                                           | `MutationContext`, `setProperty`, `setRelation*`, `saveInstance`, `innerUpdate`, `cleanCopy`                   |
| `model/transaction.ts`                                        | `runTransaction`, `TransactionContext`                                                                         |
| `model/subscription.ts`                                       | `createSubscription()`, shared registry, fingerprinting                                                        |

**Unified hydration** — `getData()` (single-instance) and `instancesFromSurrealResult()` (bulk) previously had divergent hydration implementations (one using latest-wins, the other first-wins); both now share `hydrateInstanceFromLinks()` from `hydration.ts` with consistent latest-wins ordering.

**Transaction API (Phase 3b)**

```ts
// Before (fragile — leaked batch if any save threw):
const batchId = await perspective.createBatch();
await a.save(batchId);
await b.save(batchId);
await perspective.commitBatch(batchId);

// After:
await Ad4mModel.transaction(perspective, async (tx) => {
  await a.save(tx.batchId);
  await b.save(tx.batchId);
});
```

**Include API / IncludeMap (Phase 3c → 3f)**

```ts
// Prisma-style eager loading — relations stay as bare IDs unless include is set
Recipe.findAll(perspective, {
  include: {
    comments: true,
    tags: { where: { active: true }, limit: 5 },
  },
});
```

Removed the old `relations` field and `.relations()` fluent method.

**Link-listener subscription API (Phase 3d)**

- New `createSubscription<T>()` in `subscription.ts` — fires immediately then on every relevant `link-added`/`link-removed`
- Relevance check: only re-queries when a changed link's predicate belongs to the model (properties + relations); optional source filter
- **Shared registry**: identical `(model, query)` pairs on the same perspective share one `findAll()` execution and one set of link listeners via a `WeakMap`-keyed registry
- **Result fingerprinting** (`stableFingerprint`): callbacks only fire when the result set actually changed
- **Late subscriber fast-path**: second subscriber receives cached `lastResults` immediately via microtask
- **Last-subscriber teardown**: shared entry and listeners are fully cleaned up when listener count reaches zero
- 50 ms coalesce window (`SETTLE_MS`) to batch rapid link events from a single `save()` into one re-query
- `.live(callback)` fluent terminal on `ModelQueryBuilder` alongside `.get()`

**Server-push SurrealDB subscription removal (Phase 3e)**

Deleted the legacy server-side SurrealDB subscription system that was made redundant by Phase 3d's client-side subscription registry:

_TypeScript (client):_

- Removed `isSurrealDB` field from `QuerySubscriptionProxy` and its `if/else` surreal branches in `subscribe()`, keepalive loop, and `dispose()`
- Deleted `subscribeSurrealDB()` from `PerspectiveProxy`
- Deleted `perspectiveSubscribeSurrealQuery()`, `perspectiveKeepAliveSurrealQuery()`, `perspectiveDisposeSurrealQuerySubscription()` from `PerspectiveClient`

_Rust (server):_

- Deleted `SurrealSubscribedQuery` struct
- Dropped `trigger_surreal_subscription_check` and `surreal_subscribed_queries` fields + `Arc::new()` initialisers from `PerspectiveInstance`
- Removed `surreal_subscription_cleanup_loop()` from `start_background_tasks()`
- Deleted 5 functions: `subscribe_and_query_surreal`, `keepalive_surreal_query`, `dispose_surreal_query_subscription`, `surreal_subscription_cleanup_loop`, `check_surreal_subscribed_queries`
- Deleted 3 GraphQL mutation resolvers: `perspective_subscribe_surreal_query`, `perspective_keep_alive_surreal_query`, `perspective_dispose_surreal_query_subscription`
- Removed two `trigger_surreal_subscription_check` trigger lines from the link-added/link-removed update paths

Net: –876 lines, +769 lines across 4 files (the `PerspectiveClient` additions are JSDoc and test-fixture refactoring).

---

### 🚀 Phase 4 — Advanced features

- **Model inheritance via SHACL `sh:node`** — `SHACLShape` gains `parentShapes`, `addParentShape()`, and emits `sh:node` in both `toTurtle()` and `toLinks()` serializers; child shape uses only own properties/relations and references the parent via `sh:node` instead of duplicating; runtime inheritance already worked via the WeakMap prototype-chain walk in `getPropertiesMetadata`/`getRelationsMetadata`
- **`Ad4mModel.create<T>(perspective, data)`** — static factory that constructs, assigns, and saves in one call
- **`Ad4mModel.register(perspective)`** — thin wrapper around `ensureSDNASubjectClass()` for a consistent static API
- **Remove `has_child` and source-scoping** — removed source param from constructor, `has_child` link write on create path, `Query.source`, `ModelQueryBuilder.source()`, and the source-scoped relevance check in subscription; legacy perspectives had meaningless `source → ad4m://has_child → baseExpression` links
- **`baseExpression` → `id`** throughout the `Ad4mModel` layer (breaking): `get baseExpression()` public getter removed, private field renamed, `MutationContext.baseExpression` → `id`; `PerspectiveProxy` and the Rust executor retain `baseExpression` as the correct protocol-level vocabulary
- **Remove `isInstance`, `prologCondition`, `where.condition`** from the query API
- **Drop `run()` alias** on `ModelQueryBuilder`
- **Fix `stableFingerprint`** — was always `undefined` because it referenced the deleted `baseExpression` field; now uses `id`
- **Fix multi-field sort** dropping all but the first key in `operations.ts`
- **Fix `count()` N+1** — added `hasJsFilterConditions()` fast path; `count()` was hydrating all rows just to count them

---

### 🔄 SHACL / SurrealDB migration

- **Migrated all flow methods to SHACL** — rewrote `sdnaFlows`, `availableFlows`, `startFlow`, `expressionsInFlowState`, `flowState`, `flowActions`, `runFlowAction` in `PerspectiveProxy.ts` to use `SHACLFlow`/`getFlow()` instead of Prolog `infer()` calls
- **Removed `parse_prolog_sdna_to_shacl_links`** (328-line Rust function) and its backward-compat Prolog→SHACL code generation from `add_sdna`; SHACL→Prolog direction (`shacl_to_prolog.rs`) is kept for compatibility
- **Extracted `shacl_to_prolog.rs`** — SHACL→Prolog compat code moved into its own module with comprehensive unit tests
- **Removed `subjectClassesFromSHACL` GraphQL endpoint** — replaced with client-side SHACL link queries via `findClassByProperties()`
- **Replaced `buildQueryFromTemplate()` / Prolog-based `SubjectClassOption`** with client-side SHACL matching via `findClassByProperties()`; then replaced by a single SurrealDB query (two-pass client-side processing) to avoid N+1 round trips; `subjectClassesByTemplate` falls back to `findClassByProperties()` when class name lookup fails
- **Used `SHACLShape.toJSON()`** in `ensureSDNASubjectClass` instead of manual JSON; `getSubjectClassMetadataFromSDNA` now uses `getShacl()`/`SHACLShape.fromLinks()`
- **Batched SHACL/Flow link writes** — `addShacl()`/`addFlow()` use `addLinks()` batch API; `getShacl()`/`getFlow()` use a single `querySurrealDB()` call instead of individual `get()` calls
- **Converted `ends_with` filters** to SurrealDB `string::ends_with` queries (5 functions in `perspective_instance.rs` that previously fetched all and filtered in memory)
- Fixed SurrealQL function name: `string::starts_with` not `starts::with`; replaced `SQL LIKE` with `string::starts_with` in `getFlow()`
- Fixed `sdnaCode` nullable in GraphQL schema — was `String!`, must be `String` since Prolog is now optional
- **Executor commit ordering fix** — `update_prolog_engines()` (which spawns the pubsub `link-added` task) previously ran _before_ `persist_link_diff()` (the SurrealDB write); any subscriber calling `findAll()` immediately on `link-added` would read stale data; order swapped so SurrealDB is committed before pubsub fires

---

### 🧪 Test suite refactor

**Infrastructure:**

- Added `helpers/` directory: `ports.ts` (dynamic port allocation), `executor.ts` (`AgentHandle`), `assertions.ts` (`waitUntil`), `index.ts` (barrel)
- Added `wipePerspective()` export to `utils/utils.ts`
- Centralized `global.fetch` polyfill into `tests/setup.ts`
- Migrated auth and multi-user tests to a shared `startAgent` helper
- Removed `findAndKillProcess` entirely — matched any process by name and could kill a live AD4M instance; teardown already uses `tree-kill` with the specific child PID

**New model test suite** (`tests/model/`):

- `models.ts` — `TestPost`, `TestComment`, `TestTag`, `TestBaseModel`, `TestDerivedModel` fixture models
- `model-core.test.ts` — 20 CRUD + decorator coverage tests
- `model-query.test.ts` — `where`/`order`/`limit`/`offset`/`count`/`paginate`/`findAllAndCount`/`include` sub-query tests
- `model-subscriptions.test.ts` — live subscription tests using `.live()` API
- `model-transactions.test.ts` — batch/transaction pattern tests including rollback (7 tests)
- `model-inheritance.test.ts` — metadata isolation, SHACL, polymorphic `findAll` tests
- `model-where-operators.test.ts` — 9 tests covering all `WhereCondition` operators (`IN`, `not`, `contains`, `gt`, `gte`, `lt`, `lte`, `between`)
- `model-prolog.test.ts` — 5 pure-function tests for `generatePrologFacts()` + 2 executor `infer()` tests

**Key fixes found during test rebuild:**

- `register()` calls added to `beforeEach` in all model test files — SHACL definitions are stored as links and therefore don't survive `wipePerspective()`
- `resolveLanguage: 'literal'` on properties so SurrealQL `WHERE` uses `fn::parse_literal(out.uri) = 'value'` (fixes `where:{title:...}` and `count()` with `where`)
- Transaction tests rewritten to avoid `create+delete` within the same batch (the runtime doesn't handle constructor+destructor for the same entity in one committed batch)
- Fixed same-batch double-save bug: `#savedOnce` flag on instances passes an `alreadyExists` hint to `saveInstance()` to skip the SurrealDB existence-check on subsequent saves in the same uncommitted batch
- Fixed `@BelongsToMany` `include` hydration

**Reorganisation:**

- Split monolithic `multi-user-simple.test.ts` into **8 focused test files**
- Reorganised tests into subfolders (`auth/`, `model/`, `sdna/`, `multi-user/`)
- Renamed `prolog-and-literals.test.ts` → `sdna.test.ts`; removed duplicate model tests from it
- Script renames across the monorepo: `test-main` → `test`, `test-all` → `test-run`, `test-run` → `test:ci`; removed legacy aliases; combined auth scripts; folded `test-from-json-schema` into `test-model`

**Test runner fixes:**

- Replaced stale `findAndKillProcess('holochain')` / `findAndKillProcess('lair-keystore')` with `findAndKillProcess('ad4m')` — everything is a single `ad4m-executor` process since the Rust refactor
- Switched from downloading stale pre-built language bundles (CJS, required ESM conversion) to the bootstrap seed (`tests/js/bootstrapSeed.json`); the language-language fetches other system languages by hash from the Cloudflare bootstrap store at runtime; removed `node-wget-js` and `unzipper` dependencies

---

### ⚙️ CI — migrated to self-hosted runner

- **Replaced Docker executor** (`coasys/ad4m-ci-linux`) **with self-hosted machine runner** `coasys/marvin` (AMD Ryzen 9 9950X, 60 GB RAM, Ubuntu 25.04; Rust 1.92, Node 18, Deno, Go, pnpm pre-installed)
- **Removed remote cache** — `restore/save_cache` was uploading/downloading the full Rust `target/` dir (~20 GB) causing 29-minute cache restore steps on every run; the persistent machine runner does incremental compilation instead (`cleanup_working_directory: false`)
- **Fixed nvm stdout corruption** in cargo build scripts — `nvm use 18` in `$BASH_ENV` printed `Now using node v18.20.8` to stdout which corrupted the `libffi` link step via a bash syntax error in a subshell; fixed by adding the Node 18 bin dir directly to `PATH`

---

### 🐛 Other bug fixes

- Fixed neighbourhood signal routing: `send_signal()` and `send_broadcast()` in `perspective_instance.rs` only searched `list_user_emails()` to decide if a recipient is local; the main agent has no email so signals to it fell through to the link language (which doesn't loop back), causing silent delivery failure when a managed user and the main agent co-owned a neighbourhood; fixed to also check the main agent's DID
- Fixed broadcast loopback — broadcasts skip local delivery when `owners` is `None` or empty; `owners=None/[]` treated as implicit main-agent ownership for legacy perspectives
- Fixed `innerUpdate` emitting `Property X has no metadata, skipping` noise for generated relation methods and un-decorated fields; `setProperty` now throws for truly unknown direct calls
- Fixed `save()` create/update routing: checks SurrealDB for existing links before branching; `setProperty()` now encodes raw values as `literal://` URIs before passing to `executeAction`, mirroring Rust's `resolve_property_value`
- Fixed `queryToSurrealQL` SELECT — was `->link AS links` (returns target node records, `id`/`uri` only); fixed with a correlated subquery returning `predicate`, `out.uri as target`, `author`, `timestamp`; also removed phantom `$perspective` filter (no such field on the link table — it was a silent no-op)
- Fixed quote escape no-op in `fetchInstance.ts` (`.replace(/'/g, "'")` replaced with same character)
- Fixed `infer()` call in `perspective_instance.rs` not awaiting `findAndKillProcess` in error handlers
- Multi-user test suite fixes: auth flow, error strings, URI validation, bootstrap timing
- **Literal-guard fix (from `origin/dev` merge)** — dev commits `70d1d508` / `d7a2e708` added a `propMeta.resolveLanguage === 'literal'` guard before `literal://` URI parsing in the legacy monolithic `Ad4mModel.ts` to prevent crashing on non-literal string values; our decomposed `hydration.ts` (`resolveValue()`) already incorporates an equivalent and strictly-superior guard — `(resolveLanguage === "literal" || resolveLanguage === undefined)` — covering both explicit literal properties and plain undecorated string fields

---

### 📄 Documentation

- Added `AD4M-MODEL-REFACTOR.md` refactor plan (continuously updated through 2026-02-24)
- Added `core/src/model/README.md` — architecture overview, full Recipe example, decorator reference table, query API cheatsheet, transaction pattern, `fromJSONSchema` examples, inheritance notes; corrected two method name errors after initial authoring (`.run()` → `.get()`, `.subscribe()` → `.live()` on the fluent builder)
- Added Phase G deprecation plan — documents what can be removed from `PerspectiveProxy` / Rust once Flux and `ad4m-hooks` migrate off the `Subject` proxy API to `Ad4mModel` (`getSubjectData`, `getSubjectProxy`, Rust `get_subject_data()`)
- Added `SUBSCRIPTION_STRATEGY.md` — documents client-side subscription architecture, multi-user node compatibility, shared registry design
- Trimmed JSDoc verbosity by 24–37% across `Ad4mModel.ts`, `decorators.ts`, `transaction.ts`, `types.ts`
- Updated CHANGELOG with 21 unreleased entries since 0.11.1 (Fixed ×11, Added ×5, Changed ×5)
