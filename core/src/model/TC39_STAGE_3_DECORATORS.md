# TC39 Stage 3 Decorator Migration Plan

> **Status:** Planning  
> **Created:** 2026-02-28  
> **Estimated effort:** ~7–9 days

---

## Table of Contents

1. [Problem Statement](#problem-statement)
2. [Why TC39 Decorators](#why-tc39-decorators)
3. [Current Architecture (Legacy Decorators)](#current-architecture-legacy-decorators)
4. [Target Architecture (TC39 Decorators)](#target-architecture-tc39-decorators)
5. [Implementation Roadmap](#implementation-roadmap)
   - [Phase 1: Upgrade TypeScript](#phase-1-upgrade-typescript)
   - [Phase 2: Add `declare` to Flux Model Fields](#phase-2-add-declare-to-flux-model-fields)
   - [Phase 3: Rewrite Decorators to TC39 Spec](#phase-3-rewrite-decorators-to-tc39-spec)
   - [Phase 4: Update Ad4mModel Constructor](#phase-4-update-ad4mmodel-constructor)
   - [Phase 5: Fix Projection (Remove Hacks)](#phase-5-fix-projection-remove-hacks)
   - [Phase 6: Update All Consumer tsconfigs](#phase-6-update-all-consumer-tsconfigs)
   - [Phase 7: Integration Testing](#phase-7-integration-testing)
6. [Effort Estimate](#effort-estimate)
7. [What You Get](#what-you-get)
8. [Gotchas & Risks](#gotchas--risks)
9. [Appendix: Current State Audit](#appendix-current-state-audit)

---

## Problem Statement

Ad4m's model system needs to:

1. **Map class fields ↔ link-triple predicates** (`title` ↔ `"test://title"`)
2. **Generate SHACL shapes** (W3C standard) from the schema for `ensureSDNASubjectClass`
3. **Hydrate instances** from SurrealDB query results
4. **Persist via link mutations** (add/remove/set links)
5. **Provide instance methods** (`save()`, `addComments()`, `get()`, `delete()`)
6. **Support inheritance** (`TestDerivedModel extends TestBaseModel`)
7. **Work with Vue/React reactivity** (no `#private` fields)

The current system uses **legacy TypeScript experimental decorators** (`experimentalDecorators: true`). These decorators receive `target` = the **class prototype**, and every decorator writes to it:

```ts
// decorators.ts — current @Property implementation
Object.defineProperty(target, key, { configurable: true, writable: true });
```

This creates properties like `TestPost.prototype.body = undefined`, `TestPost.prototype.title = undefined`, etc. on the **prototype**, not the instance.

### Concrete bugs caused by this

- **`delete instance.body` doesn't fully work** — removes the own property, but `'body' in instance` still returns `true` because `TestPost.prototype.body` exists. This broke our `properties` field projection feature.
- **Setter stubs on the prototype** — `target[setTitle] = () => {}` puts empty functions on the prototype that could mask real setter wiring if the constructor fails.
- **`Object.keys(instance)` includes hydrated data but not prototype-shadow fields** — leading to asymmetric behaviour depending on whether a field was hydrated or not.

---

## Why TC39 Decorators

| Aspect                      | Legacy (`experimentalDecorators`)                    | TC39 Stage 3 (TS ≥ 5.0)                                         |
| --------------------------- | ---------------------------------------------------- | --------------------------------------------------------------- |
| **Spec status**             | Deprecated direction; based on an abandoned proposal | JavaScript standard (Stage 3, shipping in engines)              |
| **`target` parameter**      | The **prototype**                                    | N/A — field decorators receive `undefined` + a `context` object |
| **Per-instance init**       | Not available — must write to prototype              | `context.addInitializer()` runs code in the constructor         |
| **Metadata sharing**        | Manual WeakMaps keyed on constructor                 | `context.metadata` — TC39-standard shared object per class      |
| **`emitDecoratorMetadata`** | Supported (Reflect.getMetadata)                      | Not supported — use `context.metadata` instead                  |
| **Prototype pollution**     | Inherent to the pattern                              | Impossible — field decorators don't receive the prototype       |

The public API stays **identical** — `@Property({ through: "test://title" })` looks exactly the same to consumers.

---

## Current Architecture (Legacy Decorators)

### How `@Property` works today

```ts
// core/src/model/decorators.ts (current)
export function Property(opts: PropertyOptions) {
  return function <T>(target: T, key: keyof T) {
    // 1. Register metadata in WeakMap keyed on constructor
    const existing = propertyRegistry.get((target as any).constructor) ?? {};
    propertyRegistry.set((target as any).constructor, {
      ...existing,
      [key as string]: { ...existing[key as string], ...opts },
    });

    // 2. Place setter stub on PROTOTYPE (pollution)
    if (!opts.readOnly) {
      target[`set${capitalize(key as string)}`] = () => {};
    }

    // 3. Place field descriptor on PROTOTYPE (pollution)
    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}
```

**Steps 2 and 3 write to the prototype**, which is the root cause of all the issues.

### How `@HasMany` / `@HasOne` work today

Same pattern — register metadata, place `addX`/`removeX`/`setX` stubs on the prototype, call `Object.defineProperty` on the prototype.

### How `@Model` works today

Class decorator. Sets `target.prototype.className`, generates SHACL via `target.generateSHACL = function() { ... }`. This part is fine — class decorators always receive the constructor, not the prototype.

### tsconfig settings

| Project             | `experimentalDecorators` | `emitDecoratorMetadata` | `target` | `useDefineForClassFields`             | TypeScript version |
| ------------------- | ------------------------ | ----------------------- | -------- | ------------------------------------- | ------------------ |
| `ad4m/core`         | `true`                   | `true`                  | `ES2020` | not set (default: `false` for ES2020) | `^4.6.2`           |
| `flux/packages/api` | `true`                   | not set                 | `ES6`    | not set (default: `false` for ES6)    | inherited          |
| `we` playground     | `true`                   | `true`                  | `ES2022` | **`false`** (explicit)                | inherited          |

### Field declaration patterns

| Project        | Pattern                               | Example               | Count                       |
| -------------- | ------------------------------------- | --------------------- | --------------------------- |
| **Flux**       | Bare type annotation (no initializer) | `body: string;`       | ~60 fields across 22 models |
| **Flux**       | With initializer (arrays)             | `views: App[] = [];`  | ~20 fields                  |
| **We**         | Always has initializer                | `body: string = "";`  | ~30 fields across 10 models |
| **ad4m tests** | Always has initializer                | `title: string = "";` | ~15 fields across 6 models  |

---

## Target Architecture (TC39 Decorators)

### Core idea: field decorators only store metadata, class decorator collects and registers

```
┌─────────────────────────────────────────────────────────────┐
│  @Property / @HasMany / @Flag  (field decorators)           │
│  → write to context.metadata.__ad4m_properties / _relations │
│  → NO writes to prototype                                   │
│  → return initializer function (pass-through or flag value) │
└──────────────────────┬──────────────────────────────────────┘
                       │ context.metadata is shared
                       ▼
┌─────────────────────────────────────────────────────────────┐
│  @Model  (class decorator)                                  │
│  → reads context.metadata to get all field/relation info    │
│  → registers in propertyRegistry / relationRegistry WeakMap │
│  → attaches className, generateSHACL                        │
└─────────────────────────────────────────────────────────────┘
```

### `@Property` — TC39 version

```ts
function Property(opts: PropertyOptions) {
  return function (
    _value: undefined, // field decorators receive undefined
    context: ClassFieldDecoratorContext, // TC39 context object
  ) {
    const key = String(context.name);

    // Store metadata in the shared context.metadata object.
    // All field decorators on the same class share this object.
    const meta = context.metadata as any;
    meta.__ad4m_properties ??= {};
    meta.__ad4m_properties[key] = {
      ...(meta.__ad4m_properties[key] ?? {}),
      ...opts,
    };

    // Return an initializer function that runs when the field is assigned.
    // For most fields this is a pass-through — the class field initializer
    // (e.g. `body: string = ""`) provides the default value.
    return (initialValue: any) => initialValue;

    // NO Object.defineProperty on prototype
    // NO setter stubs on prototype
  };
}
```

### `@Flag` — TC39 version

```ts
function Flag(opts: FlagOptions) {
  return function (_value: undefined, context: ClassFieldDecoratorContext) {
    const key = String(context.name);

    const meta = context.metadata as any;
    meta.__ad4m_properties ??= {};
    meta.__ad4m_properties[key] = {
      through: opts.through,
      initial: opts.value,
      flag: true,
      readOnly: true,
      required: true,
    };

    // Return initializer that sets the flag value — regardless of
    // what the class field initializer says, flags always equal opts.value
    return () => opts.value;
  };
}
```

### `@HasMany` — TC39 version

```ts
function HasMany(
  relatedModelOrOpts: (() => any) | RelationOptions,
  opts?: RelationOptions,
) {
  const resolvedOpts =
    typeof relatedModelOrOpts === "function" ? opts! : relatedModelOrOpts;
  const relatedModel =
    typeof relatedModelOrOpts === "function" ? relatedModelOrOpts : undefined;

  return function (_value: undefined, context: ClassFieldDecoratorContext) {
    const key = String(context.name);

    const meta = context.metadata as any;
    meta.__ad4m_relations ??= {};
    meta.__ad4m_relations[key] = {
      ...resolvedOpts,
      direction: "forward" as const,
      ...(relatedModel ? { relatedModel } : {}),
    };

    // Pass through the initializer (typically `= []`)
    return (initialValue: any) => initialValue;

    // NO addX/removeX/setX stubs on prototype —
    // these are wired in the Ad4mModel constructor already
  };
}
```

### `@HasOne`, `@BelongsToOne`, `@BelongsToMany` — same pattern

Each stores its metadata in `context.metadata.__ad4m_relations` with the appropriate `direction` and `maxCount` values. No prototype writes.

### `@Model` — TC39 version

```ts
function Model(opts: ModelConfig) {
  return function <T extends new (...args: any[]) => any>(
    target: T,
    context: ClassDecoratorContext,
  ) {
    // At this point, ALL field decorators have already run and populated
    // context.metadata with __ad4m_properties and __ad4m_relations.
    const meta = context.metadata as any;
    const properties = meta.__ad4m_properties ?? {};
    const relations = meta.__ad4m_relations ?? {};

    // Register in WeakMaps (same as today, but populated cleanly)
    propertyRegistry.set(target, properties);
    relationRegistry.set(target, relations);

    // Attach className (same as today)
    target.prototype.className = opts.name;
    target.className = opts.name;

    // Attach generateSHACL (same logic as today — reads from registries)
    target.generateSHACL = function () {
      // ... identical SHACL generation code ...
    };

    return target;
  };
}
```

### Inheritance support

TC39 `context.metadata` uses prototype-based inheritance automatically:

```ts
@Model({ name: "Base" })
class Base extends Ad4mModel {
  @Property({ through: "base://content" })
  content: string = "";
}

@Model({ name: "Derived" })
class Derived extends Base {
  @Property({ through: "derived://extra" })
  extra: string = "";
}
// Derived[Symbol.metadata].__ad4m_properties inherits Base's via prototype chain
// getPropertiesMetadata(Derived) returns { content: {...}, extra: {...} }
```

The existing `getPropertiesMetadata()` / `getRelationsMetadata()` functions that walk the constructor prototype chain continue to work unchanged.

---

## Implementation Roadmap

### Phase 1: Upgrade TypeScript

**Files:** root `package.json`, `core/package.json`, all `tsconfig.json` files  
**Effort:** 1–2 days  
**Risk:** High (touches all projects)

1. Bump TypeScript to `^5.4` (or latest stable 5.x) in:
   - `ad4m/core/package.json` (currently `^4.6.2`)
   - Any other packages that pin their own TS version
2. In every `tsconfig.json`:
   - Remove `"experimentalDecorators": true`
   - Remove `"emitDecoratorMetadata": true`
   - Set `"target": "ES2022"` or higher
   - Set `"useDefineForClassFields": true` (explicit, even though it's the default for ES2022+)
3. Fix any type errors from the TS version bump (likely minimal)
4. Validate builds pass: `pnpm build` in ad4m root, `yarn build` in flux

> **Note:** Do this on a dedicated branch. The tsconfig changes alone will break decorators until Phase 3 is complete, so Phases 1–3 should land together.

### Phase 2: Add `declare` to Flux Model Fields

**Files:** All ~22 model files in `flux/packages/api/src/`  
**Effort:** 1 day  
**Risk:** Low (mechanical transformation)

With `useDefineForClassFields: true`, a bare field like:

```ts
@Property({ through: BODY })
body: string;
```

would emit `this.body = undefined` in the constructor, clobbering whatever the hydration pipeline sets. The fix:

```ts
@Property({ through: BODY })
declare body: string;
```

`declare` tells TypeScript "this field exists for type-checking but don't emit any runtime code." The decorator and hydration pipeline handle the actual value.

**Rules:**

| Field pattern                                 | Action needed                                                             |
| --------------------------------------------- | ------------------------------------------------------------------------- |
| `body: string;` (no initializer)              | Add `declare` → `declare body: string;`                                   |
| `body: string = "";` (with initializer)       | No change — initializer runs in constructor, creates own property         |
| `views: App[] = [];` (array with initializer) | No change                                                                 |
| `@Flag type: string;`                         | Add `declare` → `declare type: string;` (Flag initializer sets the value) |

**We models** already use initializers on all fields, so they need **no changes** for this phase.

**Ad4m test models** already use initializers on all fields, so they need **no changes** for this phase.

### Phase 3: Rewrite Decorators to TC39 Spec

**Files:** `core/src/model/decorators.ts`  
**Effort:** 2–3 days  
**Risk:** Medium (core logic unchanged, just new signatures)

This is the core of the migration. Each decorator changes its function signature but keeps the same metadata-registration logic. See the [Target Architecture](#target-architecture-tc39-decorators) section above for the full new implementations.

**Summary of changes per decorator:**

| Decorator        | What changes                                                                                                                                               | What stays the same                              |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------ |
| `@Property`      | Signature → `(undefined, ClassFieldDecoratorContext)`. No `Object.defineProperty`. No setter stubs. Stores in `context.metadata`.                          | PropertyOptions interface. Metadata shape.       |
| `@Flag`          | Same as Property. Returns `() => opts.value` as initializer.                                                                                               | FlagOptions interface.                           |
| `@HasMany`       | Signature → `(undefined, ClassFieldDecoratorContext)`. No `Object.defineProperty`. No add/remove/set stubs. Stores in `context.metadata.__ad4m_relations`. | RelationOptions interface. relatedModel factory. |
| `@HasOne`        | Same as HasMany but adds `maxCount: 1`.                                                                                                                    |                                                  |
| `@BelongsToOne`  | Same pattern, `direction: "reverse"`, `maxCount: 1`.                                                                                                       |                                                  |
| `@BelongsToMany` | Same pattern, `direction: "reverse"`.                                                                                                                      |                                                  |
| `@Model`         | Signature → `(target, ClassDecoratorContext)`. Reads `context.metadata` instead of WeakMaps directly. Registers in WeakMaps. SHACL generation unchanged.   | ModelConfig interface. All SHACL logic.          |

**What's deleted:**

- Every `Object.defineProperty(target, key, { configurable: true, writable: true })` line
- Every `target[setterName] = () => {}` line (setter stubs on prototype)
- Every `target[adderName] = () => {}` / `target[removerName] = () => {}` line

**`Symbol.metadata` polyfill** — TC39 `context.metadata` requires `Symbol.metadata` to exist. TypeScript 5.2+ emits the polyfill automatically. For older environments, add at the top of `decorators.ts`:

```ts
Symbol.metadata ??= Symbol("Symbol.metadata");
```

### Phase 4: Update Ad4mModel Constructor

**File:** `core/src/model/Ad4mModel.ts`  
**Effort:** 0.5 day  
**Risk:** Low

The constructor already wires `addX`/`removeX`/`setX` methods from the relation registry:

```ts
constructor(perspective: PerspectiveProxy, id?: string) {
  this._id = id ? id : Literal.from(makeRandomId(24)).toUrl();
  this._perspective = perspective;

  // Wire up real relation mutator methods (already exists, no change needed)
  const relations = getRelationsMetadata(proto.constructor);
  for (const key of Object.keys(relations)) {
    if (relations[key].direction === "reverse") continue;
    const cap = capitalize(key);
    this[`add${cap}`] = (value, batchId?) => mutation.setRelationAdder(...);
    this[`remove${cap}`] = (value, batchId?) => mutation.setRelationRemover(...);
    this[`set${cap}`] = (value, batchId?) => mutation.setRelationSetter(...);
  }
}
```

This **doesn't need to change** — it reads from `relationRegistry` which is populated by `@Model` in Phase 3. The setter stubs that were previously on the prototype are no longer needed because the constructor was already overwriting them with real implementations.

**Optional enhancement:** add a `getModelMetadata()` path that can read from `Symbol.metadata` on the constructor as a fallback:

```ts
static getModelMetadata(): ModelMetadata {
  // Primary: WeakMap (populated by @Model)
  const fromRegistry = _getModelMetadata(this);
  if (fromRegistry) return fromRegistry;

  // Fallback: Symbol.metadata (TC39 standard)
  const meta = (this as any)[Symbol.metadata];
  if (meta) {
    return {
      className: this.prototype.className,
      properties: meta.__ad4m_properties ?? {},
      relations: meta.__ad4m_relations ?? {},
    };
  }

  throw new Error("Model metadata not found — is @Model applied?");
}
```

### Phase 5: Fix Projection (Remove Hacks)

**Files:**

- `core/src/model/query/operations.ts`
- `tests/js/tests/model/model-query.test.ts`

**Effort:** 0.5 day  
**Risk:** Low

With no prototype-shadow properties, `delete instance.body` will make `'body' in instance` return `false` correctly.

**Revert test assertions** from:

```ts
expect(r).to.not.have.own.property("body"); // current hack
```

back to:

```ts
expect(r).to.not.have.property("body"); // clean assertion
```

The projection code in `operations.ts` (`delete instance[key]`) works correctly with zero workarounds.

### Phase 6: Update All Consumer tsconfigs

**Files:** Every `tsconfig.json` across `ad4m`, `flux`, `we`  
**Effort:** 0.5 day  
**Risk:** Low

Ensure all projects have:

```jsonc
{
  "compilerOptions": {
    // Remove these:
    // "experimentalDecorators": true,
    // "emitDecoratorMetadata": true,

    // Add/ensure these:
    "target": "ES2022",
    "useDefineForClassFields": true,
  },
}
```

**Affected tsconfig counts:**

- Flux: ~20 tsconfig files with `experimentalDecorators: true`
- We: ~4 tsconfig files
- Ad4m: core + test-runner configs

### Phase 7: Integration Testing

**Effort:** 1–2 days  
**Risk:** Medium

1. **Ad4m model tests:** `cd tests/js && pnpm test-model` — all query, transaction, and dirty-tracking tests
2. **SHACL validation:** Compare generated Turtle output before/after migration — must be byte-identical
3. **Flux build + dev:** `cd flux && yarn build && yarn dev` — ensure all 22 models hydrate correctly
4. **We playground:** Build and run the ad4m-model-testing playground
5. **Flux channel pinning:** The original bug that triggered this whole investigation — verify pinning a channel no longer duplicates conversations

---

## Effort Estimate

| Phase                            | Effort        | Risk                                          |
| -------------------------------- | ------------- | --------------------------------------------- |
| 1. Upgrade TypeScript            | 1–2 days      | High — touches all projects                   |
| 2. Add `declare` to Flux models  | 1 day         | Low — mechanical                              |
| 3. Rewrite decorators            | 2–3 days      | Medium — core logic unchanged, new signatures |
| 4. Update Ad4mModel constructor  | 0.5 day       | Low                                           |
| 5. Fix projection (remove hacks) | 0.5 day       | Low                                           |
| 6. Update tsconfigs              | 0.5 day       | Low                                           |
| 7. Integration testing           | 1–2 days      | Medium                                        |
| **Total**                        | **~7–9 days** |                                               |

> **Phases 1–3 must land together** on a single branch. The tsconfig changes break legacy decorators, and the new decorators require the tsconfig changes.

---

## What You Get

- **Zero prototype pollution** — decorators never write to the prototype
- **`delete instance.field` works correctly** — no phantom properties from prototype chain
- **Future-proof** — using the JavaScript standard, not a deprecated TS experiment
- **Cleaner separation of concerns** — field decorators only store metadata, class decorator collects and registers
- **`context.metadata`** — the TC39-standard way to share data between decorators, replacing manual WeakMap coordination
- **Same public API** — `@Property({ through: "..." })`, `@HasMany(...)`, etc. look identical to consumers
- **Same SHACL output** — the generated shapes are unchanged
- **Dirty tracking just works** — no prototype shadows to confuse snapshot comparisons
- **Projection just works** — `delete` fully removes properties, no `.own.property` hacks needed

---

## Gotchas & Risks

### 1. `emitDecoratorMetadata` is not supported by TC39 decorators

If anything relies on `Reflect.getMetadata()` at runtime, it needs to be replaced with `context.metadata`. The Ad4m codebase **does not appear to use** `Reflect.getMetadata()`, so this should be fine. Verify with:

```bash
grep -r "Reflect.getMetadata\|Reflect.defineMetadata\|reflect-metadata" --include="*.ts" core/src/
```

### 2. Decorator execution order changes slightly

In legacy decorators, field decorators run when the class is defined (at class parse time). In TC39, field decorator **initializers** run in the constructor. However, the metadata-via-`context.metadata` approach avoids timing issues because:

- Field decorators populate `context.metadata` at class definition time (the decorator function itself runs at definition time — only the _initializer_ it returns runs per-instance)
- The `@Model` class decorator runs at class definition time and reads the already-populated `context.metadata`

### 3. `Symbol.metadata` polyfill

TC39 `context.metadata` requires `Symbol.metadata` to exist. TypeScript 5.2+ emits the polyfill automatically. For older environments or bundlers that strip it, add at the top of `decorators.ts`:

```ts
Symbol.metadata ??= Symbol("Symbol.metadata");
```

### 4. Flux models without initializers need `declare`

With `useDefineForClassFields: true`, a bare `body: string;` emits `this.body = undefined` in the constructor. This would clobber hydrated values. The fix is `declare body: string;`.

**Enforcement:** Add an ESLint rule or a custom lint check that flags model fields (decorated with `@Property`, `@HasMany`, etc.) that lack either `declare` or an initializer.

### 5. Third-party decorator libraries

Check if any dependencies use `experimentalDecorators` in their published types. Most libraries ship plain `.d.ts` files that don't depend on decorator mode, but verify.

### 6. Vue reactivity

Vue 3's reactivity system uses Proxy-based tracking. `declare` fields that are later set by hydration will still be reactive — Vue tracks property access/mutation on the proxy, regardless of whether the property was declared with `declare` or an initializer. The `#private` concern (which breaks Vue proxies) is unrelated and already avoided in Ad4mModel.

---

## Appendix: Current State Audit

### Model count across projects

| Project    | Model classes | Fields without initializer                   | Uses `declare` |
| ---------- | ------------- | -------------------------------------------- | -------------- |
| Flux       | 22            | ~60 fields (all `@Property`/`@Flag` scalars) | No             |
| We         | 10            | 0 (all have initializers)                    | No             |
| Ad4m tests | 6             | 0 (all have initializers)                    | No             |

### Prototype properties created by current decorators

For a model like `TestPost` with `@Property title`, `@Property body`, `@Property viewCount`, `@HasMany tags`, `@HasMany comments`, `@HasOne pinnedComment`:

```
TestPost.prototype own keys:
  constructor, type, setTitle, title, setBody, body,
  setViewCount, viewCount, addTags, removeTags, setTags, tags,
  addComments, removeComments, setComments, comments,
  addPinnedComment, removePinnedComment, setPinnedComment, pinnedComment, className
```

Every `@Property` creates 2 prototype entries (field + setter stub).  
Every `@HasMany`/`@HasOne` creates 4 prototype entries (field + add + remove + set stubs).

**After TC39 migration:** `TestPost.prototype` will only have `constructor` and `className`. Everything else is either an own property (from class field initializers) or wired in the `Ad4mModel` constructor (mutator methods).

### Files to modify

| File                                            | Phase | Change                                                                                                             |
| ----------------------------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------ |
| `ad4m/core/package.json`                        | 1     | Bump `typescript` to `^5.4`                                                                                        |
| All `tsconfig.json` (~24 files)                 | 1, 6  | Remove `experimentalDecorators`/`emitDecoratorMetadata`, set `target: ES2022`, add `useDefineForClassFields: true` |
| `flux/packages/api/src/**/*.ts` (~22 files)     | 2     | Add `declare` to ~60 bare-typed fields                                                                             |
| `ad4m/core/src/model/decorators.ts`             | 3     | Full decorator rewrite                                                                                             |
| `ad4m/core/src/model/Ad4mModel.ts`              | 4     | Optional: add `Symbol.metadata` fallback in `getModelMetadata()`                                                   |
| `ad4m/core/src/model/query/operations.ts`       | 5     | No code change needed — `delete` just works now                                                                    |
| `ad4m/tests/js/tests/model/model-query.test.ts` | 5     | Revert `.own.property` → `.have.property`                                                                          |
