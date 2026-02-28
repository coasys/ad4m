# Decouple SHACL from the Mutation Hot Path

> **Status:** Planning  
> **Created:** 2026-02-28  
> **Priority:** High — removes a class of bugs and simplifies the mutation path

---

## Table of Contents

1. [Problem](#problem)
2. [Current Mutation Flow](#current-mutation-flow)
3. [What's Wrong With This](#whats-wrong-with-this)
4. [Proposed Architecture](#proposed-architecture)
5. [What SHACL Should Still Be Used For](#what-shacl-should-still-be-used-for)
6. [Implementation Plan](#implementation-plan)
7. [Risks and Considerations](#risks-and-considerations)

---

## Problem

SHACL shapes are currently used as an **instruction set** for mutations. The Rust
executor reads constructor/setter/adder/remover actions from the SHACL shape and
interprets them to create and update links. This means the ORM's decorator
metadata is serialised into SHACL, transmitted to Rust, parsed back into action
lists, and then executed as link operations — a round-trip through a
serialisation format when the ORM already has all the information it needs to
create links directly.

---

## Current Mutation Flow

### Create path (first `save()`)

```
ORM:  instance.save()                  [TypeScript]
  → perspective.createSubject(instance, id)
    → Rust executor reads SHACL shape
      → interprets ad4m://constructorActions
        → executes addLink() for each constructor action
      → reads initialValues from instance
        → calls setter actions to overwrite constructor placeholders
    → returns to TypeScript
```

The create path goes through:

1. TypeScript decorator metadata → SHACL Turtle serialisation
2. Turtle → Rust SHACL parser
3. Parser → constructor action list
4. Action list → link operations
5. Then separately: initial values → setter action list → more link operations

### Update path (`save()` on existing instance)

```
ORM:  instance.save()                  [TypeScript]
  → mutation.innerUpdate()
    → for each dirty property: setProperty() → addLink() directly
    → for each dirty relation: setRelationSetter() → addLink()/removeLink() directly
```

The update path **already skips SHACL entirely**. It reads decorator metadata
(predicate URIs, relation types) from the registry and creates/removes links
directly via the perspective API. This is simpler, faster, and easier to debug.

### The asymmetry

|                         | Create                             | Update                     |
| ----------------------- | ---------------------------------- | -------------------------- |
| **Reads metadata from** | SHACL shape (via Rust)             | Decorator registry (JS)    |
| **Mutation logic in**   | Rust SHACL interpreter             | TypeScript `mutation.ts`   |
| **Debuggable in**       | Rust (requires executor source)    | TypeScript (same codebase) |
| **Latency**             | SHACL parse + interpret + link ops | Direct link ops            |

The update path is the correct architecture. The create path should work the same way.

---

## What's Wrong With This

### 1. SHACL action generation and ORM mutation logic can disagree

The SHACL shape is generated from decorator metadata by `generateSHACL()` in
`decorators.ts`. The mutation logic in `mutation.ts` also reads from decorator
metadata. These are **two independent code paths** that must stay in sync:

- If a decorator option is added (e.g. `local: true`), it must be handled in
  both `generateSHACL()` (for the constructor action) and `mutation.ts` (for
  updates). If one is missed, creates and updates behave differently.

- The `createSubject` call in Rust has its own logic for "initial values" that
  overwrite constructor placeholders. This mechanism is invisible from
  TypeScript — if it fails silently, the ORM has no way to detect or recover.

### 2. The Rust round-trip adds latency

Every first `save()` goes: JS → Rust SHACL parser → action interpreter → link
API → SurrealDB reindex → back to JS. The update path skips the SHACL
parser/interpreter entirely. Moving creates to the same path removes a
serialisation boundary.

### 3. Debugging mutations requires tracing into Rust

When a `save()` produces unexpected links (wrong predicate, missing property,
duplicate link), debugging the create path requires reading the Rust executor's
SHACL interpreter. The update path is debuggable entirely in TypeScript — you
can set breakpoints in `mutation.ts` and inspect every link operation.

### 4. The SHACL "constructor placeholder" pattern is fragile

The current SHACL constructor emits placeholder links like:

```turtle
ad4m://constructorActions → addLink(this, "test://title", "literal://string:")
```

Then `createSubject` calls setter actions to overwrite the placeholder with the
real value. This means every property is written **twice** on creation — once as
a placeholder, once as the real value. If the setter action fails, the
placeholder persists as stale data.

---

## Proposed Architecture

### Create path (proposed)

```
ORM:  instance.save()                  [TypeScript]
  → mutation.innerCreate()             [new function]
    → for each @Flag:     addLink(id, predicate, flagValue)
    → for each @Property: addLink(id, predicate, literal://value)
    → for each @HasMany:  links already exist (added via addX())
    → captureSnapshot(instance)
    → done
```

This mirrors the update path. The ORM reads decorator metadata directly and
creates links without going through SHACL. Each property is written **once**
with the correct value — no placeholder/overwrite dance.

### What `innerCreate()` would look like

```ts
async function innerCreate(ctx: MutationContext): Promise<void> {
  const metadata = getModelMetadata(ctx.instance.constructor);

  // 1. Write flag links (type markers)
  for (const [key, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.flag) {
      await ctx.perspective.add(
        new Link({
          source: ctx.id,
          predicate: propMeta.through,
          target: propMeta.initial, // flag value
        }),
      );
    }
  }

  // 2. Write scalar property links
  for (const [key, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.flag || propMeta.readOnly) continue;
    const value = ctx.instance[key];
    if (value === undefined || value === null) continue;

    const target = encodeAsLiteral(value, propMeta);
    await ctx.perspective.add(
      new Link({
        source: ctx.id,
        predicate: propMeta.through,
        target,
        ...(propMeta.local ? { local: true } : {}),
      }),
    );
  }

  // 3. Write forward-relation links
  //    (Relations added via addX() before save() already have links —
  //     but relations set as initial data in create() need links too)
  for (const [key, relMeta] of Object.entries(metadata.relations)) {
    if (relMeta.direction === "reverse") continue;
    const value = ctx.instance[key];
    if (!value) continue;

    const ids = Array.isArray(value)
      ? value.map((v) => (typeof v === "string" ? v : v.id))
      : [typeof value === "string" ? value : value.id];

    for (const targetId of ids) {
      await ctx.perspective.add(
        new Link({
          source: ctx.id,
          predicate: relMeta.through,
          target: targetId,
          ...(relMeta.local ? { local: true } : {}),
        }),
      );
    }
  }
}
```

### What `save()` becomes

```ts
async save(batchId?: string) {
  if (this._savedOnce) {
    await mutation.innerUpdate(this._mutationContext(), batchId);
  } else {
    await mutation.innerCreate(this._mutationContext(), batchId);
  }
  this._savedOnce = true;
}
```

This is **much simpler** than the current flow which goes through
`createSubject` → SHACL → Rust.

---

## What SHACL Should Still Be Used For

SHACL shapes are valuable. They should stay — just not in the mutation path.

### 1. Validation

"Does this subgraph conform to the Channel shape?" SHACL was literally designed
for this. After creating/updating an instance, the runtime could optionally
validate the resulting subgraph against its shape.

### 2. Interop / Schema Discovery

Other agents or tools can read a perspective's SHACL shapes to understand what
data structures exist. "What does a Message look like in this neighbourhood?"
→ read the Message SHACL shape.

### 3. `ensureSDNASubjectClass` / SurrealDB Indexing

The Rust executor uses SHACL shapes to configure SurrealDB's indexing — which
predicates to index, what the expected cardinality is, etc. This is the right
use of SHACL and should continue unchanged.

### 4. Documentation

SHACL shapes are human-readable (in Turtle format) and describe the schema in a
standards-compliant way. They serve as living documentation of the data model.

### Summary

| Use case                      | Keep SHACL?                         | On critical path?                     |
| ----------------------------- | ----------------------------------- | ------------------------------------- |
| Creating instances            | **No** → ORM creates links directly | Was on path, move off                 |
| Updating instances            | Already not used                    | Already off path                      |
| Validation                    | **Yes**                             | Optional / background                 |
| Schema discovery / interop    | **Yes**                             | Not on mutation path                  |
| SurrealDB index configuration | **Yes**                             | On init path (once), not per-mutation |
| Documentation                 | **Yes**                             | Not on any runtime path               |

---

## Implementation Plan

### Step 1: Implement `innerCreate()` in `mutation.ts`

Write the direct-link-creation function as shown above. It reads from the
decorator metadata registry (same source as `innerUpdate()`) and creates links
via the perspective API.

**Prerequisite:** Understand how `encodeAsLiteral()` should work — the current
SHACL path uses `resolveLanguage` to determine how to encode values. The new
path needs the same logic. This likely already exists somewhere in the codebase
(check how `setProperty` in `mutation.ts` encodes values for updates).

### Step 2: Add a feature flag

```ts
const USE_DIRECT_CREATE = true; // flip to false to revert
```

In `save()`, when `!this._savedOnce`:

- If flag is on: call `innerCreate()` directly
- If flag is off: call `createSubject()` (current SHACL path)

This allows A/B testing and safe rollback.

### Step 3: Ensure `ensureSDNASubjectClass` still works

The SHACL shape must still be generated and registered with the perspective so
SurrealDB knows how to index the model. This is the `ensureSDNASubjectClass` /
`register()` call that happens at app startup — it's separate from per-instance
mutation and should continue unchanged.

Verify that SurrealDB correctly indexes instances created via direct links
(without going through `createSubject`). The index should be based on link
patterns matching the SHACL shape, not on how the links were created.

### Step 4: Run full test suite

- All `model-query.test.ts` tests
- All `model-transactions.test.ts` tests
- Flux app smoke test (create channel, send message, pin channel)
- Verify SHACL shapes are still generated correctly (`generateSHACL()` is unchanged)

### Step 5: Remove the SHACL mutation code path

Once validated, remove `createSubject()` from the ORM's save path. Keep:

- `generateSHACL()` for shape generation
- `ensureSDNASubjectClass()` for registration
- The Rust SHACL parser for index configuration

Remove:

- The `constructorActions` / `setterActions` interpretation in Rust (or leave it
  for backward compatibility with older clients, but the ORM no longer uses it)
- The `initialValues` mechanism in `createSubject`

---

## Risks and Considerations

### 1. `createSubject` may do more than just create links

The Rust `createSubject` implementation may have side effects beyond link
creation — e.g. notifying neighbourhood sync, triggering specific SurrealDB
operations, or updating internal state. These would need to be replicated or
triggered separately in the direct-create path.

**Mitigation:** Audit the Rust `createSubject` implementation before starting.
If it does more than link creation, those side effects need to be available via
a separate API call.

### 2. Batch operations

The current `save(batchId)` pattern passes a batch ID to `createSubject`. The
direct-create path needs to pass the same batch ID to each `addLink()` call so
they're grouped in the same transaction.

### 3. Backward compatibility

Older versions of the Rust executor expect `createSubject` calls. If the
TypeScript ORM starts creating links directly, it must still work with both old
and new executor versions during the transition period.

**Mitigation:** The feature flag (Step 2) allows reverting per-deployment.

### 4. Neighbourhood sync

When an instance is created via `createSubject`, the executor may signal
neighbourhood peers differently than when individual links are added. Verify
that direct link creation triggers the same sync behaviour.
