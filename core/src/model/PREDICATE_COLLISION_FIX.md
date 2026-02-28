# Fix Predicate Collision: Type-Safe Relation Hydration

> **Status:** Agreed — implementing Path 3 (Hybrid)  
> **Created:** 2026-02-28  
> **Priority:** High — this is the root cause of the channel pinning duplication bug

---

## Table of Contents

1. [The Bug](#the-bug)
2. [Root Cause](#root-cause)
3. [Why This Happens](#why-this-happens)
4. [The Deeper Problem](#the-deeper-problem)
5. [Solution Options](#solution-options)
6. [Recommendation](#recommendation)
7. [Implementation Plan](#implementation-plan)
8. [Migration Strategy](#migration-strategy)

---

## The Bug

When a user pins a channel in Flux, all conversations inside that channel get
duplicated. The channel ends up with two copies of every conversation link.

### Reproduction

1. Create a channel with 3 conversations
2. Pin the channel (toggle `isPinned`)
3. Observe: the channel now has 6 conversations (3 originals + 3 duplicates)

### Immediate cause

`Channel.findOne()` hydrates the full channel, including all relations. When
`save()` is called to persist the `isPinned` change, it re-writes **all**
relation links — including `conversations` — creating duplicates.

This was partially fixed by the **dirty tracking** system (see `snapshot.ts`),
which detects unchanged relations and skips them during `save()`. But the dirty
tracking fix treats the symptom. The underlying predicate collision problem
remains.

---

## Root Cause

The `Channel` model has two different relations that use the **same predicate**:

```ts
@Model("Channel")
class Channel extends Ad4mModel {
  @HasMany({ type: "Conversation", through: "ad4m://has_child" })
  conversations: Conversation[] = [];

  @HasMany({ type: "Channel", through: "ad4m://has_child" })
  childChannels: Channel[] = [];
}
```

Both `conversations` and `childChannels` use `"ad4m://has_child"` as their
predicate. In the link store, a channel's children look like:

```
channel-1 --ad4m://has_child--> conversation-A
channel-1 --ad4m://has_child--> conversation-B
channel-1 --ad4m://has_child--> child-channel-X
```

When the ORM hydrates `conversations`, it queries:

```sql
SELECT * FROM links WHERE source = 'channel-1' AND predicate = 'ad4m://has_child'
```

This returns **all three links** — including the child channel link. The ORM
then tries to instantiate all of them as `Conversation` objects. The child
channel link either:

1. Gets silently instantiated as a malformed `Conversation` (wrong type), or
2. Gets filtered out during hydration because it doesn't have the right SHACL
   type marker

In practice, the ORM relies on SurrealDB's subject-class index to filter by
type, so the hydration usually works correctly for **reading**. But when `save()`
re-writes the relation links, it doesn't know which links in the store are
"conversations" and which are "child channels" — they all have the same
predicate.

---

## Why This Happens

### RDF predicates are untyped

In RDF, a triple `(subject, predicate, object)` says nothing about the **type**
of the object. The predicate `ad4m://has_child` means "this thing has a child"
but doesn't say whether the child is a Conversation, a Channel, or a Cat.

This is by design in RDF — predicates are intentionally generic to enable
schema-free linking. But the ORM pretends predicates carry type information. When
it sees `@HasMany({ type: "Conversation", through: "ad4m://has_child" })`, it
assumes all `has_child` links from a Channel point to Conversations.

### The ORM reads correctly but writes naively

**Reading** works because the ORM doesn't just query by predicate — it also
checks the subject class (via SurrealDB's SHACL index). So when hydrating
`conversations`, it effectively does:

```
WHERE predicate = 'ad4m://has_child' AND target.type = 'Conversation'
```

**Writing** is where things break. When `save()` re-writes `conversations`, it:

1. Removes all links with predicate `ad4m://has_child` from the source
2. Re-adds links for each item in the `conversations` array

Step 1 also removes the `childChannels` links (same predicate). Step 2 doesn't
add them back because they weren't in the `conversations` array.

With dirty tracking, `save()` now skips unchanged relations entirely. But if
`conversations` **is** changed (say, a new conversation is added), the
re-write in step 1 still blows away the child channel links.

---

## The Deeper Problem

This isn't specific to Channel. **Any model that reuses a predicate across
multiple relations** will have the same issue. And it's not obvious to the
developer that they're creating a collision — `has_child` is a perfectly natural
predicate for both "a channel's conversations" and "a channel's sub-channels".

### Examples of potential collisions

```ts
// Collision: same predicate for different target types
@HasMany({ type: "Message", through: "ad4m://has_item" })
messages: Message[];

@HasMany({ type: "Attachment", through: "ad4m://has_item" })
attachments: Attachment[];

// Collision: @HasMany and @HasOne with same predicate
@HasMany({ type: "Tag", through: "ad4m://tagged_with" })
tags: Tag[];

@HasOne({ type: "Tag", through: "ad4m://tagged_with" })
primaryTag: Tag;
```

---

## Solution Options

### Option A: Enforce Unique Predicates (Recommended First Step)

**Rule:** Within a single `@Model`, every `through` predicate must be unique
across all `@HasMany`, `@HasOne`, `@BelongsToOne`, and `@BelongsToMany`
decorators.

**Implementation:**

```ts
// In the @Model decorator:
function Model(name: string) {
  return function (constructor: Function) {
    const relations = relationRegistry.get(constructor) ?? [];
    const predicates = new Map<string, string>(); // predicate → field name

    for (const rel of relations) {
      const existing = predicates.get(rel.through);
      if (existing) {
        throw new Error(
          `@Model("${name}"): predicate "${rel.through}" is used by both ` +
            `"${existing}" and "${rel.key}". Each relation must use a unique ` +
            `predicate. Use "ad4m://has_conversation" and ` +
            `"ad4m://has_child_channel" instead of reusing "ad4m://has_child".`,
        );
      }
      predicates.set(rel.through, rel.key);
    }

    // ... rest of @Model logic
  };
}
```

**Migration for Channel:**

```ts
// Before (collision):
@HasMany({ type: "Conversation", through: "ad4m://has_child" })
conversations: Conversation[] = [];

@HasMany({ type: "Channel", through: "ad4m://has_child" })
childChannels: Channel[] = [];

// After (unique predicates):
@HasMany({ type: "Conversation", through: "ad4m://has_conversation" })
conversations: Conversation[] = [];

@HasMany({ type: "Channel", through: "ad4m://has_child_channel" })
childChannels: Channel[] = [];
```

**Pros:**

- Simple to implement (~20 lines)
- Catches the bug at definition time (loud error, not silent data corruption)
- No changes to the query/mutation pipeline
- Aligns with how the ORM actually uses predicates (as typed foreign keys)

**Cons:**

- Loses some RDF generality (predicates become model-specific keys)
- Doesn't solve the case where two different models use the same predicate
  pointing at the same target type (but that's not a real problem in practice)

---

### Option B: Type-Filtered Hydration and Mutation

**Rule:** The ORM always considers the target type when reading and writing
relation links. A relation `@HasMany({ type: "Conversation", through: "ad4m://has_child" })`
only reads/writes links where the target is a `Conversation`.

**Reading (already works):**
SurrealDB's subject-class index already filters by type during hydration.

**Writing (needs change):**

```ts
// In mutation.ts, when updating a HasMany relation:
async function updateRelation(ctx, key, relMeta) {
  const currentLinks = await ctx.perspective.get(
    new LinkQuery({
      source: ctx.id,
      predicate: relMeta.through,
    }),
  );

  // Only touch links whose targets are of the correct type
  const relevantLinks = currentLinks.filter((link) =>
    isOfType(link.target, relMeta.type),
  );

  const desiredIds = new Set(ctx.instance[key].map((v) => v.id));
  const currentIds = new Set(relevantLinks.map((l) => l.data.target));

  // Remove links that are no longer in the array
  for (const link of relevantLinks) {
    if (!desiredIds.has(link.data.target)) {
      await ctx.perspective.remove(link);
    }
  }

  // Add links for new items
  for (const id of desiredIds) {
    if (!currentIds.has(id)) {
      await ctx.perspective.add(
        new Link({
          source: ctx.id,
          predicate: relMeta.through,
          target: id,
        }),
      );
    }
  }
}
```

The key change is `isOfType()` — checking whether a target has the expected
subject-class marker before touching its link.

**Pros:**

- No breaking changes to existing predicates
- Preserves RDF generality (same predicate, different types, handled correctly)
- More aligned with how RDF actually works

**Cons:**

- **Mutation cost:** `isOfType()` requires querying each target's type
  (additional DB lookups). For a `HasMany` with N items, that's up to N extra
  SurrealDB queries per `save()`. These could be batched into a single
  `WHERE id IN [...]` query, but the round-trip still exists.
- **Query cost: none.** The read path already uses SurrealDB's subject-class
  index to filter by type — this is how hydration works today regardless of
  which option you choose. Option B doesn't change the query path at all.
  The cost is purely on the write side.
- Adds complexity to the mutation path (harder to debug)
- Still doesn't prevent the developer from accidentally creating ambiguous
  schemas — the ORM silently handles it, so the developer may never realise
  two relations share a predicate

---

### Option C: Compound Predicates

**Rule:** The ORM automatically generates unique predicates by combining the
user-specified predicate with the target type.

```ts
@HasMany({ type: "Conversation", through: "ad4m://has_child" })
conversations: Conversation[] = [];

// Internally becomes: predicate = "ad4m://has_child#Conversation"

@HasMany({ type: "Channel", through: "ad4m://has_child" })
childChannels: Channel[] = [];

// Internally becomes: predicate = "ad4m://has_child#Channel"
```

**Pros:**

- No breaking API changes for the developer
- Unique predicates without manual naming
- Query and mutation just work

**Cons:**

- Breaks RDF interop (non-standard predicate URIs)
- Existing data uses un-suffixed predicates — needs migration
- Hides what's actually happening (developer sees `has_child`, store has `has_child#Conversation`)
- Fragile if model names change

---

## Recommendation

### These are alternative strategies, not sequential steps

**Important:** Options A and B are mutually exclusive philosophies. If Option B
is implemented (the ORM correctly handles shared predicates via type filtering),
then Option A's hard error is wrong — it forbids something the ORM now handles
correctly. You can't enforce "predicates must be unique" and then build a system
that correctly handles non-unique predicates. Pick one:

### Path 1: Option A alone — Enforce Unique Predicates

Choose this if you believe:

- The ORM should treat predicates as typed foreign keys (one predicate = one relation)
- RDF generality within a single model isn't needed
- Developers should be explicit about their link semantics

**What you get:**

- ~20 lines of code in `@Model`
- Zero changes to query or mutation logic
- Bugs caught at definition time with a clear error message
- Self-documenting schemas (`ad4m://has_conversation` is clearer than `ad4m://has_child`)

**What you give up:**

- Developers can't reuse a predicate across relations within a model
- Existing data with shared predicates needs migration (rename links)

**This is the simpler path.** No new runtime complexity. The fix is in the
schema, not the engine.

### Path 2: Option B alone — Type-Filtered Mutation

Choose this if you believe:

- Shared predicates are a valid RDF pattern the ORM should support
- The ORM should be smart enough to handle them correctly
- Forcing predicate renaming across existing data is too costly

**What you get:**

- Correct read and write behaviour even with shared predicates
- No breaking changes to existing models or data
- Better RDF alignment

**What you give up:**

- Extra `isOfType()` lookups during mutation (write-side cost only — the read
  path already uses SurrealDB's type index and is unaffected)
- More complex mutation logic (harder to debug)
- Developers still won't get a warning when they create an ambiguous schema —
  the ORM silently handles it, which means the developer may not even realise
  two relations share a predicate until they inspect the link store

### Path 3: Option A as warning + Option B as fix (Hybrid)

There is one valid combination: use Option A as a **warning** (not error) and
Option B as the actual correctness mechanism:

```ts
// In @Model:
if (predicateCollision) {
  console.warn(
    `[AD4M ORM] Model "${name}": predicate "${rel.through}" is shared by ` +
      `"${existing}" and "${rel.key}". The ORM handles this correctly via ` +
      `type-filtered mutation, but consider using unique predicates for clarity.`,
  );
}
```

This gives developers visibility ("hey, you're sharing predicates") without
breaking their code, while the type-filtering ensures correctness regardless.

**This is the recommended path.** It fixes the bug at the engine level,
gives developers visibility without breaking changes, and leaves predicate
renaming as an optional clean-up rather than a hard prerequisite.

### Our recommendation: Path 3 (Hybrid — Warning + Type-Filtered Mutation)

After further discussion, Path 3 is the optimal route. Here's why:

1. **Correctness by default.** Type-filtered mutation (Option B) means the ORM
   handles shared predicates correctly regardless of whether the developer
   notices the collision. Silent data corruption is eliminated at the engine
   level, not just at the schema level.

2. **The warning is still valuable.** Shared predicates are almost always a
   modelling mistake. The `console.warn` in `@Model` gives developers immediate
   visibility without breaking existing code or requiring a forced migration.
   It's a nudge, not a blocker.

3. **Write-side cost is acceptable.** The `isOfType()` lookups during mutation
   can be batched into a single `WHERE id IN [...]` query per relation, keeping
   the extra round-trip to one per dirty relation per `save()`. For the typical
   model this is negligible, and the correctness guarantee outweighs it.

4. **No forced migration.** Path 1 requires renaming all colliding predicates
   and writing a data migration before shipping. Path 3 fixes the bug
   immediately in the engine, then lets predicate renaming happen at a more
   comfortable pace — or not at all, if developers prefer to leave their schemas
   as-is.

5. **Future-proof.** If RDF interop with external agents becomes a requirement,
   shared predicates are already handled correctly. Nothing needs to be
   re-architected later.

---

## Implementation Plan

### Phase 1: Audit All Models

Search all model files across Flux and We for predicate collisions:

```bash
# Find all @HasMany/@HasOne/@BelongsTo* decorators and their predicates
grep -rn '@Has\|@BelongsTo' --include='*.ts' | grep 'through:' | \
  sed 's/.*through:\s*["'\'']\([^"'\'']*\)["'\''].*/\1/' | \
  sort | uniq -c | sort -rn
```

Confirm the Channel collision is the only one, or identify others.

### Phase 2: Implement Type-Filtered Mutation (Option B — the core fix)

Update `mutation.ts` so that when re-writing a relation, the ORM only touches
links whose targets are of the correct type. This eliminates the data corruption
regardless of whether predicates are shared.

```ts
// In mutation.ts, when updating a HasMany relation:
async function updateRelation(ctx, key, relMeta) {
  const currentLinks = await ctx.perspective.get(
    new LinkQuery({
      source: ctx.id,
      predicate: relMeta.through,
    }),
  );

  // Only touch links whose targets are of the correct type — this ensures
  // shared predicates across different relation fields don't clobber each other.
  const relevantLinks = await filterByType(currentLinks, relMeta.type);

  const desiredIds = new Set(ctx.instance[key].map((v) => v.id));
  const currentIds = new Set(relevantLinks.map((l) => l.data.target));

  for (const link of relevantLinks) {
    if (!desiredIds.has(link.data.target)) {
      await ctx.perspective.remove(link);
    }
  }

  for (const id of desiredIds) {
    if (!currentIds.has(id)) {
      await ctx.perspective.add(
        new Link({ source: ctx.id, predicate: relMeta.through, target: id }),
      );
    }
  }
}
```

`filterByType` should batch the type lookups into a single `WHERE id IN [...]`
query to keep the extra round-trip to one per dirty relation per `save()`.

### Phase 3: Add @Model Collision Warning (Option A as warning)

Add the duplicate-predicate check in the `@Model` decorator, emitting a
**warning** (not an error) so developers are alerted without breaking existing
code:

```ts
// In the @Model decorator:
const relations = relationRegistry.get(constructor) ?? [];
const predicates = new Map<string, string>();

for (const rel of relations) {
  const existing = predicates.get(rel.through);
  if (existing) {
    console.warn(
      `[AD4M ORM] Model "${name}": predicate "${rel.through}" is shared by ` +
        `"${existing}" and "${rel.key}". The ORM handles this correctly via ` +
        `type-filtered mutation, but consider using unique predicates for clarity.`,
    );
  }
  predicates.set(rel.through, rel.key);
}
```

### Phase 4: (Optional) Rename Colliding Predicates

With Phases 2 and 3 in place the bug is already fixed. Renaming predicates is
now a clean-up exercise, not a hard requirement. Do it when convenient:

```ts
// Before:
@HasMany({ type: "Conversation", through: "ad4m://has_child" })
conversations: Conversation[] = [];

@HasMany({ type: "Channel", through: "ad4m://has_child" })
childChannels: Channel[] = [];

// After:
@HasMany({ type: "Conversation", through: "ad4m://has_conversation" })
conversations: Conversation[] = [];

@HasMany({ type: "Channel", through: "ad4m://has_child_channel" })
childChannels: Channel[] = [];
```

If predicates are renamed, ship a one-time data migration run on app startup
(gated by a version flag) to rewrite existing links:

```ts
async function migrateChannelPredicates(perspective: PerspectiveProxy) {
  const links = await perspective.get(
    new LinkQuery({ predicate: "ad4m://has_child" }),
  );
  for (const link of links) {
    const targetType = await getSubjectClass(link.data.target);
    let newPredicate: string;
    if (targetType === "Conversation") {
      newPredicate = "ad4m://has_conversation";
    } else if (targetType === "Channel") {
      newPredicate = "ad4m://has_child_channel";
    } else {
      continue; // leave non-Channel/Conversation has_child links untouched
    }
    await perspective.remove(link);
    await perspective.add(new Link({ ...link.data, predicate: newPredicate }));
  }
}
```

### Phase 5: Verify

- All existing tests pass
- Channel pinning no longer duplicates conversations
- New conversations appear correctly
- Child channels appear correctly
- Shared-predicate warning fires in dev console for any colliding models

---

## Migration Strategy

### Identifying Collisions

Run this audit across all model files:

```bash
# Find all @HasMany/@HasOne/@BelongsTo* decorators and their predicates
grep -rn '@Has\|@BelongsTo' --include='*.ts' | grep 'through:' | \
  sed 's/.*through:\s*["'\'']\([^"'\'']*\)["'\''].*/\1/' | \
  sort | uniq -c | sort -rn
```

### Known Collisions (from Flux audit)

| Model   | Field 1         | Field 2         | Shared Predicate   |
| ------- | --------------- | --------------- | ------------------ |
| Channel | `conversations` | `childChannels` | `ad4m://has_child` |

> **Note:** This list should be expanded with a full audit of all models in
> Flux (~22 models) and We (~10 models).

### Renaming Convention

When splitting a shared predicate, use descriptive relation-specific names:

| Before                                  | After                      |
| --------------------------------------- | -------------------------- |
| `ad4m://has_child` (for conversations)  | `ad4m://has_conversation`  |
| `ad4m://has_child` (for child channels) | `ad4m://has_child_channel` |
| `ad4m://has_item` (for messages)        | `ad4m://has_message`       |
| `ad4m://has_item` (for attachments)     | `ad4m://has_attachment`    |

The pattern is: `ad4m://has_<singular_relation_name>`.

### Data Migration Considerations

1. **Local-first:** Each user's local perspective needs migration. Ship the
   migration as part of the app update — run it once on startup if a version
   flag isn't set.

2. **Shared neighbourhoods:** Other agents may still create links with the old
   predicate. The ORM should accept both old and new predicates during a
   transition period, but only write new predicates.

3. **Ordering:** Migrate reads first (accept both predicates), then writes (use
   new predicate), then drop old-predicate support.

### Transition Period Query

During migration, the ORM can query both predicates:

```ts
// In operations.ts, during include hydration:
const predicates = relMeta.legacyPredicates
  ? [relMeta.through, ...relMeta.legacyPredicates]
  : [relMeta.through];

// Query each predicate and merge results
```

This could be supported via a decorator option:

```ts
@HasMany({
  type: "Conversation",
  through: "ad4m://has_conversation",
  legacyPredicates: ["ad4m://has_child"],  // also read from old predicate
})
conversations: Conversation[] = [];
```

Once all data is migrated, remove the `legacyPredicates` option.
