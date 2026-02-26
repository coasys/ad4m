# Ad4mModel — ORM layer for AD4M perspectives

`Ad4mModel` is a class-based ORM that maps TypeScript objects to subgraphs inside an
AD4M [`PerspectiveProxy`](../perspectives/PerspectiveProxy.ts). Each instance is a
cluster of RDF-style link triples sharing a common **base URI** (the instance `id`).

---

## Quick start

```typescript
import { Ad4mModel, Model, Property, Flag, HasMany } from "@coasys/ad4m";

@Model({ name: "Recipe" })
export class Recipe extends Ad4mModel {
  @Flag({ through: "recipe://type", value: "recipe://Recipe" })
  type: string = "";

  @Property({ through: "recipe://title", resolveLanguage: "literal" })
  title: string = "";

  @Property({ through: "recipe://status", initial: "recipe://draft" })
  status: string = "";

  @HasMany({ through: "recipe://ingredient" })
  ingredients: string[] = [];
}

// Install the SHACL/SDNA subject class once (idempotent):
await Recipe.register(perspective);

// Create
const r = await Recipe.create(perspective, { title: "Pasta" });

// Read
const loaded = new Recipe(perspective, r.id);
await loaded.get();
console.log(loaded.title); // "Pasta"

// Update
loaded.title = "Pasta al Pomodoro";
await loaded.save();

// Delete
await loaded.delete();
```

---

## Architecture

```
PerspectiveProxy (graph store)
  └── "subject" node  ←→  Ad4mModel instance
        ├── ad4m://type → "recipe://Recipe"    (@Flag)
        ├── recipe://title → Literal("Pasta")  (@Property)
        └── recipe://ingredient → <uri>…       (@HasMany)
```

- **`id`** — the base URI of the instance subgraph (auto-generated from a random
  24-char literal if not supplied to the constructor).
- **Properties** — single-valued link triples `(id, predicate, value)` described by
  `@Property` or `@Flag`.
- **Relations** — multi-valued link sets described by `@HasMany`, `@HasOne`,
  `@BelongsToOne`, `@BelongsToMany`; each generates `add*`, `remove*`, `set*` methods.
- **Queries** — `findAll`, `findOne`, `query()` builder, `paginate`, `count` — all
  compiled to SurrealQL and executed against the perspective's local graph engine.
- **Subscriptions** — `subscribe()` / `query().live()` re-run the query on each
  relevant link change and deliver fresh results via callback.

> **Layer boundary**: `PerspectiveProxy` still uses `baseExpression` in its
> protocol-level Subject API. `Ad4mModel` exposes `id` as its ORM abstraction —
> the two refer to the same URI.

---

## Decorator reference

### `@Model({ name })`

**Required** on every class extending `Ad4mModel`. Registers the SDNA subject class
name used by the perspective's Prolog engine and enables all static query methods.

```typescript
@Model({ name: "Comment" })
class Comment extends Ad4mModel { ... }
```

---

### `@Property(opts)`

Single-valued property backed by one link triple.

| Option            | Description                                                           |
| ----------------- | --------------------------------------------------------------------- |
| `through`         | Predicate URI **(required)**                                          |
| `initial`         | Default value added by the constructor action                         |
| `required`        | Adds `sh:minCount 1`                                                  |
| `writable`        | Generates a setter action (default `true` when `through` is set)      |
| `resolveLanguage` | `"literal"` to store/retrieve via the Literal language                |
| `local`           | Store only in the local perspective (not synced to the network)       |
| `getter`          | Custom SurrealQL expression; used for computed / read-only properties |
| `transform`       | Post-fetch `(rawValue) => transformedValue` function                  |

```typescript
@Property({ through: "post://title", resolveLanguage: "literal" })
title: string = "";

// Read-only computed value
@Property({
  through: "post://wordCount",
  writable: false,
  getter: `array::len(string::split((<-link[WHERE predicate='post://body'].in.uri)[0], ' '))`,
})
wordCount: number = 0;
```

---

### `@Flag({ through, value })`

Immutable type-marker: a `@Property` whose value is fixed at creation and never
changed. Useful for type discrimination.

```typescript
@Flag({ through: "ad4m://type", value: "flux://Message" })
type: string = "";
```

---

### `@HasMany(opts)` / `@HasOne(opts)`

Forward relation: generates `add*`, `remove*`, and `set*` instance methods.

```typescript
@HasMany({ through: "post://tag" })
tags: string[] = [];

// Typed — only returns URIs that are instances of the related model
@HasMany(() => Comment, { through: "post://comment" })
comments: Comment[] = [];
export interface Post extends HasManyMethods<"tags" | "comments"> {}
```

`@HasOne` is the same but limits the collection to a single value (`sh:maxCount 1`).

---

### `@BelongsToOne(relatedModel, opts)` / `@BelongsToMany(relatedModel, opts)`

Reverse relation: traverses links **owned by the other side** — read-only, no mutator
methods generated.

```typescript
@BelongsToOne(() => Post, { through: "post://comment" })
post: string = "";
```

---

## Querying

### Static helpers

```typescript
// All instances
const all = await Recipe.findAll(perspective);

// Filtered
const hot = await Recipe.findAll(perspective, {
  where: { status: "recipe://published", rating: { gt: 4 } },
  order: { createdAt: "DESC" },
  limit: 20,
  offset: 0,
});

// Count
const n = await Recipe.count(perspective, {
  where: { status: "recipe://draft" },
});

// With total (for pagination UI)
const { results, totalCount } = await Recipe.findAllAndCount(perspective, {
  limit: 10,
});

// Explicit page
const page = await Recipe.paginate(perspective, 10, 2);
```

### Fluent query builder

```typescript
const results = await Recipe.query(perspective)
  .where({ status: "recipe://published" })
  .order({ createdAt: "DESC" })
  .limit(10)
  .get();

// First match
const one = await Recipe.query(perspective).where({ title: "Pasta" }).first();

// Count matching records
const n = await Recipe.query(perspective)
  .where({ status: "recipe://draft" })
  .count();
```

### Subscriptions

```typescript
const sub = Recipe.subscribe(
  perspective,
  { where: { status: "recipe://cooking" }, debounce: 300 },
  (recipes) => setRecipes(recipes),
);

// Or via the builder:
const sub2 = Recipe.query(perspective)
  .where({ status: "recipe://cooking" })
  .live((recipes) => setRecipes(recipes));

// Always clean up:
sub.unsubscribe();
sub2.unsubscribe();
```

---

## Transactions (batch operations)

```typescript
await Ad4mModel.transaction(perspective, async (tx) => {
  const post = new Post(perspective);
  post.title = "Hello world";
  await post.save(tx.batchId);

  const comment = new Comment(perspective);
  comment.body = "First!";
  await comment.save(tx.batchId);
});
// Commits atomically; any throw aborts the batch
```

---

## `fromJSONSchema` — dynamic model generation

Build an `Ad4mModel` subclass from a JSON Schema at runtime (useful for generic tooling
or schema-driven UIs):

```typescript
const schema = {
  title: "Person",
  type: "object",
  properties: {
    name: { type: "string" },
    email: { type: "string" },
  },
};

// Option 1 — explicit namespace
const PersonClass = Ad4mModel.fromJSONSchema(schema, {
  name: "Person",
  namespace: "person://",
  resolveLanguage: "literal",
});

// Option 2 — property-level predicate override
const ContactClass = Ad4mModel.fromJSONSchema(schema, {
  name: "Contact",
  namespace: "contact://",
  propertyMapping: {
    name: "foaf://name",
    email: "foaf://mbox",
  },
});

// Option 3 — embed predicates in the schema itself
const schemaWithMeta = {
  title: "Product",
  "x-ad4m": { namespace: "product://" },
  properties: {
    name: { type: "string", "x-ad4m": { through: "product://title" } },
  },
};
const ProductClass = Ad4mModel.fromJSONSchema(schemaWithMeta, {
  name: "Product",
});

await ProductClass.register(perspective);
const p = await ProductClass.create(perspective, { name: "Widget" });
```

---

## Inheritance

Use `@Model` on the parent **and** the child. The SHACL generator emits `sh:node` to
reference the parent shape instead of duplicating its properties:

```typescript
@Model({ name: "BaseBlock" })
class BaseBlock extends Ad4mModel {
  @Property({ through: "block://createdBy" })
  createdBy: string = "";
}

@Model({ name: "PollBlock" })
class PollBlock extends BaseBlock {
  @Property({ through: "poll://question" })
  question: string = "";
}
// PollBlock SHACL shape: sh:node <block://BaseBlockShape> + own poll:// properties only
```
