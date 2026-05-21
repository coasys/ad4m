# 9. Ad4mModel — Application Data Model API

## 9.1 Overview

Ad4mModel is the **primary developer-facing API** for building applications on AD4M. It provides a decorator-based, ORM-like abstraction over the link graph, SHACL shapes, and SPARQL queries.

Application developers SHOULD use Ad4mModel for all data operations. Developers SHOULD NOT interact directly with links, SHACL shapes, or Holochain primitives unless building framework-level tooling.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

## 9.2 Model Declaration

A model class declares a Subject Class using decorators that generate SHACL shapes (see §4).

### @Model

Registers a class as a Subject Class:

```typescript
@Model({ name: "Message" })
class Message extends Ad4mModel {
  // properties and relations
}
```

The `name` parameter MUST be unique within the perspective and becomes the SHACL target class identifier. The decorator:
- Sets `className` on both the prototype and the constructor.
- Generates `generateSDNA()` and `generateSHACL()` static methods from decorator metadata.
- Uses memoized SHACL generation via `getMemoizedSHACL()` to avoid recomputation.

### @Property

Declares a scalar property mapped to a link predicate:

```typescript
@Property({
  through: "flux://has_body",       // Link predicate URI (REQUIRED)
  resolveLanguage: "literal",       // Expression language for value resolution (default: "literal")
  required: false,                  // Maps to sh:minCount 1 (default: false)
  initial: undefined,               // Default value on creation (ad4m://initial)
  readOnly: false,                  // If true, no setter generated (default: false)
  local: false,                     // Store as local-only link (default: false)
  getter: undefined,                // Custom graph traversal expression
  transform: undefined,             // Post-retrieval value transform function
  options: undefined,               // Enum constraint (sh:in) — array of {value, label?}
})
body: string = "";
```

Smart defaults:
- `required` → `false` (properties are optional by default)
- `readOnly` → `false`
- `resolveLanguage` → `"literal"`
- `initial` → `undefined` (no link created until a value is explicitly set); when `required: true` and no `initial` is provided, defaults to `"literal:string:uninitialized"` as a sentinel.

- `through` — The predicate URI used in the link triple `(instance, through, value)`. REQUIRED unless `getter` is provided.
- `resolveLanguage` — If set, the target expression is dereferenced through the named Language. Default: `"literal"`.
- `required` — If `true`, generates `sh:minCount 1` and includes a sentinel initial value. Instances without this property will not match the Subject Class shape.
- `initial` — Default value set during instance creation via constructor actions.
- `readOnly` — If `true`, no setter method is generated. Maps to `ad4m://writable false`.
- `getter` — Custom graph traversal expression. The expression can reference `Base` which is replaced with the instance's base expression. Example: `"SELECT ?target WHERE { <Base> <flux://has_reply> ?target . } LIMIT 1"`.
- `options` — Array of `{ value: string, label?: string }` objects defining the allowed values for this property. Generates an `sh:in` constraint on the SHACL property shape.

### @Optional

Equivalent to `@Property` with `required: false` and without automatic `resolveLanguage` or `initial` defaults:

```typescript
@Optional({
  through: "flux://has_description",
  resolveLanguage: "literal"
})
description?: string;
```

### @ReadOnly

Equivalent to `@Property` with `readOnly: true`:

```typescript
@ReadOnly({
  through: "post://created_at",
  initial: new Date().toISOString()
})
createdAt: string = "";
```

### @Flag

A boolean flag represented by link existence. If the link exists, the flag is `true`; if absent, `false`. Flags are always required and immutable — they serve as type discriminators for shape conformance:

```typescript
@Flag({
  through: "ad4m://type",
  value: "flux://message"
})
type: string = "";
```

Use of `@Flag` is discouraged unless specifically needed for type-based filtering or discrimination between different kinds of models.

### @HasMany

Declares a one-to-many forward relation. The instance is the link source. Supports two calling conventions:

```typescript
// Options-object style
@HasMany({ through: "flux://has_reaction" })
reactions: string[] = [];

// Target-first shorthand (for typed relations)
@HasMany(() => Comment, { through: "post://comment" })
comments: Comment[] = [];
```

Generates collection methods: `addReactions(value)`, `removeReactions(value)`, `setReactions(values)`.

Options:
- `through` — Predicate URI. Defaults to `'ad4m://has_child'` if omitted.
- `target` — Target model class thunk for hydration and type filtering. Optional.
- `getter` — Custom read-only graph traversal (mutually exclusive with `through` and `target`).
- `filter` — Auto-generate DB-level conformance filter when `target` is set. Default: `true`.
- `where` — Filter constraints on linked targets using the query DSL.
- `local` — Whether the link is stored locally only.

### @HasOne

Declares a one-to-one forward relation (`sh:maxCount 1`):

```typescript
@HasOne(() => Profile, { through: "flux://has_author" })
author?: Profile;
```

### @BelongsToOne

Declares an inverse one-to-one relation. The instance is the link *target*, not the source. Read-only — the owning side manages the link:

```typescript
@BelongsToOne(() => Post, { through: "post://author" })
post?: Post;
```

### @BelongsToMany

Declares an inverse one-to-many relation. Read-only — mutation MUST go through the owning side's `@HasMany`:

```typescript
@BelongsToMany(() => Post, { through: "post://tag" })
posts: Post[] = [];
```

### @Collection

Alias for `@HasMany`:

```typescript
@Collection({ through: "flux://has_block" })
blocks: string[] = [];
```

## 9.3 Decorator Metadata Registry

Decorator metadata is stored using **WeakMaps** keyed by class constructor. This avoids issues with inheritance chains sharing mutable state.

Three registries exist:
- **`propertyRegistry`** — `WeakMap<Function, Record<string, PropertyMetadataEntry>>`
- **`relationRegistry`** — `WeakMap<Function, Record<string, RelationMetadataEntry>>`

Metadata retrieval walks the prototype chain (parent-first order) so subclass decorators compose with parent decorators. Results are memoized per class constructor via `propertiesMetadataCache` and `relationsMetadataCache`.

### Programmatic Registration

For dynamic model creation (e.g., from JSON Schema), metadata can be registered programmatically:

```typescript
setPropertyRegistryEntry(ctor, propName, { through: "...", required: true });
setRelationRegistryEntry(ctor, relName, { predicate: "...", kind: "hasMany" });
```

### Conformance Filters

When a relation has a known `target` model class, `buildConformanceFilter()` inspects the target's property metadata to derive:
- **Flag checks** — predicate + value match (type discrimination)
- **Required property checks** — predicate exists

These conditions are compiled into a SPARQL `SELECT` query used as a getter for the relation, ensuring only conformant linked nodes are returned.

## 9.4 Complete Example

```typescript
@Model({ name: "Space" })
class Space extends Ad4mModel {
  @Property({
    through: "flux://has_name",
    resolveLanguage: "literal",
    required: true
  })
  name: string = "";

  @Optional({
    through: "flux://has_description",
    resolveLanguage: "literal"
  })
  description?: string;

  @Collection({ through: "flux://has_channel" })
  channels: string[] = [];
}

@Model({ name: "Block" })
class Block extends Ad4mModel {
  @Flag({
    through: "ad4m://type",
    value: "flux://block"
  })
  type: string = "";

  @Property({
    through: "flux://has_content",
    resolveLanguage: "literal",
    required: true
  })
  content: string = "";

  @Optional({ through: "flux://has_block_type" })
  blockType?: string;
}
```

## 9.5 Model Lifecycle

### Registration

Before use, a model's SHACL shape MUST be registered in the perspective:

```typescript
await Message.register(perspective);
```

This generates the SHACL shape from decorator metadata and stores it as SDNA links (see §4.2). Implementations SHOULD cache registration and skip if the shape already exists.

The `options` field on `@Property` decorators generates `sh:in` constraints stored as part of the SHACL property shape:

```typescript
@Property({
  through: "task://priority",
  options: [
    { value: "low", label: "Low" },
    { value: "medium", label: "Medium" },
    { value: "high", label: "High" }
  ]
})
priority: string = "";
```

### Creation

```typescript
const msg = await Message.create(perspective, {
  body: "Hello, world!",
  author: "did:key:z6Mk..."
});
```

`create()`:
1. Generates a unique base expression URI for the new instance.
2. Executes SHACL constructor actions (creating links for required properties with initial values).
3. Sets any additional data values provided.
4. Returns a hydrated model instance.

Options:
- `batchId` — Group link mutations into a single sync batch.
- `parent` — Scope the instance under a parent expression via a predicate link.

### Reading (Hydration)

Model instances are hydrated from links. Given a base expression URI, the runtime:
1. Queries all links where the URI is the source.
2. Maps link predicates to property/relation metadata from decorators.
3. Populates the model instance fields from link targets.
4. Evaluates custom getters for properties/relations that define them.

### Updating

```typescript
msg.body = "Updated content";
await msg.save();
```

The model tracks dirty fields. `save()` persists only modified fields as link mutations (`setSingleTarget` for scalar properties, collection operations for relations).

Static bulk update:

```typescript
await Message.update(perspective, instanceId, { body: "New content" });
```

### Deletion

```typescript
await msg.delete();
// or statically:
await Message.remove(perspective, instanceId);
```

Executes SHACL destructor actions, removing all links associated with the instance including incoming links from other instances.

## 9.6 Querying

### Static Query Methods

| Method | Description |
|--------|-------------|
| `Model.findAll(perspective, query?)` | Returns all instances matching the query |
| `Model.findOne(perspective, query?)` | Returns the first matching instance, or `null` |
| `Model.findById(perspective, id)` | Returns the instance with the given base expression URI |
| `Model.count(perspective, query?)` | Returns the count of matching instances |
| `Model.paginate(perspective, pageSize, page, query?)` | Returns a `PaginationResult<T>` |
| `Model.findAllAndCount(perspective, query?)` | Returns `{ results, totalCount }` |

### Query DSL

The `query` parameter accepts a `Query` object:

```typescript
interface Query {
  where?: Where;
  order?: Order;
  limit?: number;
  offset?: number;
  parent?: ParentScope;
  include?: IncludeMap;
  count?: boolean;
}
```

#### Where Clauses

Equality:
```typescript
const done = await Todo.findAll(perspective, {
  where: { state: "done" }
});
```

Comparison operators:
```typescript
const recent = await Post.findAll(perspective, {
  where: {
    createdAt: { gt: "2024-01-01" }
  }
});
```

Supported operators: `equals`, `notEquals`, `gt`, `gte`, `lt`, `lte`, `between`, `like`, `in`, `notIn`.

#### Ordering

```typescript
const sorted = await Post.findAll(perspective, {
  order: { createdAt: "DESC" }
});
```

#### Pagination

```typescript
const page = await Post.paginate(perspective, 20, 1, {
  where: { published: true },
  order: { createdAt: "DESC" }
});
// page.results, page.totalCount, page.pageSize, page.pageNumber
```

#### Parent-Scoped Queries

Queries MAY be scoped to children of a parent expression:

```typescript
// Model form (predicate auto-resolved from relation metadata)
const channelMessages = await Message.findAll(perspective, {
  parent: { model: Channel, id: channelUri }
});

// Raw form (explicit predicate)
const channelMessages = await Message.findAll(perspective, {
  parent: { id: channelUri, predicate: "flux://has_message" }
});
```

#### Eager Loading (include)

Relations can be eager-loaded to avoid N+1 query patterns:

```typescript
const posts = await Post.findAll(perspective, {
  include: {
    comments: true,                              // One level deep
    author: true,                                // HasOne relation
  }
});

// Sub-query: only the 5 most-recent comments
const posts = await Post.findAll(perspective, {
  include: {
    comments: { order: { createdAt: "DESC" }, limit: 5 }
  }
});

// Nested eager-load
const posts = await Post.findAll(perspective, {
  include: {
    comments: { include: { author: true } }
  }
});
```

### Fluent Query Builder

`Model.query()` returns a `ModelQueryBuilder` for chained query construction:

```typescript
const results = await Message.query(perspective)
  .where({ channel: channelId })
  .order({ createdAt: "DESC" })
  .limit(50)
  .offset(0)
  .include({ author: true })
  .execute();
```

### Reactive Queries (subscribe)

`Model.query().subscribe()` returns a reactive subscription. Subscriptions execute **server-side in Rust** — the Rust engine builds trigger SPARQL from the model's SHACL shape predicates and re-executes the query when matching links change, pushing updated results to the client:

```typescript
const subscription = Message.query(perspective)
  .where({ channel: channelId })
  .subscribe((messages) => {
    // Called whenever the result set changes (pushed from server)
    console.log("Updated messages:", messages);
  });

// Later: unsubscribe
subscription.unsubscribe();
```

#### Count Subscriptions

```typescript
const countSub = Message.query(perspective)
  .where({ channel: channelId })
  .countSubscribe((count) => {
    console.log("Message count:", count);
  });
```

#### Paginated Subscriptions

```typescript
const paginatedSub = Message.query(perspective)
  .where({ channel: channelId })
  .order({ createdAt: "DESC" })
  .limit(20)
  .paginateSubscribe((page) => {
    // page.results, page.totalCount
    console.log("Page updated:", page);
  });
```

## 9.7 Query Engine — Rust-Side SPARQL

Ad4mModel queries execute **server-side in Rust**. The SDK is a thin WebSocket client that sends query parameters via the `perspective.modelQuery` RPC operation and receives hydrated JSON instances.

### Architecture

```text
Ad4mModel.findAll(perspective, query)
  → executeModelQuery()
    → WS RPC: perspective.modelQuery
      → Rust model_query.rs
        → SPARQL generation from SHACL shape + query params
        → Typed literal parsing (parse_literal_value)
        → Where filtering (equals, notEquals, gt, gte, lt, lte, like, in, notIn, between)
        → Sorting by any property, ASC/DESC
        → Pagination via limit/offset
        → SPARQL COUNT fast path
        → Include resolution (forward + reverse, recursive, max depth 8)
        → Getter evaluation server-side
      → Returns hydrated JSON instances + totalCount
  → jsonToModelInstance() (constructs class instances from JSON)
```

### Storage Model

The query engine operates over the RDF 1.2 reifier storage model (see [§1.9](./01-core-data-model.md#19-link-storage-model-rdf-12-reifiers)). Direct triples live in the default graph; metadata is accessed via reifier patterns:

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?s WHERE {
  ?s <predicate> ?o .
  ?reifier rdf:reifies <<( ?s <predicate> ?o )>> .
}
```

### Where Filters

ALL comparison operators are SPARQL-pushable — they execute in Rust within the SPARQL engine, not in JavaScript post-hydration:

| Operator | Description |
|----------|-------------|
| `equals` | Exact value match |
| `notEquals` | Negation |
| `gt` | Greater than (numeric/date) |
| `gte` | Greater than or equal (numeric/date) |
| `lt` | Less than (numeric/date) |
| `lte` | Less than or equal (numeric/date) |
| `like` | Pattern match |
| `in` | Set membership |
| `notIn` | Set exclusion |
| `between` | Range (inclusive) |

### Custom SPARQL Functions

The Oxigraph store registers custom functions:

| Function | Description |
|----------|-------------|
| `fn::parse_literal(term)` | Decodes `literal:string:...` URIs into plain string literals. Handles `string:`, `number:`, `boolean:`, and `json:` types. For JSON literals containing a `data` field (signed expressions), extracts just the data value. |
| `fn::strip_html(term)` | Strips HTML tags from literal values. |

### Query Translation

The Rust engine generates SPARQL from the SHACL shape and query parameters:

1. **Base pattern** — `SELECT ?s WHERE { ?s <predicate> ?o . }` for each required/flag property (instance conformance).
2. **Parent scope** — `<parentId> <predicate> ?s .` constraining the instance set.
3. **Where filters** — Equality and comparison operators pushed into SPARQL FILTER clauses and triple patterns.
4. **Order** — `ORDER BY` on literal values via `fn::parse_literal()`.
5. **Limit/Offset** — Direct SPARQL `LIMIT`/`OFFSET`.
6. **COUNT fast path** — When only a count is needed, generates `SELECT (COUNT(DISTINCT ?s) AS ?count)` avoiding full hydration.

### Include (Eager Loading)

Relations are resolved recursively in Rust with a maximum depth of 8 (`MAX_INCLUDE_DEPTH`). The engine handles:

- **Forward relations** (`@HasMany`, `@HasOne`) — follows links where the instance is the source
- **Reverse relations** (`@BelongsToOne`, `@BelongsToMany`) — follows links where the instance is the target

Results are grouped and hydrated in a single pass, avoiding N+1 query patterns.

### Server-Side Subscriptions

The `perspective.modelSubscribe` RPC operation creates a server-side subscription:

1. Rust builds trigger SPARQL from the model's SHACL shape predicates
2. On link changes matching any trigger predicate, Rust re-executes the full model query
3. Updated results are pushed to the client via the `query-subscription-update` event
4. The client receives the complete updated result set (not a delta)

```typescript
// SDK usage
const sub = await Message.query(perspective)
  .where({ channel: channelId })
  .subscribe(callback);

// Under the hood:
// 1. SDK calls perspective.modelSubscribe RPC
// 2. Server creates subscription, returns subscriptionId
// 3. On link changes, server pushes query-subscription-update events
// 4. SDK deserializes and calls callback with updated instances
```

### Getter Evaluation

Properties with `@Property({ getter })` expressions execute server-side in Rust via `evaluate_getters_batch()`. This eliminates N × round-trips that would be required if each getter were evaluated client-side:

```typescript
@Property({
  getter: "SELECT ?target WHERE { <Base> <flux://has_reply> ?target . } LIMIT 1"
})
latestReply?: string;
```

The `<Base>` placeholder is replaced with each instance's base expression URI, and all getter queries for a batch of instances are executed together.

### Query Validation

`validate_readonly_query()` ensures user-supplied SPARQL queries are read-only by parsing with Oxigraph's SPARQL parser. Only `SELECT`, `ASK`, `CONSTRUCT`, and `DESCRIBE` are accepted; `INSERT`, `DELETE`, `DROP`, etc. are rejected.

## 9.8 Transactions

`Model.transaction()` provides atomic batch operations:

```typescript
await Model.transaction(perspective, async (tx) => {
  const msg = await Message.create(tx, { body: "Hello" });
  await Reaction.create(tx, { emoji: "👍", message: msg.id });
  // All mutations are committed together
});
```

All link mutations within the transaction are batched and committed as a single `PerspectiveDiff`.

## 9.9 SHACL Shape Generation

Ad4mModel decorators automatically generate SHACL shapes. The generation process uses `buildSHACL()` from the decorator metadata:

1. **Class → NodeShape**: `@Model({ name })` creates a `sh:NodeShape` with `sh:targetClass` set to the class URI.
2. **Properties → PropertyShapes**: Each `@Property`, `@Optional`, `@Flag`, `@HasMany`, `@HasOne`, etc. generates a `sh:property` entry with appropriate constraints.
3. **Options → sh:in**: Properties with `options` generate an `sh:in` constraint listing the allowed values.
4. **Constructor Actions**: Required properties with `initial` values generate constructor actions that create links on instance creation.
5. **Destructor Actions**: Generated to clean up all property links on instance deletion.
6. **Conformance Conditions**: For typed relations, conformance filters are auto-derived from the target model's shape.

The SHACL shapes are stored as SDNA links as specified in §4.2. Implementations MUST generate conformant SHACL shapes from decorator metadata. Shapes MUST be interoperable — a model registered by one implementation MUST be readable by another.

## 9.10 JSON Schema Integration

Models MAY be created dynamically from JSON Schema definitions:

```typescript
import { fromJSONSchema } from '@coasys/ad4m';

const TaskModel = fromJSONSchema({
  title: "Task",
  type: "object",
  properties: {
    name: { type: "string", "x-ad4m": { through: "task://name", resolveLanguage: "literal" } },
    status: { type: "string", "x-ad4m": { through: "task://status", initial: "open" } },
    tags: { type: "array", items: { type: "string" }, "x-ad4m": { through: "task://tag" } }
  },
  required: ["name", "status"]
}, { name: "Task" });

await TaskModel.register(perspective);
const task = await TaskModel.create(perspective, { name: "Ship it" });
```

The `x-ad4m` extension in JSON Schema properties maps to Ad4mModel decorator options. This is useful for runtime-defined models from app manifests or configuration files.

### JSON Schema Mapping

| JSON Schema | Ad4mModel |
|-------------|-----------|
| `type: "string"` with `required` | `@Property` |
| `type: "string"` without `required` | `@Optional` |
| `type: "array"` | `@Collection` / `@HasMany` |
| `x-ad4m.through` | `through` predicate |
| `x-ad4m.resolveLanguage` | `resolveLanguage` |
| `x-ad4m.initial` | `initial` value |

## 9.11 Implementation Requirements

| Feature | Priority | Notes |
|---------|----------|-------|
| Ad4mModel base class with CRUD | **SHOULD** | Primary developer API |
| Decorator-based model declaration | **SHOULD** | `@Model`, `@Property`, `@Optional`, `@ReadOnly`, `@Flag`, `@HasMany`, `@HasOne`, `@BelongsToOne`, `@BelongsToMany`, `@Collection` |
| WeakMap-based metadata registry | **SHOULD** | Avoids prototype-mutation issues |
| Query DSL (where, order, limit, offset, parent, include) | **SHOULD** | Must translate to SPARQL |
| SPARQL query engine | **MUST** | Required for Ad4mModel query execution |
| SPARQL custom functions (`fn::parse_literal`, `fn::strip_html`) | **SHOULD** | Required for literal value filtering |
| Rust-side model query engine | **SHOULD** | Server-side execution for performance |
| Server-side model subscriptions | **SHOULD** | Push-based reactive queries |
| `sh:in` enum constraint support | **SHOULD** | Enum validation and options metadata |
| Fluent query builder | **MAY** | Developer convenience |
| Reactive queries (subscribe) | **SHOULD** | Server-side perspective.modelSubscribe subscriptions; polling/basic queries sufficient for minimal implementations |
| Transactions (batch operations) | **MAY** | Atomic multi-mutation commits |
| JSON Schema integration | **MAY** | Dynamic model creation |
| Dirty tracking and save | **SHOULD** | Efficient link mutation |
| Eager loading (include) | **SHOULD** | Avoids N+1 query patterns |
| Conformance filters for typed relations | **SHOULD** | DB-level type filtering |

Client-side Ad4mModel implementations (TypeScript/JavaScript) are the primary target. Server-side executors MUST support SPARQL queries over the link graph and SHOULD implement the `perspective.modelQuery` RPC operation to enable efficient server-side query execution.

## 9.12 PerspectiveProxy Introspection APIs

The PerspectiveProxy provides introspection methods for working with registered models and their SHACL shapes at runtime:

### listRegisteredClasses()

Returns all registered SHACL class names in the perspective:

```typescript
const classes = await perspective.listRegisteredClasses();
// ["Message", "Channel", "Todo", "Block"]
```

### getClassShape(className)

Returns property metadata for a class including `sh:in` options:

```typescript
const shape = await perspective.getClassShape("Todo");
// {
//   properties: [
//     { path: "todo://state", datatype: "xsd:string", maxCount: 1, minCount: 1, options: [...] },
//     { path: "todo://has_title", datatype: "xsd:string", maxCount: 1 }
//   ]
// }
```

### getInstanceClasses(baseExpression)

Returns which registered classes an instance conforms to:

```typescript
const classes = await perspective.getInstanceClasses("expression://abc123");
// ["Message", "Post"]  — instance matches both shapes
```

### getNamedOptions(className)

Returns `sh:in` values grouped by property for a given class:

```typescript
const options = await perspective.getNamedOptions("Todo");
// {
//   state: [
//     { value: "open", label: "Open" },
//     { value: "in-progress", label: "In Progress" },
//     { value: "done", label: "Done" }
//   ]
// }
```

### addNamedOption(className, prop, value, label?)

Dynamically adds an `sh:in` option to an existing property shape:

```typescript
await perspective.addNamedOption("Todo", "state", "blocked", "Blocked");
```

### modelQuery(className, queryJson, shapeJson?)

Executes a model query directly via the Rust engine:

```typescript
const result = await perspective.modelQuery("Todo", {
  where: { state: "open" },
  order: { createdAt: "DESC" },
  limit: 10
});
// { instances: [...], totalCount: 42 }
```

### modelSubscribe(className, queryJson, shapeJson?)

Creates a server-side model subscription:

```typescript
const sub = await perspective.modelSubscribe("Todo", {
  where: { state: "open" }
}, (result) => {
  // Called when matching links change
  console.log("Updated todos:", result.instances);
});
```
