# 10. Ad4mModel — Application Data Model API

## 10.1 Overview

Ad4mModel is the **primary developer-facing API** for building applications on AD4M. It provides a decorator-based, ORM-like abstraction over the link graph, SHACL shapes, and SPARQL queries.

Application developers SHOULD use Ad4mModel for all data operations. Developers SHOULD NOT interact directly with links, SHACL shapes, or Holochain primitives unless building framework-level tooling.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

## 10.2 Model Declaration

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
})
body: string = "";
```

Smart defaults:
- `required` → `false` (properties are optional by default)
- `readOnly` → `false`
- `resolveLanguage` → `"literal"`
- `initial` → `undefined` (no link created until a value is explicitly set); when `required: true` and no `initial` is provided, defaults to `"literal:string:uninitialized"` as a sentinel.

- `through` — The predicate URI used in the link triple `(instance, through, value)`. REQUIRED unless `getter` or `prologGetter` is provided.
- `resolveLanguage` — If set, the target expression is dereferenced through the named Language. Default: `"literal"`.
- `required` — If `true`, generates `sh:minCount 1` and includes a sentinel initial value. Instances without this property will not match the Subject Class shape.
- `initial` — Default value set during instance creation via constructor actions.
- `readOnly` — If `true`, no setter method is generated. Maps to `ad4m://writable false`.
- `getter` — Custom graph traversal expression. The expression can reference `Base` which is replaced with the instance's base expression. Example: `"SELECT ?target WHERE { <Base> <flux://has_reply> ?target . } LIMIT 1"`.

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

## 10.3 Decorator Metadata Registry

Decorator metadata is stored using **WeakMaps** keyed by class constructor, replacing the old prototype-mutation approach. This avoids issues with inheritance chains sharing mutable state.

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

## 10.4 Complete Example

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

## 10.5 Model Lifecycle

### Registration

Before use, a model's SHACL shape MUST be registered in the perspective:

```typescript
await Message.register(perspective);
```

This generates the SHACL shape from decorator metadata and stores it as SDNA links (see §4.2). Implementations SHOULD cache registration and skip if the shape already exists.

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

## 10.6 Querying

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

Supported operators: `gt`, `lt`, `gte`, `lte`, `between`, `contains`, `not`.

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

`Model.query().subscribe()` returns a reactive subscription that re-runs when the perspective changes:

```typescript
const subscription = Message.query(perspective)
  .where({ channel: channelId })
  .subscribe((messages) => {
    // Called whenever the result set changes
    console.log("Updated messages:", messages);
  });

// Later: unsubscribe
subscription.unsubscribe();
```

## 10.7 Query Engine — SPARQL

Ad4mModel translates the Query DSL into **SPARQL 1.1** queries against the Oxigraph triple store. This replaces the earlier SurrealDB-based query path.

### Storage Model

Each AD4M link is stored as:
- A direct triple `(source, predicate, target)` in a **named graph** (keyed by SHA-256 hash of the link).
- Metadata triples (author, timestamp, proof, status) in the **default graph**, keyed by the named graph IRI.

Ontology URIs for metadata:
- `ad4m://ontology/author`
- `ad4m://ontology/timestamp`
- `ad4m://ontology/proofKey`
- `ad4m://ontology/proofSignature`
- `ad4m://ontology/proofValid`
- `ad4m://ontology/status`

### Query Translation

`buildSPARQLQuery()` translates Query DSL clauses to SPARQL:

1. **Base pattern** — `SELECT ?s WHERE { GRAPH ?g { ?s <predicate> ?o } }` for each required/flag property.
2. **Parent scope** — `GRAPH ?gp { <parentId> <predicate> ?s }` constraining the instance set.
3. **Where filters** — Equality on link targets pushed down to SPARQL triple patterns; literal-stored values use the custom `fn::parse_literal()` function. Comparison operators (`gt`, `lt`, `gte`, `lte`, `between`, `contains`) on literal properties are evaluated in JavaScript post-hydration.
4. **Order** — `ORDER BY` on literal values via `fn::parse_literal()`.
5. **Limit/Offset** — Direct SPARQL `LIMIT`/`OFFSET` when no JS-only filters exist.

### Custom SPARQL Functions

The Oxigraph store registers two custom functions:

| Function | Description |
|----------|-------------|
| `fn::parse_literal(term)` | Decodes `literal:string:...` URIs into plain string literals. Handles `string:`, `number:`, `boolean:`, and `json:` types. For JSON literals containing a `data` field (signed expressions), extracts just the data value. |
| `fn::strip_html(term)` | Strips HTML tags from literal values. |

### Query Validation

`validate_readonly_query()` ensures user-supplied SPARQL queries are read-only by parsing with Oxigraph's SPARQL parser. Only `SELECT`, `ASK`, `CONSTRUCT`, and `DESCRIBE` are accepted; `INSERT`, `DELETE`, `DROP`, etc. are rejected.

### Batch Queries

`buildBatchSPARQLQuery()` handles eager-loaded relations by constructing batch SPARQL queries that resolve N+1 patterns into single queries. Results are grouped and hydrated via `hydrateBatchResult()`.

### Prolog Fallback

Prolog remains available as a query engine parameter for backward compatibility. It is used primarily for SHACL inference and subject-class resolution (see §4). The Query DSL also supports Prolog-based query building via `buildParentQuery()`, `buildWhereQuery()`, etc.

## 10.8 Transactions

`Model.transaction()` provides atomic batch operations:

```typescript
await Model.transaction(perspective, async (tx) => {
  const msg = await Message.create(tx, { body: "Hello" });
  await Reaction.create(tx, { emoji: "👍", message: msg.id });
  // All mutations are committed together
});
```

All link mutations within the transaction are batched and committed as a single `PerspectiveDiff`.

## 10.9 SHACL Shape Generation

Ad4mModel decorators automatically generate SHACL shapes. The generation process uses `buildSHACL()` from the decorator metadata:

1. **Class → NodeShape**: `@Model({ name })` creates a `sh:NodeShape` with `sh:targetClass` set to the class URI.
2. **Properties → PropertyShapes**: Each `@Property`, `@Optional`, `@Flag`, `@HasMany`, `@HasOne`, etc. generates a `sh:property` entry with appropriate constraints.
3. **Constructor Actions**: Required properties with `initial` values generate constructor actions that create links on instance creation.
4. **Destructor Actions**: Generated to clean up all property links on instance deletion.
5. **Conformance Conditions**: For typed relations, conformance filters are auto-derived from the target model's shape.

The SHACL shapes are stored as SDNA links as specified in §4.2. Implementations MUST generate conformant SHACL shapes from decorator metadata. Shapes MUST be interoperable — a model registered by one implementation MUST be readable by another.

## 10.10 JSON Schema Integration

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

## 10.11 Implementation Requirements

| Feature | Priority | Notes |
|---------|----------|-------|
| Ad4mModel base class with CRUD | **SHOULD** | Primary developer API |
| Decorator-based model declaration | **SHOULD** | `@Model`, `@Property`, `@Optional`, `@ReadOnly`, `@Flag`, `@HasMany`, `@HasOne`, `@BelongsToOne`, `@BelongsToMany`, `@Collection` |
| WeakMap-based metadata registry | **SHOULD** | Replaces prototype-mutation approach |
| Query DSL (where, order, limit, offset, parent, include) | **SHOULD** | Must translate to SPARQL |
| SPARQL query engine | **MUST** | Required for Ad4mModel query execution |
| SPARQL custom functions (`fn::parse_literal`, `fn::strip_html`) | **SHOULD** | Required for literal value filtering |
| Fluent query builder | **MAY** | Developer convenience |
| Reactive queries (subscribe) | **MAY** | Delta-based subscription updates |
| Transactions (batch operations) | **MAY** | Atomic multi-mutation commits |
| JSON Schema integration | **MAY** | Dynamic model creation |
| Dirty tracking and save | **SHOULD** | Efficient link mutation |
| Eager loading (include) | **SHOULD** | Avoids N+1 query patterns |
| Conformance filters for typed relations | **SHOULD** | DB-level type filtering |
| Prolog query fallback | **MAY** | Backward compatibility |

Client-side Ad4mModel implementations (TypeScript/JavaScript) are the primary target. Server-side executors MUST support SPARQL queries over the link graph to enable Ad4mModel query execution.
