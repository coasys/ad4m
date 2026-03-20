# 10. Ad4mModel — Application Data Model API

## 10.1 Overview

Ad4mModel is the **primary developer-facing API** for building applications on AD4M. It provides a decorator-based, ORM-like abstraction over the link graph, SHACL shapes, and SPARQL queries.

Application developers SHOULD use Ad4mModel for all data operations. Developers SHOULD NOT interact directly with links, SHACL shapes, SPARQL, or Holochain primitives unless building framework-level tooling.

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

The `name` parameter MUST be unique within the perspective and becomes the SHACL target class identifier.

### @Property

Declares a scalar property mapped to a link predicate:

```typescript
@Property({
  through: "flux://has_body",       // Link predicate URI (REQUIRED)
  resolveLanguage: "literal",       // Expression language for value resolution
  writable: true,                   // Whether updates are allowed (default: true)
  required: true,                   // Maps to sh:minCount 1
  initial: "Hello"                  // Default value on creation (ad4m://initial)
})
body: string = "";
```

- `through` — The predicate URI used in the link triple `(instance, through, value)`. REQUIRED.
- `resolveLanguage` — If set, the target expression is dereferenced through the named Language.
- `writable` — Maps to `ad4m://writable`. Default `true`.
- `required` — If `true`, generates `sh:minCount 1`. Instances without this property will not match the Subject Class shape.
- `initial` — Default value set during instance creation via constructor actions.

### @Optional

Equivalent to `@Property` with `required: false`. The property is not required for shape conformance:

```typescript
@Optional({
  through: "flux://has_description",
  writable: true,
  resolveLanguage: "literal"
})
description?: string;
```

### @Flag

A boolean flag represented by link existence. If the link exists, the flag is `true`; if absent, `false`:

```typescript
@Flag({
  through: "ad4m://type",
  value: "flux://message"
})
type: string = "";
```

Flag properties are required and immutable — they serve as type discriminators for shape conformance.

### @HasMany

Declares a one-to-many forward relation. The instance is the link source:

```typescript
@HasMany({
  through: "flux://has_reaction"
})
reactions: string[] = [];
```

Generates collection methods: `addReactions(value)`, `removeReactions(value)`, `setCollectionReactions(values)`.

### @HasOne

Declares a one-to-one forward relation (`sh:maxCount 1`):

```typescript
@HasOne({
  through: "flux://has_author",
  target: () => Profile
})
author?: Profile;
```

### @BelongsToOne

Declares an inverse one-to-one relation. The instance is the link *target*, not the source:

```typescript
@BelongsToOne({
  through: "flux://has_channel"
})
channel?: Channel;
```

### @BelongsToMany

Declares an inverse one-to-many relation:

```typescript
@BelongsToMany({
  through: "flux://has_member"
})
members: string[] = [];
```

### @Collection

Alias for `@HasMany`. Used by WE-compatible applications:

```typescript
@Collection({
  through: "flux://has_block"
})
blocks: string[] = [];
```

## 10.3 Complete Example — WE-Compatible Models

The following example shows models from a WE-compatible application (Flux):

```typescript
@Model({ name: "Space" })
class Space extends Ad4mModel {
  @Property({
    through: "flux://has_name",
    resolveLanguage: "literal",
    writable: true,
    required: true
  })
  name: string = "";

  @Optional({
    through: "flux://has_description",
    resolveLanguage: "literal",
    writable: true
  })
  description?: string;

  @Collection({
    through: "flux://has_channel"
  })
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
    writable: true,
    required: true
  })
  content: string = "";

  @Optional({
    through: "flux://has_block_type",
    writable: true
  })
  blockType?: string;
}
```

## 10.4 Model Lifecycle

### Registration

Before use, a model's SHACL shape MUST be registered in the perspective:

```typescript
await Message.register(perspective);
```

This generates the SHACL shape from the decorator metadata and stores it as SDNA links (see §4.2). Implementations SHOULD cache registration and skip if the shape already exists.

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

An optional `batchId` parameter MAY be provided to group link mutations into a single sync batch.

### Reading (Hydration)

Model instances are hydrated from links. Given a base expression URI, the runtime:
1. Queries all links where the URI is the source.
2. Maps link predicates to property/relation metadata from decorators.
3. Populates the model instance fields from link targets.

### Updating

```typescript
msg.body = "Updated content";
await msg.save();
```

The model tracks dirty fields. `save()` persists only modified fields as link mutations (`setSingleTarget` for scalar properties, collection operations for relations).

Bulk updates:

```typescript
await msg.update({ body: "New content", description: "Added desc" });
```

### Deletion

```typescript
await msg.delete();
```

Executes SHACL destructor actions, removing all links associated with the instance.

## 10.5 Querying

### Static Query Methods

| Method | Description |
|--------|-------------|
| `Model.findAll(perspective, query?)` | Returns all instances matching the query |
| `Model.findOne(perspective, query?)` | Returns the first matching instance, or `null` |
| `Model.findById(perspective, id)` | Returns the instance with the given base expression URI |
| `Model.count(perspective, query?)` | Returns the count of matching instances |
| `Model.paginate(perspective, pageSize, page, query?)` | Returns a `PaginationResult<T>` |

### Query DSL

The `query` parameter accepts a `Query` object:

```typescript
interface Query {
  where?: Where;
  order?: OrderBy;
  limit?: number;
  offset?: number;
  parent?: ParentQuery;
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
    createdAt: { op: "gt", value: "2024-01-01" }
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
const channelMessages = await Message.findAll(perspective, {
  parent: { id: channelUri, predicate: "flux://has_message" }
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
  .execute();
```

## 10.6 Query Engine — SPARQL

Ad4mModel generates **SPARQL** queries from the Query DSL. The underlying GraphQL field is `perspectiveQuerySPARQL`.

Implementations MUST support SPARQL as the query engine for Ad4mModel operations. The SPARQL queries operate over the link graph, where each link is a triple `(source, predicate, target)`.

Example generated SPARQL for `Todo.findAll(p, { where: { state: "done" } })`:

```sparql
SELECT DISTINCT ?subject WHERE {
  ?subject <todo://state> ?state .
  FILTER(?state = "done")
}
```

### Raw SPARQL Access

For advanced queries beyond the Query DSL, raw SPARQL MAY be executed:

```typescript
const results = await perspective.querySPARQL(`
  SELECT ?msg ?body WHERE {
    ?channel <flux://has_message> ?msg .
    ?msg <flux://has_body> ?body .
    FILTER(?channel = <${channelUri}>)
  }
  ORDER BY DESC(?body)
  LIMIT 50
`);
```

### Deprecated Query Engines

- `perspectiveQuerySurrealDB` — **DEPRECATED**. Implementations MAY translate SurrealDB queries to SPARQL internally for backward compatibility, but new applications MUST NOT depend on SurrealDB support.
- `perspectiveInfer` (Prolog) — **DEPRECATED**. Legacy Prolog inference is not required in new implementations.

## 10.7 SHACL Shape Generation

Ad4mModel decorators automatically generate SHACL shapes. The generation process:

1. **Class → NodeShape**: `@Model({ name })` creates a `sh:NodeShape` with `sh:targetClass` set to the class URI.
2. **Properties → PropertyShapes**: Each `@Property`, `@Optional`, `@Flag`, `@HasMany`, `@HasOne`, etc. generates a `sh:property` entry with appropriate constraints.
3. **Constructor Actions**: Required properties with `initial` values generate constructor actions that create links on instance creation.
4. **Destructor Actions**: Generated to clean up all property links on instance deletion.

The SHACL shapes are stored as SDNA links as specified in §4.2. See §4.3 for the full SHACL mapping.

Implementations MUST generate conformant SHACL shapes from decorator metadata. The shapes MUST be interoperable — a model registered by one implementation MUST be readable by another.

## 10.8 JSON Schema Integration

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

The `x-ad4m` extension in JSON Schema properties maps to Ad4mModel decorator options. This is useful for runtime-defined models (e.g., from app manifests or configuration files).

### JSON Schema Mapping

| JSON Schema | Ad4mModel |
|-------------|-----------|
| `type: "string"` with `required` | `@Property` |
| `type: "string"` without `required` | `@Optional` |
| `type: "array"` | `@Collection` / `@HasMany` |
| `x-ad4m.through` | `through` predicate |
| `x-ad4m.resolveLanguage` | `resolveLanguage` |
| `x-ad4m.initial` | `initial` value |

## 10.9 Implementation Requirements

| Feature | Priority | Notes |
|---------|----------|-------|
| Ad4mModel base class with CRUD | **SHOULD** | Primary developer API |
| Decorator-based model declaration | **SHOULD** | `@Model`, `@Property`, `@Optional`, `@Flag`, `@HasMany`, `@HasOne`, `@BelongsToOne`, `@BelongsToMany`, `@Collection` |
| Query DSL (where, order, limit, offset, parent) | **SHOULD** | Must translate to SPARQL |
| SPARQL query generation | **MUST** | Required query engine |
| Fluent query builder | **MAY** | Developer convenience |
| JSON Schema integration | **MAY** | Dynamic model creation |
| Dirty tracking and save | **SHOULD** | Efficient link mutation |
| Batch operations (batchId) | **MAY** | Group mutations for sync efficiency |

Client-side Ad4mModel implementations (TypeScript/JavaScript) are the primary target. Server-side executors MUST support `perspectiveQuerySPARQL` to enable Ad4mModel query execution.
