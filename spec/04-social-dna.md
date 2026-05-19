# 4. Social DNA (SDNA)

## 4.1 Overview

Social DNA (SDNA) defines data schemas over the link graph in a Perspective. It uses **SHACL** (Shapes Constraint Language, a W3C standard) to declare subject classes — structured types that are reified over raw links.

SDNA enables applications to work with typed objects (e.g., "Message", "Post", "Channel") while the underlying storage is always links. The SHACL shapes describe the expected graph structure, property constraints, and cardinality rules.

SHACL is the sole normative representation for subject class definitions.

## 4.2 SDNA Link Structure

SDNA definitions are stored as links within the Perspective itself, using special predicates:

### Subject Classes (SHACL)

| Source | Predicate | Target | Purpose |
|--------|-----------|--------|---------|
| `ad4m://self` | `ad4m://has_sdna` | `ad4m://sdna_<ClassName>` | Declares a subject class |
| `ad4m://sdna_<ClassName>` | `ad4m://sdna_type` | `literal:string:subject_class` | Marks it as a subject class |
| `ad4m://sdna_<ClassName>` | `sh:targetClass` | `<class_uri>` | The SHACL target class URI |
| `ad4m://sdna_<ClassName>` | `sh:property` | `ad4m://sdna_<ClassName>_prop_<N>` | Links to property shapes |

### Property Shapes

Each property shape is a node with links describing the constraint:

| Source | Predicate | Target | Purpose |
|--------|-----------|--------|---------|
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh:path` | `<predicate_uri>` | The link predicate for this property |
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh:datatype` | `xsd:string` | Data type |
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh:maxCount` | `literal:number:1` | Scalar property (single value) |
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh:minCount` | `literal:number:1` | Required property |
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh:class` | `<class_uri>` | Reference to another Subject Class |
| `ad4m://sdna_<ClassName>_prop_<N>` | `ad4m://initial` | `<value>` | Default value on creation |
| `ad4m://sdna_<ClassName>_prop_<N>` | `ad4m://resolveLanguage` | `<language_name>` | Expression language for value resolution |
| `ad4m://sdna_<ClassName>_prop_<N>` | `ad4m://writable` | `literal:boolean:true` | Property can be updated |
| `ad4m://sdna_<ClassName>_prop_<N>` | `sh://in` | `literal:string:[...]` | Allowed values (enum constraint) |

### Cardinality Rules

| Constraint | Meaning |
|-----------|---------|
| `sh:maxCount 1` present | Scalar property (single value) |
| No `sh:maxCount` | Collection (multiple values) |
| `sh:minCount 1` present | Required property (must exist for instance check) |
| No `sh:minCount` | Optional property |

### Flows

| Source | Predicate | Target | Purpose |
|--------|-----------|--------|---------|
| `ad4m://self` | `ad4m://has_flow` | `literal:...` | Declares a flow definition |

### Custom SDNA

| Source | Predicate | Target | Purpose |
|--------|-----------|--------|---------|
| `ad4m://self` | `ad4m://has_sdna` | `literal:...` | Declares a subject class |
| `ad4m://self` | `ad4m://has_custom_sdna` | `literal:...` | Custom SHACL rules |

## 4.3 Subject Classes

A subject class defines a typed entity over links. It is declared using SHACL shapes stored as links in the perspective.

### SHACL Representation

Here is how a Todo class maps to SHACL:

**TypeScript Model:**
```typescript
@ModelOptions({ name: "Todo" })
class Todo extends Ad4mModel {
  @Property({
    through: "todo://state",
    initial: "todo://ready"
  })
  state: string = "";

  @Optional({
    through: "todo://has_title",
    writable: true,
    resolveLanguage: "literal"
  })
  title?: string;

  @Collection({
    through: "todo://comment"
  })
  comments: string[] = [];
}
```

**SHACL (Turtle notation):**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ad4m: <ad4m://> .

<todo://Todo> a sh:NodeShape ;
  sh:targetClass <todo://Todo> ;

  # Scalar property: state (required, has initial value)
  sh:property [
    sh:path <todo://state> ;
    sh:datatype xsd:string ;
    sh:maxCount 1 ;
    sh:minCount 1 ;
    ad4m:initial "todo://ready" ;
  ] ;

  # Scalar property: title (optional, resolved via "literal" language)
  sh:property [
    sh:path <todo://has_title> ;
    sh:datatype xsd:string ;
    sh:maxCount 1 ;
    ad4m:resolveLanguage "literal" ;
    ad4m:writable true ;
  ] ;

  # Collection: comments (unbounded)
  sh:property [
    sh:path <todo://comment> ;
    sh:datatype xsd:string ;
  ] .
```

**Stored as Links:**
```text
(ad4m://self) --ad4m://has_sdna--> (ad4m://sdna_Todo)
(ad4m://sdna_Todo) --ad4m://sdna_type--> (literal:string:subject_class)
(ad4m://sdna_Todo) --sh:targetClass--> (todo://Todo)
(ad4m://sdna_Todo) --sh:property--> (ad4m://sdna_Todo_prop_0)

# Property shape for "state":
(ad4m://sdna_Todo_prop_0) --sh:path--> (todo://state)
(ad4m://sdna_Todo_prop_0) --sh:datatype--> (xsd:string)
(ad4m://sdna_Todo_prop_0) --sh:maxCount--> (literal:number:1)
(ad4m://sdna_Todo_prop_0) --sh:minCount--> (literal:number:1)
(ad4m://sdna_Todo_prop_0) --ad4m://initial--> (todo://ready)

# ... and so on for each property
```

### AD4M SHACL Extensions

AD4M extends standard SHACL with custom predicates under the `ad4m://` namespace:

| Predicate | Purpose | Example |
|-----------|---------|---------|
| `ad4m://initial` | Default value set on instance creation | `"todo://ready"` |
| `ad4m://resolveLanguage` | Expression language used to dereference property values | `"literal"` |
| `ad4m://writable` | Whether the property supports updates | `true` / `false` |
| `ad4m://sdna_type` | Discriminates SDNA node type | `"subject_class"` |
| `ad4m://has_sdna` | Links self to an SDNA definition | `ad4m://sdna_Todo` |
| `sh://in` | Allowed values for the property (enum constraint) | `[{"value":"open","label":"Open"},...]` |

### Instance Resolution

An expression is considered an instance of a Subject Class if it has links matching all **required** properties (`sh:minCount >= 1`) of that class. The runtime:

1. Loads all SHACL shapes from `ad4m://has_sdna` links
2. For a candidate expression, checks if links exist matching each required property shape's `sh:path`
3. Multiple Subject Classes can match the same expression (subject-oriented)

### Subject-Oriented Pattern Recognition

Different applications can define different Subject Classes that interpret the same base expression differently:

- A chat app sees a "Message" with replies and reactions
- A task app sees a "Todo" with state and assignments
- A social app sees a "Post" with likes and shares

Each interpretation is equally valid — they are subjective lenses on the same graph structure. This is inspired by subject-oriented programming.

## 4.4 Actions Format

Property setters, collection operations, and constructors return JSON-encoded action arrays:

```typescript
interface PerspectiveAction {
  action: "addLink" | "removeLink" | "setSingleTarget" | "collectionSetter";
  source: string;     // "this" = the instance base URI
  predicate: string;
  target: string;     // "value" = the value being set, "*" = wildcard
  local?: boolean;    // If true, create as local-only link
}
```

- `addLink` — Add a new link
- `removeLink` — Remove matching links
- `setSingleTarget` — Remove existing link with this source+predicate, add new one
- `collectionSetter` — Replace all links with this source+predicate with new values

## 4.5 TypeScript Decorator API

The reference implementation provides a decorator-based TypeScript API for defining Subject Classes. The decorators generate SHACL shapes automatically.

### @ModelOptions

Registers a class as a Subject Class:

```typescript
@ModelOptions({ name: "Recipe" })
class Recipe extends Ad4mModel {
  // ...
}
```

### @Property

Defines a required writable property:

```typescript
@Property({
  through: "recipe://name",          // Predicate URI (becomes sh:path)
  resolveLanguage: "literal",        // Dereference target as Literal
  initial: "Untitled",               // Default value (ad4m://initial)
  required: true,                     // sh:minCount 1
  writable: true,                     // ad4m://writable (default: true)
  local: false                        // Store as shared link (default)
})
name: string = "";
```

### @Optional

Like @Property but without `sh:minCount` — the property is not required for the instance check.

### @ReadOnly

Like @Property with `ad4m://writable` set to false.

### @Flag

A required immutable property with a fixed value — used for type discrimination:

```typescript
@Flag({
  through: "ad4m://type",
  value: "ad4m://message"
})
type: string = "";
```

### @Collection

Defines a one-to-many relationship (no `sh:maxCount` constraint):

```typescript
@Collection({
  through: "recipe://ingredient",
  where: { isInstance: Ingredient },  // Optional: filter by subject class
  local: false
})
ingredients: string[] = [];
```

Generated methods: `addIngredients(value)`, `removeIngredients(value)`, `setCollectionIngredients(values)`.

### @Property with `options` (Enum Constraint)

The `options` parameter generates an `sh:in` constraint, restricting the property to a set of allowed values:

```typescript
@Property({
  through: "task://status",
  options: [
    { value: "open", label: "Open" },
    { value: "in-progress", label: "In Progress" },
    { value: "done", label: "Done" }
  ]
})
status: string = "";
```

The `options` array is stored as an `sh:in` link on the property shape node. Each option has a `value` (the stored link target) and an optional `label` (human-readable display name). Applications can use `getNamedOptions()` to retrieve the allowed values for UI rendering (e.g., dropdown menus, select inputs).

### @InstanceQuery

Defines a static query method:

```typescript
@InstanceQuery({ where: { name: "Chocolate Cake" } })
static async findByName(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
```

### SHACL Direct API

For advanced use cases, SHACL shapes can be created programmatically:

```typescript
import { SHACLShape } from '@coasys/ad4m';

const shape = new SHACLShape('recipe://Recipe');
shape.addProperty({
  path: 'recipe://name',
  datatype: 'xsd:string',
  maxCount: 1,
  minCount: 1,
});
shape.addProperty({
  path: 'recipe://ingredient',
  datatype: 'xsd:string',
  // No maxCount = collection
});

await perspective.addShacl('Recipe', shape);
```

## 4.6 Query Engines

### SPARQL

The query engine is an in-process **Oxigraph 0.5.7** SPARQL 1.1 engine with disk persistence. Each AD4M link is stored using the RDF 1.2 reifier model in the default graph (see [§1.9](./01-core-data-model.md#19-link-storage-model-rdf-12-reifiers)).

SPARQL is used for:
- Ad4mModel query execution (all `findAll`, `findOne`, `count`, etc. translate to SPARQL)
- Direct queries via `perspective.querySparql` RPC operation
- Custom queries from applications
- Server-side model subscriptions (trigger matching and result re-computation)

Custom SPARQL functions:
- `<ad4m://fn/parse_literal>(term)` — Decodes `literal:string:...` URIs into plain string literals
- `<ad4m://fn/strip_html>(term)` — Strips HTML tags from literal values

Query validation ensures only read-only queries (SELECT/ASK/CONSTRUCT/DESCRIBE) are accepted from user-facing APIs.

```sparql
# Example: Find all todos in "done" state
SELECT ?todo WHERE {
  ?todo <todo://state> <todo://done> .
}

# Example: Find posts by author with literal content
SELECT ?post ?content WHERE {
  ?post <post://has_content> ?raw .
  BIND(<ad4m://fn/parse_literal>(?raw) AS ?content)
}

# Example: Query with metadata via reifier
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?todo ?author WHERE {
  ?todo <todo://state> <todo://done> .
  ?reifier rdf:reifies <<( ?todo <todo://state> <todo://done> )>> .
  ?reifier <ad4m://ontology/author> ?author .
}
```

## 4.7 Flows

Flows define state machines over Subject Class instances. They specify valid states and transitions, managing link-based state tracking internally.

```typescript
const todoFlow = {
  name: "TODO",
  states: { ready: 0, doing: 0.5, done: 1 },
  stateQuery: (base) => `todo://state`,
  transitions: [
    {
      from: "ready", to: "doing", action: "Start",
      effects: [
        { action: "addLink", source: "this", predicate: "todo://state", target: "todo://doing" },
        { action: "removeLink", source: "this", predicate: "todo://state", target: "todo://ready" }
      ]
    }
  ]
};
```

> **Note:** The Flow system is implementation-defined in its specifics. Alternative implementations SHOULD support the `ad4m://has_flow` predicate but MAY defer full flow support.

## 4.8 SDNA in Neighbourhoods

When a Perspective becomes a Neighbourhood (shared), its SDNA is shared with all participants:

- All agents share the same Subject Class definitions (SHACL shapes)
- Schema evolution happens by adding/modifying SDNA links in the shared Perspective
- The SDNA provides a shared understanding of the data model without a central schema registry
- SHACL shapes are synced like any other links via the LinkLanguage

## 4.9 Implementation Requirements

For alternative implementations:

| Feature | Priority | Notes |
|---------|----------|-------|
| SHACL shape storage/retrieval | **MUST** | Normative representation |
| Subject class instance resolution | **MUST** | Match expressions against SHACL shapes |
| Property get/set via link operations | **MUST** | Core CRUD operations |
| Collection operations | **MUST** | Add/remove/set for multi-value properties |
| SPARQL query support | **MUST** | Primary query engine (Oxigraph or equivalent) |
| Prolog inference | **MAY** | Not required for new implementations |
| SHACL custom rules | **MAY** | For advanced constraint checking and reasoning |
| Flow support | **MAY** | State machine functionality |
| `Ad4mModel` / decorator API | **MAY** | Client-side convenience; not required in the executor |
