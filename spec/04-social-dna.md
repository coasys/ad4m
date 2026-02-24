# 4. Social DNA (SDNA)

## 4.1 Overview

Social DNA (SDNA) defines data schemas over the link graph in a Perspective. It uses **Prolog rules** to declare subject classes — structured types that are reified over raw links.

SDNA enables applications to work with typed objects (e.g., "Message", "Post", "Channel") while the underlying storage is always links. The Prolog engine queries and validates the link structure.

## 4.2 SDNA Link Structure

SDNA definitions are stored as links within the Perspective itself, using special predicates:

| Source | Predicate | Target | Purpose |
|--------|-----------|--------|---------|
| `ad4m://self` | `ad4m://has_subject_class` | `literal://...` | Declares a subject class (Prolog code as literal) |
| `ad4m://self` | `ad4m://has_flow` | `literal://...` | Declares a flow |
| `ad4m://self` | `ad4m://has_custom_sdna` | `literal://...` | Custom Prolog rules |

The target is a `literal://` URI containing the Prolog source code.

An additional link type stores the SDNA code body:

| Source | Predicate | Target |
|--------|-----------|--------|
| `<class_declaration_target>` | `ad4m://sdna` | `literal://...` |

## 4.3 Subject Classes

A subject class defines a typed entity over links. It is declared in Prolog with these predicates:

### Core Predicates

```prolog
% Declare a subject class with a unique identifier atom
subject_class("ClassName", c_atom).

% Instance check: what links must exist for Base to be an instance
instance(c_atom, Base) :- 
    triple(Base, "type://predicate", _).

% Properties
property(c_atom, "propertyName").
property_getter(c_atom, Base, "propertyName", Value) :- 
    triple(Base, "some://predicate", Value).
property_setter(c_atom, "propertyName", Actions) :- 
    Actions = '[{"action":"setSingleTarget","source":"this","predicate":"some://predicate","target":"value"}]'.

% Property resolution (dereference the target as a Language expression)
property_resolve(c_atom, "propertyName").
property_resolve_language(c_atom, "propertyName", "literal").

% Collections
collection(c_atom, "collectionName").
collection_getter(c_atom, Base, "collectionName", List) :- 
    findall(T, triple(Base, "has://item", T), List).
collection_adder(c_atom, "collectionName", Actions) :- 
    Actions = '[{"action":"addLink","source":"this","predicate":"has://item","target":"value"}]'.
collection_remover(c_atom, "collectionName", Actions) :- 
    Actions = '[{"action":"removeLink","source":"this","predicate":"has://item","target":"value"}]'.
collection_setter(c_atom, "collectionName", Actions) :- 
    Actions = '[{"action":"collectionSetter","source":"this","predicate":"has://item","target":"value"}]'.

% Constructor: actions to run when creating a new instance
constructor(c_atom, Actions) :- 
    Actions = '[{"action":"addLink","source":"this","predicate":"type://predicate","target":"type://value"}]'.

% Destructor: actions to remove an instance
destructor(c_atom, Actions) :- 
    Actions = '[{"action":"removeLink","source":"this","predicate":"type://predicate","target":"*"}]'.
```

### Actions Format

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

## 4.4 Prolog Facts from Links

The Prolog engine is populated with facts derived from the Perspective's links:

```prolog
% For each link in the perspective:
triple("source_uri", "predicate_uri", "target_uri").

% Extended form with timestamp and author:
link("source_uri", "predicate_uri", "target_uri", TimestampMillis, "author_did").
```

Additional node facts provide URI decomposition:

```prolog
languageAddress("expression://addr", "language_address").
languageName("expression://addr", "language_name").
expressionAddress("expression://addr", "expression_address_part").
```

## 4.5 TypeScript Decorator API

The reference implementation provides a decorator-based TypeScript API for defining Subject Classes. The decorators generate the Prolog SDNA code.

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
  through: "recipe://name",          // Predicate URI
  resolveLanguage: "literal",        // Dereference target as Literal
  initial: "Untitled",               // Default value (for required props)
  required: true,                     // Must exist for instance check
  writable: true,                     // Generate setter (default: true)
  local: false                        // Store as shared link (default)
})
name: string = "";
```

### @Optional

Like @Property but marks the property as not required for the instance check.

### @ReadOnly

Like @Property with `writable: false`.

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

Defines a one-to-many relationship:

```typescript
@Collection({
  through: "recipe://ingredient",
  where: { isInstance: Ingredient },  // Optional: filter by subject class
  local: false
})
ingredients: string[] = [];
```

Generated methods: `addIngredients(value)`, `removeIngredients(value)`, `setCollectionIngredients(values)`.

### @InstanceQuery

Defines a static query method:

```typescript
@InstanceQuery({ where: { name: "Chocolate Cake" } })
static async findByName(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
```

## 4.6 Flows

Flows define state machines over Subject Class instances. They specify valid states and transitions using Prolog rules. Flow definitions are stored as SDNA links with predicate `ad4m://has_flow`.

> **Note:** The Flow system is implementation-defined in its specifics. Alternative implementations SHOULD support the `ad4m://has_flow` predicate but MAY defer full flow support.

## 4.7 SDNA in Neighbourhoods

When a Perspective becomes a Neighbourhood (shared), its SDNA is shared with all participants. This means:
- All agents in a Neighbourhood share the same Subject Class definitions
- Schema evolution happens by adding/modifying SDNA links in the shared Perspective
- The SDNA provides a shared understanding of the data model without requiring a central schema registry
