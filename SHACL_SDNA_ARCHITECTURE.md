# SHACL SDNA Architecture
**Replacing Prolog-based Subject DNA with W3C SHACL + AD4M Extensions**

## Overview

AD4M uses Subject DNA (SDNA) to define data schemas and their operational behavior. This document describes the migration from Prolog-based SDNA to a SHACL-based approach that:

1. **W3C Conformant** - Schema definitions use standard SHACL predicates
2. **Extensible** - AD4M-specific actions use a separate `ad4m://` namespace
3. **Queryable** - All metadata stored as links, queryable via SurrealQL or simple link queries
4. **Consistent** - Same pattern for Classes and Flows

---

## Architecture: Separate Links Approach

### Design Principles

1. **One predicate = one action type** - Clear semantics
2. **SHACL for schema, AD4M for behavior** - Clean separation
3. **Named URIs** - Property shapes have queryable URIs (not blank nodes)
4. **JSON in literals** - Actions stored as JSON arrays in `literal://string:` format

### Link Structure

For a class named `Recipe` with namespace `recipe://`:

| Link Type | Source | Predicate | Target |
|-----------|--------|-----------|--------|
| Shape Type | `recipe://RecipeShape` | `rdf://type` | `sh://NodeShape` |
| Target Class | `recipe://RecipeShape` | `sh://targetClass` | `recipe://Recipe` |
| Has Property | `recipe://RecipeShape` | `sh://property` | `recipe://Recipe.name` |
| Property Type | `recipe://Recipe.name` | `rdf://type` | `sh://PropertyShape` |
| Property Path | `recipe://Recipe.name` | `sh://path` | `recipe://name` |
| Datatype | `recipe://Recipe.name` | `sh://datatype` | `xsd://string` |
| Constructor | `recipe://RecipeShape` | `ad4m://constructor` | `literal://string:[...]` |
| Destructor | `recipe://RecipeShape` | `ad4m://destructor` | `literal://string:[...]` |
| Setter | `recipe://Recipe.name` | `ad4m://setter` | `literal://string:[...]` |
| Adder | `recipe://Recipe.items` | `ad4m://adder` | `literal://string:[...]` |
| Remover | `recipe://Recipe.items` | `ad4m://remover` | `literal://string:[...]` |

---

## Example: Complete Recipe Class

### TypeScript Definition

```typescript
@ModelOptions({
  name: "Recipe",
  namespace: "recipe://"
})
class Recipe {
  @SubjectProperty({ through: "recipe://name", writable: true })
  name: string = "";

  @SubjectProperty({ through: "recipe://rating", writable: true })
  rating: number = 0;

  @SubjectCollection({ through: "recipe://has_ingredient" })
  ingredients: string[] = [];
}
```

### W3C SHACL Links (Schema)

```turtle
# Shape definition
recipe://RecipeShape rdf:type sh:NodeShape .
recipe://RecipeShape sh:targetClass recipe://Recipe .

# Property: name
recipe://Recipe.name rdf:type sh:PropertyShape .
recipe://Recipe.name sh:path recipe://name .
recipe://Recipe.name sh:datatype xsd:string .
recipe://Recipe.name sh:maxCount 1 .
recipe://RecipeShape sh:property recipe://Recipe.name .

# Property: rating
recipe://Recipe.rating rdf:type sh:PropertyShape .
recipe://Recipe.rating sh:path recipe://rating .
recipe://Recipe.rating sh:datatype xsd:integer .
recipe://Recipe.rating sh:maxCount 1 .
recipe://RecipeShape sh:property recipe://Recipe.rating .

# Collection: ingredients
recipe://Recipe.ingredients rdf:type ad4m:CollectionShape .
recipe://Recipe.ingredients sh:path recipe://has_ingredient .
recipe://Recipe.ingredients sh:nodeKind sh:IRI .
recipe://RecipeShape sh:property recipe://Recipe.ingredients .
```

### AD4M Action Links (Behavior)

```turtle
# Constructor - creates instance with default values
recipe://RecipeShape ad4m://constructor "literal://string:[
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"\"},
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"0\"}
]" .

# Destructor - removes instance links
recipe://RecipeShape ad4m://destructor "literal://string:[
  {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"recipe://name\"},
  {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"recipe://rating\"}
]" .

# Property setters
recipe://Recipe.name ad4m://setter "literal://string:[
  {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"value\"}
]" .

recipe://Recipe.rating ad4m://setter "literal://string:[
  {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"value\"}
]" .

# Collection operations
recipe://Recipe.ingredients ad4m://adder "literal://string:[
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
]" .

recipe://Recipe.ingredients ad4m://remover "literal://string:[
  {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
]" .
```

---

## Action Types

### Shape-Level Actions

| Predicate | Purpose | Bound To |
|-----------|---------|----------|
| `ad4m://constructor` | Create instance with defaults | `{namespace}{ClassName}Shape` |
| `ad4m://destructor` | Remove instance and links | `{namespace}{ClassName}Shape` |

### Property-Level Actions

| Predicate | Purpose | Bound To |
|-----------|---------|----------|
| `ad4m://setter` | Set single-valued property | `{namespace}{ClassName}.{propertyName}` |
| `ad4m://adder` | Add to collection | `{namespace}{ClassName}.{collectionName}` |
| `ad4m://remover` | Remove from collection | `{namespace}{ClassName}.{collectionName}` |

### Action JSON Format

```json
[
  {
    "action": "addLink|removeLink|setSingleTarget|collectionSetter",
    "source": "this|uuid|literal",
    "predicate": "namespace://predicate",
    "target": "value|*|specific_value",
    "local": true  // optional
  }
]
```

---

## Querying SHACL Links

### Get Constructor Actions

```rust
// Find shape with constructor
let links = self.get_links(&LinkQuery {
    predicate: Some("ad4m://constructor".to_string()),
    ..Default::default()
}).await?;

// Find one matching class name
for link in links {
    if link.data.source.ends_with(&format!("{}Shape", class_name)) {
        // Parse JSON from literal://string:{json}
        let actions = parse_literal_json(&link.data.target)?;
        return Ok(actions);
    }
}
```

### Get Property Setter

```rust
// Find property shape with setter
let prop_suffix = format!("{}.{}", class_name, property_name);
let links = self.get_links(&LinkQuery {
    predicate: Some("ad4m://setter".to_string()),
    ..Default::default()
}).await?;

for link in links {
    if link.data.source.ends_with(&prop_suffix) {
        let actions = parse_literal_json(&link.data.target)?;
        return Ok(actions);
    }
}
```

---

## Implementation Status

### Phase 1: SHACL Infrastructure (Complete)

- [x] TypeScript: Generate SHACL JSON with actions in `generateSDNA()`
- [x] TypeScript: Serialize SHACL to links in `PerspectiveProxy.ensureSdnaLinks()`
- [x] Rust: Parse SHACL JSON in `shacl_parser.rs`
- [x] Rust: Store action links with separate predicates

### Phase 2: Replace Prolog Queries (In Progress)

- [x] `get_constructor_actions()` - Try SHACL first, fallback to Prolog
- [x] `get_destructor_actions()` - Try SHACL first, fallback to Prolog
- [x] `get_property_setter_actions()` - Try SHACL first, fallback to Prolog
- [x] `get_collection_adder_actions()` - Try SHACL first, fallback to Prolog
- [x] `get_collection_remover_actions()` - Try SHACL first, fallback to Prolog
- [x] `resolve_property_value()` - Try SHACL for resolve language
- [x] TypeScript `removeSubject()` - Try SHACL for destructor actions

### Phase 3: Remove Prolog Fallbacks (This PR)

> **Note:** Prolog engines (scryer-prolog) are kept for complex queries and future
> advanced features. Only the _fallback pattern_ is removed - SHACL becomes the
> primary source for SDNA actions.

- [x] Remove Prolog fallbacks for action retrieval (SHACL-first is now SHACL-only)
- [ ] Migrate Flows to same SHACL link pattern
- [x] Keep scryer-prolog dependency (for complex Prolog queries later)

---

## Benefits

1. **W3C Standard** - Interoperable with SHACL ecosystem
2. **Cleaner Runtime** - SHACL as single source for SDNA actions (Prolog still available for complex queries)
3. **Queryable** - All metadata as links in SurrealDB
4. **Debuggable** - Inspect schema as regular links
5. **Extensible** - Add new action types without schema changes

---

## Files

| File | Purpose |
|------|---------|
| `core/src/model/decorators.ts` | `generateSHACL()` - Creates SHACL from decorators |
| `core/src/shacl/SHACLShape.ts` | SHACL shape class with `toLinks()` |
| `core/src/perspectives/PerspectiveProxy.ts` | `ensureSdnaLinks()` - Stores SHACL as links |
| `rust-executor/src/perspectives/shacl_parser.rs` | Parses SHACL JSON, generates links |
| `rust-executor/src/perspectives/perspective_instance.rs` | Action retrieval with SHACL-first |
