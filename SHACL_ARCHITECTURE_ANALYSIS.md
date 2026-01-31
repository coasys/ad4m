# SHACL Architecture Analysis
**Date:** 2026-01-31  
**Context:** Nico's question about decorator → Rust communication and JSON Schema integration

## Current Implementation

### Decorator → Perspective Flow
1. TypeScript decorators (`@ModelOptions`, `@Property`, `@Collection`) gather metadata
2. `ModelOptions` decorator creates `SHACLShape` object with property constraints
3. Each property/collection adds a `SHACLPropertyShape` with `name` field
4. `shape.toLinks()` converts to RDF triples:
   ```typescript
   // Example output for Recipe.name property:
   [
     { source: "recipe://RecipeShape", predicate: "sh://property", target: "recipe://Recipe.name" },
     { source: "recipe://Recipe.name", predicate: "sh://path", target: "recipe://name" },
     { source: "recipe://Recipe.name", predicate: "sh://datatype", target: "xsd://string" },
     { source: "recipe://Recipe.name", predicate: "sh://minCount", target: "literal://1^^xsd:integer" },
     { source: "recipe://Recipe.name", predicate: "sh://maxCount", target: "literal://1^^xsd:integer" }
   ]
   ```
5. Links stored directly in perspective via `PerspectiveProxy.addShacl()`
6. No serialization/deserialization step - just links

### Key Design: Named Property Shapes
- **Old approach:** Blank nodes `_:propShape0`, `_:propShape1` (not queryable)
- **New approach:** Named URIs `{namespace}{ClassName}.{propertyName}` (queryable)
- **Example:** `recipe://Recipe.name` instead of `_:propShape0`
- **Benefit:** Can query with SurrealQL to find all properties, their constraints, etc.

### Storage Format
```
Perspective Links (RDF triples):
  recipe://RecipeShape -> rdf://type -> sh://NodeShape
  recipe://RecipeShape -> sh://targetClass -> recipe://Recipe
  recipe://RecipeShape -> sh://property -> recipe://Recipe.name
  recipe://Recipe.name -> sh://path -> recipe://name
  recipe://Recipe.name -> sh://datatype -> xsd://string
  ...
```

## Nico's Questions

1. **Decorator → Rust communication:** How does the SHACL structure get from TypeScript decorators to Rust?
2. **JSON Schema integration:** How should `ensureSDNASubjectClass(jsonSchema)` work?
3. **Storage representation:** Links (queryable) vs serialized string (simpler)?

## Architectural Options

### Option A: Current Approach (Links All The Way)

**Flow:**
```
TypeScript Decorators
  → SHACLShape object
  → toLinks() method
  → Array<Link>
  → PerspectiveProxy.addShacl()
  → Rust: links.add_link() for each triple
  → Stored as RDF triples in perspective
```

**Pros:**
- ✅ Schemas are queryable with SurrealQL
- ✅ No serialization overhead
- ✅ Consistent with AD4M's RDF-native architecture
- ✅ Named property shapes enable powerful queries
- ✅ No special protocol between frontend/backend

**Cons:**
- ❌ More links to store (vs single string literal)
- ❌ Retrieval requires `fromLinks()` reconstruction
- ❌ Potentially more complex debugging

**JSON Schema integration:**
```typescript
function ensureSDNASubjectClass(jsonSchema: object) {
  const shape = jsonSchemaToSHACL(jsonSchema); // Parse JSON Schema
  const links = shape.toLinks();               // Same conversion
  await perspective.addShacl(className, shape); // Same storage
}
```

### Option B: Serialize to Turtle/JSON-LD String

**Flow:**
```
TypeScript Decorators
  → SHACLShape object
  → toTurtle() or toJSON-LD()
  → String literal
  → Store as single link: ad4m://self -> ad4m://sdna -> literal://string:...
```

**Pros:**
- ✅ Simpler storage (one link instead of many)
- ✅ Easier to inspect raw SHACL
- ✅ Standard format (Turtle/JSON-LD)

**Cons:**
- ❌ Not queryable without parsing
- ❌ Back to opaque string literals (same problem as Prolog)
- ❌ Defeats purpose of SHACL migration
- ❌ Would need parser in Rust to extract constraints

### Option C: Hybrid (String + Links)

**Flow:**
Store both:
- Canonical serialization as string (for export/interop)
- Expanded links (for queries)

**Pros:**
- ✅ Best of both worlds
- ✅ Queryable + portable

**Cons:**
- ❌ Duplication
- ❌ Synchronization complexity
- ❌ Double storage overhead

## Recommendation

**Option A (Current Approach)** is the best choice for AD4M's use case:

### Rationale

1. **Queryability is the primary goal**
   - The whole point of moving to SHACL was to make schemas queryable
   - Named property shapes enable powerful SurrealQL queries
   - Can find all properties, check constraints, validate at runtime

2. **Consistent with AD4M architecture**
   - AD4M is RDF-native (everything is links/triples)
   - Storing SHACL as links follows the same pattern as instance data
   - No special cases or exceptions

3. **No frontend/backend protocol needed**
   - Links are the universal data format in AD4M
   - Frontend calls `perspective.add(link)` for each triple
   - Backend just stores links (no parsing, no protocol)

4. **JSON Schema integration is straightforward**
   - Parse JSON Schema → Create SHACLShape → toLinks() → store
   - Same flow as decorators, same storage format
   - No duplication of logic

### Implementation for JSON Schema

```typescript
// JSON Schema → SHACL converter
function jsonSchemaToSHACL(schema: any, namespace: string): SHACLShape {
  const className = schema.title || "UnnamedClass";
  const shapeUri = `${namespace}${className}Shape`;
  const targetClass = `${namespace}${className}`;
  
  const shape = new SHACLShape(shapeUri, targetClass);
  
  // Convert properties
  for (const [propName, propDef] of Object.entries(schema.properties || {})) {
    const prop: SHACLPropertyShape = {
      name: propName,  // Enable named URI generation
      path: `${namespace}${propName}`,
      datatype: mapJsonTypeToXSD(propDef.type),
      minCount: schema.required?.includes(propName) ? 1 : undefined,
      maxCount: propDef.type === 'array' ? undefined : 1,
    };
    
    shape.addProperty(prop);
  }
  
  return shape;
}

// Usage in ensureSDNASubjectClass
async function ensureSDNASubjectClass(
  perspective: PerspectiveProxy,
  jsonSchema: object,
  namespace: string
) {
  const shape = jsonSchemaToSHACL(jsonSchema, namespace);
  const links = shape.toLinks();
  
  // Store exactly like decorators do
  await perspective.addShacl(jsonSchema.title, shape);
}
```

### Storage Overhead Analysis

**Single Recipe model with 3 properties:**
- Prolog string: 1 link (~500 bytes string literal)
- SHACL links: ~15 links (~1.5KB total)
- **Overhead:** 3x storage

**But:**
- Queryability is worth it
- SurrealDB is disk-based (storage is cheap)
- Can index property names, datatypes, etc.
- Enables runtime validation without Prolog engine

## Questions for Claude Code

1. **Performance:** What's the impact of 15 links vs 1 string on query performance?
2. **Validation:** How should runtime validation use the SHACL links?
3. **Migration:** Strategy for converting existing Prolog SDNA to SHACL links?
4. **Edge cases:** Any corner cases in JSON Schema → SHACL conversion?
5. **Compression:** Could we compress the link structure (e.g., shared property definitions)?

## Next Steps

1. ✅ Review this analysis with Claude Code
2. ⏳ Implement `jsonSchemaToSHACL()` converter
3. ⏳ Update `ensureSDNASubjectClass()` to use SHACL
4. ⏳ Write tests for JSON Schema → SHACL conversion
5. ⏳ Performance benchmark: Prolog string vs SHACL links
6. ⏳ Migration script: Existing perspectives → SHACL format
