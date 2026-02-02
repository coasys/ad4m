# SHACL Link Structure Overview
**Complete class representation with W3C SHACL standard + AD4M extensions**

## Example: Recipe Class

Let's show all links that would represent a complete `Recipe` class with properties and actions.

### TypeScript Decorator Definition
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

---

## Approach 1: Multiple Links (Original Design)

### 1. W3C SHACL Standard Links

#### Shape Definition
```turtle
# Main shape node
recipe://RecipeShape
  rdf:type sh:NodeShape ;
  sh:targetClass recipe://Recipe .
```

#### Property: name
```turtle
# Property shape node (named URI, not blank node)
recipe://Recipe.name
  rdf:type sh:PropertyShape ;
  sh:path recipe://name ;
  sh:datatype xsd:string ;
  sh:maxCount 1 .

# Link property to shape
recipe://RecipeShape
  sh:property recipe://Recipe.name .
```

#### Property: rating
```turtle
recipe://Recipe.rating
  rdf:type sh:PropertyShape ;
  sh:path recipe://rating ;
  sh:datatype xsd:integer ;
  sh:maxCount 1 .

recipe://RecipeShape
  sh:property recipe://Recipe.rating .
```

#### Collection: ingredients
```turtle
recipe://Recipe.ingredients
  rdf:type sh:PropertyShape ;
  sh:path recipe://has_ingredient ;
  sh:nodeKind sh:IRI .  # References other entities

recipe://RecipeShape
  sh:property recipe://Recipe.ingredients .
```

**Total W3C SHACL links: 10** (1 shape + 3 properties × 3 links each)

### 2. AD4M Action Extensions

These extend SHACL with operational behavior (not part of W3C standard):

#### Constructor Actions
```turtle
recipe://RecipeShape
  ad4m://constructor "literal://string:[
    {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"\"},
    {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"0\"}
  ]" .
```

#### Property Setters
```turtle
recipe://Recipe.name
  ad4m://setter "literal://string:[
    {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"value\"}
  ]" .

recipe://Recipe.rating
  ad4m://setter "literal://string:[
    {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"value\"}
  ]" .
```

#### Collection Operations
```turtle
recipe://Recipe.ingredients
  ad4m://adder "literal://string:[
    {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
  ]" .

recipe://Recipe.ingredients
  ad4m://remover "literal://string:[
    {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
  ]" .
```

**Total AD4M action links: 5** (1 constructor + 2 setters + 2 collection ops)

### Summary: Approach 1
- **Total links per class:** 15
- **W3C SHACL conformant:** Yes (10 links)
- **AD4M extensions:** 5 links using `ad4m://` namespace
- **Query complexity:** O(n properties) for actions

---

## Approach 2: Single Action Manifest (Optimized)

### 1. W3C SHACL Standard Links
**Same as Approach 1** - 10 links for shape + properties

### 2. AD4M Action Extension (Single Link)

```turtle
recipe://RecipeShape
  ad4m://actionManifest "literal://string:{
    \"constructor\": [
      {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"\"},
      {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"0\"}
    ],
    \"destructor\": [],
    \"properties\": {
      \"name\": {
        \"setter\": [
          {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"value\"}
        ]
      },
      \"rating\": {
        \"setter\": [
          {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"value\"}
        ]
      }
    },
    \"collections\": {
      \"ingredients\": {
        \"adder\": [
          {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
        ],
        \"remover\": [
          {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"recipe://has_ingredient\", \"target\": \"value\"}
        ]
      }
    }
  }" .
```

### Summary: Approach 2
- **Total links per class:** 11
- **W3C SHACL conformant:** Yes (10 links)
- **AD4M extensions:** 1 link using `ad4m://` namespace
- **Query complexity:** O(1) for all actions

**Efficiency gain:** 27% fewer links, O(1) vs O(n) queries

---

## SHACL Conformance

### What's Standard W3C SHACL?
✅ **These predicates are W3C standard:**
- `rdf:type`
- `sh:NodeShape`
- `sh:PropertyShape`
- `sh:targetClass`
- `sh:property`
- `sh:path`
- `sh:datatype`
- `sh:maxCount`
- `sh:minCount`
- `sh:nodeKind`

✅ **Standard SHACL tools can:**
- Parse our shapes
- Validate data against constraints
- Export to Turtle/JSON-LD
- Interoperate with other SHACL systems

### What's AD4M Extension?
⚠️ **These predicates are AD4M-specific:**
- `ad4m://constructor`
- `ad4m://destructor`
- `ad4m://actionManifest`
- `ad4m://setter`
- `ad4m://adder`
- `ad4m://remover`

⚠️ **Standard SHACL tools will:**
- Ignore these predicates (unknown namespace)
- Still process all W3C SHACL correctly
- Can export partial graph (SHACL only, minus actions)

**This is intentional and correct!** SHACL is designed to be extensible via custom namespaces.

---

## How It Works Together

### 1. Schema Validation (W3C SHACL)
```rust
// Query: Get all properties for Recipe class
SELECT source, predicate, target 
FROM links 
WHERE source = "recipe://RecipeShape" 
  AND predicate = "sh:property"
// Returns: recipe://Recipe.name, recipe://Recipe.rating, recipe://Recipe.ingredients

// Query: Get constraints for name property
SELECT source, predicate, target
FROM links
WHERE source = "recipe://Recipe.name"
// Returns: sh:path=recipe://name, sh:datatype=xsd:string, sh:maxCount=1
```

### 2. Instance Creation (AD4M Actions)
```rust
// Query: Get constructor actions
SELECT target
FROM links
WHERE source = "recipe://RecipeShape" 
  AND predicate = "ad4m://constructor"
// OR (Approach 2):
WHERE source = "recipe://RecipeShape" 
  AND predicate = "ad4m://actionManifest"

// Parse JSON, execute actions:
addLink(expr, "recipe://name", "")
addLink(expr, "recipe://rating", "0")
```

### 3. Property Updates (AD4M Actions)
```rust
// Approach 1: Query property-specific setter
SELECT target
FROM links
WHERE source = "recipe://Recipe.name" 
  AND predicate = "ad4m://setter"

// Approach 2: Extract from manifest
// (already cached from shape query)
let manifest = parse_action_manifest(recipe_shape);
let setter = manifest.properties["name"].setter;

// Execute action:
setSingleTarget(expr, "recipe://name", new_value)
```

---

## Comparison Table

| Aspect | Approach 1 (Multiple) | Approach 2 (Manifest) |
|--------|----------------------|----------------------|
| **Links per class** | 15 | 11 |
| **SHACL conformance** | Full | Full |
| **Action queries** | O(n properties) | O(1) |
| **Storage overhead** | Higher | Lower |
| **Granularity** | Per-property actions | Bundled actions |
| **Extensibility** | Add new action types easily | Must update manifest structure |

---

## Rust Query Examples

### Approach 1: Multiple Links
```rust
async fn get_property_setter_actions(
    &self,
    class_name: &str,
    property: &str,
    context: &AgentContext,
) -> Result<Vec<Command>, AnyError> {
    // 1. Find property shape URI
    let prop_uri = format!("{}{}.{}", namespace, class_name, property);
    
    // 2. Query action link
    let links = self.get_links(&LinkQuery {
        source: Some(prop_uri),
        predicate: Some("ad4m://setter".to_string()),
        ..Default::default()
    }).await?;
    
    // 3. Parse JSON from target
    if let Some(link) = links.first() {
        let json_str = extract_literal_string(&link.data.target)?;
        let actions: Vec<Command> = serde_json::from_str(&json_str)?;
        return Ok(actions);
    }
    
    Err(anyhow!("No setter found"))
}
```

### Approach 2: Single Manifest
```rust
async fn get_property_setter_actions(
    &self,
    class_name: &str,
    property: &str,
    context: &AgentContext,
) -> Result<Vec<Command>, AnyError> {
    // 1. Get manifest (cached after first query)
    let manifest = self.get_action_manifest(class_name).await?;
    
    // 2. Extract property setter
    manifest.properties
        .get(property)
        .and_then(|p| p.setter.clone())
        .ok_or(anyhow!("No setter found"))
}

// Cache helper
async fn get_action_manifest(&self, class_name: &str) -> Result<ActionManifest> {
    let shape_uri = format!("{}{}Shape", namespace, class_name);
    let links = self.get_links(&LinkQuery {
        source: Some(shape_uri),
        predicate: Some("ad4m://actionManifest".to_string()),
        ..Default::default()
    }).await?;
    
    if let Some(link) = links.first() {
        let json_str = extract_literal_string(&link.data.target)?;
        return Ok(serde_json::from_str(&json_str)?);
    }
    
    Err(anyhow!("No action manifest found"))
}
```

---

## Recommendation

**Use Approach 2 (Single Manifest)** because:
- ✅ 27% fewer links
- ✅ O(1) query complexity
- ✅ Natural caching (fetch manifest once per class)
- ✅ Easier to extend (add new action types to manifest)
- ✅ Still fully SHACL conformant

The only trade-off is slightly more complex JSON structure, but that's worth the performance gain.

---

## Next Steps

1. ✅ Confirmed architecture is sound
2. 🔄 Choose approach (recommend Approach 2)
3. 🔄 Implement TypeScript: Extract actions from `generateSDNA()`, add to SHACL JSON
4. 🔄 Implement Rust: Parse action manifest, store as links
5. 🔄 Replace Prolog queries with manifest lookups
6. 🔄 Test with integration tests
