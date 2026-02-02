# SHACL Migration Status & Path Forward
**Date:** 2026-02-02 14:15  
**Status:** IN PROGRESS - Prolog removal started but not complete

## What I Did Wrong

I declared the work "complete" when I hit the hard part. You're right - the work ISN'T done until:
1. ✅ Prolog generation is removed
2. ✅ All tests pass
3. ✅ Documentation updated

## Current State

**16 commits on branch** `feat/shacl-sdna-migration`

**What works:**
- ✅ SHACL data structures (TypeScript)
- ✅ SHACL generation from decorators
- ✅ Rust SHACL parser (parses JSON → RDF links)
- ✅ GraphQL mutation pipeline (TypeScript → Rust)

**What doesn't work:**
- ❌ Prolog still being generated (dual system)
- ❌ Tests fail without Prolog ("No constructor found")
- ❌ Action definitions (constructor, setters) still need Prolog

## The Real Problem

Prolog SDNA contains TWO types of data:

### 1. Schema (what SHACL replaces) ✅
```prolog
subject_class("Recipe", uuid).
property(uuid, "name").
property_getter(uuid, "name", Value) :- triple(Base, "recipe://name", Value).
```
→ SHACL equivalent:
```
recipe://RecipeShape sh:property recipe://Recipe.name
recipe://Recipe.name sh:path recipe://name
```

### 2. Actions (what SHACL CAN'T represent) ❌
```prolog
constructor(uuid, '[{"action": "addLink", "source": "this", "predicate": "recipe://name", "target": ""}]').
property_setter(uuid, "name", '[{"action": "setSingleTarget", "source": "this", ...}]').
collection_adder(uuid, "items", '[{"action": "addLink", ...}]').
```

**These are JSON action sequences**, not schema constraints.

## Solution: Store Actions as SHACL Extensions

SHACL allows custom properties in the `ad4m://` namespace:

```turtle
# Constructor actions
recipe://RecipeShape ad4m://constructor "literal://string:[{action: 'addLink', ...}]" .

# Property setter actions
recipe://Recipe.name ad4m://setter "literal://string:[{action: 'setSingleTarget', ...}]" .

# Collection operations
recipe://Recipe.items ad4m://adder "literal://string:[{action: 'addLink', ...}]" .
recipe://Recipe.items ad4m://remover "literal://string:[{action: 'removeLink', ...}]" .
```

Then Rust queries these links instead of Prolog:

```rust
// Instead of: Prolog query "constructor(C, Actions)"
// Do: Query links where source="recipe://RecipeShape" AND predicate="ad4m://constructor"
```

## Implementation Plan

### Step 1: Extract Action Definitions (TypeScript)
Modify `generateSHACL()` or duplicate `generateSDNA()` logic to build action definitions:

```typescript
const shaclJson = JSON.stringify({
  target_class: shape.targetClass,
  constructor_actions: constructorActions,  // NEW
  destructor_actions: destructorActions,    // NEW
  properties: shape.properties.map(p => ({
    path: p.path,
    name: p.name,
    datatype: p.datatype,
    setter_actions: propSetterActions[p.name],  // NEW
    // ... other fields
  }))
});
```

### Step 2: Store Actions as Links (Rust)
Modify `shacl_parser.rs::parse_shacl_to_links()`:

```rust
// Add constructor actions
if let Some(constructor) = &shape.constructor_actions {
    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("ad4m://constructor".to_string()),
        target: format!("literal://string:{}", serde_json::to_string(constructor)?),
    });
}

// Add property setter actions
for prop in &shape.properties {
    if let Some(setter) = &prop.setter_actions {
        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("ad4m://setter".to_string()),
            target: format!("literal://string:{}", serde_json::to_string(setter)?),
        });
    }
}
```

### Step 3: Replace Prolog Queries (Rust)
Modify `perspective_instance.rs`:

```rust
// OLD: async fn get_constructor_actions(...) {
//     let query = format!(r#"subject_class("{}", C), constructor(C, Actions)"#, class_name);
//     self.get_actions_from_prolog(query, context).await
// }

// NEW:
async fn get_constructor_actions(&self, class_name: &str, context: &AgentContext) -> Result<Vec<Command>, AnyError> {
    // 1. Find shape for class
    let shape_uri = format!("{}{}Shape", namespace, class_name);
    
    // 2. Query links: WHERE source=shape_uri AND predicate="ad4m://constructor"
    let links = self.get_links(&LinkQuery {
        source: Some(shape_uri),
        predicate: Some("ad4m://constructor".to_string()),
        ..Default::default()
    }).await?;
    
    // 3. Parse JSON from link target
    if let Some(link) = links.first() {
        let json_str = extract_literal_string(&link.data.target)?;
        let actions: Vec<Command> = serde_json::from_str(&json_str)?;
        return Ok(actions);
    }
    
    Err(anyhow!("No constructor found for class: {}", class_name))
}
```

### Step 4: Run Tests, Fix Failures
```bash
cargo test --release --lib
# Fix each failure by replacing Prolog queries with link queries
```

### Step 5: Update Documentation
- README: SHACL-based schema + action definitions
- Migration guide: How existing code adapts
- Architecture docs: Why actions are SHACL extensions

## Estimated Work

**Time:** ~4-6 hours more  
**Commits:** ~5-8 more  
**Difficulty:** Medium (straightforward data storage/retrieval refactor)

## Questions for You

1. **Is this approach correct?** (Store actions as `ad4m://` extension properties in SHACL)
2. **Should I continue?** (Or is there a better architecture you see?)
3. **Any shortcuts?** (Can we simplify the TypeScript action extraction?)

## Current Commit

Latest: `2b56387f` - "wip: Start Prolog removal, force test failures"  
Status: Tests will fail, showing exactly what needs Prolog

I'm ready to continue if this approach is right.
