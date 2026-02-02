# SHACL + Flows: Unified Separate Links Approach
**Consistent action representation for Classes and Flows**

## Design Decision: Separate Links (Approach 1)

**Rationale:**
1. ✅ **Consistency** - Same pattern for Classes and Flows
2. ✅ **Clarity** - One predicate = one action array (clear semantics)
3. ✅ **No query penalty** - SurrealQL batch queries eliminate O(n) concern
4. ✅ **Extensibility** - Add new action types without changing structure

---

## Part 1: Subject Classes (Recipe Example)

### W3C SHACL Links
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
```

### AD4M Action Links (Separate)
```turtle
# Constructor
recipe://RecipeShape ad4m://constructor "literal://string:[
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://name\", \"target\": \"\"},
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"recipe://rating\", \"target\": \"0\"}
]" .

# Destructor (optional)
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

**Pattern:** Each action type = one link with clear predicate semantics

---

## Part 2: Flows (State Machine Example)

### Flow Definition Example
```typescript
// Hypothetical Flow: Order Processing
flow://OrderFlow {
  initial_state: "pending"
  
  states: {
    pending: { /* actions */ },
    processing: { /* actions */ },
    completed: { /* actions */ },
    cancelled: { /* actions */ }
  }
  
  transitions: {
    pending -> processing: { /* conditions */ },
    processing -> completed: { /* conditions */ },
    processing -> cancelled: { /* conditions */ }
  }
}
```

### Flow SHACL Links (Schema)
```turtle
# Flow shape
flow://OrderFlowShape rdf:type sh:NodeShape .
flow://OrderFlowShape sh:targetClass flow://OrderFlow .
flow://OrderFlowShape ad4m://initialState "pending" .

# State definitions (as properties)
flow://OrderFlow.pending rdf:type sh:PropertyShape .
flow://OrderFlow.pending sh:path flow://state_pending .
flow://OrderFlowShape sh:property flow://OrderFlow.pending .

flow://OrderFlow.processing rdf:type sh:PropertyShape .
flow://OrderFlow.processing sh:path flow://state_processing .
flow://OrderFlowShape sh:property flow://OrderFlow.processing .

flow://OrderFlow.completed rdf:type sh:PropertyShape .
flow://OrderFlow.completed sh:path flow://state_completed .
flow://OrderFlowShape sh:property flow://OrderFlow.completed .
```

### Flow Action Links (Separate)
```turtle
# State entry actions (what happens when entering a state)
flow://OrderFlow.pending ad4m://onEntry "literal://string:[
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"flow://status\", \"target\": \"pending\"},
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"flow://timestamp\", \"target\": \"now\"}
]" .

flow://OrderFlow.processing ad4m://onEntry "literal://string:[
  {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"flow://status\", \"target\": \"processing\"},
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"flow://processor\", \"target\": \"agent_id\"}
]" .

flow://OrderFlow.completed ad4m://onEntry "literal://string:[
  {\"action\": \"setSingleTarget\", \"source\": \"this\", \"predicate\": \"flow://status\", \"target\": \"completed\"},
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"flow://completed_at\", \"target\": \"now\"}
]" .

# State exit actions (what happens when leaving a state)
flow://OrderFlow.pending ad4m://onExit "literal://string:[
  {\"action\": \"removeLink\", \"source\": \"this\", \"predicate\": \"flow://status\", \"target\": \"pending\"}
]" .

# Transition actions (what happens during state transition)
flow://OrderFlow.transition_pending_to_processing ad4m://onTransition "literal://string:[
  {\"action\": \"addLink\", \"source\": \"this\", \"predicate\": \"flow://transition_log\", \"target\": \"pending->processing\"}
]" .
```

**Pattern:** Same as Classes - one action array per link, clear predicate semantics

---

## Part 3: SurrealQL Batch Queries

### Query All Class Actions at Once
```surrealql
LET $class_uri = "recipe://RecipeShape";
LET $class_name = "Recipe";

-- Fetch all actions in one query
SELECT 
  -- Constructor
  (SELECT target FROM link WHERE source = $class_uri AND predicate = "ad4m://constructor")[0] AS constructor,
  
  -- Destructor
  (SELECT target FROM link WHERE source = $class_uri AND predicate = "ad4m://destructor")[0] AS destructor,
  
  -- All property setters
  (SELECT 
     ARRAY_AGG({
       property: STRING::SPLIT(source, ".")[1],
       actions: target
     })
     FROM link 
     WHERE source LIKE fn::concat($class_uri, ".", $class_name, ".%") 
       AND predicate = "ad4m://setter"
  ) AS property_setters,
  
  -- All collection operations
  (SELECT 
     source,
     predicate,
     target
     FROM link 
     WHERE source LIKE fn::concat($class_uri, ".", $class_name, ".%") 
       AND predicate IN ["ad4m://adder", "ad4m://remover", "ad4m://setter"]
  ) AS collection_ops
```

**Result:** Single query returns all actions with variable bindings

### Query All Flow State Actions at Once
```surrealql
LET $flow_uri = "flow://OrderFlowShape";

-- Fetch all state actions in one query
SELECT 
  -- Initial state
  (SELECT target FROM link WHERE source = $flow_uri AND predicate = "ad4m://initialState")[0] AS initial_state,
  
  -- All state entry actions
  (SELECT 
     ARRAY_AGG({
       state: STRING::SPLIT(source, ".")[1],
       on_entry: (SELECT target FROM link WHERE source = parent.source AND predicate = "ad4m://onEntry")[0],
       on_exit: (SELECT target FROM link WHERE source = parent.source AND predicate = "ad4m://onExit")[0]
     })
     FROM link 
     WHERE source LIKE fn::concat($flow_uri, ".%") 
       AND predicate IN ["ad4m://onEntry", "ad4m://onExit"]
     GROUP BY source
  ) AS state_actions,
  
  -- All transition actions
  (SELECT 
     source,
     target AS actions
     FROM link 
     WHERE source LIKE fn::concat($flow_uri, ".transition_%") 
       AND predicate = "ad4m://onTransition"
  ) AS transition_actions
```

**Result:** Single query returns complete flow definition

---

## Part 4: Consistency Across System

### Unified Action Predicate Vocabulary

| Predicate | Used In | Meaning |
|-----------|---------|---------|
| `ad4m://constructor` | Classes | Create instance |
| `ad4m://destructor` | Classes | Destroy instance |
| `ad4m://setter` | Classes (properties) | Set property value |
| `ad4m://adder` | Classes (collections) | Add to collection |
| `ad4m://remover` | Classes (collections) | Remove from collection |
| `ad4m://initialState` | Flows | Starting state |
| `ad4m://onEntry` | Flows (states) | Enter state actions |
| `ad4m://onExit` | Flows (states) | Leave state actions |
| `ad4m://onTransition` | Flows (transitions) | Transition actions |

**Benefits:**
- Clear semantics for each action type
- Extensible (add new predicates without breaking structure)
- Queryable with standard RDF/SPARQL patterns
- Consistent between Classes and Flows

---

## Part 5: Rust Implementation Pattern

### Generic Action Fetcher
```rust
async fn get_actions(
    &self,
    source_uri: &str,
    action_predicate: &str,
    context: &AgentContext,
) -> Result<Vec<Command>, AnyError> {
    let links = self.get_links(&LinkQuery {
        source: Some(source_uri.to_string()),
        predicate: Some(action_predicate.to_string()),
        ..Default::default()
    }).await?;
    
    if let Some(link) = links.first() {
        let json_str = extract_literal_string(&link.data.target)?;
        let actions: Vec<Command> = serde_json::from_str(&json_str)?;
        return Ok(actions);
    }
    
    Ok(vec![]) // Empty if not found
}

// Usage for Classes
async fn get_constructor_actions(&self, class_name: &str) -> Result<Vec<Command>> {
    let shape_uri = format!("{}{}Shape", namespace, class_name);
    self.get_actions(&shape_uri, "ad4m://constructor", context).await
}

async fn get_property_setter(&self, class_name: &str, property: &str) -> Result<Vec<Command>> {
    let prop_uri = format!("{}{}.{}", namespace, class_name, property);
    self.get_actions(&prop_uri, "ad4m://setter", context).await
}

// Usage for Flows
async fn get_state_entry_actions(&self, flow_name: &str, state: &str) -> Result<Vec<Command>> {
    let state_uri = format!("flow://{}.{}", flow_name, state);
    self.get_actions(&state_uri, "ad4m://onEntry", context).await
}

async fn get_transition_actions(&self, flow_name: &str, transition: &str) -> Result<Vec<Command>> {
    let trans_uri = format!("flow://{}.transition_{}", flow_name, transition);
    self.get_actions(&trans_uri, "ad4m://onTransition", context).await
}
```

**Pattern:** Single generic function, multiple specialized wrappers

---

## Part 6: Migration Strategy

### Phase 1: Classes (Current SHACL Work)
1. ✅ Extract actions from `generateSDNA()`
2. ✅ Add to SHACL JSON (separate fields per action type)
3. ✅ Store as separate links in Rust
4. ✅ Replace Prolog queries with link queries
5. ✅ Tests pass

### Phase 2: Flows (Future Work)
1. ⏳ Identify current Prolog flow definitions
2. ⏳ Design Flow SHACL schema (states, transitions)
3. ⏳ Extract flow actions from Prolog
4. ⏳ Store as separate links (same pattern as Classes)
5. ⏳ Replace flow Prolog queries
6. ⏳ Tests pass

**Key:** Classes implementation paves the way for Flows

---

## Recommendation: Use Separate Links (Approach 1)

**Advantages over Single Manifest:**
- ✅ Consistent with Flows (same pattern)
- ✅ Clear semantics (one predicate = one action type)
- ✅ No query penalty (SurrealQL batch queries)
- ✅ Better extensibility (add new action types easily)
- ✅ More granular (query specific action types if needed)

**Trade-off:**
- More links per class (15 vs 11)
- But SurrealQL makes this irrelevant (single batch query)

**Conclusion:** Separate links is the right choice for a unified, consistent system.

---

## Next Steps

1. ✅ Confirmed separate links approach
2. 🔄 Update TypeScript SHACL generation (separate action fields)
3. 🔄 Update Rust parser (store separate action links)
4. 🔄 Implement generic `get_actions()` pattern
5. 🔄 Replace Prolog queries
6. 🔄 Tests pass
7. ⏳ Apply same pattern to Flows (Phase 2)
