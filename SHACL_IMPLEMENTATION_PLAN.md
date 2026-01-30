# SHACL SDNA Migration - Implementation Plan

## Goal
Replace Prolog-based Social DNA (SDNA) definitions with SHACL (Shapes Constraint Language) while maintaining backward compatibility and the existing Ad4mModel decorator API.

## Background Context

### Current System
- TypeScript decorators (@Property, @Collection, @ModelOptions) define model metadata
- `ModelOptions` decorator generates `generateSDNA()` method that outputs Prolog predicates
- Prolog predicates stored as literal in Perspective
- **Already migrated to SurrealDB**: Instance queries, property access, link traversal
- **Still using Prolog**: SDNA definitions, type checking, validation

### Why SHACL?
- W3C Recommendation (official web standard)
- Native RDF format (perfect for our triple/link-based architecture)
- Built-in validation constraints
- Better tooling and interoperability
- More familiar to developers than Prolog
- Declarative and easier to reason about

## Phase 1: SHACL Generation (This Implementation)

### Step 1: Create SHACL Data Structures
**Files to create:**
- `core/src/shacl/SHACLShape.ts` - Core SHACL shape classes
- `core/src/shacl/SHACLValidator.ts` - Validation logic
- `core/src/shacl/SHACLSerializer.ts` - Turtle/Links serialization

**Classes needed:**
```typescript
class SHACLShape {
  nodeShapeUri: string
  targetClass?: string
  properties: SHACLPropertyShape[]
  
  toTurtle(): string
  toLinks(): Link[]
  fromLinks(links: Link[]): SHACLShape
}

class SHACLPropertyShape {
  path: string
  datatype?: string
  nodeKind?: 'IRI' | 'Literal' | 'BlankNode'
  minCount?: number
  maxCount?: number
  pattern?: string
  minInclusive?: number
  maxInclusive?: number
  hasValue?: string
  local?: boolean  // AD4M-specific
  writable?: boolean  // AD4M-specific
}

class SHACLValidator {
  validate(shape: SHACLShape, perspective: PerspectiveProxy, instance: string): ValidationReport
}
```

### Step 2: Update Decorators
**File to modify:** `core/src/model/decorators.ts`

**Changes:**
1. Keep existing `generateSDNA()` for backward compatibility
2. Add new `generateSHACL()` method to `ModelOptions` decorator
3. Convert decorator metadata to SHACL shapes

**Mapping:**
- `@ModelOptions({ name: "Recipe" })` → `sh:NodeShape` with `sh:targetClass`
- `@Property({ through: "...", required: true })` → `sh:PropertyShape` with `sh:minCount 1`
- `@Collection({ through: "..." })` → `sh:PropertyShape` with no `sh:maxCount`
- `@Flag({ through: "...", value: "..." })` → `sh:PropertyShape` with `sh:hasValue`
- `@Optional()` → `sh:PropertyShape` with `sh:minCount 0`

### Step 3: Storage Integration
**File to modify:** `core/src/perspectives/PerspectiveProxy.ts`

**New methods:**
```typescript
async addShacl(name: string, shape: SHACLShape): Promise<void>
async getShacl(name: string): Promise<SHACLShape | null>
async getAllShacl(): Promise<SHACLShape[]>
async validateInstance(shapeUri: string, instanceUri: string): Promise<ValidationReport>
```

**Storage strategy:**
Store SHACL as RDF triples (links) in the Perspective:
```
<recipe:RecipeShape> <rdf:type> <sh:NodeShape>
<recipe:RecipeShape> <sh:targetClass> <recipe:Recipe>
<recipe:RecipeShape> <sh:property> <_:prop1>
<_:prop1> <sh:path> <recipe:name>
<_:prop1> <sh:datatype> <xsd:string>
<_:prop1> <sh:minCount> "1"^^xsd:integer
```

### Step 4: Dual System Support
**Goal:** Run both Prolog and SHACL in parallel during migration

**Changes to `Ad4mModel`:**
1. Keep `generateSDNA()` active
2. Add `generateSHACL()` active
3. Both get called when adding SDNA to perspective
4. Validation tries SHACL first, falls back to Prolog

### Step 5: Testing
**Files to create:**
- `core/src/shacl/SHACLShape.test.ts`
- `core/src/shacl/SHACLValidator.test.ts`
- `tests/js/tests/shacl-integration.test.ts`

**Test cases:**
1. Generate SHACL from decorators
2. Serialize to Turtle
3. Store as links in Perspective
4. Retrieve and reconstruct shape
5. Validate instances
6. Compare Prolog vs SHACL results

## Implementation Order

1. ✅ **Research** (DONE - see `memory/learning/shacl-migration-research-2026-01-30.md`)

2. **Create SHACL core** (`core/src/shacl/`)
   - SHACLShape class
   - SHACLPropertyShape class
   - Turtle serialization
   - Link serialization/deserialization

3. **Update decorators** (`core/src/model/decorators.ts`)
   - Add `generateSHACL()` to ModelOptions
   - Convert metadata to SHACL

4. **Storage integration** (`core/src/perspectives/PerspectiveProxy.ts`)
   - addShacl/getShacl methods
   - Link-based storage

5. **Validation** (`core/src/shacl/SHACLValidator.ts`)
   - Basic constraint checking
   - Integration with existing validation flow

6. **Tests**
   - Unit tests for SHACL classes
   - Integration tests with Perspective
   - Comparison tests (Prolog vs SHACL)

7. **Documentation**
   - Update docs/social-dna.md
   - Add migration guide

## Success Criteria

- [ ] SHACL shapes generated from decorators
- [ ] SHACL stored as links in Perspective
- [ ] SHACL retrieved and reconstructed correctly
- [ ] Basic validation works (required fields, types)
- [ ] All existing tests still pass
- [ ] Dual system (Prolog + SHACL) runs in parallel
- [ ] Documentation updated

## Files to Review Before Starting

1. `core/src/model/decorators.ts` (lines 576+) - Current Prolog generation
2. `core/src/model/Ad4mModel.ts` - Model base class
3. `core/src/perspectives/PerspectiveProxy.ts` - addSdna method
4. `docs.ad4m.dev` - Social DNA documentation

## Key Design Decisions

1. **Storage:** Links (native RDF) rather than Turtle literals
2. **Namespace:** Use `sh:` prefix for SHACL standard properties
3. **Extension:** Use `ad4m:` prefix for AD4M-specific metadata (writable, local)
4. **Migration:** Dual system - both Prolog and SHACL active
5. **Validation:** SHACL-first with Prolog fallback
6. **Backward compatibility:** Keep existing API unchanged

## Notes for Claude Code

- Start with Step 2 (SHACL core classes)
- Use TypeScript strict mode
- Follow existing code style in `core/src/model/`
- Add JSDoc comments
- Write tests alongside implementation
- Commit frequently with clear messages
- Ask for checkpoints at major milestones

## Reference Material

- SHACL W3C Spec: https://www.w3.org/TR/shacl/
- RDF Turtle: https://www.w3.org/TR/turtle/
- Research doc: `memory/learning/shacl-migration-research-2026-01-30.md`
- Current Prolog gen: `core/src/model/decorators.ts:576-750`

---

**Branch:** feat/shacl-sdna-migration  
**Start:** 2026-01-30 23:08  
**Approach:** Incremental with Git checkpoints
