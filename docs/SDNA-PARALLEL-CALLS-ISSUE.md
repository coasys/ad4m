# SDNA Parallel Calls Issue and Solutions

## Problem Statement

When multiple `perspective.ensureSDNASubjectClass()` calls are made in parallel (e.g., using `Promise.all()`), the operations hang or fail to complete. This occurs because the underlying Rust implementation uses a mutex (`sdna_change_mutex`) to serialize SDNA modifications, which cannot handle concurrent calls efficiently.

### Example of Problematic Code

```typescript
// This WILL hang or fail:
await Promise.all([
  perspective.ensureSDNASubjectClass(Community),
  perspective.ensureSDNASubjectClass(Channel),
  perspective.ensureSDNASubjectClass(App),
  perspective.ensureSDNASubjectClass(Conversation),
  // ... more classes
]);
```

### Why It Happens

1. **Mutex Lock**: The `add_sdna` function in `rust-executor/src/perspectives/perspective_instance.rs` (line 1499) acquires an exclusive lock via `sdna_change_mutex`
2. **Serialization**: Only one SDNA modification can proceed at a time
3. **Resource Exhaustion**: When 12+ GraphQL mutations queue up waiting for the same mutex:
   - Request timeouts may occur
   - Connection pool limits may be reached
   - Async runtime can experience contention
   - Results in deadlock or timeout cascade

### Current Workaround

```typescript
// This WORKS - sequential execution:
await perspective.ensureSDNASubjectClass(Community);
await perspective.ensureSDNASubjectClass(Channel);
await perspective.ensureSDNASubjectClass(App);
await perspective.ensureSDNASubjectClass(Conversation);
// ... more classes
```

**Note**: All AD4M tests use sequential calls, never parallel.

## Historical Context

- The `sdna_change_mutex` has existed since March 2024 (commit `c6f1f96c`)
- This limitation has always been present but may have gone unnoticed due to:
  - Fewer concurrent operations in typical usage
  - Network latency naturally spacing out calls
  - Timing differences in test vs. production environments
- The issue became more apparent with increased usage patterns and faster development machines

## Solution Options

### Option 1: Queue System in AD4M

**Implementation**: Add an internal queue in the Rust `add_sdna` method that processes requests sequentially behind the scenes.

**Pros**:

- Clean API - users don't need to know about the limitation
- Existing code using `Promise.all()` continues working
- No breaking changes

**Cons**:

- Adds complexity to AD4M core
- Promises resolve out of order (first call might finish last)
- Still not truly parallel, just hides the limitation
- May introduce subtle timing issues in dependent code

### Option 2: Batch Operation API (RECOMMENDED)

**Implementation**: Add a new method that handles multiple classes in a single operation:

```typescript
// New batch API
await perspective.ensureSDNASubjectClasses([
  Community,
  Channel,
  App,
  Conversation,
  ConversationSubgroup,
  Topic,
  Embedding,
  SemanticRelationship,
  Message,
  TaskBoard,
  TaskColumn,
  Task,
]);

// Singular method still available (must be used sequentially)
await perspective.ensureSDNASubjectClass(Community);
```

**Pros**:

- Most efficient - one mutex acquisition for all classes
- Clear intent in the API
- Could be truly atomic
- Similar to database "batch insert" patterns
- Non-breaking - adds new method alongside existing one
- Explicit about what's happening

**Cons**:

- Requires API changes across stack (Rust, Core, TypeScript client)
- Existing code needs migration (but can be gradual)
- Additional API surface to maintain

**Implementation Scope**:

1. Rust: Add batch handler in `perspective_instance.rs`
2. GraphQL: Add `perspectiveAddSdnaMultiple` mutation
3. Core: Add `PerspectiveProxy.ensureSDNASubjectClasses()`
4. TypeScript: Update type definitions

### Option 3: Remove/Relax the Mutex

**Implementation**: Make SDNA additions concurrent-safe at the Prolog/database level.

**Pros**:

- True parallelism
- Best performance theoretically

**Cons**:

- Prolog engines may not support concurrent modifications
- High risk of introducing data corruption bugs
- Requires significant architectural changes
- May require replacing Prolog engine entirely
- Unknown unknowns in distributed scenarios

### Option 4: Documentation Only

**Implementation**: Document that `ensureSDNASubjectClass` must be called sequentially.

**Pros**:

- Zero implementation cost
- Matches current reality
- All tests already follow this pattern

**Cons**:

- Footgun for developers - easy to get wrong
- Non-obvious failure mode (hangs rather than error messages)
- Goes against modern async/await best practices
- Poor developer experience

## Recommendation: Hybrid Approach

### Phase 1: Immediate (Now)

1. **Fix Flux**: Change to sequential calls
2. **Document limitation**: Add clear warnings in API documentation
3. **Add JSDoc warnings** to `ensureSDNASubjectClass`:
   ```typescript
   /**
    * @warning This method cannot be called in parallel.
    * Use ensureSDNASubjectClasses() for multiple classes,
    * or call sequentially with await between each call.
    */
   async ensureSDNASubjectClass(jsClass: any): Promise<void>
   ```

### Phase 2: Medium Term (Next Sprint)

1. **Implement batch API** (`ensureSDNASubjectClasses`)
2. **Migrate common patterns** in flux-api, we-\* packages
3. **Add linter rule** to catch `Promise.all()` with `ensureSDNASubjectClass`

### Phase 3: Long Term (Future)

1. **Consider architectural improvements** if performance becomes critical
2. **Evaluate queueing system** if batch API proves insufficient

## Migration Guide

### For Flux

**Before:**

```typescript
await Promise.all([
  perspective.ensureSDNASubjectClass(Community),
  perspective.ensureSDNASubjectClass(Channel),
  // ...
]);
```

**After (Immediate Fix):**

```typescript
// Add models to the perspectives SDNA sequentially to avoid mutex contention
// Running in parallel with Promise.all causes deadlocks in the Prolog/SDNA engine
await perspective.ensureSDNASubjectClass(Community);
await perspective.ensureSDNASubjectClass(Channel);
await perspective.ensureSDNASubjectClass(App);
// ...
```

**After (With Batch API):**

```typescript
await perspective.ensureSDNASubjectClasses([
  Community,
  Channel,
  App,
  Conversation,
  ConversationSubgroup,
  Topic,
  Embedding,
  SemanticRelationship,
  Message,
  TaskBoard,
  TaskColumn,
  Task,
]);
```

## References

- Original issue identified in Flux community creation
- `sdna_change_mutex` introduced: commit `c6f1f96c` (March 2024)
- Related "we" codebase workaround: Uses `Promise.all()` + 500ms sleep
- All AD4M tests use sequential pattern

## Related Files

- `rust-executor/src/perspectives/perspective_instance.rs` - Line 1499 (mutex lock)
- `core/src/perspectives/PerspectiveProxy.ts` - Line 1755 (`ensureSDNASubjectClass`)
- `flux/packages/api/src/createCommunity.ts` - Community creation pattern
- `we/packages/design-system/5-widgets/src/solid/widgets/modals/CreateSpaceModalWidget.tsx` - Similar pattern with workaround
