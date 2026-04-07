# AD4M Language Interface Analysis & Recommendations

*Analysis Date: 2026-03-07*
*Context: Language interface refactoring for WASM compatibility and future-proofing*

## Current Architecture Overview

### How Languages Work Today

**1. Language Loading Pattern (LanguageController.ts:250-267)**
```typescript
// Language module exports a `create` function
import languageSource from './language.bundle.js'

// Extract create function (handles ESM/CJS interop)
let create;
if (!languageSource.default) {
    create = languageSource;
} else {
    if (languageSource.default.default) {
        create = languageSource.default.default;
    } else {
        create = languageSource.default;
    }
}

// Create language instance with context
const language = await create({
    ...this.#context, 
    customSettings, 
    storageDirectory, 
    Holochain, 
    ad4mSignal
})
```

**2. Language Interface Structure (Language.ts)**
```typescript
interface Language {
    readonly name: string;
    readonly expressionAdapter?: ExpressionAdapter;  // Nested object
    readonly linksAdapter?: LinkSyncAdapter;         // Nested object
    readonly telepresenceAdapter?: TelepresenceAdapter; // Nested object
    // ... more nested adapters
    interactions(expression: Address): Interaction[];
}
```

**3. Current Implementation Pattern (p-diff-sync, NextGraph)**
```typescript
// Language creates nested adapter objects
export default async function create(context: LanguageContext): Promise<Language> {
  const linksAdapter = new LinkAdapter(context);
  const telepresenceAdapter = new TelepresenceAdapter(context);
  
  return {
    name: 'my-language',
    linksAdapter,        // Nested instance
    telepresenceAdapter, // Nested instance
    interactions: () => []
  }
}
```

## Problems with Current Design

### 1. **WASM Compatibility Issues**
- **Complex object graphs**: WASM languages must construct nested JavaScript objects to return from `create()`
- **Prototype chains**: Nested adapters rely on JavaScript prototype chains which are awkward in WASM
- **Memory management**: Creating many small JS objects from WASM is inefficient

### 2. **Indirection Overhead**
- Runtime checks like `language.linksAdapter?.sync()` add overhead
- Deep property access: `language.telepresenceAdapter.sendSignal()`
- Optional chaining complexity throughout the codebase

### 3. **ESM/CJS Interop Complexity**
```typescript
// LanguageController has to handle multiple export patterns:
if (!languageSource.default) {
    create = languageSource;
} else if (languageSource.default.default) {
    create = languageSource.default.default;
} else {
    create = languageSource.default;
}
```

### 4. **Poor Self-Discovery**
- Runtime must probe for capabilities: `if (language.linksAdapter)`
- No explicit contract about what a language provides
- Error-prone: calling missing adapter methods causes runtime failures

### 5. **Initialization Complexity**
- Languages must implement `create()` factory function
- Context passing through nested constructors
- Hard to statically analyze what a language provides

---

## Recommended New Design

### Core Principle: Flat Exports, Not Nested Objects

Instead of returning a nested object from `create()`, languages export **individual functions directly**:

#### Current (Problematic):
```typescript
// language.bundle.js
export default function create(context) {
  return {
    name: 'my-language',
    linksAdapter: {
      sync: () => {...},
      commit: (diff) => {...},
      // ...
    },
    expressionAdapter: {
      get: (addr) => {...},
      putAdapter: { createPublic: (content) => {...} }
    }
  }
}
```

#### Recommended (WASM-Friendly):
```typescript
// language.bundle.js - flat exports

// Metadata
export const name = 'my-language';
export const version = '1.0.0';

// Capability declarations (static, analyzable)
export const capabilities = ['link-sync', 'expression-storage'];

// Link Sync functions (flat exports)
export function linkSyncWritable(): boolean { return true; }
export function linkSyncPublic(): boolean { return false; }
export function linkSyncOthers(): Promise<DID[]> {...}
export function linkSyncCurrentRevision(): Promise<string> {...}
export function linkSyncSync(): Promise<PerspectiveDiff> {...}
export function linkSyncCommit(diff: PerspectiveDiff): Promise<string> {...}
export function linkSyncRender(): Promise<Perspective> {...}
export function linkSyncAddCallback(callback: PerspectiveDiffObserver) {...}

// Expression functions (flat exports)
export function expressionGet(address: Address): Promise<Expression | null> {...}
export function expressionCreatePublic(content: object): Promise<Address> {...}

// Telepresence functions (optional, flat)
export function telepresenceSetOnlineStatus(status: PerspectiveExpression): Promise<void> {...}
export function telepresenceSendSignal(remoteDid: string, payload: PerspectiveExpression): Promise<object> {...}

// Lifecycle
export function init(context: LanguageContext): Promise<void> {...}
export function teardown(): Promise<void> {...}
```

### Benefits of Flat Design

1. **WASM Native**: Functions are WASM's natural export format
   - No nested object construction required
   - Direct function calls across the WASM/JS boundary
   - Better memory efficiency

2. **Static Analysis**: `capabilities` array tells runtime what's available
   - No runtime probing needed
   - Clear contract
   - Type-safe at compile time

3. **Simpler Loading**:
```typescript
// New loading pattern
import * as language from './language.bundle.js';

// Check capabilities
if (language.capabilities?.includes('link-sync')) {
  // Direct function call - no nesting
  const diff = await language.linkSyncSync();
}
```

4. **No Factory Function**: Just call `init()` if it exists
   - Simpler mental model
   - Less boilerplate
   - Easier to understand

---

## Migration Strategy

### Phase 1: Support Both Patterns (Backward Compatible)

```typescript
// LanguageController.ts
async loadLanguage(sourcePath: string) {
    const module = await import(sourcePath);
    
    // New pattern: flat exports
    if (module.capabilities) {
        return this.loadFlatLanguage(module);
    }
    
    // Legacy pattern: create() function returning nested object
    if (module.default || module.create) {
        return this.loadLegacyLanguage(module);
    }
}

private loadFlatLanguage(module: any): Language {
    // Wrap flat exports in adapter for internal use
    return {
        name: module.name,
        
        // Adapter proxies to flat functions
        linksAdapter: module.capabilities?.includes('link-sync') ? {
            sync: module.linkSyncSync.bind(module),
            commit: module.linkSyncCommit.bind(module),
            // ...
        } : undefined,
        
        expressionAdapter: module.capabilities?.includes('expression-storage') ? {
            get: module.expressionGet.bind(module),
            putAdapter: { createPublic: module.expressionCreatePublic.bind(module) }
        } : undefined,
        
        // ... other adapters
    }
}
```

### Phase 2: Update Core Interfaces

```typescript
// New Language interface (internal representation stays similar)
// But languages can export flat instead of nested

// New LanguageContext - minimal, explicit
interface LanguageContext {
    agent: AgentService;
    storageDirectory: string;
    customSettings?: object;
    
    // Optional services based on capabilities
    holochain?: HolochainLanguageDelegate;  // Only for 'holochain' capability
    signal?: (payload: object) => void;      // Signal callback
}
```

### Phase 3: Language Developer Experience

**New template for JS/TS:**
```typescript
// New simple pattern
export const name = 'my-language';
export const version = '1.0.0';
export const capabilities = ['link-sync'];

// State (module-level, simple)
let context: LanguageContext;

export async function init(ctx: LanguageContext) {
    context = ctx;
    // Setup...
}

// Just export functions
export async function linkSyncSync(): Promise<PerspectiveDiff> {
    // Implementation...
}

export async function linkSyncCommit(diff: PerspectiveDiff): Promise<string> {
    // Implementation...
}
```

**WASM language (Rust):**
```rust
// Much simpler - just export functions
#[wasm_bindgen]
pub fn name() -> String { "my-language".into() }

#[wasm_bindgen]
pub fn capabilities() -> JsValue {
    serde_wasm_bindgen::to_value(&["link-sync"]).unwrap()
}

#[wasm_bindgen]
pub async fn link_sync_sync() -> Result<JsValue, JsValue> {
    // Implementation...
}

#[wasm_bindgen]
pub async fn link_sync_commit(diff: JsValue) -> Result<String, JsValue> {
    // Implementation...
}
```

---

## Additional Improvements

### 1. **Explicit Capability Declaration**
```typescript
export const capabilities = [
    'link-sync',           // Has LinkSyncAdapter functions
    'expression-storage',  // Has ExpressionAdapter functions  
    'expression-readonly', // Read-only expression language
    'telepresence',        // Has TelepresenceAdapter functions
    'direct-message',      // Has DirectMessageAdapter functions
    'holochain',           // Needs Holochain delegate
] as const;
```

### 2. **Standardized Function Naming**
Prefix functions by capability:
- `linkSync*` for LinkSyncAdapter
- `expression*` for ExpressionAdapter
- `telepresence*` for TelepresenceAdapter
- `directMessage*` for DirectMessageAdapter

### 3. **Simplified Context**
Instead of passing everything to everyone:
```typescript
interface LanguageContext {
    agent: AgentService;           // Always available
    storageDirectory: string;      // Always available
    customSettings?: object;       // Language-specific settings
    
    // Services injected based on capabilities
    // (runtime checks what to provide)
}
```

### 4. **Async Init Optional**
```typescript
// Synchronous init for simple languages
export function init(context: LanguageContext): void;

// Or async for complex setup
export async function init(context: LanguageContext): Promise<void>;

// Or don't export init at all - stateless languages work without it
```

---

## Implementation Checklist

### Core Changes
- [ ] Update `LanguageController` to detect and support flat exports
- [ ] Add capability-based context injection
- [ ] Create adapter wrappers for backward compatibility
- [ ] Update TypeScript types in `@coasys/ad4m`

### Rust Executor Changes
- [ ] Update `Language` struct in `rust-executor/src/languages/language.rs`
- [ ] Modify JS core to handle flat exports
- [ ] Update language loading in `js_core/languages_extension.rs`

### Documentation
- [ ] Write new Language specification
- [ ] Create migration guide for existing languages
- [ ] Update skill documentation
- [ ] Create WASM language template

### Tooling
- [ ] Update test runner for new pattern
- [ ] Create language scaffolding tool
- [ ] Add linting for capability declarations

### Migration
- [ ] Migrate p-diff-sync (reference implementation)
- [ ] Migrate agent-language
- [ ] Migrate neighbourhood-language
- [ ] Help Josh update NextGraph language

---

## Relation to Test Runner

Once the language interface is refactored:
1. Languages become simpler to test (just functions)
2. Test runner can use capability declarations to know what to test
3. Mock languages easier to create for testing
4. Clearer error messages when capabilities are missing

---

## Summary

**Current**: Factory function returns nested object with adapters
- Complex for WASM
- Runtime probing for capabilities  
- Deep property access overhead

**Proposed**: Flat exports with capability declarations
- Native to WASM
- Static capability discovery
- Direct function calls
- Backward compatible during transition

This change makes AD4M languages:
1. **Easier to write** in any language that compiles to WASM
2. **Easier to understand** (flat is simpler than nested)
3. **Easier to test** (functions are testable units)
4. **Future-proof** for new backends beyond Holochain
