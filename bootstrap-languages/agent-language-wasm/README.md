# Agent Language WASM

WASM implementation of the agent-expression-store language for AD4M, demonstrating the new flat export pattern.

## Architecture

This language is implemented in Rust and compiled to WebAssembly, using the flat export pattern:

```typescript
// Instead of returning nested adapters:
export default function create() {
  return { expressionAdapter: { get: ..., putAdapter: ... } }
}

// We export flat functions:
export const capabilities = ['expression-storage'];
export async function expression_get(address) { ... }
export async function expression_create_public(content) { ... }
```

## Building

```bash
# Install wasm-pack if not already installed
cargo install wasm-pack

# Build the WASM module
wasm-pack build --target web --out-dir pkg

# Or use npm script
npm run build
```

## Usage in AD4M

```javascript
import * as agentLanguage from '@coasys/agent-language-wasm';

// Check capabilities
console.log(agentLanguage.capabilities); // ['expression-storage']

// Initialize
await agentLanguage.init({
  agent: { did: 'did:key:z6Mk...' },
  storageDirectory: '/path/to/storage'
});

// Create agent expression
const agent = {
  did: 'did:key:z6Mk...',
  perspective: { links: [] },
  directMessageLanguage: 'literal://...'
};
const address = await agentLanguage.expression_create_public(agent);

// Retrieve
const expression = await agentLanguage.expression_get(address);

// Cleanup
agentLanguage.teardown();
```

## Testing

This language can be tested with `@coasys/ad4m-test`:

```bash
# Build first
npm run build

# Run tests
ad4m-test --test ./test.js --bundle ./index.js --meta '{"name":"agent-expression-store-wasm","description":"WASM agent language","possibleTemplateParams":[]}'
```

## Differences from Original

| Aspect | Original (TypeScript) | WASM Version |
|--------|----------------------|--------------|
| Storage | Holochain DNA | In-memory (for demo) |
| Pattern | Nested adapters | Flat exports |
| Bundle size | ~50KB (depends on Holochain) | ~140KB (WASM + JS) |
| Crypto | Holochain keys | Mock signatures (for demo) |

## Future Work

- Replace in-memory storage with Holochain integration via FFI
- Implement real cryptographic signatures
- Add persistence to filesystem

## Flat Export Benefits

1. **WASM-friendly**: Functions are natural WASM exports
2. **Static analysis**: Capabilities array tells runtime what's available
3. **Simpler**: No nested object construction
4. **Type-safe**: Easier to generate TypeScript definitions
