# WASM Language Runtime for AD4M

This module enables AD4M language modules to be compiled to WebAssembly and executed in the Wasmer runtime, sharing the same WASM engine that Holochain already uses.

## Architecture

```
┌─────────────────────────────────────────┐
│              AD4M Executor              │
│  ┌────────────────────────────────────┐  │
│  │  Wasmer Runtime (shared)          │  │
│  │  ┌──────────┐  ┌───────────────┐  │  │
│  │  │ Language  │  │  Holochain    │  │  │
│  │  │ WASM      │  │  DNA WASM     │  │  │
│  │  │ modules   │  │  modules      │  │  │
│  │  └──────────┘  └───────────────┘  │  │
│  └────────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

## Components

### `rust-executor/src/wasm_core/`
- **`mod.rs`** — WASM language loader, instance management, host function implementations
- **`abi.rs`** — ABI type definitions, version constants, serialisation helpers
- **`error.rs`** — Error types for WASM operations
- **`tests.rs`** — Integration tests

### `wasm-language-sdk/`
Rust crate for language authors. Provides:
- Types: `Expression`, `Link`, `LinkExpression`, `Interaction`, etc.
- Traits: `ExpressionLanguage`, `LinkLanguage`, `LanguageInteractions`, `LanguageTeardown`
- `ad4m_language!` macro that generates all WASM exports
- Host function bindings: `agent_did()`, `create_signed_expression()`, `hash()`, `log()`, etc.
- Memory management: `alloc`/`dealloc` implementations

### `examples/wasm-languages/note-store/`
Port of `tests/js/languages/note-store/` to Rust. Demonstrates:
- Implementing `ExpressionLanguage` trait
- Using host functions for signing and hashing
- In-memory expression storage

## Building

### Enable the feature
```bash
cargo check --features wasm-languages
```

### Build the example language
```bash
cd examples/wasm-languages/note-store
cargo build --target wasm32-unknown-unknown --release
```

The WASM binary will be at `target/wasm32-unknown-unknown/release/note_store_wasm.wasm` (~119KB).

## ABI Specification

### Version
- Current: `AD4M_LANGUAGE_ABI_VERSION = 1`
- Host checks version on load and rejects incompatible modules

### Memory Protocol
Data is passed across the WASM boundary using a **fat pointer** encoding:
- A `u64` value encodes `(ptr: u32, len: u32)` — upper 32 bits = pointer, lower 32 bits = length
- Guest exports `ad4m_alloc(size: u32) -> u32` and `ad4m_dealloc(ptr: u32, size: u32)`
- All structured data is serialised as JSON (UTF-8)

### Required Exports
| Export | Signature | Description |
|---|---|---|
| `ad4m_abi_version` | `() -> u32` | Returns the ABI version |
| `ad4m_alloc` | `(u32) -> u32` | Allocate memory |
| `ad4m_dealloc` | `(u32, u32) -> ()` | Free memory |
| `ad4m_language_name` | `() -> u64` | Returns fat ptr to name string |
| `memory` | (exported memory) | Linear memory |

### Optional Exports
| Export | Signature | Description |
|---|---|---|
| `ad4m_expression_get` | `(u32, u32) -> u64` | Get expression by address |
| `ad4m_expression_put` | `(u32, u32) -> u64` | Create expression |
| `ad4m_link_add` | `(u32, u32) -> u64` | Add link |
| `ad4m_link_remove` | `(u32, u32) -> ()` | Remove link |
| `ad4m_link_get_links` | `(u32, u32) -> u64` | Query links |
| `ad4m_interactions` | `(u32, u32) -> u64` | Get interactions |
| `ad4m_teardown` | `() -> ()` | Cleanup |
| `ad4m_is_immutable_expression` | `(u32, u32) -> u32` | Check immutability |

### Host Functions (imports from "ad4m" module)
| Import | Signature | Description |
|---|---|---|
| `agent_did` | `() -> u64` | Get agent DID |
| `agent_sign` | `(u32, u32) -> u64` | Sign data |
| `agent_verify` | `(u32, u32) -> u64` | Verify signature |
| `agent_create_signed_expression` | `(u32, u32) -> u64` | Create signed expression |
| `log_message` | `(u32, u32) -> ()` | Log a message |
| `hash` | `(u32, u32) -> u64` | Compute content hash |
| `hc_call` | `(u32, u32) -> u64` | Call Holochain zome |
| `perspective_diff_received` | `(u32, u32) -> ()` | Notify of perspective diff |
| `sync_state_changed` | `(u32, u32) -> ()` | Notify of sync state change |

## Writing a WASM Language

```rust
use ad4m_wasm_language_sdk::prelude::*;
use ad4m_wasm_language_sdk::ad4m_language;

#[derive(Default)]
struct MyLanguage {
    // state
}

impl ExpressionLanguage for MyLanguage {
    fn get(&mut self, address: &str) -> Option<Expression> {
        // look up expression
        None
    }
    fn put(&mut self, content: &serde_json::Value) -> String {
        let expr = create_signed_expression(content).unwrap();
        let json = serde_json::to_string(&expr).unwrap();
        hash(&json).unwrap_or_default()
    }
}

impl LanguageInteractions for MyLanguage {
    fn interactions(&self, _addr: &str) -> Vec<Interaction> { vec![] }
}

ad4m_language!(MyLanguage, "my-language");
```

Compile with:
```bash
cargo build --target wasm32-unknown-unknown --release
```

## Language Metadata

WASM languages declare their runtime in language metadata:
```json
{
  "name": "my-language",
  "runtime": "wasm",
  "bundlePath": "language.wasm"
}
```

The executor detects `"runtime": "wasm"` and routes to the WASM loader instead of Deno.
