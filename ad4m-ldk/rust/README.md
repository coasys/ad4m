# ad4m-ldk

**AD4M Language Development Kit — Rust, compiled to WebAssembly.**

[AD4M](https://ad4m.dev) is an agent-centric runtime for decentralized
applications. In AD4M, every storage backend and every communication
protocol is a pluggable **Language** — a small module with a
well-defined interface, loaded into a sandboxed isolate by the AD4M
executor. This crate is the SDK for authoring AD4M Languages in Rust
and compiling them to a single-file WASM bundle the executor can load
alongside Languages written in TypeScript.

- Full conceptual overview: **[docs.ad4m.dev/languages](https://docs.ad4m.dev/languages)**
- Normative interface spec (WIT):
  [`ad4m-lang.wit`](https://github.com/coasys/ad4m/blob/dev/docs-src/ad4m-lang.wit)
- Prose spec:
  [`language-interface-spec.md`](https://github.com/coasys/ad4m/blob/dev/docs-src/language-interface-spec.md)

## Add to your crate

```toml
[package]
name = "my-ad4m-language"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
ad4m-ldk = "0.13"
```

Build targets WASM via `wasm-bindgen`:

```sh
cargo build --target wasm32-unknown-unknown --release
wasm-bindgen --target deno --out-dir build \
    target/wasm32-unknown-unknown/release/my_ad4m_language.wasm
```

The AD4M repo's `bootstrap-languages/agent-language/rust-impl/` has a
worked `build.sh` you can copy.

## Minimal example

```rust
use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

pub struct NoteStore;

impl Language for NoteStore {
    fn name()    -> &'static str { "note-store" }
    fn version() -> &'static str { "1.0.0" }

    async fn init() -> LanguageResult<Self> {
        Ok(NoteStore)
    }
}

impl ExpressionCapability for NoteStore {
    async fn expression_create(
        &mut self,
        content: serde_json::Value,
    ) -> LanguageResult<Address> {
        let signed = rt::agent_create_signed_expression_typed(&content);
        let body = rt::http_post_json("https://example.com/notes", &signed)
            .await
            .map_err(|e| LanguageError::internal(format!("POST failed: {:?}", e)))?;
        Ok(body)
    }

    async fn expression_get(
        &mut self,
        address: Address,
    ) -> LanguageResult<Option<Expression>> {
        let body = rt::http_get(&format!("https://example.com/notes/{}", address))
            .await
            .map_err(|e| LanguageError::internal(format!("GET failed: {:?}", e)))?;
        Ok(serde_json::from_str(&body).ok())
    }
}

// Wires up wasm-bindgen exports for exactly the capabilities listed.
ad4m_language! {
    language: NoteStore,
    capabilities: [expression],
    holochain_signal: false,
}
```

The `ad4m_language!` macro expands into the wasm-bindgen `extern`s the
AD4M executor looks for — one per capability method. The WASM export
table ends up minimal: the executor introspects it at load time and
caches the capability set, so what you export is what the runtime
thinks your Language can do.

## What this crate gives you

- **Capability traits** (`Language`, `ExpressionCapability`,
  `PerspectiveCommitCapability`, `PerspectiveSyncCapability`,
  `PerspectiveQueryCapability`, `PeersCapability`,
  `TelepresenceCapability`, `LanguageSourceCapability`,
  `HolochainSignalHandler`). Implement only the ones your Language
  actually supports.
- **Typed Rust wrappers** around every `ad4m:host` import
  (`rt::agent_did()`, `rt::holochain_call()`, `rt::storage_get()`,
  `rt::http_post_json()`, `rt::emit_perspective_diff_typed()`, …).
  The typed variants route serialization through a maps-as-objects
  serializer so structured payloads actually make it across the
  wasm-bindgen boundary — don't use raw `serde_wasm_bindgen::to_value`.
- **`ad4m_language!{}` macro** — emits the wasm-bindgen exports for
  the capabilities you list, plus a thread-local instance holder that
  handles re-entrant async calls correctly.
- **`LanguageError` / `LanguageResult`** — error types that map
  cleanly onto the executor-side error taxonomy.

See `src/lib.rs` for the public surface.

## Optional extensions

Beyond the core interface, runtimes may ship optional extensions. Your
Language can use them, but the runtime might not provide them — the
Rust wrappers return `Result` so you handle the missing-extension case
explicitly.

- **Holochain** — `rt::holochain_register_dnas()`, `rt::holochain_call()`,
  plus the `HolochainSignalHandler` trait for receiving DNA signals.
  Declare your Language in the `ad4m-language-holochain` WIT world.
- **Storage File I/O** — `rt::read_storage_file()`,
  `rt::write_storage_file()`. Raw path-based read/write. Prefer the
  core KV (`rt::storage_get()` / `rt::storage_put()`) unless you
  specifically need filesystem-like semantics.

## Reference Languages

The two end-to-end Rust Languages in the AD4M repo are the best
worked examples:

- [`agent-language`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/agent-language/rust-impl)
  — Holochain-backed agent profile store.
- [`centralized-agent-language`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/centralized-agent-language/rust-impl)
  — HTTP-backed agent profile store. The simpler of the two; good
  starting point.

Both ship as real bootstrap Languages inside the AD4M executor and
are exercised by the multi-user integration tests.

## Related

- [`@coasys/ad4m-ldk`](https://www.npmjs.com/package/@coasys/ad4m-ldk)
  — JS/TypeScript ALDK. Same runtime interface, different authoring
  language.
- [AD4M repo](https://github.com/coasys/ad4m) — executor, spec,
  bootstrap Languages.

## License

CAL-1.0. Same as AD4M.
