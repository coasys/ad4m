# ad4m-ldk (Rust)

AD4M Language Development Kit — the Rust crate Language authors depend on
to build an AD4M Language into a single-file WASM ESM bundle that the
executor can load alongside JS-authored languages.

Consumers implement one of the capability traits in `src/traits.rs`
(`ExpressionCapability`, `PerspectiveSyncCapability`, ...) plus the core
`Language` trait, and then emit the top-level named exports the executor
looks for with the `ad4m_language!` macro (`src/macros.rs`).

See `bootstrap-languages/agent-language/rust-impl/` and
`bootstrap-languages/centralized-agent-language/rust-impl/` for two
end-to-end consumers — one backed by Holochain, one backed by a
centralized HTTP server.

## How a Language is loaded

The Rust ALDK → executor path, end-to-end:

1. **Build.** `cargo build --target wasm32-unknown-unknown --release` →
   `wasm-bindgen --target deno --out-dir build-deno …` → a per-language
   `inline.mjs` replaces wasm-bindgen's `fetch(new URL(...))` loader with
   a base64-inlined `WebAssembly.compile` so the final `build/bundle.js`
   is a single file. The bundle.js exposes the Language v1 top-level
   named exports the executor expects (`name`, `version`, `init`,
   `expressionCreate`, ...).

2. **Bootstrap.** `rust-executor/src/js_core/language_bootstrap.js`
   imports the bundle as an ES module and captures the module
   namespace — i.e. the exported functions — into an internal table
   keyed by language address.

3. **Host imports.** Before `init()` fires,
   `rust-executor/src/js_core/wasm_imports.ts` is executed (refcounted
   per-isolate) to install the host-import surface on `globalThis`.
   These are the symbols the WASM module linked against via
   `#[wasm_bindgen] extern "C"` in `src/imports.rs` (`__agent_did`,
   `__holochain_call`, `__signal_emit`, `__http_fetch`, ...).

4. **Capability detection.**
   `rust-executor/src/languages/language_runtime.rs::register_callbacks`
   walks the module namespace once, records which capability methods
   are actually exported, and stores the resulting capability set on
   the executor-side `Language` struct in
   `rust-executor/src/languages/language.rs`. Later calls go through
   `Language::has(Capability::X)` instead of re-probing with
   `typeof === "function"`.

5. **init().** The executor invokes the language's `init` export,
   which `ad4m_language!` expands into a wasm-bindgen wrapper around
   `YourLang::init()`. This is where Rust languages reach into
   `ad4m_ldk::imports` to register DNAs, read settings, etc. — every
   one of those calls resolves to a symbol installed in step 3.

6. **Dispatch.** Each subsequent executor → language call goes through
   a wasm-bindgen-generated export on the module namespace.
   `__AD4M_LANG_STATE` (a `RefCell` held in a `thread_local!`) gives
   each call mutable access to the `Language` value that `init()`
   constructed; the take/put-back pattern in `macros.rs` keeps the
   borrow out of any `await` frame so two concurrent async calls
   serialize cleanly.

JS-authored languages share steps 1 (esbuild bundle instead of
cargo+wasm-bindgen), 2, 4, 5, and 6 — but skip step 3 entirely and
reach the same Deno ops through the JS ALDK wrappers in
`ad4m-ldk/js/src/imports.ts`.

## Where things live

- `src/lib.rs` — crate entry, re-exports.
- `src/traits.rs` — the capability trait split.
- `src/types.rs` — `Address`, `Expression`, error types.
- `src/imports.rs` — `#[wasm_bindgen] extern "C"` declarations plus
  typed Rust wrappers. The JS-side counterpart is
  `rust-executor/src/js_core/wasm_imports.ts`.
- `src/macros.rs` — `ad4m_language!` and the per-capability
  `__ad4m_cap!` expanders. This is where dispatch shims get emitted.
- `src/state.rs` — `__AD4M_LANG_STATE` thread-local plus the take/put
  helpers the macros use.
