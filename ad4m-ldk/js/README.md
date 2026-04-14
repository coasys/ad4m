# @coasys/ad4m-ldk (JS)

AD4M Language Development Kit — the JS/TS package Language authors import
to build an AD4M Language using `defineLanguage()` and the typed host-import
wrappers in `src/imports.ts`.

Consumers call `defineLanguage({ name, version, init, expression, ... })`
(see `src/defineLanguage.ts`) and re-export the result's flat spread so
the executor's module-namespace loader sees the top-level named exports
it needs. The bootstrap-languages under
`bootstrap-languages/*/index.ts` are all consumers of this package.

## How a JS Language is loaded

The JS ALDK → executor path, end-to-end:

1. **Build.** Each bootstrap language's `esbuild.ts` bundles
   `index.ts` into a single-file ES module at `build/bundle.js`. The
   esbuild config aliases `@coasys/ad4m-ldk` to the workspace source
   (`ad4m-ldk/js/src/index.ts`) so `defineLanguage` and the typed
   `imports.ts` wrappers get inlined into the bundle — the resulting
   file has no external imports.

2. **Bootstrap.** `rust-executor/src/js_core/language_bootstrap.js`
   imports the bundle as an ES module and captures the module
   namespace — i.e. the exported functions — into an internal table
   keyed by language address.

3. **No WASM host imports.** Unlike Rust-authored languages, a JS
   language does NOT need `wasm_imports.ts` to install a
   wasm-bindgen extern surface on `globalThis`. The JS ALDK wrappers
   in `src/imports.ts` (`agentDid()`, `holochainCall()`,
   `emitPerspectiveDiff()`, ...) reach the same Deno ops as the
   WASM bridge — just via `globalThis.AGENT`,
   `globalThis.LANGUAGE_CONTROLLER`, and
   `globalThis.__holochainDelegate__`, which the executor's
   extension JS installs for every isolate.

4. **Capability detection.**
   `rust-executor/src/languages/language_runtime.rs::register_callbacks`
   walks the captured module namespace once, records which capability
   methods are actually exported, and stores the resulting capability
   set on the executor-side `Language` struct. Later calls go through
   `Language::has(Capability::X)` instead of re-probing with
   `typeof === "function"`.

5. **init().** The executor invokes the language's `init` export.
   This is where JS languages reach into the JS ALDK `imports`
   wrappers (e.g. to read settings or register DNAs) — the wrappers
   resolve to the globals from step 3.

6. **Dispatch.** Each subsequent executor → language call goes
   through the same top-level named exports from the module
   namespace — `commit`, `currentRevision`, `expressionCreate`,
   `render`, etc. A Rust-authored language produces the same set
   of exports through the `ad4m_language!` macro; the executor
   doesn't care which side generated them.

## Where things live

- `src/index.ts` — package entry, re-exports.
- `src/defineLanguage.ts` — the `defineLanguage()` helper and the
  flat-export mapping the executor expects.
- `src/imports.ts` — typed wrappers over the
  `globalThis.AGENT` / `LANGUAGE_CONTROLLER` /
  `__holochainDelegate__` surfaces the executor exposes.
- `src/types.ts` — shared types (`Address`, `Expression`, capability
  interfaces).
- `src/errors.ts` — error helpers that bridge to the executor's
  `LanguageError` variants.
