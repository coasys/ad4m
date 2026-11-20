# web-executor-link-test/ — agent guide

A cdylib that proves the web executor (ad4m-executor for wasm32) links. Built by
`scripts/web-executor/check.sh`: `cargo build --release -p web-executor-link-test --target
wasm32-unknown-unknown`, which also gives the Wasm size.

- `src/lib.rs`: one export, `ad4m_web_link_probe`, that opens an in-memory `Ad4mDb` and
  `SparqlStore` and builds the RPC handler map. It exists so the linker keeps ring 2; an
  export that reaches less proves less.
- `tests/`: wasm-bindgen tests of the executor as a whole crate, under Node (`check.sh`
  runs them): SQLite and Oxigraph open in memory, the handler map serves only `agent.*` and
  `perspective.*`, Prolog reads answer empty matches. The executor's own `--lib` tests do
  not build for wasm32 (its ring-2 test modules use native-only helpers), so tests that need
  the real crate on wasm32 go here.
- Native: `#![cfg(target_arch = "wasm32")]` and a wasm32-only dependency, so it is an empty
  lib with no dependencies and changes no native build.
- Not the web executor's entry crate (that is S9's `web-executor/`), and never published.
