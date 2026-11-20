# web/ — agent guide

Code that exists only in the web executor (`target_arch = "wasm32"`). Declared from
`src/lib_wasm.rs` only; a native build never compiles a file here.

| File | Role |
|---|---|
| `rt.rs` | `spawn`, `spawn_blocking`, `JoinHandle`, `AbortHandle`, `JoinError` on the JS event loop (`wasm_bindgen_futures::spawn_local`). `crate::rt` re-exports it on wasm32 and tokio on native |
| `time.rs` | Clocks (`web_time`: `Instant`, `SystemTime`) and timers on `setTimeout` (`wasmtimer`: `sleep`, `sleep_until`, `timeout`, `interval`, their clock `TimerInstant`). `crate::rt::time` re-exports it on wasm32 and std/tokio on native |
| `scryer_types.rs` | scryer-prolog's `Term` and `LeafAnswer`, same shape. `lib_wasm.rs` names this crate `scryer_prolog` (`extern crate self as scryer_prolog`) and re-exports both at its root, so ring 2's `use scryer_prolog::Term;` compiles unchanged |
| `stubs/` | The modules outside ring 2, hand-written: only the items ring 2 names, each returning `stubs::NotAvailable` or native's "absent" answer. Declared by path in `lib_wasm.rs` (and two in `perspectives/mod.rs`). See `stubs/AGENTS.md` |
| `prolog_service/` | `crate::prolog_service` without scryer: the items ring 2 names, same signatures as native, `PROLOG_MODE` `Disabled`; every query is an error ("Prolog is not available in the web executor"). Declared by path in `lib_wasm.rs` as `crate::prolog_service`. `python3 -I scripts/web-executor/check-prolog-stub.py` checks that every `prolog_service::` path ring 2 uses is here |

Invariants:
- Same names and methods as the tokio items `crate::rt` re-exports on native, so a call
  site compiles on both targets unchanged.
- No threads: `spawn` needs no `Send`, `spawn_blocking` runs its closure at once.
- Stubs return errors and never panic.

Check: `bash scripts/web-executor/check.sh` (repo root) prints `wasm32 check: <n> errors`,
`wasm32 link: ok` and the Wasm size.

Tests: `wasm-bindgen-test` under Node, as CI runs `ad4m-hash-chain` on wasm32:
`CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner cargo test -p ad4m-executor --lib --target wasm32-unknown-unknown`.
