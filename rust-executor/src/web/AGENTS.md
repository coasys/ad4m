# web/ — agent guide

Code that exists only in the web executor (`target_arch = "wasm32"`). Declared from
`src/lib_wasm.rs` only; a native build never compiles a file here.

| File | Role |
|---|---|
| `rt.rs` | `spawn`, `spawn_blocking`, `JoinHandle`, `AbortHandle`, `JoinError` on the JS event loop (`wasm_bindgen_futures::spawn_local`). `crate::rt` re-exports it on wasm32 and tokio on native |
| `time.rs` | Clocks (`web_time`: `Instant`, `SystemTime`) and timers on `setTimeout` (`wasmtimer`: `sleep`, `sleep_until`, `timeout`, `interval`, their clock `TimerInstant`). `crate::rt::time` re-exports it on wasm32 and std/tokio on native |

Invariants:
- Same names and methods as the tokio items `crate::rt` re-exports on native, so a call
  site compiles on both targets unchanged.
- No threads: `spawn` needs no `Send`, `spawn_blocking` runs its closure at once.
- Stubs return errors and never panic.

Tests: `wasm-bindgen-test` under Node, as CI runs `ad4m-hash-chain` on wasm32:
`CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner cargo test -p ad4m-executor --lib --target wasm32-unknown-unknown`.
