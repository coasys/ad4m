# web/ — agent guide

Code that exists only in the web executor (`target_arch = "wasm32"`). Declared from
`src/lib_wasm.rs` only; a native build never compiles a file here.

| File | Role |
|---|---|
| `rt.rs` | `spawn`, `spawn_blocking`, `JoinHandle`, `AbortHandle`, `JoinError`, `JoinSet` on the JS event loop (`wasm_bindgen_futures::spawn_local`). `crate::rt` re-exports it on wasm32 and tokio on native |
| `time.rs` | Clocks (`web_time`: `Instant`, `SystemTime`) and timers on `setTimeout`: `sleep`, `sleep_until`, `timeout`, `interval`, their clock `TimerInstant`. One `setTimeout` per pending sleep, cleared and freed on drop. Not wasmtimer: 0.4.5 stops every timer after 19 cleared timeouts. `crate::rt::time` re-exports it on wasm32 and std/tokio on native |
| `fs.rs` | `crate::rt::fs` on wasm32: an in-memory map from path to bytes (the agent file, profiles). Gone on reload |
| `boot.rs` | The web executor's way in: `boot(config_json)` (in-memory config, `:memory:` SQLite, local wallet, agent service, handler map, the operator context of an empty token on loopback), `dispatch(frame_json)` (one RPC frame in, its reply frame out, outside any connection: the smoke test's entry). No JS types: the `web-executor` crate makes them wasm-bindgen exports |
| `session.rs` | `connect(on_frame, on_closed) -> Socket`: one RPC connection on native's own loop (`api::rpc_session::serve`) over two channels, events from `build_event_stream` per connection, the operator context with a fresh `connection_id`. One per `WorkerSocket` |
| `eval_watchdog.rs` | `perspectives::sparql_store`'s eval watchdog as a task instead of a thread. Declared by path inside `sparql_store` (it reaches the store's private stop record) |
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

Tests: `wasm-bindgen-test` under Node in `web-executor-link-test/tests/` (the executor's own
`--lib` tests do not build for wasm32). The smoke test: `bash scripts/web-executor/build.sh &&
node tests/web-executor/smoke.mjs`.
