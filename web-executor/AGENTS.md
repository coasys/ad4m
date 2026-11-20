# web-executor/ — agent guide

The web executor's entry crate: `ad4m-executor` built for wasm32 behind three wasm-bindgen
exports. The core side is `rust-executor/src/web/boot.rs`; this crate only turns it into JS
calls and sends the core's `log` records to the console.

- `src/lib.rs`: `boot(config_json, log_level?)`, `dispatch(frame_json) -> Promise<string>`
  (one RPC frame `{id, type, params}` in, `{id, result}` / `{id, error}` out, never rejects),
  `on_event(callback)` (every published event as its socket frame `{type, ...payload}`).
- Build: `bash scripts/web-executor/build.sh [node|web]` (release, wasm-bindgen CLI of the
  lockfile's version, `wasm-opt -Oz` when installed). Glue in `pkg/` (not committed).
- Smoke: `node tests/web-executor/smoke.mjs` (six calls, `6/6 ok`). `SMOKE_IDLE_MS=10000`
  counts timer wake-ups while idle.
- In memory only: a reload starts with no agent and no perspectives. The page is the
  operator (no token, decision 6): any script on the page can call anything.
- Native: `#![cfg(target_arch = "wasm32")]` and wasm32-only dependencies, so an empty lib.
- Do not put JS types into `rust-executor`: they stay here and in `src/web/`.
