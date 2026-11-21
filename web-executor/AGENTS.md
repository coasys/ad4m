# web-executor/ — agent guide

The web executor's entry crate: `ad4m-executor` built for wasm32 behind wasm-bindgen exports,
and `js/`, which runs it in a Worker behind a WebSocket the unchanged `Ad4mClient` takes. The
core side is `rust-executor/src/web/` (`boot.rs`, `session.rs`); this crate only turns it into
JS calls and sends the core's `log` records to the console.

- `src/lib.rs`: `boot(config_json, log_level?)`; `connect(on_frame, on_closed) -> Connection`
  (`send(frame)`, `close()`): one RPC connection on the core's own loop (`api/rpc_session.rs`),
  so `ping`, `request.cancel`, `events.watch` and live queries behave as on the native socket;
  `dispatch(frame_json) -> Promise<string>` (one frame outside any connection, for the smoke
  test); `on_panic(callback)` (the panic message, before the module aborts); `panic_for_test`.
- `js/` (erasable TypeScript: Node runs it as it is; a bundler builds the browser side):
  `WorkerSocket.ts` (`WorkerHost`, and the WebSocket surface `ApiClient` uses: each socket is
  one connection), `worker.ts` (the Worker's side), `node.ts` + `node-worker.ts`
  (`worker_threads`), `browser.ts` + `browser-worker.ts` (a module Worker), `protocol.ts`
  (the messages). `new Ad4mClient(url, '', { webSocketImpl: host.WorkerSocket })`.
- A panic aborts the module: the Worker catches the `RuntimeError`, posts `dead` with the panic
  message, and every socket closes 1011 with it; later sockets close at once the same way.
- Build: `bash scripts/web-executor/build.sh [node|web]` (release, wasm-bindgen CLI of the
  lockfile's version, `wasm-opt -Oz` when installed). Glue in `pkg/` (not committed).
- Tests (after `build.sh` and `cd core && pnpm run build`): `node tests/web-executor/smoke.mjs`
  (six calls, `6/6 ok`; `SMOKE_IDLE_MS=10000` counts timer wake-ups),
  `client.test.mjs` (`Ad4mClient` through the Worker, `5 passed`), `worker-death.test.mjs`.
- In memory only: a reload starts with no agent and no perspectives. The page is the
  operator (no token, decision 6): every socket is the operator; any script on the page can
  call anything.
- Native: `#![cfg(target_arch = "wasm32")]` and wasm32-only dependencies, so an empty lib.
- Do not put JS types into `rust-executor`: they stay here and in `src/web/`.
