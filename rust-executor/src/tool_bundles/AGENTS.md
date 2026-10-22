# tool_bundles/ — agent guide

JavaScript an app ships so agents can use its tools, run in an isolate that can compute and
nothing else. Not Languages: no network, storage, agent or holochain access, and never loaded
at boot.

## Files

| File | Role |
|---|---|
| `sandbox.rs` | `BundleSandbox`: one bare `JsRuntime` per bundle on its own thread; `call(path, args)` / `read(path)`, JSON in and out; time limit on the load and on each call, memory cap on the V8 heap and (through the isolate's ArrayBuffer allocator) on buffers |
| `bootstrap.js` | Runs before the bundle: the only globals it gets beyond the language (`structuredClone`, `TextEncoder`, `TextDecoder`, `atob`, `btoa`, `crypto.getRandomValues`, `console`), removes `Deno` and `WebAssembly`, makes `ArrayBuffer`/`SharedArrayBuffer` fixed-length only, evaluates to the invoker |
| `store.rs` | `ToolBundleStore`: `<data>/tool_bundles/<address>.js`, addressed by `js_core::utils::content_address` |
| `tests.rs` | Fixture bundles: calls, isolation, limits, store |

## Invariants

- The isolate registers one extension, `tool_bundle_host` (random bytes, a log line). What a
  bundle can do is exactly the ops registered: never add one that reaches files, the network,
  timers, the agent or a service. A bundle that needs more is asking for a Language.
- No module loader: `import` inside a bundle fails. A bundle is one self-contained file.
- No snapshot: `bootstrap.js` is executed at runtime, so changing it needs no `pnpm build`.
  Keep it pure ASCII.
- A terminated isolate (time limit, memory cap) is never reused; the next call starts a new one.
- No bundle code runs without a time limit: the isolate's handle is published before the bundle
  is evaluated, loading has the call time limit, and a call runs as a task of its own so its limit
  holds when the caller is dropped. A start that is given up on terminates its isolate.
- Every byte a bundle can hold is counted by the heap limit or by the allocator's budget. Memory
  V8 reserves on its own (`WebAssembly.Memory`, resizable buffers) is unreachable from a bundle,
  and memory an op allocates in Rust is copied into a counted buffer before a bundle gets it
  (`TextEncoder.encode`). A new global or op that hands out a buffer must keep this true; the
  memory-cap tests in `tests.rs` pin each route.
- A bundle's `console` never logs above debug, and a line is cut to 2,000 characters.
- Calls are typed: an export path and JSON arguments. Never build JS source with `format!`.
- The store refuses any address that is not one, so a caller's string never names a path.

## Do not

- Do not give bundles state a caller relies on between calls: an isolate may be replaced.
- Do not store a bundle under the languages directory: everything there is booted as a Language.
