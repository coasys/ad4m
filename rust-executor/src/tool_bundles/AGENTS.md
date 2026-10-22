# tool_bundles/ — agent guide

JavaScript an app ships so agents can use its tools, run in an isolate that can compute and
nothing else. Not Languages: no network, storage, agent or holochain access, and never loaded
at boot.

## Files

| File | Role |
|---|---|
| `sandbox.rs` | `BundleSandbox`: a bare `JsRuntime` on its own thread (the registry keeps one per bundle and starts a fresh one per record call); `call(path, args)` / `read(path)`, JSON in and out; time limit on the load and on each call, memory cap on the V8 heap and (through the isolate's ArrayBuffer allocator) on buffers |
| `bootstrap.js` | Runs before the bundle: the only globals it gets beyond the language (`structuredClone`, `TextEncoder`, `TextDecoder`, `atob`, `btoa`, `crypto.getRandomValues`, `console`), removes `Deno` and `WebAssembly`, makes `ArrayBuffer`/`SharedArrayBuffer` fixed-length only, evaluates to the invoker |
| `store.rs` | `ToolBundleStore`: `<data>/tool_bundles/<address>.js`, addressed by `js_core::utils::content_address` |
| `registry.rs` | `ToolBundleRegistry::global()`: install (a toolset name already taken replaces its bundle, which must not drop any of that bundle's toolsets), `load_stored` at boot, `find_tool` (a `FoundTool` calls in a fresh isolate), `read_doc`. Reached by `api/tool_bundles_ws.rs`, `mcp/tools/instances/record_tools.rs` and `mcp/tools/docs.rs` |
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
- `bootstrap.js` code that runs after the bundle has loaded (the globals it defines) uses only
  intrinsics it captured before the bundle ran (`Reflect.apply`, a getter taken from its
  descriptor, ...), never a global or prototype member looked up at call time: the bundle may
  have replaced those. This matters most wherever it holds an uncounted Rust-backed buffer.
- A bundle's `console` never logs above debug, and a line is cut to 2,000 characters.
- Calls are typed: an export path and JSON arguments. Never build JS source with `format!`.
- The store refuses any address that is not one, so a caller's string never names a path.
- A bundle never touches a perspective. `call_record_tool` reads the record and writes what a
  tool answers through the class setters, in one batch, as the calling agent.
- Tool names are unique across the node; a toolset's doc topics sit under its own name and may
  not take an executor topic. Tests get a registry over a temp-dir store, never `~/.ad4m`.
- A record is seen by one call: every tool call runs in a fresh isolate that ends with it. A
  bundle runs one record call at a time (`Loaded::calls`, one permit, held by the call's own task
  until its isolate is done), so concurrent callers queue rather than each holding an isolate's
  full memory cap. The
  isolate a bundle keeps installed reads its exports and serves its docs, and is never given a
  record. Bundle docs are served to authenticated callers only (`get_documentation` checks);
  the executor's own topics stay open.
- `install` changes nothing unless every check passes and the source is stored: check, `put`,
  then replace.
- A tool's answer is written only after `validate_properties(.., WriteMode::Update)`, like
  `instance_update`; the setters do not check type or cardinality.

## Do not

- Do not give bundles state a caller relies on between calls: an isolate may be replaced.
- Do not store a bundle under the languages directory: everything there is booted as a Language.
