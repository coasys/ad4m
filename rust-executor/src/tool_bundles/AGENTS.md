# tool_bundles/ — agent guide

JavaScript the executor runs for its agents' tools, in an isolate that can compute and nothing
else. Not Languages: no network, storage, agent or holochain access.

## Files

| File | Role |
|---|---|
| `sandbox.rs` | `BundleSandbox`: one bare `JsRuntime` per bundle on its own thread; `call(path, args)` / `read(path)`, JSON in and out; time limit on the load and on each call, memory cap on the V8 heap and (through the isolate's ArrayBuffer allocator) on buffers |
| `bootstrap.js` | Runs before the bundle: the only globals it gets beyond the language (`structuredClone`, `TextEncoder`, `TextDecoder`, `atob`, `btoa`, `crypto.getRandomValues`, `console`), removes `Deno` and `WebAssembly`, makes `ArrayBuffer`/`SharedArrayBuffer` fixed-length only, evaluates to the invoker |
| `builtin.rs` | WE's template tools, compiled in: `template_tools.bundle.js` (WE's build of `@we/agent-tools`) and `template_tools.json` (its tools as it declares them, and the WE commit). `call` runs a tool in a fresh isolate; `read_doc` serves the `template` topics from one kept isolate. Both files are written by `scripts/vendor-template-tools.sh`, never by hand; a test fails when they disagree |
| `tests.rs` | Fixture bundles: calls, isolation, limits |

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

- A bundle never touches a perspective; `mcp/tools/instances/template_tools.rs` reads and writes.
- A record is seen by one call: every tool call runs in a fresh isolate that ends with it.
- Template tool calls run one at a time (`builtin::CALLS_AT_ONCE`): each isolate has the whole
  memory cap, so concurrent callers queue instead of each starting one. The slot is held on the
  call's own task until its isolate is done, so a caller that gives up does not free it early.

## Do not

- Do not give bundles state a caller relies on between calls: an isolate may be replaced.
