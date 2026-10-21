# tool_bundles/ — agent guide

JavaScript an app ships so agents can use its tools, run in an isolate that can compute and
nothing else. Not Languages: no network, storage, agent or holochain access, and never loaded
at boot.

## Files

| File | Role |
|---|---|
| `sandbox.rs` | `BundleSandbox`: one bare `JsRuntime` per bundle on its own thread; `call(path, args)` / `read(path)`, JSON in and out; per-call time limit and heap cap |
| `bootstrap.js` | Runs before the bundle: the only globals it gets beyond the language (`structuredClone`, `TextEncoder`, `TextDecoder`, `atob`, `btoa`, `crypto.getRandomValues`, `console`), removes `Deno`, evaluates to the invoker |
| `store.rs` | `ToolBundleStore`: `<data>/tool_bundles/<address>.js`, addressed by `js_core::utils::content_address` |
| `registry.rs` | `ToolBundleRegistry::global()`: install (a toolset name already taken replaces its bundle), `load_stored` at boot, `find_tool`, `read_doc`. Reached by `api/tool_bundles_ws.rs` and `mcp/tools/instances/record_tools.rs` |
| `tests.rs` | Fixture bundles: calls, isolation, limits, store |

## Invariants

- The isolate registers one extension, `tool_bundle_host` (random bytes, a log line). What a
  bundle can do is exactly the ops registered: never add one that reaches files, the network,
  timers, the agent or a service. A bundle that needs more is asking for a Language.
- No module loader: `import` inside a bundle fails. A bundle is one self-contained file.
- No snapshot: `bootstrap.js` is executed at runtime, so changing it needs no `pnpm build`.
  Keep it pure ASCII.
- A terminated isolate (time limit, heap cap) is never reused; the next call starts a new one.
- Calls are typed: an export path and JSON arguments. Never build JS source with `format!`.
- The store refuses any address that is not one, so a caller's string never names a path.
- A bundle never touches a perspective. `call_record_tool` reads the record and writes what a
  tool answers through the class setters, in one batch, as the calling agent.
- Tool names are unique across the node; a toolset's doc topics sit under its own name and may
  not take an executor topic. Tests get a registry over a temp-dir store, never `~/.ad4m`.

## Do not

- Do not give bundles state a caller relies on between calls: an isolate may be replaced.
- Do not store a bundle under the languages directory: everything there is booted as a Language.
