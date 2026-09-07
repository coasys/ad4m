# mcp/ — agent guide

Model Context Protocol server exposing AD4M to LLM agents (`rmcp`). Transport +
auth in `server.rs`; `Ad4mMcpHandler` in `tools/mod.rs` implements `list_tools` /
`call_tool`.

## Tool files (`tools/`)

| File | Tools |
|---|---|
| `mod.rs` | `Ad4mMcpHandler`, dispatch, perspective access helpers (`get_readable_perspective`, `get_writable_perspective`), SHACL link helpers |
| `auth.rs` | login / capability request flow |
| `perspectives.rs` | perspective + link CRUD, `infer` (Prolog; returns empty while Prolog disabled) |
| `subjects.rs` | static subject CRUD/property/collection tools. **Duplicates `dynamic.rs` and has diverged** (spec item 2: make these delegate) |
| `dynamic.rs` | Generates one tool per SHACL class (`query_<Class>`, `create_<Class>`, …) from `mcp/shacl.rs`; the canonical implementation of subject ops |
| `children.rs` | parent/child relationship tools |
| `flows.rs` | SHACLFlow state/transition tools |
| `subscriptions.rs` | query subscriptions / wakers |
| `neighbourhoods.rs`, `profiles.rs`, `languages.rs` | publish/join neighbourhoods, agent profiles, installed languages |
| `harness_bridge.rs`, `provider_impl.rs`, `side_effects.rs` | Adapters that expose MCP tools to the interpretation harness (`ai_service::harness::ToolProvider`) and classify their side effects. Moving to `agentic/` (spec item 7) |

`shacl.rs`: `ShaclClass`/`ShaclProperty` projections of `ModelShape` for tool
schemas. `load_class` uses the instance shape cache; `load_class_properties_with_uri`
is a second, raw-link parser slated for deletion.

## Rules

- MCP does **not** go through `api/` handlers; it calls `PerspectiveInstance` directly.
  Keep semantics aligned with the WS handlers (same capability, same `AgentContext`).
- Permission denial returns a 404-shaped JSON string (not 403) by design: don't leak
  perspective existence.
- Multi-step writes must use `create_batch` … `commit_batch` so partial state is never
  visible (the static `set_subject_property` currently violates this).
- New subject-level behaviour goes in `dynamic.rs` only; static tools wrap it.
