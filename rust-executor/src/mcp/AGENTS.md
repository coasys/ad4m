# mcp/ — agent guide

Model Context Protocol server exposing AD4M to LLM agents (`rmcp`). Transport +
auth in `server.rs`; `Ad4mMcpHandler` in `tools/mod.rs` implements `list_tools` /
`call_tool`.

## Tool files (`tools/`)

| File | Tools |
|---|---|
| `mod.rs` | `Ad4mMcpHandler`, dispatch, perspective access helpers (`get_readable_perspective`, `get_writable_perspective`), SHACL link helpers |
| `auth.rs` | login / capability request flow |
| `perspectives.rs` | perspective + link CRUD |
| `instances/` | the static instance surface: `instance_query` / `_get` / `_create` / `_update` / `_remove`, `instance_add_to_collection` / `_remove_from_collection`, `instance_transcript`, child links, and `describe_perspective`. Class-agnostic — the class is a parameter, not a tool name |
| `dynamic.rs` | Generates one tool per SHACL class (`query_<Class>`, `create_<Class>`, …) from `mcp/shacl.rs`. Exposed over MCP **only** with `dynamicClassTools` (off by default); always merged into the in-process harness surface |
| `docs.rs` | `get_documentation` — the executor's own agent docs, one `DocTopic` per markdown file, compiled in via `include_str!` |
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
  visible.
- **New instance-level behaviour goes in `instances/` — `dynamic.rs` depends on it,
  not the reverse** (`dynamic.rs` calls `instances::generate_instance_uri`; nothing
  under `instances/` references `dynamic`). The static surface is the default one
  agents see, so a behaviour added only to `dynamic.rs` is invisible unless the
  operator opted into `dynamicClassTools`.
- Adding a doc topic means three edits that must stay in step: the markdown file,
  the `DocTopic` variant (+ `ALL`), and the plugin's `DocTopic` schema in
  `plugins/ad4m/staticToolDefs.ts`. `docs_only_point_at_served_topics` and
  `get_documentation_serves_every_topic` fail if you miss one.
