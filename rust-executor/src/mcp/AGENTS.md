# mcp/ — agent guide

Model Context Protocol server exposing AD4M to LLM agents (`rmcp`). Transport in
`server.rs`; `Ad4mMcpHandler` in `tools/mod.rs` implements `list_tools` /
`call_tool`.

## Auth: one `Session` per MCP session (`caller.rs`)

- Each MCP session has a `Caller` (in `McpContext`), which holds the
  `agent::session::Session` its token opened with `Session::open` — the same
  function RPC uses. The token comes from the `Authorization` header (`Bearer x`
  or raw `x`) or from a login tool (`login_email`, `verify_email_code`,
  `generate_jwt` → `store_token_and_respond`). A remembered session wins over a
  later header; a header replaces a session that stopped working.
- No token: `Session::open("")` again on every call — the operator on a loopback
  listener of a node without an admin credential, anonymous otherwise. The
  listener's `ListenerReach` comes from the bound socket (`mcp_router` adds it as
  an extension); a request with a forwarding header reads as `Network`, as on RPC.
- `call_tool` resolves the caller on every call and refuses non-`AUTH_TOOLS`
  calls unless the session works and is not anonymous. Tools read it through
  `session()` / `require(cap)` / `get_agent_context()`, which run
  `Session::check` again, so a revoked, expired or logged-out token is refused
  mid-session. The in-process harness (`harness_bridge.rs`) skips `call_tool`, so
  these helpers are the check there.
- Never resolve tokens in a tool (`decode_jwt`, `capabilities_on`,
  `AgentContext::from_auth_token`): ask the `Session`.
- `auth_status` reports the same resolved session (principal, did, capabilities,
  or the `AuthError`), so `authenticated` is true exactly when a tool call passes.

## Tool files (`tools/`)

| File | Tools |
|---|---|
| `mod.rs` | `Ad4mMcpHandler`, dispatch, perspective access helpers (`get_readable_perspective`, `get_writable_perspective`), SHACL link helpers |
| `auth.rs` | login / capability request flow |
| `perspectives.rs` | perspective + link CRUD |
| `instances/` | the static instance surface: `instance_query` / `_get` / `_create` / `_update` / `_remove`, `instance_add_to_collection` / `_remove_from_collection`, `instance_transcript`, child links, and `describe_perspective`. Class-agnostic — the class is a parameter, not a tool name. Also `execute_commands` (`commands.rs`), the deliberate raw-SDNA escape hatch: it runs actions directly and does **not** go through the class's SHACL validation, so it is the one tool here that can write a shape the model would reject |
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
  Keep semantics aligned with the WS handlers (same capability, same `Session::acts_as`).
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
