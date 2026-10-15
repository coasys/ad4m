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
  listener of an executor without an admin credential, anonymous otherwise. The
  listener's `ListenerReach` comes from the bound socket (`mcp_router` adds it as
  an extension); a request with a forwarding header reads as `Network`, as on RPC.
- `call_tool` resolves the caller on every call and refuses non-`AUTH_TOOLS`
  calls unless the session works and is not anonymous. Tools read it through
  `session()` / `get_agent_context()` and the perspective helpers, which run
  `Session::check` again, so a token whose grant was revoked or removed (logout,
  `revokeToken`, `removeApp`) or that expired is refused mid-session. The in-process harness (`harness_bridge.rs`) skips `call_tool`, so
  these helpers are the check there.
- Never resolve tokens in a tool (`decode_jwt`): ask the `Session`. A JWT acts as its
  grant's agent (`Principal::Agent`); `auth_status` names a grant on the main agent
  `"app"` and one on a managed agent `"user"` on the wire, and a token without an active
  grant "no active grant".
- An interpretation pass holds a session instead of a token (`McpContext::with_session`):
  the caller's RPC session, or for a pass without a caller (the auto-processor's) an
  in-process session (`Session::in_process`, no token, no grant) acting as the
  perspective's owner — the user who owns it, otherwise the main agent — and either way
  limited to querying that perspective. Never the operator
  (`perspectives::interpretation::run::pass_mcp_context`). The pass is offered the read
  tools generated for its offered classes only, matched by exact name
  (`Ad4mMcpHandler::class_tool_names`), never a static tool.

## Tools that wrap an RPC method dispatch through the handler map

A tool that does the job of an RPC method calls it with
`call::<T>(method, params, failed)` (`tools/mod.rs`), which wraps `call_method`, decodes the
result into the contract's type, answers a refusal (401, 403, 404) as `method_error` and any
other failure as `failed` words it (`labelled`, `labelled_json`, or `method_error`), so each
tool keeps its own output shape without its own match arms. `call_method`: the shared handler map (`api::ws_handler::shared_handler_map`) runs the
method's contract check, its guard and its handler with a `RequestContext` built from the
caller's `Session` (`request_context`). The tool has no capability or ownership check of its
own. A tool that does its own work before the call (cloning a link language, resolving a flow
name, loading the file-storage language) runs `admit_method` first, so a refused caller
causes no side effect.

| Tool | RPC method |
|---|---|
| `list_perspectives`, `add_perspective` | `perspective.all`, `perspective.create` |
| `add_link`, `add_child` | `perspective.addLink` |
| `query_links`, `get_children`, `get_flows` | `perspective.queryLinks` |
| `add_model`, `add_flow` | `perspective.addSdna` |
| `execute_commands` | `perspective.executeCommands` |
| `flow_proposal_accept`, `flow_proposal_reject`, `flow_valid_outputs` | `perspective.acceptFlowProposal`, `.rejectFlowProposal`, `.flowValidOutputs` |
| `language_meta` | `language.meta` |
| `list_link_language_templates` | `runtime.linkLanguageTemplates` |
| `neighbourhood_publish_from_perspective`, `neighbourhood_join_from_url` | `neighbourhood.publish`, `neighbourhood.join` |
| `get_my_did`, `get_agent_profile` | `agent.get` |
| `get_agent_public_perspective` | `agent.byDid` (a DID), `agent.get` (none) |
| `set_agent_profile`, `set_agent_public_perspective` | `agent.get` + `agent.updateProfile` |
| `set_agent_profile_picture` | `expression.create` + `agent.get` + `agent.updateProfile` |
| `login_email`, `signup`, `request_login_verification`, `verify_email_code` | `user.login`, `user.create`, `user.requestVerification`, `user.verifyEmail` |
| `request_capability`, `generate_jwt` | `agent.requestCapability` + `agent.permitCapability`, `agent.generateJwt` |

- The login tools and the capability request run as a caller without a token
  (`call_without_token` in `auth.rs`): they obtain the session that replaces the MCP
  session's one. `agent.permitCapability` (the inline mint code) runs as the caller.
- Refusals answer `{"error": .., "status": <code>}` (`method_error`). A missing perspective
  and one that is not the caller's both answer 404 from the guard itself (D6); MCP remaps
  nothing.
- `api/tests/method_matrix_mcp_tests.rs` runs every wrapping tool as each method-matrix
  caller on each resource (`World::id(owner, target)`) and checks that it answers the status
  of its methods' guards, and that its first method answers the row's
  `Expect::{Granted, Forbidden, NotFound}`. Per-tool tests do not restate those rows; they
  pin only what the matrix does not (guard order before side effects, capability scoped to
  another perspective, the operator's list, functional round trips). A new tool goes into
  its `tools()` list or its `NOT_WRAPPED` list; `every_tool_is_classified` fails otherwise.
- Tools with no RPC method keep their own logic: `describe_perspective`, the `instance_*`
  tools, `instance_transcript`, `flow_state`, `flow_actions`, `generate_waker_query`,
  `get_mention_waker_config`, `get_documentation`, `auth_status`, and the dynamic per-class
  tools. Those that take a perspective get it through `get_readable_perspective` /
  `get_writable_perspective`, which run the guard of the perspective RPC methods,
  `Guard::perspective(perspective_query_capability | perspective_update_capability)`, on the
  caller's `RequestContext` (`Owns { Perspective }`) and answer refusals through
  `method_error`. So the operator
  reaches every perspective, a user only its own, an app the unowned and main-agent ones, with
  the capability RPC requires. A dynamic write tool checks the write rule before it looks up
  the class; `generate_dynamic_tools` lists classes of perspectives the caller may read
  (`may_read_perspective`). `every_perspective_tool_without_a_method_follows_the_perspective_rule`
  (in the MCP matrix tests) runs each of them as every matrix caller; a new perspective tool
  without an RPC method goes into its `perspective_tools()` list.
- `auth_status` reports the same resolved session (principal, did, capabilities,
  or the `AuthError`), so `authenticated` is true exactly when a tool call passes.

## Tool files (`tools/`)

| File | Tools |
|---|---|
| `mod.rs` | `Ad4mMcpHandler`, dispatch, perspective access helpers (`get_readable_perspective`, `get_writable_perspective`), SHACL link helpers |
| `auth.rs` | login / capability request flow |
| `perspectives.rs` | perspective + link CRUD |
| `instances/` | the static instance surface: `instance_query` / `_get` / `_create` / `_update` / `_remove`, `instance_add_to_collection` / `_remove_from_collection`, `instance_transcript`, child links, and `describe_perspective`. Class-agnostic — the class is a parameter, not a tool name; `instance_transcript` takes no class at all, it is one model query on the built-in TextContent (#1446). Also `execute_commands` (`commands.rs`), the deliberate raw-SDNA escape hatch: it runs actions directly and does **not** go through the class's SHACL validation, so it is the one tool here that can write a shape the model would reject |
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

- A tool that does the job of an RPC method calls it (`call`), never the code behind
  it. The other tools call `PerspectiveInstance` directly; keep their semantics aligned with
  the WS handlers (same capability, same `Session::acts_as`).
- Permission denial on a perspective is 404 from the guard (D6), never 403: don't leak
  perspective existence. No tool names an AI task; an MCP session reaches tasks only through
  the handler map, under `Owns { Task }`
  (`main_agent_apps_reach_no_managed_agents_ai_task_through_mcp`).
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
