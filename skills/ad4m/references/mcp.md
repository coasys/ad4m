# AD4M MCP Server

The MCP (Model Context Protocol) server enables AI agents to interact with AD4M natively. It dynamically generates tools from SHACL subject class definitions, so agents can work with structured data without knowing the underlying link graph.

## Enabling

```bash
ad4m-executor run --enable-mcp true --mcp-port 3001 ...
```

## Transport

**Streamable HTTP** at `POST http://localhost:3001/mcp`. Uses the `rmcp` crate.

- **Not SSE-only, not stdio** — the MCP server runs alongside the GraphQL server in the same process.
- All requests go to the same `/mcp` endpoint as HTTP POST.
- Responses are delivered as SSE streams (`text/event-stream`) within the HTTP response body.
- Clients **must** include `Accept: application/json, text/event-stream` on every request.

## Session Lifecycle

MCP uses stateful sessions. Each session is identified by a `Mcp-Session-Id` header returned from the initialize handshake.

### ⚠️ Required handshake (initialize → notifications/initialized)

Every MCP session **must** complete a two-step handshake before tool calls work:

```bash
# Step 1: Initialize — capture Mcp-Session-Id from response header
curl -si -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"my-app","version":"1.0"}}}'
# → Response headers include: Mcp-Session-Id: <uuid>

# Step 2: Send initialized NOTIFICATION (no "id" field = notification, not request)
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized"}'
```

**Without step 2**, the server expects the `notifications/initialized` message as the next thing after initialize. If it receives a tool call instead, it fails with `"expect initialized notification"` and **terminates the session**. This is the most common cause of "MCP sessions expire immediately."

### Session persistence

- Sessions are tied to the executor process — **restarting the executor invalidates all sessions**.
- Sessions persist across multiple HTTP requests as long as the `Mcp-Session-Id` header is included.
- There is no explicit session timeout when `keep_alive` is set to `None` (the default).

## Authentication

MCP auth is **session-scoped** — authenticate once per session, and the token is stored server-side for all subsequent calls in that session.

### ⚠️ Agent must be unlocked first

The wallet is encrypted at rest. After starting or restarting the executor, the agent must be unlocked via GraphQL before MCP auth tools will work:

```bash
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentUnlock(passphrase: \"<passphrase>\", holochain: true) { did isUnlocked } }"}'
```

Without this, `request_capability` and `generate_jwt` return `"Wallet is locked"`. The agent appears initialized but cannot sign tokens.

### Single-user auth flow (request_capability + generate_jwt)

```bash
# Step 1: Request capability
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"request_capability","arguments":{"app_name":"my-bot","app_desc":"AI agent","app_url":"http://localhost"}}}'
# → Returns: { request_id: "...", code: "189217", message: "Capability requested and auto-permitted..." }
# The code is also printed to executor stdout.

# Step 2: Generate JWT
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"generate_jwt","arguments":{"request_id":"<id>","code":"<code>"}}}'
# → Returns: { success: true, token: "eyJ...", message: "JWT generated and stored. You are now authenticated." }
# Token is stored in the session — no need to pass it on subsequent calls.
```

### Multi-user auth flow (login_email)

For executors with multi-user mode enabled:

```bash
# Login with email/password (user must have been created via runtimeCreateUser)
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"login_email","arguments":{"email":"user@example.com","password":"pass123"}}}'
```

### Check auth status

```bash
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"auth_status","arguments":{}}}'
```

## Core Tools

These are always available regardless of SDNA:

### Auth Tools
| Tool | Description |
|------|-------------|
| `request_capability` | Request capability token (step 1/2 of local auth) |
| `generate_jwt` | Generate JWT from capability request (step 2/2) |
| `auth_status` | Check current authentication state |
| `signup` | Create account (multi-user mode) |
| `login_email` | Login with email/password (multi-user mode) |
| `request_login_verification` | Request email verification |
| `verify_email_code` | Verify email with code |

### Perspective & Link Tools
| Tool | Description |
|------|-------------|
| `add_perspective` | Create a new perspective |
| `list_perspectives` | List all perspectives |
| `add_link` | Add a raw link (source, predicate, target) |
| `query_links` | Query links by source/predicate/target |
| `query_subjects` | Find instances of a subject class |
| `create_subject` | Create a new subject instance |
| `get_subject_data` | Get full data for a subject |
| `set_subject_property` | Set a property on a subject |
| `delete_subject` | Delete a subject instance |

### Children & Collections
| Tool | Description |
|------|-------------|
| `get_subject_children` | Get children of a subject instance |
| `get_subject_collection` | Get items in a collection property |
| `add_to_collection` | Add item to a collection property |
| `remove_from_collection` | Remove item from a collection |

### Neighbourhood Tools
| Tool | Description |
|------|-------------|
| `neighbourhood_join_from_url` | Join a shared neighbourhood |
| `neighbourhood_publish_from_perspective` | Publish a perspective as a neighbourhood |

### Agent Tools
| Tool | Description |
|------|-------------|
| `get_agent_profile` | Get agent's DID and profile |
| `set_agent_profile` | Update agent profile |
| `set_agent_profile_picture` | Set agent profile picture |
| `get_agent_public_perspective` | Get agent's public perspective |
| `set_agent_public_perspective` | Set agent's public perspective |

### AI/Flow Tools
| Tool | Description |
|------|-------------|
| `get_models` | List available AI models |
| `add_model` | Add an AI model |
| `infer` | Run inference on a model |
| `get_flows` | List AI flows |
| `add_flow` | Add a new flow |
| `flow_start` | Start a flow |
| `flow_state` | Get flow state |
| `flow_actions` | List flow actions |
| `flow_run_action` | Run a flow action |

### Utility
| Tool | Description |
|------|-------------|
| `execute_commands` | Execute arbitrary commands |
| `generate_waker_query` | Generate SurrealQL for waker subscription |

## Dynamic Tools (from SHACL)

When a perspective has SHACL SDNA (e.g., after joining a Flux neighbourhood), tools are dynamically generated per subject class:

For a class `Channel` with scalar `name`, scalar `description`, and collection `messages`:

| Tool | Description |
|------|-------------|
| `channel_create` | Create a Channel instance |
| `channel_query` | Find Channel instances |
| `channel_get` | Get a Channel by URI |
| `channel_delete` | Delete a Channel |
| `channel_set_name` | Set the name property |
| `channel_set_description` | Set the description property |
| `channel_get_messages` | Get all messages in the collection |
| `channel_add_messages` | Add a message to the collection |
| `channel_remove_messages` | Remove a message from the collection |

A Flux neighbourhood typically exposes ~248 tools total (core + dynamic).

### Naming Convention

Class-first: `{class}_{action}` or `{class}_{action}_{property}`.

## Error Handling

MCP tool calls return JSON-RPC responses via SSE:

```
data: {"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"{...}"}],"isError":false}}
```

The `text` field contains JSON with the actual result or error. Common errors:

| Error | Cause | Fix |
|-------|-------|-----|
| `Wallet is locked` | Agent not unlocked after restart | `agentUnlock` via GraphQL |
| `main signing key not found` | `agentGenerate` never called | Run `agentGenerate` once |
| `expect initialized notification` | Missing `notifications/initialized` | Send it after `initialize` |
| `Session not found` | Executor restarted or session expired | Re-initialize a new session |
| `Can't find permitted request` | Wrong request_id or code | Re-run `request_capability` |
| `Perspective not found` | Invalid UUID | Check `list_perspectives` |
| `Subject class not found` | SDNA not loaded in perspective | Join a neighbourhood with SDNA, or `add_model` |
| `Not Acceptable` | Missing `Accept` header | Include `Accept: application/json, text/event-stream` |

## Complete Session Example

```bash
# 1. Ensure agent is unlocked (via GraphQL, not MCP)
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: admin-secret" \
  -d '{"query":"mutation { agentUnlock(passphrase: \"my-passphrase\", holochain: true) { isUnlocked } }"}'

# 2. Initialize MCP session
curl -si -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -D /tmp/mcp-headers.txt \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"my-bot","version":"1.0"}}}' > /dev/null

MCP_SESSION=$(grep -i 'mcp-session-id' /tmp/mcp-headers.txt | awk '{print $2}' | tr -d '\r')

# 3. Send initialized notification
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $MCP_SESSION" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized"}'

# 4. Authenticate
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $MCP_SESSION" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"request_capability","arguments":{"app_name":"my-bot","app_desc":"AI agent","app_url":"http://localhost"}}}'
# → Parse request_id and code from response

curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $MCP_SESSION" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"generate_jwt","arguments":{"request_id":"<id>","code":"<code>"}}}'

# 5. Use tools (authenticated)
curl -s -X POST http://localhost:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $MCP_SESSION" \
  -d '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"list_perspectives","arguments":{}}}'
```
