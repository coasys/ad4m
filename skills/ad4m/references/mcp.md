# MCP Server Reference

## Overview

The AD4M executor includes a built-in MCP (Model Context Protocol) server that exposes perspectives, neighbourhoods, agent management, AI flows, and SDNA-derived CRUD operations as MCP tools. This enables AI agents and LLM-based clients to interact with AD4M programmatically.

## Transport

**Streamable HTTP** at `POST /mcp` (not SSE, not stdio). Backed by the `rmcp` Rust crate.

Enable with:
```bash
ad4m-executor run \
  --app-data-path /tmp/ad4m-data \
  --admin-credential <password> \
  --enable-mcp true \
  --mcp-port 3001
```

The MCP server binds to `127.0.0.1` only. For remote access, use an SSH tunnel:
```bash
ssh -L 3001:127.0.0.1:3001 user@remote-host
```

## Session Lifecycle

### Handshake (required)

Every MCP session requires a three-step handshake before tool calls work:

1. **Initialize** — send `initialize` request, capture `Mcp-Session-Id` from response header
2. **Send `notifications/initialized`** — this is a JSON-RPC **notification** (no `id` field). The `rmcp` session worker blocks until it receives this. Without it, the next tool call triggers `"expect initialized notification"` and the session is terminated.
3. **Tool calls** — now permitted for the lifetime of the session

### Complete handshake example

```bash
# 1. Initialize — note the -i flag to see response headers
SESSION_ID=$(curl -si -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2024-11-05",
      "capabilities": {},
      "clientInfo": {"name": "my-client", "version": "1.0"}
    }
  }' | grep -i 'mcp-session-id' | awk '{print $2}' | tr -d '\r')

echo "Session: $SESSION_ID"

# 2. Send initialized notification (CRITICAL — no "id" field)
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -d '{"jsonrpc": "2.0", "method": "notifications/initialized"}'

# 3. Now tool calls work
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {"name": "list_perspectives", "arguments": {}}
  }'
```

### Session expiry

Sessions are tied to the executor process. **Restarting the executor invalidates all sessions** — clients must re-authenticate from scratch.

## Authentication

MCP uses a capability-based auth flow. The agent wallet **must be unlocked** before auth tools work.

### Pre-requisites

1. `agentGenerate` has been called (creates the wallet "main" key for JWT signing)
2. `agentUnlock` has been called after any executor restart
3. MCP session handshake completed (see above)

### Auth flow

```bash
# 1. Request capability — returns request_id and a 6-digit code
#    The code is ALSO printed to executor stdout
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -d '{
    "jsonrpc": "2.0", "id": 3,
    "method": "tools/call",
    "params": {
      "name": "request_capability",
      "arguments": {
        "app_name": "my-agent",
        "app_desc": "AI agent for neighbourhood interaction",
        "app_url": "http://localhost"
      }
    }
  }'
# → {"request_id": "...", "code": "123456", ...}

# 2. Generate JWT — authenticates the session
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -d '{
    "jsonrpc": "2.0", "id": 4,
    "method": "tools/call",
    "params": {
      "name": "generate_jwt",
      "arguments": {
        "request_id": "<request_id from step 1>",
        "code": "<code from step 1>"
      }
    }
  }'
# → Session is now authenticated. All subsequent calls use the token.
```

### Multi-user auth (email/password)

When multi-user mode is enabled, use `login_email` instead of the capability flow:

```bash
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: $SESSION_ID" \
  -d '{
    "jsonrpc": "2.0", "id": 3,
    "method": "tools/call",
    "params": {
      "name": "login_email",
      "arguments": {
        "email": "user@example.com",
        "password": "password"
      }
    }
  }'
```

### Wallet-locked errors

If the wallet is locked, auth tools return clear errors:

| Error message | Meaning | Fix |
|--------------|---------|-----|
| `Agent wallet is locked. Call agentUnlock first.` | Executor restarted, wallet not unlocked | Run `agentUnlock` via GraphQL |
| `Wallet is locked. The agent must be unlocked before generating JWTs.` | Same, hit during JWT generation | Same fix |
| `main signing key not found. Agent may not have been initialized.` | `agentGenerate` never called | Run `agentGenerate` via GraphQL |

## Tool Inventory

### Core tools (always available)

**Perspectives:**
| Tool | Description |
|------|-------------|
| `add_perspective` | Create a new local perspective |
| `list_perspectives` | List all perspectives |
| `query_links` | Query links in a perspective by source/target/predicate |
| `add_link` | Add a link to a perspective |

**Subjects (SDNA):**
| Tool | Description |
|------|-------------|
| `query_subjects` | Query subject instances by class |
| `create_subject` | Create a new subject instance |
| `get_subject_data` | Get all properties of a subject |
| `set_subject_property` | Set a property on a subject |
| `delete_subject` | Delete a subject instance |
| `get_subject_children` | Get child subjects |
| `get_subject_collection` | Get a collection property |
| `add_to_collection` | Add item to a collection |
| `remove_from_collection` | Remove item from a collection |

**Neighbourhoods:**
| Tool | Description |
|------|-------------|
| `neighbourhood_join_from_url` | Join a neighbourhood by URL |
| `neighbourhood_publish_from_perspective` | Publish a perspective as a neighbourhood |

**Agent:**
| Tool | Description |
|------|-------------|
| `get_agent_profile` | Get current agent profile |
| `set_agent_profile` | Update agent profile |
| `set_agent_profile_picture` | Set profile picture |
| `get_agent_public_perspective` | Get agent's public perspective |
| `set_agent_public_perspective` | Set agent's public perspective |

**Auth:**
| Tool | Description |
|------|-------------|
| `request_capability` | Start capability auth flow |
| `generate_jwt` | Complete capability auth flow |
| `auth_status` | Check current auth status |
| `signup` | Create new user (multi-user mode) |
| `login_email` | Login with email/password |
| `request_login_verification` | Request email verification |
| `verify_email_code` | Verify email code |

**AI / Flows:**
| Tool | Description |
|------|-------------|
| `get_models` | List available AI models |
| `add_model` | Register a new AI model |
| `infer` | Run inference on a model |
| `get_flows` | List AI flows |
| `add_flow` | Create an AI flow |
| `flow_start` | Start a flow |
| `flow_state` | Get flow state |
| `flow_actions` | Get available flow actions |
| `flow_run_action` | Execute a flow action |

**Utility:**
| Tool | Description |
|------|-------------|
| `execute_commands` | Execute shell commands on the executor host |
| `generate_waker_query` | Generate SurrealQL subscription query for a subject class |

### SDNA-derived tools (dynamic)

When you join a neighbourhood with SHACL-defined subject classes, the MCP server dynamically generates CRUD tools for each class. For example, a Flux neighbourhood with message, channel, conversation, and community classes exposes tools like:

- `message_create`, `message_query`, `message_get`, `message_update`, `message_delete`
- `channel_create`, `channel_query`, `channel_get`
- `community_create`, `community_query`, `community_get`
- etc.

A typical Flux neighbourhood exposes **~248 tools** total (core + dynamic).

## Waker Integration

The `generate_waker_query` tool creates SurrealQL queries for change-detection subscriptions. Use with `perspectiveSubscribeSurrealQuery` and a waker bridge to get notified of neighbourhood changes.

### Known issue: double-encoding

`generate_waker_query` URL-encodes the source address inside the generated SurrealQL query, producing:
```text
literal://string:literal%3A%2F%2Fstring%3A...
```
instead of:
```text
literal://string:actual-address
```

**Always verify the generated query** against `perspectiveQueryLinks` results and manually correct the source address if needed.

### Waker bridge pattern

```text
[Neighbourhood] → perspectiveSubscribeSurrealQuery
                → WebSocket subscription fires on change
                → Waker bridge POSTs to OpenClaw /hooks/wake
                → Agent wakes, reads new messages, responds
```

See `waker-bridge/ad4m-waker.js` in the AD4M repo for the reference implementation.

## Error Reference

| Error | Cause | Resolution |
|-------|-------|------------|
| `expect initialized notification` | Missing `notifications/initialized` after handshake | Send the notification before any tool calls |
| Empty/no response to tool calls | Session not initialized or expired | Re-run full handshake |
| `Agent wallet is locked` | Wallet not unlocked after restart | `agentUnlock(passphrase, holochain: true)` |
| `main signing key not found` | `agentGenerate` never called | Run `agentGenerate` mutation |
| `Capability not matched` | Insufficient permissions for the operation | Authenticate via `request_capability` + `generate_jwt` |
| `User already exists` | Duplicate `signup` / `runtimeCreateUser` | Use `login_email` instead |
| Tool call returns `"error"` with no detail | Server-side exception in tool handler | Check executor logs for stack trace |
| Session stops working after executor restart | All sessions invalidated on restart | Re-authenticate from scratch |
