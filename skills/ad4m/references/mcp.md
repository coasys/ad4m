# AD4M MCP Server

The MCP (Model Context Protocol) server enables AI agents to interact with AD4M natively. It dynamically generates tools from SHACL subject class definitions, so agents can work with structured data without knowing the underlying link graph.

## Enabling

```bash
ad4m-executor run --enable-mcp true --mcp-port 3001 ...
```

## Transport

HTTP with Server-Sent Events (SSE). Connect at `http://localhost:3001/`.

**Not stdio** — the MCP server runs alongside the GraphQL server in the same process.

## Authentication

Bearer token in the HTTP `Authorization` header:

```
Authorization: Bearer <token>
```

The token is the `--admin-credential` value passed to the executor. Without an admin credential, empty token has full access.

## Core Tools

These are always available regardless of SDNA:

| Tool | Description |
|------|-------------|
| `agent_me` | Get current agent DID and status |
| `agent_generate` | Generate new agent keys (first run only) |
| `agent_unlock` | Unlock agent with passphrase |
| `agent_lock` | Lock agent keys |
| `perspective_create` | Create a new perspective |
| `perspective_list` | List all perspectives |
| `add_link` | Add a link to a perspective |
| `get_links` | Query links in a perspective |
| `remove_link` | Remove a link from a perspective |
| `get_models` | List available subject classes (SHACL shapes) |
| `add_model` | Add SHACL SDNA to a perspective |

## Dynamic Tools (from SHACL)

When a perspective has SHACL SDNA, tools are generated per class:

For a class `Channel` with scalar `name`, scalar `description`, and collection `messages`:

| Tool | Description |
|------|-------------|
| `channel_create` | Create a Channel instance (required props as params) |
| `channel_get` | Get a Channel by URI |
| `channel_set_name` | Set the name property |
| `channel_set_description` | Set the description property |
| `channel_get_messages` | Get all messages in the collection |
| `channel_add_messages` | Add a message to the collection |
| `channel_remove_messages` | Remove a message from the collection |

### Naming Convention

Class-first: `{class}_{action}` or `{class}_{action}_{property}`.

Examples:
- `task_create`, `task_set_title`, `task_get_assignees`
- `post_create`, `post_set_content`, `post_add_comment`

### Tool Parameters

All dynamic tools include:
- `perspective_uuid` — which perspective to operate on
- Property-specific params (type-checked against SHACL datatype)

## Workflow Example

```
1. agent_me                    → verify identity
2. perspective_create          → create workspace
3. add_model                   → add SHACL schema
4. get_models                  → verify schema loaded
5. channel_create              → create a Channel
6. channel_set_name            → set its name
7. message_create              → create a Message
8. channel_add_messages        → add message to channel
```

## Error Handling

MCP tool calls return JSON-RPC responses:
- Success: `{ "result": { "content": [{ "type": "text", "text": "..." }] } }`
- Error: `{ "error": { "code": -32000, "message": "..." } }`

Common errors:
- `Perspective not found` — invalid UUID
- `Subject class not found` — SDNA not added to perspective
- `Unauthorized` — missing or invalid auth token
