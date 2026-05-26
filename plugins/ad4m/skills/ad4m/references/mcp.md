# AD4M MCP Server

The MCP (Model Context Protocol) server enables AI agents to interact with AD4M natively. It dynamically generates tools from SHACL subject class definitions, so agents can work with structured data without knowing the underlying link graph.

## Enabling

```bash
ad4m-executor run --enable-mcp true --mcp-port 3001 ...
```

## Transport

Streamable HTTP (bidirectional HTTP with SSE-like streaming). Connect at `http://localhost:3001/mcp`.

**Not stdio** — the MCP server runs alongside the AD4M API server in the same process. Uses JSON-RPC 2.0 over HTTP POST with session management via `Mcp-Session-Id` header.

**Do NOT use `curl` to call MCP tools.** The server always responds with `Content-Type: text/event-stream` (Server-Sent Events), not plain JSON. Raw curl will get garbled SSE event data. You must use a proper MCP client (your tool interface, an MCP SDK, etc.).

## Authentication

Two authentication methods are available:

### Option A: `--admin-credential` flag (recommended for single-agent setups)

Start the executor with `--admin-credential <secret>`. All MCP tool calls from that session are automatically authenticated — no extra auth step needed. The credential is passed as part of the session context.

> **Note:** HTTP `Authorization` headers are NOT reliably forwarded to MCP tool handlers by all MCP clients. If you set `headers` in your `.mcp.json` config, the header may not reach the auth check. Use `--admin-credential` or the JWT flow below instead.

### Option B: JWT auth flow (works with any MCP client)

Use the MCP auth tools (no auth required to call these):

1. `ad4m_request_capability(app_name: "AI Agent", app_desc: "AD4M bot")` → returns `request_id`
2. Find the 6-digit verification code in the executor's **stdout** (log file or screen session)
3. `ad4m_generate_jwt(request_id: "<from step 1>", code: "<6-digit code>")` → returns JWT
4. All subsequent tool calls in this session are authenticated

Without any admin credential configured, empty token has full access.

## Core Tools

These are always available regardless of SDNA:

**Perspective & Link Tools:**

| Tool                     | Description                                   |
| ------------------------ | --------------------------------------------- |
| `ad4m_list_perspectives` | List all perspectives                         |
| `ad4m_add_perspective`   | Create a new perspective                      |
| `ad4m_add_link`          | Add a link to a perspective                   |
| `ad4m_query_links`       | Query links in a perspective                  |
| `ad4m_get_models`        | List available subject classes (SHACL shapes) |
| `ad4m_add_model`         | Add SHACL SDna to a perspective               |
| `ad4m_infer`             | Run Prolog queries for complex reasoning      |

**Subject CRUD Tools:**

| Tool                          | Description                            |
| ----------------------------- | -------------------------------------- |
| `ad4m_query_subjects`         | Find instances of a subject class      |
| `ad4m_get_subject_data`       | Get full data for a subject instance   |
| `ad4m_create_subject`         | Create a new subject instance          |
| `ad4m_set_subject_property`   | Set a property on a subject            |
| `ad4m_delete_subject`         | Delete a subject instance              |
| `ad4m_get_subject_collection` | Get items in a collection property     |
| `ad4m_add_to_collection`      | Add item to a collection               |
| `ad4m_remove_from_collection` | Remove item from a collection          |
| `ad4m_execute_commands`       | Execute commands on a subject instance |

**Child/Tree Tools:**

| Tool                            | Description                                                                                                                            |
| ------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `ad4m_add_child`                | Add a child to a subject                                                                                                               |
| `ad4m_get_children`             | Get children of a subject (addresses, timestamps, authors)                                                                             |
| `ad4m_get_children_body_parsed` | Get the most recent N children (default 50) with resolved body text, author names, and timestamps as a formatted transcript. Use `limit` param to control count. **Preferred for reading conversations.** |
| `ad4m_get_subject_children`     | Get children with optional class filtering                                                                                             |

**Profile Tools:**

| Tool                             | Description                                              |
| -------------------------------- | -------------------------------------------------------- |
| `ad4m_get_my_did`                | Get the current agent's DID (for filtering own messages) |
| `ad4m_get_agent_profile`         | Get agent's DID and profile                              |
| `ad4m_set_agent_profile`         | Update agent profile fields                              |
| `ad4m_set_agent_profile_picture` | Set agent's profile picture                              |

**Neighbourhood & Language Tools:**

| Tool                                          | Description                                                            |
| --------------------------------------------- | ---------------------------------------------------------------------- |
| `ad4m_language_meta`                          | Get metadata about a language by address                               |
| `ad4m_list_link_language_templates`           | List available P2P sync templates for neighbourhoods                   |
| `ad4m_neighbourhood_publish_from_perspective` | Publish a perspective as a shared neighbourhood (auto-clones template) |
| `ad4m_neighbourhood_join_from_url`            | Join an existing neighbourhood by URL                                  |

**Waker/Subscription Tools:**

| Tool                            | Description                               |
| ------------------------------- | ----------------------------------------- |
| `ad4m_generate_waker_query`     | Generate SPARQL for waker subscription    |
| `ad4m_get_mention_waker_config` | Get waker config for tracking mentions    |

**Auth Tools (no auth required):**

| Tool                      | Description                            |
| ------------------------- | -------------------------------------- |
| `ad4m_request_capability` | Step 1 of local auth flow              |
| `ad4m_generate_jwt`       | Step 2 of local auth flow              |
| `ad4m_login_email`        | Login with email/password (multi-user) |
| `ad4m_signup`             | Create new account (multi-user)        |
| `ad4m_verify_email_code`  | Verify email code (multi-user)         |
| `ad4m_auth_status`        | Check current authentication status    |

## Language & Neighbourhood Tools

### Inspecting Languages

Use `ad4m_language_meta` to get information about any language address — name, description, author, template params, source code link.

### Publishing Neighbourhoods

To share a perspective as a P2P neighbourhood:

```
1. ad4m_list_link_language_templates  → get available sync engines
2. ad4m_neighbourhood_publish_from_perspective(
     perspective_uuid,
     link_language_template: templates[0].address,
     name: "My Neighbourhood"
   )                             → auto-clones template, publishes, returns URL
```

The tool handles link language cloning automatically. Each neighbourhood gets a unique sync instance derived from the template.

### Joining Neighbourhoods

```
ad4m_neighbourhood_join_from_url(url: "neighbourhood://Qm...")
  → creates local perspective synced with the neighbourhood
```

## Dynamic Tools (from SHACL)

When a perspective has SHACL SDNA, tools are generated per class:

For a class `Channel` with scalar `name`, scalar `description`, and collection `messages`:

| Tool                           | Description                                                                                                            |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------- |
| `ad4m_channel_create`          | Create a Channel instance (required props as params; `expression_address` optional; `parent` optional to add as child) |
| `ad4m_channel_query`           | Query all Channel instances in the perspective                                                                         |
| `ad4m_channel_list`            | List Channel instances that are children of a parent with addresses, timestamps, and authors (sorted by timestamp)     |
| `ad4m_channel_get`             | Get a Channel by expression address                                                                                    |
| `ad4m_channel_delete`          | Delete a Channel instance                                                                                              |
| `ad4m_channel_set_name`        | Set the name property                                                                                                  |
| `ad4m_channel_set_description` | Set the description property                                                                                           |
| `ad4m_channel_get_messages`    | Get all messages in the collection                                                                                     |
| `ad4m_channel_add_messages`    | Add a message to the collection                                                                                        |
| `ad4m_channel_remove_messages` | Remove a message from the collection                                                                                   |

### Naming Convention

Class names are **lowercased** in tool names: `{class_lower}_{action}` or `{class_lower}_{action}_{property_lower}`.

Generated tool patterns per class:

- `{class}_create` — create instance (`perspective_id`, optional `expression_address`, optional `parent`, + required properties)
- `{class}_query` — query all instances (`perspective_id`)
- `{class}_list` — list instances that are children of a parent with addresses, timestamps, and authors (`perspective_id`, `parent`)
- `{class}_get` — get instance data (`perspective_id`, `expression_address`)
- `{class}_delete` — delete instance (`perspective_id`, `expression_address`)
- `{class}_set_{property}` — set scalar property (`perspective_id`, `expression_address`, `value`)
- `{class}_get_{collection}` — get collection items (`perspective_id`, `expression_address`)
- `{class}_add_{collection}` — add to collection (`perspective_id`, `expression_address`, `value`)
- `{class}_remove_{collection}` — remove from collection (`perspective_id`, `expression_address`, `value`)

### Tool Parameters

- `perspective_id` — which perspective to operate on (NOT `perspective_uuid`) — required for all tools
- `expression_address` — optional on `{class}_create` only (auto-generated if omitted); required on get/update/delete operations
- `parent` — optional on `{class}_create` only (adds as child); required on `{class}_list`
- `parent` — optional parent address to add the new instance as a child of (e.g., channel ID)
- Property-specific params (type-checked against SHACL datatype)

## Workflow Example

```
1. ad4m_get_my_did                  → get your DID for filtering own messages
2. ad4m_get_agent_profile           → verify identity
3. ad4m_add_perspective             → create workspace
4. ad4m_add_model                   → add SHACL schema
5. ad4m_get_models                  → verify schema loaded
6. ad4m_channel_create              → create a Channel (expression_address optional)
7. ad4m_message_create              → create a Message (expression_address auto-generated, provide body + parent=<channel>)
8. ad4m_message_list(perspective_id, parent=<channel>) → list all messages in channel
```

## Error Handling

MCP tool calls return JSON-RPC responses:

- Success: `{ "result": { "content": [{ "type": "text", "text": "..." }] } }`
- Error: `{ "error": { "code": -32000, "message": "..." } }`

Common errors:

- `Perspective not found` — invalid UUID
- `Subject class not found` — SDNA not added to perspective
- `Unauthorized` — missing or invalid auth token
