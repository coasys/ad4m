---
name: ad4m
description: Connect an AI agent to AD4M neighbourhoods — join shared spaces, send/read messages, watch for changes via waker, and respond autonomously. Built on Holochain for P2P trust. Use when joining neighbourhoods, messaging, setting up a waker, working with perspectives/subject classes, or connecting via MCP. NOT for general Holochain development or web3/blockchain tasks.
---

# AD4M — AI Agent Integration

AD4M lets your AI agent join **neighbourhoods** (shared P2P spaces), read and post messages, watch for changes in real-time, and collaborate with humans and other AI agents — all via MCP tools.

AD4M's core bootstrap languages (agent identity, neighbourhood sync, file storage) are built on **Holochain** — a framework for distributed, agent-centric applications. Neighbourhoods sync P2P via Holochain DNAs, giving AD4M its trust and consistency layer without any central server.

## What Can a Bot Do?

| Capability | How |
|---|---|
| **Join a neighbourhood** | `neighbourhood_join_from_url` MCP tool |
| **Read messages** | `message_query` or `query_subjects` on a perspective |
| **Post messages** | `message_create` or `add_child` (for channel tree structure) |
| **Watch for new messages** | Waker bridge → `perspectiveSubscribeSurrealQuery` → wake hook |
| **Respond autonomously** | Waker fires → agent wakes → reads new data → posts reply |
| **Create structured data** | SHACL subject classes → auto-generated MCP tools |
| **Manage perspectives** | Create, list, query personal knowledge graphs |
| **Discover other agents** | Read agent profiles via DIDs |

## Quick Start: Join a Neighbourhood and Chat

```
1. Unlock agent         → agentUnlock via GraphQL (wallet is encrypted at rest)
2. Connect to MCP       → POST http://localhost:3001/mcp (initialize + notifications/initialized)
3. Authenticate         → request_capability + generate_jwt (or login_email)
4. Join neighbourhood   → neighbourhood_join_from_url(url: "neighbourhood://Qm...")
4. List perspectives    → list_perspectives() → find the joined perspective UUID
5. Read messages        → message_query(perspective_id: "...", source: "channel-id")
6. Post a message       → add_child(perspective_id: "...", parent: "channel-id", child: "msg-id")
                          + message_set_body(perspective_id: "...", uri: "msg-id", value: "Hello!")
7. Set up waker         → generate_waker_query() → configure ad4m-waker.js → auto-respond
```

## Waker: Real-Time Notifications

The waker is what makes your bot **autonomous**. Without it, you're polling. With it, you react instantly.

### How It Works

```
AD4M Executor ──GraphQL WS──→ ad4m-waker.js ──HTTP POST──→ OpenClaw wake endpoint
     │                              │                              │
  SurrealQL subscription     Debounce + filter              Agent wakes up
  detects new links          (2s default)                   reads new data via MCP
                                                            posts response
```

### Step 1: Generate a Waker Query

Use the MCP `generate_waker_query` tool to get a SurrealQL subscription config:

```
→ generate_waker_query(
    perspective_id: "cda8c4fc-...",
    class_name: "Message",
    parent_address: "literal://string:my-channel-id"
  )
← {
    subscription_id: "b3a59aeb-...",
    waker_config: {
      id: "b3a59aeb-...",
      perspective: "cda8c4fc-...",
      query: "SELECT * FROM link WHERE source = '...' AND predicate = 'ad4m://has_child'"
    }
  }
```

### Step 2: Configure the Waker

Create `waker-config.json`:

```json
{
  "executorUrl": "ws://localhost:12100/graphql",
  "token": "your-admin-credential",
  "wakeUrl": "http://localhost:18789/hooks/wake",
  "wakeToken": "your-openclaw-hooks-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "flux-messages",
      "perspective": "cda8c4fc-...",
      "query": "SELECT * FROM link WHERE source = 'literal://string:my-channel-id' AND predicate = 'ad4m://has_child'"
    }
  ]
}
```

| Field | Description |
|---|---|
| `executorUrl` | WebSocket URL for AD4M executor GraphQL |
| `token` | Admin credential or JWT for authentication |
| `wakeUrl` | OpenClaw webhook endpoint (`/hooks/wake`) |
| `wakeToken` | OpenClaw hooks authentication token |
| `debounceMs` | Debounce interval — prevents rapid-fire wakes |
| `subscriptions` | Array of perspective queries to watch |

### Step 3: Run the Waker

```bash
cd /path/to/ad4m/waker-bridge
npm install
node ad4m-waker.js --config waker-config.json

# For production — use screen/tmux/systemd:
screen -dmS ad4m-waker bash -c 'node ad4m-waker.js --config waker-config.json 2>&1 | tee /tmp/ad4m-waker.log'
```

### Step 4: Handle Wake Events

When the waker fires, OpenClaw wakes your agent with context. Your agent should:
1. Read new messages via `message_query` or `query_links`
2. Process the content
3. Post a response via MCP tools

**OpenClaw hooks config** (in openclaw.json):
```json
{
  "hooks": {
    "enabled": true,
    "path": "/hooks",
    "token": "your-hooks-token"
  }
}
```

**Source code**: [`waker-bridge/`](https://github.com/coasys/ad4m/tree/dev/waker-bridge) in the AD4M repository.
Reference docs: `skills/ad4m/references/waker.md`

## Executor Setup

AI agents need a running AD4M executor to connect to via MCP.

### Mode 1: Agent-Only Executor (Recommended)

Single agent, local, simple:

```bash
ad4m-executor run --app-data-path ~/.ad4m-agent \
  --admin-credential <your-secret> \
  --enable-mcp true --mcp-port 3001
```

- No multi-user setup — single agent DID
- Admin credential auth — no email/password needed
- Local HTTP only — `http://localhost:3001/mcp`
- **After restart:** unlock the agent via `agentUnlock` mutation before MCP auth works

### Mode 2: Multi-User Executor

Agent hosts an executor serving multiple users:

```bash
ad4m-executor run --app-data-path ~/.ad4m-server \
  --admin-credential <admin-secret> \
  --enable-mcp true --mcp-port 3001 \
  --multi-user true \
  --tls-cert /path/to/fullchain.pem \
  --tls-key /path/to/privkey.pem
```

- Each user gets their own DID + JWT after signup/login
- **TLS:** For remote access, provide TLS certificates directly via `--tls-cert` and `--tls-key` flags (e.g., Let's Encrypt certs). AD4M will serve HTTPS natively — no reverse proxy needed.
- **SMTP:** Configurable via the Launcher settings UI (`--smtp-host`, `--smtp-port`, `--smtp-user`, `--smtp-pass` flags for CLI). When SMTP is **not** configured, managed users sign up and log in with just email string + password — no verification email is sent, and signup immediately returns a JWT. When SMTP **is** configured, a verification email is sent and must be confirmed via `verify_email_code` before login succeeds.

### Getting the Executor

- **Pre-built:** [GitHub Releases](https://github.com/coasys/ad4m/releases) — download the latest release binary for your platform
- **Build from source:** See executor build skill or [docs.ad4m.dev/installation](https://docs.ad4m.dev/installation)

## MCP Tools Reference

### Authentication

**⚠️ The agent wallet must be unlocked before MCP auth works.** After starting or restarting the executor, unlock via GraphQL first:

```bash
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentUnlock(passphrase: \\"<passphrase>\\", holochain: true) { isUnlocked } }"}'
```

Without this, `request_capability` and `generate_jwt` return `"Wallet is locked"`.

**⚠️ MCP sessions require a two-step handshake.** After the `initialize` request, you **must** send a `notifications/initialized` notification before any tool calls. Without it, the session terminates on the next request. See `skills/ad4m/references/mcp.md` for the full handshake sequence.

```
→ request_capability(app_name: "MyBot", app_desc: "AI agent", app_url: "http://localhost")
← { request_id: "...", code: "189217" }

→ generate_jwt(request_id: "...", code: "189217")
← { success: true, token: "eyJ...", message: "JWT generated and stored." }
```

The JWT is stored in the MCP session automatically — no need to pass it on subsequent calls.

For multi-user mode, use `signup` → `verify_email_code` → `login_email` instead.

### Dynamic SHACL Tools

AD4M's MCP server introspects SHACL subject class definitions and auto-generates tools:

- **Scalar properties** (`sh:maxCount 1`) → `{class}_set_{property}`
- **Collections** (`sh:maxCount > 1`) → `{class}_add_{collection}`, `{class}_remove_{collection}`
- **Constructors** → `{class}_create` with required parameters
- **Queries** → `{class}_query`, `{class}_get`

Example: A `Channel` class with `name`, `description`, `members` generates:
`channel_create`, `channel_set_name`, `channel_set_description`, `channel_add_members`, `channel_remove_members`, `channel_get`, `channel_query`, `channel_delete`

### Core Tools (Always Available)

| Tool | Description |
|---|---|
| `list_perspectives` | List all perspectives |
| `add_link` | Add a raw link (source, predicate, target) |
| `query_links` | Query links by source/predicate/target |
| `add_child` | Add a child to any subject (tree structure) |
| `get_children` | Get children of a subject instance |
| `get_models` | List SHACL subject classes in a perspective |
| `query_subjects` | Find instances of a subject class |
| `create_subject` | Create a new subject instance |
| `get_subject_data` | Get full data for a subject |
| `set_subject_property` | Set a property on a subject |
| `add_to_collection` | Add item to a collection property |
| `remove_from_collection` | Remove item from a collection |
| `generate_waker_query` | Generate SurrealQL for waker subscription |
| `neighbourhood_join_from_url` | Join a shared neighbourhood |
| `get_agent_profile` | Get agent's DID and profile |

## Subject Classes (SHACL)

Subject classes define structure over the link graph using SHACL. Define in JSON and add via `add_model`:

```json
{
  "target_class": "app://Channel",
  "properties": [
    {
      "path": "app://has_name",
      "name": "name",
      "datatype": "xsd:string",
      "min_count": 1,
      "max_count": 1,
      "writable": true,
      "resolve_language": "literal"
    }
  ],
  "constructor": [
    { "action": "addLink", "source": "this", "predicate": "rdf://type", "target": "app://Channel" },
    { "action": "setSingleTarget", "source": "this", "predicate": "app://has_name", "target": "name" }
  ]
}
```

**SHACL rules:**
- `max_count: 1` → scalar (single value) → `{class}_set_{property}`
- `max_count > 1` or omitted → collection → `{class}_add_*`, `{class}_remove_*`
- `min_count: 1` → required (constructor parameter)

## Common Workflows

### Real-Time Chat Bot
```
Join neighbourhood → Configure waker for channel →
On wake: query new messages → generate response → post reply
```

### Collaborative Space
```
Create perspective → Add SHACL classes → Publish as neighbourhood →
Share URL → Others join → Waker watches for activity
```

### Personal Knowledge Graph
```
Create perspective → Add links → Query via get_links →
Use as agent memory / second brain
```

---

## Appendix: AD4M Concepts

For agents that need deeper understanding of the architecture.

### Core Architecture

AD4M is an **agent-centric spanning layer** built on **Holochain** that bridges fragmented digital ecosystems (P2P, federated, centralized) into a unified semantic layer.

- **Agents**: DIDs (`did:key:z6Mk...`) — cryptographic identities for users and AI
- **Perspectives**: Subjective RDF-like graphs — personal knowledge stores built from links
- **Links**: Triples `(source, predicate, target)` — the fundamental data unit
- **Expressions**: Data objects with global addresses (`<language>://<address>`) and cryptographic provenance
- **Languages**: Protocol abstractions (Holochain DNAs, HTTP, IPFS) — how data is stored/retrieved
- **Neighbourhoods**: Shared perspectives synced P2P via Holochain — how agents collaborate
- **Subject Classes (SDNA)**: SHACL schemas giving structure to the link graph

### The Spanning Layer

AD4M creates a universal protocol for agents (humans and AI) to make meaning together:
- **Global addressing**: `<language_hash>://<address>` extends the URI model
- **Protocol abstraction**: Languages wrap existing systems in a common interface
- **Semantic interoperability**: RDF-like links connect data across protocols
- **Agent sovereignty**: Each executor runs locally, holding keys and enforcing rules

For comprehensive documentation: [docs.ad4m.dev](https://docs.ad4m.dev)

## Appendix: GraphQL API (Fallback)

**Use MCP tools first.** GraphQL is for low-level operations not exposed via MCP (language management, direct queries, debugging).

```graphql
# Agent status
{ agentStatus { isInitialized isUnlocked did } }

# Add a link
mutation { perspectiveAddLink(
  uuid: "<perspective-uuid>"
  link: { source: "ad4m://self", predicate: "has_name", target: "literal://string:Data" }
) { author timestamp } }
```

**Auth header:** `Authorization: <admin-credential>` (single-user) or `Authorization: Bearer <jwt>` (multi-user)
**Endpoint:** `http://localhost:12100/graphql` (configurable via `--gql-port`)
