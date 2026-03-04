---
name: ad4m
description: Set up and interact with AD4M (Agent-Centric Distributed Application Meta-ontology) — a spanning layer that extends the internet to enable distributed collective intelligence. Use when installing AD4M executor, creating/joining neighbourhoods, working with perspectives and links, using SHACL subject classes, or connecting via MCP server for AI agent integration. AD4M bridges fragmented ecosystems (P2P, federated, centralized) into a unified semantic layer with agent-centric data sovereignty. NOT for: general Holochain development without AD4M, or web3/blockchain tasks.
---

# AD4M

## What is AD4M?

AD4M is an **agent-centric spanning layer** built on top of the internet, forming a bridge between fragmented digital ecosystems. Unlike traditional app-centric architectures where applications own your data, AD4M inverts this model: **each user runs their own executor** (a local runtime), owns their data in perspectives (subjective knowledge graphs), and chooses which protocols (languages) to speak.

### The Spanning Layer Concept

Think of AD4M as a new layer in the internet stack — one that doesn't replace existing protocols but harmonizes them. Just as TCP/IP created a universal protocol for machines to communicate, AD4M creates a universal protocol for agents (humans and AI) to make meaning together:

- **Global addressing scheme**: Every piece of data gets a universal address like `<language_hash>://<address>` (extending the URI model)
- **Protocol abstraction**: Languages wrap existing systems (HTTP, IPFS, Holochain, databases) in a common interface
- **Semantic interoperability**: RDF-like links connect data across protocols and platforms
- **Agent sovereignty**: Each user's executor runs locally, holding their keys and enforcing their rules

### Architecture

AD4M separates concerns in a novel way:

- **Perspectives**: Subjective RDF-like graphs — your personal knowledge store, built from links (triples)
- **Languages**: Protocol abstractions (Holochain DNAs, HTTP APIs, IPFS) — how data is stored and retrieved
- **Expressions**: Data objects with cryptographic provenance — globally addressable via `<language>://<address>`
- **Neighbourhoods**: Shared perspectives synced P2P via Holochain — how agents collaborate
- **Subject Classes (SDNA)**: SHACL-defined schemas — structure and validation over the link graph

**Built on Holochain:** AD4M's core bootstrap languages (agent, perspective, neighbourhood, language) are implemented as Holochain DNAs, providing the distributed trust layer. Neighbourhoods leverage Holochain's DHT for P2P synchronization and validation.

This architecture enables true data portability: your messages, posts, files, and social graph live in your perspectives, accessible to any app you grant permission to.

## Core Concepts (Quick Reference)

- **Agents**: DIDs (e.g., `did:key:z6Mk...`) representing users or AI entities. Each agent has cryptographic keys and signs all their actions.
- **Perspectives**: Subjective RDF-like graphs — personal knowledge stores built from links. Every piece of data lives in a perspective.
- **Links**: Triples `(source, predicate, target)` — the fundamental data unit. All knowledge is represented as links.
- **Expressions**: Data objects with global addresses (`<language>://<address>`) and cryptographic provenance. Created by languages.
- **Languages**: Protocol abstractions (Holochain DNAs, HTTP, IPFS, databases). Each language defines how to create/retrieve expressions.
- **Neighbourhoods**: Shared perspectives synced P2P via Holochain. How agents collaborate and share data.
- **Subject Classes (SDNA)**: SHACL-defined schemas that give structure to the link graph. Define properties, collections, constructors, and actions.

For deeper architecture details, see the [AD4M documentation](https://docs.ad4m.dev).

## Executor Setup for AI Agents

AI agents need a running AD4M executor to connect to via MCP. There are two deployment modes:

### Mode 1: Agent-Only Executor (Recommended)

The agent runs its own executor locally — simplest and most secure:

- **No multi-user setup needed** — single agent DID, no email/password auth
- **Admin credential authentication** — use `--admin-credential <secret>` flag
- **Local HTTP only** — `http://localhost:3001/mcp`, no TLS needed
- **Example:**
  ```bash
  ad4m-executor run --app-data-path ~/.ad4m-agent \
    --admin-credential <your-secret> \
    --enable-mcp true --mcp-port 3001
  ```

**When to use:** Most cases — agent has full control, no shared access, simple auth.

### Mode 2: Multi-User Executor (Agent + Human Users)

The agent hosts an executor that serves multiple users (including the agent itself):

- **Multi-user mode required** — each user gets their own DID + JWT after signup/login
- **Email/password authentication** — users authenticate via MCP `signup`/`login_email` tools
- **TLS/HTTPS required if remote** — provide certificates directly to AD4M or use a reverse proxy

**Basic example:**
```bash
ad4m-executor run --app-data-path ~/.ad4m-server \
  --admin-credential <admin-secret> \
  --enable-mcp true --mcp-port 3001 \
  --multi-user true \
  --tls-cert /path/to/fullchain.pem \
  --tls-key /path/to/privkey.pem
```

This serves HTTPS directly from AD4M. Alternatively, use a reverse proxy (nginx, Caddy) to terminate TLS and proxy to localhost HTTP.

**Email verification (SMTP setup):**
- **SMTP not configured** — users log in with email string + password only (no verification email sent)
- **SMTP configured** — verification emails are sent on signup (via launcher Settings → SMTP or `--smtp-*` CLI flags)

The SMTP configuration is optional — multi-user mode works without it, just without email-based account verification. For production deployments serving external users, SMTP is recommended.

**When to use:** Agent provides AD4M access to multiple humans, or agent needs to interact with human users' perspectives.

**Security note:** Multi-user executors require TLS certificates when accessed remotely. Use Let's Encrypt for free certificates, or self-signed certificates for testing.

### Getting the Executor Binary

- **Pre-built:** Download from [GitHub Releases](https://github.com/coasys/ad4m/releases) (Linux, macOS)
- **Build from source:** See [Installation Guide](https://docs.ad4m.dev/installation)
- **Launcher (for humans):** [AD4M Launcher](https://github.com/coasys/ad4m/releases) includes GUI setup wizard

## Working with AD4M via MCP

**Primary interface for AI agents:** MCP tools provide high-level operations over subject classes (structured data). Use MCP tools first — they auto-generate from SHACL schemas and include natural language descriptions optimized for LLMs.

### MCP Server (AI Agent Interface)

Enable with `--enable-mcp true` when starting the executor. The MCP server runs at `http://localhost:3001/mcp` (or your custom port).

**Why MCP?**
- **Tool discovery**: AI agents automatically discover available operations
- **Dynamic SHACL tools**: Subject classes generate domain-specific tools at runtime
- **Natural language descriptions**: Every tool includes LLM-optimized descriptions
- **Built-in auth**: Signup, login, and JWT management through MCP tools

**Dynamic SHACL-driven Tools:**

AD4M's MCP server introspects SHACL subject class definitions in perspectives and generates tools automatically:

- **Scalar properties** (`sh:maxCount 1`) → `{class}_set_{property}`, `{class}_get_{property}`
- **Collections** (`sh:maxCount > 1`) → `{class}_add_{collection}`, `{class}_remove_{collection}`
- **Constructors** → `{class}_create` with required property parameters

For example, a `Channel` SDNA with:
- `name` (scalar string property)
- `description` (scalar string property)
- `members` (collection of Agents)

Auto-generates: `channel_create`, `channel_set_name`, `channel_set_description`, `channel_add_members`, `channel_remove_members`, `channel_get`

**Core MCP tools** (always available): `perspective_create`, `perspective_list`, `add_link`, `get_links`, `remove_link`, `get_models`, `query_subjects`, `create_subject`, `agent_me`, `agent_unlock`, `signup`, `login_email`, `auth_status`, `join_neighbourhood`, `publish_neighbourhood`.

For comprehensive MCP documentation, workflow examples, and authentication details, see the [MCP Integration Guide](https://docs.ad4m.dev/developer-guides/mcp).

## Subject Classes (SHACL)

Subject classes define structure over the link graph using SHACL (Shapes Constraint Language). Define them in JSON format and add to a perspective using the MCP `add_model` tool.

**SHACL rules:**
- `max_count: 1` → scalar property (single value) → generates `{class}_set_{property}` tool
- `max_count > 1` or omitted → collection (multiple values) → generates `{class}_add_{collection}`, `{class}_remove_{collection}` tools
- `min_count: 1` → required property (becomes parameter in `{class}_create` constructor)

**Example:** Channel class with name (required scalar), description (optional scalar), and messages (collection):

```json
{
  "target_class": "app://Channel",
  "properties": [
    {
      "path": "app://has_channel_name",
      "name": "name",
      "datatype": "xsd:string",
      "min_count": 1,
      "max_count": 1,
      "writable": true,
      "resolve_language": "literal"
    },
    {
      "path": "app://has_channel_description",
      "name": "description",
      "datatype": "xsd:string",
      "min_count": 0,
      "max_count": 1,
      "writable": true,
      "resolve_language": "literal"
    },
    {
      "path": "app://has_messages",
      "name": "messages",
      "node_kind": "app://Message",
      "min_count": 0,
      "writable": true,
      "resolve_language": "app"
    }
  ],
  "constructor": [
    {
      "action": "addLink",
      "source": "this",
      "predicate": "rdf://type",
      "target": "app://Channel"
    },
    {
      "action": "setSingleTarget",
      "source": "this",
      "predicate": "app://has_channel_name",
      "target": "name"
    }
  ]
}
```

**Adding to a perspective:**
```
→ add_model(
    perspective_id: "abc123...",
    class_name: "Channel",
    shacl_json: "{...json above...}"
  )
← { success: true }

→ get_models(perspective_id: "abc123...")
← [ { name: "Channel", properties: ["name", "description"], collections: ["messages"] } ]
```

**Auto-generated tools:**
- `channel_create(perspective_id, name, description?)` — name is required (min_count: 1)
- `channel_set_name(perspective_id, uri, value)` — update name (max_count: 1)
- `channel_set_description(perspective_id, uri, value)` — update description
- `channel_add_messages(perspective_id, uri, value)` — add a Message to the collection
- `channel_remove_messages(perspective_id, uri, value)` — remove a Message
- `channel_get(perspective_id, uri)` — retrieve full Channel data

## Real-Time Notifications (Waker)

The AD4M waker (`waker-bridge/ad4m-waker.ts`) watches perspectives for changes and wakes your AI agent when new data arrives. It uses `PerspectiveProxy.subscribeSurrealDB()` — the same reactive query mechanism that powers Flux UI.

### How It Works

1. **Subscribe**: Configure SurrealQL queries to monitor specific perspectives
2. **Detect**: Waker receives real-time notifications when links match your query
3. **Wake**: Waker sends HTTP POST to your agent's wake endpoint with context
4. **React**: Your agent reads the new data via MCP tools and responds

### Setup

**Install and build:**
```bash
cd /path/to/ad4m/waker-bridge
npm install
npm run build  # Compiles TypeScript to JavaScript
```

**Configuration** (`waker-config.json`):
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
      "perspective": "cda8c4fc-7a2d-419a-b840-851c0e80b8b9",
      "query": "SELECT * FROM link WHERE source = 'literal://string:my-channel-id' AND predicate = 'ad4m://has_child'"
    }
  ]
}
```

**Running in production:**
```bash
# Use screen or tmux for persistence
screen -dmS ad4m-waker bash -c 'node dist/ad4m-waker.js --config waker-config.json 2>&1 | tee /tmp/ad4m-waker.log'

# Or use systemd/Docker for production deployments
```

### Generating Queries with MCP

The MCP `subscribe_to_model` tool can generate appropriate SurrealQL queries for SHACL subject classes:

```
→ subscribe_to_model(perspective_id: "abc123...", class_name: "Message")
← {
    query: "SELECT * FROM link WHERE target LIKE 'ad4m://Message%'",
    description: "Monitors all Message instances in this perspective"
  }
```

Use this query in your waker config to get notified when new Messages are created.

### Integration with OpenClaw

The waker sends HTTP POST to your configured `wakeUrl` with a JSON payload:

```json
{
  "text": "New data in perspective: abc123... (subscription: flux-messages)",
  "context": {
    "subscriptionId": "flux-messages",
    "perspective": "abc123...",
    "changes": [ /* link objects */ ]
  }
}
```

OpenClaw receives this, wakes your agent, and includes the context in the wake message.

**Source code**: [`waker-bridge/`](https://github.com/coasys/ad4m/tree/dev/waker-bridge) in the AD4M repository.

## Common Workflows

### 1. Personal Knowledge Graph
```
Create perspective → Add links (source, predicate, target) → Query via get_links
```
Use your perspective as a second brain — store notes, tasks, bookmarks, relationships.

### 2. Collaborative Space (Neighbourhood)
```
Create perspective → Add SHACL SDNA (e.g., Channel, Message) → 
Publish as neighbourhood → Share URL → Others join and sync
```
Every neighbourhood is a shared perspective with validation rules enforced via SDNA.

### 3. Structured Data with SHACL
```
Install or define SDNA → MCP tools auto-generate → 
Use {class}_create, {class}_set_{property} tools
```
SHACL subject classes provide ORM-like structure over the link graph.

### 4. AI Agent Integration (Full Cycle)
```
Connect via MCP → List perspectives → Get models (SHACL classes) →
Create subjects (channel_create, message_create) → 
Query subjects (query_subjects) → Subscribe via waker → 
React to changes on wake
```

### 5. Real-Time Chat Bot
```
Join neighbourhood (join_neighbourhood) → 
Configure waker subscription → 
On wake: query new messages → generate response → 
Send message (message_create or channel_add_messages)
```

### 6. Cross-Protocol Data Portability
```
Fetch HTTP expression (http://example.com/data.json) →
Store in perspective as link (source: did:key:..., target: http://...) →
Join IPFS neighbourhood (neighbourhood://Qm...) →
Reference HTTP data from IPFS expressions
```
AD4M bridges protocols — mix HTTP, IPFS, Holochain data in one graph.

---

## Appendix: GraphQL API (Fallback)

**Note:** MCP tools should be your primary interface. Use GraphQL only when MCP dynamic tools don't cover your specific use case. GraphQL is designed for the JavaScript client (`@coasys/ad4m`) and human developers, not AI agents.

### When to Use GraphQL

- Low-level operations not exposed via MCP (e.g., language management, direct Prolog queries)
- Debugging or inspecting executor state
- Building custom tooling outside the MCP abstraction

### GraphQL Examples

```graphql
# Check agent status
{ agentStatus { isInitialized isUnlocked did } }

# Create a perspective
mutation { perspectiveAdd(name: "My Space") { uuid name } }

# Add a link (prefer MCP add_link tool)
mutation { perspectiveAddLink(
  uuid: "<perspective-uuid>"
  link: { source: "ad4m://self", predicate: "has_name", target: "literal://string:Data" }
) { author timestamp } }

# Get links (prefer MCP get_links tool)
{ perspectiveQueryLinks(uuid: "<perspective-uuid>", query: { source: "ad4m://self" }) {
  data { source predicate target }
} }

# Join a neighbourhood (prefer MCP join_neighbourhood tool)
mutation { neighbourhoodJoinFromUrl(url: "neighbourhood://<hash>") { uuid name } }
```

**Authentication:**
- Include `Authorization: <admin-credential>` header for single-user mode
- Include `Authorization: Bearer <jwt>` for multi-user mode after login

**GraphQL endpoint:** `http://localhost:12000/graphql` (default, configurable via `--gql-port`)

For comprehensive GraphQL documentation, see the [AD4M JavaScript client docs](https://docs.ad4m.dev).
