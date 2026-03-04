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

## Setup for AI Agents

### Option 1: AD4M Launcher (Recommended for Desktop)

The easiest way to run AD4M is via the [AD4M Launcher](https://github.com/coasys/ad4m/releases) — a system-tray app that bundles the executor with a setup wizard:

1. Download the latest release for your OS (macOS, Linux AppImage)
2. Install and launch — it initializes your agent (DID + keys) on first run
3. Enable the MCP server in Settings → toggle "MCP Server" and set port (default: 3001)
4. Restart the launcher for MCP to become available

The launcher runs the executor at `http://localhost:12000/graphql` (GraphQL) and `http://localhost:3001/mcp` (MCP).

### Option 2: CLI Executor (For Servers/Scripts)

For headless servers, Docker, or scripting, use the CLI executor binary:

```bash
# 1. Download executor from GitHub releases (Linux x64 example)
curl -L -o ad4m-executor https://github.com/coasys/ad4m/releases/latest/download/ad4m-cli-executor-linux-x64
chmod +x ad4m-executor

# 2. Init (creates bootstrap seed — MUST run before first start)
./ad4m-executor init --data-path ~/.ad4m

# 3. Run executor with MCP enabled
./ad4m-executor run --app-data-path ~/.ad4m --gql-port 12100 \
  --admin-credential <secret> --enable-mcp true --mcp-port 3001

# 4. Generate agent (if not done during init)
# Download the CLI client separately or use GraphQL directly
```

**Important flags:**
- `--admin-credential`: Secret token for admin access (use a strong random string)
- `--enable-mcp true`: Enables the MCP server
- `--mcp-port 3001`: MCP server port (default: 3001)
- `--gql-port 12100`: GraphQL server port (default: 12000 in launcher, customizable in CLI)

### Option 3: Build from Source

For development or custom builds:

```bash
git clone --branch dev https://github.com/coasys/ad4m.git
cd ad4m/cli
cargo build --release --bin ad4m-executor

# Binary at: target/release/ad4m-executor
# Follow CLI executor steps above
```

See the [AD4M Installation Guide](https://docs.ad4m.dev/installation) for full details, including system requirements and dependencies.

## Interacting with AD4M

AD4M provides two complementary interfaces: **GraphQL** (full programmatic API) and **MCP** (AI agent-optimized tools).

### Security & Authentication

**Local development (single-user):**
- HTTP only, no TLS needed (`http://localhost:12100` / `http://localhost:3001`)
- Authenticate using the `--admin-credential` you set when starting the executor
- Include as `Authorization: <credential>` header for GraphQL
- For MCP, either include in Authorization header or use the `auth_status` check

**Production/multi-user deployments:**
- Use standard HTTPS with your own TLS certificates (reverse proxy recommended: nginx, Caddy)
- Each user gets their own DID and JWT after signup/login via MCP or GraphQL
- JWTs are scoped to individual users and their perspectives
- See [AD4M Authentication Guide](https://docs.ad4m.dev/auth) for multi-user setup details

**Note:** AD4M executors are designed to run locally (one per user) or on trusted infrastructure. For remote access, secure the connection with TLS and use strong credentials.

### GraphQL Examples

```graphql
# Check agent status
{ agentStatus { isInitialized isUnlocked did } }

# Create a perspective
mutation { perspectiveAdd(name: "My Space") { uuid name } }

# Add a link
mutation { perspectiveAddLink(
  uuid: "<perspective-uuid>"
  link: { source: "ad4m://self", predicate: "has_name", target: "literal://string:Data" }
) { author timestamp } }

# Get links
{ perspectiveQueryLinks(uuid: "<perspective-uuid>", query: { source: "ad4m://self" }) {
  data { source predicate target }
} }

# Join a neighbourhood
mutation { neighbourhoodJoinFromUrl(url: "neighbourhood://<hash>") { uuid name } }
```

Include `Authorization: <admin-credential>` header for authenticated requests.

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

Subject classes define structure over the link graph using SHACL (Shapes Constraint Language):

- `sh:maxCount 1` → scalar property (single value, `set_{prop}` tool)
- `sh:maxCount > 1` → collection (multiple values, `add_{coll}` / `remove_{coll}` tools)
- Class-first naming: `channel_create`, `task_set_title`, `post_add_comment`

Example SHACL for a Channel class:
```turtle
:ChannelShape a sh:NodeShape ;
  sh:targetClass :Channel ;
  sh:property [
    sh:path :name ;
    sh:datatype xsd:string ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:path :messages ;
    sh:class :Message ;
  ] .
```

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
