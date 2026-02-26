---
name: ad4m
description: Set up and interact with AD4M (Agent-Centric Distributed Application Meta-ontology) — a distributed, local-first platform for building social apps on Holochain. Use when installing AD4M executor, creating/joining neighbourhoods, working with perspectives and links, using SHACL subject classes, or connecting via MCP server. AD4M enables peer-to-peer collaboration through shared perspectives synced via Holochain. NOT for: general Holochain development without AD4M, or web3/blockchain tasks.
---

# AD4M

AD4M is an agent-centric, local-first platform where each user runs their own executor. Unlike app-centric architectures, AD4M separates data (perspectives), storage protocols (languages), and social interaction patterns (subject classes/SDNA).

## Core Concepts

- **Perspectives**: Subjective RDF-like graphs — personal knowledge stores. Every piece of data lives in a perspective.
- **Links**: Triples `(source, predicate, target)` — the fundamental data unit. All data is links.
- **Languages**: Protocol abstractions (Holochain DNAs, HTTP, IPFS). Handle storage and retrieval.
- **Neighbourhoods**: Shared perspectives synced P2P via Holochain. How agents collaborate.
- **Subject Classes**: SHACL-defined schemas that give structure to the link graph. Define properties, collections, and actions on perspectives.

For deeper architecture details, see [references/architecture.md](references/architecture.md).

## Setup for AI Agents

See [references/setup.md](references/setup.md) for the full setup process.

**Quick version:**
```bash
# 1. Init (creates bootstrap seed — MUST run before first start)
ad4m-executor init --data-path ~/.ad4m

# 2. Run executor
ad4m-executor run --app-data-path ~/.ad4m --gql-port 12100 \
  --admin-credential <secret> --enable-mcp true

# 3. Generate agent (via CLI)
ad4m --executor-url http://localhost:12100/graphql agent generate --passphrase <pass>
```

## Interacting with AD4M

Two interfaces: **GraphQL** (port 12100) and **MCP** (port 3001).

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

### MCP Server

Enable with `--enable-mcp true`. See [references/mcp.md](references/mcp.md) for details.

The MCP server dynamically generates tools from SHACL subject class definitions. When SDNA defines a `Channel` class with properties `name` and `description`, tools like `channel_create`, `channel_get`, `channel_set_name` are auto-generated.

Core MCP tools: `perspective_create`, `perspective_list`, `add_link`, `get_links`, `get_models`, `agent_me`, `agent_unlock`.

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

The AD4M waker (`waker-bridge/ad4m-waker.js`) watches perspectives for changes and wakes your agent when new data arrives. Uses `PerspectiveProxy.subscribeSurrealDB()` — same mechanism as Flux UI.

**Setup:**
```bash
cd waker-bridge && npm install
node ad4m-waker.js --config waker-config.json
```

**Config** (`waker-config.json`):
```json
{
  "executorUrl": "ws://localhost:12100/graphql",
  "token": "your-admin-credential",
  "wakeUrl": "http://localhost:18789/hooks/wake",
  "wakeToken": "your-openclaw-hooks-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "my-messages",
      "perspective": "perspective-uuid",
      "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
    }
  ]
}
```

The MCP `subscribe_to_model` tool can generate appropriate SurrealQL queries for you. Run the waker in a `screen` session alongside the executor.

**Flow**: SurrealQL query change detected → waker sends wake to OpenClaw → agent reads messages via MCP tools.

## Common Workflows

1. **Personal knowledge graph**: Create perspective → add links → query links
2. **Collaborative space**: Create perspective → publish as neighbourhood → share URL → others join
3. **Structured data**: Add SHACL SDNA to perspective → use subject class tools
4. **AI agent integration**: Connect via MCP → discover models → create/query structured data
5. **Real-time chat**: Join neighbourhood → subscribe via waker → respond to messages on wake
