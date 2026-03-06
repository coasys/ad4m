# AD4M Architecture

## Agent-Centric Model

AD4M inverts the traditional app-centric paradigm. Instead of users creating accounts on applications, each agent (user or AI) runs their own AD4M executor — a personal semantic web node.

- **No central servers** — agents communicate P2P via Holochain
- **Data sovereignty** — all data lives in agent-controlled perspectives
- **Protocol evolvability** — languages abstract away storage, enabling migration without data loss

## Perspectives

A perspective is a subjective graph of links — a personal knowledge graph. Every piece of data in AD4M exists as links within perspectives.

```
Perspective "My Notes"
├── Link: (ad4m://self) --has_name--> (literal://string:Data)
├── Link: (ad4m://self) --has_role--> (literal://string:AI Agent)
└── Link: (did:key:z6Mk...) --authored--> (Qm...expression-hash)
```

Perspectives are local by default. Publishing a perspective as a neighbourhood makes it shared.

### Key Operations
- `perspectiveAdd(name)` — create
- `perspectiveQueryLinks(uuid, query)` — query links by source/predicate/target
- `perspectiveAddLink(uuid, link)` — add a link
- `perspectiveRemoveLink(uuid, link)` — remove a link

## Links

The fundamental data unit. An RDF-like triple:

```typescript
interface Link {
  source: string;      // URI — what the link is about
  predicate?: string;  // URI — the relationship type (optional)
  target: string;      // URI — what it points to
}
```

Wrapped as `LinkExpression` with metadata:
```typescript
interface LinkExpression {
  data: Link;          // The actual triple
  author: string;      // DID of creator
  timestamp: string;   // ISO datetime
  proof: object;       // Cryptographic signature
}
```

### URI Conventions
- `ad4m://self` — the perspective itself
- `literal://string:value` — inline string literal
- `literal://number:42` — inline number
- `literal://json:{"key":"value"}` — inline JSON
- `did:key:z6Mk...` — agent identity
- `Qm...` — content-addressed expression (language-specific)

## Languages

Protocol abstractions that handle expression storage and retrieval. A language defines:
- How to create expressions (write)
- How to retrieve expressions by address (read)
- Optionally: real-time sync via Holochain

Examples:
- **Holochain-based languages** — P2P shared state (used for neighbourhoods)
- **Note/IPFS language** — content-addressed immutable storage
- **Direct message language** — encrypted P2P messaging

Languages are installed from the bootstrap seed on first init.

## Neighbourhoods

A shared perspective. Created by publishing a local perspective with a link language (Holochain DNA for sync).

```
Agent A's Perspective ←──sync──→ Agent B's Perspective
         └──── Link Language (Holochain) ────┘
```

### Creating a Neighbourhood
1. Create a perspective
2. Publish with a link language: `neighbourhoodPublishFromPerspective(perspectiveUUID, linkLanguage, meta)`
3. Share the `neighbourhood://...` URL

### Joining a Neighbourhood
1. `neighbourhoodJoinFromUrl(url)` — downloads meta, installs required languages, syncs links

## Subject Classes (SHACL SDNA)

Subject classes impose structure on the link graph using SHACL (Shapes Constraint Language). They define schemas that map object-oriented concepts to link patterns.

### How It Works

A SHACL NodeShape defines a class. PropertyShapes define properties that map to specific link patterns in the graph.

```turtle
:MessageShape a sh:NodeShape ;
  sh:targetClass :Message ;
  sh:property [
    sh:path :body ;             # predicate used in links
    sh:datatype xsd:string ;
    sh:maxCount 1 ;             # scalar property
    sh:minCount 1 ;             # required
  ] ;
  sh:property [
    sh:path :reactions ;
    sh:class :Reaction ;        # typed collection
    # no maxCount = unbounded collection
  ] .
```

### Property Types

| SHACL constraint | Interpretation | Generated tools |
|-----------------|----------------|-----------------|
| `sh:maxCount 1` | Scalar property | `{class}_set_{prop}` |
| No `sh:maxCount` or `> 1` | Collection | `{class}_add_{prop}`, `{class}_remove_{prop}`, `{class}_get_{prop}` |
| `sh:minCount 1` | Required on creation | Included in `{class}_create` params |
| `sh:datatype xsd:string` | String value | Value stored as `literal://string:...` |
| `sh:class :Other` | Reference to another subject class | Value is a URI |

### Link Mapping

When you set `message.body = "Hello"` via a subject class:
```
Link: (<message-instance-uri>) --:body--> (literal://string:Hello)
```

When you add to a collection `channel.messages.add(msg)`:
```
Link: (<channel-instance-uri>) --:messages--> (<message-instance-uri>)
```

### SDNA in Perspectives

SHACL definitions are stored as links in the perspective itself:
```
Link: (ad4m://self) --ad4m://has_sdna--> (literal://json:<shacl-json>)
```

Query available models: `get_models` (MCP) or retrieve links with predicate `ad4m://has_sdna`.

## Built-in Services

- **AI Service** — local LLM inference, embeddings, Whisper transcription
- **Prolog Engine** — logic queries over perspectives (legacy, being replaced)
- **SurrealDB** — per-perspective document store for indexed queries
