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

- `ad4m_add_perspective(name)` — create a new perspective
- `ad4m_query_links(perspective_id, source?, predicate?, target?)` — query links by source/predicate/target
- `ad4m_add_link(perspective_id, source, predicate, target)` — add a link
- `ad4m_add_model(perspective_id, class_name, shacl_json)` — register a subject class schema

## Links

The fundamental data unit. An RDF-like triple:

```typescript
interface Link {
  source: string; // URI — what the link is about
  predicate?: string; // URI — the relationship type (optional)
  target: string; // URI — what it points to
}
```

Wrapped as `LinkExpression` with metadata:

```typescript
interface LinkExpression {
  data: Link; // The actual triple
  author: string; // DID of creator
  timestamp: string; // ISO datetime
  proof: object; // Cryptographic signature
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
2. Publish with a link language: `ad4m_neighbourhoodPublishFromPerspective(perspectiveUUID, linkLanguage, meta)`
3. Share the `neighbourhood://...` URL

### Joining a Neighbourhood

1. `ad4m_neighbourhoodJoinFromUrl(url)` — downloads meta, installs required languages, syncs links

## Subject Classes (SHACL SDNA)

Subject classes impose structure on the link graph using SHACL (Shapes Constraint Language). They define schemas that map object-oriented concepts to link patterns.

### How It Works

Classes are registered via the `ad4m_add_model` MCP tool (or `add_sdna()` in Rust) using a JSON representation of a SHACL shape. The JSON is parsed by `SHACLShape` / `PropertyShape` structs and converted to RDF links in the perspective.

### SHACL JSON Format

```json
{
  "target_class": "message://Message",
  "constructor_actions": [
    {
      "action": "addLink",
      "source": "this",
      "predicate": "rdf://type",
      "target": "message://Message"
    }
  ],
  "destructor_actions": [
    {
      "action": "removeLink",
      "source": "this",
      "predicate": "rdf://type",
      "target": "message://Message"
    }
  ],
  "properties": [
    {
      "path": "message://body",
      "name": "body",
      "datatype": "xsd://string",
      "min_count": 1,
      "max_count": 1,
      "writable": true,
      "setter": [
        {
          "action": "setSingleTarget",
          "source": "this",
          "predicate": "message://body",
          "target": "value"
        }
      ]
    },
    {
      "path": "message://reactions",
      "name": "reactions",
      "node_kind": "IRI",
      "collection": true,
      "adder": [
        {
          "action": "addLink",
          "source": "this",
          "predicate": "message://reactions",
          "target": "value"
        }
      ],
      "remover": [
        {
          "action": "removeLink",
          "source": "this",
          "predicate": "message://reactions",
          "target": "value"
        }
      ]
    }
  ]
}
```

### Top-Level Fields

| Field                 | Type            | Description                                                                                                     |
| --------------------- | --------------- | --------------------------------------------------------------------------------------------------------------- |
| `target_class`        | string (URI)    | Fully qualified class URI. The scheme becomes the namespace (e.g. `message://Message` → namespace `message://`) |
| `constructor_actions` | AD4MAction[]    | Link operations executed when creating an instance                                                              |
| `destructor_actions`  | AD4MAction[]    | Link operations executed when deleting an instance                                                              |
| `properties`          | PropertyShape[] | Property definitions (see below)                                                                                |

### PropertyShape Fields

| Field              | Type         | Description                                                            |
| ------------------ | ------------ | ---------------------------------------------------------------------- |
| `path`             | string (URI) | Predicate URI used in links for this property                          |
| `name`             | string?      | Property name (derived from `path` if omitted)                         |
| `datatype`         | string?      | Value type constraint, e.g. `xsd://string`, `xsd://dateTime`           |
| `min_count`        | number?      | Minimum cardinality. `1` = required on creation                        |
| `max_count`        | number?      | Maximum cardinality. `1` = scalar property. Omit or `> 1` = collection |
| `writable`         | bool?        | Whether the property can be updated after creation                     |
| `collection`       | bool?        | Explicit collection flag (alternative to omitting `max_count`)         |
| `node_kind`        | string?      | `"IRI"` for references to other entities, `"Literal"` for values       |
| `local`            | bool?        | If true, links are stored locally (not shared in neighbourhood)        |
| `resolve_language` | string?      | Language to use when resolving expression URIs (e.g. `"literal"`)      |
| `setter`           | AD4MAction[] | Actions for setting a scalar property value                            |
| `adder`            | AD4MAction[] | Actions for adding to a collection                                     |
| `remover`          | AD4MAction[] | Actions for removing from a collection                                 |

### AD4MAction Fields

| Field       | Type   | Description                                                    |
| ----------- | ------ | -------------------------------------------------------------- |
| `action`    | string | Operation: `"addLink"`, `"removeLink"`, or `"setSingleTarget"` |
| `source`    | string | `"this"` (instance URI) or a literal URI                       |
| `predicate` | string | Predicate URI for the link                                     |
| `target`    | string | `"value"` (substituted at runtime) or a literal URI            |
| `local`     | bool?  | If true, the link is local-only                                |

### Generated MCP Tools

Once registered, dynamic tools are auto-generated:

| Property type                                     | Generated tools                               |
| ------------------------------------------------- | --------------------------------------------- |
| Scalar (`max_count: 1`)                           | `{class}_set_{prop}`                          |
| Collection (`collection: true` or no `max_count`) | `{class}_add_{prop}`, `{class}_remove_{prop}` |
| Required (`min_count: 1`)                         | Parameter included in `{class}_create`        |

### Link Mapping

When you set `message.body = "Hello"` via a subject class:

```
Link: (<message-instance-uri>) --message://body--> (literal://string:Hello)
```

When you add to a collection `message.reactions.add(uri)`:

```
Link: (<message-instance-uri>) --message://reactions--> (<reaction-uri>)
```

### SDNA Storage in Perspectives

SHACL definitions are decomposed into RDF links in the perspective. Key link patterns:

```
(ad4m://self) --ad4m://has_shacl--> (literal://string:shacl://Message)
(literal://string:shacl://Message) --ad4m://shacl_shape_uri--> (message://MessageShape)
(message://Message) --rdf://type--> (ad4m://SubjectClass)
(message://MessageShape) --sh://property--> (message://Message.body)
```

Query available models: `get_models` (MCP) or retrieve links with predicate `ad4m://has_shacl`.

## Built-in Services

- **AI Service** — local LLM inference, embeddings, Whisper transcription
- **Prolog Engine** — logic queries over perspectives (legacy, being replaced)
- **SurrealDB** — per-perspective document store for indexed queries
