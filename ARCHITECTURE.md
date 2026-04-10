# AD4M Architecture

## System Diagram

```
┌─────────────────────────────────────────────┐
│ Applications (Flux, CLI, custom)            │
├─────────────────────────────────────────────┤
│ @coasys/ad4m-connect (auth + connection)    │
├─────────────────────────────────────────────┤
│ @coasys/ad4m (TypeScript SDK)               │
│  Ad4mClient → PerspectiveProxy → Ad4mModel  │
├──────────────┬──────────────────────────────┤
│ GraphQL API  │  REST API (/api/v1/)         │
├──────────────┴──────────────────────────────┤
│ ad4m-executor (Rust)                        │
│  ┌─────────────┬──────────┬───────────────┐ │
│  │ Perspectives │ SPARQL   │ Subscriptions │ │
│  │ (link store) │ (Oxigraph)│ (PubSub)     │ │
│  ├─────────────┼──────────┼───────────────┤ │
│  │ Languages   │ Holochain│ Deno Runtime  │ │
│  │ (plugins)   │ (DHT)    │ (JS execution)│ │
│  └─────────────┴──────────┴───────────────┘ │
└─────────────────────────────────────────────┘
```

## Core Concepts

- **Perspectives** — Containers for links. Each perspective is a local-first graph database (Oxigraph). Can be shared as neighbourhoods.
- **Links** — Subject-predicate-target triples with metadata (author, timestamp, status, proof). The atomic unit of data.
- **Languages** — Plugins for expression resolution. Each language handles a URI scheme (e.g., `did:`, `literal:`, IPFS hashes). Written in JS, executed in Deno runtime.
- **Neighbourhoods** — Shared perspectives synced via Holochain DHT. A neighbourhood = a perspective + a link language (Holochain DNA for gossip).

## Data Model

Links stored as named graphs in Oxigraph. Each link = 7 RDF triples in its own named graph.

Graph IRI: `ad4m://link/<sha256_hex>` (full 64-char hex of `sha256(source+predicate+target+timestamp)`)

Triples per link: source, predicate, target, author, timestamp, status, proof.

### Literal Format

Inline data encoded as URIs:
- `literal:json:{"data":"...","type":"..."}`
- `literal:string:...`

## Key Data Flows

```
Add link → SPARQL store insert → subscription notification → UI update
Query links → SPARQL SELECT → hydrate models → return to client
Share perspective → create neighbourhood → Holochain gossip syncs links
```

## Dependency Chain

```
core (tsc + rollup) → connect (esbuild re-bundles core) → Flux/apps
                    → ad4m-hooks
rust-executor (cargo build) → cli
                            → integration tests (use built binary)
```
