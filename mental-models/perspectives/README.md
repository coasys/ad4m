# Perspectives

A **perspective** is a graph of signed **links** that the executor stores for one or more owning agents. **Social DNA**, stored as links in the same graph, defines the shape of the data (subject classes) and its behaviour (flows, interpretation). A perspective created or joined as a **neighbourhood** syncs its shared links with other agents through a link language, and admits members by person through its **membership graph**.

**Why:** All app data is links, so one store, one query engine and one sync path serve every app. The schema is links too, so it syncs with the data and every member reads the same definitions.

**Parent:** [ADAM](../README.md)

## Parts
- [Links and store](links/README.md): the link types, local and shared status, the Oxigraph store and the write sequence.
- [Querying](querying/README.md): the four ways to read a perspective: link queries, SPARQL, model queries and live queries.
- [Social DNA](social-dna/README.md): subject classes, flows and interpretation, all defined as links in the perspective.
- [Neighbourhoods](neighbourhoods/README.md): creating and joining, membership, link sync, telepresence and calls.

<details><summary>How it works</summary>

```
caller (SDK / MCP / CLI)                      other executors
   │  perspective.* / neighbourhood.* RPC            ▲
   ▼                                                  │ link language
PerspectiveInstance (one per uuid, in PERSPECTIVES)   │ (see Languages)
   ├─ SparqlStore (Oxigraph, RocksDB)  ◀── links ─────┤
   ├─ membership replay (neighbourhoods only)         │
   ├─ shape cache  ◀── SHACL links                    │
   ├─ live-query subscriptions                         │
   ├─ background loops: link language, sync, commit queue, fallback sync,
   │                    live queries, notifications, auto-processor, flow pass at load
   └─ link_language: Option<Language>  ───────────────┘
```

1. `perspectives::add_perspective` stores a `PerspectiveHandle` (uuid, name, shared URL, neighbourhood, state, owners) in SQLite and puts a `PerspectiveInstance` in the global registry `PERSPECTIVES`.
2. The instance opens its own Oxigraph store and starts its background loops (`start_background_tasks`). The link-language and notification loops wait for an event (a publish, a link change); the sync, commit-queue, fallback-sync, live-query and auto-processor loops poll on fixed intervals, also when the perspective is idle.
3. Callers reach it through the RPC methods. Each method's guard checks the caller against the handle's `owners` ([Guards](../agents/access/guards.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/perspectives/](../../rust-executor/src/perspectives): the whole subsystem. Start with its [AGENTS.md](../../rust-executor/src/perspectives/AGENTS.md).
- [perspectives/mod.rs](../../rust-executor/src/perspectives/mod.rs): registry `PERSPECTIVES`, `add_perspective`, `get_perspective`, `remove_perspective`, `import_perspective` / `export_perspective`, routing of link-language callbacks.
- [perspectives/perspective_instance.rs](../../rust-executor/src/perspectives/perspective_instance.rs): `PerspectiveInstance`, one `impl` block in a file of about 9,900 lines (tests included). It holds link writes, sync, SDNA, subjects, queries, subscriptions, telepresence and the auto-processor.
- [types/domain.rs](../../rust-executor/src/types/domain.rs): `PerspectiveHandle`, `PerspectiveState`.
- [api/perspectives_ws.rs](../../rust-executor/src/api/perspectives_ws.rs): every `perspective.*` RPC method; [api/guards.rs](../../rust-executor/src/api/guards.rs): `Guard::perspective`.
- SDK: [core/src/perspectives/PerspectiveProxy.ts](../../core/src/perspectives/PerspectiveProxy.ts).
- Docs: [docs-src/pages/perspectives.mdx](../../docs-src/pages/perspectives.mdx).

</details>
