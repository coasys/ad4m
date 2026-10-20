# Perspectives

A **perspective** is a graph of signed **links** that the executor stores for one or more owning agents. Each link is signed by the **device key** of the agent that wrote it; the executor maps that key to its **person** (`did:scid`) through the key log and stores a **link verdict** per link. **Social DNA**, stored as links in the same graph, defines the shape of the data (subject classes) and its behaviour (flows, interpretation). A perspective created or joined as a **neighbourhood** (a space) syncs its shared links with the other members through a link language, and admits members by person through its **membership graph**.

**Why:** All app data is links, so one store, one query engine and one sync path serve every app. The schema is links too, so it syncs with the data and every member reads the same definitions. A link names the device key, not the person: links keep their format, a bot belongs to its owner by one rule, and the signature check needs the key log anyway, so finding the person costs no extra call.

**Parent:** [ADAM](../README.md)

## Parts
- [Links and store](links/README.md): the link types, anchors and verdicts, local and shared status, the Oxigraph store and the write sequence.
- [Querying](querying/README.md): the four ways to read a perspective, and which links each read sees.
- [Social DNA](social-dna/README.md): subject classes, flows and interpretation, all defined as links in the perspective.
- [Neighbourhoods](neighbourhoods/README.md): creating and joining neighbourhoods, membership by person, link sync, telepresence and calls.

<details><summary>How it works</summary>

```
caller (SDK / MCP / CLI)                          other executors
   │  perspective.* / neighbourhood.* RPC, guard Owns { Perspective }   ▲
   ▼                                                                    │ link language
PerspectiveInstance (one per uuid, in PERSPECTIVES)                     │ (see Languages)
   ├─ SparqlStore (Oxigraph, RocksDB, plain)  ◀── links + verdicts ─────┤
   │     └─ named graph coasys:membership  ◀── membership links ────────┤
   ├─ identity resolver: author_of, LinkVerdict (KelCache)              │
   ├─ Membership replay (neighbourhoods only)                           │
   ├─ shape cache  ◀── SHACL links                                      │
   ├─ live-query subscriptions                                          │
   ├─ background loops: link language, sync, commit queue, fallback sync,
   │                    live queries, notifications, auto-processor, flow pass at load
   └─ link_language: Option<Language>  ─────────────────────────────────┘
```

1. `perspectives::add_perspective` stores a `PerspectiveHandle` (uuid, name, shared URL, neighbourhood, state, owners) in the executor's SQLite file. The file is encrypted as a whole; name, neighbourhood and URL also sit in sealed columns under the perspective's own store key, wrapped once per owner ([Keyring and storage secret](../agents/executor/data/keyring.md)).
2. The instance opens its own Oxigraph store, only after the executor is unlocked. The store is plain on disk, behind file modes ([Oxigraph store](links/oxigraph-store.md)). It starts its background loops (`start_background_tasks`), which run on fixed intervals, also when the perspective is idle.
3. Callers reach it through RPC methods whose guard is `Owns { Perspective }`: the session's acting agent must be in `owners`. A perspective the caller may not reach answers 404, as a missing one does ([Guards](../agents/access/guards.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/perspectives/](../../rust-executor/src/perspectives): the whole subsystem. Start with its [AGENTS.md](../../rust-executor/src/perspectives/AGENTS.md).
- [perspectives/mod.rs](../../rust-executor/src/perspectives/mod.rs): registry `PERSPECTIVES`, `add_perspective`, `get_perspective`, `remove_perspective`, `import_perspective` / `export_perspective`, routing of link-language callbacks.
- [perspectives/perspective_instance.rs](../../rust-executor/src/perspectives/perspective_instance.rs): `PerspectiveInstance`, one `impl` block in a file of about 9,800 lines (tests included). It holds link writes, sync, SDNA, subjects, queries, subscriptions, telepresence and the auto-processor.
- [identity/](../../rust-executor/src/identity) (new): the key-log cache, `author_of` and the link verdicts ([Link verdicts](links/verdicts.md)).
- [membership.rs](../../rust-executor/src/membership.rs) (new): one membership replay per neighbourhood ([Membership](neighbourhoods/membership/README.md)).
- [types/domain.rs](../../rust-executor/src/types/domain.rs): `PerspectiveHandle`, `PerspectiveState`.
- [api/perspectives_ws.rs](../../rust-executor/src/api/perspectives_ws.rs): every `perspective.*` RPC method; [api/guards.rs](../../rust-executor/src/api/guards.rs): `Guard::perspective`.
- SDK: [core/src/perspectives/PerspectiveProxy.ts](../../core/src/perspectives/PerspectiveProxy.ts).
- Docs: [docs-src/pages/perspectives.mdx](../../docs-src/pages/perspectives.mdx).

</details>
