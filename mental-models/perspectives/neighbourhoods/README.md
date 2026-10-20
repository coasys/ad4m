# Neighbourhoods

A **neighbourhood** is a perspective shared between agents through a link language. Its **neighbourhood expression** (the link language address plus meta links) is published at a `neighbourhood://<address>` URL. Its **membership graph** says who is a member, by person. Every member's executor holds its own copy of the perspective and syncs the shared links through the link language. The same link language carries **telepresence**: online status, signals and broadcasts.

**Why:** A shared space has no owner server. Each member keeps a full local copy, and the link language decides how copies converge. So the transport (Holochain, a link server, another network) can differ per neighbourhood. Telepresence uses the same language, so calls need no second network.

**Parent:** [Perspectives](../README.md)

## Parts
- [Creating and joining](creating-and-joining.md): how a perspective becomes a neighbourhood, how an agent joins one, and co-owners on one executor.
- [Membership](membership/README.md): the membership graph, its replay, and the reads it filters.
- [Neighbourhood sync](sync.md): the perspective's sync loops and states, the commit queue, and inbound diffs.
- [Presence and signals](presence-and-signals.md): telepresence: online status, other agents, signals and broadcasts.
- [Neighbourhood calls](calls.md): audio and video calls among members, over mesh or the SFU.

<details><summary>How it works</summary>

```
executor A                                         executor B
PerspectiveInstance ──commit(diff)──▶ link language ◀──sync()── PerspectiveInstance
   ▲  │                               (Languages)                     │  ▲
   │  │  membership replay ── membership* imports ──▶ link language   │  │
   │  └──────── set_online_status / send_signal / send_broadcast ─────┘  │
   └── diff_from_link_language / telepresence_signal_from_link_language ─┘
```

1. `neighbourhood.create`, `neighbourhood.publish` or `neighbourhood.join` attaches a `Neighbourhood` to the `PerspectiveHandle` (`neighbourhood`, `shared_url`, `state`, `owners`) and starts the membership replay.
2. The perspective loads the link language and tells it the local agents (`set_local_agents`).
3. Shared writes are committed; peer writes arrive as diffs; online status, signals and broadcasts pass through the language's telepresence adapter.

The member list is the membership replay ([Membership](membership/README.md)). The executor knows its local owners (`owners`); the agents online come from the link language ([Presence and signals](presence-and-signals.md)).

Link languages themselves (p-diff-sync on Holochain, the server link language, templates) are in [Link languages](../../languages/link-languages/README.md). The neighbourhood expression is stored by the [neighbourhood language](../../languages/other-kinds/neighbourhood-and-perspective-languages.md).

</details>

<details><summary>Where in the code</summary>

- [neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): publish and join.
- [api/neighbourhoods_ws.rs](../../../rust-executor/src/api/neighbourhoods_ws.rs): `neighbourhood.*` RPC methods.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): sync loops, `commit`, `diff_from_link_language`, telepresence methods.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): routing of link-language callbacks to the right perspective.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `Neighbourhood`, `PerspectiveState`, `OnlineAgent`.
- SDK: [core/src/neighbourhood/](../../../core/src/neighbourhood) (`NeighbourhoodClient`, `NeighbourhoodProxy`, and `Session`, the call class of [Neighbourhood calls](calls.md)).
- Docs: [docs-src/pages/neighbourhoods.mdx](../../../docs-src/pages/neighbourhoods.mdx).

</details>
