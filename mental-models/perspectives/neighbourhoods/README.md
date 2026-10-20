# Neighbourhoods

A **neighbourhood** (in the apps, a **space**) is a perspective shared between members through a link language. Its **neighbourhood expression** (the link language address plus meta links) is published at a `neighbourhood://<address>` URL. The executor creates a neighbourhood in one call that fixes its link language for the neighbourhood's life; the default is the server link language. Each neighbourhood keeps a **membership graph**, which says who is a member by person, and which every link language and the link server read through one **membrane interface**. Every member's executor holds its own copy of the perspective and syncs the shared links through the link language. The same link language carries **telepresence**: online status, signals and broadcasts.

**Why:** Each member keeps a full local copy, and the link language decides how copies converge, so the transport can differ per neighbourhood. Admission must not differ by transport: one membership graph in the perspective serves every link language, so a later move to another network needs no one admitted again. The executor, not the app, picks and fills the link language, so apps need no template knowledge and the choice is recorded once. Telepresence uses the same language, so calls need no second network.

**Parent:** [Perspectives](../README.md)

## Parts
- [Creating and joining](creating-and-joining.md): how a perspective becomes a neighbourhood, how an agent joins one, co-owners on one executor, and the neighbourhoods a device holds.
- [Membership](membership/README.md): the membership graph, its replay, the membrane interface, and the reads it filters.
- [Neighbourhood sync](sync.md): the perspective's sync loops and states, the commit queue, and inbound diffs.
- [Presence and signals](presence-and-signals.md): telepresence: online status, other agents, signals and broadcasts.
- [Neighbourhood calls](calls.md): audio and video calls among members, over mesh or the SFU.

<details><summary>How it works</summary>

```
executor A                                                      executor B
PerspectiveInstance ──commit(diff)─────────▶ link language ◀──sync()── PerspectiveInstance
   │  Membership replay ◀── membership channel (plaintext) ──▶  Membership replay  │
   │        └── membership* imports ──▶ (Languages)                                │
   ▲  └──────── set_online_status / send_signal / send_broadcast ──────────────────┘
   └── diff_from_link_language / telepresence_signal_from_link_language
```

1. `neighbourhood.create` (or `neighbourhood.join`) attaches a `Neighbourhood` to the `PerspectiveHandle` (`neighbourhood`, `shared_url`, `state`, `owners`), stores its `LinkLanguageChoice`, and starts the membership replay ([Creating and joining](creating-and-joining.md)).
2. The perspective loads the link language and tells it the local agents (`set_local_agents`).
3. Shared writes are committed; peer writes arrive as diffs; membership links travel on their own channel; online status, signals and broadcasts pass through the language's telepresence adapter.

Link languages themselves (the server link language with its link server and room keys, p-diff-sync on Holochain, templates) are in [Link languages](../../languages/link-languages/README.md). The neighbourhood expression is stored by the [neighbourhood language](../../languages/other-kinds/neighbourhood-and-perspective-languages.md).

</details>

<details><summary>Where in the code</summary>

- [neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): publish and join; [neighbourhoods/create.rs](../../../rust-executor/src/neighbourhoods/create.rs) (new): `neighbourhood.create`.
- [membership.rs](../../../rust-executor/src/membership.rs) (new): the membership replay per neighbourhood; [ad4m-identity-core/src/membership.rs](../../../ad4m-identity-core/src/membership.rs) (new): the replay rules.
- [api/neighbourhoods_ws.rs](../../../rust-executor/src/api/neighbourhoods_ws.rs): `neighbourhood.*` RPC methods.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): sync loops, `commit`, `diff_from_link_language`, telepresence methods.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): routing of link-language callbacks to the right perspective.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `Neighbourhood`, `PerspectiveState`, `OnlineAgent`.
- SDK: [core/src/neighbourhood/](../../../core/src/neighbourhood) (`NeighbourhoodClient`, `NeighbourhoodProxy`, and `Session`, the call class of [Neighbourhood calls](calls.md)).
- Docs: [docs-src/pages/neighbourhoods.mdx](../../../docs-src/pages/neighbourhoods.mdx).

</details>
