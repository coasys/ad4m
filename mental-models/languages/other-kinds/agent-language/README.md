# Agent language

The **agent language** is the system language behind the `did` alias, and the executor's only route to identities. It does two jobs. As an expression language, it resolves a DID (`did:key:…` or `did:scid:…`) to that agent's signed **agent record** (the agent profile). Through the [identity interface](identity-interface.md), it reads and appends **key logs**, asks the **key index** who owns a key, and carries offers, inboxes and private records. In the default seed it runs on the [shared store](shared-store.md). It signs nothing and holds no key: it passes signed objects through.

**Why:** The executor must reach logs and profiles without knowing where they live: any store or network that serves the same calls with the same admission rules can carry them. A language gives that seam, chosen by the seed. Signing stays in the executor and the apps, so a language never holds a key and a store can withhold an event but never forge one. The development profile directory at `mock.ad4m.dev` lost: the shared store now serves the profiles of `did:key` agents too, each signed by its own key. What the profile holds: [Agent profiles](../../../agents/identity/profiles.md).

**Parent:** [Other language kinds](../README.md)

## Parts
- [Identity interface](identity-interface.md): the `identity*` calls, their routes and errors, and the rules every implementation follows.
- [Token interface](token-interface.md): the `token*` calls for balances and transfers; interfaces only, answering `disabled`.
- [Shared store](shared-store.md): the `identity-store` service behind the default agent language.

<details><summary>How it works</summary>

| Package | Backend | Identity interface |
|---|---|---|
| `centralized-agent-language` (default seed) | the shared store, URL from the language's settings | yes: TypeScript and a Rust ALDK port |
| `agent-language` | Holochain DNA | no |
| `local/agent-language.js` | `ad4m:host` KV, or files in `storagePath` | no |

Profiles:

1. Publish: the executor calls `expression_create(<agent language>, profile, ctx)` as the agent. All agents on the executor, main and managed, publish through the same agent-language isolate; `agentDid()` is the calling agent because `expression_create` passes its `AgentContext`. For a `did:key` agent with no log, `expressionCreate` sends `PUT /v1/agents/{did:key}` with a record the key signs. An agent with a log publishes through `identityPutAgent` with a signed request. So an agent record has two write paths: the expression capability for a `did:key` agent, the identity interface for an agent with a log.
2. Resolve: `expressionGet(did)` reads `GET /v1/agents/{did}`. A `did:key` record must verify against that key; a `did:scid` record against a key valid at the record's position in the log.
3. The agent record keeps `did` as the device `did:key`; the new field `identity` carries the person's `did:scid` ([Agent profiles](../../../agents/identity/profiles.md)).

The store URL comes from `languageSettings()`. The executor fills it from its config key `identity.storeUrl`, which the seed sets ([Bootstrap languages](../../runtime/bootstrap-languages.md)).

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/centralized-agent-language/index.ts](../../../../bootstrap-languages/centralized-agent-language/index.ts), [rust-impl/src/lib.rs](../../../../bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs): the default agent language, both implementations.
- [bootstrap-languages/agent-language/](../../../../bootstrap-languages/agent-language): Holochain agent language (`index.ts`, `rust-impl/`, `hc-dna/`).
- [bootstrap-languages/local/agent-language.js](../../../../bootstrap-languages/local/agent-language.js): local agent language.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`.
- [rust-executor/src/identity/backend.rs](../../../../rust-executor/src/identity/backend.rs) (new): `IdentityBackend`, the executor's one client of the identity interface.
- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `get_agent_language`; the `did` special case in `expression_create` and `parse_expr_url`.

</details>
