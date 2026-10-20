# Agent language

The **agent language** is the expression language behind the `did` alias. Its expression address is a DID. Its expression is the agent profile (`{did, perspective, …}`), signed by that agent. `expression.get("did:key:…")` resolves to it. Through the [identity interface](identity-interface.md) it is also the executor's only route to **key logs** and the **key index**. It signs nothing and holds no key.

**Why:** Any agent must be able to look up another agent's profile from the DID alone. As a language, the directory can change: the default seed uses the [shared store](shared-store.md), and a Holochain version exists. A reader checks each profile's signature. What the profile contains and when it is published: [Agent profiles](../../../agents/identity/profiles.md).

**Parent:** [Other language kinds](../README.md)

## Parts
- [Identity interface](identity-interface.md): the `identity*` calls, their routes and errors, and the rules every implementation follows.
- [Token interface](token-interface.md): the `token*` calls for balances and transfers; interfaces only, answering `disabled`.
- [Shared store](shared-store.md): the `identity-store` service behind the default agent language.

<details><summary>How it works</summary>

| Package | Backend | Identity interface |
|---|---|---|
| `centralized-agent-language` (default seed) | HTTP, the [shared store](shared-store.md) (URL from the language's settings) | yes |
| `agent-language` | Holochain DNA | no |
| `local/agent-language.js` | `ad4m:host` KV, or files in `storagePath` | no |

1. Publish ([Agent profiles](../../../agents/identity/profiles.md)): the executor writes the profile as the agent, through `expression_create` for a `did:key` agent with no log and `identityPutAgent` for an agent with a log. All agents on the executor, main and managed, publish through the same agent-language isolate.
2. Resolve: `expressionGet(did)` reads the record for a `did:key` or `did:scid`, and the executor checks the proof ([Expressions and addresses](../../expressions/README.md)).

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/centralized-agent-language/index.ts](../../../../bootstrap-languages/centralized-agent-language/index.ts), [rust-impl/src/lib.rs](../../../../bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs): default agent language.
- [bootstrap-languages/agent-language/](../../../../bootstrap-languages/agent-language): Holochain agent language (`index.ts`, `rust-impl/`, `hc-dna/`).
- [bootstrap-languages/local/agent-language.js](../../../../bootstrap-languages/local/agent-language.js): local agent language.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`.
- [rust-executor/src/identity/backend.rs](../../../../rust-executor/src/identity/backend.rs) (new): `IdentityBackend`, the executor's one client of the identity interface.
- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `get_agent_language`; the `did` special case in `expression_create` and `parse_expr_url`.

</details>
