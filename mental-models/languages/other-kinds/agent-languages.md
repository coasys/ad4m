# Agent languages

An **agent language** is the expression language behind the `did` alias. Its expression address is a DID. Its expression is the agent profile (`{did, perspective, …}`), signed by that agent. `expression.get("did:key:…")` resolves to it.

**Why:** Any agent must be able to look up another agent's profile from the DID alone. As a language, the directory can change: the default seed uses an HTTP directory, and a Holochain version exists. The default agent language stores each profile as an agent record on the identity store ([identity-store/](../../../identity-store)), and serves the identity calls on that store too; a reader checks each record's signature. What the profile contains and when it is published: [Agent profiles](../../agents/identity/profiles.md).

**Parent:** [Other language kinds](README.md)

<details><summary>How it works</summary>

| Package | Backend | Built with |
|---|---|---|
| `centralized-agent-language` (default seed) | HTTP, the identity store's `/v1/agents/{did}` at the `storeUrl` setting, which the executor writes from `identity.storeUrl` or the network seed (`install_agent_language`) | Rust ALDK → WASM |
| `agent-language` | Holochain DNA | TypeScript `index.ts` (name `agent-expression-store`) and a Rust implementation |
| `local/agent-language.js` | `ad4m:host` KV, or files in `storagePath` | plain JS (name `local-agent-store`) |

Publish (when and what: [Agent profiles](../../agents/identity/profiles.md)):

1. The executor calls `expression_create(<agent language>, profile, ctx)` as the agent. All agents on the executor, main and managed, publish through the same agent-language isolate.
2. In the language, `expressionCreate` checks `content.did == agentDid()`. `agentDid()` is the calling agent because `expression_create` passes its `AgentContext`. It refuses a `did:scid` agent: the executor publishes a person's profile through `identityPutAgent` (`publish_person_profile`).
3. It signs the profile with `agentCreateSignedExpression`, wraps it in an agent record signed with `agentSign`, and PUTs it to `/v1/agents/{did}`; on a 409 it retries once just above the stored version.

Resolve:

1. `expressionGet(did)` GETs `/v1/agents/{did}` and keeps the record only when its signature holds: by the `did:key` itself, or for a `did:scid` by a device key that the person's key log lists at the record's anchor. A `did:scid` answer adds `proof.record`.
2. The executor checks the proof ([Expressions and addresses](../expressions/README.md)).

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs](../../../bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs): default agent language; [agents.rs](../../../bootstrap-languages/centralized-agent-language/rust-impl/src/agents.rs): `create`, `get`; [store.rs](../../../bootstrap-languages/centralized-agent-language/rust-impl/src/store.rs): the identity calls, `storeUrl`.
- [bootstrap-languages/agent-language/](../../../bootstrap-languages/agent-language): Holochain agent language (`index.ts`, `rust-impl/`, `hc-dna/`).
- [bootstrap-languages/local/agent-language.js](../../../bootstrap-languages/local/agent-language.js): local agent language.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`, `publish_profile`.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `get_agent_language`, `install_agent_language`, `agent_language_for`; the `did` special case in `expression_create` and `parse_expr_url`.

</details>