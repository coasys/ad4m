# Agent languages

An **agent language** is the expression language behind the `did` alias. Its expression address is a DID. Its expression is the agent profile (`{did, perspective, …}`), signed by that agent. `expression.get("did:key:…")` resolves to it.

**Why:** Any agent must be able to look up another agent's profile from the DID alone. As a language, the directory can change: the default seed uses an HTTP directory, and a Holochain version exists. The default agent language posts profiles to a development profile directory (`mock.ad4m.dev`). The server behind it is not in this repository; a reader checks each profile's signature. What the profile contains and when it is published: [Agent profiles](../../agents/identity/profiles.md).

**Parent:** [Other language kinds](README.md)

<details><summary>How it works</summary>

| Package | Backend | Built with |
|---|---|---|
| `centralized-agent-language` (default seed) | HTTP, development profile directory `https://mock.ad4m.dev/agent` (constant `ENDPOINT`) | Rust ALDK → WASM |
| `agent-language` | Holochain DNA | TypeScript `index.ts` (name `agent-expression-store`) and a Rust implementation |
| `local/agent-language.js` | `ad4m:host` KV, or files in `storagePath` | plain JS (name `local-agent-store`) |

Publish (when and what: [Agent profiles](../../agents/identity/profiles.md)):

1. The executor calls `expression_create(<agent language>, profile, ctx)` as the agent. All agents on the executor, main and managed, publish through the same agent-language isolate.
2. In the language, `expressionCreate` checks `content.did == agentDid()`. `agentDid()` is the calling agent because `expression_create` passes its `AgentContext`.
3. It signs with `agentCreateSignedExpression` and POSTs `{did, expression}`.

Resolve:

1. `expressionGet(did)` GETs `ENDPOINT?did=<did>`.
2. The executor checks the proof ([Expressions and addresses](../expressions/README.md)).

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs](../../../bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs): default agent language.
- [bootstrap-languages/agent-language/](../../../bootstrap-languages/agent-language): Holochain agent language (`index.ts`, `rust-impl/`, `hc-dna/`).
- [bootstrap-languages/local/agent-language.js](../../../bootstrap-languages/local/agent-language.js): local agent language.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `get_agent_language`; the `did` special case in `expression_create` and `parse_expr_url`.

</details>