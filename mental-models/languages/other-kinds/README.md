# Other language kinds

Besides link languages, ADAM uses **expression languages**. An expression language exports `expressionGet` and usually `expressionCreate`, and stores one kind of signed data. Four have a fixed role in the executor and are reached by alias: the language, agent, neighbourhood and perspective languages. The **agent language** also exports the identity interface, and only the executor calls it there. The rest are ordinary languages that apps install by address.

**Why:** The executor needs a few well-known stores: who is this DID and which keys speak for it, what is this neighbourhood, which code is at this address. It does not hard-code where they live. Each store is a language that the bootstrap seed chooses, so the choice can differ per network.

**Parent:** [Languages](../README.md)

## Parts
- [Agent language](agent-language/README.md): the `did` alias and the executor's route to key logs, the key index and agent records.
- [Neighbourhood and perspective languages](neighbourhood-and-perspective-languages.md): store neighbourhood expressions, which name a link language; the perspective language stores nothing.
- [File storage languages](file-storage-languages.md): store files as expressions.

The language language has its own page under [Installing languages](../runtime/installing/README.md).

<details><summary>How it works</summary>

| Package | Kind | Backend | In the default seed |
|---|---|---|---|
| `language-language` | language store | the language store Worker | yes, alias `lang` ([Language language](../runtime/installing/language-language.md)) |
| `centralized-agent-language` | agent, identity | the shared store | yes, alias `did` |
| `agent-language` | agent | Holochain | no |
| `neighbourhood-language` | neighbourhood | the language store Worker | yes, alias `neighbourhood` |
| `perspective-language` | perspective | none: stores nothing | yes, alias `perspective` |
| `file-storage` | files | Holochain | also published |
| `centralized-file-storage` | files | the language store Worker | also published |
| `embedding-vector-language` | embedding vectors | none: the address is the deflated, base64 content | also published |
| `eas` | Ethereum attestations (read-only) | easscan.org HTTP | also published |

What the executor needs from each fixed-role language:

| Alias | Executor call that uses it |
|---|---|
| `lang` | `install_language` and `language_by_ref` fetch meta and bundles; `language.publish`, `applyTemplate` and `neighbourhood.create` store them |
| `did` | `publish_agent_to_language` stores profiles; `agent.byDid` and `expression.get("did:…")` read them; `IdentityBackend` makes every `identity*` call |
| `neighbourhood` | `create_neighbourhood_with_context` on create and publish; `get_neighbourhood` on join |
| `perspective` | none: the executor loads it and registers the alias; the language stores nothing |

Only the agent language reads its back-end host from its settings. The other HTTP languages keep their host (the language store Worker) as a constant in their source. Some import helper modules from `esm.sh` or `deno.land` URLs, which esbuild bundles at build time.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/](../../../bootstrap-languages): all packages above.
- [bootstrap-languages/embedding-vector-language/index.ts](../../../bootstrap-languages/embedding-vector-language/index.ts), [bootstrap-languages/eas/index.ts](../../../bootstrap-languages/eas/index.ts): the two languages without a child page.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `get_agent_language`, `get_language_language`, `get_neighbourhood_language`, `get_perspective_language`.
- [cli/seed_proto.json](../../../cli/seed_proto.json): which packages the default seed publishes.

</details>
