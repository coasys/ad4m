# Other language kinds

Besides link languages, ADAM uses **expression languages**. An expression language exports `expressionGet` and usually `expressionCreate`, and stores one kind of signed data. Four have a fixed role in the executor and are reached by alias: the language, agent, neighbourhood and perspective languages. The rest are ordinary languages that apps install by address.

**Why:** The executor needs a few well-known stores: who is this DID, what is this neighbourhood, which code is at this address. It does not hard-code where they live. Each store is a language that the bootstrap seed chooses, so the choice can differ per network.

**Parent:** [Languages](../README.md)

## Parts
- [Agent languages](agent-languages.md): the `did` alias: resolve a DID to the agent profile.
- [Neighbourhood and perspective languages](neighbourhood-and-perspective-languages.md): store neighbourhood expressions, which name a link language; the perspective language stores nothing.
- [File storage languages](file-storage-languages.md): store files as expressions.

The language language has its own page under [Installing languages](../runtime/installing/README.md).

<details><summary>How it works</summary>

| Package | Kind | Backend | In the default seed |
|---|---|---|---|
| `language-language` | language store | Cloudflare KV | yes, alias `lang` ([Language language](../runtime/installing/language-language.md)) |
| `centralized-agent-language` | agent | HTTP | yes, alias `did` |
| `agent-language` | agent | Holochain | no |
| `neighbourhood-language` | neighbourhood | Cloudflare KV | yes, alias `neighbourhood` |
| `perspective-language` | perspective | none: stores nothing | yes, alias `perspective` |
| `file-storage` | files | Holochain | also published |
| `centralized-file-storage` | files | Cloudflare KV | also published |
| `embedding-vector-language` | embedding vectors | none: the address is the deflated, base64 content | also published |
| `eas` | Ethereum attestations (read-only) | easscan.org HTTP | also published |

What the executor needs from each fixed-role language:

| Alias | Executor call that uses it |
|---|---|
| `lang` | `install_language` and `language_by_ref` fetch meta and bundles; `language.publish` and `applyTemplate` store them |
| `did` | `publish_agent_to_language` stores profiles; `agent.byDid` and `expression.get("did:…")` read them |
| `neighbourhood` | `create_neighbourhood_with_context` on publish; `get_neighbourhood` on join |
| `perspective` | none: the executor loads it and registers the alias; the language stores nothing |

The HTTP languages keep their back-end hosts (the development profile directory `mock.ad4m.dev`, a Cloudflare Worker) as constants in their source, not as settings. Some import helper modules from `esm.sh` or `deno.land` URLs, which esbuild bundles at build time.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/](../../../bootstrap-languages): all packages above.
- [bootstrap-languages/embedding-vector-language/index.ts](../../../bootstrap-languages/embedding-vector-language/index.ts), [bootstrap-languages/eas/index.ts](../../../bootstrap-languages/eas/index.ts): the two languages without a child page.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `get_agent_language`, `get_language_language`, `get_neighbourhood_language`, `get_perspective_language`.
- [cli/seed_proto.json](../../../cli/seed_proto.json): which packages the default seed publishes.

</details>