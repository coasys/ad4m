# Language language

The **language language** is the system language that stores and serves other languages. Its expressions are language meta records (`LanguageMeta`), signed by the publisher. Its expression address is the language address, and `languageGetSource(address)` returns the bundle. The executor loads it first, from the bundle text inside the bootstrap seed, under alias `lang`.

**Why:** Install-by-address needs one place every executor can ask. When that place is a language, the store can change (central store, shared directory) without a change to the executor. The executor checks the bundle hash, so the store need not be trusted for integrity.

**Parent:** [Installing languages](README.md)

<details><summary>How it works</summary>

| Variant | Storage | Used by |
|---|---|---|
| `bootstrap-languages/language-language` | HTTP: a Cloudflare Workers KV (key-value) proxy (`PROXY_URL` constant) | the default `mainnet_seed.json` |
| `bootstrap-languages/local/language-language.js` | `ad4m:host` KV, or files in `storagePath` | local seed, Docker, integration tests |

1. Publish: `expressionCreate({bundle, meta})` hashes the bundle and signs `meta` with `agentCreateSignedExpression`. The default variant stores the bundle and then `meta-<hash>`; an existing key counts as stored, and any other store failure throws. The local variant stores `meta-<hash>` and then `bundle-<hash>`, so a failed bundle write leaves the meta behind.
2. Get meta: `expressionGet(address)` returns the signed meta expression. The default variant keeps each meta it reads in memory and answers later reads from there. A miss is not kept, so the next read asks again, and two reads of an uncached meta at the same time both go to the proxy. The local variant reads its storage on every call.
3. Get bundle: `languageGetSource(address)`.

On the default seed, every executor fetches every language from one Cloudflare Worker (`PROXY_URL`). The executor loads the language language itself from the seed's inline bundle text, not by address, so its address is the hash of the bundle that the seed carries.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/language-language/index.ts](../../../../bootstrap-languages/language-language/index.ts): default implementation.
- [bootstrap-languages/local/language-language.js](../../../../bootstrap-languages/local/language-language.js): local implementation.
- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `load_core_system_languages` (step 1), `get_language_language`, every `languageGetSource` / `expressionGet` call on it.
- [rust-executor/src/runtime_service/mod.rs](../../../../rust-executor/src/runtime_service/mod.rs): `get_language_language_bundle` (from the seed).

</details>