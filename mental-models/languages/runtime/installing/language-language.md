# Language language

The **language language** is the system language that stores and serves other languages. Its expressions are language meta records (`LanguageMeta`), signed by the publisher. Its expression address is the language address, and `languageGetSource(address)` returns the bundle. The executor loads it first, from the bundle text inside the bootstrap seed, under alias `lang`.

**Why:** Install-by-address needs one place every executor can ask. When that place is a language, the store can change (central store, shared directory) without a change to the executor. The executor checks the bundle hash, so the store need not be trusted for integrity. The registry behind the default language language is also the only control on which languages reach a host: hosts run no language allowlist, and a tenant installs only what this registry holds from an author the host trusts ([Language trust and templates](trust-and-templates.md)).

**Parent:** [Installing languages](README.md)

<details><summary>How it works</summary>

| Variant | Storage | Used by |
|---|---|---|
| `bootstrap-languages/language-language` | HTTP: the language store, a Cloudflare Worker (`PROXY_URL` constant): `POST {key, value}` refuses an existing key; `GET ?key=` answers a signed URL; no delete route | the default `mainnet_seed.json` |
| `bootstrap-languages/local/language-language.js` | `ad4m:host` KV, or files in `storagePath` | local seed, Docker, integration tests |

1. Publish: `expressionCreate({bundle, meta})` hashes the bundle, signs `meta` with `agentCreateSignedExpression`, and stores `meta-<hash>` and then the bundle.
2. Get meta: `expressionGet(address)` returns the signed meta expression.
3. Get bundle: `languageGetSource(address)`.

On the default seed, every executor fetches every language from one Cloudflare Worker (`PROXY_URL`). The executor loads the language language itself from the seed's inline bundle text, not by address, so its address is the hash of the bundle that the seed carries.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/language-language/index.ts](../../../../bootstrap-languages/language-language/index.ts): default implementation.
- [bootstrap-languages/local/language-language.js](../../../../bootstrap-languages/local/language-language.js): local implementation.
- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `load_core_system_languages` (step 1), `get_language_language`, every `languageGetSource` / `expressionGet` call on it.
- [rust-executor/src/runtime_service/mod.rs](../../../../rust-executor/src/runtime_service/mod.rs): `get_language_language_bundle` (from the seed).

</details>
