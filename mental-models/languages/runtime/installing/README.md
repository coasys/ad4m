# Installing languages

**Installing** a language takes four steps:

1. Get the bundle, from disk or from the language language by address.
2. Check that the bundle hashes to the address.
3. Save it as `<data>/languages/<address>/bundle.js`.
4. Load it into an isolate.

**Publishing** stores a bundle and its language meta (`LanguageMeta`) in the language language, so other executors can install it.

**Why:** Executors share code only by address. An executor that fetches a bundle can prove it got the right bytes, so it does not have to trust the store for integrity. Whether to *run* the code is a separate decision.

**Parent:** [Language runtime](../README.md)

## Parts
- [Language trust and templates](trust-and-templates.md): which authors' languages the executor accepts, and how templating makes a checkable copy per neighbourhood.
- [Language language](language-language.md): the system language that stores bundles and their language meta, and the stores behind it.

<details><summary>How it works</summary>

Four install paths exist:

| Path | Used by | Checks |
|---|---|---|
| `language_by_ref(addr)` | RPC `language.get`, model queries, MCP profile tool | proof valid; author trusted, or a template of a trusted source; then `install_language` |
| `LanguageController::install_language(addr)` | neighbourhood publish and join, the `ensure_link_language` retry loop | hash only; up to 10 attempts each for meta and bundle, sleeping 5 s × n after each miss |
| `install_language_from_address(addr, true)` | system languages, known link languages at boot | hash only; one attempt |
| `load_installed_languages` | boot | none: loads every `languages/<dir>/bundle.js` |

When the link-language install fails on join, `install_neighbourhood_with_context` still creates the perspective, with state `LinkLanguageFailedToInstall`. `ensure_link_language` then retries the install after 0.5 s, doubling up to 30 s ([Neighbourhood sync](../../../perspectives/neighbourhoods/sync.md)).

Publishing:

1. `language.publish` (refused to user sessions) reads `languagePath` from the executor's file system, hashes it, and calls `expressionCreate({bundle, meta})` on the language language as the main agent.
2. `language.applyTemplate` builds a templated copy and publishes it as the caller (`ctx.acts_as()`), so managed agents can create neighbourhoods.
3. Both then load the new bundle locally.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `install_language_from_address`, `load_installed_languages`, `install_language`, `language_by_ref`, `save_language_bundle`, `language_remove`.
- [rust-executor/src/languages/conductor_languages.rs](../../../../rust-executor/src/languages/conductor_languages.rs): `load_link_and_installed_languages`.
- [rust-executor/src/api/languages_ws.rs](../../../../rust-executor/src/api/languages_ws.rs): `publish_language`, `apply_template`, `remove_language`, `write_settings`.
- [rust-executor/src/neighbourhoods.rs](../../../../rust-executor/src/neighbourhoods.rs): install calls on publish and join (`install_neighbourhood_with_context`).
- [rust-executor/src/types/domain.rs](../../../../rust-executor/src/types/domain.rs): `LanguageMeta`, `LanguageLanguageInput`.
- [cli/src/languages.rs](../../../../cli/src/languages.rs): CLI `languages` commands.

</details>