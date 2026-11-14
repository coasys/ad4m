# Installing languages

**Installing** a language takes four steps:

1. Get the bundle, from disk or from the language language by address.
2. Check that the bundle hashes to the address.
3. Save it as `<data>/ad4m/languages/<address>/bundle.js`.
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
| `LanguageController::install_language(addr)` | neighbourhood publish, `ensure_link_language`, the background load after `applyTemplate` and the MCP clone, `language_by_ref` | hash only. One install per address at a time; a second caller waits, then finds the language loaded. A bundle on disk is only hash-checked. Otherwise one source request, then one meta request, each with a 30 s timeout (`LANGUAGE_FETCH_TIMEOUT`). A missing source fails the install; a missing meta does not |
| `install_language_from_address(addr, true)` | system languages, known link languages at boot | hash only; one attempt |
| `load_installed_languages` | boot | none: loads every `languages/<dir>/bundle.js` |

Join does not install the link language. `install_neighbourhood_with_context` creates the perspective with state `NeighbourhoodJoinInitiated` and returns. `ensure_link_language` then installs it. A failed install sets `LinkLanguageFailedToInstall` and is retried after 0.5 s, doubling up to 30 s ([Neighbourhood sync](../../../perspectives/neighbourhoods/sync.md)).

Publishing:

1. `language.publish` (refused to user sessions) reads `languagePath` from the executor's file system, hashes it, and calls `expressionCreate({bundle, meta})` on the language language as the main agent.
2. `language.applyTemplate` builds a templated copy and publishes it as the caller (`ctx.acts_as()`), so managed agents can create neighbourhoods.
3. `language.publish` then loads the new bundle before it returns. `language.applyTemplate` returns the address first and loads the bundle in the background through `install_language`. A neighbourhood publish with that address waits for this install.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `install_language_from_address`, `load_installed_languages`, `install_language`, `language_by_ref`, `save_language_bundle`, `language_remove`.
- [rust-executor/src/languages/conductor_languages.rs](../../../../rust-executor/src/languages/conductor_languages.rs): `load_link_and_installed_languages`.
- [rust-executor/src/api/languages_ws.rs](../../../../rust-executor/src/api/languages_ws.rs): `publish_language`, `apply_template`, `remove_language`, `write_settings`.
- [rust-executor/src/neighbourhoods.rs](../../../../rust-executor/src/neighbourhoods.rs): the install call on publish (`neighbourhood_publish_from_perspective_with_context`).
- [rust-executor/src/perspectives/perspective_instance.rs](../../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_link_language`, which installs the link language after a join.
- [rust-executor/src/types/domain.rs](../../../../rust-executor/src/types/domain.rs): `LanguageMeta`, `LanguageLanguageInput`.
- [cli/src/languages.rs](../../../../cli/src/languages.rs): CLI `languages` commands.

</details>