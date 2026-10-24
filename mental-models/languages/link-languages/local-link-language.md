# Local link language

The **local link language** (`local-link-language`) is a link language with no network. It keeps the links of one neighbourhood in memory and in the `ad4m:host` key-value store, and reports itself synced. Links never leave the executor, so agents on two executors cannot share a neighbourhood through it. It is one of the **local bootstrap languages**: plain JavaScript system languages that run ADAM with no Holochain and no central services.

**Why:** Tests, Docker images and standalone executors need neighbourhoods that work without a Holochain DHT or a server. The SFU end-to-end tests and the local integration tests use it, so they do not depend on Holochain timing. Those tests check executor logic, not sync.

**Parent:** [Link languages](README.md)

<details><summary>How it works</summary>

1. A `//!@ad4m-template-variable` `uid` constant gives each neighbourhood its own copy, address and storage.
2. The language imports only `storageGet`, `storagePut`, `agentDid` and `emitPerspectiveDiff`.
3. `perspectiveCommit` applies the diff to the stored link list, increments a revision counter and saves to the KV store (`link-state`). It emits nothing.
4. The first `perspectiveSyncSync` after load emits all stored links once with `emitPerspectiveDiff`; later calls do nothing. `peersRemote` returns `[]`.
5. Shared-directory mode is for the sibling languages, not this one:
   - The language language, neighbourhood language and agent language in `bootstrap-languages/local/` accept a `storagePath` setting.
   - With it, they read and write files in that directory (the optional File I/O extension).
   - The directory must lie inside the executor's working directory: only there may a system language write (`JsCore::new_for_language` in [js_core/mod.rs](../../../rust-executor/src/js_core/mod.rs)).
   - Several executors that point at one directory see each other's languages, neighbourhoods and profiles.
   - The link language has no such mode.

Run with `ad4m-executor run --run-holochain false` and a seed from `node generate-seed.mjs . dist`.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/local/link-language.js](../../../bootstrap-languages/local/link-language.js): the link language.
- [bootstrap-languages/local/README.md](../../../bootstrap-languages/local/README.md): shared-directory mode and seed generation.
- [bootstrap-languages/local/generate-seed.mjs](../../../bootstrap-languages/local/generate-seed.mjs): writes `docker_seed.json` and the bundles.
- [docker/generate-seed.mjs](../../../docker/generate-seed.mjs), [docker-entrypoint.sh](../../../docker-entrypoint.sh): Docker use.
- [tests/js/utils/sharedStores.ts](../../../tests/js/utils/sharedStores.ts), [tests/js/tests/integration-local.test.ts](../../../tests/js/tests/integration-local.test.ts): tests on the local languages.

</details>