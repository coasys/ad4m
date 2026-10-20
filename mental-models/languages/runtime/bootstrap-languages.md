# Bootstrap languages

The **bootstrap seed** (`BootstrapSeed`) is a JSON file that names the languages an executor starts with: the language language bundle (inline), the addresses of the agent, neighbourhood and perspective languages, the **known link languages**, and the trusted agents. **Bootstrap languages** are the packages in `bootstrap-languages/` that the seed points at.

**Why:** A new executor must reach the network before it can install anything. The seed gives it the one bundle it cannot fetch (the language language) and the addresses of the rest. A different seed gives a different network: the local seed runs ADAM with no Holochain and no central services.

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

When the seed languages load (the executor's whole boot order is on [Executor startup](../../agents/executor/startup.md)):

1. `init` writes the seed to `<data>/mainnet_seed.seed` (the built-in `mainnet_seed.json`, or `--network-bootstrap-seed <file>`). `RuntimeService::init_global_instance` reads it.
2. On agent generate or unlock (`agent_ws.rs`), `load_core_system_languages` loads the language language from the inline bundle, then the agent, neighbourhood and perspective languages by address, in parallel, and registers the aliases.
3. In the background, `conductor_startup.rs` starts the Holochain conductor (unless `run_holochain` is false) and then runs `load_link_and_installed_languages`: the known link languages, then every other `languages/<addr>/bundle.js`.

The default seed, built by `ad4m dev generate-bootstrap` from `cli/seed_proto.json`:

| Role | Package |
|---|---|
| language language | `language-language` (Cloudflare KV) |
| agent language | `centralized-agent-language` (HTTP) |
| neighbourhood language | `neighbourhood-language` (Cloudflare KV) |
| perspective language | `perspective-language` |
| known link languages | `p-diff-sync`, `server-link-language` |
| also published | `file-storage`, `centralized-file-storage`, `eas`, `embedding-vector-language` |

On the default seed, the language language and the neighbourhood language use one Cloudflare Worker, and the agent language posts to an HTTP endpoint. The link layer offers both a peer-to-peer option (p-diff-sync) and a server option (the server link language).

`bootstrap-languages/local/generate-seed.mjs` builds the local seed from `local/*.js`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/mainnet_seed.json](../../../rust-executor/src/mainnet_seed.json), [rust-executor/src/globals.rs](../../../rust-executor/src/globals.rs) (`MAINNET_JSON`), [rust-executor/src/init.rs](../../../rust-executor/src/init.rs) (`write_seed_config`).
- [rust-executor/src/runtime_service/mod.rs](../../../rust-executor/src/runtime_service/mod.rs): `BootstrapSeed`, `RuntimeService`, known link languages (seed + DB table `known_link_languages`).
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `load_core_system_languages`; [conductor_languages.rs](../../../rust-executor/src/languages/conductor_languages.rs).
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../rust-executor/src/holochain_service/conductor_startup.rs), [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): when the loads run.
- [cli/seed_proto.json](../../../cli/seed_proto.json), [cli/src/dev.rs](../../../cli/src/dev.rs) (`GenerateBootstrap`), [cli/src/bootstrap_publish.rs](../../../cli/src/bootstrap_publish.rs): build the default seed.
- [bootstrap-languages/](../../../bootstrap-languages), [bootstrap-languages/local/](../../../bootstrap-languages/local): packages.

</details>