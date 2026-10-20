# Bootstrap languages

The **bootstrap seed** (`BootstrapSeed`) is a JSON file that names what an executor starts with: the language language bundle (inline), the addresses of the agent, neighbourhood and perspective languages, the **known link languages**, the trusted agents, the shared store (`identity.storeUrl`, `identity.storeKeys`) and the default link servers (`linkServers`). **Bootstrap languages** are the packages in `bootstrap-languages/` that the seed points at.

**Why:** A new executor must reach the network before it can install anything. The seed gives it the one bundle it cannot fetch (the language language), the addresses of the rest, and the services that must be known before any call: where key logs live, which keys sign their heads, and where new neighbourhoods sync. Compiled in, these need no lookup at start and cannot fail there; a change needs a release, so the host names must stay stable. A different seed gives a different network: the local seed runs ADAM with no Holochain and no central services.

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

When the seed languages load (the executor's whole boot order is on [Executor startup](../../agents/executor/startup.md)):

1. `init` writes the seed to `<data>/mainnet_seed.seed` (the built-in `mainnet_seed.json`, or `--network-bootstrap-seed <file>`). `RuntimeService::init_global_instance` reads it. The seed and the executor's config both name `identity.storeUrl` and `identity.storeKeys`, and the [link-server directory](../link-languages/server-link-language/link-server-directory.md) takes `linkServers` as its last source.
2. On agent generate or unlock (`agent_ws.rs`), `load_core_system_languages` loads the language language from the inline bundle, then the agent, neighbourhood and perspective languages by address, in parallel, and registers the aliases. The agent language's settings get the store URL.
3. In the background, `conductor_startup.rs` starts the Holochain conductor (unless `run_holochain` is false) and then runs `load_link_and_installed_languages`: the known link languages, then every other `languages/<addr>/bundle.js`.

The default seed, built by `ad4m dev generate-bootstrap` from `cli/seed_proto.json`:

| Role | Package | Backend |
|---|---|---|
| language language | `language-language` | the language store Worker |
| agent language | `centralized-agent-language` | the shared store |
| neighbourhood language | `neighbourhood-language` | the language store Worker |
| perspective language | `perspective-language` | none |
| known link languages | `server-link-language`, `p-diff-sync` | a link server; Holochain |
| also published | `file-storage`, `centralized-file-storage`, `eas`, `embedding-vector-language` | — |
| `linkServers` | `https://link.coasys.org` (production), a separate staging name in staging builds | — |
| `identity` | `storeUrl` `https://store.coasys.org`, `storeKeys` (the store's head keys) | — |

On the default seed, the language language and the neighbourhood language use one language store Worker ([Language language](installing/language-language.md)); identities and profiles live in one shared store; new neighbourhoods sync through one link server. Only the link layer has a peer-to-peer option (p-diff-sync), and new neighbourhoods cannot pick it. The seed's list of known link languages marks no default: `neighbourhood.create` holds the choice.

`bootstrap-languages/local/generate-seed.mjs` builds the local seed from `local/*.js`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/mainnet_seed.json](../../../rust-executor/src/mainnet_seed.json), [rust-executor/src/globals.rs](../../../rust-executor/src/globals.rs) (`MAINNET_JSON`), [rust-executor/src/init.rs](../../../rust-executor/src/init.rs) (`write_seed_config`).
- [rust-executor/src/runtime_service/mod.rs](../../../rust-executor/src/runtime_service/mod.rs): `BootstrapSeed` (with `linkServers` and `identity`), `RuntimeService`, known link languages (seed + DB table `known_link_languages`).
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `load_core_system_languages`; [conductor_languages.rs](../../../rust-executor/src/languages/conductor_languages.rs).
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../rust-executor/src/holochain_service/conductor_startup.rs), [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): when the loads run.
- [cli/seed_proto.json](../../../cli/seed_proto.json), [cli/src/dev.rs](../../../cli/src/dev.rs) (`GenerateBootstrap`), [cli/src/bootstrap_publish.rs](../../../cli/src/bootstrap_publish.rs): build the default seed.
- [bootstrap-languages/](../../../bootstrap-languages), [bootstrap-languages/local/](../../../bootstrap-languages/local): packages.

</details>
