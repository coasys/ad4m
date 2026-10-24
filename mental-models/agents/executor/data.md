# Executor data

The executor keeps all its state under one **app data path** (default `~/.ad4m`). It holds the agent file with the encrypted keystore, one SQLite database (`ad4m_db.sqlite`), one Oxigraph store per perspective, the installed languages, the Holochain conductor's data and the apps registry. In **hosted mode** the wallet and the user table come from a **hosting service** over HTTP (`SharedWallet`, `SharedDb`), and the executor backs up perspective stores as snapshots.

**Why:** All state sits under one directory, so moving or backing up an executor is a copy of one folder. Hosted mode lets several executors of one hosting service share keys and the user table.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

| Path (under app data path) | Content | Written by |
|---|---|---|
| `ad4m/agent.json` | `AgentStore`: main DID, DID document, signing key id, encrypted keystore (all agents' keys on a local wallet; an empty string in hosted mode, where `SharedWallet` keeps the default `export` and the keys stay with the hosting service, [wallet.rs](../../../rust-executor/src/wallet.rs)), main profile | `AgentService::try_save` |
| `ad4m/agent.json.legacy` | backup of a legacy-format keystore, until the next direct unlock | `AgentService::unlock` |
| `ad4m/agentProfile.json` | main agent profile (read first if present) | older executors |
| `ad4m/users/<email>/profile.json` | managed agent profile | `AgentService::store_managed_agent_profile` |
| `ad4m_db.sqlite` | `Ad4mDb`: perspective handles, diffs, users, settings, trusted agents, friends, entanglement proofs, notifications, AI models and tasks, billing, email codes | `Ad4mDb` |
| `perspectives/<uuid>/` | Oxigraph (RocksDB) store of one perspective | see [Oxigraph store](../../perspectives/links/oxigraph-store.md) |
| `ad4m/languages/` | installed language bundles and settings | see [Installing languages](../../languages/runtime/installing/README.md) |
| `ad4m/h/c`, `ad4m/h/d` | Holochain conductor config and data | Holochain service |
| `apps_data.json` | app grants: request id → app info, JWT, revoked flag; a JSON file, not a SQLite table | `apps_map` (path set by `run()`) |
| `mainnet_seed.seed` | bootstrap seed (system language addresses) | `ad4m-executor init` ([init.rs](../../../rust-executor/src/init.rs)) |

Some files sit under `<app_data>/ad4m/`, others directly under `<app_data>/`. Trusted agents, friends, known link languages, the outbox and entanglement proofs are executor-wide tables with no agent column ([db.rs](../../../rust-executor/src/db.rs)). They belong to the main agent; managed agents have none of these lists.

Hosted mode (`wallet_backend = "shared"`, `db_backend = "shared"`):

1. `SharedWallet` fetches key material from `wallet_backend_url` with `internal_api_token`.
2. `SharedDb` stores users under the fixed namespace `shared:platform`. A user created on one executor is imported on first login to another (`import_user_from_shared_db`).
3. `perspective_snapshot` restores perspective stores at start and backs them up every `snapshot_interval_secs`.
4. `run()` panics if `internal_api_token` equals `admin_credential`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): data directories, backend choice, `Ad4mDb::init_global_instance`.
- [rust-executor/src/db.rs](../../../rust-executor/src/db.rs): every SQLite table and query (`Ad4mDb`).
- [rust-executor/src/db_backend.rs](../../../rust-executor/src/db_backend.rs): `DbBackend`, `LocalDb`, `SharedDb`.
- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `LocalWallet`, `SharedWallet`.
- [rust-executor/src/perspective_snapshot.rs](../../../rust-executor/src/perspective_snapshot.rs): restore and periodic backup.
- [rust-executor/src/agent/capabilities/apps_map.rs](../../../rust-executor/src/agent/capabilities/apps_map.rs): the apps registry file.
- [rust-executor/src/utils.rs](../../../rust-executor/src/utils.rs): `ad4m_data_directory`, `languages_directory`.

</details>
