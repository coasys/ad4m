# Executor data

The executor keeps all its state under one **app data path** (default `~/.ad4m`). It holds the agent file with the encrypted keystore, one SQLite database (`ad4m_db.sqlite`), one Oxigraph store per perspective, the installed languages, and the Holochain conductor's data. In **hosted mode** the wallet, and the users and perspective metadata that executors share, come from a **hosting service** over HTTP (`SharedWallet`, `SharedDb`); every other table stays in the executor's SQLite database, and the executor backs up perspective stores as snapshots.

**Why:** All state sits under one directory, so moving or backing up an executor is a copy of one folder. Hosted mode lets several executors of one hosting service share keys, users and perspective metadata.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

| Path (under app data path) | Content | Written by |
|---|---|---|
| `ad4m/agent.json` | `AgentStore`: main DID, DID document, signing key id, encrypted keystore (all agents' keys on a local wallet; an empty string in hosted mode, where `SharedWallet` keeps the default `export` and the keys stay with the hosting service, [wallet.rs](../../../rust-executor/src/wallet.rs)), main profile | `AgentService::try_save` |
| `ad4m/agent.json.legacy` | backup of a legacy-format keystore, until the next direct unlock | `AgentService::unlock` |
| `ad4m/agentProfile.json` | main agent profile (read first if present) | older executors |
| `ad4m/users/<email>/profile.json` | managed agent profile | `AgentService::store_managed_agent_profile` |
| `ad4m_db.sqlite` | `Ad4mDb`: perspective handles, diffs, users, grants, settings, trusted agents, friends, entanglement proofs, notifications, AI models and tasks, billing, email codes | `Ad4mDb` |
| `perspectives/<uuid>/` | Oxigraph (RocksDB) store of one perspective | see [Oxigraph store](../../perspectives/links/oxigraph-store.md) |
| `ad4m/languages/` | installed language bundles and settings | see [Installing languages](../../languages/runtime/installing/README.md) |
| `ad4m/h/c` | Holochain conductor data (its `data_root_path`) | Holochain service |
| `mainnet_seed.seed` | bootstrap seed (system language addresses) | `ad4m-executor init` ([init.rs](../../../rust-executor/src/init.rs)) |

Some files sit under `<app_data>/ad4m/`, others directly under `<app_data>/`. Trusted agents, friends, known link languages, the outbox and entanglement proofs are executor-wide tables with no agent column ([db.rs](../../../rust-executor/src/db.rs)). They belong to the main agent; managed agents have none of these lists.

Hosted mode (`wallet_backend = "shared"`, `db_backend = "shared"`):

1. `SharedWallet` fetches key material from `wallet_backend_url` with `internal_api_token`.
2. `SharedDb` sends only the key-value methods (`get`, `list`, `upsert`, `delete`) to the hosting service: users and perspective metadata under the fixed namespace `shared:platform`. It runs every other method on the executor's SQLite database, as `LocalDb` does. A user created on one executor is imported on first login to another (`import_user_from_shared_db`). `perspective.all` loads the perspectives that other executors created (`rehydrate_perspective_from_backend`).
3. `perspective_snapshot` restores perspective stores at start and backs them up every `snapshot_interval_secs`.
4. `run()` panics if `internal_api_token` equals `admin_credential`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): data directories, backend choice, `Ad4mDb::init_global_instance`.
- [rust-executor/src/db.rs](../../../rust-executor/src/db.rs) and [db/](../../../rust-executor/src/db): every SQLite table and query (`Ad4mDb`); [db/tasks.rs](../../../rust-executor/src/db/tasks.rs): the owner of each AI task.
- [rust-executor/src/db_backend.rs](../../../rust-executor/src/db_backend.rs): `db_backend()`, the executor's database: executor code calls it, never `Ad4mDb` directly. `DbBackend` has one method per `Ad4mDb` method that executor code calls (`ad4m_db_methods!`) and the key-value methods; `LocalDb`, `SharedDb`. A database that is not open is an error; a lock poisoned by a panic still serves the database (`on_ad4m_db`).
- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `LocalWallet`, `SharedWallet`.
- [rust-executor/src/perspective_snapshot.rs](../../../rust-executor/src/perspective_snapshot.rs): restore and periodic backup.
- [rust-executor/src/db/grants.rs](../../../rust-executor/src/db/grants.rs): the `grants` table ([App grants](../access/app-grants.md)).
- [rust-executor/src/utils.rs](../../../rust-executor/src/utils.rs): `ad4m_data_directory`, `languages_directory`.

</details>
