# Executor data

The executor keeps all its state under one **app data path** (default `~/.ad4m`): the wallet file, the **keyring**, one SQLite database (`ad4m_db.sqlite`), one Oxigraph store per perspective, the installed languages and their storage, and the Holochain conductor's data. The database and language storage are **encrypted at rest**: the whole SQLite file under the executor's key, plus each person's rows and each perspective's language files under keys that only that person (or the perspective's owners) can open. The Oxigraph stores, which hold the links, stay **plain** behind file permissions. In **hosted mode** the wallet and the user table come from a **hosting service** over HTTP (`SharedWallet`, `SharedDb`), and the executor backs up perspective stores as sealed snapshots.

**Why:** A key on the same disk in the clear would protect nothing, so every key sits in memory and the one secret that opens them sits off the data disk. Keys per person give per-person **crypto-shredding**: destroying a tenant's data key makes their encrypted rows and language storage unreadable. SPARQL needs plaintext terms, and Oxigraph exposes no encrypted environment, so encrypting the links would need a fork of Oxigraph and its RocksDB binding; that fork lost, because executors are trusted. A file-system layer under everything (`fscrypt`, FUSE) lost too: neither gives the same result on every kind of executor. All state still sits under one directory, so moving an executor is a copy of one folder and its secret.

**Parent:** [Executor](../README.md)

## Parts
- [Keyring and storage secret](keyring.md): the one secret per executor, the key hierarchy, the `DataKeys` service, the sealed state, rotation, a lost secret.
- [Encrypted stores](encrypted-stores.md): SQLCipher and sealed columns, sealed language files, the plain Oxigraph stores, the storage status, crypto-shredding, the migration of plaintext executors.
- [Export and import](export-and-import.md): the person's export file with every link and its original signature, import, the operator's tools, the sealed platform snapshot.

<details><summary>How it works</summary>

| Path (under app data path) | Content | At rest |
|---|---|---|
| `ad4m/agent.json` | `AgentStore`: main DID, DID document, signing key id, encrypted keystore (every agent's key on a local wallet), main profile | keystore under the derived agent passphrase |
| `ad4m/keyring.json` | `keyring_id`, the root check, every wrapped key with its versions | wrapped keys only |
| `ad4m_db.sqlite` | `Ad4mDb`: perspective handles, diffs, users, tenants, invites, the operator record, grants, settings, key-log cache, trusted agents, notifications, AI models and tasks, billing, bot activity | SQLCipher; personal columns sealed |
| `perspectives/<uuid>/sparql_store/` | Oxigraph (RocksDB) store of one perspective: every link with its proof ([Oxigraph store](../../../perspectives/links/oxigraph-store.md)) | plain; directory `0700`, files `0600` |
| `ad4m/languages/<address>/` | bundles (plain) and the language's storage files ([Installing languages](../../../languages/runtime/installing/README.md)) | storage sealed per file |
| `ad4m/lan/cert.der`, `ad4m/lan/key.sealed` | the desktop's home-network listener certificate and its private key | key sealed under the executor key |
| `ad4m/tmp/exports/<id>` | an export while it is read out, at most 15 minutes | sealed under the person's key |
| `ad4m/h/c`, `ad4m/h/d` | Holochain conductor config and data | Holochain's encryption, under the derived agent passphrase |
| `ad4m/users/<username>/profile.json` | managed agent profile | plain |
| `mainnet_seed.seed` | bootstrap seed | plain |

Some files sit under `<app_data>/ad4m/`, others directly under `<app_data>/`. Trusted agents, friends, known link languages and entanglement proofs are executor-wide tables with no agent column ([db.rs](../../../../rust-executor/src/db.rs)). They belong to the main agent; managed agents have none of these lists. Tasks, the outbox and the expression cache have an owner column. The server link language keeps a sealed copy of a neighbourhood's links in its storage, while the Oxigraph store holds the same links plain.

Hosted mode (`wallet_backend = "shared"`, `db_backend = "shared"`): `SharedWallet` fetches key material from `wallet_backend_url` with `internal_api_token`; `SharedDb` stores users under the namespace `shared:platform`; `perspective_snapshot` restores perspective stores at start and backs them up every `snapshot_interval_secs`, sealed ([Export and import](export-and-import.md)). A host such as Coasys cloud runs local backends; hosted mode is the platform's shared backend for executors that it provisions.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../../rust-executor/src/lib.rs): data directories, backend choice, `Ad4mDb::init_global_instance`.
- [rust-executor/src/db.rs](../../../../rust-executor/src/db.rs): every SQLite table and query (`Ad4mDb`); [rust-executor/src/db/grants.rs](../../../../rust-executor/src/db/grants.rs) (new): the grants table.
- [rust-executor/src/at_rest/](../../../../rust-executor/src/at_rest) (new): the keyring, the stores' keys, export and import, the migration.
- [rust-executor/src/db_backend.rs](../../../../rust-executor/src/db_backend.rs): `DbBackend`, `LocalDb`, `SharedDb`.
- [rust-executor/src/wallet.rs](../../../../rust-executor/src/wallet.rs): `LocalWallet`, `SharedWallet`.
- [rust-executor/src/perspective_snapshot.rs](../../../../rust-executor/src/perspective_snapshot.rs): restore and periodic backup.
- [rust-executor/src/utils.rs](../../../../rust-executor/src/utils.rs): `ad4m_data_directory`, `languages_directory`.

</details>
