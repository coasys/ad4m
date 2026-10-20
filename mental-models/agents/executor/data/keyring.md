# Keyring and storage secret

Each executor has one **storage secret**: 32 random bytes, kept off the data disk. From it the executor derives, in memory, the **root key** and the wallet's **agent passphrase**. The root key wraps the **executor store key** and one **person data key** per person (the main agent, each tenant, each password user and guest); each data key wraps that person's **store keys**, and each perspective's store key is wrapped once per owner. The wrapped keys live in `keyring.json`. The `DataKeys` service unwraps them at `storage.unlock` and holds every key in locked, zeroed memory for the life of the process.

**Why:** A store key under each owner's data key lets several owners open one perspective, makes a data-key rotation re-wrap a few keys instead of re-encrypting data, and makes destroying one data key remove every wrap under it. Data at rest uses a symmetric wrap under the root key, not a seal to the person's public key: the executor must open a tenant's data while the tenant is away (sync, flows, AI tasks). One secret per executor, with no recovery code: a second secret or a recovery code would multiply what an operator must keep, and a person's identity and neighbourhoods do not depend on any one executor's data.

**Parent:** [Executor data](README.md)

<details><summary>How it works</summary>

```
storage secret ─ HKDF "ad4m:agent-passphrase:v1" ─► agent passphrase ─► agent.json, Holochain's databases and lair
       └─ HKDF(salt keyring_id, "ad4m:at-rest:root:v1") ─► root key (memory only)
             ├─ wraps ─► executor store key ─► SQLCipher key, system-language storage, snapshot archives, executor secret files
             └─ wraps ─► person data key (Main │ User(username)), versioned
                           ├─ wraps ─► person store key ─► that person's sealed rows
                           └─ wraps ─► perspective store key (one wrap per owner) ─► its link language's files, its sealed rows
perspectives/<uuid>/sparql_store ─ no key
```

| Where | Secret source |
|---|---|
| Desktop app | the OS keystore item `org.coasys.app` / `adam-storage-<hash of the data path>`; the shell passes it with `storage.unlock` over loopback with the admin credential, never on the command line or in the environment. Linux without a Secret Service: a typed passphrase, Argon2id with `keyring_id` as salt |
| Server | `AD4M_STORAGE_SECRET_FILE`, refused on the data directory's file system; a systemd credential, TPM-sealed where present |
| Coasys cloud, or any executor by hand | `ad4m-executor storage unlock` reads it from the terminal and calls `storage.unlock` on loopback |
| The launcher (embedded) | `Ad4mConfig.storage_secret`, in memory |

States: `sealed` → `unlocking` → (`migrating`) → `open`; a wrong secret or a corrupt keyring returns to `sealed` with `lastError`. On the first start the executor makes `keyring_id`, the root key, the executor store key and the person `main`.

| Method | Guard | Does |
|---|---|---|
| `runtime.storageStatus` | open, works sealed | `state`, `stores`, `encryptedAtRest`; `keyringId` while sealed; operator fields (`secretSource`, `rootGeneration`, `migration`, `lastError`) |
| `storage.unlock { secret }` | operator, local only | the start-up unlock |
| `storage.rotateSecret`, `storage.reencrypt` | operator, local only | a new secret (re-wraps under a new root; moves the wallet and Holochain to the new passphrase); a store's re-encryption |
| `storage.rotatePersonKey { owner? }` | operator, or an agent for its own | a new version of a data key |
| `storage.keys` | operator | `KeyringSummary`, no key material |

A lost secret loses that executor's data and its wallet, not the identity: the key log, neighbourhoods on the link server and every export survive. `ad4m-executor storage reset` renames the old files `.lost-<time>`, makes a new keyring and a new wallet, so a new device key; the person links the executor again.

The storage secret is not tied to the operator record's identity: whoever holds the secret opens the data, and the recorded operator manages the executor.

</details>

<details><summary>Where in the code</summary>

- [keyring/](../../../../keyring) (new): crate `ad4m-keyring` beside the identity core: `SecretKey`, `KeyVersion`, `VersionedKey`, `wrap`/`unwrap` with `WrapContext`, `hkdf_subkey`, the X25519 room-key seal; native and WASM.
- [rust-executor/src/at_rest/data_keys.rs](../../../../rust-executor/src/at_rest/data_keys.rs) (new): `DataKeysApi` (`unlock`, `create_person`, `destroy_person`, `create_store`, `add_owner`, `remove_owner`, `destroy_store`, `subkey`, rotation), `seal_executor_secret`, `open_executor_secret`.
- [rust-executor/src/at_rest/keyring_file.rs](../../../../rust-executor/src/at_rest/keyring_file.rs) (new): `keyring.json` and its `fsync`ed writes.
- [rust-executor/src/at_rest/secret_source.rs](../../../../rust-executor/src/at_rest/secret_source.rs) (new): `SecretSourceKind { InProcess, File, UnlockCall }`.
- [rust-executor/src/at_rest/agent_passphrase.rs](../../../../rust-executor/src/at_rest/agent_passphrase.rs) (new): `derive_agent_passphrase`.
- [rust-executor/src/api/storage_ws.rs](../../../../rust-executor/src/api/storage_ws.rs) (new): the methods above; events `storage-state-changed`, `storage-migration-progress`.

</details>
