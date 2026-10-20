# Encrypted stores

Three kinds of store hold a person's data on an executor, and two of them are encrypted at rest. `ad4m_db.sqlite` is a **SQLCipher** file under a subkey of the executor store key, and the columns that hold a person's content are **sealed** under that person's or that perspective's store key. **Language storage** is sealed per file through the host's storage calls. Each perspective's **Oxigraph store** stays plain, behind file modes `0700`/`0600`, and opens only after unlock. `runtime.storageStatus.stores` reports each kind as `encrypted` or `plain`, and every screen picks its storage line from that report.

**Why:** SQLCipher is already linked into the executor through Holochain's encryption feature, so one `PRAGMA key` covers the whole file, its journal and its WAL. Sealed columns give per-person shredding without splitting a large database layer across one connection per person; one file per person lost for that reason. Language storage is sealed by the host, not by each language, so every language gets it, and a non-system language loses direct file access so that it cannot write around the seal. No screen claims more than the report says: the links are named as not encrypted. SPARQL needs plaintext terms, and Oxigraph exposes no encrypted environment, so encrypting the links would need a fork of Oxigraph and its RocksDB binding; that fork lost, because executors are trusted. A file-system layer under everything (`fscrypt`, FUSE) lost too: neither gives the same result on every kind of executor.

**Parent:** [Executor data](README.md)

<details><summary>How it works</summary>

| Store | Key | Notes |
|---|---|---|
| `ad4m_db.sqlite` | SQLCipher subkey of the executor store key | `cipher_memory_security`, `temp_store = MEMORY`; a wrong key fails at the first `SELECT` |
| sealed columns (`s1:` + version, nonce, AEAD) | `Rows` subkey of `Perspective(uuid)` or `Person(owner)` | `perspective_handle.name`, `neighbourhood`, `shared_url`; `perspective_diff`; `flow_replica_state`; `notifications`; `tasks`; `expression` (per owner); `outbox`. No index or `ORDER BY` uses a sealed column |
| language storage (`AD4MSEAL` files) | the perspective's store key for its link language; the executor store key for system languages | `op_language_storage_read` / `write` replace Deno's file calls in `language_bootstrap.js` |
| Oxigraph store | none | released `oxigraph` crate; plain RocksDB files |
| LAN certificate key | executor store key | `ad4m/lan/key.sealed` |

The server link language keeps a sealed copy of a neighbourhood's links in its language storage, while the Oxigraph store holds the same links plain.

Crypto-shredding: a tenant's delete (`host.leave { keepData: false }`, `host.removeTenant`, a pending record's expiry) calls `destroy_person`. The tenant's sealed rows and language storage become unreadable; each Oxigraph store with no owner left is deleted as files, so freed blocks can hold links until the disk reuses them. "Keep" changes nothing in the keyring: the executor still opens the data for the perspectives that other owners share. Executor-scope data is never shredded per person: the users and tenants tables, settings, metering, `models.api_key` (SQLCipher only) and the storage of system languages.

Migration of a plaintext executor, once, at the first start on the release: the executor makes the keyring with one person per `users` row, then encrypts the database (`sqlcipher_export`, then seal each column row by row), then each language directory. Each step compares counts and a hash before it renames over the old copy; a mismatch keeps the plaintext and returns to `sealed`. The Oxigraph stores stay in place and only get their file modes. An older executor cannot open the result.

| `stores` | `database` | `languageStorage` | `links` |
|---|---|---|---|
| secret off the data disk | `encrypted` | `encrypted` | `plain` (always) |
| secret on the data disk | `plain` | `plain` | `plain` |

`encryptedAtRest` is `true` only when every kind is `encrypted`; no executor reports it in this design.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/at_rest/sqlite.rs](../../../../rust-executor/src/at_rest/sqlite.rs) (new): SQLCipher keying and sealed columns; `Ad4mDb::new` in [db.rs](../../../../rust-executor/src/db.rs) runs `PRAGMA key` first.
- [rust-executor/src/at_rest/lang_storage.rs](../../../../rust-executor/src/at_rest/lang_storage.rs) (new) and [rust-executor/src/js_core/languages_extension.rs](../../../../rust-executor/src/js_core/languages_extension.rs): `op_language_storage_read`, `op_language_storage_write`; the scope by language address.
- [rust-executor/src/js_core/language_bootstrap.js](../../../../rust-executor/src/js_core/language_bootstrap.js): `readStorageFile`, `writeStorageFile` call the ops.
- [rust-executor/src/js_core/mod.rs](../../../../rust-executor/src/js_core/mod.rs): `JsCore::new_for_language` grants a non-system language no read or write on its directory.
- [rust-executor/src/perspectives/sparql_store.rs](../../../../rust-executor/src/perspectives/sparql_store.rs): `SparqlStore::new` opens after unlock, with the file modes.
- [rust-executor/src/at_rest/migrate.rs](../../../../rust-executor/src/at_rest/migrate.rs) (new): the migration orchestrator.
- [docs-src/host-contract.md](../../../../docs-src/host-contract.md): "Extension: Storage File I/O" says the host encrypts storage, and a language must not write with the runtime's own file API.

</details>
