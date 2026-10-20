# Export and import

A person's **export** holds every perspective that the session's agent owns on one executor, each link exactly as it was signed (author, timestamp, target, proof), plus the person's key log as the executor's cache holds it. Another executor imports it and checks every author itself. Any agent session, the operator's or a tenant's, exports its own perspectives in chunks; the file the person saves is plain gzip JSON. Import writes private perspectives as new perspectives with their links unchanged, and returns the neighbourhood URLs for the app to join again. Leaving a host starts with an export.

**Why:** The executor never re-signs during an export or an import, so the signatures still prove who wrote each link, and an import cannot forge one. Neighbourhoods are joined again rather than written from the file, so an old copy cannot push past links into a live neighbourhood. A plaintext dump of the whole database lost: it would undo encryption at rest, so the operator's `db` export is refused. The file is plain, and the save screen warns the person to keep it safe.

**Parent:** [Executor data](README.md)

<details><summary>How it works</summary>

| Method | Guard | Does |
|---|---|---|
| `data.exportPrepare { perspectives? }` | an agent session, its own perspectives only | builds the file in `ad4m/tmp/exports/<id>`, sealed under the person's key; answers size, SHA-256 and chunk count; deleted after the last chunk or 15 minutes |
| `data.exportChunk { exportId, index }` | the session that prepared it | one base64 chunk |
| `data.importBegin`, `data.importChunk`, `data.importCommit` | an agent session | checks format and size (`max_import_bytes`), writes private perspectives, returns `ImportResult` with the neighbourhoods to join |
| `runtime.exportData`, `runtime.importData` | operator, local only | type `perspective` reads and writes the `ExportFile` format; type `db` answers `db-export-refused` |

`ExportFile`: `format: "coasys-export"`, `version: 1`, `executor`, `person { didScid, deviceDid }`, `keyLog`, and `perspectives[]` with `kind` (`private` or `space`), `neighbourhoodUrl`, `linkLanguage` and `links[]`. The executor builds each `SignedLink` from the Oxigraph store's reifier annotations (`ONT_AUTHOR`, `ONT_TIMESTAMP`, `ONT_PROOF_KEY`, `ONT_PROOF_SIG`, `ONT_WIRE_TARGET`).

On import, each link's signature is checked as for a link that arrives by sync, and the result is kept as `proofValid`; a link that fails is kept and counted, never dropped and never re-signed. Authors resolve through `author_of`. The key log in the file is not written to the shared store.

The platform snapshot (hosted mode only) archives RocksDB checkpoints as tar.gz and seals the whole archive with a stream AEAD under the executor's `Archive` subkey (header `AD4MARCH`). Restoring it needs the same executor's keyring.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/at_rest/export.rs](../../../../rust-executor/src/at_rest/export.rs) (new): `ExportFile`, `ExportedPerspective`, `SignedLink`, prepare, chunks, import.
- [rust-executor/src/api/data_ws.rs](../../../../rust-executor/src/api/data_ws.rs) (new): the `data.*` methods.
- [rust-executor/src/api/runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs): `runtime.exportData`, `runtime.importData`.
- [rust-executor/src/perspectives/sparql_store.rs](../../../../rust-executor/src/perspectives/sparql_store.rs): the reifier annotations an export reads.
- [rust-executor/src/at_rest/bundle.rs](../../../../rust-executor/src/at_rest/bundle.rs) (new) and [rust-executor/src/perspective_snapshot.rs](../../../../rust-executor/src/perspective_snapshot.rs): the sealed platform snapshot.
- `DataClient` in the TypeScript SDK (new, in [core/](../../../../core)).

</details>
