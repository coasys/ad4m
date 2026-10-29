# Oxigraph store

Each perspective has one **Oxigraph store** (`SparqlStore`): an RDF store on RocksDB at `{data}/perspectives/{uuid}/sparql_store`. The store keeps each link as one direct triple `source predicate target`, plus an RDF 1.2 **reifier** node that carries the link's author, timestamp, proof, signature verdict and status.

**Why:** The direct triple lets plain SPARQL and model queries match data as ordinary RDF. The reifier keeps the metadata that makes a link a signed expression, without hiding the triple behind it. One store per perspective keeps perspectives independent: removing a perspective removes a directory. Each store is its own RocksDB instance, so the number of open files grows with the number of perspectives. Each store opens with `max_open_files` 266 (`SPARQL_STORE_MAX_OPEN_FILES`): at most about 274 descriptors, about 70 for a typical store.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

```
<source> <predicate> <target> .                      # the data (target may be a typed literal)
<link:HASH> rdf:reifies <<( <source> <predicate> <target> )>> .
<link:HASH> ad4m://ontology/author         "did:key:…" .
<link:HASH> ad4m://ontology/timestamp      "2026-…" .
<link:HASH> ad4m://ontology/proofKey       "…" .
<link:HASH> ad4m://ontology/proofSignature "…" .
<link:HASH> ad4m://ontology/proofValid     "true" .
<link:HASH> ad4m://ontology/status         "Shared" .
<link:HASH> ad4m://ontology/wireTarget     "…" .   # only when the signed target differs from the stored term
```

1. `HASH` is SHA-256 over author, source, predicate, normalized target and timestamp (`make_reifier_iri`). The same link written twice lands on the same reifier. The second write replaces the status and other annotations, but writes the proof (key, signature, verdict, `wireTarget`) only when none is stored or when the stored one did not verify and the new one does (`proof_write`). A verified proof is never replaced.
2. `literal:string|number|boolean|json:` targets become typed RDF literals (`target_to_storage_term`). `wireTarget` keeps the exact signed string when the stored form differs, so the signature still verifies.
3. The store refuses a link whose predicate is `rdf:reifies` or starts with `ad4m://ontology/` or `ad4m://system/` (`is_link_metadata`): such a link would add or delete a fact on another link's reifier. Reads skip such triples as metadata.
4. A writer mutex serializes link transactions across clones of the store; reads use snapshots.
5. Startup (`perspectives::initialize_from_db`): open the store, run `migrate_links_from_rusqlite_to_sparql` (links from the earlier SQLite storage), remove links in the metadata vocabulary an older store holds (`purge_link_metadata_links`; a perspective whose sweep fails does not open), then start the instance.
6. Hosted executors with `wallet_backend = "shared"` restore `{data}/perspectives/` from a tar.gz snapshot at startup and upload new snapshots periodically (`perspective_snapshot.rs`). Other executors have no automatic backup of their stores; `export_perspective` exports one perspective.

</details>

<details><summary>Where in the code</summary>

- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): `SparqlStore::new`, `add_link`, `remove_link`, `add_remote_link`, `remove_remote_link`, `proof_write`, `is_link_metadata`, `purge_link_metadata_links`, `insert_link_triples`, `query_links`, `query_links_top_n_by_timestamp`, `query_arbitrary`, `validate_readonly_query`, `decorated_links_query`, `backup`, `reload`.
- [perspectives/migration.rs](../../../rust-executor/src/perspectives/migration.rs): `migrate_links_from_rusqlite_to_sparql`.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `set_app_data_path`, `initialize_from_db`, `sweep_link_metadata`.
- [perspective_snapshot.rs](../../../rust-executor/src/perspective_snapshot.rs): `create_snapshot`, `restore_snapshot`, `backup_perspectives`, `spawn_periodic_backup`.
- [perspectives/memory_diagnostics.rs](../../../rust-executor/src/perspectives/memory_diagnostics.rs): periodic memory stats per store.

</details>
