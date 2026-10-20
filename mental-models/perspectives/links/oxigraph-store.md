# Oxigraph store

Each perspective has one **Oxigraph store** (`SparqlStore`): an RDF store on RocksDB at `{data}/perspectives/{uuid}/sparql_store`. The store keeps each link as one direct triple `source predicate target`, plus an RDF 1.2 **reifier** node that carries the link's author, timestamp, proof, link verdict and status. Data links sit in the default graph; a neighbourhood's membership links sit in the named graph `coasys:membership`. The store is plain on disk, behind file permissions.

**Why:** The direct triple lets plain SPARQL and model queries match data as ordinary RDF. The reifier keeps the metadata that makes a link a signed expression, without hiding the triple behind it. One store per perspective keeps perspectives independent: removing a perspective removes a directory. Each store is its own RocksDB instance, so the number of open files grows with the number of perspectives. The store stays plain behind file modes; why the option of encrypting it lost is on [Executor data](../../agents/executor/data/README.md).

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

```
<source> <predicate> <target> .                      # the data (target may be a typed literal)
<link:HASH> rdf:reifies <<( <source> <predicate> <target> )>> .
<link:HASH> ad4m://ontology/author         "did:key:…" .   # the signing device key
<link:HASH> ad4m://ontology/timestamp      "2026-…" .
<link:HASH> ad4m://ontology/proofKey       "…" .
<link:HASH> ad4m://ontology/proofSignature "…" .
<link:HASH> <verdict annotation>           "Valid" .        # replaces proofValid "true"
<link:HASH> ad4m://ontology/status         "Shared" .
<link:HASH> ad4m://ontology/wireTarget     "…" .   # only when the signed target differs from the stored term
```

1. `HASH` is SHA-256 over author, source, predicate, normalized target and timestamp (`make_reifier_iri`). The same link written twice lands on the same reifier, and the second write replaces the first one's annotations.
2. `literal:string|number|boolean|json:` targets become typed RDF literals (`target_to_storage_term`). `wireTarget` keeps the exact signed string when the stored form differs, so the signature still verifies.
3. A writer mutex serializes link transactions across clones of the store; reads use snapshots.
4. Startup (`perspectives::initialize_from_db`): the executor opens a store only after it is unlocked, with directory mode `0700` and file mode `0600`. It runs `migrate_links_from_rusqlite_to_sparql` (links from the earlier SQLite storage), then starts the instance.
5. Delete: when no owner is left, the executor closes the store and deletes its directory. The links are not crypto-shredded; freed disk blocks can keep them.
6. Export writes every link with its original signature, plus the key log, so another executor can check every author ([Export and import](../../agents/executor/data/export-and-import.md)). No executor backs up its stores. The platform snapshot (`perspective_snapshot.rs`, hosted mode only) seals each archive as a whole and takes RocksDB checkpoints.

</details>

<details><summary>Where in the code</summary>

- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): `SparqlStore::new`, `add_link`, `remove_link`, `insert_link_triples`, `query_links`, `query_links_top_n_by_timestamp`, `query_arbitrary`, `validate_readonly_query`, `decorated_links_query`, `backup`, `reload`.
- [perspectives/migration.rs](../../../rust-executor/src/perspectives/migration.rs): `migrate_links_from_rusqlite_to_sparql`.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `set_app_data_path`, `initialize_from_db`.
- [perspective_snapshot.rs](../../../rust-executor/src/perspective_snapshot.rs): `create_snapshot`, `restore_snapshot`, `backup_perspectives`, `spawn_periodic_backup`.
- [perspectives/memory_diagnostics.rs](../../../rust-executor/src/perspectives/memory_diagnostics.rs): periodic memory stats per store.

</details>
