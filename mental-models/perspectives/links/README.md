# Links and store

A perspective's data is a set of **link expressions**: signed `(source, predicate, target)` triples with an author (the `did:key` of the signing device key), a timestamp, an **anchor** when that key sits in a key log, a **link verdict** and a **link status** (`Local` or `Shared`). Each perspective keeps its links in its own Oxigraph store on disk; a neighbourhood keeps its membership links in a named graph of the same store. Every write follows one sequence: sign, store with a verdict, notify, then commit shared links to the neighbourhood.

**Why:** A signature on every link lets any reader check who wrote it, also after the link has synced through a third party. The anchor and the verdict let a reader check the key against the person's key log, and tell a forged link from one it cannot check yet. An RDF store gives standard SPARQL over the links that apps write. Links used to live in SQLite rows ([Oxigraph store](oxigraph-store.md) covers the migration).

**Parent:** [Perspectives](../README.md)

## Parts
- [Links](link-expressions.md): the `Link` and `LinkExpression` types, how a link is signed and anchored, and literal targets.
- [Link verdicts](verdicts.md): the four verdicts, how the executor resolves a key to its person, and which reads see which links.
- [Local and shared links](local-and-shared.md): what link status decides, and which paths set it.
- [Oxigraph store](oxigraph-store.md): how a link becomes RDF triples, where the store lives, the membership graph, migration and export.
- [Link write path](write-path.md): the steps of every write, batches, and the events a write emits.

<details><summary>How it works</summary>

| Data | Where | At rest | Code |
|---|---|---|---|
| Links (local and shared), with verdicts | Oxigraph store `{data}/perspectives/{uuid}/sparql_store` | plain, file modes `0700` / `0600` | `SparqlStore` |
| Membership links of a neighbourhood | named graph `coasys:membership` in the same store | plain | `Membership` |
| Perspective handle (name, owners, neighbourhood, state) | SQLite table `perspective_handle` | encrypted file; name, neighbourhood and URL sealed under the perspective's store key | `Ad4mDb` |
| Shared diffs the link language has not accepted yet | SQLite table `perspective_diff` | additions and removals sealed under the perspective's store key | `Ad4mDb::get_pending_diffs` |
| Verified key logs of link authors | SQLite, through the hash-chain module's tables | encrypted file | `KelCache` |
| Open batches | memory only (`batch_store`) | — | `PerspectiveInstance::create_batch` |
| Parsed subject-class shapes | memory only (`shape_cache`) | — | `PerspectiveInstance::get_shape` |

The keys behind "sealed" and "encrypted" are on [Keyring and storage secret](../../agents/executor/data/keyring.md).

</details>

<details><summary>Where in the code</summary>

- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): `SparqlStore`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `add_link`, `add_links`, `link_mutations`, `update_link`, `remove_links`, `get_links`, `create_batch` / `commit_batch`.
- [types/core.rs](../../../rust-executor/src/types/core.rs): domain types `Link`, `LinkExpression`, `ExpressionProof` with `Anchor`, `DecoratedLinkExpression`, `PerspectiveDiff`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): wire types `LinkStatus`, `PerspectiveHandle`, `DecoratedPerspectiveDiff`.
- [identity/verdict.rs](../../../rust-executor/src/identity/verdict.rs) (new): `LinkVerdict`.
- [db.rs](../../../rust-executor/src/db.rs): `perspective_handle` and `perspective_diff` tables.

</details>
