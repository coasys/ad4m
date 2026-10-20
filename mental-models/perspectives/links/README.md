# Links and store

A perspective's data is a set of **link expressions**: signed `(source, predicate, target)` triples with an author, a timestamp, a **link verdict** and a **link status** (`Local` or `Shared`). Each perspective keeps its links in its own Oxigraph store on disk. Every write follows one sequence: store, notify, then commit shared links to the neighbourhood.

**Why:** A signature on every link lets any reader check who wrote it, also after the link has synced through a third party. An RDF store gives standard SPARQL over the links that apps write. Links used to live in SQLite rows ([Oxigraph store](oxigraph-store.md) covers the migration).

**Parent:** [Perspectives](../README.md)

## Parts
- [Links](link-expressions.md): the `Link` and `LinkExpression` types, how a link is signed and verified, and literal targets.
- [Link verdicts](verdicts.md): the four verdicts, how the executor resolves a key to its person, and which reads see which links.
- [Local and shared links](local-and-shared.md): what link status decides, and which paths set it.
- [Oxigraph store](oxigraph-store.md): how a link becomes RDF triples, where the store lives, migration and backup.
- [Link write path](write-path.md): the steps of every write, batches, and the events a write emits.

<details><summary>How it works</summary>

| Data | Where | Code |
|---|---|---|
| Links (local and shared) | Oxigraph store `{data}/perspectives/{uuid}/sparql_store` | `SparqlStore` |
| Membership links of a neighbourhood | named graph `coasys:membership` in the same store | `Membership` |
| Perspective handle (name, owners, neighbourhood, state) | SQLite table `perspective_handle` | `Ad4mDb` |
| Shared diffs the link language has not accepted yet | SQLite table `perspective_diff` | `Ad4mDb::get_pending_diffs` |
| Open batches | memory only (`batch_store`) | `PerspectiveInstance::create_batch` |
| Parsed subject-class shapes | memory only (`shape_cache`) | `PerspectiveInstance::get_shape` |

</details>

<details><summary>Where in the code</summary>

- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): `SparqlStore`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `add_link`, `add_links`, `link_mutations`, `update_link`, `remove_links`, `get_links`, `create_batch` / `commit_batch`.
- [types/core.rs](../../../rust-executor/src/types/core.rs): domain types `Link`, `LinkExpression`, `DecoratedLinkExpression`, `PerspectiveDiff`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): wire types `LinkStatus`, `PerspectiveHandle`, `DecoratedPerspectiveDiff`.
- [db.rs](../../../rust-executor/src/db.rs): `perspective_handle` and `perspective_diff` tables.

</details>
