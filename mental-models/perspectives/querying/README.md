# Querying

The executor answers four kinds of read over one perspective's Oxigraph store. **Link queries** match source, predicate, target and a time range. **SPARQL queries** are read-only and take any graph pattern. **Model queries** return instances of subject classes, built from their SHACL shapes. **Live queries** re-run a SPARQL or model query and push the changes to the caller.

**Why:** Because queries run in the executor, one implementation serves the SDK, MCP tools, the CLI and languages, and only results cross the socket. Model queries moved from the TypeScript SDK into Rust, so filtering, relations and paging run next to the data.

**Parent:** [Perspectives](../README.md)

## Parts
- [SPARQL queries](sparql.md): the read-only SPARQL method, its limits, and how to read link metadata.
- [Model queries](model-queries.md): the pipeline that turns a subject class's shape into SPARQL and JSON instances.
- [Live queries](live-queries.md): when a subscription re-runs, how deltas reach the caller, and notifications.

<details><summary>How it works</summary>

| RPC method | Engine | Code |
|---|---|---|
| `perspective.queryLinks` | link pattern on the store | `PerspectiveInstance::get_links` → `SparqlStore::query_links*` |
| `perspective.querySparql` | SPARQL (`engine: "sparql"`) | `sparql_query_cancellable` → `query_arbitrary` |
| `perspective.modelQuery` | model query | `model_query` → `execute_model_query` |
| `perspective.subjectClassesOf` | SPARQL conformance | `subject_classes_of` |
| `perspective.subscribeQuery`, `perspective.modelSubscribe` | live query | `subscribe_and_query`, `model_subscribe_and_query` |

Every method checks the perspective query capability and owner access first (see [Capabilities](../../agents/access/capabilities.md) and [Access checks](../../agents/access/checks.md)).

The reads are layered by signature verdict. A model query reads only links whose signature verifies (`proof_valid_filter` in [sparql_builder.rs](../../../rust-executor/src/perspectives/model_query/sparql_builder.rs)), unless the query sets `includeUnverified`. `perspective.queryLinks` and SPARQL are the raw reads: they also return unverified links, and each link carries its verdict (`proof.valid`, or `ad4m://ontology/proofValid` in SPARQL).

The executor contains a disabled Prolog engine; it is not supported. Queries use SPARQL and model queries.

</details>

<details><summary>Where in the code</summary>

- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): the handlers listed above.
- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): store-level queries.
- [perspectives/model_query/](../../../rust-executor/src/perspectives/model_query): the model query engine.
- [perspectives/subject_classes_of.rs](../../../rust-executor/src/perspectives/subject_classes_of.rs): which classes a URI conforms to.
- SDK: [core/src/perspectives/PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts), [core/src/model/](../../../core/src/model).
- Guide: [docs-src/pages/developer-guides/sparql-queries.mdx](../../../docs-src/pages/developer-guides/sparql-queries.mdx).

</details>
