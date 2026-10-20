# Querying

The executor answers four kinds of read over one perspective's Oxigraph store. **Link queries** match source, predicate, target and a time range. **SPARQL queries** are read-only and take any graph pattern. **Model queries** return instances of subject classes, built from their SHACL shapes. **Live queries** re-run a SPARQL or model query and push the changes to the caller. Two rules decide which links a read sees: a **logic read** uses only `Valid` links, a **display read** hides only `Invalid` ones ([Link verdicts](../links/verdicts.md)); and in a neighbourhood, a read skips links whose author maps to no member or admitted guest at the link's time ([Membership](../neighbourhoods/membership/README.md)).

**Why:** Because queries run in the executor, one implementation serves the SDK, MCP tools, the CLI and languages, and only results cross the socket. Model queries moved from the TypeScript SDK into Rust, so filtering, relations and paging run next to the data. The verdict and membership rules sit in the executor for the same reason: a template that counts votes cannot forget them.

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
| `neighbourhood.countByPerson` | people, not keys | [Counts by person](../neighbourhoods/membership/counts-by-person.md) |

Every method runs behind the guard `Owns { Perspective }` with the perspective query capability (see [Capabilities](../../agents/access/grants/capabilities.md) and [Guards](../../agents/access/guards.md)). A session that acts as a bot agent also needs `read` for this neighbourhood in its [bot grant](../../agents/access/grants/bot-grants.md).

The reads are layered by link verdict. A model query is a logic read: it reads only `Valid` links (`proof_valid_filter` in [sparql_builder.rs](../../../rust-executor/src/perspectives/model_query/sparql_builder.rs)), unless the query sets `includeUnverified`. Flow folds and person counts are logic reads too. `perspective.queryLinks` and SPARQL are the raw reads: each link they return carries its verdict. A SPARQL query reads the store directly, so it filters by verdict only when it matches the reifier's verdict annotation itself, and the member filter does not reach it ([SPARQL queries](sparql.md)).

The executor contains a disabled Prolog engine; it is not supported. Queries use SPARQL and model queries.

</details>

<details><summary>Where in the code</summary>

- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): the handlers listed above.
- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): store-level queries.
- [perspectives/model_query/](../../../rust-executor/src/perspectives/model_query): the model query engine.
- [perspectives/subject_classes_of.rs](../../../rust-executor/src/perspectives/subject_classes_of.rs): which classes a URI conforms to.
- [identity/verdict.rs](../../../rust-executor/src/identity/verdict.rs) (new), [membership.rs](../../../rust-executor/src/membership.rs) (new): the verdict and member rules that reads apply.
- SDK: [core/src/perspectives/PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts), [core/src/model/](../../../core/src/model).
- Guide: [docs-src/pages/developer-guides/sparql-queries.mdx](../../../docs-src/pages/developer-guides/sparql-queries.mdx).

</details>
