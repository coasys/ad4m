# SPARQL queries

A **SPARQL query** (`perspective.querySparql`) runs a caller's read-only SPARQL 1.2 query on the perspective's Oxigraph store and returns the result as JSON. The query sees the raw data: the direct link triples and the reifier nodes with author (a device key), timestamp, proof, link verdict and status ([Oxigraph store](../links/oxigraph-store.md)). In a neighbourhood, the membership links sit in the named graph `coasys:membership`; a query reads them with `GRAPH <coasys:membership> { … }`.

**Why:** SPARQL is the standard query language for RDF, the data model the store already uses. It gives apps every graph pattern that model queries do not cover. Writes stay on the signed link API, so a query can never create an unsigned link.

**Parent:** [Querying](README.md)

<details><summary>How it works</summary>

1. The handler checks the capability and owner access, then reads `engine` (default `"sparql"`; any other value is refused).
2. `validate_readonly_query` parses the text as a SPARQL query. `INSERT`, `DELETE` and other updates fail to parse and are refused. `SELECT`, `ASK`, `CONSTRUCT` and `DESCRIBE` pass.
3. The query runs on a blocking thread. The handler stops waiting after `SPARQL_QUERY_TIMEOUT_SECS` (30 s, answer 408). The client can cancel with `request.cancel` (answer 499).
4. `query_arbitrary` returns the executor's own JSON shape, not the W3C SPARQL JSON results format: `SELECT` rows as an array of JSON objects keyed by variable name, `ASK` as a boolean, and graph results as subject/predicate/object rows. Unlike the internal `query`, it does not re-encode `?target` / `?t` bindings as `literal:` strings.
5. Two custom functions are registered: `<ad4m://fn/parse_literal>(?x)` decodes a `literal:` value, `<ad4m://fn/strip_html>(?x)` removes HTML tags.

SPARQL returns links of every verdict, and links by authors who are not members. To read only `Valid` links, a query matches the reifier's verdict annotation itself. The member filter does not apply to SPARQL: membership comes from the replay, not from a stored triple. `?author` binds a device key, and a query cannot group by person, because the key-to-person map lives in the resolver, not in the store ([Link verdicts](../links/verdicts.md)).

Read a link with its metadata:

```sparql
SELECT ?s ?p ?o ?author WHERE {
  ?s ?p ?o .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?s ?p ?o )>> ;
     <ad4m://ontology/author> ?author .
}
```

</details>

<details><summary>Where in the code</summary>

- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): `query_sparql`, `SPARQL_QUERY_TIMEOUT_SECS`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `sparql_query`, `sparql_query_cancellable`.
- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): `validate_readonly_query`, `query`, `query_arbitrary`, `query_cancellable`, custom functions `parse_literal` and `strip_html`.
- SDK: `PerspectiveProxy.querySparql` in [core/src/perspectives/PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts); result typing in [core/src/perspectives/SparqlBindings.ts](../../../core/src/perspectives/SparqlBindings.ts).
- Guide: [docs-src/pages/developer-guides/sparql-queries.mdx](../../../docs-src/pages/developer-guides/sparql-queries.mdx).

</details>
