# Replacing RDF-star with Named Graphs for Link Metadata

## Problem

The SPARQL migration uses **RDF-star** (quoted triples) to annotate links with metadata — author, timestamp, proof signature, etc. RDF-star is an Oxigraph extension, not standard SPARQL 1.1. This creates a hard coupling to Oxigraph and prevents interoperability with other triple stores, federated query endpoints, or standard RDF tooling.

### Current approach (RDF-star)

Each AD4M link is stored as a direct triple with RDF-star annotations:

```turtle
# The link
<source> <predicate> <target> .

# Metadata as RDF-star annotations on the quoted triple
<< <source> <predicate> <target> >> ad4m:ontology/author "did:key:z6Mk..." .
<< <source> <predicate> <target> >> ad4m:ontology/timestamp "1712025600000" .
<< <source> <predicate> <target> >> ad4m:ontology/proofKey "ed25519:..." .
<< <source> <predicate> <target> >> ad4m:ontology/proofSignature "0x..." .
<< <source> <predicate> <target> >> ad4m:ontology/proofValid "true" .
<< <source> <predicate> <target> >> ad4m:ontology/status "Shared" .
```

Current query pattern:
```sparql
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  ?source ?predicate ?target .
  FILTER(isIRI(?source) && isIRI(?predicate))
  BIND(<< ?source ?predicate ?target >> AS ?ann)
  ?ann ad4m:ontology/author ?author .
  ?ann ad4m:ontology/timestamp ?timestamp .
}
```

### Why this is a problem

1. **Non-standard** — RDF-star is a W3C Community Group Report, not a W3C Recommendation. Many triple stores don't support it.
2. **Oxigraph lock-in** — can't swap to another store (Jena, Blazegraph, Virtuoso, etc.) without rewriting all annotation queries.
3. **No federation** — can't expose AD4M perspectives as standard SPARQL endpoints for external tools to query.
4. **Fragile** — the `BIND(<< ?source ?predicate ?target >> AS ?ann)` pattern is syntactically unusual and confusing to developers unfamiliar with RDF-star.

---

## Alternative Approaches

### Option A: Standard RDF Reification

The W3C-standard way to annotate triples: create a node that *describes* the triple.

```turtle
<source> <predicate> <target> .

_:link1 rdf:type rdf:Statement .
_:link1 rdf:subject <source> .
_:link1 rdf:predicate <predicate> .
_:link1 rdf:object <target> .
_:link1 ad4m:author "did:key:z6Mk..." .
_:link1 ad4m:timestamp "1712025600000" .
```

**Storage:** 4 reification triples + N metadata triples per link = **10 triples per link** (vs 1 + 6 = 7 with RDF-star).

**Query:**
```sparql
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  ?source ?predicate ?target .
  ?stmt rdf:type rdf:Statement ;
        rdf:subject ?source ;
        rdf:predicate ?predicate ;
        rdf:object ?target ;
        ad4m:author ?author ;
        ad4m:timestamp ?timestamp .
}
```

**Verdict:** ❌ Too verbose. 10 triples per link is expensive. The query join through the reification node is slow. Standard but impractical.

---

### Option B: Named Graphs (Recommended) ✅

Each link lives in its own named graph. The graph IRI serves as the link's identity. Metadata triples reference the graph IRI.

```turtle
# The link, in its own named graph
GRAPH <link:a1b2c3d4> {
  <source> <predicate> <target> .
}

# Metadata in the default graph, keyed by graph IRI
<link:a1b2c3d4> ad4m:ontology/author "did:key:z6Mk..." .
<link:a1b2c3d4> ad4m:ontology/timestamp "1712025600000" .
<link:a1b2c3d4> ad4m:ontology/proofKey "ed25519:..." .
<link:a1b2c3d4> ad4m:ontology/proofSignature "0x..." .
<link:a1b2c3d4> ad4m:ontology/proofValid "true" .
<link:a1b2c3d4> ad4m:ontology/status "Shared" .
```

**Storage:** 1 quad (triple + graph name) + 6 metadata triples = **7 triples per link** (same as RDF-star).

**Query patterns:**

```sparql
# Get all links with metadata
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  GRAPH ?g { ?source ?predicate ?target . }
  FILTER(isIRI(?source) && isIRI(?predicate))
  ?g ad4m:ontology/author ?author .
  ?g ad4m:ontology/timestamp ?timestamp .
}

# Get links without metadata (simple, fast)
SELECT ?source ?predicate ?target WHERE {
  ?source ?predicate ?target .
  FILTER(isIRI(?source) && isIRI(?predicate))
}

# Get metadata for a specific link
SELECT ?author ?timestamp ?proofSig WHERE {
  GRAPH ?g { <source> <predicate> <target> . }
  ?g ad4m:ontology/author ?author .
  ?g ad4m:ontology/timestamp ?timestamp .
  ?g ad4m:ontology/proofSignature ?proofSig .
}

# Get all links by a specific author
SELECT ?source ?predicate ?target WHERE {
  ?g ad4m:ontology/author "did:key:z6Mk..." .
  GRAPH ?g { ?source ?predicate ?target . }
}

# Get links newer than a timestamp
SELECT ?source ?predicate ?target ?timestamp WHERE {
  ?g ad4m:ontology/timestamp ?timestamp .
  FILTER(?timestamp > "1712025600000")
  GRAPH ?g { ?source ?predicate ?target . }
}
```

**Verdict:** ✅ Standard SPARQL 1.1. Same storage cost as RDF-star. Clean query patterns. Named graphs are universally supported.

---

### Option C: Singleton Properties

Each link gets a unique predicate IRI that carries the metadata.

```turtle
<source> <predicate/link:a1b2c3d4> <target> .
<predicate/link:a1b2c3d4> rdf:singletonPropertyOf <predicate> .
<predicate/link:a1b2c3d4> ad4m:author "did:key:z6Mk..." .
```

**Verdict:** ❌ Unusual pattern, poor tooling support, confusing semantics. Not recommended.

---

## Detailed Design: Named Graph Approach

### Graph IRI Generation

Each link needs a stable, unique graph IRI. Options:

1. **Hash-based:** `link:<sha256(source + predicate + target + timestamp)>` — deterministic, no collisions for distinct links
2. **UUID-based:** `link:<uuid-v4>` — simple, but requires storing the mapping
3. **Proof-based:** `link:<proofSignature[:16]>` — naturally unique per signed link

**Recommendation:** Hash-based. It's deterministic (same link always gets the same graph IRI), doesn't require extra storage, and is collision-resistant.

```rust
fn make_graph_iri(link: &DecoratedLinkExpression) -> NamedNode {
    use sha2::{Sha256, Digest};
    let mut hasher = Sha256::new();
    hasher.update(link.data.source.as_bytes());
    hasher.update(link.data.predicate.as_bytes());
    hasher.update(link.data.target.as_bytes());
    hasher.update(link.timestamp.as_bytes());
    let hash = hex::encode(hasher.finalize());
    NamedNode::new_unchecked(format!("link:{}", &hash[..32]))
}
```

### Insert Operation

```rust
fn insert_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
    let (source, predicate, target) = make_direct_triple(link);
    let graph = make_graph_iri(link);

    // 1. Insert the direct triple into the link's named graph
    self.store.insert(QuadRef::new(
        source.as_ref(),
        predicate.as_ref(),
        TermRef::NamedNode(target.as_ref()),
        graph.as_ref().into(),
    ))?;

    // 2. Insert metadata triples in the default graph, keyed by graph IRI
    let annotations = [
        (ONT_AUTHOR, &link.author),
        (ONT_TIMESTAMP, &link.timestamp),
        (ONT_PROOF_KEY, &link.proof.key),
        (ONT_PROOF_SIG, &link.proof.signature),
        (ONT_PROOF_VALID, &link.proof.valid.unwrap_or(false).to_string()),
        (ONT_STATUS, status_str(&link.status)),
    ];

    for (pred_uri, value) in &annotations {
        self.store.insert(QuadRef::new(
            graph.as_ref().into(),
            NamedNodeRef::new_unchecked(pred_uri),
            TermRef::Literal(literal(value).as_ref()),
            GraphNameRef::DefaultGraph,
        ))?;
    }

    Ok(())
}
```

### Remove Operation

```rust
fn remove_link_triples(&self, link: &DecoratedLinkExpression) -> Result<(), Error> {
    let graph = make_graph_iri(link);

    // 1. Remove the direct triple from the named graph
    let graph_quads: Vec<_> = self.store
        .quads_for_pattern(None, None, None, Some(graph.as_ref().into()))
        .collect::<Result<Vec<_>, _>>()?;
    for quad in graph_quads {
        self.store.remove(&quad)?;
    }

    // 2. Remove metadata triples from the default graph
    let meta_quads: Vec<_> = self.store
        .quads_for_pattern(Some(graph.as_ref().into()), None, None, Some(GraphNameRef::DefaultGraph))
        .collect::<Result<Vec<_>, _>>()?;
    for quad in meta_quads {
        self.store.remove(&quad)?;
    }

    Ok(())
}
```

### Query Migration

The main SPARQL query pattern in `core/src/model/query-sparql.ts` changes from:

```sparql
-- Current (RDF-star)
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  {conformance joins}
  ?source ?predicate ?target .
  FILTER(isIRI(?source) && isIRI(?predicate))
  BIND(<< ?source ?predicate ?target >> AS ?ann)
  ?ann <ad4m://ontology/author> ?author .
  ?ann <ad4m://ontology/timestamp> ?timestamp .
  {where filters}
}
```

To:

```sparql
-- Named graphs (standard SPARQL 1.1)
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  {conformance joins}
  GRAPH ?linkGraph { ?source ?predicate ?target . }
  FILTER(isIRI(?source) && isIRI(?predicate))
  ?linkGraph <ad4m://ontology/author> ?author .
  ?linkGraph <ad4m://ontology/timestamp> ?timestamp .
  {where filters}
}
```

For queries that don't need metadata (most `Ad4mModel.findAll` queries without author/timestamp ordering), the query can omit the `GRAPH` wrapper entirely:

```sparql
-- Fast path: no metadata needed
SELECT ?source ?predicate ?target WHERE {
  ?source ?predicate ?target .
  FILTER(isIRI(?source) && isIRI(?predicate))
}
```

This works because SPARQL queries the **union of all graphs** by default (the "default dataset"). Triples in named graphs are still matched by unscoped triple patterns.

**Important caveat:** Whether unscoped patterns query named graphs depends on the store's default dataset configuration. Oxigraph includes named graphs in the default dataset, but this should be explicitly verified and documented. If needed, use `FROM NAMED` or configure the dataset explicitly.

---

## Migration Path

### Phase 1: Dual-write (backward compatible)

1. When inserting a link, write both:
   - The RDF-star annotation (existing format)
   - The named graph + metadata triples (new format)
2. Queries continue using RDF-star patterns (no change yet)
3. This allows rollback without data loss

### Phase 2: Query migration

1. Update `buildSPARQLQuery` in `core/src/model/query-sparql.ts` to use `GRAPH ?linkGraph` pattern
2. Update `groupSPARQLResults` to handle the new result format
3. Update the Rust `query_links` method to use named graph patterns
4. Run full test suite — all 313 unit tests + 153 integration tests

### Phase 3: Remove RDF-star

1. Stop writing RDF-star annotations
2. Migration: for existing stores, convert RDF-star annotations to named graph format
3. Remove the `BIND(<< ... >> AS ?ann)` patterns from all queries
4. Clean up the RDF-star triples from existing stores

### Phase 4: Verify portability

1. Test with a different SPARQL 1.1 store (e.g., Jena TDB, Apache Fuseki)
2. Verify all queries work without modification
3. Document the named graph convention in the AD4M spec

---

## Trade-offs

### Advantages over RDF-star

| Aspect | RDF-star | Named Graphs |
|--------|----------|--------------|
| **Standard compliance** | Community Group Report | SPARQL 1.1 Recommendation ✅ |
| **Store compatibility** | Oxigraph, Stardog, GraphDB | All SPARQL 1.1 stores ✅ |
| **Storage cost** | 1 triple + 6 annotations = 7 | 1 quad + 6 triples = 7 |
| **Query without metadata** | Works (direct triple still exists) | Works (default dataset includes named graphs) |
| **Query with metadata** | `BIND(<< ?s ?p ?o >> AS ?ann)` | `GRAPH ?g { ?s ?p ?o }` |
| **Link identity** | Implicit (the quoted triple) | Explicit (the graph IRI) ✅ |
| **Federation** | Non-standard syntax may be rejected | Standard, works everywhere ✅ |
| **Developer familiarity** | Unusual syntax | Named graphs are well-known ✅ |

### Disadvantages

1. **Graph IRI overhead:** Each link creates a named graph. For perspectives with millions of links, this is millions of named graphs. Most stores handle this fine, but it should be benchmarked.
2. **Default dataset semantics:** The behavior of unscoped triple patterns (whether they query named graphs) varies by store configuration. Need to verify and configure explicitly.
3. **Migration effort:** Existing RDF-star data needs conversion. The dual-write approach mitigates risk but adds temporary complexity.

---

## Test Statements

- `it should store link triples in a named graph identified by a deterministic hash IRI`
- `it should store metadata (author, timestamp, proof) in the default graph keyed by the graph IRI`
- `it should generate the same graph IRI for the same link data (deterministic)`
- `it should generate different graph IRIs for links with different timestamps`
- `it should remove all named graph triples AND default graph metadata triples when removing a link`
- `it should return link metadata via GRAPH ?g pattern in SPARQL queries`
- `unscoped triple patterns (no GRAPH wrapper) should still match triples in named graphs`
- `query for links by author should work via ?g ad4m:author ?author ; GRAPH ?g { ?s ?p ?o }`
- `query for links by timestamp range should work via FILTER on metadata triples`
- `all existing Ad4mModel queries should produce identical results after migration`
- `the named graph approach should work with Oxigraph, Jena TDB, and Apache Fuseki`
- `storage cost per link should be ≤ 7 triples (1 quad + 6 metadata)`
- `query performance for findAll without metadata should be within 10% of current RDF-star performance`
- `query performance for findAll with metadata should be within 20% of current RDF-star performance`
- `perspectives with 100,000 links should have acceptable named graph creation/deletion performance`
