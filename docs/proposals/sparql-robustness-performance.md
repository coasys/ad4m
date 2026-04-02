# AD4M SPARQL Migration — Comprehensive Review & Improvement Proposals

**Branch:** `feat/sparql-replaces-surrealdb` (PR [#760](https://github.com/coasys/ad4m/pull/760))
**Scope:** 88 files, 10,541 insertions, 11,027 deletions
**Status:** All 7 CI suites green, Flux deploy preview functional

---

## 1. Architecture Summary

The migration replaces SurrealDB with [Oxigraph](https://github.com/oxigraph/oxigraph) (a Rust-native in-memory RDF triple store) as the query engine for AD4M perspectives.

**Core mapping:**
- Each AD4M link (`source → predicate → target`) becomes a direct RDF triple
- Link metadata (author, timestamp, proof) stored as RDF-star annotations on quoted triples
- Custom SPARQL functions (`fn::parse_literal`, `fn::strip_html`) bridge AD4M's literal URI scheme to SPARQL pattern matching
- TypeScript `Ad4mModel` generates SPARQL queries, hydrates results, and applies JS-level post-filtering

**Key architectural decisions:**
- All property-level where-filtering done in JavaScript after hydration (SPARQL `parse_literal` is unreliable across environments)
- Subscription result fingerprinting prevents spurious callbacks when raw SPARQL results change but filtered results don't
- Dual-engine support: SPARQL (default) and Prolog (fallback) in `ModelQueryBuilder`

---

## 2. What Works Well

### 2.1 Clean Rust-side triple store implementation
`sparql_service/mod.rs` (1,128 lines) is well-structured. The triple mapping is clear and correct. RDF-star annotations for metadata are elegant — they keep the core triple pattern clean (`source predicate target`) while attaching provenance data. The custom `parse_literal_fn` correctly handles all AD4M literal subtypes (string, number, boolean, json) including signed expression unwrapping.

### 2.2 JS post-filtering strategy
Moving all property-level where-filtering to JavaScript after hydration was the right call. `matchesCondition()` handles equality, NOT, IN, gt/gte/lt/lte, between, and contains — comprehensive, well-tested (17 unit tests), and immune to cross-environment SPARQL function differences.

### 2.3 Subscription fingerprinting
`buildFingerprint` in `ModelQueryBuilder.subscribe()` serializes all enumerable properties, sorted by ID. Solves the real problem of subscription callbacks firing when a non-matching record is added (raw SPARQL changes, filtered set doesn't).

### 2.4 Comprehensive test coverage
39 new unit tests covering matchesCondition, hydrateFromLinks, instancesFromQueryResult, groupSPARQLResults, buildSPARQLQuery, and property deletion timing. Plus 153 integration tests, 65 MCP tests, and the JS integration suite all pass.

### 2.5 Migration path
`migration.rs` handles converting existing perspectives from SQLite to SPARQL, including the `literal://` → `literal:` URI format conversion. Well-tested with dedicated migration tests.

### 2.6 Massive complexity reduction
`perspective_instance.rs`: 2,934 lines removed, 275 added. The SurrealDB service (2,499 lines) is gone entirely. The query layer is simpler, more standard, and more testable.

---

## 3. Problems & Proposed Solutions


#### Tier 1 — Robustness (Data Safety)

### 3.1 🔴 CRITICAL: No concurrent write protection

**Problem:** `SparqlService` wraps the store in `Arc` but not `Mutex` or `RwLock`. Oxigraph's `Store` handles concurrent reads safely, but `insert` and `remove` may have surprising behavior under concurrent writes. Link sync, language callbacks, and subscription updates can happen simultaneously.

The current code works because perspective operations are largely serialized through async/await, but this is an implicit assumption, not an enforced guarantee.

**Impact:** Potential data corruption or panics under high-concurrency scenarios (e.g., SFU with multiple users adding links simultaneously).

**Proposed solution:**
```rust
pub struct SparqlService {
    store: Arc<RwLock<Store>>,
}
```

Use `RwLock` to allow concurrent reads (SPARQL queries) but exclusive writes (insert/remove). The read path is the hot path (queries), so `RwLock` is better than `Mutex`. Alternatively, Oxigraph's `Store` may already be internally thread-safe for writes — this should be verified against the Oxigraph documentation and if so, documented as an explicit assumption.

**Effort:** Small — wrap in RwLock, add `.read()` / `.write()` calls at each access point.

**Test statements:**
- `it should allow concurrent SPARQL reads without blocking`
- `it should serialize concurrent write operations (insert + remove) without data corruption`
- `it should not deadlock when a read query runs during a write operation`
- `it should maintain triple count consistency when 10 threads add links simultaneously`
- `it should not lose triples when add_link and remove_link race on different links`
- `it should return correct query results immediately after a concurrent write completes`

---

### 3.2 🔴 CRITICAL: In-memory store — no persistence

**Problem:** `SparqlService::new()` creates an in-memory Oxigraph store. The `_data_path` parameter is accepted but **ignored** (prefixed with `_`). Every restart rebuilds the SPARQL index from scratch by re-reading all links from SQLite via `sync_existing_links_to_sparql`. For large perspectives (thousands of links), this is an O(n) startup cost.

Anyone reading the constructor signature would assume the store persists to disk, but it doesn't.

**Impact:** Startup latency scales linearly with perspective size. For production nodes with many large perspectives, restart time could become significant.

**Proposed solution:**
```rust
pub fn new(data_path: Option<&str>) -> Result<Self, Error> {
    let store = match data_path {
        Some(path) => {
            let store_path = std::path::Path::new(path).join("sparql_store");
            std::fs::create_dir_all(&store_path)?;
            Store::open(&store_path)?
        }
        None => Store::new()?,  // In-memory for tests
    };
    Ok(SparqlService { store: Arc::new(store) })
}
```

Then pass the perspective's data path when constructing the service. The `reload()` method becomes unnecessary for startup — only needed for migration. Oxigraph's persistent store uses RocksDB under the hood and handles concurrent reads efficiently.

**Effort:** Small — Oxigraph already supports `Store::open(path)`. Main work is plumbing the data path through and removing the startup rebuild.

**Test statements:**
- `it should persist triples across store close and reopen when data_path is provided`
- `it should recover all links after executor restart without calling sync_existing_links_to_sparql`
- `it should use in-memory store when data_path is None (test mode)`
- `it should create the sparql_store directory if it does not exist`
- `it should handle corrupt store files gracefully (fall back to rebuild from SQLite)`
- `it should complete startup within 500ms for a perspective with 10,000 links when using persistent store`
- `it should complete startup within 500ms for a perspective with 0 links when using in-memory store`

---

### 3.3 🟢 MINOR: Pubsub type safety

**Problem:** The `APPS_CHANGED` bug (publishing `""` where `Option<Apps>` was expected) exposed a systemic issue — the pubsub system uses string serialization with no type checking. Any publisher can silently corrupt any subscriber.

**Proposed solution:**
1. Create typed pubsub channels: `PubSubChannel<T>` that enforces serialization at the type level
2. Or at minimum, add a message type tag: `{ "type": "Apps", "data": {...} }` with validation on the subscriber side

**Effort:** Medium — touches the pubsub infrastructure.

**Test statements:**
- `publishing a typed message to APPS_CHANGED should be deserializable as Option<Apps>`
- `publishing an empty string to a typed channel should fail at compile time or produce a clear runtime error`
- `subscriber should receive a type validation error (not EOF) when message format is wrong`
- `all pubsub topics should have documented expected message types`
- `PubSubChannel<T>::publish should enforce serde::Serialize on the message type`
- `PubSubChannel<T>::subscribe should enforce serde::Deserialize on the handler type`
- `mismatched publish/subscribe types should produce a compile-time error (typed channels) or a clear runtime error (tagged messages)`

---


#### Tier 2 — Performance (Query Speed & Scalability)

### 3.4 🟠 MAJOR: N+1 relation hydration queries

**Problem:** `hydrateRelations` in `hydration.ts` calls `perspective.get(new LinkQuery(...))` for each instance + each relation. For `findAll` returning 50 instances with 3 `@HasMany` relations each, that's 150 additional queries. The batch SPARQL query (`query-sparql-batch.ts`, 304 lines) exists but **isn't used for include hydration** — it's only used for initial instance fetching.

**Impact:** Performance degrades quadratically with result set size × relation count. This is the main bottleneck for Flux's channel/message rendering.

**Proposed solution:**
1. Use `buildBatchSPARQLQuery` to fetch all relation targets in a single query per relation type
2. Group the targets by parent instance ID
3. Hydrate all instances in one pass instead of N individual queries

```typescript
// Instead of:
for (const instance of instances) {
  const links = await perspective.get(new LinkQuery({ source: instance.id, predicate: relMeta.predicate }));
  instance[relName] = links.map(l => l.data.target);
}

// Do:
const allTargets = await perspective.querySparql(`
  SELECT ?source ?target WHERE {
    VALUES ?source { ${instances.map(i => `<${i.id}>`).join(' ')} }
    ?source <${relMeta.predicate}> ?target .
  }
`);
// Group by source, assign to instances
```

**Effort:** Medium — the batch query infrastructure exists, just needs to be wired into the include hydration path.

**Test statements:**
- `findAll with include should issue at most 1 SPARQL query per relation type, not 1 per instance`
- `findAll({ include: { comments: true } }) for 50 instances should complete in <200ms (not 50× single-query latency)`
- `batch relation hydration should return identical results to N+1 hydration`
- `batch hydration should correctly group targets by parent instance ID`
- `batch hydration with nested includes (post → comments → reactions) should use batched queries at each level`
- `batch hydration should handle empty relation sets without error`
- `batch hydration with where sub-query on relation should filter correctly`
- `batch hydration with limit sub-query on relation should cap per-parent, not globally`

---

### 3.5 🟡 MODERATE: No push-down filtering for literal properties

**Problem:** For properties with `resolveLanguage: "literal"` (which is most properties), SPARQL only adds a JOIN to ensure the property exists but does NO value filtering. `findAll({ where: { name: "Alice" } })` fetches ALL instances and filters client-side. With 10,000 instances and 1 match, 9,999 instances are hydrated and discarded.

**Impact:** Query performance scales with total instance count, not result count. Acceptable for small perspectives, problematic at scale.

**Proposed solution:**
For literal properties where the value is known at query time, add a SPARQL FILTER using `parse_literal`:
```sparql
?source <recipe://name> ?wTarget_name .
FILTER(fn::parse_literal(?wTarget_name) = "Alice")
```

This was originally attempted but removed because `parse_literal` was unreliable. The issue was specific to comparison operators (gt/lt) on string-encoded values. For **exact equality matching**, `parse_literal` should work reliably. Keep the JS post-filter as a safety net, but add the SPARQL filter to reduce the result set.

**Effort:** Small — add the FILTER back for equality and IN operators only. Keep JS post-filter for comparison operators.

**Test statements:**
- `SPARQL query for where: { name: "Alice" } should include FILTER(fn::parse_literal(?target) = "Alice")`
- `SPARQL query for where: { name: ["Alice", "Bob"] } should include FILTER with IN clause`
- `SPARQL query for where: { rating: { gt: 5 } } should NOT include SPARQL FILTER (JS post-filter only)`
- `findAll with where: { name: "Alice" } on 10,000 instances should return in <100ms (SPARQL-level filtering)`
- `findAll with literal equality push-down should return identical results to JS-only filtering`
- `push-down filter should correctly handle URL-encoded literal values`
- `push-down filter should correctly handle signed expression JSON (extracting .data field)`

---

### 3.6 🟡 MODERATE: Subscription re-query frequency

**Problem:** SPARQL subscriptions re-run the entire query whenever ANY link in the perspective changes — not just links matching the query. The Rust executor's subscription loop compares result strings and only sends updates when they differ, but the query itself still executes on every change.

**Impact:** For perspectives with high write throughput (e.g., real-time messaging), this means every incoming message triggers a full re-query for every active subscription, even unrelated ones.

**Proposed solution:**
1. **Short term:** Add predicate-based subscription filtering in the Rust executor. When a link is added/removed, only re-run subscriptions whose SPARQL query references that link's predicate.
2. **Medium term:** Parse the SPARQL query to extract referenced predicates at subscription registration time. Use these as a filter to skip irrelevant re-queries.
3. **Long term:** Implement incremental SPARQL evaluation — only re-compute the delta, not the full result set.

**Effort:** Medium for predicate filtering, large for incremental evaluation.

**Test statements:**
- `adding a link with predicate P should only re-query subscriptions that reference predicate P`
- `adding a link with predicate P should NOT re-query subscriptions that only reference predicate Q`
- `subscription registration should extract referenced predicates from the SPARQL query`
- `subscription with WHERE clause on predicate P should not fire when predicate Q changes`
- `subscription without predicate filter should still fire on any change (backward compatible)`
- `subscription re-query count should be proportional to relevant changes, not total changes`
- `100 link additions to predicate P should trigger at most 100 re-queries for P-subscriptions, not 100 × N for all subscriptions`

---

### 3.7 🟡 MODERATE: Fingerprint performance

**Problem:** `buildFingerprint` serializes every enumerable property of every result instance via `JSON.stringify` on every subscription update. For large result sets with complex objects (nested relations, long text fields), this is expensive.

**Proposed solution:**
Replace full serialization with a lightweight hash:
```typescript
const buildFingerprint = (results: any[]) => {
    // Hash: IDs + count + last-modified timestamp
    const ids = results.map(r => r.id).sort().join(',');
    const timestamps = results.map(r => r.updatedAt || r.timestamp || '').join(',');
    return `${results.length}:${ids}:${timestamps}`;
};
```

This catches additions, removals, and timestamp changes without serializing full objects. For property-level change detection (e.g., a title edit), the subscription re-query already produces different raw SPARQL results, which triggers the `processResults` path — the fingerprint only needs to catch cases where JS post-filtering eliminates the difference.

**Effort:** Small.

**Test statements:**
- `buildFingerprint for 100 instances should complete in <5ms`
- `buildFingerprint should detect instance addition (new ID)`
- `buildFingerprint should detect instance removal (missing ID)`
- `buildFingerprint should detect property value change (different timestamp)`
- `buildFingerprint should NOT trigger false positives for identical result sets`
- `lightweight fingerprint should produce identical suppress/emit decisions as full JSON.stringify fingerprint`

---


#### Tier 3 — Correctness & Safety

### 3.8 🟡 MODERATE: SPARQL query validation is minimal

**Problem:** `validate_readonly_query` does string-level checking (starts with SELECT/ASK/etc., doesn't contain INSERT/DELETE). This could be bypassed with comments or string literals:
```sparql
SELECT * WHERE { # DELETE
  ?s ?p ?o
}
```

**Impact:** Low for single-user mode. For multi-user mode with untrusted user queries, this is a potential injection vector.

**Proposed solution:**
Use Oxigraph's query parser to validate:
```rust
fn validate_readonly_query(query: &str) -> Result<(), Error> {
    match oxigraph::sparql::Query::parse(query, None) {
        Ok(parsed) => match parsed {
            oxigraph::sparql::Query::Select { .. } |
            oxigraph::sparql::Query::Ask { .. } |
            oxigraph::sparql::Query::Construct { .. } |
            oxigraph::sparql::Query::Describe { .. } => Ok(()),
        },
        Err(e) => Err(anyhow!("Invalid SPARQL query: {}", e)),
    }
}
```

This is structurally sound — if it parses as a SELECT, it IS a SELECT. No string manipulation can change that.

**Effort:** Small.

**Test statements:**
- `validate_readonly_query should accept valid SELECT queries`
- `validate_readonly_query should accept valid ASK queries`
- `validate_readonly_query should accept valid CONSTRUCT queries`
- `validate_readonly_query should reject INSERT DATA queries`
- `validate_readonly_query should reject DELETE WHERE queries`
- `validate_readonly_query should reject DROP GRAPH queries`
- `validate_readonly_query should reject queries with INSERT hidden in comments`
- `validate_readonly_query should reject queries with DELETE hidden in string literals`
- `validate_readonly_query should reject syntactically invalid SPARQL`
- `validate_readonly_query should use Oxigraph parser, not string matching`

---

### 3.9 🟡 MODERATE: convertGetterToSPARQL is a fragile regex parser

**Problem:** `hydration.ts:458` parses SurrealDB-style getter strings with regex:
```
->link[WHERE predicate = 'P'].out[WHERE count(->link[WHERE predicate = 'Q' AND out.uri = 'V']) > 0].uri
```

This handles known patterns but returns `null` silently for unrecognized patterns. Future getter formats will fail without warning.

**Impact:** New Subject Class definitions with custom getters could silently produce empty results.

**Proposed solution:**
1. **Short term:** Add a `console.warn` when `convertGetterToSPARQL` returns null for a non-empty getter string
2. **Medium term:** Define a proper getter AST instead of regex parsing. The SHACL generator (`shacl-gen.ts`) already knows the getter structure — emit it as structured data (JSON) instead of a SurrealDB query string
3. **Long term:** Deprecate string-based getters entirely. The `@HasMany(() => TargetClass, { through: predicate })` decorator pattern + auto-generated conformance getters (from `buildConformanceFilter`) is the correct approach

**Effort:** Small for the warning, medium for the AST approach.

**Test statements:**
- `convertGetterToSPARQL should log a warning when it returns null for a non-empty getter string`
- `convertGetterToSPARQL should parse simple relation pattern: ->link[WHERE predicate = 'P'].out.uri`
- `convertGetterToSPARQL should parse flag conformance: .out[WHERE count(->link[WHERE predicate = 'P' AND out.uri = 'V']) > 0]`
- `convertGetterToSPARQL should parse parse_literal conformance: fn::parse_literal(out.uri) = 'V'`
- `convertGetterToSPARQL should parse multiple AND conditions in WHERE clause`
- `convertGetterToSPARQL should handle [0] scalar suffix`
- `SHACL generator should emit getter as structured JSON instead of SurrealDB query string`
- `auto-generated conformance getters from @HasMany(() => Target) should produce valid SPARQL without string parsing`

---

### 3.10 🟢 MINOR: Error messages from SPARQL failures are opaque

**Problem:** When a SPARQL query fails (syntax error, missing variable), the error propagates as a generic Oxigraph error string without the query text.

**Proposed solution:**
```rust
pub fn query(&self, query_string: &str) -> Result<String, Error> {
    validate_readonly_query(query_string)?;
    let options = self.query_options();
    self.store.query_opt(query_string, options)
        .map_err(|e| anyhow!("SPARQL query failed: {}\nQuery: {}", e, query_string))
        ...
}
```

**Effort:** Trivial.

**Test statements:**
- `SPARQL query syntax error should include the query text in the error message`
- `SPARQL query with unbound variable should include the query text in the error message`
- `SPARQL query referencing non-existent function should include the query text in the error`
- `error message should be human-readable (not just Oxigraph internal error code)`
- `error message should truncate query text at 500 chars for very large queries`

---


#### Tier 4 — Technical Debt

### 3.11 🟠 MAJOR: Dead Prolog code paths in Ad4mModel

**Problem:** `Ad4mModel.ts` still has complete Prolog query generation — `queryToProlog` (line 1077), `countQueryToProlog` (line 1274), `instancesFromPrologResult` (line 1151) — 10+ references. `ModelQueryBuilder` has full `else` branches for every SPARQL operation that fall through to Prolog. This is ~300 lines of code that runs on no default path.

**Impact:** Maintenance burden. Every change to the query pipeline needs to be considered against both engines. The Prolog path will silently rot since it's never exercised in CI.

**Proposed solution:**
1. **Immediately:** Add deprecation warnings to the Prolog path methods. Add a `console.warn('Prolog engine is deprecated, use SPARQL')` in `ModelQueryBuilder.engine('prolog')`.
2. **Next release:** Remove Prolog query generation from `Ad4mModel.ts` entirely. Keep `perspective.infer()` for SDNA introspection (Subject.ts still needs it), but remove the `queryToProlog`, `countQueryToProlog`, `instancesFromPrologResult` methods.
3. **Remove the `engine()` method** from `ModelQueryBuilder` — SPARQL is the only engine.

**Effort:** Medium — straightforward deletion, but need to verify no external consumers call `engine('prolog')`.

**Test statements:**
- `it should default to SPARQL engine without explicit engine() call`
- `it should throw or warn when engine('prolog') is called (deprecated)`
- `it should not export queryToProlog, countQueryToProlog, or instancesFromPrologResult`
- `it should produce identical results via SPARQL for all operations that previously used Prolog`
- `findAll with where clause should work without any Prolog engine available`
- `subscribe() should work without any Prolog engine available`
- `count() should work without any Prolog engine available`

---

### 3.12 🟠 MAJOR: Subject.ts ↔ Ad4mModel.ts split

**Problem:** Two different code paths for subject class operations:
- `Subject.ts` (legacy, 158 lines): Uses Prolog (`perspective.infer()`) for SDNA introspection (property discovery, setter evaluation), falls back to SPARQL for data queries
- `Ad4mModel.ts` (new, 2,022 lines): Uses SPARQL for everything via decorator metadata

Flux's signup flow uses `Subject.ts` via `ensureSDNASubjectClass`. This means Prolog is still required for basic operations.

**Impact:** Two maintenance surfaces. Bugs fixed in one path may not be fixed in the other. The Prolog dependency can't be fully removed until Subject.ts is migrated.

**Proposed solution:**
1. `PerspectiveProxy` already has `getSubjectClassMetadataFromSDNA()` which parses SHACL shapes directly — no Prolog needed
2. Refactor `Subject.ts` to use SHACL-based metadata resolution instead of Prolog `infer()` calls
3. The setter evaluation in Subject.ts uses `eval()` on Prolog-generated setter strings — these should be converted to direct link operations based on SHACL property metadata
4. **Ultimate goal:** `Subject.ts` becomes a thin wrapper around `Ad4mModel` patterns, or is deprecated entirely in favor of `Ad4mModel`

**Effort:** Large — Subject.ts is deeply entangled with the Prolog SDNA format. Best done as a separate PR.

**Test statements:**
- `Subject.get() should resolve property values via SPARQL/SHACL metadata, not Prolog infer()`
- `Subject.set() should write links directly based on SHACL property metadata, not eval'd Prolog setters`
- `Subject should discover properties from SHACL shapes without calling perspective.infer()`
- `Subject should discover collections from SHACL shapes without calling perspective.infer()`
- `ensureSDNASubjectClass should work with Prolog engine disabled`
- `Flux signup profile creation should complete without any Prolog query`
- `Subject and Ad4mModel should return identical data for the same subject class and instance`
- `Subject property getters should use getSubjectClassMetadataFromSDNA() not infer()`

---

### 3.13 🟢 MINOR: RDF-star is Oxigraph-specific

**Problem:** The migration uses RDF-star (quoted triples for annotations), which is not standard SPARQL 1.1. If AD4M ever needs a different triple store, the RDF-star dependency would need abstraction.

**Impact:** Low — Oxigraph is a good fit and RDF-star is on the W3C standards track. But worth documenting as a known coupling.

**Proposed solution:** Document the RDF-star dependency in the architecture docs. If a store swap is ever needed, the annotation triples could be converted to reification (standard RDF) or a property graph model.

**Effort:** Documentation only.

**Test statements:**
- `RDF-star annotation triples should be queryable via BIND(<< ?s ?p ?o >> AS ?ann) pattern`
- `author and timestamp should be retrievable from RDF-star annotations on any link`
- `removing a link should also remove all its RDF-star annotation triples`
- `the SPARQL service should document RDF-star as a required store capability`
- `if RDF-star is unavailable, the service should fail with a clear error at initialization`

---

### 3.14 🟢 MINOR: `literal:` URI format is non-standard

**Problem:** AD4M uses `literal:string:`, `literal:json:` etc. as URI schemes. These are not valid IRIs per RFC 3986. Oxigraph accepts them via `NamedNode::new_unchecked`, but stricter RDF tooling would reject them.

**Impact:** Low — AD4M's URI scheme is internal. But it prevents interoperability with standard RDF tools (SPARQL endpoints, Linked Data platforms).

**Proposed solution:** No immediate action needed. If interoperability becomes important, prefix with a proper namespace: `ad4m:literal:string:` → `urn:ad4m:literal:string:` or use datatype annotations on standard RDF literals.

**Effort:** Large if pursued (touches all literal handling), but not urgent.

**Test statements:**
- `literal:string: URIs should be accepted by NamedNode::new_unchecked without error`
- `literal:json: URIs with URL-encoded JSON should round-trip through insert + query`
- `literal:number: URIs should be queryable via fn::parse_literal`
- `literal:boolean: URIs should be queryable via fn::parse_literal`
- `Oxigraph should not reject any AD4M literal URI format currently in use`
- `if migrating to urn:ad4m: prefix, all existing literal URIs should be convertible`

---

## 4. Priority Ranking (Performance & Robustness First)

### Tier 1 — Robustness (data safety)
| # | Issue | Severity | Effort |
|---|-------|----------|--------|
| 3.1 | No concurrent write protection | 🔴 Critical | Small |
| 3.2 | In-memory store / no persistence | 🔴 Critical | Small |
| 3.3 | Pubsub type safety | 🟢 Minor | Medium |

### Tier 2 — Performance (query speed & scalability)
| # | Issue | Severity | Effort |
|---|-------|----------|--------|
| 3.4 | N+1 relation hydration | 🟠 Major | Medium |
| 3.5 | No push-down for literal equality | 🟡 Moderate | Small |
| 3.6 | Subscription re-query frequency | 🟡 Moderate | Medium |
| 3.7 | Fingerprint performance | 🟡 Moderate | Small |

### Tier 3 — Correctness & Safety
| # | Issue | Severity | Effort |
|---|-------|----------|--------|
| 3.8 | SPARQL validation | 🟡 Moderate | Small |
| 3.9 | Fragile getter parser | 🟡 Moderate | Small |
| 3.10 | Opaque error messages | 🟢 Minor | Trivial |

### Tier 4 — Technical Debt
| # | Issue | Severity | Effort |
|---|-------|----------|--------|
| 3.11 | Dead Prolog code | 🟠 Major | Medium |
| 3.12 | Subject.ts legacy split | 🟠 Major | Large |
| 3.13 | RDF-star coupling | 🟢 Minor | Docs |
| 3.14 | literal: URI format | 🟢 Minor | Large |

---

## 5. Verdict

The SPARQL migration is **architecturally sound, well-tested, and ready to merge**. It's a net simplification: removes the SurrealDB dependency (2,499 lines), replaces it with a standard query language backed by a Rust-native store (1,128 lines), and provides a clear path to further optimization.

The two critical issues (persistence and concurrency) should be addressed in a fast-follow PR. The performance issues (N+1 hydration, push-down filtering) are existing architectural limitations that the migration doesn't worsen — they're just more visible now.

The dead Prolog code and Subject.ts split are longer-term cleanup items that should be addressed as part of the V1 spec work.
