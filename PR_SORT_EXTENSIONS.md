# feat: model query sorting by projection counts and related model properties

## Summary

Extends the ad4m model query pipeline to support two new sort key types in `order`:

- **Projection count sort** — sort by the number of related items, e.g. sort posts by total likes:
  ```ts
  order: { $likeCount: 'DESC' }
  projections: { $likeCount: { from: 'likes', count: true } }
  ```

- **Relation-property sort** — sort by a scalar property on a directly-related model, using dotted-path syntax:
  ```ts
  order: { 'location.name': 'ASC' }
  include: { location: {} }
  ```

Both require `limit` or `offset` to be set (which triggers the TwoPhase SPARQL plan). This is the expected usage — sorting an unbounded result set by a derived aggregate without pagination would require a full-table GROUP BY and is better handled as a separate concern.

## How it works

The existing query pipeline already uses a **TwoPhase execution plan** when pagination is pushed to SPARQL:
1. Phase 1 — a lightweight pagination subquery returns source IRIs in sorted order with LIMIT/OFFSET applied
2. Phase 2 — a `VALUES ?source { ... }` property fetch retrieves the full data for that page

Both new sort types are pushed into the phase-1 subquery as SPARQL aggregates:

**Projection count** uses `COUNT(DISTINCT)` with an `OPTIONAL` join:
```sparql
OPTIONAL { ?source <ns://has-like> ?_proj_t_0 . }
(COUNT(DISTINCT ?_proj_t_0) AS ?_proj_sort_0)
...
ORDER BY DESC(?_proj_sort_0)
```
COUNT returns 0 for sources with no matches, so no null-guard is needed.

**Relation-property** uses a double-`OPTIONAL` join with `SAMPLE` aggregation and nulls-to-end ordering:
```sparql
OPTIONAL {
  ?source <ns://has-location> ?_rel_0 .
  OPTIONAL {
    ?_rel_0 <ns://loc-name> ?_rp_raw_0 .
    BIND(STR(?_rp_raw_0) AS ?_rp_str_0)
    BIND(xsd:double(STR(?_rp_raw_0)) AS ?_rp_num_0)
  }
}
(SAMPLE(?_rp_num_0) AS ?_rp_num_0)
(SAMPLE(?_rp_str_0) AS ?_rp_str_0)
...
ORDER BY ASC(IF(BOUND(?_rp_str_0), 0, 1)) ASC(?_rp_num_0) ASC(?_rp_str_0)
```
Numeric-first ordering mirrors the existing `Property` sort pattern. Sources with no linked relation sort to the end.

### TwoPhase ordering fix

A pre-existing issue was discovered and fixed as part of this work: `group_results_by_source` uses a `BTreeMap` (alphabetical by source IRI), which discards the pagination subquery's sort order after hydration. This meant even `Property` sorts could be silently overridden in edge cases where IRI-alphabetical order disagreed with the property sort order.

The fix captures the phase-1 source order before hydration and restores it afterwards. The `sort_instances` call is then skipped for the pushed-pagination path since the SPARQL-established order is already correct.

## Files changed

| File | Change |
|------|--------|
| `rust-executor/src/perspectives/model_query/types.rs` | Two new `SortKey` variants: `Projection(String)` and `RelationProperty { rel_pred, prop_pred }` |
| `rust-executor/src/perspectives/model_query/sparql_builder.rs` | `build_pagination_subquery` extended to emit SPARQL for both new key types; unit tests added |
| `rust-executor/src/perspectives/model_query/query.rs` | `order_keys_pushable` extended; sort key building extended; TwoPhase ordering fix |
| `rust-executor/src/perspectives/model_query/filtering.rs` | `sort_instances` uses `extract_sort_value` for dotted-path traversal; unit tests added |
| `core/src/model/types.ts` | `StrictTypedOrder` extended to accept `$`-prefixed projection keys and a string index signature for dotted paths |
| `rust-executor/src/perspectives/model_query/integration_tests.rs` | 6 new integration tests (see below) |

## Tests

**Unit tests** (`sparql_builder.rs`):
- Projection sort ASC, DESC
- Projection sort — COUNT DISTINCT present in generated SPARQL
- Relation-property sort — double-OPTIONAL present in generated SPARQL
- Relation-property sort DESC
- Combined property + projection sort (multi-key)
- Timestamp + projection sort uses GROUP BY

**Unit tests** (`filtering.rs`):
- `extract_sort_value` plain key
- `extract_sort_value` dotted path through object
- `extract_sort_value` dotted path through array (uses first element)
- `extract_sort_value` dotted path through empty array
- `extract_sort_value` dotted path with scalar intermediate (returns null)
- `sort_instances` dotted path ASC, DESC, nulls-to-end, plain key unchanged

**Integration tests** (`integration_tests.rs`):
- `test_sort_by_projection_count_desc` — 3 posts with 5/2/0 likes, DESC order
- `test_sort_by_projection_count_asc` — 3 posts with 3/0/7 likes, ASC order
- `test_sort_by_projection_count_with_pagination` — 5 posts, page 1 of 2 DESC, total_count verified
- `test_sort_by_relation_property_asc` — 3 posts linked to locations, ASC by name
- `test_sort_by_relation_property_desc` — same data, DESC
- `test_sort_by_relation_property_with_missing_relation` — post with no location sorts to end

## Known limitation

Projection and relation-property sorts are only applied when `limit` or `offset` is also set. Without pagination, the Rust-side fallback sort fires but the values it needs (`$likeCount`, hydrated relations) aren't resolved yet at that point in the pipeline — the sort silently becomes a no-op. This is acceptable for the intended use case. A follow-up can extend the TwoPhase trigger to fire on advanced sort keys even without pagination if needed.

## Branch

Based on `feat/model-query-construct-hydration`.
