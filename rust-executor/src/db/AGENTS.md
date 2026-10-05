# db/ — agent guide

Child modules of `../db.rs`. Each adds an `impl Ad4mDb` block for one area so
`db.rs` stops growing; being children, they reach the private `conn` field.

| File | Role |
|---|---|
| `expression_store.rs` | The expression cache (`expression`) and the publish queue (`expression_publish_queue`), both keyed by full expression URL `<language hash>://<address>`. Creates both tables (`create_tables`, called from `Ad4mDb::new`) |

## Invariants

- The cache holds only expressions their language reported immutable. Readers in
  `languages/expressions.rs` rely on this to skip the `isImmutableExpression` call on a hit.
- Key by the language's hash, never an alias: two content-addressed languages can
  mint the same address for different data.
- Publish-queue rows are never dropped: a row may hold the only copy of an
  expression outside this node. `export_all_to_json` / `import_from_json` carry them too.
