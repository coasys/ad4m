# Migration Removal Guide

This document explains how to remove the one-time Rusqlite-to-SurrealDB migration code after all users have migrated.

## When to Remove

Remove this migration code in a future version (e.g., version 0.13.0 or later) after confirming:
- All active users have upgraded to version 0.11.2 or later
- Sufficient time has passed for users to run the migration (recommend at least 2-3 releases)
- No support requests related to missing links after migration

## Removal Steps

### 1. Delete the Migration Module
```bash
rm src/perspectives/migration.rs
```

### 2. Update src/perspectives/mod.rs

Remove the migration module declaration:
```rust
// Remove this line:
pub mod migration; // TODO: Remove this module after all users have migrated to SurrealDB
```

Remove migration calls from `initialize_from_db()`:
```rust
// Remove these lines (around line 74-78):
// Migrate links from Rusqlite to SurrealDB (one-time migration)
// TODO: Remove this migration call after all users have migrated
if let Err(e) = migration::migrate_links_from_rusqlite_to_surrealdb(&handle_clone.uuid, &surreal_service).await {
    log::error!("Failed to migrate links for perspective {}: {}", handle_clone.uuid, e);
}
```

Remove migration calls from `add_perspective()`:
```rust
// Remove these lines (around line 119-123):
// Migrate links from Rusqlite to SurrealDB (one-time migration)
// TODO: Remove this migration call after all users have migrated
if let Err(e) = migration::migrate_links_from_rusqlite_to_surrealdb(&handle.uuid, &surreal_service).await {
    log::error!("Failed to migrate links for perspective {}: {}", handle.uuid, e);
}
```

### 3. (Optional) Clean Up Database Migration Support

If you want to completely remove migration infrastructure from src/db.rs:

Remove the migration tracking table from `new()`:
```rust
// Remove these lines (around line 158-165):
conn.execute(
    "CREATE TABLE IF NOT EXISTS perspective_link_migration (
        id INTEGER PRIMARY KEY,
        perspective_uuid TEXT NOT NULL UNIQUE,
        migrated_at TEXT NOT NULL
     )",
    [],
)?;
```

Remove migration methods:
```rust
// Remove these methods:
- pub fn is_perspective_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<bool>
- pub fn mark_perspective_as_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<()>
- pub fn delete_all_links_for_perspective(&self, perspective_uuid: &str) -> Ad4mDbResult<usize>
```

**Note:** Keeping these methods is harmless and may be useful for future migrations, so removal is optional.

### 4. Verify Removal

After removing the migration code:

1. **Build the project:**
   ```bash
   cargo build --lib
   ```

2. **Run tests:**
   ```bash
   cargo test --lib
   ```

3. **Verify no references remain:**
   ```bash
   grep -r "migrate_links_from_rusqlite_to_surrealdb" rust-executor/src/
   grep -r "migration::" rust-executor/src/perspectives/mod.rs
   ```
   Both should return no results.

## Migration Summary

### What the Migration Did
- Moved all link data from the centralized Rusqlite database to per-perspective SurrealDB databases
- Preserved all link metadata (author, timestamp, proof, status)
- Ensured idempotent operation (safe to run multiple times)
- Cleaned up old link data from Rusqlite after successful migration
- Tracked migration status per perspective to prevent repeated migrations

### Files Affected by Migration

**Will be removed:**
- `src/perspectives/migration.rs` - The migration logic and tests

**Will be updated:**
- `src/perspectives/mod.rs` - Remove module declaration and migration calls

**Can optionally be cleaned up:**
- `src/db.rs` - Migration tracking methods (optional to remove)
