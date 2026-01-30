# CodeRabbit PR #652 Fix Tasks

## Critical Issues

### 1. perspective_instance.rs (lines 2425-2457)
**Issue**: `persist_link_diff` and `retry_surreal_op` silently swallow errors, risking desync.
**Fix**: Make both return `Result` and propagate failures to callers.

```rust
// Change retry_surreal_op signature to return Result
async fn retry_surreal_op<F, Fut>(
    op: F,
    uuid: &str,
    op_name: &str,
) -> Result<(), anyhow::Error>

// Change persist_link_diff signature to return Result
pub(crate) async fn persist_link_diff(
    &self,
    diff: &DecoratedPerspectiveDiff,
) -> Result<(), AnyError>

// Update callsites:
// - Functions returning Result: change `.await;` to `.await?;`
// - diff_from_link_language: use `.await.expect("SurrealDB link persistence failed");`
```

### 2. MIGRATION_REMOVAL_GUIDE.md (line 112-113)
**Issue**: Incomplete sentence in heading.
**Fix**: Change "Can optionally be cleaned up:" to "The following can optionally be cleaned up:"

### 3. migration.rs (lines 97-119)
**Issue**: Marks perspective as migrated even when some links failed, risking data loss.
**Fix**: Only mark as migrated when `error_count == 0`, otherwise return error.

### 4. mod.rs (lines 441-457)
**Issue**: Hardcodes `LinkStatus::Local` instead of preserving original status.
**Fix**: Read `link.status` and use it (fallback to `LinkStatus::Local` if None).

### 5. mod.rs (lines 56-66)
**Issue**: TOCTOU race in `initialize_from_db` - multiple tasks can create duplicate SurrealDB services.
**Fix**: Use atomic entry-or-init pattern with write lock to prevent duplicates.

### 6. perspective_instance.rs (lines 878-891)
**Issue**: SurrealDB lookup only uses (source, target, predicate) instead of full unique constraint.
**Fix**: Include author and timestamp in lookups to honor all 5 fields of link_unique_idx.

### 7. surreal_service/mod.rs (lines 532-576)
**Issue**: Overly broad error handling treats any "index" error as benign.
**Fix**: Only check for "unique", "duplicate", "already exists" - remove "index" check.

## Nitpicks

### 8. mod.rs (lines 80-93)
**Note**: Migration failures logged at error level but user won't see in UI.
Consider adding visible notification mechanism for production.

### 9. surreal_service/mod.rs (lines 756-904)
**Note**: DRY the repeated SurrealLink deserialization pattern across getters.
Consider extracting helper function.

---

## Execution Plan

1. Fix critical issues 1-7 first
2. Address nitpicks 8-9 if time permits
3. Run cargo fmt
4. Commit and push to `surreal-files` branch
5. Wait for CodeRabbit re-analysis
