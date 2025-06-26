# Filtered Prolog Engine Pools for Subscription Optimization

## Overview

This implementation introduces **filtered Prolog engine pools** to optimize subscription query performance in AD4M. When perspectives contain large amounts of data, running subscription queries against the complete dataset becomes a performance bottleneck. This solution creates dedicated, smaller engine pools containing only data reachable from specific source nodes.

## Recent Improvements

### 1. Efficient Engine Reuse
- **Before**: Created temporary engines for filtering operations (expensive)
- **After**: Reuses existing engines from the complete pool for filtering operations
- **Benefit**: Eliminates the overhead of spawning temporary engines

### 2. Specific Source Filter Detection
- **Primary Pattern**: Optimized for Ad4mModel's `triple("source", "ad4m://has_child", Base)` pattern
- **Fallback Patterns**: Still supports general triple, link, and reachable patterns
- **Smart Detection**: Only extracts source filters from literal values (not variables)

## Problem Statement

### Before
- All Prolog engines in a perspective pool contained ALL perspective data
- Subscription queries (checked every 200ms) ran against the complete dataset
- Performance degraded significantly as perspective data grew
- Memory usage scaled with total perspective size rather than relevant data

### After
- **Complete pools**: Contain all perspective data (for regular queries)
- **Filtered pools**: Contain only data reachable from specific source nodes (for subscriptions)
- **Smart routing**: Automatically routes subscription queries to appropriate filtered pools
- **Performance**: Subscription queries run much faster on smaller, relevant datasets

## Architecture

```
PrologService
├── Perspective Pools (Complete)
│   ├── Engine 1 (all data)
│   ├── Engine 2 (all data)
│   └── ...
│   └── Filtered Sub-pools
│       ├── "user123" Pool
│       │   ├── Engine 1 (user123 reachable data)
│       │   └── Engine 2 (user123 reachable data)
│       └── "root_node" Pool
│           ├── Engine 1 (root_node reachable data)
│           └── Engine 2 (root_node reachable data)
```

## Key Components

### 1. Engine Pool Types
```rust
#[derive(Clone, Debug)]
pub enum EnginePoolType {
    Complete,                    // Contains all perspective data
    FilteredBySource(String),    // Contains only data reachable from source
}
```

### 2. Smart Query Routing
```rust
pub async fn run_query_smart(&self, query: String, is_subscription: bool) -> Result<QueryResult, Error>
```

- **Regular queries**: Always use complete pools
- **Subscription queries**: Use filtered pools if source filter detected
- **Fallback**: Use complete pools if filtering fails

### 3. Enhanced Source Filter Detection
```rust
pub fn extract_source_filter(query: &str) -> Option<String>
```

**Priority Order:**
1. **Ad4mModel Pattern**: `triple("user123", "ad4m://has_child", Base)` → "user123"
2. **General Patterns**: `triple("source", _, _)`, `link("source", _, _, _, _)`, `reachable("source", _)`
3. **Variable Filtering**: Ignores variables starting with uppercase or underscore

### 4. Efficient Reachable Data Filtering
```rust
async fn get_filtered_facts_for_source(&self, source_filter: &str, all_facts: &[String]) -> Result<Vec<String>, Error>
```

- **Reuses existing engines** instead of spawning temporary ones
- Uses the existing `reachable(A,B)` predicate to determine relevant data
- **Optimized data flow**: Complete pools filter data once, filtered pools receive pre-computed subsets

## Usage

### Automatic (Recommended)
The system automatically optimizes subscription queries:

```rust
// This will automatically use a filtered pool if source is detected
let result = perspective.prolog_query_subscription(
    r#"triple("user123", "ad4m://has_child", Base)"#  // Primary Ad4mModel pattern
).await?;

// Also works with general patterns
let result = perspective.prolog_query_subscription(
    r#"triple("user123", "likes", Target)"#  // General fallback pattern
).await?;
```

### Manual Service-Level
```rust
let service = get_prolog_service().await;

// Regular query (uses complete pool)
let result = service.run_query(perspective_id, query).await?;

// Subscription query (uses filtered pool if possible)
let result = service.run_query_subscription(perspective_id, query).await?;
```

## Performance Benefits

### Memory Efficiency
- **Before**: Each engine loads all N facts
- **After**: Filtered engines load only relevant subset (typically << N facts)

### Query Performance
- **Before**: O(N) scan through all facts for each subscription check
- **After**: O(M) scan where M is the reachable subset size

### Engine Efficiency
- **Before**: Spawned temporary engines for filtering operations
- **After**: Reuses existing engines, eliminating spawn overhead

### Scalability
- **Before**: Performance degrades linearly with total perspective size
- **After**: Performance scales with relevant data size per subscription

## Example Scenarios

### Scenario 1: Ad4mModel Optimization
```prolog
% Query: triple("user123", "ad4m://has_child", Base)
% Optimized pattern recognition creates filtered pool for "user123"

% Complete pool contains all data:
triple("user123", "ad4m://has_child", "post1").
triple("user123", "likes", "post2").
triple("user456", "ad4m://has_child", "post3").
triple("post1", "content", "Hello world").

% Filtered pool for "user123" contains only:
triple("user123", "ad4m://has_child", "post1").
triple("user123", "likes", "post2").
triple("post1", "content", "Hello world").
```

### Scenario 2: Hierarchical Data
```prolog
% Query: reachable("root", X)
% Only loads data reachable from "root" node
triple("root", "contains", "folder1").
triple("folder1", "contains", "file1").
triple("file1", "type", "document").

% Excludes unrelated data:
% triple("other_root", "contains", "other_file").
```

## Implementation Details

### Optimized Data Loading Process
1. **Complete Pool Update**: All engines receive all facts
2. **Efficient Filtering**: Reuses existing complete pool engine to run reachable queries
3. **Filtered Pool Update**: Receives pre-computed filtered facts directly
4. **No Engine Spawning**: Eliminates expensive temporary engine creation

### Query Routing Logic
1. Check if query is a subscription (`is_subscription: true`)
2. **Priority 1**: Check for Ad4mModel pattern `triple("source", "ad4m://has_child", Base)`
3. **Priority 2**: Check for general patterns with literal sources
4. Route to filtered pool if source found, otherwise use complete pool

### Error Handling
- If filtered pool creation fails, fallback to complete pool
- Engine failures handled at individual engine level
- Pool recovery on next update cycle
- Graceful degradation ensures system stability

## Testing

Comprehensive tests verify:
- ✅ Ad4mModel-specific source filter extraction
- ✅ General pattern source filter extraction as fallback
- ✅ Variable filtering (no extraction from variables)
- ✅ Smart routing behavior for subscription vs. regular queries
- ✅ Engine reuse efficiency
- ✅ Filtered pool creation and management

## Migration

This is a **backward-compatible** addition:
- ✅ Existing code continues to work unchanged
- ✅ Performance improvements are automatic for subscription queries
- ✅ No breaking changes to public APIs
- ✅ Zero configuration required

## Performance Monitoring

Monitor these metrics:
- Filtered pool creation rate and efficiency
- Subscription query response times (should improve significantly)
- Memory usage per pool type
- Engine reuse vs. temporary engine creation ratio
- Source filter detection accuracy

## Future Enhancements

Potential improvements:
1. **Multiple source filtering**: Support queries with multiple source constraints
2. **Predicate-based filtering**: Filter by predicate patterns in addition to source
3. **Adaptive pool sizing**: Dynamically adjust pool sizes based on usage patterns
4. **Query result caching**: Cache frequent subscription query results
5. **Background precomputation**: Pre-filter common query patterns
6. **Advanced pattern detection**: Support more complex Ad4mModel query patterns 