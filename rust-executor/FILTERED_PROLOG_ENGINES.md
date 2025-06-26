# Filtered Prolog Engine Pools for Subscription Optimization

## Overview

This implementation introduces **filtered Prolog engine pools** to optimize subscription query performance in AD4M. When perspectives contain large amounts of data, running subscription queries against the complete dataset becomes a performance bottleneck. This solution creates dedicated, smaller engine pools containing only data reachable from specific source nodes.

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

### 3. Source Filter Detection
```rust
pub fn extract_source_filter(query: &str) -> Option<String>
```

Detects patterns like:
- `triple("user123", _, _)` → source filter: "user123"
- `link("root_node", _, _, _, _)` → source filter: "root_node"  
- `reachable("item1", _)` → source filter: "item1"

### 4. Reachable Data Filtering
```rust
async fn get_filtered_facts_for_source(&self, source_filter: &str, all_facts: &[String]) -> Result<Vec<String>, Error>
```

Uses the existing `reachable(A,B)` predicate to determine what data should be included in filtered engines.

## Usage

### Automatic (Recommended)
The system automatically optimizes subscription queries:

```rust
// This will automatically use a filtered pool if source is detected
let result = perspective.prolog_query_subscription(
    r#"triple("user123", "likes", Target)"#
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

### Scalability
- **Before**: Performance degrades linearly with total perspective size
- **After**: Performance scales with relevant data size per subscription

## Example Scenarios

### Scenario 1: User Activity Streams
```prolog
% Complete pool contains:
triple("user1", "posted", "message1").
triple("user1", "likes", "message2").
triple("user2", "posted", "message3").
triple("user2", "likes", "message4").
triple("message1", "content", "Hello world").
triple("message2", "content", "Great post!").

% Filtered pool for "user1" contains only:
triple("user1", "posted", "message1").
triple("user1", "likes", "message2").
triple("message1", "content", "Hello world").
triple("message2", "content", "Great post!").
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

## Configuration

### Pool Sizes
- **Complete pools**: 10 engines (default)
- **Filtered pools**: 3 engines (sufficient for subscriptions)

### Query Patterns Supported
- `triple("literal_source", _, _)`
- `link("literal_source", _, _, _, _)`
- `reachable("literal_source", _)`

Variables (uppercase or underscore-prefixed) are not filtered.

## Implementation Details

### Data Loading Process
1. **Complete Pool Update**: All engines receive all facts
2. **Filtered Pool Update**: 
   - Run `reachable("source", Target)` query on complete pool
   - Filter facts to include only those involving reachable nodes
   - Load filtered facts into filtered pool engines

### Query Routing Logic
1. Check if query is a subscription (`is_subscription: true`)
2. Extract source filter from query text
3. If source found, route to filtered pool
4. Otherwise, use complete pool

### Error Handling
- If filtered pool creation fails, fallback to complete pool
- Engine failures handled at individual engine level
- Pool recovery on next update cycle

## Testing

Comprehensive tests cover:
- Source filter extraction accuracy
- Filtered pool creation and data subset verification  
- Smart routing behavior for subscription vs. regular queries
- Performance characteristics with large datasets

## Migration

This is a **backward-compatible** addition:
- Existing code continues to work unchanged
- Performance improvements are automatic for subscription queries
- No breaking changes to public APIs

## Performance Monitoring

Monitor these metrics:
- Filtered pool creation rate
- Subscription query response times
- Memory usage per pool type
- Cache hit rates for filtered queries

## Future Enhancements

Potential improvements:
1. **Multiple source filtering**: Support queries with multiple source constraints
2. **Predicate-based filtering**: Filter by predicate patterns in addition to source
3. **Adaptive pool sizing**: Dynamically adjust pool sizes based on usage
4. **Query result caching**: Cache frequent subscription query results
5. **Background precomputation**: Pre-filter common query patterns 