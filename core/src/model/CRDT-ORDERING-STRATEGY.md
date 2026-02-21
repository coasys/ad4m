# CRDT Ordering Strategy for AD4M Collections

## Overview

Abstract P2P-safe ordering strategies into the `@Collection` decorator to make conflict-free list ordering automatic and transparent to developers.

**Goals:**

- ✅ Make P2P-safe ordering the default, not an afterthought
- ✅ Eliminate boilerplate across all future list-based plugins
- ✅ Provide clear migration path as algorithms improve
- ✅ Enable A/B testing of different strategies

---

## Proposed API Design

### Developer-Facing API

```typescript
@Collection({
  through: 'ad4m://has_child',
  where: { isInstance: Task },
  ordering: {
    strategy: 'linkedList',  // 'linkedList' | 'fractionalIndex' | 'timestamp' | 'manual'
    sortBy: 'taskName',      // Fallback for ties (optional)
  }
})
orderedTasks: Task[] = [];
```

### Usage - Just Like Regular Arrays

```typescript
// Developers use normal array operations
column.orderedTasks.push(newTask); // Append
column.orderedTasks.splice(2, 0, task); // Insert at index 2
column.orderedTasks.splice(1, 1); // Remove at index 1
await column.update(); // Framework generates CRDT links
```

---

## Architecture

### Layer 1: Model Decorator (API Surface)

```typescript
// In @coasys/ad4m package
interface OrderingConfig {
  strategy: 'linkedList' | 'fractionalIndex' | 'timestamp' | 'manual';
  sortBy?: string; // Property name for tiebreaker
  conflictResolution?: 'lww' | 'merge'; // Last-write-wins or merge
}

interface CollectionOptions {
  through?: string;
  where?: any;
  ordering?: OrderingConfig;
  sortBy?: string; // Deprecated in favor of ordering.sortBy
}

function Collection(options: CollectionOptions) {
  return function (target: any, propertyKey: string) {
    // Register this collection with ordering metadata
    const metadata = {
      propertyKey,
      options,
      // Store reference to ordering strategy instance
      orderingStrategy: OrderingStrategyFactory.create(options.ordering),
    };

    Reflect.defineMetadata('collection:ordered', metadata, target, propertyKey);
  };
}
```

### Layer 2: Ordering Strategy Interface

```typescript
// Strategy pattern for different ordering approaches
interface OrderingStrategy {
  // Called when array is read from perspective
  reconstruct(items: Ad4mModel[], links: Link[]): Ad4mModel[];

  // Called when array is modified
  generateLinks(items: Ad4mModel[], operation: Operation, context: OperationContext): Link[];

  // Called to clean up tombstones/old data
  garbageCollect?(links: Link[], cutoffDate: Date): Link[];
}

interface Operation {
  type: 'insert' | 'delete' | 'move' | 'reorder';
  index: number;
  item?: Ad4mModel;
  targetIndex?: number; // For moves
}

interface OperationContext {
  agentDID: string;
  timestamp: number;
  collectionId: string; // The column/parent ID
}
```

### Layer 3: Concrete Strategies

#### LinkedList Strategy (RGA-based)

```typescript
class LinkedListStrategy implements OrderingStrategy {
  private readonly predicates = {
    after: 'ad4m://ordered_after',
    positionId: 'ad4m://position_id',
    deleted: 'ad4m://position_deleted',
  };

  reconstruct(items: Ad4mModel[], links: Link[]): Ad4mModel[] {
    // Build afterMap from links
    const afterMap = new Map<string | null, Array<{ item: Ad4mModel; positionId: string }>>();

    links.forEach((link) => {
      if (link.data.predicate === this.predicates.after) {
        const item = items.find((i) => i.baseExpression === link.data.source);
        const positionIdLink = links.find(
          (l) => l.data.source === item.baseExpression && l.data.predicate === this.predicates.positionId,
        );

        if (item && !this.isDeleted(item, links)) {
          const afterId = link.data.target === 'null' ? null : link.data.target;
          afterMap.set(afterId, [
            ...(afterMap.get(afterId) || []),
            { item, positionId: positionIdLink?.data.target || '' },
          ]);
        }
      }
    });

    // Sort items with same afterId by positionId (timestamp_agentDID)
    afterMap.forEach((items) => {
      items.sort((a, b) => this.comparePositionIds(a.positionId, b.positionId));
    });

    // Traverse linked list from head (null)
    const result: Ad4mModel[] = [];
    let current: string | null = null;

    while (afterMap.has(current)) {
      const nextItems = afterMap.get(current)!;
      result.push(...nextItems.map((i) => i.item));
      current = nextItems[nextItems.length - 1].item.baseExpression;
    }

    return result;
  }

  generateLinks(items: Ad4mModel[], operation: Operation, context: OperationContext): Link[] {
    const links: Link[] = [];
    const positionId = `${context.timestamp}_${context.agentDID}`;

    switch (operation.type) {
      case 'insert': {
        const afterId = operation.index === 0 ? null : items[operation.index - 1]?.baseExpression;

        links.push(
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.after,
            target: afterId || 'null',
          }),
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.positionId,
            target: `literal://string:${positionId}`,
          }),
        );
        break;
      }

      case 'delete': {
        links.push(
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.deleted,
            target: 'literal://boolean:true',
          }),
        );
        break;
      }

      case 'move': {
        // Move = delete + insert with new position
        links.push(
          // Mark old position as deleted
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.deleted,
            target: 'literal://boolean:true',
          }),
          // Create new position
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.after,
            target: items[operation.targetIndex! - 1]?.baseExpression || 'null',
          }),
          new Link({
            source: operation.item!.baseExpression,
            predicate: this.predicates.positionId,
            target: `literal://string:${positionId}_move`,
          }),
        );
        break;
      }
    }

    return links;
  }

  private comparePositionIds(a: string, b: string): number {
    const [tsA, didA] = a.split('_');
    const [tsB, didB] = b.split('_');
    return Number(tsA) - Number(tsB) || didA.localeCompare(didB);
  }

  private isDeleted(item: Ad4mModel, links: Link[]): boolean {
    return links.some(
      (l) =>
        l.data.source === item.baseExpression &&
        l.data.predicate === this.predicates.deleted &&
        l.data.target === 'literal://boolean:true',
    );
  }
}
```

#### Fractional Index Strategy

```typescript
class FractionalIndexStrategy implements OrderingStrategy {
  private readonly predicates = {
    position: 'ad4m://fractional_position',
    positionId: 'ad4m://position_id',
  };

  reconstruct(items: Ad4mModel[], links: Link[]): Ad4mModel[] {
    // Map items to their fractional positions
    const itemPositions = items.map((item) => {
      const positionLink = links.find(
        (l) => l.data.source === item.baseExpression && l.data.predicate === this.predicates.position,
      );
      return {
        item,
        position: positionLink?.data.target || '1.0',
      };
    });

    // Sort by fractional position lexicographically
    itemPositions.sort((a, b) => {
      const posCompare = a.position.localeCompare(b.position);
      if (posCompare !== 0) return posCompare;

      // Tiebreaker: use positionId
      const idA = this.getPositionId(a.item, links);
      const idB = this.getPositionId(b.item, links);
      return this.comparePositionIds(idA, idB);
    });

    return itemPositions.map((p) => p.item);
  }

  generateLinks(items: Ad4mModel[], operation: Operation, context: OperationContext): Link[] {
    const links: Link[] = [];
    const positionId = `${context.timestamp}_${context.agentDID}`;

    if (operation.type === 'insert') {
      const position = this.calculateFractionalPosition(items[operation.index - 1], items[operation.index], context);

      links.push(
        new Link({
          source: operation.item!.baseExpression,
          predicate: this.predicates.position,
          target: `literal://string:${position}`,
        }),
        new Link({
          source: operation.item!.baseExpression,
          predicate: this.predicates.positionId,
          target: `literal://string:${positionId}`,
        }),
      );
    }

    return links;
  }

  private calculateFractionalPosition(
    before: Ad4mModel | undefined,
    after: Ad4mModel | undefined,
    context: OperationContext,
  ): string {
    // Implement fractional indexing algorithm
    // Returns string like "1.5", "1.75", etc.
    // See: https://www.figma.com/blog/realtime-editing-of-ordered-sequences/

    const beforePos = before ? this.getPosition(before) : '0';
    const afterPos = after ? this.getPosition(after) : '2';

    return this.generateKeyBetween(beforePos, afterPos, context.agentDID);
  }

  private generateKeyBetween(a: string, b: string, agentDID: string): string {
    // Simplified - real implementation more complex
    const aNum = parseFloat(a);
    const bNum = parseFloat(b);
    let mid = (aNum + bNum) / 2;

    // Add small agent-specific offset for deterministic tiebreaking
    const agentOffset = agentDID.charCodeAt(0) / 1000000;
    mid += agentOffset;

    return mid.toFixed(10);
  }

  private getPosition(item: Ad4mModel): string {
    // Would actually query links, simplified here
    return '1.0';
  }

  private getPositionId(item: Ad4mModel, links: Link[]): string {
    const link = links.find(
      (l) => l.data.source === item.baseExpression && l.data.predicate === this.predicates.positionId,
    );
    return link?.data.target || '';
  }

  private comparePositionIds(a: string, b: string): number {
    const [tsA, didA] = a.split('_');
    const [tsB, didB] = b.split('_');
    return Number(tsA) - Number(tsB) || didA.localeCompare(didB);
  }
}
```

#### Strategy Factory

```typescript
class OrderingStrategyFactory {
  static create(config?: OrderingConfig): OrderingStrategy {
    if (!config || config.strategy === 'manual') {
      return new ManualStrategy(); // Current behavior (stringified arrays)
    }

    switch (config.strategy) {
      case 'linkedList':
        return new LinkedListStrategy();
      case 'fractionalIndex':
        return new FractionalIndexStrategy();
      case 'timestamp':
        return new TimestampStrategy();
      default:
        throw new Error(`Unknown ordering strategy: ${config.strategy}`);
    }
  }
}
```

### Layer 4: Array Proxy (Magic Layer)

```typescript
// In Ad4mModel base class
class Ad4mModel {
  // ... existing code

  protected wrapCollectionWithOrdering(propertyKey: string, items: any[], orderingStrategy: OrderingStrategy): any[] {
    const self = this;

    // Create a proxy that intercepts array modifications
    return new Proxy(items, {
      get(target, prop) {
        // Intercept array methods that modify order
        if (prop === 'push' || prop === 'unshift' || prop === 'splice') {
          return function (...args: any[]) {
            const operation = self.arrayOperationToOperation(prop, args, target);

            // Store pending operation for next update()
            self._pendingOrderingOperations = self._pendingOrderingOperations || [];
            self._pendingOrderingOperations.push({
              propertyKey,
              strategy: orderingStrategy,
              operation,
            });

            // Apply to local array immediately (optimistic update)
            return Array.prototype[prop].apply(target, args);
          };
        }

        return Reflect.get(target, prop);
      },
    });
  }

  async update(batchId?: string) {
    // ... existing update logic

    // Generate and apply ordering links
    if (this._pendingOrderingOperations) {
      const context: OperationContext = {
        agentDID: await this.getAgentDID(),
        timestamp: Date.now(),
        collectionId: this.baseExpression,
      };

      for (const pending of this._pendingOrderingOperations) {
        const links = pending.strategy.generateLinks(this[pending.propertyKey], pending.operation, context);

        await this.perspective.addLinks(links, batchId);
      }

      this._pendingOrderingOperations = [];
    }

    // ... rest of existing update logic
  }

  private arrayOperationToOperation(method: string, args: any[], array: any[]): Operation {
    // Convert array method calls to Operation objects
    switch (method) {
      case 'push':
        return { type: 'insert', index: array.length, item: args[0] };
      case 'splice':
        const [index, deleteCount, ...items] = args;
        if (deleteCount > 0 && items.length === 0) {
          return { type: 'delete', index, item: array[index] };
        }
        if (items.length > 0) {
          return { type: 'insert', index, item: items[0] };
        }
      // Handle moves, etc.
      // ... more cases
    }
  }
}
```

---

## Migration Strategy

### Phase 1: Add Parallel Support

```typescript
// Old way still works
@Property({ through: 'flux://ordered_task_ids' })
orderedTaskIds: string;

// New way opt-in
@Collection({
  through: 'ad4m://has_child',
  where: { isInstance: Task },
  ordering: { strategy: 'linkedList' }
})
orderedTasks: Task[] = [];
```

### Phase 2: Migration Helper

```typescript
async migrateToOrderedCollection() {
  const oldIds = JSON.parse(this.orderedTaskIds);

  // Generate links for existing order
  const batchId = await this.perspective.createBatch();
  let previousId = null;

  for (const taskId of oldIds) {
    await this.perspective.addLink({
      source: taskId,
      predicate: 'ad4m://ordered_after',
      target: previousId || 'null'
    }, batchId);

    await this.perspective.addLink({
      source: taskId,
      predicate: 'ad4m://position_id',
      target: `literal://string:${Date.now()}_migration`
    }, batchId);

    previousId = taskId;
  }

  await this.perspective.commitBatch(batchId);

  // Remove old property
  this.orderedTaskIds = '[]';
  await this.update();
}
```

---

## Usage Examples

### Kanban Column with Automatic Ordering

```typescript
@ModelOptions({ name: 'TaskColumn' })
export default class TaskColumn extends Ad4mModel {
  @Property({ through: 'flux://column_name' })
  columnName: string;

  @Collection({
    through: 'ad4m://has_child',
    where: { isInstance: Task },
    ordering: {
      strategy: 'linkedList',
      sortBy: 'taskName', // Fallback for visual stability
    },
  })
  tasks: Task[] = [];
}
```

### In Component - Just Use Normal Array Methods

```typescript
async function moveTask(task: Task, fromColumn: TaskColumn, toColumn: TaskColumn, index: number) {
  // Remove from old column
  const oldIndex = fromColumn.tasks.findIndex((t) => t.baseExpression === task.baseExpression);
  fromColumn.tasks.splice(oldIndex, 1);

  // Add to new column at specific position
  toColumn.tasks.splice(index, 0, task);

  // Save both (framework generates all the CRDT links)
  await Promise.all([fromColumn.update(), toColumn.update()]);
}
```

---

## Strategy Comparison

### RGA (LinkedList Strategy)

**Best for:**

- Simple implementation needs
- Storage/bandwidth is a concern
- Team wants to understand the code easily
- Lists with < 50 items typically

**Pros:**

- ✅ Simple 1:1 mapping (one link per list element)
- ✅ Easy to query and debug
- ✅ Natural for AD4M's triple store
- ✅ Can leverage AD4M's built-in conflict resolution

**Cons:**

- ❌ O(n) insert/delete operations
- ❌ Need to reconstruct full list on every read
- ❌ Occasional "weird" ordering when conflicts happen

**Storage per item:** ~2 links = ~200 bytes

### YATA Strategy

**Best for:**

- Heavy concurrent editing
- Need best-possible conflict resolution
- Undo/redo requirements
- Complex editing workflows

**Pros:**

- ✅ O(1) amortized operations
- ✅ Better semantic preservation for complex edits
- ✅ Sophisticated conflict resolution

**Cons:**

- ❌ 3× more links per item
- ❌ More complex to implement correctly
- ❌ Harder to debug
- ❌ Need to maintain invariants across multiple links

**Storage per item:** ~5 links = ~500 bytes

### Fractional Indexing

**Best for:**

- Simple implementation with good performance
- When you want deterministic ordering
- Lists that don't change structure often

**Pros:**

- ✅ Simple to understand
- ✅ Good performance
- ✅ Lexicographic ordering is intuitive
- ✅ Can rebalance if positions get too long

**Cons:**

- ❌ Position strings can grow over time
- ❌ Needs occasional rebalancing
- ❌ Less robust than full CRDTs for some scenarios

**Storage per item:** ~2 links = ~200 bytes

---

## Performance Characteristics

| Operation               | RGA       | YATA      | Fractional Index |
| ----------------------- | --------- | --------- | ---------------- |
| **Insert**              | O(n)      | O(1)\*    | O(1)             |
| **Delete**              | O(n)      | O(1)      | O(n)             |
| **Move**                | O(n) × 2  | O(1) × 2  | O(1)             |
| **Lookup by index**     | O(n)      | O(n)      | O(n log n)       |
| **Memory per item**     | ~64 bytes | ~96 bytes | ~64 bytes        |
| **Links per item**      | 2         | 5         | 2                |
| **Conflict resolution** | Good      | Excellent | Good             |

\*Amortized

---

## Benefits of This Approach

1. **Declarative**: Just add a flag, framework handles complexity
2. **Type-safe**: Still get `Task[]` with full TypeScript support
3. **Backwards compatible**: Old code works, migrate at your pace
4. **Testable**: Each strategy is isolated and unit-testable
5. **Extensible**: Add new strategies without touching existing code
6. **Optimized**: Framework can batch link operations
7. **Debuggable**: Clear separation between strategy and application logic

---

## Open Questions

### 1. How to handle bulk operations?

Example: `tasks = newTasksArray`

**Solution:** Treat as replace operation, diff old vs new arrays and generate appropriate operations.

### 2. Should we support custom strategies?

**Yes**, via plugin system:

```typescript
ordering: {
  strategy: new MyCustomStrategy();
}
```

### 3. How to visualize conflicts in dev tools?

Build AD4M inspector extension showing link graph visualization.

### 4. Garbage collection cadence?

Run on perspective sync with configurable threshold (e.g., tombstones older than 30 days).

### 5. Should ordering be lazy or eager?

Lazy reconstruction on read, cache result, invalidate on link changes.

---

## Implementation Phases

### Phase 1: Core Infrastructure (2 weeks)

- [ ] Add `OrderingStrategy` interface to AD4M
- [ ] Implement `LinkedListStrategy` (RGA)
- [ ] Add ordering support to `@Collection` decorator
- [ ] Create array proxy layer
- [ ] Write unit tests for strategy

### Phase 2: Integration (1 week)

- [ ] Update `Ad4mModel.update()` to handle ordering operations
- [ ] Add batch operation support
- [ ] Implement garbage collection

### Phase 3: Additional Strategies (1 week)

- [ ] Implement `FractionalIndexStrategy`
- [ ] Implement `TimestampStrategy`
- [ ] Write integration tests

### Phase 4: Migration Tools (1 week)

- [ ] Create migration helper utilities
- [ ] Write migration guide
- [ ] Add migration tests

### Phase 5: Developer Experience (1 week)

- [ ] Add TypeScript types and documentation
- [ ] Create example projects
- [ ] Build debugging tools/inspector

---

## Success Metrics

1. **Correctness**: Zero data loss in concurrent editing scenarios
2. **Performance**: < 10ms overhead for typical operations (< 100 items)
3. **Storage**: < 1MB per 1000 items with ordering metadata
4. **Developer Experience**: Migration requires < 10 lines of code change
5. **Adoption**: 80% of new list-based plugins use ordered collections

---

## References

- [Figma's Fractional Indexing](https://www.figma.com/blog/realtime-editing-of-ordered-sequences/)
- [CRDT Survey Paper](https://hal.inria.fr/hal-00932836/document)
- [RGA Algorithm](https://pages.lip6.fr/Marc.Shapiro/papers/RGA-TPDS-2011.pdf)
- [Yjs Documentation](https://docs.yjs.dev/)
- [Automerge CRDT](https://automerge.org/)
