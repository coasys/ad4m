# AD4M Decorator System Redesign

## Analysis of Current Decorator System

### Current Issues

#### 1. Confusing Naming

```typescript
@Optional({ required: true })  // Wait, what? 🤔
```

This is semantically contradictory. `@Optional` should mean optional, not "flexible configuration."

#### 2. Redundant Decorators

You have three property decorators doing essentially the same thing with different defaults:

- `@Property` = `@Optional({ required: true, writable: true })`
- `@ReadOnly` = `@Optional({ writable: false })`
- `@Optional` = the actual flexible one

#### 3. Collections Are Primitive

```typescript
@Collection({ through: "recipe://comment" })
comments: string[] = [];  // Just IDs, have to hydrate manually
```

No type safety, no auto-hydration, no relationship semantics.

#### 4. Flag Is Confusing

Documentation says "discouraged unless you need type discrimination" - then why is it a first-class decorator? Should be internal or better named.

#### 5. No Include/Eager Loading

```typescript
// Current: Manual hydration required
const channel = await Channel.findOne(perspective);
const comments = await Promise.all(
  channel.comments.map((id) =>
    Comment.findOne(perspective, { where: { base: id } }),
  ),
);

// Desired:
const channel = await Channel.findOne(perspective, {
  include: [{ relation: "comments" }],
});
// channel.comments is now Comment[] not string[]
```

---

## Proposed Improved System

### 1. Consolidate Property Decorators

**Replace `@Property`, `@Optional`, `@ReadOnly` with single `@Field`:**

```typescript
interface FieldOptions {
  through: string;
  required?: boolean;        // Default: false
  writable?: boolean;        // Default: true
  resolveLanguage?: string;
  initial?: string;
  getter?: string;
  setter?: string;
  local?: boolean;
  transform?: (value: any) => any;
}

@Field(options: FieldOptions)
```

**Examples:**

```typescript
class Recipe extends Ad4mModel {
  // Required field (old @Property)
  @Field({
    through: "recipe://name",
    required: true,
    resolveLanguage: "literal",
  })
  name: string;

  // Optional field (old @Optional)
  @Field({
    through: "recipe://description",
    resolveLanguage: "literal",
  })
  description?: string;

  // Read-only computed field (old @ReadOnly)
  @Field({
    through: "recipe://rating",
    writable: false,
    getter: "...",
  })
  averageRating: number;
}
```

**Alternative names:** `@Attribute`, `@Property`, `@Column` (but keep just one)

---

### 2. Replace `@Collection` with Relationship Decorators

**Use relationship semantics that match the graph:**

```typescript
// One-to-many: Parent has multiple children
@HasMany(() => ModelClass, options?: RelationOptions)

// Many-to-one: Child belongs to parent
@BelongsTo(() => ModelClass, options?: RelationOptions)

// One-to-one: Unique relationship
@HasOne(() => ModelClass, options?: RelationOptions)

// Many-to-many: Both sides can have multiple
@ManyToMany(() => ModelClass, options?: RelationOptions)
```

**RelationOptions:**

```typescript
interface RelationOptions {
  through: string; // Predicate for the relationship
  foreignKey?: string; // For @BelongsTo
  as?: string; // Alias for the relation
  where?: Where; // Filter related items
  order?: Order; // Order related items (with CRDT strategy!)
  ordering?: OrderingConfig; // Your new CRDT ordering! 🎉
  cascade?: boolean; // Delete related on parent delete
  local?: boolean; // Local-only relationship
}
```

**Examples:**

```typescript
@ModelOptions({ name: "Channel" })
class Channel extends Ad4mModel {
  @Field({ through: "flux://name", required: true })
  name: string;

  // HasMany with typed instances
  @HasMany(() => Message, {
    through: "ad4m://has_child",
    where: { type: "flux://has_message" },
    ordering: { strategy: "linkedList" }, // Built-in CRDT! 🔥
  })
  messages: Message[] = [];

  // HasMany with filters
  @HasMany(() => Comment, {
    through: "ad4m://has_child",
    where: { isInstance: Comment },
    order: { timestamp: "DESC" },
  })
  comments: Comment[] = [];

  // HasMany with alias
  @HasMany(() => User, {
    through: "flux://has_participant",
    as: "members",
  })
  members: User[] = [];
}

@ModelOptions({ name: "Message" })
class Message extends Ad4mModel {
  @Field({ through: "flux://body", required: true })
  body: string;

  // BelongsTo establishes the inverse
  @BelongsTo(() => Channel, {
    through: "ad4m://has_child",
    foreignKey: "channelId", // Optional: store parent ID locally
  })
  channel?: Channel;
}
```

---

### 3. Enhance Query Type with Includes

```typescript
export type Query<T = any> = {
  source?: string;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;

  // New: Nested includes (like Sequelize)
  include?: Include<T>[];

  // Deprecated: Use include instead
  properties?: string[];
  collections?: string[];
};

export type Include<T = any> = {
  // Relation name to include
  relation: keyof T;

  // Optional: Nested query for the relation
  where?: Where;
  order?: Order;
  limit?: number;

  // Recursive: Include relations of relations
  include?: Include[];

  // Optional: Alias for the included data
  as?: string;

  // Optional: Only load specific fields
  select?: string[];
};
```

**Usage Examples:**

```typescript
// Simple include
const channels = await Channel.query(perspective)
  .include([{ relation: "messages" }])
  .run();
// channels[0].messages is Message[] not string[]

// Include with filtering
const channels = await Channel.query(perspective)
  .include([
    {
      relation: "messages",
      where: { author: "did:key:xyz" },
      order: { timestamp: "DESC" },
      limit: 10,
    },
  ])
  .run();

// Nested includes (like Sequelize)
const channels = await Channel.query(perspective)
  .include([
    {
      relation: "messages",
      include: [
        {
          relation: "author", // Message belongs to User
          select: ["name", "avatar"],
        },
      ],
    },
  ])
  .run();
// channels[0].messages[0].author.name

// Multiple includes
const recipe = await Recipe.query(perspective)
  .include([
    { relation: "ingredients" },
    { relation: "comments", include: [{ relation: "author" }] },
    { relation: "reviews", where: { rating: { gt: 4 } } },
  ])
  .findOne();
```

---

### 4. Replace or Rename `@Flag`

**Option A: Rename to `@Discriminator`**

```typescript
@Discriminator({
  through: "ad4m://type",
  value: "flux://message"
})
type: string;
```

Makes it clear this is for polymorphic type discrimination.

**Option B: Merge into `@Field`**

```typescript
@Field({
  through: "ad4m://type",
  initial: "flux://message",
  writable: false,
  discriminator: true  // New flag
})
type: string;
```

**Option C: Remove entirely**
Use regular fields with validation in the model constructor.

**Recommendation:** Option A - rename to `@Discriminator` and keep it for clear intent.

---

### 5. Auto-Detect Inverse Relations

```typescript
// When you define a BelongsTo, auto-create the HasMany on the other side
@ModelOptions({ name: "Message" })
class Message extends Ad4mModel {
  @BelongsTo(() => Channel, {
    through: "ad4m://has_child",
    inverse: "messages", // Tells Channel to auto-create HasMany
  })
  channel: Channel;
}

// Now Channel automatically has:
// @HasMany(() => Message, { through: "ad4m://has_child" })
// messages: Message[]
```

This reduces boilerplate and keeps relationships in sync.

---

### 6. Add Lifecycle Hooks

```typescript
@ModelOptions({ name: "Recipe" })
class Recipe extends Ad4mModel {
  @Field({ through: "recipe://name", required: true })
  name: string;

  @HasMany(() => Ingredient, { through: "recipe://ingredient" })
  ingredients: Ingredient[];

  // Hooks
  async beforeSave() {
    // Validate, transform, etc.
    if (!this.name) throw new Error("Name required");
  }

  async afterLoad() {
    // Post-process after loading from perspective
    this.name = this.name.toUpperCase();
  }

  async beforeDelete() {
    // Cascade delete ingredients
    await Promise.all(this.ingredients.map((i) => i.delete()));
  }
}
```

---

## Recommended Final API

```typescript
// 1. Single property decorator
@Field({
  through: string;
  required?: boolean;
  writable?: boolean;
  resolveLanguage?: string;
  initial?: string;
  getter?: string;
  setter?: string;
  local?: boolean;
  transform?: (value: any) => any;
})

// 2. Relationship decorators
@HasMany(() => Model, {
  through: string;
  where?: Where;
  order?: Order;
  ordering?: { strategy: 'linkedList' | 'fractionalIndex' };
  cascade?: boolean;
  local?: boolean;
})

@BelongsTo(() => Model, {
  through: string;
  foreignKey?: string;
  inverse?: string;
})

@HasOne(() => Model, {
  through: string;
  where?: Where;
})

@ManyToMany(() => Model, {
  through: string;
  joinPredicate?: string;
})

// 3. Type discriminator
@Discriminator({
  through: string;
  value: string;
})

// 4. Enhanced Query type
export type Query<T = any> = {
  source?: string;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  include?: Include<T>[];
};

export type Include<T = any> = {
  relation: keyof T;
  where?: Where;
  order?: Order;
  limit?: number;
  include?: Include[];
  select?: string[];
};
```

---

## Complete Example: Before and After

### Before (Current System)

```typescript
@ModelOptions({ name: "Channel" })
export class Channel extends Ad4mModel {
  @Property({
    through: "flux://name",
    writable: true,
    resolveLanguage: "literal",
  })
  name: string;

  @Optional({
    through: "flux://description",
    writable: true,
    resolveLanguage: "literal",
  })
  description: string;

  @Collection({
    through: "ad4m://has_child",
    where: { isInstance: Message },
  })
  messages: string[] = []; // Just IDs!

  @Collection({
    through: "flux://has_participant",
  })
  participants: string[] = []; // Just IDs!
}

// Usage: Manual hydration hell
const channel = await Channel.findOne(perspective);
const messages = await Promise.all(
  channel.messages.map((id) =>
    Message.findOne(perspective, { where: { base: id } }),
  ),
);
const participants = await Promise.all(
  channel.participants.map((id) =>
    User.findOne(perspective, { where: { base: id } }),
  ),
);
```

### After (Proposed System)

```typescript
@ModelOptions({ name: "Channel" })
export class Channel extends Ad4mModel {
  @Field({
    through: "flux://name",
    required: true,
    resolveLanguage: "literal",
  })
  name: string;

  @Field({
    through: "flux://description",
    resolveLanguage: "literal",
  })
  description?: string;

  @HasMany(() => Message, {
    through: "ad4m://has_child",
    where: { type: "flux://has_message" },
    ordering: { strategy: "linkedList" },
  })
  messages: Message[] = []; // Typed instances!

  @HasMany(() => User, {
    through: "flux://has_participant",
    as: "members",
  })
  members: User[] = []; // Typed instances!
}

// Usage: Automatic eager loading
const channel = await Channel.query(perspective)
  .include([{ relation: "messages", limit: 10 }, { relation: "members" }])
  .findOne();

// channel.messages is already Message[] with full data
// channel.members is already User[] with full data
console.log(channel.messages[0].body);
console.log(channel.members[0].name);

// Nested includes
const channel = await Channel.query(perspective)
  .include([
    {
      relation: "messages",
      include: [{ relation: "author" }], // Message @BelongsTo User
    },
  ])
  .findOne();

console.log(channel.messages[0].author.name);
```

---

## Migration Strategy

### Phase 1: Add new decorators alongside old ones

```typescript
// Old way still works
@Property({ through: "recipe://name" })
name: string;

// New way available
@Field({ through: "recipe://name", required: true })
name: string;
```

### Phase 2: Add deprecation warnings

```typescript
@Property() // Warning: @Property is deprecated, use @Field with required: true
```

### Phase 3: Provide codemod script

```bash
npx @coasys/ad4m-migrate decorators ./src
```

**Codemod transformations:**

- `@Property(opts)` → `@Field({ ...opts, required: true })`
- `@Optional(opts)` → `@Field(opts)`
- `@ReadOnly(opts)` → `@Field({ ...opts, writable: false })`
- `@Collection(opts)` → `@HasMany(() => String, opts)` (or appropriate type)
- `@Flag(opts)` → `@Discriminator(opts)`

### Phase 4: Remove old decorators in major version

---

## Implementation Phases

### Phase 1: Core Relationship Infrastructure (3-4 weeks)

**Week 1-2: Add relationship decorators**

- [ ] Create `@HasMany`, `@BelongsTo`, `@HasOne`, `@ManyToMany` decorators
- [ ] Update metadata extraction to handle relationship types
- [ ] Modify model constructor to set up relationship proxies
- [ ] Write unit tests for each relationship type

**Week 3: Implement basic eager loading**

- [ ] Add `include` parameter to Query type
- [ ] Implement `Include<T>` type with proper TypeScript generics
- [ ] Create relationship resolver that fetches related models
- [ ] Handle circular references (max depth protection)

**Week 4: Nested includes**

- [ ] Implement recursive include resolution
- [ ] Add filtering/ordering to included relations
- [ ] Write integration tests for complex queries

### Phase 2: Field Consolidation (1-2 weeks)

**Week 1: Create `@Field` decorator**

- [ ] Implement unified `@Field` decorator with all options
- [ ] Ensure backwards compatibility with existing code
- [ ] Add migration helper functions

**Week 2: Deprecation path**

- [ ] Add deprecation warnings to old decorators
- [ ] Update all examples in documentation
- [ ] Create codemod script for automated migration

### Phase 3: CRDT Integration (2-3 weeks)

**Week 1-2: Integrate ordering strategies**

- [ ] Add `ordering` option to `@HasMany`
- [ ] Connect to LinkedList/FractionalIndex strategies from CRDT plan
- [ ] Handle ordering in relationship resolution

**Week 3: Testing & optimization**

- [ ] Test concurrent updates with different strategies
- [ ] Optimize query performance for ordered collections
- [ ] Add monitoring/debugging tools

### Phase 4: Advanced Features (2 weeks)

**Week 1: Lifecycle hooks**

- [ ] Implement `beforeSave`, `afterLoad`, `beforeDelete` hooks
- [ ] Add hook execution to model lifecycle methods
- [ ] Write examples and tests

**Week 2: Auto-inverse relations**

- [ ] Detect `inverse` option in `@BelongsTo`
- [ ] Automatically register inverse `@HasMany` on related model
- [ ] Handle bidirectional sync

### Phase 5: Documentation & Migration (1-2 weeks)

**Week 1: Complete documentation**

- [ ] Full API reference for new decorators
- [ ] Migration guide from old to new system
- [ ] Example projects showcasing best practices
- [ ] Video tutorials for complex scenarios

**Week 2: Community testing**

- [ ] Beta release with new decorators
- [ ] Gather feedback from early adopters
- [ ] Fix bugs and edge cases
- [ ] Performance tuning based on real-world usage

---

## Benefits Summary

### Developer Experience

1. **Clearer semantics**: `@Field` with `required` is unambiguous
2. **Type safety**: `messages: Message[]` instead of `string[]`
3. **Less boilerplate**: Auto-inverse relations, eager loading
4. **Better IntelliSense**: Proper TypeScript types for relations
5. **Familiar patterns**: API similar to Sequelize/TypeORM/Prisma

### Technical Benefits

1. **CRDT integration**: Ordering strategies built into relationships
2. **Graph-native**: Relationships map to graph predicates naturally
3. **Performance**: Batch loading, query optimization
4. **Future-proof**: Lifecycle hooks, validation, transforms
5. **Maintainable**: Clear separation of concerns

### Migration Path

1. **Non-breaking**: Old decorators work during transition
2. **Gradual**: Migrate one model at a time
3. **Automated**: Codemod handles 90% of changes
4. **Well-documented**: Step-by-step migration guide

---

## Open Questions & Design Decisions

### 1. Should `@Field` be the name or keep `@Property`?

**Options:**

- **Pro `@Field`**: Clear break from old API, aligns with Prisma
- **Pro `@Property`**: Familiar to existing users, less change

**Recommendation:** Use `@Field` for clean break and better semantics.

### 2. Eager vs lazy loading default?

**Options:**

- **Eager**: Include loads related by default
- **Lazy**: Only load related when explicitly included

**Recommendation:** Lazy by default for P2P bandwidth efficiency. Explicit includes when needed.

### 3. Circular dependencies in includes?

**Problem:** `Channel` includes `Messages`, `Message` includes `Channel` → infinite loop

**Solutions:**

- Detect cycles and throw error
- Max depth parameter (default: 3 levels)
- Allow with warning

**Recommendation:** Max depth of 2 by default, configurable via `maxIncludeDepth` option.

### 4. Caching strategy for included relations?

**Options:**

- Cache hydrated models in perspective
- Cache per query
- No caching (always fresh)

**Recommendation:** Per-query cache with perspective-level invalidation on link changes.

### 5. Should ordering be required for HasMany?

**Problem:** Without ordering strategy, concurrent edits cause issues

**Options:**

- Force developers to choose a strategy
- Default to `timestamp` with warning
- Default to `linkedList`

**Recommendation:** Default to `timestamp` ordering, show one-time warning suggesting explicit strategy for concurrent-write scenarios.

### 6. Type inference for @HasMany?

**Problem:** `@HasMany(() => Message)` - why the function?

**Reason:** Avoids circular dependency issues in TypeScript (Message might reference Channel, Channel references Message).

**Alternative:** Use string literals: `@HasMany('Message')` - but loses type safety.

**Recommendation:** Keep function syntax for type safety, explain in docs.

### 7. How to handle polymorphic relationships?

**Example:** Comments can belong to Posts OR Recipes

```typescript
@ModelOptions({ name: "Comment" })
class Comment extends Ad4mModel {
  @BelongsTo(() => Ad4mModel, {
    // Generic parent
    through: "ad4m://has_child",
    polymorphic: true,
    discriminator: "parentType",
  })
  parent: Post | Recipe;

  @Field({ through: "comment://parent_type" })
  parentType: string; // 'Post' or 'Recipe'
}
```

**Recommendation:** Add `polymorphic` option with discriminator field in Phase 4.

---

## Risk Assessment

### High Risk

- **Breaking changes**: Existing models need migration
  - _Mitigation_: Maintain old decorators for 2 major versions
- **Performance regression**: Eager loading might be slow
  - _Mitigation_: Extensive benchmarking, query optimization
- **Circular reference bugs**: Include cycles could crash
  - _Mitigation_: Max depth limits, cycle detection

### Medium Risk

- **TypeScript complexity**: Generic types might confuse users
  - _Mitigation_: Clear documentation, examples
- **CRDT integration bugs**: Ordering strategies are complex
  - _Mitigation_: Thorough testing, phased rollout

### Low Risk

- **Migration effort**: Developers need to learn new API
  - _Mitigation_: Codemod, migration guide, video tutorials

---

## Success Metrics

### Adoption

- **Target**: 80% of new models use new decorators within 3 months
- **Measure**: Track decorator usage in published packages

### Performance

- **Target**: < 10ms overhead for includes vs manual hydration
- **Measure**: Benchmark suite comparing old vs new

### Developer Satisfaction

- **Target**: 4.5/5 stars in developer feedback
- **Measure**: Survey after 1 month of usage

### Bug Rate

- **Target**: < 5 critical bugs in first 3 months
- **Measure**: GitHub issues tracking

---

## Conclusion

The proposed redesign brings AD4M's model layer up to par with modern ORMs (Sequelize, Prisma, TypeORM) while preserving its unique P2P/graph capabilities. The relationship-first approach aligns with the graph data model, and eager loading via includes dramatically improves developer experience.

Key improvements:

- ✅ **Clearer API**: `@Field` replaces confusing property decorators
- ✅ **Type safety**: Relationships return typed model instances
- ✅ **Better DX**: Includes work like Sequelize/Prisma
- ✅ **CRDT-ready**: Built-in ordering strategies
- ✅ **Graph-native**: Relationships map naturally to predicates
- ✅ **Future-proof**: Hooks, validation, extensibility

With careful migration planning and phased rollout, this redesign can be implemented without disrupting existing users while positioning AD4M as a best-in-class P2P ORM.
