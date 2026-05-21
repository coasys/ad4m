/**
 * Shared type definitions for the Ad4mModel system.
 *
 * Extracted into a standalone module so that utility files
 * (`query-utils.ts`, `decorators.ts`, etc.) can import types without
 * creating circular runtime dependencies on `Ad4mModel.ts`.
 */

// ---------------------------------------------------------------------------
// Forward type-only reference to Ad4mModel (no runtime circular dependency)
// ---------------------------------------------------------------------------
import type { Ad4mModel } from "./Ad4mModel";

// ---------------------------------------------------------------------------
// Query DSL types
// ---------------------------------------------------------------------------

export type WhereOps = {
  not: string | number | boolean | string[] | number[];
  between: [number, number];
  lt: number; // less than
  lte: number; // less than or equal to
  gt: number; // greater than
  gte: number; // greater than or equal to
  contains: string | number; // substring/element check
};
export type WhereCondition = string | number | boolean | string[] | number[] | { [K in keyof WhereOps]?: WhereOps[K] };
export type Where = { [propertyName: string]: WhereCondition };
export type Order = { [propertyName: string]: "ASC" | "DESC" };

/**
 * Discriminated union for parent-scoped queries.
 *
 * **Model form** (preferred) — predicate auto-resolved from the parent model's
 * relation metadata. Use `field` to disambiguate when the parent has multiple
 * relations targeting the same child class.
 *
 * **Raw form** — explicit predicate string, no metadata lookup.
 */
export type ParentScope =
  | { model: typeof Ad4mModel; id: string; field?: string }
  | { id: string; predicate: string };

/**
 * Describes which relations to eager-load when querying.
 *
 * Each value is either:
 * - `true` — hydrate the relation one level deep
 * - A `RelationSubQuery` — scoped sub-query (filter / sort / paginate / nested include)
 *
 * @example
 * ```typescript
 * // One level deep
 * { comments: true }
 *
 * // Sub-query: only the 5 most-recent comments
 * { comments: { order: { createdAt: 'DESC' }, limit: 5 } }
 *
 * // Nested eager-load
 * { comments: { include: { author: true } } }
 * ```
 */
export interface IncludeMap {
  [relation: string]: boolean | RelationSubQuery | IncludeProjection;
}

/**
 * A lightweight projection that computes a scalar or list value from a relation
 * without fully hydrating the linked instances.
 *
 * Keys in `IncludeMap` that begin with `$` are treated as projections.
 * Results are attached directly to each queried instance under the same key.
 *
 * @example
 * ```typescript
 * // Attach a like-count to each post
 * const posts = await Post.findAll(perspective, {
 *   include: {
 *     $likeCount: { from: 'likes', count: true },
 *     $myLike:    { from: 'likes', limit: 1 },
 *   }
 * });
 * posts[0].$likeCount  // number
 * posts[0].$myLike     // string ID or null
 * ```
 */
export interface IncludeProjection {
  /** The relation name on the parent model to project over. */
  from: string;
  /** When true, attaches an integer count instead of a list. */
  count?: true;
  /**
   * Bare class name of the projection target.  Set automatically by
   * `prepareModelQueryParams`; the executor resolves the target's shape
   * through its in-memory cache when applying projection where filters.
   * @internal
   */
  targetClassName?: string;
  /** Post-hydration where clause applied to the target instances. */
  where?: Where;
  /** Limit results (when 1, the attached value is unwrapped to a scalar). */
  limit?: number;
  /** Order results before applying limit. */
  order?: Order;
}

/** Type guard for IncludeProjection objects. */
export function isIncludeProjection(val: unknown): val is IncludeProjection {
  return typeof val === 'object' && val !== null && 'from' in val;
}

export type Query = {
  /** Filter to instances that are the target of a link from a given parent. */
  parent?: ParentScope;
  properties?: string[];
  include?: IncludeMap;
  /**
   * When true, eager-loads every declared forward relation on the model.
   * Equivalent to listing all relation field names in `include: {}` explicitly.
   * Useful for tree-walking and manifest-style inspections.
   * Individual relations can still be overridden via `include` (explicit entries take precedence).
   */
  includeAll?: boolean;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
  /**
   * When true, SPARQL property getters (@Property with `getter`) ARE evaluated
   * during collection hydration.  Defaults to true — getters run post-pagination
   * via batched VALUES queries so the cost is O(getters), not O(instances × getters).
   *
   * Set to false to explicitly skip getter evaluation for performance-sensitive
   * queries where getter-backed properties are not needed.
   */
  deepQuery?: boolean;
};

/**
 * Sub-query options for a specific relation inside an `IncludeMap`.
 *
 * Equivalent to `Query` without top-level scoping (`parent`) or `count`,
 * since the result set is already constrained to the linked relation.
 *
 * @example
 * ```typescript
 * await post.get({ include: { comments: { order: { createdAt: 'DESC' }, limit: 5 } } });
 * ```
 */
export type RelationSubQuery = Omit<Query, 'parent' | 'count'>;

/**
 * Options accepted by the instance `get()` method.
 *
 * A subset of `Query` — only hydration controls apply to a single known instance.
 */
export type GetOptions = Pick<Query, 'include' | 'properties'>;

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

export type AllInstancesResult = { AllInstances: Ad4mModel[]; TotalCount?: number };
export type ResultsWithTotalCount<T> = { results: T[]; totalCount?: number };
export type PaginationResult<T> = { results: T[]; totalCount?: number; pageSize: number; pageNumber: number };

// ---------------------------------------------------------------------------
// Internal value transport
// ---------------------------------------------------------------------------

export type ValueTuple = [name: string, value: any, resolve?: boolean];

// ---------------------------------------------------------------------------
// Model metadata interfaces
// ---------------------------------------------------------------------------

/**
 * Metadata for a single property extracted from decorators.
 */
export interface PropertyMetadata {
  /** The property name */
  name: string;
  /** The predicate URI (through value) */
  predicate: string;
  /** Whether the property is required */
  required: boolean;
  /** Whether the property is read-only */
  readOnly: boolean;
  /** Initial value if specified */
  initial?: string;
  /** Language for resolution (e.g., "literal") */
  resolveLanguage?: string;
  /** Custom Prolog getter code */
  prologGetter?: string;
  /** Custom Prolog setter code */
  prologSetter?: string;
  /** Custom getter code */
  getter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Transform function */
  transform?: (value: any) => any;
  /** Whether this is a flag property */
  flag?: boolean;
}

/**
 * Metadata for a single relation extracted from decorators.
 */
export interface RelationMetadata {
  /** The relation name */
  name: string;
  /** The predicate URI (through value) */
  predicate: string;
  /** Custom getter code */
  getter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Link direction: 'forward' for HasMany/HasOne, 'reverse' for BelongsToMany/BelongsToOne */
  direction?: 'forward' | 'reverse';
  /** Target model class thunk for hydration and type filtering */
  target?: () => any;
  /**
   * Whether to auto-generate a conformance filter when `target` is set.
   * Defaults to `true` — set to `false` to opt out of DB-level type filtering.
   */
  filter?: boolean;
  /** Where clause for relation filtering (query DSL) */
  where?: Where;
}

/**
 * Complete model metadata extracted from decorators.
 */
export interface ModelMetadata {
  /** The model class name from @Model */
  className: string;
  /** Map of property name to metadata */
  properties: Record<string, PropertyMetadata>;
  /** Map of relation name to metadata */
  relations: Record<string, RelationMetadata>;
}
