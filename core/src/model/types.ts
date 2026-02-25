/**
 * Public types for the AD4M model layer.
 *
 * Extracted from Ad4mModel.ts so they can be imported by query builders,
 * hydration helpers, and external consumers without pulling in the full
 * Ad4mModel class.
 */

// ── Where / Query ──────────────────────────────────────────────────────────

type WhereOps = {
  not: string | number | boolean | string[] | number[];
  between: [number, number];
  lt: number; // less than
  lte: number; // less than or equal to
  gt: number; // greater than
  gte: number; // greater than or equal to
  contains: string | number; // substring/element check
};

export type WhereCondition =
  | string
  | number
  | boolean
  | string[]
  | number[]
  | { [K in keyof WhereOps]?: WhereOps[K] };

export type Where = { [propertyName: string]: WhereCondition };
export type Order = { [propertyName: string]: "ASC" | "DESC" };

/**
 * Prisma-style eager-loading map.
 *
 * Key = relation field name on the model.
 * Value = `true` (hydrate all with no filter) or a `Query` to
 * filter / order / limit the nested set.
 *
 * @example
 * ```typescript
 * Recipe.findAll(perspective, {
 *   include: {
 *     comments: true,
 *     tags: { where: { active: true }, order: { name: 'ASC' }, limit: 5 },
 *   },
 * });
 * ```
 */
export type IncludeMap = { [relationName: string]: true | Query };

export type Query = {
  properties?: string[];
  /** Eagerly hydrate relations. Key = field name; value = `true` or a sub-`Query`. See {@link IncludeMap}. */
  include?: IncludeMap;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
};

/**
 * Extends `Query` (minus `count`) with subscription delivery options.
 * Pass to {@link Ad4mModel.subscribe} or the builder's `.subscribe()`.
 */
export type SubscribeOptions = Omit<Query, "count"> & {
  /**
   * Debounce delay in milliseconds.  Multiple link changes within this window
   * trigger only one re-query.  Default: `0` (no debouncing).
   */
  debounce?: number;
  /**
   * Called when the re-query or the callback throws.
   * Defaults to `console.error` so failures are always visible without
   * requiring every caller to handle them.
   */
  onError?: (err: Error) => void;
};

/**
 * Handle returned by `Ad4mModel.subscribe()`. Call `unsubscribe()` in cleanup.
 * `lastError` holds the most recent unhandled error, or `null`.
 */
export type Subscription = {
  unsubscribe(): void;
  /** The most recent unhandled error from re-query or callback, or `null`. */
  readonly lastError: Error | null;
};

// ── Result shapes ──────────────────────────────────────────────────────────

export type AllInstancesResult = any;
export type ResultsWithTotalCount<T> = { results: T[]; totalCount?: number };
export type PaginationResult<T> = {
  results: T[];
  totalCount?: number;
  pageSize: number;
  pageNumber: number;
};

// ── Model metadata ─────────────────────────────────────────────────────────

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
  /** Whether the property is writable */
  writable: boolean;
  /** Initial value if specified */
  initial?: string;
  /** Language for resolution (e.g., "literal") */
  resolveLanguage?: string;
  /** Custom SurrealQL getter code */
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
  /** Custom SurrealQL getter code */
  getter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Traversal direction — "forward" (default) for @HasMany/@HasOne, "reverse" for @BelongsToOne/@BelongsToMany */
  direction?: "forward" | "reverse";
  /** Maximum number of results (1 for @BelongsToOne / @HasOne) */
  maxCount?: number;
  /** Model factory for eager hydration — set when decorator is called with () => ModelClass */
  relatedModel?: () => any;
}

/**
 * Complete model metadata extracted from decorators.
 */
export interface ModelMetadata {
  /** The model class name from @ModelOptions */
  className: string;
  /** Map of property name to metadata */
  properties: Record<string, PropertyMetadata>;
  /** Map of relation name to metadata */
  relations: Record<string, RelationMetadata>;
}
