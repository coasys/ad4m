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

export type Query = {
  source?: string;
  properties?: string[];
  /** @deprecated Use `include` instead. */
  relations?: string[];
  /**
   * Relation names to eagerly load as full model instances.
   *
   * Each name must match a relation defined on the model via `@HasMany`,
   * `@HasOne`, `@BelongsToMany`, or `@BelongsToOne`. The relation must have
   * either a `relatedModel` factory (set by passing `() => ModelClass` as the
   * second decorator argument) or a `where.isInstance` class so that the
   * loader knows which model class to instantiate.
   *
   * When `include` is **not** set, the existing behaviour is preserved:
   * every relation that has a `relatedModel` factory is batch-hydrated
   * automatically.
   *
   * When `include` **is** set, only the listed relations are batch-hydrated,
   * giving callers explicit control over which sub-graphs to load.
   *
   * @example
   * ```typescript
   * const recipes = await Recipe.findAll(perspective, {
   *   include: ['author', 'comments'],
   * });
   * // recipe.author is a fully populated Author instance
   * // recipe.comments is an array of populated Comment instances
   * ```
   */
  include?: string[];
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
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
  /** Filter conditions */
  where?: { isInstance?: any; prologCondition?: string; condition?: string };
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
