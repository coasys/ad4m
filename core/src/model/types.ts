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
   * Serialised target class shape.  Set automatically by `executeModelQuery`
   * when a `where` clause is present and the target class is resolvable.
   * @internal
   */
  targetShape?: ModelMetadata;
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

// ---------------------------------------------------------------------------
// Typed Query DSL — Phase 1 of PLAN_6_TYPED_QUERIES.md
//
// These types layer compile-time field-name and value-type checking on top of
// the loose Query/Where/Order/IncludeMap. They infer everything from the
// model class's declared TS fields — no codegen, no runtime changes.
//
// Dynamic models (e.g. produced by `Ad4mModel.fromSHACL()`) carry no
// field-level type information; for those `TypedQuery<Ad4mModel>` collapses
// to the loose `Query` shape, which is the correct escape hatch.
// ---------------------------------------------------------------------------

/** Keys of T that are user-declared (not methods, not inherited from Ad4mModel). */
export type ModelDataKeys<T extends Ad4mModel> = {
  [K in keyof T]: K extends keyof Ad4mModel ? never
    : T[K] extends (...args: any[]) => any ? never
    : K
}[keyof T];

/** Keys of T that can appear in a `where` clause — scalars and scalar arrays.
 *  Excludes typed Ad4mModel references (single or array); `string[]`/`number[]`
 *  remain included because they're often filterable even when also used as
 *  link targets.
 *
 *  Iterates over `keyof T` directly (not `ModelDataKeys<T>`) to preserve the
 *  key constraint — mapped types over computed type aliases sometimes widen
 *  to `string | undefined` in the key position, which then breaks indexing. */
export type PropertyKeysOf<T extends Ad4mModel> = {
  [K in keyof T]: K extends ModelDataKeys<T>
    ? NonNullable<T[K]> extends Ad4mModel ? never
      : T[K] extends Ad4mModel[] ? never
      : K
    : never
}[keyof T];

/** Keys of T that can appear in `include` — typed Ad4mModel references AND
 *  `string[]` link-target arrays (the `@HasMany` + `string[]` pattern that
 *  AD4M uses for relations without a target class thunk). Handles both
 *  `?: Foo` (optional → `Foo | undefined`) and `Foo | null` field shapes. */
export type RelationKeysOf<T extends Ad4mModel> = {
  [K in keyof T]: K extends ModelDataKeys<T>
    ? NonNullable<T[K]> extends Ad4mModel ? K
      : T[K] extends Ad4mModel[] ? K
      : T[K] extends string[] ? K
      : never
    : never
}[keyof T];

/** The model type referenced by a relation field K on T.
 *  Falls back to `Ad4mModel` (loose) for the `string[]` relation pattern,
 *  since no target class is available at the type level. */
export type RelatedModel<T extends Ad4mModel, K extends RelationKeysOf<T>> =
    T[K] extends (infer U)[] ? (U extends Ad4mModel ? U : Ad4mModel)
  : NonNullable<T[K]> extends Ad4mModel ? NonNullable<T[K]>
  : Ad4mModel;

/** True when T has no statically-declared data fields (e.g. fromSHACL classes). */
type HasNoTypedFields<T extends Ad4mModel> =
  [PropertyKeysOf<T> | RelationKeysOf<T>] extends [never] ? true : false;

// ---- Typed where conditions --------------------------------------------------

export type StringWhereOps = {
  not?: string | string[];
  contains?: string;
};

export type NumericWhereOps = {
  not?: number | number[];
  lt?: number;
  lte?: number;
  gt?: number;
  gte?: number;
  between?: [number, number];
};

export type TypedWhereCondition<V> =
    V extends string  ? string | string[] | StringWhereOps
  : V extends number  ? number | number[] | NumericWhereOps
  : V extends boolean ? boolean
  : V extends Array<infer U>
      ? U extends string ? string | string[] | StringWhereOps
        : U extends number ? number | number[] | NumericWhereOps
        : WhereCondition
  : WhereCondition;

/** Strict Where: keys constrained to T's properties, plus the well-known
 *  link-metadata fields (`id`/`author`/`timestamp`). */
type StrictTypedWhere<T extends Ad4mModel> =
  & { [K in PropertyKeysOf<T>]?: TypedWhereCondition<T[K]> }
  & {
      base?: string | string[];
      id?: string | string[];
      author?: WhereCondition;
      timestamp?: WhereCondition;
    };

/** Public typed `where` for a model class. Falls back to the loose `Where`
 *  shape when T has no declared fields (e.g. fromSHACL-derived classes). */
export type TypedWhere<T extends Ad4mModel> =
  HasNoTypedFields<T> extends true ? Where : StrictTypedWhere<T>;

// ---- Typed order -------------------------------------------------------------

type StrictTypedOrder<T extends Ad4mModel> = {
  [K in PropertyKeysOf<T> | 'timestamp' | 'author' | 'createdAt' | 'updatedAt']?: 'ASC' | 'DESC';
};

export type TypedOrder<T extends Ad4mModel> =
  HasNoTypedFields<T> extends true ? Order : StrictTypedOrder<T>;

// ---- Typed include + projection ---------------------------------------------

/** Sub-query for an eager-loaded relation — inherits the target model's constraints. */
export type TypedRelationSubQuery<U extends Ad4mModel> = {
  where?: TypedWhere<U>;
  order?: TypedOrder<U>;
  include?: TypedIncludeMap<U>;
  limit?: number;
  offset?: number;
};

/** Projection — `from` must be a real relation on T; `where`/`order` constrained to that target.
 *  Modelled as a discriminated union so the literal `count: true` and `limit: 1`
 *  variants narrow inference into `IncludeExtras` (count → number, limit-1 → scalar). */
export type TypedIncludeProjection<T extends Ad4mModel> = {
  [K in RelationKeysOf<T>]:
    | { from: K; count: true }
    | { from: K; limit: 1; where?: TypedWhere<RelatedModel<T, K>>; order?: TypedOrder<RelatedModel<T, K>> }
    | { from: K; limit?: number; where?: TypedWhere<RelatedModel<T, K>>; order?: TypedOrder<RelatedModel<T, K>> };
}[RelationKeysOf<T>];

type StrictTypedIncludeMap<T extends Ad4mModel> =
  & { [K in RelationKeysOf<T>]?: boolean | TypedRelationSubQuery<RelatedModel<T, K>> }
  & { [K in `$${string}`]?: TypedIncludeProjection<T> };

export type TypedIncludeMap<T extends Ad4mModel> =
  HasNoTypedFields<T> extends true ? IncludeMap : StrictTypedIncludeMap<T>;

// ---- Result-side: project $-keys into the returned row type ------------------

/**
 * Given a model T and an `include` literal I, compute the extra fields
 * contributed by `$`-prefixed projection keys.
 *
 * - `{ count: true }`           → `number`
 * - `{ from: R, limit: 1 }`     → `RelatedModel<T,R> | null`
 * - `{ from: R }` (no limit/1)  → `RelatedModel<T,R>[]`
 *
 * Returns `unknown` (intersection-neutral) when `I` has no `$`-keys, so that
 * `T & IncludeExtras<T, I>` collapses back to `T` and downstream type
 * predicates (`(x): x is T => ...`) keep working unchanged.
 */
export type IncludeExtras<T extends Ad4mModel, I> =
  I extends Record<string, any>
    ? Extract<keyof I, `$${string}`> extends never
      ? unknown
      : {
          [K in Extract<keyof I, `$${string}`>]:
              I[K] extends { count: true } ? number
            : I[K] extends { from: infer R; limit: 1 }
                ? (R extends RelationKeysOf<T> ? RelatedModel<T, R> | null : never)
            : I[K] extends { from: infer R }
                ? (R extends RelationKeysOf<T> ? RelatedModel<T, R>[] : never)
            : unknown;
        }
    : unknown;

// ---- Typed Query -------------------------------------------------------------

type StrictTypedQuery<T extends Ad4mModel> = {
  parent?: ParentScope;
  properties?: PropertyKeysOf<T>[];
  include?: TypedIncludeMap<T>;
  includeAll?: boolean;
  where?: TypedWhere<T>;
  order?: TypedOrder<T>;
  offset?: number;
  limit?: number;
  count?: boolean;
  deepQuery?: boolean;
};

export type TypedQuery<T extends Ad4mModel> =
  HasNoTypedFields<T> extends true ? Query : StrictTypedQuery<T>;

/** Helper that extracts the `include` literal from a TypedQuery for use with IncludeExtras. */
export type IncludeOf<Q> = Q extends { include?: infer I } ? I : undefined;

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
