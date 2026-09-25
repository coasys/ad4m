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
import type { LinkStatus } from "../perspectives/PerspectiveProxy";
import type { NodeExpression } from "../shacl/NodeExpression";

// ---------------------------------------------------------------------------
// Query DSL types
// ---------------------------------------------------------------------------

/**
 * Who wrote a link: a DID, any of several DIDs, or `{ not }` / `{ contains }`
 * over the author DID.
 */
export type AuthorCondition =
  | string
  | string[]
  | { not?: string | string[]; contains?: string };

export type WhereOps = {
  not: string | number | boolean | string[] | number[];
  between: [number, number];
  lt: number; // less than
  lte: number; // less than or equal to
  gt: number; // greater than
  gte: number; // greater than or equal to
  contains: string | number; // substring/element check
  /** Equality as an operator: `{ eq: X }` is the bare value `X` (an array
   *  means any of). It lets a value sit beside `author`, and cannot be
   *  combined with the other value operators. */
  eq: string | number | boolean | string[] | number[];
  /** Per-link author: the link that satisfies this property's condition was
   *  written by this author, e.g. `{ agent: { eq: did, author: admin } }`.
   *  Alone, `{ agent: { author: admin } }`: admin wrote some `agent` link.
   *  Only on properties and relations stored as links (not getters,
   *  `timestamp` or `id`). See "Filtering by Author" in the model-classes guide. */
  author: AuthorCondition;
};
export type WhereCondition =
  | string
  | number
  | boolean
  | string[]
  | number[]
  | { [K in keyof WhereOps]?: WhereOps[K] }
  | Where[]
  | Where;

export type Where = {
  /** Logical OR: instance must satisfy at least one of the given sub-clauses. */
  OR?: Where[];
  /** Logical AND: instance must satisfy all of the given sub-clauses. */
  AND?: Where[];
  /** Logical NOT: instance must NOT satisfy the given sub-clause. */
  NOT?: Where;
  /**
   * Only instances that are valid outputs of a completed run of `flow`
   * (optionally: of a run settled into terminal state `state`).
   *
   * Decided executor-side by cryptographic receipt verification, not by a
   * property: an instance passes only when a verified receipt of the flow
   * names it among the outputs its quorum committed to AND its live content
   * still matches that commitment. Fail-closed — a forged, unverifiable or
   * stale receipt excludes the instance — and applied BEFORE `limit`/
   * `offset`, so a page of N is N valid outputs. Only supported as a
   * top-level key on the queried class; anywhere else the query errors.
   */
  producedByFlow?: ProducedByFlowFilter;
  [propertyName: string]: WhereCondition | undefined;
};
/** The `where.producedByFlow` filter — see {@link Where.producedByFlow}. */
export type ProducedByFlowFilter = { flow: string; state?: string };
export type Order = { [propertyName: string]: "ASC" | "DESC" };

/**
 * A named subgraph scope: a parent node + linking predicate.
 *
 * Reusable across contexts that need to identify "the subtree under node X
 * linked via predicate P": query-time filtering as a `parent` filter on a
 * `ModelQuery` (the "all Messages belonging to Channel X" case) AND
 * AutoProcessor's `existingScope` / `mintScope` fields — the former
 * constrains dedup lookups; the latter turns each new mint into a child
 * link under the given node.
 *
 * **Model form** (preferred for parent-filter queries) — predicate
 * auto-resolved from the parent model's relation metadata. Use `field` to
 * disambiguate when the parent has multiple relations targeting the same
 * child class. AutoProcessor `mintScope` requires the **Raw form** because
 * only Raw carries an explicit linking predicate; `Model` scopes have no
 * predicate to write.
 *
 * **Raw form** — explicit predicate string, no metadata lookup.
 *
 * **Traverse form** — bounded traversal from one or more anchors, for reading
 * a tree rather than one node's children. Read-only: AutoProcessor's write
 * scopes reject it, because it names no single parent to write under.
 */
export type Scope =
  | { model: typeof Ad4mModel; id: string; field?: string }
  | { id: string; predicate: string }
  | TraverseScope;

/**
 * Walk a predicate from several anchors at once, and optionally all the way
 * down.
 *
 * This exists because a tree read one level at a time costs a round trip per
 * level and — under a subscription — one subscription per *parent*. Asking for
 * every anchor in one query makes both proportional to depth instead.
 *
 * The predicate is named the same two ways the other scopes name it: literally,
 * or by the model and relation that own it. See {@link TraverseByPredicate} and
 * {@link TraverseByModel}.
 */
export type TraverseScope = TraverseByPredicate | TraverseByModel;

/** A traversal naming its predicate literally, as the Raw scope does. */
export interface TraverseByPredicate extends TraverseOptions {
  predicate: string;
}

/**
 * A traversal naming its predicate through the model that declares it, as the
 * Model scope does — `{ model: Post, field: 'comments' }` rather than
 * `'test://has_comment'`.
 *
 * Without `field` the predicate is resolved by scanning for a relation on
 * `model` whose target is the queried class, which is the same rule the Model
 * scope uses and the same reason to pass `field` when a parent has more than
 * one relation to the same child.
 */
export interface TraverseByModel extends TraverseOptions {
  model: typeof Ad4mModel;
  field?: string;
}

/** Everything a traversal says that is not how it names its predicate. */
export interface TraverseOptions {
  /** The anchors to walk from. A bare string is the single-anchor spelling. */
  ids: string | string[];
  /**
   * Follow the predicate as far as it goes rather than one step.
   *
   * The result is a flat set of everything reachable, and it does **not**
   * describe the shape it came from: SPARQL property paths bind no intermediate
   * variables, so a row says that it is under the anchor and never where. To
   * rebuild a tree, read the inverse relation (`@BelongsToOne`) alongside it and
   * assemble from the parent each row reports.
   *
   * Excludes every named anchor, not just the one a given row was reached through — a caller
   * naming two anchors where one sits below the other gets neither back, exactly as with `levels`.
   * An anchor is where the read started, not something it found, and that holds in a cycle too.
   *
   * Refused alongside `levels`, which is the bounded form of the same walk.
   */
  transitive?: boolean;
  /**
   * `'out'` (default) matches `anchor --predicate--> result`; `'in'` matches
   * `result --predicate--> anchor`, which is how to *search* among the things
   * pointing at a node — ordered, filtered, limited. A reverse `include`
   * answers the same question for rows already in hand, but cannot narrow them.
   */
  direction?: 'out' | 'in';
  /**
   * Keep at most this many results per anchor — "the top 5 replies under each
   * of these 20 comments" in one query.
   *
   * Applied by the executor between selecting ids and hydrating them, because
   * SPARQL has no per-group limit (no window functions, and sub-SELECTs are
   * uncorrelated). The practical consequence is that the over-fetch is paid in
   * ids, not records: a branch with 3,000 replies costs 3,000 strings and
   * hydrates 5.
   *
   * Pair it with `order` — without one the "top" N is whatever the store
   * happened to return first. A global `limit`/`offset` is applied to the
   * sliced union afterwards, not before it.
   *
   * Refused alongside `levels`, which applies its own per-anchor limit at
   * every depth.
   */
  limitPerAnchor?: number;
  /**
   * Walk the relation level by level, keeping this many results per anchor at each depth —
   * `[10, 5, 3]` is "ten replies, five under each of those, three under each of *those*".
   *
   * The walk happens in the executor, which is the point of it. A caller can drive the same walk by
   * asking for one level and using the ids as the next level's anchors, but each step is then a
   * network round trip, and a UI that draws as each answer lands shows the tree assembling itself a
   * level at a time. Inside the executor the levels are sequential queries against a local store
   * with nothing serialised between them, and the records are hydrated once for the union — so
   * three levels cost one request and one hydration rather than three of each.
   *
   * Results come back flat and breadth-first; read the inverse relation alongside to rebuild the
   * tree, exactly as with `transitive`. The anchors are excluded from their own result, also as
   * with `transitive` — a walk that reaches an anchor again, through a cycle or because one anchor
   * was named below another, reports it at neither place.
   *
   * Each record appears once, under whichever anchor the ordering reaches first — a reply to two
   * of the anchors is one reply, not two. It does not spend a place in the other anchors' breadth
   * either: they fill theirs with replies of their own, so a breadth of five means five distinct
   * records wherever five exist.
   *
   * A global `limit`/`offset` applies to that flat union once, after every level has been cut to
   * its own breadth — not to each level.
   *
   * Combining this with `transitive` or `limitPerAnchor` is an **error**, not a preference the
   * executor resolves: `transitive` is the unbounded form of the same walk, and `limitPerAnchor`
   * has no place to act when the walk sets a per-anchor limit at every depth itself. (It is not a
   * substitute either — with a single anchor at the top there is one group, so it caps the total
   * rather than the breadth at each level.) Put the first level's breadth in `levels[0]`.
   */
  levels?: number[];
}

/**
 * Whether this scope is a traversal rather than a single named parent.
 *
 * The other two forms carry `id`; this one carries `ids` and may name several,
 * so anything reaching for one parent — every write path — has to ask first.
 */
export function isTraverseScope(scope: Scope): scope is TraverseScope {
  return 'ids' in scope;
}

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
   * Project over everything reachable through `from`, not just one step.
   *
   * `{ from: 'comments', count: true, transitive: true }` is the whole
   * conversation under each row rather than its direct replies — which is what
   * a reader takes "42 replies" on a collapsed branch to mean. The projection
   * query is already grouped per parent and already asked of every row at once,
   * so this adds no round trip.
   *
   * Cannot be combined with a filter on the link's author or timestamp: those
   * read the reification of one link, and a path has none. Such a projection is
   * skipped with a warning rather than silently answering a different question.
   */
  transitive?: boolean;
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
  parent?: Scope;
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
  /**
   * Read instances as they exist in links of one status only.
   *
   * `'shared'` hydrates from Shared links only: every property, relation,
   * `author` and `updatedAt` comes from links that are gossiped to the
   * neighbourhood, and none from this executor's Local links. That is the
   * read to use when showing data to another user (#1024). `'local'` is the
   * converse. Unset or `null` (the default) reads both, and a property
   * declared `local: true` still reads only its Local links.
   *
   * Relations include typed `@HasMany` / `@HasOne` relations, which are filled
   * by the conformance getter the SDK generates: the executor adds the status
   * check to that getter's relation link. A hand-written `getter` is run as
   * written, so it reads links of any status unless it checks
   * `<ad4m://ontology/status>` itself.
   *
   * Included relations and the instances a `$` projection hydrates inherit
   * the setting, unless an include's sub-query sets its own. The `__links`
   * rows (see {@link Query.links}) are restricted the same way.
   *
   * Combines with {@link Query.includeUnverified}: a link must have the
   * requested status **and** pass the signature check, both on the same link.
   * So a Local link whose signature did not verify is withheld under
   * `linkStatus: 'local'` too, and comes back only with
   * `linkStatus: 'local', includeUnverified: true` (or with no `linkStatus`
   * and `includeUnverified: true`).
   *
   * Scope: this restricts the links that hydrate an instance and the links
   * that *select* it (`where`, the class's flags, `count`/`totalCount`, `$`
   * projections, `parent` scopes and the order behind `limit`/`offset`). So an
   * instance is returned under `'local'` only when it is flagged in a Local
   * link, and under `'shared'` only when it is flagged in a Shared one.
   */
  linkStatus?: LinkStatus;
  /**
   * Also hydrate from links whose signature did not verify.
   *
   * By default the executor withholds every link whose stored signature
   * verdict is not valid, so a forged or tampered link never becomes a
   * property value, a relation target, `author` or `updatedAt`. That
   * includes a typed relation's generated conformance getter. A `getter` you
   * write yourself runs as written, so it reads unverified links unless it
   * joins the link's `ad4m://ontology/proofValid` itself. Set this to
   * `true` only to *display* an unverified claim, e.g. a UI that marks a value
   * as unverified. Anything that acts on the data, such as a vote counter or a
   * role check, must leave it off.
   *
   * Included relations inherit the setting unless their sub-query sets its own.
   *
   * The same applies to the rows under `__links` and to what *selects* an
   * instance: `where` (including a per-link `author`), the class's flags,
   * `count`/`totalCount`, the order behind `limit`/`offset`, `parent` scopes
   * and `$` projections, `transitive` ones included. With `true`, selection
   * reads unverified links too.
   */
  includeUnverified?: boolean;
  /**
   * Return the individual links behind each instance, with their own author,
   * timestamp, signature and signature verdict, under `instance.__links`. For
   * a collection this is per-item provenance: who added each member, when, and
   * whether their signature holds.
   *
   * Each entry is a property or relation name the model declares, or an
   * absolute predicate IRI — including one the model does **not** declare
   * (an annotation such as a revocation tombstone). A name wins over the IRI
   * reading. Every requested entry is present on every instance, as `[]` when
   * it has no such link. An entry that is neither a declared name nor an IRI,
   * or that names a reverse relation (`@BelongsToOne` / `@BelongsToMany`),
   * makes the query fail rather than answer `[]`.
   *
   * The rows are read by a second query after the instances are hydrated, so
   * asking for them never changes `createdAt`, `updatedAt` or any property.
   * See {@link LinkRow}.
   */
  links?: string[];
};

/**
 * One stored link as returned under `__links` — the same shape as the
 * `LinkExpression` `perspective.get()` returns, including the signature
 * verdict the executor recorded when the link was stored.
 *
 * `proof.valid` is `true` only when the signature verifies against `author`.
 * A link stored without a proof arrives with `key` and `signature` set to
 * `""` and `valid: false`; that is an unverifiable link, not a valid unsigned
 * one. Anything that acts on a row (counting a vote, granting a role) should
 * require `proof.valid`.
 *
 * `valid` is never `null` here and `invalid` is always `!valid`, the same
 * convention as `perspective.get()`. So `invalid: true` does not by itself mean
 * a signature failed: it also covers a link with no proof, or with no recorded
 * verdict. To tell an unsigned link from a failed signature, check whether
 * `proof.signature` is `""`.
 */
export interface LinkRow {
  author: string;
  timestamp: string;
  data: { source: string; predicate: string; target: string };
  proof: { key: string; signature: string; valid: boolean; invalid: boolean };
  /**
   * Where this executor holds the link, spelled as `perspective.get()` spells
   * it. Absent only when the store recorded none. Not signed: it says nothing
   * about who wrote the link.
   */
  status?: "SHARED" | "LOCAL";
}

/** `instance.__links`: requested entry (spelled as requested) → its rows, oldest first. */
export type LinksMap = Record<string, LinkRow[]>;

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
      : NonNullable<T[K]> extends Ad4mModel[] ? never
      : K
    : never
}[keyof T];

/** Keys of T that can appear in `include` — typed Ad4mModel references AND
 *  `string[]` link-target arrays (the `@HasMany` + `string[]` pattern that
 *  AD4M uses for relations without a target class thunk). Handles both
 *  `?: Foo` (optional → `Foo | undefined`) and `Foo | null` field shapes, and
 *  optional/nullable array variants (`?: Foo[]`, `Foo[] | null`). */
export type RelationKeysOf<T extends Ad4mModel> = {
  [K in keyof T]: K extends ModelDataKeys<T>
    ? NonNullable<T[K]> extends Ad4mModel ? K
      : NonNullable<T[K]> extends Ad4mModel[] ? K
      : NonNullable<T[K]> extends string[] ? K
      : never
    : never
}[keyof T];

/** The model type referenced by a relation field K on T.
 *  Falls back to `Ad4mModel` (loose) for the `string[]` relation pattern,
 *  since no target class is available at the type level. */
export type RelatedModel<T extends Ad4mModel, K extends RelationKeysOf<T>> =
    NonNullable<T[K]> extends (infer U)[] ? (U extends Ad4mModel ? U : Ad4mModel)
  : NonNullable<T[K]> extends Ad4mModel ? NonNullable<T[K]>
  : Ad4mModel;

/** True when T has no statically-declared data fields (e.g. fromSHACL classes). */
type HasNoTypedFields<T extends Ad4mModel> =
  [PropertyKeysOf<T> | RelationKeysOf<T>] extends [never] ? true : false;

// ---- Typed where conditions --------------------------------------------------

export type StringWhereOps = {
  not?: string | string[];
  contains?: string;
  /** See {@link WhereOps.eq}. */
  eq?: string | string[];
  /** See {@link WhereOps.author}. */
  author?: AuthorCondition;
};

export type NumericWhereOps = {
  not?: number | number[];
  lt?: number;
  lte?: number;
  gt?: number;
  gte?: number;
  between?: [number, number];
  /** See {@link WhereOps.eq}. */
  eq?: number | number[];
  /** See {@link WhereOps.author}. */
  author?: AuthorCondition;
};

export type TypedWhereCondition<V> =
    V extends string  ? string | string[] | StringWhereOps
  : V extends number  ? number | number[] | NumericWhereOps
  : V extends boolean ? boolean | { eq?: boolean; author?: AuthorCondition }
  : V extends Array<infer U>
      ? U extends string ? string | string[] | StringWhereOps
        : U extends number ? number | number[] | NumericWhereOps
        : WhereCondition
  : WhereCondition;

/** Strict Where: keys constrained to T's properties, plus the well-known
 *  link-metadata fields (`id`/`author`/`timestamp`) and logical combinators. */
type StrictTypedWhere<T extends Ad4mModel> =
  & { [K in PropertyKeysOf<T>]?: TypedWhereCondition<T[K]> }
  & {
      base?: string | string[];
      id?: string | string[];
      /** Alone: the instance's `.author`, i.e. its earliest link's author.
       *  Beside property/relation conditions in the same object: that, AND
       *  each of those conditions is satisfied by a link this author wrote.
       *  For "this author wrote the `agent` link" alone, nest it:
       *  `{ agent: { eq: did, author } }`. It does not reach into
       *  `OR`/`AND`/`NOT` sub-clauses. See "Filtering by Author" in the
       *  model-classes guide. */
      author?: WhereCondition;
      timestamp?: WhereCondition;
      OR?: StrictTypedWhere<T>[];
      AND?: StrictTypedWhere<T>[];
      NOT?: StrictTypedWhere<T>;
    };

/** Public typed `where` for a model class. Falls back to the loose `Where`
 *  shape when T has no declared fields (e.g. fromSHACL-derived classes). */
export type TypedWhere<T extends Ad4mModel> =
  HasNoTypedFields<T> extends true ? Where : StrictTypedWhere<T>;

/** Top-level typed `where` of a query on T: {@link TypedWhere} plus
 *  `producedByFlow`. Kept out of `TypedWhere` itself because that shape is
 *  reused under `OR`/`AND`/`NOT` and in include sub-queries, where the
 *  executor rejects `producedByFlow` (see {@link Where.producedByFlow}). */
export type TypedQueryWhere<T extends Ad4mModel> =
  TypedWhere<T> & { producedByFlow?: ProducedByFlowFilter };

// ---- Typed order -------------------------------------------------------------

type StrictTypedOrder<T extends Ad4mModel> =
  & { [K in PropertyKeysOf<T> | 'timestamp' | 'author' | 'createdAt' | 'updatedAt']?: 'ASC' | 'DESC' }
  // $-prefixed projection count keys (e.g. "$likeCount") — typed at the
  // include map level but accepted here so callers can sort by them.
  & { [K in `$${string}`]?: 'ASC' | 'DESC' };

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
  /** See {@link Query.linkStatus}. Inherited from the parent query when unset. */
  linkStatus?: LinkStatus;
  /** See {@link Query.includeUnverified}. Inherited from the parent query when unset. */
  includeUnverified?: boolean;
  links?: string[];
};

/** Projection — `from` must be a real relation on T; `where`/`order` constrained to that target.
 *  Modelled as a discriminated union so the literal `count: true` and `limit: 1`
 *  variants narrow inference into `IncludeExtras` (count → number, limit-1 → scalar). */
export type TypedIncludeProjection<T extends Ad4mModel> = {
  [K in RelationKeysOf<T>]:
    | { from: K; count: true; transitive?: boolean }
    | { from: K; limit: 1; transitive?: boolean; where?: TypedWhere<RelatedModel<T, K>>; order?: TypedOrder<RelatedModel<T, K>> }
    | { from: K; limit?: number; transitive?: boolean; where?: TypedWhere<RelatedModel<T, K>>; order?: TypedOrder<RelatedModel<T, K>> };
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
  parent?: Scope;
  properties?: PropertyKeysOf<T>[];
  include?: TypedIncludeMap<T>;
  includeAll?: boolean;
  where?: TypedQueryWhere<T>;
  order?: TypedOrder<T>;
  offset?: number;
  limit?: number;
  count?: boolean;
  deepQuery?: boolean;
  /** See {@link Query.linkStatus}. */
  linkStatus?: LinkStatus;
  /** See {@link Query.includeUnverified}. */
  includeUnverified?: boolean;
  links?: string[];
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
  /** Sole selector of storage mode:
   *   - unset               → deterministic typed literal (fast POS-index
   *                            path, the default for a plain `@Property()`)
   *   - `"literal"`         → signed envelope on the built-in literal language
   *   - `<custom address>`  → expression on that custom language
   */
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
  transform?: NodeExpression;
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
  /**
   * CRDT ordering config, when this collection has a user-controlled order.
   *
   * Its presence is what makes a reorder count as a change: for an ordered
   * relation the sequence *is* the state, where an unordered one is a set.
   */
  ordering?: { strategy: 'linkedList' };
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
