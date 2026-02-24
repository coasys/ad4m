import { Literal } from "../Literal";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { SHACLShape } from "../shacl/SHACLShape";
import { makeRandomId, getRelationsMetadata } from "./decorators";
import * as mutation from "./mutation";
export type { MutationContext } from "./mutation";
import { capitalize } from "./util";

// ── Public types (re-exported so consumers see no change) ──────────────────
export type {
  Query,
  Where,
  Order,
  WhereCondition,
  AllInstancesResult,
  ResultsWithTotalCount,
  PaginationResult,
  PropertyMetadata,
  RelationMetadata,
  ModelMetadata,
  IncludeMap,
  SubscribeOptions,
  Subscription,
} from "./types";
import type {
  Query,
  ResultsWithTotalCount,
  PaginationResult,
  ModelMetadata,
  IncludeMap,
  SubscribeOptions,
  Subscription,
} from "./types";

// ── JSON Schema factory ────────────────────────────────────────────────────
import { createModelFromJSONSchema } from "./schema/fromJSONSchema";
import type {
  JSONSchema,
  JSONSchemaToModelOptions,
} from "./schema/fromJSONSchema";
export type {
  JSONSchema,
  JSONSchemaToModelOptions,
  JSONSchemaProperty, // keep re-exporting for external consumers
} from "./schema/fromJSONSchema";

// ── Fluent query builder (re-exported for consumers) ──────────────────────
import { ModelQueryBuilder } from "./query/ModelQueryBuilder";
export { ModelQueryBuilder };

// ── SurrealDB query helpers (used internally, also re-exported) ────────────
export {
  buildSurrealQuery,
  buildSurrealCountQuery,
  formatSurrealValue,
  matchesCondition,
  buildGraphTraversalWhereClause,
} from "./query/surrealCompiler";

// ── Hydration utilities (re-exported for advanced consumers) ──────────────
export {
  hydrateInstanceFromLinks,
  evaluateCustomGetters,
  normalizeTimestamp,
} from "./query/hydration";
export type { RawLink } from "./query/hydration";

// ── Static query operations (each static method below delegates here) ─────────
import * as ops from "./query/operations";
import { fetchInstanceData } from "./query/fetchInstance";

// ── Metadata helpers ────────────────────────────────────────────────────────
import { getModelMetadata as _getModelMetadata } from "./schema/metadata";

// ── Transaction API ──────────────────────────────────────────────────────────
import { runTransaction } from "./transaction";
export type { TransactionContext } from "./transaction";
// ── Subscription API ───────────────────────────────────────────────────────────────
import { createSubscription } from "./subscription";
/**
 * Base class for all AD4M data models.
 *
 * Instances are subgraphs in a {@link PerspectiveProxy}; properties and relations
 * map to typed links. Decorators (`@Property`, `@HasMany`, etc.) declare the schema;
 * query helpers (`findAll`, `query`, `subscribe`) run SurrealQL against the
 * perspective's local graph engine.
 *
 * See [README.md](./README.md) for a full worked example and decorator reference.
 */
export class Ad4mModel {
  #id: string;
  #perspective: PerspectiveProxy;
  author: string;
  createdAt: any;
  updatedAt: any;

  private static classNamesByClass = new WeakMap<
    typeof Ad4mModel,
    { [perspectiveId: string]: string }
  >();

  static async getClassName(perspective: PerspectiveProxy) {
    // Check if this is the Ad4mModel class itself or a subclass
    const isBaseClass = this === Ad4mModel;

    // For the base Ad4mModel class, we can't use the cache
    if (isBaseClass) {
      return await perspective.stringOrTemplateObjectToSubjectClassName(this);
    }

    // Get or create the cache for this class
    let classCache = this.classNamesByClass.get(this);
    if (!classCache) {
      classCache = {};
      this.classNamesByClass.set(this, classCache);
    }

    // Get or create the cached name for this perspective
    const perspectiveID = perspective.uuid;
    if (!classCache[perspectiveID]) {
      classCache[perspectiveID] =
        await perspective.stringOrTemplateObjectToSubjectClassName(this);
    }

    return classCache[perspectiveID];
  }

  /**
   * Backwards compatibility alias for createdAt.
   * @deprecated Use createdAt instead. This will be removed in a future version.
   */
  get timestamp(): any {
    return (this as any).createdAt;
  }

  /** Returns the class name, property predicates, and relation predicates from decorators. */
  public static getModelMetadata(): ModelMetadata {
    return _getModelMetadata(this);
  }

  /**
   * Installs the SHACL/SDNA subject class in `perspective` (idempotent).
   *
   * @example `await Promise.all([Post, Comment, Tag].map(M => M.register(perspective)));`
   */
  static async register(perspective: PerspectiveProxy): Promise<void> {
    await perspective.ensureSDNASubjectClass(this);
  }

  /**
   * One-shot factory: constructs, assigns `data`, saves, and returns the new instance.
   *
   * @example
   * ```typescript
   * const post = await Post.create(perspective, { title: 'Hello', body: 'World' });
   * ```
   */
  static async create<T extends Ad4mModel>(
    this: new (perspective: PerspectiveProxy) => T,
    perspective: PerspectiveProxy,
    data: Partial<Omit<T, keyof Ad4mModel>>,
  ): Promise<T> {
    const instance = new this(perspective);
    Object.assign(instance, data);
    await instance.save();
    return instance;
  }

  /**
   * Generates the SHACL shape for this model class.
   * Attached dynamically by the `@Model` decorator.
   */
  static generateSHACL(): { shape: SHACLShape; name: string } {
    throw new Error(
      "generateSHACL() is only available on classes decorated with @Model",
    );
  }

  /** @param id - Auto-generated from a random literal if omitted. */
  constructor(perspective: PerspectiveProxy, id?: string) {
    this.#id = id ? id : Literal.from(makeRandomId(24)).toUrl();
    this.#perspective = perspective;

    // Wire up real relation adder/remover/setter methods for decorator-based classes.
    // The @HasMany / @HasOne decorators place empty stubs on the prototype at class-definition
    // time (e.g. `addLocations = () => {}`). Here, at instance-creation time, we replace each
    // stub with a closure that actually calls the private implementation so that callers like
    // `instance.addLocations(value)` persist the link in the perspective.
    const proto = Object.getPrototypeOf(this);
    const relations: Record<string, any> = getRelationsMetadata(
      proto.constructor,
    );
    for (const key of Object.keys(relations)) {
      // Reverse relations (@BelongsToOne / @BelongsToMany) are read-only traversals —
      // the link is owned by the other side, so no mutator methods should exist here.
      if (relations[key].direction === "reverse") continue;

      const cap = capitalize(key);
      this[`add${cap}`] = (value: any, batchId?: string) =>
        mutation.setRelationAdder(this.#mutationContext(), key, value, batchId);
      this[`remove${cap}`] = (value: any, batchId?: string) =>
        mutation.setRelationRemover(
          this.#mutationContext(),
          key,
          value,
          batchId,
        );
      this[`set${cap}`] = (value: any, batchId?: string) =>
        mutation.setRelationSetter(
          this.#mutationContext(),
          key,
          value,
          batchId,
        );
    }
  }

  /**
   * The unique identifier (base expression URI) of this instance.
   */
  get id() {
    return this.#id;
  }

  /** Read-only perspective access for subclasses. */
  protected get perspective(): PerspectiveProxy {
    return this.#perspective;
  }

  /** Builds the context object for all mutation functions. */
  #mutationContext(): mutation.MutationContext {
    return {
      perspective: this.#perspective,
      id: this.#id,
      instance: this,
    };
  }

  /**
   * Generates a SurrealQL query string for this model.
   *
   * @param perspective - The perspective context
   * @param query - Query parameters (where, order, limit, offset, properties, relations)
   * @returns Complete SurrealQL query string ready for execution
   */
  public static async queryToSurrealQL(
    perspective: PerspectiveProxy,
    query: Query,
  ): Promise<string> {
    return ops.queryToSurrealQL(this as any, perspective, query);
  }

  /** @internal */
  public static async instancesFromSurrealResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query,
    result: any[],
    _hydrateRelations = true,
  ): Promise<ResultsWithTotalCount<T>> {
    return ops.instancesFromSurrealResult(
      this as any,
      perspective,
      query,
      result,
      _hydrateRelations,
    );
  }

  /**
   * Internal implementation used by findAll and eager relation hydration.
   * Pass `_hydrateRelations = false` to prevent recursive model hydration (depth guard).
   */
  static async _findAllInternal<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
    _hydrateRelations = true,
  ): Promise<T[]> {
    return ops._findAllInternal(
      this as any,
      perspective,
      query,
      _hydrateRelations,
    );
  }

  /**
   * Returns all instances matching `query`.
   *
   * @example
   * ```typescript
   * const recipes = await Recipe.findAll(perspective, {
   *   where: { rating: { gt: 4 } }, order: { createdAt: "DESC" }, limit: 10,
   * });
   * ```
   */
  static async findAll<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<T[]> {
    return ops.findAll(this as any, perspective, query);
  }

  /**
   * Returns the first matching instance, or `null` if none found.
   *
   * @example
   * ```typescript
   * const post = await TestPost.findOne(perspective, { where: { id: someId } });
   * ```
   */
  static async findOne<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<T | null> {
    return ops.findOne(this as any, perspective, query);
  }

  /**
   * Like `findAll` but also returns the unfiltered total count (useful for pagination UI).
   *
   * @example
   * ```typescript
   * const { results, totalCount } = await Recipe.findAllAndCount(perspective, { limit: 10 });
   * console.log(`Showing ${results.length} of ${totalCount}`);
   * ```
   */
  static async findAllAndCount<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<ResultsWithTotalCount<T>> {
    return ops.findAllAndCount(this as any, perspective, query);
  }

  /**
   * Fetches a single page of results (`pageNumber` is 1-based).
   *
   * @example
   * ```typescript
   * const page = await Recipe.paginate(perspective, 10, 1);
   * console.log(`Page ${page.pageNumber}, ${page.results.length} items`);
   * ```
   */
  static async paginate<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    pageSize: number,
    pageNumber: number,
    query?: Query,
  ): Promise<PaginationResult<T>> {
    return ops.paginate(
      this as any,
      perspective,
      pageSize,
      pageNumber,
      query ?? {},
    );
  }

  /**
   * Generates a SurrealQL COUNT query for this model.
   * @private
   */
  public static async countQueryToSurrealQL(
    perspective: PerspectiveProxy,
    query: Query,
  ): Promise<string> {
    return ops.countQueryToSurrealQL(this as any, perspective, query);
  }

  /** Returns the count of instances matching `query`. */
  static async count(perspective: PerspectiveProxy, query: Query = {}) {
    return ops.count(this as any, perspective, query);
  }

  /**
   * Persists the instance (create if new, update if existing).
   *
   * @param batchId - When provided the caller must call `perspective.commitBatch(batchId)`
   *
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * await recipe.save();        // create
   * recipe.name = "Bolognese";
   * await recipe.save();        // update (detected automatically)
   * ```
   */
  async save(batchId?: string) {
    return mutation.saveInstance(this.#mutationContext(), batchId);
  }

  /**
   * @deprecated Use `save()` instead. `save()` now automatically detects whether
   * to create or update based on whether the instance already exists in the
   * perspective. `update()` is kept for backwards compatibility and simply
   * delegates to `save()`.
   *
   * @param batchId - Optional batch ID for batch operations
   */
  async update(batchId?: string) {
    return this.save(batchId);
  }

  /**
   * Gets the model instance with all properties and relations populated.
   *
   * @returns The populated model instance
   * @throws Will throw if data retrieval fails
   *
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective, existingId);
   * await recipe.get();
   * console.log(recipe.name, recipe.ingredients);
   * ```
   */
  async get(include?: IncludeMap): Promise<this> {
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    return fetchInstanceData(this, this.#perspective, this.#id, metadata, include);
  }

  /**
   * Removes all links for this instance from the perspective.
   * @param batchId - Optional batch ID for batched operations
   */
  async delete(batchId?: string) {
    await this.#perspective.removeSubject(this, this.#id, batchId);
  }

  /**
   * Returns a fluent query builder for this model.
   *
   * @example
   * ```typescript
   * const top5 = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .order({ rating: "DESC" })
   *   .limit(5)
   *   .run();
   * ```
   */
  static query<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: Query,
  ): ModelQueryBuilder<T> {
    return new ModelQueryBuilder<T>(perspective, this as any, query);
  }

  /**
   * Runs `callback` in an atomic batch; commits on success, aborts + rethrows on failure.
   *
   * @example
   * ```typescript
   * await Ad4mModel.transaction(perspective, async (tx) => {
   *   await post.save(tx.batchId);
   *   await comment.save(tx.batchId);
   * });
   * ```
   */
  static async transaction<T>(
    perspective: PerspectiveProxy,
    callback: (tx: import("./transaction").TransactionContext) => Promise<T>,
  ): Promise<T> {
    return runTransaction(perspective, callback);
  }

  /**
   * Fires `callback` immediately with current results, then on every relevant perspective change.
   *
   * @param options  - Query params plus optional `debounce` (ms) and `onError` handler
   * @param callback - Receives the fresh result set on each delivery
   * @returns `Subscription` with `unsubscribe()` — call it in cleanup to avoid leaks
   *
   * @example
   * ```typescript
   * const sub = Post.subscribe(
   *   perspective,
   *   { where: { published: true }, debounce: 300 },
   *   (posts) => setPosts(posts),
   * );
   * sub.unsubscribe(); // in cleanup
   * ```
   */
  static subscribe<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    options: SubscribeOptions,
    callback: (results: T[]) => void,
  ): Subscription {
    return createSubscription<T>(
      (p, q) => this.findAll(p, q ?? {}),
      () => this.getModelMetadata(),
      perspective,
      options,
      callback,
    );
  }

  /**
   * Generates an `Ad4mModel` subclass from a JSON Schema definition.
   *
   * Predicate resolution order: explicit options → `x-ad4m` in schema → inferred from title/names.
   *
   * @example
   * ```typescript
   * const PersonClass = Ad4mModel.fromJSONSchema(schema, {
   *   name: "Person", namespace: "person://", resolveLanguage: "literal",
   * });
   * ```
   */
  static fromJSONSchema(
    schema: JSONSchema,
    options: JSONSchemaToModelOptions,
  ): typeof Ad4mModel {
    return createModelFromJSONSchema(this, schema, options) as typeof Ad4mModel;
  }
}
