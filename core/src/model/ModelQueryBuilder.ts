/**
 * ModelQueryBuilder — fluent query builder for Ad4mModel.
 *
 * Allows building queries with a chainable interface and either
 * running them once or subscribing to real-time updates.
 */

import type { Ad4mModel } from "./Ad4mModel";
import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import type {
  Where, Order, IncludeMap, Query,
  ResultsWithTotalCount, PaginationResult,
} from "./types";
import { pooledSubscribe } from "./subscription-pool";

/** Query builder for Ad4mModel queries.
 * Allows building queries with a fluent interface and either running them once
 * or subscribing to updates.
 * 
 * @example
 * ```typescript
 * const builder = Recipe.query(perspective)
 *   .where({ category: "Dessert" })
 *   .order({ rating: "DESC" })
 *   .limit(10);
 * 
 * // Run once
 * const recipes = await builder.run();
 * 
 * // Or subscribe to updates
 * await builder.subscribe(recipes => {
 *   console.log("Updated recipes:", recipes);
 * });
 * ```
 */
export class ModelQueryBuilder<T extends Ad4mModel> {
  private perspective: PerspectiveProxy;
  private queryParams: Query = {};
  private modelClassName: string | null = null;
  private ctor: typeof Ad4mModel;
  private currentSubscription?: any;

  constructor(perspective: PerspectiveProxy, ctor: typeof Ad4mModel, query?: Query) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
  }

  /**
   * Disposes of the current subscription if one exists.
   * 
   * This method:
   * 1. Stops the keepalive signals to the subscription
   * 2. Unsubscribes from GraphQL subscription updates
   * 3. Notifies the backend to clean up subscription resources
   * 4. Clears the subscription reference
   * 
   * You should call this method when you're done with a subscription
   * to prevent memory leaks and ensure proper cleanup.
   */
  dispose() {
    if (this.currentSubscription) {
      this.currentSubscription.dispose();
      this.currentSubscription = undefined;
    }
  }

  /**
   * Adds where conditions to the query.
   * 
   * @param conditions - The conditions to filter by
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .where({
   *   category: "Dessert",
   *   rating: { gt: 4 },
   *   tags: ["vegan", "quick"],
   *   published: true
   * })
   * ```
   */
  where(conditions: Where): ModelQueryBuilder<T> {
    this.queryParams.where = conditions;
    return this;
  }

  /**
   * Sets the order for the query results.
   * 
   * @param orderBy - The ordering criteria
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .order({ createdAt: "DESC" })
   * ```
   */
  order(orderBy: Order): ModelQueryBuilder<T> {
    this.queryParams.order = orderBy;
    return this;
  }

  /**
   * Sets the maximum number of results to return.
   * 
   * @param limit - Maximum number of results
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .limit(10)
   * ```
   */
  limit(limit: number): ModelQueryBuilder<T> {
    this.queryParams.limit = limit;
    return this;
  }

  /**
   * Sets the number of results to skip.
   * 
   * @param offset - Number of results to skip
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .offset(20) // Skip first 20 results
   * ```
   */
  offset(offset: number): ModelQueryBuilder<T> {
    this.queryParams.offset = offset;
    return this;
  }

  /**
   * Scopes the query to instances linked from a parent.
   *
   * The predicate is resolved in order of precedence:
   * 1. **Instance only** — the parent's constructor is used as the model
   *    class; its relation metadata is scanned for a relation whose
   *    `target()` matches the queried model class.
   * 2. **Instance + options with `field`** — direct field-name lookup on
   *    the parent model's relation metadata (disambiguates when a parent
   *    has multiple relations targeting the same child class).
   * 3. **String id + model class** — same metadata scan (or field lookup if
   *    options include `field`).
   * 4. **String id + string predicate** — raw escape hatch, no metadata lookup.
   *
   * Passing a plain string id with no second argument throws because the
   * predicate cannot be resolved without a model class.
   *
   * @param idOrInstance - The parent's expression URI **or** an Ad4mModel instance
   * @param modelOrPredicate - A model class (predicate auto-resolved) **or** a raw predicate string
   * @param options - Optional settings: `field` for direct relation-name lookup
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * // Instance — predicate auto-resolved from Cookbook's @HasMany(() => Recipe)
   * Recipe.query(perspective).parent(cookbook).get();
   *
   * // Instance + field — disambiguate when parent has multiple relations to same child
   * Recipe.query(perspective).parent(cookbook, { field: "recipes" }).get();
   *
   * // String id + model class
   * Recipe.query(perspective).parent(cookbookId, Cookbook).get();
   *
   * // String id + model class + field
   * Recipe.query(perspective).parent(cookbookId, Cookbook, { field: "recipes" }).get();
   *
   * // String id + raw predicate (escape hatch)
   * Recipe.query(perspective).parent(cookbookId, "cookbook://recipe").get();
   * ```
   */
  parent(idOrInstance: string | Ad4mModel, modelOrPredicate?: typeof Ad4mModel | string | { field: string }, options?: { field?: string }): ModelQueryBuilder<T> {
    const id = typeof idOrInstance === 'string' ? idOrInstance : idOrInstance.id;

    // Handle options-object as second arg: parent(instance, { field: "recipes" })
    if (typeof modelOrPredicate === 'object' && modelOrPredicate !== null && !('prototype' in modelOrPredicate)) {
      if (typeof idOrInstance === 'string') {
        throw new Error(
          'parent() called with a string id and options object requires a model class as second argument',
        );
      }
      const model = idOrInstance.constructor as typeof Ad4mModel;
      this.queryParams.parent = { id, model, field: (modelOrPredicate as { field: string }).field };
      return this;
    }

    const field = options?.field;

    if (typeof modelOrPredicate === 'string') {
      // Raw predicate string → raw form of ParentScope
      this.queryParams.parent = { id, predicate: modelOrPredicate };
    } else if (typeof modelOrPredicate === 'function') {
      // Model class → model form of ParentScope
      this.queryParams.parent = { id, model: modelOrPredicate, ...(field && { field }) };
    } else if (typeof idOrInstance !== 'string') {
      // Ad4mModel instance — derive model class from constructor
      this.queryParams.parent = { id, model: idOrInstance.constructor as typeof Ad4mModel, ...(field && { field }) };
    } else {
      throw new Error(
        'parent() called with a string id requires a second argument: either a model class or a predicate string',
      );
    }
    return this;
  }

  /**
   * Specifies which properties to include in the results.
   * 
   * @param properties - Array of property names to include
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .properties(["name", "description", "rating"])
   * ```
   */
  properties(properties: string[]): ModelQueryBuilder<T> {
    this.queryParams.properties = properties;
    return this;
  }

  /**
   * Opts in to evaluating SPARQL property getters during collection hydration.
   *
   * By default, collection queries skip getter evaluation for performance.
   * Call `.deepQuery()` when you need getter-backed properties in the result set.
   *
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * const messages = await Message.query(perspective)
   *   .parent(channel)
   *   .deepQuery()
   *   .limit(30)
   *   .get();
   * // messages[i].replyingTo is populated
   * ```
   */
  deepQuery(): ModelQueryBuilder<T> {
    (this.queryParams as any).deepQuery = true;
    return this;
  }

  /**
   * Specifies which relations to eager-load (hydrate into model instances).
   *
   * Without `include`, relation fields contain raw expression URIs (strings).
   * With `include`, the URIs are resolved into fully-hydrated model instances
   * using the `target` class declared in the relation decorator.
   *
   * Supports nested includes for multi-level eager loading.
   *
   * @param map - An IncludeMap describing which relations to hydrate
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * // Hydrate comments one level deep
   * const recipes = await Recipe.query(perspective)
   *   .include({ comments: true })
   *   .run();
   * // recipe.comments is now Comment[] (model instances), not string[]
   *
   * // Nested: hydrate comments AND each comment's author
   * const recipes = await Recipe.query(perspective)
   *   .include({ comments: { author: true } })
   *   .run();
   * ```
   */
  include(map: IncludeMap): ModelQueryBuilder<T> {
    this.queryParams.include = map;
    return this;
  }

  overrideModelClassName(className: string): ModelQueryBuilder<T> {
    this.modelClassName = className;
    return this;
  }

  /**
   * Sets the query engine to use.
   * @deprecated Prolog engine has been removed. All queries use SPARQL/Rust.
   */
  engine(_eng: 'sparql' | 'prolog'): ModelQueryBuilder<T> {
    return this;
  }

  /**
   * Executes the query once and returns the results.
   * 
   * @returns Array of matching entities
   * 
   * @example
   * ```typescript
   * const recipes = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .get();
   * ```
   */
  async get(): Promise<T[]> {
    return await this.executeSparqlQuery();
  }

  /**
   * Executes the query once using SPARQL and returns the results.
   */
  async getSparql(): Promise<T[]> {
    return this.executeSparqlQuery();
  }

  /**
   * Shared query execution logic — routes through the executor-side modelQuery endpoint.
   */
  private async executeSparqlQuery(): Promise<T[]> {
    const { results } = await (this.ctor as any).executeModelQuery(this.perspective, this.queryParams, this.modelClassName);
    return results;
  }

  /**
   * Returns the first matching instance, or `null` if none match.
   *
   * Internally sets `limit: 1` and delegates to `get()`.
   *
   * @returns The first matching instance, or `null`
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.query(perspective)
   *   .where({ name: "Pasta" })
   *   .first();
   * ```
   */
  async first(): Promise<T | null> {
    this.queryParams.limit = 1;
    const results = await this.get();
    return results[0] ?? null;
  }

  /**
   * Subscribes to the query and receives updates when results change.
   *
   * This method:
   * 1. Creates and initializes a SPARQL live query subscription (default)
   * 2. Sets up the callback to process future updates
   * 3. Returns the initial results immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param callback - Function to call with updated results
   * @returns Initial results array
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ status: "cooking" });
   *
   * const initialRecipes = await builder.subscribe(recipes => {
   *   console.log("Updated recipes:", recipes);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   */
  async subscribe(callback: (results: T[]) => void): Promise<T[]> {
    // Clean up any existing subscription
    this.dispose();

    const ctor = this.ctor;
    const sparqlQuery = await ctor.queryToSPARQL(this.perspective, this.queryParams);

    // Track last emitted result fingerprint to suppress duplicate callbacks
    let lastResultFingerprint: string | null = null;

    const buildFingerprint = (results: any[]) => {
        if (results.length === 0) return '0:';
        return JSON.stringify(results, (_, v) =>
            typeof v === 'function' ? undefined : v
        );
    };

    // On each subscription update, re-execute via Rust endpoint instead of
    // JS-side hydration. The SPARQL subscription is only a change-detection
    // signal — all actual query work (hydration, filtering, sorting, pagination)
    // is done Rust-side.
    const hydrate = async (_rawResult: any) => {
        const { results } = await (ctor as any).executeModelQuery(this.perspective, this.queryParams, this.modelClassName);
        return results;
    };

    const pooled = await pooledSubscribe(
        this.perspective,
        sparqlQuery,
        hydrate,
        (hydratedResults: T[]) => {
            const fp = buildFingerprint(hydratedResults);
            if (fp === lastResultFingerprint) return;
            lastResultFingerprint = fp;
            callback(hydratedResults);
        },
    );

    // Store dispose function as subscription
    this.currentSubscription = { dispose: pooled.dispose };

    const initialResults = pooled.initialResult as T[];
    lastResultFingerprint = buildFingerprint(initialResults);
    return initialResults;
  }

  /**
   * Subscribes to the query and receives updates using SPARQL.
   */
  async subscribeSparql(callback: (results: T[]) => void): Promise<T[]> {
    this.dispose();

    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    const processResults = async (_result: any) => {
      const { results } = await (this.ctor as any).executeModelQuery(this.perspective, this.queryParams, this.modelClassName);
      callback(results as T[]);
    };

    this.currentSubscription.onResult(processResults);

    const { results } = await (this.ctor as any).executeModelQuery(this.perspective, this.queryParams, this.modelClassName);
    return results as T[];
  }

  /**
   * Gets the total count of matching entities.
   * 
   * @returns Total count
   * 
   * @example
   * ```typescript
   * const totalDesserts = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .count();
   * ```
   */
  async count(): Promise<number> {
    const { totalCount } = await (this.ctor as any).executeModelQuery(this.perspective, { ...this.queryParams, limit: 0 }, this.modelClassName);
    return totalCount;
  }

  /**
   * Gets the total count of matching entities using SPARQL.
   * Delegates to count().
   */
  async countSparql(): Promise<number> {
    return await this.count();
  }

  /**
   * Subscribes to count updates for matching entities.
   *
   * This method:
   * 1. Creates and initializes a SPARQL live query subscription for the count (default)
   * 2. Sets up the callback to process future count updates
   * 3. Returns the initial count immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param callback - Function to call with updated count
   * @returns Initial count
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ status: "active" });
   *
   * const initialCount = await builder.countSubscribe(count => {
   *   console.log("Active items:", count);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   */
  async countSubscribe(callback: (count: number) => void): Promise<number> {
    // Clean up any existing subscription
    this.dispose();

    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    const processResults = async (_result: any) => {
      const { totalCount } = await (this.ctor as any).executeModelQuery(
        this.perspective, { ...this.queryParams, limit: 0 }, this.modelClassName
      );
      callback(totalCount);
    };

    this.currentSubscription.onResult(processResults);
    const { totalCount } = await (this.ctor as any).executeModelQuery(
      this.perspective, { ...this.queryParams, limit: 0 }, this.modelClassName
    );
    return totalCount;
  }

  /**
   * Subscribes to count updates using SPARQL.
   */
  async countSubscribeSparql(callback: (count: number) => void): Promise<number> {
    this.dispose();

    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    const processResults = async (_result: any) => {
      const { totalCount } = await (this.ctor as any).executeModelQuery(
        this.perspective, { ...this.queryParams, limit: 0 }, this.modelClassName
      );
      callback(totalCount);
    };

    this.currentSubscription.onResult(processResults);
    const { totalCount } = await (this.ctor as any).executeModelQuery(
      this.perspective, { ...this.queryParams, limit: 0 }, this.modelClassName
    );
    return totalCount;
  }

  /**
   * Gets a page of results with pagination metadata.
   * 
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @returns Paginated results with metadata
   * 
   * @example
   * ```typescript
   * const page = await Recipe.query(perspective)
   *   .where({ category: "Main" })
   *   .paginate(10, 1);
   * console.log(`Page ${page.pageNumber}, ${page.results.length} of ${page.totalCount}`);
   * ```
   */
  async paginate(pageSize: number, pageNumber: number): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(this.queryParams || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const { results, totalCount } = await (this.ctor as any).executeModelQuery(this.perspective, paginationQuery, this.modelClassName);
    return { results, totalCount, pageSize, pageNumber };
  }

  /**
   * Gets a page of results using SPARQL.
   * Delegates to paginate().
   */
  async paginateSparql(pageSize: number, pageNumber: number): Promise<PaginationResult<T>> {
    return await this.paginate(pageSize, pageNumber);
  }

  /**
   * Subscribes to paginated results updates.
   *
   * This method:
   * 1. Creates and initializes a SPARQL live query subscription for the paginated results (default)
   * 2. Sets up the callback to process future page updates
   * 3. Returns the initial page immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param callback - Function to call with updated pagination results
   * @returns Initial pagination results
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ category: "Main" });
   *
   * const initialPage = await builder.paginateSubscribe(10, 1, page => {
   *   console.log("Updated page:", page.results);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   */
  async paginateSubscribe(
    pageSize: number, 
    pageNumber: number, 
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    // Clean up any existing subscription
    this.dispose();

    // Subscribe to the FULL result set (no LIMIT/OFFSET) so the subscription
    // detects changes anywhere in the dataset (e.g. new items beyond current page).
    const subscriptionParams = { ...(this.queryParams || {}) };
    delete subscriptionParams.limit;
    delete subscriptionParams.offset;
    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, subscriptionParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    // Build the paginated query for Rust endpoint
    const paginatedQuery = {
      ...(this.queryParams || {}),
      limit: pageSize,
      offset: pageSize * (pageNumber - 1),
    };

    const processResults = async (_result: any) => {
      // Get count via separate call with limit:0
      const countQuery = { ...(this.queryParams || {}), limit: 0 };
      const { totalCount } = await (this.ctor as any).executeModelQuery(this.perspective, countQuery, this.modelClassName);
      const { results } = await (this.ctor as any).executeModelQuery(this.perspective, paginatedQuery, this.modelClassName);
      callback({ results, totalCount, pageSize, pageNumber });
    };

    this.currentSubscription.onResult(processResults);

    const countQuery = { ...(this.queryParams || {}), limit: 0 };
    const { totalCount } = await (this.ctor as any).executeModelQuery(this.perspective, countQuery, this.modelClassName);
    const { results } = await (this.ctor as any).executeModelQuery(this.perspective, paginatedQuery, this.modelClassName);
    return { results, totalCount, pageSize, pageNumber };
  }

  /**
   * Subscribes to paginated results updates using SPARQL.
   * Delegates to paginateSubscribe().
   */
  async paginateSubscribeSparql(
    pageSize: number,
    pageNumber: number,
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    return await this.paginateSubscribe(pageSize, pageNumber, callback);
  }
}
