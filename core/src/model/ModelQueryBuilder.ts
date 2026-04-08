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
  AllInstancesResult, ResultsWithTotalCount, PaginationResult,
} from "./types";
import { groupSPARQLResults } from "./query-sparql";

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
  private engineFlag: 'sparql' | 'prolog' = 'sparql';

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
   */
  engine(eng: 'sparql' | 'prolog'): ModelQueryBuilder<T> {
    this.engineFlag = eng;
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
    let results: T[];
    if (this.engineFlag === 'sparql') {
      results = await this.executeSparqlQuery();
    } else {
      const query = await this.ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
      const result = await this.perspective.infer(query);
      ({ results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result) as { results: T[] });
    }

    return results;
  }

  /**
   * Executes the query once using SPARQL and returns the results.
   */
  async getSparql(): Promise<T[]> {
    return this.executeSparqlQuery();
  }

  /**
   * Shared SPARQL query execution logic used by both get() and getSparql().
   */
  private async executeSparqlQuery(): Promise<T[]> {
    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    const rawResult = await this.perspective.querySparql(sparqlQuery);
    const { results } = await this.processSparqlResult(rawResult);
    return results;
  }

  /**
   * Groups raw SPARQL results and hydrates them into model instances.
   * Centralises the repeated groupSPARQLResults → instancesFromQueryResult pipeline.
   */
  private async processSparqlResult(rawResult: any, queryOverride?: Query): Promise<{ results: T[], totalCount: number }> {
    const grouped = groupSPARQLResults(Array.isArray(rawResult) ? rawResult : []);
    return await this.ctor.instancesFromQueryResult(this.perspective, queryOverride || this.queryParams, grouped) as { results: T[], totalCount: number };
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
   * @remarks
   * By default, this uses SPARQL live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSPARQL(false)`.
   */
  async subscribe(callback: (results: T[]) => void): Promise<T[]> {
    // Clean up any existing subscription
    this.dispose();

    const ctor = this.ctor;

    if (this.engineFlag === 'sparql') {
        const sparqlQuery = await ctor.queryToSPARQL(this.perspective, this.queryParams);
        // TODO (3.6 - Subscription predicate filtering): Extract predicates used in the
        // SPARQL query and pass them as hints to subscribeQuery so the Rust-side
        // subscription can filter link-change notifications by predicate, avoiding
        // unnecessary re-queries for unrelated link changes. This requires Rust-side
        // support in PerspectiveProxy.subscribeQuery to accept a predicate whitelist.
        this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

        // Track last emitted result fingerprint to suppress duplicate callbacks
        // when the raw SPARQL result changes but the JS-filtered set doesn't
        // (e.g. a non-matching record was added/removed).
        let lastResultFingerprint: string | null = null;

        const buildFingerprint = (results: any[]) => {
            // Lightweight fingerprint: IDs + count + timestamps.
            // Avoids JSON.stringify of all properties for performance.
            // Catches additions, removals, and timestamp-based updates.
            if (results.length === 0) return '0:';
            const ids = results.map((r: any) => r.id || '').sort().join(',');
            const ts = results.map((r: any) => r.updatedAt || r.timestamp || '').join(',');
            return `${results.length}:${ids}:${ts}`;
        };

        const processResults = async (result: any) => {
            const { results } = await this.processSparqlResult(result);
            const fp = buildFingerprint(results);
            if (fp === lastResultFingerprint) return; // filtered set unchanged — skip callback
            lastResultFingerprint = fp;
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        
        // Process initial result
        const { results } = await this.processSparqlResult(this.currentSubscription.result);
        lastResultFingerprint = buildFingerprint(results);
        // Initial results returned via Promise only — callback is for subsequent updates
        return results as T[];
    } else {
        const query = await ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
        this.currentSubscription = await this.perspective.subscribeInfer(query);

        const processResults = async (result: AllInstancesResult) => {
            const { results } = await ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        const { results } = await ctor.instancesFromPrologResult(
            this.perspective,
            this.queryParams,
            this.currentSubscription.result
        );
        // Initial results returned via Promise only — callback is for subsequent updates
        return results as T[];
    }
  }

  /**
   * Subscribes to the query and receives updates using SPARQL.
   */
  async subscribeSparql(callback: (results: T[]) => void): Promise<T[]> {
    this.dispose();

    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    const processResults = async (result: any) => {
      const { results } = await this.processSparqlResult(result);
      callback(results as T[]);
    };

    this.currentSubscription.onResult(processResults);

    const { results } = await this.processSparqlResult(this.currentSubscription.result);
    // Initial results returned via Promise only — callback is for subsequent updates
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
    if (this.engineFlag === 'sparql') {
      const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
      const rawResult = await this.perspective.querySparql(sparqlQuery);
      const { totalCount } = await this.processSparqlResult(rawResult);
      return totalCount;
    } else {
      const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
      const result = await this.perspective.infer(query);
      return result?.[0]?.TotalCount || 0;
    }
  }

  /**
   * Gets the total count of matching entities using SPARQL.
   * Delegates to count() with SPARQL engine forced.
   */
  async countSparql(): Promise<number> {
    const prev = this.engineFlag;
    this.engineFlag = 'sparql';
    try {
      return await this.count();
    } finally {
      this.engineFlag = prev;
    }
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
   * @remarks
   * By default, this uses SPARQL live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSPARQL(false)`.
   */
  async countSubscribe(callback: (count: number) => void): Promise<number> {
    // Clean up any existing subscription
    this.dispose();

    if (this.engineFlag === 'sparql') {
      const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
      this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

      const processResults = async (result: any) => {
        const { totalCount } = await this.processSparqlResult(result);
        callback(totalCount);
      };

      this.currentSubscription.onResult(processResults);
      const { totalCount } = await this.processSparqlResult(this.currentSubscription.result);
      // Initial count returned via Promise — callback for updates only
      return totalCount;
    } else {
      const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
      this.currentSubscription = await this.perspective.subscribeInfer(query);

      const processResults = async (result: any) => {
        const newCount = result?.[0]?.TotalCount || 0;
        callback(newCount);
      };

      this.currentSubscription.onResult(processResults);
      const initialCount = this.currentSubscription.result?.[0]?.TotalCount || 0;
      // Initial count returned via Promise — callback for subsequent updates only
      return initialCount;
    }
  }

  /**
   * Subscribes to count updates using SPARQL.
   */
  async countSubscribeSparql(callback: (count: number) => void): Promise<number> {
    this.dispose();

    const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, this.queryParams);
    this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

    const processResults = async (result: any) => {
      const { totalCount } = await this.processSparqlResult(result);
      callback(totalCount);
    };

    this.currentSubscription.onResult(processResults);
    const { totalCount } = await this.processSparqlResult(this.currentSubscription.result);
    // Initial count returned via Promise — callback for updates only
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
    if (this.engineFlag === 'sparql') {
      const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, paginationQuery);
      const result = await this.perspective.querySparql(sparqlQuery);
      const { results, totalCount } = await this.processSparqlResult(result, paginationQuery) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    } else {
      const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
      const result = await this.perspective.infer(prologQuery);
      const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, result)) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    }
  }

  /**
   * Gets a page of results using SPARQL.
   * Delegates to paginate() with SPARQL engine forced.
   */
  async paginateSparql(pageSize: number, pageNumber: number): Promise<PaginationResult<T>> {
    const prev = this.engineFlag;
    this.engineFlag = 'sparql';
    try {
      return await this.paginate(pageSize, pageNumber);
    } finally {
      this.engineFlag = prev;
    }
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
   * @remarks
   * By default, this uses SPARQL live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSPARQL(false)`.
   */
  async paginateSubscribe(
    pageSize: number, 
    pageNumber: number, 
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    // Clean up any existing subscription
    this.dispose();

    const paginationQuery = { ...(this.queryParams || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };

    if (this.engineFlag === 'sparql') {
      const sparqlQuery = await this.ctor.queryToSPARQL(this.perspective, paginationQuery);
      this.currentSubscription = await this.perspective.subscribeQuery(sparqlQuery);

      const processResults = async (result: any) => {
        const { results, totalCount } = await this.processSparqlResult(result, paginationQuery) as ResultsWithTotalCount<T>;
        callback({ results, totalCount, pageSize, pageNumber });
      };

      this.currentSubscription.onResult(processResults);
      const { results, totalCount } = await this.processSparqlResult(this.currentSubscription.result, paginationQuery) as ResultsWithTotalCount<T>;
      const initialPage = { results, totalCount, pageSize, pageNumber };
      // Initial page returned via Promise — callback for subsequent updates only
      return initialPage;
    } else {
      const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
      this.currentSubscription = await this.perspective.subscribeInfer(prologQuery);

      const processResults = async (r: AllInstancesResult) => {
        const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, r)) as ResultsWithTotalCount<T>;
        callback({ results, totalCount, pageSize, pageNumber });
      };

      this.currentSubscription.onResult(processResults);
      const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, this.currentSubscription.result)) as ResultsWithTotalCount<T>;
      const initialPrologPage = { results, totalCount, pageSize, pageNumber };
      // Initial page returned via Promise — callback for subsequent updates only
      return initialPrologPage;
    }
  }

  /**
   * Subscribes to paginated results updates using SPARQL.
   * Delegates to paginateSubscribe() with SPARQL engine forced.
   */
  async paginateSubscribeSparql(
    pageSize: number,
    pageNumber: number,
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    const prev = this.engineFlag;
    this.engineFlag = 'sparql';
    try {
      return await this.paginateSubscribe(pageSize, pageNumber, callback);
    } finally {
      this.engineFlag = prev;
    }
  }
}
