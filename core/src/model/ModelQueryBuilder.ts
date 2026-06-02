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
  TypedWhere, TypedOrder, TypedIncludeMap, PropertyKeysOf,
} from "./types";

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
   * 2. Unsubscribes from subscription updates
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
  where(conditions: TypedWhere<T>): ModelQueryBuilder<T> {
    this.queryParams.where = conditions as Where;
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
  order(orderBy: TypedOrder<T>): ModelQueryBuilder<T> {
    this.queryParams.order = orderBy as Order;
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
  properties(properties: PropertyKeysOf<T>[] | string[]): ModelQueryBuilder<T> {
    this.queryParams.properties = properties as string[];
    return this;
  }

  /**
   * Controls whether SPARQL property getters are evaluated during hydration.
   *
   * By default, collection queries evaluate property getters (deepQuery=true).
   * Call `.deepQuery(false)` to skip getter evaluation for performance.
   *
   * @param enabled - Whether to evaluate property getters (default: true)
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * const messages = await Message.query(perspective)
   *   .parent(channel)
   *   .deepQuery(false)  // skip property getters for performance
   *   .limit(30)
   *   .get();
   * ```
   */
  deepQuery(enabled: boolean = true): ModelQueryBuilder<T> {
    this.queryParams.deepQuery = enabled;
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
  include(map: TypedIncludeMap<T>): ModelQueryBuilder<T> {
    this.queryParams.include = map as IncludeMap;
    return this;
  }

  overrideModelClassName(className: string): ModelQueryBuilder<T> {
    this.modelClassName = className;
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
    const savedLimit = this.queryParams.limit;
    this.queryParams.limit = 1;
    const results = await this.get();
    this.queryParams.limit = savedLimit;
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

    // Build the model query params (className, queryJson).  The executor
    // resolves the shape from the perspective's SHACL triples.
    const { className, queryJson } = (ctor as any).prepareModelQueryParams(
      this.queryParams, this.modelClassName
    );

    // Register model subscription via Rust — this builds trigger SPARQL internally,
    // registers the subscription, and runs the initial query in one call.
    const { subscriptionId, result: initialModelResult } = await this.perspective.modelSubscribe(
      className, queryJson
    );

    // Convert JSON instances to model class instances
    const parseResults = (raw: any): T[] => {
      return (ctor as any).parseModelResult(this.perspective, raw, this.queryParams.include, this.queryParams.properties);
    };

    // Resolve non-literal (file-language) properties — handles both raw URI strings
    // and already-resolved FileData objects returned by the Rust executor.
    const resolveAndReturn = async (instances: T[]): Promise<T[]> => {
      await (ctor as any).resolveNonLiteralProps(this.perspective, instances);
      return instances;
    };

    // Parse initial results (with non-literal resolution)
    const initialResults = await resolveAndReturn(parseResults(initialModelResult));

    // Track last emitted result fingerprint to suppress duplicate callbacks
    let lastResultFingerprint: string | null = null;
    const buildFingerprint = (results: any[]) => {
      if (results.length === 0) return '0:';
      return JSON.stringify(results, (_, v) =>
        typeof v === 'function' ? undefined : v
      );
    };
    lastResultFingerprint = buildFingerprint(initialResults);

    // Listen for subscription updates via the same GraphQL subscription channel.
    // When Rust re-runs the model query and finds changed results, it pushes them.
    const unsubscribe = this.perspective.client.subscribeToQueryUpdates(
      subscriptionId,
      (rawResult: any) => {
        try {
          const parsed = parseResults(rawResult);
          console.debug(`[ModelQueryBuilder.subscribe] Update received for ${subscriptionId}: ${parsed.length} instances`);
          resolveAndReturn(parsed).then((results) => {
            const fp = buildFingerprint(results);
            if (fp === lastResultFingerprint) {
              console.debug(`[ModelQueryBuilder.subscribe] Fingerprint unchanged, skipping callback`);
              return;
            }
            console.debug(`[ModelQueryBuilder.subscribe] Fingerprint changed, calling callback with ${results.length} results`);
            lastResultFingerprint = fp;
            callback(results);
          }).catch((e) => {
            console.error('Model subscription update resolve error:', e);
          });
        } catch (e) {
          console.error('Model subscription update parse error:', e);
        }
      },
    );

    // Set up keepalive with recovery — uses setTimeout loop so we can
    // break out and resubscribe when the server evicts the subscription.
    let disposed = false;
    let keepaliveTimer: ReturnType<typeof setTimeout> | undefined;
    let resubscribeAttempts = 0;
    const MAX_RESUBSCRIBE_ATTEMPTS = 5;
    const keepaliveLoop = async () => {
      if (disposed) return;
      try {
        await this.perspective.client.keepAliveQuery(this.perspective.uuid, subscriptionId);
        resubscribeAttempts = 0; // Reset on success
      } catch (e: any) {
        console.warn(`Model subscription keepalive failed for ${subscriptionId}:`, e);
        if (!disposed && resubscribeAttempts < MAX_RESUBSCRIBE_ATTEMPTS) {
          resubscribeAttempts++;
          const backoffMs = Math.min(1000 * Math.pow(2, resubscribeAttempts), 60000);
          console.warn(`Resubscribing (attempt ${resubscribeAttempts}/${MAX_RESUBSCRIBE_ATTEMPTS}, backoff ${backoffMs}ms)...`);
          await new Promise(r => setTimeout(r, backoffMs));
          try {
            unsubscribe();
            await this.subscribe(callback);
          } catch (resubErr) {
            console.error('Model subscription resubscribe failed:', resubErr);
          }
        } else if (resubscribeAttempts >= MAX_RESUBSCRIBE_ATTEMPTS) {
          console.error(`Model subscription ${subscriptionId}: max resubscribe attempts reached, giving up.`);
        }
        return; // new subscription owns its own keepalive loop
      }
      if (!disposed) {
        keepaliveTimer = setTimeout(keepaliveLoop, 30000);
      }
    };
    keepaliveTimer = setTimeout(keepaliveLoop, 30000);

    // Store dispose function
    this.currentSubscription = {
      dispose: () => {
        disposed = true;
        if (keepaliveTimer) clearTimeout(keepaliveTimer);
        unsubscribe();
        this.perspective.client.disposeQuerySubscription(this.perspective.uuid, subscriptionId).catch(() => {});
      },
    };

    // Take snapshots for dirty tracking
    for (const inst of initialResults) {
      (inst as any).takeSnapshot?.(this.queryParams.include);
    }

    return initialResults;
  }

  /**
   * Subscribes to the query and receives updates using SPARQL.
   * @deprecated Use subscribe() instead — now routes through Rust model subscription.
   */
  async subscribeSparql(callback: (results: T[]) => void): Promise<T[]> {
    return this.subscribe(callback);
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

    const countParams = { ...this.queryParams, limit: 0 };
    const { className, queryJson } = (this.ctor as any).prepareModelQueryParams(
      countParams, this.modelClassName
    );

    const { subscriptionId, result: initialModelResult } = await this.perspective.modelSubscribe(
      className, queryJson
    );

    const parseCount = (raw: any): number => {
      const data = typeof raw === 'string' ? JSON.parse(raw) : raw;
      return data.totalCount ?? 0;
    };

    const initialCount = parseCount(initialModelResult);

    const unsubscribe = this.perspective.client.subscribeToQueryUpdates(
      subscriptionId,
      (rawResult: any) => {
        try {
          callback(parseCount(rawResult));
        } catch (e) {
          console.error('Count subscription update parse error:', e);
        }
      },
    );

    let disposed = false;
    let keepaliveTimer: ReturnType<typeof setTimeout> | undefined;
    let resubscribeAttempts = 0;
    const MAX_RESUBSCRIBE_ATTEMPTS = 5;
    const keepaliveLoop = async () => {
      if (disposed) return;
      try {
        await this.perspective.client.keepAliveQuery(this.perspective.uuid, subscriptionId);
        resubscribeAttempts = 0;
      } catch (e: any) {
        console.warn(`Count subscription keepalive failed for ${subscriptionId}:`, e);
        if (!disposed && resubscribeAttempts < MAX_RESUBSCRIBE_ATTEMPTS) {
          resubscribeAttempts++;
          const backoffMs = Math.min(1000 * Math.pow(2, resubscribeAttempts), 60000);
          console.warn(`Count resubscribing (attempt ${resubscribeAttempts}/${MAX_RESUBSCRIBE_ATTEMPTS}, backoff ${backoffMs}ms)...`);
          await new Promise(r => setTimeout(r, backoffMs));
          try {
            unsubscribe();
            await this.countSubscribe(callback);
          } catch (resubErr) {
            console.error('Count subscription resubscribe failed:', resubErr);
          }
        } else if (resubscribeAttempts >= MAX_RESUBSCRIBE_ATTEMPTS) {
          console.error(`Count subscription ${subscriptionId}: max resubscribe attempts reached, giving up.`);
        }
        return;
      }
      if (!disposed) {
        keepaliveTimer = setTimeout(keepaliveLoop, 30000);
      }
    };
    keepaliveTimer = setTimeout(keepaliveLoop, 30000);

    this.currentSubscription = {
      dispose: () => {
        disposed = true;
        if (keepaliveTimer) clearTimeout(keepaliveTimer);
        unsubscribe();
        this.perspective.client.disposeQuerySubscription(this.perspective.uuid, subscriptionId).catch(() => {});
      },
    };

    return initialCount;
  }

  /**
   * Subscribes to count updates using SPARQL.
   * @deprecated Use countSubscribe() instead — now routes through Rust model subscription.
   */
  async countSubscribeSparql(callback: (count: number) => void): Promise<number> {
    return this.countSubscribe(callback);
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

    const ctor = this.ctor;

    // Subscribe to the full result set (no limit/offset) so the subscription
    // detects changes anywhere in the dataset. Rust builds trigger SPARQL
    // from the shape predicates.
    const subscriptionParams = { ...(this.queryParams || {}) };
    delete subscriptionParams.limit;
    delete subscriptionParams.offset;
    const { className, queryJson } = (ctor as any).prepareModelQueryParams(
      subscriptionParams, this.modelClassName
    );

    const { subscriptionId } = await this.perspective.modelSubscribe(
      className, queryJson
    );

    // Build the paginated query for Rust endpoint (count: true fetches both results and totalCount in one call)
    const paginatedQuery = {
      ...(this.queryParams || {}),
      limit: pageSize,
      offset: pageSize * (pageNumber - 1),
      count: true,
    };

    const processResults = async () => {
      const { results, totalCount } = await (ctor as any).executeModelQuery(this.perspective, paginatedQuery, this.modelClassName);
      callback({ results, totalCount, pageSize, pageNumber });
    };

    const unsubscribe = this.perspective.client.subscribeToQueryUpdates(
      subscriptionId,
      (rawResult: any) => {
        console.debug(`[ModelQueryBuilder.paginateSubscribe] Update received for ${subscriptionId}, re-fetching paginated data...`);
        processResults().catch(e => console.error('Paginate subscription error:', e));
      },
    );

    let disposed = false;
    let keepaliveTimer: ReturnType<typeof setTimeout> | undefined;
    let resubscribeAttempts = 0;
    const MAX_RESUBSCRIBE_ATTEMPTS = 5;
    const keepaliveLoop = async () => {
      if (disposed) return;
      try {
        await this.perspective.client.keepAliveQuery(this.perspective.uuid, subscriptionId);
        resubscribeAttempts = 0;
      } catch (e: any) {
        console.warn(`Paginate subscription keepalive failed for ${subscriptionId}:`, e);
        if (!disposed && resubscribeAttempts < MAX_RESUBSCRIBE_ATTEMPTS) {
          resubscribeAttempts++;
          const backoffMs = Math.min(1000 * Math.pow(2, resubscribeAttempts), 60000);
          console.warn(`Paginate resubscribing (attempt ${resubscribeAttempts}/${MAX_RESUBSCRIBE_ATTEMPTS}, backoff ${backoffMs}ms)...`);
          await new Promise(r => setTimeout(r, backoffMs));
          try {
            unsubscribe();
            await this.paginateSubscribe(pageSize, pageNumber, callback);
          } catch (resubErr) {
            console.error('Paginate subscription resubscribe failed:', resubErr);
          }
        } else if (resubscribeAttempts >= MAX_RESUBSCRIBE_ATTEMPTS) {
          console.error(`Paginate subscription ${subscriptionId}: max resubscribe attempts reached, giving up.`);
        }
        return;
      }
      if (!disposed) {
        keepaliveTimer = setTimeout(keepaliveLoop, 30000);
      }
    };
    keepaliveTimer = setTimeout(keepaliveLoop, 30000);

    this.currentSubscription = {
      dispose: () => {
        disposed = true;
        if (keepaliveTimer) clearTimeout(keepaliveTimer);
        unsubscribe();
        this.perspective.client.disposeQuerySubscription(this.perspective.uuid, subscriptionId).catch(() => {});
      },
    };

    // Initial fetch (single call with count: true)
    const { results, totalCount } = await (ctor as any).executeModelQuery(this.perspective, paginatedQuery, this.modelClassName);
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
