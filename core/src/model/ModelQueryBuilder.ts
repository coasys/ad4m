/**
 * ModelQueryBuilder — fluent query builder for Ad4mModel.
 *
 * Allows building queries with a chainable interface and either
 * running them once or subscribing to real-time updates.
 */

import type { Ad4mModel } from "./Ad4mModel";
import type { LinkStatus, PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { requireTotalCount } from "./query-common";
import type {
  Where, Order, IncludeMap, Query,
  Page, PaginationResult, CursorPage,
  TypedQueryWhere, TypedOrder, TypedIncludeMap, PropertyKeysOf,
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
 * ```
 */
export class ModelQueryBuilder<T extends Ad4mModel> {
  private perspective: PerspectiveProxy;
  private queryParams: Query = {};
  private modelClassName: string | null = null;
  private ctor: typeof Ad4mModel;

  /** Set for a query over several classes (`Ad4mModel.queryOf`, #1238). */
  private unionClasses?: (typeof Ad4mModel)[];

  constructor(perspective: PerspectiveProxy, ctor: typeof Ad4mModel, query?: Query, unionClasses?: (typeof Ad4mModel)[]) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
    if (unionClasses) this.unionClasses = unionClasses;
  }

  /** Run `params` once and build model instances from the rows. */
  private execute(params: Query): Promise<Page<T>> {
    if (this.unionClasses) {
      return (this.ctor as any).executeUnionQuery(this.perspective, this.unionClasses, params);
    }
    return (this.ctor as any).executeModelQuery(this.perspective, params, this.modelClassName);
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
  where(conditions: TypedQueryWhere<T>): ModelQueryBuilder<T> {
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
   * 5. **String id alone** — containment through the built-in `Container`:
   *    the id's `ad4m://has_child` targets, and the targets of the `children`
   *    relation of every class the id is an instance of. The executor
   *    resolves the predicates; nothing is looked up here.
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
   *
   * // String id alone: whatever cookbookId contains
   * Recipe.query(perspective).parent(cookbookId).get();
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
      // Raw predicate string → raw form of Scope
      this.queryParams.parent = { id, predicate: modelOrPredicate };
    } else if (typeof modelOrPredicate === 'function') {
      // Model class → model form of Scope
      this.queryParams.parent = { id, model: modelOrPredicate, ...(field && { field }) };
    } else if (typeof idOrInstance !== 'string') {
      // Ad4mModel instance — derive model class from constructor
      this.queryParams.parent = { id, model: idOrInstance.constructor as typeof Ad4mModel, ...(field && { field }) };
    } else {
      // String id alone → containment form of Scope
      this.queryParams.parent = { id };
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
   * Asks for the individual links behind each instance — author, timestamp
   * and proof per link — under `instance.__links`. See `Query.links`.
   *
   * @param keys - Property / relation names, or absolute predicate IRIs
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * const [post] = await Post.query(perspective).links(["comments"]).get();
   * post.__links!.comments.map((l) => l.timestamp); // when each comment was attached
   * ```
   */
  links(keys: string[]): ModelQueryBuilder<T> {
    this.queryParams.links = keys;
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
   * Reads instances as they exist in links of one status only.
   *
   * `'shared'` leaves out every Local link, both from the values returned and
   * from what decides which instances are returned. Use it when the data is
   * shown to another user. `'local'` is the converse: it returns only
   * instances flagged in a Local link, so a Local note on a card flagged only
   * in a Shared link is read without `linkStatus`, not under `'local'`.
   * See {@link Query.linkStatus}.
   *
   * @param status - `'shared'` or `'local'`
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * const cards = await Card.query(perspective).linkStatus('shared').get();
   * ```
   */
  linkStatus(status: LinkStatus): ModelQueryBuilder<T> {
    this.queryParams.linkStatus = status;
    return this;
  }

  /**
   * Also hydrate from links whose signature did not verify.
   *
   * Off by default: the executor withholds unverified links. Turn it on only
   * to *display* an unverified claim, never for data you act on. See
   * {@link Query.includeUnverified}.
   *
   * @param enabled - Whether to include unverified links (default: true)
   * @returns The query builder for chaining
   */
  includeUnverified(enabled: boolean = true): ModelQueryBuilder<T> {
    this.queryParams.includeUnverified = enabled;
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

  /** Query the class registered under `className` instead of this builder's
   *  class (how `Ad4mModel.query(p).overrideModelClassName(name)` queries a
   *  model known only by name). On an undecorated `Ad4mModel` builder the rows
   *  hydrate as plain `Ad4mModel`s, and `includeAll` is not available (list
   *  the relations in `include`).
   *  @throws On a `queryOf()` builder: a union already names its classes, and
   *  its result type promises instances of them. */
  overrideModelClassName(className: string): ModelQueryBuilder<T> {
    if (this.unionClasses) {
      throw new Error(
        `overrideModelClassName("${className}") cannot be used on a queryOf() builder; ` +
        `use Ad4mModel.query(perspective).overrideModelClassName("${className}") instead`,
      );
    }
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
    const { results } = await this.execute(this.queryParams);
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
    // A count has no page a cursor can continue from — as paginate(), it is not sent.
    const { after: _after, ...cursorless } = this.queryParams || {};
    return requireTotalCount(await this.execute({ ...cursorless, limit: 0, count: true }));
  }

  /**
   * Gets the total count of matching entities using SPARQL.
   * Delegates to count().
   */
  async countSparql(): Promise<number> {
    return await this.count();
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
    // Page numbers page by offset; a cursor from the starting query has no meaning here.
    const { after: _after, ...base } = this.queryParams || {};
    const paginationQuery = { ...base, limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const answer = await this.execute(paginationQuery);
    return { results: answer.results, totalCount: requireTotalCount(answer), pageSize, pageNumber };
  }

  /**
   * One page in timestamp order, continued by cursor rather than by offset.
   *
   * Pass no `after` (or `""`) for the first page, then each page's
   * `nextCursor`; its absence means the last page. A row created while you
   * page cannot shift a later page the way it shifts an `offset` page. The
   * builder's `order` must be `timestamp`, `createdAt` or `updatedAt` (either
   * direction) or unset; the executor refuses any other order. The builder's
   * `offset` is ignored. Use `paginate()` for page numbers or other orders.
   *
   * @example
   * ```typescript
   * const builder = Message.query(perspective).order({ createdAt: "DESC" });
   * let page = await builder.page(50);
   * while (page.nextCursor) page = await builder.page(50, page.nextCursor);
   * ```
   */
  async page(pageSize: number, after?: string): Promise<CursorPage<T>> {
    // A cursor page returns no total, so `count` is dropped with `offset`: no COUNT runs for it.
    const { offset: _offset, count: _count, ...rest } = this.queryParams || {};
    const { results, nextCursor } = await this.execute({ ...rest, limit: pageSize, after: after ?? '' });
    return { results, ...(nextCursor !== undefined ? { nextCursor } : {}), pageSize };
  }

  /**
   * Gets a page of results using SPARQL.
   * Delegates to paginate().
   */
  async paginateSparql(pageSize: number, pageNumber: number): Promise<PaginationResult<T>> {
    return await this.paginate(pageSize, pageNumber);
  }
}
