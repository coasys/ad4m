/**
 * Fluent query builder for Ad4mModel.
 *
 * Extracted from Ad4mModel.ts. The class holds a `Query` object and delegates
 * all execution to `Ad4mModel` static methods via the `ctor` constructor
 * reference, so there is no circular import (this module imports from the
 * Ad4mModel module, but Ad4mModel does not import from here — it re-exports
 * ModelQueryBuilder from its own file for backward compatibility).
 */

import { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import {
  Order,
  PaginationResult,
  Query,
  ResultsWithTotalCount,
  Where,
} from "../types";

// Forward-reference type only — avoids importing the full Ad4mModel
// module at the class level.
export type Ad4mModelCtor<T> = typeof import("../Ad4mModel").Ad4mModel &
  (new (...args: any[]) => T);

/**
 * Fluent builder for Ad4mModel queries.
 *
 * Create via `MyModel.query(perspective)`:
 * ```typescript
 * const posts = await Post.query(perspective)
 *   .where({ published: true })
 *   .order({ createdAt: "DESC" })
 *   .limit(10)
 *   .get();
 * ```
 */
export class ModelQueryBuilder<T extends import("../Ad4mModel").Ad4mModel> {
  private perspective: PerspectiveProxy;
  private queryParams: Query = {};
  private modelClassName: string | null = null;
  private ctor: Ad4mModelCtor<T>;
  private currentSubscription?: any;

  constructor(
    perspective: PerspectiveProxy,
    ctor: Ad4mModelCtor<T>,
    query?: Query,
  ) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
  }

  /**
   * Disposes of the current live subscription (if any).
   * Call when done to avoid memory leaks.
   */
  dispose() {
    if (this.currentSubscription) {
      this.currentSubscription.dispose();
      this.currentSubscription = undefined;
    }
  }

  where(conditions: Where): ModelQueryBuilder<T> {
    this.queryParams.where = conditions;
    return this;
  }

  order(orderBy: Order): ModelQueryBuilder<T> {
    this.queryParams.order = orderBy;
    return this;
  }

  limit(limit: number): ModelQueryBuilder<T> {
    this.queryParams.limit = limit;
    return this;
  }

  offset(offset: number): ModelQueryBuilder<T> {
    this.queryParams.offset = offset;
    return this;
  }

  source(source: string): ModelQueryBuilder<T> {
    this.queryParams.source = source;
    return this;
  }

  properties(properties: string[]): ModelQueryBuilder<T> {
    this.queryParams.properties = properties;
    return this;
  }

  relations(relations: string[]): ModelQueryBuilder<T> {
    this.queryParams.relations = relations;
    return this;
  }

  /**
   * Specifies which relations to eagerly load as full model instances.
   *
   * Only the listed relations will be batch-hydrated, giving explicit control
   * over which sub-graphs to load. Without `include`, all relations with a
   * `relatedModel` factory are hydrated automatically (legacy behaviour).
   *
   * @example
   * ```typescript
   * const recipes = await Recipe.query(perspective)
   *   .include(['author', 'comments'])
   *   .get();
   * ```
   */
  include(relations: string[]): ModelQueryBuilder<T> {
    this.queryParams.include = relations;
    return this;
  }

  overrideModelClassName(className: string): ModelQueryBuilder<T> {
    this.modelClassName = className;
    return this;
  }

  /**
   * Executes the query once and returns the results.
   */
  async get(): Promise<T[]> {
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      this.queryParams,
    );
    const result = await this.perspective.querySurrealDB(surrealQuery);
    const { results } = await this.ctor.instancesFromSurrealResult(
      this.perspective,
      this.queryParams,
      result,
    );
    return results as T[];
  }

  /**
   * Subscribes to live updates and returns initial results.
   * Call `dispose()` when done.
   */
  async subscribe(callback: (results: T[]) => void): Promise<T[]> {
    this.dispose();
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      this.queryParams,
    );
    this.currentSubscription =
      await this.perspective.subscribeSurrealDB(surrealQuery);

    const processResults = async (result: any) => {
      const { results } = await this.ctor.instancesFromSurrealResult(
        this.perspective,
        this.queryParams,
        result,
      );
      callback(results as T[]);
    };

    this.currentSubscription.onResult(processResults);
    const { results } = await this.ctor.instancesFromSurrealResult(
      this.perspective,
      this.queryParams,
      this.currentSubscription.result,
    );
    return results as T[];
  }

  /** Returns the total count of matching entities. */
  async count(): Promise<number> {
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      this.queryParams,
    );
    const result = await this.perspective.querySurrealDB(surrealQuery);
    const { totalCount } = await this.ctor.instancesFromSurrealResult(
      this.perspective,
      this.queryParams,
      result,
    );
    return totalCount;
  }

  /**
   * Subscribes to count updates. Call `dispose()` when done.
   */
  async countSubscribe(callback: (count: number) => void): Promise<number> {
    this.dispose();
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      this.queryParams,
    );
    this.currentSubscription =
      await this.perspective.subscribeSurrealDB(surrealQuery);

    const processResults = async (result: any) => {
      const { totalCount } = await this.ctor.instancesFromSurrealResult(
        this.perspective,
        this.queryParams,
        result,
      );
      callback(totalCount);
    };

    this.currentSubscription.onResult(processResults);
    const { totalCount } = await this.ctor.instancesFromSurrealResult(
      this.perspective,
      this.queryParams,
      this.currentSubscription.result,
    );
    return totalCount;
  }

  /** Returns a single page of results with pagination metadata. */
  async paginate(
    pageSize: number,
    pageNumber: number,
  ): Promise<PaginationResult<T>> {
    const paginationQuery: Query = {
      ...this.queryParams,
      limit: pageSize,
      offset: pageSize * (pageNumber - 1),
      count: true,
    };
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      paginationQuery,
    );
    const result = await this.perspective.querySurrealDB(surrealQuery);
    const { results, totalCount } = (await this.ctor.instancesFromSurrealResult(
      this.perspective,
      paginationQuery,
      result,
    )) as ResultsWithTotalCount<T>;
    return { results, totalCount, pageSize, pageNumber };
  }

  /**
   * Subscribes to paginated live updates. Call `dispose()` when done.
   */
  async paginateSubscribe(
    pageSize: number,
    pageNumber: number,
    callback: (results: PaginationResult<T>) => void,
  ): Promise<PaginationResult<T>> {
    this.dispose();
    const paginationQuery: Query = {
      ...this.queryParams,
      limit: pageSize,
      offset: pageSize * (pageNumber - 1),
      count: true,
    };
    const surrealQuery = await this.ctor.queryToSurrealQL(
      this.perspective,
      paginationQuery,
    );
    this.currentSubscription =
      await this.perspective.subscribeSurrealDB(surrealQuery);

    const processResults = async (result: any) => {
      const { results, totalCount } =
        (await this.ctor.instancesFromSurrealResult(
          this.perspective,
          paginationQuery,
          result,
        )) as ResultsWithTotalCount<T>;
      callback({ results, totalCount, pageSize, pageNumber });
    };

    this.currentSubscription.onResult(processResults);
    const { results, totalCount } = (await this.ctor.instancesFromSurrealResult(
      this.perspective,
      paginationQuery,
      this.currentSubscription.result,
    )) as ResultsWithTotalCount<T>;
    return { results, totalCount, pageSize, pageNumber };
  }
}
