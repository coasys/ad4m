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
  Where,
  IncludeMap,
  SubscribeOptions,
  Subscription,
} from "../types";
import { createSubscription } from "../subscription";

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

  constructor(
    perspective: PerspectiveProxy,
    ctor: Ad4mModelCtor<T>,
    query?: Query,
  ) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
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

  properties(properties: string[]): ModelQueryBuilder<T> {
    this.queryParams.properties = properties;
    return this;
  }

  /**
   * Eagerly load specific relations as full model instances.
   *
   * Key = relation field name on the model.
   * Value = `true` (all instances) or a `Query` to filter/order/limit the nested set.
   *
   * @example
   * ```typescript
   * Recipe.query(perspective)
   *   .include({
   *     comments: true,
   *     tags: { where: { active: true }, limit: 5 },
   *   })
   *   .get();
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

  /** Executes the query and returns all matching instances. */
  async get(): Promise<T[]> {
    return this.ctor.findAll(this.perspective, this.queryParams) as Promise<
      T[]
    >;
  }

  /** Returns the first matching instance, or `null` if none found. */
  async first(): Promise<T | null> {
    return this.ctor.findOne(
      this.perspective,
      this.queryParams,
    ) as Promise<T | null>;
  }

  /** Returns the total count of matching entities. */
  async count(): Promise<number> {
    return this.ctor.count(this.perspective, this.queryParams);
  }

  /** Returns a single page of results with pagination metadata. */
  async paginate(
    pageSize: number,
    pageNumber: number,
  ): Promise<PaginationResult<T>> {
    return this.ctor.paginate(
      this.perspective,
      pageSize,
      pageNumber,
      this.queryParams,
    ) as Promise<PaginationResult<T>>;
  }

  /**
   * Terminal: creates a live subscription using the query parameters accumulated
   * so far.  Fires `callback` immediately with current results, then on every
   * relevant link change.
   *
   * Pass additional delivery options (`debounce`, `onError`) in `deliveryOpts`.
   * Call `sub.unsubscribe()` to detach.
   *
   * @example
   * ```typescript
   * const sub = Post.query(perspective)
   *   .where({ published: true })
   *   .order({ createdAt: "DESC" })
   *   .live((posts) => setPosts(posts), { debounce: 300 });
   *
   * // cleanup:
   * sub.unsubscribe();
   * ```
   */
  live(
    callback: (results: T[]) => void,
    deliveryOpts?: Pick<SubscribeOptions, "debounce" | "onError">,
  ): Subscription {
    const options: SubscribeOptions = { ...this.queryParams, ...deliveryOpts };
    return createSubscription<T>(
      (p, q) => this.ctor.findAll(p, q ?? {}),
      () => this.ctor.getModelMetadata(),
      this.perspective,
      options,
      callback,
      this.ctor as any,
    );
  }
}
