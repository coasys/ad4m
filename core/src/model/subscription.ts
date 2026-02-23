/**
 * Live subscription implementation for Ad4mModel.
 *
 * Uses PerspectiveProxy.addListener to watch for link changes, then
 * re-runs findAll whenever a relevant link is added or removed.
 * Fires immediately with the initial results.
 */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import type { LinkExpression } from "../links/Links";
import type {
  Query,
  ModelMetadata,
  Subscription,
  SubscribeOptions,
} from "./types";

/**
 * Creates a live subscription for a model query.
 *
 * Immediately invokes `callback` with the initial query results, then
 * re-invokes it whenever a relevant link is added to or removed from the
 * perspective.
 *
 * @param findAll     - The model's `findAll` static method
 * @param getMetadata - The model's `getModelMetadata` static method
 * @param perspective - The perspective to watch
 * @param options     - Query parameters + delivery options (debounce, onError)
 * @param callback    - Invoked with fresh results on every relevant change
 * @returns A `Subscription` handle with `unsubscribe()` and `lastError`
 *
 * @example
 * ```typescript
 * // Called from Ad4mModel.subscribe():
 * const sub = createSubscription(
 *   (p, q) => Post.findAll(p, q),
 *   () => Post.getModelMetadata(),
 *   perspective,
 *   { where: { published: true }, debounce: 200 },
 *   (posts) => console.log("Posts updated:", posts),
 * );
 *
 * // Later:
 * sub.unsubscribe();
 * ```
 */
export function createSubscription<T>(
  findAll: (perspective: PerspectiveProxy, query?: Query) => Promise<T[]>,
  getMetadata: () => ModelMetadata,
  perspective: PerspectiveProxy,
  options: SubscribeOptions,
  callback: (results: T[]) => void,
): Subscription {
  const { debounce: debounceMs = 0, onError, ...queryOptions } = options;
  const query: Query = queryOptions as Query;

  // Build the set of predicates this model reads/writes.
  // Any link touching one of these predicates could affect the query result.
  const metadata = getMetadata();
  const watchedPredicates = new Set<string>();
  for (const prop of Object.values(metadata.properties)) {
    if (prop.predicate) watchedPredicates.add(prop.predicate);
  }
  for (const rel of Object.values(metadata.relations)) {
    if (rel.predicate) watchedPredicates.add(rel.predicate);
  }

  let lastError: Error | null = null;
  let debounceTimer: ReturnType<typeof setTimeout> | null = null;
  let active = true;

  const rerun = async () => {
    if (!active) return;
    try {
      const results = await findAll(perspective, query);
      if (!active) return; // unsubscribed during the async query
      try {
        callback(results);
      } catch (callbackErr) {
        lastError = callbackErr as Error;
        (onError ?? console.error)(callbackErr as Error);
      }
    } catch (queryErr) {
      lastError = queryErr as Error;
      (onError ?? console.error)(queryErr as Error);
    }
  };

  const scheduleRerun: () => void =
    debounceMs > 0
      ? () => {
          if (debounceTimer !== null) clearTimeout(debounceTimer);
          debounceTimer = setTimeout(rerun, debounceMs);
        }
      : rerun;

  const isRelevant = (link: LinkExpression): boolean => {
    const predicate = link.data?.predicate ?? "";
    return watchedPredicates.has(predicate);
  };

  // LinkCallback must return null (see PerspectiveClient.ts type definition)
  const linkChangedCb = (link: LinkExpression): null => {
    if (isRelevant(link)) scheduleRerun();
    return null;
  };

  // Fire immediately with initial results
  rerun();

  perspective.addListener("link-added", linkChangedCb);
  perspective.addListener("link-removed", linkChangedCb);

  return {
    get lastError() {
      return lastError;
    },
    unsubscribe() {
      if (!active) return;
      active = false;
      if (debounceTimer !== null) {
        clearTimeout(debounceTimer);
        debounceTimer = null;
      }
      // Fire-and-forget — removeListener is async but just does an array splice.
      // Setting active = false above ensures no more callbacks fire even if
      // removeListener hasn't completed yet.
      perspective.removeListener("link-added", linkChangedCb);
      perspective.removeListener("link-removed", linkChangedCb);
    },
  };
}
