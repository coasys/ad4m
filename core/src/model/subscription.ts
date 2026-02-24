/**
 * Live subscription implementation for Ad4mModel.
 *
 * Identical queries on the same perspective share a single `findAll()` call
 * and a single pair of link listeners via an internal registry — multiple
 * components subscribing to the same data cooperate rather than each running
 * independent queries.
 *
 * Results are fingerprinted before broadcasting: callbacks only fire when
 * the result set has actually changed.
 */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import type { LinkExpression } from "../links/Links";
import type {
  Query,
  ModelMetadata,
  Subscription,
  SubscribeOptions,
} from "./types";

// ─── Helpers ──────────────────────────────────────────────────────────────────

/**
 * Stable JSON key for a Query — sorts object keys so that equivalent queries
 * produced from different code paths hash to the same string.
 */
function stableQueryKey(query: Query): string {
  if (Object.keys(query).length === 0) return "{}";
  const sorted: Record<string, unknown> = {};
  for (const k of Object.keys(query).sort()) {
    sorted[k] = (query as any)[k];
  }
  return JSON.stringify(sorted);
}

/**
 * Stable fingerprint for an array of model instances.
 *
 * Sorted by `id` so result ordering doesn't cause false positives.
 * `id` is a prototype getter and therefore not serialised by
 * `JSON.stringify` automatically — it is extracted explicitly before spreading
 * the instance's own enumerable properties.
 */
function stableFingerprint(results: any[]): string {
  const sorted = [...results].sort((a, b) => {
    const aId: string = a.id ?? "";
    const bId: string = b.id ?? "";
    return aId < bId ? -1 : aId > bId ? 1 : 0;
  });
  return JSON.stringify(sorted.map((r) => ({ id: r.id, ...r })));
}

// ─── Subscription registry ────────────────────────────────────────────────────
//
// Key: `${modelClassName}:${stableQueryKey(query)}`
//
// WeakMap: when a PerspectiveProxy is garbage-collected its inner map is
// released automatically — no manual cleanup required.

interface ListenerRecord {
  callback: (results: any[]) => void;
  onError: ((err: Error) => void) | undefined;
  /** Allows unsubscribe() to surface the last error on the Subscription handle. */
  setLastError: (err: Error) => void;
}

interface SharedEntry {
  listeners: Map<symbol, ListenerRecord>;
  /** Results from the last successful findAll(). null until first run. */
  lastResults: any[] | null;
  lastFingerprint: string | null;
  debounceTimer: ReturnType<typeof setTimeout> | null;
  /** False once the last listener unsubscribes — prevents stale async callbacks. */
  active: boolean;
  /** Removes link-added/link-removed listeners from the perspective. */
  detach(): void;
}

const registry = new WeakMap<PerspectiveProxy, Map<string, SharedEntry>>();

function getOrCreateSharedEntry(
  findAll: (perspective: PerspectiveProxy, query?: Query) => Promise<any[]>,
  perspective: PerspectiveProxy,
  query: Query,
  metadata: ModelMetadata,
  debounceMs: number,
  perspEntries: Map<string, SharedEntry>,
  key: string,
): SharedEntry {
  const existing = perspEntries.get(key);
  if (existing) return existing;

  // Minimum coalesce window — even without a user-configured debounce, a
  // single save() emits several link events; this collapses them into one
  // findAll() call.  Not a timing workaround — SurrealDB is guaranteed
  // committed before link-added events are published.
  const SETTLE_MS = 50;
  const effectiveDebounce = Math.max(debounceMs, SETTLE_MS);

  const watchedPredicates = new Set<string>();
  for (const prop of Object.values(metadata.properties)) {
    if (prop.predicate) watchedPredicates.add(prop.predicate);
  }
  for (const rel of Object.values(metadata.relations)) {
    if (rel.predicate) watchedPredicates.add(rel.predicate);
  }

  const entry: SharedEntry = {
    listeners: new Map(),
    lastResults: null,
    lastFingerprint: null,
    debounceTimer: null,
    active: true,
    detach: () => {},
  };

  const broadcast = (results: any[]) => {
    for (const {
      callback,
      onError,
      setLastError,
    } of entry.listeners.values()) {
      try {
        callback(results);
      } catch (err) {
        setLastError(err as Error);
        (onError ?? console.error)(err as Error);
      }
    }
  };

  const notifyError = (err: Error) => {
    for (const { onError, setLastError } of entry.listeners.values()) {
      setLastError(err);
      (onError ?? console.error)(err);
    }
  };

  const rerun = async () => {
    if (!entry.active) return;
    try {
      const results = await findAll(perspective, query);
      if (!entry.active) return;

      // Only broadcast if results actually changed.
      const fingerprint = stableFingerprint(results);
      if (fingerprint === entry.lastFingerprint) return;
      entry.lastFingerprint = fingerprint;
      entry.lastResults = results;

      broadcast(results);
    } catch (err) {
      notifyError(err as Error);
    }
  };

  const scheduleRerun = () => {
    if (entry.debounceTimer !== null) clearTimeout(entry.debounceTimer);
    entry.debounceTimer = setTimeout(rerun, effectiveDebounce);
  };

  // LinkCallback must return null (see PerspectiveClient.ts type definition)
  const linkChangedCb = (link: LinkExpression): null => {
    if (watchedPredicates.has(link.data?.predicate ?? "")) scheduleRerun();
    return null;
  };

  perspective.addListener("link-added", linkChangedCb);
  perspective.addListener("link-removed", linkChangedCb);
  entry.detach = () => {
    // Fire-and-forget — removeListener is async but just does an array splice.
    // entry.active = false ensures no further callbacks fire regardless.
    perspective.removeListener("link-added", linkChangedCb);
    perspective.removeListener("link-removed", linkChangedCb);
  };

  // Fire immediately with initial results.
  rerun();

  perspEntries.set(key, entry);
  return entry;
}

// ─── Public API ───────────────────────────────────────────────────────────────

/**
 * Creates a live subscription for a model query.
 *
 * Immediately invokes `callback` with the initial query results, then
 * re-invokes it whenever a relevant link is added to or removed from the
 * perspective.
 *
 * Multiple callers subscribing to the same model + query on the same
 * perspective share a single `findAll()` execution and a single pair of link
 * listeners via an internal registry. Callbacks are only fired when the
 * result set has actually changed.
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
  const metadata = getMetadata();
  const key = `${metadata.className}:${stableQueryKey(query)}`;

  let perspEntries = registry.get(perspective);
  if (!perspEntries) {
    perspEntries = new Map();
    registry.set(perspective, perspEntries);
  }

  const entry = getOrCreateSharedEntry(
    findAll as (p: PerspectiveProxy, q?: Query) => Promise<any[]>,
    perspective,
    query,
    metadata,
    debounceMs,
    perspEntries,
    key,
  );

  const listenerId = Symbol();
  let lastError: Error | null = null;

  entry.listeners.set(listenerId, {
    callback: callback as (results: any[]) => void,
    onError,
    setLastError: (err) => {
      lastError = err;
    },
  });

  // Late subscriber: if the shared entry already ran at least once, fire
  // immediately with cached results — avoids an extra findAll() round-trip.
  if (entry.lastResults !== null) {
    const cached = entry.lastResults as T[];
    Promise.resolve().then(() => {
      if (entry.listeners.has(listenerId)) callback(cached);
    });
  }

  return {
    get lastError(): Error | null {
      return lastError;
    },
    unsubscribe() {
      entry.listeners.delete(listenerId);
      if (entry.listeners.size === 0) {
        // Last subscriber — tear down the shared entry entirely.
        entry.active = false;
        if (entry.debounceTimer !== null) {
          clearTimeout(entry.debounceTimer);
          entry.debounceTimer = null;
        }
        entry.detach();
        perspEntries!.delete(key);
      }
    },
  };
}
