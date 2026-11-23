import { useState, useEffect, useRef, useCallback, useMemo, useSyncExternalStore } from "react";
import {
  PerspectiveProxy,
  Ad4mModel,
  Query,
  Scope,
  LiveQuery,
  Page,
  Snapshot,
} from "@coasys/ad4m";

type ModelCtor<T extends Ad4mModel> = (new (...args: any[]) => T) &
  typeof Ad4mModel;

/**
 * Options for `useLiveQuery`.
 *
 * @example
 * ```ts
 * // Basic collection query
 * const { data } = useLiveQuery(Message, perspective);
 *
 * // Scoped to a parent via @HasMany metadata
 * const { data } = useLiveQuery(Message, perspective, {
 *   parent: { model: Channel, id: channelId },
 * });
 *
 * // Paginated with custom query filters
 * const { data, loadMore } = useLiveQuery(Message, perspective, {
 *   parent: { model: Channel, id: channelId },
 *   query: { order: { createdAt: 'DESC' } },
 *   pageSize: 30,
 * });
 * ```
 */
type LiveOptions<T extends Ad4mModel> = {
  /**
   * When provided, restricts results to children of this parent node via the
   * declared `@HasMany` relation. Supports both model-form (auto-resolved predicate)
   * and raw-form (explicit predicate string).
   */
  parent?: Scope;
  query?: Query;
  /** When set, enables load-more / infinite-scroll mode. */
  pageSize?: number;
  preserveReferences?: boolean;
};

export type LiveCollectionResult<T> = {
  data: T[];
  loading: boolean;
  error: string;
  totalCount: number;
  loadMore: () => void;
};

export type LiveInstanceResult<T> = {
  data: T | null;
  loading: boolean;
  error: string;
};

/** What a hook shows while there is no live query: loading, or (nothing to
 *  query) an empty result. */
const fixed = (snapshot: Snapshot<Page<Ad4mModel>>) => ({
  subscribe: () => () => {},
  getSnapshot: () => snapshot,
});
const LOADING = fixed({ status: "loading", reopening: false, initial: false });
const EMPTY = fixed({ status: "ready", data: { results: [] }, reopening: false, initial: true });

// ── Overload signatures ────────────────────────────────────────────────────

/**
 * Reactive single-instance query (supply `id` to select one node).
 * Returns `{ data: T | null, loading, error }`.
 */
export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy,
  options: LiveOptions<T> & { id: string },
): LiveInstanceResult<T>;

/**
 * Reactive collection query.
 * Returns `{ data: T[], loading, error, totalCount, loadMore }`.
 */
export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy,
  options?: LiveOptions<T>,
): LiveCollectionResult<T>;

/**
 * Dynamic string-model collection query (e.g. for generic class browsers).
 * When `model` is an empty string, returns empty data immediately.
 */
export function useLiveQuery(
  model: string,
  perspective: PerspectiveProxy,
  options?: LiveOptions<Ad4mModel>,
): LiveCollectionResult<Ad4mModel>;

// ── Implementation ─────────────────────────────────────────────────────────

export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T> | string,
  perspective: PerspectiveProxy,
  options: LiveOptions<T> & { id?: string } = {},
): LiveCollectionResult<T> | LiveInstanceResult<T> {
  const {
    parent,
    query: userQuery = {},
    preserveReferences = false,
    pageSize,
    id,
  } = options;

  const isInstance = id !== undefined;

  const [live, setLive] = useState<LiveQuery<Page<Ad4mModel>> | null>(null);
  const [openError, setOpenError] = useState<string>("");
  const [pageNumber, setPageNumber] = useState(1);

  // A parent scope whose id is not yet resolved, or an empty class name, has
  // nothing to query: the result is empty, not loading.
  const parentId = parent && "id" in parent ? parent.id : undefined;
  const held = (!!parent && "id" in parent && !parentId) || (typeof model === "string" && !model);
  const source = live ?? (held ? EMPTY : LOADING);
  const snapshot = useSyncExternalStore(source.subscribe, source.getSnapshot);

  /** Build the full query from user options + pagination + parent + instance filter. */
  function buildQuery(): Query {
    const q: Query = {
      ...userQuery,
      // A page always asks for its total.
      ...(pageSize ? { limit: pageSize * pageNumber, count: true } : {}),
    };

    // Parent scope — pass through to Query; core resolves the predicate internally
    if (!q.parent && parent) {
      q.parent = parent;
    }

    // Single-instance mode: filter to specific node URI
    if (id !== undefined) {
      q.where = { ...q.where, id: id };
    }

    return q;
  }

  /** Open the live query for either a class constructor or a string class name.
   *  The casts are the documented escape hatch for generic meta-utilities that
   *  build queries dynamically: TypeScript cannot resolve whether `T` has typed
   *  fields, so the deferred `TypedQuery<T>` does not accept the raw `Query`
   *  shape built at runtime. End-user callers of the static API keep strict
   *  typing. */
  function open(q: Query): LiveQuery<Page<Ad4mModel>> {
    if (typeof model === "string") {
      return Ad4mModel.live(perspective, q, { className: model });
    }
    return (model as ModelCtor<T>).live(perspective, q) as unknown as LiveQuery<Page<Ad4mModel>>;
  }

  useEffect(() => {
    setOpenError("");
    if (!perspective || held) {
      setLive(null);
      return;
    }
    let opened: LiveQuery<Page<Ad4mModel>>;
    try {
      opened = open(buildQuery());
    } catch (err) {
      setLive(null);
      setOpenError(err instanceof Error ? err.message : String(err));
      return;
    }
    setLive(opened);
    return () => opened.dispose();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    perspective?.uuid,
    typeof model === "string" ? model : (model as ModelCtor<T>).name,
    pageNumber,
    JSON.stringify(userQuery),
    parentId,
    "field" in (parent ?? {}) ? (parent as any).field : undefined,
    id,
  ]);

  // The entries of the last result, kept while the next query opens, and
  // reused instance by instance with `preserveReferences`.
  const previous = useRef<T[]>([]);
  const entries = useMemo(() => {
    if (snapshot.status !== "ready") return previous.current;
    const next = snapshot.data!.results as T[];
    if (!preserveReferences) return next;
    const known = new Map(previous.current.map((e) => [e.id, e]));
    return next.map((n) => known.get(n.id) ?? n);
  }, [snapshot, preserveReferences]);
  previous.current = entries;

  const loading = snapshot.status === "loading";
  const error = openError || (snapshot.status === "error" ? snapshot.error!.message : "");

  const loadMore = useCallback(() => {
    if (pageSize) setPageNumber((p) => p + 1);
  }, [pageSize]);

  if (isInstance) {
    return { data: entries[0] ?? null, loading, error };
  }
  return { data: entries, loading, error, totalCount: snapshot.data?.totalCount ?? 0, loadMore };
}
