import { useState, useEffect, useRef, useCallback } from "react";
import {
  PerspectiveProxy,
  Ad4mModel,
  Query,
  ParentScope,
  PaginationResult,
  ModelQueryBuilder,
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
  parent?: ParentScope;
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

  const [loading, setLoading] = useState(true);
  const [collectionData, setCollectionData] = useState<T[]>([]);
  const [instanceData, setInstanceData] = useState<T | null>(null);
  const [error, setError] = useState<string>("");
  const [pageNumber, setPageNumber] = useState(1);
  const [totalCount, setTotalCount] = useState(0);

  const modelQueryRef = useRef<ModelQueryBuilder<T | Ad4mModel> | null>(null);

  /** Build the full query from user options + pagination + parent + instance filter. */
  function buildQuery(): Query {
    const q: Query = {
      ...userQuery,
      ...(pageSize ? { limit: pageSize * pageNumber } : {}),
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

  /** Build a ModelQueryBuilder for either a class constructor or a string class name. */
  function makeQueryBuilder(q: Query) {
    if (typeof model === "string") {
      return Ad4mModel.query(perspective, q).overrideModelClassName(model);
    }
    return (model as ModelCtor<T>).query(perspective, q);
  }

  function mergeEntries(oldEntries: T[], newEntries: T[]): T[] {
    if (!preserveReferences) return newEntries;
    const existingMap = new Map(oldEntries.map((e) => [e.id, e]));
    return newEntries.map((n) => existingMap.get(n.id) ?? n);
  }

  function handleNewEntries(newEntries: T[]) {
    if (isInstance) {
      setInstanceData(newEntries[0] ?? null);
    } else {
      setCollectionData((old) => mergeEntries(old, newEntries));
    }
  }

  function paginateSubscribeCallback({ results, totalCount: count }: PaginationResult<T>) {
    handleNewEntries(results);
    setTotalCount(count as number);
  }

  const subscribe = useCallback(() => {
    if (!perspective) return;

    // If a parent scope is declared but its id is not yet resolved, hold off
    if (parent && !parent.id) {
      setCollectionData([]);
      setInstanceData(null);
      setLoading(false);
      return;
    }

    // For string models, skip when model name is empty
    if (typeof model === "string" && !model) {
      setCollectionData([]);
      setLoading(false);
      return;
    }

    // Dispose previous subscription
    modelQueryRef.current?.dispose();
    modelQueryRef.current = null;
    setLoading(true);

    const q = buildQuery();

    try {
      modelQueryRef.current = makeQueryBuilder(q);

      if (pageSize) {
        // Paginated subscription
        modelQueryRef.current
          .paginateSubscribe(
            pageSize * pageNumber,
            1,
            paginateSubscribeCallback as (result: PaginationResult<Ad4mModel>) => void,
          )
          .then(({ results, totalCount: count }) => {
            if (isInstance) {
              setInstanceData((results[0] as T) ?? null);
            } else {
              setCollectionData(results as T[]);
            }
            setTotalCount(count as number);
            setLoading(false);
          })
          .catch((err) => {
            setError(err instanceof Error ? err.message : String(err));
            setLoading(false);
          });
      } else {
        // Non-paginated subscription
        modelQueryRef.current
          .subscribe(handleNewEntries as (results: Ad4mModel[]) => void)
          .then((results) => {
            if (isInstance) {
              setInstanceData((results[0] as T) ?? null);
            } else {
              setCollectionData(results as T[]);
            }
            setLoading(false);
          })
          .catch((err) => {
            setError(err instanceof Error ? err.message : String(err));
            setLoading(false);
          });
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setLoading(false);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    perspective?.uuid,
    typeof model === "string" ? model : (model as ModelCtor<T>).name,
    pageNumber,
    JSON.stringify(userQuery),
    parent?.id,
    "field" in (parent ?? {}) ? (parent as any).field : undefined,
    id,
  ]);

  useEffect(() => {
    subscribe();
    return () => {
      modelQueryRef.current?.dispose();
      modelQueryRef.current = null;
    };
  }, [subscribe]);

  const loadMore = useCallback(() => {
    if (pageSize) setPageNumber((p) => p + 1);
  }, [pageSize]);

  if (isInstance) {
    return { data: instanceData, loading, error };
  }
  return { data: collectionData, loading, error, totalCount, loadMore };
}
