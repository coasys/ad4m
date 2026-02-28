import { useState, useEffect, useRef, useCallback } from "react";
import {
  PerspectiveProxy,
  Ad4mModel,
  Query,
  Subscription,
  resolveParentPredicate,
} from "@coasys/ad4m";

type ModelCtor<T extends Ad4mModel> = (new (...args: any[]) => T) &
  typeof Ad4mModel;

/**
 * Scope a reactive query to a specific parent node via a `@HasMany` relation.
 *
 * The hook reads the `through` predicate from the parent model's decorator
 * metadata automatically — you never need to reference the predicate string
 * at the call site.
 *
 * @example
 * ```ts
 * // field inferred — only one @HasMany on Channel points to Message
 * const { data: messages } = useLive(Message, perspective, {
 *   parent: { model: Channel, id: channelId },
 * });
 *
 * // field explicit — needed when multiple @HasMany point to the same type
 * const { data: messages } = useLive(Message, perspective, {
 *   parent: { model: Channel, id: channelId, field: 'messages' },
 * });
 * ```
 */
type ParentScope = {
  model: ModelCtor<any>;
  id: string;
  /**
   * The `@HasMany` field on the parent model with the linking predicate.
   * Optional when exactly one `@HasMany` on the parent points to this child type.
   */
  field?: string;
};

type LiveOptions<T extends Ad4mModel> = {
  /**
   * When provided, restricts results to children of this parent node via the
   * declared `@HasMany` relation.  The subscription also watches the relation
   * predicate so additions/removals trigger a live re-query.
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
export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy,
  options: LiveOptions<T> & { id: string },
): LiveInstanceResult<T>;

/**
 * Reactive collection query.
 * Returns `{ data: T[], loading, error, totalCount, loadMore }`.
 */
export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy,
  options?: LiveOptions<T>,
): LiveCollectionResult<T>;

/**
 * Dynamic string-model collection query (e.g. for generic class browsers).
 * When `model` is an empty string, returns empty data immediately.
 */
export function useLive(
  model: string,
  perspective: PerspectiveProxy,
  options?: LiveOptions<Ad4mModel>,
): LiveCollectionResult<Ad4mModel>;

// ── Implementation ─────────────────────────────────────────────────────────

export function useLive<T extends Ad4mModel>(
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

  const subRef = useRef<Subscription | null>(null);

  /** Resolve the `parent` query from the top-level `parent` scope option. */
  function resolveParentQuery(): { id: string; predicate: string } | undefined {
    if (!parent) return undefined;
    try {
      const predicate = resolveParentPredicate(
        parent.model.getModelMetadata(),
        typeof model !== "string" ? model : undefined,
        parent.field,
      );
      return { id: parent.id, predicate };
    } catch (err) {
      console.warn(`useLive: ${err instanceof Error ? err.message : err}`);
      return undefined;
    }
  }

  /** Build the query builder for either a class constructor or a string class name. */
  function makeQueryBuilder(q: Query) {
    if (typeof model === "string") {
      return Ad4mModel.query(perspective, q).overrideModelClassName(model);
    }
    return (model as ModelCtor<T>).query(perspective, q);
  }

  function buildLiveQuery(): Query {
    const base: Query = {
      ...userQuery,
      ...(pageSize ? { limit: pageSize * pageNumber } : {}),
    };

    // Parent scope takes precedence; only set parent if not already in userQuery
    if (!base.parent) {
      const resolvedParent = resolveParentQuery();
      if (resolvedParent) base.parent = resolvedParent;
    }

    // For single-instance mode, filter to the specific node URI
    if (id !== undefined) {
      base.where = { ...base.where, base: id };
    }

    return base;
  }

  function mergeEntries(oldEntries: T[], newEntries: T[]): T[] {
    if (!preserveReferences) return newEntries;
    const existingMap = new Map(oldEntries.map((e) => [e.id, e]));
    return newEntries.map((n) => existingMap.get(n.id) ?? n);
  }

  const subscribe = useCallback(() => {
    if (!perspective) return;

    // If a parent scope is declared but its id is not yet resolved (e.g. the
    // web-component prop hasn't been set yet), hold off.  Running the query
    // with an undefined id causes escapeSurrealString(undefined) to throw.
    if (parent && !parent.id) {
      setCollectionData([]);
      setInstanceData(null);
      setLoading(false);
      return;
    }

    subRef.current?.unsubscribe();
    subRef.current = null;
    setLoading(true);

    const q = buildLiveQuery();

    // For string models, skip subscription when model name is empty
    if (typeof model === "string" && !model) {
      setCollectionData([]);
      setLoading(false);
      return;
    }

    try {
      if (isInstance) {
        subRef.current = makeQueryBuilder(q).live(
          (results) => {
            setInstanceData((results[0] as T) ?? null);
            setLoading(false);
          },
          {
            onError: (err) => {
              setError(err.message);
              setLoading(false);
            },
          },
        );
      } else {
        subRef.current = makeQueryBuilder(q).live(
          (results) => {
            setCollectionData((old) => mergeEntries(old, results as T[]));
            setLoading(false);
          },
          {
            onError: (err) => {
              setError(err.message);
              setLoading(false);
            },
          },
        );

        // Total count for pagination UI — only needed in load-more mode
        if (pageSize) {
          // Count query: same filters but without the page limit
          const countQ: Query = { ...buildLiveQuery() };
          delete countQ.limit;
          makeQueryBuilder(countQ)
            .count()
            .then(setTotalCount)
            .catch(console.error);
        }
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
    parent?.field,
    id,
  ]);

  useEffect(() => {
    subscribe();
    return () => {
      subRef.current?.unsubscribe();
      subRef.current = null;
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
