import {
  Ad4mModel,
  ModelQueryBuilder,
  PaginationResult,
  PerspectiveProxy,
  Query,
  ParentScope,
} from "@coasys/ad4m";
import {
  ComputedRef,
  isRef,
  onUnmounted,
  ref,
  Ref,
  shallowRef,
  watch,
} from "vue";

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
  data: Ref<T[]>;
  loading: Ref<boolean>;
  error: Ref<string>;
  totalCount: Ref<number>;
  loadMore: () => void;
};

export type LiveInstanceResult<T> = {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<string>;
};

// ── Overload signatures ────────────────────────────────────────────────────

/**
 * Reactive single-instance query (supply `id` to select one node).
 * Returns `{ data: Ref<T | null>, loading, error }`.
 */
export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>,
  options: LiveOptions<T> & { id: string },
): LiveInstanceResult<T>;

/**
 * Reactive collection query.
 * Returns `{ data: Ref<T[]>, loading, error, totalCount, loadMore }`.
 */
export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T>,
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>,
  options?: LiveOptions<T>,
): LiveCollectionResult<T>;

/**
 * Dynamic string-model collection query (e.g. for generic class browsers).
 * When `model` is an empty string, returns empty data immediately.
 */
export function useLiveQuery(
  model: string,
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>,
  options?: LiveOptions<Ad4mModel>,
): LiveCollectionResult<Ad4mModel>;

// ── Implementation ─────────────────────────────────────────────────────────

export function useLiveQuery<T extends Ad4mModel>(
  model: ModelCtor<T> | string,
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>,
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

  // Use shallowRef for model data to prevent Vue from deep-proxying Ad4mModel instances.
  // Deep proxying breaks JS # private fields on nested objects (e.g. PerspectiveProxy).
  // Safe because the hook always replaces the entire array/value on subscription updates.
  const collectionData = shallowRef<T[]>([]) as Ref<T[]>;
  const instanceData = shallowRef<T | null>(null) as Ref<T | null>;
  const loading = ref(true);
  const error = ref<string>("");
  const pageNumber = ref(1);
  const totalCount = ref(0);
  let modelQuery: ModelQueryBuilder<T | Ad4mModel> | null = null;

  // Normalise: accept either a raw PerspectiveProxy or a ref/computed wrapping one
  const perspectiveRef = shallowRef<PerspectiveProxy | null>(
    isRef(perspective)
      ? (perspective as ComputedRef<PerspectiveProxy | null>).value
      : perspective,
  );

  if (isRef(perspective)) {
    watch(perspective as ComputedRef<PerspectiveProxy | null>, (val) => {
      perspectiveRef.value = val;
    });
  }

  /** Build the full query from user options + pagination + parent + instance filter. */
  function buildQuery(): Query {
    const q: Query = {
      ...userQuery,
      ...(pageSize ? { limit: pageSize * pageNumber.value } : {}),
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
  function makeQueryBuilder(q: Query, p: PerspectiveProxy) {
    if (typeof model === "string") {
      return Ad4mModel.query(p, q).overrideModelClassName(model);
    }
    return (model as ModelCtor<T>).query(p, q);
  }

  function mergeEntries(oldEntries: T[], newEntries: T[]): T[] {
    if (!preserveReferences) return newEntries;
    const existingMap = new Map(oldEntries.map((e) => [e.id, e]));
    return newEntries.map((n) => existingMap.get(n.id) ?? n);
  }

  function handleNewEntries(newEntries: T[]) {
    if (isInstance) {
      instanceData.value = newEntries[0] ?? null;
    } else {
      collectionData.value = mergeEntries(collectionData.value, newEntries);
    }
  }

  function paginateSubscribeCallback(result: PaginationResult<Ad4mModel>) {
    handleNewEntries(result.results as T[]);
    totalCount.value = result.totalCount as number;
  }

  function subscribe(p: PerspectiveProxy) {
    // If a parent scope is declared but its id is not yet resolved, hold off
    if (parent && !parent.id) {
      collectionData.value = [];
      instanceData.value = null;
      loading.value = false;
      return;
    }

    // For string models, skip when model name is empty
    if (typeof model === "string" && !model) {
      collectionData.value = [];
      loading.value = false;
      return;
    }

    // Dispose previous subscription
    modelQuery?.dispose();
    modelQuery = null;
    loading.value = true;

    const q = buildQuery();

    try {
      modelQuery = makeQueryBuilder(q, p);

      if (pageSize) {
        // Paginated subscription
        modelQuery
          .paginateSubscribe(
            pageSize * pageNumber.value,
            1,
            paginateSubscribeCallback,
          )
          .then(({ results, totalCount: count }) => {
            if (isInstance) {
              instanceData.value = (results[0] as T) ?? null;
            } else {
              collectionData.value = results as T[];
            }
            totalCount.value = count as number;
            loading.value = false;
          })
          .catch((err) => {
            error.value = err instanceof Error ? err.message : String(err);
            loading.value = false;
          });
      } else {
        // Non-paginated subscription
        modelQuery
          .subscribe((results: Ad4mModel[]) => handleNewEntries(results as T[]))
          .then((results) => {
            if (isInstance) {
              instanceData.value = (results[0] as T) ?? null;
            } else {
              collectionData.value = results as T[];
            }
            loading.value = false;
          })
          .catch((err) => {
            error.value = err instanceof Error ? err.message : String(err);
            loading.value = false;
          });
      }
    } catch (err) {
      error.value = err instanceof Error ? err.message : String(err);
      loading.value = false;
    }
  }

  // Re-subscribe when perspective identity changes
  watch(
    perspectiveRef,
    (newP, oldP) => {
      if (!newP) {
        modelQuery?.dispose();
        modelQuery = null;
        loading.value = false;
        collectionData.value = [];
        instanceData.value = null;
        return;
      }
      if (newP.uuid !== oldP?.uuid) {
        collectionData.value = [];
        instanceData.value = null;
      }
      subscribe(newP);
    },
    { immediate: true },
  );

  // Re-subscribe when query or page changes
  watch([() => JSON.stringify(userQuery), pageNumber], () => {
    if (perspectiveRef.value) subscribe(perspectiveRef.value);
  });

  // Clean up on unmount
  onUnmounted(() => {
    modelQuery?.dispose();
    modelQuery = null;
  });

  const loadMore = () => {
    if (pageSize) pageNumber.value += 1;
  };

  if (isInstance) {
    return { data: instanceData, loading, error };
  }
  return { data: collectionData, loading, error, totalCount, loadMore };
}
