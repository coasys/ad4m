import { Ad4mModel, PerspectiveProxy, Query, Subscription } from "@coasys/ad4m";
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
 * Scope a reactive query to a specific parent node via a `@HasMany` relation.
 * The hook reads the `through` predicate from the parent model's decorator
 * metadata automatically.
 */
type ParentScope = {
  model: ModelCtor<any>;
  id: string;
  /** Name of the `@HasMany` field on the parent model. */
  field: string;
};

type LiveOptions<T extends Ad4mModel> = {
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>;
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
export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  options: LiveOptions<T> & { id: string },
): LiveInstanceResult<T>;

/**
 * Reactive collection query.
 * Returns `{ data: Ref<T[]>, loading, error, totalCount, loadMore }`.
 */
export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  options: LiveOptions<T>,
): LiveCollectionResult<T>;

// ── Implementation ─────────────────────────────────────────────────────────

export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  options: LiveOptions<T> & { id: string },
): LiveInstanceResult<T>;

/**
 * Reactive collection query.
 * Returns `{ data: Ref<T[]>, loading, error, totalCount, loadMore }`.
 */
export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T>,
  options: LiveOptions<T>,
): LiveCollectionResult<T>;

/**
 * Dynamic string-model collection query (e.g. for generic class browsers).
 * When `model` is an empty string, returns empty data immediately.
 */
export function useLive(
  model: string,
  options: LiveOptions<Ad4mModel>,
): LiveCollectionResult<Ad4mModel>;

// ── Implementation ─────────────────────────────────────────────────────────

export function useLive<T extends Ad4mModel>(
  model: ModelCtor<T> | string,
  options: LiveOptions<T> & { id?: string },
): LiveCollectionResult<T> | LiveInstanceResult<T> {
  const {
    perspective,
    parent,
    query: userQuery = {},
    preserveReferences = false,
    pageSize,
    id,
  } = options;

  const isInstance = id !== undefined;

  const collectionData = ref<T[]>([]) as Ref<T[]>;
  const instanceData = ref<T | null>(null) as Ref<T | null>;
  const loading = ref(true);
  const error = ref<string>("");
  const pageNumber = ref(1);
  const totalCount = ref(0);

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

  let activeSub: Subscription | null = null;

  /** Derive `linkedFrom` from the parent scope if provided. */
  function resolveLinkedFrom(): { id: string; predicate: string } | undefined {
    if (!parent) return undefined;
    const parentMeta = parent.model.getModelMetadata();
    const through = parentMeta.relations[parent.field]?.predicate;
    if (!through) {
      console.warn(
        `useLive: field "${parent.field}" not found in parent model "${parent.model.name}" relations. ` +
          `Check that "@HasMany" is declared on that field.`,
      );
      return undefined;
    }
    return { id: parent.id, predicate: through };
  }

  /** Build the query builder for either a class constructor or a string class name. */
  function makeQueryBuilder(q: Query, p: PerspectiveProxy) {
    if (typeof model === "string") {
      return Ad4mModel.query(p, q).overrideModelClassName(model);
    }
    return (model as ModelCtor<T>).query(p, q);
  }

  function buildLiveQuery(): Query {
    const base: Query = {
      ...userQuery,
      ...(pageSize ? { limit: pageSize * pageNumber.value } : {}),
    };

    // Parent scope takes precedence; only set linkedFrom if not already in userQuery
    if (!base.linkedFrom) {
      const linkedFrom = resolveLinkedFrom();
      if (linkedFrom) base.linkedFrom = linkedFrom;
    }

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

  function subscribe(p: PerspectiveProxy) {
    activeSub?.unsubscribe();
    activeSub = null;
    loading.value = true;

    const q = buildLiveQuery();

    // For string models, skip subscription when model name is empty
    if (typeof model === "string" && !model) {
      collectionData.value = [];
      loading.value = false;
      return;
    }

    try {
      if (isInstance) {
        activeSub = makeQueryBuilder(q, p).live(
          (results) => {
            instanceData.value = (results[0] as T) ?? null;
            loading.value = false;
          },
          {
            onError: (err) => {
              error.value = err.message;
              loading.value = false;
            },
          },
        );
      } else {
        activeSub = makeQueryBuilder(q, p).live(
          (results) => {
            collectionData.value = mergeEntries(
              collectionData.value,
              results as T[],
            );
            loading.value = false;
          },
          {
            onError: (err) => {
              error.value = err.message;
              loading.value = false;
            },
          },
        );

        if (pageSize) {
          const countQ: Query = { ...buildLiveQuery() };
          delete countQ.limit;
          makeQueryBuilder(countQ, p)
            .count()
            .then((n) => {
              totalCount.value = n;
            })
            .catch(console.error);
        }
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
        activeSub?.unsubscribe();
        activeSub = null;
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

  onUnmounted(() => {
    activeSub?.unsubscribe();
    activeSub = null;
  });

  const loadMore = () => {
    if (pageSize) pageNumber.value += 1;
  };

  if (isInstance) {
    return { data: instanceData, loading, error };
  }
  return { data: collectionData, loading, error, totalCount, loadMore };
}
