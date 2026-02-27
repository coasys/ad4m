import { Ad4mModel, PerspectiveProxy, Query, Subscription } from "@coasys/ad4m";
import { ComputedRef, isRef, onUnmounted, ref, Ref, shallowRef, watch } from "vue";

type ModelCtor<T extends Ad4mModel> = (new (...args: any[]) => T) & typeof Ad4mModel;

type Props<T extends Ad4mModel> = {
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>;
  model: string | ModelCtor<T>;
  query?: Query;
  /** When set, enables load-more / infinite-scroll mode. */
  pageSize?: number;
  preserveReferences?: boolean;
};

type Result<T extends Ad4mModel> = {
  entries: Ref<T[]>;
  loading: Ref<boolean>;
  error: Ref<string>;
  totalCount: Ref<number>;
  loadMore: () => void;
};

export function useModel<T extends Ad4mModel>(props: Props<T>): Result<T> {
  const { perspective, model, query = {}, preserveReferences = false, pageSize } = props;

  const entries = ref<T[]>([]) as Ref<T[]>;
  const loading = ref(true);
  const error = ref<string>("");
  const pageNumber = ref(1);
  const totalCount = ref(0);

  // Normalise: accept either a raw PerspectiveProxy or a ref/computed wrapping one
  const perspectiveRef = shallowRef<PerspectiveProxy | null>(
    isRef(perspective) ? (perspective as ComputedRef<PerspectiveProxy | null>).value : perspective
  );

  if (isRef(perspective)) {
    watch(perspective as ComputedRef<PerspectiveProxy | null>, (val) => {
      perspectiveRef.value = val;
    });
  }

  let activeSub: Subscription | null = null;

  function buildLiveQuery(): Query {
    // Growing-window strategy for load-more: always fetch from the start up to
    // the current limit so reactions/replies on earlier items stay live.
    return pageSize ? { ...query, limit: pageSize * pageNumber.value } : query;
  }

  function mergeEntries(oldEntries: T[], newEntries: T[]): T[] {
    if (!preserveReferences) return newEntries;
    const existingMap = new Map(oldEntries.map((e) => [e.id, e]));
    return newEntries.map((n) => existingMap.get(n.id) ?? n);
  }

  function buildQueryBuilder(p: PerspectiveProxy, q: Query) {
    return typeof model === "string"
      ? Ad4mModel.query(p, q).overrideModelClassName(model)
      : (model as ModelCtor<T>).query(p, q);
  }

  function subscribe(p: PerspectiveProxy) {
    // Tear down previous subscription before creating a new one
    activeSub?.unsubscribe();
    activeSub = null;
    loading.value = true;

    try {
      activeSub = buildQueryBuilder(p, buildLiveQuery()).live(
        (results) => {
          entries.value = mergeEntries(entries.value, results as T[]);
          loading.value = false;
        },
        {
          onError: (err) => {
            error.value = err.message;
            loading.value = false;
          },
        }
      );
    } catch (err) {
      error.value = err instanceof Error ? err.message : String(err);
      loading.value = false;
    }

    // Fetch totalCount independently — only used in load-more UI ("X of Y")
    if (pageSize) {
      buildQueryBuilder(p, query)
        .count()
        .then((n) => { totalCount.value = n; })
        .catch(console.error);
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
        entries.value = [];
        return;
      }
      if (newP.uuid !== oldP?.uuid) {
        entries.value = [];
      }
      subscribe(newP);
    },
    { immediate: true }
  );

  // Re-subscribe when query or page changes
  watch(
    [() => JSON.stringify(query), pageNumber],
    () => {
      if (perspectiveRef.value) subscribe(perspectiveRef.value);
    }
  );

  onUnmounted(() => {
    activeSub?.unsubscribe();
    activeSub = null;
  });

  function loadMore() {
    if (pageSize) pageNumber.value += 1;
  }

  return { entries, loading, error, totalCount, loadMore };
}
