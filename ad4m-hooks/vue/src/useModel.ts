import { ref, Ref, watch, shallowRef, ComputedRef } from "vue";
import { PerspectiveProxy, Ad4mModel, Query, PaginationResult, ModelQueryBuilder } from "@coasys/ad4m";

type Props<T extends Ad4mModel> = {
  perspective: PerspectiveProxy | ComputedRef<PerspectiveProxy | null>;
  model: string | ((new (...args: any[]) => T) & typeof Ad4mModel);
  query?: Query;
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
  let modelQuery: ModelQueryBuilder<T|Ad4mModel> | null = null;

  // Handle perspective as a ref/computed or direct value
  const isPerspectiveRef = perspective && typeof perspective === "object" && "value" in perspective;
  const perspectiveValue = isPerspectiveRef ? (perspective as Ref<PerspectiveProxy | null>).value : perspective;
  const perspectiveRef = shallowRef(perspectiveValue);

  // Set up a watch if perspective is a ref/computed
  if (isPerspectiveRef) {
    watch(
      () => (perspective as Ref<PerspectiveProxy | null>).value,
      (newVal) => {
        perspectiveRef.value = newVal;
      },
      { immediate: true }
    );
  }

  function includeBaseExpressions(entries: T[]): T[] {
    return entries.map((entry) => {
      return { ...entry, baseExpression: entry.baseExpression };
    });
  }

  function preserveEntryReferences(oldEntries: T[], newEntries: T[]): T[] {
    const existingMap = new Map(oldEntries.map((entry) => [entry.baseExpression, entry]));
    return newEntries.map((newEntry) => existingMap.get(newEntry.baseExpression) || newEntry);
  }

  function handleNewEntires(newEntries: T[]) {
    entries.value = includeBaseExpressions(
      preserveReferences ? preserveEntryReferences(entries.value, newEntries) : newEntries
    );
  }

  function paginateSubscribeCallback(result: PaginationResult<Ad4mModel>) {
    handleNewEntires(result.results as T[]);
    totalCount.value = result.totalCount as number;
  }

  async function subscribeToCollection() {
    try {
      // Return early if no perspective
      if (!perspectiveRef.value) {
        loading.value = false;
        return;
      }

      if(modelQuery) {
        modelQuery.dispose();
      }

      modelQuery =
        typeof model === "string"
          ? Ad4mModel.query(perspectiveRef.value, query).overrideModelClassName(model)
          : model.query(perspectiveRef.value, query);

      if (pageSize) {
        // Handle paginated results
        const totalPageSize = pageSize * pageNumber.value;
        const { results, totalCount: count } = await modelQuery.paginateSubscribe(
          totalPageSize,
          1,
          paginateSubscribeCallback
        );
        entries.value = includeBaseExpressions(results as T[]);
        totalCount.value = count as number;
      } else {
        // Handle non-paginated results
        const results = await modelQuery.subscribe((results: Ad4mModel[]) => handleNewEntires(results as T[]));
        entries.value = includeBaseExpressions(results as T[]);
      }
    } catch (err) {
      console.error("Error in subscribeToCollection:", err);
      error.value = err instanceof Error ? err.message : String(err);
    } finally {
      loading.value = false;
    }
  }

  function loadMore() {
    if (pageSize) {
      loading.value = true;
      pageNumber.value += 1;
    }
  }

  // Watch for perspective changes
  watch(
    perspectiveRef,
    async (newPerspective, oldPerspective) => {
      if (newPerspective !== oldPerspective) {
        loading.value = true;
        entries.value = [];
        if (newPerspective) {
          try {
            if (typeof model !== "string") {
              await newPerspective.ensureSDNASubjectClass(model);
            }
            await subscribeToCollection();
          } finally {
            loading.value = false;
          }
        } else {
          loading.value = false;
        }
      }
    },
    { immediate: true }
  );

  // Watch for query/page changes
  watch([() => JSON.stringify(query), pageNumber], async () => {
    if (perspectiveRef.value) {
      await subscribeToCollection();
    }
  });

  return { entries, loading, error, totalCount, loadMore };
}
