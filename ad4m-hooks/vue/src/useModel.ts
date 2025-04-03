import { ref, Ref, watch, computed, shallowRef } from "vue";
import { PerspectiveProxy, Ad4mModel, Query, PaginationResult } from "@coasys/ad4m";

type Props<T extends Ad4mModel> = {
  perspective: PerspectiveProxy | any;
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
  const subjectEnsured = ref(false);
  const perspectiveRef = typeof perspective === "function" ? computed(() => perspective()) : shallowRef(perspective);

  async function ensureSubject() {
    try {
      if (typeof model !== "string") await perspectiveRef.value.ensureSDNASubjectClass(model);
      subjectEnsured.value = true;
    } catch (e) {
      console.error("Failed to ensure subject:", e);
      error.value = e instanceof Error ? e.message : String(e);
    }
  }

  function includeBaseExpressions(entries: T[]): T[] {
    return entries.map((entry) => {
      return { ...entry, baseExpression: entry.baseExpression };
    });
  }

  function preserveEntryReferences(oldEntries: T[], newEntries: T[]): T[] {
    // Merge new results into old results, preserving references for optimized rendering
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
      const modelQuery =
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

  // First watch perspective changes
  watch(
    perspectiveRef,
    async (newPerspective, oldPerspective) => {
      if (newPerspective) {
        // Reset state for new perspective
        if (newPerspective !== oldPerspective) {
          loading.value = true;
          subjectEnsured.value = false;
        }
        // Ensure subject for new perspective
        await ensureSubject();
      }
    },
    { immediate: true }
  );

  // Then watch for when subject is ensured
  watch(
    subjectEnsured,
    async (newValue) => {
      if (newValue && perspectiveRef.value) await subscribeToCollection();
    },
    { immediate: true }
  );

  // Also watch for query/page changes after subject is ensured
  watch([() => JSON.stringify(query), pageNumber], async () => {
    if (subjectEnsured.value && perspectiveRef.value) await subscribeToCollection();
  });

  return { entries, loading, error, totalCount, loadMore };
}
